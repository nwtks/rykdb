// ===========================================================================
// StorageEngine.fs
// ===========================================================================

module RykDb.StorageEngineTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private withEngine (dir: string) (f: StorageEngine -> unit) =
    let engine = ok (StorageEngine.openDatabase dir)
    f engine

/// 与えたエントリ列を SSTable として書き、メタデータを返す。
let private mkSst (dir: string) (num: int64) (level: int) (entries: Entry list) =
    entries
    |> List.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
    |> List.toArray
    |> FileMetaData.writeSstable dir num level 0.01
    |> ok

/// 閾値を全レベル 0 にして「最初の非空レベル」から compaction させる。
let private pickFirstLevel =
    { CompactionOptions.defaults with
        MaxLevelBytes = Array.create Levels.Count 0L
    }

/// ファイルの Bloom を読み、[lo..hi] の範囲で「含まれないのに偽陽性で通る」キーを探す。
/// Bloom は決定的なので、同じファイルなら常に同じ結果になる。
let private findBloomFalsePositive (dir: string) (fileNumber: int64) (lo: int) (hi: int) =
    let reader =
        ok (Sstable.openTable (File.ReadAllBytes(FileMetaData.path dir fileNumber)))

    [ lo..hi ]
    |> List.tryPick (fun i ->
        let k = intKey i
        if Sstable.mayContain reader k then Some k else None)

/// ファイルの Bloom を読み、[lo..hi] の範囲で「確実に含まれない」と言われるキーを探す。
let private findBloomMiss (dir: string) (fileNumber: int64) (lo: int) (hi: int) =
    let reader =
        ok (Sstable.openTable (File.ReadAllBytes(FileMetaData.path dir fileNumber)))

    [ lo..hi ]
    |> List.tryPick (fun i ->
        let k = intKey i
        if not (Sstable.mayContain reader k) then Some k else None)

/// SSTable の本文を書き換え、Footer のチェックサムを合わせ直す。
/// 「開けるが中身が壊れたファイル」を作る（FooterSize = 80、checksum は先頭から 76 バイト目）。
let private patchSstable (dir: string) (fileNumber: int64) (patch: byte array -> unit) =
    let path = FileMetaData.path dir fileNumber
    let data = File.ReadAllBytes path
    patch data

    let footerStart = data.Length - 80
    let checksum = Hashing.checksum (Array.sub data 0 footerStart)
    data.[footerStart + 76] <- byte checksum
    data.[footerStart + 77] <- byte (checksum >>> 8)
    data.[footerStart + 78] <- byte (checksum >>> 16)
    data.[footerStart + 79] <- byte (checksum >>> 24)
    File.WriteAllBytes(path, data)

let private rowKey (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]

[<Fact>]
let ``StorageEngine.getAt: put して memtable から引ける`` () =
    withDir "se1" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let found = ok (StorageEngine.getAt engine None (intKey 1))

            match found with
            | Some e -> Assert.Equal<uint64>(1UL, e.SeqNo)
            | None -> Assert.Fail "見つからないはず"))

[<Fact>]
let ``StorageEngine.getAt: 削除は tombstone になる`` () =
    withDir "se3" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.delete engine (intKey 1))

            match ok (StorageEngine.getAt engine None (intKey 1)) with
            | Some e -> Assert.True e.IsDelete
            | None -> Assert.Fail "tombstone が残るはず"))

[<Fact>]
let ``StorageEngine.getAt: 古い snapshot からは commit 前の版が見える`` () =
    withDir "mv1" (fun dir ->
        let engine = StorageEngine.openDatabase dir |> ok
        let engine = StorageEngine.put engine (rowKey 1) [| SqlValue.Int 1L |] |> ok
        let snapBefore = engine.NextSeqNo - 1UL
        let engine = StorageEngine.put engine (rowKey 1) [| SqlValue.Int 99L |] |> ok
        // memtable には同じキーの 2 版がある
        Assert.Equal(2, engine.Mem.Count)

        let newest = StorageEngine.getAt engine (Some engine.NextSeqNo) (rowKey 1) |> ok

        match newest with
        | Some e -> Assert.Equal<int64>(99L, asInt e.Value.Value.[0])
        | None -> Assert.Fail "最新版が見つからないはず"

        let older = StorageEngine.getAt engine (Some snapBefore) (rowKey 1) |> ok

        match older with
        | Some e -> Assert.Equal<int64>(1L, asInt e.Value.Value.[0])
        | None -> Assert.Fail "古い版が見つからないはず"

        // snapshot が古すぎると「まだ書かれていない」ことになる
        let none = StorageEngine.getAt engine (Some 0UL) (rowKey 1) |> ok
        Assert.True none.IsNone)

[<Fact>]
let ``StorageEngine.getAt: ファイルが消えていれば IoError`` () =
    withDir "se-missing" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.flush engine)
            // Version は参照しているのに実ファイルだけ消える（読み取りで気付く）
            Directory.GetFiles(dir, "*.sst") |> Array.iter (fun p -> File.Delete p)

            expectErrorKind ErrorKind.IoError "ファイルが無ければ IoError" (StorageEngine.getAt engine None (intKey 1))))

[<Fact>]
let ``StorageEngine.getAt: Bloom が「無い」と言うキーはスキャンせず None`` () =
    withDir "se-bloom-miss" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst
                    dir
                    10L
                    0
                    [
                        Entry.put (intKey 1) 1UL (intRow 1)
                        Entry.put (intKey 5000) 5000UL (intRow 5000)
                    ]

            let engine =
                { engine with
                    Version = Version.addFile f Version.empty
                }

            // 範囲内で存在しないキーのうち、Bloom が「無い」と言うものを選ぶ
            let key = findBloomMiss dir 10L 2 4999
            Assert.True key.IsSome
            Assert.True((ok (StorageEngine.getAt engine None key.Value)).IsNone)))

[<Fact>]
let ``StorageEngine.getAt: Bloom の偽陽性でも範囲内の無いキーは None`` () =
    withDir "se-bloom-fp" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst
                    dir
                    10L
                    0
                    [
                        Entry.put (intKey 1) 1UL (intRow 1)
                        Entry.put (intKey 5000) 5000UL (intRow 5000)
                    ]

            let engine =
                { engine with
                    Version = Version.addFile f Version.empty
                }

            // 存在しないのに Bloom が「あるかも」と言うキー → スキャンは最初の実キー（5000）に当たる
            let key = findBloomFalsePositive dir 10L 2 4999
            Assert.True key.IsSome
            Assert.True((ok (StorageEngine.getAt engine None key.Value)).IsNone)))

[<Fact>]
let ``StorageEngine.getAt: Version の範囲が実ファイルより広くても None`` () =
    withDir "se-wide-range" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst
                    dir
                    10L
                    0
                    [
                        Entry.put (intKey 1) 1UL (intRow 1)
                        Entry.put (intKey 5000) 5000UL (intRow 5000)
                    ]

            // Version 側の Largest だけ 9000 に広げる（実ファイルの最大は 5000）
            let engine =
                { engine with
                    Version = Version.addFile { f with Largest = intKey 9000 } Version.empty
                }

            // 実ファイルの最大より大きいキー（Bloom の偽陽性でスキャンまで進む）
            let key = findBloomFalsePositive dir 10L 5001 9000
            Assert.True key.IsSome
            Assert.True((ok (StorageEngine.getAt engine None key.Value)).IsNone)))

[<Fact>]
let ``StorageEngine.getAt: 壊れたブロックは CorruptionError`` () =
    withDir "se-bad-block" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst dir 10L 0 [ Entry.put (intKey 1) 1UL (intRow 1); Entry.put (intKey 2) 2UL (intRow 2) ]

            // ブロック末尾の restart 件数を巨大値にして壊し、チェックサムは合わせ直す
            let block =
                (ok (Sstable.openTable (File.ReadAllBytes(FileMetaData.path dir 10L)))).Index.[0]

            patchSstable dir 10L (fun data ->
                let at = block.Offset + block.Length - 4
                data.[at] <- 0xFFuy
                data.[at + 1] <- 0xFFuy
                data.[at + 2] <- 0xFFuy
                data.[at + 3] <- 0x00uy)

            let engine =
                { engine with
                    Version = Version.addFile f Version.empty
                }

            expectErrorKind
                ErrorKind.CorruptionError
                "壊れたブロックは CorruptionError"
                (StorageEngine.getAt engine None (intKey 1))))

[<Fact>]
let ``StorageEngine.getAt: 古い snapshot は SSTable 内の見える版まで戻る`` () =
    withDir "se-snapshot" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst dir 10L 0 [ Entry.put (intKey 1) 5UL (intRow 5); Entry.put (intKey 1) 3UL (intRow 3) ]

            let engine =
                { engine with
                    Version = Version.addFile f Version.empty
                }

            // snapshot 4 には最新版 seq5 は見えず、1つ古い seq3 が見える
            match ok (StorageEngine.getAt engine (Some 4UL) (intKey 1)) with
            | Some e -> Assert.Equal<uint64>(3UL, e.SeqNo)
            | None -> Assert.Fail "seq3 が見えるはず"))

[<Fact>]
let ``StorageEngine.scanAt: キー昇順で重複を畳む`` () =
    withDir "se4" (fun dir ->
        withEngine dir (fun engine ->
            let engine =
                List.fold (fun e i -> ok (StorageEngine.put e (intKey i) (intRow i))) engine [ 1; 2; 3 ]

            let got = ok (StorageEngine.scanAt engine None [||] 0) |> Seq.toArray
            Assert.Equal(3, got.Length)
            Assert.Equal<byte array>(intKey 1, got.[0].Key)
            Assert.Equal<byte array>(intKey 2, got.[1].Key)
            Assert.Equal<byte array>(intKey 3, got.[2].Key)))

[<Fact>]
let ``StorageEngine.scanAt: limit が効く`` () =
    withDir "se5" (fun dir ->
        withEngine dir (fun engine ->
            let engine =
                List.fold (fun e i -> ok (StorageEngine.put e (intKey i) (intRow i))) engine [ 1; 2; 3; 4; 5 ]

            let scanned = ok (StorageEngine.scanAt engine None [||] 2) |> Seq.toArray
            Assert.Equal(2, scanned.Length)))

[<Fact>]
let ``StorageEngine.scanAt: scan も snapshot で版を絞る`` () =
    withDir "mv2" (fun dir ->
        let engine = StorageEngine.openDatabase dir |> ok
        let engine = StorageEngine.put engine (rowKey 1) [| SqlValue.Int 1L |] |> ok
        let snapBefore = engine.NextSeqNo - 1UL
        let engine = StorageEngine.put engine (rowKey 1) [| SqlValue.Int 2L |] |> ok
        let engine = StorageEngine.put engine (rowKey 2) [| SqlValue.Int 20L |] |> ok

        let old = StorageEngine.scanAt engine (Some snapBefore) [||] 0 |> ok |> Seq.toArray
        Assert.Equal(1, old.Length)
        Assert.Equal<int64>(1L, asInt old.[0].Value.Value.[0])

        let all = StorageEngine.scanAt engine None [||] 0 |> ok |> Seq.toArray
        Assert.Equal(2, all.Length))

/// キー 1..2 のファイル（10）とキー 3..4 のファイル（11）を作り、11 のマジックを
/// 壊す（openTable が失敗する＝開封時に検出される壊れたファイル）。
let private twoFilesWithBrokenSecond (dir: string) =
    let f1 =
        mkSst dir 10L 0 [ Entry.put (intKey 1) 1UL (intRow 1); Entry.put (intKey 2) 2UL (intRow 2) ]

    let f2 =
        mkSst dir 11L 0 [ Entry.put (intKey 3) 3UL (intRow 3); Entry.put (intKey 4) 4UL (intRow 4) ]

    patchSstable dir 11L (fun data ->
        let footerStart = data.Length - 80
        data.[footerStart] <- 0x00uy)

    f1, f2

[<Fact>]
let ``StorageEngine.scanAt: limit つき走査は必要にならないファイルを開かない`` () =
    withDir "se-lazy-open" (fun dir ->
        withEngine dir (fun engine ->
            let f1, f2 = twoFilesWithBrokenSecond dir

            let engine =
                { engine with
                    Version = Version.empty |> Version.addFile f1 |> Version.addFile f2
                }

            // 壊れたファイル 11 の下限はキー 3。limit 1 の走査はファイル 10 の
            // 先頭（キー 1）だけで満たされるので、ファイル 11 を開かずに済む。
            let scanned = ok (StorageEngine.scanAt engine None (intKey 1) 1) |> Seq.toArray
            Assert.Equal(1, scanned.Length)
            Assert.Equal(0, Bytes.compare scanned.[0].Key (intKey 1))))

[<Fact>]
let ``StorageEngine.scanAt: 制限なしの走査は壊れたファイルを先に検出する`` () =
    withDir "se-eager-open" (fun dir ->
        withEngine dir (fun engine ->
            let f1, f2 = twoFilesWithBrokenSecond dir

            let engine =
                { engine with
                    Version = Version.empty |> Version.addFile f1 |> Version.addFile f2
                }

            // 制限なしの走査は全ファイルを読むので、開封時に全ファイルを開いて
            // 壊れたファイルを Result で検出する。
            expectErrorKind ErrorKind.CorruptionError "制限なしの走査は壊れたファイルを検出する" (StorageEngine.scanAt engine None [||] 0)))

[<Fact>]
let ``StorageEngine.scanAt: limit つき走査でも欠落ファイルは先に検出する`` () =
    withDir "se-missing-open" (fun dir ->
        withEngine dir (fun engine ->
            let f1, f2 = twoFilesWithBrokenSecond dir
            File.Delete(FileMetaData.path dir 11L)

            let engine =
                { engine with
                    Version = Version.empty |> Version.addFile f1 |> Version.addFile f2
                }

            expectErrorKind
                ErrorKind.IoError
                "欠落ファイルは limit つき走査でも検出する"
                (StorageEngine.scanAt engine None (intKey 1) 1)))

[<Fact>]
let ``StorageEngine: flush すると SSTable から引ける`` () =
    withDir "se2" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            let engine = ok (StorageEngine.flush engine)
            // memtable は空になり、L0 にファイルが 1 個できる
            Assert.Equal(0, engine.Mem.Count)
            Assert.True((ok (StorageEngine.getAt engine None (intKey 1))).IsSome)
            Assert.True((ok (StorageEngine.getAt engine None (intKey 2))).IsSome)))

[<Fact>]
let ``StorageEngine.maybeFlush: 上限を超えたら flush する`` () =
    withDir "se-auto-flush" (fun dir ->
        withEngine dir (fun engine ->
            let engine = { engine with MemLimitBytes = 1 }
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let beforeWal = engine.Wal.FileNumber
            let flushed = ok (StorageEngine.maybeFlush engine)
            Assert.Equal(0, flushed.Mem.Count)
            Assert.Equal(1, Version.filesAt flushed.Version 0 |> Array.length)
            Assert.True(flushed.Wal.FileNumber > beforeWal)
            Assert.True((ok (StorageEngine.getAt flushed None (intKey 1))).IsSome)))

[<Fact>]
let ``StorageEngine.maybeFlush: 上限以下なら何もしない`` () =
    withDir "se-no-flush" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let same = ok (StorageEngine.maybeFlush engine)
            Assert.Equal(1, same.Mem.Count)
            Assert.Empty(Version.filesAt same.Version 0)))

[<Fact>]
let ``DiskStorageEngine.MaybeFlush: wrapper 経由でも上限超過を flush する`` () =
    withDir "se-disk-flush" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)
        let disk = DiskStorageEngine { engine with MemLimitBytes = 1 } :> IStorageEngine
        let disk = ok (disk.WriteBatch [| Entry.put (intKey 1) 1UL (intRow 1) |])
        let disk = ok (disk.MaybeFlush())

        Assert.NotEmpty(Directory.GetFiles(dir, "*.sst"))
        Assert.True((ok (disk.GetAt None (intKey 1))).IsSome))

[<Fact>]
let ``StorageEngine.maybeCompact: L0 が閾値に達したら L1 へ compaction する`` () =
    withDir "se-l0-compact" (fun dir ->
        let engine =
            [ 1..4 ]
            |> List.fold
                (fun engine i ->
                    let engine = ok (StorageEngine.put engine (intKey i) (intRow i))
                    ok (StorageEngine.flush engine))
                (ok (StorageEngine.openDatabase dir))

        Assert.Equal(4, Version.filesAt engine.Version 0 |> Array.length)
        let compacted = ok (StorageEngine.maybeCompact engine)
        Assert.Empty(Version.filesAt compacted.Version 0)
        Assert.Equal(1, Version.filesAt compacted.Version 1 |> Array.length)
        let got = ok (StorageEngine.scanAt compacted None [||] 0) |> Seq.length
        Assert.Equal(4, got))

[<Fact>]
let ``StorageEngine: flush 済みと memtable をまたいで scan できる`` () =
    withDir "se6" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            let engine = ok (StorageEngine.flush engine)
            let engine = ok (StorageEngine.put engine (intKey 3) (intRow 3))
            let got = ok (StorageEngine.scanAt engine None [||] 0) |> Seq.toArray
            Assert.Equal(3, got.Length)
            Assert.Equal<byte array>(intKey 3, got.[2].Key)))

[<Fact>]
let ``StorageEngine: flush 後も古い snapshot が効く`` () =
    withDir "mv3" (fun dir ->
        let engine = StorageEngine.openDatabase dir |> ok
        let engine = StorageEngine.put engine (rowKey 1) [| SqlValue.Int 1L |] |> ok
        let engine = StorageEngine.flush engine |> ok

        // 古い版が SSTable にある状態で新しい版を書く
        let engine = StorageEngine.put engine (rowKey 1) [| SqlValue.Int 2L |] |> ok
        let old = StorageEngine.getAt engine (Some 1UL) (rowKey 1) |> ok

        match old with
        | Some e -> Assert.Equal<int64>(1L, asInt e.Value.Value.[0])
        | None -> Assert.Fail "SSTable にある古い版が見つからないはず"

        let latest = StorageEngine.getAt engine None (rowKey 1) |> ok

        match latest with
        | Some e -> Assert.Equal<int64>(2L, asInt e.Value.Value.[0])
        | None -> Assert.Fail "memtable にある新しい版が見つからないはず")


[<Fact>]
let ``StorageEngine: WAL は flush 後に切り替わる`` () =
    withDir "re6" (fun dir ->
        withEngine dir (fun engine ->
            let before = engine.Wal.FileNumber
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.flush engine)
            Assert.True(engine.Wal.FileNumber > before)
            // 古い WAL は消えて、新しいものは空
            let wals = Wal.walFiles dir
            Assert.Single<string> wals |> ignore
            Assert.Equal(0, (ok (Wal.read dir engine.Wal.FileNumber)).Length)))

[<Fact>]
let ``StorageEngine: close して開き直すとデータが残る`` () =
    withDir "re1" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            ok (StorageEngine.close engine))

        withEngine dir (fun engine ->
            // close は flush しているので L0 から引ける
            Assert.True((ok (StorageEngine.getAt engine None (intKey 1))).IsSome)
            Assert.True((ok (StorageEngine.getAt engine None (intKey 2))).IsSome)))

[<Fact>]
let ``StorageEngine: close せずに終了しても WAL から復旧できる`` () =
    withDir "re2" (fun dir ->
        // flush しないまま「プロセス終了」を模擬する（WAL にだけ残っている）
        withEngine dir (fun engine ->
            let _ = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let _ = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            ())

        // 開き直すと WAL 再生で値が戻る
        withEngine dir (fun engine ->
            Assert.True((ok (StorageEngine.getAt engine None (intKey 1))).IsSome)
            Assert.True((ok (StorageEngine.getAt engine None (intKey 2))).IsSome)))

[<Fact>]
let ``StorageEngine: WAL 再生後は SeqNo が既存版より小さくならない`` () =
    withDir "re3" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.flush engine)
            let _ = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            ())

        withEngine dir (fun engine ->
            // 既存の L0 に入る前の seqNo より大きい値で書き再開できる
            let before = engine.NextSeqNo
            let engine = ok (StorageEngine.put engine (intKey 3) (intRow 3))
            Assert.True(engine.NextSeqNo > before)
            let engine = ok (StorageEngine.flush engine)
            let got = ok (StorageEngine.scanAt engine None [||] 0) |> Seq.toArray
            Assert.Equal(3, got.Length)))

[<Fact>]
let ``StorageEngine: Version に無い SSTable は開き直しで回収される`` () =
    withDir "comp-orphan" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)
        let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
        let engine = ok (StorageEngine.flush engine)
        let referenced = Version.allFiles engine.Version |> List.map _.FileNumber
        // Version が参照しないファイルを置く
        File.WriteAllBytes(FileMetaData.path dir 9L, [||])

        let reopened = ok (StorageEngine.openDatabase dir)
        Assert.Equal<int64 list>(referenced, Version.allFiles reopened.Version |> List.map _.FileNumber)
        Assert.True(File.Exists(FileMetaData.path dir (List.exactlyOne referenced)))
        Assert.False(File.Exists(FileMetaData.path dir 9L)))

[<Fact>]
let ``StorageEngine: 存在しない SSTable は Version から落とされる`` () =
    withDir "re4" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let _ = ok (StorageEngine.flush engine)
            // flush でできるファイルを消す（クラッシュの模擬）
            let ssts = Directory.GetFiles(dir, "*.sst")
            Assert.NotEmpty ssts
            ssts |> Array.iter (fun p -> File.Delete p)
            ())

        withEngine dir (fun engine ->
            // Version から落ちているのでエラーにならない
            Assert.False((ok (StorageEngine.getAt engine None (intKey 1))).IsSome)))

[<Fact>]
let ``StorageEngine: 実ファイルが消えていても NextFileNumber は進む`` () =
    withDir "re-next-file" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        // Version が参照するファイルが実在しない状態 + 番号 0 の WAL を作る
        let _ = ok (Manifest.openOrCreate dir)

        let _ =
            ok (Manifest.logAndApply dir 1L Version.empty (VersionEdit.addFile (fmd 1L 0 1 1)))

        let _ = ok (Wal.create dir 0L)

        let engine = ok (StorageEngine.openDatabase dir)
        // 消えたファイルの削除で NextFileNumber は 2 に進み、0 番の WAL があっても巻き戻らない
        Assert.Equal(2L, engine.Version.NextFileNumber)
        Assert.Empty(Version.allFiles engine.Version))

[<Fact>]
let ``StorageEngine: 最大 SeqNo はファイルの並び順に関係なく取れる`` () =
    withDir "re-seqmax" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let _ = ok (Manifest.openOrCreate dir)
        // SeqNo が「後のファイルで小さくなる」並び（キー順では fA → fB）
        let fA = mkSst dir 10L 0 [ Entry.put (intKey 1) 5UL (intRow 1) ]
        let fB = mkSst dir 11L 0 [ Entry.put (intKey 2) 3UL (intRow 2) ]
        let v1, m1 = ok (Manifest.logAndApply dir 1L Version.empty (VersionEdit.addFile fA))
        let _ = ok (Manifest.logAndApply dir m1 v1 (VersionEdit.addFile fB))

        let engine = ok (StorageEngine.openDatabase dir)
        // 再開後はファイル内の最大 SeqNo（5）の次から書ける
        Assert.Equal(6UL, engine.NextSeqNo))

[<Fact>]
let ``StorageEngine.maybeCompact: L1→L2 を反映し、入力ファイルを置き換える`` () =
    withDir "comp-engine" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)

        let f1 =
            mkSst dir 10L 1 [ Entry.put (intKey 1) 1UL (intRow 1); Entry.put (intKey 3) 3UL (intRow 3) ]

        let f2 =
            mkSst dir 11L 1 [ Entry.put (intKey 2) 2UL (intRow 2); Entry.put (intKey 3) 13UL (intRow 3) ]

        let engine =
            { engine with
                Version = Version.empty |> Version.addFile f1 |> Version.addFile f2
                Compaction = pickFirstLevel
                ActiveSnapshot = 14UL
            }

        let compacted = ok (StorageEngine.maybeCompact engine)
        // L1 は空になり、L2 に 1 ファイルできる
        Assert.Empty(compacted.Version.Files.[1])
        Assert.Equal(1, compacted.Version.Files.[2].Length)
        // 入力ファイルは消え、出力ファイルだけが残る
        Assert.False(File.Exists(FileMetaData.path dir 10L))
        Assert.False(File.Exists(FileMetaData.path dir 11L))

        let out = compacted.Version.Files.[2].[0]
        Assert.True(File.Exists(FileMetaData.path dir out.FileNumber))

        // データは引ける（key3 は新しい seq 13 の版）
        match ok (StorageEngine.getAt compacted None (intKey 3)) with
        | Some e -> Assert.Equal<uint64>(13UL, e.SeqNo)
        | None -> Assert.Fail "key3 が見つかりません"

        match ok (StorageEngine.getAt compacted None (intKey 1)) with
        | Some e -> Assert.Equal<uint64>(1UL, e.SeqNo)
        | None -> Assert.Fail "key1 が見つかりません"

        // 開き直しても compaction 後の形が MANIFEST から復元される
        let reopened = ok (StorageEngine.openDatabase dir)
        Assert.Empty(reopened.Version.Files.[1])
        Assert.Equal(1, reopened.Version.Files.[2].Length))

[<Fact>]
let ``StorageEngine.maybeCompact: 閾値内なら何も変えない`` () =
    withDir "comp-noop" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.flush engine)
            // 既定の閾値（L1 = 10MiB）には遠く届かない
            let compacted = ok (StorageEngine.maybeCompact engine)
            Assert.Equal(engine.Version.NextFileNumber, compacted.Version.NextFileNumber)
            Assert.Equal(engine.ManifestNumber, compacted.ManifestNumber)
            Assert.Single(Version.allFiles compacted.Version) |> ignore))

[<Fact>]
let ``StorageEngine.maybeCompact: 全部 tombstone なら出力なしで入力だけ消える`` () =
    withDir "comp-empty-out" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst dir 10L Levels.Max [ Entry.delete (intKey 1) 5UL; Entry.delete (intKey 2) 6UL ]

            let engine =
                { engine with
                    Version = Version.addFile f Version.empty
                    Compaction = pickFirstLevel
                    ActiveSnapshot = 0UL
                }

            let compacted = ok (StorageEngine.maybeCompact engine)
            Assert.Empty(Version.allFiles compacted.Version)
            Assert.False(File.Exists(FileMetaData.path dir 10L))

            // MANIFEST にも反映されている（開き直しても空のまま）
            let reopened = ok (StorageEngine.openDatabase dir)
            Assert.Empty(Version.allFiles reopened.Version)))

[<Fact>]
let ``StorageEngine: 孤児の SSTable は削除せず退避する`` () =
    withDir "re5" (fun dir ->
        withEngine dir (fun _ -> ())
        // MANIFEST に無いファイルを 1 個置いてから開く
        File.WriteAllBytes(Path.Combine(dir, "000999.sst"), [| 1uy; 2uy; 3uy |])

        withEngine dir (fun _ ->
            // 消さずに .orphans へ移す（manifest の読み落としで消すと復元できない）
            Assert.False(File.Exists(Path.Combine(dir, "000999.sst")))
            Assert.Single(Directory.GetFiles(Path.Combine(dir, ".orphans"))) |> ignore))

[<Fact>]
let ``StorageEngine: manifest の途中破損では開かず SSTable を残す`` () =
    withDir "open-corrupt-manifest" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)
        let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
        let _ = ok (StorageEngine.flush engine)

        // 有効な行の前に壊れた行を置く（bit rot を模擬）
        let manifestPath = Path.Combine(dir, "MANIFEST-000001")
        let valid = File.ReadAllLines manifestPath
        Assert.NotEmpty valid
        File.WriteAllLines(manifestPath, Array.append [| "!!!broken!!!" |] valid)

        expectErrorKind ErrorKind.CorruptionError "途中の破損は CorruptionError" (StorageEngine.openDatabase dir)
        // SSTable は削除も退避もされていない
        Assert.NotEmpty(Directory.GetFiles(dir, "*.sst"))
        Assert.False(Directory.Exists(Path.Combine(dir, ".orphans"))))

[<Fact>]
let ``StorageEngine: 古いハンドルの close は WAL を消さない`` () =
    withDir "close-stale" (fun dir ->
        let engine0 = ok (StorageEngine.openDatabase dir)
        let engine1 = ok (StorageEngine.put engine0 (intKey 1) (intRow 1))
        let engine2 = ok (StorageEngine.put engine1 (intKey 2) (intRow 2))

        // engine1 は engine2 より前の状態。WAL には engine2 の分が追記されている。
        ok (StorageEngine.close engine1)

        // 古いハンドルの close で WAL が消えていれば、engine2 の書き込みは失われる
        let reopened = ok (StorageEngine.openDatabase dir)
        Assert.True((ok (StorageEngine.getAt reopened None (intKey 1))).IsSome)
        Assert.True((ok (StorageEngine.getAt reopened None (intKey 2))).IsSome)

        ok (StorageEngine.close engine2))

[<Fact>]
let ``StorageEngine: 番号として読めない .sst は無視される`` () =
    withDir "se-junk-sst" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let _ = ok (StorageEngine.flush engine)
            ())

        // 番号として読めないファイルを置いても、開けなくなったり消されたりしない
        File.WriteAllText(Path.Combine(dir, "junk.sst"), "not a number")
        let engine = ok (StorageEngine.openDatabase dir)
        Assert.True(File.Exists(Path.Combine(dir, "junk.sst")))
        Assert.True((ok (StorageEngine.getAt engine None (intKey 1))).IsSome))
