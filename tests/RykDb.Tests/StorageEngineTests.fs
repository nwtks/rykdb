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

/// SSTable の本文を書き換え、全体・メタデータ・Footer のチェックサムを合わせ直す。
/// 「開けるが中身が壊れたファイル」を作る
/// （FooterSize = 88、全体 checksum は +76、metadata checksum は +80、Footer checksum は +84）。
let private patchSstable (dir: string) (fileNumber: int64) (patch: byte array -> unit) =
    let path = FileMetaData.path dir fileNumber
    let data = File.ReadAllBytes path

    let footerStart = data.Length - Sstable.FooterSize

    let readI64 offset =
        [ 0..7 ]
        |> List.fold (fun acc i -> acc ||| (int64 data.[data.Length - Sstable.FooterSize + offset + i] <<< (i * 8))) 0L

    let writeU32 offset (value: uint32) =
        for i in 0..3 do
            data.[footerStart + offset + i] <- byte (value >>> (i * 8) &&& 0xFFu)

    patch data
    let bodyChecksum = Hashing.checksum (Array.sub data 0 footerStart)

    writeU32 76 bodyChecksum

    let metadataOffset = int (readI64 52)
    let indexOffset = int (readI64 20)
    let indexLength = int (readI64 28)

    let metadataChecksum =
        Hashing.checksumRange data metadataOffset (indexOffset + indexLength - metadataOffset)

    writeU32 80 metadataChecksum

    let footerChecksum =
        Hashing.checksum (Array.sub data footerStart (Sstable.FooterSize - 4))

    writeU32 (Sstable.FooterSize - 4) footerChecksum

    File.WriteAllBytes(path, data)

[<Fact>]
let ``StorageEngine.getAt: 壊れたブロックは CorruptionError`` () =
    withDir "se-bad-block" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst dir 10L 0 [ Entry.put (intKey 1) 1UL (intRow 1); Entry.put (intKey 2) 2UL (intRow 2) ]

            // ブロック末尾の restart 件数を巨大値にして壊し、チェックサムは合わせ直す
            let block = (ok (Sstable.openPartial (FileMetaData.path dir 10L))).Index.[0]

            patchSstable dir 10L (fun data ->
                let at = block.Offset + block.Length - 4
                data.[at] <- 0xFFuy
                data.[at + 1] <- 0xFFuy
                data.[at + 2] <- 0xFFuy
                data.[at + 3] <- 0x00uy)

            let engine =
                { engine with
                    Version = Version.addFile f Version.Empty
                }

            expectErrorKind
                ErrorKind.CorruptionError
                "壊れたブロックは CorruptionError"
                (StorageEngine.getAt None (intKey 1) engine)))

[<Fact>]
let ``StorageEngine.getAt: 古い snapshot は SSTable 内の見える版まで戻る`` () =
    withDir "se-snapshot" (fun dir ->
        withEngine dir (fun engine ->
            let f =
                mkSst dir 10L 0 [ Entry.put (intKey 1) 5UL (intRow 5); Entry.put (intKey 1) 3UL (intRow 3) ]

            let engine =
                { engine with
                    Version = Version.addFile f Version.Empty
                }

            // snapshot 4 には最新版 seq5 は見えず、1つ古い seq3 が見える
            match ok (StorageEngine.getAt (Some 4UL) (intKey 1) engine) with
            | Some e -> Assert.Equal<uint64>(3UL, e.SeqNo)
            | None -> Assert.Fail "seq3 が見えるはず"))


[<Fact>]
let ``StorageEngine.scanAt: 多数のファイルをキー昇順にマージする`` () =
    withDir "se-many-files" (fun dir ->
        withEngine dir (fun engine ->
            // 32 ファイル × 2 エントリ。左偏ヒープのマージ（明示スタック）を多ソースで通す。
            let files =
                [ 1..32 ]
                |> List.map (fun i ->
                    mkSst
                        dir
                        (int64 (100 + i))
                        0
                        [
                            Entry.put (intKey (i * 2)) (uint64 i) (intRow (i * 2))
                            Entry.put (intKey (i * 2 + 100)) (uint64 i) (intRow (i * 2 + 100))
                        ])

            let engine =
                { engine with
                    Version = List.fold (fun v f -> Version.addFile f v) Version.Empty files
                }

            let got = ok (StorageEngine.scanAt None 0 [||] engine) |> Seq.toArray
            let expected = [ 2..2..64 ] @ [ 102..2..164 ]
            Assert.Equal(expected.Length, got.Length)

            expected
            |> List.iteri (fun i n ->
                Assert.True(Bytes.compare got.[i].Key (intKey n) = 0, sprintf "位置 %d が %d ではありません" i n))))

[<Fact>]
let ``StorageEngine.scanAt: memtable が空でも SSTable だけを走査できる`` () =
    withDir "se-file-only" (fun dir ->
        withEngine dir (fun engine ->
            // Close 後や再オープン直後は「SSTable だけ / memtable 空」になる。
            // 未オープンのファイルを開いたら pending から外さないと、毎行開き直して無限に積む。
            let f =
                mkSst dir 10L 0 [ Entry.put (intKey 1) 1UL (intRow 1); Entry.put (intKey 2) 2UL (intRow 2) ]

            let engine =
                { engine with
                    Version = Version.Empty |> Version.addFile f
                }

            let got = ok (StorageEngine.scanAt None 0 [||] engine) |> Seq.toArray
            Assert.Equal(2, got.Length)
            Assert.Equal<byte array>(intKey 1, got.[0].Key)
            Assert.Equal<byte array>(intKey 2, got.[1].Key)))

/// キー 1..2 のファイル（10）とキー 3..4 のファイル（11）を作り、11 のマジックを
/// 壊す（openTable が失敗する＝開封時に検出される壊れたファイル）。
let private twoFilesWithBrokenSecond (dir: string) =
    let f1 =
        mkSst dir 10L 0 [ Entry.put (intKey 1) 1UL (intRow 1); Entry.put (intKey 2) 2UL (intRow 2) ]

    let f2 =
        mkSst dir 11L 0 [ Entry.put (intKey 3) 3UL (intRow 3); Entry.put (intKey 4) 4UL (intRow 4) ]

    patchSstable dir 11L (fun data -> data.[data.Length - Sstable.FooterSize] <- 0x00uy)
    f1, f2

[<Fact>]
let ``StorageEngine.scanAt: limit 1 は後続の壊れたファイルを開かない`` () =
    withDir "se-lazy-open" (fun dir ->
        withEngine dir (fun engine ->
            let f1, f2 = twoFilesWithBrokenSecond dir

            let engine =
                { engine with
                    Version = Version.Empty |> Version.addFile f1 |> Version.addFile f2
                }

            // 先頭 1行はファイル 10 だけで足りる。ファイル 11 の下限はキー 3 なので開かない。
            let scanned = ok (StorageEngine.scanAt None 1 (intKey 1) engine) |> Seq.toArray
            Assert.Equal(1, scanned.Length)
            Assert.Equal(0, Bytes.compare scanned.[0].Key (intKey 1))

            // 全 4行が必要ならファイル 11 を開くので、壊れていることを検出できる。
            expectErrorKind
                ErrorKind.CorruptionError
                "必要なファイルの破損は検出する"
                (StorageEngine.scanAt None 10 (intKey 1) engine)))

[<Fact>]
let ``StorageEngine.scanAt: 制限なしの走査は壊れたファイルを先に検出する`` () =
    withDir "se-eager-open" (fun dir ->
        withEngine dir (fun engine ->
            let f1, f2 = twoFilesWithBrokenSecond dir

            let engine =
                { engine with
                    Version = Version.Empty |> Version.addFile f1 |> Version.addFile f2
                }

            // 制限なしの走査は全ファイルを読むので、開封時に全ファイルを開いて
            // 壊れたファイルを Result で検出する。
            expectErrorKind ErrorKind.CorruptionError "制限なしの走査は壊れたファイルを検出する" (StorageEngine.scanAt None 0 [||] engine)))

[<Fact>]
let ``StorageEngine.scanAt: 欠落ファイルは必要になった時点で検出する`` () =
    withDir "se-missing-open" (fun dir ->
        withEngine dir (fun engine ->
            let f1, f2 = twoFilesWithBrokenSecond dir
            File.Delete(FileMetaData.path dir 11L)

            let engine =
                { engine with
                    Version = Version.Empty |> Version.addFile f1 |> Version.addFile f2
                }

            // limit 1 はファイル 11 を必要としないので、欠落していても先頭 1 行は返せる。
            let scanned = ok (StorageEngine.scanAt None 1 (intKey 1) engine) |> Seq.toArray
            Assert.Equal(1, scanned.Length)
            // 全行が必要ならファイル 11 を開こうとして IoError になる。
            expectErrorKind ErrorKind.IoError "欠落ファイルは必要になれば検出する" (StorageEngine.scanAt None 10 (intKey 1) engine)))

[<Fact>]
let ``StorageEngine.getAt / scanAt: 必要なブロックだけを部分読みする`` () =
    withDir "se-partial-blocks" (fun dir ->
        withEngine dir (fun engine ->
            // 300件 = 3ブロック（128件ずつ）
            let entries =
                [ for i in 0..299 -> Entry.put (intKey i) (uint64 (i + 1)) (intRow i) ]

            let f = mkSst dir 10L 0 entries

            // 2番目のブロックの内部を壊す（全体チェックサムも合わなくなる）。
            let path = FileMetaData.path dir 10L
            let reader = ok (Sstable.openPartial path)
            let block = reader.Index.[1]
            let data = File.ReadAllBytes path
            data.[block.Offset + 2] <- data.[block.Offset + 2] ^^^ 0xFFuy
            File.WriteAllBytes(path, data)
            File.SetLastWriteTimeUtc(path, System.DateTime.UtcNow.AddSeconds 1.0)

            let engine =
                { engine with
                    Version = Version.Empty |> Version.addFile f
                }

            // ブロック 0 のキーは読める
            Assert.True((ok (StorageEngine.getAt None (intKey 1) engine)).IsSome)
            // ブロック 1 のキーは壊れている
            expectErrorKind ErrorKind.CorruptionError "必要なブロックの破損は検出する" (StorageEngine.getAt None (intKey 200) engine)
            // limit 5 はブロック 0 だけで足りる
            let five = ok (StorageEngine.scanAt None 5 [||] engine) |> Seq.toArray
            Assert.Equal<int>(5, five.Length)
            // 全件走査は全体チェックサムで壊れていることを検出する
            expectErrorKind ErrorKind.CorruptionError "全件走査は全体チェックサムで検出する" (StorageEngine.scanAt None 0 [||] engine)))

[<Fact>]
let ``StorageEngine.maybeFlush: flush 後に compaction が不要なら compactOnce は noop で返す`` () =
    withDir "se-mf-noop" (fun dir ->
        withEngine dir (fun engine ->
            let engine = { engine with MemLimitBytes = 1 }

            let engine =
                ok (StorageEngine.writeBatch [| Entry.put (intKey 1) 1UL (intRow 1) |] engine)

            let after = ok (StorageEngine.maybeFlush engine)
            // flush した 1 ファイルが L0 に残り、compaction は走っていない
            Assert.Equal(1, (Version.filesAt 0 after.Version).Length)))

[<Fact>]
let ``StorageEngine.maybeFlush: 4 本目の L0 で compactOnce が L0→L1 compaction を実行する`` () =
    withDir "se-mf-run" (fun dir ->
        withEngine dir (fun engine ->
            let f1 = mkSst dir 10L 0 [ Entry.put (intKey 1) 1UL (intRow 1) ]
            let f2 = mkSst dir 11L 0 [ Entry.put (intKey 2) 2UL (intRow 2) ]
            let f3 = mkSst dir 12L 0 [ Entry.put (intKey 3) 3UL (intRow 3) ]

            let version =
                Version.Empty |> Version.addFile f1 |> Version.addFile f2 |> Version.addFile f3

            let version = { version with NextFileNumber = 100L }

            let engine =
                { engine with
                    Version = version
                    MemLimitBytes = 1
                }

            let engine =
                ok (StorageEngine.writeBatch [| Entry.put (intKey 4) 4UL (intRow 4) |] engine)

            let after = ok (StorageEngine.maybeFlush engine)
            // compaction が走り、L0 は空、L1 にファイルが存在する
            Assert.Equal(0, (Version.filesAt 0 after.Version).Length)
            Assert.True((Version.filesAt 1 after.Version).Length > 0)
            // 元の 3 ファイルはディスクから削除される
            Assert.False(File.Exists(FileMetaData.path dir 10L))
            Assert.False(File.Exists(FileMetaData.path dir 11L))
            Assert.False(File.Exists(FileMetaData.path dir 12L))))

[<Fact>]
let ``StorageEngine.maybeFlush: L0 に壊れたファイルがあると compactOnce が Error を返す`` () =
    withDir "se-mf-err" (fun dir ->
        withEngine dir (fun engine ->
            let f1 = mkSst dir 10L 0 [ Entry.put (intKey 1) 1UL (intRow 1) ]
            let f2 = mkSst dir 11L 0 [ Entry.put (intKey 2) 2UL (intRow 2) ]
            let f3 = mkSst dir 12L 0 [ Entry.put (intKey 3) 3UL (intRow 3) ]
            // チェックサムを合わせずに 1 バイト壊す
            let path = FileMetaData.path dir 11L
            let data = File.ReadAllBytes path
            data.[0] <- data.[0] ^^^ 0xFFuy
            File.WriteAllBytes(path, data)

            let version =
                Version.Empty |> Version.addFile f1 |> Version.addFile f2 |> Version.addFile f3

            let version = { version with NextFileNumber = 100L }

            let engine =
                { engine with
                    Version = version
                    MemLimitBytes = 1
                }

            let engine =
                ok (StorageEngine.writeBatch [| Entry.put (intKey 4) 4UL (intRow 4) |] engine)

            expectError "壊れた入力は Error" (StorageEngine.maybeFlush engine)))

[<Fact>]
let ``DiskStorageEngine.MaybeFlush: wrapper 経由でも上限超過を flush する`` () =
    withDir "se-disk-flush" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)
        let disk = DiskStorageEngine { engine with MemLimitBytes = 1 } :> IStorageEngine
        let disk = ok (disk.WriteBatch [| Entry.put (intKey 1) 1UL (intRow 1) |])
        let disk = ok (disk.MaybeFlush())

        Assert.NotEmpty(Directory.GetFiles(dir, "*" + FileMetaData.SstExtension))
        Assert.True((ok (disk.GetAt None (intKey 1))).IsSome))

[<Fact>]
let ``StorageEngine.openDatabase: 実ファイルが消えていても NextFileNumber は進む`` () =
    withDir "re-next-file" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        // Version が参照するファイルが実在しない状態 + 番号 0 の WAL を作る
        let _ = ok (Manifest.openOrCreate dir)

        let _ =
            ok (Manifest.logAndApply dir 1L Version.Empty (VersionEdit.addFile (fmd 1L 0 1 1)))

        let _ = ok (Wal.create dir 0L)

        let engine = ok (StorageEngine.openDatabase dir)
        // 消えたファイルの削除で NextFileNumber は 2 に進み、0 番の WAL があっても巻き戻らない
        Assert.Equal(2L, engine.Version.NextFileNumber)
        Assert.Empty(Version.allFiles engine.Version))

[<Fact>]
let ``StorageEngine.openDatabase: 最大 SeqNo はファイルの並び順に関係なく取れる`` () =
    withDir "re-seqmax" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let _ = ok (Manifest.openOrCreate dir)
        // SeqNo が「後のファイルで小さくなる」並び（キー順では fA → fB）
        let fA = mkSst dir 10L 0 [ Entry.put (intKey 1) 5UL (intRow 1) ]
        let fB = mkSst dir 11L 0 [ Entry.put (intKey 2) 3UL (intRow 2) ]
        let v1, m1 = ok (Manifest.logAndApply dir 1L Version.Empty (VersionEdit.addFile fA))
        let _ = ok (Manifest.logAndApply dir m1 v1 (VersionEdit.addFile fB))

        let engine = ok (StorageEngine.openDatabase dir)
        // 再開後はファイル内の最大 SeqNo（5）の次から書ける
        Assert.Equal(6UL, engine.NextSeqNo))

[<Fact>]
let ``StorageEngine.openDatabase: 孤児の SSTable は削除せず退避する`` () =
    withDir "re5" (fun dir ->
        withEngine dir (fun _ -> ())
        // MANIFEST に無いファイルを 1 個置いてから開く
        File.WriteAllBytes(Path.Combine(dir, "000999.sst"), [| 1uy; 2uy; 3uy |])

        withEngine dir (fun _ ->
            // 消さずに .orphans へ移す（manifest の読み落としで消すと復元できない）
            Assert.False(File.Exists(Path.Combine(dir, "000999.sst")))
            Assert.Single(Directory.GetFiles(Path.Combine(dir, ".orphans"))) |> ignore))
