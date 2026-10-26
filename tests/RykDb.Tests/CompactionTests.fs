// ===========================================================================
// Compaction.fs のテスト: Leveled compaction と不要バージョン回収（GC）
// ===========================================================================

module RykDb.CompactionTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 与えたエントリ列を SSTable として書き、メタデータを返す。
let private mkSst (dir: string) (num: int64) (level: int) (entries: Entry list) =
    entries
    |> List.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
    |> List.toArray
    |> FileMetaData.writeSstable dir num level 0.01
    |> ok

/// 書いた SSTable を開く（compact の openScan と同じ経路）。
let private openBack (dir: string) (f: FileMetaData) =
    protect (fun () -> File.ReadAllBytes(FileMetaData.path dir f.FileNumber))
    >>= Sstable.openTable
    >>= fun reader -> ret (Sstable.rangeScan reader [||] 0)

/// 書いた SSTable を読み戻す（検証用）。
let private readBack (dir: string) (f: FileMetaData) = openBack dir f >>= SstScan.toArray

/// 閾値を全レベル 0 にして「最初の非空レベル」から compaction させる。
let private pickFirstLevel =
    { CompactionOptions.defaults with
        MaxLevelBytes = Array.create Levels.Count 0L
    }

/// 1回 compact して結果だけ返す。
let private compactAll (dir: string) (version: Version) (activeSnapshot: uint64) (options: CompactionOptions) =
    ok (Compaction.compact dir version options activeSnapshot 100L (openBack dir))

let private entryOf (i: int) =
    Entry.put (intKey i) (uint64 i) (intRow i)

[<Fact>]
let ``Compaction: 同名キーは最新版と最古 snapshot が見る版を残す`` () =
    withDir "comp-gc" (fun dir ->
        let f =
            mkSst
                dir
                10L
                1
                [
                    Entry.put (intKey 1) 5UL (intRow 5)
                    Entry.put (intKey 1) 3UL (intRow 3)
                    Entry.put (intKey 1) 1UL (intRow 1)
                    Entry.put (intKey 2) 2UL (intRow 2)
                ]

        let version = Version.addFile f Version.empty
        let result, _ = compactAll dir version 4UL pickFirstLevel
        // snapshot 4 が見るのは key1 の seq 3（SeqNo <= 4 の最新）。
        // seq 1 だけがどの snapshot からも見えないので落ちる。
        Assert.Equal(1, result.DroppedVersions)
        Assert.Equal(0, result.DroppedTombstones)
        Assert.Equal<int64 list>([ 10L ], result.Removed |> List.map _.FileNumber)

        let out = List.exactlyOne result.Added
        Assert.Equal(2, out.Level)
        Assert.Equal(3L, out.EntryCount)

        let entries = ok (readBack dir out)
        Assert.Equal<int>(3, entries.Length)
        Assert.Equal<byte array>(intKey 1, entries.[0].Key)
        Assert.Equal<uint64>(5UL, entries.[0].SeqNo)
        Assert.Equal<byte array>(intKey 1, entries.[1].Key)
        Assert.Equal<uint64>(3UL, entries.[1].SeqNo)
        Assert.Equal<byte array>(intKey 2, entries.[2].Key))

[<Fact>]
let ``Compaction: snapshot が生きている版は GC しない`` () =
    withDir "comp-snap" (fun dir ->
        let f =
            mkSst
                dir
                10L
                1
                [
                    Entry.put (intKey 1) 5UL (intRow 5)
                    Entry.put (intKey 1) 3UL (intRow 3)
                    Entry.put (intKey 1) 1UL (intRow 1)
                ]

        let version = Version.addFile f Version.empty
        // snapshot 2 が見るのは seq 1。落とすとデータ損失になる。
        // seq 5, 3 も将来の snapshot 用に残るので、何も落ちない。
        let result, _ = compactAll dir version 2UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(3, entries.Length)
        Assert.Equal<uint64>(5UL, entries.[0].SeqNo)
        Assert.Equal<uint64>(3UL, entries.[1].SeqNo)
        Assert.Equal<uint64>(1UL, entries.[2].SeqNo))

[<Fact>]
let ``Compaction: 新しい版があっても active snapshot が見る旧版を残す`` () =
    withDir "comp-gc-visible" (fun dir ->
        let f =
            mkSst dir 10L 1 [ Entry.put (intKey 1) 10UL (intRow 10); Entry.put (intKey 1) 5UL (intRow 5) ]

        let version = Version.addFile f Version.empty
        // snapshot 7 は seq 10 を見ない。seq 5 が唯一の可視版になる。
        let result, _ = compactAll dir version 7UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(2, entries.Length)
        Assert.Equal<uint64>(10UL, entries.[0].SeqNo)
        Assert.Equal<uint64>(5UL, entries.[1].SeqNo))

[<Fact>]
let ``Compaction: active snapshot より古い版だけなら最新版以外を落とす`` () =
    withDir "comp-gc-optimal" (fun dir ->
        let f =
            mkSst dir 10L 1 [ Entry.put (intKey 1) 5UL (intRow 5); Entry.put (intKey 1) 3UL (intRow 3) ]

        let version = Version.addFile f Version.empty
        // active が無いときの境界は LastCommitSeqNo + 1。
        // 最新版 5 が境界以下なので、3 はどの snapshot からも見えない。
        let result, _ = compactAll dir version 6UL pickFirstLevel
        Assert.Equal(1, result.DroppedVersions)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(1, entries.Length)
        Assert.Equal<uint64>(5UL, entries.[0].SeqNo))

[<Fact>]
let ``Compaction: 閾値を超えないレベルは compaction しない`` () =
    withDir "comp-none" (fun dir ->
        let f = mkSst dir 10L 1 [ entryOf 1 ]
        let version = Version.addFile f Version.empty
        // 既定の閾値（L1 = 10MiB）には遠く届かない
        let result, info = compactAll dir version 0UL CompactionOptions.defaults
        Assert.Empty result.Added
        Assert.Empty result.Removed
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(-1L, info.FileNumber)
        Assert.True(File.Exists(FileMetaData.path dir 10L)))

[<Fact>]
let ``Compaction: 上のレベルの重なるファイルも入力に含める`` () =
    withDir "comp-inputs" (fun dir ->
        let f1 =
            mkSst dir 10L 1 [ entryOf 1; entryOf 2; Entry.put (intKey 3) 3UL (intRow 3) ]
        // key3 で f1 と重なる L2 のファイル
        let f2 = mkSst dir 11L 2 [ Entry.put (intKey 3) 33UL (intRow 3); entryOf 4 ]
        let version = Version.empty |> Version.addFile f1 |> Version.addFile f2
        let result, _ = compactAll dir version 10UL pickFirstLevel
        // 入力は L1 の f1 と、重なる L2 の f2 の両方
        Assert.Equal<int64 list>([ 10L; 11L ], result.Removed |> List.map _.FileNumber)
        // key3 の seq 3 は snapshot 10 が見る版なので残す（seq 33 はそれより新しい）
        Assert.Equal(0, result.DroppedVersions)

        let out = List.exactlyOne result.Added
        Assert.Equal(2, out.Level)
        Assert.Equal(5L, out.EntryCount)

        let entries = ok (readBack dir out)
        Assert.Equal<int>(5, entries.Length)
        Assert.Equal<uint64>(1UL, entries.[0].SeqNo)
        Assert.Equal<uint64>(33UL, entries.[2].SeqNo))

[<Fact>]
let ``Compaction: 重ならない上のレベルファイルは入力に含めない`` () =
    withDir "comp-no-overlap" (fun dir ->
        // L1 のキー域は 5..6
        let f1 = mkSst dir 10L 1 [ entryOf 5; entryOf 6 ]
        // L2 には下側（0）と上側（9）の、重ならないファイルを置く
        let below = mkSst dir 11L 2 [ entryOf 0 ]
        let above = mkSst dir 12L 2 [ entryOf 9 ]

        let version =
            Version.empty
            |> Version.addFile f1
            |> Version.addFile below
            |> Version.addFile above

        let result, _ = compactAll dir version 0UL pickFirstLevel
        // 入力は L1 の f1 だけ。重ならない L2 の 2 ファイルは入力に含めない
        Assert.Equal<int64 list>([ 10L ], result.Removed |> List.map _.FileNumber)
        Assert.True(File.Exists(FileMetaData.path dir 11L))
        Assert.True(File.Exists(FileMetaData.path dir 12L)))

[<Fact>]
let ``Compaction: 最下位レベルへの出力では tombstone を落とす`` () =
    withDir "comp-bottom" (fun dir ->
        let f =
            mkSst dir 10L 5 [ Entry.delete (intKey 1) 5UL; Entry.put (intKey 2) 2UL (intRow 2) ]

        let version = Version.addFile f Version.empty
        // 全レベル閾値 0 → L5 が選ばれ、出力は L6（最下位）
        let result, _ = compactAll dir version 0UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(1, result.DroppedTombstones)

        let out = List.exactlyOne result.Added
        Assert.Equal(Levels.Max, out.Level)

        let entries = ok (readBack dir out)
        Assert.Equal<int>(1, entries.Length)
        Assert.Equal<byte array>(intKey 2, entries.[0].Key))

[<Fact>]
let ``Compaction: 途中のレベルへの出力では tombstone を残す`` () =
    withDir "comp-keep-tomb" (fun dir ->
        let f =
            mkSst dir 10L 1 [ Entry.delete (intKey 1) 5UL; Entry.put (intKey 2) 2UL (intRow 2) ]

        let version = Version.addFile f Version.empty
        // 出力は L2。まだ下に古い版が残りうるので tombstone は消せない
        let result, _ = compactAll dir version 0UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(0, result.DroppedTombstones)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(2, entries.Length)
        Assert.True(entries.[0].IsDelete))

[<Fact>]
let ``Compaction: 最下位レベルの compaction で tombstone だけなら出力しない`` () =
    withDir "comp-last-empty" (fun dir ->
        let f =
            mkSst dir 10L Levels.Max [ Entry.delete (intKey 1) 5UL; Entry.delete (intKey 2) 6UL ]

        let version = Version.addFile f Version.empty
        // L6 だけが閾値超え → 出力先も L6。tombstone は全部落ちて空になる
        let result, info = compactAll dir version 0UL pickFirstLevel
        Assert.Empty result.Added
        Assert.Equal<int64 list>([ 10L ], result.Removed |> List.map _.FileNumber)
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(2, result.DroppedTombstones)
        Assert.Equal(Levels.Max, info.Level)
        // 実ファイルの削除は呼び出し側（MANIFEST 更新後）の責務
        Assert.True(File.Exists(FileMetaData.path dir 10L)))

[<Fact>]
let ``Compaction: L0 のファイル数が閾値に達したら L1 へ compaction する`` () =
    withDir "comp-l0" (fun dir ->
        let files =
            [
                for i in 1..3 -> mkSst dir (int64 (10 + i)) 0 [ Entry.put (intKey i) (uint64 i) (intRow i) ]
            ]

        let version = files |> List.fold (fun v f -> Version.addFile f v) Version.empty
        let before, _ = compactAll dir version 0UL CompactionOptions.defaults
        Assert.Empty before.Added
        Assert.Equal(3, Version.filesAt version 0 |> Array.length)

        let fourth = mkSst dir 20L 0 [ Entry.put (intKey 4) 4UL (intRow 4) ]
        let version = Version.addFile fourth version
        let result, _ = compactAll dir version 0UL CompactionOptions.defaults
        Assert.Equal<int64 list>([ 11L; 12L; 13L; 20L ], result.Removed |> List.map _.FileNumber |> List.sort)
        Assert.Equal(1, result.Added.Length)
        Assert.Equal(1, result.Added.[0].Level)
        Assert.Equal(4L, result.Added.[0].EntryCount))

[<Fact>]
let ``Compaction: 古い版が残るなら最下位でも tombstone を消さない`` () =
    withDir "comp-keep-old-tomb" (fun dir ->
        let f =
            mkSst dir 10L Levels.Max [ Entry.delete (intKey 1) 5UL; Entry.put (intKey 1) 3UL (intRow 1) ]

        let version = Version.addFile f Version.empty
        let result, _ = compactAll dir version 0UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(0, result.DroppedTombstones)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(2, entries.Length)
        Assert.True(entries.[0].IsDelete)
        Assert.Equal<uint64>(3UL, entries.[1].SeqNo))

[<Fact>]
let ``Compaction: 出力の書き込みに失敗したら Error を返す`` () =
    withDir "comp-write-fail" (fun dir ->
        let f = mkSst dir 10L 1 [ entryOf 1 ]
        let version = Version.addFile f Version.empty
        // 存在しないディレクトリを出力先にする → 書き込みが IoError で失敗する
        let missing = Path.Combine(dir, "missing")
        let r = Compaction.compact missing version pickFirstLevel 0UL 100L (openBack dir)
        expectErrorKind ErrorKind.IoError "書き込み失敗は IoError になる" r
        // 入力ファイルは消さない（削除は呼び出し側の責務）
        Assert.True(File.Exists(FileMetaData.path dir 10L)))

[<Fact>]
let ``Compaction: 入力の読み込みに失敗したらそのエラーを返す`` () =
    withDir "comp-read-fail" (fun dir ->
        let f = fmd 10L 1 1 1
        let version = Version.addFile f Version.empty
        let broken (_: FileMetaData) = err (DbError.corruption "壊れた SSTable")
        let r = Compaction.compact dir version pickFirstLevel 0UL 100L broken
        expectErrorKind ErrorKind.CorruptionError "読み込みエラーはそのまま伝播する" r)

[<Fact>]
let ``Compaction: 出力はエントリ数ではなく OutputFileTargetBytes ごとに分ける`` () =
    withDir "comp-split-bytes" (fun dir ->
        let text = String.replicate 2000 "x"

        let es =
            [ 1..5 ]
            |> List.map (fun i -> Entry.put (intKey i) (uint64 i) [| SqlValue.Text text |])

        let f = mkSst dir 10L 1 es
        let version = Version.addFile f Version.empty

        // 1エントリ約 2KB。旧実装の「128エントリ」では 1ファイルに収まるが、
        // 目標 5KB なら 2 + 2 + 1 に割れる。
        let options =
            { pickFirstLevel with
                OutputFileTargetBytes = 5000L
            }

        let result, _ = compactAll dir version 0UL options

        let counts = result.Added |> List.sortBy _.FileNumber |> List.map _.EntryCount

        Assert.Equal<int64 list>([ 2L; 2L; 1L ], counts))

[<Fact>]
let ``Compaction: 同じキーの版は出力ファイルを跨がない`` () =
    withDir "comp-key-boundary" (fun dir ->
        let f =
            mkSst
                dir
                10L
                1
                [
                    Entry.put (intKey 1) 5UL (intRow 5)
                    Entry.put (intKey 1) 3UL (intRow 3)
                    Entry.put (intKey 1) 1UL (intRow 1)
                    Entry.put (intKey 2) 2UL (intRow 2)
                ]

        let version = Version.addFile f Version.empty

        // 目標 1 バイト → キーが変わるたびにファイルを切り替える。
        let options =
            { pickFirstLevel with
                OutputFileTargetBytes = 1L
            }

        let result, _ = compactAll dir version 0UL options
        Assert.Equal(2, result.Added.Length)

        for out in result.Added do
            let entries = ok (readBack dir out)
            let keys = entries |> Array.map _.Key |> Array.distinct
            Assert.Equal(1, keys.Length)

        // GC なしなので 4 版すべてが残る（どのファイルにも欠けない）。
        let all =
            result.Added |> List.collect (fun out -> ok (readBack dir out) |> Array.toList)

        Assert.Equal<int>(4, all.Length))

[<Fact>]
let ``Compaction: 複数ブロックの入力をストリーミングで読み書きできる`` () =
    withDir "comp-blocks" (fun dir ->
        // 1 ブロック 128 件なので 400 件は 4 ブロック（部分読みのバッチも跨ぐ）。
        let entries =
            [ 0..399 ]
            |> List.map (fun i -> Entry.put (intKey i) (uint64 (i + 1)) (intRow i))

        let f = mkSst dir 10L 1 entries
        let version = Version.addFile f Version.empty
        let result, _ = compactAll dir version 0UL pickFirstLevel

        let out = List.exactlyOne result.Added
        Assert.Equal(400L, out.EntryCount)

        let read = ok (readBack dir out)
        Assert.Equal<int>(400, read.Length)
        Assert.Equal<int>(400, read |> Array.map _.Key |> Array.distinct |> Array.length)

        let ordered =
            read
            |> Array.pairwise
            |> Array.forall (fun (a, b) -> Bytes.compare a.Key b.Key <= 0)

        Assert.True(ordered, "出力がキー昇順ではありません"))

[<Fact>]
let ``Compaction: 多数の入力ファイルをストリーミングでマージする`` () =
    withDir "comp-many-files" (fun dir ->
        let perFile = 300

        let files =
            [ 0..3 ]
            |> List.map (fun fi ->
                let entries =
                    [ 0 .. perFile - 1 ]
                    |> List.map (fun i ->
                        let n = fi * perFile + i
                        Entry.put (intKey n) (uint64 (n + 1)) (intRow n))

                mkSst dir (int64 (10 + fi)) 0 entries)

        let version = files |> List.fold (fun v f -> Version.addFile f v) Version.empty
        let result, _ = compactAll dir version 0UL CompactionOptions.defaults

        // L0 が閾値（4）に達したので 1 回で L1 へ。
        let out = List.exactlyOne result.Added
        Assert.Equal(int64 (perFile * files.Length), out.EntryCount)

        let read = ok (readBack dir out)
        Assert.Equal(perFile * files.Length, read.Length)

        let ordered =
            read
            |> Array.pairwise
            |> Array.forall (fun (a, b) -> Bytes.compare a.Key b.Key <= 0)

        Assert.True(ordered, "マージ結果がキー昇順ではありません"))
