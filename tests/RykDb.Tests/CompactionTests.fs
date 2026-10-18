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

/// 書いた SSTable を読み戻す（compact の readFile と同じ経路）。
let private readBack (dir: string) (f: FileMetaData) =
    protect (fun () -> File.ReadAllBytes(Sstable.path dir f.FileNumber))
    >>= Sstable.openTable
    >>= fun reader -> Sstable.rangeScan reader [||] 0 |> SstScan.toArray

/// 閾値を全レベル 0 にして「最初の非空レベル」から compaction させる。
let private pickFirstLevel =
    { CompactionOptions.defaults with
        MaxLevelBytes = Array.create Levels.Count 0L
    }

/// 1回 compact して結果だけ返す。
let private compactAll (dir: string) (version: Version) (activeSnapshot: uint64) (options: CompactionOptions) =
    ok (Compaction.compact dir version options activeSnapshot 100L (readBack dir))

let private entryOf (i: int) =
    Entry.put (intKey i) (uint64 i) (intRow i)

[<Fact>]
let ``Compaction: 同一キーは最新版を残し、snapshot より古い版を落とす`` () =
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
        // key1 の seq 1, 3 は snapshot 4 より古いので捨てる
        Assert.Equal(2, result.DroppedVersions)
        Assert.Equal(0, result.DroppedTombstones)
        Assert.Equal<int64 list>([ 10L ], result.Removed |> List.map _.FileNumber)

        let out = List.exactlyOne result.Added
        Assert.Equal(2, out.Level)
        Assert.Equal(2L, out.EntryCount)

        let entries = ok (readBack dir out)
        Assert.Equal<int>(2, entries.Length)
        Assert.Equal<byte array>(intKey 1, entries.[0].Key)
        Assert.Equal<uint64>(5UL, entries.[0].SeqNo)
        Assert.Equal<byte array>(intKey 2, entries.[1].Key))

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
        // snapshot 2 が見ている seq 3 は残し、seq 1 だけ落とす
        let result, _ = compactAll dir version 2UL pickFirstLevel
        Assert.Equal(1, result.DroppedVersions)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(2, entries.Length)
        Assert.Equal<uint64>(5UL, entries.[0].SeqNo)
        Assert.Equal<uint64>(3UL, entries.[1].SeqNo))

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
        Assert.True(File.Exists(Sstable.path dir 10L)))

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
        // key3 の古い版（seq 3 < snapshot 10）だけ落ちる
        Assert.Equal(1, result.DroppedVersions)

        let out = List.exactlyOne result.Added
        Assert.Equal(2, out.Level)
        Assert.Equal(4L, out.EntryCount)

        let entries = ok (readBack dir out)
        Assert.Equal<int>(4, entries.Length)
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
        Assert.True(File.Exists(Sstable.path dir 11L))
        Assert.True(File.Exists(Sstable.path dir 12L)))

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
        Assert.True(File.Exists(Sstable.path dir 10L)))

[<Fact>]
let ``Compaction: 出力の書き込みに失敗したら Error を返す`` () =
    withDir "comp-write-fail" (fun dir ->
        let f = mkSst dir 10L 1 [ entryOf 1 ]
        let version = Version.addFile f Version.empty
        // 存在しないディレクトリを出力先にする → 書き込みが IoError で失敗する
        let missing = Path.Combine(dir, "missing")
        let r = Compaction.compact missing version pickFirstLevel 0UL 100L (readBack dir)
        expectErrorKind ErrorKind.IoError "書き込み失敗は IoError になる" r
        // 入力ファイルは消さない（削除は呼び出し側の責務）
        Assert.True(File.Exists(Sstable.path dir 10L)))

[<Fact>]
let ``Compaction: 入力の読み込みに失敗したらそのエラーを返す`` () =
    withDir "comp-read-fail" (fun dir ->
        let f = fmd 10L 1 1 1
        let version = Version.addFile f Version.empty
        let broken (_: FileMetaData) = err (DbError.corruption "壊れた SSTable")
        let r = Compaction.compact dir version pickFirstLevel 0UL 100L broken
        expectErrorKind ErrorKind.CorruptionError "読み込みエラーはそのまま伝播する" r)
