// ===========================================================================
// Compaction.fs のテスト: Leveled compaction と不要バージョン回収（GC）
// ===========================================================================

module RykDb.CompactionTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 与えたエントリ列を SSTable として書き、メタデータを返す。
let private mkSst (dir: string) (num: int64) (level: int) (entries: Entry list) =
    let sstEntries =
        entries
        |> List.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
        |> List.toArray

    FileMetaData.writeSstable dir num level 0.01 sstEntries.Length sstEntries |> ok

/// 書いた SSTable を開く（検証用。値は復号する）。
let private openBack (dir: string) (f: FileMetaData) =
    SstableReader.openPartial (FileMetaData.path dir f.FileNumber)
    >>= fun partial -> ret (SstableReader.rangeScanPartial [||] 0 partial)

/// compaction の入力用。値は復号せずスライスのまま渡す。
let private openBackRaw (dir: string) (f: FileMetaData) =
    SstableReader.openPartial (FileMetaData.path dir f.FileNumber)
    >>= fun partial -> ret (SstableReader.rangeScanPartialRaw [||] 0 partial)

/// 書いた SSTable を読み戻す（検証用）。
let private readBack (dir: string) (f: FileMetaData) = openBack dir f >>= SstScan.toArray

/// 閾値を全レベル 0 にして「最初の非空レベル」から compaction させる。
let private pickFirstLevel =
    { CompactionOptions.Defaults with
        MaxLevelBytes = Array.create Levels.Count 0L
    }

/// 1回 compact して結果だけ返す。
let private compactAll (dir: string) (version: Version) (activeSnapshot: uint64) (options: CompactionOptions) =
    ok (Compaction.compact dir version options activeSnapshot 100L (openBackRaw dir))

let private entryOf (i: int) =
    Entry.put (intKey i) (uint64 i) (intRow i)

/// snapshot 以下で最も新しい版の値（tombstone は None）。
let private visibleValue (entries: SstEntry list) (snapshot: uint64) =
    entries
    |> List.filter (fun e -> e.SeqNo <= snapshot)
    |> List.sortByDescending (fun e -> e.SeqNo)
    |> List.tryHead
    |> Option.bind (fun e -> e.Value)

let private showValue =
    function
    | None -> "None"
    | Some(v: SqlValue array) -> v |> Array.map SqlLiteral.toSqlLiteral |> String.concat ","

[<Fact>]
let ``Compaction.gcGroup: 任意の snapshot で可視行が変わらない`` () =
    let random = System.Random 20261113

    for _ in 1..300 do
        let n = random.Next(1, 8)

        let entries =
            [
                for seqNo in 1..n ->
                    SstEntry.make
                        (intKey 1)
                        (uint64 seqNo)
                        (if random.Next 3 = 0 then
                             None
                         else
                             Some [| SqlValue.Int(int64 seqNo) |])
            ]
            |> List.sortWith (fun a b -> compare b.SeqNo a.SeqNo)

        let newest = List.head entries
        let older = List.tail entries
        let activeSnapshot = uint64 (random.Next(0, n + 2))
        let dropTombstones = random.Next 2 = 0

        let kept, dropped, tombstones =
            Compaction.gcGroup activeSnapshot dropTombstones older newest

        // 件数の会計が合う（入力の全版が「残す / 捨てる / tombstone を捨てる」のどれか）。
        Assert.Equal<int>(entries.Length, kept.Length + dropped + tombstones)
        // 残った版は入力の部分集合で、SeqNo が重複しない。
        Assert.Equal<int>(kept.Length, (kept |> List.map _.SeqNo |> List.distinct).Length)

        Assert.True(
            kept
            |> List.forall (fun e -> entries |> List.exists (fun x -> x.SeqNo = e.SeqNo))
        )

        // activeSnapshot 以降のどの snapshot でも可視行が一致する。
        for snapshot in activeSnapshot .. uint64 n + 1UL do
            Assert.Equal<string>(showValue (visibleValue entries snapshot), showValue (visibleValue kept snapshot))

[<Fact>]
let ``Compaction.compact: ホットキーの多数の版は snapshot 以降の全版と境界の 1版を残す`` () =
    withDir "compact-hot-key" (fun dir ->
        // 1つのキーに 600版。ファイルを跨がせてマージ経路も通す。
        let versions = [ 1..600 ]
        let half = 300

        let file1 =
            mkSst
                dir
                1L
                0
                (versions
                 |> List.take half
                 |> List.rev
                 |> List.map (fun i -> Entry.put (intKey 1) (uint64 i) (intRow i)))

        let file2 =
            mkSst
                dir
                2L
                0
                (versions
                 |> List.skip half
                 |> List.rev
                 |> List.map (fun i -> Entry.put (intKey 1) (uint64 i) (intRow i)))

        let version = Version.Empty |> Version.addFile file1 |> Version.addFile file2
        let version = { version with NextFileNumber = 10L }

        let result, _ =
            compactAll
                dir
                version
                500UL
                { pickFirstLevel with
                    L0CompactionTrigger = 1
                }

        Assert.Single(result.Added) |> ignore
        let entries = ok (readBack dir (List.exactlyOne result.Added))
        // snapshot より新しい版（501〜600）と、snapshot が見る最新版（500）を残し、
        // それより古い版（1〜499）だけを落とす（版グループのストリーミング経路）。
        let expected = [ for i in 600..-1..500 -> uint64 i ]
        Assert.Equal<uint64 list>(expected, entries |> Array.toList |> List.map _.SeqNo)
        Assert.Equal<int>(499, result.DroppedVersions)
        Assert.Equal(0, result.DroppedTombstones)
        // 可視行は変わらない（どの snapshot でも同じ版が見える）。
        let keptEntries = Array.toList entries
        Assert.Equal("600", showValue (visibleValue keptEntries 600UL))
        Assert.Equal("550", showValue (visibleValue keptEntries 550UL))
        Assert.Equal("500", showValue (visibleValue keptEntries 500UL)))

[<Fact>]
let ``Compaction.compact: 同名キーは最新版と最古 snapshot が見る版を残す`` () =
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

        let version = Version.addFile f Version.Empty
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
let ``Compaction.compact: snapshot が生きている版は GC しない`` () =
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

        let version = Version.addFile f Version.Empty
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
let ``Compaction.compact: 新しい版があっても active snapshot が見る旧版を残す`` () =
    withDir "comp-gc-visible" (fun dir ->
        let f =
            mkSst dir 10L 1 [ Entry.put (intKey 1) 10UL (intRow 10); Entry.put (intKey 1) 5UL (intRow 5) ]

        let version = Version.addFile f Version.Empty
        // snapshot 7 は seq 10 を見ない。seq 5 が唯一の可視版になる。
        let result, _ = compactAll dir version 7UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(2, entries.Length)
        Assert.Equal<uint64>(10UL, entries.[0].SeqNo)
        Assert.Equal<uint64>(5UL, entries.[1].SeqNo))

[<Fact>]
let ``Compaction.compact: active snapshot より古い版だけなら最新版以外を落とす`` () =
    withDir "comp-gc-optimal" (fun dir ->
        let f =
            mkSst dir 10L 1 [ Entry.put (intKey 1) 5UL (intRow 5); Entry.put (intKey 1) 3UL (intRow 3) ]

        let version = Version.addFile f Version.Empty
        // active が無いときの境界は LastCommitSeqNo + 1。
        // 最新版 5 が境界以下なので、3 はどの snapshot からも見えない。
        let result, _ = compactAll dir version 6UL pickFirstLevel
        Assert.Equal(1, result.DroppedVersions)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(1, entries.Length)
        Assert.Equal<uint64>(5UL, entries.[0].SeqNo))

[<Fact>]
let ``Compaction.compact: 閾値を超えないレベルは compaction しない`` () =
    withDir "comp-none" (fun dir ->
        let f = mkSst dir 10L 1 [ entryOf 1 ]
        let version = Version.addFile f Version.Empty
        // 既定の閾値（L1 = 10MiB）には遠く届かない
        let result, info = compactAll dir version 0UL CompactionOptions.Defaults
        Assert.Empty result.Added
        Assert.Empty result.Removed
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(-1L, info.FileNumber)
        Assert.True(System.IO.File.Exists(FileMetaData.path dir 10L)))

[<Fact>]
let ``Compaction.compact: 上のレベルの重なるファイルも入力に含める`` () =
    withDir "comp-inputs" (fun dir ->
        let f1 =
            mkSst dir 10L 1 [ entryOf 1; entryOf 2; Entry.put (intKey 3) 3UL (intRow 3) ]
        // key3 で f1 と重なる L2 のファイル
        let f2 = mkSst dir 11L 2 [ Entry.put (intKey 3) 33UL (intRow 3); entryOf 4 ]
        let version = Version.Empty |> Version.addFile f1 |> Version.addFile f2
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
let ``Compaction.compact: 重ならない上のレベルファイルは入力に含めない`` () =
    withDir "comp-no-overlap" (fun dir ->
        // L1 のキー域は 5..6
        let f1 = mkSst dir 10L 1 [ entryOf 5; entryOf 6 ]
        // L2 には下側（0）と上側（9）の、重ならないファイルを置く
        let below = mkSst dir 11L 2 [ entryOf 0 ]
        let above = mkSst dir 12L 2 [ entryOf 9 ]

        let version =
            Version.Empty
            |> Version.addFile f1
            |> Version.addFile below
            |> Version.addFile above

        let result, _ = compactAll dir version 0UL pickFirstLevel
        // 入力は L1 の f1 だけ。重ならない L2 の 2 ファイルは入力に含めない
        Assert.Equal<int64 list>([ 10L ], result.Removed |> List.map _.FileNumber)
        Assert.True(System.IO.File.Exists(FileMetaData.path dir 11L))
        Assert.True(System.IO.File.Exists(FileMetaData.path dir 12L)))

[<Fact>]
let ``Compaction.compact: 入力の重なり閉包は入力外の古い版も取り込む`` () =
    withDir "comp-overlap-closure" (fun dir ->
        let selected =
            [ 1..32 ] |> List.map (fun i -> mkSst dir (int64 (10 + i)) 5 [ entryOf i ])

        let excluded = mkSst dir 99L 5 [ Entry.put (intKey 100) 3UL (intRow 100) ]

        let bottom =
            mkSst dir 200L 6 [ Entry.delete (intKey 1) 5UL; Entry.delete (intKey 100) 5UL ]

        let version =
            selected @ [ excluded; bottom ]
            |> List.fold (fun v f -> Version.addFile f v) Version.Empty

        let result, _ = compactAll dir version 6UL pickFirstLevel
        let removed = result.Removed |> List.map _.FileNumber |> List.sort
        Assert.Equal<int64 list>([ 11L .. 42L ] @ [ 99L; 200L ] |> List.sort, removed)
        Assert.Equal(2, result.DroppedVersions)
        Assert.Equal(2, result.DroppedTombstones)

        let entries =
            result.Added |> List.collect (fun f -> ok (readBack dir f) |> Array.toList)

        Assert.False(entries |> List.exists (fun e -> Bytes.compare e.Key (intKey 100) = 0), "入力外の古い値が残ると削除済みの行が復活する"))

[<Fact>]
let ``Compaction.compact: 次レベルの広いファイルに合わせて同レベルの seed を広げる`` () =
    withDir "comp-overlap-expand" (fun dir ->
        let left = mkSst dir 10L 1 [ entryOf 1 ]
        let middle = mkSst dir 11L 1 [ entryOf 5; entryOf 6 ]
        let right = mkSst dir 12L 1 [ entryOf 10 ]
        let wide = mkSst dir 20L 2 [ entryOf 1; entryOf 5; entryOf 6; entryOf 10 ]

        let version =
            Version.Empty
            |> Version.addFile left
            |> Version.addFile middle
            |> Version.addFile right
            |> Version.addFile wide

        let options =
            { pickFirstLevel with
                MaxInputFiles = 1
            }

        let result, _ = compactAll dir version 0UL options
        let removed = result.Removed |> List.map _.FileNumber |> List.sort
        Assert.Equal<int64 list>([ 10L; 11L; 12L; 20L ], removed)

        let entries =
            result.Added |> List.collect (fun f -> ok (readBack dir f) |> Array.toList)

        for i in [ 1; 5; 6; 10 ] do
            Assert.True(entries |> List.exists (fun e -> Bytes.compare e.Key (intKey i) = 0), $"key {i} が出力にありません"))

[<Fact>]
let ``Compaction.compact: 最下位レベルへの出力では tombstone を落とす`` () =
    withDir "comp-bottom" (fun dir ->
        let f =
            mkSst dir 10L 5 [ Entry.delete (intKey 1) 5UL; Entry.put (intKey 2) 2UL (intRow 2) ]

        let version = Version.addFile f Version.Empty
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
let ``Compaction.compact: 途中のレベルへの出力では tombstone を残す`` () =
    withDir "comp-keep-tomb" (fun dir ->
        let f =
            mkSst dir 10L 1 [ Entry.delete (intKey 1) 5UL; Entry.put (intKey 2) 2UL (intRow 2) ]

        let version = Version.addFile f Version.Empty
        // 出力は L2。まだ下に古い版が残りうるので tombstone は消せない
        let result, _ = compactAll dir version 0UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(0, result.DroppedTombstones)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(2, entries.Length)
        Assert.True(entries.[0].IsDelete))

[<Fact>]
let ``Compaction.compact: 最下位レベルの compaction で tombstone だけなら出力しない`` () =
    withDir "comp-last-empty" (fun dir ->
        let f =
            mkSst dir 10L Levels.Max [ Entry.delete (intKey 1) 5UL; Entry.delete (intKey 2) 6UL ]

        let version = Version.addFile f Version.Empty
        // L6 だけが閾値超え → 出力先も L6。tombstone は全部落ちて空になる
        let result, info = compactAll dir version 0UL pickFirstLevel
        Assert.Empty result.Added
        Assert.Equal<int64 list>([ 10L ], result.Removed |> List.map _.FileNumber)
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(2, result.DroppedTombstones)
        Assert.Equal(Levels.Max, info.Level)
        // 実ファイルの削除は呼び出し側（MANIFEST 更新後）の責務
        Assert.True(System.IO.File.Exists(FileMetaData.path dir 10L)))

[<Fact>]
let ``Compaction.compact: L0 のファイル数が閾値に達したら L1 へ compaction する`` () =
    withDir "comp-l0" (fun dir ->
        let files =
            [
                for i in 1..3 -> mkSst dir (int64 (10 + i)) 0 [ Entry.put (intKey i) (uint64 i) (intRow i) ]
            ]

        let version = files |> List.fold (fun v f -> Version.addFile f v) Version.Empty
        let before, _ = compactAll dir version 0UL CompactionOptions.Defaults
        Assert.Empty before.Added
        Assert.Equal(3, Version.filesAt 0 version |> Array.length)

        let fourth = mkSst dir 20L 0 [ Entry.put (intKey 4) 4UL (intRow 4) ]
        let version = Version.addFile fourth version
        let result, _ = compactAll dir version 0UL CompactionOptions.Defaults
        Assert.Equal<int64 list>([ 11L; 12L; 13L; 20L ], result.Removed |> List.map _.FileNumber |> List.sort)
        Assert.Equal(1, result.Added.Length)
        Assert.Equal(1, result.Added.[0].Level)
        Assert.Equal(4L, result.Added.[0].EntryCount))

[<Fact>]
let ``Compaction.compact: 古い版が残るなら最下位でも tombstone を消さない`` () =
    withDir "comp-keep-old-tomb" (fun dir ->
        let f =
            mkSst dir 10L Levels.Max [ Entry.delete (intKey 1) 5UL; Entry.put (intKey 1) 3UL (intRow 1) ]

        let version = Version.addFile f Version.Empty
        let result, _ = compactAll dir version 0UL pickFirstLevel
        Assert.Equal(0, result.DroppedVersions)
        Assert.Equal(0, result.DroppedTombstones)

        let entries = ok (readBack dir (List.exactlyOne result.Added))
        Assert.Equal<int>(2, entries.Length)
        Assert.True(entries.[0].IsDelete)
        Assert.Equal<uint64>(3UL, entries.[1].SeqNo))

[<Fact>]
let ``Compaction.compact: 出力はエントリ数ではなく OutputFileTargetBytes ごとに分ける`` () =
    withDir "comp-split-bytes" (fun dir ->
        let text = String.replicate 2000 "x"

        let es =
            [ 1..5 ]
            |> List.map (fun i -> Entry.put (intKey i) (uint64 i) [| SqlValue.Text text |])

        let f = mkSst dir 10L 1 es
        let version = Version.addFile f Version.Empty

        // 1エントリ約 2KB。
        // 目標 5KB なら 2 + 2 + 1 に割れる。
        let options =
            { pickFirstLevel with
                OutputFileTargetBytes = 5000L
            }

        let result, _ = compactAll dir version 0UL options
        let counts = result.Added |> List.sortBy _.FileNumber |> List.map _.EntryCount
        Assert.Equal<int64 list>([ 2L; 2L; 1L ], counts))

[<Fact>]
let ``Compaction.compact: 同じキーの版は出力ファイルを跨がない`` () =
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

        let version = Version.addFile f Version.Empty

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
let ``Compaction.compact: 複数ブロックの入力をストリーミングで読み書きできる`` () =
    withDir "comp-blocks" (fun dir ->
        // 1 ブロック 128 件なので 400 件は 4 ブロック（部分読みのバッチも跨ぐ）。
        let entries =
            [ 0..399 ]
            |> List.map (fun i -> Entry.put (intKey i) (uint64 (i + 1)) (intRow i))

        let f = mkSst dir 10L 1 entries
        let version = Version.addFile f Version.Empty
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
let ``Compaction.compact: 多数の入力ファイルをストリーミングでマージする`` () =
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

        let version = files |> List.fold (fun v f -> Version.addFile f v) Version.Empty
        let result, _ = compactAll dir version 0UL CompactionOptions.Defaults
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

[<Fact>]
let ``Compaction.compact: 出力の書き込みに失敗したら Error を返す`` () =
    withDir "comp-write-fail" (fun dir ->
        let f = mkSst dir 10L 1 [ entryOf 1 ]
        let version = Version.addFile f Version.Empty
        // 存在しないディレクトリを出力先にする → 書き込みが IoError で失敗する
        let missing = System.IO.Path.Combine(dir, "missing")
        let r = Compaction.compact missing version pickFirstLevel 0UL 100L (openBackRaw dir)
        expectErrorKind ErrorKind.IoError "書き込み失敗は IoError になる" r
        // 入力ファイルは消さない（削除は呼び出し側の責務）
        Assert.True(System.IO.File.Exists(FileMetaData.path dir 10L)))

[<Fact>]
let ``Compaction.compact: 入力の読み込みに失敗したらそのエラーを返す`` () =
    withDir "comp-read-fail" (fun dir ->
        let f = fmd 10L 1 1 1
        let version = Version.addFile f Version.Empty
        let broken (_: FileMetaData) = err (DbError.corruption "壊れた SSTable")
        let r = Compaction.compact dir version pickFirstLevel 0UL 100L broken
        expectErrorKind ErrorKind.CorruptionError "読み込みエラーはそのまま伝播する" r)

[<Fact>]
let ``Compaction.compact: L0 が閾値超えでも L1 が大きく超過していれば L1 を選ぶ`` () =
    withDir "comp-level-fairness" (fun dir ->
        let l0 = [ for i in 1..4 -> mkSst dir (int64 i) 0 [ entryOf i ] ]
        let l1 = [ mkSst dir 11L 1 [ entryOf 100 ] ]
        let version = (l0 @ l1) |> List.fold (fun v f -> Version.addFile f v) Version.Empty

        let options =
            { pickFirstLevel with
                L0CompactionTrigger = 4
            }

        let result, _ = compactAll dir version 0UL options
        let removed = result.Removed |> List.map _.FileNumber |> Set.ofList
        Assert.True(Set.contains 11L removed, sprintf "L1 が選ばれませんでした: %A" removed)
        Assert.False(Set.contains 1L removed, sprintf "L0 が選ばれてしまいました: %A" removed))

[<Fact>]
let ``Compaction.compact: Smallest > Largest の入力は CorruptionError になる`` () =
    withDir "comp-broken-range" (fun dir ->
        // MANIFEST の破損で Largest < Smallest のファイルだけの Version。
        // seed 自身が inRange を外れるため expand の入力が空になり、
        // boundsOf が CorruptionError を返す。
        let broken = fmd 1L 0 2 1
        let version = Version.addFile broken Version.Empty
        // L0 はファイル数が閾値以上でしか選ばれないため 1 にする。
        let options =
            { CompactionOptions.Defaults with
                L0CompactionTrigger = 1
            }

        let r = Compaction.compact dir version options 0UL 100L (openBackRaw dir)
        expectErrorKind ErrorKind.CorruptionError "壊れたキー範囲は CorruptionError になる" r)

[<Fact>]
let ``Compaction.compactWith: 停止したら出力を残さず入力の走査を閉じる`` () =
    withDir "comp-stop" (fun dir ->
        let files = [ for i in 1..3 -> mkSst dir (int64 (10 + i)) 0 [ entryOf i ] ]
        let version = files |> List.fold (fun v f -> Version.addFile f v) Version.Empty

        // 2グループ目で 1ファイル出力させ、3回目の判定で停止する。
        let options =
            { pickFirstLevel with
                L0CompactionTrigger = 3
                OutputFileTargetBytes = 1L
            }

        let checks = ref 0

        let shouldStop () =
            checks.Value <- checks.Value + 1
            checks.Value > 2

        let result, _ =
            Compaction.compactWith dir version options 0UL (openBackRaw dir) (fun () -> 900L) shouldStop
            |> ok

        Assert.Equal(3, checks.Value)
        // 停止した回は何も install しない。
        Assert.Empty result.Added
        Assert.Empty result.Removed

        // 停止までに書いた出力は消え、入力だけが残る。
        let remaining =
            System.IO.Directory.GetFiles(dir, "*.sst")
            |> Array.map System.IO.Path.GetFileName
            |> Array.sort

        Assert.Equal<string array>([| "000011.sst"; "000012.sst"; "000013.sst" |], remaining)

        // 入力の走査を閉じていないと共有違反になる
        // （Linux は開いたままでも削除できるため、この検査が無いと Windows だけで露見する）。
        for num in [ 11L; 12L; 13L ] do
            use _ =
                new System.IO.FileStream(
                    FileMetaData.path dir num,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.ReadWrite,
                    System.IO.FileShare.None
                )

            ())

[<Fact>]
let ``Compaction.compactWith: 同一キーのグループ途中でも停止できる`` () =
    withDir "comp-stop-group" (fun dir ->
        // 同じキー 1 の版を別ファイルに置き、gather がグループを畳んでいる途中で停止する。
        let files =
            [
                mkSst dir 21L 0 [ Entry.put (intKey 1) 2UL (intRow 2) ]
                mkSst dir 22L 0 [ Entry.put (intKey 1) 1UL (intRow 1) ]
            ]

        let version = files |> List.fold (fun v f -> Version.addFile f v) Version.Empty

        let options =
            { pickFirstLevel with
                L0CompactionTrigger = 2
            }

        let checks = ref 0

        let shouldStop () =
            checks.Value <- checks.Value + 1
            // pump の先頭と nextGroup の先頭を通過し、gather の先頭で停止する。
            checks.Value > 2

        let result, _ =
            Compaction.compactWith dir version options 0UL (openBackRaw dir) (fun () -> 900L) shouldStop
            |> ok

        Assert.Empty result.Added
        Assert.Empty result.Removed

        let remaining =
            System.IO.Directory.GetFiles(dir, "*.sst")
            |> Array.map System.IO.Path.GetFileName
            |> Array.sort

        Assert.Equal<string array>([| "000021.sst"; "000022.sst" |], remaining)

        for num in [ 21L; 22L ] do
            use _ =
                new System.IO.FileStream(
                    FileMetaData.path dir num,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.ReadWrite,
                    System.IO.FileShare.None
                )

            ())
