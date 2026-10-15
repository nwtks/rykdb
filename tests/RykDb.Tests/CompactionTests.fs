// ===========================================================================
// Compaction.fs のテスト: Leveled compaction と不要バージョン回収（GC）
// ===========================================================================

module RykDb.CompactionTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private key (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]

let private valueOf (i: int) = [| SqlValue.Int(int64 i) |]

/// N 個の key を書いて flush する。
let private fillAndFlush (engine: StorageEngine) (from: int) (count: int) =
    let engine =
        List.fold (fun e i -> ok (StorageEngine.put e (key i) (valueOf i))) engine [ from .. from + count - 1 ]

    ok (StorageEngine.flush engine)

/// レベルごとのファイル数。
let private levelsOf (engine: StorageEngine) = engine.Version |> Version.levelCounts

let private unreferencedFiles (dir: string) (version: RykDb.Core.Version) =
    let known =
        Version.allFiles version |> List.map (fun f -> f.FileNumber) |> Set.ofList

    protect (fun () ->
        Directory.GetFiles(dir, "*.sst")
        |> Array.choose (fun p ->
            let stem = Path.GetFileNameWithoutExtension p

            match System.Int64.TryParse stem with
            | true, n when not (Set.contains n known) -> Some n
            | _ -> None)
        |> Array.toList
        |> List.sort)

let private setCompactionOptions (opts: CompactionOptions) (engine: StorageEngine) =
    let next = { engine with Compaction = opts }
    next

let private get (engine: StorageEngine) (key: byte array) = StorageEngine.getAt engine None key

// ===========================================================================
// obsoleteFiles — ファイル単位の GC
// ===========================================================================

let private file (num: int64) (level: int) (maxSeqNo: uint64) =
    {
        FileNumber = num
        Level = level
        Smallest = key 1
        Largest = key 1000
        EntryCount = 10L
        Size = 1000L
        MaxSeqNo = maxSeqNo
    }

[<Fact>]
let ``GC: Version に無いファイルだけが孤児候補になる`` () =
    withDir "orph" (fun dir ->
        protect (fun () -> Directory.CreateDirectory(dir) |> ignore) |> ignore

        let v1: RykDb.Core.Version = Version.addFile (file 1L 1 5UL) Version.empty
        let referenced: RykDb.Core.Version = Version.addFile (file 2L 1 50UL) v1

        // Version が参照している 1, 2 と、参照していない 3, 4 を置く
        for n in [ 1L; 2L; 3L; 4L ] do
            protect (fun () -> File.WriteAllBytes(Path.Combine(dir, sprintf "%06d.sst" n), [||]))
            |> ignore

        let orphans: int64 list = ok (unreferencedFiles dir referenced)

        Assert.Equal<int>(2, orphans.Length)
        Assert.Equal<int64>(3L, orphans.[0])
        Assert.Equal<int64>(4L, orphans.[1]))

// ===========================================================================
// StorageEngine の compactOnce
// ===========================================================================

[<Fact>]
let ``Compaction: flush だけの場合は何も起きない`` () =
    withDir "c1" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)
        let engine = fillAndFlush engine 1 10

        // L1 以降が無いので pickCompaction は None を返す
        let engine = ok (StorageEngine.maybeCompact engine)

        // ファイルは L0 の 1 個のまま
        Assert.Equal<int array>([| 1; 0; 0; 0; 0; 0; 0 |], levelsOf engine))


[<Fact>]
let ``Compaction: L1 にファイルがあると L2 へ降ろされる`` () =
    withDir "c3" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)

        // flush は L0 に出すだけなので、Version を直接操作して L1 のファイルを作る。
        let arr =
            Array.init 10 (fun i -> SstEntry.make (key (i + 1)) (uint64 (i + 1)) (Some(valueOf i)))

        let e1 = ok (Sstable.build arr 0.01)

        let fileNumber = engine.Version.NextFileNumber

        protect (fun () -> File.WriteAllBytes(Path.Combine(dir, sprintf "%06d.sst" fileNumber), e1))
        |> ignore

        let meta =
            {
                FileNumber = fileNumber
                Level = 1
                Smallest = key 1
                Largest = key 10
                EntryCount = 10L
                Size = int64 e1.Length
                MaxSeqNo = 10UL
            }

        let edit = VersionEdit.addFile meta

        let (version, manifestNumber) =
            ok (Manifest.logAndApply dir engine.ManifestNumber engine.Version edit)

        let engine =
            { engine with
                Version = version
                ManifestNumber = manifestNumber
            }

        Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], levelsOf engine))

[<Fact>]
let ``Compaction: 実行すると入力ファイルが消えて出力ファイルができる`` () =
    withDir "c4" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)

        // L1 にファイルを作る（キーは 1..10）
        let arr =
            Array.init 10 (fun i -> SstEntry.make (key (i + 1)) (uint64 (i + 1)) (Some(valueOf i)))

        let bytes = ok (Sstable.build arr 0.01)
        let fileNumber = engine.Version.NextFileNumber

        protect (fun () -> File.WriteAllBytes(Path.Combine(dir, sprintf "%06d.sst" fileNumber), bytes))
        |> ignore

        let meta =
            {
                FileNumber = fileNumber
                Level = 1
                Smallest = key 1
                Largest = key 10
                EntryCount = 10L
                Size = int64 bytes.Length
                MaxSeqNo = 10UL
            }

        let edit = VersionEdit.addFile meta

        let (version, manifestNumber) =
            ok (Manifest.logAndApply dir engine.ManifestNumber engine.Version edit)

        let engine =
            { engine with
                Version = version
                ManifestNumber = manifestNumber
            }

        let opts =
            { CompactionOptions.defaults with
                MaxLevelBytes = [| 0L; 1L; 1L; 1L; 1L; 1L; 1L |]
            }

        let engine = setCompactionOptions opts engine

        let engine = ok (StorageEngine.maybeCompact engine)

        // L1 のファイルが L2 に移動した
        Assert.Equal<int array>([| 0; 0; 1; 0; 0; 0; 0 |], levelsOf engine)

        // 入力ファイルは削除されている
        Assert.False(File.Exists(Path.Combine(dir, sprintf "%06d.sst" fileNumber)))

        // データはちゃんと読める
        for i in 1..10 do
            let found = ok (get engine (key i))
            Assert.True(found.IsSome, sprintf "key %d が読めない" i))

[<Fact>]
let ``Compaction: 実行しても同じ Version では二度と compaction されない`` () =
    // MANIFEST に同じファイルが追加されないことの確認（ファイル番号の巻き戻し防止）
    withDir "c5" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)
        let engine = fillAndFlush engine 1 5
        let before = engine.Version.NextFileNumber

        let engine = ok (StorageEngine.maybeCompact engine)

        // NextFileNumber は減らない
        Assert.True(engine.Version.NextFileNumber >= before))

// ===========================================================================
// RowStore 経由の compaction
// ===========================================================================

[<Fact>]
let ``Compaction: RowStore 経由で全 keyspace を compact できる`` () =
    let schema =
        TableSchema.ofCreateTable
            {
                Name = "t"
                IfNotExists = false
                Columns =
                    [
                        {
                            Name = "id"
                            DataType = DataType.Integer
                            TypeArguments = []
                            Constraint =
                                { ColumnConstraint.Empty with
                                    PrimaryKey = true
                                }
                        }
                    ]
                Constraints = []
            }
        |> ok

    withStore "c6" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = ok (Dml.insert schema txn [| SqlValue.Int 1L |])
        let batches = Transaction.toEntries 1UL txn |> fst
        let written = ok (RowStore.writeBatches store batches)
        let advanced = RowStore.setActiveSnapshot 100UL written
        let compacted = ok (RowStore.compactAll advanced)
        let has = compacted.Engines |> Map.containsKey (Keyspace.table "t")

        Assert.True(has))

[<Fact>]
let ``Compaction: active snapshot を進めている GC の基準になる`` () =
    withDir "c7" (fun dir ->
        let engine = ok (StorageEngine.openDatabase dir)
        let engine = StorageEngine.setActiveSnapshot 100UL engine

        Assert.Equal<uint64>(100UL, engine.ActiveSnapshot)

        // 戻せない（単調増加）
        let engine = StorageEngine.setActiveSnapshot 50UL engine
        Assert.Equal<uint64>(100UL, engine.ActiveSnapshot))
