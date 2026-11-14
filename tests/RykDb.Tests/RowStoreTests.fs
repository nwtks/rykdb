module RykDb.RowStoreTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テスト用のテーブルスキーマ（id が主キー、email に UNIQUE 索引）。
let private usersSchema =
    TableSchema.ofCreateTable
        {
            Name = "users"
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
                    {
                        Name = "email"
                        DataType = DataType.Text
                        TypeArguments = []
                        Constraint = ColumnConstraint.Empty
                    }
                    {
                        Name = "age"
                        DataType = DataType.Integer
                        TypeArguments = []
                        Constraint = ColumnConstraint.Empty
                    }
                ]
            Constraints = []
        }
    |> ok

let private withIndex (schema: TableSchema) (index: IndexSchema) =
    let indexes =
        schema.Indexes
        |> List.filter (fun i -> i.Name <> index.Name)
        |> fun l -> l @ [ index ]

    { schema with Indexes = indexes }

let private usersWithEmailIndex =
    withIndex
        usersSchema
        {
            Name = "users_email"
            Columns = [ "email" ]
            Unique = true
        }

let private rowOf (id: int) (email: string) (age: int) =
    [| SqlValue.Int(int64 id); SqlValue.Text email; SqlValue.Int(int64 age) |]

/// transaction のバッファを DB に流し込む。
let private commitInto (store: RowStore) (nextSeq: uint64) (txn: Transaction) =
    let batches = Transaction.toEntries nextSeq txn |> fst
    RowStore.writeBatches batches store |> ok

/// Close の呼び出しを記録するだけの engine（RowStore.close のテスト用）。
type private CloseTrackingEngine(name: string, fail: bool, closed: ResizeArray<string>) =
    interface IStorageEngine with
        member _.NextSeqNo = 0UL
        member _.ActiveSnapshot = 0UL

        member _.HostState =
            {
                Version = Version.Empty
                ManifestNumber = 0L<manifestNo>
                ActiveSnapshot = 0UL
                LastCompactionError = None
                Generation = 0L
            }

        member _.WalState = None
        member _.GetAt _ _ = ret None
        member _.ScanAt _ _ _ = ret SstScan.Empty
        member this.WriteBatch _ = ret (this :> IStorageEngine)
        member this.WriteBatchDeferred _ = ret (this :> IStorageEngine)
        member this.WithNextSeqNo _ = this :> IStorageEngine
        member this.WithActiveSnapshot _ = this :> IStorageEngine
        member this.WithHostState _ = this :> IStorageEngine
        member this.FlushIfNeeded _ = ret (this :> IStorageEngine, false)
        member this.MaybeFlush() = ret (this :> IStorageEngine)

        member _.Close() =
            closed.Add name

            if fail then
                err (DbError.internalError "close failed")
            else
                ret ()

[<Fact>]
let ``Keyspace: table / index の名前`` () =
    Assert.Equal("t:people", Keyspace.table "people")
    Assert.Equal("i:ix_people_age", Keyspace.index "ix_people_age")

[<Fact>]
let ``Keyspace: prefixedKey は接頭辞を外すと元のキーに戻る`` () =
    let keyspaces =
        [
            "t:a"
            "t:ab"
            "i:x"
            Keyspace.CatalogKeyspace
            // NUL を含む keyspace（エスケープしないのでバイト列にそのまま入る）
            "t:" + string (char 0) + "q"
            // 長さフィールドが varint 2 バイトになる（1 バイト前提の検証を防ぐ）
            "t:" + System.String('x', 130)
        ]

    let keys =
        [
            [||]
            [| 0uy |]
            [| 0uy; 0uy |]
            [| 255uy |]
            [| 1uy; 2uy; 3uy |]
            [| 0uy; 255uy; 0uy |]
        ]

    for keyspace in keyspaces do
        let prefix = Keyspace.prefix keyspace

        for key in keys do
            let physical = Keyspace.prefixedKey keyspace key
            Assert.True(KeyBuilder.startsWithPrefix physical prefix, sprintf "%s の物理キーに自分の接頭辞が付いていない" keyspace)
            Assert.Equal<byte array>(key, Array.sub physical prefix.Length (physical.Length - prefix.Length))

    // 別 keyspace の物理キーは自分の接頭辞だけが一致する（キーが混ざらないことの根拠）。
    for a in keyspaces do
        for b in keyspaces do
            if a <> b then
                let physical = Keyspace.prefixedKey b (intKey 1)

                Assert.False(
                    KeyBuilder.startsWithPrefix physical (Keyspace.prefix a),
                    sprintf "%s の物理キーが %s の範囲に入り込む" b a
                )

[<Fact>]
let ``RowStore: compaction 失敗後も flush 結果を保持し、再起動で commit を失わない`` () =
    withDir "rowstore-compaction-fail" (fun dir ->
        let engine = ok (StorageEngine.openDatabaseWith None dir)

        let mk (num: int64) (key: int) =
            [| Entry.put (intKey key) (uint64 key) (intRow key) |]
            |> Array.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
            |> fun entries -> FileMetaData.writeSstable dir num 0 0.01 entries
            |> ok

        let f1 = mk 10L 1
        let f2 = mk 11L 2
        let f3 = mk 12L 3

        // f2 を開けるが壊れたファイルにする（データブロックのチェックサム不一致）
        let path = FileMetaData.path dir 11L
        let data = System.IO.File.ReadAllBytes path
        data.[0] <- data.[0] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(path, data)

        let version =
            Version.Empty |> Version.addFile f1 |> Version.addFile f2 |> Version.addFile f3

        let engine =
            { engine with
                Version = { version with NextFileNumber = 100L }
                MemLimitBytes = 1
            }

        let store = openStoreWith dir (DiskStorageEngine engine :> IStorageEngine)

        // 1 回目の write で flush + compaction が走り、compaction は f2 で失敗する。
        let store =
            ok (RowStore.writeBatches [ ("t", [| Entry.put (intKey 5) 5UL (intRow 5) |]) ] store)

        // 2 回目の write は flush 済みの新しい WAL に載る。
        ok (RowStore.writeBatches [ ("t", [| Entry.put (intKey 6) 6UL (intRow 6) |]) ] store)
        |> ignore

        // クラッシュ相当: 同じディレクトリを開き直す。
        let reopened = ok (StorageEngine.openDatabaseWith None dir)
        let key5 = Keyspace.prefixedKey "t" (intKey 5)
        let key6 = Keyspace.prefixedKey "t" (intKey 6)
        Assert.True((ok (StorageEngine.getAt None key5 reopened)).IsSome)
        Assert.True((ok (StorageEngine.getAt None key6 reopened)).IsSome))

[<Fact>]
let ``RowStore: keyspace は 1 本の LSM の接頭辞で分離される`` () =
    withDir "rs7" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let row = rowOf 1 "a@x.com" 1

        let txn =
            Dml.insertWithKey (RowKey.primary usersWithEmailIndex row) txn usersWithEmailIndex row
            |> ok

        let store = commitInto store 1UL txn
        // keyspace ごとのディレクトリは作られない。
        Assert.Empty(System.IO.Directory.GetDirectories dir)

        let tableRows =
            ok (RowStore.scan (Keyspace.table "users") [||] 0 store)
            |> SstScan.toArray
            |> ok

        let indexRows =
            ok (RowStore.scan (Keyspace.index "users_email") [||] 0 store)
            |> SstScan.toArray
            |> ok

        Assert.Single tableRows |> ignore
        Assert.Single indexRows |> ignore
        Assert.Equal<byte array>(intKey 1, tableRows.[0].Key)

        ok (RowStore.close store))

[<Fact>]
let ``RowStore: format マーカーがプロセス間ロックを兼ねる`` () =
    withDir "rs-format-lock" (fun dir ->
        let formatPath = System.IO.Path.Combine(dir, ".rykdb.format")

        let canLockExternally () =
            try
                use fs =
                    new System.IO.FileStream(
                        formatPath,
                        System.IO.FileMode.Open,
                        System.IO.FileAccess.ReadWrite,
                        System.IO.FileShare.None
                    )

                true
            with :? System.IO.IOException ->
                false

        let store = ok (RowStore.openStore dir)
        Assert.True(System.IO.File.Exists formatPath)
        Assert.False(System.IO.File.Exists(System.IO.Path.Combine(dir, ".rykdb.lock")))
        Assert.False(canLockExternally ())

        ok (RowStore.close store)
        Assert.True(canLockExternally ())
        Assert.Equal("rykdb-prefixed-v1", System.IO.File.ReadAllText(formatPath).Trim()))

[<Fact>]
let ``RowStore.openStore: ロック失敗時は format マーカーを書き換えない`` () =
    withDir "rs-format-lock-fail" (fun dir ->
        let formatPath = System.IO.Path.Combine(dir, ".rykdb.format")
        System.IO.File.WriteAllText(formatPath, "keep-me\n")

        do
            use _held =
                new System.IO.FileStream(
                    formatPath,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.ReadWrite,
                    System.IO.FileShare.None
                )

            expectErrorKind ErrorKind.IoError "ロック中の DB を開けてしまった" (RowStore.openStore dir)

        Assert.Equal("keep-me", System.IO.File.ReadAllText(formatPath).Trim()))

[<Fact>]
let ``RowStore: 前方一致する keyspace 名でもキーが混ざらない`` () =
    withStore "rs-prefix-boundary" (fun store ->
        let spaceA = Keyspace.table "a"
        let spaceB = Keyspace.table "ab"

        let store =
            ok (
                RowStore.writeBatches
                    [
                        spaceA, [| Entry.put (intKey 1) 1UL (intRow 1) |]
                        spaceB, [| Entry.put (intKey 1) 2UL (intRow 2) |]
                    ]
                    store
            )

        let rowsA = ok (RowStore.scan spaceA [||] 0 store) |> SstScan.toArray |> ok
        let rowsB = ok (RowStore.scan spaceB [||] 0 store) |> SstScan.toArray |> ok
        Assert.Equal(1, rowsA.Length)
        Assert.Equal(1, rowsB.Length)
        Assert.Equal(Some(intRow 1), rowsA.[0].Value)
        Assert.Equal(Some(intRow 2), rowsB.[0].Value))

[<Fact>]
let ``RowStore: 複数 keyspace の書き込みは 1 回の WAL バッチで回復する`` () =
    withDir "rs-atomic-wal" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let b1 = "t:a", [| Entry.put (intKey 1) 1UL (intRow 1) |]
        let b2 = "t:b", [| Entry.put (intKey 2) 2UL (intRow 2) |]
        ok (RowStore.writeBatches [ b1; b2 ] store) |> ignore

        // close せずに開き直す（WAL だけが残ったクラッシュ状態）。
        let reopened = ok (RowStore.openStoreTakingOver dir)
        Assert.Equal(Some(intRow 1), ok (RowStore.getAt "t:a" (intKey 1) None reopened))
        Assert.Equal(Some(intRow 2), ok (RowStore.getAt "t:b" (intKey 2) None reopened))

        ok (RowStore.close reopened)
        ok (RowStore.close store))

[<Fact>]
let ``RowStore.close: engine の失敗を返して Close を呼ぶ`` () =
    let closed = ResizeArray<string>()

    let store =
        {
            Root = ""
            Engine = CloseTrackingEngine("a", true, closed) :> IStorageEngine
            Lock = None
            Host = None
        }

    let outcome = RowStore.close store
    expectError "close は失敗を返す" outcome
    Assert.Contains("a", closed)

[<Fact>]
let ``RowStore.commitToStore: 別ハンドルの active snapshot を GC 境界に使う`` () =
    withDir "commit-boundary" (fun dir ->
        let store = ok (RowStore.openStore dir)

        // 既に SeqNo 10 まで commit 済みのハンドル（SeqNo 11 から採番する）。
        let mgr =
            { TransactionManager.create () with
                LastCommitSeqNo = 10UL
                NextSeqNo = 11UL
            }

        let mgr, txn = TransactionManager.beginTxn mgr
        let txn = Transaction.put "t:a" (intKey 1) (intRow 1) txn

        // 同じ DB を開いた別ハンドルが snapshot 5 で読み始めている状態。
        let other = ActiveSnapshotRegistry.register dir 5UL
        let committed, _ = ok (RowStore.commitToStoreWith false dir None mgr txn store)
        let engine = ok (RowStore.ensure committed)
        // 自ハンドルだけなら 12（LastCommitSeqNo 11 + 1）だが、別ハンドルの 5 が下限になる。
        Assert.Equal(5UL, engine.ActiveSnapshot)

        ActiveSnapshotRegistry.unregister dir other
        RowStore.close committed |> ok)

[<Fact>]
let ``RowStore.commitToStore: 自分の登録は GC 境界から除外する`` () =
    withDir "commit-boundary-self" (fun dir ->
        let store = ok (RowStore.openStore dir)

        let mgr =
            { TransactionManager.create () with
                LastCommitSeqNo = 10UL
                NextSeqNo = 11UL
            }

        let mgr, txn = TransactionManager.beginTxn mgr
        let txn = Transaction.put "t:a" (intKey 1) (intRow 1) txn

        // 自分（これから commit する transaction）の登録は除外し、他の登録だけを見る。
        let own = ActiveSnapshotRegistry.register dir txn.Snapshot.SeqNo

        let committed, _ =
            ok (RowStore.commitToStoreWith false dir (Some own) mgr txn store)

        let engine = ok (RowStore.ensure committed)
        Assert.Equal(12UL, engine.ActiveSnapshot)

        ActiveSnapshotRegistry.unregister dir own
        RowStore.close committed |> ok)

[<Fact>]
let ``RowStore.clearKeyspaceChunks: チャンクを跨いでも全件を消す`` () =
    withStore "clear-chunks" (fun store ->
        let space = Keyspace.table "t"

        // ClearChunkEntries = 8192。2 チャンク目まで進む件数で、
        // lastKey からの再走査が無限ループしないことを確認する。
        let entries =
            [| for i in 0..8200 -> Entry.put (intKey i) (uint64 (i + 1)) (intRow i) |]

        let store = ok (RowStore.writeBatches [ (space, entries) ] store)

        // 既存エントリより大きい SeqNo から tombstone を採番する（実運用の再オープン相当）。
        let mgr =
            { TransactionManager.create () with
                NextSeqNo = 8202UL
                LastCommitSeqNo = 8201UL
            }

        let mgr, _ = TransactionManager.beginTxn mgr
        let store, _mgr = ok (RowStore.clearKeyspaceChunks mgr space store)
        let rows = ok (RowStore.scan space [||] 0 store) |> SstScan.toArray |> ok
        Assert.Empty rows)

[<Fact>]
let ``RowStore.scanAtRaw: scope 指定時はラッパー越しでも走査を台帳に登録する`` () =
    withStore "scan-scope" (fun store ->
        let space = Keyspace.table "t"

        let entries =
            [| for i in 0..2 -> Entry.put (intKey i) (uint64 (i + 1)) (intRow i) |]

        let store = ok (RowStore.writeBatches [ (space, entries) ] store)
        let scope = ScanScope()
        // scanWithinPrefix は SstScan(fun ...) で包むため Dispose を伝播しない。
        // 登録が発生源（scanAtRaw）で行われていることを ActiveCount で確認する。
        let scan = ok (RowStore.scanAtRaw space [||] 0 None (Some scope) store)
        Assert.Equal(1, scope.ActiveCount)
        Assert.True((ok (SstScan.next scan)).IsSome)
        Assert.Equal(1, scope.ActiveCount)

        scope.DisposeAll()
        Assert.Equal(0, scope.ActiveCount))
