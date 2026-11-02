module RykDb.RowStoreTests

open System.IO
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
    RowStore.writeBatches batches store |> ok |> ignore

/// Close の呼び出しを記録するだけの engine（RowStore.close のテスト用）。
type private CloseTrackingEngine(name: string, fail: bool, closed: ResizeArray<string>) =
    interface IStorageEngine with
        member _.NextSeqNo = 0UL
        member _.ActiveSnapshot = 0UL
        member _.GetAt _ _ = ret None
        member _.ScanAt _ _ _ = ret SstScan.Empty
        member this.WriteBatch _ = ret (this :> IStorageEngine)
        member this.WithNextSeqNo _ = this :> IStorageEngine
        member this.WithActiveSnapshot _ = this :> IStorageEngine
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
let ``Keyspace: keyspace はテーブルと索引で別になる`` () =
    Assert.Equal<string>("t:users", Keyspace.table "users")
    Assert.Equal<string>("i:users_email", Keyspace.index "users_email")
    Assert.NotEqual<string>(Keyspace.table "users", Keyspace.index "users_email")

    // ディレクトリ名に使える形（base32 なので / や : を含む名前も安全）
    Assert.NotEqual<string>(Keyspace.toDirName "t:a/b", Keyspace.toDirName "t:a_b")

[<Fact>]
let ``Keyspace: base32 はパス区切りと大文字小文字衝突を作らない`` () =
    let unicode = Keyspace.table "住所"
    let dir = Keyspace.toDirName unicode
    Assert.False(dir.Contains "/")
    Assert.False(dir.Contains "\\")
    Assert.Equal(Some unicode, Keyspace.tryFromDirName dir)

    // base64 では t:a -> dDph / t:G -> dDpH となり、
    // Windows/macOS の case-insensitive FS で衝突していた。
    let a = Keyspace.toDirName (Keyspace.table "a")
    let g = Keyspace.toDirName (Keyspace.table "G")
    Assert.NotEqual<string>(a, g)
    Assert.False(System.String.Equals(a, g, System.StringComparison.OrdinalIgnoreCase))

[<Fact>]
let ``Keyspace: toDirName / tryFromDirName の往復`` () =
    let ks = "t:people"
    let dir = Keyspace.toDirName ks
    Assert.Equal(Some ks, Keyspace.tryFromDirName dir)

[<Fact>]
let ``RowStore.openStore: 新規 DB でも catalog エンジンを登録する`` () =
    withDir "rs-catalog-engine" (fun dir ->
        let store = ok (RowStore.openStore dir)
        Assert.True(Map.containsKey Keyspace.CatalogKeyspace store.Engines)
        ok (RowStore.close store))

[<Fact>]
let ``RowStore: keyspace ごとに独立したディレクトリを持つ`` () =
    withDir "rs7" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = Dml.insert txn usersWithEmailIndex (rowOf 1 "a@x.com" 1) |> ok
        commitInto store 1UL txn

        // 主キーのツリーと索引のツリーで別のディレクトリができている
        let dirs = Directory.GetDirectories dir
        Assert.True(dirs.Length >= 2)

        let hasWal =
            dirs
            |> Array.exists (fun d -> Directory.GetFiles(d, "*" + Wal.WalExtension).Length > 0)

        Assert.True hasWal)

[<Fact>]
let ``RowStore.openStore: 壊れた keyspace は開けない`` () =
    withDir "rs-corrupt" (fun dir ->
        let ksDir = Path.Combine(dir, Keyspace.toDirName "t:broken")
        Directory.CreateDirectory ksDir |> ignore
        File.WriteAllText(Path.Combine(ksDir, "CURRENT"), "壊れた CURRENT")
        expectError "壊れた keyspace を開けてしまう" (RowStore.openStore dir))

[<Fact>]
let ``RowStore.openStore: ロック失敗時は commit journal を触らない`` () =
    withDir "rs-journal-lock-order" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let journal = Path.Combine(dir, ".commit", "journal")

        // 4バイトの長さが MaxFrameBytes を超えるフレーム + 余分なバイト。
        // touch がロック前に走ると、readFrames の validLen（4）まで切り詰める。
        let bytes = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 1uy; 2uy; 3uy |]
        File.WriteAllBytes(journal, bytes)
        expectErrorKind ErrorKind.IoError "同じ root の二重オープン" (RowStore.openStore dir)
        Assert.Equal<byte array>(bytes, File.ReadAllBytes journal)

        ok (RowStore.close store))

[<Fact>]
let ``RowStore.openStore: 開く途中で失敗してもロックを解放する`` () =
    withDir "rs-lock-cleanup" (fun dir ->
        let ksDir = Path.Combine(dir, Keyspace.toDirName "t:broken")
        Directory.CreateDirectory ksDir |> ignore
        File.WriteAllText(Path.Combine(ksDir, "CURRENT"), "壊れた CURRENT")
        expectError "壊れた keyspace を開けてしまう" (RowStore.openStore dir)

        // 失敗した open がプロセス内ロックを残していると、ここで IoError になる。
        Directory.Delete(ksDir, true)
        let store = ok (RowStore.openStore dir)
        ok (RowStore.close store))

[<Fact>]
let ``RowStore.close: 途中の engine が失敗しても残りを閉じる`` () =
    let closed = ResizeArray<string>()

    let store =
        {
            Root = ""
            Engines =
                Map.ofList
                    [
                        "t:a", CloseTrackingEngine("a", true, closed) :> IStorageEngine
                        "t:b", CloseTrackingEngine("b", false, closed) :> IStorageEngine
                    ]
            OpenEngine = fun _ -> err (DbError.internalError "unused")
            Lock = None
        }

    let outcome = RowStore.close store
    expectError "close は失敗を返す" outcome
    // 最初の engine が失敗しても、後続の engine も Close される。
    Assert.Contains("a", closed)
    Assert.Contains("b", closed)

[<Fact>]
let ``RowStore.openStore: 壊れた MANIFEST レコードは読み飛ばす`` () =
    withDir "rs-manifest" (fun dir ->
        // ルートの keyspace 開設時（openStore では subdirectory のみ走査）に読ませる
        let ksDir = Path.Combine(dir, Keyspace.toDirName "t:manifest")
        Directory.CreateDirectory ksDir |> ignore
        File.WriteAllText(Path.Combine(ksDir, "CURRENT"), "MANIFEST-000001")
        // base64 としては正しいが、中身が VersionEdit として壊れている
        File.WriteAllText(Path.Combine(ksDir, "MANIFEST-000001"), "AA==")
        // レコード単位で捨てて先へ進む（開けるが、その前後も読める）
        ok (RowStore.openStore dir) |> ignore)

/// ジャーナルの未解決件数。

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

        let committed, _ = ok (RowStore.commitToStore dir None mgr txn store)
        let engine = ok (RowStore.ensure "t:a" committed)

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

        let committed, _ = ok (RowStore.commitToStore dir (Some own) mgr txn store)
        let engine = ok (RowStore.ensure "t:a" committed)

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

        let store = ok (RowStore.writeBatches [ space, entries ] store)

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
