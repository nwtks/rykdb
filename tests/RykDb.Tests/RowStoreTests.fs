// ===========================================================================
// RowStore.fs のテスト: keyspace シャーディング・行の読み書き・制約・索引
// ===========================================================================

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
    RowStore.writeBatches store batches |> ok |> ignore

// ===========================================================================
// RowStore
// ===========================================================================

[<Fact>]
let ``RowStore: スキーマを AST から作れる`` () =
    Assert.Equal("users", usersSchema.Name)
    Assert.Equal<int list>([ 0 ], usersSchema.PrimaryKey)
    Assert.Equal<string array>([| "id" |], usersSchema.PrimaryKeyColumns)
    Assert.Equal(1, usersSchema.IndexOf "email")
    Assert.Equal(-1, usersSchema.IndexOf "nope")

[<Fact>]
let ``RowStore: 主キー無し・列無しのテーブルはエラー`` () =
    let noCols =
        {
            Name = "t"
            IfNotExists = false
            Columns = []
            Constraints = []
        }

    match TableSchema.ofCreateTable noCols with
    | Error e -> Assert.Equal<ErrorKind>(ErrorKind.ConstraintViolation, e.Kind)
    | Ok _ -> Assert.Fail("列無しのテーブルを許してはいけない")

    let noPk =
        {
            Name = "t"
            IfNotExists = false
            Columns =
                [
                    {
                        Name = "a"
                        DataType = DataType.Integer
                        TypeArguments = []
                        Constraint = ColumnConstraint.Empty
                    }
                ]
            Constraints = []
        }

    match TableSchema.ofCreateTable noPk with
    | Error e -> Assert.Equal<ErrorKind>(ErrorKind.ConstraintViolation, e.Kind)
    | Ok _ -> Assert.Fail("主キー無しのテーブルを許してはいけない")

[<Fact>]
let ``RowStore: keyspace はテーブルと索引で別になる`` () =
    Assert.Equal<string>("t:users", Keyspace.table "users")
    Assert.Equal<string>("i:users_email", Keyspace.index "users_email")
    Assert.NotEqual<string>(Keyspace.table "users", Keyspace.index "users_email")

    // ディレクトリ名に使える形（base64 なので / や : を含む名前も安全）
    Assert.NotEqual<string>(Keyspace.toDirName "t:a/b", Keyspace.toDirName "t:a_b")

[<Fact>]
let ``RowStore: 1 行挿入して主キーで引ける`` () =
    withDir "rs1" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = Dml.insert usersWithEmailIndex txn (rowOf 1 "a@example.com" 20) |> ok

        commitInto store 1UL txn

        let found =
            Dml.getByPrimaryKey store usersWithEmailIndex None [| SqlValue.Int 1L |] |> ok

        match found with
        | Some row -> Assert.Equal<string>("a@example.com", asText row.[1])
        | None -> Assert.Fail("行が見つからないはず"))

[<Fact>]
let ``RowStore: 索引で引ける`` () =
    withDir "rs2" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = Dml.insert usersWithEmailIndex txn (rowOf 7 "bob@example.com" 30) |> ok

        commitInto store 1UL txn

        let idx = usersWithEmailIndex.Indexes.[0]

        let found =
            Dml.getByIndex store usersWithEmailIndex idx None (rowOf 7 "bob@example.com" 30)
            |> ok

        match found with
        | Some row -> Assert.Equal<int64>(7L, asInt row.[0])
        | None -> Assert.Fail("索引で引けないはず")

        // 別の索引値は引けない
        let missing =
            Dml.getByIndex store usersWithEmailIndex idx None (rowOf 9 "nobody@example.com" 9)
            |> ok

        Assert.True(missing.IsNone))

[<Fact>]
let ``RowStore: NOT NULL 制約違反は弾かれる`` () =
    let notNull =
        { ColumnConstraint.Empty with
            NotNull = true
        }

    let emailCol = usersWithEmailIndex.Columns.[1]

    let schema =
        { usersWithEmailIndex with
            Columns =
                [
                    usersWithEmailIndex.Columns.[0]
                    { emailCol with Constraint = notNull }
                    usersWithEmailIndex.Columns.[2]
                ]
        }

    withDir "rs3" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        // email が NULL
        let bad = [| SqlValue.Int 1L; SqlValue.Null; SqlValue.Int 20L |]

        match Dml.insert schema txn bad with
        | Error e -> Assert.Equal<ErrorKind>(ErrorKind.ConstraintViolation, e.Kind)
        | Ok _ -> Assert.Fail("NOT NULL 制約違反を検出できなかった"))

[<Fact>]
let ``RowStore: 列数が違う行は弾かれる`` () =
    withDir "rs3b" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        match Dml.insert usersWithEmailIndex txn [| SqlValue.Int 1L |] with
        | Error e -> Assert.Equal<ErrorKind>(ErrorKind.ConstraintViolation, e.Kind)
        | Ok _ -> Assert.Fail("列数不一致を検出できなかった"))

[<Fact>]
let ``RowStore: 削除は主キーと索引の両方に tombstone を書く`` () =
    withDir "rs4" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = Dml.insert usersWithEmailIndex txn (rowOf 1 "x@example.com" 1) |> ok

        commitInto store 1UL txn

        let idx = usersWithEmailIndex.Indexes.[0]
        let _, delTxn = TransactionManager.beginTxn mgr

        let delTxn =
            Dml.deleteByPrimaryKey store usersWithEmailIndex None delTxn [| SqlValue.Int 1L |]
            |> ok

        commitInto store 10UL delTxn

        // 主キーからも索引からも消えている（索引の tombstone を忘れると行が復活する）
        let found =
            Dml.getByPrimaryKey store usersWithEmailIndex None [| SqlValue.Int 1L |] |> ok

        let byEmail =
            Dml.getByIndex store usersWithEmailIndex idx None (rowOf 1 "x@example.com" 1)
            |> ok

        Assert.True(found.IsNone)
        Assert.True(byEmail.IsNone))

[<Fact>]
let ``RowStore: 存在しない行の削除は NotFound`` () =
    withDir "rs4b" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, delTxn = TransactionManager.beginTxn mgr

        match Dml.deleteByPrimaryKey store usersWithEmailIndex None delTxn [| SqlValue.Int 999L |] with
        | Error e -> Assert.Equal<ErrorKind>(ErrorKind.NotFound, e.Kind)
        | Ok _ -> Assert.Fail("存在しない行の削除を許してはいけない"))

[<Fact>]
let ``RowStore: update は削除+挿入を 1 transaction で行う`` () =
    withDir "rs5" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = Dml.insert usersWithEmailIndex txn (rowOf 1 "old@example.com" 1) |> ok

        commitInto store 1UL txn

        let _, updTxn = TransactionManager.beginTxn mgr
        let newRow = rowOf 1 "new@example.com" 2

        let updTxn =
            Dml.updateByPrimaryKey store usersWithEmailIndex None updTxn [| SqlValue.Int 1L |] newRow
            |> ok

        commitInto store 10UL updTxn

        // 主キーで読むと新しい email が見える
        let found =
            Dml.getByPrimaryKey store usersWithEmailIndex None [| SqlValue.Int 1L |] |> ok

        match found with
        | Some row -> Assert.Equal<string>("new@example.com", asText row.[1])
        | None -> Assert.Fail("見つからないはず")

        // 古い索引値は消え、新しい索引値で引ける
        let idx = usersWithEmailIndex.Indexes.[0]

        let byOld =
            Dml.getByIndex store usersWithEmailIndex idx None (rowOf 1 "old@example.com" 1)
            |> ok

        let byNew =
            Dml.getByIndex store usersWithEmailIndex idx None (rowOf 1 "new@example.com" 2)
            |> ok

        Assert.True(byOld.IsNone)
        Assert.True(byNew.IsSome))

[<Fact>]
let ``RowStore: 主キーで全行を走査できる`` () =
    withDir "rs6" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        let txn =
            [ 1; 2; 3 ]
            |> List.fold
                (fun t i -> Dml.insert usersWithEmailIndex t (rowOf i (sprintf "u%d@example.com" i) i) |> ok)
                txn

        commitInto store 1UL txn

        let rows = Dml.scanByPrimaryKey store usersWithEmailIndex None 0 |> ok
        Assert.Equal(3, rows.Length)

        for i in 0..2 do
            Assert.Equal<int64>(int64 (i + 1), asInt rows.[i].[0]))

[<Fact>]
let ``RowStore: 主キーで全行を走査できる（limit）`` () =
    withDir "rs6b" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        let txn =
            [ 1; 2; 3; 4 ]
            |> List.fold
                (fun t i -> Dml.insert usersWithEmailIndex t (rowOf i (sprintf "u%d@example.com" i) i) |> ok)
                txn

        commitInto store 1UL txn

        let rows = Dml.scanByPrimaryKey store usersWithEmailIndex None 2 |> ok
        Assert.Equal(2, rows.Length))

[<Fact>]
let ``RowStore: keyspace ごとに独立したディレクトリを持つ`` () =
    withDir "rs7" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = Dml.insert usersWithEmailIndex txn (rowOf 1 "a@x.com" 1) |> ok

        commitInto store 1UL txn

        // 主キーのツリーと索引のツリーで別のディレクトリができている
        let dirs = Directory.GetDirectories(dir)
        Assert.True(dirs.Length >= 2)

        let hasWal =
            dirs |> Array.exists (fun d -> Directory.GetFiles(d, "*.wal").Length > 0)

        Assert.True(hasWal))

[<Fact>]
let ``RowStore: 複数テーブルは別 keyspace に載る`` () =
    let other =
        TableSchema.ofCreateTable
            {
                Name = "orders"
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
                            Name = "user_id"
                            DataType = DataType.Integer
                            TypeArguments = []
                            Constraint = ColumnConstraint.Empty
                        }
                    ]
                Constraints = []
            }
        |> ok

    withDir "rs8" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, t1 = TransactionManager.beginTxn mgr
        let t1 = Dml.insert usersWithEmailIndex t1 (rowOf 1 "a@x.com" 1) |> ok
        commitInto store 1UL t1

        let _, t2 = TransactionManager.beginTxn mgr
        let t2 = Dml.insert other t2 [| SqlValue.Int 1L; SqlValue.Int 1L |] |> ok
        commitInto store 10UL t2

        // 両方のテーブルが独立に引ける（シャーディングの確認）
        let u =
            Dml.getByPrimaryKey store usersWithEmailIndex None [| SqlValue.Int 1L |] |> ok

        let o = Dml.getByPrimaryKey store other None [| SqlValue.Int 1L |] |> ok

        Assert.True(u.IsSome)
        Assert.True(o.IsSome))
