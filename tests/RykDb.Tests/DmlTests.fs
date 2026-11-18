module RykDb.DmlTests

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

/// 主キーを計算して 1行を transaction に積む。
let private insertRow (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
    Dml.insertWithKey (RowKey.primary schema row |> ok) txn schema row

/// transaction のバッファを DB に流し込む。
let private commitInto (store: RowStore) (nextSeq: uint64) (txn: Transaction) =
    let batches = Transaction.toEntries nextSeq txn |> fst
    RowStore.writeBatches batches store |> ok |> ignore

/// 読み取り用の空 transaction。
let private emptyTxn = Transaction.create 0UL<txnId> { SeqNo = 0UL }

let private allTrue (schema: TableSchema) = Array.create schema.Columns.Length true

let private emailIndex =
    usersWithEmailIndex.Indexes |> List.find (fun i -> i.Name = "users_email")

[<Fact>]
let ``RowKey.primary: 主キー列を符号化する`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]
    let k = RowKey.primary schema row |> ok
    Assert.True(k.Length > 0)

[<Fact>]
let ``Dml.scanByPrimaryKey: 主キーで全行を走査できる`` () =
    withStore "rs6" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        let txn =
            [ 1; 2; 3 ]
            |> List.fold
                (fun t i -> insertRow usersWithEmailIndex t (rowOf i (sprintf "u%d@example.com" i) i) |> ok)
                txn

        commitInto store 1UL txn

        let rows =
            Dml.scanByPrimaryKey store usersWithEmailIndex 0 None |> ok |> Op.toArray |> ok

        Assert.Equal(3, rows.Length)

        for i in 0..2 do
            Assert.Equal<int64>(int64 (i + 1), asInt rows.[i].[0]))

[<Fact>]
let ``Dml.scanByPrimaryKey: 主キーで全行を走査できる（limit）`` () =
    withStore "rs6b" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        let txn =
            [ 1; 2; 3; 4 ]
            |> List.fold
                (fun t i -> insertRow usersWithEmailIndex t (rowOf i (sprintf "u%d@example.com" i) i) |> ok)
                txn

        commitInto store 1UL txn

        let rows =
            Dml.scanByPrimaryKey store usersWithEmailIndex 2 None |> ok |> Op.toArray |> ok

        Assert.Equal(2, rows.Length))

[<Fact>]
let ``Dml.insertWithKey: 索引エントリの値は空になる`` () =
    withStore "dml-index-empty" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = insertRow usersWithEmailIndex txn (rowOf 1 "a@example.com" 10) |> ok

        let slots = Transaction.slotsOf (Keyspace.index "users_email") txn
        Assert.NotEmpty slots

        for _, value in slots do
            match value with
            | Some values -> Assert.Empty values
            | None -> failwith "索引エントリが tombstone になっています"

        commitInto store 1UL txn

        let entries =
            RowStore.scan (Keyspace.index "users_email") [||] 0 store
            |> ok
            |> SstScan.toArray
            |> ok

        for entry in entries do
            match entry.Value with
            | Some values -> Assert.Empty values
            | None -> failwith "永続化された索引エントリが tombstone です")

[<Fact>]
let ``Dml.scanByIndexPrefix: 索引キーから索引列と主キーを復元する`` () =
    withStore "dml-index-row" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        let txn =
            [ 1, "a@example.com", 10; 2, "b@example.com", 20 ]
            |> List.fold (fun t (id, email, age) -> insertRow usersWithEmailIndex t (rowOf id email age) |> ok) txn

        commitInto store 1UL txn

        let rows =
            Dml.scanByIndexPrefixWithTxnMask
                store
                usersWithEmailIndex
                emailIndex
                [| SqlValue.Text "b@example.com" |]
                0
                None
                None
                emptyTxn
                (allTrue usersWithEmailIndex)
            |> ok
            |> Op.toArray
            |> ok

        Assert.Equal(1, rows.Length)
        Assert.Equal<int64>(2L, asInt rows.[0].[0])
        Assert.Equal<string>("b@example.com", asText rows.[0].[1])
        Assert.Equal<SqlValue>(SqlValue.Unloaded, rows.[0].[2]))

[<Fact>]
let ``Dml.scanByIndexPrefix: マスクした列は Unloaded になる`` () =
    withStore "dml-index-mask" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = insertRow usersWithEmailIndex txn (rowOf 1 "a@example.com" 10) |> ok
        commitInto store 1UL txn

        let rows =
            Dml.scanByIndexPrefixWithTxnMask
                store
                usersWithEmailIndex
                emailIndex
                [| SqlValue.Text "a@example.com" |]
                0
                None
                None
                emptyTxn
                [| false; true; false |]
            |> ok
            |> Op.toArray
            |> ok

        Assert.Equal(1, rows.Length)
        Assert.Equal<SqlValue>(SqlValue.Unloaded, rows.[0].[0])
        Assert.Equal<string>("a@example.com", asText rows.[0].[1])
        Assert.Equal<SqlValue>(SqlValue.Unloaded, rows.[0].[2]))

[<Fact>]
let ``Dml.scanByIndexPrefix: transaction の索引書き込みもキーで overlay する`` () =
    withStore "dml-index-txn" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = insertRow usersWithEmailIndex txn (rowOf 1 "a@example.com" 10) |> ok

        let rows =
            Dml.scanByIndexPrefixWithTxnMask
                store
                usersWithEmailIndex
                emailIndex
                [| SqlValue.Text "a@example.com" |]
                0
                None
                None
                txn
                (allTrue usersWithEmailIndex)
            |> ok
            |> Op.toArray
            |> ok

        Assert.Equal(1, rows.Length)
        Assert.Equal<int64>(1L, asInt rows.[0].[0]))

[<Fact>]
let ``Dml.insertWithKey: NOT NULL 制約違反は弾かれる`` () =
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

    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr
    // email が NULL
    let bad = [| SqlValue.Int 1L; SqlValue.Null; SqlValue.Int 20L |]
    expectErrorKind ErrorKind.ConstraintViolation "NOT NULL 制約違反を検出できなかった" (insertRow schema txn bad)

[<Fact>]
let ``Dml.insertWithKey: transaction 上限を超えると CapacityError`` () =
    let txn =
        { Transaction.create 1UL<txnId> { SeqNo = 0UL } with
            LimitBytes = 1L
        }

    expectErrorKind
        ErrorKind.CapacityError
        "transaction 上限を超えた INSERT を検出できなかった"
        (insertRow usersSchema txn (rowOf 1 "a@x.com" 1))

[<Fact>]
let ``Dml.insertWithKey: 列数が違う行は弾かれる`` () =
    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr

    expectErrorKind
        ErrorKind.ConstraintViolation
        "列数不一致を検出できなかった"
        (insertRow usersWithEmailIndex txn [| SqlValue.Int 1L |])
