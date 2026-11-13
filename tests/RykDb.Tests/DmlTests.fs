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
    Dml.insertWithKey (RowKey.primary schema row) txn schema row

/// transaction のバッファを DB に流し込む。
let private commitInto (store: RowStore) (nextSeq: uint64) (txn: Transaction) =
    let batches = Transaction.toEntries nextSeq txn |> fst
    RowStore.writeBatches batches store |> ok |> ignore

[<Fact>]
let ``RowKey.primary: 主キー列を符号化する`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]
    let k = RowKey.primary schema row
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
