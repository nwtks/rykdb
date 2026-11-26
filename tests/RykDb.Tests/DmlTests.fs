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

let private usersWithAgeIndex =
    withIndex
        usersSchema
        {
            Name = "users_age"
            Columns = [ "age" ]
            Unique = false
        }

let private ageIndex =
    usersWithAgeIndex.Indexes |> List.find (fun i -> i.Name = "users_age")

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
let ``Dml.scanByPrimaryKeyRange: 上限を超える行と範囲外の書き込みは結果に混ざらない`` () =
    withStore "dml-range-txn" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn0 = TransactionManager.beginTxn mgr

        let txn0 =
            [ 1; 2; 3; 4; 5 ]
            |> List.fold
                (fun t i -> insertRow usersWithEmailIndex t (rowOf i (sprintf "u%d@example.com" i) i) |> ok)
                txn0

        commitInto store 1UL txn0

        // 範囲の外（5 の削除、0 / 6 の追加）も同じ transaction に積む。
        let _, txn1 = TransactionManager.beginTxn mgr
        let txn1 = Dml.deleteRow txn1 usersWithEmailIndex (rowOf 5 "u5@example.com" 5) |> ok
        let txn1 = insertRow usersWithEmailIndex txn1 (rowOf 0 "u0@example.com" 0) |> ok
        let txn1 = insertRow usersWithEmailIndex txn1 (rowOf 6 "u6@example.com" 6) |> ok

        let rows =
            Dml.scanByPrimaryKeyRangeWithTxnMask
                store
                usersWithEmailIndex
                [| SqlValue.Int 2L |]
                (Some [| SqlValue.Int 4L |])
                0
                (allTrue usersWithEmailIndex)
                None
                None
                txn1
            |> ok
            |> Op.toArray
            |> ok

        let ids = rows |> Array.toList |> List.map (fun r -> asInt r.[0])
        Assert.Equal<int64 list>([ 2L; 3L; 4L ], ids))

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
                (allTrue usersWithEmailIndex)
                None
                None
                emptyTxn
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
                [| false; true; false |]
                None
                None
                emptyTxn
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
                (allTrue usersWithEmailIndex)
                None
                None
                txn
            |> ok
            |> Op.toArray
            |> ok

        Assert.Equal(1, rows.Length)
        Assert.Equal<int64>(1L, asInt rows.[0].[0]))

[<Fact>]
let ``Dml.scanByIndexPrefix: store のキーより前の書き込みを先に返す`` () =
    withStore "dml-index-prefix-replay" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn0 = TransactionManager.beginTxn mgr
        let txn0 = insertRow usersWithAgeIndex txn0 (rowOf 2 "u2@example.com" 15) |> ok
        commitInto store 1UL txn0

        // 同じ索引値で主キーが小さい行を transaction に積む
        // （store の索引キーより前になるので、store を先読みして戻す必要がある）。
        let _, txn1 = TransactionManager.beginTxn mgr
        let txn1 = insertRow usersWithAgeIndex txn1 (rowOf 1 "u1@example.com" 15) |> ok

        let rows =
            Dml.scanByIndexPrefixWithTxnMask
                store
                usersWithAgeIndex
                ageIndex
                [| SqlValue.Int 15L |]
                0
                (allTrue usersWithAgeIndex)
                None
                None
                txn1
            |> ok
            |> Op.toArray
            |> ok

        let ids = rows |> Array.toList |> List.map (fun r -> asInt r.[0])
        Assert.Equal<int64 list>([ 1L; 2L ], ids))

[<Fact>]
let ``Dml.scanByIndexRange: transaction の索引書き込みを store のキー間に挿入する`` () =
    withStore "dml-index-range-insert" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn0 = TransactionManager.beginTxn mgr

        let txn0 =
            [ 1, 10; 2, 20; 3, 30 ]
            |> List.fold
                (fun t (id, age) ->
                    insertRow usersWithAgeIndex t (rowOf id (sprintf "u%d@example.com" id) age)
                    |> ok)
                txn0

        commitInto store 1UL txn0

        let _, txn1 = TransactionManager.beginTxn mgr
        let txn1 = insertRow usersWithAgeIndex txn1 (rowOf 4 "u4@example.com" 15) |> ok

        let rows =
            Dml.scanByIndexRangeWithTxnMask
                store
                usersWithAgeIndex
                ageIndex
                [| SqlValue.Int 10L |]
                (Some [| SqlValue.Int 30L |])
                0
                (allTrue usersWithAgeIndex)
                None
                None
                txn1
            |> ok
            |> Op.toArray
            |> ok

        let ages = rows |> Array.toList |> List.map (fun r -> asInt r.[2])
        let ids = rows |> Array.toList |> List.map (fun r -> asInt r.[0])
        Assert.Equal<int64 list>([ 10L; 15L; 20L; 30L ], ages)
        Assert.Equal<int64 list>([ 1L; 4L; 2L; 3L ], ids))

[<Fact>]
let ``Dml.scanByIndexRange: 削除した行の索引エントリを残さずに overlay する`` () =
    withStore "dml-index-range-delete" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn0 = TransactionManager.beginTxn mgr

        let txn0 =
            [ 1, 10; 2, 20; 3, 30 ]
            |> List.fold
                (fun t (id, age) ->
                    insertRow usersWithAgeIndex t (rowOf id (sprintf "u%d@example.com" id) age)
                    |> ok)
                txn0

        commitInto store 1UL txn0

        let _, txn1 = TransactionManager.beginTxn mgr
        let txn1 = Dml.deleteRow txn1 usersWithAgeIndex (rowOf 2 "u2@example.com" 20) |> ok
        commitInto store 7UL txn1

        let _, txn2 = TransactionManager.beginTxn mgr
        let txn2 = insertRow usersWithAgeIndex txn2 (rowOf 4 "u4@example.com" 25) |> ok

        let rows =
            Dml.scanByIndexRangeWithTxnMask
                store
                usersWithAgeIndex
                ageIndex
                [| SqlValue.Int 10L |]
                (Some [| SqlValue.Int 30L |])
                0
                (allTrue usersWithAgeIndex)
                None
                None
                txn2
            |> ok
            |> Op.toArray
            |> ok

        let ages = rows |> Array.toList |> List.map (fun r -> asInt r.[2])
        let ids = rows |> Array.toList |> List.map (fun r -> asInt r.[0])
        Assert.Equal<int64 list>([ 10L; 25L; 30L ], ages)
        Assert.Equal<int64 list>([ 1L; 4L; 3L ], ids))

[<Fact>]
let ``Dml.updateRow: 索引列が変わらない更新では索引エントリを書かない`` () =
    withStore "dml-index-skip" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let row = rowOf 1 "a@example.com" 10
        commitInto store 1UL (insertRow usersWithEmailIndex txn row |> ok)

        let _, updateTxn = TransactionManager.beginTxn mgr

        // email（索引列）はそのまま、age だけ変える。
        let sameKey =
            Dml.updateRow updateTxn usersWithEmailIndex row (rowOf 1 "a@example.com" 11)
            |> ok

        Assert.Empty(Transaction.slotsOf (Keyspace.index "users_email") sameKey)

        // email（索引列）を変えれば索引エントリが書かれる。
        let newKey =
            Dml.updateRow updateTxn usersWithEmailIndex row (rowOf 1 "b@example.com" 10)
            |> ok

        Assert.Equal(2, (Transaction.slotsOf (Keyspace.index "users_email") newKey).Length))

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
