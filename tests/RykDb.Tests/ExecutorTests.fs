// ===========================================================================
// Executor.fs のテスト: Volcano 実行器（式評価・走査・集約・結合・更新）
// ===========================================================================

module RykDb.ExecutorTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 2 列（a, b）のスキーマ。
let private schema2 =
    { Schema.empty with
        Columns =
            [|
                {
                    Name = "a"
                    Qualifier = None
                    DataType = Some DataType.Integer
                }
                {
                    Name = "b"
                    Qualifier = None
                    DataType = Some DataType.Integer
                }
            |]
    }

let private row2 (a: int64) (b: int64) = [| SqlValue.Int a; SqlValue.Int b |]

/// 行の配列を Op にする（入れ子の配列推論を避けるため明示的に作る）。
let private opOfRows (rows: SqlValue array list) = Op.ofArray (List.toArray rows)

/// 1 行を作る。
let private row (values: SqlValue list) = List.toArray values

/// SqlValue から値を取り出す（テスト用）。
let private asInt (v: SqlValue) =
    match v with
    | SqlValue.Int i -> i
    | other -> failwithf "INTEGER ではありません: %A" other

let private asDec (v: SqlValue) =
    match v with
    | SqlValue.Decimal d -> d
    | SqlValue.Int i -> decimal i
    | other -> failwithf "DECIMAL ではありません: %A" other

let private asReal (v: SqlValue) =
    match v with
    | SqlValue.Real f -> f
    | other -> failwithf "REAL ではありません: %A" other

let private asStr (v: SqlValue) =
    match v with
    | SqlValue.Text t -> t
    | other -> failwithf "TEXT ではありません: %A" other

let private col (name: string) =
    Expr.Column { Qualifier = None; Name = name }

let private ev (row: SqlValue array) (e: Expr) = Eval.eval schema2 row e

let private evI (row: SqlValue array) (e: Expr) =
    match ok (ev row e) with
    | SqlValue.Int i -> i
    | v -> failwithf "INTEGER 以外が返りました: %A" v

[<Fact>]
let ``Eval: 算術は SQL の昇格規則に従う`` () =
    // int + int = int
    Assert.Equal<int64>(7L, evI (row2 3L 4L) (Expr.Binary(BinaryOp.Add, col "a", col "b")))

    // int / int は整数除算（SQL の INTEGER と同じ）
    Assert.Equal<int64>(3L, evI (row2 7L 2L) (Expr.Binary(BinaryOp.Div, col "a", col "b")))

    // 0 での除算はエラー
    match ev (row2 1L 0L) (Expr.Binary(BinaryOp.Div, col "a", col "b")) with
    | Error _ -> ()
    | Ok v -> Assert.Fail(sprintf "0 除算が OK になっています: %A" v)

[<Fact>]
let ``Eval: NULL が算術に絡むと NULL`` () =
    let r = [| SqlValue.Int 1L; SqlValue.Null |]
    Assert.True(SqlValue.isNull (ok (ev r (Expr.Binary(BinaryOp.Add, col "a", col "b")))))

[<Fact>]
let ``Eval: NULL の比較は NULL（UNKNOWN）であって FALSE ではない`` () =
    let r = [| SqlValue.Null; SqlValue.Int 1L |]

    // `NULL = 1` は NULL
    Assert.True(SqlValue.isNull (ok (ev r (Expr.Binary(BinaryOp.Eq, col "a", col "b")))))

    // `NULL <> 1` 도 NULL（FALSE ではない）
    Assert.True(SqlValue.isNull (ok (ev r (Expr.Binary(BinaryOp.Ne, col "a", col "b")))))

    // `NULL IS NULL` は TRUE
    match ok (ev r (Expr.IsNull(col "a", false))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "IS NULL が TRUE になりません: %A" v)

[<Fact>]
let ``Eval: WHERE では UNKNOWN を偽として扱う`` () =
    let r = [| SqlValue.Null; SqlValue.Int 1L |]
    let pred = Expr.Binary(BinaryOp.Gt, col "a", col "b")

    // UNKNOWN なので偽
    Assert.False(ok (Eval.matches schema2 r pred))

[<Fact>]
let ``Eval: 3 値論理の AND / OR / NOT`` () =
    // FALSE AND UNKNOWN = FALSE
    let r1 = [| SqlValue.Bool false; SqlValue.Null |]

    match ok (ev r1 (Expr.Binary(BinaryOp.And, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.False b
    | v -> Assert.Fail(sprintf "FALSE AND NULL が FALSE になりません: %A" v)

    // TRUE OR UNKNOWN = TRUE
    let r2 = [| SqlValue.Bool true; SqlValue.Null |]

    match ok (ev r2 (Expr.Binary(BinaryOp.Or, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "TRUE OR NULL が TRUE になりません: %A" v)

    // NOT UNKNOWN = UNKNOWN
    let r3 = [| SqlValue.Null; SqlValue.Int 0L |]

    Assert.True(SqlValue.isNull (ok (ev r3 (Expr.Unary(UnaryOp.Not, col "a")))))

[<Fact>]
let ``Eval: BETWEEN と IN は NULL を正しく扱う`` () =
    let r = [| SqlValue.Int 5L; SqlValue.Int 1L |]

    // 5 BETWEEN 1 AND 10 = TRUE
    match ok (ev r (Expr.Between(col "a", col "b", Expr.Literal(Literal.Int 10L), false))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "BETWEEN が TRUE になりません: %A" v)

    // 5 NOT IN (1, 2) = TRUE
    let notIn =
        ok (ev r (Expr.InList(col "a", [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.Int 2L) ], true)))

    match notIn with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "NOT IN が TRUE になりません: %A" v)

[<Fact>]
let ``末尾再帰: 巨大な IN 候補と WHEN 節を評価できる`` () =
    let r = row2 0L 1L
    let n = 100_000

    // `eval ... >>= fun` のままだと候補の数だけスタックが積まれる
    // （→ StackOverflow / exit 134）。`match` で受けて末尾再帰にしてある。
    let candidates = [ for i in 1..n -> Expr.Literal(Literal.Int(int64 i)) ]

    match ok (ev r (Expr.InList(col "a", candidates, false))) with
    | SqlValue.Bool b -> Assert.False b
    | v -> Assert.Fail(sprintf "10 万件の IN が FALSE になりません: %A" v)

    // WHEN 節も同じ（全部偽なので ELSE が返る）
    let branches =
        [
            for _ in 1..n -> Expr.Literal(Literal.Bool false), Expr.Literal(Literal.Int 1L)
        ]

    Assert.Equal<int64>(9L, evI r (Expr.Case(branches, Some(Expr.Literal(Literal.Int 9L)))))

[<Fact>]
let ``Eval: LIKE の % と _`` () =
    let schema =
        { Schema.empty with
            Columns =
                [|
                    {
                        Name = "s"
                        Qualifier = None
                        DataType = Some DataType.Text
                    }
                |]
        }

    let test (pattern: string) (text: string) =
        let e = Expr.Like(col "s", Expr.Literal(Literal.String pattern), false)

        match ok (Eval.eval schema [| SqlValue.Text text |] e) with
        | SqlValue.Bool b -> b
        | v -> failwithf "BOOLEAN 以外: %A" v

    Assert.True(test "ab%" "abcdef")
    // "%cd" は「cd で終わる」必要があり、"abcdef" は "def" で終わるので不一致。
    Assert.False(test "%cd" "abcdef")
    Assert.True(test "%cd" "abcd")
    Assert.True(test "a_c" "abc")
    Assert.False(test "a_c" "abbc")
    Assert.True(test "%" "anything")
    // `%` の後に何もない（0 文字一致）も許す
    Assert.True(test "abc%" "abc")

[<Fact>]
let ``Eval: スカラー関数`` () =
    let schema =
        { Schema.empty with
            Columns =
                [|
                    {
                        Name = "s"
                        Qualifier = None
                        DataType = Some DataType.Text
                    }
                |]
        }

    let call (name: string) (args: Expr list) =
        ok (Eval.eval schema [| SqlValue.Text "  AbC  " |] (Expr.FuncCall(name, args)))

    match call "UPPER" [ col "s" ] with
    | SqlValue.Text t -> Assert.Equal("  ABC  ", t)
    | v -> Assert.Fail(sprintf "UPPER が文字列ではありません: %A" v)

    match call "TRIM" [ col "s" ] with
    | SqlValue.Text t -> Assert.Equal("AbC", t)
    | v -> Assert.Fail(sprintf "TRIM が文字列ではありません: %A" v)

    // 未知の関数はエラー
    match Eval.eval schema [||] (Expr.FuncCall("NOPE", [])) with
    | Error _ -> ()
    | Ok _ -> Assert.Fail("未知の関数が OK になっています")

[<Fact>]
let ``Eval: 修飾子の無い列名が曖昧ならエラー`` () =
    let schema =
        { Schema.empty with
            Columns =
                [|
                    {
                        Name = "id"
                        Qualifier = Some "a"
                        DataType = None
                    }
                    {
                        Name = "id"
                        Qualifier = Some "b"
                        DataType = None
                    }
                |]
        }

    match Schema.resolve schema { Qualifier = None; Name = "id" } with
    | Error _ -> ()
    | Ok _ -> Assert.Fail("曖昧な列名が解決できてしまいました")

    // 修飾子付きなら解決できる
    Assert.Equal(1, ok (Schema.resolve schema { Qualifier = Some "b"; Name = "id" }))

// ---------------------------------------------------------------------------
// Op（不変の pull 演算子）
// ---------------------------------------------------------------------------

[<Fact>]
let ``Op: 配列の演算子は 1 行ずつ返す`` () =
    let op: Op = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])

[<Fact>]
let ``Op: 次の行を引くまで計算しない`` () =
    // 無限に伸びる Op でも 1 行しか引かずに終わる。
    let rec infinite (n: int) : Op =
        Op.Compute(fun () -> ret (Some([| SqlValue.Int(int64 n) |], infinite (n + 1))))

    let op = Op.take 3 (infinite 0)
    let rows = ok (Op.toArray op)

    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[2].[0])

[<Fact>]
let ``Op: 大きな Op でも take は早期に打ち切る`` () =
    // 1000 万行の Op を遅延評価で作る。Step の tail を先に評価すると
    // 1000 万個が先に出来上がるので、Compute で 1 行ずつ生成する。
    let rec counter (n: int) : Op =
        Op.Compute(fun () ->
            if n >= 10_000_000 then
                ret None
            else
                ret (Some(row [ SqlValue.Int(int64 n) ], counter (n + 1))))

    let rows = ok (Op.toArray (Op.take 3 (counter 0)))
    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[2].[0])

[<Fact>]
let ``Op: filter と skip`` () =
    let op =
        Op.ofArray
            [|
                [| SqlValue.Int 1L |]
                [| SqlValue.Int 2L |]
                [| SqlValue.Int 3L |]
                [| SqlValue.Int 4L |]
            |]
        |> Op.filter (fun r -> Eval.matches schema2 r (Expr.Binary(BinaryOp.Gt, col "a", Expr.Literal(Literal.Int 2L))))
        |> Op.skip 1

    // a > 2 は 3 と 4 の 2 件。そこから 1 件読み飛ばすので 1 件残る。
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(4L, asInt rows.[0].[0])

// ---------------------------------------------------------------------------
// sortBy / distinct
// ---------------------------------------------------------------------------

[<Fact>]
let ``sortBy: 昇順と降順`` () =
    let op: Op =
        Op.ofArray [| [| SqlValue.Int 3L |]; [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    let keys =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
        ]

    let sorted = ok (Ops.sortBy schema2 op keys)
    let rows = ok (Op.toArray sorted)

    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal<int64>(3L, asInt rows.[2].[0])

    let desc =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Descending
            }
        ]

    let rows2 = ok (Op.toArray (ok (Ops.sortBy schema2 op desc)))

    Assert.Equal<int64>(3L, asInt rows2.[0].[0])

[<Fact>]
let ``sortBy: 複数キーは左から優先`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 20L ]
                row [ SqlValue.Int 1L; SqlValue.Int 10L ]
                row [ SqlValue.Int 0L; SqlValue.Int 99L ]
            ]

    let keys =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
            {
                Expression = col "b"
                Direction = SortDirection.Ascending
            }
        ]

    let rows = ok (Op.toArray (ok (Ops.sortBy schema2 op keys)))
    // 0/99, 1/10, 1/20
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(10L, asInt rows.[1].[1])
    Assert.Equal<int64>(20L, asInt rows.[2].[1])

[<Fact>]
let ``distinct: 最初に現れた順序を保つ`` () =
    let op: Op =
        Op.ofArray
            [|
                [| SqlValue.Int 2L |]
                [| SqlValue.Int 1L |]
                [| SqlValue.Int 2L |]
                [| SqlValue.Int 1L |]
            |]

    let rows = ok (Op.toArray (Ops.distinct op))

    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])

// ---------------------------------------------------------------------------
// 集約
// ---------------------------------------------------------------------------

let private agg (f: AggFunc) (isStar: bool) (e: Expr) : (AggFunc * bool * Expr) = (f, isStar, e)

[<Fact>]
let ``集約: COUNT / SUM / AVG / MIN / MAX`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 1L ]
                row [ SqlValue.Int 2L ]
                row [ SqlValue.Int 3L ]
                row [ SqlValue.Null ]
            ]

    let aggs =
        [
            (agg AggFunc.Count true (Expr.Literal(Literal.Null)))
            (agg AggFunc.Count false (col "a"))
            (agg AggFunc.Sum false (col "a"))
            (agg AggFunc.Avg false (col "a"))
            (agg AggFunc.Min false (col "a"))
            (agg AggFunc.Max false (col "a"))
        ]

    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 op [] aggs)))
    Assert.Equal<int>(1, rows.Length)

    let r = rows.[0]
    // COUNT(*) は NULL を含むので 4
    Assert.Equal<int64>(4L, asInt r.[0])

    // COUNT(列) は NULL を数えないので 3
    Assert.Equal<int64>(3L, asInt r.[1])

    // SUM は NULL を無視して 6
    Assert.Equal(6M, asDec r.[2])
    // AVG = 6 / 3 = 2
    Assert.Equal(2.0, asReal r.[3], 6)
    Assert.Equal<int64>(1L, asInt r.[4])
    Assert.Equal<int64>(3L, asInt r.[5])

[<Fact>]
let ``集約: 行が 0 でも COUNT(*) は 1 行を返す`` () =
    let rows =
        ok (
            Op.toArray (ok (Ops.aggregate schema2 Op.Done [] [ (agg AggFunc.Count true (Expr.Literal(Literal.Null))) ]))
        )

    // 0 件なら 0 行（1 行 (0) を返さない）。
    Assert.Equal<int>(0, rows.Length)

[<Fact>]
let ``集約: GROUP BY でグループごとに 1 行`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 10L ]
                row [ SqlValue.Int 1L; SqlValue.Int 20L ]
                row [ SqlValue.Int 2L; SqlValue.Int 30L ]
            ]

    let aggs = [ (agg AggFunc.Sum false (col "b")) ]
    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 op [ col "a" ] aggs)))

    // 2 グループ
    Assert.Equal<int>(2, rows.Length)

    // 最初に現れたグループが先
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal(30M, asDec rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[0])
    Assert.Equal(30M, asDec rows.[1].[1])

[<Fact>]
let ``集約: GROUP BY のキーが NULL の行も 1 グループになる`` () =
    let op: Op =
        Op.ofArray [| [| SqlValue.Null; SqlValue.Int 1L |]; [| SqlValue.Null; SqlValue.Int 2L |] |]

    let aggs = [ (agg AggFunc.Count true (Expr.Literal(Literal.Null))) ]
    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 op [ col "a" ] aggs)))

    // SQL では NULL が別のグループになるが、本実装は NULL 同士を等しい扱いにする。
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[1])

// ---------------------------------------------------------------------------
// 結合
// ---------------------------------------------------------------------------

let private leftSchema =
    { Schema.empty with
        Columns =
            [|
                {
                    Name = "lid"
                    Qualifier = Some "l"
                    DataType = Some DataType.Integer
                }
            |]
    }

let private rightSchema =
    { Schema.empty with
        Columns =
            [|
                {
                    Name = "rid"
                    Qualifier = Some "r"
                    DataType = Some DataType.Integer
                }
            |]
    }

let private lcol = Expr.Column { Qualifier = Some "l"; Name = "lid" }
let private rcol = Expr.Column { Qualifier = Some "r"; Name = "rid" }

[<Fact>]
let ``結合: INNER JOIN`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let right = Op.ofArray [| [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]
    let on = Expr.Binary(BinaryOp.Eq, lcol, rcol)

    let rows = ok (Op.toArray (ok (Ops.innerJoin leftSchema left rightSchema right on)))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(3L, asInt rows.[1].[0])

[<Fact>]
let ``結合: LEFT JOIN は一致が無ければ NULL`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 2L |] |]
    let on = Expr.Binary(BinaryOp.Eq, lcol, rcol)

    let rows = ok (Op.toArray (ok (Ops.leftJoin leftSchema left rightSchema right on)))
    Assert.Equal<int>(2, rows.Length)

    // 1 は一致が無いので右が NULL
    Assert.True(SqlValue.isNull rows.[0].[1])

    // 2 は一致
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

// ---------------------------------------------------------------------------
// 更新（RowStore を使う統合テスト）
// ---------------------------------------------------------------------------

/// テスト用のテーブルスキーマ。
let private peopleSchema: TableSchema =
    let mk (name: string) (dt: DataType) =
        {
            Name = name
            DataType = dt
            TypeArguments = []
            Constraint =
                {
                    NotNull = false
                    PrimaryKey = false
                    Unique = false
                    Default = None
                }
        }

    {
        Name = "people"
        Columns = [ mk "id" DataType.Integer; mk "name" DataType.Text; mk "age" DataType.Integer ]
        PrimaryKey = [ 0 ]
        Indexes = []
    }

let private ctxOf (store: RowStore) =
    ExecContext.create store (Snapshot.noLimit)

let private intArr (v: int64) = [| SqlValue.Int v |]
let private textArr (v: string) = [| SqlValue.Text v |]

[<Fact>]
let ``DML: INSERT は行を追加する`` () =
    withDir "ins" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let ctx = ctxOf store
        let mgr = TransactionManager.create ()
        let (mgr, txn) = TransactionManager.beginTxn mgr

        let (txn, n) =
            ok (
                DmlOps.insert
                    ctx
                    peopleSchema
                    txn
                    (Some [ "id"; "name" ])
                    [ [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.String "alice") ] ]
            )

        Assert.Equal<int>(1, n)

        let (mgr, _, batches) = ok (TransactionManager.commit mgr txn)
        ok (RowStore.writeBatches store batches) |> ignore

        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0)
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal("alice", asStr rows.[0].[1]))

[<Fact>]
let ``DML: UPDATE は WHERE に合う行だけを変える`` () =
    withDir "upd" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let ctx = ctxOf store

        // まず 2 行入れる
        let mgr0 = TransactionManager.create ()
        let (mgr0, txn0) = TransactionManager.beginTxn mgr0

        let (txn0, _) =
            ok (
                DmlOps.insert
                    ctx
                    peopleSchema
                    txn0
                    None
                    [
                        [
                            Expr.Literal(Literal.Int 1L)
                            Expr.Literal(Literal.String "alice")
                            Expr.Literal(Literal.Int 20L)
                        ]
                        [
                            Expr.Literal(Literal.Int 2L)
                            Expr.Literal(Literal.String "bob")
                            Expr.Literal(Literal.Int 30L)
                        ]
                    ]
            )

        let (mgr0, _, batches0) = ok (TransactionManager.commit mgr0 txn0)
        ok (RowStore.writeBatches store batches0) |> ignore

        // age が 25 以上の行の age を 1 増やす。mgr0 を引き継ぐ
        //（新しい TransactionManager は SeqNo が 1 から始まり、更新が消える）。
        let (mgr1, txn1) = TransactionManager.beginTxn mgr0

        let (txn1, n) =
            ok (
                DmlOps.update
                    ctx
                    peopleSchema
                    txn1
                    [ ("age", Expr.Binary(BinaryOp.Add, col "age", Expr.Literal(Literal.Int 1L))) ]
                    (Some(Expr.Binary(BinaryOp.Ge, col "age", Expr.Literal(Literal.Int 25L))))
            )

        Assert.Equal<int>(1, n)
        let (mgr1, _, batches1) = ok (TransactionManager.commit mgr1 txn1)
        ok (RowStore.writeBatches store batches1) |> ignore

        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0)

        // 1 行目は age 20（WHERE に合わないので変更なし）
        Assert.Equal<int64>(20L, asInt rows.[0].[2])

        // 2 行目は 30 -> 31
        Assert.Equal<int64>(31L, asInt rows.[1].[2]))

let ``DML: DELETE は WHERE に合う行を消す`` () =
    withDir "del" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let ctx = ctxOf store
        let mgr = TransactionManager.create ()
        let (mgr, txn) = TransactionManager.beginTxn mgr

        let (txn, _) =
            ok (
                DmlOps.insert
                    ctx
                    peopleSchema
                    txn
                    None
                    [
                        [
                            Expr.Literal(Literal.Int 1L)
                            Expr.Literal(Literal.String "alice")
                            Expr.Literal(Literal.Int 20L)
                        ]
                        [
                            Expr.Literal(Literal.Int 2L)
                            Expr.Literal(Literal.String "bob")
                            Expr.Literal(Literal.Int 30L)
                        ]
                    ]
            )

        let (mgr, _, batches) = ok (TransactionManager.commit mgr txn)
        ok (RowStore.writeBatches store batches) |> ignore

        let mgr2 = TransactionManager.create ()
        let (mgr2, txn2) = TransactionManager.beginTxn mgr2

        let (txn2, n) =
            ok (
                DmlOps.delete
                    ctx
                    peopleSchema
                    txn2
                    (Some(Expr.Binary(BinaryOp.Eq, col "id", Expr.Literal(Literal.Int 1L))))
            )

        Assert.Equal<int>(1, n)
        let (mgr2, _, batches2) = ok (TransactionManager.commit mgr2 txn2)
        ok (RowStore.writeBatches store batches2) |> ignore

        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0)
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal("bob", asStr rows.[0].[1]))

[<Fact>]
let ``DML: INSERT は列順を明示できる`` () =
    withDir "inscols" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let ctx = ctxOf store
        let mgr = TransactionManager.create ()
        let (mgr, txn) = TransactionManager.beginTxn mgr

        // name, id の順で渡す
        let (txn, _) =
            ok (
                DmlOps.insert
                    ctx
                    peopleSchema
                    txn
                    (Some [ "name"; "id" ])
                    [ [ Expr.Literal(Literal.String "carol"); Expr.Literal(Literal.Int 7L) ] ]
            )

        let (mgr, _, batches) = ok (TransactionManager.commit mgr txn)
        ok (RowStore.writeBatches store batches) |> ignore

        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0)
        Assert.Equal<int64>(7L, asInt rows.[0].[0])
        Assert.Equal("carol", asStr rows.[0].[1])
        // 指定されなかった列は NULL
        Assert.True(SqlValue.isNull rows.[0].[2]))

[<Fact>]
let ``DML: INSERT の列数不一致はエラー`` () =
    withDir "insbad" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let ctx = ctxOf store
        let mgr = TransactionManager.create ()
        let (mgr, txn) = TransactionManager.beginTxn mgr

        let bad = [ [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.String "x") ] ]

        match DmlOps.insert ctx peopleSchema txn (Some [ "id" ]) bad with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("列数不一致が OK になっています"))

// ---------------------------------------------------------------------------
// スタック深度（gotchas §2.1: Step は遅延しない → Compute ラップ）
// ---------------------------------------------------------------------------

[<Fact>]
let ``末尾再帰: Op.ofArray を 20 万行組み立ててもスタックを食わない`` () =
    // 「let rest = go (i + 1) を先に評価する」実装だとコンストラクト時に
    // 行数ぶんのスタックを積み、実測 約 13 万段（Debug）で Stack overflow になる。
    // Compute ラップで「次を要求されてから 1 行だけ」作ることを深い入力で検証する。
    let rows = Array.init 200_000 (fun i -> [| SqlValue.Int(int64 i) |])
    let op = Op.ofArray rows
    let got = ok (Op.toArray op)

    Assert.Equal<int>(200_000, got.Length)
    Assert.Equal<int64>(0L, (got.[0].[0] |> SqlValue.asInt64 |> Option.defaultValue -1L))
    Assert.Equal<int64>(199_999L, (got.[got.Length - 1].[0] |> SqlValue.asInt64 |> Option.defaultValue -1L))
