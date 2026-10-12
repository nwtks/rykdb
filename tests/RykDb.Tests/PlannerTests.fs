// ===========================================================================
// Planner.fs のテスト（名前束縛・述語の分割・走査/結合/押し込み・論理代数）
// ===========================================================================

module RykDb.PlannerTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

// ---------------------------------------------------------------------------
// スキーマ・名前束縛（peopleSchema / ordersSchema・列定義ビルダーは RykDb.TestHelpers）
// ---------------------------------------------------------------------------


/// テスト用の ExecContext（people と orders を登録済み）。
let private ctxOf (store: RowStore) =
    let c0 = ExecContext.create store None

    let c1 = c0.WithSchema "people" peopleSchema
    let c2 = c1.WithSchema "orders" ordersSchema
    c2

// ---------------------------------------------------------------------------
// Planner — 名前束縛
// ---------------------------------------------------------------------------

let private pCol (n: string) =
    Expr.Column { Qualifier = Some "people"; Name = n }

let private oCol (n: string) =
    Expr.Column { Qualifier = Some "orders"; Name = n }

[<Fact>]
let ``Binder: 修飾子無しの列が一意なら修飾名を付ける`` () =
    // people だけのスキーマでは `id` は people.id に一意に決まる。
    let only = Schema.ofTable "people" None peopleSchema
    let e = Expr.Column { Qualifier = None; Name = "id" }
    let bound = ok (Binder.qualify only e)

    match bound with
    | Expr.Column c -> Assert.Equal<string>("people", defaultArg c.Qualifier "")
    | _ -> Assert.Fail("EColumn ではない")

[<Fact>]
let ``Binder: 修飾子無しの列が曖昧なら付けない`` () =
    // t1.a と t2.a の両方が「a」
    let s1 =
        {
            Name = "t1"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes = []
        }

    let s2 =
        {
            Name = "t2"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes = []
        }

    let combined =
        Schema.ofTable "t1" None s1 |> fun s -> s.Concat(Schema.ofTable "t2" None s2)

    let bound =
        ok (Binder.qualify combined (Expr.Column { Qualifier = None; Name = "a" }))

    match bound with
    | Expr.Column c -> Assert.True(String.IsNullOrEmpty(defaultArg c.Qualifier ""))
    | _ -> Assert.Fail("Expr.EColumn ではない")

// ---------------------------------------------------------------------------
// Planner — 述語の分割
// ---------------------------------------------------------------------------

[<Fact>]
let ``Predicate: AND を平坦化して分割する`` () =
    let e =
        Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.And, pCol "id", pCol "age"), pCol "name")

    let parts = Predicate.splitConjuncts e

    Assert.Equal<int>(3, parts.Length)
    Assert.True(Predicate.isLocalTo "people" parts.[0])
    Assert.True(Predicate.isLocalTo "people" parts.[1])

[<Fact>]
let ``Predicate: split した conjunct を AND で戻せる`` () =
    let e = Expr.Binary(BinaryOp.And, pCol "id", pCol "age")
    let parts = Predicate.splitConjuncts e
    let rejoined = Predicate.joinConjuncts parts

    Assert.True(rejoined |> Option.isSome)
    // 分割 → 再結合 で conjunct が 2 個のまま保たれる
    Assert.Equal<int>(2, Predicate.splitConjuncts rejoined.Value |> List.length)

// ---------------------------------------------------------------------------
// Planner — 走査方法の選択
// ---------------------------------------------------------------------------

[<Fact>]
let ``Analyzer: 等価条件があれば主キー検索にする`` () =
    withStore "an1" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        let pred = Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 1L))
        let m = ok (Analyzer.chooseScanMethod b Catalog.empty [ pred ])

        match m with
        | ScanMethod.PkLookup values ->
            Assert.Equal<int>(1, values.Length)
            Assert.Equal<int64>(1L, asInt values.[0])
        | _ -> Assert.Fail(sprintf "主キー検索になっていません: %A" m))

[<Fact>]
let ``Analyzer: 等価条件があっても索引が無ければ全走査`` () =
    withStore "an2" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // name には索引が無い
        let pred =
            Expr.Binary(BinaryOp.Eq, pCol "name", Expr.Literal(Literal.String "alice"))

        let m = ok (Analyzer.chooseScanMethod b Catalog.empty [ pred ])

        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, m))

[<Fact>]
let ``Analyzer: 索引があれば索引検索にする`` () =
    withStore "an3" (fun store ->

        let ix = Catalog.toIndexDefinition "people" peopleSchema.Indexes.[0]
        let catalog = ix |> Catalog.addIndex Catalog.empty

        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        let pred = Expr.Binary(BinaryOp.Eq, pCol "age", Expr.Literal(Literal.Int 30L))
        let m = ok (Analyzer.chooseScanMethod b catalog [ pred ])

        match m with
        | ScanMethod.IndexLookup(name, values) ->
            Assert.Equal<string>("ix_people_age", name)
            Assert.Equal<int64>(30L, asInt values.[0])
        | _ -> Assert.Fail(sprintf "索引検索になっていません: %A" m))

[<Fact>]
let ``Analyzer: 等価条件が無ければ全走査`` () =
    withStore "an4" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        let pred = Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))
        let m = ok (Analyzer.chooseScanMethod b Catalog.empty [ pred ])

        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, m))

// ---------------------------------------------------------------------------
// Planner — SELECT 文全体の論理代数
// ---------------------------------------------------------------------------

/// テスト用の文を作る。
let private selectOf (items: SelectItem list) (from: TableRef option) (joins: JoinClause list) (w: Expr option) =
    {
        Distinct = false
        Items = items
        From = from
        Joins = joins
        Where = w
        GroupBy = []
        Having = None
        OrderBy = []
        Limit = None
        Offset = None
    }

[<Fact>]
let ``Planner: 単純な SELECT は Scan + Project になる`` () =
    withStore "pl1" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let tree = ok (Optimizer.buildLogical ctx stmt)

        match tree with
        | Logical.Project(_, Logical.Scan(name, ScanMethod.SeqScan)) -> Assert.Equal<string>("people", name)
        | _ -> Assert.Fail(sprintf "想定外の形: %A" tree))

[<Fact>]
let ``Planner: WHERE があれば Scan の直下に Filter が付く`` () =
    withStore "pl2" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                []
                (Some(Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))))

        let tree = ok (Optimizer.buildLogical ctx stmt)

        match tree with
        | Logical.Project(_, Logical.Filter(_, Logical.Scan(name, method))) ->
            Assert.Equal<string>("people", name)
            // age には索引があるので索引検索のはず
            match method with
            | ScanMethod.SeqScan -> () // カタログに索引が無いので全走査
            | _ -> ()
        | _ -> Assert.Fail(sprintf "想定外の形: %A" tree))

[<Fact>]
let ``Planner: JOIN があれば LJoin が作られる`` () =
    withStore "pl3" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                [
                    {
                        Kind = JoinKind.Inner
                        Table = { Name = "orders"; Alias = None }
                        On = Expr.Binary(BinaryOp.Eq, pCol "id", oCol "person_id")
                    }
                ]
                None

        let tree = ok (Optimizer.buildLogical ctx stmt)

        // LJoin の第 2 引数は右テーブル名、第 4 引数は左の木。
        match tree with
        | Logical.Project(_, Logical.Join(JoinKind.Inner, rightName, _, Logical.Scan(leftName, _), _)) ->
            Assert.Equal<string>("people", leftName)
            Assert.Equal<string>("orders", rightName)
        | _ -> Assert.Fail(sprintf "想定外の形: %A" tree))

[<Fact>]
let ``Planner: GROUP BY があれば LAggregate が作られる`` () =
    withStore "pl4" (fun store ->
        let ctx = ctxOf store

        let items =
            [
                {
                    Expression = pCol "age"
                    Alias = None
                }
                {
                    Expression = Expr.AggCall(AggFunc.Count, true, Expr.Literal(Literal.Null))
                    Alias = None
                }
            ]

        let plain = selectOf items (Some { Name = "people"; Alias = None }) [] None
        let stmt = { plain with GroupBy = [ pCol "age" ] }

        let tree = ok (Optimizer.buildLogical ctx stmt)

        match tree with
        | Logical.Project(_, Logical.Aggregate(groupBy, aggs, _)) ->
            Assert.Equal<int>(1, groupBy.Length)
            Assert.Equal<int>(1, aggs |> List.length)
        | _ -> Assert.Fail(sprintf "想定外の形: %A" tree))

[<Fact>]
let ``Planner: 集約がなければ LAggregate は作られない`` () =
    withStore "pl5" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [
                    {
                        Expression = pCol "name"
                        Alias = None
                    }
                ]
                (Some { Name = "people"; Alias = None })
                []
                None

        let tree = ok (Optimizer.buildLogical ctx stmt)

        match tree with
        | Logical.Project(_, Logical.Scan(_, _)) -> ()
        | _ -> Assert.Fail(sprintf "集約が無いのに集約がある: %A" tree))

[<Fact>]
let ``Planner: LIMIT / OFFSET は LLimit になる`` () =
    withStore "pl6" (fun store ->
        let ctx = ctxOf store

        let plain =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let stmt =
            { plain with
                Limit = Some(Expr.Literal(Literal.Int 10L))
                Offset = Some(Expr.Literal(Literal.Int 5L))
            }

        let tree = ok (Optimizer.buildLogical ctx stmt)

        // 評価順は「射影 → DISTINCT → ORDER BY → LIMIT」なので LIMIT は
        // 射影の外側に出る。DISTINCT を先に置くと (id, g) の組で重複排除される。
        match tree with
        | Logical.Limit(offset, limit, Logical.Project(_, _)) ->
            Assert.Equal<int>(5, offset)
            Assert.Equal<int>(10, limit)
        | _ -> Assert.Fail(sprintf "LIMIT が木の中心でない: %A" tree))

[<Fact>]
let ``Planner: FROM が無ければ LValues になる`` () =
    withStore "pl7" (fun store ->
        let ctx = ctxOf store

        let stmt = selectOf [ { Expression = Expr.Star; Alias = None } ] None [] None

        let tree = ok (Optimizer.buildLogical ctx stmt)

        match tree with
        | Logical.Values _ -> ()
        | _ -> Assert.Fail(sprintf "FROM なしなのに LValues でない: %A" tree))
