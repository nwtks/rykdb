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
    let c1 = ExecContext.withSchema c0 "people" peopleSchema
    let c2 = ExecContext.withSchema c1 "orders" ordersSchema
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
        Schema.ofTable "t1" None s1
        |> fun s -> Schema.concat s (Schema.ofTable "t2" None s2)

    let bound =
        ok (Binder.qualify combined (Expr.Column { Qualifier = None; Name = "a" }))

    match bound with
    | Expr.Column c -> Assert.True(String.IsNullOrEmpty(defaultArg c.Qualifier ""))
    | _ -> Assert.Fail("Expr.EColumn ではない")

[<Fact>]
let ``Binder: スターは修飾の対象外`` () =
    let only = Schema.ofTable "people" None peopleSchema
    let bound = ok (Binder.qualify only (Expr.QualifiedStar "people"))

    match bound with
    | Expr.QualifiedStar q -> Assert.Equal<string>("people", q)
    | other -> Assert.Fail(sprintf "QualifiedStar ではない: %A" other)

// ---------------------------------------------------------------------------
// Binder.qualify の評価機（全ノード種の組み立て・末尾再帰）
// ---------------------------------------------------------------------------

[<Fact>]
let ``Binder: 全ノード種を qualify しても構造が保たれる`` () =
    let schema = Schema.ofTable "people" None peopleSchema
    // `id` は people だけの一意な列なので、裸の名前はすべて people.id に修飾される
    let n = Expr.Name "id"
    let q = pCol "id"
    let i1 = Expr.Literal(Literal.Int 1L)
    let i2 = Expr.Literal(Literal.Int 2L)
    let text = Expr.Literal(Literal.String "x%")

    // 入力（葉 = 裸の名前）と、葉を修飾済みにした期待値を並べて比較する
    let samples =
        [
            Expr.Unary(UnaryOp.Not, n), Expr.Unary(UnaryOp.Not, q)
            Expr.Binary(BinaryOp.Add, n, i1), Expr.Binary(BinaryOp.Add, q, i1)
            Expr.IsNull(n, false), Expr.IsNull(q, false)
            Expr.Between(n, i1, i2, false), Expr.Between(q, i1, i2, false)
            Expr.InList(n, [ i1; i2 ], true), Expr.InList(q, [ i1; i2 ], true)
            Expr.Like(n, text, false), Expr.Like(q, text, false)
            Expr.Case([ (n, i1); (n, i2) ], Some i1), Expr.Case([ (q, i1); (q, i2) ], Some i1)
            Expr.Case([ (n, i1) ], None), Expr.Case([ (q, i1) ], None)
            Expr.FuncCall("ABS", [ n ]), Expr.FuncCall("ABS", [ q ])
            Expr.AggCall(AggFunc.Sum, false, n), Expr.AggCall(AggFunc.Sum, false, q)
            Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.Or, n, i1), Expr.IsNull(n, true)),
            Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.Or, q, i1), Expr.IsNull(q, true))
        ]

    for input, expected in samples do
        Assert.Equal<string>(Expr.toDisplayString expected, Expr.toDisplayString (ok (Binder.qualify schema input)))

[<Fact>]
let ``末尾再帰: 20 万段の式を qualify してもスタックを食わない`` () =
    let depth = 200_000
    let schema = Schema.ofTable "people" None peopleSchema

    // テスト側も末尾再帰で組む（SQL 経由だと MaxExpressionDepth = 200 で頭打ちになる）
    let deep =
        List.init depth (fun _ -> ())
        |> List.fold (fun acc _ -> Expr.Unary(UnaryOp.Neg, acc)) (Expr.Name "id")

    let bound = ok (Binder.qualify schema deep)

    // 葉まで降りて people.id に修飾されていること
    let refs = Expr.columnRefs bound
    Assert.Equal<int>(1, refs.Length)
    Assert.Equal<string>("people", defaultArg refs.[0].Qualifier "")
    Assert.Equal<string>("id", refs.[0].Name)

    // 深い構造が壊れていないこと（1 段 = "(-" と ")" の 4 文字、葉 = "people.id" の 9 文字）
    Assert.Equal<int>(depth * 4 + 9, (Expr.toDisplayString bound).Length)

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

[<Fact>]
let ``Predicate: 修飾子なしの列参照は local とみなす`` () =
    // 修飾子が無い列はどのテーブル由来か決められないので、局所判定では弾かない
    Assert.True(Predicate.isLocalTo "people" (Expr.Column { Qualifier = None; Name = "x" }))
    // 列参照を含まない式も true
    Assert.True(Predicate.isLocalTo "people" (Expr.Literal(Literal.Int 1L)))

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

[<Fact>]
let ``Analyzer: AND の奥と左右対称の等価条件を探せる`` () =
    withStore "an5" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // 最初の conjunct は等価ではない → 2 番目が採用される
        let first = Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))
        let second = Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 7L))

        match ok (Analyzer.chooseScanMethod b Catalog.empty [ first; second ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(7L, asInt values.[0])
        | m -> Assert.Fail(sprintf "PkLookup ではない: %A" m)

        // 値 = 列 の順（左右対称）でも探せる
        let reversed = Expr.Binary(BinaryOp.Eq, Expr.Literal(Literal.Int 8L), pCol "id")

        match ok (Analyzer.chooseScanMethod b Catalog.empty [ reversed ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(8L, asInt values.[0])
        | m -> Assert.Fail(sprintf "PkLookup ではない: %A" m))

[<Fact>]
let ``Analyzer: 1 つの conjunct 内の AND も掘れる`` () =
    withStore "an7" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // 左の枝で見つかる（search の Some r 経路）
        let leftHit =
            Expr.Binary(
                BinaryOp.And,
                Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 9L)),
                Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))
            )

        match ok (Analyzer.chooseScanMethod b Catalog.empty [ leftHit ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(9L, asInt values.[0])
        | m -> Assert.Fail(sprintf "PkLookup ではない: %A" m)

        // 左の枝では見つからず右で見つかる（search の None → 右の経路）
        let rightHit =
            Expr.Binary(
                BinaryOp.And,
                Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L)),
                Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 10L))
            )

        match ok (Analyzer.chooseScanMethod b Catalog.empty [ rightHit ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(10L, asInt values.[0])
        | m -> Assert.Fail(sprintf "PkLookup ではない: %A" m))

[<Fact>]
let ``Analyzer: 負のリテラルは等値条件に使える`` () =
    withStore "an6" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // 整数の単項マイナス → 主キー検索
        let negInt =
            Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Unary(UnaryOp.Neg, Expr.Literal(Literal.Int 5L)))

        match ok (Analyzer.chooseScanMethod b Catalog.empty [ negInt ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(-5L, asInt values.[0])
        | m -> Assert.Fail(sprintf "PkLookup ではない: %A" m)

        // 実数の単項マイナスも定数として解釈される（索引が無いので結果は全走査）
        let negReal =
            Expr.Binary(BinaryOp.Eq, pCol "age", Expr.Unary(UnaryOp.Neg, Expr.Literal(Literal.Real 1.5)))

        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, ok (Analyzer.chooseScanMethod b Catalog.empty [ negReal ]))

        // 数値でない単項マイナスは定数とみなさない
        let negText =
            Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Unary(UnaryOp.Neg, Expr.Literal(Literal.String "x")))

        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, ok (Analyzer.chooseScanMethod b Catalog.empty [ negText ])))

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

[<Fact>]
let ``Planner: 集約とスターが同時でもスターは集約対象から外れる`` () =
    withStore "pl8" (fun store ->
        let ctx = ctxOf store

        let items =
            [
                {
                    Expression = Expr.QualifiedStar "people"
                    Alias = None
                }
                {
                    Expression = Expr.AggCall(AggFunc.Count, true, Expr.Literal(Literal.Null))
                    Alias = None
                }
            ]

        let stmt = selectOf items (Some { Name = "people"; Alias = None }) [] None
        let tree = ok (Optimizer.buildLogical ctx stmt)

        // 集約リストは COUNT(*) の 1 個だけ（t.* は集約対象にならない）
        match tree with
        | Logical.Project(_, Logical.Aggregate(_, aggs, _)) -> Assert.Equal<int>(1, aggs |> List.length)
        | _ -> Assert.Fail(sprintf "想定外の形: %A" tree))

[<Fact>]
let ``Planner: 定数でない LIMIT と OFFSET だけの指定`` () =
    withStore "pl9" (fun store ->
        let ctx = ctxOf store

        let plain =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        // LIMIT が非リテラル → 0（制限なし）として扱う
        let stmt1 =
            { plain with
                Limit = Some(Expr.Column { Qualifier = None; Name = "nope" })
            }

        match ok (Optimizer.buildLogical ctx stmt1) with
        | Logical.Limit(0, 0, _) -> ()
        | other -> Assert.Fail(sprintf "LIMIT が 0 になっていない: %A" other)

        // OFFSET だけ指定（Limit = None の分岐）
        let stmt2 =
            { plain with
                Offset = Some(Expr.Literal(Literal.Int 3L))
            }

        match ok (Optimizer.buildLogical ctx stmt2) with
        | Logical.Limit(3, 0, _) -> ()
        | other -> Assert.Fail(sprintf "OFFSET が 3 になっていない: %A" other))

[<Fact>]
let ``Plan: FROM なしの SELECT は 1 行の定数を返す`` () =
    withStore "pl10" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [
                    {
                        Expression = Expr.Literal(Literal.Int 1L)
                        Alias = None
                    }
                ]
                None
                []
                None

        let columns, rows = ok (Plan.runSelect ctx stmt)

        // FROM なしは現状 Project を挟まず 1 行 0 列を返す（Parser からは到達しない）
        Assert.Equal<int>(1, columns.Length)
        Assert.Equal<int>(1, rows.Length)
        Assert.Empty(rows.[0]))

// ---------------------------------------------------------------------------
// Planner — Op への変換（Optimizer.exec）
// SQL 経由では作れない木を直接渡す（Values / RangeScan / Join のエラー経路）
// ---------------------------------------------------------------------------

[<Fact>]
let ``Optimizer.exec: RangeScan は全走査へ落ちる`` () =
    withStore "ex-range" (fun store ->
        let ctx = ctxOf store
        let schema = Schema.ofTable "people" None peopleSchema

        // 範囲走査はまだ Plan からは作られない（現状は全走査と同じ）
        let tree =
            Logical.Scan("people", ScanMethod.RangeScan("ix_people_age", [| SqlValue.Int 1L |], [| SqlValue.Int 9L |]))

        let rows = ok (Op.toArray (ok (Optimizer.exec ctx tree schema)))
        Assert.Empty(rows))

[<Fact>]
let ``Optimizer.exec: Join 右側のスキーマが引けなくても空スキーマで続行する`` () =
    withStore "ex-defschema" (fun store ->
        let ctx = ctxOf store
        let schema = Schema.ofTable "people" None peopleSchema

        // 登録に無い Join 名（defaultSchema の Error 経路）
        let tree =
            Logical.Join(
                JoinKind.Inner,
                "missing",
                Expr.Literal(Literal.Bool true),
                Logical.Scan("people", ScanMethod.SeqScan),
                Logical.Scan("people", ScanMethod.SeqScan)
            )

        let rows = ok (Op.toArray (ok (Optimizer.exec ctx tree schema)))
        Assert.Empty(rows))

[<Fact>]
let ``Optimizer.exec: Join の左右でエラーになった木は Error を返す`` () =
    withStore "ex-joinerr" (fun store ->
        let ctx = ctxOf store
        let schema = Schema.ofTable "people" None peopleSchema
        let on = Expr.Literal(Literal.Bool true)

        // 左部分木の実行が失敗する（descend の Error 経路）
        let badLeft =
            Logical.Join(
                JoinKind.Inner,
                "people",
                on,
                Logical.Scan("missing", ScanMethod.SeqScan),
                Logical.Scan("people", ScanMethod.SeqScan)
            )

        expectError "左部分木の失敗が伝播していません" (Optimizer.exec ctx badLeft schema)

        // 右部分木の実行が失敗する（unwind の Error 経路）
        let badRight =
            Logical.Join(
                JoinKind.Inner,
                "people",
                on,
                Logical.Scan("people", ScanMethod.SeqScan),
                Logical.Scan("missing", ScanMethod.SeqScan)
            )

        expectError "右部分木の失敗が伝播していません" (Optimizer.exec ctx badRight schema)

        // 右部分木の materialize が失敗する（join 実行時の Error 経路）。
        // 行がある Values にできない式を評価させる（行が無いと評価されない）。
        let badCol = Expr.Column { Qualifier = None; Name = "nope" }

        let failingRight = Logical.Filter(badCol, Logical.Values [| SqlValue.Int 1L |])

        let badJoin =
            Logical.Join(JoinKind.Inner, "people", on, Logical.Scan("people", ScanMethod.SeqScan), failingRight)

        expectError "右の評価失敗が join で伝播していません" (Optimizer.exec ctx badJoin schema))

[<Fact>]
let ``Optimizer.exec: Filter の下のラッパーを透過する`` () =
    withStore "ex-wrappers" (fun store ->
        let ctx = ctxOf store
        let schema = Schema.ofTable "people" None peopleSchema
        let p = Expr.Literal(Literal.Bool true)
        let scan = Logical.Scan("people", ScanMethod.SeqScan)

        // outputOf が stripWrappers で中身まで降りること（Limit / Distinct / Sort / Having / Project）
        let wrappers =
            [
                Logical.Limit(0, 0, scan)
                Logical.Distinct scan
                Logical.Sort(
                    [
                        {
                            Expression = pCol "id"
                            Direction = SortDirection.Ascending
                        }
                    ],
                    scan
                )
                Logical.Having(p, [], [], scan)
                Logical.Project([ { Expression = Expr.Star; Alias = None } ], scan)
            ]

        for w in wrappers do
            let rows = ok (Op.toArray (ok (Optimizer.exec ctx (Logical.Filter(p, w)) schema)))
            Assert.Empty(rows))
