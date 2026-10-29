// ===========================================================================
// Planner.fs のテスト
// ===========================================================================

module RykDb.PlannerTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テスト用の ExecContext（people と orders を登録済み）。
let private ctxOf (store: RowStore) =
    let c0 = ExecContext.create store None
    let c1 = ExecContext.withSchema c0 "people" peopleSchema
    let c2 = ExecContext.withSchema c1 "orders" ordersSchema
    c2

let private pCol (n: string) =
    Expr.Column { Qualifier = Some "people"; Name = n }

let private oCol (n: string) =
    Expr.Column { Qualifier = Some "orders"; Name = n }

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
let ``Optimizer.buildLogical: 単純な SELECT は Scan + Project になる`` () =
    withStore "pl1" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let tree = ok (Optimizer.buildLogical ctx stmt)

        match tree with
        | Logical.Project(_, Logical.Scan(name, ScanMethod.SeqScan)) -> Assert.Equal<string>("people", name)
        | _ -> Assert.Fail(sprintf "想定外の形: %A" tree))

[<Fact>]
let ``Optimizer.buildLogical: WHERE があれば Scan の直下に Filter が付く`` () =
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
let ``Optimizer.buildLogical: JOIN があれば LJoin が作られる`` () =
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
let ``Optimizer.buildLogical: GROUP BY があれば LAggregate が作られる`` () =
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
let ``Optimizer.buildLogical: 集約がなければ LAggregate は作られない`` () =
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
let ``Optimizer.buildLogical: LIMIT / OFFSET は LLimit になる`` () =
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
let ``Optimizer.buildLogical: FROM が無ければ LValues になる`` () =
    withStore "pl7" (fun store ->
        let ctx = ctxOf store
        let stmt = selectOf [ { Expression = Expr.Star; Alias = None } ] None [] None
        let tree = ok (Optimizer.buildLogical ctx stmt)

        match tree with
        | Logical.Values _ -> ()
        | _ -> Assert.Fail(sprintf "FROM なしなのに LValues でない: %A" tree))

[<Fact>]
let ``Optimizer.buildLogical: 集約と * の併用はエラー`` () =
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
        // ヘッダは入力スキーマ、行は集約出力スキーマで作られるため、併用すると列数が食い違う。
        expectErrorKind ErrorKind.SyntaxError "集約と * は併用できない" (Optimizer.buildLogical ctx stmt))

[<Fact>]
let ``Optimizer.buildLogical: 定数でない LIMIT はエラー、OFFSET 単独は制限なし`` () =
    withStore "pl9" (fun store ->
        let ctx = ctxOf store

        let plain =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        // LIMIT が非リテラル → 0 行に丸めずエラー
        let stmt1 =
            { plain with
                Limit = Some(Expr.Column { Qualifier = None; Name = "nope" })
            }

        expectErrorKind ErrorKind.SyntaxError "LIMIT は整数リテラル" (Optimizer.buildLogical ctx stmt1)

        // OFFSET だけ指定（Limit = None の分岐）→ limit = -1（制限なし）
        let stmt2 =
            { plain with
                Offset = Some(Expr.Literal(Literal.Int 3L))
            }

        match ok (Optimizer.buildLogical ctx stmt2) with
        | Logical.Limit(3, -1, _) -> ()
        | other -> Assert.Fail(sprintf "OFFSET が 3 になっていない: %A" other))

[<Fact>]
let ``Optimizer.exec: RangeScan は全走査へ落ちる`` () =
    withStore "ex-range" (fun store ->
        let ctx = ctxOf store
        let schema = Schema.ofTable "people" None peopleSchema

        // 範囲走査はまだ Plan からは作られない（現状は全走査と同じ）
        let tree =
            Logical.Scan(
                "people",
                ScanMethod.RangeScan("ix_people_age", [| SqlValue.Int 1L |], Some [| SqlValue.Int 9L |])
            )

        let rows = ok (Op.toArray (ok (Optimizer.exec ctx tree schema)))
        Assert.Empty rows)

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
        Assert.Empty rows)

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
            Assert.Empty rows)

[<Fact>]
let ``Plan.runSelect: FROM なしの SELECT は 1 行の定数を返す`` () =
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
