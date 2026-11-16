// ===========================================================================
// Optimizer.fs のテスト（論理代数の実行と EXPLAIN）
// ===========================================================================

module RykDb.OptimizerTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テスト用の ExecContext（people と orders を登録済み）。
let private ctxOf (store: RowStore) =
    ExecContext.create store None
    |> ExecContext.withSchema "people" peopleSchema
    |> ExecContext.withSchema "orders" ordersSchema

let private pCol (n: string) =
    Expr.Column { Qualifier = Some "people"; Name = n }

let private oCol (n: string) =
    Expr.Column { Qualifier = Some "orders"; Name = n }

/// テスト用の文を作る。
let private selectOf (items: SelectItem list) (from: TableRef option) (joins: JoinClause list) (w: Expr option) =
    {
        Distinct = false
        Items = items
        From = Option.map TableSource.Named from
        Joins = joins
        Where = w
        GroupBy = []
        Having = None
        OrderBy = []
        Limit = None
        Offset = None
        SetOperations = []
    }

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
let ``Optimizer.explain: 論理木を 1行 1ノードで表示する`` () =
    withStore "pl-explain" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                []
                (Some(Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))))

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        Assert.Equal<string list>(
            [ "Project(*)"; "  Filter((people.age > 30))"; "    Scan(people, SeqScan)" ],
            Optimizer.explain ctx tree Schema.empty
        ))

[<Fact>]
let ``Optimizer.explain: 結合アルゴリズムも表示する`` () =
    withStore "pl-explain-join" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                [
                    {
                        Kind = JoinKind.Inner
                        Table = TableSource.Named { Name = "orders"; Alias = None }
                        On = Expr.Binary(BinaryOp.Eq, pCol "id", oCol "person_id")
                    }
                ]
                None

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        Assert.Equal<string list>(
            [
                "Project(*)"
                "  Join(INNER, HashJoin, right=orders, on=(people.id = orders.person_id))"
                "    Scan(people, SeqScan)"
                "    Scan(orders, SeqScan)"
            ],
            Optimizer.explain ctx tree Schema.empty
        ))
