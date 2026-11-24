// ===========================================================================
// Optimizer.fs のテスト（論理代数の実行と EXPLAIN）
// ===========================================================================

module RykDb.OptimizerTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Optimizer.exec: 右が結合の部分木（bushy）でも実行できる`` () =
    withSession "bushy" (fun db ->
        let db = exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE c (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "INSERT INTO a VALUES (1,10),(2,20)"
        let db = exec db "INSERT INTO b VALUES (1,10),(2,20)"
        let db = exec db "INSERT INTO c VALUES (1,10),(2,20),(3,30)"

        let stmt =
            match parseOneOk "SELECT a.id FROM a JOIN b ON a.k = b.k JOIN c ON b.k = c.k" with
            | Statement.Select s -> s
            | other -> failwithf "SELECT ではありません: %A" other

        let plan = ok (db.PrepareSelect stmt)
        let scan name = Logical.Scan(name, ScanMethod.SeqScan)

        let col q n =
            Expr.Column { Qualifier = Some q; Name = n }

        let eq x y = Expr.Binary(BinaryOp.Eq, x, y)

        // a ⋈ (b ⋈ c) の bushy 木（右が結合の部分木）。
        let innerTree =
            Logical.Join(JoinKind.Inner, "c", eq (col "b" "k") (col "c" "k"), scan "b", scan "c")

        let bushy =
            Logical.Join(JoinKind.Inner, "", eq (col "a" "k") (col "b" "k"), scan "a", innerTree)

        let tree =
            Logical.Project(
                [
                    {
                        Expression = col "a" "id"
                        Alias = None
                    }
                ],
                bushy
            )

        let rows =
            try
                let op = ok (Optimizer.exec plan.Context tree plan.Input)
                ok (Op.toArray op) |> Array.map (fun row -> asInt row.[0])
            finally
                plan.Context.Scope.DisposeAll()

        // 左深の SQL 実行と同じ結果になる。
        Assert.Equal<int64 list>([ 1L; 2L ], rows |> Array.sort |> Array.toList))

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
            Optimizer.explain ctx tree Schema.Empty
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
            Optimizer.explain ctx tree Schema.Empty
        ))

[<Fact>]
let ``末尾再帰: 50000段の論理木でも列マスクを求められる`` () =
    withStore "ex-deep-needs" (fun store ->
        let ctx = ctxOf store
        let p = Expr.Literal(Literal.Bool true)

        let tree =
            List.fold (fun child _ -> Logical.Filter(p, child)) (Logical.Values [| SqlValue.Int 1L |]) [ 1..50000 ]

        // scanNeedsMasks が明示スタックで深い木をたどること。
        // 行の取り出しは Op の深さに比例するため行わず、演算子の組み立てまでを確かめる。
        ok (Optimizer.exec ctx tree Schema.Empty) |> ignore)
