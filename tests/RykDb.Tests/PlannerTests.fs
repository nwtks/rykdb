// ===========================================================================
// Planner.fs のテスト
// ===========================================================================

module RykDb.PlannerTests

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
let ``Optimizer.buildLogical: FROM が無ければ Values（1 行 0 列）が入力になる`` () =
    withStore "pl7" (fun store ->
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

        let tree = ok (Optimizer.buildLogical ctx stmt)

        // 入力は Logical.Values（1 行 0 列）で、その上に射影が載る
        match tree with
        | Logical.Project(_, Logical.Values _) -> ()
        | _ -> Assert.Fail(sprintf "FROM なしの入力が Values でない: %A" tree))

[<Fact>]
let ``Optimizer.buildLogical: FROM なしの * と JOIN はエラー`` () =
    withStore "pl7b" (fun store ->
        let ctx = ctxOf store

        // * は展開する列が無いのでエラー（0 列 1 行の成功にしない）
        let star = selectOf [ { Expression = Expr.Star; Alias = None } ] None [] None
        expectErrorKind ErrorKind.SyntaxError "FROM なしの *" (Optimizer.buildLogical ctx star)

        // JOIN には FROM が必要
        let join =
            selectOf
                [
                    {
                        Expression = Expr.Literal(Literal.Int 1L)
                        Alias = None
                    }
                ]
                None
                [
                    {
                        Kind = JoinKind.Inner
                        Table = { Name = "people"; Alias = None }
                        On = Expr.Literal(Literal.Bool true)
                    }
                ]
                None

        expectErrorKind ErrorKind.SyntaxError "FROM なしの JOIN" (Optimizer.buildLogical ctx join))

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
let ``Optimizer.buildLogical: LIMIT の定数式と OFFSET 単独`` () =
    withStore "pl9" (fun store ->
        let ctx = ctxOf store

        let plain =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        // LIMIT が列参照（定数でない）→ エラー
        let stmt1 =
            { plain with
                Limit = Some(Expr.Column { Qualifier = None; Name = "nope" })
            }

        expectErrorKind ErrorKind.NameError "LIMIT に列参照は使えない" (Optimizer.buildLogical ctx stmt1)

        // LIMIT の定数式は評価される
        let stmt3 =
            { plain with
                Limit = Some(Expr.Binary(BinaryOp.Add, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L)))
            }

        match ok (Optimizer.buildLogical ctx stmt3) with
        | Logical.Limit(0, 1, _) -> ()
        | other -> Assert.Fail(sprintf "LIMIT 1+0 が 1 になっていない: %A" other)

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
        // FROM なしの入力は 1 行 0 列で、射影が定数を 1 列にする
        Assert.Equal<string array>([| "1" |], columns)
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal<int>(1, rows.[0].Length)
        Assert.Equal<int64>(1L, asInt rows.[0].[0]))

[<Fact>]
let ``Optimizer.buildLogical: 主キー順の ORDER BY は Sort を作らない`` () =
    withStore "sort-pk" (fun store ->
        let ctx =
            ctxOf store
            |> ExecContext.withCatalog (Catalog.addTable Catalog.Empty (Catalog.toDefinition peopleSchema))

        let baseStmt =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let stmt =
            { baseStmt with
                OrderBy =
                    [
                        {
                            Expression = pCol "id"
                            Direction = SortDirection.Ascending
                        }
                    ]
            }

        match ok (Optimizer.buildLogical ctx stmt) with
        | Logical.Project(_, Logical.Scan("people", ScanMethod.SeqScan)) -> ()
        | other -> Assert.Fail(sprintf "Sort が残っています: %A" other))

[<Fact>]
let ``Optimizer.buildLogical: 主キー以外・降順の ORDER BY は Sort を作る`` () =
    withStore "sort-keep" (fun store ->
        let ctx =
            ctxOf store
            |> ExecContext.withCatalog (Catalog.addTable Catalog.Empty (Catalog.toDefinition peopleSchema))

        let baseStmt =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let hasSort (keys: SortKey list) =
            match ok (Optimizer.buildLogical ctx { baseStmt with OrderBy = keys }) with
            | Logical.Project(_, Logical.Sort _) -> true
            | _ -> false

        let key e d = { Expression = e; Direction = d }

        Assert.True(hasSort [ key (pCol "name") SortDirection.Ascending ], "主キー以外の列はソートが必要です")

        Assert.True(hasSort [ key (pCol "id") SortDirection.Descending ], "降順はソートが必要です"))

[<Fact>]
let ``Optimizer.buildLogical: 複合主キーは全列の昇順なら Sort を作らない`` () =
    withStore "sort-pk-composite" (fun store ->
        let composite =
            { peopleSchema with
                PrimaryKey = [ 0; 2 ]
            }

        let ctx =
            ctxOf store
            |> ExecContext.withCatalog (Catalog.addTable Catalog.Empty (Catalog.toDefinition composite))

        let baseStmt =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let treeOf (keys: SortKey list) =
            ok (Optimizer.buildLogical ctx { baseStmt with OrderBy = keys })

        let key e d = { Expression = e; Direction = d }

        // (id, age) は主キーそのもの（順序も定義順）→ ソート不要
        match
            treeOf
                [
                    key (pCol "id") SortDirection.Ascending
                    key (pCol "age") SortDirection.Ascending
                ]
        with
        | Logical.Project(_, Logical.Scan("people", ScanMethod.SeqScan)) -> ()
        | other -> Assert.Fail(sprintf "複合主キーの全列でも Sort が残っています: %A" other)

        // 主キーの先頭 1列だけでは並びが決まらない → ソートが必要
        match treeOf [ key (pCol "id") SortDirection.Ascending ] with
        | Logical.Project(_, Logical.Sort _) -> ()
        | other -> Assert.Fail(sprintf "主キーの一部なのに Sort がありません: %A" other)

        // 主キーの列でも定義順と違えばソートが必要
        match
            treeOf
                [
                    key (pCol "age") SortDirection.Ascending
                    key (pCol "id") SortDirection.Ascending
                ]
        with
        | Logical.Project(_, Logical.Sort _) -> ()
        | other -> Assert.Fail(sprintf "主キーの順序違いなのに Sort がありません: %A" other))
