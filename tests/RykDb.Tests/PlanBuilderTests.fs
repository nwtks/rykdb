// ===========================================================================
// PlanBuilder.fs のテスト（論理代数の組み立て）
// ===========================================================================

module RykDb.PlanBuilderTests

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
    }

/// xtra を登録した文脈（people / orders に加えて 3つ目のテーブル）。
let private ctxWithExtra (store: RowStore) =
    ctxOf store |> ExecContext.withSchema "xtra" peopleSchema

let private xCol (n: string) =
    Expr.Column { Qualifier = Some "xtra"; Name = n }

let private eq a b = Expr.Binary(BinaryOp.Eq, a, b)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: 単純な SELECT は Scan + Project になる`` () =
    withStore "pl1" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        match tree with
        | Logical.Project(_, Logical.Scan(name, ScanMethod.SeqScan)) -> Assert.Equal<string>("people", name)
        | _ -> failActual "想定外の形" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: WHERE があれば Scan の直下に Filter が付く`` () =
    withStore "pl2" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                []
                (Some(Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))))

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        match tree with
        | Logical.Project(_, Logical.Filter(_, Logical.Scan(name, method))) ->
            Assert.Equal<string>("people", name)
            // age には索引があるので索引検索のはず
            match method with
            | ScanMethod.SeqScan -> () // カタログに索引が無いので全走査
            | _ -> ()
        | _ -> failActual "想定外の形" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: JOIN があれば Logical.Join が作られる`` () =
    withStore "pl3" (fun store ->
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

        // Logical.Join の第 2 引数は右テーブル名、第 4 引数は左の木。
        match tree with
        | Logical.Project(_, Logical.Join(JoinKind.Inner, rightName, _, Logical.Scan(leftName, _), _)) ->
            Assert.Equal<string>("people", leftName)
            Assert.Equal<string>("orders", rightName)
        | _ -> failActual "想定外の形" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: FROM のサブクエリは Logical.Derived になる`` () =
    withStore "pl-derived" (fun store ->
        let ctx = ctxOf store

        let plan =
            {
                Output =
                    {
                        Columns =
                            [|
                                {
                                    Name = "id"
                                    Qualifier = Some "x"
                                    DataType = Some DataType.Integer
                                }
                            |]
                        Tables = [ "x" ]
                    }
                Header = Schema.empty
                Input = Schema.ofTable "people" None peopleSchema
                Context = ctx
                Tree = Logical.Scan("people", ScanMethod.SeqScan)
            }

        let stmt =
            { selectOf [ { Expression = Expr.Star; Alias = None } ] None [] None with
                From = Some(TableSource.Named { Name = "x"; Alias = None })
            }

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt (Map.ofList [ ("x", plan) ]))

        match tree with
        | Logical.Project(_, Logical.Derived p) ->
            Assert.Equal<string>("id", p.Output.Columns.[0].Name)
            Assert.Equal<string option>(Some "x", p.Output.Columns.[0].Qualifier)
        | other -> failActual "Derived ではない" other)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: GROUP BY があれば Logical.Aggregate が作られる`` () =
    withStore "pl4" (fun store ->
        let ctx = ctxOf store

        let items =
            [
                {
                    Expression = pCol "age"
                    Alias = None
                }
                {
                    Expression = Expr.AggCall(AggFunc.Count, true, Expr.Literal Literal.Null)
                    Alias = None
                }
            ]

        let plain = selectOf items (Some { Name = "people"; Alias = None }) [] None
        let stmt = { plain with GroupBy = [ pCol "age" ] }

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        match tree with
        | Logical.Project(_, Logical.Aggregate(groupBy, aggs, _)) ->
            Assert.Equal<int>(1, groupBy.Length)
            Assert.Equal<int>(1, aggs |> List.length)
        | _ -> failActual "想定外の形" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: 集約がなければ Logical.Aggregate は作られない`` () =
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

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        match tree with
        | Logical.Project(_, Logical.Scan(_, _)) -> ()
        | _ -> failActual "集約が無いのに集約がある" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: LIMIT / OFFSET は Logical.Limit になる`` () =
    withStore "pl6" (fun store ->
        let ctx = ctxOf store

        let plain =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let stmt =
            { plain with
                Limit = Some(Expr.Literal(Literal.Int 10L))
                Offset = Some(Expr.Literal(Literal.Int 5L))
            }

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        // 評価順は「射影 → DISTINCT → ORDER BY → LIMIT」なので LIMIT は
        // 射影の外側に出る。DISTINCT を先に置くと (id, g) の組で重複排除される。
        match tree with
        | Logical.Limit(offset, limit, Logical.Project(_, _)) ->
            Assert.Equal<int>(5, offset)
            Assert.Equal<int>(10, limit)
        | _ -> failActual "LIMIT が木の中心でない" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: FROM が無ければ Values（1 行 0 列）が入力になる`` () =
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

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        // 入力は Logical.Values（1 行 0 列）で、その上に射影が載る
        match tree with
        | Logical.Project(_, Logical.Values _) -> ()
        | _ -> failActual "FROM なしの入力が Values でない" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: LIMIT の定数式と OFFSET 単独`` () =
    withStore "pl9" (fun store ->
        let ctx = ctxOf store

        let plain =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        // LIMIT が列参照（定数でない）→ エラー
        let stmt1 =
            { plain with
                Limit = Some(Expr.Column { Qualifier = None; Name = "nope" })
            }

        expectErrorKind ErrorKind.NameError "LIMIT に列参照は使えない" (PlanBuilder.buildLogicalWith ctx stmt1 Map.empty)

        // LIMIT の定数式は評価される
        let stmt3 =
            { plain with
                Limit = Some(Expr.Binary(BinaryOp.Add, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L)))
            }

        match ok (PlanBuilder.buildLogicalWith ctx stmt3 Map.empty) with
        | Logical.Limit(0, 1, _) -> ()
        | other -> failActual "LIMIT 1+0 が 1 になっていない" other

        // OFFSET だけ指定（Limit = None の分岐）→ limit = -1（制限なし）
        let stmt2 =
            { plain with
                Offset = Some(Expr.Literal(Literal.Int 3L))
            }

        match ok (PlanBuilder.buildLogicalWith ctx stmt2 Map.empty) with
        | Logical.Limit(3, -1, _) -> ()
        | other -> failActual "OFFSET が 3 になっていない" other)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: 主キー順の ORDER BY は Sort を作らない`` () =
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

        match ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty) with
        | Logical.Project(_, Logical.Scan("people", ScanMethod.SeqScan)) -> ()
        | other -> failActual "Sort が残っています" other)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: 主キー以外・降順の ORDER BY は Sort を作る`` () =
    withStore "sort-keep" (fun store ->
        let ctx =
            ctxOf store
            |> ExecContext.withCatalog (Catalog.addTable Catalog.Empty (Catalog.toDefinition peopleSchema))

        let baseStmt =
            selectOf [ { Expression = Expr.Star; Alias = None } ] (Some { Name = "people"; Alias = None }) [] None

        let hasSort (keys: SortKey list) =
            match ok (PlanBuilder.buildLogicalWith ctx { baseStmt with OrderBy = keys } Map.empty) with
            | Logical.Project(_, Logical.Sort _) -> true
            | _ -> false

        let key e d = { Expression = e; Direction = d }

        Assert.True(hasSort [ key (pCol "name") SortDirection.Ascending ], "主キー以外の列はソートが必要です")

        Assert.True(hasSort [ key (pCol "id") SortDirection.Descending ], "降順はソートが必要です"))

[<Fact>]
let ``PlanBuilder.buildLogicalWith: 複合主キーは全列の昇順なら Sort を作らない`` () =
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
            ok (PlanBuilder.buildLogicalWith ctx { baseStmt with OrderBy = keys } Map.empty)

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
        | other -> failActual "複合主キーの全列でも Sort が残っています" other

        // 主キーの先頭 1列だけでは並びが決まらない → ソートが必要
        match treeOf [ key (pCol "id") SortDirection.Ascending ] with
        | Logical.Project(_, Logical.Sort _) -> ()
        | other -> failActual "主キーの一部なのに Sort がありません" other

        // 主キーの列でも定義順と違えばソートが必要
        match
            treeOf
                [
                    key (pCol "age") SortDirection.Ascending
                    key (pCol "id") SortDirection.Ascending
                ]
        with
        | Logical.Project(_, Logical.Sort _) -> ()
        | other -> failActual "主キーの順序違いなのに Sort がありません" other)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: FROM なしの * と JOIN はエラー`` () =
    withStore "pl7b" (fun store ->
        let ctx = ctxOf store

        // * は展開する列が無いのでエラー（0 列 1 行の成功にしない）
        let star = selectOf [ { Expression = Expr.Star; Alias = None } ] None [] None
        expectErrorKind ErrorKind.SyntaxError "FROM なしの *" (PlanBuilder.buildLogicalWith ctx star Map.empty)

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
                        Table = TableSource.Named { Name = "people"; Alias = None }
                        On = Expr.Literal(Literal.Bool true)
                    }
                ]
                None

        expectErrorKind ErrorKind.SyntaxError "FROM なしの JOIN" (PlanBuilder.buildLogicalWith ctx join Map.empty))

[<Fact>]
let ``PlanBuilder.buildLogicalWith: 集約と * の併用はエラー`` () =
    withStore "pl8" (fun store ->
        let ctx = ctxOf store

        let items =
            [
                {
                    Expression = Expr.QualifiedStar "people"
                    Alias = None
                }
                {
                    Expression = Expr.AggCall(AggFunc.Count, true, Expr.Literal Literal.Null)
                    Alias = None
                }
            ]

        let stmt = selectOf items (Some { Name = "people"; Alias = None }) [] None
        // ヘッダは入力スキーマ、行は集約出力スキーマで作られるため、併用すると列数が食い違う。
        expectErrorKind ErrorKind.SyntaxError "集約と * は併用できない" (PlanBuilder.buildLogicalWith ctx stmt Map.empty))

[<Fact>]
let ``PlanBuilder.buildLogicalWith: WHERE の結合述語は INNER の ON へ昇格する`` () =
    withStore "pl-promote" (fun store ->
        let ctx = ctxOf store

        // ON には何も書かず、結合キーを WHERE に書く
        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                [
                    {
                        Kind = JoinKind.Inner
                        Table = TableSource.Named { Name = "orders"; Alias = None }
                        On = Expr.Literal(Literal.Bool true)
                    }
                ]
                (Some(eq (pCol "id") (oCol "person_id")))

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        // 結合の上に Filter が残らず、ON に述語が入る
        match tree with
        | Logical.Project(_, Logical.Join(JoinKind.Inner, "orders", on, _, _)) ->
            Assert.Contains("person_id", Expr.toDisplayString on)
        | _ -> failActual "想定外の形" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: INNER の結合順は選択性の高いテーブルを先に積む`` () =
    withStore "pl-order" (fun store ->
        let ctx = ctxWithExtra store

        // orders は全走査、xtra は主キー等値（1行）なので xtra を先に結合する
        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                [
                    {
                        Kind = JoinKind.Inner
                        Table = TableSource.Named { Name = "orders"; Alias = None }
                        On = eq (pCol "id") (oCol "person_id")
                    }
                    {
                        Kind = JoinKind.Inner
                        Table = TableSource.Named { Name = "xtra"; Alias = None }
                        On = eq (pCol "id") (xCol "id")
                    }
                ]
                (Some(eq (xCol "id") (Expr.Literal(Literal.Int 1L))))

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        match tree with
        | Logical.Project(_, Logical.Join(JoinKind.Inner, right, _, left, _)) ->
            // 外側（最後）が orders、内側に xtra が入る
            Assert.Equal<string>("orders", right)

            match left with
            | Logical.Join(JoinKind.Inner, inner, _, Logical.Scan(first, _), _) ->
                Assert.Equal<string>("xtra", inner)
                Assert.Equal<string>("people", first)
            | _ -> failActual "想定外の左部分木" left
        | _ -> failActual "想定外の形" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: LEFT JOIN は結合順の境界になる`` () =
    withStore "pl-left-order" (fun store ->
        let ctx = ctxWithExtra store

        // LEFT の右（orders）は動かさず、後続の INNER（xtra）だけを積む
        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                [
                    {
                        Kind = JoinKind.Left
                        Table = TableSource.Named { Name = "orders"; Alias = None }
                        On = eq (pCol "id") (oCol "person_id")
                    }
                    {
                        Kind = JoinKind.Inner
                        Table = TableSource.Named { Name = "xtra"; Alias = None }
                        On = eq (pCol "id") (xCol "id")
                    }
                ]
                (Some(eq (xCol "id") (Expr.Literal(Literal.Int 1L))))

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        match tree with
        | Logical.Project(_, Logical.Join(JoinKind.Inner, "xtra", _, left, _)) ->
            match left with
            | Logical.Join(JoinKind.Left, "orders", _, Logical.Scan(first, _), _) ->
                Assert.Equal<string>("people", first)
            | _ -> failActual "想定外の左部分木" left
        | _ -> failActual "想定外の形" tree)

[<Fact>]
let ``PlanBuilder.buildLogicalWith: INNER の後に続く LEFT JOIN も積まれる`` () =
    withStore "pl-inner-then-left" (fun store ->
        let ctx = ctxWithExtra store

        // INNER 区間のあとに LEFT が続く。区間の処理で残りを落とすと
        // city 相当のテーブルが計画から消え、列が見つからなくなる。
        let stmt =
            selectOf
                [ { Expression = Expr.Star; Alias = None } ]
                (Some { Name = "people"; Alias = None })
                [
                    {
                        Kind = JoinKind.Inner
                        Table = TableSource.Named { Name = "orders"; Alias = None }
                        On = eq (pCol "id") (oCol "person_id")
                    }
                    {
                        Kind = JoinKind.Left
                        Table = TableSource.Named { Name = "xtra"; Alias = None }
                        On = eq (pCol "id") (xCol "id")
                    }
                ]
                None

        let tree = ok (PlanBuilder.buildLogicalWith ctx stmt Map.empty)

        match tree with
        | Logical.Project(_, Logical.Join(JoinKind.Left, "xtra", _, Logical.Join(JoinKind.Inner, "orders", _, _, _), _)) ->
            ()
        | _ -> failActual "想定外の形" tree)
