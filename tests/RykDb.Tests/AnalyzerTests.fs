// ===========================================================================
// Analyzer.fs のテスト（走査方法の選択・結合アルゴリズム）
// ===========================================================================

module RykDb.AnalyzerTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Analyzer.chooseJoinAlgorithm: 左が小さく右に索引があれば IndexNestedLoop`` () =
    let leftTable =
        {
            Name = "l"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes = []
        }

    let rightTable =
        {
            Name = "r"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes =
                [
                    {
                        Name = "ix_r_a"
                        Columns = [ "a" ]
                        Unique = false
                    }
                ]
        }

    let leftSchema = Schema.ofTable "l" None leftTable
    let rightSchema = Schema.ofTable "r" None rightTable

    let on =
        Expr.Binary(
            BinaryOp.Eq,
            Expr.Column { Qualifier = Some "l"; Name = "a" },
            Expr.Column { Qualifier = Some "r"; Name = "a" }
        )

    let leftSmall = Logical.Limit(0, 1, Logical.Scan("l", ScanMethod.SeqScan))
    let rightScan = Logical.Scan("r", ScanMethod.SeqScan)

    let tableOf (name: string) =
        match name with
        | "l" -> Some leftTable
        | "r" -> Some rightTable
        | _ -> None

    match Analyzer.chooseJoinAlgorithm tableOf leftSchema rightSchema (Some rightTable) leftSmall rightScan on with
    | JoinAlgorithm.IndexNestedLoop name, _, Some lefts ->
        Assert.Equal<string>("ix_r_a", name)
        Assert.Equal<int array>([| 0 |], lefts)
    | other -> failActual "IndexNestedLoop ではありません" other

    // 左が大きい（Limit なし）なら、両方が主キー `a` の走査なのでマージ結合
    match
        Analyzer.chooseJoinAlgorithm
            tableOf
            leftSchema
            rightSchema
            (Some rightTable)
            (Logical.Scan("l", ScanMethod.SeqScan))
            rightScan
            on
    with
    | JoinAlgorithm.MergeJoin, Some keys, _ ->
        Assert.Equal<int array>([| 0 |], keys.LeftIndices)
        Assert.Equal<int array>([| 0 |], keys.RightIndices)
    | other -> failActual "MergeJoin ではありません" other

    // 非等値なら入れ子ループ
    let nonEqui =
        Expr.Binary(
            BinaryOp.Gt,
            Expr.Column { Qualifier = Some "l"; Name = "a" },
            Expr.Column { Qualifier = Some "r"; Name = "a" }
        )

    match Analyzer.chooseJoinAlgorithm tableOf leftSchema rightSchema (Some rightTable) leftSmall rightScan nonEqui with
    | JoinAlgorithm.NestedLoop, _, _ -> ()
    | other -> failActual "NestedLoop ではありません" other

[<Fact>]
let ``Analyzer.chooseJoinAlgorithm: 結合キーが走査順の先頭と一致するときだけマージ結合`` () =
    let columns = [ col "a" DataType.Integer; col "b" DataType.Integer ]

    let leftTable =
        {
            Name = "l"
            Columns = columns
            PrimaryKey = [ 0; 1 ]
            Indexes = []
        }

    let rightTable =
        {
            Name = "r"
            Columns = columns
            PrimaryKey = [ 0; 1 ]
            Indexes = []
        }

    let leftSchema = Schema.ofTable "l" None leftTable
    let rightSchema = Schema.ofTable "r" None rightTable

    let tableOf (name: string) =
        match name with
        | "l" -> Some leftTable
        | "r" -> Some rightTable
        | _ -> None

    let leftTree = Logical.Scan("l", ScanMethod.SeqScan)
    let rightTree = Logical.Scan("r", ScanMethod.SeqScan)

    let eq side column otherSide otherColumn =
        Expr.Binary(
            BinaryOp.Eq,
            Expr.Column { Qualifier = Some side; Name = column },
            Expr.Column
                {
                    Qualifier = Some otherSide
                    Name = otherColumn
                }
        )

    // ON の順が主キーと逆でも、走査順 (a, b) に並べ替えてマージ結合にする
    let onReversed = Expr.Binary(BinaryOp.And, eq "l" "b" "r" "b", eq "l" "a" "r" "a")

    match
        Analyzer.chooseJoinAlgorithm tableOf leftSchema rightSchema (Some rightTable) leftTree rightTree onReversed
    with
    | JoinAlgorithm.MergeJoin, Some keys, _ ->
        Assert.Equal<int array>([| 0; 1 |], keys.LeftIndices)
        Assert.Equal<int array>([| 0; 1 |], keys.RightIndices)
    | other -> failActual "MergeJoin ではありません" other

    // 主キーの 2 列目だけの等値は走査順の先頭と一致しないのでハッシュ結合
    match
        Analyzer.chooseJoinAlgorithm
            tableOf
            leftSchema
            rightSchema
            (Some rightTable)
            leftTree
            rightTree
            (eq "l" "b" "r" "b")
    with
    | JoinAlgorithm.HashJoin, _, _ -> ()
    | other -> failActual "HashJoin ではありません" other

    // 右の主キー順が (b, a) だとキー順が揃わないのでハッシュ結合
    let rightReversed =
        {
            Name = "r"
            Columns = columns
            PrimaryKey = [ 1; 0 ]
            Indexes = []
        }

    let planReversed =
        Analyzer.chooseJoinAlgorithm
            (fun n -> if n = "r" then Some rightReversed else Some leftTable)
            leftSchema
            rightSchema
            (Some rightReversed)
            leftTree
            rightTree
            (Expr.Binary(BinaryOp.And, eq "l" "a" "r" "a", eq "l" "b" "r" "b"))

    match planReversed with
    | JoinAlgorithm.HashJoin, _, _ -> ()
    | other -> failActual "HashJoin ではありません" other

[<Fact>]
let ``Analyzer.chooseJoinAlgorithm: テーブルスキーマが無い右は IndexNestedLoop にしない`` () =
    let leftTable =
        {
            Name = "l"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes = []
        }

    let rightTable =
        {
            Name = "r"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes =
                [
                    {
                        Name = "ix_r_a"
                        Columns = [ "a" ]
                        Unique = false
                    }
                ]
        }

    let leftSchema = Schema.ofTable "l" None leftTable
    let rightSchema = Schema.ofTable "r" None rightTable

    let on =
        Expr.Binary(
            BinaryOp.Eq,
            Expr.Column { Qualifier = Some "l"; Name = "a" },
            Expr.Column { Qualifier = Some "r"; Name = "a" }
        )

    let leftSmall = Logical.Limit(0, 1, Logical.Scan("l", ScanMethod.SeqScan))
    let rightScan = Logical.Scan("r", ScanMethod.SeqScan)

    let tableOf (name: string) =
        match name with
        | "l" -> Some leftTable
        | "r" -> Some rightTable
        | _ -> None

    // 派生テーブルはカタログの索引を持たない（rightTable = None）。
    // 索引が使える形でもインデックス入れ子ループにはしない。
    match Analyzer.chooseJoinAlgorithm tableOf leftSchema rightSchema None leftSmall rightScan on with
    | JoinAlgorithm.IndexNestedLoop _, _, _ -> Assert.Fail "IndexNestedLoop を選んではいけない"
    | _ -> ()

[<Fact>]
let ``Analyzer.chooseScanMethod: 等価条件があれば主キー検索にする`` () =
    withStore "an1" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })
        let pred = Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 1L))
        let m = ok (Analyzer.chooseScanMethod b Catalog.Empty [ pred ])

        match m with
        | ScanMethod.PkLookup values ->
            Assert.Equal<int>(1, values.Length)
            Assert.Equal<int64>(1L, asInt values.[0])
        | _ -> failActual "主キー検索になっていません" m)

[<Fact>]
let ``Analyzer.chooseScanMethod: 索引があれば索引検索にする`` () =
    withStore "an3" (fun store ->
        let ix = indexDefinition "people" peopleSchema.Indexes.[0]
        let catalog = ix |> Catalog.addIndex Catalog.Empty
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })
        let pred = Expr.Binary(BinaryOp.Eq, pCol "age", Expr.Literal(Literal.Int 30L))
        let m = ok (Analyzer.chooseScanMethod b catalog [ pred ])

        match m with
        | ScanMethod.IndexLookup(name, values) ->
            Assert.Equal<string>("ix_people_age", name)
            Assert.Equal<int64>(30L, asInt values.[0])
        | _ -> failActual "索引検索になっていません" m)

[<Fact>]
let ``Analyzer.chooseScanMethod: AND の奥と左右対称の等価条件を探せる`` () =
    withStore "an5" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })
        // 最初の conjunct は等価ではない → 2 番目が採用される
        let first = Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))
        let second = Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 7L))

        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ first; second ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(7L, asInt values.[0])
        | m -> failActual "PkLookup ではない" m

        // 値 = 列 の順（左右対称）でも探せる
        let reversed = Expr.Binary(BinaryOp.Eq, Expr.Literal(Literal.Int 8L), pCol "id")

        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ reversed ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(8L, asInt values.[0])
        | m -> failActual "PkLookup ではない" m)

[<Fact>]
let ``Analyzer.chooseScanMethod: 1 つの conjunct 内の AND も掘れる`` () =
    withStore "an7" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // 左の枝で見つかる（search の Some r 経路）
        let leftHit =
            Expr.Binary(
                BinaryOp.And,
                Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 9L)),
                Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))
            )

        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ leftHit ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(9L, asInt values.[0])
        | m -> failActual "PkLookup ではない" m

        // 左の枝では見つからず右で見つかる（search の None → 右の経路）
        let rightHit =
            Expr.Binary(
                BinaryOp.And,
                Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L)),
                Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 10L))
            )

        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ rightHit ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(10L, asInt values.[0])
        | m -> failActual "PkLookup ではない" m)

[<Fact>]
let ``Analyzer.chooseScanMethod: 負のリテラルは等値条件に使える`` () =
    withStore "an6" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // 整数の単項マイナス → 主キー検索
        let negInt =
            Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Unary(UnaryOp.Neg, Expr.Literal(Literal.Int 5L)))

        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ negInt ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(-5L, asInt values.[0])
        | m -> failActual "PkLookup ではない" m

        // 実数の単項マイナスも定数として解釈される（索引が無いので結果は全走査）
        let negReal =
            Expr.Binary(BinaryOp.Eq, pCol "age", Expr.Unary(UnaryOp.Neg, Expr.Literal(Literal.Real 1.5)))

        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, ok (Analyzer.chooseScanMethod b Catalog.Empty [ negReal ]))

        // 数値でない単項マイナスは定数とみなさない
        let negText =
            Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Unary(UnaryOp.Neg, Expr.Literal(Literal.String "x")))

        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, ok (Analyzer.chooseScanMethod b Catalog.Empty [ negText ])))

[<Fact>]
let ``Analyzer.chooseScanMethod: 複合主キーは全列一致のときだけ PkLookup`` () =
    withStore "an-comp" (fun store ->
        let schema =
            {
                Name = "pairs"
                Columns =
                    [
                        pkCol "a" DataType.Integer
                        pkCol "b" DataType.Integer
                        col "v" DataType.Text
                    ]
                PrimaryKey = [ 0; 1 ]
                Indexes = []
            }

        let ctx = ExecContext.create store None |> ExecContext.withSchema "pairs" schema
        let b = ok (Analyzer.bindTable ctx { Name = "pairs"; Alias = None })

        let eq name value =
            Expr.Binary(
                BinaryOp.Eq,
                Expr.Column
                    {
                        Qualifier = Some "pairs"
                        Name = name
                    },
                Expr.Literal value
            )

        // 列の順序が逆でも主キー順に組み立てる
        let preds = [ eq "b" (Literal.Int 2L); eq "a" (Literal.Int 1L) ]

        match ok (Analyzer.chooseScanMethod b Catalog.Empty preds) with
        | ScanMethod.PkLookup values ->
            Assert.Equal<int>(2, values.Length)
            Assert.Equal<int64>(1L, asInt values.[0])
            Assert.Equal<int64>(2L, asInt values.[1])
        | m -> failActual "PkLookup ではない" m

        // 部分一致は全走査
        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ eq "a" (Literal.Int 1L) ]) with
        | ScanMethod.SeqScan -> ()
        | m -> failActual "SeqScan ではない" m

        // キー列の型と違うリテラルは全走査（比較時に型エラーにする）
        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ eq "a" (Literal.String "1") ]) with
        | ScanMethod.SeqScan -> ()
        | m -> failActual "SeqScan ではない" m)

[<Fact>]
let ``Analyzer.chooseScanMethod: 複合索引は前置等値 + 範囲で IndexRangeScan`` () =
    withStore "an-comp-range" (fun store ->
        let schema =
            {
                Name = "pairs"
                Columns = [ col "a" DataType.Integer; col "b" DataType.Integer; col "v" DataType.Text ]
                PrimaryKey = []
                Indexes =
                    [
                        {
                            Name = "ix_pair"
                            Columns = [ "a"; "b" ]
                            Unique = false
                        }
                    ]
            }

        let ctx = ExecContext.create store None |> ExecContext.withSchema "pairs" schema
        let b = ok (Analyzer.bindTable ctx { Name = "pairs"; Alias = None })
        let ix = indexDefinition "pairs" schema.Indexes.[0]
        let catalog = ix |> Catalog.addIndex Catalog.Empty

        let colRef name =
            Expr.Column
                {
                    Qualifier = Some "pairs"
                    Name = name
                }

        let preds =
            [
                Expr.Binary(BinaryOp.Eq, colRef "a", Expr.Literal(Literal.Int 1L))
                Expr.Binary(BinaryOp.Gt, colRef "b", Expr.Literal(Literal.Int 2L))
            ]

        match ok (Analyzer.chooseScanMethod b catalog preds) with
        | ScanMethod.IndexRangeScan(name, lower, upper) ->
            Assert.Equal<string>("ix_pair", name)
            Assert.Equal<int>(2, lower.Length)
            Assert.Equal<int64>(1L, asInt lower.[0])
            Assert.Equal<int64>(2L, asInt lower.[1])
            Assert.True upper.IsNone
        | m -> failActual "IndexRangeScan ではない" m

        // 前置 a の等値が無ければ全走査
        let onlyRange = Expr.Binary(BinaryOp.Gt, colRef "b", Expr.Literal(Literal.Int 2L))

        match ok (Analyzer.chooseScanMethod b catalog [ onlyRange ]) with
        | ScanMethod.SeqScan -> ()
        | m -> failActual "SeqScan ではない" m

        // 全列等値 → IndexLookup（複合索引でも使う）
        let bothEq =
            [
                Expr.Binary(BinaryOp.Eq, colRef "a", Expr.Literal(Literal.Int 1L))
                Expr.Binary(BinaryOp.Eq, colRef "b", Expr.Literal(Literal.Int 2L))
            ]

        match ok (Analyzer.chooseScanMethod b catalog bothEq) with
        | ScanMethod.IndexLookup(name, values) ->
            Assert.Equal<string>("ix_pair", name)
            Assert.Equal<int>(2, values.Length)
            Assert.Equal<int64>(1L, asInt values.[0])
            Assert.Equal<int64>(2L, asInt values.[1])
        | m -> failActual "IndexLookup ではない" m

        // 前置 1列だけの等値 → IndexLookup（全列でなくても先頭から連続していれば使う）
        let onlyPrefix = Expr.Binary(BinaryOp.Eq, colRef "a", Expr.Literal(Literal.Int 1L))

        match ok (Analyzer.chooseScanMethod b catalog [ onlyPrefix ]) with
        | ScanMethod.IndexLookup(name, values) ->
            Assert.Equal<string>("ix_pair", name)
            Assert.Equal<int>(1, values.Length)
            Assert.Equal<int64>(1L, asInt values.[0])
        | m -> failActual "IndexLookup ではない" m

        // 先頭でない列だけの等値は索引の前置にならない → SeqScan
        let onlySecond = Expr.Binary(BinaryOp.Eq, colRef "b", Expr.Literal(Literal.Int 2L))

        match ok (Analyzer.chooseScanMethod b catalog [ onlySecond ]) with
        | ScanMethod.SeqScan -> ()
        | m -> failActual "SeqScan ではない" m)

[<Fact>]
let ``Analyzer.chooseScanMethod: LIKE 'prefix%' は索引の範囲走査にする`` () =
    withStore "an-like" (fun store ->
        let schema =
            {
                Name = "items"
                Columns = [ col "id" DataType.Integer; col "s" DataType.Text ]
                PrimaryKey = [ 0 ]
                Indexes =
                    [
                        {
                            Name = "ix_s"
                            Columns = [ "s" ]
                            Unique = false
                        }
                    ]
            }

        let ctx = ExecContext.create store None |> ExecContext.withSchema "items" schema
        let b = ok (Analyzer.bindTable ctx { Name = "items"; Alias = None })
        let ix = indexDefinition "items" schema.Indexes.[0]
        let catalog = ix |> Catalog.addIndex Catalog.Empty

        let pred =
            Expr.Like(
                Expr.Column { Qualifier = Some "items"; Name = "s" },
                Expr.Literal(Literal.String "ab%"),
                false,
                None
            )

        match ok (Analyzer.chooseScanMethod b catalog [ pred ]) with
        | ScanMethod.IndexRangeScan(name, lower, upper) ->
            Assert.Equal<string>("ix_s", name)
            Assert.Equal<int>(1, lower.Length)
            Assert.Equal<string>("ab", asText lower.[0])
            Assert.True upper.IsSome
            Assert.Equal<string>("ac", asText (expectSome "upper" upper).[0])
        | m -> failActual "IndexRangeScan ではない" m)

[<Fact>]
let ``Analyzer.chooseScanMethod: 等価条件があっても索引が無ければ全走査`` () =
    withStore "an2" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // name には索引が無い
        let pred =
            Expr.Binary(BinaryOp.Eq, pCol "name", Expr.Literal(Literal.String "alice"))

        let m = ok (Analyzer.chooseScanMethod b Catalog.Empty [ pred ])
        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, m))

[<Fact>]
let ``Analyzer.chooseScanMethod: 等価条件が無ければ全走査`` () =
    withStore "an4" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })
        let pred = Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))
        let m = ok (Analyzer.chooseScanMethod b Catalog.Empty [ pred ])
        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, m))

[<Fact>]
let ``Analyzer.chooseScanMethod: 20000段の AND を chooseScanMethod してもスタックを食わない`` () =
    withStore "an8" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // 左結合の AND の最深部に等値条件を置く。探索が非末尾だと、
        // この条件にたどり着くまでにスタックが積もる。
        let hit = Expr.Binary(BinaryOp.Eq, pCol "id", Expr.Literal(Literal.Int 42L))
        let miss = Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))

        let deep =
            List.init 20_000 (fun _ -> miss)
            |> List.fold (fun acc p -> Expr.Binary(BinaryOp.And, acc, p)) hit

        match ok (Analyzer.chooseScanMethod b Catalog.Empty [ deep ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(42L, asInt values.[0])
        | m -> failActual "PkLookup ではない" m)
