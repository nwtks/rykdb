// ===========================================================================
// Analyzer.fs のテスト（走査方法の選択・結合アルゴリズム）
// ===========================================================================

module RykDb.AnalyzerTests

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

[<Fact>]
let ``Binder.qualify: 修飾子無しの列が一意なら修飾名を付ける`` () =
    // people だけのスキーマでは `id` は people.id に一意に決まる。
    let only = Schema.ofTable "people" None peopleSchema
    let e = Expr.Column { Qualifier = None; Name = "id" }
    let bound = ok (Binder.qualify only e)

    match bound with
    | Expr.Column c -> Assert.Equal<string>("people", defaultArg c.Qualifier "")
    | _ -> Assert.Fail "EColumn ではない"

[<Fact>]
let ``Binder.qualify: 修飾子無しの列が曖昧なら付けない`` () =
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
    | Expr.Column c -> Assert.True(System.String.IsNullOrEmpty(defaultArg c.Qualifier ""))
    | _ -> Assert.Fail "Expr.EColumn ではない"

[<Fact>]
let ``Binder.qualify: スターは修飾の対象外`` () =
    let only = Schema.ofTable "people" None peopleSchema
    let bound = ok (Binder.qualify only (Expr.QualifiedStar "people"))

    match bound with
    | Expr.QualifiedStar q -> Assert.Equal<string>("people", q)
    | other -> Assert.Fail(sprintf "QualifiedStar ではない: %A" other)

[<Fact>]
let ``Binder.qualify: 全ノード種を qualify しても構造が保たれる`` () =
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
            Expr.Case([ n, i1; n, i2 ], Some i1), Expr.Case([ q, i1; q, i2 ], Some i1)
            Expr.Case([ (n, i1) ], None), Expr.Case([ (q, i1) ], None)
            Expr.FuncCall("ABS", [ n ]), Expr.FuncCall("ABS", [ q ])
            Expr.AggCall(AggFunc.Sum, false, n), Expr.AggCall(AggFunc.Sum, false, q)
            Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.Or, n, i1), Expr.IsNull(n, true)),
            Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.Or, q, i1), Expr.IsNull(q, true))
        ]

    for input, expected in samples do
        Assert.Equal<string>(Expr.toDisplayString expected, Expr.toDisplayString (ok (Binder.qualify schema input)))

[<Fact>]
let ``Predicate.splitConjuncts: AND を平坦化して分割する`` () =
    let e =
        Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.And, pCol "id", pCol "age"), pCol "name")

    let parts = Predicate.splitConjuncts e

    Assert.Equal<int>(3, parts.Length)
    Assert.True(Predicate.isLocalTo "people" parts.[0])
    Assert.True(Predicate.isLocalTo "people" parts.[1])

[<Fact>]
let ``Predicate.joinConjuncts: split した conjunct を AND で戻せる`` () =
    let e = Expr.Binary(BinaryOp.And, pCol "id", pCol "age")
    let parts = Predicate.splitConjuncts e
    let rejoined = Predicate.joinConjuncts parts

    Assert.True(rejoined |> Option.isSome)
    // 分割 → 再結合 で conjunct が 2 個のまま保たれる
    Assert.Equal<int>(2, Predicate.splitConjuncts rejoined.Value |> List.length)

[<Fact>]
let ``Predicate.isLocalTo: 修飾子なしの列参照は local とみなさない`` () =
    // 修飾子のない列はどのテーブル由来か特定できないため pushdown しない
    Assert.False(Predicate.isLocalTo "people" (Expr.Column { Qualifier = None; Name = "x" }))
    // 列参照を含まない式は true
    Assert.True(Predicate.isLocalTo "people" (Expr.Literal(Literal.Int 1L)))

[<Fact>]
let ``Analyzer.chooseScanMethod: 等価条件があれば主キー検索にする`` () =
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
let ``Analyzer.chooseScanMethod: 等価条件があっても索引が無ければ全走査`` () =
    withStore "an2" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })

        // name には索引が無い
        let pred =
            Expr.Binary(BinaryOp.Eq, pCol "name", Expr.Literal(Literal.String "alice"))

        let m = ok (Analyzer.chooseScanMethod b Catalog.empty [ pred ])
        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, m))

[<Fact>]
let ``Analyzer.chooseScanMethod: 索引があれば索引検索にする`` () =
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
let ``Analyzer.chooseScanMethod: 等価条件が無ければ全走査`` () =
    withStore "an4" (fun store ->
        let b = ok (Analyzer.bindTable (ctxOf store) { Name = "people"; Alias = None })
        let pred = Expr.Binary(BinaryOp.Gt, pCol "age", Expr.Literal(Literal.Int 30L))
        let m = ok (Analyzer.chooseScanMethod b Catalog.empty [ pred ])
        Assert.Equal<ScanMethod>(ScanMethod.SeqScan, m))

[<Fact>]
let ``Analyzer.chooseScanMethod: AND の奥と左右対称の等価条件を探せる`` () =
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
let ``Analyzer.chooseScanMethod: 負のリテラルは等値条件に使える`` () =
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

        let ctx = ExecContext.withSchema (ExecContext.create store None) "pairs" schema
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

        match ok (Analyzer.chooseScanMethod b Catalog.empty preds) with
        | ScanMethod.PkLookup values ->
            Assert.Equal<int>(2, values.Length)
            Assert.Equal<int64>(1L, asInt values.[0])
            Assert.Equal<int64>(2L, asInt values.[1])
        | m -> Assert.Fail(sprintf "PkLookup ではない: %A" m)

        // 部分一致は全走査
        match ok (Analyzer.chooseScanMethod b Catalog.empty [ eq "a" (Literal.Int 1L) ]) with
        | ScanMethod.SeqScan -> ()
        | m -> Assert.Fail(sprintf "SeqScan ではない: %A" m)

        // キー列の型と違うリテラルは全走査（比較時に型エラーにする）
        match ok (Analyzer.chooseScanMethod b Catalog.empty [ eq "a" (Literal.String "1") ]) with
        | ScanMethod.SeqScan -> ()
        | m -> Assert.Fail(sprintf "SeqScan ではない: %A" m))

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

        let ctx = ExecContext.withSchema (ExecContext.create store None) "pairs" schema
        let b = ok (Analyzer.bindTable ctx { Name = "pairs"; Alias = None })
        let ix = Catalog.toIndexDefinition "pairs" schema.Indexes.[0]
        let catalog = ix |> Catalog.addIndex Catalog.empty

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
        | m -> Assert.Fail(sprintf "IndexRangeScan ではない: %A" m)

        // 前置 a の等値が無ければ全走査
        let onlyRange = Expr.Binary(BinaryOp.Gt, colRef "b", Expr.Literal(Literal.Int 2L))

        match ok (Analyzer.chooseScanMethod b catalog [ onlyRange ]) with
        | ScanMethod.SeqScan -> ()
        | m -> Assert.Fail(sprintf "SeqScan ではない: %A" m)

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
        | m -> Assert.Fail(sprintf "IndexLookup ではない: %A" m)

        // 前置 1列だけの等値 → IndexLookup（全列でなくても先頭から連続していれば使う）
        let onlyPrefix = Expr.Binary(BinaryOp.Eq, colRef "a", Expr.Literal(Literal.Int 1L))

        match ok (Analyzer.chooseScanMethod b catalog [ onlyPrefix ]) with
        | ScanMethod.IndexLookup(name, values) ->
            Assert.Equal<string>("ix_pair", name)
            Assert.Equal<int>(1, values.Length)
            Assert.Equal<int64>(1L, asInt values.[0])
        | m -> Assert.Fail(sprintf "IndexLookup ではない: %A" m)

        // 先頭でない列だけの等値は索引の前置にならない → SeqScan
        let onlySecond = Expr.Binary(BinaryOp.Eq, colRef "b", Expr.Literal(Literal.Int 2L))

        match ok (Analyzer.chooseScanMethod b catalog [ onlySecond ]) with
        | ScanMethod.SeqScan -> ()
        | m -> Assert.Fail(sprintf "SeqScan ではない: %A" m))

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

        let ctx = ExecContext.withSchema (ExecContext.create store None) "items" schema
        let b = ok (Analyzer.bindTable ctx { Name = "items"; Alias = None })
        let ix = Catalog.toIndexDefinition "items" schema.Indexes.[0]
        let catalog = ix |> Catalog.addIndex Catalog.empty

        let pred =
            Expr.Like(Expr.Column { Qualifier = Some "items"; Name = "s" }, Expr.Literal(Literal.String "ab%"), false)

        match ok (Analyzer.chooseScanMethod b catalog [ pred ]) with
        | ScanMethod.IndexRangeScan(name, lower, upper) ->
            Assert.Equal<string>("ix_s", name)
            Assert.Equal<int>(1, lower.Length)
            Assert.Equal<string>("ab", asText lower.[0])
            Assert.True upper.IsSome
            Assert.Equal<string>("ac", asText upper.Value.[0])
        | m -> Assert.Fail(sprintf "IndexRangeScan ではない: %A" m))

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

        match ok (Analyzer.chooseScanMethod b Catalog.empty [ deep ]) with
        | ScanMethod.PkLookup values -> Assert.Equal<int64>(42L, asInt values.[0])
        | m -> Assert.Fail(sprintf "PkLookup ではない: %A" m))

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

    match Analyzer.chooseJoinAlgorithm leftSchema rightSchema rightTable leftSmall rightScan on with
    | JoinAlgorithm.IndexNestedLoop name, _, Some lefts ->
        Assert.Equal<string>("ix_r_a", name)
        Assert.Equal<int array>([| 0 |], lefts)
    | other -> Assert.Fail(sprintf "IndexNestedLoop ではありません: %A" other)

    // 左が大きい（Limit なし）ならハッシュ結合
    match
        Analyzer.chooseJoinAlgorithm
            leftSchema
            rightSchema
            rightTable
            (Logical.Scan("l", ScanMethod.SeqScan))
            rightScan
            on
    with
    | JoinAlgorithm.HashJoin, _, _ -> ()
    | other -> Assert.Fail(sprintf "HashJoin ではありません: %A" other)

    // 非等値なら入れ子ループ
    let nonEqui =
        Expr.Binary(
            BinaryOp.Gt,
            Expr.Column { Qualifier = Some "l"; Name = "a" },
            Expr.Column { Qualifier = Some "r"; Name = "a" }
        )

    match Analyzer.chooseJoinAlgorithm leftSchema rightSchema rightTable leftSmall rightScan nonEqui with
    | JoinAlgorithm.NestedLoop, _, _ -> ()
    | other -> Assert.Fail(sprintf "NestedLoop ではありません: %A" other)
