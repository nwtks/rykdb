// ===========================================================================
// Ops.fs のテスト: filter・sortBy・distinct・集約・射影
// ===========================================================================

module RykDb.OpsTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// このモジュールのテストは上限なし（スピルしない）経路を検証する。
/// スピル経路は専用のバジェットで別途テストする。
module private UnlimitedOps =
    let filter = Ops.filter
    let sortBy = Ops.sortBy MemoryBudget.Unlimited
    let distinct = Ops.distinct MemoryBudget.Unlimited
    let aggregate = Ops.aggregate MemoryBudget.Unlimited
    let havingFilter = Ops.havingFilter

/// 行の配列を Op にする（入れ子の配列推論を避けるため明示的に作る）。
let private opOfRows (rows: SqlValue array list) = Op.ofArray (List.toArray rows)

/// 1 行を作る。
let private row (values: SqlValue list) = List.toArray values

/// SqlValue から値を取り出す（テスト用）。
let private asDec (v: SqlValue) =
    match v with
    | SqlValue.Decimal d -> d
    | SqlValue.Int i -> decimal i
    | other -> failwithf "DECIMAL ではありません: %A" other

let private asReal (v: SqlValue) =
    match v with
    | SqlValue.Real f -> f
    | other -> failwithf "REAL ではありません: %A" other

let private col (name: string) =
    Expr.Column { Qualifier = None; Name = name }

/// 行の並びを表示用の文字列にする（スピル有無の比較用）。
let private rowsText (rows: SqlValue array array) =
    rows
    |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
    |> String.concat "|"

/// 退避に使った一時ファイルが残っていないことを確かめる（サイズ付きの詳細表示）。
let private assertNoLeftoversDetailed (dir: string) =
    let files = System.IO.Directory.GetFiles dir

    if files.Length > 0 then
        let detail =
            files
            |> Array.map (fun p -> sprintf "%s(%d)" (System.IO.Path.GetFileName p) (System.IO.FileInfo(p).Length))
            |> String.concat " "

        Assert.Fail(sprintf "一時ファイルが残っています: %s" detail)

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

    let sorted = ok (UnlimitedOps.sortBy schema2 keys op)
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

    let rows2 = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 desc op)))
    Assert.Equal<int64>(3L, asInt rows2.[0].[0])

[<Fact>]
let ``sortBy: NULL は昇順で先頭、降順で末尾`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 3L; SqlValue.Int 0L ]
                row [ SqlValue.Null; SqlValue.Int 0L ]
                row [ SqlValue.Int 1L; SqlValue.Int 0L ]
                row [ SqlValue.Null; SqlValue.Int 0L ]
                row [ SqlValue.Int 2L; SqlValue.Int 0L ]
            ]

    let ascKey =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
        ]

    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 ascKey op)))
    Assert.True(SqlValue.isNull rows.[0].[0])
    Assert.True(SqlValue.isNull rows.[1].[0])
    Assert.Equal<int64 list>([ 1L; 2L; 3L ], [ for r in 2..4 -> asInt rows.[r].[0] ])

    let descKey =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Descending
            }
        ]

    let rowsDesc = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 descKey op)))
    Assert.Equal<int64 list>([ 3L; 2L; 1L ], [ for r in 0..2 -> asInt rowsDesc.[r].[0] ])
    Assert.True(SqlValue.isNull rowsDesc.[3].[0])
    Assert.True(SqlValue.isNull rowsDesc.[4].[0])

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

    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 keys op)))
    // 0/99, 1/10, 1/20
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(10L, asInt rows.[1].[1])
    Assert.Equal<int64>(20L, asInt rows.[2].[1])

[<Fact>]
let ``sortBy: キーが空なら入力をそのまま返す`` () =
    let op = opOfRows [ row [ SqlValue.Int 2L ]; row [ SqlValue.Int 1L ] ]
    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 [] op)))
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])

[<Fact>]
let ``sortBy: 式のキーで並べ替えられる`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 2L; SqlValue.Int 0L ]
                row [ SqlValue.Int 1L; SqlValue.Int 0L ]
            ]

    let keys =
        [
            {
                Expression = Expr.Binary(BinaryOp.Add, col "a", Expr.Literal(Literal.Int 0L))
                Direction = SortDirection.Ascending
            }
        ]

    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 keys op)))
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[1].[0])

[<Fact>]
let ``sortBy: 同値のキーは次のキーで、比較不能なキーは Error になる`` () =
    // 1 列目に同じ TEXT を入れて同値にし、2 列目の差で順序が決まるようにする。
    let op =
        opOfRows
            [
                row [ SqlValue.Text "x"; SqlValue.Int 2L ]
                row [ SqlValue.Text "x"; SqlValue.Int 1L ]
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

    // 1 列目は同値（TEXT 同士）→ 2 列目で比較する
    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 keys op)))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

    // キーが 1 本だけで比較不能（型が合わない）なら Error になる（握り潰さない）
    let op2 =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 10L ]
                row [ SqlValue.Text "x"; SqlValue.Int 20L ]
            ]

    let keys1 =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
        ]

    // キーが 1 本だけで比較不能（型が合わない）なら Error になる（握り潰さない）
    let r1 = UnlimitedOps.sortBy schema2 keys1 op2
    expectErrorKind ErrorKind.TypeError "比較不能なキーは Error になる" r1

[<Fact>]
let ``sortBy: 式の評価エラーは Error になる`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L; SqlValue.Int 0L ] ]

    let keys =
        [
            {
                Expression = Expr.Binary(BinaryOp.Add, col "nope", Expr.Literal(Literal.Int 1L))
                Direction = SortDirection.Ascending
            }
        ]

    UnlimitedOps.sortBy schema2 keys op
    |> expectErrorKind ErrorKind.NameError "ORDER BY の存在しない列が OK になっています"

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

    let rows = ok (Op.toArray (UnlimitedOps.distinct op))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])

[<Fact>]
let ``havingFilter: 集約値とグループ値を定数へ置き換える`` () =
    let groupBy = [ col "a" ]

    let aggs =
        [ AggFunc.Count, true, false, Expr.Star; AggFunc.Sum, false, false, col "b" ]
    // 行 = [a; COUNT(*); SUM(b)]
    let r = [| SqlValue.Int 7L; SqlValue.Int 3L; SqlValue.Int 10L |]

    let eval (pred: Expr) =
        match UnlimitedOps.havingFilter groupBy aggs pred with
        | Error e -> DbErr e
        | Ok p -> p r

    // 集約値（COUNT(*)）
    Assert.True(
        okV (
            eval (Expr.Binary(BinaryOp.Gt, Expr.AggCall(AggFunc.Count, false, Expr.Star), Expr.Literal(Literal.Int 2L)))
        )
    )

    // グループ値（列 → 先頭の値）
    Assert.True(okV (eval (Expr.Binary(BinaryOp.Eq, col "a", Expr.Literal(Literal.Int 7L)))))
    // 定数はそのまま
    Assert.True(okV (eval (Expr.Literal(Literal.Bool true))))
    // Name でも集約キーに一致すれば置き換える
    Assert.True(okV (eval (Expr.Binary(BinaryOp.Eq, Expr.Name "COUNT(*)", Expr.Literal(Literal.Int 3L)))))
    // 子を持つ式は再帰的に置き換える
    Assert.True(
        okV (eval (Expr.Binary(BinaryOp.And, Expr.Literal(Literal.Bool true), Expr.Literal(Literal.Bool true))))
    )
    // 集約リストに無い集約は NULL になる（比較は UNKNOWN → 偽）
    Assert.False(
        okV (eval (Expr.Binary(BinaryOp.Eq, Expr.AggCall(AggFunc.Min, false, col "b"), Expr.Literal(Literal.Int 99L))))
    )

    // 集約でもグループ値でもない Name / 列は置き換えられない（評価時にエラー）
    expectErrorV "未知の Name が OK になっています" (eval (Expr.Name "zz"))
    expectErrorV "未知の列が OK になっています" (eval (col "zz"))
    // スターは置き換えず、そのまま評価エラーになる
    expectErrorV "WHERE の * が OK になっています" (eval Expr.Star)
    expectErrorV "WHERE の t.* が OK になっています" (eval (Expr.QualifiedStar "t"))

[<Fact>]
let ``末尾再帰: 50000段の式を havingFilter してもスタックを食わない`` () =
    let depth = 50_000

    // テスト側も末尾再帰（fold）で組む（SQL 経由でも解析できるが、ここでは AST を直接組む）
    let deep =
        List.init depth (fun _ -> Expr.Literal(Literal.Bool true))
        |> List.fold (fun acc e -> Expr.Binary(BinaryOp.And, acc, e)) (Expr.Literal(Literal.Bool true))

    let groupBy = [ col "a" ]
    let aggs = [ (AggFunc.Count, true, false, Expr.Star) ]
    // 行 = [a; COUNT(*)]
    let r = [| SqlValue.Int 7L; SqlValue.Int 3L |]
    Assert.True(okV ((ok (UnlimitedOps.havingFilter groupBy aggs deep)) r))

[<Fact>]
let ``集約: 入力行を抱えずにグループごとに集める`` () =
    // 10 万行を 3 グループに振り、行を全部保持しない実装でも結果が同じであること。
    let rec make (i: int) : Op =
        if i >= 100_000 then
            Op.Empty
        else
            Op.compute (fun () ->
                let grp = int64 (i % 3)
                OpStep.OpRow(row [ SqlValue.Int grp; SqlValue.Int(int64 i) ], make (i + 1)))

    let aggs = [ (AggFunc.Count, true, false, Expr.Literal Literal.Null) ]

    let rows =
        ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [ col "a" ] aggs (make 0))))

    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(33_334L, asInt rows.[0].[1])
    Assert.Equal<int64>(33_333L, asInt rows.[1].[1])
    Assert.Equal<int64>(33_333L, asInt rows.[2].[1])

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
            AggFunc.Count, true, false, Expr.Literal Literal.Null
            AggFunc.Count, false, false, col "a"
            AggFunc.Sum, false, false, col "a"
            AggFunc.Avg, false, false, col "a"
            AggFunc.Min, false, false, col "a"
            AggFunc.Max, false, false, col "a"
        ]

    let rows = ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [] aggs op)))
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
let ``集約: 行が 0 でも COUNT(*) は 1行を返す`` () =
    let rows =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOps.aggregate
                        schema2
                        []
                        [ (AggFunc.Count, true, false, Expr.Literal Literal.Null) ]
                        Op.Empty
                )
            )
        )

    // SQL 標準どおり、GROUP BY なしの集約は入力 0行でも 1行 (0) を返す。
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])

[<Fact>]
let ``集約: GROUP BY でグループごとに 1行`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 10L ]
                row [ SqlValue.Int 1L; SqlValue.Int 20L ]
                row [ SqlValue.Int 2L; SqlValue.Int 30L ]
            ]

    let aggs = [ (AggFunc.Sum, false, false, col "b") ]
    let rows = ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [ col "a" ] aggs op)))
    // 2グループ
    Assert.Equal<int>(2, rows.Length)

    // 最初に現れたグループが先
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal(30M, asDec rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[0])
    Assert.Equal(30M, asDec rows.[1].[1])

[<Fact>]
let ``集約: GROUP BY のキーが NULL の行も 1グループになる`` () =
    let op: Op =
        Op.ofArray [| [| SqlValue.Null; SqlValue.Int 1L |]; [| SqlValue.Null; SqlValue.Int 2L |] |]

    let aggs = [ (AggFunc.Count, true, false, Expr.Literal Literal.Null) ]
    let rows = ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [ col "a" ] aggs op)))

    // SQL では NULL が別のグループになるが、本実装は NULL 同士を等しい扱いにする。
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[1])

[<Fact>]
let ``集約: SUM / AVG の REAL と非数値`` () =
    // REAL は REAL のまま累積される
    let reals = opOfRows [ row [ SqlValue.Real 1.5 ]; row [ SqlValue.Real 2.5 ] ]

    let rows =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOps.aggregate
                        schema2
                        []
                        [ AggFunc.Sum, false, false, col "a"; AggFunc.Avg, false, false, col "a" ]
                        reals
                )
            )
        )

    Assert.Equal(4.0, asReal rows.[0].[0], 6)
    Assert.Equal(2.0, asReal rows.[0].[1], 6)

    // 数値でない値は Error になる（F15: 集約エラーは伝播する）
    let texts = opOfRows [ row [ SqlValue.Text "x" ] ]

    let r2 =
        UnlimitedOps.aggregate schema2 [] [ (AggFunc.Sum, false, false, col "a") ] texts

    expectError "SUM(文字列) は Error になる" r2

    // AVG で全行 NULL は 0 除算せず NULL
    let nulls = opOfRows [ row [ SqlValue.Null ] ]

    let rows3 =
        ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [] [ (AggFunc.Avg, false, false, col "a") ] nulls)))

    Assert.True(SqlValue.isNull rows3.[0].[0])

[<Fact>]
let ``集約: GROUP BY 式の評価失敗は Error`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L; SqlValue.Int 2L ] ]

    expectErrorKind
        ErrorKind.NameError
        "存在しない列の GROUP BY が OK になっています"
        (UnlimitedOps.aggregate schema2 [ col "nope" ] [ (AggFunc.Count, true, false, Expr.Literal Literal.Null) ] op)

[<Fact>]
let ``project: 項目が空なら入力をそのまま返す`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L ] ]
    let rows = ok (Op.toArray (ok (Ops.project schema2 schema2 [] [] op)))
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])

[<Fact>]
let ``distinct: 上限を超えて退避しても結果と順序が変わらない`` () =
    withDir "distinct-spill" (fun dir ->
        // 37 種類の行が繰り返し現れる（300 行）。
        let op () =
            opOfRows
                [
                    for i in 0..299 -> row [ SqlValue.Int(int64 (i % 37)); SqlValue.Int(int64 (i % 37)) ]
                ]

        let baseline = ok (Op.toArray (UnlimitedOps.distinct (op ())))

        // 1行目から退避する極小バジェット。
        let spilled = ok (Op.toArray (Ops.distinct (tightBudget dir 1L) (op ())))

        // メモリと退避が混ざる中間のバジェット。
        let mixed = ok (Op.toArray (Ops.distinct (tightBudget dir 2000L) (op ())))
        Assert.Equal<int>(37, baseline.Length)
        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        Assert.Equal<string>(rowsText baseline, rowsText mixed)
        // 最初に現れた順序（0, 1, 2, ...）が保たれている。
        Assert.Equal<int64>(0L, asInt baseline.[0].[0])
        Assert.Equal<int64>(36L, asInt baseline.[36].[0])

        // 一時ファイルは残らない。
        assertNoLeftoversDetailed dir)

[<Fact>]
let ``集約: 上限を超えて退避しても結果と順序が変わらない`` () =
    withDir "aggregate-spill" (fun dir ->
        let op () =
            opOfRows
                [
                    for i in 0..499 -> row [ SqlValue.Int(int64 (i % 53)); SqlValue.Int(int64 i) ]
                ]

        let aggs =
            [
                AggFunc.Count, true, false, Expr.Literal Literal.Null
                AggFunc.Sum, false, false, col "b"
                AggFunc.Max, false, false, col "b"
            ]

        let baseline =
            ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [ col "a" ] aggs (op ()))))

        let spilled =
            ok (Op.toArray (ok (Ops.aggregate (tightBudget dir 1L) schema2 [ col "a" ] aggs (op ()))))

        let mixed =
            ok (Op.toArray (ok (Ops.aggregate (tightBudget dir 3000L) schema2 [ col "a" ] aggs (op ()))))

        Assert.Equal<int>(53, baseline.Length)
        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        Assert.Equal<string>(rowsText baseline, rowsText mixed)
        // 最初に現れたグループ順。
        Assert.Equal<int64>(0L, asInt baseline.[0].[0])
        Assert.Equal<int64>(52L, asInt baseline.[52].[0])

        assertNoLeftoversDetailed dir)

[<Fact>]
let ``集約: 退避したグループにも DISTINCT 集約が効く`` () =
    withDir "aggregate-spill-distinct" (fun dir ->
        let op () =
            opOfRows
                [
                    for i in 0..299 -> row [ SqlValue.Int(int64 (i % 7)); SqlValue.Int(int64 (i % 5)) ]
                ]

        let aggs = [ (AggFunc.Count, false, true, col "b") ]

        let baseline =
            ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [ col "a" ] aggs (op ()))))

        let spilled =
            ok (Op.toArray (ok (Ops.aggregate (tightBudget dir 1L) schema2 [ col "a" ] aggs (op ()))))

        Assert.Equal<int>(7, baseline.Length)
        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        assertNoLeftoversDetailed dir)

[<Fact>]
let ``集約: DISTINCT の値が上限を超えても全集約関数が同じ結果`` () =
    withDir "aggregate-distinct-spill" (fun dir ->
        // 1グループに 500行、値は 0..49 を繰り返す（ユニーク 50個）。
        // グループ分割では割れないので、Seen の退避経路が必須。
        let op () =
            opOfRows [ for i in 0..499 -> row [ SqlValue.Int(int64 (i % 50)) ] ]

        let aggs =
            [
                AggFunc.Count, false, true, col "a"
                AggFunc.Sum, false, true, col "a"
                AggFunc.Avg, false, true, col "a"
                AggFunc.Min, false, true, col "a"
                AggFunc.Max, false, true, col "a"
            ]

        let baseline = ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [] aggs (op ()))))

        let spilled =
            ok (Op.toArray (ok (Ops.aggregate (tightBudget dir 1L) schema2 [] aggs (op ()))))

        Assert.Equal<int>(1, baseline.Length)
        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        Assert.Equal<int64>(50L, asInt baseline.[0].[0])
        Assert.Equal<decimal>(1225M, asDec baseline.[0].[1])
        assertNoLeftoversDetailed dir)

[<Fact>]
let ``集約: 最後のスピル以降に溜まった DISTINCT 値も結果に含める`` () =
    // 作業メモリ 2048B では 65 個目でスピルし、残り 35 個は `Seen` に溜まったまま終わる。
    // finalize が `Seen` を run に含めないと COUNT が 65 になり、Min/Max も過小・過大のまま
    // 返っていた（実測 65/100）。
    withDir "aggregate-distinct-spill-tail" (fun dir ->
        let op () =
            opOfRows [ for i in 0..99 -> row [ SqlValue.Int(int64 i) ] ]

        let aggs =
            [
                AggFunc.Count, false, true, col "a"
                AggFunc.Sum, false, true, col "a"
                AggFunc.Min, false, true, col "a"
                AggFunc.Max, false, true, col "a"
            ]

        let spilled =
            ok (Op.toArray (ok (Ops.aggregate (tightBudget dir 2048L) schema2 [] aggs (op ()))))

        Assert.Equal<int64>(100L, asInt spilled.[0].[0])
        Assert.Equal<decimal>(4950M, asDec spilled.[0].[1])
        Assert.Equal<int64>(0L, asInt spilled.[0].[2])
        Assert.Equal<int64>(99L, asInt spilled.[0].[3])
        assertNoLeftoversDetailed dir)

[<Fact>]
let ``集約: 退避しても DECIMAL のスケール違いを同じグループにする`` () =
    // 1.0 と 1.00 は SQL では等しい。退避のキーを `ValueCodec` で符号化すると
    // 別のグループに分かれていた（メモリ経路は構造的等価で一致していた）。
    withDir "aggregate-spill-decimal" (fun dir ->
        let op () =
            opOfRows
                [
                    row [ SqlValue.Decimal 1.0M; SqlValue.Int 1L ]
                    row [ SqlValue.Decimal 1.00M; SqlValue.Int 1L ]
                    row [ SqlValue.Decimal 2.00M; SqlValue.Int 1L ]
                ]

        let aggs = [ (AggFunc.Count, false, false, col "b") ]

        let baseline =
            ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [ col "a" ] aggs (op ()))))

        let spilled =
            ok (Op.toArray (ok (Ops.aggregate (tightBudget dir 1L) schema2 [ col "a" ] aggs (op ()))))

        Assert.Equal<int>(2, baseline.Length)
        Assert.Equal<int>(2, spilled.Length)

        // 退避経路はグループの代表値を最後に読んだ行の表現で返す（1.0 でも 1.00 でも同じ値）。
        match spilled.[0].[0] with
        | SqlValue.Decimal d -> Assert.Equal<decimal>(1M, d)
        | other -> failActual "DECIMAL ではありません" other

        Assert.Equal<int64 array>([| 1L; 2L |], spilled |> Array.map (fun r -> asInt r.[1]) |> Array.sort)
        assertNoLeftoversDetailed dir)

[<Fact>]
let ``集約: DISTINCT の退避は NULL と REAL も正しく扱う`` () =
    withDir "aggregate-distinct-spill-null" (fun dir ->
        let op () =
            opOfRows
                [
                    for i in 0..199 ->
                        row
                            [
                                if i % 7 = 0 then
                                    SqlValue.Null
                                else
                                    SqlValue.vReal (float (i % 11))
                            ]
                ]

        let aggs =
            [ AggFunc.Count, false, true, col "a"; AggFunc.Sum, false, true, col "a" ]

        let baseline = ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [] aggs (op ()))))

        let spilled =
            ok (Op.toArray (ok (Ops.aggregate (tightBudget dir 1L) schema2 [] aggs (op ()))))

        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        assertNoLeftoversDetailed dir)

[<Fact>]
let ``集約: DISTINCT の退避ファイルは集約完了時点で消える`` () =
    withDir "aggregate-distinct-spill-cleanup" (fun dir ->
        let scope = ScanScope()

        let budget =
            { tightBudget dir 1L with
                Scope = Some scope
            }

        let op () =
            opOfRows [ for i in 0..199 -> row [ SqlValue.Int(int64 (i % 50)) ] ]

        let aggs = [ (AggFunc.Count, false, true, col "a") ]
        let result = ok (Ops.aggregate budget schema2 [] aggs (op ()))

        // aggregate は入力を読み切るので、退避した値の畳み込みもこの時点で終わっている。
        assertNoLeftoversDetailed dir

        // 出力を 1行も読まずに打ち切っても（LIMIT 0 相当）、残骸は出ない。
        let rows = ok (Op.toArray result)
        Assert.Equal<int64>(50L, asInt rows.[0].[0])
        scope.DisposeAll()
        assertNoLeftoversDetailed dir)

[<Fact>]
let ``列解決: 未知の列は行が 0 件でも演算子の構築時に Error`` () =
    // 入力が 0 行だと述語・射影・並べ替えは一度も評価されない。
    // 列解決は構築時に検査し、行数に依存せず NameError にする。
    let sortKey =
        {
            Expression = col "nope"
            Direction = SortDirection.Ascending
        }

    let item =
        {
            Expression = col "nope"
            Alias = None
        }

    expectErrorKind ErrorKind.NameError "filter が OK になっています" (UnlimitedOps.filter schema2 (col "nope") Op.Empty)
    expectErrorKind ErrorKind.NameError "sortBy が OK になっています" (UnlimitedOps.sortBy schema2 [ sortKey ] Op.Empty)
    expectErrorKind ErrorKind.NameError "project が OK になっています" (Ops.project schema2 schema2 [] [ item ] Op.Empty)

    expectErrorKind
        ErrorKind.NameError
        "aggregate が OK になっています"
        (UnlimitedOps.aggregate schema2 [ col "nope" ] [ (AggFunc.Count, true, false, Expr.Star) ] Op.Empty)
