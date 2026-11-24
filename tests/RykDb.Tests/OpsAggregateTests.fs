// ===========================================================================
// OpsAggregate.fs のテスト（HAVING・集約）
// ===========================================================================

module RykDb.OpsAggregateTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers
open RykDb.TestHelpers.OpsFixtures

[<Fact>]
let ``集約: INTEGER と DECIMAL が等しいグループは 1 つにまとめ、先頭行の値を返す`` () =
    let op: Op =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Decimal 1.0M |]; [| SqlValue.Int 2L |] |]

    let aggs = [ (AggFunc.Count, true, false, Expr.Star) ]
    let rows = ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [ col "a" ] aggs op)))
    Assert.Equal<int>(2, rows.Length)
    // グループ値は先頭出現の値（と型）を保つ
    Assert.Equal(SqlValue.Int 1L, rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[0].[1])
    Assert.Equal(SqlValue.Int 2L, rows.[1].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[1])

[<Fact>]
let ``集約: DISTINCT 集約の等値も SQL 等値（INTEGER と DECIMAL）で判定する`` () =
    let op: Op = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Decimal 1.0M |] |]
    let aggs = [ (AggFunc.Count, false, true, col "a") ]
    let rows = ok (Op.toArray (ok (UnlimitedOps.aggregate schema2 [] aggs op)))
    Assert.Equal<int64>(1L, asInt rows.[0].[0])

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
            ok (Op.toArray (ok (OpsAggregate.aggregate (tightBudget dir 1L) schema2 [ col "a" ] aggs (op ()))))

        let mixed =
            ok (Op.toArray (ok (OpsAggregate.aggregate (tightBudget dir 3000L) schema2 [ col "a" ] aggs (op ()))))

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
            ok (Op.toArray (ok (OpsAggregate.aggregate (tightBudget dir 1L) schema2 [ col "a" ] aggs (op ()))))

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
            ok (Op.toArray (ok (OpsAggregate.aggregate (tightBudget dir 1L) schema2 [] aggs (op ()))))

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
            ok (Op.toArray (ok (OpsAggregate.aggregate (tightBudget dir 2048L) schema2 [] aggs (op ()))))

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
            ok (Op.toArray (ok (OpsAggregate.aggregate (tightBudget dir 1L) schema2 [ col "a" ] aggs (op ()))))

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
            ok (Op.toArray (ok (OpsAggregate.aggregate (tightBudget dir 1L) schema2 [] aggs (op ()))))

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
        let result = ok (OpsAggregate.aggregate budget schema2 [] aggs (op ()))

        // aggregate は入力を読み切るので、退避した値の畳み込みもこの時点で終わっている。
        assertNoLeftoversDetailed dir

        // 出力を 1行も読まずに打ち切っても（LIMIT 0 相当）、残骸は出ない。
        let rows = ok (Op.toArray result)
        Assert.Equal<int64>(50L, asInt rows.[0].[0])
        scope.DisposeAll()
        assertNoLeftoversDetailed dir)
