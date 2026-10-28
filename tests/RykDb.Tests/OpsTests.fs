// ===========================================================================
// Ops.fs のテスト: Op 演算子・sortBy・distinct・集約・結合
// ===========================================================================

module RykDb.OpsTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 2 列（a, b）のスキーマ。
let private schema2 =
    outSchema
        [
            outCol "a" None (Some DataType.Integer)
            outCol "b" None (Some DataType.Integer)
        ]

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

let private leftSchema =
    outSchema [ outCol "lid" (Some "l") (Some DataType.Integer) ]

let private rightSchema =
    outSchema [ outCol "rid" (Some "r") (Some DataType.Integer) ]

let private lcol = Expr.Column { Qualifier = Some "l"; Name = "lid" }
let private rcol = Expr.Column { Qualifier = Some "r"; Name = "rid" }

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

    let sorted = ok (Ops.sortBy schema2 op keys)
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

    let rows2 = ok (Op.toArray (ok (Ops.sortBy schema2 op desc)))
    Assert.Equal<int64>(3L, asInt rows2.[0].[0])

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

    let rows = ok (Op.toArray (ok (Ops.sortBy schema2 op keys)))
    // 0/99, 1/10, 1/20
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(10L, asInt rows.[1].[1])
    Assert.Equal<int64>(20L, asInt rows.[2].[1])

[<Fact>]
let ``distinctWithin: バジェット超過は CapacityError`` () =
    // 1行 ≈ 数十バイト。バジェット 1 なら 2 行目で超過する。
    let dup =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 1L ]
                row [ SqlValue.Int 2L; SqlValue.Int 2L ]
                row [ SqlValue.Int 3L; SqlValue.Int 3L ]
            ]

    // distinctWithin は遅延の Op を返すので、流し込んで初めて CapacityError になる。
    expectErrorKind ErrorKind.CapacityError "バジェット超過は CapacityError" (Op.toArray (Ops.distinctWithin 1L dup))

[<Fact>]
let ``集約: 入力行を抱えずにグループごとに集める`` () =
    // 10 万行を 3 グループに振り、行を全部保持しない実装でも結果が同じであること。
    let rec make (i: int) : Op =
        if i >= 100_000 then
            Op.Done
        else
            Op.Compute(fun () ->
                let grp = int64 (i % 3)
                ret (Some(row [ SqlValue.Int grp; SqlValue.Int(int64 i) ], make (i + 1))))

    let aggs = [ (AggFunc.Count, true, false, Expr.Literal Literal.Null) ]
    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 (make 0) [ col "a" ] aggs)))

    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(33_334L, asInt rows.[0].[1])
    Assert.Equal<int64>(33_333L, asInt rows.[1].[1])
    Assert.Equal<int64>(33_333L, asInt rows.[2].[1])

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

    let rows = ok (Op.toArray (Ops.distinct op))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])

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

    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 op [] aggs)))
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
                ok (Ops.aggregate schema2 Op.Done [] [ (AggFunc.Count, true, false, Expr.Literal(Literal.Null)) ])
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
    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 op [ col "a" ] aggs)))
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
    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 op [ col "a" ] aggs)))

    // SQL では NULL が別のグループになるが、本実装は NULL 同士を等しい扱いにする。
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[1])

[<Fact>]
let ``結合: INNER JOIN`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let right = Op.ofArray [| [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]
    let on = Expr.Binary(BinaryOp.Eq, lcol, rcol)

    let rows = ok (Op.toArray (ok (Ops.innerJoin leftSchema left rightSchema right on)))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(3L, asInt rows.[1].[0])

[<Fact>]
let ``結合: LEFT JOIN は一致が無ければ NULL`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 2L |] |]
    let on = Expr.Binary(BinaryOp.Eq, lcol, rcol)

    let rows = ok (Op.toArray (ok (Ops.leftJoin leftSchema left rightSchema right on)))
    Assert.Equal<int>(2, rows.Length)

    // 1 は一致が無いので右が NULL
    Assert.True(SqlValue.isNull rows.[0].[1])

    // 2 は一致
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

[<Fact>]
let ``結合: ハッシュ INNER JOIN`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let right = Op.ofArray [| [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let rows =
        ok (Op.toArray (ok (Ops.innerJoinHash leftSchema left rightSchema right [| 0 |] [| 0 |] None)))

    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(3L, asInt rows.[1].[0])

[<Fact>]
let ``結合: ハッシュ LEFT JOIN は一致が無ければ NULL`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 2L |] |]

    let rows =
        ok (Op.toArray (ok (Ops.leftJoinHash leftSchema left rightSchema right [| 0 |] [| 0 |] None)))

    Assert.Equal<int>(2, rows.Length)
    Assert.True(SqlValue.isNull rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

[<Fact>]
let ``結合: ハッシュ結合は NULL キーを一致させない`` () =
    let left = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]

    let rows =
        ok (Op.toArray (ok (Ops.innerJoinHash leftSchema left rightSchema right [| 0 |] [| 0 |] None)))

    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])

[<Fact>]
let ``結合: ハッシュ結合は複合キーと残りの ON 句を評価する`` () =
    let leftSchema2 =
        outSchema
            [
                outCol "a" (Some "l") (Some DataType.Integer)
                outCol "b" (Some "l") (Some DataType.Integer)
            ]

    let rightSchema2 =
        outSchema
            [
                outCol "a" (Some "r") (Some DataType.Integer)
                outCol "b" (Some "r") (Some DataType.Integer)
            ]

    let left = Op.ofArray [| [| SqlValue.Int 1L; SqlValue.Int 10L |] |]

    let right =
        Op.ofArray
            [|
                [| SqlValue.Int 1L; SqlValue.Int 10L |]
                [| SqlValue.Int 1L; SqlValue.Int 20L |]
            |]

    let residual =
        Expr.Binary(BinaryOp.Gt, Expr.Column { Qualifier = Some "r"; Name = "b" }, Expr.Literal(Literal.Int 15L))

    // キー a だけでは 2 行候補。b > 15 で 1 行に絞る。
    let rows =
        ok (Op.toArray (ok (Ops.innerJoinHash leftSchema2 left rightSchema2 right [| 0 |] [| 0 |] (Some residual))))

    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(20L, asInt rows.[0].[3])

[<Fact>]
let ``結合: インデックス入れ子ループ（INNER / LEFT）`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let lookup key =
        match key with
        | [| SqlValue.Int 2L |] -> Ok(Op.ofArray [| [| SqlValue.Int 2L |] |])
        | [| SqlValue.Int 3L |] -> Ok(Op.ofArray [| [| SqlValue.Int 3L |] |])
        | _ -> Ok Op.empty

    let inner =
        ok (Op.toArray (Ops.innerJoinIndexed leftSchema left rightSchema [| 0 |] lookup None))

    Assert.Equal<int>(2, inner.Length)
    Assert.Equal<int64>(2L, asInt inner.[0].[0])
    Assert.Equal<int64>(3L, asInt inner.[1].[0])

    let left2 = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    let leftRows =
        ok (Op.toArray (Ops.leftJoinIndexed leftSchema left2 rightSchema [| 0 |] lookup None))

    Assert.Equal<int>(2, leftRows.Length)
    Assert.True(SqlValue.isNull leftRows.[0].[1])
    Assert.Equal<int64>(2L, asInt leftRows.[1].[1])

[<Fact>]
let ``sortBy: キーが空なら入力をそのまま返す`` () =
    let op = opOfRows [ row [ SqlValue.Int 2L ]; row [ SqlValue.Int 1L ] ]
    let rows = ok (Op.toArray (ok (Ops.sortBy schema2 op [])))
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])

[<Fact>]
let ``sortBy: 列参照でないキーはエラー`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L ] ]

    // a + 1 は列の直接参照ではない
    let keys =
        [
            {
                Expression = Expr.Binary(BinaryOp.Add, col "a", Expr.Literal(Literal.Int 1L))
                Direction = SortDirection.Ascending
            }
        ]

    expectErrorKind ErrorKind.NameError "ORDER BY の非列キーが OK になっています" (Ops.sortBy schema2 op keys)

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
    let rows = ok (Op.toArray (ok (Ops.sortBy schema2 op keys)))
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
    let r1 = Ops.sortBy schema2 op2 keys1
    expectErrorKind ErrorKind.TypeError "比較不能なキーは Error になる" r1

[<Fact>]
let ``project: 項目が空なら入力をそのまま返す`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L ] ]
    let rows = ok (Op.toArray (Ops.project schema2 [] op []))
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])

[<Fact>]
let ``集約: GROUP BY 式の評価失敗は Error`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L; SqlValue.Int 2L ] ]

    expectErrorKind
        ErrorKind.NameError
        "存在しない列の GROUP BY が OK になっています"
        (Ops.aggregate schema2 op [ col "nope" ] [ (AggFunc.Count, true, false, Expr.Literal Literal.Null) ])

[<Fact>]
let ``集約: SUM / AVG の REAL と非数値`` () =
    // REAL は REAL のまま累積される
    let reals = opOfRows [ row [ SqlValue.Real 1.5 ]; row [ SqlValue.Real 2.5 ] ]

    let rows =
        ok (
            Op.toArray (
                ok (
                    Ops.aggregate
                        schema2
                        reals
                        []
                        [ AggFunc.Sum, false, false, col "a"; AggFunc.Avg, false, false, col "a" ]
                )
            )
        )

    Assert.Equal(4.0, asReal rows.[0].[0], 6)
    Assert.Equal(2.0, asReal rows.[0].[1], 6)

    // 数値でない値は Error になる（F15: 集約エラーは伝播する）
    let texts = opOfRows [ row [ SqlValue.Text "x" ] ]
    let r2 = Ops.aggregate schema2 texts [] [ (AggFunc.Sum, false, false, col "a") ]
    expectError "SUM(文字列) は Error になる" r2

    // AVG で全行 NULL は 0 除算せず NULL
    let nulls = opOfRows [ row [ SqlValue.Null ] ]

    let rows3 =
        ok (Op.toArray (ok (Ops.aggregate schema2 nulls [] [ (AggFunc.Avg, false, false, col "a") ])))

    Assert.True(SqlValue.isNull rows3.[0].[0])

[<Fact>]
let ``havingFilter: 集約値とグループ値を定数へ置き換える`` () =
    let groupBy = [ col "a" ]

    let aggs =
        [ AggFunc.Count, true, false, Expr.Star; AggFunc.Sum, false, false, col "b" ]
    // 行 = [a; COUNT(*); SUM(b)]
    let r = [| SqlValue.Int 7L; SqlValue.Int 3L; SqlValue.Int 10L |]

    let eval (pred: Expr) = Ops.havingFilter groupBy aggs pred r

    // 集約値（COUNT(*)）
    Assert.True(
        ok (
            eval (Expr.Binary(BinaryOp.Gt, Expr.AggCall(AggFunc.Count, false, Expr.Star), Expr.Literal(Literal.Int 2L)))
        )
    )

    // グループ値（列 → 先頭の値）
    Assert.True(ok (eval (Expr.Binary(BinaryOp.Eq, col "a", Expr.Literal(Literal.Int 7L)))))
    // 定数はそのまま
    Assert.True(ok (eval (Expr.Literal(Literal.Bool true))))
    // Name でも集約キーに一致すれば置き換える
    Assert.True(ok (eval (Expr.Binary(BinaryOp.Eq, Expr.Name "COUNT(*)", Expr.Literal(Literal.Int 3L)))))
    // 子を持つ式は再帰的に置き換える
    Assert.True(ok (eval (Expr.Binary(BinaryOp.And, Expr.Literal(Literal.Bool true), Expr.Literal(Literal.Bool true)))))
    // 集約リストに無い集約は NULL になる（比較は UNKNOWN → 偽）
    Assert.False(
        ok (eval (Expr.Binary(BinaryOp.Eq, Expr.AggCall(AggFunc.Min, false, col "b"), Expr.Literal(Literal.Int 99L))))
    )

    // 集約でもグループ値でもない Name / 列は置き換えられない（評価時にエラー）
    expectError "未知の Name が OK になっています" (eval (Expr.Name "zz"))
    expectError "未知の列が OK になっています" (eval (col "zz"))
    // スターは置き換えず、そのまま評価エラーになる
    expectError "WHERE の * が OK になっています" (eval Expr.Star)
    expectError "WHERE の t.* が OK になっています" (eval (Expr.QualifiedStar "t"))

[<Fact>]
let ``末尾再帰: 50000段の式を havingFilter してもスタックを食わない`` () =
    let depth = 50_000

    // テスト側も末尾再帰（fold）で組む（SQL 経由だと MaxExpressionDepth = 200 で頭打ちになる）
    let deep =
        List.init depth (fun _ -> Expr.Literal(Literal.Bool true))
        |> List.fold (fun acc e -> Expr.Binary(BinaryOp.And, acc, e)) (Expr.Literal(Literal.Bool true))

    let groupBy = [ col "a" ]
    let aggs = [ (AggFunc.Count, true, false, Expr.Star) ]
    // 行 = [a; COUNT(*)]
    let r = [| SqlValue.Int 7L; SqlValue.Int 3L |]
    Assert.True(ok (Ops.havingFilter groupBy aggs deep r))

[<Fact>]
let ``結合: INNER JOIN の ON 評価失敗は Error になる`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 1L |] |]

    let div0 =
        Expr.Binary(BinaryOp.Div, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L))

    let on = Expr.Binary(BinaryOp.Eq, div0, Expr.Literal(Literal.Int 0L))

    // 黙って空結果にせず、エラーとして呼び出し元へ返す。
    expectError "ON の評価失敗が握り潰されています" (Op.toArray (ok (Ops.innerJoin leftSchema left rightSchema right on)))

[<Fact>]
let ``結合: LEFT JOIN の ON 評価失敗も Error になる`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    // 一致した後にエラーへ当たっても、中途半端な行を返さずエラーにする。
    let div0 =
        Expr.Binary(BinaryOp.Div, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L))

    let on =
        Expr.Binary(
            BinaryOp.Or,
            Expr.Binary(BinaryOp.Eq, lcol, rcol),
            Expr.Binary(BinaryOp.Eq, div0, Expr.Literal(Literal.Int 0L))
        )

    expectError "ON の評価失敗が握り潰されています" (Op.toArray (ok (Ops.leftJoin leftSchema left rightSchema right on)))
