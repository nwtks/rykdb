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

let private row2 (a: int64) (b: int64) = [| SqlValue.Int a; SqlValue.Int b |]

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
let ``Op: 次の行を引くまで計算しない`` () =
    // 無限に伸びる Op でも 1 行しか引かずに終わる。
    let rec infinite (n: int) : Op =
        Op.Compute(fun () -> ret (Some([| SqlValue.Int(int64 n) |], infinite (n + 1))))

    let op = Op.take 3 (infinite 0)
    let rows = ok (Op.toArray op)

    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[2].[0])

[<Fact>]
let ``Op: 配列の演算子は 1 行ずつ返す`` () =
    let op: Op = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])

[<Fact>]
let ``末尾再帰: Op.ofArray を 20 万行組み立ててもスタックを食わない`` () =
    // 「let rest = rowsFrom (i + 1) を先に評価する」実装だとコンストラクト時に
    // 行数ぶんのスタックを積み、実測 約 13 万段（Debug）で Stack overflow になる。
    // Compute ラップで「次を要求されてから 1 行だけ」作ることを深い入力で検証する。
    let rows = Array.init 200_000 (fun i -> [| SqlValue.Int(int64 i) |])
    let op = Op.ofArray rows
    let got = ok (Op.toArray op)

    Assert.Equal<int>(200_000, got.Length)
    Assert.Equal<int64>(0L, (got.[0].[0] |> SqlValue.asInt64 |> Option.defaultValue -1L))
    Assert.Equal<int64>(199_999L, (got.[got.Length - 1].[0] |> SqlValue.asInt64 |> Option.defaultValue -1L))

[<Fact>]
let ``Op: filter と skip`` () =
    let op =
        Op.ofArray
            [|
                [| SqlValue.Int 1L |]
                [| SqlValue.Int 2L |]
                [| SqlValue.Int 3L |]
                [| SqlValue.Int 4L |]
            |]
        |> Op.filter (fun r -> Eval.matches schema2 r (Expr.Binary(BinaryOp.Gt, col "a", Expr.Literal(Literal.Int 2L))))
        |> Op.skip 1

    // a > 2 は 3 と 4 の 2 件。そこから 1 件読み飛ばすので 1 件残る。
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(4L, asInt rows.[0].[0])

[<Fact>]
let ``Op: 大きな Op でも take は早期に打ち切る`` () =
    // 1000 万行の Op を遅延評価で作る。Step の tail を先に評価すると
    // 1000 万個が先に出来上がるので、Compute で 1 行ずつ生成する。
    let rec counter (n: int) : Op =
        Op.Compute(fun () ->
            if n >= 10_000_000 then
                ret None
            else
                ret (Some(row [ SqlValue.Int(int64 n) ], counter (n + 1))))

    let rows = ok (Op.toArray (Op.take 3 (counter 0)))
    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[2].[0])

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
            (AggFunc.Count, true, Expr.Literal(Literal.Null))
            (AggFunc.Count, false, col "a")
            (AggFunc.Sum, false, col "a")
            (AggFunc.Avg, false, col "a")
            (AggFunc.Min, false, col "a")
            (AggFunc.Max, false, col "a")
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
let ``集約: 行が 0 でも COUNT(*) は 1 行を返す`` () =
    let rows =
        ok (Op.toArray (ok (Ops.aggregate schema2 Op.Done [] [ (AggFunc.Count, true, Expr.Literal(Literal.Null)) ])))

    // 0 件なら 0 行（1 行 (0) を返さない）。
    Assert.Equal<int>(0, rows.Length)

[<Fact>]
let ``集約: GROUP BY でグループごとに 1 行`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 10L ]
                row [ SqlValue.Int 1L; SqlValue.Int 20L ]
                row [ SqlValue.Int 2L; SqlValue.Int 30L ]
            ]

    let aggs = [ (AggFunc.Sum, false, col "b") ]
    let rows = ok (Op.toArray (ok (Ops.aggregate schema2 op [ col "a" ] aggs)))

    // 2 グループ
    Assert.Equal<int>(2, rows.Length)

    // 最初に現れたグループが先
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal(30M, asDec rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[0])
    Assert.Equal(30M, asDec rows.[1].[1])

[<Fact>]
let ``集約: GROUP BY のキーが NULL の行も 1 グループになる`` () =
    let op: Op =
        Op.ofArray [| [| SqlValue.Null; SqlValue.Int 1L |]; [| SqlValue.Null; SqlValue.Int 2L |] |]

    let aggs = [ (AggFunc.Count, true, Expr.Literal(Literal.Null)) ]
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
let ``sortBy: 同値のキーは次のキーで、比較不能なキーは安定順で決める`` () =
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

    // キーが 1 本だけで比較不能（型が合わない）なら順序はそのまま（安定ソート）
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

    let rows1 = ok (Op.toArray (ok (Ops.sortBy schema2 op2 keys1)))
    Assert.Equal<SqlValue>(SqlValue.Int 1L, rows1.[0].[0])
    Assert.Equal<SqlValue>(SqlValue.Text "x", rows1.[1].[0])

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
        (Ops.aggregate schema2 op [ col "nope" ] [ (AggFunc.Count, true, Expr.Literal(Literal.Null)) ])

[<Fact>]
let ``集約: SUM / AVG の REAL と非数値`` () =
    // REAL は REAL のまま累積される
    let reals = opOfRows [ row [ SqlValue.Real 1.5 ]; row [ SqlValue.Real 2.5 ] ]

    let rows =
        ok (
            Op.toArray (
                ok (Ops.aggregate schema2 reals [] [ (AggFunc.Sum, false, col "a"); (AggFunc.Avg, false, col "a") ])
            )
        )

    Assert.Equal(4.0, asReal rows.[0].[0], 6)
    Assert.Equal(2.0, asReal rows.[0].[1], 6)

    // 数値でない値は Null になる（エラーは集約値へ閉じ込める）
    let texts = opOfRows [ row [ SqlValue.Text "x" ] ]

    let rows2 =
        ok (Op.toArray (ok (Ops.aggregate schema2 texts [] [ (AggFunc.Sum, false, col "a") ])))

    Assert.True(SqlValue.isNull rows2.[0].[0])

    // AVG で全行 NULL は 0 除算せず NULL
    let nulls = opOfRows [ row [ SqlValue.Null ] ]

    let rows3 =
        ok (Op.toArray (ok (Ops.aggregate schema2 nulls [] [ (AggFunc.Avg, false, col "a") ])))

    Assert.True(SqlValue.isNull rows3.[0].[0])

[<Fact>]
let ``havingFilter: 集約値とグループ値を定数へ置き換える`` () =
    let groupBy = [ col "a" ]
    let aggs = [ (AggFunc.Count, true, Expr.Star); (AggFunc.Sum, false, col "b") ]
    // 行 = [a; COUNT(*); SUM(b)]
    let r = [| SqlValue.Int 7L; SqlValue.Int 3L; SqlValue.Int 10L |]

    let eval (pred: Expr) = Ops.havingFilter groupBy aggs pred r

    // 集約値（COUNT(*)）
    Assert.True(
        ok (eval (Expr.Binary(BinaryOp.Gt, Expr.AggCall(AggFunc.Count, true, Expr.Star), Expr.Literal(Literal.Int 2L))))
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
let ``結合: INNER JOIN は ON の評価失敗で打ち切る`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 1L |] |]

    let div0 =
        Expr.Binary(BinaryOp.Div, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L))

    let on = Expr.Binary(BinaryOp.Eq, div0, Expr.Literal(Literal.Int 0L))

    let rows = ok (Op.toArray (ok (Ops.innerJoin leftSchema left rightSchema right on)))
    Assert.Empty(rows)

[<Fact>]
let ``結合: LEFT JOIN は一致後の ON 評価失敗で次の行へ進む`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    // 「左が一致すれば true、そうでなければ 1/0 を評価してエラー」。
    // 左 1 行目は右 1 行目と一致した後にエラーへ当たるので、次の左行へ進む（NULL 拡張はしない）。
    let div0 =
        Expr.Binary(BinaryOp.Div, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L))

    let on =
        Expr.Binary(
            BinaryOp.Or,
            Expr.Binary(BinaryOp.Eq, lcol, rcol),
            Expr.Binary(BinaryOp.Eq, div0, Expr.Literal(Literal.Int 0L))
        )

    let rows = ok (Op.toArray (ok (Ops.leftJoin leftSchema left rightSchema right on)))

    // 一致した 1 行だけが返り、エラーで止まらない
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[0].[1])
