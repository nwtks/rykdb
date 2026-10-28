// ===========================================================================
// Op.fs のテスト: Op 演算子（pull / map / filter / take / skip / toArray）
// ===========================================================================

module RykDb.OpTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 2列（a, b）のスキーマ。
let private schema2 =
    outSchema
        [
            outCol "a" None (Some DataType.Integer)
            outCol "b" None (Some DataType.Integer)
        ]

/// 1行を作る。
let private row (values: SqlValue list) = List.toArray values

let private col (name: string) =
    Expr.Column { Qualifier = None; Name = name }

[<Fact>]
let ``Op: 次の行を引くまで計算しない`` () =
    // 無限に伸びる Op でも 1行しか引かずに終わる。
    let rec infinite (n: int) : Op =
        Op.Compute(fun () -> ret (Some([| SqlValue.Int(int64 n) |], infinite (n + 1))))

    let op = Op.take 3 (infinite 0)
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[2].[0])

[<Fact>]
let ``Op: 配列の演算子は 1行ずつ返す`` () =
    let op: Op = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])

[<Fact>]
let ``末尾再帰: Op.ofArray を 200000行組み立ててもスタックを食わない`` () =
    // 「let rest = rowsFrom (i + 1) を先に評価する」実装だとコンストラクト時に
    // 行数ぶんのスタックを積み、実測 約 130000段（Debug）で Stack overflow になる。
    // Compute ラップで「次を要求されてから 1行だけ」作ることを深い入力で検証する。
    let rows = Array.init 200_000 (fun i -> [| SqlValue.Int(int64 i) |])
    let op = Op.ofArray rows
    let got = ok (Op.toArray op)
    Assert.Equal<int>(200_000, got.Length)

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

    // a > 2 は 3 と 4 の 2件。そこから 1件読み飛ばすので 1件残る。
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(4L, asInt rows.[0].[0])

[<Fact>]
let ``Op: 大きな Op でも take は早期に打ち切る`` () =
    // 1000000行の Op を遅延評価で作る。Step の tail を先に評価すると
    // 1000000個が先に出来上がるので、Compute で 1行ずつ生成する。
    let rec counter n =
        Op.Compute(fun () ->
            if n >= 1_000_000 then
                ret None
            else
                ret (Some(row [ SqlValue.Int(int64 n) ], counter (n + 1))))

    let rows = ok (Op.toArray (Op.take 3 (counter 0)))
    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[2].[0])
