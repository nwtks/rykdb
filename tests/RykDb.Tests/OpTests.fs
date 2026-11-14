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

let private matches = Eval.resolver >> Eval.matchesWith

[<Fact>]
let ``Op: 次の行を引くまで計算しない`` () =
    // 無限に伸びる Op でも 1行しか引かずに終わる。
    let rec infinite (n: int) : Op =
        Op.compute (fun () -> ret (Some([| SqlValue.Int(int64 n) |], infinite (n + 1))))

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
let ``Op: emit / empty / ofRow は 1行ずつ返す`` () =
    let op = Op.emit (row [ SqlValue.Int 7L ]) (Op.ofRow (row [ SqlValue.Int 8L ]))
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(7L, asInt rows.[0].[0])
    Assert.Equal<int64>(8L, asInt rows.[1].[0])

[<Fact>]
let ``Op: 純粋なソースでも同じ値を 2 回 next すると先頭行が再び返る（継続を使う理由）`` () =
    let src = Op.ofArray [| row [ SqlValue.Int 1L ]; row [ SqlValue.Int 2L ] |]
    let first = ok (Op.next src)
    let again = ok (Op.next src)

    match first, again with
    | Some(f, nxt), Some(a, _) ->
        // 同じ値を 2 回 next すると先頭行が再び返る（one-shot 違反）。
        Assert.Equal<SqlValue array>(f, a)
        // 継続を使えば残りは 1行だけ。
        Assert.Equal<int>(1, (ok (Op.toArray nxt)).Length)
    | _ -> Assert.Fail "2 行あるはず"

[<Fact>]
let ``Op: one-shot ガードは同じ値の 2 回目の next を検出する`` () =
    let op = oneShot "guard" (Op.ofRow (row [ SqlValue.Int 1L ]))
    ok (Op.next op) |> ignore

    let mutable detected = false

    try
        Op.next op |> ignore
    with _ ->
        detected <- true

    Assert.True detected

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
        |> Op.filter (fun r -> matches schema2 r (Expr.Binary(BinaryOp.Gt, col "a", Expr.Literal(Literal.Int 2L))))
        |> Op.skip 1

    // a > 2 は 3 と 4 の 2件。そこから 1件読み飛ばすので 1件残る。
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(4L, asInt rows.[0].[0])

[<Fact>]
let ``Op: map / filter / take / skip は入力を 1 回だけ消費する`` () =
    let source =
        oneShot "source" (Op.ofArray [| row [ SqlValue.Int 1L ]; row [ SqlValue.Int 2L ]; row [ SqlValue.Int 3L ] |])

    let mapped = oneShot "map" (Op.map (fun r -> ret r) source)

    let filtered =
        oneShot "filter" (Op.filter (fun r -> ret (asInt r.[0] >= 2L)) mapped)

    let skipped = oneShot "skip" (Op.skip 1 filtered)
    let op = oneShot "take" (Op.take 1 skipped)
    let rows = ok (Op.toArray op)
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(3L, asInt rows.[0].[0])

[<Fact>]
let ``Op: pull はソースを 1 回だけ消費する`` () =
    let src =
        oneShot "pull" (Op.ofArray [| row [ SqlValue.Int 1L ]; row [ SqlValue.Int 2L ] |])

    let out = Op.pull (fun r nxt -> ret (Some(r, nxt))) src
    let rows = ok (Op.toArray out)
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[1].[0])

[<Fact>]
let ``Op: 大きな Op でも take は早期に打ち切る`` () =
    // 1000000行の Op を遅延評価で作る。Step の tail を先に評価すると
    // 1000000個が先に出来上がるので、Compute で 1行ずつ生成する。
    let rec counter n =
        Op.compute (fun () ->
            if n >= 1_000_000 then
                ret None
            else
                ret (Some(row [ SqlValue.Int(int64 n) ], counter (n + 1))))

    let rows = ok (Op.toArray (Op.take 3 (counter 0)))
    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[2].[0])

[<Fact>]
let ``Op.toArrayLimited: 行数上限ちょうどは成功し、1行超過は CapacityError`` () =
    let op =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let rows = ok (Op.toArrayLimited 3L System.Int64.MaxValue op)
    Assert.Equal<int>(3, rows.Length)
    expectErrorKind ErrorKind.CapacityError "行数上限を超えたら CapacityError" (Op.toArrayLimited 2L System.Int64.MaxValue op)

[<Fact>]
let ``Op.toArrayLimited: バイト上限ちょうどは成功し、1バイト超過は CapacityError`` () =
    let source = [| [| SqlValue.Int 1L; SqlValue.Text "abc" |] |]
    let rowBytes = int64 (ValueCodec.managedRowBytes source.[0])
    Assert.True(rowBytes > 0L)
    let op = Op.ofArray source
    let rows = ok (Op.toArrayLimited 1L rowBytes op)
    Assert.Equal<int>(1, rows.Length)
    expectErrorKind ErrorKind.CapacityError "バイト上限を超えたら CapacityError" (Op.toArrayLimited 1L (rowBytes - 1L) op)

[<Fact>]
let ``Op.toArray: 呼び出しごとに独立した配列を返す`` () =
    // 部分適用した toArray が ResizeArray を共有すると、2回目に前回の行が混ざる。
    let op = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let first = ok (Op.toArray op)
    let second = ok (Op.toArray op)
    Assert.Equal<int>(2, first.Length)
    Assert.Equal<int>(2, second.Length)
    Assert.NotSame(first, second)
