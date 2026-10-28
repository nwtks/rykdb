// ===========================================================================
// ExtSort.fs のテスト: 外部マージソート（溢れ・run・merge）
// ===========================================================================

module RykDb.ExtSortTests

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
let private col (name: string) =
    Expr.Column { Qualifier = None; Name = name }

[<Fact>]
let ``ExtSort: バジェットが小さいと一時ファイルに溢らして整列できる`` () =
    // 1行 ≈ 数十バイト。バジェット 1 ならほぼ毎行が run として溢れる。
    let n = 500

    let shuffled =
        [ 0 .. n - 1 ]
        |> List.map (fun i -> row [ SqlValue.Int(int64 ((i * 37 + 11) % n)); SqlValue.Int(int64 i) ])

    let cmp (a: SqlValue array) (b: SqlValue array) =
        match SqlValue.compareValues a.[0] b.[0] with
        | Ok c -> c
        | Error _ -> 0

    let sorted =
        ok (ExtSort.sortRows 1L Map.empty (fun _ _ -> ret Map.empty) cmp (opOfRows shuffled))

    let rows = ok (Op.toArray sorted)

    Assert.Equal<int>(n, rows.Length)

    for i in 1 .. n - 1 do
        Assert.True(asInt rows.[i - 1].[0] <= asInt rows.[i].[0], $"行 {i} が昇順ではありません")

    // 一時ファイルは残らない（run は尽きた時点で消える）。
    let leftovers =
        System.IO.Directory.GetFiles(System.IO.Path.GetTempPath(), "rykdb-sort-" + "*")

    Assert.All(leftovers, fun p -> Assert.Fail($"一時ファイルが残っています: {p}"))

[<Fact>]
let ``ExtSort: 比較不能な型は呼び出し時に Error`` () =
    let mixed =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 1L ]
                row [ SqlValue.Text "x"; SqlValue.Int 2L ]
            ]

    let keys =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
        ]

    expectErrorKind ErrorKind.TypeError "比較不能な型は Error" (Ops.sortBy schema2 mixed keys)

[<Fact>]
let ``ExtSort: 5000行を小さなバジェットで溢らしても整列できる`` () =
    // 1 行ずつ run に溢らしながら 5000行を流す。
    // ingest が末尾再帰でなければスタックを積み上がる（数百 run で枯渇する）。
    let n = 5_000

    let rec make (i: int) : Op =
        if i >= n then
            Op.Done
        else
            Op.Compute(fun () ->
                let v = int64 ((i * 7919 + 13) % n)
                ret (Some(row [ SqlValue.Int v; SqlValue.Int(int64 i) ], make (i + 1))))

    let cmp (a: SqlValue array) (b: SqlValue array) =
        match SqlValue.compareValues a.[0] b.[0] with
        | Ok c -> c
        | Error _ -> 0

    // バジェット 256バイト → 数百個の run が生まれる。
    let sorted =
        ok (ExtSort.sortRows 256L Map.empty (fun _ _ -> ret Map.empty) cmp (make 0))

    let rows = ok (Op.toArray sorted)

    Assert.Equal<int>(n, rows.Length)

    let ascending =
        rows
        |> Array.pairwise
        |> Array.forall (fun (a, b) -> asInt a.[0] <= asInt b.[0])

    Assert.True(ascending, "昇順に並んでいません")

    // 一時ファイルは残らない。
    let leftovers =
        System.IO.Directory.GetFiles(System.IO.Path.GetTempPath(), "rykdb-sort-*")

    Assert.Equal<int>(0, leftovers.Length)
