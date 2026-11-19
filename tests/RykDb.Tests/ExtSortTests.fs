// ===========================================================================
// ExtSort.fs のテスト: 外部マージソート（溢れ・run・merge）
// ===========================================================================

module RykDb.ExtSortTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// スピルしない既定のバジェットで `Ops.sortBy` を呼ぶ。
module private UnlimitedOps =
    let sortBy = Ops.sortBy MemoryBudget.Unlimited

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

/// ソートの受け皿だけを指定したバジェット（スピルを強制する）。
let private sortBudget (bytes: int64) =
    { MemoryBudget.Default with
        Sort = bytes
    }

[<Fact>]
let ``ExtSort.sortRows: バジェットが小さいと一時ファイルに溢らして整列できる`` () =
    // 1行 ≈ 数十バイト。バジェット 1 ならほぼ毎行が run として溢れる。
    let n = 500

    let shuffled =
        [ 0 .. n - 1 ]
        |> List.map (fun i -> row [ SqlValue.Int(int64 ((i * 37 + 11) % n)); SqlValue.Int(int64 i) ])

    let cmp (a: SqlValue array) (b: SqlValue array) =
        match compareValues a.[0] b.[0] with
        | Ok c -> c
        | Error _ -> 0

    let sorted =
        ok (ExtSort.sortRows (sortBudget 1L) Map.empty (fun _ _ -> DbOk Map.empty) cmp (opOfRows shuffled))

    let rows = ok (Op.toArray sorted)
    Assert.Equal<int>(n, rows.Length)

    for i in 1 .. n - 1 do
        Assert.True(asInt rows.[i - 1].[0] <= asInt rows.[i].[0], $"行 {i} が昇順ではありません")

    // 一時ファイルは残らない（run は尽きた時点で消える）。
    let leftovers =
        System.IO.Directory.GetFiles(System.IO.Path.GetTempPath(), "rykdb-sort-" + "*")

    Assert.All(leftovers, fun p -> Assert.Fail $"一時ファイルが残っています: {p}")

[<Fact>]
let ``ExtSort.sortRows: 50000行を小さなバジェットで溢らしても整列できる`` () =
    // 1 行ずつ run に溢らしながら 50000行を流す。
    // ingest が末尾再帰でなければスタックを積み上がる（数百 run で枯渇する）。
    let n = 50_000

    let rec make (i: int) : Op =
        if i >= n then
            Op.Empty
        else
            Op.compute (fun () ->
                let v = int64 ((i * 7919 + 13) % n)
                OpStep.OpRow(row [ SqlValue.Int v; SqlValue.Int(int64 i) ], make (i + 1)))

    let cmp (a: SqlValue array) (b: SqlValue array) =
        match compareValues a.[0] b.[0] with
        | Ok c -> c
        | Error _ -> 0

    // バジェット 256バイト → 数百個の run が生まれる。
    // run は MaxFanIn（32）を超えるので、中間 run への multi-pass merge も通る。
    let sorted =
        ok (ExtSort.sortRows (sortBudget 256L) Map.empty (fun _ _ -> DbOk Map.empty) cmp (make 0))

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

[<Fact>]
let ``ExtSort.sortRows: スピル先を指定しても一時ファイルが残らない`` () =
    withDir "extsort-spill-dir" (fun dir ->
        let budget =
            { MemoryBudget.Default with
                Sort = 1L
                Directory = Some dir
            }

        let shuffled =
            [
                for i in 0..99 -> row [ SqlValue.Int(int64 ((i * 37 + 11) % 100)); SqlValue.Int(int64 i) ]
            ]

        let cmp (a: SqlValue array) (b: SqlValue array) =
            match compareValues a.[0] b.[0] with
            | Ok c -> c
            | Error _ -> 0

        let sorted =
            ok (ExtSort.sortRows budget Map.empty (fun _ _ -> DbOk Map.empty) cmp (opOfRows shuffled))

        let rows = ok (Op.toArray sorted)
        Assert.Equal<int>(100, rows.Length)

        let leftovers = System.IO.Directory.GetFiles dir

        if leftovers.Length > 0 then
            Assert.Fail(sprintf "一時ファイルが残っています: %A" (leftovers |> Array.map System.IO.Path.GetFileName)))

[<Fact>]
let ``ExtSort.sortRows: 2 本のスピル結果を交互に読める`` () =
    // 結合の代替経路（両側を外部ソートしてマージ結合）と同じ使い方。
    withDir "extsort-two" (fun dir ->
        let budget =
            { MemoryBudget.Default with
                Sort = 1L
                Directory = Some dir
            }

        let make (n: int) (step: int) =
            opOfRows [ for i in 0 .. n - 1 -> row [ SqlValue.Int(int64 ((i * step) % n)) ] ]

        let cmp (a: SqlValue array) (b: SqlValue array) =
            match compareValues a.[0] b.[0] with
            | Ok c -> c
            | Error _ -> 0

        let a = ok (ExtSort.sortRows budget () (fun () _ -> DbOk()) cmp (make 50 7))
        let b = ok (ExtSort.sortRows budget () (fun () _ -> DbOk()) cmp (make 40 3))

        // 2 本を交互に進める（片方だけを読み切らない）。
        let rec interleave (x: Op) (y: Op) acc =
            match Op.next x, Op.next y with
            | OpStep.OpRow(rx, nx), OpStep.OpRow(ry, ny) -> interleave nx ny (rx :: ry :: acc)
            | OpStep.OpRow(rx, nx), OpStep.OpDone -> interleave nx Op.Empty (rx :: acc)
            | OpStep.OpDone, OpStep.OpRow(ry, ny) -> interleave Op.Empty ny (ry :: acc)
            | OpStep.OpError e, _ -> failwith e.Message
            | _, OpStep.OpError e -> failwith e.Message
            | OpStep.OpDone, OpStep.OpDone -> List.rev acc

        let all = interleave a b []
        Assert.Equal<int>(90, all.Length)

        let leftovers = System.IO.Directory.GetFiles dir

        if leftovers.Length > 0 then
            Assert.Fail(sprintf "一時ファイルが残っています: %A" (leftovers |> Array.map System.IO.Path.GetFileName)))

[<Fact>]
let ``ExtSort.sortRows: 比較不能な型は呼び出し時に Error`` () =
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

    expectErrorKind ErrorKind.TypeError "比較不能な型は Error" (UnlimitedOps.sortBy schema2 keys mixed)
