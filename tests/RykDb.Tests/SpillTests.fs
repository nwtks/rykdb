// ===========================================================================
// Spill.fs のテスト: 行の退避・読み戻し・パーティション
// ===========================================================================

module RykDb.SpillTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 上限だけを指定したバジェット（一時ファイルもこのディレクトリへ）。
let private budgetIn (dir: string) (bytes: int64) =
    { MemoryBudget.Default with
        Work = bytes
        Sort = bytes
        Directory = Some dir
    }

/// 退避に使った一時ファイルが残っていないことを確かめる。
let private assertNoLeftovers (dir: string) =
    let files = System.IO.Directory.GetFiles dir

    if files.Length > 0 then
        Assert.Fail(sprintf "一時ファイルが残っています: %A" (files |> Array.map System.IO.Path.GetFileName))

let private rowsOf (values: int list) =
    values |> List.map (fun i -> [| SqlValue.Int(int64 i) |])

let private intsOf (rows: SqlValue array array) =
    rows |> Array.map (fun r -> asInt r.[0])

[<Fact>]
let ``Spill.spool: 上限内はメモリに収まる`` () =
    withDir "spill-memory" (fun dir ->
        let spool =
            ok (Spill.spool (budgetIn dir (1024L * 1024L)) (Op.ofArray (rowsOf [ 1; 2; 3 ] |> Array.ofList)))

        Assert.False(Spill.isSpilled spool)
        Assert.Equal<int>(3, Spill.spoolCount spool)

        let rows = ok (Op.toArray (ok (Spill.reopen None spool)))
        Assert.Equal<int64 array>([| 1L; 2L; 3L |], intsOf rows)
        assertNoLeftovers dir)

[<Fact>]
let ``Spill.reopen: 手で作った run を読める`` () =
    withDir "spill-reopen-manual" (fun dir ->
        let path = Spill.runPath (Some dir) "manual" 0

        match Spill.openSink path with
        | Error e -> Assert.Fail $"書き込み口を開けません: {e.Message}"
        | Ok sink ->
            for i in [ 1; 2; 3 ] do
                match Spill.writeRow [| SqlValue.Int(int64 i) |] sink with
                | Error e -> Assert.Fail $"書き込めません: {e.Message}"
                | Ok() -> ()

            Spill.closeSink sink

        let spool = Spill.RowSpool.Disk([ path ], 3)
        Assert.True(Spill.isSpilled spool)

        let rows = ok (Op.toArray (ok (Spill.reopen None spool)))
        Assert.Equal<int>(3, rows.Length)
        Assert.Equal<int64>(1L, asInt rows.[0].[0])
        assertNoLeftovers dir)

[<Fact>]
let ``Spill.spool: 上限を超えると退避し、reopen で同じ順序で読める`` () =
    withDir "spill-disk" (fun dir ->
        let values = [ 1..100 ]

        let spool =
            ok (Spill.spool (budgetIn dir 1L) (Op.ofArray (rowsOf values |> Array.ofList)))

        Assert.True(Spill.isSpilled spool)
        Assert.Equal<int>(100, Spill.spoolCount spool)

        let rows = ok (Op.toArray (ok (Spill.reopen None spool)))
        Assert.Equal<int64 array>(values |> List.map int64 |> Array.ofList, intsOf rows)

        // reopen は尽きた run を消すので、最後はディレクトリが空になる。
        assertNoLeftovers dir)

[<Fact>]
let ``Spill.spool: reopenKeep は何度でも読める`` () =
    withDir "spill-keep" (fun dir ->
        let values = [ 1..50 ]

        let spool =
            ok (Spill.spool (budgetIn dir 1L) (Op.ofArray (rowsOf values |> Array.ofList)))

        // 3 回読んでも毎回同じ行が返る（JOIN の右側を左行ごとに走査する使い方）。
        for _ in 1..3 do
            let rows = ok (Op.toArray (ok (Spill.reopenKeep None spool)))
            Assert.Equal<int64 array>(values |> List.map int64 |> Array.ofList, intsOf rows)

        // 明示的に消すまでファイルは残る。
        Assert.NotEmpty(System.IO.Directory.GetFiles dir)
        Spill.deleteSpool spool
        assertNoLeftovers dir)

[<Fact>]
let ``Spill.spool: 列マスクの Unloaded も往復する`` () =
    withDir "spill-unloaded" (fun dir ->
        let rows = [| for i in 1..10 -> [| SqlValue.Int(int64 i); SqlValue.Unloaded |] |]

        let spool = ok (Spill.spool (budgetIn dir 1L) (Op.ofArray rows))
        Assert.True(Spill.isSpilled spool)

        let read = ok (Op.toArray (ok (Spill.reopen None spool)))
        Assert.Equal<int>(10, read.Length)
        Assert.Equal<int64>(1L, asInt read.[0].[0])

        Assert.True(
            match read.[0].[1] with
            | SqlValue.Unloaded -> true
            | _ -> false
        )

        assertNoLeftovers dir)

[<Fact>]
let ``Spill.foldRun: 終端でファイルを消す`` () =
    withDir "spill-fold" (fun dir ->
        let path = Spill.runPath (Some dir) "fold" 0

        match Spill.openSink path with
        | Error e -> Assert.Fail $"書き込み口を開けません: {e.Message}"
        | Ok sink ->
            rowsOf [ 1; 2; 3 ]
            |> List.iter (fun r ->
                match Spill.writeRow r sink with
                | Error e -> Assert.Fail $"書き込めません: {e.Message}"
                | Ok() -> ())

            Spill.closeSink sink

        let sum = ok (Spill.foldRun path 0L (fun acc row -> ret (acc + asInt row.[0])))
        Assert.Equal<int64>(6L, sum)
        assertNoLeftovers dir)

[<Fact>]
let ``Spill.partitionOfKey: 同じキーは同じパーティション、salt を変えると分け方が変わる`` () =
    let key = [| 1uy; 2uy; 3uy |]
    Assert.Equal<int>(Spill.partitionOfKey 0UL key, Spill.partitionOfKey 0UL [| 1uy; 2uy; 3uy |])

    let indices =
        [ for salt in 0UL .. 7UL -> Spill.partitionOfKey salt key ] |> List.distinct

    // 8種類の salt で全部同じパーティションに落ちるのは極端に低確率。
    Assert.True(indices.Length > 1, "salt を変えてもパーティションが変わっていません")
