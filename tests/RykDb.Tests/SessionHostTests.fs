// ===========================================================================
// SessionHost.fs のテスト: グループコミット（CommitDurability）の fsync 統合
// ===========================================================================

module RykDb.SessionHostTests

open System.Threading
open System.Threading.Tasks
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private pos (fileNumber: int64) (size: int) : WalState =
    { FileNumber = fileNumber; Size = size }

[<Fact>]
let ``CommitDurability: 複数登録を 1回の fsync にまとめる`` () =
    let mutable syncs = 0
    let targets = ResizeArray<WalState>()

    let durable =
        CommitDurability(fun target ->
            syncs <- syncs + 1
            targets.Add target
            ret ())

    let first = pos 10L 100
    let second = pos 10L 200
    durable.Register first
    durable.Register second
    ok (durable.Await first)
    ok (durable.Await second)
    Assert.Equal(1, syncs)
    Assert.Equal<WalState>(second, targets.[0])

[<Fact>]
let ``CommitDurability: 後のファイルが同期済みなら古い位置は待たない`` () =
    let mutable syncs = 0

    let durable =
        CommitDurability(fun _ ->
            syncs <- syncs + 1
            ret ())

    let newer = pos 20L 10
    durable.Register newer
    ok (durable.Await newer)
    ok (durable.Await(pos 19L 1000))
    Assert.Equal(1, syncs)

[<Fact>]
let ``CommitDurability: fsync 失敗を全待機者に伝播し、再試行しない`` () =
    let mutable syncs = 0

    let durable =
        CommitDurability(fun _ ->
            syncs <- syncs + 1
            err (DbError.internalError "fsync failed"))

    durable.Register(pos 30L 10)
    expectError "最初の待機者がエラーになる" (durable.Await(pos 30L 10))
    expectError "後から来た待機者にもエラーを返す" (durable.Await(pos 30L 20))
    Assert.Equal(1, syncs)
    Assert.True durable.Failure.IsSome

[<Fact>]
let ``CommitDurability: 同時待機でも fsync は 1回だけ`` () =
    let mutable syncs = 0

    let durable =
        CommitDurability(fun _ ->
            Interlocked.Increment(&syncs) |> ignore
            ret ())

    let first = pos 40L 10
    let second = pos 40L 20
    durable.Register first
    durable.Register second

    let tasks =
        [|
            Task.Run(fun () -> durable.Await first)
            Task.Run(fun () -> durable.Await second)
        |]

    Task.WaitAll(tasks |> Array.map (fun t -> t :> Task))
    tasks |> Array.iter (fun t -> ok t.Result)
    Assert.Equal(1, syncs)
