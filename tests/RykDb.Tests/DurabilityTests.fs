// ===========================================================================
// グループコミットのテスト: 複数 commit の fsync 統合と失敗伝播。
// ===========================================================================

module RykDb.DurabilityTests

open System.Threading
open System.Threading.Tasks
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private pos (fileNumber: int64) (size: int) : WalState =
    { FileNumber = fileNumber; Size = size }

[<Fact>]
let ``Wal: deferred append の位置を fsync でき、削除済みファイルは同期済み扱い`` () =
    withDir "wal-deferred-sync" (fun dir ->
        let state = ok (Wal.create dir 1L)
        let state = ok (Wal.appendDeferred dir [ WalRecord.Commit 1UL<txnId> ] state)
        Assert.True(state.Size > 0)
        ok (Wal.sync dir state)
        Assert.True(System.IO.File.Exists(Wal.path dir 1L))
        ok (Wal.sync dir { state with FileNumber = 999L }))

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

[<Fact>]
let ``Session: 並行 commit を Close して再オープンしても読める`` () =
    withDir "session-group-commit" (fun dir ->
        let session = ok (Session.OpenWith dir)

        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        let tasks =
            [|
                for i in 1..16 do
                    Task.Run(fun () -> session.Execute(sprintf "INSERT INTO t VALUES (%d, %d)" i i))
            |]

        Task.WaitAll(tasks |> Array.map (fun t -> t :> Task))
        tasks |> Array.iter (fun t -> ok t.Result |> ignore)
        ok (session.Close())

        let reopened = ok (Session.OpenWith dir)

        try
            let selected = ok (reopened.Execute "SELECT id FROM t ORDER BY id")
            Assert.Equal<int>(16, selected.Rows.Length)
        finally
            reopened.Close() |> ignore)
