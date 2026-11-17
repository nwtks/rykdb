// ===========================================================================
// SessionHost.fs のテスト: グループコミット（CommitDurability）の fsync 統合
// ===========================================================================

module RykDb.SessionHostTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private pos (fileNumber: int64) (size: int) : WalState =
    { FileNumber = fileNumber; Size = size }

/// 条件が満たされるまで待つ（満たされなければテストを失敗させる）。
let private waitFor (label: string) (predicate: unit -> bool) =
    let deadline = System.DateTime.UtcNow.AddSeconds 10.0

    let rec loop () =
        if predicate () then
            ()
        elif System.DateTime.UtcNow > deadline then
            Assert.Fail label
        else
            System.Threading.Thread.Sleep 1
            loop ()

    loop ()

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
            System.Threading.Interlocked.Increment(&syncs) |> ignore
            ret ())

    let first = pos 40L 10
    let second = pos 40L 20
    durable.Register first
    durable.Register second

    let tasks =
        [|
            System.Threading.Tasks.Task.Run(fun () -> durable.Await first)
            System.Threading.Tasks.Task.Run(fun () -> durable.Await second)
        |]

    System.Threading.Tasks.Task.WaitAll(tasks |> Array.map (fun t -> t :> System.Threading.Tasks.Task))
    tasks |> Array.iter (fun t -> ok t.Result)
    Assert.Equal(1, syncs)

[<Fact>]
let ``DatabaseHost: 同時 commit の WAL 位置を writeGate の内側で登録し fsync をまとめる`` () =
    withDir "group-commit" (fun root ->
        let syncs = ResizeArray<WalState>()
        let syncGate = obj ()
        let firstSyncEntered = new System.Threading.ManualResetEventSlim false
        let releaseFirst = new System.Threading.ManualResetEventSlim false
        let mutable blockFirst = false

        let durable =
            CommitDurability(fun target ->
                let count =
                    lock syncGate (fun () ->
                        syncs.Add target
                        syncs.Count)

                if blockFirst && count = 1 then
                    firstSyncEntered.Set()
                    releaseFirst.Wait()

                ret ())

        let host = ok (DatabaseHost.OpenWith(root, commitDurability = durable))

        let execute sql =
            host.ExecuteNonTransaction SessionState.none ResultLimits.Unlimited (parseOneOk sql)

        try
            ok (execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)") |> ignore
            lock syncGate (fun () -> syncs.Clear())
            blockFirst <- true

            let t1 =
                System.Threading.Tasks.Task.Run(fun () -> execute "INSERT INTO t VALUES (1, 1)")

            Assert.True(firstSyncEntered.Wait 10000, "最初の commit が fsync に入らない")

            let p1 =
                match durable.Pending with
                | Some p -> p
                | None -> failwith "最初の commit の WAL 位置が登録されていない"

            let t2 =
                System.Threading.Tasks.Task.Run(fun () -> execute "INSERT INTO t VALUES (2, 2)")

            waitFor "2番目の commit の WAL 位置が登録されない" (fun () ->
                match durable.Pending with
                | Some p -> (p.FileNumber, p.Size) > (p1.FileNumber, p1.Size)
                | None -> false)

            let p2 = durable.Pending.Value

            let t3 =
                System.Threading.Tasks.Task.Run(fun () -> execute "INSERT INTO t VALUES (3, 3)")

            waitFor "3番目の commit の WAL 位置が登録されない" (fun () ->
                match durable.Pending with
                | Some p -> (p.FileNumber, p.Size) > (p2.FileNumber, p2.Size)
                | None -> false)

            let p3 = durable.Pending.Value
            releaseFirst.Set()

            System.Threading.Tasks.Task.WaitAll
                [|
                    t1 :> System.Threading.Tasks.Task
                    t2 :> System.Threading.Tasks.Task
                    t3 :> System.Threading.Tasks.Task
                |]

            ok t1.Result |> ignore
            ok t2.Result |> ignore
            ok t3.Result |> ignore
            Assert.Equal(2, syncs.Count)

            let last = syncs.[syncs.Count - 1]
            Assert.True((last.FileNumber, last.Size) >= (p3.FileNumber, p3.Size), "最後の fsync が 3番目の WAL 位置まで届いていない")
        finally
            releaseFirst.Set()
            ok (host.Close()) |> ignore)
