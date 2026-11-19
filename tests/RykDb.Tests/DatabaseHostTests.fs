// ===========================================================================
// DatabaseHost.fs のテスト: グループコミット（CommitDurability）と
// 共有 committed state（snapshot 登録・解除）
// ===========================================================================

module RykDb.DatabaseHostTests

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

/// transaction を開始して Session を捨てる（呼び出し側は戻り値しか持たない）。
/// `NoInlining` は Debug ビルドのローカルがメソッド末尾まで生き残るのを避けるため。
/// 戻り値は「開始直後は確かに登録されていた」こと。
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private abandonTransaction (host: DatabaseHost) =
    let session = Session.Create(host, SessionOptions.Default, false)
    ok (session.Begin())
    // session はここを抜けると到達不能になる（RegistrationLease の finalizer が登録を外せる）。
    (host.Committed.Store.Runtime.OldestExcept(host.Committed.Root, None)).IsSome

/// finalizer を確実に走らせる。
let private collectGarbage () =
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()

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
            host.ExecuteNonTransaction TxnState.none ResultLimits.Unlimited (parseOneOk sql)

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

[<Fact>]
let ``DatabaseHost: active transaction は snapshot registry に登録され、commit / rollback / Close で外れる`` () =
    withDbHost "reg-lifecycle" (fun host session ->
        let runtime = host.Committed.Store.Runtime
        let root = host.Committed.Root
        Assert.True((runtime.OldestExcept(root, None)).IsNone)

        // 明示 transaction: Begin で登録、Commit で解除
        ok (session.Begin())
        Assert.True((runtime.OldestExcept(root, None)).IsSome)
        ok (session.Commit())
        Assert.True((runtime.OldestExcept(root, None)).IsNone)

        // Rollback でも解除
        ok (session.Begin())
        Assert.True((runtime.OldestExcept(root, None)).IsSome)
        ok (session.Rollback())
        Assert.True((runtime.OldestExcept(root, None)).IsNone)

        // SQL の BEGIN / COMMIT 経由でも同じ
        ok (session.Execute "BEGIN") |> ignore
        Assert.True((runtime.OldestExcept(root, None)).IsSome)
        ok (session.Execute "COMMIT") |> ignore
        Assert.True((runtime.OldestExcept(root, None)).IsNone)

        // 暗黙 transaction は Execute の中で登録・解除が完結する
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        Assert.True((runtime.OldestExcept(root, None)).IsNone)

        // Close でも解除される（閉じた接続の snapshot は誰も読めない）
        ok (session.Begin())
        Assert.True((runtime.OldestExcept(root, None)).IsSome)
        ok (session.Close())
        Assert.True((runtime.OldestExcept(root, None)).IsNone))

[<Fact>]
let ``DatabaseHost: commit せずに捨てた transaction も GC されれば登録が外れる`` () =
    withDbHost "reg-abandon" (fun host session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        Assert.True(abandonTransaction host)

        collectGarbage ()
        // 最後の Session が消えたので登録が外れる（修正前はここに残り続けた）。
        Assert.True((host.Committed.Store.Runtime.OldestExcept(host.Committed.Root, None)).IsNone)

        // 登録が残っていると ConflictError になる DDL が通る。
        ok (session.Execute "CREATE INDEX ix_v ON t (v)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1, 10)") |> ignore
        let _, found = ok (execSql session "SELECT id FROM t WHERE v = 10")
        Assert.Equal<int>(1, found.Rows.Length))

[<Fact>]
let ``DatabaseHost: 生きている transaction は GC されても登録が残る`` () =
    withDbHost "reg-live-gc" (fun host session ->
        ok (session.Begin())
        // session はこのあと Rollback で使うので GC されない（finalizer は走らない）。
        collectGarbage ()
        Assert.True((host.Committed.Store.Runtime.OldestExcept(host.Committed.Root, None)).IsSome)

        ok (session.Rollback())
        Assert.True((host.Committed.Store.Runtime.OldestExcept(host.Committed.Root, None)).IsNone))

[<Fact>]
let ``DatabaseHost: Dispose でも transaction の登録が外れる`` () =
    withDbHost "reg-dispose" (fun host session ->
        let runtime = host.Committed.Store.Runtime
        let root = host.Committed.Root
        ok (session.Begin())
        Assert.True((runtime.OldestExcept(root, None)).IsSome)

        (session :> System.IDisposable).Dispose()
        Assert.True((runtime.OldestExcept(root, None)).IsNone))

[<Fact>]
let ``DatabaseHost: 型は公開されていない`` () =
    let hostType = typeof<Session>.Assembly.GetType "RykDb.Core.DatabaseHost"
    Assert.NotNull hostType
    Assert.False hostType.IsPublic
