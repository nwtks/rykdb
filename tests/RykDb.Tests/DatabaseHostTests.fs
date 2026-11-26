// ===========================================================================
// DatabaseHost.fs のテスト: グループコミット（CommitDurability）と
// 共有 committed state（snapshot 登録・解除）
// ===========================================================================

module RykDb.DatabaseHostTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private pos (fileNumber: FileNumber) (size: int) : WalState =
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
let ``DatabaseHost: fsync 失敗の COMMIT は適用済みを CommittedUnsynced で返す`` () =
    withDir "commit-unsynced" (fun root ->
        let mutable failSync = false

        let durable =
            CommitDurability(fun _ ->
                if failSync then
                    err (DbError.io (System.IO.IOException "fsync failed"))
                else
                    ret ())

        let host = ok (DatabaseHost.OpenWith(root, commitDurability = durable))

        let execute local sql =
            host.ExecuteNonTransaction local ResultLimits.Unlimited (parseOneOk sql)

        try
            ok (execute TxnState.none "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
            |> ignore

            let local = ok (host.Begin TxnState.none)
            let local2, _ = ok (execute local "INSERT INTO t VALUES (1, 10)")
            failSync <- true

            // 修正前: commit は適用済みなのに結果が捨てられ、生の fsync エラーだけが返った。
            match host.Commit local2 with
            | Ok(CommitOutcome.CommittedUnsynced(next, e), _) ->
                Assert.Equal<ErrorKind>(ErrorKind.IoError, e.Kind)
                Assert.True(e.Message.Contains "適用されました", sprintf "メッセージが適用済みを伝えない: %s" e.Message)
                Assert.False(TxnState.inTransaction next)
            | other -> Assert.Fail(sprintf "CommittedUnsynced が返るはず: %A" other)

            // commit は適用済み: manager から transaction は消え、読み取りに見える。
            Assert.True(host.Committed.Manager.Active.IsEmpty)
            let _, found = ok (execute TxnState.none "SELECT id FROM t WHERE v = 10")
            Assert.Equal<int>(1, found.Rows.Length)
            Assert.True(host.DurabilityFailure.IsSome)
        finally
            host.Close() |> ignore)

[<Fact>]
let ``DatabaseHost: fsync 失敗後も BEGIN と ROLLBACK は成功する`` () =
    withDir "begin-after-fsync-fail" (fun root ->
        let mutable failSync = false

        let durable =
            CommitDurability(fun _ ->
                if failSync then
                    err (DbError.io (System.IO.IOException "fsync failed"))
                else
                    ret ())

        let host = ok (DatabaseHost.OpenWith(root, commitDurability = durable))

        let execute sql =
            host.ExecuteNonTransaction TxnState.none ResultLimits.Unlimited (parseOneOk sql)

        try
            ok (execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)") |> ignore
            failSync <- true
            // 暗黙 commit は適用済みだが、永続化に失敗するので Error。
            expectError "fsync 失敗後の書き込み" (execute "INSERT INTO t VALUES (1, 10)")
            Assert.True(host.DurabilityFailure.IsSome)

            // BEGIN / ROLLBACK は WAL を書かないので成功する
            // （修正前は fsync 待ちで失敗し、manager に ghost transaction が残った）。
            let local = ok (host.Begin TxnState.none)
            Assert.True(TxnState.inTransaction local)
            ok (host.Rollback local) |> ignore
            Assert.True(host.Committed.Manager.Active.IsEmpty)
        finally
            host.Close() |> ignore)

[<Fact>]
let ``Session: fsync 失敗の COMMIT は transaction を閉じてからエラーを返す`` () =
    withDir "session-commit-unsynced" (fun root ->
        let mutable failSync = false

        let durable =
            CommitDurability(fun _ ->
                if failSync then
                    err (DbError.io (System.IO.IOException "fsync failed"))
                else
                    ret ())

        let host = ok (DatabaseHost.OpenWith(root, commitDurability = durable))
        let session = Session.Create(host, SessionOptions.Default, true)

        try
            ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
            |> ignore

            ok (session.Begin()) |> ignore
            ok (session.Execute "INSERT INTO t VALUES (1, 10)") |> ignore
            failSync <- true

            match session.Commit() with
            | Error e -> Assert.Equal<ErrorKind>(ErrorKind.IoError, e.Kind)
            | Ok() -> Assert.Fail "fsync 失敗なのに COMMIT が成功した"

            // 修正前: 接続ローカルが更新されず「transaction 中」のまま host と分断した。
            Assert.False(session.InTransaction)
            Assert.True(host.DurabilityFailure.IsSome)
        finally
            session.Close() |> ignore)

[<Fact>]
let ``DatabaseHost: 上限を超えた transaction の COMMIT も自動で rollback する`` () =
    let root = "mem:" + System.Guid.NewGuid().ToString "N"
    let store = openStoreWith "" (memoryEngine ())

    let manager, txn =
        TransactionManager.beginTxnWithLimit (Some 1L) (TransactionManager.create ())

    let txn = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] txn

    let host =
        DatabaseHost(
            root,
            {
                Root = root
                Store = store
                Catalog = Catalog.Empty
                Manager = manager
                DeferDurability = true
            },
            commitDurability = CommitDurability(fun _ -> ret ())
        )

    try
        let local: TxnState =
            {
                CatalogAtBegin = Catalog.Empty
                Active = Some txn
                Pending = None
                Lease = None
            }

        // 上限は減らないので、何度 COMMIT しても成功しない。競合と同じく abort する。
        match host.Commit local with
        | Ok(CommitOutcome.RolledBack(_, e), _) -> Assert.Equal(ErrorKind.CapacityError, e.Kind)
        | Ok(CommitOutcome.Committed _, _) -> Assert.Fail "上限超過なのに commit できた"
        | Ok(CommitOutcome.CommittedUnsynced _, _) -> Assert.Fail "上限超過なのに commit できた"
        | Error e -> Assert.Fail(sprintf "COMMIT が Error を返した: %A" e.Kind)

        Assert.True(host.Committed.Manager.Active.IsEmpty)
    finally
        RowStore.close store |> ignore

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
let ``DatabaseHost: 放棄された transaction は次の書き込みで manager から回収される`` () =
    withDbHost "reg-abandon-reclaim" (fun host session ->
        Assert.True(abandonTransaction host)
        // BEGIN は manager の Active へ載る（snapshot registry とは別）
        Assert.Equal(1, host.Committed.Manager.Active.Count)

        collectGarbage ()
        // finalizer は snapshot 登録を外すが、manager の Active は次の書き込みまで残る。
        Assert.True((host.Committed.Store.Runtime.OldestExcept(host.Committed.Root, None)).IsNone)
        Assert.Equal(1, host.Committed.Manager.Active.Count)

        // 次の書き込み（暗黙 transaction）が回収し、放棄された transaction は commit できない。
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        Assert.True(host.Committed.Manager.Active.IsEmpty)
        Assert.Equal<int>(0, host.Committed.Manager.LastCommitted.Count)

        // 回収済みなので通常の書き込み・読み取りが続く。
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
let ``DatabaseHost: DDL のカタログ保存後も Manager の採番は保存済み SeqNo の後ろになる`` () =
    withDbHost "ddl-seq" (fun host session ->
        // テーブル + 暗黙 UNIQUE 索引でカタログに 2エントリ書く。
        let session = exec session "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT UNIQUE)"

        let committed = host.Committed
        let lastSeq = RowStore.lastSeqNo committed.Store

        // カタログは Transaction を介さず採番するが、Manager は保存済み SeqNo に追随する
        // （追随しないと次の commit がカタログと同じ SeqNo を再利用する）。
        Assert.True(lastSeq > 0UL)
        Assert.Equal<uint64>(lastSeq, committed.Manager.LastCommitSeqNo)
        Assert.Equal<uint64>(lastSeq + 1UL, committed.Manager.NextSeqNo)

        // データを書いても SeqNo は単調に増え、Manager と一致し続ける。
        let session = exec session "INSERT INTO t VALUES (1, 'a')"
        let committed = host.Committed
        let lastSeq2 = RowStore.lastSeqNo committed.Store

        Assert.True(lastSeq2 > lastSeq)
        Assert.Equal<uint64>(lastSeq2, committed.Manager.LastCommitSeqNo)
        Assert.Equal<uint64>(lastSeq2 + 1UL, committed.Manager.NextSeqNo)

        // 続けて DDL しても同じ関係が保たれる。
        let _ = exec session "CREATE TABLE u (id INTEGER PRIMARY KEY)"
        let committed = host.Committed
        let lastSeq3 = RowStore.lastSeqNo committed.Store

        Assert.Equal<uint64>(lastSeq3, committed.Manager.LastCommitSeqNo)
        Assert.Equal<uint64>(lastSeq3 + 1UL, committed.Manager.NextSeqNo))

[<Fact>]
let ``DatabaseHost: background compaction の install 後も書き込み文が削除済みファイルを踏まない`` () =
    withDir "host-bg-install" (fun dir ->
        let releaseCompaction = new System.Threading.ManualResetEventSlim false

        let store =
            ok (
                RowStore.openStoreWithOptions
                    { RowStoreOptions.Background with
                        MemLimitBytes = Some 1
                        ScanHook = Some(fun _ -> releaseCompaction.Wait 30000 |> ignore)
                    }
                    dir
            )

        let host = ok (DatabaseHost.OpenWithStore(dir, store))
        let session = Session.Create(host, SessionOptions.Default, true)

        try
            let mutable session = exec session "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"

            for i in 1..4 do
                session <- exec session (sprintf "INSERT INTO t VALUES (%d, 'v%d')" i i)

            let coordinator = expectSome "store.Host" store.Host

            let fileNumbers (version: Version) =
                Version.allFiles version |> List.map _.FileNumber |> Set.ofList

            // 最後の commit 時点の Version と、compaction が install した Version の差を待つ。
            // 旧ファイルがディスクから消えるまで待つのは、修正前のコードで確実に失敗させるため。
            let committedNumbers =
                fileNumbers host.Committed.Store.Engine.PublishedState.Version

            releaseCompaction.Set()

            waitFor "compaction が install されません" (fun () ->
                let removed =
                    Set.difference committedNumbers (fileNumbers coordinator.Current.Version)

                not (Set.isEmpty removed)
                && removed
                   |> Set.forall (fun n -> not (System.IO.File.Exists(FileMetaData.path dir n))))

            // 修正前は確定状態の store が最後の commit 時点の Version を指したまま
            // compaction が旧ファイルを削除していたため、UPDATE の走査が IoError になった。
            let _, result = ok (execSql session "UPDATE t SET v = 'x'")
            Assert.Equal(4, result.RowsAffected)
        finally
            releaseCompaction.Set()
            session.Close() |> ignore)

[<Fact>]
let ``DatabaseHost: 型は公開されていない`` () =
    let hostType = typeof<Session>.Assembly.GetType "RykDb.Core.DatabaseHost"
    Assert.NotNull hostType
    Assert.False hostType.IsPublic
