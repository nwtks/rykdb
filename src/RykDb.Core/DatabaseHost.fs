namespace RykDb.Core

[<RequireQualifiedAccess>]
type private SyncStep =
    | SyncWait
    | SyncDone
    | SyncLead of WalState
    | SyncFail of DbError

/// グループコミットの進行状態。
/// <remarks>
/// `gate` の内側だけで遷移する。
/// 「同期中なのに失敗している」のような組み合わせを作れないよう、
/// 同期済み・登録済みの位置も同じ DU にまとめる。
/// </remarks>
[<RequireQualifiedAccess>]
type private SyncState =
    /// 誰も fsync していない。`synced` は到達済みの位置、`pending` は登録済みの最大位置。
    | Idle of WalState option * WalState option
    /// 先頭の待機者が `target` まで fsync している。
    | Leading of WalState option * WalState * WalState option
    /// fsync に失敗した（以後の全待機者に同じエラーを返す）。
    | Failed of DbError

/// 複数の commit の WAL 位置をまとめ、リーダーが 1回だけ fsync する。
/// 先に登録された位置を後から登録された位置が包含する場合、後者だけを同期すればよい。
type internal CommitDurability(sync: WalState -> Result<unit, DbError>) =
    /// 状態を保護するロック。
    let gate = obj ()

    /// グループコミットの進行状態。
    let mutable state = SyncState.Idle(None, None)

    /// 到達済みの位置が pos を包含するか。
    let covers (pos: WalState) =
        function
        | Some(current: WalState) -> (current.FileNumber, current.Size) >= (pos.FileNumber, pos.Size)
        | None -> false

    /// 2つの WAL 位置のうち後ろを返す。
    let later (a: WalState) (b: WalState) =
        if (a.FileNumber, a.Size) >= (b.FileNumber, b.Size) then
            a
        else
            b

    /// 登録済みの最大位置と pos の後ろを返す。
    let laterPending (pos: WalState) =
        function
        | Some current -> later current pos
        | None -> pos

    /// 到達済みの位置と target の後ろを返す。
    let laterSynced (target: WalState) =
        function
        | Some current -> later current target
        | None -> target

    /// pos が fsync 済みになるまで待つ（先頭の待機者がリーダーとして同期する）。
    [<TailCall>]
    let rec awaitLoop (pos: WalState) =
        let step =
            lock gate (fun () ->
                match state with
                | SyncState.Failed e -> SyncStep.SyncFail e
                | SyncState.Idle(synced, _) when covers pos synced -> SyncStep.SyncDone
                | SyncState.Idle(synced, pending) ->
                    let target = laterPending pos pending
                    state <- SyncState.Leading(synced, target, pending)
                    SyncStep.SyncLead target
                | SyncState.Leading _ ->
                    System.Threading.Monitor.Wait gate |> ignore
                    SyncStep.SyncWait)

        match step with
        | SyncStep.SyncDone -> ret ()
        | SyncStep.SyncFail e -> err e
        | SyncStep.SyncWait -> awaitLoop pos
        | SyncStep.SyncLead target ->
            let result = sync target

            lock gate (fun () ->
                state <-
                    match result with
                    | Ok() ->
                        match state with
                        | SyncState.Leading(synced, _, pending) ->
                            SyncState.Idle(Some(laterSynced target synced), pending)
                        | current -> current
                    | Error e -> SyncState.Failed e

                System.Threading.Monitor.PulseAll gate)

            result

    /// 失敗していればそのエラーを返す。
    member _.Failure =
        lock gate (fun () ->
            match state with
            | SyncState.Failed e -> Some e
            | SyncState.Idle _
            | SyncState.Leading _ -> None)

    /// 登録済みの最大 WAL 位置（テスト・診断用）。
    member internal _.Pending =
        lock gate (fun () ->
            match state with
            | SyncState.Idle(_, pending)
            | SyncState.Leading(_, _, pending) -> pending
            | SyncState.Failed _ -> None)

    /// 同期対象の位置を登録する。
    member _.Register(pos: WalState) =
        lock gate (fun () ->
            match state with
            | SyncState.Idle(synced, pending) -> state <- SyncState.Idle(synced, Some(laterPending pos pending))
            | SyncState.Leading(synced, target, pending) ->
                state <- SyncState.Leading(synced, target, Some(laterPending pos pending))
            | SyncState.Failed _ -> ())

    /// 指定位置まで同期済みになるまで待つ。
    /// 最初の待機者がリーダーとなり、登録済みの最大位置まで同期する。
    member _.Await(pos: WalState) = awaitLoop pos

/// `DatabaseHost` のライフサイクル。
/// <remarks>
/// `Closed` は終端で、`Close` を実行するときに最後の state を読めるよう `Committed` を保持する。
/// 「閉じたか」と確定状態を別々の mutable にすると、Close 後の publish を作れてしまう。
/// </remarks>
[<RequireQualifiedAccess>]
type private HostLifecycle =
    | Running of Committed
    | Closed of Committed

/// COMMIT の結果。
/// <remarks>
/// write-write 競合や書き込みバッファの上限超過は、再試行しても commit できない。
/// `DatabaseHost.Commit` はその場合、その場で transaction を abort し、
/// 接続を transaction 外へ戻す。
/// エラーは値として返し、呼び出し側（`Session`）が接続状態を更新してからエラーとして返す。
/// WAL の fsync に失敗した場合も commit は**適用済み**（永続化だけ保証されない）。
/// その場合も接続を transaction 外へ戻し、`CommittedUnsynced` で適用済みであることを伝える。
/// 結果を捨てて Error だけ返すと、接続ローカルが「transaction 中」のまま host と分断する。
/// </remarks>
[<RequireQualifiedAccess>]
type internal CommitOutcome =
    | Committed of TxnState
    /// commit は適用済みだが、WAL の fsync に失敗した（永続化は保証されない）。
    | CommittedUnsynced of next: TxnState * error: DbError
    | RolledBack of next: TxnState * error: DbError

/// 同一 DB を共有する複数の `Session`（接続）のコミット状態。
/// <remarks>
/// 1つの `RowStore` と `TransactionManager` を全接続で共有し、
/// 各 `Session` は自分だけの transaction を持つ。
/// - 確定状態（`Committed`）はここが唯一の持ち主。
/// - 書き込み・BEGIN / COMMIT / ROLLBACK・DDL は `writeGate` で直列化する。
/// - 読み取りは `writeGate` を取らず、確定状態のスナップショットを見る。
/// - WAL の fsync はホスト単位で 1回にまとめる（`CommitDurability`）。
/// - ストアを開くのも閉じるのもこの型（`OpenWithBackground` / `Close`）。
/// - 閉じるのは所有者だけ（`SessionPool.Close` / 単独の `Session.Close`）。
/// </remarks>
module internal DatabaseHostHelpers =
    /// 放棄された transaction を manager から外すループ。
    let rec drainAbandoned
        (abandoned: System.Collections.Concurrent.ConcurrentQueue<TxnId>)
        (changed: bool)
        (manager: TransactionManager)
        =
        match abandoned.TryDequeue() with
        | true, txnId -> drainAbandoned abandoned true (TransactionManager.abortId txnId manager)
        | _ -> manager, changed

    /// BEGIN の本体（接続ローカルの状態を作る）。
    let beginRun
        (root: string)
        (abandoned: System.Collections.Concurrent.ConcurrentQueue<TxnId>)
        (publish: Committed -> unit)
        (current: Committed)
        =
        current.Store.Runtime.WithShared(fun () ->
            let mgr, txn = TransactionManager.beginTxn current.Manager

            let registration =
                current.Store.Runtime.Register(root, txn.Snapshot.SeqNo, ActiveRegistrationKind.Transaction)

            match Catalog.load current.Store with
            | Error e ->
                current.Store.Runtime.Unregister registration
                err e
            | Ok fresh ->
                let lease = RegistrationLease(registration, (fun () -> abandoned.Enqueue txn.TxnId))

                let next = { current with Manager = mgr }
                publish next

                let nextLocal: TxnState =
                    {
                        CatalogAtBegin = fresh
                        Active = Some txn
                        Pending = None
                        Lease = Some lease
                    }

                ret (next, (nextLocal, StatementResult.Empty)))

type internal DatabaseHost(root: string, state: Committed, ?commitDurability: CommitDurability) =
    /// ライフサイクル（確定状態と Close 状態）を保護する短いロック。
    let stateGate = obj ()

    /// 書き込み・transaction 操作・Close を直列化するロック。
    let writeGate = obj ()

    /// 放棄された接続の transaction の TxnId（finalizer が積む）。
    let abandoned = System.Collections.Concurrent.ConcurrentQueue<TxnId>()

    /// WAL のグループコミット。
    let durability =
        match commitDurability with
        | Some d -> d
        | None -> CommitDurability(fun pos -> Wal.sync root pos)

    /// ホストのライフサイクル（Running / Closed）。
    /// 確定状態は transaction を持たない（型で保証される）。
    let mutable lifecycle = HostLifecycle.Running { state with DeferDurability = true }

    /// 現在のライフサイクルを読む。
    let snapshot () = lock stateGate (fun () -> lifecycle)

    /// 確定状態と接続ローカルの transaction から実行状態を作る。
    let execStateOf (committed: Committed) (local: TxnState) =
        {
            Committed = committed
            Txn = local
            Registration = local.Lease |> Option.map _.Registration
            StatsDeltas = Map.empty
        }

    /// Close 後は何もしない（呼び出しはすべて writeGate の内側で Closed を確認済み）。
    let publish (state: Committed) =
        lock stateGate (fun () ->
            match lifecycle with
            | HostLifecycle.Running _ -> lifecycle <- HostLifecycle.Running state
            | HostLifecycle.Closed _ -> ())

    /// Close 後の操作のエラー。
    let closedError () = DbError.internalError "データベースは閉じられています"

    /// 放棄された接続の transaction を manager から外す（`writeGate` の内側で呼ぶ）。
    /// <remarks>
    /// 接続が Close されずに GC されるとその transaction は二度と commit できない。
    /// `Active` に残すと `activeSnapshot` が下がらず、`LastCommitted` がキーを保持し続ける。
    /// finalizer はキューに積むだけなので gate で止まらない。`abortId` は冪等。
    /// </remarks>
    let reclaimAbandoned (current: Committed) =
        match DatabaseHostHelpers.drainAbandoned abandoned false current.Manager with
        | manager, true ->
            let next = { current with Manager = manager }
            publish next
            next
        | _, false -> current

    /// writeGate の内側で実行し、WAL を進めたときだけ fsync を gate の外で待つ。
    /// <remarks>
    /// 戻り値は (run の値, 永続化エラー)。Some なら run の変更は適用済みで fsync だけが失敗している
    /// （以後の操作は `CommitDurability` の失敗状態でエラーになる）。
    /// run の結果を捨てて Error だけ返すと、接続状態が host と分断する
    /// （COMMIT 済みなのに「transaction 中」のまま残る）。
    /// BEGIN / ROLLBACK と失敗した文は WAL を進めないので待たない。
    /// </remarks>
    let serialized (run: Committed -> Result<Committed * 'a, DbError>) =
        let outcome, position =
            lock writeGate (fun () ->
                match snapshot () with
                | HostLifecycle.Closed _ -> err (closedError ()), None
                | HostLifecycle.Running current ->
                    let current = reclaimAbandoned current
                    let store, lease = RowStore.acquireSnapshot current.Store
                    let current = { current with Store = store }

                    try
                        let outcome, position =
                            match run current with
                            | Error e -> err e, None
                            | Ok(next, value) ->
                                let position =
                                    if next.Store.Engine.WalState = current.Store.Engine.WalState then
                                        None
                                    else
                                        next.Store.Engine.WalState

                                position |> Option.iter durability.Register
                                ret value, position

                        outcome, position
                    finally
                        lease.Dispose())

        match outcome, position with
        | Error e, _ -> err e
        | Ok value, None -> ret (value, None)
        | Ok value, Some pos ->
            match durability.Await pos with
            | Ok() -> ret (value, None)
            | Error e -> ret (value, Some e)

    /// 与えたストアで DB を開く。
    /// カタログ読込・DROP 後始末・background worker の開始まで行う。
    /// 失敗した場合はストアを閉じる。
    static member OpenWithStore(root: string, store: RowStore, ?commitDurability: CommitDurability) =
        match Catalog.load store with
        | Error e ->
            RowStore.close store |> ignore
            err e
        | Ok catalog ->
            let lastSeq = RowStore.lastSeqNo store

            let seeded =
                { TransactionManager.create () with
                    LastCommitSeqNo = lastSeq
                    NextSeqNo = lastSeq + 1UL
                }

            match seeded |> DropCleanupRecovery.recover store catalog with
            | Error e ->
                RowStore.close store |> ignore
                err e
            | Ok(store, mgr) ->
                let lastSeq = RowStore.lastSeqNo store

                let manager =
                    { mgr with
                        LastCommitSeqNo = lastSeq
                        NextSeqNo = lastSeq + 1UL
                    }

                RowStore.startBackground store

                new DatabaseHost(
                    root,
                    {
                        Root = root
                        Store = store
                        Catalog = catalog
                        Manager = manager
                        DeferDurability = true
                    },
                    ?commitDurability = commitDurability
                )
                |> ret

    /// background compaction を有効にして開く（公開 API の `Session` が使う）。
    /// 同じ root はプロセス内で 1つしか開けない。
    static member OpenWithBackground(root: string, ?commitDurability: CommitDurability) =
        result {
            let! store = RowStore.openStoreWithBackground root
            return! DatabaseHost.OpenWithStore(root, store, ?commitDurability = commitDurability)
        }

    /// background compaction を無効にして開く（同期 compaction）。
    static member OpenWith(root: string, ?commitDurability: CommitDurability) =
        result {
            let! store = RowStore.openStore root
            return! DatabaseHost.OpenWithStore(root, store, ?commitDurability = commitDurability)
        }

    /// 現在の committed state（Close 後は最後の state）。
    member _.Committed =
        lock stateGate (fun () ->
            match lifecycle with
            | HostLifecycle.Running state
            | HostLifecycle.Closed state -> state)

    /// 永続化（WAL の fsync）の失敗。失敗後は全接続がエラーを返す。
    member _.DurabilityFailure = durability.Failure

    /// 読み取り用の committed state と Version 全ファイルの pin を 1回で返す
    /// （返した lease を Dispose するまで GC から守る）。
    member _.AcquireReadSnapshot() =
        match snapshot () with
        | HostLifecycle.Closed _ -> err (closedError ())
        | HostLifecycle.Running state ->
            let store, lease = RowStore.acquireSnapshot state.Store

            let readState =
                if obj.ReferenceEquals(store, state.Store) then
                    state
                else
                    { state with Store = store }

            ret (readState, lease)

    /// 明示 transaction を始める（接続ローカルの状態を返す）。
    member _.Begin(local: TxnState) =
        if TxnState.inTransaction local then
            ret local
        else
            serialized (DatabaseHostHelpers.beginRun root abandoned publish)
            |> Result.map (fun ((nextLocal, _), _) -> nextLocal)

    /// COMMIT。接続の transaction を共有の Manager で確定する。
    /// <remarks>
    /// write-write 競合やバッファ上限超過で失敗した場合、
    /// その transaction は再試行しても commit できない
    /// （競合は同じ snapshot のままでは永久に競合し、上限は減らない）。
    /// そのため transaction を abort して接続を transaction 外へ戻し、
    /// エラーを `CommitOutcome.RolledBack` として返す。
    /// WAL の fsync に失敗した場合、commit は適用済みだが永続化は保証されない。
    /// 接続を transaction 外へ戻し、`CommitOutcome.CommittedUnsynced` として返す。
    /// それ以外の失敗（WAL の I/O など）では transaction を残す。
    /// </remarks>
    member _.Commit(local: TxnState) =
        match local.Active with
        | None -> err (DbError.internalError "コミットする transaction がありません")
        | Some _ ->
            serialized (fun current ->
                match Engine.executeCommit (execStateOf current local) with
                | Ok next ->
                    local.Lease |> Option.iter _.Release()
                    publish next.Committed
                    ret (next.Committed, (CommitOutcome.Committed next.Txn, StatementResult.Empty))
                | Error e when e.Kind = ErrorKind.ConflictError || e.Kind = ErrorKind.CapacityError ->
                    let rolledBack = Engine.executeRollback (execStateOf current local)
                    local.Lease |> Option.iter _.Release()
                    publish rolledBack.Committed
                    ret (rolledBack.Committed, (CommitOutcome.RolledBack(rolledBack.Txn, e), StatementResult.Empty))
                | Error e -> err e)
            >>= fun (outcome, result as value, durability) ->
                match durability with
                | None -> ret value
                | Some e ->
                    match outcome with
                    | CommitOutcome.Committed next ->
                        let unsynced =
                            DbError.create
                                e.Kind
                                (sprintf "COMMIT は適用されましたが、WAL の fsync に失敗しました（再起動すると失われる可能性があります）: %s" e.Message)

                        ret (CommitOutcome.CommittedUnsynced(next, unsynced), result)
                    | CommitOutcome.CommittedUnsynced _
                    | CommitOutcome.RolledBack _ -> ret value

    /// ROLLBACK。接続の transaction を捨てる。
    /// <remarks>
    /// transaction が無い場合は何もしない（成功）。
    /// COMMIT の競合で自動 rollback 済みでも、後始末の ROLLBACK を失敗させない。
    /// </remarks>
    member _.Rollback(local: TxnState) =
        match local.Active with
        | None -> ret (local, StatementResult.Empty)
        | Some _ ->
            serialized (fun current ->
                let next = Engine.executeRollback (execStateOf current local)
                local.Lease |> Option.iter _.Release()
                publish next.Committed
                ret (next.Committed, (next.Txn, StatementResult.Empty)))
            |> Result.map (fun (value, _) -> value)

    /// transaction 以外の文（SELECT / DML / DDL）を実行する。
    member _.ExecuteNonTransaction (local: TxnState) (limits: ResultLimits) (stmt: Statement) =
        serialized (fun current ->
            Engine.executeStatementWithLimits limits (execStateOf current local) stmt
            >>= fun (next, result) ->
                // 登録が残っている間だけ、接続が持つリースを引き継ぐ。
                let nextLocal =
                    { next.Txn with
                        Lease = if next.Registration.IsSome then local.Lease else None
                    }

                if next.Active.IsNone && next.Pending.IsNone then
                    publish next.Committed
                    ret (next.Committed, (nextLocal, result))
                else
                    // transaction 内: 共有するのは Manager の変化だけ
                    let committed = { current with Manager = next.Manager }
                    publish committed
                    ret (committed, (nextLocal, result)))
        >>= fun (value, durability) ->
            match durability with
            | None -> ret value
            | Some e -> err e

    /// DB 全体を閉じる（memtable を flush し、WAL を削除し、プロセスロックを解放する）。
    /// <remarks>
    /// 残っている transaction は manager から外してから閉じる
    /// （正常経路では `Session.Close` が先に rollback している）。
    /// </remarks>
    member _.Close() =
        let captured =
            lock writeGate (fun () ->
                match snapshot () with
                | HostLifecycle.Closed _ -> None
                | HostLifecycle.Running current ->
                    lock stateGate (fun () -> lifecycle <- HostLifecycle.Closed current)
                    Some current)

        match captured with
        | None -> ret ()
        | Some state ->
            let syncError =
                match state.Store.Engine.WalState with
                | Some pos ->
                    durability.Register pos

                    match durability.Await pos with
                    | Error e -> Some e
                    | Ok() -> None
                | None -> None

            let state =
                if Map.isEmpty state.Manager.Active then
                    state
                else
                    let mgr =
                        state.Manager.Active
                        |> Map.toList
                        |> List.fold (fun m (_, txn) -> TransactionManager.abort txn m) state.Manager

                    { state with Manager = mgr }

            let closeResult = RowStore.close state.Store

            match syncError with
            | Some e -> err e
            | None -> closeResult
