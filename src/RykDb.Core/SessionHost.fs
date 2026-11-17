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
    let gate = obj ()
    let mutable state = SyncState.Idle(None, None)

    let covers (pos: WalState) =
        function
        | Some(current: WalState) -> (current.FileNumber, current.Size) >= (pos.FileNumber, pos.Size)
        | None -> false

    let later (a: WalState) (b: WalState) =
        if (a.FileNumber, a.Size) >= (b.FileNumber, b.Size) then
            a
        else
            b

    let laterPending (pos: WalState) =
        function
        | Some current -> later current pos
        | None -> pos

    let laterSynced (target: WalState) =
        function
        | Some current -> later current target
        | None -> target

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

/// 接続ローカルの transaction 状態。
/// <remarks>
/// committed な Store / Catalog / Manager は `DatabaseHost` が共有し、
/// ここには接続だけが持つ transaction を置く。
/// Snapshot.SeqNo は BEGIN 時に固定されるので、
/// 実行のたびに最新の Store へ載せ替えても可視性（repeatable read）は変わらない。
/// </remarks>
type internal SessionState =
    {
        /// BEGIN 時点のカタログ。transaction 中は DDL が拒否されるので変化しない。
        Catalog: Catalog
        /// 明示 transaction。
        Active: Transaction option
        /// 未 commit の変更を積んだ transaction。
        Pending: Transaction option
        /// 進行中 transaction の ActiveSnapshot 登録。
        Registration: ActiveRegistration option
    }

module internal SessionState =
    /// transaction を持たない状態。
    let none: SessionState =
        {
            Catalog = Catalog.Empty
            Active = None
            Pending = None
            Registration = None
        }

    let inTransaction (state: SessionState) = state.Active.IsSome

    let ofState (state: DbState) =
        {
            Catalog = state.Catalog
            Active = state.Active
            Pending = state.Pending
            Registration = state.ActiveRegistration
        }

    /// 共有の committed state に接続ローカルの transaction を重ねる。
    /// transaction 外では committed state をそのまま使う。
    let apply (local: SessionState) (committed: DbState) =
        match local.Active with
        | None -> committed
        | Some _ ->
            { committed with
                Catalog = local.Catalog
                Active = local.Active
                Pending = local.Pending
                ActiveRegistration = local.Registration
            }

/// `DatabaseHost` のライフサイクル。
/// <remarks>
/// `Closed` は終端で、`Close` を実行するときに最後の state を読めるよう `DbState` を保持する。
/// 「閉じたか」と committed state を別々の mutable にすると、Close 後の publish を作れてしまう。
/// </remarks>
[<RequireQualifiedAccess>]
type private HostLifecycle =
    | Running of DbState
    | Closed of DbState

/// 同一 DB を共有する複数の `Session`（接続）のコミット状態。
/// <remarks>
/// 1つの `RowStore` と `TransactionManager` を全接続で共有し、
/// 各 `Session` は自分だけの transaction を持つ。
/// - committed な `DbState`（Store / Catalog / Manager）はここが唯一の持ち主。
/// - 書き込み・BEGIN / COMMIT / ROLLBACK・DDL は `writeGate` で直列化する。
/// - 読み取りは `writeGate` を取らず、committed state のスナップショットを見る。
/// - WAL の fsync はホスト単位で 1回にまとめる（`CommitDurability`）。
/// - 閉じるのは所有者だけ（`SessionPool.Close` / 単独の `Session.Close`）。
/// </remarks>
type internal DatabaseHost(root: string, owner: Database, ?commitDurability: CommitDurability) =
    // ライフサイクル（committed state と Close 状態）を保護する短いロック。
    let stateGate = obj ()
    // 書き込み・transaction 操作・Close を直列化するロック。
    let writeGate = obj ()

    let durability =
        match commitDurability with
        | Some d -> d
        | None -> CommitDurability(fun pos -> Wal.sync root pos)

    // committed state は常に Active / Pending / ActiveRegistration を持たない。
    let mutable lifecycle = HostLifecycle.Running owner.StateWithDeferredDurability

    let snapshot () = lock stateGate (fun () -> lifecycle)

    /// Close 後は何もしない（呼び出しはすべて writeGate の内側で Closed を確認済み）。
    let publish (state: DbState) =
        lock stateGate (fun () ->
            match lifecycle with
            | HostLifecycle.Running _ -> lifecycle <- HostLifecycle.Running state
            | HostLifecycle.Closed _ -> ())

    let closedError () = DbError.internalError "データベースは閉じられています"

    /// writeGate の内側で実行し、WAL の fsync を gate の外で待つ。
    /// `run` は「実行後の共有状態」と値も返す（読み取りだけなら入力をそのまま返す）。
    let serialized (run: DbState -> Result<DbState * 'a, DbError>) =
        let outcome, position =
            lock writeGate (fun () ->
                match snapshot () with
                | HostLifecycle.Closed _ -> Error(closedError ()), None
                | HostLifecycle.Running current ->
                    let outcome, position =
                        match run current with
                        | Error e -> Error e, current.Store.Engine.WalState
                        | Ok(next, value) -> Ok value, next.Store.Engine.WalState

                    position |> Option.iter durability.Register
                    outcome, position)

        match position with
        | Some pos ->
            match durability.Await pos with
            | Error e -> err e
            | Ok() -> outcome
        | None -> outcome

    /// 現在の committed state（Close 後は最後の state）。
    member _.Committed =
        lock stateGate (fun () ->
            match lifecycle with
            | HostLifecycle.Running state
            | HostLifecycle.Closed state -> state)

    /// 永続化（WAL の fsync）の失敗。失敗後は全接続がエラーを返す。
    member _.DurabilityFailure = durability.Failure

    /// 読み取り用の committed state と、GC から守る lease を返す。
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
    member _.Begin(local: SessionState) =
        if SessionState.inTransaction local then
            ret local
        else
            let run (current: DbState) =
                RootGate.withShared root (fun () ->
                    let mgr, txn = TransactionManager.beginTxn current.Manager
                    let registration = ActiveSnapshotRegistry.register root txn.Snapshot.SeqNo

                    match Catalog.load current.Store with
                    | Error e ->
                        ActiveSnapshotRegistry.unregister root registration
                        err e
                    | Ok fresh ->
                        let next = { current with Manager = mgr }
                        publish next

                        let nextLocal: SessionState =
                            {
                                Catalog = fresh
                                Active = Some txn
                                Pending = None
                                Registration = Some registration
                            }

                        ret (next, (nextLocal, StatementResult.Empty)))

            serialized run |> Result.map fst

    /// COMMIT。接続の transaction を共有の Manager で確定する。
    member _.Commit(local: SessionState) =
        match local.Active with
        | None -> err (DbError.internalError "コミットする transaction がありません")
        | Some _ ->
            serialized (fun current ->
                let state = SessionState.apply local current

                match Engine.executeCommit state with
                | Error e -> err e
                | Ok next ->
                    let committed =
                        { next with
                            Active = None
                            Pending = None
                            ActiveRegistration = None
                        }

                    publish committed
                    ret (committed, (SessionState.ofState committed, StatementResult.Empty)))

    /// ROLLBACK。接続の transaction を捨てる。
    member _.Rollback(local: SessionState) =
        match local.Active with
        | None -> err (DbError.internalError "ロールバックする transaction がありません")
        | Some _ ->
            serialized (fun current ->
                let state = SessionState.apply local current
                let next = Engine.executeRollback state

                let committed =
                    { next with
                        Active = None
                        Pending = None
                        ActiveRegistration = None
                    }

                publish committed
                ret (committed, (SessionState.ofState committed, StatementResult.Empty)))

    /// transaction 以外の文（SELECT / DML / DDL）を実行する。
    member _.ExecuteNonTransaction (local: SessionState) (limits: ResultLimits) (stmt: Statement) =
        serialized (fun current ->
            let state = SessionState.apply local current

            match Engine.executeStatementWithLimits limits state stmt with
            | Error e -> err e
            | Ok(next, result) ->
                let nextLocal = SessionState.ofState next

                if next.Active.IsNone && next.Pending.IsNone then
                    let committed =
                        { next with
                            Active = None
                            Pending = None
                            ActiveRegistration = None
                        }

                    publish committed
                    ret (committed, (nextLocal, result))
                else
                    // transaction 内: 共有するのは Manager の変化だけ
                    let committed = { current with Manager = next.Manager }
                    publish committed
                    ret (committed, (nextLocal, result)))

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

                    { state with
                        Manager = mgr
                        Active = None
                        Pending = None
                        ActiveRegistration = None
                    }

            let closeResult = owner.WithState(state).Close()

            match syncError with
            | Some e -> err e
            | None -> closeResult
