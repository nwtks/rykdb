// ===========================================================================
// Session — 公開エントリポイント。
// ライブラリ利用者・CLI・TCP サーバはこの層だけを見る。
//
// * Database の不変ハンドルを内部に隠し、常に最新の状態を保持する。
//   公開メソッドはハンドルを返さないので、戻り値の差し替え忘れは起こらない。
// * SELECT のみの Execute は複数スレッド／複数接続から並行実行できる。
//   書き込み・transaction 操作・Close は直列化される。
// * 読み取りは入口で現在のハンドルとスナップショット（ファイル pin）を同じ短いロック区間で取得し、
//   実行中はロックを取らない（MVCC スナップショット読み）。
// * Close は実行中の読み取りが終わるのを待ってから閉じる。
// * 1つの Session は 1つの論理接続として扱う。
//   複数の接続で共有すると transaction も共有される。
// * Execute は SQL を 1文ずつ実行し、成功するたびに内部ハンドルを更新する。
//   途中の文が失敗しても、最後に成功した状態は失わない。
// ===========================================================================

namespace RykDb.Core

/// `Execute` の前後で transaction の状態がどう変わったか。
/// <remarks>
/// サーバが接続ごとの transaction 所有権を判定するために使う。
/// </remarks>
[<RequireQualifiedAccess>]
type SessionTransactionChange =
    /// transaction の状態は変わっていない。
    | NoChange
    /// この実行で transaction が始まった。
    | Started
    /// この実行で transaction が終わった（COMMIT / ROLLBACK）。
    | Ended

/// Session の結果上限。
/// materialize する SELECT 結果に適用する。
type SessionOptions =
    {
        /// 1回の Execute で保持する SELECT 行数の上限。
        MaxResultRows: int64
        /// 1回の Execute で保持する SELECT 行のメモリ概算上限。
        MaxResultBytes: int64
    }

    /// 既定値（100万行 / 256MiB）。
    static member Default =
        {
            MaxResultRows = 1_000_000L
            MaxResultBytes = 256L * 1024L * 1024L
        }

    static member internal Validate(options: SessionOptions) =
        if options.MaxResultRows < 0L then
            DbError.capacity "MaxResultRows は 0 以上でなければなりません" |> err
        elif options.MaxResultBytes < 0L then
            DbError.capacity "MaxResultBytes は 0 以上でなければなりません" |> err
        else
            ret ()

type private ResultBudget = { RowsLeft: int64; BytesLeft: int64 }

/// SELECT 結果を1行ずつ読むリーダー。
/// <remarks>
/// `Session.TryOpenReader` が返す。
/// 読み終わるか `Dispose` するまで snapshot と走査リソースを保持する。
/// 1つのインスタンスを複数スレッドから同時に使ってはならない。
/// </remarks>
type RowReader internal (columns: string array, op: Op, release: unit -> unit) =
    let mutable current = op
    let mutable closed = false
    let mutable exhausted = false

    let releaseResources () =
        if not closed then
            closed <- true
            release ()

    /// 列名。
    member _.Columns = columns

    /// 次の行を読む。
    /// 終端では `Ok None` を返し、リソースを自動で解放する。
    /// 明示的に `Dispose` した後に呼ぶとエラーになる。
    member _.TryRead() =
        if exhausted then
            ret None
        elif closed then
            err (DbError.internalError "RowReader は閉じられています")
        else
            match Op.next current with
            | Error e ->
                releaseResources ()
                err e
            | Ok None ->
                exhausted <- true
                releaseResources ()
                ret None
            | Ok(Some(row, nxt)) ->
                current <- nxt
                ret (Some row)

    override this.Finalize() =
        try
            releaseResources ()
        with _ ->
            ()

    interface System.IDisposable with
        member this.Dispose() =
            releaseResources ()
            System.GC.SuppressFinalize this

[<RequireQualifiedAccess>]
type private SyncStep =
    | SyncWait
    | SyncDone
    | SyncLead of WalState
    | SyncFail of DbError

/// 複数の commit の WAL 位置をまとめ、リーダーが 1回だけ fsync する。
/// 先に登録された位置を後から登録された位置が包含する場合、後者だけを同期すればよい。
type internal CommitDurability(sync: WalState -> Result<unit, DbError>) =
    let gate = obj ()
    let mutable syncedFile = 0L
    let mutable syncedSize = 0
    let mutable pending: WalState option = None
    let mutable syncing = false
    let mutable failure: DbError option = None

    let covers (pos: WalState) =
        pos.FileNumber < syncedFile
        || pos.FileNumber = syncedFile && pos.Size <= syncedSize

    let later (a: WalState) (b: WalState) =
        if (a.FileNumber, a.Size) >= (b.FileNumber, b.Size) then
            a
        else
            b

    /// 失敗していればそのエラーを返す。
    member _.Failure = lock gate (fun () -> failure)

    /// 同期対象の位置を登録する。
    member _.Register(pos: WalState) =
        lock gate (fun () ->
            pending <-
                match pending with
                | Some current -> Some(later current pos)
                | None -> Some pos)

    /// 指定位置まで同期済みになるまで待つ。
    /// 最初の待機者がリーダーとなり、登録済みの最大位置まで同期する。
    member _.Await(pos: WalState) =
        let rec loop () =
            let step =
                lock gate (fun () ->
                    match failure with
                    | Some e -> SyncStep.SyncFail e
                    | None when covers pos -> SyncStep.SyncDone
                    | None when not syncing ->
                        syncing <- true

                        let target =
                            match pending with
                            | Some current -> later current pos
                            | None -> pos

                        SyncStep.SyncLead target
                    | None ->
                        System.Threading.Monitor.Wait gate |> ignore
                        SyncStep.SyncWait)

            match step with
            | SyncStep.SyncDone -> ret ()
            | SyncStep.SyncFail e -> err e
            | SyncStep.SyncWait -> loop ()
            | SyncStep.SyncLead target ->
                let result = sync target

                lock gate (fun () ->
                    match result with
                    | Ok() ->
                        if (target.FileNumber, target.Size) > (syncedFile, syncedSize) then
                            syncedFile <- target.FileNumber
                            syncedSize <- target.Size
                    | Error e -> failure <- Some e

                    syncing <- false
                    System.Threading.Monitor.PulseAll gate)

                result

        loop ()

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
type internal DatabaseHost(root: string, owner: Database) =
    // committed / closed を保護する短いロック。
    let stateGate = obj ()
    // 書き込み・transaction 操作・Close を直列化するロック。
    let writeGate = obj ()
    let durability = CommitDurability(fun pos -> Wal.sync root pos)
    // committed state は常に Active / Pending / ActiveRegistration を持たない。
    let mutable committed = owner.StateWithDeferredDurability
    let mutable closed = false

    let snapshot () =
        lock stateGate (fun () -> committed, closed)

    let publish (state: DbState) =
        lock stateGate (fun () -> committed <- state)

    let closedError () = DbError.internalError "データベースは閉じられています"

    /// writeGate の内側で実行し、WAL の fsync を gate の外で待つ。
    /// `run` は「実行後の共有状態」と値も返す（読み取りだけなら入力をそのまま返す）。
    member private this.Serialized(run: DbState -> Result<DbState * 'a, DbError>) =
        let outcome, position =
            lock writeGate (fun () ->
                let current, wasClosed = snapshot ()

                if wasClosed then
                    Error(closedError ()), None
                else
                    match run current with
                    | Error e -> Error e, current.Store.Engine.WalState
                    | Ok(next, value) -> Ok value, next.Store.Engine.WalState)

        match position with
        | Some pos ->
            match durability.Await pos with
            | Error e -> err e
            | Ok() -> outcome
        | None -> outcome

    /// 現在の committed state。
    member _.Committed = lock stateGate (fun () -> committed)

    /// 永続化（WAL の fsync）の失敗。失敗後は全接続がエラーを返す。
    member _.DurabilityFailure = durability.Failure

    /// 読み取り用の committed state と、GC から守る lease を返す。
    member _.AcquireReadSnapshot() =
        let state, wasClosed = snapshot ()

        if wasClosed then
            err (closedError ())
        else
            let store, lease = RowStore.acquireSnapshot state.Store

            let readState =
                if obj.ReferenceEquals(store, state.Store) then
                    state
                else
                    { state with Store = store }

            ret (readState, lease)

    /// 明示 transaction を始める（接続ローカルの状態を返す）。
    member this.Begin(local: SessionState) =
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

            this.Serialized run |> Result.map fst

    /// COMMIT。接続の transaction を共有の Manager で確定する。
    member this.Commit(local: SessionState) =
        match local.Active with
        | None -> err (DbError.internalError "コミットする transaction がありません")
        | Some _ ->
            this.Serialized(fun current ->
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
    member this.Rollback(local: SessionState) =
        match local.Active with
        | None -> err (DbError.internalError "ロールバックする transaction がありません")
        | Some _ ->
            this.Serialized(fun current ->
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
    member this.ExecuteNonTransaction (local: SessionState) (limits: ResultLimits) (stmt: Statement) =
        this.Serialized(fun current ->
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
                let current, wasClosed = snapshot ()

                if wasClosed then
                    None
                else
                    closed <- true
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
                        |> List.fold
                            (fun (m: TransactionManager) (_, record) -> TransactionManager.abort record.Txn m)
                            state.Manager

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

/// データベースへのセッション（1接続）。
/// <remarks>
/// 実際のコミット状態（Store / Catalog / Manager）は `DatabaseHost` が共有し、
/// ここには接続だけが持つ transaction を置く。
/// SELECT のみの `Execute` は複数スレッドから並行実行できる。
/// 書き込み・transaction 操作・`Close` は接続内で直列化される。
/// 実行中の transaction がある間は、読み取りも含めて直列化される
/// （read-your-writes とスナップショットの一貫性を保つため）。
/// </remarks>
type Session internal (host: DatabaseHost, options: SessionOptions, ownsHost: bool) =
    // local / closed / readers を保護する短いロック。
    // 読み取りは入口と出口でしか取らないので、実行中の書き込みに待たされない。
    let stateGate = obj ()
    // 書き込み・transaction 操作・Close を接続内で直列化するロック。
    let writeGate = obj ()
    let mutable local = SessionState.none
    let mutable closed = false
    let mutable readers = 0

    let limits =
        {
            MaxRows = options.MaxResultRows
            MaxBytes = options.MaxResultBytes
        }

    let initialBudget =
        {
            RowsLeft = limits.MaxRows
            BytesLeft = limits.MaxBytes
        }

    let limitsOfBudget (budget: ResultBudget) =
        {
            MaxRows = max 0L budget.RowsLeft
            MaxBytes = max 0L budget.BytesLeft
        }

    let consumeBudget (result: StatementResult) (budget: ResultBudget) =
        if Array.isEmpty result.Columns then
            budget
        else
            let rows = int64 result.Rows.Length
            let bytes = result.Rows |> Array.sumBy (ValueCodec.managedRowBytes >> int64)

            {
                RowsLeft = max 0L (budget.RowsLeft - rows)
                BytesLeft = max 0L (budget.BytesLeft - bytes)
            }

    let closedError () =
        DbError.internalError "Session は閉じられています"

    let summarize (results: StatementResult list) =
        let selects = results |> List.filter (fun r -> not (Array.isEmpty r.Columns))

        match selects with
        | [ s ] -> s
        | _ ->
            {
                RowsAffected = results |> List.sumBy _.RowsAffected
                Columns = [||]
                Rows = [||]
            }

    /// 排他経路。
    /// 文を 1つずつ host の writeGate の下で実行し、成功するたびに接続ローカルの状態を更新する。
    /// WAL の fsync 待ちは host が writeGate の外で行う。
    let runExclusive (statements: Statement list) =
        lock writeGate (fun () ->
            let start = lock stateGate (fun () -> if closed then None else Some local)

            match start with
            | None -> err (closedError ())
            | Some start ->
                let wasInTransaction = SessionState.inTransaction start

                let rec run budget acc state =
                    function
                    | [] -> ret (List.rev acc)
                    | stmt :: tail ->
                        let outcome =
                            match stmt with
                            | Statement.Begin ->
                                host.Begin state |> Result.map (fun next -> (next, StatementResult.Empty))
                            | Statement.Commit -> host.Commit state
                            | Statement.Rollback -> host.Rollback state
                            | _ -> host.ExecuteNonTransaction state (limitsOfBudget budget) stmt

                        match outcome with
                        | Error e -> err e
                        | Ok(next, result) ->
                            lock stateGate (fun () -> local <- next)
                            run (consumeBudget result budget) (result :: acc) next tail

                match run initialBudget [] start statements with
                | Error e -> err e
                | Ok results ->
                    let nowInTransaction = lock stateGate (fun () -> SessionState.inTransaction local)

                    let change =
                        match wasInTransaction, nowInTransaction with
                        | false, true -> SessionTransactionChange.Started
                        | true, false -> SessionTransactionChange.Ended
                        | _ -> SessionTransactionChange.NoChange

                    ret (summarize results, change))

    /// 読み取りの入口。
    /// closed ならエラー、transaction 中なら None（排他経路へ）。
    /// スナップショット取得と readers の加算を同じロック区間で行う
    /// （間に commit + flush + compaction が入ると、古い snapshot に必要な版が消え得るため）。
    let tryEnterRead () =
        lock stateGate (fun () ->
            if closed then
                Error(closedError ())
            elif SessionState.inTransaction local then
                Ok None
            else
                match host.AcquireReadSnapshot() with
                | Error e -> Error e
                | Ok(readState, lease) ->
                    readers <- readers + 1
                    Ok(Some(readState, lease)))

    /// 読み取りの出口。
    /// lease を返して readers を減らす。
    let exitRead (lease: System.IDisposable) =
        lease.Dispose()

        lock stateGate (fun () ->
            readers <- readers - 1

            if readers = 0 then
                System.Threading.Monitor.PulseAll stateGate)

    /// 読み取り経路。
    /// SELECT だけを、捕捉したスナップショットに対してロックなしで実行する。
    let runRead (statements: Statement list) =
        match tryEnterRead () with
        | Error e -> err e
        | Ok None -> runExclusive statements
        | Ok(Some(readState, lease)) ->
            try
                let rec run budget acc =
                    function
                    | [] -> ret (List.rev acc)
                    | stmt :: tail ->
                        match Engine.executeStatementWithLimits (limitsOfBudget budget) readState stmt with
                        | Error e -> err e
                        | Ok(_, result) -> run (consumeBudget result budget) (result :: acc) tail

                match run initialBudget [] statements with
                | Error e -> err e
                | Ok results -> ret (summarize results, SessionTransactionChange.NoChange)
            finally
                exitRead lease

    /// SELECT だけのバッチか。
    let isReadOnlyBatch =
        List.forall (function
            | Statement.Select _ -> true
            | _ -> false)

    /// 単一文または複数文を実行し、transaction の遷移も返す。
    let executeCore (sql: string) =
        if lock stateGate (fun () -> closed) then
            err (closedError ())
        else
            match host.DurabilityFailure with
            | Some e -> err e
            | None ->
                match Parser.parseStatements sql with
                | Error e -> err e
                | Ok statements when isReadOnlyBatch statements -> runRead statements
                | Ok statements -> runExclusive statements

    /// DB を開く（background compaction 有効、既定の上限）。
    static member OpenWith(root: string) =
        Session.OpenWith(root, SessionOptions.Default)

    /// DB を開く（background compaction 有効、指定した上限）。
    static member OpenWith(root: string, options: SessionOptions) =
        match SessionOptions.Validate options with
        | Error e -> err e
        | Ok() ->
            Database.OpenWithBackground root
            |> Result.map (fun db -> new Session(DatabaseHost(root, db), options, true))

    /// テスト・内部用: 既に開いた `Database` から Session を作る（ホストを所有する）。
    static member internal OpenWith(root: string, options: SessionOptions, db: Database) =
        new Session(DatabaseHost(root, db), options, true)

    /// 実行中の transaction があるか。
    member _.InTransaction =
        lock stateGate (fun () -> not closed && SessionState.inTransaction local)

    /// テーブル一覧を返す。
    member _.TableNames() =
        lock stateGate (fun () ->
            if closed then
                err (closedError ())
            else
                ret (host.Committed.Catalog.Tables |> List.map _.Name))

    /// 単一の SELECT を materialize せずにストリーミング用に開く。
    /// <remarks>
    /// 対象は「文が1つだけで、それが SELECT」の場合のみ。
    /// それ以外（複数文・非 SELECT・空 SQL・transaction 中）は `Ok None` を返すので、
    /// 呼び出し側は `Execute` にフォールバックする。
    /// 結果は `SessionOptions` の `MaxResultRows` / `MaxResultBytes` で制限しない
    /// （保持せずに1行ずつ返すため）。
    /// 必要なら SQL の `LIMIT` を使う。
    /// 返した `RowReader` は使い終わったら `Dispose` すること。
    /// </remarks>
    member _.TryOpenReader(sql: string) =
        if lock stateGate (fun () -> closed) then
            err (closedError ())
        else
            match host.DurabilityFailure with
            | Some e -> err e
            | None ->
                match Parser.parseStatements sql with
                | Error e -> err e
                | Ok [ Statement.Select stmt ] ->
                    match tryEnterRead () with
                    | Error e -> err e
                    | Ok None -> ret None
                    | Ok(Some(readState, lease)) ->
                        match Engine.openSelect readState stmt with
                        | Error e ->
                            exitRead lease
                            err e
                        | Ok stream ->
                            let release () =
                                try
                                    stream.Cleanup()
                                finally
                                    exitRead lease

                            ret (Some(new RowReader(stream.Columns, stream.Op, release)))
                | Ok _ -> ret None

    /// 単一文または複数文を実行する。
    /// <remarks>
    /// 文は順に実行され、成功するたびに内部ハンドルが更新される。
    /// 途中の文が失敗した場合、それ以前に成功した文の状態は保持される。
    /// `BEGIN` 済みなら transaction は開いたままになるので、
    /// 必要に応じて `Rollback()` すること。
    /// SELECT だけのバッチは、実行中の書き込みと並行して実行される。
    /// </remarks>
    member _.Execute(sql: string) = executeCore sql |> Result.map fst

    /// トランザクションを始める。
    member _.Begin() =
        lock writeGate (fun () ->
            match lock stateGate (fun () -> if closed then None else Some local) with
            | None -> err (closedError ())
            | Some state ->
                match host.Begin state with
                | Error e -> err e
                | Ok next ->
                    lock stateGate (fun () -> local <- next)
                    ret ())

    /// COMMIT。
    member _.Commit() =
        lock writeGate (fun () ->
            match lock stateGate (fun () -> if closed then None else Some local) with
            | None -> err (closedError ())
            | Some state ->
                host.Commit state
                |> Result.map (fun (next, _) -> lock stateGate (fun () -> local <- next)))

    /// ROLLBACK。
    member _.Rollback() =
        lock writeGate (fun () ->
            match lock stateGate (fun () -> if closed then None else Some local) with
            | None -> err (closedError ())
            | Some state ->
                host.Rollback state
                |> Result.map (fun (next, _) -> lock stateGate (fun () -> local <- next)))

    /// このセッションを閉じる。
    /// <remarks>
    /// 進行中の transaction は rollback してから閉じる。
    /// 実行中の読み取りがある場合は、それらが完了するまで待つ。
    /// 二重 Close は安全。Close 後の呼び出しはエラーになる。
    /// `Session.OpenWith` で開いたセッション（ホストを所有）は DB 全体を閉じる。
    /// `SessionPool.CreateSession` で作ったセッションは、自分の transaction を rollback するだけで、
    /// DB は `SessionPool.Close` まで開いたままになる。
    /// </remarks>
    member _.Close() =
        lock writeGate (fun () ->
            let captured =
                lock stateGate (fun () ->
                    if closed then
                        None
                    else
                        closed <- true
                        Some local)

            match captured with
            | None -> ret ()
            | Some state ->
                lock stateGate (fun () ->
                    while readers > 0 do
                        System.Threading.Monitor.Wait stateGate |> ignore)

                let rollback =
                    if SessionState.inTransaction state then
                        host.Rollback state |> Result.map ignore
                    else
                        ret ()

                let closeResult = if ownsHost then host.Close() else ret ()

                match rollback, closeResult with
                | Error e, _ -> err e
                | Ok(), Error e -> err e
                | Ok(), Ok() -> ret ())

    interface System.IDisposable with
        member this.Dispose() = this.Close() |> ignore

/// 同一 DB を共有する複数の `Session`（接続）を発行する。
/// <remarks>
/// 各 `Session` は独立した transaction を持ち、
/// 採番・競合検出・WAL・memtable は全接続で共有する。
/// サーバは接続ごとに `CreateSession` し、切断時に `Session.Close` する。
/// `Close` は DB 全体を閉じる（全接続の終了後に呼ぶ）。
/// </remarks>
type SessionPool internal (host: DatabaseHost, options: SessionOptions) =
    let gate = obj ()
    let mutable closed = false

    /// DB を開く（既定の上限）。
    static member OpenWith(root: string) =
        SessionPool.OpenWith(root, SessionOptions.Default)

    /// DB を開く（指定した上限）。
    static member OpenWith(root: string, options: SessionOptions) =
        match SessionOptions.Validate options with
        | Error e -> err e
        | Ok() ->
            Database.OpenWithBackground root
            |> Result.map (fun db -> new SessionPool(DatabaseHost(root, db), options))

    /// 接続用の Session を作る。使い終わったら `Session.Close` する。
    member _.CreateSession() =
        lock gate (fun () ->
            if closed then
                err (DbError.internalError "SessionPool は閉じられています")
            else
                ret (new Session(host, options, false)))

    /// DB 全体を閉じる（接続中の Session は以後エラーになる）。
    member _.Close() =
        let shouldClose =
            lock gate (fun () ->
                if closed then
                    false
                else
                    closed <- true
                    true)

        if shouldClose then host.Close() else ret ()

    interface System.IDisposable with
        member this.Dispose() = this.Close() |> ignore
