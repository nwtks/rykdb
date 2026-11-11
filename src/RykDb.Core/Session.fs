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

    /// 上限なし（既存の Database API と同じ挙動）。
    static member Unlimited =
        {
            MaxResultRows = System.Int64.MaxValue
            MaxResultBytes = System.Int64.MaxValue
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
    let mutable rowsRead = 0L
    let mutable closed = false
    let mutable exhausted = false

    let releaseResources () =
        if not closed then
            closed <- true
            release ()

    /// 列名。
    member _.Columns = columns

    /// これまでに返した行数。
    member _.RowsRead = rowsRead

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
                rowsRead <- rowsRead + 1L
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

/// データベースへのセッション。
/// <remarks>
/// `Database` の不変ハンドルを内部に隠し、常に最新の状態を保持する。
/// SELECT のみの `Execute` は複数スレッドから並行実行できる。
/// 書き込み・transaction 操作・`Close` は直列化される。
/// 実行中の transaction がある間は、読み取りも含めて直列化される
/// （read-your-writes とスナップショットの一貫性を保つため）。
/// </remarks>
type Session internal (root: string, initialDb: Database, options: SessionOptions) =
    // current / closed / readers を保護する短いロック。
    // 読み取りは入口と出口でしか取らないので、実行中の書き込みに待たされない。
    let stateGate = obj ()
    // 書き込み・transaction 操作・Close を直列化するロック。
    let writeGate = obj ()
    let initial = initialDb.WithDeferredDurability()
    let durability = CommitDurability(fun pos -> Wal.sync root pos)
    let mutable current = initial
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

    /// 現在のハンドルを短いロックで取得する（close 済みなら None）。
    let currentHandle () =
        lock stateGate (fun () -> if closed then None else Some current)

    /// 次のハンドルを短いロックで公開する。
    let publish (next: Database) =
        lock stateGate (fun () -> current <- next)

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
    /// 書き込み・transaction 操作を直列化して実行する。
    /// WAL 位置の登録までは writeGate 内で行い、fsync の待機は writeGate の外で行う。
    let runExclusive (statements: Statement list) =
        let outcome, position =
            lock writeGate (fun () ->
                match currentHandle () with
                | None -> err (closedError ()), None
                | Some db ->
                    let wasInTransaction = db.InTransaction

                    let rec run (db: Database) budget acc =
                        function
                        | [] -> ret (db, List.rev acc)
                        | stmt :: tail ->
                            let db, lease = db.AcquireSnapshot()
                            use _lease = lease

                            match db.ExecuteWithLimits(limitsOfBudget budget, stmt) with
                            | Error e -> err e
                            | Ok(next, result) ->
                                publish next
                                run next (consumeBudget result budget) (result :: acc) tail

                    let outcome, position =
                        match run db initialBudget [] statements with
                        | Error e ->
                            let pos = currentHandle () |> Option.bind (fun d -> d.WalPosition)
                            Error e, pos
                        | Ok(finalDb, results) ->
                            publish finalDb

                            let change =
                                match wasInTransaction, finalDb.InTransaction with
                                | false, true -> SessionTransactionChange.Started
                                | true, false -> SessionTransactionChange.Ended
                                | _ -> SessionTransactionChange.NoChange

                            ret (summarize results, change), finalDb.WalPosition

                    position |> Option.iter durability.Register
                    outcome, position)

        match position with
        | Some pos ->
            match durability.Await pos with
            | Error e -> err e
            | Ok() -> outcome
        | None -> outcome

    /// 読み取りの入口。
    /// closed ならエラー、transaction 中なら None（排他経路へ）。
    /// ハンドル捕捉とスナップショット取得を同じロック区間で行う
    /// （間に commit + flush + compaction が入ると、古い snapshot に必要な版が消え得るため）。
    let tryEnterRead () =
        lock stateGate (fun () ->
            if closed then
                Error(closedError ())
            elif current.InTransaction then
                Ok None
            else
                let db, lease = current.AcquireSnapshot()
                readers <- readers + 1
                Ok(Some(db, lease)))

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
        | Ok(Some(db, lease)) ->
            try
                let rec run (db: Database) budget acc =
                    function
                    | [] -> ret (List.rev acc)
                    | stmt :: tail ->
                        match db.ExecuteWithLimits(limitsOfBudget budget, stmt) with
                        | Error e -> err e
                        | Ok(next, result) -> run next (consumeBudget result budget) (result :: acc) tail

                match run db initialBudget [] statements with
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
            match durability.Failure with
            | Some e -> err e
            | None ->
                match Parser.parseStatements sql with
                | Error e -> err e
                | Ok statements when isReadOnlyBatch statements -> runRead statements
                | Ok statements -> runExclusive statements

    /// transaction 操作（BEGIN / COMMIT / ROLLBACK）の共通経路。
    /// 排他ロックの下でスナップショットを取り、適用後のハンドルを公開する。
    let runTransaction (operation: Database -> Result<Database, DbError>) =
        match durability.Failure with
        | Some e -> err e
        | None ->
            let outcome, position =
                lock writeGate (fun () ->
                    match currentHandle () with
                    | None -> err (closedError ()), None
                    | Some db ->
                        let db, lease = db.AcquireSnapshot()
                        use _lease = lease

                        let outcome, position =
                            match operation db with
                            | Error e ->
                                let pos = currentHandle () |> Option.bind (fun d -> d.WalPosition)
                                Error e, pos
                            | Ok next ->
                                publish next
                                Ok(), next.WalPosition

                        position |> Option.iter durability.Register
                        outcome, position)

            match position with
            | Some pos ->
                match durability.Await pos with
                | Error e -> err e
                | Ok() -> outcome
            | None -> outcome

    /// DB を開く（background compaction 有効、既定の上限）。
    static member OpenWith(root: string) =
        Session.OpenWith(root, SessionOptions.Default)

    /// DB を開く（background compaction 有効、指定した上限）。
    static member OpenWith(root: string, options: SessionOptions) =
        match SessionOptions.Validate options with
        | Error e -> err e
        | Ok() ->
            Database.OpenWithBackground root
            |> Result.map (fun db -> new Session(root, db, options))

    /// DB のルートディレクトリ。
    member _.Root = root

    /// 実行中の transaction があるか。
    member _.InTransaction = lock stateGate (fun () -> not closed && current.InTransaction)

    /// テーブル一覧を返す。
    member _.TableNames() =
        lock stateGate (fun () ->
            if closed then
                err (closedError ())
            else
                ret (current.TableNames()))

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
            match durability.Failure with
            | Some e -> err e
            | None ->
                match Parser.parseStatements sql with
                | Error e -> err e
                | Ok [ Statement.Select stmt ] ->
                    match tryEnterRead () with
                    | Error e -> err e
                    | Ok None -> ret None
                    | Ok(Some(db, lease)) ->
                        match db.OpenSelect stmt with
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

    /// 単一文または複数文を実行し、transaction の遷移も返す。
    /// <remarks>
    /// サーバが接続ごとの transaction 所有権を判定するために使う。
    /// 読み取り専用バッチは常に `NoChange` を返す。
    /// </remarks>
    member _.ExecuteTracked(sql: string) = executeCore sql

    /// トランザクションを始める。
    member _.Begin() = runTransaction _.Begin()

    /// COMMIT。
    member _.Commit() = runTransaction _.Commit()

    /// ROLLBACK。
    member _.Rollback() = runTransaction _.Rollback()

    /// DB を閉じる。
    /// <remarks>
    /// 進行中の transaction は rollback してから閉じる。
    /// 実行中の読み取りがある場合は、それらが完了するまで待つ。
    /// 二重 Close は安全。Close 後の呼び出しはエラーになる。
    /// </remarks>
    member _.Close() =
        lock writeGate (fun () ->
            let captured =
                lock stateGate (fun () ->
                    if closed then
                        None
                    else
                        closed <- true
                        Some current)

            match captured with
            | None -> ret ()
            | Some db ->
                lock stateGate (fun () ->
                    while readers > 0 do
                        System.Threading.Monitor.Wait stateGate |> ignore)

                let syncError =
                    match db.WalPosition with
                    | Some pos ->
                        durability.Register pos

                        match durability.Await pos with
                        | Error e -> Some e
                        | Ok() -> None
                    | None -> None

                let rollbackError, closeTarget =
                    if db.InTransaction then
                        match db.Rollback() with
                        | Error e -> Some e, db
                        | Ok rolledBack -> None, rolledBack
                    else
                        None, db

                let closeResult = closeTarget.Close()

                match syncError, rollbackError with
                | Some e, _ -> err e
                | None, Some e -> err e
                | None, None -> closeResult)

    interface System.IDisposable with
        member this.Dispose() = this.Close() |> ignore
