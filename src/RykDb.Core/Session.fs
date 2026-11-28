// ===========================================================================
// Session — 公開エントリポイント。
// ライブラリ利用者・CLI・TCP サーバはこの層だけを見る。
//
// * committed state は `DatabaseHost`（internal）が隠し、
//   Session は接続ローカルの transaction だけを持つ。
// * SELECT のみの Execute は並行実行でき、書き込み・transaction 操作・Close は直列化される。
//   読み取りは入口で状態とスナップショット（ファイル pin）を同じ短いロック区間で取る。
// * Close は実行中の読み取りが終わるのを待つ。1つの Session は 1つの論理接続。
// * Execute は複数文を順に実行し、途中で失敗してもそれ以前に成功した文の状態は失わない。
// ===========================================================================

namespace RykDb.Core

/// Session の結果上限と作業メモリ。
[<NoEquality; NoComparison>]
type SessionOptions =
    {
        /// 1回の Execute で保持する SELECT 行数の上限。
        /// `-1` で無制限（`-2` 以下はエラー）。
        MaxResultRows: int64
        /// 1回の Execute で保持する SELECT 行のメモリ概算上限。
        /// `-1` で無制限（`-2` 以下はエラー）。
        MaxResultBytes: int64
        /// JOIN / DISTINCT / GROUP BY / IN が作業用に保持してよいメモリの概算上限（バイト）。
        /// 超えた分は一時ファイルへ退避する。`-1` で無制限（`-2` 以下はエラー）。
        WorkMemBytes: int64
        /// ORDER BY の受け皿（メモリ内で整列する上限）。
        /// 超えた分は一時ファイルの run へ退避する。`-1` で無制限（`-2` 以下はエラー）。
        SortBufferBytes: int64
        /// 一時ファイルの置き場所（`None` は OS の一時ディレクトリ）。
        SpillDirectory: string option
        /// ANALYZE のキャンセル。
        CancelAnalysis: System.Threading.CancellationToken
        /// ANALYZE の進捗（処理名, 処理済み行数）。
        OnAnalyzeProgress: (string * int64 -> unit) option
    }

    /// 既定値（結果 100万行 / 256MiB、作業 256MiB、ソート受け皿 64MiB）。
    static member Default =
        {
            MaxResultRows = 1_000_000L
            MaxResultBytes = 256L * 1024L * 1024L
            WorkMemBytes = 256L * 1024L * 1024L
            SortBufferBytes = 64L * 1024L * 1024L
            SpillDirectory = None
            CancelAnalysis = System.Threading.CancellationToken.None
            OnAnalyzeProgress = None
        }

    static member internal Validate(options: SessionOptions) =
        if options.MaxResultRows < -1L then
            DbError.capacity "MaxResultRows は -1（無制限）以上でなければなりません" |> err
        elif options.MaxResultBytes < -1L then
            DbError.capacity "MaxResultBytes は -1（無制限）以上でなければなりません" |> err
        elif options.WorkMemBytes < -1L then
            DbError.capacity "WorkMemBytes は -1（無制限）以上でなければなりません" |> err
        elif options.SortBufferBytes < -1L then
            DbError.capacity "SortBufferBytes は -1（無制限）以上でなければなりません" |> err
        else
            ret ()

/// `RowReader` の読み取り状態。
/// <remarks>
/// 「読んでいる / 読み終えた / 閉じた」を 1つの値で表す。
/// `Reading` だけが走査と snapshot を保持している（他 2状態は解放済み）。
/// </remarks>
[<NoEquality; NoComparison>]
type private RowReaderState =
    /// 読み取り中。
    | Reading of Op
    /// 読み取りを終えた（リソースは解放済み）。
    | Exhausted
    /// `Dispose` / 読み取りエラーでリソースを解放した。
    | Closed

/// SELECT 結果を1行ずつ読むリーダー。
/// <remarks>
/// `Session.TryOpenReader` が返す。
/// 読み終わるか `Dispose` するまで snapshot と走査リソースを保持する。
/// 1つのインスタンスを複数スレッドから同時に使ってはならない。
/// </remarks>
type RowReader internal (columns: string array, op: Op, release: unit -> unit) =
    /// リーダーの状態（Reading / Exhausted / Closed）。
    let mutable state = Reading op

    /// リソースを解放して次の状態にする。
    /// `Reading` 以外（既に解放済み）なら何もしないので、
    /// `release` は 1回しか呼ばれない。
    let releaseTo (next: RowReaderState) =
        match state with
        | Reading _ ->
            state <- next
            release ()
        | Exhausted
        | Closed -> ()

    /// 列名。
    member _.Columns = columns

    /// 次の行を読む。
    /// 終端では `Ok None` を返し、リソースを自動で解放する。
    /// 明示的に `Dispose` した後に呼ぶとエラーになる。
    member _.TryRead() =
        match state with
        | Exhausted -> ret None
        | Closed -> err (DbError.internalError "RowReader は閉じられています")
        | Reading current ->
            match Op.next current with
            | OpStep.OpError e ->
                releaseTo Closed
                err e
            | OpStep.OpDone ->
                releaseTo Exhausted
                ret None
            | OpStep.OpRow(row, nxt) ->
                match LoadedRow.create (SqlValue.copyRow row) with
                | Error e ->
                    releaseTo Closed
                    err e
                | Ok loaded ->
                    state <- Reading nxt
                    ret (Some loaded)

    override _.Finalize() =
        try
            releaseTo Closed
        with _ ->
            ()

    interface System.IDisposable with
        member this.Dispose() =
            releaseTo Closed
            System.GC.SuppressFinalize this

type internal ResultBudget = { RowsLeft: int64; BytesLeft: int64 }

/// `Execute` の前後で transaction の状態がどう変わったか。
/// <remarks>
/// `Execute` の内部で transaction の遷移を判定するために使う
/// （公開 API には出さない）。
/// </remarks>
[<RequireQualifiedAccess>]
type private SessionTransactionChange =
    /// transaction の状態は変わっていない。
    | NoChange
    /// この実行で transaction が始まった。
    | Started
    /// この実行で transaction が終わった（COMMIT / ROLLBACK）。
    | Ended

/// `Session` の接続ローカル状態。
/// <remarks>
/// `Closed` は終端で、以後の操作はすべてエラーになる。
/// 「閉じたか」と `TxnState` を別々に持つと、閉じた後も `local` を読めてしまう。
/// 実行中の読み取り数（`readers`）はカウンタなので `Session` 側で別に持つ。
/// </remarks>
[<RequireQualifiedAccess>]
type private LocalSession =
    | Open of TxnState
    | Closed

/// Session のロジックを接続状態から切り出したヘルパー。
module internal SessionHelpers =

    /// 上限から実行用の制限を作る（残予算を反映する）。
    let limitsOfBudget (limits: ResultLimits) (budget: ResultBudget) =
        {
            MaxRows = max 0L budget.RowsLeft
            MaxBytes = max 0L budget.BytesLeft
            Budget = limits.Budget
            Cancel = limits.Cancel
            Progress = limits.Progress
        }

    /// 実行結果の行数とバイト数を予算から差し引く。
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

    /// 閉じたセッションに対するエラーを作る。
    let closedError () =
        DbError.internalError "Session は閉じられています"

    /// バッチの結果を 1つの結果へまとめる。
    let summarize (results: StatementResult list) =
        let selects = results |> List.filter (fun r -> not (Array.isEmpty r.Columns))

        match selects with
        | [ s ] -> s
        | _ -> StatementResult.Affected(results |> List.sumBy _.RowsAffected)

    /// 読み取り経路の文を順に実行する。
    [<TailCall>]
    let rec runReadLoop
        (limits: ResultLimits)
        (readState: Committed)
        (budget: ResultBudget)
        (acc: StatementResult list)
        =
        function
        | [] -> ret (List.rev acc)
        | stmt :: tail ->
            match
                Engine.executeStatementWithLimits (limitsOfBudget limits budget) (ExecState.ofCommitted readState) stmt
            with
            | Ok(_, result) -> runReadLoop limits readState (consumeBudget result budget) (result :: acc) tail
            | Error e -> err e

    /// 排他経路で文を 1つずつ実行する（末尾再帰）。
    [<TailCall>]
    let rec runExclusiveLoop
        (host: DatabaseHost)
        (limits: ResultLimits)
        (setState: TxnState -> unit)
        (budget: ResultBudget)
        (acc: StatementResult list)
        (state: TxnState)
        (statements: Statement list)
        =
        match statements with
        | [] -> ret (List.rev acc)
        | stmt :: tail ->
            let outcome =
                match stmt with
                | Statement.Begin -> host.Begin state |> Result.map (fun next -> next, StatementResult.Empty)
                | Statement.Commit ->
                    host.Commit state
                    >>= fun (outcome, result) ->
                        match outcome with
                        | CommitOutcome.Committed next -> ret (next, result)
                        | CommitOutcome.CommittedUnsynced(next, e)
                        | CommitOutcome.RolledBack(next, e) ->
                            setState next
                            err e
                | Statement.Rollback -> host.Rollback state
                | _ -> host.ExecuteNonTransaction state (limitsOfBudget limits budget) stmt

            match outcome with
            | Ok(next, result) ->
                setState next
                runExclusiveLoop host limits setState (consumeBudget result budget) (result :: acc) next tail
            | Error e -> err e

    /// SELECT / EXPLAIN だけのバッチか。
    let isReadOnlyBatch =
        List.forall (function
            | Statement.Select _
            | Statement.Explain _ -> true
            | Statement.ExplainAnalyze _ -> true
            | _ -> false)

    /// パラメータ配列を先頭から検査する。
    [<TailCall>]
    let rec validateParametersLoop (parameters: SqlValue array) (i: int) =
        if i >= parameters.Length then
            ret ()
        else
            match parameters.[i] with
            | SqlValue.Unloaded -> err (DbError.typeError "パラメータに SqlValue.Unloaded は指定できません")
            | SqlValue.Text s when isNull s -> err (DbError.typeError "パラメータの TEXT に null は指定できません")
            | SqlValue.Blob b when isNull b -> err (DbError.typeError "パラメータの BLOB に null 配列は指定できません")
            | _ -> validateParametersLoop parameters (i + 1)

    /// ゲートを跨ぐ例外を InternalError に変換して実行する。
    let executeStatementsCore
        (isReadOnlyBatch: Statement list -> bool)
        (runRead: Statement list -> Result<'a, DbError>)
        (runExclusive: Statement list -> Result<'a, DbError>)
        (statements: Statement list)
        =
        try
            if isReadOnlyBatch statements then
                runRead statements
            else
                runExclusive statements
        with ex ->
            err (DbError.internalError ex.Message)

    /// fsync 失敗後でも受け付ける文（transaction を閉じる ROLLBACK だけ）。
    /// ROLLBACK は WAL を書かないので、失敗状態でも安全に通せる。
    let isClosingStatement =
        function
        | Statement.Rollback -> true
        | _ -> false

    /// 解析済みの文を実行する（ゲート＝closed / durability を確認した後）。
    /// <remarks>
    /// fsync 失敗後も **transaction を閉じる ROLLBACK だけ**は受け付ける。
    /// 残したままだと接続が transaction を閉じられず、切断するまで回収できない。
    /// ROLLBACK は WAL を書かないので安全に通せる。
    /// それ以外の文は従来どおり同じエラーで失敗する。
    /// </remarks>
    let executeCore
        (isClosed: unit -> bool)
        (host: DatabaseHost)
        (validateParameters: SqlValue array -> Result<unit, DbError>)
        (runStatements: Statement list -> Result<'a, DbError>)
        (sql: string)
        (parameters: SqlValue array)
        =
        if isClosed () then
            err (closedError ())
        else
            validateParameters parameters
            >>= fun () ->
                let parameters = SqlValue.copyRow parameters

                Parser.parseStatementsWith parameters sql
                >>= fun statements ->
                    match host.DurabilityFailure with
                    | Some e when not (List.forall isClosingStatement statements) -> err e
                    | _ -> runStatements statements

    /// RowReader のリソースを解放する。
    let private readerRelease
        (exitRead: System.IDisposable -> unit)
        (stream: SelectStream)
        (lease: System.IDisposable)
        =
        try
            stream.Cleanup()
        finally
            exitRead lease

    /// `TryOpenReader` の実装。
    let tryOpenReaderCore
        (isClosed: unit -> bool)
        (host: DatabaseHost)
        (validateParameters: SqlValue array -> Result<unit, DbError>)
        (tryEnterRead: unit -> Result<(Committed * System.IDisposable) option, DbError>)
        (exitRead: System.IDisposable -> unit)
        (limits: ResultLimits)
        (sql: string)
        (parameters: SqlValue array)
        =
        if isClosed () then
            err (closedError ())
        else
            match host.DurabilityFailure with
            | Some e -> err e
            | None ->
                validateParameters parameters
                >>= fun () ->
                    let parameters = SqlValue.copyRow parameters

                    match Parser.parseStatementsWith parameters sql with
                    | Ok [ Statement.Select stmt ] ->
                        match tryEnterRead () with
                        | Ok None -> ret None
                        | Ok(Some(readState, lease)) ->
                            let opened =
                                protect (fun () ->
                                    Engine.openSelect limits.Budget (ExecState.ofCommitted readState) stmt)
                                >>= id

                            match opened with
                            | Error e ->
                                exitRead lease
                                err e
                            | Ok stream ->
                                ret (
                                    Some(
                                        new RowReader(
                                            stream.Columns,
                                            stream.Op,
                                            fun () -> readerRelease exitRead stream lease
                                        )
                                    )
                                )
                        | Error e -> err e
                    | Ok _ -> ret None
                    | Error e -> err e

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
    /// 接続ローカル（`local` / `readers`）を保護する短いロック。
    /// 読み取りは入口と出口でしか取らないので、実行中の書き込みに待たされない。
    let stateGate = obj ()

    /// 書き込み・transaction 操作・Close を接続内で直列化するロック。
    let writeGate = obj ()

    /// 接続だけが持つ状態（Open / Closed、実行中の transaction）。
    let mutable local = LocalSession.Open TxnState.none

    /// 実行中の読み取り数（Close は 0 になるまで待つ）。
    let mutable readers = 0

    /// `SessionOptions` を実行用の上限へ変換したもの。
    let limits =
        {
            MaxRows =
                if options.MaxResultRows < 0L then
                    System.Int64.MaxValue
                else
                    options.MaxResultRows
            MaxBytes =
                if options.MaxResultBytes < 0L then
                    System.Int64.MaxValue
                else
                    options.MaxResultBytes
            Budget =
                {
                    Work = options.WorkMemBytes
                    Sort = options.SortBufferBytes
                    Directory = options.SpillDirectory
                    Scope = None
                }
            Cancel = options.CancelAnalysis
            Progress = options.OnAnalyzeProgress
        }

    /// 1文ごとの結果上限（行・バイト）。
    let initialBudget =
        {
            RowsLeft = limits.MaxRows
            BytesLeft = limits.MaxBytes
        }

    /// Close 済みか。
    let isClosed () =
        lock stateGate (fun () ->
            match local with
            | LocalSession.Closed -> true
            | LocalSession.Open _ -> false)

    /// 接続が transaction 中か。
    let inTransaction () =
        lock stateGate (fun () ->
            match local with
            | LocalSession.Closed -> false
            | LocalSession.Open state -> TxnState.inTransaction state)

    /// 排他経路。
    /// 文を 1つずつ host の writeGate の下で実行し、成功するたびに接続ローカルの状態を更新する。
    /// WAL の fsync 待ちは host が writeGate の外で行う。
    let runExclusive (statements: Statement list) =
        lock writeGate (fun () ->
            match lock stateGate (fun () -> local) with
            | LocalSession.Closed -> err (SessionHelpers.closedError ())
            | LocalSession.Open start ->
                let wasInTransaction = TxnState.inTransaction start

                SessionHelpers.runExclusiveLoop
                    host
                    limits
                    (fun next -> lock stateGate (fun () -> local <- LocalSession.Open next))
                    initialBudget
                    []
                    start
                    statements
                >>= fun results ->
                    let nowInTransaction = inTransaction ()

                    let change =
                        match wasInTransaction, nowInTransaction with
                        | false, true -> SessionTransactionChange.Started
                        | true, false -> SessionTransactionChange.Ended
                        | _ -> SessionTransactionChange.NoChange

                    ret (SessionHelpers.summarize results, change))

    /// 読み取りの入口。
    /// closed ならエラー、transaction 中なら None（排他経路へ）。
    /// スナップショット取得と readers の加算を同じロック区間で行う
    /// （間に commit + flush + compaction が入ると、古い snapshot に必要な版が消え得るため）。
    let tryEnterRead () =
        lock stateGate (fun () ->
            match local with
            | LocalSession.Closed -> err (SessionHelpers.closedError ())
            | LocalSession.Open state when TxnState.inTransaction state -> ret None
            | LocalSession.Open _ ->
                host.AcquireReadSnapshot()
                >>= fun (readState, lease) ->
                    readers <- readers + 1
                    ret (Some(readState, lease)))

    /// 読み取りの出口。
    /// lease を返して readers を減らす。
    let exitRead (lease: System.IDisposable) =
        lease.Dispose()

        lock stateGate (fun () ->
            readers <- readers - 1

            if readers = 0 then
                System.Threading.Monitor.PulseAll stateGate)

    /// 読み取り経路。
    /// SELECT / EXPLAIN だけを、捕捉したスナップショットに対してロックなしで実行する。
    let runRead (statements: Statement list) =
        match tryEnterRead () with
        | Ok None -> runExclusive statements
        | Ok(Some(readState, lease)) ->
            try
                SessionHelpers.runReadLoop limits readState initialBudget [] statements
                >>= fun results -> ret (SessionHelpers.summarize results, SessionTransactionChange.NoChange)
            finally
                exitRead lease
        | Error e -> err e

    /// パラメータに `SqlValue.Unloaded`（内部専用の未復号マーカー）が含まれていないか検査する。
    /// 呼び出し側が誤って内部値を持ち込むと、行のマスク漏れと同じ事故になるため拒否する。
    /// `Blob` の配列は呼び出し側と共有しない（`SqlValue.copyRow` で複製する）。
    let validateParameters (parameters: SqlValue array) =
        if isNull parameters then
            err (DbError.typeError "parameters に null は指定できません")
        else
            SessionHelpers.validateParametersLoop parameters 0

    /// 解析済みの文を実行する（ゲート＝closed / durability を確認した後）。
    /// <remarks>
    /// 想定外の例外がゲート（`writeGate` / `readers` / lease）を跨いで呼び出し側へ漏れると、
    /// `Close` が永久に待つなど後続の操作が壊れる。
    /// `Session` の境界で `InternalError` に変換し、例外を送らない契約にする。
    /// </remarks>
    let executeStatementsCore (statements: Statement list) =
        SessionHelpers.executeStatementsCore SessionHelpers.isReadOnlyBatch runRead runExclusive statements

    /// 単一文または複数文を実行し、transaction の遷移も返す。
    let executeCore (sql: string) (parameters: SqlValue array) =
        SessionHelpers.executeCore isClosed host validateParameters executeStatementsCore sql parameters

    /// `TryOpenReader` の実装。
    let tryOpenReaderCore (sql: string) (parameters: SqlValue array) =
        SessionHelpers.tryOpenReaderCore isClosed host validateParameters tryEnterRead exitRead limits sql parameters

    /// DB を開く（background compaction 有効、既定の上限）。
    static member OpenWith(root: string) =
        Session.OpenWith(root, SessionOptions.Default)

    /// DB を開く（background compaction 有効、指定した上限）。
    static member OpenWith(root: string, options: SessionOptions) =
        SessionOptions.Validate options
        >>= fun () ->
            Spill.sweepStale options.SpillDirectory

            DatabaseHost.OpenWithBackground root
            |> Result.map (fun host -> new Session(host, options, true))

    /// テスト・ベンチ用: 同期 compaction で開く（ホストを所有する）。
    static member internal OpenWithSync(root: string, options: SessionOptions) =
        SessionOptions.Validate options
        >>= fun () ->
            Spill.sweepStale options.SpillDirectory

            DatabaseHost.OpenWith root
            |> Result.map (fun host -> new Session(host, options, true))

    /// テスト・内部用: 既に開いた `RowStore` から Session を作る（ホストを所有する）。
    static member internal OpenWithStore(root: string, options: SessionOptions, store: RowStore) =
        SessionOptions.Validate options
        >>= fun () ->
            DatabaseHost.OpenWithStore(root, store)
            |> Result.map (fun host -> new Session(host, options, true))

    /// テスト・内部用: 既に開いた `DatabaseHost` から接続を作る。
    /// `ownsHost = false` なら `Close` は自分の transaction を rollback するだけで、DB は閉じない。
    static member internal Create(host: DatabaseHost, options: SessionOptions, ownsHost: bool) =
        new Session(host, options, ownsHost)

    /// 実行中の transaction があるか。
    member _.InTransaction = inTransaction ()

    /// 直近の flush 失敗（成功したら None）。
    /// WAL へ commit 済みのため書き込みは継続できるが、SSTable 化が失敗している。
    member _.LastFlushError = host.Committed.Store.Engine.LastFlushError

    /// 直近の background compaction の失敗（成功したら None）。
    /// 同期 compaction で開いた場合は常に None。
    member _.LastCompactionError =
        host.Committed.Store.Host
        |> Option.bind (fun coordinator -> coordinator.LastCompactionError)

    /// テーブル一覧を返す。
    member _.TableNames() =
        lock stateGate (fun () ->
            match local with
            | LocalSession.Closed -> err (SessionHelpers.closedError ())
            | LocalSession.Open _ -> ret (host.Committed.Catalog.Tables |> List.map _.Name))

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
    member _.TryOpenReader(sql: string) = tryOpenReaderCore sql [||]

    /// パラメータ付きで単一 SELECT をストリーミング用に開く。
    /// SQL 中の `?` の扱いは `Execute(sql, parameters)` と同じ。
    /// 値は SQL 文字列へ連結せず `Expr.Value` として埋め込むため、
    /// 値の内容で SQL の構文が変わることはない。
    member _.TryOpenReader(sql: string, parameters: SqlValue array) = tryOpenReaderCore sql parameters

    /// 単一文または複数文を実行する。
    /// <remarks>
    /// 文は順に実行され、成功するたびに接続ローカルの状態と committed state が更新される。
    /// 途中の文が失敗した場合、それ以前に成功した文の状態は保持される。
    /// `BEGIN` 済みなら transaction は開いたままになるので、
    /// 必要に応じて `Rollback()` すること。
    /// SELECT だけのバッチは、実行中の書き込みと並行して実行される。
    /// </remarks>
    member _.Execute(sql: string) = executeCore sql [||] |> Result.map fst

    /// パラメータ付きで単一文または複数文を実行する。
    /// <remarks>
    /// SQL 中の `?` は、出現順に `parameters` の値へ置き換えられる。
    /// 個数が一致しない場合は `TypeError` になる。
    /// 値は SQL リテラルと同じく式として評価される
    /// （`LIMIT ?` や `LIKE ?`、`INSERT ... VALUES (?, ?)` にも使える）。
    /// 値は SQL 文字列へ連結せず `Expr.Value` として埋め込むため、
    /// 値の内容で SQL の構文が変わることはない。
    /// </remarks>
    member _.Execute(sql: string, parameters: SqlValue array) =
        executeCore sql parameters |> Result.map fst

    /// 解析済みの 1文を実行する（テスト・ベンチ用。`Execute` と同じゲートを通る）。
    member internal _.ExecuteStatement(stmt: Statement) =
        if isClosed () then
            err (SessionHelpers.closedError ())
        else
            match host.DurabilityFailure with
            | Some e when not (SessionHelpers.isClosingStatement stmt) -> err e
            | _ -> executeStatementsCore [ stmt ] |> Result.map fst

    /// SELECT の計画だけを組み立てる（テスト・ベンチ用）。
    /// <remarks>
    /// 書き込みを挟まない用途に限る（固定 snapshot を ActiveSnapshot に登録しない）。
    /// </remarks>
    member internal _.PrepareSelect(stmt: SelectStatement) =
        Engine.prepareSelect (ExecState.ofCommitted host.Committed) stmt

    member internal _.PrepareSelectWith(strategy: JoinOrderStrategy, stmt: SelectStatement) =
        Engine.prepareSelectWith strategy (ExecState.ofCommitted host.Committed) stmt

    /// トランザクションを始める。
    member _.Begin() =
        lock writeGate (fun () ->
            match lock stateGate (fun () -> local) with
            | LocalSession.Closed -> err (SessionHelpers.closedError ())
            | LocalSession.Open state ->
                host.Begin state
                |> Result.map (fun next ->
                    lock stateGate (fun () -> local <- LocalSession.Open next)
                    ()))

    /// COMMIT。
    /// <remarks>
    /// write-write 競合で失敗した場合、host が transaction を abort する。
    /// 接続は transaction 外へ戻り、`ConflictError` が返る
    /// （同じ snapshot のまま再 COMMIT しても永久に競合するため）。
    /// </remarks>
    member _.Commit() =
        lock writeGate (fun () ->
            match lock stateGate (fun () -> local) with
            | LocalSession.Closed -> err (SessionHelpers.closedError ())
            | LocalSession.Open state ->
                host.Commit state
                >>= fun (outcome, _) ->
                    match outcome with
                    | CommitOutcome.Committed next ->
                        lock stateGate (fun () -> local <- LocalSession.Open next)
                        ret ()
                    | CommitOutcome.CommittedUnsynced(next, e)
                    | CommitOutcome.RolledBack(next, e) ->
                        lock stateGate (fun () -> local <- LocalSession.Open next)
                        err e)

    /// ROLLBACK。
    /// <remarks>
    /// transaction が無い場合は何もしない（自動 rollback 済みでも後始末を失敗させない）。
    /// </remarks>
    member _.Rollback() =
        lock writeGate (fun () ->
            match lock stateGate (fun () -> local) with
            | LocalSession.Closed -> err (SessionHelpers.closedError ())
            | LocalSession.Open state ->
                host.Rollback state
                |> Result.map (fun (next, _) -> lock stateGate (fun () -> local <- LocalSession.Open next)))

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
                    match local with
                    | LocalSession.Closed -> None
                    | LocalSession.Open state ->
                        local <- LocalSession.Closed
                        Some state)

            match captured with
            | None -> ret ()
            | Some state ->
                lock stateGate (fun () ->
                    while readers > 0 do
                        System.Threading.Monitor.Wait stateGate |> ignore)

                let rollback =
                    if TxnState.inTransaction state then
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

/// `SessionPool` が新しい接続を受け付けるか。
[<RequireQualifiedAccess>]
type private PoolLifecycle =
    | Open
    | Closed

/// 同一 DB を共有する複数の `Session`（接続）を発行する。
/// <remarks>
/// 各 `Session` は独立した transaction を持ち、
/// 採番・競合検出・WAL・memtable は全接続で共有する。
/// サーバは接続ごとに `CreateSession` し、切断時に `Session.Close` する。
/// `Close` は DB 全体を閉じる（全接続の終了後に呼ぶ）。
/// </remarks>
type SessionPool internal (host: DatabaseHost, options: SessionOptions) =
    /// `lifecycle` を保護するロック。
    let gate = obj ()

    /// プールのライフサイクル。
    let mutable lifecycle = PoolLifecycle.Open

    /// DB を開く（既定の上限）。
    static member OpenWith(root: string) =
        SessionPool.OpenWith(root, SessionOptions.Default)

    /// DB を開く（指定した上限）。
    static member OpenWith(root: string, options: SessionOptions) =
        SessionOptions.Validate options
        >>= fun () ->
            DatabaseHost.OpenWithBackground root
            |> Result.map (fun host -> new SessionPool(host, options))

    /// 接続用の Session を作る。使い終わったら `Session.Close` する。
    member _.CreateSession() =
        lock gate (fun () ->
            match lifecycle with
            | PoolLifecycle.Closed -> err (DbError.internalError "SessionPool は閉じられています")
            | PoolLifecycle.Open -> ret (new Session(host, options, false)))

    /// DB 全体を閉じる（接続中の Session は以後エラーになる）。
    member _.Close() =
        let shouldClose =
            lock gate (fun () ->
                match lifecycle with
                | PoolLifecycle.Closed -> false
                | PoolLifecycle.Open ->
                    lifecycle <- PoolLifecycle.Closed
                    true)

        if shouldClose then host.Close() else ret ()

    interface System.IDisposable with
        member this.Dispose() = this.Close() |> ignore
