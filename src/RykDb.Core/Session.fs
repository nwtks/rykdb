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

/// データベースへのセッション。
/// <remarks>
/// `Database` の不変ハンドルを内部に隠し、常に最新の状態を保持する。
/// SELECT のみの `Execute` は複数スレッドから並行実行できる。
/// 書き込み・transaction 操作・`Close` は直列化される。
/// 実行中の transaction がある間は、読み取りも含めて直列化される
/// （read-your-writes とスナップショットの一貫性を保つため）。
/// </remarks>
type Session internal (root: string, initial: Database) =
    // current / closed / readers を保護する短いロック。
    // 読み取りは入口と出口でしか取らないので、実行中の書き込みに待たされない。
    let stateGate = obj ()
    // 書き込み・transaction 操作・Close を直列化するロック。
    let writeGate = obj ()
    let mutable current = initial
    let mutable closed = false
    let mutable readers = 0

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
    let runExclusive (statements: Statement list) =
        lock writeGate (fun () ->
            match currentHandle () with
            | None -> err (closedError ())
            | Some db ->
                let wasInTransaction = db.InTransaction

                let rec run (db: Database) acc =
                    function
                    | [] -> ret (db, List.rev acc)
                    | stmt :: tail ->
                        let db, lease = db.AcquireSnapshot()
                        use _lease = lease

                        match db.Execute stmt with
                        | Error e -> err e
                        | Ok(next, result) ->
                            publish next
                            run next (result :: acc) tail

                match run db [] statements with
                | Error e -> err e
                | Ok(finalDb, results) ->
                    publish finalDb

                    let change =
                        match wasInTransaction, finalDb.InTransaction with
                        | false, true -> SessionTransactionChange.Started
                        | true, false -> SessionTransactionChange.Ended
                        | _ -> SessionTransactionChange.NoChange

                    ret (summarize results, change))

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
                let rec run (db: Database) acc =
                    function
                    | [] -> ret (List.rev acc)
                    | stmt :: tail ->
                        match db.Execute stmt with
                        | Error e -> err e
                        | Ok(next, result) -> run next (result :: acc) tail

                match run db [] statements with
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
            match Parser.parseStatements sql with
            | Error e -> err e
            | Ok statements when isReadOnlyBatch statements -> runRead statements
            | Ok statements -> runExclusive statements

    /// transaction 操作（BEGIN / COMMIT / ROLLBACK）の共通経路。
    /// 排他ロックの下でスナップショットを取り、適用後のハンドルを公開する。
    let runTransaction (operation: Database -> Result<Database, DbError>) =
        lock writeGate (fun () ->
            match currentHandle () with
            | None -> err (closedError ())
            | Some db ->
                let db, lease = db.AcquireSnapshot()
                use _lease = lease

                operation db
                |> Result.map (fun next ->
                    publish next
                    ()))

    /// DB を開く（background compaction 有効）。
    static member OpenWith(root: string) =
        Database.OpenWithBackground root |> Result.map (fun db -> new Session(root, db))

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

                let rollbackError, closeTarget =
                    if db.InTransaction then
                        match db.Rollback() with
                        | Error e -> Some e, db
                        | Ok rolledBack -> None, rolledBack
                    else
                        None, db

                let closeResult = closeTarget.Close()

                match rollbackError with
                | Some e -> err e
                | None -> closeResult)

    interface System.IDisposable with
        member this.Dispose() = this.Close() |> ignore
