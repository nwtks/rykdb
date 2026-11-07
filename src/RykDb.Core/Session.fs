// ===========================================================================
// Session — 公開エントリポイント。
// ライブラリ利用者・CLI・TCP サーバはこの層だけを見る。
//
//   let session = Session.OpenWith root
//   let result = session.Execute "INSERT ..."
//   session.Begin() / session.Commit() / session.Rollback() / session.Close()
//
// * Database の不変ハンドルを内部に隠し、常に最新の状態を保持する。
//   公開メソッドはハンドルを返さないので、戻り値の差し替え忘れは起こらない。
// * 1つの Session への呼び出しはロックで直列化する。
//   1 Session = 1 論理接続として扱う。
// * Execute は SQL を 1文ずつ実行し、成功するたびに内部ハンドルを更新する。
//   途中の文が失敗しても、最後に成功した状態は失わない。
// ===========================================================================

namespace RykDb.Core

/// データベースへの直列化されたセッション。
/// <remarks>
/// `Database` の不変ハンドルを内部に隠し、常に最新の状態を保持する。
/// 1つの Session への呼び出しはロックで直列化される。
/// 複数接続が必要な場合は、同じ root を複数開くのではなく DB を分けること。
/// </remarks>
type Session internal (root: string, initial: Database) =
    let gate = obj ()
    let mutable current = initial
    let mutable closed = false

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

    /// DB を開く。
    static member OpenWith(root: string) =
        Database.OpenWith root |> Result.map (fun db -> new Session(root, db))

    /// DB のルートディレクトリ。
    member _.Root = root

    /// 実行中の transaction があるか。
    member _.InTransaction = lock gate (fun () -> not closed && current.InTransaction)

    /// テーブル一覧を返す。
    member _.TableNames() =
        lock gate (fun () ->
            if closed then
                Error(closedError ())
            else
                Ok(current.TableNames()))

    /// 単一文または複数文を実行する。
    /// <remarks>
    /// 文は順に実行され、成功するたびに内部ハンドルが更新される。
    /// 途中の文が失敗した場合、それ以前に成功した文の状態は保持される。
    /// `BEGIN` 済みなら transaction は開いたままになるので、
    /// 必要に応じて `Rollback()` すること。
    /// </remarks>
    member _.Execute(sql: string) =
        let rec run acc =
            function
            | [] -> Ok(List.rev acc)
            | stmt :: tail ->
                match current.Execute stmt with
                | Error e -> Error e
                | Ok(next, result) ->
                    current <- next
                    run (result :: acc) tail

        lock gate (fun () ->
            if closed then
                Error(closedError ())
            else
                Parser.parseStatements sql >>= run [] |> Result.map summarize)

    /// トランザクションを始める。
    member _.Begin() =
        lock gate (fun () ->
            if closed then
                Error(closedError ())
            else
                current.Begin()
                |> Result.map (fun next ->
                    current <- next
                    ()))

    /// COMMIT。
    member _.Commit() =
        lock gate (fun () ->
            if closed then
                Error(closedError ())
            else
                current.Commit()
                |> Result.map (fun next ->
                    current <- next
                    ()))

    /// ROLLBACK。
    member _.Rollback() =
        lock gate (fun () ->
            if closed then
                Error(closedError ())
            else
                current.Rollback()
                |> Result.map (fun next ->
                    current <- next
                    ()))

    /// DB を閉じる。
    /// <remarks>
    /// 進行中の transaction は rollback してから閉じる。
    /// 二重 Close は安全。Close 後の呼び出しはエラーになる。
    /// </remarks>
    member _.Close() =
        lock gate (fun () ->
            if closed then
                Ok()
            else
                closed <- true

                let rollbackError =
                    if current.InTransaction then
                        match current.Rollback() with
                        | Error e -> Some e
                        | Ok rolledBack ->
                            current <- rolledBack
                            None
                    else
                        None

                let closeResult = current.Close()

                match rollbackError with
                | Some e -> Error e
                | None -> closeResult)

    interface System.IDisposable with
        member this.Dispose() = this.Close() |> ignore
