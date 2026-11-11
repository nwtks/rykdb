// ===========================================================================
// RykDb.Server — プロトコルとサーバ本体。
// 1接続 = 1クライアント。接続ごとに Task を起動して並行に処理する。
// 複数接続は 1つの Session を共有する（SELECT は並行、書き込みは Session 内で直列）。
// 改行区切りの 1行テキストを交換し、応答の先頭タグで成否を分ける:
//   OK <行数>\t<列名,...>   成功(続く行がデータ。SELECT は0行でも列名を返す)
//   OK <行数>               DML など列を持たない成功
//   STREAM\t<列名,...>      ストリーミング SELECT のヘッダ(続く行がデータ、END で終端)
//   END                     STREAM の終端
//   ERR <メッセージ>        失敗
//   HALT                    終了
//
// `.stream <SQL>` は単一 SELECT を materialize せずに STREAM で返す。
// 対象外（複数文・非 SELECT・transaction 中など）は通常の OK / ERR にフォールバックする。
//
// データ行の中のバックスラッシュ・改行・CR は `\\` / `\n` / `\r` にエスケープする
// （TEXT に生の改行が入っていても 1応答 = 1行を保つ）。
// クライアント側は RykDb.Client が元へ戻す。
//
// テストや他のホストは serve / handleClient を直接使える。
// ===========================================================================

module RykDb.Server

open RykDb.Core

[<Literal>]
let OkTag = "OK"

[<Literal>]
let ErrTag = "ERR"

[<Literal>]
let HaltTag = "HALT"

[<Literal>]
let StreamTag = "STREAM"

[<Literal>]
let EndTag = "END"

/// 応答の 1データ行をプロトコルに載せられる形にする。
/// バックスラッシュ・改行・CR をエスケープする（クライアント側で元へ戻す）。
let private escapeLine (s: string) =
    let sb = System.Text.StringBuilder s.Length

    s
    |> String.iter (function
        | '\\' -> sb.Append "\\\\" |> ignore
        | '\n' -> sb.Append "\\n" |> ignore
        | '\r' -> sb.Append "\\r" |> ignore
        | c -> sb.Append c |> ignore)

    sb.ToString()

let private encodedColumns (columns: string array) =
    columns |> Array.map System.Uri.EscapeDataString |> String.concat ","

/// 列名がある場合だけタブ + 列名を付ける（既存の OK ヘッダーと同じ形）。
let private columnsSuffix (columns: string array) =
    if columns.Length = 0 then
        ""
    else
        "\t" + encodedColumns columns

let private rowLine =
    Array.map SqlLiteral.toSqlLiteral >> String.concat "," >> escapeLine

/// 応答を 1行ずつ書き出す。
/// 結果全体を 1本の文字列へ結合しないので、
/// 大結果で「行ごとの文字列 + 結合結果」の 2枚分のコピーが要らない。
/// 戻り値は「書き出せたか」（送信失敗で接続を閉じるため）。
let private sendRows (writer: System.IO.StreamWriter) (columns: string array) (rows: SqlValue array array) =
    let header = sprintf "%s %d%s" OkTag rows.Length (columnsSuffix columns)

    try
        writer.AutoFlush <- false

        try
            writer.WriteLine header

            if rows.Length > 0 then
                for row in rows do
                    rowLine row |> writer.WriteLine

            writer.Flush()
        finally
            writer.AutoFlush <- true

        true
    with _ ->
        false

let private handleClient (onLog: string -> unit) (session: Session) (client: System.Net.Sockets.TcpClient) =
    use stream = client.GetStream()

    use reader =
        new System.IO.StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)

    use writer =
        new System.IO.StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)

    writer.AutoFlush <- true

    let mutable ownsTransaction = false

    let runSql sql =
        match session.ExecuteTracked sql with
        | Error e -> Error e
        | Ok(result, change) ->
            match change with
            | SessionTransactionChange.Started -> ownsTransaction <- true
            | SessionTransactionChange.Ended -> ownsTransaction <- false
            | SessionTransactionChange.NoChange -> ()

            Ok result

    let send (line: string) =
        try
            writer.WriteLine line
            writer.Flush()
            true
        with _ ->
            false

    let sendResult result =
        if Array.isEmpty result.Columns then
            sprintf "%s %d" OkTag result.RowsAffected |> send
        else
            sendRows writer result.Columns result.Rows

    let sendStream sql =
        match session.TryOpenReader sql with
        | Error e -> sprintf "%s %s" ErrTag (DbError.toDisplayString e) |> send
        | Ok None ->
            match runSql sql with
            | Error e -> sprintf "%s %s" ErrTag (DbError.toDisplayString e) |> send
            | Ok result -> sendResult result
        | Ok(Some rowReader) ->
            use rowReader = rowReader
            let header = sprintf "%s%s" StreamTag (columnsSuffix rowReader.Columns)

            if not (send header) then
                false
            else
                let rec loop () =
                    match rowReader.TryRead() with
                    | Error e -> sprintf "%s %s" ErrTag (DbError.toDisplayString e) |> send
                    | Ok None -> send EndTag
                    | Ok(Some row) -> if send (rowLine row) then loop () else false

                loop ()

    let mutable running = true

    while running do
        let line = reader.ReadLine()

        if isNull line then
            running <- false
        else
            let sql = line.Trim()

            if sql.Length = 0 then
                running <- sprintf "%s 0" OkTag |> send
            else
                match sql with
                | ".exit"
                | ".quit" ->
                    send HaltTag |> ignore
                    running <- false
                | ".ping" -> running <- sprintf "%s 0\tpong" OkTag |> send
                | ".tables" ->
                    match session.TableNames() with
                    | Error e -> running <- sprintf "%s %s" ErrTag (DbError.toDisplayString e) |> send
                    | Ok names ->
                        if List.isEmpty names then
                            running <- send (sprintf "%s 0" OkTag)
                        else
                            let rows = names |> List.map (fun n -> [| SqlValue.Text n |]) |> List.toArray
                            running <- sendRows writer [| "tables" |] rows
                | ".stream" -> running <- sprintf "%s .stream には SQL が必要です" ErrTag |> send
                | _ when sql.StartsWith(".stream ", System.StringComparison.Ordinal) ->
                    running <- sendStream (sql.Substring(".stream ".Length).Trim())
                | _ ->
                    match runSql sql with
                    | Error e -> running <- sprintf "%s %s" ErrTag (DbError.toDisplayString e) |> send
                    | Ok result -> running <- sendResult result

    if ownsTransaction && session.InTransaction then
        match session.Rollback() with
        | Ok() -> ()
        | Error e -> sprintf "切断時のロールバックに失敗しました: %s" (DbError.toDisplayString e) |> onLog

let serve
    (session: Session)
    (listener: System.Net.Sockets.TcpListener)
    (stop: System.Threading.CancellationToken)
    (onLog: string -> unit)
    =
    use _ =
        stop.Register(fun _ ->
            try
                listener.Stop()
            with _ ->
                ())

    let clients =
        System.Collections.Concurrent.ConcurrentDictionary<int, System.Net.Sockets.TcpClient>()

    let handlers =
        System.Collections.Concurrent.ConcurrentDictionary<int, System.Threading.Tasks.Task>()

    let handleConnection clientId client =
        try
            handleClient (fun m -> sprintf "[%d] %s" clientId m |> onLog) session client
        with ex ->
            sprintf "[%d] 例外: %s" clientId ex.Message |> onLog

        try
            if client.Client.Connected then
                client.Client.Shutdown System.Net.Sockets.SocketShutdown.Send
        with _ ->
            ()

        client.Close()
        clients.TryRemove clientId |> ignore
        handlers.TryRemove clientId |> ignore
        sprintf "[%d] 切断されました" clientId |> onLog

    let mutable clientId = 0
    let idGate = obj ()

    while not stop.IsCancellationRequested do
        try
            let client = listener.AcceptTcpClient()
            client.ReceiveTimeout <- 30000

            let cid =
                lock idGate (fun () ->
                    clientId <- clientId + 1
                    clientId)

            clients.[cid] <- client
            sprintf "[%d] 接続されました" cid |> onLog
            handlers.[cid] <- System.Threading.Tasks.Task.Run(fun () -> handleConnection cid client)
        with
        | :? System.ObjectDisposedException -> ()
        | :? System.InvalidOperationException when stop.IsCancellationRequested -> ()
        | :? System.Net.Sockets.SocketException when stop.IsCancellationRequested -> ()
        | :? System.Net.Sockets.SocketException -> ()
        | ex ->
            if not stop.IsCancellationRequested then
                sprintf "接続処理で例外: %s" ex.Message |> onLog

    listener.Stop()

    for client in clients.Values do
        try
            client.Close()
        with _ ->
            ()

    let pending = handlers.Values |> Seq.toArray

    if pending.Length > 0 then
        System.Threading.Tasks.Task.WaitAll(pending, 10000) |> ignore

    session.Close() |> ignore
