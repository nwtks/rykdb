// ===========================================================================
// RykDb.Server — プロトコルとサーバ本体。
// 1接続 = 1クライアント、逐次処理。
// 改行区切りの 1行テキストを交換し、応答の先頭タグで成否を分ける:
//   OK <行数>\t<列名,...>   成功(続く行がデータ。SELECT は0行でも列名を返す)
//   OK <行数>               DML など列を持たない成功
//   ERR <メッセージ>        失敗
//   HALT                    終了
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

/// 応答を 1行ずつ書き出す。
/// 結果全体を 1本の文字列へ結合しないので、
/// 大結果で「行ごとの文字列 + 結合結果」の 2枚分のコピーが要らない。
/// 戻り値は「書き出せたか」（送信失敗で接続を閉じるため）。
let private sendRows (writer: System.IO.StreamWriter) (columns: string array) (rows: SqlValue array array) =
    let header =
        if columns.Length = 0 then
            sprintf "%s %d" OkTag rows.Length
        else
            sprintf
                "%s %d\t%s"
                OkTag
                rows.Length
                (System.String.Join(",", columns |> Array.map System.Uri.EscapeDataString))

    try
        writer.AutoFlush <- false

        try
            writer.WriteLine header

            if rows.Length > 0 then
                for row in rows do
                    row
                    |> Array.map SqlLiteral.toSqlLiteral
                    |> String.concat ","
                    |> escapeLine
                    |> writer.WriteLine

            writer.Flush()
        finally
            writer.AutoFlush <- true

        true
    with _ ->
        false

let private handleClient (onLog: string -> unit) (db: Database) (client: System.Net.Sockets.TcpClient) =
    use stream = client.GetStream()

    use reader =
        new System.IO.StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)

    use writer =
        new System.IO.StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)

    writer.AutoFlush <- true

    let send (line: string) =
        try
            writer.WriteLine line
            writer.Flush()
            true
        with _ ->
            false

    let mutable current = db
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
                    let names = current.TableNames()

                    if List.isEmpty names then
                        running <- send (sprintf "%s 0" OkTag)
                    else
                        let rows = names |> List.map (fun n -> [| SqlValue.Text n |]) |> List.toArray

                        running <- sendRows writer [| "tables" |] rows
                | _ ->
                    match current.Execute sql with
                    | Error e -> running <- sprintf "%s %s" ErrTag (DbError.toDisplayString e) |> send
                    | Ok(next, result) ->
                        current <- next
                        running <- sendRows writer result.Columns result.Rows

    if current.InTransaction then
        match current.Rollback() with
        | Ok rolledBack -> current <- rolledBack
        | Error e -> sprintf "切断時のロールバックに失敗しました: %s" (DbError.toDisplayString e) |> onLog

    "切断されました" |> onLog
    current

let serve
    (db: Database)
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

    let mutable clientId = 0
    let mutable currentDb = db

    while not stop.IsCancellationRequested do
        try
            let client = listener.AcceptTcpClient()
            client.ReceiveTimeout <- 30000
            clientId <- clientId + 1
            sprintf "[%d] 接続されました" clientId |> onLog

            try
                let finalDb =
                    handleClient (fun m -> sprintf "[%d] %s" clientId m |> onLog) currentDb client

                currentDb <- finalDb
            with ex ->
                sprintf "[%d] 例外: %s" clientId ex.Message |> onLog

            try
                if client.Client.Connected then
                    client.Client.Shutdown System.Net.Sockets.SocketShutdown.Send
            with _ ->
                ()

            client.Close()
            sprintf "[%d] 切断されました" clientId |> onLog
        with
        | :? System.ObjectDisposedException -> ()
        | :? System.InvalidOperationException when stop.IsCancellationRequested -> ()
        | :? System.Net.Sockets.SocketException when stop.IsCancellationRequested -> ()
        | :? System.Net.Sockets.SocketException -> ()
        | ex ->
            if not stop.IsCancellationRequested then
                sprintf "接続処理で例外: %s" ex.Message |> onLog

    listener.Stop()
    currentDb.Close() |> ignore
