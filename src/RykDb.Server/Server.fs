// ===========================================================================
// RykDb.Server — プロトコルとサーバ本体。
// 1接続 = 1クライアント、逐次処理。
// 改行区切りの 1行テキストを交換し、応答の先頭タグで成否を分ける:
//   OK <行数>\t<列名,...>   成功(続く行がデータ)
//   ERR <メッセージ>        失敗
//   HALT                    終了
//
// テストや他のホストは serve / handleClient を直接使える。
// ===========================================================================

module RykDb.Server

open System
open System.IO
open System.Net.Sockets
open System.Text
open System.Threading
open RykDb.Core

[<Literal>]
let OkTag = "OK"

[<Literal>]
let ErrTag = "ERR"

[<Literal>]
let HaltTag = "HALT"

let private encode (columns: string array) (rows: SqlValue array array) =
    if rows.Length = 0 then
        sprintf "%s %d" OkTag rows.Length
    else
        let header = sprintf "%s %d\t%s" OkTag rows.Length (String.Join(",", columns))

        let body =
            rows
            |> Array.map (fun r -> r |> Array.map SqlLiteral.toSqlLiteral |> String.concat ",")

        String.Join("\n", Array.append [| header |] body)

let private handleClient (db: Database) (client: TcpClient) (onLog: string -> unit) =
    use stream = client.GetStream()
    use reader = new StreamReader(stream, Encoding.UTF8, false, 1024, true)
    use writer = new StreamWriter(stream, Encoding.UTF8, 1024, true)
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
                running <- send ""
            else
                match sql with
                | ".exit"
                | ".quit" ->
                    send HaltTag |> ignore
                    running <- false
                | ".ping" -> running <- send (sprintf "%s 0\tpong" OkTag)
                | ".tables" ->
                    let names = current.TableNames()

                    let body =
                        if List.isEmpty names then
                            sprintf "%s 0" OkTag
                        else
                            encode [| "tables" |] (names |> List.map (fun n -> [| SqlValue.Text n |]) |> List.toArray)

                    running <- send body
                | _ ->
                    match current.Execute sql with
                    | Error e -> running <- send (sprintf "%s %s" ErrTag (DbError.toDisplayString e))
                    | Ok(next, result) ->
                        current <- next
                        running <- send (encode result.Columns result.Rows)

    onLog "切断されました"

let serve (db: Database) (listener: TcpListener) (stop: CancellationToken) (onLog: string -> unit) =
    use _ =
        stop.Register(fun _ ->
            try
                listener.Stop()
            with _ ->
                ())

    let mutable clientId = 0

    while not stop.IsCancellationRequested do
        try
            let client = listener.AcceptTcpClient()
            clientId <- clientId + 1
            onLog (sprintf "[%d] 接続されました" clientId)

            try
                handleClient db client (fun m -> onLog (sprintf "[%d] %s" clientId m))
            with ex ->
                onLog (sprintf "[%d] 例外: %s" clientId ex.Message)

            try
                if client.Client.Connected then
                    client.Client.Shutdown SocketShutdown.Send
            with _ ->
                ()

            client.Close()
            onLog (sprintf "[%d] 切断されました" clientId)
        with
        | :? ObjectDisposedException -> ()
        | :? InvalidOperationException when stop.IsCancellationRequested -> ()
        | :? SocketException when stop.IsCancellationRequested -> ()
        | :? SocketException -> ()
        | ex ->
            if not stop.IsCancellationRequested then
                onLog (sprintf "接続処理で例外: %s" ex.Message)

    listener.Stop()
