// ===========================================================================
// RykDb.Client — サーバ参照用の TCP クライアント
//
// RykDbClient は 1行ずつ問い合わせ、応答をタグ(OK / ERR / HALT)で受け取る。
// テストでは CLI と同じ経路でサーバを叩ける。
// ===========================================================================

module RykDb.Client

open RykDb.Server

/// サーバの応答。
type Reply =
    | Rows of columns: string list * rows: string list
    | Failure of string
    | Halt

/// TCP 経由の SQL クライアント。
type RykDbClient(host: string, port: int) =
    let client = new System.Net.Sockets.TcpClient(host, port)
    let stream = client.GetStream()

    let reader =
        new System.IO.StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)

    let writer =
        new System.IO.StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)

    do writer.AutoFlush <- true

    member _.Send(line: string) =
        writer.WriteLine line
        writer.Flush()

        let header =
            try
                let h = reader.ReadLine()
                if isNull h then None else Some h
            with
            | :? System.IO.IOException -> None
            | :? System.Net.Sockets.SocketException -> None
            | :? System.ObjectDisposedException -> None

        match header with
        | None -> Halt
        | Some h when h.StartsWith HaltTag -> Halt
        | Some h when h.StartsWith ErrTag -> Failure(h.Substring(ErrTag.Length).Trim())
        | Some h ->
            let body = h.Substring(OkTag.Length).Trim()

            match body.Split([| ' '; '\t' |], System.StringSplitOptions.RemoveEmptyEntries) with
            | [||] -> Rows([], [])
            | parts ->
                let count =
                    match System.Int32.TryParse parts.[0] with
                    | true, n when n >= 0 -> n
                    | _ -> 0

                if parts.Length < 2 then
                    Rows([], [])
                else
                    let columns = parts.[1].Split ',' |> Array.toList

                    let rows =
                        [ 1..count ]
                        |> List.choose (fun _ ->
                            let r = reader.ReadLine()

                            if isNull r then None else Some r)

                    Rows(columns, rows)

    member this.Query(sql: string) = this.Send sql

    member this.Close() =
        try
            this.Send ".exit" |> ignore
        with _ ->
            ()

        writer.Dispose()
        reader.Dispose()
        stream.Dispose()
        client.Dispose()

    interface System.IDisposable with
        member this.Dispose() = this.Close()
