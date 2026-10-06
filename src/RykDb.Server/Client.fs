// ===========================================================================
// RykDb.Client — サーバ参照用の TCP クライアント
//
// RykDbClient は 1行ずつ問い合わせ、応答をタグ(OK / ERR / HALT)で受け取る。
// テストでは CLI と同じ経路でサーバを叩ける。
// ===========================================================================

module RykDb.Client

open System
open System.IO
open System.Net.Sockets
open System.Text
open RykDb.Server

/// サーバの応答。
type Reply =
    | Rows of columns: string list * rows: string list
    | Failure of string
    | Halt

/// TCP 経由の SQL クライアント。
type RykDbClient(host: string, port: int) =
    let client = new TcpClient(host, port)
    let stream = client.GetStream()
    let reader = new StreamReader(stream, Encoding.UTF8, false, 1024, true)
    let writer = new StreamWriter(stream, Encoding.UTF8, 1024, true)
    do writer.AutoFlush <- true

    member _.Send(line: string) =
        writer.WriteLine line
        writer.Flush()

        let header =
            try
                let h = reader.ReadLine()
                if isNull h then None else Some h
            with
            | :? IOException -> None
            | :? SocketException -> None
            | :? ObjectDisposedException -> None

        match header with
        | None -> Halt
        | Some h when h.StartsWith HaltTag -> Halt
        | Some h when h.StartsWith ErrTag -> Failure(h.Substring(ErrTag.Length).Trim())
        | Some h ->
            let body = h.Substring(OkTag.Length).Trim()

            match body.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) with
            | [||] -> Rows([], [])
            | parts ->
                let count =
                    match Int32.TryParse parts.[0] with
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

    interface IDisposable with
        member this.Dispose() = this.Close()
