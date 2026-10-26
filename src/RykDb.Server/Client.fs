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

/// サーバがプロトコルに載せるために付けたエスケープ
/// （`\\` / `\n` / `\r`）を 1パスで元へ戻す。
let private unescapeLine (s: string) =
    let sb = System.Text.StringBuilder s.Length

    let rec loop i =
        if i >= s.Length then
            sb.ToString()
        else
            match s.[i] with
            | '\\' when i + 1 < s.Length ->
                match s.[i + 1] with
                | 'n' ->
                    sb.Append '\n' |> ignore
                    loop (i + 2)
                | 'r' ->
                    sb.Append '\r' |> ignore
                    loop (i + 2)
                | '\\' ->
                    sb.Append '\\' |> ignore
                    loop (i + 2)
                | _ ->
                    sb.Append '\\' |> ignore
                    loop (i + 1)
            | c ->
                sb.Append c |> ignore
                loop (i + 1)

    loop 0

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
        let rec readRows columns remaining acc =
            if remaining = 0 then
                Rows(columns, List.rev acc)
            else
                match reader.ReadLine() with
                | null -> Failure "サーバ応答が途中で切断されました"
                | row -> readRows columns (remaining - 1) (unescapeLine row :: acc)

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
        | Some h when h.StartsWith(OkTag + " ", System.StringComparison.Ordinal) ->
            let tab = h.IndexOf '\t'
            let status = if tab < 0 then h else h.Substring(0, tab)
            let columnsText = if tab < 0 then None else Some(h.Substring(tab + 1))
            let countText = status.Substring(OkTag.Length).Trim()

            match System.Int32.TryParse countText, columnsText with
            | (true, count), columnsText when count >= 0 ->
                let columns =
                    columnsText
                    |> Option.map (fun text ->
                        text.Split ',' |> Array.map System.Uri.UnescapeDataString |> Array.toList)
                    |> Option.defaultValue []

                readRows columns count []
            | _ -> Failure(sprintf "不正なサーバ応答ヘッダー: %s" h)
        | Some h -> Failure(sprintf "不正なサーバ応答: %s" h)

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
