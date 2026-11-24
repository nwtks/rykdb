// ===========================================================================
// RykDb.Client — サーバ参照用の TCP クライアント
//
// RykDbClient は 1行ずつ問い合わせ、応答をタグ(OK / STREAM / ERR / HALT)で受け取る。
// テストでは CLI と同じ経路でサーバを叩ける。
// ===========================================================================

module RykDb.Client

open RykDb.Server

/// サーバの応答。
type Reply =
    /// 通常の OK 応答を materialize したもの。
    | Rows of columns: string list * rows: string list
    /// 列を持たない OK 応答（DML / DDL）の影響行数。
    | Affected of rowsAffected: int
    /// STREAM / `.stream` の応答。行はコールバックで受け取り、ここには件数だけ残す。
    | Streamed of columns: string list * rows: int
    | Failure of string
    | Halt

/// エスケープを 1文字ずつ元へ戻して sb へ積む（末尾再帰）。
[<TailCall>]
let rec private unescapeLineLoop (sb: System.Text.StringBuilder) (s: string) (i: int) =
    if i >= s.Length then
        sb.ToString()
    else
        match s.[i] with
        | '\\' when i + 1 < s.Length ->
            match s.[i + 1] with
            | 'n' ->
                sb.Append '\n' |> ignore
                unescapeLineLoop sb s (i + 2)
            | 'r' ->
                sb.Append '\r' |> ignore
                unescapeLineLoop sb s (i + 2)
            | '\\' ->
                sb.Append '\\' |> ignore
                unescapeLineLoop sb s (i + 2)
            | _ ->
                sb.Append '\\' |> ignore
                unescapeLineLoop sb s (i + 1)
        | c ->
            sb.Append c |> ignore
            unescapeLineLoop sb s (i + 1)

/// サーバがプロトコルに載せるために付けたエスケープ
/// （`\\` / `\n` / `\r`）を 1パスで元へ戻す。
let private unescapeLine (s: string) =
    let sb = System.Text.StringBuilder s.Length
    unescapeLineLoop sb s 0

/// STREAM 応答の行を onRow へ渡しながら END まで読む（末尾再帰）。
[<TailCall>]
let rec private readStreamRows
    (tryReadLine: unit -> string option)
    (columns: string list)
    (onRow: string -> unit)
    (count: int)
    =
    match tryReadLine () with
    | None -> Failure "サーバ応答が途中で切断されました"
    | Some line when line = EndTag -> Streamed(columns, count)
    | Some line when line.StartsWith(ErrTag, System.StringComparison.Ordinal) ->
        Failure(line.Substring(ErrTag.Length).Trim())
    | Some line ->
        onRow (unescapeLine line)
        readStreamRows tryReadLine columns onRow (count + 1)

/// 通常 OK にフォールバックした場合の行を onRow へ渡しながら読む（末尾再帰）。
[<TailCall>]
let rec private readFallbackRows
    (tryReadLine: unit -> string option)
    (columns: string list)
    (onRow: string -> unit)
    (count: int)
    (remaining: int)
    =
    if remaining = 0 then
        Streamed(columns, count)
    else
        match tryReadLine () with
        | None -> Failure "サーバ応答が途中で切断されました"
        | Some line ->
            onRow (unescapeLine line)
            readFallbackRows tryReadLine columns onRow count (remaining - 1)

/// TCP 経由の SQL クライアント。
type RykDbClient(host: string, port: int) =
    let client = new System.Net.Sockets.TcpClient(host, port)
    let stream = client.GetStream()

    let reader =
        new System.IO.StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)

    let writer =
        new System.IO.StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)

    do writer.AutoFlush <- true

    let tryReadLine () =
        try
            let line = reader.ReadLine()
            if isNull line then None else Some line
        with
        | :? System.IO.IOException -> None
        | :? System.Net.Sockets.SocketException -> None
        | :? System.ObjectDisposedException -> None

    let parseColumns (text: string) =
        if text.Length = 0 then
            []
        else
            text.Split ',' |> Array.map System.Uri.UnescapeDataString |> Array.toList

    /// ヘッダのタブ以降を列名として読む。タブが無ければ列なし。
    let columnsOfHeader (header: string) =
        let tab = header.IndexOf '\t'

        if tab < 0 then
            []
        else
            parseColumns (header.Substring(tab + 1))

    let parseOkHeader (header: string) =
        let tab = header.IndexOf '\t'
        let status = if tab < 0 then header else header.Substring(0, tab)
        let columns = columnsOfHeader header
        let countText = status.Substring(OkTag.Length).Trim()

        match System.Int32.TryParse countText with
        | true, count when count >= 0 -> Some(columns, count)
        | _ -> None

    [<TailCall>]
    let rec readRows (columns: string list) (remaining: int) (acc: string list) =
        if remaining = 0 then
            Rows(columns, List.rev acc)
        else
            match tryReadLine () with
            | None -> Failure "サーバ応答が途中で切断されました"
            | Some row -> readRows columns (remaining - 1) (unescapeLine row :: acc)

    [<TailCall>]
    let rec readStream (columns: string list) (acc: string list) =
        match tryReadLine () with
        | None -> Failure "サーバ応答が途中で切断されました"
        | Some line when line = EndTag -> Rows(columns, List.rev acc)
        | Some line when line.StartsWith(ErrTag, System.StringComparison.Ordinal) ->
            Failure(line.Substring(ErrTag.Length).Trim())
        | Some line -> readStream columns (unescapeLine line :: acc)

    member _.Send(line: string) =
        writer.WriteLine line
        writer.Flush()

        match tryReadLine () with
        | None -> Halt
        | Some h when h.StartsWith HaltTag -> Halt
        | Some h when h.StartsWith ErrTag -> Failure(h.Substring(ErrTag.Length).Trim())
        | Some h when h = StreamTag || h.StartsWith(StreamTag + "\t", System.StringComparison.Ordinal) ->
            readStream (columnsOfHeader h) []
        | Some h when h.StartsWith(OkTag + " ", System.StringComparison.Ordinal) ->
            match parseOkHeader h with
            | Some([], affected) -> Affected affected
            | Some(columns, count) -> readRows columns count []
            | None -> Failure(sprintf "不正なサーバ応答ヘッダー: %s" h)
        | Some h -> Failure(sprintf "不正なサーバ応答: %s" h)

    /// 単一 SELECT を `.stream` で要求し、行ごとに `onRow` へ渡す。
    /// サーバが通常の OK にフォールバックした場合も、その行を `onRow` に渡す。
    /// 行の蓄積はしないため、大きな結果でもクライアント側のメモリは行数に比例しない。
    member _.SendStream(sql: string, onRow: string -> unit) =
        let request =
            if sql = ".stream" || sql.StartsWith(".stream ", System.StringComparison.Ordinal) then
                sql
            else
                ".stream " + sql

        writer.WriteLine request
        writer.Flush()

        match tryReadLine () with
        | None -> Halt
        | Some h when h.StartsWith HaltTag -> Halt
        | Some h when h.StartsWith ErrTag -> Failure(h.Substring(ErrTag.Length).Trim())
        | Some h when h = StreamTag || h.StartsWith(StreamTag + "\t", System.StringComparison.Ordinal) ->
            let columns = columnsOfHeader h
            readStreamRows tryReadLine columns onRow 0
        | Some h when h.StartsWith(OkTag + " ", System.StringComparison.Ordinal) ->
            match parseOkHeader h with
            | None -> Failure(sprintf "不正なサーバ応答ヘッダー: %s" h)
            | Some([], affected) -> Affected affected
            | Some(columns, count) -> readFallbackRows tryReadLine columns onRow count count
        | Some h -> Failure(sprintf "不正なサーバ応答: %s" h)

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
