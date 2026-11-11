// ===========================================================================
// RykDb CLI — SQL 実行と表示の共通部分。
//   REPL / ファイル実行の両方から使い、テストからも直接検証できるようにする。
// ===========================================================================

module RykDb.Cli

open System.IO
open RykDb.Core

let piecesOf (input: string) =
    let pieces = ResizeArray<string>()
    let buf = System.Text.StringBuilder()

    let flush () =
        let text = buf.ToString().Trim()

        if not (System.String.IsNullOrWhiteSpace text) then
            pieces.Add text

        buf.Clear() |> ignore

    let rec loop i inString =
        if i >= input.Length then
            flush ()
            pieces.ToArray()
        else
            let c = input.[i]

            if inString then
                buf.Append c |> ignore

                if c = '\'' then
                    if i + 1 < input.Length && input.[i + 1] = '\'' then
                        buf.Append '\'' |> ignore
                        loop (i + 2) true
                    else
                        loop (i + 1) false
                else
                    loop (i + 1) true
            else
                match c with
                | '\'' ->
                    buf.Append c |> ignore
                    loop (i + 1) true
                | ';' ->
                    flush ()
                    loop (i + 1) false
                | _ ->
                    buf.Append c |> ignore
                    loop (i + 1) false

    loop 0 false

let splitStatements = piecesOf >> Array.toList

let private writeResult (out: TextWriter) (r: StatementResult) =
    r.ToDisplayLines() |> List.iter out.WriteLine

/// `RowReader` を materialize せずに表示する。
/// `StatementResult.ToDisplayLines()` と同じ形式（ヘッダ・区切り・2スペース字下げ）にする。
let private writeReader (out: TextWriter) (reader: RowReader) =
    let header = System.String.Join(" | ", reader.Columns)
    out.WriteLine header
    out.WriteLine(System.String('-', header.Length))

    let rec loop () =
        match reader.TryRead() with
        | Error e -> Error e
        | Ok None -> Ok()
        | Ok(Some row) ->
            row
            |> Array.map SqlLiteral.toSqlLiteral
            |> String.concat ", "
            |> fun text -> out.WriteLine("  " + text)

            loop ()

    loop ()

/// 単一の SQL を実行して表示する。
/// SELECT は `TryOpenReader` で逐次表示し、それ以外は通常の `Execute` を使う。
let executeSql (out: TextWriter) (session: Session) (sql: string) =
    let trimmed = sql.Trim()

    match session.TryOpenReader trimmed with
    | Ok(Some reader) ->
        use reader = reader
        writeReader out reader
    | Ok None -> session.Execute trimmed |> Result.map (writeResult out)
    | Error e -> Error e

/// セミコロン区切りのスクリプトを順に実行する。最初のエラーで停止する。
let runScript (out: TextWriter) (session: Session) (sql: string) =
    let rec loop =
        function
        | [] -> Ok()
        | stmt :: tail ->
            match executeSql out session stmt with
            | Ok() -> loop tail
            | Error e -> Error e

    splitStatements sql |> loop
