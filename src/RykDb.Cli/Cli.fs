// ===========================================================================
// RykDb CLI — SQL 実行と表示の共通部分。
//   REPL / ファイル実行の両方から使い、テストからも直接検証できるようにする。
// ===========================================================================

module RykDb.Cli

open System.IO
open RykDb.Core

/// バッファに溜めた断片を 1文として確定し、pieces に追加する。
let private flushPieces (buf: System.Text.StringBuilder) (pieces: ResizeArray<string>) =
    let text = buf.ToString().Trim()

    if not (System.String.IsNullOrWhiteSpace text) then
        pieces.Add text

    buf.Clear() |> ignore

/// 入力を 1文字ずつ走査し、引用符とセミコロンで文を切り分ける。
[<TailCall>]
let rec private piecesLoop
    (input: string)
    (buf: System.Text.StringBuilder)
    (pieces: ResizeArray<string>)
    (i: int)
    (inString: bool)
    =
    if i >= input.Length then
        flushPieces buf pieces
        pieces.ToArray()
    else
        let c = input.[i]

        if inString then
            buf.Append c |> ignore

            if c = '\'' then
                if i + 1 < input.Length && input.[i + 1] = '\'' then
                    buf.Append '\'' |> ignore
                    piecesLoop input buf pieces (i + 2) true
                else
                    piecesLoop input buf pieces (i + 1) false
            else
                piecesLoop input buf pieces (i + 1) true
        else
            match c with
            | '\'' ->
                buf.Append c |> ignore
                piecesLoop input buf pieces (i + 1) true
            | ';' ->
                flushPieces buf pieces
                piecesLoop input buf pieces (i + 1) false
            | _ ->
                buf.Append c |> ignore
                piecesLoop input buf pieces (i + 1) false

let piecesOf (input: string) =
    let pieces = ResizeArray<string>()
    let buf = System.Text.StringBuilder()
    piecesLoop input buf pieces 0 false

let private splitStatements = piecesOf >> Array.toList

/// EXPLAIN の結果か（単一の TEXT 列 `plan`）。
/// 計画行は人間向けのテキストなので、TEXT の引用符を付けずに表示する。
let private isPlanResult (r: StatementResult) =
    r.Columns.Length = 1
    && r.Columns.[0] = "plan"
    && r.Rows
       |> Array.forall (fun row ->
           row.Length = 1
           && match row.[0] with
              | SqlValue.Text _ -> true
              | _ -> false)

let private writeResult (out: TextWriter) (r: StatementResult) =
    if isPlanResult r then
        let header = r.Columns.[0]
        out.WriteLine header
        out.WriteLine(System.String('-', header.Length))

        for row in r.Rows do
            match row.[0] with
            | SqlValue.Text line -> out.WriteLine("  " + line)
            | other -> out.WriteLine("  " + SqlLiteral.toSqlLiteral other)
    else
        r.ToDisplayLines() |> List.iter out.WriteLine

/// RowReader を末尾まで読み、各行を表示して次の読み出しへ進む。
[<TailCall>]
let rec private writeReaderLoop (out: TextWriter) (reader: RowReader) =
    match reader.TryRead() with
    | Error e -> Error e
    | Ok None -> Ok()
    | Ok(Some row) ->
        row
        |> LoadedRow.toArray
        |> Array.map SqlLiteral.toSqlLiteral
        |> String.concat ", "
        |> fun text -> out.WriteLine("  " + text)

        writeReaderLoop out reader

/// `RowReader` を materialize せずに表示する。
/// `StatementResult.ToDisplayLines()` と同じ形式（ヘッダ・区切り・2スペース字下げ）にする。
let private writeReader (out: TextWriter) (reader: RowReader) =
    let header = System.String.Join(" | ", reader.Columns)
    out.WriteLine header
    out.WriteLine(System.String('-', header.Length))
    writeReaderLoop out reader

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

/// 文を先頭から順に実行し、最初のエラーで停止する。
[<TailCall>]
let rec private runScriptLoop (out: TextWriter) (session: Session) =
    function
    | [] -> Ok()
    | stmt :: tail ->
        match executeSql out session stmt with
        | Ok() -> runScriptLoop out session tail
        | Error e -> Error e

/// セミコロン区切りのスクリプトを順に実行する。最初のエラーで停止する。
let runScript (out: TextWriter) (session: Session) (sql: string) =
    splitStatements sql |> runScriptLoop out session
