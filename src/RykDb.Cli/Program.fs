// ===========================================================================
// RykDb CLI — 対話的な SQL シェル。
//   dotnet run --project src/RykDb.Cli -- [-f <SQL ファイル>] [DB のディレクトリ]
//
// 対話モード: エラーは表示するだけで終了しない。
// 入力の末尾が `;` ならば即実行、無ければ入力を続ける。
// ===========================================================================

open RykDb.Core

open RykDb.Cli

let private coloredLine (color: System.ConsoleColor) (emit: string -> unit) (line: string) =
    System.Console.ForegroundColor <- color
    emit line
    System.Console.ResetColor()

let private printError (e: DbError) =
    coloredLine System.ConsoleColor.Red (fun m -> eprintfn "エラー: %s" m) (DbError.toDisplayString e)

let private dotCommand (session: Session) (line: string) =
    let parts = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)

    match parts with
    | [| ".exit" |]
    | [| ".quit" |] -> false
    | [| ".help" |] ->
        printfn "コマンド:"
        printfn "  .tables             テーブル一覧"
        printfn "  .begin              transaction を開始"
        printfn "  .commit             現在の transaction を commit"
        printfn "  .rollback           現在の transaction を破棄"
        printfn "  .exit               終了"
        printfn ""
        printfn "それ以外は SQL として実行します(`;` で即実行)。"
        true
    | [| ".tables" |] ->
        match session.TableNames() with
        | Error e -> printError e
        | Ok names ->
            if List.isEmpty names then
                printfn "(テーブルはありません)"
            else
                names |> List.iter (fun n -> printfn "  %s" n)

        true
    | [| ".begin" |] ->
        match session.Begin() with
        | Ok() -> printfn "(begin しました)"
        | Error e -> printError e

        true
    | [| ".commit" |] ->
        match session.Commit() with
        | Ok() -> printfn "(commit しました)"
        | Error e -> printError e

        true
    | [| ".rollback" |] ->
        match session.Rollback() with
        | Ok() -> printfn "(rollback しました)"
        | Error e -> printError e

        true
    | _ ->
        coloredLine System.ConsoleColor.Yellow (fun m -> printfn "%s" m) "未知のコマンドです(`.help` でコマンド一覧)"
        true

let private banner (root: string) =
    printfn "RykDb REPL"
    printfn "DB: %s" root
    printfn "`.help` でコマンド一覧、`.exit` で終了"
    printfn ""

let private execOne (session: Session) (sql: string) =
    let trimmed = sql.Trim()

    if System.String.IsNullOrWhiteSpace trimmed then
        true
    elif trimmed.StartsWith "." then
        dotCommand session trimmed
    else
        match executeSql System.Console.Out session trimmed with
        | Ok() -> true
        | Error e ->
            printError e
            true

let private runRepl (session: Session) =
    let splitForRun merged =
        let pieces = piecesOf merged

        if Array.isEmpty pieces then
            [||], [||]
        else
            let complete = merged.EndsWith(";", System.StringComparison.Ordinal)
            let last = pieces.Length - 1

            if complete then
                pieces, [||]
            else
                pieces.[0 .. last - 1], pieces.[last..last]

    let mutable running = true
    let mutable buffer = ""

    while running do
        System.Console.Write(
            if System.String.IsNullOrWhiteSpace buffer then
                "db> "
            else
                " -> "
        )

        match System.Console.ReadLine() with
        | null -> running <- false
        | line ->
            let trimmed = line.Trim()

            if System.String.IsNullOrWhiteSpace buffer && trimmed.StartsWith "." then
                running <- execOne session trimmed
            else
                let merged =
                    if System.String.IsNullOrWhiteSpace buffer then
                        line
                    else
                        buffer + " " + line

                let toRun, toKeep = splitForRun merged

                for sql in toRun do
                    if running then
                        running <- execOne session sql

                buffer <- if Array.isEmpty toKeep then "" else toKeep.[0]

let private runFile (session: Session) (path: string) =
    if not (System.IO.File.Exists path) then
        Error(sprintf "ファイル %s がありません" path |> DbError.notFound)
    else
        let sql = System.IO.File.ReadAllText path

        match runScript System.Console.Out session sql with
        | Ok() -> Ok()
        | Error e ->
            printError e
            Error e

[<EntryPoint>]
let main argv =
    let fileMode = argv |> Array.exists (fun a -> a = "-f" || a = "--file")

    let fileIndex =
        if fileMode then
            Some(argv |> Array.findIndex (fun a -> a = "-f" || a = "--file"))
        else
            None

    let skipIndex =
        match fileIndex with
        | Some i -> Some(i + 1)
        | None -> None

    let root =
        argv
        |> Array.mapi (fun i a -> i, a)
        |> Array.choose (fun (i, a) ->
            if skipIndex = Some i then None
            elif a = "-f" || a = "--file" || a.StartsWith "-" then None
            else Some a)
        |> Array.tryHead
        |> Option.defaultValue (System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), "rykdb-data"))

    match Session.OpenWith root with
    | Error e ->
        printError e
        1
    | Ok session ->
        let outcome =
            try
                match fileIndex with
                | Some i when i + 1 >= argv.Length -> Error(DbError.create ErrorKind.SyntaxError "--file にはファイル名が必要です")
                | Some i -> runFile session argv.[i + 1]
                | None ->
                    banner root
                    runRepl session
                    Ok()
            with ex ->
                eprintfn "内部エラー: %s" ex.Message
                Error(DbError.internalError ex.Message)

        if session.InTransaction then
            session.Rollback() |> ignore

        session.Close() |> ignore

        match outcome with
        | Ok() -> 0
        | Error _ -> 1
