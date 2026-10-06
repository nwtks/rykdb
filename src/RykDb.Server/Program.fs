// ===========================================================================
// RykDb Server — エントリポイント
//
// 使い方:
//   dotnet run --project src/RykDb.Server -- [-p ポート番号] [DB のディレクトリ]
//
// 本体とプロトコルは Server.fs、クライアントは Client.fs にある。
// テストは RykDb.Server.serve を直接呼べる。
// ===========================================================================

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading
open RykDb.Core
open RykDb.Server

let private parseArgs (argv: string array) =
    let mutable port = 7090
    let mutable root = Path.Combine(Directory.GetCurrentDirectory(), "rykdb-data")
    let positional = ResizeArray<string>()
    let mutable i = 0
    let mutable err: string option = None

    while i < argv.Length && err.IsNone do
        let a = argv.[i]

        if (a = "-p" || a = "--port") && i + 1 < argv.Length then
            match Int32.TryParse argv.[i + 1] with
            | true, p ->
                port <- p
                i <- i + 2
            | _ -> err <- Some(sprintf "ポート番号が不正です: %s" argv.[i + 1])
        else
            positional.Add a
            i <- i + 1

    if positional.Count > 0 then
        root <- positional.[0]

    match err with
    | Some e -> Error e
    | None -> Ok(port, root)

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Error e ->
        eprintfn "%s" e
        1
    | Ok(port, root) ->
        match Database.openWith root with
        | Error e ->
            eprintfn "DB を開けません: %s" (DbError.toDisplayString e)
            1
        | Ok db ->
            let listener = new TcpListener(IPAddress.Any, port)
            let mutable started = false

            try
                listener.Start()
                started <- true
            with ex ->
                eprintfn "待ち受けできません(ポート %d): %s" port ex.Message

            if not started then
                listener.Stop()
                1
            else
                printfn "RykDb Server listening on port %d" port
                printfn "DB: %s" root
                printfn "Ctrl+C で終了します。"
                Console.Out.Flush()

                use cts = new CancellationTokenSource()

                Console.CancelKeyPress
                |> Event.add (fun e ->
                    e.Cancel <- true
                    cts.Cancel())

                try
                    serve db listener cts.Token (fun m ->
                        printfn "%s" m
                        Console.Out.Flush())
                finally
                    db.Close() |> ignore

                printfn "停止しました。"
                0
