// ===========================================================================
// ベンチマークの起動点。
//
// 例:
//   dotnet run -c Release --project tools/RykDb.Bench
//   dotnet run -c Release --project tools/RykDb.Bench -- --class query
//   dotnet run -c Release --project tools/RykDb.Bench -- --quick
//   dotnet run -c Release --project tools/RykDb.Bench -- --dry
//
// 既定はクエリ系 + compaction の両方。結果は
// `BenchmarkDotNet.Artifacts/`（実行時のカレントディレクトリ）に残る。
// CI ではビルドのみで、このプログラムは実行しない。
// ===========================================================================

module RykDb.Bench.Program

open System
open System.IO
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Exporters.Json
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Loggers
open BenchmarkDotNet.Running
open RykDb.Bench

type private Options =
    {
        /// None は両方。`query` / `compaction` で絞る。
        Class: string option
        /// `default` / `quick` / `dry`。
        Mode: string
    }

let private usage () =
    printfn "使い方: dotnet run -c Release --project tools/RykDb.Bench -- [--class query|compaction] [--quick|--dry]"
    printfn "  --class query       クエリ系のみ"
    printfn "  --class compaction  compaction のみ"
    printfn "  --quick             短いジョブ（既定より速いが精度は低い）"
    printfn "  --dry               1回だけ実行して設定を検証する"

let private parseArgs (argv: string array) =
    let rec loop i (options: Options) =
        if i >= argv.Length then
            options
        else
            match argv.[i] with
            | "--class" when i + 1 < argv.Length ->
                loop
                    (i + 2)
                    { options with
                        Class = Some argv.[i + 1]
                    }
            | "--quick" -> loop (i + 1) { options with Mode = "quick" }
            | "--dry" -> loop (i + 1) { options with Mode = "dry" }
            | "-h"
            | "--help" ->
                usage ()
                exit 0
            | other ->
                eprintfn "不明な引数です: %s" other
                usage ()
                exit 2

    loop 0 { Class = None; Mode = "default" }

[<EntryPoint>]
let main argv =
    let options = parseArgs argv

    match options.Class with
    | None
    | Some "query"
    | Some "compaction" -> ()
    | Some other ->
        eprintfn "不明な --class です: %s（query または compaction）" other
        exit 2

    let job =
        match options.Mode with
        | "quick" -> Job.ShortRun
        | "dry" -> Job.Dry
        | _ -> Job.Default

    let config =
        ManualConfig
            .CreateEmpty()
            .AddColumnProvider(DefaultColumnProviders.Instance)
            .AddLogger(ConsoleLogger.Default)
            .AddExporter(JsonExporter.Full)
            .AddDiagnoser(MemoryDiagnoser.Default)
            .AddJob(job)
            .WithArtifactsPath(Path.Combine(Environment.CurrentDirectory, "BenchmarkDotNet.Artifacts"))

    let runQuery =
        match options.Class with
        | None
        | Some "query" -> true
        | _ -> false

    let runCompaction =
        match options.Class with
        | None
        | Some "compaction" -> true
        | _ -> false

    printfn "RykDb benchmarks（CI ではビルドのみ。実行は手元で -c Release）"

    if runQuery then
        BenchmarkRunner.Run<QueryBenchmarks> config |> ignore

    if runCompaction then
        BenchmarkRunner.Run<CompactionBenchmarks> config |> ignore

    0
