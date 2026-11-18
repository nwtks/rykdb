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
        /// BenchmarkDotNet の `--filter` に渡すパターン（繰り返し指定できる）。
        Filters: string list
    }

let private usage () =
    printfn
        "使い方: dotnet run -c Release --project tools/RykDb.Bench -- [--class query|compaction|index|eval|op] [--quick|--dry] [--filter <pattern>]"

    printfn "  --class query       クエリ系のみ"
    printfn "  --class compaction  compaction のみ"
    printfn "  --class index       索引の読み書き・サイズのベンチのみ"
    printfn "  --class eval        式評価器のマイクロベンチのみ"
    printfn "  --class op          Op（pull 演算子）のマイクロベンチのみ"
    printfn "  --quick             短いジョブ（既定より速いが精度は低い）"
    printfn "  --dry               1回だけ実行して設定を検証する"
    printfn "  --filter <pattern>  シナリオ名で絞る（例: *QueryBenchmarks.PointLookup）"

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
            | "--filter" when i + 1 < argv.Length ->
                loop
                    (i + 2)
                    { options with
                        Filters = options.Filters @ [ argv.[i + 1] ]
                    }
            | "-h"
            | "--help" ->
                usage ()
                exit 0
            | other ->
                eprintfn "不明な引数です: %s" other
                usage ()
                exit 2

    loop
        0
        {
            Class = None
            Mode = "default"
            Filters = []
        }

[<EntryPoint>]
let main argv =
    let options = parseArgs argv

    match options.Class with
    | None
    | Some "query"
    | Some "compaction"
    | Some "index"
    | Some "eval"
    | Some "op" -> ()
    | Some other ->
        eprintfn "不明な --class です: %s（query / compaction / index / eval / op）" other
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

    let runIndex =
        match options.Class with
        | None
        | Some "index" -> true
        | _ -> false

    let runEval =
        match options.Class with
        | None
        | Some "eval" -> true
        | _ -> false

    let runOp =
        match options.Class with
        | None
        | Some "op" -> true
        | _ -> false

    printfn "RykDb benchmarks（CI ではビルドのみ。実行は手元で -c Release）"

    // `--filter` は BenchmarkDotNet の引数としてそのまま渡す。
    // BDN の `--filter` は値を複数並べられる（`--filter *A* *B*`）。
    let filterArgs =
        match options.Filters with
        | [] -> [||]
        | patterns -> Array.append [| "--filter" |] (List.toArray patterns)

    if runQuery then
        BenchmarkRunner.Run<QueryBenchmarks>(config, filterArgs) |> ignore

    if runCompaction then
        BenchmarkRunner.Run<CompactionBenchmarks>(config, filterArgs) |> ignore

    if runIndex then
        BenchmarkRunner.Run<IndexBenchmarks>(config, filterArgs) |> ignore

    if runEval then
        BenchmarkRunner.Run<EvalBenchmarks>(config, filterArgs) |> ignore

    if runOp then
        BenchmarkRunner.Run<OpBenchmarks>(config, filterArgs) |> ignore

    0
