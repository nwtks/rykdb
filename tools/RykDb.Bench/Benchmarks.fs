// ===========================================================================
// RykDb の性能ベンチマーク。
//
// 目的: 性能に影響する変更の前後を同じ条件で比較できるようにする。
// CI ではビルドのみ。実行は手元で `dotnet run -c Release` を使う。
//
// 方針:
//   * クエリ系は `Database.OpenWith`（同期 compaction）で開き、
//     背景 compaction のタイミングで測定が揺れないようにする。
//   * シード後は一度 Close して memtable を SSTable へ flush し、
//     ディスクからの走査を測る（再オープンして測定する）。
//   * compaction は内部 API を使い、4本の L0 ファイルを 1回 compact する。
//     出力は呼び出しごとに削除する（削除時間は測定対象外）。
// ===========================================================================

namespace RykDb.Bench

open System.IO
open BenchmarkDotNet.Attributes
open RykDb.Core

module private Check =
    let ok (r: Result<'a, DbError>) =
        match r with
        | Ok v -> v
        | Error e -> failwith (DbError.toDisplayString e)

module private BenchEnv =
    let newTempDir (label: string) =
        let id = System.Guid.NewGuid().ToString "N"
        let dir = Path.Combine(Path.GetTempPath(), sprintf "rykdb-bench-%s-%s" label id)
        Directory.CreateDirectory dir |> ignore
        dir

    let deleteDir (dir: string) =
        try
            if Directory.Exists dir then
                Directory.Delete(dir, true)
        with _ ->
            ()

module private BenchText =
    /// 48文字程度の TEXT 値（可変長の符号化・コピーを含む行を作る）。
    let ofInt (i: int) =
        sprintf "value-%08d-0123456789abcdef0123456789abcdef" i

/// クエリ系のベンチマーク。
/// シードは 1回だけ行い、その後は同じ DB を読み続ける。
[<MemoryDiagnoser>]
type QueryBenchmarks() =
    let mutable session = Unchecked.defaultof<Session>
    let mutable root = ""
    let pointSql = "SELECT b FROM t WHERE id = 50000"
    let countSql = "SELECT COUNT(*) FROM t"
    let rangeSql = "SELECT COUNT(*) FROM t WHERE id BETWEEN 1000 AND 1999"
    let filterSql = "SELECT COUNT(*) FROM t WHERE a < 50"
    let joinSql = "SELECT COUNT(*) FROM a JOIN b ON a.id = b.id JOIN c ON b.id = c.id"
    let mergeJoinSql = "SELECT COUNT(*) FROM a JOIN b ON a.id = b.id"

    let residualJoinSql =
        "SELECT COUNT(*) FROM a JOIN b ON a.id = b.id AND a.v + b.v > 190"
    // 結果は 0行。ON 句だけを 200 × 20,000回評価する（出力行の割当を除く）。
    let nestedLoopSql = "SELECT COUNT(*) FROM c JOIN b ON c.v = b.v + 1000"

    let insertSql =
        let values = [ for i in 0..99 -> sprintf "(%d, 'payload')" i ] |> String.concat ", "
        sprintf "BEGIN; INSERT INTO ins VALUES %s; ROLLBACK;" values

    member private _.Exec(sql: string) =
        session.Execute sql |> Check.ok |> ignore

    member private this.Seed(table: string, batchSize: int, maxId: int, row: int -> string) =
        for start in 0..batchSize..maxId do
            let stop = min maxId (start + batchSize - 1)
            this.Exec "BEGIN"
            let values = [ for i in start..stop -> row i ] |> String.concat ", "
            this.Exec(sprintf "INSERT INTO %s VALUES %s" table values)
            this.Exec "COMMIT"

    [<GlobalSetup>]
    member this.Setup() =
        root <- BenchEnv.newTempDir "query"
        let db = Database.OpenWith root |> Check.ok
        session <- Session.OpenWith(root, SessionOptions.Default, db)
        this.Exec "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER, b TEXT)"
        this.Exec "CREATE TABLE a (id INTEGER PRIMARY KEY, v INTEGER)"
        this.Exec "CREATE TABLE b (id INTEGER PRIMARY KEY, v INTEGER)"
        this.Exec "CREATE TABLE c (id INTEGER PRIMARY KEY, v INTEGER)"
        this.Exec "CREATE TABLE ins (id INTEGER PRIMARY KEY, v TEXT)"

        // 10万行。a は 0..999 を繰り返し、b は 48文字程度の TEXT。
        this.Seed("t", 10_000, 99_999, fun i -> sprintf "(%d, %d, '%s')" i (i % 1000) (BenchText.ofInt i))
        // JOIN 用に 2万行ずつ。c は 200行（非等値の入れ子ループを 400万回にする）。
        this.Seed("a", 5_000, 19_999, fun i -> sprintf "(%d, %d)" i (i % 100))
        this.Seed("b", 5_000, 19_999, fun i -> sprintf "(%d, %d)" i (i % 100))
        this.Seed("c", 200, 199, fun i -> sprintf "(%d, %d)" i i)

        // memtable を SSTable へ flush し、ディスク走査を測れる状態にする。
        session.Close() |> Check.ok |> ignore

        let reopened = Database.OpenWith root |> Check.ok
        session <- Session.OpenWith(root, SessionOptions.Default, reopened)

    [<GlobalCleanup>]
    member _.Cleanup() =
        try
            if not (isNull (box session)) then
                session.Close() |> ignore
        with _ ->
            ()

        BenchEnv.deleteDir root

    /// 主キーのポイント検索（SSTable の Bloom + 部分読み）。
    [<Benchmark>]
    member this.PointLookup() = this.Exec pointSql

    /// 全件の COUNT(*)。
    /// 現状は値もデコードするため、値の遅延デコードの効果を測る基準になる。
    [<Benchmark>]
    member this.CountStar() = this.Exec countSql

    /// 主キーの範囲走査（RangeScan）。
    [<Benchmark>]
    member this.RangeScan() = this.Exec rangeSql

    /// 索引の無い列の全走査（SeqScan + Filter）。
    [<Benchmark>]
    member this.FilterSeqScan() = this.Exec filterSql

    /// 3テーブル JOIN（主キー同士の等値結合）。
    [<Benchmark>]
    member this.Join3Tables() = this.Exec joinSql

    /// 2テーブルの等値結合（主キー順の走査なのでマージ結合になる）。
    [<Benchmark>]
    member this.MergeJoin2Tables() = this.Exec mergeJoinSql

    /// 等値キー + residual の結合（左右の行を連結せずに ON を評価する経路）。
    [<Benchmark>]
    member this.JoinWithResidual() = this.Exec residualJoinSql

    /// 非等値の入れ子ループ（結果 0行）。ON 句の評価だけを測る。
    [<Benchmark>]
    member this.NestedLoopNoMatch() = this.Exec nestedLoopSql

    /// 100行の INSERT + ROLLBACK（parse / plan / 制約検査 / transaction バッファ）。
    [<Benchmark>]
    member this.Insert100Rows() = this.Exec insertSql

/// compaction のベンチマーク。
/// L0 に 4ファイル（各 25,000件）を作り、1回の L0 → L1 compaction を測る。
[<MemoryDiagnoser>]
type CompactionBenchmarks() =
    let mutable dir = ""
    let mutable version = Version.Empty
    let mutable added: FileMetaData list = []

    member private _.OpenScan(f: FileMetaData) =
        match Sstable.openPartial (FileMetaData.path dir f.FileNumber) with
        | Error e -> Error e
        | Ok partial -> Ok(Sstable.rangeScanPartial [||] 0 partial)

    [<GlobalSetup>]
    member _.Setup() =
        dir <- BenchEnv.newTempDir "compaction"
        let fileCount = 4
        let rowsPerFile = 25_000
        let mutable current = Version.Empty

        for file in 0 .. fileCount - 1 do
            let entries =
                [|
                    for i in 0 .. rowsPerFile - 1 ->
                        let id = i * fileCount + file

                        SstEntry.make
                            (KeyBuilder.ofValues [ SqlValue.Int(int64 id) ])
                            (uint64 (file + 1))
                            (Some [| SqlValue.Int(int64 id); SqlValue.Text(BenchText.ofInt id) |])
                |]

            let meta =
                FileMetaData.writeSstable dir (int64 (10 + file)) 0 0.01 entries |> Check.ok

            current <- Version.addFile meta current

        version <- current

    [<GlobalCleanup>]
    member _.Cleanup() = BenchEnv.deleteDir dir

    /// L0 の 4ファイルを 1回 compaction する。
    [<Benchmark>]
    member this.CompactL0() =
        let result, _ =
            Compaction.compact dir version CompactionOptions.Defaults 0UL 100L this.OpenScan
            |> Check.ok

        added <- result.Added

    /// 出力ファイルは測定の外側で削除する（削除時間を混ぜない）。
    [<IterationCleanup>]
    member _.CleanupInvocation() =
        let files = added
        added <- []

        if not (List.isEmpty files) then
            FileMetaData.deleteObsolete dir files |> Check.ok |> ignore
