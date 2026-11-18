// ===========================================================================
// RykDb の性能ベンチマーク。
//
// 目的: 性能に影響する変更の前後を同じ条件で比較できるようにする。
// CI ではビルドのみ。実行は手元で `dotnet run -c Release` を使う。
//
// 方針:
//   * クエリ系は `Session.OpenWithSync`（同期 compaction）で開き、
//     背景 compaction のタイミングで測定が揺れないようにする。
//   * シード後は一度 Close して memtable を SSTable へ flush し、
//     ディスクからの走査を測る（再オープンして測定する）。
//   * ポイント検索は 4段階を同じ DB で測れるようにする:
//       PointLookup（SQL）> PointLookupParsed（parse 済みの文）
//       > PointLookupPlanned（計画済みの木）> PointLookupStorage（ストレージ直）
//     差を取ると parse / plan / 実行の内訳になる。
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
    /// `pointSql` を parse した文。parse を除いた実行を測るために 1回だけ作る。
    let mutable parsedLookup = Unchecked.defaultof<Statement>
    /// `pointSql` の計画。parse と plan を除いた実行を測るために 1回だけ作る。
    let mutable lookupPlan = Unchecked.defaultof<PlannedQuery>
    /// `SELECT b` が復号する列（id, a, b の b だけ）。PkLookup が使うマスクと同じ。
    let lookupMask = [| false; false; true |]
    let pointSql = "SELECT b FROM t WHERE id = 50000"
    let countSql = "SELECT COUNT(*) FROM t"
    let rangeSql = "SELECT COUNT(*) FROM t WHERE id BETWEEN 1000 AND 1999"
    let filterSql = "SELECT COUNT(*) FROM t WHERE a < 50"
    let constantSql = "SELECT 1 FROM t"
    let constantCountSql = "SELECT COUNT(*) FROM t WHERE 1 = 1"
    let joinSql = "SELECT COUNT(*) FROM a JOIN b ON a.id = b.id JOIN c ON b.id = c.id"
    let mergeJoinSql = "SELECT COUNT(*) FROM a JOIN b ON a.id = b.id"

    let residualJoinSql =
        "SELECT COUNT(*) FROM a JOIN b ON a.id = b.id AND a.v + b.v > 190"
    // 結果は 0行。ON 句だけを 200 × 20,000回評価する（出力行の割当を除く）。
    let nestedLoopSql = "SELECT COUNT(*) FROM c JOIN b ON c.v = b.v + 1000"

    let insertSql =
        let values = [ for i in 0..99 -> sprintf "(%d, 'payload')" i ] |> String.concat ", "
        sprintf "BEGIN; INSERT INTO ins VALUES %s; ROLLBACK;" values

    /// 10,000行を 2,000行ずつ 5文に分けた INSERT（1 transaction で流して ROLLBACK する）。
    let insert10kStatements =
        [|
            for block in 0..4 ->
                let start = block * 2_000
                let stop = start + 1_999

                let values =
                    [ for i in start..stop -> sprintf "(%d, 'payload')" i ] |> String.concat ", "

                sprintf "INSERT INTO ins VALUES %s" values
        |]

    /// 主キーのポイント検索 500回分（SQL の組み立て時間を測定に含めない）。
    let lookup500Sql =
        Array.init 500 (fun i -> sprintf "SELECT b FROM t WHERE id = %d" (i + 1))

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
        session <- Session.OpenWithSync(root, SessionOptions.Default) |> Check.ok
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

        session <- Session.OpenWithSync(root, SessionOptions.Default) |> Check.ok

        // 低レベルシナリオ用に、文（AST）と計画を 1回だけ作る。
        let select =
            match Parser.parseStatements pointSql with
            | Ok [ Statement.Select stmt ] -> stmt
            | Ok _ -> failwith "PointLookup の SQL は SELECT 1文である必要があります"
            | Error e -> failwith (DbError.toDisplayString e)

        parsedLookup <- Statement.Select select
        lookupPlan <- session.PrepareSelect select |> Check.ok

        // 計画が本当に 1行を返すことを 1回だけ確認する（測定条件の取り違え防止）。
        let rowCount (op: Op) = (Op.toArray op |> Check.ok).Length

        let plannedRows =
            try
                let op =
                    Optimizer.exec lookupPlan.Context lookupPlan.Tree lookupPlan.Input |> Check.ok

                rowCount op
            finally
                lookupPlan.Context.Scope.DisposeAll()

        let storageRows =
            try
                let op =
                    ExecContext.byPrimaryKeyMasked lookupPlan.Context "t" [| SqlValue.Int 50000L |] lookupMask
                    |> Check.ok

                rowCount op
            finally
                lookupPlan.Context.Scope.DisposeAll()

        if plannedRows <> 1 || storageRows <> 1 then
            failwithf "PointLookup の低レベルシナリオが 1行を返しません（planned=%d, storage=%d）" plannedRows storageRows

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

    /// parse 済みの文をそのまま実行する（lexer / parser と Session のゲートを通らない）。
    /// `PointLookup` との差が parse のコストになる。
    [<Benchmark>]
    member _.PointLookupParsed() =
        // 読み取り専用の SELECT は状態を変えないので、同じ Session を使い続けられる。
        let result = session.ExecuteStatement parsedLookup |> Check.ok
        result.Rows.Length

    /// 計画済みの論理木から Op を組み立てて実行する（parse も plan もしない）。
    /// `PointLookupParsed` との差がプランナのコストになる。
    /// 行の materialize は `Op.toArrayLimited` まで
    /// （`Engine.runSelect` の `ensureLoaded` / 行の複製は含まない）。
    [<Benchmark>]
    member _.PointLookupPlanned() =
        let ctx = lookupPlan.Context

        try
            let op = Optimizer.exec ctx lookupPlan.Tree lookupPlan.Input |> Check.ok
            (Op.toArrayLimited System.Int64.MaxValue System.Int64.MaxValue op |> Check.ok).Length
        finally
            ctx.Scope.DisposeAll()

    /// ストレージ層の主キー検索だけ（`PkLookup` が実行する呼び出しそのもの）。
    /// Dml → RowStore → SSTable のポイント検索と、必要な列の復号だけを測る。
    [<Benchmark>]
    member _.PointLookupStorage() =
        let ctx = lookupPlan.Context

        try
            let op =
                ExecContext.byPrimaryKeyMasked ctx "t" [| SqlValue.Int 50000L |] lookupMask
                |> Check.ok

            (Op.toArrayLimited System.Int64.MaxValue System.Int64.MaxValue op |> Check.ok).Length
        finally
            ctx.Scope.DisposeAll()

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

    /// 定数射影（行の値を一切読まない）。
    [<Benchmark>]
    member this.SelectConstant() = this.Exec constantSql

    /// 定数述語の COUNT(*)（行の値を一切読まない）。
    [<Benchmark>]
    member this.CountConstantWhere() = this.Exec constantCountSql

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

    /// 10,000行の INSERT（2,000行 × 5文）+ ROLLBACK。
    [<Benchmark>]
    member this.Insert10kRows() =
        this.Exec "BEGIN"

        for sql in insert10kStatements do
            this.Exec sql

        this.Exec "ROLLBACK"

    /// 主キーのポイント検索を 500回連続で行う。
    [<Benchmark>]
    member _.PointLookup500() =
        let mutable rows = 0L

        for sql in lookup500Sql do
            let r = session.Execute sql |> Check.ok
            rows <- rows + int64 r.Rows.Length

        rows

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
        | Ok partial -> Ok(Sstable.rangeScanPartialRaw [||] 0 partial)

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
                            (KeyBuilder.ofValues [ SqlValue.Int(int64 id) ] |> Check.ok)
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

/// 式評価器のマイクロベンチ。
/// DB を使わず、`Eval` の Kont / Step などの割当だけを見る。
[<MemoryDiagnoser>]
type EvalBenchmarks() =
    let outCol name =
        {
            Name = name
            Qualifier = Some "t"
            DataType = Some DataType.Integer
        }

    let schema =
        {
            Columns = [| outCol "a"; outCol "b" |]
            Tables = [ "t" ]
        }

    let row = [| SqlValue.Int 500L; SqlValue.Int 10L |]
    let compareA = SqlValue.Int 1L
    let compareB = SqlValue.Int 2L

    let col name =
        Expr.Column { Qualifier = Some "t"; Name = name }

    // a = b + 1000（二項 2 つ + 列 2 つ + リテラル 1 つ）
    let binary =
        Expr.Binary(BinaryOp.Eq, col "a", Expr.Binary(BinaryOp.Add, col "b", Expr.Literal(Literal.Int 1000L)))

    // a < 1000 AND (b = 10 OR b IS NULL)
    let complex =
        Expr.Binary(
            BinaryOp.And,
            Expr.Binary(BinaryOp.Lt, col "a", Expr.Literal(Literal.Int 1000L)),
            Expr.Binary(
                BinaryOp.Or,
                Expr.Binary(BinaryOp.Eq, col "b", Expr.Literal(Literal.Int 10L)),
                Expr.IsNull(col "b", false)
            )
        )

    // 式はすべて計画時に列解決済み（ベンチは評価だけを見る）。
    let resolver = Eval.prepare schema [ binary; complex ]

    [<Benchmark>]
    member _.MatchesLiteralV() =
        Eval.matchesWithV resolver row (Expr.Literal(Literal.Int 1L)) |> ignore

    [<Benchmark>]
    member _.MatchesColumnV() =
        Eval.matchesWithV resolver row (col "a") |> ignore

    [<Benchmark>]
    member _.MatchesBinaryV() =
        Eval.matchesWithV resolver row binary |> ignore

    [<Benchmark>]
    member _.MatchesComplexV() =
        Eval.matchesWithV resolver row complex |> ignore

    [<Benchmark>]
    member _.MatchesComplexJoinedV() =
        Eval.matchesJoinedV resolver row row complex |> ignore

    [<Benchmark>]
    member _.CompareValuesV() =
        match SqlValue.compareValuesV compareA compareB with
        | DbOk _ -> ()
        | DbErr _ -> failwith "比較できませんでした"

/// Op（Volcano pull）のマイクロベンチ。
/// DB を使わず、pull 層（`Op.next` / `Op.pull` / `map` / `filter` / `skip` / `take`）の割当を見る。
[<MemoryDiagnoser>]
type OpBenchmarks() =
    /// 1行 2列（a, b）。フィルタは a、変換は b を使う。
    let rows =
        Array.init 10_000 (fun i -> [| SqlValue.Int(int64 i); SqlValue.Int(int64 (i % 100)) |])

    /// a >= 5000 だけ通す。
    let predicate (r: SqlValue array) =
        DbOk(
            match r.[0] with
            | SqlValue.Int v -> v >= 5000L
            | _ -> false
        )

    /// b を +1 する（新しい行を作る）。
    let mapper (r: SqlValue array) =
        DbOk
            [|
                r.[0]
                SqlValue.Int(
                    match r.[1] with
                    | SqlValue.Int v -> v + 1L
                    | _ -> 0L
                )
            |]

    /// 1行だけの Step。`next` の呼び出しコストだけを見る（Step は純粋なので再利用してよい）。
    let step = Op.emit [| SqlValue.Int 1L |] Op.Empty

    /// 材料化だけ（`ofArray` → `toArray`）。
    [<Benchmark>]
    member _.OpMaterialize() =
        Op.ofArray rows |> Op.toArray |> Check.ok |> ignore

    /// filter → map → skip → take のパイプライン（10,000行のうち 1,000行を出す）。
    [<Benchmark>]
    member _.OpFilterMapTake() =
        Op.ofArray rows
        |> Op.filter predicate
        |> Op.map mapper
        |> Op.skip 100
        |> Op.take 1000
        |> Op.toArray
        |> Check.ok
        |> ignore

    /// `Step` から 1行引く。`next` の `Ok` / `Some` / tuple の割当を見る。
    [<Benchmark>]
    member _.NextStep() =
        match Op.next step with
        | OpStep.OpRow _ -> ()
        | OpStep.OpDone -> failwith "行が返りませんでした"
        | OpStep.OpError e -> failwith (DbError.toDisplayString e)

/// 索引のストレージ・書き込み・被覆 / 非被覆読みのベンチマーク。
/// 同じデータを「索引あり」「索引なし」の 2つの DB に入れ、
/// 総サイズの差を索引のストレージ増分として出力する。
[<MemoryDiagnoser>]
type IndexBenchmarks() =
    let mutable indexed = Unchecked.defaultof<Session>
    let mutable plain = Unchecked.defaultof<Session>
    let mutable indexedRoot = ""
    let mutable plainRoot = ""

    let exec (s: Session) (sql: string) = s.Execute sql |> Check.ok |> ignore

    let rowsOf (s: Session) (sql: string) = (s.Execute sql |> Check.ok).Rows.Length

    let seed (s: Session) (table: string) (maxId: int) =
        for start in 0..10_000..maxId do
            let stop = min maxId (start + 9_999)

            let values =
                [
                    for i in start..stop -> sprintf "(%d, %d, '%s')" i (i % 1000) (BenchText.ofInt i)
                ]
                |> String.concat ", "

            exec s "BEGIN"
            exec s (sprintf "INSERT INTO %s VALUES %s" table values)
            exec s "COMMIT"

    let insert100Sql =
        let values =
            [ for i in 0..99 -> sprintf "(%d, %d, 'payload-%d')" (20_000 + i) (i % 1000) i ]
            |> String.concat ", "

        sprintf "BEGIN; INSERT INTO t VALUES %s; ROLLBACK;" values

    let dirBytes (dir: string) =
        Directory.EnumerateFiles(dir, "*", SearchOption.AllDirectories)
        |> Seq.sumBy (fun path -> FileInfo(path).Length)

    [<GlobalSetup>]
    member _.Setup() =
        indexedRoot <- BenchEnv.newTempDir "index"
        plainRoot <- BenchEnv.newTempDir "index-plain"
        indexed <- Session.OpenWithSync(indexedRoot, SessionOptions.Default) |> Check.ok
        plain <- Session.OpenWithSync(plainRoot, SessionOptions.Default) |> Check.ok
        exec indexed "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER, b TEXT)"
        exec plain "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER, b TEXT)"
        exec indexed "CREATE INDEX ix_a ON t (a)"
        seed indexed "t" 19_999
        seed plain "t" 19_999

        indexed.Close() |> Check.ok |> ignore
        plain.Close() |> Check.ok |> ignore
        indexed <- Session.OpenWithSync(indexedRoot, SessionOptions.Default) |> Check.ok
        plain <- Session.OpenWithSync(plainRoot, SessionOptions.Default) |> Check.ok

        let covered = rowsOf indexed "SELECT a FROM t WHERE a = 500"
        let fetched = rowsOf indexed "SELECT b FROM t WHERE a = 500"
        let counted = rowsOf indexed "SELECT COUNT(*) FROM t WHERE a = 500"

        if covered <> 20 || fetched <> 20 || counted <> 1 then
            failwithf "索引ベンチの前提が崩れています（covered=%d, fetched=%d, counted=%d）" covered fetched counted

        printfn
            "索引のストレージ: 索引あり=%d バイト / 索引なし=%d バイト（差 %d バイト）"
            (dirBytes indexedRoot)
            (dirBytes plainRoot)
            (dirBytes indexedRoot - dirBytes plainRoot)

    [<GlobalCleanup>]
    member _.Cleanup() =
        try
            if not (isNull (box indexed)) then
                indexed.Close() |> ignore

            if not (isNull (box plain)) then
                plain.Close() |> ignore
        with _ ->
            ()

        BenchEnv.deleteDir indexedRoot
        BenchEnv.deleteDir plainRoot

    /// 被覆読み（索引列 + 主キーだけで完結する）。
    [<Benchmark>]
    member _.IndexCoveredLookup() =
        rowsOf indexed "SELECT a FROM t WHERE a = 500"

    /// 非被覆読み（本体から b を fetch する）。
    [<Benchmark>]
    member _.IndexFetchLookup() =
        rowsOf indexed "SELECT b FROM t WHERE a = 500"

    /// 件数だけ（値を復号しない）。
    [<Benchmark>]
    member _.IndexCoveredCount() =
        rowsOf indexed "SELECT COUNT(*) FROM t WHERE a = 500"

    /// 非被覆の範囲読み。
    [<Benchmark>]
    member _.IndexFetchRange() =
        rowsOf indexed "SELECT b FROM t WHERE a BETWEEN 100 AND 199"

    /// 索引 2本（作成済みの ix_a と暗黙なし）を持つ表への 100行 INSERT + ROLLBACK。
    [<Benchmark>]
    member _.InsertWithIndex100Rows() = exec indexed insert100Sql

    /// 索引なしの表への 100行 INSERT + ROLLBACK。
    [<Benchmark>]
    member _.InsertNoIndex100Rows() = exec plain insert100Sql
