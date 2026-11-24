// ===========================================================================
// EngineAnalyze — 統計の収集と ANALYZE。
// ===========================================================================

namespace RykDb.Core

module internal EngineAnalyze =
    /// 標本から等頻度ヒストグラムの境界を選ぶ（`buckets + 1` 個）。
    /// 値の分布が偏っていても、境界が同じ割合の行を区切るので CDF の推定がぶれにくい。
    /// 標本が少ないときは全標本をそのまま返す。
    let private equiDepthHistogram (buckets: int) (samples: float list) =
        match samples |> List.sort with
        | [] -> []
        | sorted ->
            let values = Array.ofList sorted
            let last = values.Length - 1

            if values.Length <= buckets then
                List.ofArray values
            else
                [
                    for k in 0..buckets -> values.[int (System.Math.Round(float k * float last / float buckets))]
                ]

    /// 線形合同法の次の乱数（reservoir sampling 用）。
    let private nextRandom (state: int64) =
        state * 6364136223846793005L + 1442695040888963407L

    /// 標本の件数を全体の件数へスケールする。
    let private scaleCount (sampleSize: int64) (totalRows: int64) (n: int64) =
        if sampleSize <= 0L then
            n
        else
            (n * totalRows + sampleSize / 2L) / sampleSize

    /// 2回以上現れた値を多い順に最大 mcvLimit 個返す（件数は標本→全体へスケール）。
    let private frequentValues
        (mcvLimit: int)
        (totalRows: int64)
        (sampleSize: int64)
        (counts: System.Collections.Generic.Dictionary<string, int64>)
        =
        counts
        |> Seq.filter (fun kv -> kv.Value >= 2L)
        |> Seq.map (fun kv -> kv.Key, scaleCount sampleSize totalRows kv.Value)
        |> Seq.sortByDescending (fun (value, rows) -> rows, value)
        |> Seq.truncate mcvLimit
        |> List.ofSeq

    /// 標本の NULL 件数を全体の件数へスケールする（標本が空なら 0）。
    let private scaleNulls (totalRows: int64) (sampleSize: int64) (nulls: int64) =
        if sampleSize <= 0L then
            0L
        else
            (nulls * totalRows + sampleSize / 2L) / sampleSize

    /// 統計収集で共有する可変のアキュムレータ一式。
    /// 1 回の ANALYZE の間だけ使う（モジュールスコープには置かない）。
    type private StatsAccumulators =
        {
            /// 列ごとの値（SQL リテラル表記）と出現数。
            ColumnSets: System.Collections.Generic.Dictionary<string, int64> array
            /// 列位置ごとの NULL 数。
            NullCounts: System.Collections.Generic.Dictionary<int, int64>
            /// 列位置ごとの数値の最小・最大。
            MinMax: System.Collections.Generic.Dictionary<int, struct (float * float)>
            /// ヒストグラム用の reservoir 標本。
            NumericSamples: System.Collections.Generic.Dictionary<int, float list>
            /// 標本に数えた数値の件数。
            NumericSeen: System.Collections.Generic.Dictionary<int, int64>
            /// reservoir の乱数状態。
            NumericRng: System.Collections.Generic.Dictionary<int, int64>
            /// 索引・索引列の位置・prefix ごとのキー集合。
            Indexes: (IndexDefinition * int array * System.Collections.Generic.HashSet<string> array) list
        }

    /// テーブルの列と索引からアキュムレータを作る。
    let private createStatsAccumulators (schema: TableSchema) (state: ExecState) =
        let schemaColumns = schema.Columns |> Array.ofList

        let indexes =
            Catalog.indexesOf schema.Name state.Catalog
            |> List.choose (fun index ->
                let positions =
                    index.Columns
                    |> List.map (fun name ->
                        schemaColumns |> Array.tryFindIndex (fun col -> SqlName.equal col.Name name))

                if positions |> List.exists Option.isNone then
                    None
                else
                    Some(
                        index,
                        positions |> List.choose id |> Array.ofList,
                        Array.init index.Columns.Length (fun _ -> System.Collections.Generic.HashSet<string>())
                    ))

        {
            ColumnSets =
                Array.init schemaColumns.Length (fun _ -> System.Collections.Generic.Dictionary<string, int64>())
            NullCounts = System.Collections.Generic.Dictionary<int, int64>()
            MinMax = System.Collections.Generic.Dictionary<int, struct (float * float)>()
            NumericSamples = System.Collections.Generic.Dictionary<int, float list>()
            NumericSeen = System.Collections.Generic.Dictionary<int, int64>()
            NumericRng = System.Collections.Generic.Dictionary<int, int64>()
            Indexes = indexes
        }

    /// 文字列キーの出現数を 1 増やす（なければ 1）。
    let private bumpCount (counts: System.Collections.Generic.Dictionary<string, int64>) (key: string) =
        match counts.TryGetValue key with
        | true, n -> counts.[key] <- n + 1L
        | _ -> counts.[key] <- 1L

    /// 列位置ごとの件数を 1 増やす（なければ 1）。
    let private bumpPosition (counts: System.Collections.Generic.Dictionary<int, int64>) (position: int) =
        match counts.TryGetValue position with
        | true, n -> counts.[position] <- n + 1L
        | _ -> counts.[position] <- 1L

    /// 数値列の最小・最大を更新する。
    let private updateMinMax
        (minMax: System.Collections.Generic.Dictionary<int, struct (float * float)>)
        (position: int)
        (v: float)
        =
        match minMax.TryGetValue position with
        | true, struct (lo, hi) -> minMax.[position] <- struct (min lo v, max hi v)
        | _ -> minMax.[position] <- struct (v, v)

    /// reservoir 標本へ 1 件加える（`seen` は加算後の件数）。
    let private addSample
        (samples: System.Collections.Generic.Dictionary<int, float list>)
        (rng: System.Collections.Generic.Dictionary<int, int64>)
        (limit: int)
        (position: int)
        (seen: int64)
        (v: float)
        =
        let current =
            match samples.TryGetValue position with
            | true, xs -> xs
            | _ -> []

        if List.length current < limit then
            samples.[position] <- v :: current
        else
            let state =
                match rng.TryGetValue position with
                | true, s -> s
                | _ -> 1L

            let state = nextRandom state
            rng.[position] <- state
            let j = int ((state >>> 1) % seen)

            if j < limit then
                samples.[position] <- current |> List.mapi (fun k existing -> if k = j then v else existing)

    /// 列値 1つ分（NULL 以外）の統計を加算する。
    let private accumulateValue (acc: StatsAccumulators) (histogramSamples: int) (position: int) (value: SqlValue) =
        bumpCount acc.ColumnSets.[position] (SqlLiteral.toSqlLiteral value)

        match SqlValue.asNumeric value with
        | Some d ->
            let v = float d
            updateMinMax acc.MinMax position v
            bumpPosition acc.NumericSeen position
            addSample acc.NumericSamples acc.NumericRng histogramSamples position acc.NumericSeen.[position] v
        | None -> ()

    /// 索引 1本分のキー集合へ行の値を加える。
    let private accumulateIndexKeys
        (positions: int array)
        (sets: System.Collections.Generic.HashSet<string> array)
        (row: SqlValue array)
        =
        positions
        |> Array.fold
            (fun (key, k) pos ->
                if pos < row.Length then
                    let part = SqlLiteral.toSqlLiteral row.[pos]
                    let key = key + string part.Length + ":" + part
                    sets.[k].Add key |> ignore

                key, k + 1)
            ("", 0)
        |> ignore

    /// 1行を統計の辞書へ加算する。
    let private accumulateRow (acc: StatsAccumulators) (histogramSamples: int) (row: SqlValue array) =
        row
        |> Array.iteri (fun i value ->
            if i < acc.ColumnSets.Length then
                if SqlValue.isNull value then
                    bumpPosition acc.NullCounts i
                else
                    accumulateValue acc histogramSamples i value)

        for _, positions, sets in acc.Indexes do
            accumulateIndexKeys positions sets row

    /// 走査を全件消費して統計を加算する（末尾再帰）。
    [<TailCall>]
    let rec private consumeScan
        (cancel: System.Threading.CancellationToken)
        (progress: (string * int64 -> unit) option)
        (tableName: string)
        (accumulate: SqlValue array -> unit)
        (totalBytes: float)
        (count: int64)
        (scan: SstScan)
        =
        if count % 8192L = 0L && cancel.IsCancellationRequested then
            SstScan.dispose scan
            err (DbError.capacity "ANALYZE はキャンセルされました")
        else
            if count % 8192L = 0L then
                progress |> Option.iter (fun report -> report (tableName, count))

            match SstScan.next scan with
            | Error e ->
                SstScan.dispose scan
                err e
            | Ok None -> ret (count, totalBytes)
            | Ok(Some(entry, rest)) ->
                entry.Value |> Option.iter accumulate

                let totalBytes =
                    match entry.Value with
                    | Some row -> totalBytes + float (ValueCodec.managedRowBytes row)
                    | None -> totalBytes

                consumeScan cancel progress tableName accumulate totalBytes (count + 1L) rest

    /// reservoir sampling で標本を集める（末尾再帰）。
    [<TailCall>]
    let rec private sampleScan
        (cancel: System.Threading.CancellationToken)
        (progress: (string * int64 -> unit) option)
        (tableName: string)
        (n: int)
        (count: int64)
        (samples: SqlValue array list)
        (sampleCount: int)
        (rng: int64)
        (scan: SstScan)
        =
        if count % 8192L = 0L && cancel.IsCancellationRequested then
            SstScan.dispose scan
            err (DbError.capacity "ANALYZE はキャンセルされました")
        else
            if count % 8192L = 0L then
                progress |> Option.iter (fun report -> report (tableName, count))

            match SstScan.next scan with
            | Error e ->
                SstScan.dispose scan
                err e
            | Ok None -> ret (count, samples, sampleCount)
            | Ok(Some(entry, rest)) ->
                match entry.Value with
                | None -> sampleScan cancel progress tableName n count samples sampleCount rng rest
                | Some row ->
                    let rng = nextRandom rng
                    let count = count + 1L

                    if sampleCount < n then
                        sampleScan cancel progress tableName n count (row :: samples) (sampleCount + 1) rng rest
                    else
                        let j = int ((rng >>> 1) % count)

                        if j < n then
                            let samples = samples |> List.mapi (fun i current -> if i = j then row else current)

                            sampleScan cancel progress tableName n count samples sampleCount rng rest
                        else
                            sampleScan cancel progress tableName n count samples sampleCount rng rest

    /// サンプルの有無に応じて走査を回し、(全体行数, 標本数, 行バイト合計) を返す。
    let private collectScanTotals
        (tableName: string)
        (cancel: System.Threading.CancellationToken)
        (progress: (string * int64 -> unit) option)
        (sample: int option)
        (accumulate: SqlValue array -> unit)
        (scan: SstScan)
        =
        match sample |> Option.map (min 100_000) with
        | None ->
            result {
                let! count, bytes = consumeScan cancel progress tableName accumulate 0.0 0L scan
                return count, count, bytes
            }
        | Some n ->
            result {
                let! count, samples, sampleCount =
                    sampleScan cancel progress tableName n 0L [] 0 1L scan

                samples |> List.iter accumulate
                let bytes = samples |> List.sumBy (ValueCodec.managedRowBytes >> float)
                return count, sampleCount, bytes
            }

    /// 列ごとの統計を組み立てる。
    let private buildColumnStats
        (schemaColumns: ColumnDefinition array)
        (acc: StatsAccumulators)
        (totalRows: int64)
        (sampleSize: int64)
        (histogramBuckets: int)
        =
        let mcvLimit = 32

        schemaColumns
        |> Array.indexed
        |> Array.map (fun (i, col) ->
            let nulls =
                match acc.NullCounts.TryGetValue i with
                | true, n -> n
                | _ -> 0L

            let minValue, maxValue =
                match acc.MinMax.TryGetValue i with
                | true, struct (lo, hi) -> Some lo, Some hi
                | _ -> None, None

            let histogram =
                match acc.NumericSamples.TryGetValue i with
                | true, samples -> equiDepthHistogram histogramBuckets samples
                | _ -> []

            {
                Name = col.Name
                NDV = int64 acc.ColumnSets.[i].Count
                Nulls = scaleNulls totalRows sampleSize nulls
                Min = minValue
                Max = maxValue
                Histogram = histogram
                Mcv = frequentValues mcvLimit totalRows sampleSize acc.ColumnSets.[i]
            })
        |> List.ofArray

    /// 索引ごとの prefix NDV を組み立てる。
    let private buildIndexStats
        (indexes: (IndexDefinition * int array * System.Collections.Generic.HashSet<string> array) list)
        =
        indexes
        |> List.map (fun (index, _, sets) ->
            let indexStats: IndexStats =
                {
                    Name = index.IndexName
                    Columns = index.Columns
                    PrefixNDV = sets |> Array.map (fun set -> int64 set.Count) |> List.ofArray
                }

            indexStats)

    /// テーブルの行数、列 NDV・NULL 数・数値範囲、索引 prefix NDV を収集する。
    /// `sample` が Some n なら reservoir sampling で n 行だけを標本にする。
    let private collectStats
        (defn: TableDefinition)
        (state: ExecState)
        (sample: int option)
        (cancel: System.Threading.CancellationToken)
        (progress: (string * int64 -> unit) option)
        =
        progress |> Option.iter (fun report -> report (defn.Name, 0L))

        result {
            let! schema = Catalog.toSchema defn
            let acc = createStatsAccumulators schema state
            let histogramSamples = 1024
            let histogramBuckets = 32

            let! scan = RowStore.scan (Keyspace.table schema.Name) [||] 0 state.Store
            let accumulate = accumulateRow acc histogramSamples

            let! totalRows, sampleSize, rowBytesTotal =
                collectScanTotals defn.Name cancel progress sample accumulate scan

            let rowBytes =
                if sampleSize <= 0L then
                    0.0
                else
                    rowBytesTotal / float sampleSize

            let columns =
                buildColumnStats (schema.Columns |> Array.ofList) acc totalRows sampleSize histogramBuckets

            let indexes = buildIndexStats acc.Indexes

            progress |> Option.iter (fun report -> report (defn.Name, totalRows))

            return
                {
                    Table = schema.Name
                    RowCount = totalRows
                    Columns = columns
                    Indexes = indexes
                    AnalyzedAt = Some System.DateTimeOffset.UtcNow
                    Changes = 0L
                    RowBytes = rowBytes
                }
        }

    /// テーブルの統計を再収集する。名前が None なら全テーブル。
    /// 統計はカタログに 1回で保存する。
    let private analyzeCore
        (table: string option)
        (sample: int option)
        (cancel: System.Threading.CancellationToken)
        (progress: (string * int64 -> unit) option)
        (state: ExecState)
        =
        result {
            let! defns =
                match table with
                | Some name ->
                    match Catalog.findTable name state.Catalog with
                    | Some defn -> ret [ defn ]
                    | None -> err (sprintf "テーブル %s が存在しません" name |> DbError.notFound)
                | None -> ret state.Catalog.Tables

            let! stats =
                defns
                |> List.map (fun defn -> collectStats defn state sample cancel progress)
                |> all

            let nextCatalog =
                stats |> List.fold (fun catalog s -> Catalog.addStats s catalog) state.Catalog

            let! next = ExecState.saved nextCatalog state
            let next = ExecState.clearStatsDeltas (stats |> List.map (fun s -> s.Table)) next
            return next, stats
        }

    /// ANALYZE: 統計を取り直してカタログへ保存する（排他 DDL）。
    let analyze
        (table: string option, sample: int option)
        (cancel: System.Threading.CancellationToken)
        (progress: (string * int64 -> unit) option)
        (state: ExecState)
        =
        ExecState.exclusiveDdlWith state (analyzeCore table sample cancel progress)
