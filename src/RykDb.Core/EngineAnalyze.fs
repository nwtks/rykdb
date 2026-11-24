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

    /// 1行を統計の辞書へ加算する。
    let private accumulateRow
        (columnSets: System.Collections.Generic.Dictionary<string, int64> array)
        (nullCounts: System.Collections.Generic.Dictionary<int, int64>)
        (minMax: System.Collections.Generic.Dictionary<int, struct (float * float)>)
        (numericSamples: System.Collections.Generic.Dictionary<int, float list>)
        (numericSeen: System.Collections.Generic.Dictionary<int, int64>)
        (numericRng: System.Collections.Generic.Dictionary<int, int64>)
        (indexes: (IndexDefinition * int array * System.Collections.Generic.HashSet<string> array) list)
        (histogramSamples: int)
        (row: SqlValue array)
        =
        row
        |> Array.iteri (fun i value ->
            if i < columnSets.Length then
                if SqlValue.isNull value then
                    let current =
                        match nullCounts.TryGetValue i with
                        | true, n -> n
                        | _ -> 0L

                    nullCounts.[i] <- current + 1L
                else
                    let key = SqlLiteral.toSqlLiteral value

                    match columnSets.[i].TryGetValue key with
                    | true, n -> columnSets.[i].[key] <- n + 1L
                    | _ -> columnSets.[i].[key] <- 1L

                    match SqlValue.asNumeric value with
                    | Some d ->
                        let v = float d

                        match minMax.TryGetValue i with
                        | true, struct (lo, hi) -> minMax.[i] <- struct (min lo v, max hi v)
                        | _ -> minMax.[i] <- struct (v, v)

                        let seen =
                            match numericSeen.TryGetValue i with
                            | true, n -> n + 1L
                            | _ -> 1L

                        numericSeen.[i] <- seen

                        let samples =
                            match numericSamples.TryGetValue i with
                            | true, xs -> xs
                            | _ -> []

                        if List.length samples < histogramSamples then
                            numericSamples.[i] <- v :: samples
                        else
                            let rng =
                                match numericRng.TryGetValue i with
                                | true, s -> s
                                | _ -> 1L

                            let rng = nextRandom rng
                            numericRng.[i] <- rng
                            let j = int ((rng >>> 1) % seen)

                            if j < histogramSamples then
                                numericSamples.[i] <-
                                    samples |> List.mapi (fun k current -> if k = j then v else current)
                    | None -> ())

        for _, positions, sets in indexes do
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
            let schemaColumns = schema.Columns |> Array.ofList

            let columnSets =
                Array.init schemaColumns.Length (fun _ -> System.Collections.Generic.Dictionary<string, int64>())

            let nullCounts = System.Collections.Generic.Dictionary<int, int64>()
            let minMax = System.Collections.Generic.Dictionary<int, struct (float * float)>()
            let numericSamples = System.Collections.Generic.Dictionary<int, float list>()
            let numericSeen = System.Collections.Generic.Dictionary<int, int64>()
            let numericRng = System.Collections.Generic.Dictionary<int, int64>()

            let histogramSamples = 1024
            let histogramBuckets = 32

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

            let! scan = RowStore.scan (Keyspace.table schema.Name) [||] 0 state.Store

            let accumulate =
                accumulateRow
                    columnSets
                    nullCounts
                    minMax
                    numericSamples
                    numericSeen
                    numericRng
                    indexes
                    histogramSamples

            let! totalRows, sampleSize, rowBytesTotal =
                match sample |> Option.map (min 100_000) with
                | None ->
                    result {
                        let! count, bytes = consumeScan cancel progress defn.Name accumulate 0.0 0L scan
                        return count, count, bytes
                    }
                | Some n ->
                    result {
                        let! count, samples, sampleCount =
                            sampleScan cancel progress defn.Name n 0L [] 0 1L scan

                        samples |> List.iter accumulate
                        let bytes = samples |> List.sumBy (ValueCodec.managedRowBytes >> float)
                        return count, sampleCount, bytes
                    }

            let rowBytes =
                if sampleSize <= 0L then
                    0.0
                else
                    rowBytesTotal / float sampleSize

            let mcvLimit = 32

            let columns =
                schemaColumns
                |> Array.indexed
                |> Array.map (fun (i, col) ->
                    let nulls =
                        match nullCounts.TryGetValue i with
                        | true, n -> n
                        | _ -> 0L

                    let minValue, maxValue =
                        match minMax.TryGetValue i with
                        | true, struct (lo, hi) -> Some lo, Some hi
                        | _ -> None, None

                    let histogram =
                        match numericSamples.TryGetValue i with
                        | true, samples -> equiDepthHistogram histogramBuckets samples
                        | _ -> []

                    let column =
                        {
                            Name = col.Name
                            NDV = int64 columnSets.[i].Count
                            Nulls = scaleNulls totalRows sampleSize nulls
                            Min = minValue
                            Max = maxValue
                            Histogram = histogram
                            Mcv = frequentValues mcvLimit totalRows sampleSize columnSets.[i]
                        }

                    column)
                |> List.ofArray

            let indexes =
                indexes
                |> List.map (fun (index, _, sets) ->
                    let indexStats: IndexStats =
                        {
                            Name = index.IndexName
                            Columns = index.Columns
                            PrefixNDV = sets |> Array.map (fun set -> int64 set.Count) |> List.ofArray
                        }

                    indexStats)

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

    let analyze
        (table: string option, sample: int option)
        (cancel: System.Threading.CancellationToken)
        (progress: (string * int64 -> unit) option)
        (state: ExecState)
        =
        ExecState.exclusiveDdlWith state (analyzeCore table sample cancel progress)
