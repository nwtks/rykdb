// ===========================================================================
// OpsAggregate — HAVING と集約。
// ===========================================================================

namespace RykDb.Core

open OpsShared

module internal OpsAggregate =
    /// HAVING を張る。
    /// 集約値・グループ値をスロット参照へ置き換えた述語を計画時に 1回だけ作り、
    /// 行ごとの評価では列位置を引くだけにする。
    /// 置換後に残った列参照はスロットのスキーマで解決できず NameError になる。
    let havingFilter (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (predicate: Expr) =
        match prepareAggExprs groupBy aggs [ predicate ] with
        | Error e -> err e
        | Ok(resolve, [ p ]) -> ret (fun row -> Eval.matchesWithV resolve row p)
        | Ok _ -> err (DbError.internalError "HAVING の置換結果が不正です")

    /// 集約の 1 項目分のアキュムレータ。
    /// SUM / AVG は REAL が現れるまでは decimal、現れた後は double で累積し、最後に型を決める
    /// （整数・DECIMAL の丸め誤差を避ける）。
    type private Acc =
        {
            Kind: AggFunc
            Count: int64
            SumDecimal: decimal
            SawReal: bool
            SumReal: double
            Min: SqlValue option
            Max: SqlValue option
            /// DISTINCT の重複判定キー（`keyNormalize`）→ 最初に現れた元の値。
            /// 退避時は元の値を書く（型・スケールを保って再集計する）。
            Seen: Map<SqlValue, SqlValue>
            SeenRuns: string list
        }

    /// 集約の実行状態。
    type private AggContext =
        {
            Budget: MemoryBudget
            Resolve: Prepared
            GroupBy: Expr list
            Aggs: (AggFunc * bool * bool * Expr) list
            NormalizeKey: SqlValue array -> SqlValue array
            SeenBytes: Acc list -> int64
            Buckets: System.Collections.Generic.Dictionary<SqlValue array, Acc list>
            OutputKeys: System.Collections.Generic.Dictionary<SqlValue array, SqlValue array>
            FirstSeen: System.Collections.Generic.Dictionary<SqlValue array, int64>
        }

    /// 集約の退避書き込み失敗時にライターを破棄する。
    let private aggAbandon (writer: Spill.PartitionWriter) (e: DbError) =
        writer.Abandon()
        err e

    /// GROUP BY 式を評価してキー配列へ詰める再帰本体。
    [<TailCall>]
    let rec private aggFillKeys (resolve: Prepared) (row: SqlValue array) (keys: SqlValue array) (i: int) =
        function
        | [] -> DbOk keys
        | e :: tail ->
            match Eval.evalWithV resolve row e with
            | DbErr er -> DbErr er
            | DbOk v ->
                keys.[i] <- v
                aggFillKeys resolve row keys (i + 1) tail

    /// 入力行から GROUP BY キーを作る。
    let private aggKeyOfRow (resolve: Prepared) (groupBy: Expr list) (row: SqlValue array) =
        let keys = Array.zeroCreate<SqlValue> groupBy.Length
        aggFillKeys resolve row keys 0 groupBy

    /// DECIMAL の加算（範囲超過を検査する）。
    let private aggAddDecimal (a: decimal) (b: decimal) =
        let maxD = System.Decimal.MaxValue
        let minD = System.Decimal.MinValue

        if a > 0M && b > 0M && a > maxD - b || a < 0M && b < 0M && a < minD - b then
            DbErr(DbError.constraintViolation "SUM の結果が DECIMAL の範囲を超えました")
        else
            DbOk(a + b)

    /// DISTINCT の見た値を退避ファイルへ流す。
    let private aggSpillSeen (budget: MemoryBudget) (acc: Acc) =
        let stamp = System.Guid.NewGuid().ToString "N"

        let rows =
            acc.Seen
            |> Map.toList
            |> List.map (fun (_, v) ->
                ValueCodec.writeValues [| v |] Writer.Empty
                |> Result.map (fun w -> [| SqlValue.Blob(Writer.toArray w) |]))
            |> all

        match rows with
        | Error e -> DbErr e
        | Ok rows ->
            match Spill.writeChunk budget.Directory stamp 0 (List.toArray rows) with
            | Error e -> DbErr e
            | Ok path ->
                Spill.registerRun budget.Scope path

                DbOk
                    { acc with
                        Seen = Map.empty
                        SeenRuns = path :: acc.SeenRuns
                    }

    /// SUM / AVG の 1値加算。
    /// REAL は非有限、INTEGER / DECIMAL は桁あふれを検査する。
    let private aggAddSum (acc: Acc) (bump: Acc) (newSeen: Map<SqlValue, SqlValue>) (v: SqlValue) =
        match v with
        | SqlValue.Real f ->
            let sum = acc.SumReal + float acc.SumDecimal + f

            if
                not (System.Double.IsFinite sum)
                && System.Double.IsFinite acc.SumReal
                && System.Double.IsFinite f
            then
                DbErr(DbError.constraintViolation "SUM の結果が REAL の範囲を超えました")
            else
                DbOk
                    { bump with
                        SawReal = true
                        SumReal = sum
                        SumDecimal = 0M
                        Seen = newSeen
                    }
        | SqlValue.Int _
        | SqlValue.Decimal _ ->
            match SqlValue.asNumeric v with
            | Some d ->
                if acc.SawReal then
                    let sum = acc.SumReal + float d

                    if not (System.Double.IsFinite sum) && System.Double.IsFinite acc.SumReal then
                        DbErr(DbError.constraintViolation "SUM の結果が REAL の範囲を超えました")
                    else
                        DbOk
                            { bump with
                                SumReal = sum
                                Seen = newSeen
                            }
                else
                    aggAddDecimal acc.SumDecimal d
                    |> DbResult.map (fun sum ->
                        { bump with
                            SumDecimal = sum
                            Seen = newSeen
                        })
            | None -> DbErr(DbError.internalError "数値の変換に失敗しました（内部不整合）")
        | _ ->
            DbErr(
                sprintf "%s は数値だけを受け付けますが %s です" acc.Kind.Name (SqlLiteral.toSqlLiteral v)
                |> DbError.typeError
            )

    /// 1つの値をアキュムレータへ畳み込む。
    let private aggAddValue (acc: Acc) (bump: Acc) (v: SqlValue) (newSeen: Map<SqlValue, SqlValue>) =
        match acc.Kind with
        | AggFunc.Count -> DbOk { bump with Seen = newSeen }
        | AggFunc.Sum
        | AggFunc.Avg -> aggAddSum acc bump newSeen v
        | AggFunc.Min ->
            let updated = { bump with Seen = newSeen }

            match acc.Min with
            | None -> DbOk { updated with Min = Some v }
            | Some cur ->
                match SqlValue.compareValuesV v cur with
                | DbErr e -> DbErr e
                | DbOk c ->
                    if c < 0 then
                        DbOk { updated with Min = Some v }
                    else
                        DbOk updated
        | AggFunc.Max ->
            let updated = { bump with Seen = newSeen }

            match acc.Max with
            | None -> DbOk { updated with Max = Some v }
            | Some cur ->
                match SqlValue.compareValuesV v cur with
                | DbErr e -> DbErr e
                | DbOk c ->
                    if c > 0 then
                        DbOk { updated with Max = Some v }
                    else
                        DbOk updated

    /// 集約 1項目のアキュムレータ更新（DISTINCT 判定と退避を含む）。
    let private aggAddAcc (budget: MemoryBudget) (acc: Acc) (value: SqlValue option) (isStar: bool) (distinct: bool) =
        let seenKey =
            match value with
            | Some v when not (SqlValue.isNull v) -> Some(SqlValue.keyNormalize v)
            | _ -> None

        let alreadySeen =
            distinct && seenKey |> Option.exists (fun k -> Map.containsKey k acc.Seen)

        if alreadySeen then
            DbOk acc
        else
            let counted =
                if isStar then
                    Some SqlValue.Null
                else
                    match value with
                    | Some v when not (SqlValue.isNull v) -> Some v
                    | _ -> None

            let newSeen =
                if distinct then
                    match seenKey, value with
                    | Some k, Some original when not (SqlValue.isNull original) -> Map.add k original acc.Seen
                    | _ -> acc.Seen
                else
                    acc.Seen

            let withSeen =
                let seenLimit =
                    if budget.Work < 0L then
                        System.Int64.MaxValue
                    else
                        max 64L (budget.Work / 32L)

                if distinct && int64 (Map.count newSeen) > seenLimit then
                    aggSpillSeen budget { acc with Seen = newSeen }
                else
                    DbOk { acc with Seen = newSeen }

            match withSeen with
            | DbErr e -> DbErr e
            | DbOk a ->
                match counted with
                | None -> DbOk a
                | Some v ->
                    let bump = { a with Count = a.Count + 1L }
                    aggAddValue a bump v a.Seen

    /// 集約引数を評価して 1項目を更新する。
    let private aggAdd
        (resolve: Prepared)
        (budget: MemoryBudget)
        (star: bool)
        (distinct: bool)
        (arg: Expr)
        (acc: Acc)
        (row: SqlValue array)
        =
        if star then
            aggAddAcc budget acc None true distinct
        else
            match Eval.evalWithV resolve row arg with
            | DbErr e -> DbErr e
            | DbOk v -> aggAddAcc budget acc (Some v) false distinct

    /// 1行をバケットの全アキュムレータへ畳み込む再帰本体。
    [<TailCall>]
    let rec private aggAddStep
        (aggs: (AggFunc * bool * bool * Expr) list)
        (resolve: Prepared)
        (budget: MemoryBudget)
        (row: SqlValue array)
        (defs: (AggFunc * bool * bool * Expr) list)
        (accs: Acc list)
        (out: Acc list)
        =
        match defs, accs with
        | [], [] -> DbOk(List.rev out)
        | (_, star, distinct, arg) :: defsTail, acc :: accsTail ->
            match aggAdd resolve budget star distinct arg acc row with
            | DbErr e -> DbErr e
            | DbOk next -> aggAddStep aggs resolve budget row defsTail accsTail (next :: out)
        | _ -> DbErr(DbError.internalError "集約の状態数が一致しません（内部不整合）")

    /// 1行をバケットの全アキュムレータへ畳み込む。
    let private aggAddToBucket
        (aggs: (AggFunc * bool * bool * Expr) list)
        (resolve: Prepared)
        (budget: MemoryBudget)
        (bucket: Acc list)
        (row: SqlValue array)
        =
        aggAddStep aggs resolve budget row aggs bucket []

    /// 空のアキュムレータを作る。
    let private aggEmptyAcc (kind: AggFunc) =
        {
            Kind = kind
            Count = 0L
            SumDecimal = 0M
            SawReal = false
            SumReal = 0.0
            Min = None
            Max = None
            Seen = Map.empty
            SeenRuns = []
        }

    /// 空のバケットを作る。
    let private aggNewBucket = List.map (fun (f, _, _, _) -> aggEmptyAcc f)

    /// 退避行の先頭バイト列を比較する。
    let private aggCompareBlob (a: SqlValue array) (b: SqlValue array) =
        match a.[0], b.[0] with
        | SqlValue.Blob x, SqlValue.Blob y -> Bytes.compare x y
        | _ -> 0

    /// DISTINCT 集約の退避行を 1つのアキュムレータへ畳み込む再帰本体。
    [<TailCall>]
    let rec private aggFoldDistinct (a: Acc) (previous: byte array option) (op: Op) =
        match Op.next op with
        | OpStep.OpError e -> DbErr e
        | OpStep.OpDone -> DbOk a
        | OpStep.OpRow(row, nxt) ->
            match row.[0] with
            | SqlValue.Blob bytes when previous <> Some bytes ->
                match ValueCodec.readValues (Cursor.create bytes) with
                | Error e -> DbErr e
                | Ok(values, _) ->
                    let v = values.[0]
                    let bump = { a with Count = a.Count + 1L }

                    match aggAddValue a bump v Map.empty with
                    | DbErr e -> DbErr e
                    | DbOk next -> aggFoldDistinct next (Some bytes) nxt
            | SqlValue.Blob _ -> aggFoldDistinct a previous nxt
            | _ -> DbErr(DbError.internalError "DISTINCT 集約の退避行が不正です")

    /// 退避済み DISTINCT 値をマージして確定させる。
    let private aggFinalizeAcc (budget: MemoryBudget) (acc: Acc) =
        if List.isEmpty acc.SeenRuns then
            DbOk acc
        else
            let flushed =
                if Map.isEmpty acc.Seen then
                    DbOk acc
                else
                    aggSpillSeen budget acc

            match flushed with
            | DbErr e -> DbErr e
            | DbOk acc ->
                let paths = acc.SeenRuns

                match Spill.reopen budget.Scope (Spill.RowSpool.Disk(paths, 0)) with
                | Error e ->
                    Spill.forget paths
                    paths |> List.iter (Spill.forgetRun budget.Scope)
                    DbErr e
                | Ok src ->
                    match ExtSort.sortRows budget () (fun () _ -> DbOk()) aggCompareBlob src with
                    | Error e ->
                        Spill.forget paths
                        paths |> List.iter (Spill.forgetRun budget.Scope)
                        DbErr e
                    | Ok sorted ->
                        paths |> List.iter (Spill.forgetRun budget.Scope)

                        aggFoldDistinct (aggEmptyAcc acc.Kind) None sorted
                        |> DbResult.map (fun final -> { final with SeenRuns = [] })

    /// アキュムレータを SQL 値へ変換する。
    let private aggFinishAcc (acc: Acc) =
        match acc.Kind with
        | AggFunc.Count -> SqlValue.Int acc.Count
        | AggFunc.Sum ->
            if acc.Count = 0L then SqlValue.Null
            elif acc.SawReal then SqlValue.vReal acc.SumReal
            else SqlValue.Decimal acc.SumDecimal
        | AggFunc.Avg ->
            if acc.Count = 0L then
                SqlValue.Null
            elif acc.SawReal then
                SqlValue.vReal (acc.SumReal / float acc.Count)
            else
                SqlValue.vReal (float acc.SumDecimal / float acc.Count)
        | AggFunc.Min -> defaultArg acc.Min SqlValue.Null
        | AggFunc.Max -> defaultArg acc.Max SqlValue.Null

    /// バケットを出力行へ変換する。
    let private aggRowOf (key: SqlValue array) (bucket: Acc list) =
        bucket |> List.map aggFinishAcc |> List.toArray |> Array.append key

    /// バケットの使用バイト数を見積もる。
    let private aggBucketBytes seenBytes (key: SqlValue array) (bucket: Acc list) =
        let seen =
            seenBytes bucket
            + (bucket |> List.sumBy (fun a -> int64 (List.length a.SeenRuns) * 64L))

        int64 (ValueCodec.managedRowBytes key) + 96L * int64 bucket.Length + seen + 64L

    /// 集約結果が 0 行のときの行を作る。
    let private aggEmptyResult =
        List.map (fun (f, _, _, _) -> aggFinishAcc (aggEmptyAcc f)) >> List.toArray

    /// 退避行の連番を比較する。
    let private aggCompareFirst (a: SqlValue array) (b: SqlValue array) =
        match a.[0], b.[0] with
        | SqlValue.Int x, SqlValue.Int y -> compare x y
        | _ -> 0

    /// GROUP BY キーを符号化する。
    let private aggKeyOf
        (resolve: Prepared)
        (groupBy: Expr list)
        (normalizeKey: SqlValue array -> SqlValue array)
        (r: SqlValue array)
        =
        match aggKeyOfRow resolve groupBy r with
        | DbErr e -> err e
        | DbOk k -> KeyBuilder.ofValues (normalizeKey k)

    /// 退避行に連番を付ける。
    let private aggFrame (s: int64) = Array.append [| SqlValue.Int s |]

    /// 退避行から GROUP BY キーを復元する。
    let private aggKeyOfFrame
        (resolve: Prepared)
        (groupBy: Expr list)
        (normalizeKey: SqlValue array -> SqlValue array)
        (f: SqlValue array)
        =
        match f.[0] with
        | SqlValue.Int _ -> aggKeyOf resolve groupBy normalizeKey (Array.sub f 1 (f.Length - 1))
        | _ -> err (DbError.internalError "GROUP BY の退避行が不正です")

    /// パーティション畳み込みの初期状態を作る。
    let private aggSeed (aggs: (AggFunc * bool * bool * Expr) list) =
        struct (Array.empty<SqlValue>, aggNewBucket aggs, System.Int64.MaxValue)

    /// 退避行をパーティション状態へ畳み込む。
    let private aggAddFrame
        (resolve: Prepared)
        (groupBy: Expr list)
        (aggs: (AggFunc * bool * bool * Expr) list)
        (budget: MemoryBudget)
        struct (_, accs: Acc list, first: int64)
        (f: SqlValue array)
        =
        match f.[0] with
        | SqlValue.Int s ->
            let r = Array.sub f 1 (f.Length - 1)

            match aggKeyOfRow resolve groupBy r with
            | DbErr e -> err e
            | DbOk k ->
                match aggAddToBucket aggs resolve budget accs r with
                | DbErr e -> err e
                | DbOk next -> ret struct (k, next, min first s)
        | _ -> err (DbError.internalError "GROUP BY の退避行が不正です")

    /// パーティション状態のバイト数。
    let private aggStateBytes (seenBytes: Acc list -> int64) struct (key: SqlValue array, accs: Acc list, _) =
        aggBucketBytes seenBytes key accs

    /// パーティション状態を出力行へ変換する。
    let private aggEmitGroup (budget: MemoryBudget) struct (key: SqlValue array, accs: Acc list, first: int64) =
        accs
        |> List.map (aggFinalizeAcc budget >> DbResult.toResult)
        |> all
        |> Result.map (fun finalAccs ->
            Array.append
                [| SqlValue.Int first |]
                (Array.append key (finalAccs |> List.map aggFinishAcc |> List.toArray))
            |> Some)

    /// 最初の出現順にグループ行を出す。
    [<TailCall>]
    let rec private aggEmitGroups
        (outputKeys: System.Collections.Generic.Dictionary<SqlValue array, SqlValue array>)
        (buckets: System.Collections.Generic.Dictionary<SqlValue array, Acc list>)
        =
        function
        | [] -> Op.Empty
        | key :: rest ->
            Op.compute (fun () ->
                OpStep.OpRow(aggRowOf outputKeys.[key] buckets.[key], aggEmitGroups outputKeys buckets rest))

    /// グループ表をパーティションへ退避し、マージ済みの遅延 Op を返す。
    let private aggExternalize (ctx: AggContext) (startSeq: int64) (start: Op) =
        let stamp = System.Guid.NewGuid().ToString "N"

        Spill.openPartitions ctx.Budget.Directory stamp 0 Spill.Partitions
        >>= fun writer ->
            let written =
                Spill.fillPartitions writer (aggKeyOf ctx.Resolve ctx.GroupBy ctx.NormalizeKey) aggFrame startSeq start

            match written with
            | Error e -> aggAbandon writer e
            | Ok _ ->
                match writer.Close() with
                | Error e -> aggAbandon writer e
                | Ok() ->
                    ctx.Buckets
                    |> Seq.map (fun kv ->
                        KeyBuilder.ofValues kv.Key
                        |> Result.map (fun encoded ->
                            encoded, struct (ctx.OutputKeys.[kv.Key], kv.Value, ctx.FirstSeen.[kv.Key])))
                    |> all
                    >>= fun seeds ->
                        Spill.groupPartitions
                            ctx.Budget
                            stamp
                            (aggKeyOfFrame ctx.Resolve ctx.GroupBy ctx.NormalizeKey)
                            (fun () -> aggSeed ctx.Aggs)
                            (aggAddFrame ctx.Resolve ctx.GroupBy ctx.Aggs ctx.Budget)
                            (aggStateBytes ctx.SeenBytes)
                            (aggEmitGroup ctx.Budget)
                            seeds
                            (writer.Paths |> Array.toList)
                        >>= fun rows ->
                            ExtSort.sortRows ctx.Budget () (fun () _ -> DbOk()) aggCompareFirst rows
                            |> Result.map (Op.map (fun f -> DbOk(Array.sub f 1 (f.Length - 1))))

    /// 入力行をグループ表へ畳み込む（末尾再帰）。上限超過時は退避する。
    [<TailCall>]
    let rec private aggConsume (ctx: AggContext) (order: SqlValue array list) (used: int64) (seq: int64) (cur: Op) =
        match Op.next cur with
        | OpStep.OpError e -> err e
        | OpStep.OpDone ->
            ctx.Buckets
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> List.ofSeq
            |> List.map (fun (k, bucket) ->
                bucket
                |> List.map (aggFinalizeAcc ctx.Budget >> DbResult.toResult)
                |> all
                |> Result.map (fun final -> k, final))
            |> all
            >>= fun finalized ->
                finalized |> List.iter (fun (k, bucket) -> ctx.Buckets.[k] <- bucket)

                if List.isEmpty order && List.isEmpty ctx.GroupBy then
                    ret (Op.ofArray [| aggEmptyResult ctx.Aggs |])
                else
                    ret (aggEmitGroups ctx.OutputKeys ctx.Buckets (List.rev order))
        | OpStep.OpRow(row, nxt) ->
            match aggKeyOfRow ctx.Resolve ctx.GroupBy row with
            | DbErr e -> err e
            | DbOk k ->
                let key = ctx.NormalizeKey k

                match ctx.Buckets.TryGetValue key with
                | true, bucket ->
                    match aggAddToBucket ctx.Aggs ctx.Resolve ctx.Budget bucket row with
                    | DbErr e -> err e
                    | DbOk next ->
                        ctx.Buckets.[key] <- next
                        let grown = max 0L (ctx.SeenBytes next - ctx.SeenBytes bucket)

                        if MemoryBudget.exceeded (ctx.Budget.Work, used + grown) then
                            aggExternalize ctx (seq + 1L) nxt
                        else
                            aggConsume ctx order (used + grown) (seq + 1L) nxt
                | _ ->
                    let size = aggBucketBytes ctx.SeenBytes key (aggNewBucket ctx.Aggs)

                    if MemoryBudget.exceeded (ctx.Budget.Work, used + size) then
                        aggExternalize ctx seq (Op.emit row nxt)
                    else
                        let bucket = aggNewBucket ctx.Aggs

                        match aggAddToBucket ctx.Aggs ctx.Resolve ctx.Budget bucket row with
                        | DbErr e -> err e
                        | DbOk next ->
                            ctx.Buckets.[key] <- next
                            ctx.OutputKeys.[key] <- k
                            ctx.FirstSeen.[key] <- seq
                            aggConsume ctx (key :: order) (used + size) (seq + 1L) nxt

    /// GROUP BY + 集約。
    /// groupBy が空でも集約があれば 1グループだけ出す
    /// （`SELECT COUNT(*) FROM 空のテーブル` が 1行を返す SQL の挙動）。
    /// <remarks>
    /// 入力行は**抱え込まない**。
    /// メモリは「グループ数 × アキュムレータ」に比例し、行数には比例しない。
    /// グループ数が上限を超えたら以降の行をグループキーでパーティションへ退避し、
    /// パーティションごとに畳み込んでから最初の出現順に並べ直す（`CapacityError` にせず完走する）。
    /// </remarks>
    let aggregate
        (budget: MemoryBudget)
        (schema: Schema)
        (groupBy: Expr list)
        (aggs: (AggFunc * bool * bool * Expr) list)
        (src: Op)
        =
        let resolve =
            Eval.prepare schema (groupBy @ (aggs |> List.map (fun (_, _, _, e) -> e)))

        let normalizeKey = Array.map SqlValue.keyNormalize
        let seenBytes = List.sumBy (fun a -> int64 (Map.count a.Seen) * 32L)

        let buckets =
            System.Collections.Generic.Dictionary<SqlValue array, Acc list>(
                HashIdentity.Structural: System.Collections.Generic.IEqualityComparer<SqlValue array>
            )

        let outputKeys =
            System.Collections.Generic.Dictionary<SqlValue array, SqlValue array>(
                HashIdentity.Structural: System.Collections.Generic.IEqualityComparer<SqlValue array>
            )

        let firstSeen =
            System.Collections.Generic.Dictionary<SqlValue array, int64>(
                HashIdentity.Structural: System.Collections.Generic.IEqualityComparer<SqlValue array>
            )

        let ctx =
            {
                Budget = budget
                Resolve = resolve
                GroupBy = groupBy
                Aggs = aggs
                NormalizeKey = normalizeKey
                SeenBytes = seenBytes
                Buckets = buckets
                OutputKeys = outputKeys
                FirstSeen = firstSeen
            }

        match Eval.checkPrepared resolve with
        | Error e -> err e
        | Ok() -> aggConsume ctx [] 0L 0L src
