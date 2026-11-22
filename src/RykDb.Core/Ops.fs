namespace RykDb.Core

module internal Ops =
    /// 集約呼び出しの識別キー（`COUNT(age)` のような表示文字列）。
    let private aggKey (f: AggFunc) (distinct: bool) (arg: Expr) =
        if distinct then
            sprintf "%s(DISTINCT %s)" f.Name (Expr.toDisplayString arg)
        else
            sprintf "%s(%s)" f.Name (Expr.toDisplayString arg)

    /// 集約後の行のスロット番号表。
    /// GROUP BY 式が先頭に、集約呼び出しがその後に並ぶ（`Ops.aggregate` の出力順）。
    /// 行に依存しないので 1クエリにつき 1回だけ作る。
    let private aggOffsets (groupBy: Expr list) =
        let groupSlots =
            groupBy |> List.mapi (fun i e -> Expr.toDisplayString e, i) |> Map.ofList

        List.mapi (fun i (f, _, distinct, arg) -> aggKey f distinct arg, List.length groupBy + i)
        >> Map.ofList
        >> Map.fold (fun acc k v -> Map.add k v acc) groupSlots

    /// 葉（列・リテラル・集約呼び出し）は既存の置換で扱える。
    /// それ以外の GROUP BY 式（`age + 1` など）だけ、木全体の表示文字列で照合する。
    let private isLeafExpr =
        function
        | Expr.Literal _
        | Expr.Value _
        | Expr.Column _
        | Expr.Name _
        | Expr.Star
        | Expr.QualifiedStar _
        | Expr.AggCall _ -> true
        | _ -> false

    /// 木全体を置き換える必要がある GROUP BY 式の表示 -> スロット。
    /// 空なら `substituteAggSlots` は表示文字列を作らない
    /// （深い HAVING 式で O(深さ²) にならないようにするため）。
    let private wholeGroupOffsets =
        List.mapi (fun i e -> e, i)
        >> List.filter (fun (e, _) -> not (isLeafExpr e))
        >> List.map (fun (e, i) -> Expr.toDisplayString e, i)
        >> Map.ofList

    /// 集約後の行のスロットを指す予約名。
    /// ソースに書けない制御文字から始め、ユーザーの識別子と衝突しないようにする。
    let private slotName (i: int) = "\u0001" + string i

    /// スロット参照だけを列に持つスキーマ（列位置 = スロット番号）。
    /// 置換後に残った列参照はここで解決できず、構築時の `checkPrepared` が NameError にする。
    let private slotSchema (width: int) =
        { Schema.empty with
            Columns =
                Array.init width (fun i ->
                    {
                        Name = slotName i
                        Qualifier = None
                        DataType = None
                    })
        }

    /// 集約後の式を、グループ値・集約値のスロット参照へ置き換える（計画時に 1 回）。
    /// 置換の構造は行の値に依存しないので、行ごとに組み直す必要はない。
    let private substituteAggSlots (offsets: Map<string, int>) (wholeOffsets: Map<string, int>) (e: Expr) =
        let slot key =
            match offsets |> Map.tryFind key with
            | Some i when i >= 0 -> Some(Expr.Column { Qualifier = None; Name = slotName i })
            | _ -> None

        let wholeSlot x =
            if Map.isEmpty wholeOffsets || isLeafExpr x then
                None
            else
                slot (Expr.toDisplayString x)

        let rewriteLeaf x =
            match wholeSlot x with
            | Some l -> l
            | None ->
                match x with
                | Expr.AggCall(f, distinct, arg) ->
                    match slot (aggKey f distinct arg) with
                    | Some l -> l
                    | None -> Expr.Literal Literal.Null
                | Expr.Literal l -> Expr.Literal l
                | Expr.Column _ as orig ->
                    match slot (Expr.toDisplayString orig) with
                    | Some l -> l
                    | None -> orig
                | Expr.Name n as orig ->
                    match slot n with
                    | Some l -> l
                    | None -> orig
                | other -> other

        Expr.rewrite rewriteLeaf e

    /// 集約後の行を読む式の一覧を、スロット参照の式と評価準備へ変換する。
    /// 幅は集約後の行幅（GROUP BY 数 + 集約数）。
    /// 解決できない参照が残っていれば、行を 1つも評価する前に NameError を返す。
    let private prepareAggExprs (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (exprs: Expr list) =
        let offsets = aggOffsets groupBy aggs
        let wholeOffsets = wholeGroupOffsets groupBy
        let width = groupBy.Length + aggs.Length
        let substituted = all (List.map (substituteAggSlots offsets wholeOffsets) exprs)

        match substituted with
        | Error e -> err e
        | Ok slotExprs ->
            let resolve = Eval.prepare (slotSchema width) slotExprs

            match Eval.checkPrepared resolve with
            | Error e -> err e
            | Ok() -> ret (resolve, slotExprs)

    /// 述語で弾く。
    /// 列参照が解決できなければ、行を評価する前に NameError を返す
    /// （入力が 0 行でも未知の列・関数名はエラー）。
    let filter (schema: Schema) (predicate: Expr) (src: Op) =
        let resolve = Eval.prepare schema [ predicate ]

        match Eval.checkPrepared resolve with
        | Error e -> err e
        | Ok() -> ret (Op.filter (fun row -> Eval.matchesWithV resolve row predicate) src)

    /// ソート。
    /// ORDER BY のキーを列位置へ解決できるならそのまま使い、
    /// 式が含まれる場合はキーを評価して行末へ足してからソートする。
    /// 集約後の行（`groupBy` / `aggs` が非空）では、キー式を
    /// グループ値・集約値のスロット参照へ計画時に置き換えてから評価する
    /// （`ORDER BY COUNT(*)` / `ORDER BY age + 1` / 別名・序数経由の同じ形）。
    /// `*` / `t.*` はORDER BY には書けない。
    let sortBy
        (budget: MemoryBudget)
        (schema: Schema)
        (groupBy: Expr list)
        (aggs: (AggFunc * bool * bool * Expr) list)
        (keys: RykDb.Core.SortKey list)
        (src: Op)
        =
        let resolve = Eval.prepare schema (keys |> List.map _.Expression)

        let compareWith plan (a: SqlValue array) (b: SqlValue array) =
            let rec compareKeys =
                function
                | [] -> 0
                | (i, dir) :: tail ->
                    match SqlValue.compareValuesForSortV a.[i] b.[i] with
                    | DbErr _ -> compareKeys tail
                    | DbOk c ->
                        if c = 0 then
                            compareKeys tail
                        else
                            match dir with
                            | SortDirection.Ascending -> c
                            | SortDirection.Descending -> -c

            compareKeys plan

        let check plan reps (row: SqlValue array) =
            let rec loop acc =
                function
                | [] -> DbOk acc
                | (i, _) :: tail ->
                    let v = row.[i]

                    if SqlValue.isNull v then
                        loop acc tail
                    else
                        match Map.tryFind i acc with
                        | None -> loop (Map.add i v acc) tail
                        | Some representative ->
                            match SqlValue.compareValuesV representative v with
                            | DbErr e -> DbErr e
                            | DbOk _ -> loop acc tail

            loop reps plan

        let resolveColumn (k: SortKey) =
            match Schema.tryAsColumnRef k.Expression with
            | None -> None
            | Some c ->
                let bare = { Qualifier = None; Name = c.Name }

                match resolve.Resolve c with
                | Ok i -> Some(i, k.Direction)
                | Error _ ->
                    match resolve.Resolve bare with
                    | Ok i -> Some(i, k.Direction)
                    | Error _ -> None

        let unresolved =
            keys
            |> List.tryPick (fun k ->
                match Schema.tryAsColumnRef k.Expression with
                | None -> None
                | Some c ->
                    let bare = { Qualifier = None; Name = c.Name }

                    match Schema.resolve c schema with
                    | Ok _ -> None
                    | Error _ ->
                        match Schema.resolve bare schema with
                        | Ok _ -> None
                        | Error e -> Some e)

        match unresolved with
        | Some e -> err e
        | None ->
            if List.isEmpty keys then
                ret src
            else
                let columns = keys |> List.map resolveColumn

                if List.forall Option.isSome columns then
                    let plan = columns |> List.choose id

                    result { return! ExtSort.sortRows budget Map.empty (check plan) (compareWith plan) src }
                else
                    let exprs = keys |> List.map _.Expression

                    let prepared =
                        if List.isEmpty groupBy && List.isEmpty aggs then
                            let resolve = Eval.prepare schema exprs

                            match Eval.checkPrepared resolve with
                            | Error e -> err e
                            | Ok() -> ret (resolve, exprs)
                        else
                            prepareAggExprs groupBy aggs exprs

                    match prepared with
                    | Error e -> err e
                    | Ok(resolveKeys, slotExprs) ->
                        let width = schema.Columns.Length
                        let plan = keys |> List.mapi (fun i k -> width + i, k.Direction)

                        let projected =
                            Op.map
                                (fun row ->
                                    let keyValues = Array.zeroCreate<SqlValue> slotExprs.Length

                                    let rec fill i =
                                        function
                                        | [] -> DbOk(Array.append row keyValues)
                                        | e :: tail ->
                                            match Eval.evalWithV resolveKeys row e with
                                            | DbErr er -> DbErr er
                                            | DbOk v ->
                                                keyValues.[i] <- v
                                                fill (i + 1) tail

                                    fill 0 slotExprs)
                                src

                        ExtSort.sortRows budget Map.empty (check plan) (compareWith plan) projected
                        |> Result.map (Op.map (fun row -> DbOk(Array.sub row 0 width)))

    /// 重複除去。
    /// 最初に現れた順序を保つ（SQL の DISTINCT）。
    /// <remarks>
    /// 上限内は符号化キーの `HashSet` でストリーミングし、
    /// 超えたら (連番, 行) をパーティションへ退避し、
    /// キーごとに最小連番の行だけを残して連番順に並べ直す（出現順を保つ）。
    /// 既に出した行はマーカー（`Blob` キー + `Int64.MinValue`）で伝える。
    /// `seen` は呼び出しごとに作る（クエリ間で共有すると後続クエリの行まで重複扱いになる）。
    /// `probe` は `Op.compute` の継続内の再帰で 1行ごとに 1段だけ進む。
    /// </remarks>
    let distinct (budget: MemoryBudget) (src: Op) =
        let encode row =
            match KeyBuilder.ofValues (Array.map SqlValue.keyNormalize row) with
            | Error e -> DbErr e
            | Ok key -> DbOk key

        let compareSeq (a: SqlValue array) (b: SqlValue array) =
            match a.[0], b.[0] with
            | SqlValue.Int x, SqlValue.Int y -> compare x y
            | _ -> 0

        let frame s r = Array.append [| SqlValue.Int s |] r

        let keyOfFrame (f: SqlValue array) =
            match f.[0] with
            | SqlValue.Blob b -> ret b
            | SqlValue.Int _ -> encode (Array.sub f 1 (f.Length - 1)) |> DbResult.toResult
            | _ -> err (DbError.internalError "DISTINCT の退避行が不正です")

        let seed () =
            struct (System.Int64.MaxValue, Array.empty<SqlValue>)

        let addOne struct (best, bestRow) (f: SqlValue array) =
            match f.[0] with
            | SqlValue.Blob _ -> ret struct (System.Int64.MinValue, [||])
            | SqlValue.Int s ->
                let r = Array.sub f 1 (f.Length - 1)

                if s < best then
                    ret struct (s, r)
                else
                    ret struct (best, bestRow)
            | _ -> err (DbError.internalError "DISTINCT の退避行が不正です")

        let stateBytes struct (_, r) =
            int64 (ValueCodec.managedRowBytes r) + 64L

        let emitOne struct (best, r) =
            if best = System.Int64.MinValue then
                ret None
            else
                ret (Some(Array.append [| SqlValue.Int best |] r))

        let seen = System.Collections.Generic.HashSet<byte array> HashIdentity.Structural

        let externalize seq row key rest =
            let stamp = System.Guid.NewGuid().ToString "N"

            Spill.openPartitions budget.Directory stamp 0 Spill.Partitions
            >>= fun writer ->
                let marker k =
                    writer.Write(Spill.partitionOfKey 0UL k, [| SqlValue.Blob k; SqlValue.Int System.Int64.MinValue |])

                let abandon e =
                    writer.Abandon()
                    err e

                let written =
                    (ret (), seen)
                    ||> Seq.fold (fun acc k -> acc >>= fun () -> marker k)
                    >>= fun () -> writer.Write(Spill.partitionOfKey 0UL key, frame seq row)
                    >>= fun () -> Spill.fillPartitions writer (encode >> DbResult.toResult) frame (seq + 1L) rest

                match written with
                | Error e -> abandon e
                | Ok _ ->
                    writer.Close()

                    Spill.groupPartitions
                        budget
                        stamp
                        keyOfFrame
                        seed
                        addOne
                        stateBytes
                        emitOne
                        []
                        (writer.Paths |> Array.toList)
                    >>= fun rows ->
                        ExtSort.sortRows budget () (fun () _ -> DbOk()) compareSeq rows
                        |> Result.map (Op.map (fun f -> DbOk(Array.sub f 1 (f.Length - 1))))

        let rec probe used seq cur =
            Op.compute (fun () ->
                match Op.next cur with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone -> OpStep.OpDone
                | OpStep.OpRow(row, nxt) ->
                    match encode row with
                    | DbErr e -> OpStep.OpError e
                    | DbOk key ->
                        if seen.Contains key then
                            Op.next (probe used seq nxt)
                        else
                            let size = int64 (ValueCodec.managedRowBytes row) + 32L

                            if MemoryBudget.exceeded (budget.Work, used + size) then
                                match externalize seq row key nxt with
                                | Error e -> OpStep.OpError e
                                | Ok op -> Op.next op
                            else
                                seen.Add key |> ignore
                                OpStep.OpRow(row, probe (used + size) (seq + 1L) nxt))

        probe 0L 0L src

    /// 式中に現れる集約呼び出しの一覧（実行順に揃える）。
    let aggCalls =
        let isStarArg =
            function
            | Expr.Star -> true
            | _ -> false

        let collectAggs =
            Expr.fold (fun a node ->
                match node with
                | Expr.AggCall(f, distinct, x) -> (f, isStarArg x, distinct, x) :: a
                | _ -> a)

        List.fold collectAggs [] >> List.rev

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
            Seen: Set<SqlValue>
            SeenRuns: string list
        }

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

        let keyOfRow row =
            let keys = Array.zeroCreate<SqlValue> groupBy.Length

            let rec fill i =
                function
                | [] -> DbOk keys
                | e :: tail ->
                    match Eval.evalWithV resolve row e with
                    | DbErr er -> DbErr er
                    | DbOk v ->
                        keys.[i] <- v
                        fill (i + 1) tail

            fill 0 groupBy

        let normalizeKey = Array.map SqlValue.keyNormalize

        let addDecimal a b =
            let maxD = System.Decimal.MaxValue
            let minD = System.Decimal.MinValue

            if a > 0M && b > 0M && a > maxD - b || a < 0M && b < 0M && a < minD - b then
                DbErr(DbError.constraintViolation "SUM の結果が DECIMAL の範囲を超えました")
            else
                DbOk(a + b)

        let spillSeen acc =
            let stamp = System.Guid.NewGuid().ToString "N"

            let rows =
                acc.Seen
                |> Set.toList
                |> List.map (fun v ->
                    ValueCodec.writeValues [| v |] Writer.Empty
                    |> Result.map (fun w -> [| SqlValue.Blob(Writer.toArray w) |]))
                |> all

            match rows with
            | Error e -> DbErr e
            | Ok rows ->
                match Spill.writeChunk budget.Directory stamp 0 (List.toArray rows) with
                | Error e -> DbErr e
                | Ok path ->
                    DbOk
                        { acc with
                            Seen = Set.empty
                            SeenRuns = path :: acc.SeenRuns
                        }

        let addValue acc bump v newSeen =
            match acc.Kind with
            | AggFunc.Count -> DbOk { bump with Seen = newSeen }
            | AggFunc.Sum
            | AggFunc.Avg ->
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
                            addDecimal acc.SumDecimal d
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

        let addAcc acc value isStar distinct =
            let seenValue =
                match value with
                | Some v when not (SqlValue.isNull v) -> Some(SqlValue.keyNormalize v)
                | _ -> None

            let alreadySeen =
                distinct && seenValue |> Option.exists (fun v -> Set.contains v acc.Seen)

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
                        match seenValue with
                        | Some v -> Set.add v acc.Seen
                        | None -> acc.Seen
                    else
                        acc.Seen

                let withSeen =
                    let seenLimit =
                        if budget.Work < 0L then
                            System.Int64.MaxValue
                        else
                            max 64L (budget.Work / 32L)

                    if distinct && int64 (Set.count newSeen) > seenLimit then
                        spillSeen { acc with Seen = newSeen }
                    else
                        DbOk { acc with Seen = newSeen }

                match withSeen with
                | DbErr e -> DbErr e
                | DbOk a ->
                    match counted with
                    | None -> DbOk a
                    | Some v ->
                        let bump = { a with Count = a.Count + 1L }
                        addValue a bump v a.Seen

        let add star distinct arg acc row =
            if star then
                addAcc acc None true distinct
            else
                match Eval.evalWithV resolve row arg with
                | DbErr e -> DbErr e
                | DbOk v -> addAcc acc (Some v) false distinct

        let addToBucket bucket row =
            let rec step defs accs out =
                match defs, accs with
                | [], [] -> DbOk(List.rev out)
                | (_, star, distinct, arg) :: defsTail, acc :: accsTail ->
                    match add star distinct arg acc row with
                    | DbErr e -> DbErr e
                    | DbOk next -> step defsTail accsTail (next :: out)
                | _ -> DbErr(DbError.internalError "集約の状態数が一致しません（内部不整合）")

            step aggs bucket []

        let emptyAcc kind =
            {
                Kind = kind
                Count = 0L
                SumDecimal = 0M
                SawReal = false
                SumReal = 0.0
                Min = None
                Max = None
                Seen = Set.empty
                SeenRuns = []
            }

        let newBucket () =
            aggs |> List.map (fun (f, _, _, _) -> emptyAcc f)

        let compareBlob (a: SqlValue array) (b: SqlValue array) =
            match a.[0], b.[0] with
            | SqlValue.Blob x, SqlValue.Blob y -> Bytes.compare x y
            | _ -> 0

        let foldDistinct fresh prev cur =
            let rec go (a: Acc) previous op =
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

                            match addValue a bump v Set.empty with
                            | DbErr e -> DbErr e
                            | DbOk next -> go next (Some bytes) nxt
                    | SqlValue.Blob _ -> go a previous nxt
                    | _ -> DbErr(DbError.internalError "DISTINCT 集約の退避行が不正です")

            go fresh prev cur

        let finalizeAcc acc =
            if List.isEmpty acc.SeenRuns then
                DbOk acc
            else
                let flushed = if Set.isEmpty acc.Seen then DbOk acc else spillSeen acc

                match flushed with
                | DbErr e -> DbErr e
                | DbOk acc ->
                    let paths = acc.SeenRuns

                    let runOps =
                        paths
                        |> List.map (fun p -> Spill.reopen budget.Scope (Spill.RowSpool.Disk([ p ], 0)))
                        |> all

                    match runOps with
                    | Error e ->
                        Spill.forget paths
                        DbErr e
                    | Ok ops ->
                        let src = List.foldBack Op.append ops Op.Empty

                        match ExtSort.sortRows budget () (fun () _ -> DbOk()) compareBlob src with
                        | Error e ->
                            Spill.forget paths
                            DbErr e
                        | Ok sorted ->
                            foldDistinct (emptyAcc acc.Kind) None sorted
                            |> DbResult.map (fun final -> { final with SeenRuns = [] })

        let finishAcc acc =
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

        let rowOf key bucket =
            bucket |> List.map finishAcc |> List.toArray |> Array.append key

        let seenBytes = List.sumBy (fun a -> int64 (Set.count a.Seen) * 32L)

        let bucketBytes key bucket =
            let seen =
                seenBytes bucket
                + (bucket |> List.sumBy (fun a -> int64 (List.length a.SeenRuns) * 64L))

            int64 (ValueCodec.managedRowBytes key) + 96L * int64 bucket.Length + seen + 64L

        let emptyResult () =
            aggs |> List.map (fun (f, _, _, _) -> finishAcc (emptyAcc f)) |> List.toArray

        let compareFirst (a: SqlValue array) (b: SqlValue array) =
            match a.[0], b.[0] with
            | SqlValue.Int x, SqlValue.Int y -> compare x y
            | _ -> 0

        let keyOf r =
            match keyOfRow r with
            | DbErr e -> err e
            | DbOk k -> KeyBuilder.ofValues (normalizeKey k)

        let frame s = Array.append [| SqlValue.Int s |]

        let keyOfFrame (f: SqlValue array) =
            match f.[0] with
            | SqlValue.Int _ -> keyOf (Array.sub f 1 (f.Length - 1))
            | _ -> err (DbError.internalError "GROUP BY の退避行が不正です")

        let seed () =
            struct (Array.empty<SqlValue>, newBucket (), System.Int64.MaxValue)

        let addFrame struct (_, accs, first) (f: SqlValue array) =
            match f.[0] with
            | SqlValue.Int s ->
                let r = Array.sub f 1 (f.Length - 1)

                match keyOfRow r with
                | DbErr e -> err e
                | DbOk k ->
                    match addToBucket accs r with
                    | DbErr e -> err e
                    | DbOk next -> ret struct (k, next, min first s)
            | _ -> err (DbError.internalError "GROUP BY の退避行が不正です")

        let stateBytes struct (key, accs, _) = bucketBytes key accs

        let emitGroup struct (key, accs, first) =
            accs
            |> List.map (finalizeAcc >> DbResult.toResult)
            |> all
            |> Result.map (fun finalAccs ->
                Array.append
                    [| SqlValue.Int first |]
                    (Array.append key (finalAccs |> List.map finishAcc |> List.toArray))
                |> Some)

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

        let externalize startSeq start =
            let stamp = System.Guid.NewGuid().ToString "N"

            Spill.openPartitions budget.Directory stamp 0 Spill.Partitions
            >>= fun writer ->
                let abandon e =
                    writer.Abandon()
                    err e

                let written = Spill.fillPartitions writer keyOf frame startSeq start

                match written with
                | Error e -> abandon e
                | Ok _ ->
                    writer.Close()

                    let seeds =
                        buckets
                        |> Seq.choose (fun kv ->
                            match KeyBuilder.ofValues kv.Key with
                            | Ok encoded -> Some(encoded, struct (outputKeys.[kv.Key], kv.Value, firstSeen.[kv.Key]))
                            | Error _ -> None)
                        |> List.ofSeq

                    Spill.groupPartitions
                        budget
                        stamp
                        keyOfFrame
                        seed
                        addFrame
                        stateBytes
                        emitGroup
                        seeds
                        (writer.Paths |> Array.toList)
                    >>= fun rows ->
                        ExtSort.sortRows budget () (fun () _ -> DbOk()) compareFirst rows
                        |> Result.map (Op.map (fun f -> DbOk(Array.sub f 1 (f.Length - 1))))

        let rec emitGroups =
            function
            | [] -> Op.Empty
            | key :: rest -> Op.compute (fun () -> OpStep.OpRow(rowOf outputKeys.[key] buckets.[key], emitGroups rest))

        let rec consume order used seq cur =
            match Op.next cur with
            | OpStep.OpError e -> err e
            | OpStep.OpDone ->
                buckets
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> List.ofSeq
                |> List.map (fun (k, bucket) ->
                    bucket
                    |> List.map (finalizeAcc >> DbResult.toResult)
                    |> all
                    |> Result.map (fun final -> k, final))
                |> all
                >>= fun finalized ->
                    finalized |> List.iter (fun (k, bucket) -> buckets.[k] <- bucket)

                    if List.isEmpty order && List.isEmpty groupBy then
                        ret (Op.ofArray [| emptyResult () |])
                    else
                        ret (emitGroups (List.rev order))
            | OpStep.OpRow(row, nxt) ->
                match keyOfRow row with
                | DbErr e -> err e
                | DbOk k ->
                    let key = normalizeKey k

                    match buckets.TryGetValue key with
                    | true, bucket ->
                        match addToBucket bucket row with
                        | DbErr e -> err e
                        | DbOk next ->
                            buckets.[key] <- next
                            let grown = max 0L (seenBytes next - seenBytes bucket)

                            if MemoryBudget.exceeded (budget.Work, used + grown) then
                                externalize (seq + 1L) nxt
                            else
                                consume order (used + grown) (seq + 1L) nxt
                    | _ ->
                        let size = bucketBytes key (newBucket ())

                        if MemoryBudget.exceeded (budget.Work, used + size) then
                            externalize seq (Op.emit row nxt)
                        else
                            let bucket = newBucket ()

                            match addToBucket bucket row with
                            | DbErr e -> err e
                            | DbOk next ->
                                buckets.[key] <- next
                                outputKeys.[key] <- k
                                firstSeen.[key] <- seq
                                consume (key :: order) (used + size) (seq + 1L) nxt

        match Eval.checkPrepared resolve with
        | Error e -> err e
        | Ok() -> consume [] 0L 0L src

    /// 射影。
    /// `SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は `expandSchema`（論理的な入力スキーマ = FROM 順）で展開する。
    /// 行の評価は `evalSchema`（実行時の実際のレイアウト）で解決する。
    /// JOIN の並べ替えで両者がずれても、`*` の列順は論理スキーマのまま保たれる。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    /// 置換は計画時に 1回だけ行い、行ごとには組み直さない。
    /// 列参照が解決できなければ、行を評価する前に NameError を返す
    /// （集約後は GROUP BY 列と集約呼び出しを置き換えた後の式で検査する）。
    let project (expandSchema: Schema) (evalSchema: Schema) (groupBy: Expr list) (items: SelectItem list) (src: Op) =
        let aggCallsOf = List.map _.Expression >> aggCalls

        let evalItems resolve row (exprs: Expr list) =
            let values = Array.zeroCreate<SqlValue> exprs.Length

            let rec fill i =
                function
                | [] -> DbOk values
                | e :: tail ->
                    match Eval.evalWithV resolve row e with
                    | DbErr er -> DbErr er
                    | DbOk v ->
                        values.[i] <- v
                        fill (i + 1) tail

            fill 0 exprs

        if List.isEmpty items then
            ret src
        else
            let projection = SelectProjection.create expandSchema items
            let allExprs = projection.Expressions
            let foundAggs = aggCallsOf projection.SelectItems

            if List.isEmpty foundAggs && List.isEmpty groupBy then
                let resolve = Eval.prepare evalSchema allExprs

                match Eval.checkPrepared resolve with
                | Error e -> err e
                | Ok() -> ret (Op.map (fun row -> evalItems resolve row allExprs) src)
            else
                prepareAggExprs groupBy foundAggs allExprs
                |> Result.map (fun (resolveAgg, slotExprs) ->
                    Op.map (fun row -> evalItems resolveAgg row slotExprs) src)
