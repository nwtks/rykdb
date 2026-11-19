namespace RykDb.Core

module internal Ops =
    /// 述語で弾く。
    let filter (schema: Schema) (predicate: Expr) =
        let resolve = Eval.prepare schema [ predicate ]
        Op.filter (fun row -> Eval.matchesWithV resolve row predicate)

    /// ソート。
    /// ORDER BY のキーを列位置へ解決できるならそのまま使い、
    /// 式が含まれる場合はキーを評価して行末へ足してからソートする。
    /// `*` / `t.*` はORDER BY には書けない。
    let sortBy (budget: MemoryBudget) (schema: Schema) (keys: RykDb.Core.SortKey list) (src: Op) =
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

        if List.isEmpty keys then
            ret src
        else
            let columns = keys |> List.map resolveColumn

            if List.forall Option.isSome columns then
                let plan = columns |> List.choose id

                result { return! ExtSort.sortRows budget Map.empty (check plan) (compareWith plan) src }
            else
                let exprs = keys |> List.map _.Expression
                let width = schema.Columns.Length
                let plan = keys |> List.mapi (fun i k -> width + i, k.Direction)

                let projected =
                    Op.map
                        (fun row ->
                            let keyValues = Array.zeroCreate<SqlValue> exprs.Length

                            let rec fill i =
                                function
                                | [] -> DbOk(Array.append row keyValues)
                                | e :: tail ->
                                    match Eval.evalWithV resolve row e with
                                    | DbErr er -> DbErr er
                                    | DbOk v ->
                                        keyValues.[i] <- v
                                        fill (i + 1) tail

                            fill 0 exprs)
                        src

                result {
                    let! sorted =
                        ExtSort.sortRows budget Map.empty (check plan) (compareWith plan) projected

                    return Op.map (fun row -> DbOk(Array.sub row 0 width)) sorted
                }

    /// 重複除去。
    /// 重複を除き、最初に現れた順序を保つ（SQL の DISTINCT と同じ）。
    /// 入力順を保つには、直前の行だけでなく、これまでに現れた行をすべて記録する必要がある。
    /// たとえば `2, 1, 2` では、最後の `2` は直前の `1` と異なるため、直前との比較だけでは除去できない。
    /// <remarks>
    /// 上限内は符号化キーの `HashSet` でストリーミングに返す。
    /// 上限を超えたら (連番, 行) をパーティションへ退避し、
    /// キーごとに最小の連番の行だけを残してから連番順に並べ直す
    /// （＝「最初に現れた順」を保ったまま、ピークメモリを上限内に抑える）。
    /// 既に出した行はマーカー（`Blob` キー + `Int64.MinValue`）で伝え、後続の重複を落とす。
    /// `seen` は呼び出しごとに作る。クエリ間で共有すると、後続クエリの行まで重複扱いになる。
    /// `probe` は `Op.compute` の継続内の再帰で、1行ごとに 1段だけ進む
    /// （TailAudit はラムダ内の呼び出しとして報告する）。
    /// </remarks>
    let distinct (budget: MemoryBudget) (src: Op) =
        let encode row =
            match ValueCodec.writeValues row Writer.Empty with
            | Error e -> DbErr e
            | Ok w -> DbOk(Writer.toArray w)

        let compareSeq (a: SqlValue array) (b: SqlValue array) =
            match a.[0], b.[0] with
            | SqlValue.Int x, SqlValue.Int y -> compare x y
            | _ -> 0

        let seen = System.Collections.Generic.HashSet<byte array> HashIdentity.Structural

        let externalize seq row key rest =
            let stamp = System.Guid.NewGuid().ToString "N"

            match Spill.openPartitions budget.Directory stamp 0 Spill.Partitions with
            | Error e -> err e
            | Ok writer ->
                let marker k =
                    writer.Write(Spill.partitionOfKey 0UL k, [| SqlValue.Blob k; SqlValue.Int System.Int64.MinValue |])

                let frame s r = Array.append [| SqlValue.Int s |] r

                let abandon e =
                    writer.Abandon()
                    err e

                let keyOfFrame (f: SqlValue array) =
                    match f.[0] with
                    | SqlValue.Blob b -> ret b
                    | SqlValue.Int _ -> encode (Array.sub f 1 (f.Length - 1)) |> DbResult.toResult
                    | _ -> err (DbError.internalError "DISTINCT の退避行が不正です")

                let seed () =
                    struct (System.Int64.MaxValue, Array.empty<SqlValue>)

                let addOne struct (best, bestRow) (f: SqlValue array) =
                    match f.[0] with
                    | SqlValue.Blob _ -> Ok struct (System.Int64.MinValue, [||])
                    | SqlValue.Int s ->
                        let r = Array.sub f 1 (f.Length - 1)

                        if s < best then
                            Ok struct (s, r)
                        else
                            Ok struct (best, bestRow)
                    | _ -> Error(DbError.internalError "DISTINCT の退避行が不正です")

                let stateBytes struct (_, r) =
                    int64 (ValueCodec.managedRowBytes r) + 64L

                let emitOne struct (best, r) =
                    if best = System.Int64.MinValue then
                        Ok None
                    else
                        Ok(Some(Array.append [| SqlValue.Int best |] r))

                let markers =
                    (ret (), seen)
                    ||> Seq.fold (fun acc k ->
                        match acc with
                        | Error e -> Error e
                        | Ok() -> marker k)

                let written =
                    markers
                    >>= fun () -> writer.Write(Spill.partitionOfKey 0UL key, frame seq row)
                    >>= fun () -> Spill.fillPartitions writer (encode >> DbResult.toResult) frame (seq + 1L) rest

                match written with
                | Error e -> abandon e
                | Ok _ ->
                    writer.Close()

                    match
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
                    with
                    | Error e -> err e
                    | Ok rows ->
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
    /// 空なら `substituteAggs` は表示文字列を作らない
    /// （深い HAVING 式で O(深さ²) にならないようにするため）。
    let private wholeGroupOffsets =
        List.mapi (fun i e -> e, i)
        >> List.filter (fun (e, _) -> not (isLeafExpr e))
        >> List.map (fun (e, i) -> Expr.toDisplayString e, i)
        >> Map.ofList

    /// 集約呼び出しの式を、計算済みの値（定数）へ置き換える。
    /// HAVING と SELECT 項目の両方で使う。
    /// `offsets` は `aggOffsets` の結果。
    /// 行に依存するのは `row` だけなので offsets は呼び出し側で 1回だけ作る
    /// （`project` / `havingFilter` は出力 1行ごとに呼ぶため、
    /// ここで作ると行数ぶんだけマップを組み直すことになる）。
    /// 組み立ては `Expr.rewrite`（末尾再帰の評価機）に任せるので、式の深さに依存しない。
    let private substituteAggs (offsets: Map<string, int>) (wholeOffsets: Map<string, int>) (row: SqlValue array) =
        let slot key =
            match offsets |> Map.tryFind key with
            | Some i when i >= 0 && i < row.Length -> Some(Expr.Value row.[i])
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

        Expr.rewrite rewriteLeaf

    /// HAVING を張る。
    /// 集約後の行は位置のみの意味を持つので Schema.empty で評価する。
    /// offsets はクロージャを 1回作るときにだけ組み立て、行ごとに持ち越さない。
    let havingFilter (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (predicate: Expr) =
        let offsets = aggOffsets groupBy aggs
        let wholeOffsets = wholeGroupOffsets groupBy
        let resolve = Eval.prepare Schema.empty [ predicate ]

        fun row ->
            match substituteAggs offsets wholeOffsets row predicate with
            | Error e -> DbErr e
            | Ok p -> Eval.matchesWithV resolve row p

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
            First: SqlValue option
            Seen: Set<SqlValue>
        }

    /// GROUP BY + 集約。
    /// groupBy が空でも集約があれば 1グループだけ出す
    /// （`SELECT COUNT(*) FROM 空のテーブル` が 0 ではなく 1行を返すため。SQL の通常の挙動）。
    /// <remarks>
    /// 入力行は**抱え込まない**。
    /// グループごとの集約値だけを更新していくので、
    /// メモリは「グループ数 × アキュムレータ」に比例し、
    /// 入力行数には比例しない。
    /// グループ数が上限を超えたら、以降の行をグループキーでパーティションへ退避し、
    /// パーティションごとに畳み込んでから先頭の出現順に並べ直す
    /// （上限を超えても `CapacityError` にせず、ピークメモリを抑えて完走する）。
    /// 出力順は従来どおり「最初に現れたグループ」の順。
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

        let addDecimal a b =
            let maxD = System.Decimal.MaxValue
            let minD = System.Decimal.MinValue

            if a > 0M && b > 0M && a > maxD - b || a < 0M && b < 0M && a < minD - b then
                DbErr(DbError.constraintViolation "SUM の結果が DECIMAL の範囲を超えました")
            else
                DbOk(a + b)

        let addValue acc bump v newSeen =
            match acc.Kind with
            | AggFunc.Count -> DbOk { bump with Seen = newSeen }
            | AggFunc.Sum
            | AggFunc.Avg ->
                match v with
                | SqlValue.Real f ->
                    DbOk
                        { bump with
                            SawReal = true
                            SumReal = acc.SumReal + float acc.SumDecimal + f
                            SumDecimal = 0M
                            Seen = newSeen
                        }
                | SqlValue.Int _
                | SqlValue.Decimal _ ->
                    match SqlValue.asNumeric v with
                    | Some d ->
                        if acc.SawReal then
                            DbOk
                                { bump with
                                    SumReal = acc.SumReal + float d
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
            let alreadySeen =
                distinct
                && match value with
                   | Some v when not (SqlValue.isNull v) -> Set.contains v acc.Seen
                   | _ -> false

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

                let bump = { acc with Count = acc.Count + 1L }

                let newSeen =
                    if distinct then
                        match value with
                        | Some v when not (SqlValue.isNull v) -> Set.add v acc.Seen
                        | _ -> acc.Seen
                    else
                        acc.Seen

                match counted with
                | None -> DbOk { acc with Seen = newSeen }
                | Some v -> addValue acc bump v newSeen

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
                First = None
                Seen = Set.empty
            }

        let newBucket () =
            aggs |> List.map (fun (f, _, _, _) -> emptyAcc f)

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

        let rowOf bucket key =
            bucket |> List.map finishAcc |> List.toArray |> Array.append key

        let bucketBytes key bucket =
            let seen = bucket |> List.sumBy (fun a -> int64 (Set.count a.Seen) * 32L)
            int64 (ValueCodec.managedRowBytes key) + 96L * int64 bucket.Length + seen + 64L

        let emptyResult () =
            aggs |> List.map (fun (f, _, _, _) -> finishAcc (emptyAcc f)) |> List.toArray

        let compareFirst (a: SqlValue array) (b: SqlValue array) =
            match a.[0], b.[0] with
            | SqlValue.Int x, SqlValue.Int y -> compare x y
            | _ -> 0

        let encodeKey key =
            ValueCodec.writeValues key Writer.Empty |> Result.map Writer.toArray

        let buckets =
            System.Collections.Generic.Dictionary<SqlValue array, Acc list>(
                HashIdentity.Structural: System.Collections.Generic.IEqualityComparer<SqlValue array>
            )

        let firstSeen =
            System.Collections.Generic.Dictionary<SqlValue array, int64>(
                HashIdentity.Structural: System.Collections.Generic.IEqualityComparer<SqlValue array>
            )

        let externalize row seq rest =
            let stamp = System.Guid.NewGuid().ToString "N"

            match Spill.openPartitions budget.Directory stamp 0 Spill.Partitions with
            | Error e -> err e
            | Ok writer ->
                let keyOf r =
                    keyOfRow r |> DbResult.toResult >>= encodeKey

                let frame s r = Array.append [| SqlValue.Int s |] r

                let abandon e =
                    writer.Abandon()
                    err e

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
                        | DbErr e -> Error e
                        | DbOk k ->
                            match addToBucket accs r with
                            | DbErr e -> Error e
                            | DbOk next -> Ok struct (k, next, min first s)
                    | _ -> Error(DbError.internalError "GROUP BY の退避行が不正です")

                let stateBytes struct (key, accs, _) = bucketBytes key accs

                let emitGroup struct (key, accs, first) =
                    Array.append
                        [| SqlValue.Int first |]
                        (Array.append key (accs |> List.map finishAcc |> List.toArray))
                    |> Some
                    |> Ok

                let written = Spill.fillPartitions writer keyOf frame seq (Op.emit row rest)

                match written with
                | Error e -> abandon e
                | Ok _ ->
                    writer.Close()

                    let seeds =
                        buckets
                        |> Seq.choose (fun kv ->
                            match encodeKey kv.Key with
                            | Ok encoded -> Some(encoded, struct (kv.Key, kv.Value, firstSeen.[kv.Key]))
                            | Error _ -> None)
                        |> List.ofSeq

                    match
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
                    with
                    | Error e -> err e
                    | Ok rows ->
                        ExtSort.sortRows budget () (fun () _ -> DbOk()) compareFirst rows
                        |> Result.map (Op.map (fun f -> DbOk(Array.sub f 1 (f.Length - 1))))

        let rec emitGroups =
            function
            | [] -> Op.Empty
            | key :: rest -> Op.compute (fun () -> OpStep.OpRow(rowOf buckets.[key] key, emitGroups rest))

        let rec consume order used seq cur =
            match Op.next cur with
            | OpStep.OpError e -> err e
            | OpStep.OpDone ->
                if List.isEmpty order && List.isEmpty groupBy then
                    ret (Op.ofArray [| emptyResult () |])
                else
                    ret (emitGroups (List.rev order))
            | OpStep.OpRow(row, nxt) ->
                match keyOfRow row with
                | DbErr e -> err e
                | DbOk k ->
                    match buckets.TryGetValue k with
                    | true, bucket ->
                        match addToBucket bucket row with
                        | DbErr e -> err e
                        | DbOk next ->
                            buckets.[k] <- next
                            consume order used (seq + 1L) nxt
                    | _ ->
                        let size = bucketBytes k (newBucket ())

                        if MemoryBudget.exceeded (budget.Work, used + size) then
                            externalize row seq nxt
                        else
                            let bucket = newBucket ()

                            match addToBucket bucket row with
                            | DbErr e -> err e
                            | DbOk next ->
                                buckets.[k] <- next
                                firstSeen.[k] <- seq
                                consume (k :: order) (used + size) (seq + 1L) nxt

        consume [] 0L 0L src

    /// 射影。
    /// `SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は対象テーブルの全列に展開する。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    let project (schema: Schema) (groupBy: Expr list) (items: SelectItem list) (src: Op) =
        let aggCallsOf = List.map _.Expression >> aggCalls

        if List.isEmpty items then
            src
        else
            let projection = SelectProjection.create schema items
            let allExprs = projection.Expressions
            let foundAggs = aggCallsOf projection.SelectItems
            let offsets = aggOffsets groupBy foundAggs
            let wholeOffsets = wholeGroupOffsets groupBy
            let resolve = Eval.prepare schema allExprs

            let evalItems row (exprs: Expr list) =
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

            if List.isEmpty foundAggs && List.isEmpty groupBy then
                Op.map (fun row -> evalItems row allExprs) src
            else
                let resolveAgg = Eval.prepare Schema.empty allExprs

                Op.map
                    (fun row ->
                        let values = Array.zeroCreate<SqlValue> allExprs.Length

                        let rec fill i =
                            function
                            | [] -> DbOk values
                            | e :: tail ->
                                match substituteAggs offsets wholeOffsets row e with
                                | Error er -> DbErr er
                                | Ok e ->
                                    match Eval.evalWithV resolveAgg row e with
                                    | DbErr er -> DbErr er
                                    | DbOk v ->
                                        values.[i] <- v
                                        fill (i + 1) tail

                        fill 0 allExprs)
                    src
