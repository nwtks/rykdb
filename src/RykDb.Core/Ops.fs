namespace RykDb.Core

module internal Ops =
    /// DISTINCT が覚えられるユニーク行の概算上限（バイト）。
    /// 超えたら `CapacityError` で止める（OOM でプロセスごと落とさないため）。
    let private DistinctBudgetBytes = 256L * 1024L * 1024L

    /// 述語で弾く。
    let filter (schema: Schema) (predicate: Expr) =
        let resolve = Eval.resolver schema
        Op.filter (fun row -> Eval.matchesWithV resolve row predicate)

    /// ソート。
    /// ORDER BY のキーを列位置へ解決できるならそのまま使い、
    /// 式が含まれる場合はキーを評価して行末へ足してからソートする。
    /// `*` / `t.*` はORDER BY には書けない。
    let sortBy (schema: Schema) (keys: RykDb.Core.SortKey list) (src: Op) =
        let resolve = Eval.resolver schema

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

                result {
                    return! ExtSort.sortRows ExtSort.DefaultBudgetBytes Map.empty (check plan) (compareWith plan) src
                }
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
                        ExtSort.sortRows ExtSort.DefaultBudgetBytes Map.empty (check plan) (compareWith plan) projected

                    return Op.map (fun row -> DbOk(Array.sub row 0 width)) sorted
                }

    /// 重複除去。
    /// 重複を除き、最初に現れた順序を保つ（SQL の DISTINCT と同じ）。
    /// 入力順を保つには、直前の行だけでなく、これまでに現れた行をすべて記録する必要がある。
    /// たとえば `2, 1, 2` では、最後の `2` は直前の `1` と異なるため、直前との比較だけでは除去できない。
    /// <remarks>
    /// 既出行を `SqlValue array` のまま保存すると、値ごとに多くのオブジェクトを保持する。
    /// `ValueCodec` で行をバイト列に変換してから集合に入れる。同じ値の行は同じバイト列になる。
    /// メモリ使用量はユニーク行数に応じて増えるが、上限を設けている。
    /// 上限を超えた場合は、プロセスが OOM で終了する代わりに `CapacityError` を返す。
    /// `seen` は呼び出しごとに作る。クエリ間で共有すると、後続クエリの行まで重複扱いになる。
    /// </remarks>
    let distinct (src: Op) =
        let encode row =
            match ValueCodec.writeValues row Writer.Empty with
            | Error e -> DbErr e
            | Ok w -> DbOk(Writer.toArray w)

        let seen = System.Collections.Generic.HashSet<byte array> HashIdentity.Structural

        let rec skipSeen used =
            Op.pull (fun row nxt ->
                match encode row with
                | DbErr e -> OpStep.OpError e
                | DbOk key ->
                    if seen.Contains key then
                        Op.next (skipSeen used nxt)
                    elif used > DistinctBudgetBytes then
                        sprintf "SELECT DISTINCT のユニーク行が多すぎます（概算 %d バイトの上限）。条件で絞るか、集約で代替してください" DistinctBudgetBytes
                        |> DbError.capacity
                        |> OpStep.OpError
                    else
                        seen.Add key |> ignore
                        OpStep.OpRow(row, skipSeen (used + int64 (ValueCodec.managedRowBytes row)) nxt))

        skipSeen 0L src

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
        let resolve = Eval.resolver Schema.empty

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
    /// 入力行数には比例しない
    /// （従来は全行を `ResizeArray` に保持していた）。
    /// グループ数にも上限を置き、超過は `CapacityError` で止める。
    /// 出力順は従来どおり「最初に現れたグループ」の順。
    /// </remarks>
    let aggregate (schema: Schema) (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (src: Op) =
        let groupBudget = 4_000_000
        let resolve = Eval.resolver schema

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

        let addToBucket (bucket: ResizeArray<Acc>) row =
            let rec step i =
                if i >= aggs.Length then
                    DbOk()
                else
                    let _, star, distinct, arg = aggs.[i]

                    match add star distinct arg bucket.[i] row with
                    | DbErr e -> DbErr e
                    | DbOk acc ->
                        bucket.[i] <- acc
                        step (i + 1)

            step 0

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
            aggs |> List.map (fun (f, _, _, _) -> emptyAcc f) |> ResizeArray

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
            let av = bucket |> Seq.map finishAcc |> Seq.toArray
            Array.append key av

        let buckets =
            System.Collections.Generic.Dictionary<SqlValue array, ResizeArray<Acc>>(
                HashIdentity.Structural: System.Collections.Generic.IEqualityComparer<SqlValue array>
            )

        let order = ResizeArray<SqlValue array>()

        let rec consume cur =
            match Op.next cur with
            | OpStep.OpError e -> err e
            | OpStep.OpDone -> ret ()
            | OpStep.OpRow(row, nxt) ->
                match keyOfRow row with
                | DbErr e -> err e
                | DbOk k ->
                    match buckets.TryGetValue k with
                    | true, bucket ->
                        match addToBucket bucket row with
                        | DbErr e -> err e
                        | DbOk() -> consume nxt
                    | _ ->
                        if order.Count >= groupBudget then
                            sprintf "GROUP BY のグループ数が多すぎます（上限 %d）。条件で絞るか、サンプリングしてください" groupBudget
                            |> DbError.capacity
                            |> err
                        else
                            let bucket = newBucket ()

                            match addToBucket bucket row with
                            | DbErr e -> err e
                            | DbOk() ->
                                buckets.[k] <- bucket
                                order.Add k
                                consume nxt

        let rec emitGroups i =
            if i >= order.Count then
                Op.Empty
            else
                Op.compute (fun () ->
                    let key = order.[i]
                    OpStep.OpRow(rowOf buckets.[key] key, emitGroups (i + 1)))

        result {
            do! consume src

            if order.Count = 0 && List.isEmpty groupBy then
                let vals =
                    aggs |> List.map (fun (f, _, _, _) -> finishAcc (emptyAcc f)) |> List.toArray

                return Op.ofArray [| vals |]
            else
                return emitGroups 0
        }

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
            let resolve = Eval.resolver schema

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
                let resolveAgg = Eval.resolver Schema.empty

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

    // 結合は実装を `OpsJoin.fs` に分けている。
    let innerJoin = OpsJoin.innerJoin
    let leftJoin = OpsJoin.leftJoin
    let innerJoinHash = OpsJoin.innerJoinHash
    let leftJoinHash = OpsJoin.leftJoinHash
    let innerJoinMerge = OpsJoin.innerJoinMerge
    let leftJoinMerge = OpsJoin.leftJoinMerge
    let innerJoinIndexed = OpsJoin.innerJoinIndexed
    let leftJoinIndexed = OpsJoin.leftJoinIndexed
