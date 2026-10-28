namespace RykDb.Core

module internal Ops =
    /// DISTINCT が覚えられるユニーク行の概算上限（バイト）。
    /// 超えたら `CapacityError` で止める（OOM でプロセスごと落とさないため）。
    let private DistinctBudgetBytes = 256L * 1024L * 1024L

    /// 1行の概算バイト数（メモリ使用量の見積もり用）。
    let private rowBytes (row: SqlValue array) =
        row |> Array.sumBy ValueCodec.approximateBytes

    /// 述語で弾く。
    let filter (schema: Schema) (src: Op) (predicate: Expr) =
        Op.filter (fun row -> Eval.matches schema row predicate) src

    /// ソート。
    /// ORDER BY のキー（列位置と方向）を先に解決してから比較関数へ落とす。
    /// `*` / `t.*` はORDER BY には書けない。
    let sortBy (schema: Schema) (src: Op) (keys: RykDb.Core.SortKey list) =
        let compareWith plan (a: SqlValue array) (b: SqlValue array) =
            let rec compareKeys (rest: (int * SortDirection) list) =
                match rest with
                | [] -> 0
                | (i, dir) :: tail ->
                    match SqlValue.compareValues a.[i] b.[i] with
                    | Error _ -> compareKeys tail
                    | Ok c ->
                        if c = 0 then
                            compareKeys tail
                        else
                            match dir with
                            | SortDirection.Ascending -> c
                            | SortDirection.Descending -> -c

            compareKeys plan

        let check plan (reps: Map<int, SqlValue>) (row: SqlValue array) =
            plan
            |> List.fold
                (fun (acc: Result<Map<int, SqlValue>, DbError>) (i, _) ->
                    acc
                    |> Result.bind (fun m ->
                        let v = row.[i]

                        if SqlValue.isNull v then
                            ret m
                        else
                            match Map.tryFind i m with
                            | None -> ret (Map.add i v m)
                            | Some representative ->
                                SqlValue.compareValues representative v |> Result.map (fun _ -> m)))
                (ret reps)

        if List.isEmpty keys then
            ret src
        else
            let resolved =
                keys
                |> List.map (fun k ->
                    let sk = k
                    let expr = sk.Expression
                    let dir = sk.Direction

                    match Schema.tryAsColumnRef expr with
                    | None ->
                        err (
                            sprintf "ORDER BY には列の直接参照だけを書けます（%s）" (Expr.toDisplayString expr)
                            |> DbError.name
                        )
                    | Some c ->
                        let bare = { Qualifier = None; Name = c.Name }

                        match Schema.resolve schema c with
                        | Ok i -> Ok(i, dir)
                        | Error _ -> Schema.resolve schema bare |> Result.map (fun i -> i, dir))
                |> all

            result {
                let! plan = resolved
                return! ExtSort.sortRows ExtSort.DefaultBudgetBytes Map.empty (check plan) (compareWith plan) src
            }

    /// 重複除去。
    /// 保持する順序は最初に現れた順（SQL の DISTINCT と同じ）。
    /// 既出のキーを全部覚える必要がある。
    /// 「直前のキーだけ」で比較するのは `DISTINCT` ではなく `DISTINCT ON` にしかならない
    /// （`2, 1, 2` で 2 が 2 回目に出るため、直前比較では除去できない）。
    /// バジェットを指定できる実体は `distinctWithin`。
    /// <remarks>
    /// 既出行を `SqlValue array` のまま抱えるとボックス付きオブジェクトが山盛りになるので、
    /// `ValueCodec` で扁平なバイト列にしてから集合に入れる（同じ値なら同じ列になる）。
    /// メモリはユニーク行数に比例するが、無制限ではない。
    /// バジェットを超えると OOM でプロセスごと落とす代わりに `CapacityError` で止める。
    /// </remarks>
    let distinctWithin (budgetBytes: int64) (src: Op) =
        let encode (row: SqlValue array) =
            ValueCodec.writeValues row Writer.Empty |> Writer.toArray

        let rec skipSeen (seen: System.Collections.Generic.HashSet<byte array>) (used: int64) (cur: Op) =
            Op.pull cur (fun row nxt ->
                let key = encode row

                if seen.Contains key then
                    Op.next (skipSeen seen used nxt)
                elif used > budgetBytes then
                    err (
                        DbError.capacity (
                            sprintf "SELECT DISTINCT のユニーク行が多すぎます（概算 %d バイトの上限）。条件で絞るか、集約で代替してください" budgetBytes
                        )
                    )
                else
                    seen.Add key |> ignore
                    ret (Some(row, skipSeen seen (used + int64 (rowBytes row)) nxt)))

        skipSeen (System.Collections.Generic.HashSet<byte array> HashIdentity.Structural) 0L src

    /// 重複除去（既定バジェット）。
    let distinct (src: Op) = distinctWithin DistinctBudgetBytes src

    /// 式中に現れる集約呼び出しの一覧（実行順に揃える）。
    let aggCalls (exprs: Expr list) =
        let isStarArg (x: Expr) =
            match x with
            | Expr.Star -> true
            | _ -> false

        let collectAggs (e: Expr) (acc: (AggFunc * bool * bool * Expr) list) =
            Expr.fold
                (fun a node ->
                    match node with
                    | Expr.AggCall(f, distinct, x) -> (f, isStarArg x, distinct, x) :: a
                    | _ -> a)
                acc
                e

        exprs |> List.fold (fun acc e -> collectAggs e acc) [] |> List.rev

    /// 集約呼び出しの識別キー（`COUNT(age)` のような表示文字列）。
    let private aggKey (f: AggFunc) (distinct: bool) (arg: Expr) =
        if distinct then
            sprintf "%s(DISTINCT %s)" f.Name (Expr.toDisplayString arg)
        else
            sprintf "%s(%s)" f.Name (Expr.toDisplayString arg)

    /// 集約後の行のスロット番号表。
    /// GROUP BY 式が先頭に、集約呼び出しがその後に並ぶ（`Ops.aggregate` の出力順）。
    /// 行に依存しないので 1クエリにつき 1回だけ作る。
    let private aggOffsets (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) =
        let groupSlots =
            groupBy |> List.mapi (fun i e -> Expr.toDisplayString e, i) |> Map.ofList

        let aggSlots =
            aggs
            |> List.mapi (fun i (f, _, distinct, arg) -> aggKey f distinct arg, List.length groupBy + i)
            |> Map.ofList

        Map.fold (fun acc k v -> Map.add k v acc) groupSlots aggSlots

    /// 集約呼び出しの式を、計算済みの値（定数）へ置き換える。
    /// HAVING と SELECT 項目の両方で使う。
    /// `offsets` は `aggOffsets` の結果。
    /// 行に依存するのは `row` だけなので offsets は呼び出し側で 1回だけ作る
    /// （`project` / `havingFilter` は出力 1行ごとに呼ぶため、
    /// ここで作ると行数ぶんだけマップを組み直すことになる）。
    /// 組み立ては `Expr.rewrite`（末尾再帰の評価機）に任せるので、式の深さに依存しない。
    let private substituteAggs (offsets: Map<string, int>) (e: Expr) (row: SqlValue array) =
        let slot key =
            match offsets |> Map.tryFind key with
            | Some i when i >= 0 && i < row.Length -> Some(Expr.Literal(Literal.ofValue row.[i]))
            | _ -> None

        let rewriteLeaf (x: Expr) =
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

    /// HAVING を張る。
    /// 集約後の行は位置のみの意味を持つので Schema.empty で評価する。
    /// offsets はクロージャを 1回作るときにだけ組み立て、行ごとに持ち越さない。
    let havingFilter (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (predicate: Expr) =
        let offsets = aggOffsets groupBy aggs
        fun row -> substituteAggs offsets predicate row >>= Eval.matches Schema.empty row

    /// 集約の 1 項目分の_accumulator。
    /// SUM / AVG は decimal で累積し、最後に型を決める（丸め誤差を避ける）。
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
    /// 入力行は**抱え込まない**。グループごとの集約値だけを更新していくので、
    /// メモリは「グループ数 × アクュムレータ」に比例し、入力行数には比例しない
    /// （従来は全行を `ResizeArray` に保持していた）。
    /// グループ数にも上限を置き、超過は `CapacityError` で止める。
    /// 出力順は従来どおり「最初に現れたグループ」の順。
    /// </remarks>
    let aggregate (schema: Schema) (src: Op) (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) =
        let groupBudget = 4_000_000

        let keyOfRow (row: SqlValue array) =
            groupBy
            |> List.map (fun e -> Eval.eval schema row e)
            |> all
            |> Result.map List.toArray

        let emptyAcc (kind: AggFunc) =
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

        let addDecimal (a: decimal) (b: decimal) =
            let maxD = System.Decimal.MaxValue
            let minD = System.Decimal.MinValue

            if a > 0M && b > 0M && a > maxD - b || a < 0M && b < 0M && a < minD - b then
                err (DbError.constraintViolation "SUM の結果が DECIMAL の範囲を超えました")
            else
                ret (a + b)

        let keepExtreme
            (v: SqlValue)
            (wins: int -> bool)
            (bumped: Acc)
            (current: SqlValue option)
            (set: Acc -> SqlValue -> Acc)
            (seen: Set<SqlValue>)
            =
            match current with
            | None -> ret (set { bumped with Seen = seen } v)
            | Some cur ->
                SqlValue.compareValues v cur
                |> Result.map (fun c ->
                    if wins c then
                        set { bumped with Seen = seen } v
                    else
                        { bumped with Seen = seen })

        let addValue (acc: Acc) (bump: Acc) (v: SqlValue) (newSeen: Set<SqlValue>) =
            match acc.Kind with
            | AggFunc.Count -> ret { bump with Seen = newSeen }
            | AggFunc.Sum
            | AggFunc.Avg ->
                match v with
                | SqlValue.Real f ->
                    ret
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
                            ret
                                { bump with
                                    SumReal = acc.SumReal + float d
                                    Seen = newSeen
                                }
                        else
                            addDecimal acc.SumDecimal d
                            |> Result.map (fun sum ->
                                { bump with
                                    SumDecimal = sum
                                    Seen = newSeen
                                })
                    | None -> err (DbError.internalError "数値の変換に失敗しました（内部不整合）")
                | _ ->
                    err (
                        sprintf "%s は数値だけを受け付けますが %s です" acc.Kind.Name (SqlLiteral.toSqlLiteral v)
                        |> DbError.typeError
                    )
            | AggFunc.Min -> keepExtreme v (fun c -> c < 0) bump acc.Min (fun b x -> { b with Min = Some x }) newSeen
            | AggFunc.Max -> keepExtreme v (fun c -> c > 0) bump acc.Max (fun b x -> { b with Max = Some x }) newSeen

        let addAcc (acc: Acc) (value: SqlValue option) (isStar: bool) (distinct: bool) =
            let alreadySeen =
                distinct
                && match value with
                   | Some v when not (SqlValue.isNull v) -> Set.contains v acc.Seen
                   | _ -> false

            if alreadySeen then
                ret acc
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
                | None -> ret { acc with Seen = newSeen }
                | Some v -> addValue acc bump v newSeen

        let finishAcc (acc: Acc) =
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

        let add (star: bool) (distinct: bool) (arg: Expr) (acc: Acc) (row: SqlValue array) =
            if star then
                addAcc acc None true distinct
            else
                Eval.eval schema row arg >>= fun v -> addAcc acc (Some v) false distinct

        let newBucket () =
            aggs |> List.map (fun (f, _, _, _) -> emptyAcc f) |> ResizeArray

        let addToBucket (bucket: ResizeArray<Acc>) (row: SqlValue array) =
            let rec step (i: int) =
                if i >= aggs.Length then
                    ret ()
                else
                    let _, star, distinct, arg = aggs.[i]

                    match add star distinct arg bucket.[i] row with
                    | Error e -> Error e
                    | Ok acc ->
                        bucket.[i] <- acc
                        step (i + 1)

            step 0

        let rowOf (key: SqlValue array) (bucket: ResizeArray<Acc>) =
            let av = bucket |> Seq.map finishAcc |> Seq.toArray
            Array.append key av

        let consume
            (buckets: System.Collections.Generic.Dictionary<SqlValue array, ResizeArray<Acc>>)
            (order: ResizeArray<SqlValue array>)
            (cur: Op)
            =
            let rec go (cur: Op) =
                match Op.next cur with
                | Error e -> Error e
                | Ok None -> ret ()
                | Ok(Some(row, nxt)) ->
                    match keyOfRow row with
                    | Error e -> Error e
                    | Ok k ->
                        match buckets.TryGetValue k with
                        | true, bucket ->
                            match addToBucket bucket row with
                            | Error e -> Error e
                            | Ok() -> go nxt
                        | _ ->
                            if order.Count >= groupBudget then
                                err (
                                    DbError.capacity (
                                        sprintf "GROUP BY のグループ数が多すぎます（上限 %d）。条件で絞るか、サンプリングしてください" groupBudget
                                    )
                                )
                            else
                                let bucket = newBucket ()

                                match addToBucket bucket row with
                                | Error e -> Error e
                                | Ok() ->
                                    buckets.[k] <- bucket
                                    order.Add k
                                    go nxt

            go cur

        let emitGroups
            (buckets: System.Collections.Generic.Dictionary<SqlValue array, ResizeArray<Acc>>)
            (order: ResizeArray<SqlValue array>)
            =
            let rec from (i: int) =
                if i >= order.Count then
                    Op.Done
                else
                    Op.Compute(fun () ->
                        let key = order.[i]
                        ret (Some(rowOf key buckets.[key], from (i + 1))))

            from 0

        let buckets =
            System.Collections.Generic.Dictionary<SqlValue array, ResizeArray<Acc>>(
                HashIdentity.Structural: System.Collections.Generic.IEqualityComparer<SqlValue array>
            )

        let order = ResizeArray<SqlValue array>()

        result {
            do! consume buckets order src

            if order.Count = 0 && List.isEmpty groupBy then
                let vals =
                    aggs |> List.map (fun (f, _, _, _) -> finishAcc (emptyAcc f)) |> List.toArray

                return Op.ofArray [| vals |]
            else
                return emitGroups buckets order
        }

    /// nested loop 結合の内側ループ（1つの左行 × 右の全行）を回す共通処理。
    /// onExhausted matched は右を最後まで試し終えたときの Op。
    /// onError error matched は ON 句の評価が失敗したときの Op。
    /// 失敗はエラーとして流す（黙って行を落とすと気づきにくい誤答になる）。
    /// matched は「この左行で既に一致を出したか」。
    /// INNER はどちらも matched を無視し、LEFT だけが NULL 拡張と継続を使い分ける。
    let private joinLoop
        (leftSchema: Schema)
        (rightSchema: Schema)
        (on: Expr)
        (onExhausted: bool -> Op)
        (onError: DbError -> bool -> Op)
        (rightRows: SqlValue array array)
        (lrow: SqlValue array)
        =
        let joinPred (leftSchema: Schema) (rightSchema: Schema) (on: Expr) (l: SqlValue array) (r: SqlValue array) =
            Eval.matches (Schema.concat leftSchema rightSchema) (Array.append l r) on

        let rec loop (matched: bool) (i: int) =
            if i >= rightRows.Length then
                onExhausted matched
            else
                let rrow = rightRows.[i]

                match joinPred leftSchema rightSchema on lrow rrow with
                | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, loop true (i + 1))))
                | Ok false -> Op.Compute(fun () -> Op.next (loop matched (i + 1)))
                | Error e -> onError e matched

        loop false 0

    /// INNER JOIN（nested loop）。
    /// 右側は内側ループごとに先頭から引き直す。
    /// 一致が無い左行は捨て、ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let innerJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        let rec joinRows rightRows (cur: Op) =
            Op.pull cur (fun lrow lnext ->
                Op.next (
                    joinLoop
                        leftSchema
                        rightSchema
                        on
                        (fun _ -> joinRows rightRows lnext)
                        (fun e _ -> Op.Compute(fun () -> err e))
                        rightRows
                        lrow
                ))

        Op.toArray right |> Result.map (fun rightRows -> joinRows rightRows left)

    /// LEFT JOIN。
    /// 左の行に右が 1行も無ければ NULL で埋める。
    /// ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let leftJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        let rec joinRows rightRows width (cur: Op) =
            Op.pull cur (fun lrow lnext ->
                let onExhausted matched =
                    if matched then
                        joinRows rightRows width lnext
                    else
                        Op.Step(Array.append lrow (Array.create width SqlValue.Null), joinRows rightRows width lnext)

                let onError e _ = Op.Compute(fun () -> err e)
                Op.next (joinLoop leftSchema rightSchema on onExhausted onError rightRows lrow))

        Op.toArray right
        |> Result.map (fun rightRows -> joinRows rightRows rightSchema.Columns.Length left)

    /// 等値結合のキーが NULL を含むか（SQL では NULL は一致しない）。
    let private hasNullKey (row: SqlValue array) (indices: int array) =
        indices |> Array.exists (fun i -> SqlValue.isNull row.[i])

    let private keyOf (row: SqlValue array) (indices: int array) = indices |> Array.map (fun i -> row.[i])

    /// ハッシュ結合 / インデックス入れ子ループの共通ループ。
    /// lookupRows は左キーから右行の配列を返す（見つからなければ空配列）。
    /// <remarks>
    /// Op.Compute の継続内の相互再帰は「次の行を要求されたとき」に 1段ずつ進むので、スタックは積み上がらない。
    /// TailAudit はラムダ内の呼び出しを非末尾として報告するためベースラインに登録している。
    /// </remarks>
    let private joinProbe
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (leftKeys: int array)
        (lookupRows: SqlValue array -> Result<SqlValue array array, DbError>)
        (residual: Expr option)
        (isLeft: bool)
        =
        let residualMatches
            (leftSchema: Schema)
            (rightSchema: Schema)
            (residual: Expr option)
            (lrow: SqlValue array)
            (rrow: SqlValue array)
            =
            match residual with
            | None -> ret true
            | Some p -> Eval.matches (Schema.concat leftSchema rightSchema) (Array.append lrow rrow) p

        let rightWidth = rightSchema.Columns.Length

        let padded (lrow: SqlValue array) =
            Array.append lrow (Array.create rightWidth SqlValue.Null)

        let rec probe (cur: Op) =
            Op.pull cur (fun lrow lnext ->
                if hasNullKey lrow leftKeys then
                    if isLeft then
                        Op.next (Op.Compute(fun () -> ret (Some(padded lrow, probe lnext))))
                    else
                        Op.next (probe lnext)
                else
                    match lookupRows (keyOf lrow leftKeys) with
                    | Error e -> err e
                    | Ok rows -> Op.next (emit lrow lnext rows 0 false))

        and emit (lrow: SqlValue array) (lnext: Op) (rows: SqlValue array array) (i: int) (matched: bool) =
            if i >= rows.Length then
                if isLeft && not matched then
                    Op.Compute(fun () -> ret (Some(padded lrow, probe lnext)))
                else
                    probe lnext
            else
                let rrow = rows.[i]

                match residualMatches leftSchema rightSchema residual lrow rrow with
                | Error e -> Op.Compute(fun () -> err e)
                | Ok false -> Op.Compute(fun () -> Op.next (emit lrow lnext rows (i + 1) matched))
                | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, emit lrow lnext rows (i + 1) true)))

        probe left

    /// 右側のハッシュ表を作る（NULL キーは登録しない）。
    let private buildHashTable (rightRows: SqlValue array array) (rightKeys: int array) =
        let table =
            System.Collections.Generic.Dictionary<SqlValue array, ResizeArray<SqlValue array>> HashIdentity.Structural

        for rrow in rightRows do
            if not (hasNullKey rrow rightKeys) then
                let key = keyOf rrow rightKeys

                match table.TryGetValue key with
                | true, bucket -> bucket.Add rrow
                | _ -> table.[key] <- ResizeArray<SqlValue array>([ rrow ])

        table

    let private lookupHash
        (table: System.Collections.Generic.Dictionary<SqlValue array, ResizeArray<SqlValue array>>)
        (key: SqlValue array)
        =
        match table.TryGetValue key with
        | true, bucket -> ret (bucket.ToArray())
        | _ -> ret [||]

    /// INNER JOIN（ハッシュ結合）。右側をハッシュ表にする。
    let innerJoinHash
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (right: Op)
        (leftKeys: int array)
        (rightKeys: int array)
        (residual: Expr option)
        =
        Op.toArray right
        |> Result.map (fun rightRows ->
            let table = buildHashTable rightRows rightKeys
            joinProbe leftSchema left rightSchema leftKeys (lookupHash table) residual false)

    /// LEFT JOIN（ハッシュ結合）。
    let leftJoinHash
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (right: Op)
        (leftKeys: int array)
        (rightKeys: int array)
        (residual: Expr option)
        =
        Op.toArray right
        |> Result.map (fun rightRows ->
            let table = buildHashTable rightRows rightKeys
            joinProbe leftSchema left rightSchema leftKeys (lookupHash table) residual true)

    /// INNER JOIN（インデックス入れ子ループ）。
    let innerJoinIndexed
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (leftKeys: int array)
        (lookup: SqlValue array -> Result<Op, DbError>)
        (residual: Expr option)
        =
        joinProbe leftSchema left rightSchema leftKeys (fun key -> lookup key |> Result.bind Op.toArray) residual false

    /// LEFT JOIN（インデックス入れ子ループ）。
    let leftJoinIndexed
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (leftKeys: int array)
        (lookup: SqlValue array -> Result<Op, DbError>)
        (residual: Expr option)
        =
        joinProbe leftSchema left rightSchema leftKeys (fun key -> lookup key |> Result.bind Op.toArray) residual true

    /// 射影。
    /// `SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は対象テーブルの全列に展開する。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    let project (schema: Schema) (groupBy: Expr list) (src: Op) (items: SelectItem list) =
        let sameRef (c: OutputColumn) =
            let col (qualifier: string option) (name: string) = { Qualifier = qualifier; Name = name }
            col c.Qualifier c.Name

        let aggCallsOf (items: SelectItem list) =
            aggCalls (items |> List.map (fun i -> i.Expression))

        if List.isEmpty items then
            src
        else
            let allExprs =
                items
                |> List.map (fun item ->
                    let exprs =
                        match Schema.expandStar schema item.Expression with
                        | Some(_, columns) -> columns |> List.map (fun c -> sameRef c |> Expr.Column)
                        | None -> [ item.Expression ]

                    item, exprs)
                |> List.collect (fun (_, exprs) -> exprs)

            let foundAggs = aggCallsOf items

            if List.isEmpty foundAggs then
                Op.map (fun row -> allExprs |> List.map (Eval.eval schema row) |> all |> Result.map List.toArray) src
            else
                let offsets = aggOffsets groupBy foundAggs

                Op.map
                    (fun row ->
                        allExprs
                        |> List.map (fun e -> substituteAggs offsets e row >>= Eval.eval Schema.empty row)
                        |> all
                        |> Result.map List.toArray)
                    src
