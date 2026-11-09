namespace RykDb.Core

module internal Ops =
    /// DISTINCT が覚えられるユニーク行の概算上限（バイト）。
    /// 超えたら `CapacityError` で止める（OOM でプロセスごと落とさないため）。
    let private DistinctBudgetBytes = 256L * 1024L * 1024L

    /// 述語で弾く。
    let filter (schema: Schema) (predicate: Expr) =
        Op.filter (fun row -> Eval.matches schema row predicate)

    /// ソート。
    /// ORDER BY のキーを列位置へ解決できるならそのまま使い、
    /// 式が含まれる場合はキーを評価して行末へ足してからソートする。
    /// `*` / `t.*` はORDER BY には書けない。
    let sortBy (schema: Schema) (keys: RykDb.Core.SortKey list) (src: Op) =
        let compareWith plan (a: SqlValue array) (b: SqlValue array) =
            let rec compareKeys =
                function
                | [] -> 0
                | (i, dir) :: tail ->
                    match SqlValue.compareValuesForSort a.[i] b.[i] with
                    | Error _ -> compareKeys tail
                    | Ok c ->
                        if c = 0 then
                            compareKeys tail
                        else
                            match dir with
                            | SortDirection.Ascending -> c
                            | SortDirection.Descending -> -c

            compareKeys plan

        let check plan reps (row: SqlValue array) =
            plan
            |> List.fold
                (fun (acc: Result<Map<int, SqlValue>, DbError>) (i, _) ->
                    acc
                    >>= fun m ->
                        let v = row.[i]

                        if SqlValue.isNull v then
                            ret m
                        else
                            match Map.tryFind i m with
                            | None -> ret (Map.add i v m)
                            | Some representative -> SqlValue.compareValues representative v |> Result.map (fun _ -> m))
                (ret reps)

        let resolveColumn (k: SortKey) =
            match Schema.tryAsColumnRef k.Expression with
            | None -> None
            | Some c ->
                let bare = { Qualifier = None; Name = c.Name }

                match Schema.resolve schema c with
                | Ok i -> Some(i, k.Direction)
                | Error _ ->
                    match Schema.resolve schema bare with
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
                            exprs
                            |> List.map (Eval.eval schema row)
                            |> all
                            |> Result.map (fun keyValues -> Array.append row (Array.ofList keyValues)))
                        src

                result {
                    let! sorted =
                        ExtSort.sortRows ExtSort.DefaultBudgetBytes Map.empty (check plan) (compareWith plan) projected

                    return Op.map (fun row -> ret (Array.sub row 0 width)) sorted
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
            ValueCodec.writeValues row Writer.Empty |> Writer.toArray

        let seen = System.Collections.Generic.HashSet<byte array> HashIdentity.Structural

        let rec skipSeen used =
            Op.pull (fun row nxt ->
                let key = encode row

                if seen.Contains key then
                    Op.next (skipSeen used nxt)
                elif used > DistinctBudgetBytes then
                    sprintf "SELECT DISTINCT のユニーク行が多すぎます（概算 %d バイトの上限）。条件で絞るか、集約で代替してください" DistinctBudgetBytes
                    |> DbError.capacity
                    |> err
                else
                    seen.Add key |> ignore
                    ret (Some(row, skipSeen (used + int64 (ValueCodec.managedRowBytes row)) nxt)))

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

        fun row ->
            substituteAggs offsets wholeOffsets row predicate
            >>= Eval.matches Schema.empty row

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

        let keyOfRow row =
            groupBy
            |> List.map (fun e -> Eval.eval schema row e)
            |> all
            |> Result.map List.toArray

        let addDecimal a b =
            let maxD = System.Decimal.MaxValue
            let minD = System.Decimal.MinValue

            if a > 0M && b > 0M && a > maxD - b || a < 0M && b < 0M && a < minD - b then
                err (DbError.constraintViolation "SUM の結果が DECIMAL の範囲を超えました")
            else
                ret (a + b)

        let keepExtreme v wins bumped set seen =
            function
            | None -> ret (set { bumped with Seen = seen } v)
            | Some cur ->
                SqlValue.compareValues v cur
                |> Result.map (fun c ->
                    if wins c then
                        set { bumped with Seen = seen } v
                    else
                        { bumped with Seen = seen })

        let addValue acc bump v newSeen =
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
            | AggFunc.Min -> keepExtreme v (fun c -> c < 0) bump (fun b x -> { b with Min = Some x }) newSeen acc.Min
            | AggFunc.Max -> keepExtreme v (fun c -> c > 0) bump (fun b x -> { b with Max = Some x }) newSeen acc.Max

        let addAcc acc value isStar distinct =
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

        let add star distinct arg acc row =
            if star then
                addAcc acc None true distinct
            else
                Eval.eval schema row arg >>= fun v -> addAcc acc (Some v) false distinct

        let addToBucket (bucket: ResizeArray<Acc>) row =
            let rec step i =
                if i >= aggs.Length then
                    ret ()
                else
                    let _, star, distinct, arg = aggs.[i]

                    match add star distinct arg bucket.[i] row with
                    | Error e -> err e
                    | Ok acc ->
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
            | Error e -> err e
            | Ok None -> ret ()
            | Ok(Some(row, nxt)) ->
                match keyOfRow row with
                | Error e -> err e
                | Ok k ->
                    match buckets.TryGetValue k with
                    | true, bucket ->
                        match addToBucket bucket row with
                        | Error e -> err e
                        | Ok() -> consume nxt
                    | _ ->
                        if order.Count >= groupBudget then
                            sprintf "GROUP BY のグループ数が多すぎます（上限 %d）。条件で絞るか、サンプリングしてください" groupBudget
                            |> DbError.capacity
                            |> err
                        else
                            let bucket = newBucket ()

                            match addToBucket bucket row with
                            | Error e -> err e
                            | Ok() ->
                                buckets.[k] <- bucket
                                order.Add k
                                consume nxt

        let rec emitGroups i =
            if i >= order.Count then
                Op.Done
            else
                Op.Compute(fun () ->
                    let key = order.[i]
                    ret (Some(rowOf buckets.[key] key, emitGroups (i + 1))))

        result {
            do! consume src

            if order.Count = 0 && List.isEmpty groupBy then
                let vals =
                    aggs |> List.map (fun (f, _, _, _) -> finishAcc (emptyAcc f)) |> List.toArray

                return Op.ofArray [| vals |]
            else
                return emitGroups 0
        }

    /// JOIN の右側を保持する上限（管理メモリの概算バイト）。
    /// 入れ子ループ / ハッシュ結合は右側の全行を保持するので、
    /// OOM でプロセスごと落とさないための上限を置く
    /// （DISTINCT / transaction と同じ考え方）。
    let private JoinBudgetBytes = 256L * 1024L * 1024L

    /// 右側の行を上限付きで materialize する（JOIN が右側の全行を保持するため）。
    /// 1行の管理メモリに、リストの cons（24 バイト）と最終配列のスロット（8 バイト）を足して見積もる。
    /// ハッシュ結合は表（バケット）も作るが、行の配列は表を作る後に捨てられる。
    let private toArrayWithBudget (budget: int64) =
        let rec go used acc cur =
            match Op.next cur with
            | Error e -> err e
            | Ok None -> ret (List.rev acc |> List.toArray)
            | Ok(Some(row, nxt)) ->
                let used = used + int64 (ValueCodec.managedRowBytes row) + 32L

                if used > budget then
                    sprintf "JOIN の右側の行が多すぎます（概算 %d バイトの上限）。条件で絞るか、インデックスで絞ってください" budget
                    |> DbError.capacity
                    |> err
                else
                    go used (row :: acc) nxt

        go 0L []

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
        let joinPred leftSchema rightSchema l r =
            Eval.matches (Schema.concat leftSchema rightSchema) (Array.append l r)

        let rec loop matched i =
            if i >= rightRows.Length then
                onExhausted matched
            else
                let rrow = rightRows.[i]

                match joinPred leftSchema rightSchema lrow rrow on with
                | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, loop true (i + 1))))
                | Ok false -> Op.Compute(fun () -> Op.next (loop matched (i + 1)))
                | Error e -> onError e matched

        loop false 0

    /// INNER JOIN（nested loop）。
    /// 右側は内側ループごとに先頭から引き直す。
    /// 一致が無い左行は捨て、ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let innerJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        let rec joinRows cur rightRows =
            Op.pull
                (fun lrow lnext ->
                    Op.next (
                        joinLoop
                            leftSchema
                            rightSchema
                            on
                            (fun _ -> joinRows lnext rightRows)
                            (fun e _ -> Op.Compute(fun () -> err e))
                            rightRows
                            lrow
                    ))
                cur

        toArrayWithBudget JoinBudgetBytes right |> Result.map (joinRows left)

    /// LEFT JOIN。
    /// 左の行に右が 1行も無ければ NULL で埋める。
    /// ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let leftJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        let rec joinRows width cur rightRows =
            Op.pull
                (fun lrow lnext ->
                    let onExhausted matched =
                        if matched then
                            joinRows width lnext rightRows
                        else
                            Op.Step(
                                Array.append lrow (Array.create width SqlValue.Null),
                                joinRows width lnext rightRows
                            )

                    let onError e _ = Op.Compute(fun () -> err e)
                    Op.next (joinLoop leftSchema rightSchema on onExhausted onError rightRows lrow))
                cur

        toArrayWithBudget JoinBudgetBytes right
        |> Result.map (joinRows rightSchema.Columns.Length left)

    /// 等値結合のキーが NULL を含むか（SQL では NULL は一致しない）。
    let private hasNullKey (row: SqlValue array) =
        Array.exists (fun i -> SqlValue.isNull row.[i])

    let private keyOf (row: SqlValue array) = Array.map (fun i -> row.[i])

    /// 等値キー以外の ON 句（residual）を評価する。
    /// 結合後の行（左 + 右）を一時配列に連結してから評価する。
    let private residualMatches leftSchema rightSchema lrow rrow =
        function
        | None -> ret true
        | Some p -> Eval.matches (Schema.concat leftSchema rightSchema) (Array.append lrow rrow) p

    /// ハッシュ結合 / インデックス入れ子ループの共通ループ。
    /// lookupRows は左キーから右行のリストを返す（見つからなければ空）。
    /// <remarks>
    /// Op.Compute の継続内の相互再帰は「次の行を要求されたとき」に 1段ずつ進むので、スタックは積み上がらない。
    /// TailAudit はラムダ内の呼び出しを非末尾として報告するためベースラインに登録している。
    /// </remarks>
    let private joinProbe
        (isLeft: bool)
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (lookupRows: SqlValue array -> Result<ResizeArray<SqlValue array>, DbError>)
        (residual: Expr option)
        =
        let rightWidth = rightSchema.Columns.Length

        let padded lrow =
            Array.append lrow (Array.create rightWidth SqlValue.Null)

        let rec probe cur =
            Op.pull
                (fun lrow lnext ->
                    if hasNullKey lrow leftKeys then
                        if isLeft then
                            Op.next (Op.Compute(fun () -> ret (Some(padded lrow, probe lnext))))
                        else
                            Op.next (probe lnext)
                    else
                        match lookupRows (keyOf lrow leftKeys) with
                        | Error e -> err e
                        | Ok rows -> Op.next (emit lrow lnext rows 0 false))
                cur

        and emit lrow lnext rows i matched =
            if i >= rows.Count then
                if isLeft && not matched then
                    Op.Compute(fun () -> ret (Some(padded lrow, probe lnext)))
                else
                    probe lnext
            else
                let rrow = rows.[i]

                match residualMatches leftSchema rightSchema lrow rrow residual with
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

    /// 見つからないときの空リスト。
    /// `joinProbe` は `rows` を読み取り専用でしか使わないので共有してよい
    /// （モジュールスコープの値は 1回だけ評価される。`Ops.distinct` の教訓参照）。
    let private emptyRows = ResizeArray<SqlValue array>()

    let private lookupHash
        (table: System.Collections.Generic.Dictionary<SqlValue array, ResizeArray<SqlValue array>>)
        (key: SqlValue array)
        =
        match table.TryGetValue key with
        | true, bucket -> ret bucket
        | _ -> ret emptyRows

    /// INNER JOIN（ハッシュ結合）。右側をハッシュ表にする。
    let innerJoinHash
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        =
        toArrayWithBudget JoinBudgetBytes right
        |> Result.map (fun rightRows ->
            let table = buildHashTable rightRows rightKeys
            joinProbe false leftSchema left leftKeys rightSchema (lookupHash table) residual)

    /// LEFT JOIN（ハッシュ結合）。
    let leftJoinHash
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        =
        toArrayWithBudget JoinBudgetBytes right
        |> Result.map (fun rightRows ->
            let table = buildHashTable rightRows rightKeys
            joinProbe true leftSchema left leftKeys rightSchema (lookupHash table) residual)

    /// マージ結合の入力側の状態。
    /// `MergeStart` はまだ 1行も取り出していない入力、`MergeAt` は現在行と続き。
    type private MergeSide =
        | MergeStart of Op
        | MergeAt of SqlValue array * Op
        | MergeDone

    /// 次の 1行を先読みした状態にする。
    let private pullMergeSide (side: MergeSide) =
        let fromOp op =
            match Op.next op with
            | Error e -> err e
            | Ok None -> ret MergeDone
            | Ok(Some(row, rest)) -> ret (MergeAt(row, rest))

        match side with
        | MergeStart op
        | MergeAt(_, op) -> fromOp op
        | MergeDone -> ret MergeDone

    /// まだ 1行も取り出していなければ、先頭行を先読みする。
    let private ensureMergeSide (side: MergeSide) =
        match side with
        | MergeStart _ -> pullMergeSide side
        | _ -> ret side

    /// 結合キーの比較（NULL は呼び出し側で除く）。
    let rec private compareKeyVectors (i: int) (a: SqlValue array) (b: SqlValue array) =
        if i >= a.Length then
            ret 0
        else
            match SqlValue.compareValues a.[i] b.[i] with
            | Error e -> err e
            | Ok 0 -> compareKeyVectors (i + 1) a b
            | Ok c -> ret c

    /// 等値キーの行をまとめて取り出す（入力はキー昇順）。
    /// 返り値は (グループの行, グループの次の行を含む状態)。
    /// 1グループのメモリは `JoinBudgetBytes` で制限する。
    let private takeMergeGroup (keys: int array) (side: MergeSide) =
        match side with
        | MergeStart _
        | MergeDone -> ret ([||], side)
        | MergeAt(first, _) ->
            let groupKey = keyOf first keys

            let rec go used acc current =
                match pullMergeSide current with
                | Error e -> err e
                | Ok MergeDone -> ret (List.rev acc |> List.toArray, MergeDone)
                | Ok(MergeStart _) -> DbError.internalError "merge join: 入力状態が不正です" |> err
                | Ok(MergeAt(row, _) as next) ->
                    match compareKeyVectors 0 (keyOf row keys) groupKey with
                    | Error e -> err e
                    | Ok 0 ->
                        let used = used + int64 (ValueCodec.managedRowBytes row) + 32L

                        if used > JoinBudgetBytes then
                            sprintf "JOIN の同じキーの行が多すぎます（概算 %d バイトの上限）" JoinBudgetBytes
                            |> DbError.capacity
                            |> err
                        else
                            go used (row :: acc) next
                    | Ok _ -> ret (List.rev acc |> List.toArray, next)

            let firstBytes = int64 (ValueCodec.managedRowBytes first) + 32L

            if firstBytes > JoinBudgetBytes then
                sprintf "JOIN の同じキーの行が多すぎます（概算 %d バイトの上限）" JoinBudgetBytes
                |> DbError.capacity
                |> err
            else
                go firstBytes [ first ] side

    /// マージ結合。両入力は結合キーで昇順に並んでいる必要がある
    /// （`Analyzer.chooseJoinAlgorithm` が走査の並び順を確認して選ぶ）。
    /// 右側を materialize せず、両側を 1行ずつ流す。
    /// <remarks>
    /// `Op.Compute` の継続内で `loop` / `emitGroup` を呼ぶので、TailAudit はラムダ内の
    /// 呼び出しを非末尾として報告する（`joinProbe` の `probe/emit` と同じ、1行ごとに 1段だけ進む）。
    /// </remarks>
    let private mergeJoin
        (isLeft: bool)
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        =
        let rightWidth = rightSchema.Columns.Length

        let padded lrow =
            Array.append lrow (Array.create rightWidth SqlValue.Null)

        let rec loop lside rside =
            match lside, rside with
            | MergeStart _, _ ->
                Op.Compute(fun () ->
                    match ensureMergeSide lside with
                    | Error e -> err e
                    | Ok lnext -> Op.next (loop lnext rside))
            | _, MergeStart _ ->
                Op.Compute(fun () ->
                    match ensureMergeSide rside with
                    | Error e -> err e
                    | Ok rnext -> Op.next (loop lside rnext))
            | MergeDone, _ -> Op.empty
            | _, MergeDone ->
                if not isLeft then
                    Op.empty
                else
                    match lside with
                    | MergeAt(lrow, _) ->
                        Op.Compute(fun () ->
                            match pullMergeSide lside with
                            | Error e -> err e
                            | Ok lnext -> ret (Some(padded lrow, loop lnext MergeDone)))
                    | _ -> Op.empty
            | MergeAt(lrow, _), MergeAt(rrow, _) ->
                if hasNullKey lrow leftKeys then
                    Op.Compute(fun () ->
                        match pullMergeSide lside with
                        | Error e -> err e
                        | Ok lnext ->
                            if isLeft then
                                ret (Some(padded lrow, loop lnext rside))
                            else
                                Op.next (loop lnext rside))
                elif hasNullKey rrow rightKeys then
                    Op.Compute(fun () ->
                        match pullMergeSide rside with
                        | Error e -> err e
                        | Ok rnext -> Op.next (loop lside rnext))
                else
                    match compareKeyVectors 0 (keyOf lrow leftKeys) (keyOf rrow rightKeys) with
                    | Error e -> Op.Compute(fun () -> err e)
                    | Ok c when c < 0 ->
                        Op.Compute(fun () ->
                            match pullMergeSide lside with
                            | Error e -> err e
                            | Ok lnext ->
                                if isLeft then
                                    ret (Some(padded lrow, loop lnext rside))
                                else
                                    Op.next (loop lnext rside))
                    | Ok c when c > 0 ->
                        Op.Compute(fun () ->
                            match pullMergeSide rside with
                            | Error e -> err e
                            | Ok rnext -> Op.next (loop lside rnext))
                    | Ok _ ->
                        match takeMergeGroup leftKeys lside with
                        | Error e -> Op.Compute(fun () -> err e)
                        | Ok(lgroup, lnext) ->
                            match takeMergeGroup rightKeys rside with
                            | Error e -> Op.Compute(fun () -> err e)
                            | Ok(rgroup, rnext) -> emitGroup lgroup rgroup lnext rnext 0 0 false

        and emitGroup lgroup rgroup lnext rnext i j matchedThisRow =
            if i >= lgroup.Length then
                loop lnext rnext
            elif j >= rgroup.Length then
                if isLeft && not matchedThisRow then
                    Op.Compute(fun () ->
                        ret (Some(padded lgroup.[i], emitGroup lgroup rgroup lnext rnext (i + 1) 0 false)))
                else
                    emitGroup lgroup rgroup lnext rnext (i + 1) 0 false
            else
                let lrow = lgroup.[i]
                let rrow = rgroup.[j]

                match residualMatches leftSchema rightSchema lrow rrow residual with
                | Error e -> Op.Compute(fun () -> err e)
                | Ok false ->
                    Op.Compute(fun () -> Op.next (emitGroup lgroup rgroup lnext rnext i (j + 1) matchedThisRow))
                | Ok true ->
                    Op.Compute(fun () ->
                        ret (Some(Array.append lrow rrow, emitGroup lgroup rgroup lnext rnext i (j + 1) true)))

        Op.Compute(fun () -> Op.next (loop (MergeStart left) (MergeStart right)))

    /// INNER JOIN（マージ結合）。両入力は結合キーで昇順であること。
    /// 右側を materialize しないので、右が大きなテーブルでもメモリはキー重複分だけで済む。
    let innerJoinMerge
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        =
        mergeJoin false leftSchema left leftKeys rightSchema right rightKeys residual
        |> ret

    /// LEFT JOIN（マージ結合）。両入力は結合キーで昇順であること。
    let leftJoinMerge
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        =
        mergeJoin true leftSchema left leftKeys rightSchema right rightKeys residual
        |> ret

    /// インデックス検索の結果を上限付きで materialize して `joinProbe` の形に載せる。
    /// 非一意索引の prefix 検索は 1キーに大量の行が付き得るため上限を置く。
    let private lookupBounded (lookup: SqlValue array -> Result<Op, DbError>) (key: SqlValue array) =
        lookup key
        >>= fun op -> toArrayWithBudget JoinBudgetBytes op |> Result.map ResizeArray<SqlValue array>

    /// INNER JOIN（インデックス入れ子ループ）。
    let innerJoinIndexed
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (lookup: SqlValue array -> Result<Op, DbError>)
        =
        joinProbe false leftSchema left leftKeys rightSchema (lookupBounded lookup)

    /// LEFT JOIN（インデックス入れ子ループ）。
    let leftJoinIndexed
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (lookup: SqlValue array -> Result<Op, DbError>)
        =
        joinProbe true leftSchema left leftKeys rightSchema (lookupBounded lookup)

    /// 射影。
    /// `SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は対象テーブルの全列に展開する。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    let project (schema: Schema) (groupBy: Expr list) (items: SelectItem list) (src: Op) =
        let col qualifier name = { Qualifier = qualifier; Name = name }
        let sameRef c = col c.Qualifier c.Name
        let aggCallsOf = List.map _.Expression >> aggCalls

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
            let offsets = aggOffsets groupBy foundAggs
            let wholeOffsets = wholeGroupOffsets groupBy

            if List.isEmpty foundAggs && List.isEmpty groupBy then
                Op.map (fun row -> allExprs |> List.map (Eval.eval schema row) |> all |> Result.map List.toArray) src
            else
                Op.map
                    (fun row ->
                        allExprs
                        |> List.map (fun e -> substituteAggs offsets wholeOffsets row e >>= Eval.eval Schema.empty row)
                        |> all
                        |> Result.map List.toArray)
                    src
