namespace RykDb.Core

module internal OpsJoin =
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
            | OpStep.OpError e -> err e
            | OpStep.OpDone -> ret (List.rev acc |> List.toArray)
            | OpStep.OpRow(row, nxt) ->
                let used = used + int64 (ValueCodec.managedRowBytes row) + 32L

                if used > budget then
                    sprintf "JOIN の右側の行が多すぎます（概算 %d バイトの上限）。条件で絞るか、インデックスで絞ってください" budget
                    |> DbError.capacity
                    |> err
                else
                    go used (row :: acc) nxt

        go 0L []

    /// 行の末尾に NULL を足した新しい行を作る。
    /// `Array.create` + `Array.append` の 2回の割当を避ける。
    let private padWithNulls (row: SqlValue array) (width: int) =
        let combined = Array.zeroCreate<SqlValue>(row.Length + width)
        Array.blit row 0 combined 0 row.Length
        Array.fill combined row.Length width SqlValue.Null
        combined

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
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]
        let joinPred l r = Eval.matchesJoinedV resolve l r

        // 1回の Op.next で「一致するまで右を進める」。
        // 候補ごとに Op.compute を作らないので、一致しない行が多い結合で割当が増えない。
        let rec loop matched i =
            Op.compute (fun () -> advance matched i)

        and advance matched j =
            if j >= rightRows.Length then
                Op.next (onExhausted matched)
            else
                let rrow = rightRows.[j]

                match joinPred lrow rrow on with
                | DbOk true -> OpStep.OpRow(Array.append lrow rrow, loop true (j + 1))
                | DbOk false -> advance matched (j + 1)
                | DbErr e -> Op.next (onError e matched)

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
                            (fun e _ -> Op.compute (fun () -> OpStep.OpError e))
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
                            Op.emit (padWithNulls lrow width) (joinRows width lnext rightRows)

                    let onError e _ = Op.compute (fun () -> OpStep.OpError e)
                    Op.next (joinLoop leftSchema rightSchema on onExhausted onError rightRows lrow))
                cur

        toArrayWithBudget JoinBudgetBytes right
        |> Result.map (joinRows rightSchema.Columns.Length left)

    /// 等値結合のキーが NULL を含むか（SQL では NULL は一致しない）。
    let private hasNullKey (row: SqlValue array) =
        Array.exists (fun i -> SqlValue.isNull row.[i])

    let private keyOf (row: SqlValue array) = Array.map (fun i -> row.[i])

    /// 等値キー以外の ON 句（residual）を評価する。
    /// 左右の行を連結せずに評価する。
    let private residualMatches resolve lrow rrow =
        function
        | None -> DbOk true
        | Some p -> Eval.matchesJoinedV resolve lrow rrow p

    /// ハッシュ結合 / インデックス入れ子ループの共通ループ。
    /// lookupRows は左キーから右行のリストを返す（見つからなければ None）。
    /// <remarks>
    /// Op.compute の継続内の相互再帰は「次の行を要求されたとき」に 1段ずつ進むので、スタックは積み上がらない。
    /// TailAudit はラムダ内の呼び出しを非末尾として報告するためベースラインに登録している。
    /// </remarks>
    let private joinProbe
        (isLeft: bool)
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (lookupRows: SqlValue array -> Result<SqlValue array array option, DbError>)
        (residual: Expr option)
        =
        let rightWidth = rightSchema.Columns.Length

        let resolve =
            Eval.prepare (Schema.concat leftSchema rightSchema) (Option.toList residual)

        let padded lrow = padWithNulls lrow rightWidth

        let rec probe cur =
            Op.pull
                (fun lrow lnext ->
                    if hasNullKey lrow leftKeys then
                        if isLeft then
                            OpStep.OpRow(padded lrow, probe lnext)
                        else
                            Op.next (probe lnext)
                    else
                        match lookupRows (keyOf lrow leftKeys) with
                        | Error e -> OpStep.OpError e
                        | Ok None ->
                            if isLeft then
                                OpStep.OpRow(padded lrow, probe lnext)
                            else
                                Op.next (probe lnext)
                        | Ok(Some rows) -> Op.next (emit lrow lnext rows 0 false))
                cur

        and emit lrow lnext rows i matched =
            if i >= rows.Length then
                if isLeft && not matched then
                    Op.compute (fun () -> OpStep.OpRow(padded lrow, probe lnext))
                else
                    probe lnext
            else
                let rrow = rows.[i]

                match residualMatches resolve lrow rrow residual with
                | DbErr e -> Op.compute (fun () -> OpStep.OpError e)
                | DbOk false -> Op.compute (fun () -> Op.next (emit lrow lnext rows (i + 1) matched))
                | DbOk true ->
                    Op.compute (fun () -> OpStep.OpRow(Array.append lrow rrow, emit lrow lnext rows (i + 1) true))

        probe left

    /// 右側のハッシュ表を作る（NULL キーは登録しない）。
    /// バケットは右入力の順序を保つ（マージ結合と同じ出力順にするため）。
    let private buildHashTable (rightRows: SqlValue array array) (rightKeys: int array) =
        let buckets =
            System.Collections.Generic.Dictionary<SqlValue array, SqlValue array list> HashIdentity.Structural

        for rrow in rightRows do
            if not (hasNullKey rrow rightKeys) then
                let key = keyOf rrow rightKeys

                match buckets.TryGetValue key with
                | true, bucket -> buckets.[key] <- rrow :: bucket
                | _ -> buckets.[key] <- [ rrow ]

        let table =
            System.Collections.Generic.Dictionary<SqlValue array, SqlValue array array> HashIdentity.Structural

        for kv in buckets do
            table.[kv.Key] <- kv.Value |> List.rev |> List.toArray

        table

    let private lookupHash
        (table: System.Collections.Generic.Dictionary<SqlValue array, SqlValue array array>)
        (key: SqlValue array)
        =
        match table.TryGetValue key with
        | true, bucket -> ret (Some bucket)
        | _ -> ret None

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
            | OpStep.OpError e -> DbErr e
            | OpStep.OpDone -> DbOk MergeDone
            | OpStep.OpRow(row, rest) -> DbOk(MergeAt(row, rest))

        match side with
        | MergeStart op
        | MergeAt(_, op) -> fromOp op
        | MergeDone -> DbOk MergeDone

    /// まだ 1行も取り出していなければ、先頭行を先読みする。
    let private ensureMergeSide (side: MergeSide) =
        match side with
        | MergeStart _ -> pullMergeSide side
        | _ -> DbOk side

    /// 結合キーの比較（NULL は呼び出し側で除く）。
    /// キー配列を作らずに、行とキー列位置から直接比較する。
    [<TailCall>]
    let rec private compareKeyVectors
        (i: int)
        (leftKeys: int array)
        (left: SqlValue array)
        (rightKeys: int array)
        (right: SqlValue array)
        =
        if i >= leftKeys.Length then
            DbOk 0
        else
            match SqlValue.compareValuesV left.[leftKeys.[i]] right.[rightKeys.[i]] with
            | DbErr e -> DbErr e
            | DbOk 0 -> compareKeyVectors (i + 1) leftKeys left rightKeys right
            | DbOk c -> DbOk c

    /// 等値キーの行をまとめて取り出す（入力はキー昇順）。
    /// 返り値は (グループの行, グループの次の行を含む状態)。
    /// 1グループのメモリは `JoinBudgetBytes` で制限する。
    let private takeMergeGroup (keys: int array) (side: MergeSide) =
        match side with
        | MergeStart _
        | MergeDone -> ret ([||], side)
        | MergeAt(first, _) ->
            let rec go used acc current =
                match pullMergeSide current with
                | DbErr e -> err e
                | DbOk MergeDone -> ret (List.rev acc |> List.toArray, MergeDone)
                | DbOk(MergeStart _) -> DbError.internalError "merge join: 入力状態が不正です" |> err
                | DbOk(MergeAt(row, _) as next) ->
                    match compareKeyVectors 0 keys first keys row with
                    | DbErr e -> err e
                    | DbOk 0 ->
                        let used = used + int64 (ValueCodec.managedRowBytes row) + 32L

                        if used > JoinBudgetBytes then
                            sprintf "JOIN の同じキーの行が多すぎます（概算 %d バイトの上限）" JoinBudgetBytes
                            |> DbError.capacity
                            |> err
                        else
                            go used (row :: acc) next
                    | DbOk _ -> ret (List.rev acc |> List.toArray, next)

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
    /// `Op.compute` の継続内で `loop` / `emitGroup` を呼ぶので、TailAudit はラムダ内の
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

        let resolve =
            Eval.prepare (Schema.concat leftSchema rightSchema) (Option.toList residual)

        let padded lrow = padWithNulls lrow rightWidth

        let rec loop lside rside =
            match lside, rside with
            | MergeStart _, _ ->
                Op.compute (fun () ->
                    match ensureMergeSide lside with
                    | DbErr e -> OpStep.OpError e
                    | DbOk lnext -> Op.next (loop lnext rside))
            | _, MergeStart _ ->
                Op.compute (fun () ->
                    match ensureMergeSide rside with
                    | DbErr e -> OpStep.OpError e
                    | DbOk rnext -> Op.next (loop lside rnext))
            | MergeDone, _ -> Op.Empty
            | _, MergeDone ->
                if not isLeft then
                    Op.Empty
                else
                    match lside with
                    | MergeAt(lrow, _) ->
                        Op.compute (fun () ->
                            match pullMergeSide lside with
                            | DbErr e -> OpStep.OpError e
                            | DbOk lnext -> OpStep.OpRow(padded lrow, loop lnext MergeDone))
                    | _ -> Op.Empty
            | MergeAt(lrow, _), MergeAt(rrow, _) ->
                if hasNullKey lrow leftKeys then
                    Op.compute (fun () ->
                        match pullMergeSide lside with
                        | DbErr e -> OpStep.OpError e
                        | DbOk lnext ->
                            if isLeft then
                                OpStep.OpRow(padded lrow, loop lnext rside)
                            else
                                Op.next (loop lnext rside))
                elif hasNullKey rrow rightKeys then
                    Op.compute (fun () ->
                        match pullMergeSide rside with
                        | DbErr e -> OpStep.OpError e
                        | DbOk rnext -> Op.next (loop lside rnext))
                else
                    match compareKeyVectors 0 leftKeys lrow rightKeys rrow with
                    | DbErr e -> Op.compute (fun () -> OpStep.OpError e)
                    | DbOk c when c < 0 ->
                        Op.compute (fun () ->
                            match pullMergeSide lside with
                            | DbErr e -> OpStep.OpError e
                            | DbOk lnext ->
                                if isLeft then
                                    OpStep.OpRow(padded lrow, loop lnext rside)
                                else
                                    Op.next (loop lnext rside))
                    | DbOk c when c > 0 ->
                        Op.compute (fun () ->
                            match pullMergeSide rside with
                            | DbErr e -> OpStep.OpError e
                            | DbOk rnext -> Op.next (loop lside rnext))
                    | DbOk _ ->
                        match takeMergeGroup leftKeys lside with
                        | Error e -> Op.compute (fun () -> OpStep.OpError e)
                        | Ok(lgroup, lnext) ->
                            match takeMergeGroup rightKeys rside with
                            | Error e -> Op.compute (fun () -> OpStep.OpError e)
                            | Ok(rgroup, rnext) -> emitGroup lgroup rgroup lnext rnext 0 0 false

        and emitGroup lgroup rgroup lnext rnext i j matchedThisRow =
            if i >= lgroup.Length then
                loop lnext rnext
            elif j >= rgroup.Length then
                if isLeft && not matchedThisRow then
                    Op.compute (fun () ->
                        OpStep.OpRow(padded lgroup.[i], emitGroup lgroup rgroup lnext rnext (i + 1) 0 false))
                else
                    Op.compute (fun () -> Op.next (emitGroup lgroup rgroup lnext rnext (i + 1) 0 false))
            else
                let lrow = lgroup.[i]
                let rrow = rgroup.[j]

                match residualMatches resolve lrow rrow residual with
                | DbErr e -> Op.compute (fun () -> OpStep.OpError e)
                | DbOk false ->
                    Op.compute (fun () -> Op.next (emitGroup lgroup rgroup lnext rnext i (j + 1) matchedThisRow))
                | DbOk true ->
                    Op.compute (fun () ->
                        OpStep.OpRow(Array.append lrow rrow, emitGroup lgroup rgroup lnext rnext i (j + 1) true))

        loop (MergeStart left) (MergeStart right)

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
        >>= fun op ->
            toArrayWithBudget JoinBudgetBytes op
            |> Result.map (fun rows -> if Array.isEmpty rows then None else Some rows)

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
