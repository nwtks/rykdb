// ===========================================================================
// OpsJoinMerge — マージ結合。
// ===========================================================================

namespace RykDb.Core

open OpsJoinShared

module internal OpsJoinMerge =
    /// マージ結合の入力側の状態。
    /// `MergeStart` はまだ 1行も取り出していない入力、`MergeAt` は現在行と続き。
    type private MergeSide =
        | MergeStart of Op
        | MergeAt of SqlValue array * Op
        | MergeDone

    /// Op から次の 1行を先読みする。
    let private mergeSideFromOp (op: Op) =
        match Op.next op with
        | OpStep.OpError e -> DbErr e
        | OpStep.OpDone -> DbOk MergeDone
        | OpStep.OpRow(row, rest) -> DbOk(MergeAt(row, rest))

    /// 次の 1行を先読みした状態にする。
    let private pullMergeSide =
        function
        | MergeStart op
        | MergeAt(_, op) -> mergeSideFromOp op
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

    /// 結合キーでの行の比較（外部ソート用）。
    /// NULL は最小として並べる（NULL キーは結合しないので順序は任意）。
    /// 比較できない値（Unloaded・型不一致）は 0 を返して並びを保つ。
    /// エラー自体はマージ結合の `compareKeyVectors` が返す。
    /// 結合キーを位置から順に比較する本体。
    [<TailCall>]
    let rec private compareKeyRowsAt (keys: int array) (a: SqlValue array) (b: SqlValue array) (i: int) =
        if i >= keys.Length then
            0
        else
            match a.[keys.[i]], b.[keys.[i]] with
            | SqlValue.Null, SqlValue.Null -> compareKeyRowsAt keys a b (i + 1)
            | SqlValue.Null, _ -> -1
            | _, SqlValue.Null -> 1
            | x, y ->
                match SqlValue.compareValuesV x y with
                | DbOk 0 -> compareKeyRowsAt keys a b (i + 1)
                | DbOk c -> c
                | DbErr _ -> 0

    /// 結合キー列だけで 2行を比較する。
    let private compareKeyRows (keys: int array) (a: SqlValue array) (b: SqlValue array) = compareKeyRowsAt keys a b 0

    /// マージ結合フォールバック用に、パーティションの run をキー順に外部ソートする。
    /// 退避に使うメモリは作業メモリを超えないようにする。
    let sortByJoinKey (budget: MemoryBudget) (keys: int array) (src: Op) =
        let sortBudget =
            { budget with
                Sort =
                    if budget.Work < 0L then budget.Sort
                    elif budget.Sort < 0L then budget.Work
                    else min budget.Sort budget.Work
            }

        ExtSort.sortRows sortBudget () (fun () _ -> DbOk()) (compareKeyRows keys) src

    /// 先頭行と同じ結合キーかどうかを比較する。
    let private mergeSameKey (keys: int array) (first: SqlValue array) (row: SqlValue array) =
        match compareKeyVectors 0 keys first keys row with
        | DbErr e -> err e
        | DbOk 0 -> ret true
        | DbOk _ -> ret false

    /// メモリ内に同じキーの行を集める。
    [<TailCall>]
    let rec private mergeFill
        (budget: MemoryBudget)
        (keys: int array)
        (first: SqlValue array)
        (buf: SqlValue array list)
        (bufBytes: int64)
        (current: MergeSide)
        =
        match pullMergeSide current with
        | DbErr e -> err e
        | DbOk MergeDone -> ret (List.rev buf, None, MergeDone)
        | DbOk(MergeStart _) -> err (DbError.internalError "merge join: 入力状態が不正です")
        | DbOk(MergeAt(row, _) as next) ->
            match mergeSameKey keys first row with
            | Ok false -> ret (List.rev buf, None, next)
            | Ok true ->
                let size = int64 (ValueCodec.managedRowBytes row) + 32L

                if MemoryBudget.exceeded (budget.Work, bufBytes + size) then
                    ret (List.rev buf, Some row, next)
                else
                    mergeFill budget keys first (row :: buf) (bufBytes + size) next
            | Error e -> err e

    /// 退避ファイルへ行をすべて書き出す。
    [<TailCall>]
    let rec private mergeWriteAll (sink: Sink) (rest: SqlValue array list) =
        match rest with
        | [] -> ret ()
        | r :: tail ->
            match Spill.writeRow r sink with
            | Ok() -> mergeWriteAll sink tail
            | x -> x

    /// 残りの同じキー行を退避ファイルへ流す。
    [<TailCall>]
    let rec private mergeDrain
        (keys: int array)
        (sink: Sink)
        (first: SqlValue array)
        (count: int)
        (current: MergeSide)
        =
        match pullMergeSide current with
        | DbErr e -> err e
        | DbOk MergeDone -> ret (count, MergeDone)
        | DbOk(MergeStart _) -> err (DbError.internalError "merge join: 入力状態が不正です")
        | DbOk(MergeAt(row, _) as next) ->
            match mergeSameKey keys first row with
            | Ok false -> ret (count, next)
            | Ok true ->
                match Spill.writeRow row sink with
                | Ok() -> mergeDrain keys sink first (count + 1) next
                | Error e -> err e
            | Error e -> err e

    /// 等値キーの行をまとめて取り出す（入力はキー昇順）。
    /// 返り値は (グループの行, グループの次の行を含む状態)。
    /// グループも上限つきで退避する（`Spill.RowSpool`）。
    let private takeMergeGroup (budget: MemoryBudget) (keys: int array) =
        function
        | MergeStart _
        | MergeDone as side -> ret (Spill.RowSpool.Memory [||], side)
        | MergeAt(first, _) as side ->
            let firstBytes = int64 (ValueCodec.managedRowBytes first) + 32L

            match mergeFill budget keys first [ first ] firstBytes side with
            | Ok(rows, None, next) -> ret (Spill.RowSpool.Memory(List.toArray rows), next)
            | Ok(rows, Some pending, next) ->
                let path = Spill.runPath budget.Directory (System.Guid.NewGuid().ToString "N") 0

                Spill.openSink path
                >>= fun sink ->
                    let buffered = rows @ [ pending ]

                    match mergeWriteAll sink buffered with
                    | Error e ->
                        Spill.closeSink sink
                        Spill.unlink path
                        err e
                    | Ok() ->
                        match mergeDrain keys sink first buffered.Length next with
                        | Error e ->
                            Spill.closeSink sink
                            Spill.unlink path
                            err e
                        | Ok(count, next') ->
                            match Spill.closeSinkResult sink with
                            | Error e ->
                                Spill.unlink path
                                err e
                            | Ok() ->
                                let spool = Spill.RowSpool.Disk([ path ], count)
                                Spill.registerSpool budget.Scope spool
                                ret (spool, next')
            | Error e -> err e

    /// 開始状態の側を 1行先読みして次へ進む。
    let private advanceMergeSide (next: MergeSide -> Op) (side: MergeSide) =
        Op.compute (fun () ->
            match ensureMergeSide side with
            | DbErr e -> OpStep.OpError e
            | DbOk advanced -> Op.next (next advanced))

    /// 左側を 1行進める（LEFT JOIN なら現在行を NULL 拡張して出す）。
    let private advanceMergeLeft
        (isLeft: bool)
        (padded: SqlValue array -> SqlValue array)
        (loop: MergeSide -> MergeSide -> Op)
        (lside: MergeSide)
        (rside: MergeSide)
        (lrow: SqlValue array)
        =
        Op.compute (fun () ->
            match pullMergeSide lside with
            | DbErr e -> OpStep.OpError e
            | DbOk lnext ->
                if isLeft then
                    OpStep.OpRow(padded lrow, loop lnext rside)
                else
                    Op.next (loop lnext rside))

    /// 右側を 1行進める。
    let private advanceMergeRight (loop: MergeSide -> MergeSide -> Op) (lside: MergeSide) (rside: MergeSide) =
        Op.compute (fun () ->
            match pullMergeSide rside with
            | DbErr e -> OpStep.OpError e
            | DbOk rnext -> Op.next (loop lside rnext))

    /// キーが等しい行のグループを両側から取り出して交叉結合へ渡す。
    let private emitMergeGroup
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (emitGroup: Spill.RowSpool -> Spill.RowSpool -> MergeSide -> MergeSide -> Op)
        (lside: MergeSide)
        (rside: MergeSide)
        =
        match takeMergeGroup budget leftKeys lside with
        | Error e -> Op.compute (fun () -> OpStep.OpError e)
        | Ok(lgroup, lnext) ->
            match takeMergeGroup budget rightKeys rside with
            | Error e -> Op.compute (fun () -> OpStep.OpError e)
            | Ok(rgroup, rnext) -> emitGroup lgroup rgroup lnext rnext

    /// 両側に現在行があるときの 1ステップ（NULL キー・大小・等値を振り分ける）。
    let private stepMerge
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (padded: SqlValue array -> SqlValue array)
        (emitGroup: Spill.RowSpool -> Spill.RowSpool -> MergeSide -> MergeSide -> Op)
        (loop: MergeSide -> MergeSide -> Op)
        (lside: MergeSide)
        (rside: MergeSide)
        (lrow: SqlValue array)
        (rrow: SqlValue array)
        =
        if hasNullKey lrow leftKeys then
            advanceMergeLeft isLeft padded loop lside rside lrow
        elif hasNullKey rrow rightKeys then
            advanceMergeRight loop lside rside
        else
            match compareKeyVectors 0 leftKeys lrow rightKeys rrow with
            | DbErr e -> Op.compute (fun () -> OpStep.OpError e)
            | DbOk c when c < 0 -> advanceMergeLeft isLeft padded loop lside rside lrow
            | DbOk c when c > 0 -> advanceMergeRight loop lside rside
            | DbOk _ -> emitMergeGroup budget leftKeys rightKeys emitGroup lside rside

    /// マージ結合で左行を右幅まで NULL で埋める。
    let private mergePadded (rightWidth: int) (lrow: SqlValue array) = padWithNulls lrow rightWidth

    /// マージ結合で残った入力を捨てる Op。
    let private mergeDrainOp =
        function
        | MergeStart op
        | MergeAt(_, op) -> Op.filter (fun _ -> DbOk false) op
        | MergeDone -> Op.Empty

    /// マージ結合のメインループ。
    let rec private mergeJoinLoop
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (lside: MergeSide)
        (rside: MergeSide)
        =
        match lside, rside with
        | MergeStart _, _ ->
            advanceMergeSide
                (fun lnext -> mergeJoinLoop isLeft budget leftKeys rightKeys rightWidth resolve residual lnext rside)
                lside
        | _, MergeStart _ ->
            advanceMergeSide
                (fun rnext -> mergeJoinLoop isLeft budget leftKeys rightKeys rightWidth resolve residual lside rnext)
                rside
        | MergeDone, _ -> mergeDrainOp rside
        | _, MergeDone ->
            if not isLeft then
                mergeDrainOp lside
            else
                match lside with
                | MergeAt(lrow, _) ->
                    Op.compute (fun () ->
                        match pullMergeSide lside with
                        | DbErr e -> OpStep.OpError e
                        | DbOk lnext ->
                            OpStep.OpRow(
                                mergePadded rightWidth lrow,
                                mergeJoinLoop
                                    isLeft
                                    budget
                                    leftKeys
                                    rightKeys
                                    rightWidth
                                    resolve
                                    residual
                                    lnext
                                    MergeDone
                            ))
                | _ -> Op.Empty
        | MergeAt(lrow, _), MergeAt(rrow, _) ->
            stepMerge
                isLeft
                budget
                leftKeys
                rightKeys
                (mergePadded rightWidth)
                (mergeJoinEmitGroup isLeft budget leftKeys rightKeys rightWidth resolve residual)
                (mergeJoinLoop isLeft budget leftKeys rightKeys rightWidth resolve residual)
                lside
                rside
                lrow
                rrow

    /// マージ結合で一致したグループを出力する。
    and private mergeJoinEmitGroup
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (lspool: Spill.RowSpool)
        (rspool: Spill.RowSpool)
        (lnext: MergeSide)
        (rnext: MergeSide)
        =
        match lspool, rspool with
        | Spill.RowSpool.Memory leftRows, Spill.RowSpool.Disk _ ->
            match Spill.reopenKeep budget.Scope rspool with
            | Error e -> Op.compute (fun () -> OpStep.OpError e)
            | Ok rrows ->
                let matched = if isLeft then Array.create leftRows.Length false else [||]

                mergeJoinRightOuter
                    isLeft
                    budget
                    leftKeys
                    rightKeys
                    rightWidth
                    resolve
                    residual
                    leftRows
                    matched
                    lnext
                    rnext
                    lspool
                    rspool
                    rrows
        | _ ->
            match Spill.reopenKeep budget.Scope lspool with
            | Error e -> Op.compute (fun () -> OpStep.OpError e)
            | Ok lrows ->
                mergeJoinNextLeft
                    isLeft
                    budget
                    leftKeys
                    rightKeys
                    rightWidth
                    resolve
                    residual
                    lspool
                    rspool
                    lnext
                    rnext
                    lrows

    /// メモリ上の左グループを内側にして、退避した右グループを外側に流す。
    /// `LEFT` では一致しなかった左行を最後に NULL 拡張する。
    and private mergeJoinRightOuter
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (leftRows: SqlValue array array)
        (matched: bool array)
        (lnext: MergeSide)
        (rnext: MergeSide)
        (lspool: Spill.RowSpool)
        (rspool: Spill.RowSpool)
        (rrows: Op)
        =
        Op.compute (fun () ->
            match Op.next rrows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                Spill.forgetSpool budget.Scope lspool
                Spill.deleteSpool lspool
                Spill.forgetSpool budget.Scope rspool
                Spill.deleteSpool rspool

                if isLeft then
                    Op.next (
                        mergeJoinUnmatchedLeft
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            leftRows
                            matched
                            0
                            lnext
                            rnext
                    )
                else
                    Op.next (mergeJoinLoop isLeft budget leftKeys rightKeys rightWidth resolve residual lnext rnext)
            | OpStep.OpRow(rrow, rrest) ->
                Op.next (
                    mergeJoinRightRow
                        isLeft
                        rightWidth
                        resolve
                        residual
                        leftRows
                        matched
                        0
                        rrow
                        (mergeJoinRightOuter
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            leftRows
                            matched
                            lnext
                            rnext
                            lspool
                            rspool
                            rrest)
                ))

    /// 1 右行とメモリ上の全左行を突き合わせる。
    and private mergeJoinRightRow
        (isLeft: bool)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (leftRows: SqlValue array array)
        (matched: bool array)
        (rowIndex: int)
        (rrow: SqlValue array)
        (rest: Op)
        =
        if rowIndex >= leftRows.Length then
            rest
        else
            Op.compute (fun () ->
                let lrow = leftRows.[rowIndex]

                match residualMatches resolve lrow rrow residual with
                | DbErr e -> OpStep.OpError e
                | DbOk false ->
                    Op.next (
                        mergeJoinRightRow isLeft rightWidth resolve residual leftRows matched (rowIndex + 1) rrow rest
                    )
                | DbOk true ->
                    if isLeft then
                        matched.[rowIndex] <- true

                    OpStep.OpRow(
                        Array.append lrow rrow,
                        mergeJoinRightRow isLeft rightWidth resolve residual leftRows matched (rowIndex + 1) rrow rest
                    ))

    /// 右グループを流し終えたあと、`LEFT` で一致しなかった左行を NULL 拡張して出す。
    and private mergeJoinUnmatchedLeft
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (leftRows: SqlValue array array)
        (matched: bool array)
        (rowIndex: int)
        (lnext: MergeSide)
        (rnext: MergeSide)
        =
        if rowIndex >= leftRows.Length then
            mergeJoinLoop isLeft budget leftKeys rightKeys rightWidth resolve residual lnext rnext
        else
            Op.compute (fun () ->
                if matched.[rowIndex] then
                    Op.next (
                        mergeJoinUnmatchedLeft
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            leftRows
                            matched
                            (rowIndex + 1)
                            lnext
                            rnext
                    )
                else
                    OpStep.OpRow(
                        mergePadded rightWidth leftRows.[rowIndex],
                        mergeJoinUnmatchedLeft
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            leftRows
                            matched
                            (rowIndex + 1)
                            lnext
                            rnext
                    ))

    /// 一致グループの左行を順に出力する。
    and private mergeJoinNextLeft
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (lspool: Spill.RowSpool)
        (rspool: Spill.RowSpool)
        (lnext: MergeSide)
        (rnext: MergeSide)
        (lrows: Op)
        =
        Op.compute (fun () ->
            match Op.next lrows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                Spill.forgetSpool budget.Scope lspool
                Spill.deleteSpool lspool
                Spill.forgetSpool budget.Scope rspool
                Spill.deleteSpool rspool
                Op.next (mergeJoinLoop isLeft budget leftKeys rightKeys rightWidth resolve residual lnext rnext)
            | OpStep.OpRow(lrow, lrest) ->
                match Spill.reopenKeep budget.Scope rspool with
                | Error e -> OpStep.OpError e
                | Ok rrows ->
                    Op.next (
                        mergeJoinNextRight
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            lspool
                            rspool
                            lnext
                            rnext
                            lrow
                            lrest
                            rrows
                            false
                    ))

    /// 一致グループの右行を評価する。
    and private mergeJoinNextRight
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftKeys: int array)
        (rightKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (lspool: Spill.RowSpool)
        (rspool: Spill.RowSpool)
        (lnext: MergeSide)
        (rnext: MergeSide)
        (lrow: SqlValue array)
        (lrest: Op)
        (rrows: Op)
        (matched: bool)
        =
        Op.compute (fun () ->
            match Op.next rrows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                if isLeft && not matched then
                    OpStep.OpRow(
                        mergePadded rightWidth lrow,
                        mergeJoinNextLeft
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            lspool
                            rspool
                            lnext
                            rnext
                            lrest
                    )
                else
                    Op.next (
                        mergeJoinNextLeft
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            lspool
                            rspool
                            lnext
                            rnext
                            lrest
                    )
            | OpStep.OpRow(rrow, rrest) ->
                match residualMatches resolve lrow rrow residual with
                | DbErr e -> OpStep.OpError e
                | DbOk false ->
                    Op.next (
                        mergeJoinNextRight
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            lspool
                            rspool
                            lnext
                            rnext
                            lrow
                            lrest
                            rrest
                            matched
                    )
                | DbOk true ->
                    OpStep.OpRow(
                        Array.append lrow rrow,
                        mergeJoinNextRight
                            isLeft
                            budget
                            leftKeys
                            rightKeys
                            rightWidth
                            resolve
                            residual
                            lspool
                            rspool
                            lnext
                            rnext
                            lrow
                            lrest
                            rrest
                            true
                    ))

    /// マージ結合。両入力は結合キーで昇順に並んでいる必要がある
    /// （`Analyzer.chooseJoinAlgorithm` が走査の並び順を確認して選ぶ）。
    /// 右側を materialize せず、両側を 1行ずつ流す。
    /// <remarks>
    /// `Op.compute` の継続内で `loop` / `emitGroup` を呼ぶので、1行ごとに 1段だけ進む。
    /// </remarks>
    let mergeJoinWith
        (isLeft: bool)
        (budget: MemoryBudget)
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
            Eval.prepare (Schema.concat leftSchema rightSchema) (residualExprs residual)

        mergeJoinLoop isLeft budget leftKeys rightKeys rightWidth resolve residual (MergeStart left) (MergeStart right)

    /// INNER JOIN（マージ結合）。両入力は結合キーで昇順であること。
    let innerJoinMerge
        (budget: MemoryBudget)
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        =
        match checkResidual leftSchema rightSchema residual with
        | Error e -> err e
        | Ok() -> ret (mergeJoinWith false budget leftSchema left leftKeys rightSchema right rightKeys residual)

    /// LEFT JOIN（マージ結合）。両入力は結合キーで昇順であること。
    let leftJoinMerge
        (budget: MemoryBudget)
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        =
        match checkResidual leftSchema rightSchema residual with
        | Error e -> err e
        | Ok() -> ret (mergeJoinWith true budget leftSchema left leftKeys rightSchema right rightKeys residual)
