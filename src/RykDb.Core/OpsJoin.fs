// ===========================================================================
// OpsJoin — 入れ子ループ結合とインデックス入れ子ループ。
// ===========================================================================

namespace RykDb.Core

open OpsJoinShared
open OpsJoinHash

module internal OpsJoin =
    /// nested loop の 1ステップを遅延して作る。
    [<TailCall>]
    let rec private joinLoopGo
        (onExhausted: bool -> Op)
        (onError: DbError -> bool -> Op)
        (lrow: SqlValue array)
        (rightRows: SqlValue array array)
        (resolve: Prepared)
        (on: Expr)
        (matched: bool)
        (i: int)
        =
        Op.compute (fun () -> joinLoopAdvance onExhausted onError lrow rightRows resolve on matched i)

    /// nested loop の内側を 1行進める。
    and private joinLoopAdvance
        (onExhausted: bool -> Op)
        (onError: DbError -> bool -> Op)
        (lrow: SqlValue array)
        (rightRows: SqlValue array array)
        (resolve: Prepared)
        (on: Expr)
        (matched: bool)
        (j: int)
        =
        if j >= rightRows.Length then
            Op.next (onExhausted matched)
        else
            let rrow = rightRows.[j]

            match joinPred resolve on lrow rrow with
            | DbOk true ->
                OpStep.OpRow(
                    Array.append lrow rrow,
                    joinLoopGo onExhausted onError lrow rightRows resolve on true (j + 1)
                )
            | DbOk false -> joinLoopAdvance onExhausted onError lrow rightRows resolve on matched (j + 1)
            | DbErr e -> Op.next (onError e matched)

    /// 入れ子ループの本体（一致ごとに emit する）。
    let private joinLoop
        (onExhausted: bool -> Op)
        (onError: DbError -> bool -> Op)
        (lrow: SqlValue array)
        (rightRows: SqlValue array array)
        (resolve: Prepared)
        (on: Expr)
        =
        joinLoopGo onExhausted onError lrow rightRows resolve on false 0

    /// INNER の内側ループ（左行を 1つずつ進める）。
    let rec private innerJoinRows (resolve: Prepared) (rightRows: SqlValue array array) (on: Expr) (cur: Op) =
        Op.pull
            (fun lrow lnext ->
                Op.next (
                    joinLoop
                        (fun _ -> innerJoinRows resolve rightRows on lnext)
                        (fun e _ -> Op.compute (fun () -> OpStep.OpError e))
                        lrow
                        rightRows
                        resolve
                        on
                ))
            cur

    /// INNER 結合（左右とも配列）。
    let private innerJoinArrays
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (rightRows: SqlValue array array)
        (on: Expr)
        =
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]
        innerJoinRows resolve rightRows on left

    /// ON 句評価失敗時の Op。
    let private leftJoinOnError (e: DbError) _ = Op.compute (fun () -> OpStep.OpError e)

    /// LEFT JOIN の内側ループ（左行を 1つずつ進める）。
    [<TailCall>]
    let rec private leftJoinRows
        (resolve: Prepared)
        (rightRows: SqlValue array array)
        (on: Expr)
        (width: int)
        (cur: Op)
        =
        Op.pull
            (fun lrow lnext ->
                Op.next (
                    joinLoop
                        (leftJoinOnExhausted resolve rightRows on width lrow lnext)
                        leftJoinOnError
                        lrow
                        rightRows
                        resolve
                        on
                ))
            cur

    /// 右が尽きたときの NULL 拡張と継続を作る。
    and private leftJoinOnExhausted
        (resolve: Prepared)
        (rightRows: SqlValue array array)
        (on: Expr)
        (width: int)
        (lrow: SqlValue array)
        (lnext: Op)
        (matched: bool)
        =
        if matched then
            leftJoinRows resolve rightRows on width lnext
        else
            Op.emit (padWithNulls lrow width) (leftJoinRows resolve rightRows on width lnext)

    /// LEFT 結合（左右とも配列）。
    let private leftJoinArrays
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (rightRows: SqlValue array array)
        (on: Expr)
        =
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]
        leftJoinRows resolve rightRows on rightSchema.Columns.Length left

    /// 退避 NL の左ブロックの最大行数。
    /// 右をブロックごとに 1回だけ流すためのブロック（行数固定でメモリを抑える）。
    [<Literal>]
    let private SpilledBlockRows = 64

    /// 退避右側の入れ子ループ（ブロック単位）。
    /// 左をブロックに溜め、右はブロックごとに 1回だけ先頭から読む。
    /// I/O は O((左行数 / ブロック行数) × 右行数) に減る（旧実装は左行ごとに再読込）。
    /// 出力順は右行優先（1右行に対してブロック内の左を回す）で、
    /// `ORDER BY` が無い限り SQL は行順を保証しない。
    /// <remarks>
    /// 各ステップは `Op.compute` の継続内で 1段だけ進み、スタックは積み上がらない。
    /// `matched` はブロック局所のスクラッチで、演算子の外へ出さない。
    /// </remarks>
    let rec private spilledBlockLoad
        (isLeft: bool)
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (width: int)
        (acc: ResizeArray<SqlValue array>)
        (cur: Op)
        =
        if acc.Count >= SpilledBlockRows then
            spilledBlockStart isLeft budget spool resolve on width (acc.ToArray()) cur
        else
            Op.compute (fun () ->
                match Op.next cur with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone ->
                    if acc.Count = 0 then
                        Spill.forgetSpool budget.Scope spool
                        Spill.deleteSpool spool
                        OpStep.OpDone
                    else
                        Op.next (spilledBlockStart isLeft budget spool resolve on width (acc.ToArray()) cur)
                | OpStep.OpRow(lrow, nxt) ->
                    acc.Add lrow
                    Op.next (spilledBlockLoad isLeft budget spool resolve on width acc nxt))

    and private spilledBlockStart
        (isLeft: bool)
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (width: int)
        (leftRows: SqlValue array array)
        (lnext: Op)
        =
        match Spill.reopenKeep budget.Scope spool with
        | Error e -> Op.compute (fun () -> OpStep.OpError e)
        | Ok rows ->
            let matched = Array.create leftRows.Length false

            Op.compute (fun () ->
                Op.next (spilledBlockNext isLeft budget spool resolve on width leftRows lnext matched rows))

    and private spilledBlockNext
        (isLeft: bool)
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (width: int)
        (leftRows: SqlValue array array)
        (lnext: Op)
        (matched: bool array)
        (rows: Op)
        =
        Op.compute (fun () ->
            match Op.next rows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                Op.next (spilledBlockFinish isLeft budget spool resolve on width leftRows lnext matched 0)
            | OpStep.OpRow(rrow, rest) ->
                Op.next (spilledBlockCompare isLeft budget spool resolve on width leftRows lnext matched rest rrow 0))

    and private spilledBlockCompare
        (isLeft: bool)
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (width: int)
        (leftRows: SqlValue array array)
        (lnext: Op)
        (matched: bool array)
        (rows: Op)
        (rrow: SqlValue array)
        (i: int)
        =
        if i >= leftRows.Length then
            spilledBlockNext isLeft budget spool resolve on width leftRows lnext matched rows
        else
            match Eval.matchesJoinedV resolve leftRows.[i] rrow on with
            | DbErr e -> Op.compute (fun () -> OpStep.OpError e)
            | DbOk false ->
                spilledBlockCompare isLeft budget spool resolve on width leftRows lnext matched rows rrow (i + 1)
            | DbOk true ->
                matched.[i] <- true

                Op.emit
                    (Array.append leftRows.[i] rrow)
                    (spilledBlockCompare isLeft budget spool resolve on width leftRows lnext matched rows rrow (i + 1))

    and private spilledBlockFinish
        (isLeft: bool)
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (width: int)
        (leftRows: SqlValue array array)
        (lnext: Op)
        (matched: bool array)
        (i: int)
        =
        if i >= leftRows.Length then
            spilledBlockLoad isLeft budget spool resolve on width (ResizeArray<SqlValue array>()) lnext
        elif isLeft && not matched.[i] then
            Op.emit
                (padWithNulls leftRows.[i] width)
                (spilledBlockFinish isLeft budget spool resolve on width leftRows lnext matched (i + 1))
        else
            spilledBlockFinish isLeft budget spool resolve on width leftRows lnext matched (i + 1)

    /// INNER 結合（右をスプールから読む）。
    let private innerJoinSpilled
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (on: Expr)
        =
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]
        spilledBlockLoad false budget spool resolve on 0 (ResizeArray<SqlValue array>()) left

    /// 右側を退避したときの LEFT JOIN。
    let private leftJoinSpilled
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (on: Expr)
        =
        let width = rightSchema.Columns.Length
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]
        spilledBlockLoad true budget spool resolve on width (ResizeArray<SqlValue array>()) left

    /// INNER JOIN（nested loop）。
    /// 右側は内側ループごとに先頭から引き直す。
    /// 一致が無い左行は捨て、ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let innerJoin (budget: MemoryBudget) (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        match checkExprs leftSchema rightSchema [ on ] with
        | Error e -> err e
        | Ok() ->
            Spill.spool budget right
            >>= function
                | Spill.RowSpool.Memory rightRows -> ret (innerJoinArrays leftSchema left rightSchema rightRows on)
                | spool -> ret (innerJoinSpilled budget spool leftSchema left rightSchema on)

    /// LEFT JOIN。
    /// 左の行に右が 1行も無ければ NULL で埋める。
    /// ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let leftJoin (budget: MemoryBudget) (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        match checkExprs leftSchema rightSchema [ on ] with
        | Error e -> err e
        | Ok() ->
            Spill.spool budget right
            >>= function
                | Spill.RowSpool.Memory rightRows -> ret (leftJoinArrays leftSchema left rightSchema rightRows on)
                | spool -> ret (leftJoinSpilled budget spool leftSchema left rightSchema on)


    /// INNER JOIN（インデックス入れ子ループ）。
    /// 索引検索の結果は materialize せず 1行ずつ流す。
    let innerJoinIndexed
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (residual: Expr option)
        (lookupRows: SqlValue array -> Result<Op, DbError>)
        =
        match checkResidual leftSchema rightSchema residual with
        | Error e -> err e
        | Ok() ->
            ret (
                joinProbe
                    false
                    leftSchema
                    left
                    leftKeys
                    rightSchema
                    residual
                    (fun row -> Ok(keyOf row leftKeys))
                    lookupRows
            )

    /// LEFT JOIN（インデックス入れ子ループ）。
    let leftJoinIndexed
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (residual: Expr option)
        (lookupRows: SqlValue array -> Result<Op, DbError>)
        =
        match checkResidual leftSchema rightSchema residual with
        | Error e -> err e
        | Ok() ->
            ret (
                joinProbe
                    true
                    leftSchema
                    left
                    leftKeys
                    rightSchema
                    residual
                    (fun row -> Ok(keyOf row leftKeys))
                    lookupRows
            )
