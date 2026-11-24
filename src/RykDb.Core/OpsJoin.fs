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

    /// 右側を退避したときの入れ子ループ。
    /// 左行ごとに run を先頭から読み直す（I/O は O(左行数 × 右行数) になるが、メモリは一定）。
    /// <remarks>
    /// `joinRows` / `scan` は `Op.compute` の継続内で呼ぶので、1行ごとに 1段だけ進み、
    /// スタックは積み上がらない。
    /// </remarks>
    /// 退避右側の入れ子ループ（左行を進める）。
    [<TailCall>]
    let rec private spilledJoinRows
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (cur: Op)
        =
        Op.compute (fun () ->
            match Op.next cur with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                Spill.forgetSpool budget.Scope spool
                Spill.deleteSpool spool
                OpStep.OpDone
            | OpStep.OpRow(lrow, lnext) ->
                match Spill.reopenKeep budget.Scope spool with
                | Error e -> OpStep.OpError e
                | Ok rows -> Op.next (spilledScan budget spool resolve on lrow lnext rows))

    /// 退避右側の 1行を評価して次へ進める。
    and private spilledScan
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (lrow: SqlValue array)
        (lnext: Op)
        (rows: Op)
        =
        Op.compute (fun () ->
            match Op.next rows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone -> Op.next (spilledJoinRows budget spool resolve on lnext)
            | OpStep.OpRow(rrow, rest) ->
                match Eval.matchesJoinedV resolve lrow rrow on with
                | DbErr e -> OpStep.OpError e
                | DbOk true ->
                    OpStep.OpRow(Array.append lrow rrow, spilledScan budget spool resolve on lrow lnext rest)
                | DbOk false -> Op.next (spilledScan budget spool resolve on lrow lnext rest))

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
        spilledJoinRows budget spool resolve on left

    /// 退避右側の LEFT JOIN（左行を進める）。
    [<TailCall>]
    let rec private leftSpilledJoinRows
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (width: int)
        (cur: Op)
        =
        Op.compute (fun () ->
            match Op.next cur with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                Spill.forgetSpool budget.Scope spool
                Spill.deleteSpool spool
                OpStep.OpDone
            | OpStep.OpRow(lrow, lnext) ->
                match Spill.reopenKeep budget.Scope spool with
                | Error e -> OpStep.OpError e
                | Ok rows -> Op.next (leftSpilledScan budget spool resolve on width lrow lnext rows false))

    /// 退避右側の LEFT JOIN（右行を評価する）。
    and private leftSpilledScan
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (resolve: Prepared)
        (on: Expr)
        (width: int)
        (lrow: SqlValue array)
        (lnext: Op)
        (rows: Op)
        (matched: bool)
        =
        Op.compute (fun () ->
            match Op.next rows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                if matched then
                    Op.next (leftSpilledJoinRows budget spool resolve on width lnext)
                else
                    OpStep.OpRow(padWithNulls lrow width, leftSpilledJoinRows budget spool resolve on width lnext)
            | OpStep.OpRow(rrow, rest) ->
                match Eval.matchesJoinedV resolve lrow rrow on with
                | DbErr e -> OpStep.OpError e
                | DbOk true ->
                    OpStep.OpRow(
                        Array.append lrow rrow,
                        leftSpilledScan budget spool resolve on width lrow lnext rest true
                    )
                | DbOk false -> Op.next (leftSpilledScan budget spool resolve on width lrow lnext rest matched))

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
        leftSpilledJoinRows budget spool resolve on width left

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
