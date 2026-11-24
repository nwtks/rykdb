// ===========================================================================
// OpsJoin — JOIN の物理演算子。
//
// 保持する行は `Spill.spool` で上限つきにする。
// 上限内ならメモリ、超えたら一時ファイルへ退避するので、
// どれだけ大きい入力でも `CapacityError` にはならずピークメモリは上限内に収まる。
// ===========================================================================

namespace RykDb.Core

module internal OpsJoin =
    /// 行の末尾に NULL を足した新しい行を作る。
    /// `Array.create` + `Array.append` の 2回の割当を避ける。
    let private padWithNulls (row: SqlValue array) (width: int) =
        let combined = Array.zeroCreate<SqlValue>(row.Length + width)
        Array.blit row 0 combined 0 row.Length
        Array.fill combined row.Length width SqlValue.Null
        combined

    /// 結合キーの値だけを取り出す（キー配列を作る）。
    let private keyOf (row: SqlValue array) = Array.map (fun i -> row.[i])

    /// 結合キーを順序保持符号化する。
    /// ハッシュの等値判定を SQL の比較（`SqlValue.compareValuesV`）と一致させる:
    /// NaN は単一のビットパターンに正規化され、DECIMAL の 1.0 と 1.00 は同じキーになる。
    /// パーティション分割も同じ符号化を使うので、等しいキーは必ず同じパーティションに入る。
    let private encodeKey (row: SqlValue array) (keys: int array) = KeyBuilder.ofValues (keyOf row keys)

    /// 等値結合のキーが NULL を含むか（SQL では NULL は一致しない）。
    let private hasNullKey (row: SqlValue array) =
        Array.exists (fun i -> SqlValue.isNull row.[i])

    let private residualExprs =
        function
        | None -> []
        | Some p -> [ p ]

    /// 等値キー以外の ON 句（residual）を評価する。
    /// 左右の行を連結せずに評価する。
    let private residualMatches (resolve: Prepared) (lrow: SqlValue array) (rrow: SqlValue array) =
        function
        | None -> DbOk true
        | Some p -> Eval.matchesJoinedV resolve lrow rrow p

    /// ON 句・residual の列参照を検査する。
    /// 入力が 0 行でも未知の列は演算子の構築時に NameError にする。
    let private checkExprs (leftSchema: Schema) (rightSchema: Schema) (exprs: Expr list) =
        Eval.checkPrepared (Eval.prepare (Schema.concat leftSchema rightSchema) exprs)

    /// residual（等値キー以外の ON 句）の列参照を検査する。
    let private checkResidual (leftSchema: Schema) (rightSchema: Schema) =
        function
        | None -> Ok()
        | Some p -> checkExprs leftSchema rightSchema [ p ]

    /// nested loop 結合の内側ループ（1つの左行 × 右の全行）を回す共通処理。
    /// `resolve` は呼び出し側で 1回だけ準備する（左行ごとに作り直さない）。
    /// onExhausted matched は右を最後まで試し終えたときの Op。
    /// onError error matched は ON 句の評価が失敗したときの Op。
    /// matched は「この左行で既に一致を出したか」。
    /// INNER はどちらも matched を無視し、LEFT だけが NULL 拡張と継続を使い分ける。
    /// 結合述語を左右の行へ評価する。
    let private joinPred (resolve: Prepared) (on: Expr) (l: SqlValue array) (r: SqlValue array) =
        Eval.matchesJoinedV resolve l r on

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

    /// 左行を右幅まで NULL で埋める。
    let private joinPadded (rightWidth: int) (lrow: SqlValue array) = padWithNulls lrow rightWidth

    /// 結合キーで右側を引く走査（`joinProbe` の左行側）。
    let rec private joinProbeLoop
        (isLeft: bool)
        (leftKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (keyFor: SqlValue array -> Result<'Key, DbError>)
        (lookupRows: 'Key -> Result<Op, DbError>)
        (cur: Op)
        =
        Op.pull
            (fun lrow lnext ->
                if hasNullKey lrow leftKeys then
                    if isLeft then
                        OpStep.OpRow(
                            joinPadded rightWidth lrow,
                            joinProbeLoop isLeft leftKeys rightWidth resolve residual keyFor lookupRows lnext
                        )
                    else
                        Op.next (joinProbeLoop isLeft leftKeys rightWidth resolve residual keyFor lookupRows lnext)
                else
                    match keyFor lrow with
                    | Error e -> OpStep.OpError e
                    | Ok key ->
                        match lookupRows key with
                        | Error e -> OpStep.OpError e
                        | Ok rows ->
                            Op.next (
                                joinProbeEmit
                                    isLeft
                                    leftKeys
                                    rightWidth
                                    resolve
                                    residual
                                    keyFor
                                    lookupRows
                                    lrow
                                    lnext
                                    rows
                                    false
                            ))
            cur

    /// 結合キーで引いた右行を評価する（`joinProbe` の右行側）。
    and private joinProbeEmit
        (isLeft: bool)
        (leftKeys: int array)
        (rightWidth: int)
        (resolve: Prepared)
        (residual: Expr option)
        (keyFor: SqlValue array -> Result<'Key, DbError>)
        (lookupRows: 'Key -> Result<Op, DbError>)
        (lrow: SqlValue array)
        (lnext: Op)
        (rows: Op)
        (matched: bool)
        =
        Op.compute (fun () ->
            match Op.next rows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone ->
                if isLeft && not matched then
                    OpStep.OpRow(
                        joinPadded rightWidth lrow,
                        joinProbeLoop isLeft leftKeys rightWidth resolve residual keyFor lookupRows lnext
                    )
                else
                    Op.next (joinProbeLoop isLeft leftKeys rightWidth resolve residual keyFor lookupRows lnext)
            | OpStep.OpRow(rrow, rest) ->
                match residualMatches resolve lrow rrow residual with
                | DbErr e -> OpStep.OpError e
                | DbOk false ->
                    Op.next (
                        joinProbeEmit
                            isLeft
                            leftKeys
                            rightWidth
                            resolve
                            residual
                            keyFor
                            lookupRows
                            lrow
                            lnext
                            rest
                            matched
                    )
                | DbOk true ->
                    OpStep.OpRow(
                        Array.append lrow rrow,
                        joinProbeEmit
                            isLeft
                            leftKeys
                            rightWidth
                            resolve
                            residual
                            keyFor
                            lookupRows
                            lrow
                            lnext
                            rest
                            true
                    ))

    /// ハッシュ結合 / インデックス入れ子ループの共通ループ。
    /// `keyFor` は左行からルックアップ用のキーを作る
    /// （ハッシュ結合は符号化したバイト列、インデックス探索は索引列の値）。
    /// lookupRows は左キーから右の行を返す（空の Op でもよい）。
    /// <remarks>
    /// Op.compute の継続内の相互再帰は「次の行を要求されたとき」に 1段ずつ進むので、スタックは積み上がらない。
    /// </remarks>
    let private joinProbe
        (isLeft: bool)
        (leftSchema: Schema)
        (left: Op)
        (leftKeys: int array)
        (rightSchema: Schema)
        (residual: Expr option)
        (keyFor: SqlValue array -> Result<'Key, DbError>)
        (lookupRows: 'Key -> Result<Op, DbError>)
        =
        let rightWidth = rightSchema.Columns.Length

        let resolve =
            Eval.prepare (Schema.concat leftSchema rightSchema) (residualExprs residual)

        joinProbeLoop isLeft leftKeys rightWidth resolve residual keyFor lookupRows left

    /// ハッシュ表の 1バケットへ行を追加する。
    let private hashAdd
        (buckets: System.Collections.Generic.Dictionary<byte array, SqlValue array list>)
        (key: byte array)
        (rrow: SqlValue array)
        =
        match buckets.TryGetValue key with
        | true, bucket -> buckets.[key] <- rrow :: bucket
        | _ -> buckets.[key] <- [ rrow ]

    /// 右側のハッシュ表を作る（NULL キーは登録しない）。
    /// キーは順序保持符号化のバイト列（`encodeKey`）なので、
    /// `SqlValue` の構造的等価と SQL の比較が食い違う値（NaN など）も正しく一致する。
    /// バケットは右入力の順序を保つ（マージ結合と同じ出力順にするため）。
    let private buildHashTable (rightRows: SqlValue array array) (rightKeys: int array) =
        let buckets =
            System.Collections.Generic.Dictionary<byte array, SqlValue array list> HashIdentity.Structural

        rightRows
        |> foldOk
            (fun () rrow ->
                if hasNullKey rrow rightKeys then
                    OkUnit
                else
                    match encodeKey rrow rightKeys with
                    | Error e -> err e
                    | Ok key ->
                        hashAdd buckets key rrow
                        OkUnit)
            ()
        >>= fun () ->
            let table =
                System.Collections.Generic.Dictionary<byte array, SqlValue array array> HashIdentity.Structural

            for kv in buckets do
                table.[kv.Key] <- kv.Value |> List.rev |> List.toArray

            ret table

    let private lookupHash
        (table: System.Collections.Generic.Dictionary<byte array, SqlValue array array>)
        (key: byte array)
        =
        match table.TryGetValue key with
        | true, bucket -> ret (Op.ofArray bucket)
        | _ -> ret Op.Empty

    /// 左 build 版ハッシュ結合の probe（右行を 1つずつ進める）。
    let rec private buildLeftProbe
        (right: Op)
        (rightKeys: int array)
        (table: System.Collections.Generic.Dictionary<byte array, SqlValue array array>)
        (resolve: Prepared)
        (residual: Expr option)
        (cur: Op)
        =
        Op.pull
            (fun rrow rnext ->
                if hasNullKey rrow rightKeys then
                    Op.next (buildLeftProbe right rightKeys table resolve residual rnext)
                else
                    match encodeKey rrow rightKeys with
                    | Error e -> OpStep.OpError e
                    | Ok key ->
                        match lookupHash table key with
                        | Error e -> OpStep.OpError e
                        | Ok rows -> Op.next (buildLeftEmit right rightKeys table resolve residual rrow rnext rows))
            cur

    /// 左 build 版ハッシュ結合の一致行を出す。
    and private buildLeftEmit
        (right: Op)
        (rightKeys: int array)
        (table: System.Collections.Generic.Dictionary<byte array, SqlValue array array>)
        (resolve: Prepared)
        (residual: Expr option)
        (rrow: SqlValue array)
        (rnext: Op)
        (rows: Op)
        =
        Op.compute (fun () ->
            match Op.next rows with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone -> Op.next (buildLeftProbe right rightKeys table resolve residual rnext)
            | OpStep.OpRow(lrow, rest) ->
                match residualMatches resolve lrow rrow residual with
                | DbErr e -> OpStep.OpError e
                | DbOk false -> Op.next (buildLeftEmit right rightKeys table resolve residual rrow rnext rest)
                | DbOk true ->
                    OpStep.OpRow(
                        Array.append lrow rrow,
                        buildLeftEmit right rightKeys table resolve residual rrow rnext rest
                    ))

    /// 左をハッシュ表にした INNER ハッシュ結合の probe。
    /// 右行ごとに左の一致行を引き、`左行 ++ 右行` の順で返す（列順は通常の JOIN と同じ）。
    let private joinProbeBuildLeft
        (leftSchema: Schema)
        (rightSchema: Schema)
        (right: Op)
        (rightKeys: int array)
        (residual: Expr option)
        (table: System.Collections.Generic.Dictionary<byte array, SqlValue array array>)
        =
        let resolve =
            Eval.prepare (Schema.concat leftSchema rightSchema) (residualExprs residual)

        buildLeftProbe right rightKeys table resolve residual right

    /// 結合結果が尽きたら cleanup を呼ぶ再帰本体。
    let rec private onExhaustedGo (cleanup: unit -> unit) (cur: Op) =
        Op.compute (fun () ->
            match Op.next cur with
            | OpStep.OpDone ->
                cleanup ()
                OpStep.OpDone
            | OpStep.OpError e ->
                cleanup ()
                OpStep.OpError e
            | OpStep.OpRow(row, nxt) -> OpStep.OpRow(row, onExhaustedGo cleanup nxt))

    /// 結合結果が尽きたら `cleanup` を呼ぶ（退避したファイルを確実に消す）。
    let private onExhausted (cleanup: unit -> unit) = onExhaustedGo cleanup

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

    let private compareKeyRows (keys: int array) (a: SqlValue array) (b: SqlValue array) = compareKeyRowsAt keys a b 0

    /// マージ結合フォールバック用に、パーティションの run をキー順に外部ソートする。
    /// 退避に使うメモリは作業メモリを超えないようにする。
    let private sortByJoinKey (budget: MemoryBudget) (keys: int array) (src: Op) =
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
                            Spill.closeSink sink
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
    let private mergeJoinWith
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

    /// パーティション分割用に結合キーを順序保持符号化する。
    let private hashKeyBytes (keys: int array) (row: SqlValue array) = encodeKey row keys

    /// 退避した spool をスコープへ登録して台帳に積む。
    let private hashRegister
        (budget: MemoryBudget)
        (spools: System.Collections.Generic.List<Spill.RowSpool>)
        (spool: Spill.RowSpool)
        =
        Spill.registerSpool budget.Scope spool
        spools.Add spool
        spool

    /// 再分割の深さ上限。
    [<Literal>]
    let private MaxPartitionDepth = 4

    /// 再分割の総数の上限（1回で両側 16本ずつ作る）。
    [<Literal>]
    let private MaxPartitionSplits = 128

    /// 片側をパーティションへ分割し、spool として登録する。
    let private hashPartitionSide
        (budget: MemoryBudget)
        (stamp: string)
        (spools: System.Collections.Generic.List<Spill.RowSpool>)
        (serial: int)
        (keys: int array)
        (src: Op)
        =
        Spill.openPartitions budget.Directory stamp serial Spill.Partitions
        >>= fun writer ->
            match Spill.fillPartitions writer (hashKeyBytes keys) (fun _ row -> row) 0L src with
            | Error e ->
                writer.Abandon()
                err e
            | Ok _ ->
                writer.Close()

                writer.Paths
                |> Array.map (fun p -> hashRegister budget spools (Spill.RowSpool.Disk([ p ], 0)))
                |> ret

    /// 1つの spool を salt 付きで再分割する。
    let private hashRepartitionOne
        (budget: MemoryBudget)
        (stamp: string)
        (spools: System.Collections.Generic.List<Spill.RowSpool>)
        (salt: uint64)
        (side: string)
        (keys: int array)
        =
        function
        | Spill.RowSpool.Memory rows -> ret [| Spill.RowSpool.Memory rows |]
        | Spill.RowSpool.Disk([ path ], _) ->
            let callStamp = System.Guid.NewGuid().ToString "N"

            Spill.repartitionRun
                budget.Directory
                (sprintf "%s-%s-%s" stamp side callStamp)
                0
                Spill.Partitions
                salt
                (hashKeyBytes keys)
                path
            |> Result.map (
                List.map (fun p -> hashRegister budget spools (Spill.RowSpool.Disk([ p ], 0)))
                >> Array.ofList
            )
        | Spill.RowSpool.Disk _ -> err (DbError.internalError "hash join: パーティションが複数の run です")

    /// パーティションを明示スタックで 1つずつ処理する（末尾再帰）。
    [<TailCall>]
    let rec private hashJoinWork
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftSchema: Schema)
        (leftKeys: int array)
        (rightSchema: Schema)
        (rightKeys: int array)
        (residual: Expr option)
        (spools: System.Collections.Generic.List<Spill.RowSpool>)
        (stamp: string)
        (splits: int)
        (acc: Op list)
        (work: (int * uint64 * Spill.RowSpool * Spill.RowSpool) list)
        =
        match work with
        | [] -> ret (List.foldBack Op.append (List.rev acc) Op.Empty)
        | (depth, salt, lspool, rspool) :: rest ->
            match Spill.reopenKeep budget.Scope rspool >>= Spill.spool budget with
            | Ok(Spill.RowSpool.Memory rightRows) ->
                match buildHashTable rightRows rightKeys with
                | Ok table ->
                    Spill.forgetSpool budget.Scope rspool
                    Spill.deleteSpool rspool

                    match Spill.reopen budget.Scope lspool with
                    | Ok l ->
                        let op =
                            joinProbe
                                isLeft
                                leftSchema
                                l
                                leftKeys
                                rightSchema
                                residual
                                (fun row -> encodeKey row leftKeys)
                                (lookupHash table)

                        hashJoinWork
                            isLeft
                            budget
                            leftSchema
                            leftKeys
                            rightSchema
                            rightKeys
                            residual
                            spools
                            stamp
                            splits
                            (op :: acc)
                            rest
                    | Error e -> err e
                | Error e -> err e
            | Ok rightSpool when depth < MaxPartitionDepth && splits < MaxPartitionSplits ->
                Spill.forgetSpool budget.Scope rightSpool
                Spill.deleteSpool rightSpool

                match hashRepartitionOne budget stamp spools salt "l" leftKeys lspool with
                | Ok lparts ->
                    match hashRepartitionOne budget stamp spools salt "r" rightKeys rspool with
                    | Ok rparts ->
                        let next =
                            Array.map2 (fun l r -> depth + 1, salt + 1UL, l, r) lparts rparts
                            |> Array.toList

                        hashJoinWork
                            isLeft
                            budget
                            leftSchema
                            leftKeys
                            rightSchema
                            rightKeys
                            residual
                            spools
                            stamp
                            (splits + 1)
                            acc
                            (next @ rest)
                    | Error e -> err e
                | Error e -> err e
            | Ok rightSpool ->
                Spill.forgetSpool budget.Scope rightSpool
                Spill.deleteSpool rightSpool

                match Spill.reopen budget.Scope lspool with
                | Ok l ->
                    match sortByJoinKey budget leftKeys l with
                    | Ok sortedLeft ->
                        match Spill.reopen budget.Scope rspool with
                        | Ok r ->
                            match sortByJoinKey budget rightKeys r with
                            | Ok sortedRight ->
                                let op =
                                    mergeJoinWith
                                        isLeft
                                        budget
                                        leftSchema
                                        sortedLeft
                                        leftKeys
                                        rightSchema
                                        sortedRight
                                        rightKeys
                                        residual

                                hashJoinWork
                                    isLeft
                                    budget
                                    leftSchema
                                    leftKeys
                                    rightSchema
                                    rightKeys
                                    residual
                                    spools
                                    stamp
                                    splits
                                    (op :: acc)
                                    rest
                            | Error e -> err e
                        | Error e -> err e
                    | Error e -> err e
                | Error e -> err e
            | Error e -> err e

    /// ハッシュ結合の外部版。
    /// 両側を結合キーで `Spill.Partitions` 本に分割し、
    /// 各パーティションの右をハッシュ表にして左を probe する。
    /// 上限を超えるパーティションは salt を変えて再分割し、
    /// 深さ上限でマージ結合へフォールバックする。
    /// <remarks>
    /// 出力順はパーティション順（ORDER BY が無い限り SQL は順序を保証しない）。
    /// 一時ファイルは `Spill.registerSpool` で `ScanScope` に登録する。
    /// `joinWork` は明示スタックで 1パーティションずつ処理する。
    /// </remarks>
    let private hashJoinPartitioned
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
        let stamp = System.Guid.NewGuid().ToString "N"
        let spools = System.Collections.Generic.List<Spill.RowSpool>()

        hashPartitionSide budget stamp spools 0 leftKeys left
        >>= fun lparts ->
            hashPartitionSide budget stamp spools Spill.Partitions rightKeys right
            >>= fun rparts ->
                let work = Array.map2 (fun l r -> 0, 1UL, l, r) lparts rparts |> Array.toList

                hashJoinWork isLeft budget leftSchema leftKeys rightSchema rightKeys residual spools stamp 0 [] work
                |> Result.map (
                    onExhausted (fun () ->
                        for sp in spools do
                            Spill.forgetSpool budget.Scope sp
                            Spill.deleteSpool sp)
                )

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

    /// INNER JOIN（ハッシュ結合）。右側をハッシュ表にする。
    let innerJoinHash
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
        | Ok() ->
            Spill.spool budget right
            >>= function
                | Spill.RowSpool.Memory rightRows ->
                    buildHashTable rightRows rightKeys
                    >>= fun table ->
                        ret (
                            joinProbe
                                false
                                leftSchema
                                left
                                leftKeys
                                rightSchema
                                residual
                                (fun row -> encodeKey row leftKeys)
                                (lookupHash table)
                        )
                | spool ->
                    Spill.reopen budget.Scope spool
                    >>= fun rightRows ->
                        hashJoinPartitioned
                            false
                            budget
                            leftSchema
                            left
                            leftKeys
                            rightSchema
                            rightRows
                            rightKeys
                            residual

    /// INNER JOIN（ハッシュ結合）。左側をハッシュ表にし、右を 1回だけ流す。
    /// 左が小さい場合のメモリ使用量を右のサイズから切り離す。
    /// 左が上限を超えて退避したときは、右を build する通常の経路へフォールバックする。
    let innerJoinHashBuildLeft
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
        | Ok() ->
            Spill.spool budget left
            >>= function
                | Spill.RowSpool.Memory leftRows ->
                    buildHashTable leftRows leftKeys
                    >>= fun table -> ret (joinProbeBuildLeft leftSchema rightSchema right rightKeys residual table)
                | spool ->
                    Spill.reopen budget.Scope spool
                    >>= fun leftRows ->
                        innerJoinHash budget leftSchema leftRows leftKeys rightSchema right rightKeys residual

    /// LEFT JOIN（ハッシュ結合）。
    let leftJoinHash
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
        | Ok() ->
            Spill.spool budget right
            >>= function
                | Spill.RowSpool.Memory rightRows ->
                    buildHashTable rightRows rightKeys
                    >>= fun table ->
                        ret (
                            joinProbe
                                true
                                leftSchema
                                left
                                leftKeys
                                rightSchema
                                residual
                                (fun row -> encodeKey row leftKeys)
                                (lookupHash table)
                        )
                | spool ->
                    Spill.reopen budget.Scope spool
                    >>= fun rightRows ->
                        hashJoinPartitioned
                            true
                            budget
                            leftSchema
                            left
                            leftKeys
                            rightSchema
                            rightRows
                            rightKeys
                            residual

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
