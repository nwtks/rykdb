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

    /// nested loop 結合の内側ループ（1つの左行 × 右の全行）を回す共通処理。
    /// `resolve` は呼び出し側で 1回だけ準備する（左行ごとに作り直さない）。
    /// onExhausted matched は右を最後まで試し終えたときの Op。
    /// onError error matched は ON 句の評価が失敗したときの Op。
    /// matched は「この左行で既に一致を出したか」。
    /// INNER はどちらも matched を無視し、LEFT だけが NULL 拡張と継続を使い分ける。
    let private joinLoop
        (onExhausted: bool -> Op)
        (onError: DbError -> bool -> Op)
        (lrow: SqlValue array)
        (rightRows: SqlValue array array)
        (resolve: Prepared)
        (on: Expr)
        =
        let joinPred l r = Eval.matchesJoinedV resolve l r on

        let rec loop matched i =
            Op.compute (fun () -> advance matched i)

        and advance matched j =
            if j >= rightRows.Length then
                Op.next (onExhausted matched)
            else
                let rrow = rightRows.[j]

                match joinPred lrow rrow with
                | DbOk true -> OpStep.OpRow(Array.append lrow rrow, loop true (j + 1))
                | DbOk false -> advance matched (j + 1)
                | DbErr e -> Op.next (onError e matched)

        loop false 0

    let private innerJoinArrays
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (rightRows: SqlValue array array)
        (on: Expr)
        =
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]

        let rec joinRows cur =
            Op.pull
                (fun lrow lnext ->
                    Op.next (
                        joinLoop
                            (fun _ -> joinRows lnext)
                            (fun e _ -> Op.compute (fun () -> OpStep.OpError e))
                            lrow
                            rightRows
                            resolve
                            on
                    ))
                cur

        joinRows left

    let private leftJoinArrays
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (rightRows: SqlValue array array)
        (on: Expr)
        =
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]

        let rec joinRows width cur =
            Op.pull
                (fun lrow lnext ->
                    let onExhausted matched =
                        if matched then
                            joinRows width lnext
                        else
                            Op.emit (padWithNulls lrow width) (joinRows width lnext)

                    let onError e _ = Op.compute (fun () -> OpStep.OpError e)
                    Op.next (joinLoop onExhausted onError lrow rightRows resolve on))
                cur

        joinRows rightSchema.Columns.Length left

    /// 右側を退避したときの入れ子ループ。
    /// 左行ごとに run を先頭から読み直す（I/O は O(左行数 × 右行数) になるが、メモリは一定）。
    /// <remarks>
    /// `joinRows` / `scan` は `Op.compute` の継続内で呼ぶので、1行ごとに 1段だけ進み、
    /// スタックは積み上がらない。
    /// </remarks>
    let private innerJoinSpilled
        (budget: MemoryBudget)
        (spool: Spill.RowSpool)
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (on: Expr)
        =
        let resolve = Eval.prepare (Schema.concat leftSchema rightSchema) [ on ]

        let rec joinRows cur =
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
                    | Ok rows -> Op.next (scan lrow lnext rows))

        and scan lrow lnext rows =
            Op.compute (fun () ->
                match Op.next rows with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone -> Op.next (joinRows lnext)
                | OpStep.OpRow(rrow, rest) ->
                    match Eval.matchesJoinedV resolve lrow rrow on with
                    | DbErr e -> OpStep.OpError e
                    | DbOk true -> OpStep.OpRow(Array.append lrow rrow, scan lrow lnext rest)
                    | DbOk false -> Op.next (scan lrow lnext rest))

        joinRows left

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

        let rec joinRows cur =
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
                    | Ok rows -> Op.next (scan lrow lnext rows false))

        and scan lrow lnext rows matched =
            Op.compute (fun () ->
                match Op.next rows with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone ->
                    if matched then
                        Op.next (joinRows lnext)
                    else
                        OpStep.OpRow(padWithNulls lrow width, joinRows lnext)
                | OpStep.OpRow(rrow, rest) ->
                    match Eval.matchesJoinedV resolve lrow rrow on with
                    | DbErr e -> OpStep.OpError e
                    | DbOk true -> OpStep.OpRow(Array.append lrow rrow, scan lrow lnext rest true)
                    | DbOk false -> Op.next (scan lrow lnext rest matched))

        joinRows left

    /// INNER JOIN（nested loop）。
    /// 右側は内側ループごとに先頭から引き直す。
    /// 一致が無い左行は捨て、ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let innerJoin (budget: MemoryBudget) (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        Spill.spool budget right
        >>= function
            | Spill.RowSpool.Memory rightRows -> ret (innerJoinArrays leftSchema left rightSchema rightRows on)
            | spool -> ret (innerJoinSpilled budget spool leftSchema left rightSchema on)

    /// LEFT JOIN。
    /// 左の行に右が 1行も無ければ NULL で埋める。
    /// ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let leftJoin (budget: MemoryBudget) (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        Spill.spool budget right
        >>= function
            | Spill.RowSpool.Memory rightRows -> ret (leftJoinArrays leftSchema left rightSchema rightRows on)
            | spool -> ret (leftJoinSpilled budget spool leftSchema left rightSchema on)

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
        let padded lrow = padWithNulls lrow rightWidth

        let resolve =
            Eval.prepare (Schema.concat leftSchema rightSchema) (residualExprs residual)

        let rec probe cur =
            Op.pull
                (fun lrow lnext ->
                    if hasNullKey lrow leftKeys then
                        if isLeft then
                            OpStep.OpRow(padded lrow, probe lnext)
                        else
                            Op.next (probe lnext)
                    else
                        match keyFor lrow with
                        | Error e -> OpStep.OpError e
                        | Ok key ->
                            match lookupRows key with
                            | Error e -> OpStep.OpError e
                            | Ok rows -> Op.next (emit lrow lnext rows false))
                cur

        and emit lrow lnext rows matched =
            Op.compute (fun () ->
                match Op.next rows with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone ->
                    if isLeft && not matched then
                        OpStep.OpRow(padded lrow, probe lnext)
                    else
                        Op.next (probe lnext)
                | OpStep.OpRow(rrow, rest) ->
                    match residualMatches resolve lrow rrow residual with
                    | DbErr e -> OpStep.OpError e
                    | DbOk false -> Op.next (emit lrow lnext rest matched)
                    | DbOk true -> OpStep.OpRow(Array.append lrow rrow, emit lrow lnext rest true))

        probe left

    /// 右側のハッシュ表を作る（NULL キーは登録しない）。
    /// キーは順序保持符号化のバイト列（`encodeKey`）なので、
    /// `SqlValue` の構造的等価と SQL の比較が食い違う値（NaN など）も正しく一致する。
    /// バケットは右入力の順序を保つ（マージ結合と同じ出力順にするため）。
    let private buildHashTable (rightRows: SqlValue array array) (rightKeys: int array) =
        let buckets =
            System.Collections.Generic.Dictionary<byte array, SqlValue array list> HashIdentity.Structural

        let add key rrow =
            match buckets.TryGetValue key with
            | true, bucket -> buckets.[key] <- rrow :: bucket
            | _ -> buckets.[key] <- [ rrow ]

        rightRows
        |> foldOk
            (fun () rrow ->
                if hasNullKey rrow rightKeys then
                    OkUnit
                else
                    match encodeKey rrow rightKeys with
                    | Error e -> err e
                    | Ok key ->
                        add key rrow
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

    /// 結合結果が尽きたら `cleanup` を呼ぶ（退避したファイルを確実に消す）。
    /// `go` は `Op.compute` の継続内の再帰で、1行ごとに 1段だけ進む。
    let private onExhausted (cleanup: unit -> unit) =
        let rec go cur =
            Op.compute (fun () ->
                match Op.next cur with
                | OpStep.OpDone ->
                    cleanup ()
                    OpStep.OpDone
                | OpStep.OpError e ->
                    cleanup ()
                    OpStep.OpError e
                | OpStep.OpRow(row, nxt) -> OpStep.OpRow(row, go nxt))

        go

    /// マージ結合の入力側の状態。
    /// `MergeStart` はまだ 1行も取り出していない入力、`MergeAt` は現在行と続き。
    type private MergeSide =
        | MergeStart of Op
        | MergeAt of SqlValue array * Op
        | MergeDone

    /// 次の 1行を先読みした状態にする。
    let private pullMergeSide =
        let fromOp op =
            match Op.next op with
            | OpStep.OpError e -> DbErr e
            | OpStep.OpDone -> DbOk MergeDone
            | OpStep.OpRow(row, rest) -> DbOk(MergeAt(row, rest))

        function
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

    /// 結合キーでの行の比較（外部ソート用）。
    /// NULL は最小として並べる（NULL キーは結合しないので順序は任意）。
    /// 比較できない値（Unloaded・型不一致）は 0 を返して並びを保つ。
    /// エラー自体はマージ結合の `compareKeyVectors` が返す。
    let private compareKeyRows (keys: int array) (a: SqlValue array) (b: SqlValue array) =
        let rec compareAt i =
            if i >= keys.Length then
                0
            else
                match a.[keys.[i]], b.[keys.[i]] with
                | SqlValue.Null, SqlValue.Null -> compareAt (i + 1)
                | SqlValue.Null, _ -> -1
                | _, SqlValue.Null -> 1
                | x, y ->
                    match SqlValue.compareValuesV x y with
                    | DbOk 0 -> compareAt (i + 1)
                    | DbOk c -> c
                    | DbErr _ -> 0

        compareAt 0

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

    /// 等値キーの行をまとめて取り出す（入力はキー昇順）。
    /// 返り値は (グループの行, グループの次の行を含む状態)。
    /// グループも上限つきで退避する（`Spill.RowSpool`）。
    let private takeMergeGroup (budget: MemoryBudget) (keys: int array) (side: MergeSide) =
        let sameKey first row =
            match compareKeyVectors 0 keys first keys row with
            | DbErr e -> err e
            | DbOk 0 -> ret true
            | DbOk _ -> ret false

        let rec fill first buf bufBytes current =
            match pullMergeSide current with
            | DbErr e -> err e
            | DbOk MergeDone -> ret (List.rev buf, None, MergeDone)
            | DbOk(MergeStart _) -> err (DbError.internalError "merge join: 入力状態が不正です")
            | DbOk(MergeAt(row, _) as next) ->
                match sameKey first row with
                | Ok false -> ret (List.rev buf, None, next)
                | Ok true ->
                    let size = int64 (ValueCodec.managedRowBytes row) + 32L

                    if MemoryBudget.exceeded (budget.Work, bufBytes + size) then
                        ret (List.rev buf, Some row, next)
                    else
                        fill first (row :: buf) (bufBytes + size) next
                | Error e -> err e

        let rec writeAll sink rest =
            match rest with
            | [] -> ret ()
            | r :: tail ->
                match Spill.writeRow r sink with
                | Ok() -> writeAll sink tail
                | x -> x

        let rec drain sink first count current =
            match pullMergeSide current with
            | DbErr e -> err e
            | DbOk MergeDone -> ret (count, MergeDone)
            | DbOk(MergeStart _) -> err (DbError.internalError "merge join: 入力状態が不正です")
            | DbOk(MergeAt(row, _) as next) ->
                match sameKey first row with
                | Ok false -> ret (count, next)
                | Ok true ->
                    match Spill.writeRow row sink with
                    | Ok() -> drain sink first (count + 1) next
                    | Error e -> err e
                | Error e -> err e

        match side with
        | MergeStart _
        | MergeDone -> ret (Spill.RowSpool.Memory [||], side)
        | MergeAt(first, _) ->
            let firstBytes = int64 (ValueCodec.managedRowBytes first) + 32L

            match fill first [ first ] firstBytes side with
            | Ok(rows, None, next) -> ret (Spill.RowSpool.Memory(List.toArray rows), next)
            | Ok(rows, Some pending, next) ->
                let path = Spill.runPath budget.Directory (System.Guid.NewGuid().ToString "N") 0

                Spill.openSink path
                >>= fun sink ->
                    let buffered = rows @ [ pending ]

                    match writeAll sink buffered with
                    | Error e ->
                        Spill.closeSink sink
                        Spill.unlink path
                        err e
                    | Ok() ->
                        match drain sink first buffered.Length next with
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
        let padded lrow = padWithNulls lrow rightWidth

        let resolve =
            Eval.prepare (Schema.concat leftSchema rightSchema) (residualExprs residual)

        let drain =
            function
            | MergeStart op
            | MergeAt(_, op) -> Op.filter (fun _ -> DbOk false) op
            | MergeDone -> Op.Empty

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
            | MergeDone, _ -> drain rside
            | _, MergeDone ->
                if not isLeft then
                    drain lside
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
                        match takeMergeGroup budget leftKeys lside with
                        | Error e -> Op.compute (fun () -> OpStep.OpError e)
                        | Ok(lgroup, lnext) ->
                            match takeMergeGroup budget rightKeys rside with
                            | Error e -> Op.compute (fun () -> OpStep.OpError e)
                            | Ok(rgroup, rnext) -> emitGroup lgroup rgroup lnext rnext

        and emitGroup lspool rspool lnext rnext =
            match Spill.reopenKeep budget.Scope lspool with
            | Error e -> Op.compute (fun () -> OpStep.OpError e)
            | Ok lrows -> nextLeft lspool rspool lnext rnext lrows

        and nextLeft lspool rspool lnext rnext lrows =
            Op.compute (fun () ->
                match Op.next lrows with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone ->
                    Spill.forgetSpool budget.Scope lspool
                    Spill.deleteSpool lspool
                    Spill.forgetSpool budget.Scope rspool
                    Spill.deleteSpool rspool
                    Op.next (loop lnext rnext)
                | OpStep.OpRow(lrow, lrest) ->
                    match Spill.reopenKeep budget.Scope rspool with
                    | Error e -> OpStep.OpError e
                    | Ok rrows -> Op.next (nextRight lspool rspool lnext rnext lrow lrest rrows false))

        and nextRight lspool rspool lnext rnext lrow lrest rrows matched =
            Op.compute (fun () ->
                match Op.next rrows with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone ->
                    if isLeft && not matched then
                        OpStep.OpRow(padded lrow, nextLeft lspool rspool lnext rnext lrest)
                    else
                        Op.next (nextLeft lspool rspool lnext rnext lrest)
                | OpStep.OpRow(rrow, rrest) ->
                    match residualMatches resolve lrow rrow residual with
                    | DbErr e -> OpStep.OpError e
                    | DbOk false -> Op.next (nextRight lspool rspool lnext rnext lrow lrest rrest matched)
                    | DbOk true ->
                        OpStep.OpRow(Array.append lrow rrow, nextRight lspool rspool lnext rnext lrow lrest rrest true))

        loop (MergeStart left) (MergeStart right)

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
        let maxDepth = 4

        /// 再分割の総数の上限。
        /// 1回で両側 16本ずつ作るので、これ × 32 本が一時ファイル数の上限になる
        /// （上限に達したら整列マージフォールバックへ回す）。
        let maxSplits = 128

        let keyBytes keys row = encodeKey row keys

        let register spool =
            Spill.registerSpool budget.Scope spool
            spools.Add spool
            spool

        let partitionSide serial keys src =
            Spill.openPartitions budget.Directory stamp serial Spill.Partitions
            >>= fun writer ->
                match Spill.fillPartitions writer (keyBytes keys) (fun _ row -> row) 0L src with
                | Error e ->
                    writer.Abandon()
                    err e
                | Ok _ ->
                    writer.Close()

                    writer.Paths
                    |> Array.map (fun p -> register (Spill.RowSpool.Disk([ p ], 0)))
                    |> ret

        let repartitionOne salt side keys =
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
                    (keyBytes keys)
                    path
                |> Result.map (List.map (fun p -> register (Spill.RowSpool.Disk([ p ], 0))) >> Array.ofList)
            | Spill.RowSpool.Disk _ -> err (DbError.internalError "hash join: パーティションが複数の run です")

        let rec joinWork splits acc =
            function
            | [] -> ret (List.foldBack Op.append (List.rev acc) Op.Empty)
            | (depth, salt, lspool, rspool) :: rest ->
                match Spill.reopenKeep budget.Scope rspool >>= Spill.spool budget with
                | Ok(Spill.RowSpool.Memory rightRows) ->
                    match buildHashTable rightRows rightKeys with
                    | Ok table ->
                        match Spill.reopenKeep budget.Scope lspool with
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

                            joinWork splits (op :: acc) rest
                        | Error e -> err e
                    | Error e -> err e
                | Ok rightSpool when depth < maxDepth && splits < maxSplits ->
                    Spill.forgetSpool budget.Scope rightSpool
                    Spill.deleteSpool rightSpool

                    match repartitionOne salt "l" leftKeys lspool with
                    | Ok lparts ->
                        match repartitionOne salt "r" rightKeys rspool with
                        | Ok rparts ->
                            let next =
                                Array.map2 (fun l r -> depth + 1, salt + 1UL, l, r) lparts rparts
                                |> Array.toList

                            joinWork (splits + 1) acc (next @ rest)
                        | Error e -> err e
                    | Error e -> err e
                | Ok rightSpool ->
                    Spill.forgetSpool budget.Scope rightSpool
                    Spill.deleteSpool rightSpool

                    match Spill.reopenKeep budget.Scope lspool with
                    | Ok l ->
                        match sortByJoinKey budget leftKeys l with
                        | Ok sortedLeft ->
                            match Spill.reopenKeep budget.Scope rspool with
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

                                    joinWork splits (op :: acc) rest
                                | Error e -> err e
                            | Error e -> err e
                        | Error e -> err e
                    | Error e -> err e
                | Error e -> err e

        partitionSide 0 leftKeys left
        >>= fun lparts ->
            partitionSide Spill.Partitions rightKeys right
            >>= fun rparts ->
                let work = Array.map2 (fun l r -> 0, 1UL, l, r) lparts rparts |> Array.toList

                joinWork 0 [] work
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
        ret (mergeJoinWith false budget leftSchema left leftKeys rightSchema right rightKeys residual)

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
        ret (mergeJoinWith true budget leftSchema left leftKeys rightSchema right rightKeys residual)

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
                    hashJoinPartitioned false budget leftSchema left leftKeys rightSchema rightRows rightKeys residual

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
                    hashJoinPartitioned true budget leftSchema left leftKeys rightSchema rightRows rightKeys residual

    /// INNER JOIN（インデックス入れ子ループ）。
    /// 索引検索の結果は materialize せず 1行ずつ流す。
    let innerJoinIndexed leftSchema left leftKeys rightSchema residual lookupRows =
        joinProbe false leftSchema left leftKeys rightSchema residual (fun row -> Ok(keyOf row leftKeys)) lookupRows

    /// LEFT JOIN（インデックス入れ子ループ）。
    let leftJoinIndexed leftSchema left leftKeys rightSchema residual lookupRows =
        joinProbe true leftSchema left leftKeys rightSchema residual (fun row -> Ok(keyOf row leftKeys)) lookupRows
