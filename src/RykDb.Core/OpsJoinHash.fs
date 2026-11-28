// ===========================================================================
// OpsJoinHash — ハッシュ結合（パーティション分割を含む）。
// ===========================================================================

namespace RykDb.Core

open OpsJoinShared
open OpsJoinMerge

module internal OpsJoinHash =
    /// ハッシュ表（キー → 一致行）。1段構築のため ResizeArray をそのまま保持する。
    type private HashTable = System.Collections.Generic.Dictionary<byte array, ResizeArray<SqlValue array>>

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
    let joinProbe
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
    let private hashAdd (table: HashTable) (key: byte array) (rrow: SqlValue array) =
        match table.TryGetValue key with
        | true, bucket -> bucket.Add rrow
        | _ ->
            let bucket = ResizeArray<SqlValue array>()
            bucket.Add rrow
            table.[key] <- bucket

    /// 右側のハッシュ表を作る（NULL キーは登録しない）。
    /// キーは順序保持符号化のバイト列（`encodeKey`）なので、
    /// `SqlValue` の構造的等価と SQL の比較が食い違う値（NaN など）も正しく一致する。
    /// バケットは右入力の順序を保つ（マージ結合と同じ出力順にするため）。
    let private buildHashTable (rightRows: SqlValue array array) (rightKeys: int array) =
        let table =
            System.Collections.Generic.Dictionary<byte array, ResizeArray<SqlValue array>> HashIdentity.Structural

        rightRows
        |> foldOk
            (fun () rrow ->
                if hasNullKey rrow rightKeys then
                    OkUnit
                else
                    match encodeKey rrow rightKeys with
                    | Error e -> err e
                    | Ok key ->
                        hashAdd table key rrow
                        OkUnit)
            ()
        |> Result.map (fun () -> table)

    /// ハッシュ表からキーの行を引く。
    let private lookupHash (table: HashTable) (key: byte array) =
        match table.TryGetValue key with
        | true, bucket -> ret (Op.ofResizeArray bucket)
        | _ -> ret Op.Empty

    /// 左 build 版ハッシュ結合の probe（右行を 1つずつ進める）。
    let rec private buildLeftProbe
        (right: Op)
        (rightKeys: int array)
        (table: HashTable)
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
        (table: HashTable)
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
        (table: HashTable)
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
                match writer.Close() with
                | Error e ->
                    writer.Abandon()
                    err e
                | Ok() ->
                    writer.Paths
                    |> Array.mapi (fun i p ->
                        hashRegister budget spools (Spill.RowSpool.Disk([ p ], 0, writer.BytesWritten.[i])))
                    |> ret

    [<TailCall>]
    let rec private hashPartitionLeftGather
        (budget: MemoryBudget)
        (stamp: string)
        (spools: System.Collections.Generic.List<Spill.RowSpool>)
        (keys: int array)
        (buckets: SqlValue array list array)
        (used: int64)
        (cur: Op)
        =
        match Op.next cur with
        | OpStep.OpError e -> err e
        | OpStep.OpDone ->
            buckets
            |> Array.map (fun rows -> Spill.RowSpool.Memory(List.rev rows |> List.toArray))
            |> ret
        | OpStep.OpRow(row, nxt) ->
            match hashKeyBytes keys row with
            | Error e -> err e
            | Ok key ->
                let size = int64 (ValueCodec.managedRowBytes row) + 32L

                if MemoryBudget.exceeded (budget.Work, used + size) then
                    Spill.openPartitions budget.Directory stamp 0 Spill.Partitions
                    >>= fun writer ->
                        let written =
                            buckets
                            |> Array.mapi (fun i rows ->
                                rows
                                |> List.rev
                                |> List.fold (fun acc r -> acc >>= fun () -> writer.Write(i, r)) (ret ()))
                            |> Array.fold (fun acc step -> acc >>= fun () -> step) (ret ())

                        match written with
                        | Error e ->
                            writer.Abandon()
                            err e
                        | Ok() ->
                            match
                                Spill.fillPartitions writer (hashKeyBytes keys) (fun _ row -> row) 0L (Op.emit row nxt)
                            with
                            | Error e ->
                                writer.Abandon()
                                err e
                            | Ok _ ->
                                match writer.Close() with
                                | Error e ->
                                    writer.Abandon()
                                    err e
                                | Ok() ->
                                    writer.Paths
                                    |> Array.mapi (fun i p ->
                                        hashRegister
                                            budget
                                            spools
                                            (Spill.RowSpool.Disk([ p ], 0, writer.BytesWritten.[i])))
                                    |> ret
                else
                    let p = Spill.partitionOfKey 0UL key
                    buckets.[p] <- row :: buckets.[p]
                    hashPartitionLeftGather budget stamp spools keys buckets (used + size) nxt

    /// 左側をパーティションへ分割する。
    /// まずメモリ上の16バケットへ溜め、作業メモリに収まれば run を作らずに返す。
    /// 超えた時点で溜めた行を `PartitionWriter` へ書き出し、残りを既存経路で分割する
    /// （`Op` は one-shot なので、事前判定のために読み切ってからやり直すことはできない）。
    let private hashPartitionLeft
        (budget: MemoryBudget)
        (stamp: string)
        (spools: System.Collections.Generic.List<Spill.RowSpool>)
        (keys: int array)
        (src: Op)
        =
        let buckets = Array.init Spill.Partitions (fun _ -> [])
        hashPartitionLeftGather budget stamp spools keys buckets 0L src

    [<TailCall>]
    let rec private splitMemory (salt: uint64) (keys: int array) (buckets: SqlValue array list array) =
        function
        | [] ->
            buckets
            |> Array.map (fun rs -> Spill.RowSpool.Memory(List.rev rs |> List.toArray))
            |> ret
        | row :: rest ->
            match hashKeyBytes keys row with
            | Error e -> err e
            | Ok key ->
                let p = Spill.partitionOfKey salt key
                buckets.[p] <- row :: buckets.[p]
                splitMemory salt keys buckets rest

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
        | Spill.RowSpool.Memory rows ->
            let buckets = Array.init Spill.Partitions (fun _ -> [])
            splitMemory salt keys buckets (rows |> Array.toList)
        | Spill.RowSpool.Disk([ path ], _, _) ->
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
                List.map (fun (p, bytes) -> hashRegister budget spools (Spill.RowSpool.Disk([ p ], 0, bytes)))
                >> Array.ofList
            )
        | Spill.RowSpool.Disk _ -> err (DbError.internalError "hash join: パーティションが複数の run です")

    /// パーティション 1組の処理結果。
    type private HashJoinStep =
        /// ハッシュ結合の Op を作れた。
        | HashJoinEmit of Op
        /// 再分割したパーティション（深さ, salt, 左, 右）。
        | HashJoinSplit of (int * uint64 * Spill.RowSpool * Spill.RowSpool) list
        /// 深さ上限に達したのでマージ結合へフォールバックした。
        | HashJoinMergeFallback of Op

    /// 左右のパーティションを salt を変えて再分割する。
    let private hashSplitBoth budget stamp spools salt depth leftKeys rightKeys lspool rspool =
        match hashRepartitionOne budget stamp spools salt "l" leftKeys lspool with
        | Ok lparts ->
            match hashRepartitionOne budget stamp spools salt "r" rightKeys rspool with
            | Ok rparts ->
                let next =
                    Array.map2 (fun l r -> depth + 1, salt + 1UL, l, r) lparts rparts
                    |> Array.toList

                ret (HashJoinSplit next)
            | Error e -> err e
        | Error e -> err e

    /// パーティション 1組を処理する。
    /// メモリに載ればハッシュ表を作って probe し、超過なら再分割、深さ上限ならマージ結合へ落とす。
    let private hashJoinStep
        (isLeft: bool)
        (budget: MemoryBudget)
        (leftSchema: Schema)
        (leftKeys: int array)
        (rightSchema: Schema)
        (rightKeys: int array)
        (residual: Expr option)
        (spools: System.Collections.Generic.List<Spill.RowSpool>)
        (stamp: string)
        (depth: int)
        (salt: uint64)
        (canSplit: bool)
        (lspool: Spill.RowSpool)
        (rspool: Spill.RowSpool)
        =
        if canSplit && MemoryBudget.exceeded (budget.Work, Spill.spoolBytes rspool) then
            hashSplitBoth budget stamp spools salt depth leftKeys rightKeys lspool rspool
        else
            match Spill.loadSpoolLimited budget.Work rspool with
            | Ok(Some rightRows) ->
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

                        ret (HashJoinEmit op)
                    | Error e -> err e
                | Error e -> err e
            | Ok None when canSplit -> hashSplitBoth budget stamp spools salt depth leftKeys rightKeys lspool rspool
            | Ok None ->
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

                                ret (HashJoinMergeFallback op)
                            | Error e -> err e
                        | Error e -> err e
                    | Error e -> err e
                | Error e -> err e
            | Error e -> err e

    /// パーティションを 1つずつ遅延処理する。
    /// 先頭の 1 行を返す前に全パーティションのハッシュ表を作らない
    /// （LIMIT や早期終了で残りを構築しない）。
    /// 再帰は `Op.compute` の継続内で 1パーティションずつ進む（スタックは積まれない）。
    let rec private hashJoinWorkOp
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
        (work: (int * uint64 * Spill.RowSpool * Spill.RowSpool) list)
        =
        Op.compute (fun () ->
            match work with
            | [] -> OpStep.OpDone
            | (depth, salt, lspool, rspool) :: rest ->
                let canSplit = depth < MaxPartitionDepth && splits < MaxPartitionSplits

                match
                    hashJoinStep
                        isLeft
                        budget
                        leftSchema
                        leftKeys
                        rightSchema
                        rightKeys
                        residual
                        spools
                        stamp
                        depth
                        salt
                        canSplit
                        lspool
                        rspool
                with
                | Error e -> OpStep.OpError e
                | Ok(HashJoinEmit op) ->
                    Op.next (
                        Op.append
                            op
                            (hashJoinWorkOp
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
                                rest)
                    )
                | Ok(HashJoinSplit next) ->
                    Op.next (
                        hashJoinWorkOp
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
                            (next @ rest)
                    )
                | Ok(HashJoinMergeFallback op) ->
                    Op.next (
                        Op.append
                            op
                            (hashJoinWorkOp
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
                                rest)
                    ))

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

        hashPartitionLeft budget stamp spools leftKeys left
        >>= fun lparts ->
            hashPartitionSide budget stamp spools Spill.Partitions rightKeys right
            >>= fun rparts ->
                let work = Array.map2 (fun l r -> 0, 1UL, l, r) lparts rparts |> Array.toList

                ret (
                    hashJoinWorkOp isLeft budget leftSchema leftKeys rightSchema rightKeys residual spools stamp 0 work
                    |> onExhausted (fun () ->
                        for sp in spools do
                            Spill.forgetSpool budget.Scope sp
                            Spill.deleteSpool sp)
                )


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
                    Spill.forgetSpool budget.Scope spool

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
                    Spill.forgetSpool budget.Scope spool

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
                    Spill.forgetSpool budget.Scope spool

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
