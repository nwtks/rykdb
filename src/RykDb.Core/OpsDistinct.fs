// ===========================================================================
// OpsDistinct — DISTINCT。
// ===========================================================================

namespace RykDb.Core

module internal OpsDistinct =
    /// DISTINCT の符号化キー。
    let private distinctEncode (row: SqlValue array) =
        match KeyBuilder.ofValues (Array.map SqlValue.keyNormalize row) with
        | Error e -> DbErr e
        | Ok key -> DbOk key

    /// DISTINCT の退避行を連番で比較する。
    let private compareDistinctSeq (a: SqlValue array) (b: SqlValue array) =
        match a.[0], b.[0] with
        | SqlValue.Int x, SqlValue.Int y -> compare x y
        | _ -> 0

    /// DISTINCT の退避行に連番を付ける。
    let private distinctFrame (s: int64) (r: SqlValue array) = Array.append [| SqlValue.Int s |] r

    /// DISTINCT の退避行からキーを取り出す。
    let private distinctKeyOfFrame (f: SqlValue array) =
        match f.[0] with
        | SqlValue.Blob b -> ret b
        | SqlValue.Int _ -> distinctEncode (Array.sub f 1 (f.Length - 1)) |> DbResult.toResult
        | _ -> err (DbError.internalError "DISTINCT の退避行が不正です")

    /// DISTINCT のパーティション状態の初期値。
    let private distinctSeed () =
        struct (System.Int64.MaxValue, Array.empty<SqlValue>)

    /// DISTINCT のパーティション状態に 1行を畳み込む。
    let private distinctAddOne struct (best: int64, bestRow: SqlValue array) (f: SqlValue array) =
        match f.[0] with
        | SqlValue.Blob _ -> ret struct (System.Int64.MinValue, [||])
        | SqlValue.Int s ->
            let r = Array.sub f 1 (f.Length - 1)

            if s < best then
                ret struct (s, r)
            else
                ret struct (best, bestRow)
        | _ -> err (DbError.internalError "DISTINCT の退避行が不正です")

    /// DISTINCT のパーティション状態のバイト数。
    let private distinctStateBytes struct (_, r: SqlValue array) =
        int64 (ValueCodec.managedRowBytes r) + 64L

    /// DISTINCT のパーティション状態を出力行へ変換する。
    let private distinctEmitOne struct (best: int64, r: SqlValue array) =
        if best = System.Int64.MinValue then
            ret None
        else
            ret (Some(Array.append [| SqlValue.Int best |] r))

    /// 退避済みキーのマーカー行を 1つ書き出す。
    let private distinctMarker (writer: Spill.PartitionWriter) (k: byte array) =
        writer.Write(Spill.partitionOfKey 0UL k, [| SqlValue.Blob k; SqlValue.Int System.Int64.MinValue |])

    /// 書き込み失敗時に退避ライターを破棄する。
    let private distinctAbandon (writer: Spill.PartitionWriter) (e: DbError) =
        writer.Abandon()
        err e

    /// メモリ上限を超えた分をパーティションへ退避してグループ化する。
    let private distinctExternalize
        (budget: MemoryBudget)
        (seen: System.Collections.Generic.HashSet<byte array>)
        (seq: int64)
        (row: SqlValue array)
        (key: byte array)
        (rest: Op)
        =
        let stamp = System.Guid.NewGuid().ToString "N"

        Spill.openPartitions budget.Directory stamp 0 Spill.Partitions
        >>= fun writer ->
            let written =
                (ret (), seen)
                ||> Seq.fold (fun acc k -> acc >>= fun () -> distinctMarker writer k)
                >>= fun () -> writer.Write(Spill.partitionOfKey 0UL key, distinctFrame seq row)
                >>= fun () ->
                    Spill.fillPartitions writer (distinctEncode >> DbResult.toResult) distinctFrame (seq + 1L) rest

            match written with
            | Error e -> distinctAbandon writer e
            | Ok _ ->
                match writer.Close() with
                | Error e -> distinctAbandon writer e
                | Ok() ->
                    Spill.groupPartitions
                        budget
                        stamp
                        distinctKeyOfFrame
                        distinctSeed
                        distinctAddOne
                        distinctStateBytes
                        distinctEmitOne
                        []
                        (writer.Paths |> Array.toList)
                    >>= fun rows ->
                        ExtSort.sortRows budget () (fun () _ -> DbOk()) compareDistinctSeq rows
                        |> Result.map (Op.map (fun f -> DbOk(Array.sub f 1 (f.Length - 1))))

    /// DISTINCT のストリーミング本体（使用バイト数と連番を明示する）。
    let rec private distinctProbe
        (budget: MemoryBudget)
        (seen: System.Collections.Generic.HashSet<byte array>)
        (used: int64)
        (seq: int64)
        (cur: Op)
        =
        Op.compute (fun () ->
            match Op.next cur with
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpDone -> OpStep.OpDone
            | OpStep.OpRow(row, nxt) ->
                match distinctEncode row with
                | DbErr e -> OpStep.OpError e
                | DbOk key ->
                    if seen.Contains key then
                        Op.next (distinctProbe budget seen used seq nxt)
                    else
                        let size = int64 (ValueCodec.managedRowBytes row) + 32L

                        if MemoryBudget.exceeded (budget.Work, used + size) then
                            match distinctExternalize budget seen seq row key nxt with
                            | Error e -> OpStep.OpError e
                            | Ok op -> Op.next op
                        else
                            seen.Add key |> ignore
                            OpStep.OpRow(row, distinctProbe budget seen (used + size) (seq + 1L) nxt))

    /// 重複除去。
    /// 最初に現れた順序を保つ（SQL の DISTINCT）。
    /// <remarks>
    /// 上限内は符号化キーの `HashSet` でストリーミングし、
    /// 超えたら (連番, 行) をパーティションへ退避し、
    /// キーごとに最小連番の行だけを残して連番順に並べ直す（出現順を保つ）。
    /// 既に出した行はマーカー（`Blob` キー + `Int64.MinValue`）で伝える。
    /// `seen` は呼び出しごとに作る（クエリ間で共有すると後続クエリの行まで重複扱いになる）。
    /// `probe` は `Op.compute` の継続内の再帰で 1行ごとに 1段だけ進む。
    /// </remarks>
    let distinct (budget: MemoryBudget) (src: Op) =
        let seen = System.Collections.Generic.HashSet<byte array> HashIdentity.Structural
        distinctProbe budget seen 0L 0L src
