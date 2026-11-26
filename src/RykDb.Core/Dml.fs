namespace RykDb.Core

module internal RowKey =
    /// 索引の列値を取り出す（索引列の定義順）。
    /// 索引列は CREATE INDEX で検証済みだが、壊れたカタログを黙って落とさず `NameError` にする。
    let indexValues (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.map (TableSchema.requireIndexOf schema)
        |> all
        |> Result.map (fun positions -> positions |> List.map (fun i -> row.[i]))

    /// 主キーの LSM へ入れるキー（内部ホットパス用）。
    let primaryV (schema: TableSchema) (row: SqlValue array) =
        KeyBuilder.ofValues (TableSchema.primaryKeyOf schema row) |> DbResult.ofResult

    /// 主キーの LSM へ入れるキー。
    let primary (schema: TableSchema) (row: SqlValue array) =
        primaryV schema row |> DbResult.toResult

    /// 索引の LSM へ入れるキー（索引列 + 主キー列、内部ホットパス用）。
    let indexKeyV (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        match indexValues schema index row with
        | Error e -> DbErr e
        | Ok values ->
            KeyBuilder.ofIndexAndPrimary values (TableSchema.primaryKeyOf schema row)
            |> DbResult.ofResult

    /// 索引の LSM へ入れるキー（索引列 + 主キー列）。
    let indexKey (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        indexKeyV schema index row |> DbResult.toResult

/// DML — スキーマと transaction を使う上位操作。
/// 1行の書き込みは「主キーへの 1件 + 各索引への 1件」。
/// これらは論理 keyspace が違うだけで物理的には同じ LSM に入り、
/// 1回の WAL バッチで原子的に commit される。
/// 削除は主キー + 全索引に tombstone を書く
/// （書き漏らすと索引経由で消えた行が復活する）。
module internal Dml =
    /// テーブルの keyspace 名（`t:<table>`）。
    let private tableSpace (schema: TableSchema) = Keyspace.table schema.Name

    /// 索引 schema を登録する。
    let private indexSpace (index: IndexSchema) = Keyspace.index index.Name

    /// 索引エントリの値。
    /// 索引列 + 主キーはキーに埋め込んであるので、値は空にする
    /// （行全体の複製を避ける。読み取りはキーから復号する）。
    let private EmptyIndexValue: SqlValue array = Array.empty

    /// UNIQUE 索引の予約キーを transaction に積む。
    let private reserveUniqueValues (txn: Transaction) (schema: TableSchema) (index: IndexSchema) =
        function
        | None -> ret txn
        | Some values ->
            RowKey.indexValues schema index values
            >>= fun keyValues ->
                if keyValues |> List.exists SqlValue.isNull then
                    ret txn
                else
                    KeyBuilder.ofValues keyValues
                    |> Result.map (fun key -> Transaction.reserveUnique index.Name key txn)

    /// 索引エントリのキーを作る（差分反映用の薄いラッパー）。
    let private indexKeyFor (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        RowKey.indexKey schema index row

    /// 旧行と新行の差分を索引へ反映する。
    /// 同じ索引キーなら上書きだけ行い、キーが変わった場合だけ旧キーを削除する。
    let private applyIndexChange
        (txn: Transaction)
        (schema: TableSchema)
        (oldRow: SqlValue array option)
        (newRow: SqlValue array option)
        =
        schema.Indexes
        |> foldOk
            (fun current index ->
                let reserved =
                    if index.Unique then
                        reserveUniqueValues current schema index oldRow
                        >>= fun afterOld -> reserveUniqueValues afterOld schema index newRow
                    else
                        ret current

                let space = indexSpace index

                match oldRow, newRow with
                | None, None -> reserved
                | None, Some row ->
                    reserved
                    >>= fun txn ->
                        indexKeyFor schema index row
                        |> Result.map (fun key -> Transaction.put space key EmptyIndexValue txn)
                | Some row, None ->
                    reserved
                    >>= fun txn ->
                        indexKeyFor schema index row
                        |> Result.map (fun key -> Transaction.remove space key txn)
                | Some oldValue, Some newValue ->
                    reserved
                    >>= fun txn ->
                        indexKeyFor schema index oldValue
                        >>= fun oldKey ->
                            indexKeyFor schema index newValue
                            |> Result.map (fun newKey ->
                                if Bytes.compare oldKey newKey = 0 then
                                    txn
                                else
                                    let removed = Transaction.remove space oldKey txn
                                    Transaction.put space newKey EmptyIndexValue removed))
            txn

    /// 残りの txn スロットを順に出力する遅延 Op。
    let rec private overlayEmitRemaining (slotArr: (byte array * SqlValue array option) array) (i: int) =
        if i >= slotArr.Length then
            Op.Empty
        else
            Op.compute (fun () ->
                match slotArr.[i] with
                | _, Some v -> OpStep.OpRow(v, overlayEmitRemaining slotArr (i + 1))
                | _, None -> Op.next (overlayEmitRemaining slotArr (i + 1)))

    /// store 行と txn スロットをキー順にマージする遅延 Op。
    let rec private overlayMerge
        (keyOf: SqlValue array -> DbResult<byte array>)
        (slotArr: (byte array * SqlValue array option) array)
        (cur: Op)
        (i: int)
        =
        if i >= slotArr.Length then
            cur
        else
            Op.compute (fun () ->
                match Op.next cur with
                | OpStep.OpError e -> OpStep.OpError e
                | OpStep.OpDone -> Op.next (overlayEmitRemaining slotArr i)
                | OpStep.OpRow(row, nxt) ->
                    let sk, sv = slotArr.[i]

                    match keyOf row with
                    | DbErr e -> OpStep.OpError e
                    | DbOk rk ->
                        match Bytes.compare rk sk with
                        | c when c < 0 -> OpStep.OpRow(row, overlayMerge keyOf slotArr nxt i)
                        | 0 ->
                            match sv with
                            | Some v -> OpStep.OpRow(v, overlayMerge keyOf slotArr nxt (i + 1))
                            | None -> Op.next (overlayMerge keyOf slotArr nxt (i + 1))
                        | _ ->
                            match sv with
                            | Some v -> OpStep.OpRow(v, Op.emit row (overlayMerge keyOf slotArr nxt (i + 1)))
                            | None -> OpStep.OpRow(row, overlayMerge keyOf slotArr nxt (i + 1)))

    /// store の行に txn の書き込みを重ねる（ストリーミング）。
    /// <remarks>
    /// store の行も txn のバッファも**キー昇順**なので、両者を突き合わせながら 1行ずつ合成できる。
    /// store 側は 1行だけ、slot 側は txn のバッファぶんだけしか持たない。
    /// slot は 1回ずつしか消費されない（store 行と突き合わせたとき、あるいは store が尽きたとき）。
    /// </remarks>
    let private overlayRows
        (keyOf: SqlValue array -> DbResult<byte array>)
        (slots: (byte array * SqlValue array option) list)
        (rows: Op)
        =
        if List.isEmpty slots then
            rows
        else
            let slotArr =
                slots |> List.sortWith (fun (a, _) (b, _) -> Bytes.compare a b) |> List.toArray

            overlayMerge keyOf slotArr rows 0

    /// store の読み取り上限（txn の削除分を足す）。
    /// 削除スロットは overlay で出力から落ちるので、その分だけ余分に読む。
    let private storeLimitFor (limit: int) (slots: (byte array * SqlValue array option) list) =
        if limit <= 0 then
            0
        else
            let deletions =
                slots |> List.fold (fun n (_, value) -> if value.IsNone then n + 1 else n) 0

            int (min (int64 System.Int32.MaxValue) (int64 limit + int64 deletions))

    /// 主キーのキーが範囲内か。
    let private inKeyRange (lowerBytes: byte array) (upperBytes: byte array option) (key: byte array) =
        Bytes.compare key lowerBytes >= 0
        && match upperBytes with
           | None -> true
           | Some hb -> Bytes.compare key hb <= 0

    /// 索引キーが範囲内か。
    let private inIndexRange (lowerBytes: byte array) (upperBytes: byte array option) (key: byte array) =
        inKeyRange lowerBytes upperBytes key
        || match upperBytes with
           | Some hb -> KeyBuilder.startsWithPrefix key hb
           | None -> false

    /// マスクに列を足したコピーを返す（共有しているマスクを書き換えない）。
    let private widenMask (mask: bool array) (indices: int seq) =
        let copy = Array.copy mask

        for i in indices do
            if i >= 0 && i < copy.Length then
                copy.[i] <- true

        copy

    /// 索引を構成する列の表内位置。
    let private indexColumnIndices (schema: TableSchema) (index: IndexSchema) =
        index.Columns |> List.map (TableSchema.requireIndexOf schema) |> all

    /// 索引キーに含まれる列の表内位置（索引列 + 主キー列の順）。
    let private indexAndPrimaryIndices (schema: TableSchema) (index: IndexSchema) =
        indexColumnIndices schema index
        |> Result.map (fun positions -> positions @ schema.PrimaryKey)

    /// 索引スキャンで使う mask と、本体から fetch する列の mask を返す。
    /// <remarks>
    /// 索引キーには索引列 + 主キーが入っているので、その範囲内だけなら索引スキャンで完結する。
    /// 被覆外の列が必要な場合は、スキャン時に主キーを復号できるよう scanMask に PK を足し、
    /// fetchMask に「必要かつ被覆外」の列を立てる。
    /// </remarks>
    let indexAccessMasks (schema: TableSchema) (index: IndexSchema) (mask: bool array) =
        indexAndPrimaryIndices schema index
        |> Result.map (fun positions ->
            let covered = Array.create schema.Columns.Length false

            positions
            |> List.iter (fun p ->
                if p >= 0 && p < covered.Length then
                    covered.[p] <- true)

            let fetchMask =
                Array.init schema.Columns.Length (fun i -> i < mask.Length && mask.[i] && not covered.[i])

            let needsFetch = fetchMask |> Array.exists id

            let scanMask =
                if needsFetch then
                    widenMask mask schema.PrimaryKey
                else
                    mask

            scanMask, fetchMask)

    /// 走査行の幅を検証して列配列にする。
    let private rowOfFixedWidth (width: int) (row: SqlValue array) =
        if row.Length = width then
            Ok row
        elif row.Length = 0 then
            Ok(Array.create width SqlValue.Unloaded)
        else
            err (DbError.internalError (sprintf "行の幅が列数と一致しません（%d 列、%d 値）" width row.Length))

    /// 走査から 1行ずつ Op を組み立てる（emit 側）。
    [<TailCall>]
    let rec private opOfScanEmit (width: int) (take: SstEntry -> bool) (scan: SstScan) =
        Op.compute (fun () -> opOfScanPull width take scan)

    /// 走査の 1ステップを進め、不要な行はスキップする（pull 側）。
    and private opOfScanPull (width: int) (take: SstEntry -> bool) (current: SstScan) =
        match SstScan.next current with
        | Error e ->
            SstScan.dispose current
            OpStep.OpError e
        | Ok None -> OpStep.OpDone
        | Ok(Some(e, rest)) ->
            if not (take e) then
                SstScan.dispose rest
                OpStep.OpDone
            else
                match e.Value with
                | Some row ->
                    match rowOfFixedWidth width row with
                    | Ok values -> OpStep.OpRow(values, opOfScanEmit width take rest)
                    | Error e ->
                        SstScan.dispose rest
                        OpStep.OpError e
                | None -> opOfScanPull width take rest

    /// `SstScan` を `Op` に変換する。
    /// `take` が false になった時点で走査を打ち切る（キー昇順なので範囲の上限に使える）。
    /// <remarks>
    /// engine の走査は tombstone を返さない契約（`StorageEngine.scanAtWith` が除外する。
    /// 回帰: `StorageEngine: 走査とポイント検索は tombstone を返さない`）。
    /// 防御として `Value = None` は読み飛ばし、想定外の行幅は `InternalError` にする。
    /// </remarks>
    let private opOfScan (width: int) (take: SstEntry -> bool) (scan: SstScan) = opOfScanEmit width take scan

    /// 索引キーから表幅の行を組み立てる関数を作る。
    /// `positions` は「索引列 + 主キー列」の表内位置。
    /// mask false の列は復号せず `Unloaded` のままにする。
    let private indexRowDecoder (schema: TableSchema) (positions: int list) (mask: bool array) (key: byte array) =
        let positions' = Array.ofList positions

        let compactMask =
            positions' |> Array.map (fun p -> p >= 0 && p < mask.Length && mask.[p])

        OrderPreserving.readValuesMasked compactMask positions'.Length (Cursor.create key)
        >>= fun (values, rest) ->
            if not (Cursor.atEnd rest) then
                err (DbError.corruption "索引キーに余分なバイトがあります")
            else
                let row = Array.create schema.Columns.Length SqlValue.Unloaded
                Array.iteri (fun i p -> row.[p] <- values.[i]) positions'
                ret row

    /// 索引スロットだけを残り出力する。
    [<TailCall>]
    let rec private indexScanEmitSlots
        (rowOf: byte array -> Result<SqlValue array, DbError>)
        (slotArr: (byte array * SqlValue array option) array)
        (slotCount: int)
        (i: int)
        =
        if i >= slotCount then
            Op.Empty
        else
            Op.compute (fun () -> indexScanAdvanceSlots rowOf slotArr slotCount i)

    /// 索引スロットの 1ステップを進める。
    and private indexScanAdvanceSlots
        (rowOf: byte array -> Result<SqlValue array, DbError>)
        (slotArr: (byte array * SqlValue array option) array)
        (slotCount: int)
        (j: int)
        =
        if j >= slotCount then
            OpStep.OpDone
        else
            let key, value = slotArr.[j]

            match value with
            | None -> indexScanAdvanceSlots rowOf slotArr slotCount (j + 1)
            | Some _ ->
                match rowOf key with
                | Error e -> OpStep.OpError e
                | Ok row -> OpStep.OpRow(row, indexScanEmitSlots rowOf slotArr slotCount (j + 1))

    /// 索引スキャンを 1行ずつ引く。
    [<TailCall>]
    let rec private indexScanPull
        (rowOf: byte array -> Result<SqlValue array, DbError>)
        (inRange: byte array -> bool)
        (slotArr: (byte array * SqlValue array option) array)
        (slotCount: int)
        (scan: SstScan)
        (i: int)
        =
        Op.compute (fun () -> indexScanAdvanceScan rowOf inRange slotArr slotCount scan i)

    /// 索引スキャンの 1ステップを進め、スロットとマージする。
    and private indexScanAdvanceScan
        (rowOf: byte array -> Result<SqlValue array, DbError>)
        (inRange: byte array -> bool)
        (slotArr: (byte array * SqlValue array option) array)
        (slotCount: int)
        (scan: SstScan)
        (j: int)
        =
        if j >= slotCount then
            match SstScan.next scan with
            | Error e ->
                SstScan.dispose scan
                OpStep.OpError e
            | Ok None -> OpStep.OpDone
            | Ok(Some(e, rest)) ->
                if not (inRange e.Key) then
                    SstScan.dispose rest
                    OpStep.OpDone
                elif e.Value.IsNone then
                    indexScanAdvanceScan rowOf inRange slotArr slotCount rest j
                else
                    match rowOf e.Key with
                    | Error e -> OpStep.OpError e
                    | Ok row -> OpStep.OpRow(row, indexScanPull rowOf inRange slotArr slotCount rest j)
        else
            match SstScan.next scan with
            | Error e ->
                SstScan.dispose scan
                OpStep.OpError e
            | Ok None -> Op.next (indexScanEmitSlots rowOf slotArr slotCount j)
            | Ok(Some(e, rest)) ->
                if not (inRange e.Key) then
                    SstScan.dispose rest
                    Op.next (indexScanEmitSlots rowOf slotArr slotCount j)
                else
                    let sk, sv = slotArr.[j]

                    match Bytes.compare e.Key sk with
                    | c when c < 0 ->
                        if e.Value.IsNone then
                            indexScanAdvanceScan rowOf inRange slotArr slotCount rest j
                        else
                            match rowOf e.Key with
                            | Error e -> OpStep.OpError e
                            | Ok row -> OpStep.OpRow(row, indexScanPull rowOf inRange slotArr slotCount rest j)
                    | 0 ->
                        match sv with
                        | None -> indexScanAdvanceScan rowOf inRange slotArr slotCount rest (j + 1)
                        | Some _ ->
                            match rowOf sk with
                            | Error e -> OpStep.OpError e
                            | Ok row -> OpStep.OpRow(row, indexScanPull rowOf inRange slotArr slotCount rest (j + 1))
                    | _ ->
                        let replay =
                            SstScan.ofStepWithClose (fun () -> ret (Some(e, rest))) (fun () -> SstScan.dispose rest)

                        match sv with
                        | None -> indexScanAdvanceScan rowOf inRange slotArr slotCount replay (j + 1)
                        | Some _ ->
                            match rowOf sk with
                            | Error e -> OpStep.OpError e
                            | Ok row -> OpStep.OpRow(row, indexScanPull rowOf inRange slotArr slotCount replay (j + 1))

    /// 索引スキャン。
    /// store の走査と transaction のスロットを**キーで**マージする。
    /// <remarks>
    /// 索引値は空なので、行はキーから復号する。
    /// `slots` の値（`Some` / `None`）は存在・削除の区別にだけ使い、中身は読まない。
    /// engine は tombstone を返さない契約なので、`e.Value.IsNone` は防御分岐。
    /// </remarks>
    let private opOfIndexScan
        (rowOf: byte array -> Result<SqlValue array, DbError>)
        (inRange: byte array -> bool)
        (slots: (byte array * SqlValue array option) list)
        (scan: SstScan)
        =
        let slotArr =
            slots |> List.sortWith (fun (a, _) (b, _) -> Bytes.compare a b) |> List.toArray

        let slotCount = slotArr.Length
        indexScanPull rowOf inRange slotArr slotCount scan 0

    /// 符号化済みの主キーで既存行を列マスク付きで取り出す。
    let getByPrimaryKeyBytesWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (key: byte array)
        (mask: bool array)
        (visible: uint64 option)
        (txn: Transaction)
        =
        match Transaction.tryFind (tableSpace schema) key txn with
        | Some slot -> ret (slot |> Option.map (SqlValue.maskRow mask))
        | None -> RowStore.getAtMasked (tableSpace schema) key mask visible store

    /// 主キーで既存行を列マスク付きで取り出す。
    let getByPrimaryKeyWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (key: SqlValue array)
        (mask: bool array)
        (visible: uint64 option)
        (txn: Transaction)
        =
        KeyBuilder.ofValues key
        >>= fun bytes -> getByPrimaryKeyBytesWithTxnMask store schema bytes mask visible txn

    /// 主キーで prefix 走査する（キー昇順）。
    /// 返り値は遅延の `Op`（1行ずつ引くまで store を読まない）。
    let private scanByPrimaryKeyScopedMask
        (store: RowStore)
        (schema: TableSchema)
        (limit: int)
        (mask: bool array)
        (visible: uint64 option)
        (scope: ScanScope option)
        =
        RowStore.scanAtRawMasked (tableSpace schema) [||] limit mask visible scope store
        |> Result.map (opOfScan schema.Columns.Length (fun _ -> true))

    /// 主キーで prefix 走査する（キー昇順、スコープなし・全列）。
    let scanByPrimaryKey (store: RowStore) (schema: TableSchema) (limit: int) (visible: uint64 option) =
        scanByPrimaryKeyScopedMask store schema limit (Array.create schema.Columns.Length true) visible None

    /// 主キーで prefix 走査する（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`（1行ずつ引くまで store を読まない）。
    let scanByPrimaryKeyWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (limit: int)
        (mask: bool array)
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        =
        let slots = Transaction.slotsOf (tableSpace schema) txn

        let mask =
            if List.isEmpty slots then
                mask
            else
                widenMask mask schema.PrimaryKey

        scanByPrimaryKeyScopedMask store schema (storeLimitFor limit slots) mask visible scope
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.primaryV schema) slots rows
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 主キー範囲をストアから走査し、上限チェックと LIMIT を掛ける。
    let private scanPrimaryKeyRange
        (store: RowStore)
        (schema: TableSchema)
        (mask: bool array)
        (visible: uint64 option)
        (scope: ScanScope option)
        (lowerBytes: byte array)
        (upperBytes: byte array option)
        (limit: int)
        =
        RowStore.scanAtRawMasked (tableSpace schema) lowerBytes limit mask visible scope store
        |> Result.map (fun scan ->
            let rows =
                opOfScan schema.Columns.Length (fun e -> inKeyRange lowerBytes upperBytes e.Key) scan

            if limit > 0 then Op.take limit rows else rows)

    /// 主キーの範囲走査（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByPrimaryKeyRangeWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        (mask: bool array)
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        =
        result {
            let! startBytes = KeyBuilder.ofValues startKey
            let! endBytes = KeyBuilder.ofOptionalValues endKey

            let slots =
                Transaction.slotsOf (tableSpace schema) txn
                |> List.filter (fun (key, _) -> inKeyRange startBytes endBytes key)

            let mask =
                if List.isEmpty slots then
                    mask
                else
                    widenMask mask schema.PrimaryKey

            let! rows =
                scanPrimaryKeyRange store schema mask visible scope startBytes endBytes (storeLimitFor limit slots)

            let merged = overlayRows (RowKey.primaryV schema) slots rows

            if limit > 0 then
                return Op.take limit merged
            else
                return merged
        }

    /// 索引の値に一致する全行を引く（txn の書き込みを含む）。
    /// 索引値は空で、行は「索引列 + 主キー」のキーから復号する。
    /// 返り値は遅延の `Op`。
    let scanByIndexPrefixWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (values: SqlValue array)
        (limit: int)
        (mask: bool array)
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        =
        result {
            let! prefix = KeyBuilder.ofValues values
            let! positions = indexAndPrimaryIndices schema index

            let slots =
                Transaction.slotsOf (indexSpace index) txn
                |> List.filter (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)

            let rowOf = indexRowDecoder schema positions mask

            let! scan =
                RowStore.scanAtRawMasked (indexSpace index) prefix (storeLimitFor limit slots) [||] visible scope store

            let rows =
                opOfIndexScan rowOf (fun key -> KeyBuilder.startsWithPrefix key prefix) slots scan

            if limit > 0 then return Op.take limit rows else return rows
        }

    /// 索引の値が範囲 [lower, upper] に入る行を引く（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByIndexRangeWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (lower: SqlValue array)
        (upper: SqlValue array option)
        (limit: int)
        (mask: bool array)
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        =
        result {
            let! lowerBytes = KeyBuilder.ofValues lower
            let! upperBytes = KeyBuilder.ofOptionalValues upper
            let! positions = indexAndPrimaryIndices schema index

            let slots =
                Transaction.slotsOf (indexSpace index) txn
                |> List.filter (fun (key, _) -> inIndexRange lowerBytes upperBytes key)

            let rowOf = indexRowDecoder schema positions mask

            let! scan =
                RowStore.scanAtRawMasked
                    (indexSpace index)
                    lowerBytes
                    (storeLimitFor limit slots)
                    [||]
                    visible
                    scope
                    store

            let rows =
                opOfIndexScan rowOf (fun key -> inIndexRange lowerBytes upperBytes key) slots scan

            if limit > 0 then return Op.take limit rows else return rows
        }

    /// 1行を transaction に積む（まだ DB には入らない）。
    /// 索引のエントリも同じ transaction に入れるので、commit 原子性が保たれる。
    /// `DmlOps.insert` が制約検査で作ったキーをそのまま使い、二重の検索・符号化を避ける。
    let insertWithKey (key: byte array) (txn: Transaction) (schema: TableSchema) (row: SqlValue array) =
        result {
            do! TableSchema.validateRow schema row
            let next = Transaction.put (tableSpace schema) key row txn
            let! changed = applyIndexChange next schema None (Some row)
            return! Transaction.checkLimit changed
        }

    /// 主キー列が更新前後で同じか検査する。
    let private ensureSamePrimaryKey (schema: TableSchema) (oldRow: SqlValue array) (newRow: SqlValue array) =
        if Array.forall2 (=) (TableSchema.primaryKeyOf schema oldRow) (TableSchema.primaryKeyOf schema newRow) then
            OkUnit
        else
            sprintf "テーブル %s の主キー列は更新できません（DELETE + INSERT を使ってください）" schema.Name
            |> DbError.constraintViolation
            |> err

    /// 走査済みの行をそのまま使って更新する（主キー検索をやり直さない）。
    /// 主キーは変更できない（DELETE + INSERT を使う）。
    let updateRow (txn: Transaction) (schema: TableSchema) (oldRow: SqlValue array) (newRow: SqlValue array) =
        result {
            do! TableSchema.validateRow schema newRow
            do! ensureSamePrimaryKey schema oldRow newRow
            let! key = RowKey.primary schema newRow
            let afterMain = Transaction.put (tableSpace schema) key newRow txn
            let! changed = applyIndexChange afterMain schema (Some oldRow) (Some newRow)
            return! Transaction.checkLimit changed
        }

    /// 走査済みの行をそのまま使って削除する（主キー検索をやり直さない）。
    let deleteRow (txn: Transaction) (schema: TableSchema) (oldRow: SqlValue array) =
        result {
            let! key = RowKey.primary schema oldRow
            let next = Transaction.remove (tableSpace schema) key txn
            let! changed = applyIndexChange next schema (Some oldRow) None
            return! Transaction.checkLimit changed
        }
