namespace RykDb.Core

module internal RowKey =
    /// 索引の列値を取り出す。
    /// 索引列は CREATE INDEX で検証済み。
    /// 万一欠けている場合は、その列を落とす（防御）。
    let indexValues (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.choose (fun name ->
            match TableSchema.indexOf schema name with
            | Some i -> Some row.[i]
            | None -> None)

    /// 主キーの LSM へ入れるキー（内部ホットパス用）。
    let primaryV (schema: TableSchema) (row: SqlValue array) =
        KeyBuilder.ofValues (TableSchema.primaryKeyOf schema row) |> DbResult.ofResult

    /// 主キーの LSM へ入れるキー。
    let primary (schema: TableSchema) (row: SqlValue array) =
        primaryV schema row |> DbResult.toResult

    /// 索引の LSM へ入れるキー（索引列 + 主キー列、内部ホットパス用）。
    let indexKeyV (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        KeyBuilder.ofIndexAndPrimary (indexValues schema index row) (TableSchema.primaryKeyOf schema row)
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
    let private emptyIndexValue: SqlValue array = Array.empty

    let private reserveUniqueValues (txn: Transaction) (schema: TableSchema) (index: IndexSchema) =
        function
        | None -> ret txn
        | Some values ->
            let keyValues = RowKey.indexValues schema index values

            if keyValues |> List.exists SqlValue.isNull then
                ret txn
            else
                KeyBuilder.ofValues keyValues
                |> Result.map (fun key -> Transaction.reserveUnique index.Name key txn)

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
                let keyFor row = RowKey.indexKey schema index row

                match oldRow, newRow with
                | None, None -> reserved
                | None, Some row ->
                    reserved
                    >>= fun txn ->
                        keyFor row
                        |> Result.map (fun key -> Transaction.put space key emptyIndexValue txn)
                | Some row, None ->
                    reserved
                    >>= fun txn -> keyFor row |> Result.map (fun key -> Transaction.remove space key txn)
                | Some oldValue, Some newValue ->
                    reserved
                    >>= fun txn ->
                        keyFor oldValue
                        >>= fun oldKey ->
                            keyFor newValue
                            |> Result.map (fun newKey ->
                                if oldKey = newKey then
                                    Transaction.put space newKey emptyIndexValue txn
                                else
                                    let removed = Transaction.remove space oldKey txn
                                    Transaction.put space newKey emptyIndexValue removed))
            txn

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
            let slotArr = slots |> List.sortBy fst |> List.toArray

            let rec emitRemaining i =
                if i >= slotArr.Length then
                    Op.Empty
                else
                    Op.compute (fun () ->
                        match slotArr.[i] with
                        | _, Some v -> OpStep.OpRow(v, emitRemaining (i + 1))
                        | _, None -> Op.next (emitRemaining (i + 1)))

            let rec merge cur i =
                if i >= slotArr.Length then
                    cur
                else
                    Op.compute (fun () ->
                        match Op.next cur with
                        | OpStep.OpError e -> OpStep.OpError e
                        | OpStep.OpDone -> Op.next (emitRemaining i)
                        | OpStep.OpRow(row, nxt) ->
                            let sk, sv = slotArr.[i]

                            match keyOf row with
                            | DbErr e -> OpStep.OpError e
                            | DbOk rk ->
                                match Bytes.compare rk sk with
                                | c when c < 0 -> OpStep.OpRow(row, merge nxt i)
                                | 0 ->
                                    match sv with
                                    | Some v -> OpStep.OpRow(v, merge nxt (i + 1))
                                    | None -> Op.next (merge nxt (i + 1))
                                | _ ->
                                    match sv with
                                    | Some v -> OpStep.OpRow(v, Op.emit row (merge nxt (i + 1)))
                                    | None -> OpStep.OpRow(row, merge nxt (i + 1)))

            merge rows 0

    let private storeLimitFor (limit: int) (slots: (byte array * SqlValue array option) list) =
        if limit <= 0 then
            0
        else
            let deletions = slots |> List.filter (fun (_, value) -> value.IsNone) |> List.length
            int (min (int64 System.Int32.MaxValue) (int64 limit + int64 deletions))

    let private inKeyRange (lowerBytes: byte array) (upperBytes: byte array option) (key: byte array) =
        Bytes.compare key lowerBytes >= 0
        && match upperBytes with
           | None -> true
           | Some hb -> Bytes.compare key hb <= 0

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

    /// `SstScan` を `Op` に変換する。
    /// `take` が false になった時点で走査を打ち切る（キー昇順なので範囲の上限に使える）。
    /// `SstScan` が返す None（削除済み）は読み飛ばす。
    let private opOfScan (width: int) (take: SstEntry -> bool) (scan: SstScan) =
        let rowOf (value: SqlValue array option) =
            match value with
            | Some row when row.Length = width -> row
            | Some row when row.Length = 0 -> Array.create width SqlValue.Unloaded
            | Some row -> row
            | None -> [||]

        let rec emit scan =
            Op.compute (fun () ->
                let rec pull current =
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
                            | Some row -> OpStep.OpRow(rowOf (Some row), emit rest)
                            | None -> pull rest

                pull scan)

        emit scan

    /// 索引キーから表幅の行を組み立てる関数を作る。
    /// `positions` は「索引列 + 主キー列」の表内位置。
    /// mask false の列は復号せず `Unloaded` のままにする。
    let private indexRowDecoder (schema: TableSchema) (positions: int list) (mask: bool array) (key: byte array) =
        let positions' = Array.ofList positions

        let compactMask =
            positions' |> Array.map (fun p -> p >= 0 && p < mask.Length && mask.[p])

        match OrderPreserving.readValuesMasked compactMask positions'.Length (Cursor.create key) with
        | Error e -> err e
        | Ok(values, rest) ->
            if not (Cursor.atEnd rest) then
                err (DbError.corruption "索引キーに余分なバイトがあります")
            else
                let row = Array.create schema.Columns.Length SqlValue.Unloaded
                Array.iteri (fun i p -> row.[p] <- values.[i]) positions'
                ret row

    /// 索引スキャン。
    /// store の走査と transaction のスロットを**キーで**マージする。
    /// <remarks>
    /// 索引値は空なので、行はキーから復号する。
    /// `slots` の値（`Some` / `None`）は存在・削除の区別にだけ使い、中身は読まない。
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

        let rec emitSlots i =
            if i >= slotCount then
                Op.Empty
            else
                Op.compute (fun () -> advance i)

        and advance j =
            if j >= slotCount then
                OpStep.OpDone
            else
                let key, value = slotArr.[j]

                match value with
                | None -> advance (j + 1)
                | Some _ ->
                    match rowOf key with
                    | Error e -> OpStep.OpError e
                    | Ok row -> OpStep.OpRow(row, emitSlots (j + 1))

        let rec pull scan i = Op.compute (fun () -> advance scan i)

        and advance scan j =
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
                        advance rest j
                    else
                        match rowOf e.Key with
                        | Error e -> OpStep.OpError e
                        | Ok row -> OpStep.OpRow(row, pull rest j)
            else
                match SstScan.next scan with
                | Error e ->
                    SstScan.dispose scan
                    OpStep.OpError e
                | Ok None -> Op.next (emitSlots j)
                | Ok(Some(e, rest)) ->
                    if not (inRange e.Key) then
                        SstScan.dispose rest
                        Op.next (emitSlots j)
                    else
                        let sk, sv = slotArr.[j]

                        match Bytes.compare e.Key sk with
                        | c when c < 0 ->
                            if e.Value.IsNone then
                                advance rest j
                            else
                                match rowOf e.Key with
                                | Error e -> OpStep.OpError e
                                | Ok row -> OpStep.OpRow(row, pull rest j)
                        | 0 ->
                            match sv with
                            | None -> advance rest (j + 1)
                            | Some _ ->
                                match rowOf sk with
                                | Error e -> OpStep.OpError e
                                | Ok row -> OpStep.OpRow(row, pull rest (j + 1))
                        | _ ->
                            let replay =
                                SstScan.ofStepWithClose (fun () -> ret (Some(e, rest))) (fun () -> SstScan.dispose rest)

                            match sv with
                            | None -> advance replay (j + 1)
                            | Some _ ->
                                match rowOf sk with
                                | Error e -> OpStep.OpError e
                                | Ok row -> OpStep.OpRow(row, pull replay (j + 1))

        pull scan 0

    /// 符号化済みの主キーで既存行を列マスク付きで取り出す。
    let getByPrimaryKeyBytesWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (key: byte array)
        (visible: uint64 option)
        (txn: Transaction)
        (mask: bool array)
        =
        match Transaction.tryFind (tableSpace schema) key txn with
        | Some slot -> ret (slot |> Option.map (SqlValue.maskRow mask))
        | None -> RowStore.getAtMasked (tableSpace schema) key visible mask store

    /// 主キーで既存行を列マスク付きで取り出す。
    let getByPrimaryKeyWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (key: SqlValue array)
        (visible: uint64 option)
        (txn: Transaction)
        (mask: bool array)
        =
        KeyBuilder.ofValues key
        >>= fun bytes -> getByPrimaryKeyBytesWithTxnMask store schema bytes visible txn mask

    /// 主キーで prefix 走査する（キー昇順）。
    /// 返り値は遅延の `Op`（1行ずつ引くまで store を読まない）。
    let private scanByPrimaryKeyScopedMask
        (scope: ScanScope option)
        (store: RowStore)
        (schema: TableSchema)
        (limit: int)
        (visible: uint64 option)
        (mask: bool array)
        =
        RowStore.scanAtRawMasked (tableSpace schema) [||] limit visible scope mask store
        |> Result.map (opOfScan schema.Columns.Length (fun _ -> true))

    /// 主キーで prefix 走査する（キー昇順、スコープなし・全列）。
    let scanByPrimaryKey store schema limit visible =
        scanByPrimaryKeyScopedMask None store schema limit visible (Array.create schema.Columns.Length true)

    /// 主キーで prefix 走査する（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`（1行ずつ引くまで store を読まない）。
    let scanByPrimaryKeyWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (limit: int)
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        (mask: bool array)
        =
        let slots = Transaction.slotsOf (tableSpace schema) txn
        // overlay は主キーを読むので、書き込みがあるときは主キー列を足す。
        let mask =
            if List.isEmpty slots then
                mask
            else
                widenMask mask schema.PrimaryKey

        scanByPrimaryKeyScopedMask scope store schema (storeLimitFor limit slots) visible mask
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.primaryV schema) slots rows
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 主キーで prefix 走査する（txn の書き込みを含む・bool 互換）。
    let scanByPrimaryKeyWithTxnValues store schema limit visible scope txn needsValues =
        scanByPrimaryKeyWithTxnMask
            store
            schema
            limit
            visible
            scope
            txn
            (Array.create schema.Columns.Length needsValues)

    /// 主キーの範囲走査（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByPrimaryKeyRangeWithTxnMask
        (store: RowStore)
        (schema: TableSchema)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        (mask: bool array)
        =
        result {
            let! startBytes = KeyBuilder.ofValues startKey
            let! endBytes = KeyBuilder.ofOptionalValues endKey

            let slots =
                Transaction.slotsOf (tableSpace schema) txn
                |> List.filter (fun (key, _) -> inKeyRange startBytes endBytes key)

            let mask =
                if endBytes.IsNone && List.isEmpty slots then
                    mask
                else
                    widenMask mask schema.PrimaryKey

            let scanPrimaryKeyRange lowerBytes upperBytes limit =
                RowStore.scanAtRawMasked (tableSpace schema) lowerBytes limit visible scope mask store
                |> Result.map (fun scan ->
                    let rows =
                        opOfScan schema.Columns.Length (fun e -> inKeyRange lowerBytes upperBytes e.Key) scan

                    if limit > 0 then Op.take limit rows else rows)

            let! rows = scanPrimaryKeyRange startBytes endBytes (storeLimitFor limit slots)
            let merged = overlayRows (RowKey.primaryV schema) slots rows

            let filtered =
                match endBytes with
                | None -> merged
                | Some eb ->
                    Op.filter
                        (fun row ->
                            match RowKey.primaryV schema row with
                            | DbErr e -> DbErr e
                            | DbOk key -> DbOk(Bytes.compare key eb <= 0))
                        merged

            if limit > 0 then
                return Op.take limit filtered
            else
                return filtered
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
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        (mask: bool array)
        =
        result {
            let! prefix = KeyBuilder.ofValues values
            let! positions = indexAndPrimaryIndices schema index

            let slots =
                Transaction.slotsOf (indexSpace index) txn
                |> List.filter (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)

            let rowOf = indexRowDecoder schema positions mask

            let! scan =
                RowStore.scanAtRawMasked (indexSpace index) prefix (storeLimitFor limit slots) visible scope [||] store

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
        (visible: uint64 option)
        (scope: ScanScope option)
        (txn: Transaction)
        (mask: bool array)
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
                    visible
                    scope
                    [||]
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

    /// 走査済みの行をそのまま使って更新する（主キー検索をやり直さない）。
    /// 主キーは変更できない（DELETE + INSERT を使う）。
    let updateRow (txn: Transaction) (schema: TableSchema) (oldRow: SqlValue array) (newRow: SqlValue array) =
        let ensureSamePrimaryKey schema oldRow newRow =
            if Array.forall2 (=) (TableSchema.primaryKeyOf schema oldRow) (TableSchema.primaryKeyOf schema newRow) then
                OkUnit
            else
                sprintf "テーブル %s の主キー列は更新できません（DELETE + INSERT を使ってください）" schema.Name
                |> DbError.constraintViolation
                |> err

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
