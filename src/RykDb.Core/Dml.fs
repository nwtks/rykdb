namespace RykDb.Core

module internal RowKey =
    /// 索引の列値を取り出す。
    /// 索引列は CREATE INDEX で検証済み。
    /// 万一欠けている場合は、その列を落とす（防御）。
    let private indexValues (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.choose (fun name ->
            match TableSchema.indexOf schema name with
            | Some i -> Some row.[i]
            | None -> None)

    /// 主キーの LSM へ入れるキー。
    let primary (schema: TableSchema) (row: SqlValue array) =
        KeyBuilder.ofValues (TableSchema.primaryKeyOf schema row)

    /// 索引の LSM へ入れるキー（索引列 + 主キー列）。
    let indexKey (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        KeyBuilder.ofIndexAndPrimary (indexValues schema index row) (TableSchema.primaryKeyOf schema row)

/// DML — スキーマと transaction を使う上位操作。
/// 1行の書き込みは「主キーの LSM への 1件 + 各索引の LSM への 1件」。
/// これらは異なる keyspace なので並列に commit できる。
/// 削除は主キー + 全索引に tombstone を書く
/// （書き漏らすと索引経由で消えた行が復活する）。
module internal Dml =
    /// テーブルの keyspace 名（`t:<table>`）。
    let private tableSpace (schema: TableSchema) = Keyspace.table schema.Name

    /// 索引 schema を登録する。
    let private indexSpace (index: IndexSchema) = Keyspace.index index.Name

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
                let keyFor row = RowKey.indexKey schema index row
                let space = indexSpace index

                match oldRow, newRow with
                | None, None -> ret current
                | None, Some row -> ret (Transaction.put space (keyFor row) row current)
                | Some row, None -> ret (Transaction.remove space (keyFor row) current)
                | Some oldValue, Some newValue ->
                    let oldKey = keyFor oldValue
                    let newKey = keyFor newValue

                    if oldKey = newKey then
                        ret (Transaction.put space newKey newValue current)
                    else
                        let removed = Transaction.remove space oldKey current
                        ret (Transaction.put space newKey newValue removed))
            txn

    /// store の行に txn の書き込みを重ねる（ストリーミング）。
    /// <remarks>
    /// store の行も txn のバッファも**キー昇順**なので、両者を突き合わせながら 1行ずつ合成できる。
    /// 従来は「store の全行を配列化 → 差し替え → 追加分を足して再ソート」だったので、
    /// transaction 中の大テーブル走査が O(テーブル行数) のメモリを使った。
    /// ここでは store 側は 1行だけ、slot 側は txn のバッファぶんだけしか持たない。
    /// slot は 1回ずつしか消費されない（store 行と突き合わせたとき、あるいは store が尽きたとき）。
    /// </remarks>
    let private overlayRows
        (keyOf: SqlValue array -> byte array)
        (slots: (byte array * SqlValue array option) list)
        (rows: Op)
        =
        if List.isEmpty slots then
            rows
        else
            let slotArr = slots |> List.sortBy fst |> List.toArray

            let rec emitRemaining i =
                if i >= slotArr.Length then
                    Op.Done
                else
                    Op.Compute(fun () ->
                        match slotArr.[i] with
                        | _, Some v -> ret (Some(v, emitRemaining (i + 1)))
                        | _, None -> emitRemaining (i + 1) |> Op.next)

            let rec merge cur i =
                if i >= slotArr.Length then
                    cur
                else
                    Op.Compute(fun () ->
                        match Op.next cur with
                        | Error e -> err e
                        | Ok None -> emitRemaining i |> Op.next
                        | Ok(Some(row, nxt)) ->
                            let rk = keyOf row
                            let sk, sv = slotArr.[i]

                            match Bytes.compare rk sk with
                            | c when c < 0 -> ret (Some(row, merge nxt i))
                            | 0 ->
                                match sv with
                                | Some v -> ret (Some(v, merge nxt (i + 1)))
                                | None -> merge nxt (i + 1) |> Op.next
                            | _ ->
                                match sv with
                                | Some v -> ret (Some(v, Op.Step(row, merge nxt (i + 1))))
                                | None -> ret (Some(row, merge nxt (i + 1))))

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

    /// 主キーで既存行を取り出す（txn の書き込みを含む）。
    let getByPrimaryKeyWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (key: SqlValue array)
        (visible: uint64 option)
        (txn: Transaction)
        =
        let getByPrimaryKey key =
            RowStore.getAt (tableSpace schema) (KeyBuilder.ofValues key) visible store

        match Transaction.tryFind (tableSpace schema) (KeyBuilder.ofValues key) txn with
        | Some slot -> ret slot
        | None -> getByPrimaryKey key

    /// 主キーで prefix 走査する（キー昇順）。
    let scanByPrimaryKey (store: RowStore) (schema: TableSchema) (limit: int) (visible: uint64 option) =
        RowStore.scanAt (tableSpace schema) [||] limit visible store
        |> Result.map (Seq.map (fun (_, v) -> v |> Option.defaultValue [||]))

    /// 主キーで prefix 走査する（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`（1行ずつ引くまで store を読まない）。
    let scanByPrimaryKeyWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (limit: int)
        (visible: uint64 option)
        (txn: Transaction)
        =
        let slots = Transaction.slotsOf (tableSpace schema) txn

        scanByPrimaryKey store schema (storeLimitFor limit slots) visible
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.primary schema) slots (Op.ofSeq rows)
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 主キーの範囲走査（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByPrimaryKeyRangeWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        (visible: uint64 option)
        (txn: Transaction)
        =
        let scanPrimaryKeyRange lowerBytes upperBytes limit =
            RowStore.scanAt (tableSpace schema) lowerBytes limit visible store
            |> Result.map (
                Seq.choose (fun (key, value) ->
                    match value with
                    | Some row when inKeyRange lowerBytes upperBytes key -> Some row
                    | _ -> None)
                >> Op.ofSeq
                >> fun op -> if limit > 0 then Op.take limit op else op
            )

        let startBytes = KeyBuilder.ofValues startKey
        let endBytes = endKey |> Option.map KeyBuilder.ofValues

        let slots =
            Transaction.slotsOf (tableSpace schema) txn
            |> List.filter (fun (key, _) -> inKeyRange startBytes endBytes key)

        scanPrimaryKeyRange startBytes endBytes (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let merged = overlayRows (RowKey.primary schema) slots rows

            let filtered =
                match endBytes with
                | None -> merged
                | Some eb -> Op.filter (fun row -> ret (Bytes.compare (RowKey.primary schema row) eb <= 0)) merged

            if limit > 0 then Op.take limit filtered else filtered)

    /// 索引の値に一致する全行を引く（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByIndexPrefixWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (values: SqlValue array)
        (limit: int)
        (visible: uint64 option)
        (txn: Transaction)
        =
        let scanIndexPrefix prefix limit =
            RowStore.scanAt (indexSpace index) prefix limit visible store
            |> Result.map (
                Seq.takeWhile (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)
                >> Seq.choose snd
                >> Op.ofSeq
                >> fun op -> if limit > 0 then Op.take limit op else op
            )

        let prefix = KeyBuilder.ofValues values

        let slots =
            Transaction.slotsOf (indexSpace index) txn
            |> List.filter (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)

        scanIndexPrefix prefix (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.indexKey schema index) slots rows
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 索引の値が範囲 [lower, upper] に入る行を引く（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByIndexRangeWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (lower: SqlValue array)
        (upper: SqlValue array option)
        (limit: int)
        (visible: uint64 option)
        (txn: Transaction)
        =
        let scanIndexRange lowerBytes upperBytes limit =
            RowStore.scanAt (indexSpace index) lowerBytes limit visible store
            |> Result.map (
                Seq.filter (fun (key, _) -> inIndexRange lowerBytes upperBytes key)
                >> Seq.choose snd
                >> Op.ofSeq
                >> fun op -> if limit > 0 then Op.take limit op else op
            )

        let lowerBytes = KeyBuilder.ofValues lower
        let upperBytes = upper |> Option.map KeyBuilder.ofValues

        let slots =
            Transaction.slotsOf (indexSpace index) txn
            |> List.filter (fun (key, _) -> inIndexRange lowerBytes upperBytes key)

        scanIndexRange lowerBytes upperBytes (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.indexKey schema index) slots rows
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 1行を transaction に積む（まだ DB には入らない）。
    /// 索引のエントリも同じ transaction に入れるので、commit 原子性が保たれる。
    let insert (txn: Transaction) (schema: TableSchema) (row: SqlValue array) =
        result {
            do! TableSchema.validateRow schema row
            let tks = tableSpace schema
            let pk = RowKey.primary schema row

            match Transaction.tryFind tks pk txn with
            | Some(Some _) ->
                return!
                    sprintf "PRIMARY KEY 制約に違反: %s" schema.Name
                    |> DbError.constraintViolation
                    |> err
            | _ -> ()

            let next = Transaction.put tks pk row txn
            let! changed = applyIndexChange next schema None (Some row)
            return! Transaction.checkLimit changed
        }

    /// 走査済みの行をそのまま使って更新する（主キー検索をやり直さない）。
    /// 主キーは変更できない（DELETE + INSERT を使う）。
    let updateRow (txn: Transaction) (schema: TableSchema) (oldRow: SqlValue array) (newRow: SqlValue array) =
        let ensureSamePrimaryKey schema oldRow newRow =
            if Array.forall2 (=) (TableSchema.primaryKeyOf schema oldRow) (TableSchema.primaryKeyOf schema newRow) then
                ret ()
            else
                sprintf "テーブル %s の主キー列は更新できません（DELETE + INSERT を使ってください）" schema.Name
                |> DbError.constraintViolation
                |> err

        result {
            do! TableSchema.validateRow schema newRow
            do! ensureSamePrimaryKey schema oldRow newRow

            let afterMain =
                Transaction.put (tableSpace schema) (RowKey.primary schema newRow) newRow txn

            let! changed = applyIndexChange afterMain schema (Some oldRow) (Some newRow)
            return! Transaction.checkLimit changed
        }

    /// 走査済みの行をそのまま使って削除する（主キー検索をやり直さない）。
    let deleteRow (txn: Transaction) (schema: TableSchema) (oldRow: SqlValue array) =
        result {
            let next = Transaction.remove (tableSpace schema) (RowKey.primary schema oldRow) txn
            let! changed = applyIndexChange next schema (Some oldRow) None
            return! Transaction.checkLimit changed
        }
