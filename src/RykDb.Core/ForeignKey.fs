// ===========================================================================
// ForeignKey — 外部キー（REFERENCES）の定義の解決と参照整合性検査。
//
//  ・参照先は親テーブルの主キーだけ（列の型も一致させる）。
//  ・子の外部キー列に NULL を含む行は参照しない（MATCH SIMPLE。SQL の既定）。
//  ・参照アクションは未対応。RESTRICT（即時検査）だけを行う。
//      - 子の INSERT / UPDATE は親行の存在を要求する。
//      - 親の DELETE は参照している子行があると ConstraintViolation。
//    親の主キーは UPDATE できない（`Dml.updateRow` が拒否する）ので、親の UPDATE 検査は要らない。
// ===========================================================================

namespace RykDb.Core

module internal ForeignKey =
    /// 大文字小文字を無視した列名リストの一致。
    let private sameNames (a: string list) (b: string list) =
        a.Length = b.Length && List.forall2 SqlName.equal a b

    /// 列名のリストを列定義へ解決する（上から順）。
    let private columnsOf (columns: ColumnDefinition list) (names: string list) =
        names
        |> List.map (fun name ->
            match columns |> List.tryFind (fun col -> SqlName.equal col.Name name) with
            | Some col -> ret col
            | None -> err (DbError.name (sprintf "列 %s が定義されていません" name)))
        |> all

    /// 主キー列名（キーの符号化順 = 列の定義順）。
    let private primaryKeyNames (schema: TableSchema) =
        TableSchema.primaryKeyColumns schema |> List.ofArray

    /// `CREATE TABLE` の外部キーを検証し、カタログへ保存する形にする。
    /// 参照先の列は省略時には親の主キーへ解決し、指定時も主キーと同順であることを要求する。
    let resolveCreatedTable (catalog: Catalog) (schema: TableSchema) (c: CreateTable) =
        let references =
            (c.Columns
             |> List.choose (fun col -> col.Constraint.References |> Option.map (fun r -> [ col.Name ], r)))
            @ (c.Constraints
               |> List.choose (fun tc ->
                   match tc with
                   | TableConstraint.ForeignKey(columns, r) -> Some(columns, r)
                   | _ -> None))

        let resolveOne (columns: string list, r: ForeignKeyRef) =
            result {
                do!
                    if List.isEmpty columns then
                        err (DbError.constraintViolation "FOREIGN KEY の列が空です")
                    else
                        ret ()

                match SqlName.duplicate columns with
                | Some name -> return! err (DbError.name (sprintf "FOREIGN KEY の列 %s が重複しています" name))
                | None -> ()

                let! childColumns = columnsOf c.Columns columns

                let! parent =
                    if Catalog.eqName r.ParentTable schema.Name then
                        ret schema
                    else
                        match Catalog.findTable r.ParentTable catalog with
                        | Some defn -> Catalog.toSchema defn
                        | None -> err (DbError.name (sprintf "参照先テーブル %s が存在しません" r.ParentTable))

                let parentKey = primaryKeyNames parent

                let! resolvedKey =
                    match r.ParentColumns with
                    | None -> ret parentKey
                    | Some names when sameNames names parentKey -> ret parentKey
                    | Some names ->
                        err (
                            DbError.constraintViolation (
                                sprintf
                                    "参照先の列（%s）はテーブル %s の主キー（%s）と同順で指定してください"
                                    (System.String.Join(", ", names))
                                    parent.Name
                                    (System.String.Join(", ", parentKey))
                            )
                        )

                do!
                    if columns.Length = resolvedKey.Length then
                        ret ()
                    else
                        err (
                            DbError.constraintViolation (
                                sprintf "FOREIGN KEY の列数(%d)と参照先の主キー列数(%d)が一致しません" columns.Length resolvedKey.Length
                            )
                        )

                let! parentColumns = columnsOf parent.Columns resolvedKey

                do!
                    List.zip childColumns parentColumns
                    |> List.map (fun (childCol, parentCol) ->
                        if childCol.DataType = parentCol.DataType then
                            ret ()
                        else
                            err (
                                DbError.typeError (
                                    sprintf
                                        "外部キーの列 %s の型 %s が参照先 %s.%s の型 %s と一致しません"
                                        childCol.Name
                                        (DataType.toSqlName childCol.DataType)
                                        parent.Name
                                        parentCol.Name
                                        (DataType.toSqlName parentCol.DataType)
                                )
                            ))
                    |> all
                    |> Result.map ignore

                return
                    {
                        Columns = columns
                        ParentTable = r.ParentTable
                        ParentColumns = resolvedKey
                    }
            }

        references |> List.map resolveOne |> all

    /// 列名のリストから行の値を取り出す。
    let private valuesOf (schema: TableSchema) (names: string list) (row: SqlValue array) =
        names
        |> List.map (TableSchema.requireIndexOf schema)
        |> all
        >>= fun positions ->
            let values = positions |> List.map (fun i -> row.[i]) |> List.toArray

            if
                values
                |> Array.exists (function
                    | SqlValue.Unloaded -> true
                    | _ -> false)
            then
                err (DbError.internalError "外部キーの列が復号されていません")
            else
                ret values

    let private parentSchemaOf (catalog: Catalog) (table: string) =
        match Catalog.findTable table catalog with
        | Some defn -> Catalog.toSchema defn
        | None -> err (DbError.name (sprintf "参照先テーブル %s が存在しません" table))

    let private checkOne
        (store: RowStore)
        (catalog: Catalog)
        (visible: uint64 option)
        (txn: Transaction)
        (schema: TableSchema)
        (fk: ForeignKeyDefinition)
        (row: SqlValue array)
        =
        result {
            let! values = valuesOf schema fk.Columns row

            if values |> Array.exists SqlValue.isNull then
                return ()
            else
                let! parent = parentSchemaOf catalog fk.ParentTable
                let mask = Array.create parent.Columns.Length false
                let! key = KeyBuilder.ofValues values
                let! found = Dml.getByPrimaryKeyBytesWithTxnMask store parent key visible txn mask

                if found.IsSome then
                    return ()
                else
                    return!
                        sprintf
                            "外部キー制約に違反: %s(%s) が参照する %s(%s) の行がありません"
                            schema.Name
                            (System.String.Join(", ", fk.Columns))
                            fk.ParentTable
                            (System.String.Join(", ", fk.ParentColumns))
                        |> DbError.constraintViolation
                        |> err
        }

    /// 子行（INSERT / UPDATE 後）の外部キーを検査する。
    /// テーブルがカタログに無い（単体テストのスキーマ）なら何もしない。
    let checkChildRow
        (store: RowStore)
        (catalog: Catalog)
        (visible: uint64 option)
        (txn: Transaction)
        (schema: TableSchema)
        (row: SqlValue array)
        =
        match Catalog.findTable schema.Name catalog with
        | Some defn ->
            defn.ForeignKeys
            |> List.map (fun fk -> checkOne store catalog visible txn schema fk row)
            |> all
            |> Result.map ignore
        | None -> ret ()

    /// Op の行を先頭から調べ、`matches` が真になった時点で打ち切る。
    /// Op は one-shot なので途中で捨てた走査は Scope の DisposeAll で閉じる。
    let private anyRow (matches: SqlValue array -> DbResult<bool>) (op: Op) =
        let rec loop current =
            match Op.next current with
            | OpStep.OpError e -> Error e
            | OpStep.OpDone -> Ok false
            | OpStep.OpRow(row, next) ->
                match matches row with
                | DbErr e -> Error e
                | DbOk true -> Ok true
                | DbOk false -> loop next

        loop op

    let private sameValue (a: SqlValue) (b: SqlValue) =
        match SqlValue.compareValuesV a b with
        | DbOk 0 -> true
        | _ -> false

    let private rowMatches (positions: int list) (values: SqlValue array) (row: SqlValue array) =
        positions
        |> List.mapi (fun i p -> sameValue row.[p] values.[i])
        |> List.forall id

    let private maskOf (schema: TableSchema) (names: string list) =
        names
        |> List.map (TableSchema.requireIndexOf schema)
        |> all
        |> Result.map (fun positions ->
            let mask = Array.create schema.Columns.Length false

            for p in positions do
                if p >= 0 && p < mask.Length then
                    mask.[p] <- true

            mask)

    /// 子テーブルに、外部キー列が `values` と一致する行があるか。
    let private childRowExists
        (store: RowStore)
        (child: TableSchema)
        (fkColumns: string list)
        (values: SqlValue array)
        (visible: uint64 option)
        (scope: ScanScope)
        (txn: Transaction)
        =
        result {
            let mask = Array.create child.Columns.Length false

            let indexHit =
                child.Indexes
                |> List.tryFind (fun idx -> sameNames (idx.Columns |> List.truncate fkColumns.Length) fkColumns)

            match indexHit with
            | Some idx ->
                let! op =
                    Dml.scanByIndexPrefixWithTxnMask store child idx values 1 visible (Some scope) txn mask

                return! anyRow (fun _ -> DbOk true) op
            | None ->
                if sameNames (primaryKeyNames child) fkColumns then
                    let! op =
                        Dml.scanByPrimaryKeyRangeWithTxnMask
                            store
                            child
                            values
                            (Some values)
                            1
                            visible
                            (Some scope)
                            txn
                            mask

                    return! anyRow (fun _ -> DbOk true) op
                else
                    let! scanMask = maskOf child fkColumns
                    let! positions = fkColumns |> List.map (TableSchema.requireIndexOf child) |> all

                    child.PrimaryKey
                    |> List.iter (fun p ->
                        if p >= 0 && p < scanMask.Length then
                            scanMask.[p] <- true)

                    let! op =
                        Dml.scanByPrimaryKeyWithTxnMask store child 0 visible (Some scope) txn scanMask

                    return! anyRow (fun row -> DbOk(rowMatches positions values row)) op
        }

    /// 親行を削除してよいか（RESTRICT）。
    /// 参照している子行があれば `ConstraintViolation`。
    let checkParentDelete
        (store: RowStore)
        (catalog: Catalog)
        (visible: uint64 option)
        (scope: ScanScope)
        (txn: Transaction)
        (schema: TableSchema)
        (row: SqlValue array)
        =
        Catalog.referencingForeignKeys schema.Name catalog
        |> List.map (fun (childDefn, fk) ->
            result {
                let! child =
                    Catalog.toSchema childDefn
                    |> Result.map (fun s ->
                        { s with
                            Indexes = Catalog.indexSchemasOf childDefn.Name catalog
                        })

                let! values = valuesOf schema fk.ParentColumns row

                if values |> Array.exists SqlValue.isNull then
                    return ()
                else
                    let! exists = childRowExists store child fk.Columns values visible scope txn

                    if exists then
                        return!
                            sprintf
                                "外部キー制約に違反: テーブル %s の行が %s(%s) を参照しています"
                                child.Name
                                schema.Name
                                (System.String.Join(", ", fk.ParentColumns))
                            |> DbError.constraintViolation
                            |> err
                    else
                        return ()
            })
        |> all
        |> Result.map ignore
