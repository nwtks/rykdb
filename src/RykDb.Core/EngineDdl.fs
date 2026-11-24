// ===========================================================================
// EngineDdl — テーブル・索引の DDL。
// ===========================================================================

namespace RykDb.Core

module internal EngineDdl =
    /// CREATE TABLE 本体（排他は呼び出し側で取る）。
    let private createTableCore (c: CreateTable) (state: ExecState) =
        let existing = Catalog.findTable c.Name state.Catalog

        match existing, c.IfNotExists with
        | Some _, true -> ret state
        | Some _, false -> err (sprintf "テーブル %s は既に存在します" c.Name |> DbError.alreadyExists)
        | None, _ ->
            result {
                do! Keyspace.validateName "テーブル" c.Name
                let! schema = TableSchema.ofCreateTable c
                let! foreignKeys = ForeignKey.resolveCreatedTable state.Catalog schema c

                let implicitUniqueCols =
                    (schema.Columns
                     |> List.filter (fun col -> col.Constraint.Unique)
                     |> List.map (fun col -> [ col.Name ]))
                    @ (c.Constraints
                       |> List.choose (fun tc ->
                           match tc with
                           | TableConstraint.UniqueColumns cols -> Some cols
                           | _ -> None))

                let implicitIndexes =
                    implicitUniqueCols
                    |> List.mapi (fun i cols ->
                        {
                            IndexName = Catalog.implicitIndexName c.Name i
                            TableName = c.Name
                            Columns = cols
                            Unique = true
                        })

                let catalog1 =
                    Catalog.addTable (Catalog.toDefinitionWithForeignKeys schema foreignKeys) state.Catalog

                let catalog2 =
                    implicitIndexes |> List.fold (fun cat idx -> Catalog.addIndex idx cat) catalog1

                let spaces =
                    Keyspace.table c.Name
                    :: (implicitIndexes |> List.map (fun idx -> Keyspace.index idx.IndexName))

                let! cleaned =
                    spaces |> foldOk (fun st sp -> ExecState.clearSpaceChunksResult sp st) state

                return! ExecState.saved catalog2 cleaned
            }

    /// CREATE TABLE。
    /// カタログ更新と後始末を排他で行う。
    let createTable (c: CreateTable) (state: ExecState) =
        createTableCore c |> ExecState.exclusiveDdl state

    /// DROP TABLE。
    /// 手順:
    ///   1. 先にカタログから定義を外してテーブルを見えなくする
    ///   2. テーブルと全索引 keyspace の全エントリをチャンク単位で tombstone 消去する
    /// 後始末が中断しても、次回の CREATE TABLE が同名 keyspace を空にしてから作る。
    let private dropTableCore (name: string) (ifExists: bool) (state: ExecState) =
        match Catalog.findTable name state.Catalog with
        | None ->
            if ifExists then
                ret state
            else
                err (sprintf "テーブル %s が存在しません" name |> DbError.notFound)
        | Some defn ->
            result {
                let! () =
                    match
                        state.Catalog.Tables
                        |> List.filter (fun t -> not (Catalog.eqName t.Name defn.Name))
                        |> List.tryFind (fun t ->
                            t.ForeignKeys |> List.exists (fun fk -> Catalog.eqName fk.ParentTable defn.Name))
                    with
                    | Some child ->
                        sprintf "テーブル %s は %s の外部キーから参照されているため削除できません" defn.Name child.Name
                        |> DbError.constraintViolation
                        |> err
                    | None -> ret ()

                let! schema = Catalog.toSchema defn

                let withIndexes =
                    { schema with
                        Indexes = Catalog.indexSchemasOf schema.Name state.Catalog
                    }

                let spaces =
                    Keyspace.table schema.Name
                    :: (withIndexes.Indexes |> List.map (fun index -> Keyspace.index index.Name))

                let! () = DropCleanup.add state.Store.Root spaces

                let! next =
                    match ExecState.saved (Catalog.removeTable name state.Catalog) state with
                    | Ok next -> ret next
                    | Error e ->
                        DropCleanup.clear state.Store.Root |> ignore
                        err e

                match spaces |> foldOk (fun st sp -> ExecState.clearSpaceChunksResult sp st) next with
                | Ok cleaned ->
                    DropCleanup.clear state.Store.Root |> ignore
                    return cleaned
                | Error _ -> return next
            }

    /// DROP TABLE
    /// （後始末をジャーナルに書いてからカタログを削除する）。
    let dropTable (name: string) (ifExists: bool) (state: ExecState) =
        dropTableCore name ifExists |> ExecState.exclusiveDdl state

    /// 索引キーの重複を走査して検査する。
    [<TailCall>]
    let rec private checkUniqueEntries (idx: IndexSchema) (previous: byte array option) (scan: SstScan) =
        match SstScan.next scan with
        | Ok None -> ret ()
        | Ok(Some(e, rest)) ->
            match OrderPreserving.readValues idx.Columns.Length (Cursor.create e.Key) with
            | Ok(values, _) ->
                if values |> Array.exists SqlValue.isNull then
                    checkUniqueEntries idx previous rest
                else
                    match KeyBuilder.ofValues values with
                    | Ok prefix ->
                        match previous with
                        | Some before when Bytes.compare before prefix = 0 ->
                            err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                        | _ -> checkUniqueEntries idx (Some prefix) rest
                    | Error e -> err e
            | Error e -> err e
        | Error e -> err e

    /// 索引 keyspace の UNIQUE 制約を検査する。
    let private checkUniqueIndex (idx: IndexSchema) (store: RowStore) =
        RowStore.scanKeys (Keyspace.index idx.Name) [||] 0 store
        >>= fun scan -> SstScan.withScan scan (fun scan -> checkUniqueEntries idx None scan)

    /// 後始末の失敗を無視して keyspace を空にする。
    let private clearSpaceIgnoringError (state: ExecState) (space: string) =
        match ExecState.clearSpaceChunksResult space state with
        | Ok next -> next
        | Error _ -> state

    /// 索引 keyspace に全行の索引エントリを書き込む。
    let private backfillIndex (tableName: string) (idx: IndexSchema) (state: ExecState) =
        match Catalog.findTable tableName state.Catalog with
        | None -> ret state
        | Some defn ->
            result {
                let! schema = Catalog.toSchema defn

                let withIndex =
                    { schema with
                        Indexes = idx :: schema.Indexes
                    }

                let space = Keyspace.index idx.Name
                let! cleared = ExecState.clearSpaceChunksResult space state
                let! rows = Dml.scanByPrimaryKey cleared.Store withIndex 0 None

                let! filled =
                    ExecState.applyOpChunks
                        (fun txn row ->
                            RowKey.indexKey withIndex idx row
                            |> Result.map (fun key -> Transaction.put space key Array.empty txn))
                        rows
                        cleared

                if idx.Unique then
                    match checkUniqueIndex idx filled.Store with
                    | Ok() -> return filled
                    | Error e ->
                        clearSpaceIgnoringError filled space |> ignore
                        return! err e
                else
                    return filled
            }

    /// CREATE INDEX 本体（排他は呼び出し側で取る）。
    let private createIndexCore (ci: CreateIndex) (state: ExecState) =
        match Catalog.findTable ci.Table state.Catalog with
        | None -> err (sprintf "テーブル %s が存在しません" ci.Table |> DbError.notFound)
        | Some tableDef ->
            result {
                do! Keyspace.validateName "索引" ci.Name

                do!
                    if Catalog.isReservedIndexName ci.Name then
                        err (DbError.name (sprintf "索引名 %s は予約されています" ci.Name))
                    else
                        ret ()

                let! () =
                    match Catalog.findIndex ci.Name state.Catalog with
                    | Some _ -> err (sprintf "索引 %s は既に存在します" ci.Name |> DbError.alreadyExists)
                    | None -> ret ()

                do!
                    if List.isEmpty ci.Columns then
                        err (DbError.constraintViolation (sprintf "索引 %s の列が空です" ci.Name))
                    else
                        ret ()

                let! () =
                    match SqlName.duplicate ci.Columns with
                    | Some name -> err (DbError.name (sprintf "索引 %s の列 %s が重複しています" ci.Name name))
                    | None -> ret ()

                let missing =
                    ci.Columns
                    |> List.filter (fun n -> tableDef.Columns |> List.exists (fun c -> Catalog.eqName n c.Name) |> not)

                do!
                    if not (List.isEmpty missing) then
                        err (
                            sprintf "テーブル %s に列 %s がありません" ci.Table (System.String.Join(", ", missing))
                            |> DbError.notFound
                        )
                    else
                        ret ()

                let defn =
                    {
                        IndexName = ci.Name
                        TableName = ci.Table
                        Columns = ci.Columns
                        Unique = ci.Unique
                    }

                let idxSchema = Catalog.toIndexSchema defn
                let next = Catalog.addIndex defn state.Catalog
                let! filled = backfillIndex ci.Table idxSchema state
                return! ExecState.saved next filled
            }

    /// CREATE INDEX
    /// （索引 keyspace を空にしてから backfill する）。
    let createIndex (ci: CreateIndex) (state: ExecState) =
        match Catalog.findIndex ci.Name state.Catalog with
        | Some _ when ci.IfNotExists -> ret state
        | _ -> createIndexCore ci |> ExecState.exclusiveDdl state

    /// DROP INDEX。
    /// 先にカタログから定義を外して索引を見えなくし、
    /// その後で索引 keyspace をチャンク単位で tombstone 消去する。
    /// 後始末が中断しても、次回の CREATE INDEX が同名 keyspace を空にしてから作る。
    let private dropIndexCore (name: string) (ifExists: bool) (state: ExecState) =
        match Catalog.findIndex name state.Catalog with
        | None ->
            if ifExists then
                ret state
            else
                err (sprintf "索引 %s が存在しません" name |> DbError.notFound)
        | Some defn when Catalog.isReservedIndexName defn.IndexName ->
            err (DbError.constraintViolation (sprintf "索引 %s は UNIQUE 制約用のため DROP できません" defn.IndexName))
        | Some defn ->
            result {
                let space = Keyspace.index defn.IndexName
                let! () = DropCleanup.add state.Store.Root [ space ]

                let! next =
                    match ExecState.saved (Catalog.removeIndex name state.Catalog) state with
                    | Ok next -> ret next
                    | Error e ->
                        DropCleanup.clear state.Store.Root |> ignore
                        err e

                match ExecState.clearSpaceChunksResult space next with
                | Ok cleaned ->
                    DropCleanup.clear state.Store.Root |> ignore
                    return cleaned
                | Error _ -> return next
            }

    /// DROP INDEX。
    let dropIndex (name: string) (ifExists: bool) (state: ExecState) =
        dropIndexCore name ifExists |> ExecState.exclusiveDdl state

    /// 列を追加するときの既存行の値。
    /// DEFAULT を評価して列の型へ強制する（無ければ NULL）。
    let private addedColumnValue (col: ColumnDefinition) =
        match col.Constraint.Default with
        | Some d ->
            Eval.eval Schema.Empty [||] d
            >>= fun value -> SqlValue.coerce col.DataType value
        | None -> ret SqlValue.Null

    /// ALTER で行数を合わせるときの、想定外の列数のエラー。
    let private widthError (schema: TableSchema) (row: SqlValue array) (expected: int) =
        sprintf "テーブル %s の行の列数が想定外です（%d 列、期待 %d 列）" schema.Name row.Length expected
        |> DbError.corruption
        |> err

    /// keyspace の走査を畳み込んで transaction に積む。
    [<TailCall>]
    let rec private rewriteSpaceLoop
        (space: string)
        (transform: SqlValue array -> Result<SqlValue array option, DbError>)
        (txn: Transaction)
        (scan: SstScan)
        =
        match SstScan.next scan with
        | Ok None -> ret txn
        | Ok(Some(e, rest)) ->
            match e.Value with
            | None -> rewriteSpaceLoop space transform txn rest
            | Some row ->
                match transform row with
                | Ok None -> rewriteSpaceLoop space transform txn rest
                | Ok(Some next) ->
                    match Transaction.put space e.Key next txn |> Transaction.checkLimit with
                    | Ok txn2 -> rewriteSpaceLoop space transform txn2 rest
                    | Error e -> err e
                | Error e -> err e
        | Error e -> err e

    /// keyspace の全行を変換して transaction に積む。
    /// `transform` が None を返した行は書き換えない（対象外・変換済み）。
    let private rewriteSpace
        (space: string)
        (transform: SqlValue array -> Result<SqlValue array option, DbError>)
        (txn: Transaction)
        (store: RowStore)
        =
        RowStore.scan space [||] 0 store
        >>= fun scan -> SstScan.withScan scan (fun scan -> rewriteSpaceLoop space transform txn scan)

    /// テーブル keyspace の行変換器を作る。
    let private rewriteTableOf (schema: TableSchema) =
        rewriteSpace (Keyspace.table schema.Name)

    /// 行の書き換えとカタログ更新を 1 transaction で commit する。
    let private commitRewrite
        (state: ExecState)
        (catalog: Catalog)
        (build: Transaction -> Result<Transaction, DbError>)
        =
        let mgr, txn = TransactionManager.beginTxn state.Manager

        result {
            let! txn2 = build txn

            let! store, mgr2 =
                RowStore.commitToStoreWith
                    state.DeferDurability
                    state.Root
                    state.ActiveRegistration
                    mgr
                    txn2
                    state.Store

            return
                { state with
                    Committed =
                        { state.Committed with
                            Store = store
                            Manager = mgr2
                            Catalog = catalog
                        }
                }
        }

    /// 列追加時に既存行へ既定値を付ける。
    let private addColumnTransform
        (schema: TableSchema)
        (oldWidth: int)
        (newWidth: int)
        (pad: SqlValue)
        (row: SqlValue array)
        =
        if row.Length = newWidth then
            ret None
        elif row.Length = oldWidth then
            ret (Some(Array.append row [| pad |]))
        else
            widthError schema row oldWidth

    /// 列追加の行書き換え transaction を組み立てる。
    let private addColumnBuild
        (state: ExecState)
        (tableName: string)
        (withIndexes: TableSchema)
        (nextDefn: TableDefinition)
        (transform: SqlValue array -> Result<SqlValue array option, DbError>)
        (txn: Transaction)
        =
        let nextSchema =
            { withIndexes with
                Columns = nextDefn.Columns
            }

        rewriteTableOf nextSchema transform txn state.Store
        |> Result.map (fun txn -> txn |> Catalog.putTableInTxn nextDefn |> Catalog.removeStatsInTxn tableName)

    /// 列削除時に既存行から 1列を除く。
    let private dropColumnTransform
        (schema: TableSchema)
        (oldWidth: int)
        (newWidth: int)
        (pos: int)
        (row: SqlValue array)
        =
        if row.Length = newWidth then
            ret None
        elif row.Length = oldWidth then
            ret (Some(Array.init newWidth (fun i -> row.[if i < pos then i else i + 1])))
        else
            widthError schema row oldWidth

    /// 列削除の行書き換え transaction を組み立てる。
    let private dropColumnBuild
        (state: ExecState)
        (tableName: string)
        (withIndexes: TableSchema)
        (newColumns: ColumnDefinition list)
        (nextDefn: TableDefinition)
        (transform: SqlValue array -> Result<SqlValue array option, DbError>)
        (txn: Transaction)
        =
        let nextSchema =
            { withIndexes with
                Columns = newColumns
            }

        rewriteTableOf nextSchema transform txn state.Store
        |> Result.map (fun txn -> txn |> Catalog.putTableInTxn nextDefn |> Catalog.removeStatsInTxn tableName)

    /// 列名の変更を 1列へ適用する。
    let private renameColumn (oldName: string) (newName: string) (column: string) =
        if SqlName.equal column oldName then newName else column

    /// 外部キー定義の列名を変更する（参照先が自テーブルの場合だけ親側も変える）。
    let private renameForeignKeyLocal (selfName: string) (rename: string -> string) (fk: ForeignKeyDefinition) =
        { fk with
            Columns = fk.Columns |> List.map rename
            ParentColumns =
                if Catalog.eqName fk.ParentTable selfName then
                    fk.ParentColumns |> List.map rename
                else
                    fk.ParentColumns
        }

    /// 列名変更の transaction を組み立てる。
    let private renameColumnBuild
        (selfName: string)
        (nextDefn: TableDefinition)
        (nextIndexes: IndexDefinition list)
        (referencing: TableDefinition list)
        (txn: Transaction)
        =
        referencing
        |> List.fold
            (fun t childDefn -> Catalog.putTableInTxn childDefn t)
            (nextIndexes
             |> List.fold
                 (fun t idx -> Catalog.putIndexInTxn idx t)
                 (txn |> Catalog.putTableInTxn nextDefn |> Catalog.removeStatsInTxn selfName))
        |> ret

    /// ADD COLUMN の制約を検証する。
    let private validateAddedColumn (schema: TableSchema) (col: ColumnDefinition) =
        result {
            do!
                if (TableSchema.indexOf schema col.Name).IsSome then
                    err (DbError.name (sprintf "列 %s は既に存在します" col.Name))
                else
                    ret ()

            do!
                if col.Constraint.PrimaryKey then
                    err (DbError.constraintViolation "ALTER TABLE ... ADD COLUMN で PRIMARY KEY は指定できません")
                else
                    ret ()

            do!
                if col.Constraint.References.IsSome then
                    err (DbError.constraintViolation "ALTER TABLE ... ADD COLUMN で REFERENCES は指定できません（テーブルを作り直してください）")
                else
                    ret ()

            do!
                if col.Constraint.Unique then
                    err (
                        DbError.constraintViolation
                            "ALTER TABLE ... ADD COLUMN で UNIQUE は指定できません（CREATE UNIQUE INDEX を使ってください）"
                    )
                else
                    ret ()

            do!
                if col.Constraint.NotNull && col.Constraint.Default.IsNone then
                    err (DbError.constraintViolation (sprintf "NOT NULL 列 %s を追加するには DEFAULT が必要です" col.Name))
                else
                    ret ()
        }

    /// 列を追加し、既存行へ既定値を付ける。
    let private alterAddColumn
        (state: ExecState)
        (defn: TableDefinition)
        (schema: TableSchema)
        (withIndexes: TableSchema)
        (col: ColumnDefinition)
        =
        result {
            do! validateAddedColumn schema col

            let! pad = addedColumnValue col
            let oldWidth = schema.Columns.Length
            let newWidth = oldWidth + 1

            let nextDefn =
                { defn with
                    Columns = defn.Columns @ [ col ]
                }

            let transform = addColumnTransform schema oldWidth newWidth pad
            let build = addColumnBuild state defn.Name withIndexes nextDefn transform

            return! commitRewrite state (Catalog.addTable nextDefn (Catalog.removeStats defn.Name state.Catalog)) build
        }

    /// DROP COLUMN の制約を検証する。
    let private validateDropColumn
        (state: ExecState)
        (defn: TableDefinition)
        (schema: TableSchema)
        (withIndexes: TableSchema)
        (pos: int)
        (name: string)
        =
        result {
            do!
                if schema.PrimaryKey |> List.contains pos then
                    err (DbError.constraintViolation (sprintf "主キー列 %s は削除できません（主キーは LSM のキーに使われています）" name))
                else
                    ret ()

            do!
                if
                    defn.ForeignKeys
                    |> List.exists (fun fk -> fk.Columns |> List.exists (fun c -> SqlName.equal c name))
                then
                    err (DbError.constraintViolation (sprintf "列 %s は外部キーで使われているため削除できません" name))
                else
                    ret ()

            do!
                if
                    state.Catalog.Tables
                    |> List.exists (fun t ->
                        t.ForeignKeys
                        |> List.exists (fun fk ->
                            Catalog.eqName fk.ParentTable defn.Name
                            && fk.ParentColumns |> List.exists (fun c -> SqlName.equal c name)))
                then
                    err (DbError.constraintViolation (sprintf "列 %s は外部キーの参照先になっているため削除できません" name))
                else
                    ret ()

            do!
                match
                    withIndexes.Indexes
                    |> List.tryFind (fun idx -> idx.Columns |> List.exists (fun c -> SqlName.equal c name))
                with
                | Some idx when Catalog.isReservedIndexName idx.Name ->
                    err (DbError.constraintViolation (sprintf "列 %s は UNIQUE 制約で使われているため削除できません" name))
                | Some idx -> err (DbError.constraintViolation (sprintf "列 %s は索引 %s で使われているため削除できません" name idx.Name))
                | None -> ret ()
        }

    /// 列を削除し、既存行から値を取り除く。
    let private alterDropColumn
        (state: ExecState)
        (defn: TableDefinition)
        (schema: TableSchema)
        (withIndexes: TableSchema)
        (name: string)
        =
        result {
            let! pos = TableSchema.requireIndexOf schema name
            do! validateDropColumn state defn schema withIndexes pos name

            let oldWidth = schema.Columns.Length
            let newWidth = oldWidth - 1

            let newColumns =
                schema.Columns
                |> List.indexed
                |> List.choose (fun (i, c) -> if i = pos then None else Some c)

            let nextDefn = { defn with Columns = newColumns }
            let transform = dropColumnTransform schema oldWidth newWidth pos

            let build =
                dropColumnBuild state defn.Name withIndexes newColumns nextDefn transform

            return! commitRewrite state (Catalog.addTable nextDefn (Catalog.removeStats defn.Name state.Catalog)) build
        }

    /// 列名の変更を、その列を参照する子テーブルの外部キーへ伝播させる。
    let private renameReferencingTables (catalog: Catalog) (tableName: string) (rename: string -> string) =
        catalog.Tables
        |> List.filter (fun t -> not (Catalog.eqName t.Name tableName))
        |> List.choose (fun t ->
            let renamed =
                t.ForeignKeys
                |> List.map (fun fk ->
                    if Catalog.eqName fk.ParentTable tableName then
                        { fk with
                            ParentColumns = fk.ParentColumns |> List.map rename
                        }
                    else
                        fk)

            if renamed = t.ForeignKeys then
                None
            else
                Some { t with ForeignKeys = renamed })

    /// 列名を変更し、主キー・外部キー・索引定義を追随させる。
    let private alterRenameColumn
        (state: ExecState)
        (defn: TableDefinition)
        (schema: TableSchema)
        (oldName: string)
        (newName: string)
        =
        result {
            let! pos = TableSchema.requireIndexOf schema oldName

            do!
                if
                    schema.Columns
                    |> List.indexed
                    |> List.exists (fun (i, c) -> i <> pos && SqlName.equal c.Name newName)
                then
                    err (DbError.name (sprintf "列 %s は既に存在します" newName))
                else
                    ret ()

            let rename = renameColumn oldName newName
            let renameLocal = renameForeignKeyLocal defn.Name rename
            let referencing = renameReferencingTables state.Catalog defn.Name rename

            let nextDefn =
                { defn with
                    Columns =
                        schema.Columns
                        |> List.mapi (fun i c -> if i = pos then { c with Name = newName } else c)
                    PrimaryKeyColumns = defn.PrimaryKeyColumns |> List.map rename
                    ForeignKeys = defn.ForeignKeys |> List.map renameLocal
                }

            let nextIndexes =
                Catalog.indexesOf defn.Name state.Catalog
                |> List.map (fun idx ->
                    { idx with
                        Columns = idx.Columns |> List.map rename
                    })

            let nextCatalog =
                referencing
                |> List.fold
                    (fun cat t -> Catalog.addTable t cat)
                    (nextIndexes
                     |> List.fold
                         (fun cat i -> Catalog.addIndex i cat)
                         (Catalog.addTable nextDefn (Catalog.removeStats defn.Name state.Catalog)))

            let build = renameColumnBuild defn.Name nextDefn nextIndexes referencing

            return! commitRewrite state nextCatalog build
        }

    /// `ALTER TABLE`。
    /// <remarks>
    /// 行の書き換え（列の追加・削除）とカタログ更新を **1つの transaction** で commit する。
    /// 途中で失敗しても何も変わらない（行とカタログがずれた状態を残さない）。
    /// そのため 1 transaction のバッファ上限（256MiB）を超えるテーブルは `CapacityError` になる。
    /// 主キーは LSM のキーなので、主キー列の追加・削除は許可しない。
    /// </remarks>
    let private alterTableCore (a: AlterTable) (state: ExecState) =
        match Catalog.findTable a.Table state.Catalog with
        | None -> err (sprintf "テーブル %s が存在しません" a.Table |> DbError.notFound)
        | Some defn ->
            result {
                let! schema = Catalog.toSchema defn

                let withIndexes =
                    { schema with
                        Indexes = Catalog.indexSchemasOf defn.Name state.Catalog
                    }

                match a.Action with
                | AlterTableAction.AddColumn col -> return! alterAddColumn state defn schema withIndexes col
                | AlterTableAction.DropColumn name -> return! alterDropColumn state defn schema withIndexes name
                | AlterTableAction.RenameColumn(oldName, newName) ->
                    return! alterRenameColumn state defn schema oldName newName
            }

    /// ALTER TABLE（ADD / DROP / RENAME COLUMN）。
    /// カタログと行を 1 transaction で更新する。
    let alterTable (a: AlterTable) (state: ExecState) =
        alterTableCore a |> ExecState.exclusiveDdl state
