// ===========================================================================
// Engine — 文の実行層（Ddl / Engine）。
// ===========================================================================

namespace RykDb.Core

/// 文の実行結果。
type StatementResult =
    {
        RowsAffected: int
        Columns: string array
        Rows: SqlValue array array
    }

    static member empty =
        {
            RowsAffected = 0
            Columns = [||]
            Rows = [||]
        }

    member this.ToDisplayLines() =
        if Array.isEmpty this.Columns then
            [ sprintf "(%d rows)" this.RowsAffected ]
        else
            let header = System.String.Join(" | ", this.Columns)

            let body =
                this.Rows
                |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ", " >> fun s -> "  " + s)

            header :: System.String('-', header.Length) :: Array.toList body

// 書き込みは Transaction にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
module internal DmlOps =
    /// WHERE を評価するときの対象スキーマ（UPDATE / DELETE で共通）。
    let private targetSchemaOf (schema: TableSchema) = Schema.ofTable schema.Name None schema

    /// WHERE の述語。
    /// WHERE が無ければ「全部通過」（UPDATE / DELETE で共通）。
    let private matchesWhere (targetSchema: Schema) (row: SqlValue array) =
        function
        | Some w -> Eval.matches targetSchema row w
        | None -> ret true

    /// 値式を列位置に対応付け、型強制して (位置, 値) のリストにする。
    /// INSERT は行の外（定数）で、UPDATE は現在行を参照して評価するので、
    /// 評価器 evalOne を受け取って両方を同じ経路で処理する。
    let private gatherWith
        (evalOne: Expr -> Result<SqlValue, DbError>)
        (schema: TableSchema)
        (values: Expr list)
        (idxs: int list)
        =
        let rec gather acc vs is_ =
            match vs, is_ with
            | [], [] -> ret acc
            | v :: rest, i :: restIdx ->
                match evalOne v with
                | Error e -> Error e
                | Ok value ->
                    let colDef = schema.Columns.[i]

                    match SqlValue.coerce colDef.DataType value with
                    | Error e -> Error e
                    | Ok coerced -> gather ((i, coerced) :: acc) rest restIdx
            | _ -> ret acc

        gather [] values idxs

    /// gather で埋まった `(列位置, 値)` の対を、列順の行へ戻す。
    /// 埋まっていない位置は fallback に委ねる
    /// （INSERT は DEFAULT / NULL、UPDATE は元の値）。
    let private scatter (width: int) (filled: (int * SqlValue) list) (fallback: int -> SqlValue) =
        let values = Map.ofList filled

        Array.init width (fun i ->
            match Map.tryFind i values with
            | Some v -> v
            | None -> fallback i)

    /// INSERT。
    /// columns が None ならテーブル定義の順に並べる。
    /// DEFAULT / NULL の充足と型強制はここで行う。
    /// TableSchema.validateRow が NOT NULL と型を最終確認する。
    let insert
        (schema: TableSchema)
        (ctx: ExecContext)
        (txn: Transaction)
        (columns: string list option)
        (rows: Expr list list)
        =
        let evalConstant = Eval.eval Schema.empty [||]

        let indexValues (index: IndexSchema) (row: SqlValue array) =
            index.Columns
            |> List.map (TableSchema.requireIndexOf schema)
            |> all
            |> Result.map (fun positions -> positions |> List.map (fun i -> row.[i]) |> List.toArray)

        let rec check txn row pk =
            function
            | [] -> ret ()
            | idx :: rest ->
                match indexValues idx row with
                | Error e -> Error e
                | Ok values when values |> Array.exists SqlValue.isNull -> check txn row pk rest
                | Ok values ->
                    match Dml.scanByIndexPrefixWithTxn ctx.Store schema idx ctx.Visible txn values 1 with
                    | Error e -> Error e
                    | Ok scan ->
                        match Op.toArray scan with
                        | Error e -> Error e
                        | Ok rows ->
                            if
                                rows
                                |> Array.exists (fun r ->
                                    let existingPk = TableSchema.primaryKeyOf schema r
                                    not (Array.forall2 (=) existingPk pk))
                            then
                                err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                            else
                                check txn row pk rest

        let checkUnique txn row =
            let pk = TableSchema.primaryKeyOf schema row

            match Dml.getByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn pk with
            | Error e -> Error e
            | Ok(Some _) -> err (DbError.constraintViolation (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name))
            | Ok None -> check txn row pk (schema.Indexes |> List.filter _.Unique)

        let buildRow width idxs (defaults: SqlValue array) values =
            if List.length values <> List.length idxs then
                Error(
                    DbError.create
                        ErrorKind.SyntaxError
                        (sprintf "INSERT の列数(%d)と値の数(%d)が合いません" (List.length idxs) (List.length values))
                )
            else
                gatherWith evalConstant schema values idxs
                |> Result.map (fun filled -> scatter width filled (fun i -> defaults.[i]))

        result {
            let! idxs =
                match columns with
                | None -> schema.Columns |> List.mapi (fun i _ -> i) |> ret
                | Some names ->
                    match SqlName.duplicate names with
                    | Some name -> err (DbError.name (sprintf "INSERT の列 %s が重複しています" name))
                    | None -> names |> List.map (TableSchema.requireIndexOf schema) |> all

            let supplied = Set.ofList idxs

            let! defaultValues =
                schema.Columns
                |> List.mapi (fun i col ->
                    if Set.contains i supplied then
                        ret SqlValue.Null
                    else
                        match col.Constraint.Default with
                        | Some d -> evalConstant d
                        | None -> ret SqlValue.Null)
                |> all

            let defaults = List.toArray defaultValues

            return!
                rows
                |> foldOk
                    (fun (txnAcc, count) values ->
                        result {
                            let! row = buildRow schema.Columns.Length idxs defaults values
                            do! checkUnique txnAcc row
                            let! t = Dml.insert schema txnAcc row
                            return t, count + 1
                        })
                    (txn, 0)
        }

    /// UPDATE。
    /// assignments の右辺は現在行で評価する。
    /// 検索キーは更新前の行の主キー。
    /// 主キー列を書き換えると古い位置の行が残って重複する。
    let update
        (schema: TableSchema)
        (ctx: ExecContext)
        (txn: Transaction)
        (assignments: (string * Expr) list)
        (where: Expr option)
        =
        let assign targetSchema targets row =
            gatherWith (Eval.eval targetSchema row) schema (List.map snd assignments) targets
            |> Result.map (fun filled -> scatter row.Length filled (fun i -> row.[i]))

        let targetSchema = targetSchemaOf schema

        let rowBytes row =
            int64 (Entry.approxBytes (Entry.put [||] 0UL row))

        let rec go targets acc bytes cur =
            match Op.next cur with
            | Error e -> Error e
            | Ok None -> Ok(List.rev acc)
            | Ok(Some(row, next)) ->
                match matchesWhere targetSchema row where with
                | Error e -> Error e
                | Ok false -> go targets acc bytes next
                | Ok true ->
                    match assign targetSchema targets row with
                    | Error e -> Error e
                    | Ok newRow ->
                        let bytes = bytes + rowBytes row + rowBytes newRow

                        if bytes > txn.LimitBytes then
                            err (DbError.capacity (sprintf "UPDATE の対象行が transaction の上限（%d バイト）を超えました" txn.LimitBytes))
                        else
                            go targets ((row, newRow) :: acc) bytes next

        let collectUpdates txn targets =
            result {
                let! scan = Dml.scanByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn 0
                return! go targets [] 0L scan
            }

        let validateUniqueUpdates txn updates =
            let updatedPks =
                updates
                |> List.map (fun (_, newRow) -> TableSchema.primaryKeyOf schema newRow)
                |> Set.ofList

            let checkExisting idx seen key values =
                result {
                    let! scan =
                        Dml.scanByIndexPrefixWithTxn ctx.Store schema idx ctx.Visible txn values 0

                    let! rows = Op.toArray scan

                    if
                        rows
                        |> Array.exists (fun row ->
                            let existingPk = TableSchema.primaryKeyOf schema row
                            not (Set.contains existingPk updatedPks))
                    then
                        return! err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                    else
                        return Set.add key seen
                }

            let checkIndex (idx: IndexSchema) =
                result {
                    let! positions = idx.Columns |> List.map (TableSchema.requireIndexOf schema) |> all

                    let! _ =
                        updates
                        |> foldOk
                            (fun seen (_, newRow) ->
                                let values = positions |> List.map (fun i -> newRow.[i]) |> List.toArray

                                if values |> Array.exists SqlValue.isNull then
                                    ret seen
                                else
                                    let key = KeyBuilder.ofValues values

                                    if Set.contains key seen then
                                        err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                                    else
                                        checkExisting idx seen key values)
                            Set.empty

                    return ()
                }

            schema.Indexes
            |> List.filter _.Unique
            |> foldOk (fun () idx -> checkIndex idx) ()

        match assignments |> List.map fst |> SqlName.duplicate with
        | Some name -> err (DbError.name (sprintf "UPDATE の列 %s が重複しています" name))
        | None ->
            assignments
            |> List.map (fun (name, _) -> TableSchema.requireIndexOf schema name)
            |> all
            >>= fun targets ->
                result {
                    let! updates = collectUpdates txn targets
                    do! validateUniqueUpdates txn updates

                    let! txn2 =
                        updates
                        |> foldOk (fun current (oldRow, newRow) -> Dml.updateRow schema current oldRow newRow) txn

                    return txn2, List.length updates
                }

    /// DELETE。
    /// WHERE に合う行を主キー + 全索引に tombstone で消す。
    let delete (schema: TableSchema) (ctx: ExecContext) (txn: Transaction) (where: Expr option) =
        let foldRows schema (ctx: ExecContext) txn matched onRow =
            let rec go t c cur =
                match Op.next cur with
                | Error e -> Error e
                | Ok None -> ret (t, c)
                | Ok(Some(row, nxt)) ->
                    match matched row with
                    | Error e -> Error e
                    | Ok false -> go t c nxt
                    | Ok true ->
                        match onRow t c row with
                        | Error e -> Error e
                        | Ok(tc, c') -> go tc c' nxt

            result {
                let! scan = Dml.scanByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn 0
                return! go txn 0 scan
            }

        let targetSchema = targetSchemaOf schema

        foldRows schema ctx txn (fun row -> matchesWhere targetSchema row where) (fun t count row ->
            Dml.deleteRow schema t row |> Result.map (fun t2 -> t2, count + 1))

/// DB の状態。
type internal DbState =
    {
        Root: string
        Store: RowStore
        Catalog: Catalog
        Manager: TransactionManager
        Active: Transaction option
        Pending: Transaction option
        /// 進行中 transaction の ActiveSnapshot 登録。
        /// 他のハンドルの compaction が、この snapshot より新しい版だけを残して
        /// 古い版を捨ててしまわないようにするために登録する。
        /// 同じ transaction の state は複数のハンドルが共有できる（登録側で生存を数える）。
        ActiveRegistration: ActiveRegistration option
    }

/// Transaction を commit して DbState に反映する共通処理。
module internal DbStateOps =
    let commitTransaction (txn: Transaction) (state: DbState) =
        result {
            let! store, manager =
                RowStore.commitToStore state.Root state.ActiveRegistration state.Manager txn state.Store

            return
                { state with
                    Store = store
                    Manager = manager
                    Active = None
                    Pending = None
                    ActiveRegistration = None
                }
        }

module internal Ddl =
    /// カタログを保存し、保存後の Store と新しい Catalog で state を作り直す。
    /// （CREATE / DROP すべてが使う「保存 → state 更新」の定型）
    let private saved (next: Catalog) (state: DbState) =
        result {
            let! store = Catalog.save state.Store state.Catalog next

            return
                { state with
                    Store = store
                    Catalog = next
                }
        }

    /// DDL を排他の内側で実行する。
    /// <remarks>
    /// - 書き込み中の文があれば、それが終わるまで待つ（`RootGate` の共有ロック）。
    /// - 実行中の transaction があれば **待たずに ConflictError** にする。
    ///   古いスキーマで計算した索引エントリを commit されると、索引に載らない行ができるため。
    ///   待つと、commit されないまま残った transaction で永久に止まるので失敗側に倒す。
    /// - 新しい transaction の開始と文の実行は排他の間ブロックされる（実行後は新しいカタログを見る）。
    /// </remarks>
    let private exclusiveDdl (run: DbState -> Result<DbState, DbError>) (state: DbState) =
        RootGate.withExclusive state.Root (fun () ->
            match ActiveSnapshotRegistry.oldestExcept state.Root state.ActiveRegistration with
            | Some _ -> err (DbError.conflict "実行中の transaction があります。COMMIT または ROLLBACK してから DDL を実行してください")
            | None -> run state)

    let private BackfillChunkRows = 8192

    let private applyChunks
        (apply: Transaction -> 'a -> Result<Transaction, DbError>)
        (chunks: seq<'a>)
        (current: DbState)
        =
        chunks
        |> foldOk
            (fun st chunk ->
                let mgr, txn =
                    TransactionManager.beginTxnWithLimit (System.Int64.MaxValue |> Some) st.Manager

                result {
                    let! txn2 = apply txn chunk

                    let! store, mgr2 =
                        RowStore.commitToStore st.Root st.ActiveRegistration mgr txn2 st.Store

                    return
                        { st with
                            Store = store
                            Manager = mgr2
                        }
                })
            current

    /// keyspace の全エントリをチャンク単位で tombstone 消去する。
    let private clearSpaceChunksResult (space: string) (state: DbState) =
        if not (RowStore.keyspaceExists state.Store space) then
            ret state
        else
            match RowStore.scanAt state.Store space [||] Snapshot.NoLimit 0 with
            | Error e -> Error e
            | Ok stale ->
                applyChunks
                    (fun txn chunk -> foldOk (fun t (key, _) -> ret (Transaction.remove space key t)) txn chunk)
                    (Seq.chunkBySize BackfillChunkRows stale)
                    state

    let createTable (c: CreateTable) (state: DbState) =
        let createTableCore (c: CreateTable) state =
            let existing = Catalog.findTable state.Catalog c.Name

            match existing, c.IfNotExists with
            | Some _, true -> ret state
            | Some _, false -> err (sprintf "テーブル %s は既に存在します" c.Name |> DbError.alreadyExists)
            | None, _ ->
                result {
                    do! Keyspace.validateName "テーブル" c.Name
                    let! schema = TableSchema.ofCreateTable c
                    let defn = Catalog.toDefinition schema

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

                    let catalog1 = Catalog.addTable state.Catalog defn

                    let catalog2 =
                        implicitIndexes |> List.fold (fun cat idx -> Catalog.addIndex cat idx) catalog1

                    let spaces =
                        Keyspace.table c.Name
                        :: (implicitIndexes |> List.map (fun idx -> Keyspace.index idx.IndexName))

                    let! cleaned =
                        spaces |> foldOk (fun st space -> clearSpaceChunksResult space st) state

                    return! saved catalog2 cleaned
                }

        exclusiveDdl (createTableCore c) state

    /// DROP TABLE。
    /// 手順:
    ///   1. 先にカタログから定義を外してテーブルを見えなくする
    ///   2. テーブルと全索引 keyspace の全エントリをチャンク単位で tombstone 消去する
    /// 後始末が中断しても、次回の CREATE TABLE が同名 keyspace を空にしてから作る。
    let dropTable (name: string) (ifExists: bool) (state: DbState) =
        let dropTableCore name ifExists state =
            match Catalog.findTable state.Catalog name with
            | None ->
                if ifExists then
                    ret state
                else
                    err (sprintf "テーブル %s が存在しません" name |> DbError.notFound)
            | Some defn ->
                result {
                    let! schema = Catalog.toSchema defn

                    let withIndexes =
                        { schema with
                            Indexes = Catalog.indexSchemasOf state.Catalog schema.Name
                        }

                    let spaces =
                        Keyspace.table schema.Name
                        :: (withIndexes.Indexes |> List.map (fun index -> Keyspace.index index.Name))

                    let! () = DropCleanup.add state.Store.Root spaces

                    let! next =
                        match saved (Catalog.removeTable state.Catalog name) state with
                        | Ok next -> ret next
                        | Error e ->
                            DropCleanup.clear state.Store.Root |> ignore
                            Error e

                    match spaces |> foldOk (fun st space -> clearSpaceChunksResult space st) next with
                    | Ok cleaned ->
                        DropCleanup.clear state.Store.Root |> ignore
                        return cleaned
                    | Error _ -> return next
                }

        exclusiveDdl (dropTableCore name ifExists) state

    let createIndex (ci: CreateIndex) (state: DbState) =
        let checkEntries positions (idx: IndexSchema) (entries: seq<byte array * SqlValue array option>) =
            use e = entries.GetEnumerator()

            let rec loop previous =
                if not (e.MoveNext()) then
                    ret ()
                else
                    let _, value = e.Current

                    match value with
                    | None -> loop previous
                    | Some row ->
                        let values = positions |> List.map (fun i -> row.[i]) |> List.toArray

                        if values |> Array.exists SqlValue.isNull then
                            loop previous
                        else
                            let prefix = KeyBuilder.ofValues values

                            match previous with
                            | Some before when Bytes.compare before prefix = 0 ->
                                err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                            | _ -> loop (Some prefix)

            loop None

        let checkUniqueIndex schema (idx: IndexSchema) store =
            let positions = idx.Columns |> List.map (TableSchema.requireIndexOf schema) |> all

            match positions with
            | Error e -> Error e
            | Ok pos ->
                RowStore.scanAt store (Keyspace.index idx.Name) [||] Snapshot.NoLimit 0
                >>= checkEntries pos idx

        let clearSpaceChunks space state =
            match clearSpaceChunksResult space state with
            | Ok next -> next
            | Error _ -> state

        let backfillIndex tableName idx state =
            match Catalog.findTable state.Catalog tableName with
            | None -> ret state
            | Some defn ->
                result {
                    let! schema = Catalog.toSchema defn

                    let withIndex =
                        { schema with
                            Indexes = idx :: schema.Indexes
                        }

                    let space = Keyspace.index idx.Name
                    let indexKeyFor row = RowKey.indexKey withIndex idx row

                    let! cleared = clearSpaceChunksResult space state
                    let! rows = Dml.scanByPrimaryKey cleared.Store withIndex Snapshot.NoLimit 0

                    let! filled =
                        applyChunks
                            (fun txn chunk ->
                                foldOk (fun t row -> ret (Transaction.put space (indexKeyFor row) row t)) txn chunk)
                            (Seq.chunkBySize BackfillChunkRows rows)
                            cleared

                    if idx.Unique then
                        match checkUniqueIndex withIndex idx filled.Store with
                        | Ok() -> return filled
                        | Error e ->
                            clearSpaceChunks space filled |> ignore
                            return! Error e
                    else
                        return filled
                }

        let createIndexCore ci state =
            match Catalog.findTable state.Catalog ci.Table with
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
                        match Catalog.findIndex state.Catalog ci.Name with
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
                        |> List.filter (fun n ->
                            tableDef.Columns |> List.exists (fun c -> Catalog.eqName n c.Name) |> not)

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
                    let next = Catalog.addIndex state.Catalog defn
                    let! filled = backfillIndex ci.Table idxSchema state
                    return! saved next filled
                }

        exclusiveDdl (createIndexCore ci) state

    /// DROP INDEX。
    /// 先にカタログから定義を外して索引を見えなくし、
    /// その後で索引 keyspace をチャンク単位で tombstone 消去する。
    /// 後始末が中断しても、次回の CREATE INDEX が同名 keyspace を空にしてから作る。
    let dropIndex (name: string) (ifExists: bool) (state: DbState) =
        let dropIndexCore name ifExists state =
            match Catalog.findIndex state.Catalog name with
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
                        match saved (Catalog.removeIndex state.Catalog name) state with
                        | Ok next -> ret next
                        | Error e ->
                            DropCleanup.clear state.Store.Root |> ignore
                            Error e

                    match clearSpaceChunksResult space next with
                    | Ok cleaned ->
                        DropCleanup.clear state.Store.Root |> ignore
                        return cleaned
                    | Error _ -> return next
                }

        exclusiveDdl (dropIndexCore name ifExists) state

module internal Engine =
    let runStatement (state: DbState) (txn: Transaction) =
        let addTable ctx t =
            match Catalog.toSchema t with
            | Ok schema ->
                let withIndexes =
                    { schema with
                        Indexes = Catalog.indexSchemasOf state.Catalog t.Name
                    }

                ret (ExecContext.withSchema ctx t.Name withIndexes)
            | Error e -> Error e

        let contextOf state txn =
            let ctx = ExecContext.create state.Store (Snapshot.toVisible txn.Snapshot)
            let ctx = ExecContext.withTransaction ctx txn
            let ctx = ExecContext.withCatalog ctx state.Catalog
            state.Catalog.Tables |> foldOk addTable ctx

        let runSelect sel =
            contextOf state txn
            >>= fun ctx ->
                Plan.runSelect ctx sel
                |> Result.map (fun (columns, rows) ->
                    let result =
                        {
                            RowsAffected = rows.Length
                            Columns = columns
                            Rows = rows
                        }

                    state, result)

        let runInsert (st: InsertStatement) ctx txn =
            ExecContext.schemaOf ctx st.Table.Name
            >>= fun schema -> DmlOps.insert schema ctx txn st.Columns st.Rows

        let runUpdate (st: UpdateStatement) ctx txn =
            ExecContext.schemaOf ctx st.Table.Name
            >>= fun schema -> DmlOps.update schema ctx txn st.Assignments st.Where

        let runDelete (st: DeleteStatement) ctx txn =
            ExecContext.schemaOf ctx st.Table.Name
            >>= fun schema -> DmlOps.delete schema ctx txn st.Where

        let runDml run =
            result {
                let! ctx = contextOf state txn
                let! outcome = run ctx txn
                let txn2, n = outcome

                return
                    { state with Pending = Some txn2 },
                    { StatementResult.empty with
                        RowsAffected = n
                    }
            }

        let runDdl run =
            if state.Active.IsSome then
                err (
                    DbError.create
                        ErrorKind.SyntaxError
                        "トランザクション中は DDL（CREATE / DROP）を実行できません。COMMIT または ROLLBACK してから実行してください"
                )
            else
                run state

        let noRows = Result.map (fun s -> s, StatementResult.empty)

        function
        | Statement.Select sel -> runSelect sel
        | Statement.Insert st -> runInsert st |> runDml
        | Statement.Update st -> runUpdate st |> runDml
        | Statement.Delete st -> runDelete st |> runDml
        | Statement.CreateTable c -> Ddl.createTable c |> runDdl |> noRows
        | Statement.DropTable(n, ifE) -> Ddl.dropTable n ifE |> runDdl |> noRows
        | Statement.CreateIndex ci -> Ddl.createIndex ci |> runDdl |> noRows
        | Statement.DropIndex(n, ifE) -> Ddl.dropIndex n ifE |> runDdl |> noRows
        | Statement.Begin
        | Statement.Commit
        | Statement.Rollback -> ret state |> noRows
