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

    static member Empty =
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
    let private saved (state: DbState) (next: Catalog) =
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
    let private exclusiveDdl (state: DbState) (run: DbState -> Result<DbState, DbError>) =
        RootGate.withExclusive state.Root (fun () ->
            match ActiveSnapshotRegistry.oldestExcept state.Root state.ActiveRegistration with
            | Some _ -> err (DbError.conflict "実行中の transaction があります。COMMIT または ROLLBACK してから DDL を実行してください")
            | None -> run state)

    let private BackfillChunkRows = 8192

    /// 1チャンク分を transaction にして commit する。
    let private commitChunk (apply: Transaction -> 'a -> Result<Transaction, DbError>) (chunk: 'a list) (st: DbState) =
        let mgr, txn =
            TransactionManager.beginTxnWithLimit (System.Int64.MaxValue |> Some) st.Manager

        result {
            let! txn2 = foldOk apply txn chunk

            let! store, mgr2 =
                RowStore.commitToStore st.Root st.ActiveRegistration mgr txn2 st.Store

            return
                { st with
                    Store = store
                    Manager = mgr2
                }
        }

    /// 走査から `size` 件までを取り出す。
    let private takeScanChunk =
        let rec loop acc n scan =
            if n <= 0 then
                ret (List.rev acc, scan)
            else
                match SstScan.next scan with
                | Error e -> err e
                | Ok None -> ret (List.rev acc, SstScan.Empty)
                | Ok(Some(e, rest)) -> loop (e :: acc) (n - 1) rest

        loop []

    /// Op から `size` 行までを取り出す。
    let private takeOpChunk =
        let rec loop acc n op =
            if n <= 0 then
                ret (List.rev acc, op)
            else
                match Op.next op with
                | Error e -> err e
                | Ok None -> ret (List.rev acc, Op.Done)
                | Ok(Some(row, rest)) -> loop (row :: acc) (n - 1) rest

        loop []

    /// Op をチャンク単位で消費する（全件を実体化しない）。
    let private applyOpChunks (apply: Transaction -> SqlValue array -> Result<Transaction, DbError>) =
        let rec loop op st =
            match takeOpChunk BackfillChunkRows op with
            | Error e -> err e
            | Ok([], _) -> ret st
            | Ok(chunk, rest) ->
                match commitChunk apply chunk st with
                | Error e -> err e
                | Ok st' -> loop rest st'

        loop

    /// keyspace の全エントリをチャンク単位で tombstone 消去する。
    /// <remarks>
    /// 同じ keyspace を走査しながら書くため、1チャンク書くたびに新しい store で
    /// `lastKey` から走査し直す。走査中に compaction がファイルを消しても、
    /// 古いスナップショットのファイルを開き続けない。
    /// </remarks>
    let private clearSpaceChunksResult (state: DbState) (space: string) =
        let rec loop st startKey =
            match RowStore.scan space startKey 0 st.Store with
            | Error e -> err e
            | Ok scan ->
                match takeScanChunk BackfillChunkRows scan with
                | Error e -> err e
                | Ok([], _) -> ret st
                | Ok(chunk, _) ->
                    let lastKey = (List.last chunk).Key

                    match
                        commitChunk (fun txn key -> ret (Transaction.remove space key txn)) (chunk |> List.map _.Key) st
                    with
                    | Error e -> err e
                    | Ok st' -> loop st' lastKey

        loop state [||]

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

                    let catalog1 = Catalog.toDefinition schema |> Catalog.addTable state.Catalog

                    let catalog2 =
                        implicitIndexes |> List.fold (fun cat idx -> Catalog.addIndex cat idx) catalog1

                    let spaces =
                        Keyspace.table c.Name
                        :: (implicitIndexes |> List.map (fun idx -> Keyspace.index idx.IndexName))

                    let! cleaned = spaces |> foldOk clearSpaceChunksResult state
                    return! saved cleaned catalog2
                }

        createTableCore c |> exclusiveDdl state

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
                        match Catalog.removeTable state.Catalog name |> saved state with
                        | Ok next -> ret next
                        | Error e ->
                            DropCleanup.clear state.Store.Root |> ignore
                            err e

                    match spaces |> foldOk clearSpaceChunksResult next with
                    | Ok cleaned ->
                        DropCleanup.clear state.Store.Root |> ignore
                        return cleaned
                    | Error _ -> return next
                }

        dropTableCore name ifExists |> exclusiveDdl state

    let createIndex (ci: CreateIndex) (state: DbState) =
        let rec loop previous positions (idx: IndexSchema) scan =
            match SstScan.next scan with
            | Error e -> err e
            | Ok None -> ret ()
            | Ok(Some(e, rest)) ->
                match e.Value with
                | None -> loop previous positions idx rest
                | Some row ->
                    let values = positions |> List.map (fun i -> row.[i]) |> List.toArray

                    if values |> Array.exists SqlValue.isNull then
                        loop previous positions idx rest
                    else
                        let prefix = KeyBuilder.ofValues values

                        match previous with
                        | Some before when Bytes.compare before prefix = 0 ->
                            err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                        | _ -> loop (Some prefix) positions idx rest

        let checkEntries = loop None

        let checkUniqueIndex schema (idx: IndexSchema) store =
            idx.Columns
            |> List.map (TableSchema.requireIndexOf schema)
            |> all
            >>= fun pos ->
                RowStore.scan (Keyspace.index idx.Name) [||] 0 store
                >>= fun scan -> checkEntries pos idx scan

        let clearSpaceChunks state space =
            match clearSpaceChunksResult state space with
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
                    let! cleared = clearSpaceChunksResult state space
                    let! rows = Dml.scanByPrimaryKey cleared.Store withIndex 0 None

                    let indexKeyFor row = RowKey.indexKey withIndex idx row

                    let! filled =
                        applyOpChunks
                            (fun txn row -> ret (Transaction.put space (indexKeyFor row) row txn))
                            rows
                            cleared

                    if idx.Unique then
                        match checkUniqueIndex withIndex idx filled.Store with
                        | Ok() -> return filled
                        | Error e ->
                            clearSpaceChunks filled space |> ignore
                            return! err e
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
                    return! saved filled next
                }

        match Catalog.findIndex state.Catalog ci.Name with
        | Some _ when ci.IfNotExists -> ret state
        | _ -> createIndexCore ci |> exclusiveDdl state

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
                        match Catalog.removeIndex state.Catalog name |> saved state with
                        | Ok next -> ret next
                        | Error e ->
                            DropCleanup.clear state.Store.Root |> ignore
                            err e

                    match clearSpaceChunksResult next space with
                    | Ok cleaned ->
                        DropCleanup.clear state.Store.Root |> ignore
                        return cleaned
                    | Error _ -> return next
                }

        dropIndexCore name ifExists |> exclusiveDdl state

module internal Engine =
    let private addTable (state: DbState) (ctx: ExecContext) (t: TableDefinition) =
        Catalog.toSchema t
        >>= fun schema ->
            let withIndexes =
                { schema with
                    Indexes = Catalog.indexSchemasOf state.Catalog t.Name
                }

            ret (ExecContext.withSchema t.Name withIndexes ctx)

    let private contextOf (state: DbState) (txn: Transaction) =
        let ctx =
            ExecContext.create state.Store (Snapshot.toVisible txn.Snapshot)
            |> ExecContext.withTransaction txn
            |> ExecContext.withCatalog state.Catalog

        state.Catalog.Tables |> foldOk (addTable state) ctx

    let private runSelect (state: DbState) (txn: Transaction) (stmt: SelectStatement) =
        contextOf state txn
        >>= fun ctx ->
            Plan.runSelect ctx stmt
            |> Result.map (fun (columns, rows) ->
                state,
                {
                    RowsAffected = rows.Length
                    Columns = columns
                    Rows = rows
                })

    let private resolveWhere (ctx: ExecContext) =
        function
        | None -> ret None
        | Some e -> Plan.resolveDmlExpr ctx e |> Result.map Some

    let private runInsert (stmt: InsertStatement) (ctx: ExecContext) =
        result {
            let! rows =
                stmt.Rows
                |> List.map (fun row -> row |> List.map (Plan.resolveDmlExpr ctx) |> all)
                |> all

            let! schema = ExecContext.schemaOf stmt.Table.Name ctx
            return! DmlOps.insert ctx schema stmt.Columns rows
        }

    let private runUpdate (stmt: UpdateStatement) (ctx: ExecContext) =
        result {
            let! assignments =
                stmt.Assignments
                |> List.map (fun (name, e) -> Plan.resolveDmlExpr ctx e |> Result.map (fun e2 -> name, e2))
                |> all

            let! whereE = resolveWhere ctx stmt.Where
            let! schema = ExecContext.schemaOf stmt.Table.Name ctx
            return! DmlOps.update ctx schema assignments whereE
        }

    let private runDelete (stmt: DeleteStatement) (ctx: ExecContext) =
        result {
            let! whereE = resolveWhere ctx stmt.Where
            let! schema = ExecContext.schemaOf stmt.Table.Name ctx
            return! DmlOps.delete ctx schema whereE
        }

    let private runDml (state: DbState) (txn: Transaction) (run: ExecContext -> Result<(Transaction * int), DbError>) =
        result {
            let! ctx = contextOf state txn
            let! outcome = run ctx
            let txn2, n = outcome

            return
                { state with Pending = Some txn2 },
                { StatementResult.Empty with
                    RowsAffected = n
                }
        }

    let private runDdl (state: DbState) (run: DbState -> Result<'a, DbError>) =
        if state.Active.IsSome then
            err (
                DbError.create
                    ErrorKind.SyntaxError
                    "トランザクション中は DDL（CREATE / DROP）を実行できません。COMMIT または ROLLBACK してから実行してください"
            )
        else
            run state

    let private noRows = Result.map (fun s -> s, StatementResult.Empty)

    let private runStatement (state: DbState) (txn: Transaction) =
        function
        | Statement.Select stmt -> runSelect state txn stmt
        | Statement.Insert stmt -> runInsert stmt |> runDml state txn
        | Statement.Update stmt -> runUpdate stmt |> runDml state txn
        | Statement.Delete stmt -> runDelete stmt |> runDml state txn
        | Statement.CreateTable c -> Ddl.createTable c |> runDdl state |> noRows
        | Statement.DropTable(n, ifE) -> Ddl.dropTable n ifE |> runDdl state |> noRows
        | Statement.CreateIndex ci -> Ddl.createIndex ci |> runDdl state |> noRows
        | Statement.DropIndex(n, ifE) -> Ddl.dropIndex n ifE |> runDdl state |> noRows
        | Statement.Begin
        | Statement.Commit
        | Statement.Rollback -> ret state |> noRows

    let executeStatement (state: DbState) (stmt: Statement) =
        match state.Active with
        | Some activeTxn ->
            let running = state.Pending |> Option.defaultValue activeTxn
            runStatement state running stmt
        | None ->
            let mgr, txn = TransactionManager.beginTxn state.Manager
            let registration = ActiveSnapshotRegistry.register state.Root txn.Snapshot.SeqNo

            let run () =
                result {
                    let startState =
                        { state with
                            Manager = mgr
                            ActiveRegistration = Some registration
                        }

                    let! midState, result = runStatement startState txn stmt
                    let toCommit = midState.Pending |> Option.defaultValue txn
                    let! nextState = DbStateOps.commitTransaction toCommit midState
                    return nextState, result
                }

            let finish r =
                ActiveSnapshotRegistry.unregister state.Root registration
                r

            let isDdl =
                match stmt with
                | Statement.CreateTable _
                | Statement.DropTable _
                | Statement.CreateIndex _
                | Statement.DropIndex _ -> true
                | _ -> false

            (if isDdl then run () else RootGate.withShared state.Root run) |> finish

    let executeBegin (state: DbState) =
        match state.Active with
        | Some _ -> ret state
        | None ->
            RootGate.withShared state.Root (fun () ->
                let mgr, txn = TransactionManager.beginTxn state.Manager
                let registration = ActiveSnapshotRegistry.register state.Root txn.Snapshot.SeqNo

                match Catalog.load state.Store with
                | Error e ->
                    ActiveSnapshotRegistry.unregister state.Root registration
                    err e
                | Ok fresh ->
                    ret
                        { state with
                            Manager = mgr
                            Active = Some txn
                            Catalog = fresh
                            ActiveRegistration = Some registration
                        })

    let executeCommit (state: DbState) =
        match state.Active with
        | None -> ret state
        | Some activeTxn ->
            let txn = state.Pending |> Option.defaultValue activeTxn

            DbStateOps.commitTransaction txn state
            |> Result.map (fun nextState ->
                state.ActiveRegistration
                |> Option.iter (ActiveSnapshotRegistry.unregister state.Root)

                nextState)

    let executeRollback (state: DbState) =
        match state.Active with
        | None -> state
        | Some txn ->
            let mgr = TransactionManager.abort txn state.Manager

            state.ActiveRegistration
            |> Option.iter (ActiveSnapshotRegistry.unregister state.Root)

            { state with
                Manager = mgr
                Active = None
                Pending = None
                ActiveRegistration = None
            }
