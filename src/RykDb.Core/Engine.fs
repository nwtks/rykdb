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
                |> Array.map (fun r ->
                    r
                    |> Array.map SqlLiteral.toSqlLiteral
                    |> String.concat ", "
                    |> fun s -> "  " + s)

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
    }

/// Transaction を commit して DbState に反映する共通処理。
module internal DbStateOps =
    /// commit してバッチを Store に書き込む。
    /// 戻り値は (書き込み後の Store, commit 後の Manager)。
    /// Active / Pending のような state フィールドの更新は呼び出し側で行う
    /// （backfillIndex はトランザクションを外さないため触らない）。
    let commitToStore (store: RowStore) (mgr: TransactionManager) (txn: Transaction) =
        TransactionManager.commit mgr txn
        >>= fun (manager, _, batches) -> RowStore.writeBatches store batches |> Result.map (fun next -> next, manager)

    let commitTransaction (state: DbState) (txn: Transaction) =
        commitToStore state.Store state.Manager txn
        |> Result.map (fun (store, manager) ->
            { state with
                Store = store
                Manager = manager
                Active = None
                Pending = None
            })

module internal Ddl =
    /// カタログを保存し、保存後の Store と新しい Catalog で state を作り直す。
    /// （CREATE / DROP すべてが使う「保存 → state 更新」の定型）
    let private saved (state: DbState) (next: Catalog) =
        Catalog.save state.Store next
        |> Result.map (fun store ->
            { state with
                Store = store
                Catalog = next
            })

    let createTable (state: DbState) (c: CreateTable) =
        let existing = Catalog.findTable state.Catalog c.Name

        match existing, c.IfNotExists with
        | Some _, true -> ret state
        | Some _, false -> err (sprintf "テーブル %s は既に存在します" c.Name |> DbError.alreadyExists)
        | None, _ ->
            TableSchema.ofCreateTable c
            >>= fun schema ->
                let defn = Catalog.toDefinition schema
                saved state (Catalog.addTable state.Catalog defn)

    let private backfillIndex (state: DbState) (tableName: string) (idx: IndexSchema) =
        match Catalog.findTable state.Catalog tableName with
        | None -> ret state
        | Some defn ->
            result {
                let! schema = Catalog.toSchema defn

                let withIndex =
                    { schema with
                        Indexes = idx :: schema.Indexes
                    }

                let mgr, txn = TransactionManager.beginTxn state.Manager
                let! rows = Dml.scanByPrimaryKey state.Store withIndex Snapshot.noLimit 0

                let! txn2 =
                    foldOk
                        (fun t row ->
                            let key = RowKey.indexKey withIndex idx row
                            ret (Transaction.put t (Keyspace.index idx.Name) key row))
                        txn
                        rows

                let! store, mgr2 = DbStateOps.commitToStore state.Store mgr txn2

                return
                    { state with
                        Store = store
                        Manager = mgr2
                    }
            }

    let createIndex (state: DbState) (ci: CreateIndex) =
        match Catalog.findTable state.Catalog ci.Table with
        | None -> err (sprintf "テーブル %s が存在しません" ci.Table |> DbError.notFound)
        | Some tableDef ->
            match Catalog.findIndex state.Catalog ci.Name with
            | Some _ -> err (sprintf "索引 %s は既に存在します" ci.Name |> DbError.alreadyExists)
            | None ->
                let missing =
                    ci.Columns
                    |> List.filter (fun n -> tableDef.Columns |> List.exists (fun c -> Catalog.eqName n c.Name) |> not)

                if not (List.isEmpty missing) then
                    err (
                        sprintf "テーブル %s に列 %s がありません" ci.Table (System.String.Join(", ", missing))
                        |> DbError.notFound
                    )
                else
                    let defn =
                        {
                            IndexName = ci.Name
                            TableName = ci.Table
                            Columns = ci.Columns
                            Unique = ci.Unique
                        }

                    let idxSchema = Catalog.toIndexSchema defn
                    let next = Catalog.addIndex state.Catalog defn

                    saved state next
                    >>= fun withCatalog -> backfillIndex withCatalog ci.Table idxSchema

    let private dropItem
        (state: DbState)
        (kind: string)
        (name: string)
        (ifExists: bool)
        (find: Catalog -> string -> 'a option)
        (remove: Catalog -> string -> Catalog)
        =
        match find state.Catalog name with
        | Some _ -> saved state (remove state.Catalog name)
        | None ->
            if ifExists then
                ret state
            else
                err (sprintf "%s %s が存在しません" kind name |> DbError.notFound)

    let dropTable (state: DbState) (name: string) (ifExists: bool) =
        dropItem state "テーブル" name ifExists Catalog.findTable Catalog.removeTable

    let dropIndex (state: DbState) (name: string) (ifExists: bool) =
        dropItem state "索引" name ifExists Catalog.findIndex Catalog.removeIndex

module internal Engine =
    let private contextOf (state: DbState) (txn: Transaction) : ExecContext =
        let base0 = Snapshot.toVisible txn.Snapshot |> ExecContext.create state.Store

        state.Catalog.Tables
        |> List.fold
            (fun acc t ->
                match Catalog.toSchema t with
                | Ok schema ->
                    let withIndexes =
                        { schema with
                            Indexes = Catalog.indexSchemasOf state.Catalog t.Name
                        }

                    let a1 = acc.WithSchema t.Name withIndexes
                    a1.WithCatalog state.Catalog
                | Error _ -> acc.WithCatalog state.Catalog)
            (base0.WithCatalog state.Catalog)

    let runStatement (state: DbState) (txn: Transaction) (stmt: Statement) =
        let runInsert (ctx: ExecContext) (st: InsertStatement) (txn: Transaction) =
            ctx.SchemaOf st.Table.Name
            >>= fun schema -> DmlOps.insert schema txn st.Columns st.Rows

        let runUpdate (ctx: ExecContext) (st: UpdateStatement) (txn: Transaction) =
            ctx.SchemaOf st.Table.Name
            >>= fun schema -> DmlOps.update ctx schema txn st.Assignments st.Where

        let runDelete (ctx: ExecContext) (st: DeleteStatement) (txn: Transaction) =
            ctx.SchemaOf st.Table.Name
            >>= fun schema -> DmlOps.delete ctx schema txn st.Where

        let runDml
            (state: DbState)
            (txn: Transaction)
            (run: ExecContext -> Transaction -> Result<Transaction * int, DbError>)
            =
            let ctx = contextOf state txn

            run ctx txn
            |> Result.map (fun (txn2, n) ->
                let nextState = { state with Pending = Some txn2 }

                nextState,
                { StatementResult.empty with
                    RowsAffected = n
                })

        let noRows (r: Result<DbState, DbError>) =
            r |> Result.map (fun s -> s, StatementResult.empty)

        match stmt with
        | Statement.Select sel ->
            let ctx = contextOf state txn

            Plan.runSelect ctx sel
            |> Result.map (fun (columns, rows) ->
                let result =
                    {
                        RowsAffected = rows.Length
                        Columns = columns
                        Rows = rows
                    }

                state, result)
        | Statement.Insert st -> runDml state txn (fun ctx -> runInsert ctx st)
        | Statement.Update st -> runDml state txn (fun ctx -> runUpdate ctx st)
        | Statement.Delete st -> runDml state txn (fun ctx -> runDelete ctx st)
        | Statement.CreateTable c -> noRows (Ddl.createTable state c)
        | Statement.DropTable(n, ifE) -> noRows (Ddl.dropTable state n ifE)
        | Statement.CreateIndex ci -> noRows (Ddl.createIndex state ci)
        | Statement.DropIndex(n, ifE) -> noRows (Ddl.dropIndex state n ifE)
        | Statement.Begin
        | Statement.Commit
        | Statement.Rollback -> ret (state, StatementResult.empty)
