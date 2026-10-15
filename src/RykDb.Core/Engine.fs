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
        | Some _, false -> Error(DbError.alreadyExists (sprintf "テーブル %s は既に存在します" c.Name))
        | None, _ ->
            TableSchema.ofCreateTable c
            >>= fun schema ->
                let defn = Catalog.toDefinition schema
                saved state (Catalog.addTable state.Catalog defn)

    let private backfillIndex (state: DbState) (tableName: string) (idx: IndexSchema) =
        match Catalog.findTable state.Catalog tableName with
        | None -> ret state
        | Some defn ->
            Catalog.toSchema defn
            >>= fun schema ->
                let withIndex =
                    { schema with
                        Indexes = idx :: schema.Indexes
                    }

                let mgr, txn = TransactionManager.beginTxn state.Manager

                Dml.scanByPrimaryKey state.Store withIndex Snapshot.noLimit 0
                >>= foldOk
                        (fun t row ->
                            let key = RowKey.indexKey withIndex idx row
                            ret (Transaction.put t (Keyspace.index idx.Name) key row))
                        txn
                >>= fun txn2 ->
                    DbStateOps.commitToStore state.Store mgr txn2
                    |> Result.map (fun (store, mgr2) ->
                        { state with
                            Store = store
                            Manager = mgr2
                        })

    let createIndex (state: DbState) (ci: CreateIndex) =
        match Catalog.findTable state.Catalog ci.Table with
        | None -> Error(DbError.notFound (sprintf "テーブル %s が存在しません" ci.Table))
        | Some tableDef ->
            match Catalog.findIndex state.Catalog ci.Name with
            | Some _ -> Error(DbError.alreadyExists (sprintf "索引 %s は既に存在します" ci.Name))
            | None ->
                let missing =
                    ci.Columns
                    |> List.filter (fun n -> tableDef.Columns |> List.exists (fun c -> Catalog.eqName n c.Name) |> not)

                if not (List.isEmpty missing) then
                    Error(
                        DbError.notFound (sprintf "テーブル %s に列 %s がありません" ci.Table (System.String.Join(", ", missing)))
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

    /// DROP まわりの共通処理。
    /// 見つからなければ ifExists 次第、見つかったら削除してからカタログを保存する。
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
                Error(DbError.notFound (sprintf "%s %s が存在しません" kind name))

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

    let private runInsert (ctx: ExecContext) (st: InsertStatement) (txn: Transaction) =
        ctx.SchemaOf st.Table.Name
        >>= fun schema -> DmlOps.insert schema txn st.Columns st.Rows

    let private runUpdate (ctx: ExecContext) (st: UpdateStatement) (txn: Transaction) =
        ctx.SchemaOf st.Table.Name
        >>= fun schema -> DmlOps.update ctx schema txn st.Assignments st.Where

    let private runDelete (ctx: ExecContext) (st: DeleteStatement) (txn: Transaction) =
        ctx.SchemaOf st.Table.Name
        >>= fun schema -> DmlOps.delete ctx schema txn st.Where

    /// DML（INSERT / UPDATE / DELETE）の共通処理。
    /// 文脈を作って 1つ実行し、結果の transaction を Pending に入れた新しい state と RowsAffected を返す。
    let private runDml
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

    /// 「state だけ変化し、表示結果は空」の文（DDL / トランザクション制御）の共通形。
    let private noRows (r: Result<DbState, DbError>) =
        Result.map (fun s -> s, StatementResult.empty) r

    let runStatement (state: DbState) (txn: Transaction) (stmt: Statement) =
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
