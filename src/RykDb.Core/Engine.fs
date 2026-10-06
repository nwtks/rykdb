// ===========================================================================
// Engine — 文の実行層（Ddl / Engine）。
// ===========================================================================

namespace RykDb.Core

open System

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
            let header = String.Join(" | ", this.Columns)

            let body =
                this.Rows
                |> Array.map (fun r ->
                    r
                    |> Array.map SqlLiteral.toSqlLiteral
                    |> String.concat ", "
                    |> fun s -> "  " + s)

            header :: String('-', header.Length) :: Array.toList body

/// DB の状態。不変。
type internal DbState =
    {
        Root: string
        Store: RowStore
        Catalog: Catalog
        Manager: TransactionManager
        Active: Transaction option
        Pending: Transaction option
    }

module internal Ddl =
    let createTable (state: DbState) (c: CreateTable) =
        let existing = Catalog.findTable state.Catalog c.Name

        match existing, c.IfNotExists with
        | Some _, true -> ret state
        | Some _, false -> Error(DbError.alreadyExists (sprintf "テーブル %s は既に存在します" c.Name))
        | None, _ ->
            TableSchema.ofCreateTable c
            >>= fun schema ->
                let defn = Catalog.toDefinition schema
                let next = Catalog.addTable state.Catalog defn

                Catalog.save state.Store next
                |> Result.map (fun store ->
                    { state with
                        Store = store
                        Catalog = next
                    })

    let private backfillIndex (state: DbState) (tableName: string) (idx: IndexSchema) =
        match Catalog.findTable state.Catalog tableName with
        | None -> ret state
        | Some defn ->
            Catalog.toSchema defn
            |> Result.bind (fun schema ->
                let withIndex =
                    { schema with
                        Indexes = idx :: schema.Indexes
                    }

                let mgr, txn = TransactionManager.beginTxn state.Manager

                Dml.scanByPrimaryKey state.Store withIndex Snapshot.noLimit 0
                |> Result.bind (fun rows ->
                    rows
                    |> Array.fold
                        (fun acc row ->
                            match acc with
                            | Error e -> Error e
                            | Ok t ->
                                let key = RowKey.indexKey withIndex idx row
                                Ok(Transaction.put t (Keyspace.index idx.Name) key row))
                        (ret txn))
                |> Result.bind (fun txn2 ->
                    TransactionManager.commit mgr txn2
                    |> Result.bind (fun (mgr2, _, batches) ->
                        RowStore.writeBatches state.Store batches
                        |> Result.map (fun store -> store, mgr2))
                    |> Result.map (fun (store, mgr2) ->
                        { state with
                            Store = store
                            Manager = mgr2
                        })))

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
                    Error(DbError.notFound (sprintf "テーブル %s に列 %s がありません" ci.Table (String.Join(", ", missing))))
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

                    Catalog.save state.Store next
                    |> Result.bind (fun store ->
                        let withCatalog =
                            { state with
                                Store = store
                                Catalog = next
                            }

                        backfillIndex withCatalog ci.Table idxSchema)

    let dropTable (state: DbState) (name: string) (ifExists: bool) =
        match Catalog.findTable state.Catalog name with
        | None ->
            if ifExists then
                ret state
            else
                Error(DbError.notFound (sprintf "テーブル %s が存在しません" name))
        | Some _ ->
            let next = Catalog.removeTable state.Catalog name

            Catalog.save state.Store next
            |> Result.map (fun store ->
                { state with
                    Store = store
                    Catalog = next
                })

    let dropIndex (state: DbState) (name: string) (ifExists: bool) =
        match Catalog.findIndex state.Catalog name with
        | None ->
            if ifExists then
                ret state
            else
                Error(DbError.notFound (sprintf "索引 %s が存在しません" name))
        | Some _ ->
            let next = Catalog.removeIndex state.Catalog name

            Catalog.save state.Store next
            |> Result.map (fun store ->
                { state with
                    Store = store
                    Catalog = next
                })

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
        >>= fun schema -> DmlOps.insert ctx schema txn st.Columns st.Rows

    let private runUpdate (ctx: ExecContext) (st: UpdateStatement) (txn: Transaction) =
        ctx.SchemaOf st.Table.Name
        >>= fun schema -> DmlOps.update ctx schema txn st.Assignments st.Where

    let private runDelete (ctx: ExecContext) (st: DeleteStatement) (txn: Transaction) =
        ctx.SchemaOf st.Table.Name
        >>= fun schema -> DmlOps.delete ctx schema txn st.Where

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
        | Statement.Insert st ->
            let ctx = contextOf state txn

            runInsert ctx st txn
            |> Result.map (fun (txn2, n) ->
                let nextState = { state with Pending = Some txn2 }

                let result =
                    {
                        RowsAffected = n
                        Columns = [||]
                        Rows = [||]
                    }

                nextState, result)
        | Statement.Update st ->
            let ctx = contextOf state txn

            runUpdate ctx st txn
            |> Result.map (fun (txn2, n) ->
                let nextState = { state with Pending = Some txn2 }

                let result =
                    {
                        RowsAffected = n
                        Columns = [||]
                        Rows = [||]
                    }

                nextState, result)
        | Statement.Delete st ->
            let ctx = contextOf state txn

            runDelete ctx st txn
            |> Result.map (fun (txn2, n) ->
                let nextState = { state with Pending = Some txn2 }

                let result =
                    {
                        RowsAffected = n
                        Columns = [||]
                        Rows = [||]
                    }

                nextState, result)
        | Statement.CreateTable c -> Ddl.createTable state c |> Result.map (fun s -> s, StatementResult.empty)
        | Statement.DropTable(n, ifE) -> Ddl.dropTable state n ifE |> Result.map (fun s -> s, StatementResult.empty)
        | Statement.CreateIndex ci -> Ddl.createIndex state ci |> Result.map (fun s -> s, StatementResult.empty)
        | Statement.DropIndex(n, ifE) -> Ddl.dropIndex state n ifE |> Result.map (fun s -> s, StatementResult.empty)
        | Statement.Begin -> ret (state, StatementResult.empty)
        | Statement.Commit -> ret (state, StatementResult.empty)
        | Statement.Rollback -> ret (state, StatementResult.empty)
