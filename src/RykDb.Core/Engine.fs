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
        result {
            let! manager, _, batches = TransactionManager.commit mgr txn
            let snapshot = TransactionManager.activeSnapshot manager
            let! next = RowStore.writeBatchesWithSnapshot snapshot store batches
            return next, manager
        }

    let commitTransaction (state: DbState) (txn: Transaction) =
        result {
            let! store, manager = commitToStore state.Store state.Manager txn

            return
                { state with
                    Store = store
                    Manager = manager
                    Active = None
                    Pending = None
                }
        }

// 書き込みは Transaction にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
module internal DmlOps =
    /// WHERE を評価するときの対象スキーマ（UPDATE / DELETE で共通）。
    let private targetSchemaOf (schema: TableSchema) = Schema.ofTable schema.Name None schema

    /// WHERE の述語。
    /// WHERE が無ければ「全部通過」（UPDATE / DELETE で共通）。
    let private matchesWhere (targetSchema: Schema) (where: Expr option) (row: SqlValue array) =
        match where with
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
        let rec gather (vs: Expr list) (is_: int list) (acc: (int * SqlValue) list) =
            match vs, is_ with
            | [], [] -> ret acc
            | v :: rest, i :: restIdx ->
                match evalOne v with
                | Error e -> Error e
                | Ok value ->
                    let colDef = schema.Columns.[i]

                    match SqlValue.coerce colDef.DataType value with
                    | Error e -> Error e
                    | Ok coerced -> gather rest restIdx ((i, coerced) :: acc)
            | _ -> ret acc

        gather values idxs []

    /// 表を全走査して 1行ずつ処理する UPDATE / DELETE 共通の土台。
    /// matched が偽の行は件数を変えず先へ進み、 onRow が「(トランザクション, 件数)」を進めた結果を返す。
    let private foldRows
        (txn: Transaction)
        (ctx: ExecContext)
        (schema: TableSchema)
        (matched: SqlValue array -> Result<bool, DbError>)
        (onRow: Transaction -> int -> SqlValue array -> Result<Transaction * int, DbError>)
        =
        Dml.scanByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn 0
        >>= foldOk (fun (t, c) row -> matched row >>= fun keep -> if keep then onRow t c row else ret (t, c)) (txn, 0)

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
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (columns: string list option)
        (rows: Expr list list)
        =
        let evalConstant (e: Expr) = Eval.eval Schema.empty [||] e

        let indexValues (index: IndexSchema) (row: SqlValue array) =
            index.Columns
            |> List.map (TableSchema.requireIndexOf schema)
            |> all
            |> Result.map (fun positions -> positions |> List.map (fun i -> row.[i]) |> List.toArray)

        let rec check (row: SqlValue array) (pk: SqlValue array) (indexes: IndexSchema list) =
            match indexes with
            | [] -> ret ()
            | idx :: rest ->
                match indexValues idx row with
                | Error e -> Error e
                | Ok values ->
                    match Dml.scanByIndexPrefixWithTxn ctx.Store schema idx ctx.Visible txn values 1 with
                    | Error e -> Error e
                    | Ok rows ->
                        if
                            rows
                            |> Seq.exists (fun r ->
                                let existingPk = TableSchema.primaryKeyOf schema r
                                not (Array.forall2 (=) existingPk pk))
                        then
                            err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                        else
                            check row pk rest

        let checkUnique (txn: Transaction) (row: SqlValue array) =
            let pk = TableSchema.primaryKeyOf schema row

            match Dml.getByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn pk with
            | Error e -> Error e
            | Ok(Some _) -> err (DbError.constraintViolation (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name))
            | Ok None -> check row pk (schema.Indexes |> List.filter _.Unique)

        let buildRow (defaults: SqlValue array) idxs width (values: Expr list) =
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
                | Some names -> names |> List.map (TableSchema.requireIndexOf schema) |> all

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
                            let! row = buildRow defaults idxs schema.Columns.Length values
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
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (assignments: (string * Expr) list)
        (where: Expr option)
        =
        let assign (targetSchema: Schema) (targets: int list) (row: SqlValue array) =
            gatherWith (Eval.eval targetSchema row) schema (List.map snd assignments) targets
            |> Result.map (fun filled -> scatter row.Length filled (fun i -> row.[i]))

        let rec checkUniqueUpdate
            (txnAcc: Transaction)
            (newRow: SqlValue array)
            (newPk: SqlValue array)
            (indexes: IndexSchema list)
            =
            match indexes with
            | [] -> ret ()
            | idx :: rest ->
                match idx.Columns |> List.map (TableSchema.requireIndexOf schema) |> all with
                | Error e -> Error e
                | Ok positions ->
                    let values = positions |> List.map (fun i -> newRow.[i]) |> List.toArray

                    match Dml.scanByIndexPrefixWithTxn ctx.Store schema idx ctx.Visible txnAcc values 1 with
                    | Error e -> Error e
                    | Ok rows ->
                        if
                            rows
                            |> Seq.exists (fun r ->
                                let existingPk = TableSchema.primaryKeyOf schema r
                                not (Array.forall2 (=) existingPk newPk))
                        then
                            err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                        else
                            checkUniqueUpdate txnAcc newRow newPk rest

        let targetSchema = targetSchemaOf schema

        assignments
        |> List.map (fun (name, _) -> TableSchema.requireIndexOf schema name)
        |> all
        >>= fun targets ->
            foldRows txn ctx schema (matchesWhere targetSchema where) (fun txnAcc count row ->
                result {
                    let! newRow = assign targetSchema targets row
                    let newPk = TableSchema.primaryKeyOf schema newRow
                    do! checkUniqueUpdate txnAcc newRow newPk (schema.Indexes |> List.filter _.Unique)
                    let! t = Dml.updateRow schema txnAcc row newRow
                    return t, count + 1
                })

    /// DELETE。
    /// WHERE に合う行を主キー + 全索引に tombstone で消す。
    let delete (ctx: ExecContext) (schema: TableSchema) (txn: Transaction) (where: Expr option) =
        let targetSchema = targetSchemaOf schema

        foldRows txn ctx schema (matchesWhere targetSchema where) (fun t count row ->
            Dml.deleteRow schema t row |> Result.map (fun t2 -> t2, count + 1))

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

    let createTable (state: DbState) (c: CreateTable) =
        let existing = Catalog.findTable state.Catalog c.Name

        match existing, c.IfNotExists with
        | Some _, true -> ret state
        | Some _, false -> err (sprintf "テーブル %s は既に存在します" c.Name |> DbError.alreadyExists)
        | None, _ ->
            result {
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
                            IndexName = sprintf "__unique_%s_%d" c.Name i
                            TableName = c.Name
                            Columns = cols
                            Unique = true
                        })

                let catalog1 = Catalog.addTable state.Catalog defn

                let catalog2 =
                    implicitIndexes |> List.fold (fun cat idx -> Catalog.addIndex cat idx) catalog1

                return! saved state catalog2
            }

    let createIndex (state: DbState) (ci: CreateIndex) =
        let duplicateUniqueValues (schema: TableSchema) (index: IndexSchema) (rows: SqlValue array seq) =
            let indexColumns =
                index.Columns |> List.map (TableSchema.requireIndexOf schema) |> all

            match indexColumns with
            | Error e -> Error e
            | Ok positions ->
                rows
                |> Seq.fold
                    (fun result row ->
                        result
                        >>= fun seen ->
                            let key =
                                positions |> List.map (fun position -> row.[position]) |> KeyBuilder.ofValues

                            if Set.contains key seen then
                                err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" index.Name))
                            else
                                ret (Set.add key seen))
                    (ret Set.empty)

        let backfillIndex (state: DbState) (tableName: string) (idx: IndexSchema) =
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
                    let mgr, txn = TransactionManager.beginTxn state.Manager
                    let! rows = Dml.scanByPrimaryKey state.Store withIndex Snapshot.noLimit 0
                    let indexKeyFor row = RowKey.indexKey withIndex idx row

                    if idx.Unique then
                        let! _ = duplicateUniqueValues withIndex idx rows
                        ()

                    let! txn2 =
                        foldOk (fun t row -> ret (Transaction.put t space (indexKeyFor row) row)) txn rows

                    let! store, mgr2 = DbStateOps.commitToStore state.Store mgr txn2

                    return
                        { state with
                            Store = store
                            Manager = mgr2
                        }
                }

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
                    backfillIndex state ci.Table idxSchema >>= fun filled -> saved filled next

    /// DROP TABLE。
    /// 手順:
    ///   1. 残りの全行を「主キー + 全索引」に tombstone で消す（DML.DELETE と同じ経路）
    ///   2. commit してからカタログから定義を外す
    /// カタログを先に外して keyspace を残すと、同名で再 CREATE したときに旧データがそのまま見える
    /// （keyspace は名前由来のディレクトリで再利用される）。
    let dropTable (state: DbState) (name: string) (ifExists: bool) =
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

                let mgr, txn = TransactionManager.beginTxn state.Manager
                let! rows = Dml.scanByPrimaryKey state.Store withIndexes Snapshot.noLimit 0
                let! txn2 = foldOk (fun t row -> Dml.deleteRow withIndexes t row) txn rows
                let! store, mgr2 = DbStateOps.commitToStore state.Store mgr txn2

                return!
                    saved
                        { state with
                            Store = store
                            Manager = mgr2
                        }
                        (Catalog.removeTable state.Catalog name)
            }

    /// DROP INDEX。
    /// 索引 keyspace の全エントリへ tombstone を書いてからカタログから定義を外す。
    /// 書かないと、消した行の索引エントリが keyspace に残り、
    /// 同名で CREATE INDEX し直したときに tombstone のない旧エントリが見えてしまう。
    let dropIndex (state: DbState) (name: string) (ifExists: bool) =
        match Catalog.findIndex state.Catalog name with
        | None ->
            if ifExists then
                ret state
            else
                err (sprintf "索引 %s が存在しません" name |> DbError.notFound)
        | Some defn ->
            result {
                let space = Keyspace.index defn.IndexName
                let mgr, txn = TransactionManager.beginTxn state.Manager
                let! entries = RowStore.scanAt state.Store space [||] Snapshot.noLimit 0

                let! txn2 =
                    foldOk (fun t (key, _) -> ret (Transaction.remove t space key)) txn entries

                let! store, mgr2 = DbStateOps.commitToStore state.Store mgr txn2

                return!
                    saved
                        { state with
                            Store = store
                            Manager = mgr2
                        }
                        (Catalog.removeIndex state.Catalog name)
            }

module internal Engine =
    let runStatement (state: DbState) (txn: Transaction) (stmt: Statement) =
        let addTable ctx t =
            match Catalog.toSchema t with
            | Ok schema ->
                let withIndexes =
                    { schema with
                        Indexes = Catalog.indexSchemasOf state.Catalog t.Name
                    }

                ret (ExecContext.withSchema ctx t.Name withIndexes)
            | Error e -> Error e

        let contextOf (state: DbState) (txn: Transaction) =
            let ctx = ExecContext.create state.Store (Snapshot.toVisible txn.Snapshot)
            let ctx = ExecContext.withTransaction ctx txn
            let ctx = ExecContext.withCatalog ctx state.Catalog

            state.Catalog.Tables |> foldOk addTable ctx

        let runInsert (ctx: ExecContext) (st: InsertStatement) (txn: Transaction) =
            ExecContext.schemaOf ctx st.Table.Name
            >>= fun schema -> DmlOps.insert ctx schema txn st.Columns st.Rows

        let runUpdate (ctx: ExecContext) (st: UpdateStatement) (txn: Transaction) =
            ExecContext.schemaOf ctx st.Table.Name
            >>= fun schema -> DmlOps.update ctx schema txn st.Assignments st.Where

        let runDelete (ctx: ExecContext) (st: DeleteStatement) (txn: Transaction) =
            ExecContext.schemaOf ctx st.Table.Name
            >>= fun schema -> DmlOps.delete ctx schema txn st.Where

        let runDml
            (state: DbState)
            (txn: Transaction)
            (run: ExecContext -> Transaction -> Result<Transaction * int, DbError>)
            =
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

        let noRows (r: Result<DbState, DbError>) =
            r |> Result.map (fun s -> s, StatementResult.empty)

        let ddl (run: unit -> Result<DbState, DbError>) =
            if state.Active.IsSome then
                err (
                    DbError.create
                        ErrorKind.SyntaxError
                        "トランザクション中は DDL（CREATE / DROP）を実行できません。COMMIT または ROLLBACK してから実行してください"
                )
            else
                run ()

        match stmt with
        | Statement.Select sel ->
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
        | Statement.Insert st -> runDml state txn (fun ctx -> runInsert ctx st)
        | Statement.Update st -> runDml state txn (fun ctx -> runUpdate ctx st)
        | Statement.Delete st -> runDml state txn (fun ctx -> runDelete ctx st)
        | Statement.CreateTable c -> noRows (ddl (fun () -> Ddl.createTable state c))
        | Statement.DropTable(n, ifE) -> noRows (ddl (fun () -> Ddl.dropTable state n ifE))
        | Statement.CreateIndex ci -> noRows (ddl (fun () -> Ddl.createIndex state ci))
        | Statement.DropIndex(n, ifE) -> noRows (ddl (fun () -> Ddl.dropIndex state n ifE))
        | Statement.Begin
        | Statement.Commit
        | Statement.Rollback -> ret (state, StatementResult.empty)
