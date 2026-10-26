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
        /// 進行中 transaction の ActiveSnapshot 登録。
        /// 他のハンドルの compaction が、この snapshot より新しい版だけを残して
        /// 古い版を捨ててしまわないようにするために登録する。
        /// 同じ transaction の state は複数のハンドルが共有できる（登録側で生存を数える）。
        ActiveRegistration: ActiveRegistration option
    }

/// Transaction を commit して DbState に反映する共通処理。
module internal DbStateOps =
    /// commit してバッチを Store に書き込む。
    /// 戻り値は (書き込み後の Store, commit 後の Manager)。
    /// Active / Pending のような state フィールドの更新は呼び出し側で行う
    /// （backfillIndex はトランザクションを外さないため触らない）。
    /// GC 境界は「自ハンドルの active snapshot」と「同じ DB を開いている他ハンドルの active snapshot」の最小値。
    /// 後者は `ActiveSnapshotRegistry` から集約する（`registration` は自分自身の登録で、除外する）。
    /// 書き込みは `RootGate` の共有ロックを取り、DDL の排他と交差しないようにする。
    let commitToStore
        (root: string)
        (registration: ActiveRegistration option)
        (store: RowStore)
        (mgr: TransactionManager)
        (txn: Transaction)
        =
        RootGate.withShared root (fun () ->
            result {
                let! manager, _, batches = TransactionManager.commit mgr txn

                let snapshot =
                    let own = TransactionManager.activeSnapshot manager

                    match ActiveSnapshotRegistry.oldestExcept registration root with
                    | Some other -> min own other
                    | None -> own

                let! next = RowStore.writeBatchesWithSnapshot snapshot store batches
                return next, manager
            })

    let commitTransaction (state: DbState) (txn: Transaction) =
        result {
            let! store, manager =
                commitToStore state.Root state.ActiveRegistration state.Store state.Manager txn

            return
                { state with
                    Store = store
                    Manager = manager
                    Active = None
                    Pending = None
                    ActiveRegistration = None
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
        let rec go (cur: Op) (t: Transaction) (c: int) =
            match Op.next cur with
            | Error e -> Error e
            | Ok None -> ret (t, c)
            | Ok(Some(row, nxt)) ->
                match matched row with
                | Error e -> Error e
                | Ok false -> go nxt t c
                | Ok true ->
                    match onRow t c row with
                    | Error e -> Error e
                    | Ok(tc, c') -> go nxt tc c'

        result {
            let! scan = Dml.scanByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn 0
            return! go scan txn 0
        }

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

        let rec check (txn: Transaction) (row: SqlValue array) (pk: SqlValue array) (indexes: IndexSchema list) =
            match indexes with
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

        let checkUnique (txn: Transaction) (row: SqlValue array) =
            let pk = TableSchema.primaryKeyOf schema row

            match Dml.getByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn pk with
            | Error e -> Error e
            | Ok(Some _) -> err (DbError.constraintViolation (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name))
            | Ok None -> check txn row pk (schema.Indexes |> List.filter _.Unique)

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

        let targetSchema = targetSchemaOf schema

        let collectUpdates (txn: Transaction) (targets: int list) =
            let rec go (cur: Op) acc =
                match Op.next cur with
                | Error e -> Error e
                | Ok None -> Ok(List.rev acc)
                | Ok(Some(row, next)) ->
                    match matchesWhere targetSchema where row with
                    | Error e -> Error e
                    | Ok false -> go next acc
                    | Ok true ->
                        match assign targetSchema targets row with
                        | Error e -> Error e
                        | Ok newRow -> go next ((row, newRow) :: acc)

            result {
                let! scan = Dml.scanByPrimaryKeyWithTxn ctx.Store schema ctx.Visible txn 0
                return! go scan []
            }

        let validateUniqueUpdates (txn: Transaction) (updates: (SqlValue array * SqlValue array) list) =
            let updatedPks =
                updates
                |> List.map (fun (_, newRow) -> TableSchema.primaryKeyOf schema newRow)
                |> Set.ofList

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
                                        result {
                                            let! scan =
                                                Dml.scanByIndexPrefixWithTxn
                                                    ctx.Store
                                                    schema
                                                    idx
                                                    ctx.Visible
                                                    txn
                                                    values
                                                    0

                                            let! rows = Op.toArray scan

                                            if
                                                rows
                                                |> Array.exists (fun row ->
                                                    let existingPk = TableSchema.primaryKeyOf schema row
                                                    not (Set.contains existingPk updatedPks))
                                            then
                                                return!
                                                    err (
                                                        DbError.constraintViolation (
                                                            sprintf "UNIQUE 制約に違反: %s" idx.Name
                                                        )
                                                    )
                                            else
                                                return Set.add key seen
                                        })
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
            match ActiveSnapshotRegistry.oldestExcept state.ActiveRegistration state.Root with
            | Some _ -> err (DbError.conflict "実行中の transaction があります。COMMIT または ROLLBACK してから DDL を実行してください")
            | None -> run state)

    let createTable (state: DbState) (c: CreateTable) =
        let createTableCore (state: DbState) (c: CreateTable) =
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

                    return! saved state catalog2
                }

        exclusiveDdl state (fun fresh -> createTableCore fresh c)

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
                            let values = positions |> List.map (fun position -> row.[position])

                            if values |> List.exists SqlValue.isNull then
                                ret seen
                            else
                                let key = KeyBuilder.ofValues values

                                if Set.contains key seen then
                                    err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" index.Name))
                                else
                                    ret (Set.add key seen))
                    (ret Set.empty)

        let BackfillChunkRows = 8192

        let backfillIndex (state: DbState) (tableName: string) (idx: IndexSchema) =
            match Catalog.findTable state.Catalog tableName with
            | None -> ret state
            | Some defn ->
                let applyChunks
                    (current: DbState)
                    (chunks: seq<'a>)
                    (apply: Transaction -> 'a -> Result<Transaction, DbError>)
                    =
                    chunks
                    |> foldOk
                        (fun st chunk ->
                            let mgr, txn = TransactionManager.beginTxnWithLimit System.Int64.MaxValue st.Manager

                            result {
                                let! txn2 = apply txn chunk

                                let! store, mgr2 =
                                    DbStateOps.commitToStore st.Root st.ActiveRegistration st.Store mgr txn2

                                return
                                    { st with
                                        Store = store
                                        Manager = mgr2
                                    }
                            })
                        current

                result {
                    let! schema = Catalog.toSchema defn

                    let withIndex =
                        { schema with
                            Indexes = idx :: schema.Indexes
                        }

                    let space = Keyspace.index idx.Name
                    let indexKeyFor row = RowKey.indexKey withIndex idx row

                    let! cleared =
                        RowStore.scanAt state.Store space [||] Snapshot.noLimit 0
                        |> Result.bind (fun stale ->
                            applyChunks state (Seq.chunkBySize BackfillChunkRows stale) (fun txn chunk ->
                                foldOk (fun t (key, _) -> ret (Transaction.remove t space key)) txn chunk))

                    let! rows = Dml.scanByPrimaryKey cleared.Store withIndex Snapshot.noLimit 0

                    if idx.Unique then
                        let! checkRows = Dml.scanByPrimaryKey cleared.Store withIndex Snapshot.noLimit 0
                        let! _ = duplicateUniqueValues withIndex idx checkRows
                        ()

                    return!
                        applyChunks cleared (Seq.chunkBySize BackfillChunkRows rows) (fun txn chunk ->
                            foldOk (fun t row -> ret (Transaction.put t space (indexKeyFor row) row)) txn chunk)
                }

        let createIndexCore (state: DbState) (ci: CreateIndex) =
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
                    let! filled = backfillIndex state ci.Table idxSchema
                    return! saved filled next
                }

        exclusiveDdl state (fun fresh -> createIndexCore fresh ci)

    /// DROP TABLE。
    /// 手順:
    ///   1. テーブル keyspace と全索引 keyspace の全エントリに tombstone を書く
    ///   2. commit してからカタログから定義を外す
    /// カタログを先に外して keyspace を残すと、同名で再 CREATE したときに旧データがそのまま見える
    /// （keyspace は名前由来のディレクトリで再利用される）。
    let dropTable (state: DbState) (name: string) (ifExists: bool) =
        let dropTableCore (state: DbState) (name: string) (ifExists: bool) =
            match Catalog.findTable state.Catalog name with
            | None ->
                if ifExists then
                    ret state
                else
                    err (sprintf "テーブル %s が存在しません" name |> DbError.notFound)
            | Some defn ->
                let clearSpace (txn: Transaction) (space: string) =
                    RowStore.scanAt state.Store space [||] Snapshot.noLimit 0
                    |> Result.bind (foldOk (fun t (key, _) -> ret (Transaction.remove t space key)) txn)

                result {
                    let! schema = Catalog.toSchema defn

                    let withIndexes =
                        { schema with
                            Indexes = Catalog.indexSchemasOf state.Catalog schema.Name
                        }

                    let spaces =
                        Keyspace.table schema.Name
                        :: (withIndexes.Indexes |> List.map (fun index -> Keyspace.index index.Name))

                    let mgr, txn =
                        TransactionManager.beginTxnWithLimit System.Int64.MaxValue state.Manager

                    let! txn2 = foldOk clearSpace txn spaces

                    let! store, mgr2 =
                        DbStateOps.commitToStore state.Root state.ActiveRegistration state.Store mgr txn2

                    return!
                        saved
                            { state with
                                Store = store
                                Manager = mgr2
                            }
                            (Catalog.removeTable state.Catalog name)
                }

        exclusiveDdl state (fun fresh -> dropTableCore fresh name ifExists)

    /// DROP INDEX。
    /// 索引 keyspace の全エントリへ tombstone を書いてからカタログから定義を外す。
    /// 書かないと、消した行の索引エントリが keyspace に残り、
    /// 同名で CREATE INDEX し直したときに tombstone のない旧エントリが見えてしまう。
    let dropIndex (state: DbState) (name: string) (ifExists: bool) =
        let dropIndexCore (state: DbState) (name: string) (ifExists: bool) =
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

                    let mgr, txn =
                        TransactionManager.beginTxnWithLimit System.Int64.MaxValue state.Manager

                    let! entries = RowStore.scanAt state.Store space [||] Snapshot.noLimit 0

                    let! txn2 =
                        foldOk (fun t (key, _) -> ret (Transaction.remove t space key)) txn entries

                    let! store, mgr2 =
                        DbStateOps.commitToStore state.Root state.ActiveRegistration state.Store mgr txn2

                    return!
                        saved
                            { state with
                                Store = store
                                Manager = mgr2
                            }
                            (Catalog.removeIndex state.Catalog name)
                }

        exclusiveDdl state (fun fresh -> dropIndexCore fresh name ifExists)

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
