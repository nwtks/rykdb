// ===========================================================================
// Engine — 文の実行層（Ddl / Engine）。
// ===========================================================================

namespace RykDb.Core

/// 文の実行結果。
/// <remarks>
/// コンストラクタは非公開。構築は `Empty` / `Create` / `Affected` / `OfRows` だけ。
/// `Create` は復号済みの `LoadedRow` だけを受け取るので、
/// `SqlValue.Unloaded` を含む結果は型として作れない。
/// </remarks>
[<Sealed>]
type StatementResult private (rowsAffected: int, columns: string array, rows: SqlValue array array) =
    static member Empty = StatementResult(0, [||], [||])

    /// 復号済みの行から作る（結果行の正規の構築経路）。
    static member internal Create(rowsAffected: int, columns: string array, rows: LoadedRow array) =
        StatementResult(rowsAffected, columns, Array.map LoadedRow.toArray rows)

    /// 行を持たない結果（DML / DDL / バッチの集約）。
    static member internal Affected(rowsAffected: int) =
        StatementResult(rowsAffected, [||], [||])

    /// 生の行から作る（テスト・診断用）。復号済みかを検査する。
    static member internal OfRows(rowsAffected: int, columns: string array, rows: SqlValue array array) =
        rows
        |> Array.map LoadedRow.create
        |> all
        |> Result.map (fun loaded ->
            StatementResult(rowsAffected, columns, loaded |> List.toArray |> Array.map LoadedRow.toArray))

    /// 更新・削除などの対象件数（SELECT では行数）。
    member _.RowsAffected = rowsAffected

    /// 列名（行を持たない結果では空）。
    member _.Columns = columns

    /// 復号済みの行（`LoadedRow` で検証済み）。
    member _.Rows = rows

    member _.ToDisplayLines() =
        if Array.isEmpty columns then
            [ sprintf "(%d rows)" rowsAffected ]
        else
            let header = System.String.Join(" | ", columns)

            let body =
                rows
                |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ", " >> fun s -> "  " + s)

            header :: System.String('-', header.Length) :: Array.toList body

/// SELECT 結果の上限。
/// `Int64.MaxValue` は無制限。0 は 1行も許さない。
type internal ResultLimits = { MaxRows: int64; MaxBytes: int64 }

module internal ResultLimits =
    let Unlimited =
        {
            MaxRows = System.Int64.MaxValue
            MaxBytes = System.Int64.MaxValue
        }

/// ストリーミング SELECT の実行状態。
/// `Session.RowReader` が保持し、読み終わるか破棄されるまで snapshot 登録と走査の破棄を遅延させる。
type internal SelectStream =
    {
        Columns: string array
        Op: Op
        Scope: ScanScope
        /// `Scope` の破棄と snapshot 登録の解除。複数回呼んでも安全。
        Cleanup: unit -> unit
    }

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
        /// WAL の fsync を Session のグループコミットに任せるか。
        DeferDurability: bool
    }

/// Transaction を commit して DbState に反映する共通処理。
module internal DbStateOps =
    let commitTransaction (txn: Transaction) (state: DbState) =
        result {
            let! store, manager =
                RowStore.commitToStoreWith
                    state.DeferDurability
                    state.Root
                    state.ActiveRegistration
                    state.Manager
                    txn
                    state.Store

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
        state.Store.Runtime.WithExclusive(fun () ->
            match state.Store.Runtime.OldestExcept(state.Root, state.ActiveRegistration) with
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
                RowStore.commitToStoreWith st.DeferDurability st.Root st.ActiveRegistration mgr txn2 st.Store

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
                | OpStep.OpError e -> err e
                | OpStep.OpDone -> ret (List.rev acc, Op.Empty)
                | OpStep.OpRow(row, rest) -> loop (row :: acc) (n - 1) rest

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
            match RowStore.scanKeys space startKey 0 st.Store with
            | Error e -> err e
            | Ok scan ->
                match takeScanChunk BackfillChunkRows scan with
                | Error e -> err e
                | Ok(chunk, _) ->
                    match List.tryLast chunk with
                    | None -> ret st
                    | Some lastEntry ->
                        let lastKey = lastEntry.Key

                        match
                            commitChunk
                                (fun txn key -> ret (Transaction.remove space key txn))
                                (chunk |> List.map _.Key)
                                st
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
                        match KeyBuilder.ofValues values with
                        | Error e -> err e
                        | Ok prefix ->
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
                let mask = Array.init schema.Columns.Length (fun i -> List.contains i pos)

                RowStore.scanAtRawMasked (Keyspace.index idx.Name) [||] 0 None None mask store
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

                    let! filled =
                        applyOpChunks
                            (fun txn row ->
                                RowKey.indexKey withIndex idx row
                                |> Result.map (fun key -> Transaction.put space key row txn))
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

        let createIndexCore (ci: CreateIndex) state =
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

    /// 列を追加するときの既存行の値。
    /// DEFAULT を評価して列の型へ強制する（無ければ NULL）。
    let private addedColumnValue (col: ColumnDefinition) =
        match col.Constraint.Default with
        | Some d ->
            Eval.eval Schema.empty [||] d
            >>= fun value -> SqlValue.coerce col.DataType value
        | None -> ret SqlValue.Null

    /// ALTER で行数を合わせるときの、想定外の列数のエラー。
    let private widthError (schema: TableSchema) (row: SqlValue array) (expected: int) =
        sprintf "テーブル %s の行の列数が想定外です（%d 列、期待 %d 列）" schema.Name row.Length expected
        |> DbError.corruption
        |> err

    /// keyspace の全行を変換して transaction に積む。
    /// `transform` が None を返した行は書き換えない（対象外・変換済み）。
    let private rewriteSpace
        (space: string)
        (transform: SqlValue array -> Result<SqlValue array option, DbError>)
        (txn: Transaction)
        (store: RowStore)
        =
        let rec loop txn scan =
            match SstScan.next scan with
            | Error e -> err e
            | Ok None -> ret txn
            | Ok(Some(e, rest)) ->
                match e.Value with
                | None -> loop txn rest
                | Some row ->
                    match transform row with
                    | Error e -> err e
                    | Ok None -> loop txn rest
                    | Ok(Some next) ->
                        match Transaction.put space e.Key next txn |> Transaction.checkLimit with
                        | Error e -> err e
                        | Ok txn2 -> loop txn2 rest

        RowStore.scan space [||] 0 store
        >>= fun scan -> SstScan.withScan scan (fun scan -> loop txn scan)

    /// テーブル本体と全索引 keyspace の行を書き換える。
    /// <remarks>
    /// 索引エントリは行の値も保持するため、列を足す・削除するときは
    /// テーブル本体と同じ変換を全索引にも適用しないと、索引経由の読み取りだけ列数がずれる。
    /// </remarks>
    let private rewriteTableAndIndexes (schema: TableSchema) transform txn store =
        Keyspace.table schema.Name
        :: (schema.Indexes |> List.map (fun idx -> Keyspace.index idx.Name))
        |> foldOk (fun txn space -> rewriteSpace space transform txn store) txn

    /// `ALTER TABLE`。
    /// <remarks>
    /// 行の書き換え（列の追加・削除）とカタログ更新を **1つの transaction** で commit する。
    /// 途中で失敗しても何も変わらない（行とカタログがずれた状態を残さない）。
    /// そのため 1 transaction のバッファ上限（256MiB）を超えるテーブルは `CapacityError` になる。
    /// 主キーは LSM のキーなので、主キー列の追加・削除は許可しない。
    /// </remarks>
    let alterTable (a: AlterTable) (state: DbState) =
        let commitRewrite catalog build =
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
                        Store = store
                        Manager = mgr2
                        Catalog = catalog
                    }
            }

        let alterTableCore a state =
            match Catalog.findTable state.Catalog a.Table with
            | None -> err (sprintf "テーブル %s が存在しません" a.Table |> DbError.notFound)
            | Some defn ->
                result {
                    let! schema = Catalog.toSchema defn

                    let withIndexes =
                        { schema with
                            Indexes = Catalog.indexSchemasOf state.Catalog defn.Name
                        }

                    match a.Action with
                    | AlterTableAction.AddColumn col ->
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
                            if col.Constraint.Unique then
                                err (
                                    DbError.constraintViolation
                                        "ALTER TABLE ... ADD COLUMN で UNIQUE は指定できません（CREATE UNIQUE INDEX を使ってください）"
                                )
                            else
                                ret ()

                        do!
                            if col.Constraint.NotNull && col.Constraint.Default.IsNone then
                                err (
                                    DbError.constraintViolation (sprintf "NOT NULL 列 %s を追加するには DEFAULT が必要です" col.Name)
                                )
                            else
                                ret ()

                        let! pad = addedColumnValue col
                        let oldWidth = schema.Columns.Length
                        let newWidth = oldWidth + 1

                        let nextDefn =
                            { defn with
                                Columns = defn.Columns @ [ col ]
                            }

                        let transform (row: SqlValue array) =
                            if row.Length = newWidth then
                                ret None
                            elif row.Length = oldWidth then
                                ret (Some(Array.append row [| pad |]))
                            else
                                widthError schema row oldWidth

                        let build txn =
                            let nextSchema =
                                { withIndexes with
                                    Columns = nextDefn.Columns
                                }

                            rewriteTableAndIndexes nextSchema transform txn state.Store
                            |> Result.map (Catalog.putTableInTxn nextDefn)

                        return! commitRewrite (Catalog.addTable state.Catalog nextDefn) build
                    | AlterTableAction.DropColumn name ->
                        let! pos = TableSchema.requireIndexOf schema name

                        do!
                            if schema.PrimaryKey |> List.contains pos then
                                err (
                                    DbError.constraintViolation (sprintf "主キー列 %s は削除できません（主キーは LSM のキーに使われています）" name)
                                )
                            else
                                ret ()

                        do!
                            match
                                withIndexes.Indexes
                                |> List.tryFind (fun idx -> idx.Columns |> List.exists (fun c -> SqlName.equal c name))
                            with
                            | Some idx when Catalog.isReservedIndexName idx.Name ->
                                err (DbError.constraintViolation (sprintf "列 %s は UNIQUE 制約で使われているため削除できません" name))
                            | Some idx ->
                                err (DbError.constraintViolation (sprintf "列 %s は索引 %s で使われているため削除できません" name idx.Name))
                            | None -> ret ()

                        let oldWidth = schema.Columns.Length
                        let newWidth = oldWidth - 1

                        let newColumns =
                            schema.Columns
                            |> List.mapi (fun i c -> i, c)
                            |> List.choose (fun (i, c) -> if i = pos then None else Some c)

                        let nextDefn = { defn with Columns = newColumns }

                        let transform (row: SqlValue array) =
                            if row.Length = newWidth then
                                ret None
                            elif row.Length = oldWidth then
                                ret (Some(Array.init newWidth (fun i -> row.[if i < pos then i else i + 1])))
                            else
                                widthError schema row oldWidth

                        let build txn =
                            let nextSchema =
                                { withIndexes with
                                    Columns = newColumns
                                }

                            rewriteTableAndIndexes nextSchema transform txn state.Store
                            |> Result.map (Catalog.putTableInTxn nextDefn)

                        return! commitRewrite (Catalog.addTable state.Catalog nextDefn) build
                    | AlterTableAction.RenameColumn(oldName, newName) ->
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

                        let rename column =
                            if SqlName.equal column oldName then newName else column

                        let nextDefn =
                            { defn with
                                Columns =
                                    schema.Columns
                                    |> List.mapi (fun i c -> if i = pos then { c with Name = newName } else c)
                                PrimaryKeyColumns = defn.PrimaryKeyColumns |> List.map rename
                            }

                        let nextIndexes =
                            Catalog.indexesOf state.Catalog defn.Name
                            |> List.map (fun idx ->
                                { idx with
                                    Columns = idx.Columns |> List.map rename
                                })

                        let nextCatalog =
                            nextIndexes
                            |> List.fold Catalog.addIndex (Catalog.addTable state.Catalog nextDefn)

                        let build txn =
                            nextIndexes
                            |> List.fold (fun t idx -> Catalog.putIndexInTxn idx t) (Catalog.putTableInTxn nextDefn txn)
                            |> ret

                        return! commitRewrite nextCatalog build
                }

        alterTableCore a |> exclusiveDdl state

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

    let private runSelect (limits: ResultLimits) (state: DbState) (txn: Transaction) (stmt: SelectStatement) =
        contextOf state txn
        >>= fun ctx ->
            try
                Plan.runSelect ctx stmt
                >>= fun (columns, op) ->
                    Op.toArrayLimited limits.MaxRows limits.MaxBytes op
                    >>= (Array.map (SqlValue.copyRow >> LoadedRow.create)
                         >> Array.toList
                         >> all
                         >> Result.map (fun checkedRows ->
                             state, StatementResult.Create(checkedRows.Length, columns, checkedRows |> List.toArray)))
            finally
                ctx.Scope.DisposeAll()

    /// EXPLAIN: 実行計画を組み立て、木を 1行 1ノードの文字列にして返す。
    /// クエリ本体は実行しない（ただしサブクエリは計画時に評価される。Planner の計画経路と同じ）。
    let private runExplain (state: DbState) (txn: Transaction) (stmt: SelectStatement) =
        contextOf state txn
        >>= fun ctx ->
            try
                Plan.explainSelect ctx stmt
                >>= fun plan ->
                    Optimizer.explain plan.Context plan.Tree plan.Input
                    |> List.map (fun line -> LoadedRow.create [| SqlValue.Text line |])
                    |> all
                    |> Result.map (fun loaded ->
                        state, StatementResult.Create(loaded.Length, [| "plan" |], List.toArray loaded))
            finally
                ctx.Scope.DisposeAll()

    /// SELECT の計画だけを組み立てる（実行しない）。
    /// <remarks>
    /// ベンチ・診断用の入口。
    /// 返した計画の `Context` は snapshot を固定したまま再利用できるので、
    /// `Optimizer.exec` + `Op.toArrayLimited` を繰り返せば
    /// 「parse / plan を除いた実行」だけを測れる。
    /// 実行のたびに `plan.Context.Scope.DisposeAll()` を呼ぶこと。
    /// snapshot を登録しないため、読み取りの間に書き込みが入る用途には使わない。
    /// </remarks>
    let internal prepareSelect (state: DbState) (stmt: SelectStatement) =
        let snapshot =
            {
                SeqNo = state.Manager.LastCommitSeqNo
            }

        let txn = Transaction.create 0UL<txnId> snapshot

        match contextOf state txn with
        | Error e -> err e
        | Ok ctx ->
            match Plan.explainSelect ctx stmt with
            | Ok plan -> ret plan
            | Error e ->
                ctx.Scope.DisposeAll()
                err e

    /// SELECT を materialize せずにストリーミング用の Op として開く。
    /// 呼び出し側は返された `Cleanup` を必ず呼び、snapshot 登録と走査を解放する。
    let openSelect (state: DbState) (stmt: SelectStatement) =
        if state.Active.IsSome then
            err (DbError.internalError "transaction 中の SELECT は RowReader で開けません")
        else
            let mgr, txn = TransactionManager.beginTxn state.Manager
            let registration = state.Store.Runtime.Register(state.Root, txn.Snapshot.SeqNo)

            let startState =
                { state with
                    Manager = mgr
                    ActiveRegistration = Some registration
                }

            let run () =
                match contextOf startState txn with
                | Error e ->
                    state.Store.Runtime.Unregister registration
                    err e
                | Ok ctx ->
                    match Plan.runSelect ctx stmt with
                    | Error e ->
                        ctx.Scope.DisposeAll()
                        state.Store.Runtime.Unregister registration
                        err e
                    | Ok(columns, op) ->
                        ret
                            {
                                Columns = columns
                                Op = op
                                Scope = ctx.Scope
                                Cleanup =
                                    fun () ->
                                        ctx.Scope.DisposeAll()
                                        state.Store.Runtime.Unregister registration
                            }

            state.Store.Runtime.WithShared run

    let private resolveWhere (target: Schema) (ctx: ExecContext) =
        function
        | None -> ret None
        | Some e -> Plan.resolveDmlExprWith target ctx e |> Result.map Some

    let private runInsert (stmt: InsertStatement) (ctx: ExecContext) =
        result {
            let! schema = ExecContext.schemaOf stmt.Table.Name ctx

            return!
                match stmt.Source with
                | InsertSource.Values rows ->
                    result {
                        let! resolved =
                            rows
                            |> List.map (fun row -> row |> List.map (Plan.resolveDmlExpr ctx) |> all)
                            |> all

                        return! DmlOps.insert ctx schema stmt.Columns resolved
                    }
                | InsertSource.Select sel ->
                    result {
                        let! _, op = Plan.runSelect ctx sel
                        return! DmlOps.insertSelect ctx schema stmt.Columns op
                    }
        }

    let private runUpdate (stmt: UpdateStatement) (ctx: ExecContext) =
        result {
            let! schema = ExecContext.schemaOf stmt.Table.Name ctx
            let target = Schema.ofTable schema.Name None schema

            let! assignments =
                stmt.Assignments
                |> List.map (fun (name, e) -> Plan.resolveDmlExprWith target ctx e |> Result.map (fun e2 -> name, e2))
                |> all

            let! whereE = resolveWhere target ctx stmt.Where
            return! DmlOps.update ctx schema assignments whereE
        }

    let private runDelete (stmt: DeleteStatement) (ctx: ExecContext) =
        result {
            let! schema = ExecContext.schemaOf stmt.Table.Name ctx
            let target = Schema.ofTable schema.Name None schema
            let! whereE = resolveWhere target ctx stmt.Where
            return! DmlOps.delete ctx schema whereE
        }

    let private runDml (state: DbState) (txn: Transaction) (run: ExecContext -> Result<(Transaction * int), DbError>) =
        contextOf state txn
        >>= fun ctx ->
            try
                result {
                    let! outcome = run ctx
                    let txn2, n = outcome
                    return { state with Pending = Some txn2 }, StatementResult.Affected n
                }
            finally
                ctx.Scope.DisposeAll()

    let private runDdl (state: DbState) (run: DbState -> Result<'a, DbError>) =
        if state.Active.IsSome then
            err (
                DbError.create
                    ErrorKind.SyntaxError
                    "トランザクション中は DDL（CREATE / DROP / ALTER）を実行できません。COMMIT または ROLLBACK してから実行してください"
            )
        else
            run state

    let private noRows = Result.map (fun s -> s, StatementResult.Empty)

    let private runStatement (limits: ResultLimits) (state: DbState) (txn: Transaction) =
        function
        | Statement.Select stmt -> runSelect limits state txn stmt
        | Statement.Explain stmt -> runExplain state txn stmt
        | Statement.Insert stmt -> runInsert stmt |> runDml state txn
        | Statement.Update stmt -> runUpdate stmt |> runDml state txn
        | Statement.Delete stmt -> runDelete stmt |> runDml state txn
        | Statement.CreateTable c -> Ddl.createTable c |> runDdl state |> noRows
        | Statement.DropTable(n, ifE) -> Ddl.dropTable n ifE |> runDdl state |> noRows
        | Statement.CreateIndex ci -> Ddl.createIndex ci |> runDdl state |> noRows
        | Statement.DropIndex(n, ifE) -> Ddl.dropIndex n ifE |> runDdl state |> noRows
        | Statement.AlterTable a -> Ddl.alterTable a |> runDdl state |> noRows
        | Statement.Begin
        | Statement.Commit
        | Statement.Rollback -> ret state |> noRows

    let executeStatementWithLimits (limits: ResultLimits) (state: DbState) (stmt: Statement) =
        match state.Active with
        | Some activeTxn ->
            let running = state.Pending |> Option.defaultValue activeTxn
            runStatement limits state running stmt
        | None ->
            let mgr, txn = TransactionManager.beginTxn state.Manager
            let registration = state.Store.Runtime.Register(state.Root, txn.Snapshot.SeqNo)

            let isReadOnly =
                match stmt with
                | Statement.Select _
                | Statement.Explain _ -> true
                | _ -> false

            let run () =
                result {
                    let startState =
                        { state with
                            Manager = mgr
                            ActiveRegistration = Some registration
                        }

                    let! midState, result = runStatement limits startState txn stmt

                    if isReadOnly then
                        return state, result
                    else
                        let toCommit = midState.Pending |> Option.defaultValue txn
                        let! nextState = DbStateOps.commitTransaction toCommit midState
                        return nextState, result
                }

            let finish r =
                state.Store.Runtime.Unregister registration
                r

            let isDdl =
                match stmt with
                | Statement.CreateTable _
                | Statement.DropTable _
                | Statement.CreateIndex _
                | Statement.DropIndex _
                | Statement.AlterTable _ -> true
                | _ -> false

            (if isDdl then run () else state.Store.Runtime.WithShared run) |> finish

    let executeBegin (state: DbState) =
        match state.Active with
        | Some _ -> ret state
        | None ->
            state.Store.Runtime.WithShared(fun () ->
                let mgr, txn = TransactionManager.beginTxn state.Manager
                let registration = state.Store.Runtime.Register(state.Root, txn.Snapshot.SeqNo)

                match Catalog.load state.Store with
                | Error e ->
                    state.Store.Runtime.Unregister registration
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
                state.ActiveRegistration |> Option.iter state.Store.Runtime.Unregister
                nextState)

    let executeRollback (state: DbState) =
        match state.Active with
        | None -> state
        | Some txn ->
            let mgr = TransactionManager.abort txn state.Manager
            state.ActiveRegistration |> Option.iter state.Store.Runtime.Unregister

            { state with
                Manager = mgr
                Active = None
                Pending = None
                ActiveRegistration = None
            }
