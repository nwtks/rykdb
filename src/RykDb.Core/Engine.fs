// ===========================================================================
// Engine — 文の実行層（SELECT / DML / DDL / transaction）。
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
type internal ResultLimits =
    {
        MaxRows: int64
        MaxBytes: int64
        /// 演算子が使ってよいメモリ作業領域とスピル先。
        Budget: MemoryBudget
    }

module internal ResultLimits =
    let Unlimited =
        {
            MaxRows = System.Int64.MaxValue
            MaxBytes = System.Int64.MaxValue
            Budget = MemoryBudget.Default
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

/// 確定済み（commit 済み）の DB 状態。
/// <remarks>
/// 接続ローカルの transaction を持たない。
/// 「確定状態に transaction が載っている」状態を型で作れないようにするため、
/// transaction は `TxnState`、実行時の合成は `ExecState` に分ける。
/// </remarks>
type internal Committed =
    {
        Root: string
        Store: RowStore
        Catalog: Catalog
        Manager: TransactionManager
        /// WAL の fsync を Session のグループコミットに任せるか。
        DeferDurability: bool
    }

/// 接続ローカルの transaction 状態。
/// <remarks>
/// 確定状態（`Committed`）は `DatabaseHost` が共有し、ここには接続だけが持つ
/// transaction を置く。`Snapshot.SeqNo` は BEGIN 時に固定されるので、
/// 実行のたびに最新の Store へ載せ替えても可視性（repeatable read）は変わらない。
/// </remarks>
type internal TxnState =
    {
        /// BEGIN 時点のカタログ。transaction 中は DDL が拒否されるので変化しない。
        CatalogAtBegin: Catalog
        /// 明示 transaction。
        Active: Transaction option
        /// 未 commit の変更を積んだ transaction。
        Pending: Transaction option
        /// 進行中 transaction の ActiveSnapshot 登録のリース。
        /// 接続（`Session`）が Close されずに GC された場合も、
        /// 最後のリースが回収された時点で登録が外れる（`RegistrationLease`）。
        Lease: RegistrationLease option
    }

module internal TxnState =
    /// transaction を持たない状態。
    let none: TxnState =
        {
            CatalogAtBegin = Catalog.Empty
            Active = None
            Pending = None
            Lease = None
        }

    let inTransaction (state: TxnState) = state.Active.IsSome

/// 1回の文実行の入力（確定状態 + 接続ローカルの transaction + 実行中の登録）。
/// <remarks>
/// `Registration` は実行中の文（暗黙 transaction を含む）の一時的な登録で、
/// 確定状態には載らない。
/// 読み取りは既存の名前（`Store` / `Catalog` / `Active` など）をそのまま使える。
/// 更新は `Committed` / `Txn` を組み直す。
/// </remarks>
type internal ExecState =
    {
        Committed: Committed
        Txn: TxnState
        /// 実行中の文の ActiveSnapshot 登録（暗黙 transaction・DDL の排他判定用）。
        Registration: ActiveRegistration option
    }

    member this.Root = this.Committed.Root
    member this.Store = this.Committed.Store
    member this.Manager = this.Committed.Manager
    member this.DeferDurability = this.Committed.DeferDurability

    /// 有効なカタログ（transaction 中は BEGIN 時点、外は確定版）。
    member this.Catalog =
        if this.Txn.Active.IsSome then
            this.Txn.CatalogAtBegin
        else
            this.Committed.Catalog

    member this.Active = this.Txn.Active
    member this.Pending = this.Txn.Pending
    member this.ActiveRegistration = this.Registration

module internal ExecState =
    /// 確定状態だけから実行状態を作る（transaction なし）。
    let ofCommitted (state: Committed) =
        {
            Committed = state
            Txn = TxnState.none
            Registration = None
        }

    let commitTransaction (txn: Transaction) (state: ExecState) =
        result {
            let! store, manager =
                RowStore.commitToStoreWith
                    state.Committed.DeferDurability
                    state.Committed.Root
                    state.Registration
                    state.Committed.Manager
                    txn
                    state.Committed.Store

            return
                { state.Committed with
                    Store = store
                    Manager = manager
                }
        }

    /// カタログを保存し、保存後の Store と新しい Catalog で state を作り直す。
    /// （CREATE / DROP すべてが使う「保存 → state 更新」の定型）
    /// <remarks>
    /// カタログのエントリは Transaction を介さず `Catalog.save` が engine の `NextSeqNo` から採番する。
    /// そのままでは Manager の採番がカタログより前を指し、データとカタログで SeqNo が重複する。
    /// 保存後に Manager を engine の高水位まで進め、snapshot もそれに追随させる。
    /// </remarks>
    let private saved (next: Catalog) (state: ExecState) =
        result {
            let! store = Catalog.save state.Committed.Store state.Committed.Catalog next

            return
                { state with
                    Committed =
                        { state.Committed with
                            Store = store
                            Catalog = next
                            Manager = TransactionManager.synchronize (RowStore.lastSeqNo store) state.Committed.Manager
                        }
                }
        }

    /// DDL を排他の内側で実行する。
    /// <remarks>
    /// - 書き込み中の文があれば、それが終わるまで待つ（`RootRuntime` の共有ロック）。
    /// - 実行中の transaction があれば **待たずに ConflictError** にする。
    ///   古いスキーマで計算した索引エントリを commit されると、索引に載らない行ができるため。
    ///   待つと、commit されないまま残った transaction で永久に止まるので失敗側に倒す。
    /// - 新しい transaction の開始と文の実行は排他の間ブロックされる（実行後は新しいカタログを見る）。
    /// </remarks>
    let private exclusiveDdl (state: ExecState) (run: ExecState -> Result<ExecState, DbError>) =
        state.Store.Runtime.WithExclusive(fun () ->
            match state.Store.Runtime.OldestExcept(state.Root, state.ActiveRegistration) with
            | Some _ -> err (DbError.conflict "実行中の transaction があります。COMMIT または ROLLBACK してから DDL を実行してください")
            | None -> run state)

    let private BackfillChunkRows = 8192

    /// 1チャンク分を transaction にして commit する。
    let private commitChunk
        (apply: Transaction -> 'a -> Result<Transaction, DbError>)
        (chunk: 'a list)
        (st: ExecState)
        =
        let mgr, txn =
            TransactionManager.beginTxnWithLimit (System.Int64.MaxValue |> Some) st.Manager

        result {
            let! txn2 = foldOk apply txn chunk

            let! store, mgr2 =
                RowStore.commitToStoreWith st.DeferDurability st.Root st.ActiveRegistration mgr txn2 st.Store

            return
                { st with
                    Committed =
                        { st.Committed with
                            Store = store
                            Manager = mgr2
                        }
                }
        }

    /// 走査から `size` 件までを取り出す。
    let private takeScanChunk =
        let rec loop acc n scan =
            if n <= 0 then
                ret (List.rev acc, scan)
            else
                match SstScan.next scan with
                | Ok None -> ret (List.rev acc, SstScan.Empty)
                | Ok(Some(e, rest)) -> loop (e :: acc) (n - 1) rest
                | Error e ->
                    SstScan.dispose scan
                    err e

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
            | Ok([], _) -> ret st
            | Ok(chunk, rest) ->
                match commitChunk apply chunk st with
                | Ok st' -> loop rest st'
                | Error e -> err e
            | Error e -> err e

        loop

    /// keyspace の全エントリをチャンク単位で tombstone 消去する。
    /// <remarks>
    /// 同じ keyspace を走査しながら書くため、1チャンク書くたびに新しい store で
    /// `lastKey` から走査し直す。走査中に compaction がファイルを消しても、
    /// 古いスナップショットのファイルを開き続けない。
    /// </remarks>
    let private clearSpaceChunksResult (space: string) (state: ExecState) =
        let rec loop (st: ExecState) startKey =
            match RowStore.scanKeys space startKey 0 st.Store with
            | Ok scan ->
                match takeScanChunk BackfillChunkRows scan with
                | Ok(chunk, rest) ->
                    SstScan.dispose rest

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
                        | Ok st' -> loop st' lastKey
                        | Error e -> err e
                | Error e -> err e
            | Error e -> err e

        loop state [||]

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

                let! cleaned = spaces |> foldOk (fun st sp -> clearSpaceChunksResult sp st) state
                return! saved catalog2 cleaned
            }

    let createTable (c: CreateTable) (state: ExecState) = createTableCore c |> exclusiveDdl state

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
                    match saved (Catalog.removeTable name state.Catalog) state with
                    | Ok next -> ret next
                    | Error e ->
                        DropCleanup.clear state.Store.Root |> ignore
                        err e

                match spaces |> foldOk (fun st sp -> clearSpaceChunksResult sp st) next with
                | Ok cleaned ->
                    DropCleanup.clear state.Store.Root |> ignore
                    return cleaned
                | Error _ -> return next
            }

    let dropTable (name: string) (ifExists: bool) (state: ExecState) =
        dropTableCore name ifExists |> exclusiveDdl state

    let private createIndexCore (ci: CreateIndex) (state: ExecState) =
        let rec checkEntries (idx: IndexSchema) previous scan =
            match SstScan.next scan with
            | Ok None -> ret ()
            | Ok(Some(e, rest)) ->
                match OrderPreserving.readValues idx.Columns.Length (Cursor.create e.Key) with
                | Ok(values, _) ->
                    if values |> Array.exists SqlValue.isNull then
                        checkEntries idx previous rest
                    else
                        match KeyBuilder.ofValues values with
                        | Ok prefix ->
                            match previous with
                            | Some before when Bytes.compare before prefix = 0 ->
                                err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                            | _ -> checkEntries idx (Some prefix) rest
                        | Error e -> err e
                | Error e -> err e
            | Error e -> err e

        let checkUniqueIndex (idx: IndexSchema) store =
            RowStore.scanKeys (Keyspace.index idx.Name) [||] 0 store
            >>= fun scan -> SstScan.withScan scan (fun scan -> checkEntries idx None scan)

        let clearSpaceChunks state space =
            match clearSpaceChunksResult space state with
            | Ok next -> next
            | Error _ -> state

        let backfillIndex tableName idx (state: ExecState) =
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
                    let! cleared = clearSpaceChunksResult space state
                    let! rows = Dml.scanByPrimaryKey cleared.Store withIndex 0 None

                    let! filled =
                        applyOpChunks
                            (fun txn row ->
                                RowKey.indexKey withIndex idx row
                                |> Result.map (fun key -> Transaction.put space key Array.empty txn))
                            rows
                            cleared

                    if idx.Unique then
                        match checkUniqueIndex idx filled.Store with
                        | Ok() -> return filled
                        | Error e ->
                            clearSpaceChunks filled space |> ignore
                            return! err e
                    else
                        return filled
                }

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
                return! saved next filled
            }

    let createIndex (ci: CreateIndex) (state: ExecState) =
        match Catalog.findIndex ci.Name state.Catalog with
        | Some _ when ci.IfNotExists -> ret state
        | _ -> createIndexCore ci |> exclusiveDdl state

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
                    match saved (Catalog.removeIndex name state.Catalog) state with
                    | Ok next -> ret next
                    | Error e ->
                        DropCleanup.clear state.Store.Root |> ignore
                        err e

                match clearSpaceChunksResult space next with
                | Ok cleaned ->
                    DropCleanup.clear state.Store.Root |> ignore
                    return cleaned
                | Error _ -> return next
            }

    let dropIndex (name: string) (ifExists: bool) (state: ExecState) =
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
            | Ok None -> ret txn
            | Ok(Some(e, rest)) ->
                match e.Value with
                | None -> loop txn rest
                | Some row ->
                    match transform row with
                    | Ok None -> loop txn rest
                    | Ok(Some next) ->
                        match Transaction.put space e.Key next txn |> Transaction.checkLimit with
                        | Ok txn2 -> loop txn2 rest
                        | Error e -> err e
                    | Error e -> err e
            | Error e -> err e

        RowStore.scan space [||] 0 store
        >>= fun scan -> SstScan.withScan scan (fun scan -> loop txn scan)

    /// `ALTER TABLE`。
    /// <remarks>
    /// 行の書き換え（列の追加・削除）とカタログ更新を **1つの transaction** で commit する。
    /// 途中で失敗しても何も変わらない（行とカタログがずれた状態を残さない）。
    /// そのため 1 transaction のバッファ上限（256MiB）を超えるテーブルは `CapacityError` になる。
    /// 主キーは LSM のキーなので、主キー列の追加・削除は許可しない。
    /// </remarks>
    let private alterTableCore (a: AlterTable) (state: ExecState) =
        let rewriteTable (schema: TableSchema) =
            rewriteSpace (Keyspace.table schema.Name)

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
                        Committed =
                            { state.Committed with
                                Store = store
                                Manager = mgr2
                                Catalog = catalog
                            }
                    }
            }

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
                        if col.Constraint.References.IsSome then
                            err (
                                DbError.constraintViolation
                                    "ALTER TABLE ... ADD COLUMN で REFERENCES は指定できません（テーブルを作り直してください）"
                            )
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

                        rewriteTable nextSchema transform txn state.Store
                        |> Result.map (Catalog.putTableInTxn nextDefn)

                    return! commitRewrite (Catalog.addTable nextDefn state.Catalog) build
                | AlterTableAction.DropColumn name ->
                    let! pos = TableSchema.requireIndexOf schema name

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

                        rewriteTable nextSchema transform txn state.Store
                        |> Result.map (Catalog.putTableInTxn nextDefn)

                    return! commitRewrite (Catalog.addTable nextDefn state.Catalog) build
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

                    let renameLocal (fk: ForeignKeyDefinition) =
                        { fk with
                            Columns = fk.Columns |> List.map rename
                            ParentColumns =
                                if Catalog.eqName fk.ParentTable defn.Name then
                                    fk.ParentColumns |> List.map rename
                                else
                                    fk.ParentColumns
                        }

                    let referencing =
                        state.Catalog.Tables
                        |> List.filter (fun t -> not (Catalog.eqName t.Name defn.Name))
                        |> List.choose (fun t ->
                            let renamed =
                                t.ForeignKeys
                                |> List.map (fun fk ->
                                    if Catalog.eqName fk.ParentTable defn.Name then
                                        { fk with
                                            ParentColumns = fk.ParentColumns |> List.map rename
                                        }
                                    else
                                        fk)

                            if renamed = t.ForeignKeys then
                                None
                            else
                                Some { t with ForeignKeys = renamed })

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
                                 (Catalog.addTable nextDefn state.Catalog))

                    let build txn =
                        referencing
                        |> List.fold
                            (fun t childDefn -> Catalog.putTableInTxn childDefn t)
                            (nextIndexes
                             |> List.fold
                                 (fun t idx -> Catalog.putIndexInTxn idx t)
                                 (Catalog.putTableInTxn nextDefn txn))
                        |> ret

                    return! commitRewrite nextCatalog build
            }

    let alterTable (a: AlterTable) (state: ExecState) = alterTableCore a |> exclusiveDdl state

module internal Engine =
    let private addTable (state: ExecState) (ctx: ExecContext) (t: TableDefinition) =
        Catalog.toSchema t
        >>= fun schema ->
            let withIndexes =
                { schema with
                    Indexes = Catalog.indexSchemasOf t.Name state.Catalog
                }

            ret (ExecContext.withSchema t.Name withIndexes ctx)

    let private contextOf (budget: MemoryBudget) (state: ExecState) (txn: Transaction) =
        let baseCtx =
            ExecContext.create state.Store (Snapshot.toVisible txn.Snapshot)
            |> ExecContext.withTransaction txn
            |> ExecContext.withCatalog state.Catalog

        let ctx =
            ExecContext.withBudget
                { budget with
                    Scope = Some baseCtx.Scope
                }
                baseCtx

        state.Catalog.Tables |> foldOk (addTable state) ctx

    let private runSelect (limits: ResultLimits) (state: ExecState) (txn: Transaction) (stmt: SelectStatement) =
        contextOf limits.Budget state txn
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
    /// クエリ本体は実行しない（ただしサブクエリは計画時に評価される）。
    let private runExplain (limits: ResultLimits) (state: ExecState) (txn: Transaction) (stmt: SelectStatement) =
        contextOf limits.Budget state txn
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
    let internal prepareSelect (state: ExecState) (stmt: SelectStatement) =
        let snapshot =
            {
                SeqNo = state.Manager.LastCommitSeqNo
            }

        Transaction.create 0UL<txnId> snapshot
        |> contextOf MemoryBudget.Default state
        >>= fun ctx ->
            match Plan.explainSelect ctx stmt with
            | Ok plan -> ret plan
            | Error e ->
                ctx.Scope.DisposeAll()
                err e

    /// SELECT を materialize せずにストリーミング用の Op として開く。
    /// 呼び出し側は返された `Cleanup` を必ず呼び、snapshot 登録と走査を解放する。
    let openSelect (budget: MemoryBudget) (state: ExecState) (stmt: SelectStatement) =
        if state.Active.IsSome then
            err (DbError.internalError "transaction 中の SELECT は RowReader で開けません")
        else
            let mgr, txn = TransactionManager.beginTxn state.Manager
            let registration = state.Store.Runtime.Register(state.Root, txn.Snapshot.SeqNo)

            let startState =
                { state with
                    Committed = { state.Committed with Manager = mgr }
                    Registration = Some registration
                }

            let run () =
                match contextOf budget startState txn with
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

    let private runDml
        (limits: ResultLimits)
        (state: ExecState)
        (txn: Transaction)
        (run: ExecContext -> Result<(Transaction * int), DbError>)
        =
        contextOf limits.Budget state txn
        >>= fun ctx ->
            try
                result {
                    let! outcome = run ctx
                    let txn2, n = outcome

                    return
                        { state with
                            Txn = { state.Txn with Pending = Some txn2 }
                        },
                        StatementResult.Affected n
                }
            finally
                ctx.Scope.DisposeAll()

    let private runDdl (state: ExecState) (run: ExecState -> Result<ExecState, DbError>) =
        if state.Active.IsSome then
            err (
                DbError.create
                    ErrorKind.SyntaxError
                    "トランザクション中は DDL（CREATE / DROP / ALTER）を実行できません。COMMIT または ROLLBACK してから実行してください"
            )
        else
            run state |> Result.map (fun next -> next, StatementResult.Empty)

    let private noRows = Result.map (fun s -> s, StatementResult.Empty)

    let private runStatement (limits: ResultLimits) (state: ExecState) (txn: Transaction) =
        function
        | Statement.Select stmt -> runSelect limits state txn stmt
        | Statement.Explain stmt -> runExplain limits state txn stmt
        | Statement.Insert stmt -> runInsert stmt |> runDml limits state txn
        | Statement.Update stmt -> runUpdate stmt |> runDml limits state txn
        | Statement.Delete stmt -> runDelete stmt |> runDml limits state txn
        | Statement.CreateTable c -> ExecState.createTable c |> runDdl state
        | Statement.DropTable(n, ifE) -> ExecState.dropTable n ifE |> runDdl state
        | Statement.CreateIndex ci -> ExecState.createIndex ci |> runDdl state
        | Statement.DropIndex(n, ifE) -> ExecState.dropIndex n ifE |> runDdl state
        | Statement.AlterTable a -> ExecState.alterTable a |> runDdl state
        | Statement.Begin
        | Statement.Commit
        | Statement.Rollback -> ret state |> noRows

    let executeStatementWithLimits (limits: ResultLimits) (state: ExecState) (stmt: Statement) =
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
                            Committed = { state.Committed with Manager = mgr }
                            Registration = Some registration
                        }

                    let! midState, result = runStatement limits startState txn stmt

                    if isReadOnly then
                        return state, result
                    else
                        let toCommit = midState.Pending |> Option.defaultValue txn
                        let! committed = ExecState.commitTransaction toCommit midState
                        return ExecState.ofCommitted committed, result
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

    let executeCommit (state: ExecState) =
        match state.Active with
        | None -> ret state
        | Some activeTxn ->
            let txn = state.Pending |> Option.defaultValue activeTxn

            ExecState.commitTransaction txn state
            |> Result.map (fun committed ->
                state.ActiveRegistration |> Option.iter state.Store.Runtime.Unregister
                ExecState.ofCommitted committed)

    let executeRollback (state: ExecState) =
        match state.Active with
        | None -> state
        | Some txn ->
            let mgr = TransactionManager.abort txn state.Manager
            state.ActiveRegistration |> Option.iter state.Store.Runtime.Unregister
            ExecState.ofCommitted { state.Committed with Manager = mgr }
