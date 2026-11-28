// ===========================================================================
// Engine — 実行状態と型。
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
        /// ANALYZE など長時間処理のキャンセル。
        Cancel: System.Threading.CancellationToken
        /// 長時間処理の進捗（処理名, 処理済み行数）。
        Progress: (string * int64 -> unit) option
    }

module internal ResultLimits =
    /// 上限なしの `ResultLimits`。
    let Unlimited =
        {
            MaxRows = System.Int64.MaxValue
            MaxBytes = System.Int64.MaxValue
            Budget = MemoryBudget.Default
            Cancel = System.Threading.CancellationToken.None
            Progress = None
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

    /// transaction 中か。
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
        /// COMMIT 前の DML が触った行数（小文字のテーブル名 -> 行数）。
        /// COMMIT で統計の `Changes` に畳み込み、ROLLBACK で捨てる。
        StatsDeltas: Map<string, int64>
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
            StatsDeltas = Map.empty
        }

    /// 統計の更新行数を transaction に積む（COMMIT と同じ atomically に反映する）。
    /// 統計の無いテーブルは対象外（鮮度を示す相手が無い）。
    let private applyStatsDeltas (txn: Transaction) (state: ExecState) =
        state.StatsDeltas
        |> Map.fold
            (fun (txn, catalog) table delta ->
                match Catalog.findStats table catalog with
                | None -> txn, catalog
                | Some stats ->
                    let next =
                        { stats with
                            Changes = stats.Changes + delta
                        }

                    Catalog.putStatsInTxn next txn, Catalog.addStats next catalog)
            (txn, state.Committed.Catalog)

    /// DML が触った行数を積む（COMMIT まで持つ）。
    let addStatsDelta (table: string) (rows: int) (state: ExecState) =
        if rows <= 0 || Option.isNone (Catalog.findStats table state.Catalog) then
            state
        else
            let key = table.ToLowerInvariant()
            let delta = state.StatsDeltas |> Map.tryFind key |> Option.defaultValue 0L

            { state with
                StatsDeltas = Map.add key (delta + int64 rows) state.StatsDeltas
            }

    /// 統計を取り直したテーブルの未反映の行数を捨てる。
    let clearStatsDeltas (tables: string list) (state: ExecState) =
        if List.isEmpty tables || Map.isEmpty state.StatsDeltas then
            state
        else
            let cleared = tables |> List.map (fun t -> t.ToLowerInvariant()) |> Set.ofList

            { state with
                StatsDeltas = state.StatsDeltas |> Map.filter (fun k _ -> not (Set.contains k cleared))
            }

    /// transaction を manager で確定し、確定状態を返す。
    let commitTransaction (txn: Transaction) (state: ExecState) =
        result {
            let txn, catalog = applyStatsDeltas txn state

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
                    Catalog = catalog
                }
        }

    /// カタログを保存し、保存後の Store と新しい Catalog で state を作り直す。
    /// （CREATE / DROP すべてが使う「保存 → state 更新」の定型）
    /// <remarks>
    /// カタログのエントリは Transaction を介さず `Catalog.save` が engine の `NextSeqNo` から採番する。
    /// そのままでは Manager の採番がカタログより前を指し、データとカタログで SeqNo が重複する。
    /// 保存後に Manager を engine の高水位まで進め、snapshot もそれに追随させる。
    /// </remarks>
    let saved (next: Catalog) (state: ExecState) =
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
    /// - 文を跨いで残っている明示 transaction（BEGIN のまま）があれば **待たずに ConflictError** にする。
    ///   古いスキーマで計算した索引エントリを commit されると、索引に載らない行ができるため。
    ///   待つと、commit されないまま残った transaction で永久に止まるので失敗側に倒す。
    /// - 文の実行中だけの登録（読み取り・暗黙 transaction）は拒否しない。
    ///   共有ロックを保持しているので、この排他ロックが完了を待てる
    ///   （判定は登録の種別。GC 境界は種別を問わない `OldestExcept`）。
    /// - 新しい transaction の開始と文の実行は排他の間ブロックされる（実行後は新しいカタログを見る）。
    /// </remarks>
    let exclusiveDdlWith (state: ExecState) (run: ExecState -> Result<'a, DbError>) =
        state.Store.Runtime.WithExclusive(fun () ->
            if state.Store.Runtime.TransactionExists(state.Root, state.ActiveRegistration) then
                DbError.conflict "実行中の transaction があります。COMMIT または ROLLBACK してから DDL を実行してください"
                |> err
            else
                run state)

    /// DDL を排他の下で実行する
    /// （文を跨ぐ明示 transaction があれば待たずに ConflictError）。
    let exclusiveDdl (state: ExecState) (run: ExecState -> Result<ExecState, DbError>) = exclusiveDdlWith state run

    /// backfill を commit する 1チャンクの行数。
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

    /// 走査から最大 n 件を取り出す（エラー時は走査を解放する）。
    [<TailCall>]
    let rec private takeScanChunkLoop (acc: SstEntry list) (n: int) (scan: SstScan) =
        if n <= 0 then
            ret (List.rev acc, scan)
        else
            match SstScan.next scan with
            | Ok None -> ret (List.rev acc, SstScan.Empty)
            | Ok(Some(e, rest)) -> takeScanChunkLoop (e :: acc) (n - 1) rest
            | Error e ->
                SstScan.dispose scan
                err e

    /// 走査から `size` 件までを取り出す。
    let private takeScanChunk = takeScanChunkLoop []

    /// Op から最大 n 行を取り出す。
    [<TailCall>]
    let rec private takeOpChunkLoop (acc: SqlValue array list) (n: int) (op: Op) =
        if n <= 0 then
            ret (List.rev acc, op)
        else
            match Op.next op with
            | OpStep.OpError e -> err e
            | OpStep.OpDone -> ret (List.rev acc, Op.Empty)
            | OpStep.OpRow(row, rest) -> takeOpChunkLoop (row :: acc) (n - 1) rest

    /// Op から `size` 行までを取り出す。
    let private takeOpChunk = takeOpChunkLoop []

    /// Op をチャンク単位で消費する（全件を実体化しない）。
    [<TailCall>]
    let rec private applyOpChunksLoop apply (op: Op) (st: ExecState) =
        match takeOpChunk BackfillChunkRows op with
        | Ok([], _) -> ret st
        | Ok(chunk, rest) ->
            match commitChunk apply chunk st with
            | Ok st' -> applyOpChunksLoop apply rest st'
            | Error e -> err e
        | Error e -> err e

    /// Op をチャンク単位で消費する（全件を実体化しない）。
    let applyOpChunks (apply: Transaction -> SqlValue array -> Result<Transaction, DbError>) = applyOpChunksLoop apply

    /// keyspace の全エントリをチャンク単位で tombstone 消去する。
    /// <remarks>
    /// 同じ keyspace を走査しながら書くため、1チャンク書くたびに新しい store で
    /// `lastKey` から走査し直す。走査中に compaction がファイルを消しても、
    /// 古いスナップショットのファイルを開き続けない。
    /// 起動時の後始末は `RowStore.clearKeyspaceChunks` を使う
    /// （その時点には `ExecState` が無い）。
    /// </remarks>
    [<TailCall>]
    let rec private clearSpaceChunksLoop (space: string) (st: ExecState) (startKey: byte array) =
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
                        commitChunk (fun txn key -> ret (Transaction.remove space key txn)) (chunk |> List.map _.Key) st
                    with
                    | Ok st' -> clearSpaceChunksLoop space st' lastKey
                    | Error e -> err e
            | Error e -> err e
        | Error e -> err e

    /// keyspace をチャンク単位で全消去する（DDL 用）。
    let clearSpaceChunksResult (space: string) (state: ExecState) = clearSpaceChunksLoop space state [||]
