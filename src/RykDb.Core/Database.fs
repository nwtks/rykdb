// ===========================================================================
// Database — 公開エントリポイント。
// ライブラリ利用者・CLI・TCP サーバはこの層だけを見る。
//
//   let db = Database.OpenWith root
//   let db, r = db.Execute "INSERT ..."
//   db.Begin() / db.Commit() / db.Rollback() / db.Close()
//
// * Execute は暗黙の transaction で実行し自動 commit。
//   Begin 以降は Commit / Rollback まで同じ transaction。
//   snapshot は transaction 開始時に固定(repeatable read)。
// * DDL は即時反映（カタログを書き換えた時点で見える）。
// ===========================================================================

namespace RykDb.Core

/// DB のハンドル。
/// <remarks>
/// ハンドルは underlying state を不変に持つ。
/// 書き込みは新しい state を返すので、ハンドル自体は変更されない。
/// これにより「複数の接続が同じ DB を触る」状況を型で表現できる。
/// </remarks>
type Database internal (root: string, state: DbState) =
    /// このハンドルの生存に紐づく ActiveSnapshot 登録のリース。
    /// ハンドルが commit / rollback / Close されずに捨てられても、
    /// 最後の 1本が GC された時点で登録が外れるよう保持しておく
    /// （放棄された transaction で DDL が永久に ConflictError にならないようにする）。
    let lease = state.ActiveRegistration |> Option.map RegistrationLease

    /// このハンドルがプロセスロックの所有者か判定するためのトークン。
    /// `Execute` / `Begin` / `Commit` は新しいハンドルを返すので、
    /// 新しいハンドルを作るたびに所有者を置き換える。
    /// 古いハンドルの Close で、生きている新しいハンドルのロックを外さないため。
    let storeLeaseToken =
        state.Store.Lock |> Option.map (fun lockLease -> lockLease.Attach())

    /// DB を開く。
    /// <remarks>
    /// 同じ root はプロセス内で 1つしか開けない。2回目は `IoError` を返す
    /// （別の `RowStore` ができて memtable が分かれ、commit が見えないため）。
    /// 別プロセスが開いている場合も `IoError`。
    /// </remarks>
    static member OpenWith(root: string) =
        result {
            let! store = RowStore.openStore root
            return! Database.OpenWithStore root store
        }

    /// 与えた RowStore で DB を開く。
    static member internal OpenWithStore (root: string) (store: RowStore) =
        match Catalog.load store with
        | Error e ->
            RowStore.close store |> ignore
            err e
        | Ok catalog ->
            match TransactionManager.create () |> DropCleanupRecovery.recover store catalog with
            | Error e ->
                RowStore.close store |> ignore
                err e
            | Ok(store, mgr) ->
                let lastSeq = RowStore.lastSeqNo store

                let manager =
                    { mgr with
                        LastCommitSeqNo = lastSeq
                        NextSeqNo = lastSeq + 1UL
                    }

                new Database(
                    root,
                    {
                        Root = root
                        Store = store
                        Catalog = catalog
                        Manager = manager
                        Active = None
                        Pending = None
                        ActiveRegistration = None
                    }
                )
                |> ret

    /// DB のルートディレクトリ。
    member _.Root = root

    /// 実行中の transaction があるか。
    member _.InTransaction = state.Active.IsSome

    /// テーブル一覧を返す。
    member _.TableNames() = state.Catalog.Tables |> List.map _.Name

    /// DB を閉じる（全 keyspace の memtable を flush し、WAL を削除する）。
    /// <remarks>
    /// ハンドルは不変なので「閉じた」状態をこのハンドルには記録しない。
    /// Close 後に同じハンドルで Execute すると WAL が再作成されるため、Close 後は破棄すること。
    /// 二重 Close は安全（2回目は何もしない）。
    /// 進行中の transaction は ActiveSnapshot の登録から外す
    /// （閉じたハンドルの transaction はもう読めないため、版を残す理由が無い）。
    /// Close し忘れても、登録は最後のハンドルが GC された時点で外れる（`RegistrationLease`）。
    /// </remarks>
    member _.Close() =
        lease |> Option.iter _.Release()

        match state.Store.Lock, storeLeaseToken with
        | Some lockLease, Some token when lockLease.IsOwner token -> RowStore.close state.Store
        | Some _, Some _ -> ret ()
        | _ -> RowStore.close state.Store

    interface System.IDisposable with
        member this.Dispose() = this.Close() |> ignore

    /// トランザクションを始める。
    /// 明示的な BEGIN。
    /// カタログの再読み込みに失敗したらエラーを返す（古いカタログで黙って続行しない）。
    member this.Begin() =
        if state.Active.IsSome then
            ret this
        else
            RootGate.withShared state.Root (fun () ->
                let mgr, txn = TransactionManager.beginTxn state.Manager
                let registration = ActiveSnapshotRegistry.register state.Root txn.Snapshot.SeqNo

                match Catalog.load state.Store with
                | Error e ->
                    ActiveSnapshotRegistry.unregister state.Root registration
                    err e
                | Ok fresh ->
                    new Database(
                        root,
                        { state with
                            Manager = mgr
                            Active = Some txn
                            Catalog = fresh
                            ActiveRegistration = Some registration
                        }
                    )
                    |> ret)

    /// COMMIT。
    /// バッファをエンジンへ流す。
    /// <remarks>
    /// コミットする transaction は「Active か Pending のうちバッファを持つ方」。
    /// Pending は DML が実行されるたびに更新されるので、複数文の DML を 1つの transaction にまとめられる。
    /// </remarks>
    member _.Commit() =
        match state.Pending |> Option.orElse state.Active with
        | None -> err (DbError.internalError "コミットする transaction がありません")
        | Some txn ->
            DbStateOps.commitTransaction txn state
            |> Result.map (fun nextState ->
                state.ActiveRegistration |> Option.iter (ActiveSnapshotRegistry.unregister root)
                new Database(root, nextState))

    /// ROLLBACK。
    /// バッファを捨てる。
    member _.Rollback() =
        match state.Active with
        | None -> err (DbError.internalError "ロールバックする transaction がありません")
        | Some txn ->
            let mgr = TransactionManager.abort txn state.Manager
            state.ActiveRegistration |> Option.iter (ActiveSnapshotRegistry.unregister root)

            new Database(
                root,
                { state with
                    Manager = mgr
                    Active = None
                    Pending = None
                    ActiveRegistration = None
                }
            )
            |> ret

    /// 単一文または複数文を実行する。
    /// <remarks>
    /// 戻り値の Database を次の呼び出しに渡すこと。
    /// ハンドルは不変なので、戻り値を捨てると DDL / DML の結果が見えない。
    /// 明示的な transaction が無い場合は暗黙の transactionで実行し commit する。
    /// 複数文(`;` 区切り)が渡された場合は順に実行する。
    /// </remarks>
    member this.Execute(sql: string) =
        let results = ResizeArray<StatementResult>()
        let registrations = ResizeArray<ActiveRegistration>()

        let rec runStatements st stmts =
            match stmts with
            | [] -> ret st
            | stmt :: tail ->
                match stmt with
                | Statement.Begin ->
                    match Engine.executeBegin st with
                    | Error e -> err e
                    | Ok nextState ->
                        if st.Active.IsNone then
                            nextState.ActiveRegistration |> Option.iter registrations.Add

                        results.Add StatementResult.Empty
                        runStatements nextState tail
                | Statement.Commit ->
                    match Engine.executeCommit st with
                    | Error e -> err e
                    | Ok nextState ->
                        results.Add StatementResult.Empty
                        runStatements nextState tail
                | Statement.Rollback ->
                    let nextState = Engine.executeRollback st
                    results.Add StatementResult.Empty
                    runStatements nextState tail
                | _ ->
                    match Engine.executeStatement st stmt with
                    | Error e -> err e
                    | Ok(nextState, result) ->
                        results.Add result
                        runStatements nextState tail

        let finishError e =
            registrations |> Seq.iter (ActiveSnapshotRegistry.unregister state.Root)
            err e

        result {
            let! stmts = Parser.parseStatements sql

            match runStatements state stmts with
            | Error e -> return! finishError e
            | Ok finalState ->
                let selects =
                    results |> Seq.filter (fun r -> not (Array.isEmpty r.Columns)) |> Seq.toList

                let summary =
                    match selects with
                    | [ s ] -> s
                    | _ ->
                        {
                            RowsAffected = results |> Seq.sumBy _.RowsAffected
                            Columns = [||]
                            Rows = [||]
                        }

                let isReadOnly =
                    stmts
                    |> List.forall (function
                        | Statement.Select _ -> true
                        | _ -> false)

                let next = if isReadOnly then this else new Database(root, finalState)
                return next, summary
        }
