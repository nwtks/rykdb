// ===========================================================================
// Database — 内部ハンドル。
//
//   let db = Database.OpenWith root
//   let db, r = db.Execute stmt
//   db.Begin() / db.Commit() / db.Rollback() / db.Close()
//
// * Execute は 1文を実行し、暗黙の transaction なら自動 commit。
//   Begin 以降は Commit / Rollback まで同じ transaction。
//   snapshot は transaction 開始時に固定(repeatable read)。
// * DDL は即時反映（カタログを書き換えた時点で見える）。
// ===========================================================================

namespace RykDb.Core

/// DB の不変ハンドル。
/// <remarks>
/// ハンドルは underlying state を不変に持つ。
/// 書き込みは新しい state を返すので、ハンドル自体は変更されない。
/// 戻り値を捨てると状態が古くなるため、この型は公開しない。
/// 公開 API の `Session` が常に最新ハンドルを保持する。
/// </remarks>
type internal Database(root: string, state: DbState, ownsStore: bool) =
    /// このハンドルの生存に紐づく ActiveSnapshot 登録のリース。
    /// ハンドルが commit / rollback / Close されずに捨てられても、
    /// 最後の 1本が GC された時点で登録が外れるよう保持しておく
    /// （放棄された transaction で DDL が永久に ConflictError にならないようにする）。
    let lease = state.ActiveRegistration |> Option.map RegistrationLease

    /// このハンドルがプロセスロックの所有者か判定するためのトークン。
    /// `Execute` / `Begin` / `Commit` は新しいハンドルを返すので、
    /// 新しいハンドルを作るたびに所有者を置き換える。
    /// 古いハンドルの Close で、生きている新しいハンドルのロックを外さないため。
    /// `ownsStore = false` のハンドルは **所有者にならない**。
    /// ただし現在の生成箇所はすべて `true` で、
    /// 接続ごとの共有は`Session` の `ownsHost = false` で行う
    /// （所有者を奪うと、最後に Close すべきホストが no-op になり DB が閉じられなくなる）。
    let storeLeaseToken =
        if ownsStore then
            state.Store.Lock |> Option.map (fun lockLease -> lockLease.Attach())
        else
            None

    /// background compaction を有効にして開く。
    /// <remarks>
    /// 同じ root はプロセス内で 1つしか開けない。2回目は `IoError` を返す
    /// （別の `RowStore` ができて memtable が分かれ、commit が見えないため）。
    /// 別プロセスが開いている場合も `IoError`。
    /// </remarks>
    static member OpenWithBackground(root: string) =
        result {
            let! store = RowStore.openStoreWithBackground root
            return! Database.OpenWithStore root store
        }

    /// background compaction を無効にして開く。
    static member internal OpenWith(root: string) =
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

                RowStore.startBackground store

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
                        DeferDurability = false
                    },
                    true
                )
                |> ret

    /// DB のルートディレクトリ。
    member internal _.Root = root

    /// 実行中の transaction があるか。
    member internal _.InTransaction = state.Active.IsSome

    /// WAL の fsync を `Session` のグループコミットに任せる状態を返す。
    /// ハンドルを作らないので ProcessLock の所有者は変わらない。
    member _.StateWithDeferredDurability = { state with DeferDurability = true }

    /// DB ごとの RootGate / active snapshot 登録（テスト・診断用）。
    member internal _.Runtime = state.Store.Runtime

    /// 状態だけ差し替えたハンドルを返す。ストアの所有権は引き継ぐ。
    member _.WithState(next: DbState) = new Database(root, next, ownsStore)

    /// テーブル一覧を返す。
    member internal _.TableNames() = state.Catalog.Tables |> List.map _.Name

    /// DB を閉じる（memtable を flush し、WAL を削除する）。
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

    /// SELECT の計画だけを組み立てる（実行しない。ベンチ・診断用）。
    /// 返した `PlannedQuery` は `Optimizer.exec` で繰り返し実行できる。
    member internal _.PrepareSelect(stmt: SelectStatement) = Engine.prepareSelect state stmt

    /// トランザクションを始める。
    /// 明示的な BEGIN。
    /// カタログの再読み込みに失敗したらエラーを返す（古いカタログで黙って続行しない）。
    member internal this.Begin() =
        if state.Active.IsSome then
            ret this
        else
            state.Store.Runtime.WithShared(fun () ->
                let mgr, txn = TransactionManager.beginTxn state.Manager
                let registration = state.Store.Runtime.Register(state.Root, txn.Snapshot.SeqNo)

                match Catalog.load state.Store with
                | Error e ->
                    state.Store.Runtime.Unregister registration
                    err e
                | Ok fresh ->
                    new Database(
                        root,
                        { state with
                            Manager = mgr
                            Active = Some txn
                            Catalog = fresh
                            ActiveRegistration = Some registration
                        },
                        ownsStore
                    )
                    |> ret)

    /// COMMIT。
    /// バッファをエンジンへ流す。
    /// <remarks>
    /// コミットする transaction は「Active か Pending のうちバッファを持つ方」。
    /// Pending は DML が実行されるたびに更新されるので、複数文の DML を 1つの transaction にまとめられる。
    /// </remarks>
    member internal _.Commit() =
        match state.Pending |> Option.orElse state.Active with
        | None -> err (DbError.internalError "コミットする transaction がありません")
        | Some txn ->
            DbStateOps.commitTransaction txn state
            |> Result.map (fun nextState ->
                state.ActiveRegistration
                |> Option.iter (fun registration -> state.Store.Runtime.Unregister registration)

                new Database(root, nextState, ownsStore))

    /// ROLLBACK。
    /// バッファを捨てる。
    member internal _.Rollback() =
        match state.Active with
        | None -> err (DbError.internalError "ロールバックする transaction がありません")
        | Some txn ->
            let mgr = TransactionManager.abort txn state.Manager

            state.ActiveRegistration
            |> Option.iter (fun registration -> state.Store.Runtime.Unregister registration)

            new Database(
                root,
                { state with
                    Manager = mgr
                    Active = None
                    Pending = None
                    ActiveRegistration = None
                },
                ownsStore
            )
            |> ret

    /// 1文だけ実行して、次のハンドルと結果を返す。
    /// <remarks>
    /// 戻り値の Database を次の呼び出しに渡すこと。
    /// ハンドルは不変なので、戻り値を捨てると DDL / DML の結果が見えない。
    /// 明示的な transaction が無い場合は暗黙の transaction で実行し commit する。
    /// 複数文を実行する場合は、公開 API の `Session.Execute` を使う。
    /// </remarks>
    member internal this.Execute(stmt: Statement) =
        this.ExecuteWithLimits(ResultLimits.Unlimited, stmt)

    /// 1文だけ実行し、SELECT 結果には limits を適用する。
    member private this.ExecuteWithLimits(limits: ResultLimits, stmt: Statement) =
        let nextOf nextState =
            if obj.ReferenceEquals(nextState, state) then
                this
            else
                new Database(root, nextState, ownsStore)

        match stmt with
        | Statement.Begin ->
            Engine.executeBegin state
            |> Result.map (fun nextState -> nextOf nextState, StatementResult.Empty)
        | Statement.Commit ->
            Engine.executeCommit state
            |> Result.map (fun nextState -> nextOf nextState, StatementResult.Empty)
        | Statement.Rollback ->
            let nextState = Engine.executeRollback state
            ret (nextOf nextState, StatementResult.Empty)
        | _ ->
            Engine.executeStatementWithLimits limits state stmt
            |> Result.map (fun (nextState, result) -> nextOf nextState, result)
