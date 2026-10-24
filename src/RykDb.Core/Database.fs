// ===========================================================================
// Database — 公開エントリポイント。
// ライブラリ利用者・CLI・TCP サーバはこの層だけを見る。
//
//   let db = Database.openWith root
//   let db, r = db.Execute "INSERT ..."
//   let db, rs = db.Query "SELECT ..."
//   db.Begin() / db.Commit() / db.Rollback() / db.Close()
//
// * Execute / Query は暗黙の transaction で実行し自動 commit。
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
    /// DB のルートディレクトリ。
    member _.Root = root

    /// DB を閉じる（全 keyspace の memtable を flush し、WAL を削除する）。
    /// <remarks>
    /// ハンドルは不変なので「閉じた」状態をこのハンドルには記録しない。
    /// Close 後に同じハンドルで Execute すると WAL が再作成されるため、Close 後は破棄すること。
    /// 二重 Close は安全（2 回目は何もしない）。
    /// </remarks>
    member _.Close() = RowStore.close state.Store

    /// トランザクションを始める。
    /// 明示的な BEGIN。
    /// カタログの再読み込みに失敗したらエラーを返す（古いカタログで黙って続行しない）。
    member this.Begin() =
        if state.Active.IsSome then
            ret this
        else
            let mgr, txn = TransactionManager.beginTxn state.Manager

            Catalog.load state.Store
            |> Result.map (fun fresh ->
                let nextState =
                    { state with
                        Manager = mgr
                        Active = Some txn
                        Catalog = fresh
                    }

                Database(root, nextState))

    /// 実行中の transaction があるか。
    member _.InTransaction = state.Active.IsSome

    /// COMMIT。
    /// バッファをエンジンへ流す。
    /// <remarks>
    /// コミットする transaction は「Active か Pending のうちバッファを持つ方」。
    /// Pending は DML が実行されるたびに更新されるので、複数文の DML を 1つの transaction にまとめられる。
    /// </remarks>
    member _.Commit() =
        let target =
            match state.Pending with
            | Some p -> Some p
            | None -> state.Active

        match target with
        | None -> err (DbError.internalError "コミットする transaction がありません")
        | Some txn ->
            DbStateOps.commitTransaction state txn
            |> Result.map (fun nextState -> Database(root, nextState))

    /// ROLLBACK。
    /// バッファを捨てる。
    member _.Rollback() =
        match state.Active with
        | None -> err (DbError.internalError "ロールバックする transaction がありません")
        | Some txn ->
            let mgr = TransactionManager.abort state.Manager txn.TxnId

            let nextState =
                { state with
                    Manager = mgr
                    Active = None
                    Pending = None
                }

            Ok(Database(root, nextState))

    /// 単一文または複数文を実行する。
    /// <remarks>
    /// 戻り値の Database を次の呼び出しに渡すこと。
    /// ハンドルは不変なので、戻り値を捨てると DDL / DML の結果が見えない。
    /// 明示的な transaction が無い場合は暗黙の transactionで実行し commit する。
    /// 複数文(`;` 区切り)が渡された場合は順に実行する。
    /// </remarks>
    member _.Execute(sql: string) =
        let results = ResizeArray<StatementResult>()

        let rec runStatements (st: DbState) (rest: Statement list) =
            match rest with
            | [] -> ret st
            | stmt :: tail ->
                match stmt with
                | Statement.Begin ->
                    if st.Active.IsSome then
                        results.Add StatementResult.empty
                        runStatements st tail
                    else
                        let mgr, txn = TransactionManager.beginTxn st.Manager

                        match Catalog.load st.Store with
                        | Error e -> Error e
                        | Ok fresh ->
                            let nextState =
                                { st with
                                    Manager = mgr
                                    Active = Some txn
                                    Catalog = fresh
                                }

                            results.Add StatementResult.empty
                            runStatements nextState tail
                | Statement.Commit ->
                    match st.Active with
                    | None ->
                        results.Add StatementResult.empty
                        runStatements st tail
                    | Some _ ->
                        let target =
                            match st.Pending with
                            | Some p -> Some p
                            | None -> st.Active

                        match target with
                        | None ->
                            results.Add StatementResult.empty
                            runStatements st tail
                        | Some txn ->
                            match DbStateOps.commitTransaction st txn with
                            | Error e -> Error e
                            | Ok nextState ->
                                results.Add StatementResult.empty
                                runStatements nextState tail
                | Statement.Rollback ->
                    match st.Active with
                    | None ->
                        results.Add StatementResult.empty
                        runStatements st tail
                    | Some txn ->
                        let mgr = TransactionManager.abort st.Manager txn.TxnId

                        let nextState =
                            { st with
                                Manager = mgr
                                Active = None
                                Pending = None
                            }

                        results.Add StatementResult.empty
                        runStatements nextState tail
                | _ ->
                    match st.Active with
                    | Some activeTxn ->
                        let running =
                            match st.Pending with
                            | Some p -> p
                            | None -> activeTxn

                        match Engine.runStatement st running stmt with
                        | Error e -> Error e
                        | Ok(nextState, result) ->
                            results.Add result

                            let carried =
                                match stmt with
                                | Statement.Insert _
                                | Statement.Update _
                                | Statement.Delete _ -> nextState
                                | _ -> { nextState with Pending = st.Pending }

                            runStatements carried tail
                    | None ->
                        let mgr, txn = TransactionManager.beginTxn st.Manager

                        let needsReload =
                            match stmt with
                            | Statement.CreateTable _
                            | Statement.DropTable _
                            | Statement.CreateIndex _
                            | Statement.DropIndex _ -> true
                            | _ -> false

                        let catalogResult = if needsReload then Catalog.load st.Store else Ok st.Catalog

                        match catalogResult with
                        | Error e -> Error e
                        | Ok fresh ->
                            let startState =
                                { st with
                                    Manager = mgr
                                    Catalog = fresh
                                }

                            match Engine.runStatement startState txn stmt with
                            | Error e -> Error e
                            | Ok(midState, result) ->
                                let toCommit =
                                    match midState.Pending with
                                    | Some p -> p
                                    | None -> txn

                                match DbStateOps.commitTransaction midState toCommit with
                                | Error e -> Error e
                                | Ok nextState ->
                                    results.Add result
                                    runStatements nextState tail

        result {
            let! stmts = Parser.parseStatements sql
            let! finalState = runStatements state stmts

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

            return Database(root, finalState), summary
        }

    /// SELECT 専用の実行。
    /// 戻り値の Database を次の呼び出しに渡すこと。
    /// DDL / DML / BEGIN / COMMIT / ROLLBACK は `Execute` を使う。
    member this.Query(sql: string) =
        match Parser.parseStatements sql with
        | Error e -> Error e
        | Ok statements ->
            if
                statements
                |> List.forall (fun stmt ->
                    match stmt with
                    | Statement.Select _ -> true
                    | _ -> false)
            then
                this.Execute sql
            else
                err (
                    DbError.create
                        ErrorKind.SyntaxError
                        "Query は SELECT 専用です（DDL / DML / BEGIN / COMMIT は Execute を使ってください）"
                )

    /// テーブル一覧を返す。
    member _.TableNames() = state.Catalog.Tables |> List.map _.Name

    /// 与えた RowStore で DB を開く。
    static member internal openWithStore (root: string) (store: RowStore) =
        result {
            let! catalog = Catalog.load store
            let lastSeq = RowStore.lastSeqNo store

            let manager =
                let fresh = TransactionManager.create ()

                { fresh with
                    LastCommitSeqNo = lastSeq
                    NextSeqNo = lastSeq + 1UL
                }

            return
                Database(
                    root,
                    {
                        Root = root
                        Store = store
                        Catalog = catalog
                        Manager = manager
                        Active = None
                        Pending = None
                    }
                )
        }

    /// エンジンの開き方を差し替えて DB を開く（テスト用）。
    static member internal openWithEngine (root: string) (openEngine: string -> Result<IStorageEngine, DbError>) =
        Database.openWithStore root (RowStore.openStoreWith root openEngine)

    /// DB を開く。
    static member openWith(root: string) =
        result {
            let! store = RowStore.openStore root
            return! Database.openWithStore root store
        }
