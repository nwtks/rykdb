// ===========================================================================
// Engine — 文の実行層（SELECT / DML / DDL / transaction）。
// ===========================================================================

namespace RykDb.Core

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

    /// 推定の元になった統計が古いときの 1行（無ければ None）。
    /// `ANALYZE` 以降にコミットされた DML の行数を出す。
    let private staleStatsLine (state: ExecState) =
        let stale =
            state.Catalog.Stats
            |> List.filter (fun s -> s.Changes > 0L)
            |> List.sortBy (fun s -> s.Table.ToLowerInvariant())
            |> List.map (fun s -> sprintf "%s(+%d)" s.Table s.Changes)

        match stale with
        | [] -> None
        | _ -> Some(sprintf "Stats stale: %s" (String.concat ", " stale))

    /// EXPLAIN: 実行計画を組み立て、木を 1行 1ノードの文字列にして返す。
    /// クエリ本体は実行しない（ただしサブクエリは計画時に評価される）。
    let private runExplain (limits: ResultLimits) (state: ExecState) (txn: Transaction) (stmt: SelectStatement) =
        contextOf limits.Budget state txn
        >>= fun ctx ->
            try
                Plan.explainSelect ctx stmt
                >>= fun plan ->
                    let lines = Optimizer.explain plan.Context plan.Tree plan.Input

                    let lines =
                        staleStatsLine state
                        |> Option.map List.singleton
                        |> Option.defaultValue []
                        |> (@) lines

                    lines
                    |> List.map (fun line -> LoadedRow.create [| SqlValue.Text line |])
                    |> all
                    |> Result.map (fun loaded ->
                        state, StatementResult.Create(loaded.Length, [| "plan" |], List.toArray loaded))
            finally
                ctx.Scope.DisposeAll()

    /// 実測行数表からノードの行数を引く。
    let private actualCountOf (counts: System.Collections.Generic.Dictionary<obj, int64>) (tree: Logical) =
        match counts.TryGetValue(box tree) with
        | true, n -> Some n
        | false, _ -> None

    /// EXPLAIN ANALYZE: クエリを実行し、走査と結合の実測行数を推定に添える。
    /// 結果は materialize するが返さない（末尾に件数と経過時間の 1行を足す）。
    let private runExplainAnalyze (limits: ResultLimits) (state: ExecState) (txn: Transaction) (stmt: SelectStatement) =
        contextOf limits.Budget state txn
        >>= fun ctx ->
            try
                Plan.explainSelect ctx stmt
                >>= fun plan ->
                    let counts =
                        System.Collections.Generic.Dictionary<obj, int64>(HashIdentity.Reference)

                    let clock = System.Diagnostics.Stopwatch.StartNew()

                    let executed =
                        Optimizer.execWithCounts plan.Context counts plan.Tree plan.Input
                        >>= Op.toArrayLimited limits.MaxRows limits.MaxBytes

                    clock.Stop()

                    match executed with
                    | Error e -> Error e
                    | Ok rows ->
                        let lines =
                            Optimizer.explainWithCounts (actualCountOf counts) plan.Context plan.Tree plan.Input

                        let stale =
                            staleStatsLine state |> Option.map List.singleton |> Option.defaultValue []

                        let summary =
                            sprintf "Execution: rows=%d elapsed=%.3fms" rows.Length clock.Elapsed.TotalMilliseconds

                        let loaded =
                            (lines @ stale @ [ summary ])
                            |> List.map (fun line -> LoadedRow.create [| SqlValue.Text line |])

                        match all loaded with
                        | Error e -> Error e
                        | Ok loadedRows ->
                            Ok(state, StatementResult.Create(loadedRows.Length, [| "plan" |], Array.ofList loadedRows))
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
    let prepareSelectWith (strategy: JoinOrderStrategy) (state: ExecState) (stmt: SelectStatement) =
        let snapshot =
            {
                SeqNo = state.Manager.LastCommitSeqNo
            }

        Transaction.create 0UL<txnId> snapshot
        |> contextOf MemoryBudget.Default state
        |> Result.map (ExecContext.withJoinOrder strategy)
        >>= fun ctx ->
            match Plan.explainSelect ctx stmt with
            | Ok plan -> ret plan
            | Error e ->
                ctx.Scope.DisposeAll()
                err e

    let prepareSelect (state: ExecState) (stmt: SelectStatement) =
        prepareSelectWith JoinOrderStrategy.Dp state stmt

    /// ストリーミング SELECT を共有ロックの内側で開く。
    let private openSelectRun
        (budget: MemoryBudget)
        (state: ExecState)
        (startState: ExecState)
        (txn: Transaction)
        (registration: ActiveRegistration)
        (stmt: SelectStatement)
        =
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

            state.Store.Runtime.WithShared(fun () -> openSelectRun budget state startState txn registration stmt)

    let private resolveWhere (target: Schema) (ctx: ExecContext) =
        function
        | None -> ret None
        | Some e -> Plan.resolveDmlExprWith target ctx e |> Result.map Some

    /// 索引走査を開き、足りない列を主キーから取り補う。
    let private indexCandidateScan
        (ctx: ExecContext)
        (schemaName: string)
        (mask: bool array)
        (indexName: string)
        (build: bool array -> Result<Op, DbError>)
        =
        result {
            let! scanMask, fetchMask = ExecContext.indexAccessMasks ctx schemaName indexName mask

            let! op = build scanMask

            if fetchMask |> Array.exists id then
                return! ExecContext.fetchMissing ctx schemaName fetchMask op
            else
                return op
        }

    /// UPDATE / DELETE の候補行を、WHERE から選んだ走査方法で開く。
    /// WHERE は呼び出し側が行ごとに再評価するので、走査は候補の十分条件にだけ使う
    /// （範囲の閉区間など、走査が広めに返しても結果は変わらない）。
    let private dmlCandidateScan (ctx: ExecContext) (schema: TableSchema) (where: Expr option) (mask: bool array) =
        let method =
            match where with
            | None -> ScanMethod.SeqScan
            | Some w ->
                let binding: TableBinding =
                    {
                        Ref = { Name = schema.Name; Alias = None }
                        RealName = schema.Name
                        Schema = schema
                    }

                match Binder.qualify (Schema.ofTable schema.Name None schema) w with
                | Error _ -> ScanMethod.SeqScan
                | Ok qualified ->
                    match Analyzer.chooseScanMethod binding ctx.Catalog [ qualified ] with
                    | Ok method -> method
                    | Error _ -> ScanMethod.SeqScan

        match method with
        | ScanMethod.SeqScan ->
            Dml.scanByPrimaryKeyWithTxnMask ctx.Store schema 0 ctx.Visible (Some ctx.Scope) ctx.Txn mask
        | ScanMethod.PkLookup values ->
            result {
                let! row =
                    Dml.getByPrimaryKeyWithTxnMask ctx.Store schema values ctx.Visible ctx.Txn mask

                match row with
                | Some r -> return Op.ofRow r
                | None -> return Op.Empty
            }
        | ScanMethod.RangeScan(_, startKey, endKey) ->
            Dml.scanByPrimaryKeyRangeWithTxnMask
                ctx.Store
                schema
                startKey
                endKey
                0
                ctx.Visible
                (Some ctx.Scope)
                ctx.Txn
                mask
        | ScanMethod.IndexLookup(indexName, values) ->
            indexCandidateScan ctx schema.Name mask indexName (fun scanMask ->
                ExecContext.byIndexMasked ctx schema.Name indexName values 0 scanMask)
        | ScanMethod.IndexRangeScan(indexName, lower, upper) ->
            indexCandidateScan ctx schema.Name mask indexName (fun scanMask ->
                ExecContext.byIndexRangeMasked ctx schema.Name indexName lower upper 0 scanMask)

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
            return! DmlOps.updateWith (dmlCandidateScan ctx schema whereE) ctx schema assignments whereE
        }

    let private runDelete (stmt: DeleteStatement) (ctx: ExecContext) =
        result {
            let! schema = ExecContext.schemaOf stmt.Table.Name ctx
            let target = Schema.ofTable schema.Name None schema
            let! whereE = resolveWhere target ctx stmt.Where
            return! DmlOps.deleteWith (dmlCandidateScan ctx schema whereE) ctx schema whereE
        }

    let private runDml
        (limits: ResultLimits)
        (state: ExecState)
        (txn: Transaction)
        (table: string)
        (run: ExecContext -> Result<(Transaction * int), DbError>)
        =
        contextOf limits.Budget state txn
        >>= fun ctx ->
            try
                result {
                    let! outcome = run ctx
                    let txn2, n = outcome

                    let next =
                        { state with
                            Txn = { state.Txn with Pending = Some txn2 }
                        }

                    return ExecState.addStatsDelta table n next, StatementResult.Affected n
                }
            finally
                ctx.Scope.DisposeAll()

    let private runDdlWithState (state: ExecState) (run: ExecState -> Result<ExecState * StatementResult, DbError>) =
        if state.Active.IsSome then
            err (
                DbError.create
                    ErrorKind.SyntaxError
                    "トランザクション中は DDL（CREATE / DROP / ALTER）を実行できません。COMMIT または ROLLBACK してから実行してください"
            )
        else
            run state

    let private runDdl (state: ExecState) (run: ExecState -> Result<ExecState, DbError>) =
        runDdlWithState state (run >> Result.map (fun next -> next, StatementResult.Empty))

    let private noRows = Result.map (fun s -> s, StatementResult.Empty)

    /// 列の数値範囲を表示用文字列にする。
    let private columnRangeText (c: ColumnStats) =
        match c.Min, c.Max with
        | Some lo, Some hi -> sprintf "%s=[%g,%g]" c.Name lo hi
        | _ -> ""

    /// ANALYZE の結果（見出しと 1テーブル 1行）を作る。
    let private analyzeResultRows (stats: TableStats list) =
        let columns =
            [|
                "table"
                "rows"
                "columns"
                "ndv"
                "nulls"
                "ranges"
                "indexes"
                "histograms"
                "mcv"
                "row_bytes"
                "analyzed_at"
            |]

        let rows =
            stats
            |> List.map (fun (s: TableStats) ->
                [|
                    SqlValue.Text s.Table
                    SqlValue.Int s.RowCount
                    SqlValue.Int(int64 s.Columns.Length)
                    SqlValue.Text(
                        s.Columns
                        |> List.map (fun c -> sprintf "%s=%d" c.Name c.NDV)
                        |> String.concat ","
                    )
                    SqlValue.Text(
                        s.Columns
                        |> List.map (fun c -> sprintf "%s=%d" c.Name c.Nulls)
                        |> String.concat ","
                    )
                    SqlValue.Text(
                        s.Columns
                        |> List.map columnRangeText
                        |> List.filter ((<>) "")
                        |> String.concat ","
                    )
                    SqlValue.Text(
                        s.Indexes
                        |> List.map (fun i -> sprintf "%s=%A" i.Name i.PrefixNDV)
                        |> String.concat ","
                    )
                    SqlValue.Text(
                        s.Columns
                        |> List.filter (fun c -> not (List.isEmpty c.Histogram))
                        |> List.map (fun c -> sprintf "%s:%d" c.Name (List.length c.Histogram))
                        |> String.concat ","
                    )
                    SqlValue.Text(
                        s.Columns
                        |> List.filter (fun c -> not (List.isEmpty c.Mcv))
                        |> List.map (fun c -> sprintf "%s:%d" c.Name (List.length c.Mcv))
                        |> String.concat ","
                    )
                    SqlValue.Int(int64 (System.Math.Round s.RowBytes))
                    SqlValue.Text(
                        match s.AnalyzedAt with
                        | Some t -> t.UtcDateTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)
                        | None -> ""
                    )
                |])
            |> Array.ofList

        StatementResult.OfRows(0, columns, rows)

    let private runStatement (limits: ResultLimits) (state: ExecState) (txn: Transaction) =
        function
        | Statement.Select stmt -> runSelect limits state txn stmt
        | Statement.Explain stmt -> runExplain limits state txn stmt
        | Statement.ExplainAnalyze stmt -> runExplainAnalyze limits state txn stmt
        | Statement.Insert stmt -> runDml limits state txn stmt.Table.Name (runInsert stmt)
        | Statement.Update stmt -> runDml limits state txn stmt.Table.Name (runUpdate stmt)
        | Statement.Delete stmt -> runDml limits state txn stmt.Table.Name (runDelete stmt)
        | Statement.CreateTable c -> EngineDdl.createTable c |> runDdl state
        | Statement.DropTable(n, ifE) -> EngineDdl.dropTable n ifE |> runDdl state
        | Statement.CreateIndex ci -> EngineDdl.createIndex ci |> runDdl state
        | Statement.DropIndex(n, ifE) -> EngineDdl.dropIndex n ifE |> runDdl state
        | Statement.AlterTable a -> EngineDdl.alterTable a |> runDdl state
        | Statement.Analyze(name, sample) ->
            runDdlWithState state (fun st ->
                EngineAnalyze.analyze (name, sample) limits.Cancel limits.Progress st
                >>= fun (next, stats) -> analyzeResultRows stats |> Result.map (fun result -> next, result))
        | Statement.Begin
        | Statement.Commit
        | Statement.Rollback -> ret state |> noRows

    /// 1文を実行し、読み取り専用なら状態を戻し、それ以外は commit する。
    let private executeStatementRun
        (limits: ResultLimits)
        (state: ExecState)
        (txn: Transaction)
        (stmt: Statement)
        (mgr: TransactionManager)
        (registration: ActiveRegistration)
        (isReadOnly: bool)
        =
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

    /// 文の終了時に snapshot 登録を外す。
    let private unregisterAfter (state: ExecState) (registration: ActiveRegistration) (r: 'a) =
        state.Store.Runtime.Unregister registration
        r

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
                | Statement.ExplainAnalyze _ -> true
                | _ -> false

            let isDdl =
                match stmt with
                | Statement.CreateTable _
                | Statement.DropTable _
                | Statement.CreateIndex _
                | Statement.DropIndex _
                | Statement.AlterTable _
                | Statement.Analyze _ -> true
                | _ -> false

            let run =
                fun () -> executeStatementRun limits state txn stmt mgr registration isReadOnly

            (if isDdl then run () else state.Store.Runtime.WithShared run)
            |> unregisterAfter state registration

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
