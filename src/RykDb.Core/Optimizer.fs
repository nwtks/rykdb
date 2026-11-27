// ===========================================================================
// Optimizer — 論理代数を pull 演算子に変換して実行する（`exec` / `explain`）。
// ===========================================================================

namespace RykDb.Core

module internal Optimizer =
    /// ハッシュ結合の build 側。
    /// 左が小さいときは左を build して右を 1回だけ流す。
    type private HashBuildSide =
        | HashBuildRight
        | HashBuildLeft

    /// ハッシュ結合の build 側を推定行数で決める。
    /// LEFT JOIN は左行を保存する必要があるため常に右を build する。
    let private hashBuildSide
        (statsOf: string -> TableStats option)
        (algorithm: JoinAlgorithm)
        (kind: JoinKind)
        (left: Logical)
        (right: Logical)
        =
        match algorithm, kind with
        | JoinAlgorithm.HashJoin, JoinKind.Inner ->
            if Analyzer.estimateRowsWith statsOf left < Analyzer.estimateRowsWith statsOf right then
                HashBuildLeft
            else
                HashBuildRight
        | _ -> HashBuildRight

    /// EXPLAIN ANALYZE 用に、ノードの出力行数を数える。
    /// 計測しない設定（`RowCounts = None`）では Op をそのまま返す。
    let private countOutput (ctx: ExecContext) (node: Logical) (op: Op) =
        match ctx.RowCounts with
        | None -> op
        | Some counts ->
            let key = box node

            Op.map
                (fun row ->
                    let current =
                        match counts.TryGetValue key with
                        | true, n -> n
                        | false, _ -> 0L

                    counts.[key] <- current + 1L
                    DbResult.DbOk row)
                op

    /// Join の結合待ち情報。
    /// 右部分木の Op は左部分木を評価した後に作る
    /// （評価順を left → right → join に保つため）。
    type private JoinFrame =
        {
            /// この結合の `Logical` ノード（EXPLAIN ANALYZE の実測行数のキー）。
            Node: Logical
            Kind: JoinKind
            Right: Logical
            On: Expr
            /// 右テーブルを除いたスキーマ（左部分木へ下ろすもの）
            LeftSchema: Schema
            RightSchema: Schema
            /// 右テーブルの参照名（別名含む）
            RightName: string
            Algorithm: JoinAlgorithm
            Keys: JoinKeys option
            /// インデックス列順の左キー位置
            LeftKeyIndices: int array
            /// ハッシュ結合の build 側
            BuildSide: HashBuildSide
        }

    /// 評価待ちの処理。
    /// Result を消費してから次へ進む処理をクロージャではなく値として積む。
    type private ExecFrame =
        | FilterFrame of Expr * Schema
        | AggregateFrame of Expr list * (AggFunc * bool * bool * Expr) list * Schema
        | HavingFrame of Expr * (AggFunc * bool * bool * Expr) list * Expr list
        | DistinctFrame
        | SortFrame of SortKey list * Schema * Expr list * (AggFunc * bool * bool * Expr) list
        | LimitFrame of int * int
        /// 射影。第2・3要素は `*` の展開用（論理 = FROM 順）と評価用（実際の行レイアウト）のスキーマ。
        | ProjectFrame of SelectItem list * Schema * Schema * Expr list
        /// 派生テーブルの内側を評価し終えた。外側の実行文脈へ戻す。
        | DerivedFrame of ExecContext
        /// 左部分木を評価し終えた Join。次に右部分木を評価する。
        | JoinRightFrame of JoinFrame
        /// 左部分木を評価し終えた Index Nested Loop。右部分木は評価しない。
        | JoinIndexLeftFrame of JoinFrame
        /// 右部分木も評価し終えた Join。左 Op を保持している。
        | JoinApplyFrame of JoinFrame * Op
        /// UNION ALL の枝を評価中（外側の文脈・完了した枝の Op（逆順）・残りの枝）。
        | UnionFrame of ExecContext * Op list * PlannedQuery list

    /// 評価機の状態。
    /// 木を 1つ降りるか、Op が上がってきたか。
    type private ExecStep =
        /// 第 3要素はストレージへ降ろせる件数ヒント（Some 0 は「0行」）。
        /// 第 4要素は索引 fetch の前に評価する述語（被覆列だけを参照）。
        | EvalNode of Logical * Schema * int option * Expr option
        | ApplyOp of Op

    /// テーブルスキーマの既定値（未登録なら空）。
    let private defaultSchema (ctx: ExecContext) (name: string) =
        match ExecContext.schemaOf name ctx with
        | Ok s -> s
        | Error _ ->
            {
                Name = name
                Columns = []
                PrimaryKey = []
                Indexes = []
            }

    /// テーブル名から schema を引く。
    let private tableOf (ctx: ExecContext) (name: string) =
        match ExecContext.schemaOf name ctx with
        | Ok s -> Some s
        | Error _ -> None

    /// 実行に影響しないラッパを剥がす。
    [<TailCall>]
    let rec private stripWrappers =
        function
        | Logical.Sort(_, child) -> stripWrappers child
        | Logical.Limit(_, _, child) -> stripWrappers child
        | Logical.Distinct child -> stripWrappers child
        | Logical.Having(_, _, _, child) -> stripWrappers child
        | Logical.Filter(_, child) -> stripWrappers child
        | Logical.Project(_, child) -> stripWrappers child
        | other -> other

    /// 集約結果の出力列を作る。
    let private aggregateColumn name =
        {
            Name = name
            Qualifier = None
            DataType = None
        }

    /// 集約の出力スキーマを作る。
    let private aggregateSchema (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) =
        let groupColumns = groupBy |> List.map (Expr.toDisplayString >> aggregateColumn)

        let aggColumns =
            aggs
            |> List.map (fun (f, star, _, arg) ->
                let suffix = if star then "*" else Expr.toDisplayString arg
                aggregateColumn (sprintf "%s(%s)" f.Name suffix))

        { Schema.Empty with
            Columns = Array.append (List.toArray groupColumns) (List.toArray aggColumns)
        }

    /// 結合を含まないノードの出力スキーマ。
    let private baseSchemaOf (ctx: ExecContext) (inSchema: Schema) =
        function
        | Logical.Aggregate(groupBy, aggs, _) -> aggregateSchema groupBy aggs
        | Logical.Derived plan -> plan.Output
        | Logical.UnionAll(plan :: _) -> plan.Output
        | Logical.Scan(name, _) -> Schema.ofTable name None (defaultSchema ctx name)
        | Logical.Values _ -> Schema.Empty
        | _ -> inSchema

    /// 出力スキーマを求める作業項目。
    /// 右部分木のスキーマは後順でしか決まらないが、
    /// 作業リストへ積んでおけば木の形に依存せず左から順に連結できる。
    type private SchemaWork =
        /// 木の出力スキーマを求めて連結する。
        | SchemaOf of Logical
        /// 右部分木のスキーマを求めて連結する（結合の部分木なら中を左からたどり直す）。
        | RightSchemaOf of string * Logical

    /// 結合木を左から深さ優先でたどり、各ノードのスキーマを連結する。
    /// 右部分木（bushy）は作業リストに積むので、木の深さに依存せずスタックは一定。
    [<TailCall>]
    let rec private outputSchema (ctx: ExecContext) (inSchema: Schema) (acc: Schema) (work: SchemaWork list) =
        match work with
        | [] -> acc
        | SchemaOf tree :: rest ->
            match stripWrappers tree with
            | Logical.Join(_, name, _, left, right) ->
                outputSchema ctx inSchema acc (SchemaOf left :: RightSchemaOf(name, right) :: rest)
            | leaf -> outputSchema ctx inSchema (Schema.concat acc (baseSchemaOf ctx inSchema leaf)) rest
        | RightSchemaOf(name, right) :: rest ->
            match stripWrappers right with
            | Logical.Derived plan -> outputSchema ctx inSchema (Schema.concat acc plan.Output) rest
            | Logical.Join _ as subtree -> outputSchema ctx inSchema acc (SchemaOf subtree :: rest)
            | _ ->
                outputSchema ctx inSchema (Schema.concat acc (Schema.ofTable name None (defaultSchema ctx name))) rest

    /// 論理木の出力スキーマを求める。
    let private outputOf (ctx: ExecContext) (inSchema: Schema) (tree: Logical) =
        outputSchema ctx inSchema Schema.Empty [ SchemaOf tree ]

    /// 出力スキーマのメモ。実行・EXPLAIN の 1回分だけ使う。
    /// `Logical` は参照等価で、ノード参照は木の中で一意なので文脈はキーに含めない
    /// （同じノードは常に同じ実行文脈からだけ問い合わされる）。
    type private OutputSchemaMemo() =
        let cache =
            System.Collections.Generic.Dictionary<obj, Schema> HashIdentity.Reference

        member _.TryFind(tree: Logical) =
            match cache.TryGetValue(box tree) with
            | true, schema -> Some schema
            | false, _ -> None

        member _.Add(tree: Logical, schema: Schema) = cache.[box tree] <- schema

    /// 出力スキーマを求める作業項目。
    type private OutputJob =
        | NeedNode of Logical
        | PutWrapper of Logical * Logical
        | PutJoin of Logical * Logical * Logical

    /// NeedNode 作業項目を処理する（子ノードを積むか、スキーマを確定する）。
    let private processNeedNode
        (ctx: ExecContext)
        (inSchema: Schema)
        (memo: OutputSchemaMemo)
        (node: Logical)
        (rest: OutputJob list)
        =
        match memo.TryFind node with
        | Some _ -> rest
        | None ->
            match node with
            | Logical.Scan _
            | Logical.Values _
            | Logical.Aggregate _
            | Logical.Derived _
            | Logical.UnionAll _ ->
                memo.Add(node, baseSchemaOf ctx inSchema node)
                rest
            | Logical.Filter(_, child)
            | Logical.Sort(_, child)
            | Logical.Limit(_, _, child)
            | Logical.Distinct child
            | Logical.Having(_, _, _, child)
            | Logical.Project(_, child) -> NeedNode child :: PutWrapper(node, child) :: rest
            | Logical.Join(_, _, _, left, right) ->
                NeedNode left :: NeedNode right :: PutJoin(node, left, right) :: rest

    /// 作業項目 1つを処理し、残りの作業リストを返す。
    let private processOutputJob (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (rest: OutputJob list) =
        function
        | NeedNode node -> processNeedNode ctx inSchema memo node rest
        | PutWrapper(parent, child) ->
            match memo.TryFind child with
            | Some schema -> memo.Add(parent, schema)
            | None -> ()

            rest
        | PutJoin(parent, left, right) ->
            match memo.TryFind left, memo.TryFind right with
            | Some leftSchema, Some rightSchema -> memo.Add(parent, Schema.concat leftSchema rightSchema)
            | _ -> ()

            rest

    /// 出力スキーマの作業項目を末尾再帰で処理する。
    [<TailCall>]
    let rec private fillOutputSchemasLoop (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) =
        function
        | [] -> ()
        | job :: rest -> fillOutputSchemasLoop ctx inSchema memo (processOutputJob ctx inSchema memo rest job)

    /// 部分木の全ノードの出力スキーマを後順で求め、メモに入れる。
    /// 明示スタックなので木の形に依存せずスタックは一定。
    let private fillOutputSchemas (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (root: Logical) =
        fillOutputSchemasLoop ctx inSchema memo [ NeedNode root ]

    /// 出力スキーマをメモから引く（未計算なら部分木を 1パスで計算してから引く）。
    let private outputOfMemo (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (tree: Logical) =
        match memo.TryFind tree with
        | Some schema -> schema
        | None ->
            fillOutputSchemas ctx inSchema memo tree

            match memo.TryFind tree with
            | Some schema -> schema
            | None -> outputOf ctx inSchema tree

    /// スキーマの全列位置を返す。
    let private scanAllColumns (schema: Schema) =
        if schema.Columns.Length = 0 then
            Set.empty
        else
            Set.ofSeq [ 0 .. schema.Columns.Length - 1 ]

    /// 列参照を位置へ解決する（修飾子なしも試す）。
    let private scanResolveRef (schema: Schema) (r: ColumnRef) =
        match Schema.resolve r schema with
        | Ok i -> Some i
        | Error _ ->
            match Schema.resolve { Qualifier = None; Name = r.Name } schema with
            | Ok i -> Some i
            | Error _ -> None

    /// 式が参照する列位置の集合を返す。
    let private scanRefsOf (schema: Schema) (e: Expr) =
        if Expr.hasAnySubquery e then
            scanAllColumns schema
        else
            let refs =
                Expr.fold
                    (fun acc x ->
                        match x with
                        | Expr.Column c -> c :: acc
                        | Expr.Name n -> { Qualifier = None; Name = n } :: acc
                        | _ -> acc)
                    []
                    e

            let resolved = refs |> List.map (scanResolveRef schema)

            if List.exists Option.isNone resolved then
                scanAllColumns schema
            else
                resolved |> List.choose id |> Set.ofList

    /// 複数の式が参照する列位置を合わせる。
    let private scanRefsOfAll (schema: Schema) (exprs: Expr list) =
        exprs |> List.fold (fun acc e -> Set.union acc (scanRefsOf schema e)) Set.empty

    /// 必要列の後方伝播の作業項目。
    /// ノードと、そのノードを実行するときの文脈・入力スキーマ・必要な列位置を持つ。
    type private ScanNeedsJob =
        {
            Context: ExecContext
            Input: Schema
            Needed: Set<int>
            Tree: Logical
        }

    /// 子ノードの作業項目を作る（文脈と入力スキーマを引き継ぐ）。
    let private scanNeedsChild (item: ScanNeedsJob) (needed: Set<int>) (child: Logical) =
        {
            Context = item.Context
            Input = item.Input
            Needed = needed
            Tree = child
        }

    /// JOIN の左右へ必要列を分割し、子の作業項目を作る。
    let private scanNeedsJoinJobs
        (memo: OutputSchemaMemo)
        (item: ScanNeedsJob)
        (on: Expr)
        (left: Logical)
        (right: Logical)
        =
        let leftSchema = outputOfMemo item.Context item.Input memo left
        let rightSchema = outputOfMemo item.Context item.Input memo right
        let joined = Schema.concat leftSchema rightSchema
        let withOn = Set.union item.Needed (scanRefsOf joined on)
        let leftWidth = leftSchema.Columns.Length
        let leftNeeded = withOn |> Set.filter (fun i -> i < leftWidth)

        let rightNeeded =
            withOn
            |> Set.filter (fun i -> i >= leftWidth)
            |> Set.map (fun i -> i - leftWidth)

        let leftJob =
            { item with
                Input = leftSchema
                Needed = leftNeeded
                Tree = left
            }

        let rightJob =
            { item with
                Input = rightSchema
                Needed = rightNeeded
                Tree = right
            }

        [ leftJob; rightJob ]

    /// 派生テーブルの内側の作業項目を作る。
    let private scanNeedsDerivedJob (memo: OutputSchemaMemo) (item: ScanNeedsJob) (plan: PlannedQuery) =
        let innerOutput = outputOfMemo plan.Context plan.Input memo plan.Tree

        let innerNeeded =
            item.Needed |> Set.filter (fun i -> i < innerOutput.Columns.Length)

        {
            Context = plan.Context
            Input = plan.Input
            Needed = innerNeeded
            Tree = plan.Tree
        }

    /// UNION ALL の各枝の作業項目を作る（各枝は全列を必要とする）。
    let private scanNeedsUnionJobs (memo: OutputSchemaMemo) (plans: PlannedQuery list) =
        plans
        |> List.map (fun plan ->
            {
                Context = plan.Context
                Input = plan.Input
                Needed = scanAllColumns (outputOfMemo plan.Context plan.Input memo plan.Tree)
                Tree = plan.Tree
            })

    /// 作業項目 1つを処理し、確定した Scan のマスクと残りの作業リストを返す。
    let private processScanNeedsJob
        (memo: OutputSchemaMemo)
        (masks: (obj * bool array) list)
        (item: ScanNeedsJob)
        (rest: ScanNeedsJob list)
        =
        match item.Tree with
        | Logical.Scan _ as node ->
            let mask =
                Array.init item.Input.Columns.Length (fun i -> Set.contains i item.Needed)

            (box node, mask) :: masks, rest
        | Logical.Filter(p, child) ->
            let childSchema = outputOfMemo item.Context item.Input memo child

            masks,
            scanNeedsChild item (Set.union item.Needed (scanRefsOf childSchema p)) child
            :: rest
        | Logical.Project(items, child) ->
            let childSchema = outputOfMemo item.Context item.Input memo child
            let projection = SelectProjection.create childSchema items

            masks,
            scanNeedsChild item (scanRefsOfAll childSchema projection.Expressions) child
            :: rest
        | Logical.Sort(keys, child) ->
            let childSchema = outputOfMemo item.Context item.Input memo child
            let keyExprs = keys |> List.map _.Expression

            masks,
            scanNeedsChild item (Set.union item.Needed (scanRefsOfAll childSchema keyExprs)) child
            :: rest
        | Logical.Aggregate(groupBy, aggs, child) ->
            let childSchema = outputOfMemo item.Context item.Input memo child

            let exprs =
                groupBy
                @ (aggs
                   |> List.filter (fun (_, star, _, _) -> not star)
                   |> List.map (fun (_, _, _, arg) -> arg))

            masks, scanNeedsChild item (scanRefsOfAll childSchema exprs) child :: rest
        | Logical.Having(h, aggs, groupBy, child) ->
            let aggSchema = aggregateSchema groupBy aggs

            masks,
            scanNeedsChild item (Set.union item.Needed (scanRefsOf aggSchema h)) child
            :: rest
        | Logical.Distinct child ->
            let childSchema = outputOfMemo item.Context item.Input memo child
            masks, scanNeedsChild item (scanAllColumns childSchema) child :: rest
        | Logical.Limit(_, _, child) -> masks, scanNeedsChild item item.Needed child :: rest
        | Logical.Join(_, _, on, left, right) -> masks, scanNeedsJoinJobs memo item on left right @ rest
        | Logical.Derived plan -> masks, scanNeedsDerivedJob memo item plan :: rest
        | Logical.UnionAll plans -> masks, scanNeedsUnionJobs memo plans @ rest
        | Logical.Values _ -> masks, rest

    /// 必要列を後方伝播する作業を末尾再帰で処理する。
    /// 明示スタックなので木の形に依存せずスタックは一定。
    [<TailCall>]
    let rec private scanNeedsLoop (memo: OutputSchemaMemo) (masks: (obj * bool array) list) =
        function
        | [] -> masks
        | item :: rest ->
            let masks, next = processScanNeedsJob memo masks item rest
            scanNeedsLoop memo masks next

    /// 出力列の必要集合を後方伝播し、各 Scan の列マスクを求める。
    /// `Logical` は参照等価の `obj` キーで覚える（構造比較はできない）。
    let private scanNeedsMasks (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (tree: Logical) =
        let root =
            {
                Context = ctx
                Input = inSchema
                Needed = scanAllColumns (outputOfMemo ctx inSchema memo tree)
                Tree = tree
            }

        let entries =
            scanNeedsLoop memo [] [ root ]
            |> List.map (fun (key, mask) -> System.Collections.Generic.KeyValuePair<obj, bool array>(key, mask))

        System.Collections.Immutable.ImmutableDictionary.CreateRange<obj, bool array>(HashIdentity.Reference, entries)

    /// 集約ノードの GROUP BY 式を拾う。
    let private groupByOf (tree: Logical) =
        match stripWrappers tree with
        | Logical.Aggregate(groupBy, _, _) -> groupBy
        | _ -> []

    /// 子（ラッパ越し）が集約なら GROUP BY と集約の一覧を返す。
    /// 集約後の ORDER BY を、グループ値・集約値のスロット参照へ置き換えるために使う。
    let private aggregateOf (tree: Logical) =
        match stripWrappers tree with
        | Logical.Aggregate(groupBy, aggs, _) -> Some(groupBy, aggs)
        | _ -> None

    /// 入れ子ループ結合を実行する。
    let private execNestedLoopJoin (ctx: ExecContext) (frame: JoinFrame) (left: Op) (right: Op) =
        match frame.Kind with
        | JoinKind.Inner -> OpsJoin.innerJoin ctx.Budget frame.LeftSchema left frame.RightSchema right frame.On
        | JoinKind.Left -> OpsJoin.leftJoin ctx.Budget frame.LeftSchema left frame.RightSchema right frame.On

    /// ハッシュ結合を実行する（build 側と INNER/LEFT の組み合わせを振り分ける）。
    let private execHashJoin (ctx: ExecContext) (frame: JoinFrame) (left: Op) (right: Op) =
        match frame.Keys with
        | None -> err (DbError.internalError "ハッシュ結合に結合キーがありません")
        | Some keys ->
            match frame.Kind with
            | JoinKind.Inner ->
                match frame.BuildSide with
                | HashBuildLeft ->
                    OpsJoinHash.innerJoinHashBuildLeft
                        ctx.Budget
                        frame.LeftSchema
                        left
                        keys.LeftIndices
                        frame.RightSchema
                        right
                        keys.RightIndices
                        keys.Residual
                | HashBuildRight ->
                    OpsJoinHash.innerJoinHash
                        ctx.Budget
                        frame.LeftSchema
                        left
                        keys.LeftIndices
                        frame.RightSchema
                        right
                        keys.RightIndices
                        keys.Residual
            | JoinKind.Left ->
                OpsJoinHash.leftJoinHash
                    ctx.Budget
                    frame.LeftSchema
                    left
                    keys.LeftIndices
                    frame.RightSchema
                    right
                    keys.RightIndices
                    keys.Residual

    /// マージ結合を実行する。
    let private execMergeJoin (ctx: ExecContext) (frame: JoinFrame) (left: Op) (right: Op) =
        match frame.Keys with
        | None -> err (DbError.internalError "マージ結合に結合キーがありません")
        | Some keys ->
            match frame.Kind with
            | JoinKind.Inner ->
                OpsJoinMerge.innerJoinMerge
                    ctx.Budget
                    frame.LeftSchema
                    left
                    keys.LeftIndices
                    frame.RightSchema
                    right
                    keys.RightIndices
                    keys.Residual
            | JoinKind.Left ->
                OpsJoinMerge.leftJoinMerge
                    ctx.Budget
                    frame.LeftSchema
                    left
                    keys.LeftIndices
                    frame.RightSchema
                    right
                    keys.RightIndices
                    keys.Residual

    /// 結合アルゴリズムを選んで実行する。
    let private execJoin (ctx: ExecContext) (frame: JoinFrame) (left: Op) (right: Op) =
        match frame.Algorithm with
        | JoinAlgorithm.NestedLoop -> execNestedLoopJoin ctx frame left right
        | JoinAlgorithm.HashJoin -> execHashJoin ctx frame left right
        | JoinAlgorithm.IndexNestedLoop _ ->
            DbError.internalError "index nested loop は JoinIndexLeftFrame で実行します" |> err
        | JoinAlgorithm.MergeJoin -> execMergeJoin ctx frame left right

    /// 文脈からテーブル統計を引く。
    let private statsOfContext (ctx: ExecContext) (name: string) = ExecContext.statsOf name ctx

    /// 索引スキャンで追加の主キー lookup なしに読める列位置。
    /// `Dml.indexAccessMasks` と同じ「索引列 + 主キー」の規則を使う。
    let private coveredColumns (ctx: ExecContext) (name: string) (indexName: string) =
        match ExecContext.schemaOf name ctx with
        | Error _ -> Set.empty
        | Ok schema ->
            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> Set.empty
            | Some idx ->
                (idx.Columns |> List.choose (TableSchema.indexOf schema)) @ schema.PrimaryKey
                |> Set.ofList

    /// 索引走査と必要なら非被覆列の fetch を結ぶ。
    /// `preFilter` は被覆列（索引列 + 主キー）だけで評価できる述語で、
    /// 主キー lookup の前に適用して捨てられる行を減らす。
    let private execIndexScan
        (ctx: ExecContext)
        (name: string)
        (indexName: string)
        (mask: bool array)
        (preFilter: Expr option)
        (build: bool array -> Result<Op, DbError>)
        =
        result {
            let! scanMask, fetchMask = ExecContext.indexAccessMasks ctx name indexName mask
            let! op = build scanMask

            let! op =
                match preFilter with
                | None -> ret op
                | Some p -> Ops.filter (Schema.ofTable name None (defaultSchema ctx name)) p op

            if fetchMask |> Array.exists id then
                return! ExecContext.fetchMissing ctx name fetchMask op
            else
                return op
        }

    /// 走査ノードを Op に変換する（件数ヒントと列マスクを適用）。
    let private execScan (node: 'a) (ctx: ExecContext) (name: string) (method: ScanMethod) (preFilter: Expr option) =
        let mask =
            match ctx.ScanNeeds.TryGetValue(box node) with
            | true, mask -> mask
            | _ -> Array.create (defaultSchema ctx name).Columns.Length true

        function
        | Some 0 -> ret Op.Empty
        | hint ->
            let limit = defaultArg hint 0

            match method with
            | ScanMethod.SeqScan -> ExecContext.tableMasked ctx name limit mask
            | ScanMethod.PkLookup values -> ExecContext.byPrimaryKeyMasked ctx name values mask
            | ScanMethod.IndexLookup(indexName, values) ->
                execIndexScan ctx name indexName mask preFilter (fun scanMask ->
                    ExecContext.byIndexMasked ctx name indexName values limit scanMask)
            | ScanMethod.RangeScan(_, startKey, endKey) ->
                ExecContext.rangeScanMasked ctx name startKey endKey limit mask
            | ScanMethod.IndexRangeScan(indexName, lower, upper) ->
                execIndexScan ctx name indexName mask preFilter (fun scanMask ->
                    ExecContext.byIndexRangeMasked ctx name indexName lower upper limit scanMask)

    /// オフセットと件数を Int32 の範囲へクランプして足す。
    let private addClamp (a: int) (b: int) =
        if a > System.Int32.MaxValue - b then
            System.Int32.MaxValue
        else
            a + b

    /// 子へ渡す件数ヒントを求める。
    let private childLimitHint (offset: int) (count: int) (hint: int option) =
        if count = 0 then
            Some 0
        else
            match count with
            | c when c >= 0 ->
                let inner = addClamp offset c

                Some(
                    match hint with
                    | Some h -> min h inner
                    | None -> inner
                )
            | _ -> hint |> Option.map (fun h -> addClamp offset h)

    /// 索引で右行を引き、必要なら非被覆列を fetch する。
    let private lookupIndexRows
        (ctx: ExecContext)
        (frame: JoinFrame)
        (indexName: string)
        (scanMask: bool array)
        (fetchMask: bool array)
        (keyValues: SqlValue array)
        =
        result {
            let! op =
                ExecContext.byIndexMasked ctx frame.RightName indexName keyValues 0 scanMask

            if fetchMask |> Array.exists id then
                return! ExecContext.fetchMissing ctx frame.RightName fetchMask op
            else
                return op
        }

    /// Filter を剥がして右部分木の Scan を探す。
    [<TailCall>]
    let rec private rightScanOf =
        function
        | Logical.Scan _ as scan -> Some scan
        | Logical.Filter(_, child) -> rightScanOf child
        | _ -> None

    /// 索引検索の結果を左行と結合する。
    let private joinWithLookup
        (frame: JoinFrame)
        (left: Op)
        (keys: JoinKeys)
        (lookupRows: SqlValue array -> Result<Op, DbError>)
        =
        match frame.Kind with
        | JoinKind.Inner ->
            OpsJoin.innerJoinIndexed
                frame.LeftSchema
                left
                frame.LeftKeyIndices
                frame.RightSchema
                keys.Residual
                lookupRows
        | JoinKind.Left ->
            OpsJoin.leftJoinIndexed
                frame.LeftSchema
                left
                frame.LeftKeyIndices
                frame.RightSchema
                keys.Residual
                lookupRows

    /// インデックス入れ子ループを実行する。
    let private execIndexJoin (ctx: ExecContext) (frame: JoinFrame) (left: Op) =
        match frame.Keys with
        | None -> err (DbError.internalError "index nested loop に結合キーがありません")
        | Some keys ->
            let baseMask =
                match rightScanOf frame.Right with
                | Some scan ->
                    match ctx.ScanNeeds.TryGetValue(box scan) with
                    | true, mask -> mask
                    | _ -> Array.create frame.RightSchema.Columns.Length true
                | None -> Array.create frame.RightSchema.Columns.Length true

            let residualColumns =
                match keys.Residual with
                | None -> Set.empty
                | Some p ->
                    Expr.columnRefs p
                    |> List.choose (fun c ->
                        match Schema.resolve c frame.RightSchema with
                        | Ok i -> Some i
                        | Error _ -> None)
                    |> Set.ofList

            let rightMask =
                Array.init frame.RightSchema.Columns.Length (fun i ->
                    i < baseMask.Length && baseMask.[i] || Set.contains i residualColumns)

            match frame.Algorithm with
            | JoinAlgorithm.IndexNestedLoop JoinLookup.PrimaryKey ->
                joinWithLookup frame left keys (fun keyValues ->
                    ExecContext.byPrimaryKeyMasked ctx frame.RightName keyValues rightMask)
            | JoinAlgorithm.IndexNestedLoop(JoinLookup.Index indexName) ->
                result {
                    let! scanMask, fetchMask =
                        ExecContext.indexAccessMasks ctx frame.RightName indexName rightMask

                    return! joinWithLookup frame left keys (lookupIndexRows ctx frame indexName scanMask fetchMask)
                }
            | _ -> err (DbError.internalError "index nested loop ではありません")

    /// 結合ノードの実行フレームを組み立てる。
    let private buildJoinFrame
        (ctx: ExecContext)
        (memo: OutputSchemaMemo)
        (inSchema: Schema)
        (node: Logical)
        (kind: JoinKind)
        (name: string)
        (on: Expr)
        (left: Logical)
        (right: Logical)
        =
        match ctx.JoinPlans.TryGetValue(box node) with
        | true, (:? JoinFrame as cached) -> cached, cached.LeftSchema
        | _ ->
            let leftSchema = outputOfMemo ctx inSchema memo left

            let rightSchema, rightTable =
                match stripWrappers right with
                | Logical.Derived plan -> plan.Output, None
                | Logical.Join _ -> outputOfMemo ctx inSchema memo right, None
                | _ ->
                    let table = defaultSchema ctx name
                    Schema.ofTable name None table, Some table

            let statsOf = statsOfContext ctx

            let algorithm, keys, leftKeyIndices =
                Analyzer.chooseJoinAlgorithmWith statsOf (tableOf ctx) leftSchema rightSchema rightTable left right on

            let frame =
                {
                    Node = node
                    Kind = kind
                    Right = right
                    On = on
                    LeftSchema = leftSchema
                    RightSchema = rightSchema
                    RightName = name
                    Algorithm = algorithm
                    Keys = keys
                    LeftKeyIndices = defaultArg leftKeyIndices [||]
                    BuildSide = hashBuildSide statsOf algorithm kind left right
                }

            frame, leftSchema

    /// 論理木を 1ノードずつ処理する。
    /// 継続は ExecFrame list に積むので、ラッパの深さに依存せずスタックは一定。
    [<TailCall>]
    let rec private run (ctx: ExecContext) (memo: OutputSchemaMemo) (frames: ExecFrame list) =
        function
        | EvalNode(node, inSchema, hint, preFilter) ->
            match node with
            | Logical.Join(kind, name, on, lTree, rTree) ->
                let frame, leftSchema =
                    buildJoinFrame ctx memo inSchema node kind name on lTree rTree

                match frame.Algorithm with
                | JoinAlgorithm.IndexNestedLoop _ ->
                    run ctx memo (JoinIndexLeftFrame frame :: frames) (EvalNode(lTree, leftSchema, None, None))
                | _ -> run ctx memo (JoinRightFrame frame :: frames) (EvalNode(lTree, leftSchema, None, None))
            | Logical.Values values -> run ctx memo frames (ApplyOp(Op.ofRow values))
            | Logical.Derived plan ->
                let innerCtx =
                    { plan.Context with
                        ScanNeeds = ctx.ScanNeeds
                    }

                run innerCtx memo (DerivedFrame ctx :: frames) (EvalNode(plan.Tree, plan.Input, None, None))
            | Logical.UnionAll [] -> run ctx memo frames (ApplyOp Op.Empty)
            | Logical.UnionAll(plan :: rest) ->
                let innerCtx =
                    { plan.Context with
                        ScanNeeds = ctx.ScanNeeds
                    }

                run innerCtx memo (UnionFrame(ctx, [], rest) :: frames) (EvalNode(plan.Tree, plan.Input, None, None))
            | Logical.Scan(name, method) ->
                match execScan node ctx name method preFilter hint with
                | Ok op -> run ctx memo frames (ApplyOp(countOutput ctx node op))
                | Error e -> err e
            | Logical.Filter(p, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child

                let preConjuncts, remaining =
                    match child with
                    | Logical.Scan(name, ScanMethod.IndexLookup(indexName, _))
                    | Logical.Scan(name, ScanMethod.IndexRangeScan(indexName, _, _)) ->
                        let covered = coveredColumns ctx name indexName

                        Predicate.splitConjuncts p
                        |> List.partition (fun conjunct ->
                            scanRefsOf childSchema conjunct |> Set.forall (fun i -> Set.contains i covered))
                    | _ -> [], Predicate.splitConjuncts p

                let preFilter = Predicate.joinConjuncts preConjuncts
                let remainingExpr = Predicate.joinConjuncts remaining

                match remainingExpr with
                | Some rest ->
                    run ctx memo (FilterFrame(rest, childSchema) :: frames) (EvalNode(child, inSchema, None, preFilter))
                | None -> run ctx memo frames (EvalNode(child, inSchema, None, preFilter))
            | Logical.Aggregate(groupBy, aggs, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child

                run
                    ctx
                    memo
                    (AggregateFrame(groupBy, aggs, childSchema) :: frames)
                    (EvalNode(child, inSchema, None, None))
            | Logical.Having(h, aggs, groupBy, child) ->
                run ctx memo (HavingFrame(h, aggs, groupBy) :: frames) (EvalNode(child, inSchema, None, None))
            | Logical.Distinct child -> run ctx memo (DistinctFrame :: frames) (EvalNode(child, inSchema, None, None))
            | Logical.Sort(keys, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child

                let groupBy, aggs =
                    match aggregateOf child with
                    | Some(groupBy, aggs) -> groupBy, aggs
                    | None -> [], []

                run
                    ctx
                    memo
                    (SortFrame(keys, childSchema, groupBy, aggs) :: frames)
                    (EvalNode(child, inSchema, None, None))
            | Logical.Limit(offset, limit, child) ->
                let childHint = childLimitHint offset limit hint
                run ctx memo (LimitFrame(offset, limit) :: frames) (EvalNode(child, inSchema, childHint, None))
            | Logical.Project(items, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child
                let groupBy = groupByOf child

                run
                    ctx
                    memo
                    (ProjectFrame(items, inSchema, childSchema, groupBy) :: frames)
                    (EvalNode(child, inSchema, hint, None))
        | ApplyOp op ->
            match frames with
            | [] -> ret op
            | frame :: rest ->
                match frame with
                | DerivedFrame outer -> run outer memo rest (ApplyOp op)
                | FilterFrame(p, childSchema) ->
                    match Ops.filter childSchema p op with
                    | Ok filtered -> run ctx memo rest (ApplyOp filtered)
                    | Error e -> err e
                | AggregateFrame(groupBy, aggs, inSchema) ->
                    match OpsAggregate.aggregate ctx.Budget inSchema groupBy aggs op with
                    | Ok aggregated -> run ctx memo rest (ApplyOp aggregated)
                    | Error e -> err e
                | HavingFrame(h, aggs, groupBy) ->
                    match OpsAggregate.havingFilter groupBy aggs h with
                    | Error e -> err e
                    | Ok p -> run ctx memo rest (ApplyOp(Op.filter p op))
                | DistinctFrame -> run ctx memo rest (ApplyOp(OpsDistinct.distinct ctx.Budget op))
                | SortFrame(keys, childSchema, groupBy, aggs) ->
                    match OpsSort.sortBy ctx.Budget childSchema groupBy aggs keys op with
                    | Ok sorted -> run ctx memo rest (ApplyOp sorted)
                    | Error e -> err e
                | LimitFrame(offset, limit) ->
                    let skipped = if offset > 0 then Op.skip offset op else op
                    let limited = if limit >= 0 then Op.take limit skipped else skipped
                    run ctx memo rest (ApplyOp limited)
                | ProjectFrame(items, expandSchema, evalSchema, groupBy) ->
                    match Ops.project expandSchema evalSchema groupBy items op with
                    | Ok projected -> run ctx memo rest (ApplyOp projected)
                    | Error e -> err e
                | JoinIndexLeftFrame frame ->
                    match execIndexJoin ctx frame op with
                    | Ok joined -> run ctx memo rest (ApplyOp(countOutput ctx frame.Node joined))
                    | Error e -> err e
                | JoinRightFrame frame ->
                    run
                        ctx
                        memo
                        (JoinApplyFrame(frame, op) :: rest)
                        (EvalNode(frame.Right, frame.RightSchema, None, None))
                | JoinApplyFrame(frame, leftOp) ->
                    match execJoin ctx frame leftOp op with
                    | Ok joined -> run ctx memo rest (ApplyOp(countOutput ctx frame.Node joined))
                    | Error e -> err e
                | UnionFrame(outer, doneOps, []) ->
                    let combined = List.foldBack Op.append (List.rev (op :: doneOps)) Op.Empty
                    run outer memo rest (ApplyOp combined)
                | UnionFrame(outer, doneOps, plan :: plans) ->
                    let innerCtx =
                        { plan.Context with
                            ScanNeeds = outer.ScanNeeds
                        }

                    run
                        innerCtx
                        memo
                        (UnionFrame(outer, op :: doneOps, plans) :: rest)
                        (EvalNode(plan.Tree, plan.Input, None, None))

    /// 論理代数を Op に変換する。
    /// 結合ノードを後順で集める作業項目。
    /// <remarks>
    /// 下へ渡すスキーマは常に「入力スキーマ」schema のまま。
    /// outputOf child schema は「child の出力スキーマ」であって、
    /// child を実行するときの入力スキーマではない。
    /// ここを間違えると、`SELECT SUM(age) FROM t` で
    /// 集約ノードが「集約後のヘッダ（`SUM(t.age)`）」を入力として受け取るため、
    /// 引数の `t.age` を評価できず `SUM` が Null になり、
    /// `GROUP BY` も別の列でグループ化してしまう。
    /// childSchema は「そのノードが結果を読む側」だけが使う。
    /// </remarks>
    type private JoinPlanJob =
        | VisitNode of Logical * Schema
        | PutJoinPlan of Logical * Schema

    /// 結合ノード 1つ分の実行プランを求めて辞書へ積む。
    let private processJoinPlanJob
        (baseCtx: ExecContext)
        (memo: OutputSchemaMemo)
        (acc: (obj * obj) list)
        (frames: JoinPlanJob list)
        =
        function
        | VisitNode(Logical.Join(_, _, _, left, right) as node, inSchema) ->
            let leftSchema = outputOfMemo baseCtx inSchema memo left
            let rightSchema = outputOfMemo baseCtx inSchema memo right

            acc,
            VisitNode(left, leftSchema)
            :: VisitNode(right, rightSchema)
            :: PutJoinPlan(node, inSchema)
            :: frames
        | VisitNode((Logical.Filter(_, child) | Logical.Sort(_, child) | Logical.Limit(_, _, child) | Logical.Distinct child | Logical.Having(_,
                                                                                                                                              _,
                                                                                                                                              _,
                                                                                                                                              child) | Logical.Project(_,
                                                                                                                                                                       child) | Logical.Aggregate(_,
                                                                                                                                                                                                  _,
                                                                                                                                                                                                  child)),
                    inSchema) -> acc, VisitNode(child, inSchema) :: frames
        | VisitNode(_, _) -> acc, frames
        | PutJoinPlan(Logical.Join(kind, name, on, left, right) as node, inSchema) ->
            let frame, _ = buildJoinFrame baseCtx memo inSchema node kind name on left right
            (box node, box frame) :: acc, frames
        | PutJoinPlan(_, _) -> acc, frames

    [<TailCall>]
    let rec private collectJoinPlan (ctx: ExecContext) (memo: OutputSchemaMemo) (acc: (obj * obj) list) =
        function
        | [] ->
            acc
            |> List.map (fun (k, v) -> System.Collections.Generic.KeyValuePair<obj, obj>(k, v))
            |> fun entries ->
                System.Collections.Immutable.ImmutableDictionary.CreateRange<obj, obj>(HashIdentity.Reference, entries)
        | job :: rest ->
            let acc, next = processJoinPlanJob ctx memo acc rest job
            collectJoinPlan ctx memo acc next

    /// 結合ノードの実行プランを 1パスで求める。
    let private collectJoinPlans (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (tree: Logical) =
        collectJoinPlan ctx memo [] [ VisitNode(tree, inSchema) ]

    /// 実行に必要なプラン情報（列マスクと結合プラン）を文脈へ埋める。
    /// 既に埋まっていれば再計算しない（計画時の 1回分を再利用する）。
    let prepareContext (ctx: ExecContext) (inSchema: Schema) (tree: Logical) =
        if not ctx.ScanNeeds.IsEmpty && not ctx.JoinPlans.IsEmpty then
            ctx
        else
            let memo = OutputSchemaMemo()

            let baseCtx =
                { ctx with
                    JoinPlans = System.Collections.Immutable.ImmutableDictionary.Create()
                }

            { ctx with
                ScanNeeds = scanNeedsMasks ctx inSchema memo tree
                JoinPlans = collectJoinPlans baseCtx inSchema memo tree
            }

    let exec (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        let memo = OutputSchemaMemo()
        let ctx = prepareContext ctx schema tree
        run ctx memo [] (EvalNode(tree, schema, None, None))

    /// 実行しながら、走査と結合の出力行数を `counts` に数える。
    /// `EXPLAIN ANALYZE` 用で、通常の `exec` は計測しない。
    let execWithCounts
        (ctx: ExecContext)
        (counts: System.Collections.Generic.Dictionary<obj, int64>)
        (tree: Logical)
        (schema: Schema)
        =
        exec { ctx with RowCounts = Some counts } tree schema

    /// EXPLAIN の作業項目。
    type private ExplainItem =
        {
            Indent: int
            Tree: Logical
            Schema: Schema
            /// このノードを実行するときの文脈
            /// （派生テーブルの内側は別の文脈を持つ）。
            Context: ExecContext
        }

    /// EXPLAIN の作業項目を作る。
    let private explainChildItem (indent: int) (tree: Logical) (schema: Schema) (context: ExecContext) =
        {
            Indent = indent
            Tree = tree
            Schema = schema
            Context = context
        }

    /// 子ノードの作業項目を作る。
    let private explainChildOf (item: ExplainItem) (tree: Logical) (schema: Schema) =
        explainChildItem (item.Indent + 1) tree schema item.Context

    /// 値を SQL リテラル表記へ変換する。
    let private explainValueText = SqlLiteral.toSqlLiteral

    /// 値配列をカンマ区切り文字列へ変換する。
    let private explainValuesText = Array.map explainValueText >> String.concat ", "

    /// SELECT 項目の表示文字列を作る。
    let private explainItemText (item: SelectItem) =
        let expression = Expr.toDisplayString item.Expression

        match item.Alias with
        | Some alias -> sprintf "%s AS %s" expression alias
        | None -> expression

    /// 範囲走査のキー表示を作る。
    let private explainRangeText (lower: SqlValue array) (upper: SqlValue array option) =
        let upper = upper |> Option.map explainValuesText |> Option.defaultValue "*"
        sprintf "%s..%s" (explainValuesText lower) upper

    /// 走査方法の表示を作る。
    let private explainScanText (name: string) =
        function
        | ScanMethod.SeqScan -> sprintf "%s, SeqScan" name
        | ScanMethod.PkLookup values -> sprintf "%s, PkLookup(%s)" name (explainValuesText values)
        | ScanMethod.IndexLookup(index, values) ->
            sprintf "%s, IndexLookup(%s, %s)" name index (explainValuesText values)
        | ScanMethod.RangeScan(_, lower, upper) -> sprintf "%s, RangeScan(%s)" name (explainRangeText lower upper)
        | ScanMethod.IndexRangeScan(index, lower, upper) ->
            sprintf "%s, IndexRangeScan(%s, %s)" name index (explainRangeText lower upper)

    /// 結合アルゴリズムの表示を作る。
    let private explainAlgorithmText =
        function
        | JoinAlgorithm.NestedLoop -> "NestedLoop"
        | JoinAlgorithm.HashJoin -> "HashJoin"
        | JoinAlgorithm.MergeJoin -> "MergeJoin"
        | JoinAlgorithm.IndexNestedLoop JoinLookup.PrimaryKey -> "IndexNestedLoop(pk)"
        | JoinAlgorithm.IndexNestedLoop(JoinLookup.Index index) -> sprintf "IndexNestedLoop(%s)" index

    /// JOIN 種別の表示を作る。
    let private explainJoinKindText =
        function
        | JoinKind.Inner -> "INNER"
        | JoinKind.Left -> "LEFT"

    /// 集約呼び出しの表示を作る。
    let private explainAggText (f: AggFunc, star: bool, distinct: bool, arg: Expr) =
        let body = if star then "*" else Expr.toDisplayString arg
        sprintf "%s(%s%s)" f.Name (if distinct then "DISTINCT " else "") body

    /// 並び替えキーの表示を作る。
    let private explainSortText (k: SortKey) =
        let direction =
            match k.Direction with
            | SortDirection.Ascending -> "ASC"
            | SortDirection.Descending -> "DESC"

        sprintf "%s %s" (Expr.toDisplayString k.Expression) direction

    /// スキーマの管理メモリ概算を返す。
    let private explainRowBytesOfSchema (schema: Schema) =
        Analyzer.rowBytesOfTypes (schema.Columns |> Seq.map _.DataType)

    /// 索引走査で主キー fetch が必要なときの表示（` + Fetch`）を返す。
    let private explainFetchSuffix (item: ExplainItem) (name: string) (method: ScanMethod) =
        match method with
        | ScanMethod.IndexLookup(indexName, _)
        | ScanMethod.IndexRangeScan(indexName, _, _) ->
            match item.Context.ScanNeeds.TryGetValue(box item.Tree) with
            | true, mask ->
                match ExecContext.indexAccessMasks item.Context name indexName mask with
                | Ok(_, fetchMask) when fetchMask |> Array.exists id -> " + Fetch"
                | _ -> ""
            | _ -> ""
        | _ -> ""

    /// ハッシュ結合の表示（build 側とスピル見積りを添える）。
    let private explainHashJoinText
        (statsOf: string -> TableStats option)
        (ctx: ExecContext)
        (kind: JoinKind)
        (left: Logical)
        (right: Logical)
        (leftSchema: Schema)
        (rightSchema: Schema)
        =
        let buildSide = hashBuildSide statsOf JoinAlgorithm.HashJoin kind left right

        match buildSide with
        | HashBuildLeft -> "HashJoin(left)"
        | HashBuildRight ->
            let leftRows = Analyzer.estimateRowsWith statsOf left
            let rightRows = Analyzer.estimateRowsWith statsOf right
            let buildRows = min leftRows rightRows

            let buildRowBytes =
                if leftRows < rightRows then
                    explainRowBytesOfSchema leftSchema
                else
                    explainRowBytesOfSchema rightSchema

            let spilled =
                Analyzer.spilledBuildRows
                    buildRows
                    buildRowBytes
                    (if ctx.Budget.Work >= 0L then
                         float ctx.Budget.Work
                     else
                         -1.0)

            if spilled > 0.0 then
                sprintf "HashJoin(spill=%.0fKB)" (spilled * buildRowBytes / 1024.0)
            else
                "HashJoin"

    /// JOIN ノードの表示と子項目を作る。
    let private explainJoinNode
        (memo: OutputSchemaMemo)
        (item: ExplainItem)
        (kind: JoinKind)
        (name: string)
        (on: Expr)
        (left: Logical)
        (right: Logical)
        =
        let leftSchema = outputOfMemo item.Context item.Schema memo left

        let rightSchema, rightTable =
            match stripWrappers right with
            | Logical.Derived plan -> plan.Output, None
            | Logical.Join _ -> outputOfMemo item.Context item.Schema memo right, None
            | _ ->
                let table = defaultSchema item.Context name
                Schema.ofTable name None table, Some table

        let statsOf = statsOfContext item.Context

        let algorithm, _, _ =
            Analyzer.chooseJoinAlgorithmWith
                statsOf
                (tableOf item.Context)
                leftSchema
                rightSchema
                rightTable
                left
                right
                on

        let algorithmTextValue =
            match algorithm with
            | JoinAlgorithm.HashJoin -> explainHashJoinText statsOf item.Context kind left right leftSchema rightSchema
            | _ -> explainAlgorithmText algorithm

        let text =
            sprintf
                "Join(%s, %s, right=%s, on=%s)"
                (explainJoinKindText kind)
                algorithmTextValue
                name
                (Expr.toDisplayString on)

        text, [ explainChildOf item left leftSchema; explainChildOf item right rightSchema ]

    /// 1ノードを EXPLAIN の行と子項目へ変換する。
    let private explainDescribe (memo: OutputSchemaMemo) (item: ExplainItem) =
        match item.Tree with
        | Logical.Scan(name, method) ->
            sprintf "Scan(%s)%s" (explainScanText name method) (explainFetchSuffix item name method), []
        | Logical.Values values ->
            if Array.isEmpty values then
                "Values", []
            else
                sprintf "Values(%s)" (explainValuesText values), []
        | Logical.Filter(predicate, child) ->
            sprintf "Filter(%s)" (Expr.toDisplayString predicate), [ explainChildOf item child item.Schema ]
        | Logical.Project(items, child) ->
            let text = items |> List.map explainItemText |> String.concat ", "
            sprintf "Project(%s)" text, [ explainChildOf item child item.Schema ]
        | Logical.Join(kind, name, on, left, right) -> explainJoinNode memo item kind name on left right
        | Logical.Aggregate(groupBy, aggs, child) ->
            let text =
                sprintf
                    "Aggregate(group by [%s], aggregates [%s])"
                    (groupBy |> List.map Expr.toDisplayString |> String.concat ", ")
                    (aggs |> List.map explainAggText |> String.concat ", ")

            text, [ explainChildOf item child item.Schema ]
        | Logical.Having(predicate, _, _, child) ->
            sprintf "Having(%s)" (Expr.toDisplayString predicate), [ explainChildOf item child item.Schema ]
        | Logical.Distinct child -> "Distinct", [ explainChildOf item child item.Schema ]
        | Logical.Sort(keys, child) ->
            let text = keys |> List.map explainSortText |> String.concat ", "
            sprintf "Sort(%s)" text, [ explainChildOf item child item.Schema ]
        | Logical.Limit(offset, limit, child) ->
            let text =
                if limit >= 0 then
                    sprintf "Limit(offset=%d, count=%d)" offset limit
                else
                    sprintf "Limit(offset=%d)" offset

            text, [ explainChildOf item child item.Schema ]
        | Logical.Derived plan -> "Derived", [ explainChildItem (item.Indent + 1) plan.Tree plan.Input plan.Context ]
        | Logical.UnionAll plans ->
            "UnionAll",
            plans
            |> List.map (fun plan -> explainChildItem (item.Indent + 1) plan.Tree plan.Input plan.Context)

    /// EXPLAIN の行を末尾再帰で組み立てる。
    [<TailCall>]
    let rec private explainEmit (actual: Logical -> int64 option) (memo: OutputSchemaMemo) (acc: string list) =
        function
        | [] -> List.rev acc
        | item :: rest ->
            let text, children = explainDescribe memo item

            let rowSuffix =
                if List.isEmpty item.Context.Catalog.Stats then
                    ""
                else
                    let statsOf = statsOfContext item.Context
                    let rows = Analyzer.estimateRowsWith statsOf item.Tree

                    let rowsText =
                        if System.Double.IsFinite rows then
                            sprintf " rows=%.0f" rows
                        elif System.Double.IsPositiveInfinity rows then
                            " rows=inf"
                        else
                            ""

                    let selectivityText =
                        match item.Tree with
                        | Logical.Join(_, _, on, _, _) ->
                            match Analyzer.joinSelectivity statsOf on with
                            | Some selectivity -> sprintf " sel=%.4g" selectivity
                            | None -> ""
                        | _ -> ""

                    rowsText + selectivityText

            let actualSuffix =
                actual item.Tree
                |> Option.map (fun rows -> sprintf " actual=%d" rows)
                |> Option.defaultValue ""

            let line = String.replicate item.Indent "  " + text + rowSuffix + actualSuffix
            explainEmit actual memo (line :: acc) (children @ rest)

    /// 論理木を EXPLAIN 用の行にする（実行しない）。
    /// `exec` と同じ規則で走査方法・結合アルゴリズムを選び、木を深さ優先で並べる。
    /// `actual` が Some を返したノードには実測行数（`EXPLAIN ANALYZE`）を添える。
    let explainWithCounts (actual: Logical -> int64 option) (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        let memo = OutputSchemaMemo()

        let ctx =
            { ctx with
                ScanNeeds = scanNeedsMasks ctx schema memo tree
            }

        explainEmit actual memo [] [ explainChildItem 0 tree schema ctx ]

    /// 論理木を EXPLAIN 用の行にする（実行しない。実測行数は添えない）。
    let explain (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        explainWithCounts (fun _ -> None) ctx tree schema
