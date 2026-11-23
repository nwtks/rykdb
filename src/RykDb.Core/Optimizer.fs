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
        | EvalNode of Logical * Schema * int option
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

    let private aggregateSchema (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) =
        let column name =
            {
                Name = name
                Qualifier = None
                DataType = None
            }

        let groupColumns = groupBy |> List.map (Expr.toDisplayString >> column)

        let aggColumns =
            aggs
            |> List.map (fun (f, star, _, arg) ->
                let suffix = if star then "*" else Expr.toDisplayString arg
                column (sprintf "%s(%s)" f.Name suffix))

        { Schema.empty with
            Columns = Array.append (List.toArray groupColumns) (List.toArray aggColumns)
        }

    /// 結合を含まないノードの出力スキーマ。
    let private baseSchemaOf (ctx: ExecContext) (inSchema: Schema) =
        function
        | Logical.Aggregate(groupBy, aggs, _) -> aggregateSchema groupBy aggs
        | Logical.Derived plan -> plan.Output
        | Logical.UnionAll(plan :: _) -> plan.Output
        | Logical.Scan(name, _) -> Schema.ofTable name None (defaultSchema ctx name)
        | Logical.Values _ -> Schema.empty
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

    let private outputOf (ctx: ExecContext) (inSchema: Schema) (tree: Logical) =
        outputSchema ctx inSchema Schema.empty [ SchemaOf tree ]

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
    let private processOutputJob
        (ctx: ExecContext)
        (inSchema: Schema)
        (memo: OutputSchemaMemo)
        (rest: OutputJob list)
        =
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

    /// 部分木の全ノードの出力スキーマを後順で求め、メモに入れる。
    /// 明示スタックなので木の形に依存せずスタックは一定。
    let private fillOutputSchemas (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (root: Logical) =
        let rec loop jobs =
            match jobs with
            | [] -> ()
            | job :: rest -> loop (processOutputJob ctx inSchema memo rest job)

        loop [ NeedNode root ]

    /// 出力スキーマをメモから引く（未計算なら部分木を 1パスで計算してから引く）。
    let private outputOfMemo (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (tree: Logical) =
        match memo.TryFind tree with
        | Some schema -> schema
        | None ->
            fillOutputSchemas ctx inSchema memo tree

            match memo.TryFind tree with
            | Some schema -> schema
            | None -> outputOf ctx inSchema tree

    /// 出力列の必要集合を後方伝播し、各 Scan の列マスクを求める。
    /// `Logical` は参照等価の `obj` キーで覚える（構造比較はできない）。
    let private scanNeedsMasks (ctx: ExecContext) (inSchema: Schema) (memo: OutputSchemaMemo) (tree: Logical) =
        let allColumns schema =
            if schema.Columns.Length = 0 then
                Set.empty
            else
                Set.ofSeq [ 0 .. schema.Columns.Length - 1 ]

        let resolveRef schema r =
            match Schema.resolve r schema with
            | Ok i -> Some i
            | Error _ ->
                match Schema.resolve { Qualifier = None; Name = r.Name } schema with
                | Ok i -> Some i
                | Error _ -> None

        let refsOf schema e =
            if Expr.hasAnySubquery e then
                allColumns schema
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

                let resolved = refs |> List.map (resolveRef schema)

                if List.exists Option.isNone resolved then
                    allColumns schema
                else
                    resolved |> List.choose id |> Set.ofList

        let refsOfAll schema exprs =
            exprs |> List.fold (fun acc e -> Set.union acc (refsOf schema e)) Set.empty

        let rec go nodeCtx nodeInput needed =
            function
            | Logical.Scan _ as node ->
                [
                    (box node, Array.init nodeInput.Columns.Length (fun i -> Set.contains i needed))
                ]
            | Logical.Filter(p, child) ->
                let childSchema = outputOfMemo nodeCtx nodeInput memo child
                go nodeCtx nodeInput (Set.union needed (refsOf childSchema p)) child
            | Logical.Project(items, child) ->
                let childSchema = outputOfMemo nodeCtx nodeInput memo child
                let projection = SelectProjection.create childSchema items
                go nodeCtx nodeInput (refsOfAll childSchema projection.Expressions) child
            | Logical.Sort(keys, child) ->
                let childSchema = outputOfMemo nodeCtx nodeInput memo child
                let keyExprs = keys |> List.map _.Expression
                go nodeCtx nodeInput (Set.union needed (refsOfAll childSchema keyExprs)) child
            | Logical.Aggregate(groupBy, aggs, child) ->
                let childSchema = outputOfMemo nodeCtx nodeInput memo child

                let exprs =
                    groupBy
                    @ (aggs
                       |> List.filter (fun (_, star, _, _) -> not star)
                       |> List.map (fun (_, _, _, arg) -> arg))

                go nodeCtx nodeInput (refsOfAll childSchema exprs) child
            | Logical.Having(h, aggs, groupBy, child) ->
                let aggSchema = aggregateSchema groupBy aggs
                go nodeCtx nodeInput (Set.union needed (refsOf aggSchema h)) child
            | Logical.Distinct child ->
                let childSchema = outputOfMemo nodeCtx nodeInput memo child
                go nodeCtx nodeInput (allColumns childSchema) child
            | Logical.Limit(_, _, child) -> go nodeCtx nodeInput needed child
            | Logical.Join(_, _, on, left, right) ->
                let leftSchema = outputOfMemo nodeCtx nodeInput memo left
                let rightSchema = outputOfMemo nodeCtx nodeInput memo right
                let joined = Schema.concat leftSchema rightSchema
                let withOn = Set.union needed (refsOf joined on)
                let leftWidth = leftSchema.Columns.Length
                let leftNeeded = withOn |> Set.filter (fun i -> i < leftWidth)

                let rightNeeded =
                    withOn
                    |> Set.filter (fun i -> i >= leftWidth)
                    |> Set.map (fun i -> i - leftWidth)

                go nodeCtx leftSchema leftNeeded left @ go nodeCtx rightSchema rightNeeded right
            | Logical.Derived plan ->
                let innerOutput = outputOfMemo plan.Context plan.Input memo plan.Tree
                let innerNeeded = needed |> Set.filter (fun i -> i < innerOutput.Columns.Length)
                go plan.Context plan.Input innerNeeded plan.Tree
            | Logical.UnionAll plans ->
                plans
                |> List.collect (fun plan ->
                    go
                        plan.Context
                        plan.Input
                        (allColumns (outputOfMemo plan.Context plan.Input memo plan.Tree))
                        plan.Tree)
            | Logical.Values _ -> []

        let entries =
            go ctx inSchema (allColumns (outputOfMemo ctx inSchema memo tree)) tree
            |> List.map (fun (key, mask) -> System.Collections.Generic.KeyValuePair<obj, bool array>(key, mask))

        System.Collections.Immutable.ImmutableDictionary.CreateRange<obj, bool array>(HashIdentity.Reference, entries)

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
                    OpsJoin.innerJoinHashBuildLeft
                        ctx.Budget
                        frame.LeftSchema
                        left
                        keys.LeftIndices
                        frame.RightSchema
                        right
                        keys.RightIndices
                        keys.Residual
                | HashBuildRight ->
                    OpsJoin.innerJoinHash
                        ctx.Budget
                        frame.LeftSchema
                        left
                        keys.LeftIndices
                        frame.RightSchema
                        right
                        keys.RightIndices
                        keys.Residual
            | JoinKind.Left ->
                OpsJoin.leftJoinHash
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
                OpsJoin.innerJoinMerge
                    ctx.Budget
                    frame.LeftSchema
                    left
                    keys.LeftIndices
                    frame.RightSchema
                    right
                    keys.RightIndices
                    keys.Residual
            | JoinKind.Left ->
                OpsJoin.leftJoinMerge
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

    /// 論理木を 1ノードずつ処理する。
    /// 継続は ExecFrame list に積むので、ラッパの深さに依存せずスタックは一定。
    [<TailCall>]
    let rec private run (ctx: ExecContext) (memo: OutputSchemaMemo) (frames: ExecFrame list) (step: ExecStep) =
        let buildJoinFrame inSchema node kind name on left right =
            let leftSchema = outputOfMemo ctx inSchema memo left

            let rightSchema, rightTable =
                match stripWrappers right with
                | Logical.Derived plan -> plan.Output, None
                | Logical.Join _ -> outputOfMemo ctx inSchema memo right, None
                | _ ->
                    let table = defaultSchema ctx name
                    Schema.ofTable name None table, Some table

            let statsOf name = ExecContext.statsOf name ctx

            let algorithm, keys, leftKeyIndices =
                Analyzer.chooseJoinAlgorithmWith statsOf (tableOf ctx) leftSchema rightSchema rightTable left right on

            let frame: JoinFrame =
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

        let execIndexScan ctx name indexName mask build =
            result {
                let! scanMask, fetchMask = ExecContext.indexAccessMasks ctx name indexName mask
                let! op = build scanMask

                if fetchMask |> Array.exists id then
                    return! ExecContext.fetchMissing ctx name fetchMask op
                else
                    return op
            }

        let execScan node ctx name method =
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
                    execIndexScan ctx name indexName mask (fun scanMask ->
                        ExecContext.byIndexMasked ctx name indexName values limit scanMask)
                | ScanMethod.RangeScan(_, startKey, endKey) ->
                    ExecContext.rangeScanMasked ctx name startKey endKey limit mask
                | ScanMethod.IndexRangeScan(indexName, lower, upper) ->
                    execIndexScan ctx name indexName mask (fun scanMask ->
                        ExecContext.byIndexRangeMasked ctx name indexName lower upper limit scanMask)

        let addClamp a b =
            if a > System.Int32.MaxValue - b then
                System.Int32.MaxValue
            else
                a + b

        let childLimitHint offset count hint =
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

        let lookup frame indexName scanMask fetchMask keyValues =
            result {
                let! op =
                    ExecContext.byIndexMasked ctx frame.RightName indexName keyValues 0 scanMask

                if fetchMask |> Array.exists id then
                    return! ExecContext.fetchMissing ctx frame.RightName fetchMask op
                else
                    return op
            }

        let rec rightScanOf =
            function
            | Logical.Scan _ as scan -> Some scan
            | Logical.Filter(_, child) -> rightScanOf child
            | _ -> None

        let joinWith frame left keys lookupRows =
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

        let execIndexJoin frame left =
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
                    joinWith frame left keys (fun keyValues ->
                        ExecContext.byPrimaryKeyMasked ctx frame.RightName keyValues rightMask)
                | JoinAlgorithm.IndexNestedLoop(JoinLookup.Index indexName) ->
                    result {
                        let! scanMask, fetchMask =
                            ExecContext.indexAccessMasks ctx frame.RightName indexName rightMask

                        return! joinWith frame left keys (lookup frame indexName scanMask fetchMask)
                    }
                | _ -> err (DbError.internalError "index nested loop ではありません")

        match step with
        | EvalNode(node, inSchema, hint) ->
            match node with
            | Logical.Join(kind, name, on, lTree, rTree) ->
                let frame, leftSchema = buildJoinFrame inSchema node kind name on lTree rTree

                match frame.Algorithm with
                | JoinAlgorithm.IndexNestedLoop _ ->
                    run ctx memo (JoinIndexLeftFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
                | _ -> run ctx memo (JoinRightFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
            | Logical.Values values -> run ctx memo frames (ApplyOp(Op.ofRow values))
            | Logical.Derived plan ->
                let innerCtx =
                    { plan.Context with
                        ScanNeeds = ctx.ScanNeeds
                    }

                run innerCtx memo (DerivedFrame ctx :: frames) (EvalNode(plan.Tree, plan.Input, None))
            | Logical.UnionAll [] -> run ctx memo frames (ApplyOp Op.Empty)
            | Logical.UnionAll(plan :: rest) ->
                let innerCtx =
                    { plan.Context with
                        ScanNeeds = ctx.ScanNeeds
                    }

                run innerCtx memo (UnionFrame(ctx, [], rest) :: frames) (EvalNode(plan.Tree, plan.Input, None))
            | Logical.Scan(name, method) ->
                match execScan node ctx name method hint with
                | Ok op -> run ctx memo frames (ApplyOp(countOutput ctx node op))
                | Error e -> err e
            | Logical.Filter(p, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child
                run ctx memo (FilterFrame(p, childSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Aggregate(groupBy, aggs, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child
                run ctx memo (AggregateFrame(groupBy, aggs, childSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Having(h, aggs, groupBy, child) ->
                run ctx memo (HavingFrame(h, aggs, groupBy) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Distinct child -> run ctx memo (DistinctFrame :: frames) (EvalNode(child, inSchema, None))
            | Logical.Sort(keys, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child

                let groupBy, aggs =
                    match aggregateOf child with
                    | Some(groupBy, aggs) -> groupBy, aggs
                    | None -> [], []

                run ctx memo (SortFrame(keys, childSchema, groupBy, aggs) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Limit(offset, limit, child) ->
                let childHint = childLimitHint offset limit hint
                run ctx memo (LimitFrame(offset, limit) :: frames) (EvalNode(child, inSchema, childHint))
            | Logical.Project(items, child) ->
                let childSchema = outputOfMemo ctx inSchema memo child
                let groupBy = groupByOf child

                run
                    ctx
                    memo
                    (ProjectFrame(items, inSchema, childSchema, groupBy) :: frames)
                    (EvalNode(child, inSchema, hint))
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
                    match Ops.aggregate ctx.Budget inSchema groupBy aggs op with
                    | Ok aggregated -> run ctx memo rest (ApplyOp aggregated)
                    | Error e -> err e
                | HavingFrame(h, aggs, groupBy) ->
                    match Ops.havingFilter groupBy aggs h with
                    | Error e -> err e
                    | Ok p -> run ctx memo rest (ApplyOp(Op.filter p op))
                | DistinctFrame -> run ctx memo rest (ApplyOp(Ops.distinct ctx.Budget op))
                | SortFrame(keys, childSchema, groupBy, aggs) ->
                    match Ops.sortBy ctx.Budget childSchema groupBy aggs keys op with
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
                    match execIndexJoin frame op with
                    | Ok joined -> run ctx memo rest (ApplyOp(countOutput ctx frame.Node joined))
                    | Error e -> err e
                | JoinRightFrame frame ->
                    run ctx memo (JoinApplyFrame(frame, op) :: rest) (EvalNode(frame.Right, frame.RightSchema, None))
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
                        (EvalNode(plan.Tree, plan.Input, None))

    /// 論理代数を Op に変換する。
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
    let exec (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        Spill.sweepStale ctx.Budget.Directory
        let memo = OutputSchemaMemo()

        let ctx =
            { ctx with
                ScanNeeds = scanNeedsMasks ctx schema memo tree
            }

        run ctx memo [] (EvalNode(tree, schema, None))

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

    /// 論理木を EXPLAIN 用の行にする（実行しない）。
    /// `exec` と同じ規則で走査方法・結合アルゴリズムを選び、木を深さ優先で並べる。
    /// `actual` が Some を返したノードには実測行数（`EXPLAIN ANALYZE`）を添える。
    let explainWithCounts (actual: Logical -> int64 option) (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        let memo = OutputSchemaMemo()

        let ctx =
            { ctx with
                ScanNeeds = scanNeedsMasks ctx schema memo tree
            }

        let childItem indent tree schema context =
            {
                Indent = indent
                Tree = tree
                Schema = schema
                Context = context
            }

        let childOf item tree schema =
            childItem (item.Indent + 1) tree schema item.Context

        let valueText = SqlLiteral.toSqlLiteral

        let valuesText = Array.map valueText >> String.concat ", "

        let itemText item =
            let expression = Expr.toDisplayString item.Expression

            match item.Alias with
            | Some alias -> sprintf "%s AS %s" expression alias
            | None -> expression

        let rangeText lower upper =
            let upper = upper |> Option.map valuesText |> Option.defaultValue "*"
            sprintf "%s..%s" (valuesText lower) upper

        let scanText name =
            function
            | ScanMethod.SeqScan -> sprintf "%s, SeqScan" name
            | ScanMethod.PkLookup values -> sprintf "%s, PkLookup(%s)" name (valuesText values)
            | ScanMethod.IndexLookup(index, values) -> sprintf "%s, IndexLookup(%s, %s)" name index (valuesText values)
            | ScanMethod.RangeScan(_, lower, upper) -> sprintf "%s, RangeScan(%s)" name (rangeText lower upper)
            | ScanMethod.IndexRangeScan(index, lower, upper) ->
                sprintf "%s, IndexRangeScan(%s, %s)" name index (rangeText lower upper)

        let algorithmText =
            function
            | JoinAlgorithm.NestedLoop -> "NestedLoop"
            | JoinAlgorithm.HashJoin -> "HashJoin"
            | JoinAlgorithm.MergeJoin -> "MergeJoin"
            | JoinAlgorithm.IndexNestedLoop JoinLookup.PrimaryKey -> "IndexNestedLoop(pk)"
            | JoinAlgorithm.IndexNestedLoop(JoinLookup.Index index) -> sprintf "IndexNestedLoop(%s)" index

        let joinKindText =
            function
            | JoinKind.Inner -> "INNER"
            | JoinKind.Left -> "LEFT"

        let aggText (f: AggFunc, star, distinct, arg) =
            let body = if star then "*" else Expr.toDisplayString arg
            sprintf "%s(%s%s)" f.Name (if distinct then "DISTINCT " else "") body

        let sortText k =
            let direction =
                match k.Direction with
                | SortDirection.Ascending -> "ASC"
                | SortDirection.Descending -> "DESC"

            sprintf "%s %s" (Expr.toDisplayString k.Expression) direction

        let describe item =
            match item.Tree with
            | Logical.Scan(name, method) ->
                let fetchSuffix =
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

                sprintf "Scan(%s)%s" (scanText name method) fetchSuffix, []
            | Logical.Values values ->
                if Array.isEmpty values then
                    "Values", []
                else
                    sprintf "Values(%s)" (valuesText values), []
            | Logical.Filter(predicate, child) ->
                sprintf "Filter(%s)" (Expr.toDisplayString predicate), [ childOf item child item.Schema ]
            | Logical.Project(items, child) ->
                let text = items |> List.map itemText |> String.concat ", "
                sprintf "Project(%s)" text, [ childOf item child item.Schema ]
            | Logical.Join(kind, name, on, left, right) ->
                let leftSchema = outputOfMemo item.Context item.Schema memo left

                let rightSchema, rightTable =
                    match stripWrappers right with
                    | Logical.Derived plan -> plan.Output, None
                    | Logical.Join _ -> outputOfMemo item.Context item.Schema memo right, None
                    | _ ->
                        let table = defaultSchema item.Context name
                        Schema.ofTable name None table, Some table

                let statsOf name = ExecContext.statsOf name item.Context

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
                    let buildSide = hashBuildSide statsOf algorithm kind left right

                    match algorithm, buildSide with
                    | JoinAlgorithm.HashJoin, HashBuildLeft -> "HashJoin(left)"
                    | JoinAlgorithm.HashJoin, HashBuildRight ->
                        let leftRows = Analyzer.estimateRowsWith statsOf left
                        let rightRows = Analyzer.estimateRowsWith statsOf right
                        let buildRows = min leftRows rightRows

                        let buildRowBytes =
                            if leftRows < rightRows then
                                Analyzer.rowBytesOfSchema leftSchema
                            else
                                Analyzer.rowBytesOfSchema rightSchema

                        let spilled =
                            Analyzer.spilledBuildRows
                                buildRows
                                buildRowBytes
                                (if item.Context.Budget.Work >= 0L then
                                     float item.Context.Budget.Work
                                 else
                                     -1.0)

                        if spilled > 0.0 then
                            sprintf "HashJoin(spill=%.0fKB)" (spilled * buildRowBytes / 1024.0)
                        else
                            "HashJoin"

                    | _ -> algorithmText algorithm

                let text =
                    sprintf
                        "Join(%s, %s, right=%s, on=%s)"
                        (joinKindText kind)
                        algorithmTextValue
                        name
                        (Expr.toDisplayString on)

                text, [ childOf item left leftSchema; childOf item right rightSchema ]
            | Logical.Aggregate(groupBy, aggs, child) ->
                let text =
                    sprintf
                        "Aggregate(group by [%s], aggregates [%s])"
                        (groupBy |> List.map Expr.toDisplayString |> String.concat ", ")
                        (aggs |> List.map aggText |> String.concat ", ")

                text, [ childOf item child item.Schema ]
            | Logical.Having(predicate, _, _, child) ->
                sprintf "Having(%s)" (Expr.toDisplayString predicate), [ childOf item child item.Schema ]
            | Logical.Distinct child -> "Distinct", [ childOf item child item.Schema ]
            | Logical.Sort(keys, child) ->
                let text = keys |> List.map sortText |> String.concat ", "
                sprintf "Sort(%s)" text, [ childOf item child item.Schema ]
            | Logical.Limit(offset, limit, child) ->
                let text =
                    if limit >= 0 then
                        sprintf "Limit(offset=%d, count=%d)" offset limit
                    else
                        sprintf "Limit(offset=%d)" offset

                text, [ childOf item child item.Schema ]
            | Logical.Derived plan -> "Derived", [ childItem (item.Indent + 1) plan.Tree plan.Input plan.Context ]
            | Logical.UnionAll plans ->
                "UnionAll",
                plans
                |> List.map (fun plan -> childItem (item.Indent + 1) plan.Tree plan.Input plan.Context)

        let rec emit acc =
            function
            | [] -> List.rev acc
            | item :: rest ->
                let text, children = describe item

                let rowSuffix =
                    if List.isEmpty item.Context.Catalog.Stats then
                        ""
                    else
                        let statsOf name = ExecContext.statsOf name item.Context
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
                emit (line :: acc) (children @ rest)

        emit
            []
            [
                {
                    Indent = 0
                    Tree = tree
                    Schema = schema
                    Context = ctx
                }
            ]

    /// 論理木を EXPLAIN 用の行にする（実行しない。実測行数は添えない）。
    let explain (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        explainWithCounts (fun _ -> None) ctx tree schema
