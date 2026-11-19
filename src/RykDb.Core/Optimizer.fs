// ===========================================================================
// Optimizer — 論理代数を pull 演算子に変換して実行する（`run` / `exec` / `explain`）。
// ===========================================================================

namespace RykDb.Core

module internal Optimizer =
    /// Join の結合待ち情報。
    /// 右部分木の Op は左部分木を評価した後に作る
    /// （評価順を left → right → join に保つため）。
    type private JoinFrame =
        {
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
        }

    /// 評価待ちの処理。
    /// Result を消費してから次へ進む処理をクロージャではなく値として積む。
    type private ExecFrame =
        | FilterFrame of Expr * Schema
        | AggregateFrame of Expr list * (AggFunc * bool * bool * Expr) list * Schema
        | HavingFrame of Expr * (AggFunc * bool * bool * Expr) list * Expr list
        | DistinctFrame
        | SortFrame of SortKey list * Schema
        | LimitFrame of int * int
        | ProjectFrame of SelectItem list * Schema * Expr list
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

    let private rightSchemaOf (ctx: ExecContext) (name: string) (right: Logical) =
        match stripWrappers right with
        | Logical.Derived plan -> plan.Output
        | _ -> Schema.ofTable name None (defaultSchema ctx name)

    /// 左スパインを降りて、下端のノードと右側のスキーマを集める。
    [<TailCall>]
    let rec private spine (ctx: ExecContext) (acc: Schema list) (tree: Logical) =
        match stripWrappers tree with
        | Logical.Join(_, name, _, left, right) -> spine ctx (rightSchemaOf ctx name right :: acc) left
        | other -> other, acc

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

    let private outputOf (ctx: ExecContext) (inSchema: Schema) (tree: Logical) =
        let leaf, rights = spine ctx [] tree

        let baseSchema =
            match leaf with
            | Logical.Aggregate(groupBy, aggs, _) -> aggregateSchema groupBy aggs
            | Logical.Derived plan -> plan.Output
            | Logical.UnionAll(plan :: _) -> plan.Output
            | Logical.Scan(name, _) -> Schema.ofTable name None (defaultSchema ctx name)
            | Logical.Values _ -> Schema.empty
            | _ -> inSchema

        List.fold Schema.concat baseSchema rights

    /// 出力列の必要集合を後方伝播し、各 Scan の列マスクを求める。
    /// `Logical` は参照等価の `obj` キーで覚える（構造比較はできない）。
    let private scanNeedsMasks (ctx: ExecContext) (inSchema: Schema) (tree: Logical) =
        let allColumns schema =
            if schema.Columns.Length = 0 then
                Set.empty
            else
                Set.ofSeq [ 0 .. schema.Columns.Length - 1 ]

        let resolveRef schema r =
            match Schema.resolve schema r with
            | Ok i -> Some i
            | Error _ ->
                match Schema.resolve schema { Qualifier = None; Name = r.Name } with
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
                let childSchema = outputOf nodeCtx nodeInput child
                go nodeCtx nodeInput (Set.union needed (refsOf childSchema p)) child
            | Logical.Project(items, child) ->
                let childSchema = outputOf nodeCtx nodeInput child
                let projection = SelectProjection.create childSchema items
                go nodeCtx nodeInput (refsOfAll childSchema projection.Expressions) child
            | Logical.Sort(keys, child) ->
                let childSchema = outputOf nodeCtx nodeInput child
                let keyExprs = keys |> List.map _.Expression
                go nodeCtx nodeInput (Set.union needed (refsOfAll childSchema keyExprs)) child
            | Logical.Aggregate(groupBy, aggs, child) ->
                let childSchema = outputOf nodeCtx nodeInput child

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
                let childSchema = outputOf nodeCtx nodeInput child
                go nodeCtx nodeInput (allColumns childSchema) child
            | Logical.Limit(_, _, child) -> go nodeCtx nodeInput needed child
            | Logical.Join(_, _, on, left, right) ->
                let leftSchema = outputOf nodeCtx nodeInput left
                let rightSchema = outputOf nodeCtx nodeInput right
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
                let innerOutput = outputOf plan.Context plan.Input plan.Tree
                let innerNeeded = needed |> Set.filter (fun i -> i < innerOutput.Columns.Length)
                go plan.Context plan.Input innerNeeded plan.Tree
            | Logical.UnionAll plans ->
                plans
                |> List.collect (fun plan ->
                    go plan.Context plan.Input (allColumns (outputOf plan.Context plan.Input plan.Tree)) plan.Tree)
            | Logical.Values _ -> []

        let entries =
            go ctx inSchema (allColumns (outputOf ctx inSchema tree)) tree
            |> List.map (fun (key, mask) -> System.Collections.Generic.KeyValuePair<obj, bool array>(key, mask))

        System.Collections.Immutable.ImmutableDictionary.CreateRange<obj, bool array>(HashIdentity.Reference, entries)

    let private groupByOf (tree: Logical) =
        match stripWrappers tree with
        | Logical.Aggregate(groupBy, _, _) -> groupBy
        | _ -> []

    /// 論理木を 1ノードずつ処理する。
    /// 継続は ExecFrame list に積むので、ラッパの深さに依存せずスタックは一定。
    [<TailCall>]
    let rec private run (ctx: ExecContext) (frames: ExecFrame list) (step: ExecStep) =
        let buildJoinFrame inSchema kind name on left right =
            let leftSchema = outputOf ctx inSchema left

            let rightSchema, rightTable =
                match stripWrappers right with
                | Logical.Derived plan -> plan.Output, None
                | _ ->
                    let table = defaultSchema ctx name
                    Schema.ofTable name None table, Some table

            let algorithm, keys, leftKeyIndices =
                Analyzer.chooseJoinAlgorithm (tableOf ctx) leftSchema rightSchema rightTable left right on

            let frame: JoinFrame =
                {
                    Kind = kind
                    Right = right
                    On = on
                    LeftSchema = leftSchema
                    RightSchema = rightSchema
                    RightName = name
                    Algorithm = algorithm
                    Keys = keys
                    LeftKeyIndices = defaultArg leftKeyIndices [||]
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
                | _ -> Array.create 0 true

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

        let execIndexJoin frame left =
            match frame.Keys with
            | None -> err (DbError.internalError "index nested loop に結合キーがありません")
            | Some keys ->
                let indexName =
                    match frame.Algorithm with
                    | JoinAlgorithm.IndexNestedLoop indexName -> indexName
                    | _ -> ""

                let rightMask =
                    match frame.Right with
                    | Logical.Scan _ as scan ->
                        match ctx.ScanNeeds.TryGetValue(box scan) with
                        | true, mask -> mask
                        | _ -> Array.create frame.RightSchema.Columns.Length true
                    | _ -> Array.create frame.RightSchema.Columns.Length true

                result {
                    let! scanMask, fetchMask =
                        ExecContext.indexAccessMasks ctx frame.RightName indexName rightMask

                    return
                        match frame.Kind with
                        | JoinKind.Inner ->
                            OpsJoin.innerJoinIndexed
                                frame.LeftSchema
                                left
                                frame.LeftKeyIndices
                                frame.RightSchema
                                keys.Residual
                                (lookup frame indexName scanMask fetchMask)
                        | JoinKind.Left ->
                            OpsJoin.leftJoinIndexed
                                frame.LeftSchema
                                left
                                frame.LeftKeyIndices
                                frame.RightSchema
                                keys.Residual
                                (lookup frame indexName scanMask fetchMask)
                }

        let execJoin frame left right =
            match frame.Algorithm with
            | JoinAlgorithm.NestedLoop ->
                match frame.Kind with
                | JoinKind.Inner -> OpsJoin.innerJoin ctx.Budget frame.LeftSchema left frame.RightSchema right frame.On
                | JoinKind.Left -> OpsJoin.leftJoin ctx.Budget frame.LeftSchema left frame.RightSchema right frame.On
            | JoinAlgorithm.HashJoin ->
                match frame.Keys with
                | None -> err (DbError.internalError "ハッシュ結合に結合キーがありません")
                | Some keys ->
                    match frame.Kind with
                    | JoinKind.Inner ->
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
            | JoinAlgorithm.IndexNestedLoop _ ->
                DbError.internalError "index nested loop は JoinIndexLeftFrame で実行します" |> err
            | JoinAlgorithm.MergeJoin ->
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

        match step with
        | EvalNode(node, inSchema, hint) ->
            match node with
            | Logical.Join(kind, name, on, lTree, rTree) ->
                let frame, leftSchema = buildJoinFrame inSchema kind name on lTree rTree

                match frame.Algorithm with
                | JoinAlgorithm.IndexNestedLoop _ ->
                    run ctx (JoinIndexLeftFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
                | _ -> run ctx (JoinRightFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
            | Logical.Values values -> run ctx frames (ApplyOp(Op.ofRow values))
            | Logical.Derived plan ->
                let innerCtx =
                    { plan.Context with
                        ScanNeeds = ctx.ScanNeeds
                    }

                run innerCtx (DerivedFrame ctx :: frames) (EvalNode(plan.Tree, plan.Input, None))
            | Logical.UnionAll [] -> run ctx frames (ApplyOp Op.Empty)
            | Logical.UnionAll(plan :: rest) ->
                let innerCtx =
                    { plan.Context with
                        ScanNeeds = ctx.ScanNeeds
                    }

                run innerCtx (UnionFrame(ctx, [], rest) :: frames) (EvalNode(plan.Tree, plan.Input, None))
            | Logical.Scan(name, method) as node ->
                match execScan node ctx name method hint with
                | Error e -> err e
                | Ok op -> run ctx frames (ApplyOp op)
            | Logical.Filter(p, child) ->
                let childSchema = outputOf ctx inSchema child
                run ctx (FilterFrame(p, childSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Aggregate(groupBy, aggs, child) ->
                let childSchema = outputOf ctx inSchema child
                run ctx (AggregateFrame(groupBy, aggs, childSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Having(h, aggs, groupBy, child) ->
                run ctx (HavingFrame(h, aggs, groupBy) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Distinct child -> run ctx (DistinctFrame :: frames) (EvalNode(child, inSchema, None))
            | Logical.Sort(keys, child) ->
                let childSchema = outputOf ctx inSchema child
                run ctx (SortFrame(keys, childSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Limit(offset, limit, child) ->
                let childHint = childLimitHint offset limit hint
                run ctx (LimitFrame(offset, limit) :: frames) (EvalNode(child, inSchema, childHint))
            | Logical.Project(items, child) ->
                let childSchema = outputOf ctx inSchema child
                let groupBy = groupByOf child
                run ctx (ProjectFrame(items, childSchema, groupBy) :: frames) (EvalNode(child, inSchema, hint))
        | ApplyOp op ->
            match frames with
            | [] -> ret op
            | frame :: rest ->
                match frame with
                | DerivedFrame outer -> run outer rest (ApplyOp op)
                | FilterFrame(p, childSchema) -> run ctx rest (ApplyOp(Ops.filter childSchema p op))
                | AggregateFrame(groupBy, aggs, inSchema) ->
                    match Ops.aggregate ctx.Budget inSchema groupBy aggs op with
                    | Error e -> err e
                    | Ok aggregated -> run ctx rest (ApplyOp aggregated)
                | HavingFrame(h, aggs, groupBy) ->
                    let p = Ops.havingFilter groupBy aggs h
                    run ctx rest (ApplyOp(Op.filter p op))
                | DistinctFrame -> run ctx rest (ApplyOp(Ops.distinct ctx.Budget op))
                | SortFrame(keys, childSchema) ->
                    match Ops.sortBy ctx.Budget childSchema keys op with
                    | Error e -> err e
                    | Ok sorted -> run ctx rest (ApplyOp sorted)
                | LimitFrame(offset, limit) ->
                    let skipped = if offset > 0 then Op.skip offset op else op
                    let limited = if limit >= 0 then Op.take limit skipped else skipped
                    run ctx rest (ApplyOp limited)
                | ProjectFrame(items, childSchema, groupBy) ->
                    run ctx rest (ApplyOp(Ops.project childSchema groupBy items op))
                | JoinIndexLeftFrame frame ->
                    match execIndexJoin frame op with
                    | Error e -> err e
                    | Ok joined -> run ctx rest (ApplyOp joined)
                | JoinRightFrame frame ->
                    run ctx (JoinApplyFrame(frame, op) :: rest) (EvalNode(frame.Right, frame.RightSchema, None))
                | JoinApplyFrame(frame, leftOp) ->
                    match execJoin frame leftOp op with
                    | Error e -> err e
                    | Ok joined -> run ctx rest (ApplyOp joined)
                | UnionFrame(outer, doneOps, []) ->
                    let combined = List.foldBack Op.append (List.rev (op :: doneOps)) Op.Empty
                    run outer rest (ApplyOp combined)
                | UnionFrame(outer, doneOps, plan :: plans) ->
                    let innerCtx =
                        { plan.Context with
                            ScanNeeds = outer.ScanNeeds
                        }

                    run
                        innerCtx
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
        let ctx =
            { ctx with
                ScanNeeds = scanNeedsMasks ctx schema tree
            }

        run ctx [] (EvalNode(tree, schema, None))

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
    let explain (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        let ctx =
            { ctx with
                ScanNeeds = scanNeedsMasks ctx schema tree
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
            | JoinAlgorithm.IndexNestedLoop index -> sprintf "IndexNestedLoop(%s)" index

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
                let leftSchema = outputOf item.Context item.Schema left

                let rightSchema, rightTable =
                    match stripWrappers right with
                    | Logical.Derived plan -> plan.Output, None
                    | _ ->
                        let table = defaultSchema item.Context name
                        Schema.ofTable name None table, Some table

                let algorithm, _, _ =
                    Analyzer.chooseJoinAlgorithm (tableOf item.Context) leftSchema rightSchema rightTable left right on

                let text =
                    sprintf
                        "Join(%s, %s, right=%s, on=%s)"
                        (joinKindText kind)
                        (algorithmText algorithm)
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
                let line = String.replicate item.Indent "  " + text
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
