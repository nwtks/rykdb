// ===========================================================================
// Planner —  論理代数を pull 演算子に変換する。
// ===========================================================================

namespace RykDb.Core

module internal Optimizer =
    /// 走査が主キー順（キー昇順）に結果を返すか。
    /// `SeqScan` / 主キー等値 / 主キー範囲は主キー順に返る。
    /// 索引経由の走査は索引順なので対象外（`WHERE` の索引選択で順序が変わる）。
    let private scansInPrimaryKeyOrder (tree: Logical) =
        let rec scanOf tree =
            match tree with
            | Logical.Filter(_, child) -> scanOf child
            | Logical.Scan(_, ScanMethod.SeqScan) -> true
            | Logical.Scan(_, ScanMethod.PkLookup _) -> true
            | Logical.Scan(_, ScanMethod.RangeScan _) -> true
            | _ -> false

        scanOf tree

    /// ORDER BY を省略できるか。
    /// <remarks>
    /// 単一テーブル・集約なし・DISTINCT なしで、ORDER BY の先頭が
    /// **主キー全列を定義順・昇順**で並べているなら、走査結果がそのまま順序になる。
    /// 主キーは一意なので、後続のキーは順序に影響しない。
    /// 主キーの一部だけ（複合主キーの 1列目だけ）では並びが決まらないので対象外。
    /// 索引走査（`IndexLookup` / `IndexRangeScan`）は索引順なので対象外。
    /// </remarks>
    let private sortIsRedundant
        (ctx: ExecContext)
        (stmt: SelectStatement)
        (schema: Schema)
        (keys: SortKey list)
        (tree: Logical)
        =
        let primaryKey =
            match stmt.From with
            | Some r when List.isEmpty stmt.Joins && not stmt.Distinct ->
                Catalog.findTable ctx.Catalog r.Name
                |> Option.bind (fun defn ->
                    match Catalog.toSchema defn with
                    | Ok table -> Some table.PrimaryKey
                    | Error _ -> None)
            | _ -> None

        let resolved =
            keys
            |> List.map (fun k ->
                match Schema.tryAsColumnRef k.Expression with
                | Some c ->
                    match Schema.resolve schema c with
                    | Ok i -> Some(i, k.Direction)
                    | Error _ -> None
                | None -> None)

        match primaryKey with
        | Some pk when
            not (List.isEmpty pk)
            && scansInPrimaryKeyOrder tree
            && List.length keys >= pk.Length
            && not (resolved |> List.exists Option.isNone)
            ->
            let prefix = resolved |> List.choose id |> List.truncate pk.Length

            (prefix |> List.map fst) = pk
            && prefix
               |> List.forall (fun (_, direction) -> direction = SortDirection.Ascending)
        | _ -> false

    /// SELECT 文を論理代数に変換する。
    let buildLogical (ctx: ExecContext) (stmt: SelectStatement) =
        let bindTables ctx stmt =
            result {
                let! baseBindings =
                    match stmt.From with
                    | None -> ret []
                    | Some r -> Analyzer.bindTable ctx r |> Result.map (fun b -> [ b ])

                let! bindings =
                    stmt.Joins
                    |> List.fold
                        (fun acc j ->
                            result {
                                let! bs = acc
                                let! b = Analyzer.bindTable ctx j.Table
                                return bs @ [ b ]
                            })
                        (ret baseBindings)

                return bindings
            }

        let concatSchemas = List.map Analyzer.schemaOf >> Schema.concatAll

        let buildScan catalog allSchema b predicates =
            let name = Analyzer.refName b

            let local =
                predicates
                |> List.filter (fun p -> Binder.referencesOnly name p && not (Binder.hasAmbiguousColumn allSchema p))

            result {
                let! method = Analyzer.chooseScanMethod b catalog local
                let scan = Logical.Scan(name, method)

                match Predicate.joinConjuncts local with
                | None -> return scan
                | Some p -> return Logical.Filter(p, scan)
            }

        let joinStep (bindings: TableBinding list) predicates accState (i, j) =
            result {
                let! acc, topPreds = accState
                let idx = i + 1
                let right = bindings.[idx]
                let leftSchema = bindings |> List.take idx |> concatSchemas
                let allSchema = concatSchemas bindings
                let onSchema = Schema.concat leftSchema (Analyzer.schemaOf right)
                let! on = Binder.qualify onSchema j.On
                let rightName = Analyzer.refName right

                let rightPreds =
                    predicates
                    |> List.filter (fun p ->
                        Binder.referencesOnly rightName p && not (Binder.hasAmbiguousColumn allSchema p))

                match j.Kind with
                | JoinKind.Inner ->
                    let! rightScan = buildScan ctx.Catalog allSchema right rightPreds
                    return Logical.Join(j.Kind, rightName, on, acc, rightScan), topPreds
                | JoinKind.Left ->
                    let! rightScan = buildScan ctx.Catalog allSchema right []
                    return Logical.Join(j.Kind, rightName, on, acc, rightScan), topPreds @ rightPreds
            }

        let localToAny allSchema bindings p =
            not (Binder.hasAmbiguousColumn allSchema p)
            && bindings |> List.exists (fun b -> Binder.referencesOnly (Analyzer.refName b) p)

        let applyAggregate schema stmt tree =
            let aliases = Alias.dropCyclic (Alias.ofItems stmt.Items)

            let selectExprs =
                stmt.Items
                |> List.map (fun item -> item.Expression)
                |> List.filter (fun e ->
                    match e with
                    | Expr.Star -> false
                    | Expr.QualifiedStar _ -> false
                    | _ -> true)

            result {
                let! orderExprs =
                    stmt.OrderBy
                    |> List.map (fun k -> Alias.substitute schema aliases k.Expression)
                    |> all

                let! qs = Binder.qualifyMany schema (selectExprs @ orderExprs)
                let selectAggs = Ops.aggCalls (qs |> List.take (List.length selectExprs))
                let orderAggs = orderExprs |> List.map (fun e -> Ops.aggCalls [ e ]) |> List.concat

                let! having, havingAggs =
                    match stmt.Having with
                    | None -> ret (None, [])
                    | Some h ->
                        result {
                            let! h' = Alias.substitute schema aliases h
                            let! hs = Binder.qualifyMany schema [ h' ]

                            match hs with
                            | [ hv ] -> return Some hv, Ops.aggCalls [ hv ]
                            | _ -> return! err (DbError.internalError "HAVING の修飾に失敗")
                        }

                let! groupBy =
                    let groupAliases = Alias.withoutAggregates aliases

                    stmt.GroupBy
                    |> List.map (Alias.substitute schema groupAliases)
                    |> all
                    >>= Binder.qualifyMany schema

                let allAggs = selectAggs @ orderAggs @ havingAggs
                let hasAgg = not (List.isEmpty allAggs)
                let hasGroupBy = not (List.isEmpty groupBy)

                let hasStar =
                    stmt.Items.IsEmpty
                    || stmt.Items
                       |> List.exists (fun item ->
                           match item.Expression with
                           | Expr.Star
                           | Expr.QualifiedStar _ -> true
                           | _ -> false)

                if (hasAgg || hasGroupBy) && hasStar then
                    return!
                        DbError.create ErrorKind.SyntaxError "GROUP BY / 集約関数を使うときは SELECT * を書けません（列を明示してください）"
                        |> err
                elif not (hasAgg || hasGroupBy) then
                    match having with
                    | Some _ -> return! DbError.create ErrorKind.SyntaxError "HAVING には GROUP BY か集約関数が必要です" |> err
                    | None -> return tree
                else
                    let aggNode = Logical.Aggregate(groupBy, allAggs, tree)

                    match having with
                    | None -> return aggNode
                    | Some hv -> return Logical.Having(hv, allAggs, groupBy, aggNode)
            }

        let expandItem schema item =
            match item.Expression with
            | Expr.Star
            | Expr.QualifiedStar _ ->
                match Schema.expandStar schema item.Expression with
                | Some(_, cols) ->
                    cols
                    |> List.map (fun c ->
                        Expr.Column
                            {
                                Qualifier = c.Qualifier
                                Name = c.Name
                            })
                | None -> [ item.Expression ]
            | e -> [ e ]

        let ordinalExpr outputExprs itemCount =
            function
            | Expr.Literal(Literal.Int n) ->
                let idx = int n - 1

                if n < 1L || idx >= itemCount then
                    sprintf "ORDER BY の列番号 %d は範囲外です" n
                    |> DbError.create ErrorKind.SyntaxError
                    |> err
                else
                    match List.tryItem idx outputExprs with
                    | None -> err (DbError.internalError "ORDER BY の列番号が解決できません")
                    | Some item -> ret item
            | e -> ret e

        let applySort schema stmt tree =
            if List.isEmpty stmt.OrderBy then
                ret tree
            else
                let aliases = Alias.dropCyclic (Alias.ofItems stmt.Items)
                let outputExprs = stmt.Items |> List.collect (expandItem schema)
                let itemCount = List.length outputExprs

                result {
                    let! exprs =
                        stmt.OrderBy
                        |> List.map (fun k ->
                            ordinalExpr outputExprs itemCount k.Expression
                            >>= Alias.substitute schema aliases)
                        |> all

                    let! qualified = Binder.qualifyMany schema exprs

                    let keys =
                        List.map2 (fun e (k: SortKey) -> { k with Expression = e }) qualified stmt.OrderBy

                    if sortIsRedundant ctx stmt schema keys tree then
                        return tree
                    else
                        return Logical.Sort(keys, tree)
                }

        let qualifyItems schema =
            List.map (fun item ->
                Binder.qualify schema item.Expression
                |> Result.map (fun e -> { item with Expression = e }))
            >> all

        let validateStars schema =
            stmt.Items
            |> List.tryPick (fun item ->
                match item.Expression with
                | Expr.QualifiedStar q when not (Schema.hasQualifier schema q) ->
                    Some(sprintf "テーブルまたは別名 '%s' が FROM にありません" q |> DbError.name)
                | _ -> None)
            |> function
                | Some e -> err e
                | None -> ret ()

        let evalCount what =
            function
            | Expr.Literal(Literal.Int i) when i >= 0L && i <= int64 System.Int32.MaxValue -> ret (int i)
            | e ->
                Eval.eval Schema.empty [||] e
                >>= function
                    | SqlValue.Int i when i >= 0L && i <= int64 System.Int32.MaxValue -> ret (int i)
                    | SqlValue.Int _ ->
                        sprintf "%s は 0 以上 %d 以下の整数で指定してください" what System.Int32.MaxValue
                        |> DbError.create ErrorKind.SyntaxError
                        |> err
                    | _ ->
                        sprintf "%s は 0 以上の整数で指定してください" what
                        |> DbError.create ErrorKind.SyntaxError
                        |> err

        let applyLimit stmt tree =
            match stmt.Limit, stmt.Offset with
            | None, None -> ret tree
            | _ ->
                result {
                    let! limit =
                        match stmt.Limit with
                        | None -> ret -1
                        | Some e -> evalCount "LIMIT" e

                    let! offset =
                        match stmt.Offset with
                        | None -> ret 0
                        | Some e -> evalCount "OFFSET" e

                    return Logical.Limit(offset, limit, tree)
                }

        result {
            let! bindings = bindTables ctx stmt

            if List.isEmpty bindings then
                return Logical.Values [||]
            else
                let allSchema = concatSchemas bindings
                let! () = validateStars allSchema

                let! predicates =
                    match stmt.Where with
                    | None -> ret []
                    | Some w -> Binder.qualify allSchema w |> Result.map Predicate.splitConjuncts

                let head = List.head bindings
                let! firstTree = buildScan ctx.Catalog allSchema head predicates
                let initial = ret (firstTree, [])

                let! joined, topPreds =
                    List.fold (joinStep bindings predicates) initial (List.indexed stmt.Joins)

                let cross =
                    predicates |> List.filter (fun p -> not (localToAny allSchema bindings p))

                let filtered =
                    match Predicate.joinConjuncts (topPreds @ cross) with
                    | None -> joined
                    | Some p -> Logical.Filter(p, joined)

                let! withAgg = applyAggregate allSchema stmt filtered
                let! withSorted = applySort allSchema stmt withAgg
                let! items = qualifyItems allSchema stmt.Items
                let projected = Logical.Project(items, withSorted)

                let deduped =
                    if stmt.Distinct then
                        Logical.Distinct projected
                    else
                        projected

                return! applyLimit stmt deduped
        }

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
        /// 左部分木を評価し終えた Join。次に右部分木を評価する。
        | JoinRightFrame of JoinFrame
        /// 左部分木を評価し終えた Index Nested Loop。右部分木は評価しない。
        | JoinIndexLeftFrame of JoinFrame
        /// 右部分木も評価し終えた Join。左 Op を保持している。
        | JoinApplyFrame of JoinFrame * Op

    /// 評価機の状態。
    /// 木を 1つ降りるか、Op が上がってきたか。
    type private ExecStep =
        /// 第 3要素はストレージへ降ろせる件数ヒント（Some 0 は「0行」）。
        | EvalNode of Logical * Schema * int option
        | ApplyOp of Op

    /// 論理木を 1ノードずつ処理する。
    /// 継続は ExecFrame list に積むので、ラッパの深さに依存せずスタックは一定。
    [<TailCall>]
    let rec private run (ctx: ExecContext) (frames: ExecFrame list) (step: ExecStep) =
        let defaultSchema ctx name =
            match ExecContext.schemaOf name ctx with
            | Ok s -> s
            | Error _ ->
                {
                    Name = name
                    Columns = []
                    PrimaryKey = []
                    Indexes = []
                }

        let tableOf (name: string) =
            match ExecContext.schemaOf name ctx with
            | Ok s -> Some s
            | Error _ -> None

        let execScan ctx hint name method =
            match hint with
            | Some 0 -> ret Op.empty
            | _ ->
                let limit = defaultArg hint 0

                match method with
                | ScanMethod.SeqScan -> ExecContext.table ctx name limit
                | ScanMethod.PkLookup values -> ExecContext.byPrimaryKey ctx name values
                | ScanMethod.IndexLookup(indexName, values) -> ExecContext.byIndex ctx name indexName values limit
                | ScanMethod.RangeScan(_, startKey, endKey) -> ExecContext.rangeScan ctx name startKey endKey limit
                | ScanMethod.IndexRangeScan(indexName, lower, upper) ->
                    ExecContext.byIndexRange ctx name indexName lower upper limit

        let rec stripWrappers =
            function
            | Logical.Sort(_, child) -> stripWrappers child
            | Logical.Limit(_, _, child) -> stripWrappers child
            | Logical.Distinct child -> stripWrappers child
            | Logical.Having(_, _, _, child) -> stripWrappers child
            | Logical.Filter(_, child) -> stripWrappers child
            | Logical.Project(_, child) -> stripWrappers child
            | other -> other

        let outputOf inSchema tree =
            match stripWrappers tree with
            | Logical.Aggregate(groupBy, aggs, _) ->
                let gv =
                    groupBy
                    |> List.map (fun e ->
                        {
                            Name = Expr.toDisplayString e
                            Qualifier = None
                            DataType = None
                        })

                let av =
                    aggs
                    |> List.map (fun (f, star, _, arg) ->
                        let suffix = if star then "*" else Expr.toDisplayString arg

                        {
                            Name = sprintf "%s(%s)" f.Name suffix
                            Qualifier = None
                            DataType = None
                        })

                { Schema.empty with
                    Columns = Array.append (List.toArray gv) (List.toArray av)
                }
            | _ -> inSchema

        let groupByOf tree =
            match stripWrappers tree with
            | Logical.Aggregate(groupBy, _, _) -> groupBy
            | _ -> []

        let addClamp a b =
            if a > System.Int32.MaxValue - b then
                System.Int32.MaxValue
            else
                a + b

        let execJoin left right frame =
            match frame.Algorithm with
            | JoinAlgorithm.NestedLoop ->
                match frame.Kind with
                | JoinKind.Inner -> Ops.innerJoin frame.LeftSchema left frame.RightSchema right frame.On
                | JoinKind.Left -> Ops.leftJoin frame.LeftSchema left frame.RightSchema right frame.On
            | JoinAlgorithm.HashJoin ->
                let keys = frame.Keys.Value

                match frame.Kind with
                | JoinKind.Inner ->
                    Ops.innerJoinHash
                        frame.LeftSchema
                        left
                        keys.LeftIndices
                        frame.RightSchema
                        right
                        keys.RightIndices
                        keys.Residual
                | JoinKind.Left ->
                    Ops.leftJoinHash
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
                let keys = frame.Keys.Value

                match frame.Kind with
                | JoinKind.Inner ->
                    Ops.innerJoinMerge
                        frame.LeftSchema
                        left
                        keys.LeftIndices
                        frame.RightSchema
                        right
                        keys.RightIndices
                        keys.Residual
                | JoinKind.Left ->
                    Ops.leftJoinMerge
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
                let rightTable = defaultSchema ctx name
                let rightSchema = Schema.ofTable name None rightTable

                let leftSchema =
                    { inSchema with
                        Columns =
                            inSchema.Columns
                            |> Array.filter (fun c ->
                                match c.Qualifier with
                                | Some q -> not (Catalog.eqName q name)
                                | None -> true)
                        Tables = inSchema.Tables |> List.filter (fun t -> not (Catalog.eqName t name))
                    }

                let algorithm, keys, leftKeyIndices =
                    Analyzer.chooseJoinAlgorithm tableOf leftSchema rightSchema rightTable lTree rTree on

                let frame =
                    {
                        Kind = kind
                        Right = rTree
                        On = on
                        LeftSchema = leftSchema
                        RightSchema = rightSchema
                        RightName = name
                        Algorithm = algorithm
                        Keys = keys
                        LeftKeyIndices = defaultArg leftKeyIndices [||]
                    }

                match algorithm with
                | JoinAlgorithm.IndexNestedLoop _ ->
                    run ctx (JoinIndexLeftFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
                | _ -> run ctx (JoinRightFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
            | Logical.Values values -> run ctx frames (ApplyOp(Op.ofRow values))
            | Logical.Scan(name, method) ->
                match execScan ctx hint name method with
                | Error e -> err e
                | Ok op -> run ctx frames (ApplyOp op)
            | Logical.Filter(p, child) ->
                let childSchema = outputOf inSchema child
                run ctx (FilterFrame(p, childSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Aggregate(groupBy, aggs, child) ->
                run ctx (AggregateFrame(groupBy, aggs, inSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Having(h, aggs, groupBy, child) ->
                run ctx (HavingFrame(h, aggs, groupBy) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Distinct child -> run ctx (DistinctFrame :: frames) (EvalNode(child, inSchema, None))
            | Logical.Sort(keys, child) ->
                let childSchema = outputOf inSchema child
                run ctx (SortFrame(keys, childSchema) :: frames) (EvalNode(child, inSchema, None))
            | Logical.Limit(offset, limit, child) ->
                let childHint =
                    if limit = 0 then
                        Some 0
                    else
                        match limit with
                        | l when l >= 0 ->
                            let inner = addClamp offset l

                            Some(
                                match hint with
                                | Some h -> min h inner
                                | None -> inner
                            )
                        | _ -> hint |> Option.map (fun h -> addClamp offset h)

                run ctx (LimitFrame(offset, limit) :: frames) (EvalNode(child, inSchema, childHint))
            | Logical.Project(items, child) ->
                let childSchema = outputOf inSchema child
                let groupBy = groupByOf child
                run ctx (ProjectFrame(items, childSchema, groupBy) :: frames) (EvalNode(child, inSchema, hint))
        | ApplyOp op ->
            match frames with
            | [] -> ret op
            | frame :: rest ->
                match frame with
                | FilterFrame(p, childSchema) -> run ctx rest (ApplyOp(Ops.filter childSchema p op))
                | AggregateFrame(groupBy, aggs, inSchema) ->
                    match Ops.aggregate inSchema groupBy aggs op with
                    | Error e -> err e
                    | Ok aggregated -> run ctx rest (ApplyOp aggregated)
                | HavingFrame(h, aggs, groupBy) ->
                    let p = Ops.havingFilter groupBy aggs h
                    run ctx rest (ApplyOp(Op.filter p op))
                | DistinctFrame -> run ctx rest (ApplyOp(Ops.distinct op))
                | SortFrame(keys, childSchema) ->
                    match Ops.sortBy childSchema keys op with
                    | Error e -> err e
                    | Ok sorted -> run ctx rest (ApplyOp sorted)
                | LimitFrame(offset, limit) ->
                    let skipped = if offset > 0 then Op.skip offset op else op
                    let limited = if limit >= 0 then Op.take limit skipped else skipped
                    run ctx rest (ApplyOp limited)
                | ProjectFrame(items, childSchema, groupBy) ->
                    run ctx rest (ApplyOp(Ops.project childSchema groupBy items op))
                | JoinIndexLeftFrame frame ->
                    let keys = frame.Keys.Value

                    let indexName =
                        match frame.Algorithm with
                        | JoinAlgorithm.IndexNestedLoop indexName -> indexName
                        | _ -> ""

                    let lookup keyValues =
                        ExecContext.byIndex ctx frame.RightName indexName keyValues 0

                    let joined =
                        match frame.Kind with
                        | JoinKind.Inner ->
                            Ops.innerJoinIndexed
                                frame.LeftSchema
                                op
                                frame.LeftKeyIndices
                                frame.RightSchema
                                lookup
                                keys.Residual
                        | JoinKind.Left ->
                            Ops.leftJoinIndexed
                                frame.LeftSchema
                                op
                                frame.LeftKeyIndices
                                frame.RightSchema
                                lookup
                                keys.Residual

                    run ctx rest (ApplyOp joined)
                | JoinRightFrame frame ->
                    run ctx (JoinApplyFrame(frame, op) :: rest) (EvalNode(frame.Right, frame.RightSchema, None))
                | JoinApplyFrame(frame, leftOp) ->
                    match execJoin leftOp op frame with
                    | Error e -> err e
                    | Ok joined -> run ctx rest (ApplyOp joined)

    /// 論理代数を Op に落とす。
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
        run ctx [] (EvalNode(tree, schema, None))

module internal Plan =
    /// SELECT 文を実行して、結果（列名 + 行）を返す。
    let runSelect (ctx: ExecContext) (stmt: SelectStatement) =
        let registerAll =
            foldOk
                (fun (c, acc) (r: TableRef) ->
                    result {
                        let realName = r.Name
                        let! schema = ExecContext.schemaOf realName c
                        let c1 = ExecContext.withSchema realName schema c

                        let c2 =
                            match r.Alias with
                            | None -> c1
                            | Some a -> ExecContext.withSchema a schema c1

                        return c2, Schema.concat acc (Schema.ofTable realName r.Alias schema)
                    })
                (ctx, Schema.empty)

        let sources =
            (match stmt.From with
             | None -> []
             | Some r -> [ r ])
            @ (stmt.Joins |> List.map _.Table)

        let duplicate =
            sources
            |> List.countBy (fun r -> defaultArg r.Alias r.Name)
            |> List.tryFind (fun (_, c) -> c > 1)

        match duplicate with
        | Some(name, _) -> sprintf "テーブル %s が重複しています（エイリアスを付けてください）" name |> DbError.name |> err
        | None ->
            result {
                let! ctx2, schema = registerAll sources
                let! tree = Optimizer.buildLogical ctx2 stmt
                let! op = Optimizer.exec ctx2 tree schema
                let! rows = Op.toArray op
                let header = Schema.projectHeader schema stmt.Items
                return header, rows
            }
