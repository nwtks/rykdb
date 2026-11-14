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
    /// 単一テーブルを主キー順に走査し、集約・DISTINCT・JOIN がない場合に判定する。
    /// `ORDER BY` の先頭が主キー全列を定義順・昇順で含むなら、走査結果はすでに必要な順序である。
    /// 主キーは一意なので、その後ろにある `ORDER BY` 項目は順序を変えない。
    /// 複合主キーの一部だけでは全行の順序が決まらず、索引走査も主キー順を保証しないため対象外。
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
            | Some(TableSource.Named r) when List.isEmpty stmt.Joins && not stmt.Distinct ->
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

            prefix |> List.map fst = pk
            && prefix
               |> List.forall (fun (_, direction) -> direction = SortDirection.Ascending)
        | _ -> false

    /// FROM / JOIN の束縛。実テーブルと派生テーブル（FROM のサブクエリ）を区別する。
    type private SourceBinding =
        | Base of TableBinding
        | DerivedSource of TableRef * PlannedQuery

    /// 束縛の参照名（別名があれば別名）。
    let private bindingName =
        function
        | Base b -> Analyzer.refName b
        | DerivedSource(r, _) -> r.Name

    /// 束縛の出力スキーマ。
    let private bindingSchema =
        function
        | Base b -> Analyzer.schemaOf b
        | DerivedSource(_, plan) -> plan.Output

    /// SELECT 文を論理代数に変換する。
    /// `derived` は計画済みの派生テーブル（別名の小文字 -> 計画）。
    let buildLogicalWith (ctx: ExecContext) (stmt: SelectStatement) (derived: Map<string, PlannedQuery>) =
        let bindSource (r: TableRef) =
            match derived |> Map.tryFind (r.Name.ToLowerInvariant()) with
            | Some plan -> ret (DerivedSource(r, plan))
            | None -> Analyzer.bindTable ctx r |> Result.map Base

        let bindTables stmt =
            result {
                let! baseBindings =
                    match stmt.From with
                    | None -> ret []
                    | Some(TableSource.Named r) -> bindSource r |> Result.map (fun b -> [ b ])
                    | Some(TableSource.Derived(_, alias)) ->
                        bindSource { Name = alias; Alias = None } |> Result.map (fun b -> [ b ])

                let! bindings =
                    stmt.Joins
                    |> List.fold
                        (fun acc j ->
                            result {
                                let! bs = acc

                                let! b =
                                    match j.Table with
                                    | TableSource.Named r -> bindSource r
                                    | TableSource.Derived(_, alias) -> bindSource { Name = alias; Alias = None }

                                return bs @ [ b ]
                            })
                        (ret baseBindings)

                return bindings
            }

        let concatSchemas = List.map bindingSchema >> Schema.concatAll

        let lowerName b = (bindingName b).ToLowerInvariant()

        let localPreds name =
            List.choose (fun (p, refs) ->
                match refs with
                | Some s when Set.count s = 1 && Catalog.eqName (Set.minElement s) name -> Some p
                | _ -> None)

        let validateStars schema =
            stmt.Items
            |> List.tryPick (fun item ->
                match item.Expression with
                | Expr.Star when Array.isEmpty schema.Columns ->
                    Some(DbError.create ErrorKind.SyntaxError "FROM のない SELECT では * を使えません（列を明示してください）")
                | Expr.QualifiedStar q when not (Schema.hasQualifier schema q) ->
                    Some(sprintf "テーブルまたは別名 '%s' が FROM にありません" q |> DbError.name)
                | _ -> None)
            |> function
                | Some e -> err e
                | None -> ret ()

        let referencedTables p =
            let hasBareName =
                Expr.fold
                    (fun found e ->
                        found
                        || match e with
                           | Expr.Name _ -> true
                           | _ -> false)
                    false
                    p

            let allQualified =
                Expr.columnRefs p
                |> List.forall (fun (c: ColumnRef) -> Option.isSome c.Qualifier)

            if hasBareName || not allQualified || Expr.hasCorrelated p then
                None
            else
                Expr.columnRefs p
                |> List.choose (fun c -> c.Qualifier)
                |> List.map (fun q -> q.ToLowerInvariant())
                |> Set.ofList
                |> Some

        let buildScan catalog b local =
            let withPredicates node =
                match Predicate.joinConjuncts local with
                | None -> ret node
                | Some p -> ret (Logical.Filter(p, node))

            match b with
            | DerivedSource(_, plan) -> withPredicates (Logical.Derived plan)
            | Base tb ->
                result {
                    let! method = Analyzer.chooseScanMethod tb catalog local
                    return! withPredicates (Logical.Scan(bindingName b, method))
                }

        let estimateRows tree =
            let rec unwrap count t =
                match t with
                | Logical.Filter(_, child) -> unwrap (count + 1) child
                | _ -> count, t

            let filters, leaf = unwrap 0 tree

            let baseRows =
                match leaf with
                | Logical.Scan(_, ScanMethod.PkLookup _) -> 1.0
                | Logical.Scan(_, ScanMethod.IndexLookup _) -> 2.0
                | Logical.Scan(_, ScanMethod.RangeScan _)
                | Logical.Scan(_, ScanMethod.IndexRangeScan _) -> 100.0
                | _ -> 1000.0

            max 1.0 (baseRows * pown 0.1 (min filters 3))

        let buildJoin (bindings: SourceBinding list) (scans: Logical array) acc accSchema kind rightIndex on promoted =
            let right = bindings.[rightIndex]
            let rightSchema = bindingSchema right

            Binder.qualify (Schema.concat accSchema rightSchema) on
            |> Result.map (fun qualified ->
                let onAll =
                    match promoted with
                    | [] -> qualified
                    | ps -> Predicate.joinConjuncts (qualified :: ps) |> Option.defaultValue qualified

                Logical.Join(kind, bindingName right, onAll, acc, scans.[rightIndex]),
                Schema.concat accSchema rightSchema)

        let equiLinks e =
            Predicate.splitConjuncts e
            |> List.choose (fun conjunct ->
                match conjunct with
                | Expr.Binary(BinaryOp.Eq, a, b) ->
                    let qualifierOf x =
                        match Schema.tryAsColumnRef x with
                        | Some(r: ColumnRef) -> r.Qualifier
                        | None -> None

                    match qualifierOf a, qualifierOf b with
                    | Some qa, Some qb when not (Catalog.eqName qa qb) ->
                        Some(Set.ofList [ qa.ToLowerInvariant(); qb.ToLowerInvariant() ])
                    | _ -> None
                | _ -> None)

        let chooseOrder joined edges items dependencies =
            let eligible joined name =
                match dependencies |> Map.tryFind name with
                | None -> true
                | Some refs -> Set.isSubset refs (Set.add name joined)

            let connected joined name =
                edges
                |> List.exists (fun e -> Set.contains name e && not (Set.isEmpty (Set.intersect e joined)))

            let rec go joined remaining acc =
                match remaining with
                | [] -> List.rev acc
                | _ ->
                    let ready = remaining |> List.filter (fun (n, _, _) -> eligible joined n)
                    let pool = if List.isEmpty ready then remaining else ready
                    let linked, isolated = pool |> List.partition (fun (n, _, _) -> connected joined n)
                    let choice = if List.isEmpty linked then isolated else linked

                    let name =
                        choice
                        |> List.minBy (fun (_, rows, position) -> rows, position)
                        |> fun (n, _, _) -> n

                    go (Set.add name joined) (remaining |> List.filter (fun (n, _, _) -> n <> name)) (name :: acc)

            go joined items []

        let promoteAt joinPredicates joined used next =
            let take, used' =
                joinPredicates
                |> Array.indexed
                |> Array.fold
                    (fun (take, used) (k, (refs, p)) ->
                        if
                            not (Set.contains k used)
                            && Set.contains next refs
                            && Set.isSubset refs (Set.add next joined)
                        then
                            p :: take, Set.add k used
                        else
                            take, used)
                    ([], used)

            List.rev take, used'

        let rec buildSteps
            (bindings: SourceBinding list)
            scans
            (estimates: float array)
            joinPredicates
            acc
            accSchema
            joined
            used
            steps
            =
            match steps with
            | [] -> ret (acc, accSchema, used)
            | (index, clause) :: rest when clause.Kind = JoinKind.Left ->
                match buildJoin bindings scans acc accSchema JoinKind.Left index clause.On [] with
                | Error e -> err e
                | Ok(node, schema) ->
                    buildSteps
                        bindings
                        scans
                        estimates
                        joinPredicates
                        node
                        schema
                        (Set.add (lowerName bindings.[index]) joined)
                        used
                        rest
            | _ ->
                let inner = steps |> List.takeWhile (fun (_, j) -> j.Kind = JoinKind.Inner)
                let restSteps = steps |> List.skipWhile (fun (_, j) -> j.Kind = JoinKind.Inner)

                let segmentSchema =
                    inner
                    |> List.fold (fun schema (i, _) -> Schema.concat schema (bindingSchema bindings.[i])) accSchema

                let qualifiedOns =
                    inner
                    |> List.map (fun (i, j) ->
                        let qualified =
                            match Binder.qualify segmentSchema j.On with
                            | Ok e -> Some e
                            | Error _ -> None

                        i, j, qualified)

                let edges =
                    (qualifiedOns
                     |> List.collect (fun (_, _, q) -> q |> Option.map equiLinks |> Option.defaultValue []))
                    @ (joinPredicates |> Array.toList |> List.map fst)

                let dependencies =
                    qualifiedOns
                    |> List.map (fun (i, _, q) ->
                        q
                        |> Option.bind referencedTables
                        |> Option.map (fun refs -> lowerName bindings.[i], refs))

                let order =
                    if dependencies |> List.forall Option.isSome then
                        chooseOrder
                            joined
                            edges
                            (inner |> List.map (fun (i, _) -> lowerName bindings.[i], estimates.[i], i))
                            (dependencies |> List.choose id |> Map.ofList)
                    else
                        inner |> List.map (fun (i, _) -> lowerName bindings.[i])

                let byName =
                    inner |> List.map (fun (i, j) -> lowerName bindings.[i], (i, j)) |> Map.ofList

                let rec apply acc accSchema joined used order =
                    match order with
                    | [] -> ret (acc, accSchema, used)
                    | next :: tail ->
                        let index, clause = byName |> Map.find next
                        let promoted, used' = promoteAt joinPredicates joined used next

                        match buildJoin bindings scans acc accSchema JoinKind.Inner index clause.On promoted with
                        | Error e -> err e
                        | Ok(node, schema) -> apply node schema (Set.add next joined) used' tail

                match apply acc accSchema joined used order with
                | Error e -> err e
                | Ok(node, schema, used') ->
                    buildSteps
                        bindings
                        scans
                        estimates
                        joinPredicates
                        node
                        schema
                        (Set.union joined (Set.ofList (inner |> List.map (fun (i, _) -> lowerName bindings.[i]))))
                        used'
                        restSteps

        let aggregateTree schema stmt tree =
            let aliases = Alias.dropCyclic (Alias.ofItems stmt.Items)

            let selectExprs =
                stmt.Items
                |> List.map (fun item -> item.Expression)
                |> List.filter (function
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

                let correlatedInOutput =
                    selectExprs |> List.exists Expr.hasCorrelated
                    || orderExprs |> List.exists Expr.hasCorrelated
                    || match having with
                       | Some h -> Expr.hasCorrelated h
                       | None -> false

                if (hasAgg || hasGroupBy) && correlatedInOutput then
                    return!
                        "GROUP BY / 集約関数を使うクエリの SELECT / HAVING / ORDER BY では相関サブクエリを使えません"
                        |> DbError.create ErrorKind.SyntaxError
                        |> err
                elif (hasAgg || hasGroupBy) && hasStar then
                    return!
                        "GROUP BY / 集約関数を使うときは SELECT * を書けません（列を明示してください）"
                        |> DbError.create ErrorKind.SyntaxError
                        |> err
                elif not (hasAgg || hasGroupBy) then
                    match having with
                    | Some _ -> return! "HAVING には GROUP BY か集約関数が必要です" |> DbError.create ErrorKind.SyntaxError |> err
                    | None -> return tree
                else
                    let aggNode = Logical.Aggregate(groupBy, allAggs, tree)

                    match having with
                    | None -> return aggNode
                    | Some hv -> return Logical.Having(hv, allAggs, groupBy, aggNode)
            }

        let applyAggregate schema stmt tree =
            let hasAggregateOrGroup =
                not (List.isEmpty stmt.GroupBy)
                || stmt.Having.IsSome
                || stmt.Items |> List.exists (fun item -> Expr.isAggregate item.Expression)
                || stmt.OrderBy |> List.exists (fun k -> Expr.isAggregate k.Expression)

            if not hasAggregateOrGroup then
                ret tree
            else
                aggregateTree schema stmt tree

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

        let evalCount what =
            function
            | Expr.Literal(Literal.Int i) when i >= 0L && i <= int64 System.Int32.MaxValue -> ret (int i)
            | e when Expr.hasCorrelated e ->
                sprintf "%s に相関サブクエリは使えません" what |> DbError.create ErrorKind.SyntaxError |> err
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
            let! bindings = bindTables stmt

            if stmt.From.IsNone && not (List.isEmpty stmt.Joins) then
                return! DbError.create ErrorKind.SyntaxError "JOIN には FROM が必要です" |> err
            else
                let allSchema = concatSchemas bindings
                let! () = validateStars allSchema

                let! predicates =
                    match stmt.Where with
                    | None -> ret []
                    | Some w -> Binder.qualify allSchema w |> Result.map Predicate.splitConjuncts

                let bindingNames = bindings |> List.map lowerName |> Set.ofList

                let classified =
                    predicates
                    |> List.map (fun p ->
                        let refs =
                            referencedTables p |> Option.filter (fun s -> Set.isSubset s bindingNames)

                        p, refs)

                let joinPredicates =
                    classified
                    |> List.choose (fun (p, refs) ->
                        match refs with
                        | Some s when Set.count s >= 2 -> Some(s, p)
                        | _ -> None)

                let constantPreds =
                    classified
                    |> List.choose (fun (p, refs) ->
                        match refs with
                        | Some s when Set.isEmpty s -> Some p
                        | _ -> None)

                let unmovablePreds =
                    classified
                    |> List.choose (fun (p, refs) ->
                        match refs with
                        | None -> Some p
                        | _ -> None)

                let leftRightTables =
                    stmt.Joins
                    |> List.indexed
                    |> List.choose (fun (i, j) ->
                        if j.Kind = JoinKind.Left then
                            Some(lowerName bindings.[i + 1])
                        else
                            None)
                    |> Set.ofList

                let leftRightPreds =
                    bindings
                    |> List.filter (fun b -> Set.contains (lowerName b) leftRightTables)
                    |> List.collect (fun b -> localPreds (bindingName b) classified)

                let! scans =
                    bindings
                    |> List.mapi (fun i b ->
                        let preds =
                            if Set.contains (lowerName b) leftRightTables then
                                []
                            elif i = 0 then
                                localPreds (bindingName b) classified @ constantPreds
                            else
                                localPreds (bindingName b) classified

                        buildScan ctx.Catalog b preds)
                    |> all

                let scans' = List.toArray scans
                let estimates = scans' |> Array.map estimateRows
                let joinPredicates' = List.toArray joinPredicates

                let! joined, usedJoinPredicates =
                    match bindings with
                    | [] -> ret (Logical.Values [||], Set.empty)
                    | first :: _ ->
                        stmt.Joins
                        |> List.indexed
                        |> List.map (fun (i, j) -> i + 1, j)
                        |> buildSteps
                            bindings
                            scans'
                            estimates
                            joinPredicates'
                            scans'.[0]
                            (bindingSchema first)
                            (Set.singleton (lowerName first))
                            Set.empty
                        |> Result.map (fun (tree, _, used) -> tree, used)

                let remaining =
                    unmovablePreds
                    @ leftRightPreds
                    @ (joinPredicates'
                       |> Array.indexed
                       |> Array.choose (fun (k, (_, p)) -> if Set.contains k usedJoinPredicates then None else Some p)
                       |> Array.toList)
                    @ if List.isEmpty bindings then constantPreds else []

                let filtered =
                    match Predicate.joinConjuncts remaining with
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
        /// 派生テーブルの内側を評価し終えた。外側の実行文脈へ戻す。
        | DerivedFrame of ExecContext
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
            | Logical.Scan(name, _) -> Schema.ofTable name None (defaultSchema ctx name)
            | Logical.Values _ -> Schema.empty
            | _ -> inSchema

        List.fold Schema.concat baseSchema rights

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

        let execScan ctx name method =
            function
            | Some 0 -> ret Op.Empty
            | hint ->
                let limit = defaultArg hint 0

                match method with
                | ScanMethod.SeqScan -> ExecContext.table ctx name limit
                | ScanMethod.PkLookup values -> ExecContext.byPrimaryKey ctx name values
                | ScanMethod.IndexLookup(indexName, values) -> ExecContext.byIndex ctx name indexName values limit
                | ScanMethod.RangeScan(_, startKey, endKey) -> ExecContext.rangeScan ctx name startKey endKey limit
                | ScanMethod.IndexRangeScan(indexName, lower, upper) ->
                    ExecContext.byIndexRange ctx name indexName lower upper limit

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

        let execIndexJoin frame left =
            let keys = frame.Keys.Value

            let indexName =
                match frame.Algorithm with
                | JoinAlgorithm.IndexNestedLoop indexName -> indexName
                | _ -> ""

            let lookup keyValues =
                ExecContext.byIndex ctx frame.RightName indexName keyValues 0

            match frame.Kind with
            | JoinKind.Inner ->
                Ops.innerJoinIndexed frame.LeftSchema left frame.LeftKeyIndices frame.RightSchema lookup keys.Residual
            | JoinKind.Left ->
                Ops.leftJoinIndexed frame.LeftSchema left frame.LeftKeyIndices frame.RightSchema lookup keys.Residual

        let execJoin frame left right =
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
                let frame, leftSchema = buildJoinFrame inSchema kind name on lTree rTree

                match frame.Algorithm with
                | JoinAlgorithm.IndexNestedLoop _ ->
                    run ctx (JoinIndexLeftFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
                | _ -> run ctx (JoinRightFrame frame :: frames) (EvalNode(lTree, leftSchema, None))
            | Logical.Values values -> run ctx frames (ApplyOp(Op.ofRow values))
            | Logical.Derived plan ->
                run plan.Context (DerivedFrame ctx :: frames) (EvalNode(plan.Tree, plan.Input, None))
            | Logical.Scan(name, method) ->
                match execScan ctx name method hint with
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
                | JoinIndexLeftFrame frame -> run ctx rest (ApplyOp(execIndexJoin frame op))
                | JoinRightFrame frame ->
                    run ctx (JoinApplyFrame(frame, op) :: rest) (EvalNode(frame.Right, frame.RightSchema, None))
                | JoinApplyFrame(frame, leftOp) ->
                    match execJoin frame leftOp op with
                    | Error e -> err e
                    | Ok joined -> run ctx rest (ApplyOp joined)

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
            | Logical.Scan(name, method) -> sprintf "Scan(%s)" (scanText name method), []
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

module internal Plan =
    /// IN サブクエリの結果を保持する上限（管理メモリの概算バイト）。
    /// `Ops` の JOIN と同じ考え方で、OOM ではなく `CapacityError` で止める。
    let private SubqueryBudgetBytes = 256L * 1024L * 1024L

    /// IN サブクエリの行を上限付きで集める。
    let private collectSubqueryRows =
        let rec go used acc cur =
            match Op.next cur with
            | Error e -> err e
            | Ok None -> ret (List.rev acc |> List.toArray)
            | Ok(Some(row, nxt)) ->
                let used = used + int64 (ValueCodec.managedRowBytes row) + 32L

                if used > SubqueryBudgetBytes then
                    sprintf "IN サブクエリの結果が多すぎます（概算 %d バイトの上限）" SubqueryBudgetBytes
                    |> DbError.capacity
                    |> err
                else
                    go used (row :: acc) nxt

        go 0L []

    /// 相関サブクエリの外側スコープ。
    /// 先頭が直近の外側（式を含むクエリの入力）で、後ろへ行くほど外側。
    type private OuterScope =
        {
            Schema: Schema
            /// 実行時に行が確定していないスコープ（計画中のクエリ自身）は None。
            Row: SqlValue array option
        }

    /// 修飾子が内側スコープのテーブルを指しているか。
    /// 内側に同名のテーブルがあるなら、列が無くても外側へはフォールバックしない（SQL のスコープ規則）。
    let private shadowedByInner (inner: Schema) (r: ColumnRef) =
        match r.Qualifier with
        | None -> false
        | Some q -> inner.Tables |> List.exists (Catalog.eqName q)

    /// 内側で解決できず、外側スコープに一致する列参照の位置（0 = 直近）を返す。
    let private outerScopeIndex (inner: Schema) (scopes: OuterScope list) (r: ColumnRef) =
        if Schema.resolve inner r |> Result.isOk || shadowedByInner inner r then
            None
        else
            let rec go i =
                function
                | [] -> None
                | s :: rest ->
                    match Schema.resolve s.Schema r with
                    | Ok _ -> Some i
                    | Error _ -> go (i + 1) rest

            go 0 scopes

    /// 論理木が直接参照する式を列挙する。
    /// 派生テーブルの内側は別スコープなので含めない。
    let private treeExprs (tree: Logical) =
        let childrenOf =
            function
            | Logical.Filter(e, child) -> [ e ], [ child ]
            | Logical.Project(items, child) -> items |> List.map _.Expression, [ child ]
            | Logical.Join(_, _, on, left, right) -> [ on ], [ left; right ]
            | Logical.Aggregate(groupBy, aggs, child) -> groupBy @ (aggs |> List.map (fun (_, _, _, e) -> e)), [ child ]
            | Logical.Having(h, aggs, groupBy, child) ->
                h :: (aggs |> List.map (fun (_, _, _, e) -> e)) @ groupBy, [ child ]
            | Logical.Distinct child -> [], [ child ]
            | Logical.Sort(keys, child) -> keys |> List.map _.Expression, [ child ]
            | Logical.Limit(_, _, child) -> [], [ child ]
            | Logical.Scan _
            | Logical.Values _
            | Logical.Derived _ -> [], []

        let rec go acc pending =
            match pending with
            | [] -> List.rev acc
            | t :: rest ->
                let exprs, kids = childrenOf t
                go (List.rev exprs @ acc) (kids @ rest)

        go [] [ tree ]

    /// 論理木が相関している外側スコープの位置（0 = 直近の外側）を集める。
    /// 入れ子の相関サブクエリは、その `Depths` のうち深さ 2 以上
    /// （このクエリより外側への依存）だけを引き継ぐ。
    let private outerRefs (inner: Schema) (scopes: OuterScope list) =
        let addExpr =
            Expr.fold (fun acc ->
                function
                | Expr.Column c ->
                    match outerScopeIndex inner scopes c with
                    | Some i -> i :: acc
                    | None -> acc
                | Expr.Name n ->
                    match outerScopeIndex inner scopes { Qualifier = None; Name = n } with
                    | Some i -> i :: acc
                    | None -> acc
                | Expr.CorrelatedSubquery c ->
                    c.Depths
                    |> List.filter (fun d -> d >= 2)
                    |> List.fold (fun acc d -> d - 2 :: acc) acc
                | _ -> acc)

        treeExprs >> List.fold addExpr [] >> List.distinct

    /// 外側参照を外側行の値へ置き換える（内側スコープが優先）。
    /// 解決できない参照はそのまま残し、評価時の `NameError` に委ねる。
    let private substituteOuter (inner: Schema) (scopes: OuterScope list) =
        let subst r =
            if Schema.resolve inner r |> Result.isOk || shadowedByInner inner r then
                None
            else
                let rec go =
                    function
                    | [] -> None
                    | s :: rest ->
                        match Schema.resolve s.Schema r with
                        | Ok i ->
                            match s.Row with
                            | Some row when i >= 0 && i < row.Length -> Some(Expr.Value row.[i])
                            | _ -> None
                        | Error _ -> go rest

                go scopes

        Expr.rewrite (fun x ->
            match x with
            | Expr.Column c -> defaultArg (subst c) x
            | Expr.Name n -> defaultArg (subst { Qualifier = None; Name = n }) x
            | _ -> x)

    /// 文の各式に外側参照の置換を適用する。
    /// 入れ子のサブクエリ（`Subquery` / `Exists`）の内部には入らない
    /// （そのスコープは実行時に別途解決する）。
    let private substituteStatement (inner: Schema) (scopes: OuterScope list) (stmt: SelectStatement) =
        let sub = substituteOuter inner scopes

        let subOpt =
            function
            | None -> ret None
            | Some e -> sub e |> Result.map Some

        result {
            let! items =
                stmt.Items
                |> List.map (fun i -> sub i.Expression |> Result.map (fun e -> { i with Expression = e }))
                |> all

            let! joins =
                stmt.Joins
                |> List.map (fun j -> sub j.On |> Result.map (fun e -> { j with On = e }))
                |> all

            let! whereE = subOpt stmt.Where
            let! groupBy = stmt.GroupBy |> List.map sub |> all
            let! having = subOpt stmt.Having

            let! orderBy =
                stmt.OrderBy
                |> List.map (fun k -> sub k.Expression |> Result.map (fun e -> { k with Expression = e }))
                |> all

            let! limit = subOpt stmt.Limit
            let! offset = subOpt stmt.Offset

            return
                { stmt with
                    Items = items
                    Joins = joins
                    Where = whereE
                    GroupBy = groupBy
                    Having = having
                    OrderBy = orderBy
                    Limit = limit
                    Offset = offset
                }
        }

    /// 現在のクエリの行を先頭に差し込んだスコープ列。
    let private withCurrentRow (row: SqlValue array) =
        function
        | s :: rest -> { s with Row = Some row } :: rest
        | [] -> []

    /// 相関サブクエリの依存深さ（1 = 直近の外側行）。
    let private depthsOf = List.map ((+) 1) >> List.sort

    /// CPS のリスト走査。
    /// 1件ずつ `f` に渡し、書き換え結果を積み直して最後に継続 `k` へ渡す。
    /// 継続呼び出しが末尾になるので、リスト長に依存せずスタックは積み上がらない。
    [<TailCall>]
    let rec private traverseC
        (f: 'a -> ('a -> Result<'r, DbError>) -> Result<'r, DbError>)
        (pending: 'a list)
        (acc: 'a list)
        (k: 'a list -> Result<'r, DbError>)
        : Result<'r, DbError> =
        match pending with
        | [] -> k (List.rev acc)
        | x :: rest -> f x (fun x2 -> traverseC f rest (x2 :: acc) k)

    /// FROM / JOIN の入力を `TableRef` に読み替える。
    /// `resolveSourcesC` が派生テーブルを実テーブル参照（別名）へ書き換えるので、
    /// ここに来る `Derived` は解決漏れ。
    let private sourceRef =
        function
        | TableSource.Named r -> r
        | TableSource.Derived(_, alias) -> { Name = alias; Alias = None }

    /// 派生テーブルの出力スキーマへ別名の修飾を付ける。
    let private withQualifier (alias: string) (schema: Schema) =
        {
            Columns = schema.Columns |> Array.map (fun c -> { c with Qualifier = Some alias })
            Tables = [ alias ]
        }

    /// 文の入力（FROM / JOIN）を実テーブルとして登録する。
    /// 派生テーブルはスキーマだけを連結する（カタログにも ctx.Schemas にも登録しない）。
    let private registerSources (ctx: ExecContext) (derived: Map<string, PlannedQuery>) =
        foldOk
            (fun (c, acc) (r: TableRef) ->
                match derived |> Map.tryFind (r.Name.ToLowerInvariant()) with
                | Some plan -> ret (c, Schema.concat acc plan.Output)
                | None ->
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

    /// FROM / JOIN の派生テーブルを計画する（CPS、実行はしない）。
    /// AST の `Derived` は実テーブル参照（別名）へ書き換え、
    /// 計画結果は別名の小文字をキーにした Map で返す。
    [<TailCall>]
    let rec private resolveSourcesC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (k: SelectStatement -> Map<string, PlannedQuery> -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<'r, DbError> =
        let rec go pending done' plans =
            match pending with
            | [] ->
                let rewritten = List.rev done'

                let fromSource, joinSources =
                    if Option.isSome stmt.From then
                        match rewritten with
                        | first :: rest -> Some first, rest
                        | [] -> None, []
                    else
                        None, rewritten

                let joins =
                    List.map2 (fun (j: JoinClause) t -> { j with Table = t }) stmt.Joins joinSources

                k
                    { stmt with
                        From = fromSource
                        Joins = joins
                    }
                    plans
            | TableSource.Named r :: rest -> go rest (TableSource.Named r :: done') plans
            | TableSource.Derived(sel, alias) :: rest ->
                planCoreCWith
                    runner
                    []
                    (fun plan ->
                        let plan =
                            { plan with
                                Output = withQualifier alias plan.Output
                            }

                        go
                            rest
                            (TableSource.Named { Name = alias; Alias = None } :: done')
                            (Map.add (alias.ToLowerInvariant()) plan plans))
                    ctx
                    sel

        let hasDerived =
            (match stmt.From with
             | Some(TableSource.Derived _) -> true
             | _ -> false)
            || (stmt.Joins
                |> List.exists (fun j ->
                    match j.Table with
                    | TableSource.Derived _ -> true
                    | _ -> false))

        if not hasDerived then
            k stmt Map.empty
        else
            let sources = (stmt.From |> Option.toList) @ (stmt.Joins |> List.map _.Table)
            go sources [] Map.empty

    /// 外側スコープ付きで SELECT を計画する（CPS）。実行はしない。
    /// `outer` は「この SELECT の外側」のスコープ列（先頭が直近）。
    /// `runner` は相関サブクエリを外側行ごとに再計画・実行する関数。
    and [<TailCall>] private planCoreCWith
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (outer: OuterScope list)
        (k: PlannedQuery -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<'r, DbError> =
        let originalItems = stmt.Items

        resolveSourcesC
            runner
            (fun rewritten derived ->
                let sources =
                    (match rewritten.From with
                     | None -> []
                     | Some r -> [ sourceRef r ])
                    @ (rewritten.Joins |> List.map (fun j -> sourceRef j.Table))

                let duplicate =
                    sources
                    |> List.countBy (fun r -> defaultArg r.Alias r.Name)
                    |> List.tryFind (fun (_, c) -> c > 1)

                match duplicate with
                | Some(name, _) -> sprintf "テーブル %s が重複しています（エイリアスを付けてください）" name |> DbError.name |> err
                | None ->
                    registerSources ctx derived sources
                    >>= fun (ctx2, inputSchema) ->
                        let scopes = { Schema = inputSchema; Row = None } :: outer

                        resolveSelectC
                            runner
                            scopes
                            (fun resolved ->
                                Optimizer.buildLogicalWith ctx2 resolved derived
                                >>= fun tree ->
                                    let projection = SelectProjection.create inputSchema originalItems

                                    k
                                        {
                                            Output =
                                                {
                                                    Columns = projection.ReferenceHeader
                                                    Tables = []
                                                }
                                            Header =
                                                {
                                                    Columns = projection.DisplayHeader
                                                    Tables = []
                                                }
                                            Input = inputSchema
                                            Context = ctx2
                                            Tree = tree
                                        })
                            ctx
                            rewritten)
            ctx
            stmt

    /// サブクエリを解決してから論理木を組んで実行する（CPS）。
    /// 文の各フィールドを継続の連鎖で解決する。
    /// フィールドの数は固定なので、この関数自身は再帰しない。
    /// `scopes` の先頭はこのクエリの入力スキーマ（行は実行時に確定するので None）。
    /// <remarks>
    /// `planCoreCWith` / `resolveSourcesC` / `resolveSelectC` / `resolveExprC` / `resolveLeafC` は相互再帰する。
    /// どれも「残りの処理」を継続として受け取り、末尾で呼ぶので、
    /// サブクエリ・派生テーブルの入れ子段数に依存せずスタックは積み上がらない
    /// （Parser の式・SELECT と同じ方針）。
    /// </remarks>
    and [<TailCall>] private resolveSelectC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (k: SelectStatement -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<'r, DbError> =
        let resolveOpt v cont =
            match v with
            | None -> cont None
            | Some e -> resolveExprC runner scopes (fun e2 -> cont (Some e2)) ctx e

        let resolveItems =
            traverseC (fun (item: SelectItem) cont ->
                resolveExprC runner scopes (fun e -> cont { item with Expression = e }) ctx item.Expression)

        let resolveJoins =
            traverseC (fun j cont -> resolveExprC runner scopes (fun e -> cont { j with On = e }) ctx j.On)

        let resolveSorts =
            traverseC (fun (s: SortKey) cont ->
                resolveExprC runner scopes (fun e -> cont { s with Expression = e }) ctx s.Expression)

        let resolveExprs = traverseC (fun e cont -> resolveExprC runner scopes cont ctx e)

        let finish items joins whereE groupBy having orderBy limit offset =
            k
                { stmt with
                    Items = items
                    Joins = joins
                    Where = whereE
                    GroupBy = groupBy
                    Having = having
                    OrderBy = orderBy
                    Limit = limit
                    Offset = offset
                }

        let hasSubquery =
            (stmt.Items |> List.exists (fun item -> Expr.hasSubquery item.Expression))
            || (stmt.Joins |> List.exists (fun j -> Expr.hasSubquery j.On))
            || (stmt.Where |> Option.exists Expr.hasSubquery)
            || (stmt.GroupBy |> List.exists Expr.hasSubquery)
            || (stmt.Having |> Option.exists Expr.hasSubquery)
            || (stmt.OrderBy |> List.exists (fun k -> Expr.hasSubquery k.Expression))
            || (stmt.Limit |> Option.exists Expr.hasSubquery)
            || (stmt.Offset |> Option.exists Expr.hasSubquery)

        if not hasSubquery then
            k stmt
        else
            resolveItems stmt.Items [] (fun items ->
                resolveJoins stmt.Joins [] (fun joins ->
                    resolveOpt stmt.Where (fun whereE ->
                        resolveExprs stmt.GroupBy [] (fun groupBy ->
                            resolveOpt stmt.Having (fun having ->
                                resolveSorts stmt.OrderBy [] (fun orderBy ->
                                    resolveOpt stmt.Limit (fun limit ->
                                        resolveOpt stmt.Offset (fun offset ->
                                            finish items joins whereE groupBy having orderBy limit offset))))))))

    /// 式の中のサブクエリを解決する（CPS）。
    and private resolveExprC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (k: Expr -> Result<'r, DbError>)
        (ctx: ExecContext)
        (e: Expr)
        : Result<'r, DbError> =
        if Expr.hasSubquery e then
            Expr.rewriteWithC (resolveLeafC runner scopes ctx) k e
        else
            k e

    /// 式の葉にあるサブクエリを解決して、値・リテラル・IN リストへ置き換える（CPS）。
    /// 外側の行を参照する（相関）サブクエリは、実行時に外側行を受け取る
    /// `CorrelatedSubquery` へ置き換える。
    /// `Expr.rewriteWithC` に渡すので、引数は `runner -> scopes -> ctx -> 式 -> 継続` の順。
    and [<TailCall>] private resolveLeafC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (ctx: ExecContext)
        (e: Expr)
        (k: Expr -> Result<'r, DbError>)
        : Result<'r, DbError> =
        let singleColumnMessage count =
            sprintf "サブクエリは 1列だけを返す必要があります（%d 列）" count |> DbError.typeError

        let twoRowsMessage = "スカラーサブクエリが 2行以上を返しました"

        match e with
        | Expr.Subquery sel ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    if plan.Header.Columns.Length <> 1 then
                        err (singleColumnMessage plan.Header.Columns.Length)
                    else
                        match outerRefs plan.Input scopes plan.Tree with
                        | [] ->
                            Optimizer.exec plan.Context plan.Tree plan.Input
                            |> Result.bind (fun op ->
                                match Op.next op with
                                | Error er -> err er
                                | Ok None -> k (Expr.Literal Literal.Null)
                                | Ok(Some(row, rest)) ->
                                    match Op.next rest with
                                    | Error er -> err er
                                    | Ok None -> k (Expr.Value row.[0])
                                    | Ok(Some _) -> DbError.constraintViolation twoRowsMessage |> err)
                        | refs ->
                            let run row =
                                let withRow = withCurrentRow row scopes

                                substituteStatement plan.Input withRow sel
                                |> Result.bind (runner withRow ctx)
                                |> Result.bind (fun (_, op) ->
                                    match Op.next op with
                                    | Error er -> err er
                                    | Ok None -> ret SqlValue.Null
                                    | Ok(Some(r, rest)) ->
                                        match Op.next rest with
                                        | Error er -> err er
                                        | Ok None -> ret r.[0]
                                        | Ok(Some _) -> DbError.constraintViolation twoRowsMessage |> err)

                            k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
                ctx
                sel
        | Expr.Exists sel ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    match outerRefs plan.Input scopes plan.Tree with
                    | [] ->
                        Optimizer.exec plan.Context plan.Tree plan.Input
                        |> Result.bind (fun op ->
                            match Op.next op with
                            | Error er -> err er
                            | Ok found -> k (Expr.Literal(Literal.Bool found.IsSome)))
                    | refs ->
                        let run row =
                            let withRow = withCurrentRow row scopes

                            substituteStatement plan.Input withRow sel
                            |> Result.bind (runner withRow ctx)
                            |> Result.bind (fun (_, op) ->
                                match Op.next op with
                                | Error er -> err er
                                | Ok found -> ret (SqlValue.ofBool found.IsSome))

                        k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
                ctx
                sel
        | Expr.InSubquery(left, sel, negated) ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    if plan.Header.Columns.Length <> 1 then
                        err (singleColumnMessage plan.Header.Columns.Length)
                    else
                        match outerRefs plan.Input scopes plan.Tree with
                        | [] ->
                            Optimizer.exec plan.Context plan.Tree plan.Input
                            |> Result.bind (fun op ->
                                match collectSubqueryRows op with
                                | Error er -> err er
                                | Ok rows ->
                                    let values = rows |> Array.map (fun row -> Expr.Value row.[0]) |> Array.toList
                                    k (Expr.InList(left, values, negated)))
                        | refs ->
                            let currentSchema =
                                match scopes with
                                | s :: _ -> s.Schema
                                | [] -> Schema.empty

                            let resolve = Eval.resolver currentSchema

                            let run row =
                                let withRow = withCurrentRow row scopes

                                substituteStatement plan.Input withRow sel
                                |> Result.bind (runner withRow ctx)
                                |> Result.bind (fun (_, op) ->
                                    collectSubqueryRows op
                                    |> Result.bind (fun rows ->
                                        let values = rows |> Array.map (fun r -> Expr.Value r.[0]) |> Array.toList
                                        Eval.evalWith resolve row (Expr.InList(left, values, negated))))

                            k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
                ctx
                sel
        | other -> k other

    /// サブクエリを解決してから論理木を組んで実行する（CPS）。
    let private planSelectCWith
        (outer: OuterScope list)
        (k: (Schema * Op) -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        : Result<'r, DbError> =
        planCoreCWith
            runner
            outer
            (fun plan ->
                Optimizer.exec plan.Context plan.Tree plan.Input
                >>= fun op -> k (plan.Header, op))
            ctx
            stmt

    /// 相関サブクエリを外側行ごとに再計画して実行する。
    /// 外側参照を値へ置換した文を渡すので、ここから先は通常のサブクエリ解決と同じ経路を通る。
    /// <remarks>
    /// 入れ子の相関サブクエリも同じ手順で実行するため、自分自身を runner として
    /// planner に渡す必要があり `let rec` にしている。
    /// 実際の再帰呼び出しは `Run` の末尾位置（`Result.bind` の継続）で起きるので、
    /// 外側行が増えてもフレームは積み上がらない。
    /// TailAudit は「関数値を引数として渡す形」を保守的に非末尾（`bare`）と数えるため、
    /// ベースラインに 1 件だけ登録している（`StorageEngine.scanAt` の `pull` と同じ既知パターン）。
    /// </remarks>
    let rec private runSubquery
        (outer: OuterScope list)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<Schema * Op, DbError> =
        runSubquery |> planSelectCWith outer ret ctx stmt

    /// DML（INSERT / UPDATE / DELETE）の式に含まれるサブクエリを解決する。
    /// `schema` は行ごとに評価するときの対象行のスキーマ
    /// （INSERT のように行がない文は `Schema.empty`）。
    let resolveDmlExprWith (schema: Schema) (ctx: ExecContext) =
        resolveExprC runSubquery [ { Schema = schema; Row = None } ] ret ctx

    /// DML の式に含まれるサブクエリを解決する（対象行のスキーマなし）。
    let resolveDmlExpr = resolveDmlExprWith Schema.empty

    /// 外側スコープなしで SELECT を計画・実行する（CPS）。
    let private planSelectC (k: (Schema * Op) -> Result<'r, DbError>) (ctx: ExecContext) (stmt: SelectStatement) =
        runSubquery |> planSelectCWith [] k ctx stmt

    /// SELECT 文を計画・実行して、結果（列名 + 未消費の Op）を返す。
    /// 行の配列化は呼び出し側（Engine）で行う。
    let runSelect =
        planSelectC (fun (schema, op) -> ret (schema.Columns |> Array.map (fun c -> c.Name), op))

    /// EXPLAIN 用に SELECT の論理計画だけを組み立てる（実行はしない）。
    /// サブクエリは計画時に解決するため、実際に評価されるものがある
    /// （通常の SELECT と同じ計画経路を通る）。
    let explainSelect (ctx: ExecContext) (stmt: SelectStatement) =
        planCoreCWith runSubquery [] ret ctx stmt
