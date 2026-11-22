// ===========================================================================
// PlanBuilder — SELECT の論理代数（Scan / Filter / Join / Aggregate …）を組み立てる。
// ===========================================================================

namespace RykDb.Core

module internal PlanBuilder =
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
                Catalog.findTable r.Name ctx.Catalog
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
                    match Schema.resolve c schema with
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

    /// 実テーブル束縛のスキーマ（派生テーブルは None）。
    let private bindingTableSchema =
        function
        | Base b -> Some b.Schema
        | DerivedSource _ -> None

    /// 等値述語の両側のテーブル名 -> 列名。
    let private equalityColumns =
        function
        | Expr.Binary(BinaryOp.Eq, a, b) ->
            match Schema.tryAsColumnRef a, Schema.tryAsColumnRef b with
            | Some ca, Some cb ->
                match ca.Qualifier, cb.Qualifier with
                | Some qa, Some qb when not (Catalog.eqName qa qb) ->
                    Map.ofList [ qa.ToLowerInvariant(), ca.Name; qb.ToLowerInvariant(), cb.Name ]
                | _ -> Map.empty
            | _ -> Map.empty
        | _ -> Map.empty

    /// 結合順の候補を作る。
    let private joinItem
        (statsOf: string -> TableStats option)
        (binding: SourceBinding)
        (scan: Logical)
        (rows: float)
        (position: int)
        =
        match binding with
        | Base tb ->
            let indexStats name =
                statsOf (bindingName binding)
                |> Option.bind (fun stats ->
                    stats.Indexes
                    |> List.tryFind (fun (i: IndexStats) -> Catalog.eqName i.Name name))
                |> Option.map _.PrefixNDV
                |> Option.defaultValue []

            {
                Name = (bindingName binding).ToLowerInvariant()
                Rows = rows
                Position = position
                Indexes =
                    (let pkColumns = TableSchema.primaryKeyColumns tb.Schema |> List.ofArray

                     if List.isEmpty pkColumns then
                         []
                     else
                         [
                             {
                                 IndexName = "__pk"
                                 IndexColumns = pkColumns
                                 PrefixNDV = []
                                 Unique = true
                             }
                         ])
                    @ (tb.Schema.Indexes
                       |> List.map (fun index ->
                           {
                               IndexName = index.Name
                               IndexColumns = index.Columns
                               PrefixNDV = indexStats index.Name
                               Unique = index.Unique
                           }))
                RowBytes = Analyzer.rowBytesOfTypes (tb.Schema.Columns |> Seq.map (fun c -> Some c.DataType))
                NaturalOrder = Analyzer.scanOrder (fun _ -> Some tb.Schema) scan
            }
        | DerivedSource(_, plan) ->
            {
                Name = (bindingName binding).ToLowerInvariant()
                Rows = rows
                Position = position
                Indexes = []
                RowBytes = Analyzer.rowBytesOfTypes (plan.Output.Columns |> Seq.map _.DataType)
                NaturalOrder = None
            }

    /// LIMIT / OFFSET の定数式を評価する（0 以上の int）。
    let private evalCount what =
        function
        | Expr.Literal(Literal.Int i) when i >= 0L && i <= int64 System.Int32.MaxValue -> ret (int i)
        | e when Expr.hasCorrelated e -> sprintf "%s に相関サブクエリは使えません" what |> DbError.create ErrorKind.SyntaxError |> err
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

    /// bushy 木の畳み込みに使う入力（束縛・走査・述語）。
    type private BushyEnv =
        {
            Bindings: SourceBinding list
            Scans: Logical array
            ByName: Map<string, int>
            OnPredicates: (int * Set<string> * Expr) array
            JoinPredicates: (Set<string> * Expr) array
        }

    /// bushy 木の畳み込み結果（部分木・出力スキーマ・名前集合・取り込み済み述語）。
    type private BushyNode =
        {
            Tree: Logical
            Schema: Schema
            Names: Set<string>
            UsedOn: Set<int>
            UsedWhere: Set<int>
        }

    /// 左右の部分木を結合し、両方を参照する述語を ON に取り込む。
    let private joinBushy (env: BushyEnv) (left: BushyNode) (right: BushyNode) (rightName: string) =
        let both = Set.union left.Names right.Names

        let crosses refs =
            Set.isSubset refs both
            && not (Set.isEmpty (Set.intersect refs left.Names))
            && not (Set.isEmpty (Set.intersect refs right.Names))

        let ons, usedOn =
            env.OnPredicates
            |> Array.fold
                (fun (take, used) (k, refs, e) ->
                    if not (Set.contains k used) && crosses refs then
                        e :: take, Set.add k used
                    else
                        take, used)
                ([], right.UsedOn)

        let wheres, usedWhere =
            env.JoinPredicates
            |> Array.indexed
            |> Array.fold
                (fun (take, used) (k, (refs, p)) ->
                    if not (Set.contains k used) && crosses refs then
                        p :: take, Set.add k used
                    else
                        take, used)
                ([], right.UsedWhere)

        let on =
            Predicate.joinConjuncts (List.rev wheres @ List.rev ons)
            |> Option.defaultValue (Expr.Literal(Literal.Bool true))

        {
            Tree = Logical.Join(JoinKind.Inner, rightName, on, left.Tree, right.Tree)
            Schema = Schema.concat left.Schema right.Schema
            Names = both
            UsedOn = usedOn
            UsedWhere = usedWhere
        }

    /// bushy 木を後行順に畳む（CPS）。
    /// 自己再帰は継続の末尾呼び出しだけなので、木の深さにスタックは依存しない。
    /// 右部分木への再帰は継続ラムダの中で、監査上は非末尾として扱われる。
    [<TailCall>]
    let rec private foldBushy
        (env: BushyEnv)
        (tree: JoinOrderTree)
        (usedOn: Set<int>)
        (usedWhere: Set<int>)
        (k: BushyNode -> Result<'r, DbError>)
        =
        match tree with
        | JoinOrderTree.Leaf name ->
            match env.ByName |> Map.tryFind name with
            | None -> err (DbError.internalError "bushy 計画の葉が FROM / JOIN の別名に対応しません")
            | Some index ->
                {
                    Tree = env.Scans.[index]
                    Schema = bindingSchema env.Bindings.[index]
                    Names = Set.singleton name
                    UsedOn = usedOn
                    UsedWhere = usedWhere
                }
                |> k
        | JoinOrderTree.Branch(l, r) ->
            foldBushy env l usedOn usedWhere (fun left ->
                foldBushy env r left.UsedOn left.UsedWhere (fun right ->
                    let rightName =
                        match r with
                        | JoinOrderTree.Leaf n -> n
                        | JoinOrderTree.Branch _ -> ""

                    joinBushy env left right rightName |> k))

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
                | Expr.QualifiedStar q when not (Schema.hasQualifier q schema) ->
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

        let withPredicates preds node =
            match Predicate.joinConjuncts preds with
            | None -> ret node
            | Some p -> ret (Logical.Filter(p, node))

        let buildScan catalog b local =
            match b with
            | DerivedSource(_, plan) -> withPredicates local (Logical.Derived plan)
            | Base tb ->
                result {
                    let! method = Analyzer.chooseScanMethod tb catalog local

                    let remaining =
                        local |> List.filter (fun p -> not (Analyzer.impliedByScan catalog tb method p))

                    return! withPredicates remaining (Logical.Scan(bindingName b, method))
                }

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

        let equiLinks =
            Predicate.splitConjuncts
            >> List.choose (fun conjunct ->
                match conjunct with
                | Expr.Binary(BinaryOp.Eq, a, b) ->
                    let qualifierOf x =
                        match Schema.tryAsColumnRef x with
                        | Some(r: ColumnRef) -> r.Qualifier
                        | None -> None

                    match qualifierOf a, qualifierOf b with
                    | Some qa, Some qb when not (Catalog.eqName qa qb) ->
                        Some(
                            Set.ofList [ qa.ToLowerInvariant(); qb.ToLowerInvariant() ],
                            conjunct,
                            equalityColumns conjunct
                        )
                    | _ -> None
                | _ -> None)

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

        let rec apply bindings scans joinPredicates onPredicates unmovableOns byName acc accSchema joined used usedOns =
            function
            | [] ->
                let leftovers =
                    onPredicates
                    |> List.choose (fun (i, _, e) -> if Set.contains i usedOns then None else Some e)

                match Predicate.joinConjuncts leftovers with
                | None -> ret (acc, accSchema, used)
                | Some p -> ret (Logical.Filter(p, acc), accSchema, used)
            | next :: tail ->
                match byName |> Map.tryFind next with
                | None -> err (DbError.internalError "結合順の候補が FROM / JOIN の別名に対応しません")
                | Some index ->
                    let promotedWhere, used' = promoteAt joinPredicates joined used next

                    let promotedOns, usedOns' =
                        onPredicates
                        |> List.fold
                            (fun (take, used) (i, refs, e) ->
                                if not (Set.contains i used) && Set.isSubset refs (Set.add next joined) then
                                    e :: take, Set.add i used
                                else
                                    take, used)
                            ([], usedOns)

                    let ownOn =
                        unmovableOns
                        |> List.tryFind (fun (i, _) -> i = index)
                        |> Option.map snd
                        |> Option.toList

                    let on =
                        Predicate.joinConjuncts (List.rev promotedOns @ promotedWhere @ ownOn)
                        |> Option.defaultValue (Expr.Literal(Literal.Bool true))

                    match buildJoin bindings scans acc accSchema JoinKind.Inner index on [] with
                    | Ok(node, schema) ->
                        apply
                            bindings
                            scans
                            joinPredicates
                            onPredicates
                            unmovableOns
                            byName
                            node
                            schema
                            (Set.add next joined)
                            used'
                            usedOns'
                            tail
                    | Error e -> err e

        let rec grow nameOf ready accepted pending =
            let fits, waiting =
                pending
                |> List.partition (fun ((i, _), refs) ->
                    Set.isSubset refs (Set.add (nameOf i) ready)
                    && not (Set.isEmpty (Set.intersect refs ready)))

            match fits with
            | [] -> accepted
            | _ ->
                let ready' = fits |> List.fold (fun s ((i, _), _) -> Set.add (nameOf i) s) ready
                grow nameOf ready' (accepted @ (fits |> List.map fst)) waiting

        let hoistAcrossLeft bindings (steps: (int * JoinClause) list) =
            let leading = steps |> List.takeWhile (fun (_, j) -> j.Kind = JoinKind.Inner)
            let rest = steps |> List.skipWhile (fun (_, j) -> j.Kind = JoinKind.Inner)

            if List.isEmpty rest || List.isEmpty bindings then
                steps
            else
                let nameOf i = lowerName bindings.[i]

                let accepted =
                    rest
                    |> List.choose (fun (i, j) ->
                        match referencedTables j.On with
                        | Some refs when j.Kind = JoinKind.Inner -> Some((i, j), refs)
                        | _ -> None)
                    |> grow nameOf (Set.ofList (nameOf 0 :: (leading |> List.map (fun (i, _) -> nameOf i)))) []

                if List.isEmpty accepted then
                    steps
                else
                    let hoisted = accepted |> List.map fst |> Set.ofList

                    leading
                    @ accepted
                    @ (rest |> List.filter (fun (i, _) -> not (Set.contains i hoisted)))

        let rec buildSteps
            bindings
            scans
            (estimates: float array)
            estimate
            statsOf
            edgeSelectivity
            joinPredicates
            acc
            accSchema
            joined
            used
            steps
            allowFirstRelax
            =
            match steps with
            | [] -> ret (acc, accSchema, used)
            | (index, clause: JoinClause) :: rest when clause.Kind = JoinKind.Left ->
                match buildJoin bindings scans acc accSchema JoinKind.Left index clause.On [] with
                | Ok(node, schema) ->
                    buildSteps
                        bindings
                        scans
                        estimates
                        estimate
                        statsOf
                        edgeSelectivity
                        joinPredicates
                        node
                        schema
                        (Set.add (lowerName bindings.[index]) joined)
                        used
                        rest
                        false
                | Error e -> err e
            | _ ->
                let inner = steps |> List.takeWhile (fun (_, j) -> j.Kind = JoinKind.Inner)
                let restSteps = steps |> List.skipWhile (fun (_, j) -> j.Kind = JoinKind.Inner)

                let segmentSchema =
                    inner
                    |> List.fold (fun schema (i, _) -> Schema.concat schema (bindingSchema bindings.[i])) accSchema

                let qualifiedOns =
                    inner
                    |> List.map (fun (i, j) ->
                        match Binder.qualify segmentSchema j.On with
                        | Ok e -> i, j, Some e
                        | Error _ -> i, j, Some j.On)

                let edges =
                    (qualifiedOns
                     |> List.collect (fun (_, _, q) ->
                         match q with
                         | Some e ->
                             equiLinks e
                             |> List.map (fun (refs, conjunct, columns) ->
                                 {
                                     Refs = refs
                                     Selectivity = edgeSelectivity conjunct
                                     KeyColumns = columns
                                 })
                         | None -> []))
                    @ (joinPredicates
                       |> Array.toList
                       |> List.map (fun (refs, p) ->
                           {
                               Refs = refs
                               Selectivity = edgeSelectivity p
                               KeyColumns = equalityColumns p
                           }))

                let onPredicates =
                    qualifiedOns
                    |> List.choose (fun (i, _, q) ->
                        q
                        |> Option.bind (fun e -> referencedTables e |> Option.map (fun refs -> i, refs, e)))

                let unmovableOns =
                    qualifiedOns
                    |> List.choose (fun (i, _, q) ->
                        match q with
                        | Some e when (referencedTables e).IsNone -> Some(i, e)
                        | _ -> None)

                let dependencies =
                    qualifiedOns
                    |> List.map (fun (i, _, q) ->
                        q
                        |> Option.bind (fun e ->
                            match referencedTables e with
                            | Some _ -> Some(lowerName bindings.[i], Set.empty)
                            | None -> None))

                let workMemBytes =
                    if ctx.Budget.Work >= 0L then
                        float ctx.Budget.Work
                    else
                        -1.0

                let normalOrder =
                    if dependencies |> List.forall Option.isSome then
                        let initialOrder =
                            match bindings with
                            | first :: _ ->
                                bindingTableSchema first
                                |> Option.bind (fun schema -> Analyzer.scanOrder (fun _ -> Some schema) acc)
                            | [] -> None

                        let initialRowBytes =
                            Analyzer.rowBytesOfTypes (accSchema.Columns |> Seq.map _.DataType)

                        let items =
                            inner
                            |> List.map (fun (i, _) -> joinItem statsOf bindings.[i] scans.[i] estimates.[i] i)

                        let deps = dependencies |> List.choose id |> Map.ofList

                        match ctx.JoinOrder with
                        | JoinOrderStrategy.Dp ->
                            JoinOrder.choose
                                (estimate acc)
                                initialRowBytes
                                joined
                                initialOrder
                                workMemBytes
                                edges
                                items
                                deps
                        | JoinOrderStrategy.Greedy -> JoinOrder.greedy (estimate acc) joined edges items deps
                        | JoinOrderStrategy.Bushy ->
                            JoinOrder.choose
                                (estimate acc)
                                initialRowBytes
                                joined
                                initialOrder
                                workMemBytes
                                edges
                                items
                                deps
                    else
                        inner |> List.map (fun (i, _) -> lowerName bindings.[i])

                let normalByName =
                    inner |> List.map (fun (i, _) -> lowerName bindings.[i], i) |> Map.ofList

                let relaxedOrder =
                    if
                        allowFirstRelax
                        && ctx.JoinOrder = JoinOrderStrategy.Dp
                        && dependencies |> List.forall Option.isSome
                    then
                        let allTables = 0 :: (inner |> List.map fst)

                        let allByName =
                            allTables |> List.map (fun i -> lowerName bindings.[i], i) |> Map.ofList

                        let allItems =
                            allTables
                            |> List.map (fun i -> joinItem statsOf bindings.[i] scans.[i] estimates.[i] i)

                        let deps = dependencies |> List.choose id |> Map.ofList

                        let candidates =
                            allItems
                            |> List.choose (fun firstItem ->
                                let firstDeps = deps |> Map.tryFind firstItem.Name |> Option.defaultValue Set.empty

                                if not (Set.isSubset firstDeps (Set.singleton firstItem.Name)) then
                                    None
                                else
                                    let rest = allItems |> List.filter (fun item -> item.Name <> firstItem.Name)

                                    JoinOrder.selingerPlan
                                        firstItem.Rows
                                        firstItem.RowBytes
                                        (Set.singleton firstItem.Name)
                                        firstItem.NaturalOrder
                                        workMemBytes
                                        edges
                                        rest
                                        deps
                                    |> Option.map (fun plan -> plan.Cost, firstItem.Name :: plan.Order))

                        match candidates with
                        | [] -> None
                        | _ ->
                            let order = candidates |> List.minBy fst |> snd

                            order
                            |> List.tryHead
                            |> Option.bind (fun first -> allByName |> Map.tryFind first)
                            |> Option.map (fun firstIndex -> firstIndex, order, allByName)
                    else
                        None

                let bushyApplied =
                    if
                        ctx.JoinOrder = JoinOrderStrategy.Bushy
                        && joined = Set.singleton (lowerName bindings.[0])
                        && not (List.isEmpty inner)
                        && List.isEmpty unmovableOns
                        && dependencies |> List.forall Option.isSome
                    then
                        let allTables = 0 :: (inner |> List.map fst)

                        let allByName =
                            allTables |> List.map (fun i -> lowerName bindings.[i], i) |> Map.ofList

                        let allItems =
                            allTables
                            |> List.map (fun i -> joinItem statsOf bindings.[i] scans.[i] estimates.[i] i)

                        let deps =
                            (lowerName bindings.[0], Set.empty) :: (dependencies |> List.choose id)
                            |> Map.ofList

                        match JoinOrder.bushyPlan workMemBytes edges allItems deps with
                        | None -> None
                        | Some tree ->
                            let onPredicatesArr = onPredicates |> Array.ofList

                            let env =
                                {
                                    Bindings = bindings
                                    Scans = scans
                                    ByName = allByName
                                    OnPredicates = onPredicatesArr
                                    JoinPredicates = joinPredicates
                                }

                            match foldBushy env tree Set.empty Set.empty ret with
                            | Error e -> Some(Error e)
                            | Ok node ->
                                let leftoverOn =
                                    onPredicatesArr
                                    |> Array.indexed
                                    |> Array.choose (fun (k, (_, _, e)) ->
                                        if Set.contains k node.UsedOn then None else Some e)
                                    |> Array.toList

                                let leftoverWhere =
                                    joinPredicates
                                    |> Array.indexed
                                    |> Array.choose (fun (k, (_, p)) ->
                                        if Set.contains k node.UsedWhere then None else Some p)
                                    |> Array.toList

                                let finalTree =
                                    match Predicate.joinConjuncts (leftoverOn @ leftoverWhere) with
                                    | None -> node.Tree
                                    | Some p -> Logical.Filter(p, node.Tree)

                                Some(Ok(finalTree, node.Schema, node.UsedWhere))
                    else
                        None

                let applied =
                    match bushyApplied with
                    | Some result -> result
                    | None ->
                        match relaxedOrder with
                        | Some(firstIndex, order, byName) ->
                            apply
                                bindings
                                scans
                                joinPredicates
                                onPredicates
                                unmovableOns
                                byName
                                scans.[firstIndex]
                                (bindingSchema bindings.[firstIndex])
                                (Set.singleton (lowerName bindings.[firstIndex]))
                                used
                                Set.empty
                                (List.tail order)
                        | None ->
                            apply
                                bindings
                                scans
                                joinPredicates
                                onPredicates
                                unmovableOns
                                normalByName
                                acc
                                accSchema
                                joined
                                used
                                Set.empty
                                normalOrder

                match applied with
                | Ok(node, schema, used') ->
                    buildSteps
                        bindings
                        scans
                        estimates
                        estimate
                        statsOf
                        edgeSelectivity
                        joinPredicates
                        node
                        schema
                        (Set.union joined (Set.ofList (inner |> List.map (fun (i, _) -> lowerName bindings.[i]))))
                        used'
                        restSteps
                        false
                | Error e -> err e

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
                match Schema.expandStar item.Expression schema with
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
                if n < 1L || n > int64 itemCount then
                    sprintf "ORDER BY の列番号 %d は範囲外です" n
                    |> DbError.create ErrorKind.SyntaxError
                    |> err
                else
                    match List.tryItem (int n - 1) outputExprs with
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

                let statsOf name =
                    bindings
                    |> List.tryFind (fun b -> Catalog.eqName (bindingName b) name)
                    |> Option.bind (function
                        | Base tb -> Catalog.findStats tb.RealName ctx.Catalog
                        | DerivedSource _ -> None)

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
                let estimates = scans' |> Array.map (Analyzer.estimateRowsWith statsOf)
                let joinPredicates' = List.toArray joinPredicates

                let! joined, usedJoinPredicates =
                    match bindings with
                    | [] -> ret (Logical.Values [||], Set.empty)
                    | first :: _ ->
                        let steps =
                            stmt.Joins
                            |> List.indexed
                            |> List.map (fun (i, j) -> i + 1, j)
                            |> hoistAcrossLeft bindings

                        let allowFirstRelax = stmt.Joins |> List.forall (fun j -> j.Kind = JoinKind.Inner)

                        buildSteps
                            bindings
                            scans'
                            estimates
                            (Analyzer.estimateRowsWith statsOf)
                            statsOf
                            (Analyzer.equalitySelectivity statsOf)
                            joinPredicates'
                            scans'.[0]
                            (bindingSchema first)
                            (Set.singleton (lowerName first))
                            Set.empty
                            steps
                            allowFirstRelax
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

    /// 集合演算（`UNION ALL`）全体に掛かる ORDER BY / LIMIT / OFFSET を論理木へ適用する。
    /// ORDER BY は先頭の SELECT の出力列（列名・別名・序数）で解決する。
    /// 枝単位の ORDER BY / LIMIT は持てない（構文上も全体に掛かる）。
    let applySetOperationTail (output: Schema) (stmt: SelectStatement) (tree: Logical) =
        let keyOf (k: SortKey) =
            match k.Expression with
            | Expr.Literal(Literal.Int n) ->
                if n < 1L || n > int64 output.Columns.Length then
                    sprintf "ORDER BY の列番号 %d は範囲外です" n
                    |> DbError.create ErrorKind.SyntaxError
                    |> err
                else
                    ret
                        { k with
                            Expression =
                                Expr.Column
                                    {
                                        Qualifier = None
                                        Name = output.Columns.[int n - 1].Name
                                    }
                        }
            | e -> ret { k with Expression = e }

        result {
            let! keys = stmt.OrderBy |> List.map keyOf |> all
            let sorted = if List.isEmpty keys then tree else Logical.Sort(keys, tree)

            let! limit =
                match stmt.Limit with
                | None -> ret -1
                | Some e -> evalCount "LIMIT" e

            let! offset =
                match stmt.Offset with
                | None -> ret 0
                | Some e -> evalCount "OFFSET" e

            if stmt.Limit.IsNone && stmt.Offset.IsNone then
                return sorted
            else
                return Logical.Limit(offset, limit, sorted)
        }
