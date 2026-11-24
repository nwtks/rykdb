// ===========================================================================
// PlanBuilder — SELECT の論理代数（Scan / Filter / Join / Aggregate …）を組み立てる。
// ===========================================================================

namespace RykDb.Core

module internal PlanBuilder =
    /// 木が主キー順の単一走査かを調べる（Filter は透過的にたどる）。
    [<TailCall>]
    let rec private scanOfPrimaryKeyOrder (tree: Logical) =
        match tree with
        | Logical.Filter(_, child) -> scanOfPrimaryKeyOrder child
        | Logical.Scan(_, ScanMethod.SeqScan) -> true
        | Logical.Scan(_, ScanMethod.PkLookup _) -> true
        | Logical.Scan(_, ScanMethod.RangeScan _) -> true
        | _ -> false

    /// 走査が主キー順（キー昇順）に結果を返すか。
    /// `SeqScan` / 主キー等値 / 主キー範囲は主キー順に返る。
    /// 索引経由の走査は索引順なので対象外（`WHERE` の索引選択で順序が変わる）。
    let private scansInPrimaryKeyOrder (tree: Logical) = scanOfPrimaryKeyOrder tree

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

    /// 索引名に対応する PrefixNDV を取り出す（統計が無ければ空）。
    let private indexStatsOf (stats: TableStats option) (name: string) =
        stats
        |> Option.bind (fun s -> s.Indexes |> List.tryFind (fun (i: IndexStats) -> Catalog.eqName i.Name name))
        |> Option.map _.PrefixNDV
        |> Option.defaultValue []

    /// 結合順の候補を作る。
    let private joinItem (statsOf: string -> TableStats option) (position: int) (scan: Logical) (rows: float) =
        function
        | Base tb as binding ->
            let stats = statsOf (bindingName binding)

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
                               PrefixNDV = indexStatsOf stats index.Name
                               Unique = index.Unique
                           }))
                RowBytes =
                    stats
                    |> Option.filter (fun s -> s.RowBytes > 0.0)
                    |> Option.map _.RowBytes
                    |> Option.defaultValue (
                        Analyzer.rowBytesOfTypes (tb.Schema.Columns |> Seq.map (fun c -> Some c.DataType))
                    )
                NaturalOrder = Analyzer.scanOrder (fun _ -> Some tb.Schema) scan
                Lookupable = Analyzer.canScanJoinRight scan
            }
        | DerivedSource(_, plan) as binding ->
            {
                Name = (bindingName binding).ToLowerInvariant()
                Rows = rows
                Position = position
                Indexes = []
                RowBytes = Analyzer.rowBytesOfTypes (plan.Output.Columns |> Seq.map _.DataType)
                NaturalOrder = None
                Lookupable = false
            }

    /// LIMIT / OFFSET の定数式を評価する（0 以上の int）。
    let private evalCount (what: string) =
        function
        | Expr.Literal(Literal.Int i) when i >= 0L && i <= int64 System.Int32.MaxValue -> ret (int i)
        | e when Expr.hasCorrelated e -> sprintf "%s に相関サブクエリは使えません" what |> DbError.create ErrorKind.SyntaxError |> err
        | e ->
            Eval.eval Schema.Empty [||] e
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

    /// 述語が左右の部分木の両方を参照するか。
    let private crossesNames
        (both: Set<string>)
        (leftNames: Set<string>)
        (rightNames: Set<string>)
        (refs: Set<string>)
        =
        Set.isSubset refs both
        && not (Set.isEmpty (Set.intersect refs leftNames))
        && not (Set.isEmpty (Set.intersect refs rightNames))

    /// 左右の部分木を結合し、両方を参照する述語を ON に取り込む。
    let private joinBushy (env: BushyEnv) (left: BushyNode) (right: BushyNode) (rightName: string) =
        let both = Set.union left.Names right.Names

        let ons, usedOn =
            env.OnPredicates
            |> Array.fold
                (fun (take, used) (k, refs, e) ->
                    if not (Set.contains k used) && crossesNames both left.Names right.Names refs then
                        e :: take, Set.add k used
                    else
                        take, used)
                ([], right.UsedOn)

        let wheres, usedWhere =
            env.JoinPredicates
            |> Array.indexed
            |> Array.fold
                (fun (take, used) (k, (refs, p)) ->
                    if not (Set.contains k used) && crossesNames both left.Names right.Names refs then
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

    /// 束縛の参照名（小文字）。
    let private lowerName (b: SourceBinding) = (bindingName b).ToLowerInvariant()

    /// 全束縛の出力スキーマを左から連結する。
    let private concatSchemas = List.map bindingSchema >> Schema.concatAll

    /// 述語が参照するテーブル名（小文字）の集合。
    /// 裸の列名が混ざる・修飾が混在する・相関サブクエリがある場合は判定できないので None。
    let private referencedTables (p: Expr) =
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

    /// 述語のリストを node の上に張る（空ならそのまま）。
    let private withPredicates (preds: Expr list) (node: Logical) =
        match Predicate.joinConjuncts preds with
        | None -> ret node
        | Some p -> ret (Logical.Filter(p, node))

    /// FROM / JOIN の 1テーブルを束縛する（派生テーブルは計画済みの計画を使う）。
    let private bindSource (ctx: ExecContext) (derived: Map<string, PlannedQuery>) (r: TableRef) =
        match derived |> Map.tryFind (r.Name.ToLowerInvariant()) with
        | Some plan -> ret (DerivedSource(r, plan))
        | None -> Analyzer.bindTable ctx r |> Result.map Base

    /// FROM と JOIN の全テーブルを左から順に束縛する。
    let private bindTables (ctx: ExecContext) (derived: Map<string, PlannedQuery>) (stmt: SelectStatement) =
        result {
            let! baseBindings =
                match stmt.From with
                | None -> ret []
                | Some(TableSource.Named r) -> bindSource ctx derived r |> Result.map (fun b -> [ b ])
                | Some(TableSource.Derived(_, alias)) ->
                    bindSource ctx derived { Name = alias; Alias = None }
                    |> Result.map (fun b -> [ b ])

            let! bindings =
                stmt.Joins
                |> List.fold
                    (fun acc j ->
                        result {
                            let! bs = acc

                            let! b =
                                match j.Table with
                                | TableSource.Named r -> bindSource ctx derived r
                                | TableSource.Derived(_, alias) ->
                                    bindSource ctx derived { Name = alias; Alias = None }

                            return bs @ [ b ]
                        })
                    (ret baseBindings)

            return bindings
        }

    /// 1つのテーブルだけを参照する述語を集める。
    let private localPreds (name: string) =
        List.choose (fun (p, refs) ->
            match refs with
            | Some s when Set.count s = 1 && Catalog.eqName (Set.minElement s) name -> Some p
            | _ -> None)

    /// SELECT 項の `*` / `t.*` を検査する。
    let private validateStars (stmt: SelectStatement) (schema: Schema) =
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

    /// 1つの束縛に対する走査を作り、走査で処理済みの述語を除いて残りを上に張る。
    let private buildScan (catalog: Catalog) (local: Expr list) =
        function
        | DerivedSource(_, plan) -> withPredicates local (Logical.Derived plan)
        | Base tb as b ->
            result {
                let! method = Analyzer.chooseScanMethod tb catalog local

                let remaining =
                    local |> List.filter (fun p -> not (Analyzer.impliedByScan catalog tb method p))

                return! withPredicates remaining (Logical.Scan(bindingName b, method))
            }

    /// 右側の束縛を 1つ結合する。ON を修飾し、昇格済み述語を連結する。
    let private buildJoin
        (bindings: SourceBinding list)
        (scans: Logical array)
        (acc: Logical)
        (accSchema: Schema)
        (kind: JoinKind)
        (rightIndex: int)
        (promoted: Expr list)
        (on: Expr)
        =
        let right = bindings.[rightIndex]
        let rightSchema = bindingSchema right

        Binder.qualify (Schema.concat accSchema rightSchema) on
        |> Result.map (fun qualified ->
            let onAll =
                match promoted with
                | [] -> qualified
                | ps -> Predicate.joinConjuncts (qualified :: ps) |> Option.defaultValue qualified

            Logical.Join(kind, bindingName right, onAll, acc, scans.[rightIndex]), Schema.concat accSchema rightSchema)

    /// 式が列参照なら修飾子を返す。
    let private qualifierOfColumn (e: Expr) =
        match Schema.tryAsColumnRef e with
        | Some(r: ColumnRef) -> r.Qualifier
        | None -> None

    /// 等値述語を（参照テーブル集合, 述語, テーブル名 -> 結合キー列）に分解する。
    let private equiLinks (schema: Schema) =
        Predicate.splitConjuncts
        >> List.choose (fun conjunct ->
            match conjunct with
            | Expr.Binary(BinaryOp.Eq, a, b) ->
                match qualifierOfColumn a, qualifierOfColumn b with
                | Some qa, Some qb when not (Catalog.eqName qa qb) ->
                    let columns =
                        if Analyzer.isJoinKeyEquality schema conjunct then
                            equalityColumns conjunct
                        else
                            Map.empty

                    Some(Set.ofList [ qa.ToLowerInvariant(); qb.ToLowerInvariant() ], conjunct, columns)
                | _ -> None
            | _ -> None)

    /// 次に結合するテーブルに取り込める WHERE 述語を集める。
    let private promoteAt
        (joinPredicates: (Set<string> * Expr) array)
        (joined: Set<string>)
        (used: Set<int>)
        (next: string)
        =
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

    /// 結合順に従って結合を積むときの静的な入力。
    type private JoinOrderInput =
        {
            Bindings: SourceBinding list
            Scans: Logical array
            JoinPredicates: (Set<string> * Expr) array
            OnPredicates: (int * Set<string> * Expr) list
            UnmovableOns: (int * Expr) list
            ByName: Map<string, int>
        }

    /// 結合順に従って 1 つずつ結合を積む（末尾再帰）。
    /// `order` はテーブル名（小文字）の列。残った ON 述語は最後に Filter にまとめる。
    [<TailCall>]
    let rec private applyJoinOrder
        (env: JoinOrderInput)
        (acc: Logical)
        (accSchema: Schema)
        (joined: Set<string>)
        (used: Set<int>)
        (usedOns: Set<int>)
        =
        function
        | [] ->
            let leftovers =
                env.OnPredicates
                |> List.choose (fun (i, _, e) -> if Set.contains i usedOns then None else Some e)

            match Predicate.joinConjuncts leftovers with
            | None -> ret (acc, accSchema, used)
            | Some p -> ret (Logical.Filter(p, acc), accSchema, used)
        | next :: tail ->
            match env.ByName |> Map.tryFind next with
            | None -> err (DbError.internalError "結合順の候補が FROM / JOIN の別名に対応しません")
            | Some index ->
                let promotedWhere, used' = promoteAt env.JoinPredicates joined used next

                let promotedOns, usedOns' =
                    env.OnPredicates
                    |> List.fold
                        (fun (take, used) (i, refs, e) ->
                            if not (Set.contains i used) && Set.isSubset refs (Set.add next joined) then
                                e :: take, Set.add i used
                            else
                                take, used)
                        ([], usedOns)

                let ownOn =
                    env.UnmovableOns
                    |> List.tryFind (fun (i, _) -> i = index)
                    |> Option.map snd
                    |> Option.toList

                let on =
                    Predicate.joinConjuncts (List.rev promotedOns @ promotedWhere @ ownOn)
                    |> Option.defaultValue (Expr.Literal(Literal.Bool true))

                match buildJoin env.Bindings env.Scans acc accSchema JoinKind.Inner index [] on with
                | Ok(node, schema) -> applyJoinOrder env node schema (Set.add next joined) used' usedOns' tail
                | Error e -> err e

    /// 述語の依存が満たされた結合を繰り返し取り込む（LEFT JOIN 越しの前倒し用）。
    [<TailCall>]
    let rec private growAccepted
        (nameOf: 'a -> 'b)
        (ready: Set<'b>)
        (accepted: ('a * 'c) list)
        (pending: (('a * 'c) * Set<'b>) list)
        =
        let fits, waiting =
            pending
            |> List.partition (fun ((i, _), refs) ->
                Set.isSubset refs (Set.add (nameOf i) ready)
                && not (Set.isEmpty (Set.intersect refs ready)))

        match fits with
        | [] -> accepted
        | _ ->
            let ready' = fits |> List.fold (fun s ((i, _), _) -> Set.add (nameOf i) s) ready
            growAccepted nameOf ready' (accepted @ (fits |> List.map fst)) waiting

    /// 束縛の参照名（小文字）。
    let private nameOfBinding (bindings: SourceBinding list) (i: int) = lowerName bindings.[i]

    /// LEFT JOIN より前の INNER JOIN に、後続の INNER JOIN の ON から移せる述語を前倒しする。
    let private hoistAcrossLeft (bindings: SourceBinding list) (steps: (int * JoinClause) list) =
        let leading = steps |> List.takeWhile (fun (_, j) -> j.Kind = JoinKind.Inner)
        let rest = steps |> List.skipWhile (fun (_, j) -> j.Kind = JoinKind.Inner)

        if List.isEmpty rest || List.isEmpty bindings then
            steps
        else
            let accepted =
                rest
                |> List.choose (fun (i, j) ->
                    match referencedTables j.On with
                    | Some refs when j.Kind = JoinKind.Inner -> Some((i, j), refs)
                    | _ -> None)
                |> growAccepted
                    (nameOfBinding bindings)
                    (Set.ofList (
                        nameOfBinding bindings 0
                        :: (leading |> List.map (fun (i, _) -> nameOfBinding bindings i))
                    ))
                    []

            if List.isEmpty accepted then
                steps
            else
                let hoisted = accepted |> List.map fst |> Set.ofList

                leading
                @ accepted
                @ (rest |> List.filter (fun (i, _) -> not (Set.contains i hoisted)))

    /// INNER JOIN 区間の結合順解析に必要な入力（`buildSteps` の再帰で不変）。
    type private SegmentInput =
        {
            Ctx: ExecContext
            Bindings: SourceBinding list
            Scans: Logical array
            Estimates: float array
            Estimate: Logical -> float
            StatsOf: string -> TableStats option
            EdgeSelectivity: Expr -> float option
            JoinPredicates: (Set<string> * Expr) array
        }

    /// INNER JOIN 区間の ON を区間スキーマで修飾する（失敗したら元の式のまま使う）。
    let private qualifyOns (segmentSchema: Schema) =
        List.map (fun (i, j) ->
            match Binder.qualify segmentSchema j.On with
            | Ok e -> i, j, Some e
            | Error _ -> i, j, Some j.On)

    /// 区間の結合辺。ON の等値述語と、2 つ以上のテーブルを参照する WHERE 述語。
    let private segmentEdges
        (segmentSchema: Schema)
        (keySchema: Schema)
        (qualifiedOns: (int * JoinClause * Expr option) list)
        (joinPredicates: (Set<string> * Expr) array)
        (edgeSelectivity: Expr -> float option)
        =
        (qualifiedOns
         |> List.collect (fun (_, _, q) ->
             match q with
             | Some e ->
                 equiLinks segmentSchema e
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
                   KeyColumns =
                       if Analyzer.isJoinKeyEquality keySchema p then
                           equalityColumns p
                       else
                           Map.empty
               }))

    /// 結合に昇格できる ON 述語（参照テーブルが全て判明しているもの）。
    let private movableOns =
        List.choose (fun (i, _, q) ->
            q
            |> Option.bind (fun e -> referencedTables e |> Option.map (fun refs -> i, refs, e)))

    /// どの結合にも昇格できない ON 述語（実行時に結合の上の Filter で評価する）。
    let private unmovableOnsOf =
        List.choose (fun (i, _, q) ->
            match q with
            | Some e when (referencedTables e).IsNone -> Some(i, e)
            | _ -> None)

    /// 各結合の依存（そのテーブル自身を参照する ON があることを示す）。
    let private joinDependencies (bindings: SourceBinding list) =
        List.map (fun (i, _, q) ->
            q
            |> Option.bind (fun e ->
                match referencedTables e with
                | Some _ -> Some(lowerName bindings.[i], Set.empty)
                | None -> None))

    /// 既定の結合順。依存が全て判明しているときは JoinOrder 戦略に従い、
    /// それ以外は FROM 順。
    let private normalOrderOf
        (inner: (int * JoinClause) list)
        (acc: Logical)
        (accSchema: Schema)
        (joined: Set<string>)
        (dependencies: (string * Set<string>) option list)
        (edges: JoinOrderEdge list)
        (workMemBytes: float)
        (input: SegmentInput)
        =
        if dependencies |> List.forall Option.isSome then
            let initialOrder =
                match input.Bindings with
                | first :: _ ->
                    bindingTableSchema first
                    |> Option.bind (fun schema -> Analyzer.scanOrder (fun _ -> Some schema) acc)
                | [] -> None

            let initialRowBytes =
                Analyzer.rowBytesOfTypes (accSchema.Columns |> Seq.map _.DataType)

            let items =
                inner
                |> List.map (fun (i, _) ->
                    joinItem input.StatsOf i input.Scans.[i] input.Estimates.[i] input.Bindings.[i])

            let deps = dependencies |> List.choose id |> Map.ofList

            match input.Ctx.JoinOrder with
            | JoinOrderStrategy.Dp ->
                JoinOrder.choose (input.Estimate acc) initialRowBytes joined initialOrder workMemBytes edges items deps
            | JoinOrderStrategy.Greedy -> JoinOrder.greedy (input.Estimate acc) joined edges items deps
            | JoinOrderStrategy.Bushy ->
                JoinOrder.choose (input.Estimate acc) initialRowBytes joined initialOrder workMemBytes edges items deps
        else
            inner |> List.map (fun (i, _) -> lowerName input.Bindings.[i])

    /// DP の先頭テーブル固定を外した緩和計画（全て INNER のときだけ試す）。
    /// 返すのは (先頭の FROM 位置, 順序, 名前 -> FROM 位置)。
    let private relaxedOrderOf
        (inner: (int * JoinClause) list)
        (dependencies: (string * Set<string>) option list)
        (edges: JoinOrderEdge list)
        (workMemBytes: float)
        (allowFirstRelax: bool)
        (input: SegmentInput)
        =
        if
            allowFirstRelax
            && input.Ctx.JoinOrder = JoinOrderStrategy.Dp
            && dependencies |> List.forall Option.isSome
        then
            let allTables = 0 :: (inner |> List.map fst)

            let allByName =
                allTables |> List.map (fun i -> lowerName input.Bindings.[i], i) |> Map.ofList

            let allItems =
                allTables
                |> List.map (fun i -> joinItem input.StatsOf i input.Scans.[i] input.Estimates.[i] input.Bindings.[i])

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

    /// bushy 計画の試行。適用条件を満たすときだけ結果を返す。
    /// 戻り値は (木, スキーマ, 取り込んだ WHERE 述語の番号集合)。
    let private bushyResultOf
        (inner: (int * JoinClause) list)
        (joined: Set<string>)
        (unmovableOns: (int * Expr) list)
        (dependencies: (string * Set<string>) option list)
        (edges: JoinOrderEdge list)
        (onPredicates: (int * Set<string> * Expr) list)
        (workMemBytes: float)
        (input: SegmentInput)
        =
        if
            input.Ctx.JoinOrder = JoinOrderStrategy.Bushy
            && joined = Set.singleton (lowerName input.Bindings.[0])
            && not (List.isEmpty inner)
            && List.isEmpty unmovableOns
            && dependencies |> List.forall Option.isSome
        then
            let allTables = 0 :: (inner |> List.map fst)

            let allByName =
                allTables |> List.map (fun i -> lowerName input.Bindings.[i], i) |> Map.ofList

            let allItems =
                allTables
                |> List.map (fun i -> joinItem input.StatsOf i input.Scans.[i] input.Estimates.[i] input.Bindings.[i])

            let deps =
                (lowerName input.Bindings.[0], Set.empty) :: (dependencies |> List.choose id)
                |> Map.ofList

            match JoinOrder.bushyPlan workMemBytes edges allItems deps with
            | None -> None
            | Some tree ->
                let onPredicatesArr = onPredicates |> Array.ofList

                let env =
                    {
                        Bindings = input.Bindings
                        Scans = input.Scans
                        ByName = allByName
                        OnPredicates = onPredicatesArr
                        JoinPredicates = input.JoinPredicates
                    }

                match foldBushy env tree Set.empty Set.empty ret with
                | Error e -> Some(Error e)
                | Ok node ->
                    let leftoverOn =
                        onPredicatesArr
                        |> Array.indexed
                        |> Array.choose (fun (k, (_, _, e)) -> if Set.contains k node.UsedOn then None else Some e)
                        |> Array.toList

                    let leftoverWhere =
                        input.JoinPredicates
                        |> Array.indexed
                        |> Array.choose (fun (k, (_, p)) -> if Set.contains k node.UsedWhere then None else Some p)
                        |> Array.toList

                    let finalTree =
                        match Predicate.joinConjuncts (leftoverOn @ leftoverWhere) with
                        | None -> node.Tree
                        | Some p -> Logical.Filter(p, node.Tree)

                    Some(Ok(finalTree, node.Schema, node.UsedWhere))
        else
            None

    /// INNER JOIN 区間 1 つ分の計画を組み立てる。
    /// 戻り値は (木, スキーマ, 取り込んだ WHERE 述語の番号集合)。
    let private planInnerSegment
        (acc: Logical)
        (accSchema: Schema)
        (joined: Set<string>)
        (used: Set<int>)
        (inner: (int * JoinClause) list)
        (allowFirstRelax: bool)
        (input: SegmentInput)
        =
        let segmentSchema =
            inner
            |> List.fold (fun schema (i, _) -> Schema.concat schema (bindingSchema input.Bindings.[i])) accSchema

        let qualifiedOns = qualifyOns segmentSchema inner

        let edges =
            segmentEdges
                segmentSchema
                (concatSchemas input.Bindings)
                qualifiedOns
                input.JoinPredicates
                input.EdgeSelectivity

        let onPredicates = movableOns qualifiedOns
        let unmovableOns = unmovableOnsOf qualifiedOns
        let dependencies = joinDependencies input.Bindings qualifiedOns

        let workMemBytes =
            if input.Ctx.Budget.Work >= 0L then
                float input.Ctx.Budget.Work
            else
                -1.0

        let normalOrder =
            normalOrderOf inner acc accSchema joined dependencies edges workMemBytes input

        let normalByName =
            inner |> List.map (fun (i, _) -> lowerName input.Bindings.[i], i) |> Map.ofList

        let relaxedOrder =
            relaxedOrderOf inner dependencies edges workMemBytes allowFirstRelax input

        let bushyApplied =
            bushyResultOf inner joined unmovableOns dependencies edges onPredicates workMemBytes input

        match bushyApplied with
        | Some result -> result
        | None ->
            match relaxedOrder with
            | Some(firstIndex, order, byName) ->
                applyJoinOrder
                    {
                        Bindings = input.Bindings
                        Scans = input.Scans
                        JoinPredicates = input.JoinPredicates
                        OnPredicates = onPredicates
                        UnmovableOns = unmovableOns
                        ByName = byName
                    }
                    input.Scans.[firstIndex]
                    (bindingSchema input.Bindings.[firstIndex])
                    (Set.singleton (lowerName input.Bindings.[firstIndex]))
                    used
                    Set.empty
                    (List.tail order)
            | None ->
                applyJoinOrder
                    {
                        Bindings = input.Bindings
                        Scans = input.Scans
                        JoinPredicates = input.JoinPredicates
                        OnPredicates = onPredicates
                        UnmovableOns = unmovableOns
                        ByName = normalByName
                    }
                    acc
                    accSchema
                    joined
                    used
                    Set.empty
                    normalOrder

    /// FROM と JOIN を左から順に積む（末尾再帰）。
    /// LEFT JOIN は 1 つずつ、INNER JOIN は区間ごとに結合順を解析して積む。
    [<TailCall>]
    let rec private buildSteps
        (acc: Logical)
        (accSchema: Schema)
        (joined: Set<string>)
        (used: Set<int>)
        (steps: (int * JoinClause) list)
        (allowFirstRelax: bool)
        (input: SegmentInput)
        =
        match steps with
        | [] -> ret (acc, accSchema, used)
        | (index, clause: JoinClause) :: rest when clause.Kind = JoinKind.Left ->
            match buildJoin input.Bindings input.Scans acc accSchema JoinKind.Left index [] clause.On with
            | Ok(node, schema) ->
                buildSteps node schema (Set.add (lowerName input.Bindings.[index]) joined) used rest false input
            | Error e -> err e
        | _ ->
            let inner = steps |> List.takeWhile (fun (_, j) -> j.Kind = JoinKind.Inner)
            let restSteps = steps |> List.skipWhile (fun (_, j) -> j.Kind = JoinKind.Inner)

            match planInnerSegment acc accSchema joined used inner allowFirstRelax input with
            | Ok(node, schema, used') ->
                buildSteps
                    node
                    schema
                    (Set.union joined (Set.ofList (inner |> List.map (fun (i, _) -> lowerName input.Bindings.[i]))))
                    used'
                    restSteps
                    false
                    input
            | Error e -> err e

    /// 集約クエリの部品（修飾済みの SELECT / ORDER BY / HAVING / GROUP BY と集約呼び出し）。
    type private AggParts =
        {
            SelectExprs: Expr list
            OrderExprs: Expr list
            Having: Expr option
            GroupBy: Expr list
            AllAggs: (AggFunc * bool * bool * Expr) list
        }

    /// SELECT / ORDER BY / HAVING / GROUP BY を修飾し、集約呼び出しを集める。
    let private collectAggParts (schema: Schema) (stmt: SelectStatement) =
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

            return
                {
                    SelectExprs = selectExprs
                    OrderExprs = orderExprs
                    Having = having
                    GroupBy = groupBy
                    AllAggs = selectAggs @ orderAggs @ havingAggs
                }
        }

    /// 集約クエリの静的な検査（相関サブクエリ・SELECT *・HAVING 単独）。
    let private checkAggQuery (stmt: SelectStatement) (parts: AggParts) =
        let hasAgg = not (List.isEmpty parts.AllAggs)
        let hasGroupBy = not (List.isEmpty parts.GroupBy)

        let hasStar =
            stmt.Items.IsEmpty
            || stmt.Items
               |> List.exists (fun item ->
                   match item.Expression with
                   | Expr.Star
                   | Expr.QualifiedStar _ -> true
                   | _ -> false)

        let correlatedInOutput =
            parts.SelectExprs |> List.exists Expr.hasCorrelated
            || parts.OrderExprs |> List.exists Expr.hasCorrelated
            || match parts.Having with
               | Some h -> Expr.hasCorrelated h
               | None -> false

        if (hasAgg || hasGroupBy) && correlatedInOutput then
            "GROUP BY / 集約関数を使うクエリの SELECT / HAVING / ORDER BY では相関サブクエリを使えません"
            |> DbError.create ErrorKind.SyntaxError
            |> err
        elif (hasAgg || hasGroupBy) && hasStar then
            "GROUP BY / 集約関数を使うときは SELECT * を書けません（列を明示してください）"
            |> DbError.create ErrorKind.SyntaxError
            |> err
        elif not (hasAgg || hasGroupBy) then
            match parts.Having with
            | Some _ -> "HAVING には GROUP BY か集約関数が必要です" |> DbError.create ErrorKind.SyntaxError |> err
            | None -> ret ()
        else
            ret ()

    /// 集約ノード（と HAVING）を張る。
    let private aggregateTree (schema: Schema) (stmt: SelectStatement) (tree: Logical) =
        result {
            let! parts = collectAggParts schema stmt
            let! () = checkAggQuery stmt parts

            if List.isEmpty parts.AllAggs && List.isEmpty parts.GroupBy then
                return tree
            else
                let aggNode = Logical.Aggregate(parts.GroupBy, parts.AllAggs, tree)

                match parts.Having with
                | None -> return aggNode
                | Some hv -> return Logical.Having(hv, parts.AllAggs, parts.GroupBy, aggNode)
        }

    /// 集約・GROUP BY・HAVING があれば集約ノードを張る。
    let private applyAggregate (schema: Schema) (stmt: SelectStatement) (tree: Logical) =
        let hasAggregateOrGroup =
            not (List.isEmpty stmt.GroupBy)
            || stmt.Having.IsSome
            || stmt.Items |> List.exists (fun item -> Expr.isAggregate item.Expression)
            || stmt.OrderBy |> List.exists (fun k -> Expr.isAggregate k.Expression)

        if not hasAggregateOrGroup then
            ret tree
        else
            aggregateTree schema stmt tree

    /// SELECT 項を展開する（`*` / `t.*` はスキーマの列並びへ）。
    let private expandItem (schema: Schema) (item: SelectItem) =
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

    /// ORDER BY の序数（`ORDER BY 2`）を出力式へ解決する。
    let private ordinalExpr (outputExprs: Expr list) (itemCount: int) =
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

    /// ORDER BY を張る。走査順で間に合うなら省略する。
    let private applySort (ctx: ExecContext) (stmt: SelectStatement) (schema: Schema) (tree: Logical) =
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

    /// SELECT 項の式を修飾する。
    let private qualifyItems (schema: Schema) =
        List.map (fun item ->
            Binder.qualify schema item.Expression
            |> Result.map (fun e -> { item with Expression = e }))
        >> all

    /// LIMIT / OFFSET を張る。
    let private applyLimit (stmt: SelectStatement) (tree: Logical) =
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

    /// WHERE 述語を参照テーブルの数で分類したもの。
    type private ClassifiedPredicates =
        {
            /// 述語と参照テーブル集合（判定できないときは None）の対。
            All: (Expr * Set<string> option) list
            /// 2 つ以上のテーブルを参照する述語（結合順の解析で取り込む）。
            JoinPredicates: (Set<string> * Expr) list
            /// 参照テーブルが空の述語（FROM なしのときだけ先頭に張る）。
            ConstantPreds: Expr list
            /// 参照集合を判定できなかった述語（最後に Filter で張る）。
            UnmovablePreds: Expr list
        }

    /// WHERE を述語ごとに分類する。
    let private classifyPredicates (bindingNames: Set<string>) (predicates: Expr list) =
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

        {
            All = classified
            JoinPredicates = joinPredicates
            ConstantPreds = constantPreds
            UnmovablePreds = unmovablePreds
        }

    /// LEFT JOIN の右側になるテーブル名（小文字）の集合。
    let private leftRightTableNames (bindings: SourceBinding list) (stmt: SelectStatement) =
        stmt.Joins
        |> List.indexed
        |> List.choose (fun (i, j) ->
            if j.Kind = JoinKind.Left then
                Some(lowerName bindings.[i + 1])
            else
                None)
        |> Set.ofList

    /// LEFT JOIN の右側テーブルに割り当てる述語
    /// （LEFT JOIN の右に WHERE を置くと NULL 拡張行の扱いが変わるので置けない）。
    let private leftRightPredicates (classified: ClassifiedPredicates) (leftRightTables: Set<string>) =
        List.filter (fun b -> Set.contains (lowerName b) leftRightTables)
        >> List.collect (fun b -> localPreds (bindingName b) classified.All)

    /// 各束縛の走査に割り当てる述語。
    /// 先頭の束縛には定数述語も割り当てる（それ以外の置き場が無いため）。
    let private scanPredicates
        (classified: ClassifiedPredicates)
        (leftRightTables: Set<string>)
        (i: int)
        (b: SourceBinding)
        =
        if Set.contains (lowerName b) leftRightTables then
            []
        elif i = 0 then
            localPreds (bindingName b) classified.All @ classified.ConstantPreds
        else
            localPreds (bindingName b) classified.All

    /// 統計の参照（実テーブルだけ。派生テーブルは None）。
    let private statsLookup (ctx: ExecContext) (name: string) =
        List.tryFind (fun b -> Catalog.eqName (bindingName b) name)
        >> Option.bind (function
            | Base tb -> Catalog.findStats tb.RealName ctx.Catalog
            | DerivedSource _ -> None)

    /// FROM と JOIN を結合して初期の木を作る。
    /// 戻り値は (木, 取り込んだ WHERE 述語の番号集合)。
    let private joinTree
        (ctx: ExecContext)
        (stmt: SelectStatement)
        (scans: Logical array)
        (estimates: float array)
        (statsOf: string -> TableStats option)
        (joinPredicates: (Set<string> * Expr) array)
        =
        function
        | [] -> ret (Logical.Values [||], Set.empty)
        | first :: _ as bindings ->
            let input =
                {
                    Ctx = ctx
                    Bindings = bindings
                    Scans = scans
                    Estimates = estimates
                    Estimate = Analyzer.estimateRowsWith statsOf
                    StatsOf = statsOf
                    EdgeSelectivity = Analyzer.equalitySelectivity statsOf
                    JoinPredicates = joinPredicates
                }

            let steps =
                stmt.Joins
                |> List.indexed
                |> List.map (fun (i, j) -> i + 1, j)
                |> hoistAcrossLeft bindings

            let allowFirstRelax = stmt.Joins |> List.forall (fun j -> j.Kind = JoinKind.Inner)

            buildSteps
                scans.[0]
                (bindingSchema first)
                (Set.singleton (lowerName first))
                Set.empty
                steps
                allowFirstRelax
                input
            |> Result.map (fun (tree, _, used) -> tree, used)

    /// 結合で取り込めなかった述語を集める（最後に 1 つの Filter にまとめる）。
    let private leftoverPredicates
        (classified: ClassifiedPredicates)
        (leftRightPreds: Expr list)
        (joinPredicates: (Set<string> * Expr) array)
        (usedJoinPredicates: Set<int>)
        =
        classified.UnmovablePreds
        @ leftRightPreds
        @ (joinPredicates
           |> Array.indexed
           |> Array.choose (fun (k, (_, p)) -> if Set.contains k usedJoinPredicates then None else Some p)
           |> Array.toList)

    /// 束縛名から統計を引く（ctx と bindings を明示的に受け取る）。
    let private statsLookupFor (ctx: ExecContext) (bindings: SourceBinding list) (name: string) =
        statsLookup ctx name bindings

    /// SELECT 文を論理代数に変換する。
    /// `derived` は計画済みの派生テーブル（別名の小文字 -> 計画）。
    let buildLogicalWith (ctx: ExecContext) (stmt: SelectStatement) (derived: Map<string, PlannedQuery>) =
        result {
            let! bindings = bindTables ctx derived stmt

            if stmt.From.IsNone && not (List.isEmpty stmt.Joins) then
                return! DbError.create ErrorKind.SyntaxError "JOIN には FROM が必要です" |> err
            else
                let allSchema = concatSchemas bindings
                let! () = validateStars stmt allSchema

                let! predicates =
                    match stmt.Where with
                    | None -> ret []
                    | Some w -> Binder.qualify allSchema w |> Result.map Predicate.splitConjuncts

                let bindingNames = bindings |> List.map lowerName |> Set.ofList
                let classified = classifyPredicates bindingNames predicates
                let statsOf = statsLookupFor ctx bindings
                let leftRightTables = leftRightTableNames bindings stmt
                let leftRightPreds = leftRightPredicates classified leftRightTables bindings
                let joinPredicates' = List.toArray classified.JoinPredicates

                let! scans =
                    bindings
                    |> List.mapi (fun i b -> buildScan ctx.Catalog (scanPredicates classified leftRightTables i b) b)
                    |> all

                let scans' = List.toArray scans
                let estimates = scans' |> Array.map (Analyzer.estimateRowsWith statsOf)

                let! joined, usedJoinPredicates =
                    joinTree ctx stmt scans' estimates statsOf joinPredicates' bindings

                let remaining =
                    leftoverPredicates classified leftRightPreds joinPredicates' usedJoinPredicates

                let remaining =
                    if List.isEmpty bindings then
                        remaining @ classified.ConstantPreds
                    else
                        remaining

                let! filtered = withPredicates remaining joined
                let! withAgg = applyAggregate allSchema stmt filtered
                let! withSorted = applySort ctx stmt allSchema withAgg
                let! items = qualifyItems allSchema stmt.Items
                let projected = Logical.Project(items, withSorted)

                let deduped =
                    if stmt.Distinct then
                        Logical.Distinct projected
                    else
                        projected

                return! applyLimit stmt deduped
        }

    /// ORDER BY の序数を出力スキーマの列名へ解決する。
    let private sortKeyOfOutput (output: Schema) (k: SortKey) =
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

    /// 集合演算（`UNION ALL`）全体に掛かる ORDER BY / LIMIT / OFFSET を論理木へ適用する。
    /// ORDER BY は先頭の SELECT の出力列（列名・別名・序数）で解決する。
    /// 枝単位の ORDER BY / LIMIT は持てない（構文上も全体に掛かる）。
    let applySetOperationTail (output: Schema) (stmt: SelectStatement) (tree: Logical) =
        result {
            let! keys = stmt.OrderBy |> List.map (sortKeyOfOutput output) |> all
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
