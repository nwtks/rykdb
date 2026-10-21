// ===========================================================================
// Planner — 束縛 → 論理代数 → Op。
//
//   1. 名前束縛（bind）: 裸の列名を修飾名（`t.id`）に解決し、出力 Schema を作る
//   2. 論理代数（buildLogical）: Scan / Filter / Project / Join /
//      Aggregate / Sort / Limit に落とす。そのテーブルだけを参照する
//      WHERE の conjunct は Scan の直下へ落とし、走査方法もここで選ぶ
//   3. Op 組み立て（Optimizer.exec）: 論理代数を pull 演算子に変換する
//
// 評価順は 集約 → 並び替え → 射影 → DISTINCT → LIMIT で固定。
// ===========================================================================

namespace RykDb.Core

// ---------------------------------------------------------------------------
// 1. 論理代数
// ---------------------------------------------------------------------------

/// 走査方法。
[<RequireQualifiedAccess>]
type internal ScanMethod =
    /// 全走査
    | SeqScan
    /// 主キーの等値検索
    | PkLookup of SqlValue array
    /// 索引の等値検索
    | IndexLookup of string * SqlValue array
    /// 範囲走査（主キー / 索引の前方一致）
    | RangeScan of string * SqlValue array * SqlValue array

/// 結合アルゴリズム。
[<RequireQualifiedAccess>]
type internal JoinAlgorithm =
    /// 入れ子のループ（cross join + 述語）
    | NestedLoop
    /// ハッシュ結合（右側をハッシュ表にする）
    | HashJoin
    /// マージ結合（両入力がキー順）
    | MergeJoin
    /// インデックス入れ子ループ（右を索引で引く）
    | IndexNestedLoop of string

/// 論理代数（実行方法をまだ決めていない木）。
[<RequireQualifiedAccess>]
type internal Logical =
    /// テーブル走査
    | Scan of string * ScanMethod
    /// 述語で弾く
    | Filter of Expr * Logical
    /// 射影
    | Project of SelectItem list * Logical
    /// 結合。右部分木に走査方法と WHERE の pushdown 結果（Filter）を含む
    | Join of JoinKind * string * Expr * Logical * Logical
    /// 集約（groupBy + 集約リスト）
    | Aggregate of Expr list * (AggFunc * bool * Expr) list * Logical
    /// HAVING（集約後の述語）。第 3 引数は集約呼び出しの一覧
    /// （評価時に havingFilter に渡すため）。
    | Having of Expr * (AggFunc * bool * Expr) list * Expr list * Logical
    /// DISTINCT
    | Distinct of Logical
    /// 並び替え
    | Sort of SortKey list * Logical
    /// 読み飛ばし + 制限
    | Limit of int * int * Logical
    /// 定数（式のみhaving するクエリ `SELECT 1` など）
    | Values of SqlValue array

module internal Binder =
    /// 修飾子無しの列名を「修飾名」にする。
    /// <remarks>
    /// 修飾子なしで一意に決まる場合にだけ修飾名を付ける。
    /// 曖昧なら付けない（実行時の Schema.resolve がエラーにする）。
    /// 葉だけを直し、非葉ノードは形を保ったまま子を 1つずつ加工してから
    /// `Expr.plugChildren` で組み直す（`Expr.rewrite` の評価機）。
    /// </remarks>
    let qualify (schema: Schema) (e: Expr) =
        let uniqueQualifier (name: string) =
            let lower = name.ToLowerInvariant()

            let hits =
                schema.Columns
                |> Array.toList
                |> List.filter (fun (oc: OutputColumn) -> oc.Name.ToLowerInvariant() = lower)
                |> List.choose (fun (oc: OutputColumn) -> oc.Qualifier)

            match hits with
            | [ q ] -> Some q
            | _ -> None

        let rewriteLeaf (x: Expr) =
            match x with
            | Expr.Column c ->
                match c.Qualifier with
                | Some _ -> x
                | None ->
                    match uniqueQualifier c.Name with
                    | Some q -> Expr.Column { Qualifier = Some q; Name = c.Name }
                    | None -> x
            | Expr.Name name ->
                match uniqueQualifier name with
                | Some q -> Expr.Column { Qualifier = Some q; Name = name }
                | None -> x
            | _ -> x

        Expr.rewrite rewriteLeaf e

    /// 複数の式をまとめて修飾名へ。
    let qualifyMany (schema: Schema) (exprs: Expr list) =
        exprs |> List.map (fun e -> qualify schema e) |> all

    /// 式が指定したテーブルの列だけを参照しているか。
    let referencesOnly (e: Expr) (table: string) =
        let columnsOf (e: Expr) =
            Expr.columnRefs e
            |> List.map (fun c ->
                match c.Qualifier with
                | Some q -> (q + "." + c.Name).ToLowerInvariant()
                | None -> c.Name.ToLowerInvariant())

        let lower = table.ToLowerInvariant()

        columnsOf e
        |> List.forall (fun full ->
            match full.LastIndexOf '.' with
            | -1 -> true
            | i -> full.[0 .. i - 1] = lower)

module internal Predicate =
    /// 未処理の式のスタックを辿って `AND` を平坦化する。
    /// conjunct は逆順で積み、最後に `List.rev` する（末尾再帰・O(n)）。
    [<TailCall>]
    let rec private flattenLoop (pending: Expr list) (acc: Expr list) =
        match pending with
        | [] -> List.rev acc
        | Expr.Binary(BinaryOp.And, a, b) :: rest -> flattenLoop (a :: b :: rest) acc
        | x :: rest -> flattenLoop rest (x :: acc)

    /// `AND` を平坦化して conjunct のリストにする。
    /// <remarks>
    /// 両方の再帰が末尾位置にあるので、深い `AND` チェーンでもスタックが積み上がらない。
    /// </remarks>
    let splitConjuncts (e: Expr) = flattenLoop [ e ] []

    /// conjunct を全部 `AND` で繋ぎ直す。
    let joinConjuncts (parts: Expr list) =
        match parts with
        | [] -> None
        | [ x ] -> Some x
        | x :: rest -> Some(List.fold (fun acc p -> Expr.Binary(BinaryOp.And, acc, p)) x rest)

    /// この conjunct は指定テーブルの列だけを参照しているか。
    let isLocalTo (table: string) (e: Expr) = Binder.referencesOnly e table

/// 1 つのテーブルが持つスキーマと、その識別名。
type internal TableBinding =
    {
        /// SELECT 句での名前（別名があれば別名）
        Ref: TableRef
        /// 実際のテーブル名（keyspace 名に使う）
        RealName: string
        Schema: TableSchema
    }

module internal Analyzer =
    /// テーブル参照を解決してバインディングを作る。
    let bindTable (ctx: ExecContext) (r: TableRef) =
        let realName = r.Name

        ExecContext.schemaOf ctx realName
        |> Result.map (fun schema ->
            {
                Ref = r
                RealName = realName
                Schema = schema
            })

    /// テーブル参照の解決名（別名があれば別名）。
    let refName (b: TableBinding) = defaultArg b.Ref.Alias b.RealName

    /// テーブルの出力スキーマ。
    let schemaOf (b: TableBinding) =
        Schema.ofTable b.RealName b.Ref.Alias b.Schema

    /// 指定テーブルの修飾付き列参照から列名を取り出す。
    let private colOf (tn: string) (v: Expr) =
        match v with
        | Expr.Column { Qualifier = Some q; Name = n } when q.ToLowerInvariant() = tn -> Some n
        | _ -> None

    /// リテラル（負の数を含む）を SqlValue にする。
    let private litOf (v: Expr) =
        match v with
        | Expr.Literal l -> Some l.ToSqlValue
        | Expr.Unary(UnaryOp.Neg, Expr.Literal l) ->
            match l.ToSqlValue with
            | SqlValue.Int i -> Some(SqlValue.Int -i)
            | SqlValue.Real f -> Some(SqlValue.Real -f)
            | _ -> None
        | _ -> None

    /// 未処理の式のスタックを辿り、AND の奥にある最初の等値条件を返す。
    [<TailCall>]
    let rec private findEquality (tn: string) (pending: Expr list) =
        match pending with
        | [] -> None
        | x :: rest ->
            match x with
            | Expr.Binary(BinaryOp.And, a, b) -> findEquality tn (a :: b :: rest)
            | Expr.Binary(BinaryOp.Eq, a, b) ->
                match colOf tn a, litOf b with
                | Some n, Some v -> Some(n, v)
                | _ ->
                    match colOf tn b, litOf a with
                    | Some n, Some v -> Some(n, v)
                    | _ -> findEquality tn rest
            | _ -> findEquality tn rest

    /// WHERE から等価条件を探して走査方法を決める。
    let chooseScanMethod (b: TableBinding) (catalog: Catalog) (predicates: Expr list) =
        let resolveScanMethod (colName: string) (value: SqlValue) =
            let isPk =
                b.Schema.PrimaryKey
                |> List.map (fun i -> b.Schema.Columns.[i].Name)
                |> List.exists (fun n -> Catalog.eqName n colName)

            if isPk then
                ScanMethod.PkLookup [| value |]
            else
                let found =
                    Catalog.indexesOf catalog b.RealName
                    |> List.tryFind (fun i ->
                        match i.Columns with
                        | [ c ] -> Catalog.eqName c colName
                        | _ -> false)

                match found with
                | Some i -> ScanMethod.IndexLookup(i.IndexName, [| value |])
                | None -> ScanMethod.SeqScan

        let name = refName b

        match findEquality (name.ToLowerInvariant()) predicates with
        | None -> ret ScanMethod.SeqScan
        | Some(colName, value) -> ret (resolveScanMethod colName value)

module internal Optimizer =
    /// SELECT 文を論理代数に変換する。
    let buildLogical (ctx: ExecContext) (stmt: SelectStatement) =
        let bindTables (ctx: ExecContext) (stmt: SelectStatement) =
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

        let concatSchemas (bindings: TableBinding list) =
            bindings |> List.map Analyzer.schemaOf |> Schema.concatAll

        let buildScan (catalog: Catalog) (b: TableBinding) (predicates: Expr list) =
            let name = Analyzer.refName b
            let local = predicates |> List.filter (Predicate.isLocalTo name)

            result {
                let! method = Analyzer.chooseScanMethod b catalog predicates
                let scan = Logical.Scan(name, method)

                match Predicate.joinConjuncts local with
                | None -> return scan
                | Some p -> return Logical.Filter(p, scan)
            }

        let joinStep (bindings: TableBinding list) (predicates: Expr list) accState (i, j) =
            result {
                let! acc, topPreds = accState
                let idx = i + 1
                let right = bindings.[idx]
                let leftSchema = bindings |> List.take idx |> concatSchemas
                let onSchema = Schema.concat leftSchema (Analyzer.schemaOf right)
                let! on = Binder.qualify onSchema j.On
                let rightName = Analyzer.refName right
                let rightPreds = predicates |> List.filter (Predicate.isLocalTo rightName)

                match j.Kind with
                | JoinKind.Inner ->
                    let! rightScan = buildScan ctx.Catalog right rightPreds
                    return Logical.Join(j.Kind, rightName, on, acc, rightScan), topPreds
                | JoinKind.Left ->
                    let! rightScan = buildScan ctx.Catalog right []
                    return Logical.Join(j.Kind, rightName, on, acc, rightScan), topPreds @ rightPreds
            }

        let applyAggregate (schema: Schema) (tree: Logical) (stmt: SelectStatement) =
            let selectExprs =
                stmt.Items
                |> List.map (fun item -> item.Expression)
                |> List.filter (fun e ->
                    match e with
                    | Expr.Star -> false
                    | Expr.QualifiedStar _ -> false
                    | _ -> true)

            let orderExprs = stmt.OrderBy |> List.map (fun k -> k.Expression)

            let aggStage =
                Binder.qualifyMany schema (selectExprs @ orderExprs)
                |> Result.map (fun qs ->
                    let selectAggs = Ops.aggCalls (qs |> List.take (List.length selectExprs))
                    let orderAggs = orderExprs |> List.map (fun e -> Ops.aggCalls [ e ]) |> List.concat
                    selectAggs, orderAggs)

            let havingStage =
                match stmt.Having with
                | None -> ret None
                | Some h ->
                    Binder.qualifyMany schema [ h ]
                    >>= fun hs ->
                        match hs with
                        | [ hv ] -> ret (Some hv)
                        | _ -> err (DbError.internalError "HAVING の修飾に失敗")

            let groupStage = Binder.qualifyMany schema stmt.GroupBy

            result {
                let! selectAggs, orderAggs = aggStage
                let! having = havingStage
                let! groupBy = groupStage
                let allAggs = selectAggs @ orderAggs
                let hasAgg = not (List.isEmpty allAggs)
                let hasGroupBy = not (List.isEmpty groupBy)

                if not (hasAgg || hasGroupBy) then
                    return tree
                else
                    let aggNode = Logical.Aggregate(groupBy, allAggs, tree)

                    match having with
                    | None -> return aggNode
                    | Some hv -> return Logical.Having(hv, allAggs, groupBy, aggNode)
            }

        let applySort (schema: Schema) (tree: Logical) (stmt: SelectStatement) =
            if List.isEmpty stmt.OrderBy then
                ret tree
            else
                let qualified =
                    Binder.qualifyMany schema (stmt.OrderBy |> List.map (fun k -> k.Expression))

                qualified
                |> Result.map (fun qs ->
                    let keys =
                        List.map2 (fun e (k: SortKey) -> { k with Expression = e }) qs stmt.OrderBy

                    Logical.Sort(keys, tree))

        let qualifyItems (schema: Schema) (items: SelectItem list) =
            items
            |> List.map (fun item ->
                Binder.qualify schema item.Expression
                |> Result.map (fun e -> { item with Expression = e }))
            |> all

        let applyLimit (tree: Logical) (stmt: SelectStatement) =
            match stmt.Limit, stmt.Offset with
            | None, None -> ret tree
            | _ ->
                let evalConst (e: Expr) =
                    match e with
                    | Expr.Literal(Literal.Int i) -> int i
                    | _ -> 0

                let limit =
                    match stmt.Limit with
                    | None -> 0
                    | Some e -> max 0 (evalConst e)

                let offset =
                    match stmt.Offset with
                    | None -> 0
                    | Some e -> max 0 (evalConst e)

                ret (Logical.Limit(offset, limit, tree))

        let localToAny (bindings: TableBinding list) (p: Expr) =
            bindings |> List.exists (fun b -> Predicate.isLocalTo (Analyzer.refName b) p)

        result {
            let! bindings = bindTables ctx stmt

            if List.isEmpty bindings then
                return Logical.Values [||]
            else
                let allSchema = concatSchemas bindings

                let! predicates =
                    match stmt.Where with
                    | None -> ret []
                    | Some w -> Binder.qualify allSchema w |> Result.map Predicate.splitConjuncts

                let head = List.head bindings
                let! firstTree = buildScan ctx.Catalog head predicates
                let initial = ret (firstTree, [])

                let! joined, topPreds =
                    List.fold (joinStep bindings predicates) initial (List.indexed stmt.Joins)

                let cross = predicates |> List.filter (fun p -> not (localToAny bindings p))

                let filtered =
                    match Predicate.joinConjuncts (topPreds @ cross) with
                    | None -> joined
                    | Some p -> Logical.Filter(p, joined)

                let! withAgg = applyAggregate allSchema filtered stmt
                let! withSorted = applySort allSchema withAgg stmt
                let! items = qualifyItems allSchema stmt.Items
                let projected = Logical.Project(items, withSorted)

                let deduped =
                    if stmt.Distinct then
                        Logical.Distinct projected
                    else
                        projected

                return! applyLimit deduped stmt
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
        }

    /// 評価待ちの処理。
    /// Result を消費してから次へ進む処理をクロージャではなく値として積む。
    type private ExecFrame =
        | FilterFrame of Expr * Schema
        | AggregateFrame of Expr list * (AggFunc * bool * Expr) list * Schema
        | HavingFrame of Expr * (AggFunc * bool * Expr) list * Expr list
        | DistinctFrame
        | SortFrame of SortKey list * Schema
        | LimitFrame of int * int
        | ProjectFrame of SelectItem list * Schema * Expr list
        /// 左部分木を評価し終えた Join。次に右部分木を評価する。
        | JoinRightFrame of JoinFrame
        /// 右部分木も評価し終えた Join。左 Op を保持している。
        | JoinApplyFrame of JoinFrame * Op

    /// 評価機の状態。
    /// 木を 1つ降りるか、Op が上がってきたか。
    type private ExecStep =
        | EvalNode of Logical * Schema
        | ApplyOp of Op

    let private defaultSchema (ctx: ExecContext) (name: string) =
        match ExecContext.schemaOf ctx name with
        | Ok s -> s
        | Error _ ->
            {
                Name = name
                Columns = []
                PrimaryKey = []
                Indexes = []
            }

    let private execJoin (kind: JoinKind) (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        match kind with
        | JoinKind.Inner -> Ops.innerJoin leftSchema left rightSchema right on
        | JoinKind.Left -> Ops.leftJoin leftSchema left rightSchema right on

    let private execScan (ctx: ExecContext) (name: string) (method: ScanMethod) =
        match method with
        | ScanMethod.SeqScan -> ExecContext.table ctx name
        | ScanMethod.PkLookup values -> ExecContext.byPrimaryKey ctx name values
        | ScanMethod.IndexLookup(indexName, values) -> ExecContext.byIndex ctx name indexName values
        | ScanMethod.RangeScan(_, _, _) -> ExecContext.table ctx name

    [<TailCall>]
    let rec private stripWrappers (tree: Logical) =
        match tree with
        | Logical.Sort(_, child) -> stripWrappers child
        | Logical.Limit(_, _, child) -> stripWrappers child
        | Logical.Distinct child -> stripWrappers child
        | Logical.Having(_, _, _, child) -> stripWrappers child
        | Logical.Filter(_, child) -> stripWrappers child
        | Logical.Project(_, child) -> stripWrappers child
        | other -> other

    let private outputOf (tree: Logical) (inSchema: Schema) =
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
                |> List.map (fun (f, star, arg) ->
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

    let private groupByOf (tree: Logical) =
        match stripWrappers tree with
        | Logical.Aggregate(groupBy, _, _) -> groupBy
        | _ -> []

    /// 論理木を 1ノードずつ処理する。
    /// 継続は ExecFrame list に積むので、ラッパの深さに依存せずスタックは一定。
    [<TailCall>]
    let rec private run (ctx: ExecContext) (frames: ExecFrame list) (step: ExecStep) =
        match step with
        | EvalNode(node, inSchema) ->
            match node with
            | Logical.Join(kind, name, on, lTree, rTree) ->
                let rightSchema = Schema.ofTable name None (defaultSchema ctx name)

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

                let frame =
                    {
                        Kind = kind
                        Right = rTree
                        On = on
                        LeftSchema = leftSchema
                        RightSchema = rightSchema
                    }

                run ctx (JoinRightFrame frame :: frames) (EvalNode(lTree, leftSchema))
            | Logical.Values values -> run ctx frames (ApplyOp(Op.ofRow values))
            | Logical.Scan(name, method) ->
                match execScan ctx name method with
                | Error e -> Error e
                | Ok op -> run ctx frames (ApplyOp op)
            | Logical.Filter(p, child) ->
                let childSchema = outputOf child inSchema
                run ctx (FilterFrame(p, childSchema) :: frames) (EvalNode(child, childSchema))
            | Logical.Aggregate(groupBy, aggs, child) ->
                run ctx (AggregateFrame(groupBy, aggs, inSchema) :: frames) (EvalNode(child, inSchema))
            | Logical.Having(h, aggs, groupBy, child) ->
                run ctx (HavingFrame(h, aggs, groupBy) :: frames) (EvalNode(child, inSchema))
            | Logical.Distinct child -> run ctx (DistinctFrame :: frames) (EvalNode(child, inSchema))
            | Logical.Sort(keys, child) ->
                let childSchema = outputOf child inSchema
                run ctx (SortFrame(keys, childSchema) :: frames) (EvalNode(child, inSchema))
            | Logical.Limit(offset, limit, child) ->
                run ctx (LimitFrame(offset, limit) :: frames) (EvalNode(child, inSchema))
            | Logical.Project(items, child) ->
                let childSchema = outputOf child inSchema
                let groupBy = groupByOf child
                run ctx (ProjectFrame(items, childSchema, groupBy) :: frames) (EvalNode(child, inSchema))
        | ApplyOp op ->
            match frames with
            | [] -> ret op
            | frame :: rest ->
                match frame with
                | FilterFrame(p, childSchema) -> run ctx rest (ApplyOp(Ops.filter childSchema op p))
                | AggregateFrame(groupBy, aggs, inSchema) ->
                    match Ops.aggregate inSchema op groupBy aggs with
                    | Error e -> Error e
                    | Ok aggregated -> run ctx rest (ApplyOp aggregated)
                | HavingFrame(h, aggs, groupBy) ->
                    let p = Ops.havingFilter groupBy aggs h
                    run ctx rest (ApplyOp(Op.filter p op))
                | DistinctFrame -> run ctx rest (ApplyOp(Ops.distinct op))
                | SortFrame(keys, childSchema) ->
                    match Ops.sortBy childSchema op keys with
                    | Error e -> Error e
                    | Ok sorted -> run ctx rest (ApplyOp sorted)
                | LimitFrame(offset, limit) ->
                    let skipped = if offset > 0 then Op.skip offset op else op
                    let limited = if limit > 0 then Op.take limit skipped else skipped
                    run ctx rest (ApplyOp limited)
                | ProjectFrame(items, childSchema, groupBy) ->
                    run ctx rest (ApplyOp(Ops.project childSchema groupBy op items))
                | JoinRightFrame frame ->
                    run ctx (JoinApplyFrame(frame, op) :: rest) (EvalNode(frame.Right, frame.RightSchema))
                | JoinApplyFrame(frame, leftOp) ->
                    match execJoin frame.Kind frame.LeftSchema leftOp frame.RightSchema op frame.On with
                    | Error e -> Error e
                    | Ok joined -> run ctx rest (ApplyOp joined)

    /// 論理代数を Op に落とす。
    /// <remarks>
    /// 下へ渡すスキーマは常に「入力スキーマ」schema のまま。
    /// outputOf child schema は「child の出力スキーマ」であって、
    /// child を実行するときの入力スキーマではない。
    /// ここを間違えると、`SELECT SUM(age) FROM t` で
    /// 集約ノードが「集約後のヘッダ（`SUM(t.age)`）」を入力として受け取るため、
    /// 引数の `t.age` を評価できず `SUM` が Null になり、 `GROUP BY` も別の列でグループ化してしまう。
    /// childSchema は「そのノードが結果を読む側」だけが使う。
    /// </remarks>
    let exec (ctx: ExecContext) (tree: Logical) (schema: Schema) = run ctx [] (EvalNode(tree, schema))

module internal Plan =
    /// SELECT 文を実行して、結果（列名 + 行）を返す。
    let runSelect (ctx: ExecContext) (stmt: SelectStatement) =
        let registerAll (sources: TableRef list) =
            sources
            |> foldOk
                (fun (c: ExecContext, acc: Schema) r ->
                    result {
                        let realName = r.Name
                        let! schema = ExecContext.schemaOf c realName
                        let c1 = ExecContext.withSchema c realName schema

                        let c2 =
                            match r.Alias with
                            | None -> c1
                            | Some a -> ExecContext.withSchema c1 a schema

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
        | Some(name, _) -> err (sprintf "テーブル %s が重複しています（エイリアスを付けてください）" name |> DbError.name)
        | None ->
            result {
                let! ctx2, schema = registerAll sources
                let! tree = Optimizer.buildLogical ctx2 stmt
                let! op = Optimizer.exec ctx2 tree schema
                let! rows = Op.toArray op
                let header = Schema.projectHeader schema stmt.Items
                return header, rows
            }
