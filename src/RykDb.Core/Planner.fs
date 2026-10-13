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
    /// </remarks>
    let qualify (schema: Schema) (e: Expr) =
        let retE (v: Expr) = ret v

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

        let rec go (x: Expr) =
            match x with
            | Expr.Literal l -> retE (Expr.Literal l)
            | Expr.Column c ->
                match c.Qualifier with
                | Some _ -> retE x
                | None ->
                    match uniqueQualifier c.Name with
                    | Some q -> retE (Expr.Column { Qualifier = Some q; Name = c.Name })
                    | None -> retE x
            | Expr.Name name ->
                match uniqueQualifier name with
                | Some q -> retE (Expr.Column { Qualifier = Some q; Name = name })
                | None -> retE x
            | Expr.Star -> retE x
            | Expr.QualifiedStar _ -> retE x
            | _ -> Expr.mapChildrenM go x

        go e

    /// 式が参照している列の修飾名（すべて小文字）。
    let private columnsOf (e: Expr) =
        Expr.columnRefs e
        |> List.map (fun c ->
            match c.Qualifier with
            | Some q -> (q + "." + c.Name).ToLowerInvariant()
            | None -> c.Name.ToLowerInvariant())

    /// 複数の式をまとめて修飾名へ。
    let qualifyMany (schema: Schema) (exprs: Expr list) =
        all (exprs |> List.map (fun e -> qualify schema e))

    /// 式が指定したテーブルの列だけを参照しているか。
    let referencesOnly (e: Expr) (table: string) =
        let lower = table.ToLowerInvariant()

        columnsOf e
        |> List.forall (fun full ->
            match full.LastIndexOf '.' with
            | -1 -> true
            | i -> full.[0 .. i - 1] = lower)

module internal Predicate =
    /// `AND` を平坦化して conjunct のリストにする。
    let splitConjuncts (e: Expr) =
        let rec go (acc: Expr list) (x: Expr) =
            match x with
            | Expr.Binary(BinaryOp.And, a, b) -> go (go acc a) b
            | _ -> acc @ [ x ]

        go [] e

    /// conjunct を全部 `AND` で繋ぎ直す。
    let joinConjuncts (parts: Expr list) =
        match parts with
        | [] -> None
        | [ x ] -> Some x
        | x :: rest -> Some(List.fold (fun acc p -> Expr.Binary(BinaryOp.And, acc, p)) x rest)

    /// この conjunct は指定テーブルの列だけを参照しているか。
    let isLocalTo (table: string) (e: Expr) = Binder.referencesOnly e table

// ---------------------------------------------------------------------------
// 3. AST -> 論理代数
// ---------------------------------------------------------------------------

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

        ctx.SchemaOf realName
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

    /// `col = 定数` を探す（走査方法を決めるのに使う）。
    let private tryColumnEqualConstant (table: string) (e: Expr) =
        let tn = table.ToLowerInvariant()

        let colOf (v: Expr) =
            match v with
            | Expr.Column { Qualifier = Some q; Name = n } when q.ToLowerInvariant() = tn -> Some n
            | _ -> None

        let litOf (v: Expr) =
            match v with
            | Expr.Literal l -> Some(Scalar.ofLiteral l)
            | Expr.Unary(UnaryOp.Neg, Expr.Literal l) ->
                match Scalar.ofLiteral l with
                | SqlValue.Int i -> Some(SqlValue.Int -i)
                | SqlValue.Real f -> Some(SqlValue.Real -f)
                | _ -> None
            | _ -> None

        let rec go (x: Expr) =
            match x with
            | Expr.Binary(BinaryOp.And, a, b) ->
                match go a with
                | Some r -> Some r
                | None -> go b
            | Expr.Binary(BinaryOp.Eq, a, b) ->
                match colOf a, litOf b with
                | Some n, Some v -> Some(n, v)
                | _ ->
                    match colOf b, litOf a with
                    | Some n, Some v -> Some(n, v)
                    | _ -> None
            | _ -> None

        go e

    /// WHERE から等価条件を探して走査方法を決める。
    let chooseScanMethod (b: TableBinding) (catalog: Catalog) (predicates: Expr list) =
        let name = refName b
        let all = Predicate.joinConjuncts predicates

        match all with
        | Some p ->
            match tryColumnEqualConstant name p with
            | None -> ret ScanMethod.SeqScan
            | Some(colName, value) ->
                let isPk =
                    b.Schema.PrimaryKey
                    |> List.map (fun i -> b.Schema.Columns.[i].Name)
                    |> List.exists (fun n -> Catalog.eqName n colName)

                if isPk then
                    ret (ScanMethod.PkLookup [| value |])
                else
                    let found =
                        Catalog.indexesOf catalog b.RealName
                        |> List.tryFind (fun i ->
                            match i.Columns with
                            | [ c ] -> Catalog.eqName c colName
                            | _ -> false)

                    match found with
                    | Some i -> ret (ScanMethod.IndexLookup(i.IndexName, [| value |]))
                    | None -> ret ScanMethod.SeqScan
        | None -> ret ScanMethod.SeqScan

// ---------------------------------------------------------------------------
// 4. 論理代数 -> 物理 Plan
// ---------------------------------------------------------------------------
module internal Optimizer =
    /// テーブルの出力スキーマを全部連結する。
    let private concatSchemas (bindings: TableBinding list) =
        bindings |> List.map Analyzer.schemaOf |> Schema.concatAll

    /// 走査 1つ（+ その直下に落ちた filter）。
    let private buildScan (catalog: Catalog) (b: TableBinding) (predicates: Expr list) =
        let name = Analyzer.refName b
        let local = predicates |> List.filter (Predicate.isLocalTo name)

        Analyzer.chooseScanMethod b catalog predicates
        >>= fun method ->
            let scan = Logical.Scan(name, method)

            match Predicate.joinConjuncts local with
            | None -> ret scan
            | Some p -> ret (Logical.Filter(p, scan))

    /// 集約・HAVING を掛ける。
    /// <remarks>
    /// GROUP BY / HAVING / 集約の引数 / ORDER BY をすべて修飾名へ直す。
    /// パーサは裸の名前（`age`）を Name として作るので、ここを通さないと実行時に列を解決できずエラーになる。
    /// </remarks>
    let private applyAggregate (schema: Schema) (tree: Logical) (stmt: SelectStatement) =
        let selectExprs =
            stmt.Items
            |> List.map (fun item -> item.Expression)
            |> List.filter (fun e ->
                match e with
                | Expr.Star -> false
                | Expr.QualifiedStar _ -> false
                | _ -> true)

        let orderExprs = stmt.OrderBy |> List.map (fun k -> k.Expression)

        // 修飾化してから集約呼び出しを集める。
        let aggStage =
            Binder.qualifyMany schema (selectExprs @ orderExprs)
            |> Result.map (fun qs ->
                let selectAggs = Ops.aggCalls (qs |> List.take (List.length selectExprs))
                let orderAggs = orderExprs |> List.map (fun e -> Ops.aggCalls [ e ]) |> List.concat
                selectAggs, orderAggs)

        // HAVING も修飾化する。
        let havingStage =
            match stmt.Having with
            | None -> ret None
            | Some h ->
                Binder.qualifyMany schema [ h ]
                |> Result.bind (fun hs ->
                    match hs with
                    | [ hv ] -> ret (Some hv)
                    | _ -> Error(DbError.internalError "HAVING の修飾に失敗"))

        let groupStage = Binder.qualifyMany schema stmt.GroupBy

        aggStage
        |> Result.bind (fun (selectAggs, orderAggs) ->
            havingStage
            |> Result.bind (fun having ->
                groupStage
                |> Result.bind (fun groupBy ->
                    let allAggs = selectAggs @ orderAggs
                    let hasAgg = not (List.isEmpty allAggs)
                    let hasGroupBy = not (List.isEmpty groupBy)

                    if not (hasAgg || hasGroupBy) then
                        ret tree
                    else
                        let aggNode = Logical.Aggregate(groupBy, allAggs, tree)

                        match having with
                        | None -> ret aggNode
                        | Some hv -> ret (Logical.Having(hv, allAggs, groupBy, aggNode)))))

    /// ORDER BY を掛ける。
    let private applySort (schema: Schema) (tree: Logical) (stmt: SelectStatement) =
        if List.isEmpty stmt.OrderBy then
            ret tree
        else
            // ORDER BY の式も修飾化する（裸の名前 `age` のままだと実行時にエラー）。
            let qualified =
                Binder.qualifyMany schema (stmt.OrderBy |> List.map (fun k -> k.Expression))

            qualified
            |> Result.map (fun qs ->
                let keys =
                    List.map2 (fun e (k: RykDb.Core.SortKey) -> { k with Expression = e }) qs stmt.OrderBy

                Logical.Sort(keys, tree))

    /// LIMIT / OFFSET を掛ける。
    let private applyLimit (tree: Logical) (stmt: SelectStatement) =
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

    /// SELECT 文を論理代数に変換する。
    let buildLogical (ctx: ExecContext) (stmt: SelectStatement) =
        match stmt.From with
        | None -> ret []
        | Some r -> Analyzer.bindTable ctx r |> Result.map (fun b -> [ b ])
        >>= fun base0 ->
            stmt.Joins
            |> List.fold
                (fun acc j ->
                    acc
                    >>= fun bs -> Analyzer.bindTable ctx j.Table |> Result.map (fun b -> bs @ [ b ]))
                (ret base0)
            >>= fun bindings ->
                if List.isEmpty bindings then
                    ret (Logical.Values [||])
                else
                    let allSchema = concatSchemas bindings

                    match stmt.Where with
                    | None -> ret []
                    | Some w -> Binder.qualify allSchema w |> Result.map Predicate.splitConjuncts
                    >>= fun predicates ->
                        let head = List.head bindings

                        buildScan ctx.Catalog head predicates
                        >>= fun firstTree ->
                            let initial = ret (firstTree, [])

                            List.fold
                                (fun accState (i, j) ->
                                    accState
                                    >>= fun (acc2, topPreds) ->
                                        let idx = i + 1
                                        let right = bindings.[idx]
                                        let leftSchema = bindings |> List.take idx |> concatSchemas
                                        let onSchema = leftSchema.Concat(Analyzer.schemaOf right)

                                        Binder.qualify onSchema j.On
                                        >>= fun on ->
                                            let rightName = Analyzer.refName right
                                            let rightPreds = predicates |> List.filter (Predicate.isLocalTo rightName)

                                            match j.Kind with
                                            | JoinKind.Inner ->
                                                buildScan ctx.Catalog right rightPreds
                                                |> Result.map (fun rightScan ->
                                                    Logical.Join(j.Kind, rightName, on, acc2, rightScan), topPreds)
                                            | JoinKind.Left ->
                                                buildScan ctx.Catalog right []
                                                |> Result.map (fun rightScan ->
                                                    Logical.Join(j.Kind, rightName, on, acc2, rightScan),
                                                    topPreds @ rightPreds))
                                initial
                                (List.indexed stmt.Joins)
                            >>= fun (joined, topPreds) ->
                                let localToAny (p: Expr) =
                                    bindings |> List.exists (fun b -> Predicate.isLocalTo (Analyzer.refName b) p)

                                let cross = predicates |> List.filter (fun p -> not (localToAny p))

                                match Predicate.joinConjuncts (topPreds @ cross) with
                                | None -> joined
                                | Some p -> Logical.Filter(p, joined)
                                |> ret
                                >>= fun t -> applyAggregate allSchema t stmt
                                >>= fun withAgg ->
                                    applySort allSchema withAgg stmt
                                    >>= fun withSorted ->
                                        stmt.Items
                                        |> List.map (fun item ->
                                            Binder.qualify allSchema item.Expression
                                            |> Result.map (fun e -> { item with Expression = e }))
                                        |> all
                                        |> Result.bind (fun items ->
                                            let projected = Logical.Project(items, withSorted)

                                            let deduped =
                                                if stmt.Distinct then
                                                    Logical.Distinct projected
                                                else
                                                    projected

                                            applyLimit deduped stmt)

    /// 走査を実行する。
    let private execScan (ctx: ExecContext) (name: string) (method: ScanMethod) =
        match method with
        | ScanMethod.SeqScan -> Scan.table ctx name
        | ScanMethod.PkLookup values -> Scan.byPrimaryKey ctx name values
        | ScanMethod.IndexLookup(indexName, values) -> Scan.byIndex ctx name indexName values
        | ScanMethod.RangeScan(_, _, _) -> Scan.table ctx name

    /// 結合を実行する。
    let private execJoin (kind: JoinKind) (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        match kind with
        | JoinKind.Inner -> Ops.innerJoin leftSchema left rightSchema right on
        | JoinKind.Left -> Ops.leftJoin leftSchema left rightSchema right on

    /// 包み（Sort / Limit / Distinct / Having / Filter / Project）を全部剥がして、
    /// その先のノード（集約・スキャンなど）を返す。
    /// 「行の並びや絞り込みだけ」のノードは出力スキーマも GROUP BY 式も変えない
    /// ので、outputOf / groupByOf はここで同じ見方を共有する。
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

    /// 論理代数ノードの「出力スキーマ」を求める。
    /// <remarks>
    /// 集約の後の行は「groupBy の値 ++ 各集約の結果」なので、
    /// 入力スキーマ（テーブル定義）をそのまま流すと列を解決できずに落ちる。
    /// </remarks>
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

    /// 木の途中にある `LAggregate` の GROUP BY 式を掘り出す。
    /// 見つからなければ空リスト（＝集約なし）。
    let private groupByOf (tree: Logical) =
        match stripWrappers tree with
        | Logical.Aggregate(groupBy, _, _) -> groupBy
        | _ -> []

    let private defaultSchema (ctx: ExecContext) (name: string) =
        match ctx.SchemaOf name with
        | Ok s -> s
        | Error _ ->
            {
                Name = name
                Columns = []
                PrimaryKey = []
                Indexes = []
            }

    /// Join を積む側（左スパインを降りる）と回す側（引き返して結合する）の
    /// 1 段分。右部分木の Op はここで作らず、引き返してから評価する
    /// （評価順が left → right → join のまま変わらないようにするため）。
    type private JoinFrame =
        {
            Kind: JoinKind
            Right: Logical
            On: Expr
            /// 右テーブルを除いたスキーマ（左部分木へ下ろすもの）
            LeftSchema: Schema
            RightSchema: Schema
        }

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
    let rec exec (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        // Join の木は buildLogical の fold で必ず左深い。
        // 「左を評価 → 右を評価 → 結合」を exec l >>= fun lOp -> exec r >>= … と書くと
        // JOIN の数だけフレームが積もって StackOverflow するので、
        // 積む側（descend）と回す側（unwind）に分ける。
        let rec descend (stack: JoinFrame list) (left: Schema) (node: Logical) =
            match node with
            | Logical.Join(kind, name, on, lTree, rTree) ->
                let rightSchema = Schema.ofTable name None (defaultSchema ctx name)

                let leftSchema =
                    { left with
                        Columns =
                            left.Columns
                            |> Array.filter (fun c ->
                                match c.Qualifier with
                                | Some q -> not (Catalog.eqName q name)
                                | None -> true)
                        Tables = left.Tables |> List.filter (fun t -> not (Catalog.eqName t name))
                    }

                let frame =
                    {
                        Kind = kind
                        Right = rTree
                        On = on
                        LeftSchema = leftSchema
                        RightSchema = rightSchema
                    }

                descend (frame :: stack) leftSchema lTree
            | other -> exec ctx other left |> Result.map (fun op -> op, stack)

        let rec unwind (stack: JoinFrame list) (op: Op) =
            match stack with
            | [] -> ret op
            | frame :: rest ->
                match exec ctx frame.Right frame.RightSchema with
                | Error e -> Error e
                | Ok rOp ->
                    match execJoin frame.Kind frame.LeftSchema op frame.RightSchema rOp frame.On with
                    | Error e -> Error e
                    | Ok joined -> unwind rest joined

        match tree with
        | Logical.Join _ ->
            match descend [] schema tree with
            | Error e -> Error e
            | Ok(op, stack) -> unwind stack op
        | Logical.Values values -> ret (Op.ofRow values)
        | Logical.Scan(name, method) -> execScan ctx name method
        | Logical.Filter(p, child) ->
            let childSchema = outputOf child schema
            exec ctx child childSchema |> Result.map (fun op -> Ops.filter childSchema op p)
        | Logical.Aggregate(groupBy, aggs, child) ->
            exec ctx child schema >>= fun op -> Ops.aggregate schema op groupBy aggs
        | Logical.Having(h, aggs, groupBy, child) ->
            exec ctx child schema
            >>= fun op ->
                let p = Ops.havingFilter groupBy aggs h
                Op.filter p op |> ret
        | Logical.Distinct child -> exec ctx child schema |> Result.map Ops.distinct
        | Logical.Sort(keys, child) ->
            let childSchema = outputOf child schema
            exec ctx child schema >>= fun op -> Ops.sortBy childSchema op keys
        | Logical.Limit(offset, limit, child) ->
            exec ctx child schema
            |> Result.map (fun op ->
                let skipped = if offset > 0 then Op.skip offset op else op
                if limit > 0 then Op.take limit skipped else skipped)
        | Logical.Project(items, child) ->
            let childSchema = outputOf child schema
            let groupBy = groupByOf child

            exec ctx child schema
            |> Result.map (fun op -> Ops.project childSchema groupBy op items)

// ---------------------------------------------------------------------------
// 5. 公開エントリポイント
// ---------------------------------------------------------------------------
module internal Plan =
    /// SELECT 文を実行して、結果（列名 + 行）を返す。
    let runSelect (ctx: ExecContext) (stmt: SelectStatement) =
        // FROM と JOIN のテーブル参照（registerAll と出力スキーマの両方で同じ順に使う）。
        let sources =
            (match stmt.From with
             | None -> []
             | Some r -> [ r ])
            @ (stmt.Joins |> List.map (fun j -> j.Table))

        // 登録しながら出力スキーマも左から連結する
        // （SchemaOf を 登録・buildLogical・出力 の 3回呼さないため）。
        let registerAll () =
            sources
            |> foldOk
                (fun (c: ExecContext, acc: Schema) r ->
                    let realName = r.Name

                    c.SchemaOf realName
                    |> Result.bind (fun schema ->
                        let c1 = c.WithSchema realName schema

                        let c2 =
                            match r.Alias with
                            | None -> c1
                            | Some a -> c1.WithSchema a schema

                        ret (c2, acc.Concat(Schema.ofTable realName r.Alias schema))))
                (ctx, Schema.empty)

        // 識別名が重複すると exec が「右テーブルを除いたスキーマ」を名前で引けず、
        // 両方が消えて静かに誤答する（`FROM t JOIN t`）。
        // 自己結合はエイリアス（`FROM t AS x JOIN t AS y`）で表す。
        let duplicate =
            sources
            |> List.countBy (fun r -> defaultArg r.Alias r.Name)
            |> List.tryFind (fun (_, c) -> c > 1)

        match duplicate with
        | Some(name, _) -> Error(DbError.name (sprintf "テーブル %s が重複しています（エイリアスを付けてください）" name))
        | None ->
            registerAll ()
            >>= fun (ctx2, schema) ->
                Optimizer.buildLogical ctx2 stmt
                >>= fun tree ->
                    Optimizer.exec ctx2 tree schema
                    >>= fun op -> Op.toArray op
                    |> Result.map (fun rows ->
                        let header = Schema.projectHeader schema stmt.Items
                        header, rows)
