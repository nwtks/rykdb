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
// 詳細は docs/architecture.md §6.1。
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
    /// 結合。右部分木に走査方法と WHERE の pushdown 結果（LFilter）を含む
    | Join of JoinKind * string * Expr * Logical * Logical
    /// 集約（groupBy + 集約リスト）
    | Aggregate of Expr list * (AggFunc * bool * Expr) list * Logical
    /// HAVING（集約後の述語）。第 3 引数は集約呼び出しの一覧
    /// （評価時に `havingFilter` に渡すため）。
    | Having of Expr * (AggFunc * bool * Expr) list * Expr list * Logical
    /// DISTINCT
    | Distinct of Logical
    /// 並び替え
    | Sort of SortKey list * Logical
    /// 読み飛ばし + 制限
    | Limit of int * int * Logical
    /// 定数（式のみhaving するクエリ `SELECT 1` など）
    | Values of SqlValue array

module internal Results =
    /// `Result` のリストを順に評価して全部の値を返す。1 つでも Error ならそれを返す。
    let combine (rs: Result<'a, DbError> list) =
        let rec go (rest: Result<'a, DbError> list) (acc: 'a list) =
            match rest with
            | [] -> ret (List.rev acc)
            | r :: tail ->
                match r with
                | Error e -> Error e
                | Ok v -> go tail (v :: acc)

        go rs []

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
            | Expr.Unary(op, inner) -> go inner |> Result.map (fun i -> Expr.Unary(op, i))
            | Expr.Binary(op, a, b) -> go a >>= fun a2 -> go b |> Result.map (fun b2 -> Expr.Binary(op, a2, b2))
            | Expr.IsNull(inner, neg) -> go inner |> Result.map (fun i -> Expr.IsNull(i, neg))
            | Expr.Between(a, b, c, neg) ->
                go a
                >>= fun a2 -> go b >>= fun b2 -> go c |> Result.map (fun c2 -> Expr.Between(a2, b2, c2, neg))
            | Expr.InList(a, xs, neg) ->
                go a
                >>= fun a2 ->
                    xs
                    |> List.map go
                    |> Results.combine
                    |> Result.map (fun vs -> Expr.InList(a2, vs, neg))
            | Expr.Like(a, b, neg) -> go a >>= fun a2 -> go b |> Result.map (fun b2 -> Expr.Like(a2, b2, neg))
            | Expr.Case(branches, elseE) ->
                branches
                |> List.map (fun (w, t) -> go w >>= fun w2 -> go t |> Result.map (fun t2 -> w2, t2))
                |> Results.combine
                >>= fun branches2 ->
                    match elseE with
                    | None -> ret None
                    | Some e -> go e |> Result.map (fun e2 -> Some e2)
                    |> Result.map (fun el -> Expr.Case(branches2, el))
            | Expr.FuncCall(n, args) ->
                args
                |> List.map go
                |> Results.combine
                |> Result.map (fun as2 -> Expr.FuncCall(n, as2))
            | Expr.AggCall(f, star, arg) -> go arg |> Result.map (fun a -> Expr.AggCall(f, star, a))

        go e

    /// 式が参照している列の修飾名（すべて小文字）。
    let private columnsOf (e: Expr) =
        e.ColumnRefs
        |> List.map (fun (c: ColumnRef) ->
            match c.Qualifier with
            | Some q -> (q + "." + c.Name).ToLowerInvariant()
            | None -> c.Name.ToLowerInvariant())

    /// 複数の式をまとめて修飾名へ。
    let qualifyMany (schema: Schema) (exprs: Expr list) =
        Results.combine (exprs |> List.map (fun e -> qualify schema e))

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
                // `-1` のような負数
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
                    let pkValues =
                        match b.Schema.PrimaryKey with
                        | [ _ ] -> [| value |]
                        | _ -> [| value |]

                    ret (ScanMethod.PkLookup pkValues)
                else
                    let idxes = Catalog.indexesOf catalog b.RealName

                    let found =
                        idxes
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
    let private concatSchemas (bindings: TableBinding list) : Schema =
        bindings
        |> List.map Analyzer.schemaOf
        |> List.fold (fun acc s -> acc.Concat s) Schema.empty

    /// 走査 1 つ（+ その直下に落ちた filter）。
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
    /// パーサは裸の名前（`age`）を `EName` として作るので、
    /// ここを通さないと実行時に「列 t.age が存在しません」になる。
    /// </remarks>
    let private applyAggregate (schema: Schema) (tree: Logical) (stmt: SelectStatement) =
        // SELECT 項目から「集約呼び出し」を集める（`*` は除く）。
        let selectExprs =
            stmt.Items
            |> List.map (fun (item: SelectItem) -> item.Expression)
            |> List.filter (fun e ->
                match e with
                | Expr.Star -> false
                | Expr.QualifiedStar _ -> false
                | _ -> true)

        let orderExprs =
            stmt.OrderBy |> List.map (fun (k: RykDb.Core.SortKey) -> k.Expression)

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
                    List.map2 (fun (e: Expr) (k: RykDb.Core.SortKey) -> { k with Expression = e }) qs stmt.OrderBy

                Logical.Sort(keys, tree))

    /// LIMIT / OFFSET を掛ける。
    let private applyLimit (tree: Logical) (stmt: SelectStatement) =
        match stmt.Limit, stmt.Offset with
        | None, None -> ret tree
        | _ ->
            let evalConst (e: Expr) : int =
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
        // 1) テーブルを束縛する（FROM のあとに JOIN clauses が続く）
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
                    // FROM なし = `SELECT 1`
                    ret (Logical.Values [||])
                else
                    let allSchema = concatSchemas bindings

                    let whereResult =
                        match stmt.Where with
                        | None -> ret []
                        | Some w -> Binder.qualify allSchema w |> Result.map Predicate.splitConjuncts

                    whereResult
                    >>= fun predicates ->
                        // 2) 先頭のテーブルを走査
                        let head = List.head bindings

                        buildScan ctx.Catalog head predicates
                        >>= fun firstTree ->
                            // 3) JOIN を積む（bindings の並び = FROM のあとに JOIN が続く）
                            // 状態は「組み上がった木 + JOIN の上に掛ける WHERE」。
                            // LEFT JOIN の右は null 供給側なので、そちらの WHERE は
                            // 入力に落とさず上へ回す（落とすと NULL 拡張行の扱いが変わる）。
                            let initial = ret (firstTree, [])

                            List.fold
                                (fun accState (j: JoinClause) ->
                                    accState
                                    >>= fun (acc2, topPreds) ->
                                        let targetIndex =
                                            bindings
                                            |> List.findIndex (fun b -> Catalog.eqName b.RealName j.Table.Name)

                                        match targetIndex with
                                        | -1 -> ret (acc2, topPreds)
                                        | idx when idx = 0 ->
                                            // 先頭のテーブルを JOIN の右にするのは並びが壊れる。
                                            // （bindings は FROM の順なので idx=0 はべきではない、
                                            //   もし起きたら bindings の並びを疑う）
                                            ret (acc2, topPreds)
                                        | idx ->
                                            let right = bindings.[idx]
                                            let leftSoFar = bindings |> List.take idx
                                            let leftSchema = concatSchemas leftSoFar
                                            let onSchema = leftSchema.Concat(Analyzer.schemaOf right)

                                            Binder.qualify onSchema j.On
                                            >>= fun on ->
                                                let rightName = Analyzer.refName right

                                                let rightPreds =
                                                    predicates |> List.filter (Predicate.isLocalTo rightName)

                                                match j.Kind with
                                                | JoinKind.Inner ->
                                                    // INNER は WHERE を入力へ落としても結果は変わらない。
                                                    buildScan ctx.Catalog right rightPreds
                                                    |> Result.map (fun rightScan ->
                                                        Logical.Join(j.Kind, rightName, on, acc2, rightScan), topPreds)
                                                | JoinKind.Left ->
                                                    // 右は null 供給側。入力へ落とすと NULL 拡張行の可否が
                                                    // 変わる（`IS NULL` 等）ので、JOIN の上で評価する。
                                                    buildScan ctx.Catalog right []
                                                    |> Result.map (fun rightScan ->
                                                        Logical.Join(j.Kind, rightName, on, acc2, rightScan),
                                                        topPreds @ rightPreds))
                                initial
                                stmt.Joins
                            >>= fun (joined, topPreds) ->
                                // どのテーブルにも属さない WHERE（例: `a.v = b.v`）は
                                // pushdown 先が無いので、JOIN の上に掛ける。
                                let localToAny (p: Expr) =
                                    bindings |> List.exists (fun b -> Predicate.isLocalTo (Analyzer.refName b) p)

                                let cross = predicates |> List.filter (fun p -> not (localToAny p))

                                let topFilter =
                                    match Predicate.joinConjuncts (topPreds @ cross) with
                                    | None -> joined
                                    | Some p -> Logical.Filter(p, joined)

                                ret topFilter
                                // 4) 集約 -> DISTINCT -> ORDER BY -> LIMIT
                                >>= fun t -> applyAggregate allSchema t stmt
                                >>= fun withAgg ->
                                    // SQL の評価順は
                                    //   集約 → 並び替え → 射影 → DISTINCT → LIMIT
                                    // DISTINCT は射影の「後」に掛けないと、
                                    // `SELECT DISTINCT g FROM t` が
                                    // 「(id, g) の組」を重複排除して 4 行返ってしまう。
                                    applySort allSchema withAgg stmt
                                    >>= fun withSorted ->
                                        // 射影の式にも修飾名を付ける。
                                        // パーサは `SELECT id` の `id` を `EName` として作るので、
                                        // ここを通さないと `Eval.eval` が「列でも関数でもない」
                                        // エラーになる。
                                        stmt.Items
                                        |> List.map (fun (item: SelectItem) ->
                                            Binder.qualify allSchema item.Expression
                                            |> Result.map (fun e -> { item with Expression = e }))
                                        |> Results.combine
                                        |> Result.bind (fun items ->
                                            let projected = Logical.Project(items, withSorted)

                                            // DISTINCT は射影の結果に対して掛ける。
                                            // LIMIT は最も最後にする（SQL 標準どおり）。
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
    let private execJoin
        (ctx: ExecContext)
        (kind: JoinKind)
        (leftSchema: Schema)
        (left: Op)
        (rightSchema: Schema)
        (right: Op)
        (on: Expr)
        =
        match kind with
        | JoinKind.Inner -> Ops.innerJoin leftSchema left rightSchema right on
        | JoinKind.Left -> Ops.leftJoin leftSchema left rightSchema right on

    /// 論理代数ノードの「出力スキーマ」を求める。
    /// <remarks>
    /// 集約の後の行は「groupBy の値 ++ 各集約の結果」なので、
    /// 入力スキーマ（テーブル定義）をそのまま流すと
    /// 「列 t.age が存在しません」で落ちる。
    /// `LIMIT` / `SORT` / `DISTINCT` は行の並びを変えないので素通しする。
    /// </remarks>
    [<TailCall>]
    let rec private outputOf (ctx: ExecContext) (tree: Logical) (inSchema: Schema) =
        match tree with
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
        | Logical.Sort(_, child) -> outputOf ctx child inSchema
        | Logical.Limit(_, _, child) -> outputOf ctx child inSchema
        | Logical.Distinct child -> outputOf ctx child inSchema
        | Logical.Having(_, _, _, child) -> outputOf ctx child inSchema
        | Logical.Filter(_, child) -> outputOf ctx child inSchema
        | Logical.Project(_, child) -> outputOf ctx child inSchema
        | _ -> inSchema

    /// 木の途中にある `LAggregate` の GROUP BY 式を掘り出す。
    /// 見つからなければ空リスト（＝集約なし）。
    [<TailCall>]
    let rec private groupByOf (tree: Logical) =
        match tree with
        | Logical.Aggregate(groupBy, _, _) -> groupBy
        | Logical.Having(_, _, _, child) -> groupByOf child
        | Logical.Distinct child -> groupByOf child
        | Logical.Sort(_, child) -> groupByOf child
        | Logical.Limit(_, _, child) -> groupByOf child
        | Logical.Filter(_, child) -> groupByOf child
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

    /// 論理代数を Op に落とす。
    /// <remarks>
    /// 下へ渡すスキーマは常に「入力スキーマ」`schema` のまま。
    /// `outputOf ctx child schema` は「child の出力スキーマ」であって、
    /// child を実行するときの入力スキーマではない。
    /// ここを間違えると、`SELECT SUM(age) FROM t` で
    /// 集約ノードが「集約後のヘッダ（`SUM(t.age)`）」を入力として受け取る
    /// ため、引数の `t.age` を評価できず `SUM` が `VNull` になり、
    /// `GROUP BY` も別の列でグループ化してしまう。
    /// `childSchema` は「そのノードが結果を読む側」だけが使う。
    /// </remarks>
    let rec exec (ctx: ExecContext) (tree: Logical) (schema: Schema) =
        match tree with
        | Logical.Values values -> ret (Op.ofRow values)
        | Logical.Scan(name, method) -> execScan ctx name method
        | Logical.Filter(p, child) ->
            let childSchema = outputOf ctx child schema
            exec ctx child childSchema |> Result.map (fun op -> Ops.filter childSchema op p)
        | Logical.Join(kind, name, on, lTree, rTree) ->
            // 右部分木をそのまま実行する。WHERE の pushdown 結果（LFilter）と
            // 走査方法は rTree の中にあるので、`execScan` を呼ぶと
            // 右テーブルへの WHERE が消えていた。
            let rightSchema = Schema.ofTable name None (defaultSchema ctx name)

            exec ctx lTree schema
            >>= fun lOp ->
                exec ctx rTree rightSchema
                >>= fun rOp ->
                    // 左右のスキーマは「それぞれのテーブルだけ」。
                    // `Ops.innerJoin` の中で `leftSchema.Concat rightSchema` を
                    // 作るので、ここに結合済みのものを渡すと列が重複して
                    // `ON` の列解決がズレ、JOIN が 0 行になる。

                    // 左スキーマは「右テーブルの列を除いたもの」。
                    // `schema` は FROM にある全テーブルの連結なので、
                    // そのまま渡すと右の列が 2 個入り `ON` の解決がズレる。
                    let leftSchema: Schema =
                        { schema with
                            Columns =
                                schema.Columns
                                |> Array.filter (fun (c: OutputColumn) ->
                                    match c.Qualifier with
                                    | Some q -> not (Catalog.eqName q name)
                                    | None -> true)
                            Tables = schema.Tables |> List.filter (fun t -> not (Catalog.eqName t name))
                        }

                    execJoin ctx kind leftSchema lOp rightSchema rOp on
                    |> Result.map (fun op ->
                        // 結合後のスキーマは呼び出し側が管理する（ここでは Op だけ返す）
                        op)
        | Logical.Aggregate(groupBy, aggs, child) ->
            // 集約は「入力の行」で評価する（入力のスキーマが必要）。
            exec ctx child schema >>= fun op -> Ops.aggregate schema op groupBy aggs
        | Logical.Having(h, aggs, groupBy, child) ->
            exec ctx child schema
            >>= fun op ->
                // 集約後の行 = 「groupBy の値 ++ 各集約の結果」。
                // `havingFilter` はその並びを前提に HAVING の式を書き換える。
                let p = Ops.havingFilter groupBy aggs h
                Op.filter p op |> ret
        | Logical.Distinct child -> exec ctx child schema |> Result.map Ops.distinct
        | Logical.Sort(keys, child) ->
            // 並び替えは「child の出力」（集約後なら groupBy ++ aggs）に対して行う。
            // `outputOf` は「集約結果を `?0, ?1` という名前」にする。
            // `Ops.sortBy` は位置だけで解決するので、この名前が使える。
            let childSchema = outputOf ctx child schema
            // 下へは入力スキーマを渡す（`childSchema` ではない）。
            // 下の詳細を参照。
            exec ctx child schema >>= fun op -> Ops.sortBy childSchema op keys
        | Logical.Limit(offset, limit, child) ->
            exec ctx child schema
            |> Result.map (fun op ->
                // OFFSET を先に、LIMIT を後に掛ける。
                // 逆順にすると `ORDER BY id LIMIT 2 OFFSET 1` が
                // 「先頭 2 行を取ってから 1 行捨てる」になり、
                // 4 行のテーブルから 1 行しか返らない。
                let skipped = if offset > 0 then Op.skip offset op else op

                // limit = 0 は「LIMIT が無い」を表す（applyLimit が 0 に正規化する）。
                if limit > 0 then Op.take limit skipped else skipped)
        | Logical.Project(items, child) ->
            // 射影の入力スキーマは「child の出力」。
            // 集約があれば「groupBy の値 ++ 各集約の結果」の並びになる。
            // 入力スキーマ（テーブル定義）をそのまま渡すと、集約後の行に
            // 対して「列 t.age が存在しません」になる。
            let childSchema: Schema = outputOf ctx child schema

            // 入力が集約結果なら、GROUP BY 式を射影側にも渡す。
            // `LIMIT` / `SORT` / `DISTINCT` / `HAVING` は「並び替えと絞り込み
            // だけ」で集約結果を保つので、包みを全部剥がす必要がある。
            let groupBy = groupByOf child

            // 下へは入力スキーマを渡す（`childSchema` ではない）。
            exec ctx child schema
            |> Result.map (fun op -> Ops.project childSchema groupBy op items)

// ---------------------------------------------------------------------------
// 5. 公開エントリポイント
// ---------------------------------------------------------------------------
module internal Plan =
    /// SELECT 文を実行して、結果（列名 + 行）を返す。
    let runSelect (ctx: ExecContext) (stmt: SelectStatement) =
        let registerAll (stmt: SelectStatement) : Result<ExecContext, DbError> =
            let names =
                (match stmt.From with
                 | None -> []
                 | Some r -> [ r ])
                @ (stmt.Joins |> List.map (fun j -> j.Table))

            names
            |> List.fold
                (fun acc r ->
                    acc
                    >>= fun c ->
                        let realName = r.Name

                        c.SchemaOf realName
                        |> Result.bind (fun schema ->
                            let c1 = c.WithSchema realName schema

                            match r.Alias with
                            | None -> ret c1
                            | Some a -> ret (c1.WithSchema a schema)))
                (ret ctx)

        registerAll stmt
        >>= fun ctx2 ->
            Optimizer.buildLogical ctx2 stmt
            >>= fun tree ->
                let schema =
                    let names =
                        (match stmt.From with
                         | None -> []
                         | Some r -> [ r ])
                        @ (stmt.Joins |> List.map (fun j -> j.Table))

                    names
                    |> List.choose (fun r ->
                        match ctx2.SchemaOf r.Name with
                        | Ok s -> Some(Schema.ofTable r.Name r.Alias s)
                        | Error _ -> None)
                    |> List.fold (fun (acc: Schema) (s: Schema) -> acc.Concat s) Schema.empty

                Optimizer.exec ctx2 tree schema
                >>= fun op -> Op.toArray op
                |> Result.map (fun rows ->
                    let header = Schema.projectHeader schema stmt.Items
                    header, rows)
