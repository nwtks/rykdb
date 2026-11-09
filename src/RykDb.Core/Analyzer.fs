// ===========================================================================
// Analyzer — 名前束縛 → 論理代数の補助。
//
//   1. 名前束縛（bind）: 裸の列名を修飾名（`t.id`）に解決し、出力 Schema を作る
//   2. 論理代数（buildLogical）: Scan / Filter / Project / Join / Aggregate / Sort / Limit に変換する。
//      そのテーブルだけを参照する WHERE の conjunct は Scan の直下へ落とし、走査方法もここで選ぶ
//
// 評価順は 集約 → 並び替え → 射影 → DISTINCT → LIMIT で固定。
// ===========================================================================

namespace RykDb.Core

/// 走査方法。
[<RequireQualifiedAccess>]
type internal ScanMethod =
    /// 全走査
    | SeqScan
    /// 主キーの等値検索
    | PkLookup of SqlValue array
    /// 索引の等値検索（単一列索引は 1値、複合索引は全列の値）
    | IndexLookup of string * SqlValue array
    /// 範囲走査（主キー / 索引の前方一致）
    | RangeScan of string * SqlValue array * SqlValue array option
    /// 索引の範囲走査（索引列の値が [lower, upper] の行）
    | IndexRangeScan of string * SqlValue array * SqlValue array option

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

/// 左×右の等値結合キー。
type internal JoinKeys =
    {
        /// 左スキーマ内の列位置
        LeftIndices: int array
        /// 右スキーマ内の列位置
        RightIndices: int array
        /// 等値キー以外の ON 句
        Residual: Expr option
    }

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
    | Aggregate of Expr list * (AggFunc * bool * bool * Expr) list * Logical
    /// HAVING（集約後の述語）。第 3 引数は集約呼び出しの一覧
    /// （評価時に havingFilter に渡すため）。
    | Having of Expr * (AggFunc * bool * bool * Expr) list * Expr list * Logical
    /// DISTINCT
    | Distinct of Logical
    /// 並び替え
    | Sort of SortKey list * Logical
    /// 読み飛ばし + 制限
    | Limit of int * int * Logical
    /// 定数 1行（FROM を省略した `SELECT 1` などの入力）
    | Values of SqlValue array
    /// FROM / JOIN のサブクエリ（派生テーブル）。
    /// 内側の論理木を自前の入力スキーマ・実行文脈で 1回だけ実行する。
    | Derived of PlannedQuery

/// 派生テーブル（FROM / JOIN のサブクエリ）の計画結果。
/// 計画時には実行せず、外側のオペレータ木がこのノードに到達した時点で実行する。
/// <remarks>
/// `Input` と `Context` を別に持つのは、内側 SELECT が外側と違う入力スキーマ
/// （FROM のテーブル構成）と、内側の別名スキーマを登録した文脈で走るため。
/// 外側の ctx で内側を実行すると、内側のテーブル別名を引けない。
/// </remarks>
and internal PlannedQuery =
    {
        /// 外側から参照するときの出力スキーマ。
        /// 列名は参照名（`SELECT t.id` なら `id`）、派生テーブルでは別名で修飾済み。
        Output: Schema
        /// 結果表示用のスキーマ（列名は式の表示名）。
        Header: Schema
        /// 内側 SELECT の入力スキーマ
        Input: Schema
        /// 内側 SELECT を実行する文脈（外側と同じ snapshot / transaction）
        Context: ExecContext
        /// 内側 SELECT の論理木
        Tree: Logical
    }

module internal Binder =
    /// 修飾子無しの列名を「修飾名」にする。
    /// <remarks>
    /// 修飾子なしで一意に決まる場合にだけ修飾名を付ける。
    /// 曖昧なら付けない（実行時の Schema.resolve がエラーにする）。
    /// 葉だけを直し、非葉ノードは形を保ったまま子を 1つずつ加工してから
    /// `Expr.plugChildren` で組み直す（`Expr.rewrite` の評価機）。
    /// </remarks>
    let qualify (schema: Schema) =
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

        let rewriteLeaf x =
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

        Expr.rewrite rewriteLeaf

    /// 複数の式をまとめて修飾名へ。
    let qualifyMany (schema: Schema) =
        List.map (fun e -> qualify schema e) >> all

    /// 式が指定したテーブルの列だけを参照しているか。
    /// 修飾子のない列はどのテーブル由来か特定できないため、false とする
    /// （曖昧な列を誤って pushdown しないため）。
    let referencesOnly (table: string) =
        let lower = table.ToLowerInvariant()

        Expr.columnRefs
        >> List.map (fun c ->
            match c.Qualifier with
            | Some q -> (q + "." + c.Name).ToLowerInvariant()
            | None -> c.Name.ToLowerInvariant())
        >> List.forall (fun full ->
            match full.LastIndexOf '.' with
            | -1 -> false
            | i -> full.[0 .. i - 1] = lower)

    /// 修飾子なしの列が、結合後スキーマで複数テーブルにまたがって存在するか。
    /// そのような式はどのテーブルにも pushdown してはいけない
    /// （各テーブルで個別に評価すると、曖昧エラーにならず意味が変わる）。
    let hasAmbiguousColumn (schema: Schema) =
        Expr.fold
            (fun acc x ->
                match x with
                | Expr.Column { Qualifier = None; Name = n } -> n :: acc
                | Expr.Name n -> n :: acc
                | _ -> acc)
            []
        >> List.exists (fun name ->
            schema.Columns
            |> Array.filter (fun (c: OutputColumn) -> Catalog.eqName c.Name name)
            |> Array.length
            |> fun count -> count > 1)

/// SELECT 項目の別名。
/// 別名を ORDER BY / GROUP BY / HAVING で参照できるようにするために使う。
module internal Alias =
    /// 別名（小文字）→ 式 の表。別名が重複したときは後の項目を優先する。
    let ofItems =
        List.fold
            (fun acc item ->
                match item.Alias with
                | Some a -> Map.add (a.ToLowerInvariant()) item.Expression acc
                | None -> acc)
            Map.empty

    /// 自分自身に循環参照する別名を除く。
    /// （差し替えで無限ループにしないため）
    let dropCyclic (aliases: Map<string, Expr>) =
        let namesOf =
            Expr.fold
                (fun acc node ->
                    match node with
                    | Expr.Name n -> n.ToLowerInvariant() :: acc
                    | _ -> acc)
                []

        let edges =
            aliases
            |> Map.map (fun _ v -> namesOf v |> List.filter (fun n -> Map.containsKey n aliases))

        let rec searchCycle start visited =
            function
            | [] -> false
            | n :: rest ->
                if n = start then
                    true
                elif Set.contains n visited then
                    searchCycle start visited rest
                else
                    let next =
                        match Map.tryFind n edges with
                        | Some l -> l
                        | None -> []

                    searchCycle start (Set.add n visited) (next @ rest)

        let reachesSelf start =
            match Map.tryFind start edges with
            | Some l -> l
            | None -> []
            |> searchCycle start Set.empty

        aliases |> Map.filter (fun k _ -> not (reachesSelf k))

    /// 別名を式に差し替える。
    /// 入力のスキーマに同名の列があれば列を優先する（曖昧を避ける）。
    /// 差し替えた式の中の名前も差し替え対象になる（別名の連鎖を解決する）。
    /// （`Expr.rewrite` はplugChildren の失敗を返すので Result になる）
    let substitute (schema: Schema) (aliases: Map<string, Expr>) =
        let rewriteLeaf x =
            match x with
            | Expr.Name name ->
                match Map.tryFind (name.ToLowerInvariant()) aliases with
                | Some replacement ->
                    match Schema.resolve schema { Qualifier = None; Name = name } with
                    | Ok _ -> x
                    | Error _ -> replacement
                | None -> x
            | _ -> x

        Expr.rewrite rewriteLeaf

    /// 集計を含む別名は差し替えない表（GROUP BY 用）。
    /// 集計をグループキーにすると意味が壊れるため）。
    let withoutAggregates = Map.filter (fun (_: string) e -> not (Expr.isAggregate e))

module internal Predicate =
    /// 未処理の式のスタックを辿って `AND` を平坦化する。
    /// conjunct は逆順で積み、最後に `List.rev` する（末尾再帰・O(n)）。
    [<TailCall>]
    let rec private flattenLoop (acc: Expr list) (pending: Expr list) =
        match pending with
        | [] -> List.rev acc
        | Expr.Binary(BinaryOp.And, a, b) :: rest -> flattenLoop acc (a :: b :: rest)
        | x :: rest -> flattenLoop (x :: acc) rest

    /// `AND` を平坦化して conjunct のリストにする。
    /// <remarks>
    /// 両方の再帰が末尾位置にあるので、深い `AND` チェーンでもスタックが積み上がらない。
    /// </remarks>
    let splitConjuncts (e: Expr) = flattenLoop [] [ e ]

    /// conjunct を全部 `AND` で繋ぎ直す。
    let joinConjuncts =
        function
        | [] -> None
        | [ x ] -> Some x
        | x :: rest -> Some(List.fold (fun acc p -> Expr.Binary(BinaryOp.And, acc, p)) x rest)

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

        ExecContext.schemaOf realName ctx
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

    /// 走査結果が昇順で並ぶ列名（主キー / 索引の列）。
    /// 単一行しか返さない走査は空リスト（任意の順序でよい）。
    /// 並び順が保証されない木は None。
    /// <remarks>
    /// マージ結合は「両入力が結合キーで昇順」の場合にだけ使える。
    /// LSM の走査はキーのバイト順 = 主キー / 索引列の SQL 順なので、
    /// 主キー走査・索引走査はこの並びを返す。`Filter` / `Limit` は順序を変えない。
    /// </remarks>
    let private scanOrder (tableOf: string -> TableSchema option) =
        let primaryKeyOrder t =
            match TableSchema.primaryKeyColumns t |> Array.toList with
            | [] -> None
            | keys -> Some keys

        let rec go =
            function
            | Logical.Scan(name, ScanMethod.SeqScan)
            | Logical.Scan(name, ScanMethod.RangeScan _) -> tableOf name |> Option.bind primaryKeyOrder
            | Logical.Scan(name, ScanMethod.IndexLookup(indexName, _))
            | Logical.Scan(name, ScanMethod.IndexRangeScan(indexName, _, _)) ->
                tableOf name
                |> Option.bind (fun (t: TableSchema) ->
                    t.Indexes
                    |> List.tryFind (fun (i: IndexSchema) -> Catalog.eqName i.Name indexName)
                    |> Option.map (fun i -> i.Columns))
            | Logical.Scan(_, ScanMethod.PkLookup _) -> Some []
            | Logical.Filter(_, child)
            | Logical.Limit(_, _, child) -> go child
            | Logical.Values _ -> Some []
            | _ -> None

        go

    /// 結合キーを走査順に並べ替える。
    /// 走査順の先頭 `indices.Length` 列がちょうど結合キーであれば、
    /// その順のキー位置と列名を返す。
    /// （キーでない列がキーの間に挟まると、その走査はキー順にならないので None）
    let private alignJoinKeys (schema: Schema) (indices: int array) =
        let rec pick used acc =
            function
            | [] -> Some(List.rev acc)
            | n :: rest ->
                let hit =
                    indices
                    |> Array.indexed
                    |> Array.tryFind (fun (_, i) ->
                        not (List.contains i used) && SqlName.equal schema.Columns.[i].Name n)

                match hit with
                | Some(_, i) -> pick (i :: used) (i :: acc) rest
                | None -> None

        let reorder = pick [] [] >> Option.map List.toArray

        function
        | None -> None
        | Some [] -> Some(indices, [])
        | Some names ->
            if indices.Length > List.length names then
                None
            else
                let prefix = List.truncate indices.Length names
                reorder prefix |> Option.map (fun reordered -> reordered, prefix)

    /// 結合アルゴリズムを選ぶ。
    /// - 等値キーなし: 入れ子ループ
    /// - 左が小さい（上限 32行）かつ右に使える索引: インデックス入れ子ループ
    /// - 両入力が結合キー順に走査できる: マージ結合
    /// - それ以外の等値結合: ハッシュ結合
    let chooseJoinAlgorithm
        (tableOf: string -> TableSchema option)
        (leftSchema: Schema)
        (rightSchema: Schema)
        (rightTable: TableSchema option)
        (leftTree: Logical)
        (rightTree: Logical)
        (on: Expr)
        =
        let isMatch (r: ColumnRef) (c: OutputColumn) =
            match r.Qualifier with
            | Some q ->
                SqlName.equal c.Name r.Name
                && match c.Qualifier with
                   | Some cq -> SqlName.equal cq q
                   | None -> false
            | None -> SqlName.equal c.Name r.Name

        let columnIndexIn schema expr =
            match Schema.tryAsColumnRef expr with
            | None -> None
            | Some r ->
                let hits =
                    schema.Columns |> Array.indexed |> Array.filter (fun (_, c) -> isMatch r c)

                match hits with
                | [| i, _ |] -> Some i
                | _ -> None

        let tryPair (l, r) =
            match columnIndexIn leftSchema l, columnIndexIn rightSchema r with
            | Some li, Some ri ->
                match leftSchema.Columns.[li].DataType, rightSchema.Columns.[ri].DataType with
                | Some lt, Some rt when lt = rt -> Some(li, ri)
                | _ -> None
            | _ -> None

        let joinKeys on =
            let keyPairs, residual =
                on
                |> Predicate.splitConjuncts
                |> List.fold
                    (fun (keys, residual) conjunct ->
                        match conjunct with
                        | Expr.Binary(BinaryOp.Eq, a, b) ->
                            let pair =
                                match tryPair (a, b) with
                                | Some p -> Some p
                                | None -> tryPair (b, a)

                            match pair with
                            | Some(li, ri) -> (li, ri) :: keys, residual
                            | None -> keys, conjunct :: residual
                        | _ -> keys, conjunct :: residual)
                    ([], [])

            if List.isEmpty keyPairs then
                None
            else
                let ordered = List.rev keyPairs

                Some
                    {
                        LeftIndices = ordered |> List.map fst |> Array.ofList
                        RightIndices = ordered |> List.map snd |> Array.ofList
                        Residual = Predicate.joinConjuncts (List.rev residual)
                    }

        let rightIndexForJoin rightSchema (rightTable: TableSchema) keys =
            let keyNames =
                keys.RightIndices |> Array.map (fun i -> rightSchema.Columns.[i].Name)

            let findLeft colName =
                keys.RightIndices
                |> Array.tryFindIndex (fun ri -> Catalog.eqName rightSchema.Columns.[ri].Name colName)
                |> Option.map (fun keyPos -> keys.LeftIndices.[keyPos])

            rightTable.Indexes
            |> List.choose (fun idx ->
                let coversAll =
                    idx.Columns.Length = keyNames.Length
                    && idx.Columns
                       |> List.forall (fun c -> keyNames |> Array.exists (fun k -> Catalog.eqName c k))
                    && keyNames
                       |> Array.forall (fun k -> idx.Columns |> List.exists (fun c -> Catalog.eqName c k))

                if not coversAll then
                    None
                else
                    let lefts = idx.Columns |> List.map findLeft

                    if lefts |> List.forall Option.isSome then
                        Some(idx.Name, lefts |> List.choose id |> Array.ofList)
                    else
                        None)
            |> List.tryHead

        let rec estimatedRowsCps k =
            function
            | Logical.Values _ -> k 1
            | Logical.Limit(_, limit, _) when limit >= 0 -> k limit
            | Logical.Limit(_, _, child) -> estimatedRowsCps k child
            | Logical.Scan(_, ScanMethod.PkLookup _) -> k 1
            | Logical.Project(_, child) -> estimatedRowsCps k child
            | Logical.Filter(_, child) -> estimatedRowsCps k child
            | Logical.Sort(_, child) -> estimatedRowsCps k child
            | Logical.Distinct child -> estimatedRowsCps k child
            | Logical.Aggregate(groupBy, _, child) ->
                if List.isEmpty groupBy then
                    k 1
                else
                    estimatedRowsCps k child
            | Logical.Join(_, _, _, left, right) ->
                estimatedRowsCps
                    (fun l ->
                        if l = System.Int32.MaxValue then
                            k System.Int32.MaxValue
                        else
                            estimatedRowsCps
                                (fun r ->
                                    if r = System.Int32.MaxValue then
                                        k System.Int32.MaxValue
                                    else
                                        k (min l r))
                                right)
                    left
            | _ -> k System.Int32.MaxValue

        let estimatedRows = estimatedRowsCps id

        let mergeJoinKeys keys =
            let leftOrder = scanOrder tableOf leftTree
            let rightOrder = scanOrder tableOf rightTree

            match
                alignJoinKeys leftSchema keys.LeftIndices leftOrder,
                alignJoinKeys rightSchema keys.RightIndices rightOrder
            with
            | Some(leftKeys, leftNames), Some(rightKeys, rightNames) ->
                let targetNames = if List.isEmpty leftNames then rightNames else leftNames

                let leftAligned =
                    if List.isEmpty leftNames then
                        alignJoinKeys leftSchema keys.LeftIndices (Some targetNames) |> Option.map fst
                    else
                        Some leftKeys

                let rightAligned =
                    if List.isEmpty rightNames then
                        alignJoinKeys rightSchema keys.RightIndices (Some targetNames) |> Option.map fst
                    else
                        Some rightKeys

                let sameOrder =
                    List.isEmpty leftNames
                    || List.isEmpty rightNames
                    || List.length leftNames = List.length rightNames
                       && List.forall2 (fun a b -> SqlName.equal a b) leftNames rightNames

                match leftAligned, rightAligned with
                | Some lk, Some rk when sameOrder ->
                    Some
                        { keys with
                            LeftIndices = lk
                            RightIndices = rk
                        }
                | _ -> None
            | _ -> None

        match joinKeys on with
        | None -> JoinAlgorithm.NestedLoop, None, None
        | Some keys ->
            let index =
                match rightTree, rightTable with
                | Logical.Scan(_, _), Some table -> rightIndexForJoin rightSchema table keys
                | _ -> None

            match index with
            | Some(indexName, leftIndices) when estimatedRows leftTree <= 32 ->
                JoinAlgorithm.IndexNestedLoop indexName, Some keys, Some leftIndices
            | _ ->
                match mergeJoinKeys keys with
                | Some aligned -> JoinAlgorithm.MergeJoin, Some aligned, None
                | None -> JoinAlgorithm.HashJoin, Some keys, None

    let private colOf (tn: string) =
        function
        | Expr.Column { Qualifier = Some q; Name = n } when q.ToLowerInvariant() = tn -> Some n
        | _ -> None

    let private litOf =
        function
        | Expr.Literal l -> Some l.ToSqlValue
        | Expr.Unary(UnaryOp.Neg, Expr.Literal l) ->
            match l.ToSqlValue with
            | SqlValue.Int i -> Some(SqlValue.Int -i)
            | SqlValue.Real f -> Some(SqlValue.vReal -f)
            | _ -> None
        | _ -> None

    let private rangeBounds (tn: string) (colName: string) =
        let rec accumulateBounds tn colName lower upper =
            function
            | [] -> lower, upper
            | e :: tail ->
                match e with
                | Expr.Binary(BinaryOp.Gt, a, b) ->
                    let lower' =
                        match colOf tn a, litOf b with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> lower

                    let upper' =
                        match colOf tn b, litOf a with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> upper

                    accumulateBounds tn colName lower' upper' tail
                | Expr.Binary(BinaryOp.Ge, a, b) ->
                    let lower' =
                        match colOf tn a, litOf b with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> lower

                    let upper' =
                        match colOf tn b, litOf a with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> upper

                    accumulateBounds tn colName lower' upper' tail
                | Expr.Binary(BinaryOp.Lt, a, b) ->
                    let upper' =
                        match colOf tn a, litOf b with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> upper

                    let lower' =
                        match colOf tn b, litOf a with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> lower

                    accumulateBounds tn colName lower' upper' tail
                | Expr.Binary(BinaryOp.Le, a, b) ->
                    let upper' =
                        match colOf tn a, litOf b with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> upper

                    let lower' =
                        match colOf tn b, litOf a with
                        | Some n, Some v when Catalog.eqName n colName -> Some v
                        | _ -> lower

                    accumulateBounds tn colName lower' upper' tail
                | Expr.Between(x, lo, hi, false) ->
                    match colOf tn x with
                    | Some n when Catalog.eqName n colName ->
                        match litOf lo, litOf hi with
                        | Some lv, Some hv -> accumulateBounds tn colName (Some lv) (Some hv) tail
                        | _ -> accumulateBounds tn colName lower upper tail
                    | _ -> accumulateBounds tn colName lower upper tail
                | Expr.Like(x, p, false, None) ->
                    match colOf tn x, litOf p with
                    | Some n, Some(SqlValue.Text pattern) when Catalog.eqName n colName ->
                        if
                            pattern.EndsWith "%"
                            && not (pattern.Contains "_")
                            && pattern.IndexOf "%" = pattern.Length - 1
                        then
                            let prefix = pattern.Substring(0, pattern.Length - 1)
                            let lower' = Some(SqlValue.Text prefix)

                            let upper' =
                                if prefix = "" then
                                    None
                                else
                                    let lastChar = prefix.[prefix.Length - 1]

                                    if int lastChar < 0xFFFF then
                                        let newPrefix =
                                            prefix.Substring(0, prefix.Length - 1) + string (char (int lastChar + 1))

                                        Some(SqlValue.Text newPrefix)
                                    else
                                        None

                            accumulateBounds tn colName lower' upper' tail
                        else
                            accumulateBounds tn colName lower upper tail
                    | _ -> accumulateBounds tn colName lower upper tail
                | _ -> accumulateBounds tn colName lower upper tail

        List.collect Predicate.splitConjuncts >> accumulateBounds tn colName None None

    /// WHERE から等価条件を探して走査方法を決める。
    /// 複合主キー・複合索引は、全列の等値条件が揃ったときだけ Lookup にする。
    let chooseScanMethod (b: TableBinding) (catalog: Catalog) (predicates: Expr list) =
        let equalitiesOf tn =
            List.collect Predicate.splitConjuncts
            >> List.choose (function
                | Expr.Binary(BinaryOp.Eq, a, b) ->
                    match colOf tn a, litOf b with
                    | Some n, Some v when not (SqlValue.isNull v) -> Some(n, v)
                    | _ ->
                        match colOf tn b, litOf a with
                        | Some n, Some v when not (SqlValue.isNull v) -> Some(n, v)
                        | _ -> None
                | _ -> None)

        let typeMatches (schema: TableSchema) colName value =
            schema.Columns
            |> List.exists (fun c -> Catalog.eqName c.Name colName && c.DataType = SqlValue.dataType value)

        let findValue (tn: string) colName =
            equalitiesOf (tn.ToLowerInvariant()) predicates
            |> List.tryPick (fun (n, v) ->
                if Catalog.eqName n colName && typeMatches b.Schema n v then
                    Some v
                else
                    None)

        let tryIndexRange (tn: string) =
            Catalog.indexesOf catalog b.RealName
            |> List.tryPick (fun i ->
                match i.Columns with
                | [] -> None
                | cols ->
                    let rangeColIdx =
                        cols
                        |> List.tryFindIndex (fun c ->
                            let lo, _ = rangeBounds (tn.ToLowerInvariant()) c predicates

                            match lo with
                            | Some lv when typeMatches b.Schema c lv -> true
                            | _ -> false)

                    match rangeColIdx with
                    | Some idx ->
                        let rangeCol = cols.[idx]
                        let lo, hi = rangeBounds (tn.ToLowerInvariant()) rangeCol predicates

                        match lo with
                        | Some lv when typeMatches b.Schema rangeCol lv ->
                            let upper = hi |> Option.filter (fun v -> typeMatches b.Schema rangeCol v)
                            let prefixCols = cols |> List.take idx
                            let prefixVals = prefixCols |> List.map (fun c -> findValue tn c)

                            if prefixVals |> List.forall Option.isSome then
                                let lowerKey = (prefixVals |> List.choose id) @ [ lv ] |> Array.ofList

                                let upperKey =
                                    upper
                                    |> Option.map (fun uv -> (prefixVals |> List.choose id) @ [ uv ] |> Array.ofList)

                                Some(ScanMethod.IndexRangeScan(i.IndexName, lowerKey, upperKey))
                            else
                                None
                        | _ -> None
                    | None ->
                        let firstCol = cols.Head
                        let lo, hi = rangeBounds (tn.ToLowerInvariant()) firstCol predicates

                        match lo with
                        | Some lv when typeMatches b.Schema firstCol lv ->
                            let upper = hi |> Option.filter (fun v -> typeMatches b.Schema firstCol v)

                            Some(
                                ScanMethod.IndexRangeScan(
                                    i.IndexName,
                                    [| lv |],
                                    upper |> Option.map (fun v -> [| v |])
                                )
                            )
                        | _ -> None)

        let equalityPrefix tn (i: IndexDefinition) =
            i.Columns
            |> List.map (findValue tn)
            |> List.takeWhile Option.isSome
            |> List.choose id

        let tryIndexEquality tn =
            Catalog.indexesOf catalog b.RealName
            |> List.choose (fun i ->
                let values = equalityPrefix tn i
                if List.isEmpty values then None else Some(i, values))
            |> List.sortByDescending (fun (_, values) -> List.length values)
            |> List.tryHead

        let tryIndexMethod tn =
            match tryIndexEquality tn, tryIndexRange tn with
            | Some(i, values), Some range ->
                let rangeLen =
                    match range with
                    | ScanMethod.IndexRangeScan(_, lower, _) -> lower.Length
                    | _ -> 0

                if List.length values >= rangeLen then
                    Some(ScanMethod.IndexLookup(i.IndexName, Array.ofList values))
                else
                    Some range
            | Some(i, values), None -> Some(ScanMethod.IndexLookup(i.IndexName, Array.ofList values))
            | None, Some range -> Some range
            | None, None -> None

        let tn = refName b
        let pkNames = b.Schema.PrimaryKey |> List.map (fun i -> b.Schema.Columns.[i].Name)
        let pkValues = pkNames |> List.map (findValue tn)

        if not pkNames.IsEmpty && pkValues |> List.forall Option.isSome then
            ret (ScanMethod.PkLookup(pkValues |> List.choose id |> Array.ofList))
        else if pkNames.Length = 1 then
            let pkCol = pkNames.Head
            let lower, upper = rangeBounds (tn.ToLowerInvariant()) pkCol predicates

            match lower with
            | Some lv when typeMatches b.Schema pkCol lv ->
                let startKey = [| lv |]

                let endKey =
                    upper
                    |> Option.filter (fun v -> typeMatches b.Schema pkCol v)
                    |> Option.map (fun v -> [| v |])

                ret (ScanMethod.RangeScan(tn, startKey, endKey))
            | _ ->
                match tryIndexMethod tn with
                | Some m -> ret m
                | None -> ret ScanMethod.SeqScan
        else
            match tryIndexMethod tn with
            | Some m -> ret m
            | None -> ret ScanMethod.SeqScan
