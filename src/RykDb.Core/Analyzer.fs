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
    /// 索引の等値検索（単一列索引は 1値、複合索引は先頭から連続した等値列の値）
    | IndexLookup of string * SqlValue array
    /// 範囲走査（主キー / 索引の前方一致）
    | RangeScan of string * SqlValue array * SqlValue array option
    /// 索引の範囲走査（索引列の値が [lower, upper] の行）
    | IndexRangeScan of string * SqlValue array * SqlValue array option

/// インデックス入れ子ループで右テーブルを引く方法。
[<RequireQualifiedAccess>]
type internal JoinLookup =
    /// 主キーの等値検索
    | PrimaryKey
    /// 索引による検索（索引名）
    | Index of string

/// 結合アルゴリズム。
[<RequireQualifiedAccess>]
type internal JoinAlgorithm =
    /// 入れ子のループ（cross join + 述語）
    | NestedLoop
    /// ハッシュ結合（右側をハッシュ表にする）
    | HashJoin
    /// マージ結合（両入力がキー順）
    | MergeJoin
    /// インデックス入れ子ループ（右を索引 / 主キーで引く）
    | IndexNestedLoop of JoinLookup

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
    /// `UNION ALL`（各枝を順に実行して行を連結する）。
    /// 枝ごとに入力スキーマ・実行文脈が異なるので `PlannedQuery` をそのまま持つ。
    /// 出力スキーマは先頭の枝のもの（列名も先頭の SELECT に従う）。
    | UnionAll of PlannedQuery list

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

/// 1つのテーブルが持つスキーマと、その識別名。
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

    /// 右部分木が `Scan`（`Filter` を重ねてもよい）なら、実行時に評価する述語を返す。
    /// インデックス入れ子ループは右部分木を実行しないため、
    /// `Filter` の述語は residual として結合時に評価する
    /// （捨てると WHERE の pushdown が消えて結果が変わる）。
    /// ただし `PkLookup` / `IndexLookup` は等値述語を走査が吸収して `Filter` から消えるため、
    /// 右部分木を実行しない INL では述語を再評価できない。
    /// ここでは対象外にしてハッシュ / マージ / 入れ子ループへ回す（右部分木が実行される）。
    /// `RangeScan` / `IndexRangeScan` の範囲述語は `Filter` に残るので対象にできる。
    let private indexableRight =
        let rec unwrap preds =
            function
            | Logical.Scan(_, ScanMethod.SeqScan)
            | Logical.Scan(_, ScanMethod.RangeScan _)
            | Logical.Scan(_, ScanMethod.IndexRangeScan _) -> Some preds
            | Logical.Scan _ -> None
            | Logical.Filter(p, child) -> unwrap (p :: preds) child
            | _ -> None

        unwrap []

    /// 見積りの作業項目（明示スタック。「深さ優先の後置評価」を末尾再帰で回す）。
    type private EstimateWork =
        | Visit of Logical
        | ApplyFilter
        | MinOfTwo

    /// 行数の粗い見積り（統計なし）。
    /// 結合順の決定（`PlanBuilder`）とインデックス入れ子ループの判断（`chooseJoinAlgorithm`）で
    /// 同じ値を使う。上限なし・未知は `infinity`。
    /// <remarks>
    /// 基本値は走査方法だけで決め、`Filter` 1段ごとに 0.1倍（下限 1）、`Limit` は件数。
    /// `Join` は左右の小さい方（外側の行数は内側で絞られる）。
    /// </remarks>
    let estimateRows (tree: Logical) =
        let baseRows =
            function
            | Logical.Scan(_, ScanMethod.PkLookup _) -> 1.0
            | Logical.Scan(_, ScanMethod.IndexLookup _) -> 2.0
            | Logical.Scan(_, ScanMethod.RangeScan _)
            | Logical.Scan(_, ScanMethod.IndexRangeScan _) -> 100.0
            | Logical.Scan _ -> 1000.0
            | Logical.Values _ -> 1.0
            | _ -> System.Double.PositiveInfinity

        let rec go values =
            function
            | [] ->
                match values with
                | v :: _ -> v
                | [] -> System.Double.PositiveInfinity
            | Visit node :: rest ->
                match node with
                | Logical.Filter(_, child) -> go values (ApplyFilter :: Visit child :: rest)
                | Logical.Limit(_, n, _) when n >= 0 -> go (float n :: values) rest
                | Logical.Limit(_, _, child)
                | Logical.Project(_, child)
                | Logical.Sort(_, child)
                | Logical.Distinct child
                | Logical.Having(_, _, _, child) -> go values (Visit child :: rest)
                | Logical.Aggregate(groupBy, _, _) when List.isEmpty groupBy -> go (1.0 :: values) rest
                | Logical.Aggregate(_, _, child) -> go values (Visit child :: rest)
                | Logical.Join(_, _, _, left, right) -> go values (MinOfTwo :: Visit right :: Visit left :: rest)
                | leaf -> go (baseRows leaf :: values) rest
            | ApplyFilter :: rest ->
                match values with
                | v :: tail -> go (max 1.0 (v * 0.1) :: tail) rest
                | [] -> go (1.0 :: values) rest
            | MinOfTwo :: rest ->
                match values with
                | r :: l :: tail -> go (min l r :: tail) rest
                | _ -> go (System.Double.PositiveInfinity :: values) rest

        go [] [ Visit tree ]

    /// 結合アルゴリズムを選ぶ。
    /// - 等値キーなし: 入れ子ループ
    /// - 左が小さい（上限 32行）かつ右を主キー / 索引の前置で引ける: インデックス入れ子ループ
    ///   （右の `Filter` の述語は residual として評価する）
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
        let isMatch (r: ColumnRef) c =
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

        let coversAll (keyNames: string array) cols =
            List.length cols = keyNames.Length
            && cols
               |> List.forall (fun c -> keyNames |> Array.exists (fun k -> Catalog.eqName c k))
            && keyNames
               |> Array.forall (fun k -> cols |> List.exists (fun c -> Catalog.eqName c k))

        let leftIndicesOf keys cols =
            let findLeft colName =
                keys.RightIndices
                |> Array.tryFindIndex (fun ri -> Catalog.eqName rightSchema.Columns.[ri].Name colName)
                |> Option.map (fun keyPos -> keys.LeftIndices.[keyPos])

            let lefts = cols |> List.map findLeft

            if lefts |> List.forall Option.isSome then
                Some(lefts |> List.choose id |> Array.ofList)
            else
                None

        let rightLookupForJoin rightSchema (rightTable: TableSchema) keys =
            let keyNames =
                keys.RightIndices |> Array.map (fun i -> rightSchema.Columns.[i].Name)

            let pkNames =
                rightTable.PrimaryKey |> List.map (fun i -> rightTable.Columns.[i].Name)

            if List.isEmpty pkNames || not (coversAll keyNames pkNames) then
                None
            else
                leftIndicesOf keys pkNames
                |> Option.map (fun lefts -> JoinLookup.PrimaryKey, lefts)
            |> Option.orElseWith (fun () ->
                rightTable.Indexes
                |> List.choose (fun idx ->
                    let prefix = List.truncate keyNames.Length idx.Columns

                    if coversAll keyNames prefix then
                        Some(JoinLookup.Index idx.Name, prefix)
                    else
                        None)
                |> List.tryPick (fun (lookup, cols) ->
                    leftIndicesOf keys cols |> Option.map (fun lefts -> lookup, lefts)))

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
            let lookup =
                match indexableRight rightTree, rightTable with
                | Some rightPreds, Some table ->
                    rightLookupForJoin rightSchema table keys
                    |> Option.map (fun (target, leftIndices) -> target, leftIndices, rightPreds)
                | _ -> None

            match lookup with
            | Some(target, leftIndices, rightPreds) when estimateRows leftTree <= 32.0 ->
                let residual = Option.toList keys.Residual @ rightPreds |> Predicate.joinConjuncts
                JoinAlgorithm.IndexNestedLoop target, Some { keys with Residual = residual }, Some leftIndices
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
        | Expr.Value v -> Some v
        | Expr.Unary(UnaryOp.Neg, Expr.Value v) ->
            match v with
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
    /// 複合主キーは全列の等値条件が揃ったときだけ PkLookup にし、
    /// 複合索引は先頭から連続する等値列の範囲で IndexLookup にする。
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
            match SqlValue.dataType value with
            | Ok t ->
                schema.Columns
                |> List.exists (fun c -> Catalog.eqName c.Name colName && c.DataType = t)
            | Error _ -> false

        let findValue (tn: string) colName =
            equalitiesOf (tn.ToLowerInvariant()) predicates
            |> List.tryPick (fun (n, v) ->
                if Catalog.eqName n colName && typeMatches b.Schema n v then
                    Some v
                else
                    None)

        let tryIndexRange (tn: string) =
            Catalog.indexesOf b.RealName catalog
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
            Catalog.indexesOf b.RealName catalog
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

    /// 走査方法が同じ等値条件を保証するか。
    /// `PkLookup` / `IndexLookup` が吸収済みの `col = 定数` を Filter から落とすために使う
    /// （残しても結果は変わらないが、行ごとに二重評価になる）。
    let impliedByScan (catalog: Catalog) (b: TableBinding) (method: ScanMethod) (predicate: Expr) =
        let matches names (values: SqlValue array) name value =
            names
            |> List.tryFindIndex (fun n -> Catalog.eqName n name)
            |> Option.exists (fun pos ->
                pos < values.Length
                && match SqlValue.compareValuesV values.[pos] value with
                   | DbOk 0 -> true
                   | _ -> false)

        let tn = (refName b).ToLowerInvariant()

        match predicate with
        | Expr.Binary(BinaryOp.Eq, a, c) ->
            match colOf tn a, litOf c with
            | Some n, Some v -> Some(n, v)
            | _ ->
                match colOf tn c, litOf a with
                | Some n, Some v -> Some(n, v)
                | _ -> None
        | _ -> None
        |> Option.exists (fun (name, value) ->
            match method with
            | ScanMethod.PkLookup values ->
                matches (b.Schema.PrimaryKey |> List.map (fun i -> b.Schema.Columns.[i].Name)) values name value
            | ScanMethod.IndexLookup(indexName, values) ->
                Catalog.findIndex indexName catalog
                |> Option.exists (fun idx -> matches idx.Columns values name value)
            | _ -> false)
