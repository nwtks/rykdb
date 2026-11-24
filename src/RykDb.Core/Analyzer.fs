// ===========================================================================
// Analyzer — テーブル束縛、走査方法と結合アルゴリズムの選択、行数見積り。
//
//   bindTable           : テーブル参照を TableBinding へ解決する
//   chooseScanMethod    : WHERE の等値・範囲から ScanMethod を選ぶ
//   chooseJoinAlgorithm : ON と昇格した WHERE から JoinAlgorithm を選ぶ
//   estimateRows        : 統計（無ければ既定値）から行数を推定する（結合順と INL の判断で共有）
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
    /// 主キー列の並びを返す（主キーが無ければ None）。
    let private primaryKeyOrder (t: TableSchema) =
        match TableSchema.primaryKeyColumns t |> Array.toList with
        | [] -> None
        | keys -> Some keys

    /// 走査順が保証される列名を木から辿る。
    [<TailCall>]
    let rec private scanOrderGo (tableOf: string -> TableSchema option) =
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
        | Logical.Limit(_, _, child) -> scanOrderGo tableOf child
        | Logical.Values _ -> Some []
        | _ -> None

    let scanOrder (tableOf: string -> TableSchema option) = scanOrderGo tableOf

    /// 列名を未使用の結合キー位置へ順に対応付ける。
    [<TailCall>]
    let rec private alignJoinKeysPick (schema: Schema) (indices: int array) (used: int list) (acc: int list) =
        function
        | [] -> Some(List.rev acc)
        | n :: rest ->
            let hit =
                indices
                |> Array.indexed
                |> Array.tryFind (fun (_, i) -> not (List.contains i used) && SqlName.equal schema.Columns.[i].Name n)

            match hit with
            | Some(_, i) -> alignJoinKeysPick schema indices (i :: used) (i :: acc) rest
            | None -> None

    /// 対応付いた列位置を配列へ整える。
    let private alignJoinKeysReorder (schema: Schema) (indices: int array) =
        alignJoinKeysPick schema indices [] [] >> Option.map List.toArray

    /// 結合キーを走査順に並べ替える。
    /// 走査順の先頭 `indices.Length` 列がちょうど結合キーであれば、
    /// その順のキー位置と列名を返す。
    /// （キーでない列がキーの間に挟まると、その走査はキー順にならないので None）
    let private alignJoinKeys (schema: Schema) (indices: int array) =
        function
        | None -> None
        | Some [] -> Some(indices, [])
        | Some names ->
            if indices.Length > List.length names then
                None
            else
                let prefix = List.truncate indices.Length names

                alignJoinKeysReorder schema indices prefix
                |> Option.map (fun reordered -> reordered, prefix)

    /// Filter を剥がしながら Scan と述語を探す。
    [<TailCall>]
    let rec private unwrapScanPreds (preds: Expr list) =
        function
        | Logical.Scan(name, method) -> Some(name, method, preds)
        | Logical.Filter(p, child) -> unwrapScanPreds (p :: preds) child
        | _ -> None

    /// 右部分木の `Scan`（`Filter` を重ねてもよい）から、
    /// 走査名・走査方法・residual の述語を返す。
    /// インデックス入れ子ループは右部分木を実行しないため、
    /// `Filter` の述語は residual として結合時に評価する
    /// （捨てると WHERE の pushdown が消えて結果が変わる）。
    let private scanOfRight = unwrapScanPreds []

    /// 右部分木を索引で引ける形か（`Scan` か `Filter` の連なり）。
    /// インデックス入れ子ループは右部分木を実行しないため、
    /// `Filter` は residual に回して評価する。
    /// 結合順のコスト見積りが実行時と同じ条件で INL を選ぶために使う。
    let canScanJoinRight = scanOfRight >> Option.isSome

    /// 走査名で修飾した列参照を作る。
    let private absorbedColumn (rightName: string) (name: string) =
        Expr.Column
            {
                Qualifier = Some rightName
                Name = name
            }

    /// 吸収済み等値の述語を再構成する。
    let private absorbedEquality (rightName: string) (name: string) (value: SqlValue) =
        Expr.Binary(BinaryOp.Eq, absorbedColumn rightName name, Expr.Value value)

    /// 走査方法が吸収した等値述語を `Expr` に再構成する。
    /// `Analyzer.impliedByScan` は `PkLookup` / `IndexLookup` が保証する等値を
    /// `Filter` から除くため、INL では residual に戻さないと結果が変わる。
    /// 索引が見つからない・値の数が合わないなど再構成できないときは None を返し、INL にしない。
    let private absorbedPredicates (rightTable: TableSchema) (rightName: string) =
        function
        | ScanMethod.PkLookup values ->
            let pkNames =
                rightTable.PrimaryKey
                |> List.choose (fun i -> rightTable.Columns |> List.tryItem i |> Option.map _.Name)

            if pkNames.Length = values.Length then
                Some(List.map2 (absorbedEquality rightName) pkNames (List.ofArray values))
            else
                None
        | ScanMethod.IndexLookup(indexName, values) ->
            rightTable.Indexes
            |> List.tryFind (fun i -> Catalog.eqName i.Name indexName)
            |> Option.bind (fun idx ->
                if values.Length <= idx.Columns.Length then
                    Some(
                        List.map2
                            (absorbedEquality rightName)
                            (List.truncate values.Length idx.Columns)
                            (List.ofArray values)
                    )
                else
                    None)
        | _ -> Some []

    /// 見積りの作業項目（明示スタック。「深さ優先の後置評価」を末尾再帰で回す）。
    type private EstimateWork =
        | Visit of Logical
        | ApplyFilter of Expr
        | ApplyJoin of float option

    /// 修飾付き列の異なり値数。
    /// 統計が無い・未修飾なら None。
    /// 列参照から "テーブル統計 × 列統計" を引く。
    let private columnWithStatsOf (statsOf: string -> TableStats option) (expr: Expr) =
        match Schema.tryAsColumnRef expr with
        | Some(r: ColumnRef) ->
            r.Qualifier
            |> Option.bind (
                statsOf
                >> Option.bind (fun stats ->
                    stats.Columns
                    |> List.tryFind (fun c -> SqlName.equal c.Name r.Name)
                    |> Option.map (fun c -> stats, c))
            )
        | None -> None

    /// 式の修飾子（列参照でなければ None）。
    let private qualifierOf (expr: Expr) =
        match Schema.tryAsColumnRef expr with
        | Some(r: ColumnRef) -> r.Qualifier
        | None -> None

    /// NULL の割合。
    let private nullRate (stats: TableStats) (column: ColumnStats) =
        if stats.RowCount <= 0L then
            0.0
        else
            float column.Nulls / float stats.RowCount

    /// 結合キーに使える等値か（両側が同じ型の列参照）。
    /// 実行時の `chooseJoinAlgorithmWith` が結合キーにする条件（`tryPair`）と同じで、
    /// 型が違う等値は結合キーにせず residual としてだけ評価する（入れ子ループ）。
    /// 結合順のコスト見積りはこの判定を通った辺だけを INL / マージ / ハッシュの候補にする。
    let isJoinKeyEquality (schema: Schema) =
        function
        | Expr.Binary(BinaryOp.Eq, a, b) ->
            match Schema.tryAsColumnRef a, Schema.tryAsColumnRef b with
            | Some ca, Some cb ->
                match Schema.resolve ca schema, Schema.resolve cb schema with
                | Ok ia, Ok ib ->
                    match schema.Columns.[ia].DataType, schema.Columns.[ib].DataType with
                    | Some lt, Some rt -> lt = rt
                    | _ -> false
                | Error _, _
                | _, Error _ -> false
            | _ -> false
        | _ -> false

    /// 1本の等値述語の結合選択性。
    /// 両側の列に NDV 統計があるときだけ `Some` を返す。
    /// 等値比較は NULL 同士を一致させないため、NULL 率を掛けて除く。
    let equalitySelectivity (statsOf: string -> TableStats option) =
        function
        | Expr.Binary(BinaryOp.Eq, a, b) ->
            match qualifierOf a, qualifierOf b with
            | Some qa, Some qb when not (Catalog.eqName qa qb) ->
                match columnWithStatsOf statsOf a, columnWithStatsOf statsOf b with
                | Some(sa, ca), Some(sb, cb) when ca.NDV > 0L && cb.NDV > 0L ->
                    Some((1.0 - nullRate sa ca) * (1.0 - nullRate sb cb) / float (max ca.NDV cb.NDV))
                | _ -> None
            | _ -> None
        | _ -> None

    /// 型ごとの管理メモリ概算（バイト）。
    let dataTypeBytes =
        function
        | DataType.Boolean -> 1
        | DataType.Integer -> 8
        | DataType.Real -> 8
        | DataType.Decimal -> 16
        | DataType.Date -> 4
        | DataType.Timestamp -> 8
        | DataType.Uuid -> 16
        | DataType.Text -> 32
        | DataType.Blob -> 32
        | DataType.Null -> 0

    /// 1行の管理メモリ概算（行ヘッダ + 列の型別サイズ）。型が不明な列は 16 バイト。
    let rowBytesOfTypes (types: DataType option seq) =
        24.0
        + (types
           |> Seq.sumBy (fun dt -> float (dt |> Option.map dataTypeBytes |> Option.defaultValue 16)))

    /// ハッシュ結合の build 側が作業メモリを超えるときに、ディスクへ退避する行数。
    /// 実際の退避はパーティション単位なので、はみ出した割合で近似する。
    /// `workMemBytes < 0` は無制限。
    /// 退避量は結合順のコストと `EXPLAIN` の `spill=` で同じ値を使う。
    let spilledBuildRows (buildRows: float) (buildRowBytes: float) (workMemBytes: float) =
        let buildBytes = buildRows * buildRowBytes

        if workMemBytes < 0.0 || buildBytes <= workMemBytes then
            0.0
        else
            buildRows * (1.0 - workMemBytes / buildBytes)

    /// 同じ名前の集合か（大文字小文字を区別しない）。
    let private sameNameSet (a: string list) (b: string list) =
        List.length a = List.length b
        && a |> List.forall (fun x -> b |> List.exists (SqlName.equal x))

    /// 修飾名からテーブル統計と列統計を引く。
    let private joinStatsFor (statsOf: string -> TableStats option) (table: string) (name: string) =
        statsOf table
        |> Option.bind (fun stats ->
            stats.Columns
            |> List.tryFind (fun c -> SqlName.equal c.Name name)
            |> Option.map (fun c -> stats, c))

    /// 索引の先頭 prefix に対する NDV を引く。
    let private joinPrefixNdv (statsOf: string -> TableStats option) (table: string) (names: string list) =
        statsOf table
        |> Option.bind (fun stats ->
            stats.Indexes
            |> List.tryPick (fun index ->
                let prefix = List.truncate names.Length index.Columns

                if List.length prefix = names.Length && sameNameSet prefix names then
                    index.PrefixNDV |> List.tryItem (names.Length - 1)
                else
                    None))

    /// 同じテーブル対の複合等値 1グループの選択性。
    let private joinGroupSelectivity
        (statsOf: string -> TableStats option)
        (leftTable, rightTable)
        (items: (string * string) list)
        =
        let leftNames = items |> List.map fst
        let rightNames = items |> List.map snd

        let nullFactor =
            let factors =
                (leftNames
                 |> List.choose (fun name ->
                     joinStatsFor statsOf leftTable name
                     |> Option.map (fun (stats, column) -> 1.0 - nullRate stats column)))
                @ (rightNames
                   |> List.choose (fun name ->
                       joinStatsFor statsOf rightTable name
                       |> Option.map (fun (stats, column) -> 1.0 - nullRate stats column)))

            if List.length factors = List.length leftNames + List.length rightNames then
                Some(factors |> List.fold (*) 1.0)
            else
                None

        let indexNdv =
            [
                joinPrefixNdv statsOf leftTable leftNames
                joinPrefixNdv statsOf rightTable rightNames
            ]
            |> List.choose id
            |> List.sortDescending
            |> List.tryHead

        match nullFactor, indexNdv with
        | Some nullFactor, Some ndv when ndv > 0L -> Some(nullFactor / float ndv)
        | _ ->
            let perColumn =
                List.zip leftNames rightNames
                |> List.map (fun (leftName, rightName) ->
                    match joinStatsFor statsOf leftTable leftName, joinStatsFor statsOf rightTable rightName with
                    | Some(sa, ca), Some(sb, cb) when ca.NDV > 0L && cb.NDV > 0L ->
                        Some((1.0 - nullRate sa ca) * (1.0 - nullRate sb cb) / float (max ca.NDV cb.NDV))
                    | _ -> None)

            if perColumn |> List.forall Option.isSome then
                Some(perColumn |> List.choose id |> List.fold (*) 1.0)
            else
                let known = perColumn |> List.choose id
                let unknown = perColumn |> List.filter Option.isNone |> List.length

                if List.isEmpty known then
                    None
                else
                    Some((known |> List.fold (*) 1.0) * 0.1 ** float unknown)

    /// 等値結合の選択性。
    /// 統計が欠けている等値は既定の 0.1 で穴埋めする。
    /// 同じテーブル対の複合等値は、索引 prefix NDV があればそれを使う。
    /// `Some` は「その結合でどれだけ行が減るか」、`None` は統計なし。
    let joinSelectivity (statsOf: string -> TableStats option) (on: Expr) =
        let groups =
            Predicate.splitConjuncts on
            |> List.choose (fun conjunct ->
                match conjunct with
                | Expr.Binary(BinaryOp.Eq, a, b) ->
                    match qualifierOf a, qualifierOf b with
                    | Some qa, Some qb when not (Catalog.eqName qa qb) -> Some conjunct
                    | _ -> None
                | _ -> None)
            |> List.choose (fun conjunct ->
                match conjunct with
                | Expr.Binary(BinaryOp.Eq, a, b) ->
                    match Schema.tryAsColumnRef a, Schema.tryAsColumnRef b with
                    | Some ca, Some cb ->
                        match ca.Qualifier, cb.Qualifier with
                        | Some qa, Some qb when not (Catalog.eqName qa qb) ->
                            Some((qa.ToLowerInvariant(), qb.ToLowerInvariant()), (ca.Name, cb.Name))
                        | _ -> None
                    | _ -> None
                | _ -> None)
            |> List.groupBy fst
            |> List.map (fun (pair, items) -> pair, items |> List.map snd)

        if List.isEmpty groups then
            None
        else
            let estimated =
                groups
                |> List.map (fun (pair, items) -> joinGroupSelectivity statsOf pair items)

            if estimated |> List.forall Option.isNone then
                None
            else
                estimated |> List.map (Option.defaultValue 0.1) |> List.fold (*) 1.0 |> Some

    /// リテラル（負号付きを含む）の値。
    let private literalOf =
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

    /// 統計付きの列 NDV。
    let private ndvOfName (stats: TableStats) (name: string) =
        stats.Columns
        |> List.tryFind (fun c -> SqlName.equal c.Name name)
        |> Option.map _.NDV

    /// 数値リテラル。
    let private numericLiteral =
        literalOf >> Option.bind (SqlValue.asNumeric >> Option.map float)

    /// 否定述語の選択性を反転する。
    let private selectivityWithNegation (neg: bool) (value: float) =
        if neg then max 0.0 (1.0 - value) else value

    /// 列とリテラルの等値選択性（MCV を優先する）。
    let private eqToLiteralWithStats
        (statsOf: string -> TableStats option)
        (defaultSelectivity: float)
        (columnExpr: Expr)
        (literal: SqlValue)
        =
        match columnWithStatsOf statsOf columnExpr with
        | Some(stats, column) when column.NDV > 0L ->
            if SqlValue.isNull literal then
                nullRate stats column
            elif stats.RowCount <= 0L then
                defaultSelectivity
            else
                let total = float stats.RowCount
                let key = SqlLiteral.toSqlLiteral literal

                match column.Mcv |> List.tryFind (fun (value, _) -> value = key) with
                | Some(_, rows) -> min 1.0 (float rows / total)
                | None ->
                    let nulls = float column.Nulls / total
                    let mcvShare = column.Mcv |> List.sumBy (snd >> float) |> fun rows -> rows / total
                    let rest = max 0.0 (min 1.0 (1.0 - nulls - mcvShare))
                    rest / float (max 1L (column.NDV - int64 column.Mcv.Length))
        | _ -> defaultSelectivity

    /// ヒストグラムの累積分布を返す（未収集なら None）。
    let private histogramCdf (histogram: 'a list) (v: 'a) =
        if List.isEmpty histogram then
            None
        else
            let below = histogram |> List.filter (fun x -> x <= v) |> List.length
            Some(float below / float histogram.Length)

    /// 列とリテラルの範囲選択性。
    let private rangeToLiteralWithStats
        (statsOf: string -> TableStats option)
        (rangeSelectivity: float)
        (columnExpr: Expr)
        (literal: SqlValue)
        (op: BinaryOp)
        =
        match columnWithStatsOf statsOf columnExpr, SqlValue.asNumeric literal with
        | Some(_, column), Some n ->
            let v = float n

            match histogramCdf column.Histogram v with
            | Some cdf ->
                match op with
                | BinaryOp.Gt
                | BinaryOp.Ge -> 1.0 - cdf
                | BinaryOp.Lt
                | BinaryOp.Le -> cdf
                | _ -> rangeSelectivity
            | None ->
                match column.Min, column.Max with
                | Some lo, Some hi when hi > lo ->
                    let selectivity =
                        match op with
                        | BinaryOp.Gt
                        | BinaryOp.Ge -> (hi - v) / (hi - lo)
                        | BinaryOp.Lt
                        | BinaryOp.Le -> (v - lo) / (hi - lo)
                        | _ -> rangeSelectivity

                    max 0.0 (min 1.0 selectivity)
                | _ -> rangeSelectivity
        | _ -> rangeSelectivity

    /// 比較演算子を左右反転する。
    let private flipComparison =
        function
        | BinaryOp.Gt -> BinaryOp.Lt
        | BinaryOp.Ge -> BinaryOp.Le
        | BinaryOp.Lt -> BinaryOp.Gt
        | BinaryOp.Le -> BinaryOp.Ge
        | other -> other

    /// 1本の述語の選択性（統計が無ければ既定値）。
    let private filterConjunctSelectivity (statsOf: string -> TableStats option) (conjunct: Expr) =
        let defaultSelectivity = 0.1
        let rangeSelectivity = 1.0 / 3.0

        match conjunct with
        | Expr.Binary(BinaryOp.Eq, a, b) ->
            match columnWithStatsOf statsOf a, columnWithStatsOf statsOf b with
            | Some(sa, ca), Some(sb, cb) when ca.NDV > 0L && cb.NDV > 0L ->
                (1.0 - nullRate sa ca) * (1.0 - nullRate sb cb) / float (max ca.NDV cb.NDV)
            | _ ->
                match literalOf b with
                | Some literal -> eqToLiteralWithStats statsOf defaultSelectivity a literal
                | None ->
                    match literalOf a with
                    | Some literal -> eqToLiteralWithStats statsOf defaultSelectivity b literal
                    | None -> defaultSelectivity
        | Expr.Binary(BinaryOp.Ne, a, b) ->
            let equality =
                match literalOf b with
                | Some literal -> eqToLiteralWithStats statsOf defaultSelectivity a literal
                | None ->
                    match literalOf a with
                    | Some literal -> eqToLiteralWithStats statsOf defaultSelectivity b literal
                    | None -> defaultSelectivity

            max 0.0 (1.0 - equality)
        | Expr.Binary(BinaryOp.Gt | BinaryOp.Ge | BinaryOp.Lt | BinaryOp.Le as op, a, b) ->
            match literalOf b with
            | Some literal -> rangeToLiteralWithStats statsOf rangeSelectivity a literal op
            | None ->
                match literalOf a with
                | Some literal -> rangeToLiteralWithStats statsOf rangeSelectivity b literal (flipComparison op)
                | None -> defaultSelectivity
        | Expr.Between(x, low, high, neg) ->
            let selectivity =
                match columnWithStatsOf statsOf x, numericLiteral low, numericLiteral high with
                | Some(_, column), Some lo, Some hi ->
                    match histogramCdf column.Histogram hi, histogramCdf column.Histogram lo with
                    | Some highCdf, Some lowCdf -> max 0.0 (min 1.0 (highCdf - lowCdf))
                    | _ ->
                        match column.Min, column.Max with
                        | Some minValue, Some maxValue when maxValue > minValue ->
                            max 0.0 (min 1.0 ((hi - lo) / (maxValue - minValue)))
                        | _ -> rangeSelectivity
                | _ -> rangeSelectivity

            selectivityWithNegation neg selectivity
        | Expr.InList(x, values, neg) ->
            let perValue =
                values
                |> List.tryHead
                |> Option.map (fun v ->
                    literalOf v
                    |> Option.map (eqToLiteralWithStats statsOf defaultSelectivity x)
                    |> Option.defaultValue defaultSelectivity)
                |> Option.defaultValue defaultSelectivity

            selectivityWithNegation neg (min 1.0 (float (List.length values) * perValue))
        | Expr.Like(x, pattern, neg, _) ->
            let selectivity =
                let baseSelectivity =
                    match literalOf pattern with
                    | Some(SqlValue.Text text) when text.EndsWith("%", System.StringComparison.Ordinal) ->
                        if text.Length <= 1 then 1.0 else 0.1
                    | Some(SqlValue.Text _) -> 0.2
                    | _ -> defaultSelectivity

                match columnWithStatsOf statsOf x with
                | Some(stats, column) -> baseSelectivity * (1.0 - nullRate stats column)
                | None -> baseSelectivity

            selectivityWithNegation neg selectivity
        | Expr.IsNull(x, neg) ->
            let selectivity =
                match columnWithStatsOf statsOf x with
                | Some(stats, column) -> nullRate stats column
                | None -> defaultSelectivity

            selectivityWithNegation neg selectivity
        | _ -> defaultSelectivity

    let private filterSelectivity (statsOf: string -> TableStats option) =
        Predicate.splitConjuncts
        >> List.map (filterConjunctSelectivity statsOf)
        >> List.fold (*) 1.0

    /// 行数の見積り。
    /// 統計が無ければ従来の定数だけを使う。
    /// 結合順の決定（`PlanBuilder`）とインデックス入れ子ループの判断（`chooseJoinAlgorithm`）で同じ値を使う。
    /// 上限なし・未知は `infinity`。
    /// <remarks>
    /// 基本値は走査方法だけで決め、`Filter` 1段ごとに 0.1倍（下限 1）、`Limit` は件数。
    /// 統計があるときは走査の基数を実測行数にし、等値結合を `1 / max(NDV_l, NDV_r)` で見積もる。
    /// 統計が無い結合は従来どおり左右の小さい方（外側の行数は内側で絞られる）。
    /// </remarks>
    /// 選択性を行数へ適用する（下限 1行）。
    let private scaledRows (rows: float) (selectivity: float) =
        if rows <= 0.0 then 0.0 else max 1.0 (rows * selectivity)

    /// 索引の prefix NDV から選択性を求める。
    let private estimateIndexSelectivity (stats: TableStats) (indexName: string) (count: int) =
        stats.Indexes
        |> List.tryFind (fun index -> Catalog.eqName index.Name indexName)
        |> Option.bind (fun index ->
            if count <= 0 || count > index.Columns.Length then
                None
            else
                match index.PrefixNDV |> List.tryItem (count - 1) with
                | Some ndv when ndv > 0L -> Some(1.0 / float ndv)
                | _ ->
                    let prefix = List.truncate count index.Columns
                    let selectivities = prefix |> List.map (ndvOfName stats)

                    if selectivities |> List.exists Option.isNone then
                        None
                    else
                        selectivities
                        |> List.choose id
                        |> List.fold (fun acc n -> if n > 0L then acc * (1.0 / float n) else acc) 1.0
                        |> Some)

    /// 走査ノードの基本行数を求める。
    let private estimateBaseRows (statsOf: string -> TableStats option) =
        function
        | Logical.Scan(name, method) ->
            match statsOf name with
            | Some stats ->
                let rows = float stats.RowCount

                match method with
                | ScanMethod.PkLookup _ -> 1.0
                | ScanMethod.IndexLookup(indexName, values) ->
                    match estimateIndexSelectivity stats indexName values.Length with
                    | Some selectivity -> scaledRows rows selectivity
                    | None -> if rows <= 0.0 then 0.0 else max 1.0 (rows / 10.0)
                | ScanMethod.RangeScan _
                | ScanMethod.IndexRangeScan _ -> if rows <= 0.0 then 0.0 else max 1.0 (rows / 3.0)
                | ScanMethod.SeqScan -> rows
            | None ->
                match method with
                | ScanMethod.PkLookup _ -> 1.0
                | ScanMethod.IndexLookup _ -> 2.0
                | ScanMethod.RangeScan _
                | ScanMethod.IndexRangeScan _ -> 100.0
                | ScanMethod.SeqScan -> 1000.0
        | Logical.Values _ -> 1.0
        | _ -> System.Double.PositiveInfinity

    /// 見積りの作業項目を末尾再帰で畳み込む。
    [<TailCall>]
    let rec private estimateRowsGo (statsOf: string -> TableStats option) (values: float list) =
        function
        | [] ->
            match values with
            | v :: _ -> v
            | [] -> System.Double.PositiveInfinity
        | Visit node :: rest ->
            match node with
            | Logical.Filter(predicate, child) ->
                estimateRowsGo statsOf values (Visit child :: ApplyFilter predicate :: rest)
            | Logical.Limit(_, n, _) when n >= 0 -> estimateRowsGo statsOf (float n :: values) rest
            | Logical.Limit(_, _, child)
            | Logical.Project(_, child)
            | Logical.Sort(_, child)
            | Logical.Distinct child
            | Logical.Having(_, _, _, child) -> estimateRowsGo statsOf values (Visit child :: rest)
            | Logical.Aggregate(groupBy, _, _) when List.isEmpty groupBy -> estimateRowsGo statsOf (1.0 :: values) rest
            | Logical.Aggregate(_, _, child) -> estimateRowsGo statsOf values (Visit child :: rest)
            | Logical.Join(_, _, on, left, right) ->
                estimateRowsGo
                    statsOf
                    values
                    (Visit left :: Visit right :: ApplyJoin(joinSelectivity statsOf on) :: rest)
            | leaf -> estimateRowsGo statsOf (estimateBaseRows statsOf leaf :: values) rest
        | ApplyFilter predicate :: rest ->
            match values with
            | v :: tail ->
                let selectivity = filterSelectivity statsOf predicate
                let filtered = if v <= 0.0 then 0.0 else max 1.0 (v * selectivity)
                estimateRowsGo statsOf (filtered :: tail) rest
            | [] -> estimateRowsGo statsOf (1.0 :: values) rest
        | ApplyJoin selectivity :: rest ->
            match values with
            | r :: l :: tail ->
                let joined =
                    match selectivity with
                    | Some sel when System.Double.IsFinite l && System.Double.IsFinite r ->
                        if l <= 0.0 || r <= 0.0 then 0.0 else max 1.0 (l * r * sel)
                    | _ -> min l r

                estimateRowsGo statsOf (joined :: tail) rest
            | _ -> estimateRowsGo statsOf (System.Double.PositiveInfinity :: values) rest

    let estimateRowsWith (statsOf: string -> TableStats option) (tree: Logical) =
        estimateRowsGo statsOf [] [ Visit tree ]

    /// 列参照がスキーマ列に一致するか。
    let private joinRefMatches (r: ColumnRef) (c: OutputColumn) =
        match r.Qualifier with
        | Some q ->
            SqlName.equal c.Name r.Name
            && match c.Qualifier with
               | Some cq -> SqlName.equal cq q
               | None -> false
        | None -> SqlName.equal c.Name r.Name

    /// 列参照が一意に指す列位置を返す。
    let private joinColumnIndex (schema: Schema) (expr: Expr) =
        match Schema.tryAsColumnRef expr with
        | None -> None
        | Some r ->
            let hits =
                schema.Columns
                |> Array.indexed
                |> Array.filter (fun (_, c) -> joinRefMatches r c)

            match hits with
            | [| i, _ |] -> Some i
            | _ -> None

    /// 左右の同型な列参照を結合キー位置の対にする。
    let private joinTryPair (leftSchema: Schema) (rightSchema: Schema) (l: Expr, r: Expr) =
        match joinColumnIndex leftSchema l, joinColumnIndex rightSchema r with
        | Some li, Some ri ->
            match leftSchema.Columns.[li].DataType, rightSchema.Columns.[ri].DataType with
            | Some lt, Some rt when lt = rt -> Some(li, ri)
            | _ -> None
        | _ -> None

    /// ON から同型の等値キーと残差を抽出する。
    let private joinKeyPairs (leftSchema: Schema) (rightSchema: Schema) (on: Expr) =
        let keyPairs, residual =
            on
            |> Predicate.splitConjuncts
            |> List.fold
                (fun (keys, residual) conjunct ->
                    match conjunct with
                    | Expr.Binary(BinaryOp.Eq, a, b) ->
                        let pair =
                            match joinTryPair leftSchema rightSchema (a, b) with
                            | Some p -> Some p
                            | None -> joinTryPair leftSchema rightSchema (b, a)

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

    /// キー列名の集合が列集合とちょうど一致するか。
    let private joinCoversAll (keyNames: string array) (cols: string list) =
        List.length cols = keyNames.Length
        && cols
           |> List.forall (fun c -> keyNames |> Array.exists (fun k -> Catalog.eqName c k))
        && keyNames
           |> Array.forall (fun k -> cols |> List.exists (fun c -> Catalog.eqName c k))

    /// 右キー列名から対応する左キー位置を引く。
    let private joinFindLeft (rightSchema: Schema) (keys: JoinKeys) (colName: string) =
        keys.RightIndices
        |> Array.tryFindIndex (fun ri -> Catalog.eqName rightSchema.Columns.[ri].Name colName)
        |> Option.map (fun keyPos -> keys.LeftIndices.[keyPos])

    /// 右キー列集合を左キー位置の配列へ変換する。
    let private joinLeftIndices (rightSchema: Schema) (keys: JoinKeys) (cols: string list) =
        let lefts = cols |> List.map (joinFindLeft rightSchema keys)

        if lefts |> List.forall Option.isSome then
            Some(lefts |> List.choose id |> Array.ofList)
        else
            None

    /// 右テーブルの索引 prefix NDV を引く（無ければ最大値）。
    let private joinIndexPrefixNdv
        (statsOf: string -> TableStats option)
        (rightName: string)
        (keyNames: string array)
        (indexName: string)
        =
        statsOf rightName
        |> Option.bind (fun stats -> stats.Indexes |> List.tryFind (fun i -> Catalog.eqName i.Name indexName))
        |> Option.bind (fun index -> index.PrefixNDV |> List.tryItem (keyNames.Length - 1))
        |> Option.defaultValue System.Int64.MaxValue

    /// 右テーブルを引く方法（主キー優先・索引は prefix NDV 最小）を選ぶ。
    let private joinRightLookup
        (statsOf: string -> TableStats option)
        (rightName: string)
        (rightSchema: Schema)
        (rightTable: TableSchema)
        (keys: JoinKeys)
        =
        let keyNames =
            keys.RightIndices |> Array.map (fun i -> rightSchema.Columns.[i].Name)

        let pkNames =
            rightTable.PrimaryKey |> List.map (fun i -> rightTable.Columns.[i].Name)

        if List.isEmpty pkNames || not (joinCoversAll keyNames pkNames) then
            None
        else
            joinLeftIndices rightSchema keys pkNames
            |> Option.map (fun lefts -> JoinLookup.PrimaryKey, lefts)
        |> Option.orElseWith (fun () ->
            rightTable.Indexes
            |> List.choose (fun idx ->
                let prefix = List.truncate keyNames.Length idx.Columns

                if joinCoversAll keyNames prefix then
                    Some(joinIndexPrefixNdv statsOf rightName keyNames idx.Name, JoinLookup.Index idx.Name, prefix)
                else
                    None)
            |> List.sortBy (fun (ndv, _, _) -> ndv)
            |> List.tryPick (fun (_, lookup, cols) ->
                joinLeftIndices rightSchema keys cols |> Option.map (fun lefts -> lookup, lefts)))

    /// 他方のキー順に並んだ列名から、自側のキー位置を引く。
    [<TailCall>]
    let rec private joinOrderGo (otherKeyNames: string array) (ownIndices: int array) (acc: int list) =
        function
        | [] -> Some(List.rev acc |> List.toArray)
        | n :: rest ->
            match otherKeyNames |> Array.tryFindIndex (fun m -> SqlName.equal m n) with
            | Some i -> joinOrderGo otherKeyNames ownIndices (ownIndices.[i] :: acc) rest
            | None -> None

    /// 走査順の列名を自側のキー位置へ並べ替える。
    let private joinOrderBy (targetNames: string list) (otherKeyNames: string array) (ownIndices: int array) =
        joinOrderGo otherKeyNames ownIndices [] targetNames

    /// 左右のキー名が同じ対応で並んでいるか。
    let private joinPairsAligned
        (leftKeyNames: string array)
        (rightKeyNames: string array)
        (leftNames: string list)
        (rightNames: string list)
        =
        if
            List.length leftNames <> List.length rightNames
            || List.length leftNames <> leftKeyNames.Length
        then
            false
        else
            leftNames
            |> List.mapi (fun j leftName ->
                match leftKeyNames |> Array.tryFindIndex (fun n -> SqlName.equal n leftName) with
                | Some i -> SqlName.equal rightKeyNames.[i] rightNames.[j]
                | None -> false)
            |> List.forall id

    /// 両入力がキー順ならマージ結合用のキーへ整える。
    let private joinMergeKeys
        (tableOf: string -> TableSchema option)
        (leftTree: Logical)
        (rightTree: Logical)
        (leftSchema: Schema)
        (rightSchema: Schema)
        (keys: JoinKeys)
        =
        let leftOrder = scanOrder tableOf leftTree
        let rightOrder = scanOrder tableOf rightTree

        match
            alignJoinKeys leftSchema keys.LeftIndices leftOrder, alignJoinKeys rightSchema keys.RightIndices rightOrder
        with
        | Some(leftKeys, leftNames), Some(rightKeys, rightNames) ->
            let leftKeyNames =
                keys.LeftIndices |> Array.map (fun i -> leftSchema.Columns.[i].Name)

            let rightKeyNames =
                keys.RightIndices |> Array.map (fun i -> rightSchema.Columns.[i].Name)

            match leftNames, rightNames with
            | [], [] ->
                Some
                    { keys with
                        LeftIndices = leftKeys
                        RightIndices = rightKeys
                    }
            | _, [] ->
                joinOrderBy leftNames leftKeyNames keys.RightIndices
                |> Option.map (fun rk ->
                    { keys with
                        LeftIndices = leftKeys
                        RightIndices = rk
                    })
            | [], _ ->
                joinOrderBy rightNames rightKeyNames keys.LeftIndices
                |> Option.map (fun lk ->
                    { keys with
                        LeftIndices = lk
                        RightIndices = rightKeys
                    })
            | _ when joinPairsAligned leftKeyNames rightKeyNames leftNames rightNames ->
                Some
                    { keys with
                        LeftIndices = leftKeys
                        RightIndices = rightKeys
                    }
            | _ -> None
        | _ -> None

    /// 結合アルゴリズムを選ぶ。
    /// - 等値キーなし: 入れ子ループ
    /// - 左が小さい（上限 32行）かつ右を主キー / 索引の前置で引ける: インデックス入れ子ループ
    ///   （右の `Filter` の述語と、走査が吸収した等値を residual として評価する）
    /// - 両入力が結合キー順に走査できる: マージ結合
    /// - それ以外の等値結合: ハッシュ結合
    let chooseJoinAlgorithmWith
        (statsOf: string -> TableStats option)
        (tableOf: string -> TableSchema option)
        (leftSchema: Schema)
        (rightSchema: Schema)
        (rightTable: TableSchema option)
        (leftTree: Logical)
        (rightTree: Logical)
        (on: Expr)
        =
        match joinKeyPairs leftSchema rightSchema on with
        | None -> JoinAlgorithm.NestedLoop, None, None
        | Some keys ->
            let lookup =
                match scanOfRight rightTree, rightTable with
                | Some(rightName, method, rightPreds), Some table ->
                    absorbedPredicates table rightName method
                    |> Option.bind (fun absorbed ->
                        joinRightLookup statsOf rightName rightSchema table keys
                        |> Option.map (fun (target, leftIndices) -> target, leftIndices, rightPreds @ absorbed))
                | _ -> None

            match lookup with
            | Some(target, leftIndices, rightPreds) when estimateRowsWith statsOf leftTree <= 32.0 ->
                let residual = Option.toList keys.Residual @ rightPreds |> Predicate.joinConjuncts
                JoinAlgorithm.IndexNestedLoop target, Some { keys with Residual = residual }, Some leftIndices
            | _ ->
                match joinMergeKeys tableOf leftTree rightTree leftSchema rightSchema keys with
                | Some aligned -> JoinAlgorithm.MergeJoin, Some aligned, None
                | None -> JoinAlgorithm.HashJoin, Some keys, None

    let private colOf (tn: string) =
        function
        | Expr.Column { Qualifier = Some q; Name = n } when q.ToLowerInvariant() = tn -> Some n
        | _ -> None

    /// 指定列への等値リテラルを取り出す。
    let private rangeLiteralFor (tn: string) (colName: string) (a: Expr) (b: Expr) =
        match colOf tn a, literalOf b with
        | Some n, Some v when Catalog.eqName n colName -> Some v
        | _ -> None

    /// 既に見つかった値を優先する。
    let private pickFirst (found: 'a option) (current: 'a option) =
        match found with
        | Some v -> Some v
        | None -> current

    /// 連言を走査して列の上下界を畳み込む。
    [<TailCall>]
    let rec private accumulateRangeBounds
        (tn: string)
        (colName: string)
        (lower: SqlValue option)
        (upper: SqlValue option)
        =
        function
        | [] -> lower, upper
        | e :: tail ->
            match e with
            | Expr.Binary((BinaryOp.Gt | BinaryOp.Ge), a, b) ->
                accumulateRangeBounds
                    tn
                    colName
                    (pickFirst (rangeLiteralFor tn colName a b) lower)
                    (pickFirst (rangeLiteralFor tn colName b a) upper)
                    tail
            | Expr.Binary((BinaryOp.Lt | BinaryOp.Le), a, b) ->
                accumulateRangeBounds
                    tn
                    colName
                    (pickFirst (rangeLiteralFor tn colName b a) lower)
                    (pickFirst (rangeLiteralFor tn colName a b) upper)
                    tail
            | Expr.Between(x, lo, hi, false) ->
                match colOf tn x with
                | Some n when Catalog.eqName n colName ->
                    match literalOf lo, literalOf hi with
                    | Some lv, Some hv -> accumulateRangeBounds tn colName (Some lv) (Some hv) tail
                    | _ -> accumulateRangeBounds tn colName lower upper tail
                | _ -> accumulateRangeBounds tn colName lower upper tail
            | Expr.Like(x, p, false, None) ->
                match colOf tn x, literalOf p with
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

                        accumulateRangeBounds tn colName lower' upper' tail
                    else
                        accumulateRangeBounds tn colName lower upper tail
                | _ -> accumulateRangeBounds tn colName lower upper tail
            | _ -> accumulateRangeBounds tn colName lower upper tail

    let private rangeBounds (tn: string) (colName: string) =
        List.collect Predicate.splitConjuncts
        >> accumulateRangeBounds tn colName None None

    /// WHERE から指定テーブルの等値条件を列挙する。
    let private scanEqualitiesOf (tn: string) =
        List.collect Predicate.splitConjuncts
        >> List.choose (function
            | Expr.Binary(BinaryOp.Eq, a, b) ->
                match colOf tn a, literalOf b with
                | Some n, Some v when not (SqlValue.isNull v) -> Some(n, v)
                | _ ->
                    match colOf tn b, literalOf a with
                    | Some n, Some v when not (SqlValue.isNull v) -> Some(n, v)
                    | _ -> None
            | _ -> None)

    /// リテラルの型が列の宣言型と合うか。
    let private scanTypeMatches (schema: TableSchema) (colName: string) (value: SqlValue) =
        match SqlValue.dataType value with
        | Ok t ->
            schema.Columns
            |> List.exists (fun c -> Catalog.eqName c.Name colName && c.DataType = t)
        | Error _ -> false

    /// 指定列へ束縛できる等値リテラルを探す。
    let private scanFindValue (b: TableBinding) (predicates: Expr list) (tn: string) (colName: string) =
        scanEqualitiesOf (tn.ToLowerInvariant()) predicates
        |> List.tryPick (fun (n, v) ->
            if Catalog.eqName n colName && scanTypeMatches b.Schema n v then
                Some v
            else
                None)

    /// 範囲条件を持つ先頭索引列から IndexRangeScan を組み立てる。
    let private scanTryIndexRange (b: TableBinding) (catalog: Catalog) (predicates: Expr list) (tn: string) =
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
                        | Some lv when scanTypeMatches b.Schema c lv -> true
                        | _ -> false)

                match rangeColIdx with
                | Some idx ->
                    let rangeCol = cols.[idx]
                    let lo, hi = rangeBounds (tn.ToLowerInvariant()) rangeCol predicates

                    match lo with
                    | Some lv when scanTypeMatches b.Schema rangeCol lv ->
                        let upper = hi |> Option.filter (fun v -> scanTypeMatches b.Schema rangeCol v)
                        let prefixCols = cols |> List.take idx
                        let prefixVals = prefixCols |> List.map (fun c -> scanFindValue b predicates tn c)

                        if prefixVals |> List.forall Option.isSome then
                            let lowerKey = (prefixVals |> List.choose id) @ [ lv ] |> Array.ofList

                            let upperKey =
                                upper
                                |> Option.map (fun uv -> (prefixVals |> List.choose id) @ [ uv ] |> Array.ofList)

                            Some(ScanMethod.IndexRangeScan(i.IndexName, lowerKey, upperKey))
                        else
                            None
                    | _ -> None
                | None -> None)

    /// 索引の先頭から連続する等値列の値を取る。
    let private scanEqualityPrefix (b: TableBinding) (predicates: Expr list) (tn: string) (i: IndexDefinition) =
        i.Columns
        |> List.map (scanFindValue b predicates tn)
        |> List.takeWhile Option.isSome
        |> List.choose id

    /// 最長の等値プレフィックスを持つ索引を選ぶ。
    let private scanTryIndexEquality (b: TableBinding) (catalog: Catalog) (predicates: Expr list) (tn: string) =
        Catalog.indexesOf b.RealName catalog
        |> List.choose (fun i ->
            let values = scanEqualityPrefix b predicates tn i
            if List.isEmpty values then None else Some(i, values))
        |> List.sortByDescending (fun (_, values) -> List.length values)
        |> List.tryHead

    /// 等値と範囲のうち、より多くの列を絞れる方を選ぶ。
    let private scanTryIndexMethod (b: TableBinding) (catalog: Catalog) (predicates: Expr list) (tn: string) =
        match scanTryIndexEquality b catalog predicates tn, scanTryIndexRange b catalog predicates tn with
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

    /// WHERE から等価条件を探して走査方法を決める。
    /// 複合主キーは全列の等値条件が揃ったときだけ PkLookup にし,
    /// 複合索引は先頭から連続する等値列の範囲で IndexLookup にする。
    let chooseScanMethod (b: TableBinding) (catalog: Catalog) (predicates: Expr list) =
        let tn = refName b
        let pkNames = b.Schema.PrimaryKey |> List.map (fun i -> b.Schema.Columns.[i].Name)
        let pkValues = pkNames |> List.map (scanFindValue b predicates tn)

        if not pkNames.IsEmpty && pkValues |> List.forall Option.isSome then
            ret (ScanMethod.PkLookup(pkValues |> List.choose id |> Array.ofList))
        else if pkNames.Length = 1 then
            let pkCol = pkNames.Head
            let lower, upper = rangeBounds (tn.ToLowerInvariant()) pkCol predicates

            match lower with
            | Some lv when scanTypeMatches b.Schema pkCol lv ->
                let startKey = [| lv |]

                let endKey =
                    upper
                    |> Option.filter (fun v -> scanTypeMatches b.Schema pkCol v)
                    |> Option.map (fun v -> [| v |])

                ret (ScanMethod.RangeScan(tn, startKey, endKey))
            | _ ->
                match scanTryIndexMethod b catalog predicates tn with
                | Some m -> ret m
                | None -> ret ScanMethod.SeqScan
        else
            match scanTryIndexMethod b catalog predicates tn with
            | Some m -> ret m
            | None -> ret ScanMethod.SeqScan

    /// 走査キーの位置に等値リテラルが入っているか。
    let private scanMatches (names: string list) (values: SqlValue array) (name: string) (value: SqlValue) =
        names
        |> List.tryFindIndex (fun n -> Catalog.eqName n name)
        |> Option.exists (fun pos ->
            pos < values.Length
            && match SqlValue.compareValuesV values.[pos] value with
               | DbOk 0 -> true
               | _ -> false)

    /// 走査方法が同じ等値条件を保証するか。
    /// `PkLookup` / `IndexLookup` が吸収済みの `col = 定数` を Filter から落とすために使う
    /// （残しても結果は変わらないが、行ごとに二重評価になる）。
    let impliedByScan (catalog: Catalog) (b: TableBinding) (method: ScanMethod) (predicate: Expr) =
        let tn = (refName b).ToLowerInvariant()

        match predicate with
        | Expr.Binary(BinaryOp.Eq, a, c) ->
            match colOf tn a, literalOf c with
            | Some n, Some v -> Some(n, v)
            | _ ->
                match colOf tn c, literalOf a with
                | Some n, Some v -> Some(n, v)
                | _ -> None
        | _ -> None
        |> Option.exists (fun (name, value) ->
            match method with
            | ScanMethod.PkLookup values ->
                scanMatches (b.Schema.PrimaryKey |> List.map (fun i -> b.Schema.Columns.[i].Name)) values name value
            | ScanMethod.IndexLookup(indexName, values) ->
                Catalog.findIndex indexName catalog
                |> Option.exists (fun idx -> scanMatches idx.Columns values name value)
            | _ -> false)
