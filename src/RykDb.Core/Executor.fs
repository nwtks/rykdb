// ===========================================================================
// Executor — Volcano（イテレータ）方式の実行層。
// pull 演算子 `Op`（Done / Step / Compute）で組む。LIMIT / WHERE の
// 早期打ち切りが効き、無限長でもメモリは 1 行分。
// 設計（なぜ pull か・snapshot の固定）は docs/architecture.md §6.2、
// 落とし穴は docs/gotchas.md §2 / §3、NULL の扱いは docs/sql-reference.md §6.2。
// ===========================================================================

namespace RykDb.Core

open System

// ---------------------------------------------------------------------------
// 1. 行
// ---------------------------------------------------------------------------

/// 1 行 = 列順の SqlValue 配列。メモリ上は配列のまま、回さない。
type internal Row = SqlValue array

module internal Row =
    /// 全 NULL のときの等価キー用の印。文字列に現れない制御文字を使う。
    [<Literal>]
    let private nullTag = "\u0000\0NULL"

    /// 等価キー（DISTINCT / GROUP BY / ハッシュ結合のキーに使う）。
    /// NULL は SQL では「他のどの値とも等しくない」ので、
    /// ここでは NULL も NULL 同士で等しい として扱う
    /// （GROUP BY で NULL が 1 つのグループになる挙動に合わせるため）。
    let keyOf (row: Row) =
        row
        |> Array.map (fun v ->
            if SqlValue.isNull v then
                nullTag
            else
                SqlLiteral.toSqlLiteral v)
        |> String.concat "\u0001"

// ---------------------------------------------------------------------------
// 2. 出力スキーマ
// ---------------------------------------------------------------------------

/// 出力の 1 列。
type internal OutputColumn =
    {
        /// 表示名（別名があればそれ、無ければ列名）
        Name: string
        /// 修飾名（`t.id`）。修飾子付き参照の解決に使う。
        Qualifier: string option
        /// 型（式から導出できない場合は None）
        DataType: DataType option
    }

    member this.Display =
        match this.Qualifier with
        | Some q -> sprintf "%s.%s" q this.Name
        | None -> this.Name

    /// `SELECT a AS b` 相当（表示名だけ変える）。
    member this.As(label: string) =
        { this with
            Name = label
            Qualifier = None
        }

    /// `SELECT expr` で修飾子なしの列を作る。
    static member ofExpression (label: string) (dt: DataType option) =
        {
            Name = label
            Qualifier = None
            DataType = dt
        }

module internal OutputColumn =
    /// テーブルの 1 列から作る（修飾名はテーブル名か別名）。
    let ofTableColumn (table: string) (col: ColumnDefinition) =
        {
            Name = col.Name
            Qualifier = Some table
            DataType = Some col.DataType
        }

/// 演算子の出力スキーマ。列の解決と結果表示に使う。
type internal Schema =
    {
        Columns: OutputColumn array
        /// 参照可能なテーブル名（修飾子解決の候補）
        Tables: string list
    }

    static member empty = { Columns = [||]; Tables = [] }

    /// テーブルの全列を並べる。
    static member ofTable (table: string) (alias: string option) (schema: TableSchema) =
        let name = defaultArg alias table

        let cols =
            schema.Columns
            |> List.map (fun c -> OutputColumn.ofTableColumn name c)
            |> List.toArray

        { Columns = cols; Tables = [ name ] }

    /// 列に別名を付ける（表示名のみ変更）。
    member this.WithAlias (index: int) (label: string) =
        if index < 0 || index >= this.Columns.Length then
            this
        else
            let cols =
                this.Columns |> Array.mapi (fun i c -> if i = index then c.As label else c)

            { this with Columns = cols }

    /// 列を 1 つだけ残す。
    member this.Project1(index: int) =
        if index < 0 || index >= this.Columns.Length then
            { this with Columns = [||] }
        else
            { this with
                Columns = [| this.Columns.[index] |]
            }

    /// 横連結（JOIN 用）。
    member this.Concat(other: Schema) =
        {
            Columns = Array.append this.Columns other.Columns
            Tables = this.Tables @ other.Tables
        }

    /// ヘッダ行。
    member this.Header = this.Columns |> Array.map (fun c -> c.Display)

module internal Schema =
    let empty = Schema.empty

    /// 射影後のヘッダ行を作る。
    /// `SELECT *` は入力の列名をそのまま（修飾つき）。
    /// `SELECT id` は式を表示する。別名があればそれを使う。
    /// 入力スキーマのヘッダを返すと `SELECT id` が `people.id` になるので、
    /// 射影後の名前を組み立て直す必要がある。
    let projectHeader (schema: Schema) (items: SelectItem list) =
        match items with
        | [] -> schema.Header
        | _ ->
            items
            |> List.collect (fun (item: SelectItem) ->
                match item.Expression with
                | Expr.Star -> schema.Columns |> Array.toList |> List.map (fun c -> c.Name)
                | Expr.QualifiedStar q ->
                    schema.Columns
                    |> Array.filter (fun (c: OutputColumn) ->
                        match c.Qualifier with
                        | Some qual -> qual = q
                        | None -> false)
                    |> Array.toList
                    |> List.map (fun c -> c.Display)
                | other ->
                    match item.Alias with
                    | Some a -> [ a ]
                    | None -> [ Expr.toDisplayString other ])
            |> List.toArray

    /// 式を「列の直接参照」に読み替える。
    /// `EColumn` はそのまま。修飾子なしの裸の名前（`EName`）も列として扱う
    /// （パーサが WHERE / ORDER BY の裸の名前を `EName` にするため）。
    /// 集約呼び出しや式は列ではないので `None`。
    let tryAsColumnRef (e: Expr) =
        match e with
        | Expr.Column c -> Some c
        | Expr.Name n -> Some { Qualifier = None; Name = n }
        | _ -> None

    /// 列参照を解決する。見つからなければエラー、曖昧ならエラー。
    let resolve (schema: Schema) (r: ColumnRef) =
        let wanted = r.Display.ToLowerInvariant()

        let isMatch (c: OutputColumn) =
            let full = c.Display.ToLowerInvariant()
            let bare = c.Name.ToLowerInvariant()
            let qual = defaultArg c.Qualifier "" |> fun s -> s.ToLowerInvariant()

            match r.Qualifier with
            | Some q ->
                let q' = q.ToLowerInvariant()
                full = wanted || qual = q' && bare = r.Name.ToLowerInvariant()
            | None -> bare = wanted || qual = wanted

        let hits =
            schema.Columns
            |> Array.mapi (fun i c -> i, c)
            |> Array.filter (fun (_, c) -> isMatch c)
            |> Array.map fst
            |> Array.toList

        match hits with
        | [ i ] -> ret i
        | [] ->
            Error(
                DbError.name (
                    sprintf
                        "列 '%s' が存在しません（使える列: %s）"
                        r.Display
                        (if schema.Columns.Length = 0 then
                             "(なし)"
                         else
                             schema.Columns |> Array.map (fun c -> c.Display) |> String.concat ", ")
                )
            )
        | many ->
            Error(
                DbError.name (sprintf "列 '%s' が曖昧です（該当: %s）" r.Display (many |> List.map string |> String.concat ", "))
            )

    /// テーブル 1 つのスキーマを作る。
    let ofTable (table: string) (alias: string option) (schema: TableSchema) = Schema.ofTable table alias schema

// ---------------------------------------------------------------------------
// 3. Op — 不変の pull 演算子
// ---------------------------------------------------------------------------

/// 1 行取り出す演算子（不変）。
/// `Done`             = 終わった
/// `Step (row, next)` = `row` を返した。続きは `next`
/// `Compute f`        = `f` を呼んで 1 行取り出す
///
/// `Compute` が「ソースだけを包む 1 段」であることが要点。
/// 変換・フィルタ・結合はすべて「上流の `Op` を受け取って、
/// `unit -> ...` を返す関数」として書けるので、
/// ソース側の状態を壊さずに不変のまま合成できる。
/// `Step` の `next` を呼ぶまで次の行は計算されないので、
/// 無限長でもメモリは 1 行分だけ。
[<RequireQualifiedAccess>]
type internal Op =
    | Done
    /// 1 行と「その次」。`next` は評価されるので長い連鎖を組むには向かない
    /// （遅延が必要なときは `Compute` を使う）。
    | Step of SqlValue array * Op
    /// 「次に 1 行を返す関数」。引数は `unit` なので遅延評価される。
    /// ソース側が「まだ計算していない」長い系列を表すときは必ずこれを使う。
    | Compute of (unit -> Result<(SqlValue array * Op) option, DbError>)

module internal Op =
    /// 空の演算子。
    let empty = Op.Done

    /// 1 行だけの演算子。
    let ofRow (row: SqlValue array) = Op.Step(row, Op.Done)

    /// 配列を順に返す演算子。次の行は呼び出すたびに計算する。
    /// <remarks>
    /// `let rest = go (i + 1)` を先に評価すると、コンストラクト時に
    /// 行数ぶんのスタックを積む（実測: Debug で約 13 万段で Stack overflow。
    /// 10 万行の COUNT(*) スキャンは壁の約 76%）。`Compute` で 1 行ずつ作る（§2.1）。
    /// </remarks>
    let ofArray (rows: SqlValue array array) =
        let rec go (i: int) : Op =
            if i >= rows.Length then
                Op.Done
            else
                Op.Compute(fun () -> ret (Some(rows.[i], go (i + 1))))

        go 0

    /// 1 行取り出す。
    [<TailCall>]
    let rec next (op: Op) =
        match op with
        | Op.Done -> ret None
        | Op.Step(row, nxt) -> ret (Some(row, nxt))
        | Op.Compute f -> f ()

    /// 行を 1 つ変換する。失敗したら打切って Error。
    /// 変換後の行の「次」は `map` を通した残りにする
    /// （`nxt` をそのまま渡すと変換が 1 行だけに効き、次から元に戻る）。
    let rec map (f: SqlValue array -> Result<SqlValue array, DbError>) (src: Op) =
        Op.Compute(fun () ->
            next src
            >>= function
                | None -> ret None
                | Some(row, nxt) -> f row |> Result.map (fun r -> Some(r, map f nxt)))

    /// 述語で弾く。偽の行は読み飛ばして次へ進む。
    /// `p row >>= function` で分岐する（`if/then/else` だと
    /// `Result` の短絡で `Some(row, nxt)` だけが残ることがある）。
    let filter (p: SqlValue array -> Result<bool, DbError>) (src: Op) =
        let rec go (cur: Op) : Op =
            Op.Compute(fun () ->
                next cur
                >>= function
                    | None -> ret None
                    | Some(row, nxt) ->
                        let verdict = p row

                        match verdict with
                        | Ok true -> ret (Some(row, go nxt))
                        | Ok false -> next (go nxt)
                        | Error e -> err e)

        go src

    /// 先頭 n 行だけ。n <= 0 は「全部」。
    let rec take (n: int) (src: Op) =
        if n <= 0 then
            Op.Done
        else
            Op.Compute(fun () ->
                next src
                >>= function
                    | None -> ret None
                    | Some(row, nxt) -> ret (Some(row, take (n - 1) nxt)))

    /// 先頭 n 行を読み飛ばす。
    let skip (n: int) (src: Op) =
        let rec go (left: int) (cur: Op) =
            if left <= 0 then
                cur
            else
                Op.Compute(fun () ->
                    next cur
                    >>= function
                        | None -> ret None
                        | Some(_, nxt) -> next (go (left - 1) nxt))

        go n src

    /// 全行を配列に materialize する。
    /// <remarks>
    /// `match` + 再帰で書くとスタックが深くなる。
    /// `next` の直後に `>>=` を重ねると F# の末尾呼び出し最適化が効かず、
    /// 10 万行の集約で Stack overflow になる。
    /// ここでは「再帰 1 段 + ループ」にしてフレームが積み上がるのを防ぐ。
    /// </remarks>
    let toArray (op: Op) =
        let rows = ResizeArray<SqlValue array>()

        let rec go (cur: Op) =
            match next cur with
            | Error e -> Error e
            | Ok None -> Ok(rows.ToArray())
            | Ok(Some(row, nxt)) ->
                rows.Add row
                go nxt

        go op

// ---------------------------------------------------------------------------
// 4. リテラルとスカラー関数
// ---------------------------------------------------------------------------

module internal Scalar =
    /// リテラルを実行時値へ。
    let ofLiteral (l: Literal) =
        match l with
        | Literal.Null -> SqlValue.Null
        | Literal.Bool b -> SqlValue.Bool b
        | Literal.Int i -> SqlValue.Int i
        | Literal.Real f -> SqlValue.Real f
        | Literal.Decimal d -> SqlValue.Decimal d
        | Literal.String s -> SqlValue.Text s
        | Literal.Blob b -> SqlValue.Blob b

    /// LIKE のパターン照合。`%` は任意文字列、`_` は 1 文字。
    /// 正規表現に落とさずに手で照合する（LIKE の方言差を吸収するため）。
    /// LIKE のパターン照合。`%` は任意の文字列、`_` は 1 文字。
    /// 正規表現に落とさずに手で照合する（LIKE の方言差を吸収するため）。
    /// バックトラックしない。 `%` で後戻りする実装は、進捗しないステップが
    /// あると無限ループしうる（実際してプロセスが SIGABRT した）。
    /// ここでは「区間と隙間を左から 1 通りだけ試す」素直な走査にする。
    /// 計算量は O(区間数 × テキスト長)。無限ループしないことが最優先。
    let likeMatch (pattern: string) (text: string) =
        let p = pattern.ToCharArray()
        let t = text.ToCharArray()
        let m = p.Length
        let n = t.Length

        if m = 0 then
            n = 0
        else
            let rec collect (i: int) (cur: List<char>) (acc: List<char list>) =
                if i >= m then
                    List.rev (List.rev cur :: acc)
                elif p.[i] = '%' then
                    let acc2 = if List.isEmpty cur then acc else List.rev cur :: acc
                    collect (i + 1) [] acc2
                else
                    collect (i + 1) (p.[i] :: cur) acc

            let rec matchSegment (seg: List<char>) (ti: int) =
                match seg with
                | [] -> ti
                | c :: rest ->
                    if ti >= n then -1
                    elif c = '_' || c = t.[ti] then matchSegment rest (ti + 1)
                    else -1

            let segments = collect 0 [] []
            let count = List.length segments
            let anchoredEnd = p.[m - 1] <> '%'

            let rec go (idx: int) (pos: int) =
                if idx >= count then
                    not anchoredEnd || pos = n
                else
                    let seg = List.item idx segments
                    let isLast = idx = count - 1
                    let freeBefore = if idx = 0 then p.[0] = '%' else true
                    let len = List.length seg

                    if isLast && anchoredEnd && not freeBefore then
                        matchSegment seg pos = n
                    elif isLast && anchoredEnd then
                        let rec scan (k: int) =
                            if k + len > n then
                                false
                            else
                                match matchSegment seg k with
                                | -1 -> scan (k + 1)
                                | r -> r = n

                        scan pos
                    elif freeBefore then
                        let rec scan (k: int) =
                            if k + len > n then
                                false
                            else
                                match matchSegment seg k with
                                | -1 -> scan (k + 1)
                                | r -> go (idx + 1) r

                        scan pos
                    else
                        match matchSegment seg pos with
                        | -1 -> false
                        | r -> go (idx + 1) r

            go 0 0

    /// スカラー関数。未知の関数はエラー。

    let applyFunc (name: string) (args: SqlValue array) =
        let upper = name.ToUpperInvariant()

        let arity (n: int) =
            if args.Length = n then
                ret ()
            else
                Error(DbError.name (sprintf "%s 関数は %d 個の引数が必要ですが、%d 個渡されました" upper n args.Length))

        let asStr (v: SqlValue) =
            match SqlValue.asText v with
            | Some t -> t
            | None -> SqlValue.toDisplayString v

        match upper with
        | "UPPER" ->
            arity 1
            >>= fun () ->
                let t = asStr args.[0]
                ret (SqlValue.Text(t.ToUpperInvariant()))
        | "LOWER" ->
            arity 1
            >>= fun () ->
                let t = asStr args.[0]
                ret (SqlValue.Text(t.ToLowerInvariant()))
        | "LENGTH" ->
            arity 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = asStr args.[0]
                    ret (SqlValue.Int(int64 t.Length))
        | "ABS" ->
            arity 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    match args.[0] with
                    | SqlValue.Int i -> ret (SqlValue.Int(abs i))
                    | SqlValue.Real f -> ret (SqlValue.Real(abs f))
                    | SqlValue.Decimal d -> ret (SqlValue.Decimal(abs d))
                    | other ->
                        Error(DbError.typeError (sprintf "ABS は数値を受け付けますが %s です" (SqlLiteral.toSqlLiteral other)))
        | "COALESCE" ->
            let rec find (i: int) =
                if i >= args.Length then ret SqlValue.Null
                elif SqlValue.isNull args.[i] then find (i + 1)
                else ret args.[i]

            find 0
        | "TRIM" ->
            arity 1
            >>= fun () ->
                let t = (asStr args.[0]).Trim()
                ret (SqlValue.Text t)
        | "ROUND" ->
            arity 1
            >>= fun () ->
                match args.[0] with
                | SqlValue.Int i -> ret (SqlValue.Int i)
                | SqlValue.Real f -> ret (SqlValue.Real(Math.Round(f, MidpointRounding.AwayFromZero)))
                | SqlValue.Null -> ret SqlValue.Null
                | other -> Error(DbError.typeError (sprintf "ROUND は数値を受け付けますが %s です" (SqlLiteral.toSqlLiteral other)))
        | _ -> Error(DbError.name (sprintf "未知のスカラー関数 '%s' です" name))


/// Result のリストを順に評価して全部の値を返す。
module internal Lattice =
    /// `List<Result<'a, DbError>>` を全部評価する。1 つでも Error ならそれを返す。
    let combine (rs: Result<'a, DbError> list) =
        let rec go (rest: Result<'a, DbError> list) (acc: 'a list) =
            match rest with
            | [] -> ret (List.rev acc)
            | r :: tail ->
                match r with
                | Error e -> Error e
                | Ok v -> go tail (v :: acc)

        go rs []

// ---------------------------------------------------------------------------
// 5. 式評価（スカラー・3 値論理）
// ---------------------------------------------------------------------------

/// スカラー式の評価器。
/// 集約を含まない式だけを扱う（集約は `Eval` に組めない）。
/// NULL の伝播と 3 値論理は SQL 標準に合わせる。
module internal Eval =
    /// 算術の型決定。片方が REAL / DECIMAL なら結果もそうなる。
    let private arithmeticType (a: SqlValue) (b: SqlValue) =
        match a, b with
        | (SqlValue.Int _ | SqlValue.Null), (SqlValue.Int _ | SqlValue.Null) -> DataType.Integer
        | (SqlValue.Decimal _ | SqlValue.Null), (SqlValue.Decimal _ | SqlValue.Null) -> DataType.Decimal
        | _, _ -> DataType.Real

    /// 2 つの値を同じ型にそろえてから計算する。
    let private liftNumeric (a: SqlValue) (b: SqlValue) (op: decimal -> decimal -> decimal) =
        match SqlValue.asNumeric a, SqlValue.asNumeric b with
        | Some x, Some y ->
            match arithmeticType a b with
            | DataType.Integer -> SqlValue.Int(op x y |> int64)
            | DataType.Decimal -> SqlValue.Decimal(op x y)
            | _ -> SqlValue.Real(double (op x y))
        | _ -> SqlValue.Null

    let private arith (isDivision: bool) (op: decimal -> decimal -> decimal) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            ret SqlValue.Null
        elif isDivision && SqlValue.asNumeric b = Some 0M then
            Error(DbError.constraintViolation "0 での除算は定義されていません")
        else
            ret (liftNumeric a b op)

    /// 比較を 3 値論理に落とす。
    let private compare (opName: string) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            ret SqlValue.Null
        else
            SqlValue.compareValues a b
            |> Result.map (fun c ->
                let r =
                    match opName with
                    | "eq" -> c = 0
                    | "ne" -> c <> 0
                    | "lt" -> c < 0
                    | "le" -> c <= 0
                    | "gt" -> c > 0
                    | "ge" -> c >= 0
                    | _ -> false

                if r then SqlValue.Bool true else SqlValue.Bool false)

    /// WHERE 用に TruthValue で判定する（UNKNOWN は偽）。
    let private truthOf (opName: string) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            ret TruthValue.Unknown
        else
            SqlValue.compareValues a b
            |> Result.map (fun c ->
                match opName with
                | "eq" -> if c = 0 then TruthValue.True else TruthValue.False
                | "ne" -> if c <> 0 then TruthValue.True else TruthValue.False
                | "lt" -> if c < 0 then TruthValue.True else TruthValue.False
                | "le" -> if c <= 0 then TruthValue.True else TruthValue.False
                | "gt" -> if c > 0 then TruthValue.True else TruthValue.False
                | "ge" -> if c >= 0 then TruthValue.True else TruthValue.False
                | _ -> TruthValue.Unknown)

    /// 2 値（真偽を直接返す）の文脈用。NULL は False。
    let private boolOf (v: SqlValue) =
        match v with
        | SqlValue.Bool b -> b
        | _ -> false

    let private nameOf (op: BinaryOp) =
        match op with
        | BinaryOp.Eq -> "eq"
        | BinaryOp.Ne -> "ne"
        | BinaryOp.Lt -> "lt"
        | BinaryOp.Le -> "le"
        | BinaryOp.Gt -> "gt"
        | BinaryOp.Ge -> "ge"
        | _ -> "eq"

    /// 行の範囲外アクセスを安全なエラーにする。
    let private valueAt (row: SqlValue array) (i: int) (e: Expr) =
        if i < 0 || i >= row.Length then
            Error(DbError.internalError (sprintf "列インデックス %d が範囲外です（%d 列中）: %s" i row.Length (Expr.toDisplayString e)))
        else
            ret row.[i]

    /// 式を評価する。集約呼び出しはここでエラーになる。
    let rec eval (schema: Schema) (row: SqlValue array) (e: Expr) =
        match e with
        | Expr.Literal l -> ret (Scalar.ofLiteral l)
        | Expr.Column c -> Schema.resolve schema c >>= fun i -> valueAt row i e
        | Expr.Name n ->
            let probeRef = { Qualifier = None; Name = n }
            let probeExpr = Expr.Column probeRef

            match Schema.resolve schema probeRef with
            | Ok i -> valueAt row i probeExpr
            | Error _ -> Error(DbError.name (sprintf "'%s' は列でも関数でもありません" n))
        | Expr.Star -> Error(DbError.create ErrorKind.SyntaxError "WHERE 句に '*' は置けません")
        | Expr.QualifiedStar q -> Error(DbError.name (sprintf "'%s.*' はここでは使えません" q))
        | Expr.Unary(op, x) ->
            eval schema row x
            >>= fun v ->
                match op with
                | UnaryOp.Not ->
                    if SqlValue.isNull v then
                        ret SqlValue.Null
                    else
                        ret (if boolOf v then SqlValue.Bool false else SqlValue.Bool true)
                | UnaryOp.Pos -> ret v
                | UnaryOp.Neg ->
                    if SqlValue.isNull v then
                        ret SqlValue.Null
                    else
                        match v with
                        | SqlValue.Int i -> ret (SqlValue.Int -i)
                        | SqlValue.Real f -> ret (SqlValue.Real -f)
                        | SqlValue.Decimal d -> ret (SqlValue.Decimal -d)
                        | _ -> Error(DbError.typeError (sprintf "'-' は数値に適用できません: %s" (SqlLiteral.toSqlLiteral v)))
        | Expr.Binary(BinaryOp.And, a, b) ->
            eval schema row a
            >>= fun l ->
                if l = SqlValue.Bool false then
                    ret (SqlValue.Bool false)
                else
                    eval schema row b
                    >>= fun r ->
                        if SqlValue.isNull l || SqlValue.isNull r then
                            ret SqlValue.Null
                        else
                            let lt = if boolOf l then TruthValue.True else TruthValue.Unknown
                            let rt = if boolOf r then TruthValue.True else TruthValue.Unknown

                            match TruthValue.and2 lt rt with
                            | TruthValue.True -> ret (SqlValue.Bool true)
                            | TruthValue.False -> ret (SqlValue.Bool false)
                            | TruthValue.Unknown -> ret SqlValue.Null
        | Expr.Binary(BinaryOp.Or, a, b) ->
            eval schema row a
            >>= fun l ->
                if l = SqlValue.Bool true then
                    ret (SqlValue.Bool true)
                else
                    eval schema row b
                    >>= fun r ->
                        if SqlValue.isNull l || SqlValue.isNull r then
                            ret SqlValue.Null
                        else
                            let lt = if boolOf l then TruthValue.Unknown else TruthValue.False
                            let rt = if boolOf r then TruthValue.Unknown else TruthValue.False

                            match TruthValue.or2 lt rt with
                            | TruthValue.True -> ret (SqlValue.Bool true)
                            | TruthValue.False -> ret (SqlValue.Bool false)
                            | TruthValue.Unknown -> ret SqlValue.Null
        | Expr.Binary(op, a, b) ->
            eval schema row a
            >>= fun l ->
                eval schema row b
                >>= fun r ->
                    match op with
                    | BinaryOp.Add -> arith false (+) l r
                    | BinaryOp.Sub -> arith false (-) l r
                    | BinaryOp.Mul -> arith false (*) l r
                    | BinaryOp.Div -> arith true (/) l r
                    | BinaryOp.Mod -> arith true (%) l r
                    | BinaryOp.Concat ->
                        if SqlValue.isNull l || SqlValue.isNull r then
                            ret SqlValue.Null
                        else
                            ret (SqlValue.Text(SqlValue.toDisplayString l + SqlValue.toDisplayString r))
                    | BinaryOp.Like ->
                        if SqlValue.isNull l || SqlValue.isNull r then
                            ret SqlValue.Null
                        else
                            let pattern = SqlValue.toDisplayString r
                            let text = SqlValue.toDisplayString l
                            ret (SqlValue.Bool(Scalar.likeMatch pattern text))
                    | BinaryOp.Eq
                    | BinaryOp.Ne
                    | BinaryOp.Lt
                    | BinaryOp.Le
                    | BinaryOp.Gt
                    | BinaryOp.Ge -> compare (nameOf op) l r
                    | BinaryOp.And
                    | BinaryOp.Or -> Error(DbError.internalError "And/Or は先に処理される")
        | Expr.IsNull(x, negated) ->
            eval schema row x
            >>= fun v ->
                let isNull = SqlValue.isNull v
                ret (SqlValue.Bool(if negated then not isNull else isNull))
        | Expr.Between(x, lo, hi, negated) ->
            eval schema row x
            >>= fun v ->
                eval schema row lo
                >>= fun lov ->
                    eval schema row hi
                    >>= fun hiv ->
                        truthOf "ge" v lov
                        >>= fun a ->
                            truthOf "le" v hiv
                            >>= fun b ->
                                match TruthValue.and2 a b with
                                | TruthValue.True -> ret (SqlValue.Bool(not negated))
                                | TruthValue.False -> ret (SqlValue.Bool negated)
                                | TruthValue.Unknown -> ret SqlValue.Null
        | Expr.InList(x, list, negated) ->
            eval schema row x
            >>= fun v ->
                if List.isEmpty list then
                    ret (SqlValue.Bool negated)
                else
                    let rec go (rest: Expr list) (sawUnknown: bool) =
                        match rest with
                        | [] ->
                            if sawUnknown then
                                ret SqlValue.Null
                            else
                                ret (SqlValue.Bool negated)
                        | e :: tail ->
                            match eval schema row e with
                            | Error er -> Error er
                            | Ok w ->
                                match truthOf "eq" v w with
                                | Ok TruthValue.True -> ret (SqlValue.Bool(not negated))
                                | Ok TruthValue.False -> go tail sawUnknown
                                | Ok TruthValue.Unknown -> go tail true
                                | Error er -> Error er

                    go list false
        | Expr.Like(x, p, negated) ->
            eval schema row x
            >>= fun v ->
                eval schema row p
                >>= fun pv ->
                    if SqlValue.isNull v || SqlValue.isNull pv then
                        ret SqlValue.Null
                    else
                        let m = Scalar.likeMatch (SqlValue.toDisplayString pv) (SqlValue.toDisplayString v)
                        ret (SqlValue.Bool(if negated then not m else m))
        | Expr.Case(branches, elseE) ->
            let rec go (rest: (Expr * Expr) list) =
                match rest with
                | [] ->
                    match elseE with
                    | Some e -> eval schema row e
                    | None -> ret SqlValue.Null
                | (w, t) :: tail ->
                    match eval schema row w with
                    | Error er -> Error er
                    | Ok wv ->
                        if SqlValue.isNull wv then go tail
                        elif boolOf wv then eval schema row t
                        else go tail

            go branches
        | Expr.FuncCall(n, args) ->
            evalArgs schema row args
            >>= fun values -> Scalar.applyFunc n (values |> List.toArray)
        | Expr.AggCall(f, _, _) -> Error(DbError.name (sprintf "%s は集約なのでここでは評価できません" f.Name))

    and private evalArgs (schema: Schema) (row: SqlValue array) (args: Expr list) =
        args
        |> List.fold
            (fun acc e ->
                acc
                >>= fun (rev: SqlValue list) -> eval schema row e |> Result.map (fun v -> v :: rev))
            (ret [])
        |> Result.map List.rev

    /// WHERE の判定。UNKNOWN は偽（SQL 標準）。
    let matches (schema: Schema) (row: SqlValue array) (predicate: Expr) =
        eval schema row predicate
        |> Result.map (fun v ->
            match v with
            | SqlValue.Bool b -> b
            | SqlValue.Null -> false
            | SqlValue.Int i -> i <> 0L
            | _ -> false)

/// 式を組み立てる小さなヘルパ。
module internal Build =
    /// 列参照を作る（修飾子はそのまま）。
    let private col (qualifier: string option) (name: string) = { Qualifier = qualifier; Name = name }

    /// 出力列をそのままの列参照にする（`SELECT *` の展開用）。
    let sameRef (c: OutputColumn) = col c.Qualifier c.Name

// ---------------------------------------------------------------------------
// 6. 走査演算子
// ---------------------------------------------------------------------------

/// 実行時文脈。演算子はこれを「作る時点」に確定させる。
type internal ExecContext =
    {
        Store: RowStore
        /// 読み取りの可視性境界。Op 生成時に 1 回だけ決まる。
        Visible: uint64 option
        /// テーブル名（別名含む） -> スキーマ
        Schemas: Map<string, TableSchema>
        /// カタログ。索引の定義を引くのに使う。
        Catalog: Catalog
    }

    static member create (store: RowStore) (visible: uint64 option) =
        {
            Store = store
            Visible = visible
            Schemas = Map.empty
            Catalog = Catalog.empty
        }

    /// スキーマを登録する（別名つきで）。
    member this.WithSchema (alias: string) (schema: TableSchema) =
        { this with
            Schemas = Map.add alias schema this.Schemas
        }

    /// カタログを差し替える（Plan の実行時に索引定義を引くため）。
    member this.WithCatalog(catalog: Catalog) = { this with Catalog = catalog }

    /// テーブルのスキーマを引く。
    /// 別名で登録されていればそれを、無ければ実テーブル名で探す。
    member this.SchemaOf(name: string) =
        match this.Schemas |> Map.tryFind name with
        | Some s -> ret s
        | None ->
            let aliasHit =
                this.Schemas
                |> Map.toList
                |> List.tryPick (fun (alias, _) -> if Catalog.eqName alias name then Some alias else None)

            match aliasHit with
            | Some key -> ret this.Schemas.[key]
            | None ->
                match Catalog.findTable this.Catalog name with
                | Some t -> Catalog.toSchema t
                | None -> Error(DbError.name (sprintf "テーブル '%s' が存在しません" name))

module internal Scan =
    /// テーブルのスキーマを引く。無ければエラー。
    let private schemaOf (ctx: ExecContext) (table: string) =
        match ctx.Schemas |> Map.tryFind table with
        | Some s -> ret s
        | None -> Error(DbError.name (sprintf "テーブル '%s' が存在しません" table))

    /// テーブルを全走査する（主キー昇順 = 挿入順ではないが LSM のキー順）。
    let table (ctx: ExecContext) (table: string) =
        schemaOf ctx table
        >>= fun schema ->
            Dml.scanByPrimaryKey ctx.Store schema ctx.Visible 0
            |> Result.map (fun rows -> Op.ofArray rows)

    /// 索引で一致する全行を引く。
    /// 索引は一意とは限らないので、`WHERE indexed_col = v` が
    /// 1 行になるとは限らない。`v` を持つ行をすべて返す。
    let byIndex (ctx: ExecContext) (table: string) (indexName: string) (keyValues: SqlValue array) =
        schemaOf ctx table
        >>= fun schema ->
            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> Error(DbError.name (sprintf "テーブル %s に索引 %s がありません" table indexName))
            | Some idx ->
                Dml.scanByIndexPrefix ctx.Store idx ctx.Visible keyValues Int32.MaxValue
                |> Result.map Op.ofArray

    /// 主キーで 1 行を引く。
    let byPrimaryKey (ctx: ExecContext) (table: string) (key: SqlValue array) =
        schemaOf ctx table
        >>= fun schema ->
            Dml.getByPrimaryKey ctx.Store schema ctx.Visible key
            |> Result.map (fun row ->
                match row with
                | None -> Op.empty
                | Some r -> Op.ofRow r)

module internal Ops =
    /// 集約呼び出しの「識別子」。同じ呼び出しは同じ accum にまとめる。
    /// `COUNT(*)` の引数は `EStar`。DISTINCT フラグではなく、引数で判定する。
    let private isStarArg (x: Expr) =
        match x with
        | Expr.Star -> true
        | _ -> false

    let private aggKey (f: AggFunc, star: bool, arg: Expr) =
        sprintf "%s(%s)" f.Name (Expr.toDisplayString arg)

    /// 述語で弾く。
    let filter (schema: Schema) (src: Op) (predicate: Expr) =
        Op.filter (fun row -> Eval.matches schema row predicate) src

    /// 射影。`items` が `None` なら全列をそのまま。
    /// 射影。`SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は対象テーブルの全列に展開する。
    let sortBy (schema: Schema) (src: Op) (keys: RykDb.Core.SortKey list) =
        if List.isEmpty keys then
            ret src
        else
            let resolved =
                keys
                |> List.map (fun k ->
                    let sk = k
                    let expr = sk.Expression
                    let dir = sk.Direction

                    match Schema.tryAsColumnRef expr with
                    | None -> Error(DbError.name (sprintf "ORDER BY には列の直接参照だけを書けます（%s）" (Expr.toDisplayString expr)))
                    | Some c ->
                        let bare: ColumnRef = { Qualifier = None; Name = c.Name }

                        match Schema.resolve schema c with
                        | Ok i -> Ok(i, dir)
                        | Error _ -> Schema.resolve schema bare |> Result.map (fun i -> i, dir))
                |> Lattice.combine

            resolved
            |> Result.bind (fun plan ->
                let compareWith (a: SqlValue array) (b: SqlValue array) =
                    let rec go (rest: (int * SortDirection) list) =
                        match rest with
                        | [] -> 0
                        | (i, dir) :: tail ->
                            match SqlValue.compareValues a.[i] b.[i] with
                            | Error _ -> go tail
                            | Ok c ->
                                if c = 0 then
                                    go tail
                                else
                                    match dir with
                                    | SortDirection.Ascending -> c
                                    | SortDirection.Descending -> -c

                    go plan

                Op.toArray src
                |> Result.map (fun rows ->
                    let sorted = Array.sortWith compareWith rows
                    Op.ofArray sorted))

    /// 重複除去。保持する順序は最初に現れた順（SQL の DISTINCT と同じ）。
    /// 既出のキーを全部覚える必要がある。
    /// 「直前のキーだけ」で比較するのは `DISTINCT` ではなく `DISTINCT ON` にしかならない
    /// （`2, 1, 2` で 2 が 2 回目に出るため、直前比較では除去できない）。
    ///  Costs O(既出キー数 × 平均キー長)。メモリは全キーに比例する。
    /// 値が多いテーブルでは使えないが、
    /// `SELECT DISTINCT` の素直な実装としてはこれが正しい。
    let distinct (src: Op) =
        let rec go (seen: Set<string>) (cur: Op) =
            Op.Compute(fun () ->
                Op.next cur
                >>= function
                    | None -> ret None
                    | Some(row, nxt) ->
                        let k: string = Row.keyOf row

                        if Set.contains k seen then
                            Op.next (go seen nxt)
                        else
                            let nextOp: Op = go (Set.add k seen) nxt
                            ret (Some(row, nextOp)))

        go Set.empty src

    /// 集約の 1 項目分の_accumulator。
    /// SUM / AVG は decimal で累積し、最後に型を決める（丸め誤差を避ける）。
    type private Acc =
        {
            Kind: AggFunc
            Count: int64
            SumDecimal: decimal
            SawReal: bool
            SumReal: double
            Min: SqlValue option
            Max: SqlValue option
            First: SqlValue option
        }

    let private emptyAcc (kind: AggFunc) =
        {
            Kind = kind
            Count = 0L
            SumDecimal = 0M
            SawReal = false
            SumReal = 0.0
            Min = None
            Max = None
            First = None
        }

    /// 値 1 つを蓄積に追加する。`isStar` のときは NULL でも数える。
    /// 1 行を蓄積に加える。
    /// `isStar` が true（`COUNT(*)`）なら NULL でも数える。
    /// それ以外は NULL を無視する（`COUNT(列)` の SQL の挙動）。
    let private addAcc (acc: Acc) (value: SqlValue option) (isStar: bool) =
        let counted =
            if isStar then
                Some SqlValue.Null
            else
                match value with
                | Some v when not (SqlValue.isNull v) -> Some v
                | _ -> None

        match counted with
        | None -> ret acc
        | Some v ->
            match acc.Kind with
            | AggFunc.Count -> ret { acc with Count = acc.Count + 1L }
            | AggFunc.Sum
            | AggFunc.Avg ->
                match SqlValue.asNumeric v with
                | None ->
                    Error(
                        DbError.typeError (sprintf "%s は数値だけを受け付けますが %s です" acc.Kind.Name (SqlLiteral.toSqlLiteral v))
                    )
                | Some d ->
                    match v with
                    | SqlValue.Real f ->
                        ret
                            { acc with
                                Count = acc.Count + 1L
                                SawReal = true
                                SumReal = acc.SumReal + f
                            }
                    | _ ->
                        ret
                            { acc with
                                Count = acc.Count + 1L
                                SumDecimal = acc.SumDecimal + d
                            }
            | AggFunc.Min ->
                match acc.Min with
                | None ->
                    ret
                        { acc with
                            Count = acc.Count + 1L
                            Min = Some v
                        }
                | Some cur ->
                    SqlValue.compareValues v cur
                    |> Result.map (fun c ->
                        if c < 0 then
                            { acc with
                                Count = acc.Count + 1L
                                Min = Some v
                            }
                        else
                            { acc with Count = acc.Count + 1L })
            | AggFunc.Max ->
                match acc.Max with
                | None ->
                    ret
                        { acc with
                            Count = acc.Count + 1L
                            Max = Some v
                        }
                | Some cur ->
                    SqlValue.compareValues v cur
                    |> Result.map (fun c ->
                        if c > 0 then
                            { acc with
                                Count = acc.Count + 1L
                                Max = Some v
                            }
                        else
                            { acc with Count = acc.Count + 1L })

    /// 蓄積を結果値に変える。
    let private finishAcc (acc: Acc) =
        match acc.Kind with
        | AggFunc.Count -> SqlValue.Int acc.Count
        | AggFunc.Sum ->
            if acc.Count = 0L then SqlValue.Null
            elif acc.SawReal then SqlValue.vReal acc.SumReal
            else SqlValue.Decimal acc.SumDecimal
        | AggFunc.Avg ->
            if acc.Count = 0L then
                SqlValue.Null
            elif acc.SawReal then
                SqlValue.vReal (acc.SumReal / float acc.Count)
            else
                SqlValue.vReal (float acc.SumDecimal / float acc.Count)
        | AggFunc.Min -> defaultArg acc.Min SqlValue.Null
        | AggFunc.Max -> defaultArg acc.Max SqlValue.Null

    let rec private collectAggs (e: Expr) (acc: (AggFunc * bool * Expr) list) =
        match e with
        | Expr.AggCall(f, _, x) -> (f, isStarArg x, x) :: acc
        | Expr.Literal _
        | Expr.Column _
        | Expr.Name _
        | Expr.Star
        | Expr.QualifiedStar _ -> acc
        | Expr.Unary(_, x) -> collectAggs x acc
        | Expr.Binary(_, a, b) -> collectAggs b (collectAggs a acc)
        | Expr.IsNull(x, _) -> collectAggs x acc
        | Expr.Between(x, a, b, _) -> collectAggs b (collectAggs a (collectAggs x acc))
        | Expr.InList(x, xs, _) ->
            let after = xs |> List.fold (fun acc' e' -> collectAggs e' acc') (collectAggs x acc)
            after
        | Expr.Like(x, p, _) -> collectAggs p (collectAggs x acc)
        | Expr.Case(branches, elseE) ->
            let branchAcc =
                branches |> List.fold (fun a (w, t) -> collectAggs t (collectAggs w a)) acc

            match elseE with
            | Some el -> collectAggs el branchAcc
            | None -> branchAcc
        | Expr.FuncCall(_, args) -> args |> List.fold (fun a e' -> collectAggs e' a) acc

    /// 式中に現れる集約呼び出しの一覧（実行順に揃える）。
    let aggCalls (exprs: Expr list) =
        exprs |> List.fold (fun acc e -> collectAggs e acc) [] |> List.rev

    /// 式から集約呼び出しを「出現順」で集める（重複は同じ accum にまとめる）。
    /// 集約呼び出しを、計算済みの結果へ読み替える。
    /// 集約後の行は「groupBy の値 ++ 各集約の結果」の並び。
    /// 複数の SELECT 項目から集約呼び出しを集める（出現順）。
    /// `project` は `aggCalls` より前に定義されるので、ここに保存する。
    let private aggCallsOf (items: SelectItem list) =
        aggCalls (items |> List.map (fun i -> i.Expression))

    /// 式を「集約後の行の位置」で読み替える。
    /// `offsets` は「表示文字列 -> 列位置」。集約呼び出しの他、
    /// GROUP BY の列（`SELECT age, COUNT(*)` の `age`）もここに含まれる。
    let rec private rewriteAggs (e: Expr) (offsets: Map<string, int>) (row: SqlValue array) =
        let slot key =
            match offsets |> Map.tryFind key with
            | Some i when i >= 0 && i < row.Length -> Some(Expr.Literal(Literal.ofValue row.[i]))
            | _ -> None

        match e with
        | Expr.AggCall(f, star, arg) ->
            match slot (aggKey (f, star, arg)) with
            | Some l -> l
            | None -> Expr.Literal Literal.Null
        | Expr.Literal l -> Expr.Literal l
        | Expr.Column _ as orig ->
            match slot (Expr.toDisplayString orig) with
            | Some l -> l
            | None -> orig
        | Expr.Name n as orig ->
            match slot n with
            | Some l -> l
            | None -> orig
        | Expr.Star -> Expr.Star
        | Expr.QualifiedStar q -> Expr.QualifiedStar q
        | Expr.Unary(op, x) -> Expr.Unary(op, rewriteAggs x offsets row)
        | Expr.Binary(op, a, b) -> Expr.Binary(op, rewriteAggs a offsets row, rewriteAggs b offsets row)
        | Expr.IsNull(x, neg) -> Expr.IsNull(rewriteAggs x offsets row, neg)
        | Expr.Between(x, a, b, neg) ->
            Expr.Between(rewriteAggs x offsets row, rewriteAggs a offsets row, rewriteAggs b offsets row, neg)
        | Expr.InList(x, xs, neg) ->
            Expr.InList(rewriteAggs x offsets row, xs |> List.map (fun e2 -> rewriteAggs e2 offsets row), neg)
        | Expr.Like(x, p, neg) -> Expr.Like(rewriteAggs x offsets row, rewriteAggs p offsets row, neg)
        | Expr.Case(branches, elseE) ->
            let bs =
                branches
                |> List.map (fun (w, t) -> (rewriteAggs w offsets row, rewriteAggs t offsets row))

            let el =
                match elseE with
                | Some e2 -> Some(rewriteAggs e2 offsets row)
                | None -> None

            Expr.Case(bs, el)
        | Expr.FuncCall(n, args) -> Expr.FuncCall(n, args |> List.map (fun e2 -> rewriteAggs e2 offsets row))

    /// 集約結果の「列位置」を索引割りで持つ。
    /// GROUP BY の列（位置 0, 1, ...）と各集約の結果（その後）の両方を持つ。
    let private aggOffsets (groupBy: Expr list) (aggs: (AggFunc * bool * Expr) list) =
        let groupSlots =
            groupBy |> List.mapi (fun i e -> Expr.toDisplayString e, i) |> Map.ofList

        let aggSlots =
            aggs |> List.mapi (fun i a -> aggKey a, List.length groupBy + i) |> Map.ofList

        Map.fold (fun acc k v -> Map.add k v acc) groupSlots aggSlots

    /// 集約呼び出しの式を、計算済みの値（定数）へ置き換える。
    /// HAVING と SELECT 項目の両方で使う。
    let private substituteAggs
        (groupBy: Expr list)
        (aggs: (AggFunc * bool * Expr) list)
        (e: Expr)
        (row: SqlValue array)
        =
        rewriteAggs e (aggOffsets groupBy aggs) row

    /// HAVING を張る。集約後の行は位置のみの意味を持つので `Schema.empty` で評価する。
    let havingFilter (groupBy: Expr list) (aggs: (AggFunc * bool * Expr) list) (predicate: Expr) =
        fun row ->
            let replaced = substituteAggs groupBy aggs predicate row
            Eval.matches Schema.empty row replaced

    /// GROUP BY + 集約。
    /// `groupBy` が空でも集約があれば 1 グループだけ出す。
    /// （`SELECT COUNT(*) FROM 空のテーブル` が 0 ではなく 1 行を返さないと
    ///   SQL の通常の挙動と違ってしまうため）
    let aggregate (schema: Schema) (src: Op) (groupBy: Expr list) (aggs: (AggFunc * bool * Expr) list) =
        Op.toArray src
        |> Result.bind (fun (rows: SqlValue array array) ->
            let keyOfRow (row: SqlValue array) : Result<string, DbError> =
                groupBy
                |> List.map (fun e -> Eval.eval schema row e)
                |> Lattice.combine
                |> Result.map (fun (vs: SqlValue list) ->
                    vs |> List.map SqlLiteral.toSqlLiteral |> String.concat "\u0001")

            let order: ResizeArray<string> = ResizeArray<string>()

            let table: System.Collections.Generic.Dictionary<string, ResizeArray<SqlValue array>> =
                System.Collections.Generic.Dictionary<string, ResizeArray<SqlValue array>>()

            let rec go (i: int) =
                if i >= rows.Length then
                    let built: Map<string, SqlValue array list> =
                        table |> Seq.map (fun kv -> kv.Key, List.ofSeq kv.Value) |> Map.ofSeq

                    Ok(List.ofSeq order, built)
                else
                    match keyOfRow rows.[i] with
                    | Error e -> Error e
                    | Ok k ->
                        match table.TryGetValue k with
                        | true, bucket -> bucket.Add rows.[i]
                        | _ ->
                            let bucket = ResizeArray<SqlValue array>()
                            bucket.Add rows.[i]
                            table.[k] <- bucket
                            order.Add k

                        go (i + 1)

            go 0
            |> Result.map (fun (order: string list, table: Map<string, SqlValue array list>) ->
                let rowOf (k: string) =
                    match table |> Map.tryFind k with
                    | None -> None
                    | Some(members: SqlValue array list) ->
                        if List.isEmpty members then
                            None
                        else
                            let first = List.head members

                            let gv =
                                match groupBy |> List.map (fun e -> Eval.eval schema first e) |> Lattice.combine with
                                | Ok vs -> List.toArray vs
                                | Error _ -> [||]

                            let oneAgg (spec: AggFunc * bool * Expr) =
                                let f, star, arg = spec
                                let start = emptyAcc f

                                let add (acc: Acc) (row: SqlValue array) =
                                    if star then
                                        addAcc acc None true
                                    else
                                        Eval.eval schema row arg |> Result.bind (fun v -> addAcc acc (Some v) false)

                                match List.fold (fun acc row -> acc >>= fun a -> add a row) (ret start) members with
                                | Ok acc -> finishAcc acc
                                | Error _ -> SqlValue.Null

                            let av = aggs |> List.map oneAgg |> List.toArray
                            Some(Array.concat [| gv; av |])

                let rec chain (ks: string list) =
                    match ks with
                    | [] -> Op.Done
                    | k :: tail ->
                        Op.Compute(fun () ->
                            match rowOf k with
                            | None -> Op.next (chain tail)
                            | Some r -> ret (Some(r, chain tail)))

                chain order))

    /// 結合の述語を「行の組」に対して評価する。
    /// ON 句は両テーブルの列を同時に参照するので、左右を連結した行で評価する。
    let private joinPred (leftSchema: Schema) (rightSchema: Schema) (on: Expr) (l: SqlValue array) (r: SqlValue array) =
        Eval.matches (leftSchema.Concat rightSchema) (Array.append l r) on

    /// INNER JOIN（nested loop）。右側は内側ループごとに先頭から引き直す。
    let innerJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        Op.toArray right
        |> Result.map (fun (rightRows: SqlValue array array) ->
            let rec inner (rs: SqlValue array array) (lrow: SqlValue array) (lnext: Op) : Op =
                if Array.isEmpty rs then
                    go lnext
                else
                    let rrow = rs.[0]
                    let rest = rs |> Array.skip 1

                    match joinPred leftSchema rightSchema on lrow rrow with
                    | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, inner rest lrow lnext)))
                    | Ok false -> Op.Compute(fun () -> Op.next (inner rest lrow lnext))
                    | Error _ -> Op.empty

            and go (cur: Op) : Op =
                Op.Compute(fun () ->
                    Op.next cur
                    >>= function
                        | None -> ret None
                        | Some(lrow, lnext) -> Op.next (inner rightRows lrow lnext))

            go left)

    /// LEFT JOIN。左の行に右が 1 行も無ければ NULL で埋める。
    let leftJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        Op.toArray right
        |> Result.map (fun (rightRows: SqlValue array array) ->
            let width = rightSchema.Columns.Length

            let rec inner (rs: SqlValue array array) (lrow: SqlValue array) (lnext: Op) : Op =
                if Array.isEmpty rs then
                    let nulls = Array.create width SqlValue.Null
                    Op.Step(Array.append lrow nulls, go lnext)
                else
                    let rrow = rs.[0]
                    let rest = rs |> Array.skip 1

                    match joinPred leftSchema rightSchema on lrow rrow with
                    | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, innerAfter rest lrow lnext)))
                    | Ok false -> Op.Compute(fun () -> Op.next (inner rest lrow lnext))
                    | Error _ -> Op.empty

            and innerAfter (rs: SqlValue array array) (lrow: SqlValue array) (lnext: Op) : Op =
                if Array.isEmpty rs then
                    go lnext
                else
                    let rrow = rs.[0]
                    let rest = rs |> Array.skip 1

                    match joinPred leftSchema rightSchema on lrow rrow with
                    // `Step` は遅延されないので（§2.1）`Compute` で 1 行ずつ作る。
                    | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, innerAfter rest lrow lnext)))
                    | Ok false -> Op.Compute(fun () -> Op.next (innerAfter rest lrow lnext))
                    | Error _ -> go lnext

            and go (cur: Op) : Op =
                Op.Compute(fun () ->
                    Op.next cur
                    >>= function
                        | None -> ret None
                        | Some(lrow, lnext) -> Op.next (inner rightRows lrow lnext))

            go left)

    /// 射影。`SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は対象テーブルの全列に展開する。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    let project (schema: Schema) (groupBy: Expr list) (src: Op) (items: SelectItem list) =
        if List.isEmpty items then
            src
        else
            let plan: (SelectItem * Expr list) list =
                items
                |> List.map (fun (item: SelectItem) ->
                    let exprs: Expr list =
                        match item.Expression with
                        | Expr.Star ->
                            schema.Columns
                            |> Array.map (fun (c: OutputColumn) -> Expr.Column(Build.sameRef c))
                            |> Array.toList
                        | Expr.QualifiedStar q ->
                            schema.Columns
                            |> Array.filter (fun (c: OutputColumn) -> defaultArg c.Qualifier "" = q)
                            |> Array.map (fun (c: OutputColumn) -> Expr.Column(Build.sameRef c))
                            |> Array.toList
                        | other -> [ other ]

                    item, exprs)

            let allExprs = plan |> List.collect (fun (_, exprs) -> exprs)
            let foundAggs = aggCallsOf items

            if List.isEmpty foundAggs then
                Op.map
                    (fun row ->
                        allExprs
                        |> List.map (fun e -> Eval.eval schema row e)
                        |> Lattice.combine
                        |> Result.map List.toArray)
                    src
            else
                Op.map
                    (fun row ->
                        allExprs
                        |> List.map (fun e -> Eval.eval Schema.empty row (substituteAggs groupBy foundAggs e row))
                        |> Lattice.combine
                        |> Result.map List.toArray)
                    src

// ---------------------------------------------------------------------------
// 7. 更新演算子（INSERT / UPDATE / DELETE）
// ---------------------------------------------------------------------------

// 書き込みは `Transaction` にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
module internal DmlOps =
    /// 定数式だけなら行の外で評価できる（列参照が無い）。
    let private evalConstant (e: Expr) = Eval.eval Schema.empty [||] e

    /// INSERT。`columns` が None ならテーブル定義の順に並べる。
    /// DEFAULT / NULL の充足と型強制はここで行う。
    /// `TableSchema.validateRow` が NOT NULL と型を最終確認する。
    let insert
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (columns: string list option)
        (rows: Expr list list)
        =
        match columns with
        | None -> schema.Columns |> List.mapi (fun i c -> i) |> ret
        | Some names -> names |> List.map (fun n -> schema.RequireIndexOf n) |> Lattice.combine
        |> Result.bind (fun idxs ->
            let width = schema.Columns.Length

            let buildRow (values: Expr list) =
                if List.length values <> List.length idxs then
                    Error(
                        DbError.create
                            ErrorKind.SyntaxError
                            (sprintf "INSERT の列数(%d)と値の数(%d)が合いません" (List.length idxs) (List.length values))
                    )
                else
                    let rec gather (values: Expr list) (idxs: int list) (acc: (int * SqlValue) list) =
                        match values, idxs with
                        | [], [] -> ret acc
                        | v :: vs, i :: is_ ->
                            evalConstant v
                            >>= fun value ->
                                let colDef = schema.Columns.[i]

                                SqlValue.coerce colDef.DataType value
                                >>= fun coerced -> gather vs is_ ((i, coerced) :: acc)
                        | _ -> ret acc

                    gather values idxs []
                    |> Result.map (fun (filled: (int * SqlValue) list) ->
                        Array.init width (fun i ->
                            match filled |> List.tryFind (fun (j, _) -> j = i) with
                            | Some(_, v) -> v
                            | None ->
                                match schema.Columns.[i].Constraint.Default with
                                | Some d ->
                                    match evalConstant d with
                                    | Ok dv -> dv
                                    | Error _ -> SqlValue.Null
                                | None -> SqlValue.Null))

            rows
            |> List.fold
                (fun acc values ->
                    acc
                    >>= fun (txnAcc, count) ->
                        buildRow values
                        >>= fun row -> Dml.insert schema txnAcc row |> Result.map (fun t -> t, count + 1))
                (ret (txn, 0)))

    /// UPDATE。`assignments` の右辺は現在行で評価する。
    /// 検索キーは更新前の行の主キー。主キー列を書き換えると
    /// 古い位置の行が残って重複する
    /// （sql-reference.md「主キー列は更新できない」）。
    let update
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (assignments: (string * Expr) list)
        (where: Expr option)
        =
        let targetSchema = Schema.ofTable schema.Name None schema

        assignments
        |> List.map (fun (name, _) -> schema.RequireIndexOf name)
        |> Lattice.combine
        |> Result.bind (fun targets ->
            Dml.scanByPrimaryKey ctx.Store schema ctx.Visible 0
            |> Result.bind (fun current ->
                let apply (txnAcc: Transaction) (count: int) (row: SqlValue array) =
                    let rec gather (values: (string * Expr) list) (targets: int list) (acc: (int * SqlValue) list) =
                        match values, targets with
                        | [], [] -> ret acc
                        | (_, expr) :: vs, i :: is_ ->
                            Eval.eval targetSchema row expr
                            >>= fun v ->
                                let colDef = schema.Columns.[i]

                                SqlValue.coerce colDef.DataType v
                                >>= fun coerced -> gather vs is_ ((i, coerced) :: acc)
                        | _ -> ret acc

                    let assign (values: (string * Expr) list) (targets: int list) =
                        gather values targets []
                        |> Result.map (fun filled ->
                            Array.mapi
                                (fun i v ->
                                    match filled |> List.tryFind (fun (j, _) -> j = i) with
                                    | Some(_, nv) -> nv
                                    | None -> v)
                                row)

                    let matched =
                        match where with
                        | Some w -> Eval.matches targetSchema row w >>= fun keep -> ret keep
                        | None -> ret true

                    matched
                    >>= fun keep ->
                        if not keep then
                            ret (txnAcc, count)
                        else
                            assign assignments targets
                            >>= fun newRow ->
                                let pk = TableSchema.primaryKeyOf schema row

                                Dml.updateByPrimaryKey ctx.Store schema ctx.Visible txnAcc pk newRow
                                |> Result.map (fun t -> t, count + 1)

                let initial = txn, 0

                current
                |> Array.fold (fun acc row -> acc >>= fun (t, c) -> apply t c row) (ret initial)))

    /// DELETE。WHERE に合う行を主キー + 全索引に tombstone で消す。
    let delete (ctx: ExecContext) (schema: TableSchema) (txn: Transaction) (where: Expr option) =
        let targetSchema = Schema.ofTable schema.Name None schema

        Dml.scanByPrimaryKey ctx.Store schema ctx.Visible 0
        |> Result.bind (fun rows ->
            let initial = txn, 0

            rows
            |> Array.fold
                (fun acc row ->
                    acc
                    >>= fun (t: Transaction, c: int) ->
                        match where with
                        | Some w ->
                            Eval.matches targetSchema row w
                            >>= fun keep ->
                                if not keep then
                                    ret (t, c)
                                else
                                    let pk = TableSchema.primaryKeyOf schema row

                                    Dml.deleteByPrimaryKey ctx.Store schema ctx.Visible t pk
                                    |> Result.map (fun (t2: Transaction) -> t2, c + 1)
                        | None ->
                            let pk = TableSchema.primaryKeyOf schema row

                            Dml.deleteByPrimaryKey ctx.Store schema ctx.Visible t pk
                            |> Result.map (fun (t2: Transaction) -> t2, c + 1))
                (ret initial))
