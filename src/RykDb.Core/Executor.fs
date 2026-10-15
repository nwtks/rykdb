// ===========================================================================
// Executor — Volcano（イテレータ）方式の実行層。
// pull 演算子 Op（Done / Step / Compute）で組む。LIMIT / WHERE の
// 早期打ち切りが効き、無限長でもメモリは 1行分。
// ===========================================================================

namespace RykDb.Core

// ---------------------------------------------------------------------------
// 1. 行
// ---------------------------------------------------------------------------

/// 1列の SqlValue 配列。メモリ上は配列のまま、回さない。
type internal Row = SqlValue array

module internal Row =
    /// 全 NULL のときの等価キー用の印。文字列に現れない制御文字を使う。
    [<Literal>]
    let private nullTag = "\u0000\0NULL"

    /// 等価キー（DISTINCT / GROUP BY / ハッシュ結合のキーに使う）。
    /// NULL は SQL では「他のどの値とも等しくない」ので、ここでは NULL も NULL 同士で等しい として扱う
    /// （GROUP BY で NULL が 1つのグループになる挙動に合わせるため）。
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

/// 出力の 1列。
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

module internal OutputColumn =
    /// テーブルの 1列から作る（修飾名はテーブル名か別名）。
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

    /// 列に別名を付ける（表示名のみ変更）。
    member this.WithAlias (index: int) (label: string) =
        if index < 0 || index >= this.Columns.Length then
            this
        else
            let cols =
                this.Columns |> Array.mapi (fun i c -> if i = index then c.As label else c)

            { this with Columns = cols }

    /// 列を 1つだけ残す。
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
    let empty = { Columns = [||]; Tables = [] }

    /// スキーマ列を左から連結する（FROM / JOIN の出力スキーマ組み立て）。
    let concatAll (schemas: Schema list) =
        schemas |> List.fold (fun (acc: Schema) (s: Schema) -> acc.Concat s) empty

    /// `*` / `t.*` が表す出力列を展開する。どちらもなければ None。
    /// <remarks>
    /// ヘッダ行（projectHeader）と射影の式（Ops.project）が同じ列を共有する。
    /// どちらかが先に変わると `SELECT *` の列数とヘッダがずれる（実際にずれた）。
    /// Expr は `[<NoComparison>]` なので「どちらの star だったか」は
    /// 別のフラグで返す（qualified = `t.*` なら true）。
    /// </remarks>
    let expandStar (schema: Schema) (e: Expr) =
        match e with
        | Expr.Star -> Some(false, schema.Columns |> Array.toList)
        | Expr.QualifiedStar q ->
            Some(
                true,
                schema.Columns
                |> Array.filter (fun (c: OutputColumn) -> defaultArg c.Qualifier "" = q)
                |> Array.toList
            )
        | _ -> None

    /// 射影後のヘッダ行を作る。
    /// `SELECT *` は入力の列名をそのまま（修飾つき）。
    /// `SELECT id` は式を表示する。別名があればそれを使う。
    /// 入力スキーマのヘッダを返すと `SELECT id` が `people.id` になるので、
    /// 射影後の名前を組み立て直す必要がある。
    /// <remarks>
    /// `*` は修飾なしの `c.Name`、`t.*` は修飾つきの `c.Display` で出す（元の挙動）。
    /// </remarks>
    let projectHeader (schema: Schema) (items: SelectItem list) =
        match items with
        | [] -> schema.Header
        | _ ->
            items
            |> List.collect (fun item ->
                match expandStar schema item.Expression with
                | Some(qualified, columns) ->
                    if qualified then
                        columns |> List.map (fun c -> c.Display)
                    else
                        columns |> List.map (fun c -> c.Name)
                | None ->
                    match item.Alias with
                    | Some a -> [ a ]
                    | None -> [ Expr.toDisplayString item.Expression ])
            |> List.toArray

    /// 式を「列の直接参照」に読み替える。
    /// Column はそのまま。修飾子なしの裸の名前（Name）も列として扱う
    /// （パーサが WHERE / ORDER BY の裸の名前を Name にするため）。
    /// 集約呼び出しや式は列ではないので None。
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

    /// テーブルの全列を並べる。
    let ofTable (table: string) (alias: string option) (schema: TableSchema) =
        let name = defaultArg alias table

        let cols =
            schema.Columns
            |> List.map (fun c -> OutputColumn.ofTableColumn name c)
            |> List.toArray

        { Columns = cols; Tables = [ name ] }

// ---------------------------------------------------------------------------
// 3. Op — 不変の pull 演算子
// ---------------------------------------------------------------------------

/// 1行取り出す演算子。
/// Done             = 終わった
/// Step (row, next) = row を返した。続きは next
/// Compute f        = f を呼んで 1行取り出す
///
/// Compute が「ソースだけを包む 1段」であることが要点。
/// 変換・フィルタ・結合はすべて「上流の Op を受け取って、`unit -> ...` を返す関数」
/// として書けるので、ソース側の状態を壊さずに不変のまま合成できる。
/// Step の next を呼ぶまで次の行は計算されないので、無限長でもメモリは 1行分だけ。
[<RequireQualifiedAccess>]
type internal Op =
    | Done
    /// 1行と「その次」。next は評価されるので長い連鎖を組むには向かない
    /// （遅延が必要なときは Compute を使う）。
    | Step of SqlValue array * Op
    /// 「次に 1行を返す関数」。引数は unit なので遅延評価される。
    /// ソース側が「まだ計算していない」長い系列を表すときは必ずこれを使う。
    | Compute of (unit -> Result<(SqlValue array * Op) option, DbError>)

module internal Op =
    /// 空の演算子。
    let empty = Op.Done

    /// 1行だけの演算子。
    let ofRow (row: SqlValue array) = Op.Step(row, Op.Done)

    /// 配列を順に返す演算子。次の行は呼び出すたびに計算する。
    let ofArray (rows: SqlValue array array) =
        let rec rowsFrom (i: int) =
            if i >= rows.Length then
                Op.Done
            else
                Op.Compute(fun () -> ret (Some(rows.[i], rowsFrom (i + 1))))

        rowsFrom 0

    /// 1行取り出す。
    let next (op: Op) =
        match op with
        | Op.Done -> ret None
        | Op.Step(row, nxt) -> ret (Some(row, nxt))
        | Op.Compute f -> f ()

    /// ソースの次の 1行を引き出して継続に渡す。
    /// 継続は「行と残りの Op」を受け取り、返した Result をそのまま返す。
    /// 返した「その行の次」の Op が演算子の残りになる。
    let pull (src: Op) (cont: SqlValue array -> Op -> Result<(SqlValue array * Op) option, DbError>) =
        Op.Compute(fun () ->
            next src
            >>= function
                | None -> ret None
                | Some(row, nxt) -> cont row nxt)

    /// 行を 1つ変換する。失敗したら打切って Error。
    /// 変換後の行の「次」は map を通した残りにする
    /// （nxt をそのまま渡すと変換が 1行だけに効き、次から元に戻る）。
    let rec map (f: SqlValue array -> Result<SqlValue array, DbError>) (src: Op) =
        pull src (fun row nxt -> f row |> Result.map (fun r -> Some(r, map f nxt)))

    /// 述語で弾く。偽の行は読み飛ばして次へ進む。
    /// p row >>= function で分岐する
    /// （if/then/else だと Result の短絡で Some(row, nxt) だけが残ることがある）。
    let filter (p: SqlValue array -> Result<bool, DbError>) (src: Op) =
        let rec seekMatch (cur: Op) =
            pull cur (fun row nxt ->
                let verdict = p row

                match verdict with
                | Ok true -> ret (Some(row, seekMatch nxt))
                | Ok false -> next (seekMatch nxt)
                | Error e -> err e)

        seekMatch src

    /// 先頭 n 行だけ。n <= 0 は「全部」。
    let rec take (n: int) (src: Op) =
        if n <= 0 then
            Op.Done
        else
            pull src (fun row nxt -> ret (Some(row, take (n - 1) nxt)))

    /// 先頭 n 行を読み飛ばす。
    let skip (n: int) (src: Op) =
        let rec drop (left: int) (cur: Op) =
            if left <= 0 then
                cur
            else
                pull cur (fun _ nxt -> next (drop (left - 1) nxt))

        drop n src

    /// 全行を配列に materialize する。
    let toArray (op: Op) =
        let rows = ResizeArray<SqlValue array>()

        let rec drain (cur: Op) =
            match next cur with
            | Error e -> Error e
            | Ok None -> Ok(rows.ToArray())
            | Ok(Some(row, nxt)) ->
                rows.Add row
                drain nxt

        drain op

// ---------------------------------------------------------------------------
// 4. リテラルとスカラー関数
// ---------------------------------------------------------------------------

module internal Scalar =
    /// リテラルを実行時値へ（変換規則は Literal.ToSqlValue に一本化）。
    let ofLiteral (l: Literal) = l.ToSqlValue

    /// LIKE のパターン照合。`%` は任意の文字列、`_` は 1 文字。
    /// 正規表現に落とさずに手で照合する（LIKE の方言差を吸収するため）。
    /// バックトラックしない。
    /// `%` で後戻りする実装は、進捗しないステップがあると無限ループしうる。
    /// ここでは「区間と隙間を左から 1通りだけ試す」素直な走査にする。
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

            let rec matchFrom (idx: int) (pos: int) =
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
                                -1
                            else
                                match matchSegment seg k with
                                | -1 -> scan (k + 1)
                                | r -> r

                        match scan pos with
                        | -1 -> false
                        | next -> matchFrom (idx + 1) next
                    else
                        match matchSegment seg pos with
                        | -1 -> false
                        | r -> matchFrom (idx + 1) r

            matchFrom 0 0

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
                | SqlValue.Real f -> ret (SqlValue.Real(System.Math.Round(f, System.MidpointRounding.AwayFromZero)))
                | SqlValue.Null -> ret SqlValue.Null
                | other -> Error(DbError.typeError (sprintf "ROUND は数値を受け付けますが %s です" (SqlLiteral.toSqlLiteral other)))
        | _ -> Error(DbError.name (sprintf "未知のスカラー関数 '%s' です" name))


// ---------------------------------------------------------------------------
// 5. 式評価（スカラー・3値論理）
// ---------------------------------------------------------------------------

/// スカラー式の評価器。
/// 集約を含まない式だけを扱う（集約は Eval に組めない）。
/// NULL の伝播と 3値論理は SQL 標準に合わせる。
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

    /// 比較演算子を適用して 3値論理へ落とす。
    /// <remarks>
    /// compare（SqlValue を返す文脈）と truthOf（TruthValue を返す文脈）は
    /// NULL 判定と演算子の表だけが違うので、両方ここへ寄せる。
    /// 未知の演算子名は Unknown = False 側へ落とす（SQL では到達しない）。
    /// </remarks>
    let private applyComparison (opName: string) (c: int) =
        let tf b =
            if b then TruthValue.True else TruthValue.False

        match opName with
        | "eq" -> tf (c = 0)
        | "ne" -> tf (c <> 0)
        | "lt" -> tf (c < 0)
        | "le" -> tf (c <= 0)
        | "gt" -> tf (c > 0)
        | "ge" -> tf (c >= 0)
        | _ -> TruthValue.Unknown

    /// 比較を 3値論理に落とす。
    let private compare (opName: string) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            ret SqlValue.Null
        else
            SqlValue.compareValues a b
            |> Result.map (fun c ->
                match applyComparison opName c with
                | TruthValue.True -> SqlValue.Bool true
                | _ -> SqlValue.Bool false)

    /// WHERE 用に TruthValue で判定する（UNKNOWN は偽）。
    let private truthOf (opName: string) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            ret TruthValue.Unknown
        else
            SqlValue.compareValues a b |> Result.map (fun c -> applyComparison opName c)

    /// 2値（真偽を直接返す）の文脈用。NULL は False。
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

    /// SqlValue を 3値へ落とす。BOOL はそのまま、NULL と BOOL 以外は UNKNOWN。
    let private truthOfValue (v: SqlValue) =
        if SqlValue.isNull v then
            TruthValue.Unknown
        else
            match v with
            | SqlValue.Bool b -> if b then TruthValue.True else TruthValue.False
            | _ -> TruthValue.Unknown

    /// 3値の判定結果を SqlValue へ戻す（UNKNOWN は NULL）。
    let private ofTruth (t: TruthValue) =
        match t with
        | TruthValue.True -> SqlValue.Bool true
        | TruthValue.False -> SqlValue.Bool false
        | TruthValue.Unknown -> SqlValue.Null

    /// AND の 3値結合（両辺は既に評価済み）。FALSE での短絡は KBinaryL 側で先に済む。
    let private andValue (l: SqlValue) (r: SqlValue) =
        ofTruth (TruthValue.and2 (truthOfValue l) (truthOfValue r))

    /// OR の 3値結合（両辺は既に評価済み）。TRUE での短絡は KBinaryL 側で先に済む。
    let private orValue (l: SqlValue) (r: SqlValue) =
        ofTruth (TruthValue.or2 (truthOfValue l) (truthOfValue r))

    /// 継続（評価待ちの処理）。CPS の継続はクロージャではなく値として持つ。
    /// 再帰を 1つの関数 step の自己再帰にまとめれば末尾呼び出しで回り、
    /// ラムダの継続（継続の数だけフレームが積もる）や相互再帰（1フレーム残る）の問題がない。
    type private Kont =
        /// 単項演算子。値が上がってから適用する。
        | KUnary of UnaryOp
        /// 二項演算子の左辺を評価中。上がってから右辺へ進む。
        | KBinaryL of BinaryOp * Expr
        /// 二項演算子の右辺を評価中。上がってから両辺を結合する。
        | KBinaryR of BinaryOp * SqlValue
        | KIsNull of bool
        /// BETWEEN は x → lo → hi の順に評価する。
        | KBetweenX of Expr * Expr * bool
        | KBetweenLo of SqlValue * Expr * bool
        | KBetweenHi of SqlValue * SqlValue * bool
        /// IN の左辺を評価中（候補リストと否定）。
        | KInX of Expr list * bool
        /// IN の候補 1件を評価中（左辺の値, 否定, 残りの候補, UNKNOWN を見たか）。
        | KInArg of SqlValue * bool * Expr list * bool
        | KLikeX of Expr * bool
        | KLikeP of SqlValue * bool
        /// CASE の WHEN を評価中（当該の WHEN/THEN, 残りの枝, ELSE）。
        | KCaseWhen of (Expr * Expr) * (Expr * Expr) list * Expr option
        /// 関数の引数 1個を評価中（関数名, 確定済みの逆順, 残りの引数）。
        | KFuncArg of string * SqlValue list * Expr list

    /// 評価機の状態。式を 1つ降りるか（Descend）、値が上がってきたか（Arrive）。
    type private Step =
        | Descend of Expr
        | Arrive of SqlValue

    /// 式評価機。降りる／上がるを継続スタック Kont でつなぐ（CPS）。
    /// 再帰はこの関数の自己再帰だけなので全部末尾位置になり、
    /// 式の深さ・IN の候補数に依存せずスタックは一定（継続はヒープのリストに積む）。
    [<TailCall>]
    let rec private step (schema: Schema) (row: SqlValue array) (kont: Kont list) (st: Step) =
        match st with
        | Descend e ->
            match e with
            | Expr.Literal l -> step schema row kont (Arrive(Scalar.ofLiteral l))
            | Expr.Column c ->
                match Schema.resolve schema c with
                | Error er -> Error er
                | Ok i ->
                    match valueAt row i e with
                    | Error er -> Error er
                    | Ok v -> step schema row kont (Arrive v)
            | Expr.Name n ->
                let probeRef = { Qualifier = None; Name = n }

                match Schema.resolve schema probeRef with
                | Error _ -> Error(DbError.name (sprintf "'%s' は列でも関数でもありません" n))
                | Ok i ->
                    match valueAt row i (Expr.Column probeRef) with
                    | Error er -> Error er
                    | Ok v -> step schema row kont (Arrive v)
            | Expr.Star -> Error(DbError.create ErrorKind.SyntaxError "WHERE 句に '*' は置けません")
            | Expr.QualifiedStar q -> Error(DbError.name (sprintf "'%s.*' はここでは使えません" q))
            | Expr.Unary(op, x) -> step schema row (KUnary op :: kont) (Descend x)
            | Expr.Binary(op, a, b) -> step schema row (KBinaryL(op, b) :: kont) (Descend a)
            | Expr.IsNull(x, negated) -> step schema row (KIsNull negated :: kont) (Descend x)
            | Expr.Between(x, lo, hi, negated) -> step schema row (KBetweenX(lo, hi, negated) :: kont) (Descend x)
            | Expr.InList(x, list, negated) -> step schema row (KInX(list, negated) :: kont) (Descend x)
            | Expr.Like(x, p, negated) -> step schema row (KLikeX(p, negated) :: kont) (Descend x)
            | Expr.Case(branches, elseE) ->
                match branches with
                | (w, t) :: tail -> step schema row (KCaseWhen((w, t), tail, elseE) :: kont) (Descend w)
                | [] ->
                    match elseE with
                    | Some rest -> step schema row kont (Descend rest)
                    | None -> step schema row kont (Arrive SqlValue.Null)
            | Expr.FuncCall(n, args) ->
                match args with
                | a :: tail -> step schema row (KFuncArg(n, [], tail) :: kont) (Descend a)
                | [] ->
                    match Scalar.applyFunc n [||] with
                    | Error er -> Error er
                    | Ok v -> step schema row kont (Arrive v)
            | Expr.AggCall(f, _, _) -> Error(DbError.name (sprintf "%s は集約なのでここでは評価できません" f.Name))
        | Arrive v ->
            match kont with
            | [] -> ret v
            | KUnary op :: rest ->
                let computed =
                    match op with
                    | UnaryOp.Not ->
                        ret (
                            if SqlValue.isNull v then
                                SqlValue.Null
                            else
                                SqlValue.Bool(not (boolOf v))
                        )
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

                match computed with
                | Error er -> Error er
                | Ok res -> step schema row rest (Arrive res)
            | KBinaryL(op, b) :: rest ->
                let short =
                    match op with
                    | BinaryOp.And when v = SqlValue.Bool false -> Some(SqlValue.Bool false)
                    | BinaryOp.Or when v = SqlValue.Bool true -> Some(SqlValue.Bool true)
                    | _ -> None

                match short with
                | Some res -> step schema row rest (Arrive res)
                | None -> step schema row (KBinaryR(op, v) :: rest) (Descend b)
            | KBinaryR(op, l) :: rest ->
                let combined =
                    match op with
                    | BinaryOp.Add -> arith false (+) l v
                    | BinaryOp.Sub -> arith false (-) l v
                    | BinaryOp.Mul -> arith false (*) l v
                    | BinaryOp.Div -> arith true (/) l v
                    | BinaryOp.Mod -> arith true (%) l v
                    | BinaryOp.Concat ->
                        if SqlValue.isNull l || SqlValue.isNull v then
                            ret SqlValue.Null
                        else
                            ret (SqlValue.Text(SqlValue.toDisplayString l + SqlValue.toDisplayString v))
                    | BinaryOp.Like ->
                        if SqlValue.isNull l || SqlValue.isNull v then
                            ret SqlValue.Null
                        else
                            ret (
                                SqlValue.Bool(
                                    Scalar.likeMatch (SqlValue.toDisplayString v) (SqlValue.toDisplayString l)
                                )
                            )
                    | BinaryOp.Eq
                    | BinaryOp.Ne
                    | BinaryOp.Lt
                    | BinaryOp.Le
                    | BinaryOp.Gt
                    | BinaryOp.Ge -> compare (nameOf op) l v
                    | BinaryOp.And -> ret (andValue l v)
                    | BinaryOp.Or -> ret (orValue l v)

                match combined with
                | Error er -> Error er
                | Ok res -> step schema row rest (Arrive res)
            | KIsNull negated :: rest ->
                let isNull = SqlValue.isNull v
                step schema row rest (Arrive(SqlValue.Bool(if negated then not isNull else isNull)))
            | KBetweenX(lo, hi, negated) :: rest -> step schema row (KBetweenLo(v, hi, negated) :: rest) (Descend lo)
            | KBetweenLo(x, hi, negated) :: rest -> step schema row (KBetweenHi(x, v, negated) :: rest) (Descend hi)
            | KBetweenHi(x, lov, negated) :: rest ->
                match truthOf "ge" x lov with
                | Error er -> Error er
                | Ok ge ->
                    match truthOf "le" x v with
                    | Error er -> Error er
                    | Ok le ->
                        match TruthValue.and2 ge le with
                        | TruthValue.True -> step schema row rest (Arrive(SqlValue.Bool(not negated)))
                        | TruthValue.False -> step schema row rest (Arrive(SqlValue.Bool negated))
                        | TruthValue.Unknown -> step schema row rest (Arrive SqlValue.Null)
            | KInX(list, negated) :: rest ->
                match list with
                | [] -> step schema row rest (Arrive(SqlValue.Bool negated))
                | cand :: tail -> step schema row (KInArg(v, negated, tail, false) :: rest) (Descend cand)
            | KInArg(x, negated, tail, sawUnknown) :: rest ->
                match truthOf "eq" x v with
                | Error er -> Error er
                | Ok TruthValue.True -> step schema row rest (Arrive(SqlValue.Bool(not negated)))
                | Ok tv ->
                    let saw = sawUnknown || tv = TruthValue.Unknown

                    match tail with
                    | [] -> step schema row rest (Arrive(if saw then SqlValue.Null else SqlValue.Bool negated))
                    | cand :: more -> step schema row (KInArg(x, negated, more, saw) :: rest) (Descend cand)
            | KLikeX(p, negated) :: rest -> step schema row (KLikeP(v, negated) :: rest) (Descend p)
            | KLikeP(x, negated) :: rest ->
                if SqlValue.isNull x || SqlValue.isNull v then
                    step schema row rest (Arrive SqlValue.Null)
                else
                    let m = Scalar.likeMatch (SqlValue.toDisplayString v) (SqlValue.toDisplayString x)
                    step schema row rest (Arrive(SqlValue.Bool(if negated then not m else m)))
            | KCaseWhen((w, t), tail, elseE) :: rest ->
                let matched = not (SqlValue.isNull v) && boolOf v

                if matched then
                    step schema row rest (Descend t)
                else
                    match tail with
                    | (w2, t2) :: more -> step schema row (KCaseWhen((w2, t2), more, elseE) :: rest) (Descend w2)
                    | [] ->
                        match elseE with
                        | Some rest2 -> step schema row rest (Descend rest2)
                        | None -> step schema row rest (Arrive SqlValue.Null)
            | KFuncArg(n, doneRev, tail) :: rest ->
                let acc = v :: doneRev

                match tail with
                | [] ->
                    match Scalar.applyFunc n (List.rev acc |> List.toArray) with
                    | Error er -> Error er
                    | Ok res -> step schema row rest (Arrive res)
                | a :: more -> step schema row (KFuncArg(n, acc, more) :: rest) (Descend a)

    /// 式を評価する。集約呼び出しはここでエラーになる。
    /// 評価機 step を空の継続スタックで始める（末尾再帰のまま深さ・候補数に依存しない）。
    let eval (schema: Schema) (row: SqlValue array) (e: Expr) = step schema row [] (Descend e)

    /// WHERE の判定。UNKNOWN は偽（SQL 標準）。
    let matches (schema: Schema) (row: SqlValue array) (predicate: Expr) =
        eval schema row predicate
        |> Result.map (fun v ->
            match v with
            | SqlValue.Bool b -> b
            | SqlValue.Null -> false
            | SqlValue.Int i -> i <> 0L
            | _ -> false)

// ---------------------------------------------------------------------------
// 6. 走査演算子
// ---------------------------------------------------------------------------

/// 実行時文脈。演算子はこれを「作る時点」に確定させる。
type internal ExecContext =
    {
        Store: RowStore
        /// 読み取りの可視性境界。Op 生成時に 1回だけ決まる。
        Visible: uint64 option
        /// テーブル名（別名含む） -> スキーマ
        Schemas: Map<string, TableSchema>
        /// カタログ。索引の定義を引くのに使う。
        Catalog: Catalog
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

module internal ExecContext =
    let create (store: RowStore) (visible: uint64 option) =
        {
            Store = store
            Visible = visible
            Schemas = Map.empty
            Catalog = Catalog.empty
        }

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
    /// 索引は一意とは限らないので、`WHERE indexed_col = v` が 1行になるとは限らない。
    /// `v` を持つ行をすべて返す。
    let byIndex (ctx: ExecContext) (table: string) (indexName: string) (keyValues: SqlValue array) =
        schemaOf ctx table
        >>= fun schema ->
            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> Error(DbError.name (sprintf "テーブル %s に索引 %s がありません" table indexName))
            | Some idx ->
                Dml.scanByIndexPrefix ctx.Store idx ctx.Visible keyValues System.Int32.MaxValue
                |> Result.map Op.ofArray

    /// 主キーで 1行を引く。
    let byPrimaryKey (ctx: ExecContext) (table: string) (key: SqlValue array) =
        schemaOf ctx table
        >>= fun schema ->
            Dml.getByPrimaryKey ctx.Store schema ctx.Visible key
            |> Result.map (fun row ->
                match row with
                | None -> Op.empty
                | Some r -> Op.ofRow r)

module internal Ops =
    /// 列参照を作る（修飾子はそのまま）。
    let private col (qualifier: string option) (name: string) = { Qualifier = qualifier; Name = name }

    /// 出力列をそのままの列参照にする（`SELECT *` の展開用）。
    let sameRef (c: OutputColumn) = col c.Qualifier c.Name

    /// 集約呼び出しの「識別子」。同じ呼び出しは同じ accum にまとめる。
    /// `COUNT(*)` の引数は `EStar`。DISTINCT フラグではなく、引数で判定する。
    let private isStarArg (x: Expr) =
        match x with
        | Expr.Star -> true
        | _ -> false

    let private aggKey (f: AggFunc) (arg: Expr) =
        sprintf "%s(%s)" f.Name (Expr.toDisplayString arg)

    /// 述語で弾く。
    let filter (schema: Schema) (src: Op) (predicate: Expr) =
        Op.filter (fun row -> Eval.matches schema row predicate) src

    /// ソート。ORDER BY のキー（列位置と方向）を先に解決してから
    /// 比較関数へ落とす。`*` / `t.*` はORDER BY には書けない。
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
                        let bare = { Qualifier = None; Name = c.Name }

                        match Schema.resolve schema c with
                        | Ok i -> Ok(i, dir)
                        | Error _ -> Schema.resolve schema bare |> Result.map (fun i -> i, dir))
                |> all

            resolved
            >>= fun plan ->
                let compareWith (a: SqlValue array) (b: SqlValue array) =
                    let rec compareKeys (rest: (int * SortDirection) list) =
                        match rest with
                        | [] -> 0
                        | (i, dir) :: tail ->
                            match SqlValue.compareValues a.[i] b.[i] with
                            | Error _ -> compareKeys tail
                            | Ok c ->
                                if c = 0 then
                                    compareKeys tail
                                else
                                    match dir with
                                    | SortDirection.Ascending -> c
                                    | SortDirection.Descending -> -c

                    compareKeys plan

                Op.toArray src
                |> Result.map (fun rows ->
                    let sorted = Array.sortWith compareWith rows
                    Op.ofArray sorted)

    /// 重複除去。保持する順序は最初に現れた順（SQL の DISTINCT と同じ）。
    /// 既出のキーを全部覚える必要がある。
    /// 「直前のキーだけ」で比較するのは `DISTINCT` ではなく `DISTINCT ON` にしかならない
    /// （`2, 1, 2` で 2 が 2 回目に出るため、直前比較では除去できない）。
    ///  Costs O(既出キー数 × 平均キー長)。メモリは全キーに比例する。
    /// 値が多いテーブルでは使えないが、`SELECT DISTINCT` の素直な実装としてはこれが正しい。
    let distinct (src: Op) =
        let rec skipSeen (seen: Set<string>) (cur: Op) =
            Op.pull cur (fun row nxt ->
                let k = Row.keyOf row

                if Set.contains k seen then
                    Op.next (skipSeen seen nxt)
                else
                    ret (Some(row, skipSeen (Set.add k seen) nxt)))

        skipSeen Set.empty src

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

    /// 極値（MIN / MAX）の蓄積更新。
    /// 最初に見た値をそのまま採用し、それ以降は wins が真のときだけ入れ替える。
    /// <remarks>
    /// MIN と MAX の 2 コピーを 1 本にした。
    /// Count の加算は呼び出し側の bump が持つのでここに現れない。
    /// 比較は SqlValue.compareValues（型が違うと TypeError）。
    /// </remarks>
    let private keepExtreme
        (v: SqlValue)
        (wins: int -> bool)
        (bumped: Acc)
        (current: SqlValue option)
        (set: Acc -> SqlValue -> Acc)
        =
        match current with
        | None -> ret (set bumped v)
        | Some cur ->
            SqlValue.compareValues v cur
            |> Result.map (fun c -> if wins c then set bumped v else bumped)

    /// 値 1行を蓄積に加える。
    /// isStar が true（`COUNT(*)`）なら NULL でも数える。
    /// それ以外は NULL を無視する（`COUNT(列)` の SQL の挙動）。
    let private addAcc (acc: Acc) (value: SqlValue option) (isStar: bool) =
        let counted =
            if isStar then
                Some SqlValue.Null
            else
                match value with
                | Some v when not (SqlValue.isNull v) -> Some v
                | _ -> None

        let bump = { acc with Count = acc.Count + 1L }

        match counted with
        | None -> ret acc
        | Some v ->
            match acc.Kind with
            | AggFunc.Count -> ret bump
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
                            { bump with
                                SawReal = true
                                SumReal = acc.SumReal + f
                            }
                    | _ ->
                        ret
                            { bump with
                                SumDecimal = acc.SumDecimal + d
                            }
            | AggFunc.Min -> keepExtreme v (fun c -> c < 0) bump acc.Min (fun b x -> { b with Min = Some x })
            | AggFunc.Max -> keepExtreme v (fun c -> c > 0) bump acc.Max (fun b x -> { b with Max = Some x })

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

    /// 式中の集約呼び出しを pre-order に畳み込む（明示スタックの Expr.fold を使う）。
    let private collectAggs (e: Expr) (acc: (AggFunc * bool * Expr) list) =
        Expr.fold
            (fun a node ->
                match node with
                | Expr.AggCall(f, _, x) -> (f, isStarArg x, x) :: a
                | _ -> a)
            acc
            e

    /// 式中に現れる集約呼び出しの一覧（実行順に揃える）。
    let aggCalls (exprs: Expr list) =
        exprs |> List.fold (fun acc e -> collectAggs e acc) [] |> List.rev

    /// 複数の SELECT 項目から集約呼び出しを集める（出現順）。
    let private aggCallsOf (items: SelectItem list) =
        aggCalls (items |> List.map (fun i -> i.Expression))

    /// 式を「集約後の行の位置」で読み替える。
    /// offsets は「表示文字列 -> 列位置」。
    /// 集約呼び出しの他、 GROUP BY の列（`SELECT age, COUNT(*)` の `age`）もここに含まれる。
    let rec private rewriteAggs (e: Expr) (offsets: Map<string, int>) (row: SqlValue array) =
        let slot key =
            match offsets |> Map.tryFind key with
            | Some i when i >= 0 && i < row.Length -> Some(Expr.Literal(Literal.ofValue row.[i]))
            | _ -> None

        match e with
        | Expr.AggCall(f, _, arg) ->
            match slot (aggKey f arg) with
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
        | _ -> Expr.mapChildren (fun c -> rewriteAggs c offsets row) e

    /// 集約結果の「列位置」を索引割りで持つ。
    /// GROUP BY の列（位置 0, 1, ...）と各集約の結果（その後）の両方を持つ。
    let private aggOffsets (groupBy: Expr list) (aggs: (AggFunc * bool * Expr) list) =
        let groupSlots =
            groupBy |> List.mapi (fun i e -> Expr.toDisplayString e, i) |> Map.ofList

        let aggSlots =
            aggs
            |> List.mapi (fun i (f, _, arg) -> aggKey f arg, List.length groupBy + i)
            |> Map.ofList

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

    /// HAVING を張る。集約後の行は位置のみの意味を持つので Schema.empty で評価する。
    let havingFilter (groupBy: Expr list) (aggs: (AggFunc * bool * Expr) list) (predicate: Expr) =
        fun row ->
            let replaced = substituteAggs groupBy aggs predicate row
            Eval.matches Schema.empty row replaced

    /// GROUP BY + 集約。
    /// groupBy が空でも集約があれば 1グループだけ出す。
    /// （`SELECT COUNT(*) FROM 空のテーブル` が 0 ではなく 1行を返さないと
    ///   SQL の通常の挙動と違ってしまうため）
    let aggregate (schema: Schema) (src: Op) (groupBy: Expr list) (aggs: (AggFunc * bool * Expr) list) =
        Op.toArray src
        >>= fun rows ->
            let keyOfRow (row: SqlValue array) =
                groupBy
                |> List.map (fun e -> Eval.eval schema row e)
                |> all
                |> Result.map (fun vs -> Row.keyOf (List.toArray vs))

            let order = ResizeArray<string>()

            let table =
                System.Collections.Generic.Dictionary<string, ResizeArray<SqlValue array>>()

            let rec groupRows (i: int) =
                if i >= rows.Length then
                    let built = table |> Seq.map (fun kv -> kv.Key, List.ofSeq kv.Value) |> Map.ofSeq

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

                        groupRows (i + 1)

            groupRows 0
            |> Result.map (fun (order, table) ->
                let rowOf (k: string) =
                    match table |> Map.tryFind k with
                    | None -> None
                    | Some members ->
                        if List.isEmpty members then
                            None
                        else
                            let first = List.head members

                            let gv =
                                match groupBy |> List.map (fun e -> Eval.eval schema first e) |> all with
                                | Ok vs -> List.toArray vs
                                | Error _ -> [||]

                            let oneAgg (spec: AggFunc * bool * Expr) =
                                let f, star, arg = spec
                                let start = emptyAcc f

                                let add (acc: Acc) (row: SqlValue array) =
                                    if star then
                                        addAcc acc None true
                                    else
                                        Eval.eval schema row arg >>= fun v -> addAcc acc (Some v) false

                                match foldOk add start members with
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

                chain order)

    /// 結合の述語を「行の組」に対して評価する。
    /// ON 句は両テーブルの列を同時に参照するので、左右を連結した行で評価する。
    let private joinPred (leftSchema: Schema) (rightSchema: Schema) (on: Expr) (l: SqlValue array) (r: SqlValue array) =
        Eval.matches (leftSchema.Concat rightSchema) (Array.append l r) on

    /// nested loop 結合の内側ループ（1つの左行 × 右の全行）を回す共通処理。
    /// onExhausted matched は右を最後まで試し終えたときの Op。
    /// onError matched は ON 句の評価が失敗したときの Op。
    /// matched は「この左行で既に一致を出したか」。
    /// INNER はどちらも matched を無視し、LEFT だけが NULL 拡張と継続を使い分ける。
    let private joinLoop
        (leftSchema: Schema)
        (rightSchema: Schema)
        (on: Expr)
        (onExhausted: bool -> Op)
        (onError: bool -> Op)
        (rightRows: SqlValue array array)
        (lrow: SqlValue array)
        =
        let rec loop (matched: bool) (rs: SqlValue array array) =
            if Array.isEmpty rs then
                onExhausted matched
            else
                let rrow = rs.[0]
                let rest = rs |> Array.skip 1

                match joinPred leftSchema rightSchema on lrow rrow with
                | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, loop true rest)))
                | Ok false -> Op.Compute(fun () -> Op.next (loop matched rest))
                | Error _ -> onError matched

        loop false rightRows

    /// INNER JOIN（nested loop）。右側は内側ループごとに先頭から引き直す。
    /// 一致が無い左行は捨て、ON 句の評価失敗は全体を打ち切る。
    let innerJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        Op.toArray right
        |> Result.map (fun rightRows ->
            let rec joinRows (cur: Op) =
                Op.pull cur (fun lrow lnext ->
                    Op.next (
                        joinLoop
                            leftSchema
                            rightSchema
                            on
                            (fun _ -> joinRows lnext)
                            (fun _ -> Op.empty)
                            rightRows
                            lrow
                    ))

            joinRows left)

    /// LEFT JOIN。左の行に右が 1行も無ければ NULL で埋める。
    /// 一致済みの左行で ON 句の評価が失敗したときだけ次の左行へ進む
    /// （未一致なら INNER と同じく打ち切る）。
    let leftJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        Op.toArray right
        |> Result.map (fun rightRows ->
            let width = rightSchema.Columns.Length

            let rec joinRows (cur: Op) =
                Op.pull cur (fun lrow lnext ->
                    let onExhausted matched =
                        if matched then
                            joinRows lnext
                        else
                            Op.Step(Array.append lrow (Array.create width SqlValue.Null), joinRows lnext)

                    let onError matched =
                        if matched then joinRows lnext else Op.empty

                    Op.next (joinLoop leftSchema rightSchema on onExhausted onError rightRows lrow))

            joinRows left)

    /// 射影。`SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は対象テーブルの全列に展開する。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    let project (schema: Schema) (groupBy: Expr list) (src: Op) (items: SelectItem list) =
        if List.isEmpty items then
            src
        else
            let plan =
                items
                |> List.map (fun item ->
                    let exprs =
                        match Schema.expandStar schema item.Expression with
                        | Some(_, columns) -> columns |> List.map (fun c -> sameRef c |> Expr.Column)
                        | None -> [ item.Expression ]

                    item, exprs)

            let allExprs = plan |> List.collect (fun (_, exprs) -> exprs)
            let foundAggs = aggCallsOf items

            if List.isEmpty foundAggs then
                Op.map (fun row -> allExprs |> List.map (Eval.eval schema row) |> all |> Result.map List.toArray) src
            else
                Op.map
                    (fun row ->
                        allExprs
                        |> List.map (fun e -> Eval.eval Schema.empty row (substituteAggs groupBy foundAggs e row))
                        |> all
                        |> Result.map List.toArray)
                    src

// ---------------------------------------------------------------------------
// 7. 更新演算子（INSERT / UPDATE / DELETE）
// ---------------------------------------------------------------------------

// 書き込みは Transaction にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
module internal DmlOps =
    /// 定数式だけなら行の外で評価できる（列参照が無い）。
    let private evalConstant (e: Expr) = Eval.eval Schema.empty [||] e

    /// WHERE を評価するときの対象スキーマ（UPDATE / DELETE で共通）。
    let private targetSchemaOf (schema: TableSchema) = Schema.ofTable schema.Name None schema

    /// WHERE の述語。WHERE が無ければ「全部通過」（UPDATE / DELETE で共通）。
    let private matchesWhere (targetSchema: Schema) (where: Expr option) (row: SqlValue array) =
        match where with
        | Some w -> Eval.matches targetSchema row w
        | None -> ret true

    /// 値式を列位置に対応付け、型強制して (位置, 値) のリストにする。
    /// INSERT は行の外（定数）で、UPDATE は現在行を参照して評価するので、
    /// 評価器 evalOne を受け取って両方を同じ経路で処理する。
    let private gatherWith
        (evalOne: Expr -> Result<SqlValue, DbError>)
        (schema: TableSchema)
        (values: Expr list)
        (idxs: int list)
        =
        let rec gather (vs: Expr list) (is_: int list) (acc: (int * SqlValue) list) =
            match vs, is_ with
            | [], [] -> ret acc
            | v :: rest, i :: restIdx ->
                match evalOne v with
                | Error e -> Error e
                | Ok value ->
                    let colDef = schema.Columns.[i]

                    match SqlValue.coerce colDef.DataType value with
                    | Error e -> Error e
                    | Ok coerced -> gather rest restIdx ((i, coerced) :: acc)
            | _ -> ret acc

        gather values idxs []

    /// 表を全走査して 1 行ずつ処理する UPDATE / DELETE 共通の土台。
    /// matched が偽の行は件数を変えず先へ進み、 onRow が「(トランザクション, 件数)」を進めた結果を返す。
    let private foldRows
        (txn: Transaction)
        (ctx: ExecContext)
        (schema: TableSchema)
        (matched: SqlValue array -> Result<bool, DbError>)
        (onRow: Transaction -> int -> SqlValue array -> Result<Transaction * int, DbError>)
        =
        Dml.scanByPrimaryKey ctx.Store schema ctx.Visible 0
        >>= foldOk (fun (t, c) row -> matched row >>= fun keep -> if keep then onRow t c row else ret (t, c)) (txn, 0)

    /// gather で埋まった `(列位置, 値)` の対を、列順の行へ戻す。
    /// 埋まっていない位置は fallback に委ねる
    /// （INSERT は DEFAULT / NULL、UPDATE は元の値）。
    let private scatter (width: int) (filled: (int * SqlValue) list) (fallback: int -> SqlValue) =
        Array.init width (fun i ->
            match filled |> List.tryFind (fun (j, _) -> j = i) with
            | Some(_, v) -> v
            | None -> fallback i)

    /// INSERT。columns が None ならテーブル定義の順に並べる。
    /// DEFAULT / NULL の充足と型強制はここで行う。
    /// TableSchema.validateRow が NOT NULL と型を最終確認する。
    let insert (schema: TableSchema) (txn: Transaction) (columns: string list option) (rows: Expr list list) =
        match columns with
        | None -> schema.Columns |> List.mapi (fun i _ -> i) |> ret
        | Some names -> names |> List.map (fun n -> schema.RequireIndexOf n) |> all
        >>= fun idxs ->
            let width = schema.Columns.Length

            let buildRow (values: Expr list) =
                if List.length values <> List.length idxs then
                    Error(
                        DbError.create
                            ErrorKind.SyntaxError
                            (sprintf "INSERT の列数(%d)と値の数(%d)が合いません" (List.length idxs) (List.length values))
                    )
                else
                    gatherWith evalConstant schema values idxs
                    |> Result.map (fun filled ->
                        scatter width filled (fun i ->
                            match schema.Columns.[i].Constraint.Default with
                            | Some d ->
                                match evalConstant d with
                                | Ok dv -> dv
                                | Error _ -> SqlValue.Null
                            | None -> SqlValue.Null))

            rows
            |> foldOk
                (fun (txnAcc, count) values ->
                    buildRow values
                    >>= fun row -> Dml.insert schema txnAcc row |> Result.map (fun t -> t, count + 1))
                (txn, 0)

    /// UPDATE。assignments の右辺は現在行で評価する。
    /// 検索キーは更新前の行の主キー。主キー列を書き換えると
    /// 古い位置の行が残って重複する。
    let update
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (assignments: (string * Expr) list)
        (where: Expr option)
        =
        let targetSchema = targetSchemaOf schema

        assignments
        |> List.map (fun (name, _) -> schema.RequireIndexOf name)
        |> all
        >>= fun targets ->
            let assign (row: SqlValue array) =
                gatherWith (Eval.eval targetSchema row) schema (List.map snd assignments) targets
                |> Result.map (fun filled -> scatter row.Length filled (fun i -> row.[i]))

            foldRows txn ctx schema (matchesWhere targetSchema where) (fun txnAcc count row ->
                assign row
                >>= fun newRow ->
                    let pk = TableSchema.primaryKeyOf schema row

                    Dml.updateByPrimaryKey ctx.Store schema ctx.Visible txnAcc pk newRow
                    |> Result.map (fun t -> t, count + 1))

    /// DELETE。WHERE に合う行を主キー + 全索引に tombstone で消す。
    let delete (ctx: ExecContext) (schema: TableSchema) (txn: Transaction) (where: Expr option) =
        let targetSchema = targetSchemaOf schema

        foldRows txn ctx schema (matchesWhere targetSchema where) (fun t count row ->
            let pk = TableSchema.primaryKeyOf schema row

            Dml.deleteByPrimaryKey ctx.Store schema ctx.Visible t pk
            |> Result.map (fun t2 -> t2, count + 1))
