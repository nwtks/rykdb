namespace RykDb.Core

/// AST を末尾再帰で走査するための内部補助。
module internal Internal =
    /// 明示的なスタック（末尾再帰で辿る list）へ、xs の要素を
    /// 先頭から順に積み上げる補助。
    /// xs @ stack や stack @ xs だと O(n^2) になるので
    /// 反転しながら畳む。こちらも末尾再帰。
    let pushAll (xs: 'a list) (stack: 'a list) =
        let rec revInto acc =
            function
            | [] -> acc
            | x :: rest -> revInto (x :: acc) rest

        revInto stack xs

/// SQL テキスト中の位置。エラー報告に使う。
type internal Position =
    {
        Offset: int
        Line: int
        Column: int
    }

    static member Zero = { Offset = 0; Line = 1; Column = 1 }

    override x.ToString() = sprintf "%d:%d" x.Line x.Column

/// リテラル。
[<RequireQualifiedAccess>]
type internal Literal =
    | Null
    | Bool of bool
    | Int of int64
    | Real of float
    | Decimal of decimal
    | String of string
    | Blob of byte[]

    /// リテラルの型。束縛時の型決定に使う。
    member this.DataType =
        match this with
        | Null -> DataType.Null
        | Bool _ -> DataType.Boolean
        | Int _ -> DataType.Integer
        | Real _ -> DataType.Real
        | Decimal _ -> DataType.Decimal
        | String _ -> DataType.Text
        | Blob _ -> DataType.Blob

    member this.ToSqlValue: SqlValue =
        match this with
        | Null -> SqlValue.Null
        | Bool b -> SqlValue.Bool b
        | Int i -> SqlValue.Int i
        | Real f -> SqlValue.Real f
        | Decimal d -> SqlValue.Decimal d
        | String s -> SqlValue.Text s
        | Blob b -> SqlValue.Blob b

module internal Literal =
    /// 実行時値からリテラル表示へ（DDL 再構築とエラー表示に使う）。
    let ofValue (v: SqlValue) =
        match v with
        | SqlValue.Null -> Literal.Null
        | SqlValue.Bool b -> Literal.Bool b
        | SqlValue.Int i -> Literal.Int i
        | SqlValue.Real f -> Literal.Real f
        | SqlValue.Decimal d -> Literal.Decimal d
        | SqlValue.Text s -> Literal.String s
        | SqlValue.Blob b -> Literal.Blob b
        | SqlValue.Date d -> Literal.String(d.ToString "yyyy-MM-dd")
        | SqlValue.Timestamp t -> Literal.String(t.UtcDateTime.ToString "yyyy-MM-ddTHH:mm:ss.fffffffZ")
        | SqlValue.Uuid g -> Literal.String(g.ToString "D")

/// 列参照。修飾子の有無を区別したまま保持し、束縛時に解決する。
type internal ColumnRef =
    {
        Qualifier: string option
        Name: string
    }

    member x.Display =
        match x.Qualifier with
        | Some q -> sprintf "%s.%s" q x.Name
        | None -> x.Name

/// 集計関数。
[<RequireQualifiedAccess>]
type internal AggFunc =
    | Count
    | Sum
    | Avg
    | Min
    | Max

    member x.Name =
        match x with
        | Count -> "COUNT"
        | Sum -> "SUM"
        | Avg -> "AVG"
        | Min -> "MIN"
        | Max -> "MAX"

module internal AggFunc =
    let tryParse (name: string) =
        match name.ToUpperInvariant() with
        | "COUNT" -> Some AggFunc.Count
        | "SUM" -> Some AggFunc.Sum
        | "AVG" -> Some AggFunc.Avg
        | "MIN" -> Some AggFunc.Min
        | "MAX" -> Some AggFunc.Max
        | _ -> None

    /// 返り値型。COUNT は常に INTEGER。それ以外は入力の型に従う。
    let resultType (f: AggFunc) =
        match f with
        | AggFunc.Count -> Some DataType.Integer
        | AggFunc.Avg -> Some DataType.Real
        | _ -> None

[<RequireQualifiedAccess>]
type internal UnaryOp =
    /// 算術否定
    | Neg
    /// 単項 +
    | Pos
    /// 論理否定
    | Not

    member x.Display =
        match x with
        | Neg -> "-"
        | Pos -> "+"
        | Not -> "NOT"

[<RequireQualifiedAccess>]
type internal BinaryOp =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Concat
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or
    | Like

    /// NULL 伝播の扱いに必要な比較演算子かどうか。
    member x.IsComparison =
        match x with
        | Eq
        | Ne
        | Lt
        | Le
        | Gt
        | Ge -> true
        | _ -> false

    member x.IsLogical =
        match x with
        | And
        | Or -> true
        | _ -> false

    member x.Display =
        match x with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "%"
        | Concat -> "||"
        | Eq -> "="
        | Ne -> "<>"
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | And -> "AND"
        | Or -> "OR"
        | Like -> "LIKE"

/// 式。束縛後は列参照が解決済みになる。
[<RequireQualifiedAccess; NoComparison; NoEquality>]
type internal Expr =
    | Literal of Literal
    /// 修飾子付きの列参照
    | Column of ColumnRef
    /// 修飾子なしの裸の名前。スカラー関数の候補。
    | Name of string
    | Unary of UnaryOp * Expr
    | Binary of BinaryOp * Expr * Expr
    /// 否定なら isNegative = true
    | IsNull of Expr * bool
    /// low / high。否定なら isNegative = true
    | Between of Expr * Expr * Expr * bool
    /// 否定なら isNegative = true
    | InList of Expr * Expr list * bool
    /// 否定なら isNegative = true
    | Like of Expr * Expr * bool
    /// (WHEN 式, THEN 式) の並び。ELSE がないなら None。
    | Case of (Expr * Expr) list * Expr option
    /// スカラー関数呼び出し
    | FuncCall of string * Expr list
    /// 集計呼び出し。COUNT(*) は EStar を持つ。
    | AggCall of AggFunc * bool * Expr
    | Star
    /// `t.*`
    | QualifiedStar of string

    /// 式中のすべての列参照を列挙する。束縛と空き列判定に使う。
    /// <remarks>
    /// 走査する式を「スタック」として持ち、列参照を蓄積器的 Reverse に積む。
    /// `go a @ go b` とすると再帰呼び出しが末尾位置に来ないうえ O(n^2) になるので、
    /// 末尾再帰の形に書き換えている。
    /// </remarks>
    member this.ColumnRefs =
        let rec go (stack: Expr list) (acc: ColumnRef list) =
            match stack with
            | [] -> List.rev acc
            | Literal _ :: rest -> go rest acc
            | Name _ :: rest -> go rest acc
            | Star :: rest -> go rest acc
            | QualifiedStar _ :: rest -> go rest acc
            | Column c :: rest -> go rest (c :: acc)
            | Unary(_, x) :: rest -> go (x :: rest) acc
            | Binary(_, a, b) :: rest -> go (b :: a :: rest) acc
            | IsNull(x, _) :: rest -> go (x :: rest) acc
            | Between(x, a, b, _) :: rest -> go (b :: a :: x :: rest) acc
            | InList(x, xs, _) :: rest -> go (Internal.pushAll xs (x :: rest)) acc
            | Like(x, p, _) :: rest -> go (p :: x :: rest) acc
            | Case(branches, elseE) :: rest ->
                let conds = branches |> List.collect (fun (w, t) -> [ w; t ])

                let withElse =
                    match elseE with
                    | Some e -> e :: conds
                    | None -> conds

                go (Internal.pushAll withElse rest) acc
            | FuncCall(_, args) :: rest -> go (Internal.pushAll args rest) acc
            | AggCall(_, _, x) :: rest -> go (x :: rest) acc

        go [ this ] []


/// Expr.toDisplayString が使う出力項目。
/// Sub の closer を先に与えておくことで、式の出力が終わってから
/// 閉じ括弧などを出力する順序を、末尾再帰のまま表現できる。
[<RequireQualifiedAccess>]
type private PrintStep =
    /// そのまま出力する文字列
    | Text of string
    /// 式と、その後に出力する項目
    | Sub of Expr * PrintStep list

module internal Expr =
    let literal l = Expr.Literal l

    let column q n = Expr.Column { Qualifier = q; Name = n }

    let name n = Expr.Name n
    let unary op e = Expr.Unary(op, e)
    let binary op a b = Expr.Binary(op, a, b)

    /// 式に集計呼び出しが含まれるか。GROUP BY の妥当性判定に使う。
    /// <remarks>
    /// `||` の短絡評価を使うと再帰呼び出しが末尾位置に来ないため、
    /// 明示的なスタックで走査し「1 つでも見つかれば true」で打ち切る形にしている。
    /// 短絡評価の ronda 打ち切りは失われるが、結果は論理 OR で同じなので正しさは保たれる。
    /// </remarks>
    let isAggregate (e: Expr) =
        let rec go (stack: Expr list) =
            match stack with
            | [] -> false
            | Expr.AggCall _ :: _ -> true
            | Expr.Literal _ :: rest -> go rest
            | Expr.Column _ :: rest -> go rest
            | Expr.Name _ :: rest -> go rest
            | Expr.Star :: rest -> go rest
            | Expr.QualifiedStar _ :: rest -> go rest
            | Expr.Unary(_, x) :: rest -> go (x :: rest)
            | Expr.Binary(_, a, b) :: rest -> go (b :: a :: rest)
            | Expr.IsNull(x, _) :: rest -> go (x :: rest)
            | Expr.Between(x, a, b, _) :: rest -> go (b :: a :: x :: rest)
            | Expr.InList(x, xs, _) :: rest -> go (Internal.pushAll xs (x :: rest))
            | Expr.Like(x, p, _) :: rest -> go (p :: x :: rest)
            | Expr.Case(branches, elseE) :: rest ->
                let conds = branches |> List.collect (fun (w, t) -> [ w; t ])

                let withElse =
                    match elseE with
                    | Some e -> e :: conds
                    | None -> conds

                go (Internal.pushAll withElse rest)
            | Expr.FuncCall(_, args) :: rest -> go (Internal.pushAll args rest)

        go [ e ]

    /// 式をテキストへ戻す（EXPLAIN やエラーメッセージ用）。
    /// 「式」と「その式を出したあとに吐く文字列」を 1 本のスタックで表現し、
    /// 末尾再帰で処理する。`go x` のあとに `put ")"` と書くと再帰呼び出しが
    /// 末尾位置にならず、深い式（1 万段超）でスタックを食うため。
    let toDisplayString (e: Expr) =
        let sb = System.Text.StringBuilder()
        let put (s: string) = sb.Append s |> ignore
        let text (s: string) = PrintStep.Text s
        let sub (x: Expr) (closer: PrintStep list) = PrintStep.Sub(x, closer)

        let rec go (steps: PrintStep list) =
            match steps with
            | [] -> ()
            | PrintStep.Text s :: rest ->
                put s
                go rest
            | PrintStep.Sub(e, closer) :: rest ->
                let next = closer @ rest

                match e with
                | Expr.Literal l ->
                    put (SqlLiteral.toSqlLiteral l.ToSqlValue)
                    go next
                | Expr.Column c ->
                    put c.Display
                    go next
                | Expr.Name n ->
                    put n
                    go next
                | Expr.Star ->
                    put "*"
                    go next
                | Expr.QualifiedStar q ->
                    put q
                    put ".*"
                    go next
                | Expr.Unary(op, x) ->
                    put "("
                    put op.Display
                    put " "
                    go (sub x [ text ")" ] :: next)
                | Expr.Binary(op, a, b) ->
                    put "("
                    go (sub a [ text " "; text op.Display; text " " ] :: sub b [ text ")" ] :: next)
                | Expr.IsNull(x, neg) ->
                    put "("
                    go (sub x [ text (if neg then " IS NOT NULL)" else " IS NULL)") ] :: next)
                | Expr.Between(x, a, b, neg) ->
                    put "("

                    go (
                        sub x [ text (if neg then " NOT BETWEEN " else " BETWEEN ") ]
                        :: sub a [ text " AND " ]
                        :: sub b [ text ")" ]
                        :: next
                    )
                | Expr.InList(x, xs, neg) ->
                    put "("
                    let n = List.length xs

                    // 最後の要素の直後で "))" を閉じる。
                    // それ以外は ", " を付ける（i=0 の前にも "," を付けると "a IN (, 1)" になる）。
                    let items =
                        xs |> List.mapi (fun i y -> sub y [ text (if i = n - 1 then "))" else ", ") ])

                    go (sub x [ text (if neg then " NOT IN (" else " IN (") ] :: (items @ next))
                | Expr.Like(x, p, neg) ->
                    put "("

                    go (
                        sub x [ text (if neg then " NOT LIKE " else " LIKE ") ]
                        :: sub p [ text ")" ]
                        :: next
                    )
                | Expr.Case(branches, elseE) ->
                    put "(CASE "

                    // WHEN / THEN / ELSE / END を出さないと AST の中身が分からなくなる。
                    let branchSteps =
                        branches
                        |> List.collect (fun (w, t) -> [ text "WHEN "; sub w [ text " THEN " ]; sub t [ text " " ] ])

                    let elseSteps =
                        match elseE with
                        | Some v -> [ text "ELSE "; sub v [ text " " ] ]
                        | None -> []

                    go ((branchSteps @ elseSteps @ [ text "END)" ]) @ next)
                | Expr.FuncCall(n, args) ->
                    put n
                    put "("
                    let n = List.length args

                    let items =
                        args |> List.mapi (fun i y -> sub y [ text (if i = n - 1 then ")" else ", ") ])

                    go (items @ next)
                | Expr.AggCall(f, distinct, x) ->
                    put f.Name
                    put "("

                    if distinct then
                        put "DISTINCT "

                    go (sub x [ text ")" ] :: next)

        go [ PrintStep.Sub(e, []) ]
        sb.ToString()

[<RequireQualifiedAccess>]
type internal JoinKind =
    | Inner
    | Left

type internal TableRef =
    {
        Name: string
        Alias: string option
    }

    member x.Display =
        match x.Alias with
        | Some a -> sprintf "%s AS %s" x.Name a
        | None -> x.Name

type internal JoinClause =
    {
        Kind: JoinKind
        Table: TableRef
        On: Expr
    }

[<RequireQualifiedAccess>]
type internal SortDirection =
    | Ascending
    | Descending

type internal SortKey =
    {
        Expression: Expr
        Direction: SortDirection
    }

type internal SelectItem =
    {
        Expression: Expr
        Alias: string option
    }

    member x.Display =
        match x.Alias with
        | Some a -> sprintf "%s AS %s" (Expr.toDisplayString x.Expression) a
        | None -> Expr.toDisplayString x.Expression

type internal ColumnConstraint =
    {
        NotNull: bool
        PrimaryKey: bool
        Unique: bool
        Default: Expr option
    }

    static member Empty =
        {
            NotNull = false
            PrimaryKey = false
            Unique = false
            Default = None
        }

type internal ColumnDefinition =
    {
        Name: string
        DataType: DataType
        TypeArguments: (string * int) list
        Constraint: ColumnConstraint
    }

[<RequireQualifiedAccess>]
type internal TableConstraint =
    | PrimaryKeyColumns of string list
    | UniqueColumns of string list

type internal CreateTable =
    {
        Name: string
        IfNotExists: bool
        Columns: ColumnDefinition list
        Constraints: TableConstraint list
    }

type internal InsertStatement =
    {
        Table: TableRef
        Columns: string list option
        Rows: Expr list list
    }

type internal UpdateStatement =
    {
        Table: TableRef
        Assignments: (string * Expr) list
        Where: Expr option
    }

type internal DeleteStatement = { Table: TableRef; Where: Expr option }

type internal SelectStatement =
    {
        Distinct: bool
        Items: SelectItem list
        From: TableRef option
        Joins: JoinClause list
        Where: Expr option
        GroupBy: Expr list
        Having: Expr option
        OrderBy: SortKey list
        Limit: Expr option
        Offset: Expr option
    }

type internal CreateIndex =
    {
        Name: string
        Table: string
        Columns: string list
        Unique: bool
    }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type internal Statement =
    | Select of SelectStatement
    | Insert of InsertStatement
    | Update of UpdateStatement
    | Delete of DeleteStatement
    | CreateTable of CreateTable
    | DropTable of string * bool
    | CreateIndex of CreateIndex
    | DropIndex of string * bool
    | Begin
    | Commit
    | Rollback

    member x.Describe =
        match x with
        | Select _ -> "SELECT"
        | Insert _ -> "INSERT"
        | Update _ -> "UPDATE"
        | Delete _ -> "DELETE"
        | CreateTable _ -> "CREATE TABLE"
        | DropTable _ -> "DROP TABLE"
        | CreateIndex _ -> "CREATE INDEX"
        | DropIndex _ -> "DROP INDEX"
        | Begin -> "BEGIN"
        | Commit -> "COMMIT"
        | Rollback -> "ROLLBACK"
