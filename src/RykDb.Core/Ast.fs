namespace RykDb.Core

/// SQL テキスト中の位置。
/// エラー報告に使う。
type internal Position = { Offset: int; Line: int; Column: int }

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

    member this.ToSqlValue =
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

/// 列参照。
/// 修飾子の有無を区別したまま保持し、束縛時に解決する。
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

/// 式。
/// 束縛後は列参照が解決済みになる。
[<RequireQualifiedAccess; NoComparison; NoEquality>]
type internal Expr =
    | Literal of Literal
    /// 修飾子付きの列参照
    | Column of ColumnRef
    /// 修飾子なしの裸の名前。
    /// スカラー関数の候補。
    | Name of string
    | Unary of UnaryOp * Expr
    | Binary of BinaryOp * Expr * Expr
    /// 否定なら isNegative = true
    | IsNull of Expr * bool
    /// low / high。
    /// 否定なら isNegative = true
    | Between of Expr * Expr * Expr * bool
    /// 否定なら isNegative = true
    | InList of Expr * Expr list * bool
    /// 否定なら isNegative = true
    | Like of Expr * Expr * bool
    /// (WHEN 式, THEN 式) の並び。
    /// ELSE がないなら None。
    | Case of (Expr * Expr) list * Expr option
    /// スカラー関数呼び出し
    | FuncCall of string * Expr list
    /// 集計呼び出し。
    /// COUNT(*) は EStar を持つ。
    | AggCall of AggFunc * bool * Expr
    | Star
    /// `t.*`
    | QualifiedStar of string

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
    /// 直接の子式をリストで返す（走査順は左から右）。
    /// 再帰は呼び出し側が行う。
    let children (e: Expr) =
        match e with
        | Expr.Literal _
        | Expr.Column _
        | Expr.Name _
        | Expr.Star
        | Expr.QualifiedStar _ -> []
        | Expr.Unary(_, x) -> [ x ]
        | Expr.Binary(_, a, b) -> [ a; b ]
        | Expr.IsNull(x, _) -> [ x ]
        | Expr.Between(x, a, b, _) -> [ x; a; b ]
        | Expr.InList(x, xs, _) -> x :: xs
        | Expr.Like(x, p, _) -> [ x; p ]
        | Expr.Case(branches, elseE) -> (branches |> List.collect (fun (w, t) -> [ w; t ])) @ (elseE |> Option.toList)
        | Expr.FuncCall(_, args) -> args
        | Expr.AggCall(_, _, x) -> [ x ]

    /// 各子式に f を適用して式を再構成する（構造は維持）。
    let mapChildren (f: Expr -> Expr) (e: Expr) =
        match e with
        | Expr.Literal _
        | Expr.Column _
        | Expr.Name _
        | Expr.Star
        | Expr.QualifiedStar _ -> e
        | Expr.Unary(op, x) -> Expr.Unary(op, f x)
        | Expr.Binary(op, a, b) -> Expr.Binary(op, f a, f b)
        | Expr.IsNull(x, neg) -> Expr.IsNull(f x, neg)
        | Expr.Between(x, a, b, neg) -> Expr.Between(f x, f a, f b, neg)
        | Expr.InList(x, xs, neg) -> Expr.InList(f x, List.map f xs, neg)
        | Expr.Like(x, p, neg) -> Expr.Like(f x, f p, neg)
        | Expr.Case(branches, elseE) -> Expr.Case(List.map (fun (w, t) -> f w, f t) branches, Option.map f elseE)
        | Expr.FuncCall(n, args) -> Expr.FuncCall(n, List.map f args)
        | Expr.AggCall(f2, star, x) -> Expr.AggCall(f2, star, f x)

    /// 各子式にモナド関数 f（Result を返す）を適用して式を再構成する。
    let mapChildrenM (f: Expr -> Result<Expr, DbError>) (e: Expr) =
        match e with
        | Expr.Literal _
        | Expr.Column _
        | Expr.Name _
        | Expr.Star
        | Expr.QualifiedStar _ -> ret e
        | Expr.Unary(op, x) -> f x |> Result.map (fun i -> Expr.Unary(op, i))
        | Expr.Binary(op, a, b) -> f a >>= fun a2 -> f b |> Result.map (fun b2 -> Expr.Binary(op, a2, b2))
        | Expr.IsNull(x, neg) -> f x |> Result.map (fun i -> Expr.IsNull(i, neg))
        | Expr.Between(a, b, c, neg) ->
            f a
            >>= fun a2 -> f b >>= fun b2 -> f c |> Result.map (fun c2 -> Expr.Between(a2, b2, c2, neg))
        | Expr.InList(a, xs, neg) ->
            f a
            >>= fun a2 -> xs |> List.map f |> all |> Result.map (fun vs -> Expr.InList(a2, vs, neg))
        | Expr.Like(a, b, neg) -> f a >>= fun a2 -> f b |> Result.map (fun b2 -> Expr.Like(a2, b2, neg))
        | Expr.Case(branches, elseE) ->
            branches
            |> List.map (fun (w, t) -> f w >>= fun w2 -> f t |> Result.map (fun t2 -> w2, t2))
            |> all
            >>= fun branches2 ->
                match elseE with
                | None -> ret None
                | Some e2 -> f e2 |> Result.map Some
                |> Result.map (fun el -> Expr.Case(branches2, el))
        | Expr.FuncCall(n, args) -> args |> List.map f |> all |> Result.map (fun as2 -> Expr.FuncCall(n, as2))
        | Expr.AggCall(f2, star, arg) -> f arg |> Result.map (fun a -> Expr.AggCall(f2, star, a))

    /// 式木を明示スタックで pre-order にたどり、各ノードについて状態を畳み込む。
    /// 走査順は「根 → 子を左から右へ」。
    let fold (f: 's -> Expr -> 's) (state: 's) (root: Expr) =
        let pushAll (xs: 'a list) (stack: 'a list) =
            let rec revInto acc =
                function
                | [] -> acc
                | x :: rest -> revInto (x :: acc) rest

            revInto stack xs

        let rec foldStack (stack: Expr list) (acc: 's) =
            match stack with
            | [] -> acc
            | e :: rest ->
                let acc2 = f acc e
                foldStack (pushAll (List.rev (children e)) rest) acc2

        foldStack [ root ] state

    /// 式中のすべての列参照を列挙する。
    /// 束縛と空き列判定に使う。
    /// 末尾再帰の明示スタック走査（ColumnRefs の既存の挙動を維持）。
    let columnRefs (e: Expr) =
        fold
            (fun acc e ->
                match e with
                | Expr.Column c -> c :: acc
                | _ -> acc)
            []
            e
        |> List.rev

    /// 式に集計呼び出しが含まれるか。
    /// GROUP BY の妥当性判定に使う。
    /// 短絡評価の早期打ち切りは失われるが、結果は論理 OR で同じ。
    let isAggregate (e: Expr) =
        fold
            (fun found e ->
                found
                || match e with
                   | Expr.AggCall _ -> true
                   | _ -> false)
            false
            e

    /// 式をテキストへ戻す（EXPLAIN やエラーメッセージ用）。
    /// 「式」と「その式を出したあとに吐く文字列」を 1本のスタックで表現し、末尾再帰で処理する。
    /// writeSteps x のあとに put ")" と書くと再帰呼び出しが末尾位置にならず、
    /// 深い式（1万段超）でスタックを食うため。
    let toDisplayString (e: Expr) =
        let sb = System.Text.StringBuilder()
        let put (s: string) = sb.Append s |> ignore
        let text (s: string) = PrintStep.Text s
        let sub (x: Expr) (closer: PrintStep list) = PrintStep.Sub(x, closer)

        let rec writeSteps (steps: PrintStep list) =
            match steps with
            | [] -> ()
            | PrintStep.Text s :: rest ->
                put s
                writeSteps rest
            | PrintStep.Sub(e, closer) :: rest ->
                let next = closer @ rest

                match e with
                | Expr.Literal l ->
                    put (SqlLiteral.toSqlLiteral l.ToSqlValue)
                    writeSteps next
                | Expr.Column c ->
                    put c.Display
                    writeSteps next
                | Expr.Name n ->
                    put n
                    writeSteps next
                | Expr.Star ->
                    put "*"
                    writeSteps next
                | Expr.QualifiedStar q ->
                    put q
                    put ".*"
                    writeSteps next
                | Expr.Unary(op, x) ->
                    put "("
                    put op.Display
                    put " "
                    writeSteps (sub x [ text ")" ] :: next)
                | Expr.Binary(op, a, b) ->
                    put "("
                    writeSteps (sub a [ text " "; text op.Display; text " " ] :: sub b [ text ")" ] :: next)
                | Expr.IsNull(x, neg) ->
                    put "("
                    writeSteps (sub x [ text (if neg then " IS NOT NULL)" else " IS NULL)") ] :: next)
                | Expr.Between(x, a, b, neg) ->
                    put "("

                    writeSteps (
                        sub x [ text (if neg then " NOT BETWEEN " else " BETWEEN ") ]
                        :: sub a [ text " AND " ]
                        :: sub b [ text ")" ]
                        :: next
                    )
                | Expr.InList(x, xs, neg) ->
                    put "("
                    let n = List.length xs

                    let items =
                        xs |> List.mapi (fun i y -> sub y [ text (if i = n - 1 then "))" else ", ") ])

                    writeSteps (sub x [ text (if neg then " NOT IN (" else " IN (") ] :: (items @ next))
                | Expr.Like(x, p, neg) ->
                    put "("

                    writeSteps (
                        sub x [ text (if neg then " NOT LIKE " else " LIKE ") ]
                        :: sub p [ text ")" ]
                        :: next
                    )
                | Expr.Case(branches, elseE) ->
                    put "(CASE "

                    let branchSteps =
                        branches
                        |> List.collect (fun (w, t) -> [ text "WHEN "; sub w [ text " THEN " ]; sub t [ text " " ] ])

                    let elseSteps =
                        match elseE with
                        | Some v -> [ text "ELSE "; sub v [ text " " ] ]
                        | None -> []

                    writeSteps ((branchSteps @ elseSteps @ [ text "END)" ]) @ next)
                | Expr.FuncCall(n, args) ->
                    put n
                    put "("
                    let n = List.length args

                    let items =
                        args |> List.mapi (fun i y -> sub y [ text (if i = n - 1 then ")" else ", ") ])

                    writeSteps (items @ next)
                | Expr.AggCall(f, distinct, x) ->
                    put f.Name
                    put "("

                    if distinct then
                        put "DISTINCT "

                    writeSteps (sub x [ text ")" ] :: next)

        writeSteps [ PrintStep.Sub(e, []) ]
        sb.ToString()

[<RequireQualifiedAccess>]
type internal JoinKind =
    | Inner
    | Left

type internal TableRef = { Name: string; Alias: string option }

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
