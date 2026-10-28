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
    let ofValue =
        function
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

module internal Expr =
    /// 直接の子式をリストで返す（走査順は左から右）。
    /// 再帰は呼び出し側が行う。
    let private children =
        function
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

    /// 書き換え待ちの親ノード。
    /// 子を 1つずつ加工してから `plugChildren` で組み立てる。
    /// <remarks>
    /// 継続はラムダではなく値として持つ（`Eval.Kont` / `ExecFrame` と同じ方針）。
    /// `Parent` は子が加工される前の式、`Done` は加工済みの子（`children` の逆順）。
    /// `Rest` が尽きたら `Done` を `plugChildren` に渡して親を組み直す。
    /// </remarks>
    type private RewriteFrame =
        {
            /// 組み立ての雛形（子はまだ加工前）
            Parent: Expr
            /// 加工済みの子（`children` の逆順）
            Done: Expr list
            /// まだ加工していない子（先頭が次の訪問先）
            Rest: Expr list
        }

    /// 書き換え機の状態。
    /// 式を 1つ降りるか（`Visit`）、加工済みの式が上がってきたか（`Arrive`）。
    type private RewriteStep =
        | Visit of Expr
        | Arrive of Expr

    /// 葉の書き換え関数で式を組み直す（末尾再帰。式の深さに依存せずスタックは一定）。
    let rewrite (rewriteLeaf: Expr -> Expr) (root: Expr) =
        let miss kids =
            err (
                sprintf "plugChildren: 子の個数が合いません（%d 個）" (List.length kids)
                |> DbError.internalError
            )

        let rec toPairs acc =
            function
            | w :: t :: tail -> toPairs ((w, t) :: acc) tail
            | _ -> List.rev acc

        let plugChildren kids e =
            if List.length kids <> List.length (children e) then
                miss kids
            else
                match e, kids with
                | Expr.Literal _, [] -> ret e
                | Expr.Column _, [] -> ret e
                | Expr.Name _, [] -> ret e
                | Expr.Star, [] -> ret e
                | Expr.QualifiedStar _, [] -> ret e
                | Expr.Unary(op, _), [ x ] -> ret (Expr.Unary(op, x))
                | Expr.Binary(op, _, _), [ a; b ] -> ret (Expr.Binary(op, a, b))
                | Expr.IsNull(_, neg), [ x ] -> ret (Expr.IsNull(x, neg))
                | Expr.Between(_, _, _, neg), [ x; lo; hi ] -> ret (Expr.Between(x, lo, hi, neg))
                | Expr.InList(_, _, neg), x :: xs -> ret (Expr.InList(x, xs, neg))
                | Expr.Like(_, _, neg), [ x; p ] -> ret (Expr.Like(x, p, neg))
                | Expr.Case(_, elseE), _ ->
                    match elseE, List.rev kids with
                    | None, _ -> ret (Expr.Case(toPairs [] kids, None))
                    | Some _, last :: branchRev -> ret (Expr.Case(List.rev branchRev |> toPairs [], Some last))
                    | Some _, [] -> miss kids
                | Expr.FuncCall(n, _), xs -> ret (Expr.FuncCall(n, xs))
                | Expr.AggCall(f, star, _), [ x ] -> ret (Expr.AggCall(f, star, x))
                | _ -> miss kids

        let rec rewriteLoop frames =
            function
            | Visit x ->
                let x2 = rewriteLeaf x

                match children x2 with
                | [] -> Arrive x2 |> rewriteLoop frames
                | c :: rest ->
                    let frame = { Parent = x2; Done = []; Rest = rest }
                    Visit c |> rewriteLoop (frame :: frames)
            | Arrive v ->
                match frames with
                | [] -> ret v
                | frame :: more ->
                    match frame.Rest with
                    | c :: rest ->
                        let next =
                            { frame with
                                Done = v :: frame.Done
                                Rest = rest
                            }

                        Visit c |> rewriteLoop (next :: more)
                    | [] ->
                        match plugChildren (v :: frame.Done |> List.rev) frame.Parent with
                        | Error er -> Error er
                        | Ok parent -> Arrive parent |> rewriteLoop more

        Visit root |> rewriteLoop []

    /// 式木を明示スタックで pre-order にたどり、各ノードについて状態を畳み込む。
    /// 走査順は「根 → 子を左から右へ」。
    let fold (f: 's -> Expr -> 's) (state: 's) (root: Expr) =
        let rec revInto acc =
            function
            | [] -> acc
            | x :: rest -> revInto (x :: acc) rest

        let rec foldStack acc =
            function
            | [] -> acc
            | e :: rest ->
                let acc2 = f acc e
                children e |> List.rev |> revInto rest |> foldStack acc2

        foldStack state [ root ]

    /// 式中のすべての列参照を列挙する。
    /// 束縛と空き列判定に使う。
    /// 末尾再帰の明示スタック走査（ColumnRefs の既存の挙動を維持）。
    let columnRefs =
        fold
            (fun acc e ->
                match e with
                | Expr.Column c -> c :: acc
                | _ -> acc)
            []
        >> List.rev

    /// 式に集計呼び出しが含まれるか。
    /// GROUP BY の妥当性判定に使う。
    /// 短絡評価の早期打ち切りは失われるが、結果は論理 OR で同じ。
    let isAggregate =
        fold
            (fun found e ->
                found
                || match e with
                   | Expr.AggCall _ -> true
                   | _ -> false)
            false

    /// Expr.toDisplayString が使う出力項目。
    /// Sub の closer を先に与えておくことで、式の出力が終わってから
    /// 閉じ括弧などを出力する順序を、末尾再帰のまま表現できる。
    type private PrintStep =
        /// そのまま出力する文字列
        | Text of string
        /// 式と、その後に出力する項目
        | Sub of Expr * PrintStep list

    /// 式をテキストへ戻す（EXPLAIN やエラーメッセージ用）。
    /// 「式」と「その式を出したあとに吐く文字列」を 1本のスタックで表現し、末尾再帰で処理する。
    /// writeSteps x のあとに put ")" と書くと再帰呼び出しが末尾位置にならず、
    /// 深い式（1万段超）でスタックを食うため。
    let toDisplayString (e: Expr) =
        let sb = System.Text.StringBuilder()
        let put (s: string) = sb.Append s |> ignore
        let text s = Text s
        let sub x closer = Sub(x, closer)

        let rec writeSteps =
            function
            | [] -> ()
            | Text s :: rest ->
                put s
                writeSteps rest
            | Sub(e, closer) :: rest ->
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
                    sub x [ text ")" ] :: next |> writeSteps
                | Expr.Binary(op, a, b) ->
                    put "("

                    sub a [ text " "; text op.Display; text " " ] :: sub b [ text ")" ] :: next
                    |> writeSteps
                | Expr.IsNull(x, neg) ->
                    put "("

                    sub x [ text (if neg then " IS NOT NULL)" else " IS NULL)") ] :: next
                    |> writeSteps
                | Expr.Between(x, a, b, neg) ->
                    put "("

                    sub x [ text (if neg then " NOT BETWEEN " else " BETWEEN ") ]
                    :: sub a [ text " AND " ]
                    :: sub b [ text ")" ]
                    :: next
                    |> writeSteps
                | Expr.InList(x, xs, neg) ->
                    put "("
                    let n = List.length xs

                    let items =
                        xs |> List.mapi (fun i y -> sub y [ text (if i = n - 1 then "))" else ", ") ])

                    sub x [ text (if neg then " NOT IN (" else " IN (") ] :: (items @ next)
                    |> writeSteps
                | Expr.Like(x, p, neg) ->
                    put "("

                    sub x [ text (if neg then " NOT LIKE " else " LIKE ") ]
                    :: sub p [ text ")" ]
                    :: next
                    |> writeSteps
                | Expr.Case(branches, elseE) ->
                    put "(CASE "

                    let branchSteps =
                        branches
                        |> List.collect (fun (w, t) -> [ text "WHEN "; sub w [ text " THEN " ]; sub t [ text " " ] ])

                    let elseSteps =
                        match elseE with
                        | Some v -> [ text "ELSE "; sub v [ text " " ] ]
                        | None -> []

                    (branchSteps @ elseSteps @ [ text "END)" ]) @ next |> writeSteps
                | Expr.FuncCall(n, args) ->
                    put n
                    put "("
                    let n' = List.length args

                    let items =
                        args |> List.mapi (fun i y -> sub y [ text (if i = n' - 1 then ")" else ", ") ])

                    items @ next |> writeSteps
                | Expr.AggCall(f, distinct, x) ->
                    put f.Name
                    put "("

                    if distinct then
                        put "DISTINCT "

                    sub x [ text ")" ] :: next |> writeSteps

        writeSteps [ Sub(e, []) ]
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
        /// 宣言された型引数（`VARCHAR(32)` の 32 など）。
        /// RykDb は長さ制限に使わないため、カタログには保存しない（再オープン後は空）。
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
