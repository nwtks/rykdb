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

    /// `children` が返す順・個数で子式を差し替えて式を組み立て直す（`mapChildren` の組み立て側）。
    /// <remarks>
    /// `children` の逆変換で、順序・個数が一致するのは呼び出し側の不変条件。
    /// 合わないのは到達不能なので `internalError` にする（例外は使わない）。
    /// 自分で子を辿らないので深い式でもスタックを食わず、
    /// 調用側（`Expr.rewrite` の評価機）が子を 1つずつ加工して最後にここへ渡す。
    /// </remarks>
    let plugChildren (e: Expr) (kids: Expr list) =
        let miss () =
            err (
                sprintf "plugChildren: 子の個数が合いません（%d 個）" (List.length kids)
                |> DbError.internalError
            )

        if List.length kids <> List.length (children e) then
            miss ()
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
                let rec toPairs (rest: Expr list) (acc: (Expr * Expr) list) =
                    match rest with
                    | w :: t :: tail -> toPairs tail ((w, t) :: acc)
                    | _ -> List.rev acc

                match elseE, List.rev kids with
                | None, _ -> ret (Expr.Case(toPairs kids [], None))
                | Some _, last :: branchRev -> ret (Expr.Case(toPairs (List.rev branchRev) [], Some last))
                | Some _, [] -> miss ()
            | Expr.FuncCall(n, _), xs -> ret (Expr.FuncCall(n, xs))
            | Expr.AggCall(f, star, _), [ x ] -> ret (Expr.AggCall(f, star, x))
            | _ -> miss ()

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

    /// 式を 1ノードずつ降り、上がってきた子で親を組み立てる書き換え機。
    /// <remarks>
    /// 葉の判定は呼び出し側が作った `rewriteLeaf` に任せる。
    /// `rewriteLeaf` の返した式に子が無ければそこで確定し、有れば子を 1つずつ降る
    /// （`AggCall` のように「子を持つが葉として扱う」ノードもこれで表現できる）。
    /// 再帰はこの関数の自己再帰だけ（全部末尾位置）なので、
    /// 式の深さに依存せずスタックは一定（フレームはヒープの list に積む）。
    /// </remarks>
    [<TailCall>]
    let rec private rewriteLoop (rewriteLeaf: Expr -> Expr) (frames: RewriteFrame list) (st: RewriteStep) =
        match st with
        | Visit x ->
            let x2 = rewriteLeaf x

            match children x2 with
            | [] -> rewriteLoop rewriteLeaf frames (Arrive x2)
            | c :: rest ->
                let frame = { Parent = x2; Done = []; Rest = rest }
                rewriteLoop rewriteLeaf (frame :: frames) (Visit c)
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

                    rewriteLoop rewriteLeaf (next :: more) (Visit c)
                | [] ->
                    match plugChildren frame.Parent (List.rev (v :: frame.Done)) with
                    | Error er -> Error er
                    | Ok parent -> rewriteLoop rewriteLeaf more (Arrive parent)

    /// 葉の書き換え関数で式を組み直す（末尾再帰。式の深さに依存せずスタックは一定）。
    let rewrite (rewriteLeaf: Expr -> Expr) (root: Expr) = rewriteLoop rewriteLeaf [] (Visit root)

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
