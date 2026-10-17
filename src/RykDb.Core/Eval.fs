namespace RykDb.Core

module internal Scalar =
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

    /// スカラー関数。
    /// 未知の関数はエラー。
    let applyFunc (name: string) (args: SqlValue array) =
        let upper = name.ToUpperInvariant()

        let arity (n: int) =
            if args.Length = n then
                ret ()
            else
                err (sprintf "%s 関数は %d 個の引数が必要ですが、%d 個渡されました" upper n args.Length |> DbError.name)

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
                        err (
                            sprintf "ABS は数値を受け付けますが %s です" (SqlLiteral.toSqlLiteral other)
                            |> DbError.typeError
                        )
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
                | other ->
                    err (
                        sprintf "ROUND は数値を受け付けますが %s です" (SqlLiteral.toSqlLiteral other)
                        |> DbError.typeError
                    )
        | _ -> err (sprintf "未知のスカラー関数 '%s' です" name |> DbError.name)

/// スカラー式の評価器。
/// 集約を含まない式だけを扱う（集約は Eval に組めない）。
/// NULL の伝播と 3値論理は SQL 標準に合わせる。
module internal Eval =
    let private arith (isDivision: bool) (op: decimal -> decimal -> decimal) (a: SqlValue) (b: SqlValue) =
        let arithmeticType (a: SqlValue) (b: SqlValue) =
            match a, b with
            | (SqlValue.Int _ | SqlValue.Null), (SqlValue.Int _ | SqlValue.Null) -> DataType.Integer
            | (SqlValue.Decimal _ | SqlValue.Null), (SqlValue.Decimal _ | SqlValue.Null) -> DataType.Decimal
            | _, _ -> DataType.Real

        let liftNumeric (a: SqlValue) (b: SqlValue) (op: decimal -> decimal -> decimal) =
            match SqlValue.asNumeric a, SqlValue.asNumeric b with
            | Some x, Some y ->
                match arithmeticType a b with
                | DataType.Integer -> SqlValue.Int(op x y |> int64)
                | DataType.Decimal -> SqlValue.Decimal(op x y)
                | _ -> SqlValue.Real(double (op x y))
            | _ -> SqlValue.Null

        if SqlValue.isNull a || SqlValue.isNull b then
            ret SqlValue.Null
        elif isDivision && SqlValue.asNumeric b = Some 0M then
            err (DbError.constraintViolation "0 での除算は定義されていません")
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

    /// 2値（真偽を直接返す）の文脈用。
    /// NULL は False。
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
            err (
                sprintf "列インデックス %d が範囲外です（%d 列中）: %s" i row.Length (Expr.toDisplayString e)
                |> DbError.internalError
            )
        else
            ret row.[i]

    /// SqlValue を 3値へ落とす。
    /// BOOL はそのまま、NULL と BOOL 以外は UNKNOWN。
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

    /// AND の 3値結合（両辺は既に評価済み）。
    /// FALSE での短絡は KBinaryL 側で先に済む。
    let private andValue (l: SqlValue) (r: SqlValue) =
        ofTruth (TruthValue.and2 (truthOfValue l) (truthOfValue r))

    /// OR の 3値結合（両辺は既に評価済み）。
    /// TRUE での短絡は KBinaryL 側で先に済む。
    let private orValue (l: SqlValue) (r: SqlValue) =
        ofTruth (TruthValue.or2 (truthOfValue l) (truthOfValue r))

    /// 継続（評価待ちの処理）。
    /// CPS の継続はクロージャではなく値として持つ。
    /// 再帰を 1つの関数 step の自己再帰にまとめれば末尾呼び出しで回り、
    /// ラムダの継続（継続の数だけフレームが積もる）や相互再帰（1フレーム残る）の問題がない。
    type private Kont =
        /// 単項演算子。
        /// 値が上がってから適用する。
        | KUnary of UnaryOp
        /// 二項演算子の左辺を評価中。
        /// 上がってから右辺へ進む。
        | KBinaryL of BinaryOp * Expr
        /// 二項演算子の右辺を評価中。
        /// 上がってから両辺を結合する。
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

    /// 評価機の状態。
    /// 式を 1つ降りるか（Descend）、値が上がってきたか（Arrive）。
    type private Step =
        | Descend of Expr
        | Arrive of SqlValue

    /// 式評価機。
    /// 降りる／上がるを継続スタック Kont でつなぐ（CPS）。
    /// 再帰はこの関数の自己再帰だけなので全部末尾位置になり、
    /// 式の深さ・IN の候補数に依存せずスタックは一定（継続はヒープのリストに積む）。
    [<TailCall>]
    let rec private step (schema: Schema) (row: SqlValue array) (kont: Kont list) (st: Step) =
        match st with
        | Descend e ->
            match e with
            | Expr.Literal l -> step schema row kont (Arrive l.ToSqlValue)
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
                | Error _ -> err (sprintf "'%s' は列でも関数でもありません" n |> DbError.name)
                | Ok i ->
                    match valueAt row i (Expr.Column probeRef) with
                    | Error er -> Error er
                    | Ok v -> step schema row kont (Arrive v)
            | Expr.Star -> err (DbError.create ErrorKind.SyntaxError "WHERE 句に '*' は置けません")
            | Expr.QualifiedStar q -> err (sprintf "'%s.*' はここでは使えません" q |> DbError.name)
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
            | Expr.AggCall(f, _, _) -> err (sprintf "%s は集約なのでここでは評価できません" f.Name |> DbError.name)
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
                            | _ -> err (sprintf "'-' は数値に適用できません: %s" (SqlLiteral.toSqlLiteral v) |> DbError.typeError)

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

    /// 式を評価する。
    /// 集約呼び出しはここでエラーになる。
    /// 評価機 step を空の継続スタックで始める（末尾再帰のまま深さ・候補数に依存しない）。
    let eval (schema: Schema) (row: SqlValue array) (e: Expr) = step schema row [] (Descend e)

    /// WHERE の判定。
    /// UNKNOWN は偽（SQL 標準）。
    let matches (schema: Schema) (row: SqlValue array) (predicate: Expr) =
        eval schema row predicate
        |> Result.map (fun v ->
            match v with
            | SqlValue.Bool b -> b
            | SqlValue.Null -> false
            | SqlValue.Int i -> i <> 0L
            | _ -> false)
