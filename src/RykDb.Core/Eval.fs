namespace RykDb.Core

/// 1オペレータ分の評価準備（不変）。
/// <remarks>
/// 演算子が評価する式から、列参照の解決とリテラル LIKE パターンのコンパイルを
/// **計画時に 1回だけ**行って保持する。行ごとの `Schema.resolve` と LIKE の
/// 再コンパイルを繰り返さないための前計算であり、評価中に変わる状態は無い。
/// `Schema` が変わるところ（JOIN の出力、集約後など）では新しい `Prepared` を作る。
/// 列パターン（`a LIKE b`）だけは評価のたびにコンパイルする（`Like` のフォールバック）。
/// </remarks>
[<NoEquality; NoComparison>]
type internal Prepared =
    private
        {
            Source: Schema
            Resolved:
                System.Collections.Immutable.ImmutableDictionary<struct (string option * string), Result<int, DbError>>
            Patterns:
                System.Collections.Immutable.ImmutableDictionary<
                    struct (int * string),
                    Result<Scalar.LikePattern, DbError>
                 >
        }

    /// 列参照を列位置へ解決する。
    /// 計画時に拾えなかった参照だけ `Schema.resolve` に落ちる。
    /// <remarks>
    /// `TryGetValue` は F# では `bool * 'T` のタプルを返すため行ごとに確保する。
    /// ホットパスなので `ContainsKey` + インデクサで引く（参照の割当なし）。
    /// </remarks>
    member this.Resolve(r: ColumnRef) =
        let key = struct (r.Qualifier, r.Name)

        if this.Resolved.ContainsKey key then
            this.Resolved.[key]
        else
            Schema.resolve this.Source r

    /// コンパイル済み LIKE パターンを返す。
    /// `escape` はコードポイント（`Scalar.NoEscape` = ESCAPE なし）。
    /// リテラル以外のパターンはここでコンパイルする。
    member this.Like (escape: int) (pattern: string) =
        let key = struct (escape, pattern)

        if this.Patterns.ContainsKey key then
            this.Patterns.[key]
        else
            Scalar.compileTokens (if escape = Scalar.NoEscape then None else Some escape) pattern

/// 評価対象の行。
/// JOIN の ON 句では左右の行を連結せずに参照する（構造体なのでヒープ割当なし）。
[<Struct>]
type private RowView =
    | Single of SqlValue array
    | Joined of SqlValue array * SqlValue array

    /// 相関サブクエリの runner へ渡す実際の行。
    /// JOIN の ON 句で相関サブクエリがある場合だけ連結する（通常は分配なし）。
    member this.ToArray() =
        match this with
        | Single row -> row
        | Joined(l, r) -> Array.append l r

/// スカラー式の評価器。
/// 集約を含まない式だけを扱う（集約は Eval に組めない）。
/// NULL の伝播と 3値論理は SQL 標準に合わせる。
module internal Eval =
    type private ArithOp =
        | Add
        | Sub
        | Mul
        | Div
        | Mod

    /// LIKE 照合。コンパイル失敗は偽（ESCAPE なしの単純一致と同じ振る舞い）。
    let private likeMatches (resolve: Prepared) (escape: int) (pattern: string) (text: string) =
        match resolve.Like escape pattern with
        | Ok compiled -> Scalar.matchLikeTokens compiled.Tokens (System.MemoryExtensions.AsSpan text)
        | Error _ -> false

    /// LIKE 照合。コンパイル失敗をエラーとして返す（ESCAPE 付き）。
    let private tryLikeMatches (resolve: Prepared) (escape: int) (pattern: string) (text: string) =
        resolve.Like escape pattern
        |> Result.map (fun compiled -> Scalar.matchLikeTokens compiled.Tokens (System.MemoryExtensions.AsSpan text))

    let private overflowError () =
        DbErr(DbError.constraintViolation "数値演算の結果が表現可能な範囲を超えました")

    /// 整数の四則演算。
    /// 例外も `BigInteger` も使わず、桁あふれを演算前に判定する
    /// （行ごとの評価で値ごとにオブジェクトを確保しないため）。
    let private intArith (a: int64) (b: int64) (op: ArithOp) =
        let addOverflows =
            b > 0L && a > System.Int64.MaxValue - b
            || b < 0L && a < System.Int64.MinValue - b

        let subOverflows =
            b > 0L && a < System.Int64.MinValue + b
            || b < 0L && a > System.Int64.MaxValue + b

        let mulOverflows =
            if a = 0L || b = 0L then
                false
            elif a > 0L then
                if b > 0L then
                    a > System.Int64.MaxValue / b
                else
                    b < System.Int64.MinValue / a
            elif b > 0L then
                a < System.Int64.MinValue / b
            else
                b < System.Int64.MaxValue / a

        match op with
        | Add ->
            if addOverflows then
                overflowError ()
            else
                DbOk(SqlValue.Int(a + b))
        | Sub ->
            if subOverflows then
                overflowError ()
            else
                DbOk(SqlValue.Int(a - b))
        | Mul ->
            if mulOverflows then
                overflowError ()
            else
                DbOk(SqlValue.Int(a * b))
        | Div ->
            if a = System.Int64.MinValue && b = -1L then
                overflowError ()
            else
                DbOk(SqlValue.Int(a / b))
        | Mod ->
            if b = -1L then
                DbOk(SqlValue.Int 0L)
            else
                DbOk(SqlValue.Int(a % b))

    let private decimalArith (a: decimal) (b: decimal) (op: ArithOp) =
        let maxD = System.Decimal.MaxValue
        let minD = System.Decimal.MinValue

        match op with
        | Add when a > 0M && b > 0M && a > maxD - b -> overflowError ()
        | Add when a < 0M && b < 0M && a < minD - b -> overflowError ()
        | Add -> DbOk(SqlValue.Decimal(a + b))
        | Sub when a < 0M && b > 0M && a < minD + b -> overflowError ()
        | Sub when a > 0M && b < 0M && a > maxD + b -> overflowError ()
        | Sub -> DbOk(SqlValue.Decimal(a - b))
        | Mul when a = 0M || b = 0M -> DbOk(SqlValue.Decimal 0M)
        | Mul ->
            let aa = abs a
            let bb = abs b
            let overflows = bb >= 1M && aa > maxD / bb

            if overflows then
                overflowError ()
            else
                DbOk(SqlValue.Decimal(a * b))
        | Div when abs b < 1M ->
            if abs a > maxD * abs b then
                overflowError ()
            else
                DbOk(SqlValue.Decimal(a / b))
        | Div -> DbOk(SqlValue.Decimal(a / b))
        | Mod -> DbOk(SqlValue.Decimal(a % b))

    let private realArith (a: float) (b: float) (op: ArithOp) =
        let r =
            match op with
            | Add -> a + b
            | Sub -> a - b
            | Mul -> a * b
            | Div -> a / b
            | Mod -> a % b

        DbOk(SqlValue.vReal r)

    let private decimalOf (v: SqlValue) =
        match v with
        | SqlValue.Int i -> decimal i
        | SqlValue.Decimal d -> d
        | _ -> 0M

    let private floatOf (v: SqlValue) =
        match v with
        | SqlValue.Int i -> float i
        | SqlValue.Decimal d -> float d
        | SqlValue.Real f -> f
        | _ -> 0.0

    let private isZero (v: SqlValue) =
        match v with
        | SqlValue.Int 0L
        | SqlValue.Decimal 0M
        | SqlValue.Real 0.0 -> true
        | _ -> false

    let private liftNumeric (op: ArithOp) (a: SqlValue) (b: SqlValue) =
        match a, b with
        | SqlValue.Int x, SqlValue.Int y -> intArith x y op
        | (SqlValue.Int _ | SqlValue.Decimal _), (SqlValue.Int _ | SqlValue.Decimal _) ->
            decimalArith (decimalOf a) (decimalOf b) op
        | (SqlValue.Int _ | SqlValue.Decimal _ | SqlValue.Real _),
          (SqlValue.Int _ | SqlValue.Decimal _ | SqlValue.Real _) -> realArith (floatOf a) (floatOf b) op
        | _ -> DbOk SqlValue.Null

    let private arith (op: ArithOp) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            DbOk SqlValue.Null
        elif (op = Div || op = Mod) && isZero b then
            DbErr(DbError.constraintViolation "0 での除算は定義されていません")
        else
            liftNumeric op a b

    /// 比較演算子を適用して 3値論理に変換する。
    /// <remarks>
    /// compare（SqlValue を返す文脈）と truthOf（TruthValue を返す文脈）は
    /// NULL 判定と演算子の表だけが違うので、両方ここへ寄せる。
    /// 演算子は文字列タグではなく `CompareOp` で受け、6 演算子を漏れなく分岐する
    /// （未知の演算子を黙って Unknown へ寄せる既定分岐は持たない）。
    /// </remarks>
    let private applyComparison (op: CompareOp) (c: int) =
        let tf b =
            if b then TruthValue.True else TruthValue.False

        match op with
        | CompareOp.Eq -> tf (c = 0)
        | CompareOp.Ne -> tf (c <> 0)
        | CompareOp.Lt -> tf (c < 0)
        | CompareOp.Le -> tf (c <= 0)
        | CompareOp.Gt -> tf (c > 0)
        | CompareOp.Ge -> tf (c >= 0)

    /// 比較を 3値論理に変換する。
    let private compare (op: CompareOp) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            DbOk SqlValue.Null
        else
            match SqlValue.compareValuesV a b with
            | DbErr e -> DbErr e
            | DbOk c -> DbOk(SqlValue.ofBool (applyComparison op c = TruthValue.True))

    /// WHERE 用に TruthValue で判定する（UNKNOWN は偽）。
    let private truthOf (op: CompareOp) (a: SqlValue) (b: SqlValue) =
        if SqlValue.isNull a || SqlValue.isNull b then
            DbOk TruthValue.Unknown
        else
            match SqlValue.compareValuesV a b with
            | DbErr e -> DbErr e
            | DbOk c -> DbOk(applyComparison op c)

    /// 2値（真偽を直接返す）の文脈用。
    /// NULL は False。
    let private boolOf (v: SqlValue) =
        match v with
        | SqlValue.Bool b -> b
        | _ -> false

    /// 行の範囲外アクセスを安全なエラーにする。
    let private valueAt (row: RowView) (i: int) (e: Expr) =
        let length =
            match row with
            | Single r -> r.Length
            | Joined(l, r) -> l.Length + r.Length

        if i < 0 || i >= length then
            sprintf "列インデックス %d が範囲外です（%d 列中）: %s" i length (Expr.toDisplayString e)
            |> DbError.internalError
            |> DbErr
        else
            match row with
            | Single r -> DbOk r.[i]
            | Joined(l, r) -> if i < l.Length then DbOk l.[i] else DbOk r.[i - l.Length]

    /// SqlValue を 3値に変換する。
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
        | TruthValue.True -> SqlValue.TrueValue
        | TruthValue.False -> SqlValue.FalseValue
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
        /// IS [NOT] TRUE / FALSE / UNKNOWN の判定。
        | KIsTruth of TruthValue * bool
        /// CAST の変換先。
        | KCast of DataType
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
        /// IN の集合に対する判定待ち（集合, 否定）。
        | KInSet of Membership * bool
        | KLikeX of Expr * bool * Expr option
        | KLikeP of SqlValue * bool * Expr option
        /// ESCAPE 式の評価中（左辺, パターン, 否定）。
        | KLikeE of SqlValue * SqlValue * bool
        /// CASE の WHEN を評価中（当該の WHEN/THEN, 残りの枝, ELSE）。
        | KCaseWhen of (Expr * Expr) * (Expr * Expr) list * Expr option
        /// 関数の引数 1個を評価中（関数名, 確定済みの逆順, 残りの引数）。
        | KFuncArg of string * SqlValue list * Expr list

    /// 評価機の状態。
    /// 式を 1つ降りるか（Descend）、値が上がってきたか（Arrive）。
    /// ヒープに載せない（呼び出しごとに新しい値を作るだけなので struct で足りる）。
    [<Struct>]
    type private Step =
        | Descend of expr: Expr
        | Arrive of value: SqlValue

    /// 式評価機。
    /// 降りる／上がるを継続スタック Kont でつなぐ（CPS）。
    /// 再帰はこの関数の自己再帰だけなので全部末尾位置になり、
    /// 式の深さ・IN の候補数に依存せずスタックは一定（継続はヒープのリストに積む）。
    /// `resolve` は計画時に作った評価準備（`Eval.prepare`）で、
    /// 列解決と LIKE のコンパイルを済ませてある。
    [<TailCall>]
    let rec private step (resolve: Prepared) (row: RowView) (kont: Kont list) (st: Step) =
        match st with
        | Descend e ->
            match e with
            | Expr.Literal l -> step resolve row kont (Arrive l.ToSqlValue)
            | Expr.Value v -> step resolve row kont (Arrive v)
            | Expr.Subquery _
            | Expr.Exists _
            | Expr.InSubquery _ -> DbErr(DbError.create ErrorKind.SyntaxError "サブクエリはここでは使えません")
            | Expr.CorrelatedSubquery c ->
                match c.Run(row.ToArray()) with
                | Error er -> DbErr er
                | Ok v -> step resolve row kont (Arrive v)
            | Expr.Column c ->
                match resolve.Resolve c with
                | Error er -> DbErr er
                | Ok i ->
                    match valueAt row i e with
                    | DbErr er -> DbErr er
                    | DbOk v -> step resolve row kont (Arrive v)
            | Expr.Name n ->
                let probeRef = { Qualifier = None; Name = n }

                match resolve.Resolve probeRef with
                | Error _ -> DbErr(sprintf "'%s' は列でも関数でもありません" n |> DbError.name)
                | Ok i ->
                    match valueAt row i e with
                    | DbErr er -> DbErr er
                    | DbOk v -> step resolve row kont (Arrive v)
            | Expr.Star -> DbErr(DbError.create ErrorKind.SyntaxError "WHERE 句に '*' は置けません")
            | Expr.QualifiedStar q -> DbErr(sprintf "'%s.*' はここでは使えません" q |> DbError.name)
            | Expr.Unary(op, x) -> step resolve row (KUnary op :: kont) (Descend x)
            | Expr.Binary(op, a, b) -> step resolve row (KBinaryL(op, b) :: kont) (Descend a)
            | Expr.IsNull(x, negated) -> step resolve row (KIsNull negated :: kont) (Descend x)
            | Expr.IsTruth(x, expected, negated) -> step resolve row (KIsTruth(expected, negated) :: kont) (Descend x)
            | Expr.Between(x, lo, hi, negated) -> step resolve row (KBetweenX(lo, hi, negated) :: kont) (Descend x)
            | Expr.InList(x, list, negated) -> step resolve row (KInX(list, negated) :: kont) (Descend x)
            | Expr.InSet(x, set, negated) -> step resolve row (KInSet(set, negated) :: kont) (Descend x)
            | Expr.Like(x, p, negated, esc) -> step resolve row (KLikeX(p, negated, esc) :: kont) (Descend x)
            | Expr.Case(branches, elseE) ->
                match branches with
                | (w, t) :: tail -> step resolve row (KCaseWhen((w, t), tail, elseE) :: kont) (Descend w)
                | [] ->
                    match elseE with
                    | Some rest -> step resolve row kont (Descend rest)
                    | None -> step resolve row kont (Arrive SqlValue.Null)
            | Expr.FuncCall(n, args) ->
                match args with
                | a :: tail -> step resolve row (KFuncArg(n, [], tail) :: kont) (Descend a)
                | [] ->
                    match Scalar.applyFunc n [||] with
                    | Error er -> DbErr er
                    | Ok v -> step resolve row kont (Arrive v)
            | Expr.Cast(x, dt) -> step resolve row (KCast dt :: kont) (Descend x)
            | Expr.Default -> DbErr(DbError.create ErrorKind.SyntaxError "DEFAULT は INSERT の VALUES でのみ使えます")
            | Expr.AggCall(f, _, _) -> DbErr(sprintf "%s は集約なのでここでは評価できません" f.Name |> DbError.name)
        | Arrive v ->
            match kont with
            | [] -> DbOk v
            | KUnary op :: rest ->
                let computed =
                    match op with
                    | UnaryOp.Not ->
                        DbOk(
                            if SqlValue.isNull v then
                                SqlValue.Null
                            else
                                SqlValue.ofBool (not (boolOf v))
                        )
                    | UnaryOp.Pos -> DbOk v
                    | UnaryOp.Neg ->
                        if SqlValue.isNull v then
                            DbOk SqlValue.Null
                        else
                            match v with
                            | SqlValue.Int i when i = System.Int64.MinValue ->
                                DbErr(DbError.constraintViolation "符号反転の結果が範囲を超えました")
                            | SqlValue.Int i -> DbOk(SqlValue.Int -i)
                            | SqlValue.Real f -> DbOk(SqlValue.vReal -f)
                            | SqlValue.Decimal d -> DbOk(SqlValue.Decimal -d)
                            | _ -> DbErr(sprintf "'-' は数値に適用できません: %s" (SqlLiteral.toSqlLiteral v) |> DbError.typeError)

                match computed with
                | DbErr er -> DbErr er
                | DbOk res -> step resolve row rest (Arrive res)
            | KBinaryL(op, b) :: rest ->
                let short =
                    match op with
                    | BinaryOp.And when v = SqlValue.FalseValue -> Some SqlValue.FalseValue
                    | BinaryOp.Or when v = SqlValue.TrueValue -> Some SqlValue.TrueValue
                    | _ -> None

                match short with
                | Some res -> step resolve row rest (Arrive res)
                | None -> step resolve row (KBinaryR(op, v) :: rest) (Descend b)
            | KBinaryR(op, l) :: rest ->
                let combined =
                    match op with
                    | BinaryOp.Add -> arith Add l v
                    | BinaryOp.Sub -> arith Sub l v
                    | BinaryOp.Mul -> arith Mul l v
                    | BinaryOp.Div -> arith Div l v
                    | BinaryOp.Mod -> arith Mod l v
                    | BinaryOp.Concat ->
                        if SqlValue.isNull l || SqlValue.isNull v then
                            DbOk SqlValue.Null
                        else
                            DbOk(SqlValue.Text(SqlValue.toDisplayString l + SqlValue.toDisplayString v))
                    | BinaryOp.Like ->
                        if SqlValue.isNull l || SqlValue.isNull v then
                            DbOk SqlValue.Null
                        else
                            DbOk(
                                SqlValue.ofBool (
                                    likeMatches
                                        resolve
                                        Scalar.NoEscape
                                        (SqlValue.toDisplayString v)
                                        (SqlValue.toDisplayString l)
                                )
                            )
                    | BinaryOp.Eq -> compare CompareOp.Eq l v
                    | BinaryOp.Ne -> compare CompareOp.Ne l v
                    | BinaryOp.Lt -> compare CompareOp.Lt l v
                    | BinaryOp.Le -> compare CompareOp.Le l v
                    | BinaryOp.Gt -> compare CompareOp.Gt l v
                    | BinaryOp.Ge -> compare CompareOp.Ge l v
                    | BinaryOp.And -> DbOk(andValue l v)
                    | BinaryOp.Or -> DbOk(orValue l v)

                match combined with
                | DbErr er -> DbErr er
                | DbOk res -> step resolve row rest (Arrive res)
            | KIsNull negated :: rest ->
                let isNull = SqlValue.isNull v
                step resolve row rest (Arrive(SqlValue.ofBool (if negated then not isNull else isNull)))
            | KIsTruth(expected, negated) :: rest ->
                let result = truthOfValue v = expected
                step resolve row rest (Arrive(SqlValue.ofBool (if negated then not result else result)))
            | KCast dt :: rest ->
                match SqlValue.cast dt v with
                | Error er -> DbErr er
                | Ok res -> step resolve row rest (Arrive res)
            | KBetweenX(lo, hi, negated) :: rest -> step resolve row (KBetweenLo(v, hi, negated) :: rest) (Descend lo)
            | KBetweenLo(x, hi, negated) :: rest -> step resolve row (KBetweenHi(x, v, negated) :: rest) (Descend hi)
            | KBetweenHi(x, lov, negated) :: rest ->
                match truthOf CompareOp.Ge x lov with
                | DbErr er -> DbErr er
                | DbOk ge ->
                    match truthOf CompareOp.Le x v with
                    | DbErr er -> DbErr er
                    | DbOk le ->
                        match TruthValue.and2 ge le with
                        | TruthValue.True -> step resolve row rest (Arrive(SqlValue.ofBool (not negated)))
                        | TruthValue.False -> step resolve row rest (Arrive(SqlValue.ofBool negated))
                        | TruthValue.Unknown -> step resolve row rest (Arrive SqlValue.Null)
            | KInX(list, negated) :: rest ->
                match list with
                | [] -> step resolve row rest (Arrive(SqlValue.ofBool negated))
                | cand :: tail -> step resolve row (KInArg(v, negated, tail, false) :: rest) (Descend cand)
            | KInArg(x, negated, tail, sawUnknown) :: rest ->
                match truthOf CompareOp.Eq x v with
                | DbErr er -> DbErr er
                | DbOk TruthValue.True -> step resolve row rest (Arrive(SqlValue.ofBool (not negated)))
                | DbOk tv ->
                    let saw = sawUnknown || tv = TruthValue.Unknown

                    match tail with
                    | [] -> step resolve row rest (Arrive(if saw then SqlValue.Null else SqlValue.ofBool negated))
                    | cand :: more -> step resolve row (KInArg(x, negated, more, saw) :: rest) (Descend cand)
            | KInSet(set, negated) :: rest ->
                match set.Contains v with
                | DbErr er -> DbErr er
                | DbOk TruthValue.True -> step resolve row rest (Arrive(SqlValue.ofBool (not negated)))
                | DbOk TruthValue.False -> step resolve row rest (Arrive(SqlValue.ofBool negated))
                | DbOk TruthValue.Unknown -> step resolve row rest (Arrive SqlValue.Null)
            | KLikeX(p, negated, esc) :: rest -> step resolve row (KLikeP(v, negated, esc) :: rest) (Descend p)
            | KLikeP(x, negated, esc) :: rest ->
                if SqlValue.isNull x || SqlValue.isNull v then
                    step resolve row rest (Arrive SqlValue.Null)
                else
                    match esc with
                    | None ->
                        let m =
                            likeMatches
                                resolve
                                Scalar.NoEscape
                                (SqlValue.toDisplayString v)
                                (SqlValue.toDisplayString x)

                        step resolve row rest (Arrive(SqlValue.ofBool (if negated then not m else m)))
                    | Some escExpr -> step resolve row (KLikeE(x, v, negated) :: rest) (Descend escExpr)
            | KLikeE(x, pattern, negated) :: rest ->
                if SqlValue.isNull v then
                    step resolve row rest (Arrive SqlValue.Null)
                else
                    match Scalar.escapeCodePoint (SqlValue.toDisplayString v) with
                    | Error er -> DbErr er
                    | Ok escape ->
                        match
                            tryLikeMatches
                                resolve
                                escape
                                (SqlValue.toDisplayString pattern)
                                (SqlValue.toDisplayString x)
                        with
                        | Error er -> DbErr er
                        | Ok m -> step resolve row rest (Arrive(SqlValue.ofBool (if negated then not m else m)))
            | KCaseWhen((_, t), tail, elseE) :: rest ->
                let matched = not (SqlValue.isNull v) && boolOf v

                if matched then
                    step resolve row rest (Descend t)
                else
                    match tail with
                    | (w2, t2) :: more -> step resolve row (KCaseWhen((w2, t2), more, elseE) :: rest) (Descend w2)
                    | [] ->
                        match elseE with
                        | Some rest2 -> step resolve row rest (Descend rest2)
                        | None -> step resolve row rest (Arrive SqlValue.Null)
            | KFuncArg(n, doneRev, tail) :: rest ->
                let acc = v :: doneRev

                match tail with
                | [] ->
                    match Scalar.applyFunc n (List.rev acc |> List.toArray) with
                    | Error er -> DbErr er
                    | Ok res -> step resolve row rest (Arrive res)
                | a :: more -> step resolve row (KFuncArg(n, acc, more) :: rest) (Descend a)

    /// 演算子が評価する式から、列解決とリテラル LIKE のコンパイルを 1回で済ませる。
    /// 可変のキャッシュではなく、返した値は評価中に変化しない。
    /// 列パターン（`a LIKE b`）は計画時に決まらないので、ここではコンパイルしない。
    let prepare (schema: Schema) (exprs: Expr list) =
        let emptyColumns
            : System.Collections.Immutable.ImmutableDictionary<struct (string option * string), Result<int, DbError>> =
            System.Collections.Immutable.ImmutableDictionary.Create()

        let addColumn
            (acc:
                System.Collections.Immutable.ImmutableDictionary<struct (string option * string), Result<int, DbError>>)
            (c: ColumnRef)
            =
            let key = struct (c.Qualifier, c.Name)

            if acc.ContainsKey key then
                acc
            else
                acc.Add(key, Schema.resolve schema c)

        let columns =
            (emptyColumns, exprs)
            ||> List.fold (fun acc e ->
                Expr.fold
                    (fun acc node ->
                        match node with
                        | Expr.Column c -> addColumn acc c
                        | _ -> acc)
                    acc
                    e)

        let emptyLikes
            : System.Collections.Immutable.ImmutableDictionary<
                struct (int * string),
                Result<Scalar.LikePattern, DbError>
               > =
            System.Collections.Immutable.ImmutableDictionary.Create()

        let addLike
            (acc:
                System.Collections.Immutable.ImmutableDictionary<
                    struct (int * string),
                    Result<Scalar.LikePattern, DbError>
                 >)
            (escape: Expr option)
            (pattern: Expr)
            =
            match pattern with
            | Expr.Literal(Literal.String p) ->
                let code =
                    match escape with
                    | None -> Some Scalar.NoEscape
                    | Some(Expr.Literal(Literal.String s)) ->
                        match Scalar.escapeCodePoint s with
                        | Ok cp -> Some cp
                        | Error _ -> None
                    | Some _ -> None

                match code with
                | Some code ->
                    let key = struct (code, p)

                    if acc.ContainsKey key then
                        acc
                    else
                        let compiled =
                            Scalar.compileTokens (if code = Scalar.NoEscape then None else Some code) p

                        acc.Add(key, compiled)
                | None -> acc
            | _ -> acc

        let likes =
            (emptyLikes, exprs)
            ||> List.fold (fun acc e ->
                Expr.fold
                    (fun acc node ->
                        match node with
                        | Expr.Like(_, p, _, esc) -> addLike acc esc p
                        | Expr.Binary(BinaryOp.Like, _, p) -> addLike acc None p
                        | _ -> acc)
                    acc
                    e)

        {
            Source = schema
            Resolved = columns
            Patterns = likes
        }

    /// 計画時に作った評価準備で式を評価する（内部ホットパス用）。
    /// 集約呼び出しはここでエラーになる。
    /// 評価機 step を空の継続スタックで始める（末尾再帰のまま深さ・候補数に依存しない）。
    let evalWithV (resolve: Prepared) (row: SqlValue array) (e: Expr) : DbResult<SqlValue> =
        step resolve (Single row) [] (Descend e)

    /// 評価準備で式を評価する（公開境界用）。
    let evalWith (resolve: Prepared) (row: SqlValue array) (e: Expr) =
        evalWithV resolve row e |> DbResult.toResult

    /// 式を評価する（評価準備を都度作る簡易版）。
    /// 行ごとに呼ぶホットパスでは `prepare` を 1回だけ作って `evalWithV` に渡すこと。
    let eval (schema: Schema) = prepare schema [] |> evalWith

    /// 評価結果を WHERE の真偽へ変換する（UNKNOWN は偽）。
    let private asMatchV (r: DbResult<SqlValue>) : DbResult<bool> =
        match r with
        | DbErr e -> DbErr e
        | DbOk v ->
            DbOk(
                match v with
                | SqlValue.Bool b -> b
                | SqlValue.Null -> false
                | SqlValue.Int i -> i <> 0L
                | _ -> false
            )

    /// WHERE の判定（内部ホットパス用）。
    /// UNKNOWN は偽（SQL 標準）。
    let matchesWithV (resolve: Prepared) (row: SqlValue array) (predicate: Expr) =
        evalWithV resolve row predicate |> asMatchV

    /// JOIN の ON 句（および residual）の判定（内部ホットパス用）。
    /// 左右の行を連結せずに、左の列の後ろに右の列が並んでいるものとして評価する。
    /// resolve は `Schema.concat leftSchema rightSchema` と ON 句の式から作る。
    let matchesJoinedV (resolve: Prepared) (left: SqlValue array) (right: SqlValue array) (predicate: Expr) =
        step resolve (Joined(left, right)) [] (Descend predicate) |> asMatchV
