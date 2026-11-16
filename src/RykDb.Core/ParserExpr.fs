// ===========================================================================
// ParserExpr — 字句の読み進め・式（Pratt + CPS）・SELECT の構文規則。
// 文（DML / DDL / トランザクション）は `Parser.fs`。
// ===========================================================================

namespace RykDb.Core

/// パーサの状態。
/// 読み取り位置は値が持つので mutable は使わない。
/// すべての構文規則は (値, 次の状態) を返す純粋な関数になる。
/// `Parameters` は `?` へ出現順に割り当てる値。
type internal PState =
    {
        Tokens: TokenItem array
        Pos: int
        Parameters: SqlValue array
    }

/// 式と SELECT のパーサ（文は `Parser`）。
/// 失敗はすべて Result<'T, DbError> として返し、例外は送らない。
/// 式と SELECT の構文規則は CPS（継続を引数で渡す）で書き、継続は必ず末尾位置で呼ぶ。
/// これにより式・サブクエリの入れ子段数を入力で決めてもスタックオーバーフローしない
/// （深さ上限は設けない）。
module internal ParserExpr =
    /// 型名の綴り → DataType の表。綴りを増やすときはここだけ直す
    /// （`dataTypeOfName` と `parseDataType` の両方がこの表から導出される）。
    let private TypeTable =
        [
            "INTEGER", DataType.Integer
            "INT", DataType.Integer
            "BIGINT", DataType.Integer
            "REAL", DataType.Real
            "DOUBLE", DataType.Real
            "TEXT", DataType.Text
            "VARCHAR", DataType.Text
            "BLOB", DataType.Blob
            "BOOLEAN", DataType.Boolean
            "BOOL", DataType.Boolean
            "DATE", DataType.Date
            "TIMESTAMP", DataType.Timestamp
            "DATETIME", DataType.Timestamp
            "DECIMAL", DataType.Decimal
            "NUMERIC", DataType.Decimal
            "UUID", DataType.Uuid
        ]

    /// 綴り（大文字）から DataType へ。
    /// 未知なら None。
    let private dataTypeOfName (name: string) =
        TypeTable |> List.tryFind (fun (n, _) -> n = name) |> Option.map snd

    let internal peek (st: PState) = st.Tokens.[st.Pos].Token

    let private peekPos (st: PState) = st.Tokens.[st.Pos].Pos

    /// 1トークン消費した次の状態を返す。
    /// 末尾なら動かない。
    let internal advance (st: PState) =
        if st.Pos < st.Tokens.Length - 1 then
            { st with Pos = st.Pos + 1 }
        else
            st

    /// 現在位置を添えた構文エラーを返す。
    /// 送出はしない。
    let internal fail (msg: string) (st: PState) =
        err (DbError.syntax msg (peekPos st).Line)

    let internal isSymbol (s: string) (st: PState) =
        match peek st with
        | Token.Symbol x -> x = s
        | _ -> false

    let internal isKeyword (k: string) (st: PState) =
        match peek st with
        | Token.Keyword x -> x = k
        | _ -> false

    /// 未対応の集合演算。
    /// `UNION` は `UNION ALL` だけ受理する
    /// （`UNION` 単独・`INTERSECT` / `EXCEPT` は明示エラー）。
    /// これらは予約語ではないため、テーブル別名として飲み込むと
    /// 後続の SELECT が別の文として実行されてしまう。
    let internal SetOperationWords = Set.ofList [ "UNION"; "INTERSECT"; "EXCEPT" ]

    /// 未対応の JOIN 系の語。
    /// テーブル別名として飲み込むと、RIGHT JOIN などが INNER JOIN として静かに実行される。
    let private UnsupportedJoinWords =
        Set.ofList [ "RIGHT"; "FULL"; "CROSS"; "NATURAL"; "OUTER" ]

    /// 裸の別名として飲み込んではいけない語。
    /// 集合演算の語はここでは消費せず、後続の構文規則（集合演算・文の境界）に判定を委ねる。
    let private ReservedAliasWords = Set.union SetOperationWords UnsupportedJoinWords

    /// 現在位置が指定した識別子（大文字小文字を無視）か。
    let internal isIdentName (name: string) (st: PState) =
        match peek st with
        | Token.Ident s -> s.ToUpperInvariant() = name
        | _ -> false

    /// 指定した識別子があれば消費する（無くてもエラーにしない）。
    let internal eatIdentName (name: string) (st: PState) =
        if isIdentName name st then advance st else st

    /// 未対応語をテーブル別名として飲み込まないためのメッセージ。
    let private unsupportedAliasMessage (word: string) =
        match word with
        | "UNION"
        | "INTERSECT"
        | "EXCEPT" -> sprintf "%s は未対応です（集合演算は UNION ALL のみ対応。別名にする場合は AS を付けてください）" word
        | "RIGHT"
        | "FULL"
        | "CROSS"
        | "NATURAL"
        | "OUTER" -> sprintf "%s は未対応です（INNER JOIN / LEFT JOIN のみ対応。別名にする場合は AS を付けてください）" word
        | _ -> sprintf "%s は未対応です" word

    /// 集合演算が ORDER BY / LIMIT の後ろに続いている場合は、専用のエラーにする。
    /// ORDER BY / LIMIT は複合文全体に掛かるため、枝の後ろ（集合演算より前）にしか書けない。
    let private rejectSetOperation (st: PState) =
        match peek st with
        | Token.Ident s when Set.contains (s.ToUpperInvariant()) SetOperationWords ->
            fail "UNION / INTERSECT / EXCEPT は ORDER BY / LIMIT より前に書いてください" st
        | _ -> ret ()

    /// JOIN の位置に未対応語が来たら、別名や別の JOIN として解釈せず明示エラーにする。
    /// テーブルに `AS` で別名を付けた場合（`FROM t AS x RIGHT JOIN u`）もここで拾う。
    let private rejectUnsupportedClause (st: PState) =
        match peek st with
        | Token.Ident s when Set.contains (s.ToUpperInvariant()) UnsupportedJoinWords ->
            fail (unsupportedAliasMessage (s.ToUpperInvariant())) st
        | _ -> ret ()

    /// 記号pletsを消費して次の状態を返す。
    /// 消費できなくてもエラーにしない。
    let private eatSymbol (s: string) (st: PState) =
        if isSymbol s st then advance st else st

    let private eatKeyword (k: string) (st: PState) =
        if isKeyword k st then advance st else st

    let internal expectSymbol (s: string) (st: PState) =
        if isSymbol s st then
            ret ((), advance st)
        else
            fail (sprintf "'%s' が必要ですが見つかりませんでした" s) st

    let internal expectKeyword (k: string) (st: PState) =
        if isKeyword k st then
            ret ((), advance st)
        else
            fail (sprintf "%s が必要ですが見つかりませんでした" k) st

    /// 末尾の ';' は任意なので、失敗扱いにしない。
    let internal eatOptionalSemicolon (st: PState) = ret ((), eatSymbol ";" st)

    /// 識別子を読む。
    /// 型名（INTEGER など）は予約語ではなく Ident として字句解析されるので、ここでそのまま通る。
    let internal parseIdent (st: PState) =
        match peek st with
        | Token.Ident s -> ret (s, advance st)
        | Token.QuotedIdent s -> ret (s, advance st)
        | _ -> fail "識別子が必要ですが見つかりませんでした" st

    /// 括弧の対応を取って読み飛ばす（CAST の型引数用）。
    let private skipParens (st: PState) =
        let rec loop depth st =
            if depth = 0 then
                ret st
            elif st.Pos >= st.Tokens.Length - 1 then
                fail "型引数の ')' がありません" st
            else
                match peek st with
                | Token.Symbol "(" -> loop (depth + 1) (advance st)
                | Token.Symbol ")" -> loop (depth - 1) (advance st)
                | _ -> loop depth (advance st)

        if isSymbol "(" st then loop 1 (advance st) else ret st

    /// 括弧で囲まれた識別子リストを読む。
    let internal parseParenIdentList (st: PState) =
        let rec loop acc st =
            match parseIdent st with
            | Error e -> err e
            | Ok(c, st) ->
                if isSymbol "," st then
                    advance st |> loop (c :: acc)
                else
                    expectSymbol ")" st |> Result.map (fun (_, st) -> List.rev (c :: acc), st)

        expectSymbol "(" st >>= fun (_, st) -> loop [] st

    let internal parseDataType (st: PState) =
        match peek st with
        | Token.Ident s ->
            match dataTypeOfName (s.ToUpperInvariant()) with
            | Some dt -> ret (dt, advance st)
            | None -> fail (sprintf "未知の型 '%s'" s) st
        | _ -> fail "型名が必要です" st

    // -----------------------------------------------------------------------
    // 別名・対象テーブル・選択項目の補助関数。
    // 式の本体は Pratt（precedence climbing）で読む（下の節）。
    // -----------------------------------------------------------------------

    /// 列の別名を読む。
    /// 未対応の SQL 語（UNION / RIGHT など）は裸の別名として飲み込まない
    /// （後続の SELECT が別の文として実行されるのを防ぐ。`AS` を付ければ別名にできる）。
    let private parseOptAlias (st: PState) =
        if isKeyword "AS" st then
            advance st |> parseIdent |> Result.map (fun (a, st) -> Some a, st)
        else
            match peek st with
            | Token.Ident s when Set.contains (s.ToUpperInvariant()) ReservedAliasWords -> ret (None, st)
            | Token.Ident s -> ret (Some s, advance st)
            | Token.QuotedIdent s -> ret (Some s, advance st)
            | _ -> ret (None, st)

    /// テーブル別名を読む。
    /// 未対応の SQL 語は裸の別名として飲み込まない（`AS` を付ければ別名として使える）。
    /// 集合演算の語（UNION など）はここでは消費もエラーもしない
    /// （`SELECT ... FROM t UNION ALL ...` を後段の集合演算の規則で扱うため）。
    /// `USING` はここでは消費せず、呼び出し側が JOIN / DELETE の文脈に応じてエラーにする。
    let private parseTableAlias (st: PState) =
        if isKeyword "AS" st then
            advance st |> parseIdent |> Result.map (fun (a, st) -> Some a, st)
        else
            match peek st with
            | Token.Ident s when s.ToUpperInvariant() = "USING" -> ret (None, st)
            | Token.Ident s when Set.contains (s.ToUpperInvariant()) SetOperationWords -> ret (None, st)
            | Token.Ident s when Set.contains (s.ToUpperInvariant()) UnsupportedJoinWords ->
                fail (unsupportedAliasMessage (s.ToUpperInvariant())) st
            | Token.Ident s -> ret (Some s, advance st)
            | Token.QuotedIdent s -> ret (Some s, advance st)
            | _ -> ret (None, st)

    /// INSERT / UPDATE / DELETE の対象テーブル。
    /// 別名は受理しない（列解決に使われず、`WHERE x.id` だけが NameError になる
    /// 中途半端な状態を避けるため、パース時点で明示エラーにする）。
    let internal parseTableRef (st: PState) =
        match peek st with
        | Token.Symbol "(" -> fail "INSERT / UPDATE / DELETE の対象テーブルにサブクエリは使えません" st
        | _ ->
            match parseIdent st with
            | Error e -> err e
            | Ok(name, st) ->
                match parseTableAlias st with
                | Error e -> err e
                | Ok(None, st) -> ret ({ Name = name; Alias = None }, st)
                | Ok(Some _, st) -> fail "INSERT / UPDATE / DELETE の対象テーブルに別名は使えません（別名を外してください）" st

    /// 現在位置が `識別子 . *` の形かどうかを返す。
    /// 先頭が識別子でない `NULL.*` などを誤って `QualifiedStar ""` にしないよう、字句も検証する。
    let private isDotStar (st: PState) =
        st.Pos + 2 < st.Tokens.Length
        && (match st.Tokens.[st.Pos].Token with
            | Token.Ident _
            | Token.QuotedIdent _ -> true
            | _ -> false)
        && st.Tokens.[st.Pos + 1].Token = Token.Symbol "."
        && st.Tokens.[st.Pos + 2].Token = Token.Symbol "*"

    // -----------------------------------------------------------------------
    // 式（Pratt / precedence climbing）
    //
    // 優先順位（低い → 高い）:
    //   OR 10
    //   AND 20
    //   NOT（前置）30
    //   比較 / IS / IN / BETWEEN / LIKE 40（非結合）
    //   + - || 50
    //   * / % 60
    //   単項 + -（前置）70
    //   primary
    //
    // 中置演算子はループで左から読み、右辺だけを結合強度で再帰させる。
    // 左結合の右辺は lbp + 1、比較・述語の右辺は 50（加算以上）で読む。
    // 述語は非結合なので、1つの式に 2つ目の述語が来たらエラーにする。
    //
    // 再帰はすべて「継続を引数で受け、末尾で呼ぶ」形にする。
    // 継続はヒープに積まれるので、括弧・前置演算子・CASE・関数呼び出し・
    // サブクエリの入れ子段数に依存せずスタックは一定になる。
    // 深さ上限（MaxExpressionDepth）は設けない。
    // -----------------------------------------------------------------------

    [<Literal>]
    let private OrBp = 10

    [<Literal>]
    let private AndBp = 20

    [<Literal>]
    let private NotPrefixBp = 30

    [<Literal>]
    let private CompareBp = 40

    /// 比較・述語の右辺を読む最低結合強度。
    /// 加算（50）以上を受けるので `a = b + c` は比較の右辺に入り、
    /// `a = b IS NULL` の IS は右辺に入らない。
    [<Literal>]
    let private PredicateRightBp = 50

    [<Literal>]
    let private AddBp = 50

    [<Literal>]
    let private MulBp = 60

    [<Literal>]
    let private UnaryBp = 70

    /// 中置演算子の種類。
    [<RequireQualifiedAccess>]
    type private InfixKind =
        /// 左結合の二項演算子
        | Binary of BinaryOp
        /// 比較（非結合）
        | Compare of BinaryOp
        /// IS [NOT] NULL / TRUE / FALSE / UNKNOWN
        | IsPredicate
        /// [NOT] IN
        | InPredicate of bool
        /// [NOT] BETWEEN
        | BetweenPredicate of bool
        /// [NOT] LIKE
        | LikePredicate of bool
        /// NOT の後ろが IN / BETWEEN / LIKE でない
        | BadNot

    /// 現在位置が中置演算子なら (左の結合強度, 種類) を返す。
    /// NOT は後ろの字句を見て否定の述語かを決める。
    let private infixAt (st: PState) =
        match peek st with
        | Token.Keyword "OR" -> Some(OrBp, InfixKind.Binary BinaryOp.Or)
        | Token.Keyword "AND" -> Some(AndBp, InfixKind.Binary BinaryOp.And)
        | Token.Symbol "=" -> Some(CompareBp, InfixKind.Compare BinaryOp.Eq)
        | Token.Symbol "<>" -> Some(CompareBp, InfixKind.Compare BinaryOp.Ne)
        | Token.Symbol "!=" -> Some(CompareBp, InfixKind.Compare BinaryOp.Ne)
        | Token.Symbol "<=" -> Some(CompareBp, InfixKind.Compare BinaryOp.Le)
        | Token.Symbol ">=" -> Some(CompareBp, InfixKind.Compare BinaryOp.Ge)
        | Token.Symbol "<" -> Some(CompareBp, InfixKind.Compare BinaryOp.Lt)
        | Token.Symbol ">" -> Some(CompareBp, InfixKind.Compare BinaryOp.Gt)
        | Token.Symbol "+" -> Some(AddBp, InfixKind.Binary BinaryOp.Add)
        | Token.Symbol "-" -> Some(AddBp, InfixKind.Binary BinaryOp.Sub)
        | Token.Symbol "||" -> Some(AddBp, InfixKind.Binary BinaryOp.Concat)
        | Token.Symbol "*" -> Some(MulBp, InfixKind.Binary BinaryOp.Mul)
        | Token.Symbol "/" -> Some(MulBp, InfixKind.Binary BinaryOp.Div)
        | Token.Symbol "%" -> Some(MulBp, InfixKind.Binary BinaryOp.Mod)
        | Token.Keyword "IS" -> Some(CompareBp, InfixKind.IsPredicate)
        | Token.Keyword "IN" -> Some(CompareBp, InfixKind.InPredicate false)
        | Token.Keyword "BETWEEN" -> Some(CompareBp, InfixKind.BetweenPredicate false)
        | Token.Keyword "LIKE" -> Some(CompareBp, InfixKind.LikePredicate false)
        | Token.Keyword "NOT" ->
            let next =
                if st.Pos + 1 < st.Tokens.Length then
                    st.Tokens.[st.Pos + 1].Token
                else
                    Token.Eos

            match next with
            | Token.Keyword "IN" -> Some(CompareBp, InfixKind.InPredicate true)
            | Token.Keyword "BETWEEN" -> Some(CompareBp, InfixKind.BetweenPredicate true)
            | Token.Keyword "LIKE" -> Some(CompareBp, InfixKind.LikePredicate true)
            | _ -> Some(CompareBp, InfixKind.BadNot)
        | _ -> None

    /// 式を 1つ読み、継続へ渡す。
    /// 継続は「残りの解析」を表す関数で、必ず末尾位置で呼ぶ。
    /// これにより式・サブクエリの入れ子段数に依存せずスタックは積み上がらない。
    /// 実行時の末尾呼び出しは Directory.Build.props の Tailcalls=true と
    /// 深い入れ子の回帰テストで担保する。
    [<TailCall>]
    let rec private parseExprC (st: PState) (k: Expr * PState -> Result<'r, DbError>) : Result<'r, DbError> =
        parseExprBpC 0 st k

    /// 結合強度 minBp 以上の式を読む。
    and [<TailCall>] private parseExprBpC (minBp: int) (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        parsePrefixC minBp st (fun (left, st) -> parseExprLoopC minBp false left st k)

    /// 中置演算子のループ。
    /// 右辺を読むときは継続を 1つ積み、読み終えたら自分自身か継続を末尾で呼ぶ。
    and private parseExprLoopC
        (minBp: int)
        (seenPredicate: bool)
        (left: Expr)
        (st: PState)
        (k: Expr * PState -> Result<'r, DbError>)
        =
        match peek st with
        | Token.Symbol "." -> fail "'.' の後ろには列名か '*' が必要です（修飾子は識別子にだけ付けられます）" st
        | _ ->
            match infixAt st with
            | Some(lbp, kind) when lbp >= minBp ->
                if seenPredicate && lbp >= CompareBp then
                    fail "比較・述語は 1つの式に 1回だけ書けます（括弧で囲んでください）" st
                else
                    match kind with
                    | InfixKind.Binary op ->
                        parseExprBpC (lbp + 1) (advance st) (fun (right, st) ->
                            parseExprLoopC minBp false (Expr.Binary(op, left, right)) st k)
                    | InfixKind.Compare op ->
                        parseExprBpC PredicateRightBp (advance st) (fun (right, st) ->
                            parseExprLoopC minBp true (Expr.Binary(op, left, right)) st k)
                    | InfixKind.IsPredicate ->
                        let afterIs = advance st
                        let negated = isKeyword "NOT" afterIs
                        let afterNot = eatKeyword "NOT" afterIs

                        if isKeyword "TRUE" afterNot then
                            parseExprLoopC
                                minBp
                                true
                                (Expr.IsTruth(left, TruthValue.True, negated))
                                (advance afterNot)
                                k
                        elif isKeyword "FALSE" afterNot then
                            parseExprLoopC
                                minBp
                                true
                                (Expr.IsTruth(left, TruthValue.False, negated))
                                (advance afterNot)
                                k
                        elif isIdentName "UNKNOWN" afterNot then
                            parseExprLoopC
                                minBp
                                true
                                (Expr.IsTruth(left, TruthValue.Unknown, negated))
                                (advance afterNot)
                                k
                        else
                            match expectKeyword "NULL" afterNot with
                            | Error e -> err e
                            | Ok(_, st) -> parseExprLoopC minBp true (Expr.IsNull(left, negated)) st k
                    | InfixKind.InPredicate negated ->
                        let atIn = if negated then advance st else st

                        match expectKeyword "IN" atIn with
                        | Error e -> err e
                        | Ok(_, st) ->
                            parseInPredicateBodyC left negated st (fun (expr, st) ->
                                parseExprLoopC minBp true expr st k)
                    | InfixKind.BetweenPredicate negated ->
                        let atBetween = if negated then advance st else st

                        match expectKeyword "BETWEEN" atBetween with
                        | Error e -> err e
                        | Ok(_, st) ->
                            parseExprBpC PredicateRightBp st (fun (lo, st) ->
                                match expectKeyword "AND" st with
                                | Error e -> err e
                                | Ok(_, st) ->
                                    parseExprBpC PredicateRightBp st (fun (hi, st) ->
                                        parseExprLoopC minBp true (Expr.Between(left, lo, hi, negated)) st k))
                    | InfixKind.LikePredicate negated ->
                        let atLike = if negated then advance st else st

                        match expectKeyword "LIKE" atLike with
                        | Error e -> err e
                        | Ok(_, st) ->
                            parseExprBpC PredicateRightBp st (fun (pattern, st) ->
                                if isIdentName "ESCAPE" st then
                                    parseExprBpC PredicateRightBp (advance st) (fun (escape, st) ->
                                        parseExprLoopC
                                            minBp
                                            true
                                            (Expr.Like(left, pattern, negated, Some escape))
                                            st
                                            k)
                                else
                                    parseExprLoopC minBp true (Expr.Like(left, pattern, negated, None)) st k)
                    | InfixKind.BadNot -> fail "NOT の後ろには IN / BETWEEN / LIKE が必要です" (advance st)
            | _ -> k (left, st)

    /// 前置演算子（NOT / 単項 - +）と primary。
    and [<TailCall>] private parsePrefixC (minBp: int) (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        match peek st with
        | Token.Keyword "NOT" when NotPrefixBp >= minBp ->
            parseExprBpC NotPrefixBp (advance st) (fun (operand, st) -> k (Expr.Unary(UnaryOp.Not, operand), st))
        | Token.Symbol "-" when UnaryBp >= minBp ->
            parseExprBpC UnaryBp (advance st) (fun (operand, st) -> k (Expr.Unary(UnaryOp.Neg, operand), st))
        | Token.Symbol "+" when UnaryBp >= minBp ->
            parseExprBpC UnaryBp (advance st) (fun (operand, st) -> k (Expr.Unary(UnaryOp.Pos, operand), st))
        | _ -> parsePrimaryC st k

    and [<TailCall>] private parsePrimaryC (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        match peek st with
        | Token.IntLit v -> k (Expr.Literal(Literal.Int v), advance st)
        | Token.RealLit v -> k (Expr.Literal(Literal.Real v), advance st)
        | Token.StringLit s -> k (Expr.Literal(Literal.String s), advance st)
        | Token.BlobLit b -> k (Expr.Literal(Literal.Blob b), advance st)
        | Token.Param i ->
            match Array.tryItem i st.Parameters with
            | Some v -> k (Expr.Value v, advance st)
            | None -> fail (sprintf "パラメータ ?%d が指定されていません" (i + 1)) st
        | Token.Keyword "NULL" -> k (Expr.Literal Literal.Null, advance st)
        | Token.Keyword "TRUE" -> k (Expr.Literal(Literal.Bool true), advance st)
        | Token.Keyword "FALSE" -> k (Expr.Literal(Literal.Bool false), advance st)
        | Token.Keyword "DEFAULT" -> k (Expr.Default, advance st)
        | Token.Keyword "CASE" -> parseCaseC st k
        | Token.Symbol "*" -> k (Expr.Star, advance st)
        | Token.Keyword "EXISTS" -> parseExistsC st k
        | Token.Symbol "(" -> parseParenExprC st k
        | Token.Ident name -> parseNamedPrimaryC name st k
        | Token.QuotedIdent name -> parseNamedPrimaryC name st k
        | _ -> fail "式が必要ですが見つかりませんでした" st

    /// CASE 式。
    and private parseCaseC (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        match expectKeyword "CASE" st with
        | Error e -> err e
        | Ok(_, st) ->
            parseCaseBranchesC [] st (fun (branches, st) ->
                if List.isEmpty branches then
                    fail "CASE には WHEN 句が必要です" st
                elif isKeyword "ELSE" st then
                    parseExprC (advance st) (fun (elseE, st) ->
                        match expectKeyword "END" st with
                        | Error e -> err e
                        | Ok(_, st) -> k (Expr.Case(branches, Some elseE), st))
                else
                    match expectKeyword "END" st with
                    | Error e -> err e
                    | Ok(_, st) -> k (Expr.Case(branches, None), st))

    and [<TailCall>] private parseCaseBranchesC
        (acc: (Expr * Expr) list)
        (st: PState)
        (k: (Expr * Expr) list * PState -> Result<'r, DbError>)
        =
        if isKeyword "WHEN" st then
            parseExprC (advance st) (fun (cond, st) ->
                match expectKeyword "THEN" st with
                | Error e -> err e
                | Ok(_, st) -> parseExprC st (fun (res, st) -> parseCaseBranchesC ((cond, res) :: acc) st k))
        else
            k (List.rev acc, st)

    /// EXISTS (SELECT ...)
    and [<TailCall>] private parseExistsC (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        match advance st |> expectSymbol "(" with
        | Error e -> err e
        | Ok(_, afterParen) ->
            parseSelectStatementC afterParen (fun (sel, st) ->
                match expectSymbol ")" st with
                | Error e -> err e
                | Ok(_, st) -> k (Expr.Exists sel, st))

    /// 括弧付きの式、またはスカラーサブクエリ `(SELECT ...)`。
    and [<TailCall>] private parseParenExprC (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        let afterParen = advance st

        if isKeyword "SELECT" afterParen then
            parseSelectStatementC afterParen (fun (sel, st) ->
                match expectSymbol ")" st with
                | Error e -> err e
                | Ok(_, st) -> k (Expr.Subquery sel, st))
        else
            parseExprC afterParen (fun (e, st) ->
                match expectSymbol ")" st with
                | Error e -> err e
                | Ok(_, st) -> k (e, st))

    /// 識別子の後ろ（列参照 / t.* / 関数呼び出し / CAST）。
    and [<TailCall>] private parseNamedPrimaryC (name: string) (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        let st = advance st

        if isSymbol "." st then
            let afterDot = advance st

            if isSymbol "*" afterDot then
                k (Expr.QualifiedStar name, advance afterDot)
            else
                match parseIdent afterDot with
                | Error e -> err e
                | Ok(c, st) -> k (Expr.Column { Qualifier = Some name; Name = c }, st)
        elif isSymbol "(" st then
            if name.ToUpperInvariant() = "CAST" then
                parseCastExprC st k
            else
                parseCallBodyC name st k
        else
            k (Expr.Name name, st)

    /// CAST(expr AS 型[型引数])。
    and [<TailCall>] private parseCastExprC (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        match expectSymbol "(" st with
        | Error e -> err e
        | Ok(_, st) ->
            parseExprC st (fun (e, st) ->
                match expectKeyword "AS" st with
                | Error e -> err e
                | Ok(_, st) ->
                    match parseDataType st with
                    | Error e -> err e
                    | Ok(dt, st) ->
                        match skipParens st with
                        | Error e -> err e
                        | Ok st ->
                            match expectSymbol ")" st with
                            | Error e -> err e
                            | Ok(_, st) -> k (Expr.Cast(e, dt), st))

    /// 関数呼び出し（集約の DISTINCT と COUNT(*) を含む）。
    and [<TailCall>] private parseCallBodyC (name: string) (st: PState) (k: Expr * PState -> Result<'r, DbError>) =
        match expectSymbol "(" st with
        | Error e -> err e
        | Ok(_, st) ->
            let upper = name.ToUpperInvariant()

            if upper = "COUNT" && isSymbol "*" st then
                match advance st |> expectSymbol ")" with
                | Error e -> err e
                | Ok(_, st) -> k (Expr.AggCall(AggFunc.Count, false, Expr.Star), st)
            else
                let isAgg = (AggFunc.tryParse upper).IsSome
                let distinct = isAgg && isKeyword "DISTINCT" st
                let st = if distinct then advance st else st

                parseExprListC st (fun (argList, st) ->
                    match AggFunc.tryParse upper with
                    | Some agg when List.length argList = 1 -> k (Expr.AggCall(agg, distinct, List.head argList), st)
                    | _ -> k (Expr.FuncCall(upper, argList), st))

    and private parseExprListLoop (acc: Expr list) (st: PState) (k: Expr list * PState -> Result<'r, DbError>) =
        parseExprC st (fun (e, st) ->
            if isSymbol "," st then
                parseExprListLoop (e :: acc) (advance st) k
            else
                match expectSymbol ")" st with
                | Error e -> err e
                | Ok(_, st) -> k (List.rev (e :: acc), st))

    /// `(` の後ろの式を `,` 区切りで読み、`)` まで消費する。
    and [<TailCall>] private parseExprListC (st: PState) (k: Expr list * PState -> Result<'r, DbError>) =
        parseExprListLoop [] st k

    /// IN の右辺（`(SELECT ...)` か式のリスト）を読み、`)` まで消費する。
    and [<TailCall>] private parseInPredicateBodyC
        (left: Expr)
        (negated: bool)
        (st: PState)
        (k: Expr * PState -> Result<'r, DbError>)
        =
        match expectSymbol "(" st with
        | Error e -> err e
        | Ok(_, afterParen) ->
            if isKeyword "SELECT" afterParen then
                parseSelectStatementC afterParen (fun (sel, st) ->
                    match expectSymbol ")" st with
                    | Error e -> err e
                    | Ok(_, st) -> k (Expr.InSubquery(left, sel, negated), st))
            else
                parseExprListC afterParen (fun (xs, st) -> k (Expr.InList(left, xs, negated), st))

    /// SELECT 文（`UNION ALL` の複合文を含む）。
    /// 先頭の SELECT（コア）→ 集合演算の枝 → ORDER BY / LIMIT / OFFSET の順に読む。
    /// ORDER BY / LIMIT は複合文全体に掛かる（枝の後ろには書けない）。
    and [<TailCall>] private parseSelectStatementC (st: PState) (k: SelectStatement * PState -> Result<'r, DbError>) =
        parseSelectCoreC st (fun (core, st) ->
            parseSetOperationsC [] st (fun (setOps, st) ->
                parseOrderByC st (fun (orderBy, st) ->
                    parseLimitOffsetC st (fun ((limit, offset), st) ->
                        match rejectSetOperation st with
                        | Error e -> err e
                        | Ok() ->
                            k (
                                { core with
                                    SetOperations = setOps
                                    OrderBy = orderBy
                                    Limit = limit
                                    Offset = offset
                                },
                                st
                            )))))

    /// 集合演算を除いた 1つの SELECT（`SELECT ... WHERE ... GROUP BY ... HAVING ...`）。
    /// `ORDER BY` / `LIMIT` / `OFFSET` は複合文全体に掛かるのでここでは読まない。
    and [<TailCall>] private parseSelectCoreC (st: PState) (k: SelectStatement * PState -> Result<'r, DbError>) =
        match expectKeyword "SELECT" st with
        | Error e -> err e
        | Ok(_, st) ->
            let distinct = isKeyword "DISTINCT" st
            let st = if distinct then advance st else st

            parseSelectItemsC st (fun (items, st) ->
                if isKeyword "FROM" st then
                    parseSourceC (advance st) (fun (fromRef, st) ->
                        parseSelectCoreTailC distinct items (Some fromRef) st k)
                elif isKeyword "JOIN" st || isKeyword "INNER" st || isKeyword "LEFT" st then
                    fail "JOIN には FROM が必要です" st
                else
                    parseSelectCoreTailC distinct items None st k)

    /// `UNION ALL` の枝（0個以上）を読む。
    /// `UNION`（ALL なし）と `INTERSECT` / `EXCEPT` は未対応なので明示エラーにする。
    and [<TailCall>] private parseSetOperationsC
        (acc: SetOperation list)
        (st: PState)
        (k: SetOperation list * PState -> Result<'r, DbError>)
        =
        if isIdentName "UNION" st then
            let afterUnion = advance st

            if isIdentName "ALL" afterUnion then
                parseSelectCoreC (advance afterUnion) (fun (core, st) ->
                    parseSetOperationsC
                        ({
                            Kind = SetOperationKind.UnionAll
                            Statement = core
                         }
                         :: acc)
                        st
                        k)
            else
                fail "UNION（重複を除く集合演算）は未対応です（UNION ALL を使ってください）" st
        elif isIdentName "INTERSECT" st then
            fail "INTERSECT は未対応です（集合演算は UNION ALL のみ対応）" st
        elif isIdentName "EXCEPT" st then
            fail "EXCEPT は未対応です（集合演算は UNION ALL のみ対応）" st
        else
            k (List.rev acc, st)

    and private parseSelectItemsLoop
        (acc: SelectItem list)
        (st: PState)
        (k: SelectItem list * PState -> Result<'r, DbError>)
        =
        parseSelectItemC st (fun (item, st) ->
            if isSymbol "," st then
                parseSelectItemsLoop (item :: acc) (advance st) k
            else
                k (List.rev (item :: acc), st))

    /// SELECT の項目リスト（カンマ区切り、1個以上）。
    and [<TailCall>] private parseSelectItemsC (st: PState) (k: SelectItem list * PState -> Result<'r, DbError>) =
        parseSelectItemsLoop [] st k

    /// SELECT の 1項目。
    /// `*` / `t.*` は別名を取らない（`SELECT * x` を別名として飲み込まない）。
    and [<TailCall>] private parseSelectItemC (st: PState) (k: SelectItem * PState -> Result<'r, DbError>) =
        match peek st with
        | Token.Symbol "*" -> k ({ Expression = Expr.Star; Alias = None }, advance st)
        | Token.Ident name
        | Token.QuotedIdent name when isDotStar st ->
            k (
                {
                    Expression = Expr.QualifiedStar name
                    Alias = None
                },
                st |> advance |> advance |> advance
            )
        | _ ->
            parseExprC st (fun (e, st) ->
                match parseOptAlias st with
                | Error e -> err e
                | Ok(alias, st) -> k ({ Expression = e; Alias = alias }, st))

    /// FROM / JOIN の入力を読む。
    /// `(` の次が SELECT なら派生テーブル（`FROM (SELECT ...) AS x`）。
    /// 派生テーブルには別名を必須にしている（列修飾と JOIN のスキーマ特定に使うため）。
    and private parseSourceC (st: PState) (k: TableSource * PState -> Result<'r, DbError>) =
        match peek st with
        | Token.Symbol "(" ->
            let afterParen = advance st

            if isKeyword "SELECT" afterParen then
                parseSelectStatementC afterParen (fun (sel, st) ->
                    match expectSymbol ")" st with
                    | Error e -> err e
                    | Ok(_, st) ->
                        match parseTableAlias st with
                        | Error e -> err e
                        | Ok(Some alias, st) -> k (TableSource.Derived(sel, alias), st)
                        | Ok(None, st) -> fail "サブクエリ（派生テーブル）には別名が必要です（例: AS x）" st)
            else
                fail "FROM の括弧の中には SELECT を書いてください" afterParen
        | _ ->
            match parseIdent st with
            | Error e -> err e
            | Ok(name, st) ->
                match parseTableAlias st with
                | Error e -> err e
                | Ok(alias, st) -> k (TableSource.Named { Name = name; Alias = alias }, st)

    /// SELECT のコア（FROM 省略可）より後ろの句を順に読む。
    /// FROM を省略した SELECT の入力は 1行 0列になる。
    /// `ORDER BY` / `LIMIT` / `OFFSET` は集合演算（`parseSelectStatementC`）が読む。
    and [<TailCall>] private parseSelectCoreTailC
        (distinct: bool)
        (items: SelectItem list)
        (fromRef: TableSource option)
        (st: PState)
        (k: SelectStatement * PState -> Result<'r, DbError>)
        =
        parseJoinsC st (fun (joins, st) ->
            parseWhereC st (fun (whereE, st) ->
                parseGroupByC st (fun (groupBy, st) ->
                    parseHavingC st (fun (having, st) ->
                        k (
                            {
                                Distinct = distinct
                                Items = items
                                From = fromRef
                                Joins = joins
                                Where = whereE
                                GroupBy = groupBy
                                Having = having
                                OrderBy = []
                                Limit = None
                                Offset = None
                                SetOperations = []
                            },
                            st
                        )))))

    and private parseJoinsLoop
        (acc: JoinClause list)
        (st: PState)
        (k: JoinClause list * PState -> Result<'r, DbError>)
        =
        match rejectUnsupportedClause st with
        | Error e -> err e
        | Ok() ->
            let parsedKind = parseJoinKind st

            let kindAnswer =
                if isKeyword "JOIN" st then
                    Ok(JoinKind.Inner, st)
                else
                    match parsedKind with
                    | Some(kind, afterKind) ->
                        if isKeyword "JOIN" afterKind then
                            Ok(kind, afterKind)
                        else
                            fail "LEFT / INNER の後ろには JOIN が必要です" afterKind
                    | None -> Ok(JoinKind.Inner, st)

            match kindAnswer with
            | Error e -> err e
            | Ok(kind, atJoin) ->
                if isKeyword "JOIN" atJoin then
                    parseSourceC (advance atJoin) (fun (tbl, st) ->
                        if isIdentName "USING" st then
                            fail "USING は未対応です（JOIN では ON を使ってください）" st
                        else
                            match expectKeyword "ON" st with
                            | Error e -> err e
                            | Ok(_, st) ->
                                parseExprC st (fun (on, st) ->
                                    parseJoinsLoop ({ Kind = kind; Table = tbl; On = on } :: acc) st k))
                else
                    k (List.rev acc, st)

    /// JOIN 句の並び（0個以上）。
    /// 種類だけ書いて JOIN が続かない入力は黙って捨てずにエラーにする
    /// （`FROM t LEFT WHERE ...` が LEFT JOIN なしとして静かに通っていた）。
    and [<TailCall>] private parseJoinsC (st: PState) (k: JoinClause list * PState -> Result<'r, DbError>) =
        parseJoinsLoop [] st k

    /// JOIN の種類（INNER / LEFT [OUTER]）を読む。
    /// 語を消費した後の状態と種類を返す。
    /// どちらの語も無ければ None。
    and [<TailCall>] private parseJoinKind (st: PState) =
        if isKeyword "INNER" st then
            Some(JoinKind.Inner, advance st)
        elif isKeyword "LEFT" st then
            Some(JoinKind.Left, advance st |> eatIdentName "OUTER")
        else
            None

    and [<TailCall>] private parseWhereC (st: PState) (k: Expr option * PState -> Result<'r, DbError>) =
        if isKeyword "WHERE" st then
            parseExprC (advance st) (fun (e, st) -> k (Some e, st))
        else
            k (None, st)

    and [<TailCall>] private parseGroupByC (st: PState) (k: Expr list * PState -> Result<'r, DbError>) =
        if isKeyword "GROUP" st then
            match advance st |> expectKeyword "BY" with
            | Error e -> err e
            | Ok(_, st) -> parseGroupByItemsC st k
        else
            k ([], st)

    and private parseGroupByItemsLoop (acc: Expr list) (st: PState) (k: Expr list * PState -> Result<'r, DbError>) =
        parseExprC st (fun (e, st) ->
            if isSymbol "," st then
                parseGroupByItemsLoop (e :: acc) (advance st) k
            else
                k (List.rev (e :: acc), st))

    and [<TailCall>] private parseGroupByItemsC (st: PState) (k: Expr list * PState -> Result<'r, DbError>) =
        parseGroupByItemsLoop [] st k

    and [<TailCall>] private parseHavingC (st: PState) (k: Expr option * PState -> Result<'r, DbError>) =
        if isKeyword "HAVING" st then
            parseExprC (advance st) (fun (e, st) -> k (Some e, st))
        else
            k (None, st)

    and [<TailCall>] private parseOrderByC (st: PState) (k: SortKey list * PState -> Result<'r, DbError>) =
        if isKeyword "ORDER" st then
            match advance st |> expectKeyword "BY" with
            | Error e -> err e
            | Ok(_, st) -> parseOrderByItemsC st k
        else
            k ([], st)

    and private parseOrderByItemsLoop
        (acc: SortKey list)
        (st: PState)
        (k: SortKey list * PState -> Result<'r, DbError>)
        =
        parseExprC st (fun (e, st) ->
            let dir, st =
                if isKeyword "DESC" st then
                    SortDirection.Descending, advance st
                elif isKeyword "ASC" st then
                    SortDirection.Ascending, advance st
                else
                    SortDirection.Ascending, st

            let key = { Expression = e; Direction = dir }

            if isSymbol "," st then
                parseOrderByItemsLoop (key :: acc) (advance st) k
            else
                k (List.rev (key :: acc), st))

    and [<TailCall>] private parseOrderByItemsC (st: PState) (k: SortKey list * PState -> Result<'r, DbError>) =
        parseOrderByItemsLoop [] st k

    and private parseLimitOffsetC (st: PState) (k: (Expr option * Expr option) * PState -> Result<'r, DbError>) =
        if isKeyword "LIMIT" st then
            parseExprC (advance st) (fun (limit, st) ->
                if isKeyword "OFFSET" st then
                    parseExprC (advance st) (fun (offset, st) -> k ((Some limit, Some offset), st))
                else
                    k ((Some limit, None), st))
        elif isKeyword "OFFSET" st then
            parseExprC (advance st) (fun (offset, st) -> k ((None, Some offset), st))
        else
            k ((None, None), st)

    /// CPS の構文規則を、値をそのまま返す形で呼ぶ入口。
    /// DML・文の列挙など、呼び出しの入れ子が入力サイズに比例しない箇所で使う。
    let internal parseExprValue (st: PState) = parseExprC st ret

    let internal parseWhereValue (st: PState) = parseWhereC st ret

    let internal parseSelectStatementValue (st: PState) = parseSelectStatementC st ret

    let internal parseSelect (st: PState) =
        result {
            let! sel, st = parseSelectStatementValue st
            let! _, st = eatOptionalSemicolon st
            return Statement.Select sel, st
        }
