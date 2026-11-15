namespace RykDb.Core

/// パーサの状態。
/// 読み取り位置は値が持つので mutable は使わない。
/// すべての構文規則は (値, 次の状態) を返す純粋な関数になる。
type private PState = { Tokens: TokenItem array; Pos: int }

/// SQL 文を AST に変換する再帰下降パーサ。
/// 失敗はすべて Result<'T, DbError> として返し、例外は送らない。
/// 式と SELECT の構文規則は CPS（継続を引数で渡す）で書き、継続は必ず末尾位置で呼ぶ。
/// これにより式・サブクエリの入れ子段数を入力で決めてもスタックオーバーフローしない
/// （深さ上限は設けない）。
/// 実値型の小さな構文規則は Errors.fs の result { ... } で状態を束縛する。
module internal Parser =
    /// 型名の綴り → DataType の表。綴りを増やすときはここだけ直す
    /// （型名セットと parseDataType の両方がこの表から導出される）。
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

    let private peek (st: PState) = st.Tokens.[st.Pos].Token

    let private peekPos (st: PState) = st.Tokens.[st.Pos].Pos

    /// 1トークン消費した次の状態を返す。
    /// 末尾なら動かない。
    let private advance (st: PState) =
        if st.Pos < st.Tokens.Length - 1 then
            { st with Pos = st.Pos + 1 }
        else
            st

    /// 現在位置を添えた構文エラーを返す。
    /// 送出はしない。
    let private fail (msg: string) (st: PState) =
        err (DbError.syntax msg (peekPos st).Line)

    let private isSymbol (s: string) (st: PState) =
        match peek st with
        | Token.Symbol x -> x = s
        | _ -> false

    let private isKeyword (k: string) (st: PState) =
        match peek st with
        | Token.Keyword x -> x = k
        | _ -> false

    /// 未対応の集合演算。
    /// これらは予約語ではないため、テーブル別名として飲み込むと
    /// 後続の SELECT が別の文として実行されてしまう。
    let private SetOperationWords = Set.ofList [ "UNION"; "INTERSECT"; "EXCEPT" ]

    /// 未対応の JOIN 系の語。
    /// テーブル別名として飲み込むと、RIGHT JOIN などが INNER JOIN として静かに実行される。
    let private UnsupportedJoinWords =
        Set.ofList [ "RIGHT"; "FULL"; "CROSS"; "NATURAL"; "OUTER" ]

    /// テーブル別名として飲み込んではいけない語。
    let private UnsupportedTableAliasWords =
        Set.union SetOperationWords UnsupportedJoinWords

    /// 現在位置が指定した識別子（大文字小文字を無視）か。
    let private isIdentName (name: string) (st: PState) =
        match peek st with
        | Token.Ident s -> s.ToUpperInvariant() = name
        | _ -> false

    /// 指定した識別子があれば消費する（無くてもエラーにしない）。
    let private eatIdentName (name: string) (st: PState) =
        if isIdentName name st then advance st else st

    /// 未対応語をテーブル別名として飲み込まないためのメッセージ。
    let private unsupportedAliasMessage (word: string) =
        match word with
        | "UNION"
        | "INTERSECT"
        | "EXCEPT" -> sprintf "%s は未対応です（集合演算は未対応。別名にする場合は AS を付けてください）" word
        | "RIGHT"
        | "FULL"
        | "CROSS"
        | "NATURAL"
        | "OUTER" -> sprintf "%s は未対応です（INNER JOIN / LEFT JOIN のみ対応。別名にする場合は AS を付けてください）" word
        | _ -> sprintf "%s は未対応です" word

    /// 集合演算が続いている場合は、別名として飲み込まずに明示エラーにする。
    let private rejectSetOperation (st: PState) =
        match peek st with
        | Token.Ident s when Set.contains (s.ToUpperInvariant()) SetOperationWords ->
            fail "集合演算（UNION / INTERSECT / EXCEPT）は未対応です" st
        | _ -> ret ()

    /// JOIN の位置に未対応語が来たら、別名や別の JOIN として解釈せず明示エラーにする。
    /// テーブルに `AS` で別名を付けた場合（`FROM t AS x RIGHT JOIN u`）もここで拾う。
    let private rejectUnsupportedClause (st: PState) =
        match peek st with
        | Token.Ident s when Set.contains (s.ToUpperInvariant()) UnsupportedTableAliasWords ->
            fail (unsupportedAliasMessage (s.ToUpperInvariant())) st
        | _ -> ret ()

    /// 記号pletsを消費して次の状態を返す。
    /// 消費できなくてもエラーにしない。
    let private eatSymbol (s: string) (st: PState) =
        if isSymbol s st then advance st else st

    let private eatKeyword (k: string) (st: PState) =
        if isKeyword k st then advance st else st

    let private expectSymbol (s: string) (st: PState) =
        if isSymbol s st then
            ret ((), advance st)
        else
            fail (sprintf "'%s' が必要ですが見つかりませんでした" s) st

    let private expectKeyword (k: string) (st: PState) =
        if isKeyword k st then
            ret ((), advance st)
        else
            fail (sprintf "%s が必要ですが見つかりませんでした" k) st

    /// 末尾の ';' は任意なので、失敗扱いにしない。
    let private eatOptionalSemicolon (st: PState) = ret ((), eatSymbol ";" st)

    /// 識別子を読む。
    /// 型名（INTEGER など）は予約語ではなく Ident として字句解析されるので、ここでそのまま通る。
    let private parseIdent (st: PState) =
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
    let private parseParenIdentList (st: PState) =
        let rec loop acc st =
            match parseIdent st with
            | Error e -> err e
            | Ok(c, st) ->
                if isSymbol "," st then
                    advance st |> loop (c :: acc)
                else
                    expectSymbol ")" st |> Result.map (fun (_, st) -> List.rev (c :: acc), st)

        expectSymbol "(" st >>= fun (_, st) -> loop [] st

    let private parseDataType (st: PState) =
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
            | Token.Ident s when Set.contains (s.ToUpperInvariant()) UnsupportedTableAliasWords -> ret (None, st)
            | Token.Ident s -> ret (Some s, advance st)
            | Token.QuotedIdent s -> ret (Some s, advance st)
            | _ -> ret (None, st)

    /// テーブル別名を読む。
    /// 未対応の SQL 語は裸の別名として飲み込まない（`AS` を付ければ別名として使える）。
    /// `USING` はここでは消費せず、呼び出し側が JOIN / DELETE の文脈に応じてエラーにする。
    let private parseTableAlias (st: PState) =
        if isKeyword "AS" st then
            advance st |> parseIdent |> Result.map (fun (a, st) -> Some a, st)
        else
            match peek st with
            | Token.Ident s when s.ToUpperInvariant() = "USING" -> ret (None, st)
            | Token.Ident s when Set.contains (s.ToUpperInvariant()) UnsupportedTableAliasWords ->
                fail (unsupportedAliasMessage (s.ToUpperInvariant())) st
            | Token.Ident s -> ret (Some s, advance st)
            | Token.QuotedIdent s -> ret (Some s, advance st)
            | _ -> ret (None, st)

    /// INSERT / UPDATE / DELETE の対象テーブル。
    /// 別名は受理しない（列解決に使われず、`WHERE x.id` だけが NameError になる
    /// 中途半端な状態を避けるため、パース時点で明示エラーにする）。
    let private parseTableRef (st: PState) =
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

    and [<TailCall>] private parseSelectStatementC (st: PState) (k: SelectStatement * PState -> Result<'r, DbError>) =
        match expectKeyword "SELECT" st with
        | Error e -> err e
        | Ok(_, st) ->
            let distinct = isKeyword "DISTINCT" st
            let st = if distinct then advance st else st

            parseSelectItemsC st (fun (items, st) ->
                if isKeyword "FROM" st then
                    parseSourceC (advance st) (fun (fromRef, st) ->
                        parseSelectTailC distinct items (Some fromRef) st k)
                elif isKeyword "JOIN" st || isKeyword "INNER" st || isKeyword "LEFT" st then
                    fail "JOIN には FROM が必要です" st
                else
                    parseSelectTailC distinct items None st k)

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

    /// FROM（省略可）より後ろの句を順に読む。
    /// FROM を省略した SELECT の入力は 1行 0列になる。
    and [<TailCall>] private parseSelectTailC
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
                        parseOrderByC st (fun (orderBy, st) ->
                            parseLimitOffsetC st (fun ((limit, offset), st) ->
                                match rejectSetOperation st with
                                | Error e -> err e
                                | Ok() ->
                                    k (
                                        {
                                            Distinct = distinct
                                            Items = items
                                            From = fromRef
                                            Joins = joins
                                            Where = whereE
                                            GroupBy = groupBy
                                            Having = having
                                            OrderBy = orderBy
                                            Limit = limit
                                            Offset = offset
                                        },
                                        st
                                    )))))))

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
    let private parseExprValue (st: PState) = parseExprC st ret

    let private parseWhereValue (st: PState) = parseWhereC st ret

    let private parseSelectStatementValue (st: PState) = parseSelectStatementC st ret

    let private parseSelect (st: PState) =
        result {
            let! sel, st = parseSelectStatementValue st
            let! _, st = eatOptionalSemicolon st
            return Statement.Select sel, st
        }

    let private parseInsert (st: PState) =
        let simpleLiteral st =
            let nextEnds =
                st.Pos + 1 < st.Tokens.Length
                && match st.Tokens.[st.Pos + 1].Token with
                   | Token.Symbol ","
                   | Token.Symbol ")" -> true
                   | _ -> false

            if not nextEnds then
                None
            else
                match peek st with
                | Token.IntLit v -> Some(Expr.Literal(Literal.Int v), advance st)
                | Token.RealLit v -> Some(Expr.Literal(Literal.Real v), advance st)
                | Token.StringLit v -> Some(Expr.Literal(Literal.String v), advance st)
                | Token.BlobLit v -> Some(Expr.Literal(Literal.Blob v), advance st)
                | Token.Keyword "NULL" -> Some(Expr.Literal Literal.Null, advance st)
                | Token.Keyword "TRUE" -> Some(Expr.Literal(Literal.Bool true), advance st)
                | Token.Keyword "FALSE" -> Some(Expr.Literal(Literal.Bool false), advance st)
                | Token.Keyword "DEFAULT" -> Some(Expr.Default, advance st)
                | _ -> None

        let rec one acc st =
            let parsed =
                match simpleLiteral st with
                | Some e -> Ok e
                | None -> parseExprValue st

            match parsed with
            | Error e -> err e
            | Ok(e, st) ->
                if isSymbol "," st then
                    advance st |> one (e :: acc)
                else
                    expectSymbol ")" st |> Result.map (fun (_, st) -> List.rev (e :: acc), st)

        let rec rows acc st =
            match expectSymbol "(" st with
            | Error e -> err e
            | Ok(_, st) ->
                match one [] st with
                | Error e -> err e
                | Ok(r, st) ->
                    if isSymbol "," st then
                        advance st |> rows (r :: acc)
                    else
                        ret (List.rev (r :: acc), st)

        result {
            let! _, st = expectKeyword "INSERT" st
            let! _, st = expectKeyword "INTO" st
            let! tbl, st = parseTableRef st
            let hasColumns = isSymbol "(" st

            let! columns, st =
                if hasColumns then
                    parseParenIdentList st |> Result.map (fun (cols, st) -> Some cols, st)
                else
                    ret (None, st)

            if isKeyword "DEFAULT" st then
                match columns with
                | Some _ -> return! fail "INSERT ... DEFAULT VALUES では列リストを指定できません" st
                | None ->
                    let st = advance st
                    let! _, st = expectKeyword "VALUES" st
                    let! _, st = eatOptionalSemicolon st

                    return
                        {
                            Table = tbl
                            Columns = None
                            Source = InsertSource.Values [ [] ]
                        },
                        st
            elif isKeyword "SELECT" st then
                let! sel, st = parseSelectStatementValue st
                let! _, st = eatOptionalSemicolon st

                return
                    {
                        Table = tbl
                        Columns = columns
                        Source = InsertSource.Select sel
                    },
                    st
            else
                let! _, st = expectKeyword "VALUES" st
                let! rs, st = rows [] st
                let! _, st = eatOptionalSemicolon st

                return
                    {
                        Table = tbl
                        Columns = columns
                        Source = InsertSource.Values rs
                    },
                    st
        }

    let private parseUpdate (st: PState) =
        let rec assigns acc st =
            match parseIdent st with
            | Error e -> err e
            | Ok(col, st) ->
                match expectSymbol "=" st with
                | Error e -> err e
                | Ok(_, st) ->
                    match parseExprValue st with
                    | Error e -> err e
                    | Ok(e, st) ->
                        if isSymbol "," st then
                            advance st |> assigns ((col, e) :: acc)
                        else
                            ret (List.rev ((col, e) :: acc), st)

        result {
            let! _, st = expectKeyword "UPDATE" st
            let! tbl, st = parseTableRef st
            let! _, st = expectKeyword "SET" st
            let! asg, st = assigns [] st
            let! whereE, st = parseWhereValue st
            let! _, st = eatOptionalSemicolon st

            return
                {
                    Table = tbl
                    Assignments = asg
                    Where = whereE
                },
                st
        }

    let private parseDelete (st: PState) =
        result {
            let! _, st = expectKeyword "DELETE" st
            let! _, st = expectKeyword "FROM" st
            let! tbl, st = parseTableRef st

            do!
                if isIdentName "USING" st then
                    fail "DELETE ... USING は未対応です" st
                else
                    ret ()

            let! whereE, st = parseWhereValue st
            let! _, st = eatOptionalSemicolon st
            return { Table = tbl; Where = whereE }, st
        }

    /// VARCHAR(32) のような型引数を読む。
    let private parseTypeArguments (st: PState) =
        let rec loop acc st =
            match parseExprValue st with
            | Error e -> err e
            | Ok(e, st) ->
                let v =
                    match e with
                    | Expr.Literal(Literal.Int n) -> int n, true
                    | _ -> 0, false

                if isSymbol "," st then
                    advance st |> loop (v :: acc)
                else
                    expectSymbol ")" st
                    |> Result.map (fun (_, st) ->
                        v :: acc
                        |> List.rev
                        |> List.choose (fun (n, ok) -> if ok then Some("n", n) else None),
                        st)

        if not (isSymbol "(" st) then
            ret ([], st)
        else
            advance st |> loop []

    let private parseColumnConstraint (acc: ColumnConstraint) (st: PState) =
        let rec loop c st =
            if isKeyword "NOT" st then
                match advance st |> expectKeyword "NULL" with
                | Error e -> err e
                | Ok(_, st) -> loop { c with NotNull = true } st
            elif isKeyword "PRIMARY" st then
                match advance st |> expectKeyword "KEY" with
                | Error e -> err e
                | Ok(_, st) ->
                    loop
                        { c with
                            PrimaryKey = true
                            NotNull = true
                        }
                        st
            elif isKeyword "UNIQUE" st then
                advance st |> loop { c with Unique = true }
            elif isKeyword "DEFAULT" st then
                match advance st |> parseExprValue with
                | Error e -> err e
                | Ok(e, st) -> loop { c with Default = Some e } st
            else
                ret (c, st)

        loop acc st

    let private parseColumnDefinition (st: PState) =
        result {
            let! name, st = parseIdent st

            let isConstraintOnly =
                match peek st with
                | Token.Keyword "NOT"
                | Token.Keyword "PRIMARY"
                | Token.Keyword "UNIQUE"
                | Token.Keyword "DEFAULT" -> true
                | _ -> false

            let! dt, st =
                if isConstraintOnly then
                    ret (DataType.Text, st)
                else
                    parseDataType st

            let! args, st = parseTypeArguments st
            let! columnConstraint, st = parseColumnConstraint ColumnConstraint.Empty st

            return
                {
                    Name = name
                    DataType = dt
                    TypeArguments = args
                    Constraint = columnConstraint
                },
                st
        }

    /// `IF [NOT] EXISTS` を読む。
    /// `NOT` は省略形（`IF EXISTS`）にも許可される SQL 実装があるため、あれば消費する。
    /// 消費しないと `CREATE TABLE IF NOT EXISTS` が `EXISTS が必要` で落ちる。
    let private parseIfExists (st: PState) =
        if isKeyword "IF" st then
            let st1 = advance st
            let st2 = if isKeyword "NOT" st1 then advance st1 else st1
            expectKeyword "EXISTS" st2 |> Result.map (fun (_, st) -> true, st)
        else
            ret (false, st)

    let private parseCreateTable (st: PState) =
        let rec body cols constrs st =
            let next =
                if isKeyword "PRIMARY" st then
                    match expectKeyword "PRIMARY" st with
                    | Error e -> err e
                    | Ok(_, st) ->
                        match expectKeyword "KEY" st with
                        | Error e -> err e
                        | Ok(_, st) ->
                            parseParenIdentList st
                            |> Result.map (fun (pkCols, st) ->
                                cols, TableConstraint.PrimaryKeyColumns pkCols :: constrs, st)
                elif isKeyword "UNIQUE" st then
                    advance st
                    |> parseParenIdentList
                    |> Result.map (fun (uqCols, st) -> cols, TableConstraint.UniqueColumns uqCols :: constrs, st)
                else
                    parseColumnDefinition st |> Result.map (fun (c, st) -> c :: cols, constrs, st)

            match next with
            | Error e -> err e
            | Ok(cols, constrs, st) ->
                if isSymbol "," st then
                    advance st |> body cols constrs
                else
                    expectSymbol ")" st
                    |> Result.map (fun (_, st) -> List.rev cols, List.rev constrs, st)

        result {
            let! _, st = expectKeyword "CREATE" st
            let! _, st = expectKeyword "TABLE" st
            let! ifNotExists, st = parseIfExists st
            let! name, st = parseIdent st
            let! _, st = expectSymbol "(" st
            let! cols, constrs, st = body [] [] st
            let! _, st = eatOptionalSemicolon st

            return
                {
                    Name = name
                    IfNotExists = ifNotExists
                    Columns = cols
                    Constraints = constrs
                },
                st
        }

    let private parseCreateIndex (st: PState) =
        result {
            let! _, st = expectKeyword "CREATE" st
            let unique = isKeyword "UNIQUE" st
            let st = if unique then advance st else st
            let! _, st = expectKeyword "INDEX" st
            let! ifNotExists, st = parseIfExists st
            let! name, st = parseIdent st
            let! _, st = expectKeyword "ON" st
            let! tbl, st = parseIdent st
            let! cols, st = parseParenIdentList st
            let! _, st = eatOptionalSemicolon st

            return
                {
                    Name = name
                    Table = tbl
                    Columns = cols
                    Unique = unique
                    IfNotExists = ifNotExists
                },
                st
        }

    let private parseDropTable (st: PState) =
        result {
            let! _, st = expectKeyword "DROP" st
            let! _, st = expectKeyword "TABLE" st
            let! ifEx, st = parseIfExists st
            let! name, st = parseIdent st
            let! _, st = eatOptionalSemicolon st
            return Statement.DropTable(name, ifEx), st
        }

    let private parseDropIndex (st: PState) =
        result {
            let! _, st = expectKeyword "DROP" st
            let! _, st = expectKeyword "INDEX" st
            let! ifEx, st = parseIfExists st
            let! name, st = parseIdent st
            let! _, st = eatOptionalSemicolon st
            return Statement.DropIndex(name, ifEx), st
        }

    let private parseOne (st: PState) =
        let nextToken =
            if st.Pos + 1 < st.Tokens.Length then
                st.Tokens.[st.Pos + 1].Token
            else
                Token.Eos

        match peek st with
        | Token.Keyword "SELECT" -> parseSelect st
        | Token.Keyword "INSERT" -> parseInsert st |> Result.map (fun (x, st) -> Statement.Insert x, st)
        | Token.Keyword "UPDATE" -> parseUpdate st |> Result.map (fun (x, st) -> Statement.Update x, st)
        | Token.Keyword "DELETE" -> parseDelete st |> Result.map (fun (x, st) -> Statement.Delete x, st)
        | Token.Keyword "CREATE" ->
            if nextToken = Token.Keyword "TABLE" then
                parseCreateTable st |> Result.map (fun (x, st) -> Statement.CreateTable x, st)
            elif nextToken = Token.Keyword "UNIQUE" || nextToken = Token.Keyword "INDEX" then
                parseCreateIndex st |> Result.map (fun (x, st) -> Statement.CreateIndex x, st)
            else
                fail "CREATE TABLE または CREATE INDEX を指定してください" st
        | Token.Keyword "DROP" ->
            if nextToken = Token.Keyword "TABLE" then
                parseDropTable st
            elif nextToken = Token.Keyword "INDEX" then
                parseDropIndex st
            else
                fail "DROP TABLE または DROP INDEX を指定してください" st
        | Token.Keyword "BEGIN" ->
            advance st
            |> eatIdentName "TRANSACTION"
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Begin, st)
        | Token.Ident name when name.ToUpperInvariant() = "EXPLAIN" ->
            let st = advance st

            match peek st with
            | Token.Keyword "SELECT" ->
                result {
                    let! sel, st = parseSelectStatementValue st
                    let! _, st = eatOptionalSemicolon st
                    return Statement.Explain sel, st
                }
            | _ -> fail "EXPLAIN は SELECT 文だけに対応しています" st
        | Token.Ident name when name.ToUpperInvariant() = "START" ->
            let st = advance st

            if isIdentName "TRANSACTION" st then
                st
                |> advance
                |> eatOptionalSemicolon
                |> Result.map (fun (_, st) -> Statement.Begin, st)
            else
                fail "START TRANSACTION が必要です" st
        | Token.Keyword "COMMIT" ->
            advance st
            |> eatIdentName "WORK"
            |> eatIdentName "TRANSACTION"
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Commit, st)
        | Token.Keyword "END" ->
            advance st
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Commit, st)
        | Token.Keyword "ROLLBACK" ->
            advance st
            |> eatIdentName "WORK"
            |> eatIdentName "TRANSACTION"
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Rollback, st)
        | Token.Ident name when Set.contains (name.ToUpperInvariant()) SetOperationWords ->
            fail "集合演算（UNION / INTERSECT / EXCEPT）は未対応です" st
        | _ -> fail "文の開始として解釈できません" st

    /// SQL テキストを 1文以上へ分解して AST にする。
    let parseStatements (sql: string) =
        let rec parseAll acc st =
            if st.Pos >= st.Tokens.Length - 1 then
                ret (List.rev acc)
            else
                match parseOne st with
                | Error e -> err e
                | Ok(statement, next) ->
                    let separated =
                        next.Pos > st.Pos && st.Tokens.[next.Pos - 1].Token = Token.Symbol ";"

                    if next.Pos < st.Tokens.Length - 1 && not separated then
                        fail "文の区切りには ; が必要です（複数文は ; で区切ってください）" next
                    else
                        parseAll (statement :: acc) next

        Lexer.tokenize sql
        >>= fun tokens ->
            parseAll
                []
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                }

    /// SQL テキストを 1つの式として解析する。
    /// DEFAULT の永続化文字列を復元するために使う。
    let parseExpression (sql: string) =
        result {
            let! tokens = Lexer.tokenize sql

            let st =
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                }

            let! e, st = parseExprValue st

            if st.Pos >= st.Tokens.Length - 1 then
                return e
            else
                return! err (DbError.create ErrorKind.SyntaxError "式の後に余分なトークンがあります")
        }
