namespace RykDb.Core

/// パーサの状態。
/// 読み取り位置は値が持つので mutable は使わない。
/// すべての構文規則は (値, 次の状態) を返す純粋な関数になる。
type private PState =
    {
        Tokens: TokenItem array
        Pos: int
        /// 式・項の入れ子の深さ。
        /// スタックを食う量に上限を課す。
        /// CPS では継続が呼ばれるまで呼び出し元が戻らないので、
        /// 括弧だけでなく演算子 1 つにつき 1 段ぶんフレームが積まれる。
        /// そこで「式 1つの入口」と「二項演算子の右辺」の両方で深さを数える。
        /// 上限を超えたらスタックを食って Overflow する前に構文エラーにする
        /// （SQLite の SQLITE_MAX_EXPR_DEPTH と同じ考え方）。
        Depth: int
    }

/// SQL 文を AST に変換する再帰下降パーサ。
/// 失敗はすべて Result<'T, DbError> として返し、例外は送らない。
/// Errors.fs の result { ... } 計算式で状態と値をまとめて束縛する。
module internal Parser =
    /// 式・項の入れ子上限。
    [<Literal>]
    let private MaxExpressionDepth = 200

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
            "CHAR", DataType.Text
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

    /// 型名として認識される綴り。
    /// 予約語扱いにはしない。
    let private TypeNames = TypeTable |> List.map fst |> Set.ofList

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

    /// 式・項の再帰に入るときに深さを 1 増やし、上限を超えたら構文エラーにする。
    /// 戻り値は「深さを増やした状態」か「Error」。
    let private enterExpr (st: PState) =
        if st.Depth >= MaxExpressionDepth then
            err (DbError.syntax (sprintf "式の入れ子が深すぎます（上限 %d）" MaxExpressionDepth) (peekPos st).Line)
        else
            Ok { st with Depth = st.Depth + 1 }

    /// 式・項の再帰から出るときに深さを 1 減らす。
    let private exitExpr (st: PState) = { st with Depth = st.Depth - 1 }

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
    /// 型名など予約でないキーワードも受け付ける。
    let private parseIdent (st: PState) =
        match peek st with
        | Token.Ident s -> ret (s, advance st)
        | Token.QuotedIdent s -> ret (s, advance st)
        | Token.Keyword k when Set.contains k TypeNames -> ret (k.ToLowerInvariant(), advance st)
        | _ -> fail "識別子が必要ですが見つかりませんでした" st

    /// 括弧で囲まれた識別子リストを読む。
    let private parseParenIdentList (st: PState) =
        let rec loop acc st =
            match parseIdent st with
            | Error e -> Error e
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
        | Token.Keyword k when Set.contains k TypeNames ->
            match dataTypeOfName k with
            | Some dt -> ret (dt, advance st)
            | None -> fail (sprintf "未知の型 '%s'" k) st
        | _ -> fail "型名が必要です" st

    // -----------------------------------------------------------------------
    // 式（優先順位の低いものから順に並べる）
    //   OR
    //   AND
    //   NOT
    //   比較 / IS NULL / IN / BETWEEN / LIKE
    //   + - ||
    //   * / %
    //   単項 -
    //   primary
    //
    // このグループだけは 継続渡し（CPS） で書く。
    // 再帰下降構文規則は定義上お互いを呼び合う
    // （parsePrimary → parseExpr → parseOr → ... → parsePrimary）ので、
    // 普通の再帰下降構文では相互再帰が末尾再帰にならない。
    // 引数を「読むもの」ではなく 「読んだ後どうするか（継続）」 にすると、
    // すべての呼び出しが `k 結果 状態` を返す形になり末尾再帰になる。
    // これにより、入れ子段数に比例するスタック消費が消える。
    // ただし入れ子上限 MaxExpressionDepth は
    // 「異常な入力を早期に検知し、既定より読みやすいエラーにする」目的で残す。
    // -----------------------------------------------------------------------

    /// 継続。
    /// 「式を 1つ読んだ後に残りをする」。
    /// 戻り値の型 'r は呼び出し側ごとに違うので（式だけ / (Expr*Expr) list / Expr option …）
    /// .generic にしてある。
    type private K<'r> = Expr -> PState -> Result<'r, DbError>

    /// 終端の継続（読んだ式をそのまま返す）。
    let private retE (e: Expr) (st: PState) = ret (e, st)

    /// 式再帰の唯一の入口。
    /// 深さを管理して、深い入れ子をスタックではなくエラーにする。
    /// 継続が呼ばれるのは「式の読み終え時」なので、その時点で深さを戻す。
    /// 構文規則は mutually recursive なので、そのままでは必ず非末尾再帰になる。
    /// 継続（continuation）を渡して「式を 1つ読んだときに、残りを k に渡す」形にすることで、
    /// 相互再帰そのものが末尾再帰になる。
    [<TailCall>]
    let rec private parseExprC<'r> (k: K<'r>) (st: PState) =
        match enterExpr st with
        | Error e -> Error e
        | Ok st -> parseOrC (fun e st -> k e (exitExpr st)) st

    and [<TailCall>] private parseExprListElemC<'r>
        (k: Expr list -> PState -> Result<'r, DbError>)
        (acc: Expr list)
        (e: Expr)
        (st: PState)
        =
        if isSymbol "," st then
            advance st |> parseExprListToParen k (e :: acc)
        else
            match expectSymbol ")" st with
            | Error e -> Error e
            | Ok(_, st) -> k (List.rev (e :: acc)) st

    and [<TailCall>] private parseExprListToParen<'r>
        (k: Expr list -> PState -> Result<'r, DbError>)
        (acc: Expr list)
        (st: PState)
        : Result<'r, DbError> =
        parseExprC (parseExprListElemC k acc) st

    and [<TailCall>] private parseCallBodyC<'r> (k: K<'r>) (name: string) (st: PState) =
        match expectSymbol "(" st with
        | Error e -> Error e
        | Ok(_, st) ->
            let upper = name.ToUpperInvariant()

            if upper = "COUNT" && isSymbol "*" st then
                match advance st |> expectSymbol ")" with
                | Error e -> Error e
                | Ok(_, st) -> k (Expr.AggCall(AggFunc.Count, false, Expr.Star)) st
            else
                let distinct = upper = "COUNT" && isKeyword "DISTINCT" st

                if distinct then advance st else st
                |> parseExprListToParen
                    (fun argList st ->
                        match AggFunc.tryParse upper with
                        | Some agg when List.length argList = 1 ->
                            k (Expr.AggCall(agg, distinct, List.head argList)) st
                        | _ -> k (Expr.FuncCall(upper, argList)) st)
                    []

    and [<TailCall>] private parseNamedPrimaryC<'r> (k: K<'r>) (name: string) (st: PState) =
        let st = advance st

        if isSymbol "." st then
            if advance st |> isSymbol "*" then
                advance st |> advance |> k (Expr.QualifiedStar name)
            else
                match advance st |> parseIdent with
                | Error e -> Error e
                | Ok(c, st) -> k (Expr.Column { Qualifier = Some name; Name = c }) st
        elif isSymbol "(" st then
            parseCallBodyC k name st
        else
            k (Expr.Name name) st

    and [<TailCall>] private parseCaseBranches<'r>
        (k: (Expr * Expr) list -> PState -> Result<'r, DbError>)
        (acc: (Expr * Expr) list)
        (st: PState)
        =
        if isKeyword "WHEN" st then
            advance st
            |> parseExprC (fun cond st ->
                match expectKeyword "THEN" st with
                | Error e -> Error e
                | Ok(_, st) -> parseExprC (fun res st -> parseCaseBranches k ((cond, res) :: acc) st) st)
        else
            k (List.rev acc) st

    and [<TailCall>] private parseCaseTailC<'r> (k: K<'r>) (bs: (Expr * Expr) list) (st: PState) =
        if List.isEmpty bs then
            fail "CASE には WHEN 句が必要です" st
        elif isKeyword "ELSE" st then
            advance st
            |> parseExprC (fun elseE st ->
                match expectKeyword "END" st with
                | Error e -> Error e
                | Ok(_, st) -> k (Expr.Case(bs, Some elseE)) st)
        else
            match expectKeyword "END" st with
            | Error e -> Error e
            | Ok(_, st) -> k (Expr.Case(bs, None)) st

    and [<TailCall>] private parseCaseC<'r> (k: K<'r>) (st: PState) =
        match expectKeyword "CASE" st with
        | Error e -> Error e
        | Ok(_, st) -> parseCaseBranches (parseCaseTailC k) [] st

    and [<TailCall>] private parsePrimaryC<'r> (k: K<'r>) (st: PState) =
        match peek st with
        | Token.IntLit v -> advance st |> k (Expr.Literal(Literal.Int v))
        | Token.RealLit v -> advance st |> k (Expr.Literal(Literal.Real v))
        | Token.StringLit s -> advance st |> k (Expr.Literal(Literal.String s))
        | Token.BlobLit b -> advance st |> k (Expr.Literal(Literal.Blob b))
        | Token.Keyword "NULL" -> advance st |> k (Expr.Literal Literal.Null)
        | Token.Keyword "TRUE" -> advance st |> k (Expr.Literal(Literal.Bool true))
        | Token.Keyword "FALSE" -> advance st |> k (Expr.Literal(Literal.Bool false))
        | Token.Keyword "CASE" -> parseCaseC k st
        | Token.Symbol "*" -> advance st |> k Expr.Star
        | Token.Symbol "(" ->
            advance st
            |> parseExprC (fun e st ->
                match expectSymbol ")" st with
                | Error e -> Error e
                | Ok(_, st) -> k e st)
        | Token.Ident name -> parseNamedPrimaryC k name st
        | Token.QuotedIdent name -> parseNamedPrimaryC k name st
        | _ -> fail "式が必要ですが見つかりませんでした" st

    and [<TailCall>] private parseUnaryC<'r> (k: K<'r>) (st: PState) =
        if isSymbol "-" st then
            match advance st |> enterExpr with
            | Error e -> Error e
            | Ok st -> parseUnaryC (fun e st -> k (Expr.Unary(UnaryOp.Neg, e)) (exitExpr st)) st
        elif isSymbol "+" st then
            match advance st |> enterExpr with
            | Error e -> Error e
            | Ok st -> parseUnaryC (fun e st -> k (Expr.Unary(UnaryOp.Pos, e)) (exitExpr st)) st
        else
            parsePrimaryC k st

    and [<TailCall>] private parseMultiplicativeLoop<'r> (k: K<'r>) (baseDepth: int) (left: Expr) (st: PState) =
        if isSymbol "*" st || isSymbol "/" st || isSymbol "%" st then
            let op =
                if isSymbol "*" st then BinaryOp.Mul
                elif isSymbol "/" st then BinaryOp.Div
                else BinaryOp.Mod

            match advance st |> enterExpr with
            | Error e -> Error e
            | Ok st ->
                parseUnaryC (fun right st -> parseMultiplicativeLoop k baseDepth (Expr.Binary(op, left, right)) st) st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseMultiplicativeC<'r> (k: K<'r>) (st: PState) =
        parseUnaryC (fun first st -> parseMultiplicativeLoop k st.Depth first st) st

    and [<TailCall>] private parseAdditiveLoop<'r> (k: K<'r>) (baseDepth: int) (left: Expr) (st: PState) =
        if isSymbol "+" st || isSymbol "-" st || isSymbol "||" st then
            let op =
                if isSymbol "+" st then BinaryOp.Add
                elif isSymbol "-" st then BinaryOp.Sub
                else BinaryOp.Concat

            match advance st |> enterExpr with
            | Error e -> Error e
            | Ok st ->
                parseMultiplicativeC
                    (fun right st -> parseAdditiveLoop k baseDepth (Expr.Binary(op, left, right)) st)
                    st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseAdditiveC<'r> (k: K<'r>) (st: PState) =
        parseMultiplicativeC (fun first st -> parseAdditiveLoop k st.Depth first st) st

    and [<TailCall>] private parseBetweenAndC<'r>
        (k: Expr -> Expr -> PState -> Result<'r, DbError>)
        (lo: Expr)
        (st: PState)
        =
        match expectKeyword "AND" st with
        | Error e -> Error e
        | Ok(_, st) -> parseAdditiveC (fun hi st -> k lo hi st) st

    and [<TailCall>] private parseBetweenBodyC<'r> (k: Expr -> Expr -> PState -> Result<'r, DbError>) (st: PState) =
        parseAdditiveC (parseBetweenAndC k) st

    and [<TailCall>] private parseInListBodyElemC<'r>
        (k: Expr list -> PState -> Result<'r, DbError>)
        (acc: Expr list)
        (e: Expr)
        (st: PState)
        =
        if isSymbol "," st then
            advance st |> parseInListBodyLoop k (e :: acc)
        else
            match expectSymbol ")" st with
            | Error e -> Error e
            | Ok(_, st) -> k (List.rev (e :: acc)) st

    and [<TailCall>] private parseInListBodyLoop<'r>
        (k: Expr list -> PState -> Result<'r, DbError>)
        (acc: Expr list)
        (st: PState)
        : Result<'r, DbError> =
        parseExprC (parseInListBodyElemC k acc) st

    and [<TailCall>] private parseInListBodyC<'r> (k: Expr list -> PState -> Result<'r, DbError>) (st: PState) =
        match expectSymbol "(" st with
        | Error e -> Error e
        | Ok(_, st) -> parseInListBodyLoop k [] st

    and [<TailCall>] private parsePredicateTail<'r> (k: K<'r>) (left: Expr) (st: PState) =
        let compareOp =
            if isSymbol "=" st then
                Some BinaryOp.Eq
            elif isSymbol "<>" st || isSymbol "!=" st then
                Some BinaryOp.Ne
            elif isSymbol "<=" st then
                Some BinaryOp.Le
            elif isSymbol ">=" st then
                Some BinaryOp.Ge
            elif isSymbol "<" st then
                Some BinaryOp.Lt
            elif isSymbol ">" st then
                Some BinaryOp.Gt
            else
                None

        match compareOp with
        | Some op ->
            advance st
            |> parseAdditiveC (fun right st -> k (Expr.Binary(op, left, right)) st)
        | None ->
            if isKeyword "IS" st then
                let afterIs = advance st
                let negated = isKeyword "NOT" afterIs
                let afterNot = eatKeyword "NOT" afterIs

                match expectKeyword "NULL" afterNot with
                | Error e -> Error e
                | Ok(_, st) -> k (Expr.IsNull(left, negated)) st
            elif isKeyword "NOT" st then
                let afterNot = advance st

                if isKeyword "IN" afterNot then
                    advance afterNot
                    |> parseInListBodyC (fun xs st -> k (Expr.InList(left, xs, true)) st)
                elif isKeyword "BETWEEN" afterNot then
                    advance afterNot
                    |> parseBetweenBodyC (fun lo hi st -> k (Expr.Between(left, lo, hi, true)) st)
                elif isKeyword "LIKE" afterNot then
                    advance afterNot |> parseAdditiveC (fun p st -> k (Expr.Like(left, p, true)) st)
                else
                    fail "NOT の後ろには IN / BETWEEN / LIKE が必要です" afterNot
            elif isKeyword "IN" st then
                advance st
                |> parseInListBodyC (fun xs st -> k (Expr.InList(left, xs, false)) st)
            elif isKeyword "BETWEEN" st then
                advance st
                |> parseBetweenBodyC (fun lo hi st -> k (Expr.Between(left, lo, hi, false)) st)
            elif isKeyword "LIKE" st then
                advance st |> parseAdditiveC (fun p st -> k (Expr.Like(left, p, false)) st)
            else
                k left st

    and [<TailCall>] private parsePredicateC<'r> (k: K<'r>) (st: PState) =
        parseAdditiveC (parsePredicateTail k) st

    and [<TailCall>] private parseNotC<'r> (k: K<'r>) (st: PState) =
        if isKeyword "NOT" st then
            match advance st |> enterExpr with
            | Error e -> Error e
            | Ok st -> parseNotC (fun e st -> k (Expr.Unary(UnaryOp.Not, e)) (exitExpr st)) st
        else
            parsePredicateC k st

    and [<TailCall>] private parseAndLoop<'r> (k: K<'r>) (baseDepth: int) (left: Expr) (st: PState) =
        if isKeyword "AND" st then
            match advance st |> enterExpr with
            | Error e -> Error e
            | Ok st ->
                parseNotC (fun right st -> parseAndLoop k baseDepth (Expr.Binary(BinaryOp.And, left, right)) st) st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseAndC<'r> (k: K<'r>) (st: PState) =
        parseNotC (fun left st -> parseAndLoop k st.Depth left st) st

    and [<TailCall>] private parseOrLoop<'r> (k: K<'r>) (baseDepth: int) (left: Expr) (st: PState) =
        if isKeyword "OR" st then
            match advance st |> enterExpr with
            | Error e -> Error e
            | Ok st -> parseAndC (fun right st -> parseOrLoop k baseDepth (Expr.Binary(BinaryOp.Or, left, right)) st) st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseOrC<'r> (k: K<'r>) (st: PState) : Result<'r, DbError> =
        parseAndC (fun left st -> parseOrLoop k st.Depth left st) st

    /// 式を 1 つ読んで返す（継続版 parseExprC の糖）。
    /// 文レベルの構文規則から使う。
    let private parseExpr (st: PState) = parseExprC retE st

    let private parseOptAlias (st: PState) =
        if isKeyword "AS" st then
            advance st |> parseIdent |> Result.map (fun (a, st) -> Some a, st)
        else
            match peek st with
            | Token.Ident s -> ret (Some s, advance st)
            | Token.QuotedIdent s -> ret (Some s, advance st)
            | _ -> ret (None, st)

    let private parseTableRef (st: PState) =
        result {
            let! name, st = parseIdent st
            let! alias, st = parseOptAlias st
            return { Name = name; Alias = alias }, st
        }

    /// 現在位置が `識別子 . *` の形かどうかを返す。
    let private isDotStar (st: PState) =
        st.Pos + 2 < st.Tokens.Length
        && st.Tokens.[st.Pos + 1].Token = Token.Symbol "."
        && st.Tokens.[st.Pos + 2].Token = Token.Symbol "*"

    let private parseSelectItem (st: PState) =
        if isSymbol "*" st then
            ret ({ Expression = Expr.Star; Alias = None }, advance st)
        elif isDotStar st then
            let name =
                match peek st with
                | Token.Ident s
                | Token.QuotedIdent s -> s
                | _ -> ""

            let st = st |> advance |> advance |> advance

            ret (
                {
                    Expression = Expr.QualifiedStar name
                    Alias = None
                },
                st
            )
        else
            match peek st with
            | Token.Ident _
            | Token.QuotedIdent _ ->
                parseExprC
                    (fun (e: Expr) (st: PState) ->
                        parseOptAlias st
                        |> Result.map (fun (alias, st) -> { Expression = e; Alias = alias }, st))
                    st
            | _ ->
                result {
                    let! e, st = parseExprC retE st
                    let! alias, st = parseOptAlias st
                    return { Expression = e; Alias = alias }, st
                }

    [<TailCall>]
    let rec private parseJoins (acc: JoinClause list) (st: PState) =
        let kind, st =
            if isKeyword "INNER" st then JoinKind.Inner, advance st
            elif isKeyword "LEFT" st then JoinKind.Left, advance st
            else JoinKind.Inner, st

        if isKeyword "JOIN" st then
            match advance st |> parseTableRef with
            | Error e -> Error e
            | Ok(tbl, st) ->
                match expectKeyword "ON" st with
                | Error e -> Error e
                | Ok(_, st) ->
                    match parseExprC retE st with
                    | Error e -> Error e
                    | Ok(on, st) -> parseJoins ({ Kind = kind; Table = tbl; On = on } :: acc) st
        else
            ret (List.rev acc, st)

    let private parseWhere (st: PState) =
        if isKeyword "WHERE" st then
            advance st |> parseExprC (fun e st -> ret (Some e, st))
        else
            ret (None, st)

    let private parseGroupBy (acc: Expr list) (st: PState) =
        let rec loop acc st =
            match parseExprC retE st with
            | Error e -> Error e
            | Ok(e, st) ->
                if isSymbol "," st then
                    advance st |> loop (e :: acc)
                else
                    ret (List.rev (e :: acc), st)

        if isKeyword "GROUP" st then
            advance st |> expectKeyword "BY" >>= fun (_, st) -> loop acc st
        else
            ret (acc, st)

    let private parseHaving (st: PState) =
        if isKeyword "HAVING" st then
            advance st |> parseExprC (fun e st -> ret (Some e, st))
        else
            ret (None, st)

    let private parseOrderBy (acc: SortKey list) (st: PState) =
        let rec loop acc st =
            match parseExprC retE st with
            | Error e -> Error e
            | Ok(e, st) ->
                let dir, st =
                    if isKeyword "DESC" st then
                        SortDirection.Descending, advance st
                    elif isKeyword "ASC" st then
                        SortDirection.Ascending, advance st
                    else
                        SortDirection.Ascending, st

                let key = { Expression = e; Direction = dir }

                if isSymbol "," st then
                    advance st |> loop (key :: acc)
                else
                    ret (List.rev (key :: acc), st)

        if isKeyword "ORDER" st then
            advance st |> expectKeyword "BY" >>= fun (_, st) -> loop acc st
        else
            ret (acc, st)

    let private parseLimitOffset (st: PState) =
        result {
            let! limit, st =
                if isKeyword "LIMIT" st then
                    advance st |> parseExprC (fun e st -> ret (Some e, st))
                else
                    ret (None, st)

            let! offset, st =
                if isKeyword "OFFSET" st then
                    advance st |> parseExprC (fun e st -> ret (Some e, st))
                else
                    ret (None, st)

            return limit, offset, st
        }

    let private parseSelect (st: PState) =
        let rec items acc st =
            match parseSelectItem st with
            | Error e -> Error e
            | Ok(it, st) ->
                if isSymbol "," st then
                    advance st |> items (it :: acc)
                else
                    ret (List.rev (it :: acc), st)

        result {
            let! _, st = expectKeyword "SELECT" st
            let distinct = isKeyword "DISTINCT" st
            let st = if distinct then advance st else st
            let! its, st = items [] st
            let! _, st = expectKeyword "FROM" st
            let! fromRef, st = parseTableRef st
            let! joins, st = parseJoins [] st
            let! whereE, st = parseWhere st
            let! groupBy, st = parseGroupBy [] st
            let! having, st = parseHaving st
            let! orderBy, st = parseOrderBy [] st
            let! limit, offset, st = parseLimitOffset st
            let! _, st = eatOptionalSemicolon st

            let sel =
                {
                    Distinct = distinct
                    Items = its
                    From = Some fromRef
                    Joins = joins
                    Where = whereE
                    GroupBy = groupBy
                    Having = having
                    OrderBy = orderBy
                    Limit = limit
                    Offset = offset
                }

            return Statement.Select sel, st
        }

    let private parseInsert (st: PState) =
        let rec one acc st =
            match parseExprC retE st with
            | Error e -> Error e
            | Ok(e, st) ->
                if isSymbol "," st then
                    advance st |> one (e :: acc)
                else
                    expectSymbol ")" st |> Result.map (fun (_, st) -> List.rev (e :: acc), st)

        let rec rows acc st =
            match expectSymbol "(" st with
            | Error e -> Error e
            | Ok(_, st) ->
                match one [] st with
                | Error e -> Error e
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

            let! _, st = expectKeyword "VALUES" st
            let! rs, st = rows [] st
            let! _, st = eatOptionalSemicolon st

            return
                {
                    Table = tbl
                    Columns = columns
                    Rows = rs
                },
                st
        }

    let private parseUpdate (st: PState) =
        let rec assigns acc st =
            match parseIdent st with
            | Error e -> Error e
            | Ok(col, st) ->
                match expectSymbol "=" st with
                | Error e -> Error e
                | Ok(_, st) ->
                    match parseExpr st with
                    | Error e -> Error e
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
            let! whereE, st = parseWhere st
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
            let! whereE, st = parseWhere st
            let! _, st = eatOptionalSemicolon st
            return { Table = tbl; Where = whereE }, st
        }

    /// VARCHAR(32) のような型引数を読む。
    let private parseTypeArguments (st: PState) =
        let rec loop acc st =
            match parseExprC retE st with
            | Error e -> Error e
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
                | Error e -> Error e
                | Ok(_, st) -> loop { c with NotNull = true } st
            elif isKeyword "PRIMARY" st then
                match advance st |> expectKeyword "KEY" with
                | Error e -> Error e
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
                match advance st |> parseExprC retE with
                | Error e -> Error e
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
    /// `NOT` は省略形（`IF EXISTS`）にも許可される SQL 実装があるため、
    /// あれば消費する。消費しないと `CREATE TABLE IF NOT EXISTS` が
    /// `EXISTS が必要` で落ちる。
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
                    | Error e -> Error e
                    | Ok(_, st) ->
                        match expectKeyword "KEY" st with
                        | Error e -> Error e
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
            | Error e -> Error e
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
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Begin, st)
        | Token.Keyword "COMMIT" ->
            advance st
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Commit, st)
        | Token.Keyword "ROLLBACK" ->
            advance st
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Rollback, st)
        | _ -> fail "文の開始として解釈できません" st

    /// SQL テキストを 1 文以上へ分解して AST にする。
    let parseStatements (sql: string) =
        let rec parseAll acc st =
            if st.Pos >= st.Tokens.Length - 1 then
                ret (List.rev acc)
            else
                match parseOne st with
                | Error e -> Error e
                | Ok(s, st) -> parseAll (s :: acc) st

        Lexer.tokenize sql
        >>= fun tokens ->
            parseAll
                []
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                    Depth = 0
                }

    /// SQL テキストを 1 つの式として解析する。
    /// DEFAULT の永続化文字列を復元するために使う。
    let parseExpression (sql: string) =
        result {
            let! tokens = Lexer.tokenize sql

            let st =
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                    Depth = 0
                }

            let! e, st = parseExpr st

            if st.Pos >= st.Tokens.Length - 1 then
                return e
            else
                return! err (DbError.create ErrorKind.SyntaxError "式の後に余分なトークンがあります")
        }
