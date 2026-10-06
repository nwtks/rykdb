namespace RykDb.Core

/// パーサの状態。読み取り位置は値が持つので mutable は使わない。
/// すべての構文規則は (値, 次の状態) を返す純粋な関数になる。
type internal PState =
    {
        Tokens: TokenItem array
        Pos: int
        /// 式・項の入れ子の深さ。スタックを食う量に上限を課す。
        /// CPS では継続が呼ばれるまで呼び出し元が戻らないので、
        /// 括弧だけでなく演算子 1 つにつき 1 段ぶんフレームが積まれる。
        /// そこで「式 1 つの入口」と「二項演算子の右辺」の両方で深さを数える。
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
    let MaxExpressionDepth = 200

    /// 型名として認識される綴り。予約語扱いにはしない。
    let private typeNames =
        Set.ofList
            [
                "INTEGER"
                "INT"
                "BIGINT"
                "REAL"
                "DOUBLE"
                "TEXT"
                "VARCHAR"
                "CHAR"
                "BLOB"
                "BOOLEAN"
                "BOOL"
                "DATE"
                "TIMESTAMP"
                "DATETIME"
                "DECIMAL"
                "NUMERIC"
                "UUID"
            ]

    // -----------------------------------------------------------------------
    // 基本操作
    // -----------------------------------------------------------------------

    let private peek (st: PState) = st.Tokens.[st.Pos].Token

    let private peekPos (st: PState) = st.Tokens.[st.Pos].Pos

    /// 1 トークン消費した次の状態を返す。末尾なら動かない。
    let private advance (st: PState) =
        if st.Pos < st.Tokens.Length - 1 then
            { st with Pos = st.Pos + 1 }
        else
            st

    /// 式・項の再帰に入るときに深さを 1 増やし、上限を超えたら構文エラーにする。
    /// 戻り値は「深さを増やした状態」か「Error」。
    let private enterExpr (st: PState) =
        if st.Depth >= MaxExpressionDepth then
            Error(DbError.syntax (sprintf "式の入れ子が深すぎます（上限 %d）" MaxExpressionDepth) (peekPos st).Line)
        else
            Ok { st with Depth = st.Depth + 1 }

    /// 式・項の再帰から出るときに深さを 1 減らす。
    let private exitExpr (st: PState) = { st with Depth = st.Depth - 1 }

    /// 現在位置を添えた構文エラーを返す。送出はしない。
    let private fail (st: PState) (msg: string) =
        Error(DbError.syntax msg (peekPos st).Line)

    let private isSymbol (st: PState) (s: string) =
        match peek st with
        | Symbol x -> x = s
        | _ -> false

    let private isKeyword (st: PState) (k: string) =
        match peek st with
        | Keyword x -> x = k
        | _ -> false

    /// 記号pletsを消費して次の状態を返す。消費できなくてもエラーにしない。
    let private eatSymbol (st: PState) (s: string) =
        if isSymbol st s then advance st else st

    let private eatKeyword (st: PState) (k: string) =
        if isKeyword st k then advance st else st

    let private expectSymbol (st: PState) (s: string) =
        if isSymbol st s then
            ret ((), advance st)
        else
            fail st (sprintf "'%s' が必要ですが見つかりませんでした" s)

    let private expectKeyword (st: PState) (k: string) =
        if isKeyword st k then
            ret ((), advance st)
        else
            fail st (sprintf "%s が必要ですが見つかりませんでした" k)

    /// 末尾の ';' は任意なので、失敗扱いにしない。
    let private eatOptionalSemicolon (st: PState) = ret ((), eatSymbol st ";")

    /// 識別子を読む。型名など予約でないキーワードも受け付ける。
    let private parseIdent (st: PState) =
        match peek st with
        | Ident s -> ret (s, advance st)
        | QuotedIdent s -> ret (s, advance st)
        | Keyword k when Set.contains k typeNames -> ret (k.ToLowerInvariant(), advance st)
        | _ -> fail st "識別子が必要ですが見つかりませんでした"

    /// 括弧で囲まれた識別子リストを読む。
    let private parseParenIdentList (st: PState) =
        expectSymbol st "("
        >>= fun (_, st) ->
            let rec loop acc (st: PState) =
                match parseIdent st with
                | Error e -> Error e
                | Ok(c, st) ->
                    if isSymbol st "," then
                        loop (c :: acc) (advance st)
                    else
                        expectSymbol st ")" |> Result.map (fun (_, st) -> List.rev (c :: acc), st)

            loop [] st

    // -----------------------------------------------------------------------
    // 型名
    // -----------------------------------------------------------------------

    let private parseDataType (st: PState) =
        match peek st with
        | Ident s ->
            let upper = s.ToUpperInvariant()

            let dataType =
                match upper with
                | "INTEGER"
                | "INT"
                | "BIGINT" -> Some DataType.Integer
                | "REAL"
                | "DOUBLE" -> Some DataType.Real
                | "TEXT"
                | "VARCHAR"
                | "CHAR" -> Some DataType.Text
                | "BLOB" -> Some DataType.Blob
                | "BOOLEAN"
                | "BOOL" -> Some DataType.Boolean
                | "DATE" -> Some DataType.Date
                | "TIMESTAMP"
                | "DATETIME" -> Some DataType.Timestamp
                | "DECIMAL"
                | "NUMERIC" -> Some DataType.Decimal
                | "UUID" -> Some DataType.Uuid
                | _ -> None

            match dataType with
            | Some dt -> ret (dt, advance st)
            | None -> fail st (sprintf "未知の型 '%s'" s)
        | Keyword k when Set.contains k typeNames ->
            let dataType =
                match k with
                | "INTEGER"
                | "INT"
                | "BIGINT" -> Some DataType.Integer
                | "REAL"
                | "DOUBLE" -> Some DataType.Real
                | "TEXT"
                | "VARCHAR"
                | "CHAR" -> Some DataType.Text
                | "BLOB" -> Some DataType.Blob
                | "BOOLEAN"
                | "BOOL" -> Some DataType.Boolean
                | "DATE" -> Some DataType.Date
                | "TIMESTAMP"
                | "DATETIME" -> Some DataType.Timestamp
                | "DECIMAL"
                | "NUMERIC" -> Some DataType.Decimal
                | "UUID" -> Some DataType.Uuid
                | _ -> None

            match dataType with
            | Some dt -> ret (dt, advance st)
            | None -> fail st (sprintf "未知の型 '%s'" k)
        | _ -> fail st "型名が必要です"

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

    /// 継続。「式を 1 つ読んだ後に残りをする」。
    /// 戻り値の型 'r は呼び出し側ごとに違うので（式だけ / (Expr*Expr) list / Expr option …）
    /// .generic にしてある。
    type private K<'r> = Expr -> PState -> Result<'r, DbError>

    /// 終端の継続（読んだ式をそのまま返す）。
    let private retE (e: Expr) (st: PState) = ret (e, st)

    /// 式再帰の唯一の入口。深さを管理して、深い入れ子をスタックではなくエラーにする。
    /// 継続が呼ばれるのは「式の読み終え時」なので、その時点で深さを戻す。
    /// 構文規則は mutually recursive なので、そのままでは必ず非末尾再帰になる。
    /// 継続（continuation）を渡して「式を 1 つ読んだときに、残りを k に渡す」形にすることで、
    /// 相互再帰そのものが末尾再帰になる。
    [<TailCall>]
    let rec private parseExprC<'r> (k: K<'r>) (st: PState) =
        match enterExpr st with
        | Error e -> Error e
        | Ok st -> parseOrC (fun e st -> k e (exitExpr st)) st

    and private parseCallBodyArgs acc (st: PState) =
        match parseExprC retE st with
        | Error e -> Error e
        | Ok(e, st) ->
            if isSymbol st "," then
                parseCallBodyArgs (e :: acc) (advance st)
            else
                match expectSymbol st ")" with
                | Error e -> Error e
                | Ok(_, st) -> ret (List.rev (e :: acc), st)

    and [<TailCall>] private parseCallBodyC<'r> (k: K<'r>) (st: PState) (name: string) =
        match expectSymbol st "(" with
        | Error e -> Error e
        | Ok(_, st) ->
            let upper = name.ToUpperInvariant()

            // COUNT(*)
            if upper = "COUNT" && isSymbol st "*" then
                match expectSymbol st "*" with
                | Error e -> Error e
                | Ok(_, st) ->
                    match expectSymbol st ")" with
                    | Error e -> Error e
                    | Ok(_, st) -> k (Expr.AggCall(AggFunc.Count, false, Expr.Star)) st
            else
                let distinct = upper = "COUNT" && isKeyword st "DISTINCT"
                let st = if distinct then advance st else st

                match parseCallBodyArgs [] st with
                | Error e -> Error e
                | Ok(argList, st) ->
                    match AggFunc.tryParse upper with
                    | Some agg when List.length argList = 1 -> k (Expr.AggCall(agg, distinct, List.head argList)) st
                    | _ -> k (Expr.FuncCall(upper, argList)) st

    and [<TailCall>] private parseNamedPrimaryC<'r> (k: K<'r>) (st: PState) (name: string) =
        let st = advance st

        if isSymbol st "." then
            if isSymbol (advance st) "*" then
                k (Expr.QualifiedStar name) (advance (advance st))
            else
                match parseIdent (advance st) with
                | Error e -> Error e
                | Ok(c, st) -> k (Expr.Column { Qualifier = Some name; Name = c }) st
        elif isSymbol st "(" then
            parseCallBodyC k st name
        else
            k (Expr.Name name) st

    and private parseCaseBranches acc (st: PState) =
        if isKeyword st "WHEN" then
            match parseExprC retE (advance st) with
            | Error e -> Error e
            | Ok(cond, st) ->
                match expectKeyword st "THEN" with
                | Error e -> Error e
                | Ok(_, st) ->
                    match parseExprC retE st with
                    | Error e -> Error e
                    | Ok(res, st) -> parseCaseBranches ((cond, res) :: acc) st
        else
            ret (List.rev acc, st)

    and [<TailCall>] private parseCaseC<'r> (k: K<'r>) (st: PState) =
        match expectKeyword st "CASE" with
        | Error e -> Error e
        | Ok(_, st) ->
            match parseCaseBranches [] st with
            | Error e -> Error e
            | Ok(bs, st) ->
                if List.isEmpty bs then
                    fail st "CASE には WHEN 句が必要です"
                else if isKeyword st "ELSE" then
                    parseExprC
                        (fun elseE st ->
                            match expectKeyword st "END" with
                            | Error e -> Error e
                            | Ok(_, st) -> k (Expr.Case(bs, Some elseE)) st)
                        (advance st)
                else
                    match expectKeyword st "END" with
                    | Error e -> Error e
                    | Ok(_, st) -> k (Expr.Case(bs, None)) st

    and [<TailCall>] private parsePrimaryC<'r> (k: K<'r>) (st: PState) =
        match peek st with
        | IntLit v -> k (Expr.Literal(Literal.Int v)) (advance st)
        | RealLit v -> k (Expr.Literal(Literal.Real v)) (advance st)
        | StringLit s -> k (Expr.Literal(Literal.String s)) (advance st)
        | BlobLit b -> k (Expr.Literal(Literal.Blob b)) (advance st)
        | Keyword "NULL" -> k (Expr.Literal Literal.Null) (advance st)
        | Keyword "TRUE" -> k (Expr.Literal(Literal.Bool true)) (advance st)
        | Keyword "FALSE" -> k (Expr.Literal(Literal.Bool false)) (advance st)
        | Keyword "CASE" -> parseCaseC k st
        | Symbol "*" -> k Expr.Star (advance st)
        | Symbol "(" ->
            parseExprC
                (fun e st ->
                    match expectSymbol st ")" with
                    | Error e -> Error e
                    | Ok(_, st) -> k e st)
                (advance st)
        | Ident name -> parseNamedPrimaryC k st name
        | QuotedIdent name -> parseNamedPrimaryC k st name
        | _ -> fail st "式が必要ですが見つかりませんでした"

    and [<TailCall>] private parseUnaryC<'r> (k: K<'r>) (st: PState) =
        if isSymbol st "-" then
            match enterExpr (advance st) with
            | Error e -> Error e
            | Ok st -> parseUnaryC (fun e st -> k (Expr.Unary(UnaryOp.Neg, e)) (exitExpr st)) st
        elif isSymbol st "+" then
            match enterExpr (advance st) with
            | Error e -> Error e
            | Ok st -> parseUnaryC (fun e st -> k (Expr.Unary(UnaryOp.Pos, e)) (exitExpr st)) st
        else
            parsePrimaryC k st

    and [<TailCall>] private parseMultiplicativeLoop<'r> (k: K<'r>) left baseDepth (st: PState) =
        if isSymbol st "*" || isSymbol st "/" || isSymbol st "%" then
            let op =
                if isSymbol st "*" then BinaryOp.Mul
                elif isSymbol st "/" then BinaryOp.Div
                else BinaryOp.Mod

            match enterExpr (advance st) with
            | Error e -> Error e
            | Ok st ->
                parseUnaryC (fun right st -> parseMultiplicativeLoop k (Expr.Binary(op, left, right)) baseDepth st) st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseMultiplicativeC<'r> (k: K<'r>) (st: PState) =
        parseUnaryC (fun first st -> parseMultiplicativeLoop k first st.Depth st) st

    and [<TailCall>] private parseAdditiveLoop<'r> (k: K<'r>) left baseDepth (st: PState) =
        if isSymbol st "+" || isSymbol st "-" || isSymbol st "||" then
            let op =
                if isSymbol st "+" then BinaryOp.Add
                elif isSymbol st "-" then BinaryOp.Sub
                else BinaryOp.Concat

            match enterExpr (advance st) with
            | Error e -> Error e
            | Ok st ->
                parseMultiplicativeC
                    (fun right st -> parseAdditiveLoop k (Expr.Binary(op, left, right)) baseDepth st)
                    st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseAdditiveC<'r> (k: K<'r>) (st: PState) =
        parseMultiplicativeC (fun first st -> parseAdditiveLoop k first st.Depth st) st

    and [<TailCall>] private parseBetweenAndC<'r>
        (k: Expr -> Expr -> PState -> Result<'r, DbError>)
        (lo: Expr)
        (st: PState)
        =
        match expectKeyword st "AND" with
        | Error e -> Error e
        | Ok(_, st) -> parseAdditiveC (fun hi st -> k lo hi st) st

    and [<TailCall>] private parseBetweenBodyC<'r> (k: Expr -> Expr -> PState -> Result<'r, DbError>) (st: PState) =
        parseAdditiveC (parseBetweenAndC k) st

    and private parseInListBodyLoop acc (st: PState) =
        match parseExprC retE st with
        | Error e -> Error e
        | Ok(e, st) ->
            if isSymbol st "," then
                parseInListBodyLoop (e :: acc) (advance st)
            else
                match expectSymbol st ")" with
                | Error e -> Error e
                | Ok(_, st) -> ret (List.rev (e :: acc), st)

    and [<TailCall>] private parseInListBodyC<'r> (k: Expr list -> PState -> Result<'r, DbError>) (st: PState) =
        match expectSymbol st "(" with
        | Error e -> Error e
        | Ok(_, st) ->
            match parseInListBodyLoop [] st with
            | Error e -> Error e
            | Ok(xs, st) -> k xs st

    and [<TailCall>] private parsePredicateTail<'r> (k: K<'r>) (left: Expr) (st: PState) =
        let compareOp =
            if isSymbol st "=" then
                Some BinaryOp.Eq
            elif isSymbol st "<>" || isSymbol st "!=" then
                Some BinaryOp.Ne
            elif isSymbol st "<=" then
                Some BinaryOp.Le
            elif isSymbol st ">=" then
                Some BinaryOp.Ge
            elif isSymbol st "<" then
                Some BinaryOp.Lt
            elif isSymbol st ">" then
                Some BinaryOp.Gt
            else
                None

        match compareOp with
        | Some op -> parseAdditiveC (fun right st -> k (Expr.Binary(op, left, right)) st) (advance st)
        | None ->
            if isKeyword st "IS" then
                let afterIs = advance st
                let negated = isKeyword afterIs "NOT"
                let afterNot = eatKeyword afterIs "NOT"

                match expectKeyword afterNot "NULL" with
                | Error e -> Error e
                | Ok(_, st) -> k (Expr.IsNull(left, negated)) st
            elif isKeyword st "NOT" then
                let afterNot = advance st

                if isKeyword afterNot "IN" then
                    parseInListBodyC (fun xs st -> k (Expr.InList(left, xs, true)) st) (advance afterNot)
                elif isKeyword afterNot "BETWEEN" then
                    parseBetweenBodyC (fun lo hi st -> k (Expr.Between(left, lo, hi, true)) st) (advance afterNot)
                elif isKeyword afterNot "LIKE" then
                    parseAdditiveC (fun p st -> k (Expr.Like(left, p, true)) st) (advance afterNot)
                else
                    fail afterNot "NOT の後ろには IN / BETWEEN / LIKE が必要です"
            elif isKeyword st "IN" then
                parseInListBodyC (fun xs st -> k (Expr.InList(left, xs, false)) st) (advance st)
            elif isKeyword st "BETWEEN" then
                parseBetweenBodyC (fun lo hi st -> k (Expr.Between(left, lo, hi, false)) st) (advance st)
            elif isKeyword st "LIKE" then
                parseAdditiveC (fun p st -> k (Expr.Like(left, p, false)) st) (advance st)
            else
                k left st

    and [<TailCall>] private parsePredicateC<'r> (k: K<'r>) (st: PState) =
        parseAdditiveC (parsePredicateTail k) st

    and [<TailCall>] private parseNotC<'r> (k: K<'r>) (st: PState) =
        if isKeyword st "NOT" then
            match enterExpr (advance st) with
            | Error e -> Error e
            | Ok st -> parseNotC (fun e st -> k (Expr.Unary(UnaryOp.Not, e)) (exitExpr st)) st
        else
            parsePredicateC k st

    and [<TailCall>] private parseAndLoop<'r> (k: K<'r>) left baseDepth (st: PState) =
        if isKeyword st "AND" then
            match enterExpr (advance st) with
            | Error e -> Error e
            | Ok st ->
                parseNotC (fun right st -> parseAndLoop k (Expr.Binary(BinaryOp.And, left, right)) baseDepth st) st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseAndC<'r> (k: K<'r>) (st: PState) =
        parseNotC (fun left st -> parseAndLoop k left st.Depth st) st

    and [<TailCall>] private parseOrLoop<'r> (k: K<'r>) left baseDepth (st: PState) =
        if isKeyword st "OR" then
            match enterExpr (advance st) with
            | Error e -> Error e
            | Ok st -> parseAndC (fun right st -> parseOrLoop k (Expr.Binary(BinaryOp.Or, left, right)) baseDepth st) st
        else
            k left { st with Depth = baseDepth }

    and [<TailCall>] private parseOrC<'r> (k: K<'r>) (st: PState) : Result<'r, DbError> =
        parseAndC (fun left st -> parseOrLoop k left st.Depth st) st

    /// 式を 1 つ読んで返す（継続版 parseExprC の糖）。
    /// 文レベルの構文規則から使う。
    let private parseExpr (st: PState) = parseExprC retE st

    // -----------------------------------------------------------------------
    // SELECT
    // -----------------------------------------------------------------------

    let private parseOptAlias st =
        if isKeyword st "AS" then
            parseIdent (advance st) |> Result.map (fun (a, st) -> Some a, st)
        else
            match peek st with
            | Ident s -> ret (Some s, advance st)
            | QuotedIdent s -> ret (Some s, advance st)
            | _ -> ret (None, st)

    let private parseTableRef st =
        parseIdent st
        >>= fun (name, st) ->
            parseOptAlias st
            |> Result.map (fun (alias, st) -> { Name = name; Alias = alias }, st)

    /// 現在位置が `識別子 . *` の形かどうかを返す。
    let private isDotStar st =
        st.Pos + 2 < st.Tokens.Length
        && st.Tokens.[st.Pos + 1].Token = Symbol "."
        && st.Tokens.[st.Pos + 2].Token = Symbol "*"

    let private parseSelectItem st =
        if isSymbol st "*" then
            ret ({ Expression = Expr.Star; Alias = None }, advance st)
        elif isDotStar st then
            let name =
                match peek st with
                | Ident s
                | QuotedIdent s -> s
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
            | Ident name
            | QuotedIdent name ->
                let st = advance st

                let finish (e: Expr) (st: PState) : Result<SelectItem * PState, DbError> =
                    parseOptAlias st
                    |> Result.map (fun (alias, st) -> { Expression = e; Alias = alias }, st)

                if isSymbol st "(" then
                    parseCallBodyC finish st name
                elif isSymbol st "." then
                    let st1 = advance st

                    match peek st1 with
                    | Ident colName
                    | QuotedIdent colName ->
                        let st2 = advance st1

                        let e =
                            Expr.Column
                                {
                                    Qualifier = Some name
                                    Name = colName
                                }

                        finish e st2
                    | _ ->
                        let back = st |> fun s -> { s with Pos = s.Pos - 1 }
                        parseExprC (fun (e: Expr) (s: PState) -> ret ({ Expression = e; Alias = None }, s)) back
                else
                    finish (Expr.Name name) st
            | _ ->
                parseExprC retE st
                >>= fun (e, st) ->
                    parseOptAlias st
                    |> Result.map (fun (alias, st) -> { Expression = e; Alias = alias }, st)

    [<TailCall>]
    let rec private parseJoins st acc =
        let kind =
            if isKeyword st "INNER" then JoinKind.Inner
            elif isKeyword st "LEFT" then JoinKind.Left
            else JoinKind.Inner

        let st =
            if isKeyword st "INNER" || isKeyword st "LEFT" then
                advance st
            else
                st

        if isKeyword st "JOIN" then
            parseTableRef (advance st)
            >>= fun (tbl, st) ->
                expectKeyword st "ON"
                >>= fun (_, st) ->
                    parseExprC retE st
                    >>= fun (on, st) -> parseJoins st ({ Kind = kind; Table = tbl; On = on } :: acc)
        else
            ret (List.rev acc, st)

    let private parseWhere st =
        if isKeyword st "WHERE" then
            parseExprC (fun e st -> ret (Some e, st)) (advance st)
        else
            ret (None, st)

    let private parseGroupBy st acc =
        if isKeyword st "GROUP" then
            expectKeyword (advance st) "BY"
            >>= fun (_, st) ->
                let rec loop acc (st: PState) =
                    match parseExprC retE st with
                    | Error e -> Error e
                    | Ok(e, st) ->
                        if isSymbol st "," then
                            loop (e :: acc) (advance st)
                        else
                            ret (List.rev (e :: acc), st)

                loop acc st
        else
            ret (acc, st)

    let private parseHaving st =
        if isKeyword st "HAVING" then
            parseExprC (fun e st -> ret (Some e, st)) (advance st)
        else
            ret (None, st)

    let private parseOrderBy st acc =
        if isKeyword st "ORDER" then
            expectKeyword (advance st) "BY"
            >>= fun (_, st) ->
                let rec loop acc (st: PState) =
                    match parseExprC retE st with
                    | Error e -> Error e
                    | Ok(e, st) ->
                        let dir =
                            if isKeyword st "DESC" then SortDirection.Descending
                            elif isKeyword st "ASC" then SortDirection.Ascending
                            else SortDirection.Ascending

                        let st =
                            if isKeyword st "DESC" || isKeyword st "ASC" then
                                advance st
                            else
                                st

                        let key: SortKey = { Expression = e; Direction = dir }

                        if isSymbol st "," then
                            loop (key :: acc) (advance st)
                        else
                            ret (List.rev (key :: acc), st)

                loop acc st
        else
            ret (acc, st)

    let private parseLimitOffset st =
        let limit =
            if isKeyword st "LIMIT" then
                parseExprC (fun e st -> ret (Some e, st)) (advance st)
            else
                ret (None, st)

        limit
        >>= fun (limit, st) ->
            let offset =
                if isKeyword st "OFFSET" then
                    parseExprC (fun e st -> ret (Some e, st)) (advance st)
                else
                    ret (None, st)

            offset |> Result.map (fun (offset, st) -> limit, offset, st)

    let private parseSelect st =
        expectKeyword st "SELECT"
        >>= fun (_, st) ->
            let distinct = isKeyword st "DISTINCT"
            let st = if distinct then advance st else st

            let rec items acc (st: PState) =
                match parseSelectItem st with
                | Error e -> Error e
                | Ok(it, st) ->
                    if isSymbol st "," then
                        items (it :: acc) (advance st)
                    else
                        ret (List.rev (it :: acc), st)

            items [] st
            >>= fun (its, st) ->
                expectKeyword st "FROM"
                >>= fun (_, st) ->
                    parseTableRef st
                    >>= fun (fromRef, st) ->
                        parseJoins st []
                        >>= fun (joins, st) ->
                            parseWhere st
                            >>= fun (whereE, st) ->
                                parseGroupBy st []
                                >>= fun (groupBy, st) ->
                                    parseHaving st
                                    >>= fun (having, st) ->
                                        parseOrderBy st []
                                        >>= fun (orderBy, st) ->
                                            parseLimitOffset st
                                            >>= fun (limit, offset, st) ->
                                                eatOptionalSemicolon st
                                                |> Result.map (fun (_, st) ->
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

                                                    Statement.Select sel, st)

    // -----------------------------------------------------------------------
    // INSERT / UPDATE / DELETE
    // -----------------------------------------------------------------------

    let private parseInsert st =
        expectKeyword st "INSERT"
        >>= fun (_, st) ->
            expectKeyword st "INTO"
            >>= fun (_, st) ->
                parseTableRef st
                >>= fun (tbl, st) ->
                    let hasColumns = isSymbol st "("

                    let columnsResult =
                        if hasColumns then
                            parseParenIdentList st |> Result.map (fun (cols, st) -> Some cols, st)
                        else
                            ret (None, st)

                    columnsResult
                    >>= fun (columns, st) ->
                        expectKeyword st "VALUES"
                        >>= fun (_, st) ->
                            let rec one acc (st: PState) =
                                match parseExprC retE st with
                                | Error e -> Error e
                                | Ok(e, st) ->
                                    if isSymbol st "," then
                                        one (e :: acc) (advance st)
                                    else
                                        expectSymbol st ")" |> Result.map (fun (_, st) -> List.rev (e :: acc), st)

                            let rec rows acc (st: PState) =
                                match expectSymbol st "(" with
                                | Error e -> Error e
                                | Ok(_, st) ->
                                    match one [] st with
                                    | Error e -> Error e
                                    | Ok(r, st) ->
                                        if isSymbol st "," then
                                            rows (r :: acc) (advance st)
                                        else
                                            ret (List.rev (r :: acc), st)

                            rows [] st
                            >>= fun (rs, st) ->
                                eatOptionalSemicolon st
                                |> Result.map (fun (_, st) ->
                                    {
                                        Table = tbl
                                        Columns = columns
                                        Rows = rs
                                    },
                                    st)

    let private parseUpdate st =
        expectKeyword st "UPDATE"
        >>= fun (_, st) ->
            parseTableRef st
            >>= fun (tbl, st) ->
                expectKeyword st "SET"
                >>= fun (_, st) ->
                    let rec assigns acc (st: PState) =
                        match parseIdent st with
                        | Error e -> Error e
                        | Ok(col, st) ->
                            expectSymbol st "="
                            >>= fun (_, st) ->
                                parseExpr st
                                >>= fun (e, st) ->
                                    if isSymbol st "," then
                                        assigns ((col, e) :: acc) (advance st)
                                    else
                                        ret (List.rev ((col, e) :: acc), st)

                    assigns [] st
                    >>= fun (asg, st) ->
                        parseWhere st
                        >>= fun (whereE, st) ->
                            eatOptionalSemicolon st
                            |> Result.map (fun (_, st) ->
                                {
                                    Table = tbl
                                    Assignments = asg
                                    Where = whereE
                                },
                                st)

    let private parseDelete st =
        expectKeyword st "DELETE"
        >>= fun (_, st) ->
            expectKeyword st "FROM"
            >>= fun (_, st) ->
                parseTableRef st
                >>= fun (tbl, st) ->
                    parseWhere st
                    >>= fun (whereE, st) ->
                        eatOptionalSemicolon st
                        |> Result.map (fun (_, st) -> { Table = tbl; Where = whereE }, st)

    // -----------------------------------------------------------------------
    // DDL
    // -----------------------------------------------------------------------

    /// VARCHAR(32) のような型引数を読む。
    let private parseTypeArguments (st: PState) =
        if not (isSymbol st "(") then
            ret ([], st)
        else
            let rec loop acc (st: PState) =
                match parseExprC retE st with
                | Error e -> Error e
                | Ok(e, st) ->
                    let v =
                        match e with
                        | Expr.Literal(Literal.Int n) -> int n, true
                        | _ -> 0, false

                    if isSymbol st "," then
                        loop (v :: acc) (advance st)
                    else
                        expectSymbol st ")"
                        |> Result.map (fun (_, st) ->
                            List.rev acc |> List.choose (fun (n, ok) -> if ok then Some("n", n) else None), st)

            loop [] (advance st)

    let private parseColumnConstraint st acc =
        let rec loop (c: ColumnConstraint) (st: PState) =
            if isKeyword st "NOT" then
                match expectKeyword (advance st) "NULL" with
                | Error e -> Error e
                | Ok(_, st) -> loop { c with NotNull = true } st
            elif isKeyword st "PRIMARY" then
                match expectKeyword (advance st) "KEY" with
                | Error e -> Error e
                | Ok(_, st) ->
                    loop
                        { c with
                            PrimaryKey = true
                            NotNull = true
                        }
                        st
            elif isKeyword st "UNIQUE" then
                loop { c with Unique = true } (advance st)
            elif isKeyword st "DEFAULT" then
                match parseExprC retE (advance st) with
                | Error e -> Error e
                | Ok(e, st) -> loop { c with Default = Some e } st
            else
                ret (c, st)

        loop acc st

    let private parseColumnDefinition st =
        parseIdent st
        >>= fun (name, st) ->
            let isConstraintOnly =
                match peek st with
                | Keyword "NOT"
                | Keyword "PRIMARY"
                | Keyword "UNIQUE"
                | Keyword "DEFAULT" -> true
                | _ -> false

            let typeResult =
                if isConstraintOnly then
                    ret (DataType.Text, st)
                else
                    parseDataType st

            typeResult
            >>= fun (dt, st) ->
                parseTypeArguments st
                >>= fun (args, st) ->
                    parseColumnConstraint st ColumnConstraint.Empty
                    |> Result.map (fun (columnConstraint, st) ->
                        {
                            Name = name
                            DataType = dt
                            TypeArguments = args
                            Constraint = columnConstraint
                        },
                        st)

    /// `IF [NOT] EXISTS` を読む。
    /// `NOT` は省略形（`IF EXISTS`）にも許可される SQL 実装があるため、
    /// あれば消費する。消費しないと `CREATE TABLE IF NOT EXISTS` が
    /// `EXISTS が必要` で落ちる。
    let private parseIfExists st =
        if isKeyword st "IF" then
            let st1 = advance st
            let st2 = if isKeyword st1 "NOT" then advance st1 else st1
            expectKeyword st2 "EXISTS" |> Result.map (fun (_, st) -> true, st)
        else
            ret (false, st)

    let private parseCreateTable st =
        expectKeyword st "CREATE"
        >>= fun (_, st) ->
            expectKeyword st "TABLE"
            >>= fun (_, st) ->
                parseIfExists st
                >>= fun (ifNotExists, st) ->
                    parseIdent st
                    >>= fun (name, st) ->
                        expectSymbol st "("
                        >>= fun (_, st) ->
                            let rec body cols constrs (st: PState) =
                                let next =
                                    if isKeyword st "PRIMARY" then
                                        match expectKeyword st "PRIMARY" with
                                        | Error e -> Error e
                                        | Ok(_, st) ->
                                            match expectKeyword st "KEY" with
                                            | Error e -> Error e
                                            | Ok(_, st) ->
                                                parseParenIdentList st
                                                |> Result.map (fun (pkCols, st) ->
                                                    cols, TableConstraint.PrimaryKeyColumns pkCols :: constrs, st)
                                    elif isKeyword st "UNIQUE" then
                                        parseParenIdentList (advance st)
                                        |> Result.map (fun (uqCols, st) ->
                                            cols, TableConstraint.UniqueColumns uqCols :: constrs, st)
                                    else
                                        parseColumnDefinition st |> Result.map (fun (c, st) -> c :: cols, constrs, st)

                                match next with
                                | Error e -> Error e
                                | Ok(cols, constrs, st) ->
                                    if isSymbol st "," then
                                        body cols constrs (advance st)
                                    else
                                        expectSymbol st ")"
                                        |> Result.map (fun (_, st) -> List.rev cols, List.rev constrs, st)

                            body [] [] st
                            >>= fun (cols, constrs, st) ->
                                eatOptionalSemicolon st
                                |> Result.map (fun (_, st) ->
                                    {
                                        Name = name
                                        IfNotExists = ifNotExists
                                        Columns = cols
                                        Constraints = constrs
                                    },
                                    st)

    let private parseCreateIndex st =
        expectKeyword st "CREATE"
        >>= fun (_, st) ->
            let unique = isKeyword st "UNIQUE"
            let st = if unique then advance st else st

            expectKeyword st "INDEX"
            >>= fun (_, st) ->
                parseIdent st
                >>= fun (name, st) ->
                    expectKeyword st "ON"
                    >>= fun (_, st) ->
                        parseIdent st
                        >>= fun (tbl, st) ->
                            parseParenIdentList st
                            >>= fun (cols, st) ->
                                eatOptionalSemicolon st
                                |> Result.map (fun (_, st) ->
                                    {
                                        Name = name
                                        Table = tbl
                                        Columns = cols
                                        Unique = unique
                                    },
                                    st)

    let private parseDropTable st =
        expectKeyword st "DROP"
        >>= fun (_, st) ->
            expectKeyword st "TABLE"
            >>= fun (_, st) ->
                parseIfExists st
                >>= fun (ifEx, st) ->
                    parseIdent st
                    >>= fun (name, st) ->
                        eatOptionalSemicolon st
                        |> Result.map (fun (_, st) -> Statement.DropTable(name, ifEx), st)

    let private parseDropIndex st =
        expectKeyword st "DROP"
        >>= fun (_, st) ->
            expectKeyword st "INDEX"
            >>= fun (_, st) ->
                parseIfExists st
                >>= fun (ifEx, st) ->
                    parseIdent st
                    >>= fun (name, st) ->
                        eatOptionalSemicolon st
                        |> Result.map (fun (_, st) -> Statement.DropIndex(name, ifEx), st)

    // -----------------------------------------------------------------------
    // 入口
    // -----------------------------------------------------------------------

    let private parseOne st =
        let nextToken =
            if st.Pos + 1 < st.Tokens.Length then
                st.Tokens.[st.Pos + 1].Token
            else
                Eof

        match peek st with
        | Keyword "SELECT" -> parseSelect st
        | Keyword "INSERT" -> parseInsert st |> Result.map (fun (x, st) -> Statement.Insert x, st)
        | Keyword "UPDATE" -> parseUpdate st |> Result.map (fun (x, st) -> Statement.Update x, st)
        | Keyword "DELETE" -> parseDelete st |> Result.map (fun (x, st) -> Statement.Delete x, st)
        | Keyword "CREATE" ->
            if nextToken = Keyword "TABLE" then
                parseCreateTable st |> Result.map (fun (x, st) -> Statement.CreateTable x, st)
            elif nextToken = Keyword "UNIQUE" || nextToken = Keyword "INDEX" then
                parseCreateIndex st |> Result.map (fun (x, st) -> Statement.CreateIndex x, st)
            else
                fail st "CREATE TABLE または CREATE INDEX を指定してください"
        | Keyword "DROP" ->
            if nextToken = Keyword "TABLE" then parseDropTable st
            elif nextToken = Keyword "INDEX" then parseDropIndex st
            else fail st "DROP TABLE または DROP INDEX を指定してください"
        | Keyword "BEGIN" ->
            eatOptionalSemicolon (advance st)
            |> Result.map (fun (_, st) -> Statement.Begin, st)
        | Keyword "COMMIT" ->
            eatOptionalSemicolon (advance st)
            |> Result.map (fun (_, st) -> Statement.Commit, st)
        | Keyword "ROLLBACK" ->
            eatOptionalSemicolon (advance st)
            |> Result.map (fun (_, st) -> Statement.Rollback, st)
        | _ -> fail st "文の開始として解釈できません"

    /// SQL テキストを 1 文以上へ分解して AST にする。
    let parseStatements (sql: string) =
        Lexer.tokenize sql
        >>= fun tokens ->
            let rec go st acc =
                if st.Pos >= st.Tokens.Length - 1 then
                    ret (List.rev acc)
                else
                    match parseOne st with
                    | Error e -> Error e
                    | Ok(s, st) -> go st (s :: acc)

            go
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                    Depth = 0
                }
                []

    /// SQL テキストを 1 文として解析する。複数文はエラー。
    let parseStatement (sql: string) =
        parseStatements sql
        >>= fun stmts ->
            match stmts with
            | [ s ] -> ret s
            | [] -> Error(DbError.syntax "文がありません" 1)
            | _ -> Error(DbError.syntax "複数の文が渡されました（1 文ずつ実行してください）" 1)
