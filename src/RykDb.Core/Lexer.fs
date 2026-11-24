namespace RykDb.Core

[<RequireQualifiedAccess>]
type internal Token =
    /// 予約語でない識別子
    | Ident of string
    /// "..." で囲まれた識別子
    | QuotedIdent of string
    /// 予約語（すべて大文字で正規化されている）
    | Keyword of string
    | IntLit of int64
    | RealLit of float
    | StringLit of string
    /// X'ABCD'
    | BlobLit of byte[]
    /// `?` の位置パラメータ。
    /// 値は SQL テキスト中の出現順（0 始まり）。
    /// 実際の値への置き換えはパーサが行う。
    | Param of int
    /// 記号・演算子
    | Symbol of string
    | Eos

[<Struct>]
type internal TokenItem = { Token: Token; Pos: Position }

/// 字句解析器の位置。
/// 値は不変で、前進すると新しい位置を返す。
[<Struct>]
type internal LexState =
    {
        Index: int
        Line: int
        Column: int
    }

    member this.Position =
        {
            Offset = this.Index
            Line = this.Line
            Column = this.Column
        }

module internal Lexer =
    /// 予約語の一覧。
    /// パーサはここで定義した綴りだけを受け付ける。
    /// TRUE / FALSE も Keyword として字句解析する（パーサ側で真偽値リテラル扱い）。
    let private KeywordSpellings =
        [
            "SELECT"
            "FROM"
            "WHERE"
            "INSERT"
            "INTO"
            "VALUES"
            "UPDATE"
            "SET"
            "DELETE"
            "CREATE"
            "TABLE"
            "INDEX"
            "DROP"
            "ON"
            "AND"
            "OR"
            "NOT"
            "NULL"
            "IS"
            "IN"
            "BETWEEN"
            "LIKE"
            "GROUP"
            "BY"
            "HAVING"
            "ORDER"
            "ASC"
            "DESC"
            "LIMIT"
            "OFFSET"
            "JOIN"
            "INNER"
            "LEFT"
            "AS"
            "DISTINCT"
            "BEGIN"
            "COMMIT"
            "ROLLBACK"
            "PRIMARY"
            "KEY"
            "UNIQUE"
            "DEFAULT"
            "CASE"
            "WHEN"
            "THEN"
            "ELSE"
            "END"
            "EXISTS"
            "IF"
            "TRUE"
            "FALSE"
        ]

    /// 綴り → 正規化した綴り（大文字）の表。
    /// 大文字小文字を無視して引き、識別子ごとの ToUpperInvariant を避ける。
    let private makeKeywordMap (comparer: System.StringComparer) =
        let pairs =
            KeywordSpellings
            |> List.map (fun k -> System.Collections.Generic.KeyValuePair(k, k))
            |> Array.ofList

        System.Collections.Frozen.FrozenDictionary.Create<string, string>(
            comparer,
            System.ReadOnlySpan<System.Collections.Generic.KeyValuePair<string, string>> pairs
        )

    let private Keywords = makeKeywordMap System.StringComparer.OrdinalIgnoreCase

    /// 非 ASCII を含む識別子は ToUpperInvariant してから引く
    /// （ſ のような文字の大文字化で予約語に一致する挙動を保つ）。
    let private KeywordsUpper = makeKeywordMap System.StringComparer.Ordinal

    let private advance (input: string) (s: LexState) =
        if s.Index < input.Length && input.[s.Index] = '\n' then
            {
                Index = s.Index + 1
                Line = s.Line + 1
                Column = 1
            }
        else
            {
                Index = s.Index + 1
                Line = s.Line
                Column = s.Column + 1
            }

    /// 改行を含まないと分かっている範囲をまとめて進める。
    /// 改行を含む範囲に使うと行・列の位置がずれるので、必ず呼び出し側で保証すること。
    let private advanceBy (count: int) (s: LexState) =
        {
            Index = s.Index + count
            Line = s.Line
            Column = s.Column + count
        }

    /// 空白とコメントを読み飛ばす。
    /// 入れ子のブロックコメントに対応。
    [<TailCall>]
    let rec private skipLine (input: string) (n: int) (s: LexState) =
        if s.Index < n && input.[s.Index] <> '\n' then
            skipLine input n (advance input s)
        else
            s

    [<TailCall>]
    let rec private skipBlockComment (input: string) (n: int) (depth: int) (s: LexState) =
        if depth <= 0 then
            ret s
        elif s.Index >= n then
            err (DbError.syntax "unclosed comment" s.Position)
        elif s.Index + 1 < n && input.[s.Index] = '/' && input.[s.Index + 1] = '*' then
            skipBlockComment input n (depth + 1) (advance input (advance input s))
        elif s.Index + 1 < n && input.[s.Index] = '*' && input.[s.Index + 1] = '/' then
            skipBlockComment input n (depth - 1) (advance input (advance input s))
        else
            skipBlockComment input n depth (advance input s)

    [<TailCall>]
    let rec private skipTriviaLoop (input: string) (n: int) (s: LexState) =
        if s.Index >= n then
            ret s
        else
            let c = input.[s.Index]

            if c = ' ' || c = '\t' || c = '\r' || c = '\n' then
                skipTriviaLoop input n (advance input s)
            elif c = '-' && s.Index + 1 < n && input.[s.Index + 1] = '-' then
                skipTriviaLoop input n (skipLine input n s)
            elif c = '/' && s.Index + 1 < n && input.[s.Index + 1] = '*' then
                let s' = advance input (advance input s)

                match skipBlockComment input n 1 s' with
                | Ok st -> skipTriviaLoop input n st
                | Error e -> err e
            else
                ret s

    let private skipTrivia (input: string) = skipTriviaLoop input input.Length

    let private isIdentStart (c: char) = System.Char.IsLetter c || c = '_'

    let private isIdentPart (c: char) =
        System.Char.IsLetterOrDigit c || c = '_'

    let private hexDigitValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
        else -1

    /// 予約語は ASCII だけなので、非 ASCII を含む識別子は低速経路に回す。
    [<TailCall>]
    let rec private isAscii (s: string) (i: int) =
        if i >= s.Length then true
        elif System.Char.IsAscii s.[i] then isAscii s (i + 1)
        else false

    /// 識別子の終端（最初の識別子でない文字）を返す。
    [<TailCall>]
    let rec private scanIdentEnd (input: string) (n: int) (i: int) =
        if i < n && isIdentPart input.[i] then
            scanIdentEnd input n (i + 1)
        else
            i

    /// 数字の連なりを読む。
    [<TailCall>]
    let rec private scanDigits (input: string) (n: int) (s: LexState) =
        if s.Index < n && System.Char.IsDigit input.[s.Index] then
            scanDigits input n (advanceBy 1 s)
        else
            s

    /// 区切り文字まで走査する。
    /// 二重区切りはエスケープとして読み飛ばし、二重区切りがあったかを返す。
    [<TailCall>]
    let rec private scanDelimitedLoop (input: string) (n: int) (delim: char) (s: LexState) (sawDouble: bool) =
        if s.Index >= n then
            None
        elif input.[s.Index] = delim then
            if s.Index + 1 < n && input.[s.Index + 1] = delim then
                scanDelimitedLoop input n delim (advance input (advance input s)) true
            else
                Some(s, sawDouble)
        else
            scanDelimitedLoop input n delim (advance input s) sawDouble

    let private scanDelimited (input: string) (n: int) (delim: char) (start: LexState) =
        scanDelimitedLoop input n delim (advance input start) false

    /// バイナリリテラルの閉じ引用符を探す。
    [<TailCall>]
    let rec private findQuoteEnd (input: string) (n: int) (i: int) =
        if i >= n then -1
        elif input.[i] = '\'' then i
        else findQuoteEnd input n (i + 1)

    /// 区切り文字列を読み、二重区切りを 1つに畳む。
    let private collectDelimited
        (input: string)
        (n: int)
        (delim: char)
        (unclosedMsg: string)
        (position: Position)
        (start: LexState)
        =
        match scanDelimited input n delim start with
        | None -> err (DbError.syntax unclosedMsg position)
        | Some(closeState, sawDouble) ->
            let raw = input.Substring(start.Index + 1, closeState.Index - start.Index - 1)

            let value =
                if sawDouble then
                    let one = string delim
                    raw.Replace(one + one, one)
                else
                    raw

            ret (value, advance input closeState)

    /// X'..' のバイナリリテラルを読む。
    let private scanBlobLiteral (input: string) (n: int) (startPos: Position) (s: LexState) =
        let contentStart = s.Index + 2
        let closeIdx = findQuoteEnd input n contentStart

        if closeIdx < 0 then
            err (DbError.syntax "unclosed binary literal" startPos)
        else
            let digitCount = closeIdx - contentStart

            if digitCount % 2 <> 0 then
                err (DbError.syntax "binary literal has odd length" startPos)
            else
                let values =
                    Array.init (digitCount / 2) (fun k ->
                        hexDigitValue input.[contentStart + 2 * k] <<< 4
                        ||| hexDigitValue input.[contentStart + 2 * k + 1])

                if Array.exists (fun v -> v < 0) values then
                    let hex = input.Substring(contentStart, digitCount)
                    let msg = sprintf "binary literal contains non-hex character: %s" hex
                    err (DbError.syntax msg startPos)
                else
                    ret (Token.BlobLit(Array.map byte values), advanceBy (digitCount + 3) s)

    /// 識別子またはキーワードを読む。
    let private scanWord (input: string) (n: int) (s: LexState) =
        let endIdx = scanIdentEnd input n (s.Index + 1)
        let word = input.Substring(s.Index, endIdx - s.Index)
        let s' = advanceBy (endIdx - s.Index) s

        if isAscii word 0 then
            match Keywords.TryGetValue word with
            | true, kw -> ret (Token.Keyword kw, s')
            | _ -> ret (Token.Ident word, s')
        else
            match KeywordsUpper.TryGetValue(word.ToUpperInvariant()) with
            | true, kw -> ret (Token.Keyword kw, s')
            | _ -> ret (Token.Ident word, s')

    /// 数値リテラルを読む（整数 / 実数 / 指数表記）。
    let private scanNumber (input: string) (n: int) (startPos: Position) (s: LexState) =
        let afterInt = scanDigits input n s
        let hasFraction = afterInt.Index < n && input.[afterInt.Index] = '.'

        let afterFraction =
            if hasFraction then
                advance input afterInt |> scanDigits input n
            else
                afterInt

        let hasExponent =
            afterFraction.Index < n
            && (input.[afterFraction.Index] = 'e' || input.[afterFraction.Index] = 'E')

        let afterExponent =
            if hasExponent then
                let afterE = advance input afterFraction

                let afterSign =
                    if afterE.Index < n && (input.[afterE.Index] = '+' || input.[afterE.Index] = '-') then
                        advance input afterE
                    else
                        afterE

                scanDigits input n afterSign
            else
                afterFraction

        let followedByIdent =
            afterExponent.Index < n && isIdentStart input.[afterExponent.Index]

        if followedByIdent then
            let text = input.Substring(s.Index, afterExponent.Index - s.Index + 1)
            err (DbError.syntax (sprintf "numeric literal is followed by an identifier: %s" text) startPos)
        else
            let text = input.Substring(s.Index, afterExponent.Index - s.Index)

            if hasFraction || hasExponent then
                match
                    System.Double.TryParse(
                        text,
                        System.Globalization.NumberStyles.Float,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, v -> ret (Token.RealLit v, afterExponent)
                | _ -> err (DbError.syntax (sprintf "failed to parse real: %s" text) startPos)
            else
                match
                    System.Int64.TryParse(
                        text,
                        System.Globalization.NumberStyles.Integer,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, v -> ret (Token.IntLit v, afterExponent)
                | _ -> err (DbError.syntax (sprintf "failed to parse integer: %s" text) startPos)

    /// 区切り付きのリテラル（'...' / "..."）を読む。
    let private scanDelimitedToken
        (input: string)
        (n: int)
        (delim: char)
        (unclosedMessage: string)
        (toToken: string -> Token)
        (startPos: Position)
        (s: LexState)
        =
        collectDelimited input n delim unclosedMessage startPos s
        |> Result.map (fun (v, s') -> toToken v, s')

    /// 記号・パラメータを読む。
    let private scanSymbol (input: string) (n: int) (startPos: Position) (paramIndex: int) (s: LexState) =
        let next = if s.Index + 1 < n then input.[s.Index + 1] else '\000'
        let c = input.[s.Index]

        match c with
        | '<' when next = '>' -> ret (Token.Symbol "<>", advanceBy 2 s)
        | '<' when next = '=' -> ret (Token.Symbol "<=", advanceBy 2 s)
        | '<' -> ret (Token.Symbol "<", advanceBy 1 s)
        | '>' when next = '=' -> ret (Token.Symbol ">=", advanceBy 2 s)
        | '>' -> ret (Token.Symbol ">", advanceBy 1 s)
        | '=' -> ret (Token.Symbol "=", advanceBy 1 s)
        | '!' when next = '=' -> ret (Token.Symbol "!=", advanceBy 2 s)
        | '|' when next = '|' -> ret (Token.Symbol "||", advanceBy 2 s)
        | '+' -> ret (Token.Symbol "+", advanceBy 1 s)
        | '-' -> ret (Token.Symbol "-", advanceBy 1 s)
        | '*' -> ret (Token.Symbol "*", advanceBy 1 s)
        | '/' -> ret (Token.Symbol "/", advanceBy 1 s)
        | '%' -> ret (Token.Symbol "%", advanceBy 1 s)
        | '?' -> ret (Token.Param paramIndex, advanceBy 1 s)
        | ';' -> ret (Token.Symbol ";", advanceBy 1 s)
        | ',' -> ret (Token.Symbol ",", advanceBy 1 s)
        | '.' -> ret (Token.Symbol ".", advanceBy 1 s)
        | '(' -> ret (Token.Symbol "(", advanceBy 1 s)
        | ')' -> ret (Token.Symbol ")", advanceBy 1 s)
        | c -> err (DbError.syntax (sprintf "unexpected character '%c'" c) startPos)

    /// 1トークンを読む。
    /// 読み後は次の位置を返す。
    /// `paramIndex` は `?` に割り当てる位置パラメータ番号。
    let private scanToken (input: string) (n: int) (startPos: Position) (paramIndex: int) (s: LexState) =
        let c = input.[s.Index]

        if (c = 'x' || c = 'X') && s.Index + 1 < n && input.[s.Index + 1] = '\'' then
            scanBlobLiteral input n startPos s
        elif isIdentStart c then
            scanWord input n s
        elif
            System.Char.IsDigit c
            || c = '.' && s.Index + 1 < n && System.Char.IsDigit input.[s.Index + 1]
        then
            scanNumber input n startPos s
        elif c = '\'' then
            scanDelimitedToken input n '\'' "unclosed string" Token.StringLit startPos s
        elif c = '"' then
            scanDelimitedToken input n '"' "unclosed quoted identifier" Token.QuotedIdent startPos s
        else
            scanSymbol input n startPos paramIndex s

    [<TailCall>]
    let rec private collectTokens (input: string) (n: int) (acc: TokenItem list) (nextParam: int) (s: LexState) =
        match skipTrivia input s with
        | Ok s when s.Index >= n -> ret (List.rev ({ Token = Token.Eos; Pos = s.Position } :: acc))
        | Ok s ->
            let startPos = s.Position

            match scanToken input n startPos nextParam s with
            | Ok(tok, s') ->
                let nextParam =
                    match tok with
                    | Token.Param _ -> nextParam + 1
                    | _ -> nextParam

                collectTokens input n ({ Token = tok; Pos = startPos } :: acc) nextParam s'
            | Error e -> err e
        | Error e -> err e

    /// SQL テキストをトークン列へ分解する。
    /// 位置と出力は不変で、外側の再帰が値を積み上げる。
    /// `?` は出現順に 0 始まりの番号を振られる。
    let tokenize (input: string) =
        { Index = 0; Line = 1; Column = 1 } |> collectTokens input input.Length [] 0
