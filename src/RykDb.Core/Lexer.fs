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
    /// 記号・演算子
    | Symbol of string
    | Eos

type internal TokenItem =
    {
        Token: Token
        Pos: Position
    }

    member x.Describe =
        match x.Token with
        | Token.Ident s -> sprintf "identifier %s" s
        | Token.QuotedIdent s -> sprintf "quoted identifier %s" s
        | Token.Keyword s -> sprintf "keyword %s" s
        | Token.IntLit v -> sprintf "integer %d" v
        | Token.RealLit v -> sprintf "real %g" v
        | Token.StringLit _ -> "string"
        | Token.BlobLit _ -> "binary"
        | Token.Symbol s -> sprintf "symbol %s" s
        | Token.Eos -> "end of statement"

/// 字句解析器の位置。
/// 値は不変で、前進すると新しい位置を返す。
[<Struct>]
type internal LexState =
    {
        Index: int
        Line: int
        Column: int
    }

    member s.Position =
        {
            Offset = s.Index
            Line = s.Line
            Column = s.Column
        }

module internal Lexer =
    /// 予約語の一覧。
    /// パーサはここで定義した綴りだけを受け付ける。
    let private Keywords =
        Set.ofList
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
            ]

    /// 記号・演算子。
    /// 長的なものを先に試す。
    let private Symbols =
        [
            "<>"
            "!="
            "<="
            ">="
            "||"
            "="
            "<"
            ">"
            "+"
            "-"
            "*"
            "/"
            "%"
            ";"
            ","
            "."
            "("
            ")"
        ]

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

    /// 空白とコメントを読み飛ばす。
    /// 入れ子のブロックコメントに対応。
    let private skipTrivia (input: string) (s: LexState) =
        let rec skipLine n s =
            if s.Index < n && input.[s.Index] <> '\n' then
                advance input s |> skipLine n
            else
                s

        let rec loopBlockComment n depth s =
            if depth <= 0 then
                ret s
            elif s.Index >= n then
                err (DbError.syntax "unclosed comment" s.Line)
            elif s.Index + 1 < n && input.[s.Index] = '/' && input.[s.Index + 1] = '*' then
                advance input s |> advance input |> loopBlockComment n (depth + 1)
            elif s.Index + 1 < n && input.[s.Index] = '*' && input.[s.Index + 1] = '/' then
                advance input s |> advance input |> loopBlockComment n (depth - 1)
            else
                advance input s |> loopBlockComment n depth

        let rec loop n s =
            if s.Index >= n then
                ret s
            else
                let c = input.[s.Index]

                if c = ' ' || c = '\t' || c = '\r' || c = '\n' then
                    advance input s |> loop n
                elif c = '-' && s.Index + 1 < n && input.[s.Index + 1] = '-' then
                    skipLine n s |> loop n
                elif c = '/' && s.Index + 1 < n && input.[s.Index + 1] = '*' then
                    let s' = advance input s |> advance input

                    match loopBlockComment n 1 s' with
                    | Error e -> Error e
                    | Ok st -> loop n st
                else
                    ret s

        loop input.Length s

    /// 1トークンを読む。
    /// 読み後は次の位置を返す。
    let private scanToken (input: string) (startPos: Position) (s: LexState) =
        let isIdentStart c = System.Char.IsLetter c || c = '_'

        let isIdentPart c =
            System.Char.IsLetterOrDigit c || c = '_'

        let hexValue c =
            if c >= '0' && c <= '9' then int c - int '0'
            elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
            elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
            else -1

        let rec collectHex n (sb: System.Text.StringBuilder) s =
            if s.Index >= n then
                err (DbError.syntax "unclosed binary literal" startPos.Line)
            elif input.[s.Index] = '\'' then
                let hex = sb.ToString()

                if hex.Length % 2 <> 0 then
                    err (DbError.syntax "binary literal has odd length" startPos.Line)
                else
                    let nonHex = hex |> Seq.exists (fun ch -> hexValue ch < 0)

                    if nonHex then
                        let msg = sprintf "binary literal contains non-hex character: %s" hex
                        err (DbError.syntax msg startPos.Line)
                    else
                        let bytes =
                            [|
                                for i in 0 .. hex.Length / 2 - 1 do
                                    yield System.Convert.ToByte(hex.Substring(i * 2, 2), 16)
                            |]

                        Ok(Token.BlobLit bytes, advance input s)
            else
                sb.Append(input.[s.Index]) |> ignore
                advance input s |> collectHex n sb

        let rec collect n (sb: System.Text.StringBuilder) s =
            if s.Index < n && isIdentPart input.[s.Index] then
                sb.Append(input.[s.Index]) |> ignore
                advance input s |> collect n sb
            else
                sb.ToString(), s

        let rec digits n st =
            if st.Index < n && System.Char.IsDigit input.[st.Index] then
                advance input st |> digits n
            else
                st

        let rec collectChars (input: string) n delim unclosedMsg line (sb: System.Text.StringBuilder) s =
            if s.Index >= n then
                err (DbError.syntax unclosedMsg line)
            elif input.[s.Index] = delim then
                if s.Index + 1 < n && input.[s.Index + 1] = delim then
                    sb.Append delim |> ignore

                    advance input s
                    |> advance input
                    |> collectChars input n delim unclosedMsg line sb
                else
                    Ok(sb.ToString(), advance input s)
            else
                sb.Append(input.[s.Index]) |> ignore
                advance input s |> collectChars input n delim unclosedMsg line sb

        let collectDelimited input n delim unclosedMsg line start =
            advance input start
            |> collectChars input n delim unclosedMsg line (System.Text.StringBuilder())

        let rec advanceN count st =
            if count = 0 then
                st
            else
                advance input st |> advanceN (count - 1)

        let n = input.Length
        let c = input.[s.Index]

        if (c = 'x' || c = 'X') && s.Index + 1 < n && input.[s.Index + 1] = '\'' then
            let s' = advance input s |> advance input
            collectHex n (System.Text.StringBuilder()) s'
        elif isIdentStart c then
            let word, s' = collect n (System.Text.StringBuilder()) s
            let upper = word.ToUpperInvariant()

            let tok =
                if Set.contains upper Keywords then Token.Keyword upper
                elif upper = "TRUE" then Token.Keyword "TRUE"
                elif upper = "FALSE" then Token.Keyword "FALSE"
                else Token.Ident word

            Ok(tok, s')
        elif
            System.Char.IsDigit c
            || c = '.' && s.Index + 1 < n && System.Char.IsDigit input.[s.Index + 1]
        then
            let afterInt = digits n s
            let hasFraction = afterInt.Index < n && input.[afterInt.Index] = '.'

            let afterFraction =
                if hasFraction then
                    advance input afterInt |> digits n
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

                    digits n afterSign
                else
                    afterFraction

            let text = input.Substring(s.Index, afterExponent.Index - s.Index)

            if hasFraction || hasExponent then
                match
                    System.Double.TryParse(
                        text,
                        System.Globalization.NumberStyles.Float,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, v -> Ok(Token.RealLit v, afterExponent)
                | _ -> err (DbError.syntax (sprintf "failed to parse real: %s" text) startPos.Line)
            else
                match
                    System.Int64.TryParse(
                        text,
                        System.Globalization.NumberStyles.Integer,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, v -> Ok(Token.IntLit v, afterExponent)
                | _ -> err (DbError.syntax (sprintf "failed to parse integer: %s" text) startPos.Line)
        elif c = '\'' then
            collectDelimited input n '\'' "unclosed string" startPos.Line s
            |> Result.map (fun (v, s') -> Token.StringLit v, s')
        elif c = '"' then
            collectDelimited input n '"' "unclosed quoted identifier" startPos.Line s
            |> Result.map (fun (v, s') -> Token.QuotedIdent v, s')
        else
            let matched =
                Symbols
                |> List.tryFind (fun sym ->
                    s.Index + sym.Length <= n
                    && System.String.CompareOrdinal(input, s.Index, sym, 0, sym.Length) = 0)

            match matched with
            | None -> err (DbError.syntax (sprintf "unexpected character '%c'" c) startPos.Line)
            | Some sym -> Ok(Token.Symbol sym, advanceN sym.Length s)

    /// SQL テキストをトークン列へ分解する。
    /// 位置と出力は不変で、外側の再帰が値を積み上げる。
    let tokenize (input: string) =
        let rec collectTokens n acc s =
            match skipTrivia input s with
            | Error e -> Error e
            | Ok s when s.Index >= n -> Ok(List.rev ({ Token = Token.Eos; Pos = s.Position } :: acc))
            | Ok s ->
                let startPos = s.Position

                match scanToken input startPos s with
                | Error e -> Error e
                | Ok(tok, s') -> collectTokens n ({ Token = tok; Pos = startPos } :: acc) s'

        { Index = 0; Line = 1; Column = 1 } |> collectTokens input.Length []
