namespace RykDb.Core

open System
open System.Globalization
open System.Text

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
    | Eof

type internal TokenItem =
    {
        Token: Token
        Pos: Position
    }

    member x.Describe =
        match x.Token with
        | Ident s -> sprintf "識別子 %s" s
        | QuotedIdent s -> sprintf "引用識別子 %s" s
        | Keyword s -> sprintf "予約語 %s" s
        | IntLit v -> sprintf "整数 %d" v
        | RealLit v -> sprintf "実数 %g" v
        | StringLit _ -> "文字列"
        | BlobLit _ -> "バイナリ"
        | Symbol s -> sprintf "記号 %s" s
        | Eof -> "文の終わり"

/// 字句解析器の位置。値は不変で、前進すると新しい位置を返す。
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
    /// 予約語の一覧。パーサはここで定義した綴りだけを受け付ける。
    let private keywords =
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

    /// 記号・演算子。長的なものを先に試す。
    let private symbols =
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

    let private isIdentStart (c: char) = Char.IsLetter c || c = '_'
    let private isIdentPart (c: char) = Char.IsLetterOrDigit c || c = '_'

    let private hexValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
        else -1

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

    /// 空白とコメントを読み飛ばす。入れ子のブロックコメントに対応。
    let private skipTrivia (input: string) (s: LexState) =
        let n = input.Length

        let rec loopBlockComment (s: LexState) (depth: int) =
            if depth <= 0 then
                ret s
            elif s.Index >= n then
                err (DbError.syntax "コメントが閉じられていません" s.Line)
            elif s.Index + 1 < n && input.[s.Index] = '/' && input.[s.Index + 1] = '*' then
                loopBlockComment (advance input (advance input s)) (depth + 1)
            elif s.Index + 1 < n && input.[s.Index] = '*' && input.[s.Index + 1] = '/' then
                loopBlockComment (advance input (advance input s)) (depth - 1)
            else
                loopBlockComment (advance input s) depth

        let rec loop (s: LexState) =
            if s.Index >= n then
                ret s
            else
                let c = input.[s.Index]

                if c = ' ' || c = '\t' || c = '\r' || c = '\n' then
                    loop (advance input s)
                elif c = '-' && s.Index + 1 < n && input.[s.Index + 1] = '-' then
                    let rec skipLine (s: LexState) =
                        if s.Index < n && input.[s.Index] <> '\n' then
                            skipLine (advance input s)
                        else
                            s

                    loop (skipLine s)
                elif c = '/' && s.Index + 1 < n && input.[s.Index + 1] = '*' then
                    let s' = advance input (advance input s)

                    match loopBlockComment s' 1 with
                    | Error e -> Error e
                    | Ok st -> loop st
                else
                    ret s

        loop s

    /// `delim` で囲まれた文字列リテラル / 引用識別子を読む。
    /// `delim` を 2 つ続けると 1 文字として取り込む（SQL 標準のエスケープ）。
    /// `'` の文字列と `"` の引用識別子は閉じ方もエスケープも同じなので 1 本に集約する。
    let private collectDelimited
        (input: string)
        (n: int)
        (delim: char)
        (unclosedMsg: string)
        (line: int)
        (start: LexState)
        =
        let rec collectChars (s: LexState) (sb: StringBuilder) =
            if s.Index >= n then
                err (DbError.syntax unclosedMsg line)
            elif input.[s.Index] = delim then
                if s.Index + 1 < n && input.[s.Index + 1] = delim then
                    sb.Append delim |> ignore
                    collectChars (advance input (advance input s)) sb
                else
                    Ok(sb.ToString(), advance input s)
            else
                sb.Append(input.[s.Index]) |> ignore
                collectChars (advance input s) sb

        collectChars (advance input start) (StringBuilder())

    /// 1 トークンを読む。読み後は次の位置を返す。
    let private scanToken (input: string) (s: LexState) (startPos: Position) =
        let n = input.Length
        let c = input.[s.Index]

        if (c = 'x' || c = 'X') && s.Index + 1 < n && input.[s.Index + 1] = '\'' then
            let s' = advance input (advance input s)

            let rec collectHex (s: LexState) (sb: StringBuilder) =
                if s.Index >= n then
                    err (DbError.syntax "バイナリリテラルが閉じられていません" startPos.Line)
                elif input.[s.Index] = '\'' then
                    let hex = sb.ToString()

                    if hex.Length % 2 <> 0 then
                        err (DbError.syntax "バイナリリテラルの長さが奇数です" startPos.Line)
                    else
                        let nonHex = hex |> Seq.exists (fun ch -> hexValue ch < 0)

                        if nonHex then
                            let msg = sprintf "バイナリリテラルに 16 進でない文字があります: %s" hex
                            err (DbError.syntax msg startPos.Line)
                        else
                            let bytes =
                                [|
                                    for i in 0 .. hex.Length / 2 - 1 do
                                        yield Convert.ToByte(hex.Substring(i * 2, 2), 16)
                                |]

                            Ok(BlobLit bytes, advance input s)
                else
                    sb.Append(input.[s.Index]) |> ignore
                    collectHex (advance input s) sb

            collectHex s' (StringBuilder())

        elif isIdentStart c then
            let rec collect (s: LexState) (sb: StringBuilder) =
                if s.Index < n && isIdentPart input.[s.Index] then
                    sb.Append(input.[s.Index]) |> ignore
                    collect (advance input s) sb
                else
                    sb.ToString(), s

            let word, s' = collect s (StringBuilder())
            let upper = word.ToUpperInvariant()

            let tok =
                if Set.contains upper keywords then Keyword upper
                elif upper = "TRUE" then Keyword "TRUE"
                elif upper = "FALSE" then Keyword "FALSE"
                else Ident word

            Ok(tok, s')
        elif Char.IsDigit c || c = '.' && s.Index + 1 < n && Char.IsDigit input.[s.Index + 1] then
            let rec digits (st: LexState) =
                if st.Index < n && Char.IsDigit input.[st.Index] then
                    digits (advance input st)
                else
                    st

            let afterInt = digits s
            let hasFraction = afterInt.Index < n && input.[afterInt.Index] = '.'

            let afterFraction =
                if hasFraction then
                    digits (advance input afterInt)
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

                    digits afterSign
                else
                    afterFraction

            let text = input.Substring(s.Index, afterExponent.Index - s.Index)

            if hasFraction || hasExponent then
                match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, v -> Ok(RealLit v, afterExponent)
                | _ -> err (DbError.syntax (sprintf "実数の解析に失敗しました: %s" text) startPos.Line)
            else
                match Int64.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                | true, v -> Ok(IntLit v, afterExponent)
                | _ -> err (DbError.syntax (sprintf "整数の解析に失敗しました: %s" text) startPos.Line)

        elif c = '\'' then
            collectDelimited input n '\'' "文字列が閉じられていません" startPos.Line s
            |> Result.map (fun (v, s') -> StringLit v, s')

        elif c = '"' then
            collectDelimited input n '"' "引用識別子が閉じられていません" startPos.Line s
            |> Result.map (fun (v, s') -> QuotedIdent v, s')

        // 6) 記号・演算子
        else
            let matched =
                symbols
                |> List.tryFind (fun sym ->
                    s.Index + sym.Length <= n
                    && String.CompareOrdinal(input, s.Index, sym, 0, sym.Length) = 0)

            match matched with
            | None -> err (DbError.syntax (sprintf "予期しない文字 '%c'" c) startPos.Line)
            | Some sym ->
                let rec advanceN (st: LexState) (count: int) =
                    if count = 0 then
                        st
                    else
                        advanceN (advance input st) (count - 1)

                Ok(Symbol sym, advanceN s sym.Length)

    /// SQL テキストをトークン列へ分解する。
    /// 位置と出力は不変で、外側の再帰が値を積み上げる。
    let tokenize (input: string) =
        let n = input.Length
        let start = { Index = 0; Line = 1; Column = 1 }

        let rec collectTokens (s: LexState) (acc: TokenItem list) =
            match skipTrivia input s with
            | Error e -> Error e
            | Ok s when s.Index >= n -> Ok(List.rev ({ Token = Eof; Pos = s.Position } :: acc))
            | Ok s ->
                let startPos = s.Position

                match scanToken input s startPos with
                | Error e -> Error e
                | Ok(tok, s') -> collectTokens s' ({ Token = tok; Pos = startPos } :: acc)

        collectTokens start []
