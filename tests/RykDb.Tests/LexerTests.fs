// ===========================================================================
// Lexer.fs（字句解析）
// ===========================================================================

module RykDb.LexerTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private tokens (sql: string) =
    match Lexer.tokenize sql with
    | Ok ts -> ts
    | Error e ->
        Assert.Fail(sprintf "字句解析に失敗: %s" (DbError.toDisplayString e))
        []

let private kinds (sql: string) =
    tokens sql |> List.map (fun t -> t.Token)

[<Fact>]
let ``Lexer: 位置情報が付く`` () =
    let ts = tokens "a\nb"
    Assert.Equal(1, ts.[0].Pos.Line)
    Assert.Equal(2, ts.[1].Pos.Line)

[<Fact>]
let ``Lexer: 位置情報は行と列を持つ`` () =
    let ts = tokens "ab cd\nef"
    // 1行目: "ab" は 1列目、"cd" は 4列目
    Assert.Equal((1, 1), (ts.[0].Pos.Line, ts.[0].Pos.Column))
    Assert.Equal((1, 4), (ts.[1].Pos.Line, ts.[1].Pos.Column))
    // 2行目は 1列目から
    Assert.Equal((2, 1), (ts.[2].Pos.Line, ts.[2].Pos.Column))

[<Fact>]
let ``Lexer: 行コメントとブロックコメントを読み飛ばす`` () =
    let ks = kinds "1 -- comment\n + /* block */ 2"
    Assert.Equal<Token list>([ Token.IntLit 1L; Token.Symbol "+"; Token.IntLit 2L; Token.Eos ], ks)

[<Fact>]
let ``Lexer: 入れ子のブロックコメントを読み飛ばす`` () =
    let ks = kinds "/* a /* b */ c */ 1"
    Assert.Equal<Token list>([ Token.IntLit 1L; Token.Eos ], ks)

[<Fact>]
let ``Lexer: 引用識別子`` () =
    let ks = kinds "\"col\""
    Assert.Equal<Token list>([ Token.QuotedIdent "col"; Token.Eos ], ks)

[<Fact>]
let ``Lexer: 識別子と予約語`` () =
    let ks = kinds "SELECT foo FROM bar"

    Assert.Equal<Token list>(
        [
            Token.Keyword "SELECT"
            Token.Ident "foo"
            Token.Keyword "FROM"
            Token.Ident "bar"
            Token.Eos
        ],
        ks
    )

[<Fact>]
let ``Lexer: 整数・実数・文字列・記号`` () =
    let ks = kinds "42 3.14 'hi' = , ( )"

    Assert.Equal<Token list>(
        [
            Token.IntLit 42L
            Token.RealLit 3.14
            Token.StringLit "hi"
            Token.Symbol "="
            Token.Symbol ","
            Token.Symbol "("
            Token.Symbol ")"
            Token.Eos
        ],
        ks
    )

[<Fact>]
let ``Lexer: バイナリリテラル`` () =
    let ks = kinds "X'AB'"

    match ks with
    | [ Token.BlobLit b; Token.Eos ] -> Assert.Equal<byte array>([| 0xABuy |], b)
    | other -> failActual "BlobLit ではない" other

[<Fact>]
let ``Lexer: 実数の指数表記`` () =
    let ks = kinds "1e5 1.5E+3 1.5e-3"

    Assert.Equal<Token list>(
        [
            Token.RealLit 100000.0
            Token.RealLit 1500.0
            Token.RealLit 0.0015
            Token.Eos
        ],
        ks
    )

[<Fact>]
let ``Lexer: 型名は予約語にしない`` () =
    // 型名は Ident として字句解析され、列名・テーブル名にも使える
    let ks = kinds "INTEGER TEXT"
    Assert.Equal<Token list>([ Token.Ident "INTEGER"; Token.Ident "TEXT"; Token.Eos ], ks)

[<Fact>]
let ``Lexer: 空入力は Eof のみ`` () =
    let ts = tokens ""
    Assert.Equal(1, ts.Length)

    match ts.Head.Token with
    | Token.Eos -> ()
    | other -> failActual "Eof ではない" other

[<Fact>]
let ``Lexer: 閉じないブロックコメントはエラー`` () =
    expectErrorKind ErrorKind.SyntaxError "エラーになるべき" (Lexer.tokenize "1 /* unclosed")

[<Fact>]
let ``Lexer: 閉じない文字列はエラー`` () =
    expectErrorKind ErrorKind.SyntaxError "エラーになるべき" (Lexer.tokenize "'unclosed")

[<Fact>]
let ``Lexer: 予期しない文字はエラー`` () =
    expectErrorKind ErrorKind.SyntaxError "エラーになるべき" (Lexer.tokenize "@")

[<Fact>]
let ``Lexer: 大きすぎる整数はエラー`` () =
    // Int64 に収まらない桁数は Int64.TryParse が失敗する
    match Lexer.tokenize "99999999999999999999" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: バイナリリテラルの異常系はエラー`` () =
    let bad (sql: string) =
        expectErrorKind ErrorKind.SyntaxError (sprintf "エラーになるべき: %s" sql) (Lexer.tokenize sql)

    bad "X'12" // 閉じていない
    bad "X'123'" // 桁数が奇数
    bad "X'0G'" // 16 進でない文字

[<Fact>]
let ``Lexer: 文字列リテラルの二重区切りを 1 つに畳む`` () =
    let ks = kinds "'' 'it''s' 'a''''b'"

    Assert.Equal<Token list>(
        [
            Token.StringLit ""
            Token.StringLit "it's"
            Token.StringLit "a''b"
            Token.Eos
        ],
        ks
    )

[<Fact>]
let ``Lexer: 引用識別子の二重区切りを 1 つに畳む`` () =
    let ks = kinds "\"a\"\"b\" \"\"\"\""
    Assert.Equal<Token list>([ Token.QuotedIdent "a\"b"; Token.QuotedIdent "\""; Token.Eos ], ks)

[<Fact>]
let ``Lexer: 予約語は大文字小文字を無視して判定する`` () =
    let ks = kinds "select SeLeCt true FALSE TrUe"

    Assert.Equal<Token list>(
        [
            Token.Keyword "SELECT"
            Token.Keyword "SELECT"
            Token.Keyword "TRUE"
            Token.Keyword "FALSE"
            Token.Keyword "TRUE"
            Token.Eos
        ],
        ks
    )

[<Fact>]
let ``Lexer: 非 ASCII の識別子は Ident として読む`` () =
    // 予約語は ASCII だけ。非 ASCII を含む語は（大文字化しても予約語にならない限り）Ident。
    let ks = kinds "café"
    Assert.Equal<Token list>([ Token.Ident "café"; Token.Eos ], ks)

[<Fact>]
let ``Lexer: バイナリリテラルは空・小文字・混在した 16 進を受け付ける`` () =
    match kinds "X'' x'abCd'" with
    | [ Token.BlobLit empty; Token.BlobLit bytes; Token.Eos ] ->
        Assert.Empty empty
        Assert.Equal<byte array>([| 0xABuy; 0xCDuy |], bytes)
    | other -> failActual "BlobLit ではない" other

[<Fact>]
let ``Lexer: 識別子に数字とアンダースコアを含められる`` () =
    let ks = kinds "a1_b _x"
    Assert.Equal<Token list>([ Token.Ident "a1_b"; Token.Ident "_x"; Token.Eos ], ks)

[<Fact>]
let ``Lexer: 比較・論理演算子の記号を 1 トークンで読む`` () =
    let ks = kinds "<> <= >= != || < > ="

    Assert.Equal<Token list>(
        [
            Token.Symbol "<>"
            Token.Symbol "<="
            Token.Symbol ">="
            Token.Symbol "!="
            Token.Symbol "||"
            Token.Symbol "<"
            Token.Symbol ">"
            Token.Symbol "="
            Token.Eos
        ],
        ks
    )

[<Fact>]
let ``Lexer: 単独の ! と | はエラー`` () =
    let bad (sql: string) =
        expectErrorKind ErrorKind.SyntaxError (sprintf "エラーになるべき: %s" sql) (Lexer.tokenize sql)

    bad "!"
    bad "|"

[<Fact>]
let ``Lexer: 閉じない引用識別子はエラー`` () =
    match Lexer.tokenize "\"unclosed" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: 複数行の文字列の後の位置は行を進める`` () =
    let ts = tokens "'a\nb' 1"
    Assert.Equal((2, 4), (ts.[1].Pos.Line, ts.[1].Pos.Column))
