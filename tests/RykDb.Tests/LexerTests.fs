// ===========================================================================
// Lexer.fs（字句解析）
// ===========================================================================

module RykDb.LexerTests

open Xunit
open RykDb.Core

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
    | other -> Assert.Fail(sprintf "BlobLit ではない: %A" other)

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
let ``Lexer: 空入力は Eof のみ`` () =
    let ts = tokens ""
    Assert.Equal(1, ts.Length)

    match ts.Head.Token with
    | Token.Eos -> ()
    | other -> Assert.Fail(sprintf "Eof ではない: %A" other)

[<Fact>]
let ``Lexer: 閉じないブロックコメントはエラー`` () =
    match Lexer.tokenize "1 /* unclosed" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: 閉じない文字列はエラー`` () =
    match Lexer.tokenize "'unclosed" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: 予期しない文字はエラー`` () =
    match Lexer.tokenize "@" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: 大きすぎる整数はエラー`` () =
    // Int64 に収まらない桁数は Int64.TryParse が失敗する
    match Lexer.tokenize "99999999999999999999" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: バイナリリテラルの異常系はエラー`` () =
    let bad (sql: string) =
        match Lexer.tokenize sql with
        | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
        | Ok _ -> Assert.Fail(sprintf "エラーになるべき: %s" sql)

    bad "X'12" // 閉じていない
    bad "X'123'" // 桁数が奇数
    bad "X'0G'" // 16 進でない文字
