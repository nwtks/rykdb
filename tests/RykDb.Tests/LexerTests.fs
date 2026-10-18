// ===========================================================================
// Lexer.fs（字句解析）
// ===========================================================================

module RykDb.LexerTests

open System
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
let ``Lexer: 行コメントとブロックコメントを読み飛ばす`` () =
    let ks = kinds "1 -- comment\n + /* block */ 2"
    Assert.Equal<Token list>([ Token.IntLit 1L; Token.Symbol "+"; Token.IntLit 2L; Token.Eof ], ks)

[<Fact>]
let ``Lexer: 閉じないブロックコメントはエラー`` () =
    match Lexer.tokenize "1 /* unclosed" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: 引用識別子`` () =
    let ks = kinds "\"col\""
    Assert.Equal<Token list>([ Token.QuotedIdent "col"; Token.Eof ], ks)

[<Fact>]
let ``Lexer: 閉じない文字列はエラー`` () =
    match Lexer.tokenize "'unclosed" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: 識別子と予約語`` () =
    let ks = kinds "SELECT foo FROM bar"

    Assert.Equal<Token list>(
        [
            Token.Keyword "SELECT"
            Token.Ident "foo"
            Token.Keyword "FROM"
            Token.Ident "bar"
            Token.Eof
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
            Token.Eof
        ],
        ks
    )

[<Fact>]
let ``Lexer: バイナリリテラル`` () =
    let ks = kinds "X'AB'"

    match ks with
    | [ Token.BlobLit b; Token.Eof ] -> Assert.Equal<byte array>([| 0xABuy |], b)
    | other -> Assert.Fail(sprintf "BlobLit ではない: %A" other)

[<Fact>]
let ``Lexer: 予期しない文字はエラー`` () =
    match Lexer.tokenize "@" with
    | Error e -> Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Lexer: 空入力は Eof のみ`` () =
    let ts = tokens ""
    Assert.Equal(1, ts.Length)

    match ts.Head.Token with
    | Token.Eof -> ()
    | other -> Assert.Fail(sprintf "Eof ではない: %A" other)
