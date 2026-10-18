// ===========================================================================
// Schema.fs のテスト: 出力スキーマの列解決
// ===========================================================================

module RykDb.SchemaTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Schema.concat: 列とテーブルを連結する`` () =
    let s1 = outSchema [ outCol "a" (Some "t") (Some DataType.Integer) ]
    let s2 = outSchema [ outCol "b" (Some "u") (Some DataType.Text) ]
    let s1' = { s1 with Tables = [ "t" ] }
    let s2' = { s2 with Tables = [ "u" ] }
    let c = Schema.concat s1' s2'
    Assert.Equal(2, c.Columns.Length)
    Assert.Equal<string list>([ "t"; "u" ], c.Tables)

[<Fact>]
let ``Schema.concatAll: 空リストは empty`` () =
    let c = Schema.concatAll []
    Assert.Equal(0, c.Columns.Length)
    Assert.True(List.isEmpty c.Tables)

[<Fact>]
let ``Schema.expandStar: * は全列`` () =
    let s =
        outSchema
            [
                outCol "id" (Some "t") (Some DataType.Integer)
                outCol "name" (Some "t") (Some DataType.Text)
            ]

    match Schema.expandStar s Expr.Star with
    | Some(false, cols) -> Assert.Equal(2, cols.Length)
    | other -> Assert.Fail(sprintf "expandStar * が想定外: %A" other)

[<Fact>]
let ``Schema.expandStar: t.* は修飾一致のみ`` () =
    let s =
        outSchema
            [
                outCol "id" (Some "t") (Some DataType.Integer)
                outCol "id" (Some "u") (Some DataType.Integer)
            ]

    match Schema.expandStar s (Expr.QualifiedStar "t") with
    | Some(true, cols) -> Assert.Equal(1, cols.Length)
    | other -> Assert.Fail(sprintf "expandStar t.* が想定外: %A" other)

[<Fact>]
let ``Schema.projectHeader: * は入力列名`` () =
    let s = outSchema [ outCol "id" (Some "t") (Some DataType.Integer) ]
    let h = Schema.projectHeader s [ { Expression = Expr.Star; Alias = None } ]
    Assert.Equal<string array>([| "id" |], h)

[<Fact>]
let ``Schema.tryAsColumnRef: Column / Name を列参照に`` () =
    match Schema.tryAsColumnRef (Expr.Column { Qualifier = Some "t"; Name = "id" }) with
    | Some c -> Assert.Equal("id", c.Name)
    | None -> Assert.Fail "Column は Some になるべき"

    match Schema.tryAsColumnRef (Expr.Name "x") with
    | Some c -> Assert.Equal("x", c.Name)
    | None -> Assert.Fail "Name は Some になるべき"

    Assert.True((Schema.tryAsColumnRef (Expr.Literal(Literal.Int 1L))).IsNone)

[<Fact>]
let ``Schema.ofTable: テーブルの全列を並べる`` () =
    let ts = TestHelpers.peopleSchema
    let s = Schema.ofTable "people" None ts
    Assert.Equal(ts.Columns.Length, s.Columns.Length)
    Assert.Equal<string list>([ "people" ], s.Tables)

[<Fact>]
let ``Eval: 修飾子の無い列名が曖昧ならエラー`` () =
    let schema = outSchema [ outCol "id" (Some "a") None; outCol "id" (Some "b") None ]

    expectError "曖昧な列名が解決できてしまいました" (Schema.resolve schema { Qualifier = None; Name = "id" })

    // 修飾子付きなら解決できる
    Assert.Equal(1, ok (Schema.resolve schema { Qualifier = Some "b"; Name = "id" }))
