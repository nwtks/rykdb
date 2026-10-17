// ===========================================================================
// Schema.fs のテスト: 出力スキーマの列解決
// ===========================================================================

module RykDb.SchemaTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Eval: 修飾子の無い列名が曖昧ならエラー`` () =
    let schema = outSchema [ outCol "id" (Some "a") None; outCol "id" (Some "b") None ]

    expectError "曖昧な列名が解決できてしまいました" (Schema.resolve schema { Qualifier = None; Name = "id" })

    // 修飾子付きなら解決できる
    Assert.Equal(1, ok (Schema.resolve schema { Qualifier = Some "b"; Name = "id" }))
