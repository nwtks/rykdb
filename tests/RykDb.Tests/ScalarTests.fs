// ===========================================================================
// Scalar.fs のテスト: LIKE の照合（トークン化・ESCAPE・コードポイント）
// ===========================================================================

module RykDb.ScalarTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Scalar.matchLikeTokens: 計画時にコンパイルしたパターンは行ごとに配列を確保しない`` () =
    // リテラルパターンは `Eval.prepare` が 1回だけコンパイルし、
    // 以降はコンパイル済みトークン列で照合する。
    let pattern = "a%b%c"
    let text = "aXXbYYc"
    let schema = outSchema [ outCol "s" None (Some DataType.Text) ]

    let expr =
        Expr.Like(Expr.Column { Qualifier = None; Name = "s" }, Expr.Literal(Literal.String pattern), false, None)

    let prepared = Eval.prepare schema [ expr ]
    let compiled = ok (prepared.Like Scalar.NoEscape pattern)
    Assert.True(Scalar.matchLikeTokens compiled.Tokens (System.MemoryExtensions.AsSpan text))

    let before = System.GC.GetAllocatedBytesForCurrentThread()

    let rec loop i matched =
        if i >= 1000 then
            matched
        else
            let m =
                match prepared.Like Scalar.NoEscape pattern with
                | Ok cached -> Scalar.matchLikeTokens cached.Tokens (System.MemoryExtensions.AsSpan text)
                | Error _ -> false

            loop (i + 1) (if m then matched + 1 else matched)

    let matched = loop 0 0
    let allocated = System.GC.GetAllocatedBytesForCurrentThread() - before
    Assert.Equal(1000, matched)
    Assert.True(allocated < 10_000L, sprintf "1000 回で %d バイト確保しました" allocated)
