// ===========================================================================
// OpsDistinct.fs のテスト（DISTINCT）
// ===========================================================================

module RykDb.OpsDistinctTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers
open RykDb.TestHelpers.OpsFixtures

[<Fact>]
let ``distinct: 最初に現れた順序を保つ`` () =
    let op: Op =
        Op.ofArray
            [|
                [| SqlValue.Int 2L |]
                [| SqlValue.Int 1L |]
                [| SqlValue.Int 2L |]
                [| SqlValue.Int 1L |]
            |]

    let rows = ok (Op.toArray (UnlimitedOps.distinct op))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])

[<Fact>]
let ``distinct: SQL 等値で同じ値（INTEGER / DECIMAL / REAL）は 1 行にまとめる`` () =
    // `1 = 1.0` は真なので DISTINCT も同じ値として扱う（先頭の行を残す）。
    let op: Op =
        Op.ofArray
            [|
                [| SqlValue.Int 1L |]
                [| SqlValue.Decimal 1.0M |]
                [| SqlValue.Decimal 1.00M |]
                [| SqlValue.Real 1.0 |]
                [| SqlValue.Int 2L |]
            |]

    let rows = ok (Op.toArray (UnlimitedOps.distinct op))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal(SqlValue.Int 1L, rows.[0].[0])
    Assert.Equal(SqlValue.Int 2L, rows.[1].[0])

[<Fact>]
let ``distinct: 上限を超えて退避しても結果と順序が変わらない`` () =
    withDir "distinct-spill" (fun dir ->
        // 37 種類の行が繰り返し現れる（300 行）。
        let op () =
            opOfRows
                [
                    for i in 0..299 -> row [ SqlValue.Int(int64 (i % 37)); SqlValue.Int(int64 (i % 37)) ]
                ]

        let baseline = ok (Op.toArray (UnlimitedOps.distinct (op ())))

        // 1行目から退避する極小バジェット。
        let spilled = ok (Op.toArray (OpsDistinct.distinct (tightBudget dir 1L) (op ())))

        // メモリと退避が混ざる中間のバジェット。
        let mixed = ok (Op.toArray (OpsDistinct.distinct (tightBudget dir 2000L) (op ())))
        Assert.Equal<int>(37, baseline.Length)
        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        Assert.Equal<string>(rowsText baseline, rowsText mixed)
        // 最初に現れた順序（0, 1, 2, ...）が保たれている。
        Assert.Equal<int64>(0L, asInt baseline.[0].[0])
        Assert.Equal<int64>(36L, asInt baseline.[36].[0])

        // 一時ファイルは残らない。
        assertNoLeftoversDetailed dir)
