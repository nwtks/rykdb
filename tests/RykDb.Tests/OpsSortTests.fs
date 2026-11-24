// ===========================================================================
// OpsSort.fs のテスト（sortBy）
// ===========================================================================

module RykDb.OpsSortTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers
open RykDb.TestHelpers.OpsFixtures

[<Fact>]
let ``sortBy: 昇順と降順`` () =
    let op: Op =
        Op.ofArray [| [| SqlValue.Int 3L |]; [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    let keys =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
        ]

    let sorted = ok (UnlimitedOps.sortBy schema2 keys op)
    let rows = ok (Op.toArray sorted)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal<int64>(3L, asInt rows.[2].[0])

    let desc =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Descending
            }
        ]

    let rows2 = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 desc op)))
    Assert.Equal<int64>(3L, asInt rows2.[0].[0])

[<Fact>]
let ``sortBy: NULL は昇順で先頭、降順で末尾`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 3L; SqlValue.Int 0L ]
                row [ SqlValue.Null; SqlValue.Int 0L ]
                row [ SqlValue.Int 1L; SqlValue.Int 0L ]
                row [ SqlValue.Null; SqlValue.Int 0L ]
                row [ SqlValue.Int 2L; SqlValue.Int 0L ]
            ]

    let ascKey =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
        ]

    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 ascKey op)))
    Assert.True(SqlValue.isNull rows.[0].[0])
    Assert.True(SqlValue.isNull rows.[1].[0])
    Assert.Equal<int64 list>([ 1L; 2L; 3L ], [ for r in 2..4 -> asInt rows.[r].[0] ])

    let descKey =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Descending
            }
        ]

    let rowsDesc = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 descKey op)))
    Assert.Equal<int64 list>([ 3L; 2L; 1L ], [ for r in 0..2 -> asInt rowsDesc.[r].[0] ])
    Assert.True(SqlValue.isNull rowsDesc.[3].[0])
    Assert.True(SqlValue.isNull rowsDesc.[4].[0])

[<Fact>]
let ``sortBy: 複数キーは左から優先`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 20L ]
                row [ SqlValue.Int 1L; SqlValue.Int 10L ]
                row [ SqlValue.Int 0L; SqlValue.Int 99L ]
            ]

    let keys =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
            {
                Expression = col "b"
                Direction = SortDirection.Ascending
            }
        ]

    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 keys op)))
    // 0/99, 1/10, 1/20
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.Equal<int64>(10L, asInt rows.[1].[1])
    Assert.Equal<int64>(20L, asInt rows.[2].[1])

[<Fact>]
let ``sortBy: キーが空なら入力をそのまま返す`` () =
    let op = opOfRows [ row [ SqlValue.Int 2L ]; row [ SqlValue.Int 1L ] ]
    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 [] op)))
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])

[<Fact>]
let ``sortBy: 式のキーで並べ替えられる`` () =
    let op =
        opOfRows
            [
                row [ SqlValue.Int 2L; SqlValue.Int 0L ]
                row [ SqlValue.Int 1L; SqlValue.Int 0L ]
            ]

    let keys =
        [
            {
                Expression = Expr.Binary(BinaryOp.Add, col "a", Expr.Literal(Literal.Int 0L))
                Direction = SortDirection.Ascending
            }
        ]

    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 keys op)))
    Assert.Equal<int64>(1L, asInt rows.[0].[0])
    Assert.Equal<int64>(2L, asInt rows.[1].[0])

[<Fact>]
let ``sortBy: 同値のキーは次のキーで、比較不能なキーは Error になる`` () =
    // 1 列目に同じ TEXT を入れて同値にし、2 列目の差で順序が決まるようにする。
    let op =
        opOfRows
            [
                row [ SqlValue.Text "x"; SqlValue.Int 2L ]
                row [ SqlValue.Text "x"; SqlValue.Int 1L ]
            ]

    let keys =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
            {
                Expression = col "b"
                Direction = SortDirection.Ascending
            }
        ]

    // 1 列目は同値（TEXT 同士）→ 2 列目で比較する
    let rows = ok (Op.toArray (ok (UnlimitedOps.sortBy schema2 keys op)))
    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

    // キーが 1 本だけで比較不能（型が合わない）なら Error になる（握り潰さない）
    let op2 =
        opOfRows
            [
                row [ SqlValue.Int 1L; SqlValue.Int 10L ]
                row [ SqlValue.Text "x"; SqlValue.Int 20L ]
            ]

    let keys1 =
        [
            {
                Expression = col "a"
                Direction = SortDirection.Ascending
            }
        ]

    // キーが 1 本だけで比較不能（型が合わない）なら Error になる（握り潰さない）
    let r1 = UnlimitedOps.sortBy schema2 keys1 op2
    expectErrorKind ErrorKind.TypeError "比較不能なキーは Error になる" r1

[<Fact>]
let ``sortBy: 式の評価エラーは Error になる`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L; SqlValue.Int 0L ] ]

    let keys =
        [
            {
                Expression = Expr.Binary(BinaryOp.Add, col "nope", Expr.Literal(Literal.Int 1L))
                Direction = SortDirection.Ascending
            }
        ]

    UnlimitedOps.sortBy schema2 keys op
    |> expectErrorKind ErrorKind.NameError "ORDER BY の存在しない列が OK になっています"

[<Fact>]
let ``sortBy: 集約後の行を集約呼び出し・GROUP BY 式で並べ替えられる`` () =
    // 集約後の行 = [GROUP BY 値; 集約値]。ORDER BY の集約式・GROUP BY 式は
    // 計画時にスロット参照へ置き換えて評価する。
    let op: Op =
        Op.ofArray
            [|
                [| SqlValue.Int 1L; SqlValue.Int 2L |]
                [| SqlValue.Int 2L; SqlValue.Int 5L |]
                [| SqlValue.Int 3L; SqlValue.Int 1L |]
            |]

    let groupBy = [ col "a" ]
    let aggs = [ (AggFunc.Count, true, false, Expr.Star) ]

    let byAgg =
        [
            {
                Expression = Expr.AggCall(AggFunc.Count, false, Expr.Star)
                Direction = SortDirection.Descending
            }
        ]

    let rows =
        ok (Op.toArray (ok (OpsSort.sortBy MemoryBudget.Unlimited schema2 groupBy aggs byAgg op)))

    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(1L, asInt rows.[1].[0])
    Assert.Equal<int64>(3L, asInt rows.[2].[0])

    // GROUP BY 式（`a + 0`）とスコープに無い集約はエラーになる（NULL で偽にしない）
    let byGroupExpr =
        [
            {
                Expression = Expr.Binary(BinaryOp.Add, col "a", Expr.Literal(Literal.Int 0L))
                Direction = SortDirection.Descending
            }
        ]

    let rows2 =
        ok (Op.toArray (ok (OpsSort.sortBy MemoryBudget.Unlimited schema2 groupBy aggs byGroupExpr op)))

    Assert.Equal<int64>(3L, asInt rows2.[0].[0])

    let unknown =
        [
            {
                Expression = col "zz"
                Direction = SortDirection.Ascending
            }
        ]

    expectErrorKind
        ErrorKind.NameError
        "集約後に存在しない列を ORDER BY しても OK になっています"
        (OpsSort.sortBy MemoryBudget.Unlimited schema2 groupBy aggs unknown op)
