// ===========================================================================
// Plan.fs のテスト（SELECT の計画・実行の入口）
// ===========================================================================

module RykDb.PlanTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テスト用の ExecContext（people と orders を登録済み）。
let private ctxOf (store: RowStore) =
    ExecContext.create store None
    |> ExecContext.withSchema "people" peopleSchema
    |> ExecContext.withSchema "orders" ordersSchema

/// テスト用の文を作る。
let private selectOf (items: SelectItem list) (from: TableRef option) (joins: JoinClause list) (w: Expr option) =
    {
        Distinct = false
        Items = items
        From = Option.map TableSource.Named from
        Joins = joins
        Where = w
        GroupBy = []
        Having = None
        OrderBy = []
        Limit = None
        Offset = None
    }

[<Fact>]
let ``Plan.runSelect: FROM なしの SELECT は 1 行の定数を返す`` () =
    withStore "pl10" (fun store ->
        let ctx = ctxOf store

        let stmt =
            selectOf
                [
                    {
                        Expression = Expr.Literal(Literal.Int 1L)
                        Alias = None
                    }
                ]
                None
                []
                None

        let columns, op = ok (Plan.runSelect ctx stmt)
        let rows = ok (Op.toArray op)
        // FROM なしの入力は 1行 0列で、射影が定数を 1列にする
        Assert.Equal<string array>([| "1" |], columns)
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal<int>(1, rows.[0].Length)
        Assert.Equal<int64>(1L, asInt rows.[0].[0]))
