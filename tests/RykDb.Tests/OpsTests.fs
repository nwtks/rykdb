// ===========================================================================
// Ops.fs のテスト（filter・project・列解決）
// ===========================================================================

module RykDb.OpsTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers
open RykDb.TestHelpers.OpsFixtures

[<Fact>]
let ``project: 項目が空なら入力をそのまま返す`` () =
    let op = opOfRows [ row [ SqlValue.Int 1L ] ]
    let rows = ok (Op.toArray (ok (Ops.project schema2 schema2 [] [] op)))
    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(1L, asInt rows.[0].[0])

[<Fact>]
let ``列解決: 未知の列は行が 0 件でも演算子の構築時に Error`` () =
    // 入力が 0 行だと述語・射影・並べ替えは一度も評価されない。
    // 列解決は構築時に検査し、行数に依存せず NameError にする。
    let sortKey =
        {
            Expression = col "nope"
            Direction = SortDirection.Ascending
        }

    let item =
        {
            Expression = col "nope"
            Alias = None
        }

    expectErrorKind ErrorKind.NameError "filter が OK になっています" (UnlimitedOps.filter schema2 (col "nope") Op.Empty)
    expectErrorKind ErrorKind.NameError "sortBy が OK になっています" (UnlimitedOps.sortBy schema2 [ sortKey ] Op.Empty)
    expectErrorKind ErrorKind.NameError "project が OK になっています" (Ops.project schema2 schema2 [] [ item ] Op.Empty)

    expectErrorKind
        ErrorKind.NameError
        "aggregate が OK になっています"
        (UnlimitedOps.aggregate schema2 [ col "nope" ] [ (AggFunc.Count, true, false, Expr.Star) ] Op.Empty)
