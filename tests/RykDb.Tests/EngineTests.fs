// ===========================================================================
// Engine.fs のテスト: INSERT / UPDATE / DELETE
// ===========================================================================

module RykDb.EngineTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``StatementResult: 表示行はヘッダ・区切り・行を並べる`` () =
    Assert.Equal<string list>([ "(0 rows)" ], StatementResult.Empty.ToDisplayLines())

    let r =
        {
            RowsAffected = 0
            Columns = [| "a"; "b" |]
            Rows = [| [| SqlValue.Int 1L; SqlValue.Text "x" |] |]
        }

    Assert.Equal<string list>([ "a | b"; "-----"; "  1, 'x'" ], r.ToDisplayLines())
