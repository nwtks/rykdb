// ===========================================================================
// Engine.fs のテスト: INSERT / UPDATE / DELETE
// ===========================================================================

module RykDb.EngineTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``StatementResult.ToDisplayLines: 表示行はヘッダ・区切り・行を並べる`` () =
    Assert.Equal<string list>([ "(0 rows)" ], StatementResult.Empty.ToDisplayLines())

    let r: StatementResult =
        ok (StatementResult.OfRows(0, [| "a"; "b" |], [| [| SqlValue.Int 1L; SqlValue.Text "x" |] |]))

    Assert.Equal<string list>([ "a | b"; "-----"; "  1, 'x'" ], r.ToDisplayLines())

[<Fact>]
let ``StatementResult.ToDisplayLines: 0行の SELECT でもヘッダと区切りは出る`` () =
    let r: StatementResult = ok (StatementResult.OfRows(0, [| "id"; "v" |], [||]))
    Assert.Equal<string list>([ "id | v"; "------" ], r.ToDisplayLines())

[<Fact>]
let ``StatementResult.OfRows: Unloaded を含む行は拒否する`` () =
    expectErrorKind
        ErrorKind.InternalError
        "Unloaded を含む結果は作れない"
        (StatementResult.OfRows(0, [| "a" |], [| [| SqlValue.Unloaded |] |]))
