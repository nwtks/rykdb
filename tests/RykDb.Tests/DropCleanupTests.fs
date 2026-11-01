module RykDb.DropCleanupTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``DropCleanup: 未完了の後始末対象を追加・復元・消去できる`` () =
    withDir "drop-cleanup-journal" (fun dir ->
        ok (DropCleanup.write dir [ "t:a" ])
        ok (DropCleanup.add dir [ "i:b"; "t:a" ])
        Assert.Equal<string list>([ "t:a"; "i:b" ], ok (DropCleanup.pending dir))
        ok (DropCleanup.clear dir)
        Assert.Empty(ok (DropCleanup.pending dir)))
