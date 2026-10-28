// ===========================================================================
// Wal.fs（書き込みログ）
// ===========================================================================

module RykDb.WalTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// ディレクトリを用意して WAL を 1つ作る（ディスク経路のテスト用）。
let private freshWal (dir: string) =
    ok (Manifest.ensureDirectory dir)
    ok (Wal.create dir 1L)

[<Fact>]
let ``Wal.foldCommitted: コミット済みだけを順に再生する`` () =
    withDir "wal-fold" (fun dir ->
        let w0 = freshWal dir

        let e1 = Entry.put (intKey 1) 1UL (intRow 1)
        let e2 = Entry.put (intKey 2) 2UL (intRow 2)
        let e3 = Entry.put (intKey 3) 3UL (intRow 3)

        let records =
            [
                WalRecord.Begin 1UL
                WalRecord.Batch(1UL, [| e1 |])
                WalRecord.Commit 1UL
                WalRecord.Begin 2UL
                WalRecord.Batch(2UL, [| e2 |])
                WalRecord.Abort 2UL
                WalRecord.Begin 3UL
                WalRecord.Batch(3UL, [| e3 |])
                WalRecord.Commit 3UL
            ]

        let _ = ok (Wal.append dir records w0)

        let replayed =
            ok (Wal.foldCommitted dir 1L (fun acc entry -> entry :: acc) []) |> List.rev

        Assert.Equal<uint64 list>([ 1UL; 3UL ], replayed |> List.map _.SeqNo))

[<Fact>]
let ``Wal.walFiles: 存在しないディレクトリの walFiles は空`` () =
    let missing =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), "rykdb-wal-missing-" + string System.DateTime.UtcNow.Ticks)

    Assert.Empty(Wal.walFiles missing)
