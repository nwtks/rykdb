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

        let replayed, maxTxnId =
            ok (Wal.foldCommitted dir 1L (fun acc entry -> entry :: acc) [])

        Assert.Equal<uint64 list>([ 1UL; 3UL ], List.rev replayed |> List.map _.SeqNo)
        // Abort された txn 2 も含め、WAL に現れた最大 txnId を返す
        Assert.Equal(3UL, maxTxnId))

[<Fact>]
let ``Wal.walFiles: 存在しないディレクトリの walFiles は空`` () =
    let missing =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), "rykdb-wal-missing-" + string System.DateTime.UtcNow.Ticks)

    Assert.Empty(Wal.walFiles missing)

[<Fact>]
let ``Wal.foldCommitted: 任意の位置で切っても commit 済み prefix だけ再生する`` () =
    withDir "wal-truncate" (fun dir ->
        let w0 = freshWal dir
        let random = System.Random 20261116

        let records =
            [
                for txnId in 1UL .. 20UL do
                    let committed = random.Next 2 = 0

                    yield WalRecord.Begin txnId

                    for i in 1 .. random.Next(0, 3) do
                        let key = intKey (int txnId * 10 + i)
                        let entry = Entry.put key (txnId * 10UL + uint64 i) (intRow (int txnId * 10 + i))
                        yield WalRecord.Batch(txnId, [| entry |])

                    yield
                        if committed then
                            WalRecord.Commit txnId
                        else
                            WalRecord.Abort txnId
            ]

        let _ = ok (Wal.append dir records w0)
        let path = Wal.path dir 1L
        let bytes = System.IO.File.ReadAllBytes path

        let replay () =
            let entries, _ = ok (Wal.foldCommitted dir 1L (fun acc entry -> entry :: acc) [])
            List.rev entries

        let expected = replay ()

        for length in 0 .. bytes.Length do
            let truncated = if length = 0 then [||] else bytes.[0 .. length - 1]

            System.IO.File.WriteAllBytes(path, truncated)
            let actual = replay ()

            Assert.True(
                actual.Length <= expected.Length && List.take actual.Length expected = actual,
                sprintf "切断位置 %d で commit 済み prefix 以外が再生された" length
            ))
