// ===========================================================================
// Transactions.fs のテスト: MVCC の可視性・採番・abort・競合検出
// ===========================================================================

module RykDb.TransactionsTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private rowKey (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]

let private isVisible (snap: Snapshot) (seqNo: uint64) = seqNo <= snap.SeqNo

let private currentSeqNo (mgr: TransactionManager) = mgr.LastCommitSeqNo

let private snapshot (mgr: TransactionManager) : Snapshot =
    { Snapshot.SeqNo = mgr.LastCommitSeqNo }

let private activeCount (mgr: TransactionManager) = mgr.Active.Count

[<Fact>]
let ``Transaction.create: 空の transaction`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn = Transaction.create 1UL snap
    Assert.Equal(1UL, txn.TxnId)
    Assert.Equal(0UL, txn.Snapshot.SeqNo)
    Assert.True(Map.isEmpty txn.Buffers)

[<Fact>]
let ``Transaction.put / remove: Writes に反映される`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn0 = Transaction.create 1UL snap
    let txn1 = Transaction.put txn0 "t:people" (rowKey 1) [| SqlValue.Int 1L |]
    Assert.True(Map.containsKey "t:people" txn1.Buffers)

    let txn2 = Transaction.remove txn1 "t:people" (rowKey 1)
    // remove は slot を None にする（tombstone）
    let batch = txn2.Buffers.["t:people"]
    Assert.True(batch |> Map.exists (fun _ v -> v.IsNone))

[<Fact>]
let ``Transaction.put: 同じ transaction 内の二重書き込みは 1 件になる`` () =
    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put txn "t:x" [| 1uy |] [| SqlValue.Int 1L |]
    let txn = Transaction.put txn "t:x" [| 1uy |] [| SqlValue.Int 99L |]
    let batches, _ = Transaction.toEntries 1UL txn
    let total = batches |> List.sumBy (fun (_, es) -> Array.length es)
    Assert.Equal(1, total)

[<Fact>]
let ``Transaction.conflictKey: keyspace と key を連結`` () =
    let k = Transaction.conflictKey "t:people" (rowKey 1)
    Assert.True(k.StartsWith "t:people:")

[<Fact>]
let ``Transaction.writeKeys: 書き込みキーを返す`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn0 = Transaction.create 1UL snap
    let txn1 = Transaction.put txn0 "t:people" (rowKey 1) [| SqlValue.Int 1L |]
    let keys = Transaction.writeKeys txn1
    Assert.Equal(1, keys.Count)

[<Fact>]
let ``Transaction: read-only な transaction は keys が空`` () =
    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr
    Assert.Empty(Transaction.writeKeys txn)

[<Fact>]
let ``TransactionManager.abort: abort は何も残さない`` () =
    let mgr = TransactionManager.create ()
    let mgr, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put txn "t:x" [| 1uy |] [| SqlValue.Int 1L |]
    Assert.Equal(1, activeCount mgr)

    let mgr = TransactionManager.abort mgr txn.TxnId
    Assert.Equal(0, activeCount mgr)
    Assert.Equal<uint64>(0UL, currentSeqNo mgr)

[<Fact>]
let ``TransactionManager.commit: commit 済み SeqNo を表す`` () =
    let mgr = TransactionManager.create ()
    Assert.Equal<uint64>(0UL, currentSeqNo mgr)

    let mgr, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put txn "t:x" [| 1uy |] [| SqlValue.Int 1L |]
    let mgr, seq1, _ = TransactionManager.commit mgr txn |> ok
    let snap = snapshot mgr
    Assert.Equal<uint64>(seq1, snap.SeqNo)
    Assert.True(isVisible snap seq1)
    Assert.False(isVisible snap (seq1 + 1UL))

[<Fact>]
let ``TransactionManager.commit: 同じキーを 2 つの transaction が書くと後者が ConflictError`` () =
    let mgr = TransactionManager.create ()
    let mgr, t1 = TransactionManager.beginTxn mgr
    let t1 = Transaction.put t1 "t:x" [| 1uy |] [| SqlValue.Int 1L |]
    let mgr, _, _ = TransactionManager.commit mgr t1 |> ok

    // t2 は snapshot を撮ってから t3 に同じキーを書かれた
    let mgr, t2 = TransactionManager.beginTxn mgr
    let t2 = Transaction.put t2 "t:y" [| 1uy |] [| SqlValue.Int 2L |]

    let mgr, t3 = TransactionManager.beginTxn mgr
    let t3 = Transaction.put t3 "t:y" [| 1uy |] [| SqlValue.Int 3L |]
    let mgr, _, _ = TransactionManager.commit mgr t3 |> ok
    expectErrorKind ErrorKind.ConflictError "write-write 競合を検出できなかった" (TransactionManager.commit mgr t2)

[<Fact>]
let ``TransactionManager.commit: 別 keyspace の同じキーは衝突しない`` () =
    // keyspace シャーディングの設計意図：別ツリーなら衝突しない
    let mgr = TransactionManager.create ()
    let mgr, t1 = TransactionManager.beginTxn mgr
    let t1 = Transaction.put t1 "t:x" [| 1uy |] [| SqlValue.Int 1L |]
    let mgr, seq1, _ = TransactionManager.commit mgr t1 |> ok

    let mgr, t2 = TransactionManager.beginTxn mgr
    let t2 = Transaction.put t2 "i:x" [| 1uy |] [| SqlValue.Int 2L |]
    let _, seq2, _ = TransactionManager.commit mgr t2 |> ok
    Assert.True(seq2 > seq1)

[<Fact>]
let ``TransactionManager.commit: abort 済み transaction の commit はエラー`` () =
    let mgr = TransactionManager.create ()
    let mgr, txn = TransactionManager.beginTxn mgr
    let mgr = TransactionManager.abort mgr txn.TxnId

    // Active から消えているので内部エラーになる
    expectErrorKind ErrorKind.InternalError "abort 済みの commit" (TransactionManager.commit mgr txn)
