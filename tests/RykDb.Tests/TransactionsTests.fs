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

// ===========================================================================
// Snapshot / MVCC の可視性
// ===========================================================================

[<Fact>]
let ``Snapshot: commit 済み SeqNo を表す`` () =
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
let ``MVCC: 古い snapshot からは commit 前の版が見える`` () =
    withDir "mv1" (fun dir ->
        let engine = StorageEngine.openDatabase dir |> ok
        let engine = StorageEngine.put engine (rowKey 1) ([| SqlValue.Int 1L |]) |> ok

        let snapBefore = engine.NextSeqNo - 1UL
        let engine = StorageEngine.put engine (rowKey 1) ([| SqlValue.Int 99L |]) |> ok

        // memtable には同じキーの 2 版がある
        Assert.Equal(2, engine.Mem.Count)

        let newest = StorageEngine.getAt engine (Some(engine.NextSeqNo)) (rowKey 1) |> ok

        match newest with
        | Some e -> Assert.Equal<int64>(99L, asInt e.Value.Value.[0])
        | None -> Assert.Fail("最新版が見つからないはず")

        let older = StorageEngine.getAt engine (Some snapBefore) (rowKey 1) |> ok

        match older with
        | Some e -> Assert.Equal<int64>(1L, asInt e.Value.Value.[0])
        | None -> Assert.Fail("古い版が見つからないはず")

        // snapshot が古すぎると「まだ書かれていない」ことになる
        let none = StorageEngine.getAt engine (Some 0UL) (rowKey 1) |> ok
        Assert.True(none.IsNone))

[<Fact>]
let ``MVCC: scan も snapshot で版を絞る`` () =
    withDir "mv2" (fun dir ->
        let engine = StorageEngine.openDatabase dir |> ok
        let engine = StorageEngine.put engine (rowKey 1) ([| SqlValue.Int 1L |]) |> ok
        let snapBefore = engine.NextSeqNo - 1UL
        let engine = StorageEngine.put engine (rowKey 1) ([| SqlValue.Int 2L |]) |> ok
        let engine = StorageEngine.put engine (rowKey 2) ([| SqlValue.Int 20L |]) |> ok

        let old = StorageEngine.scanAt engine (Some snapBefore) [||] 0 |> ok
        Assert.Equal(1, old.Length)
        Assert.Equal<int64>(1L, asInt old.[0].Value.Value.[0])

        let all = StorageEngine.scanAt engine None [||] 0 |> ok
        Assert.Equal(2, all.Length))

[<Fact>]
let ``MVCC: flush 後も古い snapshot が効く`` () =
    withDir "mv3" (fun dir ->
        let engine = StorageEngine.openDatabase dir |> ok
        let engine = StorageEngine.put engine (rowKey 1) ([| SqlValue.Int 1L |]) |> ok
        let engine = StorageEngine.flush engine |> ok

        // 古い版が SSTable にある状態で新しい版を書く
        let engine = StorageEngine.put engine (rowKey 1) ([| SqlValue.Int 2L |]) |> ok

        let old = StorageEngine.getAt engine (Some 1UL) (rowKey 1) |> ok

        match old with
        | Some e -> Assert.Equal<int64>(1L, asInt e.Value.Value.[0])
        | None -> Assert.Fail("SSTable にある古い版が見つからないはず")

        let latest = StorageEngine.getAt engine None (rowKey 1) |> ok

        match latest with
        | Some e -> Assert.Equal<int64>(2L, asInt e.Value.Value.[0])
        | None -> Assert.Fail("memtable にある新しい版が見つからないはず"))

// ---------------------------------------------------------------------------
// Transaction: 基本操作
// ---------------------------------------------------------------------------

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
let ``Transaction: 同じ transaction 内の二重書き込みは 1 件になる`` () =
    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put txn "t:x" [| 1uy |] [| SqlValue.Int 1L |]
    let txn = Transaction.put txn "t:x" [| 1uy |] [| SqlValue.Int 99L |]

    let batches, _ = Transaction.toEntries 1UL txn
    let total = batches |> List.sumBy (fun (_, es) -> Array.length es)

    Assert.Equal(1, total)

// ---------------------------------------------------------------------------
// TransactionManager: abort と write-write 競合検出
// ---------------------------------------------------------------------------

[<Fact>]
let ``Transaction: abort は何も残さない`` () =
    let mgr = TransactionManager.create ()
    let mgr, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put txn "t:x" [| 1uy |] [| SqlValue.Int 1L |]

    Assert.Equal(1, activeCount mgr)

    let mgr = TransactionManager.abort mgr txn.TxnId

    Assert.Equal(0, activeCount mgr)
    Assert.Equal<uint64>(0UL, currentSeqNo mgr)

[<Fact>]
let ``Transaction: 同じキーを 2 つの transaction が書くと後者が ConflictError`` () =
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
let ``Transaction: 別 keyspace の同じキーは衝突しない`` () =
    // keyspace シャーディングの設計意図：別ツリーなら衝突しない
    let mgr = TransactionManager.create ()
    let mgr, t1 = TransactionManager.beginTxn mgr
    let t1 = Transaction.put t1 "t:x" [| 1uy |] [| SqlValue.Int 1L |]
    let mgr, seq1, _ = TransactionManager.commit mgr t1 |> ok

    let mgr, t2 = TransactionManager.beginTxn mgr
    let t2 = Transaction.put t2 "i:x" [| 1uy |] [| SqlValue.Int 2L |]
    let mgr, seq2, _ = TransactionManager.commit mgr t2 |> ok

    Assert.True(seq2 > seq1)
