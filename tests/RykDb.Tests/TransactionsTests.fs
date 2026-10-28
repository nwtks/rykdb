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
    let txn1 = Transaction.put "t:people" (rowKey 1) [| SqlValue.Int 1L |] txn0
    Assert.True(Map.containsKey "t:people" txn1.Buffers)

    let txn2 = Transaction.remove "t:people" (rowKey 1) txn1
    // remove は slot を None にする（tombstone）
    let batch = txn2.Buffers.["t:people"]
    Assert.True(batch |> Map.exists (fun _ v -> v.IsNone))

[<Fact>]
let ``Transaction.put: 同じ transaction 内の二重書き込みは 1 件になる`` () =
    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] txn
    let txn = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 99L |] txn
    let batches, _ = Transaction.toEntries 1UL txn
    let total = batches |> List.sumBy (fun (_, es) -> Array.length es)
    Assert.Equal(1, total)

[<Fact>]
let ``Transaction: バッファ上限を超えると commit できない`` () =
    let mgr =
        { TransactionManager.create () with
            MaxTransactionBytes = 1L
        }

    let mgr, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] txn
    expectErrorKind ErrorKind.CapacityError "上限を超えた transaction を commit できてしまう" (TransactionManager.commit txn mgr)

[<Fact>]
let ``Transaction: ApproxBytes は値の上書きで減る`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn0 = Transaction.create 1UL snap

    let txn1 =
        Transaction.put "t:x" [| 1uy |] [| SqlValue.Text(System.String('x', 1000)) |] txn0

    let txn2 = Transaction.put "t:x" [| 1uy |] [| SqlValue.Text "x" |] txn1
    Assert.True(txn2.ApproxBytes < txn1.ApproxBytes)

[<Fact>]
let ``Transaction.conflictKey: keyspace と key を連結`` () =
    let k = Transaction.conflictKey "t:people" (rowKey 1)
    Assert.True(k.StartsWith "t:people:")

[<Fact>]
let ``Transaction.writeKeys: 書き込みキーを返す`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn0 = Transaction.create 1UL snap
    let txn1 = Transaction.put "t:people" (rowKey 1) [| SqlValue.Int 1L |] txn0
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
    let txn = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] txn
    Assert.Equal(1, activeCount mgr)

    let mgr = TransactionManager.abort txn mgr
    Assert.Equal(0, activeCount mgr)
    Assert.Equal<uint64>(0UL, currentSeqNo mgr)

[<Fact>]
let ``TransactionManager.commit: commit 済み SeqNo を表す`` () =
    let mgr = TransactionManager.create ()
    Assert.Equal<uint64>(0UL, currentSeqNo mgr)

    let mgr, txn = TransactionManager.beginTxn mgr
    let txn = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] txn
    let mgr, seq1, _ = TransactionManager.commit txn mgr |> ok
    let snap = snapshot mgr
    Assert.Equal<uint64>(seq1, snap.SeqNo)
    Assert.True(isVisible snap seq1)
    Assert.False(isVisible snap (seq1 + 1UL))

[<Fact>]
let ``TransactionManager.commit: 同じキーを 2 つの transaction が書くと後者が ConflictError`` () =
    let mgr = TransactionManager.create ()
    let mgr, t1 = TransactionManager.beginTxn mgr
    let t1 = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] t1
    let mgr, _, _ = TransactionManager.commit t1 mgr |> ok

    // t2 は snapshot を撮ってから t3 に同じキーを書かれた
    let mgr, t2 = TransactionManager.beginTxn mgr
    let t2 = Transaction.put "t:y" [| 1uy |] [| SqlValue.Int 2L |] t2

    let mgr, t3 = TransactionManager.beginTxn mgr
    let t3 = Transaction.put "t:y" [| 1uy |] [| SqlValue.Int 3L |] t3
    let mgr, _, _ = TransactionManager.commit t3 mgr |> ok
    expectErrorKind ErrorKind.ConflictError "write-write 競合を検出できなかった" (TransactionManager.commit t2 mgr)

[<Fact>]
let ``TransactionManager.commit: 別 keyspace の同じキーは衝突しない`` () =
    // keyspace シャーディングの設計意図：別ツリーなら衝突しない
    let mgr = TransactionManager.create ()
    let mgr, t1 = TransactionManager.beginTxn mgr
    let t1 = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] t1
    let mgr, seq1, _ = TransactionManager.commit t1 mgr |> ok

    let mgr, t2 = TransactionManager.beginTxn mgr
    let t2 = Transaction.put "i:x" [| 1uy |] [| SqlValue.Int 2L |] t2
    let _, seq2, _ = TransactionManager.commit t2 mgr |> ok
    Assert.True(seq2 > seq1)

[<Fact>]
let ``TransactionManager.activeSnapshot: 最古の active snapshot を返す`` () =
    let mgr = TransactionManager.create ()
    // active が無ければ「次の SeqNo」
    Assert.Equal<uint64>(1UL, TransactionManager.activeSnapshot mgr)

    let mgr1, t1 = TransactionManager.beginTxn mgr
    Assert.Equal<uint64>(0UL, TransactionManager.activeSnapshot mgr1)
    let t1 = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] t1
    let mgr2, _, _ = TransactionManager.commit t1 mgr1 |> ok
    Assert.Equal<uint64>(2UL, TransactionManager.activeSnapshot mgr2)

    // t2 (snapshot 1) が残っている間は、t3 が commit しても境界は 1 のまま
    let mgr3, t2 = TransactionManager.beginTxn mgr2
    let mgr4, t3 = TransactionManager.beginTxn mgr3
    let t3 = Transaction.put "t:y" [| 2uy |] [| SqlValue.Int 2L |] t3
    let mgr5, _, _ = TransactionManager.commit t3 mgr4 |> ok
    Assert.Equal<uint64>(1UL, TransactionManager.activeSnapshot mgr5)

    let mgr6 = TransactionManager.abort t2 mgr5
    Assert.Equal<uint64>(3UL, TransactionManager.activeSnapshot mgr6)

    // SeqNo が上限なら +1 しない
    let saturated =
        { TransactionManager.create () with
            LastCommitSeqNo = System.UInt64.MaxValue
        }

    Assert.Equal<uint64>(System.UInt64.MaxValue, TransactionManager.activeSnapshot saturated)

[<Fact>]
let ``TransactionManager.commit: active が無くなれば LastCommitted も掃除する`` () =
    let mgr = TransactionManager.create ()
    let mgr, t1 = TransactionManager.beginTxn mgr
    let t1 = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] t1
    let mgr, _, _ = TransactionManager.commit t1 mgr |> ok

    // commit 直後は自分の版が残る
    Assert.Equal<int>(1, mgr.LastCommitted.Count)

    // active の無い状態で次の commit が来たら、もう衝突判定に使わないので捨てられる
    let mgr, t2 = TransactionManager.beginTxn mgr
    let mgr, _, _ = TransactionManager.commit t2 mgr |> ok
    Assert.Equal<int>(0, mgr.LastCommitted.Count)

[<Fact>]
let ``TransactionManager.commit: 古い snapshot の transaction が居る間は掃除しない`` () =
    let mgr = TransactionManager.create ()
    let mgr, old = TransactionManager.beginTxn mgr
    let mgr, t = TransactionManager.beginTxn mgr
    let w = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] t
    let mgr, _, _ = TransactionManager.commit w mgr |> ok

    // old (snapshot 0) が生きている間は、衝突検出のために版を残す
    Assert.True(mgr.LastCommitted.ContainsKey(Transaction.conflictKey "t:x" [| 1uy |]))

    let mgr = TransactionManager.abort old mgr
    let mgr, t2 = TransactionManager.beginTxn mgr
    let mgr, _, _ = TransactionManager.commit t2 mgr |> ok
    Assert.Equal<int>(0, mgr.LastCommitted.Count)

[<Fact>]
let ``TransactionManager.commit: abort 済み transaction の commit はエラー`` () =
    let mgr = TransactionManager.create ()
    let mgr, txn = TransactionManager.beginTxn mgr
    let mgr = TransactionManager.abort txn mgr

    // Active から消えているので内部エラーになる
    expectErrorKind ErrorKind.InternalError "abort 済みの commit" (TransactionManager.commit txn mgr)

/// root の重複を避ける（xUnit はテストクラス単位で並列に走る）。
let private freshRoot (tag: string) =
    System.IO.Path.Combine(System.IO.Path.GetTempPath(), tag + "-" + string System.DateTime.UtcNow.Ticks)

[<Fact>]
let ``ActiveSnapshotRegistry: root ごとに最古の active snapshot を返す`` () =
    let root = freshRoot "reg"

    let first = ActiveSnapshotRegistry.register root 10UL
    let second = ActiveSnapshotRegistry.register root 7UL

    Assert.Equal<uint64 option>(Some 7UL, ActiveSnapshotRegistry.oldestExcept root None)

    // 自分自身の登録は除外できる（これから commit する transaction）
    Assert.Equal<uint64 option>(Some 10UL, ActiveSnapshotRegistry.oldestExcept root (Some second))

    ActiveSnapshotRegistry.unregister root first
    Assert.Equal<uint64 option>(Some 7UL, ActiveSnapshotRegistry.oldestExcept root None)

    ActiveSnapshotRegistry.unregister root second
    Assert.Equal<uint64 option>(None, ActiveSnapshotRegistry.oldestExcept root None)

[<Fact>]
let ``ActiveSnapshotRegistry: 別 root は混ざらない`` () =
    let root = freshRoot "reg-a"
    let other = freshRoot "reg-b"

    let a = ActiveSnapshotRegistry.register root 5UL
    let b = ActiveSnapshotRegistry.register other 3UL

    Assert.Equal<uint64 option>(Some 5UL, ActiveSnapshotRegistry.oldestExcept root None)
    Assert.Equal<uint64 option>(Some 3UL, ActiveSnapshotRegistry.oldestExcept other None)

    ActiveSnapshotRegistry.unregister root a
    ActiveSnapshotRegistry.unregister other b

    Assert.Equal<uint64 option>(None, ActiveSnapshotRegistry.oldestExcept root None)
    Assert.Equal<uint64 option>(None, ActiveSnapshotRegistry.oldestExcept other None)

[<Fact>]
let ``ActiveSnapshotRegistry: 相対パスと絶対パスは同じ DB として扱う`` () =
    let rel = "./" + System.IO.Path.GetFileName(freshRoot "reg-path")

    let token = ActiveSnapshotRegistry.register rel 4UL

    // GetFullPath で正規化するので、同じ DB を別表記で開いても同じ束になる
    let full = System.IO.Path.GetFullPath rel

    match ActiveSnapshotRegistry.oldestExcept full None with
    | Some snap -> Assert.Equal(4UL, snap)
    | None -> Assert.Fail "相対パスで登録した snapshot が引けない"

    ActiveSnapshotRegistry.unregister full token
    Assert.Equal<uint64 option>(None, ActiveSnapshotRegistry.oldestExcept rel None)
