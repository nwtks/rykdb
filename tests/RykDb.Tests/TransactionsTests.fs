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

let private validate (m: TransactionManager) =
    let fail msg =
        err (DbError.internalError ("TransactionManager: " + msg))

    result {
        do!
            if m.NextSeqNo > m.LastCommitSeqNo then
                ret ()
            else
                fail "NextSeqNo が LastCommitSeqNo 以下"

        do!
            if m.NextTxnId > 0UL<txnId> then
                ret ()
            else
                fail "NextTxnId が 0"

        do!
            if m.Active |> Map.forall (fun id txn -> txn.TxnId = id && id < m.NextTxnId) then
                ret ()
            else
                fail "Active の TxnId が不整合"

        do!
            if m.Active |> Map.forall (fun _ txn -> txn.Snapshot.SeqNo <= m.LastCommitSeqNo) then
                ret ()
            else
                fail "Active の snapshot が LastCommitSeqNo より未来"

        do!
            if
                m.LastCommitted
                |> Map.forall (fun _ seq -> seq > 0UL && seq <= m.LastCommitSeqNo)
            then
                ret ()
            else
                fail "LastCommitted の SeqNo が範囲外"

        do!
            if m.LastCommitted |> Map.forall (fun _ seq -> seq < m.NextSeqNo) then
                ret ()
            else
                fail "NextSeqNo が LastCommitted 以下"

        return ()
    }

[<Fact>]
let ``Transaction.create: 空の transaction`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn = Transaction.create 1UL<txnId> snap
    Assert.Equal<uint64<txnId>>(1UL<txnId>, txn.TxnId)
    Assert.Equal(0UL, txn.Snapshot.SeqNo)
    Assert.True(Map.isEmpty txn.Buffers)

[<Fact>]
let ``Transaction.put / remove: Writes に反映される`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn0 = Transaction.create 1UL<txnId> snap
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
let ``Transaction: put したキー・行配列を後から書き換えてもバッファは保たれる`` () =
    let key = rowKey 1
    let row = [| SqlValue.Int 1L |]

    let txn =
        Transaction.put "t:x" key row (Transaction.create 1UL<txnId> (snapshot (TransactionManager.create ())))

    // put の後に呼び出し側が配列を書き換える（所有権を越えた変更）。
    key.[0] <- 0xFFuy
    row.[0] <- SqlValue.Int 999L

    match Transaction.tryFind "t:x" (rowKey 1) txn with
    | Some(Some v) -> Assert.Equal<int64>(1L, asInt v.[0])
    | Some None -> Assert.Fail "put した行が tombstone になった"
    | None -> Assert.Fail "put したキーが見つからない（バッファが壊れた）"

[<Fact>]
let ``Transaction: ApproxBytes は値の上書きで減る`` () =
    let snap = { Snapshot.SeqNo = 0UL }
    let txn0 = Transaction.create 1UL<txnId> snap

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
    let txn0 = Transaction.create 1UL<txnId> snap
    let txn1 = Transaction.put "t:people" (rowKey 1) [| SqlValue.Int 1L |] txn0
    let keys = Transaction.writeKeys txn1
    Assert.Equal(1, keys.Count)

[<Fact>]
let ``Transaction.reserveUnique: 同じ索引値の予約は 1 件にまとまる`` () =
    let txn0 = Transaction.create 1UL<txnId> { Snapshot.SeqNo = 0UL }
    let key = KeyBuilder.ofValues [| SqlValue.Text "x@example.com" |]
    let txn1 = Transaction.reserveUnique "idx_email" key txn0
    let txn2 = Transaction.reserveUnique "idx_email" key txn1
    Assert.Equal(1, txn2.Reservations.Count)
    Assert.Equal(txn1.ApproxBytes, txn2.ApproxBytes)

[<Fact>]
let ``TransactionManager.commit: UNIQUE の論理予約が競合する`` () =
    let mgr0 = TransactionManager.create ()
    let mgr1, txnA = TransactionManager.beginTxn mgr0
    let mgr2, txnB = TransactionManager.beginTxn mgr1
    let key = KeyBuilder.ofValues [| SqlValue.Text "x@example.com" |]
    let txnA = Transaction.reserveUnique "idx_email" key txnA
    let txnB = Transaction.reserveUnique "idx_email" key txnB
    let mgr3, _, _ = ok (TransactionManager.commit txnA mgr2)
    expectErrorKind ErrorKind.ConflictError "UNIQUE 予約の競合を検出できない" (TransactionManager.commit txnB mgr3)

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
let ``TransactionManager.validate: 初期状態は妥当`` () =
    ok (validate (TransactionManager.create ())) |> ignore

[<Fact>]
let ``TransactionManager.validate: NextSeqNo の逆転を検出`` () =
    let mgr =
        { TransactionManager.create () with
            NextSeqNo = 1UL
            LastCommitSeqNo = 1UL
        }

    expectErrorKind ErrorKind.InternalError "NextSeqNo の逆転を検出するはず" (validate mgr)

[<Fact>]
let ``TransactionManager.validate: Active の snapshot が未来なら検出`` () =
    let mgr, txn = TransactionManager.beginTxn (TransactionManager.create ())

    let broken =
        { mgr with
            Active =
                Map.add
                    txn.TxnId
                    { txn with
                        Snapshot =
                            {
                                Snapshot.SeqNo = mgr.LastCommitSeqNo + 1UL
                            }
                    }
                    mgr.Active
        }

    expectErrorKind ErrorKind.InternalError "Active の snapshot が未来を検出するはず" (validate broken)

[<Fact>]
let ``TransactionManager.validate: LastCommitted の範囲外を検出`` () =
    let mgr =
        { TransactionManager.create () with
            LastCommitted = Map.ofList [ ("k", 1UL) ]
        }

    expectErrorKind ErrorKind.InternalError "LastCommitted の範囲外を検出するはず" (validate mgr)

[<Fact>]
let ``TransactionManager.validate: begin/put/commit/abort を繰り返しても不変条件を保つ`` () =
    let random = System.Random 20261111
    let mutable mgr = TransactionManager.create ()
    let mutable active: Transaction list = []

    for _ in 1..200 do
        match random.Next 3 with
        | 0 ->
            let next, txn = TransactionManager.beginTxn mgr
            mgr <- next
            active <- txn :: active
        | 1 ->
            match active with
            | txn :: rest ->
                let txn =
                    Transaction.put "t:x" [| byte (random.Next 4) |] [| SqlValue.Int(int64 (random.Next 100)) |] txn

                active <- txn :: rest
            | [] -> ()
        | _ ->
            match active with
            | txn :: rest ->
                match TransactionManager.commit txn mgr with
                | Ok(next, _, _) -> mgr <- next
                | Error _ -> mgr <- TransactionManager.abort txn mgr

                active <- rest
            | [] -> ()

        ok (validate mgr) |> ignore

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
    // 論理 keyspace が違えば物理キーも接頭辞で分かれるので衝突しない
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
let ``TransactionManager.commit: active が無ければ commit 直後に LastCommitted を捨てる`` () =
    let mgr = TransactionManager.create ()
    let mgr, t1 = TransactionManager.beginTxn mgr
    let t1 = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 1L |] t1
    let mgr, seq1, _ = TransactionManager.commit t1 mgr |> ok
    // 衝突し得る active な transaction が居ないので、commit 直後に捨てられる
    Assert.Equal<int>(0, mgr.LastCommitted.Count)
    Assert.Equal<uint64>(seq1, mgr.LastCommitSeqNo)

    // 次の commit でも空のまま
    let mgr, t2 = TransactionManager.beginTxn mgr
    let mgr, _, _ = TransactionManager.commit t2 mgr |> ok
    Assert.Equal<int>(0, mgr.LastCommitted.Count)

    // commit 済みキーへの後続の書き込みは競合しない（snapshot が commit 後）
    let mgr, t3 = TransactionManager.beginTxn mgr
    let t3 = Transaction.put "t:x" [| 1uy |] [| SqlValue.Int 2L |] t3
    let mgr, seq3, _ = TransactionManager.commit t3 mgr |> ok
    Assert.True(seq3 > seq1)
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

[<Fact>]
let ``ActiveSnapshotRegistry: 並行 register/unregister で登録が消えない`` () =
    let root = freshRoot "reg-race"

    // 登録が bucket に載っている間は oldestExcept が必ず Some を返す。
    // bucket の GetOrAdd と空 bucket の削除が競合すると None になる。
    System.Threading.Tasks.Parallel.For(
        0,
        5000,
        fun i ->
            let registration = ActiveSnapshotRegistry.register root (uint64 i + 1UL)
            Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsSome)
            ActiveSnapshotRegistry.unregister root registration
    )
    |> ignore

    Assert.Equal<uint64 option>(None, ActiveSnapshotRegistry.oldestExcept root None)
