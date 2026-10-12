// ===========================================================================
// Transactions — MVCC の snapshot・採番・write-write 競合検出。
//
// 1. 採番: コミット済みの論理時計を 1 つに並ばせる（「DB の時刻」）
// 2. 可視性: snapshot = SeqNo <= S の書き込みだけ見える。
//    StorageEngine.getAt / scanAt が SeqNo で絞るので実装はフィルタ 1 つだけ
// 3. 競合検出: 同じキーに書いたら後から commit した方を失敗させる
//    （first-committer-wins）
//
// 書き込みはバッファに貯め、commit 時に一度だけ engine へ流す
// （read-your-writes・abort 残留なし・commit order 一意）。
// keyspace 単位の並列性は RowStore が持つ。
// 詳細は docs/architecture.md §8。
// ===========================================================================

namespace RykDb.Core

/// transaction の識別子。
type internal TxnId = uint64

/// 読み取りの可視性境界。これより大きい SeqNo の書き込みは見えない。
type internal Snapshot = { SeqNo: uint64 }

module internal Snapshot =
    /// この snapshot から見える SeqNo か（Option で「制限なし」を表す）。
    let toVisible (snap: Snapshot) = Some snap.SeqNo

    /// 制限なし。
    let noLimit: uint64 option = None

// ---------------------------------------------------------------------------
// Transaction
// ---------------------------------------------------------------------------

/// 書き込み中の transaction。commit まで DB から見えない。
type internal Transaction =
    {
        TxnId: TxnId
        /// transaction 開始時点の可視性境界
        Snapshot: Snapshot
        /// keyspace -> (キー -> 値)。同一キーは最後の書き込みが勝つ。
        Buffers: Map<string, Map<byte array, SqlValue array option>>
    }

module internal Transaction =
    let private empty (txnId: TxnId) (snap: Snapshot) =
        {
            TxnId = txnId
            Snapshot = snap
            Buffers = Map.empty
        }

    /// 書き込み可能な transaction を作る。
    let create (txnId: TxnId) (snap: Snapshot) = empty txnId snap

    /// バッファの 1 スロットを差し替える（`Some value` = 書き込み、`None` = 削除）。
    /// keyspace ごとの内側 Map が無ければ空から作る。
    /// put / remove は積む値だけが違うので、この 1 本に集約する。
    let private setSlot (txn: Transaction) (keyspace: string) (key: byte array) (slot: SqlValue array option) =
        let inner =
            match Map.tryFind keyspace txn.Buffers with
            | Some m -> m
            | None -> Map.empty

        { txn with
            Buffers = Map.add keyspace (Map.add key slot inner) txn.Buffers
        }

    /// 値を書き込む（バッファに積む）。
    let put (txn: Transaction) (keyspace: string) (key: byte array) (value: SqlValue array) =
        setSlot txn keyspace key (Some value)

    /// 削除を書き込む（バッファに積む）。
    let remove (txn: Transaction) (keyspace: string) (key: byte array) = setSlot txn keyspace key None

    /// 「(keyspace, キー)」を競合検出用の文字列 1 本に畳む。
    /// Map のキーは byte[] でも構造比較で並ぶので、文字列化は一意性を保てる。
    let conflictKey (keyspace: string) (key: byte array) =
        keyspace + ":" + System.Convert.ToBase64String(key)

    /// 競合検出用に「(keyspace, キー)」を文字列 1 本に畳んだ集合。
    let writeKeys (txn: Transaction) =
        txn.Buffers
        |> Map.toList
        |> List.collect (fun (ks, m) -> m |> Map.toList |> List.map (fun (k, _) -> conflictKey ks k))
        |> Set.ofList

    /// バッファを keyspace ごとに「SeqNo 降順で採番済みの Entry 配列」に変換する。
    /// 採番は <paramref name="nextSeqNo"/> から 1 つずつ採番する。
    /// 決定的な順序にするため keyspace 名 → キーの昇順で採番する
    /// （同じ transaction は常に同じ SeqNo 割当になる）。
    let toEntries (nextSeqNo: uint64) (txn: Transaction) =
        let rec step (items: (byte array * SqlValue array option) list) (seqNo: uint64) (acc: Entry list) =
            match items with
            | [] -> List.rev acc
            | (k, v) :: tail ->
                let e =
                    match v with
                    | Some values -> Entry.put k seqNo values
                    | None -> Entry.delete k seqNo

                step tail (seqNo + 1UL) (e :: acc)

        let rec go
            (spaces: (string * (byte array * SqlValue array option) list) list)
            (seqNo: uint64)
            (acc: (string * Entry array) list)
            =
            match spaces with
            | [] -> List.rev acc, seqNo
            | (ks, entries) :: rest ->
                let made = step entries seqNo [] |> Array.ofList
                go rest (seqNo + uint64 entries.Length) ((ks, made) :: acc)

        go (txn.Buffers |> Map.toList |> List.map (fun (ks, m) -> ks, m |> Map.toList)) nextSeqNo []

// ---------------------------------------------------------------------------
// TransactionManager
// ---------------------------------------------------------------------------

/// active な transaction 1 件分。
type internal TxnRecord =
    {
        Txn: Transaction
        /// 書き込み済みの key 集合（keyspace:キー の文字列）
        Keys: Set<string>
    }

/// transaction を管理し、commit order の一意性と write-write 競合を保証する。
/// commit は直列化する: 採番と競合検査が 1 つの临界域に入る。
/// 読み取りはこの構造体に一切触らない（snapshot を掴むだけ）。
type internal TransactionManager =
    {
        /// 次に採番する SeqNo
        NextSeqNo: uint64
        /// 次に採番する TxnId
        NextTxnId: TxnId
        /// commit 済みの transaction が最後に書いた (keyspace:キー) -> SeqNo
        LastCommitted: Map<string, uint64>
        /// アクティブな transaction
        Active: Map<TxnId, TxnRecord>
        /// 最後に commit された SeqNo（=「DB の現在時刻」）
        LastCommitSeqNo: uint64
    }

module internal TransactionManager =
    let create () =
        {
            NextSeqNo = 1UL
            NextTxnId = 1UL
            LastCommitted = Map.empty
            Active = Map.empty
            LastCommitSeqNo = 0UL
        }

    /// 現在の「DB の時刻」で transaction を始める。
    let beginTxn (mgr: TransactionManager) =
        let snap = { Snapshot.SeqNo = mgr.LastCommitSeqNo }
        let txn = Transaction.create mgr.NextTxnId snap
        let record = { Txn = txn; Keys = Set.empty }

        let updated =
            { mgr with
                NextTxnId = mgr.NextTxnId + 1UL
                Active = Map.add mgr.NextTxnId record mgr.Active
            }

        updated, txn

    /// transaction を破棄する（abort）。バッファは捨てられるので DB には何も残らない。
    let abort (mgr: TransactionManager) (txnId: TxnId) =
        { mgr with
            Active = Map.remove txnId mgr.Active
        }

    /// commit する。
    /// 1. write-write 競合を検査する。
    ///    「transaction の snapshot より後に、衝突する key が commit 済み」であれば衝突。
    /// 2. 問題なければ SeqNo を採番して LastCommitted を更新する。
    /// 返り値の Entry 配列を StorageEngine に流し込むと DB に反映される。
    let commit (mgr: TransactionManager) (txn: Transaction) =
        let txnId = txn.TxnId

        // 渡された Transaction 自身を使う。Active に残っている「begin 時の空の
        // transaction」を使うと、バッファの書き込みが全部消えている。
        if not (Map.containsKey txnId mgr.Active) then
            Error(DbError.internalError (sprintf "Transaction %d が存在しません" txnId))
        else
            let snapSeq = txn.Snapshot.SeqNo

            // ---- write-write 競合検出 ----
            // キー集合は「beginTxn 時の空集合」ではなく commit 時のバッファ から計算する。
            // （begin 時点のままだと 1 つも書かれていないので衝突を検出できない）
            let conflicting =
                Transaction.writeKeys txn
                |> Set.toList
                |> List.filter (fun k ->
                    match mgr.LastCommitted |> Map.tryFind k with
                    | Some seq -> seq > snapSeq
                    | None -> false)

            if not (List.isEmpty conflicting) then
                Error(
                    DbError.conflict (sprintf "Transaction %d が key %d 件と write-write 競合しました" txnId conflicting.Length)
                )
            else
                // ---- 採番と バッファの確定 ----
                let batches, nextSeq = Transaction.toEntries mgr.NextSeqNo txn

                let lastCommitted =
                    batches
                    |> List.fold
                        (fun acc (ks, entries) ->
                            entries
                            |> Array.fold
                                (fun acc e ->
                                    let k = Transaction.conflictKey ks e.Key

                                    match acc |> Map.tryFind k with
                                    | Some old when old >= e.SeqNo -> acc
                                    | _ -> Map.add k e.SeqNo acc)
                                acc)
                        mgr.LastCommitted

                let commitSeq = nextSeq - 1UL

                let updated =
                    { mgr with
                        NextSeqNo = nextSeq
                        LastCommitted = lastCommitted
                        LastCommitSeqNo = commitSeq
                        Active = Map.remove txnId mgr.Active
                    }

                Ok(updated, commitSeq, batches)
