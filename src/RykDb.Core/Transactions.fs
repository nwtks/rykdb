// ===========================================================================
// Transactions — MVCC の snapshot・採番・write-write 競合検出。
//
// 1. 採番: コミット済みの論理時計を 1つに並ばせる（「DB の時刻」）
// 2. 可視性: snapshot = SeqNo <= S の書き込みだけ見える。
//    StorageEngine.getAt / scanAt が SeqNo で絞るので実装はフィルタ 1つだけ
// 3. 競合検出: 同じキーに書いたら後から commit した方を失敗させる
//    （first-committer-wins）
//
// 書き込みはバッファに貯め、commit 時に一度だけ engine へ流す
// （read-your-writes・abort 残留なし・commit order 一意）。
// keyspace 単位の並列性は RowStore が持つ。
// ===========================================================================

namespace RykDb.Core

/// transaction の識別子。
type internal TxnId = uint64

/// 読み取りの可視性境界。
/// これより大きい SeqNo の書き込みは見えない。
type internal Snapshot = { SeqNo: uint64 }

module internal Snapshot =
    /// この snapshot から見える SeqNo か（Option で「制限なし」を表す）。
    let toVisible (snap: Snapshot) = Some snap.SeqNo

    /// 制限なし。
    let noLimit: uint64 option = None

/// 書き込み中の transaction。
/// commit まで DB から見えない。
type internal Transaction =
    {
        TxnId: TxnId
        /// transaction 開始時点の可視性境界
        Snapshot: Snapshot
        /// keyspace -> (キー -> 値)。
        /// 同一キーは最後の書き込みが勝つ。
        Buffers: Map<string, Map<byte array, SqlValue array option>>
    }

module internal Transaction =
    /// 書き込み可能な transaction を作る。
    let create (txnId: TxnId) (snap: Snapshot) =
        {
            TxnId = txnId
            Snapshot = snap
            Buffers = Map.empty
        }

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

    /// txn がこのキーへ書いた内容。
    /// 外側の None は未書き込み、内側の None は削除を表す。
    let tryFind (txn: Transaction) (keyspace: string) (key: byte array) =
        txn.Buffers |> Map.tryFind keyspace |> Option.bind (Map.tryFind key)

    /// txn が keyspace に書いた全スロット。
    let slotsOf (txn: Transaction) (keyspace: string) =
        txn.Buffers
        |> Map.tryFind keyspace
        |> Option.map Map.toList
        |> Option.defaultValue []

    /// 「(keyspace, キー)」を競合検出用の文字列 1本に畳む。
    /// Map のキーは byte[] でも構造比較で並ぶので、文字列化は一意性を保てる。
    let conflictKey (keyspace: string) (key: byte array) =
        keyspace + ":" + System.Convert.ToBase64String key

    /// 競合検出用に「(keyspace, キー)」を文字列 1本に畳んだ集合。
    let writeKeys (txn: Transaction) =
        txn.Buffers
        |> Map.toList
        |> List.collect (fun (ks, m) -> m |> Map.toList |> List.map (fun (k, _) -> conflictKey ks k))
        |> Set.ofList

    /// バッファを keyspace ごとに「SeqNo 降順で採番済みの Entry 配列」に変換する。
    /// 採番は nextSeqNo から 1つずつ採番する。
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

        let rec perKeyspace
            (spaces: (string * (byte array * SqlValue array option) list) list)
            (seqNo: uint64)
            (acc: (string * Entry array) list)
            =
            match spaces with
            | [] -> List.rev acc, seqNo
            | (ks, entries) :: rest ->
                let made = step entries seqNo [] |> Array.ofList
                perKeyspace rest (seqNo + uint64 entries.Length) ((ks, made) :: acc)

        perKeyspace (txn.Buffers |> Map.toList |> List.map (fun (ks, m) -> ks, m |> Map.toList)) nextSeqNo []

/// active な transaction 1件分。
type internal TxnRecord =
    {
        Txn: Transaction
        /// 書き込み済みの key 集合（keyspace:キー の文字列）
        Keys: Set<string>
    }

/// transaction を管理し、commit order の一意性と write-write 競合を保証する。
type internal TransactionManager =
    {
        /// 次に採番する SeqNo
        NextSeqNo: uint64
        /// 次に採番する TxnId
        NextTxnId: TxnId
        /// commit 済みの transaction が最後に書いた (keyspace:キー) -> SeqNo。
        /// commit のたびに `activeSnapshot` より古い版を捨てるので、
        /// サイズは「最古の active snapshot 以降に書かれたキー」に収まって無限成長しない。
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

    /// transaction を破棄する（abort）。
    /// バッファは捨てられるので DB には何も残らない。
    let abort (mgr: TransactionManager) (txnId: TxnId) =
        { mgr with
            Active = Map.remove txnId mgr.Active
        }

    /// GC の境界になる最古の active snapshot。
    /// active な transaction が無ければ「次の commit で採番される SeqNo」を返す
    /// （現在の最後の commit までは誰も見ないので、古い版を捨てられる）。
    let activeSnapshot (mgr: TransactionManager) =
        let oldest =
            mgr.Active
            |> Map.fold (fun acc _ record -> min acc record.Txn.Snapshot.SeqNo) System.UInt64.MaxValue

        if oldest = System.UInt64.MaxValue then
            if mgr.LastCommitSeqNo = System.UInt64.MaxValue then
                mgr.LastCommitSeqNo
            else
                mgr.LastCommitSeqNo + 1UL
        else
            oldest

    /// commit する。
    /// 1. write-write 競合を検査する。
    ///    「transaction の snapshot より後に、衝突する key が commit 済み」であれば衝突。
    /// 2. 問題なければ SeqNo を採番して LastCommitted を更新する。
    /// 3. LastCommitted のうち SeqNo < activeSnapshot の版を掃除する。
    ///    衝突判定は「commit が transaction の snapshot より後」でしか起こらない
    ///    （未来の snapshot は現在の LastCommitSeqNo 以上）なので、
    ///    「全部の active snapshot 以降に commit された版」以外は捨てられる。
    ///    捨てないと書き込んだ全キーがプロセス寿命の間メモリに残り続ける。
    /// 返り値の Entry 配列を StorageEngine に流し込むと DB に反映される。
    let commit (mgr: TransactionManager) (txn: Transaction) =
        let findConflicts (txn: Transaction) (lastCommitted: Map<string, uint64>) (snapSeq: uint64) =
            Transaction.writeKeys txn
            |> Set.toList
            |> List.filter (fun k ->
                match lastCommitted |> Map.tryFind k with
                | Some seq -> seq > snapSeq
                | None -> false)

        let updateLastCommitted (batches: (string * Entry array) list) (lastCommitted: Map<string, uint64>) =
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
                lastCommitted

        if not (Map.containsKey txn.TxnId mgr.Active) then
            err (sprintf "Transaction %d が存在しません" txn.TxnId |> DbError.internalError)
        else
            let conflicting = findConflicts txn mgr.LastCommitted txn.Snapshot.SeqNo

            if not (List.isEmpty conflicting) then
                err (
                    sprintf "Transaction %d が key %d 件と write-write 競合しました" txn.TxnId conflicting.Length
                    |> DbError.conflict
                )
            else
                let batches, nextSeq = Transaction.toEntries mgr.NextSeqNo txn
                let committed = updateLastCommitted batches mgr.LastCommitted
                let commitSeq = nextSeq - 1UL

                let staged =
                    { mgr with
                        Active = Map.remove txn.TxnId mgr.Active
                    }

                let gcThreshold = activeSnapshot staged
                let lastCommitted = committed |> Map.filter (fun _ seq -> seq >= gcThreshold)

                let updated =
                    { staged with
                        NextSeqNo = nextSeq
                        LastCommitted = lastCommitted
                        LastCommitSeqNo = commitSeq
                    }

                Ok(updated, commitSeq, batches)
