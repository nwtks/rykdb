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

/// 同じ DB を指す root 表記の揺れ（相対・絶対・末尾の /）を吸収する。
module internal RootKey =
    let normalize (root: string) =
        if System.String.IsNullOrEmpty root then
            ""
        else
            match protect (fun () -> System.IO.Path.GetFullPath root) with
            | Ok full -> full
            | Error _ -> root

/// 進行中 transaction の snapshot 登録 1件。
/// <remarks>
/// 1つの transaction の state を持つ `Database` ハンドルは複数できる
/// （`let tx = db.Begin()` のあと、`tx.Execute` のたびに新しいハンドルが返る）。
/// どれか 1本でも生きている間は snapshot を残す必要があるので、
/// ハンドルの生存トークンを数え、最後の 1本が GC されたら登録を外す。
/// これが無いと、commit / rollback / Close されずに捨てられたハンドルの登録が残り、
/// 同じ root の DDL が永久に `ConflictError` になる（architecture §7.8）。
/// トークンは `obj()` の参照等価。
/// Core は `mutable` / `ref` を 0件に保つ規約なので、
/// 連番カウンタではなく参照等価のオブジェクトを鍵にする。
/// </remarks>
type internal ActiveRegistration internal (root: string, snapshot: uint64) =
    /// 生存しているハンドルのトークン。
    let handles = System.Collections.Concurrent.ConcurrentDictionary<obj, byte>()

    /// 正規化済みの root。
    member _.Root = root

    /// ハンドルの生存を登録し、解除に使うトークンを返す。
    member _.Attach() =
        let token = obj ()
        handles.[token] <- 0uy
        token

    /// ハンドルの解除。
    /// 戻り値は「これが最後の 1本だった」かどうか。
    member _.Detach(token: obj) =
        handles.TryRemove token |> ignore
        handles.IsEmpty

/// プロセス内で開いている全ハンドルの active transaction の snapshot を集約する。
/// <remarks>
/// GC の境界は「最古の active snapshot」なので、
/// 同じ DB を別ハンドルで開いて長い transaction を走らせている場合も、
/// その snapshot を考慮しないと compaction がまだ必要な版を捨ててしまう
/// （`TransactionManager` はハンドル単位のため分からない）。
/// 複数プロセスからの同時アクセスは非対応（architecture §12.2）なので、プロセス内の表で十分。
/// 永続化もしない（再オープン後の transaction は存在しない）。
/// </remarks>
module internal ActiveSnapshotRegistry =
    /// 正規化した root -> (登録 -> snapshot)。
    let private byRoot =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            System.Collections.Concurrent.ConcurrentDictionary<ActiveRegistration, uint64>
         >
            System.StringComparer.Ordinal

    let private bucket (root: string) =
        byRoot.GetOrAdd(
            RootKey.normalize root,
            fun _ -> System.Collections.Concurrent.ConcurrentDictionary<ActiveRegistration, uint64>()
        )

    /// snapshot を登録し、解除に使う登録を返す。
    let register (root: string) (snapshot: uint64) =
        let key = RootKey.normalize root
        let registration = ActiveRegistration(key, snapshot)
        (bucket key).[registration] <- snapshot
        registration

    /// 登録を解除する（commit 成功・rollback・Close・最後のハンドルの GC）。
    /// 解除し忘れても「版を余分に残す」側にしか倒れない（安全側）。
    let unregister (root: string) (registration: ActiveRegistration) =
        let key = RootKey.normalize root

        match byRoot.TryGetValue key with
        | true, inner ->
            inner.TryRemove registration |> ignore

            if inner.IsEmpty then
                byRoot.TryRemove key |> ignore
        | _ -> ()

    /// その root で active な snapshot の最小値。
    /// registration に自分の登録を渡すと、これから commit する transaction 自身を除外できる。
    let oldestExcept (self: ActiveRegistration option) (root: string) =
        match byRoot.TryGetValue(RootKey.normalize root) with
        | true, inner ->
            inner
            |> Seq.fold
                (fun acc pair ->
                    let isSelf =
                        match self with
                        | Some registration -> System.Object.ReferenceEquals(registration, pair.Key)
                        | None -> false

                    if isSelf then
                        acc
                    else
                        match acc with
                        | Some current -> Some(min current pair.Value)
                        | None -> Some pair.Value)
                None
        | _ -> None

/// `Database` ハンドルの生存に結び付いた登録リース。
/// <remarks>
/// ハンドルが commit / rollback / Close されずに捨てられても、
/// GC された時点で（その登録のハンドルが最後の 1本なら）登録を外す。
/// finalizer が走るのは「そのハンドルがもう使えない」状態なので、
/// まだ commit できる transaction を誤って解放することはない。
/// 明示的に終わらせた場合は登録が先に外れているので何もしない（解除は冪等）。
/// </remarks>
type internal RegistrationLease(registration: ActiveRegistration) =
    /// このハンドルの生存トークン。
    let token = registration.Attach()

    /// このハンドルの登録を外す。
    /// 最後の 1本なら registry からも消える。二重呼び出しは無害。
    member _.Release() =
        if registration.Detach token then
            ActiveSnapshotRegistry.unregister registration.Root registration

    override this.Finalize() =
        // finalizer から例外を漏らさない（.NET ではプロセス終了になる）。
        protect (fun () -> this.Release()) |> ignore

/// 進行中の書き込みと DDL を同じ root 単位で直列化する。
/// <remarks>
/// `CREATE INDEX` の backfill は「テーブルを走査して索引エントリを書く」ので、
/// 途中で別の書き込みが入ると索引に載らない行ができる。
/// - **DDL**（CREATE INDEX / DROP INDEX / DROP TABLE / CREATE TABLE）は排他ロックを取る。
/// - **文の実行と commit** は共有ロックを取り、DDL の実行中は待つ。
/// 明示 transaction はロックを保持しない
/// （DDL と重なった場合は commit 時にカタログ版の不一致を検出して ConflictError にする。）。
/// 同じスレッドの再帰を許す（DDL の内部 commit は排他を持ったまま共有を取る）。
/// </remarks>
module internal RootGate =
    /// 正規化した root -> ゲート。
    let private gates =
        System.Collections.Concurrent.ConcurrentDictionary<string, System.Threading.ReaderWriterLockSlim>
            System.StringComparer.Ordinal

    let private gateOf (root: string) =
        gates.GetOrAdd(
            RootKey.normalize root,
            fun _ -> new System.Threading.ReaderWriterLockSlim(System.Threading.LockRecursionPolicy.SupportsRecursion)
        )

    /// 書き込み系（文の実行と commit）の共有ロック。
    /// 既にこのスレッドがロックを持っていれば何もしない（DDL の内部 commit・入れ子の commit）。
    let withShared (root: string) (f: unit -> 'r) =
        let gate = gateOf root

        if gate.IsReadLockHeld || gate.IsWriteLockHeld then
            f ()
        else
            gate.EnterReadLock()

            try
                f ()
            finally
                gate.ExitReadLock()

    /// DDL の排他ロック。
    /// 書き込み中の文があれば終わるまで待つ。
    let withExclusive (root: string) (f: unit -> 'r) =
        let gate = gateOf root

        if gate.IsWriteLockHeld then
            f ()
        else
            gate.EnterWriteLock()

            try
                f ()
            finally
                gate.ExitWriteLock()
