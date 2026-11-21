// ===========================================================================
// Transactions — MVCC の snapshot・採番・write-write 競合検出。
//
// 1. 採番: コミット済みの論理時計を 1つに並ばせる（「DB の時刻」）
// 2. 可視性: snapshot = SeqNo <= S の書き込みだけ見える。
//    StorageEngine.getAtMasked / scanAtWith が SeqNo で絞るので実装はフィルタ 1つだけ
// 3. 競合検出: 同じキーに書いたら後から commit した方を失敗させる
//    （first-committer-wins）
//
// 書き込みはバッファに貯め、commit 時に一度だけ engine へ流す
// （read-your-writes・abort 残留なし・commit order 一意）。
// keyspace 単位の並列性は RowStore が持つ。
// ===========================================================================

namespace RykDb.Core

/// 読み取りの可視性境界。
/// これより大きい SeqNo の書き込みは見えない。
type internal Snapshot = { SeqNo: uint64 }

module internal Snapshot =
    /// この snapshot から見える SeqNo か（Option で「制限なし」を表す）。
    let toVisible (snap: Snapshot) = Some snap.SeqNo

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
        /// バッファの概算バイト数。
        ApproxBytes: int64
        /// UNIQUE 索引値の論理予約キー。物理エントリは書かない。
        Reservations: Set<string>
        /// この transaction が保持できるバッファの上限。
        LimitBytes: int64
    }

module internal Transaction =
    /// 既定の transaction バッファ上限（256MiB）。
    [<Literal>]
    let DefaultLimitBytes = 256L * 1024L * 1024L

    [<Literal>]
    let private ReservationKeyspacePrefix = "@unique:"

    /// 書き込み可能な transaction を作る。
    let create (txnId: TxnId) (snap: Snapshot) =
        {
            TxnId = txnId
            Snapshot = snap
            Buffers = Map.empty
            ApproxBytes = 0L
            Reservations = Set.empty
            LimitBytes = DefaultLimitBytes
        }

    /// transaction のバッファ 1スロット（Map ノード・キー配列・行・値を含む）の
    /// ヒープ上の概算サイズ。
    /// `Entry.managedBytes` は Treap ノードの分も数えるので使わない（バッファは Map）。
    let private slotBytes (keyspace: string) (key: byte array) (slot: SqlValue array option) =
        let rowBytes =
            match slot with
            | Some values -> ValueCodec.managedRowBytes values
            | None -> 0

        int64 keyspace.Length + 48L + 24L + int64 key.Length + int64 rowBytes

    /// <remarks>
    /// **所有権を移す**:
    /// キーは Map の順序の根拠、行は read-your-writes が返す値なので、
    /// 呼び出し側が後から配列を書き換えるとバッファが壊れる。
    /// そこでここで複製する。
    /// </remarks>
    let private setSlot
        (keyspace: string)
        (givenKey: byte array)
        (givenSlot: SqlValue array option)
        (txn: Transaction)
        =
        let key = Array.copy givenKey
        let slot = givenSlot |> Option.map SqlValue.copyRow

        let inner =
            match Map.tryFind keyspace txn.Buffers with
            | Some m -> m
            | None -> Map.empty

        let oldBytes =
            inner
            |> Map.tryFind key
            |> Option.map (slotBytes keyspace key)
            |> Option.defaultValue 0L

        { txn with
            Buffers = Map.add keyspace (Map.add key slot inner) txn.Buffers
            ApproxBytes = txn.ApproxBytes - oldBytes + slotBytes keyspace key slot
        }

    /// 値を書き込む（バッファに積む）。
    let put (keyspace: string) (key: byte array) (value: SqlValue array) = setSlot keyspace key (Some value)

    /// 削除を書き込む（バッファに積む）。
    let remove (keyspace: string) (key: byte array) = setSlot keyspace key None

    /// バッファが上限を超えているか。
    let isOverLimit (txn: Transaction) = txn.ApproxBytes > txn.LimitBytes

    /// 上限超過時のエラー。
    let overLimitError (txn: Transaction) =
        sprintf "transaction の書き込みバッファが上限（%d バイト）を超えました" txn.LimitBytes
        |> DbError.capacity

    /// 上限内なら transaction を返し、超えていれば CapacityError。
    let checkLimit (txn: Transaction) =
        if isOverLimit txn then
            err (overLimitError txn)
        else
            ret txn

    /// txn がこのキーへ書いた内容。
    /// 外側の None は未書き込み、内側の None は削除を表す。
    let tryFind (keyspace: string) (key: byte array) (txn: Transaction) =
        txn.Buffers |> Map.tryFind keyspace |> Option.bind (Map.tryFind key)

    /// txn が keyspace に書いた全スロット。
    let slotsOf (keyspace: string) (txn: Transaction) =
        txn.Buffers
        |> Map.tryFind keyspace
        |> Option.map Map.toList
        |> Option.defaultValue []

    /// 「(keyspace, キー)」を競合検出用の文字列 1本に畳む。
    /// Map のキーは byte[] でも構造比較で並ぶので、文字列化は一意性を保てる。
    let conflictKey (keyspace: string) (key: byte array) =
        keyspace + ":" + System.Convert.ToBase64String key

    /// UNIQUE 索引値の論理予約キー。
    let private uniqueReservationKey (indexName: string) (key: byte array) =
        conflictKey (ReservationKeyspacePrefix + indexName) key

    /// UNIQUE 索引値の予約を記録する。
    let reserveUnique (indexName: string) (key: byte array) (txn: Transaction) =
        let reservation = uniqueReservationKey indexName key

        if Set.contains reservation txn.Reservations then
            txn
        else
            { txn with
                Reservations = Set.add reservation txn.Reservations
                ApproxBytes = txn.ApproxBytes + int64 reservation.Length * 2L + 48L
            }

    /// 競合検出用に「(keyspace, キー)」を文字列 1本に畳んだ集合。
    let writeKeys (txn: Transaction) =
        let physical =
            txn.Buffers
            |> Map.toList
            |> Seq.collect (fun (ks, m) -> m |> Map.keys |> Seq.map (conflictKey ks))
            |> Set.ofSeq

        Set.union physical txn.Reservations

    /// バッファを keyspace ごとに「SeqNo 昇順で採番済みの Entry 配列」に変換する。
    /// 採番は nextSeqNo から 1つずつ採番する。
    /// 決定的な順序にするため keyspace 名 → キーの昇順で採番する
    /// （同じ transaction は常に同じ SeqNo 割当になる）。
    let toEntries (nextSeqNo: uint64) (txn: Transaction) =
        let rec step seqNo acc =
            function
            | [] -> List.rev acc
            | (k, v) :: tail ->
                let e =
                    match v with
                    | Some values -> Entry.put k seqNo values
                    | None -> Entry.delete k seqNo

                step (seqNo + 1UL) (e :: acc) tail

        let rec perKeyspace seqNo acc =
            function
            | [] -> List.rev acc, seqNo
            | (ks, entries) :: rest ->
                let made = step seqNo [] entries |> Array.ofList
                perKeyspace (seqNo + uint64 entries.Length) ((ks, made) :: acc) rest

        txn.Buffers
        |> Map.toList
        |> List.map (fun (ks, m) -> ks, m |> Map.toList)
        |> perKeyspace nextSeqNo []

/// transaction を管理し、commit order の一意性と write-write 競合を保証する。
type internal TransactionManager =
    {
        /// 次に採番する SeqNo
        NextSeqNo: uint64
        /// 次に採番する TxnId
        NextTxnId: TxnId
        /// commit 済み transaction が最後に書いた (keyspace:キー) -> SeqNo。
        /// 競合判定では、active transaction の snapshot より後に書かれた版だけを参照する。
        /// そのため、最古の active snapshot より前の履歴は commit ごとに削除できる。
        /// active transaction がなければ履歴は不要なので、commit 直後に空になる。
        LastCommitted: Map<string, uint64>
        /// アクティブな transaction（TxnId -> 開始時の状態）。
        /// 書き込みは `Transaction` 側の不変更新で行い、ここには開始時の値を置く。
        Active: Map<TxnId, Transaction>
        /// 最後に commit された SeqNo（=「DB の現在時刻」）
        LastCommitSeqNo: uint64
        /// 1 transaction のバッファ上限（バイト）。
        MaxTransactionBytes: int64
    }

module internal TransactionManager =
    let create () =
        {
            NextSeqNo = 1UL
            NextTxnId = 1UL<txnId>
            LastCommitted = Map.empty
            Active = Map.empty
            LastCommitSeqNo = 0UL
            MaxTransactionBytes = Transaction.DefaultLimitBytes
        }

    /// engine の高水位（最後に採番された SeqNo）に Manager を合わせる。
    /// <remarks>
    /// 失敗した DDL の後始末のように、Manager を経由せず engine へ書かれた SeqNo があっても
    /// 次の採番がそれを巻き戻さないようにする
    /// （巻き戻すと同じキーに同じ SeqNo の版ができ、可視性と GC が曖昧になる）。
    /// </remarks>
    let synchronize (lastSeq: uint64) (manager: TransactionManager) =
        let next =
            if lastSeq = System.UInt64.MaxValue then
                lastSeq
            else
                lastSeq + 1UL

        { manager with
            NextSeqNo = max manager.NextSeqNo next
            LastCommitSeqNo = max manager.LastCommitSeqNo lastSeq
        }

    /// 指定した上限で transaction を始める。
    let beginTxnWithLimit (limit: int64 option) (manager: TransactionManager) =
        let snap =
            {
                Snapshot.SeqNo = manager.LastCommitSeqNo
            }

        let txn =
            { Transaction.create manager.NextTxnId snap with
                LimitBytes = Option.defaultValue manager.MaxTransactionBytes limit
            }

        let updated =
            { manager with
                NextTxnId = manager.NextTxnId + 1UL<txnId>
                Active = Map.add manager.NextTxnId txn manager.Active
            }

        updated, txn

    /// 現在の「DB の時刻」で transaction を始める。
    let beginTxn = beginTxnWithLimit None

    /// GC の境界になる最古の active snapshot。
    /// active な transaction が無ければ「次の commit で採番される SeqNo」を返す
    /// （現在の最後の commit までは誰も見ないので、古い版を捨てられる）。
    let activeSnapshot (manager: TransactionManager) =
        let oldest =
            manager.Active
            |> Map.fold (fun acc _ txn -> min acc txn.Snapshot.SeqNo) System.UInt64.MaxValue

        if oldest = System.UInt64.MaxValue then
            if manager.LastCommitSeqNo = System.UInt64.MaxValue then
                manager.LastCommitSeqNo
            else
                manager.LastCommitSeqNo + 1UL
        else
            oldest

    /// commit する。
    /// 1. transaction 開始時の snapshot より後に、同じキーへの commit があれば競合とする。
    /// 2. 競合がなければ SeqNo を割り当て、今回の書き込みを `LastCommitted` に記録する。
    /// 3. active transaction のうち最も古い snapshot を境界に、不要になった履歴を削除する。
    ///    各 transaction が競合を検出するには、自分の snapshot より後の履歴だけあればよい。
    ///    active transaction がなければ、今回の履歴も不要なので直ちに削除する。
    ///    履歴を残し続けると、書き込まれたすべてのキーがメモリに蓄積する。
    /// 返り値の Entry 配列を StorageEngine に渡すと、書き込みが DB に反映される。
    let commit (txn: Transaction) (manager: TransactionManager) =
        let findConflicts txn lastCommitted snapSeq =
            if Map.isEmpty lastCommitted then
                []
            else
                Transaction.writeKeys txn
                |> Set.toList
                |> List.filter (fun k ->
                    match lastCommitted |> Map.tryFind k with
                    | Some seq -> seq > snapSeq
                    | None -> false)

        let updateLastCommitted lastCommitted (batches: (string * Entry array) list) commitSeq =
            let physical =
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

            txn.Reservations
            |> Set.fold
                (fun acc k ->
                    match acc |> Map.tryFind k with
                    | Some old when old >= commitSeq -> acc
                    | _ -> Map.add k commitSeq acc)
                physical

        if not (Map.containsKey txn.TxnId manager.Active) then
            err (sprintf "Transaction %d が存在しません" txn.TxnId |> DbError.internalError)
        elif Transaction.isOverLimit txn then
            err (Transaction.overLimitError txn)
        else
            let conflicting = findConflicts txn manager.LastCommitted txn.Snapshot.SeqNo

            if not (List.isEmpty conflicting) then
                err (
                    sprintf "Transaction %d が key %d 件と write-write 競合しました" txn.TxnId conflicting.Length
                    |> DbError.conflict
                )
            else
                let batches, allocatedNext = Transaction.toEntries manager.NextSeqNo txn

                let commitSeq =
                    if Set.isEmpty txn.Reservations then
                        allocatedNext - 1UL
                    else
                        max (allocatedNext - 1UL) (manager.LastCommitSeqNo + 1UL)

                let nextSeq = commitSeq + 1UL

                let staged =
                    { manager with
                        Active = Map.remove txn.TxnId manager.Active
                    }

                let gcThreshold =
                    activeSnapshot
                        { staged with
                            LastCommitSeqNo = commitSeq
                        }

                let lastCommitted =
                    if gcThreshold > commitSeq then
                        Map.empty
                    else
                        updateLastCommitted manager.LastCommitted batches commitSeq
                        |> Map.filter (fun _ seq -> seq >= gcThreshold)

                let updated =
                    { staged with
                        NextSeqNo = nextSeq
                        LastCommitted = lastCommitted
                        LastCommitSeqNo = commitSeq
                    }

                ret (updated, commitSeq, batches)

    /// transaction を破棄する（abort）。
    /// バッファは捨てられるので DB には何も残らない。
    let abort (txn: Transaction) (manager: TransactionManager) =
        { manager with
            Active = Map.remove txn.TxnId manager.Active
        }

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
/// 1つの登録に対して、それを参照するリース（`RegistrationLease`）が複数存在しうる。
/// リースが1つでも残っていれば transaction は利用中なので、その snapshot の登録を維持する。
/// リースごとに生存トークンを付け、最後のトークンが GC で回収されたときに登録を解除する。
/// 登録を解除しないと、使われていない snapshot が残り、
/// 同じ root の DDL が `ConflictError` で拒否され続ける。
/// トークンは `obj()` の参照等価。
/// Core は `mutable` / `ref` を 0件に保つ規約なので、
/// 連番カウンタではなく参照等価のオブジェクトを鍵にする。
/// </remarks>
type internal ActiveRegistration internal (root: string, unregister: ActiveRegistration -> unit) =
    /// 生存しているリースのトークン。
    let handles = System.Collections.Concurrent.ConcurrentDictionary<obj, byte>()

    /// 正規化済みの root。
    member _.Root = root

    /// リースの生存を登録し、解除に使うトークンを返す。
    member _.Attach() =
        let token = obj ()
        handles.[token] <- 0uy
        token

    /// リースの解除。
    /// 戻り値は「これが最後の 1本だった」かどうか。
    member _.Detach(token: obj) =
        handles.TryRemove token |> ignore
        handles.IsEmpty

    /// 登録を解除する（登録先のレジストリを保持している）。
    member this.Unregister() = unregister this

/// DB ごとの active transaction の snapshot を集約する。
/// <remarks>
/// GC の境界は「最古の active snapshot」なので、
/// 同じ root に複数の store / 接続があり、その 1つで長い transaction を走らせている場合も、
/// その snapshot を考慮しないと compaction がまだ必要な版を捨ててしまう
/// （`TransactionManager` は接続側の状態だけでは分からない）。
/// 複数プロセスからの同時アクセスは非対応なので、DB ごとの表で十分。
/// 永続化もしない（再オープン後の transaction は存在しない）。
/// </remarks>
type internal SnapshotRegistry() =
    /// 正規化した root -> (登録 -> snapshot)。
    let byRoot =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            System.Collections.Concurrent.ConcurrentDictionary<ActiveRegistration, uint64>
         >
            System.StringComparer.Ordinal

    /// 全操作を直列化する。
    /// <remarks>
    /// `Register` が bucket を `GetOrAdd` してから登録を追加する前に、
    /// 別スレッドの `Unregister` が空の bucket を削除すると、
    /// 登録が削除済みの bucket に書かれて消える。
    /// 登録・解除・最古値の走査を 1本の Gate に通して競合をなくす。
    /// </remarks>
    let gate = new System.Threading.SemaphoreSlim(1, 1)

    let withGate (f: unit -> 'T) =
        gate.Wait()

        try
            f ()
        finally
            gate.Release() |> ignore

    /// 正規化済み key の bucket を取る（呼び出しは Gate の内側）。
    let bucket (key: string) =
        byRoot.GetOrAdd(key, fun _ -> System.Collections.Concurrent.ConcurrentDictionary<ActiveRegistration, uint64>())

    /// snapshot を登録し、解除に使う登録を返す。
    member this.Register(root: string, snapshot: uint64) =
        let key = RootKey.normalize root

        withGate (fun () ->
            let registration = ActiveRegistration(key, fun reg -> this.Unregister reg)
            (bucket key).[registration] <- snapshot
            registration)

    /// 登録を解除する（commit 成功・rollback・Close・最後の接続の GC）。
    /// 解除し忘れても「版を余分に残す」側にしか倒れない（安全側）。
    member _.Unregister(registration: ActiveRegistration) =
        withGate (fun () ->
            match byRoot.TryGetValue registration.Root with
            | true, inner ->
                inner.TryRemove registration |> ignore

                if inner.IsEmpty then
                    byRoot.TryRemove registration.Root |> ignore
            | _ -> ())

    /// その root で active な snapshot の最小値。
    /// registration に自分の登録を渡すと、これから commit する transaction 自身を除外できる。
    member _.OldestExcept(root: string, registration: ActiveRegistration option) =
        let key = RootKey.normalize root

        withGate (fun () ->
            match byRoot.TryGetValue key with
            | true, inner ->
                inner
                |> Seq.fold
                    (fun acc pair ->
                        let isSelf =
                            match registration with
                            | Some reg -> System.Object.ReferenceEquals(reg, pair.Key)
                            | None -> false

                        if isSelf then
                            acc
                        else
                            match acc with
                            | Some current -> Some(min current pair.Value)
                            | None -> Some pair.Value)
                    None
            | _ -> None)

/// DB ごとの RootRuntime と active snapshot 登録。
/// `RowStore` が所有し、Session 間で共有する。
type internal RootRuntime() =
    let gate =
        new System.Threading.ReaderWriterLockSlim(System.Threading.LockRecursionPolicy.SupportsRecursion)

    let snapshots = SnapshotRegistry()

    /// 書き込み系（文の実行と commit）の共有ロック。
    /// 既にこのスレッドがロックを持っていれば何もしない（DDL の内部 commit・入れ子の commit）。
    member _.WithShared(f: unit -> 'r) =
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
    member _.WithExclusive(f: unit -> 'r) =
        if gate.IsWriteLockHeld then
            f ()
        else
            gate.EnterWriteLock()

            try
                f ()
            finally
                gate.ExitWriteLock()

    member _.Register(root: string, snapshot: uint64) = snapshots.Register(root, snapshot)

    member _.Unregister(registration: ActiveRegistration) = snapshots.Unregister registration

    member _.OldestExcept(root: string, registration: ActiveRegistration option) =
        snapshots.OldestExcept(root, registration)

/// 進行中 transaction の登録の生存に結び付いたリース。
/// <remarks>
/// `TxnState.Lease` が持ち、接続（`Session`）が Close されずに捨てられても、
/// GC された時点で（その登録のリースが最後の 1本なら）登録を外す。
/// finalizer が走るのは「その接続がもう使えない」状態なので、
/// まだ commit できる transaction を誤って解放することはない。
/// 明示的に終わらせた場合は登録が先に外れているので何もしない（解除は冪等）。
/// </remarks>
type internal RegistrationLease(registration: ActiveRegistration) =
    /// このリースの生存トークン。
    let token = registration.Attach()

    /// 登録そのもの（`ExecState.ActiveRegistration` へ渡す）。
    member _.Registration = registration

    /// このリースの登録を外す。
    /// 最後の 1本なら registry からも消える。二重呼び出しは無害。
    member _.Release() =
        if registration.Detach token then
            registration.Unregister()

    override this.Finalize() =
        // finalizer から例外を漏らさない（.NET ではプロセス終了になる）。
        protect (fun () -> this.Release()) |> ignore
