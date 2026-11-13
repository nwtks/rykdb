// ===========================================================================
// StorageEngine — LSM ツリーを 1本公開する層（1 DB = ここが 1本）。
//
// 探索は「memtable -> L0（新しい順）-> 各レベル（新しい順）」の順。
// 各ステップで Bloom filter と VersionSet の key range で枝刈りする。
// 可視性（SeqNo <= snapshot）は getAt / scanAt の visible フィルタで判定する。
// ===========================================================================

namespace RykDb.Core

/// 部分読み用の SSTable リーダー（index / Bloom / meta のみ）のキャッシュ。
module private SstPartialCache =
    let private Cache =
        System.Collections.Concurrent.ConcurrentDictionary<string, System.WeakReference<SstPartial>>
            System.StringComparer.Ordinal

    /// キー数がこの倍数になるたびに、死んだ弱参照を掃除する。
    /// キーは「ファイルごと」に増える（flush / compaction でファイルが作り直される）ため、
    /// 掃除しないと辞書のキー文字列だけがプロセス寿命で溜まり続ける。
    [<Literal>]
    let private SweepEvery = 512

    /// ファイルの同一性を表すキー。
    /// 同じ番号のファイルが差し替わった場合はサイズ/mtime が変わるので別キーになる。
    let keyOf (path: string) (info: System.IO.FileInfo) =
        sprintf "%s|%d|%d" path info.Length info.LastWriteTimeUtc.Ticks

    let tryGet (key: string) =
        match Cache.TryGetValue key with
        | true, weak ->
            match weak.TryGetTarget() with
            | true, partial -> Some partial
            | _ ->
                Cache.TryRemove key |> ignore
                None
        | _ -> None

    /// 死んだ弱参照（対象の `SstPartial` が GC されたエントリ）を辞書から外す。
    /// 生きた参照は、実行中の走査が使っている可能性があるので残す。
    let private sweep () =
        for kv in Cache do
            match kv.Value.TryGetTarget() with
            | true, _ -> ()
            | _ -> Cache.TryRemove kv.Key |> ignore

    let put (key: string) (partial: SstPartial) =
        Cache.[key] <- System.WeakReference<SstPartial> partial

        if Cache.Count % SweepEvery = 0 then
            sweep ()

/// StorageEngine の不変状態。
/// すべての操作は新しい状態を返す。
type internal StorageEngine =
    {
        /// データベースのディレクトリ
        Dir: string
        /// 有効な manifest 番号
        /// （単位 `manifestNo`。SSTable のファイル番号とは取り違えない）。
        ManifestNumber: int64<manifestNo>
        /// 現在の LSM 形状
        Version: Version
        /// 書き込み先 memtable
        Mem: Memtable
        /// 現在の WAL
        Wal: WalState
        /// 次に採番する seqNo
        NextSeqNo: uint64
        /// 次に採番する txnId
        NextTxnId: TxnId
        /// memtable のメモリ上限（超えたら flush する）
        MemLimitBytes: int
        /// コンパクションの設定
        Compaction: CompactionOptions
        /// 最古の active snapshot の SeqNo。
        /// GC は「これより新しい版」と「これ以下の最新 1版」を残す（Compaction.gc）。
        /// 0 は「全版を残す」。
        ActiveSnapshot: uint64
        /// 直近の compaction の失敗（成功したら None）。
        /// flush は成功しているため、失敗しても engine は flush 後の状態を保つ。
        LastCompactionError: DbError option
    }

module internal StorageEngine =
    // writeBatch / maybeFlush / close は、同じ DB の WAL / memtable 競合を
    // 防ぐため直列化する。
    // 異なる DB は並行に進められるよう、ディレクトリ単位の Gate にする
    // （プロセス全体の 1本にしない）。
    let private DirGates =
        System.Collections.Concurrent.ConcurrentDictionary<string, System.Threading.SemaphoreSlim>
            System.StringComparer.Ordinal

    let private normalizeDir (dir: string) =
        match protect (fun () -> System.IO.Path.GetFullPath dir) with
        | Ok full -> full
        | Error _ -> dir

    let private dirGate (dir: string) =
        DirGates.GetOrAdd(normalizeDir dir, fun _ -> new System.Threading.SemaphoreSlim(1, 1))

    let private withDirGate (dir: string) (f: unit -> 'T) =
        let gate = dirGate dir
        gate.Wait()

        try
            f ()
        finally
            gate.Release() |> ignore

    /// SSTable に書く Bloom filter の誤検出率。
    [<Literal>]
    let private BloomFalsePositiveRate = 0.01

    /// 既定の memtable 上限（64 MiB）。
    [<Literal>]
    let private DefaultMemLimitBytes = 64 * 1024 * 1024

    /// 部分読み用の reader を開く（index / Bloom / meta のみ）。
    let private openPartialSstIn (dir: string) (f: FileMetaData) =
        let path = FileMetaData.path dir f.FileNumber

        protect (fun () -> System.IO.FileInfo path |> SstPartialCache.keyOf path)
        >>= fun key ->
            match SstPartialCache.tryGet key with
            | Some partial -> ret partial
            | None ->
                Sstable.openPartial path
                >>= fun partial ->
                    SstPartialCache.put key partial
                    ret partial

    let private openPartialSst (f: FileMetaData) (engine: StorageEngine) = openPartialSstIn engine.Dir f

    /// バックグラウンド compaction 用の走査。
    /// ファイルの先頭から全件を遅延して読む。
    let openScan (dir: string) (f: FileMetaData) =
        openPartialSstIn dir f
        >>= fun partial -> ret (Sstable.rangeScanPartial [||] 0 partial)

    /// ポイントルックアップ。
    /// memtable -> L0 -> 各レベルの順に見る。
    /// visible が Some s のときは SeqNo が s 以下の最新版を返す。
    /// これが MVCC の可視性。
    /// None なら制限なし。
    let getAt (visible: uint64 option) (key: byte array) (engine: StorageEngine) =
        let rec probe files =
            match files with
            | [] -> ret None
            | f :: rest ->
                match openPartialSst f engine with
                | Error e -> err e
                | Ok partial ->
                    match Sstable.lookup key visible partial with
                    | Error e -> err e
                    | Ok(Some e) -> ret (Some e)
                    | Ok None -> probe rest

        match Memtable.getVisible key visible engine.Mem with
        | Some e -> ret (Some(SstEntry.make e.Key e.SeqNo e.Value))
        | None -> Version.filesForPointLookup key engine.Version |> probe

    /// 遅延走査の状態。
    /// `next` を呼ぶたびに、未オープンのファイルを必要な分だけ開き、
    /// マージから可視・最新・非 tombstone の 1件を取り出す。
    type private ScanState =
        {
            /// まだ開いていないファイル（最小キー昇順）
            Pending: (int * byte array * FileMetaData) list
            /// 現在の k-way merge
            Merge: SstMerge
            /// 直前に出力したキー（同じキーの古い版を読み飛ばす）
            EmittedKey: byte array option
            /// 出力済みの件数（limit 用。tombstone は数えない）
            Count: int
        }

    /// 範囲ルックアップ。
    /// キー昇順の pull（`SstScan`）を返す。
    /// `SstScan.next` を呼ぶまで SSTable のブロックを読まない。
    /// visible が Some s のときは SeqNo が s 以下の版を返す。
    /// limit > 0 のときは limit 件を出力した時点で終端になる。
    /// 破損・欠落は、そのファイル・ブロックが必要になった `next` が `Error` を返す
    /// （呼び出し時点では検出しない。全件を読む演算子は最終結果を返す前に必ず Error になる）。
    /// ヒープは 1回だけ組み、以降は「取り出したソースの続きを戻す」だけにする
    /// （行ごとにソース列から作り直すと O(行数 × ファイル数 log ファイル数) になる）。
    /// マージとヒープは `SstMerge`（Sstable.fs）に集約する。
    let scanAt (visible: uint64 option) (limit: int) (startKey: byte array) (engine: StorageEngine) =
        let seqToScan (rows: SstEntry seq) =
            let e = rows.GetEnumerator()

            let rec pull () =
                if e.MoveNext() then
                    SstScan.emit pull e.Current
                else
                    (e :> System.IDisposable).Dispose() |> ignore
                    SstScan.Empty

            pull ()

        let sourceBound startKey f =
            if Bytes.compare f.Smallest startKey > 0 then
                f.Smallest
            else
                startKey

        let openPending index f merge =
            openPartialSst f engine
            >>= fun partial -> ret (Sstable.rangeScanPartial startKey 0 partial)
            >>= fun scan ->
                match SstScan.next scan with
                | Error e -> err e
                | Ok None -> ret merge
                | Ok(Some(e, nxt)) -> ret (SstMerge.insert index e nxt merge)

        let rec prepare pending merge =
            match pending with
            | [] -> ret (merge, [])
            | (index, bound, f) :: rest ->
                match SstMerge.top merge with
                | Some top when Bytes.compare bound top.Key > 0 -> ret (merge, pending)
                | _ ->
                    match openPending index f merge with
                    | Error e -> err e
                    | Ok merge' -> prepare rest merge'

        let isVisible e =
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true

        let closeState (stateObj: obj) =
            SstMerge.dispose (stateObj :?> ScanState).Merge

        let rec stepObj (stateObj: obj) =
            let state = stateObj :?> ScanState

            if limit > 0 && state.Count >= limit then
                ret None
            else
                match prepare state.Pending state.Merge with
                | Error e -> err e
                | Ok(ready, remaining) ->
                    match SstMerge.next ready with
                    | Error e -> err e
                    | Ok None -> ret None
                    | Ok(Some(e, merge')) ->
                        let sameKey =
                            match state.EmittedKey with
                            | Some k -> Bytes.compare k e.Key = 0
                            | None -> false

                        if sameKey || not (isVisible e) then
                            let nextState =
                                { state with
                                    Pending = remaining
                                    Merge = merge'
                                }

                            stepObj (box nextState)
                        else
                            let appended = e.Value.IsSome

                            let advanced =
                                { state with
                                    Pending = remaining
                                    Merge = merge'
                                    EmittedKey = Some e.Key
                                    Count = if appended then state.Count + 1 else state.Count
                                }

                            if appended then
                                Some(e, SstScan.ofState (box advanced) stepObj closeState) |> ret
                            else
                                stepObj (box advanced)

        let pendingFiles =
            Version.allFiles engine.Version
            |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
            |> List.map (fun f -> sourceBound startKey f, f)
            |> List.sortWith (fun (a, _) (b, _) -> Bytes.compare a b)
            |> List.mapi (fun i (bound, f) -> i + 1, bound, f)

        let memSeq =
            Memtable.rangeScan startKey 0 engine.Mem
            |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)

        match SstScan.next (seqToScan memSeq) with
        | Error e -> err e
        | Ok None ->
            SstScan.ofState
                (box
                    {
                        Pending = pendingFiles
                        Merge = SstMerge.Empty
                        EmittedKey = None
                        Count = 0
                    })
                stepObj
                closeState
            |> ret
        | Ok(Some(e, nxt)) ->
            SstScan.ofState
                (box
                    {
                        Pending = pendingFiles
                        Merge = SstMerge.insert 0 e nxt SstMerge.Empty
                        EmittedKey = None
                        Count = 0
                    })
                stepObj
                closeState
            |> ret

    /// 書き込みバッチ。
    /// まず WAL（Begin/Batch/Commit を 1回の書き込みで）へ、
    /// それから memtable へ反映する。
    let private writeBatchCore (sync: bool) (entries: Entry array) (engine: StorageEngine) =
        let apply wal txnId =
            let mem = Memtable.apply entries engine.Mem
            let nextTxnId = txnId + 1UL<txnId>

            { engine with
                Wal = wal
                Mem = mem
                NextTxnId = nextTxnId
            }

        withDirGate engine.Dir (fun () ->
            result {
                let txnId = engine.NextTxnId

                let records =
                    [
                        WalRecord.Begin txnId
                        WalRecord.Batch(txnId, entries)
                        WalRecord.Commit txnId
                    ]

                let! wal =
                    if sync then
                        Wal.append engine.Dir records engine.Wal
                    else
                        Wal.appendDeferred engine.Dir records engine.Wal

                return apply wal txnId
            })

    let writeBatch = writeBatchCore true

    /// WAL への追記だけ行い、fsync は Session のグループコミットに任せる。
    let writeBatchDeferred = writeBatchCore false

    /// memtable を L0 の SSTable にして書き出す。
    /// memtable が空なら何もしない（WAL も切り替えない）。
    let private flush (engine: StorageEngine) =
        let rotateWal engine (next: WalState) =
            result {
                do! Wal.sync engine.Dir engine.Wal
                do! protect (fun () -> Wal.walFiles engine.Dir |> List.iter System.IO.File.Delete |> ignore)
                let! _ = Wal.create engine.Dir next.FileNumber
                return { engine with Wal = next }
            }

        let finishFlush engine version manifestNumber =
            result {
                let! rotated = Wal.rotate engine.Wal |> rotateWal engine

                return
                    { rotated with
                        Version =
                            { version with
                                NextFileNumber = version.NextFileNumber + 1L
                            }
                        ManifestNumber = manifestNumber
                        Mem = Memtable.Empty
                    }
            }

        result {
            if engine.Mem.Count = 0 then
                return engine
            else
                let fileNumber = engine.Version.NextFileNumber

                let! meta =
                    Memtable.rangeScan [||] 0 engine.Mem
                    |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
                    |> FileMetaData.writeSstable engine.Dir fileNumber 0 BloomFalsePositiveRate

                let! version, manifestNumber =
                    VersionEdit.addFile meta
                    |> Manifest.logAndApply engine.Dir engine.ManifestNumber engine.Version

                return! finishFlush engine version manifestNumber
        }

    /// memtable が上限に達していれば flush し、続けて compaction する。
    /// 上限に達していなければ何もしない。
    /// flush / compaction / WAL rotate は同じ keyspace の writeBatch / close と競合させないため、
    /// ディレクトリ単位の Gate の内側で行う。
    /// WAL への書き込みが成功した時点で commit は成立しているため、
    /// flush / compaction の失敗を Error にしない（呼び出し側が SeqNo を再利用し得る）。
    /// flush 失敗時は memtable を残して次の write で再試行し、
    /// compaction 失敗時は flush 後の engine に LastCompactionError を記録して返す。
    /// 上限に達していれば flush だけを行う（compaction はしない）。
    /// background compaction の有効時は RowStore がこちらを呼び、MANIFEST 反映後に worker を起こす。
    /// 戻り値の bool は「実際に flush したか」。
    let flushIfNeededWith (allocateFileNumber: unit -> int64) (engine: StorageEngine) =
        withDirGate engine.Dir (fun () ->
            if engine.Mem.Count > 0 && engine.Mem.ApproxBytes >= engine.MemLimitBytes then
                let fileNumber = allocateFileNumber ()

                let engine =
                    { engine with
                        Version =
                            { engine.Version with
                                NextFileNumber = fileNumber
                            }
                    }

                flush engine |> Result.map (fun flushed -> flushed, true)
            else
                ret (engine, false))

    /// 1 回の Leveled compaction を行う。
    /// 手順:
    ///   1. 対象レベルExceeded しているファイルを選ぶ
    ///   2. 入力をストリーミングでマージし、GC で不要な版を落とす
    ///   3. 出力をファイルに書き、MANIFEST に記録する（先に MANIFEST）
    ///   4. 入力ファイルを削除する（MANIFEST の後に）
    /// 3 と 4 の順序が重要: MANIFEST を先に書かないと、
    /// クラッシュ時に「MANIFEST は新しいがファイルは古い」= データ損失になる。
    /// 入力はファイル全体を読まず、ブロック単位で読む（部分読みリーダー）。
    let private compactOnce (engine: StorageEngine) =
        let scanOf f =
            openPartialSst f engine
            >>= fun partial -> ret (Sstable.rangeScanPartial [||] 0 partial)

        let buildCompacted version manifestNumber result =
            let nextNum =
                match result.Added with
                | [] -> version.NextFileNumber
                | files -> files |> List.map _.FileNumber |> List.max |> (+) 1L

            { engine with
                Version =
                    { version with
                        NextFileNumber = nextNum
                    }
                ManifestNumber = manifestNumber
            }

        result {
            let! compactionResult, _ =
                Compaction.compact
                    engine.Dir
                    engine.Version
                    engine.Compaction
                    engine.ActiveSnapshot
                    engine.Version.NextFileNumber
                    scanOf

            if List.isEmpty compactionResult.Added && List.isEmpty compactionResult.Removed then
                return engine
            else
                let! version, manifestNumber =
                    Manifest.logAndApply
                        engine.Dir
                        engine.ManifestNumber
                        engine.Version
                        {
                            VersionEdit.AddedFiles = compactionResult.Added
                            DeletedFiles = compactionResult.Removed
                        }

                do! FileMetaData.deleteObsolete engine.Dir compactionResult.Removed
                return buildCompacted version manifestNumber compactionResult
        }

    /// memtable が上限に達していれば flush し、続けて compaction する（同期版）。
    let maybeFlush (engine: StorageEngine) =
        withDirGate engine.Dir (fun () ->
            if engine.Mem.Count > 0 && engine.Mem.ApproxBytes >= engine.MemLimitBytes then
                match flush engine with
                | Error _ -> ret engine
                | Ok flushed ->
                    match compactOnce flushed with
                    | Ok compacted ->
                        ret
                            { compacted with
                                LastCompactionError = None
                            }
                    | Error e ->
                        ret
                            { flushed with
                                LastCompactionError = Some e
                            }
            else
                ret engine)

    /// データベースを開く（新規なら作る）。
    /// [memLimitBytes] を指定すると memtable の上限を差し替える。
    let openDatabaseWith (memLimitBytes: int option) (dir: string) =
        let dropMissingFiles dir manifestNumber version =
            let missing =
                Version.allFiles version
                |> List.filter (fun f -> not (System.IO.File.Exists(FileMetaData.path dir f.FileNumber)))

            if List.isEmpty missing then
                ret (version, manifestNumber)
            else
                {
                    VersionEdit.AddedFiles = []
                    DeletedFiles = missing
                }
                |> Manifest.logAndApply dir manifestNumber version

        let parseNumber (path: string) =
            match System.Int64.TryParse(System.IO.Path.GetFileNameWithoutExtension path) with
            | true, v -> Some v
            | _ -> None

        let sstNumbersOnDisk dir =
            System.IO.Directory.GetFiles(dir, "*" + FileMetaData.SstExtension)
            |> Array.choose parseNumber

        let move quarantine n =
            let source = FileMetaData.path dir n
            let name = System.IO.Path.GetFileName source
            let target = System.IO.Path.Combine(quarantine, name)

            let target =
                if System.IO.File.Exists target then
                    target + "." + string System.DateTime.UtcNow.Ticks
                else
                    target

            System.IO.File.Move(source, target, true)

        let quarantineOrphans dir version =
            let referenced = Version.allFiles version |> List.map _.FileNumber |> Set.ofList

            let orphans =
                sstNumbersOnDisk dir |> Array.filter (fun n -> not (Set.contains n referenced))

            protect (fun () ->
                if not (Array.isEmpty orphans) then
                    let quarantine = System.IO.Path.Combine(dir, ".orphans")
                    System.IO.Directory.CreateDirectory quarantine |> ignore
                    Array.iter (move quarantine) orphans)

        let latestWalNumber dir =
            Wal.walFiles dir |> List.choose parseNumber |> List.sort |> List.tryLast

        let openOrCreateWal dir version =
            let walNumber =
                match latestWalNumber dir with
                | Some n -> n
                | None -> version.NextFileNumber

            protect (fun () -> System.IO.File.Exists(Wal.path dir walNumber))
            >>= fun exists ->
                if exists then
                    protect (fun () -> int (System.IO.FileInfo(Wal.path dir walNumber).Length))
                    |> Result.map (fun size -> { FileNumber = walNumber; Size = size })
                else
                    Wal.create dir walNumber

        let maxSeqNoOnDisk version =
            Version.allFiles version |> List.fold (fun acc f -> max acc f.MaxSeqNo) 0UL

        let maxFileNumberOnDisk dir =
            let ssts = sstNumbersOnDisk dir
            let wals = Wal.walFiles dir |> List.choose parseNumber |> List.toArray
            Array.append ssts wals |> Array.fold (fun acc n -> max acc n) 0L

        let emptyEngine dir =
            {
                Dir = dir
                ManifestNumber = 1L<manifestNo>
                Version = Version.Empty
                Mem = Memtable.Empty
                Wal = { FileNumber = 1L; Size = 0 }
                NextSeqNo = 1UL
                NextTxnId = 1UL<txnId>
                MemLimitBytes = DefaultMemLimitBytes
                Compaction = CompactionOptions.Defaults
                ActiveSnapshot = 0UL
                LastCompactionError = None
            }

        let buildRecovered version manifestNumber walState mem walMax diskMax walMaxTxn =
            let nextSeqNo = max walMax diskMax + 1UL
            let nextTxnId = walMaxTxn + 1UL<txnId>

            let bumped =
                { version with
                    NextFileNumber = max version.NextFileNumber (maxFileNumberOnDisk dir + 1L)
                }

            { emptyEngine dir with
                ManifestNumber = manifestNumber
                Version = bumped
                Wal = walState
                NextSeqNo = nextSeqNo
                NextTxnId = nextTxnId
                Mem = mem
            }

        result {
            let! version, manifestNumber = Manifest.openOrCreate dir
            let! version', manifestNumber' = dropMissingFiles dir manifestNumber version
            do! quarantineOrphans dir version'
            let! walState = openOrCreateWal dir version'

            let! replayed =
                Wal.foldCommitted
                    dir
                    walState.FileNumber
                    (fun (mem, maxSeq) entry -> Memtable.insert entry mem, max maxSeq entry.SeqNo)
                    (Memtable.Empty, 0UL)

            let (mem, walMax), walMaxTxn = replayed
            let diskMax = maxSeqNoOnDisk version'

            let engine =
                buildRecovered version' manifestNumber' walState mem walMax diskMax walMaxTxn

            return
                match memLimitBytes with
                | Some limit -> { engine with MemLimitBytes = limit }
                | None -> engine
        }

    /// データベースを閉じる（memtable を flush してから WAL を消す）。
    /// <remarks>
    /// ハンドルは不変なので、古いハンドルでも `Close` を呼べる。
    /// 古いハンドルの WAL は新しいハンドルが既に書き進めているため、
    /// WAL の実サイズがハンドルの記録と一致しないときは flush も WAL 削除もしない
    /// （古い memtable を flush すると新しいハンドルの書き込みを消してしまう）。
    /// </remarks>
    let close (engine: StorageEngine) =
        withDirGate engine.Dir (fun () ->
            let isLatestWriter =
                match protect (fun () -> System.IO.FileInfo(Wal.path engine.Dir engine.Wal.FileNumber).Length) with
                | Ok size -> size = int64 engine.Wal.Size
                | Error _ -> false

            if not isLatestWriter then
                ret ()
            else
                result {
                    let! flushed = flush engine
                    do! Wal.sync flushed.Dir flushed.Wal

                    Wal.walFiles flushed.Dir
                    |> List.iter (fun p -> protect (fun () -> System.IO.File.Delete p) |> ignore)

                    return ()
                })

/// RowStore が使う StorageEngine の操作面。
/// テストではメモリ実装に差し替えられる。
type internal IStorageEngine =
    abstract NextSeqNo: uint64
    abstract ActiveSnapshot: uint64
    /// MANIFEST に反映済みの状態（background compaction が共有する）。
    abstract HostState: HostState
    /// 現在の WAL 位置。メモリエンジンは None。
    abstract WalState: WalState option
    abstract GetAt: visible: uint64 option -> key: byte array -> Result<SstEntry option, DbError>
    abstract ScanAt: visible: uint64 option -> startKey: byte array -> limit: int -> Result<SstScan, DbError>
    abstract WriteBatch: entries: Entry array -> Result<IStorageEngine, DbError>
    /// WAL 追記のみ行い、fsync は呼び出し側に任せる。
    abstract WriteBatchDeferred: entries: Entry array -> Result<IStorageEngine, DbError>
    abstract WithNextSeqNo: nextSeqNo: uint64 -> IStorageEngine
    abstract WithActiveSnapshot: snapshot: uint64 -> IStorageEngine
    abstract WithHostState: state: HostState -> IStorageEngine
    /// 上限超過時は flush だけを行う（compaction はしない）。
    /// 戻り値の bool は「実際に flush したか」。
    abstract FlushIfNeeded: allocateFileNumber: (unit -> int64) -> Result<IStorageEngine * bool, DbError>
    abstract MaybeFlush: unit -> Result<IStorageEngine, DbError>
    abstract Close: unit -> Result<unit, DbError>

/// 実ディスクの StorageEngine を IStorageEngine として公開する。
type internal DiskStorageEngine(engine: StorageEngine) =
    interface IStorageEngine with
        member _.NextSeqNo = engine.NextSeqNo
        member _.ActiveSnapshot = engine.ActiveSnapshot

        member _.HostState =
            {
                Version = engine.Version
                ManifestNumber = engine.ManifestNumber
                ActiveSnapshot = engine.ActiveSnapshot
                LastCompactionError = engine.LastCompactionError
                Generation = 0L
            }

        member _.WalState = Some engine.Wal

        member _.GetAt visible key = StorageEngine.getAt visible key engine

        member _.ScanAt visible startKey limit =
            StorageEngine.scanAt visible limit startKey engine

        member _.WriteBatch entries =
            StorageEngine.writeBatch entries engine
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.WriteBatchDeferred entries =
            StorageEngine.writeBatchDeferred entries engine
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.WithNextSeqNo nextSeqNo =
            DiskStorageEngine { engine with NextSeqNo = nextSeqNo } :> IStorageEngine

        member _.WithActiveSnapshot snapshot =
            DiskStorageEngine
                { engine with
                    ActiveSnapshot = snapshot
                }
            :> IStorageEngine

        member _.WithHostState state =
            DiskStorageEngine
                { engine with
                    Version = state.Version
                    ManifestNumber = state.ManifestNumber
                    ActiveSnapshot = state.ActiveSnapshot
                    LastCompactionError = state.LastCompactionError
                }
            :> IStorageEngine

        member _.FlushIfNeeded allocateFileNumber =
            StorageEngine.flushIfNeededWith allocateFileNumber engine
            |> Result.map (fun (next, flushed) -> DiskStorageEngine next :> IStorageEngine, flushed)

        member _.MaybeFlush() =
            StorageEngine.maybeFlush engine
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.Close() = StorageEngine.close engine
