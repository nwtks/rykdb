// ===========================================================================
// StorageEngine — LSM ツリーを 1本公開する層（1 DB = ここが 1本）。
//
// 探索は「memtable -> L0（新しい順）-> L1 以降（レベル順）」の順。
// 各ステップで Bloom filter と VersionSet の key range で枝刈りする。
// 可視性（SeqNo <= snapshot）は getAtMasked / scanAtWith の visible フィルタで判定する。
// ===========================================================================

namespace RykDb.Core

/// 1つの DB が共有する実行時状態。
/// `StorageEngine` の不変コピー間で参照を共有し、DB の寿命と一緒に回収する。
type internal StorageRuntime() =
    let gate = new System.Threading.SemaphoreSlim(1, 1)

    let lastFlushError =
        System.Collections.Concurrent.ConcurrentDictionary<string, DbError option>()

    do lastFlushError.["current"] <- None

    member _.Gate = gate

    /// 直近の flush の失敗（成功したら None）。
    member _.LastFlushError
        with get () = lastFlushError.["current"]
        and set (value: DbError option) = lastFlushError.["current"] <- value

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
        /// GC は「これより新しい版」と「これ以下の最新 1版」を残す（Compaction.gcGroup）。
        /// 0 は「全版を残す」。
        ActiveSnapshot: uint64
        /// DB ごとの実行時状態（書き込み Gate）。
        Runtime: StorageRuntime
    }

module internal StorageEngine =
    // writeBatch / maybeFlush / close は、同じ DB の WAL / memtable 競合を防ぐため直列化する。
    // Gate は DB の Runtime が所有する。
    let private withRuntimeGate (engine: StorageEngine) (f: unit -> 'T) =
        engine.Runtime.Gate.Wait()

        try
            f ()
        finally
            engine.Runtime.Gate.Release() |> ignore

    /// SSTable に書く Bloom filter の誤検出率。
    [<Literal>]
    let private BloomFalsePositiveRate = 0.01

    /// 既定の memtable 上限（64 MiB）。
    [<Literal>]
    let private DefaultMemLimitBytes = 64 * 1024 * 1024

    /// 読み取り用メタデータを返す。
    /// flush / compaction が作ったファイルは `ReadInfo` を持っている。
    /// MANIFEST から復元したファイル（テストの手組みを含む）は、ここで 1回だけ解析する。
    let private readInfoOf (dir: string) (f: FileMetaData) =
        match f.ReadInfo with
        | Some info -> ret info
        | None -> Sstable.openPartial (FileMetaData.path dir f.FileNumber)

    let private readInfoIn (f: FileMetaData) (engine: StorageEngine) = readInfoOf engine.Dir f

    /// バックグラウンド compaction 用の走査。
    /// ファイルの先頭から全件を遅延して読む。
    /// compaction はキー・SeqNo・削除タグしか見ないので、値は復号せずスライスのまま渡す。
    let openScan (dir: string) (f: FileMetaData) =
        readInfoOf dir f >>= fun info -> ret (Sstable.rangeScanPartialRaw [||] 0 info)

    /// 列マスク付きのポイントルックアップ。
    let getAtMasked (visible: uint64 option) (key: byte array) (mask: bool array) (engine: StorageEngine) =
        let rec probe files =
            match files with
            | [] -> ret None
            | f :: rest ->
                match readInfoIn f engine with
                | Ok info ->
                    match Sstable.lookupMasked mask key visible info with
                    | Ok(Some e) -> ret (Some e)
                    | Ok None -> probe rest
                    | Error e -> err e
                | Error e -> err e

        match Memtable.getVisible key visible engine.Mem with
        | Some e ->
            let value = e.Value |> Option.map (SqlValue.maskRow mask)
            ret (Some(SstEntry.make e.Key e.SeqNo value))
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
    /// マージとヒープは `SstMerge`（SstScan.fs）に集約する。
    let private scanAtWith
        (visible: uint64 option)
        (limit: int)
        (startKey: byte array)
        (maskOpt: bool array option)
        (engine: StorageEngine)
        =
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
            readInfoIn f engine
            >>= fun info ->
                match maskOpt with
                | None -> Sstable.rangeScanPartial startKey 0 info
                | Some mask when mask.Length = 0 -> Sstable.rangeScanPartialSkipped startKey 0 info
                | Some mask -> Sstable.rangeScanPartialMasked mask startKey 0 info
                |> ret
                >>= fun scan ->
                    match SstScan.next scan with
                    | Ok None -> ret merge
                    | Ok(Some(e, nxt)) -> ret (SstMerge.insert index e nxt merge)
                    | Error e -> err e

        let rec prepare pending merge =
            match pending with
            | [] -> ret (merge, [])
            | (index, bound, f) :: rest ->
                match SstMerge.top merge with
                | Some top when Bytes.compare bound top.Key > 0 -> ret (merge, pending)
                | _ ->
                    match openPending index f merge with
                    | Ok merge' -> prepare rest merge'
                    | Error e -> err e

        let isVisible e =
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true

        let closeState state = SstMerge.dispose state.Merge

        let rec stepObj state =
            if limit > 0 && state.Count >= limit then
                ret None
            else
                match prepare state.Pending state.Merge with
                | Ok(ready, remaining) ->
                    match SstMerge.next ready with
                    | Ok None -> ret None
                    | Ok(Some(e, merge')) ->
                        let sameKey =
                            match state.EmittedKey with
                            | Some k -> Bytes.compare k e.Key = 0
                            | None -> false

                        if sameKey || not (isVisible e) then
                            { state with
                                Pending = remaining
                                Merge = merge'
                            }
                            |> stepObj
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
                                Some(e, SstScan.ofState advanced stepObj closeState) |> ret
                            else
                                stepObj advanced
                    | Error e -> err e
                | Error e -> err e

        let pendingFiles =
            Version.allFiles engine.Version
            |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
            |> List.map (fun f -> sourceBound startKey f, f)
            |> List.sortWith (fun (a, _) (b, _) -> Bytes.compare a b)
            |> List.mapi (fun i (bound, f) -> i + 1, bound, f)

        let memSeq =
            Memtable.rangeScan startKey 0 engine.Mem
            |> Seq.map (fun e ->
                let value =
                    match maskOpt with
                    | None -> e.Value
                    | Some mask -> e.Value |> Option.map (SqlValue.maskRow mask)

                SstEntry.make e.Key e.SeqNo value)

        match SstScan.next (seqToScan memSeq) with
        | Ok None ->
            SstScan.ofState
                {
                    Pending = pendingFiles
                    Merge = SstMerge.Empty
                    EmittedKey = None
                    Count = 0
                }
                stepObj
                closeState
            |> ret
        | Ok(Some(e, nxt)) ->
            SstScan.ofState
                {
                    Pending = pendingFiles
                    Merge = SstMerge.insert 0 e nxt SstMerge.Empty
                    EmittedKey = None
                    Count = 0
                }
                stepObj
                closeState
            |> ret
        | Error e -> err e

    /// 範囲ルックアップ（`needsValues = true` なら値を復号し、false なら空の行を返す）。
    let scanAtValues visible limit startKey needsValues engine =
        scanAtWith visible limit startKey (if needsValues then None else Some [||]) engine

    /// 範囲ルックアップ（列マスクに従って必要な列だけ復号する）。
    let scanAtMasked visible limit startKey (mask: bool array) engine =
        scanAtWith visible limit startKey (Some mask) engine

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

        withRuntimeGate engine (fun () ->
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
    /// <remarks>
    /// 失敗時は「次に採番するファイル番号を進めた engine」とエラーを返す。
    /// MANIFEST の反映結果が不明な場合でも、同じファイル番号を再利用して
    /// MANIFEST が参照する SSTable を上書きしないため。
    /// </remarks>
    let private flushCore (engine: StorageEngine) =
        let rotateWal engine (next: WalState) =
            result {
                do! Wal.sync engine.Dir engine.Wal
                // 先に新しい WAL を作る。作成に失敗した場合に旧 WAL を残し、
                // engine が指す位置のログを失わないようにする。
                let! _ = Wal.create engine.Dir next.FileNumber

                protect (fun () ->
                    Wal.walFiles engine.Dir
                    |> List.filter (fun path -> path <> Wal.path engine.Dir next.FileNumber)
                    |> List.iter System.IO.File.Delete)
                |> ignore

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

        let bumpNextFileNumber (engine: StorageEngine) fileNumber =
            { engine with
                Version =
                    { engine.Version with
                        NextFileNumber = max engine.Version.NextFileNumber (fileNumber + 1L)
                    }
            }

        if Memtable.isEmpty engine.Mem then
            Ok engine
        else
            let fileNumber = engine.Version.NextFileNumber
            let bumped = bumpNextFileNumber engine fileNumber

            match
                Memtable.rangeScan [||] 0 engine.Mem
                |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
                |> FileMetaData.writeSstable engine.Dir fileNumber 0 BloomFalsePositiveRate
            with
            | Error e -> Error(bumped, e)
            | Ok meta ->
                match
                    VersionEdit.addFile meta
                    |> Manifest.logAndApplyWithOutcome engine.Dir engine.ManifestNumber engine.Version
                with
                | ManifestApplyResult.Applied(version, manifestNumber) ->
                    match finishFlush engine version manifestNumber with
                    | Ok flushed -> Ok flushed
                    | Error e -> Error(bumped, e)
                | ManifestApplyResult.NotApplied e
                | ManifestApplyResult.Unknown e -> Error(bumped, e)

    /// flush を実行し、失敗を DB の診断欄へ残す。
    let private flush (engine: StorageEngine) =
        let outcome = flushCore engine

        engine.Runtime.LastFlushError <-
            match outcome with
            | Ok _ -> None
            | Error(_, e) -> Some e

        outcome

    /// memtable が上限に達していれば flush する
    /// （compaction はしない。background compaction は worker が行う）。
    /// 上限に達していなければ何もしない。
    /// flush / WAL rotate は同じ keyspace の writeBatch / close と競合させないため、
    /// ディレクトリ単位の Gate の内側で行う。
    /// flush 自体の失敗は Result で返す。
    /// WAL への書き込みが成功した時点で commit は成立しているため、呼び出し側の `RowStore` はそれを握り潰し、
    /// 失敗前の memtable を保持して次の write で再試行する。
    /// background compaction の有効時は RowStore がこちらを呼び、MANIFEST 反映後に worker を起こす。
    /// 戻り値の bool は「実際に flush したか」。
    let flushIfNeededWith (allocateFileNumber: unit -> int64) (engine: StorageEngine) =
        withRuntimeGate engine (fun () ->
            if
                not (Memtable.isEmpty engine.Mem)
                && Memtable.approxBytes engine.Mem >= engine.MemLimitBytes
            then
                let fileNumber = allocateFileNumber ()

                let engine =
                    { engine with
                        Version =
                            { engine.Version with
                                NextFileNumber = fileNumber
                            }
                    }

                flush engine
                |> function
                    | Ok flushed -> ret (flushed, true)
                    | Error(bumped, _) -> ret (bumped, false)
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
            readInfoIn f engine
            >>= fun info -> ret (Sstable.rangeScanPartialRaw [||] 0 info)

        let nextFileNumberAfter version result =
            match result.Added with
            | [] -> version.NextFileNumber
            | files -> max version.NextFileNumber ((files |> List.map _.FileNumber |> List.max) + 1L)

        let buildCompacted version manifestNumber result =
            { engine with
                Version =
                    { version with
                        NextFileNumber = nextFileNumberAfter version result
                    }
                ManifestNumber = manifestNumber
            }

        match
            Compaction.compact
                engine.Dir
                engine.Version
                engine.Compaction
                engine.ActiveSnapshot
                engine.Version.NextFileNumber
                scanOf
        with
        | Error e -> Error(engine, e)
        | Ok(compactionResult, _) ->
            if List.isEmpty compactionResult.Added && List.isEmpty compactionResult.Removed then
                Ok engine
            else
                let fallbackEngine =
                    { engine with
                        Version =
                            { engine.Version with
                                NextFileNumber = nextFileNumberAfter engine.Version compactionResult
                            }
                    }

                match
                    Manifest.logAndApplyWithOutcome
                        engine.Dir
                        engine.ManifestNumber
                        engine.Version
                        {
                            VersionEdit.AddedFiles = compactionResult.Added
                            DeletedFiles = compactionResult.Removed
                        }
                with
                | ManifestApplyResult.Applied(version, manifestNumber) ->
                    let compacted = buildCompacted version manifestNumber compactionResult

                    match FileMetaData.deleteObsolete engine.Dir compactionResult.Removed with
                    | Ok() -> Ok compacted
                    | Error e -> Error(compacted, e)
                | ManifestApplyResult.NotApplied e ->
                    FileMetaData.deleteObsolete engine.Dir compactionResult.Added |> ignore
                    Error(fallbackEngine, e)
                | ManifestApplyResult.Unknown e -> Error(fallbackEngine, e)

    /// memtable が上限に達していれば flush し、続けて compaction する（同期版）。
    /// compaction の失敗は flush 後の engine と一緒に返す。
    let maybeFlush (engine: StorageEngine) =
        withRuntimeGate engine (fun () ->
            if
                not (Memtable.isEmpty engine.Mem)
                && Memtable.approxBytes engine.Mem >= engine.MemLimitBytes
            then
                match flush engine with
                | Error(bumped, e) -> ret (bumped, Some e)
                | Ok flushed ->
                    match compactOnce flushed with
                    | Ok compacted -> ret (compacted, None)
                    | Error(bumped, e) -> ret (bumped, Some e)
            else
                ret (engine, None))

    /// データベースを開く（新規なら作る）。
    /// [memLimitBytes] を指定すると memtable の上限を差し替える。
    let openDatabaseWith (memLimitBytes: int option) (dir: string) =
        let ensureFilesPresent dir version =
            let missing =
                Version.allFiles version
                |> List.filter (fun f -> not (System.IO.File.Exists(FileMetaData.path dir f.FileNumber)))

            if List.isEmpty missing then
                ret ()
            else
                let names =
                    missing
                    |> List.map (fun f -> FileMetaData.path dir f.FileNumber)
                    |> String.concat ", "

                DbError.corruption (sprintf "MANIFEST が参照する SSTable が見つかりません: %s" names) |> err

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
                    protect (fun () -> System.IO.FileInfo(Wal.path dir walNumber).Length)
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
                Wal = { FileNumber = 1L; Size = 0L }
                NextSeqNo = 1UL
                NextTxnId = 1UL<txnId>
                MemLimitBytes = DefaultMemLimitBytes
                Compaction = CompactionOptions.Defaults
                ActiveSnapshot = 0UL
                Runtime = StorageRuntime()
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

        /// MANIFEST から復元したファイルの読み取りメタデータを 1回だけ構築する。
        /// ここで解析した結果は不変の Version に載り、読み取りのたびに開き直さない。
        let hydrateVersion (version: Version) =
            let hydrate (f: FileMetaData) =
                match f.ReadInfo with
                | Some _ -> ret f
                | None ->
                    Sstable.openPartial (FileMetaData.path dir f.FileNumber)
                    |> Result.map (fun info -> { f with ReadInfo = Some info })

            let hydrateLevel (files: FileMetaData array) =
                files |> Array.map hydrate |> all |> Result.map List.toArray

            version.Files
            |> Array.map hydrateLevel
            |> all
            |> Result.map (fun levels ->
                { version with
                    Files = List.toArray levels
                })

        result {
            let! version, manifestNumber = Manifest.openOrCreate dir
            do! ensureFilesPresent dir version
            do! quarantineOrphans dir version
            let! version' = hydrateVersion version
            let! walState = openOrCreateWal dir version'

            let! replayed =
                Wal.foldCommitted
                    dir
                    walState.FileNumber
                    (fun (mem, maxSeq) entry -> Memtable.insert entry mem, max maxSeq entry.SeqNo)
                    (Memtable.Empty, 0UL)

            let (mem, walMax), walMaxTxn, goodWalSize = replayed
            let! alignedWal = Wal.truncateTo dir walState goodWalSize
            let diskMax = maxSeqNoOnDisk version'

            let engine =
                buildRecovered version' manifestNumber alignedWal mem walMax diskMax walMaxTxn

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
        withRuntimeGate engine (fun () ->
            let isLatestWriter =
                match protect (fun () -> System.IO.FileInfo(Wal.path engine.Dir engine.Wal.FileNumber).Length) with
                | Ok size -> size = engine.Wal.Size
                | Error _ -> false

            if not isLatestWriter then
                ret ()
            else
                result {
                    match flush engine with
                    | Error(_, e) -> return! err e
                    | Ok flushed ->
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
    abstract PublishedState: PublishedState
    /// 現在の WAL 位置。メモリエンジンは None。
    abstract WalState: WalState option
    /// 直近の flush 失敗（成功したら None）。
    abstract LastFlushError: DbError option

    /// 列マスクに従って必要な列だけ復号して 1行を引く。
    abstract GetAtMasked:
        visible: uint64 option -> key: byte array -> mask: bool array -> Result<SstEntry option, DbError>

    /// 行の値が必要ないときは `needsValues = false`（値を作らず空の行を返す）。
    abstract ScanAtValues:
        visible: uint64 option -> startKey: byte array -> limit: int -> needsValues: bool -> Result<SstScan, DbError>

    /// 列マスクに従って必要な列だけ復号する。
    abstract ScanAtMasked:
        visible: uint64 option -> startKey: byte array -> limit: int -> mask: bool array -> Result<SstScan, DbError>

    abstract WriteBatch: entries: Entry array -> Result<IStorageEngine, DbError>
    /// WAL 追記のみ行い、fsync は呼び出し側に任せる。
    abstract WriteBatchDeferred: entries: Entry array -> Result<IStorageEngine, DbError>
    abstract WithNextSeqNo: nextSeqNo: uint64 -> IStorageEngine
    abstract WithActiveSnapshot: snapshot: uint64 -> IStorageEngine
    /// coordinator が公開した Version / MANIFEST 番号 / GC 境界で差し替える。
    abstract WithHostState: state: PublishedState -> IStorageEngine
    /// 上限超過時は flush だけを行う（compaction はしない）。
    /// 戻り値の bool は「実際に flush したか」。
    abstract FlushIfNeeded: allocateFileNumber: (unit -> int64) -> Result<IStorageEngine * bool, DbError>
    abstract MaybeFlush: unit -> Result<IStorageEngine * DbError option, DbError>
    abstract Close: unit -> Result<unit, DbError>

/// 実ディスクの StorageEngine を IStorageEngine として公開する。
type internal DiskStorageEngine(engine: StorageEngine) =
    interface IStorageEngine with
        member _.NextSeqNo = engine.NextSeqNo
        member _.ActiveSnapshot = engine.ActiveSnapshot

        member _.PublishedState =
            {
                Version = engine.Version
                ManifestNumber = engine.ManifestNumber
                ActiveSnapshot = engine.ActiveSnapshot
                LastCompactionError = None
                Generation = 0L
            }

        member _.WalState = Some engine.Wal
        member _.LastFlushError = engine.Runtime.LastFlushError

        member _.GetAtMasked visible key mask =
            StorageEngine.getAtMasked visible key mask engine

        member _.ScanAtValues visible startKey limit needsValues =
            StorageEngine.scanAtValues visible limit startKey needsValues engine

        member _.ScanAtMasked visible startKey limit mask =
            StorageEngine.scanAtMasked visible limit startKey mask engine

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
                }
            :> IStorageEngine

        member _.FlushIfNeeded allocateFileNumber =
            StorageEngine.flushIfNeededWith allocateFileNumber engine
            |> Result.map (fun (next, flushed) -> DiskStorageEngine next :> IStorageEngine, flushed)

        member _.MaybeFlush() =
            StorageEngine.maybeFlush engine
            |> Result.map (fun (next, error) -> DiskStorageEngine next :> IStorageEngine, error)

        member _.Close() = StorageEngine.close engine
