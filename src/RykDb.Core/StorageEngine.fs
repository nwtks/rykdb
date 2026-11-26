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
    /// flush / compaction / close を直列化する gate。
    let gate = new System.Threading.SemaphoreSlim(1, 1)

    /// 直近の flush 失敗（成功したら None）。
    let lastFlushError =
        System.Collections.Concurrent.ConcurrentDictionary<string, DbError option>()

    do lastFlushError.["current"] <- None

    member _.Gate = gate

    /// 直近の flush の失敗（成功したら None）。
    member _.LastFlushError
        with get () = lastFlushError.["current"]
        and set (value: DbError option) = lastFlushError.["current"] <- value

/// RowStore が使う StorageEngine の操作面。
/// テストではメモリ実装に差し替えられる。
type internal IStorageEngine =
    abstract NextSeqNo: SeqNo
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
    abstract WithNextSeqNo: nextSeqNo: SeqNo -> IStorageEngine
    abstract WithActiveSnapshot: snapshot: uint64 -> IStorageEngine
    /// coordinator が公開した Version / MANIFEST 番号 / GC 境界で差し替える。
    abstract WithHostState: state: PublishedState -> IStorageEngine
    /// 上限超過時は flush だけを行う（compaction はしない）。
    /// 戻り値の bool は「実際に flush したか」。
    abstract FlushIfNeeded: allocateFileNumber: (unit -> FileNumber) -> Result<IStorageEngine * bool, DbError>
    abstract MaybeFlush: unit -> Result<IStorageEngine * DbError option, DbError>
    abstract Close: unit -> Result<unit, DbError>

/// StorageEngine の不変状態。
/// すべての操作は新しい状態を返す。
type internal StorageEngine =
    {
        /// データベースのディレクトリ
        Dir: string
        /// 有効な manifest 番号
        /// （単位 `manifestNo`。SSTable のファイル番号とは取り違えない）。
        ManifestNumber: ManifestNumber
        /// 現在の LSM 形状
        Version: Version
        /// 書き込み先 memtable
        Mem: Memtable
        /// 現在の WAL
        Wal: WalState
        /// 次に採番する seqNo
        NextSeqNo: SeqNo
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
    /// writeBatch / maybeFlush / close を DB 単位で直列化する（同じ WAL / memtable の競合防止）。
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
        | None -> SstableReader.openPartial (FileMetaData.path dir f.FileNumber)

    /// ファイルの読み取り用メタデータを引く。
    let private readInfoIn (f: FileMetaData) (engine: StorageEngine) = readInfoOf engine.Dir f

    /// 行の列挙子から遅延走査を作る（終端で列挙子を閉じる）。
    [<TailCall>]
    let rec private pullFromEnumerator (e: System.Collections.Generic.IEnumerator<SstEntry>) () =
        if e.MoveNext() then
            SstScan.emit (pullFromEnumerator e) e.Current
        else
            (e :> System.IDisposable).Dispose() |> ignore
            SstScan.Empty

    /// 行列を SstScan に変換する。
    let private seqToScan (rows: SstEntry seq) =
        let e = rows.GetEnumerator()
        pullFromEnumerator e ()

    /// ファイルの走査開始キーを、要求キーとの大きい方に取る。
    let private sourceBound (startKey: byte array) (f: FileMetaData) =
        if Bytes.compare f.Smallest startKey > 0 then
            f.Smallest
        else
            startKey

    /// 未オープンのファイルを開き、先頭 1件をマージへ登録する。
    let private openPending
        (engine: StorageEngine)
        (maskOpt: bool array option)
        (startKey: byte array)
        (index: int)
        (f: FileMetaData)
        (merge: SstMerge)
        =
        readInfoIn f engine
        >>= fun info ->
            match maskOpt with
            | None -> SstableReader.rangeScanPartial startKey 0 info
            | Some mask when mask.Length = 0 -> SstableReader.rangeScanPartialSkipped startKey 0 info
            | Some mask -> SstableReader.rangeScanPartialMasked mask startKey 0 info
            |> ret
            >>= fun scan ->
                match SstScan.next scan with
                | Ok None -> ret merge
                | Ok(Some(e, nxt)) -> ret (SstMerge.insert index e nxt merge)
                | Error e -> err e

    /// 現在の最上位より小さいキー範囲のファイルを先に開く。
    [<TailCall>]
    let rec private prepareScan
        (engine: StorageEngine)
        (maskOpt: bool array option)
        (startKey: byte array)
        (pending: (int * byte array * FileMetaData) list)
        (merge: SstMerge)
        =
        match pending with
        | [] -> ret (merge, [])
        | (index, bound, f) :: rest ->
            match SstMerge.top merge with
            | Some top when Bytes.compare bound top.Key > 0 -> ret (merge, pending)
            | _ ->
                match openPending engine maskOpt startKey index f merge with
                | Ok merge' -> prepareScan engine maskOpt startKey rest merge'
                | Error e -> err e

    /// snapshot に対して可視かどうかを判定する。
    let private isVisibleEntry (visible: uint64 option) (e: SstEntry) =
        match visible with
        | Some s -> e.SeqNo <= s
        | None -> true

    /// バックグラウンド compaction 用の走査。
    /// ファイルの先頭から全件を遅延して読む。
    /// compaction はキー・SeqNo・削除タグしか見ないので、値は復号せずスライスのまま渡す。
    let openScan (dir: string) (f: FileMetaData) =
        readInfoOf dir f
        >>= fun info -> ret (SstableReader.rangeScanPartialRaw [||] 0 info)

    /// 列マスク付きポイントルックアップのファイル走査本体。
    [<TailCall>]
    let rec private probeAtMasked
        (key: byte array)
        (mask: bool array)
        (visible: uint64 option)
        (engine: StorageEngine)
        =
        function
        | [] -> ret None
        | f :: rest ->
            match readInfoIn f engine with
            | Ok info ->
                match SstableReader.lookupMasked key mask visible info with
                | Ok(Some e) -> ret (Some e)
                | Ok None -> probeAtMasked key mask visible engine rest
                | Error e -> err e
            | Error e -> err e

    /// 列マスク付きのポイントルックアップ。
    let getAtMasked (key: byte array) (mask: bool array) (visible: uint64 option) (engine: StorageEngine) =
        match Memtable.getVisible key visible engine.Mem with
        | Some e ->
            let value = e.Value |> Option.map (SqlValue.maskRow mask)
            ret (Some(SstEntry.make e.Key e.SeqNo value))
        | None ->
            Version.filesForPointLookup key engine.Version
            |> probeAtMasked key mask visible engine

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

    /// 走査が持つマージのソースを閉じる。
    let private closeScanState (state: ScanState) = SstMerge.dispose state.Merge

    /// マージから可視・最新・非 tombstone の 1件を取り出す。
    let rec private stepScanState
        (limit: int)
        (visible: uint64 option)
        (engine: StorageEngine)
        (maskOpt: bool array option)
        (startKey: byte array)
        (state: ScanState)
        =
        if limit > 0 && state.Count >= limit then
            ret None
        else
            match prepareScan engine maskOpt startKey state.Pending state.Merge with
            | Ok(ready, remaining) ->
                match SstMerge.next ready with
                | Ok None -> ret None
                | Ok(Some(e, merge')) ->
                    let sameKey =
                        match state.EmittedKey with
                        | Some k -> Bytes.compare k e.Key = 0
                        | None -> false

                    if sameKey || not (isVisibleEntry visible e) then
                        { state with
                            Pending = remaining
                            Merge = merge'
                        }
                        |> stepScanState limit visible engine maskOpt startKey
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
                            Some(
                                e,
                                SstScan.ofState
                                    advanced
                                    (stepScanState limit visible engine maskOpt startKey)
                                    closeScanState
                            )
                            |> ret
                        else
                            stepScanState limit visible engine maskOpt startKey advanced
                | Error e -> err e
            | Error e -> err e

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
                (stepScanState limit visible engine maskOpt startKey)
                closeScanState
            |> ret
        | Ok(Some(e, nxt)) ->
            SstScan.ofState
                {
                    Pending = pendingFiles
                    Merge = SstMerge.insert 0 e nxt SstMerge.Empty
                    EmittedKey = None
                    Count = 0
                }
                (stepScanState limit visible engine maskOpt startKey)
                closeScanState
            |> ret
        | Error e -> err e

    /// 範囲ルックアップ（`needsValues = true` なら値を復号し、false なら空の行を返す）。
    let scanAtValues
        (visible: uint64 option)
        (limit: int)
        (startKey: byte array)
        (needsValues: bool)
        (engine: StorageEngine)
        =
        scanAtWith visible limit startKey (if needsValues then None else Some [||]) engine

    /// 範囲ルックアップ（列マスクに従って必要な列だけ復号する）。
    let scanAtMasked
        (visible: uint64 option)
        (limit: int)
        (startKey: byte array)
        (mask: bool array)
        (engine: StorageEngine)
        =
        scanAtWith visible limit startKey (Some mask) engine

    /// WAL と memtable の反映結果を新しい engine にまとめる。
    let private applyBatch (entries: Entry array) (engine: StorageEngine) (wal: WalState) (txnId: TxnId) =
        let mem = Memtable.apply entries engine.Mem
        let nextTxnId = txnId + 1UL<txnId>

        { engine with
            Wal = wal
            Mem = mem
            NextTxnId = nextTxnId
        }

    /// 書き込みバッチ。
    /// まず WAL（Begin/Batch/Commit を 1回の書き込みで）へ、
    /// それから memtable へ反映する。
    let private writeBatchCore (sync: bool) (entries: Entry array) (engine: StorageEngine) =
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

                return applyBatch entries engine wal txnId
            })

    /// バッチを WAL へ追記して fsync まで行う。
    let writeBatch = writeBatchCore true

    /// WAL への追記だけ行い、fsync は Session のグループコミットに任せる。
    let writeBatchDeferred = writeBatchCore false

    /// WAL を同期して古い WAL を削除し、次の WAL へ切り替える。
    let private rotateWal (engine: StorageEngine) (next: WalState) =
        result {
            do! Wal.sync engine.Dir engine.Wal
            let! stale = Wal.walFiles engine.Dir
            let! _ = Wal.create engine.Dir next.FileNumber

            protect (fun () ->
                stale
                |> List.filter (fun path -> path <> Wal.path engine.Dir next.FileNumber)
                |> List.iter System.IO.File.Delete)
            |> ignore

            return { engine with Wal = next }
        }

    /// MANIFEST 反映後の Version を受け取り、WAL を切り替えて flush 後の engine を作る。
    let private finishFlush (engine: StorageEngine) (version: Version) (manifestNumber: ManifestNumber) =
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

    /// ファイル番号の下限を進めた engine を返す。
    let private bumpNextFileNumber (engine: StorageEngine) (fileNumber: FileNumber) =
        { engine with
            Version =
                { engine.Version with
                    NextFileNumber = max engine.Version.NextFileNumber (fileNumber + 1L)
                }
        }

    /// memtable を L0 の SSTable にして書き出す。
    /// memtable が空なら何もしない（WAL も切り替えない）。
    /// <remarks>
    /// 失敗時は「次に採番するファイル番号を進めた engine」とエラーを返す。
    /// MANIFEST の反映結果が不明な場合でも、同じファイル番号を再利用して
    /// MANIFEST が参照する SSTable を上書きしないため。
    /// </remarks>
    let private flushCore (engine: StorageEngine) =
        if Memtable.isEmpty engine.Mem then
            Ok engine
        else
            let fileNumber = engine.Version.NextFileNumber
            let bumped = bumpNextFileNumber engine fileNumber

            match
                Memtable.rangeScan [||] 0 engine.Mem
                |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
                |> FileMetaData.writeSstable engine.Dir fileNumber 0 BloomFalsePositiveRate (Memtable.count engine.Mem)
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
                    | Error e ->
                        let applied =
                            { engine with
                                Version =
                                    { version with
                                        NextFileNumber = max version.NextFileNumber (engine.Wal.FileNumber + 1L)
                                    }
                                ManifestNumber = manifestNumber
                            }

                        Error(applied, e)
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
    /// flush 自体の失敗は `LastFlushError` に記録し、Result は Ok（未 flush）として返す。
    /// WAL への書き込みが成功した時点で commit は成立しているため、
    /// 呼び出し側の `RowStore` は失敗前の memtable を保持して次の write で再試行する。
    /// background compaction の有効時は RowStore がこちらを呼び、MANIFEST 反映後に worker を起こす。
    /// 戻り値の bool は「実際に flush したか」。
    let flushIfNeededWith (allocateFileNumber: unit -> FileNumber) (engine: StorageEngine) =
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

    /// compaction 用にファイルの部分読みスキャンを開く。
    let private scanOfFile (engine: StorageEngine) (f: FileMetaData) =
        readInfoIn f engine
        >>= fun info -> ret (SstableReader.rangeScanPartialRaw [||] 0 info)

    /// 追加されたファイルの最大番号の次を返す。
    let private nextFileNumberAfter (version: Version) (result: CompactionResult) =
        match result.Added with
        | [] -> version.NextFileNumber
        | files -> max version.NextFileNumber ((files |> List.map _.FileNumber |> List.max) + 1L)

    /// compaction 結果を反映した engine を組み立てる。
    let private buildCompacted
        (engine: StorageEngine)
        (version: Version)
        (manifestNumber: ManifestNumber)
        (result: CompactionResult)
        =
        { engine with
            Version =
                { version with
                    NextFileNumber = nextFileNumberAfter version result
                }
            ManifestNumber = manifestNumber
        }

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
        match
            Compaction.compact
                engine.Dir
                engine.Version
                engine.Compaction
                engine.ActiveSnapshot
                engine.Version.NextFileNumber
                (scanOfFile engine)
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
                    let compacted = buildCompacted engine version manifestNumber compactionResult

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

    /// MANIFEST が参照する SSTable がすべて存在するか確認する。
    let private ensureFilesPresent (dir: string) (version: Version) =
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

    /// パスから数値のファイル番号を取り出す。
    let private parseNumber (path: string) : FileNumber option =
        match System.Int64.TryParse(System.IO.Path.GetFileNameWithoutExtension path) with
        | true, v -> Some v
        | _ -> None

    /// ディレクトリ内の SSTable 番号を列挙する。
    let private sstNumbersOnDisk (dir: string) =
        protect (fun () ->
            System.IO.Directory.GetFiles(dir, "*" + FileMetaData.SstExtension)
            |> Array.choose parseNumber)

    /// 孤立ファイルを検疫ディレクトリへ移す。
    let private moveToQuarantine (dir: string) (quarantine: string) (n: FileNumber) =
        let source = FileMetaData.path dir n
        let name = System.IO.Path.GetFileName source
        let target = System.IO.Path.Combine(quarantine, name)

        let target =
            if System.IO.File.Exists target then
                target + "." + string System.DateTime.UtcNow.Ticks
            else
                target

        System.IO.File.Move(source, target, true)

    /// 参照されていない SSTable を検疫へ移す。
    let private quarantineOrphans (dir: string) (version: Version) =
        result {
            let referenced = Version.allFiles version |> List.map _.FileNumber |> Set.ofList
            let! numbers = sstNumbersOnDisk dir
            let orphans = numbers |> Array.filter (fun n -> not (Set.contains n referenced))

            do!
                protect (fun () ->
                    if not (Array.isEmpty orphans) then
                        let quarantine = System.IO.Path.Combine(dir, ".orphans")
                        System.IO.Directory.CreateDirectory quarantine |> ignore
                        Array.iter (moveToQuarantine dir quarantine) orphans)

            return ()
        }

    /// WAL ファイル番号のうち最大のものを返す。
    let private latestWalNumber =
        Wal.walFiles
        >> Result.map (fun files -> files |> List.choose parseNumber |> List.sort |> List.tryLast)

    /// WAL を開く（無ければ作る）。
    let private openOrCreateWal (dir: string) (version: Version) =
        result {
            let! latest = latestWalNumber dir

            let walNumber =
                match latest with
                | Some n -> n
                | None -> version.NextFileNumber

            let! exists = protect (fun () -> System.IO.File.Exists(Wal.path dir walNumber))

            if exists then
                return!
                    protect (fun () -> System.IO.FileInfo(Wal.path dir walNumber).Length)
                    |> Result.map (fun size -> { FileNumber = walNumber; Size = size })
            else
                return! Wal.create dir walNumber
        }

    /// ディスク上の最大 SeqNo を返す。
    let private maxSeqNoOnDisk: Version -> SeqNo =
        Version.allFiles >> List.fold (fun acc f -> max acc f.MaxSeqNo) 0UL

    /// ディスク上の最大ファイル番号を返す。
    let private maxFileNumberOnDisk (dir: string) =
        result {
            let! ssts = sstNumbersOnDisk dir
            let! wals = Wal.walFiles dir
            let walNumbers = wals |> List.choose parseNumber |> List.toArray
            return Array.append ssts walNumbers |> Array.fold (fun acc n -> max acc n) 0L
        }

    /// 空の engine を組み立てる。
    let private emptyEngine (dir: string) =
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

    /// SSTable の ReadInfo を 1つ埋める。
    let private hydrateFile (dir: string) (f: FileMetaData) =
        match f.ReadInfo with
        | Some _ -> ret f
        | None ->
            SstableReader.openPartial (FileMetaData.path dir f.FileNumber)
            |> Result.map (fun info -> { f with ReadInfo = Some info })

    /// 1レベルの ReadInfo を埋める。
    let private hydrateLevel (dir: string) (files: FileMetaData array) =
        files |> Array.map (hydrateFile dir) |> all |> Result.map List.toArray

    /// Version の全レベルの ReadInfo を埋める。
    let private hydrateVersion (dir: string) (version: Version) =
        version.Files
        |> Array.map (hydrateLevel dir)
        |> all
        |> Result.map (fun levels ->
            { version with
                Files = List.toArray levels
            })

    /// 復旧済みの状態から起動時のエンジンを作る。
    let private buildRecovered
        (dir: string)
        (version: Version)
        (manifestNumber: ManifestNumber)
        (walState: WalState)
        (mem: Memtable)
        (walMax: SeqNo)
        (diskMax: SeqNo)
        (diskMaxFile: FileNumber)
        (walMaxTxn: TxnId)
        =
        let nextSeqNo = max walMax diskMax + 1UL
        let nextTxnId = walMaxTxn + 1UL<txnId>

        let bumped =
            { version with
                NextFileNumber = max version.NextFileNumber (diskMaxFile + 1L)
            }

        { emptyEngine dir with
            ManifestNumber = manifestNumber
            Version = bumped
            Wal = walState
            NextSeqNo = nextSeqNo
            NextTxnId = nextTxnId
            Mem = mem
        }

    /// データベースを開く（新規なら作る）。
    /// [memLimitBytes] を指定すると memtable の上限を差し替える。
    let openDatabaseWith (memLimitBytes: int option) (dir: string) =
        result {
            let! version, manifestNumber = Manifest.openOrCreate dir
            do! ensureFilesPresent dir version
            do! quarantineOrphans dir version
            let! version' = hydrateVersion dir version
            let! walState = openOrCreateWal dir version'

            let! replayed =
                Wal.foldCommitted
                    dir
                    walState.FileNumber
                    (fun (mem, maxSeq) entry -> Memtable.insert entry mem, max maxSeq entry.SeqNo)
                    (Memtable.Empty, 0UL)

            let (mem, walMax), walMaxTxn, goodWalSize = replayed
            let! alignedWal = Wal.truncateTo dir goodWalSize walState
            let diskMax = maxSeqNoOnDisk version'
            let! diskMaxFile = maxFileNumberOnDisk dir

            let engine =
                buildRecovered dir version' manifestNumber alignedWal mem walMax diskMax diskMaxFile walMaxTxn

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
                        let! stale = Wal.walFiles flushed.Dir

                        stale
                        |> List.iter (fun p -> protect (fun () -> System.IO.File.Delete p) |> ignore)

                        return ()
                })

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

        member _.GetAtMasked (visible: uint64 option) (key: byte array) (mask: bool array) =
            StorageEngine.getAtMasked key mask visible engine

        member _.ScanAtValues (visible: uint64 option) (startKey: byte array) (limit: int) (needsValues: bool) =
            StorageEngine.scanAtValues visible limit startKey needsValues engine

        member _.ScanAtMasked (visible: uint64 option) (startKey: byte array) (limit: int) (mask: bool array) =
            StorageEngine.scanAtMasked visible limit startKey mask engine

        member _.WriteBatch(entries: Entry array) =
            StorageEngine.writeBatch entries engine
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.WriteBatchDeferred(entries: Entry array) =
            StorageEngine.writeBatchDeferred entries engine
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.WithNextSeqNo(nextSeqNo: SeqNo) =
            DiskStorageEngine { engine with NextSeqNo = nextSeqNo } :> IStorageEngine

        member _.WithActiveSnapshot(snapshot: uint64) =
            DiskStorageEngine
                { engine with
                    ActiveSnapshot = snapshot
                }
            :> IStorageEngine

        member _.WithHostState(state: PublishedState) =
            DiskStorageEngine
                { engine with
                    Version = state.Version
                    ManifestNumber = state.ManifestNumber
                    ActiveSnapshot = state.ActiveSnapshot
                }
            :> IStorageEngine

        member _.FlushIfNeeded(allocateFileNumber: unit -> FileNumber) =
            StorageEngine.flushIfNeededWith allocateFileNumber engine
            |> Result.map (fun (next, flushed) -> DiskStorageEngine next :> IStorageEngine, flushed)

        member _.MaybeFlush() =
            StorageEngine.maybeFlush engine
            |> Result.map (fun (next, error) -> DiskStorageEngine next :> IStorageEngine, error)

        member _.Close() = StorageEngine.close engine
