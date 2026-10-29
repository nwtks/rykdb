// ===========================================================================
// StorageEngine — LSM ツリーを 1本公開する層（keyspace 1本 = ここが 1本）。
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

    let put (key: string) (partial: SstPartial) =
        Cache.[key] <- System.WeakReference<SstPartial> partial

/// StorageEngine の不変状態。
/// すべての操作は新しい状態を返す。
type internal StorageEngine =
    {
        /// データベースのディレクトリ
        Dir: string
        /// 有効な manifest 番号
        ManifestNumber: int64
        /// 現在の LSM 形状
        Version: Version
        /// 書き込み先 memtable
        Mem: Memtable
        /// 現在の WAL
        Wal: WalState
        /// 次に採番する seqNo
        NextSeqNo: uint64
        /// 次に採番する txnId
        NextTxnId: uint64
        /// memtable のメモリ上限（超えたら flush する）
        MemLimitBytes: int
        /// コンパクションの設定
        Compaction: CompactionOptions
        /// 最古の active snapshot の SeqNo。
        /// GC は「これより新しい版」と「これ以下の最新 1版」を残す（Compaction.gc）。
        /// 0 は「全版を残す」。
        ActiveSnapshot: uint64
    }

module internal StorageEngine =
    // writeBatch と close は、同じ immutable handle 系列の WAL 競合を防ぐため直列化する。
    let private WriteCloseGate = new System.Threading.SemaphoreSlim(1, 1)

    /// SSTable に書く Bloom filter の誤検出率。
    [<Literal>]
    let private BloomFalsePositiveRate = 0.01

    /// 既定の memtable 上限（64 MiB）。
    [<Literal>]
    let private DefaultMemLimitBytes = 64 * 1024 * 1024

    /// 部分読み用の reader を開く（index / Bloom / meta のみ）。
    let private openPartialSst (f: FileMetaData) (engine: StorageEngine) =
        let path = FileMetaData.path engine.Dir f.FileNumber

        match protect (fun () -> SstPartialCache.keyOf path (System.IO.FileInfo path)) with
        | Error e -> Error e
        | Ok key ->
            match SstPartialCache.tryGet key with
            | Some partial -> Ok partial
            | None ->
                Sstable.openPartial path
                >>= fun partial ->
                    SstPartialCache.put key partial
                    Ok partial

    /// ポイントルックアップ。
    /// memtable -> L0 -> 各レベルの順に見る。
    /// visible が Some s のときは SeqNo が s 以下の最新版を返す。
    /// これが MVCC の可視性。
    /// None なら制限なし。
    let getAt (visible: uint64 option) (key: byte array) (engine: StorageEngine) =
        let rec probe =
            function
            | [] -> ret None
            | f :: rest ->
                match openPartialSst f engine with
                | Error e -> Error e
                | Ok partial ->
                    match Sstable.lookup key visible partial with
                    | Error e -> Error e
                    | Ok(Some e) -> ret (Some e)
                    | Ok None -> probe rest

        match Memtable.getVisible key visible engine.Mem with
        | Some e -> ret (Some(SstEntry.make e.Key e.SeqNo e.Value))
        | None -> Version.filesForPointLookup key engine.Version |> probe

    /// 範囲ルックアップ。
    /// キー昇順で返す（同一キーは 1件だけ）。
    /// visible が Some s のときは SeqNo が s 以下の版を返す。
    /// limit > 0 のときは必要なファイルだけを開いて先頭 limit 件を返す。
    /// limit <= 0 のときは最後まで読むので全ファイルを開く（破損・欠落はその時点で検出する）。
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
                    SstScan.Empty

            pull ()

        let sourceBound startKey f =
            if Bytes.compare f.Smallest startKey > 0 then
                f.Smallest
            else
                startKey

        let openPending index f merge =
            let startScan =
                match openPartialSst f engine with
                | Error e -> Error e
                | Ok partial -> Ok(Sstable.rangeScanPartial startKey 0 partial)

            match startScan with
            | Error e -> Error e
            | Ok scan ->
                match SstScan.next scan with
                | Error e -> Error e
                | Ok None -> Ok merge
                | Ok(Some(e, nxt)) -> Ok(SstMerge.insert index e nxt merge)

        let rec prepare pending merge =
            match pending with
            | [] -> Ok(merge, [])
            | (index, bound, f) :: rest ->
                match SstMerge.top merge with
                | Some top when Bytes.compare bound top.Key > 0 -> Ok(merge, pending)
                | _ ->
                    match openPending index f merge with
                    | Error e -> Error e
                    | Ok merge' -> prepare rest merge'

        let isVisible e =
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true

        let rec collectLimitted pending emittedKey acc count merge =
            if limit > 0 && count >= limit then
                Ok(List.rev acc)
            else
                match prepare pending merge with
                | Error e -> Error e
                | Ok(ready, remaining) ->
                    match SstMerge.next ready with
                    | Error e -> Error e
                    | Ok None -> Ok(List.rev acc)
                    | Ok(Some(e, merge')) ->
                        let sameKey =
                            match emittedKey with
                            | Some k -> Bytes.compare k e.Key = 0
                            | None -> false

                        if sameKey || not (isVisible e) then
                            collectLimitted remaining emittedKey acc count merge'
                        else
                            let appended = e.Value.IsSome

                            collectLimitted
                                remaining
                                (Some e.Key)
                                (if appended then e :: acc else acc)
                                (if appended then count + 1 else count)
                                merge'

        let rec collectAll pending emittedKey acc merge =
            match prepare pending merge with
            | Error e -> Error e
            | Ok(ready, remaining) ->
                match SstMerge.next ready with
                | Error e -> Error e
                | Ok None -> Ok(List.rev acc)
                | Ok(Some(e, merge')) ->
                    let sameKey =
                        match emittedKey with
                        | Some k -> Bytes.compare k e.Key = 0
                        | None -> false

                    if sameKey || not (isVisible e) then
                        collectAll remaining emittedKey acc merge'
                    elif e.Value.IsSome then
                        collectAll remaining (Some e.Key) (e :: acc) merge'
                    else
                        collectAll remaining (Some e.Key) acc merge'

        let collect merge =
            let pendingFiles =
                Version.allFiles engine.Version
                |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
                |> List.map (fun f -> sourceBound startKey f, f)
                |> List.sortWith (fun (a, _) (b, _) -> Bytes.compare a b)
                |> List.mapi (fun i (bound, f) -> i + 1, bound, f)

            if limit > 0 then
                collectLimitted pendingFiles None [] 0 merge |> Result.map Seq.ofList
            else
                collectAll pendingFiles None [] merge |> Result.map Seq.ofList

        let memSeq =
            Memtable.rangeScan startKey 0 engine.Mem
            |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)

        let initial =
            match SstScan.next (seqToScan memSeq) with
            | Error e -> Error e
            | Ok None -> Ok SstMerge.Empty
            | Ok(Some(e, nxt)) -> Ok(SstMerge.insert 0 e nxt SstMerge.Empty)

        match initial with
        | Error e -> Error e
        | Ok merge -> collect merge

    /// 書き込みバッチ。
    /// まず WAL（Begin/Batch/Commit を 1 回の書き込みで）へ、
    /// それから memtable へ反映する。
    let writeBatch (entries: Entry array) (engine: StorageEngine) =
        let apply wal txnId =
            let mem = Memtable.apply entries engine.Mem
            let nextTxnId = txnId + 1UL

            { engine with
                Wal = wal
                Mem = mem
                NextTxnId = nextTxnId
            }

        WriteCloseGate.Wait()

        try
            result {
                let txnId = engine.NextTxnId

                let! wal =
                    Wal.append
                        engine.Dir
                        [
                            WalRecord.Begin txnId
                            WalRecord.Batch(txnId, entries)
                            WalRecord.Commit txnId
                        ]
                        engine.Wal

                return apply wal txnId
            }
        finally
            WriteCloseGate.Release() |> ignore

    /// memtable を L0 の SSTable にして書き出す。
    /// memtable が空なら何もしない（WAL も切り替えない）。
    let private flush (engine: StorageEngine) =
        let rotateWal engine (next: WalState) =
            result {
                do! protect (fun () -> Wal.walFiles engine.Dir |> List.iter System.IO.File.Delete |> ignore)
                let! _ = Wal.create engine.Dir next.FileNumber
                return { engine with Wal = next }
            }

        let finishFlush engine version manifestNumber =
            result {
                let! rotated = rotateWal engine (Wal.rotate engine.Wal)

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
                    Memtable.toArray engine.Mem
                    |> Array.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
                    |> FileMetaData.writeSstable engine.Dir fileNumber 0 BloomFalsePositiveRate

                let! version, manifestNumber =
                    VersionEdit.addFile meta
                    |> Manifest.logAndApply engine.Dir engine.ManifestNumber engine.Version

                return! finishFlush engine version manifestNumber
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
        let scanOf f =
            match openPartialSst f engine with
            | Error e -> Error e
            | Ok partial -> Ok(Sstable.rangeScanPartial [||] 0 partial)

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

    /// memtable が上限に達していれば flush し、続けて compaction する。
    /// 上限に達していなければ何もしない。
    let maybeFlush (engine: StorageEngine) =
        if engine.Mem.Count > 0 && engine.Mem.ApproxBytes >= engine.MemLimitBytes then
            flush engine >>= compactOnce
        else
            ret engine

    /// データベースを開く（新規なら作る）。
    let openDatabase (dir: string) =
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
                ManifestNumber = 1L
                Version = Version.Empty
                Mem = Memtable.Empty
                Wal = { FileNumber = 1L; Size = 0 }
                NextSeqNo = 1UL
                NextTxnId = 1UL
                MemLimitBytes = DefaultMemLimitBytes
                Compaction = CompactionOptions.Defaults
                ActiveSnapshot = 0UL
            }

        let buildRecovered version manifestNumber walState mem walMax diskMax =
            let nextSeqNo = max walMax diskMax + 1UL

            let bumped =
                { version with
                    NextFileNumber = max version.NextFileNumber (maxFileNumberOnDisk dir + 1L)
                }

            { emptyEngine dir with
                ManifestNumber = manifestNumber
                Version = bumped
                Wal = walState
                NextSeqNo = nextSeqNo
                Mem = mem
            }

        result {
            let! version, manifestNumber = Manifest.openOrCreate dir
            let! version', manifestNumber' = dropMissingFiles dir manifestNumber version
            do! quarantineOrphans dir version'
            let! walState = openOrCreateWal dir version'

            let! mem, walMax =
                Wal.foldCommitted
                    dir
                    walState.FileNumber
                    (fun (mem, maxSeq) entry -> Memtable.insert entry mem, max maxSeq entry.SeqNo)
                    (Memtable.Empty, 0UL)

            let diskMax = maxSeqNoOnDisk version'
            return buildRecovered version' manifestNumber' walState mem walMax diskMax
        }

    /// データベースを閉じる（memtable を flush してから WAL を消す）。
    /// <remarks>
    /// ハンドルは不変なので、古いハンドルでも `Close` を呼べる。
    /// 古いハンドルの WAL は新しいハンドルが既に書き進めているため、
    /// WAL の実サイズがハンドルの記録と一致しないときは flush も WAL 削除もしない
    /// （古い memtable を flush すると新しいハンドルの書き込みを消してしまう）。
    /// </remarks>
    let close (engine: StorageEngine) =
        WriteCloseGate.Wait()

        try
            let isLatestWriter =
                match protect (fun () -> System.IO.FileInfo(Wal.path engine.Dir engine.Wal.FileNumber).Length) with
                | Ok size -> size = int64 engine.Wal.Size
                | Error _ -> false

            if not isLatestWriter then
                ret ()
            else
                flush engine
                |> Result.map (fun e ->
                    Wal.walFiles e.Dir
                    |> List.iter (fun p -> protect (fun () -> System.IO.File.Delete p) |> ignore))
        finally
            WriteCloseGate.Release() |> ignore

/// RowStore が使う StorageEngine の操作面。
/// テストではメモリ実装に差し替えられる。
type internal IStorageEngine =
    abstract NextSeqNo: uint64
    abstract ActiveSnapshot: uint64
    abstract GetAt: visible: uint64 option -> key: byte array -> Result<SstEntry option, DbError>
    abstract ScanAt: visible: uint64 option -> startKey: byte array -> limit: int -> Result<seq<SstEntry>, DbError>
    abstract WriteBatch: entries: Entry array -> Result<IStorageEngine, DbError>
    abstract WithNextSeqNo: nextSeqNo: uint64 -> IStorageEngine
    abstract WithActiveSnapshot: snapshot: uint64 -> IStorageEngine
    abstract MaybeFlush: unit -> Result<IStorageEngine, DbError>
    abstract Close: unit -> Result<unit, DbError>

/// 実ディスクの StorageEngine を IStorageEngine として公開する。
type internal DiskStorageEngine(engine: StorageEngine) =
    interface IStorageEngine with
        member _.NextSeqNo = engine.NextSeqNo
        member _.ActiveSnapshot = engine.ActiveSnapshot
        member _.GetAt visible key = StorageEngine.getAt visible key engine

        member _.ScanAt visible startKey limit =
            StorageEngine.scanAt visible limit startKey engine

        member _.WriteBatch entries =
            StorageEngine.writeBatch entries engine
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.WithNextSeqNo nextSeqNo =
            DiskStorageEngine { engine with NextSeqNo = nextSeqNo } :> IStorageEngine

        member _.WithActiveSnapshot snapshot =
            DiskStorageEngine
                { engine with
                    ActiveSnapshot = snapshot
                }
            :> IStorageEngine

        member _.MaybeFlush() =
            StorageEngine.maybeFlush engine
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.Close() = StorageEngine.close engine
