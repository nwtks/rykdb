// ===========================================================================
// StorageEngine — LSM ツリーを 1本公開する層（keyspace 1本 = ここが 1本）。
//
// 探索は「memtable -> L0（新しい順）-> 各レベル（新しい順）」の順。
// 各ステップで Bloom filter と VersionSet の key range で枝刈りする。
// 可視性（SeqNo <= snapshot）は getAt / scanAt の visible フィルタで判定する。
// ===========================================================================

namespace RykDb.Core

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

/// パース済み SSTable reader のプロセス内キャッシュ。
/// SSTable は一度書かれたら内容が変わらない（ファイル番号は単調増加）ので、
/// パス + サイズ + mtime をキーにすれば安全に再利用できる。
/// 大きな DB でもメモリを圧迫しないよう、値は弱参照で持つ（GC が回収できる）。
module internal SstReaderCache =
    let private cache =
        System.Collections.Concurrent.ConcurrentDictionary<string, System.WeakReference<SstReader>>
            System.StringComparer.Ordinal

    /// ファイルの同一性を表すキー。
    /// 同じ番号のファイルが差し替わった場合はサイズ/mtime が変わるので別キーになる。
    let keyOf (path: string) (info: System.IO.FileInfo) =
        sprintf "%s|%d|%d" path info.Length info.LastWriteTimeUtc.Ticks

    let tryGet (key: string) =
        match cache.TryGetValue key with
        | true, weak ->
            match weak.TryGetTarget() with
            | true, reader -> Some reader
            | _ ->
                cache.TryRemove key |> ignore
                None
        | _ -> None

    let put (key: string) (reader: SstReader) =
        cache.[key] <- System.WeakReference<SstReader> reader

/// 部分読み用の SSTable リーダー（index / Bloom / meta のみ）のキャッシュ。
module internal SstPartialCache =
    let private cache =
        System.Collections.Concurrent.ConcurrentDictionary<string, System.WeakReference<SstPartial>>
            System.StringComparer.Ordinal

    let tryGet (key: string) =
        match cache.TryGetValue key with
        | true, weak ->
            match weak.TryGetTarget() with
            | true, partial -> Some partial
            | _ ->
                cache.TryRemove key |> ignore
                None
        | _ -> None

    let put (key: string) (partial: SstPartial) =
        cache.[key] <- System.WeakReference<SstPartial> partial

module internal StorageEngine =
    /// SSTable に書く Bloom filter の誤検出率。
    [<Literal>]
    let private BloomFalsePositiveRate = 0.01

    /// 既定の memtable 上限（64 MiB）。
    [<Literal>]
    let private DefaultMemLimitBytes = 64 * 1024 * 1024

    /// ファイル名（拡張子除く）を番号として読む。
    /// SSTable でも WAL でも同じ規則。
    let private parseNumber (path: string) =
        match System.Int64.TryParse(System.IO.Path.GetFileNameWithoutExtension path) with
        | true, v -> Some v
        | _ -> None

    /// snapshot（visible）から見える版かどうか。
    /// None なら常に見える。
    let private isVisible (visible: uint64 option) (e: SstEntry) =
        match visible with
        | Some s -> e.SeqNo <= s
        | None -> true

    /// ファイルを開く（Bloom と index を持つ reader を作る）。
    /// パース結果は弱参照キャッシュに載せ、同じファイルを何度も読まない。
    let private openSstable (engine: StorageEngine) (f: FileMetaData) =
        let readFileBytes (dir: string) (fileNumber: int64) =
            protect (fun () -> System.IO.File.ReadAllBytes(FileMetaData.path dir fileNumber))

        let path = FileMetaData.path engine.Dir f.FileNumber

        match protect (fun () -> SstReaderCache.keyOf path (System.IO.FileInfo path)) with
        | Error e -> Error e
        | Ok key ->
            match SstReaderCache.tryGet key with
            | Some reader -> Ok reader
            | None ->
                readFileBytes engine.Dir f.FileNumber
                >>= Sstable.openTable
                >>= fun reader ->
                    SstReaderCache.put key reader
                    Ok reader

    /// 部分読み用の reader を開く（index / Bloom / meta のみ）。
    let private openPartialSst (engine: StorageEngine) (f: FileMetaData) =
        let path = FileMetaData.path engine.Dir f.FileNumber

        match protect (fun () -> SstReaderCache.keyOf path (System.IO.FileInfo path)) with
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
    let getAt (engine: StorageEngine) (visible: uint64 option) (key: byte array) =
        let rec seek (visible: uint64 option) (key: byte array) (cur: SstScan) =
            match SstScan.next cur with
            | Error e -> Error e
            | Ok None -> ret None
            | Ok(Some(e, nxt)) ->
                match Bytes.compare e.Key key with
                | c when c < 0 -> seek visible key nxt
                | 0 ->
                    if isVisible visible e then
                        ret (Some e)
                    else
                        seek visible key nxt
                | _ -> ret None

        let firstVisible (visible: uint64 option) (key: byte array) (scan: SstScan) = seek visible key scan

        let rec probe (files: FileMetaData list) =
            match files with
            | [] -> ret None
            | f :: rest ->
                match openPartialSst engine f with
                | Error e -> Error e
                | Ok partial when partial.HasBlockChecksums ->
                    match Sstable.lookup partial visible key with
                    | Error e -> Error e
                    | Ok(Some e) -> ret (Some e)
                    | Ok None -> probe rest
                | Ok _ ->
                    match openSstable engine f with
                    | Error e -> Error e
                    | Ok reader ->
                        if not (Sstable.mayContain reader key) then
                            probe rest
                        else
                            match Sstable.rangeScan reader key 0 |> firstVisible visible key with
                            | Error e -> Error e
                            | Ok(Some e) -> ret (Some e)
                            | Ok None -> probe rest

        match Memtable.getVisible engine.Mem key visible with
        | Some e -> ret (Some(SstEntry.make e.Key e.SeqNo e.Value))
        | None -> Version.filesForPointLookup engine.Version key |> probe

    /// k-way merge のソース。
    type private MergeSource =
        /// 未オープンのファイル。byte array はそのファイルが返し得る最小キーの下限。
        | Unopened of byte array * FileMetaData
        /// オープン済み（先頭エントリと残りの走査）。
        | Opened of SstEntry * SstScan
        /// 空。
        | Drained

    /// 範囲ルックアップ。
    /// キー昇順で返す（同一キーは 1件だけ）。
    /// visible が Some s のときは SeqNo が s 以下の版を返す。
    /// limit > 0 のときは必要なファイルだけを開いて先頭 limit 件を返す。
    /// limit <= 0 のときは全ファイルを開いてチェックサムを検証してから遅延 merge する。
    let scanAt (engine: StorageEngine) (visible: uint64 option) (startKey: byte array) (limit: int) =
        let sourceBound (startKey: byte array) (f: FileMetaData) =
            if Bytes.compare f.Smallest startKey > 0 then
                f.Smallest
            else
                startKey

        let seqToScan (rows: seq<SstEntry>) =
            let e = rows.GetEnumerator()

            let rec pull () =
                if e.MoveNext() then
                    SstScan.emit e.Current pull
                else
                    SstScan.empty

            pull ()

        let rec pick (i: int) (best: (int * SstEntry * SstScan) option) (rest: MergeSource list) =
            match rest with
            | [] -> best
            | Opened(e, scan) :: tail ->
                match best with
                | None -> pick (i + 1) (Some(i, e, scan)) tail
                | Some(_, be, _) ->
                    if SstEntry.compare e be < 0 then
                        pick (i + 1) (Some(i, e, scan)) tail
                    else
                        pick (i + 1) best tail
            | Unopened _ :: tail
            | Drained :: tail -> pick (i + 1) best tail

        let rec loop rest value (k: int) (revPrefix: MergeSource list) (tail: MergeSource list) =
            match tail with
            | [] -> rest
            | s :: tail' ->
                if k = 0 then
                    List.append (List.rev revPrefix) (value :: tail')
                else
                    loop rest value (k - 1) (s :: revPrefix) tail'

        let replaceAt (i: int) (value: MergeSource) (rest: MergeSource list) = loop rest value i [] rest

        let rec advance (state: MergeSource list) =
            match pick 0 None state with
            | None -> None
            | Some(i, e, rest) ->
                let nextSource =
                    match SstScan.next rest with
                    | Error _ -> Drained
                    | Ok None -> Drained
                    | Ok(Some(e2, nxt)) -> Opened(e2, nxt)

                Some(e, replaceAt i nextSource state)

        let lazyMergeScan (sources: MergeSource list) = Seq.unfold advance sources

        let isVisible (e: SstEntry) =
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true

        /// 遅延 collapseDuplicates（同一キーの最新版だけ残す）。
        let collapseDuplicatesLazy (src: seq<SstEntry>) =
            Seq.unfold
                (fun (cur: System.Collections.Generic.IEnumerator<SstEntry>, emittedKey: byte array option) ->
                    let rec next () =
                        if cur.MoveNext() then
                            let e = cur.Current

                            match emittedKey with
                            | Some k when Bytes.compare k e.Key = 0 ->
                                // 同一キーの古い版 → スキップ
                                next ()
                            | _ when not (isVisible e) ->
                                // 見えない版 → スキップ
                                next ()
                            | _ -> Some(e, (cur, Some e.Key))
                        else
                            None

                    next ())
                (src.GetEnumerator(), None)

        let rec pickOpened (i: int) (best: (int * SstEntry * SstScan) option) (rest: MergeSource list) =
            match rest with
            | [] -> best
            | Opened(e, scan) :: tail ->
                match best with
                | None -> pickOpened (i + 1) (Some(i, e, scan)) tail
                | Some(_, be, _) ->
                    if SstEntry.compare e be < 0 then
                        pickOpened (i + 1) (Some(i, e, scan)) tail
                    else
                        pickOpened (i + 1) best tail
            | Unopened _ :: tail
            | Drained :: tail -> pickOpened (i + 1) best tail

        let rec pickUnopened (i: int) (best: (int * byte array * FileMetaData) option) (rest: MergeSource list) =
            match rest with
            | [] -> best
            | Unopened(bound, f) :: tail ->
                match best with
                | None -> pickUnopened (i + 1) (Some(i, bound, f)) tail
                | Some(_, bestBound, _) ->
                    if Bytes.compare bound bestBound < 0 then
                        pickUnopened (i + 1) (Some(i, bound, f)) tail
                    else
                        pickUnopened (i + 1) best tail
            | _ :: tail -> pickUnopened (i + 1) best tail

        let replaceAt (i: int) (value: MergeSource) (list: MergeSource list) =
            let rec loop (k: int) (revPrefix: MergeSource list) (rest: MergeSource list) =
                match rest with
                | [] -> list
                | s :: tail ->
                    if k = 0 then
                        List.append (List.rev revPrefix) (value :: tail)
                    else
                        loop (k - 1) (s :: revPrefix) tail

            loop i [] list

        let openAt (i: int) (f: FileMetaData) (state: MergeSource list) =
            let startScan =
                match openPartialSst engine f with
                | Error e -> Error e
                | Ok partial when partial.HasBlockChecksums -> Ok(Sstable.rangeScanPartial partial startKey 0)
                | Ok _ ->
                    openSstable engine f
                    |> Result.map (fun reader -> Sstable.rangeScan reader startKey 0)

            match startScan with
            | Error e -> Error e
            | Ok scan ->
                match SstScan.next scan with
                | Error e -> Error e
                | Ok None -> Ok(replaceAt i Drained state)
                | Ok(Some(e, nxt)) -> Ok(replaceAt i (Opened(e, nxt)) state)

        let rec prepare (state: MergeSource list) =
            match pickOpened 0 None state, pickUnopened 0 None state with
            | None, None -> Ok state
            | None, Some(i, _, f) ->
                match openAt i f state with
                | Error e -> Error e
                | Ok state' -> prepare state'
            | Some(_, e, _), Some(i, bound, f) when Bytes.compare bound e.Key <= 0 ->
                match openAt i f state with
                | Error e -> Error e
                | Ok state' -> prepare state'
            | Some _, _ -> Ok state

        let rec collect (state: MergeSource list) (emittedKey: byte array option) (count: int) (acc: SstEntry list) =
            if count >= limit then
                Ok(List.rev acc)
            else
                match prepare state with
                | Error e -> Error e
                | Ok state ->
                    match pickOpened 0 None state with
                    | None -> Ok(List.rev acc)
                    | Some(i, e, rest) ->
                        match SstScan.next rest with
                        | Error err -> Error err
                        | Ok next ->
                            let nextSource =
                                match next with
                                | None -> Drained
                                | Some(e2, nxt) -> Opened(e2, nxt)

                            let state' = replaceAt i nextSource state

                            let sameKey =
                                match emittedKey with
                                | Some k -> Bytes.compare k e.Key = 0
                                | None -> false

                            if sameKey || not (isVisible e) then
                                collect state' emittedKey count acc
                            else
                                let appended = e.Value.IsSome

                                collect
                                    state'
                                    (Some e.Key)
                                    (if appended then count + 1 else count)
                                    (if appended then e :: acc else acc)

        let collectLimited (sources: MergeSource list) = collect sources None 0 []

        let rec openAll (rest: FileMetaData list) (acc: MergeSource list) =
            match rest with
            | [] -> ret (List.rev acc)
            | f :: tail ->
                match openSstable engine f with
                | Error e -> Error e
                | Ok reader ->
                    match SstScan.next (Sstable.rangeScan reader startKey 0) with
                    | Error e -> Error e
                    | Ok None -> openAll tail (Drained :: acc)
                    | Ok(Some(e, nxt)) -> openAll tail (Opened(e, nxt) :: acc)

        let sstFiles =
            Version.allFiles engine.Version
            |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
            |> List.sortByDescending _.FileNumber

        let memSeq =
            Memtable.rangeScan engine.Mem startKey 0
            |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)

        let memSource =
            match SstScan.next (seqToScan memSeq) with
            | Ok(Some(e, nxt)) -> Opened(e, nxt)
            | _ -> Drained

        if limit > 0 then
            let fileSources =
                sstFiles |> List.map (fun f -> Unopened(sourceBound startKey f, f))

            collectLimited (memSource :: fileSources) |> Result.map Seq.ofList
        else
            let finalize (merged: seq<SstEntry>) =
                collapseDuplicatesLazy merged |> Seq.filter (fun e -> e.Value.IsSome)

            openAll sstFiles []
            |> Result.map (fun sources -> finalize (lazyMergeScan (memSource :: sources)))

    /// 書き込みバッチ。
    /// まず WAL（Begin/Batch/Commit を 1 回の書き込みで）へ、
    /// それから memtable へ反映する。
    let writeBatch (engine: StorageEngine) (entries: Entry array) =
        let apply (wal: WalState) (txnId: uint64) =
            let mem = Memtable.apply entries engine.Mem
            let nextTxnId = txnId + 1UL

            { engine with
                Wal = wal
                Mem = mem
                NextTxnId = nextTxnId
            }

        result {
            let txnId = engine.NextTxnId

            let! wal =
                Wal.append
                    engine.Wal
                    engine.Dir
                    [
                        WalRecord.Begin txnId
                        WalRecord.Batch(txnId, entries)
                        WalRecord.Commit txnId
                    ]

            return apply wal txnId
        }

    let private writeOne (engine: StorageEngine) (entry: Entry) =
        let nextSeqNo = engine.NextSeqNo + 1UL

        writeBatch engine [| entry |]
        |> Result.map (fun next -> { next with NextSeqNo = nextSeqNo })

    /// 1 件書く。
    let put (engine: StorageEngine) (key: byte array) (value: SqlValue array) =
        Entry.put key engine.NextSeqNo value |> writeOne engine

    /// 1 件削除する（tombstone を書く）。
    let delete (engine: StorageEngine) (key: byte array) =
        Entry.delete key engine.NextSeqNo |> writeOne engine

    /// memtable を L0 の SSTable にして書き出す。
    /// memtable が空なら何もしない（WAL も切り替えない）。
    let flush (engine: StorageEngine) =
        let rotateWal (engine: StorageEngine) (next: WalState) =
            result {
                do! protect (fun () -> Wal.walFiles engine.Dir |> List.iter System.IO.File.Delete |> ignore)
                let! _ = Wal.create engine.Dir next.FileNumber
                return { engine with Wal = next }
            }

        let finishFlush (engine: StorageEngine) (version: Version) (manifestNumber: int64) =
            result {
                let! rotated = rotateWal engine (Wal.rotate engine.Wal)

                return
                    { rotated with
                        Version =
                            { version with
                                NextFileNumber = version.NextFileNumber + 1L
                                SequenceNumber = engine.Version.SequenceNumber + 1UL
                            }
                        ManifestNumber = manifestNumber
                        Mem = Memtable.empty
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
    ///   2. 入力を読んでマージし、GC で不要な版を落とす
    ///   3. 出力をファイルに書き、MANIFEST に記録する（先に MANIFEST）
    ///   4. 入力ファイルを削除する（MANIFEST の後に）
    /// 3 と 4 の順序が重要: MANIFEST を先に書かないと、
    /// クラッシュ時に「MANIFEST は新しいがファイルは古い」= データ損失になる。
    let private compactOnce (engine: StorageEngine) =
        let readEntriesFrom (startKey: byte array) (reader: SstReader) =
            Sstable.rangeScan reader startKey 0 |> SstScan.toArray

        let entriesOf (engine: StorageEngine) (startKey: byte array) (f: FileMetaData) =
            openSstable engine f >>= readEntriesFrom startKey

        let buildCompacted (version: Version) (manifestNumber: int64) (result: CompactionResult) =
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
                    (entriesOf engine [||])

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

    /// 必要なら compaction する。
    let maybeCompact (engine: StorageEngine) = compactOnce engine

    /// memtable が上限に達していれば flush し、続けて compaction する。
    /// 上限に達していなければ何もしない。
    let maybeFlush (engine: StorageEngine) =
        if engine.Mem.Count > 0 && engine.Mem.ApproxBytes >= engine.MemLimitBytes then
            flush engine >>= maybeCompact
        else
            ret engine

    /// データベースを開く（新規なら作る）。
    let openDatabase (dir: string) =
        let dropMissingFiles (dir: string) (manifestNumber: int64) (version: Version) =
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

        let sstNumbersOnDisk (dir: string) =
            System.IO.Directory.GetFiles(dir, "*.sst") |> Array.choose parseNumber

        let move quarantine (n: int64) =
            let source = FileMetaData.path dir n
            let name = System.IO.Path.GetFileName source
            let target = System.IO.Path.Combine(quarantine, name)

            let target =
                if System.IO.File.Exists target then
                    target + "." + string System.DateTime.UtcNow.Ticks
                else
                    target

            System.IO.File.Move(source, target, true)

        let quarantineOrphans (dir: string) (version: Version) =
            let referenced = Version.allFiles version |> List.map _.FileNumber |> Set.ofList

            let orphans =
                sstNumbersOnDisk dir |> Array.filter (fun n -> not (Set.contains n referenced))

            protect (fun () ->
                if not (Array.isEmpty orphans) then
                    let quarantine = System.IO.Path.Combine(dir, ".orphans")
                    System.IO.Directory.CreateDirectory quarantine |> ignore
                    Array.iter (move quarantine) orphans)

        let latestWalNumber (dir: string) =
            Wal.walFiles dir |> List.choose parseNumber |> List.sort |> List.tryLast

        let openOrCreateWal (dir: string) (version: Version) =
            let walNumber =
                match latestWalNumber dir with
                | Some n -> n
                | None -> version.NextFileNumber

            if System.IO.File.Exists(Wal.path dir walNumber) then
                ret
                    {
                        FileNumber = walNumber
                        Size = int (System.IO.FileInfo(Wal.path dir walNumber).Length)
                    }
            else
                Wal.create dir walNumber

        let maxSeqNoOnDisk (version: Version) =
            Version.allFiles version |> List.fold (fun acc f -> max acc f.MaxSeqNo) 0UL

        let maxFileNumberOnDisk (dir: string) =
            let ssts = sstNumbersOnDisk dir
            let wals = Wal.walFiles dir |> List.choose parseNumber |> List.toArray
            Array.append ssts wals |> Array.fold (fun acc n -> max acc n) 0L

        let emptyEngine dir =
            {
                Dir = dir
                ManifestNumber = 1L
                Version = Version.empty
                Mem = Memtable.empty
                Wal = { FileNumber = 1L; Size = 0 }
                NextSeqNo = 1UL
                NextTxnId = 1UL
                MemLimitBytes = DefaultMemLimitBytes
                Compaction = CompactionOptions.defaults
                ActiveSnapshot = 0UL
            }

        let buildRecovered
            (version: Version)
            (manifestNumber: int64)
            (walState: WalState)
            (replayed: Entry array)
            (diskMax: uint64)
            =
            let walMax = Entry.maxSeqNo replayed
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
                Mem = Memtable.apply replayed Memtable.empty
            }

        result {
            let! version, manifestNumber = Manifest.openOrCreate dir
            let! version', manifestNumber' = dropMissingFiles dir manifestNumber version
            do! quarantineOrphans dir version'
            let! walState = openOrCreateWal dir version'
            let! replayed = Wal.read dir walState.FileNumber
            let diskMax = maxSeqNoOnDisk version'
            return buildRecovered version' manifestNumber' walState replayed diskMax
        }

    /// データベースを閉じる（memtable を flush してから WAL を消す）。
    /// <remarks>
    /// ハンドルは不変なので、古いハンドルでも `Close` を呼べる。
    /// 古いハンドルの WAL は新しいハンドルが既に書き進めているため、
    /// WAL の実サイズがハンドルの記録と一致しないときは flush も WAL 削除もしない
    /// （古い memtable を flush すると新しいハンドルの書き込みを消してしまう）。
    /// </remarks>
    let close (engine: StorageEngine) =
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
        member _.GetAt visible key = StorageEngine.getAt engine visible key

        member _.ScanAt visible startKey limit =
            StorageEngine.scanAt engine visible startKey limit

        member _.WriteBatch entries =
            StorageEngine.writeBatch engine entries
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

        member _.Close() =
            StorageEngine.close engine |> Result.map ignore
