// ===========================================================================
// StorageEngine — LSM ツリーを 1本公開する層（keyspace 1本 = ここが 1本）。
//
// 探索は「memtable -> L0（新しい順）-> 各レベル（新しい順）」の順。
// 各ステップで Bloom filter と VersionSet の key range で枝刈りする。
// 可視性（SeqNo <= snapshot）は getAt / scanAt の visible フィルタで判定する。
// 制約: scan は結果を全部メモリに載せて返す。
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
        /// これより小さい版は誰も読まないので GC できる
        ActiveSnapshot: uint64
    }

module internal StorageEngine =
    /// SSTable に書く Bloom filter の誤検出率。
    [<Literal>]
    let private BloomFalsePositiveRate = 0.01

    /// 既定の memtable 上限（64 MiB）。
    [<Literal>]
    let private DefaultMemLimitBytes = 64 * 1024 * 1024

    /// 既定値で埋めた空の engine（Dir のみ引数から取る）。
    let private emptyEngine dir =
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

    /// SSTable ファイルをバイト列で読む。
    let private readFileBytes (dir: string) (fileNumber: int64) =
        protect (fun () -> System.IO.File.ReadAllBytes(FileMetaData.path dir fileNumber))

    /// ファイルを開く（Bloom と index を持つ reader を作る）。
    let private openSstable (engine: StorageEngine) (f: FileMetaData) =
        readFileBytes engine.Dir f.FileNumber >>= Sstable.openTable

    /// 開いた reader から startKey 以降の全エントリを配列にする。
    /// `[||]` なら先頭から全部。
    /// index の二分探索で startKey より前のブロックは読み飛ばす。
    let private readEntriesFrom (startKey: byte array) (reader: SstReader) =
        Sstable.rangeScan reader startKey 0 |> SstScan.toArray

    /// ファイル 1つを読んで startKey 以降の全エントリを配列にする。
    let private entriesOf (engine: StorageEngine) (startKey: byte array) (f: FileMetaData) =
        openSstable engine f >>= readEntriesFrom startKey

    /// ポイントルックアップ。
    /// memtable -> L0 -> 各レベルの順に見る。
    /// visible が Some s のときは SeqNo が s 以下の最新版を返す。
    /// これが MVCC の可視性。
    /// None なら制限なし。
    let getAt (engine: StorageEngine) (visible: uint64 option) (key: byte array) =
        let firstVisible (visible: uint64 option) (key: byte array) (scan: SstScan) =
            let rec seek (cur: SstScan) =
                match SstScan.next cur with
                | Error e -> Error e
                | Ok None -> ret None
                | Ok(Some(e, nxt)) ->
                    match Bytes.compare e.Key key with
                    | c when c < 0 -> seek nxt
                    | 0 -> if isVisible visible e then ret (Some e) else seek nxt
                    | _ -> ret None

            seek scan

        let rec probe (files: FileMetaData list) =
            match files with
            | [] -> ret None
            | f :: rest ->
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

    /// 範囲ルックアップ。
    /// キー昇順で返す（同一キーは 1 件だけ）。
    /// visible が Some s のときは SeqNo が s 以下の版を返す。
    let scanAt (engine: StorageEngine) (visible: uint64 option) (startKey: byte array) (limit: int) =
        let collapseDuplicates (visible: uint64 option) (items: SstEntry array) =
            let rec collapse (acc: SstEntry list) (arr: SstEntry array) (i: int) (emittedKey: byte array option) =
                if i >= arr.Length then
                    List.rev acc |> Array.ofList
                else
                    let e = arr.[i]

                    match emittedKey with
                    | Some k when Bytes.compare k e.Key = 0 -> collapse acc arr (i + 1) emittedKey
                    | _ when not (isVisible visible e) -> collapse acc arr (i + 1) emittedKey
                    | _ -> collapse (e :: acc) arr (i + 1) (Some e.Key)

            collapse [] items 0 None

        let memSeq =
            Memtable.rangeScan engine.Mem startKey 0
            |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
            |> Seq.toArray

        let files =
            Version.allFiles engine.Version
            |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
            |> List.sortByDescending _.FileNumber

        FileMetaData.foldFiles (entriesOf engine startKey) (fun acc entries -> entries :: acc) [] files
        |> Result.map (fun sources ->
            let ordered = List.rev sources
            let merged = memSeq :: ordered |> SstEntry.mergeSorted |> collapseDuplicates visible
            if limit > 0 then merged |> Array.truncate limit else merged)

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

        let deleteOrphans (dir: string) (version: Version) =
            let referenced = Version.allFiles version |> List.map _.FileNumber |> Set.ofList

            let orphans =
                sstNumbersOnDisk dir |> Array.filter (fun n -> not (Set.contains n referenced))

            protect (fun () ->
                orphans
                |> Array.iter (fun n -> System.IO.File.Delete(FileMetaData.path dir n))
                |> ignore)

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

        let entriesAt (dir: string) (startKey: byte array) (f: FileMetaData) =
            readFileBytes dir f.FileNumber
            >>= Sstable.openTable
            >>= readEntriesFrom startKey

        let maxSeqNoOnDisk (dir: string) (version: Version) =
            FileMetaData.foldFiles
                (entriesAt dir [||])
                (fun (acc: uint64) entries -> max acc (SstEntry.maxSeqNo entries))
                0UL
                (Version.allFiles version)

        let maxFileNumberOnDisk (dir: string) =
            let ssts = sstNumbersOnDisk dir
            let wals = Wal.walFiles dir |> List.choose parseNumber |> List.toArray
            Array.append ssts wals |> Array.fold (fun acc n -> max acc n) 0L

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
            do! deleteOrphans dir version'
            let! walState = openOrCreateWal dir version'
            let! replayed = Wal.read dir walState.FileNumber
            let! diskMax = maxSeqNoOnDisk dir version'
            return buildRecovered version' manifestNumber' walState replayed diskMax
        }

    /// データベースを閉じる（memtable を flush してから WAL を消す）。
    let close (engine: StorageEngine) =
        flush engine
        |> Result.map (fun e ->
            Wal.walFiles e.Dir
            |> List.iter (fun p -> protect (fun () -> System.IO.File.Delete p) |> ignore))

/// RowStore が使う StorageEngine の操作面。
/// テストではメモリ実装に差し替えられる。
type internal IStorageEngine =
    abstract NextSeqNo: uint64
    abstract GetAt: visible: uint64 option -> key: byte array -> Result<SstEntry option, DbError>
    abstract ScanAt: visible: uint64 option -> startKey: byte array -> limit: int -> Result<SstEntry array, DbError>
    abstract WriteBatch: entries: Entry array -> Result<IStorageEngine, DbError>
    abstract WithNextSeqNo: nextSeqNo: uint64 -> IStorageEngine
    abstract Close: unit -> Result<unit, DbError>

/// 実ディスクの StorageEngine を IStorageEngine として公開する。
type internal DiskStorageEngine(engine: StorageEngine) =
    interface IStorageEngine with
        member _.NextSeqNo = engine.NextSeqNo
        member _.GetAt visible key = StorageEngine.getAt engine visible key

        member _.ScanAt visible startKey limit =
            StorageEngine.scanAt engine visible startKey limit

        member _.WriteBatch entries =
            StorageEngine.writeBatch engine entries
            |> Result.map (fun next -> DiskStorageEngine next :> IStorageEngine)

        member _.WithNextSeqNo nextSeqNo =
            DiskStorageEngine { engine with NextSeqNo = nextSeqNo } :> IStorageEngine

        member _.Close() =
            StorageEngine.close engine |> Result.map ignore
