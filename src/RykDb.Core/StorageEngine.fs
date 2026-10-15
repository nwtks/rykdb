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
        /// ファイルキャッシュ
        Cache: BlockCache
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

    /// 既定のファイルキャッシュ容量（ファイル数）。
    [<Literal>]
    let private DefaultCacheFiles = 32

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
            Cache = BlockCache.create DefaultCacheFiles
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

    /// SSTable ファイルを直接バイト列で読む（キャッシュを通さない）。
    let private readFileBytes (dir: string) (fileNumber: int64) =
        protect (fun () -> System.IO.File.ReadAllBytes(Sstable.path dir fileNumber))

    /// ファイルを開く（Bloom と index を持つ reader を作る）。
    let private openSstable (engine: StorageEngine) (f: FileMetaData) =
        /// SSTable のファイル単位のバイト列を（キャッシュ経由で）読む。
        let readSstableBytes (engine: StorageEngine) (fileNumber: int64) =
            match BlockCache.get engine.Cache fileNumber 0 with
            | Some(_, bytes) -> ret bytes
            | None ->
                readFileBytes engine.Dir fileNumber
                |> Result.map (fun bytes ->
                    {
                        FileNumber = fileNumber
                        BlockId = 0
                        Data = bytes
                    }
                    |> BlockCache.put engine.Cache
                    |> ignore

                    bytes)

        readSstableBytes engine f.FileNumber >>= Sstable.openTable

    /// 開いた reader から startKey 以降の全エントリを配列にする。
    /// `[||]` なら先頭から全部。index の二分探索で startKey より前のブロックは読み飛ばす。
    let private readEntriesFrom (startKey: byte array) (reader: SstReader) =
        Sstable.rangeScan reader startKey 0 |> SstScan.toArray

    /// ファイル 1つを読んで startKey 以降の全エントリを配列にする
    /// （読む側。キャッシュ経由）。
    let private entriesOf (engine: StorageEngine) (startKey: byte array) (f: FileMetaData) =
        openSstable engine f >>= readEntriesFrom startKey

    /// ポイントルックアップ。
    /// memtable -> L0 -> 各レベルの順に見る。
    /// visible が Some s のときは SeqNo が s 以下の最新版を返す。
    /// これが MVCC の可視性。
    /// None なら制限なし。
    let getAt (engine: StorageEngine) (visible: uint64 option) (key: byte array) =
        /// 走査の先頭からキーが一致する版をたどり、snapshot で見える最初の（＝最新の）版を拾う。
        /// 同一キーの版は連続して並ぶので、キーを超えたらそこで打ち切る
        /// （ブロックを読み切る前に止まるので、ポイント検索は 1ブロックで済む）。
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

        let fromMem =
            match Memtable.getVisible engine.Mem key visible with
            | None -> ret None
            | Some e -> ret (Some(SstEntry.make e.Key e.SeqNo e.Value))

        match fromMem with
        | Error e -> Error e
        | Ok(Some e) -> ret (Some e)
        | Ok None -> Version.filesForPointLookup engine.Version key |> probe

    /// 範囲ルックアップ。
    /// キー昇順で返す（同一キーは 1 件だけ）。
    /// visible が Some s のときは SeqNo が s 以下の版を返す。
    let scanAt (engine: StorageEngine) (visible: uint64 option) (startKey: byte array) (limit: int) =
        /// 隣接する同じキーのエントリを 1 つに畳む。
        /// merge 後の並びは「キー昇順・同一キーは SeqNo 降順」。
        /// visible が Some s のときは SeqNo が s 以下の最初の版（= その snapshot から見た最新）を 1 件だけ残す。
        /// MVCC の可視性はここで効く。
        /// None なら制限なしで先頭（＝全体最新）を残す。
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
        if entries.Length = 0 then
            ret engine
        else
            let txnId = engine.NextTxnId

            let apply (wal: WalState) =
                let mem = Memtable.apply entries engine.Mem
                let nextTxnId = txnId + 1UL

                let next: StorageEngine =
                    { engine with
                        Wal = wal
                        Mem = mem
                        NextTxnId = nextTxnId
                    }

                ret next

            let records =
                [
                    WalRecord.Begin txnId
                    WalRecord.Batch(txnId, entries)
                    WalRecord.Commit txnId
                ]

            Wal.append engine.Wal engine.Dir records >>= apply

    /// 1 件書く / 消す（put と tombstone の共通。値は entry 側に入っている）。
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
        /// 古い WAL をすべて消してから、新しい番号の空ファイルを作る。
        /// 順序が重要：先に作ると次に消す対象に入ってしまう。
        let rotateWal (engine: StorageEngine) (next: WalState) =
            protect (fun () -> Wal.walFiles engine.Dir |> List.iter System.IO.File.Delete |> ignore)
            >>= fun () ->
                Wal.create engine.Dir next.FileNumber
                >>= fun _ -> ret { engine with Wal = next }

        /// flush の最後段（MANIFEST 反映済み）。
        /// CE の外に切り出しているのは、`result { }` の中では `>>=` が計算式の Bind に取られてしまうため。
        let finishFlush (engine: StorageEngine) (version: Version) (manifestNumber: int64) =
            rotateWal engine (Wal.rotate engine.Wal)
            >>= fun rotated ->
                ret
                    { rotated with
                        Version =
                            { version with
                                NextFileNumber = version.NextFileNumber + 1L
                                SequenceNumber = engine.Version.SequenceNumber + 1UL
                            }
                        ManifestNumber = manifestNumber
                        Mem = Memtable.empty
                    }

        if engine.Mem.Count = 0 then
            ret engine
        else
            let fileNumber = engine.Version.NextFileNumber

            Memtable.toArray engine.Mem
            |> Array.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
            |> FileMetaData.writeSstable engine.Dir fileNumber 0 BloomFalsePositiveRate
            >>= (VersionEdit.addFile
                 >> Manifest.logAndApply engine.Dir engine.ManifestNumber engine.Version)
            >>= fun (version, manifestNumber) -> finishFlush engine version manifestNumber

    /// 1 回の Leveled compaction を行う。
    /// 手順:
    ///   1. 対象レベルExceeded しているファイルを選ぶ
    ///   2. 入力を読んでマージし、GC で不要な版を落とす
    ///   3. 出力をファイルに書き、MANIFEST に記録する（先に MANIFEST）
    ///   4. 入力ファイルを削除する（MANIFEST の後に）
    /// 3 と 4 の順序が重要: MANIFEST を先に書かないと、
    /// クラッシュ時に「MANIFEST は新しいがファイルは古い」= データ損失になる。
    let private compactOnce (engine: StorageEngine) =
        match
            Compaction.compact
                engine.Dir
                engine.Version
                engine.Compaction
                engine.ActiveSnapshot
                engine.Version.NextFileNumber
                (entriesOf engine [||])
        with
        | Error e -> Error e
        | Ok(result, _) ->
            if List.isEmpty result.Added && List.isEmpty result.Removed then
                ret engine
            else
                {
                    VersionEdit.AddedFiles = result.Added
                    DeletedFiles = result.Removed
                }
                |> Manifest.logAndApply engine.Dir engine.ManifestNumber engine.Version
                >>= fun (version, manifestNumber) ->
                    Compaction.deleteObsolete engine.Dir result.Removed
                    |> Result.map (fun () ->
                        let nextNum =
                            match result.Added with
                            | [] -> version.NextFileNumber
                            | files -> files |> List.map (fun f -> f.FileNumber) |> List.max |> (+) 1L

                        let cache =
                            result.Removed
                            |> List.fold (fun c f -> BlockCache.invalidateFile c f.FileNumber) engine.Cache

                        { engine with
                            Version =
                                { version with
                                    NextFileNumber = nextNum
                                }
                            ManifestNumber = manifestNumber
                            Cache = cache
                        })

    /// 必要なら compaction する。
    let maybeCompact (engine: StorageEngine) = compactOnce engine

    /// 最古の active snapshot の SeqNo を進める（GC の基準）。
    let setActiveSnapshot (seqNo: uint64) (engine: StorageEngine) =
        { engine with
            ActiveSnapshot = max engine.ActiveSnapshot seqNo
        }

    /// データベースを開く（新規なら作る）。
    let openDatabase (dir: string) =
        /// MANIFEST が指すファイルのうち、存在しないものを Version から落とす。
        /// （クラッシュ時に flush の MANIFEST だけが書かれ、SSTable が無い状態から復旧する）
        let dropMissingFiles (dir: string) (manifestNumber: int64) (version: Version) =
            let missing =
                Version.allFiles version
                |> List.filter (fun f -> not (System.IO.File.Exists(Sstable.path dir f.FileNumber)))

            if List.isEmpty missing then
                ret (version, manifestNumber)
            else
                {
                    VersionEdit.AddedFiles = []
                    DeletedFiles = missing
                }
                |> Manifest.logAndApply dir manifestNumber version

        /// ディレクトリにある SSTable のファイル番号（孤児削除と open 時の復元で使う）。
        let sstNumbersOnDisk (dir: string) =
            System.IO.Directory.GetFiles(dir, "*.sst") |> Array.choose parseNumber

        /// MANIFEST に載っていない .sst（孤児）を消す。
        let deleteOrphans (dir: string) (version: Version) =
            let referenced =
                Version.allFiles version |> List.map (fun f -> f.FileNumber) |> Set.ofList

            let orphans =
                sstNumbersOnDisk dir |> Array.filter (fun n -> not (Set.contains n referenced))

            protect (fun () ->
                orphans
                |> Array.iter (fun n -> System.IO.File.Delete(Sstable.path dir n))
                |> ignore)

        /// WAL のうち最も新しいファイルの番号。
        let latestWalNumber (dir: string) =
            Wal.walFiles dir |> List.choose parseNumber |> List.sort |> List.tryLast

        /// ファイル 1つを読んで startKey 以降の全エントリを配列にする
        /// （読む側。engine が無い復旧時 = キャッシュなし）。
        let entriesAt (dir: string) (startKey: byte array) (f: FileMetaData) =
            readFileBytes dir f.FileNumber
            >>= Sstable.openTable
            >>= readEntriesFrom startKey

        /// 既存データの中で最大の seqNo を求める。
        /// WAL が空でも新しい seqNo が既存版より小さくならないようにするため。
        /// 全 SSTable の全エントリを走査する（O(全エントリ)）。
        let maxSeqNoOnDisk (dir: string) (version: Version) =
            FileMetaData.foldFiles
                (entriesAt dir [||])
                (fun (acc: uint64) entries -> max acc (SstEntry.maxSeqNo entries))
                0UL
                (Version.allFiles version)

        /// ディレクトリ内の全ファイル番号（SSTable と WAL）。
        let maxFileNumberOnDisk (dir: string) =
            let ssts = sstNumbersOnDisk dir
            let wals = Wal.walFiles dir |> List.choose parseNumber |> List.toArray
            Array.append ssts wals |> Array.fold (fun acc n -> max acc n) 0L

        let build (manifestNumber: int64) (version: Version) (walState: WalState) (nextSeqNo: uint64) =
            { emptyEngine dir with
                ManifestNumber = manifestNumber
                Version = version
                Wal = walState
                NextSeqNo = nextSeqNo
            }

        Manifest.openOrCreate dir
        >>= fun (version, manifestNumber) ->
            dropMissingFiles dir manifestNumber version
            >>= fun (version, manifestNumber) ->
                deleteOrphans dir version *> ret ()
                >>= fun () ->
                    let walNumber =
                        match latestWalNumber dir with
                        | Some n -> n
                        | None -> version.NextFileNumber

                    match System.IO.File.Exists(Wal.path dir walNumber) with
                    | true ->
                        ret
                            {
                                FileNumber = walNumber
                                Size = int (System.IO.FileInfo(Wal.path dir walNumber).Length)
                            }
                    | false -> Wal.create dir walNumber
                    >>= fun walState ->
                        Wal.read dir walNumber
                        >>= fun replayed ->
                            maxSeqNoOnDisk dir version
                            >>= fun diskMax ->
                                let walMax = Entry.maxSeqNo replayed
                                let nextSeqNo = max walMax diskMax + 1UL

                                let bumped =
                                    { version with
                                        NextFileNumber = max version.NextFileNumber (maxFileNumberOnDisk dir + 1L)
                                    }

                                let recovered = build manifestNumber bumped walState nextSeqNo

                                ret
                                    { recovered with
                                        Mem = Memtable.apply replayed Memtable.empty
                                    }

    /// データベースを閉じる（memtable を flush してから WAL を消す）。
    let close (engine: StorageEngine) =
        flush engine
        |> Result.map (fun e ->
            Wal.walFiles e.Dir
            |> List.iter (fun p -> protect (fun () -> System.IO.File.Delete p) |> ignore))
