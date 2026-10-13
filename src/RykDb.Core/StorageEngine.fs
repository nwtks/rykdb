// ===========================================================================
// StorageEngine — LSM ツリーを 1本公開する層（keyspace 1本 = ここが 1本）。
//
// 探索は「memtable -> L0（新しい順）-> 各レベル（新しい順）」の順。
// 各ステップで Bloom filter と VersionSet の key range で枝刈りする。
// 可視性（SeqNo <= snapshot）は getAt / scanAt の visible フィルタで判定する。
// 制約: scan は結果を全部メモリに載せて返す。
// ===========================================================================

namespace RykDb.Core

open System.IO

/// StorageEngine の不変状態。すべての操作は新しい状態を返す。
type internal StorageEngine =
    {
        /// データベースのディレクトリ
        Dir: string
        /// 有効な manifest 番号
        ManifestNumber: int64
        /// 現在の LSM 形状
        Version: RykDb.Core.Version
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
        /// 最古の active snapshot の SeqNo。これより小さい版は誰も読まないので GC できる
        ActiveSnapshot: uint64
    }

    /// memtable が上限に達しているか。
    member x.NeedsFlush = x.Mem.Count > 0 && x.Mem.ApproxBytes >= x.MemLimitBytes

/// 統計（テストと診断用）。
type internal StorageStats =
    {
        /// memtable 内のエントリ数
        Entries: int
        /// memtable の概算バイト数
        MemBytes: int
        /// WAL のバイト数
        WalBytes: int
        /// SSTable ファイル数
        FileCount: int
        /// レベルごとのファイル数
        Levels: int array
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

    // -----------------------------------------------------------------------
    // ファイル名（Sstable.fileName / Sstable.path を共有）
    // -----------------------------------------------------------------------

    /// ファイル名（拡張子除く）を番号として読む。SSTable でも WAL でも同じ規則。
    let private parseNumber (path: string) =
        match System.Int64.TryParse(Path.GetFileNameWithoutExtension path) with
        | true, v -> Some v
        | _ -> None

    /// snapshot（visible）から見える版かどうか。None なら常に見える。
    let private isVisible (visible: uint64 option) (e: SstEntry) =
        match visible with
        | Some s -> e.SeqNo <= s
        | None -> true

    /// 隣接する同じキーのエントリを 1 つに畳む。
    /// merge 後の並びは「キー昇順・同一キーは SeqNo 降順」。
    /// <paramref name="visible"/> が Some s のときは SeqNo が s 以下の最初の版
    /// （= その snapshot から見た最新）を 1 件だけ残す。MVCC の可視性はここで効く。
    /// None なら制限なしで先頭（＝全体最新）を残す。
    let private collapseDuplicates (visible: uint64 option) (items: SstEntry array) =
        let rec go (acc: SstEntry list) (arr: SstEntry array) (i: int) (emittedKey: byte array option) =
            if i >= arr.Length then
                List.rev acc |> Array.ofList
            else
                let e = arr.[i]

                match emittedKey with
                | Some k when Bytes.compare k e.Key = 0 -> go acc arr (i + 1) emittedKey
                | _ when not (isVisible visible e) -> go acc arr (i + 1) emittedKey
                | _ -> go (e :: acc) arr (i + 1) (Some e.Key)

        go [] items 0 None

    // -----------------------------------------------------------------------
    // ファイル読み
    // -------------------------------------------------------------------

    /// SSTable ファイルを直接バイト列で読む（キャッシュを通さない）。
    let private readFileBytes (dir: string) (fileNumber: int64) =
        protect (fun () -> File.ReadAllBytes(Sstable.path dir fileNumber))

    /// SSTable のファイル単位のバイト列を（キャッシュ経由で）読む。
    let private readSstableBytes (engine: StorageEngine) (fileNumber: int64) =
        match BlockCache.get engine.Cache fileNumber 0 with
        | Some(_, bytes) -> ret bytes
        | None ->
            readFileBytes engine.Dir fileNumber
            |> Result.map (fun bytes ->
                let entry =
                    {
                        FileNumber = fileNumber
                        BlockId = 0
                        Data = bytes
                    }

                BlockCache.put engine.Cache entry |> ignore
                bytes)

    /// ファイルを開く（Bloom と index を持つ reader を作る）。
    let private openSstable (engine: StorageEngine) (f: FileMetaData) =
        readSstableBytes engine f.FileNumber |> Result.bind Sstable.openTable

    /// 開いた reader から全エントリを配列にする。
    let private readAllEntries reader =
        Sstable.entries reader |> Result.map Seq.toArray

    /// ファイル 1つを読んで全エントリを配列にする（読む側。キャッシュ経由）。
    let private entriesOf (engine: StorageEngine) (f: FileMetaData) =
        openSstable engine f |> Result.bind readAllEntries

    /// ファイル 1つを読んで全エントリを配列にする
    /// （読む側。engine が無い復旧時 = キャッシュなし）。
    let private entriesAt (dir: string) (f: FileMetaData) =
        readFileBytes dir f.FileNumber >>= Sstable.openTable >>= readAllEntries

    /// ファイル列を先頭から順に処理する「回す側」。
    /// read が 1ファイル分を読み、step が状態を進める。
    /// 最初の Error で打ち切る。
    /// read / step は再帰しないので bind で合成してよい。
    let private foldFiles
        (read: FileMetaData -> Result<'a, DbError>)
        (step: 's -> 'a -> 's)
        (state: 's)
        (files: FileMetaData list)
        =
        let rec go (rest: FileMetaData list) (acc: 's) =
            match rest with
            | [] -> ret acc
            | f :: tail ->
                match read f with
                | Error e -> Error e
                | Ok v -> go tail (step acc v)

        go files state

    /// ディレクトリにある SSTable のファイル番号（孤児削除と open 時の復元で使う）。
    let private sstNumbersOnDisk (dir: string) =
        Directory.GetFiles(dir, "*.sst") |> Array.choose parseNumber

    // -----------------------------------------------------------------------
    // 検索
    // -----------------------------------------------------------------------

    /// バージョン列から snapshot で見える最初の（＝最新の）版を拾う。
    let private firstVisible (visible: uint64 option) (key: byte array) (seq: seq<SstEntry>) =
        seq
        |> Seq.takeWhile (fun e -> Bytes.compare e.Key key = 0)
        |> Seq.tryFind (isVisible visible)

    /// 点ルックアップ。memtable -> L0 -> 各レベルの順に見る。
    /// <paramref name="visible"/> が Some s のときは SeqNo が s 以下の最新版を返す。
    /// これが MVCC の可視性。None なら制限なし。
    let getAt (engine: StorageEngine) (visible: uint64 option) (key: byte array) =
        let fromMem =
            match Memtable.getVisible engine.Mem key visible with
            | None -> ret None
            | Some e -> ret (Some(SstEntry.make e.Key e.SeqNo e.Value))

        match fromMem with
        | Error e -> Error e
        | Ok(Some e) -> ret (Some e)
        | Ok None ->
            let files = Version.filesForPointLookup engine.Version key

            let rec go (files: FileMetaData list) =
                match files with
                | [] -> ret None
                | f :: rest ->
                    match openSstable engine f with
                    | Error e -> Error e
                    | Ok reader ->
                        if not (Sstable.mayContain reader key) then
                            go rest
                        else
                            match Sstable.rangeScan reader key 0 with
                            | Error e -> Error e
                            | Ok seq ->
                                match firstVisible visible key seq with
                                | Some e -> ret (Some e)
                                | None -> go rest

            go files

    /// 点ルックアップ（可視性の制限なし）。
    let get (engine: StorageEngine) (key: byte array) = getAt engine None key

    /// 範囲ルックアップ。キー昇順で返す（同一キーは 1 件だけ）。
    /// <paramref name="visible"/> が Some s のときは SeqNo が s 以下の版を返す。
    let scanAt (engine: StorageEngine) (visible: uint64 option) (startKey: byte array) (limit: int) =
        let memSeq =
            Memtable.rangeScan engine.Mem startKey 0
            |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
            |> Seq.toArray

        let files =
            Version.allFiles engine.Version
            |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
            |> List.sortByDescending (fun f -> f.FileNumber)

        foldFiles (entriesOf engine) (fun acc entries -> entries :: acc) [] files
        |> Result.map (fun sources ->
            let ordered = List.rev sources
            let merged = SstEntry.mergeSorted (memSeq :: ordered) |> collapseDuplicates visible

            let sliced =
                if Array.isEmpty startKey then
                    merged
                else
                    merged |> Array.filter (fun e -> Bytes.compare e.Key startKey >= 0)

            if limit > 0 then sliced |> Array.truncate limit else sliced)

    /// 範囲ルックアップ（可視性の制限なし）。
    let scan (engine: StorageEngine) (startKey: byte array) (limit: int) = scanAt engine None startKey limit

    /// 書き込みバッチ。まず WAL（Begin/Batch/Commit を 1 回の書き込みで）へ、
    /// それから memtable へ反映する。
    let writeBatch (engine: StorageEngine) (entries: Entry array) =
        if entries.Length = 0 then
            ret engine
        else
            let txnId = engine.NextTxnId

            let records =
                [
                    WalRecord.Begin txnId
                    WalRecord.Batch(txnId, entries)
                    WalRecord.Commit txnId
                ]

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

            Wal.append engine.Wal engine.Dir records |> Result.bind apply

    /// 1 件書く / 消す（put と tombstone の共通。値は entry 側に入っている）。
    let private writeOne (engine: StorageEngine) (entry: Entry) =
        let nextSeqNo = engine.NextSeqNo + 1UL

        writeBatch engine [| entry |]
        |> Result.map (fun next -> { next with NextSeqNo = nextSeqNo })

    /// 1 件書く。
    let put (engine: StorageEngine) (key: byte array) (value: SqlValue array) =
        writeOne engine (Entry.put key engine.NextSeqNo value)

    /// 1 件削除する（tombstone を書く）。
    let delete (engine: StorageEngine) (key: byte array) =
        writeOne engine (Entry.delete key engine.NextSeqNo)

    // -----------------------------------------------------------------------
    // flush
    // -----------------------------------------------------------------------

    /// WAL のうち最も新しいファイルの番号。
    let private latestWalNumber (dir: string) =
        Wal.walFiles dir |> List.choose parseNumber |> List.sort |> List.tryLast

    /// 古い WAL をすべて消してから、新しい番号の空ファイルを作る。
    /// 順序が重要：先に作ると次に消す対象に入ってしまう。
    let private rotateWal (engine: StorageEngine) (next: WalState) =
        protect (fun () -> Wal.walFiles engine.Dir |> List.iter (fun p -> File.Delete p) |> ignore)
        >>= fun () ->
            Wal.create engine.Dir next.FileNumber
            >>= fun _ ->
                let updated: StorageEngine = { engine with Wal = next }

                ret updated

    /// flush の最後段（MANIFEST 反映済み）。CE の外に切り出しているのは、
    /// `result { }` の中では `>>=` が計算式の Bind に取られてしまうため。
    let private finishFlush (engine: StorageEngine) (version: RykDb.Core.Version) (manifestNumber: int64) =
        rotateWal engine (Wal.rotate engine.Wal)
        >>= fun rotated ->
            let next: StorageEngine =
                { rotated with
                    Version =
                        { version with
                            NextFileNumber = version.NextFileNumber + 1L
                            SequenceNumber = engine.Version.SequenceNumber + 1UL
                        }
                    ManifestNumber = manifestNumber
                    Mem = Memtable.empty
                }

            ret next

    /// memtable を L0 の SSTable にして書き出す。
    /// memtable が空なら何もしない（WAL も切り替えない）。
    let flush (engine: StorageEngine) =
        if engine.Mem.Count = 0 then
            ret engine
        else
            let entries = Memtable.toArray engine.Mem
            let sst = entries |> Array.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
            let fileNumber = engine.Version.NextFileNumber

            FileMetaData.writeSstable engine.Dir fileNumber 0 BloomFalsePositiveRate sst
            |> Result.bind (fun meta ->
                Manifest.logAndApply engine.Dir engine.ManifestNumber engine.Version (VersionEdit.addFile meta)
                |> Result.bind (fun (version, manifestNumber) -> finishFlush engine version manifestNumber))

    // -----------------------------------------------------------------------
    // Compaction
    // -----------------------------------------------------------------------

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
                (entriesOf engine)
        with
        | Error e -> Error e
        | Ok(result, _info) ->
            if List.isEmpty result.Added && List.isEmpty result.Removed then
                ret engine
            else
                let edit =
                    {
                        VersionEdit.AddedFiles = result.Added
                        DeletedFiles = result.Removed
                    }

                Manifest.logAndApply engine.Dir engine.ManifestNumber engine.Version edit
                |> Result.bind (fun (version, manifestNumber) ->
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
                        }))

    /// 必要なら compaction する。
    let maybeCompact (engine: StorageEngine) = compactOnce engine

    /// 最古の active snapshot の SeqNo を進める（GC の基準）。
    let setActiveSnapshot (seqNo: uint64) (engine: StorageEngine) =
        let next =
            { engine with
                ActiveSnapshot = max engine.ActiveSnapshot seqNo
            }

        next

    // -----------------------------------------------------------------------
    // open / close（復旧）
    // -----------------------------------------------------------------------

    /// MANIFEST が指すファイルのうち、存在しないものを Version から落とす。
    /// （クラッシュ時に flush の MANIFEST だけが書かれ、SSTable が無い状態から復旧する）
    let private dropMissingFiles (dir: string) (manifestNumber: int64) (version: RykDb.Core.Version) =
        let missing =
            Version.allFiles version
            |> List.filter (fun f -> not (File.Exists(Sstable.path dir f.FileNumber)))

        if List.isEmpty missing then
            ret (version, manifestNumber)
        else
            let edit =
                {
                    VersionEdit.AddedFiles = []
                    DeletedFiles = missing
                }

            Manifest.logAndApply dir manifestNumber version edit

    /// MANIFEST に載っていない .sst（孤児）を消す。
    let private deleteOrphans (dir: string) (version: RykDb.Core.Version) =
        let referenced =
            Version.allFiles version |> List.map (fun f -> f.FileNumber) |> Set.ofList

        let onDisk = sstNumbersOnDisk dir
        let orphans = onDisk |> Array.filter (fun n -> not (Set.contains n referenced))

        protect (fun () -> orphans |> Array.iter (fun n -> File.Delete(Sstable.path dir n)) |> ignore)

    /// 既存データの中で最大の seqNo を求める。
    /// WAL が空でも新しい seqNo が既存版より小さくならないようにするため。
    /// 全 SSTable の全エントリを走査する（O(全エントリ)）。
    let private maxSeqNoOnDisk (dir: string) (version: RykDb.Core.Version) =
        foldFiles
            (entriesAt dir)
            (fun (acc: uint64) entries -> max acc (SstEntry.maxSeqNo entries))
            0UL
            (Version.allFiles version)

    /// ディレクトリ内の全ファイル番号（SSTable と WAL）。
    let private maxFileNumberOnDisk (dir: string) =
        let ssts = sstNumbersOnDisk dir

        let wals = Wal.walFiles dir |> List.choose parseNumber

        Array.append ssts (wals |> List.toArray)
        |> Array.fold (fun acc n -> max acc n) 0L

    /// データベースを開く（新規なら作る）。
    let openDatabase (dir: string) =
        let build (manifestNumber: int64) (version: RykDb.Core.Version) (walState: WalState) (nextSeqNo: uint64) =
            let blank = emptyEngine dir

            { blank with
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

                    match File.Exists(Wal.path dir walNumber) with
                    | true ->
                        ret
                            {
                                FileNumber = walNumber
                                Size = int (FileInfo(Wal.path dir walNumber).Length)
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
            |> List.iter (fun p -> protect (fun () -> File.Delete p) |> ignore))

    /// 統計（テストと診断用）。
    let stats (engine: StorageEngine) =
        {
            StorageStats.Entries = engine.Mem.Count
            MemBytes = engine.Mem.ApproxBytes
            WalBytes = engine.Wal.Size
            FileCount = engine.Version |> Version.allFiles |> List.length
            Levels = engine.Version |> Version.levelCounts
        }
