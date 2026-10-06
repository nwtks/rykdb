// ===========================================================================
// StorageEngine — LSM ツリーを 1 本公開する層（keyspace 1 本 = ここが 1 本）。
//
// 探索は「memtable -> L0（新しい順）-> 各レベル（新しい順）」の順。
// 各ステップで Bloom filter と VersionSet の key range で枝刈りする。
// 可視性（SeqNo <= snapshot）は getAt / scanAt の visible フィルタで判定する。
// 制約: scan は結果を全部メモリに載せて返す（architecture.md §15.2）。
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
    // ファイル名
    // -----------------------------------------------------------------------

    let private sstFileName (fileNumber: int64) = sprintf "%06d.sst" fileNumber

    let private sstPath (dir: string) (fileNumber: int64) =
        Path.Combine(dir, sstFileName fileNumber)

    let private parseSstNumber (path: string) =
        match System.Int64.TryParse(Path.GetFileNameWithoutExtension path) with
        | true, v -> Some v
        | _ -> None

    // -----------------------------------------------------------------------
    // ソート済み列のマージ
    // -----------------------------------------------------------------------

    type private Head =
        {
            FileNumber: int64
            Items: SstEntry array
            Index: int
        }

    /// 「キー昇順・同一キーは SeqNo 降順」で並んだ列を k-way merge する。
    /// ソース数は高々 1 + ファイル数なので、head を走査しても十分速い。
    let private mergeSources (sources: (int64 * SstEntry array) list) =
        let rec go (heads: Head list) (acc: SstEntry list) =
            let pick =
                heads
                |> List.filter (fun h -> h.Index < h.Items.Length)
                |> List.fold
                    (fun best h ->
                        match best with
                        | None -> Some h
                        | Some b ->
                            if SstEntry.compare h.Items.[h.Index] b.Items.[b.Index] < 0 then
                                Some h
                            else
                                Some b)
                    None

            match pick with
            | None -> List.rev acc |> Array.ofList
            | Some h ->
                let e = h.Items.[h.Index]
                let advanced = { h with Index = h.Index + 1 }

                let replaced =
                    heads |> List.map (fun x -> if x.FileNumber = h.FileNumber then advanced else x)

                go replaced (e :: acc)

        go
            (sources
             |> List.map (fun (fid, arr) ->
                 {
                     FileNumber = fid
                     Items = arr
                     Index = 0
                 }))
            []

    /// 隣接する同じキーのエントリを 1 つに畳む。
    /// merge 後の並びは「キー昇順・同一キーは SeqNo 降順」。
    /// <paramref name="visible"/> が Some s のときは SeqNo が s 以下の最初の版
    /// （= その snapshot から見た最新）を 1 件だけ残す。MVCC の可視性はここで効く。
    /// None なら制限なしで先頭（＝全体最新）を残す。
    let private collapseDuplicates (visible: uint64 option) (items: SstEntry array) =
        let visibleTo (e: SstEntry) =
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true

        let rec go (acc: SstEntry list) (arr: SstEntry array) (i: int) (emittedKey: byte array option) =
            if i >= arr.Length then
                List.rev acc |> Array.ofList
            else
                let e = arr.[i]

                match emittedKey with
                | Some k when SstEntry.compareBytes k e.Key = 0 -> go acc arr (i + 1) emittedKey
                | _ when not (visibleTo e) -> go acc arr (i + 1) emittedKey
                | _ -> go (e :: acc) arr (i + 1) (Some e.Key)

        go [] items 0 None

    // -----------------------------------------------------------------------
    // ファイル読み
    // -------------------------------------------------------------------

    /// SSTable のファイル単位のバイト列を（キャッシュ経由で）読む。
    let private readSstableBytes (engine: StorageEngine) (fileNumber: int64) =
        match BlockCache.get engine.Cache fileNumber 0 with
        | Some(_, bytes) -> ret bytes
        | None ->
            protect (fun () -> File.ReadAllBytes(sstPath engine.Dir fileNumber))
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

    // -----------------------------------------------------------------------
    // 検索
    // -----------------------------------------------------------------------

    /// バージョン列から snapshot で見える最初の（＝最新の）版を拾う。
    let private firstVisible (visible: uint64 option) (key: byte array) (seq: seq<SstEntry>) =
        seq
        |> Seq.takeWhile (fun e -> SstEntry.compareBytes e.Key key = 0)
        |> Seq.tryFind (fun e ->
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true)

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
            |> List.filter (fun f -> SstEntry.compareBytes f.Largest startKey >= 0)
            |> List.sortByDescending (fun f -> f.FileNumber)

        let rec readAll (files: FileMetaData list) (acc: (int64 * SstEntry array) list) =
            match files with
            | [] -> ret (List.rev acc)
            | f :: rest ->
                match openSstable engine f with
                | Error e -> Error e
                | Ok reader ->
                    match Sstable.entries reader with
                    | Error e -> Error e
                    | Ok seq -> readAll rest ((f.FileNumber, seq |> Seq.toArray) :: acc)

        readAll files []
        |> Result.map (fun sources ->
            let merged = mergeSources ((0L, memSeq) :: sources) |> collapseDuplicates visible

            let sliced =
                if Array.isEmpty startKey then
                    merged
                else
                    merged |> Array.filter (fun e -> SstEntry.compareBytes e.Key startKey >= 0)

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

    /// 1 件書く。
    let put (engine: StorageEngine) (key: byte array) (value: SqlValue array) =
        let entry = Entry.put key engine.NextSeqNo value
        let nextSeqNo = engine.NextSeqNo + 1UL

        let bump (e: StorageEngine) =
            let next = { e with NextSeqNo = nextSeqNo }
            ret next

        writeBatch engine [| entry |] |> Result.bind bump

    /// 1 件削除する（tombstone を書く）。
    let delete (engine: StorageEngine) (key: byte array) =
        let entry = Entry.delete key engine.NextSeqNo
        let nextSeqNo = engine.NextSeqNo + 1UL

        let bump (e: StorageEngine) =
            let next = { e with NextSeqNo = nextSeqNo }
            ret next

        writeBatch engine [| entry |] |> Result.bind bump

    // -----------------------------------------------------------------------
    // flush
    // -----------------------------------------------------------------------

    /// WAL のうち最も新しいファイルの番号。
    let private latestWalNumber (dir: string) =
        Wal.walFiles dir
        |> List.choose (fun p ->
            match System.Int64.TryParse(Path.GetFileNameWithoutExtension p) with
            | true, v -> Some v
            | _ -> None)
        |> List.sort
        |> List.tryLast

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

            let writeFile (bytes: byte array) =
                protect (fun () -> File.WriteAllBytes(sstPath engine.Dir fileNumber, bytes) |> ignore)

            let applyManifest (bytes: byte array) =
                let maxSeqNo = sst |> Array.fold (fun acc e -> max acc e.SeqNo) 0UL

                let meta =
                    {
                        FileNumber = fileNumber
                        Level = 0
                        Smallest = sst.[0].Key
                        Largest = sst.[sst.Length - 1].Key
                        EntryCount = int64 sst.Length
                        Size = int64 bytes.Length
                        MaxSeqNo = maxSeqNo
                    }

                match
                    Manifest.logAndApply engine.Dir engine.ManifestNumber engine.Version (VersionEdit.addFile meta)
                with
                | Error e -> Error e
                | Ok(version, manifestNumber) -> finishFlush engine version manifestNumber

            match Sstable.build sst BloomFalsePositiveRate with
            | Error e -> Error e
            | Ok bytes ->
                match writeFile bytes with
                | Error e -> Error e
                | Ok() -> applyManifest bytes

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
        let readFile (f: FileMetaData) =
            openSstable engine f
            |> Result.bind (fun reader -> Sstable.entries reader |> Result.map (fun seq -> seq |> Seq.toArray))

        match
            Compaction.compact
                engine.Dir
                engine.Version
                engine.Compaction
                engine.ActiveSnapshot
                engine.Version.NextFileNumber
                readFile
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
            |> List.filter (fun f -> not (File.Exists(sstPath dir f.FileNumber)))

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

        let onDisk = Directory.GetFiles(dir, "*.sst") |> Array.choose parseSstNumber
        let orphans = onDisk |> Array.filter (fun n -> not (Set.contains n referenced))

        protect (fun () -> orphans |> Array.iter (fun n -> File.Delete(sstPath dir n)) |> ignore)

    /// 既存データの中で最大の seqNo を求める。
    /// WAL が空でも新しい seqNo が既存版より小さくならないようにするため。
    /// 全 SSTable の全エントリを走査する（O(全エントリ)）。
    let private maxSeqNoOnDisk (dir: string) (version: RykDb.Core.Version) =
        let rec go (files: FileMetaData list) (acc: uint64) =
            match files with
            | [] -> ret acc
            | f :: rest ->
                match protect (fun () -> File.ReadAllBytes(sstPath dir f.FileNumber)) with
                | Error e -> Error e
                | Ok bytes ->
                    match Sstable.openTable bytes with
                    | Error e -> Error e
                    | Ok reader ->
                        match Sstable.entries reader with
                        | Error e -> Error e
                        | Ok seq ->
                            let m = seq |> Seq.fold (fun acc e -> max acc e.SeqNo) acc
                            go rest m

        go (Version.allFiles version) 0UL

    /// ディレクトリ内の全ファイル番号（SSTable と WAL）。
    let private maxFileNumberOnDisk (dir: string) =
        let ssts = Directory.GetFiles(dir, "*.sst") |> Array.choose parseSstNumber

        let wals =
            Wal.walFiles dir
            |> List.choose (fun p ->
                match System.Int64.TryParse(Path.GetFileNameWithoutExtension p) with
                | true, v -> Some v
                | _ -> None)

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
                                let walMax = replayed |> Array.fold (fun acc e -> max acc e.SeqNo) 0UL
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
