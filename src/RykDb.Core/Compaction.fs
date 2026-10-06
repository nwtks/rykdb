// ===========================================================================
// Compaction — Leveled compaction と不要バージョンの回収（GC）。
//
// L0 はキー域が重なる複数ファイル、L1 以降は「1 ファイル = 1 キー域」。
// 1 回の compaction: 入力選択 → k-way merge → gc（同一キーを最新 1 件に畳み、
// 全 active snapshot より小さい版を捨てる）→ 出力を L+1 に書き、
// MANIFEST に記録してから入力を削除する。
// 詳細は docs/architecture.md §9.7、制約（入力をメモリへ全読み・同期実行）は
// §15.2 を参照。
// ===========================================================================

namespace RykDb.Core

open System.IO

/// レベルごとの容量上限（バイト）。
type internal CompactionOptions =
    {
        /// 各レベルの上限。L1 から L6 まで 10 倍ずつ。
        MaxLevelBytes: int64 array
        /// 1 回の compaction で入力するファイル数の上限
        MaxInputFiles: int
        /// このレベル以下（最下位側）へ降ろす compaction で tombstone を消すか（既定値 = 最下位）
        DropTombstonesBelowLevel: int
        /// Bloom filter の誤検出率
        BloomFalsePositiveRate: float
    }

module internal CompactionOptions =
    /// 既定値。L0 は閾値を持たない（flush の出力先なので無制限）。
    let defaults =
        let mib = 1024L * 1024L

        let limits =
            [|
                0L
                10L * mib
                100L * mib
                1024L * mib
                10240L * mib
                102400L * mib
                1048576L * mib
            |]

        {
            MaxLevelBytes = limits
            MaxInputFiles = 32
            DropTombstonesBelowLevel = Levels.Max
            BloomFalsePositiveRate = 0.01
        }

/// compaction 1 回の結果。
type internal CompactionResult =
    {
        /// 新しく作ったファイル
        Added: FileMetaData list
        /// 入力として使った（= もう不要になった）ファイル
        Removed: FileMetaData list
        /// GC で捨てたエントリ数
        DroppedVersions: int
        /// GC で捨てた tombstone 数
        DroppedTombstones: int
    }

module internal Compaction =
    let private fileName (fileNumber: int64) = sprintf "%06d.sst" fileNumber

    let private path (dir: string) (fileNumber: int64) = Path.Combine(dir, fileName fileNumber)

    let private deleteFile (dir: string) (fileNumber: int64) =
        protect (fun () ->
            let p = path dir fileNumber

            if File.Exists p then
                File.Delete p)

    // -----------------------------------------------------------------------
    // ソート済み列のマージ
    // StorageEngine.scan と同じ k-way merge。各ソースは既に
    // 「キー昇順・同一キーは SeqNo 降順」で並んでいる。
    // -----------------------------------------------------------------------

    type private Head =
        {
            Id: int
            Items: SstEntry array
            Index: int
        }

    let private splitInto (n: int) (items: SstEntry array) =
        let rec go acc start =
            if start >= items.Length then
                List.rev acc
            else
                let len = max 1 (min n (items.Length - start))
                go (Array.sub items start len :: acc) (start + len)

        go [] 0

    let private mergeSources (sources: SstEntry array list) =
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
                let replaced = heads |> List.map (fun x -> if x.Id = h.Id then advanced else x)
                go replaced (e :: acc)

        go (sources |> List.mapi (fun i a -> { Id = i; Items = a; Index = 0 })) []

    // -----------------------------------------------------------------------
    // GC — 不要バージョンの削除
    // -----------------------------------------------------------------------

    let private gc (activeSnapshot: uint64) (dropTombstones: bool) (items: SstEntry array) =
        let rec groupEnd (start: int) =
            if start + 1 >= items.Length then
                items.Length
            elif SstEntry.compareBytes items.[start + 1].Key items.[start].Key = 0 then
                groupEnd (start + 1)
            else
                start + 1

        let rec pickMaxIdx (i: int) (best: int) (ge: int) =
            if i >= ge then
                best
            elif items.[i].SeqNo > items.[best].SeqNo then
                pickMaxIdx (i + 1) i ge
            else
                pickMaxIdx (i + 1) best ge

        let pickMaxFrom (gs: int) (ge: int) = pickMaxIdx (gs + 1) gs ge

        let rec go (acc: SstEntry list) (i: int) (dv: int) (dt: int) =
            if i >= items.Length then
                List.rev acc, dv, dt
            else
                let ge = groupEnd i
                let count = ge - i
                let best = pickMaxFrom i ge
                let e = items.[best]

                let older =
                    [
                        for j in i .. ge - 1 do
                            if j <> best && items.[j].SeqNo >= activeSnapshot then
                                yield items.[j]
                    ]

                let droppedVersions = count - 1 - older.Length
                let keepNewest = if e.IsDelete && dropTombstones then false else true
                let dv' = dv + droppedVersions
                let dt' = dt + if e.IsDelete then droppedVersions else 0
                let acc', dt'' = if not keepNewest then acc, dt' + 1 else e :: acc, dt'
                let acc''' = List.fold (fun a o -> o :: a) acc' older
                go acc''' ge dv' dt''

        let kept, dv, dt = go [] 0 0 0
        kept |> List.toArray, dv, dt

    // -----------------------------------------------------------------------
    // コンパクション
    // -----------------------------------------------------------------------

    let private selectInputs (version: RykDb.Core.Version) (level: int) (maxFiles: int) =
        let inputs = Version.filesAt version level |> Array.toList
        let nextLevel = level + 1

        let overlapsWith (files: FileMetaData list) (f: FileMetaData) =
            files
            |> List.exists (fun g ->
                SstEntry.compareBytes g.Largest f.Smallest >= 0
                && SstEntry.compareBytes f.Largest g.Smallest >= 0)

        let next =
            if nextLevel > Levels.Max then
                []
            else
                Version.filesAt version nextLevel
                |> Array.toList
                |> List.filter (overlapsWith inputs)
                |> List.truncate maxFiles

        inputs |> List.truncate maxFiles, next, nextLevel

    /// 1 回の Leveled compaction を行う。
    /// <paramref name="readFile"/> でファイルを読み、
    /// <paramref name="activeSnapshot"/> を基準に GC してから書き出す。
    /// MANIFEST の更新と実ファイルの削除は呼び出し側（StorageEngine）が行う。
    let compact
        (dir: string)
        (version: RykDb.Core.Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (nextFileNumber: int64)
        (readFile: FileMetaData -> Result<SstEntry array, DbError>)
        =
        let makeMeta
            (chunk: SstEntry array)
            (bytes: byte array)
            (fileNumber: int64)
            (outputLevel: int)
            (maxSeqNo: uint64)
            =
            {
                FileNumber = fileNumber
                Level = outputLevel
                Smallest = chunk.[0].Key
                Largest = chunk.[chunk.Length - 1].Key
                EntryCount = int64 chunk.Length
                Size = int64 bytes.Length
                MaxSeqNo = maxSeqNo
            }

        let writeOne (chunk: SstEntry array) (fileNumber: int64) (outputLevel: int) (maxSeqNo: uint64) =
            match Sstable.build chunk options.BloomFalsePositiveRate with
            | Error e -> Error e
            | Ok bytes ->
                match protect (fun () -> File.WriteAllBytes(path dir fileNumber, bytes) |> ignore) with
                | Error e -> Error e
                | Ok() -> ret (makeMeta chunk bytes fileNumber outputLevel maxSeqNo)

        let rec writeChunks
            (rest: SstEntry array list)
            (num: int64)
            (outputLevel: int)
            (maxSeqNo: uint64)
            (acc: FileMetaData list)
            =
            match rest with
            | [] -> ret acc
            | chunk :: tail ->
                let fileNumber = nextFileNumber + num

                match writeOne chunk fileNumber outputLevel maxSeqNo with
                | Error e -> Error e
                | Ok meta -> writeChunks tail (num + 1L) outputLevel maxSeqNo (meta :: acc)

        let rec readAll (rest: FileMetaData list) (acc: SstEntry array list) =
            match rest with
            | [] -> ret (List.rev acc)
            | f :: tail ->
                match readFile f with
                | Error e -> Error e
                | Ok items -> readAll tail (items :: acc)

        let finish (sources: SstEntry array list) (outputLevel: int) =
            let merged = mergeSources sources
            let dropTombstones = outputLevel >= options.DropTombstonesBelowLevel

            let cleaned, droppedVersions, droppedTombstones =
                gc activeSnapshot dropTombstones merged

            let info =
                {
                    Level = outputLevel
                    FileNumber = 0L
                    EntryCount = 0L
                    Size = 0L
                    Smallest = [||]
                    Largest = [||]
                    MaxSeqNo = 0UL
                }

            if cleaned.Length = 0 then
                let summary =
                    {
                        Added = []
                        Removed = []
                        DroppedVersions = droppedVersions
                        DroppedTombstones = droppedTombstones
                    }

                ret (summary, info)
            else
                let maxSeqNo = cleaned |> Array.fold (fun acc e -> max acc e.SeqNo) 0UL
                let chunks = splitInto (max 1 (options.MaxInputFiles * 4)) cleaned

                match writeChunks chunks 0L outputLevel maxSeqNo [] with
                | Error e -> Error e
                | Ok added ->
                    ret (
                        {
                            Added = added
                            Removed = []
                            DroppedVersions = droppedVersions
                            DroppedTombstones = droppedTombstones
                        },
                        info
                    )

        match Version.pickCompaction version options.MaxLevelBytes with
        | None ->
            ret (
                {
                    Added = []
                    Removed = []
                    DroppedVersions = 0
                    DroppedTombstones = 0
                },
                {
                    FileNumber = -1L
                    Level = 0
                    Smallest = [||]
                    Largest = [||]
                    EntryCount = 0L
                    Size = 0L
                    MaxSeqNo = 0UL
                }
            )
        | Some plan ->
            let inputs, nextFiles, _ = selectInputs version plan.Level options.MaxInputFiles
            let allInputs = inputs @ nextFiles

            match readAll allInputs [] with
            | Error e -> Error e
            | Ok sources ->
                match finish sources plan.OutputLevel with
                | Error e -> Error e
                | Ok(result, info) ->
                    let final = { result with Removed = allInputs }
                    ret (final, info)

    /// ファイルを実際に削除する。
    let deleteObsolete (dir: string) (files: FileMetaData list) =
        files
        |> List.fold (fun acc f -> acc >>= fun () -> deleteFile dir f.FileNumber) (ret ())
