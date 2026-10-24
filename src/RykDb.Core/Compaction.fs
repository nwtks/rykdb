// ===========================================================================
// Compaction — Leveled compaction と不要バージョンの回収（GC）。
//
// L0 はキー域が重なる複数ファイル、L1 以降は「1ファイル = 1キー域」。
// 1回の compaction: 入力選択 →
// k-way merge →
// gc（同一キーを最新 1件に畳み、どの snapshot からも見えない古い版を捨てる）→
// 出力を L+1 に書き、 MANIFEST に記録してから入力を削除する。
// ===========================================================================

namespace RykDb.Core

/// レベルごとの容量上限（バイト）。
type internal CompactionOptions =
    {
        /// 各レベルの上限。L1 から L6 まで 10 倍ずつ。
        MaxLevelBytes: int64 array
        /// L0 のファイル数がこの数以上になったら L0 → L1 の compaction を行う
        L0CompactionTrigger: int
        /// 1回の compaction で入力するファイル数の上限
        MaxInputFiles: int
        /// 出力 SSTable ファイルの目標バイト数（概算値で近似するため厳密ではない）
        OutputFileTargetBytes: int64
        /// このレベル以下（最下位側）へ降ろす compaction で tombstone を消すか（既定値 = 最下位）
        DropTombstonesBelowLevel: int
        /// Bloom filter の誤検出率
        BloomFalsePositiveRate: float
    }

module internal CompactionOptions =
    /// 既定値。
    /// L0 はファイル数が L0CompactionTrigger に達したら L1 へ compaction する。
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
            L0CompactionTrigger = 4
            MaxInputFiles = 32
            OutputFileTargetBytes = 4L * mib
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
    /// 不要な版を捨てる（GC）。
    /// 入力は `SstEntry.mergeSorted` の出力 = 「キー昇順・同一キーは SeqNo 降順」なので、
    /// 各キーグループの先頭が最新版。
    /// 可視性は `SeqNo <= snapshot`。
    /// したがって最古の active snapshot `S` が読むのは**`SeqNo <= S` のうち最新の 1件**であり、
    /// `S` より小さい版を一律に捨ててはいけない。
    /// 残すのは「最新版」+「`SeqNo > S` の全版」+「`SeqNo <= S` の最新 1件」。
    /// `activeSnapshot = 0` は「全版を残す」。
    let private gc (activeSnapshot: uint64) (dropTombstones: bool) (items: SstEntry array) =
        let rec groupEnd (start: int) =
            if start + 1 >= items.Length then
                items.Length
            elif Bytes.compare items.[start + 1].Key items.[start].Key = 0 then
                groupEnd (start + 1)
            else
                start + 1

        let rec loop
            (newest: SstEntry)
            (finish: int)
            (j: int)
            (kept: SstEntry list)
            (dropped: int)
            (keptVisible: bool)
            =
            if j >= finish then
                List.rev kept, dropped
            else
                let item = items.[j]

                if activeSnapshot = 0UL || item.SeqNo > activeSnapshot then
                    loop newest finish (j + 1) (item :: kept) dropped keptVisible
                elif newest.SeqNo > activeSnapshot && not keptVisible then
                    loop newest finish (j + 1) (item :: kept) dropped true
                else
                    loop newest finish (j + 1) kept (dropped + 1) keptVisible

        let keepOlder (newest: SstEntry) (start: int) (finish: int) = loop newest finish start [] 0 false

        let rec collectKept (acc: SstEntry list) (i: int) (dv: int) (dt: int) =
            if i >= items.Length then
                List.rev acc, dv, dt
            else
                let ge = groupEnd i
                let newest = items.[i]
                let older, dropped = keepOlder newest (i + 1) ge
                let keepNewest = not (newest.IsDelete && dropTombstones && List.isEmpty older)
                let dt' = if keepNewest then dt else dt + 1
                let acc' = if keepNewest then newest :: acc else acc
                let acc'' = List.fold (fun a o -> o :: a) acc' older
                collectKept acc'' ge (dv + dropped) dt'

        let kept, dv, dt = collectKept [] 0 0 0
        kept |> List.toArray, dv, dt

    /// コンパクションの候補。
    type private CompactionPlan =
        {
            /// 入力となるレベル
            Level: int
            /// 出力を置くレベル
            OutputLevel: int
            /// 入力ファイル
            InputFiles: FileMetaData list
            /// 出力時に重なり判定に使うファイル（1つ上のレベル）
            GrandParents: FileMetaData list
        }

    /// コンパクション候補を選ぶ。
    /// L0 はファイル数、L1 以降は総バイト数が閾値を超えた最初のレベルを選び、
    /// そのレベル全体＋1 段下の全ファイルを入力とする。
    /// key 範囲の重なり計算は Compaction 側で行う。
    let private pickCompaction (v: Version) (options: CompactionOptions) =
        let exceeded level =
            if level = 0 then
                let count = v.Files.[0].Length
                count > 0 && count >= options.L0CompactionTrigger
            else
                v.Files.[level] |> Array.sumBy _.Size > options.MaxLevelBytes.[level]

        let rec find level =
            if level > Levels.Max then None
            elif exceeded level then Some level
            else find (level + 1)

        match find 0 with
        | None -> None
        | Some level ->
            let grandParents =
                if level + 1 <= Levels.Max then
                    v.Files.[level + 1] |> Array.toList
                else
                    []

            Some
                {
                    Level = level
                    OutputLevel = min (level + 1) Levels.Max
                    InputFiles = v.Files.[level] |> Array.toList
                    GrandParents = grandParents
                }

    /// 1回の Leveled compaction を行う。
    /// readFile でファイルを読み、activeSnapshot を基準に GC してから書き出す。
    /// MANIFEST の更新と実ファイルの削除は呼び出し側が行う。
    let compact
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (nextFileNumber: int64)
        (readFile: FileMetaData -> Result<SstEntry array, DbError>)
        =
        let overlapsWith (files: FileMetaData list) (f: FileMetaData) =
            files
            |> List.exists (fun g ->
                Bytes.compare g.Largest f.Smallest >= 0
                && Bytes.compare f.Largest g.Smallest >= 0)

        let selectInputs (version: Version) (level: int) (maxFiles: int) =
            let inputs = Version.filesAt version level |> Array.toList |> List.truncate maxFiles
            let nextLevel = level + 1

            let next =
                if nextLevel > Levels.Max then
                    []
                else
                    Version.filesAt version nextLevel
                    |> Array.toList
                    |> List.filter (overlapsWith inputs)
                    |> List.truncate maxFiles

            inputs, next, nextLevel

        let readAll (rest: FileMetaData list) =
            rest
            |> foldOk (fun acc f -> readFile f |> Result.map (fun items -> items :: acc)) []
            |> Result.map List.rev

        let writeChunks (rest: SstEntry array list) (outputLevel: int) =
            rest
            |> List.mapi (fun i chunk -> nextFileNumber + int64 i, chunk)
            |> foldOk
                (fun acc (num, chunk) ->
                    FileMetaData.writeSstable dir num outputLevel options.BloomFalsePositiveRate chunk
                    |> Result.map (fun meta -> meta :: acc))
                []

        let finish (sources: SstEntry array list) (outputLevel: int) =
            let merged = SstEntry.mergeSorted sources
            let dropTombstones = outputLevel >= options.DropTombstonesBelowLevel

            let cleaned, droppedVersions, droppedTombstones =
                gc activeSnapshot dropTombstones merged

            let info =
                { FileMetaData.zero with
                    Level = outputLevel
                }

            if cleaned.Length = 0 then
                ret (
                    {
                        Added = []
                        Removed = []
                        DroppedVersions = droppedVersions
                        DroppedTombstones = droppedTombstones
                    },
                    info
                )
            else
                let chunks = SstEntry.splitIntoBytes options.OutputFileTargetBytes cleaned

                result {
                    let! added = writeChunks chunks outputLevel

                    return
                        {
                            Added = added
                            Removed = []
                            DroppedVersions = droppedVersions
                            DroppedTombstones = droppedTombstones
                        },
                        info
                }

        let compactPlan (plan: CompactionPlan) =
            let inputs, nextFiles, _ = selectInputs version plan.Level options.MaxInputFiles
            let allInputs = inputs @ nextFiles

            result {
                let! sources = readAll allInputs
                let! result, info = finish sources plan.OutputLevel
                let final = { result with Removed = allInputs }
                return final, info
            }

        match pickCompaction version options with
        | Some plan -> compactPlan plan
        | None ->
            ret (
                {
                    Added = []
                    Removed = []
                    DroppedVersions = 0
                    DroppedTombstones = 0
                },
                { FileMetaData.zero with
                    FileNumber = -1L
                }
            )
