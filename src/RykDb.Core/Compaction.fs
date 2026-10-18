// ===========================================================================
// Compaction — Leveled compaction と不要バージョンの回収（GC）。
//
// L0 はキー域が重なる複数ファイル、L1 以降は「1ファイル = 1キー域」。
// 1回の compaction: 入力選択 →
// k-way merge →
// gc（同一キーを最新 1件に畳み、全 active snapshot より小さい版を捨てる）→
// 出力を L+1 に書き、 MANIFEST に記録してから入力を削除する。
// ===========================================================================

namespace RykDb.Core

/// レベルごとの容量上限（バイト）。
type internal CompactionOptions =
    {
        /// 各レベルの上限。L1 から L6 まで 10 倍ずつ。
        MaxLevelBytes: int64 array
        /// 1回の compaction で入力するファイル数の上限
        MaxInputFiles: int
        /// このレベル以下（最下位側）へ降ろす compaction で tombstone を消すか（既定値 = 最下位）
        DropTombstonesBelowLevel: int
        /// Bloom filter の誤検出率
        BloomFalsePositiveRate: float
    }

module internal CompactionOptions =
    /// 既定値。
    /// L0 は閾値を持たない（flush の出力先なので無制限）。
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
    /// 不要な版を捨てる（GC）。
    /// 入力は `SstEntry.mergeSorted` の出力 = 「キー昇順・同一キーは SeqNo 降順」なので、
    /// 各キーグループの先頭が最新版。
    /// グループの 2件目以降は active snapshot から見える版だけ残す。
    let private gc (activeSnapshot: uint64) (dropTombstones: bool) (items: SstEntry array) =
        let rec groupEnd (start: int) =
            if start + 1 >= items.Length then
                items.Length
            elif Bytes.compare items.[start + 1].Key items.[start].Key = 0 then
                groupEnd (start + 1)
            else
                start + 1

        let rec collectKept (acc: SstEntry list) (i: int) (dv: int) (dt: int) =
            if i >= items.Length then
                List.rev acc, dv, dt
            else
                let ge = groupEnd i
                let count = ge - i
                let e = items.[i]

                let older =
                    [
                        for j in i + 1 .. ge - 1 do
                            if items.[j].SeqNo >= activeSnapshot then
                                yield items.[j]
                    ]

                let droppedVersions = count - 1 - older.Length
                let keepNewest = if e.IsDelete && dropTombstones then false else true
                let dv' = dv + droppedVersions
                let dt' = dt + if e.IsDelete then droppedVersions else 0
                let acc', dt'' = if not keepNewest then acc, dt' + 1 else e :: acc, dt'
                let acc''' = List.fold (fun a o -> o :: a) acc' older
                collectKept acc''' ge dv' dt''

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
    /// 1段目のレベルから閾値を超えたら、そのレベル全体＋1 段下の全ファイルを入力とする。
    /// key 範囲の重なり計算は Compaction 側で行う。
    let private pickCompaction (v: Version) (maxLevelSize: int64 array) =
        let rec find level =
            if level > Levels.Max then
                None
            else
                let total = v.Files.[level] |> Array.sumBy _.Size

                if total > maxLevelSize.[level] then
                    Some level
                else
                    find (level + 1)

        match find 1 with
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
            let inputs = Version.filesAt version level |> Array.toList
            let nextLevel = level + 1

            let next =
                if nextLevel > Levels.Max then
                    []
                else
                    Version.filesAt version nextLevel
                    |> Array.toList
                    |> List.filter (overlapsWith inputs)
                    |> List.truncate maxFiles

            inputs |> List.truncate maxFiles, next, nextLevel

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
                let chunks = SstEntry.splitInto (options.MaxInputFiles * 4) cleaned

                match writeChunks chunks outputLevel with
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

        let compactPlan (plan: CompactionPlan) =
            let inputs, nextFiles, _ = selectInputs version plan.Level options.MaxInputFiles
            let allInputs = inputs @ nextFiles

            match readAll allInputs with
            | Error e -> Error e
            | Ok sources ->
                match finish sources plan.OutputLevel with
                | Error e -> Error e
                | Ok(result, info) ->
                    let final = { result with Removed = allInputs }
                    ret (final, info)

        match pickCompaction version options.MaxLevelBytes with
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
