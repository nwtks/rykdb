// ===========================================================================
// Compaction — Leveled compaction と不要バージョンの回収（GC）。
//
// L0 はキー域が重なる複数ファイル、L1 以降は「1ファイル = 1キー域」。
// 1回の compaction: 入力選択 →
// 入力をブロック単位でストリーミング merge（`SstMerge`）→
// gc（同一キーを最新 1件に畳み、どの snapshot からも見えない古い版を捨てる）→
// 出力を L+1 に書き、 MANIFEST に記録してから入力を削除する。
// 入力全体はメモリに載せない（ファイルごとの現在ブロック + 出力ファイル 1本分だけ）。
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
    /// 1キー分のエントリを GC する。
    /// <remarks>
    /// 入力は「キー昇順・同一キーは SeqNo 降順」なので、グループの先頭が最新版。
    /// 可視性は `SeqNo <= snapshot`。
    /// したがって最古の active snapshot `S` が読むのは**`SeqNo <= S` のうち最新の 1件**であり、
    /// `S` より小さい版を一律に捨ててはいけない。
    /// 残すのは「最新版」+「`SeqNo > S` の全版」+「`SeqNo <= S` の最新 1件」。
    /// `activeSnapshot = 0` は「全版を残す」。
    /// 戻り値は (残すエントリ, 捨てた版数, 捨てた tombstone 数)。
    /// </remarks>
    let private gcGroup (activeSnapshot: uint64) (dropTombstones: bool) (newest: SstEntry) (older: SstEntry list) =
        let rec keepOlder rest kept dropped keptVisible =
            match rest with
            | [] -> List.rev kept, dropped
            | item :: tail ->
                if activeSnapshot = 0UL || item.SeqNo > activeSnapshot then
                    keepOlder tail (item :: kept) dropped keptVisible
                elif newest.SeqNo > activeSnapshot && not keptVisible then
                    keepOlder tail (item :: kept) dropped true
                else
                    keepOlder tail kept (dropped + 1) keptVisible

        let keptOlder, dropped = keepOlder older [] 0 false
        let keepNewest = not (newest.IsDelete && dropTombstones && List.isEmpty keptOlder)
        let kept = if keepNewest then newest :: keptOlder else keptOlder
        kept, dropped, (if keepNewest then 0 else 1)

    /// マージ済みストリームから 1キー分を読み、GC した結果を返す。
    /// 空なら None。
    /// 戻り値は (残すエントリ, 捨てた版数, 捨てた tombstone 数, 続き)。
    let private nextGroup (activeSnapshot: uint64) (dropTombstones: bool) (merge: SstMerge) =
        let rec gather newest m acc =
            match SstMerge.top m with
            | Some e when Bytes.compare e.Key newest.Key = 0 ->
                match SstMerge.next m with
                | Error e -> Error e
                | Ok None -> Ok(m, List.rev acc)
                | Ok(Some(nxt, rest)) -> gather newest rest (nxt :: acc)
            | _ -> Ok(m, List.rev acc)

        match SstMerge.next merge with
        | Error e -> Error e
        | Ok None -> Ok None
        | Ok(Some(newest, afterNewest)) ->
            match gather newest afterNewest [] with
            | Error e -> Error e
            | Ok(rest, older) ->
                let kept, dropped, tombstones = gcGroup activeSnapshot dropTombstones newest older
                Ok(Some(kept, dropped, tombstones, rest))

    /// コンパクションの候補。
    type private CompactionPlan =
        {
            /// 入力となるレベル
            Level: int
            /// 出力を置くレベル
            OutputLevel: int
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
            Some
                {
                    Level = level
                    OutputLevel = min (level + 1) Levels.Max
                }

    /// 1回の Leveled compaction を行う（ストリーミング）。
    /// <remarks>
    /// 入力は全読みしない。各ファイルを `SstScan` で開き、`SstMerge` で 1件ずつマージしながら
    /// キー単位で GC し、出力ファイル 1本分（`OutputFileTargetBytes` 相当）だけをメモリに持つ。
    /// 出力ファイルはキーの切れ目で切り替えるので、1つのキーの版が複数ファイルに跨らない。
    /// `openScan` はファイルの先頭から読むスキャンを返す（部分読みリーダーを使えばブロック単位で読む）。
    /// MANIFEST の更新と実ファイルの削除は呼び出し側が行う。
    /// </remarks>
    let compact
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (nextFileNumber: int64)
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        =
        let overlapsWith files f =
            files
            |> List.exists (fun g ->
                Bytes.compare g.Largest f.Smallest >= 0
                && Bytes.compare f.Largest g.Smallest >= 0)

        let selectInputs version level maxFiles =
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

        let rec openAll files scans =
            match files with
            | [] -> Ok(List.rev scans |> List.toArray)
            | f :: rest ->
                match openScan f with
                | Error e -> Error e
                | Ok scan -> openAll rest (scan :: scans)

        let flushChunk plan chunk fileNumber =
            match chunk with
            | [] -> ret None
            | _ ->
                let entries = chunk |> List.rev |> List.toArray

                FileMetaData.writeSstable dir fileNumber plan.OutputLevel options.BloomFalsePositiveRate entries
                |> Result.map Some

        let finish plan allInputs added droppedVersions droppedTombstones =
            {
                Added = List.rev added
                Removed = allInputs
                DroppedVersions = droppedVersions
                DroppedTombstones = droppedTombstones
            },
            { FileMetaData.zero with
                Level = plan.OutputLevel
            }

        let rec pump
            plan
            allInputs
            dropTombstones
            merge
            chunk
            chunkBytes
            fileNumber
            added
            droppedVersions
            droppedTombstones
            =
            match nextGroup activeSnapshot dropTombstones merge with
            | Error e -> Error e
            | Ok None ->
                flushChunk plan chunk fileNumber
                |> Result.map (fun last ->
                    let added =
                        match last with
                        | Some meta -> meta :: added
                        | None -> added

                    finish plan allInputs added droppedVersions droppedTombstones)
            | Ok(Some(kept, dropped, tombstones, rest)) ->
                let keptBytes = kept |> List.sumBy (fun e -> int64 (SstEntry.approximateBytes e))
                let chunk' = List.rev kept @ chunk

                if
                    not (List.isEmpty chunk)
                    && chunkBytes + keptBytes > options.OutputFileTargetBytes
                then
                    match flushChunk plan chunk fileNumber with
                    | Error e -> Error e
                    | Ok meta ->
                        let added =
                            match meta with
                            | Some m -> m :: added
                            | None -> added

                        pump
                            plan
                            allInputs
                            dropTombstones
                            rest
                            (List.rev kept)
                            keptBytes
                            (fileNumber + 1L)
                            added
                            (droppedVersions + dropped)
                            (droppedTombstones + tombstones)
                else
                    pump
                        plan
                        allInputs
                        dropTombstones
                        rest
                        chunk'
                        (chunkBytes + keptBytes)
                        fileNumber
                        added
                        (droppedVersions + dropped)
                        (droppedTombstones + tombstones)

        let compactPlan plan =
            let inputs, nextFiles, _ = selectInputs version plan.Level options.MaxInputFiles
            let allInputs = inputs @ nextFiles
            let dropTombstones = plan.OutputLevel >= options.DropTombstonesBelowLevel

            result {
                let! scans = openAll allInputs []
                let! merge = SstMerge.ofScans scans
                return! pump plan allInputs dropTombstones merge [] 0L nextFileNumber [] 0 0
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
