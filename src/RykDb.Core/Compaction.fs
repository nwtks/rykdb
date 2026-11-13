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
        /// 1回の compaction で seed とするファイル数の目安（重なり閉包により超えうる）
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
    let Defaults =
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
    /// コンパクションの候補。
    type private CompactionPlan =
        {
            /// 入力となるレベル
            Level: int
            /// 出力を置くレベル
            OutputLevel: int
        }

    /// 同一キーの版グループを GC する（純粋関数）。
    /// `older` は `newest` より古い版（SeqNo 降順）。
    /// 戻り値は (残す版, 捨てた版数, 捨てた tombstone 数)。
    /// `activeSnapshot = 0` は「制限なし」を表す。
    let internal gcGroup (activeSnapshot: uint64) (dropTombstones: bool) (older: SstEntry list) (newest: SstEntry) =
        let rec keepOlder kept dropped keptVisible rest =
            match rest with
            | [] -> List.rev kept, dropped
            | item :: tail ->
                if activeSnapshot = 0UL || item.SeqNo > activeSnapshot then
                    keepOlder (item :: kept) dropped keptVisible tail
                elif newest.SeqNo > activeSnapshot && not keptVisible then
                    keepOlder (item :: kept) dropped true tail
                else
                    keepOlder kept (dropped + 1) keptVisible tail

        match older with
        | [] ->
            let keepNewest = not (newest.IsDelete && dropTombstones)
            (if keepNewest then [ newest ] else []), 0, (if keepNewest then 0 else 1)
        | _ when activeSnapshot = 0UL -> newest :: older, 0, 0
        | _ ->
            let keptOlder, dropped = keepOlder [] 0 false older
            let keepNewest = not (newest.IsDelete && dropTombstones && List.isEmpty keptOlder)
            let kept = if keepNewest then newest :: keptOlder else keptOlder
            kept, dropped, (if keepNewest then 0 else 1)

    /// 1回の Leveled compaction を行う（ストリーミング）。
    /// <remarks>
    /// 入力は全読みしない。各ファイルを `SstScan` で開き、`SstMerge` で 1件ずつマージしながら
    /// キー単位で GC し、出力ファイル 1本分（`OutputFileTargetBytes` 相当）だけをメモリに持つ。
    /// 出力ファイルはキーの切れ目で切り替えるので、1つのキーの版が複数ファイルに跨らない。
    /// `openScan` はファイルの先頭から読むスキャンを返す（部分読みリーダーを使えばブロック単位で読む）。
    /// MANIFEST の更新と実ファイルの削除は呼び出し側が行う。
    /// </remarks>
    let compactWith
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        (allocateFileNumber: unit -> int64)
        (shouldStop: unit -> bool)
        =
        let emptyResult =
            {
                Added = []
                Removed = []
                DroppedVersions = 0
                DroppedTombstones = 0
            },
            { FileMetaData.Zero with
                FileNumber = -1L
            }

        let deleteOutputs (files: FileMetaData list) =
            FileMetaData.deleteObsolete dir files |> ignore

        let exceeded v level =
            if level = 0 then
                let count = v.Files.[0].Length
                count > 0 && count >= options.L0CompactionTrigger
            else
                v.Files.[level] |> Array.sumBy _.Size > options.MaxLevelBytes.[level]

        let rec find v level =
            if level > Levels.Max then None
            elif exceeded v level then Some level
            else find v (level + 1)

        let pickCompaction v =
            match find v 0 with
            | None -> None
            | Some level ->
                Some
                    {
                        Level = level
                        OutputLevel = min (level + 1) Levels.Max
                    }

        let minKeyOf files =
            files
            |> Array.map (fun f -> f.Smallest)
            |> Array.reduce (fun a b -> if Bytes.compare a b <= 0 then a else b)

        let maxKeyOf files =
            files
            |> Array.map (fun f -> f.Largest)
            |> Array.reduce (fun a b -> if Bytes.compare a b >= 0 then a else b)

        let inRange minKey maxKey f =
            Bytes.compare f.Largest minKey >= 0 && Bytes.compare f.Smallest maxKey <= 0

        let rec expand current next minBound maxBound =
            let inputs = current |> Array.filter (inRange minBound maxBound)
            let nextFiles = next |> Array.filter (inRange minBound maxBound)
            let found = Array.append inputs nextFiles
            let minKey' = minKeyOf found
            let maxKey' = maxKeyOf found

            if Bytes.compare minKey' minBound = 0 && Bytes.compare maxKey' maxBound = 0 then
                inputs |> Array.toList, nextFiles |> Array.toList
            else
                expand current next minKey' maxKey'

        let selectInputs version level maxFiles =
            let current = Version.filesAt level version
            let seed = current |> Array.toList |> List.truncate maxFiles

            match seed with
            | [] -> [], []
            | _ ->
                let nextLevel = level + 1

                let next =
                    if nextLevel > Levels.Max then
                        [||]
                    else
                        Version.filesAt nextLevel version

                let seed' = Array.ofList seed
                expand current next (minKeyOf seed') (maxKeyOf seed')

        let rec openAll files scans =
            match files with
            | [] -> ret (List.rev scans |> List.toArray)
            | f :: rest ->
                match openScan f with
                | Error e ->
                    scans |> List.iter SstScan.dispose
                    err e
                | Ok scan -> openAll rest (scan :: scans)

        let openMerge scans =
            match SstMerge.ofScans scans with
            | Ok merge -> ret merge
            | Error e ->
                scans |> Array.iter SstScan.dispose
                err e

        let rec gather newest m acc =
            match SstMerge.top m with
            | Some e when Bytes.compare e.Key newest.Key = 0 ->
                match SstMerge.next m with
                | Error e -> err e
                | Ok None -> ret (m, List.rev acc)
                | Ok(Some(nxt, rest)) -> gather newest rest (nxt :: acc)
            | _ -> ret (m, List.rev acc)

        let nextGroup dropTombstones merge =
            match SstMerge.next merge with
            | Error e -> err e
            | Ok None -> ret None
            | Ok(Some(newest, afterNewest)) ->
                gather newest afterNewest []
                >>= fun (rest, older) ->
                    let kept, dropped, tombstones = gcGroup activeSnapshot dropTombstones older newest
                    ret (Some(kept, dropped, tombstones, rest))

        let flushChunk plan chunk fileNumber =
            match chunk with
            | [] -> ret None
            | _ ->
                FileMetaData.writeSstable
                    dir
                    fileNumber
                    plan.OutputLevel
                    options.BloomFalsePositiveRate
                    (List.rev chunk)
                |> Result.map Some

        let finish plan allInputs added droppedVersions droppedTombstones =
            {
                Added = List.rev added
                Removed = allInputs
                DroppedVersions = droppedVersions
                DroppedTombstones = droppedTombstones
            },
            { FileMetaData.Zero with
                Level = plan.OutputLevel
            }

        let rec pump plan allInputs dropTombstones merge chunk chunkBytes added droppedVersions droppedTombstones =
            if shouldStop () then
                deleteOutputs added
                ret emptyResult
            else
                match nextGroup dropTombstones merge with
                | Error e ->
                    deleteOutputs added
                    err e
                | Ok None ->
                    match flushChunk plan chunk (allocateFileNumber ()) with
                    | Error e ->
                        deleteOutputs added
                        err e
                    | Ok last ->
                        let added =
                            match last with
                            | Some meta -> meta :: added
                            | None -> added

                        ret (finish plan allInputs added droppedVersions droppedTombstones)
                | Ok(Some(kept, dropped, tombstones, rest)) ->
                    let keptBytes = kept |> List.sumBy (fun e -> int64 (SstEntry.approximateBytes e))
                    let chunk' = List.fold (fun acc item -> item :: acc) chunk kept

                    if
                        not (List.isEmpty chunk)
                        && chunkBytes + keptBytes > options.OutputFileTargetBytes
                    then
                        match flushChunk plan chunk (allocateFileNumber ()) with
                        | Error e ->
                            deleteOutputs added
                            err e
                        | Ok meta ->
                            let added' =
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
                                added'
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
                            added
                            (droppedVersions + dropped)
                            (droppedTombstones + tombstones)

        let runPump plan allInputs merge =
            let dropTombstones = plan.OutputLevel >= options.DropTombstonesBelowLevel

            match pump plan allInputs dropTombstones merge [] 0L [] 0 0 with
            | Ok result -> ret result
            | Error e ->
                SstMerge.dispose merge
                err e

        let compactPlan plan =
            let inputs, nextFiles = selectInputs version plan.Level options.MaxInputFiles
            let allInputs = inputs @ nextFiles

            result {
                let! scans = openAll allInputs []
                let! merge = openMerge scans
                return! runPump plan allInputs merge
            }

        match pickCompaction version with
        | Some plan -> compactPlan plan
        | None -> ret emptyResult

    /// 同期 compaction。
    /// ファイル番号は [nextFileNumber] から 1 ずつ採番する。
    let compact
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (nextFileNumber: int64)
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        =
        let numberKey = "n"
        let counter = System.Collections.Concurrent.ConcurrentDictionary<string, int64>()
        counter.[numberKey] <- nextFileNumber

        let rec allocate () =
            let current = counter.[numberKey]
            let next = current + 1L

            if counter.TryUpdate(numberKey, next, current) then
                current
            else
                allocate ()

        compactWith dir version options activeSnapshot openScan allocate (fun () -> false)
