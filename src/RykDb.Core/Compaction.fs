// ===========================================================================
// Compaction — Leveled compaction と不要バージョンの回収（GC）。
//
// L0 はキー域が重なる複数ファイル、L1 以降は「1ファイル = 1キー域」。
// 1回の compaction: 入力選択 →
// 入力をブロック単位でストリーミング merge（`SstMerge`）→
// gc（同一キーは active snapshot より新しい版と、その snapshot から見える最新 1版を残し、それより古い版を捨てる）→
// 出力を次レベル（最下位 L6 では同じ L6）に書き、MANIFEST に記録してから入力を削除する。
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
        /// <remarks>
        /// 最下位以外を指定すると、上位レベルに残る旧版を確認せずに tombstone を落としてデータが復活し得る
        /// （LevelDB の IsBaseLevelForKey 相当の検査がないため）。
        /// 変更する場合は入力選択に上位レベルの重なり検査を追加すること。
        /// </remarks>
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

/// compaction 1回の結果。
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
    /// 指定レベルのファイル数・サイズが compaction の閾値を超えたか。
    let private levelExceeded (options: CompactionOptions) (v: Version) (level: int) =
        if level = 0 then
            let count = v.Files.[0].Length
            count > 0 && count >= options.L0CompactionTrigger
        else
            v.Files.[level] |> Array.sumBy _.Size > options.MaxLevelBytes.[level]

    /// 閾値を超えた最初のレベルを探す。
    [<TailCall>]
    let rec private findCompactionLevel (options: CompactionOptions) (v: Version) (level: int) =
        if level > Levels.Max then None
        elif levelExceeded options v level then Some level
        else findCompactionLevel options v (level + 1)

    /// コンパクションの候補。
    type private CompactionPlan =
        {
            /// 入力となるレベル
            Level: int
            /// 出力を置くレベル
            OutputLevel: int
        }

    /// 次に compaction するレベルを選ぶ。
    let private pickCompaction (options: CompactionOptions) (v: Version) =
        match findCompactionLevel options v 0 with
        | None -> None
        | Some level ->
            Some
                {
                    Level = level
                    OutputLevel = min (level + 1) Levels.Max
                }

    /// 何も追加・削除しなかったときの結果。
    let private EmptyCompactResult =
        {
            Added = []
            Removed = []
            DroppedVersions = 0
            DroppedTombstones = 0
        },
        { FileMetaData.Zero with
            FileNumber = -1L
        }

    /// 作成済みの出力ファイルを削除する。
    let private deleteCompactOutputs (dir: string) (files: FileMetaData list) =
        FileMetaData.deleteObsolete dir files |> ignore

    /// 古い版を残すか捨てるかを判定しながら走査する。
    [<TailCall>]
    let rec private keepOlder
        (activeSnapshot: uint64)
        (newest: SstEntry)
        (kept: SstEntry list)
        (dropped: int)
        (keptVisible: bool)
        =
        function
        | [] -> List.rev kept, dropped
        | item :: tail ->
            if activeSnapshot = 0UL || item.SeqNo > activeSnapshot then
                keepOlder activeSnapshot newest (item :: kept) dropped keptVisible tail
            elif newest.SeqNo > activeSnapshot && not keptVisible then
                keepOlder activeSnapshot newest (item :: kept) dropped true tail
            else
                keepOlder activeSnapshot newest kept (dropped + 1) keptVisible tail

    /// 同一キーの版グループを GC する（純粋関数）。
    /// `older` は `newest` より古い版（SeqNo 降順）。
    /// 戻り値は (残す版, 捨てた版数, 捨てた tombstone 数)。
    /// `activeSnapshot = 0` は「制限なし」を表す。
    let internal gcGroup (activeSnapshot: uint64) (dropTombstones: bool) (older: SstEntry list) (newest: SstEntry) =
        match older with
        | [] ->
            let keepNewest = not (newest.IsDelete && dropTombstones)
            (if keepNewest then [ newest ] else []), 0, (if keepNewest then 0 else 1)
        | _ when activeSnapshot = 0UL -> newest :: older, 0, 0
        | _ ->
            let keptOlder, dropped = keepOlder activeSnapshot newest [] 0 false older
            let keepNewest = not (newest.IsDelete && dropTombstones && List.isEmpty keptOlder)
            let kept = if keepNewest then newest :: keptOlder else keptOlder
            kept, dropped, (if keepNewest then 0 else 1)

    type private GatherResult =
        | GatherStopped
        | Gathered of SstMerge * SstEntry list

    /// 同じキーの版を集める（停止要求時は中断）。
    [<TailCall>]
    let rec private gatherGroup (shouldStop: unit -> bool) (newest: SstEntry) (m: SstMerge) (acc: SstEntry list) =
        if shouldStop () then
            ret GatherStopped
        else
            match SstMerge.top m with
            | Some e when Bytes.compare e.Key newest.Key = 0 ->
                if shouldStop () then
                    ret GatherStopped
                else
                    match SstMerge.next m with
                    | Ok None -> ret (Gathered(m, List.rev acc))
                    | Ok(Some(nxt, rest)) -> gatherGroup shouldStop newest rest (nxt :: acc)
                    | Error e -> err e
            | _ -> ret (Gathered(m, List.rev acc))

    type private NextGroup =
        | NextGroupDone
        | NextGroupStopped
        | NextGroup of SstEntry list * int * int * SstMerge

    /// GC を適用した次のグループを取り出す。
    let private nextGroup (activeSnapshot: uint64) (shouldStop: unit -> bool) (dropTombstones: bool) (merge: SstMerge) =
        if shouldStop () then
            ret NextGroupStopped
        else
            match SstMerge.next merge with
            | Ok None -> ret NextGroupDone
            | Ok(Some(newest, afterNewest)) ->
                match gatherGroup shouldStop newest afterNewest [] with
                | Ok GatherStopped -> ret NextGroupStopped
                | Ok(Gathered(rest, older)) ->
                    let kept, dropped, tombstones = gcGroup activeSnapshot dropTombstones older newest
                    ret (NextGroup(kept, dropped, tombstones, rest))
                | Error e -> err e
            | Error e -> err e

    /// 出力ファイル番号の供給方法。
    /// 同期 compaction は決まった開始番号から純粋に進め、
    /// バックグラウンド compaction はホストが持つ allocator から取得する。
    type private FileNumberSupply =
        | Sequence of FileNumber
        | Allocator of (unit -> FileNumber)

    /// ファイル番号を採番して供給状態を進める。
    let private takeFileNumber (supply: FileNumberSupply) =
        match supply with
        | Sequence current -> current, Sequence(current + 1L)
        | Allocator allocate -> allocate (), supply

    /// 溜めたチャンクを 1ファイルとして書き出す。
    let private flushChunk
        (dir: string)
        (options: CompactionOptions)
        (plan: CompactionPlan)
        (fileNumber: FileNumber)
        (chunk: SstEntry list)
        =
        match chunk with
        | [] -> ret None
        | _ ->
            FileMetaData.writeSstable
                dir
                fileNumber
                plan.OutputLevel
                options.BloomFalsePositiveRate
                (List.length chunk)
                (List.rev chunk)
            |> Result.map Some

    /// compaction の結果レコードを組み立てる。
    let private finish
        (plan: CompactionPlan)
        (allInputs: FileMetaData list)
        (added: FileMetaData list)
        (droppedVersions: int)
        (droppedTombstones: int)
        =
        {
            Added = List.rev added
            Removed = allInputs
            DroppedVersions = droppedVersions
            DroppedTombstones = droppedTombstones
        },
        { FileMetaData.Zero with
            Level = plan.OutputLevel
        }

    /// グループを出力ファイルへ書き出す（末尾再帰）。
    [<TailCall>]
    let rec private pump
        (dir: string)
        (options: CompactionOptions)
        (plan: CompactionPlan)
        (activeSnapshot: uint64)
        (supply: FileNumberSupply)
        (shouldStop: unit -> bool)
        (allInputs: FileMetaData list)
        (dropTombstones: bool)
        (merge: SstMerge)
        (chunk: SstEntry list)
        (chunkBytes: int64)
        (added: FileMetaData list)
        (droppedVersions: int)
        (droppedTombstones: int)
        =
        if shouldStop () then
            deleteCompactOutputs dir added
            ret EmptyCompactResult
        else
            match nextGroup activeSnapshot shouldStop dropTombstones merge with
            | Error e ->
                deleteCompactOutputs dir added
                err e
            | Ok NextGroupStopped ->
                deleteCompactOutputs dir added
                ret EmptyCompactResult
            | Ok NextGroupDone ->
                let fileNumber, _ = takeFileNumber supply

                match flushChunk dir options plan fileNumber chunk with
                | Error e ->
                    deleteCompactOutputs dir added
                    err e
                | Ok last ->
                    let added =
                        match last with
                        | Some meta -> meta :: added
                        | None -> added

                    ret (finish plan allInputs added droppedVersions droppedTombstones)
            | Ok(NextGroup(kept, dropped, tombstones, rest)) ->
                let keptBytes = kept |> List.sumBy (fun e -> int64 (SstEntry.approximateBytes e))
                let chunk' = List.fold (fun acc item -> item :: acc) chunk kept

                if
                    not (List.isEmpty chunk)
                    && chunkBytes + keptBytes > options.OutputFileTargetBytes
                then
                    let fileNumber, supply = takeFileNumber supply

                    match flushChunk dir options plan fileNumber chunk with
                    | Error e ->
                        deleteCompactOutputs dir added
                        err e
                    | Ok meta ->
                        let added' =
                            match meta with
                            | Some m -> m :: added
                            | None -> added

                        pump
                            dir
                            options
                            plan
                            activeSnapshot
                            supply
                            shouldStop
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
                        dir
                        options
                        plan
                        activeSnapshot
                        supply
                        shouldStop
                        allInputs
                        dropTombstones
                        rest
                        chunk'
                        (chunkBytes + keptBytes)
                        added
                        (droppedVersions + dropped)
                        (droppedTombstones + tombstones)

    /// 出力を書き切って merge を解放する。
    let private runPump
        (dir: string)
        (options: CompactionOptions)
        (plan: CompactionPlan)
        (activeSnapshot: uint64)
        (supply: FileNumberSupply)
        (shouldStop: unit -> bool)
        (allInputs: FileMetaData list)
        (merge: SstMerge)
        =
        let dropTombstones = plan.OutputLevel >= options.DropTombstonesBelowLevel

        try
            pump dir options plan activeSnapshot supply shouldStop allInputs dropTombstones merge [] 0L [] 0 0
        finally
            SstMerge.dispose merge

    /// ファイルがキー範囲と重なるか。
    let private inRange (minKey: byte array) (maxKey: byte array) (f: FileMetaData) =
        Bytes.compare f.Largest minKey >= 0 && Bytes.compare f.Smallest maxKey <= 0

    /// ファイルのキー範囲を現在の境界へ広げる。
    let private widen (mn: byte array, mx: byte array) (f: FileMetaData) =
        let mn' = if Bytes.compare f.Smallest mn <= 0 then f.Smallest else mn
        let mx' = if Bytes.compare f.Largest mx >= 0 then f.Largest else mx
        mn', mx'

    /// ファイル列のキー範囲を求める（空なら CorruptionError）。
    let private boundsOf (files: FileMetaData array) =
        match Array.tryHead files with
        | None -> err (DbError.corruption "compaction の入力ファイルが空です（MANIFEST のキー範囲が不正）")
        | Some first -> ret (files |> Array.fold widen (first.Smallest, first.Largest))

    /// 境界が動かなくなるまで入力ファイルを広げる。
    [<TailCall>]
    let rec private expand
        (current: FileMetaData array)
        (next: FileMetaData array)
        (minBound: byte array)
        (maxBound: byte array)
        =
        let inputs = current |> Array.filter (inRange minBound maxBound)
        let nextFiles = next |> Array.filter (inRange minBound maxBound)
        let found = Array.append inputs nextFiles

        match boundsOf found with
        | Ok(minKey', maxKey') ->
            if Bytes.compare minKey' minBound = 0 && Bytes.compare maxKey' maxBound = 0 then
                ret (inputs |> Array.toList, nextFiles |> Array.toList)
            else
                expand current next minKey' maxKey'
        | Error e -> err e

    /// 指定レベルの compaction 入力を選ぶ。
    let private selectInputs (version: Version) (level: int) (maxFiles: int) =
        let current = Version.filesAt level version
        let seed = current |> Array.toList |> List.truncate maxFiles

        match seed with
        | [] -> ret ([], [])
        | _ ->
            let nextLevel = level + 1

            let next =
                if nextLevel > Levels.Max then
                    [||]
                else
                    Version.filesAt nextLevel version

            Array.ofList seed
            |> boundsOf
            >>= fun (minBound, maxBound) -> expand current next minBound maxBound

    /// 入力ファイルを走査として開く（停止要求時は閉じて None）。
    [<TailCall>]
    let rec private openInputs
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        (shouldStop: unit -> bool)
        (scans: SstScan list)
        (files: FileMetaData list)
        =
        if shouldStop () then
            scans |> List.iter SstScan.dispose
            ret None
        else
            match files with
            | [] -> ret (Some(List.rev scans |> List.toArray))
            | f :: rest ->
                match openScan f with
                | Error e ->
                    scans |> List.iter SstScan.dispose
                    err e
                | Ok scan -> openInputs openScan shouldStop (scan :: scans) rest

    /// 開いた走査からマージを組み立てる（失敗時は全走査を閉じる）。
    let private openMerge (scans: SstScan array) =
        match SstMerge.ofScans scans with
        | Ok merge -> ret merge
        | Error e ->
            scans |> Array.iter SstScan.dispose
            err e

    /// 選択したレベルの compaction を実行する。
    let private compactPlan
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (plan: CompactionPlan)
        (activeSnapshot: uint64)
        (supply: FileNumberSupply)
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        (shouldStop: unit -> bool)
        =
        result {
            let! inputs, nextFiles = selectInputs version plan.Level options.MaxInputFiles
            let allInputs = inputs @ nextFiles
            let! opened = openInputs openScan shouldStop [] allInputs

            match opened with
            | None -> return EmptyCompactResult
            | Some scans ->
                let! merge = openMerge scans
                return! runPump dir options plan activeSnapshot supply shouldStop allInputs merge
        }

    /// 1回の Leveled compaction を行う（ストリーミング）。
    /// <remarks>
    /// 入力は全読みしない。各ファイルを `SstScan` で開き、`SstMerge` で 1件ずつマージしながら
    /// キー単位で GC し、出力ファイル 1本分（`OutputFileTargetBytes` 相当）だけをメモリに持つ。
    /// 出力ファイルはキーの切れ目で切り替えるので、1つのキーの版が複数ファイルに跨らない。
    /// `openScan` はファイルの先頭から読むスキャンを返す（部分読みリーダーを使えばブロック単位で読む）。
    /// MANIFEST の更新と実ファイルの削除は呼び出し側が行う。
    /// </remarks>
    let private runCompact
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (supply: FileNumberSupply)
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        (shouldStop: unit -> bool)
        =
        match pickCompaction options version with
        | Some plan -> compactPlan dir version options plan activeSnapshot supply openScan shouldStop
        | None -> ret EmptyCompactResult

    /// バックグラウンド compaction。
    /// ファイル番号は [allocateFileNumber] から取得する。
    /// `shouldStop` が true を返したら、作成済みの出力を削除し、
    /// 入力の走査を閉じて空の結果を返す（MANIFEST には何も書かない）。
    let compactWith
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        (allocateFileNumber: unit -> FileNumber)
        (shouldStop: unit -> bool)
        =
        runCompact dir version options activeSnapshot (Allocator allocateFileNumber) openScan shouldStop

    /// 同期 compaction。
    /// ファイル番号は [nextFileNumber] から 1 ずつ採番する。
    let compact
        (dir: string)
        (version: Version)
        (options: CompactionOptions)
        (activeSnapshot: uint64)
        (nextFileNumber: FileNumber)
        (openScan: FileMetaData -> Result<SstScan, DbError>)
        =
        runCompact dir version options activeSnapshot (Sequence nextFileNumber) openScan (fun () -> false)
