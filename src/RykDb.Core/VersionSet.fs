namespace RykDb.Core

/// LSM のレベル設定。
module internal Levels =
    /// レベル数。L0 が flush 出力、L1〜L6 が compaction の出力先。
    [<Literal>]
    let Count = 7

    /// 最下位レベル。
    [<Literal>]
    let Max = 6

/// 1 つの SSTable のメタデータ。
type internal FileMetaData =
    {
        FileNumber: int64
        Level: int
        /// このファイルが持つ最小キー
        Smallest: byte array
        /// このファイルが持つ最大キー
        Largest: byte array
        EntryCount: int64
        /// ファイルサイズ（バイト）
        Size: int64
        /// このファイルに含まれる SeqNo の最大値。
        /// GC の判定に使う：「最古の active snapshot より小さい SeqNo の版は
        /// もう誰にも見えないので捨てられる」ために使う。
        MaxSeqNo: uint64
    }

    /// このファイルが [key] を持ち得るか（Bloom より粗い判定）。
    member x.MightContain(key: byte array) =
        SstEntry.compareBytes x.Smallest key <= 0
        && SstEntry.compareBytes key x.Largest <= 0

/// コンパクションの候補。
type internal CompactionPlan =
    {
        /// 入力となるレベル
        Level: int
        /// 出力を置くレベル
        OutputLevel: int
        /// 入力ファイル
        InputFiles: FileMetaData list
        /// 出力時に重なり判定に使うファイル（1 つ上のレベル）
        GrandParents: FileMetaData list
    }

/// LSM のバージョン（レベルごとのファイル集合のスナップショット）。不変。
/// ファイル追加・削除は新しい Version を返すので、読み取り中に状態が書き換わることはない。
type internal Version =
    {
        /// [level] が、そのレベルのファイル（キーの昇順に並べる）
        Files: FileMetaData array array
        /// コンパクションが選ぶために使うApproximateSize
        SequenceNumber: uint64
        NextFileNumber: int64
    }

module internal Version =
    let empty =
        {
            Files = Array.create Levels.Count [||]
            SequenceNumber = 0UL
            NextFileNumber = 1L
        }

    /// 指定レベルのファイル列。
    let filesAt (v: Version) (level: int) = v.Files.[level]

    /// ファイルを追加した新しい Version を返す。
    let addFile (f: FileMetaData) (v: Version) =
        let levelFiles = v.Files.[f.Level]

        let merged =
            Array.append [| f |] levelFiles
            |> Array.sortWith (fun a b -> SstEntry.compareBytes a.Smallest b.Smallest)

        { v with
            Files = Array.mapi (fun i l -> if i = f.Level then merged else l) v.Files
        }

    /// ファイルを削除した新しい Version を返す。
    let deleteFiles (targets: FileMetaData list) (v: Version) =
        let targetNumbers = targets |> List.map (fun f -> f.FileNumber) |> Set.ofList

        let newFiles =
            v.Files
            |> Array.map (fun levelFiles ->
                levelFiles
                |> Array.filter (fun f -> not (Set.contains f.FileNumber targetNumbers)))

        { v with Files = newFiles }

    /// すべてのファイル（レベル昇順、同レベルはキー昇順）。
    let allFiles (v: Version) =
        v.Files |> Array.collect id |> Array.toList

    /// レベルごとのファイル数。
    let levelCounts (v: Version) = v.Files |> Array.map Array.length

    /// 点ルックアップで consulted すべきファイル（新しい順）。
    let filesForPointLookup (v: Version) (key: byte array) =
        v.Files
        |> Array.mapi (fun level files -> files |> Array.map (fun f -> level, f))
        |> Array.collect id
        |> Array.filter (fun (_, f) -> f.MightContain key)
        |> Array.sortByDescending (fun (level, f) -> struct (-level, f.FileNumber))
        |> Array.map snd
        |> Array.toList

    /// コンパクション候補を選ぶ。
    /// 1 段目のレベルから閾値を超えたら、そのレベル全体＋1 段下の全ファイルを
    /// 入力とする。key 範囲の重なり計算は Compaction 側で行う。
    let pickCompaction (v: Version) (maxLevelSize: int64 array) =
        let rec find level =
            if level > Levels.Max then
                None
            else
                let total = v.Files.[level] |> Array.sumBy (fun f -> f.Size)

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

/// 1 回のバージョン変更。
type internal VersionEdit =
    {
        AddedFiles: FileMetaData list
        DeletedFiles: FileMetaData list
    }

module internal VersionEdit =
    let addFile f =
        {
            AddedFiles = [ f ]
            DeletedFiles = []
        }

    let deleteFile f =
        {
            AddedFiles = []
            DeletedFiles = [ f ]
        }

    // -----------------------------------------------------------------------
    // 直列化
    //   [tag:varint] : 1 = 追加, 2 = 削除
    //   追加: [fileNumber][level][entryCount][size][maxSeqNo][smallest][largest]
    //   削除: [fileNumber][level]
    // -----------------------------------------------------------------------

    let private encodeFile (f: FileMetaData) =
        Writer.empty
        |> Writer.varint (uint64 f.FileNumber)
        |> fun w -> Writer.varint (uint64 f.Level) w
        |> fun w -> Writer.varint (uint64 f.EntryCount) w
        |> fun w -> Writer.varint (uint64 f.Size) w
        |> fun w -> Writer.varint f.MaxSeqNo w
        |> fun w -> Writer.lengthPrefixed f.Smallest w
        |> fun w -> Writer.lengthPrefixed f.Largest w
        |> Writer.toArray

    let private encodeDeleted (f: FileMetaData) =
        Writer.empty
        |> Writer.varint (uint64 f.FileNumber)
        |> fun w -> Writer.varint (uint64 f.Level) w
        |> Writer.toArray

    let encode (edit: VersionEdit) =
        let added =
            edit.AddedFiles
            |> List.fold (fun w f -> w |> Writer.varint 1UL |> fun w -> Writer.writeBytes (encodeFile f) w) Writer.empty

        let deleted =
            edit.DeletedFiles
            |> List.fold
                (fun w f -> w |> Writer.varint 2UL |> fun w -> Writer.writeBytes (encodeDeleted f) w)
                Writer.empty

        let body = Array.append (Writer.toArray added) (Writer.toArray deleted)
        Writer.empty |> Writer.lengthPrefixed body |> Writer.toArray

    let decode (data: byte array) =
        let readFileNumber c =
            Read.readVarint c |> Result.map (fun (v, c) -> int64 v, c)

        let readLevel c =
            Read.readVarint c |> Result.map (fun (v, c) -> int v, c)

        let step (c: Cursor) =
            result {
                let! tag, c = Read.readVarint c

                let! c, file =
                    if tag = 1UL then
                        result {
                            let! num, c = readFileNumber c
                            let! lvl, c = readLevel c
                            let! cnt, c = Read.readVarint c
                            let! size, c = Read.readVarint c
                            let! maxSeq, c = Read.readVarint c
                            let! smallest, c = Read.readLengthPrefixed c
                            let! largest, c = Read.readLengthPrefixed c

                            return
                                c,
                                {
                                    FileNumber = num
                                    Level = lvl
                                    Smallest = smallest
                                    Largest = largest
                                    EntryCount = int64 cnt
                                    Size = int64 size
                                    MaxSeqNo = maxSeq
                                }
                        }
                    elif tag = 2UL then
                        result {
                            let! num, c = readFileNumber c
                            let! lvl, c = readLevel c

                            return
                                c,
                                {
                                    FileNumber = num
                                    Level = lvl
                                    Smallest = [||]
                                    Largest = [||]
                                    EntryCount = 0L
                                    Size = 0L
                                    MaxSeqNo = 0UL
                                }
                        }
                    else
                        Error(DbError.corruption (sprintf "VersionEdit: 不明なタグ %d" tag))

                return c, file, tag = 1UL
            }

        let rec go (c: Cursor) (added: FileMetaData list) (deleted: FileMetaData list) =
            if Cursor.atEnd c then
                ret (List.rev added, List.rev deleted)
            else
                match step c with
                | Error e -> Error e
                | Ok(c', f, isAdded) ->
                    if isAdded then
                        go c' (f :: added) deleted
                    else
                        go c' added (f :: deleted)

        result {
            let! body, _ = Read.readLengthPrefixed (Cursor.create data)
            let! added, deleted = go (Cursor.create body) [] []

            return
                [
                    {
                        AddedFiles = added
                        DeletedFiles = deleted
                    }
                ]
        }
