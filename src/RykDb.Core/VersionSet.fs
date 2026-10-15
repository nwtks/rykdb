namespace RykDb.Core

/// LSM のレベル設定。
module internal Levels =
    /// レベル数。
    /// L0 が flush 出力、L1〜L6 が compaction の出力先。
    [<Literal>]
    let Count = 7

    /// 最下位レベル。
    [<Literal>]
    let Max = 6

/// 1つの SSTable のメタデータ。
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
        /// GC の判定に使う：
        /// 「最古の active snapshot より小さい SeqNo の版はもう誰にも見えないので捨てられる」ために使う。
        MaxSeqNo: uint64
    }

module internal FileMetaData =
    /// キー範囲も件数も未確定の空メタデータ。
    /// デコード途中の「番号とレベルだけ分かった」状態や、「compaction なし」の報告に使う
    /// （フィールドは上書きする）。
    let zero =
        {
            FileNumber = 0L
            Level = 0
            Smallest = [||]
            Largest = [||]
            EntryCount = 0L
            Size = 0L
            MaxSeqNo = 0UL
        }

    /// エントリ列を SSTable としてビルドし、ファイルに書き、メタデータを返す。
    /// ビルド・書き出し・メタ算出が 1つの経路を通るので、「書いたバイト数」と Size が食い違うことが無い。
    let writeSstable
        (dir: string)
        (fileNumber: int64)
        (level: int)
        (bloomFalsePositiveRate: float)
        (entries: SstEntry array)
        =
        let ofChunk (fileNumber: int64) (level: int) (size: int64) (entries: SstEntry array) =
            {
                FileNumber = fileNumber
                Level = level
                Smallest = entries.[0].Key
                Largest = entries.[entries.Length - 1].Key
                EntryCount = int64 entries.Length
                Size = size
                MaxSeqNo = SstEntry.maxSeqNo entries
            }

        match Sstable.build entries bloomFalsePositiveRate with
        | Error e -> Error e
        | Ok bytes ->
            match protect (fun () -> System.IO.File.WriteAllBytes(Sstable.path dir fileNumber, bytes) |> ignore) with
            | Error e -> Error e
            | Ok() -> ret (ofChunk fileNumber level (int64 bytes.Length) entries)

    /// このファイルが [key] を持ち得るか（Bloom より粗い判定）。
    let mightContain (f: FileMetaData) (key: byte array) =
        Bytes.compare f.Smallest key <= 0 && Bytes.compare key f.Largest <= 0

    /// ファイル列を先頭から順に処理する「回す側」。
    /// read が 1ファイル分を読み、step が状態を進める。
    /// 最初の Error で打ち切る。
    /// read / step は再帰しないので bind で合成してよい。
    let foldFiles
        (read: FileMetaData -> Result<'a, DbError>)
        (step: 's -> 'a -> 's)
        (state: 's)
        (files: FileMetaData list)
        =
        let rec foldEach (rest: FileMetaData list) (acc: 's) =
            match rest with
            | [] -> ret acc
            | f :: tail ->
                match read f with
                | Error e -> Error e
                | Ok v -> foldEach tail (step acc v)

        foldEach files state

/// LSM のバージョン（レベルごとのファイル集合のスナップショット）。
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
            |> Array.sortWith (fun a b -> Bytes.compare a.Smallest b.Smallest)

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

    /// ポイントルックアップで consulted すべきファイル（新しい順）。
    let filesForPointLookup (v: Version) (key: byte array) =
        v.Files
        |> Array.mapi (fun level files -> files |> Array.map (fun f -> level, f))
        |> Array.collect id
        |> Array.filter (fun (_, f) -> FileMetaData.mightContain f key)
        |> Array.sortByDescending (fun (level, f) -> struct (-level, f.FileNumber))
        |> Array.map snd
        |> Array.toList

/// 1回のバージョン変更。
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

    /// VersionEdit のファイル変更を適用する（削除してから追加）。
    let apply (version: Version) (edit: VersionEdit) =
        let afterDelete = Version.deleteFiles edit.DeletedFiles version

        edit.AddedFiles
        |> List.fold (fun current file -> Version.addFile file current) afterDelete

    // -----------------------------------------------------------------------
    // 直列化
    //   [tag:varint] : 1 = 追加, 2 = 削除
    //   追加: [fileNumber][level][entryCount][size][maxSeqNo][smallest][largest]
    //   削除: [fileNumber][level]
    // -----------------------------------------------------------------------

    let encode (edit: VersionEdit) =
        let encodeFile (f: FileMetaData) =
            Writer.build
                [
                    Writer.varint (uint64 f.FileNumber)
                    Writer.varint (uint64 f.Level)
                    Writer.varint (uint64 f.EntryCount)
                    Writer.varint (uint64 f.Size)
                    Writer.varint f.MaxSeqNo
                    Writer.lengthPrefixed f.Smallest
                    Writer.lengthPrefixed f.Largest
                ]

        let encodeDeleted (f: FileMetaData) =
            Writer.build [ Writer.varint (uint64 f.FileNumber); Writer.varint (uint64 f.Level) ]

        let steps =
            (edit.AddedFiles
             |> List.collect (fun f -> [ Writer.varint 1UL; Writer.writeBytes (encodeFile f) ]))
            @ (edit.DeletedFiles
               |> List.collect (fun f -> [ Writer.varint 2UL; Writer.writeBytes (encodeDeleted f) ]))

        Writer.build [ Writer.lengthPrefixed (Writer.build steps) ]

    let decode (data: byte array) =
        let readFileNumber c =
            Reader.readVarint c |> Result.map (fun (v, c) -> int64 v, c)

        let readLevel c =
            Reader.readVarint c |> Result.map (fun (v, c) -> int v, c)

        let step (c: Cursor) =
            result {
                let! tag, c = Reader.readVarint c

                let! c, file =
                    if tag = 1UL then
                        result {
                            let! num, c = readFileNumber c
                            let! lvl, c = readLevel c
                            let! cnt, c = Reader.readVarint c
                            let! size, c = Reader.readVarint c
                            let! maxSeq, c = Reader.readVarint c
                            let! smallest, c = Reader.readLengthPrefixed c
                            let! largest, c = Reader.readLengthPrefixed c

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
                                { FileMetaData.zero with
                                    FileNumber = num
                                    Level = lvl
                                }
                        }
                    else
                        Error(DbError.corruption (sprintf "VersionEdit: 不明なタグ %d" tag))

                return c, file, tag = 1UL
            }

        let rec readFiles (c: Cursor) (added: FileMetaData list) (deleted: FileMetaData list) =
            if Cursor.atEnd c then
                ret (List.rev added, List.rev deleted)
            else
                match step c with
                | Error e -> Error e
                | Ok(c', f, isAdded) ->
                    if isAdded then
                        readFiles c' (f :: added) deleted
                    else
                        readFiles c' added (f :: deleted)

        result {
            let! body, _ = Reader.readLengthPrefixed (Cursor.create data)
            let! added, deleted = readFiles (Cursor.create body) [] []

            return
                [
                    {
                        AddedFiles = added
                        DeletedFiles = deleted
                    }
                ]
        }
