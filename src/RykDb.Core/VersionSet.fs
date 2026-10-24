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
        let levelFiles = filesAt v f.Level

        let merged =
            Array.append [| f |] levelFiles
            |> Array.sortWith (fun a b -> Bytes.compare a.Smallest b.Smallest)

        { v with
            Files = v.Files |> Array.mapi (fun i l -> if i = f.Level then merged else l)
        }

    /// ファイルを削除した新しい Version を返す。
    let deleteFiles (targets: FileMetaData list) (v: Version) =
        let targetNumbers = targets |> List.map _.FileNumber |> Set.ofList

        let newFiles =
            v.Files
            |> Array.map (fun levelFiles ->
                levelFiles
                |> Array.filter (fun f -> not (Set.contains f.FileNumber targetNumbers)))

        { v with Files = newFiles }

    /// ポイントルックアップで consulted すべきファイル（新しい順）。
    let filesForPointLookup (v: Version) (key: byte array) =
        v.Files
        |> Array.mapi (fun level files -> files |> Array.map (fun f -> level, f))
        |> Array.collect id
        |> Array.filter (fun (_, f) -> FileMetaData.mightContain f key)
        |> Array.sortByDescending (fun (level, f) -> struct (-level, f.FileNumber))
        |> Array.map snd
        |> Array.toList

    /// すべてのファイル（レベル昇順、同レベルはキー昇順）。
    let allFiles (v: Version) =
        v.Files |> Array.collect id |> Array.toList

    /// レベルごとのファイル数。
    let levelCounts (v: Version) = v.Files |> Array.map Array.length

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

        Writer.build [ Writer.build steps |> Writer.lengthPrefixed ]

    let decode (data: byte array) =
        let readFileNumber c =
            Reader.readVarint c |> Result.map (fun (v, c) -> int64 v, c)

        let readLevel c =
            Reader.readVarint c |> Result.map (fun (v, c) -> int v, c)

        let decodeFile (c: Cursor) =
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

        let decodeDeleted (c: Cursor) =
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

        let step (c: Cursor) =
            result {
                let! tag, c = Reader.readVarint c

                let! c, file =
                    if tag = 1UL then
                        decodeFile c
                    elif tag = 2UL then
                        decodeDeleted c
                    else
                        err (sprintf "VersionEdit: 不明なタグ %d" tag |> DbError.corruption)

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
            let! body, _ = Cursor.create data |> Reader.readLengthPrefixed
            let! added, deleted = readFiles (Cursor.create body) [] []

            return
                [
                    {
                        AddedFiles = added
                        DeletedFiles = deleted
                    }
                ]
        }
