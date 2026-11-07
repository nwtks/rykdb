namespace RykDb.Core

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
    /// SSTable ファイルの拡張子。
    [<Literal>]
    let SstExtension = ".sst"

    /// キー範囲も件数も未確定の空メタデータ。
    /// デコード途中の「番号とレベルだけ分かった」状態や、「compaction なし」の報告に使う
    /// （フィールドは上書きする）。
    let Zero =
        {
            FileNumber = 0L
            Level = 0
            Smallest = [||]
            Largest = [||]
            EntryCount = 0L
            Size = 0L
            MaxSeqNo = 0UL
        }

    /// SSTable のフルパス。
    let path (dir: string) (fileNumber: int64) =
        let fileName fileNumber =
            sprintf "%06d%s" fileNumber SstExtension

        System.IO.Path.Combine(dir, fileName fileNumber)

    /// エントリ列を SSTable としてファイルへストリーム書きし、メタデータを返す。
    /// ブロックは 1個ずつ組み立てて書くので、ファイル全体分のメモリを持たない。
    /// 入力の `seq` は 1回だけ列挙する
    /// （memtable の flush が全件を配列にしないため）。
    /// 失敗時（キー順違反・書き込み失敗・空の入力）は部分ファイルを消してからエラーを返す。
    let writeSstable
        (dir: string)
        (fileNumber: int64)
        (level: int)
        (bloomFalsePositiveRate: float)
        (entries: seq<SstEntry>)
        =
        let ofChunk info =
            {
                FileNumber = fileNumber
                Level = level
                Smallest = info.FirstKey
                Largest = info.LastKey
                EntryCount = info.EntryCount
                Size = info.Size
                MaxSeqNo = info.MaxSeqNo
            }

        let target = path dir fileNumber

        let deletePartial () =
            protect (fun () ->
                if System.IO.File.Exists target then
                    System.IO.File.Delete target)
            |> ignore

        let written =
            protect (fun () ->
                use fs =
                    new System.IO.FileStream(
                        target,
                        System.IO.FileMode.Create,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.None,
                        64 * 1024,
                        System.IO.FileOptions.SequentialScan
                    )

                Sstable.writeTo fs bloomFalsePositiveRate entries
                >>= fun info ->
                    fs.Flush true
                    ret info)
            >>= id

        match written with
        | Error e ->
            deletePartial ()
            err e
        | Ok info when info.EntryCount = 0L ->
            deletePartial ()
            err (DbError.internalError "Sstable: 空のエントリ列は書き出せません")
        | Ok info -> ret (ofChunk info)

    /// このファイルが [key] を持ち得るか（Bloom より粗い判定）。
    let mightContain (key: byte array) (f: FileMetaData) =
        Bytes.compare f.Smallest key <= 0 && Bytes.compare key f.Largest <= 0

    /// ファイルを実際に削除する。
    let deleteObsolete (dir: string) (files: FileMetaData list) =
        let deleteFile dir fileNumber =
            protect (fun () ->
                let p = path dir fileNumber

                if System.IO.File.Exists p then
                    System.IO.File.Delete p)

        files |> foldOk (fun () f -> deleteFile dir f.FileNumber) ()

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
        NextFileNumber: int64
    }

module internal Version =
    let Empty =
        {
            Files = Array.create Levels.Count [||]
            NextFileNumber = 1L
        }

    /// 指定レベルのファイル列。
    let filesAt (level: int) (v: Version) = v.Files.[level]

    /// ファイルを追加した新しい Version を返す。
    let addFile (f: FileMetaData) (v: Version) =
        let levelFiles = filesAt f.Level v

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
    let filesForPointLookup (key: byte array) (v: Version) =
        v.Files
        |> Array.mapi (fun level files -> files |> Array.map (fun f -> level, f))
        |> Array.collect id
        |> Array.filter (fun (_, f) -> FileMetaData.mightContain key f)
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
        let encodeFile f =
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

        let encodeDeleted f =
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
            Reader.readVarint c
            >>= fun (v, c) ->
                if v > uint64 Levels.Max then
                    err (sprintf "VersionEdit: レベル %d が範囲外です" v |> DbError.corruption)
                else
                    ret (int v, c)

        let decodeFile c =
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

        let decodeDeleted c =
            result {
                let! num, c = readFileNumber c
                let! lvl, c = readLevel c

                return
                    c,
                    { FileMetaData.Zero with
                        FileNumber = num
                        Level = lvl
                    }
            }

        let step c =
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

        let rec readFiles c added deleted =
            if Cursor.atEnd c then
                ret (List.rev added, List.rev deleted)
            else
                match step c with
                | Error e -> err e
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
