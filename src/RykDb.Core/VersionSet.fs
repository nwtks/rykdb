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
        /// 起動時の NextSeqNo 復元に使う
        /// （GC は `Compaction.gcGroup` が activeSnapshot と版ごとの SeqNo で判定する）。
        MaxSeqNo: uint64
        /// 読み取り用のメタデータ（footer / index / Bloom / meta の解析結果）。
        /// <remarks>
        /// MANIFEST には保存しない（ファイル内容から導出できる）。
        /// flush / compaction の書き出し時に 1回で作り、
        /// open 時に MANIFEST から復元したファイルは 1回だけ hydrate する。
        /// 不変の `Version` に付随するので、読み取りのたびに開き直さない。
        /// </remarks>
        ReadInfo: SstReadInfo option
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
            ReadInfo = None
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
                ReadInfo =
                    Some
                        {
                            Path = path dir fileNumber
                            Index = info.Index
                            MinKey = info.FirstKey
                            MaxKey = info.LastKey
                            Bloom = info.Bloom
                            EntryCount = info.EntryCount
                            MaxSeqNo = info.MaxSeqNo
                        }
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

                SstableFormat.writeTo fs bloomFalsePositiveRate entries
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
                        ReadInfo = None
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
                | Ok(c', f, isAdded) ->
                    if isAdded then
                        readFiles c' (f :: added) deleted
                    else
                        readFiles c' added (f :: deleted)
                | Error e -> err e

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

/// MANIFEST に反映済みの現在状態
/// （background compaction が共有する）。
type internal HostState =
    {
        Version: Version
        /// 有効な manifest 番号
        /// （SSTable のファイル番号とは単位が違う）。
        ManifestNumber: int64<manifestNo>
        /// commit 時に更新される GC 境界。0 は「全版を残す」。
        ActiveSnapshot: uint64
        /// 直近の compaction の失敗（成功したら None）。
        LastCompactionError: DbError option
        /// publish のたびに増える世代番号。
        Generation: int64
    }

module internal HostState =
    let initial (version: Version) (manifestNumber: int64<manifestNo>) =
        {
            Version = version
            ManifestNumber = manifestNumber
            ActiveSnapshot = 0UL
            LastCompactionError = None
            Generation = 0L
        }

module internal Manifest =
    /// manifest ファイル名の接頭辞。
    [<Literal>]
    let ManifestPrefix = "MANIFEST-"

    let private fileName (manifestNumber: int64<manifestNo>) =
        sprintf "%s%06d" ManifestPrefix manifestNumber

    let private manifestPath (dir: string) (manifestNumber: int64<manifestNo>) =
        System.IO.Path.Combine(dir, fileName manifestNumber)

    [<Literal>]
    let CurrentFileName = "CURRENT"

    let private currentPath (dir: string) =
        System.IO.Path.Combine(dir, CurrentFileName)

    let private writeCurrent (dir: string) (manifestNumber: int64<manifestNo>) =
        protect (fun () ->
            let tmp = currentPath dir + ".tmp"
            System.IO.File.WriteAllText(tmp, fileName manifestNumber + "\n")

            use fs =
                new System.IO.FileStream(
                    tmp,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.None
                )

            fs.Flush true
            fs.Close()
            System.IO.File.Move(tmp, currentPath dir, true))

    /// MANIFEST の 1レコード。
    /// [checksum:u32LE][VersionEdit 本体] のフレームを base64 にして 1行にする。
    /// checksum は本体全体の CRC32C（途中の完全な行の破損を検出する）。
    let private encodeRecord (payload: byte array) =
        let frame =
            Writer.build [ Writer.uint32LE (Hashing.checksum payload); Writer.writeBytes payload ]

        System.Convert.ToBase64String frame + "\n"

    let private decodeRecord (line: string) =
        try
            let frame = System.Convert.FromBase64String line

            if frame.Length < 4 then
                err (DbError.corruption "manifest のレコードが短すぎます")
            else
                let payload = Array.sub frame 4 (frame.Length - 4)

                if Hashing.checksum payload <> Bytes.readLE32 frame 0 then
                    err (DbError.corruption "manifest のレコードのチェックサム不一致")
                else
                    VersionEdit.decode payload
        with
        | :? System.FormatException as e -> err (sprintf "manifest の base64 が不正: %s" e.Message |> DbError.corruption)
        | e -> err (sprintf "manifest のレコードが不正: %s" e.Message |> DbError.corruption)

    /// ディレクトリを作る（既にあれば何もしない）。
    let ensureDirectory (dir: string) =
        protect (fun () ->
            if not (System.IO.Directory.Exists dir) then
                System.IO.Directory.CreateDirectory dir |> ignore)

    /// データベースを開く（新規なら作成する）。
    /// 返り値は (Version, 有効な manifest 番号)。
    let openOrCreate (dir: string) =
        let readCurrent dir =
            protect (fun () ->
                if not (System.IO.File.Exists(currentPath dir)) then
                    None
                else
                    let text = System.IO.File.ReadAllText(currentPath dir).Trim()

                    if text = "" then
                        None
                    elif text.StartsWith ManifestPrefix && text.EndsWith ".manifest" then
                        text.Substring(9, text.Length - 9 - ".manifest".Length)
                        |> System.Int64.Parse
                        |> LanguagePrimitives.Int64WithMeasure<manifestNo>
                        |> Some
                    elif text.StartsWith ManifestPrefix then
                        text.Substring 9
                        |> System.Int64.Parse
                        |> LanguagePrimitives.Int64WithMeasure<manifestNo>
                        |> Some
                    else
                        raise (System.IO.InvalidDataException(sprintf "CURRENT の内容が不正: %s" text)))

        let rec decodeLines acc (lines: string list) =
            match lines with
            | [] -> ret (List.rev acc, false)
            | line :: rest ->
                if line.Trim() = "" then
                    decodeLines acc rest
                else
                    match decodeRecord line with
                    | Ok edits -> decodeLines (List.rev edits @ acc) rest
                    | Error _ when rest |> List.forall (fun l -> l.Trim() = "") -> ret (List.rev acc, true)
                    | Error e -> err e

        let read dir manifestNumber =
            let path = manifestPath dir manifestNumber

            if System.IO.File.Exists path then
                ret (System.IO.File.ReadAllLines path |> Array.toList)
            else
                sprintf "有効な manifest が見つかりません: %s" (fileName manifestNumber)
                |> DbError.corruption
                |> err

        let readAll dir manifestNumber =
            match protect (fun () -> read dir manifestNumber) with
            | Ok(Error e) -> err e
            | Ok(Ok lines) ->
                let hasContent = lines |> List.exists (fun line -> line.Trim() <> "")

                decodeLines [] lines
                |> Result.map (fun (edits, truncated) -> edits, truncated, hasContent)
            | Error e -> err e

        let applyAll edits initial =
            List.fold VersionEdit.apply initial edits

        let hasSstFiles dir =
            protect (fun () -> System.IO.Directory.GetFiles(dir, "*" + FileMetaData.SstExtension).Length > 0)

        result {
            let! _ = ensureDirectory dir
            let! hasSsts = hasSstFiles dir
            let! current = readCurrent dir

            match current with
            | Some manifestNumber ->
                let! decoded = readAll dir manifestNumber
                let edits, _, hasContent = decoded
                let version = applyAll edits Version.Empty

                if List.isEmpty (Version.allFiles version) && hasContent && hasSsts then
                    return! err (DbError.corruption "manifest が壊れており、SSTable の管理情報を復元できません（SSTable は変更していません）")
                else
                    return version, manifestNumber
            | None ->
                if hasSsts then
                    return! err (DbError.corruption "CURRENT が無いのに SSTable が残っています（データ損失の恐れ）")
                else
                    let manifestNumber = 1L<manifestNo>

                    let! _ =
                        protect (fun () -> System.IO.File.WriteAllText(manifestPath dir manifestNumber, ""))

                    let! _ = writeCurrent dir manifestNumber
                    return Version.Empty, manifestNumber
        }

    /// manifest がこれを超えたら、現在の Version 全体を新しい manifest へ書き直す。
    [<Literal>]
    let private MaxManifestBytes = 4L * 1024L * 1024L

    /// .VersionEdit を反映し、Version と次の manifest 番号を返す。
    /// <remarks>
    /// `manifestNumber` は単位 `manifestNo` を持つ。
    /// この関数は `version.NextFileNumber`（SSTable のファイル番号・素の `int64`）も扱うので、
    /// 2つの番号を取り違えるとコンパイルが通らない。
    /// </remarks>
    let logAndApply (dir: string) (manifestNumber: int64<manifestNo>) (version: Version) (edit: VersionEdit) =
        let append dir manifestNumber edit =
            protect (fun () ->
                let path = manifestPath dir manifestNumber
                let text = encodeRecord (VersionEdit.encode edit)

                use fs =
                    new System.IO.FileStream(
                        path,
                        System.IO.FileMode.Append,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.Read
                    )

                let data = System.Text.Encoding.UTF8.GetBytes text
                fs.Write(data, 0, data.Length)
                fs.Flush true)

        let parseManifestNumber (path: string) =
            let name = System.IO.Path.GetFileName path

            if name.StartsWith(ManifestPrefix, System.StringComparison.Ordinal) then
                match System.Int64.TryParse(name.Substring 9) with
                | true, value -> Some(LanguagePrimitives.Int64WithMeasure<manifestNo> value)
                | _ -> None
            else
                None

        let nextManifestNumber dir current =
            protect (fun () ->
                System.IO.Directory.GetFiles(dir, ManifestPrefix + "*")
                |> Array.choose parseManifestNumber
                |> Array.fold (fun acc number -> max acc number) current
                |> fun highest -> highest + 1L<manifestNo>)

        let writeSnapshot dir manifestNumber version =
            protect (fun () ->
                let edit =
                    {
                        AddedFiles = Version.allFiles version
                        DeletedFiles = []
                    }

                let text = encodeRecord (VersionEdit.encode edit)
                let data = System.Text.Encoding.UTF8.GetBytes text

                use fs =
                    new System.IO.FileStream(
                        manifestPath dir manifestNumber,
                        System.IO.FileMode.Create,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.Read
                    )

                fs.Write(data, 0, data.Length)
                fs.Flush true)

        result {
            let! _ = append dir manifestNumber edit
            let next = VersionEdit.apply version edit
            let seed = version.NextFileNumber

            let highestAdded =
                edit.AddedFiles |> List.fold (fun acc f -> max acc f.FileNumber) seed

            let used = highestAdded + 1L
            let current = next.NextFileNumber

            let bumped =
                if used > current then
                    { next with NextFileNumber = used }
                else
                    next

            let! size =
                protect (fun () -> System.IO.FileInfo(manifestPath dir manifestNumber).Length)

            if size < MaxManifestBytes then
                return bumped, manifestNumber
            else
                let! nextNumber = nextManifestNumber dir manifestNumber
                let! _ = writeSnapshot dir nextNumber bumped
                let! _ = writeCurrent dir nextNumber

                protect (fun () ->
                    let old = manifestPath dir manifestNumber

                    if System.IO.File.Exists old then
                        System.IO.File.Delete old)
                |> ignore

                return bumped, nextNumber
        }
