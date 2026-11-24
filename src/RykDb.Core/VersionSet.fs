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

    /// SSTable のファイル名（6桁の番号と拡張子）。
    let private fileName (fileNumber: int64) =
        sprintf "%06d%s" fileNumber SstExtension

    /// SSTable のフルパス。
    let path (dir: string) (fileNumber: int64) =
        System.IO.Path.Combine(dir, fileName fileNumber)

    /// 書き出し結果から SSTable のメタデータを組み立てる。
    let private ofChunk (dir: string) (fileNumber: int64) (level: int) (info: SstWriteInfo) =
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

    /// 失敗時に部分ファイルを削除する。
    let private deletePartial (target: string) =
        protect (fun () ->
            if System.IO.File.Exists target then
                System.IO.File.Delete target)
        |> ignore

    /// エントリ列を SSTable としてファイルへストリーム書きし、メタデータを返す。
    /// ブロックと Bloom filter は 1個ずつ組み立てて書くので、ファイル全体分のメモリを持たない。
    /// 入力の `seq` は 1回だけ列挙する
    /// （memtable の flush が全件を配列にしないため）。
    /// `expectedEntries` は Bloom filter のビット数を決める見積り。
    /// 失敗時（キー順違反・書き込み失敗・空の入力）は部分ファイルを消してからエラーを返す。
    let writeSstable
        (dir: string)
        (fileNumber: int64)
        (level: int)
        (bloomFalsePositiveRate: float)
        (expectedEntries: int)
        (entries: seq<SstEntry>)
        =
        let target = path dir fileNumber

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

                SstableFormat.writeTo fs bloomFalsePositiveRate expectedEntries entries
                >>= fun info ->
                    fs.Flush true
                    ret info)
            >>= id

        match written with
        | Error e ->
            deletePartial target
            err e
        | Ok info when info.EntryCount = 0L ->
            deletePartial target
            err (DbError.internalError "Sstable: 空のエントリ列は書き出せません")
        | Ok info -> ret (ofChunk dir fileNumber level info)

    /// このファイルが [key] を持ち得るか（Bloom より粗い判定）。
    let mightContain (key: byte array) (f: FileMetaData) =
        Bytes.compare f.Smallest key <= 0 && Bytes.compare key f.Largest <= 0

    /// ファイルを 1つ削除する（存在しなければ何もしない）。
    let private deleteFile (dir: string) (fileNumber: int64) =
        protect (fun () ->
            let p = path dir fileNumber

            if System.IO.File.Exists p then
                System.IO.File.Delete p)

    /// ファイルを実際に削除する。
    let deleteObsolete (dir: string) (files: FileMetaData list) =
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

    /// キー以上の Smallest を持つ最初の位置を二分探索する。
    [<TailCall>]
    let rec private search (key: byte array) (files: FileMetaData array) (lo: int) (hi: int) =
        if lo >= hi then
            lo
        else
            let mid = lo + (hi - lo) / 2

            if Bytes.compare files.[mid].Smallest key <= 0 then
                search key files (mid + 1) hi
            else
                search key files lo mid

    /// レベル内で key を含み得るファイルを 1つ返す。
    /// L1 以降は Smallest 昇順・非重複なので二分探索できる。
    let private findLevelFile (key: byte array) (files: FileMetaData array) =
        let index = search key files 0 files.Length

        if index > 0 && FileMetaData.mightContain key files.[index - 1] then
            Some files.[index - 1]
        else
            None

    /// ポイントルックアップで consulted すべきファイル（新しい順）。
    /// L0 はキー域が重なり得るので範囲内の全ファイルを番号降順に返す。
    /// L1 以降は 1ファイル = 1キー域なので、レベルごとの二分探索で候補を絞る。
    let filesForPointLookup (key: byte array) (v: Version) =
        let l0 =
            v.Files.[0]
            |> Array.filter (fun f -> FileMetaData.mightContain key f)
            |> Array.sortByDescending (fun f -> f.FileNumber)
            |> Array.toList

        let higher =
            [ 1 .. Levels.Count - 1 ]
            |> List.collect (fun level ->
                match findLevelFile key v.Files.[level] with
                | Some f -> [ f ]
                | None -> [])

        l0 @ higher

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

    /// VersionEdit の追加ファイルを符号化する。
    let private encodeFile (f: FileMetaData) =
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

    /// VersionEdit の削除ファイルを符号化する。
    let private encodeDeleted (f: FileMetaData) =
        Writer.build [ Writer.varint (uint64 f.FileNumber); Writer.varint (uint64 f.Level) ]

    let encode (edit: VersionEdit) =
        let steps =
            (edit.AddedFiles
             |> List.collect (fun f -> [ Writer.varint 1UL; Writer.writeBytes (encodeFile f) ]))
            @ (edit.DeletedFiles
               |> List.collect (fun f -> [ Writer.varint 2UL; Writer.writeBytes (encodeDeleted f) ]))

        Writer.build [ Writer.build steps |> Writer.lengthPrefixed ]

    /// VersionEdit の符号化された int64 を読む（上限超過は CorruptionError）。
    let private readEditInt64 (label: string) (c: Cursor) =
        Reader.readVarint c
        >>= fun (v, c) ->
            if v > uint64 System.Int64.MaxValue then
                err (sprintf "VersionEdit: %s が大きすぎます" label |> DbError.corruption)
            else
                ret (int64 v, c)

    /// VersionEdit のファイル番号を読む。
    let private readEditFileNumber (c: Cursor) = readEditInt64 "ファイル番号" c

    /// VersionEdit のレベルを読む（範囲外は CorruptionError）。
    let private readEditLevel (c: Cursor) =
        Reader.readVarint c
        >>= fun (v, c) ->
            if v > uint64 Levels.Max then
                err (sprintf "VersionEdit: レベル %d が範囲外です" v |> DbError.corruption)
            else
                ret (int v, c)

    /// VersionEdit の追加ファイル 1件を読む。
    let private decodeEditFile (c: Cursor) =
        result {
            let! num, c = readEditFileNumber c
            let! lvl, c = readEditLevel c
            let! cnt, c = readEditInt64 "エントリ数" c
            let! size, c = readEditInt64 "ファイルサイズ" c
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
                    EntryCount = cnt
                    Size = size
                    MaxSeqNo = maxSeq
                    ReadInfo = None
                }
        }

    /// VersionEdit の削除ファイル 1件を読む。
    let private decodeEditDeleted (c: Cursor) =
        result {
            let! num, c = readEditFileNumber c
            let! lvl, c = readEditLevel c

            return
                c,
                { FileMetaData.Zero with
                    FileNumber = num
                    Level = lvl
                }
        }

    /// VersionEdit のレコード 1件を読む（追加なら isAdded=true）。
    let private decodeEditStep (c: Cursor) =
        result {
            let! tag, c = Reader.readVarint c

            let! c, file =
                if tag = 1UL then
                    decodeEditFile c
                elif tag = 2UL then
                    decodeEditDeleted c
                else
                    err (sprintf "VersionEdit: 不明なタグ %d" tag |> DbError.corruption)

            return c, file, tag = 1UL
        }

    /// VersionEdit の全レコードを読む。
    [<TailCall>]
    let rec private readEditFiles (c: Cursor) (added: FileMetaData list) (deleted: FileMetaData list) =
        if Cursor.atEnd c then
            ret (List.rev added, List.rev deleted)
        else
            match decodeEditStep c with
            | Ok(c', f, isAdded) ->
                if isAdded then
                    readEditFiles c' (f :: added) deleted
                else
                    readEditFiles c' added (f :: deleted)
            | Error e -> err e

    let decode (data: byte array) =
        result {
            let! body, _ = Cursor.create data |> Reader.readLengthPrefixed
            let! added, deleted = readEditFiles (Cursor.create body) [] []

            return
                [
                    {
                        AddedFiles = added
                        DeletedFiles = deleted
                    }
                ]
        }

/// MANIFEST への反映結果。
/// <remarks>
/// `NotApplied` は追記前の内容が保たれていることを意味し、出力ファイルを削除してよい。
/// `Unknown` は追記が Disk に残ったか判別できない状態で、出力を消してはいけない
/// （再オープン時に MANIFEST が参照している可能性がある）。
/// </remarks>
[<RequireQualifiedAccess>]
type internal ManifestApplyResult =
    | Applied of version: Version * manifestNumber: int64<manifestNo>
    | NotApplied of error: DbError
    | Unknown of error: DbError

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

    /// manifest のファイル名から番号を取り出す。
    let private parseManifestNumber (path: string) =
        let name = System.IO.Path.GetFileName path

        if name.StartsWith(ManifestPrefix, System.StringComparison.Ordinal) then
            match System.Int64.TryParse(name.Substring 9) with
            | true, value -> Some(LanguagePrimitives.Int64WithMeasure<manifestNo> value)
            | _ -> None
        else
            None

    /// 既存の最大番号より 1つ大きい manifest 番号を返す。
    let private nextManifestNumber (dir: string) (current: int64<manifestNo>) =
        protect (fun () ->
            System.IO.Directory.GetFiles(dir, ManifestPrefix + "*")
            |> Array.choose parseManifestNumber
            |> Array.fold (fun acc number -> max acc number) current
            |> fun highest -> highest + 1L<manifestNo>)

    /// Version 全体を新しい manifest に書き出す（スナップショット）。
    let private writeSnapshot (dir: string) (manifestNumber: int64<manifestNo>) (version: Version) =
        protect (fun () ->
            let edit =
                {
                    AddedFiles = Version.allFiles version
                    DeletedFiles = []
                }

            let data =
                System.Text.Encoding.UTF8.GetBytes(encodeRecord (VersionEdit.encode edit))

            use fs =
                new System.IO.FileStream(
                    manifestPath dir manifestNumber,
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.Read
                )

            fs.Write(data, 0, data.Length)
            fs.Flush true)

    /// 追記の失敗種別。
    /// `Rejected` は追記前の内容が保たれていることを意味する。
    /// `Uncertain` は追記前の長さへ戻すことにも失敗し、レコードの有無を保証できない。
    type private AppendFailure =
        | AppendRejected of DbError
        | AppendUncertain of DbError

    /// 末尾の改行まで遡って、完全な行の終端位置を返す。
    [<TailCall>]
    let rec private lastNewline (fs: System.IO.FileStream) (pos: int64) =
        if pos <= 0L then
            0L
        else
            fs.Position <- pos - 1L

            if fs.ReadByte() = int '\n' then
                pos
            else
                lastNewline fs (pos - 1L)

    /// 追記前の長さと内容から、レコードが実際に書き込まれたかを判定する。
    let private recordWasWritten (path: string) (data: byte array) (expectedLine: string) (oldLength: int64) =
        match
            protect (fun () ->
                System.IO.FileInfo(path).Length = oldLength + int64 data.Length
                && System.IO.File.ReadAllLines path |> Array.tryLast = Some expectedLine)
        with
        | Ok written -> written
        | Error _ -> false

    /// ファイルを追記前の長さまで切り詰める。
    let private rollback (path: string) (oldLength: int64) =
        protect (fun () ->
            use fs =
                new System.IO.FileStream(
                    path,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.ReadWrite,
                    System.IO.FileShare.Read
                )

            fs.SetLength oldLength
            fs.Flush true)

    /// 追記前の有効な長さを求め、必要なら途中の行を切り詰める。
    let private readOldLength (path: string) =
        use fs =
            new System.IO.FileStream(
                path,
                System.IO.FileMode.Open,
                System.IO.FileAccess.ReadWrite,
                System.IO.FileShare.Read
            )

        let oldLength =
            if fs.Length = 0L then
                0L
            else
                fs.Position <- fs.Length - 1L

                if fs.ReadByte() = int '\n' then
                    fs.Length
                else
                    lastNewline fs (fs.Length - 1L)

        if fs.Length <> oldLength then
            fs.SetLength oldLength
            fs.Flush true

        oldLength

    /// MANIFEST に 1レコード追記する。
    /// <remarks>
    /// 末尾が改行で終わっていなければ、前回の書き込みが途中で失敗したとみなして
    /// 最後の完全な行まで切り詰めてから追記する。
    /// 書き込み中に失敗した場合は、追記前の長さまで戻す。
    /// それにも失敗した場合だけ `AppendUncertain` を返す。
    /// </remarks>
    let private append (dir: string) (manifestNumber: int64<manifestNo>) (edit: VersionEdit) =
        let path = manifestPath dir manifestNumber
        let text = encodeRecord (VersionEdit.encode edit)
        let data = System.Text.Encoding.UTF8.GetBytes text
        let expectedLine = text.TrimEnd '\n'

        let oldLength =
            protect (fun () -> System.IO.File.Exists path)
            >>= fun exists ->
                if exists then
                    protect (fun () -> readOldLength path)
                else
                    sprintf "MANIFEST が見つかりません: %s" (fileName manifestNumber)
                    |> DbError.corruption
                    |> err

        match oldLength with
        | Error e -> Error(AppendRejected e)
        | Ok oldLength ->
            match
                protect (fun () ->
                    use fs =
                        new System.IO.FileStream(
                            path,
                            System.IO.FileMode.Open,
                            System.IO.FileAccess.Write,
                            System.IO.FileShare.Read
                        )

                    fs.Position <- oldLength
                    fs.Write(data, 0, data.Length)
                    fs.Flush true)
            with
            | Ok() -> Ok(oldLength + int64 data.Length)
            | Error _ when recordWasWritten path data expectedLine oldLength -> Ok(oldLength + int64 data.Length)
            | Error e ->
                match rollback path oldLength with
                | Ok() -> Error(AppendRejected e)
                | Error _ -> Error(AppendUncertain e)

    /// ディレクトリを作る（既にあれば何もしない）。
    let ensureDirectory (dir: string) =
        protect (fun () ->
            if not (System.IO.Directory.Exists dir) then
                System.IO.Directory.CreateDirectory dir |> ignore)

    /// CURRENT が指す manifest 番号を読む。
    let private readCurrent (dir: string) =
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

    /// manifest の行を順にデコードし、末尾の途中行を検出する。
    [<TailCall>]
    let rec private decodeLines (acc: VersionEdit list) (lines: string list) =
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

    /// manifest ファイルの行を読む。
    let private read (dir: string) (manifestNumber: int64<manifestNo>) =
        let path = manifestPath dir manifestNumber

        if System.IO.File.Exists path then
            ret (System.IO.File.ReadAllLines path |> Array.toList)
        else
            sprintf "有効な manifest が見つかりません: %s" (fileName manifestNumber)
            |> DbError.corruption
            |> err

    /// manifest を保護して読み、デコード結果と末尾の状態を返す。
    let private readAll (dir: string) (manifestNumber: int64<manifestNo>) =
        match protect (fun () -> read dir manifestNumber) with
        | Ok(Error e) -> err e
        | Ok(Ok lines) ->
            let hasContent = lines |> List.exists (fun line -> line.Trim() <> "")

            decodeLines [] lines
            |> Result.map (fun (edits, truncated) -> edits, truncated, hasContent)
        | Error e -> err e

    /// デコードした編集を Version へ順に適用する。
    let private applyAll (edits: VersionEdit list) (initial: Version) =
        List.fold VersionEdit.apply initial edits

    /// SSTable ファイルが残っているか。
    let private hasSstFiles (dir: string) =
        protect (fun () -> System.IO.Directory.GetFiles(dir, "*" + FileMetaData.SstExtension).Length > 0)

    /// データベースを開く（新規なら作成する）。
    /// 返り値は (Version, 有効な manifest 番号)。
    let openOrCreate (dir: string) =
        result {
            let! _ = ensureDirectory dir
            let! hasSsts = hasSstFiles dir
            let! current = readCurrent dir

            match current with
            | Some manifestNumber ->
                let! decoded = readAll dir manifestNumber
                let edits, truncated, hasContent = decoded
                let version = applyAll edits Version.Empty

                if List.isEmpty (Version.allFiles version) && hasContent && hasSsts then
                    return! err (DbError.corruption "manifest が壊れており、SSTable の管理情報を復元できません（SSTable は変更していません）")
                elif truncated then
                    let! nextNumber = nextManifestNumber dir manifestNumber
                    let! _ = writeSnapshot dir nextNumber version
                    let! _ = writeCurrent dir nextNumber

                    protect (fun () ->
                        let old = manifestPath dir manifestNumber

                        if System.IO.File.Exists old then
                            System.IO.File.Delete old)
                    |> ignore

                    return version, nextNumber
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

    /// .VersionEdit を反映し、結果を「反映済み / 未反映 / 不明」で返す。
    /// <remarks>
    /// ローテーションが必要な場合は、追記ではなく新しい MANIFEST へ Version 全体を書いて CURRENT を切り替える。
    /// CURRENT の切り替えに失敗した場合は旧 MANIFEST がそのまま残る。
    /// `manifestNumber` は単位 `manifestNo` を持つ。
    /// この関数は `version.NextFileNumber`（SSTable のファイル番号・素の `int64`）も扱うので、
    /// 2つの番号を取り違えるとコンパイルが通らない。
    /// </remarks>
    let logAndApplyWithOutcome
        (dir: string)
        (manifestNumber: int64<manifestNo>)
        (version: Version)
        (edit: VersionEdit)
        =
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

        let recordLength =
            VersionEdit.encode edit
            |> encodeRecord
            |> System.Text.Encoding.UTF8.GetByteCount
            |> int64

        let path = manifestPath dir manifestNumber

        match
            protect (fun () ->
                if System.IO.File.Exists path then
                    System.IO.FileInfo(path).Length
                else
                    0L)
        with
        | Error e -> ManifestApplyResult.NotApplied e
        | Ok size when size + recordLength > MaxManifestBytes ->
            match nextManifestNumber dir manifestNumber with
            | Error e -> ManifestApplyResult.NotApplied e
            | Ok nextNumber ->
                match writeSnapshot dir nextNumber bumped with
                | Error e -> ManifestApplyResult.NotApplied e
                | Ok() ->
                    match writeCurrent dir nextNumber with
                    | Error e -> ManifestApplyResult.NotApplied e
                    | Ok() ->
                        protect (fun () ->
                            if System.IO.File.Exists path then
                                System.IO.File.Delete path)
                        |> ignore

                        ManifestApplyResult.Applied(bumped, nextNumber)
        | Ok _ ->
            match append dir manifestNumber edit with
            | Ok _ -> ManifestApplyResult.Applied(bumped, manifestNumber)
            | Error(AppendRejected e) -> ManifestApplyResult.NotApplied e
            | Error(AppendUncertain e) -> ManifestApplyResult.Unknown e

    /// .VersionEdit を反映する。
    /// <remarks>
    /// 戻り値の `Ok` は「MANIFEST に反映済み」、`Error` は「反映されていないか不明」を表す。
    /// 不明と未反映を区別する必要がある呼び出し側は `logAndApplyWithOutcome` を使う。
    /// </remarks>
    let logAndApply (dir: string) (manifestNumber: int64<manifestNo>) (version: Version) (edit: VersionEdit) =
        match logAndApplyWithOutcome dir manifestNumber version edit with
        | ManifestApplyResult.Applied(version, manifestNumber) -> Ok(version, manifestNumber)
        | ManifestApplyResult.NotApplied e
        | ManifestApplyResult.Unknown e -> Error e

/// MANIFEST に反映済みの現在状態
/// （background compaction が共有する）。
type internal PublishedState =
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

module internal PublishedState =
    let initial (version: Version) (manifestNumber: int64<manifestNo>) =
        {
            Version = version
            ManifestNumber = manifestNumber
            ActiveSnapshot = 0UL
            LastCompactionError = None
            Generation = 0L
        }
