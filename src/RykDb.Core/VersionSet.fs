namespace RykDb.Core

/// 1つの SSTable のメタデータ。
type internal FileMetaData =
    {
        FileNumber: FileNumber
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
        MaxSeqNo: SeqNo
        /// 読み取り用のメタデータ（footer / index / Bloom / meta の解析結果）。
        /// MANIFEST には保存しない（ファイル内容から導出できる）。
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
    let private fileName (fileNumber: FileNumber) =
        sprintf "%06d%s" fileNumber SstExtension

    /// SSTable のフルパス。
    let path (dir: string) (fileNumber: FileNumber) =
        System.IO.Path.Combine(dir, fileName fileNumber)

    /// 書き出し結果から SSTable のメタデータを組み立てる。
    let private ofChunk (dir: string) (fileNumber: FileNumber) (level: int) (info: SstWriteInfo) =
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
        (fileNumber: FileNumber)
        (level: int)
        (bloomFalsePositiveRate: float)
        (expectedEntries: int)
        (entries: SstEntry seq)
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

                SstableWriter.writeTo fs bloomFalsePositiveRate expectedEntries entries
                |> Result.map (fun info ->
                    fs.Flush true
                    info))
            >>= id

        match written with
        | Error e ->
            deletePartial target
            err e
        | Ok info when info.EntryCount = 0L ->
            deletePartial target
            err (DbError.internalError "Sstable: 空のエントリ列は書き出せません")
        | Ok info -> ret (ofChunk dir fileNumber level info)

    /// ファイルを 1つ削除する（存在しなければ何もしない）。
    let private deleteFile (dir: string) (fileNumber: FileNumber) =
        protect (fun () ->
            let p = path dir fileNumber

            if System.IO.File.Exists p then
                System.IO.File.Delete p)

    /// ファイルを実際に削除する。
    let deleteObsolete (dir: string) =
        foldOk (fun () f -> deleteFile dir f.FileNumber) ()

    /// このファイルが [key] を持ち得るか（Bloom より粗い判定）。
    let mightContain (key: byte array) (f: FileMetaData) =
        Bytes.compare f.Smallest key <= 0 && Bytes.compare key f.Largest <= 0

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
        NextFileNumber: FileNumber
    }

module internal Version =
    /// 空の Version（全レベル空、次のファイル番号 1）。
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
            |> Array.map (Array.filter (fun f -> not (Set.contains f.FileNumber targetNumbers)))

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
            |> Array.filter (FileMetaData.mightContain key)
            |> Array.sortByDescending _.FileNumber
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
    /// ファイル追加だけの編集を作る。
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
    let private encodeAdded (f: FileMetaData) =
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

    /// VersionEdit をバイト列にする。
    let encode (edit: VersionEdit) =
        let steps =
            (edit.AddedFiles
             |> List.collect (fun f -> [ Writer.varint 1UL; Writer.writeBytes (encodeAdded f) ]))
            @ (edit.DeletedFiles
               |> List.collect (fun f -> [ Writer.varint 2UL; Writer.writeBytes (encodeDeleted f) ]))

        Writer.build [ Writer.build steps |> Writer.lengthPrefixed ]

    /// VersionEdit の符号化された int64 を読む（上限超過は CorruptionError）。
    let private readInt64 (label: string) (c: Cursor) =
        Reader.readVarint c
        >>= fun (v, c) ->
            if v > uint64 System.Int64.MaxValue then
                err (sprintf "VersionEdit: %s が大きすぎます" label |> DbError.corruption)
            else
                ret (int64 v, c)

    /// VersionEdit のファイル番号を読む。
    let private readFileNumber = readInt64 "ファイル番号"

    /// VersionEdit のレベルを読む（範囲外は CorruptionError）。
    let private readLevel (c: Cursor) =
        Reader.readVarint c
        >>= fun (v, c) ->
            if v > uint64 Levels.Max then
                err (sprintf "VersionEdit: レベル %d が範囲外です" v |> DbError.corruption)
            else
                ret (int v, c)

    /// VersionEdit の追加ファイル 1件を読む。
    let private decodeAdded (c: Cursor) =
        result {
            let! num, c = readFileNumber c
            let! lvl, c = readLevel c
            let! cnt, c = readInt64 "エントリ数" c
            let! size, c = readInt64 "ファイルサイズ" c
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
    let private decodeDeleted (c: Cursor) =
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

    /// VersionEdit のレコード 1件を読む（追加なら isAdded=true）。
    let private decodeStep (c: Cursor) =
        result {
            let! tag, c = Reader.readVarint c

            let! c, file =
                if tag = 1UL then
                    decodeAdded c
                elif tag = 2UL then
                    decodeDeleted c
                else
                    err (sprintf "VersionEdit: 不明なタグ %d" tag |> DbError.corruption)

            return c, file, tag = 1UL
        }

    /// VersionEdit の全レコードを読む。
    [<TailCall>]
    let rec private readFiles (c: Cursor) (added: FileMetaData list) (deleted: FileMetaData list) =
        if Cursor.atEnd c then
            ret (List.rev added, List.rev deleted)
        else
            match decodeStep c with
            | Ok(c', f, isAdded) ->
                if isAdded then
                    readFiles c' (f :: added) deleted
                else
                    readFiles c' added (f :: deleted)
            | Error e -> err e

    /// バイト列から VersionEdit を復元する（要素 1件のリスト）。
    let decode (data: byte array) =
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

/// MANIFEST への反映結果。
[<RequireQualifiedAccess>]
type internal ManifestApplyResult =
    | Applied of version: Version * manifestNumber: ManifestNumber
    /// 追記前の内容が保たれている。出力ファイルを削除してよい。
    | NotApplied of error: DbError
    /// 追記が Disk に残ったか判別できない。出力を消してはいけない
    /// （再オープン時に MANIFEST が参照している可能性がある）。
    | Unknown of error: DbError

module internal Manifest =
    /// manifest がこれを超えたら、現在の Version 全体を新しい manifest へ書き直す。
    [<Literal>]
    let private MaxManifestBytes = 4L * 1024L * 1024L

    /// manifest ファイル名の接頭辞。
    [<Literal>]
    let private ManifestPrefix = "MANIFEST-"

    /// MANIFEST のファイル名。
    let private fileName (manifestNumber: ManifestNumber) =
        sprintf "%s%06d" ManifestPrefix manifestNumber

    /// MANIFEST のパス。
    let private manifestPath (dir: string) (manifestNumber: ManifestNumber) =
        System.IO.Path.Combine(dir, fileName manifestNumber)

    /// CURRENT の A スロット。
    [<Literal>]
    let private CurrentSlotA = "CURRENT.A"

    /// CURRENT の B スロット。
    [<Literal>]
    let private CurrentSlotB = "CURRENT.B"

    /// SSTable ファイルが残っているか。
    let private hasSstFiles (dir: string) =
        protect (fun () -> System.IO.Directory.GetFiles(dir, "*" + FileMetaData.SstExtension).Length > 0)

    /// manifest のファイル名から番号を取り出す。
    let private parseManifestNumber (path: string) =
        let name = System.IO.Path.GetFileName path

        if name.StartsWith(ManifestPrefix, System.StringComparison.Ordinal) then
            match System.Int64.TryParse(name.Substring ManifestPrefix.Length) with
            | true, value -> Some(LanguagePrimitives.Int64WithMeasure<manifestNo> value)
            | _ -> None
        else
            None

    /// CURRENT スロットを読み、内容が正しければ manifest 番号を返す。
    /// 壊れたスロットは無視する（もう一方のスロットで復旧する）。
    let private readCurrentSlot (dir: string) (slot: string) =
        protect (fun () ->
            let p = System.IO.Path.Combine(dir, slot)

            if not (System.IO.File.Exists p) then
                None
            else
                let text = System.IO.File.ReadAllText(p).Trim()
                let parts = text.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)

                if parts.Length <> 2 then
                    None
                else
                    match
                        System.Int64.TryParse(
                            parts.[1],
                            System.Globalization.NumberStyles.HexNumber,
                            System.Globalization.CultureInfo.InvariantCulture
                        )
                    with
                    | false, _ -> None
                    | true, checksum ->
                        let name = parts.[0]

                        if Hashing.checksum (System.Text.Encoding.UTF8.GetBytes name) <> uint32 checksum then
                            None
                        else
                            parseManifestNumber name)
        |> Result.defaultValue None

    /// CURRENT の候補（A/B スロット）を新しい順に返す。
    /// MANIFEST ファイルは、これらが両方失われたときの最終手段としてだけ使う。
    let private readCurrentCandidates (dir: string) =
        [ CurrentSlotA; CurrentSlotB ]
        |> List.choose (fun slot -> readCurrentSlot dir slot |> Option.map (fun mn -> mn, slot))
        |> List.distinctBy fst
        |> List.sortByDescending fst

    /// ディスク上の MANIFEST を新しい順に返す（CURRENT が失われたときの最終手段）。
    let private readManifestCandidates (dir: string) =
        match protect (fun () -> System.IO.Directory.GetFiles(dir, ManifestPrefix + "*")) with
        | Ok files ->
            files
            |> Array.toList
            |> List.choose parseManifestNumber
            |> List.distinct
            |> List.sortDescending
            |> List.map (fun manifestNumber -> manifestNumber, "MANIFEST")
        | Error _ -> []

    /// CURRENT スロットの内容を符号化する（manifest 名 + CRC）。
    let private encodeCurrentSlot (manifestNumber: ManifestNumber) =
        let name = fileName manifestNumber
        let checksum = Hashing.checksum (System.Text.Encoding.UTF8.GetBytes name)
        sprintf "%s %08X\n" name checksum

    /// CURRENT スロットを書いて fsync する。
    let private writeCurrentSlot (dir: string) (slot: string) (manifestNumber: ManifestNumber) =
        protect (fun () ->
            let data = System.Text.Encoding.UTF8.GetBytes(encodeCurrentSlot manifestNumber)

            use fs =
                new System.IO.FileStream(
                    System.IO.Path.Combine(dir, slot),
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.None
                )

            fs.Write(data, 0, data.Length)
            fs.Flush true)

    /// 初期化で両方のスロットを同じ manifest 番号にする。
    let private writeCurrentSlotsBoth (dir: string) (manifestNumber: ManifestNumber) =
        writeCurrentSlot dir CurrentSlotA manifestNumber
        >>= fun () -> writeCurrentSlot dir CurrentSlotB manifestNumber

    /// manifest ファイルの行を読む。
    let private read (dir: string) (manifestNumber: ManifestNumber) =
        let path = manifestPath dir manifestNumber

        if System.IO.File.Exists path then
            ret (System.IO.File.ReadAllLines path |> Array.toList)
        else
            sprintf "有効な manifest が見つかりません: %s" (fileName manifestNumber)
            |> DbError.corruption
            |> err

    /// base64 の 1行から VersionEdit を復元する（チェックサム検証付き）。
    let private decodeEdit (line: string) =
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

    /// manifest の行を順にデコードし、末尾の途中行を検出する。
    [<TailCall>]
    let rec private decodeLines (acc: VersionEdit list) =
        function
        | [] -> ret (List.rev acc, false)
        | line: string :: rest ->
            if line.Trim() = "" then
                decodeLines acc rest
            else
                match decodeEdit line with
                | Ok edits -> decodeLines (List.rev edits @ acc) rest
                | Error _ when rest |> List.forall (fun l -> l.Trim() = "") -> ret (List.rev acc, true)
                | Error e -> err e

    /// manifest を保護して読み、デコード結果と末尾の状態を返す。
    let private readAll (dir: string) (manifestNumber: ManifestNumber) =
        match protect (fun () -> read dir manifestNumber) with
        | Ok(Ok lines) ->
            let hasContent = lines |> List.exists (fun line -> line.Trim() <> "")

            decodeLines [] lines
            |> Result.map (fun (edits, truncated) -> edits, truncated, hasContent)
        | Ok(Error e)
        | Error e -> err e

    [<TailCall>]
    let rec private applyHistoryLoop (current: Version) (acc: Version list) =
        function
        | [] -> List.rev (current :: acc)
        | edit :: rest -> applyHistoryLoop (VersionEdit.apply current edit) (current :: acc) rest

    /// 編集を先頭から適用し、各編集後の Version を履歴として返す。
    /// 参照 SSTable が欠けた最後の編集を巻き戻すために使う。
    let private applyHistory (edits: VersionEdit list) = applyHistoryLoop Version.Empty [] edits

    [<TailCall>]
    let rec private lastValidVersionLoop (isValid: Version -> bool) (index: int) (best: (int * Version) option) =
        function
        | [] -> best
        | version :: rest ->
            let best = if isValid version then Some(index, version) else best
            lastValidVersionLoop isValid (index + 1) best rest

    /// 履歴のうち、条件を満たす最後の Version とその位置を返す。
    let private lastValidVersion (isValid: Version -> bool) (history: Version list) =
        lastValidVersionLoop isValid 0 None history

    let private acceptable
        (isValid: Version -> bool)
        (hasSsts: bool)
        (hasContent: bool)
        (hasFileEdits: bool)
        (version: Version)
        =
        isValid version
        && (not (List.isEmpty (Version.allFiles version)) || not hasFileEdits)
        && (not (List.isEmpty (Version.allFiles version)) || not hasContent || not hasSsts)

    /// 既存の最大番号より 1つ大きい manifest 番号を返す。
    let private nextManifestNumber (dir: string) (current: ManifestNumber) =
        protect (fun () ->
            System.IO.Directory.GetFiles(dir, ManifestPrefix + "*")
            |> Array.choose parseManifestNumber
            |> Array.fold max current
            |> (+) 1L<manifestNo>)

    /// MANIFEST の 1レコード。
    /// [checksum:u32LE][VersionEdit 本体] のフレームを base64 にして 1行にする。
    /// checksum は本体全体の CRC32C（途中の完全な行の破損を検出する）。
    let private encodeEdit (edit: VersionEdit) =
        let payload = VersionEdit.encode edit

        let frame =
            Writer.build [ Writer.uint32LE (Hashing.checksum payload); Writer.writeBytes payload ]

        System.Convert.ToBase64String frame + "\n"

    /// Version 全体を新しい manifest に書き出す（スナップショット）。
    let private writeSnapshot (dir: string) (manifestNumber: ManifestNumber) (version: Version) =
        protect (fun () ->
            let data =
                {
                    AddedFiles = Version.allFiles version
                    DeletedFiles = []
                }
                |> encodeEdit
                |> System.Text.Encoding.UTF8.GetBytes

            use fs =
                new System.IO.FileStream(
                    manifestPath dir manifestNumber,
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.Read
                )

            fs.Write(data, 0, data.Length)
            fs.Flush true)

    [<TailCall>]
    let rec private tryCandidates (dir: string) (isValid: Version -> bool) (hasSsts: bool) (lastError: DbError option) =
        function
        | [] ->
            lastError
            |> Option.defaultValue (DbError.corruption "有効な MANIFEST が見つかりません")
            |> err
        | (manifestNumber, _) :: rest ->
            match readAll dir manifestNumber with
            | Error e -> tryCandidates dir isValid hasSsts (Some e) rest
            | Ok(edits, truncated, hasContent) ->
                let history = applyHistory edits

                let hasFileEdits =
                    edits
                    |> List.exists (fun edit ->
                        not (List.isEmpty edit.AddedFiles) || not (List.isEmpty edit.DeletedFiles))

                match lastValidVersion (acceptable isValid hasSsts hasContent hasFileEdits) history with
                | None -> tryCandidates dir isValid hasSsts lastError rest
                | Some(index, version) ->
                    if not truncated && index = history.Length - 1 then
                        ret (version, manifestNumber)
                    else
                        result {
                            let! nextNumber = nextManifestNumber dir manifestNumber
                            let! _ = writeSnapshot dir nextNumber version
                            let! _ = writeCurrentSlotsBoth dir nextNumber
                            return version, nextNumber
                        }

    /// ディレクトリを作る（既にあれば何もしない）。
    let ensureDirectory (dir: string) =
        protect (fun () ->
            if not (System.IO.Directory.Exists dir) then
                System.IO.Directory.CreateDirectory dir |> ignore)

    /// データベースを開く本体。
    /// [isValid] は「参照する SSTable がすべて存在するか」の判定。
    /// MANIFEST の編集履歴を先頭から適用し、条件を満たす最後の Version を選ぶ。
    /// 現在の Version が壊れていれば、その Version を新しい MANIFEST にスナップショットして復旧する。
    let private openOrCreateCore (dir: string) (isValid: Version -> bool) =
        result {
            let! _ = ensureDirectory dir
            let! hasSsts = hasSstFiles dir
            let candidates = readCurrentCandidates dir
            let fallback = readManifestCandidates dir

            match candidates with
            | [] ->
                if List.isEmpty fallback then
                    if hasSsts then
                        return! err (DbError.corruption "CURRENT が無いのに SSTable が残っています（データ損失の恐れ）")
                    else
                        let manifestNumber = 1L<manifestNo>

                        do!
                            protect (fun () ->
                                use fs =
                                    new System.IO.FileStream(
                                        manifestPath dir manifestNumber,
                                        System.IO.FileMode.Create,
                                        System.IO.FileAccess.Write,
                                        System.IO.FileShare.Read
                                    )

                                fs.Flush true)

                        do! writeCurrentSlotsBoth dir manifestNumber
                        return Version.Empty, manifestNumber
                else
                    return! tryCandidates dir isValid hasSsts None fallback
            | _ ->
                match tryCandidates dir isValid hasSsts None candidates with
                | Ok found -> return found
                | Error e when not (List.isEmpty fallback) ->
                    return! tryCandidates dir isValid hasSsts (Some e) fallback
                | Error e -> return! err e
        }

    /// データベースを開く（新規なら作成する）。
    /// 返り値は (Version, 有効な manifest 番号)。
    let openOrCreate (dir: string) = openOrCreateCore dir (fun _ -> true)

    /// 参照する SSTable の存在を検証して開く（クラッシュ復旧用）。
    let openOrCreateWith (dir: string) (isValid: Version -> bool) = openOrCreateCore dir isValid

    /// デコードした編集を Version へ順に適用する。
    let private applyAll = List.fold VersionEdit.apply Version.Empty

    /// 残っている MANIFEST が参照するファイル番号の和集合。
    /// まだ削除していない compaction 入力を孤児として退避しないために使う。
    let referencedFileNumbers (dir: string) =
        let manifests =
            match protect (fun () -> System.IO.Directory.GetFiles(dir, ManifestPrefix + "*")) with
            | Ok files -> files |> Array.toList |> List.choose parseManifestNumber
            | Error _ -> []

        manifests
        |> List.fold
            (fun acc manifestNumber ->
                match readAll dir manifestNumber with
                | Ok(edits, _, _) ->
                    let version = applyAll edits

                    Version.allFiles version
                    |> List.fold (fun set f -> Set.add f.FileNumber set) acc
                | Error _ -> acc)
            Set.empty

    [<TailCall>]
    let rec private decodeAll (dir: string) (acc: (ManifestNumber * Set<FileNumber>) list) =
        function
        | [] -> Some(List.rev acc)
        | manifestNumber :: rest ->
            match readAll dir manifestNumber with
            | Error _ -> None
            | Ok(edits, _, _) ->
                let version = applyAll edits
                let files = Version.allFiles version |> List.map _.FileNumber |> Set.ofList
                decodeAll dir ((manifestNumber, files) :: acc) rest

    let private filesOf (predicate: 'a * Set<'b> -> bool) =
        List.filter predicate
        >> List.fold (fun acc (_, files) -> Set.union acc files) Set.empty

    /// 古い MANIFEST と、その MANIFEST だけが参照している SSTable を削除する。
    /// [activeManifestNumber] は必ず保持する。
    /// どの MANIFEST か読めないときは、ファイルを消さずに何もしない。
    /// どの MANIFEST からも参照されない SSTable（真の孤児）は削除せず、
    /// 呼び出し側の検疫（`.orphans/`）に任せる。
    let collectGarbage (dir: string) (activeManifestNumber: ManifestNumber) (keepCount: int) =
        let sorted =
            match protect (fun () -> System.IO.Directory.GetFiles(dir, ManifestPrefix + "*")) with
            | Ok files -> files |> Array.toList |> List.choose parseManifestNumber
            | Error _ -> []
            |> List.sortDescending

        match decodeAll dir [] sorted with
        | None -> ()
        | Some entries ->
            let keptSet =
                sorted
                |> List.indexed
                |> List.filter (fun (index, manifestNumber) ->
                    index < keepCount || manifestNumber = activeManifestNumber)
                |> List.map snd
                |> Set.ofList

            let keptFiles =
                filesOf (fun (manifestNumber, _) -> Set.contains manifestNumber keptSet) entries

            let droppedFiles =
                filesOf (fun (manifestNumber, _) -> not (Set.contains manifestNumber keptSet)) entries

            entries
            |> List.map fst
            |> List.filter (fun manifestNumber -> not (Set.contains manifestNumber keptSet))
            |> List.iter (fun manifestNumber ->
                protect (fun () ->
                    let path = manifestPath dir manifestNumber

                    if System.IO.File.Exists path then
                        System.IO.File.Delete path)
                |> ignore)

            Set.difference droppedFiles keptFiles
            |> Set.iter (fun fileNumber ->
                protect (fun () ->
                    let path = FileMetaData.path dir fileNumber

                    if System.IO.File.Exists path then
                        System.IO.File.Delete path)
                |> ignore)

    /// CURRENT の A/B スロットを更新する。
    /// 現在の有効なスロットのうち古い方（同値なら A）を上書きする。
    /// rename を使わないので、CURRENT の耐久性が rename に依存しない。
    let private writeCurrentSlots (dir: string) (manifestNumber: ManifestNumber) =
        let a = readCurrentSlot dir CurrentSlotA
        let b = readCurrentSlot dir CurrentSlotB

        let target =
            match a, b with
            | Some x, Some y -> if x <= y then CurrentSlotA else CurrentSlotB
            | Some _, None -> CurrentSlotB
            | None, Some _ -> CurrentSlotA
            | None, None -> CurrentSlotA

        writeCurrentSlot dir target manifestNumber

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

    /// 追記前の長さと内容から、レコードが実際に書き込まれたかを判定する。
    let private recordWasWritten (path: string) (oldLength: int64) (data: byte array) (expectedLine: string) =
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

    /// 追記の失敗種別。
    type private AppendFailure =
        /// 追記前の内容が保たれている。
        | AppendRejected of DbError
        /// 追記前の長さへ戻すことにも失敗し、レコードの有無を保証できない。
        | AppendUncertain of DbError

    /// MANIFEST に 1レコード追記する。
    /// <remarks>
    /// 末尾が改行で終わっていなければ、前回の書き込みが途中で失敗したとみなして
    /// 最後の完全な行まで切り詰めてから追記する。
    /// 書き込み中に失敗した場合は、追記前の長さまで戻す。
    /// それにも失敗した場合だけ `AppendUncertain` を返す。
    /// </remarks>
    let private append (dir: string) (manifestNumber: ManifestNumber) (edit: VersionEdit) =
        let path = manifestPath dir manifestNumber

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
            let text = encodeEdit edit
            let data = System.Text.Encoding.UTF8.GetBytes text
            let expectedLine = text.TrimEnd '\n'

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
            | Error _ when recordWasWritten path oldLength data expectedLine -> Ok(oldLength + int64 data.Length)
            | Error e ->
                match rollback path oldLength with
                | Ok() -> Error(AppendRejected e)
                | Error _ -> Error(AppendUncertain e)

    /// .VersionEdit を反映し、結果を「反映済み / 未反映 / 不明」で返す。
    /// <remarks>
    /// ローテーションが必要な場合は、追記ではなく新しい MANIFEST へ Version 全体を書いて CURRENT を切り替える。
    /// CURRENT の切り替えに失敗した場合は旧 MANIFEST がそのまま残る。
    /// </remarks>
    let logAndApplyWithOutcome (dir: string) (manifestNumber: ManifestNumber) (version: Version) (edit: VersionEdit) =
        let seed = version.NextFileNumber

        let highestAdded =
            edit.AddedFiles |> List.fold (fun acc f -> max acc f.FileNumber) seed

        let used = highestAdded + 1L
        let next = VersionEdit.apply version edit
        let current = next.NextFileNumber

        let bumped =
            if used > current then
                { next with NextFileNumber = used }
            else
                next

        let recordLength =
            encodeEdit edit |> System.Text.Encoding.UTF8.GetByteCount |> int64

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
                    match writeCurrentSlots dir nextNumber with
                    | Error e -> ManifestApplyResult.NotApplied e
                    | Ok() -> ManifestApplyResult.Applied(bumped, nextNumber)
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
    let logAndApply (dir: string) (manifestNumber: ManifestNumber) (version: Version) (edit: VersionEdit) =
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
        ManifestNumber: ManifestNumber
        /// commit 時に更新される GC 境界。0 は「全版を残す」。
        ActiveSnapshot: uint64
        /// 直近の compaction の失敗（成功したら None）。
        LastCompactionError: DbError option
        /// publish のたびに増える世代番号。
        Generation: int64
    }

module internal PublishedState =
    /// 初期状態（Generation 0、GC 境界 0 = 全版を残す）。
    let initial (version: Version) (manifestNumber: ManifestNumber) =
        {
            Version = version
            ManifestNumber = manifestNumber
            ActiveSnapshot = 0UL
            LastCompactionError = None
            Generation = 0L
        }
