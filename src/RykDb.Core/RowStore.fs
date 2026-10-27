// ===========================================================================
// RowStore — テーブル・索引を keyspace 単位の LSM ツリーへ載せる層。
//
//   主キー: keyspace "t:<table>"、キーは KeyBuilder.ofValues 主キー列
//   索引  : keyspace "i:<index>"、キーは KeyBuilder.ofIndexAndPrimary
//           （末尾に主キーを持たせるので索引値 → 行が一意に決まる）
//   ディレクトリ: <root>/<base32(keyspace)>/
//
// 行は列順の SqlValue 配列。保存時 ValueCodec でバイト列に落とす。
// ===========================================================================

namespace RykDb.Core

/// 索引の定義。
type internal IndexSchema =
    {
        Name: string
        /// 索引を張る列
        Columns: string list
        /// 値が一意か（非 UNIQUE なら false）
        Unique: bool
    }

/// テーブルのスキーマ。
type internal TableSchema =
    {
        Name: string
        /// 列の定義（順序は行の並びと一致する）
        Columns: ColumnDefinition list
        /// 主キーになる列のインデックス（Columns 内の位置）
        PrimaryKey: int list
        /// この表の索引
        Indexes: IndexSchema list
    }

module internal TableSchema =
    /// 列名の重複を検証する（SQL の識別子は大文字小文字を区別しない）。
    let private validateColumnNames (columns: ColumnDefinition list) =
        match columns |> List.map _.Name |> SqlName.duplicate with
        | Some name -> err (DbError.name (sprintf "列 %s が重複しています" name))
        | None -> ret ()

    /// PRIMARY KEY の定義を1つだけ取り出す。
    /// 列レベルとテーブルレベルの両方、複数のテーブル制約はエラーにする。
    let private primaryKeyNames (c: CreateTable) =
        let tableConstraints =
            c.Constraints
            |> List.choose (fun tc ->
                match tc with
                | TableConstraint.PrimaryKeyColumns cols -> Some cols
                | _ -> None)

        let columnNames =
            c.Columns |> List.filter _.Constraint.PrimaryKey |> List.map _.Name

        match tableConstraints, columnNames with
        | [], [] -> err (sprintf "テーブル %s に主キーがありません" c.Name |> DbError.constraintViolation)
        | [ cols ], [] when cols.IsEmpty -> err (sprintf "テーブル %s の主キー列が空です" c.Name |> DbError.constraintViolation)
        | [ cols ], [] -> ret cols
        | [], [ name ] -> ret [ name ]
        | [], _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY が複数の列に指定されています" c.Name
                |> DbError.constraintViolation
            )
        | _ :: _ :: _, _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY のテーブル制約が複数あります" c.Name
                |> DbError.constraintViolation
            )
        | [ _ ], _ :: _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY が列とテーブルの両方に指定されています" c.Name
                |> DbError.constraintViolation
            )

    /// PRIMARY KEY の列が実在し、重複していないことを検証する。
    let private validatePrimaryKeyColumns (columns: ColumnDefinition list) (names: string list) =
        match SqlName.duplicate names with
        | Some name -> err (DbError.name (sprintf "PRIMARY KEY の列 %s が重複しています" name))
        | None ->
            names
            |> List.map (fun name ->
                match columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                | Some _ -> ret ()
                | None -> err (DbError.name (sprintf "主キー列 %s が定義されていません" name)))
            |> all
            |> Result.map ignore

    /// テーブルレベルの UNIQUE 制約の列が実在し、重複していないことを検証する。
    let private validateUniqueColumns (columns: ColumnDefinition list) (names: string list) =
        if List.isEmpty names then
            err (DbError.constraintViolation "UNIQUE 制約の列が空です")
        else
            match SqlName.duplicate names with
            | Some name -> err (DbError.name (sprintf "UNIQUE 制約の列 %s が重複しています" name))
            | None ->
                names
                |> List.map (fun name ->
                    match columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                    | Some _ -> ret ()
                    | None -> err (DbError.name (sprintf "UNIQUE 制約の列 %s が定義されていません" name)))
                |> all
                |> Result.map ignore

    /// 主キー列に NOT NULL と PRIMARY KEY を立てる。
    /// テーブルレベルの PRIMARY KEY でも列定義を自己完結させる。
    let private markPrimaryKeyColumns (pkNames: string list) (columns: ColumnDefinition list) =
        columns
        |> List.map (fun col ->
            if pkNames |> List.exists (fun name -> SqlName.equal name col.Name) then
                { col with
                    Constraint =
                        { col.Constraint with
                            NotNull = true
                            PrimaryKey = true
                        }
                }
            else
                col)

    /// CreateTable の AST からスキーマを作る。
    let ofCreateTable (c: CreateTable) =
        result {
            do!
                if c.Columns.IsEmpty then
                    err (sprintf "テーブル %s に列がありません" c.Name |> DbError.constraintViolation)
                else
                    ret ()

            do! validateColumnNames c.Columns
            let! pkNames = primaryKeyNames c
            do! validatePrimaryKeyColumns c.Columns pkNames

            let! () =
                c.Constraints
                |> List.choose (fun tc ->
                    match tc with
                    | TableConstraint.UniqueColumns cols -> Some cols
                    | _ -> None)
                |> List.map (validateUniqueColumns c.Columns)
                |> all
                |> Result.map ignore

            let! pk =
                pkNames
                |> List.map (fun name ->
                    match c.Columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                    | Some i -> ret i
                    | None -> err (sprintf "主キー列 %s が定義されていません" name |> DbError.name))
                |> all
                |> Result.map List.sort

            return
                {
                    Name = c.Name
                    Columns = markPrimaryKeyColumns pkNames c.Columns
                    PrimaryKey = pk
                    Indexes = []
                }
        }

    /// NOT NULL 制約と型を検証する。
    let validateRow (schema: TableSchema) (row: SqlValue array) =
        if row.Length <> schema.Columns.Length then
            err (
                sprintf "テーブル %s は %d 列ですが %d 個の値が渡されました" schema.Name schema.Columns.Length row.Length
                |> DbError.constraintViolation
            )
        else
            schema.Columns
            |> List.mapi (fun i col -> i, col)
            |> foldOk
                (fun () (i, col) ->
                    let isPrimaryKey = schema.PrimaryKey |> List.contains i

                    if row.[i] = SqlValue.Null && (col.Constraint.NotNull || isPrimaryKey) then
                        err (
                            sprintf "NOT NULL 制約に違反: %s.%s" schema.Name col.Name
                            |> DbError.constraintViolation
                        )
                    else
                        ret ())
                ()

    /// 主キー部分だけ取り出す。
    let primaryKeyOf (schema: TableSchema) (row: SqlValue array) =
        schema.PrimaryKey |> List.map (fun i -> row.[i]) |> List.toArray

    /// 主キー列名。
    let primaryKeyColumns (schema: TableSchema) =
        schema.PrimaryKey |> List.map (fun i -> schema.Columns.[i].Name) |> List.toArray

    /// 列名 → 位置。
    /// SQL の識別子は大文字小文字を区別しないので、ここで正規化する。
    let indexOf (schema: TableSchema) (column: string) =
        schema.Columns |> List.tryFindIndex (fun c -> SqlName.equal c.Name column)

    /// 列名 → 位置（見つからなければエラー）。
    let requireIndexOf (schema: TableSchema) (column: string) =
        match indexOf schema column with
        | Some i -> ret i
        | None -> err (sprintf "テーブル %s に列 %s がありません" schema.Name column |> DbError.name)

module internal RowKey =
    /// 索引の列値を取り出す。
    /// 索引列は CREATE INDEX で検証済み。
    /// 万一欠けている場合は、その列を落とす（防御）。
    let private indexValues (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.choose (fun name ->
            match TableSchema.indexOf schema name with
            | Some i -> Some row.[i]
            | None -> None)

    /// 主キーの LSM へ入れるキー。
    let primary (schema: TableSchema) (row: SqlValue array) =
        KeyBuilder.ofValues (TableSchema.primaryKeyOf schema row)

    /// 索引の LSM へ入れるキー（索引列 + 主キー列）。
    let indexKey (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        KeyBuilder.ofIndexAndPrimary (indexValues schema index row) (TableSchema.primaryKeyOf schema row)

    /// 索引キー（検索側）。
    /// row は列順の完全な行で受け取る。
    /// 列の値は「索引の列だけ」を並べたものではなく、
    /// スキーマのどの列にあるかが分かる形的（全部入り）である必要がある。
    let indexPrefix (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        indexValues schema index row |> KeyBuilder.ofValues

/// 書き込みの単位。(keyspace, key) -> 値（None は削除）。
module internal Keyspace =
    [<Literal>]
    let MaxNameBytes = 128

    let private base32Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

    /// UTF-8 バイト列を base32（RFC 4648、パディングなし）にする。
    let private encodeBase32 (bytes: byte array) =
        let rec loop (i: int) (buffer: int) (bits: int) (acc: char list) =
            if bits >= 5 then
                let shift = bits - 5
                let value = buffer >>> shift &&& 0x1F
                loop i (buffer &&& (1 <<< shift) - 1) shift (base32Alphabet.[value] :: acc)
            elif i < bytes.Length then
                loop (i + 1) (buffer <<< 8 ||| int bytes.[i]) (bits + 8) acc
            elif bits > 0 then
                let value = buffer <<< (5 - bits) &&& 0x1F
                base32Alphabet.[value] :: acc
            else
                acc

        loop 0 0 0 [] |> List.rev |> List.toArray |> System.String

    /// base32 をデコードする。不正な文字・長さ・パディングなら None。
    let private decodeBase32 (text: string) =
        let valueOf (c: char) =
            if c >= 'A' && c <= 'Z' then Some(int c - int 'A')
            elif c >= 'a' && c <= 'z' then Some(int c - int 'a')
            elif c >= '2' && c <= '7' then Some(26 + int c - int '2')
            else None

        let rec loop (i: int) (buffer: int) (bits: int) (acc: byte list) =
            if i >= text.Length then
                if bits >= 5 || buffer <> 0 then
                    None
                else
                    Some(List.rev acc |> List.toArray)
            else
                match valueOf text.[i] with
                | None -> None
                | Some value ->
                    let buffer = (buffer <<< 5) ||| value
                    let bits = bits + 5

                    if bits >= 8 then
                        let shift = bits - 8
                        let b = (buffer >>> shift) &&& 0xFF
                        loop (i + 1) (buffer &&& ((1 <<< shift) - 1)) shift (byte b :: acc)
                    else
                        loop (i + 1) buffer bits acc

        loop 0 0 0 []

    let private utf8 = System.Text.UTF8Encoding(false, true)

    let private keyspaceOfBytes (bytes: byte array) =
        try
            let keyspace = utf8.GetString bytes

            match keyspace with
            | ks when System.String.IsNullOrEmpty ks -> None
            | "__catalog__" -> Some keyspace
            | ks when
                not (ks.StartsWith("t:", System.StringComparison.Ordinal))
                && not (ks.StartsWith("i:", System.StringComparison.Ordinal))
                ->
                None
            | ks -> Some ks
        with _ ->
            None

    /// テーブルの主キー用 keyspace 名。
    let table (name: string) = "t:" + name

    /// インデックスのキー用 keyspace 名。
    let index (name: string) = "i:" + name

    /// keyspace 名をディレクトリ名に使える形にする。
    /// base32 は英大文字と数字だけで、`/` や大文字小文字だけの衝突を作らない。
    let toDirName (keyspace: string) =
        keyspace |> System.Text.Encoding.UTF8.GetBytes |> encodeBase32

    /// ディレクトリ名を keyspace 名に戻す（keyspace 名でなければ `None`）。
    let tryFromDirName (dirName: string) =
        dirName |> decodeBase32 |> Option.bind keyspaceOfBytes

    /// テーブル名・索引名の長さを検証する。
    /// ディレクトリ名（base32）が主要 OS の 255 バイト制限に収まる範囲に制限する。
    let validateName (kind: string) (name: string) =
        let bytes = System.Text.Encoding.UTF8.GetByteCount name

        if bytes > MaxNameBytes then
            err (DbError.name (sprintf "%s名 %s は長すぎます（UTF-8 で %d バイト以下にしてください）" kind name MaxNameBytes))
        else
            ret ()

/// DROP 後の keyspace 後始末を再開するためのジャーナル。
/// カタログ削除前にこれを書き、後始末が完了したら消す。
/// 起動時にカタログに存在しない keyspace だけを掃除する。
module internal DropCleanup =
    [<Literal>]
    let private fileName = ".drop-cleanup"

    let private path (root: string) = System.IO.Path.Combine(root, fileName)

    /// 未完了の後始末対象 keyspace を読む。
    let pending (root: string) =
        if System.String.IsNullOrEmpty root then
            ret []
        else
            protect (fun () ->
                let p = path root

                if not (System.IO.File.Exists p) then
                    []
                else
                    System.IO.File.ReadAllLines p
                    |> Array.filter (fun line -> line.Trim() <> "")
                    |> Array.map (fun line ->
                        System.Convert.FromBase64String line |> System.Text.Encoding.UTF8.GetString)
                    |> Array.toList)

    /// 後始末対象を書く（カタログ削除より前）。
    let write (root: string) (spaces: string list) =
        if System.String.IsNullOrEmpty root then
            ret ()
        elif List.isEmpty spaces then
            ret ()
        else
            protect (fun () ->
                let p = path root
                let tmp = p + ".tmp"

                let text =
                    spaces
                    |> List.map (fun space -> System.Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes space))
                    |> String.concat "\n"

                let data = System.Text.Encoding.UTF8.GetBytes(text + "\n")

                use fs =
                    new System.IO.FileStream(
                        tmp,
                        System.IO.FileMode.Create,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.None
                    )

                fs.Write(data, 0, data.Length)
                fs.Flush true
                fs.Close()
                System.IO.File.Move(tmp, p, true))

    /// 既存の未完了対象を残したまま、新しい後始末対象を追加する。
    let add (root: string) (spaces: string list) =
        match pending root with
        | Error e -> Error e
        | Ok existing -> write root (List.distinct (existing @ spaces))

    /// 後始末が完了したらジャーナルを消す。
    let clear (root: string) =
        if System.String.IsNullOrEmpty root then
            ret ()
        else
            protect (fun () ->
                let p = path root
                let tmp = p + ".tmp"

                if System.IO.File.Exists p then
                    System.IO.File.Delete p

                if System.IO.File.Exists tmp then
                    System.IO.File.Delete tmp)

/// keyspace を跨ぐ commit の意思決定ログ（追記型ジャーナル）。
/// <remarks>
/// 各 keyspace の WAL は独立しているため、途中でクラッシュすると
/// 「テーブルは書けたが索引が書けていない」状態になりうる。
/// そこで `.commit/journal` へ **公開（publish）** と **解決（resolve）** を 1レコードずつ追記する。
/// - 公開は「すでにあるファイルへの追記 + `Flush(true)`」だけで、
///   **ディレクトリエントリを更新しない**。
///   .NET 標準機能ではディレクトリを fsync できない（クロスプラットフォームを守るため P/Invoke は使わない）ので、
///   rename / delete に頼らない設計にする。
///   ジャーナルはオープン時に必ず作っておく（`touch`）ため、
///   公開のたびにディレクトリが汚れることもない。
/// - レコードは `[長さ][ペイロード][チェックサム]` のフレーム。
///   書き込みが途中で切れた尾部はチェックサムで検出し、**未公開として捨てる**
///   （公開の完了前に WAL へは 1 バイトも書かないので、捨てて安全）。
/// - 適用が終わったら resolve を追記する。解決済みの公開は再生対象から外れる。
/// - 再生は冪等（`applyBatchesCore idempotent = true`）。
///   resolve に失敗して公開が残っても、次回の再生では適用済みのエントリを読み飛ばすだけ。
/// </remarks>
module internal CommitIntent =
    let private dir (root: string) = System.IO.Path.Combine(root, ".commit")

    let private journalPath (root: string) =
        System.IO.Path.Combine(dir root, "journal")

    /// ジャーナルをこれ以上伸ばさない閾値（バイト）。
    /// 超えていて「未解決ゼロ」のときだけ間引く（公開記録は消さない）。
    [<Literal>]
    let private CompactThresholdBytes = 262144L

    /// 1フレームの最大長（異常な長さを弾く）。
    [<Literal>]
    let private MaxFrameBytes = 268435456L

    /// レコード種別。
    [<Literal>]
    let private TagPublished = 1uy

    [<Literal>]
    let private TagResolved = 2uy

    /// ジャーナルの 1レコード。
    type private Frame =
        /// 公開（適用開始の意思表示）。id は maxSeq + GUID。
        | Published of string * (string * Entry array) list
        /// 解決（適用完了）。
        | Resolved of string

    /// 1バッチ分の読み取り。
    let private readBatchItem (c: Cursor) =
        result {
            let! ks, c1 = Reader.readStr c
            let! batchBytes, c2 = Reader.readLengthPrefixed c1
            let! _, entries = Entry.decodeBatch batchBytes
            return (ks, entries), c2
        }

    let private encodePublish (ident: string) (batches: (string * Entry array) list) =
        let steps =
            [
                yield Writer.writeByte TagPublished
                yield Writer.str ident
                yield Writer.varint (uint64 batches.Length)

                for ks, entries in batches do
                    yield Writer.str ks
                    yield Writer.lengthPrefixed (Entry.encodeBatch 0UL entries)
            ]

        Writer.build steps

    let private encodeResolve (ident: string) =
        Writer.build [ Writer.writeByte TagResolved; Writer.str ident ]

    let private decodeFrame (data: byte array) =
        result {
            let c = Cursor.create data
            let! tag, c1 = Reader.readByte c

            match tag with
            | TagPublished ->
                let! ident, c2 = Reader.readStr c1
                let! count, c3 = Reader.readVarint c2

                if count > uint64 System.Int32.MaxValue then
                    return! err (DbError.corruption "CommitIntent: バッチ数が大きすぎます")
                else
                    let! batches, _ = Reader.readItems readBatchItem count c3
                    return Published(ident, batches)
            | TagResolved ->
                let! ident, _ = Reader.readStr c1
                return Resolved ident
            | other -> return! err (sprintf "CommitIntent: 未知のレコード種別 %d" other |> DbError.corruption)
        }

    /// フレーム 1個分のバイト列（[長さ][ペイロード][チェックサム]）。
    let private frameBytes (payload: byte array) =
        let head =
            Writer.build [ Writer.uint32LE (uint32 payload.Length); Writer.writeBytes payload ]

        let crc = Hashing.checksum head
        Array.append head (Writer.build [ Writer.uint32LE crc ])

    /// フレーム 1個を読む。
    /// `None` は「クリーンな終端」または「直前の追記が途切れた尾部」（両方とも捨てて安全）。
    let private readFrame (s: System.IO.Stream) =
        let rec fill (buf: byte array) (have: int) (want: int) =
            if have >= want then
                ret true
            else
                let got = s.Read(buf, have, want - have)
                if got <= 0 then ret false else fill buf (have + got) want

        let header = Array.zeroCreate<byte> 4
        let first = s.Read(header, 0, 4)

        if first < 4 then
            ret None
        else
            let len = uint64 (Bytes.readLE32 header 0)

            if len > uint64 MaxFrameBytes then
                ret None
            else
                let payload = Array.zeroCreate<byte>(int len)

                match fill payload 0 (int len) with
                | Error e -> Error e
                | Ok false -> ret None
                | Ok true ->
                    let crcBytes = Array.zeroCreate<byte> 4

                    match fill crcBytes 0 4 with
                    | Error e -> Error e
                    | Ok false -> ret None
                    | Ok true ->
                        let stored = Bytes.readLE32 crcBytes 0
                        let body = Array.append header payload

                        if Hashing.checksum body <> stored then
                            ret None
                        else
                            decodeFrame payload |> Result.map Some

    /// ジャーナルを読んで全フレームを返す。
    /// 返り値は (フレーム, 有効な末尾バイト位置)。尾部の途切れは読み飛ばす。
    let private readFrames (path: string) =
        protect (fun () ->
            use s = System.IO.File.OpenRead path

            let rec loop (acc: Frame list) =
                match readFrame s with
                | Error e -> Error e
                | Ok None -> ret (List.rev acc, s.Position)
                | Ok(Some frame) -> loop (frame :: acc)

            loop [])
        |> Result.bind id

    /// ジャーナルの追記・間引きを直列化する（root ごとではなくプロセス全体で 1本）。
    /// 書き込みは `RootGate` の共有ロック下で走るので、複数スレッドが同時に commit し得る。
    let private gate = new System.Threading.SemaphoreSlim(1, 1)

    /// フレームを 1つ追記してディスクへ落とす。
    let private appendFrame (root: string) (payload: byte array) =
        let frame = frameBytes payload
        gate.Wait()

        try
            protect (fun () ->
                System.IO.Directory.CreateDirectory(dir root) |> ignore

                use fs =
                    new System.IO.FileStream(
                        journalPath root,
                        System.IO.FileMode.Append,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.Read
                    )

                fs.Write(frame, 0, frame.Length)
                fs.Flush true)
        finally
            gate.Release() |> ignore

    /// ジャーナルを先に作っておく。
    /// 作成はディレクトリを 1回だけ汚す。
    /// 以後の公開は「すでにあるファイルへの追記」になり、ディレクトリエントリを更新しなくなる
    /// （標準機能でディレクトリを fsync できないため）。
    /// <remarks>
    /// 途中で切れた尾部があれば**切り詰める**。
    /// 切れたまま後ろへ追記すると、あの尾部で読みが止まり、以後の公開が全部読めなくなるため。
    /// 切り詰めは既存ファイルの長さ変更（inode は同じ）なので、ディレクトリエントリは更新しない。
    /// </remarks>
    let touch (root: string) =
        if System.String.IsNullOrEmpty root then
            ret ()
        else
            result {
                let d = dir root
                do! protect (fun () -> System.IO.Directory.CreateDirectory d |> ignore)
                let p = journalPath root

                do!
                    protect (fun () ->
                        if not (System.IO.File.Exists p) then
                            use fs =
                                new System.IO.FileStream(
                                    p,
                                    System.IO.FileMode.CreateNew,
                                    System.IO.FileAccess.Write,
                                    System.IO.FileShare.Read
                                )

                            fs.Flush true)

                do!
                    match readFrames p with
                    | Error _ -> ret ()
                    | Ok(frames, validLen) ->
                        let len =
                            match protect (fun () -> System.IO.FileInfo(p).Length) with
                            | Ok n -> n
                            | Error _ -> 0L

                        if validLen < len then
                            protect (fun () ->
                                use fs =
                                    new System.IO.FileStream(
                                        p,
                                        System.IO.FileMode.Open,
                                        System.IO.FileAccess.Write,
                                        System.IO.FileShare.Read
                                    )

                                fs.SetLength validLen
                                fs.Flush true)
                        else
                            ret ()

                return ()
            }

    /// 未解決が無いときだけジャーナルを間引く（閾値超えのとき）。
    /// <remarks>
    /// すでにあるファイルを上書き（truncate）するだけなので、ディレクトリエントリは更新しない。
    /// 間引くのは「未解決ゼロ」のときだけ。
    /// 公開記録を消すと、適用前の commit が再生できなくなる。
    /// 失敗しても致命的ではない（次回のオープンで挑戦する）ので握り潰す。
    /// </remarks>
    let trimIfIdle (root: string) =
        if not (System.String.IsNullOrEmpty root) then
            let p = journalPath root

            let oversize =
                match protect (fun () -> System.IO.FileInfo(p).Length) with
                | Ok len -> len >= CompactThresholdBytes
                | Error _ -> false

            if oversize then
                gate.Wait()

                try
                    match readFrames p with
                    | Error _ -> ()
                    | Ok(frames, _) ->
                        let solved =
                            frames
                            |> List.choose (function
                                | Resolved id -> Some id
                                | Published _ -> None)
                            |> Set.ofList

                        let unresolved =
                            frames
                            |> List.exists (function
                                | Published(id, _) -> not (Set.contains id solved)
                                | Resolved _ -> false)

                        if not unresolved then
                            protect (fun () ->
                                use fs =
                                    new System.IO.FileStream(
                                        p,
                                        System.IO.FileMode.Create,
                                        System.IO.FileAccess.Write,
                                        System.IO.FileShare.Read
                                    )

                                fs.Flush true)
                            |> ignore
                finally
                    gate.Release() |> ignore

    /// 公開レコードを追記する。
    /// 返り値は解決に使う id。
    /// <remarks>
    /// ここではまだ WAL に何も書かない。
    /// フレームが完成して `Flush(true)` が返った時点で初めて
    /// 「この commit は開始した」と外部から見える。
    /// 途中で切れた尾部はフレーム不完全として捨てられる。
    /// </remarks>
    let publish (root: string) (batches: (string * Entry array) list) =
        let maxSeq =
            batches
            |> List.collect (fun (_, entries) -> Array.toList entries)
            |> List.map _.SeqNo
            |> List.max

        let ident =
            sprintf "%s-%s" (maxSeq.ToString("D20")) (System.Guid.NewGuid().ToString("N"))

        appendFrame root (encodePublish ident batches) |> Result.map (fun () -> ident)

    /// 解決レコードを追記する（適用完了の記録）。
    /// 失敗を握り潰すと、その後に同じキーが更新・GC されたあとの再起動で
    /// 古い intent が再適用され得るので、エラーとして返す
    /// （再生は冪等なので、それでも安全側には倒れる）。
    let resolve (root: string) (ident: string) = appendFrame root (encodeResolve ident)

    /// 未解決の intent を返す（ジャーナルの公開 − 解決）。
    /// 尾部の途切れは「公開未完了」として読み飛ばされる。
    /// (id, batches) を返し、解決には同じ id を使う。
    let pending (root: string) =
        let p = journalPath root

        if System.IO.File.Exists p then
            result {
                let! frames, _ = readFrames p

                let solved =
                    frames
                    |> List.choose (function
                        | Resolved id -> Some id
                        | Published _ -> None)
                    |> Set.ofList

                return
                    frames
                    |> List.choose (function
                        | Published(id, batches) when not (Set.contains id solved) -> Some(id, batches)
                        | _ -> None)
            }
        else
            ret []

/// keyspace ごとのエンジンとスキーマを束ねたもの。
/// エンジンの開き方を差し替えられるので、テストではディスクに触れない実装を使える。
[<NoEquality; NoComparison>]
type internal RowStore =
    {
        Root: string
        /// keyspace → エンジン
        Engines: Map<string, IStorageEngine>
        /// まだ開いていない keyspace のエンジンを作る
        OpenEngine: string -> Result<IStorageEngine, DbError>
        /// プロセス間ロックのトークン。
        /// メモリ上のテスト用 store は None（ファイルを作らない）。
        Lock: obj option
    }

/// 同じ DB を二重に開かないようにする（`.rykdb.lock`）。
/// <remarks>
/// **別プロセス**の同時オープンは WAL / MANIFEST を壊すので `FileShare.None` で拒否する。
/// **同じプロセス**でも既に開いている root は二度開けない。
/// `Database.openWith` のたびに別の `RowStore`（別 memtable / WAL ビュー）ができ、
/// 一方の commit が他方から見えない（古い行を読んでしまう）ため。
/// クラッシュ再現テストだけは `takeOver = true` で意図的に二重オープンする。
/// 最後のハンドルを Close するとファイルロックも解放される。
/// </remarks>
module internal ProcessLock =
    /// root に置くロックファイル名。
    let private fileName = ".rykdb.lock"

    /// 台帳の更新だけを直列化する（ロック自体は root ごとに 1本）。
    let private gate = new System.Threading.SemaphoreSlim(1, 1)

    /// 正規化した root -> (トークン -> プロセス内で共有する FileStream)。
    let private held =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            System.Collections.Concurrent.ConcurrentDictionary<obj, System.IO.FileStream>
         >
            System.StringComparer.Ordinal

    /// ロックを取り、解除に使うトークンを返す。
    /// - 別プロセスが開いている → `IoError`
    /// - 同じプロセスが既に開いている → `takeOver` でなければ `IoError`
    let acquire (root: string) (takeOver: bool) =
        gate.Wait()

        try
            let key = RootKey.normalize root

            let bucket =
                held.GetOrAdd(
                    key,
                    fun _ -> System.Collections.Concurrent.ConcurrentDictionary<obj, System.IO.FileStream>()
                )

            let token = obj ()

            match bucket |> Seq.tryHead with
            | Some existing when takeOver ->
                bucket.[token] <- existing.Value
                ret token
            | Some _ ->
                err (DbError.create ErrorKind.IoError (sprintf "データベース %s は同じプロセスで既に開かれています。Close してから開き直してください" key))
            | None ->
                let path = System.IO.Path.Combine(key, fileName)

                match
                    protect (fun () ->
                        new System.IO.FileStream(
                            path,
                            System.IO.FileMode.OpenOrCreate,
                            System.IO.FileAccess.ReadWrite,
                            System.IO.FileShare.None
                        ))
                with
                | Ok stream ->
                    bucket.[token] <- stream
                    ret token
                | Error _ ->
                    err (DbError.create ErrorKind.IoError (sprintf "データベース %s は別のプロセスが開いています（%s）" key fileName))
        finally
            gate.Release() |> ignore

    /// ロックを解除する（二重解除は無害）。
    /// 同じ root を開いている他トークンが残っていればロックは保持したまま。
    let release (root: string) (token: obj) =
        gate.Wait()

        try
            let key = RootKey.normalize root

            match held.TryGetValue key with
            | true, bucket ->
                match bucket.TryRemove token with
                | true, stream ->
                    if bucket.IsEmpty then
                        held.TryRemove key |> ignore
                        protect (fun () -> stream.Dispose()) |> ignore
                | _ -> ()
            | _ -> ()
        finally
            gate.Release() |> ignore

module internal RowStore =
    let private ensure (store: RowStore) (keyspace: string) =
        match store.Engines |> Map.tryFind keyspace with
        | Some engine -> ret engine
        | None -> store.OpenEngine keyspace

    let private applyBatchesCore
        (idempotent: bool)
        (activeSnapshot: uint64)
        (store: RowStore)
        (batches: (string * Entry array) list)
        =
        let rec loop (engine: IStorageEngine) (rest: Entry list) (acc: Entry list) =
            match rest with
            | [] -> ret (List.rev acc |> List.toArray)
            | e :: tail ->
                match engine.GetAt (Some e.SeqNo) e.Key with
                | Error err -> Error err
                | Ok(Some existing) when existing.SeqNo = e.SeqNo && existing.Value = e.Value -> loop engine tail acc
                | Ok _ -> loop engine tail (e :: acc)

        let filterApplied (engine: IStorageEngine) (entries: Entry array) = loop engine (Array.toList entries) []

        let rec writeAll (rest: (string * Entry array) list) (current: RowStore) =
            match rest with
            | [] -> ret current
            | (ks, entries) :: tail ->
                if Array.isEmpty entries then
                    writeAll tail current
                else
                    match ensure current ks with
                    | Error err -> Error err
                    | Ok engine ->
                        let engine = engine.WithActiveSnapshot activeSnapshot

                        let filtered =
                            if idempotent then
                                filterApplied engine entries
                            else
                                ret entries

                        match filtered with
                        | Error err -> Error err
                        | Ok entries when Array.isEmpty entries -> writeAll tail current
                        | Ok entries ->
                            match engine.WriteBatch entries with
                            | Error err -> Error err
                            | Ok updated ->
                                let maxSeq = Entry.maxSeqNo entries
                                let bumped = updated.WithNextSeqNo(max updated.NextSeqNo (maxSeq + 1UL))

                                match bumped.MaybeFlush() with
                                | Error err -> Error err
                                | Ok flushed ->
                                    writeAll
                                        tail
                                        { current with
                                            Engines = Map.add ks flushed current.Engines
                                        }

        writeAll batches store

    /// keyspace ごとにバッチを適用する（intent の公開・削除はしない）。
    /// activeSnapshot は compaction の GC 境界。0 は「全版を残す」。
    let applyBatches (activeSnapshot: uint64) (store: RowStore) (batches: (string * Entry array) list) =
        applyBatchesCore false activeSnapshot store batches

    /// LSM のストアらしいディレクトリか。
    /// 作成途中の残骸や無関係なディレクトリを keyspace と誤認しないための判定。
    let private isStoreDir (dir: string) =
        match
            protect (fun () ->
                System.IO.File.Exists(System.IO.Path.Combine(dir, "CURRENT"))
                || System.IO.Directory.GetFiles(dir, Manifest.manifestPrefix + "*").Length > 0
                || System.IO.Directory.GetFiles(dir, "*" + Wal.walExtension).Length > 0)
        with
        | Ok true -> true
        | _ -> false

    /// ルート直下の keyspace ディレクトリを探す。
    let private discoverKeyspaces (root: string) =
        System.IO.Directory.GetDirectories root
        |> Array.filter isStoreDir
        |> Array.choose (fun dir -> System.IO.Path.GetFileName dir |> Keyspace.tryFromDirName)
        |> Array.toList
        |> List.distinct

    /// DB を開く（keyspace は必要に応じて作られる）。
    /// <remarks>
    /// 既存の keyspace ディレクトリはすべて開いておく。
    /// 開かないまま後始末すると、Database.openWith が「SeqNo をどこから採番するか」を知れず、
    /// LastCommitSeqNo = 0 から始まり、既にディスクにある行がすべて「未来」として見えなくなる。
    /// 同じ root が既にこのプロセスで開かれていれば `IoError`。
    /// </remarks>
    let private openStoreCore (takeOver: bool) (root: string) =
        let openDiskEngine (root: string) (keyspace: string) =
            let dir = System.IO.Path.Combine(root, Keyspace.toDirName keyspace)

            result {
                do! Manifest.ensureDirectory dir
                let! engine = StorageEngine.openDatabase dir
                return DiskStorageEngine engine :> IStorageEngine
            }

        let openEngine = openDiskEngine root

        let closeEngines (engines: Map<string, IStorageEngine>) =
            engines |> Map.toList |> List.iter (fun (_, engine) -> engine.Close() |> ignore)

        let rec openEngines (rest: string list) (acc: Map<string, IStorageEngine>) =
            match rest with
            | [] -> Ok acc
            | ks :: tail ->
                match openEngine ks with
                | Error err -> Error(err, acc)
                | Ok e -> openEngines tail (Map.add ks e acc)

        let replayPending (store: RowStore) =
            if System.String.IsNullOrEmpty store.Root then
                ret store
            else
                result {
                    let! unresolved = CommitIntent.pending store.Root

                    return!
                        unresolved
                        |> foldOk
                            (fun current (ident, batches) ->
                                result {
                                    let! next = applyBatchesCore true 0UL current batches
                                    let! () = CommitIntent.resolve store.Root ident
                                    return next
                                })
                            store
                }

        result {
            do! protect (fun () -> System.IO.Directory.CreateDirectory root |> ignore)
            do! CommitIntent.touch root
            let! token = ProcessLock.acquire root takeOver

            let! known =
                match protect (fun () -> discoverKeyspaces root) with
                | Ok keyspaces -> ret keyspaces
                | Error e ->
                    ProcessLock.release root token
                    Error e

            let! engines =
                match openEngines known Map.empty with
                | Ok opened when Map.containsKey "__catalog__" opened -> ret opened
                | Ok opened ->
                    match openEngine "__catalog__" with
                    | Ok catalogEngine -> ret (Map.add "__catalog__" catalogEngine opened)
                    | Error e ->
                        closeEngines opened
                        ProcessLock.release root token
                        Error e
                | Error(e, opened) ->
                    closeEngines opened
                    ProcessLock.release root token
                    Error e

            let store =
                {
                    Root = root
                    Engines = engines
                    OpenEngine = openEngine
                    Lock = Some token
                }

            match replayPending store with
            | Ok ready -> return ready
            | Error e ->
                closeEngines store.Engines
                ProcessLock.release root token
                return! Error e
        }

    /// DB を開く。同じ root が既にこのプロセスで開かれていれば `IoError`。
    let openStore (root: string) = openStoreCore false root

    /// 既に開いているハンドルを「クラッシュで失った」ことにして開き直す（テスト用）。
    /// 本番経路では使わない（二重に開くと memtable が分かれて古い行を読む）。
    let internal openStoreTakingOver (root: string) = openStoreCore true root

    /// エンジンの開き方を差し替えて、空の RowStore を作る（テスト用）。
    /// ディスクに触れない実装を渡せば、keyspace はすべてメモリ上に作られる。
    let openStoreWith (root: string) (openEngine: string -> Result<IStorageEngine, DbError>) =
        {
            Root = root
            Engines = Map.empty
            OpenEngine = openEngine
            Lock = None
        }

    /// keyspace のエンジン（無ければ作る）。
    let engine (store: RowStore) (keyspace: string) = ensure store keyspace

    /// その keyspace に既にエンジンまたはディレクトリがあるか。
    /// 空の keyspace を読むためにエンジンを新規作成しないための判定。
    let keyspaceExists (store: RowStore) (keyspace: string) =
        Map.containsKey keyspace store.Engines
        || (not (System.String.IsNullOrEmpty store.Root)
            && System.IO.Directory.Exists(System.IO.Path.Combine(store.Root, Keyspace.toDirName keyspace)))

    /// keyspace ごとに Entry を書き込む。
    /// batches は (keyspace, Entry 配列) の列。
    /// Entry の SeqNo は TransactionManager が採番した値をそのまま使う。
    /// keyspace ごとに 1 から採番し直すと MVCC が壊れる（同じ SeqNo が別のキーを占めてしまう）。
    /// <remarks>
    /// 複数 keyspace にまたがる書き込みは、**先に intent を追記して公開**してから適用する。
    /// 適用が途中で失敗したら公開のままエラーを返す（握り潰さない）。
    /// 次回のオープンで再生し、冪等に適用される（`replayPending`）。
    /// 適用が終われば resolve を追記して解決する。
    /// 1 keyspace の書き込みは WAL 追記自体が原子的なので intent を作らない。
    /// </remarks>
    let writeBatchesWithSnapshot (activeSnapshot: uint64) (store: RowStore) (batches: (string * Entry array) list) =
        let nonEmpty =
            batches |> List.filter (fun (_, entries) -> not (Array.isEmpty entries))

        if List.isEmpty nonEmpty then
            ret store
        elif nonEmpty.Length = 1 || System.String.IsNullOrEmpty store.Root then
            applyBatches activeSnapshot store nonEmpty
        else
            result {
                let! ident = CommitIntent.publish store.Root nonEmpty
                let! next = applyBatches activeSnapshot store nonEmpty
                let! () = CommitIntent.resolve store.Root ident
                CommitIntent.trimIfIdle store.Root
                return next
            }

    /// GC 境界を指定しない書き込み（カタログ・索引 backfill・テスト用）。
    let writeBatches (store: RowStore) (batches: (string * Entry array) list) =
        writeBatchesWithSnapshot 0UL store batches

    /// keyspace から読む（SeqNo 制限つき）。
    let getAt (store: RowStore) (keyspace: string) (key: byte array) (visible: uint64 option) =
        result {
            let! e = engine store keyspace
            let! sst = e.GetAt visible key
            return Option.bind _.Value sst
        }

    /// keyspace を prefix で走査する（主キー/索引の前方一致）。
    let scanAt (store: RowStore) (keyspace: string) (startKey: byte array) (visible: uint64 option) (limit: int) =
        result {
            let! e = engine store keyspace
            let! entries = e.ScanAt visible startKey limit

            return
                entries
                |> Seq.map (fun sst -> sst.Key, sst.Value)
                |> Seq.filter (fun (_, v) -> v.IsSome)
        }

    /// keyspace を走査する（制限なし）。
    let scan (store: RowStore) (keyspace: string) (startKey: byte array) (limit: int) =
        scanAt store keyspace startKey None limit

    [<Literal>]
    let private ClearChunkEntries = 8192

    /// keyspace の全エントリをチャンク単位で tombstone 消去する。
    /// 起動時の DROP 後始末にも使う。
    let clearKeyspaceChunks (activeSnapshot: uint64) (store: RowStore) (mgr: TransactionManager) (space: string) =
        if not (keyspaceExists store space) then
            ret (store, mgr)
        else
            scanAt store space [||] Snapshot.noLimit 0
            |> Result.bind (fun stale ->
                stale
                |> Seq.chunkBySize ClearChunkEntries
                |> foldOk
                    (fun (currentStore, currentMgr) chunk ->
                        let mgr0, txn =
                            TransactionManager.beginTxnWithLimit System.Int64.MaxValue currentMgr

                        result {
                            let! txn2 = foldOk (fun t (key, _) -> ret (Transaction.remove t space key)) txn chunk

                            let! mgr1, _, batches = TransactionManager.commit mgr0 txn2
                            let! store1 = writeBatchesWithSnapshot activeSnapshot currentStore batches
                            return store1, mgr1
                        })
                    (store, mgr))

    /// 全 keyspace の NextSeqNo の最大値 − 1（＝保存済みの SeqNo）。
    let lastSeqNo (store: RowStore) =
        store.Engines
        |> Map.toList
        |> List.fold (fun acc (_, e) -> max acc (if e.NextSeqNo = 0UL then 0UL else e.NextSeqNo - 1UL)) 0UL

    /// 全 keyspace を close し、プロセス間ロックを外す。
    /// ロックの解除は二重 Close でも無害（トークンは一度しか外れない）。
    let close (store: RowStore) =
        let outcome = store.Engines |> Map.toList |> foldOk (fun () (_, e) -> e.Close()) ()

        match store.Lock with
        | Some token -> ProcessLock.release store.Root token
        | None -> ()

        outcome

/// DML — スキーマと transaction を使う上位操作。
/// 1行の書き込みは「主キーの LSM への 1件 + 各索引の LSM への 1件」。
/// これらは異なる keyspace なので並列に commit できる。
/// 削除は主キー + 全索引に tombstone を書く
/// （書き漏らすと索引経由で消えた行が復活する）。
module internal Dml =
    /// テーブルの keyspace 名（"t:&lt;table&gt;"）。
    let private tableSpace (schema: TableSchema) = Keyspace.table schema.Name

    /// 索引 schema を登録する。
    let private indexSpace (index: IndexSchema) = Keyspace.index index.Name

    /// 旧行と新行の差分を索引へ反映する。
    /// 同じ索引キーなら上書きだけ行い、キーが変わった場合だけ旧キーを削除する。
    let private applyIndexChange
        (schema: TableSchema)
        (txn: Transaction)
        (oldRow: SqlValue array option)
        (newRow: SqlValue array option)
        =
        schema.Indexes
        |> foldOk
            (fun current index ->
                let keyFor row = RowKey.indexKey schema index row
                let space = indexSpace index

                match oldRow, newRow with
                | None, None -> ret current
                | None, Some row -> ret (Transaction.put current space (keyFor row) row)
                | Some row, None -> ret (Transaction.remove current space (keyFor row))
                | Some oldValue, Some newValue ->
                    let oldKey = keyFor oldValue
                    let newKey = keyFor newValue

                    if oldKey = newKey then
                        ret (Transaction.put current space newKey newValue)
                    else
                        let removed = Transaction.remove current space oldKey
                        ret (Transaction.put removed space newKey newValue))
            txn

    /// store の行に txn の書き込みを重ねる（ストリーミング）。
    /// <remarks>
    /// store の行も txn のバッファも**キー昇順**なので、両者を突き合わせながら 1行ずつ合成できる。
    /// 従来は「store の全行を配列化 → 差し替え → 追加分を足して再ソート」だったので、
    /// transaction 中の大テーブル走査が O(テーブル行数) のメモリを使った。
    /// ここでは store 側は 1行だけ、slot 側は txn のバッファぶんだけしか持たない。
    /// slot は 1回ずつしか消費されない（store 行と突き合わせたとき、あるいは store が尽きたとき）。
    /// </remarks>
    let private overlayRows
        (keyOf: SqlValue array -> byte array)
        (slots: (byte array * SqlValue array option) list)
        (rows: Op)
        =
        if List.isEmpty slots then
            rows
        else
            let slotArr = slots |> List.sortBy fst |> List.toArray

            let rec emitRemaining (i: int) =
                if i >= slotArr.Length then
                    Op.Done
                else
                    Op.Compute(fun () ->
                        match slotArr.[i] with
                        | _, Some v -> ret (Some(v, emitRemaining (i + 1)))
                        | _, None -> Op.next (emitRemaining (i + 1)))

            let rec merge (cur: Op) (i: int) =
                Op.Compute(fun () ->
                    if i >= slotArr.Length then
                        Op.next cur |> Result.map (Option.map (fun (r, nx) -> r, merge nx i))
                    else
                        match Op.next cur with
                        | Error e -> Error e
                        | Ok None -> Op.next (emitRemaining i)
                        | Ok(Some(row, nx)) ->
                            let rk = keyOf row
                            let sk, sv = slotArr.[i]

                            match Bytes.compare rk sk with
                            | c when c < 0 ->
                                match sv with
                                | Some v -> ret (Some(v, merge cur (i + 1)))
                                | None -> Op.next (merge cur (i + 1))
                            | 0 ->
                                match sv with
                                | Some v -> ret (Some(v, merge nx (i + 1)))
                                | None -> Op.next (merge nx (i + 1))
                            | _ ->
                                match sv with
                                | Some v -> ret (Some(v, merge cur (i + 1)))
                                | None -> Op.next (merge cur (i + 1)))

            merge rows 0

    let private storeLimitFor (limit: int) (slots: (byte array * SqlValue array option) list) =
        if limit <= 0 then
            0
        else
            let deletions = slots |> List.filter (fun (_, value) -> value.IsNone) |> List.length
            int (min (int64 System.Int32.MaxValue) (int64 limit + int64 deletions))

    let private inKeyRange (lowerBytes: byte array) (upperBytes: byte array option) (key: byte array) =
        Bytes.compare key lowerBytes >= 0
        && match upperBytes with
           | None -> true
           | Some hb -> Bytes.compare key hb <= 0

    let private inIndexRange (lowerBytes: byte array) (upperBytes: byte array option) (key: byte array) =
        inKeyRange lowerBytes upperBytes key
        || match upperBytes with
           | Some hb -> KeyBuilder.startsWithPrefix key hb
           | None -> false

    /// 主キーで既存行を取り出す（txn の書き込みを含む）。
    let getByPrimaryKeyWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        =
        let getByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (key: SqlValue array) =
            RowStore.getAt store (tableSpace schema) (KeyBuilder.ofValues key) visible

        match Transaction.tryFind txn (tableSpace schema) (KeyBuilder.ofValues key) with
        | Some slot -> ret slot
        | None -> getByPrimaryKey store schema visible key

    /// 主キーで prefix 走査する（キー昇順）。
    let scanByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (limit: int) =
        RowStore.scanAt store (tableSpace schema) [||] visible limit
        |> Result.map (Seq.map (fun (_, v) -> v |> Option.defaultValue [||]))

    /// 主キーで prefix 走査する（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`（1行ずつ引くまで store を読まない）。
    let scanByPrimaryKeyWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (limit: int)
        =
        let slots = Transaction.slotsOf txn (tableSpace schema)

        scanByPrimaryKey store schema visible (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.primary schema) slots (Op.ofSeq rows)
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 主キーの範囲走査（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByPrimaryKeyRangeWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        =
        let scanPrimaryKeyRange
            (store: RowStore)
            (schema: TableSchema)
            (visible: uint64 option)
            (lowerBytes: byte array)
            (upperBytes: byte array option)
            (limit: int)
            =
            RowStore.scanAt store (tableSpace schema) lowerBytes visible limit
            |> Result.map (fun rows ->
                rows
                |> Seq.choose (fun (key, value) ->
                    match value with
                    | Some row when inKeyRange lowerBytes upperBytes key -> Some row
                    | _ -> None)
                |> Op.ofSeq
                |> fun op -> if limit > 0 then Op.take limit op else op)

        let startBytes = KeyBuilder.ofValues startKey
        let endBytes = endKey |> Option.map KeyBuilder.ofValues

        let slots =
            Transaction.slotsOf txn (tableSpace schema)
            |> List.filter (fun (key, _) -> inKeyRange startBytes endBytes key)

        scanPrimaryKeyRange store schema visible startBytes endBytes (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let merged = overlayRows (RowKey.primary schema) slots rows

            let filtered =
                match endBytes with
                | None -> merged
                | Some eb -> Op.filter (fun row -> ret (Bytes.compare (RowKey.primary schema row) eb <= 0)) merged

            if limit > 0 then Op.take limit filtered else filtered)

    /// 索引の値に一致する全行を引く（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByIndexPrefixWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (values: SqlValue array)
        (limit: int)
        =
        let scanIndexPrefix
            (store: RowStore)
            (index: IndexSchema)
            (visible: uint64 option)
            (prefix: byte array)
            (limit: int)
            =
            RowStore.scanAt store (indexSpace index) prefix visible limit
            |> Result.map (fun rows ->
                rows
                |> Seq.takeWhile (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)
                |> Seq.choose snd
                |> Op.ofSeq
                |> fun op -> if limit > 0 then Op.take limit op else op)

        let prefix = KeyBuilder.ofValues values

        let slots =
            Transaction.slotsOf txn (indexSpace index)
            |> List.filter (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)

        scanIndexPrefix store index visible prefix (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.indexKey schema index) slots rows
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 索引の値が範囲 [lower, upper] に入る行を引く（txn の書き込みを含む）。
    /// 返り値は遅延の `Op`。
    let scanByIndexRangeWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (lower: SqlValue array)
        (upper: SqlValue array option)
        (limit: int)
        =
        let scanIndexRange
            (store: RowStore)
            (index: IndexSchema)
            (visible: uint64 option)
            (lowerBytes: byte array)
            (upperBytes: byte array option)
            (limit: int)
            =
            RowStore.scanAt store (indexSpace index) lowerBytes visible limit
            |> Result.map (fun rows ->
                rows
                |> Seq.filter (fun (key, _) -> inIndexRange lowerBytes upperBytes key)
                |> Seq.choose snd
                |> Op.ofSeq
                |> fun op -> if limit > 0 then Op.take limit op else op)

        let lowerBytes = KeyBuilder.ofValues lower
        let upperBytes = upper |> Option.map KeyBuilder.ofValues

        let slots =
            Transaction.slotsOf txn (indexSpace index)
            |> List.filter (fun (key, _) -> inIndexRange lowerBytes upperBytes key)

        scanIndexRange store index visible lowerBytes upperBytes (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let overlaid = overlayRows (RowKey.indexKey schema index) slots rows
            if limit > 0 then Op.take limit overlaid else overlaid)

    /// 1行を transaction に積む（まだ DB には入らない）。
    /// 索引のエントリも同じ transaction に入れるので、commit 原子性が保たれる。
    let insert (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
        result {
            do! TableSchema.validateRow schema row
            let tks = tableSpace schema
            let pk = RowKey.primary schema row

            match Transaction.tryFind txn tks pk with
            | Some(Some _) -> return! err (DbError.constraintViolation (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name))
            | _ -> ()

            let next = Transaction.put txn tks pk row
            let! changed = applyIndexChange schema next None (Some row)
            return! Transaction.checkLimit changed
        }

    let private replaceRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) (newRow: SqlValue array) =
        let ensureSamePrimaryKey (schema: TableSchema) (oldRow: SqlValue array) (newRow: SqlValue array) =
            if Array.forall2 (=) (TableSchema.primaryKeyOf schema oldRow) (TableSchema.primaryKeyOf schema newRow) then
                ret ()
            else
                err (
                    sprintf "テーブル %s の主キー列は更新できません（DELETE + INSERT を使ってください）" schema.Name
                    |> DbError.constraintViolation
                )

        result {
            do! ensureSamePrimaryKey schema oldRow newRow

            let afterMain =
                Transaction.put txn (tableSpace schema) (RowKey.primary schema newRow) newRow

            let! changed = applyIndexChange schema afterMain (Some oldRow) (Some newRow)
            return! Transaction.checkLimit changed
        }

    /// 主キーで更新する（主キーと全索引に新しい値を書き込む）。
    /// 読み手には 1transaction 分の原子的な書き換えとして見える。
    /// 削除してから挿入してはいけない。
    /// Transaction.Buffers は `Map<key, value option>` なので、
    /// 同一キーに「tombstone → put」と書くと最後の put だけが残る
    /// （Map の同一キーは上書きされるため tombstone が消える）。
    /// 逆に「put → tombstone」なら put が消える。
    /// LSM は「新しい SeqNo の put が古い put を隠す」ので、 put だけを書けば更新になる。
    /// 索引側は「古い索引キー → tombstone, 新しい索引キー → put」の 2件が必要。
    let updateByPrimaryKey
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        (newRow: SqlValue array)
        =
        result {
            do! TableSchema.validateRow schema newRow
            let! row = getByPrimaryKeyWithTxn store schema visible txn key

            match row with
            | None -> return! err (DbError.notFound "更新する行が見つかりません")
            | Some oldRow -> return! replaceRow schema txn oldRow newRow
        }

    /// 走査済みの行をそのまま使って更新する（主キー検索をやり直さない）。
    /// 主キーは変更できない（DELETE + INSERT を使う）。
    let updateRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) (newRow: SqlValue array) =
        result {
            do! TableSchema.validateRow schema newRow
            return! replaceRow schema txn oldRow newRow
        }

    let private removeRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) =
        result {
            let next = Transaction.remove txn (tableSpace schema) (RowKey.primary schema oldRow)
            let! changed = applyIndexChange schema next (Some oldRow) None
            return! Transaction.checkLimit changed
        }

    /// 主キーで削除する。
    /// 主キーと全索引に tombstone を書く。
    /// 元の行を取得してから索引キーを組み立てるので、NOT NULL でも壊れない。
    let deleteByPrimaryKey
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        =
        result {
            let! row = getByPrimaryKeyWithTxn store schema visible txn key

            match row with
            | None -> return! err (sprintf "削除する行が見つかりません: %s" schema.Name |> DbError.notFound)
            | Some oldRow -> return! removeRow schema txn oldRow
        }

    /// 走査済みの行をそのまま使って削除する（主キー検索をやり直さない）。
    let deleteRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) = removeRow schema txn oldRow
