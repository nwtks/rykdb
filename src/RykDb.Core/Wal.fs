// ===========================================================================
// WAL（Write-Ahead Log）— 論理ログ。ページイメージではなく更新内容を記録する。
//   Begin(txnId) / Batch(txnId, entries) / Commit(txnId) / Abort(txnId)
// を並べ、open 時に先頭から走査して memtable へ再生する。
//
//   1 レコード: [payloadLength:u32LE][checksum:u32LE][payload]
//
// 長さ・チェックサムが合わないレコードは書きかけ（torn write）として打ち切る。
// 追記は「最後に完了する」性質があるので、壊れたレコードの後ろは読まなくてよい。
// ===========================================================================

namespace RykDb.Core

open System.IO

/// WAL に記録する論理レコード。
[<RequireQualifiedAccess>]
type internal WalRecord =
    /// トランザクション開始
    | Begin of uint64
    /// 書き込みバッチ（keyspace ごとに 1 本）
    | Batch of uint64 * Entry array
    /// コミット（このトランザクションの Batch が有効になる）
    | Commit of uint64
    /// 中断（このトランザクションの Batch は捨てる）
    | Abort of uint64

/// WAL の現在位置。不変。
type internal WalState =
    {
        /// 現在の WAL ファイル番号
        FileNumber: int64
        /// 現在の WAL ファイルに書き込んだバイト数
        Size: int
    }

module internal Wal =
    [<Literal>]
    let private TagBegin = 0uy

    [<Literal>]
    let private TagBatch = 1uy

    [<Literal>]
    let private TagCommit = 2uy

    [<Literal>]
    let private TagAbort = 3uy

    let private fileName (fileNumber: int64) = sprintf "%06d.wal" fileNumber

    let path (dir: string) (fileNumber: int64) = Path.Combine(dir, fileName fileNumber)

    let private encodeEntry (e: Entry) =
        let body =
            Writer.build
                [
                    Writer.varint e.SeqNo
                    Writer.bool e.IsDelete
                    Writer.lengthPrefixed e.Key
                    match e.Value with
                    | Some values when not e.IsDelete -> ValueCodec.writeValues values
                    | _ -> id
                ]

        Writer.build [ Writer.lengthPrefixed body ]

    let private decodeEntry (data: byte array) =
        result {
            let c = Cursor.create data
            let! seqNo, c = Read.readVarint c
            let! isDelete, c = Read.readBool c
            let! key, c = Read.readLengthPrefixed c

            let! value =
                if isDelete then
                    ret None
                else
                    ValueCodec.readValues c |> Result.map (fun (values, _) -> Some values)

            return
                {
                    Key = key
                    SeqNo = seqNo
                    Value = value
                }
        }

    let private encodeBatch (txnId: uint64) (entries: Entry array) =
        entries
        |> Array.fold
            (fun w e -> Writer.writeBytes (encodeEntry e) w)
            (Writer.empty |> Writer.uint64BE txnId |> Writer.varint (uint64 entries.Length))
        |> Writer.toArray

    let private decodeBatch (data: byte array) =
        let readOne (c: Cursor) =
            Read.readLengthPrefixed c
            |> Result.bind (fun (bs, c1) -> decodeEntry bs |> Result.map (fun e -> e, c1))

        result {
            let c = Cursor.create data
            let! txnId, c = Read.readUint64BE c
            let! count, c = Read.readVarint c

            if count > uint64 System.Int32.MaxValue then
                return! Error(DbError.corruption (sprintf "Wal: バッチのエントリ数が大きすぎます (%d)" count))
            else
                let! items, _ = Read.readItems count readOne c
                return txnId, List.toArray items
        }

    let private encodePayload (r: WalRecord) =
        let txnRecord tag txnId =
            Writer.build [ Writer.writeByte tag; Writer.uint64BE txnId ]

        match r with
        | WalRecord.Begin txnId -> txnRecord TagBegin txnId
        | WalRecord.Commit txnId -> txnRecord TagCommit txnId
        | WalRecord.Abort txnId -> txnRecord TagAbort txnId
        | WalRecord.Batch(txnId, entries) ->
            Writer.build [ Writer.writeByte TagBatch; Writer.writeBytes (encodeBatch txnId entries) ]

    let private decodePayload (payload: byte array) =
        result {
            let c = Cursor.create payload
            let! tag, c = Read.readByte c

            if tag = TagBegin then
                let! txnId, _ = Read.readUint64BE c
                return WalRecord.Begin txnId
            elif tag = TagCommit then
                let! txnId, _ = Read.readUint64BE c
                return WalRecord.Commit txnId
            elif tag = TagAbort then
                let! txnId, _ = Read.readUint64BE c
                return WalRecord.Abort txnId
            elif tag = TagBatch then
                let! bs, _ = Read.readBytes (Cursor.remaining c) c
                let! txnId, entries = decodeBatch bs
                return WalRecord.Batch(txnId, entries)
            else
                return! Error(DbError.corruption (sprintf "Wal: 不明なレコード種別 %d" (int tag)))
        }

    /// フレーム 1件を読む（frame が書く [len:u32LE][checksum:u32LE][payload] の対）。
    /// 短い・チェックサム不一致・payload 不正はすべて Error（呼び出し側で打ち切り）。
    let private readFrame (c: Cursor) =
        result {
            let! len, c1 = Read.readUInt32LE c
            let! sum, c2 = Read.readUInt32LE c1
            let n = int len

            if n < 0 then
                return! Error(DbError.corruption "Wal: フレーム長が不正です")
            else
                let! payload, c3 = Read.readBytes n c2

                if Hashing.checksum payload <> sum then
                    return! Error(DbError.corruption "Wal: チェックサム不一致")
                else
                    let! r = decodePayload payload
                    return r, c3
        }

    let private readRecords (data: byte array) =
        // 壊れた/書きかけレコードで打ち切る（以降のバイトは捨てる）。
        let rec go (c: Cursor) (acc: WalRecord list) =
            match readFrame c with
            | Error _ -> List.rev acc
            | Ok(r, c') -> go c' (r :: acc)

        go (Cursor.create data) []

    /// コミットされたトランザクションのエントリをまとめて返す。
    /// 未コミット / 中断したものは捨てる。
    let private committedEntries (records: WalRecord list) =
        let rec go (recs: WalRecord list) (pending: (uint64 * Entry array) list) (acc: Entry list) =
            match recs with
            | [] -> List.rev acc |> Array.ofList
            | WalRecord.Batch(txnId, entries) :: rest -> go rest ((txnId, entries) :: pending) acc
            | WalRecord.Abort txnId :: rest -> go rest (pending |> List.filter (fun (t, _) -> t <> txnId)) acc
            | WalRecord.Commit txnId :: rest ->
                let mine =
                    pending
                    |> List.filter (fun (t, _) -> t = txnId)
                    |> List.collect (fun (_, e) -> Array.toList e)
                    |> List.rev

                go rest (pending |> List.filter (fun (t, _) -> t <> txnId)) (mine @ acc)
            | WalRecord.Begin _ :: rest -> go rest pending acc

        go records [] []

    /// WAL ファイルを読み、コミット済みエントリを返す。
    let read (dir: string) (fileNumber: int64) =
        protect (fun () ->
            let p = path dir fileNumber

            if File.Exists p then
                File.ReadAllBytes p |> readRecords |> committedEntries
            else
                [||])

    let private frame (payload: byte array) =
        Writer.build
            [
                Writer.uint32LE (uint32 payload.Length)
                Writer.uint32LE (Hashing.checksum payload)
                Writer.writeBytes payload
            ]

    /// レコード列を追記する。
    /// Begin / Batch / Commit をまとめて 1 回の書き込みで書き出すので、
    /// コミット済みトランザクションが中途半端にならない。
    let append (state: WalState) (dir: string) (records: WalRecord list) =
        let bytes =
            records
            |> List.map (fun r -> frame (encodePayload r))
            |> List.toArray
            |> Array.concat

        protect (fun () ->
            use fs =
                new FileStream(path dir state.FileNumber, FileMode.Append, FileAccess.Write, FileShare.Read)

            if bytes.Length > 0 then
                fs.Write(bytes, 0, bytes.Length)

            fs.Flush true

            { state with
                Size = state.Size + bytes.Length
            })

    /// ディレクトリ内の WAL ファイル名（復旧時に孤児を消すのに使う）。
    let walFiles (dir: string) =
        if Directory.Exists dir then
            Directory.GetFiles(dir, "*.wal") |> Array.toList
        else
            []

    /// WAL ファイルを作る。
    let create (dir: string) (fileNumber: int64) =
        protect (fun () ->
            File.WriteAllBytes(path dir fileNumber, [||])
            { FileNumber = fileNumber; Size = 0 })

    /// 新しい WAL ファイル番号に切り替える（flush 後に呼ぶ）。
    let rotate (state: WalState) =
        {
            FileNumber = state.FileNumber + 1L
            Size = 0
        }
