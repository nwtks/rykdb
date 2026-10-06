// ===========================================================================
// WAL（Write-Ahead Log）— 論理ログ。ページイメージではなく更新内容を記録する。
//   Begin(txnId) / Batch(txnId, entries) / Commit(txnId) / Abort(txnId)
// を並べ、open 時に先頭から走査して memtable へ再生する。
//
//   1 レコード: [payloadLength:u32LE][checksum:u32LE][payload]
//
// 長さ・チェックサムが合わないレコードは書きかけ（torn write）として打ち切る。
// 追記は「最後に完了する」性質があるので、壊れたレコードの後ろは読まなくてよい。
// 詳細は docs/architecture.md §9.6。
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

    let private checksum (data: byte array) =
        Array.fold (fun h b -> (h ^^^ uint32 b) * 16777619u) 2166136261u data

    let private le32 (v: uint32) =
        [|
            for i in 0..3 do
                yield byte (v >>> 8 * i)
        |]

    let private readLE32 (data: byte array) (offset: int) =
        let at (i: int) = uint32 data.[offset + i]
        at 0 ||| (at 1 <<< 8) ||| (at 2 <<< 16) ||| (at 3 <<< 24)

    let private encodeEntry (e: Entry) =
        let body =
            Writer.empty
            |> Writer.varint e.SeqNo
            |> fun w -> Writer.bool e.IsDelete w
            |> fun w -> Writer.lengthPrefixed e.Key w
            |> fun w ->
                match e.Value, e.IsDelete with
                | Some values, false -> ValueCodec.writeValues values w
                | _ -> w
            |> Writer.toArray

        Writer.empty
        |> fun w -> Writer.writeBytes (Writer.toArray (Writer.lengthPrefixed body w)) w
        |> Writer.toArray

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
        let decodeEntries (count: uint64) (start: Cursor) =
            let rec go (i: uint64) (acc: Entry list) (c: Cursor) =
                if i >= count then
                    List.rev acc |> Array.ofList |> ret
                else
                    match Read.readLengthPrefixed c with
                    | Error e -> Error e
                    | Ok(bs, c1) ->
                        match decodeEntry bs with
                        | Error e -> Error e
                        | Ok e -> go (i + 1UL) (e :: acc) c1

            go 0UL [] start

        result {
            let c = Cursor.create data
            let! txnId, c = Read.readUint64BE c
            let! count, c = Read.readVarint c

            if count > uint64 System.Int32.MaxValue then
                return! Error(DbError.corruption (sprintf "Wal: バッチのエントリ数が大きすぎます (%d)" count))
            else
                let! entries = decodeEntries count c
                return txnId, entries
        }

    let private encodePayload (r: WalRecord) =
        match r with
        | WalRecord.Begin txnId ->
            Writer.empty
            |> Writer.writeByte TagBegin
            |> fun w -> Writer.uint64BE txnId w
            |> Writer.toArray
        | WalRecord.Commit txnId ->
            Writer.empty
            |> Writer.writeByte TagCommit
            |> fun w -> Writer.uint64BE txnId w
            |> Writer.toArray
        | WalRecord.Abort txnId ->
            Writer.empty
            |> Writer.writeByte TagAbort
            |> fun w -> Writer.uint64BE txnId w
            |> Writer.toArray
        | WalRecord.Batch(txnId, entries) ->
            Writer.empty
            |> Writer.writeByte TagBatch
            |> fun w -> Writer.writeBytes (encodeBatch txnId entries) w
            |> Writer.toArray

    let private frame (payload: byte array) =
        Array.concat [| le32 (uint32 payload.Length); le32 (checksum payload); payload |]

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

    let private readRecords (data: byte array) =
        let rec go (pos: int) (acc: WalRecord list) =
            if pos + 8 > data.Length then
                List.rev acc, pos
            else
                let len = int (readLE32 data pos)
                let sum = readLE32 data (pos + 4)

                if len < 0 || pos + 8 + len > data.Length then
                    List.rev acc, pos
                else
                    let payload = Array.sub data (pos + 8) len

                    if checksum payload <> sum then
                        List.rev acc, pos
                    else
                        match decodePayload payload with
                        | Ok r -> go (pos + 8 + len) (r :: acc)
                        | Error _ -> List.rev acc, pos

        go 0 []

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
                File.ReadAllBytes p |> readRecords |> fst |> committedEntries
            else
                [||])

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

    /// 新しい WAL ファイル番号に切り替える（flush 後に呼ぶ）。
    let rotate (state: WalState) =
        {
            FileNumber = state.FileNumber + 1L
            Size = 0
        }
