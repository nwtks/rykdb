// ===========================================================================
// WAL（Write-Ahead Log）— 論理ログ。
// ページイメージではなく更新内容を記録する。
//   Begin(txnId) / Batch(txnId, entries) / Commit(txnId) / Abort(txnId)
// を並べ、open 時に先頭から走査して memtable へ再生する。
//
//   1レコード: [payloadLength:u32LE][checksum:u32LE][payload]
//
// 長さ・チェックサムが合わないレコードは書きかけ（torn write）として打ち切る。
// 追記は「最後に完了する」性質があるので、壊れたレコードの後ろは読まなくてよい。
// ===========================================================================

namespace RykDb.Core

/// WAL に記録する論理レコード。
[<RequireQualifiedAccess>]
type internal WalRecord =
    /// トランザクション開始
    | Begin of uint64
    /// 書き込みバッチ（keyspace ごとに 1本）
    | Batch of uint64 * Entry array
    /// コミット（このトランザクションの Batch が有効になる）
    | Commit of uint64
    /// 中断（このトランザクションの Batch は捨てる）
    | Abort of uint64

/// WAL の現在位置。
type internal WalState =
    {
        /// 現在の WAL ファイル番号
        FileNumber: int64
        /// 現在の WAL ファイルに書き込んだバイト数
        Size: int
    }

module internal Wal =
    /// WAL ファイルの拡張子。
    [<Literal>]
    let internal walExtension = ".wal"

    [<Literal>]
    let private TagBegin = 0uy

    [<Literal>]
    let private TagBatch = 1uy

    [<Literal>]
    let private TagCommit = 2uy

    [<Literal>]
    let private TagAbort = 3uy

    let private encodePayload (r: WalRecord) =
        let txnRecord tag txnId =
            Writer.build [ Writer.writeByte tag; Writer.uint64BE txnId ]

        match r with
        | WalRecord.Begin txnId -> txnRecord TagBegin txnId
        | WalRecord.Commit txnId -> txnRecord TagCommit txnId
        | WalRecord.Abort txnId -> txnRecord TagAbort txnId
        | WalRecord.Batch(txnId, entries) ->
            Writer.build
                [
                    Writer.writeByte TagBatch
                    Writer.writeBytes (Entry.encodeBatch txnId entries)
                ]

    let private decodePayload (payload: byte array) =
        result {
            let c = Cursor.create payload
            let! tag, c = Reader.readByte c

            match tag with
            | TagBegin ->
                let! txnId, _ = Reader.readUint64BE c
                return WalRecord.Begin txnId
            | TagCommit ->
                let! txnId, _ = Reader.readUint64BE c
                return WalRecord.Commit txnId
            | TagAbort ->
                let! txnId, _ = Reader.readUint64BE c
                return WalRecord.Abort txnId
            | TagBatch ->
                let! bs, _ = Reader.readBytes (Cursor.remaining c) c
                let! txnId, entries = Entry.decodeBatch bs
                return WalRecord.Batch(txnId, entries)
            | _ -> return! err (sprintf "Wal: 不明なレコード種別 %d" (int tag) |> DbError.corruption)
        }

    let encodeFrames (records: WalRecord list) =
        let frame (payload: byte array) =
            Writer.build
                [
                    Writer.uint32LE (uint32 payload.Length)
                    Writer.uint32LE (Hashing.checksum payload)
                    Writer.writeBytes payload
                ]

        records |> List.map (encodePayload >> frame) |> List.toArray |> Array.concat

    let decodeFrames (data: byte array) =
        let readFrame (c: Cursor) =
            result {
                let! len, c1 = Reader.readUInt32LE c
                let! sum, c2 = Reader.readUInt32LE c1
                let n = int len

                if n < 0 then
                    return! err (DbError.corruption "Wal: フレーム長が不正です")
                else
                    let! payload, c3 = Reader.readBytes n c2

                    if Hashing.checksum payload <> sum then
                        return! err (DbError.corruption "Wal: チェックサム不一致")
                    else
                        let! r = decodePayload payload
                        return r, c3
            }

        let rec decodeAt (c: Cursor) (acc: WalRecord list) =
            if Cursor.atEnd c then
                ret (List.rev acc)
            elif Cursor.remaining c < 8 then
                ret (List.rev acc)
            else
                match Reader.readUInt32LE c with
                | Error e -> Error e
                | Ok(length, _) when uint64 length > uint64 (Cursor.remaining c - 8) -> ret (List.rev acc)
                | _ ->
                    match readFrame c with
                    | Error e -> Error e
                    | Ok(record, next) -> decodeAt next (record :: acc)

        decodeAt (Cursor.create data) []

    /// 1レコードを「未コミットの Batch」と「コミット済み Entry」の状態へ反映する。
    let private applyCommitted (record: WalRecord) (pending: (uint64 * Entry array) list, acc: Entry list) =
        match record with
        | WalRecord.Begin _ -> pending, acc
        | WalRecord.Batch(txnId, entries) -> (txnId, entries) :: pending, acc
        | WalRecord.Abort txnId -> pending |> List.filter (fun (t, _) -> t <> txnId), acc
        | WalRecord.Commit txnId ->
            let mine =
                pending
                |> List.filter (fun (t, _) -> t = txnId)
                |> List.collect (fun (_, e) -> Array.toList e)
                |> List.rev

            pending |> List.filter (fun (t, _) -> t <> txnId), mine @ acc

    let committedEntries (records: WalRecord list) =
        records
        |> List.fold (fun state record -> applyCommitted record state) ([], [])
        |> snd
        |> List.rev
        |> Array.ofList

    let private fileName (fileNumber: int64) =
        sprintf "%06d%s" fileNumber walExtension

    let path (dir: string) (fileNumber: int64) =
        System.IO.Path.Combine(dir, fileName fileNumber)

    /// レコード列を追記する。
    /// Begin / Batch / Commit をまとめて 1回の書き込みで書き出すので、
    /// コミット済みトランザクションが中途半端にならない。
    let append (state: WalState) (dir: string) (records: WalRecord list) =
        let bytes = encodeFrames records

        protect (fun () ->
            if bytes.Length > 0 then
                use fs =
                    new System.IO.FileStream(
                        path dir state.FileNumber,
                        System.IO.FileMode.Append,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.Read
                    )

                fs.Write(bytes, 0, bytes.Length)
                fs.Flush true

            { state with
                Size = state.Size + bytes.Length
            })

    /// バッファへ要求サイズまで読む。末尾に達したら読めたバイト数を返す。
    let private readAtMost (fs: System.IO.FileStream) (buffer: byte array) =
        let rec loop offset =
            if offset >= buffer.Length then
                offset
            else
                let read = fs.Read(buffer, offset, buffer.Length - offset)

                if read <= 0 then offset else loop (offset + read)

        loop 0

    /// ストリームから 1フレーム読む。書きかけの末尾は `None`（そこで打ち切り）。
    let private readFrameFrom (fs: System.IO.FileStream) =
        let header = Array.zeroCreate<byte> 8
        let headerBytes = readAtMost fs header

        if headerBytes < header.Length then
            ret None
        else
            let length = uint64 (Bytes.readLE32 header 0)
            let checksum = Bytes.readLE32 header 4
            let remaining = fs.Length - fs.Position

            if length > uint64 remaining then
                ret None
            elif length > uint64 System.Int32.MaxValue then
                err (DbError.corruption "Wal: フレーム長が不正です")
            else
                let payload = Array.zeroCreate<byte>(int length)
                let payloadBytes = readAtMost fs payload

                if payloadBytes < payload.Length then
                    ret None
                elif Hashing.checksum payload <> checksum then
                    err (DbError.corruption "Wal: チェックサム不一致")
                else
                    decodePayload payload |> Result.map Some

    let private foldFrames (fs: System.IO.FileStream) (initial: 's) (step: 's -> WalRecord -> Result<'s, DbError>) =
        let rec loop state =
            match readFrameFrom fs with
            | Error e -> Error e
            | Ok None -> Ok state
            | Ok(Some record) ->
                match step state record with
                | Error e -> Error e
                | Ok next -> loop next

        loop initial

    /// WAL ファイルをストリーミングで読み、コミット済みエントリを返す。
    /// 1フレームずつ読むので、WAL 全体をメモリへ載せない。
    let read (dir: string) (fileNumber: int64) =
        let p = path dir fileNumber

        protect (fun () -> System.IO.File.Exists p)
        >>= fun exists ->
            if exists then
                protect (fun () ->
                    use fs =
                        new System.IO.FileStream(
                            p,
                            System.IO.FileMode.Open,
                            System.IO.FileAccess.Read,
                            System.IO.FileShare.Read
                        )

                    foldFrames fs ([], []) (fun state record -> ret (applyCommitted record state))
                    |> Result.map (fun (_, acc) -> List.rev acc |> Array.ofList))
                |> Result.bind id
            else
                ret [||]

    /// コミット済みエントリをメモリに溜めず、1件ずつ `step` へ渡して再生する。
    /// 大きな WAL でもコミット済みエントリの配列を保持しない。
    let foldCommitted (dir: string) (fileNumber: int64) (initial: 's) (step: 's -> Entry -> 's) =
        let p = path dir fileNumber

        let applyRecord (pending, state) record =
            match record with
            | WalRecord.Begin _ -> pending, state
            | WalRecord.Batch(txnId, entries) -> (txnId, entries) :: pending, state
            | WalRecord.Abort txnId -> pending |> List.filter (fun (t, _) -> t <> txnId), state
            | WalRecord.Commit txnId ->
                let mine =
                    pending
                    |> List.filter (fun (t, _) -> t = txnId)
                    |> List.collect (fun (_, e) -> Array.toList e)
                    |> List.rev

                pending |> List.filter (fun (t, _) -> t <> txnId), List.fold step state mine

        protect (fun () ->
            if not (System.IO.File.Exists p) then
                ret initial
            else
                use fs =
                    new System.IO.FileStream(
                        p,
                        System.IO.FileMode.Open,
                        System.IO.FileAccess.Read,
                        System.IO.FileShare.Read
                    )

                foldFrames fs ([], initial) (fun state record -> ret (applyRecord state record))
                |> Result.map snd)
        |> Result.bind id

    /// ディレクトリ内の WAL ファイル名（復旧時に孤児を消すのに使う）。
    let walFiles (dir: string) =
        if System.IO.Directory.Exists dir then
            System.IO.Directory.GetFiles(dir, "*" + walExtension) |> Array.toList
        else
            []

    /// WAL ファイルを作る。
    let create (dir: string) (fileNumber: int64) =
        protect (fun () ->
            System.IO.File.WriteAllBytes(path dir fileNumber, [||])
            { FileNumber = fileNumber; Size = 0 })

    /// 新しい WAL ファイル番号に切り替える（flush 後に呼ぶ）。
    let rotate (state: WalState) =
        {
            FileNumber = state.FileNumber + 1L
            Size = 0
        }
