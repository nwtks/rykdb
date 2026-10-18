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
    [<Literal>]
    let private TagBegin = 0uy

    [<Literal>]
    let private TagBatch = 1uy

    [<Literal>]
    let private TagCommit = 2uy

    [<Literal>]
    let private TagAbort = 3uy

    let private fileName (fileNumber: int64) = sprintf "%06d.wal" fileNumber

    let path (dir: string) (fileNumber: int64) =
        System.IO.Path.Combine(dir, fileName fileNumber)

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

    /// レコード列を追記する。
    /// Begin / Batch / Commit をまとめて 1回の書き込みで書き出すので、
    /// コミット済みトランザクションが中途半端にならない。
    let append (state: WalState) (dir: string) (records: WalRecord list) =
        let frame (payload: byte array) =
            Writer.build
                [
                    Writer.uint32LE (uint32 payload.Length)
                    Writer.uint32LE (Hashing.checksum payload)
                    Writer.writeBytes payload
                ]

        let bytes =
            records |> List.map (encodePayload >> frame) |> List.toArray |> Array.concat

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

    /// WAL ファイルを読み、コミット済みエントリを返す。
    let read (dir: string) (fileNumber: int64) =
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

        /// 壊れた/書きかけレコードで打ち切る（以降のバイトは捨てる）。
        let rec readFrames (c: Cursor) (acc: WalRecord list) =
            match readFrame c with
            | Error _ -> List.rev acc
            | Ok(r, c') -> readFrames c' (r :: acc)

        let readRecords (data: byte array) = readFrames (Cursor.create data) []

        let rec selectCommitted (recs: WalRecord list) (pending: (uint64 * Entry array) list) (acc: Entry list) =
            match recs with
            | [] -> List.rev acc |> Array.ofList
            | WalRecord.Batch(txnId, entries) :: rest -> selectCommitted rest ((txnId, entries) :: pending) acc
            | WalRecord.Abort txnId :: rest ->
                selectCommitted rest (pending |> List.filter (fun (t, _) -> t <> txnId)) acc
            | WalRecord.Commit txnId :: rest ->
                let mine =
                    pending
                    |> List.filter (fun (t, _) -> t = txnId)
                    |> List.collect (fun (_, e) -> Array.toList e)
                    |> List.rev

                selectCommitted rest (pending |> List.filter (fun (t, _) -> t <> txnId)) (mine @ acc)
            | WalRecord.Begin _ :: rest -> selectCommitted rest pending acc

        let committedEntries (records: WalRecord list) = selectCommitted records [] []

        protect (fun () ->
            let p = path dir fileNumber

            if System.IO.File.Exists p then
                System.IO.File.ReadAllBytes p |> readRecords |> committedEntries
            else
                [||])

    /// ディレクトリ内の WAL ファイル名（復旧時に孤児を消すのに使う）。
    let walFiles (dir: string) =
        if System.IO.Directory.Exists dir then
            System.IO.Directory.GetFiles(dir, "*.wal") |> Array.toList
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
