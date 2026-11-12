// ===========================================================================
// WAL（Write-Ahead Log）— 論理ログ。
// ページイメージではなく更新内容を記録する。
//   Begin(txnId) / Batch(txnId, entries) / Commit(txnId) / Abort(txnId)
// を並べ、open 時に先頭から走査して memtable へ再生する。
//
//   1レコード: [payloadLength:u32LE][checksum:u32LE(CRC32C)][payload]
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
    let WalExtension = ".wal"

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
                    Entry.encodeBatch txnId entries |> Writer.writeBytes
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

    let path (dir: string) (fileNumber: int64) =
        let fileName fileNumber =
            sprintf "%06d%s" fileNumber WalExtension

        System.IO.Path.Combine(dir, fileName fileNumber)

    /// レコード列を追記する。
    /// 1レコード（= 1フレーム）ずつ書き出すので、全フレームを 1本の配列に連結しない
    /// （大きな transaction でピークが約 2倍にならない）。
    /// Begin / Batch / Commit がフレーム単位で完結しているので、
    /// プロセスが途中で落ちてもコミット済みトランザクションが中途半端にならない
    /// （未 commit の Batch は再生時に捨てられ、書きかけのフレームは CRC で検出される）。
    let private appendCore (sync: bool) (dir: string) (records: WalRecord list) (state: WalState) =
        let headerOf (payload: byte array) =
            Writer.build
                [
                    uint32 payload.Length |> Writer.uint32LE
                    Hashing.checksum payload |> Writer.uint32LE
                ]

        let rec writeAll (fs: System.IO.FileStream) written =
            function
            | [] -> written
            | record :: rest ->
                let payload = encodePayload record
                let header = headerOf payload
                fs.Write(header, 0, header.Length)
                fs.Write(payload, 0, payload.Length)
                writeAll fs (written + header.Length + payload.Length) rest

        match records with
        | [] -> ret state
        | _ ->
            protect (fun () ->
                use fs =
                    new System.IO.FileStream(
                        path dir state.FileNumber,
                        System.IO.FileMode.Append,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.ReadWrite
                    )

                let written = writeAll fs 0 records
                fs.Flush sync

                { state with
                    Size = state.Size + written
                })

    /// レコード列を追記して fsync まで行う。
    let append = appendCore true

    /// レコード列を追記するが fsync は呼び出し側に任せる。
    /// Session のグループコミットで、複数の commit を 1回の fsync にまとめるために使う。
    let appendDeferred = appendCore false

    /// WAL の現在位置まで fsync する。
    /// rotate で古い WAL が削除済みの場合は、削除前に同期済みなので成功として扱う。
    let sync (dir: string) (state: WalState) =
        let walPath = path dir state.FileNumber

        let syncCore () =
            protect (fun () ->
                use fs =
                    new System.IO.FileStream(
                        walPath,
                        System.IO.FileMode.Open,
                        System.IO.FileAccess.ReadWrite,
                        System.IO.FileShare.ReadWrite
                    )

                fs.Flush true)

        if not (System.IO.File.Exists walPath) then
            ret ()
        else
            match syncCore () with
            | Error _ when not (System.IO.File.Exists walPath) -> ret ()
            | result -> result

    /// コミット済みエントリをメモリに溜めず、1件ずつ `step` へ渡して再生する。
    /// 大きな WAL でもコミット済みエントリの配列を保持しない。
    /// 戻り値は `(再生後の状態, WAL に現れた最大 txnId)`。
    /// 最大 txnId は open 時に NextTxnId を復元し、既存ファイル内での採番の再利用を防ぐために返す。
    let foldCommitted (dir: string) (fileNumber: int64) (step: 's -> Entry -> 's) (initial: 's) =
        let readAtMost (fs: System.IO.FileStream) (buffer: byte array) =
            let rec loop offset =
                if offset >= buffer.Length then
                    offset
                else
                    let read = fs.Read(buffer, offset, buffer.Length - offset)
                    if read <= 0 then offset else loop (offset + read)

            loop 0

        let readFrameFrom fs =
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

        let foldFrames fs step initial =
            let rec loop state =
                match readFrameFrom fs with
                | Error e -> err e
                | Ok None -> ret state
                | Ok(Some record) ->
                    match step state record with
                    | Error e -> err e
                    | Ok next -> loop next

            loop initial

        let applyRecord (pending, maxTxnId, state) =
            function
            | WalRecord.Begin txnId -> pending, max maxTxnId txnId, state
            | WalRecord.Batch(txnId, entries) -> (txnId, entries) :: pending, max maxTxnId txnId, state
            | WalRecord.Abort txnId -> pending |> List.filter (fun (t, _) -> t <> txnId), max maxTxnId txnId, state
            | WalRecord.Commit txnId ->
                let mine =
                    pending
                    |> List.filter (fun (t, _) -> t = txnId)
                    |> List.collect (fun (_, e) -> Array.toList e)
                    |> List.rev

                pending |> List.filter (fun (t, _) -> t <> txnId), max maxTxnId txnId, List.fold step state mine

        let p = path dir fileNumber

        protect (fun () ->
            if not (System.IO.File.Exists p) then
                ret (initial, 0UL)
            else
                use fs =
                    new System.IO.FileStream(
                        p,
                        System.IO.FileMode.Open,
                        System.IO.FileAccess.Read,
                        System.IO.FileShare.Read
                    )

                foldFrames fs (fun state record -> ret (applyRecord state record)) ([], 0UL, initial)
                |> Result.map (fun (_, maxTxnId, state) -> state, maxTxnId))
        >>= id

    /// ディレクトリ内の WAL ファイル名（復旧時に孤児を消すのに使う）。
    let walFiles (dir: string) =
        if System.IO.Directory.Exists dir then
            System.IO.Directory.GetFiles(dir, "*" + WalExtension) |> Array.toList
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
