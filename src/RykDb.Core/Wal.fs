// ===========================================================================
// WAL（Write-Ahead Log）— 論理ログ。
// ページイメージではなく更新内容を記録する。
//   Begin(txnId) / Batch(txnId, entries) / Commit(txnId) / Abort(txnId)
// を並べ、open 時に先頭から走査して memtable へ再生する。
//
//   1レコード: [payloadLength:u32LE][checksum:u32LE(CRC32C)][payload]
//
// 長さが合わないレコードは書きかけ（torn write）として打ち切り、
// チェックサム不一致は CorruptionError にする。
// 壊れたレコードの後ろは読まずに打ち切る。
// ===========================================================================

namespace RykDb.Core

/// WAL に記録する論理レコード。
/// transaction 番号は単位 `txnId` を持つ
/// （`SeqNo` の素の `uint64` と取り違えないため）。
/// バイト列への直列化は `encodePayload` / `decodePayload` でだけ行う。
[<RequireQualifiedAccess>]
type internal WalRecord =
    /// トランザクション開始
    | Begin of TxnId
    /// 書き込みバッチ（keyspace ごとに 1本）
    | Batch of TxnId * Entry array
    /// コミット（このトランザクションの Batch が有効になる）
    | Commit of TxnId
    /// 中断（このトランザクションの Batch は捨てる）
    | Abort of TxnId

/// WAL の現在位置。
type internal WalState =
    {
        /// 現在の WAL ファイル番号
        FileNumber: int64
        /// 現在の WAL ファイルに書き込んだバイト数
        Size: int64
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

    /// タグと txnId だけから成るレコードを符号化する。
    let private txnRecord (tag: byte) (txnId: TxnId) =
        Writer.build [ Writer.writeByte tag; Writer.uint64BE (uint64 txnId) ]

    let private encodePayload (r: WalRecord) =
        match r with
        | WalRecord.Begin txnId -> ret (txnRecord TagBegin txnId)
        | WalRecord.Commit txnId -> ret (txnRecord TagCommit txnId)
        | WalRecord.Abort txnId -> ret (txnRecord TagAbort txnId)
        | WalRecord.Batch(txnId, entries) ->
            Entry.encodeBatch txnId entries
            |> Result.map (fun bytes -> Writer.build [ Writer.writeByte TagBatch; Writer.writeBytes bytes ])

    let private decodePayload (payload: byte array) =
        result {
            let c = Cursor.create payload
            let! tag, c = Reader.readByte c

            match tag with
            | TagBegin ->
                let! raw, _ = Reader.readUint64BE c
                return WalRecord.Begin(LanguagePrimitives.UInt64WithMeasure<txnId> raw)
            | TagCommit ->
                let! raw, _ = Reader.readUint64BE c
                return WalRecord.Commit(LanguagePrimitives.UInt64WithMeasure<txnId> raw)
            | TagAbort ->
                let! raw, _ = Reader.readUint64BE c
                return WalRecord.Abort(LanguagePrimitives.UInt64WithMeasure<txnId> raw)
            | TagBatch ->
                let! bs, _ = Reader.readBytes (Cursor.remaining c) c
                let! txnId, entries = Entry.decodeBatch bs
                return WalRecord.Batch(txnId, entries)
            | _ -> return! err (sprintf "Wal: 不明なレコード種別 %d" (int tag) |> DbError.corruption)
        }

    /// ファイル番号から WAL ファイル名を作る。
    let private walFileName (fileNumber: int64) =
        sprintf "%06d%s" fileNumber WalExtension

    let path (dir: string) (fileNumber: int64) =
        System.IO.Path.Combine(dir, walFileName fileNumber)

    /// レコード列を追記する。
    /// 1レコード（= 1フレーム）ずつ書き出すので、全フレームを 1本の配列に連結しない
    /// （大きな transaction でピークが約 2倍にならない）。
    /// Begin / Batch / Commit がフレーム単位で完結しているので、
    /// プロセスが途中で落ちてもコミット済みトランザクションが中途半端にならない
    /// （未 commit の Batch は再生時に捨てられ、書きかけのフレームは CRC で検出される）。
    /// フレームヘッダ（長さ + CRC32C）を符号化する。
    let private headerOf (payload: byte array) =
        Writer.build
            [
                uint32 payload.Length |> Writer.uint32LE
                Hashing.checksum payload |> Writer.uint32LE
            ]

    /// レコードを 1フレームずつ書き出し、書き込んだバイト数を返す。
    [<TailCall>]
    let rec private writeAll (fs: System.IO.FileStream) (written: int) =
        function
        | [] -> ret written
        | record :: rest ->
            match encodePayload record with
            | Ok payload ->
                let header = headerOf payload
                fs.Write(header, 0, header.Length)
                fs.Write(payload, 0, payload.Length)
                writeAll fs (written + header.Length + payload.Length) rest
            | Error e -> err e

    let private appendCore (sync: bool) (dir: string) (records: WalRecord list) (state: WalState) =
        match records with
        | [] -> ret state
        | _ ->
            protect (fun () ->
                use fs =
                    new System.IO.FileStream(
                        path dir state.FileNumber,
                        System.IO.FileMode.OpenOrCreate,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.ReadWrite
                    )

                let expected = int64 state.Size

                if fs.Length < expected then
                    err (DbError.corruption "Wal: 記録よりファイルが短くなっています")
                else
                    if fs.Length > expected then
                        fs.SetLength expected
                        fs.Flush true

                    fs.Seek(expected, System.IO.SeekOrigin.Begin) |> ignore

                    writeAll fs 0 records
                    |> Result.map (fun written ->
                        fs.Flush sync

                        { state with
                            Size = state.Size + int64 written
                        }))
            >>= id

    /// レコード列を追記して fsync まで行う。
    let append = appendCore true

    /// レコード列を追記するが fsync は呼び出し側に任せる。
    /// Session のグループコミットで、複数の commit を 1回の fsync にまとめるために使う。
    let appendDeferred = appendCore false

    /// 既存の WAL ファイルを開いて fsync する。
    let private syncCore (walPath: string) =
        protect (fun () ->
            use fs =
                new System.IO.FileStream(
                    walPath,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.ReadWrite,
                    System.IO.FileShare.ReadWrite
                )

            fs.Flush true)

    /// WAL の現在位置まで fsync する。
    /// rotate で古い WAL が削除済みの場合は、削除前に同期済みなので成功として扱う。
    let sync (dir: string) (state: WalState) =
        let walPath = path dir state.FileNumber

        if not (System.IO.File.Exists walPath) then
            ret ()
        else
            match syncCore walPath with
            | Error _ when not (System.IO.File.Exists walPath) -> ret ()
            | result -> result

    /// バッファが埋まるか EOF まで読み、読み込んだバイト数を返す。
    [<TailCall>]
    let rec private readAtMostLoop (fs: System.IO.FileStream) (buffer: byte array) (offset: int) =
        if offset >= buffer.Length then
            offset
        else
            let read = fs.Read(buffer, offset, buffer.Length - offset)

            if read <= 0 then
                offset
            else
                readAtMostLoop fs buffer (offset + read)

    /// ファイルからバッファ分だけ読み込む。
    let private readAtMost (fs: System.IO.FileStream) (buffer: byte array) = readAtMostLoop fs buffer 0

    /// 現在位置から 1フレーム読み、レコードを返す（不完全なら None）。
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

    /// フレームを順に読み、step で状態を畳み込む（停止位置も返す）。
    [<TailCall>]
    let rec private foldFramesLoop
        (fs: System.IO.FileStream)
        (step: 's -> WalRecord -> Result<'s, DbError>)
        (state: 's)
        =
        let positionBefore = fs.Position

        match readFrameFrom fs with
        | Ok None -> ret (state, positionBefore)
        | Ok(Some record) ->
            match step state record with
            | Ok next -> foldFramesLoop fs step next
            | Error e -> err e
        | Error e -> err e

    /// フレーム列を先頭から畳み込む。
    let private foldFrames (fs: System.IO.FileStream) (step: 's -> WalRecord -> Result<'s, DbError>) (initial: 's) =
        foldFramesLoop fs step initial

    /// 1レコードを pending 表へ反映し、(pending, 最大 txnId, 状態) を返す。
    let private applyRecord
        (step: 's -> Entry -> 's)
        (pending: Map<TxnId, Entry array list>, maxTxnId: TxnId, state: 's)
        =
        function
        | WalRecord.Begin txnId -> pending, max maxTxnId txnId, state
        | WalRecord.Batch(txnId, entries) ->
            let batches =
                match pending.TryFind txnId with
                | Some earlier -> entries :: earlier
                | None -> [ entries ]

            Map.add txnId batches pending, max maxTxnId txnId, state
        | WalRecord.Abort txnId -> Map.remove txnId pending, max maxTxnId txnId, state
        | WalRecord.Commit txnId ->
            let mine =
                match pending.TryFind txnId with
                | Some batches -> batches |> List.rev |> List.collect Array.toList
                | None -> []

            Map.remove txnId pending, max maxTxnId txnId, List.fold step state mine

    /// コミット済みエントリをメモリに溜めず、1件ずつ `step` へ渡して再生する。
    /// 大きな WAL でもコミット済みエントリの配列を保持しない。
    /// 戻り値は `(再生後の状態, WAL に現れた最大 txnId, 最後の完全なフレームの終端位置)`。
    /// 最大 txnId は open 時に NextTxnId を復元し、既存ファイル内での採番の再利用を防ぐために返す。
    /// 終端位置は open 時に不完全な末尾を切り詰めるために返す。
    let foldCommitted (dir: string) (fileNumber: int64) (step: 's -> Entry -> 's) (initial: 's) =
        let p = path dir fileNumber

        protect (fun () ->
            if not (System.IO.File.Exists p) then
                ret (initial, 0UL<txnId>, 0L)
            else
                use fs =
                    new System.IO.FileStream(
                        p,
                        System.IO.FileMode.Open,
                        System.IO.FileAccess.Read,
                        System.IO.FileShare.Read
                    )

                foldFrames
                    fs
                    (fun state record -> ret (applyRecord step state record))
                    (Map.empty, 0UL<txnId>, initial)
                |> Result.map (fun ((_, maxTxnId, state), goodSize) -> state, maxTxnId, goodSize))
        >>= id

    /// 最後の完全なフレームの終端までファイルを切り詰め、WalState の Size を合わせる。
    /// 不完全な末尾を残したまま追記すると、次回の再生がそこで止まり、
    /// 後続の commit が見えなくなるため、open 時に必ず呼ぶ。
    let truncateTo (dir: string) (state: WalState) (goodSize: int64) =
        if goodSize < 0L then
            err (DbError.internalError "Wal: ファイルサイズが範囲外です")
        else
            let p = path dir state.FileNumber

            protect (fun () ->
                if not (System.IO.File.Exists p) then
                    err (DbError.create ErrorKind.IoError (sprintf "WAL が見つかりません: %s" p))
                else
                    use fs =
                        new System.IO.FileStream(
                            p,
                            System.IO.FileMode.Open,
                            System.IO.FileAccess.ReadWrite,
                            System.IO.FileShare.ReadWrite
                        )

                    if fs.Length <> goodSize then
                        fs.SetLength goodSize
                        fs.Flush true

                    ret { state with Size = goodSize })
            >>= id

    /// ディレクトリ内の WAL ファイル名（復旧時に孤児を消すのに使う）。
    let walFiles (dir: string) =
        protect (fun () ->
            if System.IO.Directory.Exists dir then
                System.IO.Directory.GetFiles(dir, "*" + WalExtension) |> Array.toList
            else
                [])

    /// WAL ファイルを作る。
    let create (dir: string) (fileNumber: int64) =
        protect (fun () ->
            System.IO.File.WriteAllBytes(path dir fileNumber, [||])
            { FileNumber = fileNumber; Size = 0L })

    /// 新しい WAL ファイル番号に切り替える（flush 後に呼ぶ）。
    let rotate (state: WalState) =
        {
            FileNumber = state.FileNumber + 1L
            Size = 0L
        }
