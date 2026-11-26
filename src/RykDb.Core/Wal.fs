// ===========================================================================
// WAL（Write-Ahead Log）— 論理ログ。
// Begin / Batch / Commit / Abort を記録し、open 時に再生する。
// ファイル先頭: [magic:8][version:u32LE][headerCRC:u32LE]
// 1フレーム    : [payloadLength:u32LE][payloadCRC:u32LE][headerCRC:u32LE][payload]
// 長さが合わない末尾は書きかけとして打ち切り、チェックサム不一致は CorruptionError にする。
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
        FileNumber: FileNumber
        /// 現在の WAL ファイルに書き込んだバイト数
        Size: int64
    }

module internal Wal =
    /// WAL ファイルの拡張子。
    [<Literal>]
    let WalExtension = ".wal"

    /// Begin のタグ。
    [<Literal>]
    let private TagBegin = 0uy

    /// Batch のタグ。
    [<Literal>]
    let private TagBatch = 1uy

    /// Commit のタグ。
    [<Literal>]
    let private TagCommit = 2uy

    /// Abort のタグ。
    [<Literal>]
    let private TagAbort = 3uy

    /// WAL のファイルヘッダーの magic。
    [<Literal>]
    let private Magic = "RYKWAL02"

    /// ファイルヘッダーの長さ（magic 8 + version 4 + header CRC 4）。
    [<Literal>]
    let private HeaderLength = 16

    /// フレームヘッダーの長さ（length 4 + payload CRC 4 + header CRC 4）。
    [<Literal>]
    let private FrameHeaderLength = 12

    /// WAL のファイル形式バージョン。
    [<Literal>]
    let private FormatVersion = 2u

    /// magic のバイト列。
    let private magicBytes = System.Text.Encoding.ASCII.GetBytes Magic

    /// タグと txnId だけのレコードを符号化する。
    let private txnRecord (tag: byte) (txnId: TxnId) =
        Writer.build [ Writer.writeByte tag; Writer.uint64BE (uint64 txnId) ]

    /// レコードをバイト列にする。
    let private encodePayload (r: WalRecord) =
        match r with
        | WalRecord.Begin txnId -> ret (txnRecord TagBegin txnId)
        | WalRecord.Commit txnId -> ret (txnRecord TagCommit txnId)
        | WalRecord.Abort txnId -> ret (txnRecord TagAbort txnId)
        | WalRecord.Batch(txnId, entries) ->
            Entry.encodeBatch txnId entries
            |> Result.map (fun bytes -> Writer.build [ Writer.writeByte TagBatch; Writer.writeBytes bytes ])

    /// バイト列からレコードを復元する。
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

    /// ファイル番号から WAL のファイル名を作る。
    let private walFileName (fileNumber: FileNumber) =
        sprintf "%06d%s" fileNumber WalExtension

    /// WAL ファイルのパス。
    let path (dir: string) (fileNumber: FileNumber) =
        System.IO.Path.Combine(dir, walFileName fileNumber)

    /// ファイルヘッダー（magic + version + header CRC）を符号化する。
    let private header () =
        let prefix =
            Writer.build [ Writer.writeBytes magicBytes; Writer.uint32LE FormatVersion ]

        Writer.build [ Writer.writeBytes prefix; Writer.uint32LE (Hashing.checksum prefix) ]

    [<TailCall>]
    let rec private readHeader (fs: System.IO.FileStream) (header: byte array) (offset: int) =
        if offset >= header.Length then
            offset
        else
            let read = fs.Read(header, offset, header.Length - offset)

            if read <= 0 then
                offset
            else
                readHeader fs header (offset + read)

    /// ファイル先頭のヘッダーを検証する。
    /// 戻り値は「ヘッダーが完全か」。空・途中までは書きかけとして false を返す。
    /// magic が一致しないファイル（旧 v1 を含む）は CorruptionError にする。
    let private detectFormat (fs: System.IO.FileStream) =
        let original = fs.Position
        let header = Array.zeroCreate<byte> HeaderLength
        fs.Position <- 0L
        let headerBytes = readHeader fs header 0
        fs.Position <- original

        if headerBytes < magicBytes.Length then
            ret false
        else
            let prefix = Array.sub header 0 magicBytes.Length

            if prefix <> magicBytes then
                err (DbError.corruption "Wal: 不明な形式です（v1 の WAL は読み取れません）")
            elif headerBytes < HeaderLength then
                ret false
            else
                let version = Bytes.readLE32 header 8
                let checksum = Bytes.readLE32 header 12

                if version <> FormatVersion || Hashing.checksumRange header 0 12 <> checksum then
                    err (DbError.corruption "Wal: ファイルヘッダーのチェックサム不一致")
                else
                    ret true

    /// フレームヘッダ（長さ + payload CRC + ヘッダ CRC）を符号化する。
    let private frameHeader (payload: byte array) =
        let frame =
            Writer.build
                [
                    uint32 payload.Length |> Writer.uint32LE
                    Hashing.checksum payload |> Writer.uint32LE
                ]

        Writer.build [ Writer.writeBytes frame; Writer.uint32LE (Hashing.checksum frame) ]

    /// レコードを 1フレームずつ書き出し、書き込んだバイト数を返す。
    [<TailCall>]
    let rec private writeAll (fs: System.IO.FileStream) (written: int) =
        function
        | [] -> ret written
        | record :: rest ->
            match encodePayload record with
            | Ok payload ->
                let header = frameHeader payload
                fs.Write(header, 0, header.Length)
                fs.Write(payload, 0, payload.Length)
                writeAll fs (written + header.Length + payload.Length) rest
            | Error e -> err e

    /// 追記本体。
    /// フレーム単位で書き、書きかけの末尾は切り捨ててから追記する。
    let private appendCore (sync: bool) (dir: string) (records: WalRecord list) (state: WalState) =
        match records with
        | [] -> ret state
        | _ ->
            protect (fun () ->
                use fs =
                    new System.IO.FileStream(
                        path dir state.FileNumber,
                        System.IO.FileMode.OpenOrCreate,
                        System.IO.FileAccess.ReadWrite,
                        System.IO.FileShare.ReadWrite
                    )

                let expected = int64 state.Size

                if fs.Length < expected then
                    err (DbError.corruption "Wal: 記録よりファイルが短くなっています")
                else
                    if fs.Length > expected then
                        fs.SetLength expected
                        fs.Flush true

                    let headerBytes =
                        if expected = 0L then
                            let header = header ()
                            fs.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                            fs.Write(header, 0, header.Length)
                            Ok header.Length
                        else
                            detectFormat fs
                            |> Result.bind (fun complete ->
                                if complete then
                                    Ok 0
                                else
                                    err (DbError.corruption "Wal: ファイルヘッダーが不完全です"))

                    match headerBytes with
                    | Error e -> err e
                    | Ok headerBytes ->
                        let startPosition = if expected = 0L then int64 headerBytes else expected
                        fs.Seek(startPosition, System.IO.SeekOrigin.Begin) |> ignore

                        match writeAll fs 0 records with
                        | Error e -> err e
                        | Ok written ->
                            fs.Flush sync

                            ret
                                { state with
                                    Size = state.Size + int64 headerBytes + int64 written
                                })
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
        let header = Array.zeroCreate<byte> FrameHeaderLength
        let headerBytes = readAtMost fs header

        if headerBytes < header.Length then
            ret None
        else
            let length = uint64 (Bytes.readLE32 header 0)
            let payloadChecksum = Bytes.readLE32 header 4
            let headerChecksum = Bytes.readLE32 header 8

            if Hashing.checksumRange header 0 8 <> headerChecksum then
                err (DbError.corruption "Wal: フレームヘッダーのチェックサム不一致")
            else
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
                    elif Hashing.checksum payload <> payloadChecksum then
                        err (DbError.corruption "Wal: チェックサム不一致")
                    else
                        decodePayload payload |> Result.map Some

    /// フレームを順に読み、step で状態を畳み込む（停止位置も返す）。
    [<TailCall>]
    let rec private foldFrames (fs: System.IO.FileStream) (step: 's -> WalRecord -> Result<'s, DbError>) (state: 's) =
        let positionBefore = fs.Position

        match readFrameFrom fs with
        | Ok None -> ret (state, positionBefore)
        | Ok(Some record) ->
            match step state record with
            | Ok next -> foldFrames fs step next
            | Error e -> err e
        | Error e -> err e

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
    let foldCommitted (dir: string) (fileNumber: FileNumber) (step: 's -> Entry -> 's) (initial: 's) =
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

                match detectFormat fs with
                | Error e -> err e
                | Ok false -> ret (initial, 0UL<txnId>, 0L)
                | Ok true ->
                    fs.Position <- int64 HeaderLength

                    foldFrames
                        fs
                        (fun state record -> ret (applyRecord step state record))
                        (Map.empty, 0UL<txnId>, initial)
                    |> Result.map (fun ((_, maxTxnId, state), goodSize) -> state, maxTxnId, goodSize))
        >>= id

    /// 最後の完全なフレームの終端までファイルを切り詰め、WalState の Size を合わせる。
    /// 不完全な末尾を残したまま追記すると、次回の再生がそこで止まり、
    /// 後続の commit が見えなくなるため、open 時に必ず呼ぶ。
    let truncateTo (dir: string) (goodSize: int64) (state: WalState) =
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

    /// WAL ファイルを作る（v2 のファイルヘッダーを書いて fsync する）。
    let create (dir: string) (fileNumber: FileNumber) =
        protect (fun () ->
            use fs =
                new System.IO.FileStream(
                    path dir fileNumber,
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.None
                )

            let header = header ()
            fs.Write(header, 0, header.Length)
            fs.Flush true

            {
                FileNumber = fileNumber
                Size = int64 header.Length
            })

    /// 新しい WAL ファイル番号に切り替える（flush 後に呼ぶ）。
    let rotate (state: WalState) =
        {
            FileNumber = state.FileNumber + 1L
            Size = 0L
        }
