// ===========================================================================
// SstableReader — 不変のソート済みファイルの読み取り（部分読み・走査・ポイント検索）。
// ===========================================================================

namespace RykDb.Core

/// SSTable の部分読み取りに必要なメタデータ（不変）。
/// <remarks>
/// footer / index / Bloom / meta を 1回解析した結果で、ファイル内容だけで決まる純粋な値。
/// `FileMetaData.ReadInfo` に載せ、読み取りのたびに開き直さない。
/// DB・プロセスの寿命にわたるキャッシュではない（不変の Version に付随する）。
/// </remarks>
type internal SstReadInfo =
    {
        Path: string
        Index: SstIndexEntry array
        MinKey: byte array
        MaxKey: byte array
        Bloom: BloomFilter option
        EntryCount: int64
        MaxSeqNo: SeqNo
    }

module internal SstableReader =
    /// カーソルを末尾まで読み、配列へ詰める。
    [<TailCall>]
    let rec private readItem
        (step: Cursor -> 'State -> Result<Cursor * 'Item * 'State, DbError>)
        (c: Cursor)
        (state: 'State)
        (count: int)
        (items: 'Item array)
        =
        if Cursor.atEnd c then
            if count = items.Length then
                ret items
            else
                ret (Array.sub items 0 count)
        else
            match step c state with
            | Ok(c', item, state') ->
                let items =
                    if count < items.Length then
                        items
                    else
                        let grown = Array.zeroCreate<'Item>(max 16 (items.Length * 2))
                        Array.blit items 0 grown 0 count
                        grown

                items.[count] <- item
                readItem step c' state' (count + 1) items
            | Error e -> err e

    /// Cursor から終端まで 1件ずつ読み出して配列にまとめる。
    /// step は「読むたびに変わる状態」（キーの接頭辞共有など）を
    /// `(cursor, state) -> Result<cursor * 要素 * state, DbError>` として受け取る。
    /// 状態が要らないときは unit を渡す。
    let private readItems
        (step: Cursor -> 'State -> Result<Cursor * 'Item * 'State, DbError>)
        (c: Cursor)
        (state: 'State)
        =
        readItem step c state 0 (Array.zeroCreate<'Item> 128)

    /// メタデータから最小・最大キーを取り出す。
    /// 空のテーブルはメタデータが無い（空キー）。
    /// 中身があるのに壊れていれば CorruptionError（空キーへ黙って倒さない）。
    let private readMinMaxKey (metaBytes: byte array) =
        if metaBytes.Length = 0 then
            ret ([||], [||])
        else
            Cursor.create metaBytes
            |> Reader.readLengthPrefixed
            >>= fun (mn, c) -> Reader.readLengthPrefixed c |> Result.map (fun (mx, _) -> mn, mx)

    /// Bloom filter のバイト列を復元する。
    let private readBloomBytes (bytes: byte array) =
        if bytes.Length = 0 then
            ret None
        else
            BloomFilter.deserialize bytes |> Result.map Some

    /// 索引の 1エントリを読む。
    let private decodeIndexEntry (c: Cursor) =
        result {
            let! lastKey, c1 = Reader.readLengthPrefixed c
            let! off, c2 = Reader.readInt64LE c1
            let! len, c3 = Reader.readInt64LE c2

            if
                off < 0L
                || len < 0L
                || off > int64 System.Int32.MaxValue
                || len > int64 System.Int32.MaxValue
            then
                return! err (DbError.corruption "Sstable: 索引の位置が不正です")
            else
                let entry =
                    {
                        LastKey = lastKey
                        Offset = int off
                        Length = int len
                    }

                return c3, entry, ()
        }

    /// 索引バイト列をエントリ配列にする。
    let private parseIndexBytes (bytes: byte array) =
        if bytes.Length = 0 then
            ret [||]
        else
            readItems (fun c _ -> decodeIndexEntry c) (Cursor.create bytes) ()

    /// 索引エントリの範囲と順序を検証する。
    [<TailCall>]
    let rec private validateIndex (footerStart: int64) (prev: SstIndexEntry option) =
        function
        | [] -> ret ()
        | entry :: rest ->
            let endPos = int64 entry.Offset + int64 entry.Length

            if endPos > footerStart then
                err (DbError.corruption "Sstable: 索引の範囲がファイル外です")
            else
                match prev with
                | Some prevEntry ->
                    if int64 entry.Offset < int64 prevEntry.Offset + int64 prevEntry.Length then
                        err (DbError.corruption "Sstable: 索引の範囲が重なっています")
                    elif Bytes.compare prevEntry.LastKey entry.LastKey > 0 then
                        err (DbError.corruption "Sstable: 索引の LastKey が昇順ではありません")
                    else
                        validateIndex footerStart (Some entry) rest
                | None -> validateIndex footerStart (Some entry) rest

    /// 索引エントリの範囲と順序を検証する。
    let private validateIndexEntries (footerStart: int64) (entries: SstIndexEntry array) =
        validateIndex footerStart None (Array.toList entries)

    /// ストリームの指定範囲をバイト列として読む。
    let private readRegion (stream: System.IO.FileStream) (offset: int64) (length: int) =
        if length = 0 then
            [||]
        else
            stream.Seek(offset, System.IO.SeekOrigin.Begin) |> ignore
            let buffer = Array.zeroCreate<byte> length
            stream.ReadExactly(buffer, 0, length)
            buffer

    /// Footer を読む。
    let private readFooter (stream: System.IO.FileStream) (size: int) =
        let start = stream.Length - int64 size

        if start < 0L then
            None
        else
            let bytes = readRegion stream start size

            if System.Text.Encoding.ASCII.GetString(bytes, 0, SstableFooter.Magic.Length) = SstableFooter.Magic then
                Some(start, bytes)
            else
                None

    /// 部分読みのメタデータを読んで SstReadInfo を作る。
    let private readPartialInfo (stream: System.IO.FileStream) (path: string) =
        match readFooter stream SstableFooter.FooterSize with
        | None -> err (DbError.corruption "Sstable: Footer がありません")
        | Some(footerStart, footerBytes) ->
            SstableFooter.parseFooterBytes footerStart footerBytes
            >>= fun footer ->
                let metaBytes = readRegion stream (int64 footer.MetaOff) footer.MetaLen
                let bloomBytes = readRegion stream (int64 footer.BloomOff) footer.BloomLen
                let indexBytes = readRegion stream (int64 footer.IndexOff) footer.IndexLen

                result {
                    let! minKey, maxKey = readMinMaxKey metaBytes
                    let! bloom = readBloomBytes bloomBytes
                    let! indexEntries = parseIndexBytes indexBytes

                    if
                        Hashing.checksumChunks [| metaBytes; bloomBytes; indexBytes |]
                        <> footer.MetadataChecksum
                    then
                        return! err (DbError.corruption "Sstable: 部分読みメタデータのチェックサム不一致")
                    else if (footer.EntryCount = 0) <> (indexEntries.Length = 0) then
                        return! err (DbError.corruption "Sstable: エントリ数と索引の有無が一致しません")
                    else
                        do! validateIndexEntries footerStart indexEntries

                        return
                            {
                                Path = path
                                Index = indexEntries
                                MinKey = minKey
                                MaxKey = maxKey
                                Bloom = bloom
                                EntryCount = int64 footer.EntryCount
                                MaxSeqNo = footer.MaxSeqNo
                            }
                }

    /// ファイルの一部だけを読んで SSTable を開く。
    /// footer / index / Bloom / meta だけを読み、データブロックは必要になったとき読む。
    let openPartial (path: string) =
        protect (fun () ->
            use stream = System.IO.File.OpenRead path
            readPartialInfo stream path)
        >>= id

    /// 索引上で startKey 以上になる最初のブロックを二分探索する。
    [<TailCall>]
    let rec private searchBlock (startKey: byte array) (lo: int) (hi: int) (partial: SstReadInfo) =
        if lo >= hi then
            lo
        else
            let mid = lo + (hi - lo) / 2

            if Bytes.compare partial.Index.[mid].LastKey startKey < 0 then
                searchBlock startKey (mid + 1) hi partial
            else
                searchBlock startKey lo mid partial

    /// ブロック内の値をどう扱うか。
    /// Decode は通常のクエリ、Raw は compaction、Skip は列が不要なクエリ。
    /// `MaskValues` はマスクと「どの列も要らない」の事前計算を持つ。
    type private ValueMode =
        | DecodeValues
        | RawValues
        | SkipValues
        | MaskValues of bool array * bool

    /// ブロック内の 1エントリを復号する（値の扱いは mode で決める）。
    let private decodeBlockEntry (mode: ValueMode) (c: Cursor) (prev: byte array) =
        result {
            let! shared, c = Reader.readVarint c

            if shared > uint64 prev.Length then
                return! err (DbError.corruption "Sstable: 接頭辞長が直前のキーを超えています")

            let! nonShared, c = Reader.readVarint c

            if nonShared > uint64 System.Int32.MaxValue then
                return! err (DbError.corruption "Sstable: キー長が不正です")
            else
                let sharedLength = int shared
                let tailLength = int nonShared

                if tailLength > System.Int32.MaxValue - sharedLength then
                    return! err (DbError.corruption "Sstable: キー長が不正です")
                else
                    let! valueLen, c = Reader.readVarint c

                    if valueLen > uint64 System.Int32.MaxValue then
                        return! err (DbError.corruption "Sstable: 値長が不正です")
                    else
                        let key = Array.zeroCreate<byte>(sharedLength + tailLength)
                        Array.blit prev 0 key 0 sharedLength

                        return!
                            Reader.copyBytesInto key sharedLength tailLength c
                            >>= fun c ->
                                if Bytes.compare prev key > 0 then
                                    err (DbError.corruption "Sstable: ブロック内のキーが昇順ではありません")
                                else
                                    match mode with
                                    | DecodeValues ->
                                        SstEntry.decodeValueAt (int valueLen) c
                                        >>= fun (seqNo, value, c) -> ret (c, SstEntry.make key seqNo value, key)
                                    | RawValues ->
                                        SstEntry.decodeRawValueAt (int valueLen) c
                                        >>= fun (seqNo, raw, c) -> ret (c, SstEntry.makeRaw key seqNo raw, key)
                                    | SkipValues ->
                                        SstEntry.decodeSkippedValueAt (int valueLen) c
                                        >>= fun (seqNo, value, c) -> ret (c, SstEntry.make key seqNo value, key)
                                    | MaskValues(mask, keepsNothing) ->
                                        SstEntry.decodeMaskedValueAt mask keepsNothing (int valueLen) c
                                        >>= fun (seqNo, value, c) -> ret (c, SstEntry.make key seqNo value, key)
        }

    /// バッファ内の範囲にある 1ブロックをエントリ列へ復元する。
    /// 末尾 4バイトのチェックサムを検証する。
    let private parseBlockAt (mode: ValueMode) (data: byte array) (offset: int) (length: int) =
        if offset < 0 || length < 4 || offset > data.Length - length then
            err (DbError.corruption "Sstable: ブロック範囲が不正です")
        else
            let payloadLen = length - 4
            let payloadStart = offset
            let stored = Bytes.readLE32 data (payloadStart + payloadLen)

            if Hashing.checksumRange data payloadStart payloadLen <> stored then
                err (DbError.corruption "Sstable: ブロックのチェックサム不一致")
            else
                Cursor.createSlice data payloadStart payloadLen
                >>= fun payload -> readItems (decodeBlockEntry mode) payload [||]

    /// 生バイトから 1ブロックを解析する。
    let private parseRawBlock (mode: ValueMode) (raw: byte array) (startOffset: int) (i: int) (partial: SstReadInfo) =
        let ie = partial.Index.[i]
        let relativeOffset = ie.Offset - startOffset

        if relativeOffset < 0 || ie.Length < 0 || relativeOffset > raw.Length - ie.Length then
            err (DbError.corruption "Sstable: 索引範囲が読み込みブロックの範囲外です")
        else
            parseBlockAt mode raw relativeOffset ie.Length
            >>= fun entries ->
                if
                    entries.Length = 0
                    || Bytes.compare entries.[entries.Length - 1].Key ie.LastKey <> 0
                then
                    err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                elif i > 0 && Bytes.compare partial.Index.[i - 1].LastKey entries.[0].Key > 0 then
                    err (DbError.corruption "Sstable: 前のブロックの LastKey より小さいキーで始まっています")
                else
                    ret entries

    /// 最大 batchBlocks 個分の生バイトをまとめて読む。
    let private readRawBatch (stream: Lazy<System.IO.FileStream>) (batchBlocks: int) (i: int) (partial: SstReadInfo) =
        let batchEnd = min (i + batchBlocks) partial.Index.Length
        let startOffset = partial.Index.[i].Offset
        let endEntry = partial.Index.[batchEnd - 1]
        let total = int64 endEntry.Offset + int64 endEntry.Length - int64 startOffset

        if total < 0L || total > int64 System.Int32.MaxValue then
            err (DbError.corruption "Sstable: 索引範囲が不正です")
        else
            protect (fun () ->
                let s = stream.Value
                s.Seek(int64 startOffset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte>(int total)
                s.ReadExactly(buffer, 0, int total)
                buffer, startOffset, batchEnd)

    /// 開いたファイルストリームを閉じる。
    let private closeScanStream (stream: Lazy<System.IO.FileStream>) =
        if stream.IsValueCreated then
            stream.Value.Dispose()

    /// 次のブロック（バッチ）を読み込む。
    let rec private loadBlock
        (stream: Lazy<System.IO.FileStream>)
        (mode: ValueMode)
        (startKey: byte array)
        (limit: int)
        (batchBlocks: int)
        (raw: byte array)
        (startOffset: int)
        (batchEnd: int)
        (count: int)
        (i: int)
        (partial: SstReadInfo)
        =
        if i >= partial.Index.Length || limit > 0 && count >= limit then
            closeScanStream stream
            SstScan.Empty
        elif i < batchEnd then
            SstScan.ofStepWithClose
                (fun () ->
                    match parseRawBlock mode raw startOffset i partial with
                    | Error e ->
                        closeScanStream stream
                        err e
                    | Ok entries ->
                        SstScan.next (
                            scanEntries
                                stream
                                mode
                                startKey
                                limit
                                batchBlocks
                                raw
                                startOffset
                                batchEnd
                                count
                                (i + 1)
                                partial
                                0
                                entries
                        ))
                (fun () -> closeScanStream stream)
        else
            SstScan.ofStepWithClose
                (fun () ->
                    match readRawBatch stream batchBlocks i partial with
                    | Error e ->
                        closeScanStream stream
                        err e
                    | Ok(raw, startOffset, batchEnd) ->
                        SstScan.next (
                            loadBlock stream mode startKey limit batchBlocks raw startOffset batchEnd count i partial
                        ))
                (fun () -> closeScanStream stream)

    /// 読み込み済みブロックの残りエントリを走査する。
    and private scanEntries
        (stream: Lazy<System.IO.FileStream>)
        (mode: ValueMode)
        (startKey: byte array)
        (limit: int)
        (batchBlocks: int)
        (raw: byte array)
        (startOffset: int)
        (batchEnd: int)
        (count: int)
        (i: int)
        (partial: SstReadInfo)
        (pos: int)
        (entries: SstEntry array)
        =
        if pos >= entries.Length then
            loadBlock stream mode startKey limit batchBlocks raw startOffset batchEnd count i partial
        elif limit > 0 && count >= limit then
            closeScanStream stream
            SstScan.Empty
        elif Bytes.compare entries.[pos].Key startKey < 0 then
            SstScan.ofStepWithClose
                (fun () ->
                    SstScan.next (
                        scanEntries
                            stream
                            mode
                            startKey
                            limit
                            batchBlocks
                            raw
                            startOffset
                            batchEnd
                            count
                            i
                            partial
                            (pos + 1)
                            entries
                    ))
                (fun () -> closeScanStream stream)
        else
            SstScan.ofArray
                entries
                pos
                count
                limit
                (fun nextCount ->
                    SstScan.next (
                        loadBlock stream mode startKey limit batchBlocks raw startOffset batchEnd nextCount i partial
                    ))
                (fun () -> closeScanStream stream)


    /// 部分読みした reader から startKey 以降を遅延生成する。
    /// ブロックは最大 8個まとめて 1回の open で読み、解析は必要になったときだけ行う。
    /// `RawValues` は compaction 用で値を復号せずスライスだけを保持し、
    /// `SkipValues` は値が不要なクエリ用で空の行を返す。
    let private rangeScanPartialWith (mode: ValueMode) (startKey: byte array) (limit: int) (partial: SstReadInfo) =
        let stream =
            lazy
                (new System.IO.FileStream(
                    partial.Path,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.Read,
                    System.IO.FileShare.Read,
                    64 * 1024,
                    System.IO.FileOptions.SequentialScan
                ))

        let batchBlocks = 8
        let startBlock = searchBlock startKey 0 partial.Index.Length partial

        if startBlock >= partial.Index.Length then
            closeScanStream stream
            SstScan.Empty
        else
            SstScan.ofStepWithClose
                (fun () ->
                    match readRawBatch stream batchBlocks startBlock partial with
                    | Error e ->
                        closeScanStream stream
                        err e
                    | Ok(raw, startOffset, batchEnd) ->
                        SstScan.next (
                            loadBlock
                                stream
                                mode
                                startKey
                                limit
                                batchBlocks
                                raw
                                startOffset
                                batchEnd
                                0
                                startBlock
                                partial
                        ))
                (fun () -> closeScanStream stream)

    /// 値を復号せずに走査する（compaction 用）。
    let rangeScanPartialRaw = rangeScanPartialWith RawValues

    /// 値を復号せず、空の行だけを返す（列が不要なクエリ用）。
    let rangeScanPartialSkipped = rangeScanPartialWith SkipValues

    /// 列マスクに従って必要な列だけ復号する。
    /// 「どの列も要らない」は走査の構築時に 1回だけ判定する。
    let rangeScanPartialMasked (mask: bool array) =
        let keepsNothing = mask.Length = 0 || Array.forall not mask
        rangeScanPartialWith (MaskValues(mask, keepsNothing))

    /// 値を復号しながら走査する（通常のクエリ・検証用）。
    let rangeScanPartial = rangeScanPartialWith DecodeValues

    /// Bloom filter でキーが含まれ得るかを判定する。
    let private mayContainPartial (key: byte array) (partial: SstReadInfo) =
        match partial.Bloom with
        | None -> true
        | Some bloom -> Hashing.hashBytes key |> Hashing.mix |> BloomFilter.mightContain bloom

    /// 索引上でキー以上になる最初のエントリを二分探索する。
    [<TailCall>]
    let rec private searchIndex (key: byte array) (lo: int) (hi: int) (partial: SstReadInfo) =
        if lo >= hi then
            lo
        else
            let mid = lo + (hi - lo) / 2

            if Bytes.compare partial.Index.[mid].LastKey key < 0 then
                searchIndex key (mid + 1) hi partial
            else
                searchIndex key lo mid partial

    /// 索引エントリが指すブロックを読む。
    let private readIndexBlock (indexEntry: SstIndexEntry) (partial: SstReadInfo) =
        protect (fun () ->
            use stream = System.IO.File.OpenRead partial.Path
            stream.Seek(int64 indexEntry.Offset, System.IO.SeekOrigin.Begin) |> ignore
            let buffer = Array.zeroCreate<byte> indexEntry.Length
            stream.ReadExactly(buffer, 0, buffer.Length)
            buffer)

    /// ブロック内の varint を読む。
    [<TailCall>]
    let rec private readVarintGo (shift: int) (acc: uint64) (data: byte array) (payloadLen: int) (i: int) =
        if i >= payloadLen || shift >= 64 then
            struct (false, 0UL, i)
        else
            let b = data.[i]
            let payload = b &&& 0x7Fuy

            if shift = 63 && payload > 1uy then
                struct (false, 0UL, i)
            else
                let acc = acc ||| (uint64 payload <<< shift)

                if b &&& 0x80uy = 0uy then
                    struct (true, acc, i + 1)
                else
                    readVarintGo (shift + 7) acc data payloadLen (i + 1)

    /// 指定位置の varint を読む。
    let private readVarintAt = readVarintGo 0 0UL

    /// ブロック内の 1エントリのヘッダを読み、接頭辞長・キー末尾長・キー開始位置・値長を返す。
    let private readLookupHeader
        (data: byte array)
        (payloadLen: int)
        (prev: byte array)
        (pos: int)
        : Result<struct (int * int * int * int), DbError> =
        let struct (ok1, shared, p1) = readVarintAt data payloadLen pos
        let struct (ok2, tailLen, p2) = readVarintAt data payloadLen p1
        let struct (ok3, valueLen, p3) = readVarintAt data payloadLen p2

        if not (ok1 && ok2 && ok3) then
            err (DbError.corruption "Sstable: ブロックの varint が不正です")
        elif shared > uint64 prev.Length then
            err (DbError.corruption "Sstable: 接頭辞長が直前のキーを超えています")
        elif
            tailLen > uint64 System.Int32.MaxValue
            || int tailLen > System.Int32.MaxValue - int shared
        then
            err (DbError.corruption "Sstable: キー長が不正です")
        elif int tailLen > payloadLen - p3 then
            err (DbError.corruption "Sstable: キーがブロックの終端を超えています")
        else
            let sharedLength = int shared
            let tailLength = int tailLen
            let valuePos = p3 + tailLength

            if valueLen > uint64 System.Int32.MaxValue || int valueLen > payloadLen - valuePos then
                err (DbError.corruption "Sstable: 値がブロックの終端を超えています")
            else
                ret struct (sharedLength, tailLength, p3, int valueLen)

    /// 値部分を復号する（列マスクを適用する場合がある）。
    let private decodeLookupValue
        (data: byte array)
        (mask: bool array)
        (keepsNothing: bool)
        (valuePos: int)
        (valueLen: int)
        =
        Cursor.createSlice data valuePos valueLen
        >>= SstEntry.decodeMaskedValueAt mask keepsNothing valueLen

    /// ブロック内を先頭から走査してキーを探す。
    [<TailCall>]
    let rec private scanLookupBlock
        (key: byte array)
        (mask: bool array)
        (keepsNothing: bool)
        (visible: uint64 option)
        (indexEntry: SstIndexEntry)
        (data: byte array)
        (payloadLen: int)
        (prev: byte array)
        (pos: int)
        =
        if pos >= payloadLen then
            if Bytes.compare prev indexEntry.LastKey <> 0 then
                err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
            else
                ret None
        else
            match readLookupHeader data payloadLen prev pos with
            | Error e -> err e
            | Ok(struct (sharedLength, tailLength, keyStart, valueLength)) ->
                let valuePos = keyStart + tailLength
                let entryKey = Array.zeroCreate<byte>(sharedLength + tailLength)
                Array.blit prev 0 entryKey 0 sharedLength
                Array.blit data keyStart entryKey sharedLength tailLength

                if Bytes.compare prev entryKey > 0 then
                    err (DbError.corruption "Sstable: ブロック内のキーが昇順ではありません")
                else
                    let ordering = Bytes.compare entryKey key

                    if ordering < 0 then
                        scanLookupBlock
                            key
                            mask
                            keepsNothing
                            visible
                            indexEntry
                            data
                            payloadLen
                            entryKey
                            (valuePos + valueLength)
                    elif ordering > 0 then
                        ret None
                    else
                        match decodeLookupValue data mask keepsNothing valuePos valueLength with
                        | Ok(seqNo, value, _) ->
                            let isVisible =
                                match visible with
                                | Some s -> seqNo <= s
                                | None -> true

                            if isVisible then
                                ret (Some(SstEntry.make entryKey seqNo value))
                            else
                                scanLookupBlock
                                    key
                                    mask
                                    keepsNothing
                                    visible
                                    indexEntry
                                    data
                                    payloadLen
                                    entryKey
                                    (valuePos + valueLength)
                        | Error e -> err e

    /// 1ブロックを検索する（チェックサムを検証してから走査）。
    let private lookupIndexEntry
        (key: byte array)
        (mask: bool array)
        (visible: uint64 option)
        (indexEntry: SstIndexEntry)
        (partial: SstReadInfo)
        =
        readIndexBlock indexEntry partial
        >>= fun data ->
            let n = data.Length
            let payloadLen = n - 4

            if n < 4 then
                err (DbError.corruption "Sstable: ブロックにチェックサムがありません")
            elif Hashing.checksumRange data 0 payloadLen <> Bytes.readLE32 data payloadLen then
                err (DbError.corruption "Sstable: ブロックのチェックサム不一致")
            else
                let keepsNothing = mask.Length = 0 || Array.forall not mask
                scanLookupBlock key mask keepsNothing visible indexEntry data payloadLen [||] 0

    /// 索引を先頭から辿って最初の可視版を探す。
    [<TailCall>]
    let rec private findIndexEntry
        (key: byte array)
        (mask: bool array)
        (visible: uint64 option)
        (i: int)
        (partial: SstReadInfo)
        =
        if i >= partial.Index.Length then
            ret None
        else
            let indexEntry = partial.Index.[i]

            match lookupIndexEntry key mask visible indexEntry partial with
            | Ok(Some e) -> ret (Some e)
            | Ok None ->
                if Bytes.compare indexEntry.LastKey key = 0 then
                    findIndexEntry key mask visible (i + 1) partial
                else
                    ret None
            | Error e -> err e

    /// 部分読みで 1キーを検索する。
    /// 同一キーの複数版は SeqNo 降順で並ぶので、最初の可視版を返す。
    /// 可視版がブロック末尾まで無い場合は、次のブロックに古い版が続く可能性があるため読み進める。
    let private lookupWith (key: byte array) (mask: bool array) (visible: uint64 option) (partial: SstReadInfo) =
        if not (mayContainPartial key partial) then
            ret None
        else
            findIndexEntry key mask visible (searchIndex key 0 partial.Index.Length partial) partial

    /// 列マスクに従って必要な列だけ復号して 1キーを検索する。
    let lookupMasked (key: byte array) (mask: bool array) (visible: uint64 option) (partial: SstReadInfo) =
        lookupWith key mask visible partial
