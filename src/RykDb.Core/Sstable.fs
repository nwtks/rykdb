// ===========================================================================
// SSTable — 不変のソート済みファイルの読み取り（部分読み・走査・ポイント検索）。
// ===========================================================================

namespace RykDb.Core

module internal Sstable =
    /// カーソルを末尾まで読み、配列へ詰める本体。
    [<TailCall>]
    let rec private readItemsLoop
        (step: Cursor -> 'State -> Result<Cursor * 'Item * 'State, DbError>)
        (c: Cursor)
        (state: 'State)
        (items: 'Item array)
        (count: int)
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
                readItemsLoop step c' state' items (count + 1)
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
        readItemsLoop step c state (Array.zeroCreate<'Item> 128) 0

    /// メタデータから最小・最大キーを取り出す（壊れていたら空に倒す）。
    let private readMinMaxKey (metaBytes: byte array) =
        match Cursor.create metaBytes |> Reader.readLengthPrefixed with
        | Ok(mn, c) ->
            match Reader.readLengthPrefixed c with
            | Ok(mx, _) -> mn, mx
            | Error _ -> [||], [||]
        | Error _ -> [||], [||]

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

    /// 索引エントリの範囲と順序を検証する本体。
    [<TailCall>]
    let rec private validateIndexLoop (footerStart: int64) (prev: SstIndexEntry option) =
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
                        validateIndexLoop footerStart (Some entry) rest
                | None -> validateIndexLoop footerStart (Some entry) rest

    /// 索引エントリの範囲と順序を検証する。
    let private validateIndexEntries (footerStart: int64) (entries: SstIndexEntry array) =
        validateIndexLoop footerStart None (Array.toList entries)

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

            if System.Text.Encoding.ASCII.GetString(bytes, 0, SstableFormat.Magic.Length) = SstableFormat.Magic then
                Some(start, bytes)
            else
                None

    /// Footer の内容を検証する。
    let private validateFooter (info: SstFooterInfo) (magic: string) (footerStart: int64) =
        if magic <> SstableFormat.Magic then
            err (DbError.corruption "Sstable: マジックが違います")
        elif
            info.Version < SstableFormat.MinReadableVersion
            || info.Version > SstableFormat.Version
        then
            err (sprintf "Sstable: 未対応のバージョン %d" info.Version |> DbError.corruption)
        elif
            footerStart < 0L
            || int64 info.MetaOff + int64 info.MetaLen <> int64 info.BloomOff
            || int64 info.BloomOff + int64 info.BloomLen <> int64 info.IndexOff
            || int64 info.IndexOff + int64 info.IndexLen <> footerStart
        then
            err (DbError.corruption "Sstable: Footer の範囲が不正です")
        else
            ret info

    /// Footer バイト列を解析して検証する。
    let private parseFooterBytes (footerBytes: byte array) (footerStart: int64) =
        SstableFormat.readFooterFields footerBytes
        >>= fun (magic, info) -> validateFooter info magic footerStart

    /// 部分読みのメタデータを読んで SstReadInfo を作る。
    let private readPartialInfo (path: string) (stream: System.IO.FileStream) =
        match readFooter stream SstableFormat.FooterSize with
        | None -> err (DbError.corruption "Sstable: Footer がありません")
        | Some(footerStart, footerBytes) ->
            parseFooterBytes footerBytes footerStart
            >>= fun footer ->
                let metaBytes = readRegion stream (int64 footer.MetaOff) footer.MetaLen
                let bloomBytes = readRegion stream (int64 footer.BloomOff) footer.BloomLen
                let indexBytes = readRegion stream (int64 footer.IndexOff) footer.IndexLen
                let minKey, maxKey = readMinMaxKey metaBytes

                result {
                    let! bloom = readBloomBytes bloomBytes
                    let! indexEntries = parseIndexBytes indexBytes

                    if
                        Hashing.checksumChunks [| metaBytes; bloomBytes; indexBytes |]
                        <> footer.MetadataChecksum
                    then
                        return! err (DbError.corruption "Sstable: 部分読みメタデータのチェックサム不一致")
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
            readPartialInfo path stream)
        >>= id

    /// ブロック内の値をどう扱うか。
    /// Decode は通常のクエリ、Raw は compaction、Skip は列が不要なクエリ。
    type private ValueMode =
        | DecodeValues
        | RawValues
        | SkipValues
        | MaskValues of bool array

    /// ブロック内の 1エントリを復号する（値の扱いは mode で決める）。
    let private decodeBlockEntry (mode: ValueMode) (c: Cursor) (prev: byte array) =
        result {
            let! shared, c = Reader.readVarint c

            if shared > uint64 prev.Length then
                return! err (DbError.corruption "Sstable: 接頭辞長が直前のキーを超えています")

            let! nonShared, c = Reader.readVarint c
            let! valueLen, c = Reader.readVarint c

            if nonShared > uint64 System.Int32.MaxValue then
                return! err (DbError.corruption "Sstable: キー長が不正です")
            else
                let sharedLength = int shared
                let tailLength = int nonShared

                if tailLength > System.Int32.MaxValue - sharedLength then
                    return! err (DbError.corruption "Sstable: キー長が不正です")
                elif valueLen > uint64 System.Int32.MaxValue then
                    return! err (DbError.corruption "Sstable: 値長が不正です")
                else
                    let key = Array.zeroCreate<byte>(sharedLength + tailLength)
                    Array.blit prev 0 key 0 sharedLength

                    return!
                        Reader.copyBytesInto key sharedLength tailLength c
                        >>= fun c ->
                            match mode with
                            | DecodeValues ->
                                SstEntry.decodeValueAt c (int valueLen)
                                >>= fun (seqNo, value, c) -> ret (c, SstEntry.make key seqNo value, key)
                            | RawValues ->
                                SstEntry.decodeRawValueAt c (int valueLen)
                                >>= fun (seqNo, raw, c) -> ret (c, SstEntry.makeRaw key seqNo raw, key)
                            | SkipValues ->
                                SstEntry.decodeSkippedValueAt c (int valueLen)
                                >>= fun (seqNo, value, c) -> ret (c, SstEntry.make key seqNo value, key)
                            | MaskValues mask ->
                                SstEntry.decodeMaskedValueAt mask c (int valueLen)
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

    /// 索引上で startKey 以上になる最初のブロックを二分探索する。
    [<TailCall>]
    let rec private searchBlock (partial: SstReadInfo) (startKey: byte array) (lo: int) (hi: int) =
        if lo >= hi then
            lo
        else
            let mid = lo + (hi - lo) / 2

            if Bytes.compare partial.Index.[mid].LastKey startKey < 0 then
                searchBlock partial startKey (mid + 1) hi
            else
                searchBlock partial startKey lo mid

    /// 開いたファイルストリームを閉じる。
    let private closeScanStream (stream: Lazy<System.IO.FileStream>) =
        if stream.IsValueCreated then
            stream.Value.Dispose()

    /// 最大 batchBlocks 個分の生バイトをまとめて読む。
    let private readRawBatch (partial: SstReadInfo) (stream: Lazy<System.IO.FileStream>) (batchBlocks: int) (i: int) =
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

    /// 生バイトから 1ブロックを解析する。
    let private parseRawBlock (mode: ValueMode) (partial: SstReadInfo) (raw: byte array) (startOffset: int) (i: int) =
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
                else
                    ret entries

    /// ステップ走査に後始末を付ける。
    let private stepScanWithClose (close: unit -> unit) (step: unit -> Result<(SstEntry * SstScan) option, DbError>) =
        SstScan.ofStepWithClose step close

    /// 読み込み済みブロックの残りエントリを走査する。
    let rec private scanEntries
        (mode: ValueMode)
        (startKey: byte array)
        (limit: int)
        (partial: SstReadInfo)
        (stream: Lazy<System.IO.FileStream>)
        (batchBlocks: int)
        (raw: byte array)
        (startOffset: int)
        (batchEnd: int)
        (pos: int)
        (count: int)
        (i: int)
        (entries: SstEntry array)
        =
        if pos >= entries.Length then
            loadBlock mode startKey limit partial stream batchBlocks raw startOffset batchEnd count i
        elif limit > 0 && count >= limit then
            closeScanStream stream
            SstScan.Empty
        elif Bytes.compare entries.[pos].Key startKey < 0 then
            stepScanWithClose (fun () -> closeScanStream stream) (fun () ->
                SstScan.next (
                    scanEntries
                        mode
                        startKey
                        limit
                        partial
                        stream
                        batchBlocks
                        raw
                        startOffset
                        batchEnd
                        (pos + 1)
                        count
                        i
                        entries
                ))
        else
            SstScan.ofArray
                entries
                pos
                count
                limit
                (fun nextCount ->
                    SstScan.next (
                        loadBlock mode startKey limit partial stream batchBlocks raw startOffset batchEnd nextCount i
                    ))
                (fun () -> closeScanStream stream)

    /// 次のブロック（バッチ）を読み込む。
    and private loadBlock
        (mode: ValueMode)
        (startKey: byte array)
        (limit: int)
        (partial: SstReadInfo)
        (stream: Lazy<System.IO.FileStream>)
        (batchBlocks: int)
        (raw: byte array)
        (startOffset: int)
        (batchEnd: int)
        (count: int)
        (i: int)
        =
        if i >= partial.Index.Length || limit > 0 && count >= limit then
            closeScanStream stream
            SstScan.Empty
        elif i < batchEnd then
            stepScanWithClose (fun () -> closeScanStream stream) (fun () ->
                match parseRawBlock mode partial raw startOffset i with
                | Error e ->
                    closeScanStream stream
                    err e
                | Ok entries ->
                    SstScan.next (
                        scanEntries
                            mode
                            startKey
                            limit
                            partial
                            stream
                            batchBlocks
                            raw
                            startOffset
                            batchEnd
                            0
                            count
                            (i + 1)
                            entries
                    ))
        else
            stepScanWithClose (fun () -> closeScanStream stream) (fun () ->
                match readRawBatch partial stream batchBlocks i with
                | Error e ->
                    closeScanStream stream
                    err e
                | Ok(raw, startOffset, batchEnd) ->
                    SstScan.next (
                        loadBlock mode startKey limit partial stream batchBlocks raw startOffset batchEnd count i
                    ))

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
        let startBlock = searchBlock partial startKey 0 partial.Index.Length

        if startBlock >= partial.Index.Length then
            closeScanStream stream
            SstScan.Empty
        else
            stepScanWithClose (fun () -> closeScanStream stream) (fun () ->
                match readRawBatch partial stream batchBlocks startBlock with
                | Error e ->
                    closeScanStream stream
                    err e
                | Ok(raw, startOffset, batchEnd) ->
                    SstScan.next (
                        loadBlock mode startKey limit partial stream batchBlocks raw startOffset batchEnd 0 startBlock
                    ))

    /// 値を復号せずに走査する（compaction 用）。
    let rangeScanPartialRaw = rangeScanPartialWith RawValues

    /// 値を復号せず、空の行だけを返す（列が不要なクエリ用）。
    let rangeScanPartialSkipped = rangeScanPartialWith SkipValues

    /// 列マスクに従って必要な列だけ復号する。
    let rangeScanPartialMasked (mask: bool array) = rangeScanPartialWith (MaskValues mask)

    /// 値を復号しながら走査する（通常のクエリ・検証用）。
    let rangeScanPartial = rangeScanPartialWith DecodeValues

    /// Bloom filter でキーが含まれ得るかを判定する。
    let private mayContainPartial (key: byte array) (partial: SstReadInfo) =
        match partial.Bloom with
        | None -> true
        | Some bloom -> Hashing.hashBytes key |> Hashing.mix |> BloomFilter.mightContain bloom

    /// 索引上でキー以上になる最初のエントリを二分探索する。
    [<TailCall>]
    let rec private searchIndex (partial: SstReadInfo) (key: byte array) (lo: int) (hi: int) =
        if lo >= hi then
            lo
        else
            let mid = lo + (hi - lo) / 2

            if Bytes.compare partial.Index.[mid].LastKey key < 0 then
                searchIndex partial key (mid + 1) hi
            else
                searchIndex partial key lo mid

    /// 索引エントリが指すブロックを読む。
    let private readIndexBlock (partial: SstReadInfo) (indexEntry: SstIndexEntry) =
        protect (fun () ->
            use stream = System.IO.File.OpenRead partial.Path
            stream.Seek(int64 indexEntry.Offset, System.IO.SeekOrigin.Begin) |> ignore
            let buffer = Array.zeroCreate<byte> indexEntry.Length
            stream.ReadExactly(buffer, 0, buffer.Length)
            buffer)

    /// ブロック内の varint を読む本体。
    [<TailCall>]
    let rec private readVarintGo (data: byte array) (payloadLen: int) (shift: int) (acc: uint64) (i: int) =
        if i >= payloadLen || shift >= 64 then
            struct (false, 0UL, i)
        else
            let b = data.[i]
            let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)

            if b &&& 0x80uy = 0uy then
                struct (true, acc, i + 1)
            else
                readVarintGo data payloadLen (shift + 7) acc (i + 1)

    /// 指定位置の varint を読む。
    let private readVarintAt (data: byte array) (payloadLen: int) (pos: int) = readVarintGo data payloadLen 0 0UL pos

    /// 値部分を復号する（列マスクを適用する場合がある）。
    let private decodeLookupValue (maskOpt: bool array option) (data: byte array) (valuePos: int) (valueLen: int) =
        Cursor.createSlice data valuePos valueLen
        >>= fun slice ->
            match maskOpt with
            | None -> SstEntry.decodeValueAt slice valueLen
            | Some mask -> SstEntry.decodeMaskedValueAt mask slice valueLen

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
                ret (struct (sharedLength, tailLength, p3, int valueLen))

    /// ブロック内を先頭から走査してキーを探す。
    [<TailCall>]
    let rec private scanLookupBlock
        (maskOpt: bool array option)
        (key: byte array)
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
                let ordering = Bytes.compare entryKey key

                if ordering < 0 then
                    scanLookupBlock maskOpt key visible indexEntry data payloadLen entryKey (valuePos + valueLength)
                elif ordering > 0 then
                    ret None
                else
                    match decodeLookupValue maskOpt data valuePos valueLength with
                    | Ok(seqNo, value, _) ->
                        let isVisible =
                            match visible with
                            | Some s -> seqNo <= s
                            | None -> true

                        if isVisible then
                            ret (Some(SstEntry.make entryKey seqNo value))
                        else
                            scanLookupBlock
                                maskOpt
                                key
                                visible
                                indexEntry
                                data
                                payloadLen
                                entryKey
                                (valuePos + valueLength)
                    | Error e -> err e

    /// 1ブロックを検索する（チェックサムを検証してから走査）。
    let private lookupIndexEntry
        (maskOpt: bool array option)
        (key: byte array)
        (visible: uint64 option)
        (partial: SstReadInfo)
        (indexEntry: SstIndexEntry)
        =
        readIndexBlock partial indexEntry
        >>= fun data ->
            let n = data.Length
            let payloadLen = n - 4

            if n < 4 then
                err (DbError.corruption "Sstable: ブロックにチェックサムがありません")
            elif Hashing.checksumRange data 0 payloadLen <> Bytes.readLE32 data payloadLen then
                err (DbError.corruption "Sstable: ブロックのチェックサム不一致")
            else
                scanLookupBlock maskOpt key visible indexEntry data payloadLen [||] 0

    /// 索引を先頭から辿って最初の可視版を探す。
    [<TailCall>]
    let rec private findIndexEntry
        (maskOpt: bool array option)
        (key: byte array)
        (visible: uint64 option)
        (partial: SstReadInfo)
        (i: int)
        =
        if i >= partial.Index.Length then
            ret None
        else
            let indexEntry = partial.Index.[i]

            match lookupIndexEntry maskOpt key visible partial indexEntry with
            | Ok(Some e) -> ret (Some e)
            | Ok None ->
                if Bytes.compare indexEntry.LastKey key = 0 then
                    findIndexEntry maskOpt key visible partial (i + 1)
                else
                    ret None
            | Error e -> err e

    /// 部分読みで 1キーを検索する。
    /// 同一キーの複数版は SeqNo 降順で並ぶので、最初の可視版を返す。
    /// 可視版がブロック末尾まで無い場合は、次のブロックに古い版が続く可能性があるため読み進める。
    let private lookupWith
        (maskOpt: bool array option)
        (key: byte array)
        (visible: uint64 option)
        (partial: SstReadInfo)
        =
        if not (mayContainPartial key partial) then
            ret None
        else
            findIndexEntry maskOpt key visible partial (searchIndex partial key 0 partial.Index.Length)

    /// 列マスクに従って必要な列だけ復号して 1キーを検索する。
    let lookupMasked (mask: bool array) (key: byte array) (visible: uint64 option) (partial: SstReadInfo) =
        lookupWith (Some mask) key visible partial
