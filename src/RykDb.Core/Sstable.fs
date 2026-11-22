// ===========================================================================
// SSTable — 不変のソート済みファイルの読み取り（部分読み・走査・ポイント検索）。
// 形式の定義・Footer・書き出しは `SstableFormat.fs`。
// ===========================================================================

namespace RykDb.Core

module internal Sstable =
    /// Footer の固定長（形式は `SstableFormat`）。
    let internal FooterSize = SstableFormat.FooterSize

    /// Cursor から終端まで 1件ずつ読み出して配列にまとめる。
    /// step は「読むたびに変わる状態」（キーの接頭辞共有など）を
    /// `(cursor, state) -> Result<cursor * 要素 * state, DbError>` として受け取る。
    /// 状態が要らないときは unit を渡す。
    let private readItems
        (step: Cursor -> 'State -> Result<Cursor * 'Item * 'State, DbError>)
        (c: Cursor)
        (state: 'State)
        =
        let rec readAll c state (items: 'Item array) count =
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
                    readAll c' state' items (count + 1)
                | Error e -> err e

        readAll c state (Array.zeroCreate<'Item> 128) 0

    /// ファイルの一部だけを読んで SSTable を開く。
    /// footer / index / Bloom / meta だけを読み、データブロックは必要になったとき読む。
    let openPartial (path: string) =
        let readMinMaxKey metaBytes =
            match Cursor.create metaBytes |> Reader.readLengthPrefixed with
            | Ok(mn, c) ->
                match Reader.readLengthPrefixed c with
                | Ok(mx, _) -> mn, mx
                | Error _ -> [||], [||]
            | Error _ -> [||], [||]

        let readBloomBytes (bytes: byte array) =
            if bytes.Length = 0 then
                ret None
            else
                BloomFilter.deserialize bytes |> Result.map Some

        let decodeIndexEntry c _ =
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

        let parseIndexBytes (bytes: byte array) =
            if bytes.Length = 0 then
                ret [||]
            else
                readItems decodeIndexEntry (Cursor.create bytes) ()

        let validateIndexEntries footerStart entries =
            let rec loop prev =
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
                                loop (Some entry) rest
                        | None -> loop (Some entry) rest

            loop None (Array.toList entries)

        let readRegion (stream: System.IO.FileStream) offset length =
            if length = 0 then
                [||]
            else
                stream.Seek(offset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte> length
                stream.ReadExactly(buffer, 0, length)
                buffer

        let readFooter (stream: System.IO.FileStream) size =
            let start = stream.Length - int64 size

            if start < 0L then
                None
            else
                let bytes = readRegion stream start size

                if System.Text.Encoding.ASCII.GetString(bytes, 0, SstableFormat.Magic.Length) = SstableFormat.Magic then
                    Some(start, bytes)
                else
                    None

        let validateFooter (info: SstableFormat.FooterInfo) magic footerStart =
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

        let parseFooterBytes footerBytes footerStart =
            SstableFormat.readFooterFields footerBytes
            >>= fun (magic, info) -> validateFooter info magic footerStart

        let readAll stream =
            match readFooter stream FooterSize with
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

        protect (fun () ->
            use stream = System.IO.File.OpenRead path
            readAll stream)
        >>= id

    /// ブロック内の値をどう扱うか。
    /// Decode は通常のクエリ、Raw は compaction、Skip は列が不要なクエリ。
    type private ValueMode =
        | DecodeValues
        | RawValues
        | SkipValues
        | MaskValues of bool array

    /// バッファ内の範囲にある 1ブロックをエントリ列へ復元する。
    /// 末尾 4バイトのチェックサムを検証する。
    let private parseBlockAt (mode: ValueMode) (data: byte array) (offset: int) (length: int) =
        let decodeEntry c (prev: byte array) =
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
                >>= fun payload -> readItems decodeEntry payload [||]

    /// 部分読みした reader から startKey 以降を遅延生成する。
    /// ブロックは最大 8個まとめて 1回の open で読み、解析は必要になったときだけ行う。
    /// `RawValues` は compaction 用で値を復号せずスライスだけを保持し、
    /// `SkipValues` は値が不要なクエリ用で空の行を返す。
    let private rangeScanPartialWith (mode: ValueMode) (startKey: byte array) (limit: int) (partial: SstReadInfo) =
        let rec search lo hi =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare partial.Index.[mid].LastKey startKey < 0 then
                    search (mid + 1) hi
                else
                    search lo mid

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

        let close () =
            if stream.IsValueCreated then
                stream.Value.Dispose()

        let batchBlocks = 8

        let readRawBatch i =
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

        let parseRawBlock (raw: byte array) startOffset i =
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

        let stepScan (step: unit -> Result<(SstEntry * SstScan) option, DbError>) = SstScan.ofStepWithClose step close

        let rec scanEntries raw startOffset batchEnd pos count i (entries: SstEntry array) =
            if pos >= entries.Length then
                loadBlock raw startOffset batchEnd count i
            elif limit > 0 && count >= limit then
                close ()
                SstScan.Empty
            elif Bytes.compare entries.[pos].Key startKey < 0 then
                stepScan (fun () -> SstScan.next (scanEntries raw startOffset batchEnd (pos + 1) count i entries))
            else
                SstScan.ofArray
                    entries
                    pos
                    count
                    limit
                    (fun nextCount -> SstScan.next (loadBlock raw startOffset batchEnd nextCount i))
                    close

        and loadBlock raw startOffset batchEnd count i =
            if i >= partial.Index.Length || limit > 0 && count >= limit then
                close ()
                SstScan.Empty
            elif i < batchEnd then
                stepScan (fun () ->
                    match parseRawBlock raw startOffset i with
                    | Error e ->
                        close ()
                        err e
                    | Ok entries -> SstScan.next (scanEntries raw startOffset batchEnd 0 count (i + 1) entries))
            else
                stepScan (fun () ->
                    match readRawBatch i with
                    | Error e ->
                        close ()
                        err e
                    | Ok(raw, startOffset, batchEnd) -> SstScan.next (loadBlock raw startOffset batchEnd count i))

        let startBlock = search 0 partial.Index.Length

        if startBlock >= partial.Index.Length then
            close ()
            SstScan.Empty
        else
            stepScan (fun () ->
                match readRawBatch startBlock with
                | Error e ->
                    close ()
                    err e
                | Ok(raw, startOffset, batchEnd) -> SstScan.next (loadBlock raw startOffset batchEnd 0 startBlock))

    /// 値を復号せずに走査する（compaction 用）。
    let rangeScanPartialRaw = rangeScanPartialWith RawValues

    /// 値を復号せず、空の行だけを返す（列が不要なクエリ用）。
    let rangeScanPartialSkipped = rangeScanPartialWith SkipValues

    /// 列マスクに従って必要な列だけ復号する。
    let rangeScanPartialMasked (mask: bool array) = rangeScanPartialWith (MaskValues mask)

    /// 値を復号しながら走査する（通常のクエリ・検証用）。
    let rangeScanPartial = rangeScanPartialWith DecodeValues

    /// 部分読みで 1キーを検索する。
    /// 同一キーの複数版は SeqNo 降順で並ぶので、最初の可視版を返す。
    /// 可視版がブロック末尾まで無い場合は、次のブロックに古い版が続く可能性があるため読み進める。
    let private lookupWith
        (maskOpt: bool array option)
        (key: byte array)
        (visible: uint64 option)
        (partial: SstReadInfo)
        =
        let mayContainPartial key (partial: SstReadInfo) =
            match partial.Bloom with
            | None -> true
            | Some bloom -> Hashing.hashBytes key |> Hashing.mix |> BloomFilter.mightContain bloom

        let rec search lo hi =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare partial.Index.[mid].LastKey key < 0 then
                    search (mid + 1) hi
                else
                    search lo mid

        let readBlock indexEntry =
            protect (fun () ->
                use stream = System.IO.File.OpenRead partial.Path
                stream.Seek(int64 indexEntry.Offset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte> indexEntry.Length
                stream.ReadExactly(buffer, 0, buffer.Length)
                buffer)

        let readVarintAt (data: byte array) payloadLen pos =
            let rec go shift acc i =
                if i >= payloadLen || shift >= 64 then
                    struct (false, 0UL, i)
                else
                    let b = data.[i]
                    let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)

                    if b &&& 0x80uy = 0uy then
                        struct (true, acc, i + 1)
                    else
                        go (shift + 7) acc (i + 1)

            go 0 0UL pos

        let decodeValue data valuePos valueLen =
            Cursor.createSlice data valuePos valueLen
            >>= fun slice ->
                match maskOpt with
                | None -> SstEntry.decodeValueAt slice valueLen
                | Some mask -> SstEntry.decodeMaskedValueAt mask slice valueLen

        let rec scan (indexEntry: SstIndexEntry) data payloadLen prev pos =
            if pos >= payloadLen then
                if Bytes.compare prev indexEntry.LastKey <> 0 then
                    err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                else
                    ret None
            else
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
                        let entryKey = Array.zeroCreate<byte>(sharedLength + tailLength)
                        Array.blit prev 0 entryKey 0 sharedLength
                        Array.blit data p3 entryKey sharedLength tailLength
                        let ordering = Bytes.compare entryKey key

                        if ordering < 0 then
                            scan indexEntry data payloadLen entryKey (valuePos + int valueLen)
                        elif ordering > 0 then
                            ret None
                        else
                            match decodeValue data valuePos (int valueLen) with
                            | Ok(seqNo, value, _) ->
                                let isVisible =
                                    match visible with
                                    | Some s -> seqNo <= s
                                    | None -> true

                                if isVisible then
                                    ret (Some(SstEntry.make entryKey seqNo value))
                                else
                                    scan indexEntry data payloadLen entryKey (valuePos + int valueLen)
                            | Error e -> err e

        let lookupEntry indexEntry =
            readBlock indexEntry
            >>= fun data ->
                let n = data.Length
                let payloadLen = n - 4

                if n < 4 then
                    err (DbError.corruption "Sstable: ブロックにチェックサムがありません")
                elif Hashing.checksumRange data 0 payloadLen <> Bytes.readLE32 data payloadLen then
                    err (DbError.corruption "Sstable: ブロックのチェックサム不一致")
                else
                    scan indexEntry data payloadLen [||] 0

        let rec find i =
            if i >= partial.Index.Length then
                ret None
            else
                let indexEntry = partial.Index.[i]

                match lookupEntry indexEntry with
                | Ok(Some e) -> ret (Some e)
                | Ok None ->
                    if Bytes.compare indexEntry.LastKey key = 0 then
                        find (i + 1)
                    else
                        ret None
                | Error e -> err e

        if not (mayContainPartial key partial) then
            ret None
        else
            find (search 0 partial.Index.Length)

    /// 列マスクに従って必要な列だけ復号して 1キーを検索する。
    let lookupMasked (mask: bool array) (key: byte array) (visible: uint64 option) (partial: SstReadInfo) =
        lookupWith (Some mask) key visible partial
