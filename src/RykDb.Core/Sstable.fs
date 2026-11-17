// ===========================================================================
// SSTable — 不変のソート済みファイルの読み取り（部分読み・走査・ポイント検索）。
// 形式の定義・Footer・ブロックキャッシュ・書き出しは `SstableFormat.fs`。
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
                | Error e -> err e
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

        readAll c state (Array.zeroCreate<'Item> 128) 0

    /// ファイルの一部だけを読んで SSTable を開く。
    /// footer / index / Bloom / meta だけを読み、データブロックは必要になったとき読む。
    let openPartialWith (cache: SstCache) (path: string) =
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

                        return
                            {
                                Path = path
                                FileKey = ""
                                Index = indexEntries
                                MinKey = minKey
                                MaxKey = maxKey
                                Bloom = bloom
                                EntryCount = int64 footer.EntryCount
                                MaxSeqNo = footer.MaxSeqNo
                                Cache = cache
                            }
                    }

        protect (fun () ->
            use stream = System.IO.File.OpenRead path

            readAll stream
            >>= fun partial ->
                ret
                    { partial with
                        FileKey =
                            sprintf "%s|%d|%d" path stream.Length (System.IO.File.GetLastWriteTimeUtc(path).Ticks)
                    })
        >>= id

    /// ファイルの一部だけを読んで SSTable を開く（単体利用向け）。
    /// DB 経由の読み取りは、DB が所有するキャッシュを渡す `openPartialWith` を使う。
    let openPartial (path: string) = openPartialWith (SstCache()) path

    /// 接頭辞共有されたキーを復元する。
    /// `Array.append (Array.take ...)` の中間配列を作らない。
    let private reconstructKey (prev: byte array) (shared: uint64) (tail: byte array) =
        if shared = 0UL then
            tail
        else
            let sharedLength = int shared
            let key = Array.zeroCreate<byte>(sharedLength + tail.Length)
            Array.blit prev 0 key 0 sharedLength
            Array.blit tail 0 key sharedLength tail.Length
            key

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
                match Cursor.createSlice data payloadStart payloadLen with
                | Error e -> err e
                | Ok payload -> readItems decodeEntry payload [||]

    /// 部分読みした reader から startKey 以降を遅延生成する。
    /// ブロックは最大 8個まとめて 1回の open で読み、解析は必要になったときだけ行う。
    /// `RawValues` は compaction 用で値を復号せずスライスだけを保持し、
    /// `SkipValues` は値が不要なクエリ用で空の行を返す。
    let private rangeScanPartialWith (mode: ValueMode) (startKey: byte array) (limit: int) (partial: SstPartial) =
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
            let total = endEntry.Offset + endEntry.Length - startOffset

            protect (fun () ->
                let s = stream.Value
                s.Seek(int64 startOffset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte> total
                s.ReadExactly(buffer, 0, total)
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
    let rangeScanPartialRaw (startKey: byte array) (limit: int) (partial: SstPartial) =
        rangeScanPartialWith RawValues startKey limit partial

    /// 値を復号せず、空の行だけを返す（列が不要なクエリ用）。
    let rangeScanPartialSkipped (startKey: byte array) (limit: int) (partial: SstPartial) =
        rangeScanPartialWith SkipValues startKey limit partial

    /// 列マスクに従って必要な列だけ復号する。
    let rangeScanPartialMasked (mask: bool array) (startKey: byte array) (limit: int) (partial: SstPartial) =
        rangeScanPartialWith (MaskValues mask) startKey limit partial

    /// 値を復号しながら走査する（通常のクエリ・検証用）。
    let rangeScanPartial (startKey: byte array) (limit: int) (partial: SstPartial) =
        rangeScanPartialWith DecodeValues startKey limit partial

    /// ブロック内のキー・SeqNo・値の位置をキャッシュする。
    /// ポイント検索のたびにキー復元と値の読み飛ばしを繰り返さないため。
    /// `Partial` を強参照して、ブロックがキャッシュされている間は
    /// DB の部分読みキャッシュの弱参照が外れても index/Bloom を開き直さないようにする。
    let private cacheBlock (partial: SstPartial) (data: byte array) (indexEntry: SstIndexEntry) =
        let rec loop payloadLen (prev: byte array) keys seqNos offsets lengths c =
            if Cursor.atEnd c then
                match keys with
                | [] -> err (DbError.corruption "Sstable: ブロックにエントリがありません")
                | lastKey :: _ ->
                    if Bytes.compare lastKey indexEntry.LastKey <> 0 then
                        err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                    else
                        let block =
                            {
                                Partial = partial
                                Raw = data
                                Keys = keys |> List.rev |> Array.ofList
                                SeqNos = seqNos |> List.rev |> Array.ofList
                                ValueOffsets = offsets |> List.rev |> Array.ofList
                                ValueLengths = lengths |> List.rev |> Array.ofList
                                LastKey = lastKey
                            }

                        ret block
            else
                match Reader.readVarint c with
                | Error e -> err e
                | Ok(shared, c1) ->
                    if shared > uint64 prev.Length then
                        err (DbError.corruption "Sstable: 接頭辞長が直前のキーを超えています")
                    else
                        match Reader.readVarint c1 with
                        | Error e -> err e
                        | Ok(nonShared, c2) ->
                            match Reader.readVarint c2 with
                            | Error e -> err e
                            | Ok(valueLen, c3) ->
                                if nonShared > uint64 System.Int32.MaxValue then
                                    err (DbError.corruption "Sstable: キー長が不正です")
                                else
                                    match Reader.readBytes (int nonShared) c3 with
                                    | Error e -> err e
                                    | Ok(tail, c4) ->
                                        let key = reconstructKey prev shared tail
                                        let valueOffset = Cursor.pos c4
                                        let valueLength = int valueLen

                                        if
                                            valueLength < 0
                                            || valueLength > payloadLen
                                            || valueOffset > payloadLen - valueLength
                                        then
                                            err (DbError.corruption "Sstable: 値長が不正です")
                                        else
                                            let slice = Cursor.createSlice data valueOffset valueLength

                                            match slice >>= Reader.readVarint with
                                            | Error e -> err e
                                            | Ok(seqNo, _) ->
                                                match Reader.skipBytes valueLength c4 with
                                                | Error e -> err e
                                                | Ok c5 ->
                                                    loop
                                                        payloadLen
                                                        key
                                                        (key :: keys)
                                                        (seqNo :: seqNos)
                                                        (valueOffset :: offsets)
                                                        (valueLength :: lengths)
                                                        c5

        let n = data.Length

        if n < 4 then
            err (DbError.corruption "Sstable: ブロックにチェックサムがありません")
        else
            let payloadLen = n - 4
            let stored = Bytes.readLE32 data payloadLen

            if Hashing.checksumRange data 0 payloadLen <> stored then
                err (DbError.corruption "Sstable: ブロックのチェックサム不一致")
            else
                match Cursor.createSlice data 0 payloadLen with
                | Error e -> err e
                | Ok slice -> loop payloadLen [||] [] [] [] [] slice

    let private lookupCachedWith
        (maskOpt: bool array option)
        (block: CachedSstBlock)
        (key: byte array)
        (visible: uint64 option)
        (indexEntry: SstIndexEntry)
        =
        if Bytes.compare block.LastKey indexEntry.LastKey <> 0 then
            err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
        else
            let rec lowerBound lo hi =
                if lo >= hi then
                    lo
                else
                    let mid = lo + (hi - lo) / 2

                    if Bytes.compare block.Keys.[mid] key < 0 then
                        lowerBound (mid + 1) hi
                    else
                        lowerBound lo mid

            let rec findVisible i =
                if i >= block.Keys.Length || Bytes.compare block.Keys.[i] key <> 0 then
                    ret None
                else
                    let isVisible =
                        match visible with
                        | Some s -> block.SeqNos.[i] <= s
                        | None -> true

                    if not isVisible then
                        findVisible (i + 1)
                    else
                        let offset = block.ValueOffsets.[i]
                        let length = block.ValueLengths.[i]

                        match maskOpt with
                        | None ->
                            let valueBytes = Array.sub block.Raw offset length

                            match SstEntry.decodeValue valueBytes with
                            | Error e -> err e
                            | Ok(seqNo, value) -> ret (Some(SstEntry.make block.Keys.[i] seqNo value))
                        | Some mask ->
                            match Cursor.createSlice block.Raw offset length with
                            | Error e -> err e
                            | Ok c ->
                                match SstEntry.decodeMaskedValueAt mask c length with
                                | Error e -> err e
                                | Ok(seqNo, value, _) -> ret (Some(SstEntry.make block.Keys.[i] seqNo value))

            findVisible (lowerBound 0 block.Keys.Length)

    /// 部分読みで 1キーを検索する。
    /// 同一キーの複数版は SeqNo 降順で並ぶので、最初の可視版を返す。
    /// 可視版がブロック末尾まで無い場合は、次のブロックに古い版が続く可能性があるため読み進める。
    let private lookupWith
        (maskOpt: bool array option)
        (key: byte array)
        (visible: uint64 option)
        (partial: SstPartial)
        =
        let mayContainPartial key partial =
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

        let readBlock (indexEntry: SstIndexEntry) =
            protect (fun () ->
                use stream = System.IO.File.OpenRead partial.Path
                stream.Seek(int64 indexEntry.Offset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte> indexEntry.Length
                stream.ReadExactly(buffer, 0, buffer.Length)
                buffer)

        let lookupEntry indexEntry =
            match partial.Cache.TryGetBlock(partial, indexEntry) with
            | Some block -> lookupCachedWith maskOpt block key visible indexEntry
            | None ->
                match readBlock indexEntry with
                | Error e -> err e
                | Ok raw ->
                    match cacheBlock partial raw indexEntry with
                    | Error e -> err e
                    | Ok block ->
                        partial.Cache.PutBlock(partial, indexEntry, block)
                        lookupCachedWith maskOpt block key visible indexEntry

        let rec find i =
            if i >= partial.Index.Length then
                ret None
            else
                let indexEntry = partial.Index.[i]

                match lookupEntry indexEntry with
                | Error e -> err e
                | Ok(Some e) -> ret (Some e)
                | Ok None ->
                    if Bytes.compare indexEntry.LastKey key = 0 then
                        find (i + 1)
                    else
                        ret None

        if not (mayContainPartial key partial) then
            ret None
        else
            find (search 0 partial.Index.Length)

    /// 列マスクに従って必要な列だけ復号して 1キーを検索する。
    let lookupMasked mask key visible partial =
        lookupWith (Some mask) key visible partial
