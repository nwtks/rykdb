// ===========================================================================
// SSTable の書き出し — ブロック・Bloom filter・索引・Footer の生成。
// ===========================================================================

namespace RykDb.Core

/// `SstableWriter.writeTo` の結果。
/// 入力が `seq` なので、書き出しに成功してから先頭・末尾のキーを数え直せない。
/// `FileMetaData.writeSstable` がこれを `FileMetaData` に載せる。
/// `Index` / `Bloom` は書き出しと同時に得られるので、
/// 読み取り用の `SstReadInfo` を再解析なしで組み立てられる。
type internal SstWriteInfo =
    {
        /// 書き込んだバイト数
        Size: int64
        /// 先頭エントリのキー（0 件なら空配列）
        FirstKey: byte array
        /// 末尾エントリのキー（0 件なら空配列）
        LastKey: byte array
        EntryCount: int64
        MaxSeqNo: SeqNo
        /// 索引（ブロック位置）。
        Index: SstIndexEntry array
        /// 書き出した Bloom filter（0 件なら None）。
        Bloom: BloomFilter option
    }

module internal SstableWriter =
    /// 1ブロックあたりのエントリ数。
    [<Literal>]
    let private EntriesPerBlock = 128

    /// 共通接頭辞の長さを先頭から数える。
    [<TailCall>]
    let rec private sharedLength (prev: byte array) (key: byte array) (i: int) (n: int) =
        if i >= n then n
        elif prev.[i] = key.[i] then sharedLength prev key (i + 1) n
        else i

    /// 2つのキーの共通接頭辞の長さを返す。
    let private sharedPrefix (prev: byte array) (key: byte array) =
        let n = min prev.Length key.Length
        sharedLength prev key 0 n

    /// 1エントリの符号化後の長さを返す。
    let private entryLength (prevKey: byte array) (e: SstEntry) =
        let shared = sharedPrefix prevKey e.Key
        let tailLength = e.Key.Length - shared
        let valueLength = SstEntry.encodedValueLength e

        Writer.varintLength (uint64 shared)
        + Writer.varintLength (uint64 tailLength)
        + Writer.varintLength (uint64 valueLength)
        + tailLength
        + valueLength

    /// 1エントリをバッファの pos から書き込み、次の位置を返す。
    let private writeEntry (out: byte array) (pos: int) (prevKey: byte array) (e: SstEntry) =
        let shared = sharedPrefix prevKey e.Key
        let tailLength = e.Key.Length - shared
        let valueLength = SstEntry.encodedValueLength e
        let pos = Writer.writeVarintTo out pos (uint64 shared)
        let pos = Writer.writeVarintTo out pos (uint64 tailLength)
        let pos = Writer.writeVarintTo out pos (uint64 valueLength)
        Array.blit e.Key shared out pos tailLength
        SstEntry.writeValueTo out (pos + tailLength) e

    /// ブロック本体のバイト長を数える（int64 で蓄積してから上限を検査）。
    [<TailCall>]
    let rec private blockLength (prev: byte array) (i: int) (total: int64) (entries: SstEntry array) =
        if i >= entries.Length then
            total
        else
            blockLength entries.[i].Key (i + 1) (total + int64 (entryLength prev entries.[i])) entries

    /// エントリをブロックへ順に書き込む。
    [<TailCall>]
    let rec private writeEntries (block: byte array) (prev: byte array) (i: int) (pos: int) (entries: SstEntry array) =
        if i >= entries.Length then
            ret pos
        else
            match writeEntry block pos prev entries.[i] with
            | Ok next -> writeEntries block entries.[i].Key (i + 1) next entries
            | Error e -> err e

    /// エントリ列からチェックサム付きのブロックを組み立てる。
    let private buildBlock (entries: SstEntry array) =
        let total = blockLength [||] 0 0L entries

        if total > int64 System.Int32.MaxValue - 4L then
            err (DbError.internalError "Sstable: ブロックが大きすぎます")
        else
            let length = int total
            let block = Array.zeroCreate<byte>(length + 4)

            writeEntries block [||] 0 0 entries
            |> Result.map (fun _ ->
                let sum = Hashing.checksumRange block 0 length
                block.[length] <- byte (sum &&& 0xFFu)
                block.[length + 1] <- byte (sum >>> 8 &&& 0xFFu)
                block.[length + 2] <- byte (sum >>> 16 &&& 0xFFu)
                block.[length + 3] <- byte (sum >>> 24 &&& 0xFFu)
                block)

    /// ブロックをストリームへ書き出し、索引要素を返す。
    let private writeBlock
        (stream: System.IO.Stream)
        (offset: int)
        (indexEntries: SstIndexEntry list)
        (entries: SstEntry array)
        =
        buildBlock entries
        >>= fun data ->
            let nextOffset = int64 offset + int64 data.Length

            if nextOffset > int64 System.Int32.MaxValue then
                err (DbError.internalError "Sstable: ファイルが大きすぎます")
            else
                stream.Write(data, 0, data.Length)

                let entry =
                    {
                        LastKey = entries.[entries.Length - 1].Key
                        Offset = offset
                        Length = data.Length
                    }

                ret (int nextOffset, entry :: indexEntries)

    /// 索引を直列化して書き出せる形にする。
    let private indexBytesOf =
        Array.collect (fun (e: SstIndexEntry) ->
            [|
                Writer.lengthPrefixed e.LastKey
                Writer.int64LE (int64 e.Offset)
                Writer.int64LE (int64 e.Length)
            |])
        >> Array.toList
        >> Writer.build

    /// 残っているブロックを書き出し、(データ長, 索引) を返す。
    let private flushPendingBlock
        (stream: System.IO.Stream)
        (offset: int)
        (indexEntries: SstIndexEntry list)
        (entries: SstEntry list)
        =
        if List.isEmpty entries then
            ret (offset, indexEntries)
        else
            entries |> List.rev |> List.toArray |> writeBlock stream offset indexEntries

    /// メタ（先頭・末尾キー）を符号化する。
    let private metaBytesOf (firstKey: byte array option) (lastKey: byte array option) =
        match firstKey, lastKey with
        | Some first, Some last -> Writer.build [ Writer.lengthPrefixed first; Writer.lengthPrefixed last ]
        | _ -> [||]

    /// Bloom filter とその符号化結果を返す（0 件なら None と空配列）。
    let private bloomBytesOf (bloom: BloomFilter.BloomBuilder) (count: int64) =
        if count = 0L then
            None, [||]
        else
            let filter = bloom.Build()
            Some filter, BloomFilter.serialize filter

    /// フッター手前に書き出すメタ・Bloom・索引の符号化結果。
    type private SstableTailParts =
        {
            DataSize: int
            MetaBytes: byte array
            BloomFilter: BloomFilter option
            BloomBytes: byte array
            Index: SstIndexEntry array
            IndexBytes: byte array
        }

    /// フッター手前に書き出すメタ・Bloom・索引をまとめる。
    let private buildTail
        (bloom: BloomFilter.BloomBuilder)
        (count: int64)
        (firstKey: byte array option)
        (lastKey: byte array option)
        (dataSize: int)
        (indexEntries: SstIndexEntry list)
        =
        let metaBytes = metaBytesOf firstKey lastKey
        let bloomFilter, bloomBytes = bloomBytesOf bloom count
        let bloomOff = int64 dataSize + int64 metaBytes.Length
        let indexOff = bloomOff + int64 bloomBytes.Length
        let index = List.rev indexEntries |> List.toArray
        let indexBytes = indexBytesOf index

        if indexOff + int64 indexBytes.Length > int64 System.Int32.MaxValue then
            err (DbError.internalError "Sstable: ファイルが大きすぎます")
        else
            ret
                {
                    DataSize = dataSize
                    MetaBytes = metaBytes
                    BloomFilter = bloomFilter
                    BloomBytes = bloomBytes
                    Index = index
                    IndexBytes = indexBytes
                }

    /// メタ・Bloom・索引・フッターを書き出し、ファイルサイズを返す。
    let private writeTail (stream: System.IO.Stream) (maxSeqNo: SeqNo) (count: int64) (tail: SstableTailParts) =
        let metaBytes = tail.MetaBytes
        let bloomBytes = tail.BloomBytes
        let indexBytes = tail.IndexBytes
        let bloomOff = int64 tail.DataSize + int64 metaBytes.Length
        let indexOff = bloomOff + int64 bloomBytes.Length

        let metadataChecksum =
            Hashing.checksumChunks [| metaBytes; bloomBytes; indexBytes |]

        stream.Write(metaBytes, 0, metaBytes.Length)
        stream.Write(bloomBytes, 0, bloomBytes.Length)
        stream.Write(indexBytes, 0, indexBytes.Length)

        let footer =
            SstableFooter.encodeFooter
                maxSeqNo
                (int indexOff)
                indexBytes.Length
                (int bloomOff)
                bloomBytes.Length
                tail.DataSize
                metaBytes.Length
                (int count)
                metadataChecksum

        stream.Write(footer, 0, footer.Length)
        ret (indexOff + int64 indexBytes.Length + int64 footer.Length)

    /// ブロック列・Bloom・索引・フッターを書き出し、結果を返す。
    let private finishWrite
        (stream: System.IO.Stream)
        (bloom: BloomFilter.BloomBuilder)
        (firstKey: byte array option)
        (lastKey: byte array option)
        (maxSeqNo: SeqNo)
        (count: int64)
        (offset: int)
        (indexEntries: SstIndexEntry list)
        (entries: SstEntry list)
        =
        flushPendingBlock stream offset indexEntries entries
        >>= fun (dataSize, indexEntries) ->
            buildTail bloom count firstKey lastKey dataSize indexEntries
            >>= fun tail ->
                writeTail stream maxSeqNo count tail
                |> Result.map (fun size ->
                    {
                        Size = size
                        FirstKey = defaultArg firstKey [||]
                        LastKey = defaultArg lastKey [||]
                        EntryCount = count
                        MaxSeqNo = maxSeqNo
                        Index = tail.Index
                        Bloom = tail.BloomFilter
                    })

    /// エントリをブロック単位で書き出す（末尾再帰）。
    [<TailCall>]
    let rec private writeLoop
        (stream: System.IO.Stream)
        (bloom: BloomFilter.BloomBuilder)
        (e: System.Collections.Generic.IEnumerator<SstEntry>)
        (firstKey: byte array option)
        (lastKey: byte array option)
        (maxSeqNo: SeqNo)
        (count: int64)
        (blockCount: int)
        (offset: int)
        (indexEntries: SstIndexEntry list)
        (prev: SstEntry option)
        (entries: SstEntry list)
        =
        if blockCount >= EntriesPerBlock then
            match entries |> List.rev |> List.toArray |> writeBlock stream offset indexEntries with
            | Ok(nextOffset, indexEntries') ->
                writeLoop stream bloom e firstKey lastKey maxSeqNo count 0 nextOffset indexEntries' prev []
            | Error e -> err e
        elif e.MoveNext() then
            let item = e.Current
            bloom.Add item.Key

            match prev with
            | Some p when SstEntry.compare p item > 0 -> err (DbError.internalError "Sstable: エントリがキー順に並んでいません")
            | _ ->
                writeLoop
                    stream
                    bloom
                    e
                    (match firstKey with
                     | None -> Some item.Key
                     | _ -> firstKey)
                    (Some item.Key)
                    (max maxSeqNo item.SeqNo)
                    (count + 1L)
                    (blockCount + 1)
                    offset
                    indexEntries
                    (Some item)
                    (item :: entries)
        else
            finishWrite stream bloom firstKey lastKey maxSeqNo count offset indexEntries entries

    /// エントリ列を SSTable v6 として stream に書き出し、書き込んだ結果を返す。
    /// <remarks>
    /// 入力は `seq` で 1回だけ列挙する（memtable の flush が全件を配列にしないため）。
    /// キー順であることの検証は書き込みと同時に行う（失敗時は呼び出し側が部分ファイルを消す）。
    /// ブロックと Bloom filter は書き出しと同時に構築するので、ファイル全体分のメモリを持たない。
    /// `expectedEntries` は Bloom filter のビット数を決める見積り。
    /// 実件数がこれを超えても false negative は出ない（誤検出率が上がるだけ）。
    /// I/O 例外は呼び出し側の `protect` が処理する。
    /// </remarks>
    let writeTo
        (stream: System.IO.Stream)
        (bloomFalsePositiveRate: float)
        (expectedEntries: int)
        (entries: SstEntry seq)
        =
        let bloom = BloomFilter.BloomBuilder(max 1 expectedEntries, bloomFalsePositiveRate)
        use e = entries.GetEnumerator()
        writeLoop stream bloom e None None 0UL 0L 0 0 [] None []
