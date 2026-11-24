// ===========================================================================
// SSTable のファイル形式 — 定数・Footer・読み取りメタデータ・書き出し。
// 読み取り（部分読み・走査・ポイント検索）は `Sstable.fs`。
//
//   [Block 0]...[Block N-1][Meta][Bloom][Index][Footer 84 bytes]
//
// 各ブロックは同じ接頭辞を持つ連続エントリのキー共有部分を圧縮する。
// ===========================================================================

namespace RykDb.Core

/// 索引の 1要素。
/// 各ブロックの位置と長さを張る。
type internal SstIndexEntry =
    {
        /// このブロックに含まれるキーのうち最大のもの
        LastKey: byte array
        Offset: int
        Length: int
    }

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
        MaxSeqNo: uint64
    }

/// `SstableFormat.writeTo` の結果。
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
        MaxSeqNo: uint64
        /// 索引（ブロック位置）。
        Index: SstIndexEntry array
        /// 書き出した Bloom filter（0 件なら None）。
        Bloom: BloomFilter option
    }

module internal SstableFormat =
    /// 1ブロックあたりのエントリ数。
    [<Literal>]
    let private EntriesPerBlock = 128

    [<Literal>]
    let internal Magic = "RykDbSST"

    /// 書き出すファイルのバージョン。
    /// v6 はブロック末尾・メタデータ（meta / Bloom / index）・Footer を CRC32C で検証する。
    /// 全体チェックサムは持たない（部分読みでは読まなかったブロックは検証されない）。
    /// v6 未満は読めない。
    [<Literal>]
    let internal Version = 6

    /// 読み取れる下限バージョン。
    [<Literal>]
    let internal MinReadableVersion = 6

    /// 最新 Footer の固定長（本体 80 + Footer チェックサム 4）。
    [<Literal>]
    let internal FooterSize = 84

    /// Footer で footerChecksum がカバーするバイト数（metadataChecksum フィールド込み）。
    [<Literal>]
    let private FooterChecksumLength = 80

    // Footer
    //   magic(8) version(4) maxSeqNo(8) indexOff(8) indexLen(8)
    //   bloomOff(8) bloomLen(8) metaOff(8) metaLen(8) entryCount(8)
    //   metadataChecksum(4) footerChecksum(4)
    //   = 8 + 4 + 8 * 8 + 4 + 4 = 84バイト（FooterSize と一致させる）
    // footerChecksum は先頭 80バイト（metadataChecksum を含む）のチェックサム。
    // 部分読みではこのフッターだけを検証できる。
    //
    // 全体 checksum は持たない。
    // データブロックはそれぞれ末尾のチェックサムで守られ、
    // meta / Bloom / index は metadataChecksum、
    // Footer は footerChecksum で守られる。
    // 部分読みでは読まなかったブロックを検証できないが、全体 checksum があっても同じだった。
    //
    // maxSeqNo は起動時の NextSeqNo 復元のために必要。
    // ファイル内の最大 SeqNo を保存する
    //（GC は版ごとの SeqNo と active snapshot で判定する）。

    let private encodeFooter
        (maxSeqNo: uint64)
        (indexOff: int)
        (indexLen: int)
        (bloomOff: int)
        (bloomLen: int)
        (metaOff: int)
        (metaLen: int)
        (entryCount: int)
        (metadataChecksum: uint32)
        =
        let body =
            Writer.build
                [
                    Writer.writeBytes (System.Text.Encoding.ASCII.GetBytes Magic)
                    Writer.uint32LE (uint32 Version)
                    Writer.int64LE (int64 maxSeqNo)
                    Writer.int64LE (int64 indexOff)
                    Writer.int64LE (int64 indexLen)
                    Writer.int64LE (int64 bloomOff)
                    Writer.int64LE (int64 bloomLen)
                    Writer.int64LE (int64 metaOff)
                    Writer.int64LE (int64 metaLen)
                    Writer.int64LE (int64 entryCount)
                    Writer.uint32LE metadataChecksum
                ]

        let footerChecksum = Hashing.checksum body
        Writer.build [ Writer.writeBytes body; Writer.uint32LE footerChecksum ]

    /// footer を decode した情報。
    type internal FooterInfo =
        {
            Version: int
            MaxSeqNo: uint64
            IndexOff: int
            IndexLen: int
            BloomOff: int
            BloomLen: int
            MetaOff: int
            MetaLen: int
            EntryCount: int
            MetadataChecksum: uint32
        }

    /// Footer の値が int に収まるかを判定する。
    let private fitsInt (v: int64) =
        v >= 0L && v <= int64 System.Int32.MaxValue

    let internal readFooterFields (footerBytes: byte array) =
        result {
            let c = Cursor.create footerBytes
            let! magicBytes, c = Reader.readBytes Magic.Length c
            let! version, c = Reader.readUInt32LE c
            let! maxSeqNo, c = Reader.readInt64LE c
            let! indexOff, c = Reader.readInt64LE c
            let! indexLen, c = Reader.readInt64LE c
            let! bloomOff, c = Reader.readInt64LE c
            let! bloomLen, c = Reader.readInt64LE c
            let! metaOff, c = Reader.readInt64LE c
            let! metaLen, c = Reader.readInt64LE c
            let! entryCount, c = Reader.readInt64LE c
            let! metadataChecksum, c = Reader.readUInt32LE c
            let! footerChecksum, _ = Reader.readUInt32LE c

            if Hashing.checksumRange footerBytes 0 FooterChecksumLength <> footerChecksum then
                return! err (DbError.corruption "Sstable: Footer のチェックサム不一致")
            else if
                not (fitsInt indexOff)
                || not (fitsInt indexLen)
                || not (fitsInt bloomOff)
                || not (fitsInt bloomLen)
                || not (fitsInt metaOff)
                || not (fitsInt metaLen)
                || not (fitsInt entryCount)
            then
                return! err (DbError.corruption "Sstable: Footer の値が大きすぎます")
            else
                return
                    System.Text.Encoding.ASCII.GetString magicBytes,
                    {
                        Version = int version
                        MaxSeqNo = uint64 maxSeqNo
                        IndexOff = int indexOff
                        IndexLen = int indexLen
                        BloomOff = int bloomOff
                        BloomLen = int bloomLen
                        MetaOff = int metaOff
                        MetaLen = int metaLen
                        EntryCount = int entryCount
                        MetadataChecksum = metadataChecksum
                    }
        }

    /// 共通接頭辞の長さを先頭から数える。
    [<TailCall>]
    let rec private sharedLength (prev: byte array) (key: byte array) (n: int) (i: int) =
        if i >= n then n
        elif prev.[i] = key.[i] then sharedLength prev key n (i + 1)
        else i

    /// 2つのキーの共通接頭辞の長さを返す。
    let private sharedPrefix (prev: byte array) (key: byte array) =
        let n = min prev.Length key.Length
        sharedLength prev key n 0

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
    let private writeEntry (out: byte array) pos (prevKey: byte array) (e: SstEntry) =
        let shared = sharedPrefix prevKey e.Key
        let tailLength = e.Key.Length - shared
        let valueLength = SstEntry.encodedValueLength e
        let pos = Writer.writeVarintTo out pos (uint64 shared)
        let pos = Writer.writeVarintTo out pos (uint64 tailLength)
        let pos = Writer.writeVarintTo out pos (uint64 valueLength)
        Array.blit e.Key shared out pos tailLength
        SstEntry.writeValueTo out (pos + tailLength) e

    /// ブロック本体のバイト長を数える。
    [<TailCall>]
    let rec private bodyLength (items: SstEntry array) (i: int) (prev: byte array) (total: int) =
        if i >= items.Length then
            total
        else
            bodyLength items (i + 1) items.[i].Key (total + entryLength prev items.[i])

    /// エントリをブロックへ順に書き込む。
    [<TailCall>]
    let rec private writeEntries (items: SstEntry array) (block: byte array) (i: int) (prev: byte array) (pos: int) =
        if i >= items.Length then
            ret pos
        else
            match writeEntry block pos prev items.[i] with
            | Ok next -> writeEntries items block (i + 1) items.[i].Key next
            | Error e -> err e

    /// エントリ列からチェックサム付きのブロックを組み立てる。
    let private buildBlock (items: SstEntry array) =
        let length = bodyLength items 0 [||] 0
        let block = Array.zeroCreate<byte>(length + 4)

        writeEntries items block 0 [||] 0
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
        (items: SstEntry array)
        (offset: int)
        (indexEntries: SstIndexEntry list)
        =
        buildBlock items
        >>= fun data ->
            let nextOffset = int64 offset + int64 data.Length

            if nextOffset > int64 System.Int32.MaxValue then
                err (DbError.internalError "Sstable: ファイルが大きすぎます")
            else
                stream.Write(data, 0, data.Length)

                let entry =
                    {
                        LastKey = items.[items.Length - 1].Key
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

    /// ブロック列・Bloom・索引・フッターを書き出し、結果を返す。
    let private finishWrite
        (stream: System.IO.Stream)
        (bloom: BloomFilter.BloomBuilder)
        (firstKey: byte array option)
        (lastKey: byte array option)
        (maxSeqNo: uint64)
        (count: int64)
        (block: SstEntry list)
        (offset: int)
        (indexEntries: SstIndexEntry list)
        =
        (if List.isEmpty block then
             ret (offset, indexEntries)
         else
             writeBlock stream (block |> List.rev |> List.toArray) offset indexEntries)
        >>= fun (dataSize, indexEntries) ->
            let metaBytes =
                match firstKey, lastKey with
                | Some first, Some last -> Writer.build [ Writer.lengthPrefixed first; Writer.lengthPrefixed last ]
                | _ -> [||]

            let bloomOff = int64 dataSize + int64 metaBytes.Length
            let bloomFilter = if count = 0L then None else Some(bloom.Build())

            let bloomBytes =
                bloomFilter |> Option.map BloomFilter.serialize |> Option.defaultValue [||]

            let indexOff = bloomOff + int64 bloomBytes.Length
            let index = List.rev indexEntries |> List.toArray
            let indexBytes = indexBytesOf index

            if indexOff + int64 indexBytes.Length > int64 System.Int32.MaxValue then
                err (DbError.internalError "Sstable: ファイルが大きすぎます")
            else
                let metadataChecksum =
                    Hashing.checksumChunks [| metaBytes; bloomBytes; indexBytes |]

                stream.Write(metaBytes, 0, metaBytes.Length)
                stream.Write(bloomBytes, 0, bloomBytes.Length)
                stream.Write(indexBytes, 0, indexBytes.Length)

                let footer =
                    encodeFooter
                        maxSeqNo
                        (int indexOff)
                        indexBytes.Length
                        (int bloomOff)
                        bloomBytes.Length
                        dataSize
                        metaBytes.Length
                        (int count)
                        metadataChecksum

                stream.Write(footer, 0, footer.Length)

                ret
                    {
                        Size = indexOff + int64 indexBytes.Length + int64 footer.Length
                        FirstKey = defaultArg firstKey [||]
                        LastKey = defaultArg lastKey [||]
                        EntryCount = count
                        MaxSeqNo = maxSeqNo
                        Index = index
                        Bloom = bloomFilter
                    }

    /// エントリをブロック単位で書き出す（末尾再帰）。
    [<TailCall>]
    let rec private writeLoop
        (stream: System.IO.Stream)
        (bloom: BloomFilter.BloomBuilder)
        (e: System.Collections.Generic.IEnumerator<SstEntry>)
        (firstKey: byte array option)
        (lastKey: byte array option)
        (maxSeqNo: uint64)
        (count: int64)
        (block: SstEntry list)
        (blockCount: int)
        (offset: int)
        (indexEntries: SstIndexEntry list)
        (prev: SstEntry option)
        =
        if blockCount >= EntriesPerBlock then
            match writeBlock stream (block |> List.rev |> List.toArray) offset indexEntries with
            | Ok(nextOffset, indexEntries') ->
                writeLoop stream bloom e firstKey lastKey maxSeqNo count [] 0 nextOffset indexEntries' prev
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
                    (item :: block)
                    (blockCount + 1)
                    offset
                    indexEntries
                    (Some item)
        else
            finishWrite stream bloom firstKey lastKey maxSeqNo count block offset indexEntries

    /// エントリ列を SSTable v6 として stream に書き出し、書き込んだ結果を返す。
    /// <remarks>
    /// 入力は `seq` で **1回だけ列挙する**（memtable の flush が全件を配列にしないため）。
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
        (entries: seq<SstEntry>)
        =
        let bloom = BloomFilter.BloomBuilder(max 1 expectedEntries, bloomFalsePositiveRate)
        use e = entries.GetEnumerator()
        writeLoop stream bloom e None None 0UL 0L [] 0 0 [] None
