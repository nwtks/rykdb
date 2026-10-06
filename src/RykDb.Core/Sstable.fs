// ===========================================================================
// SSTable — 不変のソート済みファイル。メモリ上に展開した byte[] として扱う。
// ブロック境界と索引だけを公開し、ファイル I/O とブロックキャッシュは
// StorageEngine が担う。
//
//   [Block 0]...[Block N-1][Meta][Bloom][Index][Footer 80 bytes]
//
// 各ブロックは LevelDB 風の restart 配列を持ち、同じ接頭辞を持つ連続
// エントリのキー共有部分を圧縮する（entry 形式は buildBlock のコメント）。
// レイアウトの詳細は docs/architecture.md §9.2。
// ===========================================================================

namespace RykDb.Core

/// SSTable の 1 エントリ。
type internal SstEntry =
    {
        Key: byte array
        SeqNo: uint64
        /// None が削除 tombstone
        Value: SqlValue array option
    }

    member x.IsDelete = x.Value.IsNone

module internal SstEntry =
    /// バイト列の昇順比較（前方が短いほうが小さい）。
    let compareBytes (a: byte array) (b: byte array) =
        let n = min a.Length b.Length

        let rec go i =
            if i >= n then compare a.Length b.Length
            elif a.[i] = b.[i] then go (i + 1)
            else compare (int a.[i]) (int b.[i])

        go 0

    /// 並び。キー昇順・同一キーは SeqNo 降順（新しい版が先）。
    let compare (a: SstEntry) (b: SstEntry) =
        match compareBytes a.Key b.Key with
        | 0 -> compare b.SeqNo a.SeqNo
        | c -> c

    /// 値部分（SeqNo + 種別 + 行値）を符号化する。
    let encodeValue (e: SstEntry) =
        let tag, values =
            e.Value |> Option.map (fun vs -> 1uy, vs) |> Option.defaultValue (0uy, [||])

        Writer.empty
        |> Writer.varint e.SeqNo
        |> fun w -> Writer.writeByte tag w
        |> fun w -> ValueCodec.writeValues values w
        |> Writer.toArray

    /// 値部分を復元する。キーは呼び出し側が付ける。
    let decodeValue (data: byte array) =
        result {
            let c = Cursor.create data
            let! seqNo, c = Read.readVarint c
            let! tag, c = Read.readByte c
            let! values, _ = ValueCodec.readValues c

            if tag = 0uy then
                return seqNo, None
            elif tag = 1uy then
                return seqNo, Some values
            else
                return! Error(DbError.corruption (sprintf "Sstable: 不明な値タグ %d" (int tag)))
        }

    let make key seqNo value =
        {
            Key = key
            SeqNo = seqNo
            Value = value
        }

/// 索引の 1 要素。各ブロックの位置と長さを張る。
type internal SstIndexEntry =
    {
        /// このブロックに含まれるキーのうち最大のもの
        LastKey: byte array
        Offset: int
        Length: int
    }

/// SSTable の読み取り側。構築後は不変。
type internal SstReader =
    {
        Data: byte array
        Index: SstIndexEntry array
        MinKey: byte array
        MaxKey: byte array
        Bloom: BloomFilter option
        EntryCount: int64
        /// このファイルに含まれる SeqNo の最大値。GC が「消してもよい版」を判断するのに使う
        MaxSeqNo: uint64
    }

module internal Sstable =
    /// Footer の固定長。
    [<Literal>]
    let private FooterSize = 80

    /// 1 ブロックあたりのエントリ数。
    [<Literal>]
    let private EntriesPerBlock = 128

    /// restart 配列を挿入する間隔。
    [<Literal>]
    let private RestartInterval = 16

    [<Literal>]
    let private Magic = "RykDbSST"

    [<Literal>]
    let private Version = 1

    // -----------------------------------------------------------------------
    // 補助
    // -----------------------------------------------------------------------

    /// FNV-1a 32bit。CRC32C の代わりに軽量な破損検出に使う。
    let private checksum (data: byte array) =
        Array.fold (fun h b -> (h ^^^ uint32 b) * 16777619u) 2166136261u data

    let private le32 (v: uint32) =
        [|
            for i in 0..3 do
                yield byte (v >>> 8 * i)
        |]

    let private le64 (v: int64) =
        [|
            for i in 0..7 do
                yield byte (v >>> 8 * i)
        |]

    let private readLE32 (data: byte array) (offset: int) =
        let at (i: int) = uint32 data.[offset + i]
        at 0 ||| (at 1 <<< 8) ||| (at 2 <<< 16) ||| (at 3 <<< 24)

    let private readLE64 (data: byte array) (offset: int) =
        int64 (readLE32 data offset) ||| (int64 (readLE32 data (offset + 4)) <<< 32)

    /// 2 つのキーの共通接頭辞長。
    let private sharedPrefix (prev: byte array) (key: byte array) =
        let n = min prev.Length key.Length

        let rec go i =
            if i >= n then 0
            elif prev.[i] = key.[i] then go (i + 1)
            else i

        go 0

    // -----------------------------------------------------------------------
    // ブロック
    // -----------------------------------------------------------------------

    /// エントリ列を 1 ブロックのバイト列へ変換する。
    let private buildBlock (items: SstEntry array) =
        let body =
            items
            |> Array.mapi (fun i e ->
                let prev = if i = 0 then [||] else items.[i - 1].Key
                let shared = sharedPrefix prev e.Key
                let tail = e.Key |> Array.skip shared
                let value = SstEntry.encodeValue e

                Writer.empty
                |> Writer.varint (uint64 shared)
                |> fun w -> Writer.varint (uint64 (e.Key.Length - shared)) w
                |> fun w -> Writer.varint (uint64 value.Length) w
                |> fun w -> Writer.writeBytes tail w
                |> Writer.writeBytes value
                |> Writer.toArray)

        // restart の位置は累積バイト数から求める
        let restartSet =
            Set.ofList [ 0..RestartInterval .. System.Math.Max(0, body.Length - 1) ]

        let _, restartsRev =
            [ 0 .. body.Length - 1 ]
            |> List.fold
                (fun (running, acc) i ->
                    let acc = if restartSet.Contains i then (i, running) :: acc else acc
                    running + body.[i].Length, acc)
                (0, [])

        let restarts = List.rev restartsRev

        let restartBytes =
            Array.concat (
                (restarts |> List.map (fun (_, off) -> le32 (uint32 off)))
                @ [ le32 (uint32 restarts.Length) ]
            )

        Array.concat [| Array.concat body; restartBytes |]

    /// ブロックをエントリ列へ復元する。
    let private parseBlock (data: byte array) =
        let step (c: Cursor) (prev: byte array) =
            result {
                let! shared, c = Read.readVarint c
                let! nonShared, c = Read.readVarint c
                let! valueLen, c = Read.readVarint c
                let! tail, c = Read.readBytes (int nonShared) c
                let! valueBytes, c = Read.readBytes (int valueLen) c
                let! seqNo, value = SstEntry.decodeValue valueBytes

                let key =
                    if shared = 0UL then
                        tail
                    else
                        Array.append (Array.take (int shared) prev) tail

                return c, key, SstEntry.make key seqNo value
            }

        let n = data.Length

        if n < 8 then
            Error(DbError.corruption "Sstable: ブロックが短すぎます")
        else
            let numRestarts = int (readLE32 data (n - 4))
            let restartArraySize = (numRestarts + 1) * 4

            if numRestarts <= 0 || restartArraySize > n then
                Error(DbError.corruption "Sstable: restart 配列が不正です")
            else
                let entryEnd = n - restartArraySize

                let rec go (c: Cursor) (prev: byte array) (acc: SstEntry list) =
                    if c.Pos >= entryEnd then
                        ret (List.toArray (List.rev acc))
                    else
                        match step c prev with
                        | Error e -> Error e
                        | Ok(c', key, entry) -> go c' key (entry :: acc)

                go (Cursor.createSlice data 0 entryEnd) [||] []

    /// Footer
    ///   magic(8) version(4) maxSeqNo(8) indexOff(8) indexLen(8)
    ///   bloomOff(8) bloomLen(8) metaOff(8) metaLen(8) entryCount(8) checksum(4)
    ///   = 8 + 4 + 8 * 8 + 4 = 80 バイト（FooterSize と一致させる）
    ///
    /// maxSeqNo は GC のために必要。
    /// 「最古の snapshot から見えない版」の判定に使う
    /// （LevelDB の smallestSnapshot と同じ考え方）。
    let private encodeFooter
        (data: byte array)
        (maxSeqNo: uint64)
        (indexOff: int)
        (indexLen: int)
        (bloomOff: int)
        (bloomLen: int)
        (metaOff: int)
        (metaLen: int)
        (entryCount: int)
        =
        let body =
            Array.concat
                [|
                    System.Text.Encoding.ASCII.GetBytes Magic
                    le32 (uint32 Version)
                    le64 (int64 maxSeqNo)
                    le64 (int64 indexOff)
                    le64 (int64 indexLen)
                    le64 (int64 bloomOff)
                    le64 (int64 bloomLen)
                    le64 (int64 metaOff)
                    le64 (int64 metaLen)
                    le64 (int64 entryCount)
                    le32 (checksum data)
                |]

        ret (Array.append data body)

    /// footer を decode した情報。
    type private FooterInfo =
        {
            MaxSeqNo: uint64
            IndexOff: int
            IndexLen: int
            BloomOff: int
            BloomLen: int
            MetaOff: int
            MetaLen: int
            EntryCount: int
        }

    let private decodeFooter (data: byte array) =
        let n = data.Length

        if n < FooterSize then
            Error(DbError.corruption "Sstable: Footer がありません")
        else
            let footerStart = n - FooterSize
            let magic = System.Text.Encoding.ASCII.GetString(data, footerStart, 8)

            if magic <> Magic then
                Error(DbError.corruption "Sstable: マジックが違います")
            else
                let version = int (readLE32 data (footerStart + 8))
                let maxSeqNo = readLE64 data (footerStart + 12) |> uint64
                let indexOff = int (readLE64 data (footerStart + 20))
                let indexLen = int (readLE64 data (footerStart + 28))
                let bloomOff = int (readLE64 data (footerStart + 36))
                let bloomLen = int (readLE64 data (footerStart + 44))
                let metaOff = int (readLE64 data (footerStart + 52))
                let metaLen = int (readLE64 data (footerStart + 60))
                let entryCount = int (readLE64 data (footerStart + 68))
                let stored = readLE32 data (footerStart + 76)

                if version <> Version then
                    Error(DbError.corruption (sprintf "Sstable: 未対応のバージョン %d" version))
                elif
                    indexOff < 0
                    || indexLen < 0
                    || indexOff + indexLen > footerStart
                    || bloomOff < 0
                    || bloomLen < 0
                    || bloomOff + bloomLen > footerStart
                    || metaOff < 0
                    || metaLen < 0
                    || metaOff + metaLen > footerStart
                    || footerStart < 0
                then
                    Error(DbError.corruption "Sstable: Footer の範囲が不正です")
                elif checksum (Array.sub data 0 footerStart) <> stored then
                    Error(DbError.corruption "Sstable: チェックサム不一致（ファイルが壊れています）")
                else
                    ret
                        {
                            MaxSeqNo = maxSeqNo
                            IndexOff = indexOff
                            IndexLen = indexLen
                            BloomOff = bloomOff
                            BloomLen = bloomLen
                            MetaOff = metaOff
                            MetaLen = metaLen
                            EntryCount = entryCount
                        }

    // -----------------------------------------------------------------------
    // 構築
    // -----------------------------------------------------------------------

    /// 配列を n 個ずつの塊に分ける。
    let private chunk (items: SstEntry array) (n: int) =
        let rec go acc start =
            if start >= items.Length then
                List.rev acc
            else
                let len = max 1 (min n (items.Length - start))
                go (Array.sub items start len :: acc) (start + len)

        go [] 0

    /// エントリ列から SSTable のバイナリを作る。
    /// 入力はキー昇順・同一キーは SeqNo 降順に並んでいる必要がある（検証する）。
    let build (entries: SstEntry array) (bloomFalsePositiveRate: float) =
        let offsetsOf (built: byte array list) =
            built |> List.scan (fun acc b -> acc + b.Length) 0

        result {
            // 並びの検証（ここでしか保証されない）
            // レコードの構造比較だと SeqNo が昇順になってしまうので、
            // SSTable の並び（キー昇順・同一キーは SeqNo 降順）で判定する
            let unordered =
                entries
                |> Array.pairwise
                |> Array.exists (fun (a, b) -> SstEntry.compare a b > 0)

            if unordered then
                return! Error(DbError.internalError "Sstable: エントリがキー順に並んでいません")

            // 1) ブロックへ切り分けて連結する。offset は累積から求める。
            let blocks = chunk entries EntriesPerBlock
            let built = blocks |> List.map buildBlock
            let offsets = offsetsOf built

            let indexEntries =
                blocks
                |> List.mapi (fun i items ->
                    {
                        LastKey = items.[items.Length - 1].Key
                        Offset = List.item i offsets
                        Length = (List.item i built).Length
                    })

            let dataSize =
                match built with
                | [] -> 0
                | _ -> List.item (built.Length - 1) offsets + (List.last built).Length

            // 2) meta block（min_key / max_key）
            let metaBytes =
                match entries with
                | [||] -> [||]
                | _ ->
                    Writer.empty
                    |> Writer.lengthPrefixed entries.[0].Key
                    |> fun w -> Writer.lengthPrefixed entries.[entries.Length - 1].Key w
                    |> Writer.toArray

            let metaOff = dataSize
            let bloomOff = metaOff + metaBytes.Length

            // 3) Bloom filter
            let bloomBytes =
                match entries with
                | [||] -> [||]
                | _ ->
                    BloomFilter.ofKeys
                        (entries |> Array.collect (fun e -> [| e.Key |]))
                        entries.Length
                        bloomFalsePositiveRate
                    |> BloomFilter.serialize

            // 4) index block
            let indexOff = bloomOff + bloomBytes.Length

            let indexBytes =
                indexEntries
                |> List.fold
                    (fun w e ->
                        w
                        |> Writer.lengthPrefixed e.LastKey
                        |> fun w -> Writer.int64LE (int64 e.Offset) w
                        |> Writer.int64LE (int64 e.Length))
                    Writer.empty
                |> Writer.toArray

            let body =
                Array.concat (List.toArray (built @ [ metaBytes; bloomBytes; indexBytes ]))

            let maxSeqNo =
                match entries with
                | [||] -> 0UL
                | _ -> entries |> Array.fold (fun acc e -> max acc e.SeqNo) 0UL

            let! withFooter =
                encodeFooter
                    body
                    maxSeqNo
                    indexOff
                    indexBytes.Length
                    bloomOff
                    bloomBytes.Length
                    metaOff
                    metaBytes.Length
                    entries.Length

            return withFooter
        }

    // -----------------------------------------------------------------------
    // 読み取り
    // -----------------------------------------------------------------------

    /// index block を SstIndexEntry 配列へ復元する。
    /// 空テーブルのときは index ブロック自体が無いので空配列を返す。
    let private readIndexBlock (data: byte array) (offset: int) (length: int) =
        let step (c: Cursor) =
            result {
                let! lastKey, c1 = Read.readLengthPrefixed c
                let! off, c2 = Read.readInt64LE c1
                let! len, c3 = Read.readInt64LE c2

                return
                    c3,
                    {
                        LastKey = lastKey
                        Offset = int off
                        Length = int len
                    }
            }

        let rec go (c: Cursor) (acc: SstIndexEntry list) =
            if Cursor.atEnd c then
                List.rev acc |> List.toArray |> ret
            else
                match step c with
                | Error e -> Error e
                | Ok(c', entry) -> go c' (entry :: acc)

        if length = 0 then
            ret [||]
        else
            go (Cursor.create (Array.sub data offset length)) []

    /// SSTable を開く。
    let openTable (data: byte array) =
        result {
            // F# の let! はタプルを 1 行で書く必要がある
            let! footer = decodeFooter data
            let! indexEntries = readIndexBlock data footer.IndexOff footer.IndexLen

            // meta block から min/max を取り出す
            let minKey, maxKey =
                let metaBytes = Array.sub data footer.MetaOff footer.MetaLen

                match Read.readLengthPrefixed (Cursor.create metaBytes) with
                | Ok(mn, c) ->
                    match Read.readLengthPrefixed c with
                    | Ok(mx, _) -> mn, mx
                    | Error _ -> [||], [||]
                | Error _ -> [||], [||]

            let! bloom =
                if footer.BloomLen = 0 then
                    ret None
                else
                    BloomFilter.deserialize (Array.sub data footer.BloomOff footer.BloomLen)
                    |> Result.map Some

            return
                {
                    Data = data
                    Index = indexEntries
                    MinKey = minKey
                    MaxKey = maxKey
                    Bloom = bloom
                    EntryCount = int64 footer.EntryCount
                    MaxSeqNo = footer.MaxSeqNo
                }
        }

    /// Bloom filter で「含まれ得る」か判定する。
    let mayContain (reader: SstReader) (key: byte array) =
        match reader.Bloom with
        | None -> true
        | Some bloom -> BloomFilter.mightContain bloom (Hashing.hashBytes key |> Hashing.mix)

    /// ブロック 1 個を読む。
    let private readBlock (reader: SstReader) (indexEntry: SstIndexEntry) =
        parseBlock (Array.sub reader.Data indexEntry.Offset indexEntry.Length)

    /// 先頭から全エントリを列挙する。
    let entries (reader: SstReader) =
        result {
            let! all =
                reader.Index
                |> Array.map (readBlock reader)
                |> Array.fold
                    (fun acc r ->
                        r
                        |> Result.bind (fun items ->
                            match acc with
                            | Ok prev -> ret (Array.append prev items)
                            | Error e -> Error e))
                    (ret [||])

            return all :> seq<SstEntry>
        }

    /// startKey 以降のエントリをキー昇順で返す。
    /// <paramref name="limit"/> が 0 以下のときは上限なし（Memtable.rangeScan と揃える）。
    let rangeScan (reader: SstReader) (startKey: byte array) (limit: int) =
        entries reader
        |> Result.map (fun seq ->
            let filtered =
                seq |> Seq.filter (fun e -> SstEntry.compareBytes e.Key startKey >= 0)

            if limit > 0 then
                filtered |> Seq.truncate limit
            else
                filtered)
