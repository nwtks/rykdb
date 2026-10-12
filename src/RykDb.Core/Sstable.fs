// ===========================================================================
// SSTable — 不変のソート済みファイル。メモリ上に展開した byte[] として扱う。
// ブロック境界と索引だけを公開し、ファイル I/O とブロックキャッシュは StorageEngine が担う。
//
//   [Block 0]...[Block N-1][Meta][Bloom][Index][Footer 80 bytes]
//
// 各ブロックは LevelDB 風の restart 配列を持ち、同じ接頭辞を持つ連続エントリの
// キー共有部分を圧縮する（entry 形式は buildBlock のコメント）。
// ===========================================================================

namespace RykDb.Core

open System.IO

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
    /// 並び。キー昇順・同一キーは SeqNo 降順（新しい版が先）。
    let compare (a: SstEntry) (b: SstEntry) =
        match Bytes.compare a.Key b.Key with
        | 0 -> compare b.SeqNo a.SeqNo
        | c -> c

    /// 値部分（SeqNo + 種別 + 行値）を符号化する。
    let encodeValue (e: SstEntry) =
        let tag, values =
            e.Value |> Option.map (fun vs -> 1uy, vs) |> Option.defaultValue (0uy, [||])

        Writer.build [ Writer.varint e.SeqNo; Writer.writeByte tag; ValueCodec.writeValues values ]

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

    /// 配列を n 個ずつの塊に分ける（SSTable のブロック分けと compaction の出力分けで共用）。
    let splitInto (n: int) (items: 'a array) =
        let rec go acc start =
            if start >= items.Length then
                List.rev acc
            else
                let len = max 1 (min n (items.Length - start))
                go (Array.sub items start len :: acc) (start + len)

        go [] 0

    /// 「キー昇順・同一キーは SeqNo 降順」で並んだ列たちを k-way merge する。
    /// ソース数は高々 1 + ファイル数なので、head を線形に走査しても十分速い。
    type private MergeHead =
        {
            Id: int
            Items: SstEntry array
            Index: int
        }

    let mergeSorted (sources: SstEntry array list) =
        let rec go (heads: MergeHead list) (acc: SstEntry list) =
            let pick =
                heads
                |> List.filter (fun h -> h.Index < h.Items.Length)
                |> List.fold
                    (fun best h ->
                        match best with
                        | None -> Some h
                        | Some b ->
                            if compare h.Items.[h.Index] b.Items.[b.Index] < 0 then
                                Some h
                            else
                                Some b)
                    None

            match pick with
            | None -> List.rev acc |> Array.ofList
            | Some h ->
                let e = h.Items.[h.Index]
                let advanced = { h with Index = h.Index + 1 }
                let replaced = heads |> List.mapi (fun i x -> if i = h.Id then advanced else x)
                go replaced (e :: acc)

        go (sources |> List.mapi (fun i a -> { Id = i; Items = a; Index = 0 })) []

    /// エントリ列に含まれる SeqNo の最大値（空なら 0）。
    let maxSeqNo (items: seq<SstEntry>) =
        items |> Seq.fold (fun acc e -> max acc e.SeqNo) 0UL

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

    /// restart 配列の 1エントリのバイト数（オフセットと件数はどちらも u32）。
    /// 書く側（buildBlock）と読む側（parseBlock）が共有する。
    [<Literal>]
    let private RestartEntryBytes = 4

    [<Literal>]
    let private Magic = "RykDbSST"

    [<Literal>]
    let private Version = 1

    /// SSTable のファイル名（%06d.sst）。StorageEngine と Compaction が共有する。
    let private fileName (fileNumber: int64) = sprintf "%06d.sst" fileNumber

    /// SSTable のフルパス。
    let path (dir: string) (fileNumber: int64) = Path.Combine(dir, fileName fileNumber)

    /// 2 つのキーの共通接頭辞長。
    let private sharedPrefix (prev: byte array) (key: byte array) =
        let n = min prev.Length key.Length

        let rec go i =
            if i >= n then 0
            elif prev.[i] = key.[i] then go (i + 1)
            else i

        go 0

    /// バイト列の列の、各要素の開始オフセットを求める（累積バイト数）。
    /// 結果は要素数 + 1 個で、最後が全体のバイト数。
    let private startOffsets (blocks: byte array list) =
        blocks |> List.scan (fun acc b -> acc + b.Length) 0

    /// Cursor から終端まで 1 件ずつ読み出して配列にまとめる。
    /// `step` は「読むたびに変わる状態」（キーの接頭辞共有など）を
    /// `(cursor, state) -> Result<cursor * 要素 * state, DbError>` として受け取る。
    /// 状態が要らないときは unit を渡す。
    /// 再帰は末尾位置の match で回す（bind を挟むと TCO が効かなくなる）。
    let private readItems
        (step: Cursor -> 'State -> Result<Cursor * 'Item * 'State, DbError>)
        (c: Cursor)
        (state: 'State)
        =
        let rec go (c: Cursor) (state: 'State) (acc: 'Item list) =
            if Cursor.atEnd c then
                ret (List.toArray (List.rev acc))
            else
                match step c state with
                | Error e -> Error e
                | Ok(c', item, state') -> go c' state' (item :: acc)

        go c state []

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

                Writer.build
                    [
                        Writer.varint (uint64 shared)
                        Writer.varint (uint64 (e.Key.Length - shared))
                        Writer.varint (uint64 value.Length)
                        Writer.writeBytes tail
                        Writer.writeBytes value
                    ])

        // restart の位置は累積バイト数から求める（RestartInterval 件ごと）
        let offsets = startOffsets (Array.toList body)

        let restarts =
            [ 0..RestartInterval .. body.Length - 1 ]
            |> List.map (fun i -> List.item i offsets)

        // restart 配列は [オフセット × n][件数]（各 u32 = RestartEntryBytes バイト）
        let restartBytes =
            Writer.build (
                (restarts |> List.map (fun off -> Writer.uint32LE (uint32 off)))
                @ [ Writer.uint32LE (uint32 restarts.Length) ]
            )

        Array.concat [| Array.concat body; restartBytes |]

    /// ブロックをエントリ列へ復元する。
    let private parseBlock (data: byte array) =
        // 読む側。キーは前のエントリと接頭辞を共有するので、直前のキーを状態で受け渡しする。
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

                return c, SstEntry.make key seqNo value, key
            }

        let n = data.Length

        // restart 配列（n 個のオフセット + 件数）が末尾に付く。最小编成は 2エントリ分。
        if n < 2 * RestartEntryBytes then
            Error(DbError.corruption "Sstable: ブロックが短すぎます")
        else
            let numRestarts = int (Read.readLE32 data (n - RestartEntryBytes))
            let restartArraySize = (numRestarts + 1) * RestartEntryBytes

            if numRestarts <= 0 || restartArraySize > n then
                Error(DbError.corruption "Sstable: restart 配列が不正です")
            else
                // restart 配列の手前までがエントリ領域。カーソルの End をそこに合わせる。
                readItems step (Cursor.createSlice data 0 (n - restartArraySize)) [||]

    /// Footer
    ///   magic(8) version(4) maxSeqNo(8) indexOff(8) indexLen(8)
    ///   bloomOff(8) bloomLen(8) metaOff(8) metaLen(8) entryCount(8) checksum(4)
    ///   = 8 + 4 + 8 * 8 + 4 = 80バイト（FooterSize と一致させる）
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
                    Writer.uint32LE (Hashing.checksum data)
                ]

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

    /// Footer をカーソルで順に読む。
    /// フィールドの並びは encodeFooter が実質のレイアウト定義なので、
    /// オフセットを手で数えない（順に読む分だけ両者がずれにくくなる）。
    /// 戻り値は (マジック文字列, バージョン, 本体, 格納チェックサム)。
    let private readFooter (c: Cursor) =
        result {
            let! magicBytes, c = Read.readBytes Magic.Length c
            let! version, c = Read.readUInt32LE c
            let! maxSeqNo, c = Read.readInt64LE c
            let! indexOff, c = Read.readInt64LE c
            let! indexLen, c = Read.readInt64LE c
            let! bloomOff, c = Read.readInt64LE c
            let! bloomLen, c = Read.readInt64LE c
            let! metaOff, c = Read.readInt64LE c
            let! metaLen, c = Read.readInt64LE c
            let! entryCount, c = Read.readInt64LE c
            let! stored, _ = Read.readUInt32LE c

            return
                System.Text.Encoding.ASCII.GetString magicBytes,
                int version,
                {
                    MaxSeqNo = uint64 maxSeqNo
                    IndexOff = int indexOff
                    IndexLen = int indexLen
                    BloomOff = int bloomOff
                    BloomLen = int bloomLen
                    MetaOff = int metaOff
                    MetaLen = int metaLen
                    EntryCount = int entryCount
                },
                stored
        }

    let private decodeFooter (data: byte array) =
        let n = data.Length

        if n < FooterSize then
            Error(DbError.corruption "Sstable: Footer がありません")
        else
            let footerStart = n - FooterSize

            match readFooter (Cursor.createSlice data footerStart FooterSize) with
            | Error e -> Error e
            | Ok(magic, version, info, stored) ->
                if magic <> Magic then
                    Error(DbError.corruption "Sstable: マジックが違います")
                elif version <> Version then
                    Error(DbError.corruption (sprintf "Sstable: 未対応のバージョン %d" version))
                elif
                    info.IndexOff < 0
                    || info.IndexLen < 0
                    || info.IndexOff + info.IndexLen > footerStart
                    || info.BloomOff < 0
                    || info.BloomLen < 0
                    || info.BloomOff + info.BloomLen > footerStart
                    || info.MetaOff < 0
                    || info.MetaLen < 0
                    || info.MetaOff + info.MetaLen > footerStart
                    || footerStart < 0
                then
                    Error(DbError.corruption "Sstable: Footer の範囲が不正です")
                elif Hashing.checksum (Array.sub data 0 footerStart) <> stored then
                    Error(DbError.corruption "Sstable: チェックサム不一致（ファイルが壊れています）")
                else
                    ret info

    // -----------------------------------------------------------------------
    // 構築
    // -----------------------------------------------------------------------

    /// エントリ列から SSTable のバイナリを作る。
    /// 入力はキー昇順・同一キーは SeqNo 降順に並んでいる必要がある（検証する）。
    let build (entries: SstEntry array) (bloomFalsePositiveRate: float) =
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
            let blocks = SstEntry.splitInto EntriesPerBlock entries
            let built = blocks |> List.map buildBlock
            let offsets = startOffsets built

            let indexEntries =
                blocks
                |> List.mapi (fun i items ->
                    {
                        LastKey = items.[items.Length - 1].Key
                        Offset = List.item i offsets
                        Length = (List.item i built).Length
                    })

            // offsets の最後がデータ領域のサイズ
            let dataSize = List.item built.Length offsets

            // 2) meta block（min_key / max_key）
            let metaBytes =
                match entries with
                | [||] -> [||]
                | _ ->
                    Writer.build
                        [
                            Writer.lengthPrefixed entries.[0].Key
                            Writer.lengthPrefixed entries.[entries.Length - 1].Key
                        ]

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
                |> List.collect (fun e ->
                    [
                        Writer.lengthPrefixed e.LastKey
                        Writer.int64LE (int64 e.Offset)
                        Writer.int64LE (int64 e.Length)
                    ])
                |> Writer.build

            let body =
                Array.concat (List.toArray (built @ [ metaBytes; bloomBytes; indexBytes ]))

            let maxSeqNo = SstEntry.maxSeqNo entries

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
        // 読む側。index エントリは互いに独立なので状態は要らない（unit）。
        let step (c: Cursor) (_: unit) =
            result {
                let! lastKey, c1 = Read.readLengthPrefixed c
                let! off, c2 = Read.readInt64LE c1
                let! len, c3 = Read.readInt64LE c2

                let entry: SstIndexEntry =
                    {
                        LastKey = lastKey
                        Offset = int off
                        Length = int len
                    }

                return c3, entry, ()
            }

        if length = 0 then
            ret [||]
        else
            readItems step (Cursor.create (Array.sub data offset length)) ()

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
        reader.Index
        |> Array.toList
        |> List.map (readBlock reader)
        |> all
        |> Result.map (fun chunks -> (chunks |> List.toArray |> Array.concat) :> seq<SstEntry>)

    /// startKey 以降のエントリをキー昇順で返す。
    /// <paramref name="limit"/> が 0 以下のときは上限なし（Memtable.rangeScan と揃える）。
    let rangeScan (reader: SstReader) (startKey: byte array) (limit: int) =
        entries reader
        |> Result.map (fun seq ->
            let filtered = seq |> Seq.filter (fun e -> Bytes.compare e.Key startKey >= 0)

            if limit > 0 then
                filtered |> Seq.truncate limit
            else
                filtered)
