// ===========================================================================
// SSTable — 不変のソート済みファイル。
// メモリ上に展開した byte[] として扱う。
// ブロック境界と索引だけを公開し、ファイル I/O とブロックキャッシュは StorageEngine が担う。
//
//   [Block 0]...[Block N-1][Meta][Bloom][Index][Footer 80 bytes]
//
// 各ブロックは LevelDB 風の restart 配列を持ち、同じ接頭辞を持つ連続エントリのキー共有部分を圧縮する
// （entry 形式は buildBlock のコメント）。
// ===========================================================================

namespace RykDb.Core

/// SSTable の 1エントリ。
type internal SstEntry =
    {
        Key: byte array
        SeqNo: uint64
        /// None が削除 tombstone
        Value: SqlValue array option
    }

    member x.IsDelete = x.Value.IsNone

module internal SstEntry =
    let make key seqNo value =
        {
            Key = key
            SeqNo = seqNo
            Value = value
        }

    /// 並び。
    /// キー昇順・同一キーは SeqNo 降順（新しい版が先）。
    let compare (a: SstEntry) (b: SstEntry) =
        match Bytes.compare a.Key b.Key with
        | 0 -> compare b.SeqNo a.SeqNo
        | c -> c

    /// 値部分（SeqNo + 種別 + 行値）を符号化する。
    let encodeValue (e: SstEntry) =
        let tag, values =
            e.Value |> Option.map (fun vs -> 1uy, vs) |> Option.defaultValue (0uy, [||])

        Writer.build [ Writer.varint e.SeqNo; Writer.writeByte tag; ValueCodec.writeValues values ]

    /// 値部分を復元する。
    /// キーは呼び出し側が付ける。
    let decodeValue (data: byte array) =
        result {
            let c = Cursor.create data
            let! seqNo, c = Reader.readVarint c
            let! tag, c = Reader.readByte c
            let! values, _ = ValueCodec.readValues c

            if tag = 0uy then
                return seqNo, None
            elif tag = 1uy then
                return seqNo, Some values
            else
                return! err (sprintf "Sstable: 不明な値タグ %d" (int tag) |> DbError.corruption)
        }

    /// 配列を n 個ずつの塊に分ける（SSTable のブロック分けに使う）。
    let splitInto (n: int) (items: 'a array) =
        let rec chunk acc start =
            if start >= items.Length then
                List.rev acc
            else
                let len = max 1 (min n (items.Length - start))
                chunk (Array.sub items start len :: acc) (start + len)

        chunk [] 0

    /// 1エントリの符号化後のおおよそのバイト数。
    /// compaction の出力をバイト幅で分けるための見積りで、実際の SSTable サイズとは厳密には一致しない。
    let approximateBytes (e: SstEntry) =
        let valueBytes (v: SqlValue) =
            match ValueCodec.fixedEncodedBytes v with
            | Some n -> n
            | None ->
                match v with
                | SqlValue.Text s -> 5 + System.Text.Encoding.UTF8.GetByteCount s
                | SqlValue.Blob bs -> 5 + bs.Length
                | _ -> 1

        let valueTotal =
            match e.Value with
            | None -> 2
            | Some vs -> Array.sumBy valueBytes vs + 4

        e.Key.Length + valueTotal

    /// 配列を「おおよそのバイト幅」で塊に分ける。
    /// 1塊は必ず 1エントリ以上で、超過する塊は要素が概算バイトを超えない限り targetBytes 以下になる。
    let splitIntoBytes (targetBytes: int64) (items: SstEntry array) =
        let rec chunk i segStart segSize acc =
            if i >= items.Length then
                if i = segStart then
                    List.rev acc
                else
                    List.rev (Array.sub items segStart (i - segStart) :: acc)
            elif i > segStart && segSize + int64 (approximateBytes items.[i]) > targetBytes then
                chunk i i 0L (Array.sub items segStart (i - segStart) :: acc)
            else
                chunk (i + 1) segStart (segSize + int64 (approximateBytes items.[i])) acc

        chunk 0 0 0L []

    /// 「キー昇順・同一キーは SeqNo 降順」で並んだ列たちを k-way merge する。
    /// ソース数は高々 1 + ファイル数なので、head を線形に走査しても十分速い。
    type private MergeHead =
        {
            Id: int
            Items: SstEntry array
            Index: int
        }

    let mergeSorted (sources: SstEntry array list) =
        let rec merge (heads: MergeHead list) (acc: SstEntry list) =
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
                merge replaced (e :: acc)

        merge (sources |> List.mapi (fun i a -> { Id = i; Items = a; Index = 0 })) []

    /// エントリ列に含まれる SeqNo の最大値（空なら 0）。
    let maxSeqNo (items: SstEntry seq) =
        items |> Seq.fold (fun acc e -> max acc e.SeqNo) 0UL

/// SSTable の遅延走査。
/// next を呼ぶまでブロックを読まない（読むのは 1個ずつ、読み終えたら次のブロックへ）。
/// ブロックの解析に失敗したら、その時点の next が Error を返して打ち切る
/// （ファイル全体のチェックサムは openTable が既に見ているので、
/// ここまで来て parse が落ちるのは掴んでいるファイルが壊れている場合だけ）。
type internal SstScan = SstScan of (unit -> Result<(SstEntry * SstScan) option, DbError>)

module internal SstScan =
    /// 終端（これ以上要素は無い）。
    let empty = SstScan(fun () -> ret None)

    /// 次の 1件。
    /// None は終端。
    let next (SstScan step) = step ()

    /// 1件を返す走査。
    /// 続き（rest）は次を要求されるまで組み立てない
    /// （ここで組み立てると 1回の next がブロック全体を辿ってしまい、遅延にならない）。
    let emit (e: SstEntry) (rest: unit -> SstScan) =
        SstScan(fun () -> Some(e, rest ()) |> ret)

    /// 残りを配列に畳む（壊れたブロックに当たったらその場で Error）。
    let toArray (scan: SstScan) =
        let rec drain (cur: SstScan) (acc: SstEntry list) =
            match next cur with
            | Error e -> Error e
            | Ok None -> List.rev acc |> Array.ofList |> ret
            | Ok(Some(e, nxt)) -> drain nxt (e :: acc)

        drain scan []

    /// 残りを遅延シーケンスに変換する。
    /// エラーが発生した場合はシーケンスが打ち切られる（エラーは呼び出し側で検出できない）。
    /// 正確なエラー伝播が必要な場合は toArray を使うこと。
    let toSeq (scan: SstScan) =
        let rec pull (cur: SstScan) =
            seq {
                match next cur with
                | Error _ -> ()
                | Ok None -> ()
                | Ok(Some(e, nxt)) ->
                    yield e
                    yield! pull nxt
            }

        pull scan

/// 索引の 1要素。
/// 各ブロックの位置と長さを張る。
type internal SstIndexEntry =
    {
        /// このブロックに含まれるキーのうち最大のもの
        LastKey: byte array
        Offset: int
        Length: int
    }

/// SSTable の読み取り側。
type internal SstReader =
    {
        Data: byte array
        Index: SstIndexEntry array
        MinKey: byte array
        MaxKey: byte array
        Bloom: BloomFilter option
        EntryCount: int64
        /// このファイルに含まれる SeqNo の最大値。
        /// GC が「消してもよい版」を判断するのに使う。
        MaxSeqNo: uint64
    }

module internal Sstable =
    /// Footer の固定長。
    [<Literal>]
    let private FooterSize = 80

    /// 1ブロックあたりのエントリ数。
    [<Literal>]
    let private EntriesPerBlock = 128

    /// restart 配列を挿入する間隔。
    [<Literal>]
    let private RestartInterval = 16

    /// restart 配列の 1エントリのバイト数（オフセットと件数はどちらも u32）。
    [<Literal>]
    let private RestartEntryBytes = 4

    [<Literal>]
    let private Magic = "RykDbSST"

    [<Literal>]
    let private Version = 1

    /// バイト列の列の、各要素の開始オフセットを求める（累積バイト数）。
    /// 結果は要素数 + 1 個で、最後が全体のバイト数。
    let private startOffsets (blocks: byte array list) =
        blocks |> List.scan (fun acc b -> acc + b.Length) 0

    /// Cursor から終端まで 1件ずつ読み出して配列にまとめる。
    /// step は「読むたびに変わる状態」（キーの接頭辞共有など）を
    /// `(cursor, state) -> Result<cursor * 要素 * state, DbError>` として受け取る。
    /// 状態が要らないときは unit を渡す。
    let private readItems
        (step: Cursor -> 'State -> Result<Cursor * 'Item * 'State, DbError>)
        (c: Cursor)
        (state: 'State)
        =
        let rec readAll (c: Cursor) (state: 'State) (acc: 'Item list) =
            if Cursor.atEnd c then
                ret (List.toArray (List.rev acc))
            else
                match step c state with
                | Error e -> Error e
                | Ok(c', item, state') -> readAll c' state' (item :: acc)

        readAll c state []

    /// エントリ列を 1ブロックのバイト列へ変換する。
    let private buildBlock (items: SstEntry array) =
        let sharedPrefix (prev: byte array) (key: byte array) =
            let n = min prev.Length key.Length

            let rec sharedLength i =
                if i >= n then 0
                elif prev.[i] = key.[i] then sharedLength (i + 1)
                else i

            sharedLength 0

        let encodeEntry (i: int) (e: SstEntry) =
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
                ]

        let body = items |> Array.mapi encodeEntry
        let offsets = Array.toList body |> startOffsets

        let restarts =
            [ 0..RestartInterval .. body.Length - 1 ]
            |> List.map (fun i -> List.item i offsets)

        let restartBytes =
            (restarts |> List.map (fun off -> Writer.uint32LE (uint32 off)))
            @ [ Writer.uint32LE (uint32 restarts.Length) ]
            |> Writer.build

        Array.concat [| Array.concat body; restartBytes |]

    /// ブロックをエントリ列へ復元する。
    let private parseBlock (data: byte array) =
        let decodeEntry (c: Cursor) (prev: byte array) =
            result {
                let! shared, c = Reader.readVarint c
                let! nonShared, c = Reader.readVarint c
                let! valueLen, c = Reader.readVarint c
                let! tail, c = Reader.readBytes (int nonShared) c
                let! valueBytes, c = Reader.readBytes (int valueLen) c
                let! seqNo, value = SstEntry.decodeValue valueBytes

                let key =
                    if shared = 0UL then
                        tail
                    else
                        Array.append (Array.take (int shared) prev) tail

                return c, SstEntry.make key seqNo value, key
            }

        let n = data.Length

        if n < 2 * RestartEntryBytes then
            err (DbError.corruption "Sstable: ブロックが短すぎます")
        else
            let numRestarts = int (Bytes.readLE32 data (n - RestartEntryBytes))
            let restartArraySize = (numRestarts + 1) * RestartEntryBytes

            if numRestarts <= 0 || restartArraySize > n then
                err (DbError.corruption "Sstable: restart 配列が不正です")
            else
                // restart 配列の手前までがエントリ領域。カーソルの End をそこに合わせる。
                readItems decodeEntry (Cursor.createSlice data 0 (n - restartArraySize)) [||]

    // Footer
    //   magic(8) version(4) maxSeqNo(8) indexOff(8) indexLen(8)
    //   bloomOff(8) bloomLen(8) metaOff(8) metaLen(8) entryCount(8) checksum(4)
    //   = 8 + 4 + 8 * 8 + 4 = 80バイト（FooterSize と一致させる）
    //
    // maxSeqNo は GC のために必要。
    // 「最古の snapshot から見えない版」の判定に使う
    // （LevelDB の smallestSnapshot と同じ考え方）。

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

    let private decodeFooter (data: byte array) =
        let readFooter (c: Cursor) =
            result {
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
                let! stored, _ = Reader.readUInt32LE c

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

        let validate (info: FooterInfo) (magic: string) (version: int) (stored: uint32) (footerStart: int) =
            if magic <> Magic then
                err (DbError.corruption "Sstable: マジックが違います")
            elif version <> Version then
                err (sprintf "Sstable: 未対応のバージョン %d" version |> DbError.corruption)
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
                err (DbError.corruption "Sstable: Footer の範囲が不正です")
            elif Hashing.checksum (Array.sub data 0 footerStart) <> stored then
                err (DbError.corruption "Sstable: チェックサム不一致（ファイルが壊れています）")
            else
                ret info

        let footerStart = data.Length - FooterSize

        if footerStart < 0 then
            err (DbError.corruption "Sstable: Footer がありません")
        else
            match Cursor.createSlice data footerStart FooterSize |> readFooter with
            | Error e -> Error e
            | Ok(magic, version, info, stored) -> validate info magic version stored footerStart

    /// エントリ列から SSTable のバイナリを作る。
    /// 入力はキー昇順・同一キーは SeqNo 降順に並んでいる必要がある（検証する）。
    let build (entries: SstEntry array) (bloomFalsePositiveRate: float) =
        let metaBytesOf (entries: SstEntry array) =
            match entries with
            | [||] -> [||]
            | _ ->
                Writer.build
                    [
                        Writer.lengthPrefixed entries.[0].Key
                        Writer.lengthPrefixed entries.[entries.Length - 1].Key
                    ]

        let bloomBytesOf (entries: SstEntry array) (bloomFalsePositiveRate: float) =
            match entries with
            | [||] -> [||]
            | _ ->
                BloomFilter.ofKeys
                    (entries |> Array.collect (fun e -> [| e.Key |]))
                    entries.Length
                    bloomFalsePositiveRate
                |> BloomFilter.serialize

        let indexBytesOf (blocks: SstEntry array list) (built: byte array list) (offsets: int list) =
            blocks
            |> List.mapi (fun i items ->
                {
                    LastKey = items.[items.Length - 1].Key
                    Offset = List.item i offsets
                    Length = (List.item i built).Length
                })
            |> List.collect (fun e ->
                [
                    Writer.lengthPrefixed e.LastKey
                    Writer.int64LE (int64 e.Offset)
                    Writer.int64LE (int64 e.Length)
                ])
            |> Writer.build

        result {
            let unordered =
                entries
                |> Array.pairwise
                |> Array.exists (fun (a, b) -> SstEntry.compare a b > 0)

            if unordered then
                return! err (DbError.internalError "Sstable: エントリがキー順に並んでいません")

            let maxSeqNo = SstEntry.maxSeqNo entries
            let blocks = SstEntry.splitInto EntriesPerBlock entries
            let built = blocks |> List.map buildBlock
            let offsets = startOffsets built
            let dataSize = List.item built.Length offsets
            let metaOff = dataSize
            let metaBytes = metaBytesOf entries
            let bloomOff = metaOff + metaBytes.Length
            let bloomBytes = bloomBytesOf entries bloomFalsePositiveRate
            let indexOff = bloomOff + bloomBytes.Length
            let indexBytes = indexBytesOf blocks built offsets

            let body =
                Array.concat (List.toArray (built @ [ metaBytes; bloomBytes; indexBytes ]))

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

    /// SSTable を開く。
    let openTable (data: byte array) =
        let readMinMaxKey (data: byte array) (offset: int) (length: int) =
            let metaBytes = Array.sub data offset length

            match Cursor.create metaBytes |> Reader.readLengthPrefixed with
            | Ok(mn, c) ->
                match Reader.readLengthPrefixed c with
                | Ok(mx, _) -> mn, mx
                | Error _ -> [||], [||]
            | Error _ -> [||], [||]

        let readBloom (data: byte array) (offset: int) (length: int) =
            if length = 0 then
                ret None
            else
                Array.sub data offset length |> BloomFilter.deserialize |> Result.map Some

        let decodeIndexEntry (c: Cursor) (_: unit) =
            result {
                let! lastKey, c1 = Reader.readLengthPrefixed c
                let! off, c2 = Reader.readInt64LE c1
                let! len, c3 = Reader.readInt64LE c2

                let entry =
                    {
                        LastKey = lastKey
                        Offset = int off
                        Length = int len
                    }

                return c3, entry, ()
            }

        let readIndexBlock (data: byte array) (offset: int) (length: int) =
            if length = 0 then
                ret [||]
            else
                readItems decodeIndexEntry (Array.sub data offset length |> Cursor.create) ()

        result {
            let! footer = decodeFooter data
            let minKey, maxKey = readMinMaxKey data footer.MetaOff footer.MetaLen
            let! bloom = readBloom data footer.BloomOff footer.BloomLen
            let! indexEntries = readIndexBlock data footer.IndexOff footer.IndexLen

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
        | Some bloom -> Hashing.hashBytes key |> Hashing.mix |> BloomFilter.mightContain bloom

    /// startKey 以降のエントリをキー昇順で遅延生成する。
    /// index の二分探索で開始ブロックへ飛び、ブロックは要求されるまで読まない。
    /// ブロックの解析に失敗したら、その時点の next が Error を返す。
    /// limit が 0 以下のときは上限なし（Memtable.rangeScan と揃える）。
    let rangeScan (reader: SstReader) (startKey: byte array) (limit: int) =
        let rec search (lo: int) (hi: int) =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare reader.Index.[mid].LastKey startKey < 0 then
                    search (mid + 1) hi
                else
                    search lo mid

        let readBlock (reader: SstReader) (indexEntry: SstIndexEntry) =
            Array.sub reader.Data indexEntry.Offset indexEntry.Length |> parseBlock

        let rec fromPos (i: int) (pos: int) (items: SstEntry array) (count: int) =
            if pos >= items.Length then
                fromBlock (i + 1) count
            elif limit > 0 && count >= limit then
                SstScan.empty
            elif Bytes.compare items.[pos].Key startKey < 0 then
                fromPos i (pos + 1) items count
            else
                SstScan.emit items.[pos] (fun () -> fromPos i (pos + 1) items (count + 1))

        and fromBlock (i: int) (count: int) =
            if i >= reader.Index.Length || limit > 0 && count >= limit then
                SstScan.empty
            else
                SstScan(fun () ->
                    match readBlock reader reader.Index.[i] with
                    | Error e -> Error e
                    | Ok items -> SstScan.next (fromPos i 0 items count))

        fromBlock (search 0 reader.Index.Length) 0

    /// 先頭から全エントリを配列で返す（全件必要ならこちら）。
    let entries (reader: SstReader) =
        rangeScan reader [||] 0 |> SstScan.toArray

/// 1つの SSTable のメタデータ。
type internal FileMetaData =
    {
        FileNumber: int64
        Level: int
        /// このファイルが持つ最小キー
        Smallest: byte array
        /// このファイルが持つ最大キー
        Largest: byte array
        EntryCount: int64
        /// ファイルサイズ（バイト）
        Size: int64
        /// このファイルに含まれる SeqNo の最大値。
        /// GC の判定に使う：
        /// 「最古の active snapshot より小さい SeqNo の版はもう誰にも見えないので捨てられる」ために使う。
        MaxSeqNo: uint64
    }

module internal FileMetaData =
    /// キー範囲も件数も未確定の空メタデータ。
    /// デコード途中の「番号とレベルだけ分かった」状態や、「compaction なし」の報告に使う
    /// （フィールドは上書きする）。
    let zero =
        {
            FileNumber = 0L
            Level = 0
            Smallest = [||]
            Largest = [||]
            EntryCount = 0L
            Size = 0L
            MaxSeqNo = 0UL
        }

    /// SSTable のファイル名（%06d.sst）。
    let private fileName (fileNumber: int64) = sprintf "%06d.sst" fileNumber

    /// SSTable のフルパス。
    let path (dir: string) (fileNumber: int64) =
        System.IO.Path.Combine(dir, fileName fileNumber)

    /// エントリ列を SSTable としてビルドし、ファイルに書き、メタデータを返す。
    /// ビルド・書き出し・メタ算出が 1つの経路を通るので、「書いたバイト数」と Size が食い違うことが無い。
    let writeSstable
        (dir: string)
        (fileNumber: int64)
        (level: int)
        (bloomFalsePositiveRate: float)
        (entries: SstEntry array)
        =
        let ofChunk (fileNumber: int64) (level: int) (size: int64) (entries: SstEntry array) =
            {
                FileNumber = fileNumber
                Level = level
                Smallest = entries.[0].Key
                Largest = entries.[entries.Length - 1].Key
                EntryCount = int64 entries.Length
                Size = size
                MaxSeqNo = SstEntry.maxSeqNo entries
            }

        result {
            let! bytes = Sstable.build entries bloomFalsePositiveRate

            let! () =
                protect (fun () ->
                    System.IO.File.WriteAllBytes(path dir fileNumber, bytes)

                    use fs =
                        new System.IO.FileStream(
                            path dir fileNumber,
                            System.IO.FileMode.Open,
                            System.IO.FileAccess.Write,
                            System.IO.FileShare.None
                        )

                    fs.Flush true)

            return ofChunk fileNumber level (int64 bytes.Length) entries
        }

    /// このファイルが [key] を持ち得るか（Bloom より粗い判定）。
    let mightContain (f: FileMetaData) (key: byte array) =
        Bytes.compare f.Smallest key <= 0 && Bytes.compare key f.Largest <= 0

    /// ファイル列を先頭から順に処理する「回す側」。
    /// read が 1ファイル分を読み、step が状態を進める。
    /// 最初の Error で打ち切る。
    /// read / step は再帰しないので bind で合成してよい。
    let foldFiles
        (read: FileMetaData -> Result<'a, DbError>)
        (step: 's -> 'a -> 's)
        (state: 's)
        (files: FileMetaData list)
        =
        let rec foldEach (rest: FileMetaData list) (acc: 's) =
            match rest with
            | [] -> ret acc
            | f :: tail ->
                match read f with
                | Error e -> Error e
                | Ok v -> foldEach tail (step acc v)

        foldEach files state

    /// ファイルを実際に削除する。
    let deleteObsolete (dir: string) (files: FileMetaData list) =
        let deleteFile (dir: string) (fileNumber: int64) =
            protect (fun () ->
                let p = path dir fileNumber

                if System.IO.File.Exists p then
                    System.IO.File.Delete p)

        files |> foldOk (fun () f -> deleteFile dir f.FileNumber) ()
