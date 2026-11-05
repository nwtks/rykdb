// ===========================================================================
// SSTable — 不変のソート済みファイル。
// 構築はブロック単位でストリームへ書き、読み取りは footer / index / Bloom の部分読みだけを行う。
//
//   [Block 0]...[Block N-1][Meta][Bloom][Index][Footer 84 bytes]
//
// 各ブロックは同じ接頭辞を持つ連続エントリのキー共有部分を圧縮する。
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

    /// 1エントリの符号化後のおおよそのバイト数。
    /// compaction の出力をバイト幅で分けるための見積りで、実際の SSTable サイズとは厳密には一致しない。
    let approximateBytes (e: SstEntry) =
        let valueTotal =
            match e.Value with
            | None -> 2
            | Some vs -> Array.sumBy ValueCodec.approximateBytes vs + 4

        e.Key.Length + valueTotal

/// SSTable の遅延走査。
/// `next` を呼ぶまでデータブロックを読まず、呼び出しごとに必要なブロックを1つずつ読む。
/// ブロックのチェックサムや内容の検証に失敗した場合は、そのブロックを読む `next` が `Error` を返す。
/// `openTable` は footer やメタデータを検証するが、まだ読んでいないデータブロックは検証しない。
type internal SstScan = SstScan of (unit -> Result<(SstEntry * SstScan) option, DbError>)

module internal SstScan =
    /// 終端（これ以上要素は無い）。
    let Empty = SstScan(fun () -> ret None)

    /// 次の 1件。
    /// None は終端。
    let next (SstScan step) = step ()

    /// 1件を返す走査。
    /// 続き（rest）は次を要求されるまで組み立てない
    /// （ここで組み立てると 1回の next がブロック全体を辿ってしまい、遅延にならない）。
    let emit (rest: unit -> SstScan) (e: SstEntry) =
        SstScan(fun () -> Some(e, rest ()) |> ret)

    /// 残りを配列に畳む（壊れたブロックに当たったらその場で Error）。
    let toArray (scan: SstScan) =
        let rec drain acc cur =
            match next cur with
            | Error e -> err e
            | Ok None -> List.rev acc |> Array.ofList |> ret
            | Ok(Some(e, nxt)) -> drain (e :: acc) nxt

        drain [] scan

    /// 条件を満たすエントリだけを返す走査。
    /// 条件を満たさない間は次のエントリを読み進める（ブロック単位のまま）。
    let filter (predicate: SstEntry -> bool) (scan: SstScan) =
        let rec find current =
            match next current with
            | Error e -> err e
            | Ok None -> ret None
            | Ok(Some(e, rest)) -> if predicate e then ret (Some(e, rest)) else find rest

        SstScan(fun () -> find scan)

    /// 純粋な seq を pull に変換する（テスト用のメモリ実装など）。
    /// seq の列挙は失敗しない前提（エラーは表現できない）。
    let ofSeq (items: seq<SstEntry>) =
        let e = items.GetEnumerator()

        let rec loop () =
            SstScan(fun () ->
                if e.MoveNext() then
                    ret (Some(e.Current, loop ()))
                else
                    (e :> System.IDisposable).Dispose() |> ignore
                    ret None)

        loop ()

/// k-way merge の 1ソース。
/// 先頭を 1件読んだ状態を表す。
type internal MergeHead =
    {
        /// ソースの識別子（完全に同じエントリが複数あるときの順序を安定させる）。
        Source: int
        Entry: SstEntry
        Scan: SstScan
    }

/// 左偏木のノード。
/// rank は右スパインの長さ。
type internal MergeNode =
    | MergeEmpty
    | MergeNode of int * MergeHead * MergeNode * MergeNode

/// 開いたソースの k-way merge。
/// <remarks>
/// 値として持ち回れる（1件読むたびに新しい `SstMerge` を返す）。
/// ヒープは左偏木で、`descend` を明示スタックにして自己再帰を消す。
/// 比較は `SstEntry.compare`（キー昇順・同一キーは SeqNo 降順）。
/// 完全に同じエントリが複数ソースにある場合はソース順で安定させる。
/// 壊れたブロックは「そのソースの次を読む」時点で Error になる。
/// </remarks>
type internal SstMerge = private SstMerge of MergeNode

module internal SstMerge =
    /// 空のマージ（すぐ終端）。
    let Empty = SstMerge MergeEmpty

    let private rank =
        function
        | MergeEmpty -> 0
        | MergeNode(r, _, _, _) -> r

    let private node head left right =
        if rank left >= rank right then
            MergeNode(rank right + 1, head, left, right)
        else
            MergeNode(rank left + 1, head, right, left)

    let private compareHead (a: MergeHead) (b: MergeHead) =
        match SstEntry.compare a.Entry b.Entry with
        | 0 -> compare a.Source b.Source
        | order -> order

    let private mergeHeaps (first: MergeNode) (second: MergeNode) =
        let rec descend picked a b =
            match a, b with
            | MergeEmpty, heap
            | heap, MergeEmpty -> picked, heap
            | MergeNode(_, ha, _, _), MergeNode(_, hb, _, _) when compareHead ha hb > 0 -> descend picked b a
            | MergeNode(_, ha, left, right), _ -> descend ((ha, left) :: picked) right b

        let picked, rest = descend [] first second
        List.fold (fun acc (head, left) -> node head left acc) rest picked

    /// 先頭を 1件読んだソースを登録する。
    let insert (source: int) (entry: SstEntry) (scan: SstScan) (SstMerge heap) =
        let singleton =
            MergeNode(
                1,
                {
                    Source = source
                    Entry = entry
                    Scan = scan
                },
                MergeEmpty,
                MergeEmpty
            )

        SstMerge(mergeHeaps heap singleton)

    /// 複数のスキャンをまとめて登録する。
    let ofScans (scans: SstScan array) =
        let ofScan source scan =
            match SstScan.next scan with
            | Error e -> err e
            | Ok None -> ret Empty
            | Ok(Some(entry, rest)) -> ret (insert source entry rest Empty)

        let union (SstMerge a) (SstMerge b) = SstMerge(mergeHeaps a b)

        let rec add i merge =
            if i >= scans.Length then
                ret merge
            else
                match ofScan i scans.[i] with
                | Error e -> err e
                | Ok more -> add (i + 1) (union merge more)

        add 0 Empty

    /// 先頭のエントリ（取り出さない）。空なら None。
    let top (SstMerge heap) =
        match heap with
        | MergeEmpty -> None
        | MergeNode(_, head, _, _) -> Some head.Entry

    /// 先頭を取り出し、そのソースを 1件進める。
    /// 進めた先が壊れていれば Error（取り出したエントリは捨てる）。
    let next (SstMerge heap) =
        match heap with
        | MergeEmpty -> ret None
        | MergeNode(_, head, left, right) ->
            match SstScan.next head.Scan with
            | Error e -> err e
            | Ok None -> ret (Some(head.Entry, SstMerge(mergeHeaps left right)))
            | Ok(Some(entry, rest)) ->
                let advanced =
                    {
                        Source = head.Source
                        Entry = entry
                        Scan = rest
                    }

                let restHeap =
                    mergeHeaps left (mergeHeaps right (MergeNode(1, advanced, MergeEmpty, MergeEmpty)))

                ret (Some(head.Entry, SstMerge restHeap))

/// 索引の 1要素。
/// 各ブロックの位置と長さを張る。
type internal SstIndexEntry =
    {
        /// このブロックに含まれるキーのうち最大のもの
        LastKey: byte array
        Offset: int
        Length: int
    }

/// SSTable の部分読み取り用リーダー。
/// ファイル全体を読まず、footer / index / Bloom / meta だけを保持する。
type internal SstPartial =
    {
        Path: string
        Index: SstIndexEntry array
        MinKey: byte array
        MaxKey: byte array
        Bloom: BloomFilter option
        EntryCount: int64
        MaxSeqNo: uint64
    }

/// `Sstable.writeTo` の結果。
/// 入力が `seq` なので、書き出しに成功してから先頭・末尾のキーを数え直せない。
/// `FileMetaData.writeSstable` がこれを `FileMetaData` に載せる。
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
    }

module internal Sstable =
    /// 1ブロックあたりのエントリ数。
    [<Literal>]
    let private EntriesPerBlock = 128

    [<Literal>]
    let private Magic = "RykDbSST"

    /// 書き出すファイルのバージョン。
    /// v5 はブロック末尾のチェックサム（restart 配列なし）と、
    /// 部分読みメタデータ（meta / Bloom / index）のチェックサムを持つ。
    /// ブロック・メタデータ・Footer のすべてが個別に検証されるため、全体チェックサムは持たない。
    /// v4 以前は読めない（Footer の長さが違うため openPartial が破損として拒否する）。
    [<Literal>]
    let private Version = 5

    /// 読み取れる下限バージョン。
    [<Literal>]
    let private MinReadableVersion = 5

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
    // 全体 checksum は持たない。データブロックはそれぞれ末尾のチェックサムで守られ、
    // meta / Bloom / index は metadataChecksum、Footer は footerChecksum で守られる。
    // 部分読みでは読まなかったブロックを検証できないが、全体 checksum があっても同じだった。
    //
    // maxSeqNo は GC のために必要。
    // 「最古の snapshot から見えない版」の判定に使う
    // （LevelDB の smallestSnapshot と同じ考え方）。

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
    type private FooterInfo =
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

    let private readFooterFields (footerBytes: byte array) =
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
            else
                let fitsInt (v: int64) =
                    v >= 0L && v <= int64 System.Int32.MaxValue

                if
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

    /// エントリ列を SSTable v5 として stream に書き出し、書き込んだ結果を返す。
    /// <remarks>
    /// 入力は `seq` で **1回だけ列挙する**（memtable の flush が全件を配列にしないため）。
    /// キー順であることの検証は書き込みと同時に行う（失敗時は呼び出し側が部分ファイルを消す）。
    /// ブロックは 1個ずつ組み立てて書くので、ファイル全体分のメモリを持たない。
    /// I/O 例外は呼び出し側の `protect` が処理する。
    /// </remarks>
    let writeTo (stream: System.IO.Stream) (bloomFalsePositiveRate: float) (entries: seq<SstEntry>) =
        use e = entries.GetEnumerator()

        let sharedPrefix (prev: byte array) (key: byte array) =
            let n = min prev.Length key.Length

            let rec sharedLength i =
                if i >= n then n
                elif prev.[i] = key.[i] then sharedLength (i + 1)
                else i

            sharedLength 0

        let encodeEntry prevKey e =
            let shared = sharedPrefix prevKey e.Key
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

        let rec copy (body: byte array array) block i pos =
            if i < body.Length then
                let b = body.[i]
                Array.blit b 0 block pos b.Length
                copy body block (i + 1) (pos + b.Length)

        let buildBlock items =
            let body =
                items
                |> Array.mapi (fun i e -> encodeEntry (if i = 0 then [||] else items.[i - 1].Key) e)

            let bodyLength = body |> Array.sumBy (fun b -> b.Length)
            let block = Array.zeroCreate<byte>(bodyLength + 4)
            copy body block 0 0
            let sum = Hashing.checksumRange block 0 bodyLength
            block.[bodyLength] <- byte (sum &&& 0xFFu)
            block.[bodyLength + 1] <- byte (sum >>> 8 &&& 0xFFu)
            block.[bodyLength + 2] <- byte (sum >>> 16 &&& 0xFFu)
            block.[bodyLength + 3] <- byte (sum >>> 24 &&& 0xFFu)
            block

        let writeBlock items offset indexEntries =
            let data = buildBlock items
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

        let indexBytesOf =
            Array.collect (fun (e: SstIndexEntry) ->
                [|
                    Writer.lengthPrefixed e.LastKey
                    Writer.int64LE (int64 e.Offset)
                    Writer.int64LE (int64 e.Length)
                |])
            >> Array.toList
            >> Writer.build

        let finish
            firstKey
            lastKey
            maxSeqNo
            count
            (keys: ResizeArray<byte array>)
            (block: ResizeArray<SstEntry>)
            offset
            indexEntries
            =
            (if block.Count = 0 then
                 ret (offset, indexEntries)
             else
                 writeBlock (block.ToArray()) offset indexEntries)
            >>= fun (dataSize, indexEntries) ->
                let metaBytes =
                    match firstKey, lastKey with
                    | Some first, Some last -> Writer.build [ Writer.lengthPrefixed first; Writer.lengthPrefixed last ]
                    | _ -> [||]

                let bloomOff = dataSize + metaBytes.Length

                let bloomBytes =
                    if count = 0L then
                        [||]
                    else
                        BloomFilter.ofKeys (int count) bloomFalsePositiveRate (keys.ToArray())
                        |> BloomFilter.serialize

                let indexOff = bloomOff + bloomBytes.Length
                let indexBytes = indexBytesOf (List.rev indexEntries |> List.toArray)

                if int64 indexOff + int64 indexBytes.Length > int64 System.Int32.MaxValue then
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
                            indexOff
                            indexBytes.Length
                            bloomOff
                            bloomBytes.Length
                            dataSize
                            metaBytes.Length
                            (int count)
                            metadataChecksum

                    stream.Write(footer, 0, footer.Length)

                    ret
                        {
                            Size = int64 (indexOff + indexBytes.Length + footer.Length)
                            FirstKey = defaultArg firstKey [||]
                            LastKey = defaultArg lastKey [||]
                            EntryCount = count
                            MaxSeqNo = maxSeqNo
                        }

        let rec loop
            firstKey
            lastKey
            maxSeqNo
            count
            (keys: ResizeArray<byte array>)
            (block: ResizeArray<SstEntry>)
            offset
            indexEntries
            prev
            =
            if block.Count >= EntriesPerBlock then
                match writeBlock (block.ToArray()) offset indexEntries with
                | Error e -> err e
                | Ok(nextOffset, indexEntries') ->
                    loop firstKey lastKey maxSeqNo count keys (ResizeArray<SstEntry>()) nextOffset indexEntries' prev
            elif e.MoveNext() then
                let item = e.Current

                match prev with
                | Some p when SstEntry.compare p item > 0 -> err (DbError.internalError "Sstable: エントリがキー順に並んでいません")
                | _ ->
                    keys.Add item.Key
                    block.Add item

                    loop
                        (match firstKey with
                         | None -> Some item.Key
                         | _ -> firstKey)
                        (Some item.Key)
                        (max maxSeqNo item.SeqNo)
                        (count + 1L)
                        keys
                        block
                        offset
                        indexEntries
                        (Some item)
            else
                finish firstKey lastKey maxSeqNo count keys block offset indexEntries

        loop None None 0UL 0L (ResizeArray<byte array>()) (ResizeArray<SstEntry>()) 0 [] None

    /// エントリ列から SSTable のバイナリを作る（テスト・小規模用途）。
    /// 通常の書き込みは `FileMetaData.writeSstable` がストリームへ直接書く。
    /// 入力はキー昇順・同一キーは SeqNo 降順に並んでいる必要がある（検証する）。
    let build (bloomFalsePositiveRate: float) (entries: SstEntry array) =
        protect (fun () ->
            use stream = new System.IO.MemoryStream()

            writeTo stream bloomFalsePositiveRate entries
            |> Result.map (fun _ -> stream.ToArray()))
        >>= id

    /// Cursor から終端まで 1件ずつ読み出して配列にまとめる。
    /// step は「読むたびに変わる状態」（キーの接頭辞共有など）を
    /// `(cursor, state) -> Result<cursor * 要素 * state, DbError>` として受け取る。
    /// 状態が要らないときは unit を渡す。
    let private readItems
        (step: Cursor -> 'State -> Result<Cursor * 'Item * 'State, DbError>)
        (c: Cursor)
        (state: 'State)
        =
        let rec readAll c state acc =
            if Cursor.atEnd c then
                ret (List.toArray (List.rev acc))
            else
                match step c state with
                | Error e -> err e
                | Ok(c', item, state') -> readAll c' state' (item :: acc)

        readAll c state []

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

                if System.Text.Encoding.ASCII.GetString(bytes, 0, Magic.Length) = Magic then
                    Some(start, bytes)
                else
                    None

        let validateFooter info magic footerStart =
            if magic <> Magic then
                err (DbError.corruption "Sstable: マジックが違います")
            elif info.Version < MinReadableVersion || info.Version > Version then
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
            readFooterFields footerBytes
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

    /// ブロックをエントリ列へ復元する。
    /// 末尾 4バイトのチェックサムを検証する。
    let private parseBlock (data: byte array) =
        let decodeEntry c (prev: byte array) =
            result {
                let! shared, c = Reader.readVarint c

                if shared > uint64 prev.Length then
                    return! err (DbError.corruption "Sstable: 接頭辞長が直前のキーを超えています")

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

        if n < 4 then
            err (DbError.corruption "Sstable: ブロックにチェックサムがありません")
        else
            let payloadLen = n - 4
            let stored = Bytes.readLE32 data payloadLen

            if Hashing.checksumRange data 0 payloadLen <> stored then
                err (DbError.corruption "Sstable: ブロックのチェックサム不一致")
            else
                readItems decodeEntry (Cursor.createSlice data 0 payloadLen) [||]

    /// 部分読みした reader から startKey 以降を遅延生成する。
    /// ブロックは最大 8個まとめて 1回の open で読み、解析は必要になったときだけ行う。
    let rangeScanPartial (startKey: byte array) (limit: int) (partial: SstPartial) =
        let batchBlocks = 8

        let rec search lo hi =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare partial.Index.[mid].LastKey startKey < 0 then
                    search (mid + 1) hi
                else
                    search lo mid

        let readRawBatch i =
            let batchEnd = min (i + batchBlocks) partial.Index.Length
            let startOffset = partial.Index.[i].Offset
            let endEntry = partial.Index.[batchEnd - 1]
            let total = endEntry.Offset + endEntry.Length - startOffset

            protect (fun () ->
                use stream = System.IO.File.OpenRead partial.Path
                stream.Seek(int64 startOffset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte> total
                stream.ReadExactly(buffer, 0, total)
                buffer, startOffset, batchEnd)

        let parseRawBlock (raw: byte array) startOffset i =
            let ie = partial.Index.[i]
            let relativeOffset = ie.Offset - startOffset

            if relativeOffset < 0 || ie.Length < 0 || relativeOffset > raw.Length - ie.Length then
                err (DbError.corruption "Sstable: 索引範囲が読み込みブロックの範囲外です")
            else
                let data = Array.sub raw relativeOffset ie.Length

                parseBlock data
                >>= fun entries ->
                    if
                        entries.Length = 0
                        || Bytes.compare entries.[entries.Length - 1].Key ie.LastKey <> 0
                    then
                        err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                    else
                        ret entries

        let rec scanEntries raw startOffset batchEnd pos count i (entries: SstEntry array) =
            if pos >= entries.Length then
                loadBlock raw startOffset batchEnd count i
            elif limit > 0 && count >= limit then
                SstScan.Empty
            elif Bytes.compare entries.[pos].Key startKey < 0 then
                SstScan(fun () -> SstScan.next (scanEntries raw startOffset batchEnd (pos + 1) count i entries))
            else
                SstScan.emit
                    (fun () -> scanEntries raw startOffset batchEnd (pos + 1) (count + 1) i entries)
                    entries.[pos]

        and loadBlock raw startOffset batchEnd count i =
            if i >= partial.Index.Length || limit > 0 && count >= limit then
                SstScan.Empty
            elif i < batchEnd then
                SstScan(fun () ->
                    match parseRawBlock raw startOffset i with
                    | Error e -> err e
                    | Ok entries -> SstScan.next (scanEntries raw startOffset batchEnd 0 count (i + 1) entries))
            else
                SstScan(fun () ->
                    match readRawBatch i with
                    | Error e -> err e
                    | Ok(raw, startOffset, batchEnd) -> SstScan.next (loadBlock raw startOffset batchEnd count i))

        let startBlock = search 0 partial.Index.Length

        if startBlock >= partial.Index.Length then
            SstScan.Empty
        else
            SstScan(fun () ->
                readRawBatch startBlock
                >>= fun (raw, startOffset, batchEnd) -> SstScan.next (loadBlock raw startOffset batchEnd 0 startBlock))

    /// 部分読みで 1キーを検索する。
    /// 同一キーの複数版は SeqNo 降順で並ぶので、最初の可視版を返す。
    /// 可視版がブロック末尾まで無い場合は、次のブロックに古い版が続く可能性があるため読み進める。
    let lookup (key: byte array) (visible: uint64 option) (partial: SstPartial) =
        let mayContainPartial key partial =
            match partial.Bloom with
            | None -> true
            | Some bloom -> Hashing.hashBytes key |> Hashing.mix |> BloomFilter.mightContain bloom

        let isVisible e =
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true

        let rec search lo hi =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare partial.Index.[mid].LastKey key < 0 then
                    search (mid + 1) hi
                else
                    search lo mid

        let readBlockFromFile partial indexEntry =
            protect (fun () ->
                use stream = System.IO.File.OpenRead partial.Path
                stream.Seek(int64 indexEntry.Offset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte> indexEntry.Length
                stream.ReadExactly(buffer, 0, buffer.Length)
                buffer)
            >>= parseBlock

        let readBlockPartial indexEntry =
            readBlockFromFile partial indexEntry
            >>= fun entries ->
                if
                    entries.Length = 0
                    || Bytes.compare entries.[entries.Length - 1].Key indexEntry.LastKey <> 0
                then
                    err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                else
                    ret entries

        let rec find i j =
            function
            | None ->
                if i >= partial.Index.Length then
                    ret None
                else
                    match readBlockPartial partial.Index.[i] with
                    | Error e -> err e
                    | Ok block -> find i 0 (Some block)
            | Some block ->
                if j >= block.Length then
                    if block.Length > 0 && Bytes.compare block.[block.Length - 1].Key key = 0 then
                        find (i + 1) 0 None
                    else
                        ret None
                else
                    let e = block.[j]

                    match Bytes.compare e.Key key with
                    | c when c < 0 -> find i (j + 1) (Some block)
                    | c when c > 0 -> ret None
                    | _ ->
                        if isVisible e then
                            ret (Some e)
                        else
                            find i (j + 1) (Some block)

        if not (mayContainPartial key partial) then
            ret None
        else
            find (search 0 partial.Index.Length) 0 None
