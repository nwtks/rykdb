// ===========================================================================
// SSTable — 不変のソート済みファイル。
// メモリ上に展開した byte[] として扱う。
// ブロック境界と索引だけを公開し、ファイル I/O とブロックキャッシュは StorageEngine が担う。
//
//   [Block 0]...[Block N-1][Meta][Bloom][Index][Footer 84 bytes]
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
        let valueTotal =
            match e.Value with
            | None -> 2
            | Some vs -> Array.sumBy ValueCodec.approximateBytes vs + 4

        e.Key.Length + valueTotal

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
    let empty = SstMerge MergeEmpty

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
        let rec descend (picked: (MergeHead * MergeNode) list) (a: MergeNode) (b: MergeNode) =
            match a, b with
            | MergeEmpty, heap
            | heap, MergeEmpty -> picked, heap
            | MergeNode(_, ha, _, _), MergeNode(_, hb, _, _) when compareHead ha hb > 0 -> descend picked b a
            | MergeNode(_, ha, left, right), _ -> descend ((ha, left) :: picked) right b

        let picked, rest = descend [] first second
        List.fold (fun acc (head, left) -> node head left acc) rest picked

    /// 2つのマージをまとめる（どちらも使い切っていない状態）。
    let union (SstMerge a) (SstMerge b) = SstMerge(mergeHeaps a b)

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

    /// スキャンを登録する（先頭を 1件読む。空なら何もしない）。
    let ofScan (source: int) (scan: SstScan) =
        match SstScan.next scan with
        | Error e -> Error e
        | Ok None -> ret empty
        | Ok(Some(entry, rest)) -> Ok(insert source entry rest empty)

    /// 複数のスキャンをまとめて登録する。
    let ofScans (scans: SstScan array) =
        let rec add (i: int) (merge: SstMerge) =
            if i >= scans.Length then
                ret merge
            else
                match ofScan i scans.[i] with
                | Error e -> Error e
                | Ok more -> add (i + 1) (union merge more)

        add 0 empty

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
            | Error e -> Error e
            | Ok None -> Ok(Some(head.Entry, SstMerge(mergeHeaps left right)))
            | Ok(Some(entry, rest)) ->
                let advanced =
                    {
                        Source = head.Source
                        Entry = entry
                        Scan = rest
                    }

                let restHeap =
                    mergeHeaps left (mergeHeaps right (MergeNode(1, advanced, MergeEmpty, MergeEmpty)))

                Ok(Some(head.Entry, SstMerge restHeap))

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
        /// v2 以降は各ブロックに 4バイトのチェックサムが付く。
        HasBlockChecksums: bool
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
        HasBlockChecksums: bool
    }

module internal Sstable =
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

    /// 書き出すファイルのバージョン。
    /// v2 は各ブロックの末尾に 4バイトのチェックサム、
    /// v3 は Footer 自身のチェックサム、v4 は部分読みメタデータのチェックサムを持つ。
    [<Literal>]
    let private Version = 4

    /// v3 と v4 を読み取る。v3 は全体 checksum でメタデータを検証する。
    [<Literal>]
    let private MinReadableVersion = 3

    /// 最新 Footer の固定長（本体 84 + Footer チェックサム 4）。
    [<Literal>]
    let internal FooterSize = 88

    [<Literal>]
    let private FooterSizeV3 = 84

    /// v4 Footer で footerChecksum がカバーするバイト数（metadataChecksum フィールド込み）。
    [<Literal>]
    let private FooterChecksumLengthV4 = 84

    /// v3 Footer で footerChecksum がカバーするバイト数（metadataChecksum フィールドなし）。
    [<Literal>]
    let private FooterChecksumLengthV3 = 80

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

    // Footer
    //   magic(8) version(4) maxSeqNo(8) indexOff(8) indexLen(8)
    //   bloomOff(8) bloomLen(8) metaOff(8) metaLen(8) entryCount(8)
    //   checksum(4) metadataChecksum(4) footerChecksum(4)
    //   = 8 + 4 + 8 * 8 + 4 + 4 + 4 = 88バイト（FooterSize と一致させる）
    // footerChecksum は先頭 84バイト（checksum と metadataChecksum を含む）のチェックサム。
    // 部分読みではこのフッターだけを検証できる。
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
        let metadataChecksum =
            Hashing.checksumRange data metaOff (indexOff + indexLen - metaOff)

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
                    Writer.uint32LE metadataChecksum
                ]

        let footerChecksum = Hashing.checksum body

        let withChecksum =
            Writer.build [ Writer.writeBytes body; Writer.uint32LE footerChecksum ]

        ret (Array.append data withChecksum)

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
            MetadataChecksum: uint32 option
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
            let! stored, c = Reader.readUInt32LE c

            let! metadataChecksum, c =
                if version >= 4u then
                    Reader.readUInt32LE c |> Result.map (fun (sum, next) -> Some sum, next)
                else
                    ret (None, c)

            let! footerChecksum, _ = Reader.readUInt32LE c

            let checksumLength =
                if version >= 4u then
                    FooterChecksumLengthV4
                else
                    FooterChecksumLengthV3

            if Hashing.checksumRange footerBytes 0 checksumLength <> footerChecksum then
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
                        int version,
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
                        },
                        stored
        }

    let private validateFooter (info: FooterInfo) (magic: string) (footerStart: int64) =
        if magic <> Magic then
            err (DbError.corruption "Sstable: マジックが違います")
        elif info.Version < MinReadableVersion || info.Version > Version then
            err (sprintf "Sstable: 未対応のバージョン %d" info.Version |> DbError.corruption)
        elif
            footerStart < 0L
            || int64 info.IndexOff + int64 info.IndexLen > footerStart
            || int64 info.BloomOff + int64 info.BloomLen > footerStart
            || int64 info.MetaOff + int64 info.MetaLen > footerStart
        then
            err (DbError.corruption "Sstable: Footer の範囲が不正です")
        else
            ret info

    let private decodeFooter (data: byte array) =
        let footerBytesAt size =
            let start = data.Length - size

            if start < 0 then
                None
            else
                let bytes = Array.sub data start size

                if System.Text.Encoding.ASCII.GetString(bytes, 0, Magic.Length) = Magic then
                    Some(start, bytes)
                else
                    None

        let parsed = [ FooterSize; FooterSizeV3 ] |> List.tryPick footerBytesAt

        match parsed with
        | None -> err (DbError.corruption "Sstable: Footer がありません")
        | Some(footerStart, footerBytes) ->
            match readFooterFields footerBytes with
            | Error e -> Error e
            | Ok(magic, _, info, stored) ->
                match validateFooter info magic (int64 footerStart) with
                | Error e -> Error e
                | Ok info ->
                    if Hashing.checksum (Array.sub data 0 footerStart) <> stored then
                        err (DbError.corruption "Sstable: チェックサム不一致（ファイルが壊れています）")
                    else
                        ret info

    /// エントリ列から SSTable のバイナリを作る。
    /// 入力はキー昇順・同一キーは SeqNo 降順に並んでいる必要がある（検証する）。
    let build (entries: SstEntry array) (bloomFalsePositiveRate: float) =
        let rec unorderedFrom (i: int) =
            if i >= entries.Length then
                false
            elif SstEntry.compare entries.[i - 1] entries.[i] > 0 then
                true
            else
                unorderedFrom (i + 1)

        let sharedPrefix (prev: byte array) (key: byte array) =
            let n = min prev.Length key.Length

            let rec sharedLength i =
                if i >= n then 0
                elif prev.[i] = key.[i] then sharedLength (i + 1)
                else i

            sharedLength 0

        let encodeEntry (items: SstEntry array) (i: int) (e: SstEntry) =
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

        let rec copy (body: byte array array) result (i: int) (pos: int) =
            if i < body.Length then
                let b = body.[i]
                Array.blit b 0 result pos b.Length
                copy body result (i + 1) (pos + b.Length)

        let buildBlock (items: SstEntry array) =
            let body = items |> Array.mapi (encodeEntry items)
            let offsets = body |> Array.scan (fun acc b -> acc + b.Length) 0

            let restarts =
                [| 0..RestartInterval .. body.Length - 1 |] |> Array.map (fun i -> offsets.[i])

            let restartBytes =
                Writer.build (
                    (restarts |> Array.map (fun off -> Writer.uint32LE (uint32 off)) |> Array.toList)
                    @ [ Writer.uint32LE (uint32 restarts.Length) ]
                )

            let payload = Array.zeroCreate<byte>(offsets.[body.Length] + restartBytes.Length)
            copy body payload 0 0
            Array.blit restartBytes 0 payload offsets.[body.Length] restartBytes.Length
            let sum = Hashing.checksumRange payload 0 payload.Length
            let withChecksum = Array.zeroCreate<byte>(payload.Length + 4)
            Array.blit payload 0 withChecksum 0 payload.Length
            withChecksum.[payload.Length] <- byte (sum &&& 0xFFu)
            withChecksum.[payload.Length + 1] <- byte (sum >>> 8 &&& 0xFFu)
            withChecksum.[payload.Length + 2] <- byte (sum >>> 16 &&& 0xFFu)
            withChecksum.[payload.Length + 3] <- byte (sum >>> 24 &&& 0xFFu)
            withChecksum

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

        let indexBytesOf (blocks: SstEntry array array) (built: byte array array) (offsets: int array) =
            blocks
            |> Array.mapi (fun i items ->
                {
                    LastKey = items.[items.Length - 1].Key
                    Offset = offsets.[i]
                    Length = built.[i].Length
                })
            |> Array.collect (fun e ->
                [|
                    Writer.lengthPrefixed e.LastKey
                    Writer.int64LE (int64 e.Offset)
                    Writer.int64LE (int64 e.Length)
                |])
            |> Array.toList
            |> Writer.build

        result {
            if entries.Length > 1 && unorderedFrom 1 then
                return! err (DbError.internalError "Sstable: エントリがキー順に並んでいません")

            let maxSeqNo = SstEntry.maxSeqNo entries
            let blocks = SstEntry.splitInto EntriesPerBlock entries |> List.toArray
            let built = blocks |> Array.map buildBlock
            let offsets = built |> Array.scan (fun acc b -> acc + b.Length) 0
            let dataSize = offsets.[built.Length]
            let metaOff = dataSize
            let metaBytes = metaBytesOf entries
            let bloomOff = metaOff + metaBytes.Length
            let bloomBytes = bloomBytesOf entries bloomFalsePositiveRate
            let indexOff = bloomOff + bloomBytes.Length
            let indexBytes = indexBytesOf blocks built offsets
            let body = Array.concat [| yield! built; metaBytes; bloomBytes; indexBytes |]

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

    let private readMinMaxKey (metaBytes: byte array) =
        match Cursor.create metaBytes |> Reader.readLengthPrefixed with
        | Ok(mn, c) ->
            match Reader.readLengthPrefixed c with
            | Ok(mx, _) -> mn, mx
            | Error _ -> [||], [||]
        | Error _ -> [||], [||]

    let private readBloomBytes (bytes: byte array) =
        if bytes.Length = 0 then
            ret None
        else
            BloomFilter.deserialize bytes |> Result.map Some

    let private parseIndexBytes (bytes: byte array) =
        let decodeIndexEntry (c: Cursor) (_: unit) =
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

        if bytes.Length = 0 then
            ret [||]
        else
            readItems decodeIndexEntry (Cursor.create bytes) ()

    /// SSTable を開く（ファイル全体を読む）。
    let openTable (data: byte array) =
        result {
            let! footer = decodeFooter data
            let metaBytes = Array.sub data footer.MetaOff footer.MetaLen
            let minKey, maxKey = readMinMaxKey metaBytes
            let! bloom = readBloomBytes (Array.sub data footer.BloomOff footer.BloomLen)
            let! indexEntries = parseIndexBytes (Array.sub data footer.IndexOff footer.IndexLen)

            return
                {
                    Data = data
                    Index = indexEntries
                    MinKey = minKey
                    MaxKey = maxKey
                    Bloom = bloom
                    EntryCount = int64 footer.EntryCount
                    MaxSeqNo = footer.MaxSeqNo
                    HasBlockChecksums = footer.Version >= 2
                }
        }

    /// ファイルの一部だけを読んで SSTable を開く。
    /// footer / index / Bloom / meta だけを読み、データブロックは必要になったとき読む。
    let openPartial (path: string) =
        let readRegion (stream: System.IO.FileStream) (offset: int64) (length: int) =
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

        let parseFooterBytes (footerBytes: byte array) (footerStart: int64) =
            match readFooterFields footerBytes with
            | Error e -> Error e
            | Ok(magic, _, info, _) -> validateFooter info magic footerStart

        let metadataChecksum (metaBytes: byte array) (bloomBytes: byte array) (indexBytes: byte array) =
            Hashing.checksum (Array.concat [| metaBytes; bloomBytes; indexBytes |])

        let readAll (stream: System.IO.FileStream) =
            let parsed =
                readFooter stream FooterSize
                |> Option.orElseWith (fun () -> readFooter stream FooterSizeV3)

            match parsed with
            | None -> Error(DbError.corruption "Sstable: Footer がありません")
            | Some(footerStart, footerBytes) ->
                match parseFooterBytes footerBytes footerStart with
                | Error e -> Error e
                | Ok footer ->
                    let metaBytes = readRegion stream (int64 footer.MetaOff) footer.MetaLen
                    let bloomBytes = readRegion stream (int64 footer.BloomOff) footer.BloomLen
                    let indexBytes = readRegion stream (int64 footer.IndexOff) footer.IndexLen
                    let minKey, maxKey = readMinMaxKey metaBytes

                    result {
                        let! bloom = readBloomBytes bloomBytes
                        let! indexEntries = parseIndexBytes indexBytes

                        match footer.MetadataChecksum with
                        | Some expected when metadataChecksum metaBytes bloomBytes indexBytes <> expected ->
                            return! err (DbError.corruption "Sstable: 部分読みメタデータのチェックサム不一致")
                        | _ -> ()

                        return
                            {
                                Path = path
                                Index = indexEntries
                                MinKey = minKey
                                MaxKey = maxKey
                                Bloom = bloom
                                EntryCount = int64 footer.EntryCount
                                MaxSeqNo = footer.MaxSeqNo
                                HasBlockChecksums = footer.Version >= 2
                            }
                    }

        protect (fun () ->
            use stream = System.IO.File.OpenRead path
            readAll stream)
        |> Result.bind id

    /// Bloom filter で「含まれ得る」か判定する。
    let mayContain (reader: SstReader) (key: byte array) =
        match reader.Bloom with
        | None -> true
        | Some bloom -> Hashing.hashBytes key |> Hashing.mix |> BloomFilter.mightContain bloom

    /// ブロックをエントリ列へ復元する。
    /// v2 以降は末尾 4バイトのチェックサムを検証する。
    let private parseBlock (hasChecksum: bool) (data: byte array) =
        let decodeEntry (c: Cursor) (prev: byte array) =
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

        let parseBlockPayload (data: byte array) (n: int) =
            if n < 2 * RestartEntryBytes then
                err (DbError.corruption "Sstable: ブロックが短すぎます")
            else
                let numRestarts = int (Bytes.readLE32 data (n - RestartEntryBytes))

                if numRestarts <= 0 || numRestarts > n / RestartEntryBytes - 1 then
                    err (DbError.corruption "Sstable: restart 配列が不正です")
                else
                    let restartArraySize = (numRestarts + 1) * RestartEntryBytes
                    readItems decodeEntry (Cursor.createSlice data 0 (n - restartArraySize)) [||]

        let n = data.Length

        if hasChecksum then
            if n < 4 then
                err (DbError.corruption "Sstable: ブロックにチェックサムがありません")
            else
                let payloadLen = n - 4
                let stored = Bytes.readLE32 data payloadLen

                if Hashing.checksumRange data 0 payloadLen <> stored then
                    err (DbError.corruption "Sstable: ブロックのチェックサム不一致")
                else
                    parseBlockPayload data payloadLen
        else
            parseBlockPayload data n

    /// 全体読みした reader から startKey 以降を遅延生成する。
    let rangeScan (reader: SstReader) (startKey: byte array) (limit: int) =
        let rec search (index: SstIndexEntry array) (lo: int) (hi: int) =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare index.[mid].LastKey startKey < 0 then
                    search index (mid + 1) hi
                else
                    search index lo mid

        let readBlock ie =
            Array.sub reader.Data ie.Offset ie.Length |> parseBlock reader.HasBlockChecksums

        let readValidated (indexEntry: SstIndexEntry) =
            readBlock indexEntry
            |> Result.bind (fun entries ->
                if
                    entries.Length = 0
                    || Bytes.compare entries.[entries.Length - 1].Key indexEntry.LastKey <> 0
                then
                    err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                else
                    ret entries)

        let rec fromPos (index: SstIndexEntry array) (i: int) (pos: int) (items: SstEntry array) (count: int) =
            if pos >= items.Length then
                fromBlock index (i + 1) count
            elif limit > 0 && count >= limit then
                SstScan.empty
            elif Bytes.compare items.[pos].Key startKey < 0 then
                fromPos index i (pos + 1) items count
            else
                SstScan.emit items.[pos] (fun () -> fromPos index i (pos + 1) items (count + 1))

        and fromBlock (index: SstIndexEntry array) (i: int) (count: int) =
            if i >= index.Length || limit > 0 && count >= limit then
                SstScan.empty
            else
                SstScan(fun () ->
                    match readValidated index.[i] with
                    | Error e -> Error e
                    | Ok items -> SstScan.next (fromPos index i 0 items count))

        let scanWith (index: SstIndexEntry array) =
            fromBlock index (search index 0 index.Length) 0

        scanWith reader.Index

    /// 部分読みした reader から startKey 以降を遅延生成する。
    /// ブロックは最大 8個まとめて 1回の open で読み、解析は必要になったときだけ行う。
    let rangeScanPartial (partial: SstPartial) (startKey: byte array) (limit: int) =
        let batchBlocks = 8

        let rec search (lo: int) (hi: int) =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare partial.Index.[mid].LastKey startKey < 0 then
                    search (mid + 1) hi
                else
                    search lo mid

        let readRawBatch (i: int) =
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

        let parseRawBlock (i: int) (raw: byte array) (startOffset: int) =
            let ie = partial.Index.[i]
            let relativeOffset = ie.Offset - startOffset

            if relativeOffset < 0 || ie.Length < 0 || relativeOffset > raw.Length - ie.Length then
                err (DbError.corruption "Sstable: 索引範囲が読み込みブロックの範囲外です")
            else
                let data = Array.sub raw relativeOffset ie.Length

                parseBlock partial.HasBlockChecksums data
                |> Result.bind (fun entries ->
                    if
                        entries.Length = 0
                        || Bytes.compare entries.[entries.Length - 1].Key ie.LastKey <> 0
                    then
                        err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                    else
                        ret entries)

        let rec scanEntries
            (raw: byte array)
            (startOffset: int)
            (batchEnd: int)
            (i: int)
            (entries: SstEntry array)
            (pos: int)
            (count: int)
            : SstScan =
            if pos >= entries.Length then
                loadBlock raw startOffset batchEnd i count
            elif limit > 0 && count >= limit then
                SstScan.empty
            elif Bytes.compare entries.[pos].Key startKey < 0 then
                SstScan(fun () -> SstScan.next (scanEntries raw startOffset batchEnd i entries (pos + 1) count))
            else
                SstScan.emit entries.[pos] (fun () ->
                    scanEntries raw startOffset batchEnd i entries (pos + 1) (count + 1))

        and loadBlock (raw: byte array) (startOffset: int) (batchEnd: int) (i: int) (count: int) : SstScan =
            if i >= partial.Index.Length || limit > 0 && count >= limit then
                SstScan.empty
            elif i < batchEnd then
                SstScan(fun () ->
                    match parseRawBlock i raw startOffset with
                    | Error e -> Error e
                    | Ok entries -> SstScan.next (scanEntries raw startOffset batchEnd (i + 1) entries 0 count))
            else
                SstScan(fun () ->
                    match readRawBatch i with
                    | Error e -> Error e
                    | Ok(raw, startOffset, batchEnd) -> SstScan.next (loadBlock raw startOffset batchEnd i count))

        let startBlock = search 0 partial.Index.Length

        if startBlock >= partial.Index.Length then
            SstScan.empty
        else
            SstScan(fun () ->
                match readRawBatch startBlock with
                | Error e -> Error e
                | Ok(raw, startOffset, batchEnd) -> SstScan.next (loadBlock raw startOffset batchEnd startBlock 0))

    /// 部分読みで 1キーを検索する。
    /// 同一キーの複数版は SeqNo 降順で並ぶので、最初の可視版を返す。
    /// 可視版がブロック末尾まで無い場合は、次のブロックに古い版が続く可能性があるため読み進める。
    let lookup (partial: SstPartial) (visible: uint64 option) (key: byte array) =
        let mayContainPartial (partial: SstPartial) (key: byte array) =
            match partial.Bloom with
            | None -> true
            | Some bloom -> Hashing.hashBytes key |> Hashing.mix |> BloomFilter.mightContain bloom

        let isVisible (e: SstEntry) =
            match visible with
            | Some s -> e.SeqNo <= s
            | None -> true

        let rec search (lo: int) (hi: int) =
            if lo >= hi then
                lo
            else
                let mid = lo + (hi - lo) / 2

                if Bytes.compare partial.Index.[mid].LastKey key < 0 then
                    search (mid + 1) hi
                else
                    search lo mid

        let readBlockFromFile (partial: SstPartial) (indexEntry: SstIndexEntry) =
            protect (fun () ->
                use stream = System.IO.File.OpenRead partial.Path
                stream.Seek(int64 indexEntry.Offset, System.IO.SeekOrigin.Begin) |> ignore
                let buffer = Array.zeroCreate<byte> indexEntry.Length
                stream.ReadExactly(buffer, 0, buffer.Length)
                buffer)
            |> Result.bind (parseBlock partial.HasBlockChecksums)

        let readBlockPartial (indexEntry: SstIndexEntry) =
            readBlockFromFile partial indexEntry
            |> Result.bind (fun entries ->
                if
                    entries.Length = 0
                    || Bytes.compare entries.[entries.Length - 1].Key indexEntry.LastKey <> 0
                then
                    err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                else
                    ret entries)

        let rec find (i: int) (entries: SstEntry array option) (j: int) =
            match entries with
            | None ->
                if i >= partial.Index.Length then
                    ret None
                else
                    match readBlockPartial partial.Index.[i] with
                    | Error e -> Error e
                    | Ok block -> find i (Some block) 0
            | Some block ->
                if j >= block.Length then
                    if block.Length > 0 && Bytes.compare block.[block.Length - 1].Key key = 0 then
                        find (i + 1) None 0
                    else
                        ret None
                else
                    let e = block.[j]

                    match Bytes.compare e.Key key with
                    | c when c < 0 -> find i (Some block) (j + 1)
                    | c when c > 0 -> ret None
                    | _ ->
                        if isVisible e then
                            ret (Some e)
                        else
                            find i (Some block) (j + 1)

        if not (mayContainPartial partial key) then
            ret None
        else
            find (search 0 partial.Index.Length) None 0

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
    /// SSTable ファイルの拡張子。
    [<Literal>]
    let internal sstExtension = ".sst"

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
    let private fileName (fileNumber: int64) =
        sprintf "%06d%s" fileNumber sstExtension

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
            if entries.Length = 0 then
                return! err (DbError.internalError "Sstable: 空のエントリ列は書き出せません")

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
