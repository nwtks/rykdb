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

    /// 値部分の正確な符号化長。
    let encodedValueLength (e: SstEntry) =
        let seqLength = Writer.varintLength e.SeqNo

        match e.Value with
        | None -> seqLength + 2
        | Some values -> seqLength + 1 + ValueCodec.encodedValuesLength values

    /// 値部分を出力配列へ直接書く。次の位置を返す。
    let writeValueTo (out: byte array) (pos: int) (e: SstEntry) =
        let pos = Writer.writeVarintTo out pos e.SeqNo

        match e.Value with
        | None ->
            out.[pos] <- 0uy
            let pos = pos + 1
            out.[pos] <- 0uy
            pos + 1
        | Some values ->
            out.[pos] <- 1uy
            ValueCodec.writeValuesTo out (pos + 1) values

    let private readSeqNo =
        let rec loop shift acc (c: Cursor) =
            if shift > 63 || c.Pos >= c.End then
                ValueNone
            else
                let b = c.Data.[c.Pos]
                let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)
                let next = { c with Pos = c.Pos + 1 }

                if b &&& 0x80uy <> 0uy then
                    loop (shift + 7) acc next
                else
                    ValueSome struct (acc, next)

        loop 0 0UL

    let private decodeHeader (c: Cursor) =
        match readSeqNo c with
        | ValueNone -> err (DbError.corruption "Sstable: SeqNo の varint が不正です")
        | ValueSome struct (seqNo, c) ->
            if c.Pos >= c.End then
                err (DbError.corruption "Sstable: 値タグがありません")
            else
                let tag = c.Data.[c.Pos]
                ret (seqNo, tag, { c with Pos = c.Pos + 1 })

    let private decodePayload (seqNo: uint64) (tag: byte) (c: Cursor) =
        result {
            let! values, _ = ValueCodec.readValues c

            if tag = 0uy then
                return seqNo, None
            elif tag = 1uy then
                return seqNo, Some values
            else
                return! err (sprintf "Sstable: 不明な値タグ %d" (int tag) |> DbError.corruption)
        }

    /// 値部分をカーソルから直接復元する。
    /// `length` バイトを消費した次のカーソルも返す。
    /// 値の一時配列を作らずにブロックを読み進められる。
    let decodeValueAt (c: Cursor) (length: int) =
        if
            length < 0
            || c.Pos < 0
            || c.End < c.Pos
            || c.End > c.Data.Length
            || length > c.End - c.Pos
        then
            err (DbError.corruption "Sstable: 値の範囲が不正です")
        else
            let payload = Cursor.createSlice c.Data c.Pos length
            let next = { c with Pos = c.Pos + length }

            match decodeHeader payload with
            | Error e -> err e
            | Ok(seqNo, tag, c) ->
                decodePayload seqNo tag c
                |> Result.map (fun (seqNo, value) -> seqNo, value, next)

    /// 値部分を復元する。
    /// キーは呼び出し側が付ける。
    let decodeValue (data: byte array) =
        match decodeHeader (Cursor.create data) with
        | Error e -> err e
        | Ok(seqNo, tag, c) -> decodePayload seqNo tag c

    /// 1エントリの符号化後のおおよそのバイト数。
    /// compaction の出力をバイト幅で分けるための見積りで、実際の SSTable サイズとは厳密には一致しない。
    let approximateBytes (e: SstEntry) =
        let valueTotal =
            match e.Value with
            | None -> 2
            | Some vs -> Array.sumBy ValueCodec.approximateBytes vs + 4

        e.Key.Length + valueTotal

/// 1回のクエリで開いた走査の台帳。
/// 消費者が早期終了しても `DisposeAll` で最新の continuation を閉じられるようにする。
/// 同じキーには最新の走査だけを保持する（古い continuation は同じファイルハンドルを共有する）。
type internal ScanScope() =
    let active =
        System.Collections.Concurrent.ConcurrentDictionary<obj, System.IDisposable>()

    /// 走査を登録する。同じキーの古い登録は、より新しい continuation で置き換える。
    member _.Register(key: obj, resource: System.IDisposable) = active.[key] <- resource

    /// 登録を外して Dispose する。未登録なら何もしない。
    member _.Release(key: obj) =
        match active.TryRemove key with
        | true, resource -> resource.Dispose()
        | _ -> ()

    /// 登録されている走査をすべて Dispose する。
    member _.DisposeAll() =
        for entry in active do
            match active.TryRemove entry.Key with
            | true, resource -> resource.Dispose()
            | _ -> ()

    /// テスト用: 登録されている走査の数。
    member _.ActiveCount = active.Count

/// SSTable の遅延走査。
/// `next` を呼ぶまでデータブロックを読まず、呼び出しごとに必要なブロックを1つずつ読む。
/// ブロックのチェックサムや内容の検証に失敗した場合は、そのブロックを読む `next` が `Error` を返す。
/// `openTable` は footer やメタデータを検証するが、まだ読んでいないデータブロックは検証しない。
type internal SstScan =
    | SstScan of (unit -> Result<(SstEntry * SstScan) option, DbError>)
    /// 配列内の次の 1件を返す状態。
    /// 1件ごとにクロージャを作らず、位置だけを進める。
    | ArrayScan of
        SstEntry array *
        int *
        int *
        int *
        (int -> Result<(SstEntry * SstScan) option, DbError>) *
        (unit -> unit)
    /// 呼び出し側の状態を共有ステップ関数で進める走査。
    /// 行ごとにクロージャを作らず、状態だけを差し替える。
    | StateScan of obj * (obj -> Result<(SstEntry * SstScan) option, DbError>) * (obj -> unit)
    /// 下流 scan と述語を保持する filter。早期終了時に下流を Dispose できる。
    | FilterScan of SstScan * (SstEntry -> bool)
    /// ScanScope に登録された走査。next のたびに最新の continuation を登録し直す。
    | ScopedScan of SstScan * ScanScope * obj

    interface System.IDisposable with
        member this.Dispose() =
            match this with
            | ArrayScan(_, _, _, _, _, close) -> close ()
            | FilterScan(inner, _) -> (inner :> System.IDisposable).Dispose()
            | StateScan(state, _, close) -> close state
            | ScopedScan(_, scope, key) -> scope.Release key
            | SstScan _ -> ()

module internal SstScan =
    /// 終端（これ以上要素は無い）。
    let Empty = SstScan(fun () -> ret None)

    /// 台帳から登録を外して閉じる。
    let private releaseScope =
        function
        | Some(scope: ScanScope, key: obj) -> scope.Release key
        | None -> ()

    /// 残りの走査を台帳の最新版へ差し替える。
    let private finishScoped (scope: (ScanScope * obj) option) (e: SstEntry) (scan: SstScan) =
        match scope with
        | None -> ret (Some(e, scan))
        | Some(scope, key) ->
            scope.Register(key, scan :> System.IDisposable)
            ret (Some(e, ScopedScan(scan, scope, key)))

    /// 次の 1件。
    /// None は終端。
    /// フィルタの継続は述語リストとして持ち、すべての再帰呼び出しを末尾に置く。
    [<TailCall>]
    let rec private nextWithFilters (filters: (SstEntry -> bool) list) (scope: (ScanScope * obj) option) =
        function
        | FilterScan(inner, predicate) -> nextWithFilters (predicate :: filters) scope inner
        | SstScan step -> continueWith filters scope (step ())
        | ArrayScan(entries, pos, count, limit, loadNext, close) ->
            if limit > 0 && count >= limit then
                close ()
                releaseScope scope
                ret None
            elif pos < entries.Length then
                continueWith
                    filters
                    scope
                    (ret (Some(entries.[pos], ArrayScan(entries, pos + 1, count + 1, limit, loadNext, close))))
            else
                continueWith filters scope (loadNext count)
        | StateScan(state, step, _) -> continueWith filters scope (step state)
        | ScopedScan(inner, scanScope, key) -> nextWithFilters filters (Some(scanScope, key)) inner

    /// 下流 scan の結果に、積んだ述語を順に適用する。
    /// すべて通れば、残りの走査にも同じ述語を付け直して返す。
    and [<TailCall>] private continueWith (filters: (SstEntry -> bool) list) (scope: (ScanScope * obj) option) =
        function
        | Error e ->
            releaseScope scope
            err e
        | Ok None ->
            releaseScope scope
            ret None
        | Ok(Some(e, rest)) ->
            match filters with
            | [] -> finishScoped scope e rest
            | _ -> applyFilters filters filters scope e rest

    /// 継続として積んだ述語を先頭から試す。
    /// 満たさなければ下流 scan の続きへ末尾再帰する。
    and [<TailCall>] private applyFilters
        (allFilters: (SstEntry -> bool) list)
        (remaining: (SstEntry -> bool) list)
        (scope: (ScanScope * obj) option)
        (e: SstEntry)
        (scan: SstScan)
        =
        match remaining with
        | [] ->
            List.fold (fun scan predicate -> FilterScan(scan, predicate)) scan allFilters
            |> finishScoped scope e
        | predicate :: tail ->
            if predicate e then
                applyFilters allFilters tail scope e scan
            else
                nextWithFilters allFilters scope scan

    let next = nextWithFilters [] None

    /// 明示的に資源を解放する（終端・エラー後の呼び出しも安全）。
    let dispose (scan: SstScan) = (scan :> System.IDisposable).Dispose()

    /// 走査を ScanScope に登録する。
    /// 途中で消費者が止まっても、ScanScope.DisposeAll が最新の continuation を閉じる。
    let scoped (scope: ScanScope) (scan: SstScan) =
        let key = obj ()
        scope.Register(key, scan :> System.IDisposable)
        ScopedScan(scan, scope, key)

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

        try
            drain [] scan
        finally
            dispose scan

    /// 条件を満たすエントリだけを返す走査。
    /// 条件を満たさない間は次のエントリを読み進める（ブロック単位のまま）。
    let filter (predicate: SstEntry -> bool) (scan: SstScan) = FilterScan(scan, predicate)

/// k-way merge の 1ソース。
/// 先頭を 1件読んだ状態を表す。
[<Struct>]
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

    /// 単一のヘッドをヒープへ追加する。
    /// singleton を作って汎用 merge に通すより、右スパインの更新1回で済む。
    let private push (head: MergeHead) =
        let rebuild = List.fold (fun acc (h, left) -> node h left acc)

        let rec descend picked =
            function
            | MergeEmpty -> rebuild (MergeNode(1, head, MergeEmpty, MergeEmpty)) picked
            | MergeNode(_, h, left, right) as current ->
                if compareHead head h > 0 then
                    descend ((h, left) :: picked) right
                else
                    rebuild (node head MergeEmpty current) picked

        descend []

    /// 先頭を 1件読んだソースを登録する。
    let insert (source: int) (entry: SstEntry) (scan: SstScan) (SstMerge heap) =
        let head =
            {
                Source = source
                Entry = entry
                Scan = scan
            }

        SstMerge(push head heap)

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

                let restHeap = mergeHeaps left right |> push advanced

                ret (Some(head.Entry, SstMerge restHeap))

    /// マージ木が保持する全走査を解放する。
    let private disposeNode (node: MergeNode) =
        let rec loop =
            function
            | [] -> ()
            | MergeEmpty :: rest -> loop rest
            | MergeNode(_, head, left, right) :: rest ->
                SstScan.dispose head.Scan
                loop (left :: right :: rest)

        loop [ node ]

    let dispose (SstMerge heap) = disposeNode heap

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
        /// path + length + mtime。同じパスのファイルが作り直された場合に
        /// ブロックキャッシュを誤って再利用しないための識別子。
        FileKey: string
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
    type private CachedSstBlock =
        {
            Partial: SstPartial
            Raw: byte array
            Keys: byte array array
            SeqNos: uint64 array
            ValueOffsets: int array
            ValueLengths: int array
            LastKey: byte array
        }

    /// ファイル世代（FileKey）とブロック位置でブロックを特定する。
    [<Struct>]
    type private SstBlockKey =
        {
            FileKey: string
            Offset: int
            Length: int
        }

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

    /// 点検索のブロック解析結果。件数上限を超えたら全消しする（LRU は持たない）。
    let private blockCache =
        System.Collections.Concurrent.ConcurrentDictionary<SstBlockKey, CachedSstBlock>()

    let private blockKey (partial: SstPartial) (indexEntry: SstIndexEntry) =
        {
            FileKey = partial.FileKey
            Offset = indexEntry.Offset
            Length = indexEntry.Length
        }

    let private tryGetCachedBlock (partial: SstPartial) (indexEntry: SstIndexEntry) =
        match blockCache.TryGetValue(blockKey partial indexEntry) with
        | true, block -> Some block
        | _ -> None

    let private putCachedBlock (partial: SstPartial) (indexEntry: SstIndexEntry) (block: CachedSstBlock) =
        if blockCache.Count >= 2048 then
            blockCache.Clear()

        blockCache.TryAdd(blockKey partial indexEntry, block) |> ignore

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

        let entryLength prevKey e =
            let shared = sharedPrefix prevKey e.Key
            let tailLength = e.Key.Length - shared
            let valueLength = SstEntry.encodedValueLength e

            Writer.varintLength (uint64 shared)
            + Writer.varintLength (uint64 tailLength)
            + Writer.varintLength (uint64 valueLength)
            + tailLength
            + valueLength

        let writeEntry out pos prevKey e =
            let shared = sharedPrefix prevKey e.Key
            let tailLength = e.Key.Length - shared
            let valueLength = SstEntry.encodedValueLength e
            let pos = Writer.writeVarintTo out pos (uint64 shared)
            let pos = Writer.writeVarintTo out pos (uint64 tailLength)
            let pos = Writer.writeVarintTo out pos (uint64 valueLength)
            Array.blit e.Key shared out pos tailLength
            SstEntry.writeValueTo out (pos + tailLength) e

        let buildBlock (items: SstEntry array) =
            let rec bodyLength i prev total =
                if i >= items.Length then
                    total
                else
                    bodyLength (i + 1) items.[i].Key (total + entryLength prev items.[i])

            let rec writeEntries block i prev pos =
                if i >= items.Length then
                    pos
                else
                    writeEntries block (i + 1) items.[i].Key (writeEntry block pos prev items.[i])

            let length = bodyLength 0 [||] 0
            let block = Array.zeroCreate<byte>(length + 4)
            writeEntries block 0 [||] 0 |> ignore
            let sum = Hashing.checksumRange block 0 length
            block.[length] <- byte (sum &&& 0xFFu)
            block.[length + 1] <- byte (sum >>> 8 &&& 0xFFu)
            block.[length + 2] <- byte (sum >>> 16 &&& 0xFFu)
            block.[length + 3] <- byte (sum >>> 24 &&& 0xFFu)
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
                                FileKey = ""
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

            readAll stream
            >>= fun partial ->
                ret
                    { partial with
                        FileKey =
                            sprintf "%s|%d|%d" path stream.Length (System.IO.File.GetLastWriteTimeUtc(path).Ticks)
                    })
        >>= id

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

    /// バッファ内の範囲にある 1ブロックをエントリ列へ復元する。
    /// 末尾 4バイトのチェックサムを検証する。
    let private parseBlockAt (data: byte array) (offset: int) (length: int) =
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
                                SstEntry.decodeValueAt c (int valueLen)
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
                readItems decodeEntry (Cursor.createSlice data payloadStart payloadLen) [||]

    /// 部分読みした reader から startKey 以降を遅延生成する。
    /// ブロックは最大 8個まとめて 1回の open で読み、解析は必要になったときだけ行う。
    let rangeScanPartial (startKey: byte array) (limit: int) (partial: SstPartial) =
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
                parseBlockAt raw relativeOffset ie.Length
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
                close ()
                SstScan.Empty
            elif Bytes.compare entries.[pos].Key startKey < 0 then
                SstScan(fun () -> SstScan.next (scanEntries raw startOffset batchEnd (pos + 1) count i entries))
            else
                ArrayScan(
                    entries,
                    pos,
                    count,
                    limit,
                    (fun nextCount -> SstScan.next (loadBlock raw startOffset batchEnd nextCount i)),
                    close
                )

        and loadBlock raw startOffset batchEnd count i =
            if i >= partial.Index.Length || limit > 0 && count >= limit then
                close ()
                SstScan.Empty
            elif i < batchEnd then
                SstScan(fun () ->
                    match parseRawBlock raw startOffset i with
                    | Error e ->
                        close ()
                        err e
                    | Ok entries -> SstScan.next (scanEntries raw startOffset batchEnd 0 count (i + 1) entries))
            else
                SstScan(fun () ->
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
            SstScan(fun () ->
                match readRawBatch startBlock with
                | Error e ->
                    close ()
                    err e
                | Ok(raw, startOffset, batchEnd) -> SstScan.next (loadBlock raw startOffset batchEnd 0 startBlock))

    /// ブロック内のキー・SeqNo・値の位置をキャッシュする。
    /// 点検索のたびにキー復元と値の読み飛ばしを繰り返さないため。
    /// `Partial` を強参照して、ブロックがキャッシュされている間は
    /// `SstPartialCache` の弱参照が外れても index/Bloom を開き直さないようにする。
    let private cacheBlock (partial: SstPartial) (data: byte array) (indexEntry: SstIndexEntry) =
        let rec loop payloadLen (prev: byte array) keys seqNos offsets lengths c =
            if Cursor.atEnd c then
                match keys with
                | [] -> err (DbError.corruption "Sstable: ブロックにエントリがありません")
                | lastKey :: _ ->
                    if Bytes.compare lastKey indexEntry.LastKey <> 0 then
                        err (DbError.corruption "Sstable: 索引の LastKey とブロックが一致しません")
                    else
                        ret
                            {
                                Partial = partial
                                Raw = data
                                Keys = keys |> List.rev |> Array.ofList
                                SeqNos = seqNos |> List.rev |> Array.ofList
                                ValueOffsets = offsets |> List.rev |> Array.ofList
                                ValueLengths = lengths |> List.rev |> Array.ofList
                                LastKey = lastKey
                            }
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
                                        let valueOffset = c4.Pos
                                        let valueLength = int valueLen

                                        if
                                            valueLength < 0
                                            || valueLength > payloadLen
                                            || valueOffset > payloadLen - valueLength
                                        then
                                            err (DbError.corruption "Sstable: 値長が不正です")
                                        else
                                            match
                                                Reader.readVarint (Cursor.createSlice data valueOffset valueLength)
                                            with
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
                Cursor.createSlice data 0 payloadLen |> loop payloadLen [||] [] [] [] []

    let private lookupCached
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
                        let valueBytes = Array.sub block.Raw block.ValueOffsets.[i] block.ValueLengths.[i]

                        match SstEntry.decodeValue valueBytes with
                        | Error e -> err e
                        | Ok(seqNo, value) -> ret (Some(SstEntry.make block.Keys.[i] seqNo value))

            findVisible (lowerBound 0 block.Keys.Length)

    /// 部分読みで 1キーを検索する。
    /// 同一キーの複数版は SeqNo 降順で並ぶので、最初の可視版を返す。
    /// 可視版がブロック末尾まで無い場合は、次のブロックに古い版が続く可能性があるため読み進める。
    let lookup (key: byte array) (visible: uint64 option) (partial: SstPartial) =
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
            match tryGetCachedBlock partial indexEntry with
            | Some block -> lookupCached block key visible indexEntry
            | None ->
                match readBlock indexEntry with
                | Error e -> err e
                | Ok raw ->
                    match cacheBlock partial raw indexEntry with
                    | Error e -> err e
                    | Ok block ->
                        putCachedBlock partial indexEntry block
                        lookupCached block key visible indexEntry

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
