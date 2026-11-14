// ===========================================================================
// SstEntry / SstScan / SstMerge — SSTable の 1エントリと遅延走査・k-way merge。
//
// 走査は one-shot（同じ値の next を 2回呼ばない）。
// 構築は SstScan.ofStep / ofArray / ofState / ofFilter / ofScoped に限り、
// ケースは SstScan.fsi で隠す。
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

    member this.IsDelete = this.Value.IsNone

module internal SstEntry =
    let make key seqNo value =
        {
            Key = key
            SeqNo = seqNo
            Value = value
        }

    /// 並び。
    /// キー昇順・同一キーは SeqNo 降順（新しい版が先）。
    /// 定義は `KeyOrder.compare` に 1つだけ置く。
    let compare (a: SstEntry) (b: SstEntry) =
        KeyOrder.compare a.Key a.SeqNo b.Key b.SeqNo

    /// 値部分の正確な符号化長。
    let encodedValueLength (e: SstEntry) =
        let seqLength = Writer.varintLength e.SeqNo

        match e.Value with
        | None -> seqLength + 2
        | Some values -> seqLength + 1 + ValueCodec.encodedValuesLength values

    /// 値部分を出力配列へ直接書く。
    /// 次の位置を返す。
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
        let rec loop shift acc c =
            if shift > 63 || Cursor.pos c >= Cursor.endPos c then
                ValueNone
            else
                let b = (Cursor.data c).[Cursor.pos c]
                let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)
                let next = Cursor.advance 1 c

                if b &&& 0x80uy <> 0uy then
                    loop (shift + 7) acc next
                else
                    ValueSome struct (acc, next)

        loop 0 0UL

    let private decodeHeader (c: Cursor) =
        match readSeqNo c with
        | ValueNone -> err (DbError.corruption "Sstable: SeqNo の varint が不正です")
        | ValueSome struct (seqNo, c) ->
            if Cursor.pos c >= Cursor.endPos c then
                err (DbError.corruption "Sstable: 値タグがありません")
            else
                let tag = (Cursor.data c).[Cursor.pos c]
                ret (seqNo, tag, Cursor.advance 1 c)

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
        let d = Cursor.data c
        let p = Cursor.pos c
        let e = Cursor.endPos c

        if length < 0 || p < 0 || e < p || e > d.Length || length > e - p then
            err (DbError.corruption "Sstable: 値の範囲が不正です")
        else
            Cursor.createSlice d p length
            >>= decodeHeader
            >>= fun (seqNo, tag, c) -> decodePayload seqNo tag c
            |> Result.map (fun (seqNo, value) ->
                let next = Cursor.advance length c
                seqNo, value, next)

    /// 値部分を復元する。
    /// キーは呼び出し側が付ける。
    let decodeValue (data: byte array) =
        Cursor.create data
        |> decodeHeader
        >>= fun (seqNo, tag, c) -> decodePayload seqNo tag c

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

    /// 走査を登録する。
    /// 同じキーの古い登録は、より新しい continuation で置き換える。
    member _.Register(key: obj, resource: System.IDisposable) = active.[key] <- resource

    /// 登録を外して Dispose する。
    /// 未登録なら何もしない。
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
    member internal _.ActiveCount = active.Count

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
    /// ステップ関数から走査を作る。
    let ofStep (step: unit -> Result<(SstEntry * SstScan) option, DbError>) = SstScan step

    /// 配列走査を作る。
    let ofArray
        (entries: SstEntry array)
        (pos: int)
        (count: int)
        (limit: int)
        (loadNext: int -> Result<(SstEntry * SstScan) option, DbError>)
        (close: unit -> unit)
        =
        ArrayScan(entries, pos, count, limit, loadNext, close)

    /// 状態付き走査を作る。
    /// `StateScan` は異なる状態型を 1つの型で持つため内部では `obj` を使うが、
    /// キャストはこの関数の中だけに閉じ込める（呼び出し側は型付きのまま書ける）。
    let ofState<'s> (state: 's) (step: 's -> Result<(SstEntry * SstScan) option, DbError>) (close: 's -> unit) =
        StateScan(box state, (fun o -> step (unbox<'s> o)), (fun o -> close (unbox<'s> o)))

    /// filter 走査を作る。
    let ofFilter (predicate: SstEntry -> bool) (scan: SstScan) = FilterScan(scan, predicate)

    /// スコープ登録された走査を作る。
    let ofScoped (scan: SstScan) (scope: ScanScope) (key: obj) = ScopedScan(scan, scope, key)

    /// 終端（これ以上要素は無い）。
    let Empty = ofStep (fun () -> ret None)

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
            ret (Some(e, ofScoped scan scope key))

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
                    (ret (Some(entries.[pos], ofArray entries (pos + 1) (count + 1) limit loadNext close)))
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
            List.fold (fun scan predicate -> ofFilter predicate scan) scan allFilters
            |> finishScoped scope e
        | predicate :: tail ->
            if predicate e then
                applyFilters allFilters tail scope e scan
            else
                nextWithFilters allFilters scope scan

    let next = nextWithFilters [] None

    /// 明示的に資源を解放する（終端・エラー後の呼び出しも安全）。
    let dispose (scan: SstScan) = (scan :> System.IDisposable).Dispose()

    /// 走査を使い、成功・失敗・例外のいずれでも Dispose する。
    /// 開いた走査を全件読み切る用途で使う。
    let withScan (scan: SstScan) (f: SstScan -> Result<'a, DbError>) =
        try
            f scan
        finally
            dispose scan

    /// 走査を ScanScope に登録する。
    /// 途中で消費者が止まっても、ScanScope.DisposeAll が最新の continuation を閉じる。
    let scoped (scope: ScanScope) (scan: SstScan) =
        let key = obj ()
        scope.Register(key, scan :> System.IDisposable)
        ofScoped scan scope key

    /// 1件を返す走査。
    /// 続き（rest）は次を要求されるまで組み立てない
    /// （ここで組み立てると 1回の next がブロック全体を辿ってしまい、遅延にならない）。
    let emit (rest: unit -> SstScan) (e: SstEntry) =
        ofStep (fun () -> Some(e, rest ()) |> ret)

    /// 残りを配列に畳む（壊れたブロックに当たったらその場で Error）。
    let toArray (scan: SstScan) =
        let rec drain acc cur =
            match next cur with
            | Error e -> err e
            | Ok None -> List.rev acc |> Array.ofList |> ret
            | Ok(Some(e, nxt)) -> drain (e :: acc) nxt

        withScan scan (drain [])

    /// 条件を満たすエントリだけを返す走査。
    /// 条件を満たさない間は次のエントリを読み進める（ブロック単位のまま）。
    let filter (predicate: SstEntry -> bool) (scan: SstScan) = ofFilter predicate scan

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

    /// 先頭のエントリ（取り出さない）。
    /// 空なら None。
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

    let dispose (SstMerge heap) =
        let rec disposeNode =
            function
            | [] -> ()
            | MergeEmpty :: rest -> disposeNode rest
            | MergeNode(_, head, left, right) :: rest ->
                SstScan.dispose head.Scan
                disposeNode (left :: right :: rest)

        disposeNode [ heap ]
