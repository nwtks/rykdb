namespace RykDb.Core

/// memtable の 1エントリ。
type internal Entry =
    {
        /// keyspace ごとに符号化済みのユーザ指定キー
        Key: byte array
        /// 書き込みの論理時計。大きいほど新しい
        SeqNo: uint64
        /// Some が値、None が削除 tombstone
        Value: SqlValue array option
    }

    member x.IsDelete = x.Value.IsNone

module internal Entry =
    let put key seqNo (value: SqlValue array) =
        {
            Key = key
            SeqNo = seqNo
            Value = Some value
        }

    let delete key seqNo =
        {
            Key = key
            SeqNo = seqNo
            Value = None
        }

    let approxBytes (e: Entry) =
        let valueBytes (v: SqlValue) =
            match v with
            | SqlValue.Text s -> s.Length + 2
            | SqlValue.Blob b -> b.Length + 2
            | SqlValue.Null -> 1
            | _ -> 9

        let payload =
            e.Value
            |> Option.map (fun vs -> 2 + (vs |> Array.sumBy valueBytes))
            |> Option.defaultValue 2

        e.Key.Length + 8 + payload

    /// エントリ列に含まれる SeqNo の最大値（空なら 0）。
    let maxSeqNo (entries: Entry seq) =
        entries |> Seq.fold (fun acc e -> max acc e.SeqNo) 0UL

    /// キーの昇順、同一キーは SeqNo の降順（新しい版が先）。
    let compareKeys (a: Entry) (b: Entry) =
        match Bytes.compare a.Key b.Key with
        | 0 -> compare b.SeqNo a.SeqNo
        | c -> c

    let compareKeyOnly (key: byte array) (e: Entry) = Bytes.compare key e.Key

    let priority (e: Entry) =
        let h = Hashing.hashBytes e.Key
        Hashing.mix (h ^^^ e.SeqNo * 0x9E3779B97F4A7C15UL) |> Hashing.mix

    let private encodeEntry (e: Entry) =
        let body =
            Writer.build
                [
                    Writer.varint e.SeqNo
                    Writer.bool e.IsDelete
                    Writer.lengthPrefixed e.Key
                    match e.Value with
                    | Some values when not e.IsDelete -> ValueCodec.writeValues values
                    | _ -> id
                ]

        Writer.build [ Writer.lengthPrefixed body ]

    let private decodeEntry (data: byte array) =
        result {
            let c = Cursor.create data
            let! seqNo, c = Reader.readVarint c
            let! isDelete, c = Reader.readBool c
            let! key, c = Reader.readLengthPrefixed c

            let! value =
                if isDelete then
                    ret None
                else
                    ValueCodec.readValues c |> Result.map (fun (values, _) -> Some values)

            return
                {
                    Key = key
                    SeqNo = seqNo
                    Value = value
                }
        }

    let encodeBatch (txnId: uint64) (entries: Entry array) =
        entries
        |> Array.fold
            (fun w e -> Writer.writeBytes (encodeEntry e) w)
            (Writer.empty |> Writer.uint64BE txnId |> Writer.varint (uint64 entries.Length))
        |> Writer.toArray

    let decodeBatch (data: byte array) =
        let readOne (c: Cursor) =
            result {
                let! bs, c1 = Reader.readLengthPrefixed c
                let! e = decodeEntry bs
                return e, c1
            }

        result {
            let c = Cursor.create data
            let! txnId, c = Reader.readUint64BE c
            let! count, c = Reader.readVarint c

            if count > uint64 System.Int32.MaxValue then
                return! err (sprintf "Wal: バッチのエントリ数が大きすぎます (%d)" count |> DbError.corruption)
            else
                let! items, _ = Reader.readItems count readOne c
                return txnId, List.toArray items
        }

/// 永続 Treap。
/// 挿入は根から再構築して新しい memtable を返す（破壊的変更なし）。
/// 構造は「キー昇順・同一キーは SeqNo 降順」で探索・全順序走査できる平衡木。
type internal TNode =
    {
        Entry: Entry
        Priority: uint64
        Left: TNode option
        Right: TNode option
    }

/// memtable。
/// 挿入は新しい memtable を返す（破壊的変更をしない）。
/// 挿入前のスナップショットはそのまま残るため、読み取りは常に安全。
type internal Memtable =
    {
        Root: TNode option
        Count: int
        /// メモリ上限判定用の概算サイズ
        ApproxBytes: int
    }

/// 範囲走査の作業リスト。
/// 探索中のノードと、「このエントリを結果に積む」という操作を交互に並べる。
[<RequireQualifiedAccess>]
type private Work =
    /// 探索するノード。
    /// None は「ここより先には対象がない」を意味する。
    | Visit of TNode option
    | Emit of Entry

module internal Memtable =
    let empty =
        {
            Root = None
            Count = 0
            ApproxBytes = 0
        }

    let private split (root: TNode option) (target: Entry) =
        let rec unwind (stack: TNode list) (left: TNode option) (right: TNode option) =
            match stack with
            | [] -> left, right
            | r :: rest ->
                if Entry.compareKeys r.Entry target < 0 then
                    unwind rest (Some { r with Right = left }) right
                else
                    unwind rest left (Some { r with Left = right })

        let rec down (stack: TNode list) (node: TNode option) =
            match node with
            | None -> unwind stack None None
            | Some r ->
                if Entry.compareKeys r.Entry target < 0 then
                    down (r :: stack) r.Right
                else
                    down (r :: stack) r.Left

        down [] root

    let private insertNode (e: Entry) (root: TNode option) =
        let p = Entry.priority e

        let rec rebuild (stack: (TNode * bool) list) (acc: TNode option) =
            match stack with
            | [] -> acc
            | (r, wentLeft) :: rest ->
                let rebuilt =
                    if wentLeft then
                        Some { r with Left = acc }
                    else
                        Some { r with Right = acc }

                rebuild rest rebuilt

        let rec descend (stack: (TNode * bool) list) (node: TNode option) =
            match node with
            | None ->
                Some
                    {
                        Entry = e
                        Priority = p
                        Left = None
                        Right = None
                    }
                |> rebuild stack
            | Some r ->
                if p > r.Priority then
                    let left, right = split (Some r) e

                    Some
                        {
                            Entry = e
                            Priority = p
                            Left = left
                            Right = right
                        }
                    |> rebuild stack
                else if Entry.compareKeys e r.Entry < 0 then
                    descend ((r, true) :: stack) r.Left
                else
                    descend ((r, false) :: stack) r.Right

        descend [] root

    /// 複数のエントリをまとめて挿入する。
    let apply (entries: Entry seq) (mem: Memtable) =
        let insert (e: Entry) (mem: Memtable) =
            {
                Root = insertNode e mem.Root
                Count = mem.Count + 1
                ApproxBytes = mem.ApproxBytes + Entry.approxBytes e
            }

        Seq.fold (fun mem e -> insert e mem) mem entries

    /// キーの最新バージョン（SeqNo が最大のもの）を返す。
    /// 同一キーの版は SeqNo 降順に並ぶので、最初に拾ったものが最新。
    /// ポイントルックアップ（MVCC の可視性つき）。
    let getVisible (mem: Memtable) (key: byte array) (visible: uint64 option) =
        let rec search (work: TNode option list) (best: Entry option) =
            match work with
            | [] -> best
            | None :: rest -> search rest best
            | Some r :: rest ->
                match Entry.compareKeyOnly key r.Entry with
                | c when c < 0 -> search (r.Left :: rest) best
                | c when c > 0 -> search (r.Right :: rest) best
                | _ ->
                    let visibleEntry =
                        match visible with
                        | Some v when r.Entry.SeqNo > v -> None
                        | _ -> Some r.Entry

                    let best' =
                        match best, visibleEntry with
                        | Some b, Some e when b.SeqNo >= e.SeqNo -> Some b
                        | _, Some e -> Some e
                        | _ -> best

                    search (r.Right :: r.Left :: rest) best'

        search [ mem.Root ] None

    /// startKey 以降をキー昇順（同一キーは SeqNo 降順）で遅延生成する。
    /// 状態は「作業リスト + 出した件数」だけで、結果を溜めない
    /// （余分なメモリは作業リスト＝木の高さ分・期待 O(log n)）。
    /// limit が 0 以下のときは上限なし。
    let rangeScan (mem: Memtable) (startKey: byte array) (limit: int) =
        let rec step struct (work, count) =
            match work with
            | [] -> None
            | _ when limit > 0 && count >= limit -> None
            | Work.Emit e :: rest -> Some(e, struct (rest, count + 1))
            | Work.Visit None :: rest -> step struct (rest, count)
            | Work.Visit(Some r) :: rest ->
                if Bytes.compare r.Entry.Key startKey < 0 then
                    step struct (Work.Visit r.Right :: rest, count)
                else
                    step struct (Work.Visit r.Left :: Work.Emit r.Entry :: Work.Visit r.Right :: rest, count)

        Seq.unfold step struct ([ Work.Visit mem.Root ], 0)

    /// キー昇順（同一キーは SeqNo 降順）で全エントリを返す。
    let toArray (mem: Memtable) = rangeScan mem [||] 0 |> Seq.toArray
