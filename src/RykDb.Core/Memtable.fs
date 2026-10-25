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
        let valueBytes v =
            match ValueCodec.fixedEncodedBytes v with
            | Some n -> n
            | None ->
                match v with
                | SqlValue.Text s -> s.Length + 2
                | SqlValue.Blob b -> b.Length + 2
                | _ -> 1

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
        let readOne c =
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
                let! items, _ = Reader.readItems readOne count c
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

module internal Memtable =
    let empty =
        {
            Root = None
            Count = 0
            ApproxBytes = 0
        }

    /// 複数のエントリをまとめて挿入する。
    let apply (entries: Entry seq) (mem: Memtable) =
        let rec unwind left right stack target =
            match stack with
            | [] -> left, right
            | r :: rest ->
                if Entry.compareKeys r.Entry target < 0 then
                    unwind (Some { r with Right = left }) right rest target
                else
                    unwind left (Some { r with Left = right }) rest target

        let rec down node stack target =
            match node with
            | None -> unwind None None stack target
            | Some r ->
                if Entry.compareKeys r.Entry target < 0 then
                    down r.Right (r :: stack) target
                else
                    down r.Left (r :: stack) target

        let split root target = down root [] target

        let rec rebuild stack node =
            match stack with
            | [] -> node
            | (r, wentLeft) :: rest ->
                let rebuilt =
                    if wentLeft then
                        Some { r with Left = node }
                    else
                        Some { r with Right = node }

                rebuild rest rebuilt

        let rec descend e p stack =
            function
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
                    descend e p ((r, true) :: stack) r.Left
                else
                    descend e p ((r, false) :: stack) r.Right

        let insertNode e root =
            let p = Entry.priority e
            descend e p [] root

        let insert e mem =
            {
                Root = insertNode e mem.Root
                Count = mem.Count + 1
                ApproxBytes = mem.ApproxBytes + Entry.approxBytes e
            }

        Seq.fold (fun mem e -> insert e mem) mem entries

    /// キーの最新バージョン（SeqNo が最大のもの）を返す。
    /// 同一キーの版は SeqNo 降順に並ぶので、最初に拾ったものが最新。
    /// ポイントルックアップ（MVCC の可視性つき）。
    /// <remarks>
    /// 同じキーの版は木の中で連続して並ぶ（キーの昇順・同一キーは SeqNo 降順）。
    /// 可視な版（SeqNo が visible 以下）は複合キー順では後ろにあるので、
    /// 「(キー, visible) 以上の最小のノード」を探す（ceiling 探索）。
    /// それが同じキーなら可視な最新版になる（探索は対数時間）。
    /// </remarks>
    let getVisible (key: byte array) (visible: uint64 option) (mem: Memtable) =
        let bound =
            match visible with
            | Some v -> v
            | None -> System.UInt64.MaxValue

        let rec ceiling (node: TNode option) (best: TNode option) =
            match node with
            | None -> best
            | Some r ->
                let c = Bytes.compare r.Entry.Key key

                if c > 0 then ceiling r.Left (Some r)
                elif c < 0 then ceiling r.Right best
                else if r.Entry.SeqNo <= bound then ceiling r.Left (Some r)
                else ceiling r.Right best

        match ceiling mem.Root None with
        | Some r when Bytes.compare r.Entry.Key key = 0 -> Some r.Entry
        | _ -> None

    /// 範囲走査の作業リスト。
    /// 探索中のノードと、「このエントリを結果に積む」という操作を交互に並べる。
    type private Work =
        /// 探索するノード。
        /// None は「ここより先には対象がない」を意味する。
        | Visit of TNode option
        | Emit of Entry

    /// startKey 以降をキー昇順（同一キーは SeqNo 降順）で遅延生成する。
    /// 状態は「作業リスト + 出した件数」だけで、結果を溜めない
    /// （余分なメモリは作業リスト＝木の高さ分・期待 O(log n)）。
    /// limit が 0 以下のときは上限なし。
    let rangeScan (startKey: byte array) (limit: int) (mem: Memtable) =
        let rec step struct (work, count) =
            match work with
            | [] -> None
            | _ when limit > 0 && count >= limit -> None
            | Emit e :: rest -> Some(e, struct (rest, count + 1))
            | Visit None :: rest -> step struct (rest, count)
            | Visit(Some r) :: rest ->
                if Bytes.compare r.Entry.Key startKey < 0 then
                    step struct (Visit r.Right :: rest, count)
                else
                    step struct (Visit r.Left :: Emit r.Entry :: Visit r.Right :: rest, count)

        Seq.unfold step struct ([ Visit mem.Root ], 0)

    /// キー昇順（同一キーは SeqNo 降順）で全エントリを返す。
    let toArray (mem: Memtable) = rangeScan [||] 0 mem |> Seq.toArray
