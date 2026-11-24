namespace RykDb.Core

/// memtable の 1エントリ。
type internal Entry =
    {
        /// 論理 keyspace の接頭辞を含む物理キー
        Key: byte array
        /// 書き込みの論理時計。大きいほど新しい
        SeqNo: uint64
        /// Some が値、None が削除 tombstone
        Value: SqlValue array option
    }

    member this.IsDelete = this.Value.IsNone

module internal Entry =
    /// 値を持つエントリを作る。
    let put (key: byte array) (seqNo: uint64) (value: SqlValue array) =
        {
            Key = key
            SeqNo = seqNo
            Value = Some value
        }

    /// 削除 tombstone のエントリを作る。
    let delete (key: byte array) (seqNo: uint64) =
        {
            Key = key
            SeqNo = seqNo
            Value = None
        }

    /// memtable が 1エントリを保持するのに要するヒープ上の概算サイズ。
    /// <remarks>
    /// Treap ノード + Entry + キー配列 + 行配列と値の合計（64bit .NET 前提の概算）。
    /// ファイルサイズの見積り（`SstEntry.approximateBytes`）とは別物。
    /// </remarks>
    let managedBytes (e: Entry) =
        let rowBytes =
            match e.Value with
            | Some values -> ValueCodec.managedRowBytes values
            | None -> 0

        48 + 40 + 24 + 24 + e.Key.Length + rowBytes

    /// エントリ列に含まれる SeqNo の最大値（空なら 0）。
    let maxSeqNo (entries: Entry seq) =
        entries |> Seq.fold (fun acc e -> max acc e.SeqNo) 0UL

    /// キーの昇順、同一キーは SeqNo の降順（新しい版が先）。
    /// 定義は `KeyOrder.compare` に 1つだけ置く。
    let compareKeys (a: Entry) (b: Entry) =
        KeyOrder.compare a.Key a.SeqNo b.Key b.SeqNo

    /// Treap の優先度。
    /// キーと SeqNo から決定的に導く。
    let priority (e: Entry) =
        let h = Hashing.hashBytes e.Key
        Hashing.mix (h ^^^ e.SeqNo * 0x9E3779B97F4A7C15UL) |> Hashing.mix

    /// 所有権を移す: キーと行を複製する。
    /// <remarks>
    /// 木の探索は配列の中身に依存するので、挿入後に呼び出し側が書き換えると壊れる。
    /// 保持する側（memtable / transaction バッファ）がこれを呼ぶ。
    /// </remarks>
    let takeOwnership (given: Entry) =
        { given with
            Key = Array.copy given.Key
            Value = given.Value |> Option.map SqlValue.copyRow
        }

    /// 1エントリを長さ付きで符号化する。
    let private encodeEntry (e: Entry) =
        let header =
            Writer.build [ Writer.varint e.SeqNo; Writer.bool e.IsDelete; Writer.lengthPrefixed e.Key ]

        let body =
            match e.Value with
            | Some values when not e.IsDelete ->
                ValueCodec.writeValues values Writer.Empty
                |> Result.map (fun vw ->
                    Writer.build [ Writer.writeBytes header; Writer.writeBytes (Writer.toArray vw) ])
            | _ -> ret header

        body |> Result.map (fun b -> Writer.build [ Writer.lengthPrefixed b ])

    /// 1エントリを復号する
    /// （フレームのチェックサムは呼び出し側で検証済み）。
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

    /// transaction のエントリ列を WAL 用のバイト列にする。
    let encodeBatch (txnId: TxnId) (entries: Entry array) =
        entries
        |> foldOk
            (fun w e -> encodeEntry e |> Result.map (fun bytes -> Writer.writeBytes bytes w))
            (Writer.Empty
             |> Writer.uint64BE (uint64 txnId)
             |> Writer.varint (uint64 entries.Length))
        |> Result.map Writer.toArray

    /// 長さ付きの 1エントリを読んで、エントリと後続カーソルを返す。
    let private readOne (c: Cursor) =
        result {
            let! bs, c1 = Reader.readLengthPrefixed c
            let! e = decodeEntry bs
            return e, c1
        }

    /// WAL のバイト列から transaction 番号とエントリ列を復元する。
    let decodeBatch (data: byte array) =
        result {
            let c = Cursor.create data
            let! rawTxnId, c = Reader.readUint64BE c
            let! count, c = Reader.readVarint c

            if count > uint64 System.Int32.MaxValue then
                return! err (sprintf "Wal: バッチのエントリ数が大きすぎます (%d)" count |> DbError.corruption)
            else
                let! items, _ = Reader.readItems readOne count c
                return LanguagePrimitives.UInt64WithMeasure<txnId> rawTxnId, List.toArray items
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
        /// メモリ上限判定用の概算サイズ（管理メモリ。Treap ノード・Entry・行配列・値を含む）
        ApproxBytes: int
    }

module internal Memtable =
    /// 空の memtable。
    let Empty =
        {
            Root = None
            Count = 0
            ApproxBytes = 0
        }

    /// 探索スタックを巻き戻し、左右の部分木を組み直す。
    [<TailCall>]
    let rec private unwind (left: TNode option) (right: TNode option) (stack: TNode list) (target: Entry) =
        match stack with
        | [] -> left, right
        | r :: rest ->
            if Entry.compareKeys r.Entry target < 0 then
                unwind (Some { r with Right = left }) right rest target
            else
                unwind left (Some { r with Left = right }) rest target

    /// 挿入位置へ向かって下降し、経路をスタックに積む。
    [<TailCall>]
    let rec private down (node: TNode option) (stack: TNode list) (target: Entry) =
        match node with
        | None -> unwind None None stack target
        | Some r ->
            if Entry.compareKeys r.Entry target < 0 then
                down r.Right (r :: stack) target
            else
                down r.Left (r :: stack) target

    /// 根から挿入位置までの経路を分ける。
    let private split (root: TNode option) (target: Entry) = down root [] target

    /// スタックを積み直して部分木を再構築する。
    [<TailCall>]
    let rec private rebuild (stack: (TNode * bool) list) (node: TNode option) =
        match stack with
        | [] -> node
        | (r, wentLeft) :: rest ->
            let rebuilt =
                if wentLeft then
                    Some { r with Left = node }
                else
                    Some { r with Right = node }

            rebuild rest rebuilt

    /// 優先度に従って挿入位置を探し、ノードを組み込む。
    [<TailCall>]
    let rec private descend (e: Entry) (p: uint64) (stack: (TNode * bool) list) =
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

    /// 新しいノードを根から挿入する。
    let private insertNode (e: Entry) (root: TNode option) =
        let p = Entry.priority e
        descend e p [] root

    /// 1エントリを挿入する。
    /// キーと行は複製して保持する（`Entry.takeOwnership`）。
    let insert (given: Entry) (mem: Memtable) =
        let e = Entry.takeOwnership given

        {
            Root = insertNode e mem.Root
            Count = mem.Count + 1
            ApproxBytes = mem.ApproxBytes + Entry.managedBytes e
        }

    /// 複数のエントリをまとめて挿入する。
    let apply (entries: Entry seq) (mem: Memtable) =
        Seq.fold (fun mem e -> insert e mem) mem entries

    /// キーと SeqNo の上限から、可視な最小ノードを探す。
    [<TailCall>]
    let rec private ceiling (key: byte array) (bound: uint64) (best: TNode option) =
        function
        | None -> best
        | Some r ->
            let c = Bytes.compare r.Entry.Key key

            if c > 0 then
                ceiling key bound (Some r) r.Left
            elif c < 0 then
                ceiling key bound best r.Right
            else if r.Entry.SeqNo <= bound then
                ceiling key bound (Some r) r.Left
            else
                ceiling key bound best r.Right

    /// 可視な最新版を返す（ポイントルックアップ）。
    /// <remarks>
    /// 木はキー昇順・同一キーは SeqNo 降順。
    /// `(キー, visible)` 以上で最小のノードを探し、見つかったノードのキーが一致すれば、
    /// それが可視な最新版である（対数時間）。
    /// </remarks>
    let getVisible (key: byte array) (visible: uint64 option) (mem: Memtable) =
        let bound =
            match visible with
            | Some v -> v
            | None -> System.UInt64.MaxValue

        match ceiling key bound None mem.Root with
        | Some r when Bytes.compare r.Entry.Key key = 0 -> Some r.Entry
        | _ -> None

    /// 範囲走査の作業リスト。
    /// 探索中のノードと、「このエントリを結果に積む」という操作を交互に並べる。
    type private Work =
        /// 探索するノード。None は「ここより先には対象がない」を意味する。
        | Visit of TNode option
        /// 結果に積むエントリ。
        | Emit of Entry

    /// 作業リストを 1段進め、次のエントリと残りの作業を返す。
    [<TailCall>]
    let rec private rangeScanStep (startKey: byte array) (limit: int) struct (work: Work list, count: int) =
        match work with
        | [] -> None
        | _ when limit > 0 && count >= limit -> None
        | Emit e :: rest -> Some(e, struct (rest, count + 1))
        | Visit None :: rest -> rangeScanStep startKey limit struct (rest, count)
        | Visit(Some r) :: rest ->
            if Bytes.compare r.Entry.Key startKey < 0 then
                rangeScanStep startKey limit struct (Visit r.Right :: rest, count)
            else
                rangeScanStep startKey limit struct (Visit r.Left :: Emit r.Entry :: Visit r.Right :: rest, count)

    /// startKey 以降をキー昇順（同一キーは SeqNo 降順）で遅延生成する。
    /// 状態は作業リスト（木の高さ分）と件数だけで、結果を溜めない。
    /// limit が 0 以下のときは上限なし。
    let rangeScan (startKey: byte array) (limit: int) (mem: Memtable) =
        Seq.unfold (rangeScanStep startKey limit) struct ([ Visit mem.Root ], 0)

    /// エントリを 1つも持たないか。
    let isEmpty (mem: Memtable) = mem.Count = 0

    /// 保持しているエントリ数
    /// （SSTable 書き出し時の Bloom filter の見積りに使う）。
    let count (mem: Memtable) = mem.Count

    /// 保持しているエントリの管理メモリ概算。
    let approxBytes (mem: Memtable) = mem.ApproxBytes
