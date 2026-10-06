namespace RykDb.Core

/// memtable の 1 エントリ。
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

    let private valueBytes (v: SqlValue) =
        match v with
        | SqlValue.Text s -> s.Length + 2
        | SqlValue.Blob b -> b.Length + 2
        | SqlValue.Null -> 1
        | _ -> 9

    let approxBytes (e: Entry) =
        let payload =
            e.Value
            |> Option.map (fun vs -> 2 + (vs |> Array.sumBy valueBytes))
            |> Option.defaultValue 2

        e.Key.Length + 8 + payload

/// 永続 Treap — 挿入は根から再構築して新しい memtable を返す（破壊的変更なし）。
/// 構造は「キー昇順・同一キーは SeqNo 降順」で探索・全順序走査できる平衡木。
/// スキップリストを採用しなかった理由は docs/architecture.md §9.1。
type internal TNode =
    {
        Entry: Entry
        Priority: uint64
        Left: TNode option
        Right: TNode option
    }

/// memtable。挿入は新しい memtable を返す（破壊的変更をしない）。
/// 挿入前のスナップショットはそのまま残るため、読み取りは常に安全。
type internal Memtable =
    {
        Root: TNode option
        Count: int
        /// メモリ上限判定用の概算サイズ
        ApproxBytes: int
    }

/// 範囲走査の作業リスト。探索中のノードと、
/// 「このエントリを結果に積む」という操作を交互に並べる。
[<RequireQualifiedAccess>]
type private Work =
    /// 探索するノード。None は「ここより先には対象がない」を意味する
    | Visit of TNode option
    | Emit of Entry

module internal Memtable =
    let empty =
        {
            Root = None
            Count = 0
            ApproxBytes = 0
        }

    let private compareKeys (a: Entry) (b: Entry) =
        let n = min a.Key.Length b.Key.Length

        let rec go i =
            if i >= n then compare a.Key.Length b.Key.Length
            elif a.Key.[i] = b.Key.[i] then go (i + 1)
            else compare (int a.Key.[i]) (int b.Key.[i])

        match go 0 with
        | 0 -> compare b.SeqNo a.SeqNo
        | c -> c

    let private compareByteArrays (a: byte array) (b: byte array) =
        let n = min a.Length b.Length

        let rec go i =
            if i >= n then compare a.Length b.Length
            elif a.[i] = b.[i] then go (i + 1)
            else compare (int a.[i]) (int b.[i])

        go 0

    let private compareKeyOnly (key: byte array) (e: Entry) = compareByteArrays key e.Key

    let private priority (e: Entry) =
        let h = Hashing.hashBytes e.Key
        let mixed = Hashing.mix (h ^^^ e.SeqNo * 0x9E3779B97F4A7C15UL)
        Hashing.mix mixed

    let private split (root: TNode option) (target: Entry) =
        let rec go (stack: TNode list) (left: TNode option) (right: TNode option) =
            match stack with
            | [] -> left, right
            | r :: rest ->
                if compareKeys r.Entry target < 0 then
                    go rest (Some { r with Right = left }) right
                else
                    go rest left (Some { r with Left = right })

        let rec down (stack: TNode list) (root: TNode option) =
            match root with
            | None -> go stack None None
            | Some r ->
                if compareKeys r.Entry target < 0 then
                    down (r :: stack) r.Right
                else
                    down (r :: stack) r.Left

        down [] root

    let private insertNode (e: Entry) (root: TNode option) =
        let p = priority e

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

        let rec go (stack: (TNode * bool) list) (root: TNode option) =
            match root with
            | None ->
                let leaf =
                    Some
                        {
                            Entry = e
                            Priority = p
                            Left = None
                            Right = None
                        }

                rebuild stack leaf
            | Some r ->
                if p > r.Priority then
                    let left, right = split (Some r) e

                    rebuild
                        stack
                        (Some
                            {
                                Entry = e
                                Priority = p
                                Left = left
                                Right = right
                            })
                else
                    let c = compareKeys e r.Entry

                    if c < 0 then
                        go ((r, true) :: stack) r.Left
                    else
                        go ((r, false) :: stack) r.Right

        go [] root

    let private insert (e: Entry) (mem: Memtable) =
        {
            Root = insertNode e mem.Root
            Count = mem.Count + 1
            ApproxBytes = mem.ApproxBytes + Entry.approxBytes e
        }

    /// 複数のエントリをまとめて挿入する。
    let apply (entries: Entry seq) (mem: Memtable) =
        Seq.fold (fun mem e -> insert e mem) mem entries

    /// キーの最新バージョン（SeqNo が最大のもの）を返す。
    /// 同一キーの版は SeqNo 降順に並ぶので、最初に拾ったものが最新。
    /// 点ルックアップ（MVCC の可視性つき）。
    /// <remarks>
    /// 点ルックアップ-matched キーだけを辿るので O(log n)。
    /// `rangeScan` で済ませると「そのキー以降の全エントリ」を挽くため
    /// O(n) になり、10 万行入れた memtable の点検索が 1 回 40 ms になる。
    /// </remarks>
    let getVisible (mem: Memtable) (key: byte array) (visible: uint64 option) =
        let rec go (work: TNode option list) (best: Entry option) =
            match work with
            | [] -> best
            | None :: rest -> go rest best
            | Some r :: rest ->
                match compareKeyOnly key r.Entry with
                | c when c < 0 -> go (r.Left :: rest) best
                | c when c > 0 -> go (r.Right :: rest) best
                | _ ->
                    let visibleEntry =
                        match visible with
                        | Some v when r.Entry.SeqNo > v -> None
                        | _ -> Some r.Entry

                    let best =
                        match best, visibleEntry with
                        | Some b, Some e when b.SeqNo >= e.SeqNo -> Some b
                        | _, Some e -> Some e
                        | _ -> best

                    go (r.Right :: r.Left :: rest) best

        go [ mem.Root ] None

    let private scanFrom (mem: Memtable) (startKey: byte array) (limit: int) =
        let rec go (work: Work list) (acc: Entry list) (count: int) =
            if limit > 0 && count >= limit then
                List.rev acc
            else
                match work with
                | [] -> List.rev acc
                | Work.Emit e :: rest -> go rest (e :: acc) (count + 1)
                | Work.Visit None :: rest -> go rest acc count
                | Work.Visit(Some r) :: rest ->
                    if compareByteArrays r.Entry.Key startKey < 0 then
                        go (Work.Visit r.Right :: rest) acc count
                    else
                        go (Work.Visit r.Left :: Work.Emit r.Entry :: Work.Visit r.Right :: rest) acc count

        go [ Work.Visit mem.Root ] [] 0

    /// キー昇順（同一キーは SeqNo 降順）で全エントリを返す。
    let toArray (mem: Memtable) =
        // startKey を空にすると全件が対象になる（空キーより大きいので除外されない）
        scanFrom mem [||] 0 |> List.toArray

    /// startKey 以降のキーをキー昇順で返す。
    /// 木を使うので開始位置への到達は O(log n)。
    let rangeScan (mem: Memtable) (startKey: byte array) (limit: int) =
        seq { yield! scanFrom mem startKey limit }
