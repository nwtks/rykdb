// ===========================================================================
// ValueSet — `x [NOT] IN (SELECT ...)` の値集合（作業メモリ上限つき）。
//
// 上限内はソート済み配列を二分探索する。超えたら符号化値を一時ファイルへ書き出し、
// ブロックごとの先頭値（疎な索引）でブロックを特定してそのブロックだけを読む。
// 等値判定は SQL の比較（`SqlValue.compareValuesV`）に合わせ、
// 符号化バイトの一致では `Int 1` と `Decimal 1.0` が別物になる問題を避ける。
// NULL は保存せず `HasNull` だけを持つ。
// 一時ファイルは `ScanScope` に登録し、早期終了・エラーでも閉じて消す。
// ===========================================================================

namespace RykDb.Core

/// 退避した値集合の 1ブロック（メモリ上の索引）。
type internal ValueBlock =
    {
        /// ブロック先頭の値（二分探索の境界）。
        First: SqlValue
        /// ファイル内の開始位置（バイト）。
        Offset: int64
        /// ブロックの行数。
        Count: int
    }

/// ソート済みファイルに置いた値集合。
/// <remarks>
/// 読み取り用のストリームと直近 1ブロックをキャッシュする。
/// 1つのクエリの評価スレッドから順に呼ばれる前提（`RowReader` と同じ）。
/// </remarks>
[<Sealed>]
type internal IndexedValues(path: string, blocks: ValueBlock array, bloom: BloomFilter) =
    let gate = obj ()
    let mutable reader: System.IO.Stream option = None
    let mutable cachedIndex = -1
    let mutable cachedRows: SqlValue array = [||]
    let mutable disposed = false

    let closeReader () =
        match reader with
        | Some s ->
            protect (fun () -> s.Dispose()) |> ignore
            reader <- None
        | None -> ()

    let openReader () =
        match reader with
        | Some s -> s
        | None ->
            let fs =
                new System.IO.FileStream(
                    path,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.Read,
                    System.IO.FileShare.Read
                )

            let s = new System.IO.BufferedStream(fs, 65536)
            reader <- Some s
            s

    /// ブロックを読む（直近のブロックはキャッシュ）。
    let readBlock (index: int) =
        if cachedIndex = index && not (Array.isEmpty cachedRows) then
            Ok cachedRows
        else
            let block = blocks.[index]
            let s = openReader ()
            s.Seek(block.Offset, System.IO.SeekOrigin.Begin) |> ignore
            let src = new Source(s)
            let rows = Array.zeroCreate<SqlValue> block.Count

            let rec fill i =
                if i >= block.Count then
                    Ok rows
                else
                    match Spill.readRow src with
                    | Error e -> Error e
                    | Ok None -> Error(DbError.corruption "ValueSet: 退避したブロックが途中で終わりました")
                    | Ok(Some row) ->
                        rows.[i] <- row.[0]
                        fill (i + 1)

            match fill 0 with
            | Error e -> Error e
            | Ok _ ->
                cachedIndex <- index
                cachedRows <- rows
                Ok rows

    let searchRows (rows: SqlValue array) (v: SqlValue) =
        let rec go lo hi =
            if lo > hi then
                Ok false
            else
                let mid = (lo + hi) / 2

                match SqlValue.compareValuesV rows.[mid] v with
                | DbErr e -> Error e
                | DbOk 0 -> Ok true
                | DbOk c -> if c < 0 then go (mid + 1) hi else go lo (mid - 1)

        go 0 (rows.Length - 1)

    /// 空か（NULL だけの集合は空だが、`ValueSet` 側が `HasNull` で区別する）。
    member _.IsEmpty = blocks.Length = 0

    /// 値を含むか（SQL の等値）。
    member _.Contains(v: SqlValue) =
        lock gate (fun () ->
            match ValueCodec.writeValues [| v |] Writer.Empty |> Result.map Writer.toArray with
            | Error e -> Error e
            | Ok bytes ->
                let h = bytes |> Hashing.hashBytes |> Hashing.mix

                if not (BloomFilter.mightContain bloom h) then
                    Ok false
                else
                    // `v` を含み得るブロックは「先頭値が v 以下である最後のブロック」だけ。
                    let rec findBlock lo hi best =
                        if lo > hi then
                            Ok best
                        else
                            let mid = (lo + hi) / 2

                            match SqlValue.compareValuesV blocks.[mid].First v with
                            | DbErr e -> Error e
                            | DbOk c ->
                                if c <= 0 then
                                    findBlock (mid + 1) hi (Some mid)
                                else
                                    findBlock lo (mid - 1) best

                    match findBlock 0 (blocks.Length - 1) None with
                    | Error e -> Error e
                    | Ok None -> Ok false
                    | Ok(Some index) ->
                        match readBlock index with
                        | Error e -> Error e
                        | Ok rows -> searchRows rows v)

    interface System.IDisposable with
        member _.Dispose() =
            lock gate (fun () ->
                if not disposed then
                    disposed <- true
                    closeReader ()
                    Spill.unlink path)

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal ValueSetStorage =
    /// メモリ上（NULL を含まないソート済み配列）。
    | InMemory of rows: SqlValue array
    /// 一時ファイル（NULL を含まないソート済み）。
    | OnDisk of store: IndexedValues

/// IN サブクエリの値集合。
/// <remarks>
/// 作業メモリに収まる間はソート済み配列、超えたら一時ファイル + 疎な索引で保持する。
/// NULL は保存せず `HasNull` だけを持つ（SQL の `IN` では NULL は一致しない）。
/// </remarks>
[<Sealed>]
type internal ValueSet internal (storage: ValueSetStorage, hasNull: bool) =
    /// ソート済み配列の二分探索（SQL の等値）。
    let searchArray (rows: SqlValue array) (v: SqlValue) =
        let rec go lo hi =
            if lo > hi then
                Ok false
            else
                let mid = (lo + hi) / 2

                match SqlValue.compareValuesV rows.[mid] v with
                | DbErr e -> Error e
                | DbOk 0 -> Ok true
                | DbOk c -> if c < 0 then go (mid + 1) hi else go lo (mid - 1)

        go 0 (rows.Length - 1)

    /// 値を含むか（SQL の 3値論理）。
    /// 空集合は FALSE、要素に NULL があって見つからない場合は UNKNOWN。
    member _.Contains(v: SqlValue) =
        let empty =
            match storage with
            | ValueSetStorage.InMemory rows -> rows.Length = 0
            | ValueSetStorage.OnDisk store -> store.IsEmpty

        if empty && not hasNull then
            DbOk TruthValue.False
        elif SqlValue.isNull v then
            DbOk TruthValue.Unknown
        else
            let found =
                match storage with
                | ValueSetStorage.InMemory rows -> searchArray rows v
                | ValueSetStorage.OnDisk store -> store.Contains v

            match found with
            | Error e -> DbErr e
            | Ok true -> DbOk TruthValue.True
            | Ok false -> DbOk(if hasNull then TruthValue.Unknown else TruthValue.False)

    interface System.IDisposable with
        member _.Dispose() =
            match storage with
            | ValueSetStorage.InMemory _ -> ()
            | ValueSetStorage.OnDisk store -> (store :> System.IDisposable).Dispose()

/// 値集合の構築。
module internal ValueSet =
    /// 1ブロックあたりの行数。索引のメモリは「値の個数 / BlockRows」に比例する。
    [<Literal>]
    let private BlockRows = 512

    /// Bloom filter の誤検出率（偽陽性は遅くなるだけで、正しさには影響しない）。
    [<Literal>]
    let private BloomFalsePositiveRate = 0.01

    /// 1値を符号化する（Bloom のキー）。
    let private encode (v: SqlValue) =
        ValueCodec.writeValues [| v |] Writer.Empty |> Result.map Writer.toArray

    /// 値の全順序比較（ソート用）。比較不能は検出済みなので 0 に倒す。
    let private compareValues (a: SqlValue array) (b: SqlValue array) =
        match SqlValue.compareValuesForSortV a.[0] b.[0] with
        | DbOk c -> c
        | DbErr _ -> 0

    /// 比較不能な型が混ざっていないかを早い段階で検査する。
    /// 代表値（最初の非 NULL 値）と比べれば、型の組み合わせはすべて検出できる。
    let private checkComparable (representative: SqlValue option) (row: SqlValue array) =
        let v = row.[0]

        if SqlValue.isNull v then
            DbOk representative
        else
            match representative with
            | None -> DbOk(Some v)
            | Some rep ->
                match SqlValue.compareValuesV rep v with
                | DbErr e -> DbErr e
                | DbOk _ -> DbOk representative

    /// メモリ上の集合をソート済み配列にする。
    let private sortedArray (set: System.Collections.Generic.HashSet<SqlValue>) =
        let values = set |> Seq.toArray

        match values with
        | [||] -> Ok values
        | _ ->
            let rep = values.[0]

            let rec validate i =
                if i >= values.Length then
                    Ok()
                else
                    match SqlValue.compareValuesV rep values.[i] with
                    | DbErr e -> Error e
                    | DbOk _ -> validate (i + 1)

            match validate 1 with
            | Error e -> Error e
            | Ok() -> Ok(values |> Array.sortWith (fun a b -> compareValues [| a |] [| b |]))

    /// 1値のフレーム長（varint 長 + 符号化）。
    let private frameLength (v: SqlValue) =
        let payload = ValueCodec.encodedValuesLength [| v |]
        int64 (Writer.varintLength (uint64 payload)) + int64 payload

    /// ソート済みの行を索引付きファイルへ書き出す。
    /// NULL は書き出さず、`HasNull` だけを立てる（比較不能な値を二分探索に混ぜない）。
    let private writeIndexed (budget: MemoryBudget) (total: int) (initialHasNull: bool) (sorted: Op) =
        let path = Spill.runPath budget.Directory (System.Guid.NewGuid().ToString "N") 0
        let bloom = BloomFilter.BloomBuilder(total, BloomFalsePositiveRate)

        match Spill.openSink path with
        | Error e -> err e
        | Ok sink ->
            let abandon (e: DbError) =
                Spill.closeSink sink
                Spill.unlink path
                err e

            let writeBlock blocks (rows: SqlValue array) offset =
                let rec writeRows i pos =
                    if i >= rows.Length then
                        Ok pos
                    else
                        match Spill.writeRow [| rows.[i] |] sink with
                        | Error e -> Error e
                        | Ok() -> writeRows (i + 1) (pos + frameLength rows.[i])

                match writeRows 0 offset with
                | Error e -> Error e
                | Ok pos ->
                    Ok(
                        {
                            First = rows.[0]
                            Offset = offset
                            Count = rows.Length
                        }
                        :: blocks,
                        pos
                    )

            let addBloom v =
                match encode v with
                | Ok bytes -> bloom.Add bytes
                | Error _ -> ()

            let rec go hasNull blocks buf n offset cur =
                match Op.next cur with
                | OpStep.OpError e -> Error e
                | OpStep.OpDone ->
                    if n = 0 then
                        Ok(List.rev blocks, hasNull)
                    else
                        writeBlock blocks (Array.sub buf 0 n) offset
                        |> Result.map (fun (blocks', _) -> blocks', hasNull)
                | OpStep.OpRow(row, nxt) ->
                    if SqlValue.isNull row.[0] then
                        go true blocks buf n offset nxt
                    else
                        addBloom row.[0]
                        buf.[n] <- row.[0]

                        if n + 1 >= BlockRows then
                            match writeBlock blocks buf offset with
                            | Error e -> Error e
                            | Ok(blocks', offset') -> go hasNull blocks' (Array.zeroCreate BlockRows) 0 offset' nxt
                        else
                            go hasNull blocks buf (n + 1) offset nxt

            match go initialHasNull [] (Array.zeroCreate BlockRows) 0 0L sorted with
            | Error e -> abandon e
            | Ok(blocks, hasNull) ->
                Spill.closeSink sink
                let store = new IndexedValues(path, Array.ofList blocks, bloom.Build())

                match budget.Scope with
                | Some scope -> scope.Register(box store, store)
                | None -> ()

                Ok(new ValueSet(ValueSetStorage.OnDisk store, hasNull))

    /// 上限を超えたので、ここまでの集合と残りの入力を一時ファイルへ退避する。
    let private spillToDisk
        (budget: MemoryBudget)
        (set: System.Collections.Generic.HashSet<SqlValue>)
        (hasNull: bool)
        (rest: Op)
        =
        let chunk = set |> Seq.map (fun v -> [| v |]) |> Seq.toArray |> Op.ofArray

        Spill.spool budget (Op.append chunk rest)
        >>= fun spool ->
            let total = set.Count + Spill.spoolCount spool

            Spill.reopen budget.Scope spool
            >>= ExtSort.sortRows budget None checkComparable compareValues
            >>= writeIndexed budget total hasNull

    /// `src`（1列の行）から値集合を作る。
    /// 作業メモリを超えたら一時ファイルへ退避する。
    let build (budget: MemoryBudget) (src: Op) =
        let set = System.Collections.Generic.HashSet<SqlValue>()

        let rec scan used hasNull cur =
            match Op.next cur with
            | OpStep.OpError e -> Error e
            | OpStep.OpDone ->
                sortedArray set
                |> Result.map (fun rows -> new ValueSet(ValueSetStorage.InMemory rows, hasNull))
            | OpStep.OpRow(row, nxt) ->
                let v = row.[0]

                if SqlValue.isNull v then
                    scan used true nxt
                elif set.Add v then
                    let size = int64 (ValueCodec.managedBytes v) + 48L

                    if MemoryBudget.exceeded (budget.Work, used + size) then
                        spillToDisk budget set hasNull nxt
                    else
                        scan (used + size) hasNull nxt
                else
                    scan used hasNull nxt

        scan 0L false src
