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

/// Bloom のキー候補。SQL の等値（`SqlValue.compareValuesV`）は型・DECIMAL の
/// スケール・DATE/TIMESTAMP を跨ぐため、1つの符号化に固定すると表現違いの等値で
/// 偽陰性になる。等値になり得る表現をすべてキーにする。
module internal ValueKey =
    /// 末尾の 0 を落とす（`1.0` と `1.00` を同じ尺度にする）。
    let private normalizeDecimal (d: decimal) =
        if d = 0M then 0M else d / 1.0000000000000000000000000000M

    /// 数値として等値になり得る他の型・スケールの表現。
    let private numericEquivalents (v: SqlValue) =
        let asDecimal =
            match v with
            | SqlValue.Int i -> Some(decimal i)
            | SqlValue.Decimal d -> Some d
            | SqlValue.Real f when
                not (System.Double.IsNaN f)
                && not (System.Double.IsInfinity f)
                && System.Math.Abs f <= 7.9e28
                ->
                Some(decimal f)
            | _ -> None

        match asDecimal with
        | None -> []
        | Some d ->
            let real =
                let f = float d

                if not (System.Double.IsNaN f) && not (System.Double.IsInfinity f) && decimal f = d then
                    [ SqlValue.vReal f ]
                else
                    []

            let int =
                if
                    d = System.Decimal.Truncate d
                    && d >= decimal System.Int64.MinValue
                    && d <= decimal System.Int64.MaxValue
                then
                    [ SqlValue.Int(int64 d) ]
                else
                    []

            SqlValue.Decimal(normalizeDecimal d) :: real @ int

    /// DATE と TIMESTAMP（同日 00:00 UTC）は等値なので、両方の表現を候補にする。
    let private temporalEquivalents =
        function
        | SqlValue.Date d ->
            let ts =
                System.DateTimeOffset(d.ToDateTime System.TimeOnly.MinValue, System.TimeSpan.Zero)

            [ SqlValue.Timestamp ts ]
        | SqlValue.Timestamp t when t.UtcTicks % System.TimeSpan.TicksPerDay = 0L ->
            [ SqlValue.Date(System.DateOnly.FromDateTime t.UtcDateTime) ]
        | _ -> []

    /// 値自身と、SQL の等値が成り立つ他の表現。
    let private values (v: SqlValue) =
        v :: (numericEquivalents v @ temporalEquivalents v)

    /// Bloom に登録する候補キーの符号化列。
    let encode =
        values
        >> List.map (fun x -> ValueCodec.writeValues [| x |] Writer.Empty |> Result.map Writer.toArray)
        >> all

    /// 候補キーのハッシュ列（重複なし）。
    let hashes =
        encode
        >> Result.map (
            List.map (fun bytes -> bytes |> Hashing.hashBytes |> Hashing.mix)
            >> List.distinct
        )

/// IndexedValues / ValueSet のブロック読みと二分探索の共通処理。
module private ValueSetSearch =
    /// 退避ブロックの行を配列へ読み込む。
    [<TailCall>]
    let rec readBlockFill (block: ValueBlock) (src: Source) (rows: SqlValue array) (i: int) =
        if i >= block.Count then
            Ok rows
        else
            match Spill.readRow src with
            | Error e -> Error e
            | Ok None -> Error(DbError.corruption "ValueSet: 退避したブロックが途中で終わりました")
            | Ok(Some row) ->
                rows.[i] <- row.[0]
                readBlockFill block src rows (i + 1)

    /// ソート済み配列を二分探索する本体。
    [<TailCall>]
    let rec private searchRowsLoop (rows: SqlValue array) (v: SqlValue) (lo: int) (hi: int) =
        if lo > hi then
            Ok false
        else
            let mid = (lo + hi) / 2

            match SqlValue.compareValuesV rows.[mid] v with
            | DbErr e -> Error e
            | DbOk 0 -> Ok true
            | DbOk c ->
                if c < 0 then
                    searchRowsLoop rows v (mid + 1) hi
                else
                    searchRowsLoop rows v lo (mid - 1)

    /// 退避ブロックの行を二分探索する。
    let searchIndexedRows (rows: SqlValue array) (v: SqlValue) =
        searchRowsLoop rows v 0 (rows.Length - 1)

    /// メモリ上のソート済み配列を二分探索する。
    let searchArrayRows (rows: SqlValue array) (v: SqlValue) =
        searchRowsLoop rows v 0 (rows.Length - 1)

    /// v を含み得る最後のブロックを二分探索する。
    [<TailCall>]
    let rec findBlockIndex (blocks: ValueBlock array) (v: SqlValue) (lo: int) (hi: int) (best: int option) =
        if lo > hi then
            Ok best
        else
            let mid = (lo + hi) / 2

            match SqlValue.compareValuesV blocks.[mid].First v with
            | DbErr e -> Error e
            | DbOk c ->
                if c <= 0 then
                    findBlockIndex blocks v (mid + 1) hi (Some mid)
                else
                    findBlockIndex blocks v lo (mid - 1) best

/// ソート済みファイルに置いた値集合。
/// <remarks>
/// 読み取り用のストリームと直近 1ブロックをキャッシュする。
/// 1つのクエリの評価スレッドから順に呼ばれる前提（`RowReader` と同じ）。
/// </remarks>
[<Sealed>]
type internal IndexedValues(path: string, blocks: ValueBlock array, bloom: BloomFilter) =
    /// 直近ブロックのキャッシュを保護するロック。
    let gate = obj ()

    /// 読み取り用ストリーム（初回アクセスで開く）。
    let mutable reader: System.IO.Stream option = None

    /// 直近に読んだブロックの番号。
    let mutable cachedIndex = -1

    /// 直近に読んだブロックの行。
    let mutable cachedRows: SqlValue array = [||]

    /// 破棄済みか。
    let mutable disposed = false

    /// ストリームを閉じる。
    let closeReader () =
        match reader with
        | Some s ->
            protect (fun () -> s.Dispose()) |> ignore
            reader <- None
        | None -> ()

    /// ストリームを開く（既に開いていれば再利用する）。
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

            match ValueSetSearch.readBlockFill block src rows 0 with
            | Error e -> Error e
            | Ok _ ->
                cachedIndex <- index
                cachedRows <- rows
                Ok rows

    /// 空か（NULL だけの集合は空だが、`ValueSet` 側が `HasNull` で区別する）。
    member _.IsEmpty = blocks.Length = 0

    /// 値を含むか（SQL の等値）。
    member _.Contains(v: SqlValue) =
        lock gate (fun () ->
            match ValueKey.hashes v with
            | Error e -> Error e
            | Ok hashes ->
                if not (hashes |> List.exists (BloomFilter.mightContain bloom)) then
                    Ok false
                else
                    // `v` を含み得るブロックは「先頭値が v 以下である最後のブロック」だけ。
                    match ValueSetSearch.findBlockIndex blocks v 0 (blocks.Length - 1) None with
                    | Error e -> Error e
                    | Ok None -> Ok false
                    | Ok(Some index) ->
                        match readBlock index with
                        | Error e -> Error e
                        | Ok rows -> ValueSetSearch.searchIndexedRows rows v)

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
                | ValueSetStorage.InMemory rows -> ValueSetSearch.searchArrayRows rows v
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

    /// 代表値との比較可能性を検査する本体。
    [<TailCall>]
    let rec private validateComparable (values: SqlValue array) (rep: SqlValue) (i: int) =
        if i >= values.Length then
            Ok()
        else
            match SqlValue.compareValuesV rep values.[i] with
            | DbErr e -> Error e
            | DbOk _ -> validateComparable values rep (i + 1)

    /// メモリ上の集合をソート済み配列にする。
    let private sortedArray (set: System.Collections.Generic.HashSet<SqlValue>) =
        let values = set |> Seq.toArray

        match values with
        | [||] -> Ok values
        | _ ->
            let rep = values.[0]

            match validateComparable values rep 1 with
            | Error e -> Error e
            | Ok() -> Ok(values |> Array.sortWith (fun a b -> compareValues [| a |] [| b |]))

    /// 1値のフレーム長（varint 長 + 符号化）。
    let private frameLength (v: SqlValue) =
        let payload = ValueCodec.encodedValuesLength [| v |]
        int64 (Writer.varintLength (uint64 payload)) + int64 payload

    /// 1ブロックの行を sink へ書き出す本体。
    [<TailCall>]
    let rec private writeIndexedRows (sink: Sink) (rows: SqlValue array) (i: int) (pos: int64) =
        if i >= rows.Length then
            Ok pos
        else
            match Spill.writeRow [| rows.[i] |] sink with
            | Error e -> Error e
            | Ok() -> writeIndexedRows sink rows (i + 1) (pos + frameLength rows.[i])

    /// 1ブロックを書き出し、ブロック一覧と次のオフセットを返す。
    let private writeIndexedBlock (sink: Sink) blocks (rows: SqlValue array) (offset: int64) =
        match writeIndexedRows sink rows 0 offset with
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

    /// Bloom に 1値を追加する（符号化できない値は無視）。
    let private addIndexedBloom (bloom: BloomFilter.BloomBuilder) (v: SqlValue) =
        match ValueKey.encode v with
        | Ok keys -> keys |> List.iter bloom.Add
        | Error _ -> ()

    /// ソート済みの行を索引付きファイルへ書き出すループ本体。
    [<TailCall>]
    let rec private writeIndexedLoop
        (sink: Sink)
        (bloom: BloomFilter.BloomBuilder)
        (hasNull: bool)
        (blocks: ValueBlock list)
        (buf: SqlValue array)
        (n: int)
        (offset: int64)
        (cur: Op)
        =
        match Op.next cur with
        | OpStep.OpError e -> Error e
        | OpStep.OpDone ->
            if n = 0 then
                Ok(List.rev blocks, hasNull)
            else
                writeIndexedBlock sink blocks (Array.sub buf 0 n) offset
                |> Result.map (fun (blocks', _) -> blocks', hasNull)
        | OpStep.OpRow(row, nxt) ->
            if SqlValue.isNull row.[0] then
                writeIndexedLoop sink bloom true blocks buf n offset nxt
            else
                addIndexedBloom bloom row.[0]
                buf.[n] <- row.[0]

                if n + 1 >= BlockRows then
                    match writeIndexedBlock sink blocks buf offset with
                    | Error e -> Error e
                    | Ok(blocks', offset') ->
                        writeIndexedLoop sink bloom hasNull blocks' (Array.zeroCreate BlockRows) 0 offset' nxt
                else
                    writeIndexedLoop sink bloom hasNull blocks buf (n + 1) offset nxt

    /// ソート済みの行を索引付きファイルへ書き出す。
    /// NULL は書き出さず、`HasNull` だけを立てる（比較不能な値を二分探索に混ぜない）。
    let private writeIndexed (budget: MemoryBudget) (total: int) (initialHasNull: bool) (sorted: Op) =
        let path = Spill.runPath budget.Directory (System.Guid.NewGuid().ToString "N") 0
        let bloom = BloomFilter.BloomBuilder(total, BloomFalsePositiveRate)

        match Spill.openSink path with
        | Error e -> err e
        | Ok sink ->
            match writeIndexedLoop sink bloom initialHasNull [] (Array.zeroCreate BlockRows) 0 0L sorted with
            | Error e ->
                Spill.closeSink sink
                Spill.unlink path
                err e
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

    /// 入力を走査して値集合を溜める本体。
    [<TailCall>]
    let rec private scanValues
        (budget: MemoryBudget)
        (set: System.Collections.Generic.HashSet<SqlValue>)
        (used: int64)
        (hasNull: bool)
        (cur: Op)
        =
        match Op.next cur with
        | OpStep.OpError e -> Error e
        | OpStep.OpDone ->
            sortedArray set
            |> Result.map (fun rows -> new ValueSet(ValueSetStorage.InMemory rows, hasNull))
        | OpStep.OpRow(row, nxt) ->
            let v = row.[0]

            if SqlValue.isNull v then
                scanValues budget set used true nxt
            else
                let key = SqlValue.keyNormalize v

                if set.Add key then
                    let size = int64 (ValueCodec.managedBytes key) + 48L

                    if MemoryBudget.exceeded (budget.Work, used + size) then
                        spillToDisk budget set hasNull nxt
                    else
                        scanValues budget set (used + size) hasNull nxt
                else
                    scanValues budget set used hasNull nxt

    /// `src`（1列の行）から値集合を作る。
    /// 作業メモリを超えたら一時ファイルへ退避する。
    let build (budget: MemoryBudget) (src: Op) =
        let set = System.Collections.Generic.HashSet<SqlValue>()
        scanValues budget set 0L false src
