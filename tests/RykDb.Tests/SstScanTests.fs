// ===========================================================================
// SstScan.fs（SstEntry / ScanScope / SstScan / SstMerge）
// ===========================================================================

module RykDb.SstScanTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 配列を遅延スキャンにする（SstMerge の入力）。
let private scanOfArray (items: SstEntry array) =
    let rec from (i: int) =
        if i >= items.Length then
            SstScan.Empty
        else
            SstScan.emit (fun () -> from (i + 1)) items.[i]

    from 0

/// マージを最後まで読む。
let private drainMerge (merge: SstMerge) =
    let rec loop (m: SstMerge) (acc: SstEntry list) =
        match SstMerge.next m with
        | Error e ->
            Assert.Fail(sprintf "マージでエラー: %s" (DbError.toDisplayString e))
            Unchecked.defaultof<SstEntry array>
        | Ok None -> List.rev acc |> List.toArray
        | Ok(Some(e, rest)) -> loop rest (e :: acc)

    loop merge []

/// 同じ SstScan 値を 2回 `SstScan.next` すると失敗させる。
/// `SstScan` の one-shot 不変条件（元の値を再利用せず、返された継続だけを使う）を機械的に検査する。
let private oneShotScan (label: string) (scan: SstScan) =
    let used = ref false

    SstScan.ofStep (fun () ->
        if used.Value then
            failwithf "SstScan を 2 回消費しました: %s" label

        used.Value <- true
        SstScan.next scan)

// ---------------------------------------------------------------------------
// SstEntry
// ---------------------------------------------------------------------------

[<Fact>]
let ``SstEntry.make / compare: 同キーは SeqNo 降順`` () =
    let a = SstEntry.make (intKey 1) 1UL (Some [| SqlValue.Int 1L |])
    let b = SstEntry.make (intKey 1) 2UL (Some [| SqlValue.Int 1L |])
    // 同キーは SeqNo 降順（大きい方が先）
    Assert.True(SstEntry.compare b a < 0 || SstEntry.compare b a = 0)

[<Fact>]
let ``SstEntry.decodeValue: 不明な値タグは CorruptionError`` () =
    // SeqNo=1・タグ=2（0/1 以外）・要素数=0
    expectErrorKind ErrorKind.CorruptionError "不明な値タグはエラー" (SstEntry.decodeValue [| 1uy; 2uy; 0uy |])

// ---------------------------------------------------------------------------
// ScanScope
// ---------------------------------------------------------------------------

[<Fact>]
let ``ScanScope: 早期終了しても登録した走査を閉じられる`` () =
    let entries = sstEntries 3
    let mutable closed = false
    let close () = closed <- true
    let loadNext (_: int) = ret None
    let scan = SstScan.ofArray entries 0 0 0 loadNext close
    let scope = ScanScope()
    let scoped = SstScan.filter (fun _ -> true) (SstScan.scoped scope scan)
    Assert.Equal(1, scope.ActiveCount)
    Assert.True((ok (SstScan.next scoped)).IsSome)
    Assert.Equal(1, scope.ActiveCount)
    scope.DisposeAll()
    Assert.Equal(0, scope.ActiveCount)
    Assert.True closed
    SstScan.dispose scoped

[<Fact>]
let ``ScanScope: 最後まで読むと登録が外れる`` () =
    let entries = sstEntries 2
    let scope = ScanScope()
    let scoped = SstScan.scoped scope (scanOfEntries entries)
    let got = ok (SstScan.toArray scoped)
    Assert.Equal<int>(2, got.Length)
    Assert.Equal(0, scope.ActiveCount)

// ---------------------------------------------------------------------------
// SstScan
// ---------------------------------------------------------------------------

[<Fact>]
let ``SstScan.empty: 次は None`` () =
    match SstScan.next SstScan.Empty with
    | Ok None -> ()
    | other -> Assert.Fail(sprintf "empty は None を返すべき: %A" other)

[<Fact>]
let ``SstScan.emit: 1件ずつ取り出せる`` () =
    let e = SstEntry.make (intKey 1) 1UL (Some [| SqlValue.Int 1L |])
    let scan = SstScan.emit (fun () -> SstScan.Empty) e
    let arr = ok (SstScan.toArray scan)
    Assert.Equal(1, arr.Length)
    Assert.Equal(e, arr.[0])

[<Fact>]
let ``SstScan.toArray: empty は空配列`` () =
    let arr = ok (SstScan.toArray SstScan.Empty)
    Assert.Equal(0, arr.Length)

[<Fact>]
let ``SstScan.filter: 述語を満たすエントリだけ返す`` () =
    let entries = sstEntries 20
    let scan = scanOfEntries entries |> SstScan.filter (fun e -> e.SeqNo % 2UL = 0UL)
    let got = ok (SstScan.toArray scan)
    let expected = entries |> Array.filter (fun e -> e.SeqNo % 2UL = 0UL)
    Assert.Equal<SstEntry array>(expected, got)

[<Fact>]
let ``SstScan.filter: 入れ子のフィルタは両方適用される`` () =
    let entries = sstEntries 30

    let scan =
        scanOfEntries entries
        |> SstScan.filter (fun e -> e.SeqNo >= 5UL)
        |> SstScan.filter (fun e -> e.SeqNo < 15UL)

    let got = ok (SstScan.toArray scan)
    let expected = entries |> Array.filter (fun e -> e.SeqNo >= 5UL && e.SeqNo < 15UL)
    Assert.Equal<SstEntry array>(expected, got)

[<Fact>]
let ``SstScan.withScan: 成功・失敗のどちらでも Dispose する`` () =
    let closed = ref 0

    let make () =
        SstScan.ofState () (fun (_: unit) -> ret None) (fun (_: unit) -> closed.Value <- closed.Value + 1)

    let okResult = SstScan.withScan (make ()) (fun _ -> ret 42)
    Assert.Equal<int>(42, ok okResult)

    let errResult = SstScan.withScan (make ()) (fun _ -> err (DbError.notFound "x"))
    expectErrorKind ErrorKind.NotFound "withScan はエラーを伝播するはず" errResult
    Assert.Equal<int>(2, closed.Value)

[<Fact>]
let ``SstScan: one-shot ガードは同じ値の 2回目の next を検出する`` () =
    let scan = oneShotScan "guard" (scanOfEntries (sstEntries 1))
    Assert.True((ok (SstScan.next scan)).IsSome)

    let mutable detected = false

    try
        SstScan.next scan |> ignore
    with _ ->
        detected <- true

    Assert.True detected

[<Fact>]
let ``SstScan: filter / toArray はソースを 1回だけ消費する`` () =
    let source = oneShotScan "filter" (scanOfEntries (sstEntries 3))
    let filtered = SstScan.filter (fun _ -> true) source
    let got = ok (SstScan.toArray filtered)
    Assert.Equal<int>(3, got.Length)

// ---------------------------------------------------------------------------
// SstMerge
// ---------------------------------------------------------------------------

[<Fact>]
let ``SstMerge: ソート済みの列をまとめてマージできる`` () =
    let a = [| SstEntry.make (intKey 1) 1UL None; SstEntry.make (intKey 3) 1UL None |]
    let b = [| SstEntry.make (intKey 2) 1UL None; SstEntry.make (intKey 4) 1UL None |]
    let merge = ok (SstMerge.ofScans [| scanOfArray a; scanOfArray b |])
    let merged = drainMerge merge
    Assert.Equal<byte array array>([| intKey 1; intKey 2; intKey 3; intKey 4 |], merged |> Array.map _.Key)

    for i in 1 .. merged.Length - 1 do
        Assert.True(SstEntry.compare merged.[i - 1] merged.[i] <= 0)

[<Fact>]
let ``SstMerge: 同一キーは SeqNo 降順で、版が欠けない`` () =
    let a = [| SstEntry.make (intKey 1) 5UL None; SstEntry.make (intKey 1) 1UL None |]
    let b = [| SstEntry.make (intKey 1) 3UL None |]
    let merge = ok (SstMerge.ofScans [| scanOfArray a; scanOfArray b |])
    let merged = drainMerge merge
    Assert.Equal<uint64 list>([ 5UL; 3UL; 1UL ], merged |> Array.map _.SeqNo |> Array.toList)

[<Fact>]
let ``SstMerge: 多数のソースをまとめてマージできる`` () =
    // 左偏ヒープは右スパインを明示スタックで降りてから組み直す。
    // ソース数を増やし、再帰版と同じ順序・件数になることを固定する。
    let sources =
        [ 1..64 ]
        |> List.map (fun i ->
            [|
                SstEntry.make (intKey (i * 2)) 1UL None
                SstEntry.make (intKey (i * 2 + 128)) 1UL None
            |])

    let merge = ok (SstMerge.ofScans (sources |> List.map scanOfArray |> List.toArray))
    let merged = drainMerge merge
    let expected = [ 2..2..128 ] @ [ 130..2..256 ]
    Assert.Equal(expected.Length, merged.Length)

    expected
    |> List.iteri (fun i n -> Assert.True(intKey n = merged.[i].Key, sprintf "位置 %d が %d ではありません" i n))
