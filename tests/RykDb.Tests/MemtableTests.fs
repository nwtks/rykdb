// ===========================================================================
// Memtable.fs（永続 Treap）
//
// getVisible の探索方向を間違えると「一部のキーだけ引けない」という壊れ方をするので、
// 全キーを 1つずつ引くテストを置いている。
// ===========================================================================

module RykDb.MemtableTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// (キー番号, SeqNo) の並びから memtable を作る。
let private build (entries: (int * uint64) list) =
    let es =
        entries |> List.map (fun (k, seqNo) -> Entry.put (intKey k) seqNo (intRow k))

    Memtable.apply es Memtable.Empty

[<Fact>]
let ``Memtable: 空の memtable は何も返さない`` () =
    Assert.Equal(0, Memtable.Empty.Count)
    Assert.Equal(0, Memtable.Empty.ApproxBytes)
    Assert.Empty(Memtable.toArray Memtable.Empty)
    Assert.Empty(Memtable.rangeScan (intKey 0) 0 Memtable.Empty)
    Assert.True((Memtable.getVisible (intKey 0) None Memtable.Empty).IsNone)

[<Fact>]
let ``Memtable: apply したエントリを getVisible で引ける`` () =
    let mem =
        Memtable.apply
            [|
                Entry.put (intKey 1) 1UL [| SqlValue.Int 10L |]
                Entry.put (intKey 2) 2UL [| SqlValue.Text "hello" |]
                Entry.put (intKey 3) 3UL [| SqlValue.Blob [| 1uy; 2uy; 3uy |] |]
            |]
            Memtable.Empty

    Assert.Equal(3, mem.Count)
    Assert.Equal<SqlValue array>([| SqlValue.Int 10L |], (entry (Memtable.getVisible (intKey 1) None mem)).Value.Value)

    Assert.Equal<SqlValue array>(
        [| SqlValue.Text "hello" |],
        (entry (Memtable.getVisible (intKey 2) None mem)).Value.Value
    )

    Assert.Equal<SqlValue array>(
        [| SqlValue.Blob [| 1uy; 2uy; 3uy |] |],
        (entry (Memtable.getVisible (intKey 3) None mem)).Value.Value
    )

    Assert.True((Memtable.getVisible (intKey 99) None mem).IsNone)

[<Fact>]
let ``Memtable: apply しても元の memtable は変わらない`` () =
    let before = Memtable.apply [ Entry.put (intKey 1) 1UL (intRow 1) ] Memtable.Empty
    let after = Memtable.apply [ Entry.put (intKey 2) 2UL (intRow 2) ] before
    Assert.Equal(1, before.Count)
    Assert.Equal(2, after.Count)
    // before から見ると追加したキーは見えない
    Assert.True((Memtable.getVisible (intKey 2) None before).IsNone)
    Assert.True((Memtable.getVisible (intKey 2) None after).IsSome)
    // 変更していないキーの内容も変わらない
    Assert.Equal<SqlValue array>(
        [| SqlValue.Int 1L |],
        (entry (Memtable.getVisible (intKey 1) None before)).Value.Value
    )

    Assert.Equal<SqlValue array>([| SqlValue.Int 1L |], (entry (Memtable.getVisible (intKey 1) None after)).Value.Value)

[<Fact>]
let ``Memtable: Count と ApproxBytes は挿入したエントリの合計になる`` () =
    let entries =
        [|
            Entry.put (intKey 1) 1UL [| SqlValue.Int 10L |]
            Entry.put (intKey 2) 2UL [| SqlValue.Text "abcdef" |]
            Entry.delete (intKey 3) 3UL
        |]

    let mem = Memtable.apply entries Memtable.Empty
    Assert.Equal(entries.Length, mem.Count)
    Assert.Equal(entries |> Array.sumBy Entry.approxBytes, mem.ApproxBytes)

    // 空への apply は何も変えない
    let same = Memtable.apply [] mem
    Assert.Equal(mem.Count, same.Count)
    Assert.Equal(mem.ApproxBytes, same.ApproxBytes)

[<Fact>]
let ``Memtable / SSTable の概算は固定長の値で同じ定数を使う`` () =
    let values =
        [
            SqlValue.Null
            SqlValue.Bool true
            SqlValue.Int 42L
            SqlValue.Real 1.5
            SqlValue.Decimal 1.5M
            SqlValue.Date(System.DateOnly(2026, 1, 2))
            SqlValue.Timestamp(System.DateTimeOffset(2026, 1, 2, 3, 4, 5, System.TimeSpan.Zero))
            SqlValue.Uuid System.Guid.Empty
        ]

    let key = [| 1uy |]

    // Entry 側の overhead は「キー + SeqNo 8 + 値の個数 2」、
    // SstEntry 側は「キー + 値の個数 4」。固定長なら差は常に 6 になる。
    for v in values do
        let a = Entry.approxBytes (Entry.put key 1UL [| v |])
        let b = SstEntry.approximateBytes (SstEntry.make key 1UL (Some [| v |]))
        Assert.Equal(6, a - b)

[<Fact>]
let ``Memtable: 同一キーは SeqNo が最大の版を返す`` () =
    // 挿入順をわざとばらばらにする
    let mem =
        [ 7UL; 3UL; 9UL; 1UL ]
        |> List.fold
            (fun m seqNo -> Memtable.apply [ Entry.put (intKey 42) seqNo (intRow (int seqNo)) ] m)
            Memtable.Empty

    let e = entry (Memtable.getVisible (intKey 42) None mem)
    Assert.Equal<uint64>(9UL, e.SeqNo)

    // 同一キーの全版が保持され、SeqNo 降順に並ぶ
    let versions = Memtable.toArray mem |> Array.filter (fun e -> e.Key = intKey 42)
    Assert.Equal(4, versions.Length)
    Assert.Equal<uint64 array>([| 9UL; 7UL; 3UL; 1UL |], versions |> Array.map (fun e -> e.SeqNo))

[<Fact>]
let ``Memtable: 削除 tombstone が最新なら削除として返る`` () =
    let mem =
        Memtable.Empty
        |> Memtable.apply [ Entry.put (intKey 1) 1UL (intRow 1) ]
        |> Memtable.apply [ Entry.delete (intKey 1) 2UL ]

    let e = entry (Memtable.getVisible (intKey 1) None mem)
    Assert.True e.IsDelete
    Assert.Equal<uint64>(2UL, e.SeqNo)
    // tombstone も 1 版として残る（コンパクションまで消さない）
    Assert.Equal(2, mem.Count)

[<Fact>]
let ``Memtable: 全キーを個別に引ける（探索方向の誤りを検出する）`` () =
    let n = 2_000

    let mem =
        [
            for i in 0 .. n - 1 -> Entry.put (intKey i) (uint64 i + 1UL) [| SqlValue.Int(int64 (i * 3)) |]
        ]
        |> fun es -> Memtable.apply es Memtable.Empty

    for i in 0 .. n - 1 do
        let e = entry (Memtable.getVisible (intKey i) None mem)
        Assert.Equal<int64>(int64 (i * 3), e.Value.Value.[0] |> asInt)

    // 範囲外のキーは None（境界の直前・直後）
    Assert.True((Memtable.getVisible (intKey -1) None mem).IsNone)
    Assert.True((Memtable.getVisible (intKey n) None mem).IsNone)

[<Fact>]
let ``Memtable: visible より新しい版は見えない`` () =
    let mem =
        Memtable.apply
            [|
                Entry.put (intKey 1) 1UL (intRow 1)
                Entry.put (intKey 1) 5UL (intRow 5)
                Entry.put (intKey 1) 9UL (intRow 9)
            |]
            Memtable.Empty

    // 最新から見える版を選ぶ（SeqNo が visible 以下で最大）
    Assert.Equal<uint64>(5UL, (entry (Memtable.getVisible (intKey 1) (Some 6UL) mem)).SeqNo)
    Assert.Equal<uint64>(1UL, (entry (Memtable.getVisible (intKey 1) (Some 1UL) mem)).SeqNo)
    // どの版より古い snapshot からは存在しない
    Assert.True((Memtable.getVisible (intKey 1) (Some 0UL) mem).IsNone)

[<Fact>]
let ``Memtable: 削除も snapshot で見え隠れする`` () =
    let mem =
        Memtable.apply [| Entry.put (intKey 1) 1UL (intRow 1); Entry.delete (intKey 1) 5UL |] Memtable.Empty

    // 削除前の snapshot では行が見える
    Assert.False((entry (Memtable.getVisible (intKey 1) (Some 4UL) mem)).IsDelete)
    // 削除後の snapshot では tombstone が見える
    Assert.True((entry (Memtable.getVisible (intKey 1) (Some 5UL) mem)).IsDelete)

[<Fact>]
let ``Memtable: 接頭辞のキーは短い方が小さい`` () =
    let shorter = [| 1uy |]
    let longer = [| 1uy; 0uy |]

    let mem =
        Memtable.apply [| Entry.put longer 2UL (intRow 2); Entry.put shorter 1UL (intRow 1) |] Memtable.Empty

    let all = Memtable.toArray mem
    Assert.Equal<byte array>(shorter, all.[0].Key)
    Assert.Equal<byte array>(longer, all.[1].Key)
    // 短いキーのポイント検索が長いキーに引きずられない
    Assert.Equal<uint64>(1UL, (entry (Memtable.getVisible shorter None mem)).SeqNo)
    Assert.Equal<uint64>(2UL, (entry (Memtable.getVisible longer None mem)).SeqNo)

[<Fact>]
let ``Memtable: 乱数キーでも列挙順と可視性が参照実装と一致する`` () =
    let n = 600

    // 32 種類のキーに複数版を混ぜる（同一キーの版が複数できるようにする）
    let entries =
        [|
            for _ in 1..n ->
                let k = nextInt 0 32
                k, Entry.put (intKey k) (uint64 (nextInt 1 10_000)) (intRow (k * 1000))
        |]

    let mem = Memtable.apply (entries |> Array.map snd) Memtable.Empty
    Assert.Equal(n, mem.Count)

    // 参照実装: キーごとの版を SeqNo 降順に並べたもの
    let expectedSeqNos (k: int) =
        entries
        |> Array.filter (fun (k2, _) -> k2 = k)
        |> Array.map (fun (_, e) -> e.SeqNo)
        |> Array.sortDescending

    let all = Memtable.toArray mem
    Assert.Equal(n, all.Length)

    // 列挙は「キー昇順・同一キーは SeqNo 降順」
    all
    |> Array.pairwise
    |> Array.iter (fun (a, b) ->
        let c = compareKeyBytes a.Key b.Key
        Assert.True(c < 0 || c = 0 && a.SeqNo >= b.SeqNo, "列挙順が崩れている"))

    for k in 0..31 do
        let key = intKey k
        let expected = expectedSeqNos k

        let actual =
            all |> Array.filter (fun e -> e.Key = key) |> Array.map (fun e -> e.SeqNo)

        Assert.Equal<uint64 array>(expected, actual)

        // 最新版（visible なし）が参照実装と一致する
        match Memtable.getVisible key None mem, expected |> Array.tryHead with
        | Some e, Some s -> Assert.Equal(s, e.SeqNo)
        | None, None -> ()
        | _ -> Assert.Fail(sprintf "キー %d の最新版が一致しない" k)

        // snapshot を挟んだ可視性も一致する
        for _ in 1..10 do
            let snap = uint64 (nextInt 1 10_000)
            let expectedAt = expected |> Array.tryFind (fun s -> s <= snap)

            match Memtable.getVisible key (Some snap) mem, expectedAt with
            | Some e, Some s -> Assert.Equal(s, e.SeqNo)
            | None, None -> ()
            | _ -> Assert.Fail(sprintf "キー %d（snapshot %d）の可視版が一致しない" k snap)

    // 存在しないキーは None
    Assert.True((Memtable.getVisible (intKey 100) None mem).IsNone)

[<Fact>]
let ``Memtable: rangeScan は startKey 以降を limit 件返す`` () =
    let n = 1_000
    let mem = build [ for i in 0 .. n - 1 -> i, uint64 i + 1UL ]
    let got = Memtable.rangeScan (intKey 500) 10 mem |> Seq.toArray
    Assert.Equal(10, got.Length)
    Assert.Equal<byte array>(intKey 500, got.[0].Key)
    Assert.Equal<byte array>(intKey 509, got.[9].Key)
    // 終端を超える limit は実在する分だけ返す
    Assert.Equal(10, Memtable.rangeScan (intKey 990) 100 mem |> Seq.length)
    // limit 0 は無制限（startKey 以降の全部）
    Assert.Equal(500, Memtable.rangeScan (intKey 500) 0 mem |> Seq.length)
    // 全範囲より後ろなら空
    Assert.Empty(Memtable.rangeScan (intKey n) 0 mem)

[<Fact>]
let ``Memtable: startKey が空なら先頭から列挙する`` () =
    let mem = build [ 1, 1UL; 2, 2UL; 3, 3UL ]
    let all = Memtable.rangeScan [||] 0 mem |> Seq.toArray
    Assert.Equal(3, all.Length)
    Assert.Equal<byte array>(intKey 1, all.[0].Key)
    Assert.Equal<byte array>(intKey 3, all.[2].Key)

[<Fact>]
let ``Memtable: rangeScan は同一キーの全版を SeqNo 降順で返す`` () =
    let mem = build [ 1, 1UL; 2, 2UL; 2, 5UL; 2, 3UL; 3, 4UL ]
    let got = Memtable.rangeScan (intKey 2) 0 mem |> Seq.toArray
    // キー 2 の 3 版 + キー 3
    Assert.Equal(4, got.Length)
    Assert.Equal<uint64 array>([| 5UL; 3UL; 2UL; 4UL |], got |> Array.map (fun e -> e.SeqNo))
    Assert.Equal<byte array>(intKey 2, got.[0].Key)
    Assert.Equal<byte array>(intKey 3, got.[3].Key)

[<Fact>]
let ``Memtable: rangeScan は先頭 1件だけなら全件を実体化しない`` () =
    let n = 2_000
    let mem = build [ for i in 0 .. n - 1 -> i, uint64 i + 1UL ]
    // 一度走らせて JIT 分を測定から除く
    Memtable.rangeScan (intKey 0) 0 mem |> Seq.head |> ignore

    // 先頭 1件は根から左端まで下りるだけ（O(log n)）。
    // 全件を list に積んでいたら n 件分（20 万バイト超）を確保してしまう。
    let before = System.GC.GetAllocatedBytesForCurrentThread()
    let head = Memtable.rangeScan (intKey 0) 0 mem |> Seq.head
    let allocated = System.GC.GetAllocatedBytesForCurrentThread() - before
    Assert.Equal<byte array>(intKey 0, head.Key)
    Assert.True(allocated < 100_000L, $"先頭 1 件の取得で {allocated} バイト確保した（遅延していない）")

[<Fact>]
let ``Memtable: 挿入順に関わらずキー昇順で列挙される`` () =
    let n = 1_000
    // 決定的なシャッフル（37 と 1000 は互いに素）
    let shuffled = List.init n id |> List.sortBy (fun i -> i * 37 % n)

    let mem =
        shuffled
        |> List.map (fun i -> Entry.put (intKey i) (uint64 i + 1UL) (intRow i))
        |> fun es -> Memtable.apply es Memtable.Empty

    let all = Memtable.toArray mem
    Assert.Equal(n, all.Length)

    all
    |> Array.pairwise
    |> Array.iter (fun (a, b) -> Assert.True(compareKeyBytes a.Key b.Key < 0, "キー昇順になっていない"))

[<Fact>]
let ``Memtable: 別のキーが混ざっていても可視な最新版を返す`` () =
    // 同じキーの版が連続して並ぶ保証はない（Treap の形は挿入順で変わる）。
    let mem =
        Memtable.apply
            [|
                Entry.put (intKey 1) 1UL (intRow 1)
                Entry.put (intKey 2) 2UL (intRow 2)
                Entry.put (intKey 1) 5UL (intRow 5)
                Entry.put (intKey 2) 7UL (intRow 7)
                Entry.put (intKey 1) 9UL (intRow 9)
            |]
            Memtable.Empty

    // 最新版（SeqNo が最大の版）
    Assert.Equal<uint64>(9UL, (entry (Memtable.getVisible (intKey 1) None mem)).SeqNo)
    Assert.Equal<uint64>(7UL, (entry (Memtable.getVisible (intKey 2) None mem)).SeqNo)

    // snapshot で切ると、その時点で最新の版
    Assert.Equal<uint64>(5UL, (entry (Memtable.getVisible (intKey 1) (Some 6UL) mem)).SeqNo)
    Assert.Equal<uint64>(2UL, (entry (Memtable.getVisible (intKey 2) (Some 6UL) mem)).SeqNo)
    Assert.Equal<uint64>(1UL, (entry (Memtable.getVisible (intKey 1) (Some 1UL) mem)).SeqNo)

    // どの版も見えない / 存在しないキー
    Assert.True((Memtable.getVisible (intKey 1) (Some 0UL) mem).IsNone)
    Assert.True((Memtable.getVisible (intKey 99) None mem).IsNone)
    Assert.True((Memtable.getVisible (intKey 99) (Some 100UL) mem).IsNone)

[<Fact>]
let ``Memtable: getVisible は探索が対数時間（多数の版があっても比例しない）`` () =
    // 同じキーの版を 2000個積む。
    let versions = 2_000

    let mem =
        Seq.init versions (fun i -> Entry.put (intKey 1) (uint64 i) (intRow i))
        |> fun es -> Memtable.apply es Memtable.Empty

    let sw = System.Diagnostics.Stopwatch.StartNew()

    for _ in 1..1_000 do
        Memtable.getVisible (intKey 1) None mem |> ignore

    let ms = sw.ElapsedMilliseconds
    // 「実用時間内」の緩い基準（探索が木の大きさに比例していないこと）。
    Assert.True(ms < 2000L, sprintf "getVisible が遅すぎます: %d ms" ms)
