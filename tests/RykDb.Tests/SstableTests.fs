// ===========================================================================
// Sstable.fs
// ===========================================================================

module RykDb.SstableTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private sstKey (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]

let private sstEntries n =
    Array.init n (fun i -> SstEntry.make (sstKey i) (uint64 (i + 1)) (Some [| SqlValue.Int(int64 i) |]))
    |> Array.sortWith SstEntry.compare

[<Fact>]
let ``SSTable: 構築と全件走査の往復`` () =
    let entries = sstEntries 1000
    let data = ok (Sstable.build entries 0.01)
    let reader = ok (Sstable.openTable data)

    Assert.Equal<int64>(int64 entries.Length, reader.EntryCount)

    let all = ok (Sstable.entries reader) |> Seq.toArray
    Assert.Equal(entries.Length, all.Length)

    for i in 0 .. entries.Length - 1 do
        Assert.Equal(entries.[i].SeqNo, all.[i].SeqNo)

        Assert.Equal<string>(
            SqlLiteral.toSqlLiteral entries.[i].Value.Value.[0],
            SqlLiteral.toSqlLiteral all.[i].Value.Value.[0]
        )

[<Fact>]
let ``SSTable: 複数ブロックに分割される`` () =
    // EntriesPerBlock = 128 なので 1000 件で 8 ブロック
    let entries = sstEntries 1000
    let reader = ok (Sstable.openTable (ok (Sstable.build entries 0.01)))
    Assert.Equal(8, reader.Index.Length)

    // ブロック境界をまたぐ探索ができること
    Assert.Equal<int64>(int64 entries.Length, reader.EntryCount)

[<Fact>]
let ``SSTable: rangeScan は startKey 以降を limit 件`` () =
    let reader = ok (Sstable.openTable (ok (Sstable.build (sstEntries 1000) 0.01)))

    let got = ok (Sstable.rangeScan reader (sstKey 500) 10) |> Seq.toArray
    Assert.Equal(10, got.Length)
    Assert.True(got.[0].Key = sstKey 500)

    // 終端を超える limit は実数だけ返す
    Assert.Equal(50, (ok (Sstable.rangeScan reader (sstKey 950) 100) |> Seq.length))

[<Fact>]
let ``SSTable: Bloom filter は false negative がない`` () =
    let entries = sstEntries 20_000
    let reader = ok (Sstable.openTable (ok (Sstable.build entries 0.01)))

    Assert.True(reader.Bloom.IsSome)

    for e in entries do
        Assert.True(Sstable.mayContain reader e.Key, "追加したキーが false negative になった")

    // 別キー群の誤検出率が設計値を大きく上回らない
    let others = Array.init 20_000 (fun i -> sstKey (i + 10_000_000))
    let fp = others |> Array.filter (Sstable.mayContain reader) |> Array.length
    let rate = float fp / float others.Length
    Assert.True(rate <= 0.05, sprintf "誤検出率が高すぎます: %.4f" rate)

[<Fact>]
let ``SSTable: 空のテーブル`` () =
    let data = ok (Sstable.build [||] 0.01)
    let reader = ok (Sstable.openTable data)
    Assert.Equal<int64>(0L, reader.EntryCount)
    Assert.Equal(0, (ok (Sstable.entries reader) |> Seq.length))

[<Fact>]
let ``SSTable: 並びが崩れた入力はエラー`` () =
    let bad =
        [|
            SstEntry.make (sstKey 5) 1UL (Some [| SqlValue.Int 5L |])
            SstEntry.make (sstKey 1) 2UL (Some [| SqlValue.Int 1L |])
        |]

    match Sstable.build bad 0.01 with
    | Error e -> Assert.Equal<ErrorKind>(ErrorKind.InternalError, e.Kind)
    | Ok _ -> Assert.Fail("キー順に並んでいない入力はエラーになるはず")

[<Fact>]
let ``SSTable: 壊れたデータは CorruptionError`` () =
    let data = ok (Sstable.build (sstEntries 500) 0.01)

    // 中身の 1 バイトを反転してチェックサム不一致を起こす
    let corrupted = Array.copy data
    corrupted.[corrupted.Length / 2] <- corrupted.[corrupted.Length / 2] ^^^ 0xFFuy

    match Sstable.openTable corrupted with
    | Error e -> Assert.Equal<ErrorKind>(ErrorKind.CorruptionError, e.Kind)
    | Ok _ -> Assert.Fail("壊れたデータはエラーになるはず")

    // 先頭 1 バイトを反転するとマジック不一致
    let badMagic = Array.copy data
    badMagic.[0] <- badMagic.[0] ^^^ 0xFFuy

    match Sstable.openTable badMagic with
    | Error e -> Assert.Equal<ErrorKind>(ErrorKind.CorruptionError, e.Kind)
    | Ok _ -> Assert.Fail("マジック不一致はエラーになるはず")

[<Fact>]
let ``SSTable: 全 10 型の値を往復できる`` () =
    let values =
        [|
            [| SqlValue.Null |]
            [| SqlValue.Bool true; SqlValue.Bool false |]
            [| SqlValue.Int Int64.MinValue; SqlValue.Int 0L; SqlValue.Int Int64.MaxValue |]
            [|
                SqlValue.Real -1.5
                SqlValue.Real 0.0
                SqlValue.Real Double.PositiveInfinity
            |]
            [|
                SqlValue.Decimal -12345.6789M
                SqlValue.Decimal 0M
                SqlValue.Decimal System.Decimal.MaxValue
            |]
            [| SqlValue.Text ""; SqlValue.Text "hello"; SqlValue.Text "中文" |]
            [| SqlValue.Blob [||]; SqlValue.Blob [| 1uy; 2uy; 3uy |] |]
            [| SqlValue.Date DateOnly.MinValue; SqlValue.Date DateOnly.MaxValue |]
            [| SqlValue.Timestamp(DateTimeOffset(DateTime(0L, DateTimeKind.Utc))) |]
            [| SqlValue.Uuid(Guid.Parse "01234567-89ab-cdef-0123-456789abcdef") |]
        |]

    let entries =
        values
        |> Array.mapi (fun i vs -> SstEntry.make (KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]) 1UL (Some vs))
        |> Array.sortWith SstEntry.compare

    let reader = ok (Sstable.openTable (ok (Sstable.build entries 0.01)))
    let all = ok (Sstable.entries reader) |> Seq.toArray

    for i in 0 .. values.Length - 1 do
        let got = all.[i].Value.Value
        Assert.Equal(values.[i].Length, got.Length)

        for j in 0 .. values.[i].Length - 1 do
            Assert.Equal<string>(SqlLiteral.toSqlLiteral values.[i].[j], SqlLiteral.toSqlLiteral got.[j])

[<Fact>]
let ``SSTable: 共通接頭辞が共有される`` () =
    // 共有されない場合と比べてサイズが小さいこと
    let shared =
        Array.init 1000 (fun i -> SstEntry.make (sstKey i) 1UL (Some [| SqlValue.Int(int64 i) |]))
        |> Array.sortWith SstEntry.compare

    let withSharedPrefix = ok (Sstable.build shared 0.01)

    // ランダムなキー（共有されない）とのサイズ比較
    let rng = Random 42

    let random =
        Array.init 1000 (fun _ ->
            let bs = Array.init 12 (fun _ -> byte (rng.Next(0, 256)))
            SstEntry.make bs 1UL (Some [| SqlValue.Int 1L |]))
        |> Array.sortWith SstEntry.compare

    let withoutShared = ok (Sstable.build random 0.01)

    Assert.True(
        withSharedPrefix.Length < withoutShared.Length,
        sprintf "接頭辞共有が効いていない (%d vs %d)" withSharedPrefix.Length withoutShared.Length
    )

[<Fact>]
let ``末尾再帰: 巨大 SSTable を構築して読み出せる`` () =
    let n = 200_000

    let entries =
        Array.init n (fun i -> SstEntry.make (sstKey i) (uint64 i) (Some [| SqlValue.Int(int64 i) |]))

    let data = ok (Sstable.build entries 0.01)
    let reader = ok (Sstable.openTable data)

    Assert.Equal<int64>(int64 n, reader.EntryCount)
    Assert.True(reader.Index.Length > 1)

    // 全件走査（ブロック数だけ走査する）
    let all = ok (Sstable.entries reader) |> Seq.length
    Assert.Equal<uint64>(uint64 n, uint64 all)

// ===========================================================================
// Footer
// ===========================================================================

[<Fact>]
let ``Footer: maxSeqNo が保存・復元される`` () =
    let entries =
        [|
            SstEntry.make (sstKey 1) 7UL (Some [| SqlValue.Int 1L |])
            SstEntry.make (sstKey 2) 99UL (Some [| SqlValue.Int 2L |])
            SstEntry.make (sstKey 3) 42UL (Some [| SqlValue.Int 3L |])
        |]

    let bytes = ok (Sstable.build entries 0.01)
    let reader = ok (Sstable.openTable bytes)

    Assert.Equal<uint64>(99UL, reader.MaxSeqNo)
    Assert.Equal<int64>(3L, reader.EntryCount)

[<Fact>]
let ``Footer: 空のテーブルは maxSeqNo 0`` () =
    let bytes = ok (Sstable.build [||] 0.01)
    let reader = ok (Sstable.openTable bytes)
    Assert.Equal<uint64>(0UL, reader.MaxSeqNo)
