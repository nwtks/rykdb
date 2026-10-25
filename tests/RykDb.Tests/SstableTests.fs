// ===========================================================================
// Sstable.fs
// ===========================================================================

module RykDb.SstableTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private sstEntries n =
    Array.init n (fun i -> SstEntry.make (intKey i) (uint64 (i + 1)) (Some [| SqlValue.Int(int64 i) |]))
    |> Array.sortWith SstEntry.compare

/// Footer 末尾の 4バイト（先頭 FooterSize-4 バイトのチェックサム）を付け直す。
let private fixFooterChecksum (bytes: byte array) =
    let footerStart = bytes.Length - Sstable.FooterSize

    let checksum =
        Hashing.checksum (Array.sub bytes footerStart (Sstable.FooterSize - 4))

    for i in 0..3 do
        bytes.[footerStart + Sstable.FooterSize - 4 + i] <- byte (checksum >>> (8 * i) &&& 0xFFu)

/// ブロック解析の異常系用: 1ブロック分のデータだけを持つ読み取り側を組み立てる。
let private craftedReader (data: byte array) (offset: int) (length: int) =
    {
        Data = data
        Index =
            [|
                {
                    LastKey = [||]
                    Offset = offset
                    Length = length
                }
            |]
        MinKey = [||]
        MaxKey = [||]
        Bloom = None
        EntryCount = 0L
        MaxSeqNo = 0UL
        // craftedReader は生のブロックを渡すので、チェックサム検証は行わない。
        HasBlockChecksums = false
    }

/// 先頭から全エントリを配列で返す（全件必要ならこちら）。
let private readEntries (reader: SstReader) =
    Sstable.rangeScan reader [||] 0 |> SstScan.toArray

[<Fact>]
let ``SstEntry.make / compare: キー昇順・SeqNo 降順`` () =
    let a = SstEntry.make (intKey 1) 1UL (Some [| SqlValue.Int 1L |])
    let b = SstEntry.make (intKey 1) 2UL (Some [| SqlValue.Int 1L |])
    // 同キーは SeqNo 降順（大きい方が先）
    Assert.True(SstEntry.compare b a < 0 || SstEntry.compare b a = 0)

[<Fact>]
let ``SstEntry.encodeValue / decodeValue の往復`` () =
    let e = SstEntry.make (intKey 5) 42UL (Some [| SqlValue.Int 5L |])
    let bytes = SstEntry.encodeValue e
    let seqNo, value = ok (SstEntry.decodeValue bytes)
    Assert.Equal(e.SeqNo, seqNo)
    Assert.Equal(e.Value, value)

[<Fact>]
let ``SstEntry.decodeValue: 不明な値タグは CorruptionError`` () =
    // SeqNo=1・タグ=2（0/1 以外）・要素数=0
    expectErrorKind ErrorKind.CorruptionError "不明な値タグはエラー" (SstEntry.decodeValue [| 1uy; 2uy; 0uy |])

[<Fact>]
let ``SstEntry.splitInto: n 件ずつに分割`` () =
    let items =
        [| 1; 2; 3; 4; 5 |]
        |> Array.map (fun i -> SstEntry.make (intKey i) (uint64 i) (Some [| SqlValue.Int(int64 i) |]))

    let chunks = SstEntry.splitInto 2 items
    Assert.Equal(3, chunks.Length)
    Assert.Equal(2, chunks.[0].Length)
    Assert.Equal(2, chunks.[1].Length)
    Assert.Equal(1, chunks.[2].Length)

[<Fact>]
let ``SstEntry.splitIntoBytes: エントリ数ではなく概算バイトで分割`` () =
    let text = String.replicate 1000 "x"

    let items =
        [| 1; 2; 3 |]
        |> Array.map (fun i -> SstEntry.make (intKey i) (uint64 i) (Some [| SqlValue.Text text |]))

    // 1件あたり約 1KB。目標 2.5KB なら 2 + 1 に分かれる。
    let chunks = SstEntry.splitIntoBytes 2500L items
    Assert.Equal(2, chunks.Length)
    Assert.Equal(2, chunks.[0].Length)
    Assert.Equal(1, chunks.[1].Length)

    // 空入力は空。
    Assert.Empty(SstEntry.splitIntoBytes 2500L [||])

    // 1件だけで目標を超えても、空の塊を作らず 1件ずつ出す。
    let big =
        [|
            SstEntry.make (intKey 1) 1UL (Some [| SqlValue.Text(String.replicate 5000 "y") |])
        |]

    Assert.Equal(1, (SstEntry.splitIntoBytes 100L big).Length)

[<Fact>]
let ``SstEntry.mergeSorted: ソート済みにマージ`` () =
    let a = [| SstEntry.make (intKey 1) 1UL None; SstEntry.make (intKey 3) 1UL None |]
    let b = [| SstEntry.make (intKey 2) 1UL None; SstEntry.make (intKey 4) 1UL None |]
    let merged = SstEntry.mergeSorted [ a; b ]
    let keys = merged |> Array.map (fun e -> e.Key)
    // キー昇順
    for i in 1 .. keys.Length - 1 do
        Assert.True(SstEntry.compare merged.[i - 1] merged.[i] <= 0)

[<Fact>]
let ``SstEntry.mergeSorted: 多数のソースをまとめてマージできる`` () =
    // 左偏ヒープは右スパインを明示スタックで降りてから組み直す。
    // ソース数を増やし、再帰版と同じ順序・件数になることを固定する。
    let sources =
        [ 1..64 ]
        |> List.map (fun i ->
            [|
                SstEntry.make (intKey (i * 2)) 1UL None
                SstEntry.make (intKey (i * 2 + 128)) 1UL None
            |])

    let merged = SstEntry.mergeSorted sources
    let expected = [ 2..2..128 ] @ [ 130..2..256 ]

    Assert.Equal(expected.Length, merged.Length)

    expected
    |> List.iteri (fun i n -> Assert.True(intKey n = merged.[i].Key, sprintf "位置 %d が %d ではありません" i n))

[<Fact>]
let ``SstEntry.maxSeqNo: 最大 SeqNo を返す`` () =
    let items =
        [| SstEntry.make (intKey 1) 3UL None; SstEntry.make (intKey 2) 7UL None |]

    Assert.Equal(7UL, SstEntry.maxSeqNo items)

[<Fact>]
let ``SstScan.empty: 次は None`` () =
    match SstScan.next SstScan.empty with
    | Ok None -> ()
    | other -> Assert.Fail(sprintf "empty は None を返すべき: %A" other)

[<Fact>]
let ``SstScan.emit: 1件ずつ取り出せる`` () =
    let e = SstEntry.make (intKey 1) 1UL (Some [| SqlValue.Int 1L |])
    let scan = SstScan.emit e (fun () -> SstScan.empty)
    let arr = ok (SstScan.toArray scan)
    Assert.Equal(1, arr.Length)
    Assert.Equal(e, arr.[0])

[<Fact>]
let ``SstScan.toArray: empty は空配列`` () =
    let arr = ok (SstScan.toArray SstScan.empty)
    Assert.Equal(0, arr.Length)

[<Fact>]
let ``SSTable: maxSeqNo が保存・復元される`` () =
    let entries =
        [|
            SstEntry.make (intKey 1) 7UL (Some [| SqlValue.Int 1L |])
            SstEntry.make (intKey 2) 99UL (Some [| SqlValue.Int 2L |])
            SstEntry.make (intKey 3) 42UL (Some [| SqlValue.Int 3L |])
        |]

    let bytes = ok (Sstable.build entries 0.01)
    let reader = ok (Sstable.openTable bytes)
    Assert.Equal<uint64>(99UL, reader.MaxSeqNo)
    Assert.Equal<int64>(3L, reader.EntryCount)

[<Fact>]
let ``SSTable: 空のテーブルは maxSeqNo 0`` () =
    let bytes = ok (Sstable.build [||] 0.01)
    let reader = ok (Sstable.openTable bytes)
    Assert.Equal<uint64>(0UL, reader.MaxSeqNo)

[<Fact>]
let ``SSTable: 構築と全件走査の往復`` () =
    let entries = sstEntries 1000
    let data = ok (Sstable.build entries 0.01)
    let reader = ok (Sstable.openTable data)
    Assert.Equal<int64>(int64 entries.Length, reader.EntryCount)

    let all = ok (readEntries reader) |> Seq.toArray
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
let ``SSTable: 空のテーブル`` () =
    let data = ok (Sstable.build [||] 0.01)
    let reader = ok (Sstable.openTable data)
    Assert.Equal<int64>(0L, reader.EntryCount)
    Assert.Equal(0, ok (readEntries reader) |> Seq.length)

[<Fact>]
let ``SSTable: 全 10型の値を往復できる`` () =
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
    let all = ok (readEntries reader) |> Seq.toArray

    for i in 0 .. values.Length - 1 do
        let got = all.[i].Value.Value
        Assert.Equal(values.[i].Length, got.Length)

        for j in 0 .. values.[i].Length - 1 do
            Assert.Equal<string>(SqlLiteral.toSqlLiteral values.[i].[j], SqlLiteral.toSqlLiteral got.[j])

[<Fact>]
let ``Sstable.build: 共通接頭辞が共有される`` () =
    // 共有されない場合と比べてサイズが小さいこと
    let shared =
        Array.init 1000 (fun i -> SstEntry.make (intKey i) 1UL (Some [| SqlValue.Int(int64 i) |]))
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
let ``Sstable.build: 並びが崩れた入力はエラー`` () =
    let bad =
        [|
            SstEntry.make (intKey 5) 1UL (Some [| SqlValue.Int 5L |])
            SstEntry.make (intKey 1) 2UL (Some [| SqlValue.Int 1L |])
        |]

    expectErrorKind ErrorKind.InternalError "キー順に並んでいない入力はエラーになるはず" (Sstable.build bad 0.01)

[<Fact>]
let ``Sstable.openTable: チェックサム不一致は CorruptionError`` () =
    let data = ok (Sstable.build (sstEntries 500) 0.01)
    let corrupted = Array.copy data
    corrupted.[corrupted.Length / 2] <- corrupted.[corrupted.Length / 2] ^^^ 0xFFuy
    expectErrorKind ErrorKind.CorruptionError "チェックサム不一致はエラーになるはず" (Sstable.openTable corrupted)

[<Fact>]
let ``Sstable.openTable: マジック不一致は CorruptionError`` () =
    let data = ok (Sstable.build (sstEntries 500) 0.01)
    let badMagic = Array.copy data
    let footerStart = badMagic.Length - Sstable.FooterSize
    badMagic.[footerStart] <- badMagic.[footerStart] ^^^ 0xFFuy
    fixFooterChecksum badMagic
    expectErrorKind ErrorKind.CorruptionError "マジック不一致はエラーになるはず" (Sstable.openTable badMagic)

[<Fact>]
let ``Sstable.openTable: 未対応バージョンは CorruptionError`` () =
    let data = ok (Sstable.build (sstEntries 3) 0.01)
    let bad = Array.copy data
    bad.[bad.Length - Sstable.FooterSize + 8] <- 5uy
    fixFooterChecksum bad
    expectErrorKind ErrorKind.CorruptionError "未対応バージョンはエラーになるはず" (Sstable.openTable bad)

[<Fact>]
let ``Sstable.openTable: Footer の範囲が不正なら CorruptionError`` () =
    let baseBytes = ok (Sstable.build (sstEntries 3) 0.01)
    let footerStart = baseBytes.Length - Sstable.FooterSize

    let writeI64 (bytes: byte array) (off: int) (v: int64) =
        for i in 0..7 do
            bytes.[off + i] <- byte (v >>> (8 * i) &&& 0xFFL)

    let cases: (string * (byte array -> unit)) list =
        [
            "IndexOff 負", (fun b -> writeI64 b (footerStart + 20) -1L)
            "IndexLen 負", (fun b -> writeI64 b (footerStart + 28) -1L)
            "Index 範囲外", (fun b -> writeI64 b (footerStart + 28) (int64 footerStart))
            "BloomOff 負", (fun b -> writeI64 b (footerStart + 36) -1L)
            "BloomLen 負", (fun b -> writeI64 b (footerStart + 44) -1L)
            "Bloom 範囲外", (fun b -> writeI64 b (footerStart + 44) (int64 footerStart))
            "MetaOff 負", (fun b -> writeI64 b (footerStart + 52) -1L)
            "MetaLen 負", (fun b -> writeI64 b (footerStart + 60) -1L)
            "Meta 範囲外", (fun b -> writeI64 b (footerStart + 60) (int64 footerStart))
        ]

    for name, patch in cases do
        let bytes = Array.copy baseBytes
        patch bytes
        // Footer のチェックサムを直して、範囲検証まで到達させる。
        fixFooterChecksum bytes
        expectErrorKind ErrorKind.CorruptionError name (Sstable.openTable bytes)

[<Fact>]
let ``Sstable.openTable: Footer に満たないデータは CorruptionError`` () =
    expectErrorKind ErrorKind.CorruptionError "短すぎるデータはエラー" (Sstable.openTable (Array.create 10 0uy))

[<Fact>]
let ``Sstable.openTable: v3 Footer を読み取れる`` () =
    let data = ok (Sstable.build (sstEntries 3) 0.01)
    let footerStartV4 = data.Length - Sstable.FooterSize
    let legacyFooter = Array.sub data footerStartV4 80
    legacyFooter.[8] <- 3uy

    let legacyChecksum = Hashing.checksumRange legacyFooter 0 80

    let footer =
        Writer.build [ Writer.writeBytes legacyFooter; Writer.uint32LE legacyChecksum ]

    let legacyData = Array.concat [ Array.sub data 0 footerStartV4; footer ]
    let reader = ok (Sstable.openTable legacyData)
    Assert.Equal<int64>(3L, reader.EntryCount)

[<Fact>]
let ``Sstable.openTable: Meta が壊れていても MinKey/MaxKey は空で開ける`` () =
    // 1個目の長さ付きキーは読めるが 2個目が途中で切れている meta 領域
    let body = [| 0x01uy; 0x61uy |]

    let footerBody =
        Writer.build
            [
                Writer.writeBytes (System.Text.Encoding.ASCII.GetBytes "RykDbSST")
                Writer.uint32LE 4u
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 2L
                Writer.int64LE 0L
                Writer.uint32LE (Hashing.checksum body)
                Writer.uint32LE (Hashing.checksum [||])
            ]

    let footerChecksum = Hashing.checksum footerBody

    let footer =
        Writer.build [ Writer.writeBytes footerBody; Writer.uint32LE footerChecksum ]

    let reader = ok (Sstable.openTable (Array.append body footer))
    Assert.Equal<byte array>([||], reader.MinKey)
    Assert.Equal<byte array>([||], reader.MaxKey)
    Assert.Equal<int64>(0L, reader.EntryCount)
    // Bloom なしは「含まれ得る」= true
    Assert.True(Sstable.mayContain reader (intKey 1))

[<Fact>]
let ``Sstable.openPartial: Footer のチェックサム不一致は CorruptionError`` () =
    withDir "sst-footer-checksum" (fun dir ->
        let data = ok (Sstable.build (sstEntries 100) 0.01)
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, data)

        // Footer の indexOff を壊す（Footer チェックサムは直さない）。
        let bad = Array.copy data
        let footerStart = bad.Length - Sstable.FooterSize
        bad.[footerStart + 20] <- bad.[footerStart + 20] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(path, bad)
        expectErrorKind ErrorKind.CorruptionError "部分読みでも Footer の破損を検出する" (Sstable.openPartial path)
        expectErrorKind ErrorKind.CorruptionError "全体読みでも Footer の破損を検出する" (Sstable.openTable bad))

[<Fact>]
let ``Sstable.openPartial: Bloom メタデータの破損は checksum で検出`` () =
    withDir "sst-partial-meta-checksum" (fun dir ->
        let data = ok (Sstable.build (sstEntries 100) 0.01)
        let path = System.IO.Path.Combine(dir, "1.sst")
        let footerStart = data.Length - Sstable.FooterSize

        let bloomOffset =
            [ 0..7 ]
            |> List.fold (fun acc i -> acc ||| (int64 data.[footerStart + 36 + i] <<< (i * 8))) 0L
            |> int

        data.[bloomOffset] <- data.[bloomOffset] ^^^ 0x01uy
        System.IO.File.WriteAllBytes(path, data)

        expectErrorKind ErrorKind.CorruptionError "部分読みで Bloom の改変を見逃さない" (Sstable.openPartial path))

[<Fact>]
let ``Sstable.openPartial: 全件・limit・lookup が全読みと一致する`` () =
    withDir "sst-partial" (fun dir ->
        let entries = sstEntries 300
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, ok (Sstable.build entries 0.01))

        let partial = ok (Sstable.openPartial path)
        let all = ok (Sstable.rangeScanPartial partial [||] 0 |> SstScan.toArray)
        Assert.Equal<int>(entries.Length, all.Length)
        Assert.Equal<int>(0, Bytes.compare entries.[0].Key all.[0].Key)
        Assert.Equal<int>(0, Bytes.compare entries.[entries.Length - 1].Key all.[all.Length - 1].Key)

        let limited = ok (Sstable.rangeScanPartial partial [||] 10 |> SstScan.toArray)
        Assert.Equal<int>(10, limited.Length)

        match ok (Sstable.lookup partial None (intKey 150)) with
        | Some e -> Assert.Equal<uint64>(151UL, e.SeqNo)
        | None -> Assert.Fail "lookup が行を見つけられません"

        // 可視性: SeqNo 2 ではキー 150（SeqNo 151）は見えない
        Assert.True((ok (Sstable.lookup partial (Some 2UL) (intKey 150))).IsNone))

[<Fact>]
let ``Sstable.openPartial: 壊れたブロックは読み出し時に CorruptionError`` () =
    withDir "sst-partial-corrupt" (fun dir ->
        let entries = sstEntries 300
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, ok (Sstable.build entries 0.01))

        // index を読んだ後にブロック 0 の内部を壊す
        let partial = ok (Sstable.openPartial path)
        let data = System.IO.File.ReadAllBytes path
        let block = partial.Index.[0]
        data.[block.Offset + 2] <- data.[block.Offset + 2] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(path, data)

        expectErrorKind
            ErrorKind.CorruptionError
            "部分読みでも壊れたブロックを検出する"
            (Sstable.rangeScanPartial partial [||] 0 |> SstScan.toArray))

[<Fact>]
let ``Sstable.mayContain: Bloom filter は false negative がない`` () =
    let entries = sstEntries 20_000
    let reader = ok (Sstable.openTable (ok (Sstable.build entries 0.01)))
    Assert.True reader.Bloom.IsSome

    for e in entries do
        Assert.True(Sstable.mayContain reader e.Key, "追加したキーが false negative になった")

    // 別キー群の誤検出率が設計値を大きく上回らない
    let others = Array.init 20_000 (fun i -> intKey (i + 10_000_000))
    let fp = others |> Array.filter (Sstable.mayContain reader) |> Array.length
    let rate = float fp / float others.Length
    Assert.True(rate <= 0.05, sprintf "誤検出率が高すぎます: %.4f" rate)

[<Fact>]
let ``SSTable.rangeScan: startKey 以降を limit 件`` () =
    let reader = ok (Sstable.openTable (ok (Sstable.build (sstEntries 1000) 0.01)))
    let got = ok (SstScan.toArray (Sstable.rangeScan reader (intKey 500) 10))
    Assert.Equal(10, got.Length)
    Assert.True(got.[0].Key = intKey 500)
    // 終端を超える limit は実数だけ返す
    Assert.Equal(50, ok (SstScan.toArray (Sstable.rangeScan reader (intKey 950) 100)) |> Array.length)

[<Fact>]
let ``SSTable.rangeScan: ブロック境界をまたいでも連続する`` () =
    // EntriesPerBlock = 128 なので 1000件で 8ブロック（境界は 128 の倍数）
    let reader = ok (Sstable.openTable (ok (Sstable.build (sstEntries 1000) 0.01)))
    // 境界をまたぐ
    let got = ok (SstScan.toArray (Sstable.rangeScan reader (intKey 120) 20))
    Assert.Equal(20, got.Length)
    Assert.Equal<byte array>(intKey 120, got.[0].Key)
    Assert.Equal<byte array>(intKey 139, got.[19].Key)

    // 境界ちょうどから
    let fromBoundary = ok (SstScan.toArray (Sstable.rangeScan reader (intKey 128) 3))
    Assert.Equal(3, fromBoundary.Length)
    Assert.Equal<byte array>(intKey 128, fromBoundary.[0].Key)
    Assert.Equal<byte array>(intKey 130, fromBoundary.[2].Key)
    // 最後のブロックの途中からは残りだけ
    Assert.Equal(10, ok (SstScan.toArray (Sstable.rangeScan reader (intKey 990) 0)) |> Array.length)
    // MaxKey より後ろは空
    Assert.Empty(ok (SstScan.toArray (Sstable.rangeScan reader (intKey 1000) 0)))

[<Fact>]
let ``SSTable.rangeScan: 必要なブロックだけ読む`` () =
    let reader = ok (Sstable.openTable (ok (Sstable.build (sstEntries 200_000) 0.01)))
    // 走査の確保バイト数を測る（1回走らせて JIT 分を除いてから）
    let measure (scan: unit -> SstEntry array) =
        scan () |> ignore
        let before = GC.GetAllocatedBytesForCurrentThread()
        let got = scan ()
        GC.GetAllocatedBytesForCurrentThread() - before, got

    let firstAlloc, first =
        measure (fun () -> ok (SstScan.toArray (Sstable.rangeScan reader (intKey 0) 1)))

    let allAlloc, all =
        measure (fun () -> ok (SstScan.toArray (Sstable.rangeScan reader (intKey 0) 0)))

    Assert.Equal(1, first.Length)
    Assert.Equal<byte array>(intKey 0, first.[0].Key)
    Assert.Equal(200_000, all.Length)

    // 先頭 1件は 1ブロック（128件）分しか解析しない。
    // 全件（200,000件）を読み切るのに比べ、2桁以上小さいはず。
    Assert.True(firstAlloc * 100L < allAlloc, $"先頭  件で {firstAlloc} バイト（全件は {allAlloc} バイト）")

[<Fact>]
let ``SSTable.rangeScan: 壊れたブロックは Error になる`` () =
    // ブロックが短すぎる
    let short = craftedReader (Array.create 4 0uy) 0 4
    expectErrorKind ErrorKind.CorruptionError "短すぎるブロックはエラー" (SstScan.toArray (Sstable.rangeScan short [||] 0))

    // restart 配列の件数が 0
    let zeroRestarts = craftedReader (Array.create 8 0uy) 0 8

    expectErrorKind
        ErrorKind.CorruptionError
        "restart 件数 0 はエラー"
        (SstScan.toArray (Sstable.rangeScan zeroRestarts [||] 0))

    // restart 配列が本体より大きい（件数 3 → 16 バイト > 8 バイト）
    let bigRestarts = craftedReader [| 0uy; 0uy; 0uy; 0uy; 3uy; 0uy; 0uy; 0uy |] 0 8

    expectErrorKind
        ErrorKind.CorruptionError
        "restart 配列が大きすぎればエラー"
        (SstScan.toArray (Sstable.rangeScan bigRestarts [||] 0))

    // エントリの varint が途中で切れる（restart 配列は正常）
    let truncatedEntry =
        craftedReader [| 0x80uy; 0uy; 0uy; 0uy; 0uy; 1uy; 0uy; 0uy; 0uy |] 0 9

    expectErrorKind
        ErrorKind.CorruptionError
        "途中で切れたエントリはエラー"
        (SstScan.toArray (Sstable.rangeScan truncatedEntry [||] 0))

[<Fact>]
let ``FileMetaData.zero: 空メタデータ`` () =
    Assert.Equal(0L, FileMetaData.zero.FileNumber)
    Assert.Equal(0, FileMetaData.zero.Level)
    Assert.Equal(0L, FileMetaData.zero.EntryCount)

[<Fact>]
let ``FileMetaData.writeSstable: 書き出してメタデータを返す`` () =
    withDir "fmd-write" (fun dir ->
        let entries = sstEntries 5
        let fmd = ok (FileMetaData.writeSstable dir 1L 0 0.01 entries)
        Assert.Equal(1L, fmd.FileNumber)
        Assert.Equal(0, fmd.Level)
        Assert.Equal(5L, fmd.EntryCount)
        Assert.True(fmd.Size > 0L))

[<Fact>]
let ``FileMetaData.writeSstable: キー順でない入力はエラー`` () =
    withDir "fmd-write-bad" (fun dir ->
        let bad =
            [|
                SstEntry.make (intKey 5) 1UL (Some [| SqlValue.Int 5L |])
                SstEntry.make (intKey 1) 2UL (Some [| SqlValue.Int 1L |])
            |]

        expectErrorKind ErrorKind.InternalError "キー順でない入力はエラーになるはず" (FileMetaData.writeSstable dir 1L 0 0.01 bad))

[<Fact>]
let ``FileMetaData.writeSstable: 書き込み失敗は IoError`` () =
    withDir "fmd-write-io" (fun dir ->
        let missing = System.IO.Path.Combine(dir, "no-such-dir")

        expectErrorKind
            ErrorKind.IoError
            "存在しないディレクトリへの書き込みはエラーになるはず"
            (FileMetaData.writeSstable missing 1L 0 0.01 (sstEntries 3)))

[<Fact>]
let ``FileMetaData.mightContain: 範囲内なら true`` () =
    let f = FileMetaData.zero

    let f2 =
        { f with
            Smallest = [| 1uy |]
            Largest = [| 5uy |]
        }

    Assert.True(FileMetaData.mightContain f2 [| 3uy |])
    Assert.False(FileMetaData.mightContain f2 [| 0uy |])
    Assert.False(FileMetaData.mightContain f2 [| 9uy |])

[<Fact>]
let ``FileMetaData.foldFiles: 全ファイルを畳み、最初のエラーで打ち切る`` () =
    let files =
        [
            { FileMetaData.zero with
                FileNumber = 1L
            }
            { FileMetaData.zero with
                FileNumber = 2L
            }
        ]

    let sum =
        ok (FileMetaData.foldFiles (fun f -> Ok f.FileNumber) (fun s n -> s + n) 0L files)

    Assert.Equal(3L, sum)

    let read (_: FileMetaData) = Error(DbError.corruption "読めません")
    expectErrorKind ErrorKind.CorruptionError "read のエラーで打ち切る" (FileMetaData.foldFiles read (fun s _ -> s) 0L files)
