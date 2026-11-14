// ===========================================================================
// Sstable.fs
// ===========================================================================

module RykDb.SstableTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// バイト列を一時ファイルに書き、部分読みリーダーを開く。
let private writeTempSst (data: byte array) =
    let path =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid().ToString("N") + ".sst")

    System.IO.File.WriteAllBytes(path, data)
    path

/// Footer 末尾の 4バイト（先頭 FooterSize-4 バイトのチェックサム）を付け直す。
let private fixFooterChecksum (bytes: byte array) =
    let footerStart = bytes.Length - Sstable.FooterSize

    let checksum =
        Hashing.checksum (Array.sub bytes footerStart (Sstable.FooterSize - 4))

    for i in 0..3 do
        bytes.[footerStart + Sstable.FooterSize - 4 + i] <- byte (checksum >>> (8 * i) &&& 0xFFu)

/// 先頭から全エントリを配列で返す（全件必要ならこちら）。
let private readEntries (partial: SstPartial) =
    Sstable.rangeScanPartial [||] 0 partial |> SstScan.toArray

[<Fact>]
let ``Sstable.writeTo: 入力の seq は 1 回だけ列挙する`` () =
    let items =
        [|
            for i in 1..5 -> SstEntry.make (intKey i) (uint64 i) (Some [| SqlValue.Int(int64 i) |])
        |]

    let mutable enumerations = 0

    let once =
        seq {
            enumerations <- enumerations + 1

            if enumerations > 1 then
                failwith "入力が 2回列挙されています"

            yield! items
        }

    use stream = new System.IO.MemoryStream()
    let info = ok (Sstable.writeTo stream 0.01 once)

    Assert.Equal(1, enumerations)
    Assert.Equal<int64>(5L, info.EntryCount)
    Assert.Equal<byte array>(intKey 1, info.FirstKey)
    Assert.Equal<byte array>(intKey 5, info.LastKey)
    Assert.Equal<uint64>(5UL, info.MaxSeqNo)
    // 書いたバイト数と Size が食い違わない
    Assert.Equal<int64>(int64 (stream.ToArray().Length), info.Size)

[<Fact>]
let ``SSTable: 構築と全件走査の往復`` () =
    let entries = sstEntries 1000
    let data = ok (buildSstable 0.01 entries)
    let reader = ok (Sstable.openPartial (writeTempSst data))
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

    let reader =
        ok (Sstable.openPartial (writeTempSst (ok (buildSstable 0.01 entries))))

    Assert.Equal(8, reader.Index.Length)

    // ブロック境界をまたぐ探索ができること
    Assert.Equal<int64>(int64 entries.Length, reader.EntryCount)

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

    let reader =
        ok (Sstable.openPartial (writeTempSst (ok (buildSstable 0.01 entries))))

    let all = ok (readEntries reader) |> Seq.toArray

    for i in 0 .. values.Length - 1 do
        let got = all.[i].Value.Value
        Assert.Equal(values.[i].Length, got.Length)

        for j in 0 .. values.[i].Length - 1 do
            Assert.Equal<string>(SqlLiteral.toSqlLiteral values.[i].[j], SqlLiteral.toSqlLiteral got.[j])

[<Fact>]
let ``SSTable: maxSeqNo が保存・復元される`` () =
    let entries =
        [|
            SstEntry.make (intKey 1) 7UL (Some [| SqlValue.Int 1L |])
            SstEntry.make (intKey 2) 99UL (Some [| SqlValue.Int 2L |])
            SstEntry.make (intKey 3) 42UL (Some [| SqlValue.Int 3L |])
        |]

    let bytes = ok (buildSstable 0.01 entries)
    let reader = ok (Sstable.openPartial (writeTempSst bytes))
    Assert.Equal<uint64>(99UL, reader.MaxSeqNo)
    Assert.Equal<int64>(3L, reader.EntryCount)

[<Fact>]
let ``SSTable: 空のテーブルは maxSeqNo 0`` () =
    let bytes = ok (buildSstable 0.01 [||])
    let reader = ok (Sstable.openPartial (writeTempSst bytes))
    Assert.Equal<uint64>(0UL, reader.MaxSeqNo)

[<Fact>]
let ``SSTable: 空のテーブル`` () =
    let data = ok (buildSstable 0.01 [||])
    let reader = ok (Sstable.openPartial (writeTempSst data))
    Assert.Equal<int64>(0L, reader.EntryCount)
    Assert.Equal(0, ok (readEntries reader) |> Seq.length)

[<Fact>]
let ``Sstable.openPartial: Meta が壊れていれば CorruptionError`` () =
    // 1個目の長さ付きキーは読めるが 2個目が途中で切れている meta 領域
    let body = [| 0x01uy; 0x61uy |]

    let footerBody =
        Writer.build
            [
                Writer.writeBytes (System.Text.Encoding.ASCII.GetBytes "RykDbSST")
                Writer.uint32LE 6u
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 0L
                Writer.int64LE 2L
                Writer.int64LE 0L
                Writer.uint32LE (Hashing.checksum [||])
            ]

    let footerChecksum = Hashing.checksum footerBody

    let footer =
        Writer.build [ Writer.writeBytes footerBody; Writer.uint32LE footerChecksum ]

    expectErrorKind
        ErrorKind.CorruptionError
        "Meta の破損は CorruptionError"
        (Sstable.openPartial (writeTempSst (Array.append body footer)))

[<Fact>]
let ``Sstable.openPartial: チェックサム不一致は読み出し時に CorruptionError`` () =
    let data = ok (buildSstable 0.01 (sstEntries 500))
    let corrupted = Array.copy data
    corrupted.[corrupted.Length / 2] <- corrupted.[corrupted.Length / 2] ^^^ 0xFFuy
    let partial = ok (Sstable.openPartial (writeTempSst corrupted))

    expectErrorKind
        ErrorKind.CorruptionError
        "チェックサム不一致はエラーになるはず"
        (Sstable.rangeScanPartial [||] 0 partial |> SstScan.toArray)

[<Fact>]
let ``Sstable.openPartial: マジック不一致は CorruptionError`` () =
    let data = ok (buildSstable 0.01 (sstEntries 500))
    let badMagic = Array.copy data
    let footerStart = badMagic.Length - Sstable.FooterSize
    badMagic.[footerStart] <- badMagic.[footerStart] ^^^ 0xFFuy
    fixFooterChecksum badMagic
    expectErrorKind ErrorKind.CorruptionError "マジック不一致はエラーになるはず" (Sstable.openPartial (writeTempSst badMagic))

[<Fact>]
let ``Sstable.openPartial: 未対応バージョンは CorruptionError`` () =
    let data = ok (buildSstable 0.01 (sstEntries 3))
    let bad = Array.copy data
    bad.[bad.Length - Sstable.FooterSize + 8] <- 7uy
    fixFooterChecksum bad
    expectErrorKind ErrorKind.CorruptionError "未対応バージョンはエラーになるはず" (Sstable.openPartial (writeTempSst bad))

[<Fact>]
let ``Sstable.openPartial: Footer の範囲が不正なら CorruptionError`` () =
    let baseBytes = ok (buildSstable 0.01 (sstEntries 3))
    let footerStart = baseBytes.Length - Sstable.FooterSize

    let writeI64 (bytes: byte array) (off: int) (v: int64) =
        for i in 0..7 do
            bytes.[off + i] <- byte (v >>> 8 * i &&& 0xFFL)

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
        expectErrorKind ErrorKind.CorruptionError name (Sstable.openPartial (writeTempSst bytes))

[<Fact>]
let ``Sstable.openPartial: Footer に満たないデータは CorruptionError`` () =
    expectErrorKind ErrorKind.CorruptionError "短すぎるデータはエラー" (Sstable.openPartial (writeTempSst (Array.create 10 0uy)))

[<Fact>]
let ``Sstable.openPartial: 全件・limit・lookup が全読みと一致する`` () =
    withDir "sst-partial" (fun dir ->
        let entries = sstEntries 300
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, ok (buildSstable 0.01 entries))

        let partial = ok (Sstable.openPartial path)
        let all = ok (Sstable.rangeScanPartial [||] 0 partial |> SstScan.toArray)
        Assert.Equal<int>(entries.Length, all.Length)
        Assert.Equal<int>(0, Bytes.compare entries.[0].Key all.[0].Key)
        Assert.Equal<int>(0, Bytes.compare entries.[entries.Length - 1].Key all.[all.Length - 1].Key)

        let limited = ok (Sstable.rangeScanPartial [||] 10 partial |> SstScan.toArray)
        Assert.Equal<int>(10, limited.Length)

        match ok (Sstable.lookup (intKey 150) None partial) with
        | Some e -> Assert.Equal<uint64>(151UL, e.SeqNo)
        | None -> Assert.Fail "lookup が行を見つけられません"

        // 可視性: SeqNo 2 ではキー 150（SeqNo 151）は見えない
        Assert.True((ok (Sstable.lookup (intKey 150) (Some 2UL) partial)).IsNone))

[<Fact>]
let ``Sstable.openPartial: Footer のチェックサム不一致は CorruptionError`` () =
    withDir "sst-footer-checksum" (fun dir ->
        let data = ok (buildSstable 0.01 (sstEntries 100))
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, data)

        // Footer の indexOff を壊す（Footer チェックサムは直さない）。
        let bad = Array.copy data
        let footerStart = bad.Length - Sstable.FooterSize
        bad.[footerStart + 20] <- bad.[footerStart + 20] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(path, bad)
        expectErrorKind ErrorKind.CorruptionError "部分読みでも Footer の破損を検出する" (Sstable.openPartial path)
        expectErrorKind ErrorKind.CorruptionError "全体読みでも Footer の破損を検出する" (Sstable.openPartial (writeTempSst bad)))

[<Fact>]
let ``Sstable.openPartial: Bloom メタデータの破損は checksum で検出`` () =
    withDir "sst-partial-meta-checksum" (fun dir ->
        let data = ok (buildSstable 0.01 (sstEntries 100))
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
let ``Sstable.openPartial: 壊れたブロックは読み出し時に CorruptionError`` () =
    withDir "sst-partial-corrupt" (fun dir ->
        let entries = sstEntries 300
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, ok (buildSstable 0.01 entries))

        // index を読んだ後にブロック 0 の内部を壊す
        let partial = ok (Sstable.openPartial path)
        let data = System.IO.File.ReadAllBytes path
        let block = partial.Index.[0]
        data.[block.Offset + 2] <- data.[block.Offset + 2] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(path, data)

        expectErrorKind
            ErrorKind.CorruptionError
            "部分読みでも壊れたブロックを検出する"
            (Sstable.rangeScanPartial [||] 0 partial |> SstScan.toArray))

[<Fact>]
let ``Sstable.openPartial: 索引の範囲外は読み出し時に CorruptionError`` () =
    withDir "sst-partial-index-range" (fun dir ->
        let path = System.IO.Path.Combine(dir, "1.sst")
        let bytes = Array.copy (ok (buildSstable 0.01 (sstEntries 300)))
        let footerStart = bytes.Length - Sstable.FooterSize

        let readI64 (off: int) =
            let mutable v = 0L

            for i in 0..7 do
                v <- v ||| (int64 bytes.[off + i] <<< (8 * i))

            v

        let writeI64 (off: int) (v: int64) =
            for i in 0..7 do
                bytes.[off + i] <- byte (v >>> (8 * i) &&& 0xFFL)

        let writeU32 (off: int) (v: uint32) =
            for i in 0..3 do
                bytes.[off + i] <- byte (v >>> (8 * i) &&& 0xFFu)

        let readVarint (pos: int) =
            let rec go shift acc p =
                let b = int bytes.[p]
                let acc = acc ||| ((b &&& 0x7F) <<< shift)

                if b &&& 0x80 = 0 then
                    acc, p + 1
                else
                    go (shift + 7) acc (p + 1)

            go 0 0 pos

        let indexOff = int (readI64 (footerStart + 20))
        let indexLen = int (readI64 (footerStart + 28))

        // 先頭エントリ（長さ付き LastKey + Offset + Length）を飛ばし、2 個目の Offset を範囲外へ書き換える。
        let keyLen0, afterLen0 = readVarint indexOff
        let secondEntry = afterLen0 + keyLen0 + 16
        let keyLen1, afterLen1 = readVarint secondEntry
        writeI64 (afterLen1 + keyLen1) 1_000_000_000L

        // 索引を書き換えたので metadataChecksum と footerChecksum を付け直す。
        let metaOff = int (readI64 (footerStart + 52))
        let metaLen = int (readI64 (footerStart + 60))
        let bloomOff = int (readI64 (footerStart + 36))
        let bloomLen = int (readI64 (footerStart + 44))

        let metadataChecksum =
            Hashing.checksumChunks
                [|
                    Array.sub bytes metaOff metaLen
                    Array.sub bytes bloomOff bloomLen
                    Array.sub bytes indexOff indexLen
                |]

        writeU32 (footerStart + 76) metadataChecksum
        fixFooterChecksum bytes
        System.IO.File.WriteAllBytes(path, bytes)

        let partial = ok (Sstable.openPartial path)

        expectErrorKind
            ErrorKind.CorruptionError
            "索引の範囲外でも例外にせず CorruptionError を返す"
            (Sstable.rangeScanPartial [||] 0 partial |> SstScan.toArray))
