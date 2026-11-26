// ===========================================================================
// SstableReader.fs のテスト（部分読み・走査・ポイント検索）
// ===========================================================================

module RykDb.SstableReaderTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// バイト列を一時ファイルに書き、部分読みリーダーを開く。
let private writeTempSst (data: byte array) =
    let path =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid().ToString "N" + ".sst")

    System.IO.File.WriteAllBytes(path, data)
    path

/// Footer 末尾の 4バイト（先頭 FooterSize-4 バイトのチェックサム）を付け直す。
let private fixFooterChecksum (bytes: byte array) =
    let footerStart = bytes.Length - SstableFooter.FooterSize

    let checksum =
        Hashing.checksum (Array.sub bytes footerStart (SstableFooter.FooterSize - 4))

    for i in 0..3 do
        bytes.[footerStart + SstableFooter.FooterSize - 4 + i] <- byte (checksum >>> 8 * i &&& 0xFFu)

/// Footer の int64 フィールドを読む。
let private readI64At (bytes: byte array) (off: int) =
    let mutable v = 0L

    for i in 0..7 do
        v <- v ||| (int64 bytes.[off + i] <<< 8 * i)

    v

/// Footer の int64 フィールドを書く。
let private writeI64At (bytes: byte array) (off: int) (v: int64) =
    for i in 0..7 do
        bytes.[off + i] <- byte (v >>> 8 * i &&& 0xFFL)

/// varint を読んで (値, 次の位置) を返す。
let private readVarintAtBytes (bytes: byte array) (pos: int) =
    let rec go shift acc p =
        let b = int bytes.[p]
        let acc = acc ||| (b &&& 0x7F <<< shift)

        if b &&& 0x80 = 0 then
            acc, p + 1
        else
            go (shift + 7) acc (p + 1)

    go 0 0 pos

/// 索引を書き換えたときに呼ぶ: metadataChecksum と Footer のチェックサムを付け直す。
let private refreshMetadataChecksums (bytes: byte array) =
    let footerStart = bytes.Length - SstableFooter.FooterSize
    let indexOff = int (readI64At bytes (footerStart + 20))
    let indexLen = int (readI64At bytes (footerStart + 28))
    let metaOff = int (readI64At bytes (footerStart + 52))
    let metaLen = int (readI64At bytes (footerStart + 60))
    let bloomOff = int (readI64At bytes (footerStart + 36))
    let bloomLen = int (readI64At bytes (footerStart + 44))

    let metadataChecksum =
        Hashing.checksumChunks
            [|
                Array.sub bytes metaOff metaLen
                Array.sub bytes bloomOff bloomLen
                Array.sub bytes indexOff indexLen
            |]

    for i in 0..3 do
        bytes.[footerStart + 76 + i] <- byte (metadataChecksum >>> 8 * i &&& 0xFFu)

    fixFooterChecksum bytes

/// 先頭から全エントリを配列で返す（全件必要ならこちら）。
let private readEntries (partial: SstReadInfo) =
    SstableReader.rangeScanPartial [||] 0 partial |> SstScan.toArray

[<Fact>]
let ``SstableReader: 構築と全件走査の往復`` () =
    let entries = sstEntries 1000
    let data = ok (buildSstable 0.01 entries)
    let reader = ok (SstableReader.openPartial (writeTempSst data))
    Assert.Equal<int64>(int64 entries.Length, reader.EntryCount)

    let all = ok (readEntries reader) |> Seq.toArray
    Assert.Equal(entries.Length, all.Length)

    for i in 0 .. entries.Length - 1 do
        Assert.Equal(entries.[i].SeqNo, all.[i].SeqNo)

        Assert.Equal<string>(
            (expectSome "値" entries.[i].Value).[0] |> SqlLiteral.toSqlLiteral,
            (expectSome "値" all.[i].Value).[0] |> SqlLiteral.toSqlLiteral
        )

[<Fact>]
let ``SstableReader: raw スキャンは復号なしで同じ値・削除タグを返す`` () =
    let entries =
        Array.append (sstEntries 300) [| SstEntry.make (intKey 10_000) 1UL None |]

    let reader =
        ok (SstableReader.openPartial (writeTempSst (ok (buildSstable 0.01 entries))))

    let decoded = ok (readEntries reader)
    let raw = ok (SstableReader.rangeScanPartialRaw [||] 0 reader |> SstScan.toArray)
    Assert.Equal(decoded.Length, raw.Length)

    for i in 0 .. decoded.Length - 1 do
        let d = decoded.[i]
        let r = raw.[i]
        Assert.Equal<byte array>(d.Key, r.Key)
        Assert.Equal<uint64>(d.SeqNo, r.SeqNo)
        Assert.Equal<bool>(d.IsDelete, r.IsDelete)
        Assert.True r.RawValue.IsSome
        Assert.Equal<SqlValue array option>(d.Value, ok (SstEntry.decoded r))

[<Fact>]
let ``SstableReader: 列マスクで必要な列だけ復号する`` () =
    let entries =
        [|
            SstEntry.make (intKey 1) 1UL (Some [| SqlValue.Int 10L; SqlValue.Text "skip-me"; SqlValue.Int 30L |])
        |]

    let reader =
        ok (SstableReader.openPartial (writeTempSst (ok (buildSstable 0.01 entries))))

    let masked =
        SstableReader.rangeScanPartialMasked [| true; false; true |] [||] 0 reader
        |> SstScan.toArray
        |> ok

    let values = expectSome "値" masked.[0].Value
    Assert.Equal<SqlValue>(SqlValue.Int 10L, values.[0])
    Assert.Equal<SqlValue>(SqlValue.Unloaded, values.[1])
    Assert.Equal<SqlValue>(SqlValue.Int 30L, values.[2])

[<Fact>]
let ``SstableReader: lookupMasked は必要な列だけ復号する`` () =
    let entries =
        [|
            SstEntry.make (intKey 1) 1UL (Some [| SqlValue.Int 10L; SqlValue.Text "skip-me"; SqlValue.Int 30L |])
        |]

    let reader =
        ok (SstableReader.openPartial (writeTempSst (ok (buildSstable 0.01 entries))))

    let found =
        ok (SstableReader.lookupMasked (intKey 1) [| true; false; true |] None reader)

    Assert.True found.IsSome
    let values = (expectSome "SstEntry" found).Value |> expectSome "値"
    Assert.Equal<SqlValue>(SqlValue.Int 10L, values.[0])
    Assert.Equal<SqlValue>(SqlValue.Unloaded, values.[1])
    Assert.Equal<SqlValue>(SqlValue.Int 30L, values.[2])

[<Fact>]
let ``SstableReader: lookupMasked は値長が範囲外のブロックを CorruptionError にする`` () =
    withDir "sst-value-len" (fun dir ->
        let path = System.IO.Path.Combine(dir, "crafted.sst")
        let entryKey = intKey 1
        let lookupKey = intKey 2

        // valueLen を Int32.MaxValue 超にして、符号変換で位置が負になる入口を作る。
        let body =
            Writer.build
                [
                    Writer.varint 0UL
                    Writer.varint (uint64 entryKey.Length)
                    Writer.varint 0x80000000UL
                    Writer.writeBytes entryKey
                ]

        let block =
            Array.append body (Writer.build [ Writer.uint32LE (Hashing.checksumRange body 0 body.Length) ])

        System.IO.File.WriteAllBytes(path, block)

        let info: SstReadInfo =
            {
                Path = path
                Index =
                    [|
                        {
                            LastKey = lookupKey
                            Offset = 0
                            Length = block.Length
                        }
                    |]
                MinKey = entryKey
                MaxKey = lookupKey
                Bloom = None
                EntryCount = 1L
                MaxSeqNo = 1UL
            }

        expectErrorKind
            ErrorKind.CorruptionError
            "値長が範囲外のブロックは CorruptionError"
            (SstableReader.lookupMasked lookupKey [||] None info))

[<Fact>]
let ``SstableReader: 複数ブロックに分割される`` () =
    // EntriesPerBlock = 128 なので 1000 件で 8 ブロック
    let entries = sstEntries 1000

    let reader =
        ok (SstableReader.openPartial (writeTempSst (ok (buildSstable 0.01 entries))))

    Assert.Equal(8, reader.Index.Length)
    // ブロック境界をまたぐ探索ができること
    Assert.Equal<int64>(int64 entries.Length, reader.EntryCount)

[<Fact>]
let ``SstableReader: 全 10型の値を往復できる`` () =
    let values =
        [|
            [| SqlValue.Null |]
            [| SqlValue.Bool true; SqlValue.Bool false |]
            [|
                SqlValue.Int System.Int64.MinValue
                SqlValue.Int 0L
                SqlValue.Int System.Int64.MaxValue
            |]
            [|
                SqlValue.Real -1.5
                SqlValue.Real 0.0
                SqlValue.Real System.Double.PositiveInfinity
            |]
            [|
                SqlValue.Decimal -12345.6789M
                SqlValue.Decimal 0M
                SqlValue.Decimal System.Decimal.MaxValue
            |]
            [| SqlValue.Text ""; SqlValue.Text "hello"; SqlValue.Text "中文" |]
            [| SqlValue.Blob [||]; SqlValue.Blob [| 1uy; 2uy; 3uy |] |]
            [|
                SqlValue.Date System.DateOnly.MinValue
                SqlValue.Date System.DateOnly.MaxValue
            |]
            [|
                SqlValue.Timestamp(System.DateTimeOffset(System.DateTime(0L, System.DateTimeKind.Utc)))
            |]
            [| SqlValue.Uuid(System.Guid.Parse "01234567-89ab-cdef-0123-456789abcdef") |]
        |]

    let entries =
        values
        |> Array.mapi (fun i vs -> SstEntry.make (KeyBuilder.ofValues [ SqlValue.Int(int64 i) ] |> ok) 1UL (Some vs))
        |> Array.sortWith SstEntry.compare

    let reader =
        ok (SstableReader.openPartial (writeTempSst (ok (buildSstable 0.01 entries))))

    let all = ok (readEntries reader) |> Seq.toArray

    for i in 0 .. values.Length - 1 do
        let got = expectSome "値" all.[i].Value
        Assert.Equal(values.[i].Length, got.Length)

        for j in 0 .. values.[i].Length - 1 do
            Assert.Equal<string>(SqlLiteral.toSqlLiteral values.[i].[j], SqlLiteral.toSqlLiteral got.[j])

[<Fact>]
let ``SstableReader: maxSeqNo が保存・復元される`` () =
    let entries =
        [|
            SstEntry.make (intKey 1) 7UL (Some [| SqlValue.Int 1L |])
            SstEntry.make (intKey 2) 99UL (Some [| SqlValue.Int 2L |])
            SstEntry.make (intKey 3) 42UL (Some [| SqlValue.Int 3L |])
        |]

    let bytes = ok (buildSstable 0.01 entries)
    let reader = ok (SstableReader.openPartial (writeTempSst bytes))
    Assert.Equal<uint64>(99UL, reader.MaxSeqNo)
    Assert.Equal<int64>(3L, reader.EntryCount)

[<Fact>]
let ``SstableReader: 空のテーブルは maxSeqNo 0`` () =
    let bytes = ok (buildSstable 0.01 [||])
    let reader = ok (SstableReader.openPartial (writeTempSst bytes))
    Assert.Equal<uint64>(0UL, reader.MaxSeqNo)

[<Fact>]
let ``SstableReader: 空のテーブル`` () =
    let data = ok (buildSstable 0.01 [||])
    let reader = ok (SstableReader.openPartial (writeTempSst data))
    Assert.Equal<int64>(0L, reader.EntryCount)
    Assert.Equal(0, ok (readEntries reader) |> Seq.length)

[<Fact>]
let ``SstableReader.openPartial: Meta が壊れていれば CorruptionError`` () =
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
        (SstableReader.openPartial (writeTempSst (Array.append body footer)))

[<Fact>]
let ``SstableReader.openPartial: チェックサム不一致は読み出し時に CorruptionError`` () =
    let data = ok (buildSstable 0.01 (sstEntries 500))
    let corrupted = Array.copy data
    corrupted.[corrupted.Length / 2] <- corrupted.[corrupted.Length / 2] ^^^ 0xFFuy
    let partial = ok (SstableReader.openPartial (writeTempSst corrupted))

    expectErrorKind
        ErrorKind.CorruptionError
        "チェックサム不一致はエラーになるはず"
        (SstableReader.rangeScanPartial [||] 0 partial |> SstScan.toArray)

[<Fact>]
let ``SstableReader.openPartial: マジック不一致は CorruptionError`` () =
    let data = ok (buildSstable 0.01 (sstEntries 500))
    let badMagic = Array.copy data
    let footerStart = badMagic.Length - SstableFooter.FooterSize
    badMagic.[footerStart] <- badMagic.[footerStart] ^^^ 0xFFuy
    fixFooterChecksum badMagic
    expectErrorKind ErrorKind.CorruptionError "マジック不一致はエラーになるはず" (SstableReader.openPartial (writeTempSst badMagic))

[<Fact>]
let ``SstableReader.openPartial: 未対応バージョンは CorruptionError`` () =
    let data = ok (buildSstable 0.01 (sstEntries 3))
    let bad = Array.copy data
    bad.[bad.Length - SstableFooter.FooterSize + 8] <- 7uy
    fixFooterChecksum bad
    expectErrorKind ErrorKind.CorruptionError "未対応バージョンはエラーになるはず" (SstableReader.openPartial (writeTempSst bad))

[<Fact>]
let ``SstableReader.openPartial: Footer の範囲が不正なら CorruptionError`` () =
    let baseBytes = ok (buildSstable 0.01 (sstEntries 3))
    let footerStart = baseBytes.Length - SstableFooter.FooterSize

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
        expectErrorKind ErrorKind.CorruptionError name (SstableReader.openPartial (writeTempSst bytes))

[<Fact>]
let ``SstableReader.openPartial: Footer に満たないデータは CorruptionError`` () =
    expectErrorKind
        ErrorKind.CorruptionError
        "短すぎるデータはエラー"
        (SstableReader.openPartial (writeTempSst (Array.create 10 0uy)))

[<Fact>]
let ``SstableReader.openPartial: 全件・limit・lookup が全読みと一致する`` () =
    withDir "sst-partial" (fun dir ->
        let entries = sstEntries 300
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, ok (buildSstable 0.01 entries))

        let partial = ok (SstableReader.openPartial path)
        let all = ok (SstableReader.rangeScanPartial [||] 0 partial |> SstScan.toArray)
        Assert.Equal<int>(entries.Length, all.Length)
        Assert.Equal<int>(0, Bytes.compare entries.[0].Key all.[0].Key)
        Assert.Equal<int>(0, Bytes.compare entries.[entries.Length - 1].Key all.[all.Length - 1].Key)

        let limited = ok (SstableReader.rangeScanPartial [||] 10 partial |> SstScan.toArray)
        Assert.Equal<int>(10, limited.Length)

        match ok (SstableReader.lookupMasked (intKey 150) [||] None partial) with
        | Some e -> Assert.Equal<uint64>(151UL, e.SeqNo)
        | None -> Assert.Fail "lookup が行を見つけられません"

        // 可視性: SeqNo 2 ではキー 150（SeqNo 151）は見えない
        Assert.True((ok (SstableReader.lookupMasked (intKey 150) [||] (Some 2UL) partial)).IsNone))

[<Fact>]
let ``SstableReader.openPartial: Footer のチェックサム不一致は CorruptionError`` () =
    withDir "sst-footer-checksum" (fun dir ->
        let data = ok (buildSstable 0.01 (sstEntries 100))
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, data)

        // Footer の indexOff を壊す（Footer チェックサムは直さない）。
        let bad = Array.copy data
        let footerStart = bad.Length - SstableFooter.FooterSize
        bad.[footerStart + 20] <- bad.[footerStart + 20] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(path, bad)
        expectErrorKind ErrorKind.CorruptionError "部分読みでも Footer の破損を検出する" (SstableReader.openPartial path)

        expectErrorKind
            ErrorKind.CorruptionError
            "全体読みでも Footer の破損を検出する"
            (SstableReader.openPartial (writeTempSst bad)))

[<Fact>]
let ``SstableReader.openPartial: Bloom メタデータの破損は checksum で検出`` () =
    withDir "sst-partial-meta-checksum" (fun dir ->
        let data = ok (buildSstable 0.01 (sstEntries 100))
        let path = System.IO.Path.Combine(dir, "1.sst")
        let footerStart = data.Length - SstableFooter.FooterSize

        let bloomOffset =
            [ 0..7 ]
            |> List.fold (fun acc i -> acc ||| (int64 data.[footerStart + 36 + i] <<< i * 8)) 0L
            |> int

        data.[bloomOffset] <- data.[bloomOffset] ^^^ 0x01uy
        System.IO.File.WriteAllBytes(path, data)
        expectErrorKind ErrorKind.CorruptionError "部分読みで Bloom の改変を見逃さない" (SstableReader.openPartial path))

[<Fact>]
let ``SstableReader.openPartial: 壊れたブロックは読み出し時に CorruptionError`` () =
    withDir "sst-partial-corrupt" (fun dir ->
        let entries = sstEntries 300
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, ok (buildSstable 0.01 entries))

        // index を読んだ後にブロック 0 の内部を壊す
        let partial = ok (SstableReader.openPartial path)
        let data = System.IO.File.ReadAllBytes path
        let block = partial.Index.[0]
        data.[block.Offset + 2] <- data.[block.Offset + 2] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(path, data)

        expectErrorKind
            ErrorKind.CorruptionError
            "部分読みでも壊れたブロックを検出する"
            (SstableReader.rangeScanPartial [||] 0 partial |> SstScan.toArray))

[<Fact>]
let ``SstableReader.openPartial: 索引の範囲がファイル外なら CorruptionError`` () =
    withDir "sst-partial-index-range" (fun dir ->
        let path = System.IO.Path.Combine(dir, "1.sst")
        let bytes = Array.copy (ok (buildSstable 0.01 (sstEntries 300)))
        let footerStart = bytes.Length - SstableFooter.FooterSize
        let indexOff = int (readI64At bytes (footerStart + 20))

        // 先頭エントリ（長さ付き LastKey + Offset + Length）を飛ばし、2 個目の Offset を範囲外へ書き換える。
        let keyLen0, afterLen0 = readVarintAtBytes bytes indexOff
        let secondEntry = afterLen0 + keyLen0 + 16
        let keyLen1, afterLen1 = readVarintAtBytes bytes secondEntry
        writeI64At bytes (afterLen1 + keyLen1) 1_000_000_000L
        // 索引を書き換えたので metadataChecksum と footerChecksum を付け直す。
        refreshMetadataChecksums bytes
        System.IO.File.WriteAllBytes(path, bytes)
        // チェックサムが正しくても、範囲は open 時にファイルサイズで上界を検査する
        // （読み出し時の巨大なブロック割当・範囲外読みに到達させない）。
        expectErrorKind ErrorKind.CorruptionError "索引の範囲外は open で検出する" (SstableReader.openPartial path))

[<Fact>]
let ``SstableReader.openPartial: 索引の LastKey が昇順でなければ CorruptionError`` () =
    withDir "sst-partial-index-order" (fun dir ->
        let path = System.IO.Path.Combine(dir, "1.sst")
        let bytes = Array.copy (ok (buildSstable 0.01 (sstEntries 300)))
        let footerStart = bytes.Length - SstableFooter.FooterSize
        let indexOff = int (readI64At bytes (footerStart + 20))

        // 先頭エントリの LastKey の先頭バイト（型タグ 0x20）を 0x7F にして、
        // 2 個目以降の LastKey より大きいキーにする（二分探索の前提を壊す）。
        let keyLen0, keyStart0 = readVarintAtBytes bytes indexOff
        Assert.True(keyLen0 > 0)
        bytes.[keyStart0] <- 0x7Fuy
        refreshMetadataChecksums bytes
        System.IO.File.WriteAllBytes(path, bytes)
        expectErrorKind ErrorKind.CorruptionError "LastKey が降順の索引は open で検出する" (SstableReader.openPartial path))
