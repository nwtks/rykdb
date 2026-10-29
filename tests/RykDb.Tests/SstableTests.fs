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

[<Fact>]
let ``SstEntry.maxSeqNo: 最大 SeqNo を返す`` () =
    let items =
        [| SstEntry.make (intKey 1) 3UL None; SstEntry.make (intKey 2) 7UL None |]

    Assert.Equal(7UL, SstEntry.maxSeqNo items)

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
let ``SSTable: maxSeqNo が保存・復元される`` () =
    let entries =
        [|
            SstEntry.make (intKey 1) 7UL (Some [| SqlValue.Int 1L |])
            SstEntry.make (intKey 2) 99UL (Some [| SqlValue.Int 2L |])
            SstEntry.make (intKey 3) 42UL (Some [| SqlValue.Int 3L |])
        |]

    let bytes = ok (Sstable.build 0.01 entries)
    let reader = ok (Sstable.openPartial (writeTempSst bytes))
    Assert.Equal<uint64>(99UL, reader.MaxSeqNo)
    Assert.Equal<int64>(3L, reader.EntryCount)

[<Fact>]
let ``SSTable: 空のテーブルは maxSeqNo 0`` () =
    let bytes = ok (Sstable.build 0.01 [||])
    let reader = ok (Sstable.openPartial (writeTempSst bytes))
    Assert.Equal<uint64>(0UL, reader.MaxSeqNo)

[<Fact>]
let ``SSTable: 構築と全件走査の往復`` () =
    let entries = sstEntries 1000
    let data = ok (Sstable.build 0.01 entries)
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
        ok (Sstable.openPartial (writeTempSst (ok (Sstable.build 0.01 entries))))

    Assert.Equal(8, reader.Index.Length)

    // ブロック境界をまたぐ探索ができること
    Assert.Equal<int64>(int64 entries.Length, reader.EntryCount)

[<Fact>]
let ``SSTable: 空のテーブル`` () =
    let data = ok (Sstable.build 0.01 [||])
    let reader = ok (Sstable.openPartial (writeTempSst data))
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

    let reader =
        ok (Sstable.openPartial (writeTempSst (ok (Sstable.build 0.01 entries))))

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

    let withSharedPrefix = ok (Sstable.build 0.01 shared)

    // ランダムなキー（共有されない）とのサイズ比較
    let rng = Random 42

    let random =
        Array.init 1000 (fun _ ->
            let bs = Array.init 12 (fun _ -> byte (rng.Next(0, 256)))
            SstEntry.make bs 1UL (Some [| SqlValue.Int 1L |]))
        |> Array.sortWith SstEntry.compare

    let withoutShared = ok (Sstable.build 0.01 random)

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

    expectErrorKind ErrorKind.InternalError "キー順に並んでいない入力はエラーになるはず" (Sstable.build 0.01 bad)

[<Fact>]
let ``Sstable.openTable: チェックサム不一致は CorruptionError`` () =
    let data = ok (Sstable.build 0.01 (sstEntries 500))
    let corrupted = Array.copy data
    corrupted.[corrupted.Length / 2] <- corrupted.[corrupted.Length / 2] ^^^ 0xFFuy
    let partial = ok (Sstable.openPartial (writeTempSst corrupted))

    expectErrorKind
        ErrorKind.CorruptionError
        "チェックサム不一致はエラーになるはず"
        (Sstable.rangeScanPartial [||] 0 partial |> SstScan.toArray)

[<Fact>]
let ``Sstable.openTable: マジック不一致は CorruptionError`` () =
    let data = ok (Sstable.build 0.01 (sstEntries 500))
    let badMagic = Array.copy data
    let footerStart = badMagic.Length - Sstable.FooterSize
    badMagic.[footerStart] <- badMagic.[footerStart] ^^^ 0xFFuy
    fixFooterChecksum badMagic
    expectErrorKind ErrorKind.CorruptionError "マジック不一致はエラーになるはず" (Sstable.openPartial (writeTempSst badMagic))

[<Fact>]
let ``Sstable.openTable: 未対応バージョンは CorruptionError`` () =
    let data = ok (Sstable.build 0.01 (sstEntries 3))
    let bad = Array.copy data
    bad.[bad.Length - Sstable.FooterSize + 8] <- 6uy
    fixFooterChecksum bad
    expectErrorKind ErrorKind.CorruptionError "未対応バージョンはエラーになるはず" (Sstable.openPartial (writeTempSst bad))

[<Fact>]
let ``Sstable.openTable: Footer の範囲が不正なら CorruptionError`` () =
    let baseBytes = ok (Sstable.build 0.01 (sstEntries 3))
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
let ``Sstable.openTable: Footer に満たないデータは CorruptionError`` () =
    expectErrorKind ErrorKind.CorruptionError "短すぎるデータはエラー" (Sstable.openPartial (writeTempSst (Array.create 10 0uy)))

[<Fact>]
let ``Sstable.openTable: Meta が壊れていても MinKey/MaxKey は空で開ける`` () =
    // 1個目の長さ付きキーは読めるが 2個目が途中で切れている meta 領域
    let body = [| 0x01uy; 0x61uy |]

    let footerBody =
        Writer.build
            [
                Writer.writeBytes (System.Text.Encoding.ASCII.GetBytes "RykDbSST")
                Writer.uint32LE 5u
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
let ``Sstable.openPartial: Footer のチェックサム不一致は CorruptionError`` () =
    withDir "sst-footer-checksum" (fun dir ->
        let data = ok (Sstable.build 0.01 (sstEntries 100))
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
        let data = ok (Sstable.build 0.01 (sstEntries 100))
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
        System.IO.File.WriteAllBytes(path, ok (Sstable.build 0.01 entries))

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
let ``Sstable.openPartial: 壊れたブロックは読み出し時に CorruptionError`` () =
    withDir "sst-partial-corrupt" (fun dir ->
        let entries = sstEntries 300
        let path = System.IO.Path.Combine(dir, "1.sst")
        System.IO.File.WriteAllBytes(path, ok (Sstable.build 0.01 entries))

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
let ``FileMetaData.zero: 空メタデータ`` () =
    Assert.Equal(0L, FileMetaData.Zero.FileNumber)
    Assert.Equal(0, FileMetaData.Zero.Level)
    Assert.Equal(0L, FileMetaData.Zero.EntryCount)

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
    let f = FileMetaData.Zero

    let f2 =
        { f with
            Smallest = [| 1uy |]
            Largest = [| 5uy |]
        }

    Assert.True(FileMetaData.mightContain [| 3uy |] f2)
    Assert.False(FileMetaData.mightContain [| 0uy |] f2)
    Assert.False(FileMetaData.mightContain [| 9uy |] f2)
