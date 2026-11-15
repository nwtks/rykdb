// ===========================================================================
// Serialization.fs（順序保持符号化・値コードック）
// ===========================================================================

module RykDb.SerializationTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Reader: 負のバイト数は CorruptionError`` () =
    expectErrorKind ErrorKind.CorruptionError "負のバイト数はエラー" (Reader.readBytes -1 (Cursor.create [| 1uy |]))

[<Fact>]
let ``Reader: 10 バイトを超える varint は CorruptionError`` () =
    expectErrorKind
        ErrorKind.CorruptionError
        "長すぎる varint はエラー"
        (Reader.readVarint (Cursor.create (Array.create 10 0x80uy)))

[<Fact>]
let ``Reader: Int32.MaxValue を超える長さ指定は CorruptionError`` () =
    let bytes = Writer.build [ Writer.varint (uint64 Int32.MaxValue + 1UL) ]
    expectErrorKind ErrorKind.CorruptionError "長すぎる長さ指定はエラー" (Reader.readLengthPrefixed (Cursor.create bytes))

[<Fact>]
let ``Reader: readItems は n 件読む`` () =
    let w =
        Writer.Empty
        |> Writer.writeByte 1uy
        |> Writer.writeByte 2uy
        |> Writer.writeByte 3uy
        |> Writer.toArray

    let c = Cursor.create w

    let items, c2 =
        ok (Reader.readItems (fun c -> Reader.readByte c |> Result.map (fun (b, c) -> int b, c)) 3UL c)

    Assert.Equal<int list>([ 1; 2; 3 ], items)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Reader: readItems は readOne のエラーを伝播する`` () =
    expectErrorKind
        ErrorKind.CorruptionError
        "要素の読み取りエラーは伝播する"
        (Reader.readItems Reader.readByte 1UL (Cursor.create [||]))

[<Fact>]
let ``順序保持: 単一フィールドのバイト順が SQL 比較順と一致する`` () =
    let values = Array.init 5000 (fun _ -> randomValue ())
    let keys = values |> Array.map keyOf

    let byValue = values |> Array.sortWith compareForSort

    let byKey =
        [| 0 .. values.Length - 1 |]
        |> Array.sortWith (fun i j -> compareKeyBytes keys.[i] keys.[j])
        |> Array.map (fun i -> values.[i])

    Assert.Equal<string array>(render byValue, render byKey)

[<Fact>]
let ``順序保持: 複合キーのタプル順が SQL のタプル比較順と一致する`` () =
    let tuples =
        Array.init 3000 (fun _ -> Array.init (nextInt 1 4) (fun _ -> randomValue ()))

    let keys = tuples |> Array.map keyOfTuple

    let byValue =
        tuples
        |> Array.sortWith (fun a b ->
            let n = min a.Length b.Length
            let mutable i = 0
            let mutable r = 0

            while r = 0 && i < n do
                r <- compareForSort a.[i] b.[i]
                i <- i + 1

            if r <> 0 then r else compare a.Length b.Length)

    let byKey =
        [| 0 .. tuples.Length - 1 |]
        |> Array.sortWith (fun i j -> compareKeyBytes keys.[i] keys.[j])
        |> Array.map (fun i -> tuples.[i])

    Assert.Equal<SqlValue array array>(byValue, byKey)

[<Fact>]
let ``順序保持: 各型の境界値`` () =
    // NUL を含む文字列は F# の文字列リテラルで書かず、char 0 から組み立てる。
    let nulA = SqlValue.Text("a" + string (char 0))

    let cases: (string * SqlValue list) list =
        [
            "INTEGER",
            [
                SqlValue.Int Int64.MinValue
                SqlValue.Int -5000L
                SqlValue.Int -1L
                SqlValue.Int 0L
                SqlValue.Int 1L
                SqlValue.Int Int64.MaxValue
            ]
            "REAL",
            [
                SqlValue.Real Double.NegativeInfinity
                SqlValue.Real -1.5
                SqlValue.Real 0.0
                SqlValue.Real 1.5
                SqlValue.Real Double.PositiveInfinity
            ]
            "REAL_NaN", [ SqlValue.Real 1.5; SqlValue.Real Double.NaN ]
            "DECIMAL",
            [
                SqlValue.Decimal Decimal.MinValue
                SqlValue.Decimal -12345.6789M
                SqlValue.Decimal -0.000001M
                SqlValue.Decimal 0M
                SqlValue.Decimal 0.000001M
                SqlValue.Decimal 12345.6789M
                SqlValue.Decimal Decimal.MaxValue
            ]
            "BOOLEAN", [ SqlValue.Bool false; SqlValue.Bool true ]
            "DATE",
            [
                SqlValue.Date DateOnly.MinValue
                SqlValue.Date(DateOnly.FromDayNumber 1)
                SqlValue.Date(DateOnly.FromDayNumber 100000)
                SqlValue.Date DateOnly.MaxValue
            ]
            "TIMESTAMP",
            [
                SqlValue.Timestamp(DateTimeOffset(DateTime(0L, DateTimeKind.Utc)))
                SqlValue.Timestamp(DateTimeOffset(DateTime(1_000_000_000L, DateTimeKind.Utc)))
                SqlValue.Timestamp(DateTimeOffset(DateTime.MaxValue.Ticks, TimeSpan.Zero))
            ]
            "TEXT",
            [
                SqlValue.Text ""
                SqlValue.Text "a"
                nulA
                SqlValue.Text "ab"
                SqlValue.Text "b"
                SqlValue.Text "ÿ"
                SqlValue.Text "中"
            ]
            "BLOB",
            [
                SqlValue.Blob [||]
                SqlValue.Blob [| 0uy |]
                SqlValue.Blob [| 1uy |]
                SqlValue.Blob [| 0uy; 0uy |]
                SqlValue.Blob [| 255uy |]
            ]
            "UUID",
            [
                SqlValue.Uuid(Guid.Parse "00000000-0000-0000-0000-000000000000")
                SqlValue.Uuid(Guid.Parse "00000000-0000-0000-0000-000000000001")
                SqlValue.Uuid(Guid.Parse "7fffffff-ffff-ffff-ffff-ffffffffffff")
                SqlValue.Uuid(Guid.Parse "ffffffff-ffff-ffff-ffff-ffffffffffff")
            ]
        ]

    for _, values in cases do
        let arr = Array.ofList values
        let keys = arr |> Array.map keyOf
        let byValue = arr |> Array.sortWith compareForSort

        let byKey =
            [| 0 .. arr.Length - 1 |]
            |> Array.sortWith (fun i j -> compareKeyBytes keys.[i] keys.[j])
            |> Array.map (fun i -> arr.[i])

        Assert.Equal<string array>(render byValue, render byKey)

[<Fact>]
let ``順序保持: escape により接頭辞一致が意味論と一致する`` () =
    // "a" は "a\0" の接頭辞であり、SQL でも "a" < "a\0"。
    let a = SqlValue.Text "a"
    let aNul = SqlValue.Text("a" + string (char 0))

    // 「1 フィールドだけのキー」を接頭辞に使う（2 フィールド目があると
    // escape された 0x00 の次が 0xFF になり、接頭辞が一致しないのが正しい挙動）。
    let pA = keyOfTuple [| a |]
    let kANul = keyOfTuple [| aNul |]

    Assert.True(KeyBuilder.startsWithPrefix kANul pA)
    Assert.False(KeyBuilder.startsWithPrefix pA kANul)
    Assert.True(compareKeyBytes pA kANul < 0)

    // 接頭辞一致 == SQL の前方一致（同じ比較順序に由来するので一致するはず）
    let values =
        [|
            SqlValue.Text "a"
            aNul
            SqlValue.Text "ab"
            SqlValue.Text "b"
            SqlValue.Text ""
        |]

    let keys = values |> Array.map (fun v -> keyOfTuple [| v |])
    let matched = keys |> Array.filter (fun k -> KeyBuilder.startsWithPrefix k pA)

    let expected = values |> Array.filter (fun v -> v = a || v = aNul)

    Assert.Equal<int>(expected.Length, matched.Length)

[<Fact>]
let ``Hashing.checksum: CRC32C の既知解`` () =
    // CRC-32C（Castagnoli）の標準チェック値
    Assert.Equal(0xE3069283u, Hashing.checksum (System.Text.Encoding.ASCII.GetBytes "123456789"))
    Assert.Equal(0u, Hashing.checksum [||])

[<Fact>]
let ``Hashing.checksum: 1 ビットの破損で値が変わる`` () =
    let data = Array.init 256 (fun i -> byte i)

    for pos in [ 0; 7; 8; 63; 127; 255 ] do
        let corrupted = Array.copy data
        corrupted.[pos] <- corrupted.[pos] ^^^ 0x01uy
        Assert.NotEqual<uint32>(Hashing.checksum data, Hashing.checksum corrupted)

[<Fact>]
let ``Hashing.checksumRange: 全長の checksum と一致する`` () =
    let data = Array.init 1000 (fun i -> byte (i * 31))

    for offset in [ 0; 1; 3; 7; 8; 9; 500 ] do
        for length in [ 0; 1; 7; 8; 9; 64; 1000 - offset ] do
            Assert.Equal(Hashing.checksum (Array.sub data offset length), Hashing.checksumRange data offset length)

[<Fact>]
let ``Hashing.checksumChunks: 連結した入力と同じ値になる`` () =
    let data = Array.init 100 (fun i -> byte (i * 17))
    let chunks = [| Array.sub data 0 3; Array.sub data 3 5; Array.sub data 8 92 |]
    Assert.Equal(Hashing.checksum data, Hashing.checksumChunks chunks)
    Assert.Equal(Hashing.checksum [||], Hashing.checksumChunks [||])
    Assert.Equal(Hashing.checksum [||], Hashing.checksumChunks [| [||]; [||] |])

[<Fact>]
let ``KeyBuilder.ofValues: 空列は空キー`` () =
    let k = KeyBuilder.ofValues [] |> ok
    Assert.Equal(0, k.Length)

[<Fact>]
let ``KeyBuilder.ofValues: 値を符号化する`` () =
    let k = KeyBuilder.ofValues [ SqlValue.Int 1L ] |> ok
    Assert.True(k.Length > 0)

[<Fact>]
let ``KeyBuilder.ofValues: REAL の -0.0 は +0.0 と同じキー`` () =
    let negativeZero = BitConverter.Int64BitsToDouble 0x8000000000000000L
    let positive = KeyBuilder.ofValues [ SqlValue.vReal 0.0 ] |> ok
    let negative = KeyBuilder.ofValues [ SqlValue.Real negativeZero ] |> ok
    Assert.Equal<byte array>(positive, negative)

[<Fact>]
let ``KeyBuilder.ofValues: Unloaded は InternalError`` () =
    expectErrorKind ErrorKind.InternalError "Unloaded をキーに符号化できてしまう" (KeyBuilder.ofValues [ SqlValue.Unloaded ])

[<Fact>]
let ``KeyBuilder.ofIndexAndPrimary: 索引列 + 主キー列`` () =
    let k = KeyBuilder.ofIndexAndPrimary [ SqlValue.Int 20L ] [ SqlValue.Int 7L ] |> ok
    let prefix = KeyBuilder.ofValues [ SqlValue.Int 20L ] |> ok
    Assert.True(KeyBuilder.startsWithPrefix k prefix)

[<Fact>]
let ``KeyBuilder.startsWithPrefix: 前缀一致`` () =
    let key = [| 1uy; 2uy; 3uy |]
    let prefix = [| 1uy; 2uy |]
    Assert.True(KeyBuilder.startsWithPrefix key prefix)
    Assert.False(KeyBuilder.startsWithPrefix [| 1uy |] prefix)
    Assert.True(KeyBuilder.startsWithPrefix key [||])

[<Fact>]
let ``ValueCodec: 全型の往復`` () =
    let values = Array.init 3000 (fun _ -> randomValue ())
    let w = ValueCodec.writeValues values Writer.Empty |> ok
    let got, _ = ok (ValueCodec.readValues (Cursor.create (Writer.toArray w)))
    Assert.Equal(values.Length, got.Length)

    for i in 0 .. values.Length - 1 do
        let a, b = values.[i], got.[i]

        let same =
            match a, b with
            | SqlValue.Real x, SqlValue.Real y -> Double.IsNaN x && Double.IsNaN y || x = y
            | SqlValue.Blob x, SqlValue.Blob y -> x = y
            | SqlValue.Timestamp x, SqlValue.Timestamp y -> x.UtcTicks = y.UtcTicks
            | _ -> a = b

        Assert.True(same, sprintf "往復に失敗: %s" (SqlLiteral.toSqlLiteral a))

[<Fact>]
let ``ValueCodec: 負のゼロは正のゼロと同じ符号化`` () =
    let negativeZero = BitConverter.Int64BitsToDouble 0x8000000000000000L

    let positive =
        ValueCodec.writeValues [ SqlValue.vReal 0.0; SqlValue.Decimal 0M ] Writer.Empty
        |> ok
        |> Writer.toArray

    let negative =
        ValueCodec.writeValues [ SqlValue.Real negativeZero; SqlValue.Decimal(-0.0M) ] Writer.Empty
        |> ok
        |> Writer.toArray

    Assert.Equal<byte array>(positive, negative)

    let got, _ = ok (ValueCodec.readValues (Cursor.create negative))

    match got with
    | [| SqlValue.Real f; SqlValue.Decimal d |] ->
        let bits = Decimal.GetBits d
        Assert.Equal(0L, BitConverter.DoubleToInt64Bits f)
        Assert.Equal(0u, uint32 bits.[3] &&& 0x80000000u)
    | other -> Assert.Fail(sprintf "往復結果が想定外: %A" other)

[<Fact>]
let ``ValueCodec: 不明な型タグは CorruptionError`` () =
    // 要素数 1 + 不明なタグ 0x7F
    expectErrorKind ErrorKind.CorruptionError "不明な型タグはエラー" (ValueCodec.readValues (Cursor.create [| 1uy; 0x7Fuy |]))

[<Fact>]
let ``ValueCodec: 要素数が大きすぎると CorruptionError`` () =
    let bytes = Writer.build [ Writer.varint (uint64 Int32.MaxValue + 1UL) ]
    expectErrorKind ErrorKind.CorruptionError "要素数が大きすぎる場合はエラー" (ValueCodec.readValues (Cursor.create bytes))

[<Fact>]
let ``ValueCodec.writeValues: Unloaded は InternalError`` () =
    expectErrorKind
        ErrorKind.InternalError
        "Unloaded を行値に符号化できてしまう"
        (ValueCodec.writeValues [ SqlValue.Int 1L; SqlValue.Unloaded ] Writer.Empty)

[<Fact>]
let ``ValueCodec.writeValuesTo: Unloaded は InternalError`` () =
    let out = Array.zeroCreate<byte> 64

    expectErrorKind
        ErrorKind.InternalError
        "Unloaded を行値に符号化できてしまう"
        (ValueCodec.writeValuesTo out 0 [| SqlValue.Unloaded |])

[<Fact>]
let ``ValueCodec.managedBytes: 参照型と UTF-16 を含む管理メモリの概算`` () =
    // 符号化サイズ（ファイルサイズ見積り）より大きい（SqlValue 本体 + string/配列）
    let short = SqlValue.Text "abc"
    let long = SqlValue.Text(System.String('x', 300))

    Assert.True(ValueCodec.managedBytes short > ValueCodec.approximateBytes short)
    Assert.Equal(48 + 2 * 300, ValueCodec.managedBytes long)
    Assert.True(ValueCodec.managedBytes long > ValueCodec.managedBytes short)

    // 固定長は 24 バイト（NULL は共有インスタンスなので 0）
    Assert.Equal(24, ValueCodec.managedBytes (SqlValue.Int 42L))
    Assert.Equal(32, ValueCodec.managedBytes (SqlValue.Decimal 1.5M))
    Assert.Equal(0, ValueCodec.managedBytes SqlValue.Null)

    // 行は配列ヘッダ 24 + スロット 8 × 列数 + 各値
    let row = [| SqlValue.Int 1L; SqlValue.Null |]
    Assert.Equal(24 + 8 * 2 + 24, ValueCodec.managedRowBytes row)

[<Fact>]
let ``Entry.managedBytes: 値が長いほど保持量が大きい`` () =
    let key = [| 1uy; 2uy |]
    let entry = Entry.put key 1UL [| SqlValue.Int 1L; SqlValue.Text "abcdef" |]

    // 同じ行でも値が長いほど大きい
    let longer = Entry.put key 1UL [| SqlValue.Int 1L; SqlValue.Text "abcdefghij" |]
    Assert.True(Entry.managedBytes longer > Entry.managedBytes entry)

    // tombstone は行を持たない（差分は行配列 + 値の分だけ減る）
    let tombstone = Entry.delete key 1UL
    Assert.True(Entry.managedBytes tombstone < Entry.managedBytes entry)

let private negativeZero = BitConverter.Int64BitsToDouble 0x8000000000000000L
let private negativeNaN = BitConverter.Int64BitsToDouble 0xFFF8000000000000L

let private textOfChars (chars: int list) =
    System.String(chars |> List.map char |> Array.ofList)

let private edgeValues: (string * SqlValue list) list =
    [
        "NULL", [ SqlValue.Null ]
        "BOOLEAN", [ SqlValue.Bool false; SqlValue.Bool true ]
        "INTEGER",
        [
            SqlValue.Int Int64.MinValue
            SqlValue.Int -1L
            SqlValue.Int 0L
            SqlValue.Int 1L
            SqlValue.Int Int64.MaxValue
        ]
        "REAL",
        [
            SqlValue.Real Double.NegativeInfinity
            SqlValue.Real Double.MinValue
            SqlValue.Real -1.5
            SqlValue.Real -Double.Epsilon
            SqlValue.Real negativeZero
            SqlValue.Real 0.0
            SqlValue.Real Double.Epsilon
            SqlValue.Real 1.5
            SqlValue.Real Double.MaxValue
            SqlValue.Real Double.PositiveInfinity
            SqlValue.Real Double.NaN
            SqlValue.Real negativeNaN
        ]
        "DECIMAL",
        [
            SqlValue.Decimal Decimal.MinValue
            SqlValue.Decimal -1.00M
            SqlValue.Decimal -1M
            SqlValue.Decimal -0.0M
            SqlValue.Decimal 0M
            SqlValue.Decimal 0.00M
            SqlValue.Decimal 0.0000000000000000000000000001M
            SqlValue.Decimal 1M
            SqlValue.Decimal 1.00M
            SqlValue.Decimal Decimal.MaxValue
        ]
        "TEXT",
        [
            SqlValue.Text ""
            SqlValue.Text "A"
            SqlValue.Text "a"
            SqlValue.Text(textOfChars [ 0 ])
            SqlValue.Text("a" + textOfChars [ 0 ])
            SqlValue.Text(textOfChars [ 0x7F ])
            SqlValue.Text(textOfChars [ 0x80 ])
            SqlValue.Text(textOfChars [ 0x7FF ])
            SqlValue.Text(textOfChars [ 0x800 ])
            SqlValue.Text(textOfChars [ 0xD800 ])
            SqlValue.Text(textOfChars [ 0xDC00 ])
            SqlValue.Text(textOfChars [ 0xE000 ])
            SqlValue.Text(textOfChars [ 0xFF21 ])
            SqlValue.Text(textOfChars [ 0xFFFD ])
            SqlValue.Text(textOfChars [ 0xFFFF ])
            SqlValue.Text(textOfChars [ 0xD83D; 0xDE00 ])
            SqlValue.Text("a" + textOfChars [ 0xD83D; 0xDE00 ])
            SqlValue.Text(System.String('a', 100))
            SqlValue.Text(System.String('a', 101))
        ]
        "BLOB",
        [
            SqlValue.Blob [||]
            SqlValue.Blob [| 0uy |]
            SqlValue.Blob [| 0uy; 0uy |]
            SqlValue.Blob [| 0uy; 1uy |]
            SqlValue.Blob [| 1uy |]
            SqlValue.Blob [| 1uy; 0uy |]
            SqlValue.Blob [| 255uy |]
            SqlValue.Blob [| 255uy; 0uy |]
        ]
        "DATE",
        [
            SqlValue.Date DateOnly.MinValue
            SqlValue.Date(DateOnly.FromDayNumber 1)
            SqlValue.Date(DateOnly.FromDayNumber 100000)
            SqlValue.Date DateOnly.MaxValue
        ]
        "TIMESTAMP",
        [
            SqlValue.Timestamp(DateTimeOffset(DateTime.MinValue.Ticks, TimeSpan.Zero))
            SqlValue.Timestamp(DateTimeOffset(DateTime(1_000_000_000L, DateTimeKind.Utc)))
            SqlValue.Timestamp(DateTimeOffset(2020, 1, 1, 0, 0, 0, TimeSpan.Zero))
            SqlValue.Timestamp(DateTimeOffset(2020, 1, 1, 9, 0, 0, TimeSpan.FromHours 9.0))
            SqlValue.Timestamp(DateTimeOffset(2019, 12, 31, 15, 0, 0, TimeSpan.FromHours -9.0))
            SqlValue.Timestamp(DateTimeOffset(DateTime.MaxValue.Ticks, TimeSpan.Zero))
        ]
        "UUID",
        [
            SqlValue.Uuid Guid.Empty
            SqlValue.Uuid(Guid.Parse "00000000-0000-0000-0000-000000000001")
            SqlValue.Uuid(Guid.Parse "00000000-0000-0000-0000-ffffffffffff")
            SqlValue.Uuid(Guid.Parse "7fffffff-ffff-ffff-ffff-ffffffffffff")
            SqlValue.Uuid(Guid.Parse "80000000-0000-0000-0000-000000000000")
            SqlValue.Uuid(Guid.Parse "ffffffff-ffff-ffff-ffff-ffffffffffff")
        ]
    ]

let private allEdgeValues = edgeValues |> List.collect snd

let private comparableEdgeValues =
    edgeValues |> List.filter (fun (name, _) -> name <> "NULL")

let private label (a: SqlValue) (b: SqlValue) =
    sprintf "a=[%s] b=[%s]" (String.concat ", " (render [| a |])) (String.concat ", " (render [| b |]))

let private assertSameTypeOrder (a: SqlValue) (b: SqlValue) =
    let comparison = ok (compareValues a b)
    let keyA = keyOf a
    let keyB = keyOf b
    let keyComparison = compareKeyBytes keyA keyB

    Assert.True(
        sign comparison = sign keyComparison,
        sprintf "順序が不一致: %s comparison=%d key=%d" (label a b) comparison keyComparison
    )

    Assert.True(
        (comparison = 0) = (keyComparison = 0),
        sprintf "等値判定が不一致: %s comparison=%d key=%d" (label a b) comparison keyComparison
    )

[<Fact>]
let ``順序保持: 同一型の境界値でキー順と比較順が一致する`` () =
    for _, values in comparableEdgeValues do
        for a in values do
            for b in values do
                assertSameTypeOrder a b

[<Fact>]
let ``順序保持: キー順は型順位と同一型内の比較順に一致する`` () =
    for a in allEdgeValues do
        for b in allEdgeValues do
            let comparison = compareForSort a b
            let keyComparison = compareKeyBytes (keyOf a) (keyOf b)

            Assert.True(
                sign comparison = sign keyComparison,
                sprintf "順序が不一致: %s comparison=%d key=%d" (label a b) comparison keyComparison
            )

[<Fact>]
let ``順序保持: ランダムな同一型のペアでキー順と比較順が一致する`` () =
    let values =
        Array.init 4000 (fun _ -> randomValue ())
        |> Array.filter (fun v -> not (SqlValue.isNull v))

    let random = Random 20261110

    for _, group in values |> Array.groupBy SqlValue.dataType do
        for _ in 1..300 do
            let a = group.[random.Next(group.Length)]
            let b = group.[random.Next(group.Length)]
            assertSameTypeOrder a b

[<Fact>]
let ``順序保持: 複合キーのペアでキー順とタプル比較順が一致する`` () =
    let random = Random 20261110

    let tuples =
        Array.init 500 (fun _ ->
            Array.init (random.Next(1, 5)) (fun _ -> allEdgeValues.[random.Next(allEdgeValues.Length)]))

    let compareTuples (a: SqlValue array) (b: SqlValue array) =
        let length = min a.Length b.Length

        let rec compareAt i =
            if i >= length then
                compare a.Length b.Length
            else
                let comparison = compareForSort a.[i] b.[i]
                if comparison <> 0 then comparison else compareAt (i + 1)

        compareAt 0

    for _ in 1..5000 do
        let a = tuples.[random.Next(tuples.Length)]
        let b = tuples.[random.Next(tuples.Length)]
        let comparison = compareTuples a b
        let keyComparison = compareKeyBytes (keyOfTuple a) (keyOfTuple b)

        Assert.True(
            sign comparison = sign keyComparison,
            sprintf
                "順序が不一致: a=[%s] b=[%s] comparison=%d key=%d"
                (String.concat ", " (render a))
                (String.concat ", " (render b))
                comparison
                keyComparison
        )

[<Fact>]
let ``順序保持: Entry と SstEntry の並びが一致する`` () =
    let random = Random 20261111

    let makeEntry key seqNo =
        Entry.put key seqNo [| SqlValue.Int(int64 seqNo) |]

    for _ in 1..2000 do
        let keyA = KeyBuilder.ofValues [ SqlValue.Int(int64 (random.Next 100)) ] |> ok
        let keyB = KeyBuilder.ofValues [ SqlValue.Int(int64 (random.Next 100)) ] |> ok
        let seqA = uint64 (random.Next 10)
        let seqB = uint64 (random.Next 10)

        let eA = makeEntry keyA seqA
        let eB = makeEntry keyB seqB
        let sA = SstEntry.make keyA seqA (Some [| SqlValue.Int(int64 seqA) |])
        let sB = SstEntry.make keyB seqB (Some [| SqlValue.Int(int64 seqB) |])

        Assert.Equal(sign (Entry.compareKeys eA eB), sign (SstEntry.compare sA sB))

[<Fact>]
let ``順序保持: KeyOrder は反対称で推移的`` () =
    let random = Random 20261112

    let randomPair () =
        let key = KeyBuilder.ofValues [ SqlValue.Int(int64 (random.Next 50)) ] |> ok
        key, uint64 (random.Next 5)

    for _ in 1..1000 do
        let ka, sa = randomPair ()
        let kb, sb = randomPair ()
        let ab = sign (KeyOrder.compare ka sa kb sb)
        let ba = sign (KeyOrder.compare kb sb ka sa)
        Assert.Equal(-ab, ba)

    for _ in 1..1000 do
        let ka, sa = randomPair ()
        let kb, sb = randomPair ()
        let kc, sc = randomPair ()
        let ab = sign (KeyOrder.compare ka sa kb sb)
        let bc = sign (KeyOrder.compare kb sb kc sc)
        let ac = sign (KeyOrder.compare ka sa kc sc)

        if ab <= 0 && bc <= 0 then
            Assert.True(ac <= 0, "KeyOrder の推移性が壊れています")
