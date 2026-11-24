// ===========================================================================
// Serialization.fs（順序保持符号化・値コードック）
// ===========================================================================

module RykDb.SerializationTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private negativeZero = System.BitConverter.Int64BitsToDouble 0x8000000000000000L
let private negativeNaN = System.BitConverter.Int64BitsToDouble 0xFFF8000000000000L

let private textOfChars (chars: int list) =
    System.String(chars |> List.map char |> Array.ofList)

let private edgeValues: (string * SqlValue list) list =
    [
        "NULL", [ SqlValue.Null ]
        "BOOLEAN", [ SqlValue.Bool false; SqlValue.Bool true ]
        "INTEGER",
        [
            SqlValue.Int System.Int64.MinValue
            SqlValue.Int -1L
            SqlValue.Int 0L
            SqlValue.Int 1L
            SqlValue.Int System.Int64.MaxValue
        ]
        "REAL",
        [
            SqlValue.Real System.Double.NegativeInfinity
            SqlValue.Real System.Double.MinValue
            SqlValue.Real -1.5
            SqlValue.Real -System.Double.Epsilon
            SqlValue.Real negativeZero
            SqlValue.Real 0.0
            SqlValue.Real System.Double.Epsilon
            SqlValue.Real 1.5
            SqlValue.Real System.Double.MaxValue
            SqlValue.Real System.Double.PositiveInfinity
            SqlValue.Real System.Double.NaN
            SqlValue.Real negativeNaN
        ]
        "DECIMAL",
        [
            SqlValue.Decimal System.Decimal.MinValue
            SqlValue.Decimal -1.00M
            SqlValue.Decimal -1M
            SqlValue.Decimal -0.0M
            SqlValue.Decimal 0M
            SqlValue.Decimal 0.00M
            SqlValue.Decimal 0.0000000000000000000000000001M
            SqlValue.Decimal 1M
            SqlValue.Decimal 1.00M
            SqlValue.Decimal System.Decimal.MaxValue
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
            SqlValue.Date System.DateOnly.MinValue
            SqlValue.Date(System.DateOnly.FromDayNumber 1)
            SqlValue.Date(System.DateOnly.FromDayNumber 100000)
            SqlValue.Date System.DateOnly.MaxValue
        ]
        "TIMESTAMP",
        [
            SqlValue.Timestamp(System.DateTimeOffset(System.DateTime.MinValue.Ticks, System.TimeSpan.Zero))
            SqlValue.Timestamp(System.DateTimeOffset(System.DateTime(1_000_000_000L, System.DateTimeKind.Utc)))
            SqlValue.Timestamp(System.DateTimeOffset(2020, 1, 1, 0, 0, 0, System.TimeSpan.Zero))
            SqlValue.Timestamp(System.DateTimeOffset(2020, 1, 1, 9, 0, 0, System.TimeSpan.FromHours 9.0))
            SqlValue.Timestamp(System.DateTimeOffset(2019, 12, 31, 15, 0, 0, System.TimeSpan.FromHours -9.0))
            SqlValue.Timestamp(System.DateTimeOffset(System.DateTime.MaxValue.Ticks, System.TimeSpan.Zero))
        ]
        "UUID",
        [
            SqlValue.Uuid System.Guid.Empty
            SqlValue.Uuid(System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            SqlValue.Uuid(System.Guid.Parse "00000000-0000-0000-0000-ffffffffffff")
            SqlValue.Uuid(System.Guid.Parse "7fffffff-ffff-ffff-ffff-ffffffffffff")
            SqlValue.Uuid(System.Guid.Parse "80000000-0000-0000-0000-000000000000")
            SqlValue.Uuid(System.Guid.Parse "ffffffff-ffff-ffff-ffff-ffffffffffff")
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
    let bytes = Writer.build [ Writer.varint (uint64 System.Int32.MaxValue + 1UL) ]
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
                SqlValue.Int System.Int64.MinValue
                SqlValue.Int -5000L
                SqlValue.Int -1L
                SqlValue.Int 0L
                SqlValue.Int 1L
                SqlValue.Int System.Int64.MaxValue
            ]
            "REAL",
            [
                SqlValue.Real System.Double.NegativeInfinity
                SqlValue.Real -1.5
                SqlValue.Real 0.0
                SqlValue.Real 1.5
                SqlValue.Real System.Double.PositiveInfinity
            ]
            "REAL_NaN", [ SqlValue.Real 1.5; SqlValue.Real System.Double.NaN ]
            "DECIMAL",
            [
                SqlValue.Decimal System.Decimal.MinValue
                SqlValue.Decimal -12345.6789M
                SqlValue.Decimal -0.000001M
                SqlValue.Decimal 0M
                SqlValue.Decimal 0.000001M
                SqlValue.Decimal 12345.6789M
                SqlValue.Decimal System.Decimal.MaxValue
            ]
            "BOOLEAN", [ SqlValue.Bool false; SqlValue.Bool true ]
            "DATE",
            [
                SqlValue.Date System.DateOnly.MinValue
                SqlValue.Date(System.DateOnly.FromDayNumber 1)
                SqlValue.Date(System.DateOnly.FromDayNumber 100000)
                SqlValue.Date System.DateOnly.MaxValue
            ]
            "TIMESTAMP",
            [
                SqlValue.Timestamp(System.DateTimeOffset(System.DateTime(0L, System.DateTimeKind.Utc)))
                SqlValue.Timestamp(System.DateTimeOffset(System.DateTime(1_000_000_000L, System.DateTimeKind.Utc)))
                SqlValue.Timestamp(System.DateTimeOffset(System.DateTime.MaxValue.Ticks, System.TimeSpan.Zero))
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
                SqlValue.Uuid(System.Guid.Parse "00000000-0000-0000-0000-000000000000")
                SqlValue.Uuid(System.Guid.Parse "00000000-0000-0000-0000-000000000001")
                SqlValue.Uuid(System.Guid.Parse "7fffffff-ffff-ffff-ffff-ffffffffffff")
                SqlValue.Uuid(System.Guid.Parse "ffffffff-ffff-ffff-ffff-ffffffffffff")
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

    let random = System.Random 20261110

    for _, group in values |> Array.groupBy (SqlValue.dataType >> ok) do
        for _ in 1..300 do
            let a = group.[random.Next(group.Length)]
            let b = group.[random.Next(group.Length)]
            assertSameTypeOrder a b

[<Fact>]
let ``順序保持: 複合キーのペアでキー順とタプル比較順が一致する`` () =
    let random = System.Random 20261110

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
    let random = System.Random 20261111

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
    let random = System.Random 20261112

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
