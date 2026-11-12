module RykDb.OrderingPropertyTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

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
    let comparison = ok (SqlValue.compareValues a b)
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
let ``順序保持プロパティ: 同一型の境界値でキー順と比較順が一致する`` () =
    for _, values in comparableEdgeValues do
        for a in values do
            for b in values do
                assertSameTypeOrder a b

[<Fact>]
let ``順序保持プロパティ: キー順は型順位と同一型内の比較順に一致する`` () =
    for a in allEdgeValues do
        for b in allEdgeValues do
            let comparison = compareForSort a b
            let keyComparison = compareKeyBytes (keyOf a) (keyOf b)

            Assert.True(
                sign comparison = sign keyComparison,
                sprintf "順序が不一致: %s comparison=%d key=%d" (label a b) comparison keyComparison
            )

[<Fact>]
let ``順序保持プロパティ: ランダムな同一型のペアでキー順と比較順が一致する`` () =
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
let ``順序保持プロパティ: 複合キーのペアでキー順とタプル比較順が一致する`` () =
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
let ``順序保持プロパティ: Entry と SstEntry の並びが一致する`` () =
    let random = Random 20261111

    let makeEntry key seqNo =
        Entry.put key seqNo [| SqlValue.Int(int64 seqNo) |]

    for _ in 1..2000 do
        let keyA = KeyBuilder.ofValues [ SqlValue.Int(int64 (random.Next 100)) ]
        let keyB = KeyBuilder.ofValues [ SqlValue.Int(int64 (random.Next 100)) ]
        let seqA = uint64 (random.Next 10)
        let seqB = uint64 (random.Next 10)

        let eA = makeEntry keyA seqA
        let eB = makeEntry keyB seqB
        let sA = SstEntry.make keyA seqA (Some [| SqlValue.Int(int64 seqA) |])
        let sB = SstEntry.make keyB seqB (Some [| SqlValue.Int(int64 seqB) |])

        Assert.Equal(sign (Entry.compareKeys eA eB), sign (SstEntry.compare sA sB))

[<Fact>]
let ``順序保持プロパティ: KeyOrder は反対称で推移的`` () =
    let random = Random 20261112

    let randomPair () =
        let key = KeyBuilder.ofValues [ SqlValue.Int(int64 (random.Next 50)) ]
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
