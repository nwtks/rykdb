// ===========================================================================
// Serialization.fs（順序保持符号化・値コードック）
// ===========================================================================

module RykDb.SerializationTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

// ---------------------------------------------------------------------------
// 往復テスト
// ---------------------------------------------------------------------------

[<Fact>]
let ``順序保持: 全型の往復`` () =
    let values = Array.init 4000 (fun _ -> randomValue ())

    for v in values do
        let key = keyOf v
        let decoded, _ = ok (OrderPreserving.readFirstValue key)

        let expected =
            match v, decoded with
            | SqlValue.Real a, SqlValue.Real b -> if Double.IsNaN a then Double.IsNaN b else a = b
            | SqlValue.Blob a, SqlValue.Blob b -> a = b
            | SqlValue.Timestamp a, SqlValue.Timestamp b -> a.UtcTicks = b.UtcTicks
            | a, b -> a = b

        Assert.True(expected, sprintf "往復に失敗: %s -> %s" (SqlLiteral.toSqlLiteral v) (SqlLiteral.toSqlLiteral decoded))

// ---------------------------------------------------------------------------
// ★ 最重要: バイト順 == SQL 比較順
// ---------------------------------------------------------------------------

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

/// 型ごとの境界値でバイト順と SQL 比較順の一致を検証する
///（ランダム値では 0・極小/極大・空文字列などの端を捕まえられない）。
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

    for (_, values) in cases do
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

// ---------------------------------------------------------------------------
// 行ペイロードの往復
// ---------------------------------------------------------------------------

[<Fact>]
let ``値コードック: 全型の往復`` () =
    let values = Array.init 3000 (fun _ -> randomValue ())

    let w = ValueCodec.writeValues values Writer.empty
    let got, _ = ok (ValueCodec.readValues (Cursor.create (Writer.toArray w)))

    Assert.Equal(values.Length, got.Length)

    for i in 0 .. values.Length - 1 do
        let a, b = values.[i], got.[i]

        let same =
            match a, b with
            | SqlValue.Real x, SqlValue.Real y -> (Double.IsNaN x && Double.IsNaN y) || x = y
            | SqlValue.Blob x, SqlValue.Blob y -> x = y
            | SqlValue.Timestamp x, SqlValue.Timestamp y -> x.UtcTicks = y.UtcTicks
            | _ -> a = b

        Assert.True(same, sprintf "往復に失敗: %s" (SqlLiteral.toSqlLiteral a))

[<Fact>]
let ``末尾再帰: 接頭辞一致で長いキーを扱える`` () =
    let len = 500_000
    let prefix = Array.init len (fun i -> byte (i % 251))
    let key = Array.append prefix [| 1uy; 2uy; 3uy |]

    Assert.True(KeyBuilder.startsWithPrefix key prefix)
    Assert.True(KeyBuilder.startsWithPrefix prefix prefix)
    Assert.False(KeyBuilder.startsWithPrefix prefix key)

[<Fact>]
let ``末尾再帰: 多くの値を持つ行を往復できる`` () =
    let n = 200_000
    let values = Array.init n (fun i -> SqlValue.Int(int64 i))
    let w = ValueCodec.writeValues values Writer.empty
    let got, _ = ok (ValueCodec.readValues (Cursor.create (Writer.toArray w)))

    Assert.Equal(n, got.Length)
    Assert.Equal(int64 0, (got.[0] |> SqlValue.asInt64 |> Option.defaultValue -1L))
    Assert.Equal(int64 (n - 1), (got.[n - 1] |> SqlValue.asInt64 |> Option.defaultValue -1L))

[<Fact>]
let ``末尾再帰: 長い varint を読み書きできる`` () =
    // varint は 64bit なので最大 10 バイト。
    // 「ちょうど n バイトになる最小値」= 2^(7(n-1)) で各長さを確認する。
    for bytes in [ 1..10 ] do
        let v = 1UL <<< (7 * (bytes - 1))
        let w = Writer.varint v Writer.empty
        let arr = Writer.toArray w
        Assert.Equal(bytes, arr.Length)
        let got, _ = ok (Reader.readVarint (Cursor.create arr))
        Assert.Equal(v, got)
