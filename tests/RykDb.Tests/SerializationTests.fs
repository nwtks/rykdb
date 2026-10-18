// ===========================================================================
// Serialization.fs（順序保持符号化・値コードック）
// ===========================================================================

module RykDb.SerializationTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Writer: writeByte / readByte の往復`` () =
    let w = Writer.empty |> Writer.writeByte 42uy |> Writer.toArray
    let c = Cursor.create w
    let b, c2 = ok (Reader.readByte c)
    Assert.Equal(42uy, b)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: writeBytes / readBytes の往復`` () =
    let bs = [| 1uy; 2uy; 3uy |]
    let w = Writer.empty |> Writer.writeBytes bs |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readBytes 3 c)
    Assert.Equal<byte array>(bs, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: bool / readBool の往復`` () =
    let w = Writer.empty |> Writer.bool true |> Writer.bool false |> Writer.toArray
    let c = Cursor.create w
    let b1, c2 = ok (Reader.readBool c)
    let b2, c3 = ok (Reader.readBool c2)
    Assert.True b1
    Assert.False b2
    Assert.True(Cursor.atEnd c3)

[<Fact>]
let ``Writer: varint の往復`` () =
    for v in [ 0UL; 1UL; 127UL; 128UL; 16383UL; 16384UL; UInt64.MaxValue ] do
        let w = Writer.empty |> Writer.varint v |> Writer.toArray
        let c = Cursor.create w
        let got, c2 = ok (Reader.readVarint c)
        Assert.Equal(v, got)
        Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: int64LE / readInt64LE の往復`` () =
    let v = 0x0123456789ABCDEFL
    let w = Writer.empty |> Writer.int64LE v |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readInt64LE c)
    Assert.Equal(v, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: uint32LE / readUInt32LE の往復`` () =
    let v = 0x89ABCDEFu
    let w = Writer.empty |> Writer.uint32LE v |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readUInt32LE c)
    Assert.Equal(v, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: lengthPrefixed / readLengthPrefixed の往復`` () =
    let bs = [| 9uy; 8uy; 7uy |]
    let w = Writer.empty |> Writer.lengthPrefixed bs |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readLengthPrefixed c)
    Assert.Equal<byte array>(bs, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: str / readStr の往復`` () =
    let s = "hello"
    let w = Writer.empty |> Writer.str s |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readStr c)
    Assert.Equal(s, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: build はステップを左から適用する`` () =
    let w = Writer.build [ Writer.writeByte 1uy; Writer.writeByte 2uy ]
    Assert.Equal<byte array>([| 1uy; 2uy |], w)

[<Fact>]
let ``Cursor: createSlice は範囲を切り出す`` () =
    let data = [| 0uy; 1uy; 2uy; 3uy; 4uy |]
    let c = Cursor.createSlice data 1 3
    Assert.Equal(3, Cursor.remaining c)
    let b, c2 = ok (Reader.readByte c)
    Assert.Equal(1uy, b)
    Assert.Equal(2, Cursor.remaining c2)

[<Fact>]
let ``Cursor: atEnd は位置が終端に達したら true`` () =
    let c = Cursor.create [| 1uy |]
    let _, c2 = ok (Reader.readByte c)
    Assert.True(Cursor.atEnd c2)
    Assert.False(Cursor.atEnd c)

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
        Writer.empty
        |> Writer.writeByte 1uy
        |> Writer.writeByte 2uy
        |> Writer.writeByte 3uy
        |> Writer.toArray

    let c = Cursor.create w

    let items, c2 =
        ok (Reader.readItems 3UL (fun c -> Reader.readByte c |> Result.map (fun (b, c) -> int b, c)) c)

    Assert.Equal<int list>([ 1; 2; 3 ], items)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Reader: readItems は readOne のエラーを伝播する`` () =
    expectErrorKind
        ErrorKind.CorruptionError
        "要素の読み取りエラーは伝播する"
        (Reader.readItems 1UL Reader.readByte (Cursor.create [||]))

[<Fact>]
let ``OrderPreserving: 全型の往復`` () =
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

[<Fact>]
let ``OrderPreserving: 複合キーの先頭フィールドを読める`` () =
    // TEXT の終端 0x00 の直後に次フィールド（INT のタグ）が続くケース
    let v, _ =
        ok (OrderPreserving.readFirstValue (keyOfTuple [| SqlValue.Text "a"; SqlValue.Int 7L |]))

    Assert.Equal(SqlValue.Text "a", v)

    // escape された 0x00 を含む BLOB でも同じ経路を通る
    let v2, _ =
        ok (OrderPreserving.readFirstValue (keyOfTuple [| SqlValue.Blob [| 0uy; 1uy |]; SqlValue.Int 7L |]))

    match v2 with
    | SqlValue.Blob b -> Assert.Equal<byte array>([| 0uy; 1uy |], b)
    | other -> Assert.Fail(sprintf "BLOB ではありません: %A" other)

[<Fact>]
let ``OrderPreserving: Decimal 0 は往復できる`` () =
    let v, _ = ok (OrderPreserving.readFirstValue (keyOf (SqlValue.Decimal 0M)))
    Assert.Equal(SqlValue.Decimal 0M, v)

[<Fact>]
let ``OrderPreserving: 復元できない DECIMAL は CorruptionError`` () =
    // DECIMAL タグ + 符号=正・指数=30・桁=29 個の 9 → 9.99...×10^29 となり decimal の範囲を超える
    let bytes =
        Array.concat
            [
                [| 0x22uy; 0x01uy |]
                Writer.build [ Writer.uint64BE 0x800000000000001EUL ]
                Array.create 29 9uy
                [| 0x00uy |]
            ]

    expectErrorKind ErrorKind.CorruptionError "範囲外の DECIMAL はエラー" (OrderPreserving.readFirstValue bytes)

[<Fact>]
let ``OrderPreserving: 終端バイトが不正なら CorruptionError`` () =
    // INT の 8 バイトの後に終端 0x00 ではなく 0x01 が続く
    let bytes =
        Writer.build [ Writer.writeByte 0x20uy; Writer.uint64BE 0UL; Writer.writeByte 0x01uy ]

    expectErrorKind ErrorKind.CorruptionError "終端が不正ならエラー" (OrderPreserving.readFirstValue bytes)

[<Fact>]
let ``OrderPreserving: 終端の無い TEXT は CorruptionError`` () =
    let bytes = [| 0x50uy; 0x61uy |]
    expectErrorKind ErrorKind.CorruptionError "終端の無い TEXT はエラー" (OrderPreserving.readFirstValue bytes)

[<Fact>]
let ``OrderPreserving: 不明な型タグは CorruptionError`` () =
    expectErrorKind ErrorKind.CorruptionError "不明な型タグはエラー" (OrderPreserving.readFirstValue [| 0x01uy |])

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
let ``KeyBuilder.ofValues: 空列は空キー`` () =
    let k = KeyBuilder.ofValues []
    Assert.Equal(0, k.Length)

[<Fact>]
let ``KeyBuilder.ofValues: 値を符号化する`` () =
    let k = KeyBuilder.ofValues [ SqlValue.Int 1L ]
    Assert.True(k.Length > 0)

[<Fact>]
let ``KeyBuilder.ofIndexAndPrimary: 索引列 + 主キー列`` () =
    let k = KeyBuilder.ofIndexAndPrimary [ SqlValue.Int 20L ] [ SqlValue.Int 7L ]
    let prefix = KeyBuilder.ofValues [ SqlValue.Int 20L ]
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
    let w = ValueCodec.writeValues values Writer.empty
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
let ``ValueCodec: 不明な型タグは CorruptionError`` () =
    // 要素数 1 + 不明なタグ 0x7F
    expectErrorKind ErrorKind.CorruptionError "不明な型タグはエラー" (ValueCodec.readValues (Cursor.create [| 1uy; 0x7Fuy |]))

[<Fact>]
let ``ValueCodec: 要素数が大きすぎると CorruptionError`` () =
    let bytes = Writer.build [ Writer.varint (uint64 Int32.MaxValue + 1UL) ]
    expectErrorKind ErrorKind.CorruptionError "要素数が大きすぎる場合はエラー" (ValueCodec.readValues (Cursor.create bytes))
