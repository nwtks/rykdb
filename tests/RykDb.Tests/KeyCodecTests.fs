// ===========================================================================
// KeyCodec.fs のテスト（Hashing / KeyBuilder / ValueCodec）
// ===========================================================================

module RykDb.KeyCodecTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

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
    let negativeZero = System.BitConverter.Int64BitsToDouble 0x8000000000000000L
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
let ``KeyBuilder.inEqualityPrefix: NUL 継続値を等値から除外する`` () =
    // TEXT/BLOB の NUL は 0x00 0xFF にエスケープされるため、
    // 短い値の符号化は「NUL を含むより長い値」の接頭辞になる。
    let shortValue = SqlValue.Blob [| 0x61uy |]
    let longValue = SqlValue.Blob [| 0x61uy; 0x00uy; 0x62uy |]
    let prefix = KeyBuilder.ofValues [ shortValue ] |> ok
    let upper = KeyBuilder.equalityPrefixUpper prefix
    let exact = KeyBuilder.ofIndexAndPrimary [ shortValue ] [ SqlValue.Int 7L ] |> ok
    let longer = KeyBuilder.ofIndexAndPrimary [ longValue ] [ SqlValue.Int 7L ] |> ok
    // 生の接頭辞一致はより長い値にも当たる（startsWithPrefix の契約）。
    Assert.True(KeyBuilder.startsWithPrefix longer prefix)
    // 等値用の範囲は主キーが続く exact だけを通す。
    Assert.True(KeyBuilder.inEqualityPrefix prefix upper exact)
    Assert.True(KeyBuilder.inEqualityPrefix prefix upper prefix)
    Assert.False(KeyBuilder.inEqualityPrefix prefix upper longer)

    let textPrefix = KeyBuilder.ofValues [ SqlValue.Text "a" ] |> ok
    let textUpper = KeyBuilder.equalityPrefixUpper textPrefix

    let textLonger =
        KeyBuilder.ofIndexAndPrimary [ SqlValue.Text("a" + string (char 0) + "b") ] [ SqlValue.Int 1L ]
        |> ok

    Assert.True(KeyBuilder.startsWithPrefix textLonger textPrefix)
    Assert.False(KeyBuilder.inEqualityPrefix textPrefix textUpper textLonger)

[<Fact>]
let ``ValueCodec.readValues: スピル専用の Unloaded は CorruptionError`` () =
    let w = ok (ValueCodec.writeValuesSparse [| SqlValue.Unloaded |] Writer.Empty)
    let bytes = Writer.toArray w
    expectErrorKind ErrorKind.CorruptionError "永続データに Unloaded が現れている" (ValueCodec.readValues (Cursor.create bytes))

    match ValueCodec.readValuesSparse (Cursor.create bytes) with
    | Ok(values, _) ->
        match values.[0] with
        | SqlValue.Unloaded -> ()
        | other -> failActual "Unloaded ではない" other
    | Error e -> Assert.Fail(sprintf "sparse 復号が失敗しました: %s" (DbError.toDisplayString e))

[<Fact>]
let ``ValueCodec.readValuesMasked: 切り詰められた固定長値は CorruptionError`` () =
    let w =
        ok (ValueCodec.writeValues [| SqlValue.Int 1L; SqlValue.Decimal 2M |] Writer.Empty)

    let bytes = Writer.toArray w
    // 最後の DECIMAL（33バイト）を途中で切る。読まない列でも境界を確かめる。
    let truncated = Array.sub bytes 0 (bytes.Length - 10)

    expectErrorKind
        ErrorKind.CorruptionError
        "切り詰められた値を読み飛ばしている"
        (ValueCodec.readValuesMasked [| false; false |] (Cursor.create truncated))

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
            | SqlValue.Real x, SqlValue.Real y -> System.Double.IsNaN x && System.Double.IsNaN y || x = y
            | SqlValue.Blob x, SqlValue.Blob y -> x = y
            | SqlValue.Timestamp x, SqlValue.Timestamp y -> x.UtcTicks = y.UtcTicks
            | _ -> a = b

        Assert.True(same, sprintf "往復に失敗: %s" (SqlLiteral.toSqlLiteral a))

[<Fact>]
let ``ValueCodec: 負のゼロは正のゼロと同じ符号化`` () =
    let negativeZero = System.BitConverter.Int64BitsToDouble 0x8000000000000000L

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
        let bits = System.Decimal.GetBits d
        Assert.Equal(0L, System.BitConverter.DoubleToInt64Bits f)
        Assert.Equal(0u, uint32 bits.[3] &&& 0x80000000u)
    | other -> failActual "往復結果が想定外" other

[<Fact>]
let ``ValueCodec: 不明な型タグは CorruptionError`` () =
    // 要素数 1 + 不明なタグ 0x7F
    expectErrorKind ErrorKind.CorruptionError "不明な型タグはエラー" (ValueCodec.readValues (Cursor.create [| 1uy; 0x7Fuy |]))

[<Fact>]
let ``ValueCodec: 要素数が大きすぎると CorruptionError`` () =
    let bytes = Writer.build [ Writer.varint (uint64 System.Int32.MaxValue + 1UL) ]
    expectErrorKind ErrorKind.CorruptionError "要素数が大きすぎる場合はエラー" (ValueCodec.readValues (Cursor.create bytes))

[<Fact>]
let ``ValueCodec: 要素数がカーソル残りを超えると CorruptionError`` () =
    // 2^31-1 個を宣言するが値は 1 バイトも無い。確保より先に弾かないと巨大な配列を確保する。
    let bytes = Writer.build [ Writer.varint (uint64 System.Int32.MaxValue - 1UL) ]
    expectErrorKind ErrorKind.CorruptionError "確保前に要素数をカーソル残りと突き合わせる" (ValueCodec.readValues (Cursor.create bytes))

    expectErrorKind
        ErrorKind.CorruptionError
        "マスク復号も同じ検査をする"
        (ValueCodec.readValuesMasked [| true |] (Cursor.create bytes))

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
