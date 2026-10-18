// ===========================================================================
// Types.fs（3 値論理・値の比較）
// ===========================================================================

module RykDb.TypesTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``TruthValue: AND/OR`` () =
    Assert.Equal(TruthValue.False, TruthValue.and2 TruthValue.False TruthValue.Unknown)
    Assert.Equal(TruthValue.False, TruthValue.and2 TruthValue.Unknown TruthValue.False)
    Assert.Equal(TruthValue.Unknown, TruthValue.and2 TruthValue.True TruthValue.Unknown)
    Assert.Equal(TruthValue.Unknown, TruthValue.and2 TruthValue.Unknown TruthValue.True)
    Assert.Equal(TruthValue.True, TruthValue.and2 TruthValue.True TruthValue.True)
    Assert.Equal(TruthValue.True, TruthValue.or2 TruthValue.True TruthValue.Unknown)
    Assert.Equal(TruthValue.True, TruthValue.or2 TruthValue.Unknown TruthValue.True)
    Assert.Equal(TruthValue.Unknown, TruthValue.or2 TruthValue.False TruthValue.Unknown)
    Assert.Equal(TruthValue.Unknown, TruthValue.or2 TruthValue.Unknown TruthValue.False)
    Assert.Equal(TruthValue.False, TruthValue.or2 TruthValue.False TruthValue.False)

[<Fact>]
let ``DataType: 型タグの往復と未知タグの扱い`` () =
    let all =
        [
            DataType.Null
            DataType.Boolean
            DataType.Integer
            DataType.Real
            DataType.Decimal
            DataType.Date
            DataType.Timestamp
            DataType.Text
            DataType.Blob
            DataType.Uuid
        ]

    for t in all do
        Assert.Equal(t, ok (DataType.fromTag (DataType.toTag t)))

    // タグは照合順（rank 順）に単調増加する
    all
    |> List.map DataType.toTag
    |> List.pairwise
    |> List.iter (fun (a, b) -> Assert.True(a < b, "型タグが単調増加でない"))

    expectErrorKind ErrorKind.CorruptionError "未知のタグは CorruptionError になるはず" (DataType.fromTag 200uy)

[<Fact>]
let ``SqlValue: asNumeric は NaN と無限大を拒否する`` () =
    Assert.True((SqlValue.asNumeric (SqlValue.Real Double.NaN)).IsNone)
    Assert.True((SqlValue.asNumeric (SqlValue.Real Double.PositiveInfinity)).IsNone)
    Assert.True((SqlValue.asNumeric (SqlValue.Real Double.NegativeInfinity)).IsNone)
    Assert.Equal<decimal option>(Some 2.5M, SqlValue.asNumeric (SqlValue.Real 2.5))
    Assert.Equal<decimal option>(Some 3M, SqlValue.asNumeric (SqlValue.Int 3L))
    Assert.True((SqlValue.asNumeric (SqlValue.Text "3")).IsNone)
    Assert.True((SqlValue.asNumeric SqlValue.Null).IsNone)

[<Fact>]
let ``SqlValue.asText: Text だけ Some`` () =
    Assert.Equal(Some "hi", SqlValue.asText (SqlValue.Text "hi"))
    Assert.True((SqlValue.asText (SqlValue.Int 1L)).IsNone)
    Assert.True((SqlValue.asText SqlValue.Null).IsNone)

[<Fact>]
let ``SqlValue.asInt64: Int だけ Some`` () =
    Assert.Equal(Some 7L, SqlValue.asInt64 (SqlValue.Int 7L))
    Assert.True((SqlValue.asInt64 (SqlValue.Real 7.0)).IsNone)
    Assert.True((SqlValue.asInt64 SqlValue.Null).IsNone)

[<Fact>]
let ``SqlValue.normalizeFloat: NaN は canonicalNaN`` () =
    Assert.Equal(SqlValue.CanonicalNaNBits, BitConverter.DoubleToInt64Bits(SqlValue.normalizeFloat Double.NaN))
    Assert.Equal(1.5, SqlValue.normalizeFloat 1.5)

[<Fact>]
let ``SqlValue: NaN は正の quiet NaN に正規化される`` () =
    // .NET の Double.NaN は符号ビットが立った負の NaN。そのままキーにすると
    // 順序保持の前提が崩れるため、単一のビットパターンへ正規化する。
    let negativeNaN = BitConverter.Int64BitsToDouble 0xFFF8000000000000L

    match SqlValue.vReal negativeNaN with
    | SqlValue.Real f -> Assert.Equal(SqlValue.CanonicalNaNBits, BitConverter.DoubleToInt64Bits f)
    | other -> Assert.Fail(sprintf "REAL ではありません: %A" other)

    // 正規化後は NaN 同士が等しく、REAL の中では最大
    Assert.Equal(0, ok (SqlValue.compareValues (SqlValue.vReal negativeNaN) (SqlValue.vReal Double.NaN)))
    Assert.True(ok (SqlValue.compareValues (SqlValue.vReal Double.NaN) (SqlValue.vReal 1.0)) > 0)

[<Fact>]
let ``SqlValue.compareValues: NaN は NaN と等しく、他の数値より大きい`` () =
    let nan = SqlValue.vReal Double.NaN
    let one = SqlValue.vReal 1.0
    Assert.Equal(0, SqlValue.compareValues nan nan |> okOr 1)
    Assert.True(SqlValue.compareValues nan one |> okOr 0 > 0)
    Assert.True(SqlValue.compareValues one nan |> okOr 0 < 0)

[<Fact>]
let ``SqlValue.compareValues: 比較できない型の組み合わせは typeError`` () =
    expectErrorKind ErrorKind.TypeError "型エラーになるはず" (SqlValue.compareValues (SqlValue.Int 1L) (SqlValue.Text "1"))
    expectErrorKind ErrorKind.TypeError "型エラーになるはず" (SqlValue.compareValues (SqlValue.Bool true) (SqlValue.Int 1L))

[<Fact>]
let ``SqlValue.compareValues: NaN と他型の数値は typeError`` () =
    expectErrorKind
        ErrorKind.TypeError
        "NaN と INT は比較できない"
        (SqlValue.compareValues (SqlValue.vReal Double.NaN) (SqlValue.Int 1L))

    expectErrorKind
        ErrorKind.TypeError
        "INT と NaN は比較できない"
        (SqlValue.compareValues (SqlValue.Int 1L) (SqlValue.vReal Double.NaN))

[<Fact>]
let ``SqlValue.compareValues: 同型どうしは順序どおり`` () =
    Assert.True(ok (SqlValue.compareValues (SqlValue.Bool false) (SqlValue.Bool true)) < 0)
    Assert.True(ok (SqlValue.compareValues (SqlValue.Text "a") (SqlValue.Text "b")) < 0)
    Assert.True(ok (SqlValue.compareValues (SqlValue.Blob [| 1uy |]) (SqlValue.Blob [| 1uy; 2uy |])) < 0)

    Assert.True(
        ok (SqlValue.compareValues (SqlValue.Date(DateOnly(2026, 1, 1))) (SqlValue.Date(DateOnly(2026, 1, 2)))) < 0
    )

    let g1 = Guid "11111111-1111-1111-1111-111111111111"
    let g2 = Guid "22222222-2222-2222-2222-222222222222"
    Assert.True(ok (SqlValue.compareValues (SqlValue.Uuid g1) (SqlValue.Uuid g2)) < 0)

[<Fact>]
let ``SqlValue.compareValues: 数値 3種と日時は相互に比較できる`` () =
    Assert.True(ok (SqlValue.compareValues (SqlValue.Int 1L) (SqlValue.Int 2L)) < 0)
    Assert.True(ok (SqlValue.compareValues (SqlValue.Int 2L) (SqlValue.Real 2.5)) < 0)
    Assert.Equal(0, ok (SqlValue.compareValues (SqlValue.Real 3.0) (SqlValue.Decimal 3M)))
    Assert.True(ok (SqlValue.compareValues (SqlValue.Decimal 1M) (SqlValue.Int 2L)) < 0)
    Assert.True(ok (SqlValue.compareValues (SqlValue.Decimal 2M) (SqlValue.Decimal 1M)) > 0)

    // 同じ REAL どうしは数値順
    Assert.True(ok (SqlValue.compareValues (SqlValue.Real 1.5) (SqlValue.Real 2.5)) < 0)

    // DATE は UTC 午夜の TIMESTAMP として比較する（両方向）
    let d = DateOnly(2026, 1, 2)
    let t = DateTimeOffset(DateTime(2026, 1, 2, 0, 0, 0, DateTimeKind.Utc))
    Assert.Equal(0, ok (SqlValue.compareValues (SqlValue.Date d) (SqlValue.Timestamp t)))
    Assert.True(ok (SqlValue.compareValues (SqlValue.Date d) (SqlValue.Timestamp(t.AddDays 1.0))) < 0)
    Assert.Equal(0, ok (SqlValue.compareValues (SqlValue.Timestamp t) (SqlValue.Date d)))
    Assert.True(ok (SqlValue.compareValues (SqlValue.Timestamp(t.AddDays 1.0)) (SqlValue.Date d)) > 0)

[<Fact>]
let ``SqlValue.toDisplayString: 各型`` () =
    Assert.Equal("NULL", SqlValue.toDisplayString SqlValue.Null)
    Assert.Equal("true", SqlValue.toDisplayString (SqlValue.Bool true))
    Assert.Equal("false", SqlValue.toDisplayString (SqlValue.Bool false))
    Assert.Equal("42", SqlValue.toDisplayString (SqlValue.Int 42L))
    Assert.Equal("2.5", SqlValue.toDisplayString (SqlValue.Real 2.5))
    Assert.Equal("hi", SqlValue.toDisplayString (SqlValue.Text "hi"))
    Assert.Equal("<blob 3 bytes>", SqlValue.toDisplayString (SqlValue.Blob [| 1uy; 2uy; 3uy |]))
    Assert.Equal("2026-01-02", SqlValue.toDisplayString (SqlValue.Date(DateOnly(2026, 1, 2))))

    Assert.Equal(
        "2026-01-02T03:04:05.000Z",
        SqlValue.toDisplayString (SqlValue.Timestamp(DateTimeOffset(DateTime(2026, 1, 2, 3, 4, 5, DateTimeKind.Utc))))
    )

    Assert.Equal("1.5", SqlValue.toDisplayString (SqlValue.Decimal 1.5M))

    Assert.Equal(
        "11111111-1111-1111-1111-111111111111",
        SqlValue.toDisplayString (SqlValue.Uuid(Guid "11111111-1111-1111-1111-111111111111"))
    )

[<Fact>]
let ``SqlValue.coerce: Int -> Real`` () =
    match SqlValue.coerce DataType.Real (SqlValue.Int 3L) with
    | Ok(SqlValue.Real f) -> Assert.Equal(3.0, f)
    | other -> Assert.Fail(sprintf "coerce Int->Real が想定外: %A" other)

[<Fact>]
let ``SqlValue.coerce: Real は数値と真偽値を受ける`` () =
    match SqlValue.coerce DataType.Real (SqlValue.Real 2.5) with
    | Ok(SqlValue.Real f) -> Assert.Equal(2.5, f)
    | other -> Assert.Fail(sprintf "coerce Real->Real が想定外: %A" other)

    Assert.Equal(SqlValue.Real 1.0, ok (SqlValue.coerce DataType.Real (SqlValue.Bool true)))
    Assert.Equal(SqlValue.Real 0.0, ok (SqlValue.coerce DataType.Real (SqlValue.Bool false)))
    Assert.Equal(SqlValue.Real 2.5, ok (SqlValue.coerce DataType.Real (SqlValue.Decimal 2.5M)))
    expectErrorKind ErrorKind.TypeError "文字列は型エラー" (SqlValue.coerce DataType.Real (SqlValue.Text "1.0"))

[<Fact>]
let ``SqlValue.coerce: 不変換は typeError`` () =
    expectErrorKind ErrorKind.TypeError "型エラーになるはず" (SqlValue.coerce DataType.Integer (SqlValue.Text "x"))

[<Fact>]
let ``SqlValue.coerce: Boolean は 0/1 と真偽値だけ受ける`` () =
    Assert.Equal(SqlValue.Bool true, ok (SqlValue.coerce DataType.Boolean (SqlValue.Bool true)))
    Assert.Equal(SqlValue.Bool false, ok (SqlValue.coerce DataType.Boolean (SqlValue.Bool false)))
    Assert.Equal(SqlValue.Bool true, ok (SqlValue.coerce DataType.Boolean (SqlValue.Int 1L)))
    Assert.Equal(SqlValue.Bool false, ok (SqlValue.coerce DataType.Boolean (SqlValue.Int 0L)))
    Assert.Equal(SqlValue.Bool true, ok (SqlValue.coerce DataType.Boolean (SqlValue.Real 1.0)))
    Assert.Equal(SqlValue.Bool false, ok (SqlValue.coerce DataType.Boolean (SqlValue.Real 0.0)))
    expectErrorKind ErrorKind.TypeError "0/1 以外の整数は型エラー" (SqlValue.coerce DataType.Boolean (SqlValue.Int 2L))
    expectErrorKind ErrorKind.TypeError "0/1 以外の実数は型エラー" (SqlValue.coerce DataType.Boolean (SqlValue.Real 2.0))
    expectErrorKind ErrorKind.TypeError "文字列は型エラー" (SqlValue.coerce DataType.Boolean (SqlValue.Text "true"))

[<Fact>]
let ``SqlValue.coerce: Integer は整数的な値だけ受ける`` () =
    Assert.Equal(SqlValue.Int 1L, ok (SqlValue.coerce DataType.Integer (SqlValue.Bool true)))
    Assert.Equal(SqlValue.Int 0L, ok (SqlValue.coerce DataType.Integer (SqlValue.Bool false)))
    Assert.Equal(SqlValue.Int 3L, ok (SqlValue.coerce DataType.Integer (SqlValue.Real 3.0)))
    Assert.Equal(SqlValue.Int 4L, ok (SqlValue.coerce DataType.Integer (SqlValue.Decimal 4.0M)))
    expectErrorKind ErrorKind.TypeError "小数は型エラー" (SqlValue.coerce DataType.Integer (SqlValue.Real 3.5))
    expectErrorKind ErrorKind.TypeError "小数の DECIMAL は型エラー" (SqlValue.coerce DataType.Integer (SqlValue.Decimal 4.5M))
    expectErrorKind ErrorKind.TypeError "NaN は型エラー" (SqlValue.coerce DataType.Integer (SqlValue.Real System.Double.NaN))

[<Fact>]
let ``SqlValue.coerce: Decimal は有限の数値だけ受ける`` () =
    Assert.Equal(SqlValue.Decimal 2.5M, ok (SqlValue.coerce DataType.Decimal (SqlValue.Decimal 2.5M)))
    Assert.Equal(SqlValue.Decimal 2M, ok (SqlValue.coerce DataType.Decimal (SqlValue.Int 2L)))
    Assert.Equal(SqlValue.Decimal 2.5M, ok (SqlValue.coerce DataType.Decimal (SqlValue.Real 2.5)))
    Assert.Equal(SqlValue.Decimal 1.0M, ok (SqlValue.coerce DataType.Decimal (SqlValue.Bool true)))
    Assert.Equal(SqlValue.Decimal 0.0M, ok (SqlValue.coerce DataType.Decimal (SqlValue.Bool false)))
    expectErrorKind ErrorKind.TypeError "NaN は型エラー" (SqlValue.coerce DataType.Decimal (SqlValue.Real Double.NaN))

    expectErrorKind
        ErrorKind.TypeError
        "Infinity は型エラー"
        (SqlValue.coerce DataType.Decimal (SqlValue.Real Double.PositiveInfinity))

    expectErrorKind ErrorKind.TypeError "文字列は型エラー" (SqlValue.coerce DataType.Decimal (SqlValue.Text "1"))

[<Fact>]
let ``SqlValue.coerce: Text / Blob / Uuid への変換`` () =
    Assert.Equal(SqlValue.Text "42", ok (SqlValue.coerce DataType.Text (SqlValue.Int 42L)))

    match SqlValue.coerce DataType.Blob (SqlValue.Blob [| 1uy |]) with
    | Ok(SqlValue.Blob b) -> Assert.Equal<byte array>([| 1uy |], b)
    | other -> Assert.Fail(sprintf "coerce Blob->Blob が想定外: %A" other)

    match SqlValue.coerce DataType.Blob (SqlValue.Text "hi") with
    | Ok(SqlValue.Blob b) -> Assert.Equal<byte array>([| 104uy; 105uy |], b)
    | other -> Assert.Fail(sprintf "coerce Text->Blob が想定外: %A" other)

    let g = Guid "11111111-1111-1111-1111-111111111111"
    Assert.Equal(SqlValue.Uuid g, ok (SqlValue.coerce DataType.Uuid (SqlValue.Uuid g)))
    Assert.Equal(SqlValue.Uuid g, ok (SqlValue.coerce DataType.Uuid (SqlValue.Text(g.ToString "D"))))
    expectErrorKind ErrorKind.TypeError "GUID でない文字列は型エラー" (SqlValue.coerce DataType.Uuid (SqlValue.Text "not-a-guid"))
    expectErrorKind ErrorKind.TypeError "整数は UUID にできない" (SqlValue.coerce DataType.Uuid (SqlValue.Int 1L))
    expectErrorKind ErrorKind.TypeError "整数は Blob にできない" (SqlValue.coerce DataType.Blob (SqlValue.Int 1L))

[<Fact>]
let ``SqlValue.coerce: Date / Timestamp の相互変換と Text 解析`` () =
    let d = DateOnly(2026, 1, 2)
    let t = DateTimeOffset(DateTime(2026, 1, 2, 3, 4, 5, DateTimeKind.Utc))

    // TIMESTAMP -> DATE は UTC の日付、DATE -> TIMESTAMP は UTC 午夜
    Assert.Equal(SqlValue.Date d, ok (SqlValue.coerce DataType.Date (SqlValue.Timestamp t)))

    Assert.Equal(
        SqlValue.Timestamp(DateTimeOffset(DateTime(2026, 1, 2, 0, 0, 0, DateTimeKind.Utc))),
        ok (SqlValue.coerce DataType.Timestamp (SqlValue.Date d))
    )

    // 同じ型はそのまま通る
    Assert.Equal(SqlValue.Date d, ok (SqlValue.coerce DataType.Date (SqlValue.Date d)))
    Assert.Equal(SqlValue.Timestamp t, ok (SqlValue.coerce DataType.Timestamp (SqlValue.Timestamp t)))

    // Text は yyyy-MM-dd と ISO 8601 を受け付ける
    Assert.Equal(SqlValue.Date d, ok (SqlValue.coerce DataType.Date (SqlValue.Text "2026-01-02")))
    Assert.Equal(SqlValue.Timestamp t, ok (SqlValue.coerce DataType.Timestamp (SqlValue.Text "2026-01-02T03:04:05Z")))
    expectErrorKind ErrorKind.TypeError "不正な日付文字列は型エラー" (SqlValue.coerce DataType.Date (SqlValue.Text "2026/01/02"))
    expectErrorKind ErrorKind.TypeError "不正な日時文字列は型エラー" (SqlValue.coerce DataType.Timestamp (SqlValue.Text "garbage"))
    expectErrorKind ErrorKind.TypeError "整数は日付にできない" (SqlValue.coerce DataType.Date (SqlValue.Int 1L))
    expectErrorKind ErrorKind.TypeError "整数は日時にできない" (SqlValue.coerce DataType.Timestamp (SqlValue.Int 1L))

[<Fact>]
let ``SqlValue.coerce: NULL はどの型へも NULL のまま`` () =
    Assert.Equal(SqlValue.Null, ok (SqlValue.coerce DataType.Integer SqlValue.Null))
    Assert.Equal(SqlValue.Null, ok (SqlValue.coerce DataType.Text SqlValue.Null))
    Assert.Equal(SqlValue.Null, ok (SqlValue.coerce DataType.Null (SqlValue.Int 1L)))
