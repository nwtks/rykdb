// ===========================================================================
// Types.fs（3 値論理・値の比較）
// ===========================================================================

module RykDb.TypesTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

// ---------------------------------------------------------------------------
// 3 値論理
// ---------------------------------------------------------------------------

[<Fact>]
let ``3値論理: AND/OR`` () =
    Assert.Equal(TruthValue.False, TruthValue.and2 TruthValue.False TruthValue.Unknown)
    Assert.Equal(TruthValue.Unknown, TruthValue.and2 TruthValue.True TruthValue.Unknown)
    Assert.Equal(TruthValue.True, TruthValue.and2 TruthValue.True TruthValue.True)
    Assert.Equal(TruthValue.True, TruthValue.or2 TruthValue.True TruthValue.Unknown)
    Assert.Equal(TruthValue.Unknown, TruthValue.or2 TruthValue.False TruthValue.Unknown)

[<Fact>]
let ``比較: NaN は NaN と等しく、他の数値より大きい`` () =
    let nan = SqlValue.vReal Double.NaN
    let one = SqlValue.vReal 1.0
    Assert.Equal(0, (SqlValue.compareValues nan nan) |> okOr 1)
    Assert.True((SqlValue.compareValues nan one) |> okOr 0 > 0)
    Assert.True((SqlValue.compareValues one nan) |> okOr 0 < 0)

// ---------------------------------------------------------------------------
// 値の正規化・比較・型タグ
// ---------------------------------------------------------------------------

[<Fact>]
let ``値: NaN は正の quiet NaN に正規化される`` () =
    // .NET の Double.NaN は符号ビットが立った負の NaN。そのままキーにすると
    // 順序保持の前提が崩れるため、単一のビットパターンへ正規化する。
    let negativeNaN = BitConverter.Int64BitsToDouble(0xFFF8000000000000L)

    match SqlValue.vReal negativeNaN with
    | SqlValue.Real f -> Assert.Equal(SqlValue.CanonicalNaNBits, BitConverter.DoubleToInt64Bits f)
    | other -> Assert.Fail(sprintf "REAL ではありません: %A" other)

    // 正規化後は NaN 同士が等しく、REAL の中では最大
    Assert.Equal(0, ok (SqlValue.compareValues (SqlValue.vReal negativeNaN) (SqlValue.vReal Double.NaN)))
    Assert.True(ok (SqlValue.compareValues (SqlValue.vReal Double.NaN) (SqlValue.vReal 1.0)) > 0)

[<Fact>]
let ``値: 比較できない型の組み合わせは typeError`` () =
    expectErrorKind ErrorKind.TypeError "型エラーになるはず" (SqlValue.compareValues (SqlValue.Int 1L) (SqlValue.Text "1"))

    expectErrorKind ErrorKind.TypeError "型エラーになるはず" (SqlValue.compareValues (SqlValue.Bool true) (SqlValue.Int 1L))

[<Fact>]
let ``値: 数値 3 種と日時は相互に比較できる`` () =
    Assert.True(ok (SqlValue.compareValues (SqlValue.Int 2L) (SqlValue.Real 2.5)) < 0)
    Assert.Equal(0, ok (SqlValue.compareValues (SqlValue.Real 3.0) (SqlValue.Decimal 3M)))
    Assert.True(ok (SqlValue.compareValues (SqlValue.Decimal 1M) (SqlValue.Int 2L)) < 0)

    // DATE は UTC 午夜の TIMESTAMP として比較する
    let d = DateOnly(2026, 1, 2)
    let t = DateTimeOffset(DateTime(2026, 1, 2, 0, 0, 0, DateTimeKind.Utc))
    Assert.Equal(0, ok (SqlValue.compareValues (SqlValue.Date d) (SqlValue.Timestamp t)))
    Assert.True(ok (SqlValue.compareValues (SqlValue.Date d) (SqlValue.Timestamp(t.AddDays 1.0))) < 0)

[<Fact>]
let ``値: asNumeric は NaN と無限大を拒否する`` () =
    Assert.True((SqlValue.asNumeric (SqlValue.Real Double.NaN)).IsNone)
    Assert.True((SqlValue.asNumeric (SqlValue.Real Double.PositiveInfinity)).IsNone)
    Assert.True((SqlValue.asNumeric (SqlValue.Real Double.NegativeInfinity)).IsNone)
    Assert.Equal<decimal option>(Some 2.5M, SqlValue.asNumeric (SqlValue.Real 2.5))
    Assert.Equal<decimal option>(Some 3M, SqlValue.asNumeric (SqlValue.Int 3L))
    Assert.True((SqlValue.asNumeric (SqlValue.Text "3")).IsNone)
    Assert.True((SqlValue.asNumeric SqlValue.Null).IsNone)

[<Fact>]
let ``値: 型タグの往復と未知タグの扱い`` () =
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
