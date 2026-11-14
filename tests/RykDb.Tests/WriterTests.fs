// ===========================================================================
// Writer.fs（不変のチャンク列による書き出し）
// ===========================================================================

module RykDb.WriterTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Writer: writeByte / readByte の往復`` () =
    let w = Writer.Empty |> Writer.writeByte 42uy |> Writer.toArray
    let c = Cursor.create w
    let b, c2 = ok (Reader.readByte c)
    Assert.Equal(42uy, b)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: writeBytes / readBytes の往復`` () =
    let bs = [| 1uy; 2uy; 3uy |]
    let w = Writer.Empty |> Writer.writeBytes bs |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readBytes 3 c)
    Assert.Equal<byte array>(bs, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: bool / readBool の往復`` () =
    let w = Writer.Empty |> Writer.bool true |> Writer.bool false |> Writer.toArray
    let c = Cursor.create w
    let b1, c2 = ok (Reader.readBool c)
    let b2, c3 = ok (Reader.readBool c2)
    Assert.True b1
    Assert.False b2
    Assert.True(Cursor.atEnd c3)

[<Fact>]
let ``Writer: varint の往復`` () =
    for v in [ 0UL; 1UL; 127UL; 128UL; 16383UL; 16384UL; UInt64.MaxValue ] do
        let w = Writer.Empty |> Writer.varint v |> Writer.toArray
        let c = Cursor.create w
        let got, c2 = ok (Reader.readVarint c)
        Assert.Equal(v, got)
        Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: int64LE / readInt64LE の往復`` () =
    let v = 0x0123456789ABCDEFL
    let w = Writer.Empty |> Writer.int64LE v |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readInt64LE c)
    Assert.Equal(v, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: uint32LE / readUInt32LE の往復`` () =
    let v = 0x89ABCDEFu
    let w = Writer.Empty |> Writer.uint32LE v |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readUInt32LE c)
    Assert.Equal(v, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: lengthPrefixed / readLengthPrefixed の往復`` () =
    let bs = [| 9uy; 8uy; 7uy |]
    let w = Writer.Empty |> Writer.lengthPrefixed bs |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readLengthPrefixed c)
    Assert.Equal<byte array>(bs, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: str / readStr の往復`` () =
    let s = "hello"
    let w = Writer.Empty |> Writer.str s |> Writer.toArray
    let c = Cursor.create w
    let got, c2 = ok (Reader.readStr c)
    Assert.Equal(s, got)
    Assert.True(Cursor.atEnd c2)

[<Fact>]
let ``Writer: build はステップを左から適用する`` () =
    let w = Writer.build [ Writer.writeByte 1uy; Writer.writeByte 2uy ]
    Assert.Equal<byte array>([| 1uy; 2uy |], w)

[<Fact>]
let ``Writer: 多数のチャンクを積んでも書き込み順に並ぶ`` () =
    // `Chunks` は逆順に積まれる。`toArray` の末尾からの埋め戻しが崩れると
    // バイト列が反転・混在するので、チャンク種を混ぜて順序を固定する。
    let steps =
        [
            for i in 0..63 ->
                match i % 4 with
                | 0 -> Writer.writeByte (byte i)
                | 1 -> Writer.varint (uint64 i) // 0..63 は 1 バイト
                | 2 -> Writer.uint32LE (uint32 i)
                | _ -> Writer.writeBytes [| byte i; byte (i + 1) |]
        ]

    let expected =
        [
            for i in 0..63 ->
                match i % 4 with
                | 0 -> [| byte i |]
                | 1 -> [| byte i |]
                | 2 -> [| byte i; 0uy; 0uy; 0uy |] // リトルエンディアンは下位バイトが先頭
                | _ -> [| byte i; byte (i + 1) |]
        ]
        |> Array.concat

    Assert.Equal<byte array>(expected, Writer.build steps)
