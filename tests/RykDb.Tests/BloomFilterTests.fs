// ===========================================================================
// BloomFilter.fs
// ===========================================================================

module RykDb.BloomFilterTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``BloomFilter.mightContain: 追加したキーはすべて maybe present`` () =
    let n = 2_000
    let keys = Array.init n intKey
    let filter = BloomFilter.ofKeys keys n 0.01

    for k in keys do
        let h = Hashing.hashBytes k |> Hashing.mix
        Assert.True(BloomFilter.mightContain filter h, "追加したキーが false negative になった")

[<Fact>]
let ``BloomFilter.mightContain: 追加していないキーはほぼ「なし」になる`` () =
    let n = 1_000
    let p = 0.01
    let keys = Array.init n intKey
    let filter = BloomFilter.ofKeys keys n p

    // 別キー群を用意して、誤検出率が設計値を下回ることを確認する
    let others = Array.init 2_000 (fun i -> intKey (i + 10_000_000))

    let falsePositives =
        others
        |> Array.filter (fun k -> BloomFilter.mightContain filter (Hashing.hashBytes k |> Hashing.mix))
        |> Array.length

    let rate = float falsePositives / float others.Length
    Assert.True(rate <= p * 2.0, sprintf "誤検出率が高すぎます: %.4f (設計 %.2f)" rate p)

[<Fact>]
let ``BloomFilter.mightContain: 空のフィルタは何も含まない`` () =
    let filter = BloomFilter.ofKeys [] 0 0.01
    Assert.Equal(0, filter.Count)

    for i in 0..99 do
        let h = Hashing.hashBytes (intKey i) |> Hashing.mix
        Assert.False(BloomFilter.mightContain filter h, "空のフィルタが値を含んでいる")

[<Fact>]
let ``BloomFilter.serialize: 直列化の往復`` () =
    let keys = Array.init 5_000 intKey
    let filter = BloomFilter.ofKeys keys 5_000 0.01
    let restored = ok (BloomFilter.deserialize (BloomFilter.serialize filter))
    Assert.Equal(filter.Count, restored.Count)
    Assert.Equal(filter.HashCount, restored.HashCount)
    Assert.Equal(filter.Bits.Length, restored.Bits.Length)

    for k in keys do
        Assert.True(BloomFilter.mightContain restored (Hashing.hashBytes k |> Hashing.mix))

[<Fact>]
let ``BloomFilter.serialize: 直列化は決定的で、往復後も同じバイト列になる`` () =
    let keys = Array.init 500 intKey
    let a = BloomFilter.serialize (BloomFilter.ofKeys keys 500 0.01)
    let b = BloomFilter.serialize (BloomFilter.ofKeys keys 500 0.01)
    Assert.Equal<byte array>(a, b)

    // 往復してからもう一度直列化しても同じ結果（メタデータの欠落がない）
    let restored = ok (BloomFilter.deserialize a)
    Assert.Equal<byte array>(a, BloomFilter.serialize restored)

[<Fact>]
let ``BloomFilter.deserialize: 壊れたデータは CorruptionError`` () =
    expectErrorKind ErrorKind.CorruptionError "壊れたデータはエラーになるはず" (BloomFilter.deserialize [| 9uy; 3uy |])

[<Fact>]
let ``BloomFilter.deserialize: ビット数が Int32 を超えるデータは CorruptionError`` () =
    // version=1, hashCount=1, count=0, bitsLength=Int32.MaxValue+1
    let data =
        Writer.build
            [
                Writer.writeByte 1uy
                Writer.writeByte 1uy
                Writer.varint 0UL
                Writer.varint (uint64 System.Int32.MaxValue + 1UL)
            ]

    expectErrorKind ErrorKind.CorruptionError "ビット数が異常" (BloomFilter.deserialize data)

[<Fact>]
let ``BloomFilter.deserialize: ビット数 0 とハッシュ数 0 は CorruptionError`` () =
    let zeroBits =
        Writer.build
            [
                Writer.writeByte 1uy
                Writer.writeByte 1uy
                Writer.varint 0UL
                Writer.varint 0UL
            ]

    expectErrorKind ErrorKind.CorruptionError "ビット数 0 は除算で落ちる前に弾く" (BloomFilter.deserialize zeroBits)

    let zeroHashes =
        Writer.build
            [
                Writer.writeByte 1uy
                Writer.writeByte 0uy
                Writer.varint 0UL
                Writer.varint 8UL
                Writer.writeBytes [| 0uy |]
            ]

    expectErrorKind ErrorKind.CorruptionError "ハッシュ数 0 は弾く" (BloomFilter.deserialize zeroHashes)
