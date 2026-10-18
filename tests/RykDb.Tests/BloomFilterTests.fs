// ===========================================================================
// BloomFilter.fs
// ===========================================================================

module RykDb.BloomFilterTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``BloomFilter: 追加したキーはすべて maybe present`` () =
    let n = 20_000
    let keys = Array.init n intKey
    let filter = BloomFilter.ofKeys keys n 0.01

    for k in keys do
        let h = Hashing.hashBytes k |> Hashing.mix
        Assert.True(BloomFilter.mightContain filter h, "追加したキーが false negative になった")

[<Fact>]
let ``BloomFilter: 追加していないキーはほぼ「なし」になる`` () =
    let n = 10_000
    let p = 0.01
    let keys = Array.init n intKey
    let filter = BloomFilter.ofKeys keys n p

    // 別キー群を用意して、誤検出率が設計値を下回ることを確認する
    let others = Array.init 20_000 (fun i -> intKey (i + 10_000_000))

    let falsePositives =
        others
        |> Array.filter (fun k -> BloomFilter.mightContain filter (Hashing.hashBytes k |> Hashing.mix))
        |> Array.length

    let rate = float falsePositives / float others.Length
    Assert.True(rate <= p * 2.0, sprintf "誤検出率が高すぎます: %.4f (設計 %.2f)" rate p)

[<Fact>]
let ``BloomFilter: 空のフィルタは何も含まない`` () =
    let filter = BloomFilter.ofKeys [] 0 0.01
    Assert.Equal(0, filter.Count)

    for i in 0..99 do
        let h = Hashing.hashBytes (intKey i) |> Hashing.mix
        Assert.False(BloomFilter.mightContain filter h, "空のフィルタが値を含んでいる")

[<Fact>]
let ``BloomFilter: 直列化の往復`` () =
    let keys = Array.init 5_000 intKey
    let filter = BloomFilter.ofKeys keys 5_000 0.01
    let restored = ok (BloomFilter.deserialize (BloomFilter.serialize filter))
    Assert.Equal(filter.Count, restored.Count)
    Assert.Equal(filter.HashCount, restored.HashCount)
    Assert.Equal(filter.Bits.Length, restored.Bits.Length)

    for k in keys do
        Assert.True(BloomFilter.mightContain restored (Hashing.hashBytes k |> Hashing.mix))

[<Fact>]
let ``BloomFilter: 直列化は決定的で、往復後も同じバイト列になる`` () =
    let keys = Array.init 500 intKey
    let a = BloomFilter.serialize (BloomFilter.ofKeys keys 500 0.01)
    let b = BloomFilter.serialize (BloomFilter.ofKeys keys 500 0.01)
    Assert.Equal<byte array>(a, b)

    // 往復してからもう一度直列化しても同じ結果（メタデータの欠落がない）
    let restored = ok (BloomFilter.deserialize a)
    Assert.Equal<byte array>(a, BloomFilter.serialize restored)

[<Fact>]
let ``BloomFilter: 壊れたデータは CorruptionError`` () =
    let keys = Array.init 100 intKey
    let _ = BloomFilter.serialize (BloomFilter.ofKeys keys 100 0.01)
    expectErrorKind ErrorKind.CorruptionError "壊れたデータはエラーになるはず" (BloomFilter.deserialize [| 9uy; 3uy |])
