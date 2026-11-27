// ===========================================================================
// SstableWriter.fs のテスト（書き出し）
// ===========================================================================

module RykDb.SstableWriterTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``SstableWriter.writeTo: 入力の seq は 1 回だけ列挙する`` () =
    let items =
        [|
            for i in 1..5 -> SstEntry.make (intKey i) (uint64 i) (Some [| SqlValue.Int(int64 i) |])
        |]

    let mutable enumerations = 0

    let once =
        seq {
            enumerations <- enumerations + 1

            if enumerations > 1 then
                failwith "入力が 2回列挙されています"

            yield! items
        }

    use stream = new System.IO.MemoryStream()
    let info = ok (SstableWriter.writeTo stream 0.01 items.Length once)
    Assert.Equal(1, enumerations)
    Assert.Equal<int64>(5L, info.EntryCount)
    Assert.Equal<byte array>(intKey 1, info.FirstKey)
    Assert.Equal<byte array>(intKey 5, info.LastKey)
    Assert.Equal<uint64>(5UL, info.MaxSeqNo)
    // 書いたバイト数と Size が食い違わない
    Assert.Equal<int64>(int64 (stream.ToArray().Length), info.Size)

[<Fact>]
let ``SstableWriter.writeTo: 見積りより実件数が多くても Bloom は false negative を出さない`` () =
    let items =
        [|
            for i in 1..200 -> SstEntry.make (intKey i) (uint64 i) (Some [| SqlValue.Int(int64 i) |])
        |]

    use stream = new System.IO.MemoryStream()
    // 見積り 1 件で 200 件を書く（BloomBuilder は件数超過を許容する）。
    let info = ok (SstableWriter.writeTo stream 0.01 1 items)
    let bloom = expectSome "Bloom" info.Bloom

    items
    |> Array.iter (fun e ->
        let hash = Hashing.hashBytes e.Key |> Hashing.mix
        Assert.True(BloomFilter.mightContain bloom hash, "Bloom の false negative"))
