// ===========================================================================
// SstableFormat.fs のテスト（書き出し）
// ===========================================================================

module RykDb.SstableFormatTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``SstableFormat.writeTo: 入力の seq は 1 回だけ列挙する`` () =
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
    let info = ok (SstableFormat.writeTo stream 0.01 once)

    Assert.Equal(1, enumerations)
    Assert.Equal<int64>(5L, info.EntryCount)
    Assert.Equal<byte array>(intKey 1, info.FirstKey)
    Assert.Equal<byte array>(intKey 5, info.LastKey)
    Assert.Equal<uint64>(5UL, info.MaxSeqNo)
    // 書いたバイト数と Size が食い違わない
    Assert.Equal<int64>(int64 (stream.ToArray().Length), info.Size)
