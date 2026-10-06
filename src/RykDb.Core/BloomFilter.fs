namespace RykDb.Core

open System

/// SSTable の Bloom filter。
/// SSTable がメモリ上に載る前に「その SSTable にキーが存在し得るか」を
/// O(1) で判定し、存在しないキーではファイル I/O そのものを省く。
/// false positive（あるのに無いと返す）は許されるが、false negative（無いのにあると返す）は許されない。
/// 構築してから読み取り専用。不変なので複数の読み取りスレッドで共有できる。
[<Struct>]
type internal BloomFilter =
    {
        /// ビット配列（1 ビット = 1 byte。単純化と引き換えにメモリを使う）
        Bits: byte array
        /// ハッシュ関数個数
        HashCount: int
        /// 追加された要素数（メタデータとしても保持する）
        Count: int
    }

module internal BloomFilter =
    let private bitsFor (expectedCount: int) (falsePositiveRate: float) =
        if expectedCount <= 0 then
            8
        else
            let ln2 = log 2.0
            let m = -float expectedCount * log falsePositiveRate / (ln2 * ln2)
            max 8 (int (ceil m / 8.0) * 8)

    let private hashesFor (expectedCount: int) (bitCount: int) =
        if expectedCount <= 0 then
            1
        else
            max 1 (int (ceil (float bitCount / float expectedCount) * log 2.0))

    let private positions (filter: BloomFilter) (hash: uint64) =
        let n = uint64 filter.Bits.Length
        let h1 = (hash >>> 32) % n
        let h2 = (hash % 4294967295UL + 1UL) % n
        let step = if h2 % 2UL = 0UL then (h2 + 1UL) % n else h2

        [|
            for i in 0 .. filter.HashCount - 1 do
                yield int ((h1 + uint64 i * step) % n)
        |]

    /// キーが含まれ得るなら true（false positive を許す）。
    let mightContain (filter: BloomFilter) (keyHash: uint64) =
        positions filter keyHash
        |> Array.forall (fun pos -> filter.Bits.[pos / 8] &&& (1uy <<< pos % 8) <> 0uy)

    let private ofHashes (expectedCount: int) (falsePositiveRate: float) (hashes: uint64 seq) =
        let bitCount = bitsFor expectedCount falsePositiveRate
        let hashCount = hashesFor expectedCount bitCount
        let n = uint64 bitCount

        let positionsOf h =
            let h1 = (h >>> 32) % n
            let raw = h % 4294967295UL + 1UL
            let step = if raw % 2UL = 0UL then raw + 1UL else raw

            [|
                for i in 0 .. hashCount - 1 do
                    yield int ((h1 + uint64 i * step) % n)
            |]

        let positions = hashes |> Seq.toArray |> Array.collect positionsOf
        let bits = Array.zeroCreate<byte> bitCount

        for pos in positions do
            bits.[pos / 8] <- bits.[pos / 8] ||| (1uy <<< pos % 8)

        {
            Bits = bits
            HashCount = hashCount
            Count = positions.Length
        }

    /// キー列から filter を構築する（ハッシュはキーから導出）。
    let ofKeys (keys: byte array seq) (expectedCount: int) (falsePositiveRate: float) =
        keys
        |> Seq.map (fun k -> Hashing.hashBytes k |> Hashing.mix)
        |> ofHashes expectedCount falsePositiveRate

    // -----------------------------------------------------------------------
    // 直列化
    // -----------------------------------------------------------------------

    let serialize (filter: BloomFilter) =
        let withVersion = Writer.empty |> Writer.writeByte 1uy // フォーマットバージョン
        let withHashes = withVersion |> Writer.writeByte (byte filter.HashCount)
        let withCount = withHashes |> Writer.varint (uint64 filter.Count)
        let withBits = withCount |> Writer.varint (uint64 filter.Bits.Length)
        withBits |> Writer.writeBytes filter.Bits |> Writer.toArray

    let deserialize (data: byte array) =
        result {
            let c = Cursor.create data
            let! version, c = Read.readByte c

            if version <> 1uy then
                return! Error(DbError.corruption (sprintf "BloomFilter: 未対応のバージョン %d" version))

            let! hashCount, c = Read.readByte c
            let! count, c = Read.readVarint c
            let! bits, c = Read.readVarint c

            if bits > uint64 Int32.MaxValue then
                return! Error(DbError.corruption "BloomFilter: ビット数が異常です")

            let! bytes, _ = Read.readBytes (int bits) c

            return
                {
                    Bits = bytes
                    HashCount = int hashCount
                    Count = int count
                }
        }
