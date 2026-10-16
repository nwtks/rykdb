namespace RykDb.Core

/// SSTable の Bloom filter。
/// SSTable がメモリ上に載る前に「その SSTable にキーが存在し得るか」を O(1) で判定し、
/// 存在しないキーではファイル I/O そのものを省く。
/// false positive（あるのに無いと返す）は許されるが、false negative（無いのにあると返す）は許されない。
/// 構築してから読み取り専用。
/// 不変なので複数の読み取りスレッドで共有できる。
[<Struct>]
type internal BloomFilter =
    {
        /// ビット配列（1ビット = 1byte。単純化と引き換えにメモリを使う）
        Bits: byte array
        /// ハッシュ関数個数
        HashCount: int
        /// 追加された要素数（メタデータとしても保持する）
        Count: int
    }

module internal BloomFilter =
    let private ln2 = log 2.0

    /// double hashing の位置列。
    /// ハッシュ上位 32bit（h1）を起点、下位 32bit を 1..2^32-1 に正規化した値をステップ幅にする。
    /// ステップは奇数にしないと偏る。
    /// <remarks>
    /// 「% n してから偶数を +1」（読む側）と「偶数を +1 してから % n」（築く側）は本来別物だが、
    /// bitCount が必ず 8の倍数（= 偶数、bitsFor が 8単位に丸める）なので同値になる。
    /// 2つに分けておくと片方が書き換わった瞬間に false negative が出るので、ここでは 1つに畳んでいる。
    /// </remarks>
    let private positionsFor (bitCount: int) (hashCount: int) (hash: uint64) =
        let n = uint64 bitCount
        let h1 = (hash >>> 32) % n
        let raw = hash % 4294967295UL + 1UL
        let step = if raw % 2UL = 0UL then (raw + 1UL) % n else raw % n

        [|
            for i in 0 .. hashCount - 1 do
                yield int ((h1 + uint64 i * step) % n)
        |]

    /// キーが含まれ得るなら true（false positive を許す）。
    let mightContain (filter: BloomFilter) (keyHash: uint64) =
        positionsFor filter.Bits.Length filter.HashCount keyHash
        |> Array.forall (fun pos -> filter.Bits.[pos / 8] &&& (1uy <<< pos % 8) <> 0uy)

    /// キー列から filter を構築する（ハッシュはキーから導出）。
    let ofKeys (keys: byte array seq) (expectedCount: int) (falsePositiveRate: float) =
        let bitsFor (expectedCount: int) (falsePositiveRate: float) =
            if expectedCount <= 0 then
                8
            else
                let m = -float expectedCount * log falsePositiveRate / (ln2 * ln2)
                max 8 (int (ceil m / 8.0) * 8)

        let hashesFor (expectedCount: int) (bitCount: int) =
            if expectedCount <= 0 then
                1
            else
                max 1 (int (ceil (float bitCount / float expectedCount) * ln2))

        let ofHashes (expectedCount: int) (falsePositiveRate: float) (hashes: uint64 seq) =
            let bitCount = bitsFor expectedCount falsePositiveRate
            let hashCount = hashesFor expectedCount bitCount

            let allPositions =
                hashes |> Seq.toArray |> Array.collect (positionsFor bitCount hashCount)

            let bits = Array.zeroCreate<byte> bitCount

            for pos in allPositions do
                bits.[pos / 8] <- bits.[pos / 8] ||| (1uy <<< pos % 8)

            {
                Bits = bits
                HashCount = hashCount
                Count = allPositions.Length
            }

        keys
        |> Seq.map (Hashing.hashBytes >> Hashing.mix)
        |> ofHashes expectedCount falsePositiveRate

    let serialize (filter: BloomFilter) =
        Writer.build
            [
                Writer.writeByte 1uy
                Writer.writeByte (byte filter.HashCount)
                Writer.varint (uint64 filter.Count)
                Writer.varint (uint64 filter.Bits.Length)
                Writer.writeBytes filter.Bits
            ]

    let deserialize (data: byte array) =
        result {
            let c = Cursor.create data
            let! version, c = Reader.readByte c

            if version <> 1uy then
                return! err (sprintf "BloomFilter: 未対応のバージョン %d" version |> DbError.corruption)

            let! hashCount, c = Reader.readByte c
            let! count, c = Reader.readVarint c
            let! bitsLength, c = Reader.readVarint c

            if bitsLength > uint64 System.Int32.MaxValue then
                return! err (DbError.corruption "BloomFilter: ビット数が異常です")

            let! bits, _ = Reader.readBytes (int bitsLength) c

            return
                {
                    Bits = bits
                    HashCount = int hashCount
                    Count = int count
                }
        }
