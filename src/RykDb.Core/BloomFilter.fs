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
        /// ビット配列（1ビット = 1ビット。長さは BitCount / 8）
        Bits: byte array
        /// ビット数（Bits.Length * 8）
        BitCount: int
        /// ハッシュ関数個数
        HashCount: int
        /// 追加された要素数（メタデータとしても保持する）
        Count: int
    }

module internal BloomFilter =
    let private ln2 = log 2.0

    /// double hashing の起点とステップ。
    /// ハッシュ上位 32bit（h1）を起点、下位 32bit を 1..2^32-1 に正規化した値をステップ幅にする。
    /// ステップは奇数にしないと偏る。
    /// <remarks>
    /// 「% n してから偶数を +1」（読む側）と「偶数を +1 してから % n」（築く側）は本来別物だが、
    /// bitCount が必ず 8の倍数（= 偶数、bitsFor が 8単位に丸める）なので同値になる。
    /// 2つに分けておくと片方が書き換わった瞬間に false negative が出るので、ここでは 1つに畳んでいる。
    /// </remarks>
    let private firstAndStep (bitCount: int) (hash: uint64) =
        let n = uint64 bitCount
        let h1 = (hash >>> 32) % n
        let raw = hash % 4294967295UL + 1UL
        let step = if raw % 2UL = 0UL then (raw + 1UL) % n else raw % n
        h1, step

    /// i 番目のハッシュ位置。
    let private positionAt (bitCount: int) (h1: uint64) (step: uint64) i =
        int ((h1 + uint64 i * step) % uint64 bitCount)

    /// キー列から filter を構築する（ハッシュはキーから導出）。
    let ofKeys (expectedCount: int) (falsePositiveRate: float) =
        let bitsFor expectedCount falsePositiveRate =
            if expectedCount <= 0 then
                8
            else
                let m = -float expectedCount * log falsePositiveRate / (ln2 * ln2)
                max 8 (int (ceil m / 8.0) * 8)

        let hashesFor expectedCount bitCount =
            if expectedCount <= 0 then
                1
            else
                max 1 (int (ceil (float bitCount / float expectedCount) * ln2))

        let ofHashes expectedCount falsePositiveRate hashes =
            let bitCount = bitsFor expectedCount falsePositiveRate
            let hashCount = hashesFor expectedCount bitCount
            let bits = Array.zeroCreate<byte>(bitCount / 8)

            let addHash count hash =
                let h1, step = firstAndStep bitCount hash

                for i in 0 .. hashCount - 1 do
                    let pos = positionAt bitCount h1 step i
                    bits.[pos / 8] <- bits.[pos / 8] ||| (1uy <<< pos % 8)

                count + hashCount

            let count = hashes |> Seq.fold addHash 0

            {
                Bits = bits
                BitCount = bitCount
                HashCount = hashCount
                Count = count
            }

        Seq.map (Hashing.hashBytes >> Hashing.mix)
        >> ofHashes expectedCount falsePositiveRate

    /// キーが含まれ得るなら true（false positive を許す）。
    /// 位置の配列を作らない（ポイント検索ごとの割り当てを避ける）。
    let mightContain (filter: BloomFilter) (keyHash: uint64) =
        let h1, step = firstAndStep filter.BitCount keyHash

        let rec check i =
            if i >= filter.HashCount then
                true
            else
                let pos = positionAt filter.BitCount h1 step i

                if filter.Bits.[pos / 8] &&& (1uy <<< pos % 8) <> 0uy then
                    check (i + 1)
                else
                    false

        check 0

    /// 直列化のバージョン（1ビット = 1ビット）。
    [<Literal>]
    let private Version = 2uy

    let serialize (filter: BloomFilter) =
        Writer.build
            [
                Writer.writeByte Version
                byte filter.HashCount |> Writer.writeByte
                uint64 filter.Count |> Writer.varint
                uint64 filter.BitCount |> Writer.varint
                Writer.writeBytes filter.Bits
            ]

    let deserialize (data: byte array) =
        result {
            let c = Cursor.create data
            let! version, c = Reader.readByte c

            if version <> Version then
                return! err (sprintf "BloomFilter: 未対応のバージョン %d" version |> DbError.corruption)

            let! hashCount, c = Reader.readByte c
            let! count, c = Reader.readVarint c
            let! bitCount, c = Reader.readVarint c

            if bitCount = 0UL || bitCount > uint64 System.Int32.MaxValue then
                return! err (DbError.corruption "BloomFilter: ビット数が異常です")

            if bitCount % 8UL <> 0UL then
                return! err (DbError.corruption "BloomFilter: ビット数が 8 の倍数ではありません")

            if hashCount = 0uy then
                return! err (DbError.corruption "BloomFilter: ハッシュ数が 0 です")

            if count > uint64 System.Int32.MaxValue then
                return! err (DbError.corruption "BloomFilter: 要素数が大きすぎます")

            let! bits, c = Reader.readBytes (int (bitCount / 8UL)) c

            if not (Cursor.atEnd c) then
                return! err (DbError.corruption "BloomFilter: 末尾に余分なデータがあります")

            return
                {
                    Bits = bits
                    BitCount = int bitCount
                    HashCount = int hashCount
                    Count = int count
                }
        }
