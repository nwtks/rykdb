namespace RykDb.Core

/// SSTable の Bloom filter。
/// 「その SSTable にキーが存在し得るか」を O(1) で判定し、存在しなければファイル I/O を省く。
/// false positive は許すが false negative は許さない。
/// 構築後は不変で、複数の読み取りで共有できる。
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
    /// 自然対数 2（ビット数・ハッシュ数の式で使う）。
    let private ln2 = log 2.0

    /// double hashing の起点とステップ。
    /// 起点はハッシュ上位 32bit、ステップは下位 32bit を 1..2^32-1 に正規化した奇数。
    /// <remarks>
    /// 読み取り側と構築側で `% n` と偶数 +1 の順序が違っても、ビット数が必ず 8 の倍数（= 偶数）なので同値になる。
    /// 2つに分けると片方の変更で false negative が出るため 1つに畳んでいる。
    /// </remarks>
    let private firstAndStep (bitCount: int) (hash: uint64) =
        let n = uint64 bitCount
        let h1 = (hash >>> 32) % n
        let raw = hash % 4294967295UL + 1UL
        let step = if raw % 2UL = 0UL then (raw + 1UL) % n else raw % n
        h1, step

    /// 期待要素数と誤検出率から必要なビット数を求める（8ビット単位に丸める）。
    let private bitsFor (expectedCount: int) (falsePositiveRate: float) =
        if expectedCount <= 0 then
            8
        else
            let m = -float expectedCount * log falsePositiveRate / (ln2 * ln2)
            let words = min (ceil (m / 8.0)) (float (System.Int32.MaxValue / 8))
            max 8 (int words * 8)

    /// ビット数と期待要素数からハッシュ関数数を求める。
    let private hashesFor (expectedCount: int) (bitCount: int) =
        if expectedCount <= 0 then
            1
        else
            max 1 (int (ceil (float bitCount / float expectedCount) * ln2))

    /// i 番目のハッシュ位置。
    let private positionAt (bitCount: int) (h1: uint64) (step: uint64) (i: int32) =
        int ((h1 + uint64 i * step) % uint64 bitCount)

    /// 全ハッシュ位置のビットが立っているかを順に検査する。
    [<TailCall>]
    let rec private check (h1: uint64) (step: uint64) (i: int) (filter: BloomFilter) =
        if i >= filter.HashCount then
            true
        else
            let pos = positionAt filter.BitCount h1 step i

            if filter.Bits.[pos / 8] &&& (1uy <<< pos % 8) <> 0uy then
                check h1 step (i + 1) filter
            else
                false

    /// キーが含まれ得るなら true（false positive を許す）。
    /// 位置の配列を作らない（ポイント検索ごとの割り当てを避ける）。
    let mightContain (filter: BloomFilter) (keyHash: uint64) =
        let h1, step = firstAndStep filter.BitCount keyHash
        check h1 step 0 filter

    /// 逐次追加できる Bloom filter ビルダ。
    /// 期待要素数と誤検出率からビット数・ハッシュ数を決め、`Add` でキーを足して `Build` で確定する。
    /// 件数が期待値を大きく超えると誤検出率が悪化するが、
    /// false negative は出ないので正しさには影響しない（遅くなるだけ）。
    [<Sealed>]
    type BloomBuilder(expectedCount: int, falsePositiveRate: float) =
        let bitCount = bitsFor expectedCount falsePositiveRate
        let hashCount = hashesFor expectedCount bitCount
        let bits = Array.zeroCreate<byte>(bitCount / 8)
        let mutable hashes = 0

        /// キーを 1つ追加する（`mightContain` に渡すハッシュと同じ導出）。
        member _.Add(key: byte array) =
            let hash = key |> Hashing.hashBytes |> Hashing.mix
            let h1, step = firstAndStep bitCount hash

            for i in 0 .. hashCount - 1 do
                let pos = positionAt bitCount h1 step i
                bits.[pos / 8] <- bits.[pos / 8] ||| (1uy <<< pos % 8)

            hashes <- hashes + hashCount

        /// 確定した filter。
        member _.Build() =
            {
                Bits = bits
                BitCount = bitCount
                HashCount = hashCount
                Count = hashes
            }

    /// 直列化のバージョン（1ビット = 1ビット）。
    [<Literal>]
    let private Version = 2uy

    /// filter をバイト列にする。
    let serialize (filter: BloomFilter) =
        Writer.build
            [
                Writer.writeByte Version
                byte filter.HashCount |> Writer.writeByte
                uint64 filter.Count |> Writer.varint
                uint64 filter.BitCount |> Writer.varint
                Writer.writeBytes filter.Bits
            ]

    /// バイト列から filter を復元する。
    /// 壊れた入力は `CorruptionError`。
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
