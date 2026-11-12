namespace RykDb.Core

// System.Decimal は IDisposable ではないが F# が IDisposable を継承した構造体と誤認して FS0760 を出す。
// decimal の再構築はこの形が唯一素直なので範囲を限定して抑制する。
#nowarn "760"

/// Writer のチャンク。
/// 1バイト・固定長整数を配列にせず値のまま保持し、
/// 最後の `toArray` で 1本の配列へ書き込む。
type internal WriterChunk =
    | Bytes of byte array
    | Slice of byte array * int * int
    | Byte of byte
    | UInt64LE of uint64
    | UInt64BE of uint64
    | UInt32LE of uint32
    | Varint of uint64

/// 書き出し: Writer
/// 可変バッファではなく「不変のチャンク列」で表現する。
/// 追加操作は新しい Writer を返すので、途中で失敗しても中途半端な状態は残らない。
/// 最後に一度だけ配列へ平坦化する。
type internal Writer = { Chunks: WriterChunk list }

module internal Writer =
    let Empty = { Chunks = [] }

    let private add (chunk: WriterChunk) (w: Writer) = { Chunks = chunk :: w.Chunks }

    let writeByte (b: byte) (w: Writer) = add (Byte b) w

    /// バイト列を追加する。並びは保たれる。
    let writeBytes (bs: byte array) (w: Writer) =
        if bs.Length = 0 then w else add (Bytes bs) w

    let bool (v: bool) = add (Byte(if v then 1uy else 0uy))

    /// 符号なし LEB128（順序保持ではない）。
    let varint (v: uint64) = add (Varint v)

    /// リトルエンディアン 8バイト。
    let int64LE (v: int64) = add (UInt64LE(uint64 v))

    /// リトルエンディアン 4バイト（符号なし）。
    /// WAL のフレームヘッダや SSTable の Footer など固定長フィールド用。
    let uint32LE (v: uint32) = add (UInt32LE v)

    /// ビッグエンディアン 8バイト（符号なし）。
    /// 順序保持符号化で使う。
    let uint64BE (v: uint64) = add (UInt64BE v)

    /// 長さ付きバイト列（varint 長 + 実体）。
    let lengthPrefixed (bs: byte array) =
        varint (uint64 bs.Length) >> writeBytes bs

    /// UTF-8 文字列を長さ付きで追加する。
    let str (s: string) =
        lengthPrefixed (System.Text.Encoding.UTF8.GetBytes s)

    let rec writeVarintTo (out: byte array) (pos: int) (v: uint64) =
        if v >= 0x80UL then
            out.[pos] <- byte (v &&& 0x7FUL ||| 0x80UL)
            writeVarintTo out (pos + 1) (v >>> 7)
        else
            out.[pos] <- byte v
            pos + 1

    let varintLength (v: uint64) =
        let rec loop bytes value =
            if value < 0x80UL then
                bytes + 1
            else
                loop (bytes + 1) (value >>> 7)

        loop 0 v

    let private chunkLength =
        function
        | Bytes bs -> bs.Length
        | Slice(_, _, length) -> length
        | Byte _ -> 1
        | UInt64LE _
        | UInt64BE _ -> 8
        | UInt32LE _ -> 4
        | Varint v -> varintLength v

    /// 平坦化後のバイト長。
    let private length (w: Writer) = w.Chunks |> List.sumBy chunkLength

    let private writeChunk (out: byte array) (pos: int) =
        function
        | Bytes bs -> Array.blit bs 0 out pos bs.Length
        | Slice(bs, offset, length) -> Array.blit bs offset out pos length
        | Byte b -> out.[pos] <- b
        | UInt64LE v ->
            for i in 0..7 do
                out.[pos + i] <- byte (v >>> 8 * i &&& 0xFFUL)
        | UInt64BE v ->
            for i in 0..7 do
                out.[pos + i] <- byte (v >>> 8 * (7 - i) &&& 0xFFUL)
        | UInt32LE v ->
            for i in 0..3 do
                out.[pos + i] <- byte (v >>> 8 * i &&& 0xFFu)
        | Varint v -> writeVarintTo out pos v |> ignore

    /// 全チャンクを 1本のバイト列に平坦化する。
    /// チャンク列は逆順（先頭が最後の書き込み）なので、
    /// 出力配列の末尾から埋めて `List.rev` を不要にする。
    let toArray (w: Writer) =
        let total = length w
        let out = Array.zeroCreate<byte> total

        let rec fill pos chunks =
            match chunks with
            | [] -> out
            | chunk :: rest ->
                let next = pos - chunkLength chunk
                writeChunk out next chunk
                fill next rest

        fill total w.Chunks

    /// 書き出し手順を左から順に適用して 1本のバイト列にする。
    /// 要素は Writer.varint x のような部分適用で書ける。
    let build = List.fold (fun w f -> f w) Empty >> toArray

/// 読み取り: Cursor
/// 読み取り位置を値が持つ。
/// 更新は新しい Cursor を返すので mutable は要らない。
/// すべての読み取りは Result を返し、例外を送らない。
type internal Cursor =
    { Data: byte array; Pos: int; End: int }

module internal Cursor =
    let create (data: byte array) =
        {
            Data = data
            Pos = 0
            End = data.Length
        }

    /// 配列の一部だけを読み取るカーソル。
    /// 範囲外は読み取り時に CorruptionError になる。
    let createSlice (data: byte array) (offset: int) (length: int) =
        {
            Data = data
            Pos = offset
            End = offset + length
        }

    let inline remaining (c: Cursor) = c.End - c.Pos
    let inline atEnd (c: Cursor) = c.Pos >= c.End

module internal Reader =
    let private eof msg = err (DbError.corruption msg)

    let readByte (c: Cursor) =
        if Cursor.atEnd c then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            ret (c.Data.[c.Pos], { c with Pos = c.Pos + 1 })

    let readBytes (count: int) (c: Cursor) =
        if
            count < 0
            || c.Pos < 0
            || c.End < c.Pos
            || c.End > c.Data.Length
            || count > c.End - c.Pos
        then
            eof (sprintf "Reader: %d バイトを読み取れません（残り %d）" count (Cursor.remaining c))
        else
            ret (Array.sub c.Data c.Pos count, { c with Pos = c.Pos + count })

    let readBool = readByte >> Result.map (fun (b, c) -> b <> 0uy, c)

    /// 呼び出し側の配列へ読み取る（読み取り用の配列を確保しない）。
    let copyBytesInto (dest: byte array) (destOffset: int) (count: int) (c: Cursor) =
        if
            count < 0
            || destOffset < 0
            || destOffset > dest.Length - count
            || c.Pos < 0
            || c.End < c.Pos
            || c.End > c.Data.Length
            || count > c.End - c.Pos
        then
            eof (sprintf "Reader: %d バイトを読み取れません（残り %d）" count (Cursor.remaining c))
        else
            Array.blit c.Data c.Pos dest destOffset count
            ret { c with Pos = c.Pos + count }

    /// 読み飛ばす（配列を確保しない）。
    /// 値を使わないブロック走査で、不要なエントリの値コピーを避けるために使う。
    let skipBytes (count: int) (c: Cursor) =
        if
            count < 0
            || c.Pos < 0
            || c.End < c.Pos
            || c.End > c.Data.Length
            || count > c.End - c.Pos
        then
            eof (sprintf "Reader: %d バイトを読み飛ばせません（残り %d）" count (Cursor.remaining c))
        else
            ret { c with Pos = c.Pos + count }

    /// 符号なし LEB128 を読む。
    let private readVarintV =
        let rec decode shift acc (c: Cursor) =
            if shift > 63 || c.Pos >= c.End then
                ValueNone
            else
                let b = c.Data.[c.Pos]
                let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)
                let next = { c with Pos = c.Pos + 1 }

                if b &&& 0x80uy <> 0uy then
                    decode (shift + 7) acc next
                else
                    ValueSome struct (acc, next)

        decode 0 0UL

    let readVarint (c: Cursor) =
        match readVarintV c with
        | ValueSome struct (v, c) -> ret (v, c)
        | ValueNone -> eof "Reader: varint が不正です（長すぎるか末尾を超えました）"

    /// リトルエンディアン 8バイト。
    /// 値ごとに配列を作らない（行のデコードで最も呼ばれる）。
    let readInt64LE (c: Cursor) =
        if c.Pos + 8 > c.End then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            let d = c.Data
            let p = c.Pos

            let v =
                int64 d.[p]
                ||| (int64 d.[p + 1] <<< 8)
                ||| (int64 d.[p + 2] <<< 16)
                ||| (int64 d.[p + 3] <<< 24)
                ||| (int64 d.[p + 4] <<< 32)
                ||| (int64 d.[p + 5] <<< 40)
                ||| (int64 d.[p + 6] <<< 48)
                ||| (int64 d.[p + 7] <<< 56)

            ret (v, { c with Pos = p + 8 })

    /// リトルエンディアン 4バイト（符号なし）を読む。
    let readUInt32LE (c: Cursor) =
        if c.Pos + 4 > c.End then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            let d = c.Data
            let p = c.Pos

            let v =
                uint32 d.[p]
                ||| (uint32 d.[p + 1] <<< 8)
                ||| (uint32 d.[p + 2] <<< 16)
                ||| (uint32 d.[p + 3] <<< 24)

            ret (v, { c with Pos = p + 4 })

    /// ビッグエンディアン 8バイト（符号なし）。
    /// 順序保持された整数の復元。
    let readUint64BE (c: Cursor) =
        if c.Pos + 8 > c.End then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            let d = c.Data
            let p = c.Pos

            let v =
                uint64 d.[p] <<< 56
                ||| (uint64 d.[p + 1] <<< 48)
                ||| (uint64 d.[p + 2] <<< 40)
                ||| (uint64 d.[p + 3] <<< 32)
                ||| (uint64 d.[p + 4] <<< 24)
                ||| (uint64 d.[p + 5] <<< 16)
                ||| (uint64 d.[p + 6] <<< 8)
                ||| uint64 d.[p + 7]

            ret (v, { c with Pos = p + 8 })

    let readLengthPrefixed (c: Cursor) =
        readVarint c
        >>= fun (n, c) ->
            if n > uint64 System.Int32.MaxValue then
                eof (sprintf "Reader: 長すぎる長さ指定 %d" n)
            else
                readBytes (int n) c

    /// 長さ付き UTF-8 文字列を読む（Writer.str の逆）。
    let readStr (c: Cursor) =
        readLengthPrefixed c
        |> Result.map (fun (bs, c) -> System.Text.Encoding.UTF8.GetString bs, c)

    /// count 件を 1件ずつ読み出してリストにする。
    /// 「1件読む」処理は readOne に切り出し、ループはここ 1箇所にまとめる。
    let readItems (readOne: Cursor -> Result<'a * Cursor, DbError>) (count: uint64) (c: Cursor) =
        let rec readRemaining rest acc c =
            if rest = 0UL then
                ret (List.rev acc, c)
            else
                match readOne c with
                | Error e -> err e
                | Ok(v, c') -> readRemaining (rest - 1UL) (v :: acc) c'

        readRemaining count [] c

/// 順序保持符号化 (order-preserving encoding)
/// SSTable のマージはキーのバイト列比較で行うので、「バイト列の大小関係 = SQL の比較結果」でなければならない。
///   * 型タグを SQL 照合順に単調増加させる
///   * 数値・日付・時刻は順序を保った整数に写像してビッグエンディアンで書く
///   * TEXT / BLOB の 0x00 は 0x00 0xFF に escape し終端に 0x00 を置く
///     → 生の前方一致がそのまま正しい接頭辞一致になる
module internal OrderPreserving =
    /// 型タグ。
    /// 順序は SQL の照合順に従う。
    type Tag =
        | Null = 0x00uy
        | Bool = 0x10uy
        | Int = 0x20uy
        | Real = 0x21uy
        | Decimal = 0x22uy
        | Date = 0x30uy
        | Timestamp = 0x40uy
        | Text = 0x50uy
        | Blob = 0x60uy
        | Uuid = 0x70uy

    /// フィールド終端バイト。
    /// ペイロード中の 0x00 は必ず escape される。
    [<Literal>]
    let private Terminator = 0x00uy

    /// ペイロード中の 0x00 を escape したときの後続バイト。
    [<Literal>]
    let private EscapeMarker = 0xFFuy

    /// int64 の順序を uint64 の順序に写像する。
    /// 「2^63 を符号なしで足す」ことで、-2^63 が 0 に、+2^63-1 が 2^64-1 に対応し、単調増加が保たれる。
    /// 注意: 「負数なら全ビット反転（~~~）」というよくある書き方は int64 では順序が反転する。
    /// -1 のビットパターンが全 1 で絶対値の最大になるため、反転すると最小になり、-2 より小さくなってしまう。
    /// この書き方が正しく効くのは double（指数部に余裕がある）だけ。
    let private mapInt64 (v: int64) = uint64 v + 0x8000000000000000UL

    /// double の順序を int64 の順序に写像する（IEEE-754 のビット表現を前提）。
    let private mapDouble (v: float) =
        let bits = System.BitConverter.DoubleToInt64Bits v

        if bits < 0L then
            ~~~bits
        else
            bits ||| System.Int64.MinValue

    let private writeOrderedInt64 (v: int64) (w: Writer) = w |> Writer.uint64BE (mapInt64 v)

    /// decimal の有効桁数の上限。
    [<Literal>]
    let private DecimalMaxDigits = 29

    let private writeOrderedDecimal (v: decimal) (w: Writer) =
        let rec trim unscaled scale =
            if unscaled % System.Numerics.BigInteger 10 = System.Numerics.BigInteger.Zero then
                trim (unscaled / System.Numerics.BigInteger 10) (scale - 1)
            else
                unscaled, scale

        let decimalParts v =
            let bits = System.Decimal.GetBits v
            let flags = uint32 bits.[3]
            let negative = flags &&& 0x80000000u <> 0u
            let scale = int (flags >>> 16 &&& 0xFFu)

            let unscaled =
                System.Numerics.BigInteger(uint32 bits.[0])
                + (System.Numerics.BigInteger(uint32 bits.[1]) <<< 32)
                + (System.Numerics.BigInteger(uint32 bits.[2]) <<< 64)

            if unscaled.IsZero then
                false, 0, ""
            else
                let u, s = trim unscaled scale
                let digits = u.ToString()
                negative, int digits.Length - s, digits

        let negative, exponent, digits = decimalParts v
        let isZero = digits = ""

        let paddedDigits =
            Seq.append
                (digits |> Seq.map (fun ch -> byte (int ch) - 0x30uy))
                (Seq.init (DecimalMaxDigits - String.length digits) (fun _ -> 0uy))
            |> Seq.toArray

        let digitBytes =
            if negative then
                Array.map (fun b -> 9uy - b) paddedDigits
            else
                paddedDigits

        let exponentValue =
            if isZero then System.Int64.MinValue
            elif negative then -int64 exponent
            else int64 exponent

        w
        |> Writer.writeByte (if negative then 0x00uy else 0x01uy)
        |> writeOrderedInt64 exponentValue
        |> Writer.writeBytes digitBytes

    let private guidToOrderedBytes (g: System.Guid) =
        let s = g.ToString("N", System.Globalization.CultureInfo.InvariantCulture)

        [|
            for i in 0..15 do
                yield System.Convert.ToByte(s.Substring(i * 2, 2), 16)
        |]

    /// 値をキーへ追加する（型タグ + escape 済みペイロード + 終端）。
    let writeValue (v: SqlValue) (w: Writer) =
        let writeEscapedPayload bs w =
            let escaped =
                bs
                |> Array.collect (fun b -> if b = 0uy then [| Terminator; EscapeMarker |] else [| b |])

            w |> Writer.writeBytes escaped |> Writer.writeByte Terminator

        let tagged tag = w |> Writer.writeByte (byte tag)
        let terminate = Writer.writeByte Terminator

        match v with
        | SqlValue.Null -> tagged Tag.Null |> terminate
        | SqlValue.Bool b -> tagged Tag.Bool |> Writer.writeByte (if b then 1uy else 0uy) |> terminate
        | SqlValue.Int i -> tagged Tag.Int |> writeOrderedInt64 i |> terminate
        | SqlValue.Real f ->
            tagged Tag.Real
            |> Writer.uint64BE (uint64 (mapDouble (SqlValue.normalizeFloat f)))
            |> terminate
        | SqlValue.Decimal d -> tagged Tag.Decimal |> writeOrderedDecimal d |> terminate
        | SqlValue.Date d ->
            let days =
                d.ToDateTime(System.TimeOnly.MinValue).Ticks / System.TimeSpan.TicksPerDay

            tagged Tag.Date |> writeOrderedInt64 days |> terminate
        | SqlValue.Timestamp t -> tagged Tag.Timestamp |> writeOrderedInt64 t.UtcTicks |> terminate
        | SqlValue.Text s -> writeEscapedPayload (System.Text.Encoding.UTF8.GetBytes s) (tagged Tag.Text)
        | SqlValue.Blob bs -> writeEscapedPayload bs (tagged Tag.Blob)
        | SqlValue.Uuid g -> tagged Tag.Uuid |> Writer.writeBytes (guidToOrderedBytes g) |> terminate

module internal Hashing =
    /// バイト列の 64bit ハッシュ（FNV-1a）。
    /// Bloom filter のビット位置を決めるのに使う。
    /// キー自体とは独立に、値の並びには影響しない。
    let hashBytes (bs: byte array) =
        let offsetBasis = 14695981039346656037UL
        let prime = 1099511628211UL
        Array.fold (fun h b -> (h ^^^ uint64 b) * prime) offsetBasis bs

    /// バイト列の 32bit チェックサム（FNV-1a）。
    /// WAL のフレームと SSTable の Footer の破損検出に使う。
    let checksum (data: byte array) =
        Array.fold (fun h b -> (h ^^^ uint32 b) * 16777619u) 2166136261u data

    /// 範囲指定の 32bit チェックサム（FNV-1a）。
    /// SSTable のブロック末尾チェックサムを、コピーを作らずに計算する。
    let checksumRange (data: byte array) (offset: int) (length: int) =
        let rec loop i h =
            if i >= offset + length then
                h
            else
                loop (i + 1) ((h ^^^ uint32 data.[i]) * 16777619u)

        loop offset 2166136261u

    /// 複数のチャンクを連結したものの 32bit チェックサム（FNV-1a）。
    /// SSTable のメタデータ（meta / Bloom / index）をコピーせずに検証する。
    let checksumChunks (chunks: byte array array) =
        let rec loopChunk chunkIndex i h =
            if chunkIndex >= chunks.Length then
                h
            elif i >= chunks.[chunkIndex].Length then
                loopChunk (chunkIndex + 1) 0 h
            else
                loopChunk chunkIndex (i + 1) ((h ^^^ uint32 chunks.[chunkIndex].[i]) * 16777619u)

        loopChunk 0 0 2166136261u

    /// ハッシュ値を攪拌する（splitmix64 相当）。
    /// FNV-1a は下位ビットに偏りがちで、Bloom filter のビット分布が悪くなるため。
    let mix (x: uint64) =
        let h1 = (x ^^^ (x >>> 30)) * 0xBF58476D1CE4E5B9UL
        let h2 = (h1 ^^^ (h1 >>> 27)) * 0x94D049BB133111EBUL
        h2 ^^^ (h2 >>> 31)

/// 複合キーのビルダ。
/// フィールドを順に足して 1本のバイト列キーにする。
module internal KeyBuilder =
    /// 値の並びを 1本のキーに符号化する。
    let ofValues (values: SqlValue seq) =
        values
        |> Seq.fold (fun w v -> OrderPreserving.writeValue v w) Writer.Empty
        |> Writer.toArray

    /// 索引キー（索引列 + 主キー列）。
    /// 主キーは行を一意に特定するために末尾へ追加する。
    let ofIndexAndPrimary (indexCols: SqlValue seq) (primaryCols: SqlValue seq) =
        let appendCols = Seq.fold (fun w v -> OrderPreserving.writeValue v w)
        let withIndex = appendCols Writer.Empty indexCols
        appendCols withIndex primaryCols |> Writer.toArray

    /// バイト列 key が prefix で始まるか。
    let startsWithPrefix (key: byte array) (prefix: byte array) =
        let rec matchPrefix i =
            if i >= prefix.Length then true
            elif key.[i] = prefix.[i] then matchPrefix (i + 1)
            else false

        if key.Length < prefix.Length then false else matchPrefix 0

/// 行ペイロード用のコンパクト符号化。
/// キーと異なり、行値には順序保持が不要なので varint で詰めて小さくする。
module internal ValueCodec =
    /// 小数の負のゼロ (-0.0M) を +0.0M に正規化する。
    /// compareValues は両者を等しいと扱うため、
    /// DISTINCT などの符号化でも同じビット列にして重複と見なす。
    let private normalizeDecimal (d: decimal) = if d = 0M then 0M else d

    let private writeValue (w: Writer) =
        let tagged tag = w |> Writer.writeByte (byte tag)

        function
        | SqlValue.Null -> tagged 0uy
        | SqlValue.Bool b -> tagged 1uy |> Writer.bool b
        | SqlValue.Int i -> tagged 2uy |> Writer.int64LE i
        | SqlValue.Real f ->
            tagged 3uy
            |> Writer.int64LE (SqlValue.normalizeFloat f |> System.BitConverter.DoubleToInt64Bits)
        | SqlValue.Decimal d ->
            normalizeDecimal d
            |> System.Decimal.GetBits
            |> Array.fold (fun w b -> w |> Writer.int64LE (int64 b)) (tagged 4uy)
        | SqlValue.Text s -> tagged 5uy |> Writer.str s
        | SqlValue.Blob bs -> tagged 6uy |> Writer.lengthPrefixed bs
        | SqlValue.Date d -> tagged 7uy |> Writer.int64LE (d.ToDateTime(System.TimeOnly.MinValue).Ticks)
        | SqlValue.Timestamp t -> tagged 8uy |> Writer.int64LE t.UtcTicks
        | SqlValue.Uuid g -> tagged 9uy |> Writer.writeBytes (g.ToByteArray())

    let private readValueSlow (c: Cursor) =
        result {
            let! tag, c = Reader.readByte c

            match tag with
            | 0uy -> return SqlValue.Null, c
            | 1uy ->
                let! v, c = Reader.readBool c
                return SqlValue.Bool v, c
            | 2uy ->
                let! v, c = Reader.readInt64LE c
                return SqlValue.Int v, c
            | 3uy ->
                let! bits, c = Reader.readInt64LE c
                return System.BitConverter.Int64BitsToDouble bits |> SqlValue.vReal, c
            | 4uy ->
                let! bs, c = Reader.readBytes 32 c

                let bits =
                    [|
                        for i in 0..3 do
                            yield System.BitConverter.ToInt64(bs, i * 8) |> int
                    |]

                let flags = uint32 bits.[3]
                let scale = flags >>> 16 &&& 0xFFu

                if scale > 28u || flags &&& 0x7F00FFFFu <> 0u then
                    return! err (DbError.corruption "ValueCodec: DECIMAL のビット列が不正です")

                return System.Decimal bits |> SqlValue.Decimal, c
            | 5uy ->
                let! bs, c = Reader.readLengthPrefixed c
                return System.Text.Encoding.UTF8.GetString bs |> SqlValue.Text, c
            | 6uy ->
                let! bs, c = Reader.readLengthPrefixed c
                return SqlValue.Blob bs, c
            | 7uy ->
                let! ticks, c = Reader.readInt64LE c
                let days = ticks / System.TimeSpan.TicksPerDay

                if
                    days < int64 System.DateOnly.MinValue.DayNumber
                    || days > int64 System.DateOnly.MaxValue.DayNumber
                then
                    return! err (DbError.corruption "ValueCodec: DATE の範囲が不正です")

                return SqlValue.Date(System.DateOnly.FromDayNumber(int days)), c
            | 8uy ->
                let! ticks, c = Reader.readInt64LE c

                if ticks < 0L || ticks > System.DateTimeOffset.MaxValue.Ticks then
                    return! err (DbError.corruption "ValueCodec: TIMESTAMP の範囲が不正です")

                return System.DateTimeOffset(ticks, System.TimeSpan.Zero) |> SqlValue.Timestamp, c
            | 9uy ->
                let! bs, c = Reader.readBytes 16 c
                return System.Guid bs |> SqlValue.Uuid, c
            | _ -> return! err (sprintf "ValueCodec: 不明な型タグ %d" (int tag) |> DbError.corruption)
        }

    /// 値の並びをまとめて書き出す。
    let writeValues (values: SqlValue seq) (w: Writer) =
        let arr = Seq.toArray values
        let withCount = w |> Writer.varint (uint64 arr.Length)
        Array.fold writeValue withCount arr

    let private writeInt64LETo (out: byte array) (pos: int) (v: int64) =
        let u = uint64 v

        for i in 0..7 do
            out.[pos + i] <- byte (u >>> 8 * i &&& 0xFFUL)

        pos + 8

    /// 1値を出力配列へ直接書く。次の位置を返す。
    let private writeValueTo (out: byte array) (pos: int) =
        function
        | SqlValue.Null ->
            out.[pos] <- 0uy
            pos + 1
        | SqlValue.Bool b ->
            out.[pos] <- 1uy
            out.[pos + 1] <- if b then 1uy else 0uy
            pos + 2
        | SqlValue.Int i ->
            out.[pos] <- 2uy
            writeInt64LETo out (pos + 1) i
        | SqlValue.Real f ->
            out.[pos] <- 3uy
            writeInt64LETo out (pos + 1) (SqlValue.normalizeFloat f |> System.BitConverter.DoubleToInt64Bits)
        | SqlValue.Decimal d ->
            out.[pos] <- 4uy

            normalizeDecimal d
            |> System.Decimal.GetBits
            |> Array.fold (fun p b -> writeInt64LETo out p (int64 b)) (pos + 1)
        | SqlValue.Text s ->
            let bytes = System.Text.Encoding.UTF8.GetBytes s
            out.[pos] <- 5uy
            let p = Writer.writeVarintTo out (pos + 1) (uint64 bytes.Length)
            Array.blit bytes 0 out p bytes.Length
            p + bytes.Length
        | SqlValue.Blob bs ->
            out.[pos] <- 6uy
            let p = Writer.writeVarintTo out (pos + 1) (uint64 bs.Length)
            Array.blit bs 0 out p bs.Length
            p + bs.Length
        | SqlValue.Date d ->
            out.[pos] <- 7uy
            writeInt64LETo out (pos + 1) (d.ToDateTime(System.TimeOnly.MinValue).Ticks)
        | SqlValue.Timestamp t ->
            out.[pos] <- 8uy
            writeInt64LETo out (pos + 1) t.UtcTicks
        | SqlValue.Uuid g ->
            let bytes = g.ToByteArray()
            out.[pos] <- 9uy
            Array.blit bytes 0 out (pos + 1) 16
            pos + 17

    /// 値の並びを出力配列へ直接書く。次の位置を返す。
    let writeValuesTo (out: byte array) (pos: int) (values: SqlValue array) =
        let pos = Writer.writeVarintTo out pos (uint64 values.Length)
        Array.fold (fun p v -> writeValueTo out p v) pos values

    let private readInt (c: Cursor) =
        if c.Pos + 8 < c.End && c.Data.[c.Pos] = 2uy then
            let d = c.Data
            let p = c.Pos + 1

            let v =
                int64 d.[p]
                ||| (int64 d.[p + 1] <<< 8)
                ||| (int64 d.[p + 2] <<< 16)
                ||| (int64 d.[p + 3] <<< 24)
                ||| (int64 d.[p + 4] <<< 32)
                ||| (int64 d.[p + 5] <<< 40)
                ||| (int64 d.[p + 6] <<< 48)
                ||| (int64 d.[p + 7] <<< 56)

            ValueSome struct (SqlValue.Int v, { c with Pos = p + 8 })
        else
            ValueNone

    let private readValue (c: Cursor) =
        match readInt c with
        | ValueSome struct (v, c) -> ret (v, c)
        | ValueNone -> readValueSlow c

    let readValues (c: Cursor) =
        result {
            let! n, c = Reader.readVarint c

            if n > uint64 System.Int32.MaxValue then
                return! sprintf "ValueCodec: 要素数が大きすぎます (%d)" n |> DbError.corruption |> err
            else
                let count = int n
                let values = Array.zeroCreate<SqlValue> count

                let rec fill i c =
                    if i >= count then
                        ret (values, c)
                    else
                        match readInt c with
                        | ValueSome struct (v, c') ->
                            values.[i] <- v
                            fill (i + 1) c'
                        | ValueNone ->
                            match readValue c with
                            | Error e -> err e
                            | Ok(v, c') ->
                                values.[i] <- v
                                fill (i + 1) c'

                return! fill 0 c
        }

    /// 固定長の値の符号化サイズ（型タグ込み）。
    /// TEXT / BLOB は長さ依存なので `None`。
    let fixedEncodedBytes =
        function
        | SqlValue.Null -> Some 1
        | SqlValue.Bool _ -> Some 2
        | SqlValue.Int _ -> Some 9
        | SqlValue.Real _ -> Some 9
        | SqlValue.Decimal _ -> Some 33
        | SqlValue.Date _ -> Some 9
        | SqlValue.Timestamp _ -> Some 9
        | SqlValue.Uuid _ -> Some 17
        | SqlValue.Text _
        | SqlValue.Blob _ -> None

    /// 1値の符号化後の正確なバイト数。
    let private encodedValueLength =
        function
        | SqlValue.Text s ->
            1
            + Writer.varintLength (uint64 (System.Text.Encoding.UTF8.GetByteCount s))
            + System.Text.Encoding.UTF8.GetByteCount s
        | SqlValue.Blob bs -> 1 + Writer.varintLength (uint64 bs.Length) + bs.Length
        | v -> fixedEncodedBytes v |> Option.defaultValue 1

    /// 値の並びの符号化後の正確なバイト数。
    let encodedValuesLength (values: SqlValue array) =
        Writer.varintLength (uint64 values.Length)
        + Array.sumBy encodedValueLength values

    /// 1値の符号化後のおよそのバイト数（SSTable のファイルサイズ見積り）。
    /// 実際のファイルサイズとは厳密には一致しない。
    /// **メモリ上限の判定には使わない**
    /// （参照型の `SqlValue`・string・配列ヘッダを含まないため）。
    /// メモリの見積りは `managedBytes` / `managedRowBytes` を使う。
    let approximateBytes (v: SqlValue) =
        match fixedEncodedBytes v with
        | Some n -> n
        | None ->
            match v with
            | SqlValue.Text s -> 5 + System.Text.Encoding.UTF8.GetByteCount s
            | SqlValue.Blob bs -> 5 + bs.Length
            | _ -> 1

    /// 値 1つのヒープ上の概算サイズ（メモリ上限の判定用）。
    /// <remarks>
    /// `SqlValue` は参照型の DU なので、
    /// 1値ごとにオブジェクト ヘッダ（16 バイト）とペイロードを持つ。
    /// TEXT は `SqlValue` 本体（24）+ string（24 + UTF-16 で 2 バイト/文字）、
    /// BLOB は `SqlValue` 本体（24）+ byte 配列（24 + 長さ）。
    /// 64bit の .NET を前提にした概算で、厳密なサイズではない。
    /// </remarks>
    let managedBytes =
        function
        | SqlValue.Null -> 0
        | SqlValue.Text s -> 48 + 2 * s.Length
        | SqlValue.Blob b -> 48 + b.Length
        | SqlValue.Decimal _
        | SqlValue.Timestamp _
        | SqlValue.Uuid _ -> 32
        | SqlValue.Bool _
        | SqlValue.Int _
        | SqlValue.Real _
        | SqlValue.Date _ -> 24

    /// 1行（`SqlValue array`）のヒープ上の概算サイズ。
    /// 配列ヘッダ（24）+ スロット（8 × 列数）+ 各値（`managedBytes`）。
    let managedRowBytes (row: SqlValue array) =
        24 + 8 * row.Length + (row |> Array.sumBy managedBytes)
