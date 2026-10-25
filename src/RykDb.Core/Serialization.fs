namespace RykDb.Core

// System.Decimal は IDisposable ではないが F# が IDisposable を継承した構造体と誤認して FS0760 を出す。
// decimal の再構築はこの形が唯一素直なので範囲を限定して抑制する。
#nowarn "760"

/// 書き出し: Writer
/// 可変バッファではなく「不変のチャンク列」で表現する。
/// 追加操作は新しい Writer を返すので、途中で失敗しても中途半端な状態は残らない。
/// 最後に一度だけ配列へ平坦化する。
type internal Writer = { Chunks: byte array list }

module internal Writer =
    let empty: Writer = { Chunks = [] }

    let private add (w: Writer) (b: byte array) =
        if b.Length = 0 then w else { Chunks = b :: w.Chunks }

    let writeByte (b: byte) (w: Writer) = add w [| b |]

    /// バイト列を追加する。並びは保たれる。
    let writeBytes (bs: byte array) (w: Writer) = add w bs

    let bool (v: bool) (w: Writer) = add w [| if v then 1uy else 0uy |]

    /// 符号なし LEB128（順序保持ではない）。
    let varint (v: uint64) (w: Writer) =
        let rec emit x acc =
            if x >= 0x80UL then
                emit (x >>> 7) (byte (x &&& 0x7FUL ||| 0x80UL) :: acc)
            else
                add w (byte x :: acc |> List.rev |> List.toArray)

        emit v []

    /// リトルエンディアン 8バイト。
    let int64LE (v: int64) (w: Writer) =
        add w (Array.init 8 (fun i -> byte (v >>> 8 * i &&& 0xFFL)))

    /// リトルエンディアン 4バイト（符号なし）。
    /// WAL のフレームヘッダや SSTable の Footer など固定長フィールド用。
    let uint32LE (v: uint32) (w: Writer) =
        add w (Array.init 4 (fun i -> byte (v >>> 8 * i &&& 0xFFu)))

    /// ビッグエンディアン 8バイト（符号なし）。
    /// 順序保持符号化で使う。
    let uint64BE (v: uint64) (w: Writer) =
        add w (Array.init 8 (fun i -> byte (v >>> 8 * (7 - i) &&& 0xFFUL)))

    /// 長さ付きバイト列（varint 長 + 実体）。
    let lengthPrefixed (bs: byte array) (w: Writer) =
        w |> varint (uint64 bs.Length) |> writeBytes bs

    /// UTF-8 文字列を長さ付きで追加する。
    let str (s: string) (w: Writer) =
        lengthPrefixed (System.Text.Encoding.UTF8.GetBytes s) w

    /// 全チャンクを 1本のバイト列に平坦化する。
    let toArray (w: Writer) =
        w.Chunks |> List.rev |> List.toArray |> Array.concat

    /// 書き出し手順を左から順に適用して 1本のバイト列にする。
    /// 要素は Writer.varint x のような部分適用で書ける。
    let build (steps: (Writer -> Writer) list) =
        steps |> List.fold (fun w f -> f w) empty |> toArray

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
            Ok(c.Data.[c.Pos], { c with Pos = c.Pos + 1 })

    let readBytes (count: int) (c: Cursor) =
        if count < 0 || c.Pos + count > c.End then
            eof (sprintf "Reader: %d バイトを読み取れません（残り %d）" count (Cursor.remaining c))
        else
            Ok(Array.sub c.Data c.Pos count, { c with Pos = c.Pos + count })

    let readBool (c: Cursor) =
        readByte c |> Result.map (fun (b, c) -> b <> 0uy, c)

    /// 符号なし LEB128 を読む。
    let readVarint (c: Cursor) =
        let rec decode shift acc c =
            if shift > 63 then
                eof "Reader: varint が長すぎます"
            else
                match readByte c with
                | Error e -> Error e
                | Ok(b, c) ->
                    let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)

                    if b &&& 0x80uy <> 0uy then
                        decode (shift + 7) acc c
                    else
                        Ok(acc, c)

        decode 0 0UL c

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

            Ok(v, { c with Pos = p + 8 })

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

            Ok(v, { c with Pos = p + 4 })

    /// ビッグエンディアン 8バイト（符号なし）。
    /// 順序保持された整数の復元。
    let readUint64BE (c: Cursor) =
        if c.Pos + 8 > c.End then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            let d = c.Data
            let p = c.Pos

            let v =
                (uint64 d.[p] <<< 56)
                ||| (uint64 d.[p + 1] <<< 48)
                ||| (uint64 d.[p + 2] <<< 40)
                ||| (uint64 d.[p + 3] <<< 32)
                ||| (uint64 d.[p + 4] <<< 24)
                ||| (uint64 d.[p + 5] <<< 16)
                ||| (uint64 d.[p + 6] <<< 8)
                ||| uint64 d.[p + 7]

            Ok(v, { c with Pos = p + 8 })

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
    let readItems (count: uint64) (readOne: Cursor -> Result<'a * Cursor, DbError>) (c: Cursor) =
        let rec readRemaining rest c acc =
            if rest = 0UL then
                ret (List.rev acc, c)
            else
                match readOne c with
                | Error e -> Error e
                | Ok(v, c') -> readRemaining (rest - 1UL) c' (v :: acc)

        readRemaining count c []

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

    let private unmapInt64 (m: uint64) = int64 (m - 0x8000000000000000UL)

    /// double の順序を int64 の順序に写像する（IEEE-754 のビット表現を前提）。
    let private mapDouble (v: float) =
        let bits = System.BitConverter.DoubleToInt64Bits v

        if bits < 0L then
            ~~~bits
        else
            bits ||| System.Int64.MinValue

    let private unmapDouble (v: int64) =
        let bits = if v < 0L then v ^^^ System.Int64.MinValue else ~~~v
        System.BitConverter.Int64BitsToDouble bits

    let private writeOrderedInt64 (v: int64) (w: Writer) = w |> Writer.uint64BE (mapInt64 v)

    let private readOrderedInt64 (c: Cursor) =
        Reader.readUint64BE c |> Result.map (fun (m, c) -> unmapInt64 m, c)

    /// decimal の有効桁数の上限。
    [<Literal>]
    let private DecimalMaxDigits = 29

    /// decimal を順序を保ったバイト列へ写像する。
    /// raw な 128bit ビット列は使えない。
    /// decimal の内部表現は「非スケール値（96bit）＋ スケール（10 進指数）」なので、
    /// 0.000001（スケール 6）と Decimal.MaxValue（スケール 0）のビット列をそのまま比較しても数値順にならない。
    /// そこで値=value = D x 10^(-s) という正規形（末尾 0 を落とした桁列 D とスケール s）に直して、
    ///   (符号, 10 進指数 e = len(D) - s, D を右詰めした 29桁)
    /// の順に並べる。
    /// 同じ符号・同じ指数なら D の右詰め比較が値の比較そのものになる。
    /// 指数は順序保持された 8バイト整数で書く。
    let private decimalParts (v: decimal) =
        let rec trim u s =
            if u % System.Numerics.BigInteger 10 = System.Numerics.BigInteger.Zero then
                trim (u / System.Numerics.BigInteger 10) (s - 1)
            else
                u, s

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

    let private writeOrderedDecimal (v: decimal) (w: Writer) =
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

    let private readOrderedDecimal (c: Cursor) =
        result {
            let! sign, c = Reader.readByte c
            let! raw, c = Reader.readUint64BE c
            let! digitBytes, c = Reader.readBytes DecimalMaxDigits c
            let negative = sign = 0uy
            let stored = unmapInt64 raw
            let exponent = if negative then -stored else stored
            let isZero = digitBytes |> Array.forall (fun b -> b = 0uy)

            if isZero then
                return 0M, c
            else
                let digits =
                    digitBytes
                    |> Array.map (fun b -> if negative then 9uy - b else b)
                    |> Array.map (fun b -> char (int (b + 0x30uy)))
                    |> System.String

                let text = sprintf "%sE%d" digits (exponent - int64 DecimalMaxDigits)

                match
                    System.Decimal.TryParse(
                        text,
                        System.Globalization.NumberStyles.Float,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, d when not negative -> return d, c
                | true, d -> return -d, c
                | _ -> return! err (sprintf "OrderPreserving: decimal を復元できません: %s" text |> DbError.corruption)
        }

    let private guidToOrderedBytes (g: System.Guid) =
        let s = g.ToString("N", System.Globalization.CultureInfo.InvariantCulture)

        [|
            for i in 0..15 do
                yield System.Convert.ToByte(s.Substring(i * 2, 2), 16)
        |]

    let private orderedBytesToGuid (bs: byte array) =
        let sb = System.Text.StringBuilder 32

        bs
        |> Array.iter (fun b ->
            sb.Append(b.ToString("X2", System.Globalization.CultureInfo.InvariantCulture))
            |> ignore)

        System.Guid.ParseExact(sb.ToString(), "N")

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

    /// キーに書き込んだ 1フィールドを読む。
    /// escape を解き、フィールド終端まで消費して値を返す。
    let private readValue (start: Cursor) =
        let rec unescape c acc =
            let finish c =
                Ok(List.rev acc |> List.toArray, { c with Pos = c.Pos - 1 })

            match Reader.readByte c with
            | Error e -> Error e
            | Ok(b, c) when b <> Terminator -> unescape c (b :: acc)
            | Ok(_, c) when Cursor.atEnd c -> finish c
            | Ok(_, c) ->
                match Reader.readByte c with
                | Error e -> Error e
                | Ok(EscapeMarker, c) -> unescape c (Terminator :: acc)
                | Ok(_, _) -> finish c

        let readEscapedPayload c = unescape c []

        let finish v c =
            result {
                let! t, _ = Reader.readByte c

                if t <> Terminator then
                    return! err (sprintf "OrderPreserving: 終端が不正 (0x%02X)" t |> DbError.corruption)
                else
                    return v, c
            }

        result {
            let! tag, c = Reader.readByte start

            match LanguagePrimitives.EnumOfValue<byte, Tag> tag with
            | Tag.Null -> return! finish SqlValue.Null c
            | Tag.Bool ->
                let! v, c = Reader.readBool c
                return! finish (SqlValue.Bool v) c
            | Tag.Int ->
                let! v, c = readOrderedInt64 c
                return! finish (SqlValue.Int v) c
            | Tag.Real ->
                let! m, c = Reader.readUint64BE c
                return! finish (unmapDouble (int64 m) |> SqlValue.normalizeFloat |> SqlValue.Real) c
            | Tag.Decimal ->
                let! v, c = readOrderedDecimal c
                return! finish (SqlValue.Decimal v) c
            | Tag.Date ->
                let! days, c = readOrderedInt64 c
                return! finish (int days |> System.DateOnly.FromDayNumber |> SqlValue.Date) c
            | Tag.Timestamp ->
                let! ticks, c = readOrderedInt64 c
                return! finish (System.DateTimeOffset(ticks, System.TimeSpan.Zero) |> SqlValue.Timestamp) c
            | Tag.Text ->
                let! bytes, c = readEscapedPayload c
                return! finish (System.Text.Encoding.UTF8.GetString bytes |> SqlValue.Text) c
            | Tag.Blob ->
                let! bytes, c = readEscapedPayload c
                return! finish (SqlValue.Blob bytes) c
            | Tag.Uuid ->
                let! bs, c = Reader.readBytes 16 c
                return! finish (orderedBytesToGuid bs |> SqlValue.Uuid) c
            | _ -> return! err (sprintf "OrderPreserving: 不明な型タグ 0x%02X" tag |> DbError.corruption)
        }

    /// キーから 1フィールドだけを読む（返り値は値と読み取り後の位置）。
    let readFirstValue (key: byte array) =
        Cursor.create key |> readValue |> Result.map (fun (v, c) -> v, c.Pos)

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
        let rec loop (i: int) (h: uint32) =
            if i >= offset + length then
                h
            else
                loop (i + 1) ((h ^^^ uint32 data.[i]) * 16777619u)

        loop offset 2166136261u

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
        |> Seq.fold (fun w v -> OrderPreserving.writeValue v w) Writer.empty
        |> Writer.toArray

    /// 索引キー（索引列 + 主キー列）。
    /// 主キーは行を一意に特定するために末尾へ追加する。
    let ofIndexAndPrimary (indexCols: SqlValue seq) (primaryCols: SqlValue seq) =
        let appendCols = Seq.fold (fun w v -> OrderPreserving.writeValue v w)
        let withIndex = appendCols Writer.empty indexCols
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
    let private writeValue (v: SqlValue) (w: Writer) =
        let tagged tag = w |> Writer.writeByte (byte tag)

        match v with
        | SqlValue.Null -> tagged 0uy
        | SqlValue.Bool b -> tagged 1uy |> Writer.bool b
        | SqlValue.Int i -> tagged 2uy |> Writer.int64LE i
        | SqlValue.Real f -> tagged 3uy |> Writer.int64LE (System.BitConverter.DoubleToInt64Bits f)
        | SqlValue.Decimal d ->
            let withTag = tagged 4uy

            System.Decimal.GetBits d
            |> Array.fold (fun w b -> w |> Writer.int64LE (int64 b)) withTag
        | SqlValue.Text s -> tagged 5uy |> Writer.str s
        | SqlValue.Blob bs -> tagged 6uy |> Writer.lengthPrefixed bs
        | SqlValue.Date d -> tagged 7uy |> Writer.int64LE (d.ToDateTime(System.TimeOnly.MinValue).Ticks)
        | SqlValue.Timestamp t -> tagged 8uy |> Writer.int64LE t.UtcTicks
        | SqlValue.Uuid g -> tagged 9uy |> Writer.writeBytes (g.ToByteArray())

    let private readValue (c: Cursor) =
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
        Array.fold (fun w v -> writeValue v w) withCount arr

    let readValues (c: Cursor) =
        result {
            let! n, c = Reader.readVarint c

            if n > uint64 System.Int32.MaxValue then
                return! err (sprintf "ValueCodec: 要素数が大きすぎます (%d)" n |> DbError.corruption)

            let! items, c = Reader.readItems n readValue c
            return List.toArray items, c
        }

    /// 固定長の値の符号化サイズ（型タグ込み）。
    /// TEXT / BLOB は長さ依存なので `None`。
    let fixedEncodedBytes (v: SqlValue) =
        match v with
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
