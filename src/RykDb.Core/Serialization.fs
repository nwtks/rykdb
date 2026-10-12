// ===========================================================================
// 書き出し: Writer
//
// 可変バッファではなく「不変のチャンク列」で表現する。
// 追加操作は新しい Writer を返すので、途中で失敗しても中途半端な状態は残らない。
// 最後に一度だけ配列へ平坦化する。
// ===========================================================================

namespace RykDb.Core

open System
open System.Globalization
open System.Numerics

// System.Decimal は IDisposable ではないが F# が IDisposable を継承した構造体と
// 誤認して FS0760 を出す。decimal の再構築はこの形が唯一素直なので範囲を限定して抑制する。
#nowarn "760"

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
    /// 下位の 7 ビットから先に書くので、蓄積は逆順で。
    /// 最後に反転して正しい並びにする。
    let varint (v: uint64) (w: Writer) =
        let rec go x acc =
            if x >= 0x80UL then
                go (x >>> 7) (byte (x &&& 0x7FUL ||| 0x80UL) :: acc)
            else
                add w (byte x :: acc |> List.rev |> List.toArray)

        go v []

    /// リトルエンディアン 8 バイト。
    let int64LE (v: int64) (w: Writer) =
        add w (Array.init 8 (fun i -> byte (v >>> 8 * i &&& 0xFFL)))

    /// リトルエンディアン 4 バイト（符号なし）。
    /// WAL のフレームヘッダや SSTable の Footer など固定長フィールド用。
    let uint32LE (v: uint32) (w: Writer) =
        add w (Array.init 4 (fun i -> byte (v >>> 8 * i &&& 0xFFu)))

    /// ビッグエンディアン 8 バイト（符号なし）。順序保持符号化で使う。
    let uint64BE (v: uint64) (w: Writer) =
        add w (Array.init 8 (fun i -> byte (v >>> 8 * (7 - i) &&& 0xFFUL)))

    /// 長さ付きバイト列（varint 長 + 実体）。
    let lengthPrefixed (bs: byte array) (w: Writer) =
        w |> varint (uint64 bs.Length) |> writeBytes bs

    /// UTF-8 文字列を長さ付きで追加する。
    let str (s: string) (w: Writer) =
        lengthPrefixed (Text.Encoding.UTF8.GetBytes s) w

    /// 全チャンクを 1 本のバイト列に平坦化する。
    let toArray (w: Writer) =
        w.Chunks |> List.rev |> List.toArray |> Array.concat

    /// 書き出し手順を左から順に適用して 1 本のバイト列にする。
    /// `Writer.empty |> f1 |> f2 |> Writer.toArray` の繰り返しをまとめたもの。
    /// 要素は `Writer.varint x` のような部分適用で書ける。
    let build (steps: (Writer -> Writer) list) =
        steps |> List.fold (fun w f -> f w) empty |> toArray

/// 読み取り: Cursor
/// 読み取り位置を値が持つ。更新は新しい Cursor を返すので mutable は要らない。
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

    /// 配列の一部だけを読み取るカーソル。範囲外は読み取り時に CorruptionError になる。
    let createSlice (data: byte array) (offset: int) (length: int) =
        {
            Data = data
            Pos = offset
            End = offset + length
        }

    let remaining (c: Cursor) = c.End - c.Pos
    let atEnd (c: Cursor) = c.Pos >= c.End

module internal Read =
    let private eof msg = Error(DbError.corruption msg)

    let readByte (c: Cursor) =
        if Cursor.atEnd c then
            eof "Read: 読み取り位置が末尾を超えました"
        else
            Ok(c.Data.[c.Pos], { c with Pos = c.Pos + 1 })

    let readBytes (count: int) (c: Cursor) =
        if count < 0 || c.Pos + count > c.End then
            eof (sprintf "Read: %d バイトを読み取れません（残り %d）" count (Cursor.remaining c))
        else
            Ok(Array.sub c.Data c.Pos count, { c with Pos = c.Pos + count })

    /// 符号なし LEB128 を読む。
    let readVarint (c: Cursor) =
        let rec go shift acc (c: Cursor) =
            if shift > 63 then
                eof "Read: varint が長すぎます"
            else
                match readByte c with
                | Error e -> Error e
                | Ok(b, c) ->
                    let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)

                    if b &&& 0x80uy <> 0uy then
                        go (shift + 7) acc c
                    else
                        Ok(acc, c)

        go 0 0UL c

    let readInt64LE (c: Cursor) =
        readBytes 8 c
        |> Result.map (fun (bs, c) ->
            let v = bs |> Array.mapi (fun i b -> int64 b <<< 8 * i) |> Array.sum
            v, c)

    /// リトルエンディアン 4バイト（符号なし）を読む。
    let readUInt32LE (c: Cursor) =
        readBytes 4 c
        |> Result.map (fun (bs, c) ->
            let v =
                (uint32 bs.[0])
                ||| (uint32 bs.[1] <<< 8)
                ||| (uint32 bs.[2] <<< 16)
                ||| (uint32 bs.[3] <<< 24)

            v, c)

    /// ビッグエンディアン 8バイトを読む（順序保持された整数の復元）。
    let readInt64BE (c: Cursor) =
        readBytes 8 c
        |> Result.map (fun (bs, c) ->
            let v = bs |> Array.fold (fun acc b -> acc <<< 8 ||| int64 b) 0L
            v, c)

    /// ビッグエンディアン 8バイト（符号なし）。順序保持された整数の復元。
    let readUint64BE (c: Cursor) =
        readBytes 8 c
        |> Result.map (fun (bs, c) ->
            let v = bs |> Array.fold (fun acc b -> acc <<< 8 ||| uint64 b) 0UL
            v, c)

    let readBool (c: Cursor) =
        readByte c |> Result.map (fun (b, c) -> b <> 0uy, c)

    let readLengthPrefixed (c: Cursor) =
        readVarint c
        >>= fun (n, c) ->
            if n > uint64 Int32.MaxValue then
                eof (sprintf "Read: 長すぎる長さ指定 %d" n)
            else
                readBytes (int n) c

    /// 長さ付き UTF-8 文字列を読む（`Writer.str` の逆）。
    let readStr (c: Cursor) =
        readLengthPrefixed c
        |> Result.map (fun (bs, c) -> Text.Encoding.UTF8.GetString bs, c)

    /// 生のバイト列の固定オフセットから、リトルエンディアン 32bit を読む。
    /// SSTable の restart 配列（末尾固定位置）のように、
    /// カーソルを作らず位置を直に指して読む用（境界検査はしない）。
    let readLE32 (data: byte array) (offset: int) =
        let at (i: int) = uint32 data.[offset + i]
        at 0 ||| (at 1 <<< 8) ||| (at 2 <<< 16) ||| (at 3 <<< 24)

    /// `readLE32` の 64bit 版。
    let readLE64 (data: byte array) (offset: int) =
        int64 (readLE32 data offset) ||| (int64 (readLE32 data (offset + 4)) <<< 32)

    /// `count` 件を 1 件ずつ読み出してリストにする。
    /// 「1 件読む」処理は `readOne` に切り出し、ループはここ 1 箇所にまとめる。
    /// 再帰は末尾位置の match で回す（bind を挟むと TCO が効かなくなる）。
    let readItems (count: uint64) (readOne: Cursor -> Result<'a * Cursor, DbError>) (c: Cursor) =
        let rec go (rest: uint64) (c: Cursor) (acc: 'a list) =
            if rest = 0UL then
                ret (List.rev acc, c)
            else
                match readOne c with
                | Error e -> Error e
                | Ok(v, c') -> go (rest - 1UL) c' (v :: acc)

        go count c []

/// 順序保持符号化 (order-preserving encoding)
/// SSTable のマージはキーのバイト列比較で行うので、
/// 「バイト列の大小関係 == SQL の比較結果」でなければならない。
///   * 型タグを SQL 照合順に単調増加させる
///   * 数値・日付・時刻は順序を保った整数に写像してビッグエンディアンで書く
///   * TEXT / BLOB の 0x00 は 0x00 0xFF に escape し終端に 0x00 を置く
///     → 生の前方一致がそのまま正しい接頭辞一致になる
/// 詳細と検証例は docs/architecture.md §5.2。
module internal OrderPreserving =
    /// 型タグ。順序は SQL の照合順に従う。
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

    /// フィールド終端バイト。ペイロード中の 0x00 は必ず escape される。
    [<Literal>]
    let private Terminator = 0x00uy

    /// ペイロード中の 0x00 を escape したときの後続バイト。
    [<Literal>]
    let private EscapeMarker = 0xFFuy

    /// int64 の順序を uint64 の順序に写像する。
    /// 「2^63 を符号なしで足す」ことで、-2^63 が 0 に、+2^63-1 が 2^64-1 に対応し、
    /// 単調増加が保たれる。
    /// 注意: 「負数なら全ビット反転（~~~）」というよくある書き方は
    /// int64 では順序が反転する。-1 のビットパターンが全 1 で絶対値の最大
    /// になるため、反転すると最小になり、-2 より小さくなってしまう。
    /// この書き方が正しく効くのは double（指数部に余裕がある）だけである。
    let private mapInt64 (v: int64) = uint64 v + 0x8000000000000000UL

    let private unmapInt64 (m: uint64) = int64 (m - 0x8000000000000000UL)

    /// double の順序を int64 の順序に写像する（IEEE-754 のビット表現を前提）。
    let private mapDouble (v: float) =
        let bits = BitConverter.DoubleToInt64Bits v
        if bits < 0L then ~~~bits else bits ||| Int64.MinValue

    let private unmapDouble (v: int64) =
        let bits = if v < 0L then v ^^^ Int64.MinValue else ~~~v
        BitConverter.Int64BitsToDouble bits

    let private writeOrderedInt64 (v: int64) (w: Writer) = w |> Writer.uint64BE (mapInt64 v)

    let private readOrderedInt64 (c: Cursor) =
        Read.readUint64BE c |> Result.map (fun (m, c) -> unmapInt64 m, c)

    /// decimal の有効桁数の上限。
    [<Literal>]
    let private DecimalMaxDigits = 29

    /// decimal を順序を保ったバイト列へ写像する。
    /// raw な 128bit ビット列は使えない。
    /// decimal の内部表現は「非スケール値（96bit）＋ スケール（10 進指数）」なので、
    /// 0.000001（スケール 6）と Decimal.MaxValue（スケール 0）のビット列を
    /// そのまま比較しても数値順にならない。
    /// そこで値=value = D x 10^(-s) という正規形（末尾 0 を落とした桁列 D とスケール s）に直して、
    ///   (符号, 10 進指数 e = len(D) - s, D を右詰めした 29 桁)
    /// の順に並べる。同じ符号・同じ指数なら D の右詰め比較が値の比較そのものになる。
    /// 指数は順序保持された 8 バイト整数で書く。
    let private decimalParts (v: decimal) =
        let bits = System.Decimal.GetBits v
        let flags = uint32 bits.[3]
        let negative = flags &&& 0x80000000u <> 0u
        let scale = int (flags >>> 16 &&& 0xFFu)

        let unscaled =
            BigInteger(uint32 bits.[0])
            + (BigInteger(uint32 bits.[1]) <<< 32)
            + (BigInteger(uint32 bits.[2]) <<< 64)

        if unscaled.IsZero then
            false, 0, ""
        else
            // 末尾の 0 を落として、桁列とスケールの組を一意にする
            let rec trim (u: BigInteger) (s: int) =
                if u % BigInteger 10 = BigInteger.Zero then
                    trim (u / BigInteger 10) (s - 1)
                else
                    u, s

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
            if isZero then Int64.MinValue
            elif negative then -int64 exponent
            else int64 exponent

        w
        |> Writer.writeByte (if negative then 0x00uy else 0x01uy)
        |> Writer.writeBytes (Array.init 8 (fun i -> byte (mapInt64 exponentValue >>> 8 * (7 - i) &&& 0xFFuL)))
        |> Writer.writeBytes digitBytes

    let private readOrderedDecimal (c: Cursor) =
        result {
            let! sign, c = Read.readByte c
            let! expBytes, c = Read.readBytes 8 c
            let! digitBytes, c = Read.readBytes DecimalMaxDigits c
            let raw = expBytes |> Array.fold (fun (acc: uint64) b -> acc <<< 8 ||| uint64 b) 0UL
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
                    |> String

                let text = sprintf "%sE%d" digits (exponent - int64 DecimalMaxDigits)

                match System.Decimal.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, d when not negative -> return d, c
                | true, d -> return -d, c
                | _ -> return! Error(DbError.corruption (sprintf "OrderPreserving: decimal を復元できません: %s" text))
        }

    /// Guid を RFC 4122 のビッグエンディアン 16 バイトへ（比較可能にするため）。
    let private guidToOrderedBytes (g: Guid) =
        let s = g.ToString("N", CultureInfo.InvariantCulture)

        [|
            for i in 0..15 do
                yield Convert.ToByte(s.Substring(i * 2, 2), 16)
        |]

    let private orderedBytesToGuid (bs: byte array) =
        let sb = System.Text.StringBuilder 32

        bs
        |> Array.iter (fun b -> sb.Append(b.ToString("X2", CultureInfo.InvariantCulture)) |> ignore)

        Guid.ParseExact(sb.ToString(), "N")

    /// 0x00 を escape しつつペイロードを書き、最後に終端 0x00 を置く。
    let private writeEscapedPayload (bs: byte array) (w: Writer) =
        let escaped =
            bs
            |> Array.collect (fun b -> if b = 0uy then [| Terminator; EscapeMarker |] else [| b |])

        w |> Writer.writeBytes escaped |> Writer.writeByte Terminator

    /// 値をキーへ追加する（型タグ + escape 済みペイロード + 終端）。
    let writeValue (v: SqlValue) (w: Writer) =
        let tagged tag = w |> Writer.writeByte (byte tag)
        let terminate = Writer.writeByte Terminator

        match v with
        | SqlValue.Null -> tagged Tag.Null |> terminate
        | SqlValue.Bool b -> tagged Tag.Bool |> Writer.writeByte (if b then 1uy else 0uy) |> terminate
        | SqlValue.Int i -> tagged Tag.Int |> writeOrderedInt64 i |> terminate
        // キーに書く前に NaN を正規化する（符号ビットが立った NaN は並びが壊れるため）
        | SqlValue.Real f ->
            tagged Tag.Real
            |> Writer.uint64BE (uint64 (mapDouble (SqlValue.normalizeFloat f)))
            |> terminate
        | SqlValue.Decimal d -> tagged Tag.Decimal |> writeOrderedDecimal d |> terminate
        | SqlValue.Date d ->
            let days = d.ToDateTime(TimeOnly.MinValue).Ticks / TimeSpan.TicksPerDay
            tagged Tag.Date |> writeOrderedInt64 days |> terminate
        | SqlValue.Timestamp t -> tagged Tag.Timestamp |> writeOrderedInt64 t.UtcTicks |> terminate
        | SqlValue.Text s -> writeEscapedPayload (Text.Encoding.UTF8.GetBytes s) (tagged Tag.Text)
        | SqlValue.Blob bs -> writeEscapedPayload bs (tagged Tag.Blob)
        | SqlValue.Uuid g -> tagged Tag.Uuid |> Writer.writeBytes (guidToOrderedBytes g) |> terminate

    /// キーに書き込んだ 1 フィールドを読む。
    /// escape を解き、フィールド終端まで消費して値を返す。
    let private readValue (start: Cursor) =
        // ペイロードを既に読んだ後に、フィールド終端を確認して値を返す。
        let finish (v: SqlValue) (c: Cursor) =
            result {
                let! t, _ = Read.readByte c

                if t <> Terminator then
                    return! Error(DbError.corruption (sprintf "OrderPreserving: 終端が不正 (0x%02X)" t))
                else
                    return v, c
            }

        let readEscapedPayload (c: Cursor) =
            let rec go c acc =
                let finish c =
                    Ok(List.rev acc |> List.toArray, { c with Pos = c.Pos - 1 })

                match Read.readByte c with
                | Error e -> Error e
                | Ok(b, c) when b <> Terminator -> go c (b :: acc)
                | Ok(_, c) when Cursor.atEnd c -> finish c
                | Ok(_, c) ->
                    match Read.readByte c with
                    | Error e -> Error e
                    | Ok(EscapeMarker, c) -> go c (Terminator :: acc)
                    | Ok(_, c) -> finish c

            go c []

        result {
            let! tag, c = Read.readByte start

            let unknown =
                Error(DbError.corruption (sprintf "OrderPreserving: 不明な型タグ 0x%02X" tag))

            if tag = byte Tag.Null then
                return! finish SqlValue.Null c
            elif tag = byte Tag.Bool then
                let! v, c = Read.readBool c
                return! finish (SqlValue.Bool v) c
            elif tag = byte Tag.Int then
                let! v, c = readOrderedInt64 c
                return! finish (SqlValue.Int v) c
            elif tag = byte Tag.Real then
                let! m, c = Read.readInt64BE c
                return! finish (SqlValue.Real(SqlValue.normalizeFloat (unmapDouble m))) c
            elif tag = byte Tag.Decimal then
                let! v, c = readOrderedDecimal c
                return! finish (SqlValue.Decimal v) c
            elif tag = byte Tag.Date then
                let! days, c = readOrderedInt64 c
                return! finish (SqlValue.Date(DateOnly.FromDayNumber(int days))) c
            elif tag = byte Tag.Timestamp then
                let! ticks, c = readOrderedInt64 c
                return! finish (SqlValue.Timestamp(DateTimeOffset(ticks, TimeSpan.Zero))) c
            elif tag = byte Tag.Text then
                let! bytes, c = readEscapedPayload c
                return! finish (SqlValue.Text(Text.Encoding.UTF8.GetString bytes)) c
            elif tag = byte Tag.Blob then
                let! bytes, c = readEscapedPayload c
                return! finish (SqlValue.Blob bytes) c
            elif tag = byte Tag.Uuid then
                let! bs, c = Read.readBytes 16 c
                return! finish (SqlValue.Uuid(orderedBytesToGuid bs)) c
            else
                return! unknown
        }

    /// キーから 1 フィールドだけを読む（返り値は値と読み取り後の位置）。
    let readFirstValue (key: byte array) =
        readValue (Cursor.create key) |> Result.map (fun (v, c) -> v, c.Pos)

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

    /// ハッシュ値を攪拌する（splitmix64 相当）。
    /// FNV-1a は下位ビットに偏りがちで、Bloom filter のビット分布が悪くなるため。
    let mix (x: uint64) =
        let h1 = (x ^^^ (x >>> 30)) * 0xBF58476D1CE4E5B9UL
        let h2 = (h1 ^^^ (h1 >>> 27)) * 0x94D049BB133111EBUL
        h2 ^^^ (h2 >>> 31)

/// 複合キーのビルダ。フィールドを順に足して 1 本のバイト列キーにする。
module internal KeyBuilder =
    /// 値の並びを 1 本のキーに符号化する。
    let ofValues (values: SqlValue seq) =
        values
        |> Seq.fold (fun w v -> OrderPreserving.writeValue v w) Writer.empty
        |> Writer.toArray

    /// 索引キー（索引列 + 主キー列）。主キーは行を一意に特定するために末尾へ追加する。
    let ofIndexAndPrimary (indexCols: SqlValue seq) (primaryCols: SqlValue seq) =
        let appendCols (w: Writer) (cols: SqlValue seq) =
            Seq.fold (fun w v -> OrderPreserving.writeValue v w) w cols

        let withIndex = appendCols Writer.empty indexCols
        appendCols withIndex primaryCols |> Writer.toArray

    /// バイト列 key が prefix で始まるか。
    let startsWithPrefix (key: byte array) (prefix: byte array) =
        if key.Length < prefix.Length then
            false
        else
            // && の短絡評価を使うと再帰呼び出しが末尾位置に来ないため、
            // 「一致し切るか / 途中で不一致か」を明示的に分岐する形へ書き換える。
            let rec go i =
                if i >= prefix.Length then true
                elif key.[i] = prefix.[i] then go (i + 1)
                else false

            go 0

/// 行ペイロード用のコンパクト符号化
/// キーと異なり、行値には順序保持が不要なので varint で詰めて小さくする。
module internal ValueCodec =
    let private writeValue (v: SqlValue) (w: Writer) =
        let tagged tag = w |> Writer.writeByte (byte tag)

        match v with
        | SqlValue.Null -> tagged 0uy
        | SqlValue.Bool b -> tagged 1uy |> Writer.bool b
        | SqlValue.Int i -> tagged 2uy |> Writer.int64LE i
        | SqlValue.Real f -> tagged 3uy |> Writer.int64LE (BitConverter.DoubleToInt64Bits f)
        | SqlValue.Decimal d ->
            let withTag = tagged 4uy

            System.Decimal.GetBits d
            |> Array.fold (fun w b -> w |> Writer.int64LE (int64 b)) withTag
        | SqlValue.Text s -> tagged 5uy |> Writer.str s
        | SqlValue.Blob bs -> tagged 6uy |> Writer.lengthPrefixed bs
        | SqlValue.Date d -> tagged 7uy |> Writer.int64LE (d.ToDateTime(TimeOnly.MinValue).Ticks)
        | SqlValue.Timestamp t -> tagged 8uy |> Writer.int64LE t.UtcTicks
        | SqlValue.Uuid g -> tagged 9uy |> Writer.writeBytes (g.ToByteArray())

    let private readValue (c: Cursor) =
        result {
            let! tag, c = Read.readByte c

            if tag = 0uy then
                return SqlValue.Null, c
            elif tag = 1uy then
                let! v, c = Read.readBool c
                return SqlValue.Bool v, c
            elif tag = 2uy then
                let! v, c = Read.readInt64LE c
                return SqlValue.Int v, c
            elif tag = 3uy then
                let! bits, c = Read.readInt64LE c
                return SqlValue.vReal (BitConverter.Int64BitsToDouble bits), c
            elif tag = 4uy then
                let! bs, c = Read.readBytes 32 c

                let bits =
                    [|
                        for i in 0..3 do
                            yield BitConverter.ToInt64(bs, i * 8) |> int
                    |]

                return System.Decimal bits |> SqlValue.Decimal, c
            elif tag = 5uy then
                let! bs, c = Read.readLengthPrefixed c
                return Text.Encoding.UTF8.GetString bs |> SqlValue.Text, c
            elif tag = 6uy then
                let! bs, c = Read.readLengthPrefixed c
                return SqlValue.Blob bs, c
            elif tag = 7uy then
                let! ticks, c = Read.readInt64LE c
                return DateOnly.FromDayNumber(int (ticks / TimeSpan.TicksPerDay)) |> SqlValue.Date, c
            elif tag = 8uy then
                let! ticks, c = Read.readInt64LE c
                return SqlValue.Timestamp(DateTimeOffset(ticks, TimeSpan.Zero)), c
            elif tag = 9uy then
                let! bs, c = Read.readBytes 16 c
                return SqlValue.Uuid(Guid bs), c
            else
                return! Error(DbError.corruption (sprintf "ValueCodec: 不明な型タグ %d" (int tag)))
        }

    /// 値の並びをまとめて書き出す。
    let writeValues (values: SqlValue seq) (w: Writer) =
        let arr = Seq.toArray values
        let withCount = w |> Writer.varint (uint64 arr.Length)
        Array.fold (fun w v -> writeValue v w) withCount arr

    let readValues (c: Cursor) =
        result {
            let! n, c = Read.readVarint c

            if n > uint64 Int32.MaxValue then
                return! Error(DbError.corruption (sprintf "ValueCodec: 要素数が大きすぎます (%d)" n))

            // 件数付きの読み出しループは `Read.readItems` に一本化している。
            let! items, c = Read.readItems n readValue c
            return List.toArray items, c
        }
