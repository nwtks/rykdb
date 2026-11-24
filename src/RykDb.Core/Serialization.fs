namespace RykDb.Core

// System.Decimal は IDisposable ではないが F# が IDisposable を継承した構造体と誤認して FS0760 を出す。
// decimal の再構築はこの形が唯一素直なので範囲を限定して抑制する。
#nowarn "760"

module internal Reader =
    /// 破損として読取り失敗を返す。
    let private eof msg = err (DbError.corruption msg)

    /// 1バイト読む（末尾超過は `CorruptionError`）。
    let readByte (c: Cursor) =
        if Cursor.atEnd c then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            ret ((Cursor.data c).[Cursor.pos c], Cursor.advance 1 c)

    /// `count` バイトを新しい配列へ読む。
    let readBytes (count: int) (c: Cursor) =
        let d = Cursor.data c
        let p = Cursor.pos c
        let e = Cursor.endPos c

        if count < 0 || p < 0 || e < p || e > d.Length || count > e - p then
            eof (sprintf "Reader: %d バイトを読み取れません（残り %d）" count (Cursor.remaining c))
        else
            ret (Array.sub d p count, Cursor.advance count c)

    /// BOOL を 1バイトから読む（0 以外は true）。
    let readBool = readByte >> Result.map (fun (b, c) -> b <> 0uy, c)

    /// 呼び出し側の配列へ読み取る（読み取り用の配列を確保しない）。
    let copyBytesInto (dest: byte array) (destOffset: int) (count: int) (c: Cursor) =
        let d = Cursor.data c
        let p = Cursor.pos c
        let e = Cursor.endPos c

        if
            count < 0
            || destOffset < 0
            || destOffset > dest.Length - count
            || p < 0
            || e < p
            || e > d.Length
            || count > e - p
        then
            eof (sprintf "Reader: %d バイトを読み取れません（残り %d）" count (Cursor.remaining c))
        else
            Array.blit d p dest destOffset count
            ret (Cursor.advance count c)

    /// 読み飛ばす（配列を確保しない）。
    /// 値を使わないブロック走査で、不要なエントリの値コピーを避けるために使う。
    let skipBytes (count: int) (c: Cursor) =
        let d = Cursor.data c
        let p = Cursor.pos c
        let e = Cursor.endPos c

        if count < 0 || p < 0 || e < p || e > d.Length || count > e - p then
            eof (sprintf "Reader: %d バイトを読み飛ばせません（残り %d）" count (Cursor.remaining c))
        else
            ret (Cursor.advance count c)

    /// varint をデコードする本体。
    [<TailCall>]
    let rec private decodeVarintV (shift: int) (acc: uint64) (c: Cursor) =
        if shift > 63 || Cursor.pos c >= Cursor.endPos c then
            ValueNone
        else
            let b = (Cursor.data c).[Cursor.pos c]
            let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)
            let next = Cursor.advance 1 c

            if b &&& 0x80uy <> 0uy then
                decodeVarintV (shift + 7) acc next
            else
                ValueSome struct (acc, next)

    /// 符号なし LEB128 を読む。
    let private readVarintV = decodeVarintV 0 0UL

    /// 符号なし LEB128 を読む。
    let readVarint (c: Cursor) =
        match readVarintV c with
        | ValueSome struct (v, c) -> ret (v, c)
        | ValueNone -> eof "Reader: varint が不正です（長すぎるか末尾を超えました）"

    /// リトルエンディアン 8バイト。
    /// 値ごとに配列を作らない（行のデコードで最も呼ばれる）。
    let readInt64LE (c: Cursor) =
        if Cursor.endPos c - Cursor.pos c < 8 then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            let d = Cursor.data c
            let p = Cursor.pos c

            let v =
                int64 d.[p]
                ||| (int64 d.[p + 1] <<< 8)
                ||| (int64 d.[p + 2] <<< 16)
                ||| (int64 d.[p + 3] <<< 24)
                ||| (int64 d.[p + 4] <<< 32)
                ||| (int64 d.[p + 5] <<< 40)
                ||| (int64 d.[p + 6] <<< 48)
                ||| (int64 d.[p + 7] <<< 56)

            ret (v, Cursor.advance 8 c)

    /// リトルエンディアン 4バイト（符号なし）を読む。
    let readUInt32LE (c: Cursor) =
        if Cursor.endPos c - Cursor.pos c < 4 then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            let d = Cursor.data c
            let p = Cursor.pos c

            let v =
                uint32 d.[p]
                ||| (uint32 d.[p + 1] <<< 8)
                ||| (uint32 d.[p + 2] <<< 16)
                ||| (uint32 d.[p + 3] <<< 24)

            ret (v, Cursor.advance 4 c)

    /// ビッグエンディアン 8バイト（符号なし）。
    /// 順序保持された整数の復元。
    let readUint64BE (c: Cursor) =
        if Cursor.endPos c - Cursor.pos c < 8 then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            let d = Cursor.data c
            let p = Cursor.pos c

            let v =
                uint64 d.[p] <<< 56
                ||| (uint64 d.[p + 1] <<< 48)
                ||| (uint64 d.[p + 2] <<< 40)
                ||| (uint64 d.[p + 3] <<< 32)
                ||| (uint64 d.[p + 4] <<< 24)
                ||| (uint64 d.[p + 5] <<< 16)
                ||| (uint64 d.[p + 6] <<< 8)
                ||| uint64 d.[p + 7]

            ret (v, Cursor.advance 8 c)

    /// 長さ付きバイト列を読む（`Writer.lengthPrefixed` の逆）。
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

    /// 残り件数だけ読み出してリストへ積む本体。
    [<TailCall>]
    let rec private readItemsLoop
        (readOne: Cursor -> Result<'a * Cursor, DbError>)
        (rest: uint64)
        (acc: 'a list)
        (c: Cursor)
        =
        if rest = 0UL then
            ret (List.rev acc, c)
        else
            match readOne c with
            | Ok(v, c') -> readItemsLoop readOne (rest - 1UL) (v :: acc) c'
            | Error e -> err e

    /// count 件を 1件ずつ読み出してリストにする。
    /// 「1件読む」処理は readOne に切り出し、ループはここ 1箇所にまとめる。
    let readItems (readOne: Cursor -> Result<'a * Cursor, DbError>) (count: uint64) (c: Cursor) =
        readItemsLoop readOne count [] c

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

    /// int64 を順序保持で書く（ビッグエンディアン）。
    let private writeOrderedInt64 (v: int64) (w: Writer) = w |> Writer.uint64BE (mapInt64 v)

    /// decimal の有効桁数の上限。
    [<Literal>]
    let private DecimalMaxDigits = 29

    /// 96bit の非スケール値を 10 で割った商と余り。
    /// `BigInteger` を使わずに DECIMAL の桁列を作るための部品。
    /// 32bit のチャンクごとに「前のチャンクの余り × 2^32 + 次」を割る。
    let private div10 (hi: uint32) (mid: uint32) (lo: uint32) =
        let mid64 = uint64 (hi % 10u) <<< 32 ||| uint64 mid
        let lo64 = uint64 (uint32 (mid64 % 10UL)) <<< 32 ||| uint64 lo
        hi / 10u, uint32 (mid64 / 10UL), uint32 (lo64 / 10UL), uint32 (lo64 % 10UL)

    /// 96bit 非スケール値の桁を buffer の後ろから詰める。
    [<TailCall>]
    let rec private fillUnscaledDigits (buffer: byte array) (hi: uint32) (mid: uint32) (lo: uint32) (pos: int) =
        if hi = 0u && mid = 0u && lo = 0u then
            pos
        else
            let hi', mid', lo', digit = div10 hi mid lo
            buffer.[pos] <- byte digit
            fillUnscaledDigits buffer hi' mid' lo' (pos - 1)

    /// 末尾の 0 を落として桁数を縮める。
    [<TailCall>]
    let rec private trimUnscaledDigits (buffer: byte array) (start: int) (count: int) =
        if count > 1 && buffer.[start + count - 1] = 0uy then
            trimUnscaledDigits buffer start (count - 1)
        else
            count

    /// 96bit 非スケール値の 10進桁を `Buffer.[Start]` から `Count` 個返す。
    /// 末尾の 0 は落として `Count` を縮め、落とす前の桁数を第4要素で返す。
    /// 指数は「元の桁数 - スケール」なので、落とした 0 は指数に影響しない。
    let private unscaledDigits (hi: uint32) (mid: uint32) (lo: uint32) =
        let buffer = Array.zeroCreate<byte> DecimalMaxDigits
        let start = fillUnscaledDigits buffer hi mid lo (DecimalMaxDigits - 1) + 1
        let total = DecimalMaxDigits - start

        buffer, start, trimUnscaledDigits buffer start total, total

    /// DECIMAL を符号・指数・桁の補数で書く（順序保持）。
    let private writeOrderedDecimal (v: decimal) (w: Writer) =
        let bits = System.Decimal.GetBits v
        let flags = uint32 bits.[3]
        let scale = int (flags >>> 16 &&& 0xFFu)

        let buffer, start, count, total =
            unscaledDigits (uint32 bits.[2]) (uint32 bits.[1]) (uint32 bits.[0])

        let isZero = total = 0
        let negative = not isZero && flags &&& 0x80000000u <> 0u

        let digitBytes =
            Array.init DecimalMaxDigits (fun i ->
                let digit = if i < count then buffer.[start + i] else 0uy
                if negative then 9uy - digit else digit)

        let exponentValue =
            if isZero then
                System.Int64.MinValue
            else
                let exponent = int64 total - int64 scale
                if negative then -exponent else exponent

        w
        |> Writer.writeByte (if negative then 0x00uy else 0x01uy)
        |> writeOrderedInt64 exponentValue
        |> Writer.writeBytes digitBytes

    /// GUID を 16 バイトの生バイト列にする。
    let private guidToOrderedBytes (g: System.Guid) =
        let s = g.ToString("N", System.Globalization.CultureInfo.InvariantCulture)

        [|
            for i in 0..15 do
                yield System.Convert.ToByte(s.Substring(i * 2, 2), 16)
        |]

    /// 0x00 を escape してペイロードと終端を書き込む。
    let private writeEscapedPayload (bs: byte array) (w: Writer) =
        let escaped =
            bs
            |> Array.collect (fun b -> if b = 0uy then [| Terminator; EscapeMarker |] else [| b |])

        w |> Writer.writeBytes escaped |> Writer.writeByte Terminator

    /// 型タグを書き込む。
    let private writeTaggedValue (w: Writer) (tag: Tag) = w |> Writer.writeByte (byte tag)

    /// フィールド終端を書き込む。
    let private writeTerminator = Writer.writeByte Terminator

    /// 値をキーへ追加する（型タグ + escape 済みペイロード + 終端）。
    /// `SqlValue.Unloaded`（列マスクのプレースホルダ）は符号化できないため `InternalError` を返す。
    let writeValue (v: SqlValue) (w: Writer) =
        match v with
        | SqlValue.Null -> ret (writeTaggedValue w Tag.Null |> writeTerminator)
        | SqlValue.Bool b ->
            ret (
                writeTaggedValue w Tag.Bool
                |> Writer.writeByte (if b then 1uy else 0uy)
                |> writeTerminator
            )
        | SqlValue.Int i -> ret (writeTaggedValue w Tag.Int |> writeOrderedInt64 i |> writeTerminator)
        | SqlValue.Real f ->
            let bits =
                writeTaggedValue w Tag.Real
                |> Writer.uint64BE (uint64 (mapDouble (SqlValue.normalizeFloat f)))

            ret (bits |> writeTerminator)
        | SqlValue.Decimal d -> ret (writeTaggedValue w Tag.Decimal |> writeOrderedDecimal d |> writeTerminator)
        | SqlValue.Date d ->
            let days =
                d.ToDateTime(System.TimeOnly.MinValue).Ticks / System.TimeSpan.TicksPerDay

            ret (writeTaggedValue w Tag.Date |> writeOrderedInt64 days |> writeTerminator)
        | SqlValue.Timestamp t ->
            ret (
                writeTaggedValue w Tag.Timestamp
                |> writeOrderedInt64 t.UtcTicks
                |> writeTerminator
            )
        | SqlValue.Text s ->
            ret (writeEscapedPayload (System.Text.Encoding.UTF8.GetBytes s) (writeTaggedValue w Tag.Text))
        | SqlValue.Blob bs -> ret (writeEscapedPayload bs (writeTaggedValue w Tag.Blob))
        | SqlValue.Uuid g ->
            ret (
                writeTaggedValue w Tag.Uuid
                |> Writer.writeBytes (guidToOrderedBytes g)
                |> writeTerminator
            )
        | SqlValue.Unloaded -> err (DbError.internalError "SqlValue.Unloaded は符号化できません")

    /// 固定長フィールドの終端（0x00）を検証する。
    let private expectTerminator (c: Cursor) =
        Reader.readByte c
        >>= fun (b, c') ->
            if b = 0uy then
                ret c'
            else
                err (DbError.corruption (sprintf "OrderPreserving: 終端が不正です (0x%02X)" b))

    /// int64 の順序写像を戻す。
    let private unmapInt64 (u: uint64) = int64 (u - 0x8000000000000000UL)

    /// double の順序写像を戻す（NaN は正規化前のビット列のまま戻る）。
    let private unmapDouble (u: uint64) =
        let bits =
            if u >= 0x8000000000000000UL then
                int64 (u - 0x8000000000000000UL)
            else
                ~~~(int64 u)

        System.BitConverter.Int64BitsToDouble bits

    /// 終端を探す（0x00 0xFF は escape として飛ばす）。
    [<TailCall>]
    let rec private scanEscapedPayload (d: byte array) (finish: int) (i: int) (length: int) =
        if i >= finish then
            ValueNone
        elif d.[i] = 0uy then
            if i + 1 < finish && d.[i + 1] = 0xFFuy then
                scanEscapedPayload d finish (i + 2) (length + 1)
            else
                ValueSome struct (i, length)
        else
            scanEscapedPayload d finish (i + 1) (length + 1)

    /// escape を戻して出力配列へ詰める。
    [<TailCall>]
    let rec private fillEscapedPayload (d: byte array) (out: byte array) (terminator: int) (i: int) (j: int) =
        if i >= terminator then
            ()
        elif d.[i] = 0uy then
            out.[j] <- 0uy
            fillEscapedPayload d out terminator (i + 2) (j + 1)
        else
            out.[j] <- d.[i]
            fillEscapedPayload d out terminator (i + 1) (j + 1)

    /// escape されたペイロードを読む（0x00 0xFF を 0x00 に戻す）。
    /// 0xFF 以外が続く 0x00 を終端として扱う。
    let private readEscapedPayload (c: Cursor) =
        let d = Cursor.data c
        let start = Cursor.pos c
        let finish = Cursor.endPos c

        match scanEscapedPayload d finish start 0 with
        | ValueNone -> err (DbError.corruption "OrderPreserving: 文字列の終端がありません")
        | ValueSome struct (terminator, length) ->
            let out = Array.zeroCreate<byte> length
            fillEscapedPayload d out terminator start 0
            ret (out, Cursor.advance (terminator + 1 - start) c)

    /// 補数で書かれた 1桁を 0..9 に戻す。
    let private decimalDigit (negative: bool) (digits: byte array) (i: int) =
        let d = if negative then 9 - int digits.[i] else int digits.[i]
        if d < 0 || d > 9 then ValueNone else ValueSome d

    /// 29桁すべてが 0..9 の範囲か検査する。
    [<TailCall>]
    let rec private decimalDigitsValid (negative: bool) (digits: byte array) (i: int) =
        if i >= DecimalMaxDigits then
            ValueSome()
        else
            match decimalDigit negative digits i with
            | ValueSome _ -> decimalDigitsValid negative digits (i + 1)
            | ValueNone -> ValueNone

    /// 最後の非ゼロ桁の位置を返す。
    [<TailCall>]
    let rec private lastNonZeroDigit (negative: bool) (digits: byte array) (i: int) =
        if i < 0 then
            -1
        else
            match decimalDigit negative digits i with
            | ValueSome d when d <> 0 -> i
            | _ -> lastNonZeroDigit negative digits (i - 1)

    /// 桁列を 96bit の非スケール値へ畳む。
    [<TailCall>]
    let rec private accumulateDigits
        (negative: bool)
        (digits: byte array)
        (total: int)
        (i: int)
        (hi: uint64)
        (mid: uint64)
        (lo: uint64)
        =
        if i >= total then
            struct (hi, mid, lo)
        else
            match decimalDigit negative digits i with
            | ValueNone -> struct (hi, mid, lo)
            | ValueSome d ->
                let lo' = lo * 10UL + uint64 d
                let mid' = mid * 10UL + (lo' >>> 32)
                let hi' = hi * 10UL + (mid' >>> 32)
                accumulateDigits negative digits total (i + 1) hi' (mid' &&& 0xFFFFFFFFUL) (lo' &&& 0xFFFFFFFFUL)

    /// DECIMAL を復号する（符号・指数・29 桁の補数）。
    let private readOrderedDecimal (c: Cursor) =
        Reader.readByte c
        >>= fun (sign, c1) ->
            Reader.readUint64BE c1
            >>= fun (exponentBits, c2) ->
                Reader.readBytes DecimalMaxDigits c2
                >>= fun (digits, c3) ->
                    expectTerminator c3
                    >>= fun c4 ->
                        if sign <> 0uy && sign <> 1uy then
                            err (DbError.corruption "OrderPreserving: DECIMAL の符号が不正です")
                        else
                            let negative = sign = 0uy

                            match decimalDigitsValid negative digits 0 with
                            | ValueNone -> err (DbError.corruption "OrderPreserving: DECIMAL の桁が不正です")
                            | ValueSome() ->
                                let total = lastNonZeroDigit negative digits (DecimalMaxDigits - 1) + 1

                                if total = 0 then
                                    ret (System.Decimal.Zero, c4)
                                else
                                    let rawExponent = unmapInt64 exponentBits

                                    if rawExponent = System.Int64.MinValue then
                                        err (DbError.corruption "OrderPreserving: DECIMAL の指数が不正です")
                                    else
                                        let exponent = if negative then -rawExponent else rawExponent
                                        let scale = int64 total - exponent

                                        if scale < 0L || scale > 28L then
                                            err (DbError.corruption "OrderPreserving: DECIMAL のスケールが不正です")
                                        else
                                            let struct (hi, mid, lo) =
                                                accumulateDigits negative digits total 0 0UL 0UL 0UL

                                            if hi > 0xFFFFFFFFUL then
                                                err (DbError.corruption "OrderPreserving: DECIMAL の桁数が大きすぎます")
                                            else
                                                let flags =
                                                    uint32 (int scale) <<< 16 ||| if negative then 0x80000000u else 0u

                                                let bits =
                                                    [| int (uint32 lo); int (uint32 mid); int (uint32 hi); int flags |]

                                                ret (System.Decimal bits, c4)

    /// キーの 1値を復号する。
    let private readValue (c: Cursor) =
        Reader.readByte c
        >>= fun (tag, c1) ->
            match tag with
            | 0x00uy -> expectTerminator c1 >>= fun c2 -> ret (SqlValue.Null, c2)
            | 0x10uy ->
                Reader.readByte c1
                >>= fun (b, c2) -> expectTerminator c2 >>= fun c3 -> ret (SqlValue.ofBool (b <> 0uy), c3)
            | 0x20uy ->
                Reader.readUint64BE c1
                >>= fun (u, c2) -> expectTerminator c2 >>= fun c3 -> ret (SqlValue.Int(unmapInt64 u), c3)
            | 0x21uy ->
                Reader.readUint64BE c1
                >>= fun (u, c2) -> expectTerminator c2 >>= fun c3 -> ret (SqlValue.Real(unmapDouble u), c3)
            | 0x22uy -> readOrderedDecimal c1 |> Result.map (fun (d, c2) -> SqlValue.Decimal d, c2)
            | 0x30uy ->
                Reader.readUint64BE c1
                >>= fun (u, c2) ->
                    expectTerminator c2
                    >>= fun c3 ->
                        let days = unmapInt64 u

                        if
                            days < int64 System.DateOnly.MinValue.DayNumber
                            || days > int64 System.DateOnly.MaxValue.DayNumber
                        then
                            err (DbError.corruption "OrderPreserving: DATE の範囲が不正です")
                        else
                            ret (SqlValue.Date(System.DateOnly.FromDayNumber(int days)), c3)
            | 0x40uy ->
                Reader.readUint64BE c1
                >>= fun (u, c2) ->
                    expectTerminator c2
                    >>= fun c3 ->
                        let ticks = unmapInt64 u

                        if ticks < 0L || ticks > System.DateTimeOffset.MaxValue.Ticks then
                            err (DbError.corruption "OrderPreserving: TIMESTAMP の範囲が不正です")
                        else
                            ret (SqlValue.Timestamp(System.DateTimeOffset(ticks, System.TimeSpan.Zero)), c3)
            | 0x50uy ->
                readEscapedPayload c1
                |> Result.map (fun (bs, c2) -> SqlValue.Text(System.Text.Encoding.UTF8.GetString bs), c2)
            | 0x60uy -> readEscapedPayload c1 |> Result.map (fun (bs, c2) -> SqlValue.Blob bs, c2)
            | 0x70uy ->
                Reader.readBytes 16 c1
                >>= fun (bs, c2) ->
                    expectTerminator c2
                    |> Result.map (fun c3 ->
                        let text = System.Convert.ToHexString bs
                        SqlValue.Uuid(System.Guid text), c3)
            | _ -> err (DbError.corruption (sprintf "OrderPreserving: 不明な型タグ %d" (int tag)))

    /// 値を count 件復号する。
    /// `mask` が Some のとき、false の位置は値を作らず `SqlValue.Unloaded` を置く。
    [<TailCall>]
    let rec private readValuesLoop (mask: bool array option) (count: int) (i: int) (acc: SqlValue list) (c: Cursor) =
        if i >= count then
            ret (List.rev acc |> Array.ofList, c)
        else
            let materialize =
                match mask with
                | Some m when i < m.Length && not m.[i] -> false
                | _ -> true

            match readValue c with
            | Ok(v, c') ->
                let value = if materialize then v else SqlValue.Unloaded
                readValuesLoop mask count (i + 1) (value :: acc) c'
            | Error e -> err e

    /// 値を count 件復号する（末尾の余りは消費しない）。
    let readValues (count: int) (c: Cursor) =
        if count < 0 then
            err (DbError.corruption "OrderPreserving: 復号する値の個数が負です")
        else
            readValuesLoop None count 0 [] c

    /// マスク付きで値を count 件復号する。
    let readValuesMasked (mask: bool array) (count: int) (c: Cursor) =
        if count < 0 then
            err (DbError.corruption "OrderPreserving: 復号する値の個数が負です")
        else
            readValuesLoop (Some mask) count 0 [] c
