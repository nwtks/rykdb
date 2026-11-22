namespace RykDb.Core

// System.Decimal は IDisposable ではないが F# が IDisposable を継承した構造体と誤認して FS0760 を出す。
// decimal の再構築はこの形が唯一素直なので範囲を限定して抑制する。
#nowarn "760"

module internal Reader =
    let private eof msg = err (DbError.corruption msg)

    let readByte (c: Cursor) =
        if Cursor.atEnd c then
            eof "Reader: 読み取り位置が末尾を超えました"
        else
            ret ((Cursor.data c).[Cursor.pos c], Cursor.advance 1 c)

    let readBytes (count: int) (c: Cursor) =
        let d = Cursor.data c
        let p = Cursor.pos c
        let e = Cursor.endPos c

        if count < 0 || p < 0 || e < p || e > d.Length || count > e - p then
            eof (sprintf "Reader: %d バイトを読み取れません（残り %d）" count (Cursor.remaining c))
        else
            ret (Array.sub d p count, Cursor.advance count c)

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

    /// 符号なし LEB128 を読む。
    let private readVarintV =
        let rec decode shift acc c =
            if shift > 63 || Cursor.pos c >= Cursor.endPos c then
                ValueNone
            else
                let b = (Cursor.data c).[Cursor.pos c]
                let acc = acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)
                let next = Cursor.advance 1 c

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
                | Ok(v, c') -> readRemaining (rest - 1UL) (v :: acc) c'
                | Error e -> err e

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

    /// 96bit の非スケール値を 10 で割った商と余り。
    /// `BigInteger` を使わずに DECIMAL の桁列を作るための部品。
    /// 32bit のチャンクごとに「前のチャンクの余り × 2^32 + 次」を割る。
    let private div10 (hi: uint32) (mid: uint32) (lo: uint32) =
        let mid64 = uint64 (hi % 10u) <<< 32 ||| uint64 mid
        let lo64 = uint64 (uint32 (mid64 % 10UL)) <<< 32 ||| uint64 lo
        hi / 10u, uint32 (mid64 / 10UL), uint32 (lo64 / 10UL), uint32 (lo64 % 10UL)

    /// 96bit 非スケール値の 10進桁を `Buffer.[Start]` から `Count` 個返す。
    /// 末尾の 0 は落として `Count` を縮め、落とす前の桁数を第4要素で返す。
    /// 指数は「元の桁数 - スケール」なので、落とした 0 は指数に影響しない。
    let private unscaledDigits (hi: uint32) (mid: uint32) (lo: uint32) =
        let buffer = Array.zeroCreate<byte> DecimalMaxDigits

        let rec fill hi mid lo pos =
            if hi = 0u && mid = 0u && lo = 0u then
                pos
            else
                let hi', mid', lo', digit = div10 hi mid lo
                buffer.[pos] <- byte digit
                fill hi' mid' lo' (pos - 1)

        let start = fill hi mid lo (DecimalMaxDigits - 1) + 1
        let total = DecimalMaxDigits - start

        let rec trim count =
            if count > 1 && buffer.[start + count - 1] = 0uy then
                trim (count - 1)
            else
                count

        buffer, start, trim total, total

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

    let private guidToOrderedBytes (g: System.Guid) =
        let s = g.ToString("N", System.Globalization.CultureInfo.InvariantCulture)

        [|
            for i in 0..15 do
                yield System.Convert.ToByte(s.Substring(i * 2, 2), 16)
        |]

    /// 値をキーへ追加する（型タグ + escape 済みペイロード + 終端）。
    /// `SqlValue.Unloaded`（列マスクのプレースホルダ）は符号化できないため `InternalError` を返す。
    let writeValue (v: SqlValue) (w: Writer) =
        let writeEscapedPayload bs w =
            let escaped =
                bs
                |> Array.collect (fun b -> if b = 0uy then [| Terminator; EscapeMarker |] else [| b |])

            w |> Writer.writeBytes escaped |> Writer.writeByte Terminator

        let tagged tag = w |> Writer.writeByte (byte tag)
        let terminate = Writer.writeByte Terminator

        match v with
        | SqlValue.Null -> ret (tagged Tag.Null |> terminate)
        | SqlValue.Bool b -> ret (tagged Tag.Bool |> Writer.writeByte (if b then 1uy else 0uy) |> terminate)
        | SqlValue.Int i -> ret (tagged Tag.Int |> writeOrderedInt64 i |> terminate)
        | SqlValue.Real f ->
            let bits =
                tagged Tag.Real
                |> Writer.uint64BE (uint64 (mapDouble (SqlValue.normalizeFloat f)))

            ret (bits |> terminate)
        | SqlValue.Decimal d -> ret (tagged Tag.Decimal |> writeOrderedDecimal d |> terminate)
        | SqlValue.Date d ->
            let days =
                d.ToDateTime(System.TimeOnly.MinValue).Ticks / System.TimeSpan.TicksPerDay

            ret (tagged Tag.Date |> writeOrderedInt64 days |> terminate)
        | SqlValue.Timestamp t -> ret (tagged Tag.Timestamp |> writeOrderedInt64 t.UtcTicks |> terminate)
        | SqlValue.Text s -> ret (writeEscapedPayload (System.Text.Encoding.UTF8.GetBytes s) (tagged Tag.Text))
        | SqlValue.Blob bs -> ret (writeEscapedPayload bs (tagged Tag.Blob))
        | SqlValue.Uuid g -> ret (tagged Tag.Uuid |> Writer.writeBytes (guidToOrderedBytes g) |> terminate)
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

    /// double の順序写像を戻す。
    let private unmapDouble (u: uint64) =
        let bits =
            if u >= 0x8000000000000000UL then
                int64 (u - 0x8000000000000000UL)
            else
                ~~~(int64 u)

        System.BitConverter.Int64BitsToDouble bits

    /// escape されたペイロードを読む（0x00 0xFF を 0x00 に戻す）。
    /// 0xFF 以外が続く 0x00 を終端として扱う。
    let private readEscapedPayload (c: Cursor) =
        let d = Cursor.data c
        let start = Cursor.pos c
        let finish = Cursor.endPos c

        let rec scan i length =
            if i >= finish then
                ValueNone
            elif d.[i] = 0uy then
                if i + 1 < finish && d.[i + 1] = 0xFFuy then
                    scan (i + 2) (length + 1)
                else
                    ValueSome struct (i, length)
            else
                scan (i + 1) (length + 1)

        let rec fill (out: byte array) terminator i j =
            if i >= terminator then
                ()
            elif d.[i] = 0uy then
                out.[j] <- 0uy
                fill out terminator (i + 2) (j + 1)
            else
                out.[j] <- d.[i]
                fill out terminator (i + 1) (j + 1)

        match scan start 0 with
        | ValueNone -> err (DbError.corruption "OrderPreserving: 文字列の終端がありません")
        | ValueSome struct (terminator, length) ->
            let out = Array.zeroCreate<byte> length
            fill out terminator start 0
            ret (out, Cursor.advance (terminator + 1 - start) c)

    /// DECIMAL を復号する（符号・指数・29 桁の補数）。
    let private readOrderedDecimal (c: Cursor) =
        let digit negative (digits: byte array) i =
            let d = if negative then 9 - int digits.[i] else int digits.[i]
            if d < 0 || d > 9 then ValueNone else ValueSome d

        let rec digitsValid negative digits i =
            if i >= DecimalMaxDigits then
                ValueSome()
            else
                match digit negative digits i with
                | ValueSome _ -> digitsValid negative digits (i + 1)
                | ValueNone -> ValueNone

        let rec lastNonZero negative digits i =
            if i < 0 then
                -1
            else
                match digit negative digits i with
                | ValueSome d when d <> 0 -> i
                | _ -> lastNonZero negative digits (i - 1)

        let rec accumulate negative digits total i hi mid lo =
            if i >= total then
                struct (hi, mid, lo)
            else
                match digit negative digits i with
                | ValueNone -> struct (hi, mid, lo)
                | ValueSome d ->
                    let lo' = lo * 10UL + uint64 d
                    let mid' = mid * 10UL + (lo' >>> 32)
                    let hi' = hi * 10UL + (mid' >>> 32)
                    accumulate negative digits total (i + 1) hi' (mid' &&& 0xFFFFFFFFUL) (lo' &&& 0xFFFFFFFFUL)

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

                            match digitsValid negative digits 0 with
                            | ValueNone -> err (DbError.corruption "OrderPreserving: DECIMAL の桁が不正です")
                            | ValueSome() ->
                                let total = lastNonZero negative digits (DecimalMaxDigits - 1) + 1

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
                                            let struct (hi, mid, lo) = accumulate negative digits total 0 0UL 0UL 0UL

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

module internal Hashing =
    /// バイト列の 64bit ハッシュ（FNV-1a）。
    /// Bloom filter のビット位置を決めるのに使う。
    /// キー自体とは独立に、値の並びには影響しない。
    let hashBytes (bs: byte array) =
        let offsetBasis = 14695981039346656037UL
        let prime = 1099511628211UL
        Array.fold (fun h b -> (h ^^^ uint64 b) * prime) offsetBasis bs

    // ---- CRC32C（Castagnoli）----
    // 生成多項式の反射表現は 0x82F63B78。
    // WAL のフレーム、MANIFEST のレコード、SSTable のブロック / メタデータ / Footer の破損検出に使う
    // （FNV-1a は分布用途の hashBytes にのみ残す）。
    // slicing-by-8 で 1回に 8バイト進める。

    let private crcTable0 =
        let rec step n c =
            if n = 0 then
                c
            elif c &&& 1u <> 0u then
                step (n - 1) ((c >>> 1) ^^^ 0x82F63B78u)
            else
                step (n - 1) (c >>> 1)

        Array.init 256 (fun i -> step 8 (uint32 i))

    /// slicing-by-8 用の 8テーブル。`Tables.[k].[i]` は k バイト後ろの寄与。
    let private crcTables =
        let next (prev: uint32 array) =
            Array.init 256 (fun i -> (prev.[i] >>> 8) ^^^ crcTable0.[int (prev.[i] &&& 0xFFu)])

        let rec build n acc =
            match acc with
            | prev :: _ when n > 0 -> build (n - 1) (next prev :: acc)
            | _ -> acc |> List.rev |> Array.ofList

        build 7 [ crcTable0 ]

    /// 初期値と最終反転（CRC32C の標準パラメータ）。
    [<Literal>]
    let private CrcInitial = 0xFFFFFFFFu

    [<Literal>]
    let private CrcFinalXor = 0xFFFFFFFFu

    /// 範囲 [offset, offset + length) を 1つの連続入力として CRC を進める。
    /// チャンクをまたぐ場合は前のチャンクの戻り値をそのまま渡す。
    let private crcUpdate (crc: uint32) (data: byte array) (offset: int) (length: int) =
        let finish = offset + length

        let rec loop i crc =
            if i + 8 <= finish then
                let v =
                    System.Buffers.Binary.BinaryPrimitives.ReadUInt64LittleEndian(System.ReadOnlySpan<byte>(data, i, 8))
                    ^^^ uint64 crc

                crcTables.[7].[int (v &&& 0xFFUL)]
                ^^^ crcTables.[6].[int (v >>> 8 &&& 0xFFUL)]
                ^^^ crcTables.[5].[int (v >>> 16 &&& 0xFFUL)]
                ^^^ crcTables.[4].[int (v >>> 24 &&& 0xFFUL)]
                ^^^ crcTables.[3].[int (v >>> 32 &&& 0xFFUL)]
                ^^^ crcTables.[2].[int (v >>> 40 &&& 0xFFUL)]
                ^^^ crcTables.[1].[int (v >>> 48 &&& 0xFFUL)]
                ^^^ crcTables.[0].[int (v >>> 56 &&& 0xFFUL)]
                |> loop (i + 8)
            elif i < finish then
                loop (i + 1) (crcTables.[0].[int (crc ^^^ uint32 data.[i] &&& 0xFFu)] ^^^ (crc >>> 8))
            else
                crc

        loop offset crc

    /// バイト列の 32bit チェックサム（CRC32C / Castagnoli）。
    let checksum (data: byte array) =
        CrcFinalXor ^^^ crcUpdate CrcInitial data 0 data.Length

    /// 範囲指定の 32bit チェックサム（CRC32C）。
    /// SSTable のブロック末尾チェックサムを、コピーを作らずに計算する。
    let checksumRange (data: byte array) (offset: int) (length: int) =
        CrcFinalXor ^^^ crcUpdate CrcInitial data offset length

    /// 複数のチャンクを連結したものの 32bit チェックサム（CRC32C）。
    /// SSTable のメタデータ（meta / Bloom / index）をコピーせずに検証する。
    let checksumChunks (chunks: byte array array) =
        let rec loop i crc =
            if i >= chunks.Length then
                crc
            else
                loop (i + 1) (crcUpdate crc chunks.[i] 0 chunks.[i].Length)

        CrcFinalXor ^^^ loop 0 CrcInitial

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
    /// `SqlValue.Unloaded` が混ざっている場合は `InternalError`。
    let ofValues (values: SqlValue seq) =
        values
        |> foldOk (fun w v -> OrderPreserving.writeValue v w) Writer.Empty
        |> Result.map Writer.toArray

    /// 省略可能な値の並びを符号化する（`None` はそのまま `None`）。
    let ofOptionalValues (values: SqlValue array option) =
        match values with
        | Some vs -> ofValues vs |> Result.map Some
        | None -> ret None

    /// 索引キー（索引列 + 主キー列）。
    /// 主キーは行を一意に特定するために末尾へ追加する。
    let ofIndexAndPrimary (indexCols: SqlValue seq) (primaryCols: SqlValue seq) =
        let appendCols = foldOk (fun w v -> OrderPreserving.writeValue v w)

        appendCols Writer.Empty indexCols
        >>= fun withIndex -> appendCols withIndex primaryCols |> Result.map Writer.toArray

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
    /// compareValuesV が両者を等しいと扱うため、
    /// DISTINCT などの符号化でも同じビット列にして重複と見なす。
    let private normalizeDecimal (d: decimal) = if d = 0M then 0M else d

    let private writeValue (w: Writer) =
        let tagged tag = w |> Writer.writeByte (byte tag)

        function
        | SqlValue.Null -> ret (tagged 0uy)
        | SqlValue.Bool b -> ret (tagged 1uy |> Writer.bool b)
        | SqlValue.Int i -> ret (tagged 2uy |> Writer.int64LE i)
        | SqlValue.Real f ->
            let bits =
                tagged 3uy
                |> Writer.int64LE (SqlValue.normalizeFloat f |> System.BitConverter.DoubleToInt64Bits)

            ret bits
        | SqlValue.Decimal d ->
            let unfolded =
                normalizeDecimal d
                |> System.Decimal.GetBits
                |> Array.fold (fun w b -> w |> Writer.int64LE (int64 b)) (tagged 4uy)

            ret unfolded
        | SqlValue.Text s -> ret (tagged 5uy |> Writer.str s)
        | SqlValue.Blob bs -> ret (tagged 6uy |> Writer.lengthPrefixed bs)
        | SqlValue.Date d -> ret (tagged 7uy |> Writer.int64LE (d.ToDateTime(System.TimeOnly.MinValue).Ticks))
        | SqlValue.Timestamp t -> ret (tagged 8uy |> Writer.int64LE t.UtcTicks)
        | SqlValue.Uuid g -> ret (tagged 9uy |> Writer.writeBytes (g.ToByteArray()))
        | SqlValue.Unloaded -> err (DbError.internalError "SqlValue.Unloaded は符号化できません")

    let private readValueSlow (c: Cursor) =
        result {
            let! tag, c = Reader.readByte c

            match tag with
            | 0uy -> return SqlValue.Null, c
            | 1uy ->
                let! v, c = Reader.readBool c
                return SqlValue.ofBool v, c
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
            | 10uy -> return SqlValue.Unloaded, c
            | _ -> return! err (sprintf "ValueCodec: 不明な型タグ %d" (int tag) |> DbError.corruption)
        }

    /// 値の並びをまとめて書き出す。
    /// `SqlValue.Unloaded` が混ざっている場合は `InternalError`。
    let writeValues (values: SqlValue seq) (w: Writer) =
        let arr = Seq.toArray values
        let withCount = w |> Writer.varint (uint64 arr.Length)
        foldOk writeValue withCount arr

    /// 一時ファイル（スピル）用: `SqlValue.Unloaded` を含む行も書き出す。
    /// <remarks>
    /// 列マスクで「不要」とされた列は `Unloaded` のまま行に残る。
    /// 退避した行を読み戻したときに列位置がずれないよう、タグ 10 のプレースホルダとして往復させる。
    /// 永続化（WAL / SSTable）には使わないこと
    /// （`writeValues` は `Unloaded` を拒否する）。
    /// </remarks>
    let writeValuesSparse (values: SqlValue seq) (w: Writer) =
        let arr = Seq.toArray values
        let withCount = w |> Writer.varint (uint64 arr.Length)

        let write w =
            function
            | SqlValue.Unloaded -> ret (w |> Writer.writeByte 10uy)
            | v -> writeValue w v

        foldOk write withCount arr

    let private writeInt64LETo (out: byte array) (pos: int) (v: int64) =
        let u = uint64 v

        for i in 0..7 do
            out.[pos + i] <- byte (u >>> 8 * i &&& 0xFFUL)

        pos + 8

    /// 1値を出力配列へ直接書く。次の位置を返す。
    /// エラー時は `out` の内容が不定になる（呼び出し側はバッファを破棄する）。
    let private writeValueTo (out: byte array) (pos: int) =
        function
        | SqlValue.Null ->
            out.[pos] <- 0uy
            ret (pos + 1)
        | SqlValue.Bool b ->
            out.[pos] <- 1uy
            out.[pos + 1] <- if b then 1uy else 0uy
            ret (pos + 2)
        | SqlValue.Int i ->
            out.[pos] <- 2uy
            ret (writeInt64LETo out (pos + 1) i)
        | SqlValue.Real f ->
            out.[pos] <- 3uy
            ret (writeInt64LETo out (pos + 1) (SqlValue.normalizeFloat f |> System.BitConverter.DoubleToInt64Bits))
        | SqlValue.Decimal d ->
            out.[pos] <- 4uy

            normalizeDecimal d
            |> System.Decimal.GetBits
            |> Array.fold (fun p b -> writeInt64LETo out p (int64 b)) (pos + 1)
            |> ret
        | SqlValue.Text s ->
            let bytes = System.Text.Encoding.UTF8.GetBytes s
            out.[pos] <- 5uy
            let p = Writer.writeVarintTo out (pos + 1) (uint64 bytes.Length)
            Array.blit bytes 0 out p bytes.Length
            ret (p + bytes.Length)
        | SqlValue.Blob bs ->
            out.[pos] <- 6uy
            let p = Writer.writeVarintTo out (pos + 1) (uint64 bs.Length)
            Array.blit bs 0 out p bs.Length
            ret (p + bs.Length)
        | SqlValue.Date d ->
            out.[pos] <- 7uy
            ret (writeInt64LETo out (pos + 1) (d.ToDateTime(System.TimeOnly.MinValue).Ticks))
        | SqlValue.Timestamp t ->
            out.[pos] <- 8uy
            ret (writeInt64LETo out (pos + 1) t.UtcTicks)
        | SqlValue.Uuid g ->
            let bytes = g.ToByteArray()
            out.[pos] <- 9uy
            Array.blit bytes 0 out (pos + 1) 16
            ret (pos + 17)
        | SqlValue.Unloaded -> err (DbError.internalError "SqlValue.Unloaded は符号化できません")

    /// 値の並びを出力配列へ直接書く。次の位置を返す。
    /// `SqlValue.Unloaded` が混ざっている場合は `InternalError`。
    let writeValuesTo (out: byte array) (pos: int) (values: SqlValue array) =
        let pos = Writer.writeVarintTo out pos (uint64 values.Length)
        foldOk (writeValueTo out) pos values

    let private readInt (c: Cursor) =
        let d = Cursor.data c
        let start = Cursor.pos c

        if start + 8 < Cursor.endPos c && d.[start] = 2uy then
            let p = start + 1

            let v =
                int64 d.[p]
                ||| (int64 d.[p + 1] <<< 8)
                ||| (int64 d.[p + 2] <<< 16)
                ||| (int64 d.[p + 3] <<< 24)
                ||| (int64 d.[p + 4] <<< 32)
                ||| (int64 d.[p + 5] <<< 40)
                ||| (int64 d.[p + 6] <<< 48)
                ||| (int64 d.[p + 7] <<< 56)

            ValueSome struct (SqlValue.Int v, Cursor.advance 9 c)
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
                            | Ok(v, c') ->
                                values.[i] <- v
                                fill (i + 1) c'
                            | Error e -> err e

                return! fill 0 c
        }

    /// 長さ付きの値を読み飛ばす（マスク復号用）。
    let private skipLengthPrefixed (c: Cursor) =
        Reader.readVarint c
        >>= fun (n, c) ->
            if n > uint64 System.Int32.MaxValue || n > uint64 (Cursor.remaining c) then
                err (DbError.corruption "ValueCodec: 値の長さが不正です")
            else
                ret (Cursor.advance (int n) c)

    /// 1値を読み飛ばす（マスク復号用）。
    let private skipValue (c: Cursor) =
        Reader.readByte c
        >>= fun (tag, c) ->
            match tag with
            | 0uy -> ret c
            | 1uy -> ret (Cursor.advance 1 c)
            | 2uy
            | 3uy
            | 7uy
            | 8uy -> ret (Cursor.advance 8 c)
            | 4uy -> ret (Cursor.advance 32 c)
            | 5uy
            | 6uy -> skipLengthPrefixed c
            | 9uy -> ret (Cursor.advance 16 c)
            | _ -> err (sprintf "ValueCodec: 不明な型タグ %d" (int tag) |> DbError.corruption)

    /// 列マスクに従って値の並びを復号する。
    /// `mask.[i] = false` の列は `SqlValue.Unloaded` を置き、読み飛ばす。
    let readValuesMasked (mask: bool array) (c: Cursor) =
        result {
            let! n, c = Reader.readVarint c

            if n > uint64 System.Int32.MaxValue then
                return! sprintf "ValueCodec: 要素数が大きすぎます (%d)" n |> DbError.corruption |> err
            else
                let count = int n
                let values = Array.create count SqlValue.Unloaded

                let rec fill i c =
                    if i >= count then
                        ret (values, c)
                    else if i < mask.Length && not mask.[i] then
                        match skipValue c with
                        | Ok c' -> fill (i + 1) c'
                        | Error e -> err e
                    else
                        match readInt c with
                        | ValueSome struct (v, c') ->
                            values.[i] <- v
                            fill (i + 1) c'
                        | ValueNone ->
                            match readValue c with
                            | Ok(v, c') ->
                                values.[i] <- v
                                fill (i + 1) c'
                            | Error e -> err e

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
        | SqlValue.Unloaded -> Some 1
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
        | SqlValue.Unloaded -> 0

    /// 1行（`SqlValue array`）のヒープ上の概算サイズ。
    /// 配列ヘッダ（24）+ スロット（8 × 列数）+ 各値（`managedBytes`）。
    let managedRowBytes (row: SqlValue array) =
        24 + 8 * row.Length + (row |> Array.sumBy managedBytes)
