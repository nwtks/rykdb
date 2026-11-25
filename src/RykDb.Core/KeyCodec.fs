// ===========================================================================
// KeyCodec — ハッシュ・キー符号化・行値符号化。
// ===========================================================================

namespace RykDb.Core

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

    /// CRC32C の 1ステップ（生成多項式の反射表現）。
    [<TailCall>]
    let rec private crcStep (n: int) (c: uint32) =
        if n = 0 then
            c
        elif c &&& 1u <> 0u then
            crcStep (n - 1) ((c >>> 1) ^^^ 0x82F63B78u)
        else
            crcStep (n - 1) (c >>> 1)

    /// CRC32C の基本テーブル。
    let private crcTable0 = Array.init 256 (fun i -> crcStep 8 (uint32 i))

    /// slicing-by-8 用の次のテーブルを計算する。
    let private crcTableNext (prev: uint32 array) =
        Array.init 256 (fun i -> (prev.[i] >>> 8) ^^^ crcTable0.[int (prev.[i] &&& 0xFFu)])

    /// slicing-by-8 用のテーブルを組み立てる。
    [<TailCall>]
    let rec private crcTablesBuild (n: int) (acc: uint32 array list) =
        match acc with
        | prev :: _ when n > 0 -> crcTablesBuild (n - 1) (crcTableNext prev :: acc)
        | _ -> acc |> List.rev |> Array.ofList

    /// slicing-by-8 用の 8テーブル。`Tables.[k].[i]` は k バイト後ろの寄与。
    let private crcTables = crcTablesBuild 7 [ crcTable0 ]

    /// 初期値と最終反転（CRC32C の標準パラメータ）。
    [<Literal>]
    let private CrcInitial = 0xFFFFFFFFu

    /// 最終 XOR 値（CRC32C の標準パラメータ）。
    [<Literal>]
    let private CrcFinalXor = 0xFFFFFFFFu

    /// CRC を範囲の終端まで進める本体。
    [<TailCall>]
    let rec private crcUpdateLoop (i: int) (finish: int) (data: byte array) (crc: uint32) =
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
            |> crcUpdateLoop (i + 8) finish data
        elif i < finish then
            crcUpdateLoop (i + 1) finish data (crcTables.[0].[int (crc ^^^ uint32 data.[i] &&& 0xFFu)] ^^^ (crc >>> 8))
        else
            crc

    /// 範囲 [offset, offset + length) を 1つの連続入力として CRC を進める。
    /// チャンクをまたぐ場合は前のチャンクの戻り値をそのまま渡す。
    let private crcUpdate (offset: int) (length: int) = crcUpdateLoop offset (offset + length)

    /// バイト列の 32bit チェックサム（CRC32C / Castagnoli）。
    let checksum (data: byte array) =
        CrcFinalXor ^^^ crcUpdate 0 data.Length data CrcInitial

    /// 範囲指定の 32bit チェックサム（CRC32C）。
    /// SSTable のブロック末尾チェックサムを、コピーを作らずに計算する。
    let checksumRange (data: byte array) (offset: int) (length: int) =
        CrcFinalXor ^^^ crcUpdate offset length data CrcInitial

    /// チャンク列を順に畳む本体。
    [<TailCall>]
    let rec private checksumChunksLoop (i: int) (chunks: byte array array) (crc: uint32) =
        if i >= chunks.Length then
            crc
        else
            crcUpdate 0 chunks.[i].Length chunks.[i] crc
            |> checksumChunksLoop (i + 1) chunks

    /// 複数のチャンクを連結したものの 32bit チェックサム（CRC32C）。
    /// SSTable のメタデータ（meta / Bloom / index）をコピーせずに検証する。
    let checksumChunks (chunks: byte array array) =
        CrcFinalXor ^^^ checksumChunksLoop 0 chunks CrcInitial

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
    let ofOptionalValues =
        function
        | Some vs -> ofValues vs |> Result.map Some
        | None -> ret None

    /// 索引キー（索引列 + 主キー列）。
    /// 主キーは行を一意に特定するために末尾へ追加する。
    let ofIndexAndPrimary (indexCols: SqlValue seq) (primaryCols: SqlValue seq) =
        let appendCols = foldOk (fun w v -> OrderPreserving.writeValue v w)

        appendCols Writer.Empty indexCols
        >>= fun withIndex -> appendCols withIndex primaryCols |> Result.map Writer.toArray

    /// プレフィックス一致を先頭から辿る。
    [<TailCall>]
    let rec private matchPrefix (i: int) (key: byte array) (prefix: byte array) =
        if i >= prefix.Length then
            true
        elif key.[i] = prefix.[i] then
            matchPrefix (i + 1) key prefix
        else
            false

    /// バイト列 key が prefix で始まるか。
    let startsWithPrefix (key: byte array) (prefix: byte array) =
        if key.Length < prefix.Length then
            false
        else
            matchPrefix 0 key prefix

/// 行ペイロード用のコンパクト符号化。
/// キーと異なり、行値には順序保持が不要なので varint で詰めて小さくする。
module internal ValueCodec =
    /// 小数の負のゼロ (-0.0M) を +0.0M に正規化する。
    /// compareValuesV が両者を等しいと扱うため、
    /// DISTINCT などの符号化でも同じビット列にして重複と見なす。
    let private normalizeDecimal (d: decimal) = if d = 0M then 0M else d

    // 行ペイロードの型タグ。
    // Unloaded はスピル専用のプレースホルダ。

    [<Literal>]
    let private TagNull = 0uy

    [<Literal>]
    let private TagBool = 1uy

    [<Literal>]
    let private TagInt = 2uy

    [<Literal>]
    let private TagReal = 3uy

    [<Literal>]
    let private TagDecimal = 4uy

    [<Literal>]
    let private TagText = 5uy

    [<Literal>]
    let private TagBlob = 6uy

    [<Literal>]
    let private TagDate = 7uy

    [<Literal>]
    let private TagTimestamp = 8uy

    [<Literal>]
    let private TagUuid = 9uy

    [<Literal>]
    let private TagUnloaded = 10uy

    /// 型タグを書き込む。
    let private writeTagged (tag: byte) (w: Writer) = w |> Writer.writeByte (byte tag)

    /// 1値を行ペイロードへ書く（`Unloaded` は拒否）。
    let private writeValue (w: Writer) =
        function
        | SqlValue.Null -> writeTagged TagNull w |> ret
        | SqlValue.Bool b -> writeTagged TagBool w |> Writer.bool b |> ret
        | SqlValue.Int i -> writeTagged TagInt w |> Writer.int64LE i |> ret
        | SqlValue.Real f ->
            writeTagged TagReal w
            |> Writer.int64LE (SqlValue.normalizeFloat f |> System.BitConverter.DoubleToInt64Bits)
            |> ret
        | SqlValue.Decimal d ->
            normalizeDecimal d
            |> System.Decimal.GetBits
            |> Array.fold (fun w b -> w |> Writer.int64LE (int64 b)) (writeTagged TagDecimal w)
            |> ret
        | SqlValue.Text s -> writeTagged TagText w |> Writer.str s |> ret
        | SqlValue.Blob bs -> writeTagged TagBlob w |> Writer.lengthPrefixed bs |> ret
        | SqlValue.Date d ->
            writeTagged TagDate w
            |> Writer.int64LE (d.ToDateTime(System.TimeOnly.MinValue).Ticks)
            |> ret
        | SqlValue.Timestamp t -> writeTagged TagTimestamp w |> Writer.int64LE t.UtcTicks |> ret
        | SqlValue.Uuid g -> writeTagged TagUuid w |> Writer.writeBytes (g.ToByteArray()) |> ret
        | SqlValue.Unloaded -> err (DbError.internalError "SqlValue.Unloaded は符号化できません")

    /// 型タグを読んで 1値を復号する（高速経路に乗らない型）。
    let private readValueSlow (c: Cursor) =
        result {
            let! tag, c = Reader.readByte c

            match tag with
            | TagNull -> return SqlValue.Null, c
            | TagBool ->
                let! v, c = Reader.readBool c
                return SqlValue.ofBool v, c
            | TagInt ->
                let! v, c = Reader.readInt64LE c
                return SqlValue.Int v, c
            | TagReal ->
                let! bits, c = Reader.readInt64LE c
                return System.BitConverter.Int64BitsToDouble bits |> SqlValue.vReal, c
            | TagDecimal ->
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
            | TagText ->
                let! bs, c = Reader.readLengthPrefixed c
                return System.Text.Encoding.UTF8.GetString bs |> SqlValue.Text, c
            | TagBlob ->
                let! bs, c = Reader.readLengthPrefixed c
                return SqlValue.Blob bs, c
            | TagDate ->
                let! ticks, c = Reader.readInt64LE c
                let days = ticks / System.TimeSpan.TicksPerDay

                if
                    days < int64 System.DateOnly.MinValue.DayNumber
                    || days > int64 System.DateOnly.MaxValue.DayNumber
                then
                    return! err (DbError.corruption "ValueCodec: DATE の範囲が不正です")

                return SqlValue.Date(System.DateOnly.FromDayNumber(int days)), c
            | TagTimestamp ->
                let! ticks, c = Reader.readInt64LE c

                if ticks < 0L || ticks > System.DateTimeOffset.MaxValue.Ticks then
                    return! err (DbError.corruption "ValueCodec: TIMESTAMP の範囲が不正です")

                return System.DateTimeOffset(ticks, System.TimeSpan.Zero) |> SqlValue.Timestamp, c
            | TagUuid ->
                let! bs, c = Reader.readBytes 16 c
                return System.Guid bs |> SqlValue.Uuid, c
            | TagUnloaded -> return SqlValue.Unloaded, c
            | _ -> return! err (sprintf "ValueCodec: 不明な型タグ %d" (int tag) |> DbError.corruption)
        }

    /// 値の並びをまとめて書き出す。
    /// `SqlValue.Unloaded` が混ざっている場合は `InternalError`。
    let writeValues (values: SqlValue seq) (w: Writer) =
        let arr = Seq.toArray values
        let withCount = w |> Writer.varint (uint64 arr.Length)
        foldOk writeValue withCount arr

    /// 1値を書き出す（`Unloaded` はタグ 10 のプレースホルダにする）。
    let private writeSparse (w: Writer) =
        function
        | SqlValue.Unloaded -> ret (w |> Writer.writeByte TagUnloaded)
        | v -> writeValue w v

    /// 一時ファイル（スピル）用: `SqlValue.Unloaded` を含む行も書き出す。
    /// <remarks>
    /// 列マスクで不要とされた列は `Unloaded` のまま行に残る。
    /// 読み戻したときに列位置がずれないよう、タグ 10 のプレースホルダとして往復させる。
    /// 永続化（WAL / SSTable）には使わない。
    /// </remarks>
    let writeValuesSparse (values: SqlValue seq) (w: Writer) =
        let arr = Seq.toArray values
        let withCount = w |> Writer.varint (uint64 arr.Length)
        foldOk writeSparse withCount arr

    /// int64 をリトルエンディアンで書く。
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
            out.[pos] <- TagNull
            ret (pos + 1)
        | SqlValue.Bool b ->
            out.[pos] <- TagBool
            out.[pos + 1] <- if b then 1uy else 0uy
            ret (pos + 2)
        | SqlValue.Int i ->
            out.[pos] <- TagInt
            ret (writeInt64LETo out (pos + 1) i)
        | SqlValue.Real f ->
            out.[pos] <- TagReal

            SqlValue.normalizeFloat f
            |> System.BitConverter.DoubleToInt64Bits
            |> writeInt64LETo out (pos + 1)
            |> ret
        | SqlValue.Decimal d ->
            out.[pos] <- TagDecimal

            normalizeDecimal d
            |> System.Decimal.GetBits
            |> Array.fold (fun p b -> writeInt64LETo out p (int64 b)) (pos + 1)
            |> ret
        | SqlValue.Text s ->
            let bytes = System.Text.Encoding.UTF8.GetBytes s
            out.[pos] <- TagText
            let p = Writer.writeVarintTo out (pos + 1) (uint64 bytes.Length)
            Array.blit bytes 0 out p bytes.Length
            ret (p + bytes.Length)
        | SqlValue.Blob bs ->
            out.[pos] <- TagBlob
            let p = Writer.writeVarintTo out (pos + 1) (uint64 bs.Length)
            Array.blit bs 0 out p bs.Length
            ret (p + bs.Length)
        | SqlValue.Date d ->
            out.[pos] <- TagDate

            d.ToDateTime(System.TimeOnly.MinValue).Ticks
            |> writeInt64LETo out (pos + 1)
            |> ret
        | SqlValue.Timestamp t ->
            out.[pos] <- TagTimestamp
            ret (writeInt64LETo out (pos + 1) t.UtcTicks)
        | SqlValue.Uuid g ->
            out.[pos] <- TagUuid
            let bytes = g.ToByteArray()
            Array.blit bytes 0 out (pos + 1) 16
            ret (pos + 17)
        | SqlValue.Unloaded -> err (DbError.internalError "SqlValue.Unloaded は符号化できません")

    /// 値の並びを出力配列へ直接書く。次の位置を返す。
    /// `SqlValue.Unloaded` が混ざっている場合は `InternalError`。
    let writeValuesTo (out: byte array) (pos: int) (values: SqlValue array) =
        let pos = Writer.writeVarintTo out pos (uint64 values.Length)
        foldOk (writeValueTo out) pos values

    /// タグ 2（INTEGER）の固定長を高速に読む。
    let private readInt (c: Cursor) =
        let d = Cursor.data c
        let start = Cursor.pos c

        if Cursor.remaining c >= 9 && d.[start] = TagInt then
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

    /// INT の高速経路を試し、だめなら `readValueSlow` で読む。
    let private readValue (c: Cursor) =
        match readInt c with
        | ValueSome struct (v, c) -> ret (v, c)
        | ValueNone -> readValueSlow c

    /// 配列へ値を読み込む本体。
    [<TailCall>]
    let rec private fillReadValues (values: SqlValue array) (count: int) (i: int) (c: Cursor) =
        if i >= count then
            ret (values, c)
        else
            match readInt c with
            | ValueSome struct (v, c') ->
                values.[i] <- v
                fillReadValues values count (i + 1) c'
            | ValueNone ->
                match readValue c with
                | Ok(v, c') ->
                    values.[i] <- v
                    fillReadValues values count (i + 1) c'
                | Error e -> err e

    /// 値の並びを復号する（`writeValues` の逆）。
    let readValues (c: Cursor) =
        result {
            let! n, c = Reader.readVarint c

            if n > uint64 System.Int32.MaxValue || n > uint64 (Cursor.remaining c) then
                return! sprintf "ValueCodec: 要素数が大きすぎます (%d)" n |> DbError.corruption |> err
            else
                let count = int n
                let values = Array.zeroCreate<SqlValue> count
                return! fillReadValues values count 0 c
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
            | TagNull -> ret c
            | TagBool -> ret (Cursor.advance 1 c)
            | TagInt
            | TagReal
            | TagDate
            | TagTimestamp -> ret (Cursor.advance 8 c)
            | TagDecimal -> ret (Cursor.advance 32 c)
            | TagText
            | TagBlob -> skipLengthPrefixed c
            | TagUuid -> ret (Cursor.advance 16 c)
            | _ -> err (sprintf "ValueCodec: 不明な型タグ %d" (int tag) |> DbError.corruption)

    /// マスク付きで配列へ値を読み込む本体（false の列は読み飛ばす）。
    [<TailCall>]
    let rec private fillReadValuesMasked (mask: bool array) (values: SqlValue array) (count: int) (i: int) (c: Cursor) =
        if i >= count then
            ret (values, c)
        else if i < mask.Length && not mask.[i] then
            match skipValue c with
            | Ok c' -> fillReadValuesMasked mask values count (i + 1) c'
            | Error e -> err e
        else
            match readInt c with
            | ValueSome struct (v, c') ->
                values.[i] <- v
                fillReadValuesMasked mask values count (i + 1) c'
            | ValueNone ->
                match readValue c with
                | Ok(v, c') ->
                    values.[i] <- v
                    fillReadValuesMasked mask values count (i + 1) c'
                | Error e -> err e

    /// 列マスクに従って値の並びを復号する。
    /// `mask.[i] = false` の列は `SqlValue.Unloaded` を置き、読み飛ばす。
    let readValuesMasked (mask: bool array) (c: Cursor) =
        result {
            let! n, c = Reader.readVarint c

            if n > uint64 System.Int32.MaxValue || n > uint64 (Cursor.remaining c) then
                return! sprintf "ValueCodec: 要素数が大きすぎます (%d)" n |> DbError.corruption |> err
            else
                let count = int n
                let values = Array.create count SqlValue.Unloaded
                return! fillReadValuesMasked mask values count 0 c
        }

    /// 固定長の値の符号化サイズ（型タグ込み）。
    /// TEXT / BLOB は長さ依存なので `None`。
    let private fixedEncodedBytes =
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

    /// 1値の符号化後のおよそのバイト数（ファイルサイズ見積り）。
    /// メモリ上限の判定には使わない（参照型・string・配列ヘッダを含まない）。
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
