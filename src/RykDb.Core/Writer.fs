namespace RykDb.Core

/// 書き出し: Writer
/// 不変のチャンク列で書き出し手順を保持し、最後に一度だけバイト列へ平坦化する。
/// <remarks>
/// `Chunks` は逆順（先頭が最後に書いたチャンク）。
/// `add` が先頭へ積み、`toArray` が末尾から埋める。
/// 外でレコードを組むと順序が反転して静かに壊れるため、構築はこのモジュールの関数に限る。
/// </remarks>
type internal WriterChunk =
    | Bytes of byte array
    | Slice of byte array * int * int
    | Byte of byte
    | UInt64LE of uint64
    | UInt64BE of uint64
    | UInt32LE of uint32
    | Varint of uint64

type internal Writer = { Chunks: WriterChunk list }

module internal Writer =
    /// 空の Writer。
    let Empty = { Chunks = [] }

    /// チャンクを先頭へ積む。
    let private add (chunk: WriterChunk) (w: Writer) = { Chunks = chunk :: w.Chunks }

    /// 1バイト追加する。
    let writeByte (b: byte) (w: Writer) = add (Byte b) w

    /// バイト列を追加する。
    /// 並びは保たれる。
    let writeBytes (bs: byte array) (w: Writer) =
        if bs.Length = 0 then w else add (Bytes bs) w

    /// BOOL を 1バイト（1 / 0）で追加する。
    let bool (v: bool) (w: Writer) = add (Byte(if v then 1uy else 0uy)) w

    /// 符号なし LEB128（順序保持ではない）。
    let varint (v: uint64) (w: Writer) = add (Varint v) w

    /// リトルエンディアン 8バイト。
    let int64LE (v: int64) (w: Writer) = add (UInt64LE(uint64 v)) w

    /// リトルエンディアン 4バイト（符号なし）。
    /// WAL のフレームヘッダや SSTable の Footer など固定長フィールド用。
    let uint32LE (v: uint32) (w: Writer) = add (UInt32LE v) w

    /// ビッグエンディアン 8バイト（符号なし）。
    /// 順序保持符号化で使う。
    let uint64BE (v: uint64) (w: Writer) = add (UInt64BE v) w

    /// 長さ付きバイト列（varint 長 + 実体）。
    let lengthPrefixed (bs: byte array) (w: Writer) =
        varint (uint64 bs.Length) w |> writeBytes bs

    /// UTF-8 文字列を長さ付きで追加する。
    let str (s: string) (w: Writer) =
        lengthPrefixed (System.Text.Encoding.UTF8.GetBytes s) w

    /// varint を `out.[pos]` から書き込み、次の空き位置を返す。
    [<TailCall>]
    let rec writeVarintTo (out: byte array) (pos: int) (v: uint64) =
        if v >= 0x80UL then
            out.[pos] <- byte (v &&& 0x7FUL ||| 0x80UL)
            writeVarintTo out (pos + 1) (v >>> 7)
        else
            out.[pos] <- byte v
            pos + 1

    /// varint が占めるバイト数を上位桁から数える。
    [<TailCall>]
    let rec private varintLengthLoop (bytes: int) (value: uint64) =
        if value < 0x80UL then
            bytes + 1
        else
            varintLengthLoop (bytes + 1) (value >>> 7)

    /// varint のバイト数。
    let varintLength (v: uint64) = varintLengthLoop 0 v

    /// チャンクのバイト長。
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

    /// チャンク 1つを `out.[pos]` へ書き込む。
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

    /// 逆順のチャンク列を出力配列の末尾から埋めていく。
    [<TailCall>]
    let rec private fillChunks (out: byte array) (pos: int) (chunks: WriterChunk list) =
        match chunks with
        | [] -> out
        | chunk :: rest ->
            let next = pos - chunkLength chunk
            writeChunk out next chunk
            fillChunks out next rest

    /// 全チャンクを 1本のバイト列に平坦化する。
    /// チャンク列は逆順（先頭が最後の書き込み）なので、
    /// 出力配列の末尾から埋めて `List.rev` を不要にする。
    let toArray (w: Writer) =
        let total = length w
        let out = Array.zeroCreate<byte> total

        fillChunks out total w.Chunks

    /// 書き出し手順を左から順に適用して 1本のバイト列にする。
    /// 要素は Writer.varint x のような部分適用で書ける。
    let build = List.fold (fun w f -> f w) Empty >> toArray
