namespace RykDb.Core

/// 書き出し: Writer（不変のチャンク列）。
/// <remarks>
/// `Chunks` は逆順（先頭が最後に書いたチャンク）で保持する。
/// ケースとレコードはこのシグネチャで隠すので、
/// 構築は `Empty` と各 write 関数に限られる。
/// </remarks>
type internal Writer

module internal Writer =
    /// 空の Writer。
    val Empty: Writer

    /// 1バイト。
    val writeByte: byte -> Writer -> Writer

    /// バイト列（並びは保たれる）。
    val writeBytes: byte array -> Writer -> Writer

    /// 真偽値（1バイト）。
    val bool: bool -> (Writer -> Writer)

    /// 符号なし LEB128。
    val varint: uint64 -> (Writer -> Writer)

    /// リトルエンディアン 8バイト。
    val int64LE: int64 -> (Writer -> Writer)

    /// リトルエンディアン 4バイト。
    val uint32LE: uint32 -> (Writer -> Writer)

    /// ビッグエンディアン 8バイト。
    val uint64BE: uint64 -> (Writer -> Writer)

    /// 長さ付きバイト列（varint 長 + 実体）。
    val lengthPrefixed: byte array -> (Writer -> Writer)

    /// UTF-8 文字列を長さ付きで追加する。
    val str: string -> (Writer -> Writer)

    /// varint を出力配列へ書き、次の位置を返す。
    val writeVarintTo: byte array -> int -> uint64 -> int

    /// 符号なし LEB128 のバイト数。
    val varintLength: uint64 -> int

    /// 全チャンクを 1本のバイト列に平坦化する。
    val toArray: Writer -> byte array

    /// 書き出し手順を左から順に適用して 1本のバイト列にする。
    val build: ((Writer -> Writer) list -> byte array)
