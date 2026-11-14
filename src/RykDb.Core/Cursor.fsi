namespace RykDb.Core

/// 読み取り位置を値で持つカーソル。
/// <remarks>
/// レコードはこのシグネチャで隠す。
/// 構築は `create` / `createSlice`、前進は `advance` に限る。
/// フィールドの読み取りは `data` / `pos` / `endPos` を使う。
/// </remarks>
type internal Cursor

module internal Cursor =
    /// 配列全体を読むカーソル（`Pos = 0` / `End = Data.Length`）。
    val create: byte array -> Cursor

    /// 配列の一部だけを読むカーソル（範囲外は `CorruptionError`）。
    val createSlice: byte array -> int -> int -> Result<Cursor, DbError>

    /// 読み取り位置を進める。
    val advance: int -> Cursor -> Cursor

    /// 残りバイト数。
    val remaining: Cursor -> int

    /// 終端に達したか。
    val atEnd: Cursor -> bool

    /// 読み取り対象の配列。
    val data: Cursor -> byte array

    /// 現在の読み取り位置。
    val pos: Cursor -> int

    /// 読み取り終端（排他的）。
    val endPos: Cursor -> int
