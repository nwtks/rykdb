namespace RykDb.Core

/// 読み取り: Cursor
/// 読み取り位置を値が持つ。
/// 更新は新しい Cursor を返すので mutable は要らない。
/// すべての読み取りは Result を返し、例外を送らない。
/// <remarks>
/// レコードは `Cursor.fsi` で隠す。
/// 構築は `create` / `createSlice`、前進は `advance` に限る。
/// </remarks>
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
    /// <remarks>
    /// **範囲を検証してから作る**。
    /// 検証せずに作ると `Pos` / `End` が `Data.Length` を超え、
    /// `Reader.readByte` が `IndexOutOfRangeException` を投げる
    /// （壊れた SSTable を `CorruptionError` ではなく未処理例外で拒否してしまう）。
    /// 範囲外は `CorruptionError`。
    /// 判定は `data.Length - length` で行うので、`length` が巨大でもオーバーフローしない。
    /// </remarks>
    let createSlice (data: byte array) (offset: int) (length: int) =
        if offset < 0 || length < 0 || offset > data.Length - length then
            sprintf "Cursor: スライス範囲が不正です（offset=%d length=%d / 配列 %d バイト）" offset length data.Length
            |> DbError.corruption
            |> err
        else
            ret
                {
                    Data = data
                    Pos = offset
                    End = offset + length
                }

    /// 読み取り位置を進める。
    /// <remarks>
    /// 範囲の検証は呼び出し側（`Pos < End` の確認）が行う。
    /// **Cursor のレコード更新はこの関数の中だけに置く**。
    /// </remarks>
    let advance (n: int) (c: Cursor) = { c with Pos = c.Pos + n }

    let remaining (c: Cursor) = c.End - c.Pos
    let atEnd (c: Cursor) = c.Pos >= c.End

    /// 読み取り対象の配列。
    let data (c: Cursor) = c.Data

    /// 現在の読み取り位置。
    let pos (c: Cursor) = c.Pos

    /// 読み取り終端（排他的）。
    let endPos (c: Cursor) = c.End
