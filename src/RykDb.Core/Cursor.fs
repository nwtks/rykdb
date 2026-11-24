namespace RykDb.Core

/// 読み取り位置を持つ不変のカーソル。
/// 前進は新しい Cursor を返し、読み取りは例外ではなく `Result` を返す。
/// <remarks>
/// レコードは `Cursor.fsi` で隠す。
/// 構築は `create` / `createSlice`、前進は `advance` に限る。
/// </remarks>
type internal Cursor =
    { Data: byte array; Pos: int; End: int }

module internal Cursor =
    /// 配列全体を読むカーソル。
    let create (data: byte array) =
        {
            Data = data
            Pos = 0
            End = data.Length
        }

    /// 配列の一部を読むカーソル。
    /// 範囲は構築時に検証する（範囲外は `CorruptionError`）。
    /// <remarks>
    /// 検証せずに作ると `Pos` / `End` が配列外になり、`Reader.readByte` が
    /// `IndexOutOfRangeException` を投げて壊れた SSTable が未処理例外になる。
    /// 判定は `data.Length - length` で行うため、`length` が巨大でもオーバーフローしない。
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
    /// 範囲の検証は呼び出し側（`Pos < End` の確認）が行う。
    /// Cursor のレコード更新はこの関数の中だけに置く。
    let advance (n: int) (c: Cursor) = { c with Pos = c.Pos + n }

    /// 残りのバイト数。
    let remaining (c: Cursor) = c.End - c.Pos

    /// 読み取り終端に達しているか。
    let atEnd (c: Cursor) = c.Pos >= c.End

    /// 読み取り対象の配列。
    let data (c: Cursor) = c.Data

    /// 現在の読み取り位置。
    let pos (c: Cursor) = c.Pos

    /// 読み取り終端（排他的）。
    let endPos (c: Cursor) = c.End

    /// 先頭から `length` バイトを切り出し、切り出したカーソルと残りのカーソルを返す。
    /// 範囲外は `CorruptionError`（`createSlice` と同じ判定）。
    let splitAt (length: int) (c: Cursor) =
        if length < 0 || length > remaining c then
            sprintf "Cursor: %d バイトを切り出せません（残り %d バイト）" length (remaining c)
            |> DbError.corruption
            |> err
        else
            createSlice (data c) (pos c) length
            |> Result.map (fun taken -> taken, advance length c)
