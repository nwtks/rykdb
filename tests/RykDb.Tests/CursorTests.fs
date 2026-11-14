// ===========================================================================
// Cursor.fs（読み取り位置を値で持つカーソル）
// ===========================================================================

module RykDb.CursorTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 不変条件 `0 <= Pos <= End <= Data.Length` の検査。
/// <remarks>
/// 本番経路では呼ばない。テストで各操作の後に呼ぶ。
/// `create` / `createSlice` が構築時にこれを満たし、
/// 各読み取りが `Pos < End` を確認してから進むので帰納的に保たれる。
/// </remarks>
let private validate (c: Cursor) =
    let p = Cursor.pos c
    let e = Cursor.endPos c
    let d = Cursor.data c

    if p < 0 then
        sprintf "Cursor: Pos が負です（Pos=%d）" p |> DbError.internalError |> err
    elif e < p then
        sprintf "Cursor: End が Pos より小さい（Pos=%d End=%d）" p e
        |> DbError.internalError
        |> err
    elif e > d.Length then
        sprintf "Cursor: End が配列長を超える（End=%d / %d バイト）" e d.Length
        |> DbError.internalError
        |> err
    else
        ret ()

[<Fact>]
let ``Cursor: createSlice は範囲を切り出す`` () =
    let data = [| 0uy; 1uy; 2uy; 3uy; 4uy |]
    let c = ok (Cursor.createSlice data 1 3)
    ok (validate c)
    Assert.Equal(3, Cursor.remaining c)
    let b, c2 = ok (Reader.readByte c)
    Assert.Equal(1uy, b)
    Assert.Equal(2, Cursor.remaining c2)
    ok (validate c2)

[<Fact>]
let ``Cursor: atEnd は位置が終端に達したら true`` () =
    let c = Cursor.create [| 1uy |]
    let _, c2 = ok (Reader.readByte c)
    Assert.True(Cursor.atEnd c2)
    Assert.False(Cursor.atEnd c)

[<Fact>]
let ``Cursor: createSlice は範囲外を CorruptionError`` () =
    let data = [| 0uy; 1uy; 2uy |]
    expectErrorKind ErrorKind.CorruptionError "offset が負" (Cursor.createSlice data -1 2)
    expectErrorKind ErrorKind.CorruptionError "length が負" (Cursor.createSlice data 0 -1)
    expectErrorKind ErrorKind.CorruptionError "offset+length が配列長を超える" (Cursor.createSlice data 1 3)
    expectErrorKind ErrorKind.CorruptionError "offset が配列長を超える" (Cursor.createSlice data 4 0)
    // 判定は data.Length - length で行うので、巨大な length でもオーバーフローで通らない。
    expectErrorKind ErrorKind.CorruptionError "巨大な length がオーバーフロー回避できない" (Cursor.createSlice data 0 Int32.MaxValue)

[<Fact>]
let ``Cursor: create / createSlice / 読み取りは不変条件 0 <= Pos <= End <= Data.Length を保つ`` () =
    let data = Array.init 64 (fun i -> byte i)

    let assertValid (label: string) (c: Cursor) =
        match validate c with
        | Error e -> Assert.Fail(sprintf "%s: %s" label (DbError.toDisplayString e))
        | Ok() -> ()

    // 読み進めるたびに不変条件が壊れないこと（各読み取りは Pos < End を見てから進む）。
    let rec drain (label: string) (c: Cursor) =
        assertValid label c

        if not (Cursor.atEnd c) then
            match Reader.readByte c with
            | Error e -> Assert.Fail(sprintf "%s: %s" label (DbError.toDisplayString e))
            | Ok(_, nxt) -> drain label nxt

    assertValid "create" (Cursor.create data)

    for offset in [ 0; 1; 7; 32; 63; 64 ] do
        for length in [ 0; 1; 7; 32; 64 ] do
            if offset + length <= data.Length then
                let label = sprintf "slice offset=%d length=%d" offset length
                let c = ok (Cursor.createSlice data offset length)
                assertValid label c
                drain label c
