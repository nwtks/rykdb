// ===========================================================================
// Op — 1行取り出す pull 演算子（Volcano 方式）。
//
// Done             = 終わった
// Step (row, next) = row を返した。続きは next
// Compute f        = f を呼んで 1行取り出す
//
// Compute が「ソースだけを包む 1段」であることが要点。
// 変換・フィルタ・結合はすべて「上流の Op を受け取って、`unit -> ...` を返す関数」として書けるので、
// ソース側の状態を壊さずに不変のまま合成できる。
// Step の next を呼ぶまで次の行は計算されないので、無限長でもメモリは 1行分だけ。
// ===========================================================================

namespace RykDb.Core

/// 1列の SqlValue 配列。
/// メモリ上は配列のまま、回さない。
type internal Row = SqlValue array

[<RequireQualifiedAccess>]
type internal Op =
    | Done
    /// 1行と「その次」。
    /// next は評価されるので長い連鎖を組むには向かない
    /// （遅延が必要なときは Compute を使う）。
    | Step of SqlValue array * Op
    /// 「次に 1行を返す関数」。
    /// 引数は unit なので遅延評価される。
    /// ソース側が「まだ計算していない」長い系列を表すときは必ずこれを使う。
    | Compute of (unit -> Result<(SqlValue array * Op) option, DbError>)

module internal Op =
    /// 空の演算子。
    let empty = Op.Done

    /// 1行だけの演算子。
    let ofRow (row: SqlValue array) = Op.Step(row, Op.Done)

    /// 配列を順に返す演算子。
    /// 次の行は呼び出すたびに計算する。
    let ofArray (rows: SqlValue array array) =
        let rec rowsFrom (i: int) =
            if i >= rows.Length then
                Op.Done
            else
                Op.Compute(fun () -> ret (Some(rows.[i], rowsFrom (i + 1))))

        rowsFrom 0

    /// seq を順に返す演算子（遅延）。
    /// 次の行は呼び出すたびに計算する。
    /// ソース seq は純粋（副作用なし）であることを前提とする。
    let ofSeq (rows: seq<SqlValue array>) =
        let e = rows.GetEnumerator()

        let rec rowsFrom () =
            if e.MoveNext() then
                Op.Compute(fun () -> ret (Some(e.Current, rowsFrom ())))
            else
                (e :> System.IDisposable).Dispose() |> ignore
                Op.Done

        rowsFrom ()

    /// 1行取り出す。
    let next (op: Op) =
        match op with
        | Op.Done -> ret None
        | Op.Step(row, nxt) -> ret (Some(row, nxt))
        | Op.Compute f -> f ()

    /// ソースの次の 1行を引き出して継続に渡す。
    /// 継続は「行と残りの Op」を受け取り、返した Result をそのまま返す。
    /// 返した「その行の次」の Op が演算子の残りになる。
    let pull (src: Op) (cont: SqlValue array -> Op -> Result<(SqlValue array * Op) option, DbError>) =
        Op.Compute(fun () ->
            next src
            >>= function
                | None -> ret None
                | Some(row, nxt) -> cont row nxt)

    /// 行を 1つ変換する。
    /// 失敗したら打切って Error。
    /// 変換後の行の「次」は map を通した残りにする
    /// （nxt をそのまま渡すと変換が 1行だけに効き、次から元に戻る）。
    let rec map (f: SqlValue array -> Result<SqlValue array, DbError>) (src: Op) =
        pull src (fun row nxt -> f row |> Result.map (fun r -> Some(r, map f nxt)))

    /// 述語で弾く。
    /// 偽の行は読み飛ばして次へ進む。
    /// p row >>= function で分岐する
    /// （if/then/else だと Result の短絡で Some(row, nxt) だけが残ることがある）。
    let filter (p: SqlValue array -> Result<bool, DbError>) (src: Op) =
        let rec seekMatch (cur: Op) =
            pull cur (fun row nxt ->
                let verdict = p row

                match verdict with
                | Ok true -> ret (Some(row, seekMatch nxt))
                | Ok false -> next (seekMatch nxt)
                | Error e -> err e)

        seekMatch src

    /// 先頭 n 行だけ。
    /// n <= 0 は 0 行（`LIMIT 0` は 0 行）。
    /// 「制限なし」は呼び出し側が take を呼ばないこと表す。
    let rec take (n: int) (src: Op) =
        if n <= 0 then
            Op.Done
        else
            pull src (fun row nxt -> ret (Some(row, take (n - 1) nxt)))

    /// 先頭 n行を読み飛ばす。
    let skip (n: int) (src: Op) =
        let rec drop (left: int) (cur: Op) =
            if left <= 0 then
                cur
            else
                pull cur (fun _ nxt -> next (drop (left - 1) nxt))

        drop n src

    /// 全行を配列に materialize する。
    let toArray (op: Op) =
        let rows = ResizeArray<SqlValue array>()

        let rec drain (cur: Op) =
            match next cur with
            | Error e -> Error e
            | Ok None -> Ok(rows.ToArray())
            | Ok(Some(row, nxt)) ->
                rows.Add row
                drain nxt

        drain op
