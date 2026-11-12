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

/// 1行取り出す pull 演算子。
/// <remarks>
/// **one-shot**: 値は 1回だけ消費する。
/// `next` が返した継続（組の 2番目）だけを次の行に使い、
/// 元の `Op` をもう一度 `next` してはいけない
/// （`Step` は同じ行を再び返し、`Compute` は継続を巻き戻す）。
/// 構築は `empty` / `ofRow` / `emit` / `compute` / `ofArray` に限る
/// （`OpTests` が直接ケースを使っていないことを検査する）。
/// </remarks>
[<RequireQualifiedAccess>]
type internal Op =
    | Done
    /// 1行と「その次」。
    /// next は評価されるので長い連鎖を組むには向かない
    /// （遅延が必要なときは compute を使う）。
    | Step of SqlValue array * Op
    /// 「次に 1行を返す関数」。
    /// 引数は unit なので遅延評価される。
    /// ソース側が「まだ計算していない」長い系列を表すときは必ずこれを使う。
    | Compute of (unit -> Result<(SqlValue array * Op) option, DbError>)

module internal Op =
    /// 空の演算子。
    let Empty = Op.Done

    /// 1行だけの演算子。
    let ofRow (row: SqlValue array) = Op.Step(row, Op.Done)

    /// 1行と「その次」を返す。
    /// `nxt` には `next` が返した継続をそのまま渡す（one-shot）。
    let emit (row: SqlValue array) (nxt: Op) = Op.Step(row, nxt)

    /// 「次に 1行を返す関数」から演算子を作る。
    /// 関数は 1回だけ呼ばれる想定（one-shot）。
    /// 返す組の 2番目には、その行の続きを入れる。
    let compute (f: unit -> Result<(SqlValue array * Op) option, DbError>) = Op.Compute f

    /// 配列を順に返す演算子。
    /// 次の行は呼び出すたびに計算する。
    let ofArray (rows: SqlValue array array) =
        let rec rowsFrom i =
            if i >= rows.Length then
                Empty
            else
                compute (fun () -> ret (Some(rows.[i], rowsFrom (i + 1))))

        rowsFrom 0

    /// 1行取り出す。
    let next =
        function
        | Op.Done -> ret None
        | Op.Step(row, nxt) -> ret (Some(row, nxt))
        | Op.Compute f -> f ()

    /// ソースの次の 1行を引き出して継続に渡す。
    /// 継続は「行と残りの Op」を受け取り、返した Result をそのまま返す。
    /// 返した「その行の次」の Op が演算子の残りになる。
    let pull (cont: SqlValue array -> Op -> Result<(SqlValue array * Op) option, DbError>) (src: Op) =
        compute (fun () ->
            next src
            >>= function
                | None -> ret None
                | Some(row, nxt) -> cont row nxt)

    /// 行を 1つ変換する。
    /// 失敗したら打切って Error。
    /// 変換後の行の「次」は map を通した残りにする
    /// （nxt をそのまま渡すと変換が 1行だけに効き、次から元に戻る）。
    let rec map (f: SqlValue array -> Result<SqlValue array, DbError>) =
        pull (fun row nxt -> f row |> Result.map (fun r -> Some(r, map f nxt)))

    /// 述語で弾く。
    /// 偽の行は読み飛ばして次へ進む。
    /// p row >>= function で分岐する
    /// （if/then/else だと Result の短絡で Some(row, nxt) だけが残ることがある）。
    let filter (p: SqlValue array -> Result<bool, DbError>) =
        let rec seekMatch cur =
            pull
                (fun row nxt ->
                    match p row with
                    | Ok true -> ret (Some(row, seekMatch nxt))
                    | Ok false -> next (seekMatch nxt)
                    | Error e -> err e)
                cur

        seekMatch

    /// 先頭 n 行だけ。
    /// n <= 0 は 0 行（`LIMIT 0` は 0 行）。
    /// 「制限なし」は呼び出し側が take を呼ばないこと表す。
    let rec take (n: int) (src: Op) =
        if n <= 0 then
            Empty
        else
            pull (fun row nxt -> ret (Some(row, take (n - 1) nxt))) src

    /// 先頭 n行を読み飛ばす。
    let skip =
        let rec drop left cur =
            if left <= 0 then
                cur
            else
                pull (fun _ nxt -> next (drop (left - 1) nxt)) cur

        drop

    /// 全行を上限つきで配列に materialize する。
    /// 上限を超えたら `CapacityError` を返し、部分結果は返さない。
    let toArrayLimited (maxRows: int64) (maxBytes: int64) (op: Op) =
        let rows = ResizeArray<SqlValue array>()

        let tooLarge () =
            DbError.capacity (
                sprintf "SELECT 結果が上限を超えました（上限: %d 行 / %d バイト）。LIMIT を付けるか、SessionOptions で上限を変更してください" maxRows maxBytes
            )
            |> err

        let rec drain count bytes cur =
            match next cur with
            | Error e -> err e
            | Ok None -> ret (rows.ToArray())
            | Ok(Some(row, nxt)) ->
                if count >= maxRows then
                    tooLarge ()
                else
                    let rowBytes = int64 (ValueCodec.managedRowBytes row)

                    if rowBytes > maxBytes - bytes then
                        tooLarge ()
                    else
                        rows.Add row
                        drain (count + 1L) (bytes + rowBytes) nxt

        drain 0L 0L op

    /// 全行を配列に materialize する（無制限）。
    let toArray = toArrayLimited System.Int64.MaxValue System.Int64.MaxValue
