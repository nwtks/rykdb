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
// `Compute` の関数は `next` を呼ぶまで評価されないので、無限長でもメモリは 1行分だけ
// （`Step` の継続は構築時に評価済み）。
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
/// （`.fsi` でケースを隠して守る）。
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
    | Compute of (unit -> OpStep)

and [<Struct; NoEquality; NoComparison; RequireQualifiedAccess>] internal OpStep =
    | OpDone
    | OpError of error: DbError
    | OpRow of row: SqlValue array * next: Op

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
    /// 返す値は `OpStep`（成功値は構造体なので `Ok` を確保しない）。
    let compute (f: unit -> OpStep) = Op.Compute f

    /// 配列の残りを順に返す（`ofArray` の走査）。
    let rec private rowsFrom (rows: SqlValue array array) (i: int) =
        if i >= rows.Length then
            Empty
        else
            compute (fun () -> OpStep.OpRow(rows.[i], rowsFrom rows (i + 1)))

    /// 配列を順に返す演算子。
    /// 次の行は呼び出すたびに計算する。
    let ofArray (rows: SqlValue array array) = rowsFrom rows 0

    /// ResizeArray の残りを順に返す（`ofArray` の走査）。
    let rec private rowsFromResize (rows: ResizeArray<SqlValue array>) (i: int) =
        if i >= rows.Count then
            Empty
        else
            compute (fun () -> OpStep.OpRow(rows.[i], rowsFromResize rows (i + 1)))

    /// ResizeArray をコピーせず順に返す演算子。
    /// ハッシュ表のバケットのように、構築後に変更されない可変コレクションに使う。
    let ofResizeArray (rows: ResizeArray<SqlValue array>) = rowsFromResize rows 0

    /// 1行取り出す。
    let next =
        function
        | Op.Done -> OpStep.OpDone
        | Op.Step(row, nxt) -> OpStep.OpRow(row, nxt)
        | Op.Compute f -> f ()

    /// ソースの次の 1行を引き出して継続に渡す。
    /// 継続は「行と残りの Op」を受け取り、返した `OpStep` をそのまま返す。
    /// 返した「その行の次」の Op が演算子の残りになる。
    let pull (cont: SqlValue array -> Op -> OpStep) (src: Op) =
        compute (fun () ->
            match next src with
            | OpStep.OpDone -> OpStep.OpDone
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpRow(row, nxt) -> cont row nxt)

    /// 行を 1つ変換する。
    /// 失敗したら打切って Error。
    /// 変換後の行の「次」は map を通した残りにする
    /// （nxt をそのまま渡すと変換が 1行だけに効き、次から元に戻る）。
    let rec map (f: SqlValue array -> DbResult<SqlValue array>) =
        pull (fun row nxt ->
            match f row with
            | DbErr e -> OpStep.OpError e
            | DbOk r -> OpStep.OpRow(r, map f nxt))

    /// 述語に合う行まで読み飛ばす（`filter` の走査）。
    let rec private filterSeekMatch (p: SqlValue array -> DbResult<bool>) (cur: Op) =
        pull
            (fun row nxt ->
                match p row with
                | DbOk true -> OpStep.OpRow(row, filterSeekMatch p nxt)
                | DbOk false -> next (filterSeekMatch p nxt)
                | DbErr e -> OpStep.OpError e)
            cur

    /// 述語で弾く。
    /// 偽の行は読み飛ばして次へ進む。
    let filter (p: SqlValue array -> DbResult<bool>) = filterSeekMatch p

    /// 先頭 n 行だけ。
    /// n <= 0 は 0 行（`LIMIT 0` は 0 行）。
    /// 「制限なし」は呼び出し側が take を呼ばないこと表す。
    let rec take (n: int) (src: Op) =
        if n <= 0 then
            Empty
        else
            pull (fun row nxt -> OpStep.OpRow(row, take (n - 1) nxt)) src

    /// 先頭 n行を読み飛ばす。
    let rec skip (left: int) (cur: Op) =
        if left <= 0 then
            cur
        else
            pull (fun _ nxt -> next (skip (left - 1) nxt)) cur

    /// 2つの演算子を順に連結する（左をすべて出してから右）。
    /// 右は左が尽きるまで評価しない（`compute` の遅延を保つ）。
    let rec append (first: Op) (second: Op) =
        compute (fun () ->
            match next first with
            | OpStep.OpDone -> next second
            | OpStep.OpError e -> OpStep.OpError e
            | OpStep.OpRow(row, nxt) -> OpStep.OpRow(row, append nxt second))

    /// 上限超過のエラーを作る。
    let private resultTooLarge (maxRows: int64) (maxBytes: int64) =
        sprintf
            "SELECT 結果が上限を超えました（上限: %s 行 / %s バイト）。LIMIT を付けるか、SessionOptions で上限を変更するか、Session.TryOpenReader で 1行ずつ読んでください"
            (if maxRows >= 0L then sprintf "%d" maxRows else "無制限")
            (if maxBytes >= 0L then sprintf "%d" maxBytes else "無制限")
        |> DbError.capacity
        |> err

    /// 上限つき materialize の走査本体（末尾再帰）。
    [<TailCall>]
    let rec private drainRows
        (rowLimit: bool)
        (byteLimit: bool)
        (maxRows: int64)
        (maxBytes: int64)
        (acc: SqlValue array list)
        (count: int64)
        (bytes: int64)
        (cur: Op)
        =
        match next cur with
        | OpStep.OpError e -> err e
        | OpStep.OpDone -> ret (List.rev acc |> List.toArray)
        | OpStep.OpRow(row, nxt) ->
            if rowLimit && count >= maxRows then
                resultTooLarge maxRows maxBytes
            else
                let rowBytes = int64 (ValueCodec.managedRowBytes row)

                if byteLimit && rowBytes > maxBytes - bytes then
                    resultTooLarge maxRows maxBytes
                else
                    drainRows rowLimit byteLimit maxRows maxBytes (row :: acc) (count + 1L) (bytes + rowBytes) nxt

    /// 全行を上限つきで配列に materialize する。
    /// 上限を超えたら `CapacityError` を返し、部分結果は返さない。
    /// 上限が負なら無制限（`SessionOptions` の `-1`、または `ResultLimits.Unlimited`）。
    let toArrayLimited (maxRows: int64) (maxBytes: int64) (op: Op) =
        drainRows (maxRows >= 0L) (maxBytes >= 0L) maxRows maxBytes [] 0L 0L op

    /// 全行を配列に materialize する（無制限）。
    let toArray = toArrayLimited System.Int64.MaxValue System.Int64.MaxValue
