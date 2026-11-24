// ===========================================================================
// Ops — 述語フィルタと射影。
// ソート・DISTINCT・集約は OpsSort / OpsDistinct / OpsAggregate。
// ===========================================================================

namespace RykDb.Core

open OpsShared

module internal Ops =
    /// 述語で弾く。
    /// 列参照が解決できなければ、行を評価する前に NameError を返す
    /// （入力が 0 行でも未知の列・関数名はエラー）。
    let filter (schema: Schema) (predicate: Expr) (src: Op) =
        let resolve = Eval.prepare schema [ predicate ]

        match Eval.checkPrepared resolve with
        | Error e -> err e
        | Ok() -> ret (Op.filter (fun row -> Eval.matchesWithV resolve row predicate) src)

    /// 射影式の評価結果を一時配列へ詰める再帰本体。
    [<TailCall>]
    let rec private projectFill (resolve: Prepared) (row: SqlValue array) (values: SqlValue array) (i: int) =
        function
        | [] -> DbOk values
        | e :: tail ->
            match Eval.evalWithV resolve row e with
            | DbErr er -> DbErr er
            | DbOk v ->
                values.[i] <- v
                projectFill resolve row values (i + 1) tail

    /// 射影式を 1行分まとめて評価する。
    let private projectEvalItems (resolve: Prepared) (row: SqlValue array) (exprs: Expr list) =
        let values = Array.zeroCreate<SqlValue> exprs.Length
        projectFill resolve row values 0 exprs

    /// 射影。
    /// `SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は `expandSchema`（論理的な入力スキーマ = FROM 順）で展開する。
    /// 行の評価は `evalSchema`（実行時の実際のレイアウト）で解決する。
    /// JOIN の並べ替えで両者がずれても、`*` の列順は論理スキーマのまま保たれる。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    /// 置換は計画時に 1回だけ行い、行ごとには組み直さない。
    /// 列参照が解決できなければ、行を評価する前に NameError を返す
    /// （集約後は GROUP BY 列と集約呼び出しを置き換えた後の式で検査する）。
    let project (expandSchema: Schema) (evalSchema: Schema) (groupBy: Expr list) (items: SelectItem list) (src: Op) =
        let aggCallsOf = List.map _.Expression >> aggCalls

        if List.isEmpty items then
            ret src
        else
            let projection = SelectProjection.create expandSchema items
            let allExprs = projection.Expressions
            let foundAggs = aggCallsOf projection.SelectItems

            if List.isEmpty foundAggs && List.isEmpty groupBy then
                let resolve = Eval.prepare evalSchema allExprs

                match Eval.checkPrepared resolve with
                | Error e -> err e
                | Ok() -> ret (Op.map (fun row -> projectEvalItems resolve row allExprs) src)
            else
                prepareAggExprs groupBy foundAggs allExprs
                |> Result.map (fun (resolveAgg, slotExprs) ->
                    Op.map (fun row -> projectEvalItems resolveAgg row slotExprs) src)
