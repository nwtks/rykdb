// ===========================================================================
// OpsJoinShared — 結合の共通ヘルパー。
// ===========================================================================

namespace RykDb.Core

module internal OpsJoinShared =
    /// 行の末尾に NULL を足した新しい行を作る。
    /// `Array.create` + `Array.append` の 2回の割当を避ける。
    let padWithNulls (row: SqlValue array) (width: int) =
        let combined = Array.zeroCreate<SqlValue>(row.Length + width)
        Array.blit row 0 combined 0 row.Length
        Array.fill combined row.Length width SqlValue.Null
        combined

    /// 結合キーの値だけを取り出す（キー配列を作る）。
    let keyOf (row: SqlValue array) = Array.map (fun i -> row.[i])

    /// 結合キーを順序保持符号化する。
    /// ハッシュの等値判定を SQL の比較（`SqlValue.compareValuesV`）と一致させる:
    /// NaN は単一のビットパターンに正規化され、DECIMAL の 1.0 と 1.00 は同じキーになる。
    /// パーティション分割も同じ符号化を使うので、等しいキーは必ず同じパーティションに入る。
    let encodeKey (row: SqlValue array) (keys: int array) = KeyBuilder.ofValues (keyOf row keys)

    /// 等値結合のキーが NULL を含むか（SQL では NULL は一致しない）。
    let hasNullKey (row: SqlValue array) =
        Array.exists (fun i -> SqlValue.isNull row.[i])

    /// residual の ON 表現をリストにする。
    let residualExprs =
        function
        | None -> []
        | Some p -> [ p ]

    /// 等値キー以外の ON 句（residual）を評価する。
    /// 左右の行を連結せずに評価する。
    let residualMatches (resolve: Prepared) (lrow: SqlValue array) (rrow: SqlValue array) =
        function
        | None -> DbOk true
        | Some p -> Eval.matchesJoinedV resolve lrow rrow p

    /// ON 句・residual の列参照を検査する。
    /// 入力が 0 行でも未知の列は演算子の構築時に NameError にする。
    let checkExprs (leftSchema: Schema) (rightSchema: Schema) (exprs: Expr list) =
        Eval.checkPrepared (Eval.prepare (Schema.concat leftSchema rightSchema) exprs)

    /// residual（等値キー以外の ON 句）の列参照を検査する。
    let checkResidual (leftSchema: Schema) (rightSchema: Schema) =
        function
        | None -> Ok()
        | Some p -> checkExprs leftSchema rightSchema [ p ]

    /// nested loop 結合の内側ループ（1つの左行 × 右の全行）を回す共通処理。
    /// `resolve` は呼び出し側で 1回だけ準備する（左行ごとに作り直さない）。
    /// onExhausted matched は右を最後まで試し終えたときの Op。
    /// onError error matched は ON 句の評価が失敗したときの Op。
    /// matched は「この左行で既に一致を出したか」。
    /// INNER はどちらも matched を無視し、LEFT だけが NULL 拡張と継続を使い分ける。
    /// 結合述語を左右の行へ評価する。
    let joinPred (resolve: Prepared) (on: Expr) (l: SqlValue array) (r: SqlValue array) =
        Eval.matchesJoinedV resolve l r on
