// ===========================================================================
// OpsShared — 集約式の共通ヘルパー。
// ===========================================================================

namespace RykDb.Core

module internal OpsShared =
    /// 集約呼び出しの識別キー（`COUNT(age)` のような表示文字列）。
    let private aggKey (f: AggFunc) (distinct: bool) (arg: Expr) =
        if distinct then
            sprintf "%s(DISTINCT %s)" f.Name (Expr.toDisplayString arg)
        else
            sprintf "%s(%s)" f.Name (Expr.toDisplayString arg)

    /// 集約後の行のスロット番号表。
    /// GROUP BY 式が先頭に、集約呼び出しがその後に並ぶ（`Ops.aggregate` の出力順）。
    /// 行に依存しないので 1クエリにつき 1回だけ作る。
    let private aggOffsets (groupBy: Expr list) =
        let groupSlots =
            groupBy |> List.mapi (fun i e -> Expr.toDisplayString e, i) |> Map.ofList

        List.mapi (fun i (f, _, distinct, arg) -> aggKey f distinct arg, List.length groupBy + i)
        >> Map.ofList
        >> Map.fold (fun acc k v -> Map.add k v acc) groupSlots

    /// 葉（列・リテラル・集約呼び出し）は既存の置換で扱える。
    /// それ以外の GROUP BY 式（`age + 1` など）だけ、木全体の表示文字列で照合する。
    let private isLeafExpr =
        function
        | Expr.Literal _
        | Expr.Value _
        | Expr.Column _
        | Expr.Name _
        | Expr.Star
        | Expr.QualifiedStar _
        | Expr.AggCall _ -> true
        | _ -> false

    /// 木全体を置き換える必要がある GROUP BY 式の表示 -> スロット。
    /// 空なら `substituteAggSlots` は表示文字列を作らない
    /// （深い HAVING 式で O(深さ²) にならないようにするため）。
    let private wholeGroupOffsets =
        List.mapi (fun i e -> e, i)
        >> List.filter (fun (e, _) -> not (isLeafExpr e))
        >> List.map (fun (e, i) -> Expr.toDisplayString e, i)
        >> Map.ofList

    /// 集約後の行のスロットを指す予約名。
    /// ソースに書けない制御文字から始め、ユーザーの識別子と衝突しないようにする。
    let private slotName (i: int) = "\u0001" + string i

    /// スロット参照だけを列に持つスキーマ（列位置 = スロット番号）。
    /// 置換後に残った列参照はここで解決できず、構築時の `checkPrepared` が NameError にする。
    let private slotSchema (width: int) =
        { Schema.Empty with
            Columns =
                Array.init width (fun i ->
                    {
                        Name = slotName i
                        Qualifier = None
                        DataType = None
                    })
        }

    /// 表示文字列から集約後行のスロット参照を作る。
    let private aggSlotOf (offsets: Map<string, int>) (key: string) =
        match offsets |> Map.tryFind key with
        | Some i when i >= 0 -> Some(Expr.Column { Qualifier = None; Name = slotName i })
        | _ -> None

    /// 木全体を照合する GROUP BY 式のスロット参照を作る。
    let private aggWholeSlotOf (offsets: Map<string, int>) (wholeOffsets: Map<string, int>) (e: Expr) =
        if Map.isEmpty wholeOffsets || isLeafExpr e then
            None
        else
            aggSlotOf offsets (Expr.toDisplayString e)

    /// 集約後の式の葉をスロット参照へ置き換える。
    let private aggRewriteLeaf (offsets: Map<string, int>) (wholeOffsets: Map<string, int>) (e: Expr) =
        match aggWholeSlotOf offsets wholeOffsets e with
        | Some l -> l
        | None ->
            match e with
            | Expr.AggCall(f, distinct, arg) ->
                match aggSlotOf offsets (aggKey f distinct arg) with
                | Some l -> l
                | None -> Expr.Literal Literal.Null
            | Expr.Literal l -> Expr.Literal l
            | Expr.Column _ as orig ->
                match aggSlotOf offsets (Expr.toDisplayString orig) with
                | Some l -> l
                | None -> orig
            | Expr.Name n as orig ->
                match aggSlotOf offsets n with
                | Some l -> l
                | None -> orig
            | other -> other

    /// 集約後の式を、グループ値・集約値のスロット参照へ置き換える（計画時に 1 回）。
    /// 置換の構造は行の値に依存しないので、行ごとに組み直す必要はない。
    let private substituteAggSlots (offsets: Map<string, int>) (wholeOffsets: Map<string, int>) (e: Expr) =
        Expr.rewrite (aggRewriteLeaf offsets wholeOffsets) e

    /// 集約後の行を読む式の一覧を、スロット参照の式と評価準備へ変換する。
    /// 幅は集約後の行幅（GROUP BY 数 + 集約数）。
    /// 解決できない参照が残っていれば、行を 1つも評価する前に NameError を返す。
    let prepareAggExprs (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (exprs: Expr list) =
        let offsets = aggOffsets groupBy aggs
        let wholeOffsets = wholeGroupOffsets groupBy
        let width = groupBy.Length + aggs.Length
        let substituted = all (List.map (substituteAggSlots offsets wholeOffsets) exprs)

        match substituted with
        | Error e -> err e
        | Ok slotExprs ->
            let resolve = Eval.prepare (slotSchema width) slotExprs

            match Eval.checkPrepared resolve with
            | Error e -> err e
            | Ok() -> ret (resolve, slotExprs)


    /// 集約の引数が `*` かどうか。
    let private aggIsStarArg =
        function
        | Expr.Star -> true
        | _ -> false

    /// 式中の集約呼び出しを畳み込んで集める。
    let private aggCollectAggs =
        Expr.fold (fun a node ->
            match node with
            | Expr.AggCall(f, distinct, x) -> (f, aggIsStarArg x, distinct, x) :: a
            | _ -> a)

    /// 式中に現れる集約呼び出しの一覧（実行順に揃える）。
    let aggCalls = List.fold aggCollectAggs [] >> List.rev
