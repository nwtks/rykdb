// ===========================================================================
// OpsShared — 集約式の共通ヘルパー。
// ===========================================================================

namespace RykDb.Core

module internal OpsShared =
    /// 識別子の正規化キー（大小文字を畳み、長さで区切る）。
    let private identKey (s: string) =
        string s.Length + ":" + s.ToUpperInvariant()

    /// 列参照の正規化キー。裸の名前も同じ形にして照合する。
    let private columnKey (qualifier: string option) (name: string) =
        "C" + identKey (defaultArg qualifier "") + identKey name

    /// 実数の正規化キー（-0.0 を 0 に、特殊値は名前で表す）。
    let private canonicalReal (f: float) =
        if System.Double.IsNaN f then
            "NaN"
        elif System.Double.IsPositiveInfinity f then
            "Inf"
        elif System.Double.IsNegativeInfinity f then
            "-Inf"
        elif f = 0.0 then
            "0"
        else
            f.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

    /// SQL 値の正規化キー。型タグを必ず付け、表示が同じでも型が違う値を区別する。
    let private valueKey =
        function
        | SqlValue.Null -> "0"
        | SqlValue.Bool b -> if b then "1" else "2"
        | SqlValue.Int i -> "3" + string i
        | SqlValue.Real f -> "4" + canonicalReal f
        | SqlValue.Decimal d -> "5" + d.ToString System.Globalization.CultureInfo.InvariantCulture
        | SqlValue.Text s -> "6" + string s.Length + ":" + s
        | SqlValue.Blob b -> "7" + System.Convert.ToHexString b
        | SqlValue.Date d -> "8" + string d.DayNumber
        | SqlValue.Timestamp t -> "9" + string t.UtcDateTime.Ticks
        | SqlValue.Uuid g -> "A" + g.ToString "D"
        | SqlValue.Unloaded -> "B"

    /// 正規化キーを組み立てる手順。
    /// `Sub` の後続を先に与えておくことで、式の走査を末尾再帰のまま表現できる。
    type private KeyStep =
        /// そのまま連結する文字列
        | Text of string
        /// 式と、その式を出したあとに連結する手順
        | Sub of Expr * KeyStep list

    /// 子の並びを区切り文字で連結し、最後の項目に閉じ文字を付ける手順。
    /// 空なら閉じ文字だけを出す。
    let private separatedSubs (separator: string) (closer: string) =
        function
        | [] -> [ Text closer ]
        | items ->
            let n = List.length items

            items
            |> List.mapi (fun i e -> Sub(e, [ Text(if i = n - 1 then closer else separator) ]))

    /// 式 1つ分の手順を、後続の手順の前に展開する（自分では再帰しない）。
    let private exprSteps (next: KeyStep list) =
        let flag neg = if neg then "1" else "0"

        function
        | Expr.Literal l -> Text("L" + valueKey l.ToSqlValue) :: next
        | Expr.Value v -> Text("V" + valueKey v) :: next
        | Expr.Column c -> Text(columnKey c.Qualifier c.Name) :: next
        | Expr.Name n -> Text(columnKey None n) :: next
        | Expr.Star -> Text "S" :: next
        | Expr.QualifiedStar q -> Text("Q" + identKey q) :: next
        | Expr.Unary(op, x) -> Text("U" + op.Display + "(") :: Sub(x, [ Text ")" ]) :: next
        | Expr.Binary(op, a, b) ->
            Text("B" + op.Display + "(")
            :: Sub(a, [ Text "," ])
            :: Sub(b, [ Text ")" ])
            :: next
        | Expr.IsNull(x, neg) -> Text("I" + flag neg + "(") :: Sub(x, [ Text ")" ]) :: next
        | Expr.IsTruth(x, tv, neg) -> Text("T" + sprintf "%A" tv + flag neg + "(") :: Sub(x, [ Text ")" ]) :: next
        | Expr.Between(x, a, b, neg) ->
            Text("W" + flag neg + "(")
            :: Sub(x, [ Text "," ])
            :: Sub(a, [ Text "," ])
            :: Sub(b, [ Text ")" ])
            :: next
        | Expr.InList(x, xs, neg) ->
            Text("N" + flag neg + "(")
            :: Sub(x, [ Text ";" ])
            :: (separatedSubs "," ")" xs @ next)
        | Expr.Like(x, p, neg, esc) ->
            let escSteps =
                match esc with
                | Some escExpr -> [ Sub(escExpr, [ Text ")" ]) ]
                | None -> [ Text "-)" ]

            Text("K" + flag neg + "(")
            :: Sub(x, [ Text "," ])
            :: Sub(p, [ Text "," ])
            :: (escSteps @ next)
        | Expr.Case(branches, elseE) ->
            let branchSteps =
                branches
                |> List.mapi (fun i (w, t) ->
                    (if i > 0 then [ Text ";" ] else []) @ [ Sub(w, [ Text ":" ]); Sub(t, []) ])
                |> List.concat

            let elseSteps =
                match elseE with
                | Some elseExpr -> [ Sub(elseExpr, []) ]
                | None -> [ Text "-" ]

            Text "E(" :: (branchSteps @ [ Text ")|" ] @ elseSteps @ next)
        | Expr.FuncCall(n, args) -> Text("F" + identKey n + "(") :: (separatedSubs "," ")" args @ next)
        | Expr.Cast(x, dt) -> Text("D" + DataType.toSqlName dt + "(") :: Sub(x, [ Text ")" ]) :: next
        | Expr.Default -> Text "X" :: next
        | Expr.AggCall(f, distinct, arg) ->
            Text("A" + f.Name + (if distinct then "D" else "N") + "(")
            :: Sub(arg, [ Text ")" ])
            :: next
        | Expr.Subquery _
        | Expr.Exists _
        | Expr.InSubquery _
        | Expr.InSet _
        | Expr.CorrelatedSubquery _ as e -> Text("Z" + sprintf "%A" e) :: next

    /// 手順を末尾再帰で連結する（式の深さに依存せずスタックは一定）。
    [<TailCall>]
    let rec private writeKey (sb: System.Text.StringBuilder) =
        function
        | [] -> sb.ToString()
        | Text s :: rest ->
            sb.Append s |> ignore
            writeKey sb rest
        | Sub(e, closer) :: rest -> writeKey sb (exprSteps (closer @ rest) e)

    /// 式の正規化キー。
    /// 表示文字列は大小文字を区別し、リテラルの表示が衝突する（`1` と `1e0`）ため、意味キーには使わない。
    /// 識別子は大小無視、リテラルは型タグ付きで表す。
    let private exprKey (e: Expr) =
        writeKey (System.Text.StringBuilder()) [ Sub(e, []) ]

    /// 集約呼び出しの識別キー。
    let private aggKey (f: AggFunc) (distinct: bool) (arg: Expr) =
        exprKey (Expr.AggCall(f, distinct, arg))

    /// 集約呼び出しの表示文字列（`COUNT(*)` / `SUM(DISTINCT x)`）。
    /// `Expr.Name "COUNT(*)"` のような既存の互換経路だけに使う。
    let private aggDisplayKey (f: AggFunc) (distinct: bool) (arg: Expr) =
        if distinct then
            sprintf "%s(DISTINCT %s)" f.Name (Expr.toDisplayString arg)
        else
            sprintf "%s(%s)" f.Name (Expr.toDisplayString arg)

    /// 集約後の行のスロット番号表。
    /// GROUP BY 式が先頭に、集約呼び出しがその後に並ぶ（`Ops.aggregate` の出力順）。
    /// 行に依存しないので 1クエリにつき 1回だけ作る。
    /// 表示キーの表は `Expr.Name` の互換経路だけに使う（列やリテラルの照合には使わない）。
    let private aggOffsets (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) =
        let groupSlots = groupBy |> List.mapi (fun i e -> exprKey e, i) |> Map.ofList

        let aggSlots =
            aggs
            |> List.mapi (fun i (f, _, distinct, arg) -> aggKey f distinct arg, List.length groupBy + i)
            |> Map.ofList

        let groupDisplays =
            groupBy |> List.mapi (fun i e -> Expr.toDisplayString e, i) |> Map.ofList

        let aggDisplays =
            aggs
            |> List.mapi (fun i (f, _, distinct, arg) -> aggDisplayKey f distinct arg, List.length groupBy + i)
            |> Map.ofList

        let offsets = Map.fold (fun acc k v -> Map.add k v acc) groupSlots aggSlots

        let displayOffsets =
            Map.fold (fun acc k v -> Map.add k v acc) groupDisplays aggDisplays

        offsets, displayOffsets

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

    /// 木全体を置き換える必要がある GROUP BY 式のキー -> スロット。
    /// 空なら `substituteAggSlots` は木全体のキーを作らない
    /// （深い HAVING 式で O(深さ²) にならないようにするため）。
    let private wholeGroupOffsets =
        List.mapi (fun i e -> e, i)
        >> List.filter (fun (e, _) -> not (isLeafExpr e))
        >> List.map (fun (e, i) -> exprKey e, i)
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

    /// 正規化キーから集約後行のスロット参照を作る。
    let private aggSlotOf (offsets: Map<string, int>) (key: string) =
        match offsets |> Map.tryFind key with
        | Some i when i >= 0 -> Some(Expr.Column { Qualifier = None; Name = slotName i })
        | _ -> None

    /// 木全体を照合する GROUP BY 式のスロット参照を作る。
    let private aggWholeSlotOf (offsets: Map<string, int>) (wholeOffsets: Map<string, int>) (e: Expr) =
        if Map.isEmpty wholeOffsets || isLeafExpr e then
            None
        else
            aggSlotOf offsets (exprKey e)

    /// 集約後の式の葉をスロット参照へ置き換える。
    let private aggRewriteLeaf
        (offsets: Map<string, int>)
        (displayOffsets: Map<string, int>)
        (wholeOffsets: Map<string, int>)
        (e: Expr)
        =
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
                match aggSlotOf offsets (exprKey orig) with
                | Some l -> l
                | None -> orig
            | Expr.Name n as orig ->
                match aggSlotOf offsets (exprKey orig) with
                | Some l -> l
                | None ->
                    // `HAVING "COUNT(*)"` のような既存の名前経路だけ表示キーで照合する。
                    match aggSlotOf displayOffsets n with
                    | Some l -> l
                    | None -> orig
            | other -> other

    /// 集約後の式を、グループ値・集約値のスロット参照へ置き換える（計画時に 1 回）。
    /// 置換の構造は行の値に依存しないので、行ごとに組み直す必要はない。
    let private substituteAggSlots
        (offsets: Map<string, int>)
        (displayOffsets: Map<string, int>)
        (wholeOffsets: Map<string, int>)
        =
        Expr.rewrite (aggRewriteLeaf offsets displayOffsets wholeOffsets)

    /// 集約後の行を読む式の一覧を、スロット参照の式と評価準備へ変換する。
    /// 幅は集約後の行幅（GROUP BY 数 + 集約数）。
    /// 解決できない参照が残っていれば、行を 1つも評価する前に NameError を返す。
    let prepareAggExprs (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (exprs: Expr list) =
        let offsets, displayOffsets = aggOffsets groupBy aggs
        let wholeOffsets = wholeGroupOffsets groupBy
        let width = groupBy.Length + aggs.Length

        let substituted =
            all (List.map (substituteAggSlots offsets displayOffsets wholeOffsets) exprs)

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
