// ===========================================================================
// OpsSort — 並べ替え。
// ===========================================================================

namespace RykDb.Core

open OpsShared

module internal OpsSort =
    /// ソートキーを順に比較する（比較不能な値は飛ばす）。
    [<TailCall>]
    let rec private sortCompareKeys (plan: (int * SortDirection) list) (a: SqlValue array) (b: SqlValue array) =
        match plan with
        | [] -> 0
        | (i, dir) :: tail ->
            match SqlValue.compareValuesForSortV a.[i] b.[i] with
            | DbErr _ -> sortCompareKeys tail a b
            | DbOk c ->
                if c = 0 then
                    sortCompareKeys tail a b
                else
                    match dir with
                    | SortDirection.Ascending -> c
                    | SortDirection.Descending -> -c

    /// 比較可能性のクラス。
    /// 同じクラス内は `compareValuesV` で比較できる。
    /// 単一の代表値だけで検査すると、有限 REAL の代表値が `INTEGER vs ±Infinity` を
    /// 見逃してしまうため、型ごとに代表値を持つ。
    /// 比較可能性は推移的でない（Int と有限 REAL は比較でき、有限 REAL と非有限 REAL も
    /// 比較できるが、Int と非有限 REAL は比較できない）ので、互換クラスではなく型で分ける。
    let private sortClass =
        function
        | SqlValue.Int _ -> 1
        | SqlValue.Real f -> if System.Double.IsFinite f then 2 else 3
        | SqlValue.Decimal _ -> 4
        | SqlValue.Date _ -> 5
        | SqlValue.Timestamp _ -> 6
        | SqlValue.Bool _ -> 7
        | SqlValue.Text _ -> 8
        | SqlValue.Blob _ -> 9
        | SqlValue.Uuid _ -> 10
        | SqlValue.Unloaded -> 11
        | SqlValue.Null -> 0

    /// キー列のクラス別代表値へ 1つ値を足す。
    /// 同じクラスなら代表値と 1 回だけ比べ、新しいクラスなら既存の全クラスと比べる。
    [<TailCall>]
    let rec private checkSortRepsAgainst (v: SqlValue) (reps: (int * SqlValue) list) =
        function
        | [] -> DbOk reps
        | (_, representative) :: tail ->
            match SqlValue.compareValuesV representative v with
            | DbErr e -> DbErr e
            | DbOk _ -> checkSortRepsAgainst v reps tail

    /// キー列の代表値表へ 1つ値を足す。
    let private checkSortRep (cls: int) (v: SqlValue) (reps: (int * SqlValue) list) =
        match reps |> List.tryFind (fun (c, _) -> c = cls) with
        | Some(_, representative) ->
            match SqlValue.compareValuesV representative v with
            | DbErr e -> DbErr e
            | DbOk _ -> DbOk reps
        | None ->
            match checkSortRepsAgainst v reps reps with
            | DbErr e -> DbErr e
            | DbOk _ -> DbOk((cls, v) :: reps)

    /// 外部ソートの代表値検査を回す本体。
    [<TailCall>]
    let rec private sortCheckLoop (row: SqlValue array) (acc: Map<int, (int * SqlValue) list>) =
        function
        | [] -> DbOk acc
        | (i, _) :: tail ->
            let v = row.[i]

            if SqlValue.isNull v then
                sortCheckLoop row acc tail
            else
                let cls = sortClass v

                match Map.tryFind i acc with
                | None -> sortCheckLoop row (Map.add i [ (cls, v) ] acc) tail
                | Some reps ->
                    match checkSortRep cls v reps with
                    | DbErr e -> DbErr e
                    | DbOk reps' -> sortCheckLoop row (Map.add i reps' acc) tail

    /// キー列の代表値を集めて比較不能な値を検出する。
    let private sortCheckNulls
        (plan: (int * SortDirection) list)
        (reps: Map<int, (int * SqlValue) list>)
        (row: SqlValue array)
        =
        sortCheckLoop row reps plan

    /// ORDER BY 式を列位置へ解決する。
    let private sortResolveColumn (resolve: Prepared) (k: SortKey) =
        match Schema.tryAsColumnRef k.Expression with
        | None -> None
        | Some c ->
            let bare = { Qualifier = None; Name = c.Name }

            match resolve.Resolve c with
            | Ok i -> Some(i, k.Direction)
            | Error _ ->
                match resolve.Resolve bare with
                | Ok i -> Some(i, k.Direction)
                | Error _ -> None

    /// 評価したキー値を一時配列へ詰める再帰本体。
    [<TailCall>]
    let rec private sortFillKeys (resolve: Prepared) (row: SqlValue array) (keyValues: SqlValue array) (i: int) =
        function
        | [] -> DbOk(Array.append row keyValues)
        | e :: tail ->
            match Eval.evalWithV resolve row e with
            | DbErr er -> DbErr er
            | DbOk v ->
                keyValues.[i] <- v
                sortFillKeys resolve row keyValues (i + 1) tail

    /// ORDER BY の式が列参照なのに解決できないときのエラーを返す。
    let private unresolvedSortColumn (schema: Schema) (keys: RykDb.Core.SortKey list) =
        keys
        |> List.tryPick (fun k ->
            match Schema.tryAsColumnRef k.Expression with
            | None -> None
            | Some c ->
                let bare = { Qualifier = None; Name = c.Name }

                match Schema.resolve c schema with
                | Ok _ -> None
                | Error _ ->
                    match Schema.resolve bare schema with
                    | Ok _ -> None
                    | Error e -> Some e)

    /// 列位置へ解決済みのキーでソートする。
    let private sortByColumns (budget: MemoryBudget) (plan: (int * SortDirection) list) (src: Op) =
        ExtSort.sortRows budget Map.empty (sortCheckNulls plan) (sortCompareKeys plan) src

    /// キー式を評価して行末へ足してからソートする。
    let private sortByExpressions
        (budget: MemoryBudget)
        (schema: Schema)
        (groupBy: Expr list)
        (aggs: (AggFunc * bool * bool * Expr) list)
        (keys: RykDb.Core.SortKey list)
        (src: Op)
        =
        let exprs = keys |> List.map _.Expression

        let prepared =
            if List.isEmpty groupBy && List.isEmpty aggs then
                let resolve = Eval.prepare schema exprs

                match Eval.checkPrepared resolve with
                | Error e -> err e
                | Ok() -> ret (resolve, exprs)
            else
                prepareAggExprs groupBy aggs exprs

        match prepared with
        | Error e -> err e
        | Ok(resolveKeys, slotExprs) ->
            let width = schema.Columns.Length
            let plan = keys |> List.mapi (fun i k -> width + i, k.Direction)

            let projected =
                Op.map
                    (fun row ->
                        let keyValues = Array.zeroCreate<SqlValue> slotExprs.Length
                        sortFillKeys resolveKeys row keyValues 0 slotExprs)
                    src

            ExtSort.sortRows budget Map.empty (sortCheckNulls plan) (sortCompareKeys plan) projected
            |> Result.map (Op.map (fun row -> DbOk(Array.sub row 0 width)))

    /// ソート。
    /// ORDER BY のキーを列位置へ解決できるならそのまま使い、式が含まれる場合はキーを評価して行末へ足してからソートする。
    /// 集約後の行（`groupBy` / `aggs` が非空）では、キー式をグループ値・集約値のスロット参照へ計画時に置き換えてから評価する
    /// （`ORDER BY COUNT(*)` / `ORDER BY age + 1` / 別名・序数経由の同じ形）。
    /// `*` / `t.*` はORDER BY には書けない。
    let sortBy
        (budget: MemoryBudget)
        (schema: Schema)
        (groupBy: Expr list)
        (aggs: (AggFunc * bool * bool * Expr) list)
        (keys: RykDb.Core.SortKey list)
        (src: Op)
        =
        let resolve = Eval.prepare schema (keys |> List.map _.Expression)

        match unresolvedSortColumn schema keys with
        | Some e -> err e
        | None ->
            if List.isEmpty keys then
                ret src
            else
                let columns = keys |> List.map (sortResolveColumn resolve)

                if List.forall Option.isSome columns then
                    sortByColumns budget (columns |> List.choose id) src
                else
                    sortByExpressions budget schema groupBy aggs keys src
