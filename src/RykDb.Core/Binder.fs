namespace RykDb.Core

module internal Binder =
    /// 修飾子なしの列名が一意に決まる修飾子を返す。
    let private uniqueQualifierOf (schema: Schema) (name: string) =
        let lower = name.ToLowerInvariant()

        let hits =
            schema.Columns
            |> Array.toList
            |> List.filter (fun (oc: OutputColumn) -> oc.Name.ToLowerInvariant() = lower)
            |> List.choose (fun (oc: OutputColumn) -> oc.Qualifier)

        match hits with
        | [ q ] -> Some q
        | _ -> None

    /// 葉の列参照へ一意な修飾子を付ける。
    let private qualifyRewriteLeaf (schema: Schema) (e: Expr) =
        match e with
        | Expr.Column c ->
            match c.Qualifier with
            | Some _ -> e
            | None ->
                match uniqueQualifierOf schema c.Name with
                | Some q -> Expr.Column { Qualifier = Some q; Name = c.Name }
                | None -> e
        | Expr.Name name ->
            match uniqueQualifierOf schema name with
            | Some q -> Expr.Column { Qualifier = Some q; Name = name }
            | None -> e
        | _ -> e

    /// 修飾子無しの列名を「修飾名」にする。
    /// <remarks>
    /// 修飾子なしで一意に決まる場合にだけ修飾名を付ける。
    /// 曖昧なら付けない（実行時の Schema.resolve がエラーにする）。
    /// 葉だけを直し、非葉ノードは形を保ったまま子を 1つずつ加工してから
    /// `Expr.plugChildren` で組み直す（`Expr.rewrite` の評価機）。
    /// </remarks>
    let qualify (schema: Schema) (e: Expr) =
        match Expr.rewrite (qualifyRewriteLeaf schema) e with
        | Error err -> Error err
        | Ok qualified -> Eval.checkComparableTypes schema qualified |> Result.map (fun () -> qualified)

    /// 複数の式をまとめて修飾名へ。
    let qualifyMany (schema: Schema) =
        List.map (fun e -> qualify schema e) >> all

/// SELECT 項目の別名。
/// 別名を ORDER BY / GROUP BY / HAVING で参照できるようにするために使う。
module internal Alias =
    /// 式中の裸の名前を小文字で列挙する。
    let private aliasNamesOf =
        Expr.fold
            (fun acc node ->
                match node with
                | Expr.Name n -> n.ToLowerInvariant() :: acc
                | _ -> acc)
            []

    /// 別名の依存辺をたどり、開始別名へ循環するか調べる。
    [<TailCall>]
    let rec private aliasSearchCycle (edges: Map<'a, 'a list>) (start: 'a) (visited: Set<'a>) =
        function
        | [] -> false
        | n :: rest ->
            if n = start then
                true
            elif Set.contains n visited then
                aliasSearchCycle edges start visited rest
            else
                let next =
                    match Map.tryFind n edges with
                    | Some l -> l
                    | None -> []

                aliasSearchCycle edges start (Set.add n visited) (next @ rest)

    /// 別名が自分自身へ循環するか。
    let private aliasReachesSelf (edges: Map<'a, 'a list>) (start: 'a) =
        match Map.tryFind start edges with
        | Some l -> l
        | None -> []
        |> aliasSearchCycle edges start Set.empty

    /// 別名（小文字）→ 式 の表。別名が重複したときは後の項目を優先する。
    let ofItems =
        List.fold
            (fun acc item ->
                match item.Alias with
                | Some a -> Map.add (a.ToLowerInvariant()) item.Expression acc
                | None -> acc)
            Map.empty

    /// 自分自身に循環参照する別名を除く。
    /// （差し替えで無限ループにしないため）
    let dropCyclic (aliases: Map<string, Expr>) =
        let edges =
            aliases
            |> Map.map (fun _ v -> aliasNamesOf v |> List.filter (fun n -> Map.containsKey n aliases))

        aliases |> Map.filter (fun k _ -> not (aliasReachesSelf edges k))

    /// 別名を式へ差し替える葉の書き換え。
    let private substituteRewriteLeaf (schema: Schema) (aliases: Map<string, Expr>) (e: Expr) =
        match e with
        | Expr.Name name ->
            match Map.tryFind (name.ToLowerInvariant()) aliases with
            | Some replacement ->
                match Schema.resolve { Qualifier = None; Name = name } schema with
                | Ok _ -> e
                | Error _ -> replacement
            | None -> e
        | _ -> e

    /// 別名を式に差し替える。
    /// 入力のスキーマに同名の列があれば列を優先する（曖昧を避ける）。
    /// 差し替えた式の中の名前も差し替え対象になる（別名の連鎖を解決する）。
    /// （`Expr.rewrite` はplugChildren の失敗を返すので Result になる）
    let substitute (schema: Schema) (aliases: Map<string, Expr>) =
        Expr.rewrite (substituteRewriteLeaf schema (dropCyclic aliases))

    /// 集計を含む別名は差し替えない表（GROUP BY 用）。
    /// 集計をグループキーにすると意味が壊れるため。
    let withoutAggregates = Map.filter (fun (_: string) e -> not (Expr.isAggregate e))

module internal Predicate =
    /// 未処理の式のスタックを辿って `AND` を平坦化する。
    /// conjunct は逆順で積み、最後に `List.rev` する（末尾再帰・O(n)）。
    [<TailCall>]
    let rec private flattenLoop (acc: Expr list) (pending: Expr list) =
        match pending with
        | [] -> List.rev acc
        | Expr.Binary(BinaryOp.And, a, b) :: rest -> flattenLoop acc (a :: b :: rest)
        | x :: rest -> flattenLoop (x :: acc) rest

    /// `AND` を平坦化して conjunct のリストにする。
    /// <remarks>
    /// 両方の再帰が末尾位置にあるので、深い `AND` チェーンでもスタックが積み上がらない。
    /// </remarks>
    let splitConjuncts (e: Expr) = flattenLoop [] [ e ]

    /// conjunct を全部 `AND` で繋ぎ直す。
    let joinConjuncts =
        function
        | [] -> None
        | [ x ] -> Some x
        | x :: rest -> Some(List.fold (fun acc p -> Expr.Binary(BinaryOp.And, acc, p)) x rest)
