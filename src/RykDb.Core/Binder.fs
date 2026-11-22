namespace RykDb.Core

module internal Binder =
    /// 修飾子無しの列名を「修飾名」にする。
    /// <remarks>
    /// 修飾子なしで一意に決まる場合にだけ修飾名を付ける。
    /// 曖昧なら付けない（実行時の Schema.resolve がエラーにする）。
    /// 葉だけを直し、非葉ノードは形を保ったまま子を 1つずつ加工してから
    /// `Expr.plugChildren` で組み直す（`Expr.rewrite` の評価機）。
    /// </remarks>
    let qualify (schema: Schema) =
        let uniqueQualifier (name: string) =
            let lower = name.ToLowerInvariant()

            let hits =
                schema.Columns
                |> Array.toList
                |> List.filter (fun (oc: OutputColumn) -> oc.Name.ToLowerInvariant() = lower)
                |> List.choose (fun (oc: OutputColumn) -> oc.Qualifier)

            match hits with
            | [ q ] -> Some q
            | _ -> None

        let rewriteLeaf x =
            match x with
            | Expr.Column c ->
                match c.Qualifier with
                | Some _ -> x
                | None ->
                    match uniqueQualifier c.Name with
                    | Some q -> Expr.Column { Qualifier = Some q; Name = c.Name }
                    | None -> x
            | Expr.Name name ->
                match uniqueQualifier name with
                | Some q -> Expr.Column { Qualifier = Some q; Name = name }
                | None -> x
            | _ -> x

        Expr.rewrite rewriteLeaf

    /// 複数の式をまとめて修飾名へ。
    let qualifyMany (schema: Schema) =
        List.map (fun e -> qualify schema e) >> all

    /// 式が指定したテーブルの列だけを参照しているか。
    /// 修飾子のない列はどのテーブル由来か特定できないため、false とする
    /// （曖昧な列を誤って pushdown しないため）。
    let referencesOnly (table: string) =
        let lower = table.ToLowerInvariant()

        Expr.columnRefs
        >> List.map (fun c ->
            match c.Qualifier with
            | Some q -> (q + "." + c.Name).ToLowerInvariant()
            | None -> c.Name.ToLowerInvariant())
        >> List.forall (fun full ->
            match full.LastIndexOf '.' with
            | -1 -> false
            | i -> full.[0 .. i - 1] = lower)

    /// 修飾子なしの列が、結合後スキーマで複数テーブルにまたがって存在するか。
    /// そのような式はどのテーブルにも pushdown してはいけない
    /// （各テーブルで個別に評価すると、曖昧エラーにならず意味が変わる）。
    let hasAmbiguousColumn (schema: Schema) =
        Expr.fold
            (fun acc x ->
                match x with
                | Expr.Column { Qualifier = None; Name = n } -> n :: acc
                | Expr.Name n -> n :: acc
                | _ -> acc)
            []
        >> List.exists (fun name ->
            schema.Columns
            |> Array.filter (fun (c: OutputColumn) -> Catalog.eqName c.Name name)
            |> Array.length
            |> fun count -> count > 1)

/// SELECT 項目の別名。
/// 別名を ORDER BY / GROUP BY / HAVING で参照できるようにするために使う。
module internal Alias =
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
        let namesOf =
            Expr.fold
                (fun acc node ->
                    match node with
                    | Expr.Name n -> n.ToLowerInvariant() :: acc
                    | _ -> acc)
                []

        let edges =
            aliases
            |> Map.map (fun _ v -> namesOf v |> List.filter (fun n -> Map.containsKey n aliases))

        let rec searchCycle start visited =
            function
            | [] -> false
            | n :: rest ->
                if n = start then
                    true
                elif Set.contains n visited then
                    searchCycle start visited rest
                else
                    let next =
                        match Map.tryFind n edges with
                        | Some l -> l
                        | None -> []

                    searchCycle start (Set.add n visited) (next @ rest)

        let reachesSelf start =
            match Map.tryFind start edges with
            | Some l -> l
            | None -> []
            |> searchCycle start Set.empty

        aliases |> Map.filter (fun k _ -> not (reachesSelf k))

    /// 別名を式に差し替える。
    /// 入力のスキーマに同名の列があれば列を優先する（曖昧を避ける）。
    /// 差し替えた式の中の名前も差し替え対象になる（別名の連鎖を解決する）。
    /// （`Expr.rewrite` はplugChildren の失敗を返すので Result になる）
    let substitute (schema: Schema) (aliases: Map<string, Expr>) =
        let aliases = dropCyclic aliases

        let rewriteLeaf x =
            match x with
            | Expr.Name name ->
                match Map.tryFind (name.ToLowerInvariant()) aliases with
                | Some replacement ->
                    match Schema.resolve { Qualifier = None; Name = name } schema with
                    | Ok _ -> x
                    | Error _ -> replacement
                | None -> x
            | _ -> x

        Expr.rewrite rewriteLeaf

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
