// ===========================================================================
// Plan — SELECT のサブクエリ解決・派生テーブル・実行の入口（`runSelect`）。
// ===========================================================================

namespace RykDb.Core

module internal Plan =
    /// IN サブクエリの結果を保持する上限（管理メモリの概算バイト）。
    /// `Ops` の JOIN と同じ考え方で、OOM ではなく `CapacityError` で止める。
    let private SubqueryBudgetBytes = 256L * 1024L * 1024L

    /// IN サブクエリの行を上限付きで集める。
    let private collectSubqueryRows =
        let rec go used acc cur =
            match Op.next cur with
            | OpStep.OpError e -> err e
            | OpStep.OpDone -> ret (List.rev acc |> List.toArray)
            | OpStep.OpRow(row, nxt) ->
                let used = used + int64 (ValueCodec.managedRowBytes row) + 32L

                if used > SubqueryBudgetBytes then
                    sprintf "IN サブクエリの結果が多すぎます（概算 %d バイトの上限）" SubqueryBudgetBytes
                    |> DbError.capacity
                    |> err
                else
                    go used (row :: acc) nxt

        go 0L []

    /// 相関サブクエリの外側スコープ。
    /// 先頭が直近の外側（式を含むクエリの入力）で、後ろへ行くほど外側。
    type private OuterScope =
        {
            Schema: Schema
            /// 実行時に行が確定していないスコープ（計画中のクエリ自身）は None。
            Row: SqlValue array option
        }

    /// 修飾子が内側スコープのテーブルを指しているか。
    /// 内側に同名のテーブルがあるなら、列が無くても外側へはフォールバックしない（SQL のスコープ規則）。
    let private shadowedByInner (inner: Schema) (r: ColumnRef) =
        match r.Qualifier with
        | None -> false
        | Some q -> inner.Tables |> List.exists (Catalog.eqName q)

    /// 内側で解決できず、外側スコープに一致する列参照の位置（0 = 直近）を返す。
    let private outerScopeIndex (inner: Schema) (scopes: OuterScope list) (r: ColumnRef) =
        if Schema.resolve inner r |> Result.isOk || shadowedByInner inner r then
            None
        else
            let rec go i =
                function
                | [] -> None
                | s :: rest ->
                    match Schema.resolve s.Schema r with
                    | Ok _ -> Some i
                    | Error _ -> go (i + 1) rest

            go 0 scopes

    /// 論理木が直接参照する式を列挙する。
    /// 派生テーブルの内側は別スコープなので含めない。
    let private treeExprs (tree: Logical) =
        let childrenOf =
            function
            | Logical.Filter(e, child) -> [ e ], [ child ]
            | Logical.Project(items, child) -> items |> List.map _.Expression, [ child ]
            | Logical.Join(_, _, on, left, right) -> [ on ], [ left; right ]
            | Logical.Aggregate(groupBy, aggs, child) -> groupBy @ (aggs |> List.map (fun (_, _, _, e) -> e)), [ child ]
            | Logical.Having(h, aggs, groupBy, child) ->
                h :: (aggs |> List.map (fun (_, _, _, e) -> e)) @ groupBy, [ child ]
            | Logical.Distinct child -> [], [ child ]
            | Logical.Sort(keys, child) -> keys |> List.map _.Expression, [ child ]
            | Logical.Limit(_, _, child) -> [], [ child ]
            | Logical.Scan _
            | Logical.Values _
            | Logical.Derived _ -> [], []

        let rec go acc pending =
            match pending with
            | [] -> List.rev acc
            | t :: rest ->
                let exprs, kids = childrenOf t
                go (List.rev exprs @ acc) (kids @ rest)

        go [] [ tree ]

    /// 論理木が相関している外側スコープの位置（0 = 直近の外側）を集める。
    /// 入れ子の相関サブクエリは、その `Depths` のうち深さ 2 以上
    /// （このクエリより外側への依存）だけを引き継ぐ。
    let private outerRefs (inner: Schema) (scopes: OuterScope list) =
        let addExpr =
            Expr.fold (fun acc ->
                function
                | Expr.Column c ->
                    match outerScopeIndex inner scopes c with
                    | Some i -> i :: acc
                    | None -> acc
                | Expr.Name n ->
                    match outerScopeIndex inner scopes { Qualifier = None; Name = n } with
                    | Some i -> i :: acc
                    | None -> acc
                | Expr.CorrelatedSubquery c ->
                    c.Depths
                    |> List.filter (fun d -> d >= 2)
                    |> List.fold (fun acc d -> d - 2 :: acc) acc
                | _ -> acc)

        treeExprs >> List.fold addExpr [] >> List.distinct

    /// 外側参照を外側行の値へ置き換える（内側スコープが優先）。
    /// 解決できない参照はそのまま残し、評価時の `NameError` に委ねる。
    let private substituteOuter (inner: Schema) (scopes: OuterScope list) =
        let subst r =
            if Schema.resolve inner r |> Result.isOk || shadowedByInner inner r then
                None
            else
                let rec go =
                    function
                    | [] -> None
                    | s :: rest ->
                        match Schema.resolve s.Schema r with
                        | Ok i ->
                            match s.Row with
                            | Some row when i >= 0 && i < row.Length -> Some(Expr.Value row.[i])
                            | _ -> None
                        | Error _ -> go rest

                go scopes

        Expr.rewrite (fun x ->
            match x with
            | Expr.Column c -> defaultArg (subst c) x
            | Expr.Name n -> defaultArg (subst { Qualifier = None; Name = n }) x
            | _ -> x)

    /// 文の各式に外側参照の置換を適用する。
    /// 入れ子のサブクエリ（`Subquery` / `Exists`）の内部には入らない
    /// （そのスコープは実行時に別途解決する）。
    let private substituteStatement (inner: Schema) (scopes: OuterScope list) (stmt: SelectStatement) =
        let sub = substituteOuter inner scopes

        let subOpt =
            function
            | None -> ret None
            | Some e -> sub e |> Result.map Some

        result {
            let! items =
                stmt.Items
                |> List.map (fun i -> sub i.Expression |> Result.map (fun e -> { i with Expression = e }))
                |> all

            let! joins =
                stmt.Joins
                |> List.map (fun j -> sub j.On |> Result.map (fun e -> { j with On = e }))
                |> all

            let! whereE = subOpt stmt.Where
            let! groupBy = stmt.GroupBy |> List.map sub |> all
            let! having = subOpt stmt.Having

            let! orderBy =
                stmt.OrderBy
                |> List.map (fun k -> sub k.Expression |> Result.map (fun e -> { k with Expression = e }))
                |> all

            let! limit = subOpt stmt.Limit
            let! offset = subOpt stmt.Offset

            return
                { stmt with
                    Items = items
                    Joins = joins
                    Where = whereE
                    GroupBy = groupBy
                    Having = having
                    OrderBy = orderBy
                    Limit = limit
                    Offset = offset
                }
        }

    /// 現在のクエリの行を先頭に差し込んだスコープ列。
    let private withCurrentRow (row: SqlValue array) =
        function
        | s :: rest -> { s with Row = Some row } :: rest
        | [] -> []

    /// 相関サブクエリの依存深さ（1 = 直近の外側行）。
    let private depthsOf = List.map ((+) 1) >> List.sort

    /// CPS のリスト走査。
    /// 1件ずつ `f` に渡し、書き換え結果を積み直して最後に継続 `k` へ渡す。
    /// 継続呼び出しが末尾になるので、リスト長に依存せずスタックは積み上がらない。
    [<TailCall>]
    let rec private traverseC
        (f: 'a -> ('a -> Result<'r, DbError>) -> Result<'r, DbError>)
        (pending: 'a list)
        (acc: 'a list)
        (k: 'a list -> Result<'r, DbError>)
        : Result<'r, DbError> =
        match pending with
        | [] -> k (List.rev acc)
        | x :: rest -> f x (fun x2 -> traverseC f rest (x2 :: acc) k)

    /// FROM / JOIN の入力を `TableRef` に読み替える。
    /// `resolveSourcesC` が派生テーブルを実テーブル参照（別名）へ書き換えるので、
    /// ここに来る `Derived` は解決漏れ。
    let private sourceRef =
        function
        | TableSource.Named r -> r
        | TableSource.Derived(_, alias) -> { Name = alias; Alias = None }

    /// 派生テーブルの出力スキーマへ別名の修飾を付ける。
    let private withQualifier (alias: string) (schema: Schema) =
        {
            Columns = schema.Columns |> Array.map (fun c -> { c with Qualifier = Some alias })
            Tables = [ alias ]
        }

    /// 文の入力（FROM / JOIN）を実テーブルとして登録する。
    /// 派生テーブルはスキーマだけを連結する（カタログにも ctx.Schemas にも登録しない）。
    let private registerSources (ctx: ExecContext) (derived: Map<string, PlannedQuery>) =
        foldOk
            (fun (c, acc) (r: TableRef) ->
                match derived |> Map.tryFind (r.Name.ToLowerInvariant()) with
                | Some plan -> ret (c, Schema.concat acc plan.Output)
                | None ->
                    result {
                        let realName = r.Name
                        let! schema = ExecContext.schemaOf realName c
                        let c1 = ExecContext.withSchema realName schema c

                        let c2 =
                            match r.Alias with
                            | None -> c1
                            | Some a -> ExecContext.withSchema a schema c1

                        return c2, Schema.concat acc (Schema.ofTable realName r.Alias schema)
                    })
            (ctx, Schema.empty)

    /// FROM / JOIN の派生テーブルを計画する（CPS、実行はしない）。
    /// AST の `Derived` は実テーブル参照（別名）へ書き換え、
    /// 計画結果は別名の小文字をキーにした Map で返す。
    [<TailCall>]
    let rec private resolveSourcesC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (k: SelectStatement -> Map<string, PlannedQuery> -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<'r, DbError> =
        let rec go pending done' plans =
            match pending with
            | [] ->
                let rewritten = List.rev done'

                let fromSource, joinSources =
                    if Option.isSome stmt.From then
                        match rewritten with
                        | first :: rest -> Some first, rest
                        | [] -> None, []
                    else
                        None, rewritten

                let joins =
                    List.map2 (fun (j: JoinClause) t -> { j with Table = t }) stmt.Joins joinSources

                k
                    { stmt with
                        From = fromSource
                        Joins = joins
                    }
                    plans
            | TableSource.Named r :: rest -> go rest (TableSource.Named r :: done') plans
            | TableSource.Derived(sel, alias) :: rest ->
                planCoreCWith
                    runner
                    []
                    (fun plan ->
                        let plan =
                            { plan with
                                Output = withQualifier alias plan.Output
                            }

                        go
                            rest
                            (TableSource.Named { Name = alias; Alias = None } :: done')
                            (Map.add (alias.ToLowerInvariant()) plan plans))
                    ctx
                    sel

        let hasDerived =
            (match stmt.From with
             | Some(TableSource.Derived _) -> true
             | _ -> false)
            || stmt.Joins
               |> List.exists (fun j ->
                   match j.Table with
                   | TableSource.Derived _ -> true
                   | _ -> false)

        if not hasDerived then
            k stmt Map.empty
        else
            let sources = (stmt.From |> Option.toList) @ (stmt.Joins |> List.map _.Table)
            go sources [] Map.empty

    /// 外側スコープ付きで SELECT を計画する（CPS）。実行はしない。
    /// `outer` は「この SELECT の外側」のスコープ列（先頭が直近）。
    /// `runner` は相関サブクエリを外側行ごとに再計画・実行する関数。
    and [<TailCall>] private planCoreCWith
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (outer: OuterScope list)
        (k: PlannedQuery -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<'r, DbError> =
        let originalItems = stmt.Items

        resolveSourcesC
            runner
            (fun rewritten derived ->
                let sources =
                    (match rewritten.From with
                     | None -> []
                     | Some r -> [ sourceRef r ])
                    @ (rewritten.Joins |> List.map (fun j -> sourceRef j.Table))

                let duplicate =
                    sources
                    |> List.countBy (fun r -> defaultArg r.Alias r.Name)
                    |> List.tryFind (fun (_, c) -> c > 1)

                match duplicate with
                | Some(name, _) -> sprintf "テーブル %s が重複しています（エイリアスを付けてください）" name |> DbError.name |> err
                | None ->
                    registerSources ctx derived sources
                    >>= fun (ctx2, inputSchema) ->
                        let scopes = { Schema = inputSchema; Row = None } :: outer

                        resolveSelectC
                            runner
                            scopes
                            (fun resolved ->
                                PlanBuilder.buildLogicalWith ctx2 resolved derived
                                >>= fun tree ->
                                    let projection = SelectProjection.create inputSchema originalItems

                                    k
                                        {
                                            Output =
                                                {
                                                    Columns = projection.ReferenceHeader
                                                    Tables = []
                                                }
                                            Header =
                                                {
                                                    Columns = projection.DisplayHeader
                                                    Tables = []
                                                }
                                            Input = inputSchema
                                            Context = ctx2
                                            Tree = tree
                                        })
                            ctx
                            rewritten)
            ctx
            stmt

    /// サブクエリを解決してから論理木を組んで実行する（CPS）。
    /// 文の各フィールドを継続の連鎖で解決する。
    /// フィールドの数は固定なので、この関数自身は再帰しない。
    /// `scopes` の先頭はこのクエリの入力スキーマ（行は実行時に確定するので None）。
    /// <remarks>
    /// `planCoreCWith` / `resolveSourcesC` / `resolveSelectC` / `resolveExprC` / `resolveLeafC` は相互再帰する。
    /// どれも「残りの処理」を継続として受け取り、末尾で呼ぶので、
    /// サブクエリ・派生テーブルの入れ子段数に依存せずスタックは積み上がらない
    /// （Parser の式・SELECT と同じ方針）。
    /// </remarks>
    and [<TailCall>] private resolveSelectC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (k: SelectStatement -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<'r, DbError> =
        let resolveOpt v cont =
            match v with
            | None -> cont None
            | Some e -> resolveExprC runner scopes (fun e2 -> cont (Some e2)) ctx e

        let resolveItems =
            traverseC (fun (item: SelectItem) cont ->
                resolveExprC runner scopes (fun e -> cont { item with Expression = e }) ctx item.Expression)

        let resolveJoins =
            traverseC (fun j cont -> resolveExprC runner scopes (fun e -> cont { j with On = e }) ctx j.On)

        let resolveSorts =
            traverseC (fun (s: SortKey) cont ->
                resolveExprC runner scopes (fun e -> cont { s with Expression = e }) ctx s.Expression)

        let resolveExprs = traverseC (fun e cont -> resolveExprC runner scopes cont ctx e)

        let finish items joins whereE groupBy having orderBy limit offset =
            k
                { stmt with
                    Items = items
                    Joins = joins
                    Where = whereE
                    GroupBy = groupBy
                    Having = having
                    OrderBy = orderBy
                    Limit = limit
                    Offset = offset
                }

        let hasSubquery =
            stmt.Items |> List.exists (fun item -> Expr.hasSubquery item.Expression)
            || stmt.Joins |> List.exists (fun j -> Expr.hasSubquery j.On)
            || stmt.Where |> Option.exists Expr.hasSubquery
            || stmt.GroupBy |> List.exists Expr.hasSubquery
            || stmt.Having |> Option.exists Expr.hasSubquery
            || stmt.OrderBy |> List.exists (fun k -> Expr.hasSubquery k.Expression)
            || stmt.Limit |> Option.exists Expr.hasSubquery
            || stmt.Offset |> Option.exists Expr.hasSubquery

        if not hasSubquery then
            k stmt
        else
            resolveItems stmt.Items [] (fun items ->
                resolveJoins stmt.Joins [] (fun joins ->
                    resolveOpt stmt.Where (fun whereE ->
                        resolveExprs stmt.GroupBy [] (fun groupBy ->
                            resolveOpt stmt.Having (fun having ->
                                resolveSorts stmt.OrderBy [] (fun orderBy ->
                                    resolveOpt stmt.Limit (fun limit ->
                                        resolveOpt stmt.Offset (fun offset ->
                                            finish items joins whereE groupBy having orderBy limit offset))))))))

    /// 式の中のサブクエリを解決する（CPS）。
    and private resolveExprC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (k: Expr -> Result<'r, DbError>)
        (ctx: ExecContext)
        (e: Expr)
        : Result<'r, DbError> =
        if Expr.hasSubquery e then
            Expr.rewriteWithC (resolveLeafC runner scopes ctx) k e
        else
            k e

    /// 式の葉にあるサブクエリを解決して、値・リテラル・IN リストへ置き換える（CPS）。
    /// 外側の行を参照する（相関）サブクエリは、実行時に外側行を受け取る
    /// `CorrelatedSubquery` へ置き換える。
    /// `Expr.rewriteWithC` に渡すので、引数は `runner -> scopes -> ctx -> 式 -> 継続` の順。
    and [<TailCall>] private resolveLeafC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (ctx: ExecContext)
        (e: Expr)
        (k: Expr -> Result<'r, DbError>)
        : Result<'r, DbError> =
        let singleColumnMessage count =
            sprintf "サブクエリは 1列だけを返す必要があります（%d 列）" count |> DbError.typeError

        let twoRowsMessage = "スカラーサブクエリが 2行以上を返しました"

        match e with
        | Expr.Subquery sel ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    if plan.Header.Columns.Length <> 1 then
                        err (singleColumnMessage plan.Header.Columns.Length)
                    else
                        match outerRefs plan.Input scopes plan.Tree with
                        | [] ->
                            Optimizer.exec plan.Context plan.Tree plan.Input
                            |> Result.bind (fun op ->
                                match Op.next op with
                                | OpStep.OpError er -> err er
                                | OpStep.OpDone -> k (Expr.Literal Literal.Null)
                                | OpStep.OpRow(row, rest) ->
                                    match Op.next rest with
                                    | OpStep.OpError er -> err er
                                    | OpStep.OpDone -> k (Expr.Value row.[0])
                                    | OpStep.OpRow _ -> DbError.constraintViolation twoRowsMessage |> err)
                        | refs ->
                            let run row =
                                let withRow = withCurrentRow row scopes

                                substituteStatement plan.Input withRow sel
                                |> Result.bind (runner withRow ctx)
                                |> Result.bind (fun (_, op) ->
                                    match Op.next op with
                                    | OpStep.OpError er -> err er
                                    | OpStep.OpDone -> ret SqlValue.Null
                                    | OpStep.OpRow(r, rest) ->
                                        match Op.next rest with
                                        | OpStep.OpError er -> err er
                                        | OpStep.OpDone -> ret r.[0]
                                        | OpStep.OpRow _ -> DbError.constraintViolation twoRowsMessage |> err)

                            k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
                ctx
                sel
        | Expr.Exists sel ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    match outerRefs plan.Input scopes plan.Tree with
                    | [] ->
                        Optimizer.exec plan.Context plan.Tree plan.Input
                        |> Result.bind (fun op ->
                            match Op.next op with
                            | OpStep.OpError er -> err er
                            | OpStep.OpDone -> k (Expr.Literal(Literal.Bool false))
                            | OpStep.OpRow _ -> k (Expr.Literal(Literal.Bool true)))
                    | refs ->
                        let run row =
                            let withRow = withCurrentRow row scopes

                            substituteStatement plan.Input withRow sel
                            |> Result.bind (runner withRow ctx)
                            |> Result.bind (fun (_, op) ->
                                match Op.next op with
                                | OpStep.OpError er -> err er
                                | OpStep.OpDone -> ret (SqlValue.ofBool false)
                                | OpStep.OpRow _ -> ret (SqlValue.ofBool true))

                        k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
                ctx
                sel
        | Expr.InSubquery(left, sel, negated) ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    if plan.Header.Columns.Length <> 1 then
                        err (singleColumnMessage plan.Header.Columns.Length)
                    else
                        match outerRefs plan.Input scopes plan.Tree with
                        | [] ->
                            Optimizer.exec plan.Context plan.Tree plan.Input
                            |> Result.bind (fun op ->
                                match collectSubqueryRows op with
                                | Error er -> err er
                                | Ok rows ->
                                    let values = rows |> Array.map (fun row -> Expr.Value row.[0]) |> Array.toList
                                    k (Expr.InList(left, values, negated)))
                        | refs ->
                            let currentSchema =
                                match scopes with
                                | s :: _ -> s.Schema
                                | [] -> Schema.empty

                            let resolve = Eval.resolver currentSchema

                            let run row =
                                let withRow = withCurrentRow row scopes

                                substituteStatement plan.Input withRow sel
                                |> Result.bind (runner withRow ctx)
                                |> Result.bind (fun (_, op) ->
                                    collectSubqueryRows op
                                    |> Result.bind (fun rows ->
                                        let values = rows |> Array.map (fun r -> Expr.Value r.[0]) |> Array.toList
                                        Eval.evalWith resolve row (Expr.InList(left, values, negated))))

                            k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
                ctx
                sel
        | other -> k other

    /// サブクエリを解決してから論理木を組んで実行する（CPS）。
    let private planSelectCWith
        (outer: OuterScope list)
        (k: (Schema * Op) -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        : Result<'r, DbError> =
        planCoreCWith
            runner
            outer
            (fun plan ->
                Optimizer.exec plan.Context plan.Tree plan.Input
                >>= fun op -> k (plan.Header, op))
            ctx
            stmt

    /// 相関サブクエリを外側行ごとに再計画して実行する。
    /// 外側参照を値へ置換した文を渡すので、ここから先は通常のサブクエリ解決と同じ経路を通る。
    /// <remarks>
    /// 入れ子の相関サブクエリも同じ手順で実行するため、自分自身を runner として
    /// planner に渡す必要があり `let rec` にしている。
    /// 実際の再帰呼び出しは `Run` の末尾位置（`Result.bind` の継続）で起きるので、
    /// 外側行が増えてもフレームは積み上がらない。
    /// </remarks>
    let rec private runSubquery
        (outer: OuterScope list)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        : Result<Schema * Op, DbError> =
        runSubquery |> planSelectCWith outer ret ctx stmt

    /// DML（INSERT / UPDATE / DELETE）の式に含まれるサブクエリを解決する。
    /// `schema` は行ごとに評価するときの対象行のスキーマ
    /// （INSERT のように行がない文は `Schema.empty`）。
    let resolveDmlExprWith (schema: Schema) (ctx: ExecContext) =
        resolveExprC runSubquery [ { Schema = schema; Row = None } ] ret ctx

    /// DML の式に含まれるサブクエリを解決する（対象行のスキーマなし）。
    let resolveDmlExpr = resolveDmlExprWith Schema.empty

    /// 外側スコープなしで SELECT を計画・実行する（CPS）。
    let private planSelectC (k: (Schema * Op) -> Result<'r, DbError>) (ctx: ExecContext) (stmt: SelectStatement) =
        runSubquery |> planSelectCWith [] k ctx stmt

    /// SELECT 文を計画・実行して、結果（列名 + 未消費の Op）を返す。
    /// 行の配列化は呼び出し側（Engine）で行う。
    let runSelect =
        planSelectC (fun (schema, op) -> ret (schema.Columns |> Array.map (fun c -> c.Name), op))

    /// EXPLAIN 用に SELECT の論理計画だけを組み立てる（実行はしない）。
    /// サブクエリは計画時に解決するため、実際に評価されるものがある
    /// （通常の SELECT と同じ計画経路を通る）。
    let explainSelect (ctx: ExecContext) (stmt: SelectStatement) =
        planCoreCWith runSubquery [] ret ctx stmt
