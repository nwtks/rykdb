// ===========================================================================
// Plan — SELECT のサブクエリ解決・派生テーブル・実行の入口（`runSelect`）。
// ===========================================================================

namespace RykDb.Core

module internal Plan =
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
        if Schema.resolve r inner |> Result.isOk || shadowedByInner inner r then
            None
        else
            let rec go i =
                function
                | [] -> None
                | s :: rest ->
                    match Schema.resolve r s.Schema with
                    | Ok _ -> Some i
                    | Error _ -> go (i + 1) rest

            go 0 scopes

    /// 論理木が直接参照する式を列挙する。
    /// 派生テーブル・集合演算の枝は別スコープなので含めない。
    let private childrenOf =
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
        | Logical.Derived _
        | Logical.UnionAll _ -> [], []

    /// 論理木が相関している外側スコープの位置（0 = 直近の外側）を集める。
    /// 入れ子の相関サブクエリは、その `Depths` のうち深さ 2 以上
    /// （このクエリより外側への依存）だけを引き継ぐ。
    /// 集合演算の枝は入力スキーマが枝ごとに違うので、
    /// 作業リストに「その木を評価する入力スキーマ」を一緒に積む。
    let private outerRefs (inner: Schema) (scopes: OuterScope list) (tree: Logical) =
        let addExpr scopeSchema =
            Expr.fold (fun acc ->
                function
                | Expr.Column c ->
                    match outerScopeIndex scopeSchema scopes c with
                    | Some i -> i :: acc
                    | None -> acc
                | Expr.Name n ->
                    match outerScopeIndex scopeSchema scopes { Qualifier = None; Name = n } with
                    | Some i -> i :: acc
                    | None -> acc
                | Expr.CorrelatedSubquery c ->
                    c.Depths
                    |> List.filter (fun d -> d >= 2)
                    |> List.fold (fun acc d -> d - 2 :: acc) acc
                | _ -> acc)

        let rec collect acc =
            function
            | [] -> acc
            | (scopeSchema, t) :: rest ->
                match t with
                | Logical.UnionAll plans ->
                    let branches = plans |> List.map (fun p -> p.Input, p.Tree)
                    collect acc (branches @ rest)
                | _ ->
                    let exprs, kids = childrenOf t
                    let children = kids |> List.map (fun k -> scopeSchema, k)
                    collect (List.fold (addExpr scopeSchema) acc exprs) (children @ rest)

        collect [] [ (inner, tree) ] |> List.distinct

    /// 外側参照を外側行の値へ置き換える（内側スコープが優先）。
    /// 解決できない参照はそのまま残し、評価時の `NameError` に委ねる。
    let private substituteOuter (inner: Schema) (scopes: OuterScope list) =
        let subst r =
            if Schema.resolve r inner |> Result.isOk || shadowedByInner inner r then
                None
            else
                let rec go =
                    function
                    | [] -> None
                    | s :: rest ->
                        match Schema.resolve r s.Schema with
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

    /// 集合演算の枝を置換するときの作業フレーム。
    /// 枝は必ずコア（`SetOperations` を持たない）だが、
    /// 入れ子の相関も扱えるよう明示スタックで辿る。
    type private SubstituteFrame =
        {
            /// 枝以外を置換済みの文（`SetOperations` は空）。
            Base: SelectStatement
            /// 処理中の枝（`Kind` を保つため元のレコードを保持する）。
            Current: SetOperation
            /// まだ処理していない枝。
            Rest: SetOperation list
            /// 置換済みの枝（逆順）。
            Done: SetOperation list
        }

    /// 置換の作業状態（これから降りる文 / 組み上がった文）。
    type private SubstituteStep =
        | Visit of SelectStatement
        | Completed of SelectStatement

    /// 文の各式に外側参照の置換を適用する。
    /// 入れ子のサブクエリ（`Subquery` / `Exists`）の内部には入らない
    /// （そのスコープは実行時に別途解決する）。
    /// 集合演算の枝は自前の入力スキーマを持つが、外側参照の判定は
    /// 呼び出し側が渡した `inner`（枝の入力をすべて含む保守的なスキーマ）で行う。
    /// 枝は明示スタックで降りて戻りながら組み直す（自己再帰は全経路で末尾）。
    let private substituteStatement (inner: Schema) (scopes: OuterScope list) (stmt: SelectStatement) =
        let sub = substituteOuter inner scopes

        let subOpt =
            function
            | None -> ret None
            | Some e -> sub e |> Result.map Some

        let substituteFields s =
            result {
                let! items =
                    s.Items
                    |> List.map (fun i -> sub i.Expression |> Result.map (fun e -> { i with Expression = e }))
                    |> all

                let! joins =
                    s.Joins
                    |> List.map (fun j -> sub j.On |> Result.map (fun e -> { j with On = e }))
                    |> all

                let! whereE = subOpt s.Where
                let! groupBy = s.GroupBy |> List.map sub |> all
                let! having = subOpt s.Having

                let! orderBy =
                    s.OrderBy
                    |> List.map (fun k -> sub k.Expression |> Result.map (fun e -> { k with Expression = e }))
                    |> all

                let! limit = subOpt s.Limit
                let! offset = subOpt s.Offset

                return
                    { s with
                        Items = items
                        Joins = joins
                        Where = whereE
                        GroupBy = groupBy
                        Having = having
                        OrderBy = orderBy
                        Limit = limit
                        Offset = offset
                        SetOperations = []
                    }
            }

        let rec loop frames =
            function
            | Visit s ->
                match substituteFields s with
                | Ok baseStmt ->
                    match s.SetOperations with
                    | [] -> loop frames (Completed baseStmt)
                    | first :: rest ->
                        loop
                            ({
                                Base = baseStmt
                                Current = first
                                Rest = rest
                                Done = []
                             }
                             :: frames)
                            (Visit first.Statement)
                | Error e -> err e
            | Completed doneStmt ->
                match frames with
                | [] -> ret doneStmt
                | frame :: outerFrames ->
                    let done' =
                        { frame.Current with
                            Statement = doneStmt
                        }
                        :: frame.Done

                    match frame.Rest with
                    | [] ->
                        loop
                            outerFrames
                            (Completed
                                { frame.Base with
                                    SetOperations = List.rev done'
                                })
                    | next :: rest ->
                        loop
                            ({ frame with
                                Current = next
                                Rest = rest
                                Done = done'
                             }
                             :: outerFrames)
                            (Visit next.Statement)

        loop [] (Visit stmt)

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
        =
        match pending with
        | [] -> k (List.rev acc)
        | x :: rest -> f x (fun x2 -> traverseC f rest (x2 :: acc) k)

    /// CPS のリスト走査（要素の型が変わる版）。
    /// `traverseC` は入出力が同じ型のときだけ使える。
    /// 自己再帰は `f` へ渡すラムダの中にあるので、
    /// 監査上は非末尾として扱われる（`traverseC` と同じ）。
    [<TailCall>]
    let rec private mapC
        (f: 'a -> ('b -> Result<'r, DbError>) -> Result<'r, DbError>)
        (pending: 'a list)
        (acc: 'b list)
        (k: 'b list -> Result<'r, DbError>)
        =
        match pending with
        | [] -> k (List.rev acc)
        | x :: rest -> f x (fun y -> mapC f rest (y :: acc) k)

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
    /// `UNION ALL` の複合文は `planCompoundCWith` へ、
    /// 通常の SELECT は `planSimpleCWith` へ振り分ける。
    and [<TailCall>] private planCoreCWith
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (outer: OuterScope list)
        (k: PlannedQuery -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        =
        if List.isEmpty stmt.SetOperations then
            planSimpleCWith runner outer k ctx stmt
        else
            planCompoundCWith runner outer k ctx stmt

    /// `UNION ALL` の複合 SELECT を計画する（CPS）。
    /// 枝ごとに通常の SELECT として計画し、`Logical.UnionAll` で連結する。
    /// 列数は全枝で一致が必要（列名は先頭の SELECT に従う）。
    /// `ORDER BY` / `LIMIT` / `OFFSET` は連結後の結果全体へ掛ける
    /// （列名・別名・序数は先頭の SELECT の出力列で解決する）。
    and [<TailCall>] private planCompoundCWith
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (outer: OuterScope list)
        (k: PlannedQuery -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        =
        let branchOf s =
            { s with
                OrderBy = []
                Limit = None
                Offset = None
                SetOperations = []
            }

        let branches = stmt :: (stmt.SetOperations |> List.map _.Statement)

        let finish plans =
            match plans with
            | [] -> DbError.internalError "UNION ALL の枝が計画されていません" |> err
            | first :: _ ->
                let expected = first.Header.Columns.Length

                let mismatch =
                    plans
                    |> List.indexed
                    |> List.tryFind (fun (_, plan) -> plan.Header.Columns.Length <> expected)

                match mismatch with
                | Some(index, plan) ->
                    sprintf "UNION ALL の列数が一致しません（1番目: %d列、%d番目: %d列）" expected (index + 1) plan.Header.Columns.Length
                    |> DbError.create ErrorKind.SyntaxError
                    |> err
                | None ->
                    let tailStmt =
                        { stmt with
                            Items = []
                            From = None
                            Joins = []
                            Where = None
                            GroupBy = []
                            Having = None
                            SetOperations = []
                        }

                    let scopes = { Schema = first.Output; Row = None } :: outer

                    resolveSelectC
                        runner
                        scopes
                        (fun tail ->
                            PlanBuilder.applySetOperationTail first.Output tail (Logical.UnionAll plans)
                            >>= fun tree ->
                                k
                                    {
                                        Output = first.Output
                                        Header = first.Header
                                        Input = plans |> List.map _.Input |> Schema.concatAll
                                        Context = ctx
                                        Tree = tree
                                    })
                        ctx
                        tailStmt

        mapC (fun (b: SelectStatement) contn -> planCoreCWith runner outer contn ctx (branchOf b)) branches [] finish

    /// 外側スコープ付きで通常の SELECT を計画する（CPS）。実行はしない。
    /// `outer` は「この SELECT の外側」のスコープ列（先頭が直近）。
    /// `runner` は相関サブクエリを外側行ごとに再計画・実行する関数。
    and [<TailCall>] private planSimpleCWith
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (outer: OuterScope list)
        (k: PlannedQuery -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        =
        let originalItems = stmt.Items

        resolveSourcesC
            runner
            (fun rewritten derived ->
                let sources =
                    (match rewritten.From with
                     | None -> []
                     | Some r -> [ sourceRef r ])
                    @ (rewritten.Joins |> List.map (fun j -> sourceRef j.Table))

                let nameKey (r: TableRef) =
                    (defaultArg r.Alias r.Name).ToLowerInvariant()

                let duplicate =
                    sources
                    |> List.countBy nameKey
                    |> List.tryFind (fun (_, c) -> c > 1)
                    |> Option.bind (fun (key, _) ->
                        sources
                        |> List.tryFind (fun r -> nameKey r = key)
                        |> Option.map (fun r -> defaultArg r.Alias r.Name))

                match duplicate with
                | Some name -> sprintf "テーブル %s が重複しています（エイリアスを付けてください）" name |> DbError.name |> err
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
    and private resolveSelectC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (k: SelectStatement -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        =
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
    and [<TailCall>] private resolveExprC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (k: Expr -> Result<'r, DbError>)
        (ctx: ExecContext)
        (e: Expr)
        =
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
        =
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
                            >>= fun op ->
                                match Op.next op with
                                | OpStep.OpError er -> err er
                                | OpStep.OpDone -> k (Expr.Literal Literal.Null)
                                | OpStep.OpRow(row, rest) ->
                                    match Op.next rest with
                                    | OpStep.OpError er -> err er
                                    | OpStep.OpDone -> k (Expr.Value row.[0])
                                    | OpStep.OpRow _ -> DbError.constraintViolation twoRowsMessage |> err
                        | refs ->
                            let run row =
                                let withRow = withCurrentRow row scopes

                                substituteStatement plan.Input withRow sel
                                >>= runner withRow ctx
                                >>= fun (_, op) ->
                                    match Op.next op with
                                    | OpStep.OpError er -> err er
                                    | OpStep.OpDone -> ret SqlValue.Null
                                    | OpStep.OpRow(r, rest) ->
                                        match Op.next rest with
                                        | OpStep.OpError er -> err er
                                        | OpStep.OpDone -> ret r.[0]
                                        | OpStep.OpRow _ -> DbError.constraintViolation twoRowsMessage |> err

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
                        >>= fun op ->
                            match Op.next op with
                            | OpStep.OpError er -> err er
                            | OpStep.OpDone -> k (Expr.Literal(Literal.Bool false))
                            | OpStep.OpRow _ -> k (Expr.Literal(Literal.Bool true))
                    | refs ->
                        let run row =
                            let withRow = withCurrentRow row scopes

                            substituteStatement plan.Input withRow sel
                            >>= runner withRow ctx
                            >>= fun (_, op) ->
                                match Op.next op with
                                | OpStep.OpError er -> err er
                                | OpStep.OpDone -> ret (SqlValue.ofBool false)
                                | OpStep.OpRow _ -> ret (SqlValue.ofBool true)

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
                            >>= fun op ->
                                ValueSet.build plan.Context.Budget op
                                >>= fun set ->
                                    let membership =
                                        {
                                            Contains = set.Contains
                                            Release = fun () -> (set :> System.IDisposable).Dispose()
                                        }

                                    k (Expr.InSet(left, membership, negated))
                        | refs ->
                            let currentSchema =
                                match scopes with
                                | s :: _ -> s.Schema
                                | [] -> Schema.empty

                            let resolve = Eval.prepare currentSchema [ left ]

                            let run row =
                                let withRow = withCurrentRow row scopes

                                substituteStatement plan.Input withRow sel
                                >>= runner withRow ctx
                                >>= fun (_, op) ->
                                    ValueSet.build ctx.Budget op
                                    >>= fun set ->
                                        let membership =
                                            {
                                                Contains = set.Contains
                                                Release = fun () -> (set :> System.IDisposable).Dispose()
                                            }

                                        let result = Eval.evalWith resolve row (Expr.InSet(left, membership, negated))
                                        membership.Release()
                                        result

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
        =
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
    /// 計画関数 `planSelectCWith` に渡す必要があり `let rec` にしている。
    /// 実際の再帰呼び出しは `Run` の末尾位置で起きるので、外側行が増えてもフレームは積み上がらない。
    /// </remarks>
    let rec private runSubquery (outer: OuterScope list) (ctx: ExecContext) (stmt: SelectStatement) =
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
