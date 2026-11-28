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
    /// 外側スコープを先頭から走査して一致位置を返す。
    [<TailCall>]
    let rec private outerScopeIndexGo (r: ColumnRef) (scopes: OuterScope list) (i: int) =
        match scopes with
        | [] -> None
        | s :: rest ->
            match Schema.resolve r s.Schema with
            | Ok _ -> Some i
            | Error _ -> outerScopeIndexGo r rest (i + 1)

    /// 外側スコープの列参照を解決する。
    let private outerScopeIndex (inner: Schema) (scopes: OuterScope list) (r: ColumnRef) =
        if Schema.resolve r inner |> Result.isOk || shadowedByInner inner r then
            None
        else
            outerScopeIndexGo r scopes 0

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
    /// 式の中の外側参照位置を累積する畳み込み関数を作る。
    let private outerRefsAddExpr (scopes: OuterScope list) (scopeSchema: Schema) =
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

    /// 論理木を作業リストで辿って外側参照位置を集める。
    [<TailCall>]
    let rec private outerRefsCollect (addExpr: Schema -> 'a -> Expr -> 'a) (acc: 'a) (work: (Schema * Logical) list) =
        match work with
        | [] -> acc
        | (scopeSchema, t) :: rest ->
            match t with
            | Logical.UnionAll plans ->
                let branches = plans |> List.map (fun p -> p.Input, p.Tree)
                outerRefsCollect addExpr acc (branches @ rest)
            | _ ->
                let exprs, kids = childrenOf t
                let children = kids |> List.map (fun k -> scopeSchema, k)
                outerRefsCollect addExpr (List.fold (addExpr scopeSchema) acc exprs) (children @ rest)

    /// 相関サブクエリが参照する外側の列を集める。
    let private outerRefs (inner: Schema) (scopes: OuterScope list) (tree: Logical) =
        outerRefsCollect (outerRefsAddExpr scopes) [] [ (inner, tree) ] |> List.distinct

    /// 外側参照を外側行の値へ置き換える（内側スコープが優先）。
    /// 解決できない参照はそのまま残し、評価時の `NameError` に委ねる。
    /// 外側スコープを走査して列参照を行の値へ置き換える。
    [<TailCall>]
    let rec private substituteOuterGo (r: ColumnRef) (scopes: OuterScope list) =
        function
        | [] -> None
        | s :: rest ->
            match Schema.resolve r s.Schema with
            | Ok i ->
                match s.Row with
                | Some row when i >= 0 && i < row.Length ->
                    match row.[i] with
                    | SqlValue.Unloaded -> None
                    | v -> Some(Expr.Value v)
                | _ -> None
            | Error _ -> substituteOuterGo r scopes rest

    /// 内側で解決できない列参照を外側行の値へ置き換える。
    let private substituteOuterRef (inner: Schema) (scopes: OuterScope list) (r: ColumnRef) =
        if Schema.resolve r inner |> Result.isOk || shadowedByInner inner r then
            None
        else
            substituteOuterGo r scopes scopes

    /// 外側の列参照を値に置き換える。
    let private substituteOuter (inner: Schema) (scopes: OuterScope list) =
        Expr.rewrite (fun x ->
            match x with
            | Expr.Column c -> defaultArg (substituteOuterRef inner scopes c) x
            | Expr.Name n -> defaultArg (substituteOuterRef inner scopes { Qualifier = None; Name = n }) x
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
            /// `Rest` の各枝に対応する入力スキーマ（足りなければ `Fallback`）。
            RestSchemas: Schema list
            /// 枝のスキーマが分からないときの保守的な入力（その枝の入力）。
            Fallback: Schema
            /// 置換済みの枝（逆順）。
            Done: SetOperation list
        }

    /// 置換の作業状態。
    /// `Visit` は文とその入力スキーマ、および後続の枝（`UNION ALL`）の入力スキーマ。
    type private SubstituteStep =
        | Visit of SelectStatement * Schema * Schema list
        | Completed of SelectStatement

    /// 省略可能な式へ置換を適用する。
    let private substituteOpt (sub: 'a -> Result<'b, 'c>) =
        function
        | None -> ret None
        | Some e -> sub e |> Result.map Some

    /// SELECT 文の各フィールドへ置換を適用する（集合演算の枝は外す）。
    let private substituteStatementFields
        (sub: Expr -> Result<Expr, DbError>)
        (subOpt: Expr option -> Result<Expr option, DbError>)
        (s: SelectStatement)
        =
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

    /// 集合演算の枝を明示スタックで降りて置換する。
    [<TailCall>]
    let rec private substituteStatementLoop
        (substituteFields: Schema -> SelectStatement -> Result<SelectStatement, DbError>)
        (frames: SubstituteFrame list)
        =
        function
        | Visit(s, inner, branchSchemas) ->
            match substituteFields inner s with
            | Ok baseStmt ->
                match s.SetOperations with
                | [] -> substituteStatementLoop substituteFields frames (Completed baseStmt)
                | first :: rest ->
                    let firstSchema, restSchemas =
                        match branchSchemas with
                        | schema :: tail -> schema, tail
                        | [] -> inner, []

                    substituteStatementLoop
                        substituteFields
                        ({
                            Base = baseStmt
                            Current = first
                            Rest = rest
                            RestSchemas = restSchemas
                            Fallback = inner
                            Done = []
                         }
                         :: frames)
                        (Visit(first.Statement, firstSchema, []))
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
                    substituteStatementLoop
                        substituteFields
                        outerFrames
                        (Completed
                            { frame.Base with
                                SetOperations = List.rev done'
                            })
                | next :: rest ->
                    let nextSchema, restSchemas =
                        match frame.RestSchemas with
                        | schema :: tail -> schema, tail
                        | [] -> frame.Fallback, []

                    substituteStatementLoop
                        substituteFields
                        ({ frame with
                            Current = next
                            Rest = rest
                            RestSchemas = restSchemas
                            Done = done'
                         }
                         :: outerFrames)
                        (Visit(next.Statement, nextSchema, []))

    let private substituteFields (scopes: OuterScope list) (inner: Schema) (s: SelectStatement) =
        let sub = substituteOuter inner scopes
        substituteStatementFields sub (substituteOpt sub) s

    /// 文の各式に外側参照の置換を適用する。
    /// 入れ子のサブクエリ（`Subquery` / `Exists`）の内部には入らない
    /// （そのスコープは実行時に別途解決する）。
    /// 集合演算の枝は自前の入力スキーマを持つので、`branchSchemas`
    /// （コア → `UNION ALL` の順）の枝ごとのスキーマで外側参照を判定する。
    /// 枝 2 に外側と同じ名前の表があるだけで、枝 1 の外側参照を内側と誤判定しない。
    /// 枝は明示スタックで降りて戻りながら組み直す（自己再帰は全経路で末尾）。
    let private substituteStatement (branchSchemas: Schema list) (scopes: OuterScope list) (stmt: SelectStatement) =
        let coreSchema, setOpSchemas =
            match branchSchemas with
            | schema :: rest -> schema, rest
            | [] -> Schema.Empty, []

        substituteStatementLoop (substituteFields scopes) [] (Visit(stmt, coreSchema, setOpSchemas))

    [<TailCall>]
    let rec private branchesOf =
        function
        | Logical.UnionAll plans -> plans
        | Logical.Sort(_, child) -> branchesOf child
        | Logical.Limit(_, _, child) -> branchesOf child
        | _ -> []

    /// 相関サブクエリの枝ごとの入力スキーマ（コア → `UNION ALL` の順）。
    /// 複合文の枝は入力スキーマが違う。全体を連結したスキーマで外側参照を判定すると、
    /// 枝 2 に同名の表があるだけで枝 1 の外側参照を内側と誤判定する。
    /// 枝の数が文と合わないときは全体の入力（保守的）で埋める。
    let private branchSchemasOf (plan: PlannedQuery) (stmt: SelectStatement) =
        let branches = branchesOf plan.Tree

        List.init (1 + stmt.SetOperations.Length) (fun i ->
            List.tryItem i branches
            |> Option.map (fun (p: PlannedQuery) -> p.Input)
            |> Option.defaultValue plan.Input)

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

    /// 集合演算の枝から ORDER BY / LIMIT / OFFSET / 集合演算を除く。
    let private clearSetOperations (s: SelectStatement) =
        { s with
            OrderBy = []
            Limit = None
            Offset = None
            SetOperations = []
        }

    /// 文の入力（FROM / JOIN）を実テーブルとして登録する。
    /// 派生テーブルはスキーマだけを連結する（カタログにも ctx.Schemas にも登録しない）。
    /// 派生の引き当ては**参照名**（別名があれば別名）で行う。
    /// 実表名で引くと、実表に別名を付けても他の派生の別名と衝突して別の計画を使う。
    let private registerSources (ctx: ExecContext) (derived: Map<string, PlannedQuery>) =
        foldOk
            (fun (c, acc) (r: TableRef) ->
                match derived |> Map.tryFind ((defaultArg r.Alias r.Name).ToLowerInvariant()) with
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
            (ctx, Schema.Empty)

    /// テーブル参照の重複判定に使う名前。
    let private sourceNameKey (r: TableRef) =
        (defaultArg r.Alias r.Name).ToLowerInvariant()

    /// サブクエリの列数が 1 でないときのエラー。
    let private singleColumnSubqueryMessage (count: int) =
        sprintf "サブクエリは 1列だけを返す必要があります（%d 列）" count |> DbError.typeError

    /// スカラー相関サブクエリの実行関数。
    let private runScalarCorrelatedSubquery
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (ctx: ExecContext)
        (scopes: OuterScope list)
        (sel: SelectStatement)
        (plan: PlannedQuery)
        (row: SqlValue array)
        =
        let withRow = withCurrentRow row scopes

        substituteStatement (branchSchemasOf plan sel) withRow sel
        >>= runner withRow ctx
        >>= fun (_, op) ->
            match Op.next op with
            | OpStep.OpError er -> err er
            | OpStep.OpDone -> ret SqlValue.Null
            | OpStep.OpRow(r, rest) ->
                match Op.next rest with
                | OpStep.OpError er -> err er
                | OpStep.OpDone -> ret r.[0]
                | OpStep.OpRow _ -> "スカラーサブクエリが 2行以上を返しました" |> DbError.constraintViolation |> err

    /// EXISTS 相関サブクエリの実行関数。
    let private runExistsCorrelatedSubquery
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (ctx: ExecContext)
        (scopes: OuterScope list)
        (sel: SelectStatement)
        (plan: PlannedQuery)
        (row: SqlValue array)
        =
        let withRow = withCurrentRow row scopes

        substituteStatement (branchSchemasOf plan sel) withRow sel
        >>= runner withRow ctx
        >>= fun (_, op) ->
            match Op.next op with
            | OpStep.OpError er -> err er
            | OpStep.OpDone -> ret (SqlValue.ofBool false)
            | OpStep.OpRow _ -> ret (SqlValue.ofBool true)

    /// IN 相関サブクエリの実行関数。
    let private runInCorrelatedSubquery
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (ctx: ExecContext)
        (scopes: OuterScope list)
        (sel: SelectStatement)
        (plan: PlannedQuery)
        (resolve: Expr -> SqlValue array -> Result<SqlValue, DbError>)
        (left: Expr)
        (negated: bool)
        (row: SqlValue array)
        =
        let withRow = withCurrentRow row scopes

        substituteStatement (branchSchemasOf plan sel) withRow sel
        >>= runner withRow ctx
        >>= fun (_, op) ->
            ValueSet.build ctx.Budget op
            >>= fun set ->
                let membership =
                    {
                        Id = System.Guid.NewGuid().ToString "N"
                        Contains = set.Contains
                        Release = fun () -> (set :> System.IDisposable).Dispose()
                    }

                let result = resolve (Expr.InSet(left, membership, negated)) row
                membership.Release()
                result

    /// FROM / JOIN の派生テーブルを計画する（CPS、実行はしない）。
    /// AST の `Derived` は実テーブル参照（別名）へ書き換え、
    /// 計画結果は別名の小文字をキーにした Map で返す。
    [<TailCall>]
    let rec private resolveSourcesC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (k: SelectStatement -> Map<string, PlannedQuery> -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        =
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
            resolveSourcesLoop runner k ctx stmt sources [] Map.empty

    /// 派生テーブルの計画ループ（`resolveSourcesC` の本体）。
    and [<TailCall>] private resolveSourcesLoop
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (k: SelectStatement -> Map<string, PlannedQuery> -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        (pending: TableSource list)
        (done': TableSource list)
        (plans: Map<string, PlannedQuery>)
        =
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
        | TableSource.Named r :: rest -> resolveSourcesLoop runner k ctx stmt rest (TableSource.Named r :: done') plans
        | TableSource.Derived(sel, alias) :: rest ->
            planCoreCWith
                runner
                []
                (fun plan ->
                    let plan =
                        { plan with
                            Output = withQualifier alias plan.Output
                        }

                    resolveSourcesLoop
                        runner
                        k
                        ctx
                        stmt
                        rest
                        (TableSource.Named { Name = alias; Alias = None } :: done')
                        (Map.add (alias.ToLowerInvariant()) plan plans))
                ctx
                sel

    /// 外側スコープ付きで SELECT を計画する（CPS）。実行はしない。
    /// `UNION ALL` の複合文は `planCompoundCWith` へ、
    /// 通常の SELECT は `planSimpleCWith` へ振り分ける。
    and private planCoreCWith
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
    and private planCompoundCWith
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (outer: OuterScope list)
        (k: PlannedQuery -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        =
        let branches = stmt :: (stmt.SetOperations |> List.map _.Statement)

        mapC
            (fun (b: SelectStatement) contn -> planCoreCWith runner outer contn ctx (clearSetOperations b))
            branches
            []
            (finishCompound runner outer k ctx stmt)

    /// 複合 SELECT の枝を検証して連結し、末尾の ORDER BY などを解決する。
    and private finishCompound
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (outer: OuterScope list)
        (k: PlannedQuery -> Result<'r, DbError>)
        (ctx: ExecContext)
        (stmt: SelectStatement)
        (plans: PlannedQuery list)
        =
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

                let duplicate =
                    sources
                    |> List.countBy sourceNameKey
                    |> List.tryFind (fun (_, c) -> c > 1)
                    |> Option.bind (fun (key, _) ->
                        sources
                        |> List.tryFind (fun r -> sourceNameKey r = key)
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

    /// 省略可能な式をサブクエリ解決してから継続へ渡す。
    and [<TailCall>] private resolveOptionC
        (runner: OuterScope list -> ExecContext -> SelectStatement -> Result<Schema * Op, DbError>)
        (scopes: OuterScope list)
        (ctx: ExecContext)
        (v: Expr option)
        (cont: Expr option -> Result<'r, DbError>)
        =
        match v with
        | None -> cont None
        | Some e -> resolveExprC runner scopes (fun e2 -> cont (Some e2)) ctx e

    /// 解決済みの各フィールドを SELECT へ戻して継続へ渡す。
    and [<TailCall>] private finishResolvedSelect
        (stmt: SelectStatement)
        (k: SelectStatement -> Result<'r, DbError>)
        items
        joins
        whereE
        groupBy
        having
        orderBy
        limit
        offset
        =
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
        let resolveItems =
            traverseC (fun (item: SelectItem) cont ->
                resolveExprC runner scopes (fun e -> cont { item with Expression = e }) ctx item.Expression)

        let resolveJoins =
            traverseC (fun j cont -> resolveExprC runner scopes (fun e -> cont { j with On = e }) ctx j.On)

        let resolveSorts =
            traverseC (fun (s: SortKey) cont ->
                resolveExprC runner scopes (fun e -> cont { s with Expression = e }) ctx s.Expression)

        let resolveExprs = traverseC (fun e cont -> resolveExprC runner scopes cont ctx e)

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
                    resolveOptionC runner scopes ctx stmt.Where (fun whereE ->
                        resolveExprs stmt.GroupBy [] (fun groupBy ->
                            resolveOptionC runner scopes ctx stmt.Having (fun having ->
                                resolveSorts stmt.OrderBy [] (fun orderBy ->
                                    resolveOptionC runner scopes ctx stmt.Limit (fun limit ->
                                        resolveOptionC runner scopes ctx stmt.Offset (fun offset ->
                                            finishResolvedSelect
                                                stmt
                                                k
                                                items
                                                joins
                                                whereE
                                                groupBy
                                                having
                                                orderBy
                                                limit
                                                offset))))))))

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
        match e with
        | Expr.Subquery sel ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    if plan.Header.Columns.Length <> 1 then
                        err (singleColumnSubqueryMessage plan.Header.Columns.Length)
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
                                    | OpStep.OpRow _ -> DbError.constraintViolation "スカラーサブクエリが 2行以上を返しました" |> err
                        | refs ->
                            let run row =
                                runScalarCorrelatedSubquery runner ctx scopes sel plan row

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
                            runExistsCorrelatedSubquery runner ctx scopes sel plan row

                        k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
                ctx
                sel
        | Expr.InSubquery(left, sel, negated) ->
            planCoreCWith
                runner
                scopes
                (fun plan ->
                    if plan.Header.Columns.Length <> 1 then
                        err (singleColumnSubqueryMessage plan.Header.Columns.Length)
                    else
                        match outerRefs plan.Input scopes plan.Tree with
                        | [] ->
                            Optimizer.exec plan.Context plan.Tree plan.Input
                            >>= fun op ->
                                ValueSet.build plan.Context.Budget op
                                >>= fun set ->
                                    let membership =
                                        {
                                            Id = SubqueryKey.ofStatement sel
                                            Contains = set.Contains
                                            Release = fun () -> (set :> System.IDisposable).Dispose()
                                        }

                                    k (Expr.InSet(left, membership, negated))
                        | refs ->
                            let currentSchema =
                                match scopes with
                                | s :: _ -> s.Schema
                                | [] -> Schema.Empty

                            let resolve = Eval.prepare currentSchema [ left ]

                            let run row =
                                runInCorrelatedSubquery
                                    runner
                                    ctx
                                    scopes
                                    sel
                                    plan
                                    (fun expr currentRow -> Eval.evalWith resolve currentRow expr)
                                    left
                                    negated
                                    row

                            match Eval.checkPrepared resolve with
                            | Error e -> err e
                            | Ok() -> k (Expr.CorrelatedSubquery { Run = run; Depths = depthsOf refs }))
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
    let resolveDmlExpr = resolveDmlExprWith Schema.Empty

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
