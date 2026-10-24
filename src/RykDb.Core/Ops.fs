namespace RykDb.Core

/// 1列の SqlValue 配列。
/// メモリ上は配列のまま、回さない。
type internal Row = SqlValue array

module internal Row =
    /// 全 NULL のときの等価キー用の印。
    /// 文字列に現れない制御文字を使う。
    [<Literal>]
    let private nullTag = "\u0000\0NULL"

    /// 等価キー（DISTINCT / GROUP BY / ハッシュ結合のキーに使う）。
    /// NULL は SQL では「他のどの値とも等しくない」ので、ここでは NULL も NULL 同士で等しい として扱う
    /// （GROUP BY で NULL が 1つのグループになる挙動に合わせるため）。
    let keyOf (row: Row) =
        row
        |> Array.map (fun v ->
            if SqlValue.isNull v then
                nullTag
            else
                SqlLiteral.toSqlLiteral v)
        |> String.concat "\u0001"

/// 1行取り出す演算子。
/// Done             = 終わった
/// Step (row, next) = row を返した。続きは next
/// Compute f        = f を呼んで 1行取り出す
///
/// Compute が「ソースだけを包む 1段」であることが要点。
/// 変換・フィルタ・結合はすべて「上流の Op を受け取って、`unit -> ...` を返す関数」として書けるので、
/// ソース側の状態を壊さずに不変のまま合成できる。
/// Step の next を呼ぶまで次の行は計算されないので、無限長でもメモリは 1行分だけ。
[<RequireQualifiedAccess>]
type internal Op =
    | Done
    /// 1行と「その次」。
    /// next は評価されるので長い連鎖を組むには向かない
    /// （遅延が必要なときは Compute を使う）。
    | Step of SqlValue array * Op
    /// 「次に 1行を返す関数」。
    /// 引数は unit なので遅延評価される。
    /// ソース側が「まだ計算していない」長い系列を表すときは必ずこれを使う。
    | Compute of (unit -> Result<(SqlValue array * Op) option, DbError>)

module internal Op =
    /// 空の演算子。
    let empty = Op.Done

    /// 1行だけの演算子。
    let ofRow (row: SqlValue array) = Op.Step(row, Op.Done)

    /// 配列を順に返す演算子。
    /// 次の行は呼び出すたびに計算する。
    let ofArray (rows: SqlValue array array) =
        let rec rowsFrom (i: int) =
            if i >= rows.Length then
                Op.Done
            else
                Op.Compute(fun () -> ret (Some(rows.[i], rowsFrom (i + 1))))

        rowsFrom 0

    /// seq を順に返す演算子（遅延）。
    /// 次の行は呼び出すたびに計算する。
    /// ソース seq は純粋（副作用なし）であることを前提とする。
    let ofSeq (rows: seq<SqlValue array>) =
        let e = rows.GetEnumerator()

        let rec rowsFrom () =
            if e.MoveNext() then
                Op.Compute(fun () -> ret (Some(e.Current, rowsFrom ())))
            else
                (e :> System.IDisposable).Dispose() |> ignore
                Op.Done

        rowsFrom ()

    /// 1行取り出す。
    let next (op: Op) =
        match op with
        | Op.Done -> ret None
        | Op.Step(row, nxt) -> ret (Some(row, nxt))
        | Op.Compute f -> f ()

    /// ソースの次の 1行を引き出して継続に渡す。
    /// 継続は「行と残りの Op」を受け取り、返した Result をそのまま返す。
    /// 返した「その行の次」の Op が演算子の残りになる。
    let pull (src: Op) (cont: SqlValue array -> Op -> Result<(SqlValue array * Op) option, DbError>) =
        Op.Compute(fun () ->
            next src
            >>= function
                | None -> ret None
                | Some(row, nxt) -> cont row nxt)

    /// 行を 1つ変換する。
    /// 失敗したら打切って Error。
    /// 変換後の行の「次」は map を通した残りにする
    /// （nxt をそのまま渡すと変換が 1行だけに効き、次から元に戻る）。
    let rec map (f: SqlValue array -> Result<SqlValue array, DbError>) (src: Op) =
        pull src (fun row nxt -> f row |> Result.map (fun r -> Some(r, map f nxt)))

    /// 述語で弾く。
    /// 偽の行は読み飛ばして次へ進む。
    /// p row >>= function で分岐する
    /// （if/then/else だと Result の短絡で Some(row, nxt) だけが残ることがある）。
    let filter (p: SqlValue array -> Result<bool, DbError>) (src: Op) =
        let rec seekMatch (cur: Op) =
            pull cur (fun row nxt ->
                let verdict = p row

                match verdict with
                | Ok true -> ret (Some(row, seekMatch nxt))
                | Ok false -> next (seekMatch nxt)
                | Error e -> err e)

        seekMatch src

    /// 先頭 n 行だけ。
    /// n <= 0 は 0 行（`LIMIT 0` は 0 行）。
    /// 「制限なし」は呼び出し側が take を呼ばないことで表す。
    let rec take (n: int) (src: Op) =
        if n <= 0 then
            Op.Done
        else
            pull src (fun row nxt -> ret (Some(row, take (n - 1) nxt)))

    /// 先頭 n 行を読み飛ばす。
    let skip (n: int) (src: Op) =
        let rec drop (left: int) (cur: Op) =
            if left <= 0 then
                cur
            else
                pull cur (fun _ nxt -> next (drop (left - 1) nxt))

        drop n src

    /// 全行を配列に materialize する。
    let toArray (op: Op) =
        let rows = ResizeArray<SqlValue array>()

        let rec drain (cur: Op) =
            match next cur with
            | Error e -> Error e
            | Ok None -> Ok(rows.ToArray())
            | Ok(Some(row, nxt)) ->
                rows.Add row
                drain nxt

        drain op

module internal Ops =
    /// 述語で弾く。
    let filter (schema: Schema) (src: Op) (predicate: Expr) =
        Op.filter (fun row -> Eval.matches schema row predicate) src

    /// ソート。
    /// ORDER BY のキー（列位置と方向）を先に解決してから比較関数へ落とす。
    /// `*` / `t.*` はORDER BY には書けない。
    let sortBy (schema: Schema) (src: Op) (keys: RykDb.Core.SortKey list) =
        let compareWith plan (a: SqlValue array) (b: SqlValue array) =
            let rec compareKeys (rest: (int * SortDirection) list) =
                match rest with
                | [] -> 0
                | (i, dir) :: tail ->
                    match SqlValue.compareValues a.[i] b.[i] with
                    | Error _ -> compareKeys tail
                    | Ok c ->
                        if c = 0 then
                            compareKeys tail
                        else
                            match dir with
                            | SortDirection.Ascending -> c
                            | SortDirection.Descending -> -c

            compareKeys plan

        if List.isEmpty keys then
            ret src
        else
            let resolved =
                keys
                |> List.map (fun k ->
                    let sk = k
                    let expr = sk.Expression
                    let dir = sk.Direction

                    match Schema.tryAsColumnRef expr with
                    | None ->
                        err (
                            sprintf "ORDER BY には列の直接参照だけを書けます（%s）" (Expr.toDisplayString expr)
                            |> DbError.name
                        )
                    | Some c ->
                        let bare = { Qualifier = None; Name = c.Name }

                        match Schema.resolve schema c with
                        | Ok i -> Ok(i, dir)
                        | Error _ -> Schema.resolve schema bare |> Result.map (fun i -> i, dir))
                |> all

            result {
                let! plan = resolved
                let! rows = Op.toArray src

                let validationError =
                    if rows.Length <= 1 then
                        None
                    else
                        plan
                        |> List.tryPick (fun (i, _) ->
                            let values = rows |> Array.map (fun row -> row.[i])

                            match values |> Array.tryFind (fun v -> not (SqlValue.isNull v)) with
                            | None -> None
                            | Some representative ->
                                values
                                |> Array.tryPick (fun v ->
                                    if SqlValue.isNull v then
                                        None
                                    else
                                        match SqlValue.compareValues representative v with
                                        | Error e -> Some e
                                        | Ok _ -> None))

                match validationError with
                | Some e -> return! Error e
                | None ->
                    let sorted = Array.sortWith (compareWith plan) rows
                    return Op.ofArray sorted
            }

    /// 重複除去。
    /// 保持する順序は最初に現れた順（SQL の DISTINCT と同じ）。
    /// 既出のキーを全部覚える必要がある。
    /// 「直前のキーだけ」で比較するのは `DISTINCT` ではなく `DISTINCT ON` にしかならない
    /// （`2, 1, 2` で 2 が 2 回目に出るため、直前比較では除去できない）。
    ///  Costs O(既出キー数 × 平均キー長)。メモリは全キーに比例する。
    /// 値が多いテーブルでは使えないが、`SELECT DISTINCT` の素直な実装としてはこれが正しい。
    let distinct (src: Op) =
        let rec skipSeen (seen: Set<string>) (cur: Op) =
            Op.pull cur (fun row nxt ->
                let k = Row.keyOf row

                if Set.contains k seen then
                    Op.next (skipSeen seen nxt)
                else
                    ret (Some(row, skipSeen (Set.add k seen) nxt)))

        skipSeen Set.empty src

    /// 式中に現れる集約呼び出しの一覧（実行順に揃える）。
    let aggCalls (exprs: Expr list) =
        let isStarArg (x: Expr) =
            match x with
            | Expr.Star -> true
            | _ -> false

        let collectAggs (e: Expr) (acc: (AggFunc * bool * bool * Expr) list) =
            Expr.fold
                (fun a node ->
                    match node with
                    | Expr.AggCall(f, distinct, x) -> (f, isStarArg x, distinct, x) :: a
                    | _ -> a)
                acc
                e

        exprs |> List.fold (fun acc e -> collectAggs e acc) [] |> List.rev

    /// 集約呼び出しの識別キー（`COUNT(age)` のような表示文字列）。
    let private aggKey (f: AggFunc) (distinct: bool) (arg: Expr) =
        if distinct then
            sprintf "%s(DISTINCT %s)" f.Name (Expr.toDisplayString arg)
        else
            sprintf "%s(%s)" f.Name (Expr.toDisplayString arg)

    /// 集約後の行のスロット番号表。
    /// GROUP BY 式が先頭に、集約呼び出しがその後に並ぶ（`Ops.aggregate` の出力順）。
    /// 行に依存しないので 1クエリにつき 1回だけ作る。
    let private aggOffsets (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) =
        let groupSlots =
            groupBy |> List.mapi (fun i e -> Expr.toDisplayString e, i) |> Map.ofList

        let aggSlots =
            aggs
            |> List.mapi (fun i (f, _, distinct, arg) -> aggKey f distinct arg, List.length groupBy + i)
            |> Map.ofList

        Map.fold (fun acc k v -> Map.add k v acc) groupSlots aggSlots

    /// 集約呼び出しの式を、計算済みの値（定数）へ置き換える。
    /// HAVING と SELECT 項目の両方で使う。
    /// `offsets` は `aggOffsets` の結果。
    /// 行に依存するのは `row` だけなので offsets は呼び出し側で 1回だけ作る
    /// （`project` / `havingFilter` は出力 1行ごとに呼ぶため、
    /// ここで作ると行数ぶんだけマップを組み直すことになる）。
    /// 組み立ては `Expr.rewrite`（末尾再帰の評価機）に任せるので、式の深さに依存しない。
    let private substituteAggs (offsets: Map<string, int>) (e: Expr) (row: SqlValue array) =
        let slot key =
            match offsets |> Map.tryFind key with
            | Some i when i >= 0 && i < row.Length -> Some(Expr.Literal(Literal.ofValue row.[i]))
            | _ -> None

        let rewriteLeaf (x: Expr) =
            match x with
            | Expr.AggCall(f, distinct, arg) ->
                match slot (aggKey f distinct arg) with
                | Some l -> l
                | None -> Expr.Literal Literal.Null
            | Expr.Literal l -> Expr.Literal l
            | Expr.Column _ as orig ->
                match slot (Expr.toDisplayString orig) with
                | Some l -> l
                | None -> orig
            | Expr.Name n as orig ->
                match slot n with
                | Some l -> l
                | None -> orig
            | other -> other

        Expr.rewrite rewriteLeaf e

    /// HAVING を張る。
    /// 集約後の行は位置のみの意味を持つので Schema.empty で評価する。
    /// offsets はクロージャを 1回作るときにだけ組み立て、行ごとに持ち越さない。
    let havingFilter (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) (predicate: Expr) =
        let offsets = aggOffsets groupBy aggs
        fun row -> substituteAggs offsets predicate row >>= Eval.matches Schema.empty row

    /// 集約の 1 項目分の_accumulator。
    /// SUM / AVG は decimal で累積し、最後に型を決める（丸め誤差を避ける）。
    type private Acc =
        {
            Kind: AggFunc
            Count: int64
            SumDecimal: decimal
            SawReal: bool
            SumReal: double
            Min: SqlValue option
            Max: SqlValue option
            First: SqlValue option
            Seen: SqlValue list
        }

    /// GROUP BY + 集約。
    /// groupBy が空でも集約があれば 1グループだけ出す
    /// （`SELECT COUNT(*) FROM 空のテーブル` が 0 ではなく 1行を返すため。SQL の通常の挙動）。
    let aggregate (schema: Schema) (src: Op) (groupBy: Expr list) (aggs: (AggFunc * bool * bool * Expr) list) =
        let order = ResizeArray<string>()

        let table =
            System.Collections.Generic.Dictionary<string, ResizeArray<SqlValue array>>()

        let keyOfRow (row: SqlValue array) =
            groupBy
            |> List.map (fun e -> Eval.eval schema row e)
            |> all
            |> Result.map (fun vs -> Row.keyOf (List.toArray vs))

        let rec groupRows (rows: SqlValue array array) (i: int) =
            if i >= rows.Length then
                let built = table |> Seq.map (fun kv -> kv.Key, List.ofSeq kv.Value) |> Map.ofSeq
                Ok(List.ofSeq order, built)
            else
                match keyOfRow rows.[i] with
                | Error e -> Error e
                | Ok k ->
                    match table.TryGetValue k with
                    | true, bucket -> bucket.Add rows.[i]
                    | _ ->
                        let bucket = ResizeArray<SqlValue array>()
                        bucket.Add rows.[i]
                        table.[k] <- bucket
                        order.Add k

                    groupRows rows (i + 1)

        let emptyAcc (kind: AggFunc) =
            {
                Kind = kind
                Count = 0L
                SumDecimal = 0M
                SawReal = false
                SumReal = 0.0
                Min = None
                Max = None
                First = None
                Seen = []
            }

        let addDecimal (a: decimal) (b: decimal) =
            let maxD = System.Decimal.MaxValue
            let minD = System.Decimal.MinValue

            if a > 0M && b > 0M && a > maxD - b || a < 0M && b < 0M && a < minD - b then
                err (DbError.constraintViolation "SUM の結果が DECIMAL の範囲を超えました")
            else
                ret (a + b)

        let keepExtreme
            (v: SqlValue)
            (wins: int -> bool)
            (bumped: Acc)
            (current: SqlValue option)
            (set: Acc -> SqlValue -> Acc)
            (seen: SqlValue list)
            =
            match current with
            | None -> ret (set { bumped with Seen = seen } v)
            | Some cur ->
                SqlValue.compareValues v cur
                |> Result.map (fun c ->
                    if wins c then
                        set { bumped with Seen = seen } v
                    else
                        { bumped with Seen = seen })

        let addValue (acc: Acc) (bump: Acc) (v: SqlValue) (newSeen: SqlValue list) =
            match acc.Kind with
            | AggFunc.Count -> ret { bump with Seen = newSeen }
            | AggFunc.Sum
            | AggFunc.Avg ->
                match v with
                | SqlValue.Real f ->
                    ret
                        { bump with
                            SawReal = true
                            SumReal = acc.SumReal + float acc.SumDecimal + f
                            SumDecimal = 0M
                            Seen = newSeen
                        }
                | SqlValue.Int _
                | SqlValue.Decimal _ ->
                    match SqlValue.asNumeric v with
                    | Some d ->
                        if acc.SawReal then
                            ret
                                { bump with
                                    SumReal = acc.SumReal + float d
                                    Seen = newSeen
                                }
                        else
                            addDecimal acc.SumDecimal d
                            |> Result.map (fun sum ->
                                { bump with
                                    SumDecimal = sum
                                    Seen = newSeen
                                })
                    | None -> err (DbError.internalError "数値の変換に失敗しました（内部不整合）")
                | _ ->
                    err (
                        sprintf "%s は数値だけを受け付けますが %s です" acc.Kind.Name (SqlLiteral.toSqlLiteral v)
                        |> DbError.typeError
                    )
            | AggFunc.Min -> keepExtreme v (fun c -> c < 0) bump acc.Min (fun b x -> { b with Min = Some x }) newSeen
            | AggFunc.Max -> keepExtreme v (fun c -> c > 0) bump acc.Max (fun b x -> { b with Max = Some x }) newSeen

        let addAcc (acc: Acc) (value: SqlValue option) (isStar: bool) (distinct: bool) =
            let alreadySeen =
                distinct
                && match value with
                   | Some v when not (SqlValue.isNull v) -> List.contains v acc.Seen
                   | _ -> false

            if alreadySeen then
                ret acc
            else
                let counted =
                    if isStar then
                        Some SqlValue.Null
                    else
                        match value with
                        | Some v when not (SqlValue.isNull v) -> Some v
                        | _ -> None

                let bump = { acc with Count = acc.Count + 1L }

                let newSeen =
                    if distinct then
                        match value with
                        | Some v when not (SqlValue.isNull v) -> v :: acc.Seen
                        | _ -> acc.Seen
                    else
                        acc.Seen

                match counted with
                | None -> ret { acc with Seen = newSeen }
                | Some v -> addValue acc bump v newSeen

        let finishAcc (acc: Acc) =
            match acc.Kind with
            | AggFunc.Count -> SqlValue.Int acc.Count
            | AggFunc.Sum ->
                if acc.Count = 0L then SqlValue.Null
                elif acc.SawReal then SqlValue.vReal acc.SumReal
                else SqlValue.Decimal acc.SumDecimal
            | AggFunc.Avg ->
                if acc.Count = 0L then
                    SqlValue.Null
                elif acc.SawReal then
                    SqlValue.vReal (acc.SumReal / float acc.Count)
                else
                    SqlValue.vReal (float acc.SumDecimal / float acc.Count)
            | AggFunc.Min -> defaultArg acc.Min SqlValue.Null
            | AggFunc.Max -> defaultArg acc.Max SqlValue.Null

        let add (star: bool) (distinct: bool) (arg: Expr) (acc: Acc) (row: SqlValue array) =
            if star then
                addAcc acc None true distinct
            else
                Eval.eval schema row arg >>= fun v -> addAcc acc (Some v) false distinct

        let oneAgg (members: SqlValue array seq) (spec: AggFunc * bool * bool * Expr) =
            let f, star, distinct, arg = spec
            let start = emptyAcc f

            match foldOk (add star distinct arg) start members with
            | Ok acc -> Ok(finishAcc acc)
            | Error e -> Error e

        let rowOf (table: Map<string, SqlValue array list>) (k: string) =
            match table |> Map.tryFind k with
            | None -> Ok None
            | Some members ->
                if List.isEmpty members then
                    Ok None
                else
                    result {
                        let first = List.head members
                        let! vs = groupBy |> List.map (fun e -> Eval.eval schema first e) |> all
                        let gv = List.toArray vs
                        let! av = aggs |> List.map (oneAgg members) |> all
                        return Some(Array.concat [| gv; List.toArray av |])
                    }

        let rec build (table: Map<string, SqlValue array list>) (ks: string list) (acc: ResizeArray<SqlValue array>) =
            match ks with
            | [] -> Ok(Op.ofArray (acc.ToArray()))
            | k :: tail ->
                match rowOf table k with
                | Error e -> Error e
                | Ok None -> build table tail acc
                | Ok(Some r) ->
                    acc.Add r
                    build table tail acc

        result {
            let! rows = Op.toArray src
            let! order, table = groupRows rows 0

            if List.isEmpty order && List.isEmpty groupBy then
                let vals =
                    aggs |> List.map (fun (f, _, _, _) -> finishAcc (emptyAcc f)) |> List.toArray

                return Op.ofArray [| vals |]
            else
                return! build table order (ResizeArray<SqlValue array>())
        }

    /// nested loop 結合の内側ループ（1つの左行 × 右の全行）を回す共通処理。
    /// onExhausted matched は右を最後まで試し終えたときの Op。
    /// onError error matched は ON 句の評価が失敗したときの Op。
    /// 失敗はエラーとして流す（黙って行を落とすと気づきにくい誤答になる）。
    /// matched は「この左行で既に一致を出したか」。
    /// INNER はどちらも matched を無視し、LEFT だけが NULL 拡張と継続を使い分ける。
    let private joinLoop
        (leftSchema: Schema)
        (rightSchema: Schema)
        (on: Expr)
        (onExhausted: bool -> Op)
        (onError: DbError -> bool -> Op)
        (rightRows: SqlValue array array)
        (lrow: SqlValue array)
        =
        let joinPred (leftSchema: Schema) (rightSchema: Schema) (on: Expr) (l: SqlValue array) (r: SqlValue array) =
            Eval.matches (Schema.concat leftSchema rightSchema) (Array.append l r) on

        let rec loop (matched: bool) (i: int) =
            if i >= rightRows.Length then
                onExhausted matched
            else
                let rrow = rightRows.[i]

                match joinPred leftSchema rightSchema on lrow rrow with
                | Ok true -> Op.Compute(fun () -> ret (Some(Array.append lrow rrow, loop true (i + 1))))
                | Ok false -> Op.Compute(fun () -> Op.next (loop matched (i + 1)))
                | Error e -> onError e matched

        loop false 0

    /// INNER JOIN（nested loop）。
    /// 右側は内側ループごとに先頭から引き直す。
    /// 一致が無い左行は捨て、ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let innerJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        let rec joinRows rightRows (cur: Op) =
            Op.pull cur (fun lrow lnext ->
                Op.next (
                    joinLoop
                        leftSchema
                        rightSchema
                        on
                        (fun _ -> joinRows rightRows lnext)
                        (fun e _ -> Op.Compute(fun () -> err e))
                        rightRows
                        lrow
                ))

        Op.toArray right |> Result.map (fun rightRows -> joinRows rightRows left)

    /// LEFT JOIN。
    /// 左の行に右が 1行も無ければ NULL で埋める。
    /// ON 句の評価が失敗したらクエリ全体を Error で落とす。
    let leftJoin (leftSchema: Schema) (left: Op) (rightSchema: Schema) (right: Op) (on: Expr) =
        let rec joinRows rightRows width (cur: Op) =
            Op.pull cur (fun lrow lnext ->
                let onExhausted matched =
                    if matched then
                        joinRows rightRows width lnext
                    else
                        Op.Step(Array.append lrow (Array.create width SqlValue.Null), joinRows rightRows width lnext)

                let onError e _ = Op.Compute(fun () -> err e)
                Op.next (joinLoop leftSchema rightSchema on onExhausted onError rightRows lrow))

        Op.toArray right
        |> Result.map (fun rightRows -> joinRows rightRows rightSchema.Columns.Length left)

    /// 射影。`SELECT` の項目順に値を並べる。
    /// `*` / `t.*` は対象テーブルの全列に展開する。
    /// `groupBy` は「射影の入力が集約結果」のときの GROUP BY 式。
    /// `SELECT age, COUNT(*) ... GROUP BY age` の `age` を
    /// 「集約後の行の先頭」にある値へ読み替えるために使う。
    let project (schema: Schema) (groupBy: Expr list) (src: Op) (items: SelectItem list) =
        let sameRef (c: OutputColumn) =
            let col (qualifier: string option) (name: string) = { Qualifier = qualifier; Name = name }
            col c.Qualifier c.Name

        let aggCallsOf (items: SelectItem list) =
            aggCalls (items |> List.map (fun i -> i.Expression))

        if List.isEmpty items then
            src
        else
            let allExprs =
                items
                |> List.map (fun item ->
                    let exprs =
                        match Schema.expandStar schema item.Expression with
                        | Some(_, columns) -> columns |> List.map (fun c -> sameRef c |> Expr.Column)
                        | None -> [ item.Expression ]

                    item, exprs)
                |> List.collect (fun (_, exprs) -> exprs)

            let foundAggs = aggCallsOf items

            if List.isEmpty foundAggs then
                Op.map (fun row -> allExprs |> List.map (Eval.eval schema row) |> all |> Result.map List.toArray) src
            else
                let offsets = aggOffsets groupBy foundAggs

                Op.map
                    (fun row ->
                        allExprs
                        |> List.map (fun e -> substituteAggs offsets e row >>= Eval.eval Schema.empty row)
                        |> all
                        |> Result.map List.toArray)
                    src
