// ===========================================================================
// ParserExpr.fs（文法・演算子優先順位・CPS の式構文規則・深さ無制限）
// ===========================================================================

module RykDb.ParserExprTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// FROM の実テーブル参照（派生テーブルや FROM なしは None）。
let private fromTable (sel: SelectStatement) =
    match sel.From with
    | Some(TableSource.Named r) -> Some r
    | _ -> None

let private parseUpdateOk (sql: string) : UpdateStatement =
    expectVariant
        "UPDATE"
        (function
        | Statement.Update u -> Some u
        | _ -> None)
        sql

/// WHERE 式の表示文字列（無いときは "(なし)"）。
let private whereTextOf (sql: string) : string =
    match (parseSelectOk sql).Where with
    | Some e -> Expr.toDisplayString e
    | None -> "(なし)"

/// 解析が通ることだけを確かめる（内容は見ない）。
let private parsesOk (sql: string) = ok (parseStatement sql) |> ignore

/// 同じ WHERE を 5 回分けて、その表示がすべて一致すること（継続の再入性）。
let private repeatedWhere (sql: string) =
    [ 1..5 ] |> List.map (fun _ -> whereTextOf sql) |> List.distinct

/// 深さ上限は設けない。深い入力も構文エラーにならず、StackOverflow もしないことを確かめる。
let private assertDeepParse (name: string) (sql: string) =
    match parseStatement sql with
    | Ok _ -> ()
    | Error e -> Assert.Fail(sprintf "%s は解析できるはず: %s" name (DbError.toDisplayString e))

[<Fact>]
let ``Parser: 演算子優先順位`` () =
    // * が + より優先される（(a + (b * c)) = 1 になるはず。a + (b * (c = 1)) ではない）
    match whereOf "SELECT * FROM t WHERE a + b * c = 1" with
    | Expr.Binary(BinaryOp.Eq, _, _) -> ()
    | other -> Assert.Fail(sprintf "想定外の式: %s" (Expr.toDisplayString other))

    // AND が OR より優先される
    match whereOf "SELECT * FROM t WHERE a = 1 OR b = 2 AND c = 3" with
    | Expr.Binary(BinaryOp.Or, _, right) ->
        match right with
        | Expr.Binary(BinaryOp.And, _, _) -> ()
        | other -> Assert.Fail(sprintf "OR の右が AND でない: %s" (Expr.toDisplayString other))
    | other -> Assert.Fail(sprintf "OR ではない: %s" (Expr.toDisplayString other))

[<Fact>]
let ``Parser: BETWEEN は後続の AND を飲み込まない`` () =
    match whereOf "SELECT * FROM t WHERE a BETWEEN 1 AND 10 AND b = 2" with
    | Expr.Binary(BinaryOp.And, Expr.Between _, _) -> ()
    | other -> Assert.Fail(sprintf "BETWEEN が正しく結合されていない: %s" (Expr.toDisplayString other))

[<Fact>]
let ``式: 述語は 1 つの式に 1 回だけ`` () =
    // 比較・述語は非結合。連続して書いたら括弧を要求する。
    expectErrorKind ErrorKind.SyntaxError "= の連続" (parseStatement "SELECT * FROM t WHERE a = b = c")
    expectErrorKind ErrorKind.SyntaxError "IS NULL の後の =" (parseStatement "SELECT * FROM t WHERE a IS NULL = 1")
    expectErrorKind ErrorKind.SyntaxError "BETWEEN の後の =" (parseStatement "SELECT * FROM t WHERE a BETWEEN 1 AND 2 = 3")
    expectErrorKind ErrorKind.SyntaxError "述語の後の *" (parseStatement "SELECT * FROM t WHERE a IS NULL * 2")
    // 括弧で囲めば述語の結果を比較できる
    parsesOk "SELECT * FROM t WHERE (a = b) = c"

[<Fact>]
let ``式: NOT は述語の前置に置けない`` () =
    // 比較の右辺は加算以上なので、前置 NOT（結合強度 30）は入れない
    expectErrorKind ErrorKind.SyntaxError "= の右の NOT" (parseStatement "SELECT * FROM t WHERE a = NOT b")
    expectErrorKind ErrorKind.SyntaxError "NOT の後の比較" (parseStatement "SELECT * FROM t WHERE a NOT = 1")

[<Fact>]
let ``式: 不正な修飾子はその場でエラー`` () =
    // 識別子以外の後ろの `.` は修飾子として認めない
    expectErrorKind ErrorKind.SyntaxError "式の後ろの ." (parseStatement "SELECT (a + b) . c FROM t")
    // 正しい修飾子は従来どおり
    parsesOk "SELECT a.b, a + b.c FROM t WHERE a.b = 1"

[<Fact>]
let ``Parser: IS NULL / IN / LIKE / NOT`` () =
    match whereOf "SELECT * FROM t WHERE a IS NULL" with
    | Expr.IsNull(_, false) -> ()
    | other -> failActual "IS NULL ではない" other

    match whereOf "SELECT * FROM t WHERE a IS NOT NULL" with
    | Expr.IsNull(_, true) -> ()
    | other -> failActual "IS NOT NULL ではない" other

    match whereOf "SELECT * FROM t WHERE a IN (1, 2, 3)" with
    | Expr.InList(_, xs, false) -> Assert.Equal(3, xs.Length)
    | other -> failActual "IN ではない" other

    match whereOf "SELECT * FROM t WHERE a NOT IN (1)" with
    | Expr.InList(_, _, true) -> ()
    | other -> failActual "NOT IN ではない" other

    match whereOf "SELECT * FROM t WHERE a LIKE 'x%'" with
    | Expr.Like(_, _, false, None) -> ()
    | other -> failActual "LIKE ではない" other

    match whereOf "SELECT * FROM t WHERE NOT a = 1" with
    | Expr.Unary(UnaryOp.Not, _) -> ()
    | other -> failActual "NOT ではない" other

[<Fact>]
let ``Parser: IS TRUE / FALSE / UNKNOWN と LIKE ESCAPE`` () =
    match whereOf "SELECT * FROM t WHERE a IS TRUE" with
    | Expr.IsTruth(_, TruthValue.True, false) -> ()
    | other -> failActual "IS TRUE ではない" other

    match whereOf "SELECT * FROM t WHERE a IS NOT FALSE" with
    | Expr.IsTruth(_, TruthValue.False, true) -> ()
    | other -> failActual "IS NOT FALSE ではない" other

    match whereOf "SELECT * FROM t WHERE a IS UNKNOWN" with
    | Expr.IsTruth(_, TruthValue.Unknown, false) -> ()
    | other -> failActual "IS UNKNOWN ではない" other

    match whereOf "SELECT * FROM t WHERE a IS NOT UNKNOWN" with
    | Expr.IsTruth(_, TruthValue.Unknown, true) -> ()
    | other -> failActual "IS NOT UNKNOWN ではない" other

    match whereOf "SELECT * FROM t WHERE a LIKE 'x!%' ESCAPE '!'" with
    | Expr.Like(_, _, false, Some _) -> ()
    | other -> failActual "LIKE ESCAPE ではない" other

    match whereOf "SELECT * FROM t WHERE a NOT LIKE 'x!_' ESCAPE '!'" with
    | Expr.Like(_, _, true, Some _) -> ()
    | other -> failActual "NOT LIKE ESCAPE ではない" other

[<Fact>]
let ``Parser: CASE と関数`` () =
    match (parseSelectOk "SELECT CASE WHEN a = 1 THEN 'one' ELSE 'other' END FROM t").Items.[0].Expression with
    | Expr.Case(_, Some _) -> ()
    | other -> failActual "CASE ではない" other

    let sel = parseSelectOk "SELECT UPPER(name), LENGTH(name), COALESCE(a, b, 0) FROM t"
    Assert.Equal(3, sel.Items.Length)

    let sel2 =
        parseSelectOk "SELECT COUNT(DISTINCT a), SUM(b), AVG(c), MIN(d), MAX(e) FROM t"

    Assert.True(sel2.Items.[0].Expression |> Expr.isAggregate)
    Assert.True(sel2.Items.[1].Expression |> Expr.isAggregate)

[<Fact>]
let ``Parser: CAST と DEFAULT`` () =
    match (parseSelectOk "SELECT CAST(a AS INTEGER) FROM t").Items with
    | [ {
            Expression = Expr.Cast(_, DataType.Integer)
        } ] -> ()
    | other -> failActual "CAST ではない" other

    match parseOneOk "INSERT INTO t DEFAULT VALUES" with
    | Statement.Insert ins ->
        match ins.Source with
        | InsertSource.Values [ [] ] -> ()
        | other -> failActual "DEFAULT VALUES ではない" other
    | other -> failActual "INSERT ではない" other

    match parseOneOk "INSERT INTO t VALUES (1, DEFAULT)" with
    | Statement.Insert ins ->
        match ins.Source with
        | InsertSource.Values [ [ _; Expr.Default ] ] -> ()
        | other -> failActual "DEFAULT ではない" other
    | other -> failActual "INSERT ではない" other

[<Fact>]
let ``Parser: 集約関数の DISTINCT`` () =
    let item (sql: string) =
        match (parseSelectOk sql).Items with
        | [ it ] -> it.Expression
        | other -> failwithf "項目数が違う: %A" other

    match item "SELECT SUM(DISTINCT a) FROM t" with
    | Expr.AggCall(AggFunc.Sum, true, _) -> ()
    | other -> failActual "SUM DISTINCT ではない" other

    match item "SELECT AVG(DISTINCT a) FROM t" with
    | Expr.AggCall(AggFunc.Avg, true, _) -> ()
    | other -> failActual "AVG DISTINCT ではない" other

    match item "SELECT MIN(DISTINCT a) FROM t" with
    | Expr.AggCall(AggFunc.Min, true, _) -> ()
    | other -> failActual "MIN DISTINCT ではない" other

    match item "SELECT MAX(DISTINCT a) FROM t" with
    | Expr.AggCall(AggFunc.Max, true, _) -> ()
    | other -> failActual "MAX DISTINCT ではない" other

[<Fact>]
let ``Parser: リテラルとコメント`` () =
    // ブロックコメントと -- コメント
    match whereOf "SELECT * FROM t /* comment */ WHERE a = 1 -- trailing" with
    | Expr.Binary(BinaryOp.Eq, _, _) -> ()
    | other -> Assert.Fail(sprintf "コメント処理が誤り: %s" (Expr.toDisplayString other))

    // '' によるエスケープ
    match whereOf "SELECT * FROM t WHERE a = 'it''s'" with
    | Expr.Binary(BinaryOp.Eq, _, Expr.Literal(Literal.String s)) -> Assert.Equal("it's", s)
    | other -> Assert.Fail(sprintf "文字列エスケープが誤り: %s" (Expr.toDisplayString other))

    // バイナリリテラル
    match whereOf "SELECT * FROM t WHERE a = X'00FF'" with
    | Expr.Binary(BinaryOp.Eq, _, Expr.Literal(Literal.Blob b)) -> Assert.Equal<byte[]>([| 0uy; 255uy |], b)
    | other -> Assert.Fail(sprintf "バイナリリテラルが誤り: %s" (Expr.toDisplayString other))

    // 負数
    match whereOf "SELECT * FROM t WHERE a = -1" with
    | Expr.Binary(BinaryOp.Eq, _, Expr.Unary(UnaryOp.Neg, _)) -> ()
    | other -> Assert.Fail(sprintf "負数が誤り: %s" (Expr.toDisplayString other))

[<Fact>]
let ``Parser: JOIN`` () =
    let sel =
        parseSelectOk "SELECT * FROM a INNER JOIN b ON a.id = b.id LEFT JOIN c ON c.id = a.id"

    Assert.Equal(2, sel.Joins.Length)
    Assert.Equal<JoinKind>(JoinKind.Inner, sel.Joins.[0].Kind)
    Assert.Equal<JoinKind>(JoinKind.Left, sel.Joins.[1].Kind)

[<Fact>]
let ``Parser: LEFT OUTER JOIN`` () =
    match parseOneOk "SELECT * FROM t LEFT OUTER JOIN u ON t.id = u.id" with
    | Statement.Select sel ->
        match sel.Joins with
        | [ j ] ->
            match j.Kind with
            | JoinKind.Left -> ()
            | _ -> Assert.Fail("LEFT JOIN ではない")
        | other -> failActual "JOIN の数が違う" other
    | other -> failActual "SELECT ではない" other

[<Fact>]
let ``Parser: SELECT 各種`` () =
    let sel = parseSelectOk "SELECT * FROM users"

    Assert.Equal("users", fromTable sel |> Option.map (fun t -> t.Name) |> Option.defaultValue "")
    Assert.Equal(1, sel.Items.Length)
    Assert.Empty sel.Joins

    let sel2 =
        parseSelectOk
            "SELECT DISTINCT a, b AS bb, COUNT(*) FROM t WHERE a > 1 GROUP BY a HAVING COUNT(*) > 2 ORDER BY a DESC LIMIT 10 OFFSET 5"

    Assert.True sel2.Distinct
    Assert.Equal(3, sel2.Items.Length)
    Assert.Equal(1, sel2.GroupBy.Length)
    Assert.True sel2.Having.IsSome
    Assert.Equal(1, sel2.OrderBy.Length)
    Assert.Equal<SortDirection>(SortDirection.Descending, sel2.OrderBy.[0].Direction)
    Assert.True sel2.Limit.IsSome
    Assert.True sel2.Offset.IsSome

[<Fact>]
let ``Parser: サブクエリ（スカラー / IN / EXISTS）`` () =
    parseOneOk "SELECT (SELECT MAX(v) FROM t) FROM t" |> ignore
    parseOneOk "SELECT id FROM t WHERE id IN (SELECT id FROM t)" |> ignore
    parseOneOk "SELECT id FROM t WHERE id NOT IN (SELECT id FROM t)" |> ignore
    parseOneOk "SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t)" |> ignore
    parseOneOk "SELECT id FROM t WHERE NOT EXISTS (SELECT 1 FROM t)" |> ignore
    parseOneOk "SELECT id FROM t WHERE id = (SELECT MAX(id) FROM t)" |> ignore

    parseOneOk "SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE v > (SELECT MIN(v) FROM t))"
    |> ignore

    // 派生テーブル（FROM のサブクエリ）
    let derived = parseSelectOk "SELECT id FROM (SELECT id FROM t) x"

    match derived.From with
    | Some(TableSource.Derived(inner, alias)) ->
        Assert.Equal("x", alias)
        Assert.Equal<int>(1, inner.Items.Length)
    | other -> failActual "派生テーブルではない" other

    parseOneOk "SELECT id FROM (SELECT id FROM t) AS x JOIN u ON u.id = x.id"
    |> ignore

    parseOneOk "SELECT id FROM t JOIN (SELECT id FROM u) AS y ON y.id = t.id"
    |> ignore

    // 派生テーブルには別名が必要
    expectErrorKind ErrorKind.SyntaxError "派生テーブルの別名" (parseStatement "SELECT id FROM (SELECT id FROM t)")

    // 括弧の中には SELECT だけを書ける
    expectErrorKind ErrorKind.SyntaxError "FROM の括弧" (parseStatement "SELECT id FROM (t)")

    // EXISTS の後ろには括弧付きのサブクエリが必要
    expectErrorKind ErrorKind.SyntaxError "EXISTS の引数" (parseStatement "SELECT id FROM t WHERE EXISTS id")

    // サブクエリでも FROM は省略できる（`EXISTS (SELECT 1)` は常に真）
    parseOneOk "SELECT id FROM t WHERE EXISTS (SELECT 1)" |> ignore

[<Fact>]
let ``Parser: FROM を省略した SELECT`` () =
    // FROM なし: From = None / Joins は空
    let sel = parseSelectOk "SELECT 1"
    Assert.True sel.From.IsNone
    Assert.Empty sel.Joins
    Assert.Empty sel.GroupBy
    Assert.True sel.Where.IsNone

    // 式・別名・WHERE / ORDER BY / LIMIT も書ける
    let sel2 =
        parseSelectOk "SELECT 1 + 2 AS x, 'a' WHERE 1 = 1 ORDER BY x LIMIT 1 OFFSET 0"

    Assert.True sel2.From.IsNone
    Assert.Equal<int>(2, sel2.Items.Length)
    Assert.True sel2.Where.IsSome
    Assert.Equal<int>(1, sel2.OrderBy.Length)

    // サブクエリの AST でも From = None になる
    let exists =
        match parseSelectOk "SELECT id FROM t WHERE EXISTS (SELECT 1)" with
        | {
              Where = Some(Expr.Exists { From = None })
          } -> true
        | _ -> false

    Assert.True exists

    // JOIN には FROM が必要
    expectErrorKind ErrorKind.SyntaxError "FROM なしの JOIN" (parseStatement "SELECT 1 JOIN t ON 1 = 1")
    expectErrorKind ErrorKind.SyntaxError "FROM なしの INNER JOIN" (parseStatement "SELECT 1 INNER JOIN t ON 1 = 1")
    expectErrorKind ErrorKind.SyntaxError "FROM なしの LEFT JOIN" (parseStatement "SELECT 1 LEFT JOIN t ON 1 = 1")

[<Fact>]
let ``Parser: サブクエリの AST 形`` () =
    match parseOneOk "SELECT id FROM t WHERE id IN (SELECT id FROM t)" with
    | Statement.Select {
                           Where = Some(Expr.InSubquery(Expr.Name "id", _, false))
                       } -> ()
    | _ -> Assert.Fail "InSubquery ではありません"

    match parseOneOk "SELECT (SELECT MAX(v) FROM t) FROM t" with
    | Statement.Select {
                           Items = [ { Expression = Expr.Subquery _ } ]
                       } -> ()
    | _ -> Assert.Fail "スカラーサブクエリではありません"

    match parseOneOk "SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t)" with
    | Statement.Select { Where = Some(Expr.Exists _) } -> ()
    | _ -> Assert.Fail "EXISTS ではありません"

[<Theory>]
// CPS 化した各経路をなぞる。継続の戻り値の型が多様（Expr / (Expr*Expr) list /
// Expr option）なので取り違えの検証も兼ねる。期待値は Expr.toDisplayString の出力。
[<InlineData("1 + 2 * 3", "(1 + (2 * 3))")>]
[<InlineData("NOT NOT NOT a", "(NOT (NOT (NOT a)))")>]
[<InlineData("a IS NULL AND b IS NOT NULL", "((a IS NULL) AND (b IS NOT NULL))")>]
[<InlineData("a BETWEEN 1 AND 10", "(a BETWEEN 1 AND 10)")>]
[<InlineData("a NOT BETWEEN 1 AND 10", "(a NOT BETWEEN 1 AND 10)")>]
[<InlineData("a IN (1, 2, 3)", "(a IN (1, 2, 3))")>]
[<InlineData("a NOT IN (1, 2, 3)", "(a NOT IN (1, 2, 3))")>]
[<InlineData("a IN (1)", "(a IN (1))")>]
[<InlineData("a LIKE 'x%'", "(a LIKE 'x%')")>]
[<InlineData("a NOT LIKE 'x%'", "(a NOT LIKE 'x%')")>]
[<InlineData("CASE WHEN a THEN 1 WHEN b THEN 2 ELSE 3 END", "(CASE WHEN a THEN 1 WHEN b THEN 2 ELSE 3 END)")>]
[<InlineData("CASE WHEN a THEN 1 END", "(CASE WHEN a THEN 1 END)")>]
[<InlineData("COUNT(*)", "COUNT(*)")>]
[<InlineData("COUNT(DISTINCT a)", "COUNT(DISTINCT a)")>]
[<InlineData("f(a, b + c, -d)", "F(a, (b + c), (- d))")>]
[<InlineData("t.c = 1", "(t.c = 1)")>]
[<InlineData("t.*", "t.*")>]
[<InlineData("(a + b) * (c - d) / (e % f)", "(((a + b) * (c - d)) / (e % f))")>]
[<InlineData("a || b || c", "((a || b) || c)")>]
[<InlineData("a OR b OR c", "((a OR b) OR c)")>]
[<InlineData("a AND b AND c", "((a AND b) AND c)")>]
[<InlineData("-1 + +2", "((- 1) + (+ 2))")>]
let ``CPS: 式構文規則の各経路が解析できる`` (expr: string) (expected: string) =
    Assert.Equal<string>(expected, whereTextOf ("SELECT * FROM t WHERE " + expr))

[<Fact>]
let ``CPS: 継続は 1 回だけ呼ばれ、呼び出しごとに影響されない`` () =
    // 継続が 0 回・2 回以上だと式が壊れる。同じ式を繰り返し解析して
    // いつでも同一の AST になることを確かめる。
    let sql = "SELECT a FROM t WHERE a BETWEEN 1 AND 9 AND b IN (1, 2) OR c IS NULL"

    let rendered = repeatedWhere sql
    Assert.Single<string> rendered |> ignore
    Assert.Equal<string>("(((a BETWEEN 1 AND 9) AND (b IN (1, 2))) OR (c IS NULL))", rendered.[0])

[<Fact>]
let ``CPS: 継続は 1 回だけ呼ばれ、同じ式が連続しても影響されない`` () =
    // 同じ式を 2 回続けて解析しても AST が変わらないこと。
    let sql = "SELECT a FROM t WHERE a BETWEEN 1 AND 9 AND b IN (1, 2) OR c IS NULL"

    let rendered = repeatedWhere sql

    Assert.Single<string> rendered |> ignore
    Assert.Equal<string>("(((a BETWEEN 1 AND 9) AND (b IN (1, 2))) OR (c IS NULL))", rendered.[0])

[<Theory>]
// hoist した比較演算子の分岐を全部なぞる（判定を Option にして「関数を作る」束縛を避けていること）。
[<InlineData("a = 1", "(a = 1)")>]
[<InlineData("a <> 1", "(a <> 1)")>]
[<InlineData("a != 1", "(a <> 1)")>]
[<InlineData("a <= 1", "(a <= 1)")>]
[<InlineData("a >= 1", "(a >= 1)")>]
[<InlineData("a < 1", "(a < 1)")>]
[<InlineData("a > 1", "(a > 1)")>]
[<InlineData("a IS NULL", "(a IS NULL)")>]
[<InlineData("a IS NOT NULL", "(a IS NOT NULL)")>]
[<InlineData("a + 1 = 2", "((a + 1) = 2)")>]
[<InlineData("f(a) = 1", "(F(a) = 1)")>]
[<InlineData("CASE WHEN a THEN 1 END = 2", "((CASE WHEN a THEN 1 END) = 2)")>]
let ``末尾再帰: 比較演算子の判定が hoist 後も正しい`` (expr: string) (expected: string) =
    Assert.Equal<string>(expected, whereTextOf ("SELECT * FROM t WHERE " + expr))

[<Fact>]
let ``末尾再帰: 1000000段の深い括弧の入れ子もスタックを食わない`` () =
    // MaxExpressionDepth = 1000000 を仮に置いても届かない深さ。
    // 括弧は AST に残らないので、中身は定数 1 になる。
    let depth = 1_000_000
    let nest = System.String('(', depth) + "1" + System.String(')', depth)
    Assert.Equal<string>("1", whereTextOf ("SELECT * FROM t WHERE " + nest))

[<Fact>]
let ``末尾再帰: 長い文字列リテラルを字句解析できる`` () =
    let len = 100_000
    let body = System.String('a', len)
    let sql = sprintf "SELECT * FROM t WHERE a = '%s'" body

    match whereOf sql with
    | Expr.Binary(BinaryOp.Eq, _, Expr.Literal(Literal.String s)) -> Assert.Equal(len, s.Length)
    | other -> failActual "想定外の式" other

[<Fact>]
let ``末尾再帰: 長いバイナリリテラルを字句解析できる`` () =
    let len = 50_000
    let hex = System.String('A', len)
    let sql = sprintf "SELECT * FROM t WHERE a = X'%s'" hex
    parsesOk sql

[<Fact>]
let ``末尾再帰: 長いコメントを読み飛ばせる`` () =
    let len = 100_000
    let body = System.String('x', len)
    let sql = sprintf "SELECT * FROM t /* %s */ WHERE a = 1" body
    parsesOk sql

[<Fact>]
let ``末尾再帰: 大量のトークンを解析できる`` () =
    // 50000個の要素を持つ IN 句
    let n = 50_000
    let body = Array.init n (fun i -> string (i % 1000)) |> String.concat ","
    let sql = sprintf "SELECT * FROM t WHERE a IN (%s)" body
    parsesOk sql

[<Fact>]
let ``末尾再帰: 5000段の深い入れ子も解析できる`` () =
    let depth = 5_000
    let deep = System.String('(', depth) + "1" + System.String(')', depth)
    parsesOk ("SELECT * FROM t WHERE " + deep)

[<Fact>]
let ``末尾再帰: 5000段の深いサブクエリの入れ子も解析できる`` () =
    // サブクエリは式のパーサと SELECT のパーサが相互再帰する。
    // 継続がヒープに積まれるので、深さに比例してスタックは積み上がらない。
    let nested depth =
        [ 1..depth ]
        |> List.fold (fun e _ -> sprintf "(SELECT %s FROM t WHERE id = 1)" e) "v"

    parsesOk (sprintf "SELECT %s AS x FROM t" (nested 5_000))

[<Fact>]
let ``末尾再帰: 200000個の NOT が連なってもスタックを食わない`` () =
    let depth = 200_000
    assertDeepParse "200000個の NOT" ("SELECT * FROM t WHERE " + String.replicate depth "NOT " + "a")

[<Fact>]
let ``末尾再帰: 200000個の単項マイナスが連なってもスタックを食わない`` () =
    let depth = 200_000
    assertDeepParse "200000個の単項 '-'" ("SELECT * FROM t WHERE " + String.replicate depth "- " + "a")

[<Fact>]
let ``末尾再帰: 50000個の OR が連なってもスタックを食わない`` () =
    let depth = 50_000
    assertDeepParse "50000個の OR" ("SELECT * FROM t WHERE " + String.replicate depth "a OR " + "a")

[<Fact>]
let ``末尾再帰: BETWEEN を大量並べてもスタックを食わない`` () =
    let depth = 50_000

    assertDeepParse "大量の BETWEEN" ("SELECT * FROM t WHERE " + String.replicate depth "a BETWEEN 1 AND 2 OR " + "a")

[<Fact>]
let ``末尾再帰: 大量の JOIN 句がパースできる`` () =
    // JOIN の走査は継続の中で次の JOIN へ進む。
    // 継続呼び出しが末尾になっていないと JOIN の数だけフレームが積まれる。
    let n = 10_000

    let joins =
        Array.init n (fun i -> sprintf "JOIN t%d ON x = y" (i + 1)) |> String.concat " "

    let sel = parseSelectOk (sprintf "SELECT * FROM t0 %s" joins)
    Assert.Equal(n, sel.Joins.Length)

[<Fact>]
let ``末尾再帰: 大量の SET 句がパースできる`` () =
    let n = 20_000
    let body = Array.init n (fun i -> sprintf "c%d = %d" i i) |> String.concat ", "
    let upd = parseUpdateOk (sprintf "UPDATE t SET %s" body)
    Assert.Equal(n, upd.Assignments.Length)

[<Fact>]
let ``末尾再帰: 大量の SELECT 項目・GROUP BY・ORDER BY がパースできる`` () =
    // サブクエリ対応で SELECT の各句を式と同じ CPS グループへ入れた。
    // 句のリスト（項目 / GROUP BY / ORDER BY）の走査も継続の中で次の項目へ進むので、
    // 継続呼び出しが末尾になっていないと項目数だけフレームが積まれる。
    let n = 20_000
    let items = Array.init n (fun i -> sprintf "c%d" i) |> String.concat ", "
    Assert.Equal(n, (parseSelectOk (sprintf "SELECT %s FROM t" items)).Items.Length)

    let sel =
        parseSelectOk (sprintf "SELECT c0 FROM t GROUP BY %s ORDER BY %s" items items)

    Assert.Equal(n, sel.GroupBy.Length)
    Assert.Equal(n, sel.OrderBy.Length)

[<Fact>]
let ``末尾再帰: 20000個の加算の鎖もスタックを食わない`` () =
    let depth = 20_000
    assertDeepParse "20000個の +" ("SELECT * FROM t WHERE a" + String.replicate depth " + 1")

[<Fact>]
let ``末尾再帰: 50000個の AND の鎖もスタックを食わない`` () =
    let depth = 50_000
    assertDeepParse "50000個の AND" ("SELECT * FROM t WHERE " + String.replicate depth "a AND " + "a")

[<Fact>]
let ``末尾再帰: 20000個の単項プラスと乗算の鎖もスタックを食わない`` () =
    assertDeepParse "20000個の単項 '+'" ("SELECT * FROM t WHERE " + String.replicate 20_000 "+ " + "a")
    assertDeepParse "20000個の *" ("SELECT * FROM t WHERE a" + String.replicate 20_000 " * a")

[<Fact>]
let ``末尾再帰: 演算子チェーンを繰り返し解析できる`` () =
    let sql = "SELECT * FROM t WHERE " + String.replicate 100 "a OR " + "b"

    for _ in 1..3 do
        Assert.True((parseSelectOk sql).Where.IsSome)
