// ===========================================================================
// Parser.fs（文法・演算子優先順位・深さ制限・CPS 化した式構文規則）
// ===========================================================================

module RykDb.ParserTests

open System
open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// SQL を 1 文解析し、失敗したらテストを落とす。
let private parseOneOk (sql: string) =
    match Parser.parseStatement sql with
    | Ok s -> s
    | Error e ->
        Assert.Fail(sprintf "解析に失敗: %s\nSQL: %s" (DbError.toDisplayString e) sql)
        Unchecked.defaultof<Statement>

/// SQL を 1 文解析し、variant から値を抜き出せなければテストを落とす。
let private expectVariant (tag: string) (pick: Statement -> 'T option) (sql: string) : 'T =
    match pick (parseOneOk sql) with
    | Some v -> v
    | None ->
        Assert.Fail(tag + " ではない: " + sql)
        Unchecked.defaultof<'T>

let private parseSelectOk (sql: string) : SelectStatement =
    expectVariant
        "SELECT"
        (function
        | Statement.Select s -> Some s
        | _ -> None)
        sql

let private parseInsertOk (sql: string) : InsertStatement =
    expectVariant
        "INSERT"
        (function
        | Statement.Insert i -> Some i
        | _ -> None)
        sql

let private parseUpdateOk (sql: string) : UpdateStatement =
    expectVariant
        "UPDATE"
        (function
        | Statement.Update u -> Some u
        | _ -> None)
        sql

let private parseDeleteOk (sql: string) : DeleteStatement =
    expectVariant
        "DELETE"
        (function
        | Statement.Delete d -> Some d
        | _ -> None)
        sql

let private parseCreateTableOk (sql: string) : CreateTable =
    expectVariant
        "CREATE TABLE"
        (function
        | Statement.CreateTable c -> Some c
        | _ -> None)
        sql

let private parseCreateIndexOk (sql: string) : CreateIndex =
    expectVariant
        "CREATE INDEX"
        (function
        | Statement.CreateIndex c -> Some c
        | _ -> None)
        sql

let private parseDropTableOk (sql: string) : string * bool =
    expectVariant
        "DROP TABLE"
        (function
        | Statement.DropTable(n, ifEx) -> Some(n, ifEx)
        | _ -> None)
        sql

/// SELECT 文の WHERE 式を取り出す（無い・SELECT でないならテストを落とす）。
let private whereOf (sql: string) : Expr =
    match (parseSelectOk sql).Where with
    | Some e -> e
    | None ->
        Assert.Fail("WHERE 句がありません: " + sql)
        Unchecked.defaultof<Expr>

/// WHERE 式の表示文字列（無いときは "(なし)"）。
let private whereTextOf (sql: string) : string =
    match (parseSelectOk sql).Where with
    | Some e -> Expr.toDisplayString e
    | None -> "(なし)"

/// 解析が通ることだけを確かめる（内容は見ない）。
let private parsesOk (sql: string) =
    ok (Parser.parseStatement sql) |> ignore

/// 深さ超過が StackOverflow ではなく SyntaxError として返ること。
let private expectDepthError (label: string) (r: Result<Statement, DbError>) =
    match r with
    | Error e ->
        Assert.Equal<ErrorKind>(ErrorKind.SyntaxError, e.Kind)
        Assert.Contains("深すぎます", e.Message)
    | Ok _ -> Assert.Fail(label + " は深すぎるのでエラーになるはず")

/// 同じ WHERE を 5 回分けて、その表示がすべて一致すること（継続の再入性）。
let private repeatedWhere (sql: string) =
    [ 1..5 ] |> List.map (fun _ -> whereTextOf sql) |> List.distinct

// ---------------------------------------------------------------------------
// パーサ: 式（parseExpr）
// ---------------------------------------------------------------------------

[<Fact>]
let ``パーサ: 演算子優先順位`` () =
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
let ``パーサ: BETWEEN は後続の AND を飲み込まない`` () =
    match whereOf "SELECT * FROM t WHERE a BETWEEN 1 AND 10 AND b = 2" with
    | Expr.Binary(BinaryOp.And, Expr.Between _, _) -> ()
    | other -> Assert.Fail(sprintf "BETWEEN が正しく結合されていない: %s" (Expr.toDisplayString other))

[<Fact>]
let ``パーサ: IS NULL / IN / LIKE / NOT`` () =
    match whereOf "SELECT * FROM t WHERE a IS NULL" with
    | Expr.IsNull(_, false) -> ()
    | other -> Assert.Fail("IS NULL ではない")

    match whereOf "SELECT * FROM t WHERE a IS NOT NULL" with
    | Expr.IsNull(_, true) -> ()
    | other -> Assert.Fail("IS NOT NULL ではない")

    match whereOf "SELECT * FROM t WHERE a IN (1, 2, 3)" with
    | Expr.InList(_, xs, false) -> Assert.Equal(3, xs.Length)
    | other -> Assert.Fail("IN ではない")

    match whereOf "SELECT * FROM t WHERE a NOT IN (1)" with
    | Expr.InList(_, _, true) -> ()
    | other -> Assert.Fail("NOT IN ではない")

    match whereOf "SELECT * FROM t WHERE a LIKE 'x%'" with
    | Expr.Like(_, _, false) -> ()
    | other -> Assert.Fail("LIKE ではない")

    match whereOf "SELECT * FROM t WHERE NOT a = 1" with
    | Expr.Unary(UnaryOp.Not, _) -> ()
    | other -> Assert.Fail("NOT ではない")

[<Fact>]
let ``パーサ: CASE と関数`` () =
    match (parseSelectOk "SELECT CASE WHEN a = 1 THEN 'one' ELSE 'other' END FROM t").Items.[0].Expression with
    | Expr.Case(_, Some _) -> ()
    | other -> Assert.Fail(sprintf "CASE ではない: %A" other)

    let sel = parseSelectOk "SELECT UPPER(name), LENGTH(name), COALESCE(a, b, 0) FROM t"
    Assert.Equal(3, sel.Items.Length)

    let sel2 =
        parseSelectOk "SELECT COUNT(DISTINCT a), SUM(b), AVG(c), MIN(d), MAX(e) FROM t"

    Assert.True(sel2.Items.[0].Expression |> Expr.isAggregate)
    Assert.True(sel2.Items.[1].Expression |> Expr.isAggregate)

[<Fact>]
let ``パーサ: リテラルとコメント`` () =
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

// ---------------------------------------------------------------------------
// 末尾再帰の検証（深い入力でスタックを食わないこと）
// ---------------------------------------------------------------------------

/// 深さカウンタで構文エラーになることを確認する（StackOverflow ではない）。
let private assertDepthExceeded (name: string) (sql: string) =
    expectDepthError name (Parser.parseStatement sql)

[<Fact>]
let ``末尾再帰: 200 万個の NOT が連なってもスタックを食わない`` () =
    let depth = 2_000_000
    assertDepthExceeded "200 万個の NOT" ("SELECT * FROM t WHERE " + String.replicate depth "NOT " + "a")

[<Fact>]
let ``末尾再帰: 200 万個の単項マイナスが連なってもスタックを食わない`` () =
    let depth = 2_000_000
    assertDepthExceeded "200 万個の単項 '-'" ("SELECT * FROM t WHERE " + String.replicate depth "- " + "a")

[<Fact>]
let ``末尾再帰: 50 万個の OR が連なってもスタックを食わない`` () =
    let depth = 500_000
    assertDepthExceeded "50 万個の OR" ("SELECT * FROM t WHERE " + String.replicate depth "a OR ")

[<Fact>]
let ``末尾再帰: BETWEEN を大量並べてもスタックを食わない`` () =
    // parseBetweenAndC は継続を hoist した関数。非末尾再帰に戻っても
    // StackOverflow ではなく深さエラーになる。
    let depth = 500_000
    assertDepthExceeded "大量の BETWEEN" ("SELECT * FROM t WHERE " + String.replicate depth "a BETWEEN 1 AND 2 OR ")

[<Fact>]
let ``末尾再帰: 加算の鎖も深さエラーになる`` () =
    let depth = 200_000
    assertDepthExceeded "20 万個の +" ("SELECT * FROM t WHERE a" + String.replicate depth " + 1")

[<Fact>]
let ``末尾再帰: AND の鎖も深さエラーになる`` () =
    let depth = 500_000
    assertDepthExceeded "50 万個の AND" ("SELECT * FROM t WHERE " + String.replicate depth "a AND " + "a")

[<Fact>]
let ``末尾再帰: 単項プラスと乗算の鎖も深さエラーになる`` () =
    assertDepthExceeded "20 万個の単項 '+'" ("SELECT * FROM t WHERE " + String.replicate 200_000 "+ " + "a")

    assertDepthExceeded "20 万個の *" ("SELECT * FROM t WHERE a" + String.replicate 200_000 " * a")

[<Fact>]
let ``末尾再帰: 上限内の演算子チェーンは受理される`` () =
    // 深さカウンタの加減を忘れると 2 回目で境界を超えてしまう。
    let sql = "SELECT * FROM t WHERE " + String.replicate 100 "a OR " + "b"

    for _ in 1..3 do
        Assert.True((parseSelectOk sql).Where.IsSome)

// ---------------------------------------------------------------------------
// hoist / CPS 化した式構文規則の検証
// ---------------------------------------------------------------------------

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
let ``末尾再帰: 長い文字列リテラルを字句解析できる`` () =
    let len = 1_000_000
    let body = System.String('a', len)
    let sql = sprintf "SELECT * FROM t WHERE a = '%s'" body

    match whereOf sql with
    | Expr.Binary(BinaryOp.Eq, _, Expr.Literal(Literal.String s)) -> Assert.Equal(len, s.Length)
    | other -> Assert.Fail(sprintf "想定外の式: %A" other)

[<Fact>]
let ``末尾再帰: 長いバイナリリテラルを字句解析できる`` () =
    let len = 500_000
    let hex = System.String('A', len)
    let sql = sprintf "SELECT * FROM t WHERE a = X'%s'" hex

    parsesOk sql

[<Fact>]
let ``末尾再帰: 長いコメントを読み飛ばせる`` () =
    let len = 1_000_000
    let body = System.String('x', len)
    let sql = sprintf "SELECT * FROM t /* %s */ WHERE a = 1" body

    parsesOk sql

[<Fact>]
let ``末尾再帰: 大量のトークンを解析できる`` () =
    // 50 万個の要素を持つ IN 句
    let n = 500_000
    let body = Array.init n (fun i -> string (i % 1000)) |> String.concat ","
    let sql = sprintf "SELECT * FROM t WHERE a IN (%s)" body

    parsesOk sql

[<Fact>]
let ``末尾再帰: 深い入れ子はエラーになる（StackOverflow ではない）`` () =
    let depth = 50_000
    let deep = System.String('(', depth) + "1" + System.String(')', depth)
    let sql = "SELECT * FROM t WHERE " + deep

    expectDepthError "深すぎる入れ子" (Parser.parseStatement sql)

    // 上限内なら通ること
    parsesOk ("SELECT * FROM t WHERE " + System.String('(', 50) + "1" + System.String(')', 50))

[<Fact>]
let ``末尾再帰: CPS にした式構文規則は深さ境界で正確に動く`` () =
    // 継続を呼ぶたびに深さが 1 段戻るので「ちょうど上限」で受理・超過で SyntaxError。
    // WHERE の式が 1 段使うので括弧の入れ子は MaxExpressionDepth - 1 段まで。
    let maxNest = Parser.MaxExpressionDepth - 1

    let nest depth =
        System.String('(', depth) + "1" + System.String(')', depth)

    // 括弧は AST に残らない（( 式 ) は 式 になる）ので中身は定数 1 のはず
    Assert.Equal<string>("1", whereTextOf ("SELECT * FROM t WHERE " + nest maxNest))

    expectDepthError "上限 1 段超過" (Parser.parseStatement ("SELECT * FROM t WHERE " + nest (maxNest + 1)))

    // 先に成功しても深さの残骸が残らないこと（exitExpr を忘れると 2 回目で境界を超える）
    for _ in 1..3 do
        parsesOk ("SELECT * FROM t WHERE " + nest maxNest)

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

    Assert.Single<string>(rendered) |> ignore
    Assert.Equal<string>("(((a BETWEEN 1 AND 9) AND (b IN (1, 2))) OR (c IS NULL))", rendered.[0])

[<Fact>]
let ``CPS: 継続は 1 回だけ呼ばれ、同じ式が連続しても影響されない`` () =
    // 同じ式を 2 回続けて解析しても AST が変わらないこと。
    let sql = "SELECT a FROM t WHERE a BETWEEN 1 AND 9 AND b IN (1, 2) OR c IS NULL"

    let rendered = repeatedWhere sql

    Assert.Single<string>(rendered) |> ignore
    Assert.Equal<string>("(((a BETWEEN 1 AND 9) AND (b IN (1, 2))) OR (c IS NULL))", rendered.[0])

// ---------------------------------------------------------------------------
// パーサ: JOIN（parseJoins）
// ---------------------------------------------------------------------------

[<Fact>]
let ``パーサ: JOIN`` () =
    let sel =
        parseSelectOk "SELECT * FROM a INNER JOIN b ON a.id = b.id LEFT JOIN c ON c.id = a.id"

    Assert.Equal(2, sel.Joins.Length)
    Assert.Equal<JoinKind>(JoinKind.Inner, sel.Joins.[0].Kind)
    Assert.Equal<JoinKind>(JoinKind.Left, sel.Joins.[1].Kind)

[<Fact>]
let ``末尾再帰: 大量の JOIN 句がパースできる`` () =
    // parseJoins は `parseTableRef >>= … expectKeyword >>= … parseExprC >>= fun … ->
    // parseJoins …` で JOIN の数だけフレームが積まれていた（→ StackOverflow / exit 134）。
    // `match` で受けて末尾再帰にしてある。テーブル参照は名前を読むだけで
    // 実体は不要なので、ここで大量の JOIN をそのまま投げられる。
    let n = 100_000

    let joins =
        Array.init n (fun i -> sprintf "JOIN t%d ON x = y" (i + 1)) |> String.concat " "

    let sel = parseSelectOk (sprintf "SELECT * FROM t0 %s" joins)
    Assert.Equal(n, sel.Joins.Length)

// ---------------------------------------------------------------------------
// パーサ: 文（parseSelect → parseInsert → parseUpdate → parseCreateTable → parseStatements → parseStatement）
// ---------------------------------------------------------------------------

[<Fact>]
let ``パーサ: SELECT 各種`` () =
    let sel = parseSelectOk "SELECT * FROM users"

    Assert.Equal("users", (sel.From |> Option.map (fun t -> t.Name) |> Option.defaultValue ""))
    Assert.Equal(1, sel.Items.Length)
    Assert.Empty(sel.Joins)

    let sel2 =
        parseSelectOk
            "SELECT DISTINCT a, b AS bb, COUNT(*) FROM t WHERE a > 1 GROUP BY a HAVING COUNT(*) > 2 ORDER BY a DESC LIMIT 10 OFFSET 5"

    Assert.True(sel2.Distinct)
    Assert.Equal(3, sel2.Items.Length)
    Assert.Equal(1, sel2.GroupBy.Length)
    Assert.True(sel2.Having.IsSome)
    Assert.Equal(1, sel2.OrderBy.Length)
    Assert.Equal<SortDirection>(SortDirection.Descending, sel2.OrderBy.[0].Direction)
    Assert.True(sel2.Limit.IsSome)
    Assert.True(sel2.Offset.IsSome)

[<Fact>]
let ``パーサ: INSERT / UPDATE / DELETE`` () =
    let ins = parseInsertOk "INSERT INTO t (a, b) VALUES (1, 'x'), (2, 'y')"
    Assert.Equal(2, ins.Rows.Length)
    Assert.Equal<string list option>(Some [ "a"; "b" ], ins.Columns)

    let ins2 = parseInsertOk "INSERT INTO t VALUES (1)"
    Assert.True(ins2.Columns.IsNone)

    let upd = parseUpdateOk "UPDATE t SET a = 1, b = 2 WHERE id = 3"
    Assert.Equal(2, upd.Assignments.Length)
    Assert.True(upd.Where.IsSome)

    let del = parseDeleteOk "DELETE FROM t WHERE id = 1"
    Assert.True(del.Where.IsSome)

[<Fact>]
let ``末尾再帰: 大量の SET 句がパースできる`` () =
    // UPDATE の SET 連なりは `expectSymbol >>= … parseExpr >>= fun … -> assigns …`
    // で列の数だけフレームが積まれていた。`match` で受けて末尾再帰にしてある。
    let n = 200_000

    let body = Array.init n (fun i -> sprintf "c%d = %d" i i) |> String.concat ", "

    let upd = parseUpdateOk (sprintf "UPDATE t SET %s" body)
    Assert.Equal(n, upd.Assignments.Length)

[<Fact>]
let ``パーサ: CREATE TABLE / INDEX / DROP`` () =
    let ct =
        parseCreateTableOk
            "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER DEFAULT 0, UNIQUE(name, age))"

    Assert.Equal("users", ct.Name)
    Assert.Equal(3, ct.Columns.Length)
    Assert.Equal<DataType>(DataType.Integer, ct.Columns.[0].DataType)
    Assert.True(ct.Columns.[0].Constraint.PrimaryKey)
    Assert.True(ct.Columns.[1].Constraint.NotNull)
    Assert.True(ct.Columns.[2].Constraint.Default.IsSome)
    Assert.Equal(1, ct.Constraints.Length)

    let ci = parseCreateIndexOk "CREATE UNIQUE INDEX idx ON users (name)"
    Assert.Equal("idx", ci.Name)
    Assert.Equal("users", ci.Table)
    Assert.Equal<string list>([ "name" ], ci.Columns)
    Assert.True(ci.Unique)

    let (n, ifEx) = parseDropTableOk "DROP TABLE IF EXISTS users"
    Assert.Equal("users", n)
    Assert.True(ifEx)

[<Fact>]
let ``パーサ: 複数文`` () =
    match ok (Parser.parseStatements "CREATE TABLE t (a INTEGER); INSERT INTO t VALUES (1);") with
    | [ Statement.CreateTable _; Statement.Insert _ ] -> ()
    | other -> Assert.Fail(sprintf "想定外の文列: %A" other)

[<Fact>]
let ``末尾再帰: 大量の文を分解できる`` () =
    let n = 100_000

    let sql =
        String.Join(";", Array.init n (fun i -> sprintf "SELECT a FROM t%d WHERE a = %d" (i % 7) i))

    Assert.Equal(n, (ok (Parser.parseStatements sql)).Length)

[<Fact>]
let ``パーサ: トランザクション制御`` () =
    // xUnit の IsType は F# のオーバーロード解決が噛み合わないため、
    // パターンマッチで検証する。
    match parseOneOk "BEGIN" with
    | Statement.Begin -> ()
    | other -> Assert.Fail(sprintf "BEGIN ではない: %A" other)

    match parseOneOk "COMMIT" with
    | Statement.Commit -> ()
    | other -> Assert.Fail(sprintf "COMMIT ではない: %A" other)

    match parseOneOk "ROLLBACK" with
    | Statement.Rollback -> ()
    | other -> Assert.Fail(sprintf "ROLLBACK ではない: %A" other)

[<Fact>]
let ``パーサ: 構文エラーは位置つき DbError`` () =
    match Parser.parseStatement "SELECT * FROM" with
    | Error e ->
        Assert.Equal<ErrorKind>(ErrorKind.SyntaxError, e.Kind)
        Assert.True(e.Position.IsSome)
    | Ok _ -> Assert.Fail("エラーになるべき")

[<Fact>]
let ``パーサ: 型引数を読める`` () =
    let ct =
        parseCreateTableOk
            "CREATE TABLE t (id INTEGER PRIMARY KEY, name VARCHAR(32), price DECIMAL(10, 2), note VARCHAR(nope))"

    Assert.Equal<(string * int) list>([ ("n", 32) ], ct.Columns.[1].TypeArguments)
    Assert.Equal<(string * int) list>([ ("n", 10); ("n", 2) ], ct.Columns.[2].TypeArguments)
    // 整数リテラルでない引数は落とす
    Assert.Empty(ct.Columns.[3].TypeArguments)

[<Fact>]
let ``パーサ: 引用した識別子`` () =
    let ct =
        parseCreateTableOk "CREATE TABLE \"my tbl\" (\"id\" INTEGER PRIMARY KEY, \"va lue\" TEXT)"

    Assert.Equal("my tbl", ct.Name)
    Assert.Equal<string array>([| "id"; "va lue" |], ct.Columns |> List.map _.Name |> List.toArray)

    let sel = parseSelectOk "SELECT \"p\".*, \"p\".\"id\" FROM people AS \"p\""
    Assert.Equal(Some "p", sel.From |> Option.bind (fun t -> t.Alias))

    match sel.Items.[0].Expression with
    | Expr.QualifiedStar q -> Assert.Equal("p", q)
    | other -> Assert.Fail(sprintf "QualifiedStar ではない: %A" other)

[<Fact>]
let ``パーサ: 型名は識別子にも使える`` () =
    // 型名はキーワードとして字句解析されるが、識別子の位置では名前として受け付ける
    let ct =
        parseCreateTableOk "CREATE TABLE t (id INTEGER PRIMARY KEY, text TEXT, integer INTEGER)"

    Assert.Equal<string array>([| "id"; "text"; "integer" |], ct.Columns |> List.map _.Name |> List.toArray)
    Assert.Equal<DataType>(DataType.Text, ct.Columns.[1].DataType)

[<Fact>]
let ``パーサ: 列制約の諸形`` () =
    let ct =
        parseCreateTableOk
            "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE, nick NOT NULL, v DEFAULT 1, u PRIMARY KEY)"

    Assert.True(ct.Columns.[1].Constraint.Unique)
    Assert.True(ct.Columns.[2].Constraint.NotNull)
    // 型を書かない列は TEXT（制約だけの定義）
    Assert.Equal<DataType>(DataType.Text, ct.Columns.[2].DataType)
    Assert.True(ct.Columns.[3].Constraint.Default.IsSome)
    Assert.Equal<DataType>(DataType.Text, ct.Columns.[3].DataType)
    Assert.True(ct.Columns.[4].Constraint.PrimaryKey)

[<Fact>]
let ``パーサ: テーブル制約の主キー`` () =
    let ct =
        parseCreateTableOk "CREATE TABLE t (a INTEGER, b INTEGER, PRIMARY KEY (a, b))"

    match ct.Constraints with
    | [ TableConstraint.PrimaryKeyColumns cols ] -> Assert.Equal<string list>([ "a"; "b" ], cols)
    | other -> Assert.Fail(sprintf "テーブル制約が違う: %A" other)

[<Fact>]
let ``パーサ: DROP INDEX`` () =
    match parseOneOk "DROP INDEX IF EXISTS ix_g" with
    | Statement.DropIndex(n, ifEx) ->
        Assert.Equal("ix_g", n)
        Assert.True(ifEx)
    | other -> Assert.Fail(sprintf "DROP INDEX ではない: %A" other)

[<Fact>]
let ``パーサ: リテラルと引用識別子を式として読める`` () =
    // REAL / NULL / TRUE / FALSE のリテラル
    parsesOk "SELECT * FROM t WHERE a > 1.5 AND b = NULL AND c = TRUE AND d = FALSE"
    // 式の中の引用識別子
    parsesOk "SELECT * FROM t WHERE \"a\" > 1"
    // 式の中のスター（検証は後段）
    parsesOk "SELECT * FROM t WHERE *"

[<Fact>]
let ``パーサ: 別名と並び替え・グループ化の複数指定`` () =
    // AS なしの別名
    let sel = parseSelectOk "SELECT * FROM t x"

    Assert.Equal(Some "x", sel.From |> Option.bind (fun t -> t.Alias))

    // AS なしの引用した別名
    let sel2 = parseSelectOk "SELECT * FROM t \"x\""
    Assert.Equal(Some "x", sel2.From |> Option.bind (fun t -> t.Alias))

    // GROUP BY / ORDER BY のカンマ区切り
    let sel3 = parseSelectOk "SELECT a, COUNT(*) FROM t GROUP BY a, b ORDER BY a, b"
    Assert.Equal<int>(2, sel3.GroupBy.Length)
    Assert.Equal<int>(2, sel3.OrderBy.Length)

[<Fact>]
let ``パーサ: 文法エラーの諸形`` () =
    // 式の括弧閉じ
    expectErrorKind ErrorKind.SyntaxError ") が無い" (Parser.parseStatement "SELECT (1 FROM t")
    // IN の閉じ括弧
    expectErrorKind ErrorKind.SyntaxError "IN の ) が無い" (Parser.parseStatement "SELECT * FROM t WHERE a IN (1, 2")
    // NOT の後ろ
    expectErrorKind ErrorKind.SyntaxError "NOT の後ろが不正" (Parser.parseStatement "SELECT * FROM t WHERE a NOT 1")
    // BETWEEN の AND
    expectErrorKind
        ErrorKind.SyntaxError
        "BETWEEN の AND が無い"
        (Parser.parseStatement "SELECT * FROM t WHERE a BETWEEN 1")
    // CASE
    expectErrorKind ErrorKind.SyntaxError "CASE の WHEN が無い" (Parser.parseStatement "SELECT CASE END FROM t")
    expectErrorKind ErrorKind.SyntaxError "CASE の END が無い" (Parser.parseStatement "SELECT CASE WHEN 1 THEN 2 FROM t")
    // JOIN の ON
    expectErrorKind ErrorKind.SyntaxError "JOIN の ON が無い" (Parser.parseStatement "SELECT * FROM t JOIN u")
    // ORDER BY / GROUP BY / LIMIT の式
    expectErrorKind ErrorKind.SyntaxError "ORDER BY の式が無い" (Parser.parseStatement "SELECT * FROM t ORDER BY")
    expectErrorKind ErrorKind.SyntaxError "GROUP BY の式が無い" (Parser.parseStatement "SELECT * FROM t GROUP BY")
    expectErrorKind ErrorKind.SyntaxError "LIMIT の式が無い" (Parser.parseStatement "SELECT * FROM t LIMIT")
    // 関数呼び出しの閉じ括弧
    expectErrorKind ErrorKind.SyntaxError "関数呼び出しの ) が無い" (Parser.parseStatement "SELECT f(a FROM t")
    // DDL の形
    expectErrorKind ErrorKind.SyntaxError "未知の型" (Parser.parseStatement "CREATE TABLE t (id NOPE PRIMARY KEY)")
    expectErrorKind ErrorKind.SyntaxError "型名が無い" (Parser.parseStatement "CREATE TABLE t (id (1) PRIMARY KEY)")

    expectErrorKind
        ErrorKind.SyntaxError
        "NOT の後ろが NULL でない"
        (Parser.parseStatement "CREATE TABLE t (id INTEGER PRIMARY KEY, v NOT 1)")

    expectErrorKind
        ErrorKind.SyntaxError
        "DEFAULT の式が無い"
        (Parser.parseStatement "CREATE TABLE t (id INTEGER PRIMARY KEY, v DEFAULT)")

    expectErrorKind
        ErrorKind.SyntaxError
        "テーブル主キーの ( が無い"
        (Parser.parseStatement "CREATE TABLE t (a INTEGER, PRIMARY KEY a)")

    expectErrorKind
        ErrorKind.SyntaxError
        "テーブル主キーの KEY が無い"
        (Parser.parseStatement "CREATE TABLE t (a INTEGER, PRIMARY (a))")

    expectErrorKind
        ErrorKind.SyntaxError
        "主キー列の後に要素が無い"
        (Parser.parseStatement "CREATE TABLE t (a INTEGER, PRIMARY KEY (a,))")

    expectErrorKind
        ErrorKind.SyntaxError
        "列制約の PRIMARY の後に KEY が無い"
        (Parser.parseStatement "CREATE TABLE t (a INTEGER PRIMARY 1)")

    expectErrorKind
        ErrorKind.SyntaxError
        "型引数の式が不正"
        (Parser.parseStatement "CREATE TABLE t (a INTEGER PRIMARY KEY, v VARCHAR()")
    // 式の細かいエラー
    expectErrorKind ErrorKind.SyntaxError "関数引数の後続が不正" (Parser.parseStatement "SELECT f(a,) FROM t")
    expectErrorKind ErrorKind.SyntaxError "IN の ( が無い" (Parser.parseStatement "SELECT * FROM t WHERE a IN 1")
    expectErrorKind ErrorKind.SyntaxError "IN リストが空" (Parser.parseStatement "SELECT * FROM t WHERE a IN ()")
    expectErrorKind ErrorKind.SyntaxError "COUNT(*) の ) が無い" (Parser.parseStatement "SELECT COUNT(* FROM t")
    expectErrorKind ErrorKind.SyntaxError "修飾名の後続が不正" (Parser.parseStatement "SELECT * FROM t WHERE t. = 1")

    // 選択リストの「t. の後が列名でない」場合は式として読み直す（結果として文法エラー）
    expectErrorKind ErrorKind.SyntaxError "選択リストの t. が不正" (Parser.parseStatement "SELECT t. FROM t")

    expectErrorKind
        ErrorKind.SyntaxError
        "IS NOT の後ろが NULL でない"
        (Parser.parseStatement "SELECT * FROM t WHERE a IS NOT 1")

    expectErrorKind
        ErrorKind.SyntaxError
        "CASE WHEN の条件が無い"
        (Parser.parseStatement "SELECT CASE WHEN THEN 2 END FROM t")

    expectErrorKind
        ErrorKind.SyntaxError
        "CASE WHEN の THEN が無い"
        (Parser.parseStatement "SELECT CASE WHEN 1 2 END FROM t")

    expectErrorKind
        ErrorKind.SyntaxError
        "CASE THEN の結果が無い"
        (Parser.parseStatement "SELECT CASE WHEN 1 THEN END FROM t")

    expectErrorKind
        ErrorKind.SyntaxError
        "CASE ELSE の後に END が無い"
        (Parser.parseStatement "SELECT CASE WHEN 1 THEN 2 ELSE 3 FROM t")
    // JOIN の細かいエラー
    expectErrorKind ErrorKind.SyntaxError "JOIN のテーブルが無い" (Parser.parseStatement "SELECT * FROM t JOIN")
    expectErrorKind ErrorKind.SyntaxError "JOIN の ON 式が無い" (Parser.parseStatement "SELECT * FROM t JOIN u ON")
    // DML の細かいエラー
    expectErrorKind ErrorKind.SyntaxError "INSERT の行の ( が無い" (Parser.parseStatement "INSERT INTO t VALUES 1")
    expectErrorKind ErrorKind.SyntaxError "INSERT の行が空" (Parser.parseStatement "INSERT INTO t VALUES ()")
    expectErrorKind ErrorKind.SyntaxError "INSERT の値の後に要素が無い" (Parser.parseStatement "INSERT INTO t VALUES (1,)")
    expectErrorKind ErrorKind.SyntaxError "UPDATE の列名が無い" (Parser.parseStatement "UPDATE t SET")
    expectErrorKind ErrorKind.SyntaxError "UPDATE の = が無い" (Parser.parseStatement "UPDATE t SET a 1")
    expectErrorKind ErrorKind.SyntaxError "UPDATE の値が無い" (Parser.parseStatement "UPDATE t SET a =")
    // 文の種類
    expectErrorKind ErrorKind.SyntaxError "CREATE VIEW" (Parser.parseStatement "CREATE VIEW v")
    expectErrorKind ErrorKind.SyntaxError "DROP VIEW" (Parser.parseStatement "DROP VIEW v")
    // 空・複数文
    expectErrorKind ErrorKind.SyntaxError "空 SQL" (Parser.parseStatement "")
    expectErrorKind ErrorKind.SyntaxError "複数文" (Parser.parseStatement "SELECT * FROM t; SELECT * FROM t")

    expectErrorKind ErrorKind.SyntaxError "エラーになるべき" (Parser.parseStatement "SELCT * FROM t")
