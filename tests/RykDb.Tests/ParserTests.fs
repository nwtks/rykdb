// ===========================================================================
// Parser.fs（文法・演算子優先順位・CPS の式構文規則・深さ無制限）
// ===========================================================================

module RykDb.ParserTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers


/// FROM の実テーブル参照（派生テーブルや FROM なしは None）。
let private fromTable (sel: SelectStatement) =
    match sel.From with
    | Some(TableSource.Named r) -> Some r
    | _ -> None

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
let ``Parser: INSERT / UPDATE / DELETE`` () =
    let ins = parseInsertOk "INSERT INTO t (a, b) VALUES (1, 'x'), (2, 'y')"

    match ins.Source with
    | InsertSource.Values rows -> Assert.Equal(2, rows.Length)
    | other -> failActual "VALUES ではない" other

    Assert.Equal<string list option>(Some [ "a"; "b" ], ins.Columns)

    let ins2 = parseInsertOk "INSERT INTO t VALUES (1)"
    Assert.True ins2.Columns.IsNone

    let upd = parseUpdateOk "UPDATE t SET a = 1, b = 2 WHERE id = 3"
    Assert.Equal(2, upd.Assignments.Length)
    Assert.True upd.Where.IsSome

    let del = parseDeleteOk "DELETE FROM t WHERE id = 1"
    Assert.True del.Where.IsSome

[<Fact>]
let ``Parser: INSERT ... SELECT`` () =
    let ins = parseInsertOk "INSERT INTO t (a, b) SELECT x, y FROM u WHERE x > 0"
    Assert.Equal<string list option>(Some [ "a"; "b" ], ins.Columns)

    match ins.Source with
    | InsertSource.Select sel ->
        Assert.Equal(2, sel.Items.Length)
        Assert.True sel.Where.IsSome
    | other -> failActual "SELECT ではない" other

    // 列リストなし・ORDER BY / LIMIT 付き
    let ins2 = parseInsertOk "INSERT INTO t SELECT * FROM u ORDER BY x LIMIT 2"
    Assert.True ins2.Columns.IsNone

    match ins2.Source with
    | InsertSource.Select sel ->
        Assert.Equal(1, sel.OrderBy.Length)
        Assert.True sel.Limit.IsSome
    | other -> failActual "SELECT ではない" other

[<Fact>]
let ``Parser: DML のテーブル別名は明示エラー`` () =
    // 別名は列解決に使われないため、受理して黙って無視するのをやめた
    expectErrorKind ErrorKind.SyntaxError "UPDATE の別名" (parseStatement "UPDATE t AS x SET a = 1")
    expectErrorKind ErrorKind.SyntaxError "DELETE の別名" (parseStatement "DELETE FROM t AS x")
    expectErrorKind ErrorKind.SyntaxError "INSERT の別名" (parseStatement "INSERT INTO t AS x VALUES (1)")

[<Fact>]
let ``Parser: CREATE TABLE / INDEX / DROP`` () =
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
    Assert.True ci.Unique

    let n, ifEx = parseDropTableOk "DROP TABLE IF EXISTS users"
    Assert.Equal("users", n)
    Assert.True ifEx

[<Fact>]
let ``Parser: CREATE INDEX IF NOT EXISTS`` () =
    match parseOneOk "CREATE INDEX IF NOT EXISTS ix ON t (a)" with
    | Statement.CreateIndex ci ->
        Assert.True ci.IfNotExists
        Assert.Equal<string>("ix", ci.Name)
    | other -> failActual "CREATE INDEX ではない" other

[<Fact>]
let ``Parser: 複数文`` () =
    match ok (Parser.parseStatements "CREATE TABLE t (a INTEGER); INSERT INTO t VALUES (1);") with
    | [ Statement.CreateTable _; Statement.Insert _ ] -> ()
    | other -> failActual "想定外の文列" other

[<Fact>]
let ``Parser: 位置パラメータを値へ置き換える`` () =
    let parsed =
        Parser.parseStatementsWith [| SqlValue.Int 1L; SqlValue.Text "x" |] "SELECT id FROM t WHERE a = ? AND b = ?"

    match ok parsed with
    | [ Statement.Select sel ] ->
        match sel.Where with
        | Some(Expr.Binary(BinaryOp.And,
                           Expr.Binary(BinaryOp.Eq, _, Expr.Value a),
                           Expr.Binary(BinaryOp.Eq, _, Expr.Value b))) ->
            Assert.Equal(SqlValue.Int 1L, a)
            Assert.Equal(SqlValue.Text "x", b)
        | other -> failActual "WHERE が想定外" other
    | other -> failActual "SELECT ではない" other

[<Fact>]
let ``Parser: パラメータの個数が一致しないときは TypeError`` () =
    expectErrorKind ErrorKind.TypeError "パラメータ不足" (Parser.parseStatementsWith [||] "SELECT ?")
    expectErrorKind ErrorKind.TypeError "パラメータ過剰" (Parser.parseStatementsWith [| SqlValue.Int 1L |] "SELECT 1")

[<Fact>]
let ``Parser: 複数文は ; で区切る`` () =
    expectErrorKind ErrorKind.SyntaxError "区切りなし" (parseStatement "SELECT a FROM t SELECT a FROM t")

    Assert.Equal<int>(2, (ok (Parser.parseStatements "SELECT a FROM t; SELECT a FROM t")).Length)

[<Fact>]
let ``末尾再帰: 大量の文を分解できる`` () =
    let n = 10_000

    let sql =
        System.String.Join(";", Array.init n (fun i -> sprintf "SELECT a FROM t%d WHERE a = %d" (i % 7) i))

    Assert.Equal(n, (ok (Parser.parseStatements sql)).Length)

[<Fact>]
let ``Parser: トランザクション制御`` () =
    // xUnit の IsType は F# のオーバーロード解決が噛み合わないため、
    // パターンマッチで検証する。
    match parseOneOk "BEGIN" with
    | Statement.Begin -> ()
    | other -> failActual "BEGIN ではない" other

    match parseOneOk "COMMIT" with
    | Statement.Commit -> ()
    | other -> failActual "COMMIT ではない" other

    match parseOneOk "ROLLBACK" with
    | Statement.Rollback -> ()
    | other -> failActual "ROLLBACK ではない" other

[<Fact>]
let ``Parser: トランザクションの別綴り`` () =
    (match parseOneOk "BEGIN TRANSACTION" with
     | Statement.Begin -> ()
     | other -> failActual "BEGIN ではない" other)

    (match parseOneOk "START TRANSACTION" with
     | Statement.Begin -> ()
     | other -> failActual "START ではない" other)

    (match parseOneOk "COMMIT WORK" with
     | Statement.Commit -> ()
     | other -> failActual "COMMIT ではない" other)

    (match parseOneOk "ROLLBACK TRANSACTION" with
     | Statement.Rollback -> ()
     | other -> failActual "ROLLBACK ではない" other)

    match parseOneOk "END" with
    | Statement.Commit -> ()
    | other -> failActual "END ではない" other

[<Fact>]
let ``Parser: 構文エラーは位置つき DbError`` () =
    match parseStatement "SELECT * FROM" with
    | Error e ->
        Assert.Equal<ErrorKind>(ErrorKind.SyntaxError, e.Kind)
        Assert.True e.Position.IsSome
    | Ok _ -> Assert.Fail "エラーになるべき"

[<Fact>]
let ``Parser: 型引数を読める`` () =
    let ct =
        parseCreateTableOk
            "CREATE TABLE t (id INTEGER PRIMARY KEY, name VARCHAR(32), price DECIMAL(10, 2), note VARCHAR(nope))"

    Assert.Equal<(string * int) list>([ ("n", 32) ], ct.Columns.[1].TypeArguments)
    Assert.Equal<(string * int) list>([ "n", 10; "n", 2 ], ct.Columns.[2].TypeArguments)
    // 整数リテラルでない引数は落とす
    Assert.Empty(ct.Columns.[3].TypeArguments)

[<Fact>]
let ``Parser: 引用した識別子`` () =
    let ct =
        parseCreateTableOk "CREATE TABLE \"my tbl\" (\"id\" INTEGER PRIMARY KEY, \"va lue\" TEXT)"

    Assert.Equal("my tbl", ct.Name)
    Assert.Equal<string array>([| "id"; "va lue" |], ct.Columns |> List.map _.Name |> List.toArray)

    let sel = parseSelectOk "SELECT \"p\".*, \"p\".\"id\" FROM people AS \"p\""
    Assert.Equal(Some "p", fromTable sel |> Option.bind (fun t -> t.Alias))

    match sel.Items.[0].Expression with
    | Expr.QualifiedStar q -> Assert.Equal("p", q)
    | other -> failActual "QualifiedStar ではない" other

[<Fact>]
let ``Parser: NULL.* を QualifiedStar にしない`` () =
    // 以前は `NULL` の後ろの `.*` を読んで `QualifiedStar ""` を作っていた
    expectErrorKind ErrorKind.SyntaxError "NULL.*" (parseStatement "SELECT NULL.* FROM t")

[<Fact>]
let ``Parser: 型名は識別子にも使える`` () =
    // 型名は予約語ではなく Ident として字句解析されるので、識別子の位置でそのまま使える
    let ct =
        parseCreateTableOk "CREATE TABLE t (id INTEGER PRIMARY KEY, text TEXT, integer INTEGER)"

    Assert.Equal<string array>([| "id"; "text"; "integer" |], ct.Columns |> List.map _.Name |> List.toArray)
    Assert.Equal<DataType>(DataType.Text, ct.Columns.[1].DataType)

[<Fact>]
let ``Parser: 列制約の諸形`` () =
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
let ``Parser: テーブル制約の主キー`` () =
    let ct =
        parseCreateTableOk "CREATE TABLE t (a INTEGER, b INTEGER, PRIMARY KEY (a, b))"

    match ct.Constraints with
    | [ TableConstraint.PrimaryKeyColumns cols ] -> Assert.Equal<string list>([ "a"; "b" ], cols)
    | other -> failActual "テーブル制約が違う" other

[<Fact>]
let ``Parser: DROP INDEX`` () =
    match parseOneOk "DROP INDEX IF EXISTS ix_g" with
    | Statement.DropIndex(n, ifEx) ->
        Assert.Equal("ix_g", n)
        Assert.True(ifEx)
    | other -> failActual "DROP INDEX ではない" other

[<Fact>]
let ``Parser: リテラルと引用識別子を式として読める`` () =
    // REAL / NULL / TRUE / FALSE のリテラル
    parsesOk "SELECT * FROM t WHERE a > 1.5 AND b = NULL AND c = TRUE AND d = FALSE"
    // 式の中の引用識別子
    parsesOk "SELECT * FROM t WHERE \"a\" > 1"
    // 式の中のスター（検証は後段）
    parsesOk "SELECT * FROM t WHERE *"

[<Fact>]
let ``Parser: 別名と並び替え・グループ化の複数指定`` () =
    // AS なしの別名
    let sel = parseSelectOk "SELECT * FROM t x"

    Assert.Equal(Some "x", fromTable sel |> Option.bind (fun t -> t.Alias))

    // AS なしの引用した別名
    let sel2 = parseSelectOk "SELECT * FROM t \"x\""
    Assert.Equal(Some "x", fromTable sel2 |> Option.bind (fun t -> t.Alias))

    // GROUP BY / ORDER BY のカンマ区切り
    let sel3 = parseSelectOk "SELECT a, COUNT(*) FROM t GROUP BY a, b ORDER BY a, b"
    Assert.Equal<int>(2, sel3.GroupBy.Length)
    Assert.Equal<int>(2, sel3.OrderBy.Length)

[<Fact>]
let ``Parser: 文法エラーの諸形`` () =
    // 式の括弧閉じ
    expectErrorKind ErrorKind.SyntaxError ") が無い" (parseStatement "SELECT (1 FROM t")
    // IN の閉じ括弧
    expectErrorKind ErrorKind.SyntaxError "IN の ) が無い" (parseStatement "SELECT * FROM t WHERE a IN (1, 2")
    // NOT の後ろ
    expectErrorKind ErrorKind.SyntaxError "NOT の後ろが不正" (parseStatement "SELECT * FROM t WHERE a NOT 1")
    // BETWEEN の AND
    expectErrorKind ErrorKind.SyntaxError "BETWEEN の AND が無い" (parseStatement "SELECT * FROM t WHERE a BETWEEN 1")
    // CASE
    expectErrorKind ErrorKind.SyntaxError "CASE の WHEN が無い" (parseStatement "SELECT CASE END FROM t")
    expectErrorKind ErrorKind.SyntaxError "CASE の END が無い" (parseStatement "SELECT CASE WHEN 1 THEN 2 FROM t")
    // JOIN の ON
    expectErrorKind ErrorKind.SyntaxError "JOIN の ON が無い" (parseStatement "SELECT * FROM t JOIN u")
    // ORDER BY / GROUP BY / LIMIT の式
    expectErrorKind ErrorKind.SyntaxError "ORDER BY の式が無い" (parseStatement "SELECT * FROM t ORDER BY")
    expectErrorKind ErrorKind.SyntaxError "GROUP BY の式が無い" (parseStatement "SELECT * FROM t GROUP BY")
    expectErrorKind ErrorKind.SyntaxError "LIMIT の式が無い" (parseStatement "SELECT * FROM t LIMIT")
    // 関数呼び出しの閉じ括弧
    expectErrorKind ErrorKind.SyntaxError "関数呼び出しの ) が無い" (parseStatement "SELECT f(a FROM t")
    // DDL の形
    expectErrorKind ErrorKind.SyntaxError "未知の型" (parseStatement "CREATE TABLE t (id NOPE PRIMARY KEY)")
    expectErrorKind ErrorKind.SyntaxError "型名が無い" (parseStatement "CREATE TABLE t (id (1) PRIMARY KEY)")

    expectErrorKind
        ErrorKind.SyntaxError
        "NOT の後ろが NULL でない"
        (parseStatement "CREATE TABLE t (id INTEGER PRIMARY KEY, v NOT 1)")

    expectErrorKind
        ErrorKind.SyntaxError
        "DEFAULT の式が無い"
        (parseStatement "CREATE TABLE t (id INTEGER PRIMARY KEY, v DEFAULT)")

    expectErrorKind ErrorKind.SyntaxError "テーブル主キーの ( が無い" (parseStatement "CREATE TABLE t (a INTEGER, PRIMARY KEY a)")
    expectErrorKind ErrorKind.SyntaxError "テーブル主キーの KEY が無い" (parseStatement "CREATE TABLE t (a INTEGER, PRIMARY (a))")
    expectErrorKind ErrorKind.SyntaxError "主キー列の後に要素が無い" (parseStatement "CREATE TABLE t (a INTEGER, PRIMARY KEY (a,))")

    expectErrorKind
        ErrorKind.SyntaxError
        "列制約の PRIMARY の後に KEY が無い"
        (parseStatement "CREATE TABLE t (a INTEGER PRIMARY 1)")

    expectErrorKind
        ErrorKind.SyntaxError
        "型引数の式が不正"
        (parseStatement "CREATE TABLE t (a INTEGER PRIMARY KEY, v VARCHAR()")
    // 式の細かいエラー
    expectErrorKind ErrorKind.SyntaxError "関数引数の後続が不正" (parseStatement "SELECT f(a,) FROM t")
    expectErrorKind ErrorKind.SyntaxError "IN の ( が無い" (parseStatement "SELECT * FROM t WHERE a IN 1")
    expectErrorKind ErrorKind.SyntaxError "IN リストが空" (parseStatement "SELECT * FROM t WHERE a IN ()")
    expectErrorKind ErrorKind.SyntaxError "COUNT(*) の ) が無い" (parseStatement "SELECT COUNT(* FROM t")
    expectErrorKind ErrorKind.SyntaxError "修飾名の後続が不正" (parseStatement "SELECT * FROM t WHERE t. = 1")

    // 選択リストの「t. の後が列名でない」場合は式として読み直す（結果として文法エラー）
    expectErrorKind ErrorKind.SyntaxError "選択リストの t. が不正" (parseStatement "SELECT t. FROM t")
    expectErrorKind ErrorKind.SyntaxError "IS NOT の後ろが NULL でない" (parseStatement "SELECT * FROM t WHERE a IS NOT 1")
    expectErrorKind ErrorKind.SyntaxError "CASE WHEN の条件が無い" (parseStatement "SELECT CASE WHEN THEN 2 END FROM t")
    expectErrorKind ErrorKind.SyntaxError "CASE WHEN の THEN が無い" (parseStatement "SELECT CASE WHEN 1 2 END FROM t")
    expectErrorKind ErrorKind.SyntaxError "CASE THEN の結果が無い" (parseStatement "SELECT CASE WHEN 1 THEN END FROM t")

    expectErrorKind
        ErrorKind.SyntaxError
        "CASE ELSE の後に END が無い"
        (parseStatement "SELECT CASE WHEN 1 THEN 2 ELSE 3 FROM t")
    // JOIN の細かいエラー
    expectErrorKind ErrorKind.SyntaxError "JOIN のテーブルが無い" (parseStatement "SELECT * FROM t JOIN")
    expectErrorKind ErrorKind.SyntaxError "JOIN の ON 式が無い" (parseStatement "SELECT * FROM t JOIN u ON")
    // DML の細かいエラー
    expectErrorKind ErrorKind.SyntaxError "INSERT の行の ( が無い" (parseStatement "INSERT INTO t VALUES 1")
    expectErrorKind ErrorKind.SyntaxError "INSERT の行が空" (parseStatement "INSERT INTO t VALUES ()")
    expectErrorKind ErrorKind.SyntaxError "INSERT の値の後に要素が無い" (parseStatement "INSERT INTO t VALUES (1,)")
    expectErrorKind ErrorKind.SyntaxError "UPDATE の列名が無い" (parseStatement "UPDATE t SET")
    expectErrorKind ErrorKind.SyntaxError "UPDATE の = が無い" (parseStatement "UPDATE t SET a 1")
    expectErrorKind ErrorKind.SyntaxError "UPDATE の値が無い" (parseStatement "UPDATE t SET a =")
    // 文の種類
    expectErrorKind ErrorKind.SyntaxError "CREATE VIEW" (parseStatement "CREATE VIEW v")
    expectErrorKind ErrorKind.SyntaxError "DROP VIEW" (parseStatement "DROP VIEW v")
    // 空・複数文
    expectErrorKind ErrorKind.SyntaxError "空 SQL" (parseStatement "")
    expectErrorKind ErrorKind.SyntaxError "複数文" (parseStatement "SELECT * FROM t; SELECT * FROM t")
    expectErrorKind ErrorKind.SyntaxError "エラーになるべき" (parseStatement "SELCT * FROM t")

[<Fact>]
let ``Parser: 未対応の集合演算は明示エラー`` () =
    expectErrorKind ErrorKind.SyntaxError "UNION" (parseStatement "SELECT a FROM t UNION SELECT a FROM t")
    expectErrorKind ErrorKind.SyntaxError "UNION ALL" (parseStatement "SELECT a FROM t UNION ALL SELECT a FROM t")
    expectErrorKind ErrorKind.SyntaxError "INTERSECT" (parseStatement "SELECT a FROM t INTERSECT SELECT a FROM t")
    expectErrorKind ErrorKind.SyntaxError "EXCEPT" (parseStatement "SELECT a FROM t EXCEPT SELECT a FROM t")

    // 句の後ろに来る場合も見逃さない
    expectErrorKind
        ErrorKind.SyntaxError
        "WHERE の後の UNION"
        (parseStatement "SELECT a FROM t WHERE a = 1 UNION SELECT a FROM t")

[<Fact>]
let ``Parser: 列別名が集合演算を飲み込まない`` () =
    // 以前は SELECT 項目の別名が UNION を消費し、後続の SELECT が別の文として扱われていた
    expectErrorKind ErrorKind.SyntaxError "UNION" (parseStatement "SELECT 1 UNION SELECT 2")
    expectErrorKind ErrorKind.SyntaxError "UNION ALL" (parseStatement "SELECT 1 UNION ALL SELECT 2")

    // AS を付ければ別名として使える
    let sel = parseSelectOk "SELECT a AS union FROM t"
    Assert.Equal<string option>(Some "union", sel.Items.[0].Alias)

[<Fact>]
let ``Parser: 未対応の JOIN は明示エラー`` () =
    expectErrorKind ErrorKind.SyntaxError "RIGHT JOIN" (parseStatement "SELECT * FROM t RIGHT JOIN u ON t.id = u.id")

    // 明示 AS の別名が付いていても、後ろの RIGHT JOIN は別名として飲み込まない
    expectErrorKind
        ErrorKind.SyntaxError
        "AS 別名の後の RIGHT JOIN"
        (parseStatement "SELECT * FROM t AS x RIGHT JOIN u ON x.id = u.id")

    expectErrorKind ErrorKind.SyntaxError "FULL JOIN" (parseStatement "SELECT * FROM t FULL JOIN u ON t.id = u.id")
    expectErrorKind ErrorKind.SyntaxError "CROSS JOIN" (parseStatement "SELECT * FROM t CROSS JOIN u")
    expectErrorKind ErrorKind.SyntaxError "NATURAL JOIN" (parseStatement "SELECT * FROM t NATURAL JOIN u")
    expectErrorKind ErrorKind.SyntaxError "USING" (parseStatement "SELECT * FROM t JOIN u USING (id)")

[<Fact>]
let ``Parser: JOIN の種類だけで JOIN が無い入力はエラー`` () =
    // 以前は LEFT / INNER を消費して以降の句を読み、JOIN を黙って捨てていた
    // （`FROM t LEFT WHERE ...` が「LEFT JOIN なし」として静かに通っていた）。
    expectErrorKind ErrorKind.SyntaxError "FROM の後の LEFT" (parseStatement "SELECT * FROM t LEFT")
    expectErrorKind ErrorKind.SyntaxError "FROM の後の INNER" (parseStatement "SELECT * FROM t INNER")
    expectErrorKind ErrorKind.SyntaxError "FROM の後の LEFT OUTER" (parseStatement "SELECT * FROM t LEFT OUTER")
    expectErrorKind ErrorKind.SyntaxError "LEFT の後の WHERE" (parseStatement "SELECT * FROM t LEFT WHERE 1 = 1")
    expectErrorKind ErrorKind.SyntaxError "INNER の後の ORDER BY" (parseStatement "SELECT * FROM t INNER ORDER BY 1")

    expectErrorKind
        ErrorKind.SyntaxError
        "既存 JOIN の後の LEFT"
        (parseStatement "SELECT * FROM t JOIN u ON 1 = 1 LEFT WHERE 1 = 1")

[<Fact>]
let ``Parser: 未対応語でも列名と明示 AS の別名には使える`` () =
    parseOneOk "SELECT right FROM t" |> ignore
    parseOneOk "SELECT a AS union FROM t" |> ignore
    parseOneOk "SELECT a FROM t AS union" |> ignore
    parseOneOk "CREATE TABLE x (right INTEGER PRIMARY KEY)" |> ignore

    // 裸のテーブル別名には使えない（AS を付ける）
    expectErrorKind ErrorKind.SyntaxError "裸の RIGHT 別名" (parseStatement "SELECT a FROM t right")
    // 裸の列別名にも使えない（集合演算を飲み込まないため）
    expectErrorKind ErrorKind.SyntaxError "裸の RIGHT 列別名" (parseStatement "SELECT a right FROM t")

[<Fact>]
let ``Parser: DELETE ... USING は明示エラー`` () =
    expectErrorKind ErrorKind.SyntaxError "DELETE USING" (parseStatement "DELETE FROM t USING u")
