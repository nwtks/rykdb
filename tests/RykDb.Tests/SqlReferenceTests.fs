// ===========================================================================
// docs/sql-reference.md のテスト: SQL の構文・意味論
//
// 「SQL 文字列 → 結果」で、リファレンスに書かれた構文（文・句・式・リテラル・
// 識別子）が受理 / 拒否され、評価結果も仕様どおりであることを検証する。
// ハンドル API・トランザクション機構・ロック・永続化・索引の内部実装は
// DatabaseTests.fs にある。
// ===========================================================================

module RykDb.SqlReferenceTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// REAL を取り出す（集計結果の検証用）。
let private asReal (v: SqlValue) =
    match v with
    | SqlValue.Real f -> f
    | _ -> failwithf "REAL ではありません: %A" v

/// EXPLAIN の出力から最初の `rows=` を数値で取り出す。
let private plannedRows (text: string) =
    text.Split '\n'
    |> Array.pick (fun line ->
        let i = line.IndexOf "rows="

        if i < 0 then
            None
        else
            line.Substring(i + 5)
            |> Seq.takeWhile (fun c -> System.Char.IsDigit c || c = '.')
            |> System.String.Concat
            |> float
            |> Some)

/// 同期 compaction でディスクの DB を開く（テスト用）。
let private openDb (root: string) =
    ok (Session.OpenWithSync(root, SessionOptions.Default))

/// 作業メモリを絞ったメモリ DB でテストする（DISTINCT 集約の退避経路）。
let private withTightMemory (_tag: string) (f: Session -> unit) =
    let root = "mem:" + System.Guid.NewGuid().ToString "N"
    let store = openStoreWith "" (memoryEngine ())

    let options =
        { SessionOptions.Default with
            WorkMemBytes = 2048L
        }

    let session = ok (Session.OpenWithStore(root, options, store))

    try
        f session
    finally
        ok (session.Close()) |> ignore

/// テスト用のテーブルを用意する。
let private seeded (tag: string) (f: Session -> unit) =
    withSession tag (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, _ =
            ok (execSql db1 "INSERT INTO people VALUES (1,'alice',30),(2,'bob',25),(3,'carol',35)")

        f db2)

/// SQL を実行し、エラーメッセージに断片が含まれることを確かめる。
/// §13「よく見るメッセージ（実測）」は文言そのものが仕様なので文字で照合する。
let private expectMessage (fragment: string) (sql: string) (db: Session) =
    match execSql db sql with
    | Ok _ -> Assert.Fail(sprintf "エラーにならないはず: %s" sql)
    | Error e -> Assert.Contains(fragment, DbError.toDisplayString e)

// ---------------------------------------------------------------------------
// 静かな誤答を防ぐ（識別子の大小文字・集計・HAVING・LIMIT・DEFAULT）
// ---------------------------------------------------------------------------

[<Fact>]
let ``SqlReference: 索引列と主キーの大文字小文字は区別しない`` () =
    withSession "case-ident" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE c (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO c VALUES (1, 'a'), (2, 'b')"
        let db3 = exec db2 "CREATE INDEX ci ON c (V)"

        // 索引キーが正しく作られ、索引経由でも行が見つかる
        let _, byIndex = ok (execSql db3 "SELECT id FROM c WHERE v = 'b'")
        Assert.Equal<int>(1, byIndex.Rows.Length)
        Assert.Equal<int64>(2L, asInt byIndex.Rows.[0].[0])

        // PRIMARY KEY (ID) の表記も通る
        let db4 = exec db3 "CREATE TABLE p (id INTEGER, v TEXT, PRIMARY KEY (ID))"
        let _, inserted = ok (execSql db4 "INSERT INTO p VALUES (1, 'x')")
        Assert.Equal(1, inserted.RowsAffected))

[<Fact>]
let ``SqlReference: SUM / AVG は型が混在しても値を落とさない`` () =
    withSession "sum-mixed" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE s (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO s VALUES (1), (2), (3)"

        // 1 + 1.5 + 1.5 = 4.0（INT と REAL の混在）
        let _, sum =
            ok (execSql db2 "SELECT SUM(CASE WHEN id = 1 THEN 1 ELSE 1.5 END) FROM s")

        Assert.Equal(4.0, asReal sum.Rows.[0].[0], 10)

        let _, avg =
            ok (execSql db2 "SELECT AVG(CASE WHEN id = 1 THEN 1 ELSE 1.5 END) FROM s")

        Assert.Equal(4.0 / 3.0, asReal avg.Rows.[0].[0], 10))

[<Fact>]
let ``SqlReference: 集約なしの HAVING はエラー`` () =
    withSession "having-noagg" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"

        expectErrorKind
            ErrorKind.SyntaxError
            "HAVING には GROUP BY か集約が必要"
            (execSql db2 "SELECT id FROM t HAVING id = 999"))

[<Fact>]
let ``SqlReference: GROUP BY と * の併用はエラー`` () =
    withSession "group-star" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10), (2, 10)"
        expectErrorKind ErrorKind.SyntaxError "SELECT * は GROUP BY と併用できない" (execSql db2 "SELECT * FROM t GROUP BY v"))

[<Fact>]
let ``SqlReference: テーブル名と同じ非修飾名は列として解決しない`` () =
    withSession "resolve-qual" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE q (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO q VALUES (1)"
        expectErrorKind ErrorKind.NameError "テーブル名を列として解決しない" (execSql db2 "SELECT q FROM q"))

[<Fact>]
let ``SqlReference: t.* の大文字小文字は区別しない`` () =
    withSession "star-case" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE st (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO st VALUES (1)"
        let _, r = ok (execSql db2 "SELECT ST.* FROM st")
        Assert.Equal<string list>([ "st.id" ], r.Columns |> Array.toList))

[<Fact>]
let ``SqlReference: FROM に無い t.* は NameError`` () =
    withSession "star-unknown" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1)"
        // 一致 0 件の `t.*` を「0列の 1行」で成功させない。
        expectErrorKind ErrorKind.NameError "未知のテーブル名の t.*" (execSql db2 "SELECT nope.* FROM t")
        expectErrorKind ErrorKind.NameError "別名を実名で指定した t.*" (execSql db2 "SELECT t.* FROM t AS x"))

[<Fact>]
let ``SqlReference: LIMIT の定数式と負数`` () =
    withSession "limit-expr" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2), (3)"

        // 定数式は評価される
        let _, one = ok (execSql db2 "SELECT id FROM t LIMIT 1+0")
        Assert.Equal<int>(1, one.Rows.Length)

        let _, offset = ok (execSql db2 "SELECT id FROM t LIMIT 2 OFFSET 1+0")
        Assert.Equal<int>(2, offset.Rows.Length)

        expectErrorKind ErrorKind.SyntaxError "LIMIT -1 はエラー" (execSql db2 "SELECT id FROM t LIMIT -1")
        expectError "LIMIT に列参照は使えない" (execSql db2 "SELECT id FROM t LIMIT id")

        // LIMIT 0 は 0 行
        let _, none = ok (execSql db2 "SELECT id FROM t LIMIT 0")
        Assert.Empty none.Rows)

[<Fact>]
let ``SqlReference: DEFAULT の評価エラーは NULL にしない`` () =
    withSession "default-error" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER DEFAULT (1 / 0))"

        expectError "壊れた DEFAULT はエラーになる" (execSql db1 "INSERT INTO t (id) VALUES (1)"))

[<Fact>]
let ``SqlReference: DEFAULT は列型へ暗黙変換される`` () =
    withSession "default-coerce" (fun db0 ->
        // INTEGER の既定値を TEXT 列へ入れると TEXT に変換される
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT DEFAULT 42)"
        let db2, _ = ok (execSql db1 "INSERT INTO t (id) VALUES (1)")
        let _, rows = ok (execSql db2 "SELECT v FROM t")
        Assert.Equal<string>("42", asText rows.Rows.[0].[0])

        // 変換できない既定値は型エラー（TEXT のまま保存しない）
        let db3 =
            exec db2 "CREATE TABLE u (id INTEGER PRIMARY KEY, v INTEGER DEFAULT 'oops')"

        expectErrorKind ErrorKind.TypeError "INTEGER 列の変換できない DEFAULT" (execSql db3 "INSERT INTO u (id) VALUES (1)"))

[<Fact>]
let ``SqlReference: DEFAULT は制約の順序に依存しない`` () =
    // `DEFAULT 0 NOT NULL` の NOT を否定の述語として読み、構文エラーになっていた。
    withSession "default-order" (fun db0 ->
        let db1 =
            exec
                db0
                "CREATE TABLE t (id INTEGER PRIMARY KEY, x INTEGER DEFAULT 0 NOT NULL, y TEXT NOT NULL DEFAULT 'n')"

        let db2, _ = ok (execSql db1 "INSERT INTO t (id) VALUES (1)")
        let _, rows = ok (execSql db2 "SELECT x, y FROM t")
        Assert.Equal<int64>(0L, asInt rows.Rows.[0].[0])
        Assert.Equal<string>("n", asText rows.Rows.[0].[1])

        // ALTER ... ADD の既定値でも同じ順序を受理する
        let db3 = exec db2 "ALTER TABLE t ADD COLUMN z INTEGER DEFAULT 5 NOT NULL"
        let _, altered = ok (execSql db3 "SELECT z FROM t WHERE id = 1")
        Assert.Equal<int64>(5L, asInt altered.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: DEFAULT に非有限の REAL は指定できない`` () =
    // 既定式はカタログへ表示文字列で保存するため、`Infinity` / `NaN` へ表示される
    // リテラルは再オープン後に読み戻せない。
    withSession "default-nonfinite" (fun db0 ->
        expectErrorKind
            ErrorKind.ConstraintViolation
            "DEFAULT 1e999"
            (execSql db0 "CREATE TABLE d (id INTEGER PRIMARY KEY, x REAL DEFAULT 1e999)")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "DEFAULT -1e999"
            (execSql db0 "CREATE TABLE d (id INTEGER PRIMARY KEY, x REAL DEFAULT -1e999)")

        let db1 = exec db0 "CREATE TABLE d (id INTEGER PRIMARY KEY, x REAL DEFAULT 1e10)"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "ALTER の DEFAULT 1e999"
            (execSql db1 "ALTER TABLE d ADD COLUMN y REAL DEFAULT 1e999"))

[<Fact>]
let ``SqlReference: 非有限 REAL でも式で書いた DEFAULT は再オープン後も使える`` () =
    withDir "default-nonfinite-e2e" (fun dir ->
        let db0 = openDb dir

        let db1 =
            exec db0 "CREATE TABLE d (id INTEGER PRIMARY KEY, x REAL DEFAULT CAST('NaN' AS REAL))"

        let db2 = exec db1 "INSERT INTO d (id) VALUES (1)"
        let _, first = ok (execSql db2 "SELECT x FROM d WHERE id = 1")

        match first.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(System.Double.IsNaN f)
        | other -> failwithf "REAL ではありません: %A" other

        ok (db2.Close())

        let db3 = openDb dir
        let db4 = exec db3 "INSERT INTO d (id) VALUES (2)"
        let _, second = ok (execSql db4 "SELECT x FROM d WHERE id = 2")

        match second.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(System.Double.IsNaN f)
        | other -> failwithf "REAL ではありません: %A" other

        ok (db4.Close()))

[<Fact>]
let ``SqlReference: 複合索引の全列等値で行を引ける`` () =
    withSession "comp-index-eq" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 2, 10), (1, 3, 11), (2, 2, 12)"
        let db3 = exec db2 "CREATE INDEX ix_ab ON t (a, b)"

        // 索引の backfill 後、全列等値で 1 行だけ返る
        let _, r = ok (execSql db3 "SELECT id FROM t WHERE a = 1 AND b = 2")
        Assert.Equal<int>(1, r.Rows.Length)
        Assert.Equal<int64>(10L, asInt r.Rows.[0].[0])

        // 前置 1列だけでも IndexLookup（結果は同じ）
        let _, prefix = ok (execSql db3 "SELECT id FROM t WHERE a = 1")
        Assert.Equal<int>(2, prefix.Rows.Length))

[<Fact>]
let ``SqlReference: txn 内の索引スキャンはキー順にマージされ LIMIT でも正しい行を返す`` () =
    withSession "index-txn-merge-order" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id TEXT PRIMARY KEY, g INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES ('aaaaaaaaa', 1), ('aaab', 1)"
        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        let db4 = exec db3 "BEGIN"
        let db5 = exec db4 "INSERT INTO t VALUES ('b', 1), ('aaaaaaaaaaaa', 1)"

        // 索引キーは辞書順で aaaaaaaaa < aaaaaaaaaaaa < aaab < b。
        // txn スロットを Map 順（長さ優先）のままマージすると LIMIT 2 が誤る。
        let _, r = ok (execSql db5 "SELECT id FROM t WHERE g = 1 LIMIT 2")

        let ids =
            r.Rows
            |> Array.map (fun row ->
                match row.[0] with
                | SqlValue.Text s -> s
                | _ -> failwithf "TEXT ではありません: %A" row.[0])

        Assert.Equal<string[]>([| "aaaaaaaaa"; "aaaaaaaaaaaa" |], ids)
        exec db5 "ROLLBACK" |> ignore)

[<Fact>]
let ``SqlReference: DECIMAL(10, 2) は再オープン後も使える`` () =
    withDir "typeargs-reopen" (fun dir ->
        let db0 = openDb dir
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v DECIMAL(10, 2))"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 1.5)"
        ok (db2.Close())

        // 型引数はカタログに保存されないが、列型（DECIMAL）と値はそのまま使える
        let db3 = openDb dir
        let _, r = ok (execSql db3 "SELECT v FROM t")
        Assert.Equal<SqlValue>(SqlValue.Decimal 1.5M, r.Rows.[0].[0])
        ok (db3.Close()))

[<Fact>]
let ``SqlReference: SQL の BEGIN / COMMIT / ROLLBACK は受理される`` () =
    // 文として受理され、暗黙の transaction を制御する（§12）。
    // `Session` は SQL の BEGIN / COMMIT / ROLLBACK をライブラリ API と同じ状態機械で扱う。
    withSession "tx5" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "BEGIN"
        Assert.True db2.InTransaction
        let db3 = exec db2 "INSERT INTO t VALUES (1)"
        let db4 = exec db3 "COMMIT"
        let db5, res = ok (execSql db4 "SELECT id FROM t")
        Assert.Equal<int>(1, res.Rows.Length)
        Assert.False db5.InTransaction

        // transaction が無い状態の ROLLBACK は何もしない（後始末を失敗させない）
        let db6, _ = ok (execSql db5 "ROLLBACK")
        Assert.False db6.InTransaction
        let _, after = ok (execSql db6 "SELECT id FROM t")
        Assert.Equal<int>(1, after.Rows.Length))

[<Fact>]
let ``SqlReference: CREATE TABLE でテーブルができる`` () =
    withSession "ddl1" (fun db0 ->
        let db =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        Assert.Equal<string list>([ "people" ], ok (db.TableNames())))

[<Fact>]
let ``SqlReference: CREATE TABLE IF NOT EXISTS は 2 回呼んでも OK`` () =
    withSession "ddl2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"

        let db2 =
            exec db1 "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY, name TEXT)"

        Assert.Equal<string list>([ "people" ], ok (db2.TableNames())))

[<Fact>]
let ``SqlReference: CREATE TABLE の重複はエラー（IF NOT EXISTS なし）`` () =
    withSession "ddl3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"

        expectError
            "重複した CREATE TABLE が OK になっています"
            (execSql db1 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"))

[<Fact>]
let ``SqlReference: テーブル制約の複合主キー`` () =
    withSession "ddl6" (fun db0 ->
        // 主キーをテーブル制約で指定（列制約ではない）
        let db1 =
            exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, v INTEGER, PRIMARY KEY (a, b))"

        let db2, ins = ok (execSql db1 "INSERT INTO t (a, b) VALUES (1, 2), (1, 3)")
        Assert.Equal<int>(2, ins.RowsAffected)

        // 同じ a でも b が違えば別の行
        let _, res = ok (execSql db2 "SELECT a, b FROM t ORDER BY b")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: 複合主キーは全列一致で検索でき、部分一致も取りこぼさない`` () =
    withSession "ddl6-search" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, v TEXT, PRIMARY KEY (a, b))"

        let db2 = exec db1 "INSERT INTO t VALUES (1, 2, 'x'), (1, 3, 'y'), (2, 2, 'z')"

        let _, full = ok (execSql db2 "SELECT v FROM t WHERE a = 1 AND b = 2")
        Assert.Equal<int>(1, full.Rows.Length)
        Assert.Equal<string>("x", asText full.Rows.[0].[0])

        let _, partialA = ok (execSql db2 "SELECT v FROM t WHERE a = 1 ORDER BY b")
        Assert.Equal<string list>([ "x"; "y" ], partialA.Rows |> Array.map (fun r -> asText r.[0]) |> List.ofArray)

        let _, partialB = ok (execSql db2 "SELECT v FROM t WHERE b = 2 ORDER BY a")
        Assert.Equal<string list>([ "x"; "z" ], partialB.Rows |> Array.map (fun r -> asText r.[0]) |> List.ofArray)

        // 型が違うリテラルは黙って 0 行にせず、比較時に型エラーにする
        expectErrorKind ErrorKind.TypeError "型不一致の主キー検索" (execSql db2 "SELECT v FROM t WHERE a = '1'"))

[<Fact>]
let ``SqlReference: DROP TABLE でテーブルが消える`` () =
    withSession "ddl4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"
        let db2 = exec db1 "DROP TABLE people"
        Assert.Empty(ok (db2.TableNames())))

[<Fact>]
let ``SqlReference: DROP TABLE IF EXISTS は無ければ OK`` () =
    withSession "ddl5" (fun db0 ->
        let db1 = exec db0 "DROP TABLE IF EXISTS nothing"
        Assert.Empty(ok (db1.TableNames())))

[<Fact>]
let ``SqlReference: 存在しない列の索引はエラー`` () =
    withSession "ddl7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"
        expectError "存在しない列の CREATE INDEX が OK になっています" (execSql db1 "CREATE INDEX ix ON people (nope)"))

[<Fact>]
let ``SqlReference: 存在しないテーブルの索引と索引名の重複はエラー`` () =
    withSession "ddl8" (fun db0 ->
        expectError "存在しないテーブルの CREATE INDEX が OK になっています" (execSql db0 "CREATE INDEX ix ON nope (a)")

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_a ON t (a)"
        expectError "同名の CREATE INDEX が OK になっています" (execSql db2 "CREATE INDEX ix_a ON t (a)"))

[<Fact>]
let ``SqlReference: DROP INDEX で索引が消える`` () =
    withSession "ddl9" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2, _ = execRes db1 "INSERT INTO t VALUES (1, 10)"

        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        // 索引を使う検索が通る
        let _, found = ok (execSql db3 "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, found.Rows.Length)

        let db4 = exec db3 "DROP INDEX ix_a"
        let _, after = ok (execSql db4 "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, after.Rows.Length)

        // 消えたので同じ名前で作り直せる
        let db5 = exec db4 "CREATE INDEX ix_a ON t (a)"
        let _, again = ok (execSql db5 "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, again.Rows.Length))

[<Fact>]
let ``SqlReference: DROP INDEX IF EXISTS と存在しない索引のエラー`` () =
    withSession "ddl10" (fun db0 ->
        let db1 = exec db0 "DROP INDEX IF EXISTS nothing"
        expectError "存在しない DROP INDEX が OK になっています" (execSql db1 "DROP INDEX nothing"))

[<Fact>]
let ``SqlReference: 暗黙の UNIQUE 索引は DROP できない`` () =
    withSession "ddl-implicit-unique" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a@example.com')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "暗黙の UNIQUE 索引を DROP できてしまう"
            (execSql db2 "DROP INDEX __unique_t_0")

        // DROP に失敗しても制約は残る
        expectErrorKind
            ErrorKind.ConstraintViolation
            "暗黙 UNIQUE 索引の DROP 後に制約が消えている"
            (execSql db2 "INSERT INTO t VALUES (2, 'a@example.com')")

        // 予約名の索引はユーザーが作れない
        expectErrorKind ErrorKind.NameError "予約された索引名を許してはいけない" (execSql db2 "CREATE INDEX __unique_t_9 ON t (email)"))

[<Fact>]
let ``SqlReference: ALTER TABLE ADD COLUMN は既存行を DEFAULT で埋める`` () =
    withSession "alter-add" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"
        let db3 = exec db2 "CREATE INDEX ix_name ON t (name)"
        let db4 = exec db3 "ALTER TABLE t ADD COLUMN age INTEGER DEFAULT 0"
        let db5 = exec db4 "ALTER TABLE t ADD nn INTEGER NOT NULL DEFAULT 7"

        // 既存行は既定値で埋まる（DEFAULT なしなら NULL）
        let _, rows = ok (execSql db5 "SELECT * FROM t ORDER BY id")
        Assert.Equal<string list>([ "id"; "name"; "age"; "nn" ], rows.Columns |> Array.toList)
        Assert.Equal<int64 list>([ 0L; 0L ], rows.Rows |> Array.map (fun r -> asInt r.[2]) |> List.ofArray)
        Assert.Equal<int64 list>([ 7L; 7L ], rows.Rows |> Array.map (fun r -> asInt r.[3]) |> List.ofArray)

        // 索引経由でも新しい列が本体から fetch されて返る
        let _, byIndex = ok (execSql db5 "SELECT age, nn FROM t WHERE name = 'b'")
        Assert.Equal<int64>(0L, asInt byIndex.Rows.[0].[0])
        Assert.Equal<int64>(7L, asInt byIndex.Rows.[0].[1])

        // 新しい INSERT も既定値を使える
        let db6 = exec db5 "INSERT INTO t (id, name) VALUES (3, 'c')"
        let _, inserted = ok (execSql db6 "SELECT age, nn FROM t WHERE id = 3")
        Assert.Equal<int64>(0L, asInt inserted.Rows.[0].[0])
        Assert.Equal<int64>(7L, asInt inserted.Rows.[0].[1])

        // UPDATE も新しい列を含む行で動く
        let db7 = exec db6 "UPDATE t SET age = 5, nn = 8 WHERE id = 1"
        let _, updated = ok (execSql db7 "SELECT age, nn FROM t WHERE id = 1")
        Assert.Equal<int64>(5L, asInt updated.Rows.[0].[0])
        Assert.Equal<int64>(8L, asInt updated.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: ALTER TABLE ADD COLUMN の制約`` () =
    withSession "alter-add-errors" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"

        expectErrorKind ErrorKind.NameError "重複した列" (execSql db1 "ALTER TABLE t ADD COLUMN v INTEGER")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "PRIMARY KEY の追加"
            (execSql db1 "ALTER TABLE t ADD COLUMN k INTEGER PRIMARY KEY")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "UNIQUE の追加"
            (execSql db1 "ALTER TABLE t ADD COLUMN u INTEGER UNIQUE")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "DEFAULT なしの NOT NULL"
            (execSql db1 "ALTER TABLE t ADD COLUMN n INTEGER NOT NULL")

        expectErrorKind ErrorKind.NotFound "存在しないテーブル" (execSql db1 "ALTER TABLE nope ADD COLUMN n INTEGER")

        // 失敗した ALTER はテーブルを変えない
        let _, rows = ok (execSql db1 "SELECT * FROM t")
        Assert.Equal<string list>([ "id"; "v" ], rows.Columns |> Array.toList))

[<Fact>]
let ``SqlReference: ALTER TABLE DROP COLUMN は行と索引から列を除く`` () =
    withSession "alter-drop" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER, b TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10, 'x'), (2, 20, 'y')"
        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        let db4 = exec db3 "ALTER TABLE t DROP COLUMN b"

        let _, rows = ok (execSql db4 "SELECT * FROM t ORDER BY id")
        Assert.Equal<string list>([ "id"; "a" ], rows.Columns |> Array.toList)
        Assert.Equal<int64 list>([ 10L; 20L ], rows.Rows |> Array.map (fun r -> asInt r.[1]) |> List.ofArray)
        expectErrorKind ErrorKind.NameError "削除した列" (execSql db4 "SELECT b FROM t")

        // 索引値は空のまま（キーは変わらない）で、索引経由で正しい行が引ける
        let _, byIndex = ok (execSql db4 "SELECT id FROM t WHERE a = 20")
        Assert.Equal<int64>(2L, asInt byIndex.Rows.[0].[0])

        // 主キー列・索引列・存在しない列は削除できない
        expectErrorKind ErrorKind.ConstraintViolation "主キー列の削除" (execSql db4 "ALTER TABLE t DROP COLUMN id")
        expectErrorKind ErrorKind.ConstraintViolation "索引列の削除" (execSql db4 "ALTER TABLE t DROP COLUMN a")
        expectErrorKind ErrorKind.NameError "存在しない列の削除" (execSql db4 "ALTER TABLE t DROP COLUMN nope")

        // UNIQUE 制約の列も削除できない
        let db5 = exec db0 "CREATE TABLE u (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        expectErrorKind ErrorKind.ConstraintViolation "UNIQUE 列の削除" (execSql db5 "ALTER TABLE u DROP COLUMN email"))

[<Fact>]
let ``SqlReference: ALTER TABLE RENAME COLUMN は行と索引の列名を書き換える`` () =
    withSession "alter-rename" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10), (2, 20)"
        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        let db4 = exec db3 "ALTER TABLE t RENAME COLUMN a TO b"

        let _, rows = ok (execSql db4 "SELECT b FROM t ORDER BY b")
        Assert.Equal<int64 list>([ 10L; 20L ], rows.Rows |> Array.map (fun r -> asInt r.[0]) |> List.ofArray)
        expectErrorKind ErrorKind.NameError "旧名では引けない" (execSql db4 "SELECT a FROM t")

        // 索引の定義も新しい名前になっている（索引経由で引ける）
        let _, byIndex = ok (execSql db4 "SELECT id FROM t WHERE b = 20")
        Assert.Equal<int64>(2L, asInt byIndex.Rows.[0].[0])

        // 主キー列の名前も変えられる
        let db5 = exec db4 "ALTER TABLE t RENAME COLUMN id TO tid"
        let _, pk = ok (execSql db5 "SELECT tid FROM t WHERE tid = 1")
        Assert.Equal<int64>(1L, asInt pk.Rows.[0].[0])

        // 大小文字だけの変更もできる
        let db6 = exec db5 "ALTER TABLE t RENAME COLUMN b TO B"
        let _, upper = ok (execSql db6 "SELECT B FROM t WHERE B = 10")
        Assert.Equal<int64>(10L, asInt upper.Rows.[0].[0])

        // 重複・存在しない列
        expectErrorKind ErrorKind.NameError "重複" (execSql db6 "ALTER TABLE t RENAME COLUMN B TO tid")
        expectErrorKind ErrorKind.NameError "不在" (execSql db6 "ALTER TABLE t RENAME COLUMN nope TO x"))

[<Fact>]
let ``SqlReference: ANALYZE はテーブルの統計を再収集する`` () =
    withSession "analyze" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,10),(3,20)"
        let db3, analyzed = execRes db2 "ANALYZE t"
        Assert.Equal<int>(1, analyzed.Rows.Length)
        Assert.Equal<string>("t", asText analyzed.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt analyzed.Rows.[0].[1])
        // 列数と NDV の表示
        Assert.Equal<int64>(2L, asInt analyzed.Rows.[0].[2])
        Assert.Contains("id=3", asText analyzed.Rows.[0].[3])
        // NULL 数・数値範囲・索引 prefix NDV・ヒストグラム・収集時刻
        Assert.Equal<string list>(
            [
                "table"
                "rows"
                "columns"
                "ndv"
                "nulls"
                "ranges"
                "indexes"
                "histograms"
                "mcv"
                "row_bytes"
                "analyzed_at"
            ],
            analyzed.Columns |> Array.toList
        )

        Assert.Equal<string>("id=0,v=0", asText analyzed.Rows.[0].[4])
        Assert.Equal<string>("id=[1,3],v=[10,20]", asText analyzed.Rows.[0].[5])
        // 数値列は等頻度ヒストグラムを持つ。
        Assert.Contains("id:", asText analyzed.Rows.[0].[7])
        Assert.Contains("v:", asText analyzed.Rows.[0].[7])
        // 2回以上現れた値は頻出値として残す（v は 10 が 2回）。
        Assert.Equal<string>("v:1", asText analyzed.Rows.[0].[8])
        // 1行の管理メモリ概算（型別サイズ + 24B ヘッダ）が入る。
        // INTEGER 2列 = 24 + 2*8 + 24 + 24。
        Assert.Equal<int64>(88L, asInt analyzed.Rows.[0].[9])
        // 収集時刻が入る。
        Assert.False(System.String.IsNullOrEmpty(asText analyzed.Rows.[0].[10]))

        let _ = exec db3 "ANALYZE TABLE t;"
        let _ = exec db3 "ANALYZE t"

        // 名前を省略すると全テーブル
        let db4 = exec db3 "CREATE TABLE u (id INTEGER PRIMARY KEY, w INTEGER)"
        let db5, all = execRes db4 "ANALYZE"
        Assert.Equal<int>(2, all.Rows.Length)

        // SAMPLE は標本から NDV を集めるが、行数は全体を数える。
        let _, sampled = execRes db5 "ANALYZE t SAMPLE 2"
        Assert.Equal<int>(1, sampled.Rows.Length)
        Assert.Equal<int64>(3L, asInt sampled.Rows.[0].[1])

        // テーブル名を省略した SAMPLE は全テーブルの標本解析
        let _, sampledAll = execRes db5 "ANALYZE SAMPLE 2"
        Assert.Equal<int>(2, sampledAll.Rows.Length)

        // 存在しないテーブルは NameError
        expectErrorKind ErrorKind.NotFound "存在しないテーブル" (execSql db5 "ANALYZE nope")
        // DDL と同じく transaction 中は実行できない
        let db6 = exec db5 "BEGIN"
        expectErrorKind ErrorKind.SyntaxError "transaction 中の ANALYZE" (execSql db6 "ANALYZE t")
        exec db6 "ROLLBACK" |> ignore)

[<Fact>]
let ``SqlReference: ANALYZE は進捗を通知し、キャンセルできる`` () =
    withDir "analyze-cancel" (fun dir ->
        let progress = ResizeArray<string * int64>()

        let options =
            { SessionOptions.Default with
                OnAnalyzeProgress = Some(fun item -> progress.Add item)
            }

        let s1 = ok (Session.OpenWithSync(dir, options))
        let s1 = exec s1 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let s1 = exec s1 "INSERT INTO t VALUES (1,10),(2,20)"
        let s1 = exec s1 "ANALYZE t"
        Assert.NotEmpty progress
        ok (s1.Close()) |> ignore

        use cts = new System.Threading.CancellationTokenSource()
        cts.Cancel()

        let options =
            { SessionOptions.Default with
                CancelAnalysis = cts.Token
            }

        let s2 = ok (Session.OpenWithSync(dir, options))
        expectErrorKind ErrorKind.CapacityError "キャンセル済みの ANALYZE" (execSql s2 "ANALYZE t")
        ok (s2.Close()) |> ignore)

[<Fact>]
let ``SqlReference: EXPLAIN は統計があるとき推定行数と結合選択性を表示する`` () =
    withSession "explain-stats" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE u (id INTEGER PRIMARY KEY, k INTEGER)"
        let db3 = exec db2 "INSERT INTO t VALUES (1,10,100),(2,20,200),(3,30,300)"
        let db4 = exec db3 "INSERT INTO u VALUES (1,100),(2,200),(3,300)"

        // 統計が無いときは行数・選択性を出さない。
        let _, before = execRes db4 "EXPLAIN SELECT * FROM t JOIN u ON t.k = u.k"

        let beforeText =
            before.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        Assert.DoesNotContain("rows=", beforeText)
        Assert.DoesNotContain("sel=", beforeText)

        let db5 = exec db4 "ANALYZE"
        let _, after = execRes db5 "EXPLAIN SELECT * FROM t JOIN u ON t.k = u.k"
        let afterLines = after.Rows |> Array.map (fun row -> asText row.[0])
        let afterText = afterLines |> String.concat "\n"
        Assert.Contains("rows=", afterText)
        Assert.Contains("sel=", afterText)
        // 選択性は結合ノードにだけ付く。
        Assert.DoesNotContain(
            "sel=",
            afterLines
            |> Array.filter (fun line -> line.Contains "Scan(")
            |> String.concat "\n"
        ))

[<Fact>]
let ``SqlReference: ANALYZE の統計は再オープン後も残り、ALTER TABLE で消える`` () =
    withDir "analyze-reopen" (fun dir ->
        let s1 = ok (Session.OpenWithSync(dir, SessionOptions.Default))
        let s1 = exec s1 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let s1 = exec s1 "INSERT INTO t VALUES (1,10),(2,10),(3,20)"
        let s1 = exec s1 "UPDATE t SET v = 30 WHERE id = 2"
        let s1 = exec s1 "DELETE FROM t WHERE id = 1"
        let s1 = exec s1 "ANALYZE t"
        ok (s1.Close()) |> ignore

        let store = ok (RowStore.openStore dir)
        let loaded = ok (Catalog.load store)
        let stats = expectSome "t の統計" (Catalog.findStats "t" loaded)
        Assert.Equal<int64>(2L, stats.RowCount)
        let idColumn = stats.Columns |> List.find (fun c -> c.Name = "id")
        let vColumn = stats.Columns |> List.find (fun c -> c.Name = "v")
        Assert.Equal<int64>(2L, idColumn.NDV)
        Assert.Equal<int64>(2L, vColumn.NDV)
        Assert.False(List.isEmpty vColumn.Histogram)
        ok (RowStore.close store) |> ignore

        // ALTER TABLE は統計を削除する（行の形が変わるため）。
        let s2 = ok (Session.OpenWithSync(dir, SessionOptions.Default))
        let s2 = exec s2 "ALTER TABLE t ADD COLUMN x INTEGER"
        ok (s2.Close()) |> ignore

        let store2 = ok (RowStore.openStore dir)
        let loaded2 = ok (Catalog.load store2)
        Assert.True(Catalog.findStats "t" loaded2 |> Option.isNone)
        ok (RowStore.close store2) |> ignore)

[<Fact>]
let ``SqlReference: ALTER TABLE は transaction 中に実行できない`` () =
    withSession "alter-txn" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"

        ok (db1.Begin())
        expectErrorKind ErrorKind.SyntaxError "transaction 中の ALTER" (execSql db1 "ALTER TABLE t ADD COLUMN v INTEGER")

        ok (db1.Rollback())
        let db2 = exec db1 "ALTER TABLE t ADD COLUMN v INTEGER"
        let _, rows = ok (execSql db2 "SELECT * FROM t")
        Assert.Equal<string list>([ "id"; "v" ], rows.Columns |> Array.toList))

[<Fact>]
let ``SqlReference: ALTER TABLE の変更は再オープン後も残る`` () =
    withDir "alter-reopen" (fun dir ->
        let db0 = openDb dir
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER, x TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10, 'x'), (2, 20, 'y')"
        let db3 = exec db2 "ALTER TABLE t ADD COLUMN n INTEGER NOT NULL DEFAULT 7"
        let db4 = exec db3 "ALTER TABLE t RENAME COLUMN v TO val"
        let db5 = exec db4 "ALTER TABLE t DROP COLUMN x"
        ok (db5.Close())

        let db6 = openDb dir
        let _, rows = ok (execSql db6 "SELECT * FROM t ORDER BY id")
        Assert.Equal<string list>([ "id"; "val"; "n" ], rows.Columns |> Array.toList)
        Assert.Equal<int64 list>([ 7L; 7L ], rows.Rows |> Array.map (fun r -> asInt r.[2]) |> List.ofArray)

        let db7 = exec db6 "INSERT INTO t (id, val) VALUES (3, 30)"
        let _, inserted = ok (execSql db7 "SELECT n FROM t WHERE id = 3")
        Assert.Equal<int64>(7L, asInt inserted.Rows.[0].[0])
        ok (db7.Close()))

[<Fact>]
let ``SqlReference: UNIQUE 制約は複数の NULL を許す`` () =
    withSession "unique-null" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE u (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO u VALUES (1, NULL), (2, NULL)"
        let _, count = ok (execSql db2 "SELECT COUNT(*) FROM u")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0])

        expectErrorKind
            ErrorKind.ConstraintViolation
            "UNIQUE の非 NULL 重複を許してはいけない"
            (execSql db2 "INSERT INTO u VALUES (3, 'x'), (4, 'x')"))

[<Fact>]
let ``SqlReference: 複合 UNIQUE は NULL を含む行を重複扱いしない`` () =
    withSession "unique-null-composite" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE u (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, UNIQUE (a, b))"

        let db2 = exec db1 "INSERT INTO u VALUES (1, NULL, 1), (2, NULL, 2)"
        let _, count = ok (execSql db2 "SELECT COUNT(*) FROM u")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0])

        expectErrorKind
            ErrorKind.ConstraintViolation
            "複合 UNIQUE の非 NULL 重複を許してはいけない"
            (execSql db2 "INSERT INTO u VALUES (3, 7, 1), (4, 7, 1)"))

[<Fact>]
let ``SqlReference: UNIQUE 列の UPDATE でも NULL は重複扱いしない`` () =
    withSession "unique-null-update" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "UNIQUE 列の UPDATE で非 NULL 重複を許してはいけない"
            (execSql db2 "UPDATE t SET email = 'x'")

        let db3 = exec db2 "UPDATE t SET email = NULL"
        let _, count = ok (execSql db3 "SELECT COUNT(*) FROM t WHERE email IS NULL")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: UPDATE で UNIQUE 値を交換できる`` () =
    withSession "unique-update-swap" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        // 更新対象外の行との衝突は即時エラー
        expectErrorKind
            ErrorKind.ConstraintViolation
            "更新対象外の UNIQUE 値との衝突を許してはいけない"
            (execSql db2 "UPDATE t SET email = 'b' WHERE id = 1")

        // 同一文内で入れ替える更新は最終状態が一意なので成功する
        let db3 = exec db2 "UPDATE t SET email = CASE WHEN id = 1 THEN 'b' ELSE 'a' END"
        let _, rows = ok (execSql db3 "SELECT id, email FROM t ORDER BY id")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0])
        Assert.Equal<string>("b", asText rows.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt rows.Rows.[1].[0])
        Assert.Equal<string>("a", asText rows.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: UPDATE で UNIQUE 値を 3行で回転できる`` () =
    withSession "unique-update-rotate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, u INTEGER UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)"

        // a -> b, b -> c, c -> a の回転（全行が更新対象なので衝突しない）
        let db3 =
            exec db2 "UPDATE t SET u = CASE WHEN id = 1 THEN 20 WHEN id = 2 THEN 30 ELSE 10 END"

        let _, rows = ok (execSql db3 "SELECT u FROM t ORDER BY id")
        Assert.Equal<int64 list>([ 20L; 30L; 10L ], rows.Rows |> Array.toList |> List.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: 明示 transaction の INSERT は前の文の UNIQUE 値も見る`` () =
    // 文単位ガードの BaseSlots（文開始時の txn スロット）経由で衝突を検出する。
    withSession "unique-prior-statement" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "BEGIN"
        let db3 = exec db2 "INSERT INTO t VALUES (1, 'a')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "前の文で挿入した UNIQUE 値との衝突"
            (execSql db3 "INSERT INTO t VALUES (2, 'a')")

        // 同じ文なら PK が違っても重複を検出する（Added 経由）。
        expectErrorKind
            ErrorKind.ConstraintViolation
            "同一文内の UNIQUE 重複"
            (execSql db3 "INSERT INTO t VALUES (3, 'b'), (4, 'b')")

        let db4 = exec db3 "ROLLBACK"
        let _, rows = ok (execSql db4 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: BLOB 索引の等値検索は NUL 継続値を返さない`` () =
    // TEXT/BLOB の NUL は 0x00 0xFF にエスケープされるため、生の接頭辞一致だけでは
    // 「短い値」の索引検索が「短い値 + NUL + 続き」の行まで拾ってしまう。
    withSession "index-blob-nul" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v BLOB, note TEXT)"
        let db2 = exec db1 "CREATE INDEX iv ON t (v)"
        let db3 = exec db2 "INSERT INTO t VALUES (1, X'61', 'one'), (2, X'610062', 'two')"

        // 被覆索引（id + v）と非被覆索引（note の fetch あり）の両方。
        let _, covered = ok (execSql db3 "SELECT id FROM t WHERE v = X'61' ORDER BY id")
        Assert.Equal<int64 list>([ 1L ], covered.Rows |> Array.toList |> List.map (fun r -> asInt r.[0]))

        let _, fetched = ok (execSql db3 "SELECT note FROM t WHERE v = X'61'")
        Assert.Equal<int>(1, fetched.Rows.Length)
        Assert.Equal<string>("one", asText fetched.Rows.[0].[0])

        let _, count = ok (execSql db3 "SELECT COUNT(*) FROM t WHERE v = X'61'")
        Assert.Equal<int64>(1L, asInt count.Rows.[0].[0])

        // より長い値の完全一致は引き続き引ける。
        let _, longer = ok (execSql db3 "SELECT id FROM t WHERE v = X'610062'")
        Assert.Equal<int64 list>([ 2L ], longer.Rows |> Array.toList |> List.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: BLOB の UNIQUE は NUL 継続値を重複と見なさない`` () =
    withSession "unique-blob-nul" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v BLOB UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, X'610062')"
        let db3 = exec db2 "INSERT INTO t VALUES (2, X'61')"

        let _, count = ok (execSql db3 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0])

        // 同じ値の重複は引き続き拒否する。
        expectErrorKind
            ErrorKind.ConstraintViolation
            "同じ BLOB 値の UNIQUE 重複"
            (execSql db3 "INSERT INTO t VALUES (3, X'61')"))

[<Fact>]
let ``SqlReference: 外部キーの子行検索は NUL 継続値を一致と見なさない`` () =
    withSession "fk-blob-nul" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id BLOB PRIMARY KEY)"

        let db2 =
            exec db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid BLOB REFERENCES p(id))"

        let db3 = exec db2 "CREATE INDEX ci ON c (pid)"
        let db4 = exec db3 "INSERT INTO p VALUES (X'61'), (X'610062')"
        let db5 = exec db4 "INSERT INTO c VALUES (1, X'610062')"

        // 子が参照しているのは X'610062' だけなので、X'61' は削除できる。
        let db6 = exec db5 "DELETE FROM p WHERE id = X'61'"
        let _, count = ok (execSql db6 "SELECT COUNT(*) FROM p")
        Assert.Equal<int64>(1L, asInt count.Rows.[0].[0])

        // 実際に参照されている親は削除できない。
        expectErrorKind
            ErrorKind.ConstraintViolation
            "子行がある親の DELETE"
            (execSql db6 "DELETE FROM p WHERE id = X'610062'"))

[<Fact>]
let ``SqlReference: UNIQUE 列を代入しても値が変わらなければ通る`` () =
    withSession "unique-update-same" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'A'), (2, 'B')"

        // 同じ値に戻す更新（索引列は代入対象だが全行が変化しない）
        let db3 = exec db2 "UPDATE t SET email = email"
        let _, rows = ok (execSql db3 "SELECT email FROM t ORDER BY id")
        Assert.Equal<string list>([ "A"; "B" ], rows.Rows |> Array.toList |> List.map (fun r -> asText r.[0])))

[<Fact>]
let ``SqlReference: table-level PRIMARY KEY は NULL を拒否する`` () =
    withSession "table-pk-not-null" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER, v TEXT, PRIMARY KEY (id))"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "table-level PRIMARY KEY の NULL を許してはいけない"
            (execSql db1 "INSERT INTO t VALUES (NULL, 'x')"))

[<Fact>]
let ``SqlReference: 重複列・重複 INSERT 列・重複 UPDATE 代入を拒否する`` () =
    withSession "duplicate-names" (fun db0 ->
        expectErrorKind
            ErrorKind.NameError
            "CREATE TABLE の重複列を許してはいけない"
            (execSql db0 "CREATE TABLE dup (id INTEGER PRIMARY KEY, a INTEGER, A TEXT)")

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"

        expectErrorKind
            ErrorKind.NameError
            "INSERT の重複列を許してはいけない"
            (execSql db1 "INSERT INTO t (id, a, a) VALUES (1, 2, 3)")

        expectErrorKind ErrorKind.NameError "UPDATE の重複代入を許してはいけない" (execSql db1 "UPDATE t SET a = 1, a = 2")

        expectErrorKind
            ErrorKind.NameError
            "CREATE INDEX の重複列を許してはいけない"
            (execSql db1 "CREATE INDEX ix_dup ON t (a, a)"))

[<Fact>]
let ``SqlReference: 長すぎるテーブル名・索引名は拒否する`` () =
    withSession "long-name" (fun db0 ->
        let name = System.String('a', 128 + 1)

        expectErrorKind
            ErrorKind.NameError
            "長すぎるテーブル名を許してはいけない"
            (execSql db0 (sprintf "CREATE TABLE %s (id INTEGER PRIMARY KEY)" name))

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"

        expectErrorKind ErrorKind.NameError "長すぎる索引名を許してはいけない" (execSql db1 (sprintf "CREATE INDEX %s ON t (id)" name)))

[<Fact>]
let ``SqlReference: DEFAULT 付きの列定義も保存できる`` () =
    withSession "ddl-default" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT DEFAULT 'x')"

        // 保存時に DEFAULT 式が書かれる（再読込後に既定値として使われるかは別問題）
        let _, rows = ok (execSql db1 "SELECT id, v FROM t")
        Assert.Empty rows.Rows)

[<Fact>]
let ``SqlReference: DEFAULT は再オープン後も既定値として使われる`` () =
    withDir "default-e2e" (fun dir ->
        let db0 = openDb dir

        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER DEFAULT 42, s TEXT DEFAULT 'hi')"

        let db2 = exec db1 "INSERT INTO t (id) VALUES (1)"
        let _, before = ok (execSql db2 "SELECT v, s FROM t WHERE id = 1")
        Assert.Equal<int64>(42L, asInt before.Rows.[0].[0])
        Assert.Equal<string>("hi", asText before.Rows.[0].[1])
        ok (db2.Close())

        let db3 = openDb dir
        let _, after = ok (execSql db3 "SELECT v, s FROM t WHERE id = 1")
        Assert.Equal<int64>(42L, asInt after.Rows.[0].[0])
        Assert.Equal<string>("hi", asText after.Rows.[0].[1])

        let db4 = exec db3 "INSERT INTO t (id) VALUES (2)"
        let _, inserted = ok (execSql db4 "SELECT v, s FROM t WHERE id = 2")
        Assert.Equal<int64>(42L, asInt inserted.Rows.[0].[0])
        Assert.Equal<string>("hi", asText inserted.Rows.[0].[1])
        ok (db4.Close()))

[<Fact>]
let ``SqlReference: 数値の範囲外は例外ではなくエラーを返す`` () =
    withSession "numeric-e2e" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v REAL, d DECIMAL)"
        let db2 = exec db1 "INSERT INTO t (id, v, d) VALUES (1, 1e30, 7.9e28)"

        // REAL は decimal の範囲を超えてもそのまま計算できる
        let _, big = ok (execSql db2 "SELECT (v * v) FROM t WHERE id = 1")

        match big.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(f > 1e59 && f < 1e61, sprintf "想定外の REAL: %f" f)
        | v -> failActual "REAL ではありません" v

        // 極小の REAL も 0 に丸めない
        let _, tiny = ok (execSql db2 "SELECT (1e-30 * 1.0) FROM t WHERE id = 1")

        match tiny.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(f > 0.0 && f < 1e-29, sprintf "想定外の REAL: %f" f)
        | v -> failActual "REAL ではありません" v

        let _, sum = ok (execSql db2 "SELECT SUM(v) FROM t")

        match sum.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.Equal(1e30, f)
        | v -> failActual "REAL ではありません" v

        // 整数・DECIMAL のオーバーフローは DbError になる
        expectErrorKind
            ErrorKind.ConstraintViolation
            "整数加算のオーバーフロー"
            (execSql db2 "SELECT 9223372036854775807 + 1 FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "整数除算のオーバーフロー"
            (execSql db2 "SELECT (-9223372036854775807 - 1) / -1 FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "ABS のオーバーフロー"
            (execSql db2 "SELECT ABS(-9223372036854775807 - 1) FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "DECIMAL 乗算のオーバーフロー"
            (execSql db2 "SELECT (d * d) FROM t WHERE id = 1")

        // DECIMAL の範囲外の REAL は変換時に型エラー
        expectErrorKind ErrorKind.TypeError "DECIMAL の範囲外" (execSql db2 "INSERT INTO t (id, d) VALUES (2, 1e30)"))

[<Fact>]
let ``SqlReference: INSERT した行が SELECT で見える`` () =
    withSession "dml1" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, ins =
            ok (execSql db1 "INSERT INTO people VALUES (1, 'alice', 30), (2, 'bob', 25)")

        Assert.Equal<int>(2, ins.RowsAffected)

        let _db1, res = ok (execSql db2 "SELECT id, name FROM people ORDER BY id")
        Assert.Equal<string array>([| "id"; "name" |], res.Columns)
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<string>("alice", asText res.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: INSERT は複数 VALUES を 1 文で`` () =
    withSession "dml2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2, ins = ok (execSql db1 "INSERT INTO t VALUES (1,'a'),(2,'b'),(3,'c')")
        Assert.Equal<int>(3, ins.RowsAffected)

        let _db2, res = ok (execSql db2 "SELECT id FROM t")
        Assert.Equal<int>(3, res.Rows.Length))

[<Fact>]
let ``SqlReference: INSERT ... SELECT は選択結果を挿入する`` () =
    withSession "insert-select" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE src (id INTEGER PRIMARY KEY, v TEXT, n INTEGER)"

        let db2 = exec db1 "INSERT INTO src VALUES (1, 'a', 10), (2, 'b', 20), (3, 'c', 30)"

        let db3 =
            exec db2 "CREATE TABLE dst (id INTEGER PRIMARY KEY, v TEXT, n INTEGER DEFAULT 0)"

        // 列リストあり・式・WHERE・ORDER BY・UPPER。列リストに無い n は DEFAULT で埋まる。
        let db4, ins =
            ok (execSql db3 "INSERT INTO dst (id, v) SELECT n + 100, UPPER(v) FROM src WHERE id <= 2 ORDER BY id")

        Assert.Equal<int>(2, ins.RowsAffected)

        let _db4, rows = ok (execSql db4 "SELECT id, v, n FROM dst ORDER BY id")
        Assert.Equal<int>(2, rows.Rows.Length)
        Assert.Equal<int64>(110L, asInt rows.Rows.[0].[0])
        Assert.Equal<string>("A", asText rows.Rows.[0].[1])
        Assert.Equal<int64>(0L, asInt rows.Rows.[0].[2])
        Assert.Equal<int64>(120L, asInt rows.Rows.[1].[0])

        // FROM なしの SELECT も挿入できる
        let db5, one = ok (execSql db4 "INSERT INTO dst (id, v) SELECT 500, 'x'")
        Assert.Equal<int>(1, one.RowsAffected)

        // 自分自身から選ぶ（文の開始時点の snapshot を読み、増え続けない）
        let db6, copied =
            ok (execSql db5 "INSERT INTO src SELECT id + 10, v || '2', n FROM src")

        Assert.Equal<int>(3, copied.RowsAffected)

        let _, count = ok (execSql db6 "SELECT COUNT(*) FROM src")
        Assert.Equal<int64>(6L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: INSERT ... SELECT の制約違反は文全体を取り消す`` () =
    withSession "insert-select-errors" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT NOT NULL, n INTEGER DEFAULT 0)"

        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a', 10)"

        let countOf db =
            ok (execSql db "SELECT COUNT(*) FROM t") |> snd |> fun r -> asInt r.Rows.[0].[0]

        // 主キー重複
        expectErrorKind
            ErrorKind.ConstraintViolation
            "INSERT ... SELECT の主キー重複"
            (execSql db2 "INSERT INTO t (id) SELECT 1")

        Assert.Equal<int64>(1L, countOf db2)

        // NOT NULL 違反（DEFAULT の無い列を NULL にしてしまう）
        expectErrorKind
            ErrorKind.ConstraintViolation
            "INSERT ... SELECT の NOT NULL 違反"
            (execSql db2 "INSERT INTO t (id) SELECT 2")

        Assert.Equal<int64>(1L, countOf db2)

        // 列数の不一致
        expectErrorKind
            ErrorKind.SyntaxError
            "INSERT ... SELECT の列数不一致"
            (execSql db2 "INSERT INTO t (id) SELECT id, v FROM t")

        Assert.Equal<int64>(1L, countOf db2))

[<Fact>]
let ``SqlReference: INSERT ... SELECT の列数不一致は選択が 0 行でもエラー`` () =
    // 行が 1 つも流れないと列数検査が走らず、不正な DML が 0 行の成功になる穴を塞ぐ。
    withSession "insert-select-empty" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE src (a INTEGER PRIMARY KEY, b INTEGER)"
        let db2 = exec db1 "CREATE TABLE dst (a INTEGER PRIMARY KEY)"

        expectErrorKind
            ErrorKind.SyntaxError
            "0 行の INSERT ... SELECT の列数不一致"
            (execSql db2 "INSERT INTO dst (a) SELECT a, b FROM src")

        // 列数が合っていれば 0 行で成功する。
        let _, empty = ok (execSql db2 "INSERT INTO dst (a) SELECT a FROM src")
        Assert.Equal<int>(0, empty.RowsAffected)

        // 1 行でも流れれば従来どおり不一致を検出する。
        let db3 = exec db2 "INSERT INTO src VALUES (1, 10)"

        expectErrorKind
            ErrorKind.SyntaxError
            "1 行の INSERT ... SELECT の列数不一致"
            (execSql db3 "INSERT INTO dst (a) SELECT a, b FROM src"))

[<Fact>]
let ``SqlReference: UPDATE は WHERE に合う行だけを変える`` () =
    withSession "dml3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = execRes db1 "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)"
        let db3, upd = ok (execSql db2 "UPDATE t SET v = 99 WHERE id > 1")
        Assert.Equal<int>(2, upd.RowsAffected)

        let _db3, res = ok (execSql db3 "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(99L, asInt res.Rows.[1].[1])
        Assert.Equal<int64>(99L, asInt res.Rows.[2].[1]))

[<Fact>]
let ``SqlReference: DELETE は WHERE に合う行を消す`` () =
    withSession "dml4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)")
        let db3, del = ok (execSql db2 "DELETE FROM t WHERE v >= 20")
        Assert.Equal<int>(2, del.RowsAffected)

        let _db4, res = ok (execSql db3 "SELECT id FROM t")
        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: UPDATE / DELETE は索引・主キーの走査で候補を絞っても WHERE で再評価する`` () =
    // DML の候補走査は SELECT と同じ規則（PkLookup / IndexLookup / 範囲）。
    // 走査は候補の十分条件なので、WHERE 全体は行ごとに再評価される。
    withSession "dml-scan-method" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER, b TEXT)"
        let db2 = exec db1 "CREATE INDEX ix_a ON t (a)"
        let db3 = exec db2 "INSERT INTO t VALUES (1, 10, 'x'), (2, 20, 'y'), (3, 10, 'z')"

        // 索引等値（a = 10）で 2 行に絞り、WHERE 全体（id < 3）で 1 行に再評価
        let db4, u1 = ok (execSql db3 "UPDATE t SET b = 'u' WHERE a = 10 AND id < 3")
        Assert.Equal<int>(1, u1.RowsAffected)

        let _, r1 = ok (execSql db4 "SELECT id, b FROM t ORDER BY id")
        Assert.Equal<string>("u", asText r1.Rows.[0].[1])
        Assert.Equal<string>("y", asText r1.Rows.[1].[1])
        Assert.Equal<string>("z", asText r1.Rows.[2].[1])

        // 主キー等値
        let db5, u2 = ok (execSql db4 "UPDATE t SET b = 'v' WHERE id = 2")
        Assert.Equal<int>(1, u2.RowsAffected)

        // 索引等値で削除（2 行）
        let db6, d1 = ok (execSql db5 "DELETE FROM t WHERE a = 10")
        Assert.Equal<int>(2, d1.RowsAffected)

        // 主キーの範囲（閉区間の広い走査）でも WHERE で再評価して 1 行だけ消す
        let db7, d2 = ok (execSql db6 "DELETE FROM t WHERE id > 0 AND id <= 2")
        Assert.Equal<int>(1, d2.RowsAffected)

        let _, left = ok (execSql db7 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt left.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: SELECT * は全列を返す`` () =
    seeded "sel1" (fun db ->
        let _db5, res = ok (execSql db "SELECT * FROM people ORDER BY id")
        Assert.Equal<string array>([| "id"; "name"; "age" |], res.Columns)
        Assert.Equal<int>(3, res.Rows.Length))

[<Fact>]
let ``SqlReference: WHERE で絞る`` () =
    seeded "sel2" (fun db ->
        let _db6, res =
            ok (execSql db "SELECT name FROM people WHERE age > 26 ORDER BY name")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: ORDER BY が効く`` () =
    seeded "sel3" (fun db ->
        let _, asc = ok (execSql db "SELECT name FROM people ORDER BY age")
        Assert.Equal<string>("bob", asText asc.Rows.[0].[0])

        let _, desc = ok (execSql db "SELECT name FROM people ORDER BY age DESC")
        Assert.Equal<string>("carol", asText desc.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 主キー順の ORDER BY は昇順・降順・LIMIT が正しい`` () =
    // 主キー昇順の ORDER BY は Sort ノードを作らない（走査順がそのまま答え）。
    withSession "orderby-pk" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (3,'c'),(1,'a'),(4,'d'),(2,'b')")

        let ids sql =
            let _, res = ok (execSql db2 sql)
            res.Rows |> Array.map (fun r -> int (asInt r.[0])) |> Array.toList

        Assert.Equal<int list>([ 1; 2; 3; 4 ], ids "SELECT id FROM t ORDER BY id")
        Assert.Equal<int list>([ 4; 3; 2; 1 ], ids "SELECT id FROM t ORDER BY id DESC")
        Assert.Equal<int list>([ 1; 2 ], ids "SELECT id FROM t ORDER BY id LIMIT 2")
        Assert.Equal<int list>([ 2; 3 ], ids "SELECT id FROM t ORDER BY id LIMIT 2 OFFSET 1")
        // 別名越しでも列参照に解決される
        Assert.Equal<int list>([ 1; 2; 3; 4 ], ids "SELECT id AS k FROM t ORDER BY k"))

[<Fact>]
let ``SqlReference: 複合主キーは全列の昇順で並ぶ`` () =
    withSession "orderby-pk2" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE c (a INTEGER, b INTEGER, v TEXT, PRIMARY KEY (a, b))"

        let db2, _ =
            ok (execSql db1 "INSERT INTO c VALUES (2,2,'x'),(1,5,'y'),(1,2,'z'),(2,1,'w')")

        let _, rows = ok (execSql db2 "SELECT a, b FROM c ORDER BY a, b")

        let pairs =
            rows.Rows
            |> Array.map (fun r -> int (asInt r.[0]), int (asInt r.[1]))
            |> Array.toList

        Assert.Equal<(int * int) list>([ (1, 2); (1, 5); (2, 1); (2, 2) ], pairs)

        // 主キーの先頭 1列だけでは並びが決まらないので、全列の昇順の結果だけを確認する
        let _, partial = ok (execSql db2 "SELECT a FROM c ORDER BY a")

        Assert.Equal<int list>([ 1; 1; 2; 2 ], partial.Rows |> Array.map (fun r -> int (asInt r.[0])) |> Array.toList))

[<Fact>]
let ``SqlReference: ORDER BY は NULL を昇順で先頭、降順で末尾に置く`` () =
    withSession "orderby-null" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, x INTEGER)"

        let db2, _ =
            ok (execSql db1 "INSERT INTO t VALUES (1,NULL),(2,30),(3,NULL),(4,10),(5,20)")

        let ids sql =
            let _, res = ok (execSql db2 sql)
            res.Rows |> Array.map (fun r -> int (asInt r.[0])) |> Array.toList

        Assert.Equal<int list>([ 1; 3; 4; 5; 2 ], ids "SELECT id FROM t ORDER BY x, id")
        Assert.Equal<int list>([ 2; 5; 4; 1; 3 ], ids "SELECT id FROM t ORDER BY x DESC, id")

        // 単一キーでも NULL 以外が昇順であること。
        let _, res = ok (execSql db2 "SELECT x FROM t ORDER BY x")
        let nulls = res.Rows |> Array.filter (fun r -> SqlValue.isNull r.[0])

        let nonNull =
            res.Rows
            |> Array.choose (fun r ->
                match r.[0] with
                | SqlValue.Int i -> Some i
                | _ -> None)

        Assert.Equal<int>(2, nulls.Length)
        Assert.Equal<int64 list>([ 10L; 20L; 30L ], nonNull |> Array.toList))

[<Fact>]
let ``SqlReference: TEXT の比較は UTF-8 の符号化順に一致する`` () =
    withSession "text-utf8-order" (fun db0 ->
        // UTF-16 のコード単位順では絵文字が全角 A より小さいが、
        // UTF-8 の符号化順（コードポイント順）では全角 A のほうが小さい。
        let fullwidthA = "\uFF21"
        let emoji = "\U0001F600"

        let db1 = exec db0 "CREATE TABLE t (id TEXT PRIMARY KEY)"

        let db2, _ =
            ok (execSql db1 (sprintf "INSERT INTO t VALUES ('%s'), ('%s')" fullwidthA emoji))

        let textIds sql =
            let _, res = ok (execSql db2 sql)
            res.Rows |> Array.map (fun r -> asText r.[0]) |> Array.toList

        // 主キー走査順（ソート省略）と式ソートの結果が一致する。
        Assert.Equal<string list>([ fullwidthA; emoji ], textIds "SELECT id FROM t ORDER BY id")

        Assert.Equal<string list>([ fullwidthA; emoji ], textIds "SELECT id FROM t ORDER BY id || ''")

        // 主キーの範囲走査でも比較順が符号化順と一致する。
        Assert.Equal<string list>(
            [ emoji ],
            textIds (sprintf "SELECT id FROM t WHERE id > '%s' ORDER BY id" fullwidthA)
        )

        Assert.Equal<string list>(
            [ fullwidthA; emoji ],
            textIds (sprintf "SELECT id FROM t WHERE id BETWEEN '%s' AND '%s' ORDER BY id" fullwidthA emoji)
        )

        // 下限と上限が逆の BETWEEN は空。
        Assert.Equal<string list>(
            [],
            textIds (sprintf "SELECT id FROM t WHERE id BETWEEN '%s' AND '%s'" emoji fullwidthA)
        )

        // 索引走査でも同じ。比較順がずれると、索引の範囲に含まれる行を
        // フィルタで落としてしまう。
        let db3 = exec db2 "CREATE TABLE u (id INTEGER PRIMARY KEY, s TEXT)"

        let db4, _ =
            ok (execSql db3 (sprintf "INSERT INTO u VALUES (1,'%s'),(2,'%s')" fullwidthA emoji))

        let db5 = exec db4 "CREATE INDEX us ON u (s)"

        let intIds sql =
            let _, res = ok (execSql db5 sql)
            res.Rows |> Array.map (fun r -> asInt r.[0]) |> Array.toList

        Assert.Equal<int64 list>([ 2L ], intIds (sprintf "SELECT id FROM u WHERE s > '%s' ORDER BY id" fullwidthA))

        Assert.Equal<int64 list>(
            [ 1L; 2L ],
            intIds (sprintf "SELECT id FROM u WHERE s BETWEEN '%s' AND '%s' ORDER BY id" fullwidthA emoji)
        )

        let _, sorted = ok (execSql db5 "SELECT s FROM u ORDER BY s")

        Assert.Equal<string list>(
            [ fullwidthA; emoji ],
            sorted.Rows |> Array.map (fun r -> asText r.[0]) |> Array.toList
        ))

[<Fact>]
let ``SqlReference: REAL の -0.0 は +0.0 と同じ主キー`` () =
    withSession "real-negative-zero" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE r (x REAL PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO r VALUES (0.0, 'a')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "REAL の -0.0 が +0.0 と別の主キーになっています"
            (execSql db2 "INSERT INTO r VALUES (-0.0, 'b')")

        let _, res = ok (execSql db2 "SELECT x FROM r")
        Assert.Equal<int>(1, res.Rows.Length)

        match res.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.Equal(0L, System.BitConverter.DoubleToInt64Bits f)
        | v -> failActual "REAL ではありません" v

        let _, byValue = ok (execSql db2 "SELECT x FROM r WHERE x = -0.0")
        Assert.Equal<int>(1, byValue.Rows.Length)

        // UNIQUE 索引も同じキーで重複を検出する。
        let db3 = exec db2 "CREATE TABLE u (id INTEGER PRIMARY KEY, x REAL)"
        let db4 = exec db3 "CREATE UNIQUE INDEX ux ON u (x)"
        let db5 = exec db4 "INSERT INTO u VALUES (1, 0.0)"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "UNIQUE 索引で -0.0 と +0.0 が別の値になっています"
            (execSql db5 "INSERT INTO u VALUES (2, -0.0)"))

[<Fact>]
let ``SqlReference: 別名を ORDER BY で参照できる`` () =
    seeded "sel-alias-order" (fun db ->
        // 別名 a は age を指すので age の順（bob 25 → alice 30 → carol 35）で並ぶ
        let _, res = ok (execSql db "SELECT name AS x, age AS a FROM people ORDER BY a")
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<string>("bob", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[2].[0]))

[<Fact>]
let ``SqlReference: 別名を GROUP BY と HAVING で参照できる`` () =
    seeded "sel-alias-group" (fun db ->
        let _, res = ok (execSql db "SELECT age AS a, COUNT(*) FROM people GROUP BY a")
        Assert.Equal<int>(3, res.Rows.Length)

        // HAVING にも別名を参照できる
        let _, having = ok (execSql db "SELECT COUNT(*) AS c FROM people HAVING c > 2")
        Assert.Equal<int>(1, having.Rows.Length)
        Assert.Equal<int64>(3L, asInt having.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 別名より列が優先される`` () =
    seeded "sel-alias-priority" (fun db ->
        // age という列があるのに age という別名を付けても、ORDER BY は列を見る
        // （列の順: bob 25 → alice 30 → carol 35。別名=name なら alice が先）
        let _, res = ok (execSql db "SELECT name AS age FROM people ORDER BY age")
        Assert.Equal<string>("alice", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 別名の循環参照でも止まる`` () =
    seeded "sel-alias-cycle" (fun db ->
        // x と y が互いを指す（差し替えで無限ループにしない）
        expectError "循環参照する別名は参照できない" (execSql db "SELECT x AS y, y AS x FROM people ORDER BY x"))

[<Fact>]
let ``SqlReference: GROUP BY と COUNT`` () =
    seeded "sel4" (fun db ->
        let db2, _r = execRes db "INSERT INTO people VALUES (4,'dave',30)"

        let _q, res =
            execRes db2 "SELECT age, COUNT(*) FROM people GROUP BY age ORDER BY age"
        // age 25, 30(x2), 35 の 3 グループ
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<int64>(25L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(30L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: GROUP BY の式を SELECT / HAVING でも使える`` () =
    seeded "group-expr" (fun db ->
        // age 30, 25, 35 → age/10 は 3, 2, 3 の 2 グループ。
        let _q, res = execRes db "SELECT age / 10, COUNT(*) FROM people GROUP BY age / 10"
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(3L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[1].[1])

        // 集約を SELECT に含めなくても、GROUP BY 式を射影できる。
        let _q2, only = execRes db "SELECT age / 10 FROM people GROUP BY age / 10"
        Assert.Equal<int>(2, only.Rows.Length)
        Assert.Equal<int64>(3L, asInt only.Rows.[0].[0])
        Assert.Equal<int64>(2L, asInt only.Rows.[1].[0])

        // HAVING でも同じ式を参照できる。
        let _q3, having =
            execRes db "SELECT age / 10 FROM people GROUP BY age / 10 HAVING age / 10 = 2"

        Assert.Equal<int>(1, having.Rows.Length)
        Assert.Equal<int64>(2L, asInt having.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 集約後の参照は識別子の大小文字を区別しない`` () =
    // 識別子は大小文字を区別しない（§5）。集約後のスロット対応も同じ規則にする。
    withSession "agg-case" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 1), (2, 1), (3, 2)"

        let _, res = ok (execSql db2 "SELECT A, COUNT(*) FROM t GROUP BY a ORDER BY A")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[1].[1])

        // HAVING と ORDER BY も同じ規則で解決する。
        let _, having = ok (execSql db2 "SELECT a FROM t GROUP BY A HAVING A > 1")
        Assert.Equal<int>(1, having.Rows.Length)
        Assert.Equal<int64>(2L, asInt having.Rows.[0].[0])

        let _, ordered =
            ok (execSql db2 "SELECT a, COUNT(*) FROM t GROUP BY A ORDER BY COUNT(*) DESC, a")

        Assert.Equal<int64>(1L, asInt ordered.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 表示が同じでも型が違う集約を混同しない`` () =
    // `1` と `1e0` は表示が同じだが型が違う。スロット対応を表示文字列で行うと
    // 後勝ちで取り違え、`MIN(1) / 2` が REAL の 0.5 になる（INTEGER の 0 が正しい）。
    withSession "agg-key-type" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1)"

        let _, res = ok (execSql db2 "SELECT MIN(1) / 2, MIN(1e0) FROM t")

        match res.Rows.[0].[0], res.Rows.[0].[1] with
        | SqlValue.Int 0L, SqlValue.Real 1.0 -> ()
        | other -> failActual "INTEGER 0 と REAL 1.0" other

        // 順序を入れ替えても取り違えない。
        let _, rev = ok (execSql db2 "SELECT MIN(1e0), MIN(1) / 2 FROM t")

        match rev.Rows.[0].[0], rev.Rows.[0].[1] with
        | SqlValue.Real 1.0, SqlValue.Int 0L -> ()
        | other -> failActual "REAL 1.0 と INTEGER 0" other)

[<Fact>]
let ``SqlReference: HAVING でグループを絞る`` () =
    seeded "sel-having" (fun db ->
        let db2, _r = execRes db "INSERT INTO people VALUES (4,'dave',30)"

        // age 30 のグループ（2 人）だけ残る
        let _q, res =
            execRes db2 "SELECT age, COUNT(*) FROM people GROUP BY age HAVING COUNT(*) > 1 ORDER BY age"

        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(30L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[1])

        // 集約を含まない HAVING も動く（グループ側の値で評価される）
        let _q2, res2 =
            execRes db2 "SELECT age FROM people GROUP BY age HAVING age >= 30 ORDER BY age"

        Assert.Equal<int>(2, res2.Rows.Length)
        Assert.Equal<int64>(30L, asInt res2.Rows.[0].[0])
        Assert.Equal<int64>(35L, asInt res2.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: HAVING にしか現れない集約でも絞り込める`` () =
    seeded "sel-having-only" (fun db ->
        let db2, _r = execRes db "INSERT INTO people VALUES (4,'dave',30)"

        // COUNT(*) は SELECT に含めない（HAVING だけで使う）。
        let _q, res =
            execRes db2 "SELECT age FROM people GROUP BY age HAVING COUNT(*) > 1 ORDER BY age"

        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(30L, asInt res.Rows.[0].[0])

        // GROUP BY なしでも HAVING の集約を使える。
        let _q2, grouped = execRes db2 "SELECT 1 FROM people HAVING COUNT(*) > 0"
        Assert.Equal<int>(1, grouped.Rows.Length)

        let _q3, empty = execRes db2 "SELECT 1 FROM people HAVING COUNT(*) > 100"
        Assert.Equal<int>(0, empty.Rows.Length))

[<Fact>]
let ``SqlReference: LIMIT`` () =
    seeded "sel5" (fun db ->
        let _db7, res = ok (execSql db "SELECT id FROM people ORDER BY id LIMIT 2")
        Assert.Equal<int>(2, res.Rows.Length))

[<Fact>]
let ``SqlReference: OFFSET`` () =
    seeded "sel6" (fun db ->
        let _db8, res = ok (execSql db "SELECT id FROM people ORDER BY id LIMIT 5 OFFSET 1")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: DISTINCT`` () =
    withSession "sel7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30)")
        let _db9, res = ok (execSql db2 "SELECT DISTINCT g FROM t ORDER BY g")
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: DISTINCT を続けて実行しても毎回全行が返る`` () =
    // 既出集合をモジュール初期化時に 1個だけ作ると、2回目以降が全行を重複と見なして 0行になる。
    withSession "sel7b" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30)")

        for _ in 1..3 do
            let _db, res = ok (execSql db2 "SELECT DISTINCT g FROM t ORDER BY g")
            Assert.Equal<int>(3, res.Rows.Length)
            Assert.Equal<int64>(10L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: GROUP BY とハッシュ結合を続けて実行しても毎回同じ結果になる`` () =
    // 演算子の作業用コレクション（グループ表・ハッシュ表）は Op の構築時に作り、
    // 返した Op の外へ持ち出さない。モジュールスコープに置くと 2回目以降が壊れる。
    withSession "sel7c" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2 = exec db1 "CREATE TABLE u (id INTEGER PRIMARY KEY, h INTEGER)"
        let db3, _ = ok (execSql db2 "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30)")
        let db4, _ = ok (execSql db3 "INSERT INTO u VALUES (1,10),(2,99),(3,10)")

        let groupSql = "SELECT g, COUNT(*) FROM t GROUP BY g ORDER BY g"
        let joinSql = "SELECT t.id, u.h FROM t JOIN u ON t.g = u.h ORDER BY t.id"

        let expectedGroups = execSql db4 groupSql |> ok |> snd
        let expectedJoin = execSql db4 joinSql |> ok |> snd

        Assert.Equal<int>(3, expectedGroups.Rows.Length)
        Assert.Equal<int>(4, expectedJoin.Rows.Length)

        for _ in 1..3 do
            let _db, groups = ok (execSql db4 groupSql)
            let _db, joined = ok (execSql db4 joinSql)
            Assert.Equal<int>(expectedGroups.Rows.Length, groups.Rows.Length)
            Assert.Equal<int>(expectedJoin.Rows.Length, joined.Rows.Length)
            Assert.Equal<int64>(asInt expectedGroups.Rows.[0].[1], asInt groups.Rows.[0].[1])
            Assert.Equal<int64>(asInt expectedJoin.Rows.[0].[0], asInt joined.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 別名を付けられる`` () =
    seeded "sel8" (fun db ->
        let _db10, res = ok (execSql db "SELECT name AS n FROM people ORDER BY id LIMIT 1")
        Assert.Equal<string array>([| "n" |], res.Columns))

[<Fact>]
let ``SqlReference: 集約関数`` () =
    seeded "sel9" (fun db ->
        let _, cnt = ok (execSql db "SELECT COUNT(*) FROM people")
        Assert.Equal<int64>(3L, asInt cnt.Rows.[0].[0])

        let _, sum = ok (execSql db "SELECT SUM(age) FROM people")
        Assert.True(SqlValue.isNull sum.Rows.[0].[0] |> not)

        let _, mn = ok (execSql db "SELECT MIN(age), MAX(age) FROM people")
        Assert.Equal<int64>(25L, asInt mn.Rows.[0].[0])
        Assert.Equal<int64>(35L, asInt mn.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: JOIN`` () =
    withSession "sel10" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, _ = execRes db1 "INSERT INTO people VALUES (1,'alice',30),(2,'bob',25)"

        let db3 =
            exec db2 "CREATE TABLE orders (oid INTEGER PRIMARY KEY, person_id INTEGER, amount INTEGER)"

        let db4, _ = execRes db3 "INSERT INTO orders VALUES (1,1,100),(2,1,200),(3,2,50)"

        let _, res =
            ok (
                execSql
                    db4
                    "SELECT people.name, SUM(orders.amount) FROM people JOIN orders ON people.id = orders.person_id GROUP BY people.name ORDER BY people.name"
            )

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("bob", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 3 テーブルの入れ子 JOIN が行を返す`` () =
    // 左部分木へ渡すスキーマは「右テーブルを除いたもの」。
    withSession "join3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, av INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, bv INTEGER)"
        let db4, _ = execRes db3 "INSERT INTO a VALUES (1,10),(2,20)"
        let db5, _ = execRes db4 "INSERT INTO b VALUES (1,100),(2,200)"
        let db6, _ = execRes db5 "INSERT INTO c VALUES (1,1000),(2,2000)"

        let _, res =
            ok (execSql db6 "SELECT a.v, b.av, c.bv FROM a JOIN b ON a.id = b.id JOIN c ON b.id = c.id ORDER BY a.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(100L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(1000L, asInt res.Rows.[0].[2])
        Assert.Equal<int64>(20L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(200L, asInt res.Rows.[1].[1])
        Assert.Equal<int64>(2000L, asInt res.Rows.[1].[2]))

[<Fact>]
let ``SqlReference: エイリアスでの自己結合が行を返す`` () =
    // 自己結合の対応付けはテーブル名ではなく別名で行う。
    withSession "selfjoin" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20),(3,30)"

        // 右だけを絞り込む WHERE が効くこと（JOIN が本当に組まれていること）。
        let _, res =
            ok (execSql db2 "SELECT p.id, q.v FROM t AS p JOIN t AS q ON p.id = q.id WHERE q.v > 15 ORDER BY p.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(20L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(30L, asInt res.Rows.[1].[1])

        // 非キーでの自己結合（それぞれ自分自身とだけ一致する）。
        let _, res2 =
            ok (execSql db2 "SELECT p.id, q.id FROM t AS p JOIN t AS q ON p.v = q.v ORDER BY p.id")

        Assert.Equal<int>(3, res2.Rows.Length)
        Assert.Equal<int64>(2L, asInt res2.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt res2.Rows.[1].[1])

        // 3 段の自己結合（p / q / r すべて t のエイリアス）。
        let _, res3 =
            ok (
                execSql
                    db2
                    "SELECT p.id, q.id, r.v FROM t AS p JOIN t AS q ON p.id = q.id JOIN t AS r ON q.id = r.id WHERE p.v = 30 ORDER BY p.id"
            )

        Assert.Equal<int>(1, res3.Rows.Length)
        Assert.Equal<int64>(3L, asInt res3.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res3.Rows.[0].[1])
        Assert.Equal<int64>(30L, asInt res3.Rows.[0].[2]))

[<Fact>]
let ``SqlReference: ハッシュ結合（INNER / LEFT / NULL キー）`` () =
    withSession "joinhash" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db3 = exec db2 "INSERT INTO a VALUES (1,10),(2,20),(3,NULL)"
        let db4 = exec db3 "INSERT INTO b VALUES (1,10),(2,10),(3,30),(4,NULL)"

        // INNER: NULL キーは一致しない（a3 は落ちる）
        let _, inner =
            ok (execSql db4 "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k ORDER BY a.id, b.id")

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<int64>(1L, asInt inner.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt inner.Rows.[0].[1])
        Assert.Equal<int64>(1L, asInt inner.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt inner.Rows.[1].[1])

        // LEFT: 一致しない a2 / a3 は右側 NULL
        let _, left =
            ok (execSql db4 "SELECT a.id, b.id FROM a LEFT JOIN b ON a.k = b.k ORDER BY a.id, b.id")

        Assert.Equal<int>(4, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[2].[1])
        Assert.True(SqlValue.isNull left.Rows.[3].[1])

        // 残りの ON 句（b.id = 1）はハッシュ結合でも評価される
        let _, residual =
            ok (execSql db4 "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k AND b.id = 1")

        Assert.Equal<int>(1, residual.Rows.Length)
        Assert.Equal<int64>(1L, asInt residual.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: REAL の NaN 同士はハッシュ結合でも一致する`` () =
    // REAL の比較は NaN = NaN が真（§2）。ハッシュ結合のキーも同じ意味論にする。
    withSession "joinnan" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE nan1 (id INTEGER PRIMARY KEY, r REAL)"
        let db2 = exec db1 "CREATE TABLE nan2 (id INTEGER PRIMARY KEY, r REAL)"
        let db3 = exec db2 "INSERT INTO nan1 VALUES (1, CAST('NaN' AS REAL)), (2, 1.5)"
        let db4 = exec db3 "INSERT INTO nan2 VALUES (1, CAST('NaN' AS REAL)), (2, 1.5)"

        // r は主キーでも索引でもないのでハッシュ結合になる
        let _, res =
            ok (execSql db4 "SELECT nan1.id, nan2.id FROM nan1 JOIN nan2 ON nan1.r = nan2.r ORDER BY nan1.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: マージ結合（主キー同士・LEFT・複合キー）`` () =
    withSession "joinmerge" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE m1 (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE m2 (id INTEGER PRIMARY KEY, w INTEGER)"
        let db3 = exec db2 "INSERT INTO m1 VALUES (1,10),(2,20),(4,40)"
        let db4 = exec db3 "INSERT INTO m2 VALUES (2,200),(3,300),(4,400)"

        // 両方が主キー id の走査 → マージ結合
        let _, inner =
            ok (execSql db4 "SELECT m1.id, m1.v, m2.w FROM m1 JOIN m2 ON m1.id = m2.id ORDER BY m1.id")

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<int64>(2L, asInt inner.Rows.[0].[0])
        Assert.Equal<int64>(200L, asInt inner.Rows.[0].[2])
        Assert.Equal<int64>(4L, asInt inner.Rows.[1].[0])
        Assert.Equal<int64>(400L, asInt inner.Rows.[1].[2])

        // LEFT: 一致しない 1 は右側 NULL
        let _, left =
            ok (execSql db4 "SELECT m1.id, m2.w FROM m1 LEFT JOIN m2 ON m1.id = m2.id ORDER BY m1.id")

        Assert.Equal<int>(3, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[0].[1])
        Assert.Equal<int64>(200L, asInt left.Rows.[1].[1])
        Assert.Equal<int64>(400L, asInt left.Rows.[2].[1])

        // ON の残り条件で一致が消えた左行も NULL 拡張する
        let _, leftResidual =
            ok (execSql db4 "SELECT m1.id, m2.w FROM m1 LEFT JOIN m2 ON m1.id = m2.id AND m2.w > 250 ORDER BY m1.id")

        Assert.Equal<int>(3, leftResidual.Rows.Length)
        Assert.True(SqlValue.isNull leftResidual.Rows.[0].[1])
        Assert.True(SqlValue.isNull leftResidual.Rows.[1].[1])
        Assert.Equal<int64>(400L, asInt leftResidual.Rows.[2].[1])

        // 複合主キー: ON の順が逆でも主キー順に並べ替えてマージ結合にする
        let c1 =
            exec db4 "CREATE TABLE c1 (a INTEGER, b INTEGER, v INTEGER, PRIMARY KEY (a, b))"

        let c2 =
            exec c1 "CREATE TABLE c2 (a INTEGER, b INTEGER, w INTEGER, PRIMARY KEY (a, b))"

        let c3 = exec c2 "INSERT INTO c1 VALUES (1,1,10),(1,2,20),(2,1,30)"
        let c4 = exec c3 "INSERT INTO c2 VALUES (1,2,200),(2,1,300),(2,2,400)"

        let _, composite =
            ok (
                execSql
                    c4
                    "SELECT c1.a, c1.b, c1.v, c2.w FROM c1 JOIN c2 ON c1.b = c2.b AND c1.a = c2.a ORDER BY c1.a, c1.b"
            )

        Assert.Equal<int>(2, composite.Rows.Length)
        Assert.Equal<int64>(1L, asInt composite.Rows.[0].[0])
        Assert.Equal<int64>(2L, asInt composite.Rows.[0].[1])
        Assert.Equal<int64>(200L, asInt composite.Rows.[0].[3])
        Assert.Equal<int64>(2L, asInt composite.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt composite.Rows.[1].[1])
        Assert.Equal<int64>(300L, asInt composite.Rows.[1].[3])

        // 重複キー: 両方の k に索引があり、範囲条件で k 順の索引走査 → マージ結合
        let d1 = exec c4 "CREATE TABLE d1 (id INTEGER PRIMARY KEY, k INTEGER)"
        let d2 = exec d1 "CREATE TABLE d2 (id INTEGER PRIMARY KEY, k INTEGER)"
        let d3 = exec d2 "CREATE INDEX ix_d1_k ON d1 (k)"
        let d4 = exec d3 "CREATE INDEX ix_d2_k ON d2 (k)"
        let d5 = exec d4 "INSERT INTO d1 VALUES (1,10),(2,20),(3,20)"
        let d6 = exec d5 "INSERT INTO d2 VALUES (1,20),(2,30)"

        let _, duplicates =
            ok (
                execSql
                    d6
                    "SELECT d1.id, d2.id FROM d1 JOIN d2 ON d1.k = d2.k WHERE d1.k >= 0 AND d2.k >= 0 ORDER BY d1.id, d2.id"
            )

        Assert.Equal<int>(2, duplicates.Rows.Length)
        Assert.Equal<int64>(2L, asInt duplicates.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt duplicates.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt duplicates.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt duplicates.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: クロス名の等値結合は行を取りこぼさない`` () =
    // ON x1.id = x2.k AND x1.k = x2.id は、両側の走査順の列名が同じでも
    // 連言の位置の対応は逆。キーを名前で独立に並べ替えると誤った列同士を比較し、
    // 正しい 3 行が 0 行になっていた（マージ結合の選択時）。
    withSession "join-merge-crossed" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE x1 (id INTEGER, k INTEGER, v INTEGER, PRIMARY KEY (id, k))"

        let db2 =
            exec db1 "CREATE TABLE x2 (id INTEGER, k INTEGER, w INTEGER, PRIMARY KEY (id, k))"

        let db3 = exec db2 "INSERT INTO x1 VALUES (1,2,10),(2,4,20),(3,6,30)"
        let db4 = exec db3 "INSERT INTO x2 VALUES (2,1,100),(4,2,200),(6,3,300)"

        let _, rows =
            ok (execSql db4 "SELECT COUNT(*) FROM x1 JOIN x2 ON x1.id = x2.k AND x1.k = x2.id")

        Assert.Equal<int64>(3L, asInt rows.Rows.[0].[0])

        // 対応が揃っている場合は 0 行（取り違えの検出用の対照）。
        let _, none =
            ok (execSql db4 "SELECT COUNT(*) FROM x1 JOIN x2 ON x1.id = x2.id AND x1.k = x2.k")

        Assert.Equal<int64>(0L, asInt none.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: WHERE の真偽と NOT / CASE WHEN / IS UNKNOWN が一致する`` () =
    // 以前は WHERE 5 が TRUE なのに NOT 5 も TRUE、CASE WHEN 1 は ELSE 側だった。
    withSession "truth-consistency" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1),(2)"

        let _, whereTrue = ok (execSql db2 "SELECT COUNT(*) FROM t WHERE 5")
        Assert.Equal<int64>(2L, asInt whereTrue.Rows.[0].[0])

        let _, notTrue = ok (execSql db2 "SELECT COUNT(*) FROM t WHERE NOT 5")
        Assert.Equal<int64>(0L, asInt notTrue.Rows.[0].[0])

        let _, case =
            ok (execSql db2 "SELECT CASE WHEN 1 THEN 'a' ELSE 'b' END FROM t LIMIT 1")

        Assert.Equal<string>("a", asText case.Rows.[0].[0])

        let _, unknown = ok (execSql db2 "SELECT COUNT(*) FROM t WHERE 1 IS UNKNOWN")
        Assert.Equal<int64>(0L, asInt unknown.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: SELECT ALL は既定どおり重複を残す`` () =
    withSession "select-all" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,10),(3,20)"

        let _, rows = ok (execSql db2 "SELECT ALL g FROM t ORDER BY g")
        Assert.Equal<int>(3, rows.Rows.Length)

        let _, distinct = ok (execSql db2 "SELECT DISTINCT g FROM t ORDER BY g")
        Assert.Equal<int>(2, distinct.Rows.Length))

[<Fact>]
let ``SqlReference: DISTINCT は DECIMAL のスケール違いを同じ値とみなす`` () =
    // 1.0 と 1.00 は SQL では等しい。SELECT DISTINCT が 2 行を返していた。
    // COUNT(DISTINCT) / GROUP BY / ハッシュ結合のキーと揃える。
    withSession "distinct-decimal-scale" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, d DECIMAL)"

        let db2 =
            exec
                db1
                "INSERT INTO t VALUES (1, CAST('1.0' AS DECIMAL)), (2, CAST('1.00' AS DECIMAL)), (3, CAST('2.00' AS DECIMAL))"

        let _, rows = ok (execSql db2 "SELECT DISTINCT d FROM t ORDER BY d")
        Assert.Equal<int>(2, rows.Rows.Length)

        match rows.Rows.[0].[0], rows.Rows.[1].[0] with
        | SqlValue.Decimal a, SqlValue.Decimal b ->
            Assert.Equal<decimal>(1M, a)
            Assert.Equal<decimal>(2M, b)
        | other -> failActual "DECIMAL ではありません" other

        let _, counted = ok (execSql db2 "SELECT COUNT(DISTINCT d) FROM t")
        Assert.Equal<int64>(2L, asInt counted.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: DISTINCT 集約は退避しても型の違う同じ値を 1つに数える`` () =
    // 退避 run に元の値だけを書いていたため、Int 1 と DECIMAL 1.0 を別の値として数えていた。
    withTightMemory "distinct-spill-types" (fun db ->
        let db1 = exec db "CREATE TABLE t (i INTEGER PRIMARY KEY, v INTEGER)"

        let values =
            [ 1..100 ] |> List.map (fun i -> sprintf "(%d, %d)" i i) |> String.concat ","

        let db2, _ = ok (execSql db1 (sprintf "INSERT INTO t VALUES %s, (101, 1)" values))

        let _, rows =
            ok (
                execSql
                    db2
                    "SELECT COUNT(DISTINCT CASE WHEN i = 101 THEN CAST(1 AS DECIMAL) ELSE v END) AS c, SUM(DISTINCT CASE WHEN i = 101 THEN CAST(1 AS DECIMAL) ELSE v END) AS s FROM t"
            )

        Assert.Equal<int64>(100L, asInt rows.Rows.[0].[0])

        match rows.Rows.[0].[1] with
        | SqlValue.Decimal d -> Assert.Equal<decimal>(5050M, d)
        | other -> failwithf "DECIMAL ではありません: %A" other)

[<Fact>]
let ``SqlReference: REAL の演算が無限大になる場合はエラー`` () =
    // 黙って Infinity を返すと、比較段階で初めて型エラーになる。
    withSession "real-overflow" (fun db0 ->
        expectErrorKind ErrorKind.ConstraintViolation "1e308 * 10 は Infinity になる" (execSql db0 "SELECT 1e308 * 10")

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, x REAL)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 1e308), (2, 1e308)"

        expectErrorKind ErrorKind.ConstraintViolation "SUM(REAL) が Infinity になる" (execSql db2 "SELECT SUM(x) FROM t"))

[<Fact>]
let ``SqlReference: COUNT 以外の集約に * は使えない`` () =
    withSession "aggregate-star" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        expectErrorKind ErrorKind.SyntaxError "MIN(*)" (execSql db1 "SELECT MIN(*) FROM t")
        expectErrorKind ErrorKind.SyntaxError "SUM(*)" (execSql db1 "SELECT SUM(*) FROM t")
        expectErrorKind ErrorKind.SyntaxError "COUNT(t.*)" (execSql db1 "SELECT COUNT(t.*) FROM t"))

[<Fact>]
let ``SqlReference: 型の違う列の等値結合は TypeError`` () =
    // ハッシュ結合のキーは宣言型が一致する列だけ（`Analyzer.joinKeys`）。
    // 型が違う等値は NestedLoop の比較で TypeError になる（無言の 0 行にしない）。
    withSession "join-type-mismatch" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE ai (i INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE bt (s TEXT PRIMARY KEY)"
        let db3 = exec db2 "INSERT INTO ai VALUES (1), (2)"
        let db4 = exec db3 "INSERT INTO bt VALUES ('1'), ('2')"

        expectErrorKind
            ErrorKind.TypeError
            "INTEGER と TEXT の等値結合"
            (execSql db4 "SELECT COUNT(*) FROM ai JOIN bt ON ai.i = bt.s"))

[<Fact>]
let ``SqlReference: ORDER BY の範囲外の序数は SyntaxError`` () =
    // 以前は `int n` へ先に変換していたため、32bit 範囲外で例外が漏れる恐れがあった。
    withSession "order-by-ordinal" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1)"

        expectErrorKind
            ErrorKind.SyntaxError
            "ORDER BY 5000000000"
            (execSql db2 "SELECT id FROM t ORDER BY 5000000000")

        expectErrorKind ErrorKind.SyntaxError "ORDER BY 0" (execSql db2 "SELECT id FROM t ORDER BY 0"))

[<Fact>]
let ``SqlReference: インデックス入れ子ループ（右索引 + 左が小さい）`` () =
    withSession "joinindex" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE depts (id INTEGER PRIMARY KEY, name TEXT)"

        let db2 =
            exec db1 "CREATE TABLE users (id INTEGER PRIMARY KEY, dept_id INTEGER, name TEXT)"

        let db3 = exec db2 "CREATE INDEX ix_users_dept ON users (dept_id)"
        let db4 = exec db3 "INSERT INTO depts VALUES (1,'eng'),(2,'sales'),(3,'empty')"

        let db5 =
            exec db4 "INSERT INTO users VALUES (1,1,'alice'),(2,1,'bob'),(3,2,'carol')"

        // 左は主キー 1 行、右 dept_id に索引 → IndexNestedLoop
        let _, inner =
            ok (
                execSql
                    db5
                    "SELECT depts.name, users.name FROM depts JOIN users ON depts.id = users.dept_id WHERE depts.id = 1 ORDER BY users.id"
            )

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<string>("alice", asText inner.Rows.[0].[1])
        Assert.Equal<string>("bob", asText inner.Rows.[1].[1])

        // 一致なしの LEFT JOIN は右側 NULL
        let _, left =
            ok (
                execSql
                    db5
                    "SELECT depts.name, users.name FROM depts LEFT JOIN users ON depts.id = users.dept_id WHERE depts.id = 3"
            )

        Assert.Equal<int>(1, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[0].[1])

        // 等値キー以外の ON 句も索引結合で評価される
        let _, residual =
            ok (
                execSql
                    db5
                    "SELECT users.name FROM depts JOIN users ON depts.id = users.dept_id AND users.name = 'bob' WHERE depts.id = 1"
            )

        Assert.Equal<int>(1, residual.Rows.Length)
        Assert.Equal<string>("bob", asText residual.Rows.[0].[0])

        // 右の WHERE は Filter として pushdown されていても、索引結合の residual として評価される
        let _, local =
            ok (
                execSql
                    db5
                    "SELECT users.name FROM depts JOIN users ON depts.id = users.dept_id WHERE depts.id = 1 AND users.name <> 'alice' ORDER BY users.id"
            )

        Assert.Equal<int>(1, local.Rows.Length)
        Assert.Equal<string>("bob", asText local.Rows.[0].[0])

        // 右の等値条件が主キーそのものでも索引入れ子ループで引ける
        let _, byPk =
            ok (execSql db5 "SELECT users.name FROM depts JOIN users ON depts.id = users.id WHERE depts.id = 1")

        Assert.Equal<int>(1, byPk.Rows.Length)
        Assert.Equal<string>("alice", asText byPk.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: インデックス入れ子ループは右の吸収済み述語を落とさない`` () =
    withSession "joinindex-absorbed" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY, name TEXT)"

        let db2 =
            exec db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid INTEGER, k INTEGER, v TEXT)"

        let db3 = exec db2 "CREATE INDEX ix_c_pid ON c (pid)"
        let db4 = exec db3 "CREATE INDEX ix_c_k ON c (k)"
        let db5 = exec db4 "INSERT INTO p VALUES (1,'p1'),(2,'p2')"
        let db6 = exec db5 "INSERT INTO c VALUES (1,1,10,'c1'),(2,1,20,'c2'),(3,2,99,'c3')"

        let planText (sql: string) =
            let _, plan = ok (execSql db6 ("EXPLAIN " + sql))

            plan.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        // 右の主キー等値は INL の lookup に吸収されず、residual として評価される必要がある。
        let _, byPk =
            ok (execSql db6 "SELECT c.id FROM p JOIN c ON p.id = c.pid WHERE p.id = 1 AND c.id = 3")

        Assert.Empty byPk.Rows

        Assert.Contains(
            "IndexNestedLoop",
            planText "SELECT c.id FROM p JOIN c ON p.id = c.pid WHERE p.id = 1 AND c.id = 3"
        )

        // 右の索引等値も同様。
        let _, byIndex =
            ok (execSql db6 "SELECT c.id FROM p JOIN c ON p.id = c.pid WHERE p.id = 1 AND c.k = 99")

        Assert.Empty byIndex.Rows

        Assert.Contains(
            "IndexNestedLoop",
            planText "SELECT c.id FROM p JOIN c ON p.id = c.pid WHERE p.id = 1 AND c.k = 99"
        )

        // 一致する側は 1 行。
        let _, matched =
            ok (execSql db6 "SELECT c.id, c.k FROM p JOIN c ON p.id = c.pid WHERE p.id = 1 AND c.k = 20")

        Assert.Equal<int>(1, matched.Rows.Length)
        Assert.Equal<int64>(2L, asInt matched.Rows.[0].[0])
        Assert.Equal<int64>(20L, asInt matched.Rows.[0].[1])

        // 範囲述語は Filter に残るため、右が RangeScan でも INL の residual で評価できる。
        let _, rightRange =
            ok (execSql db6 "SELECT c.id FROM p JOIN c ON p.id = c.pid WHERE p.id = 1 AND c.id > 1")

        Assert.Equal<int>(1, rightRange.Rows.Length)
        Assert.Equal<int64>(2L, asInt rightRange.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 左が小さいハッシュ結合は左を build する`` () =
    withSession "join-hash-build-left" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE s (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_s_k ON s (k)"
        let db3 = exec db2 "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db4 = exec db3 "INSERT INTO s VALUES (1,10),(2,20)"
        let db5 = exec db4 "INSERT INTO b VALUES (1,10),(2,20)"

        let sql = "SELECT s.id, b.id FROM s JOIN b ON s.k = b.k WHERE s.k = 10"
        let _, rows = ok (execSql db5 sql)
        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[1])

        let _, plan = ok (execSql db5 ("EXPLAIN " + sql))

        let text = plan.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        Assert.Contains("HashJoin(left)", text))

[<Fact>]
let ``SqlReference: JOIN の並べ替え後も SELECT * は FROM 順の列を返す`` () =
    withSession "join-star-order" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (aid INTEGER PRIMARY KEY, av TEXT)"
        let db2 = exec db1 "CREATE TABLE b (bid INTEGER PRIMARY KEY, bv TEXT)"
        let db3 = exec db2 "CREATE TABLE c (cid INTEGER PRIMARY KEY, cv TEXT)"
        let db4 = exec db3 "INSERT INTO a VALUES (1,'A')"
        let db5 = exec db4 "INSERT INTO b VALUES (1,'B')"
        let db6 = exec db5 "INSERT INTO c VALUES (1,'C')"

        // WHERE c.cid = 1 で c が先に結合され、物理レイアウトは a, c, b になる。
        let _, star =
            ok (execSql db6 "SELECT * FROM a JOIN b ON a.aid = b.bid JOIN c ON a.aid = c.cid WHERE c.cid = 1")

        Assert.Equal<string array>([| "aid"; "av"; "bid"; "bv"; "cid"; "cv" |], star.Columns)
        Assert.Equal<int>(1, star.Rows.Length)
        Assert.Equal<int64>(1L, asInt star.Rows.[0].[0])
        Assert.Equal<string>("A", asText star.Rows.[0].[1])
        Assert.Equal<int64>(1L, asInt star.Rows.[0].[2])
        Assert.Equal<string>("B", asText star.Rows.[0].[3])
        Assert.Equal<int64>(1L, asInt star.Rows.[0].[4])
        Assert.Equal<string>("C", asText star.Rows.[0].[5])

        // 派生テーブルの `SELECT *` でも列名と値が一致する。
        let _, derived =
            ok (
                execSql
                    db6
                    "SELECT d.bv, d.cv FROM (SELECT * FROM a JOIN b ON a.aid = b.bid JOIN c ON a.aid = c.cid WHERE c.cid = 1) AS d"
            )

        Assert.Equal<int>(1, derived.Rows.Length)
        Assert.Equal<string>("B", asText derived.Rows.[0].[0])
        Assert.Equal<string>("C", asText derived.Rows.[0].[1])

        // INSERT ... SELECT * も FROM 順で書き込む。
        let db7 =
            exec db6 "CREATE TABLE t2 (k INTEGER PRIMARY KEY, x TEXT, y TEXT, z TEXT, w TEXT, u TEXT, v TEXT)"

        let db8 =
            exec
                db7
                "INSERT INTO t2 (k, x, y, z, w, u, v) SELECT 1, * FROM a JOIN b ON a.aid = b.bid JOIN c ON a.aid = c.cid WHERE c.cid = 1"

        let _, inserted = ok (execSql db8 "SELECT x, y, z, w, u, v FROM t2")
        Assert.Equal<int>(1, inserted.Rows.Length)
        Assert.Equal<string>("1", asText inserted.Rows.[0].[0])
        Assert.Equal<string>("A", asText inserted.Rows.[0].[1])
        Assert.Equal<string>("1", asText inserted.Rows.[0].[2])
        Assert.Equal<string>("B", asText inserted.Rows.[0].[3])
        Assert.Equal<string>("1", asText inserted.Rows.[0].[4])
        Assert.Equal<string>("C", asText inserted.Rows.[0].[5]))

[<Fact>]
let ``SqlReference: JOIN の非修飾で曖昧な列は NameError`` () =
    // 非修飾 id は p と q の両方にある。各テーブルへ pushdown すると
    // それぞれ id = 1 で絞ってしまい、曖昧エラーにならず意味が変わる。
    withSession "joinambig" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"

        expectErrorKind
            ErrorKind.NameError
            "JOIN の曖昧な非修飾列がエラーになりません"
            (execSql db2 "SELECT p.id FROM t AS p JOIN t AS q ON p.id = q.id WHERE id = 1"))

[<Fact>]
let ``SqlReference: テーブル名の重複はエラー（エイリアスが必要）`` () =
    // 識別名が重複すると exec が名前でスキーマを引けず（両方が消える）、
    // エラーではなく静かな誤答になるので、名前解決の段階で止める。
    withSession "dupjoin" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10)"

        expectErrorKind
            ErrorKind.NameError
            "重複したテーブル名がエラーになりません"
            (execSql db2 "SELECT t.id FROM t JOIN t ON t.id = t.id"))

[<Fact>]
let ``SqlReference: JOIN の WHERE は右テーブルと横断述語に効く`` () =
    withSession "sel11" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, _ =
            execRes db1 "INSERT INTO people VALUES (1,'alice',30),(2,'bob',25),(3,'carol',40)"

        let db3 =
            exec db2 "CREATE TABLE orders (oid INTEGER PRIMARY KEY, person_id INTEGER, amount INTEGER)"

        let db4, _ = execRes db3 "INSERT INTO orders VALUES (1,1,100),(2,1,200),(3,2,50)"

        // 右テーブルだけを参照する WHERE（pushdown が rTree に残っていること）
        let _, rightOnly =
            ok (
                execSql
                    db4
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE orders.amount = 100"
            )

        Assert.Equal<int>(1, rightOnly.Rows.Length)
        Assert.Equal<int64>(1L, asInt rightOnly.Rows.[0].[0])

        // 非修飾でも一意に決まる列（amount は orders だけ）は pushdown できる
        let _, unqualifiedUnique =
            ok (
                execSql
                    db4
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE amount = 100"
            )

        Assert.Equal<int>(1, unqualifiedUnique.Rows.Length)
        Assert.Equal<int64>(1L, asInt unqualifiedUnique.Rows.[0].[0])

        // 2 テーブルにまたがる WHERE（ON に無い述語が JOIN 上で評価される）
        let _, cross =
            ok (
                execSql
                    db4
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE people.age * 4 < orders.amount"
            )

        Assert.Equal<int>(1, cross.Rows.Length)
        Assert.Equal<int64>(1L, asInt cross.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: LEFT JOIN の WHERE は null 供給側に効く`` () =
    withSession "sel12" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, _ =
            execRes db1 "INSERT INTO people VALUES (1,'alice',30),(2,'bob',25),(3,'carol',40)"

        let db3 =
            exec db2 "CREATE TABLE orders (oid INTEGER PRIMARY KEY, person_id INTEGER, amount INTEGER)"

        let db4, _ = execRes db3 "INSERT INTO orders VALUES (1,1,100),(2,1,200),(3,2,50)"

        // 右への pushdown を入れると全 3 行が NULL 拡張されて 4 行になる
        let _, isNullRows =
            ok (
                execSql
                    db4
                    "SELECT people.id FROM people LEFT JOIN orders ON people.id = orders.person_id WHERE orders.amount IS NULL"
            )

        Assert.Equal<int>(1, isNullRows.Rows.Length)
        Assert.Equal<int64>(3L, asInt isNullRows.Rows.[0].[0])

        // 右への pushdown を入れると carol まで残って 3 行になる
        let _, matched =
            ok (
                execSql
                    db4
                    "SELECT people.id FROM people LEFT JOIN orders ON people.id = orders.person_id WHERE orders.amount = 100"
            )

        Assert.Equal<int>(1, matched.Rows.Length)
        Assert.Equal<int64>(1L, asInt matched.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: セミコロンなしの複数文はエラー`` () =
    withSession "p0-separator" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"

        expectError "セミコロンなしの複数文が OK になっています" (execSql db1 "SELECT id FROM t SELECT id FROM t"))

[<Fact>]
let ``SqlReference: UNION はエラーになり 2 文として実行されない`` () =
    withSession "p0-union" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"

        expectError "UNION がエラーになっていません" (execSql db2 "SELECT id FROM t UNION SELECT id FROM t")

        // 2 文として実行されていれば RowsAffected の合計などが返るが、ここではエラーになる。
        let _, res = ok (execSql db2 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: UNION ALL は両方の行を連結する`` () =
    withSession "union-all" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, v TEXT)"
        let db3 = exec db2 "INSERT INTO a VALUES (1, 'x'), (2, 'y')"
        let db4 = exec db3 "INSERT INTO b VALUES (2, 'y'), (3, 'z')"

        let _, res =
            ok (execSql db4 "SELECT id, v FROM a UNION ALL SELECT id, v FROM b ORDER BY id")

        // 列名は先頭の SELECT のもの。UNION ALL は重複を除かない（(2, 'y') が 2 行）。
        Assert.Equal<string array>([| "id"; "v" |], res.Columns)
        Assert.Equal<int>(4, res.Rows.Length)
        Assert.Equal<int64 list>([ 1L; 2L; 2L; 3L ], res.Rows |> Array.map (fun r -> asInt r.[0]) |> Array.toList)

        // ORDER BY / LIMIT / OFFSET は連結後の結果全体に掛かる
        let _, limited =
            ok (execSql db4 "SELECT id FROM a UNION ALL SELECT id FROM b ORDER BY id DESC LIMIT 2 OFFSET 1")

        Assert.Equal<int64 list>([ 2L; 2L ], limited.Rows |> Array.map (fun r -> asInt r.[0]) |> Array.toList))

[<Fact>]
let ``SqlReference: UNION ALL の列数が違うとエラー`` () =
    withSession "union-all-cols" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY)"
        let db3 = exec db2 "INSERT INTO a VALUES (1, 'x')"
        let db4 = exec db3 "INSERT INTO b VALUES (1)"
        expectMessage "UNION ALL の列数が一致しません" "SELECT id, v FROM a UNION ALL SELECT id FROM b" db4)

[<Fact>]
let ``SqlReference: UNION ALL は派生テーブルと INSERT ... SELECT でも使える`` () =
    withSession "union-all-ctx" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY)"
        let db4 = exec db3 "INSERT INTO a VALUES (1), (2)"
        let db5 = exec db4 "INSERT INTO b VALUES (3)"

        let _, derived =
            ok (execSql db5 "SELECT COUNT(*) FROM (SELECT id FROM a UNION ALL SELECT id FROM b) AS u")

        Assert.Equal<int64>(3L, asInt derived.Rows.[0].[0])

        let db6 = exec db5 "INSERT INTO c SELECT id FROM a UNION ALL SELECT id FROM b"
        let _, count = ok (execSql db6 "SELECT COUNT(*) FROM c")
        Assert.Equal<int64>(3L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: EXPLAIN に UnionAll が出る`` () =
    withSession "union-all-explain" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO a VALUES (1)"

        let _, plan = ok (execSql db2 "EXPLAIN SELECT id FROM a UNION ALL SELECT id FROM a")
        let lines = plan.Rows |> Array.map (fun r -> asText r.[0])
        Assert.Contains(lines, fun line -> line.Contains "UnionAll"))

[<Fact>]
let ``SqlReference: RIGHT JOIN はエラーになり INNER JOIN として実行されない`` () =
    withSession "p0-right-join" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE u (id INTEGER PRIMARY KEY, w TEXT)"
        let db3 = exec db2 "INSERT INTO t VALUES (1)"
        let db4 = exec db3 "INSERT INTO u VALUES (1, 'x'), (2, 'y')"

        expectError "RIGHT JOIN がエラーになっていません" (execSql db4 "SELECT u.id FROM t RIGHT JOIN u ON u.id = 1"))

// ---------------------------------------------------------------------------
// エラー
// ---------------------------------------------------------------------------

[<Fact>]
let ``SqlReference: 存在しないテーブルの SELECT はエラー`` () =
    withSession "err1" (fun db -> expectError "存在しないテーブルの SELECT が OK になっています" (execSql db "SELECT * FROM nothing"))

[<Fact>]
let ``SqlReference: 存在しない列の WHERE はエラー`` () =
    seeded "err2" (fun db ->
        expectError "存在しない列の WHERE が OK になっています" (execSql db "SELECT id FROM people WHERE nope = 1"))

[<Fact>]
let ``SqlReference: 構文エラーはエラー（例外ではない）`` () =
    withSession "err3" (fun db -> expectError "構文エラーが OK になっています" (execSql db "SELEC 1"))

[<Fact>]
let ``SqlReference: NOT NULL 制約違反はエラー`` () =
    withSession "err4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER NOT NULL)"

        expectError "NOT NULL 違反が OK になっています" (execSql db1 "INSERT INTO t (id) VALUES (1)"))

[<Fact>]
let ``SqlReference: QueryResult の表示が使える`` () =
    seeded "disp1" (fun db ->
        let _db15, res = ok (execSql db "SELECT id FROM people ORDER BY id LIMIT 1")
        let lines = res.ToDisplayLines()

        Assert.True(lines |> List.exists (fun l -> l.Contains "id"))
        Assert.True(lines |> List.exists (fun l -> l.Contains "1")))

[<Fact>]
let ``SqlReference: CASE WHEN が評価できる`` () =
    seeded "reg-case" (fun db ->
        let _, res =
            ok (execSql db "SELECT name FROM people WHERE (CASE WHEN age > 26 THEN 1 ELSE 0 END) = 1 ORDER BY name")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: CASE WHEN が射影に書ける`` () =
    seeded "reg-case2" (fun db ->
        let _, res =
            ok (
                execSql
                    db
                    "SELECT name AS n, CASE WHEN age > 26 THEN 'old' ELSE 'young' END AS band FROM people ORDER BY id"
            )

        Assert.Equal<string array>([| "n"; "band" |], res.Columns)
        Assert.Equal<string>("old", asText res.Rows.[0].[1])
        Assert.Equal<string>("young", asText res.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: LIMIT は OFFSET を適用してから取る`` () =
    seeded "reg-limit" (fun db ->
        let _, res = ok (execSql db "SELECT id FROM people ORDER BY id LIMIT 2 OFFSET 1")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0])

        // DESC でも同じこと。
        let _, desc =
            ok (execSql db "SELECT id FROM people ORDER BY id DESC LIMIT 2 OFFSET 1")

        Assert.Equal<int>(2, desc.Rows.Length)
        Assert.Equal<int64>(2L, asInt desc.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt desc.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 索引の等価検索は一致する行をすべて返す`` () =
    withSession "reg-index" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"

        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1,10),(2,20),(3,20),(4,30)")

        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        let _, res = ok (execSql db3 "SELECT id FROM t WHERE g = 20 ORDER BY id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 被覆列の残り述語は fetch の前後に同じ結果を返す`` () =
    // 複合索引 (a, b) の a=1 と b>10 は被覆列、c は被覆外。
    // 被覆列だけを見る conjunct を主キー fetch の前に評価しても結果は変わらない。
    withSession "prefilter-covered" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, c TEXT)"

        let db2 = exec db1 "CREATE INDEX ix_ab ON t (a, b)"

        let db3 =
            exec db2 "INSERT INTO t VALUES (1,1,5,'x'),(2,1,15,'x'),(3,1,20,'y'),(4,2,15,'x')"

        let _, rows = ok (execSql db3 "SELECT c FROM t WHERE a = 1 AND b > 10 AND c = 'x'")
        Assert.Equal<string array>([| "x" |], rows.Rows |> Array.map (fun r -> asText r.[0]))

        // DML の候補走査でも同じ述語を評価する（WHERE は再評価される）。
        let db4, upd =
            ok (execSql db3 "UPDATE t SET c = 'z' WHERE a = 1 AND b > 10 AND c = 'x'")

        Assert.Equal<int>(1, upd.RowsAffected)

        let _, after = ok (execSql db4 "SELECT id, c FROM t ORDER BY id")
        Assert.Equal<int64 list>([ 1L; 2L; 3L; 4L ], after.Rows |> Array.toList |> List.map (fun r -> asInt r.[0]))
        Assert.Equal<string array>([| "x"; "z"; "y"; "x" |], after.Rows |> Array.map (fun r -> asText r.[1])))

[<Fact>]
let ``SqlReference: 索引列を更新すると索引も追従する`` () =
    withSession "reg-index-upd" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1,10),(2,20)")
        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        // 索引キーが変わる更新（古いキーの削除 + 新しいキーの追加）
        let db4, upd = ok (execSql db3 "UPDATE t SET g = 99 WHERE id = 1")
        Assert.Equal<int>(1, upd.RowsAffected)

        let _, hit = ok (execSql db4 "SELECT id FROM t WHERE g = 99")
        Assert.Equal<int>(1, hit.Rows.Length)
        Assert.Equal<int64>(1L, asInt hit.Rows.[0].[0])

        // 古い索引エントリからは復活しない
        let _, miss = ok (execSql db4 "SELECT id FROM t WHERE g = 10")
        Assert.Equal<int>(0, miss.Rows.Length))

[<Fact>]
let ``SqlReference: 負数の主キー等値検索`` () =
    withSession "reg-negpk" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (-5, 50), (5, 50)")
        // 主キー検索（Unary Neg のリテラルから PkLookup が選ばれる）
        let _, res = ok (execSql db2 "SELECT id, v FROM t WHERE id = -5")

        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(-5L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(50L, asInt res.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: Unicode テーブル名でも再オープン後に読める`` () =
    withDir "unicode-reopen" (fun dir ->
        let db0 = openDb dir
        let db1 = exec db0 "CREATE TABLE 住所 (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO 住所 VALUES (1, '東京')"
        ok (db2.Close())

        let db3 = openDb dir
        let result = query db3 "SELECT * FROM 住所"
        Assert.Equal<int>(1, result.Rows.Length)
        Assert.Equal<string>("東京", asText result.Rows.[0].[1])
        ok (db3.Close()))

[<Fact>]
let ``SqlReference: CREATE INDEX IF NOT EXISTS は既存なら成功する`` () =
    withSession "p1-index-if" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_v ON t (v)"
        let db3 = exec db2 "CREATE INDEX IF NOT EXISTS ix_v ON t (v)"

        expectError "IF NOT EXISTS なしの重複はエラー" (execSql db3 "CREATE INDEX ix_v ON t (v)")

        let db4 = exec db3 "INSERT INTO t VALUES (1, 10), (2, 20)"
        let _, found = ok (execSql db4 "SELECT id FROM t WHERE v = 20")
        Assert.Equal<int>(1, found.Rows.Length))

[<Fact>]
let ``SqlReference: LEFT OUTER JOIN の OUTER 綴り`` () =
    withSession "p1-left-outer" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE u (id INTEGER PRIMARY KEY, w TEXT)"
        let db3 = exec db2 "INSERT INTO t VALUES (1), (2)"
        let db4 = exec db3 "INSERT INTO u VALUES (1, 'x')"

        let _, rows =
            ok (execSql db4 "SELECT t.id, u.w FROM t LEFT OUTER JOIN u ON t.id = u.id ORDER BY t.id")

        Assert.Equal<int>(2, rows.Rows.Length)
        Assert.Equal<string>("x", asText rows.Rows.[0].[1])
        Assert.True(SqlValue.isNull rows.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: BEGIN/START/COMMIT WORK/END`` () =
    withSession "p1-txn-spellings" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "BEGIN TRANSACTION"
        let db3 = exec db2 "INSERT INTO t VALUES (1)"
        let db4 = exec db3 "COMMIT WORK"
        let db5 = exec db4 "START TRANSACTION"
        let db6 = exec db5 "INSERT INTO t VALUES (2)"
        let db7 = exec db6 "END"
        let _, count = ok (execSql db7 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 集約 DISTINCT と IS TRUE/FALSE`` () =
    withSession "p1-distinct-truth" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER, b BOOLEAN)"

        let db2 =
            exec db1 "INSERT INTO t VALUES (1, 10, TRUE), (2, 10, FALSE), (3, 20, NULL)"

        let _, agg =
            ok (execSql db2 "SELECT SUM(DISTINCT v), AVG(DISTINCT v), MIN(DISTINCT v), MAX(DISTINCT v) FROM t")

        Assert.Equal(SqlValue.Decimal 30M, agg.Rows.[0].[0])
        Assert.Equal<float>(15.0, asReal agg.Rows.[0].[1])
        Assert.Equal<int64>(10L, asInt agg.Rows.[0].[2])
        Assert.Equal<int64>(20L, asInt agg.Rows.[0].[3])

        let _, isTrue = ok (execSql db2 "SELECT id FROM t WHERE b IS TRUE")
        Assert.Equal<int>(1, isTrue.Rows.Length)

        let _, isFalse = ok (execSql db2 "SELECT id FROM t WHERE b IS FALSE")
        Assert.Equal<int>(1, isFalse.Rows.Length)

        let _, isUnknown = ok (execSql db2 "SELECT id FROM t WHERE b IS UNKNOWN")
        Assert.Equal<int>(1, isUnknown.Rows.Length)

        let _, notUnknown =
            ok (execSql db2 "SELECT id FROM t WHERE b IS NOT UNKNOWN ORDER BY id")

        Assert.Equal<int>(2, notUnknown.Rows.Length)

        let _, notTrue = ok (execSql db2 "SELECT id FROM t WHERE b IS NOT TRUE ORDER BY id")
        Assert.Equal<int>(2, notTrue.Rows.Length))

[<Fact>]
let ``SqlReference: LIKE ESCAPE と CAST`` () =
    withSession "p1-like-cast" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, s TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a%b'), (2, 'axb'), (3, 'a_b')"

        let _, likePct = ok (execSql db2 "SELECT id FROM t WHERE s LIKE 'a!%b' ESCAPE '!'")
        Assert.Equal<int64>(1L, asInt likePct.Rows.[0].[0])

        let _, likeUnderscore =
            ok (execSql db2 "SELECT id FROM t WHERE s LIKE 'a!_b' ESCAPE '!'")

        Assert.Equal<int64>(3L, asInt likeUnderscore.Rows.[0].[0])

        let _, casted =
            ok (execSql db2 "SELECT CAST('42' AS INTEGER), CAST(42 AS TEXT) FROM t LIMIT 1")

        Assert.Equal<int64>(42L, asInt casted.Rows.[0].[0])
        Assert.Equal<string>("42", asText casted.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: 不正な LIKE のパターンは 0 行でもエラー`` () =
    // 定数パターンのコンパイルエラーは、行を 1 つも評価しなくても計画時に検出する。
    withSession "like-pattern-empty" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, s TEXT)"
        expectErrorKind ErrorKind.TypeError "末尾 ESCAPE" (execSql db1 "SELECT * FROM t WHERE s LIKE 'a#' ESCAPE '#'")

        expectErrorKind
            ErrorKind.TypeError
            "2 文字の ESCAPE"
            (execSql db1 "SELECT * FROM t WHERE s LIKE 'a#b' ESCAPE '##'")

        // 正しいパターンは 0 行で成功する。
        let _, empty = ok (execSql db1 "SELECT * FROM t WHERE s LIKE 'a#%' ESCAPE '#'")
        Assert.Equal<int>(0, empty.Rows.Length))

[<Fact>]
let ``SqlReference: COALESCE は非 NULL が見つかった後の引数を評価しない`` () =
    // SQL 標準の COALESCE は CASE と等価。到達しない引数のエラーで落ちない。
    withSession "coalesce-lazy" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1)"

        let _, first = ok (execSql db2 "SELECT COALESCE(1, 1 / 0) FROM t")
        Assert.Equal<int64>(1L, asInt first.Rows.[0].[0])

        let _, second = ok (execSql db2 "SELECT COALESCE(NULL, 2, 1 / 0) FROM t")
        Assert.Equal<int64>(2L, asInt second.Rows.[0].[0])

        let _, allNull = ok (execSql db2 "SELECT COALESCE(NULL, NULL) FROM t")
        Assert.Equal<SqlValue>(SqlValue.Null, allNull.Rows.[0].[0])

        // NULL のときは次の引数を評価するので、0 除算はエラーになる。
        expectErrorKind
            ErrorKind.ConstraintViolation
            "COALESCE の到達する引数のエラー"
            (execSql db2 "SELECT COALESCE(NULL, 1 / 0) FROM t"))

[<Fact>]
let ``SqlReference: LENGTH はサロゲートペアを 1文字として数える`` () =
    withSession "p1-length-surrogate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, s TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a\U0001F600b')"
        let _, rows = ok (execSql db2 "SELECT LENGTH(s) FROM t")

        Assert.Equal<int64>(3L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: LIKE の _ はサロゲートペアを 1文字として扱う`` () =
    withSession "p1-like-surrogate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, s TEXT)"

        let db2 =
            exec db1 "INSERT INTO t VALUES (1, 'a\U0001F600b'), (2, 'a\U0001F600\U0001F600b'), (3, 'a%b')"

        // サロゲートペア 1個は `_` 1個に一致し、2個なら一致しない。
        // 'a%b' も「a + 任意の 1 コードポイント + b」なので 1 に含まれる。
        let _, one = ok (execSql db2 "SELECT id FROM t WHERE s LIKE 'a_b' ORDER BY id")
        Assert.Equal<int>(2, one.Rows.Length)
        Assert.Equal<int64>(1L, asInt one.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt one.Rows.[1].[0])

        let _, two = ok (execSql db2 "SELECT id FROM t WHERE s LIKE 'a__b'")
        Assert.Equal<int>(1, two.Rows.Length)
        Assert.Equal<int64>(2L, asInt two.Rows.[0].[0])

        // ESCAPE 文字自身も 1 コードポイント（サロゲートペア可）
        let _, esc =
            ok (execSql db2 "SELECT id FROM t WHERE s LIKE 'a\U0001F600%b' ESCAPE '\U0001F600'")

        Assert.Equal<int>(1, esc.Rows.Length)
        Assert.Equal<int64>(3L, asInt esc.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: INSERT DEFAULT VALUES と DEFAULT`` () =
    withSession "p1-default-values" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY DEFAULT 1, v INTEGER DEFAULT 5, s TEXT)"

        let db2 = exec db1 "INSERT INTO t DEFAULT VALUES"
        let db3 = exec db2 "INSERT INTO t (id, v) VALUES (2, DEFAULT)"
        let _, rows = ok (execSql db3 "SELECT id, v, s FROM t ORDER BY id")

        Assert.Equal<int>(2, rows.Rows.Length)
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(5L, asInt rows.Rows.[0].[1])
        Assert.True(SqlValue.isNull rows.Rows.[0].[2])
        Assert.Equal<int64>(2L, asInt rows.Rows.[1].[0])
        Assert.Equal<int64>(5L, asInt rows.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: ORDER BY の式と序数`` () =
    withSession "p1-order-expr" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 30), (2, 10), (3, 20)"

        let _, expr = ok (execSql db2 "SELECT id FROM t ORDER BY v + 1")
        Assert.Equal<int64>(2L, asInt expr.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt expr.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt expr.Rows.[2].[0])

        let _, ordinal = ok (execSql db2 "SELECT id, v FROM t ORDER BY 2 DESC")
        Assert.Equal<int64>(1L, asInt ordinal.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt ordinal.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt ordinal.Rows.[2].[0])

        let _, alias =
            ok (execSql db2 "SELECT id, v AS k FROM t ORDER BY k + 1 DESC LIMIT 1")

        Assert.Equal<int64>(1L, asInt alias.Rows.[0].[0])

        // `SELECT *` の序数は展開後の列位置
        let _, starFirst = ok (execSql db2 "SELECT * FROM t ORDER BY 1 DESC")
        Assert.Equal<int64>(3L, asInt starFirst.Rows.[0].[0])

        let _, starSecond = ok (execSql db2 "SELECT * FROM t ORDER BY 2 ASC")
        Assert.Equal<int64>(2L, asInt starSecond.Rows.[0].[0]))

// ---------------------------------------------------------------------------
// sql-reference.md の記載を検証（§1 / §2 / §3 / §4 / §5 / §7 / §8 / §9 / §10 /
// §11 / §12 / §13 / §14）
// ---------------------------------------------------------------------------

[<Fact>]
let ``SqlReference: FROM を省略した SELECT は定数 1 行を返す`` () =
    // §7.1: FROM は省略できる。入力は 1 行 0 列で、式だけを評価する。
    withSession "select-no-from" (fun db0 ->
        let _, one = ok (execSql db0 "SELECT 1")
        Assert.Equal<string array>([| "1" |], one.Columns)
        Assert.Equal<int>(1, one.Rows.Length)
        Assert.Equal<int64>(1L, asInt one.Rows.[0].[0])

        // 別名はヘッダに使われる
        let _, expr = ok (execSql db0 "SELECT 1 + 2 AS three, 'x' AS s")
        Assert.Equal<string array>([| "three"; "s" |], expr.Columns)
        Assert.Equal<int64>(3L, asInt expr.Rows.[0].[0])

        match expr.Rows.[0].[1] with
        | SqlValue.Text s -> Assert.Equal("x", s)
        | v -> failActual "TEXT ではありません" v

        // WHERE は 1 行に対して効く（偽なら 0 行）
        let _, filtered = ok (execSql db0 "SELECT 1 WHERE 1 = 2")
        Assert.Empty filtered.Rows

        // DISTINCT / LIMIT も効く
        let _, distinct = ok (execSql db0 "SELECT DISTINCT 1")
        Assert.Equal<int>(1, distinct.Rows.Length)

        let _, limited = ok (execSql db0 "SELECT 1 LIMIT 0")
        Assert.Empty limited.Rows

        // 集約は 1 行 0 列の入力を 1 行にまとめる
        let _, counted = ok (execSql db0 "SELECT COUNT(*)")
        Assert.Equal<int64>(1L, asInt counted.Rows.[0].[0])

        // サブクエリでも FROM を省略できる
        let _, sub = ok (execSql db0 "SELECT EXISTS (SELECT 1), (SELECT 2)")
        Assert.Equal<int>(1, sub.Rows.Length)

        match sub.Rows.[0].[0], sub.Rows.[0].[1] with
        | SqlValue.Bool b, SqlValue.Int n ->
            Assert.True b
            Assert.Equal(2L, n)
        | a, b -> Assert.Fail(sprintf "想定外の値: %A / %A" a b)

        // 展開する列が無いので * / t.* はエラー
        expectErrorKind ErrorKind.SyntaxError "FROM なしの *" (execSql db0 "SELECT *")
        expectErrorKind ErrorKind.NameError "FROM なしの t.*" (execSql db0 "SELECT t.*")
        expectErrorKind ErrorKind.SyntaxError "FROM なしの JOIN" (execSql db0 "SELECT 1 JOIN t ON 1 = 1"))

[<Fact>]
let ``SqlReference: 数字から始まる識別子は受け付けない`` () =
    // §2: 数字から始められない。`_` 始まりは使える。
    withSession "ident-start" (fun db0 ->
        expectErrorKind ErrorKind.SyntaxError "数字始まりのテーブル名" (execSql db0 "CREATE TABLE 1a (id INTEGER PRIMARY KEY)")

        let db1 = exec db0 "CREATE TABLE _u (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO _u VALUES (1)"
        let _, rows = ok (execSql db2 "SELECT id FROM _u")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 型の別綴りはすべて受理される`` () =
    // §3: BOOLEAN / BOOL, INTEGER / INT / BIGINT, REAL / DOUBLE, DECIMAL / NUMERIC,
    // TEXT, DATE, TIMESTAMP / DATETIME, UUID。
    withSession "type-aliases" (fun db0 ->
        let db1 =
            exec
                db0
                "CREATE TABLE ty (id INTEGER PRIMARY KEY, a BOOL, b INT, c BIGINT, d DOUBLE, e NUMERIC, f TEXT, g DATETIME, h UUID, i BLOB, j DATE)"

        Assert.Equal<string list>([ "ty" ], ok (db1.TableNames()))

        // 文字列で渡した値は列の型へ暗黙変換される（§3）
        let db2 =
            exec
                db1
                "INSERT INTO ty (id, j, g, h) VALUES (1, '2024-01-15', '2024-01-15T10:30:00Z', '0f8fad5b-d9cb-469f-a165-70867728950e')"

        let _, rows = ok (execSql db2 "SELECT j, g, h FROM ty WHERE id = 1")
        Assert.Equal<DataType>(DataType.Date, ok (SqlValue.dataType rows.Rows.[0].[0]))
        Assert.Equal<DataType>(DataType.Timestamp, ok (SqlValue.dataType rows.Rows.[0].[1]))
        Assert.Equal<DataType>(DataType.Uuid, ok (SqlValue.dataType rows.Rows.[0].[2])))

[<Fact>]
let ``SqlReference: DECIMAL(3, 1) は値を丸めない`` () =
    // §3: 型引数は読み取られるだけで、丸めも検証も行わない。
    withSession "typeargs-no-round" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, n DECIMAL(3, 1))"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 12345.6789)"
        let _, rows = ok (execSql db2 "SELECT n FROM t")
        Assert.Equal<SqlValue>(SqlValue.Decimal 12345.6789M, rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 未知の型名は受け付けない`` () =
    // §3: 型の綴りは表のとおり。CHAR と VARCHAR は受け付けない。
    withSession "unknown-type" (fun db0 ->
        expectErrorKind
            ErrorKind.SyntaxError
            "CHAR は未知の型"
            (execSql db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, c CHAR(3))")

        expectErrorKind
            ErrorKind.SyntaxError
            "VARCHAR は未知の型"
            (execSql db0 "CREATE TABLE u (id INTEGER PRIMARY KEY, v VARCHAR(3))"))

[<Fact>]
let ``SqlReference: INSERT の値式は列参照を含められない`` () =
    // §4: 値は定数式のみ。列参照は名前解決でエラーになる。
    withSession "insert-column-ref" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT, n INTEGER)"
        let db2, ins = ok (execSql db1 "INSERT INTO t (id, v, n) VALUES (1, 'a', 2 + 3)")
        Assert.Equal<int>(1, ins.RowsAffected)

        expectErrorKind ErrorKind.NameError "VALUES の列参照" (execSql db2 "INSERT INTO t (id, v, n) VALUES (2, v, n)")

        // 失敗した 1 文は残らない
        let _, rows = ok (execSql db2 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: SELECT 項目の後置演算子`` () =
    // §5: SELECT 項目は式として解析されるので、裸の識別子・関数の後に演算子を書ける。
    withSession "select-postfix" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT, n INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a', 1)"

        let _, cat = ok (execSql db2 "SELECT v || 'x' AS c FROM t")
        Assert.Equal<string array>([| "c" |], cat.Columns)
        Assert.Equal<string>("ax", asText cat.Rows.[0].[0])

        let _, sum = ok (execSql db2 "SELECT SUM(n) + 0.5 AS s FROM t")
        Assert.Equal(1.5, asReal sum.Rows.[0].[0], 10))

[<Fact>]
let ``SqlReference: 式の列名は丸括弧つきの表示文字列`` () =
    // §7.3: `SELECT (1 + 2)` の列名は `(1 + 2)`（常に丸括弧つき）。
    withSession "expr-column-name" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"
        let _, r = ok (execSql db2 "SELECT (1 + 2), v || 'x' FROM t")
        Assert.Equal<string array>([| "(1 + 2)"; "(v || 'x')" |], r.Columns))

[<Fact>]
let ``SqlReference: ORDER BY は集約式・集約の別名・序数・GROUP BY 式で並べ替えられる`` () =
    // §7.3: 集約後の ORDER BY は GROUP BY 値と集約値を参照できる。
    withSession "order-by-agg" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 1), (2, 1), (3, 2)"

        let _, agg =
            ok (execSql db2 "SELECT g, COUNT(*) FROM t GROUP BY g ORDER BY COUNT(*) DESC")

        Assert.Equal<int64>(1L, asInt agg.Rows.[0].[0])
        Assert.Equal<int64>(2L, asInt agg.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt agg.Rows.[1].[0])

        let _, alias =
            ok (execSql db2 "SELECT g, COUNT(*) AS c FROM t GROUP BY g ORDER BY c DESC")

        Assert.Equal<int64>(1L, asInt alias.Rows.[0].[0])

        let _, ordinal =
            ok (execSql db2 "SELECT g, COUNT(*) FROM t GROUP BY g ORDER BY 2 DESC")

        Assert.Equal<int64>(1L, asInt ordinal.Rows.[0].[0])

        // GROUP BY 式も ORDER BY（式・序数）で参照できる
        let _, groupExpr =
            ok (execSql db2 "SELECT g + 10, COUNT(*) FROM t GROUP BY g + 10 ORDER BY g + 10 DESC")

        Assert.Equal<int64>(12L, asInt groupExpr.Rows.[0].[0])

        let _, groupExprOrdinal =
            ok (execSql db2 "SELECT g + 10, COUNT(*) FROM t GROUP BY g + 10 ORDER BY 1")

        Assert.Equal<int64>(11L, asInt groupExprOrdinal.Rows.[0].[0])

        // 集約外の列は従来どおり NameError
        expectErrorKind
            ErrorKind.NameError
            "GROUP BY に無い列の ORDER BY"
            (execSql db2 "SELECT g, COUNT(*) FROM t GROUP BY g ORDER BY id"))

[<Fact>]
let ``SqlReference: ORDER BY の集約は SELECT に無くても並べ替えに効く`` () =
    // SELECT に無い ORDER BY の集約を未修飾の式で集めると、集約後のスロットに一致せず
    // NULL へ置換され、並べ替えが無効になっていた（§7.6）。
    withSession "order-by-agg-unselected" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (a INTEGER PRIMARY KEY, b INTEGER)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (2, 1), (1, 100), (3, 100)")

        let _, sum = ok (execSql db2 "SELECT a FROM t GROUP BY a ORDER BY SUM(b) DESC, a")
        // SUM(b) は 1:100 / 2:1 / 3:100。同点は a で並べる。
        Assert.Equal<int64 array>([| 1L; 3L; 2L |], sum.Rows |> Array.map (fun r -> asInt r.[0]))

        let _, count =
            ok (execSql db2 "SELECT a FROM t GROUP BY a ORDER BY COUNT(b) DESC, a")

        Assert.Equal<int64 array>([| 1L; 2L; 3L |], count.Rows |> Array.map (fun r -> asInt r.[0]))

        // SELECT に同じ集約がある場合も同じ結果
        let _, withSelect =
            ok (execSql db2 "SELECT a, SUM(b) FROM t GROUP BY a ORDER BY SUM(b) DESC, a")

        Assert.Equal<int64 array>([| 1L; 3L; 2L |], withSelect.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: ORDER BY の修飾列が無い場合は別の列で並べない`` () =
    withSession "order-by-qualified" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE u (x INTEGER PRIMARY KEY)"
        let db3, _ = ok (execSql db2 "INSERT INTO t VALUES (1, 50), (2, 100)")
        let db4, _ = ok (execSql db3 "INSERT INTO u VALUES (1)")

        // u はあるが u.v は無い。裸の v（t.v）で並べない。
        expectErrorKind
            ErrorKind.NameError
            "存在しない修飾列"
            (execSql db4 "SELECT t.v FROM t JOIN u ON u.x = 1 ORDER BY u.v DESC")

        // 別名を付けたテーブルを実名で修飾しても解決しない（§5）。
        expectErrorKind ErrorKind.NameError "別名を実名で修飾" (execSql db4 "SELECT v FROM t AS x ORDER BY t.v DESC")

        // 正しい修飾はそのまま使える。
        let _, rows =
            ok (execSql db4 "SELECT t.v FROM t JOIN u ON u.x = 1 ORDER BY t.v DESC")

        Assert.Equal<int64 array>([| 100L; 50L |], rows.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: WHERE の集約は 0 行でも計画時にエラー`` () =
    withSession "aggregate-in-where" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        // 空のテーブルでも黙って 0 行を返さない。
        expectErrorKind ErrorKind.NameError "空表の WHERE 集約" (execSql db1 "SELECT * FROM t WHERE COUNT(*) > 0")

        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1)")
        expectErrorKind ErrorKind.NameError "非空表の WHERE 集約" (execSql db2 "SELECT * FROM t WHERE COUNT(*) > 0")

        expectErrorKind
            ErrorKind.NameError
            "JOIN の ON の集約"
            (execSql db2 "SELECT * FROM t AS a JOIN t AS b ON COUNT(*) > 0"))

[<Fact>]
let ``SqlReference: GROUP BY の IN サブクエリは集合ごとに別のキーになる`` () =
    // 式のキーが「左辺 + 否定」だけだと、内容の違う IN サブクエリが同じグループ列に
    // まとまり、SELECT の値が取り違えられていた。
    withSession "group-by-in-set" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (x INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2), (3)"
        let db3 = exec db2 "CREATE TABLE s1 (v INTEGER PRIMARY KEY)"
        let db4 = exec db3 "CREATE TABLE s2 (v INTEGER PRIMARY KEY)"
        let db5, _ = ok (execSql db4 "INSERT INTO s1 VALUES (1)")
        let db6, _ = ok (execSql db5 "INSERT INTO s2 VALUES (2)")

        let asBool v =
            match v with
            | SqlValue.Bool b -> b
            | _ -> failwithf "BOOLEAN ではありません: %A" v

        let _, rows =
            ok (
                execSql
                    db6
                    "SELECT x IN (SELECT v FROM s1), x IN (SELECT v FROM s2), COUNT(*) FROM t GROUP BY x IN (SELECT v FROM s1), x IN (SELECT v FROM s2)"
            )

        Assert.Equal<int>(3, rows.Rows.Length)

        let got =
            rows.Rows
            |> Array.map (fun r -> asBool r.[0], asBool r.[1], asInt r.[2])
            |> Array.sort

        Assert.Equal<(bool * bool * int64) array>([| (false, false, 1L); (false, true, 1L); (true, false, 1L) |], got)

        // SELECT と GROUP BY に同じ構文で書いたサブクエリは同じスロットに一致する。
        let _, same =
            ok (execSql db6 "SELECT x IN (SELECT v FROM s1) AS in1, COUNT(*) FROM t GROUP BY x IN (SELECT v FROM s1)")

        Assert.Equal<(bool * int64) array>(
            [| (false, 2L); (true, 1L) |],
            same.Rows |> Array.map (fun r -> asBool r.[0], asInt r.[1]) |> Array.sort
        ))

[<Fact>]
let ``SqlReference: 相関サブクエリは結合順を並べ替えても外側行を取り違えない`` () =
    // LEFT JOIN の後に書いた INNER は前に繰り上げられる（§7.5）。
    // 評価行は FROM 順ではなく実行順になるため、外側行を FROM 順へ詰め替えないと
    // 外側参照が別の列を読む。
    withSession "correlated-hoist" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, x INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, z INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, a_id INTEGER)"
        let db4 = exec db3 "CREATE TABLE d (id INTEGER PRIMARY KEY, v INTEGER)"
        let db5, _ = ok (execSql db4 "INSERT INTO a VALUES (1, 10)")
        let db6, _ = ok (execSql db5 "INSERT INTO b VALUES (1, 99)")
        let db7, _ = ok (execSql db6 "INSERT INTO c VALUES (1, 10)")
        let db8, _ = ok (execSql db7 "INSERT INTO d VALUES (1, 10)")

        // b.z = 99 なので d.v = 10 の行は一致しない（0 行が正しい）。
        let _, exists =
            ok (
                execSql
                    db8
                    "SELECT a.id FROM a LEFT JOIN b ON b.id = a.id JOIN c ON c.a_id = a.x WHERE EXISTS (SELECT 1 FROM d WHERE d.v = b.z)"
            )

        Assert.Equal<int>(0, exists.Rows.Length)

        // スカラー相関も同じ外側行を読む。
        let _, scalar =
            ok (
                execSql
                    db8
                    "SELECT a.id, (SELECT MAX(d.v) FROM d WHERE d.v = b.z) AS mx FROM a LEFT JOIN b ON b.id = a.id JOIN c ON c.a_id = a.x"
            )

        Assert.Equal<int>(1, scalar.Rows.Length)
        Assert.Equal<int64>(1L, asInt scalar.Rows.[0].[0])

        match scalar.Rows.[0].[1] with
        | SqlValue.Null -> ()
        | other -> failwithf "NULL ではありません: %A" other)

[<Fact>]
let ``SqlReference: 集約があるのに GROUP BY に無い列を選ぶとエラー`` () =
    // 上のテストの逆（集約式の ORDER BY 自体は使えるようになった）。
    withSession "order-by-agg2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"
        expectErrorKind ErrorKind.NameError "ORDER BY COUNT(*)" (execSql db2 "SELECT id FROM t ORDER BY COUNT(*)"))

[<Fact>]
let ``SqlReference: ORDER BY は非有限 REAL と他型の混在を検出する`` () =
    // 代表値が有限 REAL だと INTEGER vs ±Infinity の比較不能を見逃し、
    // 比較エラーを 0 として扱って誤った順序を返していた。
    withSession "order-by-mixed-inf" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1)"

        expectErrorKind
            ErrorKind.TypeError
            "混在型の ORDER BY"
            (execSql db2 "SELECT 1.5 AS x UNION ALL SELECT 1 UNION ALL SELECT 1e999 ORDER BY x")

        // 有限の数値同士は従来どおり並べ替えられる。
        let _, finite =
            ok (execSql db2 "SELECT 1.5 AS x UNION ALL SELECT 1 UNION ALL SELECT 0.5 ORDER BY x")

        Assert.Equal<SqlValue>(SqlValue.Real 0.5, finite.Rows.[0].[0])
        Assert.Equal<SqlValue>(SqlValue.Int 1L, finite.Rows.[1].[0])
        Assert.Equal<SqlValue>(SqlValue.Real 1.5, finite.Rows.[2].[0]))

[<Fact>]
let ``SqlReference: ORDER BY の序数は範囲外だとエラー`` () =
    // §7.3: 序数は 1 始まり。範囲外は黙って切らずに SyntaxError。
    withSession "order-by-ordinal" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        let _, ok2 = ok (execSql db2 "SELECT id, v FROM t ORDER BY 2 DESC")
        Assert.Equal<int64>(2L, asInt ok2.Rows.[0].[0])

        expectErrorKind ErrorKind.SyntaxError "序数 0" (execSql db2 "SELECT id, v FROM t ORDER BY 0")
        expectErrorKind ErrorKind.SyntaxError "序数 99" (execSql db2 "SELECT id, v FROM t ORDER BY 99"))

[<Fact>]
let ``SqlReference: 主キーの重複 INSERT はエラー（upsert しない）`` () =
    // §8: 主キー重複はエラーになる（複数列の主キーも同じ）。
    withSession "pk-duplicate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"

        expectErrorKind ErrorKind.ConstraintViolation "主キーの重複 INSERT" (execSql db2 "INSERT INTO t VALUES (1, 'b')")

        // 覆っていないこと（upsert しない）
        let _, rows = ok (execSql db2 "SELECT v FROM t WHERE id = 1")
        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<string>("a", asText rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 同じ INSERT 文内の主キー重複はエラー`` () =
    // 文の途中で同じ主キーを書いても、後の行で上書きしない。
    withSession "pk-duplicate-same-stmt" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, u TEXT UNIQUE, v INTEGER)"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "同一文内の主キー重複"
            (execSql db1 "INSERT INTO t VALUES (1,'a',10),(1,'b',20)")

        let _, count = ok (execSql db1 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt count.Rows.[0].[0])

        // 失敗した文の索引エントリも残らない。
        let _, byIndex = ok (execSql db1 "SELECT id, u, v FROM t WHERE u = 'a'")
        Assert.Empty byIndex.Rows)

[<Fact>]
let ``SqlReference: INSERT ... SELECT の生成行が主キー重複ならエラー`` () =
    withSession "pk-duplicate-select" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE s (x INTEGER PRIMARY KEY)"
        let db3 = exec db2 "INSERT INTO s VALUES (10), (20)"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "INSERT ... SELECT の生成行の主キー重複"
            (execSql db3 "INSERT INTO t (id, v) SELECT 1, x FROM s")

        let _, count = ok (execSql db3 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: UPDATE は主キー列を変えられない`` () =
    // §9: 主キー列を SET するとエラーになり、何も更新されない。
    withSession "pk-update" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        expectErrorKind ErrorKind.ConstraintViolation "主キー列の UPDATE" (execSql db2 "UPDATE t SET id = 100 WHERE id = 1")

        let _, rows = ok (execSql db2 "SELECT id FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 1L; 2L |], rows.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: UPDATE の式は更新前の行に対して評価する`` () =
    // §9: 式は更新前の行に対して評価される。
    withSession "update-pre-row" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10, 'x')"

        let db3, upd =
            ok (execSql db2 "UPDATE t SET n = n + 1, v = CAST(n AS TEXT) WHERE id = 1")

        Assert.Equal<int>(1, upd.RowsAffected)

        let _, rows = ok (execSql db3 "SELECT n, v FROM t WHERE id = 1")
        // v は「更新前」の n = 10 を見る
        Assert.Equal<int64>(11L, asInt rows.Rows.[0].[0])
        Assert.Equal<string>("10", asText rows.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: DELETE は WHERE を省略すると全行を消す`` () =
    // §10
    withSession "delete-all" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')"
        let db3, del = ok (execSql db2 "DELETE FROM t")
        Assert.Equal<int>(3, del.RowsAffected)

        let _, rows = ok (execSql db3 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 主キーの無い CREATE TABLE はエラー`` () =
    // §11.1: 主キーは省略できない。
    withSession "pk-required" (fun db0 ->
        expectErrorKind ErrorKind.ConstraintViolation "主キー無しの CREATE TABLE" (execSql db0 "CREATE TABLE t (a INTEGER)")
        Assert.Empty(ok (db0.TableNames())))

[<Fact>]
let ``SqlReference: PRIMARY KEY の定義は 1 テーブルに 1 つだけ`` () =
    // §11.1: 列レベルとテーブルレベルの併用、複数列への列レベル、
    // 複数のテーブルレベルはいずれも ConstraintViolation。
    withSession "pk-once" (fun db0 ->
        expectErrorKind
            ErrorKind.ConstraintViolation
            "列レベルとテーブルレベルの併用"
            (execSql db0 "CREATE TABLE k1 (id INTEGER PRIMARY KEY, n TEXT, PRIMARY KEY (id))")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "複数列への列レベル PRIMARY KEY"
            (execSql db0 "CREATE TABLE k2 (a INTEGER PRIMARY KEY, b TEXT PRIMARY KEY)")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "複数のテーブルレベル PRIMARY KEY"
            (execSql db0 "CREATE TABLE k3 (a INTEGER, b TEXT, PRIMARY KEY (a), PRIMARY KEY (b))")

        // 複数列のテーブルレベルは 1 定義なら通る
        let db1 = exec db0 "CREATE TABLE k4 (a INTEGER, b TEXT, PRIMARY KEY (a, b))"
        Assert.Equal<string list>([ "k4" ], ok (db1.TableNames())))

[<Fact>]
let ``SqlReference: 型を省略した列定義は TEXT になる`` () =
    // §11.1: 型を書かずに制約だけを書く構文も受け付ける（型は TEXT）。
    withSession "column-type-omitted" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, name NOT NULL)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'zz')"
        // TEXT 列なので数値も文字列表現へ変換される
        let db3 = exec db2 "INSERT INTO t VALUES (2, 123)"

        let _, rows = ok (execSql db3 "SELECT name FROM t ORDER BY id")
        Assert.Equal<string>("zz", asText rows.Rows.[0].[0])
        Assert.Equal<string>("123", asText rows.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: CREATE TABLE IF EXISTS は エラー（IF NOT EXISTS を使う）`` () =
    // §11.1: CREATE は IF NOT EXISTS、DROP は IF EXISTS だけを受理する。
    // 逆の綴りを黙って受け入れると DROP TABLE IF NOT EXISTS が削除を実行してしまう。
    withSession "create-table-if-exists" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"

        expectErrorKind
            ErrorKind.SyntaxError
            "CREATE TABLE IF EXISTS"
            (execSql db1 "CREATE TABLE IF EXISTS t (id INTEGER PRIMARY KEY, v TEXT)")

        expectErrorKind ErrorKind.SyntaxError "DROP TABLE IF NOT EXISTS" (execSql db1 "DROP TABLE IF NOT EXISTS t")

        // 逆の綴りがエラーでも、正しい綴りは従来どおり効く。
        let db2 = exec db1 "CREATE TABLE IF NOT EXISTS t (id INTEGER PRIMARY KEY, v TEXT)"
        Assert.Equal<string list>([ "t" ], ok (db2.TableNames())))

[<Fact>]
let ``SqlReference: テーブルレベル UNIQUE の列を検証する`` () =
    // §11.1: 存在しない列・重複列は NameError で拒否する。
    withSession "table-unique-columns" (fun db0 ->
        expectErrorKind
            ErrorKind.NameError
            "UNIQUE の存在しない列"
            (execSql db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a TEXT, UNIQUE (nope))")

        expectErrorKind
            ErrorKind.NameError
            "UNIQUE の重複列"
            (execSql db0 "CREATE TABLE u (id INTEGER PRIMARY KEY, a TEXT, UNIQUE (a, a))")

        let db1 =
            exec db0 "CREATE TABLE v (id INTEGER PRIMARY KEY, a TEXT, b TEXT, UNIQUE (a, b))"

        Assert.Equal<string list>([ "v" ], ok (db1.TableNames())))

[<Fact>]
let ``SqlReference: SQL の BEGIN と ROLLBACK で書き込みを取り消す`` () =
    // §12 の例どおり。BEGIN / COMMIT / ROLLBACK は暗黙の transaction を制御する。
    withSession "sql-txn" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"

        let db3 = exec db2 "BEGIN"
        let db4 = exec db3 "INSERT INTO t VALUES (2, 'b')"
        let db5 = exec db4 "ROLLBACK"
        let _, rolled = ok (execSql db5 "SELECT id FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 1L |], rolled.Rows |> Array.map (fun r -> asInt r.[0]))

        let db6 = exec db5 "BEGIN"
        let db7 = exec db6 "INSERT INTO t VALUES (3, 'c')"
        let db8 = exec db7 "COMMIT"
        let _, committed = ok (execSql db8 "SELECT id FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 1L; 3L |], committed.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: サブクエリ（スカラー・IN・EXISTS）`` () =
    withSession "subquery" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20),(3,30)"

        // スカラーサブクエリ（SELECT 項目 / WHERE）
        let _, rows =
            ok (
                execSql
                    db2
                    "SELECT id, (SELECT MAX(v) FROM t) AS mx FROM t WHERE v > (SELECT AVG(v) FROM t) ORDER BY id"
            )

        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(3L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(30L, asInt rows.Rows.[0].[1])

        // 0 行のスカラーサブクエリは NULL
        let _, none =
            ok (execSql db2 "SELECT id, (SELECT v FROM t WHERE id = 99) AS x FROM t WHERE id = 1")

        Assert.True(SqlValue.isNull none.Rows.[0].[1])

        // IN / NOT IN
        let _, inRows =
            ok (execSql db2 "SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE v >= 20) ORDER BY id")

        Assert.Equal<int>(2, inRows.Rows.Length)
        Assert.Equal<int64>(2L, asInt inRows.Rows.[0].[0])

        let _, emptyIn =
            ok (execSql db2 "SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE id = 99)")

        Assert.Equal<int>(0, emptyIn.Rows.Length)

        let _, emptyNotIn =
            ok (execSql db2 "SELECT id FROM t WHERE id NOT IN (SELECT id FROM t WHERE id = 99) ORDER BY id")

        Assert.Equal<int>(3, emptyNotIn.Rows.Length)

        // NULL を含む IN / NOT IN は UNKNOWN（どちらも 0 行）
        let _, nullIn = ok (execSql db2 "SELECT id FROM t WHERE id IN (SELECT NULL FROM t)")
        Assert.Equal<int>(0, nullIn.Rows.Length)

        let _, nullNotIn =
            ok (execSql db2 "SELECT id FROM t WHERE id NOT IN (SELECT NULL FROM t)")

        Assert.Equal<int>(0, nullNotIn.Rows.Length)

        // EXISTS / NOT EXISTS
        let _, exists =
            ok (execSql db2 "SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t WHERE v > 25) ORDER BY id")

        Assert.Equal<int>(3, exists.Rows.Length)

        let _, notExists =
            ok (execSql db2 "SELECT id FROM t WHERE NOT EXISTS (SELECT 1 FROM t WHERE v > 25)")

        Assert.Equal<int>(0, notExists.Rows.Length)

        let _, missing =
            ok (execSql db2 "SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t WHERE id = 99)")

        Assert.Equal<int>(0, missing.Rows.Length)

        let _, notMissing =
            ok (execSql db2 "SELECT id FROM t WHERE NOT EXISTS (SELECT 1 FROM t WHERE id = 99)")

        Assert.Equal<int>(3, notMissing.Rows.Length)

        // 入れ子のサブクエリ
        let _, nested =
            ok (
                execSql
                    db2
                    "SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE v > (SELECT MIN(v) FROM t)) ORDER BY id"
            )

        Assert.Equal<int>(2, nested.Rows.Length)
        Assert.Equal<int64>(2L, asInt nested.Rows.[0].[0])

        // LIMIT / OFFSET の非相関サブクエリは定数へ解決される
        let _, limited = ok (execSql db2 "SELECT id FROM t ORDER BY id LIMIT (SELECT 2)")
        Assert.Equal<int>(2, limited.Rows.Length)

        // HAVING / JOIN の ON でも使える
        let _, having =
            ok (execSql db2 "SELECT id FROM t GROUP BY id HAVING id > (SELECT MIN(id) FROM t) ORDER BY id")

        Assert.Equal<int>(2, having.Rows.Length)
        Assert.Equal<int64>(2L, asInt having.Rows.[0].[0])

        let _, joined =
            ok (
                execSql
                    db2
                    "SELECT a.id FROM t AS a JOIN t AS b ON a.id = b.id AND EXISTS (SELECT 1 FROM t WHERE v > 25) ORDER BY a.id"
            )

        Assert.Equal<int>(3, joined.Rows.Length)

        // DML 内のサブクエリ
        let db3 = exec db2 "UPDATE t SET v = v + (SELECT MIN(v) FROM t) WHERE id = 1"
        let _, updated = ok (execSql db3 "SELECT v FROM t WHERE id = 1")
        Assert.Equal<int64>(20L, asInt updated.Rows.[0].[0])

        let db4 = exec db3 "INSERT INTO t VALUES (4, (SELECT MAX(v) FROM t))"
        let _, inserted = ok (execSql db4 "SELECT v FROM t WHERE id = 4")
        Assert.Equal<int64>(30L, asInt inserted.Rows.[0].[0])

        let db5 = exec db4 "DELETE FROM t WHERE id IN (SELECT id FROM t WHERE v >= 30)"
        let _, count = ok (execSql db5 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 相関サブクエリ`` () =
    withSession "correlated" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE emp (id INTEGER PRIMARY KEY, dept TEXT, salary INTEGER)"

        let db2 = exec db1 "CREATE TABLE dept (name TEXT PRIMARY KEY, budget INTEGER)"
        let db3 = exec db2 "INSERT INTO emp VALUES (1,'a',10),(2,'a',20),(3,'b',30)"
        let db4 = exec db3 "INSERT INTO dept VALUES ('a',0),('b',100)"

        // EXISTS（外側の列を参照）
        let _, exists =
            ok (
                execSql
                    db4
                    "SELECT e.id FROM emp AS e WHERE EXISTS (SELECT 1 FROM emp AS x WHERE x.dept = e.dept AND x.salary > 25) ORDER BY e.id"
            )

        Assert.Equal<int64 array>([| 3L |], exists.Rows |> Array.map (fun r -> asInt r.[0]))

        // スカラー（外側の値ごとに集約）
        let _, scalar =
            ok (
                execSql
                    db4
                    "SELECT d.name, (SELECT MAX(e.salary) FROM emp AS e WHERE e.dept = d.name) AS mx FROM dept AS d ORDER BY d.name"
            )

        Assert.Equal<int>(2, scalar.Rows.Length)
        Assert.Equal<string>("a", asText scalar.Rows.[0].[0])
        Assert.Equal<int64>(20L, asInt scalar.Rows.[0].[1])
        Assert.Equal<string>("b", asText scalar.Rows.[1].[0])
        Assert.Equal<int64>(30L, asInt scalar.Rows.[1].[1])

        // IN（外側の行ごとに対象が変わる）
        let _, inRows =
            ok (
                execSql
                    db4
                    "SELECT e.id FROM emp AS e WHERE e.salary + 10 IN (SELECT x.salary FROM emp AS x WHERE x.dept = e.dept) ORDER BY e.id"
            )

        Assert.Equal<int64 array>([| 1L |], inRows.Rows |> Array.map (fun r -> asInt r.[0]))

        // 相関 IN でも NULL は UNKNOWN（どちらも 0 行）
        let _, nullIn =
            ok (execSql db4 "SELECT e.id FROM emp AS e WHERE e.id IN (SELECT NULL WHERE e.id = e.id)")

        Assert.Equal<int>(0, nullIn.Rows.Length)

        // 集約クエリの WHERE では相関を使える（集約後ではないため）
        let _, grouped =
            ok (
                execSql
                    db4
                    "SELECT dept, COUNT(*) FROM emp AS e WHERE EXISTS (SELECT 1 FROM emp AS x WHERE x.dept = e.dept AND x.salary > 15) GROUP BY dept ORDER BY dept"
            )

        Assert.Equal<int>(2, grouped.Rows.Length)
        Assert.Equal<int64>(2L, asInt grouped.Rows.[0].[1])
        Assert.Equal<int64>(1L, asInt grouped.Rows.[1].[1])

        // NOT EXISTS（外側の列を参照）
        let _, notExists =
            ok (
                execSql
                    db4
                    "SELECT e.id FROM emp AS e WHERE NOT EXISTS (SELECT 1 FROM emp AS x WHERE x.dept = e.dept AND x.salary > 25) ORDER BY e.id"
            )

        Assert.Equal<int64 array>([| 1L; 2L |], notExists.Rows |> Array.map (fun r -> asInt r.[0]))

        // 修飾子なしの外側列（内側に無い列は外側へフォールバックする）
        let _, unqualified =
            ok (
                execSql
                    db4
                    "SELECT e.id FROM emp AS e WHERE EXISTS (SELECT 1 FROM dept AS d WHERE d.name <> 'x' AND salary > 15) ORDER BY e.id"
            )

        Assert.Equal<int64 array>([| 2L; 3L |], unqualified.Rows |> Array.map (fun r -> asInt r.[0]))

        // 押し下げ禁止: 相関参照が JOIN の 2 番目のテーブルを指す
        let _, joined =
            ok (
                execSql
                    db4
                    "SELECT a.id FROM emp AS a JOIN dept AS b ON b.name = a.dept WHERE EXISTS (SELECT 1 FROM emp AS c WHERE c.salary > b.budget) ORDER BY a.id"
            )

        Assert.Equal<int64 array>([| 1L; 2L |], joined.Rows |> Array.map (fun r -> asInt r.[0]))

        // 入れ子（内側がさらに内側の外側を参照）
        let _, nested =
            ok (
                execSql
                    db4
                    "SELECT d.name FROM dept AS d WHERE EXISTS (SELECT 1 FROM emp AS e WHERE e.dept = d.name AND EXISTS (SELECT 1 FROM emp AS x WHERE x.dept = e.dept AND x.salary > 25)) ORDER BY d.name"
            )

        Assert.Equal<string array>([| "b" |], nested.Rows |> Array.map (fun r -> asText r.[0]))

        // 入れ子（内側が 2段上の外側を直接参照）
        let _, grandparent =
            ok (
                execSql
                    db4
                    "SELECT d.name FROM dept AS d WHERE EXISTS (SELECT 1 FROM emp AS e WHERE e.dept = d.name AND EXISTS (SELECT 1 FROM emp AS x WHERE x.dept = d.name AND x.salary > 25)) ORDER BY d.name"
            )

        Assert.Equal<string array>([| "b" |], grandparent.Rows |> Array.map (fun r -> asText r.[0]))

        // ORDER BY でも使える
        let _, ordered =
            ok (execSql db4 "SELECT e.id FROM emp AS e ORDER BY (SELECT x.salary FROM emp AS x WHERE x.id = e.id)")

        Assert.Equal<int64 array>([| 1L; 2L; 3L |], ordered.Rows |> Array.map (fun r -> asInt r.[0]))

        // UPDATE の外側の行を参照する
        let db5 =
            exec db4 "UPDATE emp SET salary = salary + (SELECT COUNT(*) FROM emp AS x WHERE x.dept = emp.dept)"

        let _, updated = ok (execSql db5 "SELECT id, salary FROM emp ORDER BY id")
        Assert.Equal<int64>(12L, asInt updated.Rows.[0].[1])
        Assert.Equal<int64>(22L, asInt updated.Rows.[1].[1])
        Assert.Equal<int64>(31L, asInt updated.Rows.[2].[1])

        // DELETE の外側の行を参照する
        let db6 =
            exec db5 "DELETE FROM emp WHERE salary < (SELECT AVG(x.salary) FROM emp AS x WHERE x.dept = emp.dept)"

        let _, remaining = ok (execSql db6 "SELECT id FROM emp ORDER BY id")
        Assert.Equal<int64 array>([| 2L; 3L |], remaining.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: UNION ALL の枝を持つ相関サブクエリは枝ごとに外側参照を判定する`` () =
    // 枝 2 に外側と同じ名前の別名（o）があると、全枝の入力を連結したスキーマでは
    // 枝 1 の o.id を「内側」と誤判定し、実行時に NameError になっていた。
    withSession "correlated-union-branches" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE u (id INTEGER PRIMARY KEY)"
        let db3, _ = ok (execSql db2 "INSERT INTO t VALUES (1),(2)")
        let db4, _ = ok (execSql db3 "INSERT INTO u VALUES (1),(2)")

        let ids sql =
            let _, rows = ok (execSql db4 sql)
            rows.Rows |> Array.map (fun row -> asInt row.[0])

        let inRows =
            ids
                "SELECT o.id FROM t AS o WHERE o.id IN (SELECT x.id FROM u AS x WHERE x.id = o.id UNION ALL SELECT o.id FROM u AS o WHERE o.id = 2) ORDER BY o.id"

        Assert.Equal<int64 array>([| 1L; 2L |], inRows)

        let existsRows =
            ids
                "SELECT o.id FROM t AS o WHERE EXISTS (SELECT 1 FROM u AS x WHERE x.id = o.id UNION ALL SELECT 1 FROM u AS o WHERE o.id = 100) ORDER BY o.id"

        Assert.Equal<int64 array>([| 1L; 2L |], existsRows))

[<Fact>]
let ``SqlReference: 相関サブクエリの未対応形`` () =
    withSession "correlated-errors" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"

        // LIMIT / OFFSET は外側の行を持たない
        expectErrorKind
            ErrorKind.SyntaxError
            "LIMIT の相関サブクエリ"
            (execSql db2 "SELECT id FROM t AS o LIMIT (SELECT o.id FROM t AS x)")

        // 集約後の SELECT / HAVING / ORDER BY も外側の行を持たない
        expectErrorKind
            ErrorKind.SyntaxError
            "集約クエリの SELECT の相関サブクエリ"
            (execSql db2 "SELECT (SELECT MAX(x.v) FROM t AS x WHERE x.v = o.v) FROM t AS o GROUP BY o.id")

        // GROUP BY も外側の行を持たない
        expectErrorKind
            ErrorKind.SyntaxError
            "GROUP BY の相関サブクエリ"
            (execSql db2 "SELECT o.id FROM t AS o GROUP BY (SELECT x.v FROM t AS x WHERE x.id = o.id)")

        // 相関スカラーが外側行ごとに 2 行以上を返す
        expectErrorKind
            ErrorKind.ConstraintViolation
            "相関スカラーが 2 行以上"
            (execSql db2 "SELECT (SELECT x.v FROM t AS x WHERE x.id >= o.id) FROM t AS o")

        // 後から JOIN するテーブルを ON から参照すると NameError（範囲外アクセスにしない）
        expectErrorKind
            ErrorKind.NameError
            "後続 JOIN の相関参照"
            (execSql
                db2
                "SELECT a.id FROM t AS a JOIN t AS b ON EXISTS (SELECT 1 FROM t AS x WHERE x.id = d.id) JOIN t AS d ON d.id = a.id"))

[<Fact>]
let ``SqlReference: サブクエリの未対応形とエラー`` () =
    withSession "subquery-errors" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"

        // スカラーサブクエリが 2 行以上
        expectErrorKind
            ErrorKind.ConstraintViolation
            "2 行以上のスカラーサブクエリ"
            (execSql db2 "SELECT id, (SELECT v FROM t) FROM t")

        // 1 列でないサブクエリ
        expectErrorKind
            ErrorKind.TypeError
            "2 列のスカラーサブクエリ"
            (execSql db2 "SELECT id, (SELECT id, v FROM t WHERE id = 1) FROM t")

        expectErrorKind
            ErrorKind.TypeError
            "2 列の IN サブクエリ"
            (execSql db2 "SELECT id FROM t WHERE id IN (SELECT id, v FROM t)")

        // 派生テーブルは §7.10 で対応。別名が無いと SyntaxError。
        expectErrorKind ErrorKind.SyntaxError "派生テーブルの別名" (execSql db2 "SELECT id FROM (SELECT id FROM t)")

        // 派生テーブルは相関できない（内側から外側の別名は見えない）
        expectErrorKind
            ErrorKind.NameError
            "相関派生テーブル"
            (execSql db2 "SELECT id FROM t AS o JOIN (SELECT o.id AS x FROM t) AS d ON d.x = o.id"))

[<Fact>]
let ``SqlReference: 集約結果は DATE の型を保つ`` () =
    // 集約後の値を Literal に落とすと DATE / TIMESTAMP / UUID が TEXT になり、
    // 外側の DATE 列と比較できなくなる（サブクエリで顕在化した）。
    withSession "agg-date" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE d (id INTEGER PRIMARY KEY, dt DATE)"
        let db2 = exec db1 "INSERT INTO d VALUES (1,'2026-11-03'),(2,'2026-11-04')"

        let _, rows = ok (execSql db2 "SELECT id FROM d WHERE dt = (SELECT MAX(dt) FROM d)")

        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(2L, asInt rows.Rows.[0].[0])

        // GROUP BY のキーも型を保つ
        let _, grouped =
            ok (execSql db2 "SELECT dt, COUNT(*) FROM d GROUP BY dt ORDER BY dt")

        match grouped.Rows.[0].[0] with
        | SqlValue.Date _ -> ()
        | v -> failActual "DATE ではありません" v)

[<Fact>]
let ``SqlReference: 深いサブクエリの入れ子も実行できる`` () =
    // `Plan.planSelect` / `resolveSelect` / `resolveExpr` / `resolveLeaf` は相互再帰するが、
    // 継続を末尾で呼ぶので入れ子段数に依存せずスタックは一定。
    // Parser 側も CPS なので、式・サブクエリの深さ上限はない。
    withSession "subquery-depth" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10)"

        let nested depth =
            [ 1..depth ]
            |> List.fold (fun e _ -> sprintf "(SELECT %s FROM t WHERE id = 1)" e) "v"

        let _, rows =
            ok (execSql db2 (sprintf "SELECT %s AS x FROM t WHERE id = 1" (nested 4000)))

        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(10L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 引用した "cast" は CAST 構文として扱わない`` () =
    // 引用識別子（`"cast"(...)`）は通常の関数呼び出しになる（関数 `CAST` は存在しない）。
    withSession "quoted-cast" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a TEXT)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1,'2')")
        expectMessage "未知のスカラー関数 'CAST' です" "SELECT \"cast\"(a) FROM t" db2

        // 通常の CAST は動く。
        let _, cast = ok (execSql db2 "SELECT CAST(a AS INTEGER) FROM t")
        Assert.Equal<int64>(2L, asInt cast.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: ORDER BY の * は句に依存しないメッセージで拒否する`` () =
    withSession "order-by-star" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2, _ = ok (execSql db1 "INSERT INTO t VALUES (1)")
        expectMessage "'*' はここでは使えません" "SELECT id FROM t ORDER BY *" db2)

[<Fact>]
let ``SqlReference: 派生テーブル（FROM / JOIN）`` () =
    withSession "derived" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20),(3,30)"
        let db3 = exec db2 "CREATE TABLE u (id INTEGER PRIMARY KEY, w INTEGER)"
        let db4 = exec db3 "INSERT INTO u VALUES (2,200),(3,300),(4,400)"

        // FROM の派生テーブル。列は別名で修飾できる。
        let _, rows =
            ok (execSql db4 "SELECT x.id, x.v FROM (SELECT id, v FROM t WHERE v >= 20) AS x ORDER BY x.id")

        Assert.Equal<int>(2, rows.Rows.Length)
        Assert.Equal<int64>(2L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(20L, asInt rows.Rows.[0].[1])

        // 列別名と * 展開（ヘッダは派生列の名前）
        let _, all =
            ok (execSql db4 "SELECT * FROM (SELECT id AS k, v * 2 AS d FROM t) x ORDER BY k")

        Assert.Equal<string array>([| "k"; "d" |], all.Columns)
        Assert.Equal<int>(3, all.Rows.Length)
        Assert.Equal<int64>(60L, asInt all.Rows.[2].[1])

        // 外側 WHERE は派生列を参照できる
        let _, filtered =
            ok (execSql db4 "SELECT x.v FROM (SELECT * FROM t) x WHERE x.v > 10 ORDER BY x.v")

        Assert.Equal<int>(2, filtered.Rows.Length)

        // 内側の ORDER BY / LIMIT が先に効く
        let _, limited =
            ok (execSql db4 "SELECT x.v FROM (SELECT v FROM t ORDER BY v DESC LIMIT 2) x ORDER BY x.v")

        Assert.Equal<int>(2, limited.Rows.Length)
        Assert.Equal<int64>(20L, asInt limited.Rows.[0].[0])
        Assert.Equal<int64>(30L, asInt limited.Rows.[1].[0])

        // JOIN の右側（INNER / LEFT）
        let _, joined =
            ok (
                execSql
                    db4
                    "SELECT t.id, d.w FROM t JOIN (SELECT id, w FROM u WHERE w >= 300) AS d ON d.id = t.id ORDER BY t.id"
            )

        Assert.Equal<int>(1, joined.Rows.Length)
        Assert.Equal<int64>(3L, asInt joined.Rows.[0].[0])
        Assert.Equal<int64>(300L, asInt joined.Rows.[0].[1])

        let _, leftJoined =
            ok (
                execSql db4 "SELECT t.id, d.w FROM t LEFT JOIN (SELECT id, w FROM u) AS d ON d.id = t.id ORDER BY t.id"
            )

        Assert.Equal<int>(3, leftJoined.Rows.Length)
        Assert.True(SqlValue.isNull leftJoined.Rows.[0].[1])
        Assert.Equal<int64>(300L, asInt leftJoined.Rows.[2].[1])

        // 派生が左（FROM）で実テーブルと結合する
        let _, joinedLeft =
            ok (
                execSql
                    db4
                    "SELECT d.id, u.w FROM (SELECT id FROM t WHERE id >= 2) AS d JOIN u ON u.id = d.id ORDER BY d.id"
            )

        Assert.Equal<int>(2, joinedLeft.Rows.Length)

        // 派生が左の LEFT JOIN（右が NULL になる行を含む）
        let _, outerLeft =
            ok (execSql db4 "SELECT d.id, u.w FROM (SELECT id FROM t) AS d LEFT JOIN u ON u.id = d.id ORDER BY d.id")

        Assert.Equal<int>(3, outerLeft.Rows.Length)
        Assert.True(SqlValue.isNull outerLeft.Rows.[0].[1])
        Assert.Equal<int64>(300L, asInt outerLeft.Rows.[2].[1])

        // 派生同士の結合
        let _, bothDerived =
            ok (
                execSql
                    db4
                    "SELECT a.id FROM (SELECT id FROM t) AS a JOIN (SELECT id FROM u) AS b ON b.id = a.id ORDER BY a.id"
            )

        Assert.Equal<int>(2, bothDerived.Rows.Length)

        // 集約を含む派生 / 外側の集約 / 集約結果への WHERE
        let _, counted =
            ok (execSql db4 "SELECT COUNT(*) FROM (SELECT id, SUM(w) AS s FROM u GROUP BY id) x")

        Assert.Equal<int64>(3L, asInt counted.Rows.[0].[0])

        let _, aggFiltered =
            ok (
                execSql
                    db4
                    "SELECT x.s FROM (SELECT id, SUM(w) AS s FROM u GROUP BY id) x WHERE x.s >= 300 ORDER BY x.s"
            )

        Assert.Equal<int>(2, aggFiltered.Rows.Length)

        // 入れ子の派生
        let _, nested =
            ok (execSql db4 "SELECT y.v FROM (SELECT x.v FROM (SELECT v FROM t) x WHERE x.v >= 20) y ORDER BY y.v")

        Assert.Equal<int>(2, nested.Rows.Length)

        // 派生内の式サブクエリ
        let _, withSub =
            ok (
                execSql
                    db4
                    "SELECT x.id FROM (SELECT id FROM t WHERE v > (SELECT MIN(w) - 190 FROM u)) x ORDER BY x.id"
            )

        Assert.Equal<int>(2, withSub.Rows.Length)

        // 修飾付きスター
        let _, qualified =
            ok (execSql db4 "SELECT x.* FROM (SELECT id, v FROM t) x ORDER BY x.id")

        Assert.Equal<int>(3, qualified.Rows.Length)

        // 内側のテーブル解決は計画時（未存在なら NameError）
        expectErrorKind ErrorKind.NameError "派生内の未存在テーブル" (execSql db4 "SELECT * FROM (SELECT id FROM missing) x")

        // 別名の重複
        expectErrorKind
            ErrorKind.NameError
            "派生テーブルの別名重複"
            (execSql db4 "SELECT * FROM t JOIN (SELECT id FROM u) AS t ON t.id = t.id")

        // 自己参照（相関は未対応なので内側から外側の別名は見えない）
        expectErrorKind ErrorKind.NameError "派生テーブルの自己参照" (execSql db4 "SELECT * FROM (SELECT * FROM x) x"))

[<Fact>]
let ``SqlReference: 派生テーブルの別名は実表名と衝突しない`` () =
    // 派生の引き当てを実表名で行っていたため、実表に別名を付けても派生の別名と衝突し、
    // 同じ走査ノードが 2 回現れて InternalError になっていた。
    withSession "derived-alias-real" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE foo (id INTEGER PRIMARY KEY, w INTEGER)"
        let db2, _ = ok (execSql db1 "INSERT INTO foo VALUES (1, 100), (2, 200)")

        // 派生の別名 foo と、実表 foo（別名 f）を同時に使える。
        let _, rows =
            ok (
                execSql
                    db2
                    "SELECT foo.id, f.w FROM (SELECT id FROM foo WHERE id = 1) AS foo JOIN foo AS f ON f.id = foo.id"
            )

        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(100L, asInt rows.Rows.[0].[1])

        // 存在しない列は InternalError ではなく NameError になる。
        expectErrorKind
            ErrorKind.NameError
            "派生別名と実表名の衝突"
            (execSql
                db2
                "SELECT d.id, f.w FROM (SELECT id FROM foo WHERE id = 1) AS foo JOIN foo AS f ON f.id = foo.id"))

[<Fact>]
let ``SqlReference: EXPLAIN は実行せずに計画を返す`` () =
    withSession "explain" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')"
        let db3 = exec db2 "CREATE INDEX ix ON t (v)"

        let planOf sql =
            let _, r = ok (execSql db3 sql)
            Assert.Equal<string list>([ "plan" ], r.Columns |> Array.toList)
            r.Rows |> Array.map (fun row -> asText row.[0]) |> Array.toList

        // 主キー等値 → PkLookup（述語は走査で吸収し Filter は残らない）
        Assert.Equal<string list>(
            [ "Project(*)"; "  Scan(t, PkLookup(2))" ],
            planOf "EXPLAIN SELECT * FROM t WHERE id = 2"
        )

        // 索引の等値 → IndexLookup（述語は走査で吸収し Filter は残らない）
        Assert.Equal<string list>(
            [ "Project(t.id)"; "  Scan(t, IndexLookup(ix, 'a'))" ],
            planOf "EXPLAIN SELECT id FROM t WHERE v = 'a'"
        )

        // 前方一致 → IndexRangeScan
        Assert.Equal<string list>(
            [
                "Project(*)"
                "  Filter((t.v LIKE 'a%'))"
                "    Scan(t, IndexRangeScan(ix, 'a'..'b'))"
            ],
            planOf "EXPLAIN SELECT * FROM t WHERE v LIKE 'a%'"
        )

        // 集約・DISTINCT・ORDER BY / LIMIT の適用順
        Assert.Equal<string list>(
            [
                "Project(t.v, COUNT(*))"
                "  Aggregate(group by [t.v], aggregates [COUNT(*)])"
                "    Scan(t, SeqScan)"
            ],
            planOf "EXPLAIN SELECT v, COUNT(*) FROM t GROUP BY v"
        )

        Assert.Equal<string list>(
            [ "Distinct"; "  Project(t.v)"; "    Scan(t, SeqScan)" ],
            planOf "EXPLAIN SELECT DISTINCT v FROM t"
        )

        Assert.Equal<string list>(
            [
                "Limit(offset=0, count=2)"
                "  Project(t.id)"
                "    Sort(t.id DESC)"
                "      Scan(t, SeqScan)"
            ],
            planOf "EXPLAIN SELECT id FROM t ORDER BY id DESC LIMIT 2"
        )

        // 主キーの範囲 → RangeScan
        Assert.Equal<string list>(
            [ "Project(*)"; "  Filter((t.id > 1))"; "    Scan(t, RangeScan(1..*))" ],
            planOf "EXPLAIN SELECT * FROM t WHERE id > 1"
        )

        // FROM なしと派生テーブル
        Assert.Equal<string list>([ "Project(1)"; "  Values" ], planOf "EXPLAIN SELECT 1")

        Assert.Equal<string list>(
            [
                "Project(x.id)"
                "  Derived"
                "    Project(t.id)"
                "      Scan(t, PkLookup(1))"
            ],
            planOf "EXPLAIN SELECT x.id FROM (SELECT id FROM t WHERE id = 1) AS x"
        )

        // 派生テーブル内の結合も、内側の文脈でアルゴリズムが決まる
        Assert.Equal<string list>(
            [
                "Project(x.id)"
                "  Derived"
                "    Project(a.id)"
                "      Join(INNER, MergeJoin, right=b, on=(a.id = b.id))"
                "        Scan(a, SeqScan)"
                "        Scan(b, SeqScan)"
            ],
            planOf "EXPLAIN SELECT x.id FROM (SELECT a.id FROM t AS a JOIN t AS b ON a.id = b.id) AS x"
        )

        // EXPLAIN は SELECT だけ
        expectErrorKind ErrorKind.SyntaxError "EXPLAIN は SELECT のみ" (execSql db3 "EXPLAIN DELETE FROM t"))

[<Fact>]
let ``SqlReference: 未対応の構文は明示的な SyntaxError`` () =
    // §14「構文が未対応のもの」。集合演算と RIGHT / FULL JOIN は ParserTests が担当。
    withSession "unsupported-syntax" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"

        // サブクエリ（式の中）は §7.9、FROM の派生テーブルは §7.10 で対応済み。
        expectErrorKind ErrorKind.SyntaxError "WITH 句" (execSql db2 "WITH c AS (SELECT id FROM t) SELECT id FROM c")

        expectErrorKind
            ErrorKind.SyntaxError
            "ウィンドウ関数"
            (execSql db2 "SELECT id, ROW_NUMBER() OVER (ORDER BY id) FROM t")

        // DDL / 実行計画
        // ALTER TABLE の ADD / DROP / RENAME COLUMN は対応済み。それ以外の操作は明示エラー。
        expectErrorKind ErrorKind.SyntaxError "RENAME TO" (execSql db2 "ALTER TABLE t RENAME TO u")
        expectErrorKind ErrorKind.SyntaxError "列の型変更" (execSql db2 "ALTER TABLE t ALTER COLUMN v TYPE INTEGER")
        expectErrorKind ErrorKind.SyntaxError "TRUNCATE" (execSql db2 "TRUNCATE TABLE t")

        // DML の未対応形
        expectErrorKind ErrorKind.SyntaxError "RETURNING" (execSql db2 "DELETE FROM t RETURNING id")

        // どれも文が実行されていないこと
        let _, rows = ok (execSql db2 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: よく見るエラーメッセージは sql-reference §13 と一致する`` () =
    // §13 の一覧は「実測」した文言そのもの。種別（ErrorKind）だけでは文言の劣化を検出できない。
    withSession "error-messages" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, n TEXT NOT NULL, num INTEGER, active BOOLEAN)"

        let db2 = exec db1 "INSERT INTO t (id, n, num, active) VALUES (1, 'a', 10, TRUE)"

        expectMessage "JOIN には FROM が必要です" "SELECT 1 JOIN t ON 1 = 1" db2
        expectMessage "LATERAL は未対応です" "SELECT * FROM t JOIN LATERAL (SELECT 1) AS x ON 1 = 1" db2
        expectMessage "カンマ結合は未対応です" "SELECT * FROM t, u" db2
        expectMessage "FROM のない SELECT では * を使えません" "SELECT *" db2
        expectMessage "式が必要ですが見つかりませんでした" "SELECT id FROM t WHERE id > 1 AND;" db2
        expectMessage "unclosed string" "SELECT 'abc;" db2
        expectMessage "テーブル 'nosuchtable' が存在しません" "SELECT * FROM nosuchtable" db2
        expectMessage "'nosuchcol' は列でも関数でもありません" "SELECT nosuchcol FROM t" db2
        expectMessage "テーブル nopk に主キーがありません" "CREATE TABLE nopk (a INTEGER)" db2
        expectMessage "テーブル t は既に存在します" "CREATE TABLE t (id INTEGER PRIMARY KEY)" db2
        expectMessage "テーブル t に列 nosuchcol がありません" "CREATE INDEX ix ON t (nosuchcol)" db2
        expectMessage "テーブル t が重複しています（エイリアスを付けてください）" "SELECT id FROM t JOIN t ON t.id = t.id" db2
        expectMessage "テーブル nosuch が存在しません" "DROP TABLE nosuch" db2
        expectMessage "INSERT の列数(1)と値の数(2)が合いません" "INSERT INTO t (id) VALUES (1, 2)" db2
        expectMessage "NOT NULL 制約に違反: t.n" "INSERT INTO t (id) VALUES (2)" db2
        expectMessage "''abc'' は 'INTEGER' に変換できません" "INSERT INTO t (id, n, num) VALUES (5, 'z', 'abc')" db2
        expectMessage "0 での除算は定義されていません" "SELECT (id) / 0 FROM t" db2
        expectMessage "LENGTH 関数は 1個の引数が必要ですが、2個渡されました" "SELECT LENGTH(id, num) FROM t" db2
        expectMessage "未知のスカラー関数 'NOSUCHFN' です" "SELECT NOSUCHFN(id) FROM t" db2
        expectMessage "サブクエリ（派生テーブル）には別名が必要です（例: AS x）" "SELECT id FROM (SELECT id FROM t)" db2
        expectMessage "型 'BOOLEAN' と 'INTEGER' は比較できません" "SELECT active = 1 FROM t" db2
        expectMessage "ROUND は数値を受け付けますが" "SELECT ROUND(n) FROM t" db2

        // 失敗した INSERT は何も残っていない
        let _, rows = ok (execSql db2 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: WHERE に書いた結合述語は ON と同じ結果になる`` () =
    // 複数テーブルを参照する WHERE の述語は INNER の ON へ昇格する
    // （INNER では ON と WHERE は同値。昇格しないと結合キーに使えず入れ子ループになる）。
    withSession "join-promote" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, g INTEGER)"
        let db3, _ = execRes db2 "INSERT INTO a VALUES (1,10),(2,20),(3,10)"
        let db4, _ = execRes db3 "INSERT INTO b VALUES (1,10),(2,30),(3,10)"

        let _, onForm =
            ok (execSql db4 "SELECT a.id, b.id FROM a JOIN b ON a.id > b.id AND a.g = b.g ORDER BY a.id, b.id")

        let _, whereForm =
            ok (execSql db4 "SELECT a.id, b.id FROM a JOIN b ON a.id > b.id WHERE a.g = b.g ORDER BY a.id, b.id")

        Assert.Equal<int>(1, onForm.Rows.Length)
        Assert.Equal<int>(onForm.Rows.Length, whereForm.Rows.Length)

        for i in 0 .. onForm.Rows.Length - 1 do
            Assert.Equal<int64>(asInt onForm.Rows.[i].[0], asInt whereForm.Rows.[i].[0])
            Assert.Equal<int64>(asInt onForm.Rows.[i].[1], asInt whereForm.Rows.[i].[1]))

[<Fact>]
let ``SqlReference: 結合順が並べ替えられても結果は同じ`` () =
    // c の主キー等値で c が先に結合される（FROM に書いた順とは違う計画になる）。
    withSession "join-order" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, av INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, bv INTEGER)"
        let db4, _ = execRes db3 "INSERT INTO a VALUES (1,10),(2,20)"
        let db5, _ = execRes db4 "INSERT INTO b VALUES (1,10),(2,20)"
        let db6, _ = execRes db5 "INSERT INTO c VALUES (1,10),(2,99)"

        let _, reordered =
            ok (execSql db6 "SELECT a.v, b.av, c.bv FROM a JOIN b ON a.id = b.id JOIN c ON a.id = c.id WHERE c.id = 1")

        let _, fromOrder =
            ok (execSql db6 "SELECT a.v, b.av, c.bv FROM c JOIN a ON a.id = c.id JOIN b ON a.id = b.id WHERE c.id = 1")

        Assert.Equal<int>(1, reordered.Rows.Length)
        Assert.Equal<int>(fromOrder.Rows.Length, reordered.Rows.Length)
        Assert.Equal<int64>(10L, asInt reordered.Rows.[0].[0])
        Assert.Equal<int64>(10L, asInt reordered.Rows.[0].[1])
        Assert.Equal<int64>(10L, asInt reordered.Rows.[0].[2]))

[<Fact>]
let ``SqlReference: 貪欲な結合順でも DP と同じ結果になる`` () =
    // A/B 用の戦略切り替え（DP / 貪欲）が結果を変えないことを固定する。
    withSession "join-order-greedy" (fun db ->
        let db = exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE c (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "INSERT INTO a VALUES (1,10),(2,20),(3,30)"
        let db = exec db "INSERT INTO b VALUES (1,10),(2,25),(3,30)"
        let db = exec db "INSERT INTO c VALUES (1,10),(2,20),(3,30)"

        let stmt =
            match parseOneOk "SELECT a.id, b.id, c.id FROM a JOIN b ON a.k = b.k JOIN c ON a.k = c.k" with
            | Statement.Select s -> s
            | other -> failwithf "SELECT ではありません: %A" other

        let run plan =
            try
                let op = ok (Optimizer.exec plan.Context plan.Tree plan.Input)

                (op |> Op.toArray |> ok)
                |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
            finally
                plan.Context.Scope.DisposeAll()

        let dp = run (ok (db.PrepareSelect stmt))
        let greedy = run (ok (db.PrepareSelectWith(JoinOrderStrategy.Greedy, stmt)))
        Assert.Equal<int>(2, dp.Length)
        Assert.Equal<string list>(dp |> Array.toList, greedy |> Array.toList))

[<Fact>]
let ``SqlReference: bushy 計画でも DP と同じ結果になる`` () =
    // 入れ替えの自由度が高い 4 テーブルで、bushy（右が部分木）の結果を固定する。
    withSession "join-order-bushy" (fun db ->
        let db = exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE c (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE d (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "INSERT INTO a VALUES (1,10),(2,20),(3,30)"
        let db = exec db "INSERT INTO b VALUES (1,10),(2,20),(3,35)"
        let db = exec db "INSERT INTO c VALUES (1,10),(2,20),(3,30)"
        let db = exec db "INSERT INTO d VALUES (1,10),(2,20),(3,30)"

        let stmt =
            match
                parseOneOk
                    "SELECT a.id, b.id, c.id, d.id FROM a JOIN b ON a.k = b.k JOIN c ON b.k = c.k JOIN d ON c.k = d.k"
            with
            | Statement.Select s -> s
            | other -> failwithf "SELECT ではありません: %A" other

        let run plan =
            try
                let op = ok (Optimizer.exec plan.Context plan.Tree plan.Input)

                (op |> Op.toArray |> ok)
                |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
                |> Array.sort
            finally
                plan.Context.Scope.DisposeAll()

        let dp = run (ok (db.PrepareSelect stmt))
        let bushy = run (ok (db.PrepareSelectWith(JoinOrderStrategy.Bushy, stmt)))
        Assert.Equal<int>(2, dp.Length)
        Assert.Equal<string array>(dp, bushy))

[<Fact>]
let ``SqlReference: 結合順は中間行数の見積りで決まる`` () =
    // c は a との結合述語を 2 本持つ（ON と昇格した WHERE）。
    // b は 1 本だけなので、c を先に結合した方が中間行数が小さい。
    withSession "join-order-edges" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, k1 INTEGER, k2 INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, k1 INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, k1 INTEGER, k2 INTEGER)"
        let db4 = exec db3 "INSERT INTO a VALUES (1,10,100),(2,20,200)"
        let db5 = exec db4 "INSERT INTO b VALUES (1,10),(2,20)"
        let db6 = exec db5 "INSERT INTO c VALUES (1,10,100),(2,20,200)"

        let sql =
            "SELECT a.id FROM a JOIN b ON a.k1 = b.k1 JOIN c ON a.k1 = c.k1 WHERE a.k2 = c.k2"

        let _, rows = ok (execSql db6 sql)
        Assert.Equal<int>(2, rows.Rows.Length)

        let _, plan = ok (execSql db6 ("EXPLAIN " + sql))

        let lines = plan.Rows |> Array.map (fun row -> asText row.[0])

        let indexOf (text: string) =
            lines |> Array.findIndex (fun (line: string) -> line.Contains text)

        // 左深木では最後に結合したテーブルが外側（先に出力）になる。
        // c を先に結合するので、外側が b、内側が c になる。
        Assert.True(indexOf "right=c" > indexOf "right=b", String.concat "\n" lines))

[<Fact>]
let ``SqlReference: 同じ列どうしの等値は行を減らさない見積りになる`` () =
    // `t.a = t.a` を独立な 2 列として 1/NDV で見積もると、行数が 1/10 になる。
    // NULL でない行すべてで真なので、NULL 率だけを掛ける。
    withSession "stats-same-column" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"

        let values =
            [ 1..10 ] |> List.map (fun i -> sprintf "(%d,%d)" i i) |> String.concat ","

        let db2 = exec db1 (sprintf "INSERT INTO t VALUES %s" values)
        let db3 = exec db2 "ANALYZE t"
        let _, plan = ok (execSql db3 "EXPLAIN SELECT id FROM t WHERE a = a")
        let text = plan.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        let filterRows =
            text.Split '\n'
            |> Array.find (fun line -> line.Contains "Filter")
            |> plannedRows

        Assert.Equal(10.0, filterRows, 1))

[<Fact>]
let ``SqlReference: bushy 計画でも区間外を参照する WHERE が使える`` () =
    // 最初の INNER 区間（a,b,c,d）の外にある e（LEFT の右）と f（LEFT の後の INNER）を
    // 参照する WHERE は区間木の上に置かず、未使用のまま外側の Filter で評価する。
    withSession "join-order-bushy-leftover" (fun db ->
        let db = exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE c (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE d (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE e (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "CREATE TABLE f (id INTEGER PRIMARY KEY, k INTEGER)"
        let db = exec db "INSERT INTO a VALUES (1,10),(2,20)"
        let db = exec db "INSERT INTO b VALUES (1,10),(2,20)"
        let db = exec db "INSERT INTO c VALUES (1,10),(2,20)"
        let db = exec db "INSERT INTO d VALUES (1,10),(2,20)"
        let db = exec db "INSERT INTO e VALUES (1,10)"
        let db = exec db "INSERT INTO f VALUES (1,10)"

        let stmt =
            match
                parseOneOk
                    "SELECT a.k FROM a JOIN b ON b.k = a.k JOIN c ON c.k = b.k JOIN d ON d.k = c.k LEFT JOIN e ON e.k = d.k JOIN f ON f.k = e.k WHERE a.k = f.k"
            with
            | Statement.Select s -> s
            | other -> failwithf "SELECT ではありません: %A" other

        let plan = ok (db.PrepareSelectWith(JoinOrderStrategy.Bushy, stmt))

        // テストの前提: bushy（右が部分木の結合）が実際に選ばれている。
        let rec hasBushyJoin =
            function
            | Logical.Join(_, _, _, _, Logical.Join _) -> true
            | Logical.Join(_, _, _, left, right) -> hasBushyJoin left || hasBushyJoin right
            | Logical.Filter(_, child)
            | Logical.Sort(_, child)
            | Logical.Limit(_, _, child)
            | Logical.Distinct child
            | Logical.Project(_, child)
            | Logical.Aggregate(_, _, child)
            | Logical.Having(_, _, _, child) -> hasBushyJoin child
            | _ -> false

        Assert.True(hasBushyJoin plan.Tree, sprintf "%A" plan.Tree)

        try
            let op = ok (Optimizer.exec plan.Context plan.Tree plan.Input)

            let rows =
                (op |> Op.toArray |> ok)
                |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
                |> Array.sort

            Assert.Equal<string array>([| "10" |], rows)
        finally
            plan.Context.Scope.DisposeAll())

[<Fact>]
let ``SqlReference: ヒストグラムは偏った分布でも範囲の行数を見積もる`` () =
    // 99行が 0、1行が 1000。min/max の線形補間なら v < 500 は 50行になるが、
    // 等頻度ヒストグラムなら 97行前後になる。
    withSession "stats-histogram" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"

        let values =
            [ 1..99 ] |> List.map (fun i -> sprintf "(%d,0)" i) |> String.concat ","

        let db2 = exec db1 (sprintf "INSERT INTO t VALUES %s" values)
        let db3 = exec db2 "INSERT INTO t VALUES (100,1000)"
        let db4 = exec db3 "ANALYZE t"

        let _, plan = ok (execSql db4 "EXPLAIN SELECT id FROM t WHERE v < 500")
        let text = plan.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"
        Assert.True(plannedRows text > 90.0, text))

[<Fact>]
let ``SqlReference: 型が違う等値は結合キーにせず入れ子ループになる`` () =
    // REAL と DECIMAL は比較できるが型が違うので、ハッシュ / マージ / INL のキーにしない。
    // 結合順の見積りも同じ条件で入れ子ループとして見積もる。
    withSession "join-key-type" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, r REAL)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, d DECIMAL)"
        let db3 = exec db2 "INSERT INTO a VALUES (1,1.5),(2,2.5)"
        let db4 = exec db3 "INSERT INTO b VALUES (1,1.5),(2,3.5)"

        let sql = "SELECT a.id, b.id FROM a JOIN b ON a.r = b.d"
        let _, plan = ok (execSql db4 ("EXPLAIN " + sql))
        let lines = plan.Rows |> Array.map (fun row -> asText row.[0])
        Assert.Contains("NestedLoop", String.concat "\n" lines)

        let _, res = ok (execSql db4 (sql + " ORDER BY a.id"))
        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: EXPLAIN ANALYZE は走査と結合の実測行数を返す`` () =
    withSession "explain-analyze" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db3 = exec db2 "INSERT INTO a VALUES (1,10),(2,20),(3,30)"
        let db4 = exec db3 "INSERT INTO b VALUES (1,10),(2,25),(3,30)"

        let sql = "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k"
        let _, plan = ok (execSql db4 ("EXPLAIN ANALYZE " + sql))
        let lines = plan.Rows |> Array.map (fun row -> asText row.[0])
        let text = String.concat "\n" lines

        // 走査と結合に実測行数が付く（a は 3行、結合は 2行）。
        Assert.Contains("Scan(a, SeqScan) actual=3", text)
        Assert.Contains("Join(INNER, HashJoin, right=b", text)
        Assert.Contains("actual=2", text)

        // 末尾に件数と経過時間の 1行が付く。
        Assert.StartsWith("Execution: rows=2 elapsed=", lines.[lines.Length - 1])
        // 実行は結果を返さない（計画の行だけ）。
        Assert.Equal<int>(lines.Length, plan.Rows.Length)

        // 通常の EXPLAIN は実行しないので actual= が付かない。
        let _, plain = ok (execSql db4 ("EXPLAIN " + sql))

        let plainText =
            plain.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        Assert.DoesNotContain("actual=", plainText)
        Assert.DoesNotContain("Execution:", plainText))

[<Fact>]
let ``SqlReference: EXPLAIN ANALYZE は4つの結合アルゴリズムで実測行数を返す`` () =
    withSession "explain-analyze-algorithms" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db3 = exec db2 "INSERT INTO a VALUES (1,10),(2,20),(3,30)"
        let db4 = exec db3 "INSERT INTO b VALUES (1,10),(2,25),(3,30)"

        let linesOf db sql =
            let _, plan = ok (execSql db ("EXPLAIN ANALYZE " + sql))

            plan.Rows |> Array.map (fun row -> asText row.[0])

        let joinLineOf (lines: string array) =
            lines |> Array.find (fun line -> line.Contains "Join(INNER")

        let check algorithm actual sql =
            let lines = linesOf db4 sql
            let joinLine = joinLineOf lines

            Assert.Contains(sprintf "Join(INNER, %s," algorithm, joinLine)
            Assert.EndsWith(sprintf "actual=%d" actual, joinLine)
            Assert.StartsWith(sprintf "Execution: rows=%d " actual, lines.[lines.Length - 1])
            lines

        // 主キー同士の等値は両側が主キー順に走査できるのでマージ結合。
        let mergeLines =
            check "MergeJoin" 3 "SELECT a.id, b.id FROM a JOIN b ON a.id = b.id"

        let mergeText = String.concat "\n" mergeLines
        Assert.Contains("Scan(a, SeqScan) actual=3", mergeText)
        Assert.Contains("Scan(b, SeqScan) actual=3", mergeText)

        // 非主キーの等値はハッシュ結合（一致は 2行）。
        let hashLines = check "HashJoin" 2 "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k"
        Assert.Contains("Scan(a, SeqScan) actual=3", String.concat "\n" hashLines)

        // 非等値の ON は入れ子ループ（a.k < b.k の一致は 4行）。
        check "NestedLoop" 4 "SELECT a.id, b.id FROM a JOIN b ON a.k < b.k" |> ignore

        // 左が 1行なら右を索引で引く。右部分木は実行しないので actual= が付かない。
        let inlLines =
            check "IndexNestedLoop(pk)" 1 "SELECT a.id, b.id FROM a JOIN b ON a.id = b.id WHERE a.id = 1"

        let inlText = String.concat "\n" inlLines
        Assert.Contains("Scan(a, PkLookup(1)) actual=1", inlText)
        Assert.DoesNotContain("Scan(b, SeqScan) actual=", inlText))

[<Fact>]
let ``SqlReference: EXPLAIN の推定行数は EXPLAIN ANALYZE の実測と近い`` () =
    // コスト較正の根拠: 統計ありの推定が実測から大きく外れないことを固定する。
    withSession "explain-estimate-vs-actual" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db3 = exec db2 "INSERT INTO a VALUES (1,10),(2,20),(3,30)"
        let db4 = exec db3 "INSERT INTO b VALUES (1,10),(2,25),(3,30)"
        let db5 = exec db4 "ANALYZE"

        let joinLineOf db prefix sql =
            let _, plan = ok (execSql db (prefix + sql))

            plan.Rows
            |> Array.map (fun row -> asText row.[0])
            |> Array.find (fun line -> line.Contains "Join(INNER")

        let compare estimated actual sql =
            let e = joinLineOf db5 "EXPLAIN " sql
            let a = joinLineOf db5 "EXPLAIN ANALYZE " sql

            Assert.Contains(estimated, e)
            Assert.Contains(actual, a)
            e, a

        // 等値（NDV=3, 3行ずつ）: 推定 3行、実測 2行。
        let hashEstimate, hashActual =
            compare "rows=3" "actual=2" "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k"

        Assert.Contains("HashJoin", hashEstimate)
        Assert.Contains("HashJoin", hashActual)

        // 非等値: 推定 3行、実測 4行（既定の選択性でも 2倍以内）。
        let nlEstimate, nlActual =
            compare "rows=3" "actual=4" "SELECT a.id, b.id FROM a JOIN b ON a.k < b.k"

        Assert.Contains("NestedLoop", nlEstimate)
        Assert.Contains("NestedLoop", nlActual)

        // 主キー 1行の INL: 推定も実測も 1行。
        let inlSql = "SELECT a.id, b.id FROM a JOIN b ON a.id = b.id WHERE a.id = 1"
        let inlEstimate, inlActual = compare "rows=1" "actual=1" inlSql
        Assert.Contains("IndexNestedLoop(pk)", inlEstimate)
        Assert.Contains("IndexNestedLoop(pk)", inlActual))

[<Fact>]
let ``SqlReference: EXPLAIN は ANALYZE 以降の更新行数を示す`` () =
    // 統計は DML で自動更新しないので、推定が古いことを EXPLAIN で示す。
    // 数えるのはコミットした行数だけ（ROLLBACK した分は数えない）。
    withSession "stats-stale" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"
        let db3 = exec db2 "ANALYZE t"

        let planOf db sql =
            let _, res = ok (execSql db ("EXPLAIN " + sql))
            res.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        Assert.DoesNotContain("Stats stale", planOf db3 "SELECT * FROM t")

        let db4 = exec db3 "INSERT INTO t VALUES (3,30)"
        Assert.Contains("Stats stale: t(+1)", planOf db4 "SELECT * FROM t")

        let db5 = exec db4 "INSERT INTO t VALUES (4,40)"
        Assert.Contains("Stats stale: t(+2)", planOf db5 "SELECT * FROM t")

        // ROLLBACK した DELETE は数えない。
        let db6 = exec db5 "BEGIN"
        let db7 = exec db6 "DELETE FROM t"
        let db8 = exec db7 "ROLLBACK"
        Assert.Contains("Stats stale: t(+2)", planOf db8 "SELECT * FROM t")

        // ANALYZE し直すと消える。
        let db9 = exec db8 "ANALYZE t"
        Assert.DoesNotContain("Stats stale", planOf db9 "SELECT * FROM t"))

[<Fact>]
let ``SqlReference: 明示 transaction の DML も COMMIT した分だけ統計の鮮度に積まれる`` () =
    withSession "stats-stale-txn" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"
        let db3 = exec db2 "ANALYZE t"

        let planOf db sql =
            let _, res = ok (execSql db ("EXPLAIN " + sql))
            res.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        // BEGIN してから INSERT しても、COMMIT すれば鮮度に積まれる。
        let db4 = exec db3 "BEGIN"
        let db5 = exec db4 "INSERT INTO t VALUES (3,30)"
        let db6 = exec db5 "COMMIT"
        Assert.Contains("Stats stale: t(+1)", planOf db6 "SELECT * FROM t")

        // ROLLBACK した分は数えない。
        let db7 = exec db6 "BEGIN"
        let db8 = exec db7 "DELETE FROM t"
        let db9 = exec db8 "ROLLBACK"
        Assert.Contains("Stats stale: t(+1)", planOf db9 "SELECT * FROM t")
        let _, remaining = ok (execSql db9 "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(3L, asInt remaining.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 並べ替えても LEFT JOIN の NULL 拡張は変わらない`` () =
    withSession "join-order-left" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE l (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE r (id INTEGER PRIMARY KEY, lv INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, rv INTEGER)"
        let db4, _ = execRes db3 "INSERT INTO l VALUES (1,10),(2,20)"
        let db5, _ = execRes db4 "INSERT INTO r VALUES (1,10)"
        let db6, _ = execRes db5 "INSERT INTO c VALUES (1,7),(2,8)"

        // LEFT の右（r）に一致しない l.id=2 は NULL 拡張されたまま残る
        let _, res =
            ok (execSql db6 "SELECT l.id, r.id FROM l LEFT JOIN r ON l.id = r.id JOIN c ON l.id = c.id ORDER BY l.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[0])
        Assert.Equal<SqlValue>(SqlValue.Null, res.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: LEFT JOIN を跨ぐ INNER は LEFT より前に繰り上げる`` () =
    // c の ON は l だけを見るので、LEFT の前へ動かしても同じ行が残る。
    withSession "join-hoist-left" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE l (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE r (id INTEGER PRIMARY KEY, lv INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, lv INTEGER)"
        let db4 = exec db3 "INSERT INTO l VALUES (1),(2),(3)"
        let db5 = exec db4 "INSERT INTO r VALUES (1,1),(2,2),(3,2)"
        let db6 = exec db5 "INSERT INTO c VALUES (1,1),(2,2)"

        let sql =
            "SELECT l.id, r.lv, c.lv FROM l LEFT JOIN r ON l.id = r.lv JOIN c ON l.id = c.lv"

        let _, plan = ok (execSql db6 ("EXPLAIN " + sql))
        let lines = plan.Rows |> Array.map (fun row -> asText row.[0])

        let indexOf (text: string) =
            lines |> Array.findIndex (fun (line: string) -> line.Contains text)

        // 繰り上げると根が LEFT になる（繰り上げないと根は INNER の c）。
        Assert.True(indexOf "LEFT" < indexOf "right=c", String.concat "\n" lines)

        // l.id=3 は c に無いので INNER で落ちる。r.lv=2 は 2 行あるので l.id=2 は 2 行残る。
        let _, res = ok (execSql db6 (sql + " ORDER BY l.id, r.id"))
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[1])
        Assert.Equal<int64>(2L, asInt res.Rows.[2].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[2].[1]))

[<Fact>]
let ``SqlReference: LEFT の右を参照する INNER は繰り上げない`` () =
    // c の ON が r を見るので、r が NULL 拡張される前に結合すると結果が変わる。
    withSession "join-hoist-left-blocked" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE l (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE r (id INTEGER PRIMARY KEY, lv INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, lv INTEGER)"
        let db4 = exec db3 "INSERT INTO l VALUES (1),(2)"
        let db5 = exec db4 "INSERT INTO r VALUES (1,10)"
        let db6 = exec db5 "INSERT INTO c VALUES (1,10)"

        let sql =
            "SELECT l.id, r.lv, c.id FROM l LEFT JOIN r ON l.id = r.id JOIN c ON r.lv = c.lv"

        let _, plan = ok (execSql db6 ("EXPLAIN " + sql))
        let lines = plan.Rows |> Array.map (fun row -> asText row.[0])

        let indexOf (text: string) =
            lines |> Array.findIndex (fun (line: string) -> line.Contains text)

        // 繰り上げないので根は INNER の c、LEFT はその内側。
        Assert.True(indexOf "right=c" < indexOf "LEFT", String.concat "\n" lines)

        // l.id=2 は r が NULL 拡張されるので c と一致せず落ちる。
        let _, res = ok (execSql db6 (sql + " ORDER BY l.id"))
        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: JOIN の後に続く LEFT JOIN も実行される`` () =
    withSession "join-inner-then-left" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE e (id INTEGER PRIMARY KEY, d INTEGER)"
        let db2 = exec db1 "CREATE TABLE d (id INTEGER PRIMARY KEY, c INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY)"
        let db4, _ = execRes db3 "INSERT INTO e VALUES (1,1),(2,2),(3,9)"
        let db5, _ = execRes db4 "INSERT INTO d VALUES (1,10),(2,20)"
        let db6, _ = execRes db5 "INSERT INTO c VALUES (10)"

        // e.id=3 は d に無いので INNER で落ち、d.id=2 は c に無いので NULL 拡張される
        let _, res =
            ok (
                execSql
                    db6
                    "SELECT e.id, d.id, c.id FROM e JOIN d ON e.d = d.id LEFT JOIN c ON d.c = c.id ORDER BY e.id"
            )

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[2])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt res.Rows.[1].[1])
        Assert.Equal<SqlValue>(SqlValue.Null, res.Rows.[1].[2]))

[<Fact>]
let ``SqlReference: 未知の列は行が 0 件でも実行前にエラー`` () =
    // 列解決は演算子の構築時に検査する。入力が 0 行で述語・射影が一度も評価されなくても
    // 未知の列・関数名は NameError（黙って 0 行を返さない）。
    withSession "unknown-column-empty" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE e (id INTEGER PRIMARY KEY, v INTEGER)"
        expectErrorKind ErrorKind.NameError "空テーブルの射影" (execSql db1 "SELECT nope FROM e")
        expectErrorKind ErrorKind.NameError "空テーブルの修飾列" (execSql db1 "SELECT e.nope FROM e")
        expectErrorKind ErrorKind.NameError "空テーブルの WHERE" (execSql db1 "SELECT * FROM e WHERE nope = 1")
        expectErrorKind ErrorKind.NameError "空テーブルの COUNT" (execSql db1 "SELECT COUNT(*) FROM e WHERE nope = 1")
        expectErrorKind ErrorKind.NameError "空テーブルの ORDER BY" (execSql db1 "SELECT id FROM e ORDER BY nope")
        expectErrorKind ErrorKind.NameError "空テーブルの UPDATE" (execSql db1 "UPDATE e SET v = 1 WHERE nope = 1")
        expectErrorKind ErrorKind.NameError "空テーブルの DELETE" (execSql db1 "DELETE FROM e WHERE nope = 1")

        // 行があっても同じエラー（検出が早くなるだけで、エラーの種類は変わらない）。
        let db2 = exec db1 "INSERT INTO e VALUES (1, 10)"
        expectErrorKind ErrorKind.NameError "1 行ありの WHERE" (execSql db2 "SELECT * FROM e WHERE nope = 1"))

[<Fact>]
let ``SqlReference: テーブル名の重複は大小文字を区別せずにエラー`` () =
    // 識別子はカタログと同じく大小文字を区別しない。
    // 重複したまま計画すると列が曖昧になるので、計画前に NameError にする。
    withSession "duplicate-table-case" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10)"
        expectErrorKind ErrorKind.NameError "実名と大文字" (execSql db2 "SELECT * FROM t JOIN T ON t.id = T.id")
        expectErrorKind ErrorKind.NameError "別名の大小文字" (execSql db2 "SELECT * FROM t AS x JOIN t AS X ON x.id = X.id")

        // 別名を付ければ同じテーブルの自己結合はできる。
        let _, res =
            ok (execSql db2 "SELECT a.id, b.v FROM t AS a JOIN t AS b ON a.id = b.id")

        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 比較できない型は行が 0 件でも計画時に TypeError`` () =
    // 列の宣言型とリテラルの型から比較可能性が分かるので、行の有無に関係なく
    // TypeError にする（1 行入れると実行時に落ちる、という差を作らない）。
    withSession "static-type-check" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE e (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "CREATE TABLE f (s TEXT PRIMARY KEY, n INTEGER)"

        expectMessage "型 'INTEGER' と 'TEXT' は比較できません" "SELECT COUNT(*) FROM e WHERE id = '1'" db2
        expectErrorKind ErrorKind.TypeError "空テーブルの等値結合" (execSql db2 "SELECT COUNT(*) FROM e JOIN f ON e.id = f.s")

        expectErrorKind
            ErrorKind.TypeError
            "空テーブルの BETWEEN"
            (execSql db2 "SELECT COUNT(*) FROM e WHERE id BETWEEN 'a' AND 'z'")

        expectErrorKind ErrorKind.TypeError "空テーブルの IN" (execSql db2 "SELECT COUNT(*) FROM e WHERE id IN (1, 'x')")
        expectErrorKind ErrorKind.TypeError "空テーブルの UPDATE" (execSql db2 "UPDATE e SET v = 'x' WHERE id = 'a'")
        expectErrorKind ErrorKind.TypeError "空テーブルの DELETE" (execSql db2 "DELETE FROM e WHERE id = 'a'")

        // 数値同士（INTEGER / DECIMAL / REAL）と NULL は比較できる。
        let _, res =
            ok (execSql db2 "SELECT COUNT(*) FROM e WHERE id = 1.5 AND id > -1e3 AND id = NULL")

        Assert.Equal<int64>(0L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: ORDER BY の式に含まれる未知の列も 0 件でエラー`` () =
    withSession "order-by-expr-ref" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE e (id INTEGER PRIMARY KEY)"

        expectErrorKind ErrorKind.NameError "式の中の未知の列" (execSql db1 "SELECT id FROM e ORDER BY nope + 1")

        // 正当な式は通る（0 行のまま）。
        let _, res = ok (execSql db1 "SELECT id FROM e ORDER BY id + 1 DESC")
        Assert.Equal<int>(0, res.Rows.Length))

[<Fact>]
let ``SqlReference: 相関 IN の左辺の未知の列も 0 件でエラー`` () =
    withSession "correlated-in-left" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE e (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE f (x INTEGER PRIMARY KEY, k INTEGER)"

        expectErrorKind
            ErrorKind.NameError
            "相関 IN の左辺"
            (execSql db2 "SELECT nope IN (SELECT f.x FROM f WHERE f.k = e.k) FROM e"))

[<Fact>]
let ``SqlReference: EXPLAIN は退避するハッシュ結合に spill を付ける`` () =
    // 作業メモリを 1KB にして、広い行のビルド側が収まらない状態を作る。
    // 表示するのは「あふれた分」で、結合順のコストと同じ式を使う。
    withDir "explain-spill" (fun dir ->
        let options =
            { SessionOptions.Default with
                WorkMemBytes = 1024L
            }

        let session = ok (Session.OpenWith(dir, options))

        try
            ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore

            let padding = String.replicate 600 "a"

            let values =
                [ 1..100 ]
                |> List.map (fun i -> sprintf "(%d,'%s')" i padding)
                |> String.concat ","

            ok (session.Execute(sprintf "INSERT INTO t VALUES %s" values)) |> ignore

            // id で結ぶと両側が主キー順になりマージ結合になるので、順序の無い v で結ぶ。
            let plan =
                ok (session.Execute "EXPLAIN SELECT * FROM t AS a JOIN t AS b ON a.v = b.v")

            let text = plan.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"
            Assert.Contains("spill=", text)
        finally
            ok (session.Close()) |> ignore)

[<Fact>]
let ``SqlReference: 頻出値（MCV）は等値の行数見積りに使う`` () =
    // 100行のうち 90行が v=1、残り 10行が 2..11。
    // NDV だけで割ると v=1 は約 9行になるが、MCV なら 90行になる。
    withSession "stats-mcv" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"

        let values =
            ([ 1..90 ] |> List.map (fun i -> sprintf "(%d,1)" i))
            @ ([ 91..100 ] |> List.map (fun i -> sprintf "(%d,%d)" i (i - 89)))
            |> String.concat ","

        let db2 = exec db1 (sprintf "INSERT INTO t VALUES %s" values)
        let db3 = exec db2 "ANALYZE t"

        let planOf db where =
            let _, plan = ok (execSql db ("EXPLAIN SELECT id FROM t WHERE " + where))
            plan.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"

        // 頻出値は実測の割合（90/100）。
        let frequent = planOf db3 "v = 1"
        Assert.Equal(90.0, plannedRows frequent, 1)

        // 未知の値は MCV の分を除いた残り（10行 / 10値）を使う。
        let rare = planOf db3 "v = 99"
        Assert.True(plannedRows rare < 5.0, rare))
