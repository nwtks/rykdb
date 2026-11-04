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

/// 1 文を実行して「新しい Database」と結果の両方を返す。
let private execRes (db: Database) (sql: string) = ok (db.Execute sql)

/// REAL を取り出す（集計結果の検証用）。
let private asReal (v: SqlValue) =
    match v with
    | SqlValue.Real f -> f
    | _ ->
        Assert.Fail(sprintf "REAL ではありません: %A" v)
        0.0

// ---------------------------------------------------------------------------
// 静かな誤答を防ぐ（識別子の大小文字・集計・HAVING・LIMIT・DEFAULT）
// ---------------------------------------------------------------------------

[<Fact>]
let ``SqlReference: 索引列と主キーの大文字小文字は区別しない`` () =
    withDb "case-ident" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE c (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO c VALUES (1, 'a'), (2, 'b')"
        let db3 = exec db2 "CREATE INDEX ci ON c (V)"

        // 索引キーが正しく作られ、索引経由でも行が見つかる
        let _, byIndex = ok (db3.Execute "SELECT id FROM c WHERE v = 'b'")
        Assert.Equal<int>(1, byIndex.Rows.Length)
        Assert.Equal<int64>(2L, asInt byIndex.Rows.[0].[0])

        // PRIMARY KEY (ID) の表記も通る
        let db4 = exec db3 "CREATE TABLE p (id INTEGER, v TEXT, PRIMARY KEY (ID))"
        let _, inserted = ok (db4.Execute "INSERT INTO p VALUES (1, 'x')")
        Assert.Equal(1, inserted.RowsAffected))

[<Fact>]
let ``SqlReference: SUM / AVG は型が混在しても値を落とさない`` () =
    withDb "sum-mixed" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE s (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO s VALUES (1), (2), (3)"

        // 1 + 1.5 + 1.5 = 4.0（INT と REAL の混在）
        let _, sum =
            ok (db2.Execute "SELECT SUM(CASE WHEN id = 1 THEN 1 ELSE 1.5 END) FROM s")

        Assert.Equal(4.0, asReal sum.Rows.[0].[0], 10)

        let _, avg =
            ok (db2.Execute "SELECT AVG(CASE WHEN id = 1 THEN 1 ELSE 1.5 END) FROM s")

        Assert.Equal(4.0 / 3.0, asReal avg.Rows.[0].[0], 10))

[<Fact>]
let ``SqlReference: 集約なしの HAVING はエラー`` () =
    withDb "having-noagg" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"

        expectErrorKind
            ErrorKind.SyntaxError
            "HAVING には GROUP BY か集約が必要"
            (db2.Execute "SELECT id FROM t HAVING id = 999"))

[<Fact>]
let ``SqlReference: GROUP BY と * の併用はエラー`` () =
    withDb "group-star" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10), (2, 10)"
        expectErrorKind ErrorKind.SyntaxError "SELECT * は GROUP BY と併用できない" (db2.Execute "SELECT * FROM t GROUP BY v"))

[<Fact>]
let ``SqlReference: テーブル名と同じ非修飾名は列として解決しない`` () =
    withDb "resolve-qual" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE q (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO q VALUES (1)"
        expectErrorKind ErrorKind.NameError "テーブル名を列として解決しない" (db2.Execute "SELECT q FROM q"))

[<Fact>]
let ``SqlReference: t.* の大文字小文字は区別しない`` () =
    withDb "star-case" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE st (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO st VALUES (1)"
        let _, r = ok (db2.Execute "SELECT ST.* FROM st")
        Assert.Equal<string list>([ "st.id" ], r.Columns |> Array.toList))

[<Fact>]
let ``SqlReference: FROM に無い t.* は NameError`` () =
    withDb "star-unknown" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1)"
        // 一致 0 件の `t.*` を「0列の 1行」で成功させない。
        expectErrorKind ErrorKind.NameError "未知のテーブル名の t.*" (db2.Execute "SELECT nope.* FROM t")
        expectErrorKind ErrorKind.NameError "別名を実名で指定した t.*" (db2.Execute "SELECT t.* FROM t AS x"))

[<Fact>]
let ``SqlReference: LIMIT の定数式と負数`` () =
    withDb "limit-expr" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2), (3)"

        // 定数式は評価される
        let _, one = ok (db2.Execute "SELECT id FROM t LIMIT 1+0")
        Assert.Equal<int>(1, one.Rows.Length)

        let _, offset = ok (db2.Execute "SELECT id FROM t LIMIT 2 OFFSET 1+0")
        Assert.Equal<int>(2, offset.Rows.Length)

        expectErrorKind ErrorKind.SyntaxError "LIMIT -1 はエラー" (db2.Execute "SELECT id FROM t LIMIT -1")
        expectError "LIMIT に列参照は使えない" (db2.Execute "SELECT id FROM t LIMIT id")

        // LIMIT 0 は 0 行（従来どおり）
        let _, none = ok (db2.Execute "SELECT id FROM t LIMIT 0")
        Assert.Empty none.Rows)

[<Fact>]
let ``SqlReference: DEFAULT の評価エラーは NULL にしない`` () =
    withDb "default-error" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER DEFAULT (1 / 0))"

        expectError "壊れた DEFAULT はエラーになる" (db1.Execute "INSERT INTO t (id) VALUES (1)"))

[<Fact>]
let ``SqlReference: DEFAULT は列型へ暗黙変換される`` () =
    withDb "default-coerce" (fun db0 ->
        // INTEGER の既定値を TEXT 列へ入れると TEXT に変換される
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT DEFAULT 42)"
        let db2, _ = ok (db1.Execute "INSERT INTO t (id) VALUES (1)")
        let _, rows = ok (db2.Execute "SELECT v FROM t")
        Assert.Equal<string>("42", asText rows.Rows.[0].[0])

        // 変換できない既定値は型エラー（TEXT のまま保存しない）
        let db3 =
            exec db2 "CREATE TABLE u (id INTEGER PRIMARY KEY, v INTEGER DEFAULT 'oops')"

        expectErrorKind ErrorKind.TypeError "INTEGER 列の変換できない DEFAULT" (db3.Execute "INSERT INTO u (id) VALUES (1)"))

[<Fact>]
let ``SqlReference: 複合索引の全列等値で行を引ける`` () =
    withDb "comp-index-eq" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 2, 10), (1, 3, 11), (2, 2, 12)"
        let db3 = exec db2 "CREATE INDEX ix_ab ON t (a, b)"

        // 索引の backfill 後、全列等値で 1 行だけ返る
        let _, r = ok (db3.Execute "SELECT id FROM t WHERE a = 1 AND b = 2")
        Assert.Equal<int>(1, r.Rows.Length)
        Assert.Equal<int64>(10L, asInt r.Rows.[0].[0])

        // 前置 1列だけでも IndexLookup（結果は同じ）
        let _, prefix = ok (db3.Execute "SELECT id FROM t WHERE a = 1")
        Assert.Equal<int>(2, prefix.Rows.Length))

[<Fact>]
let ``SqlReference: VARCHAR(32) は再オープン後も使える`` () =
    withDir "varchar-reopen" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v VARCHAR(32))"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'abc')"
        ok (db2.Close())

        // 型引数はカタログに保存されないが、列型（TEXT）と値はそのまま使える
        let db3 = ok (Database.OpenWith dir)
        let _, r = ok (db3.Execute "SELECT v FROM t")
        Assert.Equal<string>("abc", asText r.Rows.[0].[0])
        ok (db3.Close()))

[<Fact>]
let ``SqlReference: SQL の BEGIN / COMMIT / ROLLBACK は受理される`` () =
    // 文として受理される（§12）。transaction を実際に制御する様子は
    // `SQL の BEGIN と ROLLBACK で書き込みを取り消す` で検証する。
    withDb "tx5" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "BEGIN"
        let db3 = exec db2 "INSERT INTO t VALUES (1)"
        let db4 = exec db3 "COMMIT"
        let db5, res = ok (db4.Execute "SELECT id FROM t")
        Assert.Equal<int>(1, res.Rows.Length)
        // SQL の BEGIN はライブラリ API のハンドル状態（InTransaction）を立てない
        Assert.False db5.InTransaction

        let db6 = exec db5 "ROLLBACK"
        let _, after = ok (db6.Execute "SELECT id FROM t")
        Assert.Equal<int>(1, after.Rows.Length))

[<Fact>]
let ``SqlReference: CREATE TABLE でテーブルができる`` () =
    withDb "ddl1" (fun db0 ->
        let db =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        Assert.Equal<string list>([ "people" ], db.TableNames()))

[<Fact>]
let ``SqlReference: CREATE TABLE IF NOT EXISTS は 2 回呼んでも OK`` () =
    withDb "ddl2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"

        let db2 =
            exec db1 "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY, name TEXT)"

        Assert.Equal<string list>([ "people" ], db2.TableNames()))

[<Fact>]
let ``SqlReference: CREATE TABLE の重複はエラー（IF NOT EXISTS なし）`` () =
    withDb "ddl3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"

        expectError
            "重複した CREATE TABLE が OK になっています"
            (db1.Execute "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"))

[<Fact>]
let ``SqlReference: テーブル制約の複合主キー`` () =
    withDb "ddl6" (fun db0 ->
        // 主キーをテーブル制約で指定（列制約ではない）
        let db1 =
            exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, v INTEGER, PRIMARY KEY (a, b))"

        let db2, ins = ok (db1.Execute "INSERT INTO t (a, b) VALUES (1, 2), (1, 3)")
        Assert.Equal<int>(2, ins.RowsAffected)

        // 同じ a でも b が違えば別の行
        let _, res = ok (db2.Execute "SELECT a, b FROM t ORDER BY b")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: 複合主キーは全列一致で検索でき、部分一致も取りこぼさない`` () =
    withDb "ddl6-search" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, v TEXT, PRIMARY KEY (a, b))"

        let db2 = exec db1 "INSERT INTO t VALUES (1, 2, 'x'), (1, 3, 'y'), (2, 2, 'z')"

        let _, full = ok (db2.Execute "SELECT v FROM t WHERE a = 1 AND b = 2")
        Assert.Equal<int>(1, full.Rows.Length)
        Assert.Equal<string>("x", asText full.Rows.[0].[0])

        let _, partialA = ok (db2.Execute "SELECT v FROM t WHERE a = 1 ORDER BY b")
        Assert.Equal<string list>([ "x"; "y" ], partialA.Rows |> Array.map (fun r -> asText r.[0]) |> List.ofArray)

        let _, partialB = ok (db2.Execute "SELECT v FROM t WHERE b = 2 ORDER BY a")
        Assert.Equal<string list>([ "x"; "z" ], partialB.Rows |> Array.map (fun r -> asText r.[0]) |> List.ofArray)

        // 型が違うリテラルは黙って 0 行にせず、比較時に型エラーにする
        expectErrorKind ErrorKind.TypeError "型不一致の主キー検索" (db2.Execute "SELECT v FROM t WHERE a = '1'"))

[<Fact>]
let ``SqlReference: DROP TABLE でテーブルが消える`` () =
    withDb "ddl4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"
        let db2 = exec db1 "DROP TABLE people"
        Assert.Empty(db2.TableNames()))

[<Fact>]
let ``SqlReference: DROP TABLE IF EXISTS は無ければ OK`` () =
    withDb "ddl5" (fun db0 ->
        let db1 = exec db0 "DROP TABLE IF EXISTS nothing"
        Assert.Empty(db1.TableNames()))

[<Fact>]
let ``SqlReference: 存在しない列の索引はエラー`` () =
    withDb "ddl7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"
        expectError "存在しない列の CREATE INDEX が OK になっています" (db1.Execute "CREATE INDEX ix ON people (nope)"))

[<Fact>]
let ``SqlReference: 存在しないテーブルの索引と索引名の重複はエラー`` () =
    withDb "ddl8" (fun db0 ->
        expectError "存在しないテーブルの CREATE INDEX が OK になっています" (db0.Execute "CREATE INDEX ix ON nope (a)")

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_a ON t (a)"
        expectError "同名の CREATE INDEX が OK になっています" (db2.Execute "CREATE INDEX ix_a ON t (a)"))

[<Fact>]
let ``SqlReference: DROP INDEX で索引が消える`` () =
    withDb "ddl9" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2, _ = execRes db1 "INSERT INTO t VALUES (1, 10)"

        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        // 索引を使う検索が通る
        let _, found = ok (db3.Execute "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, found.Rows.Length)

        let db4 = exec db3 "DROP INDEX ix_a"
        let _, after = ok (db4.Execute "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, after.Rows.Length)

        // 消えたので同じ名前で作り直せる
        let db5 = exec db4 "CREATE INDEX ix_a ON t (a)"
        let _, again = ok (db5.Execute "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, again.Rows.Length))

[<Fact>]
let ``SqlReference: DROP INDEX IF EXISTS と存在しない索引のエラー`` () =
    withDb "ddl10" (fun db0 ->
        let db1 = exec db0 "DROP INDEX IF EXISTS nothing"
        expectError "存在しない DROP INDEX が OK になっています" (db1.Execute "DROP INDEX nothing"))

[<Fact>]
let ``SqlReference: 暗黙の UNIQUE 索引は DROP できない`` () =
    withDb "ddl-implicit-unique" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a@example.com')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "暗黙の UNIQUE 索引を DROP できてしまう"
            (db2.Execute "DROP INDEX __unique_t_0")

        // DROP に失敗しても制約は残る
        expectErrorKind
            ErrorKind.ConstraintViolation
            "暗黙 UNIQUE 索引の DROP 後に制約が消えている"
            (db2.Execute "INSERT INTO t VALUES (2, 'a@example.com')")

        // 予約名の索引はユーザーが作れない
        expectErrorKind ErrorKind.NameError "予約された索引名を許してはいけない" (db2.Execute "CREATE INDEX __unique_t_9 ON t (email)"))

[<Fact>]
let ``SqlReference: UNIQUE 制約は複数の NULL を許す`` () =
    withDb "unique-null" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE u (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO u VALUES (1, NULL), (2, NULL)"
        let _, count = ok (db2.Execute "SELECT COUNT(*) FROM u")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0])

        expectErrorKind
            ErrorKind.ConstraintViolation
            "UNIQUE の非 NULL 重複を許してはいけない"
            (db2.Execute "INSERT INTO u VALUES (3, 'x'), (4, 'x')"))

[<Fact>]
let ``SqlReference: 複合 UNIQUE は NULL を含む行を重複扱いしない`` () =
    withDb "unique-null-composite" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE u (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, UNIQUE (a, b))"

        let db2 = exec db1 "INSERT INTO u VALUES (1, NULL, 1), (2, NULL, 2)"
        let _, count = ok (db2.Execute "SELECT COUNT(*) FROM u")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0])

        expectErrorKind
            ErrorKind.ConstraintViolation
            "複合 UNIQUE の非 NULL 重複を許してはいけない"
            (db2.Execute "INSERT INTO u VALUES (3, 7, 1), (4, 7, 1)"))

[<Fact>]
let ``SqlReference: UNIQUE 列の UPDATE でも NULL は重複扱いしない`` () =
    withDb "unique-null-update" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "UNIQUE 列の UPDATE で非 NULL 重複を許してはいけない"
            (db2.Execute "UPDATE t SET email = 'x'")

        let db3 = exec db2 "UPDATE t SET email = NULL"
        let _, count = ok (db3.Execute "SELECT COUNT(*) FROM t WHERE email IS NULL")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: UPDATE で UNIQUE 値を交換できる`` () =
    withDb "unique-update-swap" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        // 更新対象外の行との衝突は即時エラー
        expectErrorKind
            ErrorKind.ConstraintViolation
            "更新対象外の UNIQUE 値との衝突を許してはいけない"
            (db2.Execute "UPDATE t SET email = 'b' WHERE id = 1")

        // 同一文内で入れ替える更新は最終状態が一意なので成功する
        let db3 = exec db2 "UPDATE t SET email = CASE WHEN id = 1 THEN 'b' ELSE 'a' END"
        let _, rows = ok (db3.Execute "SELECT id, email FROM t ORDER BY id")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0])
        Assert.Equal<string>("b", asText rows.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt rows.Rows.[1].[0])
        Assert.Equal<string>("a", asText rows.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: table-level PRIMARY KEY は NULL を拒否する`` () =
    withDb "table-pk-not-null" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER, v TEXT, PRIMARY KEY (id))"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "table-level PRIMARY KEY の NULL を許してはいけない"
            (db1.Execute "INSERT INTO t VALUES (NULL, 'x')"))

[<Fact>]
let ``SqlReference: 重複列・重複 INSERT 列・重複 UPDATE 代入を拒否する`` () =
    withDb "duplicate-names" (fun db0 ->
        expectErrorKind
            ErrorKind.NameError
            "CREATE TABLE の重複列を許してはいけない"
            (db0.Execute "CREATE TABLE dup (id INTEGER PRIMARY KEY, a INTEGER, A TEXT)")

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"

        expectErrorKind
            ErrorKind.NameError
            "INSERT の重複列を許してはいけない"
            (db1.Execute "INSERT INTO t (id, a, a) VALUES (1, 2, 3)")

        expectErrorKind ErrorKind.NameError "UPDATE の重複代入を許してはいけない" (db1.Execute "UPDATE t SET a = 1, a = 2")

        expectErrorKind
            ErrorKind.NameError
            "CREATE INDEX の重複列を許してはいけない"
            (db1.Execute "CREATE INDEX ix_dup ON t (a, a)"))

[<Fact>]
let ``SqlReference: 長すぎるテーブル名・索引名は拒否する`` () =
    withDb "long-name" (fun db0 ->
        let name = System.String('a', 128 + 1)

        expectErrorKind
            ErrorKind.NameError
            "長すぎるテーブル名を許してはいけない"
            (db0.Execute(sprintf "CREATE TABLE %s (id INTEGER PRIMARY KEY)" name))

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"

        expectErrorKind ErrorKind.NameError "長すぎる索引名を許してはいけない" (db1.Execute(sprintf "CREATE INDEX %s ON t (id)" name)))

[<Fact>]
let ``SqlReference: DEFAULT 付きの列定義も保存できる`` () =
    withDb "ddl-default" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT DEFAULT 'x')"

        // 保存時に DEFAULT 式が書かれる（再読込後に既定値として使われるかは別問題）
        let _, rows = ok (db1.Execute "SELECT id, v FROM t")
        Assert.Empty rows.Rows)

[<Fact>]
let ``SqlReference: DEFAULT は再オープン後も既定値として使われる`` () =
    withDir "default-e2e" (fun dir ->
        let db0 = ok (Database.OpenWith dir)

        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER DEFAULT 42, s TEXT DEFAULT 'hi')"

        let db2 = exec db1 "INSERT INTO t (id) VALUES (1)"
        let _, before = ok (db2.Execute "SELECT v, s FROM t WHERE id = 1")
        Assert.Equal<int64>(42L, asInt before.Rows.[0].[0])
        Assert.Equal<string>("hi", asText before.Rows.[0].[1])
        ok (db2.Close())

        let db3 = ok (Database.OpenWith dir)
        let _, after = ok (db3.Execute "SELECT v, s FROM t WHERE id = 1")
        Assert.Equal<int64>(42L, asInt after.Rows.[0].[0])
        Assert.Equal<string>("hi", asText after.Rows.[0].[1])

        let db4 = exec db3 "INSERT INTO t (id) VALUES (2)"
        let _, inserted = ok (db4.Execute "SELECT v, s FROM t WHERE id = 2")
        Assert.Equal<int64>(42L, asInt inserted.Rows.[0].[0])
        Assert.Equal<string>("hi", asText inserted.Rows.[0].[1])
        ok (db4.Close()))

[<Fact>]
let ``SqlReference: 数値の範囲外は例外ではなくエラーを返す`` () =
    withDb "numeric-e2e" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v REAL, d DECIMAL)"
        let db2 = exec db1 "INSERT INTO t (id, v, d) VALUES (1, 1e30, 7.9e28)"

        // REAL は decimal の範囲を超えてもそのまま計算できる
        let _, big = ok (db2.Execute "SELECT (v * v) FROM t WHERE id = 1")

        match big.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(f > 1e59 && f < 1e61, sprintf "想定外の REAL: %f" f)
        | v -> Assert.Fail(sprintf "REAL ではありません: %A" v)

        // 極小の REAL も 0 に丸めない
        let _, tiny = ok (db2.Execute "SELECT (1e-30 * 1.0) FROM t WHERE id = 1")

        match tiny.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(f > 0.0 && f < 1e-29, sprintf "想定外の REAL: %f" f)
        | v -> Assert.Fail(sprintf "REAL ではありません: %A" v)

        let _, sum = ok (db2.Execute "SELECT SUM(v) FROM t")

        match sum.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.Equal(1e30, f)
        | v -> Assert.Fail(sprintf "REAL ではありません: %A" v)

        // 整数・DECIMAL のオーバーフローは DbError になる
        expectErrorKind
            ErrorKind.ConstraintViolation
            "整数加算のオーバーフロー"
            (db2.Execute "SELECT 9223372036854775807 + 1 FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "整数除算のオーバーフロー"
            (db2.Execute "SELECT (-9223372036854775807 - 1) / -1 FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "ABS のオーバーフロー"
            (db2.Execute "SELECT ABS(-9223372036854775807 - 1) FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "DECIMAL 乗算のオーバーフロー"
            (db2.Execute "SELECT (d * d) FROM t WHERE id = 1")

        // DECIMAL の範囲外の REAL は変換時に型エラー
        expectErrorKind ErrorKind.TypeError "DECIMAL の範囲外" (db2.Execute "INSERT INTO t (id, d) VALUES (2, 1e30)"))

[<Fact>]
let ``SqlReference: INSERT した行が SELECT で見える`` () =
    withDb "dml1" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, ins =
            ok (db1.Execute "INSERT INTO people VALUES (1, 'alice', 30), (2, 'bob', 25)")

        Assert.Equal<int>(2, ins.RowsAffected)

        let _db1, res = ok (db2.Execute "SELECT id, name FROM people ORDER BY id")
        Assert.Equal<string array>([| "id"; "name" |], res.Columns)
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<string>("alice", asText res.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: INSERT は複数 VALUES を 1 文で`` () =
    withDb "dml2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2, ins = ok (db1.Execute "INSERT INTO t VALUES (1,'a'),(2,'b'),(3,'c')")
        Assert.Equal<int>(3, ins.RowsAffected)

        let _db2, res = ok (db2.Execute "SELECT id FROM t")
        Assert.Equal<int>(3, res.Rows.Length))

[<Fact>]
let ``SqlReference: UPDATE は WHERE に合う行だけを変える`` () =
    withDb "dml3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = execRes db1 "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)"
        let db3, upd = ok (db2.Execute "UPDATE t SET v = 99 WHERE id > 1")
        Assert.Equal<int>(2, upd.RowsAffected)

        let _db3, res = ok (db3.Execute "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(99L, asInt res.Rows.[1].[1])
        Assert.Equal<int64>(99L, asInt res.Rows.[2].[1]))

[<Fact>]
let ``SqlReference: DELETE は WHERE に合う行を消す`` () =
    withDb "dml4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)")
        let db3, del = ok (db2.Execute "DELETE FROM t WHERE v >= 20")
        Assert.Equal<int>(2, del.RowsAffected)

        let _db4, res = ok (db3.Execute "SELECT id FROM t")
        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0]))

/// テスト用のテーブルを用意する。
let private seeded (tag: string) (f: Database -> unit) =
    withDb tag (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, _ =
            ok (db1.Execute "INSERT INTO people VALUES (1,'alice',30),(2,'bob',25),(3,'carol',35)")

        f db2)

[<Fact>]
let ``SqlReference: SELECT * は全列を返す`` () =
    seeded "sel1" (fun db ->
        let _db5, res = ok (db.Execute "SELECT * FROM people ORDER BY id")
        Assert.Equal<string array>([| "id"; "name"; "age" |], res.Columns)
        Assert.Equal<int>(3, res.Rows.Length))

[<Fact>]
let ``SqlReference: WHERE で絞る`` () =
    seeded "sel2" (fun db ->
        let _db6, res =
            ok (db.Execute "SELECT name FROM people WHERE age > 26 ORDER BY name")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: ORDER BY が効く`` () =
    seeded "sel3" (fun db ->
        let _, asc = ok (db.Execute "SELECT name FROM people ORDER BY age")
        Assert.Equal<string>("bob", asText asc.Rows.[0].[0])

        let _, desc = ok (db.Execute "SELECT name FROM people ORDER BY age DESC")
        Assert.Equal<string>("carol", asText desc.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 主キー順の ORDER BY は昇順・降順・LIMIT が正しい`` () =
    // 主キー昇順の ORDER BY は Sort ノードを作らない（走査順がそのまま答え）。
    withDb "orderby-pk" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (3,'c'),(1,'a'),(4,'d'),(2,'b')")

        let ids sql =
            let _, res = ok (db2.Execute sql)
            res.Rows |> Array.map (fun r -> int (asInt r.[0])) |> Array.toList

        Assert.Equal<int list>([ 1; 2; 3; 4 ], ids "SELECT id FROM t ORDER BY id")
        Assert.Equal<int list>([ 4; 3; 2; 1 ], ids "SELECT id FROM t ORDER BY id DESC")
        Assert.Equal<int list>([ 1; 2 ], ids "SELECT id FROM t ORDER BY id LIMIT 2")
        Assert.Equal<int list>([ 2; 3 ], ids "SELECT id FROM t ORDER BY id LIMIT 2 OFFSET 1")
        // 別名越しでも列参照に解決される
        Assert.Equal<int list>([ 1; 2; 3; 4 ], ids "SELECT id AS k FROM t ORDER BY k"))

[<Fact>]
let ``SqlReference: 複合主キーは全列の昇順で並ぶ`` () =
    withDb "orderby-pk2" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE c (a INTEGER, b INTEGER, v TEXT, PRIMARY KEY (a, b))"

        let db2, _ =
            ok (db1.Execute "INSERT INTO c VALUES (2,2,'x'),(1,5,'y'),(1,2,'z'),(2,1,'w')")

        let _, rows = ok (db2.Execute "SELECT a, b FROM c ORDER BY a, b")

        let pairs =
            rows.Rows
            |> Array.map (fun r -> int (asInt r.[0]), int (asInt r.[1]))
            |> Array.toList

        Assert.Equal<(int * int) list>([ (1, 2); (1, 5); (2, 1); (2, 2) ], pairs)

        // 主キーの先頭 1列だけでは並びが決まらないので、全列の昇順の結果だけを確認する
        let _, partial = ok (db2.Execute "SELECT a FROM c ORDER BY a")

        Assert.Equal<int list>([ 1; 1; 2; 2 ], partial.Rows |> Array.map (fun r -> int (asInt r.[0])) |> Array.toList))

[<Fact>]
let ``SqlReference: 別名を ORDER BY で参照できる`` () =
    seeded "sel-alias-order" (fun db ->
        // 別名 a は age を指すので age の順（bob 25 → alice 30 → carol 35）で並ぶ
        let _, res = ok (db.Execute "SELECT name AS x, age AS a FROM people ORDER BY a")
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<string>("bob", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[2].[0]))

[<Fact>]
let ``SqlReference: 別名を GROUP BY と HAVING で参照できる`` () =
    seeded "sel-alias-group" (fun db ->
        let _, res = ok (db.Execute "SELECT age AS a, COUNT(*) FROM people GROUP BY a")
        Assert.Equal<int>(3, res.Rows.Length)

        // HAVING にも別名を参照できる
        let _, having = ok (db.Execute "SELECT COUNT(*) AS c FROM people HAVING c > 2")
        Assert.Equal<int>(1, having.Rows.Length)
        Assert.Equal<int64>(3L, asInt having.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 別名より列が優先される`` () =
    seeded "sel-alias-priority" (fun db ->
        // age という列があるのに age という別名を付けても、ORDER BY は列を見る
        // （列の順: bob 25 → alice 30 → carol 35。別名=name なら alice が先）
        let _, res = ok (db.Execute "SELECT name AS age FROM people ORDER BY age")
        Assert.Equal<string>("alice", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 別名の循環参照でも止まる`` () =
    seeded "sel-alias-cycle" (fun db ->
        // x と y が互いを指す（差し替えで無限ループにしない）
        expectError "循環参照する別名は参照できない" (db.Execute "SELECT x AS y, y AS x FROM people ORDER BY x"))

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
        let _db7, res = ok (db.Execute "SELECT id FROM people ORDER BY id LIMIT 2")
        Assert.Equal<int>(2, res.Rows.Length))

[<Fact>]
let ``SqlReference: OFFSET`` () =
    seeded "sel6" (fun db ->
        let _db8, res = ok (db.Execute "SELECT id FROM people ORDER BY id LIMIT 5 OFFSET 1")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: DISTINCT`` () =
    withDb "sel7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30)")
        let _db9, res = ok (db2.Execute "SELECT DISTINCT g FROM t ORDER BY g")
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: DISTINCT を続けて実行しても毎回全行が返る`` () =
    // 既出集合をモジュール初期化時に 1個だけ作ると、2回目以降が全行を重複と見なして 0行になる。
    withDb "sel7b" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30)")

        for _ in 1..3 do
            let _db, res = ok (db2.Execute "SELECT DISTINCT g FROM t ORDER BY g")
            Assert.Equal<int>(3, res.Rows.Length)
            Assert.Equal<int64>(10L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 別名を付けられる`` () =
    seeded "sel8" (fun db ->
        let _db10, res = ok (db.Execute "SELECT name AS n FROM people ORDER BY id LIMIT 1")
        Assert.Equal<string array>([| "n" |], res.Columns))

[<Fact>]
let ``SqlReference: 集約関数`` () =
    seeded "sel9" (fun db ->
        let _, cnt = ok (db.Execute "SELECT COUNT(*) FROM people")
        Assert.Equal<int64>(3L, asInt cnt.Rows.[0].[0])

        let _, sum = ok (db.Execute "SELECT SUM(age) FROM people")
        Assert.True(SqlValue.isNull sum.Rows.[0].[0] |> not)

        let _, mn = ok (db.Execute "SELECT MIN(age), MAX(age) FROM people")
        Assert.Equal<int64>(25L, asInt mn.Rows.[0].[0])
        Assert.Equal<int64>(35L, asInt mn.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: JOIN`` () =
    withDb "sel10" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, _ = execRes db1 "INSERT INTO people VALUES (1,'alice',30),(2,'bob',25)"

        let db3 =
            exec db2 "CREATE TABLE orders (oid INTEGER PRIMARY KEY, person_id INTEGER, amount INTEGER)"

        let db4, _ = execRes db3 "INSERT INTO orders VALUES (1,1,100),(2,1,200),(3,2,50)"

        let _, res =
            ok (
                db4.Execute
                    "SELECT people.name, SUM(orders.amount) FROM people JOIN orders ON people.id = orders.person_id GROUP BY people.name ORDER BY people.name"
            )

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("bob", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 3 テーブルの入れ子 JOIN が行を返す`` () =
    // exec は左部分木に「全スキーマ」を渡していたため、3 テーブル以上の入れ子
    // 結合で内側の JOIN が右テーブルの列まで含んだ schema で ON 句を解決し、
    // 列が見つからずに INNER JOIN が黙って 0 行を返していた。
    // 左へ下ろすのは「右テーブルを除いたスキーマ」。
    withDb "join3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, av INTEGER)"
        let db3 = exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, bv INTEGER)"
        let db4, _ = execRes db3 "INSERT INTO a VALUES (1,10),(2,20)"
        let db5, _ = execRes db4 "INSERT INTO b VALUES (1,100),(2,200)"
        let db6, _ = execRes db5 "INSERT INTO c VALUES (1,1000),(2,2000)"

        let _, res =
            ok (db6.Execute "SELECT a.v, b.av, c.bv FROM a JOIN b ON a.id = b.id JOIN c ON b.id = c.id ORDER BY a.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(100L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(1000L, asInt res.Rows.[0].[2])
        Assert.Equal<int64>(20L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(200L, asInt res.Rows.[1].[1])
        Assert.Equal<int64>(2000L, asInt res.Rows.[1].[2]))

[<Fact>]
let ``SqlReference: エイリアスでの自己結合が行を返す`` () =
    // JOIN の対応付けを RealName で探していたころは、`FROM t AS x JOIN t AS y` が
    // 先頭の t にヒットして JOIN が丸ごと省略され、
    // 列インデックスが範囲外（2 列中）になって落ちていた。
    withDb "selfjoin" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20),(3,30)"

        // 右だけを絞り込む WHERE が効くこと（JOIN が本当に組まれていること）。
        let _, res =
            ok (db2.Execute "SELECT p.id, q.v FROM t AS p JOIN t AS q ON p.id = q.id WHERE q.v > 15 ORDER BY p.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(20L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(30L, asInt res.Rows.[1].[1])

        // 非キーでの自己結合（それぞれ自分自身とだけ一致する）。
        let _, res2 =
            ok (db2.Execute "SELECT p.id, q.id FROM t AS p JOIN t AS q ON p.v = q.v ORDER BY p.id")

        Assert.Equal<int>(3, res2.Rows.Length)
        Assert.Equal<int64>(2L, asInt res2.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt res2.Rows.[1].[1])

        // 3 段の自己結合（p / q / r すべて t のエイリアス）。
        let _, res3 =
            ok (
                db2.Execute
                    "SELECT p.id, q.id, r.v FROM t AS p JOIN t AS q ON p.id = q.id JOIN t AS r ON q.id = r.id WHERE p.v = 30 ORDER BY p.id"
            )

        Assert.Equal<int>(1, res3.Rows.Length)
        Assert.Equal<int64>(3L, asInt res3.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res3.Rows.[0].[1])
        Assert.Equal<int64>(30L, asInt res3.Rows.[0].[2]))

[<Fact>]
let ``SqlReference: ハッシュ結合（INNER / LEFT / NULL キー）`` () =
    withDb "joinhash" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db3 = exec db2 "INSERT INTO a VALUES (1,10),(2,20),(3,NULL)"
        let db4 = exec db3 "INSERT INTO b VALUES (1,10),(2,10),(3,30),(4,NULL)"

        // INNER: NULL キーは一致しない（a3 は落ちる）
        let _, inner =
            ok (db4.Execute "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k ORDER BY a.id, b.id")

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<int64>(1L, asInt inner.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt inner.Rows.[0].[1])
        Assert.Equal<int64>(1L, asInt inner.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt inner.Rows.[1].[1])

        // LEFT: 一致しない a2 / a3 は右側 NULL
        let _, left =
            ok (db4.Execute "SELECT a.id, b.id FROM a LEFT JOIN b ON a.k = b.k ORDER BY a.id, b.id")

        Assert.Equal<int>(4, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[2].[1])
        Assert.True(SqlValue.isNull left.Rows.[3].[1])

        // 残りの ON 句（b.id = 1）はハッシュ結合でも評価される
        let _, residual =
            ok (db4.Execute "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k AND b.id = 1")

        Assert.Equal<int>(1, residual.Rows.Length)
        Assert.Equal<int64>(1L, asInt residual.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: マージ結合（主キー同士・LEFT・複合キー）`` () =
    withDb "joinmerge" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE m1 (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE TABLE m2 (id INTEGER PRIMARY KEY, w INTEGER)"
        let db3 = exec db2 "INSERT INTO m1 VALUES (1,10),(2,20),(4,40)"
        let db4 = exec db3 "INSERT INTO m2 VALUES (2,200),(3,300),(4,400)"

        // 両方が主キー id の走査 → マージ結合
        let _, inner =
            ok (db4.Execute "SELECT m1.id, m1.v, m2.w FROM m1 JOIN m2 ON m1.id = m2.id ORDER BY m1.id")

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<int64>(2L, asInt inner.Rows.[0].[0])
        Assert.Equal<int64>(200L, asInt inner.Rows.[0].[2])
        Assert.Equal<int64>(4L, asInt inner.Rows.[1].[0])
        Assert.Equal<int64>(400L, asInt inner.Rows.[1].[2])

        // LEFT: 一致しない 1 は右側 NULL
        let _, left =
            ok (db4.Execute "SELECT m1.id, m2.w FROM m1 LEFT JOIN m2 ON m1.id = m2.id ORDER BY m1.id")

        Assert.Equal<int>(3, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[0].[1])
        Assert.Equal<int64>(200L, asInt left.Rows.[1].[1])
        Assert.Equal<int64>(400L, asInt left.Rows.[2].[1])

        // ON の残り条件で一致が消えた左行も NULL 拡張する
        let _, leftResidual =
            ok (db4.Execute "SELECT m1.id, m2.w FROM m1 LEFT JOIN m2 ON m1.id = m2.id AND m2.w > 250 ORDER BY m1.id")

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
                c4.Execute
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
                d6.Execute
                    "SELECT d1.id, d2.id FROM d1 JOIN d2 ON d1.k = d2.k WHERE d1.k >= 0 AND d2.k >= 0 ORDER BY d1.id, d2.id"
            )

        Assert.Equal<int>(2, duplicates.Rows.Length)
        Assert.Equal<int64>(2L, asInt duplicates.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt duplicates.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt duplicates.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt duplicates.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: インデックス入れ子ループ（右索引 + 左が小さい）`` () =
    withDb "joinindex" (fun db0 ->
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
                db5.Execute
                    "SELECT depts.name, users.name FROM depts JOIN users ON depts.id = users.dept_id WHERE depts.id = 1 ORDER BY users.id"
            )

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<string>("alice", asText inner.Rows.[0].[1])
        Assert.Equal<string>("bob", asText inner.Rows.[1].[1])

        // 一致なしの LEFT JOIN は右側 NULL
        let _, left =
            ok (
                db5.Execute
                    "SELECT depts.name, users.name FROM depts LEFT JOIN users ON depts.id = users.dept_id WHERE depts.id = 3"
            )

        Assert.Equal<int>(1, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[0].[1])

        // 等値キー以外の ON 句も索引結合で評価される
        let _, residual =
            ok (
                db5.Execute
                    "SELECT users.name FROM depts JOIN users ON depts.id = users.dept_id AND users.name = 'bob' WHERE depts.id = 1"
            )

        Assert.Equal<int>(1, residual.Rows.Length)
        Assert.Equal<string>("bob", asText residual.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: JOIN の非修飾で曖昧な列は NameError`` () =
    // 非修飾 id は p と q の両方にある。各テーブルへ pushdown すると
    // それぞれ id = 1 で絞ってしまい、曖昧エラーにならず意味が変わる。
    withDb "joinambig" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"

        expectErrorKind
            ErrorKind.NameError
            "JOIN の曖昧な非修飾列がエラーになりません"
            (db2.Execute "SELECT p.id FROM t AS p JOIN t AS q ON p.id = q.id WHERE id = 1"))

[<Fact>]
let ``SqlReference: テーブル名の重複はエラー（エイリアスが必要）`` () =
    // 識別名が重複すると exec が名前でスキーマを引けず（両方が消える）、
    // エラーではなく静かな誤答になるので、名前解決の段階で止める。
    withDb "dupjoin" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10)"

        expectErrorKind
            ErrorKind.NameError
            "重複したテーブル名がエラーになりません"
            (db2.Execute "SELECT t.id FROM t JOIN t ON t.id = t.id"))

[<Fact>]
let ``SqlReference: JOIN の WHERE は右テーブルと横断述語に効く`` () =
    withDb "sel11" (fun db0 ->
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
                db4.Execute
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE orders.amount = 100"
            )

        Assert.Equal<int>(1, rightOnly.Rows.Length)
        Assert.Equal<int64>(1L, asInt rightOnly.Rows.[0].[0])

        // 非修飾でも一意に決まる列（amount は orders だけ）は従来どおり pushdown できる
        let _, unqualifiedUnique =
            ok (
                db4.Execute
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE amount = 100"
            )

        Assert.Equal<int>(1, unqualifiedUnique.Rows.Length)
        Assert.Equal<int64>(1L, asInt unqualifiedUnique.Rows.[0].[0])

        // 2 テーブルにまたがる WHERE（ON に無い述語が JOIN 上で評価される）
        let _, cross =
            ok (
                db4.Execute
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE people.age * 4 < orders.amount"
            )

        Assert.Equal<int>(1, cross.Rows.Length)
        Assert.Equal<int64>(1L, asInt cross.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: LEFT JOIN の WHERE は null 供給側に効く`` () =
    withDb "sel12" (fun db0 ->
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
                db4.Execute
                    "SELECT people.id FROM people LEFT JOIN orders ON people.id = orders.person_id WHERE orders.amount IS NULL"
            )

        Assert.Equal<int>(1, isNullRows.Rows.Length)
        Assert.Equal<int64>(3L, asInt isNullRows.Rows.[0].[0])

        // 右への pushdown を入れると carol まで残って 3 行になる
        let _, matched =
            ok (
                db4.Execute
                    "SELECT people.id FROM people LEFT JOIN orders ON people.id = orders.person_id WHERE orders.amount = 100"
            )

        Assert.Equal<int>(1, matched.Rows.Length)
        Assert.Equal<int64>(1L, asInt matched.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 複数文を ; で渡せる`` () =
    withDb "tx3" (fun db0 ->
        let sql =
            "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER); "
            + "INSERT INTO t VALUES (1, 1); "
            + "INSERT INTO t VALUES (2, 2)"

        let db, _r = execRes db0 sql
        let _q, res = execRes db "SELECT id FROM t"
        Assert.Equal<int>(2, res.Rows.Length))

[<Fact>]
let ``SqlReference: セミコロンなしの複数文はエラー`` () =
    withDb "p0-separator" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"

        expectError "セミコロンなしの複数文が OK になっています" (db1.Execute "SELECT id FROM t SELECT id FROM t")

        // `;` があれば従来どおり複数文として実行できる
        let db2 = exec db1 "INSERT INTO t VALUES (1); INSERT INTO t VALUES (2)"
        let _, res = ok (db2.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: UNION はエラーになり 2 文として実行されない`` () =
    withDb "p0-union" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"

        expectError "UNION がエラーになっていません" (db2.Execute "SELECT id FROM t UNION SELECT id FROM t")

        // 2 文として実行されていれば RowsAffected の合計などが返るが、ここではエラーになる。
        let _, res = ok (db2.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: RIGHT JOIN はエラーになり INNER JOIN として実行されない`` () =
    withDb "p0-right-join" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE u (id INTEGER PRIMARY KEY, w TEXT)"
        let db3 = exec db2 "INSERT INTO t VALUES (1)"
        let db4 = exec db3 "INSERT INTO u VALUES (1, 'x'), (2, 'y')"

        expectError "RIGHT JOIN がエラーになっていません" (db4.Execute "SELECT u.id FROM t RIGHT JOIN u ON u.id = 1"))

// ---------------------------------------------------------------------------
// エラー
// ---------------------------------------------------------------------------

[<Fact>]
let ``SqlReference: 存在しないテーブルの SELECT はエラー`` () =
    withDb "err1" (fun db -> expectError "存在しないテーブルの SELECT が OK になっています" (db.Execute "SELECT * FROM nothing"))

[<Fact>]
let ``SqlReference: 存在しない列の WHERE はエラー`` () =
    seeded "err2" (fun db ->
        expectError "存在しない列の WHERE が OK になっています" (db.Execute "SELECT id FROM people WHERE nope = 1"))

[<Fact>]
let ``SqlReference: 構文エラーはエラー（例外ではない）`` () =
    withDb "err3" (fun db -> expectError "構文エラーが OK になっています" (db.Execute "SELEC 1"))

[<Fact>]
let ``SqlReference: NOT NULL 制約違反はエラー`` () =
    withDb "err4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER NOT NULL)"

        expectError "NOT NULL 違反が OK になっています" (db1.Execute "INSERT INTO t (id) VALUES (1)"))

[<Fact>]
let ``SqlReference: QueryResult の表示が使える`` () =
    seeded "disp1" (fun db ->
        let _db15, res = ok (db.Execute "SELECT id FROM people ORDER BY id LIMIT 1")
        let lines = res.ToDisplayLines()

        Assert.True(lines |> List.exists (fun l -> l.Contains "id"))
        Assert.True(lines |> List.exists (fun l -> l.Contains "1")))

[<Fact>]
let ``SqlReference: CASE WHEN が評価できる`` () =
    seeded "reg-case" (fun db ->
        let _, res =
            ok (db.Execute "SELECT name FROM people WHERE (CASE WHEN age > 26 THEN 1 ELSE 0 END) = 1 ORDER BY name")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: CASE WHEN が射影に書ける`` () =
    seeded "reg-case2" (fun db ->
        let _, res =
            ok (
                db.Execute
                    "SELECT name AS n, CASE WHEN age > 26 THEN 'old' ELSE 'young' END AS band FROM people ORDER BY id"
            )

        Assert.Equal<string array>([| "n"; "band" |], res.Columns)
        Assert.Equal<string>("old", asText res.Rows.[0].[1])
        Assert.Equal<string>("young", asText res.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: LIMIT は OFFSET を適用してから取る`` () =
    seeded "reg-limit" (fun db ->
        let _, res = ok (db.Execute "SELECT id FROM people ORDER BY id LIMIT 2 OFFSET 1")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0])

        // DESC でも同じこと。
        let _, desc =
            ok (db.Execute "SELECT id FROM people ORDER BY id DESC LIMIT 2 OFFSET 1")

        Assert.Equal<int>(2, desc.Rows.Length)
        Assert.Equal<int64>(2L, asInt desc.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt desc.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 索引の等価検索は一致する行をすべて返す`` () =
    withDb "reg-index" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"

        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,20),(4,30)")

        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        let _, res = ok (db3.Execute "SELECT id FROM t WHERE g = 20 ORDER BY id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: 索引列を更新すると索引も追従する`` () =
    withDb "reg-index-upd" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20)")
        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        // 索引キーが変わる更新（古いキーの削除 + 新しいキーの追加）
        let db4, upd = ok (db3.Execute "UPDATE t SET g = 99 WHERE id = 1")
        Assert.Equal<int>(1, upd.RowsAffected)

        let _, hit = ok (db4.Execute "SELECT id FROM t WHERE g = 99")
        Assert.Equal<int>(1, hit.Rows.Length)
        Assert.Equal<int64>(1L, asInt hit.Rows.[0].[0])

        // 古い索引エントリからは復活しない
        let _, miss = ok (db4.Execute "SELECT id FROM t WHERE g = 10")
        Assert.Equal<int>(0, miss.Rows.Length))

[<Fact>]
let ``SqlReference: 負数の主キー等値検索`` () =
    withDb "reg-negpk" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (-5, 50), (5, 50)")
        // 主キー検索（Unary Neg のリテラルから PkLookup が選ばれる）
        let _, res = ok (db2.Execute "SELECT id, v FROM t WHERE id = -5")

        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(-5L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(50L, asInt res.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: Unicode テーブル名でも再オープン後に読める`` () =
    withDir "unicode-reopen" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE 住所 (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO 住所 VALUES (1, '東京')"
        ok (db2.Close())

        let db3 = ok (Database.OpenWith dir)
        let result = query db3 "SELECT * FROM 住所"
        Assert.Equal<int>(1, result.Rows.Length)
        Assert.Equal<string>("東京", asText result.Rows.[0].[1])
        ok (db3.Close()))

[<Fact>]
let ``SqlReference: CREATE INDEX IF NOT EXISTS は既存なら成功する`` () =
    withDb "p1-index-if" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_v ON t (v)"
        let db3 = exec db2 "CREATE INDEX IF NOT EXISTS ix_v ON t (v)"

        expectError "IF NOT EXISTS なしの重複はエラー" (db3.Execute "CREATE INDEX ix_v ON t (v)")

        let db4 = exec db3 "INSERT INTO t VALUES (1, 10), (2, 20)"
        let _, found = ok (db4.Execute "SELECT id FROM t WHERE v = 20")
        Assert.Equal<int>(1, found.Rows.Length))

[<Fact>]
let ``SqlReference: LEFT OUTER JOIN の OUTER 綴り`` () =
    withDb "p1-left-outer" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE u (id INTEGER PRIMARY KEY, w TEXT)"
        let db3 = exec db2 "INSERT INTO t VALUES (1), (2)"
        let db4 = exec db3 "INSERT INTO u VALUES (1, 'x')"

        let _, rows =
            ok (db4.Execute "SELECT t.id, u.w FROM t LEFT OUTER JOIN u ON t.id = u.id ORDER BY t.id")

        Assert.Equal<int>(2, rows.Rows.Length)
        Assert.Equal<string>("x", asText rows.Rows.[0].[1])
        Assert.True(SqlValue.isNull rows.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: BEGIN/START/COMMIT WORK/END`` () =
    withDb "p1-txn-spellings" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "BEGIN TRANSACTION; INSERT INTO t VALUES (1); COMMIT WORK"
        let db3 = exec db2 "START TRANSACTION; INSERT INTO t VALUES (2); END"
        let _, count = ok (db3.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 集約 DISTINCT と IS TRUE/FALSE`` () =
    withDb "p1-distinct-truth" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER, b BOOLEAN)"

        let db2 =
            exec db1 "INSERT INTO t VALUES (1, 10, TRUE), (2, 10, FALSE), (3, 20, NULL)"

        let _, agg =
            ok (db2.Execute "SELECT SUM(DISTINCT v), AVG(DISTINCT v), MIN(DISTINCT v), MAX(DISTINCT v) FROM t")

        Assert.Equal(SqlValue.Decimal 30M, agg.Rows.[0].[0])
        Assert.Equal<float>(15.0, asReal agg.Rows.[0].[1])
        Assert.Equal<int64>(10L, asInt agg.Rows.[0].[2])
        Assert.Equal<int64>(20L, asInt agg.Rows.[0].[3])

        let _, isTrue = ok (db2.Execute "SELECT id FROM t WHERE b IS TRUE")
        Assert.Equal<int>(1, isTrue.Rows.Length)

        let _, isFalse = ok (db2.Execute "SELECT id FROM t WHERE b IS FALSE")
        Assert.Equal<int>(1, isFalse.Rows.Length)

        let _, isUnknown = ok (db2.Execute "SELECT id FROM t WHERE b IS UNKNOWN")
        Assert.Equal<int>(1, isUnknown.Rows.Length)

        let _, notUnknown =
            ok (db2.Execute "SELECT id FROM t WHERE b IS NOT UNKNOWN ORDER BY id")

        Assert.Equal<int>(2, notUnknown.Rows.Length)

        let _, notTrue = ok (db2.Execute "SELECT id FROM t WHERE b IS NOT TRUE ORDER BY id")
        Assert.Equal<int>(2, notTrue.Rows.Length))

[<Fact>]
let ``SqlReference: LIKE ESCAPE と CAST`` () =
    withDb "p1-like-cast" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, s TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a%b'), (2, 'axb'), (3, 'a_b')"

        let _, likePct = ok (db2.Execute "SELECT id FROM t WHERE s LIKE 'a!%b' ESCAPE '!'")
        Assert.Equal<int64>(1L, asInt likePct.Rows.[0].[0])

        let _, likeUnderscore =
            ok (db2.Execute "SELECT id FROM t WHERE s LIKE 'a!_b' ESCAPE '!'")

        Assert.Equal<int64>(3L, asInt likeUnderscore.Rows.[0].[0])

        let _, casted =
            ok (db2.Execute "SELECT CAST('42' AS INTEGER), CAST(42 AS TEXT) FROM t LIMIT 1")

        Assert.Equal<int64>(42L, asInt casted.Rows.[0].[0])
        Assert.Equal<string>("42", asText casted.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: INSERT DEFAULT VALUES と DEFAULT`` () =
    withDb "p1-default-values" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY DEFAULT 1, v INTEGER DEFAULT 5, s TEXT)"

        let db2 = exec db1 "INSERT INTO t DEFAULT VALUES"
        let db3 = exec db2 "INSERT INTO t (id, v) VALUES (2, DEFAULT)"
        let _, rows = ok (db3.Execute "SELECT id, v, s FROM t ORDER BY id")

        Assert.Equal<int>(2, rows.Rows.Length)
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(5L, asInt rows.Rows.[0].[1])
        Assert.True(SqlValue.isNull rows.Rows.[0].[2])
        Assert.Equal<int64>(2L, asInt rows.Rows.[1].[0])
        Assert.Equal<int64>(5L, asInt rows.Rows.[1].[1]))

[<Fact>]
let ``SqlReference: ORDER BY の式と序数`` () =
    withDb "p1-order-expr" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 30), (2, 10), (3, 20)"

        let _, expr = ok (db2.Execute "SELECT id FROM t ORDER BY v + 1")
        Assert.Equal<int64>(2L, asInt expr.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt expr.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt expr.Rows.[2].[0])

        let _, ordinal = ok (db2.Execute "SELECT id, v FROM t ORDER BY 2 DESC")
        Assert.Equal<int64>(1L, asInt ordinal.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt ordinal.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt ordinal.Rows.[2].[0])

        let _, alias =
            ok (db2.Execute "SELECT id, v AS k FROM t ORDER BY k + 1 DESC LIMIT 1")

        Assert.Equal<int64>(1L, asInt alias.Rows.[0].[0])

        // `SELECT *` の序数は展開後の列位置
        let _, starFirst = ok (db2.Execute "SELECT * FROM t ORDER BY 1 DESC")
        Assert.Equal<int64>(3L, asInt starFirst.Rows.[0].[0])

        let _, starSecond = ok (db2.Execute "SELECT * FROM t ORDER BY 2 ASC")
        Assert.Equal<int64>(2L, asInt starSecond.Rows.[0].[0]))

// ---------------------------------------------------------------------------
// sql-reference.md の記載を検証（§1 / §2 / §3 / §4 / §5 / §7 / §8 / §9 / §10 /
// §11 / §12 / §13 / §14）
// ---------------------------------------------------------------------------

/// SQL を実行し、エラーメッセージに断片が含まれることを確かめる。
/// §13「よく見るメッセージ（実測）」は文言そのものが仕様なので文字で照合する。
let private expectMessage (fragment: string) (sql: string) (db: Database) =
    match db.Execute sql with
    | Ok _ -> Assert.Fail(sprintf "エラーにならないはず: %s" sql)
    | Error e -> Assert.Contains(fragment, DbError.toDisplayString e)

[<Fact>]
let ``SqlReference: SELECT が 1 つだけなら結果を返し、複数なら合計を返す`` () =
    // §1: SELECT が 1 つだけならその結果を返し、複数（または 0 個）なら
    // RowsAffected の合計だけを返す（Columns / Rows は空）。
    withDb "multi-statement" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        // SELECT 1 個 → 結果がそのまま返る
        let _, one = ok (db2.Execute "SELECT id FROM t ORDER BY id")
        Assert.Equal<string array>([| "id" |], one.Columns)
        Assert.Equal<int>(2, one.Rows.Length)

        // SELECT 2 個 → RowsAffected の合計（SELECT の RowsAffected はその行数）
        let _, two = ok (db2.Execute "SELECT id FROM t; SELECT v FROM t")
        Assert.Empty two.Columns
        Assert.Empty two.Rows
        Assert.Equal<int>(4, two.RowsAffected)

        // DML + SELECT → SELECT の結果
        let _, mixed =
            ok (db2.Execute "INSERT INTO t VALUES (3, 'c'); SELECT id FROM t ORDER BY id")

        Assert.Equal<string array>([| "id" |], mixed.Columns)
        Assert.Equal<int>(3, mixed.Rows.Length)
        Assert.Equal<int>(3, mixed.RowsAffected)

        // SELECT が無い → RowsAffected の合計
        let _, none =
            ok (db2.Execute "INSERT INTO t VALUES (4, 'd'); INSERT INTO t VALUES (5, 'e')")

        Assert.Empty none.Columns
        Assert.Equal<int>(2, none.RowsAffected))

[<Fact>]
let ``SqlReference: FROM を省略した SELECT は定数 1 行を返す`` () =
    // §7.1: FROM は省略できる。入力は 1 行 0 列で、式だけを評価する。
    withDb "select-no-from" (fun db0 ->
        let _, one = ok (db0.Execute "SELECT 1")
        Assert.Equal<string array>([| "1" |], one.Columns)
        Assert.Equal<int>(1, one.Rows.Length)
        Assert.Equal<int64>(1L, asInt one.Rows.[0].[0])

        // 別名はヘッダに使われる
        let _, expr = ok (db0.Execute "SELECT 1 + 2 AS three, 'x' AS s")
        Assert.Equal<string array>([| "three"; "s" |], expr.Columns)
        Assert.Equal<int64>(3L, asInt expr.Rows.[0].[0])

        match expr.Rows.[0].[1] with
        | SqlValue.Text s -> Assert.Equal("x", s)
        | v -> Assert.Fail(sprintf "TEXT ではありません: %A" v)

        // WHERE は 1 行に対して効く（偽なら 0 行）
        let _, filtered = ok (db0.Execute "SELECT 1 WHERE 1 = 2")
        Assert.Empty filtered.Rows

        // DISTINCT / LIMIT も効く
        let _, distinct = ok (db0.Execute "SELECT DISTINCT 1")
        Assert.Equal<int>(1, distinct.Rows.Length)

        let _, limited = ok (db0.Execute "SELECT 1 LIMIT 0")
        Assert.Empty limited.Rows

        // 集約は 1 行 0 列の入力を 1 行にまとめる
        let _, counted = ok (db0.Execute "SELECT COUNT(*)")
        Assert.Equal<int64>(1L, asInt counted.Rows.[0].[0])

        // サブクエリでも FROM を省略できる
        let _, sub = ok (db0.Execute "SELECT EXISTS (SELECT 1), (SELECT 2)")
        Assert.Equal<int>(1, sub.Rows.Length)

        match sub.Rows.[0].[0], sub.Rows.[0].[1] with
        | SqlValue.Bool b, SqlValue.Int n ->
            Assert.True b
            Assert.Equal(2L, n)
        | a, b -> Assert.Fail(sprintf "想定外の値: %A / %A" a b)

        // 展開する列が無いので * / t.* はエラー
        expectErrorKind ErrorKind.SyntaxError "FROM なしの *" (db0.Execute "SELECT *")
        expectErrorKind ErrorKind.NameError "FROM なしの t.*" (db0.Execute "SELECT t.*")
        expectErrorKind ErrorKind.SyntaxError "FROM なしの JOIN" (db0.Execute "SELECT 1 JOIN t ON 1 = 1"))

[<Fact>]
let ``SqlReference: 数字から始まる識別子は受け付けない`` () =
    // §2: 数字から始められない。`_` 始まりは使える。
    withDb "ident-start" (fun db0 ->
        expectErrorKind ErrorKind.SyntaxError "数字始まりのテーブル名" (db0.Execute "CREATE TABLE 1a (id INTEGER PRIMARY KEY)")

        let db1 = exec db0 "CREATE TABLE _u (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO _u VALUES (1)"
        let _, rows = ok (db2.Execute "SELECT id FROM _u")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 型の別綴りはすべて受理される`` () =
    // §3: BOOLEAN / BOOL, INTEGER / INT / BIGINT, REAL / DOUBLE, DECIMAL / NUMERIC,
    // TEXT / VARCHAR / CHAR, DATE, TIMESTAMP / DATETIME, UUID。
    withDb "type-aliases" (fun db0 ->
        let db1 =
            exec
                db0
                "CREATE TABLE ty (id INTEGER PRIMARY KEY, a BOOL, b INT, c BIGINT, d DOUBLE, e NUMERIC, f CHAR(3), g DATETIME, h UUID, i BLOB, j DATE)"

        Assert.Equal<string list>([ "ty" ], db1.TableNames())

        // 文字列で渡した値は列の型へ暗黙変換される（§3）
        let db2 =
            exec
                db1
                "INSERT INTO ty (id, j, g, h) VALUES (1, '2024-01-15', '2024-01-15T10:30:00Z', '0f8fad5b-d9cb-469f-a165-70867728950e')"

        let _, rows = ok (db2.Execute "SELECT j, g, h FROM ty WHERE id = 1")
        Assert.Equal<DataType>(DataType.Date, SqlValue.dataType rows.Rows.[0].[0])
        Assert.Equal<DataType>(DataType.Timestamp, SqlValue.dataType rows.Rows.[0].[1])
        Assert.Equal<DataType>(DataType.Uuid, SqlValue.dataType rows.Rows.[0].[2]))

[<Fact>]
let ``SqlReference: CHAR(1) は値を切り詰めない`` () =
    // §3: 型引数は読み取られるだけで、切り詰めも検証も行わない。
    withDb "char-no-truncate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, s CHAR(1))"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'abcdefghij')"
        let _, rows = ok (db2.Execute "SELECT s FROM t")
        Assert.Equal<string>("abcdefghij", asText rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: INSERT の値式は列参照を含められない`` () =
    // §4: 値は定数式のみ。列参照は名前解決でエラーになる。
    withDb "insert-column-ref" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT, n INTEGER)"
        let db2, ins = ok (db1.Execute "INSERT INTO t (id, v, n) VALUES (1, 'a', 2 + 3)")
        Assert.Equal<int>(1, ins.RowsAffected)

        expectErrorKind ErrorKind.NameError "VALUES の列参照" (db2.Execute "INSERT INTO t (id, v, n) VALUES (2, v, n)")

        // 失敗した 1 文は残らない
        let _, rows = ok (db2.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: SELECT 項目の後置演算子`` () =
    // §5: SELECT 項目は式として解析されるので、裸の識別子・関数の後に演算子を書ける。
    withDb "select-postfix" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT, n INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a', 1)"

        let _, cat = ok (db2.Execute "SELECT v || 'x' AS c FROM t")
        Assert.Equal<string array>([| "c" |], cat.Columns)
        Assert.Equal<string>("ax", asText cat.Rows.[0].[0])

        let _, sum = ok (db2.Execute "SELECT SUM(n) + 0.5 AS s FROM t")
        Assert.Equal(1.5, asReal sum.Rows.[0].[0], 10))

[<Fact>]
let ``SqlReference: 式の列名は丸括弧つきの表示文字列`` () =
    // §7.3: `SELECT (1 + 2)` の列名は `(1 + 2)`（常に丸括弧つき）。
    withDb "expr-column-name" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"
        let _, r = ok (db2.Execute "SELECT (1 + 2), v || 'x' FROM t")
        Assert.Equal<string array>([| "(1 + 2)"; "(v || 'x')" |], r.Columns))

[<Fact>]
let ``SqlReference: ORDER BY に集約式は書けない`` () =
    // §7.3: 集約式（ORDER BY COUNT(*) など）は未対応。
    withDb "order-by-agg" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"
        expectErrorKind ErrorKind.NameError "ORDER BY COUNT(*)" (db2.Execute "SELECT id FROM t ORDER BY COUNT(*)"))

[<Fact>]
let ``SqlReference: ORDER BY の序数は範囲外だとエラー`` () =
    // §7.3: 序数は 1 始まり。範囲外は黙って切らずに SyntaxError。
    withDb "order-by-ordinal" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        let _, ok2 = ok (db2.Execute "SELECT id, v FROM t ORDER BY 2 DESC")
        Assert.Equal<int64>(2L, asInt ok2.Rows.[0].[0])

        expectErrorKind ErrorKind.SyntaxError "序数 0" (db2.Execute "SELECT id, v FROM t ORDER BY 0")
        expectErrorKind ErrorKind.SyntaxError "序数 99" (db2.Execute "SELECT id, v FROM t ORDER BY 99"))

[<Fact>]
let ``SqlReference: 主キーの重複 INSERT はエラー（upsert しない）`` () =
    // §8: 主キー重複はエラーになる（複数列の主キーも同じ）。
    withDb "pk-duplicate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"

        expectErrorKind ErrorKind.ConstraintViolation "主キーの重複 INSERT" (db2.Execute "INSERT INTO t VALUES (1, 'b')")

        // 覆っていないこと（upsert しない）
        let _, rows = ok (db2.Execute "SELECT v FROM t WHERE id = 1")
        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<string>("a", asText rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: UPDATE は主キー列を変えられない`` () =
    // §9: 主キー列を SET するとエラーになり、何も更新されない。
    withDb "pk-update" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b')"

        expectErrorKind ErrorKind.ConstraintViolation "主キー列の UPDATE" (db2.Execute "UPDATE t SET id = 100 WHERE id = 1")

        let _, rows = ok (db2.Execute "SELECT id FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 1L; 2L |], rows.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: UPDATE の式は更新前の行に対して評価する`` () =
    // §9: 式は更新前の行に対して評価される。
    withDb "update-pre-row" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10, 'x')"

        let db3, upd =
            ok (db2.Execute "UPDATE t SET n = n + 1, v = CAST(n AS TEXT) WHERE id = 1")

        Assert.Equal<int>(1, upd.RowsAffected)

        let _, rows = ok (db3.Execute "SELECT n, v FROM t WHERE id = 1")
        // v は「更新前」の n = 10 を見る
        Assert.Equal<int64>(11L, asInt rows.Rows.[0].[0])
        Assert.Equal<string>("10", asText rows.Rows.[0].[1]))

[<Fact>]
let ``SqlReference: DELETE は WHERE を省略すると全行を消す`` () =
    // §10
    withDb "delete-all" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')"
        let db3, del = ok (db2.Execute "DELETE FROM t")
        Assert.Equal<int>(3, del.RowsAffected)

        let _, rows = ok (db3.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: 主キーの無い CREATE TABLE はエラー`` () =
    // §11.1: 主キーは省略できない。
    withDb "pk-required" (fun db0 ->
        expectErrorKind ErrorKind.ConstraintViolation "主キー無しの CREATE TABLE" (db0.Execute "CREATE TABLE t (a INTEGER)")
        Assert.Empty(db0.TableNames()))

[<Fact>]
let ``SqlReference: PRIMARY KEY の定義は 1 テーブルに 1 つだけ`` () =
    // §11.1: 列レベルとテーブルレベルの併用、複数列への列レベル、
    // 複数のテーブルレベルはいずれも ConstraintViolation。
    withDb "pk-once" (fun db0 ->
        expectErrorKind
            ErrorKind.ConstraintViolation
            "列レベルとテーブルレベルの併用"
            (db0.Execute "CREATE TABLE k1 (id INTEGER PRIMARY KEY, n TEXT, PRIMARY KEY (id))")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "複数列への列レベル PRIMARY KEY"
            (db0.Execute "CREATE TABLE k2 (a INTEGER PRIMARY KEY, b TEXT PRIMARY KEY)")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "複数のテーブルレベル PRIMARY KEY"
            (db0.Execute "CREATE TABLE k3 (a INTEGER, b TEXT, PRIMARY KEY (a), PRIMARY KEY (b))")

        // 複数列のテーブルレベルは 1 定義なら通る
        let db1 = exec db0 "CREATE TABLE k4 (a INTEGER, b TEXT, PRIMARY KEY (a, b))"
        Assert.Equal<string list>([ "k4" ], db1.TableNames()))

[<Fact>]
let ``SqlReference: 型を省略した列定義は TEXT になる`` () =
    // §11.1: 型を書かずに制約だけを書く構文も受け付ける（型は TEXT）。
    withDb "column-type-omitted" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, name NOT NULL)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'zz')"
        // TEXT 列なので数値も文字列表現へ変換される
        let db3 = exec db2 "INSERT INTO t VALUES (2, 123)"

        let _, rows = ok (db3.Execute "SELECT name FROM t ORDER BY id")
        Assert.Equal<string>("zz", asText rows.Rows.[0].[0])
        Assert.Equal<string>("123", asText rows.Rows.[1].[0]))

[<Fact>]
let ``SqlReference: CREATE TABLE IF EXISTS は既存なら何もしない`` () =
    // §11.1: `IF EXISTS` と `IF NOT EXISTS` の両方の綴りを受け付ける。
    withDb "create-table-if-exists" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"
        let db3 = exec db2 "CREATE TABLE IF EXISTS t (id INTEGER PRIMARY KEY, v TEXT)"
        Assert.Equal<string list>([ "t" ], db3.TableNames())

        let _, rows = ok (db3.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: テーブルレベル UNIQUE の列を検証する`` () =
    // §11.1: 存在しない列・重複列は NameError で拒否する。
    withDb "table-unique-columns" (fun db0 ->
        expectErrorKind
            ErrorKind.NameError
            "UNIQUE の存在しない列"
            (db0.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, a TEXT, UNIQUE (nope))")

        expectErrorKind
            ErrorKind.NameError
            "UNIQUE の重複列"
            (db0.Execute "CREATE TABLE u (id INTEGER PRIMARY KEY, a TEXT, UNIQUE (a, a))")

        let db1 =
            exec db0 "CREATE TABLE v (id INTEGER PRIMARY KEY, a TEXT, b TEXT, UNIQUE (a, b))"

        Assert.Equal<string list>([ "v" ], db1.TableNames()))

[<Fact>]
let ``SqlReference: SQL の BEGIN と ROLLBACK で書き込みを取り消す`` () =
    // §12 の例どおり。BEGIN / COMMIT / ROLLBACK は暗黙の transaction を制御する。
    withDb "sql-txn" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"

        let db3 = exec db2 "BEGIN; INSERT INTO t VALUES (2, 'b'); ROLLBACK"
        let _, rolled = ok (db3.Execute "SELECT id FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 1L |], rolled.Rows |> Array.map (fun r -> asInt r.[0]))

        let db4 = exec db3 "BEGIN; INSERT INTO t VALUES (3, 'c'); COMMIT"
        let _, committed = ok (db4.Execute "SELECT id FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 1L; 3L |], committed.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``SqlReference: サブクエリ（スカラー・IN・EXISTS）`` () =
    withDb "subquery" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20),(3,30)"

        // スカラーサブクエリ（SELECT 項目 / WHERE）
        let _, rows =
            ok (
                db2.Execute
                    "SELECT id, (SELECT MAX(v) FROM t) AS mx FROM t WHERE v > (SELECT AVG(v) FROM t) ORDER BY id"
            )

        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(3L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(30L, asInt rows.Rows.[0].[1])

        // 0 行のスカラーサブクエリは NULL
        let _, none =
            ok (db2.Execute "SELECT id, (SELECT v FROM t WHERE id = 99) AS x FROM t WHERE id = 1")

        Assert.True(SqlValue.isNull none.Rows.[0].[1])

        // IN / NOT IN
        let _, inRows =
            ok (db2.Execute "SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE v >= 20) ORDER BY id")

        Assert.Equal<int>(2, inRows.Rows.Length)
        Assert.Equal<int64>(2L, asInt inRows.Rows.[0].[0])

        let _, emptyIn =
            ok (db2.Execute "SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE id = 99)")

        Assert.Equal<int>(0, emptyIn.Rows.Length)

        let _, emptyNotIn =
            ok (db2.Execute "SELECT id FROM t WHERE id NOT IN (SELECT id FROM t WHERE id = 99) ORDER BY id")

        Assert.Equal<int>(3, emptyNotIn.Rows.Length)

        // NULL を含む IN / NOT IN は UNKNOWN（どちらも 0 行）
        let _, nullIn = ok (db2.Execute "SELECT id FROM t WHERE id IN (SELECT NULL FROM t)")
        Assert.Equal<int>(0, nullIn.Rows.Length)

        let _, nullNotIn =
            ok (db2.Execute "SELECT id FROM t WHERE id NOT IN (SELECT NULL FROM t)")

        Assert.Equal<int>(0, nullNotIn.Rows.Length)

        // EXISTS / NOT EXISTS
        let _, exists =
            ok (db2.Execute "SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t WHERE v > 25) ORDER BY id")

        Assert.Equal<int>(3, exists.Rows.Length)

        let _, notExists =
            ok (db2.Execute "SELECT id FROM t WHERE NOT EXISTS (SELECT 1 FROM t WHERE v > 25)")

        Assert.Equal<int>(0, notExists.Rows.Length)

        let _, missing =
            ok (db2.Execute "SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t WHERE id = 99)")

        Assert.Equal<int>(0, missing.Rows.Length)

        let _, notMissing =
            ok (db2.Execute "SELECT id FROM t WHERE NOT EXISTS (SELECT 1 FROM t WHERE id = 99)")

        Assert.Equal<int>(3, notMissing.Rows.Length)

        // 入れ子のサブクエリ
        let _, nested =
            ok (
                db2.Execute
                    "SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE v > (SELECT MIN(v) FROM t)) ORDER BY id"
            )

        Assert.Equal<int>(2, nested.Rows.Length)
        Assert.Equal<int64>(2L, asInt nested.Rows.[0].[0])

        // HAVING / JOIN の ON でも使える
        let _, having =
            ok (db2.Execute "SELECT id FROM t GROUP BY id HAVING id > (SELECT MIN(id) FROM t) ORDER BY id")

        Assert.Equal<int>(2, having.Rows.Length)
        Assert.Equal<int64>(2L, asInt having.Rows.[0].[0])

        let _, joined =
            ok (
                db2.Execute
                    "SELECT a.id FROM t AS a JOIN t AS b ON a.id = b.id AND EXISTS (SELECT 1 FROM t WHERE v > 25) ORDER BY a.id"
            )

        Assert.Equal<int>(3, joined.Rows.Length)

        // DML 内のサブクエリ
        let db3 = exec db2 "UPDATE t SET v = v + (SELECT MIN(v) FROM t) WHERE id = 1"
        let _, updated = ok (db3.Execute "SELECT v FROM t WHERE id = 1")
        Assert.Equal<int64>(20L, asInt updated.Rows.[0].[0])

        let db4 = exec db3 "INSERT INTO t VALUES (4, (SELECT MAX(v) FROM t))"
        let _, inserted = ok (db4.Execute "SELECT v FROM t WHERE id = 4")
        Assert.Equal<int64>(30L, asInt inserted.Rows.[0].[0])

        let db5 = exec db4 "DELETE FROM t WHERE id IN (SELECT id FROM t WHERE v >= 30)"
        let _, count = ok (db5.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: サブクエリの未対応形とエラー`` () =
    withDb "subquery-errors" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"

        // スカラーサブクエリが 2 行以上
        expectErrorKind
            ErrorKind.ConstraintViolation
            "2 行以上のスカラーサブクエリ"
            (db2.Execute "SELECT id, (SELECT v FROM t) FROM t")

        // 1 列でないサブクエリ
        expectErrorKind
            ErrorKind.TypeError
            "2 列のスカラーサブクエリ"
            (db2.Execute "SELECT id, (SELECT id, v FROM t WHERE id = 1) FROM t")

        expectErrorKind
            ErrorKind.TypeError
            "2 列の IN サブクエリ"
            (db2.Execute "SELECT id FROM t WHERE id IN (SELECT id, v FROM t)")

        // 相関サブクエリは未対応（外側の列は解決できない）
        expectErrorKind
            ErrorKind.NameError
            "相関サブクエリ"
            (db2.Execute "SELECT id FROM t AS o WHERE EXISTS (SELECT 1 FROM t WHERE t.id = o.id)")

        // FROM の派生テーブルは未対応
        expectErrorKind ErrorKind.SyntaxError "派生テーブル" (db2.Execute "SELECT id FROM (SELECT id FROM t) x"))

[<Fact>]
let ``SqlReference: 集約結果は DATE の型を保つ`` () =
    // 集約後の値を Literal に落とすと DATE / TIMESTAMP / UUID が TEXT になり、
    // 外側の DATE 列と比較できなくなる（サブクエリで顕在化した）。
    withDb "agg-date" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE d (id INTEGER PRIMARY KEY, dt DATE)"
        let db2 = exec db1 "INSERT INTO d VALUES (1,'2026-11-03'),(2,'2026-11-04')"

        let _, rows = ok (db2.Execute "SELECT id FROM d WHERE dt = (SELECT MAX(dt) FROM d)")

        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(2L, asInt rows.Rows.[0].[0])

        // GROUP BY のキーも型を保つ
        let _, grouped =
            ok (db2.Execute "SELECT dt, COUNT(*) FROM d GROUP BY dt ORDER BY dt")

        match grouped.Rows.[0].[0] with
        | SqlValue.Date _ -> ()
        | v -> Assert.Fail(sprintf "DATE ではありません: %A" v))

[<Fact>]
let ``SqlReference: 深いサブクエリの入れ子（上限内）は実行できる`` () =
    // `Plan.planSelect` / `resolveSelect` / `resolveExpr` / `resolveLeaf` は相互再帰する。
    // 深さは式の上限（MaxExpressionDepth = 200。1 段で複数単位を消費する）で頭打ちなので、
    // 上限内の入れ子はスタックを食わずに実行できることを固定しておく。
    withDb "subquery-depth" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10)"

        let nested depth =
            [ 1..depth ]
            |> List.fold (fun e _ -> sprintf "(SELECT %s FROM t WHERE id = 1)" e) "v"

        let _, rows =
            ok (db2.Execute(sprintf "SELECT %s AS x FROM t WHERE id = 1" (nested 90)))

        Assert.Equal<int>(1, rows.Rows.Length)
        Assert.Equal<int64>(10L, asInt rows.Rows.[0].[0])

        // 上限を超えると StackOverflow ではなく SyntaxError
        expectErrorKind ErrorKind.SyntaxError "深すぎるサブクエリ" (db2.Execute(sprintf "SELECT %s AS x FROM t" (nested 400))))

[<Fact>]
let ``SqlReference: 未対応の構文は明示的な SyntaxError`` () =
    // §14「構文が未対応のもの」。集合演算と RIGHT / FULL JOIN は ParserTests が担当。
    withDb "unsupported-syntax" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'a')"

        // サブクエリ（式の中）は §7.9 で対応。FROM の派生テーブルは未対応のまま。
        expectErrorKind ErrorKind.SyntaxError "FROM のサブクエリ" (db2.Execute "SELECT id FROM (SELECT id FROM t) x")

        expectErrorKind ErrorKind.SyntaxError "WITH 句" (db2.Execute "WITH c AS (SELECT id FROM t) SELECT id FROM c")

        expectErrorKind
            ErrorKind.SyntaxError
            "ウィンドウ関数"
            (db2.Execute "SELECT id, ROW_NUMBER() OVER (ORDER BY id) FROM t")

        // DDL / 実行計画
        expectErrorKind ErrorKind.SyntaxError "ALTER TABLE" (db2.Execute "ALTER TABLE t ADD COLUMN z INTEGER")
        expectErrorKind ErrorKind.SyntaxError "TRUNCATE" (db2.Execute "TRUNCATE TABLE t")
        expectErrorKind ErrorKind.SyntaxError "EXPLAIN" (db2.Execute "EXPLAIN SELECT * FROM t")

        // DML の未対応形
        expectErrorKind ErrorKind.SyntaxError "INSERT ... SELECT" (db2.Execute "INSERT INTO t SELECT 2, 'c'")
        expectErrorKind ErrorKind.SyntaxError "RETURNING" (db2.Execute "DELETE FROM t RETURNING id")

        // どれも文が実行されていないこと
        let _, rows = ok (db2.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))

[<Fact>]
let ``SqlReference: よく見るエラーメッセージは sql-reference §13 と一致する`` () =
    // §13 の一覧は「実測」した文言そのもの。種別（ErrorKind）だけでは文言の劣化を検出できない。
    withDb "error-messages" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, n TEXT NOT NULL, num INTEGER, active BOOLEAN)"

        let db2 = exec db1 "INSERT INTO t (id, n, num, active) VALUES (1, 'a', 10, TRUE)"

        expectMessage "JOIN には FROM が必要です" "SELECT 1 JOIN t ON 1 = 1" db2
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

        expectMessage "LENGTH 関数は 1 個の引数が必要ですが、2 個渡されました" "SELECT LENGTH(id, num) FROM t" db2

        expectMessage "未知のスカラー関数 'NOSUCHFN' です" "SELECT NOSUCHFN(id) FROM t" db2
        expectMessage "型 'BOOLEAN' と 'INTEGER' は比較できません" "SELECT active = 1 FROM t" db2
        expectMessage "ROUND は数値を受け付けますが" "SELECT ROUND(n) FROM t" db2

        // 失敗した INSERT は何も残っていない
        let _, rows = ok (db2.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0]))
