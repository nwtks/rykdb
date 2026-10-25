// ===========================================================================
// Database.fs のテスト: 公開 API・E2E・性能
//
// 「SQL 文字列 → 結果」の通し経路で Parser / Planner / Executor / Catalog の
// 連携バグと、クラッシュ・再オープン・性能までを検証する。
// ===========================================================================

module RykDb.DatabaseTests

open System.Diagnostics
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

/// ライブラリ API で DB を仕込む（E2E で共通）。
let private seedLibrary (db: Database) =
    db
    |> fun d -> exec d "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER)"
    |> fun d -> exec d "INSERT INTO people VALUES (1, 'alice', 30), (2, 'bob', 25), (3, 'carol', 35), (4, 'dave', 25)"
    |> fun d -> exec d "CREATE TABLE orders (oid INTEGER PRIMARY KEY, person_id INTEGER, amount INTEGER)"
    |> fun d -> exec d "INSERT INTO orders VALUES (1, 1, 100), (2, 1, 200), (3, 2, 50), (4, 3, 70), (5, 3, 20)"
    |> fun d -> exec d "CREATE INDEX idx_people_name ON people (name)"
    |> fun d -> exec d "CREATE INDEX idx_orders_pid ON orders (person_id)"

// ---------------------------------------------------------------------------
// トランザクション（Begin / Commit / Rollback）
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: BEGIN/COMMIT でまとめて反映される`` () =
    withDb "tx1" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = ok (db1.Begin())
        Assert.True db2.InTransaction

        let db3, _ = ok (db2.Execute "INSERT INTO t VALUES (1, 1)")
        let db4, _ = ok (db3.Execute "INSERT INTO t VALUES (2, 2)")
        let db5 = ok (db4.Commit())
        Assert.False db5.InTransaction

        let _db11, res = ok (db5.Query "SELECT id FROM t")
        Assert.Equal<int>(2, res.Rows.Length))

// クラッシュ耐性（未 commit の transaction は WAL 再生後も残らない）

[<Fact>]
let ``E2E: commit してない transaction は残らない`` () =
    withDir "crash2" (fun dir ->
        let db = seedLibrary (ok (Database.openWith dir))

        // 明示 transaction を開いて write してから捨てる。
        let tx = ok (db.Begin())
        let tx2 = exec tx "INSERT INTO people VALUES (99, 'ghost', 1)"
        Assert.True tx2.InTransaction

        // `Close` しないので、幽霊行は WAL にしか無い。
        let reopened = ok (Database.openWith dir)
        let count = query reopened "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt count.Rows.[0].[0])

        ok (reopened.Close()))

// MVCC 分離（Begin で固定した snapshot）

[<Fact>]
let ``E2E: 別ハンドルの書き込みは既存 transaction に見えない`` () =
    withDir "mvcc" (fun dir ->
        let db = seedLibrary (ok (Database.openWith dir))

        // 読み取り専用 transaction を開く（`Active` だけ立てる）。
        let reader = ok (db.Begin())
        Assert.True reader.InTransaction

        // 別のハンドルから書き込む。
        let writer = exec db "INSERT INTO people VALUES (5, 'erin', 41)"
        // reader は commit していないので、新しい行は見えない。
        let before = query reader "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt before.Rows.[0].[0])

        // 書き込んだ側からは見える。
        let after = query writer "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(5L, asInt after.Rows.[0].[0])

        ok (writer.Close()))

[<Fact>]
let ``Database: ROLLBACK で元に戻る`` () =
    withDb "tx2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 1)")

        let db3 = ok (db2.Begin())
        let db4, _ = ok (db3.Execute "INSERT INTO t VALUES (2, 2)")
        let db5 = ok (db4.Rollback())
        Assert.False db5.InTransaction

        let _db12, res = ok (db5.Query "SELECT id FROM t")
        Assert.Equal<int>(1, res.Rows.Length))

[<Fact>]
let ``Database: 二重 Begin と空の Commit / Rollback`` () =
    withDb "tx3" (fun db0 ->
        // 二重 Begin は進行中の transaction を維持する
        let db1 = ok (db0.Begin())
        let db2 = ok (db1.Begin())
        Assert.True db2.InTransaction

        // DML なしの Commit は Active の transaction をそのまま流す
        let db3 = ok (db2.Commit())
        Assert.False db3.InTransaction

        // transaction が無い状態の Commit はエラー
        expectError "transaction が無い Commit はエラー" (db3.Commit())
        // transaction が無い状態の Rollback はエラー（Commit と対称）
        expectError "transaction が無い Rollback はエラー" (db3.Rollback()))

[<Fact>]
let ``Database: transaction 中の文エラーはそのまま返る`` () =
    withDb "tx4" (fun db0 ->
        let db1 = ok (db0.Begin())
        expectError "BEGIN 中の存在しないテーブル" (db1.Execute "SELECT * FROM nope"))

[<Fact>]
let ``Database: 明示 transaction 中の DDL はエラーになる`` () =
    withDb "tx-ddl" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let tx = ok (db1.Begin())

        // DDL は自動 commit されて ROLLBACK で戻らないので、明示的に拒否する
        expectError "BEGIN 中の CREATE TABLE" (tx.Execute "CREATE TABLE u (id INTEGER PRIMARY KEY)")
        expectError "BEGIN 中の CREATE INDEX" (tx.Execute "CREATE INDEX ix ON t (v)")
        expectError "BEGIN 中の DROP TABLE" (tx.Execute "DROP TABLE t")

        // transaction を閉じれば DDL できる
        let committed = ok (tx.Commit())
        let db2 = exec committed "CREATE TABLE u (id INTEGER PRIMARY KEY)"
        Assert.Contains("u", db2.TableNames()))

// ---------------------------------------------------------------------------
// 静かな誤答を防ぐ（識別子の大小文字・集計・HAVING・LIMIT・DEFAULT）
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: 索引列と主キーの大文字小文字は区別しない`` () =
    withDb "case-ident" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE c (id INTEGER PRIMARY KEY, v TEXT)"
        let db2 = exec db1 "INSERT INTO c VALUES (1, 'a'), (2, 'b')"
        let db3 = exec db2 "CREATE INDEX ci ON c (V)"

        // 索引キーが正しく作られ、索引経由でも行が見つかる
        let _, byIndex = ok (db3.Query "SELECT id FROM c WHERE v = 'b'")
        Assert.Equal<int>(1, byIndex.Rows.Length)
        Assert.Equal<int64>(2L, asInt byIndex.Rows.[0].[0])

        // PRIMARY KEY (ID) の表記も通る
        let db4 = exec db3 "CREATE TABLE p (id INTEGER, v TEXT, PRIMARY KEY (ID))"
        let _, inserted = ok (db4.Execute "INSERT INTO p VALUES (1, 'x')")
        Assert.Equal(1, inserted.RowsAffected))

[<Fact>]
let ``Database: SUM / AVG は型が混在しても値を落とさない`` () =
    withDb "sum-mixed" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE s (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO s VALUES (1), (2), (3)"

        // 1 + 1.5 + 1.5 = 4.0（INT と REAL の混在）
        let _, sum =
            ok (db2.Query "SELECT SUM(CASE WHEN id = 1 THEN 1 ELSE 1.5 END) FROM s")

        Assert.Equal(4.0, asReal sum.Rows.[0].[0], 10)

        let _, avg =
            ok (db2.Query "SELECT AVG(CASE WHEN id = 1 THEN 1 ELSE 1.5 END) FROM s")

        Assert.Equal(4.0 / 3.0, asReal avg.Rows.[0].[0], 10))

[<Fact>]
let ``Database: 集約なしの HAVING はエラー`` () =
    withDb "having-noagg" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"

        expectErrorKind
            ErrorKind.SyntaxError
            "HAVING には GROUP BY か集約が必要"
            (db2.Query "SELECT id FROM t HAVING id = 999"))

[<Fact>]
let ``Database: GROUP BY と * の併用はエラー`` () =
    withDb "group-star" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10), (2, 10)"
        expectErrorKind ErrorKind.SyntaxError "SELECT * は GROUP BY と併用できない" (db2.Query "SELECT * FROM t GROUP BY v"))

[<Fact>]
let ``Database: テーブル名と同じ非修飾名は列として解決しない`` () =
    withDb "resolve-qual" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE q (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO q VALUES (1)"
        expectErrorKind ErrorKind.NameError "テーブル名を列として解決しない" (db2.Query "SELECT q FROM q"))

[<Fact>]
let ``Database: t.* の大文字小文字は区別しない`` () =
    withDb "star-case" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE st (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO st VALUES (1)"
        let _, r = ok (db2.Query "SELECT ST.* FROM st")
        Assert.Equal<string list>([ "st.id" ], r.Columns |> Array.toList))

[<Fact>]
let ``Database: LIMIT の非リテラルと負数はエラー`` () =
    withDb "limit-expr" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2), (3)"
        expectErrorKind ErrorKind.SyntaxError "LIMIT 1+0 はエラー" (db2.Query "SELECT id FROM t LIMIT 1+0")
        expectErrorKind ErrorKind.SyntaxError "LIMIT -1 はエラー" (db2.Query "SELECT id FROM t LIMIT -1")
        expectErrorKind ErrorKind.SyntaxError "OFFSET 1+0 はエラー" (db2.Query "SELECT id FROM t LIMIT 1 OFFSET 1+0")

        // LIMIT 0 は 0 行（従来どおり）
        let _, none = ok (db2.Query "SELECT id FROM t LIMIT 0")
        Assert.Empty none.Rows)

[<Fact>]
let ``Database: DEFAULT の評価エラーは NULL にしない`` () =
    withDb "default-error" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER DEFAULT (1 / 0))"

        expectError "壊れた DEFAULT はエラーになる" (db1.Execute "INSERT INTO t (id) VALUES (1)"))

[<Fact>]
let ``Database: Query は SELECT 専用`` () =
    withDb "query-only" (fun db0 ->
        expectErrorKind ErrorKind.SyntaxError "Query で CREATE" (db0.Query "CREATE TABLE t (id INTEGER PRIMARY KEY)")
        expectErrorKind ErrorKind.SyntaxError "Query で INSERT" (db0.Query "INSERT INTO t VALUES (1)"))

[<Fact>]
let ``Database: 複合索引の全列等値で行を引ける`` () =
    withDb "comp-index-eq" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 2, 10), (1, 3, 11), (2, 2, 12)"
        let db3 = exec db2 "CREATE INDEX ix_ab ON t (a, b)"

        // 索引の backfill 後、全列等値で 1 行だけ返る
        let _, r = ok (db3.Query "SELECT id FROM t WHERE a = 1 AND b = 2")
        Assert.Equal<int>(1, r.Rows.Length)
        Assert.Equal<int64>(10L, asInt r.Rows.[0].[0])

        // 前置 1列だけでも IndexLookup（結果は同じ）
        let _, prefix = ok (db3.Query "SELECT id FROM t WHERE a = 1")
        Assert.Equal<int>(2, prefix.Rows.Length))

[<Fact>]
let ``Database: LIMIT 1 は後続の壊れた SSTable を開かない`` () =
    withDir "limit-pushdown" (fun dir ->
        // 2回に分けて Close し、テーブル t の SSTable を 2 個作る。
        let db0 = ok (Database.openWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"
        ok (db2.Close())

        let db3 = ok (Database.openWith dir)
        let db4 = exec db3 "INSERT INTO t VALUES (3), (4)"
        ok (db4.Close())

        // openDatabase は MaxSeqNo の復元で全 SSTable を読むため、open 後に壊す。
        let db5 = ok (Database.openWith dir)

        // テーブル t の最新 SSTable（キー 3,4）を壊す。
        let tableDir = System.IO.Path.Combine(dir, Keyspace.toDirName (Keyspace.table "t"))

        let newest =
            System.IO.Directory.GetFiles(tableDir, "*.sst")
            |> Array.maxBy (fun path -> System.Int64.Parse(System.IO.Path.GetFileNameWithoutExtension path))

        let bytes = System.IO.File.ReadAllBytes newest
        bytes.[0] <- bytes.[0] ^^^ 0xFFuy
        System.IO.File.WriteAllBytes(newest, bytes)
        System.IO.File.SetLastWriteTimeUtc(newest, System.DateTime.UtcNow.AddSeconds 1.0)

        // LIMIT 1 は先頭 1行だけなので、キー 3 以降の壊れた SSTable を開かない。
        let _, one = ok (db5.Query "SELECT id FROM t LIMIT 1")
        Assert.Equal<int>(1, one.Rows.Length)
        Assert.Equal<int64>(1L, asInt one.Rows.[0].[0])

        // 全行が必要なら壊れた SSTable を開くので CorruptionError。
        expectErrorKind ErrorKind.CorruptionError "LIMIT 10 は壊れた SSTable を開く" (db5.Query "SELECT id FROM t LIMIT 10")

        ok (db5.Close()))

[<Fact>]
let ``Database: VARCHAR(32) は再オープン後も使える`` () =
    withDir "varchar-reopen" (fun dir ->
        let db0 = ok (Database.openWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v VARCHAR(32))"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 'abc')"
        ok (db2.Close())

        // 型引数はカタログに保存されないが、列型（TEXT）と値はそのまま使える
        let db3 = ok (Database.openWith dir)
        let _, r = ok (db3.Query "SELECT v FROM t")
        Assert.Equal<string>("abc", asText r.Rows.[0].[0])
        ok (db3.Close()))

[<Fact>]
let ``Database: SQL の BEGIN / COMMIT / ROLLBACK は受理される（文としては無効）`` () =
    // docs/sql-reference.md の既知の制限どおり、文としての txn 制御は何もしない。
    withDb "tx5" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "BEGIN"
        let db3 = exec db2 "INSERT INTO t VALUES (1)"
        let db4 = exec db3 "COMMIT"
        let db5, res = ok (db4.Query "SELECT id FROM t")
        Assert.Equal<int>(1, res.Rows.Length)
        // SQL の BEGIN は session の transaction を作らない
        Assert.False db5.InTransaction

        let db6 = exec db5 "ROLLBACK"
        let _, after = ok (db6.Query "SELECT id FROM t")
        Assert.Equal<int>(1, after.Rows.Length))

[<Fact>]
let ``E2E: 明示 transaction 内の書き込みが SELECT に見える`` () =
    withDb "tx6" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let tx0 = ok (db1.Begin())
        let tx1, _ = ok (tx0.Execute "INSERT INTO t VALUES (1, 10)")

        let tx2, added = ok (tx1.Query "SELECT id, v FROM t")
        Assert.Equal<int>(1, added.Rows.Length)
        Assert.Equal<int64>(1L, asInt added.Rows.[0].[0])
        Assert.Equal<int64>(10L, asInt added.Rows.[0].[1])

        let tx3, _ = ok (tx2.Execute "UPDATE t SET v = 20 WHERE id = 1")
        let tx4, updated = ok (tx3.Query "SELECT v FROM t")
        Assert.Equal<int64>(20L, asInt updated.Rows.[0].[0])

        let tx5, _ = ok (tx4.Execute "DELETE FROM t WHERE id = 1")
        let tx6, deleted = ok (tx5.Query "SELECT id FROM t")
        Assert.Equal<int>(0, deleted.Rows.Length)

        let committed = ok (tx6.Commit())
        let _, after = ok (committed.Query "SELECT id FROM t")
        Assert.Equal<int>(0, after.Rows.Length))

[<Fact>]
let ``E2E: 明示 transaction 内の索引検索が書き込みを見る`` () =
    withDb "tx7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_t_v ON t (v)"
        let tx0 = ok (db2.Begin())
        let tx1, _ = ok (tx0.Execute "INSERT INTO t VALUES (1, 7)")

        let tx2, found = ok (tx1.Query "SELECT id FROM t WHERE v = 7")
        Assert.Equal<int>(1, found.Rows.Length)

        let tx3, _ = ok (tx2.Execute "UPDATE t SET v = 8 WHERE id = 1")
        let tx4, oldKey = ok (tx3.Query "SELECT id FROM t WHERE v = 7")
        Assert.Equal<int>(0, oldKey.Rows.Length)

        let _, newKey = ok (tx4.Query "SELECT id FROM t WHERE v = 8")
        Assert.Equal<int>(1, newKey.Rows.Length))

[<Fact>]
let ``Database: Root はルートディレクトリを返す`` () =
    withDir "rootprop" (fun dir ->
        let db = ok (Database.openWith dir)
        Assert.Equal(dir, db.Root)
        ok (db.Close()))

// ---------------------------------------------------------------------------
// 実行（Execute）: DDL → DML → SELECT → エラー → 回帰 → E2E → 性能 → 末尾再帰
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: CREATE TABLE でテーブルができる`` () =
    withDb "ddl1" (fun db0 ->
        let db =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        Assert.Equal<string list>([ "people" ], db.TableNames()))

[<Fact>]
let ``Database: CREATE TABLE IF NOT EXISTS は 2 回呼んでも OK`` () =
    withDb "ddl2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"

        let db2 =
            exec db1 "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY, name TEXT)"

        Assert.Equal<string list>([ "people" ], db2.TableNames()))

[<Fact>]
let ``Database: CREATE TABLE の重複はエラー（IF NOT EXISTS なし）`` () =
    withDb "ddl3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"

        expectError
            "重複した CREATE TABLE が OK になっています"
            (db1.Execute "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"))

[<Fact>]
let ``Database: テーブル制約の複合主キー`` () =
    withDb "ddl6" (fun db0 ->
        // 主キーをテーブル制約で指定（列制約ではない）
        let db1 =
            exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, v INTEGER, PRIMARY KEY (a, b))"

        let db2, ins = ok (db1.Execute "INSERT INTO t (a, b) VALUES (1, 2), (1, 3)")
        Assert.Equal<int>(2, ins.RowsAffected)

        // 同じ a でも b が違えば別の行
        let _, res = ok (db2.Query "SELECT a, b FROM t ORDER BY b")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[1]))

[<Fact>]
let ``E2E: 複合主キーは全列一致で検索でき、部分一致も取りこぼさない`` () =
    withDb "ddl6-search" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE t (a INTEGER, b INTEGER, v TEXT, PRIMARY KEY (a, b))"

        let db2 = exec db1 "INSERT INTO t VALUES (1, 2, 'x'), (1, 3, 'y'), (2, 2, 'z')"

        let _, full = ok (db2.Query "SELECT v FROM t WHERE a = 1 AND b = 2")
        Assert.Equal<int>(1, full.Rows.Length)
        Assert.Equal<string>("x", asText full.Rows.[0].[0])

        let _, partialA = ok (db2.Query "SELECT v FROM t WHERE a = 1 ORDER BY b")
        Assert.Equal<string list>([ "x"; "y" ], partialA.Rows |> Array.map (fun r -> asText r.[0]) |> List.ofArray)

        let _, partialB = ok (db2.Query "SELECT v FROM t WHERE b = 2 ORDER BY a")
        Assert.Equal<string list>([ "x"; "z" ], partialB.Rows |> Array.map (fun r -> asText r.[0]) |> List.ofArray)

        // 型が違うリテラルは黙って 0 行にせず、比較時に型エラーにする
        expectErrorKind ErrorKind.TypeError "型不一致の主キー検索" (db2.Query "SELECT v FROM t WHERE a = '1'"))

[<Fact>]
let ``Database: DROP TABLE でテーブルが消える`` () =
    withDb "ddl4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"
        let db2 = exec db1 "DROP TABLE people"
        Assert.Empty(db2.TableNames()))

[<Fact>]
let ``Database: DROP TABLE IF EXISTS は無ければ OK`` () =
    withDb "ddl5" (fun db0 ->
        let db1 = exec db0 "DROP TABLE IF EXISTS nothing"
        Assert.Empty(db1.TableNames()))

[<Fact>]
let ``Database: 存在しない列の索引はエラー`` () =
    withDb "ddl7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)"
        expectError "存在しない列の CREATE INDEX が OK になっています" (db1.Execute "CREATE INDEX ix ON people (nope)"))

[<Fact>]
let ``Database: 存在しないテーブルの索引と索引名の重複はエラー`` () =
    withDb "ddl8" (fun db0 ->
        expectError "存在しないテーブルの CREATE INDEX が OK になっています" (db0.Execute "CREATE INDEX ix ON nope (a)")

        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_a ON t (a)"
        expectError "同名の CREATE INDEX が OK になっています" (db2.Execute "CREATE INDEX ix_a ON t (a)"))

[<Fact>]
let ``Database: UNIQUE 索引の backfill は重複値を拒否する`` () =
    withDb "ddl-unique-backfill" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 7), (2, 7)"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "重複値を含む UNIQUE 索引"
            (db2.Execute "CREATE UNIQUE INDEX ix_a ON t (a)")

        // 失敗した backfill 後も同じ名前で作成でき、索引 keyspace が不完全なまま公開されていないことを確認する。
        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        let _, found = ok (db3.Query "SELECT id FROM t WHERE a = 7")
        Assert.Equal<int>(2, found.Rows.Length))

[<Fact>]
let ``Database: DROP INDEX で索引が消える`` () =
    withDb "ddl9" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2, _ = execRes db1 "INSERT INTO t VALUES (1, 10)"

        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        // 索引を使う検索が通る
        let _, found = ok (db3.Query "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, found.Rows.Length)

        let db4 = exec db3 "DROP INDEX ix_a"
        let _, after = ok (db4.Query "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, after.Rows.Length)

        // 消えたので同じ名前で作り直せる
        let db5 = exec db4 "CREATE INDEX ix_a ON t (a)"
        let _, again = ok (db5.Query "SELECT id FROM t WHERE a = 10")
        Assert.Equal<int>(1, again.Rows.Length))

[<Fact>]
let ``Database: DROP INDEX IF EXISTS と存在しない索引のエラー`` () =
    withDb "ddl10" (fun db0 ->
        let db1 = exec db0 "DROP INDEX IF EXISTS nothing"
        expectError "存在しない DROP INDEX が OK になっています" (db1.Execute "DROP INDEX nothing"))

[<Fact>]
let ``Database: DEFAULT 付きの列定義も保存できる`` () =
    withDb "ddl-default" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT DEFAULT 'x')"

        // 保存時に DEFAULT 式が書かれる（再読込後に既定値として使われるかは別問題）
        let _, rows = ok (db1.Query "SELECT id, v FROM t")
        Assert.Empty rows.Rows)

[<Fact>]
let ``E2E: DEFAULT は再オープン後も既定値として使われる`` () =
    withDir "default-e2e" (fun dir ->
        let db0 = ok (Database.openWith dir)

        let db1 =
            exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER DEFAULT 42, s TEXT DEFAULT 'hi')"

        let db2 = exec db1 "INSERT INTO t (id) VALUES (1)"
        let _, before = ok (db2.Query "SELECT v, s FROM t WHERE id = 1")
        Assert.Equal<int64>(42L, asInt before.Rows.[0].[0])
        Assert.Equal<string>("hi", asText before.Rows.[0].[1])
        ok (db2.Close())

        let db3 = ok (Database.openWith dir)
        let _, after = ok (db3.Query "SELECT v, s FROM t WHERE id = 1")
        Assert.Equal<int64>(42L, asInt after.Rows.[0].[0])
        Assert.Equal<string>("hi", asText after.Rows.[0].[1])

        let db4 = exec db3 "INSERT INTO t (id) VALUES (2)"
        let _, inserted = ok (db4.Query "SELECT v, s FROM t WHERE id = 2")
        Assert.Equal<int64>(42L, asInt inserted.Rows.[0].[0])
        Assert.Equal<string>("hi", asText inserted.Rows.[0].[1])
        ok (db4.Close()))

[<Fact>]
let ``E2E: 数値の範囲外は例外ではなくエラーを返す`` () =
    withDb "numeric-e2e" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v REAL, d DECIMAL)"
        let db2 = exec db1 "INSERT INTO t (id, v, d) VALUES (1, 1e30, 7.9e28)"

        // REAL は decimal の範囲を超えてもそのまま計算できる
        let _, big = ok (db2.Query "SELECT (v * v) FROM t WHERE id = 1")

        match big.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(f > 1e59 && f < 1e61, sprintf "想定外の REAL: %f" f)
        | v -> Assert.Fail(sprintf "REAL ではありません: %A" v)

        // 極小の REAL も 0 に丸めない
        let _, tiny = ok (db2.Query "SELECT (1e-30 * 1.0) FROM t WHERE id = 1")

        match tiny.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.True(f > 0.0 && f < 1e-29, sprintf "想定外の REAL: %f" f)
        | v -> Assert.Fail(sprintf "REAL ではありません: %A" v)

        let _, sum = ok (db2.Query "SELECT SUM(v) FROM t")

        match sum.Rows.[0].[0] with
        | SqlValue.Real f -> Assert.Equal(1e30, f)
        | v -> Assert.Fail(sprintf "REAL ではありません: %A" v)

        // 整数・DECIMAL のオーバーフローは DbError になる
        expectErrorKind
            ErrorKind.ConstraintViolation
            "整数加算のオーバーフロー"
            (db2.Query "SELECT 9223372036854775807 + 1 FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "整数除算のオーバーフロー"
            (db2.Query "SELECT (-9223372036854775807 - 1) / -1 FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "ABS のオーバーフロー"
            (db2.Query "SELECT ABS(-9223372036854775807 - 1) FROM t")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "DECIMAL 乗算のオーバーフロー"
            (db2.Query "SELECT (d * d) FROM t WHERE id = 1")

        // DECIMAL の範囲外の REAL は変換時に型エラー
        expectErrorKind ErrorKind.TypeError "DECIMAL の範囲外" (db2.Execute "INSERT INTO t (id, d) VALUES (2, 1e30)"))

[<Fact>]
let ``Database: INSERT した行が SELECT で見える`` () =
    withDb "dml1" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, ins =
            ok (db1.Execute "INSERT INTO people VALUES (1, 'alice', 30), (2, 'bob', 25)")

        Assert.Equal<int>(2, ins.RowsAffected)

        let _db1, res = ok (db2.Query "SELECT id, name FROM people ORDER BY id")
        Assert.Equal<string array>([| "id"; "name" |], res.Columns)
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(1L, asInt res.Rows.[0].[0])
        Assert.Equal<string>("alice", asText res.Rows.[0].[1]))

[<Fact>]
let ``Database: INSERT は複数 VALUES を 1 文で`` () =
    withDb "dml2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2, ins = ok (db1.Execute "INSERT INTO t VALUES (1,'a'),(2,'b'),(3,'c')")
        Assert.Equal<int>(3, ins.RowsAffected)

        let _db2, res = ok (db2.Query "SELECT id FROM t")
        Assert.Equal<int>(3, res.Rows.Length))

[<Fact>]
let ``Database: UPDATE は WHERE に合う行だけを変える`` () =
    withDb "dml3" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = execRes db1 "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)"
        let db3, upd = ok (db2.Execute "UPDATE t SET v = 99 WHERE id > 1")
        Assert.Equal<int>(2, upd.RowsAffected)

        let _db3, res = ok (db3.Query "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(99L, asInt res.Rows.[1].[1])
        Assert.Equal<int64>(99L, asInt res.Rows.[2].[1]))

[<Fact>]
let ``Database: DELETE は WHERE に合う行を消す`` () =
    withDb "dml4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)")
        let db3, del = ok (db2.Execute "DELETE FROM t WHERE v >= 20")
        Assert.Equal<int>(2, del.RowsAffected)

        let _db4, res = ok (db3.Query "SELECT id FROM t")
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
let ``Database: SELECT * は全列を返す`` () =
    seeded "sel1" (fun db ->
        let _db5, res = ok (db.Query "SELECT * FROM people ORDER BY id")
        Assert.Equal<string array>([| "id"; "name"; "age" |], res.Columns)
        Assert.Equal<int>(3, res.Rows.Length))

[<Fact>]
let ``Database: WHERE で絞る`` () =
    seeded "sel2" (fun db ->
        let _db6, res = ok (db.Query "SELECT name FROM people WHERE age > 26 ORDER BY name")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[1].[0]))

[<Fact>]
let ``Database: ORDER BY が効く`` () =
    seeded "sel3" (fun db ->
        let _, asc = ok (db.Query "SELECT name FROM people ORDER BY age")
        Assert.Equal<string>("bob", asText asc.Rows.[0].[0])

        let _, desc = ok (db.Query "SELECT name FROM people ORDER BY age DESC")
        Assert.Equal<string>("carol", asText desc.Rows.[0].[0]))

[<Fact>]
let ``Database: 別名を ORDER BY で参照できる`` () =
    seeded "sel-alias-order" (fun db ->
        // 別名 a は age を指すので age の順（bob 25 → alice 30 → carol 35）で並ぶ
        let _, res = ok (db.Query "SELECT name AS x, age AS a FROM people ORDER BY a")
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<string>("bob", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[2].[0]))

[<Fact>]
let ``Database: 別名を GROUP BY と HAVING で参照できる`` () =
    seeded "sel-alias-group" (fun db ->
        let _, res = ok (db.Query "SELECT age AS a, COUNT(*) FROM people GROUP BY a")
        Assert.Equal<int>(3, res.Rows.Length)

        // HAVING にも別名を参照できる
        let _, having = ok (db.Query "SELECT COUNT(*) AS c FROM people HAVING c > 2")
        Assert.Equal<int>(1, having.Rows.Length)
        Assert.Equal<int64>(3L, asInt having.Rows.[0].[0]))

[<Fact>]
let ``Database: 別名より列が優先される`` () =
    seeded "sel-alias-priority" (fun db ->
        // age という列があるのに age という別名を付けても、ORDER BY は列を見る
        // （列の順: bob 25 → alice 30 → carol 35。別名=name なら alice が先）
        let _, res = ok (db.Query "SELECT name AS age FROM people ORDER BY age")
        Assert.Equal<string>("alice", asText res.Rows.[1].[0]))

[<Fact>]
let ``Database: 別名の循環参照でも止まる`` () =
    seeded "sel-alias-cycle" (fun db ->
        // x と y が互いを指す（差し替えで無限ループにしない）
        expectError "循環参照する別名は参照できない" (db.Query "SELECT x AS y, y AS x FROM people ORDER BY x"))

[<Fact>]
let ``Database: GROUP BY と COUNT`` () =
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
let ``Database: HAVING でグループを絞る`` () =
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
let ``Database: LIMIT`` () =
    seeded "sel5" (fun db ->
        let _db7, res = ok (db.Query "SELECT id FROM people ORDER BY id LIMIT 2")
        Assert.Equal<int>(2, res.Rows.Length))

[<Fact>]
let ``Database: OFFSET`` () =
    seeded "sel6" (fun db ->
        let _db8, res = ok (db.Query "SELECT id FROM people ORDER BY id LIMIT 5 OFFSET 1")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``Database: DISTINCT`` () =
    withDb "sel7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30)")
        let _db9, res = ok (db2.Query "SELECT DISTINCT g FROM t ORDER BY g")
        Assert.Equal<int>(3, res.Rows.Length)
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``Database: 別名を付けられる`` () =
    seeded "sel8" (fun db ->
        let _db10, res = ok (db.Query "SELECT name AS n FROM people ORDER BY id LIMIT 1")
        Assert.Equal<string array>([| "n" |], res.Columns))

[<Fact>]
let ``Database: 集約関数`` () =
    seeded "sel9" (fun db ->
        let _, cnt = ok (db.Query "SELECT COUNT(*) FROM people")
        Assert.Equal<int64>(3L, asInt cnt.Rows.[0].[0])

        let _, sum = ok (db.Query "SELECT SUM(age) FROM people")
        Assert.True(SqlValue.isNull sum.Rows.[0].[0] |> not)

        let _, mn = ok (db.Query "SELECT MIN(age), MAX(age) FROM people")
        Assert.Equal<int64>(25L, asInt mn.Rows.[0].[0])
        Assert.Equal<int64>(35L, asInt mn.Rows.[0].[1]))

[<Fact>]
let ``Database: JOIN`` () =
    withDb "sel10" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"

        let db2, _ = execRes db1 "INSERT INTO people VALUES (1,'alice',30),(2,'bob',25)"

        let db3 =
            exec db2 "CREATE TABLE orders (oid INTEGER PRIMARY KEY, person_id INTEGER, amount INTEGER)"

        let db4, _ = execRes db3 "INSERT INTO orders VALUES (1,1,100),(2,1,200),(3,2,50)"

        let _, res =
            ok (
                db4.Query
                    "SELECT people.name, SUM(orders.amount) FROM people JOIN orders ON people.id = orders.person_id GROUP BY people.name ORDER BY people.name"
            )

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("bob", asText res.Rows.[1].[0]))

[<Fact>]
let ``Database: 3 テーブルの入れ子 JOIN が行を返す`` () =
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
            ok (db6.Query "SELECT a.v, b.av, c.bv FROM a JOIN b ON a.id = b.id JOIN c ON b.id = c.id ORDER BY a.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(10L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(100L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(1000L, asInt res.Rows.[0].[2])
        Assert.Equal<int64>(20L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(200L, asInt res.Rows.[1].[1])
        Assert.Equal<int64>(2000L, asInt res.Rows.[1].[2]))

[<Fact>]
let ``末尾再帰: 60 段の入れ子 JOIN が正しい行を返す`` () =
    // plan の深さは JOIN の数だけ伸びる（サブクエリ未対応なので Join が唯一の源泉）。
    // exec は descend（積む側）/ unwind（回す側）で末尾化済みだが、
    // 左へ下ろす「右テーブルを除いたスキーマ」が段をまたいで正しく累積するかは
    // 深さ方向で検証する（docs/gotchas.md §3.5 のバグは 3 段で発生した）。
    // 作成は fsync 主導なので段数は 60 に抑える（120 段で 9 秒かかる実測あり）。
    let n = 60

    withDb "deepjoin" (fun db0 ->
        let names = Array.init n (fun i -> sprintf "t%d" i)

        let db1 =
            names
            |> Array.fold
                (fun db name -> exec db (sprintf "CREATE TABLE %s (id INTEGER PRIMARY KEY, v INTEGER)" name))
                db0

        let db2 =
            names
            |> Array.mapi (fun i name -> sprintf "INSERT INTO %s VALUES (1,%d),(2,%d)" name i i)
            |> Array.fold (fun db sql -> exec db sql) db1

        // ON は「1 つ前 のテーブル」と結ぶ（左深いチェーンを作る）。
        let onClauses =
            names
            |> Array.skip 1
            |> Array.mapi (fun k name -> sprintf "JOIN %s ON %s.id = %s.id" name names.[k] name)
            |> String.concat " "

        let sql = sprintf "SELECT t0.v, t%d.v FROM t0 %s ORDER BY t0.v" (n - 1) onClauses
        let _, res = ok (db2.Query sql)
        // id = 1 と 2 の 2 行が全段で一致する（エラーで 0 行になっていないこと）。
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int>(2, res.Columns.Length)
        Assert.Equal<int64>(0L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(int64 (n - 1), asInt res.Rows.[0].[1])
        Assert.Equal<int64>(0L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(int64 (n - 1), asInt res.Rows.[1].[1]))

[<Fact>]
let ``Database: エイリアスでの自己結合が行を返す`` () =
    // JOIN の対応付けを RealName で探していたころは、`FROM t AS x JOIN t AS y` が
    // 先頭の t にヒットして JOIN が丸ごと省略され、
    // 列インデックスが範囲外（2 列中）になって落ちていた。
    withDb "selfjoin" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20),(3,30)"

        // 右だけを絞り込む WHERE が効くこと（JOIN が本当に組まれていること）。
        let _, res =
            ok (db2.Query "SELECT p.id, q.v FROM t AS p JOIN t AS q ON p.id = q.id WHERE q.v > 15 ORDER BY p.id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(20L, asInt res.Rows.[0].[1])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(30L, asInt res.Rows.[1].[1])

        // 非キーでの自己結合（それぞれ自分自身とだけ一致する）。
        let _, res2 =
            ok (db2.Query "SELECT p.id, q.id FROM t AS p JOIN t AS q ON p.v = q.v ORDER BY p.id")

        Assert.Equal<int>(3, res2.Rows.Length)
        Assert.Equal<int64>(2L, asInt res2.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt res2.Rows.[1].[1])

        // 3 段の自己結合（p / q / r すべて t のエイリアス）。
        let _, res3 =
            ok (
                db2.Query
                    "SELECT p.id, q.id, r.v FROM t AS p JOIN t AS q ON p.id = q.id JOIN t AS r ON q.id = r.id WHERE p.v = 30 ORDER BY p.id"
            )

        Assert.Equal<int>(1, res3.Rows.Length)
        Assert.Equal<int64>(3L, asInt res3.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res3.Rows.[0].[1])
        Assert.Equal<int64>(30L, asInt res3.Rows.[0].[2]))

[<Fact>]
let ``Database: ハッシュ結合（INNER / LEFT / NULL キー）`` () =
    withDb "joinhash" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)"
        let db2 = exec db1 "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)"
        let db3 = exec db2 "INSERT INTO a VALUES (1,10),(2,20),(3,NULL)"
        let db4 = exec db3 "INSERT INTO b VALUES (1,10),(2,10),(3,30),(4,NULL)"

        // INNER: NULL キーは一致しない（a3 は落ちる）
        let _, inner =
            ok (db4.Query "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k ORDER BY a.id, b.id")

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<int64>(1L, asInt inner.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt inner.Rows.[0].[1])
        Assert.Equal<int64>(1L, asInt inner.Rows.[1].[0])
        Assert.Equal<int64>(2L, asInt inner.Rows.[1].[1])

        // LEFT: 一致しない a2 / a3 は右側 NULL
        let _, left =
            ok (db4.Query "SELECT a.id, b.id FROM a LEFT JOIN b ON a.k = b.k ORDER BY a.id, b.id")

        Assert.Equal<int>(4, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[2].[1])
        Assert.True(SqlValue.isNull left.Rows.[3].[1])

        // 残りの ON 句（b.id = 1）はハッシュ結合でも評価される
        let _, residual =
            ok (db4.Query "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k AND b.id = 1")

        Assert.Equal<int>(1, residual.Rows.Length)
        Assert.Equal<int64>(1L, asInt residual.Rows.[0].[1]))

[<Fact>]
let ``Database: インデックス入れ子ループ（右索引 + 左が小さい）`` () =
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
                db5.Query
                    "SELECT depts.name, users.name FROM depts JOIN users ON depts.id = users.dept_id WHERE depts.id = 1 ORDER BY users.id"
            )

        Assert.Equal<int>(2, inner.Rows.Length)
        Assert.Equal<string>("alice", asText inner.Rows.[0].[1])
        Assert.Equal<string>("bob", asText inner.Rows.[1].[1])

        // 一致なしの LEFT JOIN は右側 NULL
        let _, left =
            ok (
                db5.Query
                    "SELECT depts.name, users.name FROM depts LEFT JOIN users ON depts.id = users.dept_id WHERE depts.id = 3"
            )

        Assert.Equal<int>(1, left.Rows.Length)
        Assert.True(SqlValue.isNull left.Rows.[0].[1])

        // 等値キー以外の ON 句も索引結合で評価される
        let _, residual =
            ok (
                db5.Query
                    "SELECT users.name FROM depts JOIN users ON depts.id = users.dept_id AND users.name = 'bob' WHERE depts.id = 1"
            )

        Assert.Equal<int>(1, residual.Rows.Length)
        Assert.Equal<string>("bob", asText residual.Rows.[0].[0]))

[<Fact>]
let ``Database: JOIN の非修飾で曖昧な列は NameError`` () =
    // 非修飾 id は p と q の両方にある。各テーブルへ pushdown すると
    // それぞれ id = 1 で絞ってしまい、曖昧エラーにならず意味が変わる。
    withDb "joinambig" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10),(2,20)"

        expectErrorKind
            ErrorKind.NameError
            "JOIN の曖昧な非修飾列がエラーになりません"
            (db2.Query "SELECT p.id FROM t AS p JOIN t AS q ON p.id = q.id WHERE id = 1"))

[<Fact>]
let ``Database: テーブル名の重複はエラー（エイリアスが必要）`` () =
    // 識別名が重複すると exec が名前でスキーマを引けず（両方が消える）、
    // エラーではなく静かな誤答になるので、名前解決の段階で止める。
    withDb "dupjoin" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1,10)"

        expectErrorKind
            ErrorKind.NameError
            "重複したテーブル名がエラーになりません"
            (db2.Query "SELECT t.id FROM t JOIN t ON t.id = t.id"))

[<Fact>]
let ``Database: JOIN の WHERE は右テーブルと横断述語に効く`` () =
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
                db4.Query
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE orders.amount = 100"
            )

        Assert.Equal<int>(1, rightOnly.Rows.Length)
        Assert.Equal<int64>(1L, asInt rightOnly.Rows.[0].[0])

        // 非修飾でも一意に決まる列（amount は orders だけ）は従来どおり pushdown できる
        let _, unqualifiedUnique =
            ok (
                db4.Query "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE amount = 100"
            )

        Assert.Equal<int>(1, unqualifiedUnique.Rows.Length)
        Assert.Equal<int64>(1L, asInt unqualifiedUnique.Rows.[0].[0])

        // 2 テーブルにまたがる WHERE（ON に無い述語が JOIN 上で評価される）
        let _, cross =
            ok (
                db4.Query
                    "SELECT people.id FROM people JOIN orders ON people.id = orders.person_id WHERE people.age * 4 < orders.amount"
            )

        Assert.Equal<int>(1, cross.Rows.Length)
        Assert.Equal<int64>(1L, asInt cross.Rows.[0].[0]))

[<Fact>]
let ``Database: LEFT JOIN の WHERE は null 供給側に効く`` () =
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
                db4.Query
                    "SELECT people.id FROM people LEFT JOIN orders ON people.id = orders.person_id WHERE orders.amount IS NULL"
            )

        Assert.Equal<int>(1, isNullRows.Rows.Length)
        Assert.Equal<int64>(3L, asInt isNullRows.Rows.[0].[0])

        // 右への pushdown を入れると carol まで残って 3 行になる
        let _, matched =
            ok (
                db4.Query
                    "SELECT people.id FROM people LEFT JOIN orders ON people.id = orders.person_id WHERE orders.amount = 100"
            )

        Assert.Equal<int>(1, matched.Rows.Length)
        Assert.Equal<int64>(1L, asInt matched.Rows.[0].[0]))

[<Fact>]
let ``Database: 複数文を ; で渡せる`` () =
    withDb "tx3" (fun db0 ->
        let sql =
            "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER); "
            + "INSERT INTO t VALUES (1, 1); "
            + "INSERT INTO t VALUES (2, 2)"

        let db, _r = execRes db0 sql
        let _q, res = execRes db "SELECT id FROM t"
        Assert.Equal<int>(2, res.Rows.Length))

// ---------------------------------------------------------------------------
// エラー
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: 存在しないテーブルの SELECT はエラー`` () =
    withDb "err1" (fun db -> expectError "存在しないテーブルの SELECT が OK になっています" (db.Query "SELECT * FROM nothing"))

[<Fact>]
let ``Database: 存在しない列の WHERE はエラー`` () =
    seeded "err2" (fun db -> expectError "存在しない列の WHERE が OK になっています" (db.Query "SELECT id FROM people WHERE nope = 1"))

[<Fact>]
let ``Database: 構文エラーはエラー（例外ではない）`` () =
    withDb "err3" (fun db -> expectError "構文エラーが OK になっています" (db.Execute "SELEC 1"))

[<Fact>]
let ``Database: NOT NULL 制約違反はエラー`` () =
    withDb "err4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER NOT NULL)"

        expectError "NOT NULL 違反が OK になっています" (db1.Execute "INSERT INTO t (id) VALUES (1)"))

[<Fact>]
let ``Database: QueryResult の表示が使える`` () =
    seeded "disp1" (fun db ->
        let _db15, res = ok (db.Query "SELECT id FROM people ORDER BY id LIMIT 1")
        let lines = res.ToDisplayLines()

        Assert.True(lines |> List.exists (fun l -> l.Contains "id"))
        Assert.True(lines |> List.exists (fun l -> l.Contains "1")))

[<Fact>]
let ``Database: CASE WHEN が評価できる`` () =
    seeded "reg-case" (fun db ->
        let _, res =
            ok (db.Query "SELECT name FROM people WHERE (CASE WHEN age > 26 THEN 1 ELSE 0 END) = 1 ORDER BY name")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<string>("alice", asText res.Rows.[0].[0])
        Assert.Equal<string>("carol", asText res.Rows.[1].[0]))

[<Fact>]
let ``Database: CASE WHEN が射影に書ける`` () =
    seeded "reg-case2" (fun db ->
        let _, res =
            ok (
                db.Query
                    "SELECT name AS n, CASE WHEN age > 26 THEN 'old' ELSE 'young' END AS band FROM people ORDER BY id"
            )

        Assert.Equal<string array>([| "n"; "band" |], res.Columns)
        Assert.Equal<string>("old", asText res.Rows.[0].[1])
        Assert.Equal<string>("young", asText res.Rows.[1].[1]))

[<Fact>]
let ``Database: LIMIT は OFFSET を適用してから取る`` () =
    seeded "reg-limit" (fun db ->
        let _, res = ok (db.Query "SELECT id FROM people ORDER BY id LIMIT 2 OFFSET 1")
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0])

        // DESC でも同じこと。
        let _, desc =
            ok (db.Query "SELECT id FROM people ORDER BY id DESC LIMIT 2 OFFSET 1")

        Assert.Equal<int>(2, desc.Rows.Length)
        Assert.Equal<int64>(2L, asInt desc.Rows.[0].[0])
        Assert.Equal<int64>(1L, asInt desc.Rows.[1].[0]))

[<Fact>]
let ``Database: 索引の等価検索は一致する行をすべて返す`` () =
    withDb "reg-index" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"

        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,20),(4,30)")

        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        let _, res = ok (db3.Query "SELECT id FROM t WHERE g = 20 ORDER BY id")

        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int64>(2L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(3L, asInt res.Rows.[1].[0]))

[<Fact>]
let ``Database: 索引列を更新すると索引も追従する`` () =
    withDb "reg-index-upd" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20)")
        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        // 索引キーが変わる更新（古いキーの削除 + 新しいキーの追加）
        let db4, upd = ok (db3.Execute "UPDATE t SET g = 99 WHERE id = 1")
        Assert.Equal<int>(1, upd.RowsAffected)

        let _, hit = ok (db4.Query "SELECT id FROM t WHERE g = 99")
        Assert.Equal<int>(1, hit.Rows.Length)
        Assert.Equal<int64>(1L, asInt hit.Rows.[0].[0])

        // 古い索引エントリからは復活しない
        let _, miss = ok (db4.Query "SELECT id FROM t WHERE g = 10")
        Assert.Equal<int>(0, miss.Rows.Length))

[<Fact>]
let ``Database: DROP TABLE 後の同名再作成で旧データが復活しない`` () =
    withDb "drop-table-recreate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 'old'), (2, 'old')")
        let db3 = exec db2 "DROP TABLE t"
        let db4 = exec db3 "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)"
        let _, res = ok (db4.Query "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt res.Rows.[0].[0]))

[<Fact>]
let ``Database: 索引ごと DROP TABLE しても旧索引エントリが復活しない`` () =
    withDb "drop-table-index" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20)")
        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        let db4 = exec db3 "DROP TABLE t"
        let db5 = exec db4 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db6 = exec db5 "CREATE INDEX ix_g ON t (g)"
        let _, res = ok (db6.Query "SELECT id FROM t WHERE g = 10")
        Assert.Equal<int>(0, res.Rows.Length))

[<Fact>]
let ``Database: DROP INDEX 後の同名再作成で旧索引エントリが復活しない`` () =
    withDb "drop-index-recreate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1,10),(2,20)")
        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        // 索引を落としてから行を消す（この間は索引 tombstone が書かれない）
        let db4 = exec db3 "DROP INDEX ix_g"
        let db5 = exec db4 "DELETE FROM t WHERE id = 1"
        let db6 = exec db5 "CREATE INDEX ix_g ON t (g)"
        // 旧エントリが keyspace に残っていれば、消した行が索引検索で復活する
        let _, gone = ok (db6.Query "SELECT id FROM t WHERE g = 10")
        Assert.Equal<int>(0, gone.Rows.Length)

        let _, alive = ok (db6.Query "SELECT id FROM t WHERE g = 20")
        Assert.Equal<int>(1, alive.Rows.Length)
        Assert.Equal<int64>(2L, asInt alive.Rows.[0].[0]))

[<Fact>]
let ``Database: 負数の主キー等値検索`` () =
    withDb "reg-negpk" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (-5, 50), (5, 50)")
        // 主キー検索（Unary Neg のリテラルから PkLookup が選ばれる）
        let _, res = ok (db2.Query "SELECT id, v FROM t WHERE id = -5")

        Assert.Equal<int>(1, res.Rows.Length)
        Assert.Equal<int64>(-5L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(50L, asInt res.Rows.[0].[1]))

// ===========================================================================
// E2E（通し経路）
// ===========================================================================

// ---------------------------------------------------------------------------
// ライブラリ API の E2E
// ---------------------------------------------------------------------------

[<Fact>]
let ``E2E: 一連の流れが誤差なく通る`` () =
    withDir "flow" (fun dir ->
        let db0 = ok (Database.openWith dir)
        let db = seedLibrary db0

        // 単純な射影
        let all = query db "SELECT id, name, age FROM people ORDER BY id"
        Assert.Equal<string array>([| "id"; "name"; "age" |], all.Columns)
        Assert.Equal<int>(4, all.Rows.Length)
        Assert.Equal<string>("alice", asText all.Rows.[0].[1])
        Assert.Equal<int64>(25L, asInt all.Rows.[1].[2])

        // 集約
        let sum = query db "SELECT COUNT(*), MIN(age), MAX(age), SUM(age) FROM people"
        Assert.Equal<int64>(4L, asInt sum.Rows.[0].[0])
        Assert.Equal<int64>(25L, asInt sum.Rows.[0].[1])
        Assert.Equal<int64>(35L, asInt sum.Rows.[0].[2])

        // GROUP BY（seed の age は 30, 25, 35, 25 → 25 が 2 行、30 と 35 が 1 行）
        let grouped = query db "SELECT age, COUNT(*) FROM people GROUP BY age ORDER BY age"
        Assert.Equal<int>(3, grouped.Rows.Length)
        Assert.Equal<int64>(25L, asInt grouped.Rows.[0].[0])
        Assert.Equal<int64>(2L, asInt grouped.Rows.[0].[1])
        Assert.Equal<int64>(30L, asInt grouped.Rows.[1].[0])
        Assert.Equal<int64>(1L, asInt grouped.Rows.[1].[1])
        Assert.Equal<int64>(35L, asInt grouped.Rows.[2].[0])
        Assert.Equal<int64>(1L, asInt grouped.Rows.[2].[1])

        // JOIN + 集約
        let joined =
            query
                db
                "SELECT people.name, SUM(orders.amount) FROM people JOIN orders ON people.id = orders.person_id GROUP BY people.name ORDER BY people.name"

        Assert.Equal<int>(3, joined.Rows.Length)
        Assert.Equal<string>("alice", asText joined.Rows.[0].[0])
        Assert.Equal<string>("bob", asText joined.Rows.[1].[0])
        Assert.Equal<string>("carol", asText joined.Rows.[2].[0])

        // 索引検索
        let byIndex = query db "SELECT id FROM people WHERE name = 'bob'"
        Assert.Equal<int>(1, byIndex.Rows.Length)
        Assert.Equal<int64>(2L, asInt byIndex.Rows.[0].[0])

        // UPDATE / DELETE
        let db2 = exec db "UPDATE people SET age = 26 WHERE name = 'bob'"
        let updated = query db2 "SELECT age FROM people WHERE name = 'bob'"
        Assert.Equal<int64>(26L, asInt updated.Rows.[0].[0])

        let db3 = exec db2 "DELETE FROM people WHERE name = 'dave'"
        let after = query db3 "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(3L, asInt after.Rows.[0].[0])

        // 例外は「例外を投げずに Result で返す」
        match db3.Execute "SELECT * FROM nope" with
        | Ok(_, _) -> Assert.Fail("存在しないテーブルで成功してよいはずがない")
        | Error e -> Assert.Contains("nope", DbError.toDisplayString e)

        ok (db3.Close()))

// ---------------------------------------------------------------------------
// 性能スモーク
// ---------------------------------------------------------------------------

[<Fact>]
let ``性能: 多数行の insert → point lookup → range scan`` () =
    withDir "perf" (fun dir ->
        // 10 万行。実用時間内で終わることを確かめる。
        let rowCount = 100000

        let db =
            exec (ok (Database.openWith dir)) "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"

        // 複数行 INSERT をまとめて流す。
        let sw = Stopwatch.StartNew()
        let mutable current = db

        let mutable i = 1

        while i <= rowCount do
            let take = min 2000 (rowCount - i + 1)

            let values =
                [ i .. i + take - 1 ]
                |> List.map (fun id -> sprintf "(%d, %d)" id (id * 3))
                |> String.concat ", "

            current <- exec current (sprintf "INSERT INTO t VALUES %s" values)
            i <- i + take

        let insertMs = sw.ElapsedMilliseconds

        // point lookup（主キー）
        let sw2 = Stopwatch.StartNew()
        let mutable probe = 0L

        for id in [ 1..500 ] do
            let r = query current (sprintf "SELECT v FROM t WHERE id = %d" id)

            if r.Rows.Length = 1 then
                probe <- probe + asInt r.Rows.[0].[0]

        let lookupMs = sw2.ElapsedMilliseconds

        // range scan（集計で全行を走らせる）
        let sw3 = Stopwatch.StartNew()
        let count = query current "SELECT COUNT(*) FROM t"
        let scanMs = sw3.ElapsedMilliseconds
        Assert.Equal<int64>(int64 rowCount, asInt count.Rows.[0].[0])
        Assert.True(probe > 0)

        // 「実用時間内」の緩い基準（少なくともハングしていないこと）。
        Assert.True(insertMs < 60000L, sprintf "insert が遅すぎます: %d ms" insertMs)
        Assert.True(lookupMs < 60000L, sprintf "point lookup が遅すぎます: %d ms" lookupMs)
        Assert.True(scanMs < 60000L, sprintf "scan が遅すぎます: %d ms" scanMs)

        ok (current.Close()))

// ---------------------------------------------------------------------------
// 末尾再帰の検証（大量の文でスタックを食わないこと）
// ---------------------------------------------------------------------------

[<Fact>]
let ``末尾再帰: 大量の文を 1 度の Execute で実行できる`` () =
    // Execute の runStatements は `runStatement … |> Result.bind (fun … -> runStatements …)` で
    // 文の数だけフレームが積まれていた（→ StackOverflow / exit 134）。
    // `match` で受けて末尾再帰にしてある。
    let n = 20_000

    withDb "tail-exec" (fun db0 ->
        let db = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let sql = System.String.Join(";", Array.init n (fun _ -> "SELECT a FROM t"))
        let next, _ = ok (db.Execute sql)
        Assert.Equal<string list>([ "t" ], next.TableNames()))

// ---------------------------------------------------------------------------
// 再オープン（openWith / Close）: 永続性とクラッシュ耐性
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: 閉じて開き直すとデータが残る`` () =
    withDir "i-reopen" (fun dir ->
        let db0 = ok (Database.openWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2, _ins = ok (db1.Execute "INSERT INTO t VALUES (1, 'kept')")
        ok (db2.Close())

        // 開き直す
        let db3 = ok (Database.openWith dir)
        Assert.Equal<string list>([ "t" ], db3.TableNames())

        let _db4, res = ok (db3.Query "SELECT v FROM t")
        Assert.Equal<int>(1, res.Rows.Length)

        let cell: SqlValue = res.Rows.[0].[0]
        let actual: string = asText cell
        Assert.Equal<string>("kept", actual)

        ok (db3.Close()))

[<Fact>]
let ``Database: 古いハンドルを Close しても新しいハンドルの commit は失われない`` () =
    withDir "stale-close" (fun dir ->
        let db0 = ok (Database.openWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1)")
        let db3, _ = ok (db2.Execute "INSERT INTO t VALUES (2)")

        let before = query db3 "SELECT COUNT(*) FROM t"
        Assert.Equal<int64>(2L, asInt before.Rows.[0].[0])

        // db2 は db3 より前の状態。古いハンドルの Close で WAL を消してはいけない。
        ok (db2.Close())

        let reopened = ok (Database.openWith dir)
        let after = query reopened "SELECT COUNT(*) FROM t"
        Assert.Equal<int64>(2L, asInt after.Rows.[0].[0])
        ok (reopened.Close()))

[<Fact>]
let ``E2E: プロセス再起動を模して Disk から復元できる`` () =
    withDir "reopen" (fun dir ->
        let db = seedLibrary (ok (Database.openWith dir))
        ok (db.Close())

        // 別ハンドルを「別プロセス」として扱う。
        let reopened = ok (Database.openWith dir)
        // `TableNames` の並び順は保証されない（keyspace の base64 順）のでソートする。
        Assert.Equal<string list>([ "orders"; "people" ], reopened.TableNames() |> List.sort)

        let people = query reopened "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt people.Rows.[0].[0])

        // 索引が「作られる前」に入った行も引ける（backfill の検証）
        let byIndex = query reopened "SELECT id FROM people WHERE name = 'carol'"
        Assert.Equal<int>(1, byIndex.Rows.Length)
        Assert.Equal<int64>(3L, asInt byIndex.Rows.[0].[0])

        ok (reopened.Close()))

// クラッシュ耐性（Close を呼ばずに再オープン → WAL 再生）

[<Fact>]
let ``E2E: Close しなくても commit 済みの行は残る`` () =
    withDir "crash" (fun dir ->
        let _db = seedLibrary (ok (Database.openWith dir))
        // Close を呼ばない（memtable は SSTable 未落下、残るのは WAL 再生のみ）。
        let reopened = ok (Database.openWith dir)

        let count = query reopened "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt count.Rows.[0].[0])

        ok (reopened.Close()))

[<Fact>]
let ``Database: active transaction は snapshot registry に登録され、commit / rollback / Close で外れる`` () =
    withDir "reg-lifecycle" (fun dir ->
        let db = ok (Database.openWith dir)
        let root = db.Root
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsNone)

        // 明示 transaction: Begin で登録、Commit で解除
        let tx = ok (db.Begin())
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsSome)
        let committed = ok (tx.Commit())
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsNone)

        // Rollback でも解除
        let tx2 = ok (committed.Begin())
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsSome)
        let rolledBack = ok (tx2.Rollback())
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsNone)

        // SQL の BEGIN / COMMIT 経由でも同じ
        let sqlTx, _ = ok (rolledBack.Execute "BEGIN")
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsSome)
        let sqlDone, _ = ok (sqlTx.Execute "COMMIT")
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsNone)

        // 暗黙 transaction は Execute の中で登録・解除が完結する
        let withTable, _ = ok (sqlDone.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)")
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsNone)

        // Close でも解除される（閉じたハンドルの snapshot は誰も読めない）
        let openTx = ok (withTable.Begin())
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsSome)
        ok (openTx.Close())
        Assert.True((ActiveSnapshotRegistry.oldestExcept None root).IsNone))

// CREATE INDEX の排他

[<Fact>]
let ``Database: 実行中の transaction があると DDL は ConflictError`` () =
    withDir "ddl-conflict" (fun dir ->
        let db1 = ok (Database.openWith dir)
        let db2 = exec db1 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"

        // BEGIN は新しいハンドルを返す。元のハンドルは transaction を持たないので DDL を試せる。
        let tx = ok (db2.Begin())
        let tx2, _ = ok (tx.Execute "INSERT INTO t VALUES (1, 10)")

        // backfill 中に古いスキーマの transaction が commit すると索引が壊れるので、排他にする。
        expectErrorKind
            ErrorKind.ConflictError
            "実行中の transaction があると CREATE INDEX は ConflictError"
            (db2.Execute "CREATE INDEX ix_v ON t (v)")

        // transaction を終わらせれば通る（そのあとは索引が維持される）。
        let rolled = ok (tx2.Rollback())
        let made = exec db2 "CREATE INDEX ix_v ON t (v)"
        let inserted = exec made "INSERT INTO t VALUES (2, 20)"
        let _, found = ok (inserted.Query "SELECT id FROM t WHERE v = 20")
        Assert.Equal<int>(1, found.Rows.Length)

        ok (inserted.Close())
        ok (rolled.Close()))
