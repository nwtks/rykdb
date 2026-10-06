// ===========================================================================
// Database.fs のテスト: 公開 API・E2E・性能
//
// 「SQL 文字列 → 結果」の通し経路で Parser / Planner / Executor / Catalog の
// 連携バグと、クラッシュ・再オープン・性能までを検証する。
// ===========================================================================

module RykDb.DatabaseTests

open System
open System.Diagnostics
open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private withDb (tag: string) (f: Database -> unit) =
    let dir =
        Path.Combine(Path.GetTempPath(), sprintf "rykdb-db-%s-%d" tag DateTime.Now.Ticks)

    try
        protect (fun () -> Directory.CreateDirectory dir |> ignore) |> ignore
        let db = ok (Database.openWith dir)
        f db
        ok (db.Close())
    finally
        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

/// 1 文を実行して「新しい Database」と結果の両方を返す。
let private execRes (db: Database) (sql: string) = ok (db.Execute sql)

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
// DDL
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

        match db1.Execute "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT)" with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("重複した CREATE TABLE が OK になっています"))

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

        match db1.Execute "CREATE INDEX ix ON people (nope)" with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("存在しない列の CREATE INDEX が OK になっています"))

// ---------------------------------------------------------------------------
// DML
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// SELECT
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// トランザクション
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: BEGIN/COMMIT でまとめて反映される`` () =
    withDb "tx1" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = db1.Begin()
        Assert.True(db2.InTransaction)

        let db3, _ = ok (db2.Execute "INSERT INTO t VALUES (1, 1)")
        let db4, _ = ok (db3.Execute "INSERT INTO t VALUES (2, 2)")
        let db5 = ok (db4.Commit())

        Assert.False(db5.InTransaction)
        let _db11, res = ok (db5.Query "SELECT id FROM t")
        Assert.Equal<int>(2, res.Rows.Length))

[<Fact>]
let ``Database: ROLLBACK で元に戻る`` () =
    withDb "tx2" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 1)")

        let db3 = db2.Begin()
        let db4, _ = ok (db3.Execute "INSERT INTO t VALUES (2, 2)")
        let db5 = db4.Rollback()

        Assert.False(db5.InTransaction)
        let _db12, res = ok (db5.Query "SELECT id FROM t")
        Assert.Equal<int>(1, res.Rows.Length))

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
// 永続化
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: 閉じて開き直すとデータが残る`` () =
    let dir =
        Path.Combine(Path.GetTempPath(), sprintf "rykdb-i-reopen-%d" DateTime.Now.Ticks)

    try
        protect (fun () -> Directory.CreateDirectory(dir) |> ignore) |> ignore

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

        ok (db3.Close())
    finally
        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

// ---------------------------------------------------------------------------
// エラー
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: 存在しないテーブルの SELECT はエラー`` () =
    withDb "err1" (fun db ->
        match db.Query "SELECT * FROM nothing" with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("存在しないテーブルの SELECT が OK になっています"))

[<Fact>]
let ``Database: 存在しない列の WHERE はエラー`` () =
    seeded "err2" (fun db ->
        match db.Query "SELECT id FROM people WHERE nope = 1" with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("存在しない列の WHERE が OK になっています"))

[<Fact>]
let ``Database: 構文エラーはエラー（例外ではない）`` () =
    withDb "err3" (fun db ->
        match db.Execute "SELEC 1" with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("構文エラーが OK になっています"))

[<Fact>]
let ``Database: NOT NULL 制約違反はエラー`` () =
    withDb "err4" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER NOT NULL)"

        match db1.Execute "INSERT INTO t (id) VALUES (1)" with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("NOT NULL 違反が OK になっています"))

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
// 再オープン（永続性）
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// クラッシュ耐性（Close を呼ばずに再オープン → WAL 再生）
// ---------------------------------------------------------------------------

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
let ``E2E: commit してない transaction は残らない`` () =
    withDir "crash2" (fun dir ->
        let db = seedLibrary (ok (Database.openWith dir))

        // 明示 transaction を開いて write してから捨てる。
        let tx = db.Begin()
        let tx2 = exec tx "INSERT INTO people VALUES (99, 'ghost', 1)"
        Assert.True(tx2.InTransaction)

        // `Close` しないので、幽霊行は WAL にしか無い。
        let reopened = ok (Database.openWith dir)
        let count = query reopened "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt count.Rows.[0].[0])

        ok (reopened.Close()))

// ---------------------------------------------------------------------------
// MVCC 分離
// ---------------------------------------------------------------------------

[<Fact>]
let ``E2E: 別ハンドルの書き込みは既存 transaction に見えない`` () =
    withDir "mvcc" (fun dir ->
        let db = seedLibrary (ok (Database.openWith dir))

        // 読み取り専用 transaction を開く（`Active` だけ立てる）。
        let reader = db.Begin()
        Assert.True(reader.InTransaction)

        // 別のハンドルから書き込む。
        let writer = exec db "INSERT INTO people VALUES (5, 'erin', 41)"

        // reader は commit していないので、新しい行は見えない。
        let before = query reader "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt before.Rows.[0].[0])

        // 書き込んだ側からは見える。
        let after = query writer "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(5L, asInt after.Rows.[0].[0])

        ok (writer.Close()))

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
