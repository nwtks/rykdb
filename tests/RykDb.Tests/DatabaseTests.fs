// ===========================================================================
// Database.fs のテスト: ハンドル API・トランザクション機構・永続性・性能
//
// ライブラリ API（Begin / Commit / Rollback / Close）と、内部機構
// （MVCC / WAL / プロセスロック / 索引の backfill・後始末 / snapshot 登録）を
// 検証する。SQL の構文・意味論は SqlReferenceTests.fs にある。
// ===========================================================================

module RykDb.DatabaseTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// ライブラリ API で DB を仕込む（E2E で共通）。
let private seedLibrary (db: Database) =
    db
    |> fun d -> exec d "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER)"
    |> fun d -> exec d "INSERT INTO people VALUES (1, 'alice', 30), (2, 'bob', 25), (3, 'carol', 35), (4, 'dave', 25)"
    |> fun d -> exec d "CREATE TABLE orders (oid INTEGER PRIMARY KEY, person_id INTEGER, amount INTEGER)"
    |> fun d -> exec d "INSERT INTO orders VALUES (1, 1, 100), (2, 1, 200), (3, 2, 50), (4, 3, 70), (5, 3, 20)"
    |> fun d -> exec d "CREATE INDEX idx_people_name ON people (name)"
    |> fun d -> exec d "CREATE INDEX idx_orders_pid ON orders (person_id)"

/// 既に開いているハンドルを「クラッシュで失った」ことにして開き直す。
/// 未 commit の transaction / WAL 再生の検証に使う。
let private openWithTakingOver (root: string) =
    result {
        let! store = RowStore.openStoreTakingOver root
        return! Database.OpenWithStore root store
    }

/// transaction を開始してハンドルを捨てる（呼び出し側は戻り値しか持たない）。
/// `NoInlining` は Debug ビルドのローカルがメソッド末尾まで生き残るのを避けるため。
/// 戻り値は「開始直後は確かに登録されていた」こと。
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private abandonTransaction (db: Database) =
    let tx = ok (db.Begin())
    let registered = (ActiveSnapshotRegistry.oldestExcept db.Root None).IsSome
    // tx はここを抜けると到達不能になる（finalizer が登録を外せる）。
    registered && tx.InTransaction

/// finalizer を確実に走らせる。
let private collectGarbage () =
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()

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

        let _db11, res = ok (db5.Execute "SELECT id FROM t")
        Assert.Equal<int>(2, res.Rows.Length))

[<Fact>]
let ``E2E: commit してない transaction は残らない`` () =
    withDir "crash2" (fun dir ->
        let db = seedLibrary (ok (Database.OpenWith dir))

        // 明示 transaction を開いて write してから捨てる。
        let tx = ok (db.Begin())
        let tx2 = exec tx "INSERT INTO people VALUES (99, 'ghost', 1)"
        Assert.True tx2.InTransaction

        // `Close` しないので、幽霊行は WAL にしか無い。
        // クラッシュでハンドルを失った状況を模すため、二重オープンする。
        let reopened = ok (openWithTakingOver dir)
        let count = query reopened "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt count.Rows.[0].[0])

        ok (reopened.Close()))

[<Fact>]
let ``E2E: 別ハンドルの書き込みは既存 transaction に見えない`` () =
    withDir "mvcc" (fun dir ->
        let db = seedLibrary (ok (Database.OpenWith dir))

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

        let _db12, res = ok (db5.Execute "SELECT id FROM t")
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
let ``Database: 複数文 Execute の BEGIN 後エラーで登録を残さない`` () =
    withDb "tx-begin-error-cleanup" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        // BEGIN で作られた ActiveSnapshot 登録は、この Execute がエラーになると
        // どの Database ハンドルにも紐づかない。残ると DDL が永久に ConflictError になる。
        expectError "BEGIN 後の途中エラー" (db1.Execute "BEGIN; INSERT INTO t VALUES (1, 10); SELECT * FROM nope")

        // 登録が残っていれば CREATE INDEX は ConflictError になる。
        let db2 = exec db1 "CREATE INDEX ix_v ON t (v)"
        // 途中の INSERT は commit されていない。
        let _, count = ok (db2.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt count.Rows.[0].[0])

        let db3, _ = ok (db2.Execute "INSERT INTO t VALUES (2, 20)")
        let _, found = ok (db3.Execute "SELECT id FROM t WHERE v = 20")
        Assert.Equal<int>(1, found.Rows.Length))

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

[<Fact>]
let ``E2E: 明示 transaction 内の書き込みが SELECT に見える`` () =
    withDb "tx6" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let tx0 = ok (db1.Begin())
        let tx1, _ = ok (tx0.Execute "INSERT INTO t VALUES (1, 10)")

        let tx2, added = ok (tx1.Execute "SELECT id, v FROM t")
        Assert.Equal<int>(1, added.Rows.Length)
        Assert.Equal<int64>(1L, asInt added.Rows.[0].[0])
        Assert.Equal<int64>(10L, asInt added.Rows.[0].[1])

        let tx3, _ = ok (tx2.Execute "UPDATE t SET v = 20 WHERE id = 1")
        let tx4, updated = ok (tx3.Execute "SELECT v FROM t")
        Assert.Equal<int64>(20L, asInt updated.Rows.[0].[0])

        let tx5, _ = ok (tx4.Execute "DELETE FROM t WHERE id = 1")
        let tx6, deleted = ok (tx5.Execute "SELECT id FROM t")
        Assert.Equal<int>(0, deleted.Rows.Length)

        let committed = ok (tx6.Commit())
        let _, after = ok (committed.Execute "SELECT id FROM t")
        Assert.Equal<int>(0, after.Rows.Length))

[<Fact>]
let ``E2E: 既存行がある transaction 内の INSERT は SELECT に全行見える`` () =
    withDb "tx-overlay-insert" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 10), (3, 30), (5, 50)")

        let tx0 = ok (db2.Begin())
        let tx1, _ = ok (tx0.Execute "INSERT INTO t VALUES (0, 0), (2, 20), (4, 40)")
        let tx2, rows = ok (tx1.Execute "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 0L; 1L; 2L; 3L; 4L; 5L |], rows.Rows |> Array.map (fun r -> asInt r.[0]))

        let committed = ok (tx2.Commit())
        let _, after = ok (committed.Execute "SELECT id FROM t ORDER BY id")
        Assert.Equal<int64 array>([| 0L; 1L; 2L; 3L; 4L; 5L |], after.Rows |> Array.map (fun r -> asInt r.[0])))

[<Fact>]
let ``E2E: 既存行がある transaction 内の複数 UPDATE が正しい`` () =
    withDb "tx-overlay-update" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)")

        let tx0 = ok (db2.Begin())
        let tx1, _ = ok (tx0.Execute "UPDATE t SET v = 100 WHERE id = 1")
        let tx2, _ = ok (tx1.Execute "UPDATE t SET v = 300 WHERE id = 3")

        let tx3, rows = ok (tx2.Execute "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<int>(3, rows.Rows.Length)
        Assert.Equal<int64>(1L, asInt rows.Rows.[0].[0])
        Assert.Equal<int64>(100L, asInt rows.Rows.[0].[1])
        Assert.Equal<int64>(2L, asInt rows.Rows.[1].[0])
        Assert.Equal<int64>(20L, asInt rows.Rows.[1].[1])
        Assert.Equal<int64>(3L, asInt rows.Rows.[2].[0])
        Assert.Equal<int64>(300L, asInt rows.Rows.[2].[1])

        let committed = ok (tx3.Commit())
        let _, after = ok (committed.Execute "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<int64>(100L, asInt after.Rows.[0].[1])
        Assert.Equal<int64>(20L, asInt after.Rows.[1].[1])
        Assert.Equal<int64>(300L, asInt after.Rows.[2].[1]))

[<Fact>]
let ``E2E: 既存行がある transaction 内の索引検索が書き込みを見る`` () =
    withDb "tx-overlay-index" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_t_v ON t (v)"
        let db3, _ = ok (db2.Execute "INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)")

        let tx0 = ok (db3.Begin())
        let tx1, _ = ok (tx0.Execute "INSERT INTO t VALUES (4, 40)")
        let tx2, added = ok (tx1.Execute "SELECT id FROM t WHERE v = 40")
        Assert.Equal<int>(1, added.Rows.Length)
        Assert.Equal<int64>(4L, asInt added.Rows.[0].[0])

        let tx3, _ = ok (tx2.Execute "DELETE FROM t WHERE id = 2")
        let tx4, deleted = ok (tx3.Execute "SELECT id FROM t WHERE v = 20")
        Assert.Equal<int>(0, deleted.Rows.Length)

        let tx5, _ = ok (tx4.Execute "UPDATE t SET v = 35 WHERE id = 3")
        let tx6, oldKey = ok (tx5.Execute "SELECT id FROM t WHERE v = 30")
        Assert.Equal<int>(0, oldKey.Rows.Length)

        let _, newKey = ok (tx6.Execute "SELECT id FROM t WHERE v = 35")
        Assert.Equal<int>(1, newKey.Rows.Length))

[<Fact>]
let ``E2E: 明示 transaction 内の索引検索が書き込みを見る`` () =
    withDb "tx7" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_t_v ON t (v)"
        let tx0 = ok (db2.Begin())
        let tx1, _ = ok (tx0.Execute "INSERT INTO t VALUES (1, 7)")
        let tx2, found = ok (tx1.Execute "SELECT id FROM t WHERE v = 7")
        Assert.Equal<int>(1, found.Rows.Length)

        let tx3, _ = ok (tx2.Execute "UPDATE t SET v = 8 WHERE id = 1")
        let tx4, oldKey = ok (tx3.Execute "SELECT id FROM t WHERE v = 7")
        Assert.Equal<int>(0, oldKey.Rows.Length)

        let _, newKey = ok (tx4.Execute "SELECT id FROM t WHERE v = 8")
        Assert.Equal<int>(1, newKey.Rows.Length))

// ---------------------------------------------------------------------------
// ハンドル API・索引の backfill・DROP の後始末・ストレージ破損
// ---------------------------------------------------------------------------

/// 最新 SSTable の先頭バイトを壊し、mtime を進める。
/// `openDatabase` は MaxSeqNo の復元でデータブロックを読まないため、open 後に呼ぶ。
let private corruptNewestTableSstable (dir: string) =
    let newest =
        System.IO.Directory.GetFiles(dir, "*" + FileMetaData.SstExtension)
        |> Array.maxBy (fun path -> System.Int64.Parse(System.IO.Path.GetFileNameWithoutExtension path))

    let bytes = System.IO.File.ReadAllBytes newest
    bytes.[0] <- bytes.[0] ^^^ 0xFFuy
    System.IO.File.WriteAllBytes(newest, bytes)
    System.IO.File.SetLastWriteTimeUtc(newest, System.DateTime.UtcNow.AddSeconds 1.0)

[<Fact>]
let ``Database: Root はルートディレクトリを返す`` () =
    withDir "rootprop" (fun dir ->
        let db = ok (Database.OpenWith dir)
        Assert.Equal(dir, db.Root)
        ok (db.Close()))

[<Fact>]
let ``Database: LIMIT 1 は後続の壊れた SSTable を開かない`` () =
    withDir "limit-pushdown" (fun dir ->
        // 2回に分けて Close し、テーブル t の SSTable を 2 個作る。
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"
        ok (db2.Close())

        let db3 = ok (Database.OpenWith dir)
        let db4 = exec db3 "INSERT INTO t VALUES (3), (4)"
        ok (db4.Close())

        // openDatabase は MaxSeqNo の復元で全 SSTable を読むため、open 後に壊す。
        let db5 = ok (Database.OpenWith dir)
        corruptNewestTableSstable dir

        // LIMIT 1 は先頭 1行だけなので、キー 3 以降の壊れた SSTable を開かない。
        let _, one = ok (db5.Execute "SELECT id FROM t LIMIT 1")
        Assert.Equal<int>(1, one.Rows.Length)
        Assert.Equal<int64>(1L, asInt one.Rows.[0].[0])

        // 全行が必要なら壊れた SSTable を開くので CorruptionError。
        expectErrorKind ErrorKind.CorruptionError "LIMIT 10 は壊れた SSTable を開く" (db5.Execute "SELECT id FROM t LIMIT 10")

        ok (db5.Close()))

[<Fact>]
let ``Database: 主キー順の ORDER BY は Sort を作らず LIMIT がストレージへ降りる`` () =
    withDir "orderby-pk-pushdown" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "INSERT INTO t VALUES (1), (2)"
        ok (db2.Close())

        let db3 = ok (Database.OpenWith dir)
        let db4 = exec db3 "INSERT INTO t VALUES (3), (4)"
        ok (db4.Close())

        let db5 = ok (Database.OpenWith dir)
        corruptNewestTableSstable dir

        // 主キー昇順の ORDER BY は Sort ノードを作らないので、
        // LIMIT の件数ヒントが走査まで届き、壊れた SSTable を開かない。
        let _, one = ok (db5.Execute "SELECT id FROM t ORDER BY id LIMIT 1")
        Assert.Equal<int64>(1L, asInt one.Rows.[0].[0])

        // 別名越しの ORDER BY も列参照に解決されるので同じ。
        let _, aliased = ok (db5.Execute "SELECT id AS k FROM t ORDER BY k LIMIT 1")
        Assert.Equal<int64>(1L, asInt aliased.Rows.[0].[0])

        // 降順は Sort が必要で、ソートは全入力を読むため CorruptionError。
        expectErrorKind
            ErrorKind.CorruptionError
            "ORDER BY id DESC は全件を読む"
            (db5.Execute "SELECT id FROM t ORDER BY id DESC LIMIT 1")

        ok (db5.Close()))

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
        let _, found = ok (db3.Execute "SELECT id FROM t WHERE a = 7")
        Assert.Equal<int>(2, found.Rows.Length))

[<Fact>]
let ``Database: 大量行の CREATE INDEX はチャンクで backfill する`` () =
    withDb "ddl-index-chunks" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"

        let db2 =
            [ 1..3 ]
            |> List.fold
                (fun db block ->
                    let start = (block - 1) * 1000 + 1
                    let end' = block * 1000

                    let values =
                        [ start..end' ]
                        |> List.map (fun i -> sprintf "(%d, %d)" i (i % 10))
                        |> String.concat ","

                    exec db (sprintf "INSERT INTO t VALUES %s" values))
                db1

        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        let _, count = ok (db3.Execute "SELECT COUNT(*) FROM t WHERE a = 7")
        Assert.Equal<int64>(300L, asInt count.Rows.[0].[0])

        // UNIQUE 検査も全件 Set を保持せず、索引走査で行う。
        let db4 = exec db3 "CREATE UNIQUE INDEX ix_id ON t (id)"
        let _, found = ok (db4.Execute "SELECT COUNT(*) FROM t WHERE id = 1000")
        Assert.Equal<int64>(1L, asInt found.Rows.[0].[0])

        // 同様に負荷の高い DROP TABLE もチャンクで後始末できる。
        let db5 = exec db4 "DROP TABLE t"
        let db6 = exec db5 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let _, after = ok (db6.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt after.Rows.[0].[0]))

[<Fact>]
let ``Database: CREATE INDEX は残った索引エントリを消してから backfill する`` () =
    withDir "create-index-stale" (fun dir ->
        // 前回の失敗などで索引 keyspace にだけ残った行を用意する。
        let staleKey =
            KeyBuilder.ofIndexAndPrimary [| SqlValue.Int 999L |] [| SqlValue.Int 999L |]

        let staleRow = [| SqlValue.Int 999L; SqlValue.Int 999L |]
        let store = ok (RowStore.openStore dir)

        let store =
            ok (RowStore.writeBatches [ (Keyspace.index "ix_a", [| Entry.put staleKey 1UL staleRow |]) ] store)

        ok (RowStore.close store)

        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "CREATE INDEX ix_a ON t (a)"
        let _, rows = ok (db2.Execute "SELECT id FROM t WHERE a = 999")
        Assert.Empty rows.Rows
        ok (db2.Close()))

[<Fact>]
let ``Database: UNIQUE 索引の backfill は複数の NULL を許す`` () =
    withDb "unique-null-backfill" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, NULL), (2, NULL)"
        let db3 = exec db2 "CREATE UNIQUE INDEX ix_a ON t (a)"
        let _, count = ok (db3.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``Database: DROP TABLE 後の同名再作成で旧データが復活しない`` () =
    withDb "drop-table-recreate" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 'old'), (2, 'old')")
        let db3 = exec db2 "DROP TABLE t"
        let db4 = exec db3 "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)"
        let _, res = ok (db4.Execute "SELECT COUNT(*) FROM t")
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
        let _, res = ok (db6.Execute "SELECT id FROM t WHERE g = 10")
        Assert.Equal<int>(0, res.Rows.Length))

[<Fact>]
let ``Database: DROP TABLE は索引 keyspace の孤児エントリも消す`` () =
    withDir "drop-table-stale" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10)"
        let db3 = exec db2 "CREATE INDEX ix_g ON t (g)"
        ok (db3.Close())

        // 前回の失敗などで索引 keyspace にだけ残った行を追加する。
        let staleKey =
            KeyBuilder.ofIndexAndPrimary [| SqlValue.Int 999L |] [| SqlValue.Int 999L |]

        let store = ok (RowStore.openStore dir)

        let store =
            ok (
                RowStore.writeBatches
                    [
                        (Keyspace.index "ix_g",
                         [| Entry.put staleKey 1000UL [| SqlValue.Int 999L; SqlValue.Int 999L |] |])
                    ]
                    store
            )

        ok (RowStore.close store)

        let db4 = ok (Database.OpenWith dir)
        let db5 = exec db4 "DROP TABLE t"
        ok (db5.Close())

        let store = ok (RowStore.openStore dir)

        let indexRows =
            ok (RowStore.scan (Keyspace.index "ix_g") [||] 0 store) |> SstScan.toArray |> ok

        let tableRows =
            ok (RowStore.scan (Keyspace.table "t") [||] 0 store) |> SstScan.toArray |> ok

        Assert.Empty indexRows
        Assert.Empty tableRows
        ok (RowStore.close store))

[<Fact>]
let ``Database: 中断した DROP の後始末を open 時に再開する`` () =
    withDir "drop-cleanup-recover" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let db2 = exec db1 "INSERT INTO t VALUES (1, 10)"
        let db3 = exec db2 "CREATE INDEX ix_a ON t (a)"
        ok (db3.Close())

        // カタログ削除直後・keyspace 後始末前にクラッシュした状態を作る。
        let store = ok (RowStore.openStore dir)
        let previous = ok (Catalog.load store)
        let next = Catalog.removeTable previous "t"
        let store = ok (Catalog.save store previous next)
        ok (DropCleanup.write store.Root [ Keyspace.table "t"; Keyspace.index "ix_a" ])
        ok (RowStore.close store)

        // open 時にジャーナルを検出して後始末を再開する。
        let db4 = ok (Database.OpenWith dir)
        Assert.Empty(db4.TableNames())
        ok (db4.Close())

        let store = ok (RowStore.openStore dir)

        let tableRows =
            ok (RowStore.scan (Keyspace.table "t") [||] 0 store) |> SstScan.toArray |> ok

        let indexRows =
            ok (RowStore.scan (Keyspace.index "ix_a") [||] 0 store) |> SstScan.toArray |> ok

        Assert.Empty tableRows
        Assert.Empty indexRows
        Assert.Empty(ok (DropCleanup.pending store.Root))
        ok (RowStore.close store))

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
        let _, gone = ok (db6.Execute "SELECT id FROM t WHERE g = 10")
        Assert.Equal<int>(0, gone.Rows.Length)

        let _, alive = ok (db6.Execute "SELECT id FROM t WHERE g = 20")
        Assert.Equal<int>(1, alive.Rows.Length)
        Assert.Equal<int64>(2L, asInt alive.Rows.[0].[0]))

// ===========================================================================
// E2E（通し経路）・性能・末尾再帰
// ===========================================================================

// ---------------------------------------------------------------------------
// ライブラリ API の E2E
// ---------------------------------------------------------------------------

[<Fact>]
let ``E2E: 一連の流れが誤差なく通る`` () =
    withDir "flow" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
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
        // 10000行。実用時間内で終わることを確かめる。
        let rowCount = 10000

        let db =
            exec (ok (Database.OpenWith dir)) "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"

        // 複数行 INSERT をまとめて流す。
        let sw = System.Diagnostics.Stopwatch.StartNew()
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
        let sw2 = System.Diagnostics.Stopwatch.StartNew()
        let mutable probe = 0L

        for id in [ 1..500 ] do
            let r = query current (sprintf "SELECT v FROM t WHERE id = %d" id)

            if r.Rows.Length = 1 then
                probe <- probe + asInt r.Rows.[0].[0]

        let lookupMs = sw2.ElapsedMilliseconds

        // range scan（集計で全行を走らせる）
        let sw3 = System.Diagnostics.Stopwatch.StartNew()
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
let ``末尾再帰: 60 段の入れ子 JOIN が正しい行を返す`` () =
    // plan の深さは JOIN の数だけ伸びる（サブクエリ未対応なので Join が唯一の源泉）。
    // exec は descend（積む側）/ unwind（回す側）で末尾化済みだが、
    // 左へ下ろす「右テーブルを除いたスキーマ」が段をまたいで正しく累積するかは
    // 深さ方向で検証する。
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
        let _, res = ok (db2.Execute sql)
        // id = 1 と 2 の 2 行が全段で一致する（エラーで 0 行になっていないこと）。
        Assert.Equal<int>(2, res.Rows.Length)
        Assert.Equal<int>(2, res.Columns.Length)
        Assert.Equal<int64>(0L, asInt res.Rows.[0].[0])
        Assert.Equal<int64>(int64 (n - 1), asInt res.Rows.[0].[1])
        Assert.Equal<int64>(0L, asInt res.Rows.[1].[0])
        Assert.Equal<int64>(int64 (n - 1), asInt res.Rows.[1].[1]))

[<Fact>]
let ``末尾再帰: 大量の文を 1 度の Execute で実行できる`` () =
    // Execute の runStatements は `runStatement … |> Result.bind (fun … -> runStatements …)` で
    // 文の数だけフレームが積まれていた（→ StackOverflow / exit 134）。
    // `match` で受けて末尾再帰にしてある。
    let n = 2_000

    withDb "tail-exec" (fun db0 ->
        let db = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)"
        let sql = System.String.Join(";", Array.init n (fun _ -> "SELECT a FROM t"))
        let next, _ = ok (db.Execute sql)
        Assert.Equal<string list>([ "t" ], next.TableNames()))

[<Fact>]
let ``末尾再帰: 多数の列を持つ INSERT / UPDATE が完了する`` () =
    // 値リストの評価（gatherWith）は `evalOne … >>= fun … -> coerce …
    // >>= fun … -> gather …` で列の数だけフレームが積まれていた
    // （→ StackOverflow / exit 134）。`match` で受けて末尾再帰にしてある。
    let cols = 2_000
    let names = Array.init cols (fun i -> sprintf "c%d" i)

    // 主キーは必須（無いと CREATE TABLE 自体がエラーになる）。
    let decls =
        names
        |> Array.mapi (fun i c -> sprintf "%s INTEGER%s" c (if i = 0 then " PRIMARY KEY" else ""))
        |> String.concat ", "

    let colList = String.concat ", " names
    let values = Array.init cols string |> String.concat ", "

    withDb "tail-gather" (fun db0 ->
        let db1 = exec db0 (sprintf "CREATE TABLE wide (%s)" decls)
        let db2 = exec db1 (sprintf "INSERT INTO wide (%s) VALUES (%s)" colList values)
        let inserted = query db2 "SELECT * FROM wide"
        Assert.Equal(1, inserted.Rows.Length)
        Assert.Equal(cols, inserted.Columns.Length)

        // UPDATE も同じ経路（SET の数だけ gather が回る）。SET の列数は
        // パーサの assigns にも効くので、こちらも末尾再帰にしてある。
        // 主キー（c0）は書き換えない（主キー更新は既知の別件なので外す）。
        let sets =
            Array.init (cols - 1) (fun i -> sprintf "c%d = %d" (i + 1) (i + 2))
            |> String.concat ", "

        let db3 = exec db2 (sprintf "UPDATE wide SET %s" sets)
        let updated = query db3 (sprintf "SELECT c0, c%d FROM wide" (cols - 1))
        Assert.Equal(1, updated.Rows.Length)
        Assert.Equal<int64>(0L, asInt updated.Rows.[0].[0])
        Assert.Equal<int64>(int64 cols, asInt updated.Rows.[0].[1]))

// ---------------------------------------------------------------------------
// 再オープン（OpenWith / Close）: 永続性とクラッシュ耐性
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: 閉じて開き直すとデータが残る`` () =
    withDir "i-reopen" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)"
        let db2, _ins = ok (db1.Execute "INSERT INTO t VALUES (1, 'kept')")
        ok (db2.Close())

        // 開き直す
        let db3 = ok (Database.OpenWith dir)
        Assert.Equal<string list>([ "t" ], db3.TableNames())

        let _db4, res = ok (db3.Execute "SELECT v FROM t")
        Assert.Equal<int>(1, res.Rows.Length)

        let cell: SqlValue = res.Rows.[0].[0]
        let actual: string = asText cell
        Assert.Equal<string>("kept", actual)

        ok (db3.Close()))

[<Fact>]
let ``Database: カタログ破損で開くのに失敗してもロックを解放する`` () =
    withDir "catalog-corrupt-lock" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        ok (db1.Close())

        let store = ok (RowStore.openStore dir)

        let store =
            ok (
                RowStore.writeBatches
                    [
                        (Keyspace.CatalogKeyspace,
                         [|
                             Entry.put
                                 (System.Text.Encoding.UTF8.GetBytes "table:broken")
                                 1000UL
                                 [| SqlValue.Blob [||] |]
                         |])
                    ]
                    store
            )

        ok (RowStore.close store)

        expectError "壊れたカタログを読めてしまう" (Database.OpenWith dir)

        // 失敗した open がロックを残していると、ここで IoError になる。
        let store = ok (RowStore.openStore dir)
        ok (RowStore.close store))

[<Fact>]
let ``Database: 古いハンドルを Close しても新しいハンドルの commit は失われない`` () =
    withDir "stale-close" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1)")
        let db3, _ = ok (db2.Execute "INSERT INTO t VALUES (2)")

        let before = query db3 "SELECT COUNT(*) FROM t"
        Assert.Equal<int64>(2L, asInt before.Rows.[0].[0])

        // db2 は db3 より前の状態。古いハンドルの Close で WAL を消してはいけない。
        ok (db2.Close())

        // 古いハンドルの Close は、生きている db3 のプロセスロックも解放しない。
        expectErrorKind ErrorKind.IoError "古いハンドルの Close で再オープンできてしまった" (Database.OpenWith dir)

        // 最新ハンドルを閉じれば WAL もロックも解放され、再オープンできる。
        ok (db3.Close())
        let reopened = ok (Database.OpenWith dir)
        let after = query reopened "SELECT COUNT(*) FROM t"
        Assert.Equal<int64>(2L, asInt after.Rows.[0].[0])
        ok (reopened.Close()))

[<Fact>]
let ``E2E: プロセス再起動を模して Disk から復元できる`` () =
    withDir "reopen" (fun dir ->
        let db = seedLibrary (ok (Database.OpenWith dir))
        ok (db.Close())

        // 別ハンドルを「別プロセス」として扱う。
        let reopened = ok (Database.OpenWith dir)
        // `TableNames` の並び順は保証されない（keyspace のディレクトリ順）のでソートする。
        Assert.Equal<string list>([ "orders"; "people" ], reopened.TableNames() |> List.sort)

        let people = query reopened "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt people.Rows.[0].[0])

        // 索引が「作られる前」に入った行も引ける（backfill の検証）
        let byIndex = query reopened "SELECT id FROM people WHERE name = 'carol'"
        Assert.Equal<int>(1, byIndex.Rows.Length)
        Assert.Equal<int64>(3L, asInt byIndex.Rows.[0].[0])

        ok (reopened.Close()))

[<Fact>]
let ``E2E: Close しなくても commit 済みの行は残る`` () =
    withDir "crash" (fun dir ->
        let _db = seedLibrary (ok (Database.OpenWith dir))
        // Close を呼ばない（memtable は SSTable 未落下、残るのは WAL 再生のみ）。
        let reopened = ok (openWithTakingOver dir)

        let count = query reopened "SELECT COUNT(*) FROM people"
        Assert.Equal<int64>(4L, asInt count.Rows.[0].[0])

        ok (reopened.Close()))

// ---------------------------------------------------------------------------
// snapshot registry・プロセスロック・DDL の排他
// ---------------------------------------------------------------------------

[<Fact>]
let ``Database: active transaction は snapshot registry に登録され、commit / rollback / Close で外れる`` () =
    withDir "reg-lifecycle" (fun dir ->
        let db = ok (Database.OpenWith dir)
        let root = db.Root
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsNone)

        // 明示 transaction: Begin で登録、Commit で解除
        let tx = ok (db.Begin())
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsSome)
        let committed = ok (tx.Commit())
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsNone)

        // Rollback でも解除
        let tx2 = ok (committed.Begin())
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsSome)
        let rolledBack = ok (tx2.Rollback())
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsNone)

        // SQL の BEGIN / COMMIT 経由でも同じ
        let sqlTx, _ = ok (rolledBack.Execute "BEGIN")
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsSome)
        let sqlDone, _ = ok (sqlTx.Execute "COMMIT")
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsNone)

        // 暗黙 transaction は Execute の中で登録・解除が完結する
        let withTable, _ = ok (sqlDone.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)")
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsNone)

        // Close でも解除される（閉じたハンドルの snapshot は誰も読めない）
        let openTx = ok (withTable.Begin())
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsSome)
        ok (openTx.Close())
        Assert.True((ActiveSnapshotRegistry.oldestExcept root None).IsNone))

[<Fact>]
let ``Database: commit せずに捨てた transaction も GC されれば登録が外れる`` () =
    withDir "reg-abandon" (fun dir ->
        let db = ok (Database.OpenWith dir)
        let withTable = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        Assert.True(abandonTransaction withTable)

        collectGarbage ()
        // 最後のハンドルが消えたので登録が外れる（修正前はここに残り続けた）。
        Assert.True((ActiveSnapshotRegistry.oldestExcept withTable.Root None).IsNone)

        // 登録が残っていると ConflictError になる DDL が通る。
        let indexed = exec withTable "CREATE INDEX ix_v ON t (v)"
        let inserted = exec indexed "INSERT INTO t VALUES (1, 10)"
        let _, found = ok (inserted.Execute "SELECT id FROM t WHERE v = 10")
        Assert.Equal<int>(1, found.Rows.Length)

        ok (inserted.Close()))

[<Fact>]
let ``Database: 生きている transaction は GC されても登録が残る`` () =
    withDir "reg-live-gc" (fun dir ->
        let db = ok (Database.OpenWith dir)
        let tx = ok (db.Begin())
        // tx はこのあと Rollback で使うので GC されない（finalizer は走らない）。
        collectGarbage ()
        Assert.True((ActiveSnapshotRegistry.oldestExcept db.Root None).IsSome)

        let rolled = ok (tx.Rollback())
        Assert.True((ActiveSnapshotRegistry.oldestExcept db.Root None).IsNone)
        ok (rolled.Close()))

[<Fact>]
let ``Database: Dispose でも transaction の登録が外れる`` () =
    withDir "reg-dispose" (fun dir ->
        let db = ok (Database.OpenWith dir)
        let tx = ok (db.Begin())
        Assert.True((ActiveSnapshotRegistry.oldestExcept db.Root None).IsSome)

        (tx :> System.IDisposable).Dispose()
        Assert.True((ActiveSnapshotRegistry.oldestExcept db.Root None).IsNone))

[<Fact>]
let ``Database: 別プロセスが開いている DB は開けない`` () =
    withDir "proc-lock" (fun dir ->
        // 別プロセスが `FileShare.None` でロックを押さえている状態（同一プロセスの別 fd でも同じ扱い）。
        use _held =
            new System.IO.FileStream(
                System.IO.Path.Combine(dir, ".rykdb.lock"),
                System.IO.FileMode.OpenOrCreate,
                System.IO.FileAccess.ReadWrite,
                System.IO.FileShare.None
            )

        expectErrorKind ErrorKind.IoError "別プロセスが開いている DB は開けない" (Database.OpenWith dir))

[<Fact>]
let ``Database: 同じ root を二度開くと同じプロセスのエラーになり、Close 後は開ける`` () =
    withDir "proc-lock-share" (fun dir ->
        let lockPath = System.IO.Path.Combine(dir, ".rykdb.lock")

        let canLockExternally () =
            try
                use fs =
                    new System.IO.FileStream(
                        lockPath,
                        System.IO.FileMode.OpenOrCreate,
                        System.IO.FileAccess.ReadWrite,
                        System.IO.FileShare.None
                    )

                true
            with :? System.IO.IOException ->
                false

        let db1 = ok (Database.OpenWith dir)
        Assert.False(canLockExternally ())
        // 同じ root を二度開くと、memtable が分かれて古い行を読むので拒否する。
        expectErrorKind ErrorKind.IoError "同じ root の二重オープン" (Database.OpenWith dir)

        // Close すればロックも外れ、開き直せる。
        ok (db1.Close())
        Assert.True(canLockExternally ())

        let db2 = ok (Database.OpenWith dir)
        ok (db2.Close()))

[<Fact>]
let ``Database: 明示 transaction があるハンドルは Close でロックを外す`` () =
    withDir "proc-lock-txn" (fun dir ->
        let _ = System.IO.Path.Combine(dir, ".rykdb.lock")
        let db = ok (Database.OpenWith dir)
        let tx = ok (db.Begin())
        // 進行中 transaction を持つハンドル（元の db ではない方）を閉じてもロックは外れる。
        ok (tx.Close())
        ok (Database.OpenWith dir) |> fun d -> ok (d.Close()))

[<Fact>]
let ``Database: 古いハンドルの Close は生きている新しいハンドルのロックを解放しない`` () =
    withDir "old-close-lock" (fun dir ->
        let db0 = ok (Database.OpenWith dir)
        let db1 = exec db0 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)"
        let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 10)")
        let db3, _ = ok (db2.Execute "INSERT INTO t VALUES (2, 20)")

        // db0 は最新ではない。ここでロックを外すと、db3 が生きているのに
        // 同じ root を別 store で開けてしまう。
        ok (db0.Close())
        expectErrorKind ErrorKind.IoError "古いハンドルの Close でロックが外れてしまった" (Database.OpenWith dir)

        // 新しいハンドルはそのまま使える。
        let db4, counted = ok (db3.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt counted.Rows.[0].[0])

        // 最新ハンドルを閉じれば解放される。
        ok (db4.Close())
        let reopened = ok (Database.OpenWith dir)
        let _, after = ok (reopened.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt after.Rows.[0].[0])
        ok (reopened.Close()))

[<Fact>]
let ``Database: 実行中の transaction があると DDL は ConflictError`` () =
    withDir "ddl-conflict" (fun dir ->
        let db1 = ok (Database.OpenWith dir)
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
        let _, found = ok (inserted.Execute "SELECT id FROM t WHERE v = 20")
        Assert.Equal<int>(1, found.Rows.Length)

        ok (inserted.Close())
        ok (rolled.Close()))
