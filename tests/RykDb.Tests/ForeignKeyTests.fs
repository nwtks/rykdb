module RykDb.ForeignKeyTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テーブルの行数を数える。
let private countOf (db: Session) (table: string) =
    let _, result = ok (execSql db (sprintf "SELECT COUNT(*) FROM %s" table))
    asInt result.Rows.[0].[0]

/// 親 p / 子 c（c.pid -> p.id）を作る。
let private withParentChild (tag: string) (f: Session -> unit) =
    withSession tag (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY, name TEXT)"

        let db2 =
            exec db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES p (id))"

        f db2)

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 参照先の列を省略すると親の主キーになる`` () =
    withSession "fk-resolve" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY, name TEXT)"

        let db2 =
            exec db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES p)"

        let db3 = exec db2 "INSERT INTO p VALUES (1, 'a')"
        let db4 = exec db3 "INSERT INTO c VALUES (10, 1)"
        Assert.Equal<int64>(1L, countOf db4 "c")
        expectErrorKind ErrorKind.ConstraintViolation "親の無い値を挿入できました" (execSql db4 "INSERT INTO c VALUES (11, 2)"))

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 参照先テーブルが無いと NameError`` () =
    withSession "fk-no-parent" (fun db0 ->
        expectErrorKind
            ErrorKind.NameError
            "存在しない参照先を受け付けました"
            (execSql db0 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES nope (id))"))

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 主キー以外は参照できない`` () =
    withSession "fk-not-pk" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY, u TEXT)"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "主キー以外を参照できました"
            (execSql db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES p (u))"))

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 列の型が違うと TypeError`` () =
    withSession "fk-type" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY)"

        expectErrorKind
            ErrorKind.TypeError
            "型の違う外部キーを作成できました"
            (execSql db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid TEXT REFERENCES p (id))"))

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 子の列が無いと NameError`` () =
    withSession "fk-no-column" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY)"

        expectErrorKind
            ErrorKind.NameError
            "存在しない子の列を受け付けました"
            (execSql db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, FOREIGN KEY (nope) REFERENCES p (id))"))

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 外部キーの列の重複は NameError`` () =
    withSession "fk-dup-column" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY)"

        expectErrorKind
            ErrorKind.NameError
            "重複した外部キーの列を受け付けました"
            (execSql db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER, FOREIGN KEY (x, x) REFERENCES p (id))"))

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 列数が合わないと ConstraintViolation`` () =
    withSession "fk-width" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "列数の合わない外部キーを作成できました"
            (execSql db1 "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER, FOREIGN KEY (x) REFERENCES p)"))

[<Fact>]
let ``ForeignKey.resolveCreatedTable: 複合主キーを列の定義順で参照できる`` () =
    withSession "fk-composite" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))"

        let db2 =
            exec
                db1
                "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER, FOREIGN KEY (x, y) REFERENCES p (a, b))"

        let db3 = exec db2 "INSERT INTO p VALUES (1, 2)"
        let db4 = exec db3 "INSERT INTO c VALUES (10, 1, 2)"
        Assert.Equal<int64>(1L, countOf db4 "c")

        expectErrorKind
            ErrorKind.ConstraintViolation
            "一部だけ一致する組を挿入できました"
            (execSql db4 "INSERT INTO c VALUES (11, 1, 3)"))

[<Fact>]
let ``ForeignKey: カタログを再オープンしても定義が残る`` () =
    withDir "fk-persist" (fun dir ->
        let db1 = ok (Session.OpenWithSync(dir, SessionOptions.Default))
        let db2 = exec db1 "CREATE TABLE p (id INTEGER PRIMARY KEY)"

        let db3 =
            exec db2 "CREATE TABLE c (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES p)"

        let db4 = exec db3 "INSERT INTO p VALUES (1)"
        let db5 = exec db4 "INSERT INTO c VALUES (1, 1)"
        ok (db5.Close()) |> ignore

        let db6 = ok (Session.OpenWithSync(dir, SessionOptions.Default))

        try
            expectErrorKind
                ErrorKind.ConstraintViolation
                "再オープン後に FK が効きません"
                (execSql db6 "INSERT INTO c VALUES (2, 2)")
        finally
            ok (db6.Close()) |> ignore)

[<Fact>]
let ``ForeignKey.checkChildRow: 親の行が無い INSERT は文全体を取り消す`` () =
    withParentChild "fk-insert" (fun db0 ->
        let db1 = exec db0 "INSERT INTO p VALUES (1, 'a')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "親の無い行を挿入できました"
            (execSql db1 "INSERT INTO c VALUES (10, 1), (11, 99)")

        Assert.Equal<int64>(0L, countOf db1 "c"))

[<Fact>]
let ``ForeignKey.checkChildRow: NULL を含む外部キーは親を要求しない`` () =
    withParentChild "fk-null" (fun db0 ->
        let db1 = exec db0 "INSERT INTO c VALUES (10, NULL)"
        Assert.Equal<int64>(1L, countOf db1 "c"))

[<Fact>]
let ``ForeignKey.checkChildRow: 自分自身を参照する行を INSERT できる`` () =
    withSession "fk-self-insert" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE emp (id INTEGER PRIMARY KEY, mgr INTEGER REFERENCES emp (id))"

        // 行を積んだ後の状態で検査するので、自分の主キーを親にできる。
        let db2 = exec db1 "INSERT INTO emp VALUES (1, 1)"
        Assert.Equal<int64>(1L, countOf db2 "emp"))

[<Fact>]
let ``ForeignKey.checkChildRow: 同一文の先行行は参照できるが後続行は参照できない`` () =
    withSession "fk-self-batch" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE emp (id INTEGER PRIMARY KEY, mgr INTEGER REFERENCES emp (id))"

        // 先行行（1）は既に transaction に積まれているので見える。
        let db2 = exec db1 "INSERT INTO emp VALUES (1, NULL), (2, 1)"
        Assert.Equal<int64>(2L, countOf db2 "emp")

        // 後続行（3）はまだ書かれていないので見えない（行単位の即時検査）。
        expectErrorKind
            ErrorKind.ConstraintViolation
            "後続行を参照する INSERT が通りました"
            (execSql db2 "INSERT INTO emp VALUES (3, 4), (4, NULL)")

        Assert.Equal<int64>(2L, countOf db2 "emp"))

[<Fact>]
let ``ForeignKey.checkChildRow: UPDATE で自分自身を親にできる`` () =
    withSession "fk-self-update" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE emp (id INTEGER PRIMARY KEY, mgr INTEGER REFERENCES emp (id))"

        let db2 = exec db1 "INSERT INTO emp VALUES (1, NULL)"
        let db3 = exec db2 "UPDATE emp SET mgr = id"
        let _, result = ok (execSql db3 "SELECT mgr FROM emp")
        Assert.Equal<int64>(1L, asInt result.Rows.[0].[0]))

[<Fact>]
let ``ForeignKey.checkChildRow: INSERT ... SELECT でも検査する`` () =
    withParentChild "fk-insert-select" (fun db0 ->
        let db1 = exec db0 "INSERT INTO p VALUES (1, 'a')"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "INSERT ... SELECT が FK を検査していません"
            (execSql db1 "INSERT INTO c SELECT id + 10, id + 10 FROM p")

        Assert.Equal<int64>(0L, countOf db1 "c"))

[<Fact>]
let ``ForeignKey.checkChildRow: UPDATE で親の無い値にできない`` () =
    withParentChild "fk-update" (fun db0 ->
        let db1 = exec db0 "INSERT INTO p VALUES (1, 'a')"
        let db2 = exec db1 "INSERT INTO c VALUES (10, 1)"
        expectErrorKind ErrorKind.ConstraintViolation "親の無い値へ更新できました" (execSql db2 "UPDATE c SET pid = 2")

        let _, result = ok (execSql db2 "SELECT pid FROM c")
        Assert.Equal<int64>(1L, asInt result.Rows.[0].[0]))

[<Fact>]
let ``ForeignKey.checkParentDelete: 参照している子があると親を削除できない`` () =
    withParentChild "fk-delete" (fun db0 ->
        let db1 = exec db0 "INSERT INTO p VALUES (1, 'a')"
        let db2 = exec db1 "INSERT INTO c VALUES (10, 1)"

        expectErrorKind ErrorKind.ConstraintViolation "参照されている親を削除できました" (execSql db2 "DELETE FROM p WHERE id = 1")

        Assert.Equal<int64>(1L, countOf db2 "p"))

[<Fact>]
let ``ForeignKey.checkParentDelete: 子を削除すれば親を削除できる`` () =
    withParentChild "fk-delete-child" (fun db0 ->
        let db1 = exec db0 "INSERT INTO p VALUES (1, 'a')"
        let db2 = exec db1 "INSERT INTO c VALUES (10, 1)"
        let db3 = exec db2 "DELETE FROM c WHERE id = 10"
        let db4 = exec db3 "DELETE FROM p WHERE id = 1"
        Assert.Equal<int64>(0L, countOf db4 "p"))

[<Fact>]
let ``ForeignKey.checkParentDelete: 自己参照の行は参照が残る間は削除できない`` () =
    withSession "fk-self" (fun db0 ->
        let db1 =
            exec db0 "CREATE TABLE emp (id INTEGER PRIMARY KEY, mgr INTEGER REFERENCES emp (id))"

        let db2 = exec db1 "INSERT INTO emp VALUES (1, NULL), (2, 1)"

        expectErrorKind ErrorKind.ConstraintViolation "参照されている行を削除できました" (execSql db2 "DELETE FROM emp WHERE id = 1")

        let db3 = exec db2 "DELETE FROM emp WHERE id = 2"
        let db4 = exec db3 "DELETE FROM emp WHERE id = 1"
        Assert.Equal<int64>(0L, countOf db4 "emp"))

[<Fact>]
let ``ForeignKey.checkParentDelete: 子の索引があっても参照を検出する`` () =
    withParentChild "fk-delete-index" (fun db0 ->
        let db1 = exec db0 "CREATE INDEX c_pid ON c (pid)"
        let db2 = exec db1 "INSERT INTO p VALUES (1, 'a')"
        let db3 = exec db2 "INSERT INTO c VALUES (10, 1)"

        expectErrorKind ErrorKind.ConstraintViolation "索引経由の参照を検出できません" (execSql db3 "DELETE FROM p WHERE id = 1"))

[<Fact>]
let ``ForeignKey.checkParentDelete: 子の主キーが外部キーのときも参照を検出する`` () =
    withSession "fk-delete-pk" (fun db0 ->
        let db1 = exec db0 "CREATE TABLE p (id INTEGER PRIMARY KEY)"
        let db2 = exec db1 "CREATE TABLE c (pid INTEGER PRIMARY KEY REFERENCES p (id))"
        let db3 = exec db2 "INSERT INTO p VALUES (1)"
        let db4 = exec db3 "INSERT INTO c VALUES (1)"

        expectErrorKind ErrorKind.ConstraintViolation "主キー経由の参照を検出できません" (execSql db4 "DELETE FROM p WHERE id = 1"))

[<Fact>]
let ``ForeignKey: 参照されているテーブルは DROP できない`` () =
    withParentChild "fk-drop" (fun db0 ->
        expectErrorKind ErrorKind.ConstraintViolation "参照されている親を DROP できました" (execSql db0 "DROP TABLE p")
        let db1 = exec db0 "DROP TABLE c"
        let db2 = exec db1 "DROP TABLE p"
        expectErrorKind ErrorKind.NameError "DROP したテーブルが残っています" (execSql db2 "SELECT * FROM p"))

[<Fact>]
let ``ForeignKey: 外部キーの列は DROP COLUMN できない`` () =
    withParentChild "fk-drop-column" (fun db0 ->
        expectErrorKind ErrorKind.ConstraintViolation "外部キーの列を削除できました" (execSql db0 "ALTER TABLE c DROP COLUMN pid"))

[<Fact>]
let ``ForeignKey: 親の列名を RENAME すると子の定義が追随する`` () =
    withParentChild "fk-rename" (fun db0 ->
        let db1 = exec db0 "INSERT INTO p VALUES (1, 'a')"
        let db2 = exec db1 "ALTER TABLE p RENAME COLUMN id TO pid2"
        let db3 = exec db2 "INSERT INTO c VALUES (10, 1)"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "RENAME 後に FK が効きません"
            (execSql db3 "INSERT INTO c VALUES (11, 2)")

        let db4 = exec db3 "ALTER TABLE c RENAME COLUMN pid TO parent_id"

        expectErrorKind
            ErrorKind.ConstraintViolation
            "子の列の RENAME 後に FK が効きません"
            (execSql db4 "INSERT INTO c VALUES (12, 9)"))

[<Fact>]
let ``ForeignKey: ALTER TABLE ADD COLUMN で REFERENCES は指定できない`` () =
    withParentChild "fk-add-column" (fun db0 ->
        expectErrorKind
            ErrorKind.ConstraintViolation
            "ADD COLUMN で REFERENCES を受け付けました"
            (execSql db0 "ALTER TABLE c ADD COLUMN pid2 INTEGER REFERENCES p (id)"))
