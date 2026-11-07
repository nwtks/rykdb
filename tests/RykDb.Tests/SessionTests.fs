// ===========================================================================
// Session.fs のテスト: 公開 API のカプセル化
//
// ハンドルを返さないこと、Close 後の呼び出し、複数文の途中エラー、
// 同一 Session の並行呼び出しを検証する。
// ===========================================================================

module RykDb.SessionTests

open System.Threading.Tasks
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private withSession (tag: string) (f: Session -> unit) =
    withDir tag (fun dir ->
        let session = ok (Session.OpenWith dir)

        try
            f session
        finally
            session.Close() |> ignore)

[<Fact>]
let ``Session: ハンドルなしで連続実行できる`` () =
    withSession "session-basic" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        let inserted = ok (session.Execute "INSERT INTO t VALUES (1, 10), (2, 20)")
        Assert.Equal<int>(2, inserted.RowsAffected)

        let selected = ok (session.Execute "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<int>(2, selected.Rows.Length)
        Assert.Equal<int64>(10L, asInt selected.Rows.[0].[1])
        Assert.Equal<int64>(20L, asInt selected.Rows.[1].[1])

        Assert.Equal<string list>([ "t" ], ok (session.TableNames())))

[<Fact>]
let ``Session: 複数文の結果は SELECT が 1つならその結果、それ以外は合計`` () =
    withSession "session-multi-summary" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1, 'a'), (2, 'b')") |> ignore

        let one = ok (session.Execute "SELECT id FROM t ORDER BY id")
        Assert.Equal<string array>([| "id" |], one.Columns)
        Assert.Equal<int>(2, one.Rows.Length)

        let two = ok (session.Execute "SELECT id FROM t; SELECT v FROM t")
        Assert.Empty two.Columns
        Assert.Empty two.Rows
        Assert.Equal<int>(4, two.RowsAffected)

        let mixed =
            ok (session.Execute "INSERT INTO t VALUES (3, 'c'); SELECT id FROM t ORDER BY id")

        Assert.Equal<string array>([| "id" |], mixed.Columns)
        Assert.Equal<int>(3, mixed.Rows.Length)
        Assert.Equal<int>(3, mixed.RowsAffected)

        let none =
            ok (session.Execute "INSERT INTO t VALUES (4, 'd'); INSERT INTO t VALUES (5, 'e')")

        Assert.Empty none.Columns
        Assert.Equal<int>(2, none.RowsAffected))

[<Fact>]
let ``Session: 複数文を ; で渡せる`` () =
    withSession "session-multi" (fun session ->
        let sql =
            "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER); "
            + "INSERT INTO t VALUES (1, 1); "
            + "INSERT INTO t VALUES (2, 2)"

        ok (session.Execute sql) |> ignore
        let result = ok (session.Execute "SELECT id FROM t")
        Assert.Equal<int>(2, result.Rows.Length))

[<Fact>]
let ``Session: 大量の文を 1 度の Execute で実行できる`` () =
    let n = 2_000

    withSession "session-tail-exec" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)")
        |> ignore

        let sql = System.String.Join(";", Array.init n (fun _ -> "SELECT a FROM t"))
        ok (session.Execute sql) |> ignore
        Assert.Equal<string list>([ "t" ], ok (session.TableNames())))

[<Fact>]
let ``Session: 複数文の途中エラーでも最後の成功状態を保持する`` () =
    withSession "session-partial" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        // INSERT は暗黙 transaction で commit 済み。後続の SELECT が失敗する。
        expectError "複数文の途中エラーが成功になっています" (session.Execute "INSERT INTO t VALUES (1, 10); SELECT * FROM nope")

        // 古いハンドルに戻らず、commit 済みの INSERT が見える。
        let count = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``Session: 複数文の BEGIN 後エラーは transaction を開いたままにする`` () =
    withSession "session-partial-tx" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        expectError "BEGIN 後の途中エラーが成功になっています" (session.Execute "BEGIN; INSERT INTO t VALUES (1); SELECT * FROM nope")

        // 最後に成功した BEGIN の状態を保持する。利用者が明示的に rollback する。
        Assert.True session.InTransaction
        ok (session.Rollback())

        let count = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``Session: Begin / Commit / Rollback が使える`` () =
    withSession "session-tx" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        ok (session.Begin())
        Assert.True session.InTransaction
        ok (session.Execute "INSERT INTO t VALUES (1, 10)") |> ignore
        let inside = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt inside.Rows.[0].[0])
        ok (session.Rollback())
        Assert.False session.InTransaction

        ok (session.Begin())
        ok (session.Execute "INSERT INTO t VALUES (2, 20)") |> ignore
        ok (session.Commit())
        Assert.False session.InTransaction

        let after = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt after.Rows.[0].[0]))

[<Fact>]
let ``Session: Close 後の呼び出しはエラーになる`` () =
    withDir "session-close" (fun dir ->
        let session = ok (Session.OpenWith dir)
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        ok (session.Close())

        expectErrorKind ErrorKind.InternalError "Close 後の Execute が成功しました" (session.Execute "SELECT 1")

        expectErrorKind ErrorKind.InternalError "Close 後の Begin が成功しました" (session.Begin())

        expectErrorKind ErrorKind.InternalError "Close 後の Commit が成功しました" (session.Commit())

        expectErrorKind ErrorKind.InternalError "Close 後の Rollback が成功しました" (session.Rollback())

        expectErrorKind ErrorKind.InternalError "Close 後の TableNames が成功しました" (session.TableNames())

        Assert.False session.InTransaction
        // 二重 Close は安全。
        ok (session.Close()))

[<Fact>]
let ``Session: Close は開いた transaction を rollback する`` () =
    withDir "session-close-rollback" (fun dir ->
        let session = ok (Session.OpenWith dir)
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Begin())
        ok (session.Execute "INSERT INTO t VALUES (1)") |> ignore
        ok (session.Close())

        let reopened = ok (Session.OpenWith dir)

        try
            let count = ok (reopened.Execute "SELECT COUNT(*) FROM t")
            Assert.Equal<int64>(0L, asInt count.Rows.[0].[0])
        finally
            reopened.Close() |> ignore)

[<Fact>]
let ``Session: 同一 Session を複数スレッドで使っても書き込みが消えない`` () =
    withSession "session-thread" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        let rowCount = 64

        let tasks =
            [ 1..rowCount ]
            |> List.map (fun id ->
                Task.Run(fun () -> session.Execute(sprintf "INSERT INTO t VALUES (%d)" id) |> ok |> ignore))

        tasks |> Array.ofList |> Task.WaitAll

        let count = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(int64 rowCount, asInt count.Rows.[0].[0]))

[<Fact>]
let ``Session: 空 SQL は 0 行の結果を返す`` () =
    withSession "session-empty" (fun session ->
        let result = ok (session.Execute "")
        Assert.Equal<int>(0, result.RowsAffected)
        Assert.Empty result.Columns
        Assert.Empty result.Rows)

[<Fact>]
let ``Session: Database 型は公開されていない`` () =
    let dbType = typeof<Session>.Assembly.GetType "RykDb.Core.Database"
    Assert.NotNull dbType
    Assert.False dbType.IsPublic
