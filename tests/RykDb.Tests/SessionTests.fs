// ===========================================================================
// Session.fs のテスト: 公開 API のカプセル化
//
// ハンドルを返さないこと、Close 後の呼び出し、複数文の途中エラー、
// 同一 Session の並行呼び出しを検証する。
// ===========================================================================

module RykDb.SessionTests

open System.Threading
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

/// 走査と書き込みを任意のタイミングで止めるためのゲート。
/// 並行性テストで「読み取りが書き込み中に進む」ことを決定的に確かめる。
type private EngineGates() =
    let scanGate = new ManualResetEventSlim(true)
    let writeGate = new ManualResetEventSlim(true)

    member val ScanEntered = new ManualResetEventSlim(false)
    member val WriteEntered = new ManualResetEventSlim(false)

    member this.BlockScans() =
        this.ScanEntered.Reset()
        scanGate.Reset()

    member _.ResumeScans() = scanGate.Set()

    member this.BlockWrites() =
        this.WriteEntered.Reset()
        writeGate.Reset()

    member _.ResumeWrites() = writeGate.Set()
    member _.WaitScan() = scanGate.Wait()
    member _.WaitWrite() = writeGate.Wait()

/// 内側のエンジンに委譲しつつ、ScanAt / WriteBatch だけゲートで止める。
type private GatedEngine(gates: EngineGates, inner: IStorageEngine) =
    interface IStorageEngine with
        member _.NextSeqNo = inner.NextSeqNo
        member _.ActiveSnapshot = inner.ActiveSnapshot
        member _.HostState = inner.HostState
        member _.GetAt visible key = inner.GetAt visible key

        member _.ScanAt visible startKey limit =
            gates.ScanEntered.Set()
            gates.WaitScan()
            inner.ScanAt visible startKey limit

        member _.WriteBatch entries =
            gates.WriteEntered.Set()
            gates.WaitWrite()

            inner.WriteBatch entries
            |> Result.map (fun next -> GatedEngine(gates, next) :> IStorageEngine)

        member _.WithNextSeqNo next =
            GatedEngine(gates, inner.WithNextSeqNo next) :> IStorageEngine

        member _.WithActiveSnapshot snapshot =
            GatedEngine(gates, inner.WithActiveSnapshot snapshot) :> IStorageEngine

        member _.WithHostState state =
            GatedEngine(gates, inner.WithHostState state) :> IStorageEngine

        member _.FlushIfNeeded allocate =
            inner.FlushIfNeeded allocate
            |> Result.map (fun (next, flushed) -> GatedEngine(gates, next) :> IStorageEngine, flushed)

        member _.MaybeFlush() =
            inner.MaybeFlush()
            |> Result.map (fun next -> GatedEngine(gates, next) :> IStorageEngine)

        member _.Close() = inner.Close()

/// ゲート付きメモリエンジンの上に Session を組み立ててテストする。
/// `Session` の internal ctor と `Database.OpenWithStore` を使う。
let private withGatedSession (_tag: string) (f: Session -> EngineGates -> unit) =
    let gates = EngineGates()
    let root = "mem:" + System.Guid.NewGuid().ToString "N"

    let store = openStoreWith "" (GatedEngine(gates, memoryEngine ()) :> IStorageEngine)

    let db = ok (Database.OpenWithStore root store)
    let session = new Session(root, db)

    try
        f session gates
    finally
        gates.ResumeScans()
        gates.ResumeWrites()
        session.Close() |> ignore

/// ブロックして待つ処理は専用スレッドで動かす。
/// スレッドプールが他のテストで飽和していても開始が遅れない。
let private startDedicated (f: unit -> 'T) =
    Task.Factory.StartNew(f, TaskCreationOptions.LongRunning)

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
let ``Session: 読み取りは実行中の書き込みを待たない`` () =
    withGatedSession "session-read-during-write" (fun session gates ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1)") |> ignore
        gates.BlockWrites()

        let writer =
            startDedicated (fun () -> session.Execute "INSERT INTO t VALUES (2)" |> ok |> ignore)

        Assert.True(gates.WriteEntered.Wait 5000, "書き込みが始まりません")

        // 書き込みが WriteBatch で止まっている間に SELECT が完了する。
        let selected = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt selected.Rows.[0].[0])

        gates.ResumeWrites()
        Assert.True(writer.Wait 10000, "書き込みが完了しません")

        let after = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(2L, asInt after.Rows.[0].[0]))

[<Fact>]
let ``Session: 書き込みは実行中の読み取りを待たない`` () =
    withGatedSession "session-write-during-read" (fun session gates ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1)") |> ignore
        gates.BlockScans()

        let reader = startDedicated (fun () -> session.Execute "SELECT COUNT(*) FROM t")
        Assert.True(gates.ScanEntered.Wait 5000, "読み取りが始まりません")

        let inserted = ok (session.Execute "INSERT INTO t VALUES (2)")
        Assert.Equal<int>(1, inserted.RowsAffected)

        gates.ResumeScans()
        Assert.True(reader.Wait 10000, "読み取りが完了しません")

        // 読み取りはスナップショットなので、開始後に commit された行は見えない。
        match reader.Result with
        | Ok selected -> Assert.Equal<int64>(1L, asInt selected.Rows.[0].[0])
        | Error e -> Assert.Fail(sprintf "読み取りが失敗しました: %s" (DbError.toDisplayString e)))

[<Fact>]
let ``Session: Close は実行中の読み取りの完了を待つ`` () =
    withGatedSession "session-close-drain" (fun session gates ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1)") |> ignore
        gates.BlockScans()

        let reader = startDedicated (fun () -> session.Execute "SELECT COUNT(*) FROM t")
        Assert.True(gates.ScanEntered.Wait 5000, "読み取りが始まりません")

        let closing = startDedicated (fun () -> session.Close())
        Thread.Sleep 200
        Assert.False(closing.IsCompleted, "Close が実行中の読み取りを待っていません")

        gates.ResumeScans()
        Assert.True(reader.Wait 10000, "読み取りが完了しません")
        Assert.True(closing.Wait 10000, "Close が完了しません")

        match reader.Result with
        | Ok selected -> Assert.Equal<int64>(1L, asInt selected.Rows.[0].[0])
        | Error e -> Assert.Fail(sprintf "読み取りが失敗しました: %s" (DbError.toDisplayString e))

        expectErrorKind ErrorKind.InternalError "Close 後の Execute が成功しました" (session.Execute "SELECT 1"))

[<Fact>]
let ``Session: 並行読み取りは commit 済みの一貫した snapshot を見る`` () =
    withSession "session-parallel-read" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        let batchSize = 10
        let batches = 20

        let writer =
            Task.Run(fun () ->
                for b in 1..batches do
                    let values =
                        [ 1..batchSize ]
                        |> List.map (fun i -> sprintf "(%d)" ((b - 1) * batchSize + i))
                        |> String.concat ", "

                    ok (session.Execute(sprintf "INSERT INTO t VALUES %s" values)) |> ignore)

        let readers =
            [ 1..4 ]
            |> List.map (fun _ ->
                Task.Run(fun () ->
                    for _ in 1..20 do
                        let count = ok (session.Execute "SELECT COUNT(*) FROM t")
                        let n = asInt count.Rows.[0].[0]

                        Assert.True(n % int64 batchSize = 0L, sprintf "commit 途中の batch が見えています: %d 行" n)))

        Task.WaitAll(Array.append [| writer |] (List.toArray readers))

        let finalCount = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(int64 (batchSize * batches), asInt finalCount.Rows.[0].[0]))

[<Fact>]
let ``Session: ExecuteTracked は transaction の遷移を返す`` () =
    withSession "session-tracked" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        let change (r: Result<StatementResult * SessionTransactionChange, DbError>) = r |> ok |> snd
        Assert.Equal(SessionTransactionChange.NoChange, change (session.ExecuteTracked "SELECT 1"))
        Assert.Equal(SessionTransactionChange.Started, change (session.ExecuteTracked "BEGIN"))
        Assert.Equal(SessionTransactionChange.NoChange, change (session.ExecuteTracked "INSERT INTO t VALUES (1)"))
        Assert.Equal(SessionTransactionChange.Ended, change (session.ExecuteTracked "ROLLBACK"))
        Assert.Equal(SessionTransactionChange.NoChange, change (session.ExecuteTracked "SELECT 1"))

        Assert.Equal(
            SessionTransactionChange.Started,
            change (session.ExecuteTracked "BEGIN; INSERT INTO t VALUES (2)")
        )

        Assert.Equal(SessionTransactionChange.Ended, change (session.ExecuteTracked "COMMIT")))

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
