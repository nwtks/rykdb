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
        member _.WalState = inner.WalState
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

        member _.WriteBatchDeferred entries =
            gates.WriteEntered.Set()
            gates.WaitWrite()

            inner.WriteBatchDeferred entries
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
    let session = Session.OpenWith(root, SessionOptions.Default, db)

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
        expectErrorKind ErrorKind.InternalError "Close 後の TryOpenReader が成功しました" (session.TryOpenReader "SELECT 1")
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
let ``Session: BEGIN は writeGate を取るので実行中の書き込みを待ち、SELECT は待たない`` () =
    // transaction 制御（BEGIN / COMMIT / ROLLBACK / DDL）は writeGate の下で走るので、
    // 走査中（WriteBatch 停止中）に開始すると待つ。SELECT のみのバッチは writeGate を取らない。
    withGatedSession "session-begin-during-write" (fun session gates ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1)") |> ignore
        gates.BlockWrites()

        let writer =
            startDedicated (fun () -> session.Execute "INSERT INTO t VALUES (2)" |> ok |> ignore)

        Assert.True(gates.WriteEntered.Wait 5000, "書き込みが始まりません")

        // writeGate を握っている間は BEGIN は完了しない。
        let begun = startDedicated (fun () -> session.Begin() |> ok |> ignore)
        Assert.False(begun.Wait 300, "BEGIN が writeGate を待たずに進んだ")

        // SELECT のみは writeGate を取らないので、書き込み中でも進む。
        let selected = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt selected.Rows.[0].[0])

        gates.ResumeWrites()
        Assert.True(writer.Wait 10000, "書き込みが完了しません")
        Assert.True(begun.Wait 10000, "BEGIN が完了しません")

        ok (session.Rollback()))

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

let private withSessionOptions options (tag: string) (f: Session -> unit) =
    withDir tag (fun dir ->
        let session = ok (Session.OpenWith(dir, options))

        try
            f session
        finally
            session.Close() |> ignore)

[<Fact>]
let ``SessionOptions: 行数上限を超える SELECT は CapacityError`` () =
    let options =
        { SessionOptions.Default with
            MaxResultRows = 2L
            MaxResultBytes = System.Int64.MaxValue
        }

    withSessionOptions options "session-row-limit" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        ok (session.Execute "INSERT INTO t VALUES (1); INSERT INTO t VALUES (2); INSERT INTO t VALUES (3)")
        |> ignore

        expectErrorKind ErrorKind.CapacityError "行数上限を超えたら CapacityError" (session.Execute "SELECT * FROM t")

        // INSERT は行数上限の対象外。
        ok (session.Execute "INSERT INTO t VALUES (4)") |> ignore)

[<Fact>]
let ``SessionOptions: 複数 SELECT は累積で上限を判定する`` () =
    let options =
        { SessionOptions.Default with
            MaxResultRows = 3L
            MaxResultBytes = System.Int64.MaxValue
        }

    withSessionOptions options "session-cumulative-limit" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        ok (session.Execute "INSERT INTO t VALUES (1); INSERT INTO t VALUES (2); INSERT INTO t VALUES (3)")
        |> ignore

        // 合計 3行までは成功する。
        let rows = ok (session.Execute "SELECT * FROM t LIMIT 1; SELECT * FROM t LIMIT 2")
        Assert.Equal<int>(3, rows.RowsAffected)

        // 2文目で累積上限を超える。
        expectErrorKind
            ErrorKind.CapacityError
            "累積上限を超えたら CapacityError"
            (session.Execute "SELECT * FROM t; SELECT * FROM t"))

[<Fact>]
let ``SessionOptions: 0 行上限でも 0 行の SELECT は成功する`` () =
    let options =
        { SessionOptions.Default with
            MaxResultRows = 0L
            MaxResultBytes = 0L
        }

    withSessionOptions options "session-zero-limit" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        let empty = ok (session.Execute "SELECT * FROM t")
        Assert.Empty empty.Rows
        expectErrorKind ErrorKind.CapacityError "1行でも上限超過" (session.Execute "SELECT 1"))

[<Fact>]
let ``SessionOptions: 負の上限は開くときに CapacityError`` () =
    let dir = newDir "session-invalid-options"

    try
        let options =
            { SessionOptions.Default with
                MaxResultRows = -1L
            }

        expectErrorKind ErrorKind.CapacityError "負の行数上限はエラー" (Session.OpenWith(dir, options))
    finally
        System.IO.Directory.Delete(dir, true)

let private drainReader (reader: RowReader) =
    let rec loop acc =
        match ok (reader.TryRead()) with
        | None -> List.rev acc
        | Some row -> loop (row :: acc)

    loop []

[<Fact>]
let ``RowReader: 単一 SELECT を1行ずつ読める`` () =
    withSession "session-reader-basic" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore

        ok (session.Execute "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')")
        |> ignore

        match ok (session.TryOpenReader "SELECT id, v FROM t ORDER BY id") with
        | None -> Assert.Fail "単一 SELECT が RowReader になりませんでした"
        | Some reader ->
            use reader = reader
            Assert.Equal<string array>([| "id"; "v" |], reader.Columns)

            let rows = drainReader reader
            Assert.Equal<int>(3, rows.Length)
            Assert.Equal<int64>(1L, asInt rows.[0].[0])
            Assert.Equal("a", asText rows.[0].[1])
            Assert.Equal<int64>(3L, asInt rows.[2].[0])

            // 終端後は再度読んでも None。
            match ok (reader.TryRead()) with
            | None -> ()
            | Some _ -> Assert.Fail "終端後の TryRead が行を返しました")

[<Fact>]
let ``RowReader: 対象外の SQL は Ok None を返す`` () =
    withSession "session-reader-none" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        let empty = ok (session.TryOpenReader "")
        Assert.True(Option.isNone empty)

        let multiple = ok (session.TryOpenReader "SELECT 1; SELECT 2")
        Assert.True(Option.isNone multiple)

        let write = ok (session.TryOpenReader "INSERT INTO t VALUES (1)")
        Assert.True(Option.isNone write)
        expectErrorKind ErrorKind.SyntaxError "構文エラーは返す" (session.TryOpenReader "SELECT")

        ok (session.Begin())
        let inTransaction = ok (session.TryOpenReader "SELECT 1")
        Assert.True(Option.isNone inTransaction)
        ok (session.Rollback()))

[<Fact>]
let ``RowReader: SessionOptions の結果上限を適用しない`` () =
    let options =
        { SessionOptions.Default with
            MaxResultRows = 1L
            MaxResultBytes = 1L
        }

    withSessionOptions options "session-reader-no-limit" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1), (2), (3)") |> ignore

        match ok (session.TryOpenReader "SELECT id FROM t ORDER BY id") with
        | None -> Assert.Fail "単一 SELECT が RowReader になりませんでした"
        | Some reader ->
            use reader = reader
            let rows = drainReader reader
            Assert.Equal<int>(3, rows.Length)

        // 通常の Execute には上限が効く。
        expectErrorKind ErrorKind.CapacityError "Execute には上限が効く" (session.Execute "SELECT id FROM t"))

[<Fact>]
let ``RowReader: 読み取り中に commit しても開始時 snapshot を見る`` () =
    withSession "session-reader-snapshot" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1), (2)") |> ignore

        match ok (session.TryOpenReader "SELECT id FROM t ORDER BY id") with
        | None -> Assert.Fail "単一 SELECT が RowReader になりませんでした"
        | Some reader ->
            use reader = reader

            match ok (reader.TryRead()) with
            | Some row -> Assert.Equal<int64>(1L, asInt row.[0])
            | None -> Assert.Fail "1行目が読めませんでした"

            ok (session.Execute "INSERT INTO t VALUES (3)") |> ignore

            let rest = drainReader reader
            Assert.Equal<int>(1, rest.Length)
            Assert.Equal<int64>(2L, asInt rest.[0].[0])

        // 読み終わった後は new snapshot で 3 が見える。
        let after = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(3L, asInt after.Rows.[0].[0]))

[<Fact>]
let ``RowReader: Dispose すると Close が待たずに完了できる`` () =
    withDir "session-reader-close" (fun dir ->
        let session = ok (Session.OpenWith dir)
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1), (2)") |> ignore

        match ok (session.TryOpenReader "SELECT id FROM t ORDER BY id") with
        | None -> Assert.Fail "単一 SELECT が RowReader になりませんでした"
        | Some reader ->
            let closing = startDedicated (fun () -> session.Close())
            Thread.Sleep 200
            Assert.False(closing.IsCompleted, "Close が開いている RowReader を待っていません")

            (reader :> System.IDisposable).Dispose()
            Assert.True(closing.Wait 10000, "RowReader の Dispose 後に Close が完了しません")
            expectErrorKind ErrorKind.InternalError "Dispose 後の TryRead" (reader.TryRead()))

// ---------------------------------------------------------------------------
// SessionPool: 同一 DB を共有する複数接続
// ---------------------------------------------------------------------------

let private withPool (tag: string) (f: SessionPool -> unit) =
    withDir tag (fun dir ->
        let pool = ok (SessionPool.OpenWith dir)

        try
            f pool
        finally
            pool.Close() |> ignore)

/// 1行 1列の INTEGER を取り出す。
let private scalar (session: Session) (sql: string) =
    let result = ok (session.Execute sql)
    asInt result.Rows.[0].[0]

[<Fact>]
let ``SessionPool: 接続ごとに transaction が独立している`` () =
    withPool "pool-isolation" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
            ok (a.Begin()) |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1)") |> ignore

            // A の未 commit は B から見えない。A 自身には見える（read-your-writes）。
            Assert.Equal(0L, scalar b "SELECT COUNT(*) FROM t")
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t")

            ok (a.Commit()) |> ignore
            Assert.Equal(1L, scalar b "SELECT COUNT(*) FROM t")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 明示 transaction は開始時の snapshot を見る`` () =
    withPool "pool-repeatable" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1)") |> ignore

            ok (a.Begin()) |> ignore
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t")

            // A の transaction 中に B が commit しても、A は開始時の snapshot を見る。
            ok (b.Execute "INSERT INTO t VALUES (2)") |> ignore
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t")

            ok (a.Rollback()) |> ignore
            Assert.Equal(2L, scalar a "SELECT COUNT(*) FROM t")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 接続間で write-write 競合を検出する`` () =
    withPool "pool-conflict" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)") |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1, 10)") |> ignore

            ok (a.Begin()) |> ignore
            ok (b.Begin()) |> ignore
            ok (a.Execute "UPDATE t SET v = 11 WHERE id = 1") |> ignore
            ok (b.Execute "UPDATE t SET v = 12 WHERE id = 1") |> ignore

            ok (a.Commit()) |> ignore
            expectErrorKind ErrorKind.ConflictError "後から commit した側" (b.Commit())
            ok (b.Rollback()) |> ignore

            Assert.Equal(11L, scalar a "SELECT v FROM t WHERE id = 1")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 長い transaction 中でも他接続は commit できる`` () =
    withPool "pool-concurrent" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
            ok (a.Begin()) |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1)") |> ignore

            // A が transaction を開いたままでも、B の暗黙 transaction は通る。
            ok (b.Execute "INSERT INTO t VALUES (2)") |> ignore

            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t")

            ok (a.Commit()) |> ignore
            Assert.Equal(2L, scalar a "SELECT COUNT(*) FROM t")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 接続を閉じるとその transaction だけ rollback される`` () =
    withDir "pool-close-rollback" (fun dir ->
        let pool = ok (SessionPool.OpenWith dir)
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (a.Begin()) |> ignore
        ok (a.Execute "INSERT INTO t VALUES (1)") |> ignore

        ok (b.Begin()) |> ignore
        ok (b.Execute "INSERT INTO t VALUES (2)") |> ignore

        // A だけ閉じる。A の行は rollback され、B の transaction は残る。
        ok (a.Close())
        ok (b.Execute "INSERT INTO t VALUES (3)") |> ignore
        ok (b.Commit()) |> ignore
        Assert.Equal(2L, scalar b "SELECT COUNT(*) FROM t")
        ok (b.Close())
        ok (pool.Close())

        // 再オープンしても A の行は残っていない。
        let reopened = ok (Session.OpenWith dir)

        try
            Assert.Equal(2L, scalar reopened "SELECT COUNT(*) FROM t")
        finally
            reopened.Close() |> ignore)

[<Fact>]
let ``SessionPool: transaction 中の DDL は他接続で ConflictError`` () =
    withPool "pool-ddl" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
            ok (a.Begin()) |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1)") |> ignore

            expectErrorKind ErrorKind.ConflictError "DDL は ConflictError" (b.Execute "CREATE INDEX ix ON t (id)")

            ok (a.Commit()) |> ignore
            ok (b.Execute "CREATE INDEX ix ON t (id)") |> ignore
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: DDL の結果は他接続の次の文から見える`` () =
    withPool "pool-ddl-visible" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

            // B は開いたままでも、次の文では新しいカタログを見る。
            ok (b.Execute "INSERT INTO t VALUES (1)") |> ignore
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 閉じた後の操作はエラーになる`` () =
    withDir "pool-closed" (fun dir ->
        let pool = ok (SessionPool.OpenWith dir)
        let session = ok (pool.CreateSession())
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (pool.Close())

        expectErrorKind ErrorKind.InternalError "Close 後の Execute" (session.Execute "SELECT 1")
        expectErrorKind ErrorKind.InternalError "閉じた pool の CreateSession" (pool.CreateSession())
        session.Close() |> ignore)
