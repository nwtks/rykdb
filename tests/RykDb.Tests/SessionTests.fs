// ===========================================================================
// Session.fs のテスト: 公開 API のカプセル化
//
// ハンドルを返さないこと、Close 後の呼び出し、複数文の途中エラー、
// 同一 Session の並行呼び出しを検証する。
// ===========================================================================

module RykDb.SessionTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private withDiskSession (tag: string) (f: Session -> unit) =
    withDir tag (fun dir ->
        let session = ok (Session.OpenWith dir)

        try
            f session
        finally
            session.Close() |> ignore)

let private withDiskSessionOptions options (tag: string) (f: Session -> unit) =
    withDir tag (fun dir ->
        let session = ok (Session.OpenWith(dir, options))

        try
            f session
        finally
            session.Close() |> ignore)

/// 走査と書き込みを任意のタイミングで止めるためのゲート。
/// 並行性テストで「読み取りが書き込み中に進む」ことを決定的に確かめる。
type private EngineGates() =
    let scanGate = new System.Threading.ManualResetEventSlim true
    let writeGate = new System.Threading.ManualResetEventSlim true

    member val ScanEntered = new System.Threading.ManualResetEventSlim false
    member val WriteEntered = new System.Threading.ManualResetEventSlim false

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
        member _.PublishedState = inner.PublishedState
        member _.WalState = inner.WalState
        member _.LastFlushError = inner.LastFlushError
        member _.GetAtMasked key mask visible = inner.GetAtMasked key mask visible

        member _.ScanAtValues startKey limit visible needsValues =
            gates.ScanEntered.Set()
            gates.WaitScan()
            inner.ScanAtValues startKey limit visible needsValues

        member _.ScanAtMasked startKey limit mask visible =
            gates.ScanEntered.Set()
            gates.WaitScan()
            inner.ScanAtMasked startKey limit mask visible

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
            |> Result.map (fun (next, error) -> GatedEngine(gates, next) :> IStorageEngine, error)

        member _.Close() = inner.Close()

/// ゲート付きメモリエンジンの上に Session を組み立ててテストする。
let private withGatedSession (_tag: string) (f: Session -> EngineGates -> unit) =
    let gates = EngineGates()
    let root = "mem:" + System.Guid.NewGuid().ToString "N"
    let store = openStoreWith "" (GatedEngine(gates, memoryEngine ()) :> IStorageEngine)
    let session = ok (Session.OpenWithStore(root, SessionOptions.Default, store))

    try
        f session gates
    finally
        gates.ResumeScans()
        gates.ResumeWrites()
        session.Close() |> ignore

/// 例外を投げるかどうかのフラグ（書き込みで差し替わる engine を跨いで共有する）。
type private ThrowingGate() =
    member val Armed = false with get, set

/// 有効化すると読み取りで例外を投げるエンジン（例外経路の登録解除用）。
type private ThrowingEngine(gate: ThrowingGate, inner: IStorageEngine) =
    interface IStorageEngine with
        member _.NextSeqNo = inner.NextSeqNo
        member _.ActiveSnapshot = inner.ActiveSnapshot
        member _.PublishedState = inner.PublishedState
        member _.WalState = inner.WalState
        member _.LastFlushError = inner.LastFlushError

        member _.GetAtMasked key mask visible =
            if gate.Armed then
                failwith "test: engine read failure"

            inner.GetAtMasked key mask visible

        member _.ScanAtValues startKey limit visible needsValues =
            if gate.Armed then
                failwith "test: engine read failure"

            inner.ScanAtValues startKey limit visible needsValues

        member _.ScanAtMasked startKey limit mask visible =
            if gate.Armed then
                failwith "test: engine read failure"

            inner.ScanAtMasked startKey limit mask visible

        member _.WriteBatch entries =
            inner.WriteBatch entries
            |> Result.map (fun next -> ThrowingEngine(gate, next) :> IStorageEngine)

        member _.WriteBatchDeferred entries =
            inner.WriteBatchDeferred entries
            |> Result.map (fun next -> ThrowingEngine(gate, next) :> IStorageEngine)

        member _.WithNextSeqNo next =
            ThrowingEngine(gate, inner.WithNextSeqNo next) :> IStorageEngine

        member _.WithActiveSnapshot snapshot =
            ThrowingEngine(gate, inner.WithActiveSnapshot snapshot) :> IStorageEngine

        member _.WithHostState state =
            ThrowingEngine(gate, inner.WithHostState state) :> IStorageEngine

        member _.FlushIfNeeded allocate =
            inner.FlushIfNeeded allocate
            |> Result.map (fun (next, flushed) -> ThrowingEngine(gate, next) :> IStorageEngine, flushed)

        member _.MaybeFlush() =
            inner.MaybeFlush()
            |> Result.map (fun (next, error) -> ThrowingEngine(gate, next) :> IStorageEngine, error)

        member _.Close() = inner.Close()

/// 例外を投げられるエンジンの上に host / session を組み立てる。
let private withThrowingSession (_tag: string) (f: DatabaseHost -> Session -> ThrowingGate -> unit) =
    let gate = ThrowingGate()
    let root = "mem:" + System.Guid.NewGuid().ToString "N"

    let store =
        openStoreWith "" (ThrowingEngine(gate, memoryEngine ()) :> IStorageEngine)

    let host = ok (DatabaseHost.OpenWithStore(root, store))
    let session = Session.Create(host, SessionOptions.Default, true)

    try
        f host session gate
    finally
        session.Close() |> ignore

/// ブロックして待つ処理は専用スレッドで動かす。
/// スレッドプールが他のテストで飽和していても開始が遅れない。
let private startDedicated (f: unit -> 'T) =
    System.Threading.Tasks.Task.Factory.StartNew(f, System.Threading.Tasks.TaskCreationOptions.LongRunning)

let private drainReader (reader: RowReader) =
    let rec loop acc =
        match ok (reader.TryRead()) with
        | None -> List.rev acc
        | Some row -> loop (row :: acc)

    loop []

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
let ``Session: ハンドルなしで連続実行できる`` () =
    withDiskSession "session-basic" (fun session ->
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
let ``Session: flush / compaction の直近の失敗を公開する`` () =
    withDiskSession "session-diagnostics" (fun session ->
        // 正常に開いた直後はどちらも失敗していない。
        Assert.True(session.LastFlushError.IsNone)
        Assert.True(session.LastCompactionError.IsNone)

        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        Assert.True(session.LastFlushError.IsNone)
        // 同期 compaction の Session は worker を持たないため常に None。
        Assert.True(session.LastCompactionError.IsNone))

[<Fact>]
let ``Session: 複数文の結果は SELECT が 1つならその結果、それ以外は合計`` () =
    withDiskSession "session-multi-summary" (fun session ->
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
    withDiskSession "session-multi" (fun session ->
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

    withDiskSession "session-tail-exec" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, a INTEGER)")
        |> ignore

        let sql = System.String.Join(";", Array.init n (fun _ -> "SELECT a FROM t"))
        ok (session.Execute sql) |> ignore
        Assert.Equal<string list>([ "t" ], ok (session.TableNames())))

[<Fact>]
let ``Session: 複数文の途中エラーでも最後の成功状態を保持する`` () =
    withDiskSession "session-partial" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        // INSERT は暗黙 transaction で commit 済み。後続の SELECT が失敗する。
        expectError "複数文の途中エラーが成功になっています" (session.Execute "INSERT INTO t VALUES (1, 10); SELECT * FROM nope")

        // 古いハンドルに戻らず、commit 済みの INSERT が見える。
        let count = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(1L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``Session: 複数文の BEGIN 後エラーは transaction を開いたままにする`` () =
    withDiskSession "session-partial-tx" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        expectError "BEGIN 後の途中エラーが成功になっています" (session.Execute "BEGIN; INSERT INTO t VALUES (1); SELECT * FROM nope")

        // 最後に成功した BEGIN の状態を保持する。利用者が明示的に rollback する。
        Assert.True session.InTransaction
        ok (session.Rollback())
        let count = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(0L, asInt count.Rows.[0].[0]))

[<Fact>]
let ``Session: Begin / Commit / Rollback が使える`` () =
    withDiskSession "session-tx" (fun session ->
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
    withDiskSession "session-thread" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        let rowCount = 64

        let tasks =
            [ 1..rowCount ]
            |> List.map (fun id ->
                System.Threading.Tasks.Task.Run(fun () ->
                    session.Execute(sprintf "INSERT INTO t VALUES (%d)" id) |> ok |> ignore))

        tasks |> Array.ofList |> System.Threading.Tasks.Task.WaitAll

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
        System.Threading.Thread.Sleep 200
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
    withDiskSession "session-parallel-read" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        let batchSize = 10
        let batches = 20

        let writer =
            System.Threading.Tasks.Task.Run(fun () ->
                for b in 1..batches do
                    let values =
                        [ 1..batchSize ]
                        |> List.map (fun i -> sprintf "(%d)" ((b - 1) * batchSize + i))
                        |> String.concat ", "

                    ok (session.Execute(sprintf "INSERT INTO t VALUES %s" values)) |> ignore)

        let readers =
            [ 1..4 ]
            |> List.map (fun _ ->
                System.Threading.Tasks.Task.Run(fun () ->
                    for _ in 1..20 do
                        let count = ok (session.Execute "SELECT COUNT(*) FROM t")
                        let n = asInt count.Rows.[0].[0]

                        Assert.True(n % int64 batchSize = 0L, sprintf "commit 途中の batch が見えています: %d 行" n)))

        System.Threading.Tasks.Task.WaitAll(Array.append [| writer |] (List.toArray readers))

        let finalCount = ok (session.Execute "SELECT COUNT(*) FROM t")
        Assert.Equal<int64>(int64 (batchSize * batches), asInt finalCount.Rows.[0].[0]))

[<Fact>]
let ``Session: 文の実行が例外で終わっても snapshot 登録は外れる`` () =
    // 例外は Session の境界で InternalError になるが、登録解除は Engine の try/finally が担う。
    // 残ると GC 境界が固定され、その root の版が捨てられなくなる。
    withThrowingSession "session-throw-unregister" (fun host session gate ->
        let runtime = host.Committed.Store.Runtime
        let root = host.Committed.Root

        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1), (2)") |> ignore
        Assert.True((runtime.OldestExcept(root, None)).IsNone)

        gate.Armed <- true
        expectErrorKind ErrorKind.InternalError "例外は InternalError になる" (session.Execute "SELECT * FROM t")

        gate.Armed <- false
        Assert.True((runtime.OldestExcept(root, None)).IsNone)

        // 登録が残っていると以降の DDL も詰まる（ここでは通ることまで見る）。
        ok (session.Execute "CREATE INDEX ix ON t (id)") |> ignore)

[<Fact>]
let ``Session: 空 SQL は 0 行の結果を返す`` () =
    withDiskSession "session-empty" (fun session ->
        let result = ok (session.Execute "")
        Assert.Equal<int>(0, result.RowsAffected)
        Assert.Empty result.Columns
        Assert.Empty result.Rows)

[<Fact>]
let ``SessionOptions: 行数上限を超える SELECT は CapacityError`` () =
    let options =
        { SessionOptions.Default with
            MaxResultRows = 2L
            MaxResultBytes = System.Int64.MaxValue
        }

    withDiskSessionOptions options "session-row-limit" (fun session ->
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

    withDiskSessionOptions options "session-cumulative-limit" (fun session ->
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

    withDiskSessionOptions options "session-zero-limit" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        let empty = ok (session.Execute "SELECT * FROM t")
        Assert.Empty empty.Rows
        expectErrorKind ErrorKind.CapacityError "1行でも上限超過" (session.Execute "SELECT 1"))

[<Fact>]
let ``SessionOptions: -1 は無制限、-2 以下は開くときに CapacityError`` () =
    let dir = newDir "session-invalid-options"

    try
        // -1 は「無制限」。開けて、上限を超える行数でも返る。
        let unlimited =
            { SessionOptions.Default with
                MaxResultRows = -1L
                MaxResultBytes = -1L
            }

        match Session.OpenWith(dir, unlimited) with
        | Error e -> Assert.Fail $"無制限の上限で開けませんでした: {e.Message}"
        | Ok session ->
            use session = session
            ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

            ok (session.Execute "INSERT INTO t VALUES (1); INSERT INTO t VALUES (2); INSERT INTO t VALUES (3)")
            |> ignore

            let rows = ok (session.Execute "SELECT * FROM t")
            Assert.Equal<int>(3, rows.Rows.Length)

        // -2 は不正。
        let invalid =
            { SessionOptions.Default with
                MaxResultRows = -2L
            }

        expectErrorKind ErrorKind.CapacityError "-2 の行数上限はエラー" (Session.OpenWith(dir, invalid))
    finally
        System.IO.Directory.Delete(dir, true)

[<Fact>]
let ``RowReader: 単一 SELECT を1行ずつ読める`` () =
    withDiskSession "session-reader-basic" (fun session ->
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
    withDiskSession "session-reader-none" (fun session ->
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

    withDiskSessionOptions options "session-reader-no-limit" (fun session ->
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
let ``RowReader: SessionOptions のスピル先と作業上限を使う`` () =
    withDir "session-reader-spill" (fun dir ->
        let spillDir = System.IO.Path.Combine(dir, "spill")
        System.IO.Directory.CreateDirectory spillDir |> ignore

        let options =
            { SessionOptions.Default with
                WorkMemBytes = 1L
                SortBufferBytes = 1L
                SpillDirectory = Some spillDir
            }

        let session = ok (Session.OpenWith(dir, options))

        try
            ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
            |> ignore

            ok (session.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30),(5,20)")
            |> ignore

            match ok (session.TryOpenReader "SELECT DISTINCT v FROM t ORDER BY v") with
            | None -> Assert.Fail "単一 SELECT が RowReader になりませんでした"
            | Some reader ->
                // 演算子の構築時に退避が起きるので、指定したディレクトリに一時ファイルができる。
                Assert.NotEmpty(System.IO.Directory.GetFiles spillDir)
                use reader = reader
                let rows = drainReader reader
                Assert.Equal<int>(3, rows.Length)

            // 読み切ると一時ファイルは消える。
            Assert.Empty(System.IO.Directory.GetFiles spillDir)
        finally
            session.Close() |> ignore)

[<Fact>]
let ``RowReader: LIMIT で早期終了しても一時ファイルが残らない`` () =
    withDir "session-reader-early-exit" (fun dir ->
        let spillDir = System.IO.Path.Combine(dir, "spill")
        System.IO.Directory.CreateDirectory spillDir |> ignore

        let options =
            { SessionOptions.Default with
                WorkMemBytes = 1L
                SortBufferBytes = 1L
                SpillDirectory = Some spillDir
            }

        let session = ok (Session.OpenWith(dir, options))

        try
            ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
            |> ignore

            ok (session.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,10),(4,30),(5,20)")
            |> ignore

            match ok (session.TryOpenReader "SELECT DISTINCT v FROM t ORDER BY v LIMIT 1") with
            | None -> Assert.Fail "単一 SELECT が RowReader になりませんでした"
            | Some reader ->
                use reader = reader

                match ok (reader.TryRead()) with
                | Some _ -> ()
                | None -> Assert.Fail "先頭行が返りませんでした"

            // Dispose で ScanScope が閉じられ、退避ファイルも消える。
            Assert.Empty(System.IO.Directory.GetFiles spillDir)
        finally
            session.Close() |> ignore)

[<Fact>]
let ``Session: IN サブクエリは作業メモリを超えても退避して同じ結果`` () =
    withDir "session-in-spill" (fun dir ->
        let spillDir = System.IO.Path.Combine(dir, "spill")
        System.IO.Directory.CreateDirectory spillDir |> ignore

        let options =
            { SessionOptions.Default with
                WorkMemBytes = 1L
                SortBufferBytes = 1L
                SpillDirectory = Some spillDir
            }

        let session = ok (Session.OpenWith(dir, options))

        try
            ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
            |> ignore

            ok (session.Execute "INSERT INTO t VALUES (1,10),(2,20),(3,30),(4,40),(5,50)")
            |> ignore

            ok (session.Execute "CREATE TABLE s (id INTEGER PRIMARY KEY, v INTEGER)")
            |> ignore

            ok (session.Execute "INSERT INTO s VALUES (1,20),(2,40),(3,NULL)") |> ignore

            // IN: NULL を含む集合で見つからない値は UNKNOWN（行にならない）。
            let rows =
                ok (session.Execute "SELECT id FROM t WHERE v IN (SELECT v FROM s) ORDER BY id")

            Assert.Equal<int>(2, rows.Rows.Length)
            Assert.Equal<int64>(2L, asInt rows.Rows.[0].[0])
            Assert.Equal<int64>(4L, asInt rows.Rows.[1].[0])

            // 相関 IN も同じ結果になる。
            let correlated =
                ok (
                    session.Execute
                        "SELECT a.id FROM t AS a WHERE a.v IN (SELECT b.v FROM s AS b WHERE b.v > a.v - 1) ORDER BY a.id"
                )

            Assert.Equal<int>(2, correlated.Rows.Length)

            // クエリ終了時に退避ファイルは消える。
            Assert.Empty(System.IO.Directory.GetFiles spillDir)
        finally
            session.Close() |> ignore)

[<Fact>]
let ``RowReader: 読み取り中に commit しても開始時 snapshot を見る`` () =
    withDiskSession "session-reader-snapshot" (fun session ->
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
            System.Threading.Thread.Sleep 200
            Assert.False(closing.IsCompleted, "Close が開いている RowReader を待っていません")

            (reader :> System.IDisposable).Dispose()
            Assert.True(closing.Wait 10000, "RowReader の Dispose 後に Close が完了しません")
            expectErrorKind ErrorKind.InternalError "Dispose 後の TryRead" (reader.TryRead()))

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
            // 競合した COMMIT は transaction を自動的に rollback する
            Assert.False b.InTransaction
            Assert.Equal(11L, scalar a "SELECT v FROM t WHERE id = 1")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 競合した COMMIT は transaction を自動的に rollback する`` () =
    withPool "pool-conflict-auto-rollback" (fun pool ->
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
            // 失敗した COMMIT は transaction を捨て、接続は transaction 外へ戻る。
            Assert.False b.InTransaction
            expectErrorKind ErrorKind.InternalError "自動 rollback 後の COMMIT" (b.Commit())
            // 後始末の ROLLBACK は何もしない（成功）。
            ok (b.Rollback()) |> ignore

            // 新しい transaction で最初からやり直せる。
            ok (b.Begin()) |> ignore
            ok (b.Execute "UPDATE t SET v = 12 WHERE id = 1") |> ignore
            ok (b.Commit()) |> ignore
            Assert.Equal(12L, scalar a "SELECT v FROM t WHERE id = 1")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: SQL の COMMIT 競合でも transaction が閉じる`` () =
    withPool "pool-conflict-sql" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)") |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1, 10)") |> ignore
            ok (a.Execute "BEGIN") |> ignore
            ok (b.Execute "BEGIN") |> ignore
            ok (a.Execute "UPDATE t SET v = 11 WHERE id = 1") |> ignore
            ok (b.Execute "UPDATE t SET v = 12 WHERE id = 1") |> ignore
            ok (a.Execute "COMMIT") |> ignore
            expectErrorKind ErrorKind.ConflictError "SQL の後から COMMIT" (b.Execute "COMMIT")
            Assert.False b.InTransaction
            expectErrorKind ErrorKind.InternalError "自動 rollback 後の SQL COMMIT" (b.Execute "COMMIT")
            // 後始末の ROLLBACK は何もしない（成功）。
            ok (b.Execute "ROLLBACK") |> ignore

            // BEGIN からやり直せる。
            ok (b.Execute "BEGIN") |> ignore
            ok (b.Execute "UPDATE t SET v = 12 WHERE id = 1") |> ignore
            ok (b.Execute "COMMIT") |> ignore
            Assert.Equal(12L, scalar a "SELECT v FROM t WHERE id = 1")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 接続間で UNIQUE の同時 INSERT を検出する`` () =
    withPool "pool-unique-insert" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)")
            |> ignore

            ok (a.Begin()) |> ignore
            ok (b.Begin()) |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1, 'x@example.com')") |> ignore
            ok (b.Execute "INSERT INTO t VALUES (2, 'x@example.com')") |> ignore
            ok (a.Commit()) |> ignore
            expectErrorKind ErrorKind.ConflictError "後から commit した UNIQUE INSERT" (b.Commit())
            Assert.False b.InTransaction
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t")

            expectErrorKind
                ErrorKind.ConstraintViolation
                "commit 後の再実行は UNIQUE 違反になる"
                (b.Execute "INSERT INTO t VALUES (2, 'x@example.com')")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: 接続間で UNIQUE の同時 UPDATE を検出する`` () =
    withPool "pool-unique-update" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)")
            |> ignore

            ok (a.Execute "INSERT INTO t VALUES (1, 'a'), (2, 'b')") |> ignore
            ok (a.Begin()) |> ignore
            ok (b.Begin()) |> ignore
            ok (a.Execute "UPDATE t SET email = 'c' WHERE id = 1") |> ignore
            ok (b.Execute "UPDATE t SET email = 'c' WHERE id = 2") |> ignore
            ok (a.Commit()) |> ignore
            expectErrorKind ErrorKind.ConflictError "後から commit した UNIQUE UPDATE" (b.Commit())
            Assert.False b.InTransaction
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t WHERE email = 'c'")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: txn 内で消した UNIQUE 値は競合しない`` () =
    withPool "pool-unique-released" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)")
            |> ignore

            ok (a.Begin()) |> ignore
            ok (b.Begin()) |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1, 'x')") |> ignore
            ok (a.Execute "UPDATE t SET email = 'y' WHERE id = 1") |> ignore
            ok (b.Execute "INSERT INTO t VALUES (2, 'x')") |> ignore
            ok (b.Commit()) |> ignore
            ok (a.Commit()) |> ignore
            Assert.Equal(2L, scalar a "SELECT COUNT(*) FROM t")
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t WHERE email = 'x'")
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t WHERE email = 'y'")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: UNIQUE の NULL は接続間でも重複できる`` () =
    withPool "pool-unique-null" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)")
            |> ignore

            ok (a.Begin()) |> ignore
            ok (b.Begin()) |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1, NULL)") |> ignore
            ok (b.Execute "INSERT INTO t VALUES (2, NULL)") |> ignore
            ok (a.Commit()) |> ignore
            ok (b.Commit()) |> ignore
            Assert.Equal(2L, scalar a "SELECT COUNT(*) FROM t")
        finally
            a.Close() |> ignore
            b.Close() |> ignore)

[<Fact>]
let ``SessionPool: rollback した UNIQUE の予約は残らない`` () =
    withPool "pool-unique-rollback" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT UNIQUE)")
            |> ignore

            ok (a.Begin()) |> ignore
            ok (b.Begin()) |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1, 'x@example.com')") |> ignore
            ok (b.Execute "INSERT INTO t VALUES (2, 'x@example.com')") |> ignore
            ok (a.Rollback()) |> ignore
            ok (b.Commit()) |> ignore
            Assert.Equal(1L, scalar a "SELECT COUNT(*) FROM t")
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
let ``SessionPool: RowReader を開いている間も他接続の DDL は成功する`` () =
    withPool "pool-ddl-reader" (fun pool ->
        let a = ok (pool.CreateSession())
        let b = ok (pool.CreateSession())

        try
            ok (a.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
            ok (a.Execute "INSERT INTO t VALUES (1)") |> ignore

            match ok (a.TryOpenReader "SELECT id FROM t") with
            | None -> Assert.Fail "単一 SELECT が RowReader になりませんでした"
            | Some reader ->
                use reader = reader
                // 読み取り中の snapshot 登録は文の種別なので DDL を拒否しない
                // （種別を分ける前は ConflictError になった）。
                ok (b.Execute "CREATE TABLE u (id INTEGER PRIMARY KEY)") |> ignore
                ok (b.Execute "INSERT INTO u VALUES (10)") |> ignore

                // 読み取りは開いた時点の snapshot を保つ。
                let rows = drainReader reader
                Assert.Equal<int>(1, rows.Length)
                Assert.Equal<int64>(1L, asInt rows.[0].[0])

            // 読み取りを閉じた後も DDL は通常どおり。
            ok (b.Execute "CREATE INDEX ix_u ON u (id)") |> ignore
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

[<Fact>]
let ``Session: パラメータ付きで INSERT / SELECT / UPDATE / DELETE できる`` () =
    withDiskSession "session-params-dml" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT, score REAL)")
        |> ignore

        let inserted =
            ok (
                session.Execute(
                    "INSERT INTO t VALUES (?, ?, ?), (?, ?, ?)",
                    [|
                        SqlValue.Int 1L
                        SqlValue.Text "alice"
                        SqlValue.Real 1.5
                        SqlValue.Int 2L
                        SqlValue.Text "bob"
                        SqlValue.Null
                    |]
                )
            )

        Assert.Equal<int>(2, inserted.RowsAffected)

        let selected =
            ok (session.Execute("SELECT id, name FROM t WHERE score = ?", [| SqlValue.Real 1.5 |]))

        Assert.Equal<int>(1, selected.Rows.Length)
        Assert.Equal<int64>(1L, asInt selected.Rows.[0].[0])
        Assert.Equal("alice", asText selected.Rows.[0].[1])

        let updated =
            ok (session.Execute("UPDATE t SET name = ? WHERE id = ?", [| SqlValue.Text "bobby"; SqlValue.Int 2L |]))

        Assert.Equal<int>(1, updated.RowsAffected)

        let deleted =
            ok (session.Execute("DELETE FROM t WHERE id = ?", [| SqlValue.Int 1L |]))

        Assert.Equal<int>(1, deleted.RowsAffected)
        Assert.Equal(1L, scalar session "SELECT COUNT(*) FROM t"))

[<Fact>]
let ``Session: パラメータは LIKE / LIMIT / OFFSET / NULL にも使える`` () =
    withDiskSession "session-params-expr" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")
        |> ignore

        ok (session.Execute "INSERT INTO t VALUES (1, 'alice'), (2, 'bob'), (3, 'carol')")
        |> ignore

        let result =
            ok (
                session.Execute(
                    "SELECT id FROM t WHERE name LIKE ? ORDER BY id LIMIT ? OFFSET ?",
                    [| SqlValue.Text "a%"; SqlValue.Int 1L; SqlValue.Int 0L |]
                )
            )

        Assert.Equal<int>(1, result.Rows.Length)
        Assert.Equal<int64>(1L, asInt result.Rows.[0].[0])

        let isNull = ok (session.Execute("SELECT ? IS NULL", [| SqlValue.Null |]))
        Assert.Equal(SqlValue.Bool true, isNull.Rows.[0].[0]))

[<Fact>]
let ``Session: パラメータの型を保って入出力できる`` () =
    withDiskSession "session-params-types" (fun session ->
        ok (
            session.Execute
                "CREATE TABLE t (id INTEGER PRIMARY KEY, d DATE, ts TIMESTAMP, dec DECIMAL, u UUID, b BLOB, flag BOOLEAN)"
        )
        |> ignore

        let date = System.DateOnly(2024, 1, 2)
        let timestamp = System.DateTimeOffset(2024, 1, 2, 3, 4, 5, System.TimeSpan.Zero)
        let uuid = System.Guid.NewGuid()

        ok (
            session.Execute(
                "INSERT INTO t VALUES (?, ?, ?, ?, ?, ?, ?)",
                [|
                    SqlValue.Int 1L
                    SqlValue.Date date
                    SqlValue.Timestamp timestamp
                    SqlValue.Decimal 12.34m
                    SqlValue.Uuid uuid
                    SqlValue.Blob [| 1uy; 2uy; 3uy |]
                    SqlValue.Bool true
                |]
            )
        )
        |> ignore

        let row = (ok (session.Execute "SELECT d, ts, dec, u, b, flag FROM t")).Rows.[0]
        Assert.Equal(SqlValue.Date date, row.[0])
        Assert.Equal(SqlValue.Timestamp timestamp, row.[1])
        Assert.Equal(SqlValue.Decimal 12.34m, row.[2])
        Assert.Equal(SqlValue.Uuid uuid, row.[3])
        Assert.Equal(SqlValue.Blob [| 1uy; 2uy; 3uy |], row.[4])
        Assert.Equal(SqlValue.Bool true, row.[5]))

[<Fact>]
let ``RowReader: パラメータ付き SELECT をストリーミングできる`` () =
    withDiskSession "session-reader-params" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore

        ok (session.Execute "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'a')")
        |> ignore

        match ok (session.TryOpenReader("SELECT id FROM t WHERE v = ? ORDER BY id", [| SqlValue.Text "a" |])) with
        | None -> Assert.Fail "パラメータ付き SELECT が RowReader になりませんでした"
        | Some reader ->
            use reader = reader
            let rows = drainReader reader
            Assert.Equal<int>(2, rows.Length)
            Assert.Equal<int64>(1L, asInt rows.[0].[0])
            Assert.Equal<int64>(3L, asInt rows.[1].[0]))

[<Fact>]
let ``Session: パラメータの個数不一致・Unloaded は TypeError`` () =
    withDiskSession "session-params-errors" (fun session ->
        expectErrorKind ErrorKind.TypeError "パラメータ不足" (session.Execute("SELECT ?", [||]))
        expectErrorKind ErrorKind.TypeError "パラメータ過剰" (session.Execute("SELECT 1", [| SqlValue.Int 1L |]))
        expectErrorKind ErrorKind.TypeError "RowReader でも不足" (session.TryOpenReader("SELECT ?", [||]))
        expectErrorKind ErrorKind.TypeError "Unloaded" (session.Execute("SELECT ?", [| SqlValue.Unloaded |]))
        expectErrorKind ErrorKind.TypeError "TEXT の null" (session.Execute("SELECT ?", [| SqlValue.Text null |]))
        expectErrorKind ErrorKind.TypeError "BLOB の null" (session.Execute("SELECT ?", [| SqlValue.Blob null |]))

        expectErrorKind
            ErrorKind.TypeError
            "parameters が null"
            (session.Execute("SELECT 1", Unchecked.defaultof<SqlValue array>)))

[<Fact>]
let ``RowReader: パラメータの BLOB を後から書き換えても結果は変わらない`` () =
    withDiskSession "session-reader-param-copy" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, b BLOB)") |> ignore
        ok (session.Execute "INSERT INTO t VALUES (1, X'01'), (2, X'02')") |> ignore

        let key = [| 1uy |]

        match ok (session.TryOpenReader("SELECT id FROM t WHERE b = ? ORDER BY id", [| SqlValue.Blob key |])) with
        | None -> Assert.Fail "パラメータ付き SELECT が RowReader になりませんでした"
        | Some reader ->
            use reader = reader
            // 読み始める前に呼び出し側の配列を書き換える。
            key.[0] <- 2uy
            let rows = drainReader reader
            Assert.Equal<int>(1, rows.Length)
            Assert.Equal<int64>(1L, asInt rows.[0].[0]))

[<Fact>]
let ``Session: パラメータは複数文バッチでも通し番号`` () =
    withDiskSession "session-params-batch" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore

        let result =
            ok (
                session.Execute("BEGIN; INSERT INTO t VALUES (?, ?); COMMIT", [| SqlValue.Int 1L; SqlValue.Text "a" |])
            )

        Assert.Equal<int>(1, result.RowsAffected)
        Assert.False session.InTransaction
        Assert.Equal(1L, scalar session "SELECT COUNT(*) FROM t"))

[<Fact>]
let ``Session: パラメータでも主キー検索の計画になる`` () =
    withDiskSession "session-params-plan" (fun session ->
        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        ok (session.Execute "INSERT INTO t VALUES (1, 10), (2, 20)") |> ignore

        let plan =
            ok (session.Execute("EXPLAIN SELECT * FROM t WHERE id = ?", [| SqlValue.Int 2L |]))

        let text = plan.Rows |> Array.map (fun row -> asText row.[0]) |> String.concat "\n"
        Assert.Contains("PkLookup(2)", text))

[<Fact>]
let ``Session: 並行 commit を Close して再オープンしても読める`` () =
    withDir "session-group-commit" (fun dir ->
        let session = ok (Session.OpenWith dir)

        ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        let tasks =
            [|
                for i in 1..16 do
                    System.Threading.Tasks.Task.Run(fun () ->
                        session.Execute(sprintf "INSERT INTO t VALUES (%d, %d)" i i))
            |]

        System.Threading.Tasks.Task.WaitAll(tasks |> Array.map (fun t -> t :> System.Threading.Tasks.Task))
        tasks |> Array.iter (fun t -> ok t.Result |> ignore)
        ok (session.Close())

        let reopened = ok (Session.OpenWith dir)

        try
            let selected = ok (reopened.Execute "SELECT id FROM t ORDER BY id")
            Assert.Equal<int>(16, selected.Rows.Length)
        finally
            reopened.Close() |> ignore)
