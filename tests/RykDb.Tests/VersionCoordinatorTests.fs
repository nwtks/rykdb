// ===========================================================================
// VersionCoordinatorTests — VersionCoordinator / background compaction の縦切りテスト。
// ===========================================================================

module RykDb.VersionCoordinatorTests

open System.Diagnostics
open System.IO
open System.Threading
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private openBackgroundWith dir options =
    ok (RowStore.openStoreWithOptions options dir)

let private openBackground dir memLimit scanHook =
    openBackgroundWith
        dir
        { RowStoreOptions.Background with
            MemLimitBytes = Some memLimit
            ScanHook = scanHook
        }

let private forceL1Compaction =
    let limits = Array.copy CompactionOptions.Defaults.MaxLevelBytes
    limits.[1] <- 0L

    { CompactionOptions.Defaults with
        MaxLevelBytes = limits
    }

let private waitIdle (host: VersionCoordinator) =
    Assert.True(SpinWait.SpinUntil((fun () -> host.IsIdle), 10000), "worker が idle になりません")

let private writeWithSeq (store: RowStore) (seqNo: uint64) (i: int) =
    ok (RowStore.writeBatches [ ("t", [| Entry.put (intKey i) seqNo (intRow i) |]) ] store)

let private writeOne (store: RowStore) (i: int) = writeWithSeq store (uint64 i) i

let private refresh (store: RowStore) =
    match store.Host with
    | None -> store
    | Some host ->
        let state = host.Current

        { store with
            Engine = store.Engine.WithHostState state
        }

[<Fact>]
let ``background compaction: flush 後に L0 が background で compaction される`` () =
    withDir "bg-compact" (fun dir ->
        let store = openBackground dir 1 None

        try
            let host = expectSome "store.Host" store.Host
            let mutable current = store

            for i in 1..4 do
                current <- writeOne current i

            waitIdle host
            let state = host.Current
            Assert.True((Version.filesAt 0 state.Version).Length < 4)
            Assert.True((Version.filesAt 1 state.Version).Length > 0)

            let refreshed = refresh current
            Assert.True((ok (RowStore.getAtMasked "t" (intKey 1) None [||] refreshed)).IsSome)
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: compaction 中でも次の write が返る`` () =
    withDir "bg-nonblocking" (fun dir ->
        let started = new ManualResetEventSlim false
        let release = new ManualResetEventSlim false

        let store =
            openBackground
                dir
                1
                (Some(fun _ ->
                    started.Set()
                    release.Wait 10000 |> ignore))

        let mutable current = store

        try
            for i in 1..4 do
                current <- writeOne current i

            Assert.True(started.Wait 5000, "compaction が開始されません")

            let stopwatch = Stopwatch.StartNew()
            current <- writeOne current 5
            stopwatch.Stop()
            Assert.True(stopwatch.ElapsedMilliseconds < 1000, "commit が compaction を待っています")
        finally
            release.Set()
            RowStore.close current |> ignore)

[<Fact>]
let ``background compaction: 読み取り中のファイルは削除されず、lease 解放後に削除される`` () =
    withDir "bg-refcount" (fun dir ->
        let store =
            openBackgroundWith
                dir
                { RowStoreOptions.Background with
                    MemLimitBytes = Some 1
                    Compaction = Some forceL1Compaction
                }

        let mutable current = store

        try
            let host = expectSome "store.Host" store.Host

            for i in 1..4 do
                current <- writeOne current i

            waitIdle host
            let state, lease = host.AcquireCall()
            let pinned = Version.allFiles state.Version |> List.map _.FileNumber
            Assert.NotEmpty pinned

            // 同じキー域を再書き込みして、pinned ファイルを compaction の入力にする。
            for i in 1..4 do
                current <- writeWithSeq current (uint64 (100 + i)) i

            waitIdle host

            // compaction で実際に Version から外れたファイルだけを対象にする。
            let stillReferenced =
                Version.allFiles host.Current.Version |> List.map _.FileNumber |> Set.ofList

            let retired = pinned |> List.filter (fun n -> not (Set.contains n stillReferenced))
            Assert.NotEmpty retired

            retired
            |> List.iter (fun n -> Assert.True(File.Exists(FileMetaData.path dir n)))

            lease.Dispose()

            retired
            |> List.iter (fun n -> Assert.False(File.Exists(FileMetaData.path dir n)))
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: 診断情報と再試行要求を公開する`` () =
    withDir "bg-diagnostics" (fun dir ->
        let store = openBackground dir 1 None

        try
            let host = expectSome "store.Host" store.Host
            waitIdle host
            Assert.True host.LastCompactionError.IsNone
            host.Signal()
            waitIdle host
            Assert.True host.LastCompactionError.IsNone
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``RowStore.scanAtRawMasked: 正常終了で ScanScope の登録が外れる`` () =
    withDir "bg-scan-scope" (fun dir ->
        let store = openBackground dir 1 None

        try
            let mutable current = store

            for i in 1..4 do
                current <- writeOne current i

            let host = expectSome "store.Host" store.Host
            waitIdle host

            let snapStore, lease = RowStore.acquireSnapshot current
            let scope = ScanScope()
            let scan = ok (RowStore.scanAtRawMasked "t" [||] 0 None (Some scope) [||] snapStore)
            let rows = ok (SstScan.toArray scan)
            Assert.NotEmpty rows
            Assert.Equal(0, scope.ActiveCount)
            lease.Dispose()
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: 走査は read lease より長く Version を pin する`` () =
    withDir "bg-scan-pin" (fun dir ->
        let store = openBackground dir 1 None

        try
            let mutable current = store

            for i in 1..4 do
                current <- writeOne current i

            let host = expectSome "store.Host" store.Host
            waitIdle host

            // 現在の Version を取得し、走査だけが pin を持つ状態にする。
            let snapStore, lease = RowStore.acquireSnapshot current
            let scan = ok (RowStore.scan "t" [||] 0 snapStore)
            lease.Dispose()

            // 同じキーを新しい SeqNo で書き、compaction の入力を retired にする。
            for i in 1..4 do
                current <- writeWithSeq current (uint64 (100 + i)) i

            waitIdle host

            // 走査が Version を pin しているので、retired になってもファイルは残り、
            // 最後まで読める。
            let rows = ok (SstScan.toArray scan)
            Assert.Equal(4, rows.Length)
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: install 後に再オープンしても行が残る`` () =
    withDir "bg-reopen" (fun dir ->
        let store = openBackground dir 1 None

        try
            let mutable current = store

            for i in 1..4 do
                current <- writeOne current i

            waitIdle (expectSome "store.Host" store.Host)
            RowStore.close current |> ok |> ignore

            let reopened = ok (RowStore.openStore dir)
            let refreshed = refresh reopened

            for i in 1..4 do
                Assert.True((ok (RowStore.getAtMasked "t" (intKey i) None [||] refreshed)).IsSome)

            ok (RowStore.close reopened) |> ignore
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: close は進行中の compaction を停止して再オープンできる`` () =
    withDir "bg-close" (fun dir ->
        let started = new ManualResetEventSlim false
        let release = new ManualResetEventSlim false

        let store =
            openBackground
                dir
                1
                (Some(fun _ ->
                    started.Set()
                    release.Wait 10000 |> ignore))

        let mutable current = store

        try
            for i in 1..4 do
                current <- writeOne current i

            Assert.True(started.Wait 5000, "compaction が開始されません")
            let closing = System.Threading.Tasks.Task.Run(fun () -> RowStore.close current)
            release.Set()
            Assert.True(closing.Wait 10000, "close が完了しません")

            // 停止した compaction が入力のハンドルを残していない
            // （Linux は開いたままでも再オープンできるため、この検査が無いと Windows だけで露見する）。
            for path in Directory.GetFiles(dir, "*.sst") do
                use _ = new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
                ()

            let reopened = ok (RowStore.openStore dir)
            Assert.True((ok (RowStore.getAtMasked "t" (intKey 1) None [||] reopened)).IsSome)
            ok (RowStore.close reopened) |> ignore
        finally
            release.Set()
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: Pending が残った状態でも close が完了する`` () =
    withDir "bg-close-pending" (fun dir ->
        let store = openBackground dir 1 None
        let host = expectSome "store.Host" store.Host
        waitIdle host

        // worker が signal を消費した直後に Pending だけが残った状態を再現する。
        // close が Pending を見て起床を省略すると、worker join が永久に待つ。
        host.MarkPendingWithoutSignal()

        let closing =
            System.Threading.Tasks.Task.Run(fun () -> RowStore.close store |> ignore)

        Assert.True(closing.Wait 10000, "Pending が残っていると close が完了しません"))

[<Fact>]
let ``background compaction: 開始後に古い snapshot が現れたら境界を下げてやり直す`` () =
    withDir "bg-gc-boundary" (fun dir ->
        let started = new ManualResetEventSlim false
        let release = new ManualResetEventSlim false
        let calls = ref 0

        let hook _ =
            if Interlocked.Increment calls = 1 then
                started.Set()
                release.Wait 10000 |> ignore

        let store =
            openBackgroundWith
                dir
                { RowStoreOptions.Background with
                    MemLimitBytes = Some 1
                    Compaction = Some forceL1Compaction
                    ScanHook = Some hook
                }

        let mutable current = store

        try
            let host = expectSome "store.Host" store.Host

            // GC 境界 6 で書き込む（ActiveSnapshot = 6）。
            for i in 1..4 do
                current <-
                    ok (
                        RowStore.writeBatchesWithSnapshot
                            6UL
                            [ ("t", [| Entry.put (intKey i) (uint64 i) (intRow i) |]) ]
                            current
                    )

            let diag () =
                let st = host.Current

                sprintf
                    "compaction が開始されません (pending=%d threads=%d L0=%d L1=%d gen=%d err=%A)"
                    System.Threading.ThreadPool.PendingWorkItemCount
                    System.Threading.ThreadPool.ThreadCount
                    (Version.filesAt 0 st.Version).Length
                    (Version.filesAt 1 st.Version).Length
                    st.Generation
                    st.LastCompactionError

            Assert.True(started.Wait 5000, diag ())

            // compaction の開始後に、より古い snapshot（2）の transaction が現れる。
            let registration = store.Runtime.Register(dir, 2UL)
            release.Set()
            waitIdle host
            // 開始時の境界 6 で作った出力は破棄され、境界 2 でやり直して install される。
            Assert.Equal(2UL, host.Current.ActiveSnapshot)
            Assert.True((Version.filesAt 0 host.Current.Version).Length < 4)

            let refreshed = refresh current

            for i in 1..4 do
                Assert.True((ok (RowStore.getAtMasked "t" (intKey i) None [||] refreshed)).IsSome)

            store.Runtime.Unregister registration
        finally
            release.Set()
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: Close しなくても install 済みの行は再オープンで残る`` () =
    withDir "bg-crash" (fun dir ->
        let store =
            openBackgroundWith
                dir
                { RowStoreOptions.Background with
                    MemLimitBytes = Some 1
                    Compaction = Some forceL1Compaction
                }

        let mutable current = store

        try
            let host = expectSome "store.Host" store.Host

            for i in 1..10 do
                current <- writeOne current i

            waitIdle host
            // Close せずに開き直す（クラッシュ相当）。
            let reopened = ok (RowStore.openStoreTakingOver dir)

            for i in 1..10 do
                Assert.True((ok (RowStore.getAtMasked "t" (intKey i) None [||] reopened)).IsSome)

            ok (RowStore.close reopened) |> ignore
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: flush と install が競合してもファイル番号が重複しない`` () =
    withDir "bg-filenumber" (fun dir ->
        let store =
            openBackgroundWith
                dir
                { RowStoreOptions.Background with
                    MemLimitBytes = Some 1
                    Compaction = Some forceL1Compaction
                }

        let mutable current = store

        try
            let host = expectSome "store.Host" store.Host

            for i in 1..60 do
                current <- writeOne current i

            waitIdle host
            let state = host.Current
            let files = Version.allFiles state.Version
            Assert.Equal(files.Length, (files |> List.map _.FileNumber |> List.distinct).Length)

            files
            |> List.iter (fun f -> Assert.True(File.Exists(FileMetaData.path dir f.FileNumber)))

            let refreshed = refresh current

            for i in 1..60 do
                Assert.True((ok (RowStore.getAtMasked "t" (intKey i) None [||] refreshed)).IsSome)
        finally
            RowStore.close store |> ignore)

[<Fact>]
let ``background compaction: DB を開いた時点で未 compaction の L0 を解消する`` () =
    withDir "bg-open-trigger" (fun dir ->
        // L0 の閾値を上げた同期モードで、L0 に 4 本残したまま閉じる。
        let quiet =
            { CompactionOptions.Defaults with
                L0CompactionTrigger = System.Int32.MaxValue
                MaxLevelBytes = Array.create Levels.Count System.Int64.MaxValue
            }

        let store =
            RowStore.openStoreWithOptions
                { RowStoreOptions.Default with
                    MemLimitBytes = Some 1
                    Compaction = Some quiet
                }
                dir
            |> ok

        let mutable current = store

        try
            for i in 1..4 do
                current <- writeOne current i

            Assert.Equal(4, (Version.filesAt 0 current.Engine.PublishedState.Version).Length)
            ok (RowStore.close current) |> ignore
        with _ ->
            RowStore.close current |> ignore

        // 既定の設定で background モードで開き直すと、オープン時のシグナルで L0 が畳まれる。
        let reopened =
            { RowStoreOptions.Background with
                MemLimitBytes = Some 1
            }
            |> fun options -> RowStore.openStoreWithOptions options dir
            |> ok

        try
            let host = expectSome "reopened.Host" reopened.Host
            waitIdle host
            Assert.True((Version.filesAt 0 host.Current.Version).Length < 4)
            Assert.True((Version.filesAt 1 host.Current.Version).Length > 0)
        finally
            RowStore.close reopened |> ignore)

[<Fact>]
let ``background compaction: 書き込み・読み取り・snapshot pin を並行に回しても壊れない`` () =
    withDir "bg-stress" (fun dir ->
        let store = openBackground dir 1 None
        let writes = 120
        let mutable current = store

        try
            let host = expectSome "store.Host" store.Host
            use stop = new CancellationTokenSource()

            // pin した時点の Version だけを読む。
            // 読み取り中に compaction がファイルを消さないことは refcount が担保する。
            let readLoop (seed: int) =
                let rnd = System.Random seed

                let rec loop () =
                    if not stop.IsCancellationRequested then
                        let state, lease = host.AcquireCall()

                        try
                            let view =
                                { store with
                                    Engine = store.Engine.WithHostState state
                                }

                            let key = rnd.Next(1, writes + 1)

                            match RowStore.getAtMasked "t" (intKey key) None [| true |] view |> ok with
                            // まだ書かれていないキーは None でよい（書き込みと競合する）
                            | Some row -> Assert.Equal<SqlValue>(SqlValue.Int(int64 key), row.[0])
                            | None -> ()

                            if rnd.Next 4 = 0 then
                                Thread.Sleep(rnd.Next(1, 3))
                        finally
                            lease.Dispose()

                        loop ()

                loop ()

            // lease を長めに握り、retire / sweep を揺らす。
            let pinLoop (seed: int) =
                let rnd = System.Random seed

                let rec loop () =
                    if not stop.IsCancellationRequested then
                        let _, lease = host.AcquireCall()
                        Thread.Sleep(rnd.Next(1, 4))
                        lease.Dispose()
                        loop ()

                loop ()

            let readers =
                [| for i in 1..3 -> System.Threading.Tasks.Task.Run(fun () -> readLoop i) |]

            let pins =
                [|
                    for i in 1..2 -> System.Threading.Tasks.Task.Run(fun () -> pinLoop (100 + i))
                |]

            try
                for i in 1..writes do
                    current <- writeOne current i

                    if i % 20 = 0 then
                        Thread.Sleep 2
            finally
                stop.Cancel()

            Assert.True(System.Threading.Tasks.Task.WaitAll(Array.append readers pins, 15000), "並行タスクが停止しません")

            waitIdle host

            let state = host.Current
            ok (validateVersion state.Version) |> ignore

            match state.LastCompactionError with
            | Some e -> Assert.Fail(sprintf "background compaction が失敗しました: %s" (DbError.toDisplayString e))
            | None -> ()

            for file in Version.allFiles state.Version do
                Assert.True(
                    System.IO.File.Exists(FileMetaData.path dir file.FileNumber),
                    sprintf "Version が参照するファイルがありません: %d" file.FileNumber
                )

            // 最後の状態からは全キーが読める（欠落・重複・古い値が無い）。
            let refreshed = refresh current

            for i in 1..writes do
                match RowStore.getAtMasked "t" (intKey i) None [| true |] refreshed |> ok with
                | Some row -> Assert.Equal<SqlValue>(SqlValue.Int(int64 i), row.[0])
                | None -> Assert.Fail(sprintf "キー %d が読めません" i)
        finally
            RowStore.close current |> ignore)

[<Fact>]
let ``background compaction: ランダムな操作列でも Version と refcount の不変条件を保つ`` () =
    withDir "bg-model" (fun dir ->
        let store = openBackground dir 1 None
        let operations = 200
        let mutable current = store

        try
            let host = expectSome "store.Host" store.Host
            let rnd = System.Random 20261117
            let mutable written = 0
            let mutable lastGeneration = -1L
            let mutable lastFileNumber = 0L
            let leases = ResizeArray<int * System.IDisposable * RowStore>()

            let checkVersion (state: PublishedState) =
                ok (validateVersion state.Version) |> ignore

                Assert.True(
                    state.Generation >= lastGeneration,
                    sprintf "Generation が戻りました: %d -> %d" lastGeneration state.Generation
                )

                lastGeneration <- state.Generation

                Assert.True(
                    state.Version.NextFileNumber >= lastFileNumber,
                    sprintf "NextFileNumber が戻りました: %d -> %d" lastFileNumber state.Version.NextFileNumber
                )

                lastFileNumber <- state.Version.NextFileNumber

                match state.LastCompactionError with
                | Some e -> Assert.Fail(sprintf "compaction が失敗: %s" (DbError.toDisplayString e))
                | None -> ()

            // 現在の Version を pin してから検査する（検査中に retire されない）。
            let pinnedCheck () =
                let state, lease = host.AcquireCall()
                use _ = lease
                checkVersion state

                for file in Version.allFiles state.Version do
                    Assert.True(
                        System.IO.File.Exists(FileMetaData.path dir file.FileNumber),
                        sprintf "Version が参照するファイルがありません: %d" file.FileNumber
                    )

            let expireLeases () =
                let next = ResizeArray<int * System.IDisposable * RowStore>()

                for remaining, lease, view in leases do
                    if remaining <= 1 then
                        lease.Dispose()
                    else
                        next.Add((remaining - 1, lease, view))

                leases.Clear()
                leases.AddRange next

            let readAt (requireHit: bool) (view: RowStore) =
                if written > 0 then
                    let key = rnd.Next(1, written + 1)

                    match RowStore.getAtMasked "t" (intKey key) None [| true |] view |> ok with
                    | Some row -> Assert.Equal<SqlValue>(SqlValue.Int(int64 key), row.[0])
                    | None -> Assert.False(requireHit, sprintf "キー %d が pin 済みの state から読めません" key)

            try
                for step in 1..operations do
                    expireLeases ()

                    match rnd.Next 100 with
                    | n when n < 55 ->
                        written <- written + 1
                        current <- writeOne current written
                    | n when n < 80 ->
                        // 保持中の lease があれば、その古い state から読む（None もあり得る）。
                        if leases.Count > 0 then
                            let _, _, view = leases.[rnd.Next(leases.Count)]
                            readAt false view
                        else
                            let state, lease = host.AcquireCall()
                            use _ = lease

                            readAt
                                true
                                { store with
                                    Engine = store.Engine.WithHostState state
                                }
                    | n when n < 92 ->
                        // lease を数操作ぶん保持して retire / sweep を揺らす。
                        let state, lease = host.AcquireCall()

                        leases.Add(
                            (rnd.Next(1, 6),
                             lease,
                             { store with
                                 Engine = store.Engine.WithHostState state
                             })
                        )
                    | _ -> Thread.Sleep(rnd.Next(0, 2))

                    if step % 25 = 0 then
                        pinnedCheck ()
            finally
                for _, lease, _ in leases do
                    lease.Dispose()

                leases.Clear()

            waitIdle host
            pinnedCheck ()

            // 最終状態からは全キーが読める（欠落・torn write が無い）。
            let refreshed = refresh current

            for i in 1..written do
                match RowStore.getAtMasked "t" (intKey i) None [| true |] refreshed |> ok with
                | Some row -> Assert.Equal<SqlValue>(SqlValue.Int(int64 i), row.[0])
                | None -> Assert.Fail(sprintf "キー %d が読めません" i)
        finally
            RowStore.close current |> ignore)
