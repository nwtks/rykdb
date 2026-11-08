// ===========================================================================
// BackgroundCompactionTests — EngineHost / background compactions の縦切りテスト。
// ===========================================================================

module RykDb.BackgroundCompactionTests

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

let private waitIdle (host: EngineHost) =
    Assert.True(SpinWait.SpinUntil((fun () -> host.IsIdle), 10000), "worker が idle になりません")

let private writeWithSeq (store: RowStore) (seqNo: uint64) (i: int) =
    ok (RowStore.writeBatches [ ("t", [| Entry.put (intKey i) seqNo (intRow i) |]) ] store)

let private writeOne (store: RowStore) (i: int) = writeWithSeq store (uint64 i) i

let private refresh (store: RowStore) =
    match store.Host with
    | None -> store
    | Some host ->
        let state = host.Current()

        { store with
            Engine = store.Engine.WithHostState state
        }

[<Fact>]
let ``background compaction: flush 後に L0 が background で compaction される`` () =
    withDir "bg-compact" (fun dir ->
        let store = openBackground dir 1 None

        try
            let host = store.Host |> Option.get
            let mutable current = store

            for i in 1..4 do
                current <- writeOne current i

            waitIdle host
            let state = host.Current()
            Assert.True((Version.filesAt 0 state.Version).Length < 4)
            Assert.True((Version.filesAt 1 state.Version).Length > 0)

            let refreshed = refresh current
            Assert.True((ok (RowStore.getAt "t" (intKey 1) None refreshed)).IsSome)
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
            let host = store.Host |> Option.get

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
                Version.allFiles (host.Current()).Version |> List.map _.FileNumber |> Set.ofList

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
let ``background compaction: install 後に再オープンしても行が残る`` () =
    withDir "bg-reopen" (fun dir ->
        let store = openBackground dir 1 None

        try
            let mutable current = store

            for i in 1..4 do
                current <- writeOne current i

            waitIdle (store.Host |> Option.get)
            RowStore.close current |> ok |> ignore

            let reopened = ok (RowStore.openStore dir)
            let refreshed = refresh reopened

            for i in 1..4 do
                Assert.True((ok (RowStore.getAt "t" (intKey i) None refreshed)).IsSome)

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

            let reopened = ok (RowStore.openStore dir)
            Assert.True((ok (RowStore.getAt "t" (intKey 1) None reopened)).IsSome)
            ok (RowStore.close reopened) |> ignore
        finally
            release.Set()
            RowStore.close store |> ignore)

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
            let host = store.Host |> Option.get

            // GC 境界 6 で書き込む（ActiveSnapshot = 6）。
            for i in 1..4 do
                current <-
                    ok (
                        RowStore.writeBatchesWithSnapshot
                            6UL
                            [ ("t", [| Entry.put (intKey i) (uint64 i) (intRow i) |]) ]
                            current
                    )

            Assert.True(started.Wait 5000, "compaction が開始されません")

            // compaction の開始後に、より古い snapshot（2）の transaction が現れる。
            let registration = ActiveSnapshotRegistry.register dir 2UL
            release.Set()
            waitIdle host
            // 開始時の境界 6 で作った出力は破棄され、境界 2 でやり直して install される。
            Assert.Equal(2UL, host.Current().ActiveSnapshot)
            Assert.True((Version.filesAt 0 (host.Current()).Version).Length < 4)

            let refreshed = refresh current

            for i in 1..4 do
                Assert.True((ok (RowStore.getAt "t" (intKey i) None refreshed)).IsSome)

            ActiveSnapshotRegistry.unregister dir registration
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
            let host = store.Host |> Option.get

            for i in 1..10 do
                current <- writeOne current i

            waitIdle host
            // Close せずに開き直す（クラッシュ相当）。
            let reopened = ok (RowStore.openStoreTakingOver dir)

            for i in 1..10 do
                Assert.True((ok (RowStore.getAt "t" (intKey i) None reopened)).IsSome)

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
            let host = store.Host |> Option.get

            for i in 1..60 do
                current <- writeOne current i

            waitIdle host
            let state = host.Current()
            let files = Version.allFiles state.Version
            Assert.Equal(files.Length, (files |> List.map _.FileNumber |> List.distinct).Length)

            files
            |> List.iter (fun f -> Assert.True(File.Exists(FileMetaData.path dir f.FileNumber)))

            let refreshed = refresh current

            for i in 1..60 do
                Assert.True((ok (RowStore.getAt "t" (intKey i) None refreshed)).IsSome)
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

            Assert.Equal(4, (Version.filesAt 0 current.Engine.HostState.Version).Length)
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
            let host = reopened.Host |> Option.get
            waitIdle host
            Assert.True((Version.filesAt 0 (host.Current()).Version).Length < 4)
            Assert.True((Version.filesAt 1 (host.Current()).Version).Length > 0)
        finally
            RowStore.close reopened |> ignore)
