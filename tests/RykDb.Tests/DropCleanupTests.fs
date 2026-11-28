module RykDb.DropCleanupTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 走査だけ失敗させるエンジン（後始末失敗の注入用）。
type private FailingScanEngine(inner: IStorageEngine) =
    interface IStorageEngine with
        member _.NextSeqNo = inner.NextSeqNo
        member _.ActiveSnapshot = inner.ActiveSnapshot
        member _.PublishedState = inner.PublishedState
        member _.WalState = inner.WalState
        member _.LastFlushError = inner.LastFlushError
        member _.GetAtMasked key mask visible = inner.GetAtMasked key mask visible

        member _.ScanAtValues _startKey _limit _visible _needsValues =
            err (DbError.create ErrorKind.IoError "injected scan failure")

        member _.ScanAtMasked _startKey _limit _mask _visible =
            err (DbError.create ErrorKind.IoError "injected scan failure")

        member _.WriteBatch entries =
            inner.WriteBatch entries
            |> Result.map (fun next -> FailingScanEngine(next) :> IStorageEngine)

        member _.WriteBatchDeferred entries =
            inner.WriteBatchDeferred entries
            |> Result.map (fun next -> FailingScanEngine(next) :> IStorageEngine)

        member _.WithNextSeqNo next =
            FailingScanEngine(inner.WithNextSeqNo next) :> IStorageEngine

        member _.WithActiveSnapshot snapshot =
            FailingScanEngine(inner.WithActiveSnapshot snapshot) :> IStorageEngine

        member _.WithHostState state =
            FailingScanEngine(inner.WithHostState state) :> IStorageEngine

        member _.FlushIfNeeded allocate =
            inner.FlushIfNeeded allocate
            |> Result.map (fun (next, flushed) -> FailingScanEngine(next) :> IStorageEngine, flushed)

        member _.MaybeFlush() =
            inner.MaybeFlush()
            |> Result.map (fun (next, error) -> FailingScanEngine(next) :> IStorageEngine, error)

        member _.Close() = inner.Close()

[<Fact>]
let ``DropCleanupRecovery: 後始末に失敗してもジャーナルを残して続行する`` () =
    withDir "drop-cleanup-fail" (fun dir ->
        let store = ok (RowStore.openStore dir)
        let catalog = ok (Catalog.load store)
        ok (DropCleanup.write store.Root [ "t:gone" ])

        let failing =
            { store with
                Engine = FailingScanEngine store.Engine
            }

        let recovered =
            match DropCleanupRecovery.recover failing catalog (TransactionManager.create ()) with
            | Ok ready -> ready
            | Error e -> failwithf "後始末の失敗で recover が Error になりました: %s" (DbError.toDisplayString e)

        // open を止めず、ジャーナルは次の open のために残す。
        Assert.Equal<string list>([ "t:gone" ], ok (DropCleanup.pending dir))
        Assert.Equal<string>(store.Root, (fst recovered).Root)
        ok (RowStore.close store) |> ignore)

[<Fact>]
let ``DropCleanup: 未完了の後始末対象を追加・復元・消去できる`` () =
    withDir "drop-cleanup-journal" (fun dir ->
        ok (DropCleanup.write dir [ "t:a" ])
        ok (DropCleanup.add dir [ "i:b"; "t:a" ])
        Assert.Equal<string list>([ "t:a"; "i:b" ], ok (DropCleanup.pending dir))
        ok (DropCleanup.clear dir)
        Assert.Empty(ok (DropCleanup.pending dir)))
