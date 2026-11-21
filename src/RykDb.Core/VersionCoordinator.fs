// ===========================================================================
// VersionCoordinator — background compaction の共有状態。
//
// 不変の StorageEngine / Version の外側に、MANIFEST に反映済みの現在状態だけを持つ。
// flush と compaction worker はこの coordinator を通して Version を公開する。
//
// * 状態は ConcurrentDictionary の単一セル + TryUpdate の末尾再帰 CAS で更新する。
// * MANIFEST 追記・install は installGate で直列化し、
//   ファイル番号採番は ConcurrentDictionary の CAS で行う。
// * 読み取りは SELECT のみのバッチ単位で Version 全ファイルを pin する。
//   install は入力を retired にし、refcount が 0 になってから実ファイルを消す。
// ===========================================================================

namespace RykDb.Core

/// 1ファイルの pin / retired 状態。
type private FileRefState = { Pins: int; Retired: bool }

/// compaction worker の状態（未起動 / 起動中 / 実行中 / 停止を 1つの値で持つ）。
[<RequireQualifiedAccess>]
type private WorkerState =
    /// まだ起動していない。
    | NotStarted
    /// タスクを作成中（起動の並行呼び出しで 2本目を走らせないための予約）。
    | Starting of bool
    /// 起動済み。`activity` は `Idle` / `Busy` / `Pending` のビット和。
    | Running of System.Threading.Tasks.Task * int
    /// 停止済み（終端）。
    | Stopped

/// background compaction の共有状態。
/// `initial` は open 直後の MANIFEST 反映済み状態。
type internal VersionCoordinator
    (
        dir: string,
        initial: PublishedState,
        options: CompactionOptions,
        oldestSnapshot: unit -> uint64 option,
        scanOf: FileMetaData -> Result<SstScan, DbError>,
        scanHook: (FileMetaData -> unit) option,
        manifestApply: (int64<manifestNo> -> Version -> VersionEdit -> ManifestApplyResult) option
    ) =
    let stateKey = "current"
    let workerKey = "worker"

    let applyManifest =
        defaultArg manifestApply (fun manifestNumber version edit ->
            Manifest.logAndApplyWithOutcome dir manifestNumber version edit)

    [<Literal>]
    let Idle = 0

    [<Literal>]
    let Busy = 1

    [<Literal>]
    let Pending = 2

    let stateCell =
        System.Collections.Concurrent.ConcurrentDictionary<string, PublishedState>()

    do stateCell.[stateKey] <- initial

    let fileRefs =
        System.Collections.Concurrent.ConcurrentDictionary<int64, FileRefState>()

    /// Version 単位の参照数。最初の lease でファイルを pin し、最後の lease で release する。
    /// 参照等価で数える（構造が同じ別の Version を取り違えない）。
    let versionPins =
        System.Collections.Concurrent.ConcurrentDictionary<Version, int> HashIdentity.Reference

    /// worker のライフサイクルと activity を 1セルで持つ（遷移は CAS 1回）。
    let workerCell =
        System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>()

    do workerCell.[workerKey] <- WorkerState.NotStarted

    let installGate = new System.Threading.SemaphoreSlim(1, 1)
    let cancel = new System.Threading.CancellationTokenSource()
    let signal = new System.Threading.SemaphoreSlim(0, System.Int32.MaxValue)

    let tryDeleteFile (fileNumber: int64) =
        match
            protect (fun () ->
                let path = FileMetaData.path dir fileNumber

                if System.IO.File.Exists path then
                    System.IO.File.Delete path)
        with
        | Ok() -> true
        | Error _ -> false

    let withGate (f: unit -> 'T) =
        installGate.Wait()

        try
            f ()
        finally
            installGate.Release() |> ignore

    /// 状態を CAS で更新する。
    /// <remarks>
    /// `NextFileNumber` は巻き戻さない。採番は `allocateFileNumber` が
    /// 同じセルの CAS で進めるので、別のセルに持たない。
    /// </remarks>
    let publish (update: PublishedState -> PublishedState) =
        let rec loop () =
            let current = stateCell.[stateKey]

            let updated =
                update
                    { current with
                        Generation = current.Generation + 1L
                    }

            let next =
                { updated with
                    Version =
                        { updated.Version with
                            NextFileNumber = max updated.Version.NextFileNumber current.Version.NextFileNumber
                        }
                }

            if stateCell.TryUpdate(stateKey, next, current) then
                next
            else
                loop ()

        loop ()

    /// ファイル番号を 1つ予約する。
    /// Version を公開せずに `NextFileNumber` だけ進める（`Generation` は上げない）。
    let allocateFileNumber () =
        let rec loop () =
            let current = stateCell.[stateKey]
            let number = current.Version.NextFileNumber

            let next =
                { current with
                    Version =
                        { current.Version with
                            NextFileNumber = number + 1L
                        }
                }

            if stateCell.TryUpdate(stateKey, next, current) then
                number
            else
                loop ()

        loop ()

    let pinFile (fileNumber: int64) =
        let rec loop () =
            match fileRefs.TryGetValue fileNumber with
            | true, state ->
                if not (fileRefs.TryUpdate(fileNumber, { state with Pins = state.Pins + 1 }, state)) then
                    loop ()
            | false, _ ->
                if not (fileRefs.TryAdd(fileNumber, { Pins = 1; Retired = false })) then
                    loop ()

        loop ()

    /// Version の参照を 1つ増やす。
    /// 初回だけ全ファイルを pin するので、同じ Version への並行 lease は O(1) で増やせる。
    let pinVersion (version: Version) =
        let rec loop () =
            match versionPins.TryGetValue version with
            | true, count ->
                if not (versionPins.TryUpdate(version, count + 1, count)) then
                    loop ()
            | false, _ ->
                if versionPins.TryAdd(version, 1) then
                    Version.allFiles version |> List.iter (fun f -> pinFile f.FileNumber)
                else
                    loop ()

        loop ()

    /// 参照を 1つ減らす。0 かつ retired なら実ファイルを消す。
    let releaseFile (fileNumber: int64) =
        let rec loop () =
            match fileRefs.TryGetValue fileNumber with
            | true, state ->
                if state.Pins <= 0 then
                    false
                elif not (fileRefs.TryUpdate(fileNumber, { state with Pins = state.Pins - 1 }, state)) then
                    loop ()
                elif state.Pins > 1 then
                    false
                elif state.Retired then
                    if
                        fileRefs.TryRemove(System.Collections.Generic.KeyValuePair(fileNumber, { state with Pins = 0 }))
                    then
                        if not (tryDeleteFile fileNumber) then
                            fileRefs.TryAdd(fileNumber, { Pins = 0; Retired = true }) |> ignore

                        true
                    else
                        loop ()
                else
                    fileRefs.TryRemove(System.Collections.Generic.KeyValuePair(fileNumber, { state with Pins = 0 }))
                    |> ignore

                    false
            | false, _ -> false

        loop ()

    let retireFile (fileNumber: int64) =
        let rec loop () =
            match fileRefs.TryGetValue fileNumber with
            | true, state ->
                if state.Retired then
                    ()
                elif not (fileRefs.TryUpdate(fileNumber, { state with Retired = true }, state)) then
                    loop ()
            | false, _ ->
                if not (fileRefs.TryAdd(fileNumber, { Pins = 0; Retired = true })) then
                    loop ()

        loop ()

    /// retired かつ参照 0 のファイルを削除する（失敗したら次回に再試行）。
    let sweep () =
        fileRefs.ToArray()
        |> Array.iter (fun pair ->
            if pair.Value.Retired && pair.Value.Pins = 0 then
                if fileRefs.TryRemove(System.Collections.Generic.KeyValuePair(pair.Key, pair.Value)) then
                    if not (tryDeleteFile pair.Key) then
                        fileRefs.TryAdd(pair.Key, { Pins = 0; Retired = true }) |> ignore)

    /// Version の参照を 1つ減らす。最後の参照なら全ファイルを release する。
    let releaseVersion (version: Version) =
        let rec loop () =
            match versionPins.TryGetValue version with
            | true, count when count > 1 ->
                if not (versionPins.TryUpdate(version, count - 1, count)) then
                    loop ()
            | true, 1 ->
                if versionPins.TryRemove(System.Collections.Generic.KeyValuePair(version, 1)) then
                    Version.allFiles version
                    |> List.iter (fun f -> releaseFile f.FileNumber |> ignore)

                    sweep ()
                else
                    loop ()
            | _ -> ()

        loop ()

    let scan (f: FileMetaData) =
        Option.iter (fun hook -> hook f) scanHook
        scanOf f

    let sameFileSet (a: Version) (b: Version) =
        let numbers v =
            Version.allFiles v |> List.map _.FileNumber |> Set.ofList

        numbers a = numbers b

    let install (baseState: PublishedState, result: CompactionResult) =
        withGate (fun () ->
            let current = stateCell.[stateKey]

            let currentNumbers =
                Version.allFiles current.Version |> List.map _.FileNumber |> Set.ofList

            let inputsPresent =
                result.Removed
                |> List.forall (fun f -> Set.contains f.FileNumber currentNumbers)

            let oldestNow =
                match oldestSnapshot () with
                | Some snapshot -> min current.ActiveSnapshot snapshot
                | None -> current.ActiveSnapshot

            if not inputsPresent || oldestNow < baseState.ActiveSnapshot then
                FileMetaData.deleteObsolete dir result.Added |> ignore

                publish (fun state ->
                    { state with
                        ActiveSnapshot = oldestNow
                        LastCompactionError = None
                    })
                |> ignore
            else
                let edit =
                    {
                        VersionEdit.AddedFiles = result.Added
                        DeletedFiles = result.Removed
                    }

                match applyManifest current.ManifestNumber current.Version edit with
                | ManifestApplyResult.NotApplied e ->
                    FileMetaData.deleteObsolete dir result.Added |> ignore

                    publish (fun state ->
                        { state with
                            LastCompactionError = Some e
                        })
                    |> ignore
                | ManifestApplyResult.Unknown e ->
                    let expected = VersionEdit.apply current.Version edit

                    match Manifest.openOrCreate dir with
                    | Ok(version, manifestNumber) when sameFileSet version expected ->
                        publish (fun state ->
                            { state with
                                Version = version
                                ManifestNumber = manifestNumber
                                LastCompactionError = None
                            })
                        |> ignore

                        result.Removed |> List.iter (fun f -> retireFile f.FileNumber)
                    | Ok _ ->
                        FileMetaData.deleteObsolete dir result.Added |> ignore

                        publish (fun state ->
                            { state with
                                LastCompactionError = Some e
                            })
                        |> ignore
                    | Error _ ->
                        publish (fun state ->
                            { state with
                                LastCompactionError = Some e
                            })
                        |> ignore
                | ManifestApplyResult.Applied(version, manifestNumber) ->
                    publish (fun state ->
                        { state with
                            Version = version
                            ManifestNumber = manifestNumber
                            LastCompactionError = None
                        })
                    |> ignore

                    result.Removed |> List.iter (fun f -> retireFile f.FileNumber))

        sweep ()

    let rec runCycle remaining =
        if remaining <= 0 then
            ()
        else
            let baseState = stateCell.[stateKey]
            let shouldStop () = cancel.IsCancellationRequested

            match
                Compaction.compactWith
                    dir
                    baseState.Version
                    options
                    baseState.ActiveSnapshot
                    scan
                    allocateFileNumber
                    shouldStop
            with
            | Error e ->
                publish (fun state ->
                    { state with
                        LastCompactionError = Some e
                    })
                |> ignore

                sweep ()
            | Ok(result, _) ->
                if List.isEmpty result.Added && List.isEmpty result.Removed then
                    publish (fun state ->
                        { state with
                            LastCompactionError = None
                        })
                    |> ignore

                    sweep ()
                else
                    install (baseState, result)

                    if not cancel.IsCancellationRequested then
                        runCycle (remaining - 1)

    /// 実行中の activity を書き換える（`Running` 以外では何もしない）。
    let updateActivity (update: int -> int) =
        let rec loop () =
            match workerCell.[workerKey] with
            | WorkerState.Running(task, activity) ->
                let next = update activity

                if next = activity then
                    activity
                elif
                    workerCell.TryUpdate(
                        workerKey,
                        WorkerState.Running(task, next),
                        WorkerState.Running(task, activity)
                    )
                then
                    next
                else
                    loop ()
            | WorkerState.NotStarted
            | WorkerState.Starting _
            | WorkerState.Stopped -> Idle

        loop ()

    /// 「追加の要求あり」を立てる。既に立っていれば true。
    let markPending () =
        let rec loop () =
            match workerCell.[workerKey] with
            | WorkerState.NotStarted
            | WorkerState.Stopped -> true
            | WorkerState.Starting true -> true
            | WorkerState.Starting false ->
                if workerCell.TryUpdate(workerKey, WorkerState.Starting true, WorkerState.Starting false) then
                    false
                else
                    loop ()
            | WorkerState.Running(task, activity) ->
                if activity &&& Pending <> 0 then
                    true
                elif
                    workerCell.TryUpdate(
                        workerKey,
                        WorkerState.Running(task, activity ||| Pending),
                        WorkerState.Running(task, activity)
                    )
                then
                    false
                else
                    loop ()

        loop ()

    let signalWork () =
        if not (markPending ()) && signal.CurrentCount = 0 then
            signal.Release() |> ignore

    let finishWork () =
        updateActivity (fun current -> if current &&& Pending <> 0 then Pending else Idle)
        |> ignore

    let loop () =
        let rec step () =
            signal.Wait()

            if cancel.IsCancellationRequested then
                ()
            else
                updateActivity (fun current -> current ||| Busy &&& ~~~Pending) |> ignore

                try
                    runCycle 4
                with e ->
                    publish (fun state ->
                        { state with
                            LastCompactionError = Some(DbError.internalError e.Message)
                        })
                    |> ignore

                finishWork ()
                step ()

        step ()

    let rec claimStart () =
        match workerCell.[workerKey] with
        | WorkerState.NotStarted ->
            if workerCell.TryUpdate(workerKey, WorkerState.Starting false, WorkerState.NotStarted) then
                true
            else
                claimStart ()
        | WorkerState.Starting _
        | WorkerState.Running _
        | WorkerState.Stopped -> false

    let rec attachStart (task: System.Threading.Tasks.Task) =
        match workerCell.[workerKey] with
        | WorkerState.Starting pending ->
            let activity = if pending then Pending else Idle

            if
                not (workerCell.TryUpdate(workerKey, WorkerState.Running(task, activity), WorkerState.Starting pending))
            then
                attachStart task
        | _ -> ()

    let rec claimShutdown () =
        let current = workerCell.[workerKey]

        match current with
        | WorkerState.Stopped -> None
        | WorkerState.NotStarted
        | WorkerState.Starting _ ->
            if workerCell.TryUpdate(workerKey, WorkerState.Stopped, current) then
                None
            else
                claimShutdown ()
        | WorkerState.Running(task, _) ->
            if workerCell.TryUpdate(workerKey, WorkerState.Stopped, current) then
                Some task
            else
                claimShutdown ()

    /// 現在の状態。
    member _.Current = stateCell.[stateKey]

    /// 直近の compaction の失敗（成功したら None）。
    member _.LastCompactionError = stateCell.[stateKey].LastCompactionError

    /// install / MANIFEST 追記を直列化する。
    member _.WithInstallGate(f: unit -> 'T) = withGate f

    member _.Publish(update: PublishedState -> PublishedState) = publish update

    member _.AllocateFileNumber() = allocateFileNumber ()

    /// 現在 Version のスナップショットと、その Version を pin した lease を返す。
    /// 同じ Version への複数 lease はファイル数に比例しない（初回だけ全ファイルを pin する）。
    member _.AcquireCall() =
        withGate (fun () ->
            let state = stateCell.[stateKey]
            pinVersion state.Version

            state,
            { new System.IDisposable with
                member _.Dispose() = releaseVersion state.Version
            })

    /// 指定した Version を pin する lease を返す。
    /// 走査が呼び出し元の read lease より長生きする場合に使う。
    /// pin / release は CAS だけで完結するので installGate を取らない
    /// （point lookup ごとに呼ばれるため、MANIFEST 追記と競合させない）。
    member _.AcquireVersion(version: Version) =
        pinVersion version

        { new System.IDisposable with
            member _.Dispose() = releaseVersion version
        }

    member _.Signal() = signalWork ()

    /// worker を起動する。長時間ブロックして回り続ける専用スレッドにする
    /// （スレッドプールだと混雑時に `WaitingToRun` のまま compaction が開始されないことがある）。
    /// 二重起動は無視する。
    member _.Start() =
        if claimStart () then
            System.Threading.Tasks.Task.Factory.StartNew(
                loop,
                System.Threading.CancellationToken.None,
                System.Threading.Tasks.TaskCreationOptions.LongRunning,
                System.Threading.Tasks.TaskScheduler.Default
            )
            |> attachStart

    member _.Shutdown() =
        let task = claimShutdown ()
        cancel.Cancel()
        signal.Release() |> ignore

        task
        |> Option.iter (fun t ->
            try
                t.Wait()
            with _ ->
                ())

        sweep ()

    member internal _.IsIdle =
        match workerCell.[workerKey] with
        | WorkerState.Running(_, activity) -> activity = Idle
        | WorkerState.NotStarted
        | WorkerState.Starting _
        | WorkerState.Stopped -> true

    /// テスト用: 「signal を消費した後に Pending だけが残った」状態を作る。
    /// `Shutdown` が Pending を見て起床を省略すると worker join が永久に待つので、
    /// その回帰を再現するために使う（セマフォには追加しない）。
    member internal _.MarkPendingWithoutSignal() = markPending () |> ignore
