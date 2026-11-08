// ===========================================================================
// EngineHost — background compaction の共有状態。
//
// 不変の StorageEngine / Version の外側に、MANIFEST に反映済みの現在状態だけを
// 持つ。flush と compaction worker はこの Host を通して Version を公開する。
//
// * 状態は ConcurrentDictionary の単一セル + TryUpdate の末尾再帰 CAS で更新する。
// * MANIFEST 追記・ファイル番号採番・install は installGate で直列化する。
// * 読み取りは Session 呼び出し単位で Version 全ファイルを pin する。
//   install は入力を retired にし、refcount が 0 になってから実ファイルを消す。
// ===========================================================================

namespace RykDb.Core

/// 1ファイルの pin / retired 状態。
type private FileRefState = { Pins: int; Retired: bool }

/// background compaction の共有状態。
/// `initial` は open 直後の MANIFEST 反映済み状態。
type internal EngineHost
    (
        dir: string,
        initial: HostState,
        options: CompactionOptions,
        oldestSnapshot: unit -> uint64 option,
        scanOf: FileMetaData -> Result<SstScan, DbError>,
        scanHook: (FileMetaData -> unit) option
    ) =
    let stateKey = "current"
    let numberKey = "n"
    let activityKey = "activity"

    [<Literal>]
    let Idle = 0

    [<Literal>]
    let Busy = 1

    [<Literal>]
    let Pending = 2

    let stateCell =
        System.Collections.Concurrent.ConcurrentDictionary<string, HostState>()

    do stateCell.[stateKey] <- initial

    let fileNumbers =
        System.Collections.Concurrent.ConcurrentDictionary<string, int64>()

    do fileNumbers.[numberKey] <- initial.Version.NextFileNumber

    let fileRefs =
        System.Collections.Concurrent.ConcurrentDictionary<int64, FileRefState>()

    let activity = System.Collections.Concurrent.ConcurrentDictionary<string, int>()
    do activity.[activityKey] <- Idle

    let worker =
        System.Collections.Concurrent.ConcurrentDictionary<string, System.Threading.Tasks.Task>()

    let stopped = System.Collections.Concurrent.ConcurrentDictionary<string, unit>()

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

    let normalize (state: HostState) =
        { state with
            Version =
                { state.Version with
                    NextFileNumber = max state.Version.NextFileNumber fileNumbers.[numberKey]
                }
        }

    /// 状態を CAS で更新する。
    let publish (update: HostState -> HostState) =
        let rec loop () =
            let current = stateCell.[stateKey]

            let next =
                update
                    { current with
                        Generation = current.Generation + 1L
                    }
                |> normalize

            if stateCell.TryUpdate(stateKey, next, current) then
                next
            else
                loop ()

        loop ()

    let allocateFileNumber () =
        let rec loop () =
            let current = fileNumbers.[numberKey]
            let next = current + 1L

            if fileNumbers.TryUpdate(numberKey, next, current) then
                current
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

    let scan (f: FileMetaData) =
        Option.iter (fun hook -> hook f) scanHook
        scanOf f

    let install (baseState: HostState, result: CompactionResult) =
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
                match
                    Manifest.logAndApply
                        dir
                        current.ManifestNumber
                        current.Version
                        {
                            VersionEdit.AddedFiles = result.Added
                            DeletedFiles = result.Removed
                        }
                with
                | Error e ->
                    FileMetaData.deleteObsolete dir result.Added |> ignore

                    publish (fun state ->
                        { state with
                            LastCompactionError = Some e
                        })
                    |> ignore
                | Ok(version, manifestNumber) ->
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

    let updateActivity (update: int -> int) =
        let rec loop () =
            let current = activity.[activityKey]
            let next = update current

            if next = current then
                current
            elif activity.TryUpdate(activityKey, next, current) then
                next
            else
                loop ()

        loop ()

    let markPending () =
        let rec loop () =
            let current = activity.[activityKey]

            if (current &&& Pending) <> 0 then
                true
            elif activity.TryUpdate(activityKey, current ||| Pending, current) then
                false
            else
                loop ()

        loop ()

    let signalWork () =
        if not (markPending ()) && signal.CurrentCount = 0 then
            signal.Release() |> ignore

    let finishWork () =
        updateActivity (fun current -> if (current &&& Pending) <> 0 then Pending else Idle)
        |> ignore

    let loop () =
        let rec step () =
            signal.Wait()

            if cancel.IsCancellationRequested then
                ()
            else
                updateActivity (fun current -> (current ||| Busy) &&& ~~~Pending) |> ignore

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

    /// 現在の状態。
    member _.Current() = stateCell.[stateKey]

    /// install / MANIFEST 追記を直列化する。
    member _.WithInstallGate(f: unit -> 'T) = withGate f

    member _.Publish(update: HostState -> HostState) = publish update

    member _.AllocateFileNumber() = allocateFileNumber ()

    /// 現在 Version の全ファイルを pin したスナップショットを取る。
    /// 返した IDisposable を dispose するまで、その Version のファイルは削除されない。
    member _.AcquireCall() =
        withGate (fun () ->
            let state = stateCell.[stateKey]
            let files = Version.allFiles state.Version |> List.map _.FileNumber
            files |> List.iter pinFile

            state,
            { new System.IDisposable with
                member _.Dispose() =
                    files |> List.iter (fun n -> releaseFile n |> ignore)
                    sweep ()
            })

    member _.IsIdle = activity.[activityKey] = Idle

    member _.Signal() = signalWork ()

    member _.Start() =
        if not (worker.ContainsKey "worker") then
            worker.TryAdd("worker", System.Threading.Tasks.Task.Run loop) |> ignore

    member _.Shutdown() =
        if stopped.TryAdd("stopped", ()) then
            cancel.Cancel()
            signalWork ()

            match worker.TryGetValue "worker" with
            | true, task ->
                try
                    task.Wait()
                with _ ->
                    ()
            | false, _ -> ()

            sweep ()
