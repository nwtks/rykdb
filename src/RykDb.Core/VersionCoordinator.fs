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

/// install に必要な共有状態。
type private InstallContext =
    {
        Dir: string
        InstallGate: System.Threading.SemaphoreSlim
        StateCell: System.Collections.Concurrent.ConcurrentDictionary<string, PublishedState>
        StateKey: string
        OldestSnapshot: unit -> uint64 option
        ManifestApply: (ManifestNumber -> Version -> VersionEdit -> ManifestApplyResult) option
        FileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>
        /// compaction の入力をすぐ削除せず、起動時の検証後に回すか
        DeferDeletes: bool
    }

/// compaction 1サイクルに必要な共有状態。
type private CycleContext =
    {
        Dir: string
        Options: CompactionOptions
        StateCell: System.Collections.Concurrent.ConcurrentDictionary<string, PublishedState>
        StateKey: string
        ScanOf: FileMetaData -> Result<SstScan, DbError>
        ScanHook: (FileMetaData -> unit) option
        Cancel: System.Threading.CancellationTokenSource
        FileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>
    }

/// VersionCoordinator の共通処理。
module private VersionCoordinatorHelpers =
    /// worker の activity ビット。
    [<Literal>]
    let Idle = 0

    /// worker が処理中の activity ビット。
    [<Literal>]
    let Busy = 1

    /// 処理待ちがある activity ビット。
    [<Literal>]
    let Pending = 2

    /// MANIFEST 反映の既定実装。
    let defaultApplyManifest (dir: string) (manifestNumber: ManifestNumber) (version: Version) (edit: VersionEdit) =
        Manifest.logAndApplyWithOutcome dir manifestNumber version edit

    /// MANIFEST 反映関数を解決する。
    let applyManifestOf
        (dir: string)
        (manifestApply: (ManifestNumber -> Version -> VersionEdit -> ManifestApplyResult) option)
        =
        defaultArg manifestApply (defaultApplyManifest dir)

    /// ファイルを削除する（失敗したら false）。
    /// [deferDeletes] が true のときは削除せず、起動時の GC に回す。
    let tryDeleteFile (deferDeletes: bool) (dir: string) (fileNumber: FileNumber) =
        if deferDeletes then
            true
        else
            match
                protect (fun () ->
                    let path = FileMetaData.path dir fileNumber

                    if System.IO.File.Exists path then
                        System.IO.File.Delete path)
            with
            | Ok() -> true
            | Error _ -> false

    /// 二重 Dispose で参照数を 2回減らさないための 1回限りの解放。
    let disposableOnce (release: unit -> unit) =
        let guard = System.Collections.Concurrent.ConcurrentDictionary<string, bool>()

        { new System.IDisposable with
            member _.Dispose() =
                if guard.TryAdd("done", true) then
                    release ()
        }

    /// installGate を取って f を実行する。
    let withGate (installGate: System.Threading.SemaphoreSlim) (f: unit -> 'T) =
        installGate.Wait()

        try
            f ()
        finally
            installGate.Release() |> ignore

    /// 状態を CAS で更新するループ本体。
    [<TailCall>]
    let rec publish
        (stateCell: System.Collections.Concurrent.ConcurrentDictionary<string, PublishedState>)
        (stateKey: string)
        (update: PublishedState -> PublishedState)
        =
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
            publish stateCell stateKey update

    /// ファイル番号を 1つ予約するループ本体。
    [<TailCall>]
    let rec allocateFileNumber
        (stateCell: System.Collections.Concurrent.ConcurrentDictionary<string, PublishedState>)
        (stateKey: string)
        =
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
            allocateFileNumber stateCell stateKey

    /// ファイルの pin 数を増やすループ本体。
    [<TailCall>]
    let rec pinFile
        (fileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>)
        (fileNumber: FileNumber)
        =
        match fileRefs.TryGetValue fileNumber with
        | true, state ->
            if not (fileRefs.TryUpdate(fileNumber, { state with Pins = state.Pins + 1 }, state)) then
                pinFile fileRefs fileNumber
        | false, _ ->
            if not (fileRefs.TryAdd(fileNumber, { Pins = 1; Retired = false })) then
                pinFile fileRefs fileNumber

    /// Version の参照を増やすループ本体。
    [<TailCall>]
    let rec pinVersion
        (versionPins: System.Collections.Concurrent.ConcurrentDictionary<Version, int>)
        (fileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>)
        (version: Version)
        =
        match versionPins.TryGetValue version with
        | true, count ->
            if not (versionPins.TryUpdate(version, count + 1, count)) then
                pinVersion versionPins fileRefs version
        | false, _ ->
            if versionPins.TryAdd(version, 1) then
                Version.allFiles version |> List.iter (fun f -> pinFile fileRefs f.FileNumber)
            else
                pinVersion versionPins fileRefs version

    /// ファイルの参照を減らすループ本体。
    [<TailCall>]
    let rec releaseFile
        (deferDeletes: bool)
        (fileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>)
        (dir: string)
        (fileNumber: FileNumber)
        =
        match fileRefs.TryGetValue fileNumber with
        | true, state ->
            if state.Pins <= 0 then
                false
            elif not (fileRefs.TryUpdate(fileNumber, { state with Pins = state.Pins - 1 }, state)) then
                releaseFile deferDeletes fileRefs dir fileNumber
            elif state.Pins > 1 then
                false
            elif state.Retired then
                if fileRefs.TryRemove(System.Collections.Generic.KeyValuePair(fileNumber, { state with Pins = 0 })) then
                    if not (tryDeleteFile deferDeletes dir fileNumber) then
                        fileRefs.TryAdd(fileNumber, { Pins = 0; Retired = true }) |> ignore

                    true
                else
                    false
            else
                fileRefs.TryRemove(System.Collections.Generic.KeyValuePair(fileNumber, { state with Pins = 0 }))
                |> ignore

                false
        | false, _ -> false

    /// ファイルを retired にするループ本体。
    [<TailCall>]
    let rec retireFile
        (fileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>)
        (fileNumber: FileNumber)
        =
        match fileRefs.TryGetValue fileNumber with
        | true, state ->
            if state.Retired then
                ()
            elif not (fileRefs.TryUpdate(fileNumber, { state with Retired = true }, state)) then
                retireFile fileRefs fileNumber
        | false, _ ->
            if not (fileRefs.TryAdd(fileNumber, { Pins = 0; Retired = true })) then
                retireFile fileRefs fileNumber

    /// retired かつ参照 0 のファイルを削除する（失敗したら次回に再試行）。
    /// [deferDeletes] が true のときは追跡だけ外し、ファイルは起動時の GC に残す。
    let sweepFiles
        (deferDeletes: bool)
        (fileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>)
        (dir: string)
        =
        fileRefs.ToArray()
        |> Array.iter (fun pair ->
            if pair.Value.Retired && pair.Value.Pins = 0 then
                if fileRefs.TryRemove(System.Collections.Generic.KeyValuePair(pair.Key, pair.Value)) then
                    if not (tryDeleteFile deferDeletes dir pair.Key) then
                        fileRefs.TryAdd(pair.Key, { Pins = 0; Retired = true }) |> ignore)

    /// Version の参照を減らすループ本体。
    [<TailCall>]
    let rec releaseVersion
        (deferDeletes: bool)
        (versionPins: System.Collections.Concurrent.ConcurrentDictionary<Version, int>)
        (fileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>)
        (dir: string)
        (version: Version)
        =
        match versionPins.TryGetValue version with
        | true, count when count > 1 ->
            if not (versionPins.TryUpdate(version, count - 1, count)) then
                releaseVersion deferDeletes versionPins fileRefs dir version
        | true, 1 ->
            if versionPins.TryRemove(System.Collections.Generic.KeyValuePair(version, 1)) then
                Version.allFiles version
                |> List.iter (fun f -> releaseFile deferDeletes fileRefs dir f.FileNumber |> ignore)

                sweepFiles deferDeletes fileRefs dir
            else
                releaseVersion deferDeletes versionPins fileRefs dir version
        | _ -> ()

    /// ファイルを走査する（hook を呼んでから scanOf を実行）。
    let scanFile
        (scanHook: (FileMetaData -> unit) option)
        (scanOf: FileMetaData -> Result<SstScan, DbError>)
        (f: FileMetaData)
        =
        Option.iter (fun hook -> hook f) scanHook
        scanOf f

    /// Version のファイル番号集合を返す。
    let fileNumbers (v: Version) =
        Version.allFiles v |> List.map _.FileNumber |> Set.ofList

    /// 2つの Version が同じファイル集合か。
    let sameFileSet (a: Version) (b: Version) = fileNumbers a = fileNumbers b

    /// 停止要求が出ているか。
    let shouldStop (cancel: System.Threading.CancellationTokenSource) = cancel.IsCancellationRequested

    /// worker の activity を更新するループ本体。
    [<TailCall>]
    let rec updateActivity
        (workerCell: System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>)
        (workerKey: string)
        (update: int -> int)
        =
        match workerCell.[workerKey] with
        | WorkerState.Running(task, activity) ->
            let next = update activity

            if next = activity then
                activity
            elif
                workerCell.TryUpdate(workerKey, WorkerState.Running(task, next), WorkerState.Running(task, activity))
            then
                next
            else
                updateActivity workerCell workerKey update
        | WorkerState.NotStarted
        | WorkerState.Starting _
        | WorkerState.Stopped -> Idle

    /// 「追加の要求あり」を立てるループ本体。
    [<TailCall>]
    let rec markPending
        (workerCell: System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>)
        (workerKey: string)
        =
        match workerCell.[workerKey] with
        | WorkerState.NotStarted
        | WorkerState.Stopped -> true
        | WorkerState.Starting true -> true
        | WorkerState.Starting false ->
            if workerCell.TryUpdate(workerKey, WorkerState.Starting true, WorkerState.Starting false) then
                false
            else
                markPending workerCell workerKey
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
                markPending workerCell workerKey

    /// worker に起床を通知する。
    let signalWork (isPending: unit -> bool) (signal: System.Threading.SemaphoreSlim) =
        if not (isPending ()) && signal.CurrentCount = 0 then
            signal.Release() |> ignore

    /// 実行中の要求を片付ける。
    let finishWork
        (workerCell: System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>)
        (workerKey: string)
        =
        updateActivity workerCell workerKey (fun current -> if current &&& Pending <> 0 then Pending else Idle)
        |> ignore

    /// worker ループの 1ステップ。
    [<TailCall>]
    let rec coordinatorStep
        (signal: System.Threading.SemaphoreSlim)
        (cancel: System.Threading.CancellationTokenSource)
        (updateActivityF: (int -> int) -> int)
        (runCycle: int -> unit)
        (publishF: (PublishedState -> PublishedState) -> PublishedState)
        (finishWorkF: unit -> unit)
        =
        signal.Wait()

        if cancel.IsCancellationRequested then
            ()
        else
            updateActivityF (fun current -> current ||| Busy &&& ~~~Pending) |> ignore

            try
                runCycle 4
            with e ->
                publishF (fun state ->
                    { state with
                        LastCompactionError = Some(DbError.internalError e.Message)
                    })
                |> ignore

            finishWorkF ()
            coordinatorStep signal cancel updateActivityF runCycle publishF finishWorkF

    /// worker 起動を予約する。
    [<TailCall>]
    let rec claimStart
        (workerCell: System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>)
        (workerKey: string)
        =
        match workerCell.[workerKey] with
        | WorkerState.NotStarted ->
            if workerCell.TryUpdate(workerKey, WorkerState.Starting false, WorkerState.NotStarted) then
                true
            else
                claimStart workerCell workerKey
        | WorkerState.Starting _
        | WorkerState.Running _
        | WorkerState.Stopped -> false

    /// 起動した task を worker 状態へ結び付ける。
    [<TailCall>]
    let rec attachStart
        (workerCell: System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>)
        (workerKey: string)
        (task: System.Threading.Tasks.Task)
        =
        match workerCell.[workerKey] with
        | WorkerState.Starting pending ->
            let activity = if pending then Pending else Idle

            if
                not (workerCell.TryUpdate(workerKey, WorkerState.Running(task, activity), WorkerState.Starting pending))
            then
                attachStart workerCell workerKey task
        | _ -> ()

    /// 停止を予約し、実行中の task を取り出す。
    [<TailCall>]
    let rec claimShutdown
        (workerCell: System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>)
        (workerKey: string)
        =
        let current = workerCell.[workerKey]

        match current with
        | WorkerState.Stopped -> None
        | WorkerState.NotStarted
        | WorkerState.Starting _ ->
            if workerCell.TryUpdate(workerKey, WorkerState.Stopped, current) then
                None
            else
                claimShutdown workerCell workerKey
        | WorkerState.Running(task, _) ->
            if workerCell.TryUpdate(workerKey, WorkerState.Stopped, current) then
                Some task
            else
                claimShutdown workerCell workerKey

    /// 現在 Version を pin した lease を取得するループ本体。
    [<TailCall>]
    let rec acquireCallLoop
        (deferDeletes: bool)
        (stateCell: System.Collections.Concurrent.ConcurrentDictionary<string, PublishedState>)
        (stateKey: string)
        (versionPins: System.Collections.Concurrent.ConcurrentDictionary<Version, int>)
        (fileRefs: System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>)
        (dir: string)
        =
        let state = stateCell.[stateKey]
        pinVersion versionPins fileRefs state.Version

        if obj.ReferenceEquals(stateCell.[stateKey], state) then
            state, disposableOnce (fun () -> releaseVersion deferDeletes versionPins fileRefs dir state.Version)
        else
            releaseVersion deferDeletes versionPins fileRefs dir state.Version
            acquireCallLoop deferDeletes stateCell stateKey versionPins fileRefs dir

    /// 現在の最古 snapshot（登録が無ければ ActiveSnapshot）。
    let private oldestSnapshotAt (ctx: InstallContext) (current: PublishedState) =
        match ctx.OldestSnapshot() with
        | Some snapshot -> min current.ActiveSnapshot snapshot
        | None -> current.ActiveSnapshot

    /// 入力が現存し、最古 snapshot が開始時より古くならないか。
    let private canInstall
        (currentNumbers: Set<FileNumber>)
        (baseState: PublishedState)
        (result: CompactionResult)
        (oldestNow: uint64)
        =
        result.Removed
        |> List.forall (fun f -> Set.contains f.FileNumber currentNumbers)
        && oldestNow >= baseState.ActiveSnapshot

    /// GC 境界を下げて公開する（入力の消失・snapshot の前進でインストールできないとき）。
    /// スキップ理由を消すと診断も再試行要求も失われるので、競合エラーとして残す。
    let private publishSkippedInstall
        (ctx: InstallContext)
        (baseState: PublishedState)
        (result: CompactionResult)
        (oldestNow: uint64)
        =
        FileMetaData.deleteObsolete ctx.Dir result.Added |> ignore

        let reason =
            sprintf
                "compaction の install をスキップしました（最古 snapshot=%d, 開始時=%d, 入力=%d）"
                oldestNow
                baseState.ActiveSnapshot
                (List.length result.Removed)
            |> DbError.conflict

        publish ctx.StateCell ctx.StateKey (fun state ->
            { state with
                ActiveSnapshot = oldestNow
                LastCompactionError = Some reason
            })
        |> ignore

    /// インストール失敗を記録する（出力は残す。MANIFEST を読めないときは孤児として扱う）。
    let private publishInstallFailure (ctx: InstallContext) (error: DbError) =
        publish ctx.StateCell ctx.StateKey (fun state ->
            { state with
                LastCompactionError = Some error
            })
        |> ignore

    /// 出力を削除し、インストール失敗を記録する。
    let private publishInstallError (ctx: InstallContext) (result: CompactionResult) (error: DbError) =
        FileMetaData.deleteObsolete ctx.Dir result.Added |> ignore
        publishInstallFailure ctx error

    /// MANIFEST に反映済みの Version を公開し、削除済みファイルを retire する。
    let private publishInstalled
        (ctx: InstallContext)
        (result: CompactionResult)
        (version: Version)
        (manifestNumber: ManifestNumber)
        =
        publish ctx.StateCell ctx.StateKey (fun state ->
            { state with
                Version = version
                ManifestNumber = manifestNumber
                LastCompactionError = None
            })
        |> ignore

        result.Removed |> List.iter (fun f -> retireFile ctx.FileRefs f.FileNumber)

    /// MANIFEST 追記の結果が Unknown のとき、読み直して適用済みかを確かめる。
    /// <remarks>
    /// 読み直しは CURRENT を別の MANIFEST へ切り替えることがある
    /// （追記の巻き戻しで新しい世代を書いた場合）。
    /// その番号を捨てると、以後の flush / compaction が CURRENT の指さない MANIFEST へ追記し、
    /// 再オープン時にその出力が Version から消える。
    /// </remarks>
    let private resolveUnknownInstall
        (ctx: InstallContext)
        (current: PublishedState)
        (edit: VersionEdit)
        (result: CompactionResult)
        (error: DbError)
        =
        let expected = VersionEdit.apply current.Version edit

        match Manifest.openOrCreate ctx.Dir with
        | Ok(version, manifestNumber) when sameFileSet version expected ->
            publishInstalled ctx result expected manifestNumber
        | Ok(version, manifestNumber) ->
            let referenced = Version.allFiles version |> List.map _.FileNumber |> Set.ofList

            if
                result.Added
                |> List.exists (fun f -> Set.contains f.FileNumber referenced)
                |> not
            then
                FileMetaData.deleteObsolete ctx.Dir result.Added |> ignore

            publish ctx.StateCell ctx.StateKey (fun state ->
                { state with
                    ManifestNumber = manifestNumber
                    LastCompactionError = Some error
                })
            |> ignore
        | Error _ -> publishInstallFailure ctx error

    /// compaction 結果を MANIFEST へ反映する。
    let install (ctx: InstallContext) (baseState: PublishedState, result: CompactionResult) =
        withGate ctx.InstallGate (fun () ->
            let current = ctx.StateCell.[ctx.StateKey]

            let currentNumbers =
                Version.allFiles current.Version |> List.map _.FileNumber |> Set.ofList

            let oldestNow = oldestSnapshotAt ctx current

            if not (canInstall currentNumbers baseState result oldestNow) then
                publishSkippedInstall ctx baseState result oldestNow
            else
                let edit =
                    {
                        VersionEdit.AddedFiles = result.Added
                        DeletedFiles = result.Removed
                    }

                match applyManifestOf ctx.Dir ctx.ManifestApply current.ManifestNumber current.Version edit with
                | ManifestApplyResult.NotApplied e -> publishInstallError ctx result e
                | ManifestApplyResult.Unknown e -> resolveUnknownInstall ctx current edit result e
                | ManifestApplyResult.Applied(version, manifestNumber) ->
                    publishInstalled ctx result version manifestNumber)

        sweepFiles ctx.DeferDeletes ctx.FileRefs ctx.Dir

    /// compaction を最大 `remaining` 回まわす。
    [<TailCall>]
    let rec runCycle (ctx: CycleContext) (installCtx: InstallContext) (remaining: int) =
        if remaining <= 0 then
            ()
        else
            let baseState = ctx.StateCell.[ctx.StateKey]

            match
                Compaction.compactWith
                    ctx.Dir
                    baseState.Version
                    ctx.Options
                    baseState.ActiveSnapshot
                    (scanFile ctx.ScanHook ctx.ScanOf)
                    (fun () -> allocateFileNumber ctx.StateCell ctx.StateKey)
                    (fun () -> shouldStop ctx.Cancel)
            with
            | Error e ->
                publish ctx.StateCell ctx.StateKey (fun state ->
                    { state with
                        LastCompactionError = Some e
                    })
                |> ignore

                sweepFiles installCtx.DeferDeletes ctx.FileRefs ctx.Dir
            | Ok(result, _) ->
                if List.isEmpty result.Added && List.isEmpty result.Removed then
                    publish ctx.StateCell ctx.StateKey (fun state ->
                        { state with
                            LastCompactionError = None
                        })
                    |> ignore

                    sweepFiles installCtx.DeferDeletes ctx.FileRefs ctx.Dir
                else
                    install installCtx (baseState, result)

                    if not ctx.Cancel.IsCancellationRequested then
                        runCycle ctx installCtx (remaining - 1)

/// background compaction の共有状態。
/// `initial` は open 直後の MANIFEST 反映済み状態。
type internal VersionCoordinator
    (
        dir: string,
        initial: PublishedState,
        options: CompactionOptions,
        deferDeletes: bool,
        oldestSnapshot: unit -> uint64 option,
        scanOf: FileMetaData -> Result<SstScan, DbError>,
        scanHook: (FileMetaData -> unit) option,
        manifestApply: (ManifestNumber -> Version -> VersionEdit -> ManifestApplyResult) option
    ) =
    /// 公開状態を入れるセルのキー。
    let stateKey = "current"

    /// worker 状態を入れるセルのキー。
    let workerKey = "worker"

    /// 公開状態（1セル）。
    let stateCell =
        System.Collections.Concurrent.ConcurrentDictionary<string, PublishedState>()

    do stateCell.[stateKey] <- initial

    /// ファイル番号ごとの参照状態。
    let fileRefs =
        System.Collections.Concurrent.ConcurrentDictionary<FileNumber, FileRefState>()

    /// Version 単位の参照数。最初の lease でファイルを pin し、最後の lease で release する。
    /// 参照等価で数える（構造が同じ別の Version を取り違えない）。
    let versionPins =
        System.Collections.Concurrent.ConcurrentDictionary<Version, int> HashIdentity.Reference

    /// worker のライフサイクルと activity を 1セルで持つ（遷移は CAS 1回）。
    let workerCell =
        System.Collections.Concurrent.ConcurrentDictionary<string, WorkerState>()

    do workerCell.[workerKey] <- WorkerState.NotStarted

    /// MANIFEST 反映・採番・install を直列化する gate。
    let installGate = new System.Threading.SemaphoreSlim(1, 1)

    /// worker を停止するための token。
    let cancel = new System.Threading.CancellationTokenSource()

    /// worker を起こすシグナル。
    let signal = new System.Threading.SemaphoreSlim(0, System.Int32.MaxValue)

    /// install 経路へ渡す共通文脈。
    let installCtx: InstallContext =
        {
            Dir = dir
            InstallGate = installGate
            StateCell = stateCell
            StateKey = stateKey
            OldestSnapshot = oldestSnapshot
            ManifestApply = manifestApply
            FileRefs = fileRefs
            DeferDeletes = deferDeletes
        }

    /// compaction サイクルへ渡す共通文脈。
    let cycleCtx: CycleContext =
        {
            Dir = dir
            Options = options
            StateCell = stateCell
            StateKey = stateKey
            ScanOf = scanOf
            ScanHook = scanHook
            Cancel = cancel
            FileRefs = fileRefs
        }

    /// 現在の状態。
    member _.Current = stateCell.[stateKey]

    /// 直近の compaction の失敗（成功したら None）。
    member _.LastCompactionError = stateCell.[stateKey].LastCompactionError

    /// install / MANIFEST 追記を直列化する。
    member _.WithInstallGate(f: unit -> 'T) =
        VersionCoordinatorHelpers.withGate installGate f

    member _.Publish(update: PublishedState -> PublishedState) =
        VersionCoordinatorHelpers.publish stateCell stateKey update

    member _.AllocateFileNumber() =
        VersionCoordinatorHelpers.allocateFileNumber stateCell stateKey

    /// 現在 Version のスナップショットと、その Version を pin した lease を返す。
    /// 同じ Version への複数 lease はファイル数に比例しない（初回だけ全ファイルを pin する）。
    /// <remarks>
    /// install は「publish -> retire」の順に行うので、
    /// pin してから状態が変わっていないことを確かめれば、
    /// installGate を取らずに「ファイルが消えない Version」を掴める
    /// （SELECT が flush を待たない）。
    /// </remarks>
    member _.AcquireCall() =
        VersionCoordinatorHelpers.acquireCallLoop deferDeletes stateCell stateKey versionPins fileRefs dir

    /// 指定した Version を pin する lease を返す。
    /// 走査が呼び出し元の read lease より長生きする場合に使う。
    /// pin / release は CAS だけで完結するので installGate を取らない
    /// （point lookup ごとに呼ばれるため、MANIFEST 追記と競合させない）。
    member _.AcquireVersion(version: Version) =
        VersionCoordinatorHelpers.pinVersion versionPins fileRefs version

        VersionCoordinatorHelpers.disposableOnce (fun () ->
            VersionCoordinatorHelpers.releaseVersion deferDeletes versionPins fileRefs dir version)

    member _.Signal() =
        VersionCoordinatorHelpers.signalWork
            (fun () -> VersionCoordinatorHelpers.markPending workerCell workerKey)
            signal

    /// worker を起動する。長時間ブロックして回り続ける専用スレッドにする
    /// （スレッドプールだと混雑時に `WaitingToRun` のまま compaction が開始されないことがある）。
    /// 二重起動は無視する。
    member _.Start() =
        if VersionCoordinatorHelpers.claimStart workerCell workerKey then
            System.Threading.Tasks.Task.Factory.StartNew(
                (fun () ->
                    VersionCoordinatorHelpers.coordinatorStep
                        signal
                        cancel
                        (VersionCoordinatorHelpers.updateActivity workerCell workerKey)
                        (VersionCoordinatorHelpers.runCycle cycleCtx installCtx)
                        (VersionCoordinatorHelpers.publish stateCell stateKey)
                        (fun () -> VersionCoordinatorHelpers.finishWork workerCell workerKey)),
                System.Threading.CancellationToken.None,
                System.Threading.Tasks.TaskCreationOptions.LongRunning,
                System.Threading.Tasks.TaskScheduler.Default
            )
            |> VersionCoordinatorHelpers.attachStart workerCell workerKey

    member _.Shutdown() =
        let task = VersionCoordinatorHelpers.claimShutdown workerCell workerKey
        cancel.Cancel()
        signal.Release() |> ignore

        task
        |> Option.iter (fun t ->
            try
                t.Wait()
            with _ ->
                ())

        VersionCoordinatorHelpers.sweepFiles deferDeletes fileRefs dir

    member internal _.IsIdle =
        match workerCell.[workerKey] with
        | WorkerState.Running(_, activity) -> activity = VersionCoordinatorHelpers.Idle
        | WorkerState.NotStarted
        | WorkerState.Starting _
        | WorkerState.Stopped -> true

    /// テスト用: 「signal を消費した後に Pending だけが残った」状態を作る。
    /// `Shutdown` が Pending を見て起床を省略すると worker join が永久に待つので、
    /// その回帰を再現するために使う（セマフォには追加しない）。
    member internal _.MarkPendingWithoutSignal() =
        VersionCoordinatorHelpers.markPending workerCell workerKey |> ignore
