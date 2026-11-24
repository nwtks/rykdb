// ===========================================================================
// ExtSort — SQL レベルの行を一時ファイルへ溢らせる外部マージソート。
//
// ソートは入力全体をメモリに載せられない。
// そこで次のように処理する。
//
//   1. 受け皿（バジェット）に収まるうちはメモリに溜める
//   2. バジェットを超えたら「今の受け皿を整列して 1本の run ファイルへ吐く」
//   3. 全部読み終えたら run を k-way merge して 1本の有序列を得る
//
// メモリ使用量は「受け皿 1枚 + run ごとの先頭 1行」に比例し、
// 入力全体のサイズには比例しない。
// run の形式・一時ファイルの後始末は `Spill` が持つ。
// ===========================================================================

namespace RykDb.Core

/// 外部マージソート。
/// <remarks>
/// 入力を 1行ずつ受け、バジェットを超えると run として溢らし、最後に k-way merge する。
/// 比較関数は呼び出し側（`Ops.sortBy`）が渡す。
/// `check` は「これまでの代表値 → 行 → 次の代表値」を返す不変の fold で、
/// 比較不能な型などを早い段階で Error にする（状態は値として持ち回る）。
/// </remarks>
module internal ExtSort =
    /// 同時に開く run 数の上限。
    /// run が多い場合は最大 fan-in 本ずつ中間 run にマージし、
    /// 最後に最大 fan-in 本を k-way merge する（file descriptor の枯渇防止）。
    [<Literal>]
    let private MaxFanIn = 32

    /// run ファイルをすべて開く（失敗時は開いた分を閉じて忘れる）。
    [<TailCall>]
    let rec private openAll
        (openRun: string -> Result<'a, DbError>)
        (closeAll: 'a list -> unit)
        (acc: 'a list)
        (rest: string list)
        =
        match rest with
        | [] -> ret (List.rev acc)
        | p :: tail ->
            match openRun p with
            | Error e ->
                closeAll acc
                Spill.forget rest
                err e
            | Ok state -> openAll openRun closeAll (state :: acc) tail

    /// 先頭行が最小の run の添字を選ぶ。
    [<TailCall>]
    let rec private scan
        (compareRows: SqlValue array -> SqlValue array -> int)
        (i: int)
        (bestIdx: int)
        (best: SqlValue array option)
        =
        function
        | [] -> bestIdx
        | s :: tail ->
            match s.Head with
            | None -> scan compareRows (i + 1) bestIdx best tail
            | Some h ->
                match best with
                | Some b when compareRows h b >= 0 -> scan compareRows (i + 1) bestIdx best tail
                | _ -> scan compareRows (i + 1) i (Some h) tail

    /// 先頭 n 個を取り出して残りを返す。
    [<TailCall>]
    let rec private splitAt (n: int) (acc: 'a list) (states: 'a list) =
        if n <= 0 then
            List.rev acc, states
        else
            match states with
            | [] -> List.rev acc, []
            | h :: tail -> splitAt (n - 1) (h :: acc) tail

    /// 最小の run から sink へ行を書き出し、全 run が空になるまで続ける。
    [<TailCall>]
    let rec private writeMerged
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (sink: Sink)
        (states: RunState list)
        =
        let idx = scan compareRows 0 -1 None states

        if idx < 0 then
            closeAll states
            ret ()
        else
            let before, chosenAndAfter = splitAt idx [] states

            match chosenAndAfter with
            | [] ->
                closeAll states
                ret ()
            | chosen :: after ->
                match chosen.Head with
                | None ->
                    closeRun chosen
                    writeMerged closeAll closeRun compareRows sink (before @ after)
                | Some row ->
                    match Spill.writeRow row sink with
                    | Error e ->
                        closeAll (before @ chosenAndAfter)
                        err e
                    | Ok() ->
                        match Spill.readRow chosen.Src with
                        | Error e ->
                            closeAll (before @ chosenAndAfter)
                            err e
                        | Ok None ->
                            closeRun chosen
                            writeMerged closeAll closeRun compareRows sink (before @ after)
                        | Ok(Some nxt) ->
                            writeMerged
                                closeAll
                                closeRun
                                compareRows
                                sink
                                (before @ { chosen with Head = Some nxt } :: after)

    /// 複数の run を 1本にマージして新しい run を作る。
    let private mergeToRun
        (openRun: string -> Result<RunState, DbError>)
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (stamp: string)
        (serial: int)
        (paths: string list)
        =
        match openAll openRun closeAll [] paths with
        | Error e ->
            Spill.forget paths
            err e
        | Ok states ->
            let path = Spill.runPath budget.Directory stamp serial

            match Spill.openSink path with
            | Error e ->
                closeAll states
                err e
            | Ok sink ->
                match writeMerged closeAll closeRun compareRows sink states with
                | Error e ->
                    Spill.closeSink sink
                    Spill.forget [ path ]
                    err e
                | Ok() ->
                    Spill.closeSink sink
                    ret path

    /// 開いた run から 1行取り出し、残りの状態を返す。
    let private pull
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (states: RunState list)
        =
        let idx = scan compareRows 0 -1 None states

        if idx < 0 then
            closeAll states
            ret None
        else
            let before, chosenAndAfter = splitAt idx [] states

            match chosenAndAfter with
            | [] -> ret None
            | chosen :: after ->
                match chosen.Head with
                | None -> ret None
                | Some row ->
                    match Spill.readRow chosen.Src with
                    | Error e ->
                        closeAll (before @ chosenAndAfter)
                        err e
                    | Ok None ->
                        closeRun chosen
                        ret (Some(row, before @ after))
                    | Ok(Some nxt) ->
                        let advanced = { chosen with Head = Some nxt }
                        ret (Some(row, before @ advanced :: after))

    /// マージ済みの run から行を順に返す Op を組み立てる。
    let rec private mergeRuns
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (states: RunState list)
        =
        Op.compute (fun () ->
            match pull closeAll closeRun compareRows states with
            | Error e -> OpStep.OpError e
            | Ok None -> OpStep.OpDone
            | Ok(Some(row, rest)) -> OpStep.OpRow(row, mergeRuns closeAll closeRun compareRows rest))

    /// 受け皿をメモリ上で整列する。
    let private inMemory (compareRows: SqlValue array -> SqlValue array -> int) (buf: SqlValue array list) =
        buf |> List.toArray |> Array.sortWith compareRows |> Op.ofArray

    /// fan-in を超えた run を段階的にマージする。
    let rec private reduceRuns
        (openRun: string -> Result<RunState, DbError>)
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (stamp: string)
        (nextSerial: int)
        (known: string list)
        (paths: string list)
        =
        if List.length paths <= MaxFanIn then
            ret paths
        else
            combine
                openRun
                closeAll
                closeRun
                budget
                compareRows
                stamp
                nextSerial
                known
                0
                []
                (List.chunkBySize MaxFanIn paths)

    /// `reduceRuns` の 1段分（相互再帰）。
    and private combine
        (openRun: string -> Result<RunState, DbError>)
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (stamp: string)
        (nextSerial: int)
        (known: string list)
        (count: int)
        (outputs: string list)
        (groups: string list list)
        =
        match groups with
        | [] ->
            let outputs = List.rev outputs
            reduceRuns openRun closeAll closeRun budget compareRows stamp (nextSerial + count) (known @ outputs) outputs
        | group :: rest ->
            match mergeToRun openRun closeAll closeRun budget compareRows stamp (nextSerial + count) group with
            | Error e ->
                Spill.forget (List.concat rest)
                Spill.forget known
                Spill.forget outputs
                err e
            | Ok path ->
                combine
                    openRun
                    closeAll
                    closeRun
                    budget
                    compareRows
                    stamp
                    nextSerial
                    known
                    (count + 1)
                    (path :: outputs)
                    rest

    /// 入力の読み終わりに run を確定してマージする。
    let private finish
        (openRun: string -> Result<RunState, DbError>)
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (stamp: string)
        (buf: SqlValue array list)
        (written: int)
        (paths: string list)
        =
        if List.isEmpty paths then
            ret (inMemory compareRows buf)
        else
            let all =
                if List.isEmpty buf then
                    ret paths
                else
                    Spill.writeRun budget.Directory stamp written compareRows (buf |> List.rev |> List.toArray)
                    |> Result.map (fun p -> paths @ [ p ])

            match all with
            | Error e ->
                Spill.forget paths
                err e
            | Ok runPaths ->
                match reduceRuns openRun closeAll closeRun budget compareRows stamp (written + 1) runPaths runPaths with
                | Error e ->
                    Spill.forget runPaths
                    err e
                | Ok reduced ->
                    match openAll openRun closeAll [] reduced with
                    | Error e ->
                        Spill.forget reduced
                        err e
                    | Ok states -> ret (mergeRuns closeAll closeRun compareRows states)

    /// 入力を 1行ずつ受け、バジェット超過時に run を書き出す（末尾再帰）。
    [<TailCall>]
    let rec private ingest
        (openRun: string -> Result<RunState, DbError>)
        (closeAll: RunState list -> unit)
        (closeRun: RunState -> unit)
        (budget: MemoryBudget)
        (check: 'S -> SqlValue array -> DbResult<'S>)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (stamp: string)
        (buf: SqlValue array list)
        (bufBytes: int64)
        (written: int)
        (paths: string list)
        (state: 'S)
        (next: Op)
        =
        match Op.next next with
        | OpStep.OpError e ->
            Spill.forget paths
            err e
        | OpStep.OpDone -> finish openRun closeAll closeRun budget compareRows stamp buf written paths
        | OpStep.OpRow(row, tail) ->
            match check state row with
            | DbErr e ->
                Spill.forget paths
                err e
            | DbOk state' ->
                let size = int64 (ValueCodec.managedRowBytes row)

                if not (List.isEmpty buf) && MemoryBudget.exceeded (budget.Sort, bufBytes + size) then
                    match
                        Spill.writeRun budget.Directory stamp written compareRows (buf |> List.rev |> List.toArray)
                    with
                    | Error e ->
                        Spill.forget paths
                        err e
                    | Ok p ->
                        ingest
                            openRun
                            closeAll
                            closeRun
                            budget
                            check
                            compareRows
                            stamp
                            []
                            0L
                            (written + 1)
                            (p :: paths)
                            state'
                            (Op.emit row tail)
                else
                    ingest
                        openRun
                        closeAll
                        closeRun
                        budget
                        check
                        compareRows
                        stamp
                        (row :: buf)
                        (bufBytes + size)
                        written
                        paths
                        state'
                        tail

    /// 入力を整列して返す。
    /// `budget.Sort` が負ならスピルせずメモリだけで整列する。
    let sortRows
        (budget: MemoryBudget)
        (init: 'S)
        (check: 'S -> SqlValue array -> DbResult<'S>)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (src: Op)
        =
        let openRun = Spill.openTracked budget.Scope
        let closeRun = Spill.closeTracked budget.Scope true
        let closeAll = Spill.closeAllTracked budget.Scope
        let stamp = System.Guid.NewGuid().ToString "N"
        ingest openRun closeAll closeRun budget check compareRows stamp [] 0L 0 [] init src
