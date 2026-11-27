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
// ===========================================================================

namespace RykDb.Core

/// 外部マージソート。
/// <remarks>
/// 入力を 1行ずつ受け、バジェットを超えると run として溢らし、最後に k-way merge する。
/// 比較関数は呼び出し側（`OpsSort.sortBy`）が渡す。
/// `check` は「これまでの代表値 → 行 → 次の代表値」を返す不変の fold で、
/// 比較不能な型などを早い段階で Error にする（状態は値として持ち回る）。
/// </remarks>
module internal ExtSort =
    /// 同時に開く run 数の上限。
    /// run が多い場合は最大 fan-in 本ずつ中間 run にマージし、
    /// 最後に最大 fan-in 本を k-way merge する（file descriptor の枯渇防止）。
    [<Literal>]
    let private MaxFanIn = 32

    let private openRun (budget: MemoryBudget) (path: string) = Spill.openTracked budget.Scope path

    let private closeRun (budget: MemoryBudget) (st: RunState) = Spill.closeTracked budget.Scope true st

    /// 開いている run をすべて閉じる。
    let private closeAll (budget: MemoryBudget) = Array.iter (closeRun budget)

    /// run ファイルをすべて開く（失敗時は開いた分を閉じて忘れる）。
    [<TailCall>]
    let rec private openAll (budget: MemoryBudget) (acc: RunState list) =
        function
        | [] -> ret (List.rev acc |> List.toArray)
        | p :: tail as rest ->
            match openRun budget p with
            | Error e ->
                acc |> List.iter (closeRun budget)
                Spill.forget rest
                err e
            | Ok state -> openAll budget (state :: acc) tail

    /// 先頭行が最小の run の添字を選ぶ。
    /// run の状態は配列で持ち、選んだ添字の先頭行だけを差し替える
    /// （リストの分割と連結を行ごとに作り直さない）。
    [<TailCall>]
    let rec private scan
        (compareRows: SqlValue array -> SqlValue array -> int)
        (states: RunState array)
        (i: int)
        (bestIdx: int)
        (best: SqlValue array option)
        =
        if i >= states.Length then
            bestIdx
        else
            match states.[i].Head with
            | None -> scan compareRows states (i + 1) bestIdx best
            | Some h ->
                match best with
                | Some b when compareRows h b >= 0 -> scan compareRows states (i + 1) bestIdx best
                | _ -> scan compareRows states (i + 1) i (Some h)

    /// 最小の run から sink へ行を書き出し、全 run が空になるまで続ける。
    [<TailCall>]
    let rec private writeMerged
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (sink: Sink)
        (states: RunState array)
        =
        let idx = scan compareRows states 0 -1 None

        if idx < 0 then
            closeAll budget states
            ret ()
        else
            let chosen = states.[idx]

            match chosen.Head with
            | None ->
                closeAll budget states
                ret ()
            | Some row ->
                match Spill.writeRow row sink with
                | Error e ->
                    closeAll budget states
                    err e
                | Ok() ->
                    match Spill.readRow chosen.Src with
                    | Error e ->
                        closeAll budget states
                        err e
                    | Ok None ->
                        closeRun budget chosen
                        states.[idx] <- { chosen with Head = None }
                        writeMerged budget compareRows sink states
                    | Ok(Some nxt) ->
                        states.[idx] <- { chosen with Head = Some nxt }
                        writeMerged budget compareRows sink states

    /// 複数の run を 1本にマージして新しい run を作る。
    let private mergeToRun
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (stamp: string)
        (serial: int)
        (paths: string list)
        =
        match openAll budget [] paths with
        | Error e ->
            Spill.forget paths
            err e
        | Ok states ->
            let path = Spill.runPath budget.Directory stamp serial

            match Spill.openSink path with
            | Error e ->
                closeAll budget states
                err e
            | Ok sink ->
                match writeMerged budget compareRows sink states with
                | Error e ->
                    Spill.closeSink sink
                    Spill.forget [ path ]
                    err e
                | Ok() ->
                    match Spill.closeSinkResult sink with
                    | Ok() -> ret path
                    | Error e ->
                        Spill.forget [ path ]
                        err e

    /// 開いた run から 1行取り出し、残りの状態を返す。
    let private pull
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (states: RunState array)
        =
        let idx = scan compareRows states 0 -1 None

        if idx < 0 then
            closeAll budget states
            ret None
        else
            let chosen = states.[idx]

            match chosen.Head with
            | None -> ret None
            | Some row ->
                match Spill.readRow chosen.Src with
                | Error e ->
                    closeAll budget states
                    err e
                | Ok None ->
                    closeRun budget chosen
                    states.[idx] <- { chosen with Head = None }
                    ret (Some(row, states))
                | Ok(Some nxt) ->
                    states.[idx] <- { chosen with Head = Some nxt }
                    ret (Some(row, states))

    /// マージ済みの run から行を順に返す Op を組み立てる。
    let rec private mergeRuns
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (states: RunState array)
        =
        Op.compute (fun () ->
            match pull budget compareRows states with
            | Error e -> OpStep.OpError e
            | Ok None -> OpStep.OpDone
            | Ok(Some(row, rest)) -> OpStep.OpRow(row, mergeRuns budget compareRows rest))

    /// 受け皿をメモリ上で整列する。
    let private inMemory (compareRows: SqlValue array -> SqlValue array -> int) (buf: SqlValue array list) =
        buf |> List.toArray |> Array.sortWith compareRows |> Op.ofArray

    /// fan-in を超えた run を段階的にマージする。
    [<TailCall>]
    let rec private reduceRuns
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
            combine budget compareRows stamp nextSerial known 0 [] (List.chunkBySize MaxFanIn paths)

    /// `reduceRuns` の 1段分（相互再帰）。
    and [<TailCall>] private combine
        (budget: MemoryBudget)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (stamp: string)
        (nextSerial: int)
        (known: string list)
        (count: int)
        (outputs: string list)
        =
        function
        | [] ->
            let outputs = List.rev outputs
            reduceRuns budget compareRows stamp (nextSerial + count) (known @ outputs) outputs
        | group :: rest ->
            match mergeToRun budget compareRows stamp (nextSerial + count) group with
            | Error e ->
                Spill.forget (List.concat rest)
                Spill.forget known
                Spill.forget outputs
                err e
            | Ok path -> combine budget compareRows stamp nextSerial known (count + 1) (path :: outputs) rest

    /// 入力の読み終わりに run を確定してマージする。
    let private finish
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
                match reduceRuns budget compareRows stamp (written + 1) runPaths runPaths with
                | Error e ->
                    Spill.forget runPaths
                    err e
                | Ok reduced ->
                    match openAll budget [] reduced with
                    | Error e ->
                        Spill.forget reduced
                        err e
                    | Ok states -> ret (mergeRuns budget compareRows states)

    /// 入力を 1行ずつ受け、バジェット超過時に run を書き出す（末尾再帰）。
    [<TailCall>]
    let rec private ingest
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
        | OpStep.OpDone -> finish budget compareRows stamp buf written paths
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
                        ingest budget check compareRows stamp [] 0L (written + 1) (p :: paths) state' (Op.emit row tail)
                else
                    ingest budget check compareRows stamp (row :: buf) (bufBytes + size) written paths state' tail

    /// 入力を整列して返す。
    /// `budget.Sort` が負ならスピルせずメモリだけで整列する。
    let sortRows
        (budget: MemoryBudget)
        (init: 'S)
        (check: 'S -> SqlValue array -> DbResult<'S>)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (src: Op)
        =
        let stamp = System.Guid.NewGuid().ToString "N"
        ingest budget check compareRows stamp [] 0L 0 [] init src
