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

    /// 入力を整列して返す。
    /// `budget.Sort` が負ならスピルせずメモリだけで整列する。
    let sortRows
        (budget: MemoryBudget)
        (init: 'S)
        (check: 'S -> SqlValue array -> DbResult<'S>)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (src: Op)
        =
        let inMemory = List.toArray >> Array.sortWith compareRows >> Op.ofArray
        let openRun = Spill.openTracked budget.Scope
        let closeRun = Spill.closeTracked budget.Scope true
        let closeAll = Spill.closeAllTracked budget.Scope

        let rec openAll acc rest =
            match rest with
            | [] -> ret (List.rev acc)
            | p :: tail ->
                match openRun p with
                | Error e ->
                    closeAll acc
                    Spill.forget rest
                    err e
                | Ok state -> openAll (state :: acc) tail

        let rec scan i bestIdx best =
            function
            | [] -> bestIdx
            | s :: tail ->
                match s.Head with
                | None -> scan (i + 1) bestIdx best tail
                | Some h ->
                    match best with
                    | Some b when compareRows h b >= 0 -> scan (i + 1) bestIdx best tail
                    | _ -> scan (i + 1) i (Some h) tail

        let rec splitAt n acc states =
            if n <= 0 then
                List.rev acc, states
            else
                match states with
                | [] -> List.rev acc, []
                | h :: tail -> splitAt (n - 1) (h :: acc) tail

        let rec writeMerged sink states =
            let idx = scan 0 -1 None states

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
                        writeMerged sink (before @ after)
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
                                writeMerged sink (before @ after)
                            | Ok(Some nxt) -> writeMerged sink (before @ { chosen with Head = Some nxt } :: after)

        let mergeToRun stamp serial paths =
            match openAll [] paths with
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
                    match writeMerged sink states with
                    | Error e ->
                        Spill.closeSink sink
                        Spill.forget [ path ]
                        err e
                    | Ok() ->
                        Spill.closeSink sink
                        ret path

        let rec reduceRuns stamp nextSerial known paths =
            if List.length paths <= MaxFanIn then
                ret paths
            else
                paths |> List.chunkBySize MaxFanIn |> combine stamp nextSerial known 0 []

        and combine stamp nextSerial known count outputs =
            function
            | [] ->
                let outputs = List.rev outputs
                reduceRuns stamp (nextSerial + count) (known @ outputs) outputs
            | group :: rest ->
                match mergeToRun stamp (nextSerial + count) group with
                | Error e ->
                    Spill.forget (List.concat rest)
                    Spill.forget known
                    Spill.forget outputs
                    err e
                | Ok path -> combine stamp nextSerial known (count + 1) (path :: outputs) rest

        let pull states =
            let idx = scan 0 -1 None states

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

        let rec mergeRuns states =
            Op.compute (fun () ->
                match pull states with
                | Error e -> OpStep.OpError e
                | Ok None -> OpStep.OpDone
                | Ok(Some(row, rest)) -> OpStep.OpRow(row, mergeRuns rest))

        let finish stamp buf written paths =
            if List.isEmpty paths then
                ret (inMemory buf)
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
                    match reduceRuns stamp (written + 1) runPaths runPaths with
                    | Error e ->
                        Spill.forget runPaths
                        err e
                    | Ok reduced ->
                        match openAll [] reduced with
                        | Error e ->
                            Spill.forget reduced
                            err e
                        | Ok states -> ret (mergeRuns states)

        let rec ingest stamp buf bufBytes written paths state next =
            match Op.next next with
            | OpStep.OpError e ->
                Spill.forget paths
                err e
            | OpStep.OpDone -> finish stamp buf written paths
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
                        | Ok p -> ingest stamp [] 0L (written + 1) (p :: paths) state' (Op.emit row tail)
                    else
                        ingest stamp (row :: buf) (bufBytes + size) written paths state' tail

        Spill.sweepStale budget.Directory
        let stamp = System.Guid.NewGuid().ToString "N"
        ingest stamp [] 0L 0 [] init src
