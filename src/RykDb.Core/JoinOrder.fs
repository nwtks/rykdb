// ===========================================================================
// JoinOrder — INNER 結合区間の結合順を決める（貪欲法と Selinger DP）。
// ===========================================================================

namespace RykDb.Core

/// 結合順の候補。
/// `Rows` は走査の推定行数、`Position` は FROM 内の位置。
type internal JoinOrderItem =
    {
        Name: string
        Rows: float
        Position: int
    }

module internal JoinOrder =
    /// DP の状態。
    /// `Order` は `items` の添字を追加順に並べたもの。
    type private PlanState =
        {
            Cost: float
            Rows: float
            Order: int list
        }

    /// まだ結合していないテーブルのうち、ON の参照先が揃っているか。
    let private eligible (dependencies: Map<string, Set<string>>) (joined: Set<string>) (name: string) =
        match dependencies |> Map.tryFind name with
        | None -> true
        | Some refs -> Set.isSubset refs (Set.add name joined)

    /// 追加するテーブルに接する結合述語の本数（既に結合済みの側だけ数える）。
    let private linkCount (edges: Set<string> list) (joined: Set<string>) (name: string) =
        edges
        |> List.filter (fun e -> Set.contains name e && not (Set.isEmpty (Set.intersect e joined)))
        |> List.length

    /// テーブルを 1つ追加した後の中間行数（統計なしの選択性 0.1）。
    let private nextRows (edges: Set<string> list) (joined: Set<string>) (joinedRows: float) (item: JoinOrderItem) =
        let links = linkCount edges joined item.Name

        if links = 0 then
            joinedRows * item.Rows
        else
            max 1.0 (joinedRows * item.Rows * 0.1 ** float links)

    /// 現行の貪欲法。先頭テーブルと LEFT の境界は呼び出し側が固定する。
    let greedy
        (initialRows: float)
        (initialJoined: Set<string>)
        (edges: Set<string> list)
        (items: JoinOrderItem list)
        (dependencies: Map<string, Set<string>>)
        =
        let rec go joinedRows joined remaining acc =
            match remaining with
            | [] -> List.rev acc
            | _ ->
                let ready =
                    remaining |> List.filter (fun item -> eligible dependencies joined item.Name)

                let pool = if List.isEmpty ready then remaining else ready

                let item =
                    pool
                    |> List.minBy (fun item -> nextRows edges joined joinedRows item, item.Position)

                go
                    (nextRows edges joined joinedRows item)
                    (Set.add item.Name joined)
                    (remaining |> List.filter (fun x -> x.Name <> item.Name))
                    (item.Name :: acc)

        go initialRows initialJoined items []

    /// 同じ `mask` の状態として候補を採用するか。
    /// コストが同じなら FROM の位置順（辞書式）で決める。
    let private isBetter (items: JoinOrderItem array) (candidate: PlanState) (current: PlanState option) =
        let positions state =
            state.Order |> List.map (fun i -> items.[i].Position)

        match current with
        | None -> true
        | Some old -> (candidate.Cost, positions candidate) < (old.Cost, positions old)

    /// Selinger DP（左深・先頭固定）。
    /// 適用できないときは None（呼び出し側が貪欲へフォールバックする）。
    let selinger
        (initialRows: float)
        (initialJoined: Set<string>)
        (edges: Set<string> list)
        (items: JoinOrderItem list)
        (dependencies: Map<string, Set<string>>)
        =
        let joinedOf itemArray mask =
            let names =
                itemArray
                |> Array.indexed
                |> Array.choose (fun (i, item) -> if mask &&& (1 <<< i) <> 0 then Some item.Name else None)
                |> Array.toList

            Set.union initialJoined (Set.ofList names)

        let maxItems = 12
        let itemArray = Array.ofList items
        let count = itemArray.Length
        let itemNames = itemArray |> Array.map _.Name |> Set.ofArray

        let unknownDependency =
            dependencies
            |> Map.exists (fun _ refs ->
                refs
                |> Set.exists (fun r -> not (Set.contains r initialJoined) && not (Set.contains r itemNames)))

        if count > maxItems then
            None
        elif not (System.Double.IsFinite initialRows) then
            None
        elif itemArray |> Array.exists (fun item -> not (System.Double.IsFinite item.Rows)) then
            None
        elif unknownDependency then
            None
        else
            let fullMask = (1 <<< count) - 1

            let initial =
                {
                    Cost = 0.0
                    Rows = initialRows
                    Order = []
                }

            seq { 0..fullMask }
            |> Seq.fold
                (fun states mask ->
                    match states |> Map.tryFind mask with
                    | None -> states
                    | Some state ->
                        let joined = joinedOf itemArray mask

                        itemArray
                        |> Array.indexed
                        |> Array.fold
                            (fun states (i, item) ->
                                let bit = 1 <<< i

                                if mask &&& bit <> 0 || not (eligible dependencies joined item.Name) then
                                    states
                                else
                                    let newMask = mask ||| bit
                                    let links = linkCount edges joined item.Name
                                    let newRows = nextRows edges joined state.Rows item

                                    let delta =
                                        if links = 0 then
                                            state.Rows * item.Rows + newRows
                                        else
                                            state.Rows + item.Rows + newRows

                                    let candidate =
                                        {
                                            Cost = state.Cost + delta
                                            Rows = newRows
                                            Order = state.Order @ [ i ]
                                        }

                                    if isBetter itemArray candidate (states |> Map.tryFind newMask) then
                                        states |> Map.add newMask candidate
                                    else
                                        states)
                            states)
                (Map.ofList [ (0, initial) ])
            |> Map.tryFind fullMask
            |> Option.map (fun state -> state.Order |> List.map (fun i -> itemArray.[i].Name))

    /// DP が使えれば DP、使えなければ貪欲法。
    let choose initialRows initialJoined edges items dependencies =
        selinger initialRows initialJoined edges items dependencies
        |> Option.defaultWith (fun () -> greedy initialRows initialJoined edges items dependencies)
