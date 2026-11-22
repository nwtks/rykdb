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
        /// このテーブルが持つ索引の列順（INL の可否判定用）。
        IndexColumns: string list list
        /// 1行あたりの管理メモリ概算（スピル判定用）。
        RowBytes: float
        /// 走査が自然に返す列順（マージ結合の可否判定用）。
        NaturalOrder: string list option
    }

/// 結合述語 1 本。`Selectivity` が None なら既定の 0.1 を使う。
type internal JoinOrderEdge =
    {
        Refs: Set<string>
        Selectivity: float option
        /// テーブル名 -> その側の結合キー列（複合キーは別の辺に分かれる）。
        KeyColumns: Map<string, string>
    }

module internal JoinOrder =
    /// DP の状態。
    /// `Order` は `items` の添字を追加順に並べたもの。
    type private PlanState =
        {
            Cost: float
            Rows: float
            RowBytes: float
            /// 左深木の出力順（先頭走査の自然順を引き継ぐ）。
            Ordered: string list option
            Order: int list
        }

    /// 右テーブルを索引で引ける結合辺か。
    /// 結合キー全体が 1 つの索引の prefix に収まる場合だけ INL にする
    /// （実行時の `leftIndicesOf` / `coversAll` と同じ条件）。
    let private canIndexJoin (item: JoinOrderItem) (applicable: JoinOrderEdge list) =
        let keyColumns =
            applicable |> List.choose (fun edge -> edge.KeyColumns |> Map.tryFind item.Name)

        if List.isEmpty keyColumns then
            false
        else
            item.IndexColumns
            |> List.exists (fun columns ->
                let prefix = List.truncate keyColumns.Length columns

                List.length prefix = keyColumns.Length
                && prefix
                   |> List.forall (fun c -> keyColumns |> List.exists (fun key -> SqlName.equal c key))
                && keyColumns
                   |> List.forall (fun key -> prefix |> List.exists (fun c -> SqlName.equal c key)))

    /// 両側の自然順が結合キーで揃っているか（マージ結合の可否）。
    /// 右の走査順の先頭から、結合辺がある限り一致していれば良い。
    let private canMergeJoin (ordered: string list option) (item: JoinOrderItem) (applicable: JoinOrderEdge list) =
        match ordered, item.NaturalOrder with
        | Some left, Some right ->
            let rec loop i =
                if i >= List.length right then
                    true
                else
                    let rightColumn = right.[i]

                    match
                        applicable
                        |> List.tryFind (fun e -> e.KeyColumns |> Map.tryFind item.Name = Some rightColumn)
                    with
                    | None -> i > 0
                    | Some edge ->
                        if
                            i < List.length left
                            && edge.KeyColumns
                               |> Map.exists (fun name column -> name <> item.Name && SqlName.equal column left.[i])
                        then
                            loop (i + 1)
                        else
                            false

            loop 0
        | _ -> false

    /// まだ結合していないテーブルのうち、ON の参照先が揃っているか。
    let private eligible (dependencies: Map<string, Set<string>>) (joined: Set<string>) (name: string) =
        match dependencies |> Map.tryFind name with
        | None -> true
        | Some refs -> Set.isSubset refs (Set.add name joined)

    /// 追加するテーブルに接する結合述語（既に結合済みの側だけ数える）。
    let private applicableEdges (edges: JoinOrderEdge list) (joined: Set<string>) (name: string) =
        edges
        |> List.filter (fun e -> Set.contains name e.Refs && not (Set.isEmpty (Set.intersect e.Refs joined)))

    /// テーブルを 1つ追加した後の中間行数。
    /// 選択性が分からない辺は 0.1 を使う。0 行の入力は 0 のまま返す。
    let internal nextRows (edges: JoinOrderEdge list) (joined: Set<string>) (joinedRows: float) (item: JoinOrderItem) =
        let applicable = applicableEdges edges joined item.Name

        if List.isEmpty applicable then
            joinedRows * item.Rows
        else
            let product =
                applicable
                |> List.fold (fun acc e -> acc * (e.Selectivity |> Option.defaultValue 0.1)) (joinedRows * item.Rows)

            if product <= 0.0 then 0.0 else max 1.0 product

    /// 現行の貪欲法。先頭テーブルと LEFT の境界は呼び出し側が固定する。
    let greedy
        (initialRows: float)
        (initialJoined: Set<string>)
        (edges: JoinOrderEdge list)
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

    /// 1テーブル追加時のコスト増分。
    /// 等値結合はハッシュ / マージ / INL の最小、直積は入れ子ループ。
    let internal transitionDelta
        (rows: float)
        (rowBytes: float)
        (ordered: string list option)
        (workMemBytes: float)
        (applicable: JoinOrderEdge list)
        (item: JoinOrderItem)
        (newRows: float)
        =
        if List.isEmpty applicable then
            rows * item.Rows + newRows
        else
            let hashDelta =
                let buildRows = min rows item.Rows
                let buildRowBytes = if rows < item.Rows then rowBytes else item.RowBytes
                let buildBytes = buildRows * buildRowBytes
                let buildPenalty = 0.1 * buildRows

                let spillPenalty =
                    if workMemBytes >= 0.0 && buildBytes > workMemBytes then
                        buildRows * 2.0 + float Spill.Partitions * 8.0
                    else
                        0.0

                rows + item.Rows + newRows + buildPenalty + spillPenalty

            let mergeDelta =
                if canMergeJoin ordered item applicable then
                    Some(rows + item.Rows + newRows)
                else
                    None

            let inlDelta =
                if rows <= 32.0 && canIndexJoin item applicable then
                    Some(rows + newRows)
                else
                    None

            [ hashDelta ] @ Option.toList mergeDelta @ Option.toList inlDelta |> List.min

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
        (initialRowBytes: float)
        (initialJoined: Set<string>)
        (initialOrder: string list option)
        (workMemBytes: float)
        (edges: JoinOrderEdge list)
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
                    RowBytes = initialRowBytes
                    Ordered = initialOrder
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
                                    let applicable = applicableEdges edges joined item.Name
                                    let newRows = nextRows edges joined state.Rows item

                                    let delta =
                                        transitionDelta
                                            state.Rows
                                            state.RowBytes
                                            state.Ordered
                                            workMemBytes
                                            applicable
                                            item
                                            newRows

                                    let candidate =
                                        {
                                            Cost = state.Cost + delta
                                            Rows = newRows
                                            RowBytes = state.RowBytes + item.RowBytes
                                            Ordered = state.Ordered
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
    let choose initialRows initialRowBytes initialJoined initialOrder workMemBytes edges items dependencies =
        selinger initialRows initialRowBytes initialJoined initialOrder workMemBytes edges items dependencies
        |> Option.defaultWith (fun () -> greedy initialRows initialJoined edges items dependencies)
