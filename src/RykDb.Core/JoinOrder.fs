// ===========================================================================
// JoinOrder — INNER 結合区間の結合順を決める（貪欲法と Selinger DP）。
// ===========================================================================

namespace RykDb.Core

/// DP の結果（順序とコスト）。
type internal JoinOrderPlan = { Order: string list; Cost: float }

/// 結合順の INL 候補索引。
type internal JoinOrderIndex =
    {
        IndexName: string
        IndexColumns: string list
        PrefixNDV: int64 list
        Unique: bool
    }

/// 結合順の候補。
/// `Rows` は走査の推定行数、`Position` は FROM 内の位置。
type internal JoinOrderItem =
    {
        Name: string
        Rows: float
        Position: int
        /// このテーブルが持つ索引（INL の可否とコスト判定用）。
        Indexes: JoinOrderIndex list
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

    /// 右テーブルを索引で引ける結合辺のうち、最も選択性の高い索引を選ぶ。
    /// 結合キー全体が 1 つの索引の prefix に収まる場合だけ INL にする
    /// （実行時の `coversAll` / prefix NDV 最小の索引選択と同じ条件）。
    let private bestIndexFor (item: JoinOrderItem) (applicable: JoinOrderEdge list) =
        let keyColumns =
            applicable |> List.choose (fun edge -> edge.KeyColumns |> Map.tryFind item.Name)

        if List.isEmpty keyColumns then
            None
        else
            item.Indexes
            |> List.choose (fun index ->
                let prefix = List.truncate keyColumns.Length index.IndexColumns

                if
                    List.length prefix = keyColumns.Length
                    && prefix
                       |> List.forall (fun c -> keyColumns |> List.exists (fun key -> SqlName.equal c key))
                    && keyColumns
                       |> List.forall (fun key -> prefix |> List.exists (fun c -> SqlName.equal c key))
                then
                    let ndv =
                        index.PrefixNDV
                        |> List.tryItem (keyColumns.Length - 1)
                        |> Option.defaultValue System.Int64.MaxValue

                    Some(ndv, index)
                else
                    None)
            |> List.sortBy fst
            |> List.tryHead
            |> Option.map snd

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
    let private nextRows (edges: JoinOrderEdge list) (joined: Set<string>) (joinedRows: float) (item: JoinOrderItem) =
        let applicable = applicableEdges edges joined item.Name

        if List.isEmpty applicable then
            joinedRows * item.Rows
        else
            let product =
                applicable
                |> List.fold (fun acc e -> acc * (e.Selectivity |> Option.defaultValue 0.1)) (joinedRows * item.Rows)

            if product <= 0.0 then 0.0 else max 1.0 product

    /// 現行の貪欲法。先頭テーブルと LEFT の境界は呼び出し側が固定する。
    /// A/B・診断用に内部へ公開する。
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

    /// 両側の自然順が結合キーで揃っているときの、結合後の出力順。
    /// 右の走査順の先頭から、結合辺がある限り一致した列名を返す。
    let private mergeKeyOrder (ordered: string list option) (item: JoinOrderItem) (applicable: JoinOrderEdge list) =
        match ordered, item.NaturalOrder with
        | Some left, Some right ->
            let rec loop i acc =
                if i >= List.length right then
                    Some(List.rev acc)
                else
                    let rightColumn = right.[i]

                    match
                        applicable
                        |> List.tryFind (fun e -> e.KeyColumns |> Map.tryFind item.Name = Some rightColumn)
                    with
                    | None -> if i > 0 then Some(List.rev acc) else None
                    | Some edge ->
                        if
                            i < List.length left
                            && edge.KeyColumns
                               |> Map.exists (fun name column -> name <> item.Name && SqlName.equal column left.[i])
                        then
                            loop (i + 1) (left.[i] :: acc)
                        else
                            None

            loop 0 []
        | _ -> None

    /// 1テーブル追加時の (コスト, 結合後の出力順) の候補。
    /// ハッシュ・INL・マージはそれぞれ出力順が違うので、すべて候補にする。
    let private transitionVariants
        (rows: float)
        (rowBytes: float)
        (ordered: string list option)
        (workMemBytes: float)
        (applicable: JoinOrderEdge list)
        (item: JoinOrderItem)
        (newRows: float)
        =
        if List.isEmpty applicable then
            [ (rows * item.Rows + newRows, None) ]
        else
            let hashDelta =
                let buildRows = min rows item.Rows
                let probeRows = max rows item.Rows
                let buildRowBytes = if rows < item.Rows then rowBytes else item.RowBytes
                let buildBytes = buildRows * buildRowBytes
                let buildCost = buildRows
                let probeCost = probeRows
                let buildPenalty = 0.1 * buildRows

                let spillPenalty =
                    if workMemBytes >= 0.0 && buildBytes > workMemBytes then
                        buildRows * 2.0 + float Spill.Partitions * 8.0
                    else
                        0.0

                buildCost + probeCost + newRows + buildPenalty + spillPenalty

            let mergeVariant =
                mergeKeyOrder ordered item applicable
                |> Option.map (fun keyOrder -> rows + item.Rows + newRows, Some keyOrder)

            let inlVariant =
                if rows <= 32.0 then
                    bestIndexFor item applicable
                    |> Option.map (fun index ->
                        let probeWeight = if index.Unique then 0.5 else 0.8
                        rows * probeWeight + newRows, ordered)
                else
                    None

            [ (hashDelta, None) ] @ Option.toList mergeVariant @ Option.toList inlVariant

    /// 同じ出力順の状態として候補を採用するか。
    /// コストが同じなら FROM の位置順（辞書式）で決める。
    let private isBetter (items: JoinOrderItem array) (candidate: PlanState) (current: PlanState option) =
        let positions state =
            state.Order |> List.map (fun i -> items.[i].Position)

        match current with
        | None -> true
        | Some old -> (candidate.Cost, positions candidate) < (old.Cost, positions old)

    /// 出力順の同一性（列名の大文字小文字は区別しない）。
    let private sameOrder (a: string list option) (b: string list option) =
        match a, b with
        | None, None -> true
        | Some x, Some y -> List.length x = List.length y && List.forall2 SqlName.equal x y
        | _ -> false

    /// 同一 mask の状態リストへ候補を追加する。
    /// 出力順ごとに最良の 1件を残し、コスト順に `maxOrders` 件まで保持する。
    let private insertState
        (items: JoinOrderItem array)
        (maxOrders: int)
        (candidate: PlanState)
        (current: PlanState list)
        =
        let replaced, others =
            current |> List.partition (fun s -> sameOrder s.Ordered candidate.Ordered)

        let sameOrderBest =
            match replaced with
            | [] -> candidate
            | existing :: _ ->
                if isBetter items candidate (Some existing) then
                    candidate
                else
                    existing

        sameOrderBest :: others
        |> List.sortBy (fun s -> s.Cost)
        |> List.truncate maxOrders

    /// Selinger DP（左深・先頭固定）。
    /// 適用できないときは None（呼び出し側が貪欲へフォールバックする）。
    let selingerPlan
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
            let maxOrdersPerMask = 8

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
                    | Some stateList ->
                        let joined = joinedOf itemArray mask

                        stateList
                        |> List.fold
                            (fun states state ->
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

                                            transitionVariants
                                                state.Rows
                                                state.RowBytes
                                                state.Ordered
                                                workMemBytes
                                                applicable
                                                item
                                                newRows
                                            |> List.fold
                                                (fun states (delta, outOrder) ->
                                                    let candidate =
                                                        {
                                                            Cost = state.Cost + delta
                                                            Rows = newRows
                                                            RowBytes = state.RowBytes + item.RowBytes
                                                            Ordered = outOrder
                                                            Order = state.Order @ [ i ]
                                                        }

                                                    let current =
                                                        states |> Map.tryFind newMask |> Option.defaultValue []

                                                    states
                                                    |> Map.add
                                                        newMask
                                                        (insertState itemArray maxOrdersPerMask candidate current))
                                                states)
                                    states)
                            states)
                (Map.ofList [ (0, [ initial ]) ])
            |> Map.tryFind fullMask
            |> Option.map (fun stateList ->
                let best = stateList |> List.minBy (fun s -> s.Cost)

                {
                    Order = best.Order |> List.map (fun i -> itemArray.[i].Name)
                    Cost = best.Cost
                })

    /// DP が使えれば DP、使えなければ貪欲法。
    let choose
        (initialRows: float)
        (initialRowBytes: float)
        (initialJoined: Set<string>)
        (initialOrder: string list option)
        (workMemBytes: float)
        (edges: JoinOrderEdge list)
        (items: JoinOrderItem list)
        (dependencies: Map<string, Set<string>>)
        =
        selingerPlan initialRows initialRowBytes initialJoined initialOrder workMemBytes edges items dependencies
        |> Option.map _.Order
        |> Option.defaultWith (fun () -> greedy initialRows initialJoined edges items dependencies)
