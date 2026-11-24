// ===========================================================================
// JoinOrder — INNER 結合区間の結合順を決める（貪欲法と Selinger DP）。
// ===========================================================================

namespace RykDb.Core

/// DP の結果（順序とコスト）。
type internal JoinOrderPlan = { Order: string list; Cost: float }

/// bushy 計画の木（葉はテーブル名、節は結合）。
[<RequireQualifiedAccess>]
type internal JoinOrderTree =
    | Leaf of string
    | Branch of left: JoinOrderTree * right: JoinOrderTree

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
        /// 右部分木を索引で引ける形か（`Scan` か `Filter` の連なり）。
        /// 実行時のインデックス入れ子ループは右部分木を実行しないので、
        /// 実行しない形（派生テーブルなど）は `false` にして INL を候補にしない。
        Lookupable: bool
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
            /// 先頭が実際に走査のときだけ意味がある（マージ結合は左が `Scan` のときだけ実行される）。
            Ordered: string list option
            /// 左の入力が走査か（結合を重ねた後は false）。
            ScanInput: bool
            Order: int list
        }

    /// 右テーブルを索引で引ける結合辺のうち、最も選択性の高い索引を選ぶ。
    /// 結合キー全体が 1つの索引の prefix に収まる場合だけ INL にする
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
    /// 選択性が分からない辺は 0.1 を使う。0行の入力は 0 のまま返す。
    let private nextRows (edges: JoinOrderEdge list) (joined: Set<string>) (joinedRows: float) (item: JoinOrderItem) =
        let applicable = applicableEdges edges joined item.Name

        if List.isEmpty applicable then
            joinedRows * item.Rows
        else
            let product =
                applicable
                |> List.fold (fun acc e -> acc * (e.Selectivity |> Option.defaultValue 0.1)) (joinedRows * item.Rows)

            if product <= 0.0 then 0.0 else max 1.0 product

    /// 貪欲法で結合順を選ぶ再帰ループ。
    [<TailCall>]
    let rec private greedyGo
        (dependencies: Map<string, Set<string>>)
        (edges: JoinOrderEdge list)
        (joinedRows: float)
        (joined: Set<string>)
        (remaining: JoinOrderItem list)
        (acc: string list)
        =
        match remaining with
        | [] -> List.rev acc
        | _ ->
            let ready =
                remaining |> List.filter (fun item -> eligible dependencies joined item.Name)

            let pool = if List.isEmpty ready then remaining else ready

            let item =
                pool
                |> List.minBy (fun item -> nextRows edges joined joinedRows item, item.Position)

            greedyGo
                dependencies
                edges
                (nextRows edges joined joinedRows item)
                (Set.add item.Name joined)
                (remaining |> List.filter (fun x -> x.Name <> item.Name))
                (item.Name :: acc)

    /// 現行の貪欲法。先頭テーブルと LEFT の境界は呼び出し側が固定する。
    /// A/B・診断用に内部へ公開する。
    let greedy
        (initialRows: float)
        (initialJoined: Set<string>)
        (edges: JoinOrderEdge list)
        (items: JoinOrderItem list)
        (dependencies: Map<string, Set<string>>)
        =
        greedyGo dependencies edges initialRows initialJoined items []

    /// 右の自然順と結合辺を照合して出力順を組み立てる。
    [<TailCall>]
    let rec private mergeKeyOrderLoop
        (left: string list)
        (right: string list)
        (item: JoinOrderItem)
        (applicable: JoinOrderEdge list)
        (i: int)
        (acc: string list)
        =
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
                    mergeKeyOrderLoop left right item applicable (i + 1) (left.[i] :: acc)
                else
                    None

    /// 両側の自然順が結合キーで揃っているときの、結合後の出力順。
    /// 右の走査順の先頭から、結合辺がある限り一致した列名を返す。
    let private mergeKeyOrder (ordered: string list option) (item: JoinOrderItem) (applicable: JoinOrderEdge list) =
        match ordered, item.NaturalOrder with
        | Some left, Some right -> mergeKeyOrderLoop left right item applicable 0 []
        | _ -> None

    /// 1テーブル追加時の (コスト, 結合後の出力順) の候補。
    /// ハッシュ・INL・マージはそれぞれ出力順が違うので、すべて候補にする。
    /// 実行時の `Analyzer.chooseJoinAlgorithmWith` と同じ条件で選ぶ。
    let private transitionVariants
        (rows: float)
        (rowBytes: float)
        (leftIsScan: bool)
        (ordered: string list option)
        (workMemBytes: float)
        (applicable: JoinOrderEdge list)
        (item: JoinOrderItem)
        (newRows: float)
        =
        if List.isEmpty applicable then
            [ (rows * item.Rows + newRows, None) ]
        elif applicable |> List.forall (fun e -> Map.isEmpty e.KeyColumns) then
            [ (rows * item.Rows + newRows, None) ]
        else
            let hashDelta =
                let buildRows = min rows item.Rows
                let probeRows = max rows item.Rows
                let buildRowBytes = if rows < item.Rows then rowBytes else item.RowBytes
                let buildCost = buildRows
                let probeCost = probeRows
                let buildPenalty = 0.1 * buildRows

                let spillPenalty =
                    let spilled = Analyzer.spilledBuildRows buildRows buildRowBytes workMemBytes

                    if spilled <= 0.0 then
                        0.0
                    else
                        spilled * 2.0 + float Spill.Partitions * 8.0

                buildCost + probeCost + newRows + buildPenalty + spillPenalty

            let mergeVariant =
                if leftIsScan then
                    mergeKeyOrder ordered item applicable
                    |> Option.map (fun keyOrder -> rows + item.Rows + newRows, Some keyOrder)
                else
                    None

            let inlVariant =
                if item.Lookupable && rows <= 32.0 then
                    bestIndexFor item applicable
                    |> Option.map (fun index ->
                        let probeWeight = if index.Unique then 0.5 else 0.8
                        rows * probeWeight + newRows, ordered)
                else
                    None

            [ (hashDelta, None) ] @ Option.toList mergeVariant @ Option.toList inlVariant

    /// 状態の順序を FROM 内の位置列へ変換する。
    let private positions (items: JoinOrderItem array) (state: PlanState) =
        state.Order |> List.map (fun i -> items.[i].Position)

    /// 同じ出力順の状態として候補を採用するか。
    /// コストが同じなら FROM の位置順（辞書式）で決める。
    let private isBetter (items: JoinOrderItem array) (candidate: PlanState) (current: PlanState option) =
        match current with
        | None -> true
        | Some old -> (candidate.Cost, positions items candidate) < (old.Cost, positions items old)

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

    /// mask に含まれるテーブル名と初期集合の和集合を返す。
    let private joinedOf (itemArray: JoinOrderItem array) (initialJoined: Set<string>) (mask: int) =
        let names =
            itemArray
            |> Array.indexed
            |> Array.choose (fun (i, item) -> if mask &&& (1 <<< i) <> 0 then Some item.Name else None)
            |> Array.toList

        Set.union initialJoined (Set.ofList names)

    /// 初期状態から部分集合ごとの状態リストを埋める（左深 DP の本体）。
    let private dpCore
        (itemArray: JoinOrderItem array)
        (edges: JoinOrderEdge list)
        (dependencies: Map<string, Set<string>>)
        (workMemBytes: float)
        (initialJoined: Set<string>)
        (startMask: int)
        (initial: PlanState)
        =
        let fullMask = (1 <<< itemArray.Length) - 1
        let maxOrdersPerMask = 8

        seq { 0..fullMask }
        |> Seq.fold
            (fun states mask ->
                match states |> Map.tryFind mask with
                | None -> states
                | Some stateList ->
                    let joined = joinedOf itemArray initialJoined mask

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
                                            state.ScanInput
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
                                                        ScanInput = false
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
            (Map.ofList [ (startMask, [ initial ]) ])

    /// `mask` の最良状態（コスト最小）。
    let private bestState (mask: int) =
        Map.tryFind mask
        >> Option.bind (fun list -> list |> List.sortBy (fun s -> s.Cost) |> List.tryHead)

    /// 左深の順序から木を組み立てる。
    let private treeOfOrder (itemArray: JoinOrderItem array) =
        List.map (fun i -> JoinOrderTree.Leaf itemArray.[i].Name)
        >> List.reduce (fun l r -> JoinOrderTree.Branch(l, r))

    /// テーブル集合 `mask` の名前集合。
    let private namesOf (mask: int) =
        Array.indexed
        >> Array.choose (fun (i, item) -> if mask &&& (1 <<< i) <> 0 then Some item.Name else None)
        >> Set.ofArray

    /// 全開始点から mask の最良状態を選ぶ。
    let private bestOf (perFirst: (int * Map<int, PlanState list>) array) (mask: int) =
        perFirst
        |> Array.choose (fun (_, states) -> bestState mask states |> Option.map (fun s -> s.Cost, s))
        |> Array.sortBy fst
        |> Array.tryHead

    /// mask 内のテーブルが依存をすべて満たしているか。
    let private dependencyClosed (itemArray: JoinOrderItem array) (dependencies: Map<string, Set<string>>) (mask: int) =
        let names = namesOf mask itemArray

        itemArray
        |> Array.indexed
        |> Array.forall (fun (i, item) ->
            if mask &&& (1 <<< i) <> 0 then
                match dependencies |> Map.tryFind item.Name with
                | None -> true
                | Some refs -> Set.isSubset refs names
            else
                true)

    /// 左右の部分木を結合したときの追加コストと行数を返す。
    let private splitCost
        (edges: JoinOrderEdge list)
        (workMemBytes: float)
        (leftNames: Set<string>)
        (rightNames: Set<string>)
        (lRows: float)
        (lRowBytes: float)
        (rRows: float)
        (rRowBytes: float)
        =
        let crossing =
            edges
            |> List.filter (fun e ->
                not (Set.isEmpty (Set.intersect e.Refs leftNames))
                && not (Set.isEmpty (Set.intersect e.Refs rightNames))
                && Set.isSubset e.Refs (Set.union leftNames rightNames))

        if List.isEmpty crossing then
            None
        else
            let selectivity =
                crossing
                |> List.fold (fun acc e -> acc * (e.Selectivity |> Option.defaultValue 0.1)) 1.0

            let newRows =
                if lRows * rRows * selectivity <= 0.0 then
                    0.0
                else
                    max 1.0 (lRows * rRows * selectivity)

            if crossing |> List.forall (fun e -> Map.isEmpty e.KeyColumns) then
                Some(lRows * rRows + newRows, newRows)
            else
                let buildRows = min lRows rRows
                let buildRowBytes = if lRows < rRows then lRowBytes else rRowBytes
                let spilled = Analyzer.spilledBuildRows buildRows buildRowBytes workMemBytes

                let spillPenalty =
                    if spilled <= 0.0 then
                        0.0
                    else
                        spilled * 2.0 + float Spill.Partitions * 8.0

                let hashDelta =
                    buildRows + max lRows rRows + newRows + 0.1 * buildRows + spillPenalty

                Some(hashDelta, newRows)

    /// bushy 計画を使える条件を満たすか（件数・有限コスト・依存の閉包）。
    let private canUseBushy (itemArray: JoinOrderItem array) (dependencies: Map<string, Set<string>>) =
        let count = itemArray.Length
        let full = (1 <<< count) - 1

        count >= 3
        && count <= 6
        && not (itemArray |> Array.exists (fun item -> not (System.Double.IsFinite item.Rows)))
        && not (
            dependencies
            |> Map.exists (fun _ refs -> refs |> Set.exists (fun r -> not (Set.contains r (namesOf full itemArray))))
        )

    /// 先頭テーブル候補ごとに左深 DP を回す（部分集合の最良計画を引くため）。
    let private leftDeepByFirst
        (itemArray: JoinOrderItem array)
        (edges: JoinOrderEdge list)
        (dependencies: Map<string, Set<string>>)
        (workMemBytes: float)
        =
        itemArray
        |> Array.mapi (fun f item ->
            let initial =
                {
                    Cost = 0.0
                    Rows = item.Rows
                    RowBytes = item.RowBytes
                    Ordered = item.NaturalOrder
                    ScanInput = Option.isSome item.NaturalOrder
                    Order = [ f ]
                }

            f, dpCore itemArray edges dependencies workMemBytes (Set.singleton item.Name) (1 <<< f) initial)

    /// 全 2分割のうち最良の分割を返す。
    let private bestBushySplit
        (itemArray: JoinOrderItem array)
        (edges: JoinOrderEdge list)
        (dependencies: Map<string, Set<string>>)
        (workMemBytes: float)
        (perFirst: (int * Map<int, PlanState list>) array)
        (full: int)
        =
        seq { 1 .. full - 1 }
        |> Seq.filter (fun l -> l < full ^^^ l)
        |> Seq.choose (fun l ->
            let r = full ^^^ l

            if
                not (dependencyClosed itemArray dependencies l)
                || not (dependencyClosed itemArray dependencies r)
            then
                None
            else
                match bestOf perFirst l, bestOf perFirst r with
                | Some(lCost, lState), Some(rCost, rState) ->
                    let leftNames = namesOf l itemArray
                    let rightNames = namesOf r itemArray

                    splitCost
                        edges
                        workMemBytes
                        leftNames
                        rightNames
                        lState.Rows
                        lState.RowBytes
                        rState.Rows
                        rState.RowBytes
                    |> Option.map (fun (joinDelta, _) -> lCost + rCost + joinDelta, lState, rState)
                | _ -> None)
        |> Seq.sortBy (fun (cost, _, _) -> cost)
        |> Seq.tryHead

    /// 分割と左深のうち安い方の木を選ぶ。
    let private chooseBushyTree
        (itemArray: JoinOrderItem array)
        (perFirst: (int * Map<int, PlanState list>) array)
        (full: int)
        (split: (float * PlanState * PlanState) option)
        =
        match split with
        | Some(splitCostValue, lState, rState) ->
            match bestOf perFirst full with
            | Some(leftDeepCost, leftDeepState) when leftDeepCost <= splitCostValue ->
                Some(treeOfOrder itemArray leftDeepState.Order)
            | _ -> Some(JoinOrderTree.Branch(treeOfOrder itemArray lState.Order, treeOfOrder itemArray rState.Order))
        | None ->
            bestOf perFirst full
            |> Option.map (fun (_, state) -> treeOfOrder itemArray state.Order)

    /// bushy 計画（1段の分割。両側は左深）。
    /// 適用できないときは None。
    let bushyPlan
        (workMemBytes: float)
        (edges: JoinOrderEdge list)
        (items: JoinOrderItem list)
        (dependencies: Map<string, Set<string>>)
        =
        let itemArray = Array.ofList items
        let full = (1 <<< itemArray.Length) - 1

        if not (canUseBushy itemArray dependencies) then
            None
        else
            let perFirst = leftDeepByFirst itemArray edges dependencies workMemBytes
            let split = bestBushySplit itemArray edges dependencies workMemBytes perFirst full
            chooseBushyTree itemArray perFirst full split

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
                    ScanInput = Option.isSome initialOrder
                    Order = []
                }

            dpCore itemArray edges dependencies workMemBytes initialJoined 0 initial
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
