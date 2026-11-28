// ===========================================================================
// JoinOrder.fs のテスト（結合順の貪欲法と Selinger DP）
// ===========================================================================

module RykDb.JoinOrderTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private item name rows position : JoinOrderItem =
    {
        Name = name
        Rows = rows
        Position = position
        Indexes = []
        RowBytes = 32.0
        NaturalOrder = None
        Lookupable = true
    }

let private index name columns prefixNdv unique : JoinOrderIndex =
    {
        IndexName = name
        IndexColumns = columns
        PrefixNDV = prefixNdv
        Unique = unique
    }

let private edgeWith selectivity a b : JoinOrderEdge =
    {
        Refs = Set.ofList [ a; b ]
        Selectivity = selectivity
        KeyColumns = Map.empty
    }

let private edge a b = edgeWith None a b

let private itemWith name rows position indexes naturalOrder =
    { item name rows position with
        Indexes = indexes
        NaturalOrder = naturalOrder
    }

let private keyedEdge selectivity keys : JoinOrderEdge =
    {
        Refs = keys |> Map.toList |> List.map fst |> Set.ofList
        Selectivity = selectivity
        KeyColumns = keys
    }

/// bushy 木の葉（テーブル名）を集める。
let private leavesOf tree =
    let rec go acc tree =
        match tree with
        | JoinOrderTree.Leaf name -> name :: acc
        | JoinOrderTree.Branch(l, r) -> go (go acc l) r

    go [] tree |> Set.ofList

/// 根のどちらかが部分木か（左深なら根の子はどちらも葉）。
let private isBushyShape tree =
    match tree with
    | JoinOrderTree.Branch(JoinOrderTree.Branch _, _)
    | JoinOrderTree.Branch(_, JoinOrderTree.Branch _) -> true
    | _ -> false

[<Fact>]
let ``JoinOrder.bushyPlan: 3テーブル以上で分割を検討する`` () =
    let items = [ item "a" 1000.0 0; item "b" 1000.0 1; item "c" 1000.0 2 ]
    let edges = [ edgeWith (Some 0.5) "a" "b"; edgeWith (Some 0.01) "b" "c" ]
    let plan = JoinOrder.bushyPlan -1.0 edges items Map.empty
    let tree = expectSome "bushy 計画" plan
    Assert.Equal<Set<string>>(Set.ofList [ "a"; "b"; "c" ], leavesOf tree)

[<Fact>]
let ``JoinOrder.bushyPlan: 2テーブル以下では使わない`` () =
    let items = [ item "a" 10.0 0; item "b" 10.0 1 ]
    Assert.True((JoinOrder.bushyPlan -1.0 [ edge "a" "b" ] items Map.empty).IsNone)

[<Fact>]
let ``JoinOrder.bushyPlan: 7テーブル以上では使わない`` () =
    let items = [ for i in 0..6 -> item (sprintf "t%d" i) 10.0 i ]
    Assert.True((JoinOrder.bushyPlan -1.0 [] items Map.empty).IsNone)

[<Fact>]
let ``JoinOrder.bushyPlan: 分割の方が安ければ右が部分木になる`` () =
    // a は大きい。b⋈c が極端に小さくなり、最後に a と結合する方が安い。
    let items =
        [ item "a" 1_000_000.0 0; item "b" 1_000_000.0 1; item "c" 1_000_000.0 2 ]

    let edges = [ edgeWith (Some 0.0001) "b" "c"; edgeWith (Some 1.0) "a" "b" ]
    let tree = expectSome "bushy 計画" (JoinOrder.bushyPlan -1.0 edges items Map.empty)
    Assert.True(isBushyShape tree, sprintf "%A" tree)

/// 順序だけを返す DP。
let private selinger
    (initialRows: float)
    (initialRowBytes: float)
    (initialJoined: Set<string>)
    (initialOrder: string list option)
    (workMemBytes: float)
    (edges: JoinOrderEdge list)
    (items: JoinOrderItem list)
    (dependencies: Map<string, Set<string>>)
    =
    JoinOrder.selingerPlan initialRows initialRowBytes initialJoined initialOrder workMemBytes edges items dependencies
    |> Option.map _.Order

[<Fact>]
let ``JoinOrder.choose: DP が使えるときは DP の順を返す`` () =
    let items = [ item "b" 1000.0 1; item "c" 100.0 2; item "d" 100.0 3 ]
    let edges = [ keyedEdge (Some 0.1) (Map.ofList [ "a", "k"; "b", "k" ]) ]

    let order =
        JoinOrder.choose 1.0 1.0 (Set.singleton "a") None -1.0 edges items Map.empty

    Assert.Equal<string list>([ "c"; "d"; "b" ], order)

[<Fact>]
let ``JoinOrder.selinger: 小さい左と索引がある辺は INL のコストで優先する`` () =
    let edges = [ keyedEdge (Some 0.1) (Map.ofList [ "a", "j"; "b", "k" ]) ]

    let itemWithIndex =
        itemWith "b" 1_000_000.0 2 [ index "ix_k" [ "k" ] [ 100L ] false ] None

    let withIndex =
        expectSome
            "DP の計画"
            (JoinOrder.selingerPlan 10.0 32.0 (Set.singleton "a") None -1.0 edges [ itemWithIndex ] Map.empty)

    // 索引が無ければハッシュ結合（build + probe）のコストになる。
    let withoutIndex =
        expectSome
            "DP の計画"
            (JoinOrder.selingerPlan 10.0 32.0 (Set.singleton "a") None -1.0 edges [ item "b" 1_000_000.0 2 ] Map.empty)

    Assert.True(withIndex.Cost < withoutIndex.Cost, sprintf "INL %g < ハッシュ %g" withIndex.Cost withoutIndex.Cost)

[<Fact>]
let ``JoinOrder.selinger: 等値キーの無い辺は入れ子ループのコストで見積もる`` () =
    // 実行時は同じ型の等値キーが無ければ入れ子ループ（直積 + 残差）になる。
    // ハッシュ結合（build + probe）のコストにしてはいけない。
    let items = [ item "b" 100.0 1; item "c" 100.0 2 ]
    let residualEdges = [ edge "a" "b"; edge "a" "c" ]

    let nested =
        expectSome
            "DP の計画"
            (JoinOrder.selingerPlan 10.0 32.0 (Set.singleton "a") None -1.0 residualEdges items Map.empty)

    let keyedEdges =
        [
            keyedEdge None (Map.ofList [ "a", "k"; "b", "k" ])
            keyedEdge None (Map.ofList [ "a", "k"; "c", "k" ])
        ]

    let hashed =
        expectSome "DP の計画" (JoinOrder.selingerPlan 10.0 32.0 (Set.singleton "a") None -1.0 keyedEdges items Map.empty)

    // 10*100 + 100 + (100*100 + 1000) = 12,100 が入れ子ループの見積り。
    Assert.Equal(12100.0, nested.Cost, 3)
    Assert.True(hashed.Cost < nested.Cost, sprintf "ハッシュ %g < 入れ子ループ %g" hashed.Cost nested.Cost)

[<Fact>]
let ``JoinOrder.selinger: 右が走査でなければ INL のコストにしない`` () =
    // 実行時のインデックス入れ子ループは右が `Scan`（または `Filter` の連なり）のときだけ。
    let edges = [ keyedEdge (Some 0.1) (Map.ofList [ "a", "j"; "b", "k" ]) ]
    let indexes = [ index "ix_k" [ "k" ] [ 100L ] true ]
    let lookupable = itemWith "b" 1_000.0 2 indexes None
    let notLookupable = { lookupable with Lookupable = false }

    let withLookup =
        expectSome
            "DP の計画"
            (JoinOrder.selingerPlan 1.0 32.0 (Set.singleton "a") None -1.0 edges [ lookupable ] Map.empty)

    let withoutLookup =
        expectSome
            "DP の計画"
            (JoinOrder.selingerPlan 1.0 32.0 (Set.singleton "a") None -1.0 edges [ notLookupable ] Map.empty)

    // 右を実行しない INL は 1*0.5 + 100 = 100.5、ハッシュは 1 + 1000 + 100 + 0.1 = 1101.1。
    Assert.Equal(100.5, withLookup.Cost, 3)
    Assert.Equal(1101.1, withoutLookup.Cost, 3)

[<Fact>]
let ``JoinOrder.selinger: マージ結合は左が走査のときだけ候補にする`` () =
    // 実行時は左部分木の走査順（`Analyzer.scanOrder`）を使うので、
    // 結合を重ねた左ではマージ結合を選ばない。
    let items =
        [
            itemWith "b" 100.0 1 [ index "ix_k" [ "k" ] [ 100L ] true ] (Some [ "k" ])
            itemWith "c" 100.0 2 [ index "iy_k" [ "k" ] [ 100L ] true ] (Some [ "k" ])
        ]

    let edges =
        [
            keyedEdge (Some 0.1) (Map.ofList [ "a", "k"; "b", "k" ])
            keyedEdge (Some 0.1) (Map.ofList [ "b", "k"; "c", "k" ])
        ]

    let plan =
        expectSome
            "DP の計画"
            (JoinOrder.selingerPlan 1.0 32.0 (Set.singleton "a") (Some [ "k" ]) 100.0 edges items Map.empty)

    // 先頭は a のつもりなので、b と c の順だけを見る（マージ連鎖は前提にしない）。
    Assert.Equal<string list>([ "b"; "c" ], plan.Order)

[<Fact>]
let ``JoinOrder.selinger: 同点は FROM の位置順にする`` () =
    let items = [ item "b" 10.0 1; item "c" 10.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]
    let order = selinger 100.0 1.0 (Set.singleton "a") None -1.0 edges items Map.empty
    Assert.Equal<string list option>(Some [ "b"; "c" ], order)

[<Fact>]
let ``JoinOrder.selinger: ON の参照先が揃うまで待つ`` () =
    let items = [ item "b" 100.0 1; item "c" 100.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]

    let dependencies =
        Map.ofList [ "b", Set.ofList [ "a"; "b"; "c" ]; "c", Set.ofList [ "a"; "c" ] ]

    let order = selinger 1.0 1.0 (Set.singleton "a") None -1.0 edges items dependencies
    Assert.Equal<string list option>(Some [ "c"; "b" ], order)

[<Fact>]
let ``JoinOrder.selinger: 未知の依存先があれば DP を使わない`` () =
    let items = [ item "b" 10.0 1 ]
    let dependencies = Map.ofList [ ("b", Set.ofList [ "a"; "x" ]) ]
    let order = selinger 1.0 1.0 (Set.singleton "a") None -1.0 [] items dependencies
    Assert.True order.IsNone

[<Fact>]
let ``JoinOrder.selinger: 推定が無限なら DP を使わない`` () =
    let items = [ item "b" 10.0 1; item "c" 10.0 2 ]

    let order =
        selinger System.Double.PositiveInfinity 1.0 (Set.singleton "a") None -1.0 [] items Map.empty

    Assert.True order.IsNone

[<Fact>]
let ``JoinOrder.selinger: 上限を超えるテーブル数では DP を使わない`` () =
    let items = [ for i in 1..13 -> item (sprintf "t%d" i) 10.0 i ]
    let items = item "a" 1.0 0 :: items

    let order =
        selinger 1.0 1.0 (Set.singleton "a") None -1.0 [] (List.tail items) Map.empty

    Assert.True order.IsNone

[<Fact>]
let ``JoinOrder.selinger: 作業メモリを超えるとスピル分のコストが乗る`` () =
    // build 側（1000行 × 32B = 32,000B）が 1KB の作業メモリに収まらない。
    // 退避した行は書き出し + 読み直しの 2回としてコストに乗る。
    let items = [ item "b" 1000.0 1 ]
    let edges = [ keyedEdge None (Map.ofList [ "a", "k"; "b", "k" ]) ]

    let plan workMem =
        expectSome "DP の計画" (JoinOrder.selingerPlan 1000.0 32.0 (Set.singleton "a") None workMem edges items Map.empty)

    let unlimited = plan -1.0
    let spilled = Analyzer.spilledBuildRows 1000.0 32.0 1024.0
    let expected = unlimited.Cost + spilled * 2.0 + float Spill.Partitions * 8.0
    Assert.Equal(expected, (plan 1024.0).Cost, 3)

[<Fact>]
let ``JoinOrder.selinger: 主キーと索引の両方があるとき主キーの INL コストを使う`` () =
    // 実行側は主キーを優先する。計画側も同じ判定を使わないと
    // 「DP は索引のコストで選ぶが実行は主キー」という静かな不一致が起きる。
    let edges = [ keyedEdge (Some 0.1) (Map.ofList [ "a", "k"; "b", "k" ]) ]
    let indexes = [ index "__pk" [ "k" ] [] true; index "ix_k" [ "k" ] [ 1L ] false ]
    let item = itemWith "b" 1000.0 2 indexes None

    let plan =
        expectSome "DP の計画" (JoinOrder.selingerPlan 10.0 32.0 (Set.singleton "a") None -1.0 edges [ item ] Map.empty)

    // 主キー（一意）の probe 単価 0.5: 10 * 0.5 + 1000 = 1005。
    // 索引（非一意）なら 10 * 0.8 + 1000 = 1008 になる。
    Assert.Equal(1005.0, plan.Cost, 3)
