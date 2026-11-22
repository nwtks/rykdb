// ===========================================================================
// JoinOrder.fs のテスト（結合順の貪欲法と Selinger DP）
// ===========================================================================

module RykDb.JoinOrderTests

open Xunit
open RykDb.Core

let private item name rows position : JoinOrderItem =
    {
        Name = name
        Rows = rows
        Position = position
        Indexes = []
        RowBytes = 32.0
        NaturalOrder = None
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
    let edges = [ edge "a" "b" ]

    let order =
        JoinOrder.choose 1.0 1.0 (Set.singleton "a") None -1.0 edges items Map.empty

    Assert.Equal<string list>([ "c"; "d"; "b" ], order)

[<Fact>]
let ``JoinOrder.selinger: 小さい左と索引がある辺は INL のコストで優先する`` () =
    let items =
        [
            itemWith "b" 1_000_000.0 2 [ index "ix_k" [ "k" ] [ 100L ] false ] None
            item "c" 1_000_000.0 1
        ]

    let edges =
        [ keyedEdge (Some 0.1) (Map.ofList [ "a", "j"; "b", "k" ]); edge "a" "c" ]

    let order = selinger 10.0 32.0 (Set.singleton "a") None -1.0 edges items Map.empty
    Assert.Equal<string list option>(Some [ "b"; "c" ], order)

    // 索引情報が無ければ位置順で c が先になる。
    let noIndex = [ item "b" 1_000_000.0 2; item "c" 1_000_000.0 1 ]

    let orderNoIndex =
        selinger 10.0 32.0 (Set.singleton "a") None -1.0 edges noIndex Map.empty

    Assert.Equal<string list option>(Some [ "c"; "b" ], orderNoIndex)

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
