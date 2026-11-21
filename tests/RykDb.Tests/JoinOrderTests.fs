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
        IndexColumns = []
        RowBytes = 32.0
        NaturalOrder = None
    }

let private edgeWith selectivity a b : JoinOrderEdge =
    {
        Refs = Set.ofList [ a; b ]
        Selectivity = selectivity
        KeyColumns = Map.empty
    }

let private edge a b = edgeWith None a b

let private itemWith name rows position indexColumns naturalOrder =
    { item name rows position with
        IndexColumns = indexColumns
        NaturalOrder = naturalOrder
    }

let private keyedEdge selectivity keys : JoinOrderEdge =
    {
        Refs = keys |> Map.toList |> List.map fst |> Set.ofList
        Selectivity = selectivity
        KeyColumns = keys
    }

[<Fact>]
let ``JoinOrder.greedy: 中間行数が小さい候補を先に積む`` () =
    let items = [ item "b" 10.0 1; item "c" 1000.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]
    let order = JoinOrder.greedy 100.0 (Set.singleton "a") edges items Map.empty
    Assert.Equal<string list>([ "b"; "c" ], order)

[<Fact>]
let ``JoinOrder.greedy: 辺ごとの選択性を使う`` () =
    let items = [ item "b" 100.0 1; item "c" 100.0 2 ]
    // b は 1%、c は 50%。b の方が中間行数が小さい。
    let edges = [ edgeWith (Some 0.01) "a" "b"; edgeWith (Some 0.5) "a" "c" ]
    let order = JoinOrder.greedy 1.0 (Set.singleton "a") edges items Map.empty
    Assert.Equal<string list>([ "b"; "c" ], order)

[<Fact>]
let ``JoinOrder.selinger: 直積より後に大きな結合を回す`` () =
    // a(1) - b(1000)、c(100)、d(100) は結合述語を持たない。
    // 貪欲は b を先に積んでから c/d を直積する。DP は c/d の直積を先に済ませ、
    // b との結合（ハッシュ）を最後に回す。
    let items = [ item "b" 1000.0 1; item "c" 100.0 2; item "d" 100.0 3 ]
    let edges = [ edge "a" "b" ]
    let greedy = JoinOrder.greedy 1.0 (Set.singleton "a") edges items Map.empty

    let viaDp =
        JoinOrder.selinger 1.0 1.0 (Set.singleton "a") None -1.0 edges items Map.empty

    Assert.Equal<string list>([ "b"; "c"; "d" ], greedy)
    Assert.Equal<string list option>(Some [ "c"; "d"; "b" ], viaDp)

[<Fact>]
let ``JoinOrder.choose: DP が使えるときは DP の順を返す`` () =
    let items = [ item "b" 1000.0 1; item "c" 100.0 2; item "d" 100.0 3 ]
    let edges = [ edge "a" "b" ]

    let order =
        JoinOrder.choose 1.0 1.0 (Set.singleton "a") None -1.0 edges items Map.empty

    Assert.Equal<string list>([ "c"; "d"; "b" ], order)

[<Fact>]
let ``JoinOrder.selinger: 小さい左と索引がある辺は INL のコストで優先する`` () =
    let items = [ itemWith "b" 1_000_000.0 2 [ [ "k" ] ] None; item "c" 1_000_000.0 1 ]

    let edges =
        [ keyedEdge (Some 0.1) (Map.ofList [ "a", "j"; "b", "k" ]); edge "a" "c" ]

    let order =
        JoinOrder.selinger 10.0 32.0 (Set.singleton "a") None -1.0 edges items Map.empty

    Assert.Equal<string list option>(Some [ "b"; "c" ], order)

    // 索引情報が無ければ位置順で c が先になる。
    let noIndex = [ item "b" 1_000_000.0 2; item "c" 1_000_000.0 1 ]

    let orderNoIndex =
        JoinOrder.selinger 10.0 32.0 (Set.singleton "a") None -1.0 edges noIndex Map.empty

    Assert.Equal<string list option>(Some [ "c"; "b" ], orderNoIndex)

[<Fact>]
let ``JoinOrder.selinger: 両側が結合キー順ならマージ結合のコストを使う`` () =
    let items = [ itemWith "b" 1_000_000.0 2 [] (Some [ "k" ]); item "c" 1_000_000.0 1 ]

    let edges =
        [ keyedEdge (Some 0.1) (Map.ofList [ "a", "k"; "b", "k" ]); edge "a" "c" ]

    let b = items.[0]
    let applicable = [ edges.[0] ]
    let newRows = JoinOrder.nextRows applicable (Set.singleton "a") 10.0 b

    let ordered =
        JoinOrder.transitionDelta 10.0 32.0 (Some [ "k" ]) -1.0 applicable b newRows

    let unordered = JoinOrder.transitionDelta 10.0 32.0 None -1.0 applicable b newRows
    Assert.True(ordered < unordered)

    // 同点ではないが、DP は順序付きの入力を活かせる順序を返す。
    let order =
        JoinOrder.selinger 10.0 32.0 (Set.singleton "a") (Some [ "k" ]) -1.0 edges items Map.empty

    Assert.True order.IsSome

[<Fact>]
let ``JoinOrder.transitionDelta: 作業メモリを超える build はスピルペナルティ`` () =
    let b = item "b" 1_000_000.0 1
    let e = edge "a" "b"

    let normal =
        JoinOrder.transitionDelta 1_000_000.0 32.0 None -1.0 [ e ] b 1_000_000.0

    let spilled =
        JoinOrder.transitionDelta 1_000_000.0 32.0 None 1.0 [ e ] b 1_000_000.0

    Assert.True(spilled > normal)

[<Fact>]
let ``JoinOrder.selinger: 同点は FROM の位置順にする`` () =
    let items = [ item "b" 10.0 1; item "c" 10.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]

    let order =
        JoinOrder.selinger 100.0 1.0 (Set.singleton "a") None -1.0 edges items Map.empty

    Assert.Equal<string list option>(Some [ "b"; "c" ], order)

[<Fact>]
let ``JoinOrder.selinger: ON の参照先が揃うまで待つ`` () =
    let items = [ item "b" 100.0 1; item "c" 100.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]

    let dependencies =
        Map.ofList [ "b", Set.ofList [ "a"; "b"; "c" ]; "c", Set.ofList [ "a"; "c" ] ]

    let order =
        JoinOrder.selinger 1.0 1.0 (Set.singleton "a") None -1.0 edges items dependencies

    Assert.Equal<string list option>(Some [ "c"; "b" ], order)

[<Fact>]
let ``JoinOrder.selinger: 未知の依存先があれば DP を使わない`` () =
    let items = [ item "b" 10.0 1 ]
    let dependencies = Map.ofList [ ("b", Set.ofList [ "a"; "x" ]) ]

    let order =
        JoinOrder.selinger 1.0 1.0 (Set.singleton "a") None -1.0 [] items dependencies

    Assert.True order.IsNone

[<Fact>]
let ``JoinOrder.selinger: 推定が無限なら DP を使わない`` () =
    let items = [ item "b" 10.0 1; item "c" 10.0 2 ]

    let order =
        JoinOrder.selinger System.Double.PositiveInfinity 1.0 (Set.singleton "a") None -1.0 [] items Map.empty

    Assert.True order.IsNone

[<Fact>]
let ``JoinOrder.selinger: 上限を超えるテーブル数では DP を使わない`` () =
    let items = [ for i in 1..13 -> item (sprintf "t%d" i) 10.0 i ]
    let items = item "a" 1.0 0 :: items

    let order =
        JoinOrder.selinger 1.0 1.0 (Set.singleton "a") None -1.0 [] (List.tail items) Map.empty

    Assert.True order.IsNone
