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
    }

let private edge a b = Set.ofList [ a; b ]

[<Fact>]
let ``JoinOrder.greedy: 中間行数が小さい候補を先に積む`` () =
    let items = [ item "b" 10.0 1; item "c" 1000.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]
    let order = JoinOrder.greedy 100.0 (Set.singleton "a") edges items Map.empty
    Assert.Equal<string list>([ "b"; "c" ], order)

[<Fact>]
let ``JoinOrder.selinger: 直積より後に大きな結合を回す`` () =
    // a(1) - b(1000)、c(100)、d(100) は結合述語を持たない。
    // 貪欲は b を先に積んでから c/d を直積する。DP は c/d の直積を先に済ませ、
    // b との結合（ハッシュ）を最後に回す。
    let items = [ item "b" 1000.0 1; item "c" 100.0 2; item "d" 100.0 3 ]
    let edges = [ edge "a" "b" ]
    let greedy = JoinOrder.greedy 1.0 (Set.singleton "a") edges items Map.empty
    let viaDp = JoinOrder.selinger 1.0 (Set.singleton "a") edges items Map.empty
    Assert.Equal<string list>([ "b"; "c"; "d" ], greedy)
    Assert.Equal<string list option>(Some [ "c"; "d"; "b" ], viaDp)

[<Fact>]
let ``JoinOrder.choose: DP が使えるときは DP の順を返す`` () =
    let items = [ item "b" 1000.0 1; item "c" 100.0 2; item "d" 100.0 3 ]
    let edges = [ edge "a" "b" ]
    let order = JoinOrder.choose 1.0 (Set.singleton "a") edges items Map.empty
    Assert.Equal<string list>([ "c"; "d"; "b" ], order)

[<Fact>]
let ``JoinOrder.selinger: 同点は FROM の位置順にする`` () =
    let items = [ item "b" 10.0 1; item "c" 10.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]
    let order = JoinOrder.selinger 100.0 (Set.singleton "a") edges items Map.empty
    Assert.Equal<string list option>(Some [ "b"; "c" ], order)

[<Fact>]
let ``JoinOrder.selinger: ON の参照先が揃うまで待つ`` () =
    let items = [ item "b" 100.0 1; item "c" 100.0 2 ]
    let edges = [ edge "a" "b"; edge "a" "c" ]

    let dependencies =
        Map.ofList [ "b", Set.ofList [ "a"; "b"; "c" ]; "c", Set.ofList [ "a"; "c" ] ]

    let order = JoinOrder.selinger 1.0 (Set.singleton "a") edges items dependencies
    Assert.Equal<string list option>(Some [ "c"; "b" ], order)

[<Fact>]
let ``JoinOrder.selinger: 未知の依存先があれば DP を使わない`` () =
    let items = [ item "b" 10.0 1 ]
    let dependencies = Map.ofList [ ("b", Set.ofList [ "a"; "x" ]) ]
    let order = JoinOrder.selinger 1.0 (Set.singleton "a") [] items dependencies
    Assert.True order.IsNone

[<Fact>]
let ``JoinOrder.selinger: 推定が無限なら DP を使わない`` () =
    let items = [ item "b" 10.0 1; item "c" 10.0 2 ]

    let order =
        JoinOrder.selinger System.Double.PositiveInfinity (Set.singleton "a") [] items Map.empty

    Assert.True order.IsNone

[<Fact>]
let ``JoinOrder.selinger: 上限を超えるテーブル数では DP を使わない`` () =
    let items = [ for i in 1..13 -> item (sprintf "t%d" i) 10.0 i ]
    let items = item "a" 1.0 0 :: items

    let order =
        JoinOrder.selinger 1.0 (Set.singleton "a") [] (List.tail items) Map.empty

    Assert.True order.IsNone
