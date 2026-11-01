module RykDb.CommitIntentTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private unresolvedOf (dir: string) = (CommitIntent.pending dir |> ok).Length

/// ジャーナルファイルのパス。
let private journalOf (dir: string) = Path.Combine(dir, ".commit", "journal")

[<Fact>]
let ``RowStore: commit intent が残っていれば再オープンで全 keyspace を適用する`` () =
    withDir "rs-intent" (fun dir ->
        let b1 = "t:a", [| Entry.put (intKey 1) 1UL [| SqlValue.Int 1L |] |]
        let b2 = "t:b", [| Entry.put (intKey 2) 2UL [| SqlValue.Int 2L |] |]
        let store = RowStore.openStore dir |> ok
        let ident = CommitIntent.publish dir [ b1; b2 ] |> ok
        Assert.False(System.String.IsNullOrWhiteSpace ident)
        // 1 keyspace だけ書いたところでクラッシュした状態（intent は残す）
        RowStore.applyBatches 0UL store [ b1 ] |> ok |> ignore
        Assert.Equal(1, unresolvedOf dir)
        // クラッシュでハンドルを失った状況を模して、ディスクから開き直す。
        let reopened = RowStore.openStoreTakingOver dir |> ok

        Assert.Equal(
            Some 1UL,
            (RowStore.ensure "t:a" reopened |> ok).GetAt None (intKey 1)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(
            Some 2UL,
            (RowStore.ensure "t:b" reopened |> ok).GetAt None (intKey 2)
            |> ok
            |> Option.map _.SeqNo
        )
        // 適用後は解決され、未解決は無くなる
        Assert.Equal(0, unresolvedOf dir)
        RowStore.close reopened |> ok)

[<Fact>]
let ``RowStore: 複数 keyspace の commit は intent を追記して公開し、解決する`` () =
    withDir "rs-journal" (fun dir ->
        let b1 = "t:a", [| Entry.put (intKey 1) 1UL [| SqlValue.Int 1L |] |]
        let b2 = "t:b", [| Entry.put (intKey 2) 2UL [| SqlValue.Int 2L |] |]
        let store = RowStore.openStore dir |> ok
        let store2 = RowStore.writeBatches [ b1; b2 ] store |> ok
        // 公開と解決が両方記録され、未解決は残らない
        Assert.Equal(0, unresolvedOf dir)
        Assert.True(File.Exists(journalOf dir), "ジャーナルが作られていません")

        RowStore.close store2 |> ok
        let reopened = RowStore.openStoreTakingOver dir |> ok

        Assert.Equal(
            Some 1UL,
            (RowStore.ensure "t:a" reopened |> ok).GetAt None (intKey 1)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(
            Some 2UL,
            (RowStore.ensure "t:b" reopened |> ok).GetAt None (intKey 2)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(0, unresolvedOf dir)
        RowStore.close reopened |> ok)

[<Fact>]
let ``RowStore: 公開の追記が途中で切れた intent は再生しない`` () =
    withDir "rs-tear" (fun dir ->
        let b1 = "t:a", [| Entry.put (intKey 1) 1UL [| SqlValue.Int 1L |] |]
        let b2 = "t:b", [| Entry.put (intKey 2) 2UL [| SqlValue.Int 2L |] |]
        CommitIntent.publish dir [ b1; b2 ] |> ok |> ignore

        // 追記の途中で電源が落ちた状態（フレームの尾部が欠ける）。
        // まだ WAL に何も書いていないので、捨てるのが正しい。
        let jp = journalOf dir
        let bytes = File.ReadAllBytes jp
        File.WriteAllBytes(jp, Array.sub bytes 0 (bytes.Length - 5))

        let store = RowStore.openStoreTakingOver dir |> ok

        Assert.Equal(
            None,
            (RowStore.ensure "t:a" store |> ok).GetAt None (intKey 1)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(
            None,
            (RowStore.ensure "t:b" store |> ok).GetAt None (intKey 2)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(0, unresolvedOf dir)
        RowStore.close store |> ok)

[<Fact>]
let ``RowStore: ジャーナルが大きく未解決が無ければ間引かれる`` () =
    withDir "rs-trim" (fun dir ->
        let text = String.replicate 200 "x"

        let batch (ks: string) (baseKey: int) (count: int) =
            ks,
            [|
                for i in 0 .. count - 1 ->
                    Entry.put (intKey (baseKey + i)) (uint64 (baseKey + i + 1)) [| SqlValue.Text text |]
            |]

        let store = RowStore.openStore dir |> ok
        // 1 回目は閾値（256 KiB）未満なので間引かれない。
        let store2 =
            RowStore.writeBatches [ batch "t:a" 0 100; batch "t:b" 10000 100 ] store |> ok

        let len1 = FileInfo(journalOf dir).Length
        Assert.True(len1 > 0L, "ジャーナルが空です")
        Assert.True(len1 < 262144L, "1 回目で既に間引かれています")

        // 2 回目で閾値を超え、解決後に間引かれる。
        let store3 =
            RowStore.writeBatches [ batch "t:c" 20000 1200; batch "t:d" 30000 1200 ] store2
            |> ok

        let len2 = FileInfo(journalOf dir).Length
        Assert.True(len2 < len1, sprintf "間引かれていません（%d -> %d）" len1 len2)
        Assert.Equal(0, unresolvedOf dir)

        RowStore.close store3 |> ok
        let reopened = RowStore.openStoreTakingOver dir |> ok

        // 間引いてもデータは失われない
        Assert.True(
            (RowStore.ensure "t:c" reopened |> ok).GetAt None (intKey 20000)
            |> ok
            |> Option.isSome,
            "間引いた結果、t:c の行が消えています"
        )

        RowStore.close reopened |> ok)

[<Fact>]
let ``RowStore: 未解決の intent が残っている間はジャーナルを間引かない`` () =
    withDir "rs-notrim" (fun dir ->
        let text = String.replicate 200 "x"

        let batch (ks: string) (baseKey: int) (count: int) =
            ks,
            [|
                for i in 0 .. count - 1 ->
                    Entry.put (intKey (baseKey + i)) (uint64 (baseKey + i + 1)) [| SqlValue.Text text |]
            |]

        let store = RowStore.openStore dir |> ok

        // 閾値を超える未解決の公開を残す（適用せずにクラッシュした状態）。
        CommitIntent.publish dir [ batch "t:x" 0 1200; batch "t:y" 10000 1200 ]
        |> ok
        |> ignore

        let lenBig = FileInfo(journalOf dir).Length
        Assert.True(lenBig >= 262144L, "未解決の公開が閾値未満です")

        // 別の commit を完了させても、未解決が残るので間引かれない。
        let store2 =
            RowStore.writeBatches
                [
                    "t:p", [| Entry.put (intKey 1) 1UL [| SqlValue.Int 1L |] |]
                    "t:q", [| Entry.put (intKey 2) 2UL [| SqlValue.Int 2L |] |]
                ]
                store
            |> ok

        let lenAfter = FileInfo(journalOf dir).Length
        Assert.True(lenAfter >= lenBig, sprintf "未解決の公開が消されています（%d -> %d）" lenBig lenAfter)
        Assert.Equal(1, unresolvedOf dir)

        RowStore.close store2 |> ok)
