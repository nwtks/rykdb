// ===========================================================================
// Catalog.fs のテスト（スキーマ永続化）
// ===========================================================================

module RykDb.CatalogTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Catalog.addTable: 空のカタログにテーブルを追加・検索・削除`` () =
    let defn = Catalog.toDefinition peopleSchema
    let c1 = Catalog.addTable Catalog.empty defn
    Assert.Single c1.Tables |> ignore
    Assert.True(Catalog.findTable c1 "people" |> Option.isSome)
    // SQL の識別子は大文字小文字を区別しない
    Assert.True(Catalog.findTable c1 "PEOPLE" |> Option.isSome)

    let c2 = Catalog.removeTable c1 "people"
    Assert.Empty c2.Tables

[<Fact>]
let ``Catalog.addIndex: 索引だけを追加して読み込める`` () =
    withStore "cat3" (fun store ->
        let ix =
            {
                IndexName = "ix_x"
                TableName = "t"
                Columns = [ "a"; "b" ]
                Unique = true
            }

        let c = ix |> Catalog.addIndex Catalog.empty
        let store = ok (Catalog.save store Catalog.empty c)
        let loaded = ok (Catalog.load store)
        Assert.Empty loaded.Tables
        Assert.Single loaded.Indexes |> ignore
        Assert.Equal<string list>([ "a"; "b" ], loaded.Indexes.[0].Columns)
        Assert.True(loaded.Indexes.[0].Unique))

[<Fact>]
let ``Catalog.removeTable: 削除した定義は再読込時に消える`` () =
    withStore "cat2" (fun store ->
        let people = Catalog.toDefinition peopleSchema
        let orders = Catalog.toDefinition ordersSchema

        let c1 =
            Catalog.empty
            |> (fun c -> Catalog.addTable c people)
            |> fun c -> Catalog.addTable c orders

        let store = ok (Catalog.save store Catalog.empty c1)
        // people を消して保存し直す（消えた定義は tombstone で落ちる）
        let c2 = Catalog.removeTable c1 "people"
        let store = ok (Catalog.save store c1 c2)
        let loaded = ok (Catalog.load store)
        Assert.Single loaded.Tables |> ignore
        Assert.Equal<string>("orders", loaded.Tables.[0].Name)
        Assert.True(Catalog.findTable loaded "people" |> Option.isNone))

[<Fact>]
let ``Catalog.toSchema: TableSchema を定義に変換して戻すと元に戻る`` () =
    let defn = Catalog.toDefinition peopleSchema
    Assert.Equal<string>("people", defn.Name)
    Assert.Equal<int>(3, defn.Columns.Length)

    let back = ok (Catalog.toSchema defn)
    Assert.Equal<string>("people", back.Name)
    Assert.Equal<int>(3, back.Columns.Length)
    Assert.Equal<int>([| 0 |], back.PrimaryKey)
    // NOT NULL / PRIMARY KEY が保存されているか
    Assert.True(back.Columns.[0].Constraint.NotNull)
    Assert.True(back.Columns.[0].Constraint.PrimaryKey)

[<Fact>]
let ``Catalog.toSchema: 主キー列が存在しない場合はエラー`` () =
    let bad =
        {
            Name = "broken"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKeyColumns = [ "nope" ]
        }

    expectError "存在しない主キー列で復元できました" (Catalog.toSchema bad)

[<Fact>]
let ``Catalog.removeIndex: removeIndex は指定した索引だけ消す`` () =
    let ixA =
        {
            IndexName = "ix_a"
            TableName = "t"
            Columns = [ "a" ]
            Unique = false
        }

    let ixB =
        {
            IndexName = "ix_b"
            TableName = "t"
            Columns = [ "b" ]
            Unique = false
        }

    let c =
        Catalog.empty
        |> (fun c -> Catalog.addIndex c ixA)
        |> (fun c -> Catalog.addIndex c ixB)
        |> fun c -> Catalog.removeIndex c "IX_A" // 大文字小文字は無視

    Assert.True(Catalog.findIndex c "ix_a" |> Option.isNone)
    Assert.True(Catalog.findIndex c "ix_b" |> Option.isSome)

[<Fact>]
let ``Catalog: 保存したカタログを読み込める`` () =
    withStore "cat1" (fun store ->
        let people = Catalog.toDefinition peopleSchema
        let ix = Catalog.toIndexDefinition "people" peopleSchema.Indexes.[0]

        let c =
            people |> Catalog.addTable Catalog.empty |> fun c -> ix |> Catalog.addIndex c

        let store = ok (Catalog.save store Catalog.empty c)
        let loaded = ok (Catalog.load store)
        Assert.Single loaded.Tables |> ignore
        Assert.Single loaded.Indexes |> ignore
        Assert.Equal<string>("people", loaded.Tables.[0].Name)
        Assert.Equal<string>("ix_people_age", loaded.Indexes.[0].IndexName)
        Assert.Equal<string>("people", loaded.Indexes.[0].TableName))

[<Fact>]
let ``Catalog: DEFAULT 式は保存・再読込しても復元される`` () =
    withStore "cat-default" (fun store ->
        let defaultCol name dt value =
            { col name dt with
                Constraint =
                    { ColumnConstraint.Empty with
                        Default = Some value
                    }
            }

        let defn =
            {
                Name = "t"
                Columns =
                    [
                        pkCol "id" DataType.Integer
                        defaultCol "v" DataType.Integer (Expr.Literal(Literal.Int 42L))
                        defaultCol "s" DataType.Text (Expr.Literal(Literal.String "it's"))
                    ]
                PrimaryKeyColumns = [ "id" ]
            }

        let store =
            ok (Catalog.save store Catalog.empty (Catalog.addTable Catalog.empty defn))

        let loaded = ok (Catalog.load store)

        match Catalog.findTable loaded "t" with
        | None -> Assert.Fail("テーブルが読み込まれていません")
        | Some t ->
            let defaults =
                t.Columns
                |> List.map (fun c -> Option.map Expr.toDisplayString c.Constraint.Default)
                |> List.tail

            Assert.Equal<string option>(Some "42", defaults.[0])
            Assert.Equal<string option>(Some "'it''s'", defaults.[1]))

[<Fact>]
let ``Catalog.save: 消えた定義は差分から tombstone で落ちる`` () =
    withStore "cat-sweep" (fun store ->
        let people = Catalog.toDefinition peopleSchema

        // テーブルを保存する
        let c1 = Catalog.addTable Catalog.empty people
        let store = ok (Catalog.save store Catalog.empty c1)
        let loaded = ok (Catalog.load store)
        Assert.Single loaded.Tables |> ignore

        // 消えた定義は「保存前と保存後の差分」から tombstone にして落とす
        // （カタログ keyspace を全走査しない）。
        let store = ok (Catalog.save store c1 Catalog.empty)
        let loaded = ok (Catalog.load store)
        Assert.Empty loaded.Tables)

[<Fact>]
let ``Catalog.load: 未知のキーや不正な値は読み飛ばす`` () =
    withStore "cat-junk" (fun store ->
        let utf8 (s: string) = System.Text.Encoding.UTF8.GetBytes s

        // 有効なテーブルを保存しておき、その値を TEXT として持ち直して読ませる
        let people = Catalog.toDefinition peopleSchema

        let store =
            ok (Catalog.save store Catalog.empty (Catalog.addTable Catalog.empty people))

        let raw = ok (RowStore.scan store "__catalog__" [||] 0) |> Seq.toArray

        let value =
            match raw.[0] with
            | _, Some v -> v
            | _, None -> failwith "カタログの値がありません"

        // Latin1 は 1 バイト = 1 文字なので、ASCII の定義なら TEXT として往復できる
        let asText =
            match value with
            | [| SqlValue.Blob b |] -> System.Text.Encoding.Latin1.GetString b
            | _ -> failwith "BLOB ではありません"

        let entries =
            [|
                // 短すぎるキー（プレフィクスの判定まで届かない）
                Entry.put (utf8 "x") 100UL [| SqlValue.Blob [||] |]
                // 未知のプレフィクス
                Entry.put (utf8 "junk:y") 101UL [| SqlValue.Blob [||] |]
                // 1 列の BLOB / TEXT ではない値
                Entry.put (utf8 "table:weird") 102UL [| SqlValue.Blob [||]; SqlValue.Int 1L |]
                // 同じ内容を TEXT として持つ別テーブル
                Entry.put (utf8 "table:people2") 103UL [| SqlValue.Text asText |]
            |]

        let store = ok (RowStore.writeBatches store [ ("__catalog__", entries) ])
        let loaded = ok (Catalog.load store)
        Assert.Equal<int>(2, loaded.Tables.Length)
        Assert.True(Catalog.findTable loaded "people2" |> Option.isSome)
        Assert.False(Catalog.findTable loaded "weird" |> Option.isSome))

[<Fact>]
let ``Catalog.load: 壊れた定義はエラー`` () =
    withStore "cat-broken" (fun store ->
        let utf8 (s: string) = System.Text.Encoding.UTF8.GetBytes s

        let store =
            ok (
                RowStore.writeBatches
                    store
                    [
                        ("__catalog__", [| Entry.put (utf8 "table:broken") 1UL [| SqlValue.Blob [||] |] |])
                    ]
            )

        expectError "壊れたテーブル定義を読めてしまう" (Catalog.load store)

        // 索引定義も同様
        let store =
            ok (
                RowStore.writeBatches
                    store
                    [
                        ("__catalog__", [| Entry.put (utf8 "index:broken") 2UL [| SqlValue.Blob [||] |] |])
                    ]
            )

        expectError "壊れた索引定義を読めてしまう" (Catalog.load store))
