// ===========================================================================
// Catalog.fs のテスト（スキーマ永続化）
// ===========================================================================

module RykDb.CatalogTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Catalog: 空のカタログにテーブルを追加・検索・削除`` () =
    let defn = Catalog.toDefinition peopleSchema
    let c1 = Catalog.addTable Catalog.empty defn

    Assert.Single(c1.Tables) |> ignore
    Assert.True(Catalog.findTable c1 "people" |> Option.isSome)

    // SQL の識別子は大文字小文字を区別しない
    Assert.True(Catalog.findTable c1 "PEOPLE" |> Option.isSome)

    let c2 = Catalog.removeTable c1 "people"
    Assert.Empty(c2.Tables)

[<Fact>]
let ``Catalog: 索引だけを追加して読み込める`` () =
    withStore "cat3" (fun store ->
        let ix =
            {
                IndexName = "ix_x"
                TableName = "t"
                Columns = [ "a"; "b" ]
                Unique = true
            }

        let c = ix |> Catalog.addIndex Catalog.empty
        let store = ok (Catalog.save store c)
        let loaded = ok (Catalog.load store)

        Assert.Empty(loaded.Tables)
        Assert.Single(loaded.Indexes) |> ignore
        Assert.Equal<string list>([ "a"; "b" ], loaded.Indexes.[0].Columns)
        Assert.True(loaded.Indexes.[0].Unique))

[<Fact>]
let ``Catalog: 削除した定義は再読込時に消える`` () =
    withStore "cat2" (fun store ->
        let people = Catalog.toDefinition peopleSchema
        let orders = Catalog.toDefinition ordersSchema

        let c1 =
            Catalog.empty
            |> (fun c -> Catalog.addTable c people)
            |> (fun c -> Catalog.addTable c orders)

        let store = ok (Catalog.save store c1)

        // people を消して保存し直す
        let c2 = Catalog.removeTable c1 "people"
        let store = ok (Catalog.save store c2)

        let loaded = ok (Catalog.load store)

        Assert.Single(loaded.Tables) |> ignore
        Assert.Equal<string>("orders", loaded.Tables.[0].Name)
        Assert.True(Catalog.findTable loaded "people" |> Option.isNone))

[<Fact>]
let ``Catalog: TableSchema を定義に変換して戻すと元に戻る`` () =
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
let ``Catalog: 主キー列が存在しない場合はエラー`` () =
    let bad =
        {
            Name = "broken"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKeyColumns = [ "nope" ]
        }

    expectError "存在しない主キー列で復元できました" (Catalog.toSchema bad)

[<Fact>]
let ``Catalog: 保存したカタログを読み込める`` () =
    withStore "cat1" (fun store ->
        let people = Catalog.toDefinition peopleSchema
        let ix = Catalog.toIndexDefinition "people" peopleSchema.Indexes.[0]

        let c =
            people |> Catalog.addTable Catalog.empty |> (fun c -> ix |> Catalog.addIndex c)

        let store = ok (Catalog.save store c)
        let loaded = ok (Catalog.load store)

        Assert.Single(loaded.Tables) |> ignore
        Assert.Single(loaded.Indexes) |> ignore
        Assert.Equal<string>("people", loaded.Tables.[0].Name)
        Assert.Equal<string>("ix_people_age", loaded.Indexes.[0].IndexName)
        Assert.Equal<string>("people", loaded.Indexes.[0].TableName))
