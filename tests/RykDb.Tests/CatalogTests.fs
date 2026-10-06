// ===========================================================================
// Catalog.fs のテスト（スキーマ永続化）
// ===========================================================================

module RykDb.CatalogTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

// ---------------------------------------------------------------------------
// テスト用のスキーマ
// ---------------------------------------------------------------------------

let private col (name: string) (dt: DataType) =
    {
        Name = name
        DataType = dt
        TypeArguments = []
        Constraint =
            {
                NotNull = false
                PrimaryKey = false
                Unique = false
                Default = None
            }
    }

let private pkCol (name: string) (dt: DataType) =
    { col name dt with
        Constraint =
            {
                NotNull = true
                PrimaryKey = true
                Unique = false
                Default = None
            }
    }

/// テスト用のテーブルスキーマ。
let private peopleSchema: TableSchema =
    {
        Name = "people"
        Columns =
            [
                pkCol "id" DataType.Integer
                col "name" DataType.Text
                col "age" DataType.Integer
            ]
        PrimaryKey = [ 0 ]
        Indexes =
            [
                {
                    Name = "ix_people_age"
                    Columns = [ "age" ]
                    Unique = false
                }
            ]
    }

let private ordersSchema: TableSchema =
    {
        Name = "orders"
        Columns =
            [
                pkCol "oid" DataType.Integer
                col "person_id" DataType.Integer
                col "amount" DataType.Integer
            ]
        PrimaryKey = [ 0 ]
        Indexes =
            [
                {
                    Name = "ix_orders_person"
                    Columns = [ "person_id" ]
                    Unique = false
                }
            ]
    }

// ---------------------------------------------------------------------------
// Catalog — 定義の相互変換
// ---------------------------------------------------------------------------

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

    match Catalog.toSchema bad with
    | Error _ -> ()
    | Ok _ -> Assert.Fail("存在しない主キー列で復元できました")


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

// ---------------------------------------------------------------------------
// Catalog — 永続化
// ---------------------------------------------------------------------------

[<Fact>]
let ``Catalog: 保存したカタログを読み込める`` () =
    withDir "cat1" (fun dir ->
        let store = ok (RowStore.openStore dir)

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

[<Fact>]
let ``Catalog: 削除した定義は再読込時に消える`` () =
    withDir "cat2" (fun dir ->
        let store = ok (RowStore.openStore dir)

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
let ``Catalog: 索引だけを追加して読み込める`` () =
    withDir "cat3" (fun dir ->
        let store = ok (RowStore.openStore dir)

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
