// ===========================================================================
// ExecContext.fs（実行時文脈）
// ===========================================================================

module RykDb.ExecContextTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private peopleSchema: TableSchema =
    {
        Name = "people"
        Columns = [ TestHelpers.col "id" DataType.Integer; TestHelpers.col "name" DataType.Text ]
        PrimaryKey = [ 0 ]
        Indexes = []
    }

[<Fact>]
let ``ExecContext.create: 空の Schemas / Catalog`` () =
    withStore "ec-create" (fun store ->
        let ctx = ExecContext.create store (Snapshot.noLimit)
        Assert.True(Map.isEmpty ctx.Schemas)
        Assert.Equal(store, ctx.Store))

[<Fact>]
let ``ExecContext.withSchema: スキーマを登録する`` () =
    withStore "ec-withschema" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withSchema c0 "people" peopleSchema
        Assert.True(Map.containsKey "people" c1.Schemas))

[<Fact>]
let ``ExecContext.withCatalog: Catalog を登録する`` () =
    withStore "ec-withcatalog" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withCatalog c0 Catalog.empty
        Assert.Equal(Catalog.empty, c1.Catalog))

[<Fact>]
let ``ExecContext.schemaOf: 登録済みスキーマを引く`` () =
    withStore "ec-schemaof" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withSchema c0 "people" peopleSchema
        let s = ok (ExecContext.schemaOf c1 "people")
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.schemaOf: 未登録は NameError`` () =
    withStore "ec-schemaof-miss" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)

        match ExecContext.schemaOf c0 "nope" with
        | Error e -> Assert.Equal(ErrorKind.NameError, e.Kind)
        | Ok _ -> Assert.Fail "NameError になるべき")

[<Fact>]
let ``ExecContext.schemaOf: 大文字小文字違いのキーでも引ける`` () =
    withStore "ec-schemaof-alias" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withSchema c0 "People" peopleSchema

        // Map は Ordinal なので完全一致しないが、eqName で一致するキーへ落ちる
        let s = ok (ExecContext.schemaOf c1 "people")
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.schemaOf: カタログのテーブルへフォールバックする`` () =
    withStore "ec-schemaof-cat" (fun store ->
        let cat = Catalog.addTable Catalog.empty (Catalog.toDefinition peopleSchema)
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withCatalog c0 cat

        // Schemas に無くても Catalog から引ける
        let s = ok (ExecContext.schemaOf c1 "PEOPLE")
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.table: 登録スキーマで全走査`` () =
    withStore "ec-table" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withSchema c0 "people" peopleSchema
        let op = ok (ExecContext.table c1 "people")
        let rows = ok (Op.toArray op)
        Assert.Equal(0, rows.Length))

[<Fact>]
let ``ExecContext.byIndex: 未登録テーブルは NameError`` () =
    withStore "ec-byindex-miss-table" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)

        match ExecContext.byIndex c0 "nope" "ix" [| SqlValue.Int 1L |] with
        | Error e -> Assert.Equal(ErrorKind.NameError, e.Kind)
        | Ok _ -> Assert.Fail "NameError になるべき")

[<Fact>]
let ``ExecContext.byIndex: 未登録索引は NameError`` () =
    withStore "ec-byindex-miss-idx" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withSchema c0 "people" peopleSchema

        match ExecContext.byIndex c1 "people" "nope" [| SqlValue.Int 1L |] with
        | Error e -> Assert.Equal(ErrorKind.NameError, e.Kind)
        | Ok _ -> Assert.Fail "NameError になるべき")

[<Fact>]
let ``ExecContext.byIndex: 空の store は空の Op`` () =
    withStore "ec-byindex-empty" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withSchema c0 "people" TestHelpers.peopleSchema
        let op = ok (ExecContext.byIndex c1 "people" "ix_people_age" [| SqlValue.Int 1L |])
        let rows = ok (Op.toArray op)
        Assert.Equal(0, rows.Length))

[<Fact>]
let ``ExecContext.byPrimaryKey: 存在しない行は empty`` () =
    withStore "ec-bypk" (fun store ->
        let c0 = ExecContext.create store (Snapshot.noLimit)
        let c1 = ExecContext.withSchema c0 "people" peopleSchema
        let op = ok (ExecContext.byPrimaryKey c1 "people" [| SqlValue.Int 99L |])
        let rows = ok (Op.toArray op)
        Assert.Equal(0, rows.Length))
