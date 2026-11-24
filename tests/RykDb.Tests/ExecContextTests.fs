// ===========================================================================
// ExecContext.fs（実行時文脈）
// ===========================================================================

module RykDb.ExecContextTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``ExecContext.create: 空の Schemas と同一の Store`` () =
    withStore "ec-create" (fun store ->
        let ctx = ExecContext.create store None
        Assert.True(Map.isEmpty ctx.Schemas)
        Assert.Same(store, ctx.Store))

[<Fact>]
let ``ExecContext.withSchema: スキーマを登録する`` () =
    withStore "ec-withschema" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "people" plainPeopleSchema c0
        Assert.True(Map.containsKey "people" c1.Schemas))

[<Fact>]
let ``ExecContext.withCatalog: Catalog を登録する`` () =
    withStore "ec-withcatalog" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withCatalog Catalog.Empty c0
        Assert.Equal(Catalog.Empty, c1.Catalog))

[<Fact>]
let ``ExecContext.schemaOf: 登録済みスキーマを引く`` () =
    withStore "ec-schemaof" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "people" plainPeopleSchema c0
        let s = ok (ExecContext.schemaOf "people" c1)
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.schemaOf: 大文字小文字違いのキーでも引ける`` () =
    withStore "ec-schemaof-alias" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "People" plainPeopleSchema c0
        // Map は Ordinal なので完全一致しないが、eqName で一致するキーへ落ちる
        let s = ok (ExecContext.schemaOf "people" c1)
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.schemaOf: カタログのテーブルへフォールバックする`` () =
    withStore "ec-schemaof-cat" (fun store ->
        let cat = Catalog.addTable (Catalog.toDefinition plainPeopleSchema) Catalog.Empty
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withCatalog cat c0
        // Schemas に無くても Catalog から引ける
        let s = ok (ExecContext.schemaOf "PEOPLE" c1)
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.schemaOf: 未登録は NameError`` () =
    withStore "ec-schemaof-miss" (fun store ->
        let c0 = ExecContext.create store None

        match ExecContext.schemaOf "nope" c0 with
        | Error e -> Assert.Equal(ErrorKind.NameError, e.Kind)
        | Ok _ -> Assert.Fail "NameError になるべき")
