// ===========================================================================
// Catalog.fs のテスト（スキーマ永続化）
// ===========================================================================

module RykDb.CatalogTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テスト用のテーブルスキーマ（id が主キー、email に UNIQUE 索引）。
let private usersSchema =
    TableSchema.ofCreateTable
        {
            Name = "users"
            IfNotExists = false
            Columns =
                [
                    {
                        Name = "id"
                        DataType = DataType.Integer
                        TypeArguments = []
                        Constraint =
                            { ColumnConstraint.Empty with
                                PrimaryKey = true
                            }
                    }
                    {
                        Name = "email"
                        DataType = DataType.Text
                        TypeArguments = []
                        Constraint = ColumnConstraint.Empty
                    }
                    {
                        Name = "age"
                        DataType = DataType.Integer
                        TypeArguments = []
                        Constraint = ColumnConstraint.Empty
                    }
                ]
            Constraints = []
        }
    |> ok

[<Fact>]
let ``TableSchema: スキーマを AST から作れる`` () =
    Assert.Equal("users", usersSchema.Name)
    Assert.Equal<int list>([ 0 ], usersSchema.PrimaryKey)
    Assert.Equal<string array>([| "id" |], TableSchema.primaryKeyColumns usersSchema)
    Assert.Equal<int option>(Some 1, TableSchema.indexOf usersSchema "email")
    Assert.Equal<int option>(None, TableSchema.indexOf usersSchema "nope")

[<Fact>]
let ``TableSchema.ofCreateTable: 主キー無し・列無しのテーブルはエラー`` () =
    let noCols =
        {
            Name = "t"
            IfNotExists = false
            Columns = []
            Constraints = []
        }

    expectErrorKind ErrorKind.ConstraintViolation "列無しのテーブルを許してはいけない" (TableSchema.ofCreateTable noCols)

    let noPk =
        {
            Name = "t"
            IfNotExists = false
            Columns =
                [
                    {
                        Name = "a"
                        DataType = DataType.Integer
                        TypeArguments = []
                        Constraint = ColumnConstraint.Empty
                    }
                ]
            Constraints = []
        }

    expectErrorKind ErrorKind.ConstraintViolation "主キー無しのテーブルを許してはいけない" (TableSchema.ofCreateTable noPk)

[<Fact>]
let ``TableSchema.ofCreateTable: テーブル制約の主キーを使う`` () =
    let ast =
        {
            Name = "composite"
            IfNotExists = false
            Columns = [ col "a" DataType.Integer; col "b" DataType.Integer ]
            Constraints = [ TableConstraint.PrimaryKeyColumns [ "b"; "a" ] ]
        }

    let schema = ok (TableSchema.ofCreateTable ast)
    // 列位置に直して昇順に揃う
    Assert.Equal<int list>([ 0; 1 ], schema.PrimaryKey)

    // 存在しない主キー列はエラー
    let bad =
        { ast with
            Constraints = [ TableConstraint.PrimaryKeyColumns [ "nope" ] ]
        }

    expectErrorKind ErrorKind.NameError "存在しない主キー列で作れてしまう" (TableSchema.ofCreateTable bad)

[<Fact>]
let ``TableSchema.ofCreateTable: テーブルレベル PK は NOT NULL になる`` () =
    let ast =
        {
            Name = "t"
            IfNotExists = false
            Columns = [ col "id" DataType.Integer; col "v" DataType.Text ]
            Constraints = [ TableConstraint.PrimaryKeyColumns [ "id" ] ]
        }

    let schema = ok (TableSchema.ofCreateTable ast)
    Assert.True(schema.Columns.[0].Constraint.NotNull)
    Assert.True(schema.Columns.[0].Constraint.PrimaryKey)
    Assert.False(schema.Columns.[1].Constraint.NotNull)

    expectErrorKind
        ErrorKind.ConstraintViolation
        "テーブルレベル PK の NULL を許してはいけない"
        (TableSchema.validateRow schema [| SqlValue.Null; SqlValue.Text "x" |])

[<Fact>]
let ``TableSchema.ofCreateTable: 重複列はエラー`` () =
    let ast =
        {
            Name = "t"
            IfNotExists = false
            Columns = [ col "a" DataType.Integer; col "A" DataType.Integer ]
            Constraints = [ TableConstraint.PrimaryKeyColumns [ "a" ] ]
        }

    expectErrorKind ErrorKind.NameError "重複列を許してはいけない" (TableSchema.ofCreateTable ast)

[<Fact>]
let ``TableSchema.ofCreateTable: 複数の PRIMARY KEY 定義はエラー`` () =
    let ast =
        {
            Name = "t"
            IfNotExists = false
            Columns = [ col "a" DataType.Integer; col "b" DataType.Integer ]
            Constraints =
                [
                    TableConstraint.PrimaryKeyColumns [ "a" ]
                    TableConstraint.PrimaryKeyColumns [ "b" ]
                ]
        }

    expectErrorKind ErrorKind.ConstraintViolation "複数の PRIMARY KEY を許してはいけない" (TableSchema.ofCreateTable ast)

    let pkColumn name =
        { col name DataType.Integer with
            Constraint =
                { ColumnConstraint.Empty with
                    PrimaryKey = true
                }
        }

    let columnPks =
        { ast with
            Constraints = []
            Columns = [ pkColumn "a"; pkColumn "b" ]
        }

    expectErrorKind ErrorKind.ConstraintViolation "複数列の列レベル PRIMARY KEY を許してはいけない" (TableSchema.ofCreateTable columnPks)

[<Fact>]
let ``TableSchema.ofCreateTable: 存在しない UNIQUE 列はエラー`` () =
    let ast =
        {
            Name = "t"
            IfNotExists = false
            Columns = [ col "id" DataType.Integer ]
            Constraints =
                [
                    TableConstraint.PrimaryKeyColumns [ "id" ]
                    TableConstraint.UniqueColumns [ "nope" ]
                ]
        }

    expectErrorKind ErrorKind.NameError "存在しない UNIQUE 列を許してはいけない" (TableSchema.ofCreateTable ast)

[<Fact>]
let ``TableSchema.requireIndexOf: 無い列はエラー`` () =
    Assert.Equal<int>(0, ok (TableSchema.requireIndexOf peopleSchema "id"))
    expectErrorKind ErrorKind.NameError "無い列で位置が返った" (TableSchema.requireIndexOf peopleSchema "nope")

[<Fact>]
let ``TableSchema.validateRow: 列数不一致はエラー`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L |]

    match TableSchema.validateRow schema row with
    | Error _ -> ()
    | Ok _ -> Assert.Fail "列数不一致はエラーになるべき"

[<Fact>]
let ``TableSchema.validateRow: NOT NULL 違反はエラー`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Null; SqlValue.Text "alice"; SqlValue.Int 20L |]

    match TableSchema.validateRow schema row with
    | Error _ -> ()
    | Ok _ -> Assert.Fail "NOT NULL 違反はエラーになるべき"

[<Fact>]
let ``TableSchema.validateRow: 正常行は Ok`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]

    match TableSchema.validateRow schema row with
    | Ok _ -> ()
    | Error e -> Assert.Fail(DbError.toDisplayString e)

[<Fact>]
let ``TableSchema.primaryKeyOf: 主キー列を取り出す`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]
    let pk = TableSchema.primaryKeyOf schema row
    Assert.Equal<int>(1, pk.Length)
    Assert.Equal(SqlValue.Int 7L, pk.[0])

[<Fact>]
let ``Catalog.addTable: 空のカタログにテーブルを追加・検索・削除`` () =
    let defn = Catalog.toDefinition peopleSchema
    let c1 = Catalog.addTable Catalog.Empty defn
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

        let c = ix |> Catalog.addIndex Catalog.Empty
        let store = ok (Catalog.save store Catalog.Empty c)
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
            Catalog.Empty
            |> (fun c -> Catalog.addTable c people)
            |> fun c -> Catalog.addTable c orders

        let store = ok (Catalog.save store Catalog.Empty c1)
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
        Catalog.Empty
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
            people |> Catalog.addTable Catalog.Empty |> fun c -> ix |> Catalog.addIndex c

        let store = ok (Catalog.save store Catalog.Empty c)
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
            ok (Catalog.save store Catalog.Empty (Catalog.addTable Catalog.Empty defn))

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
        let c1 = Catalog.addTable Catalog.Empty people
        let store = ok (Catalog.save store Catalog.Empty c1)
        let loaded = ok (Catalog.load store)
        Assert.Single loaded.Tables |> ignore

        // 消えた定義は「保存前と保存後の差分」から tombstone にして落とす
        // （カタログ keyspace を全走査しない）。
        let store = ok (Catalog.save store c1 Catalog.Empty)
        let loaded = ok (Catalog.load store)
        Assert.Empty loaded.Tables)

[<Fact>]
let ``Catalog.load: 未知のキーや不正な値は読み飛ばす`` () =
    withStore "cat-junk" (fun store ->
        let utf8 (s: string) = System.Text.Encoding.UTF8.GetBytes s

        // 有効なテーブルを保存しておき、その値を TEXT として持ち直して読ませる
        let people = Catalog.toDefinition peopleSchema

        let store =
            ok (Catalog.save store Catalog.Empty (Catalog.addTable Catalog.Empty people))

        let raw = ok (RowStore.scan Keyspace.CatalogKeyspace [||] 0 store) |> Seq.toArray

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

        let store = ok (RowStore.writeBatches [ (Keyspace.CatalogKeyspace, entries) ] store)
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
                    [
                        (Keyspace.CatalogKeyspace, [| Entry.put (utf8 "table:broken") 1UL [| SqlValue.Blob [||] |] |])
                    ]
                    store
            )

        expectError "壊れたテーブル定義を読めてしまう" (Catalog.load store)

        // 索引定義も同様
        let store =
            ok (
                RowStore.writeBatches
                    [
                        (Keyspace.CatalogKeyspace, [| Entry.put (utf8 "index:broken") 2UL [| SqlValue.Blob [||] |] |])
                    ]
                    store
            )

        expectError "壊れた索引定義を読めてしまう" (Catalog.load store))

[<Fact>]
let ``Schema.concat: 列とテーブルを連結する`` () =
    let s1 = outSchema [ outCol "a" (Some "t") (Some DataType.Integer) ]
    let s2 = outSchema [ outCol "b" (Some "u") (Some DataType.Text) ]
    let s1' = { s1 with Tables = [ "t" ] }
    let s2' = { s2 with Tables = [ "u" ] }
    let c = Schema.concat s1' s2'
    Assert.Equal(2, c.Columns.Length)
    Assert.Equal<string list>([ "t"; "u" ], c.Tables)

[<Fact>]
let ``Schema.concatAll: 空リストは empty`` () =
    let c = Schema.concatAll []
    Assert.Equal(0, c.Columns.Length)
    Assert.True(List.isEmpty c.Tables)

[<Fact>]
let ``Schema.expandStar: * は全列`` () =
    let s =
        outSchema
            [
                outCol "id" (Some "t") (Some DataType.Integer)
                outCol "name" (Some "t") (Some DataType.Text)
            ]

    match Schema.expandStar s Expr.Star with
    | Some(false, cols) -> Assert.Equal(2, cols.Length)
    | other -> Assert.Fail(sprintf "expandStar * が想定外: %A" other)

[<Fact>]
let ``Schema.expandStar: t.* は修飾一致のみ`` () =
    let s =
        outSchema
            [
                outCol "id" (Some "t") (Some DataType.Integer)
                outCol "id" (Some "u") (Some DataType.Integer)
            ]

    match Schema.expandStar s (Expr.QualifiedStar "t") with
    | Some(true, cols) -> Assert.Equal(1, cols.Length)
    | other -> Assert.Fail(sprintf "expandStar t.* が想定外: %A" other)

[<Fact>]
let ``Schema.projectHeader: * は入力列名`` () =
    let s = outSchema [ outCol "id" (Some "t") (Some DataType.Integer) ]
    let h = Schema.projectHeader s [ { Expression = Expr.Star; Alias = None } ]
    Assert.Equal<string array>([| "id" |], h)

[<Fact>]
let ``Schema.projectHeader: 空なら入力ヘッダ / t.* は修飾つき`` () =
    let s = outSchema [ outCol "id" (Some "t") (Some DataType.Integer) ]
    // 射影が空なら入力スキーマのヘッダをそのまま返す
    Assert.Equal<string array>([| "t.id" |], Schema.projectHeader s [])

    // t.* は修飾つきの表示名で出す
    let h =
        Schema.projectHeader
            s
            [
                {
                    Expression = Expr.QualifiedStar "t"
                    Alias = None
                }
            ]

    Assert.Equal<string array>([| "t.id" |], h)

[<Fact>]
let ``Schema.tryAsColumnRef: Column / Name を列参照に`` () =
    match Schema.tryAsColumnRef (Expr.Column { Qualifier = Some "t"; Name = "id" }) with
    | Some c -> Assert.Equal("id", c.Name)
    | None -> Assert.Fail "Column は Some になるべき"

    match Schema.tryAsColumnRef (Expr.Name "x") with
    | Some c -> Assert.Equal("x", c.Name)
    | None -> Assert.Fail "Name は Some になるべき"

    Assert.True((Schema.tryAsColumnRef (Expr.Literal(Literal.Int 1L))).IsNone)

[<Fact>]
let ``Schema.ofTable: テーブルの全列を並べる`` () =
    let ts = peopleSchema
    let s = Schema.ofTable "people" None ts
    Assert.Equal(ts.Columns.Length, s.Columns.Length)
    Assert.Equal<string list>([ "people" ], s.Tables)

[<Fact>]
let ``Schema.resolve: 空スキーマのエラーは (なし) を出す`` () =
    match Schema.resolve (outSchema []) { Qualifier = None; Name = "x" } with
    | Error e -> Assert.Contains("(なし)", e.Message)
    | Ok _ -> Assert.Fail "NameError になるべき"

[<Fact>]
let ``Schema.resolve: 修飾子の無い列名が曖昧ならエラー`` () =
    let schema = outSchema [ outCol "id" (Some "a") None; outCol "id" (Some "b") None ]
    expectError "曖昧な列名が解決できてしまいました" (Schema.resolve schema { Qualifier = None; Name = "id" })

    // 修飾子付きなら解決できる
    Assert.Equal(1, ok (Schema.resolve schema { Qualifier = Some "b"; Name = "id" }))
