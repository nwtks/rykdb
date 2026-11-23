// ===========================================================================
// Catalog.fs のテスト（スキーマ永続化）
// ===========================================================================

module RykDb.CatalogTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``Catalog.addTable / findTable / removeTable: 空のカタログにテーブルを追加・検索・削除`` () =
    let defn = Catalog.toDefinition peopleSchema
    let c1 = Catalog.addTable defn Catalog.Empty
    Assert.Single c1.Tables |> ignore
    Assert.True(Catalog.findTable "people" c1 |> Option.isSome)
    // SQL の識別子は大文字小文字を区別しない
    Assert.True(Catalog.findTable "PEOPLE" c1 |> Option.isSome)

    let c2 = Catalog.removeTable "people" c1
    Assert.Empty c2.Tables

[<Fact>]
let ``Catalog.removeTable: 削除した定義は再読込時に消える`` () =
    withStore "cat2" (fun store ->
        let people = Catalog.toDefinition peopleSchema
        let orders = Catalog.toDefinition ordersSchema
        let c1 = Catalog.Empty |> Catalog.addTable people |> Catalog.addTable orders
        let store = ok (Catalog.save store Catalog.Empty c1)

        // people を消して保存し直す（消えた定義は tombstone で落ちる）
        let c2 = Catalog.removeTable "people" c1
        let store = ok (Catalog.save store c1 c2)
        let loaded = ok (Catalog.load store)
        Assert.Single loaded.Tables |> ignore
        Assert.Equal<string>("orders", loaded.Tables.[0].Name)
        Assert.True(Catalog.findTable "people" loaded |> Option.isNone))

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

        let c = Catalog.addIndex ix Catalog.Empty
        let store = ok (Catalog.save store Catalog.Empty c)
        let loaded = ok (Catalog.load store)
        Assert.Empty loaded.Tables
        Assert.Single loaded.Indexes |> ignore
        Assert.Equal<string list>([ "a"; "b" ], loaded.Indexes.[0].Columns)
        Assert.True(loaded.Indexes.[0].Unique))

[<Fact>]
let ``Catalog.save / load: 統計を往復し、removeTable で消える`` () =
    withStore "cat-stats" (fun store ->
        let stats =
            { tableStats
                  "people"
                  3L
                  [
                      colStats "id" 3L 0L None None
                      { colStats "name" 2L 1L (Some 1.0) (Some 3.0) with
                          Mcv = [ ("'alice'", 2L) ]
                      }
                  ]
                  [ idxStats "ix_name" [ "name" ] [ 2L ] ] with
                Changes = 4L
                RowBytes = 40.0
            }

        let c = Catalog.addStats stats Catalog.Empty
        let store = ok (Catalog.save store Catalog.Empty c)
        let loaded = ok (Catalog.load store)
        let s = expectSome "people の統計" (Catalog.findStats "PEOPLE" loaded)
        Assert.Equal<int64>(3L, s.RowCount)
        let nameColumn = s.Columns |> List.find (fun (c: ColumnStats) -> c.Name = "name")
        Assert.Equal<int64>(2L, nameColumn.NDV)
        Assert.Equal<int64>(1L, nameColumn.Nulls)
        Assert.Equal<float option>(Some 1.0, nameColumn.Min)
        Assert.Equal<float option>(Some 3.0, nameColumn.Max)
        Assert.Equal<int>(1, s.Indexes.Length)
        Assert.Equal<string list>([ "name" ], s.Indexes.[0].Columns)
        Assert.Equal<int64 list>([ 2L ], s.Indexes.[0].PrefixNDV)
        Assert.Equal<(string * int64) list>([ ("'alice'", 2L) ], nameColumn.Mcv)
        Assert.True(s.AnalyzedAt.IsNone)
        Assert.Equal<int64>(4L, s.Changes)
        Assert.Equal(40.0, s.RowBytes, 3)

        Assert.True(
            Catalog.findStats "people" (Catalog.removeTable "people" loaded)
            |> Option.isNone
        ))

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
        |> Catalog.addIndex ixA
        |> Catalog.addIndex ixB
        |> Catalog.removeIndex "IX_A" // 大文字小文字は無視

    Assert.True(Catalog.findIndex "ix_a" c |> Option.isNone)
    Assert.True(Catalog.findIndex "ix_b" c |> Option.isSome)

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
            ForeignKeys = []
        }

    expectError "存在しない主キー列で復元できました" (Catalog.toSchema bad)

[<Fact>]
let ``Catalog.save / load: 保存したカタログを読み込める`` () =
    withStore "cat1" (fun store ->
        let people = Catalog.toDefinition peopleSchema
        let ix = indexDefinition "people" peopleSchema.Indexes.[0]
        let c = Catalog.addTable people Catalog.Empty |> fun c -> Catalog.addIndex ix c
        let store = ok (Catalog.save store Catalog.Empty c)
        let loaded = ok (Catalog.load store)
        Assert.Single loaded.Tables |> ignore
        Assert.Single loaded.Indexes |> ignore
        Assert.Equal<string>("people", loaded.Tables.[0].Name)
        Assert.Equal<string>("ix_people_age", loaded.Indexes.[0].IndexName)
        Assert.Equal<string>("people", loaded.Indexes.[0].TableName))

[<Fact>]
let ``Catalog.save / load: DEFAULT 式は保存・再読込しても復元される`` () =
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
                ForeignKeys = []
            }

        let store =
            ok (Catalog.save store Catalog.Empty (Catalog.addTable defn Catalog.Empty))

        let loaded = ok (Catalog.load store)

        match Catalog.findTable "t" loaded with
        | None -> Assert.Fail "テーブルが読み込まれていません"
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
        let c1 = Catalog.addTable people Catalog.Empty
        let store = ok (Catalog.save store Catalog.Empty c1)
        let loaded = ok (Catalog.load store)
        Assert.Single loaded.Tables |> ignore

        // 消えた定義は「保存前と保存後の差分」から tombstone にして落とす
        // （カタログ keyspace を全走査しない）。
        let store = ok (Catalog.save store c1 Catalog.Empty)
        let loaded = ok (Catalog.load store)
        Assert.Empty loaded.Tables)

[<Fact>]
let ``Catalog.save: 変わっていない定義は書き直さない`` () =
    withStore "cat-diff" (fun store ->
        let people = Catalog.toDefinition peopleSchema
        let orders = Catalog.toDefinition ordersSchema

        let seqOf (name: string) (store: RowStore) =
            let key = System.Text.Encoding.UTF8.GetBytes("table:" + name)

            RowStore.scan Keyspace.CatalogKeyspace [||] 0 store
            |> ok
            |> SstScan.toArray
            |> ok
            |> Array.tryPick (fun e -> if e.Key = key then Some e.SeqNo else None)

        let c1 = Catalog.Empty |> Catalog.addTable people
        let store = ok (Catalog.save store Catalog.Empty c1)
        let peopleSeq = expectSome "people の定義がありません" (seqOf "people" store)

        // 別のテーブルを足しても、変わっていない people は書き直さない（SeqNo が進まない）。
        let c2 = Catalog.addTable orders c1
        let store = ok (Catalog.save store c1 c2)
        Assert.Equal<uint64>(peopleSeq, expectSome "people の定義がありません" (seqOf "people" store))
        Assert.True((seqOf "orders" store).IsSome)

        // 定義が変われば書き直す。
        let renamed =
            { people with
                Columns =
                    people.Columns
                    |> List.map (fun c -> if c.Name = "age" then { c with Name = "years" } else c)
            }

        let c3 = Catalog.addTable renamed c2
        let store = ok (Catalog.save store c2 c3)
        Assert.NotEqual<uint64>(peopleSeq, expectSome "people の定義がありません" (seqOf "people" store)))

[<Fact>]
let ``Catalog.load: 未知のキーや不正な値は読み飛ばす`` () =
    withStore "cat-junk" (fun store ->
        let utf8 (s: string) = System.Text.Encoding.UTF8.GetBytes s

        // 有効なテーブルを保存しておき、その値を TEXT として持ち直して読ませる
        let people = Catalog.toDefinition peopleSchema

        let store =
            ok (Catalog.save store Catalog.Empty (Catalog.addTable people Catalog.Empty))

        let raw =
            ok (RowStore.scan Keyspace.CatalogKeyspace [||] 0 store)
            |> SstScan.toArray
            |> ok

        let value =
            match raw.[0].Value with
            | Some v -> v
            | None -> failwith "カタログの値がありません"

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
        Assert.True(Catalog.findTable "people2" loaded |> Option.isSome)
        Assert.False(Catalog.findTable "weird" loaded |> Option.isSome))

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

    match Schema.expandStar Expr.Star s with
    | Some(false, cols) -> Assert.Equal(2, cols.Length)
    | other -> failActual "expandStar * が想定外" other

[<Fact>]
let ``Schema.expandStar: t.* は修飾一致のみ`` () =
    let s =
        outSchema
            [
                outCol "id" (Some "t") (Some DataType.Integer)
                outCol "id" (Some "u") (Some DataType.Integer)
            ]

    match Schema.expandStar (Expr.QualifiedStar "t") s with
    | Some(true, cols) -> Assert.Equal(1, cols.Length)
    | other -> failActual "expandStar t.* が想定外" other

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
    match Schema.resolve { Qualifier = None; Name = "x" } (outSchema []) with
    | Error e -> Assert.Contains("(なし)", e.Message)
    | Ok _ -> Assert.Fail "NameError になるべき"

[<Fact>]
let ``Schema.resolve: 修飾子の無い列名が曖昧ならエラー`` () =
    let schema = outSchema [ outCol "id" (Some "a") None; outCol "id" (Some "b") None ]
    expectError "曖昧な列名が解決できてしまいました" (Schema.resolve { Qualifier = None; Name = "id" } schema)

    // 修飾子付きなら解決できる
    Assert.Equal(1, ok (Schema.resolve { Qualifier = Some "b"; Name = "id" } schema))

[<Fact>]
let ``SelectProjection: ヘッダと式の列数が一致する`` () =
    let items =
        [
            { Expression = Expr.Star; Alias = None }
            {
                Expression = Expr.Column { Qualifier = None; Name = "a" }
                Alias = Some "x"
            }
            {
                Expression =
                    Expr.Binary(
                        BinaryOp.Add,
                        Expr.Column { Qualifier = None; Name = "a" },
                        Expr.Column { Qualifier = None; Name = "b" }
                    )
                Alias = None
            }
        ]

    let projection = SelectProjection.create schema2 items

    // * は 2 列、別名つき列が 1 列、式が 1 列。
    Assert.Equal<int>(4, projection.Expressions.Length)
    Assert.Equal<int>(4, projection.DisplayHeader.Length)
    Assert.Equal<int>(4, projection.ReferenceHeader.Length)

[<Fact>]
let ``SelectProjection: 項目が空なら入力スキーマをそのまま使う`` () =
    let projection = SelectProjection.create schema2 []
    Assert.Empty projection.Expressions
    Assert.Equal<int>(2, projection.DisplayHeader.Length)
    Assert.Equal<int>(2, projection.ReferenceHeader.Length)
