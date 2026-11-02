// ===========================================================================
// ExecContext.fs（実行時文脈）
// ===========================================================================

module RykDb.ExecContextTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テスト用のテーブルスキーマ（索引なし）。
/// `TestHelpers.peopleSchema` は索引付きで NOT NULL 検証が付くので、
/// 索引を触らない DML 経路を検証するこのファイルでは使わない（意図的な差）。
/// このファイルの `col` は式の列参照ビルダーなので、列定義は共有ヘルパーを明示的に指す。
let private peopleSchema: TableSchema =
    {
        Name = "people"
        Columns =
            [
                col "id" DataType.Integer
                col "name" DataType.Text
                col "age" DataType.Integer
            ]
        PrimaryKey = [ 0 ]
        Indexes = []
    }

let private ctxOf (store: RowStore) (txn: Transaction) =
    ExecContext.create store None |> ExecContext.withTransaction txn

/// commit して store へ流し込む。引き継いだ TransactionManager を返す。
let private commitInto (store: RowStore) (mgr: TransactionManager) (txn: Transaction) =
    let mgr2, _, batches = ok (TransactionManager.commit txn mgr)
    ok (RowStore.writeBatches batches store) |> ignore
    mgr2

/// alice(20) / bob(30) の 2 行を入れて commit する（UPDATE / DELETE の土台）。
let private seedPeople (store: RowStore) (mgr: TransactionManager) =
    let mgr, txn = TransactionManager.beginTxn mgr

    let txn, _ =
        ok (
            DmlOps.insert
                (ctxOf store txn)
                peopleSchema
                None
                [
                    [
                        Expr.Literal(Literal.Int 1L)
                        Expr.Literal(Literal.String "alice")
                        Expr.Literal(Literal.Int 20L)
                    ]
                    [
                        Expr.Literal(Literal.Int 2L)
                        Expr.Literal(Literal.String "bob")
                        Expr.Literal(Literal.Int 30L)
                    ]
                ]
        )

    commitInto store mgr txn

let private col (name: string) =
    Expr.Column { Qualifier = None; Name = name }

[<Fact>]
let ``ExecContext.create: 空の Schemas / Catalog`` () =
    withStore "ec-create" (fun store ->
        let ctx = ExecContext.create store None
        Assert.True(Map.isEmpty ctx.Schemas)
        Assert.Same(store, ctx.Store))

[<Fact>]
let ``ExecContext.withSchema: スキーマを登録する`` () =
    withStore "ec-withschema" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "people" peopleSchema c0
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
        let c1 = ExecContext.withSchema "people" peopleSchema c0
        let s = ok (ExecContext.schemaOf "people" c1)
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.schemaOf: 未登録は NameError`` () =
    withStore "ec-schemaof-miss" (fun store ->
        let c0 = ExecContext.create store None

        match ExecContext.schemaOf "nope" c0 with
        | Error e -> Assert.Equal(ErrorKind.NameError, e.Kind)
        | Ok _ -> Assert.Fail "NameError になるべき")

[<Fact>]
let ``ExecContext.schemaOf: 大文字小文字違いのキーでも引ける`` () =
    withStore "ec-schemaof-alias" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "People" peopleSchema c0

        // Map は Ordinal なので完全一致しないが、eqName で一致するキーへ落ちる
        let s = ok (ExecContext.schemaOf "people" c1)
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.schemaOf: カタログのテーブルへフォールバックする`` () =
    withStore "ec-schemaof-cat" (fun store ->
        let cat = Catalog.addTable Catalog.Empty (Catalog.toDefinition peopleSchema)
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withCatalog cat c0

        // Schemas に無くても Catalog から引ける
        let s = ok (ExecContext.schemaOf "PEOPLE" c1)
        Assert.Equal("people", s.Name))

[<Fact>]
let ``ExecContext.table: 登録スキーマで全走査`` () =
    withStore "ec-table" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "people" peopleSchema c0
        let op = ok (ExecContext.table c1 "people" 0)
        let rows = ok (Op.toArray op)
        Assert.Equal(0, rows.Length))

[<Fact>]
let ``ExecContext.byIndex: 未登録テーブルは NameError`` () =
    withStore "ec-byindex-miss-table" (fun store ->
        let c0 = ExecContext.create store None

        match ExecContext.byIndex c0 "nope" "ix" [| SqlValue.Int 1L |] 0 with
        | Error e -> Assert.Equal(ErrorKind.NameError, e.Kind)
        | Ok _ -> Assert.Fail "NameError になるべき")

[<Fact>]
let ``ExecContext.byIndex: 未登録索引は NameError`` () =
    withStore "ec-byindex-miss-idx" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "people" peopleSchema c0

        match ExecContext.byIndex c1 "people" "nope" [| SqlValue.Int 1L |] 0 with
        | Error e -> Assert.Equal(ErrorKind.NameError, e.Kind)
        | Ok _ -> Assert.Fail "NameError になるべき")

[<Fact>]
let ``ExecContext.byIndex: 空の store は空の Op`` () =
    withStore "ec-byindex-empty" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "people" TestHelpers.peopleSchema c0

        let op =
            ok (ExecContext.byIndex c1 "people" "ix_people_age" [| SqlValue.Int 1L |] 0)

        let rows = ok (Op.toArray op)
        Assert.Equal(0, rows.Length))

[<Fact>]
let ``ExecContext.byPrimaryKey: 存在しない行は empty`` () =
    withStore "ec-bypk" (fun store ->
        let c0 = ExecContext.create store None
        let c1 = ExecContext.withSchema "people" peopleSchema c0
        let op = ok (ExecContext.byPrimaryKey c1 "people" [| SqlValue.Int 99L |])
        let rows = ok (Op.toArray op)
        Assert.Equal(0, rows.Length))

[<Fact>]
let ``DmlOps.insert: INSERT は行を追加する`` () =
    withStore "ins" (fun store ->
        let mgr = TransactionManager.create ()
        let mgr, txn = TransactionManager.beginTxn mgr

        let txn, n =
            ok (
                DmlOps.insert
                    (ctxOf store txn)
                    peopleSchema
                    (Some [ "id"; "name" ])
                    [ [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.String "alice") ] ]
            )

        Assert.Equal<int>(1, n)

        commitInto store mgr txn |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema 0 None) |> Op.toArray |> ok
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal("alice", asText rows.[0].[1]))

[<Fact>]
let ``DmlOps.insert: INSERT は列順を明示できる`` () =
    withStore "inscols" (fun store ->
        let mgr = TransactionManager.create ()
        let mgr, txn = TransactionManager.beginTxn mgr

        // name, id の順で渡す
        let txn, _ =
            ok (
                DmlOps.insert
                    (ctxOf store txn)
                    peopleSchema
                    (Some [ "name"; "id" ])
                    [ [ Expr.Literal(Literal.String "carol"); Expr.Literal(Literal.Int 7L) ] ]
            )

        commitInto store mgr txn |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema 0 None) |> Op.toArray |> ok
        Assert.Equal<int64>(7L, asInt rows.[0].[0])
        Assert.Equal("carol", asText rows.[0].[1])
        // 指定されなかった列は NULL
        Assert.True(SqlValue.isNull rows.[0].[2]))

[<Fact>]
let ``DmlOps.insert: INSERT は DEFAULT で不足列を埋める`` () =
    let ageCol = TestHelpers.col "age" DataType.Integer

    let age =
        { ageCol with
            Constraint =
                { ColumnConstraint.Empty with
                    Default = Some(Expr.Literal(Literal.Int 42L))
                }
        }

    let schema =
        { peopleSchema with
            Columns =
                [
                    TestHelpers.col "id" DataType.Integer
                    TestHelpers.col "name" DataType.Text
                    age
                ]
        }

    withStore "ins-default" (fun store ->
        let mgr = TransactionManager.create ()
        let mgr, txn = TransactionManager.beginTxn mgr

        let txn, n =
            ok (DmlOps.insert (ctxOf store txn) schema (Some [ "id" ]) [ [ Expr.Literal(Literal.Int 1L) ] ])

        Assert.Equal<int>(1, n)

        commitInto store mgr txn |> ignore
        let rows = ok (Dml.scanByPrimaryKey store schema 0 None) |> Op.toArray |> ok
        Assert.Equal<int64>(42L, asInt rows.[0].[2]))

[<Fact>]
let ``DmlOps.insert: INSERT は評価できない DEFAULT をエラーにする`` () =
    let ageCol = TestHelpers.col "age" DataType.Integer

    let age =
        { ageCol with
            Constraint =
                { ColumnConstraint.Empty with
                    Default = Some(col "nope")
                }
        }

    let schema =
        { peopleSchema with
            Columns =
                [
                    TestHelpers.col "id" DataType.Integer
                    TestHelpers.col "name" DataType.Text
                    age
                ]
        }

    withStore "ins-default-bad" (fun store ->
        let mgr = TransactionManager.create ()
        let mgr, txn = TransactionManager.beginTxn mgr

        // 値を渡していない列の DEFAULT は評価される。評価エラーは NULL で握り潰さない。
        expectError
            "評価できない DEFAULT はエラー"
            (DmlOps.insert (ctxOf store txn) schema (Some [ "id" ]) [ [ Expr.Literal(Literal.Int 1L) ] ])

        let rows = ok (Dml.scanByPrimaryKey store schema 0 None) |> Op.toArray |> ok
        Assert.Empty rows)

[<Fact>]
let ``DmlOps.insert: INSERT の列数不一致はエラー`` () =
    withStore "insbad" (fun store ->
        let _ = ctxOf store
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let bad = [ [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.String "x") ] ]
        expectError "列数不一致が OK になっています" (DmlOps.insert (ctxOf store txn) peopleSchema (Some [ "id" ]) bad))

[<Fact>]
let ``DmlOps.insert: INSERT の値の評価失敗はエラー`` () =
    withStore "ins-eval" (fun store ->
        let _ = ctxOf store
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        // 定数式として評価できない列参照
        let bad = [ [ col "nope" ] ]
        expectError "評価できない式が OK になっています" (DmlOps.insert (ctxOf store txn) peopleSchema (Some [ "id" ]) bad))

[<Fact>]
let ``DmlOps.insert: INSERT の型強制失敗はエラー`` () =
    withStore "ins-coerce" (fun store ->
        let _ = ctxOf store
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        // INTEGER 列へ数値でない文字列は強制できない
        let bad = [ [ Expr.Literal(Literal.String "abc") ] ]
        expectError "型強制できない値が OK になっています" (DmlOps.insert (ctxOf store txn) peopleSchema (Some [ "id" ]) bad))

[<Fact>]
let ``DmlOps.update: UPDATE は WHERE に合う行だけを変える`` () =
    withStore "upd" (fun store ->

        // まず 2 行入れる
        let mgr0 = seedPeople store (TransactionManager.create ())
        // age が 25 以上の行の age を 1 増やす。mgr0 を引き継ぐ
        //（新しい TransactionManager は SeqNo が 1 から始まり、更新が消える）。
        let mgr1, txn1 = TransactionManager.beginTxn mgr0
        let ctx = ctxOf store txn1

        let txn1, n =
            ok (
                DmlOps.update
                    ctx
                    peopleSchema
                    [ ("age", Expr.Binary(BinaryOp.Add, col "age", Expr.Literal(Literal.Int 1L))) ]
                    (Some(Expr.Binary(BinaryOp.Ge, col "age", Expr.Literal(Literal.Int 25L))))
            )

        Assert.Equal<int>(1, n)

        commitInto store mgr1 txn1 |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema 0 None) |> Op.toArray |> ok
        // 1 行目は age 20（WHERE に合わないので変更なし）
        Assert.Equal<int64>(20L, asInt rows.[0].[2])
        // 2 行目は 30 -> 31
        Assert.Equal<int64>(31L, asInt rows.[1].[2]))

[<Fact>]
let ``DmlOps.update: UPDATE 対象が transaction 上限を超えると CapacityError`` () =
    withStore "upd-limit" (fun store ->
        let mgr = seedPeople store (TransactionManager.create ())

        let txn =
            { Transaction.create 1UL { SeqNo = mgr.LastCommitSeqNo } with
                LimitBytes = 1L
            }

        expectErrorKind
            ErrorKind.CapacityError
            "UPDATE 上限を超えた対象行を検出できなかった"
            (DmlOps.update (ctxOf store txn) peopleSchema [ ("age", Expr.Literal(Literal.Int 1L)) ] None))

[<Fact>]
let ``DmlOps.delete: DELETE は WHERE に合う行を消す`` () =
    withStore "del" (fun store ->

        // seed の TransactionManager を引き継ぐ
        //（新しい TransactionManager は SeqNo が 1 から始まり、tombstone が消える）。
        let mgr0 = seedPeople store (TransactionManager.create ())
        let mgr1, txn1 = TransactionManager.beginTxn mgr0
        let ctx = ctxOf store txn1

        let txn2, n =
            ok (
                DmlOps.delete ctx peopleSchema (Some(Expr.Binary(BinaryOp.Eq, col "id", Expr.Literal(Literal.Int 1L))))
            )

        Assert.Equal<int>(1, n)

        commitInto store mgr1 txn2 |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema 0 None) |> Op.toArray |> ok
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal("bob", asText rows.[0].[1]))
