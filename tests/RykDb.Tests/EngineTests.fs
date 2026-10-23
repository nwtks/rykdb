// ===========================================================================
// Engine.fs のテスト: INSERT / UPDATE / DELETE
// ===========================================================================

module RykDb.EngineTests

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

let private ctxOf (store: RowStore) =
    ExecContext.create store Snapshot.noLimit

/// commit して store へ流し込む。引き継いだ TransactionManager を返す。
let private commitInto (store: RowStore) (mgr: TransactionManager) (txn: Transaction) =
    let mgr2, _, batches = ok (TransactionManager.commit mgr txn)
    ok (RowStore.writeBatches store batches) |> ignore
    mgr2

/// alice(20) / bob(30) の 2 行を入れて commit する（UPDATE / DELETE の土台）。
let private seedPeople (store: RowStore) (mgr: TransactionManager) =
    let mgr, txn = TransactionManager.beginTxn mgr

    let txn, _ =
        ok (
            DmlOps.insert
                (ctxOf store)
                peopleSchema
                txn
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
let ``末尾再帰: 多数の列を持つ INSERT / UPDATE が完了する`` () =
    // 値リストの評価（gatherWith）は `evalOne … >>= fun … -> coerce …
    // >>= fun … -> gather …` で列の数だけフレームが積まれていた
    // （→ StackOverflow / exit 134）。`match` で受けて末尾再帰にしてある。
    let cols = 2_000
    let names = Array.init cols (fun i -> sprintf "c%d" i)

    // 主キーは必須（無いと CREATE TABLE 自体がエラーになる）。
    let decls =
        names
        |> Array.mapi (fun i c -> sprintf "%s INTEGER%s" c (if i = 0 then " PRIMARY KEY" else ""))
        |> String.concat ", "

    let colList = String.concat ", " names
    let values = Array.init cols string |> String.concat ", "

    withDb "tail-gather" (fun db0 ->
        let db1 = exec db0 (sprintf "CREATE TABLE wide (%s)" decls)
        let db2 = exec db1 (sprintf "INSERT INTO wide (%s) VALUES (%s)" colList values)
        let inserted = query db2 "SELECT * FROM wide"
        Assert.Equal(1, inserted.Rows.Length)
        Assert.Equal(cols, inserted.Columns.Length)

        // UPDATE も同じ経路（SET の数だけ gather が回る）。SET の列数は
        // パーサの assigns にも効くので、こちらも末尾再帰にしてある。
        // 主キー（c0）は書き換えない（主キー更新は既知の別件なので外す）。
        let sets =
            Array.init (cols - 1) (fun i -> sprintf "c%d = %d" (i + 1) (i + 2))
            |> String.concat ", "

        let db3 = exec db2 (sprintf "UPDATE wide SET %s" sets)
        let updated = query db3 (sprintf "SELECT c0, c%d FROM wide" (cols - 1))
        Assert.Equal(1, updated.Rows.Length)
        Assert.Equal<int64>(0L, asInt updated.Rows.[0].[0])
        Assert.Equal<int64>(int64 cols, asInt updated.Rows.[0].[1]))

[<Fact>]
let ``DML: INSERT は行を追加する`` () =
    withStore "ins" (fun store ->
        let mgr = TransactionManager.create ()
        let mgr, txn = TransactionManager.beginTxn mgr

        let txn, n =
            ok (
                DmlOps.insert
                    (ctxOf store)
                    peopleSchema
                    txn
                    (Some [ "id"; "name" ])
                    [ [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.String "alice") ] ]
            )

        Assert.Equal<int>(1, n)

        commitInto store mgr txn |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0) |> Seq.toArray
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal("alice", asText rows.[0].[1]))

[<Fact>]
let ``DML: INSERT は列順を明示できる`` () =
    withStore "inscols" (fun store ->
        let mgr = TransactionManager.create ()
        let mgr, txn = TransactionManager.beginTxn mgr

        // name, id の順で渡す
        let txn, _ =
            ok (
                DmlOps.insert
                    (ctxOf store)
                    peopleSchema
                    txn
                    (Some [ "name"; "id" ])
                    [ [ Expr.Literal(Literal.String "carol"); Expr.Literal(Literal.Int 7L) ] ]
            )

        commitInto store mgr txn |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0) |> Seq.toArray
        Assert.Equal<int64>(7L, asInt rows.[0].[0])
        Assert.Equal("carol", asText rows.[0].[1])
        // 指定されなかった列は NULL
        Assert.True(SqlValue.isNull rows.[0].[2]))

[<Fact>]
let ``DML: INSERT の列数不一致はエラー`` () =
    withStore "insbad" (fun store ->
        let _ = ctxOf store
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let bad = [ [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.String "x") ] ]
        expectError "列数不一致が OK になっています" (DmlOps.insert (ctxOf store) peopleSchema txn (Some [ "id" ]) bad))

[<Fact>]
let ``DML: UPDATE は WHERE に合う行だけを変える`` () =
    withStore "upd" (fun store ->
        let ctx = ctxOf store

        // まず 2 行入れる
        let mgr0 = seedPeople store (TransactionManager.create ())
        // age が 25 以上の行の age を 1 増やす。mgr0 を引き継ぐ
        //（新しい TransactionManager は SeqNo が 1 から始まり、更新が消える）。
        let mgr1, txn1 = TransactionManager.beginTxn mgr0

        let txn1, n =
            ok (
                DmlOps.update
                    ctx
                    peopleSchema
                    txn1
                    [ ("age", Expr.Binary(BinaryOp.Add, col "age", Expr.Literal(Literal.Int 1L))) ]
                    (Some(Expr.Binary(BinaryOp.Ge, col "age", Expr.Literal(Literal.Int 25L))))
            )

        Assert.Equal<int>(1, n)

        commitInto store mgr1 txn1 |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0) |> Seq.toArray
        // 1 行目は age 20（WHERE に合わないので変更なし）
        Assert.Equal<int64>(20L, asInt rows.[0].[2])
        // 2 行目は 30 -> 31
        Assert.Equal<int64>(31L, asInt rows.[1].[2]))

[<Fact>]
let ``StatementResult: 表示行はヘッダ・区切り・行を並べる`` () =
    Assert.Equal<string list>([ "(0 rows)" ], StatementResult.empty.ToDisplayLines())

    let r =
        {
            RowsAffected = 0
            Columns = [| "a"; "b" |]
            Rows = [| [| SqlValue.Int 1L; SqlValue.Text "x" |] |]
        }

    Assert.Equal<string list>([ "a | b"; "-----"; "  1, 'x'" ], r.ToDisplayLines())

[<Fact>]
let ``DML: INSERT の値の評価失敗はエラー`` () =
    withStore "ins-eval" (fun store ->
        let _ = ctxOf store
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        // 定数式として評価できない列参照
        let bad = [ [ col "nope" ] ]
        expectError "評価できない式が OK になっています" (DmlOps.insert (ctxOf store) peopleSchema txn (Some [ "id" ]) bad))

[<Fact>]
let ``DML: INSERT の型強制失敗はエラー`` () =
    withStore "ins-coerce" (fun store ->
        let _ = ctxOf store
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        // INTEGER 列へ数値でない文字列は強制できない
        let bad = [ [ Expr.Literal(Literal.String "abc") ] ]
        expectError "型強制できない値が OK になっています" (DmlOps.insert (ctxOf store) peopleSchema txn (Some [ "id" ]) bad))

[<Fact>]
let ``DML: INSERT は DEFAULT で不足列を埋める`` () =
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
            ok (DmlOps.insert (ctxOf store) schema txn (Some [ "id" ]) [ [ Expr.Literal(Literal.Int 1L) ] ])

        Assert.Equal<int>(1, n)

        commitInto store mgr txn |> ignore
        let rows = ok (Dml.scanByPrimaryKey store schema None 0) |> Seq.toArray
        Assert.Equal<int64>(42L, asInt rows.[0].[2]))

[<Fact>]
let ``DML: INSERT は評価できない DEFAULT を NULL にする`` () =
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

        let txn, _ =
            ok (DmlOps.insert (ctxOf store) schema txn (Some [ "id" ]) [ [ Expr.Literal(Literal.Int 1L) ] ])

        commitInto store mgr txn |> ignore
        let rows = ok (Dml.scanByPrimaryKey store schema None 0) |> Seq.toArray
        Assert.True(SqlValue.isNull rows.[0].[2]))

[<Fact>]
let ``DML: DELETE は WHERE に合う行を消す`` () =
    withStore "del" (fun store ->
        let ctx = ctxOf store

        // seed の TransactionManager を引き継ぐ
        //（新しい TransactionManager は SeqNo が 1 から始まり、tombstone が消える）。
        let mgr0 = seedPeople store (TransactionManager.create ())
        let mgr2, txn2 = TransactionManager.beginTxn mgr0

        let txn2, n =
            ok (
                DmlOps.delete
                    ctx
                    peopleSchema
                    txn2
                    (Some(Expr.Binary(BinaryOp.Eq, col "id", Expr.Literal(Literal.Int 1L))))
            )

        Assert.Equal<int>(1, n)

        commitInto store mgr2 txn2 |> ignore
        let rows = ok (Dml.scanByPrimaryKey store peopleSchema None 0) |> Seq.toArray
        Assert.Equal<int>(1, rows.Length)
        Assert.Equal("bob", asText rows.[0].[1]))
