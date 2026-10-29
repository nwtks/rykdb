// ===========================================================================
// RowStore.fs のテスト: keyspace シャーディング・行の読み書き・制約・索引
// ===========================================================================

module RykDb.RowStoreTests

open System.IO
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

let private withIndex (schema: TableSchema) (index: IndexSchema) =
    let indexes =
        schema.Indexes
        |> List.filter (fun i -> i.Name <> index.Name)
        |> fun l -> l @ [ index ]

    { schema with Indexes = indexes }

let private usersWithEmailIndex =
    withIndex
        usersSchema
        {
            Name = "users_email"
            Columns = [ "email" ]
            Unique = true
        }

let private rowOf (id: int) (email: string) (age: int) =
    [| SqlValue.Int(int64 id); SqlValue.Text email; SqlValue.Int(int64 age) |]

/// transaction のバッファを DB に流し込む。
let private commitInto (store: RowStore) (nextSeq: uint64) (txn: Transaction) =
    let batches = Transaction.toEntries nextSeq txn |> fst
    RowStore.writeBatches store batches |> ok |> ignore

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
let ``RowKey.primary: 主キー列を符号化する`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]
    let k = RowKey.primary schema row
    Assert.True(k.Length > 0)

[<Fact>]
let ``RowKey.indexKey: 索引列 + 主キー列を符号化する`` () =
    let schema = peopleSchema
    let idx = schema.Indexes.[0]
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]
    let k = RowKey.indexKey schema idx row
    let prefix = RowKey.indexPrefix schema idx row
    // indexKey は prefix で始まる
    Assert.True(KeyBuilder.startsWithPrefix k prefix)

[<Fact>]
let ``RowKey.indexPrefix: 索引列だけを符号化する`` () =
    let schema = peopleSchema
    let idx = schema.Indexes.[0]
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]
    let p = RowKey.indexPrefix schema idx row
    Assert.True(p.Length > 0)

[<Fact>]
let ``Keyspace: table / index の名前`` () =
    Assert.Equal("t:people", Keyspace.table "people")
    Assert.Equal("i:ix_people_age", Keyspace.index "ix_people_age")

[<Fact>]
let ``Keyspace: keyspace はテーブルと索引で別になる`` () =
    Assert.Equal<string>("t:users", Keyspace.table "users")
    Assert.Equal<string>("i:users_email", Keyspace.index "users_email")
    Assert.NotEqual<string>(Keyspace.table "users", Keyspace.index "users_email")

    // ディレクトリ名に使える形（base32 なので / や : を含む名前も安全）
    Assert.NotEqual<string>(Keyspace.toDirName "t:a/b", Keyspace.toDirName "t:a_b")

[<Fact>]
let ``Keyspace: base32 はパス区切りと大文字小文字衝突を作らない`` () =
    let unicode = Keyspace.table "住所"
    let dir = Keyspace.toDirName unicode
    Assert.False(dir.Contains "/")
    Assert.False(dir.Contains "\\")
    Assert.Equal(Some unicode, Keyspace.tryFromDirName dir)

    // base64 では t:a -> dDph / t:G -> dDpH となり、
    // Windows/macOS の case-insensitive FS で衝突していた。
    let a = Keyspace.toDirName (Keyspace.table "a")
    let g = Keyspace.toDirName (Keyspace.table "G")
    Assert.NotEqual<string>(a, g)
    Assert.False(System.String.Equals(a, g, System.StringComparison.OrdinalIgnoreCase))

[<Fact>]
let ``Keyspace: toDirName / tryFromDirName の往復`` () =
    let ks = "t:people"
    let dir = Keyspace.toDirName ks
    Assert.Equal(Some ks, Keyspace.tryFromDirName dir)

[<Fact>]
let ``DropCleanup: 未完了の後始末対象を追加・復元・消去できる`` () =
    withDir "drop-cleanup-journal" (fun dir ->
        ok (DropCleanup.write dir [ "t:a" ])
        ok (DropCleanup.add dir [ "i:b"; "t:a" ])
        Assert.Equal<string list>([ "t:a"; "i:b" ], ok (DropCleanup.pending dir))
        ok (DropCleanup.clear dir)
        Assert.Empty(ok (DropCleanup.pending dir)))

[<Fact>]
let ``RowStore.openStore: 新規 DB でも catalog エンジンを登録する`` () =
    withDir "rs-catalog-engine" (fun dir ->
        let store = ok (RowStore.openStore dir)
        Assert.True(Map.containsKey "__catalog__" store.Engines)
        ok (RowStore.close store))

[<Fact>]
let ``RowStore: keyspace ごとに独立したディレクトリを持つ`` () =
    withDir "rs7" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr
        let txn = Dml.insert usersWithEmailIndex txn (rowOf 1 "a@x.com" 1) |> ok
        commitInto store 1UL txn

        // 主キーのツリーと索引のツリーで別のディレクトリができている
        let dirs = Directory.GetDirectories dir
        Assert.True(dirs.Length >= 2)

        let hasWal =
            dirs
            |> Array.exists (fun d -> Directory.GetFiles(d, "*" + Wal.WalExtension).Length > 0)

        Assert.True hasWal)

[<Fact>]
let ``RowStore.openStore: 壊れた keyspace は開けない`` () =
    withDir "rs-corrupt" (fun dir ->
        let ksDir = Path.Combine(dir, Keyspace.toDirName "t:broken")
        Directory.CreateDirectory ksDir |> ignore
        File.WriteAllText(Path.Combine(ksDir, "CURRENT"), "壊れた CURRENT")
        expectError "壊れた keyspace を開けてしまう" (RowStore.openStore dir))

[<Fact>]
let ``RowStore.openStore: 開く途中で失敗してもロックを解放する`` () =
    withDir "rs-lock-cleanup" (fun dir ->
        let ksDir = Path.Combine(dir, Keyspace.toDirName "t:broken")
        Directory.CreateDirectory ksDir |> ignore
        File.WriteAllText(Path.Combine(ksDir, "CURRENT"), "壊れた CURRENT")
        expectError "壊れた keyspace を開けてしまう" (RowStore.openStore dir)

        // 失敗した open がプロセス内ロックを残していると、ここで IoError になる。
        Directory.Delete(ksDir, true)
        let store = ok (RowStore.openStore dir)
        ok (RowStore.close store))

[<Fact>]
let ``RowStore.openStore: 壊れた MANIFEST レコードは読み飛ばす`` () =
    withDir "rs-manifest" (fun dir ->
        // ルートの keyspace 開設時（openStore では subdirectory のみ走査）に読ませる
        let ksDir = Path.Combine(dir, Keyspace.toDirName "t:manifest")
        Directory.CreateDirectory ksDir |> ignore
        File.WriteAllText(Path.Combine(ksDir, "CURRENT"), "MANIFEST-000001")
        // base64 としては正しいが、中身が VersionEdit として壊れている
        File.WriteAllText(Path.Combine(ksDir, "MANIFEST-000001"), "AA==")
        // レコード単位で捨てて先へ進む（開けるが、その前後も読める）
        ok (RowStore.openStore dir) |> ignore)

[<Fact>]
let ``Dml.scanByPrimaryKey: 主キーで全行を走査できる`` () =
    withStore "rs6" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        let txn =
            [ 1; 2; 3 ]
            |> List.fold
                (fun t i -> Dml.insert usersWithEmailIndex t (rowOf i (sprintf "u%d@example.com" i) i) |> ok)
                txn

        commitInto store 1UL txn

        let rows =
            Dml.scanByPrimaryKey store usersWithEmailIndex None 0 |> ok |> Seq.toArray

        Assert.Equal(3, rows.Length)

        for i in 0..2 do
            Assert.Equal<int64>(int64 (i + 1), asInt rows.[i].[0]))

[<Fact>]
let ``Dml.scanByPrimaryKey: 主キーで全行を走査できる（limit）`` () =
    withStore "rs6b" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        let txn =
            [ 1; 2; 3; 4 ]
            |> List.fold
                (fun t i -> Dml.insert usersWithEmailIndex t (rowOf i (sprintf "u%d@example.com" i) i) |> ok)
                txn

        commitInto store 1UL txn

        let rows =
            Dml.scanByPrimaryKey store usersWithEmailIndex None 2 |> ok |> Seq.toArray

        Assert.Equal(2, rows.Length))

[<Fact>]
let ``Dml.insert: NOT NULL 制約違反は弾かれる`` () =
    let notNull =
        { ColumnConstraint.Empty with
            NotNull = true
        }

    let emailCol = usersWithEmailIndex.Columns.[1]

    let schema =
        { usersWithEmailIndex with
            Columns =
                [
                    usersWithEmailIndex.Columns.[0]
                    { emailCol with Constraint = notNull }
                    usersWithEmailIndex.Columns.[2]
                ]
        }

    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr
    // email が NULL
    let bad = [| SqlValue.Int 1L; SqlValue.Null; SqlValue.Int 20L |]
    expectErrorKind ErrorKind.ConstraintViolation "NOT NULL 制約違反を検出できなかった" (Dml.insert schema txn bad)

[<Fact>]
let ``Dml.insert: transaction 上限を超えると CapacityError`` () =
    let txn =
        { Transaction.create 1UL { SeqNo = 0UL } with
            LimitBytes = 1L
        }

    expectErrorKind
        ErrorKind.CapacityError
        "transaction 上限を超えた INSERT を検出できなかった"
        (Dml.insert usersSchema txn (rowOf 1 "a@x.com" 1))

[<Fact>]
let ``Dml.insert: 列数が違う行は弾かれる`` () =
    let mgr = TransactionManager.create ()
    let _, txn = TransactionManager.beginTxn mgr

    expectErrorKind
        ErrorKind.ConstraintViolation
        "列数不一致を検出できなかった"
        (Dml.insert usersWithEmailIndex txn [| SqlValue.Int 1L |])

[<Fact>]
let ``Dml.updateByPrimaryKey: 存在しない行の更新は NotFound`` () =
    withStore "rs-upd-miss" (fun store ->
        let mgr = TransactionManager.create ()
        let _, txn = TransactionManager.beginTxn mgr

        expectErrorKind
            ErrorKind.NotFound
            "存在しない行の更新を許してはいけない"
            (Dml.updateByPrimaryKey store usersWithEmailIndex None txn [| SqlValue.Int 999L |] (rowOf 999 "a@x.com" 1)))

[<Fact>]
let ``Dml.deleteByPrimaryKey: 存在しない行の削除は NotFound`` () =
    withStore "rs4b" (fun store ->
        let mgr = TransactionManager.create ()
        let _, delTxn = TransactionManager.beginTxn mgr

        expectErrorKind
            ErrorKind.NotFound
            "存在しない行の削除を許してはいけない"
            (Dml.deleteByPrimaryKey store usersWithEmailIndex None delTxn [| SqlValue.Int 999L |]))

[<Fact>]
let ``RowStore.writeBatchesWithSnapshot: engine に GC 境界を設定する`` () =
    withDir "rs-snapshot" (fun dir ->
        let store = RowStore.openStore dir |> ok
        let batch = "t:a", [| Entry.put (intKey 1) 1UL (intRow 1) |]
        let store = RowStore.writeBatchesWithSnapshot 7UL store [ batch ] |> ok
        let engine = RowStore.engine store "t:a" |> ok
        Assert.Equal(7UL, engine.ActiveSnapshot)
        RowStore.close store |> ok)

/// ジャーナルの未解決件数。
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
            (RowStore.engine reopened "t:a" |> ok).GetAt None (intKey 1)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(
            Some 2UL,
            (RowStore.engine reopened "t:b" |> ok).GetAt None (intKey 2)
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
        let store2 = RowStore.writeBatchesWithSnapshot 0UL store [ b1; b2 ] |> ok
        // 公開と解決が両方記録され、未解決は残らない
        Assert.Equal(0, unresolvedOf dir)
        Assert.True(File.Exists(journalOf dir), "ジャーナルが作られていません")

        RowStore.close store2 |> ok
        let reopened = RowStore.openStoreTakingOver dir |> ok

        Assert.Equal(
            Some 1UL,
            (RowStore.engine reopened "t:a" |> ok).GetAt None (intKey 1)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(
            Some 2UL,
            (RowStore.engine reopened "t:b" |> ok).GetAt None (intKey 2)
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
            (RowStore.engine store "t:a" |> ok).GetAt None (intKey 1)
            |> ok
            |> Option.map _.SeqNo
        )

        Assert.Equal(
            None,
            (RowStore.engine store "t:b" |> ok).GetAt None (intKey 2)
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
            RowStore.writeBatchesWithSnapshot 0UL store [ batch "t:a" 0 100; batch "t:b" 10000 100 ]
            |> ok

        let len1 = FileInfo(journalOf dir).Length
        Assert.True(len1 > 0L, "ジャーナルが空です")
        Assert.True(len1 < 262144L, "1 回目で既に間引かれています")

        // 2 回目で閾値を超え、解決後に間引かれる。
        let store3 =
            RowStore.writeBatchesWithSnapshot 0UL store2 [ batch "t:c" 20000 1200; batch "t:d" 30000 1200 ]
            |> ok

        let len2 = FileInfo(journalOf dir).Length
        Assert.True(len2 < len1, sprintf "間引かれていません（%d -> %d）" len1 len2)
        Assert.Equal(0, unresolvedOf dir)

        RowStore.close store3 |> ok
        let reopened = RowStore.openStoreTakingOver dir |> ok

        // 間引いてもデータは失われない
        Assert.True(
            (RowStore.engine reopened "t:c" |> ok).GetAt None (intKey 20000)
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
            RowStore.writeBatchesWithSnapshot
                0UL
                store
                [
                    "t:p", [| Entry.put (intKey 1) 1UL [| SqlValue.Int 1L |] |]
                    "t:q", [| Entry.put (intKey 2) 2UL [| SqlValue.Int 2L |] |]
                ]
            |> ok

        let lenAfter = FileInfo(journalOf dir).Length
        Assert.True(lenAfter >= lenBig, sprintf "未解決の公開が消されています（%d -> %d）" lenBig lenAfter)
        Assert.Equal(1, unresolvedOf dir)

        RowStore.close store2 |> ok)

[<Fact>]
let ``RowStore.commitToStore: 別ハンドルの active snapshot を GC 境界に使う`` () =
    withDir "commit-boundary" (fun dir ->
        let store = ok (RowStore.openStore dir)

        // 既に SeqNo 10 まで commit 済みのハンドル（SeqNo 11 から採番する）。
        let mgr =
            { TransactionManager.create () with
                LastCommitSeqNo = 10UL
                NextSeqNo = 11UL
            }

        let mgr, txn = TransactionManager.beginTxn mgr
        let txn = Transaction.put "t:a" (intKey 1) (intRow 1) txn

        // 同じ DB を開いた別ハンドルが snapshot 5 で読み始めている状態。
        let other = ActiveSnapshotRegistry.register dir 5UL

        let committed, _ = ok (RowStore.commitToStore dir None mgr txn store)
        let engine = ok (RowStore.engine committed "t:a")

        // 自ハンドルだけなら 12（LastCommitSeqNo 11 + 1）だが、別ハンドルの 5 が下限になる。
        Assert.Equal(5UL, engine.ActiveSnapshot)

        ActiveSnapshotRegistry.unregister dir other
        RowStore.close committed |> ok)

[<Fact>]
let ``RowStore.commitToStore: 自分の登録は GC 境界から除外する`` () =
    withDir "commit-boundary-self" (fun dir ->
        let store = ok (RowStore.openStore dir)

        let mgr =
            { TransactionManager.create () with
                LastCommitSeqNo = 10UL
                NextSeqNo = 11UL
            }

        let mgr, txn = TransactionManager.beginTxn mgr
        let txn = Transaction.put "t:a" (intKey 1) (intRow 1) txn

        // 自分（これから commit する transaction）の登録は除外し、他の登録だけを見る。
        let own = ActiveSnapshotRegistry.register dir txn.Snapshot.SeqNo

        let committed, _ = ok (RowStore.commitToStore dir (Some own) mgr txn store)
        let engine = ok (RowStore.engine committed "t:a")

        Assert.Equal(12UL, engine.ActiveSnapshot)

        ActiveSnapshotRegistry.unregister dir own
        RowStore.close committed |> ok)
