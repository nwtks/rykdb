// ===========================================================================
// TableSchema.fs のテスト（テーブル定義の検証・主キー解決）
// ===========================================================================

module RykDb.TableSchemaTests

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
let ``TableSchema.validateRow: 正常行は Ok`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]

    match TableSchema.validateRow schema row with
    | Ok _ -> ()
    | Error e -> Assert.Fail(DbError.toDisplayString e)

[<Fact>]
let ``TableSchema.validateRow: 列数不一致はエラー`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L |]

    expectError "列数不一致はエラーになるべき" (TableSchema.validateRow schema row)

[<Fact>]
let ``TableSchema.validateRow: NOT NULL 違反はエラー`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Null; SqlValue.Text "alice"; SqlValue.Int 20L |]

    expectError "NOT NULL 違反はエラーになるべき" (TableSchema.validateRow schema row)

[<Fact>]
let ``TableSchema.primaryKeyOf: 主キー列を取り出す`` () =
    let schema = peopleSchema
    let row = [| SqlValue.Int 7L; SqlValue.Text "alice"; SqlValue.Int 20L |]
    let pk = TableSchema.primaryKeyOf schema row
    Assert.Equal<int>(1, pk.Length)
    Assert.Equal(SqlValue.Int 7L, pk.[0])

[<Fact>]
let ``TableSchema.requireIndexOf: 無い列はエラー`` () =
    Assert.Equal<int>(0, ok (TableSchema.requireIndexOf peopleSchema "id"))
    expectErrorKind ErrorKind.NameError "無い列で位置が返った" (TableSchema.requireIndexOf peopleSchema "nope")
