namespace RykDb.Core

/// 索引の定義。
type internal IndexSchema =
    {
        Name: string
        /// 索引を張る列
        Columns: string list
        /// 値が一意か（非 UNIQUE なら false）
        Unique: bool
    }

/// テーブルのスキーマ。
type internal TableSchema =
    {
        Name: string
        /// 列の定義（順序は行の並びと一致する）
        Columns: ColumnDefinition list
        /// 主キーになる列のインデックス（Columns 内の位置）
        PrimaryKey: int list
        /// この表の索引
        Indexes: IndexSchema list
    }

module internal TableSchema =
    /// 列名の重複を検証する（SQL の識別子は大文字小文字を区別しない）。
    let private validateColumnNames (columns: ColumnDefinition list) =
        match columns |> List.map _.Name |> SqlName.duplicate with
        | Some name -> err (DbError.name (sprintf "列 %s が重複しています" name))
        | None -> ret ()

    /// PRIMARY KEY の定義を1つだけ取り出す。
    /// 列レベルとテーブルレベルの両方、複数のテーブル制約はエラーにする。
    let private primaryKeyNames (c: CreateTable) =
        let tableConstraints =
            c.Constraints
            |> List.choose (fun tc ->
                match tc with
                | TableConstraint.PrimaryKeyColumns cols -> Some cols
                | _ -> None)

        let columnNames =
            c.Columns |> List.filter _.Constraint.PrimaryKey |> List.map _.Name

        match tableConstraints, columnNames with
        | [], [] -> err (sprintf "テーブル %s に主キーがありません" c.Name |> DbError.constraintViolation)
        | [ cols ], [] when cols.IsEmpty -> err (sprintf "テーブル %s の主キー列が空です" c.Name |> DbError.constraintViolation)
        | [ cols ], [] -> ret cols
        | [], [ name ] -> ret [ name ]
        | [], _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY が複数の列に指定されています" c.Name
                |> DbError.constraintViolation
            )
        | _ :: _ :: _, _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY のテーブル制約が複数あります" c.Name
                |> DbError.constraintViolation
            )
        | [ _ ], _ :: _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY が列とテーブルの両方に指定されています" c.Name
                |> DbError.constraintViolation
            )

    /// PRIMARY KEY の列が実在し、重複していないことを検証する。
    let private validatePrimaryKeyColumns (columns: ColumnDefinition list) (names: string list) =
        match SqlName.duplicate names with
        | Some name -> err (DbError.name (sprintf "PRIMARY KEY の列 %s が重複しています" name))
        | None ->
            names
            |> List.map (fun name ->
                match columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                | Some _ -> ret ()
                | None -> err (DbError.name (sprintf "主キー列 %s が定義されていません" name)))
            |> all
            |> Result.map ignore

    /// テーブルレベルの UNIQUE 制約の列が実在し、重複していないことを検証する。
    let private validateUniqueColumns (columns: ColumnDefinition list) (names: string list) =
        if List.isEmpty names then
            err (DbError.constraintViolation "UNIQUE 制約の列が空です")
        else
            match SqlName.duplicate names with
            | Some name -> err (DbError.name (sprintf "UNIQUE 制約の列 %s が重複しています" name))
            | None ->
                names
                |> List.map (fun name ->
                    match columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                    | Some _ -> ret ()
                    | None -> err (DbError.name (sprintf "UNIQUE 制約の列 %s が定義されていません" name)))
                |> all
                |> Result.map ignore

    /// 主キー列に NOT NULL と PRIMARY KEY を立てる。
    /// テーブルレベルの PRIMARY KEY でも列定義を自己完結させる。
    let private markPrimaryKeyColumns (pkNames: string list) (columns: ColumnDefinition list) =
        columns
        |> List.map (fun col ->
            if pkNames |> List.exists (fun name -> SqlName.equal name col.Name) then
                { col with
                    Constraint =
                        { col.Constraint with
                            NotNull = true
                            PrimaryKey = true
                        }
                }
            else
                col)

    /// CreateTable の AST からスキーマを作る。
    let ofCreateTable (c: CreateTable) =
        result {
            do!
                if c.Columns.IsEmpty then
                    err (sprintf "テーブル %s に列がありません" c.Name |> DbError.constraintViolation)
                else
                    ret ()

            do! validateColumnNames c.Columns
            let! pkNames = primaryKeyNames c
            do! validatePrimaryKeyColumns c.Columns pkNames

            let! () =
                c.Constraints
                |> List.choose (fun tc ->
                    match tc with
                    | TableConstraint.UniqueColumns cols -> Some cols
                    | _ -> None)
                |> List.map (validateUniqueColumns c.Columns)
                |> all
                |> Result.map ignore

            let! pk =
                pkNames
                |> List.map (fun name ->
                    match c.Columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                    | Some i -> ret i
                    | None -> err (sprintf "主キー列 %s が定義されていません" name |> DbError.name))
                |> all
                |> Result.map List.sort

            return
                {
                    Name = c.Name
                    Columns = markPrimaryKeyColumns pkNames c.Columns
                    PrimaryKey = pk
                    Indexes = []
                }
        }

    /// NOT NULL 制約と型を検証する。
    let validateRow (schema: TableSchema) (row: SqlValue array) =
        if row.Length <> schema.Columns.Length then
            err (
                sprintf "テーブル %s は %d 列ですが %d 個の値が渡されました" schema.Name schema.Columns.Length row.Length
                |> DbError.constraintViolation
            )
        else
            schema.Columns
            |> List.mapi (fun i col -> i, col)
            |> foldOk
                (fun () (i, col) ->
                    let isPrimaryKey = schema.PrimaryKey |> List.contains i

                    if row.[i] = SqlValue.Null && (col.Constraint.NotNull || isPrimaryKey) then
                        err (
                            sprintf "NOT NULL 制約に違反: %s.%s" schema.Name col.Name
                            |> DbError.constraintViolation
                        )
                    else
                        ret ())
                ()

    /// 主キー部分だけ取り出す。
    let primaryKeyOf (schema: TableSchema) (row: SqlValue array) =
        schema.PrimaryKey |> List.map (fun i -> row.[i]) |> List.toArray

    /// 主キー列名。
    let primaryKeyColumns (schema: TableSchema) =
        schema.PrimaryKey |> List.map (fun i -> schema.Columns.[i].Name) |> List.toArray

    /// 列名 → 位置。
    /// SQL の識別子は大文字小文字を区別しないので、ここで正規化する。
    let indexOf (schema: TableSchema) (column: string) =
        schema.Columns |> List.tryFindIndex (fun c -> SqlName.equal c.Name column)

    /// 列名 → 位置（見つからなければエラー）。
    let requireIndexOf (schema: TableSchema) (column: string) =
        match indexOf schema column with
        | Some i -> ret i
        | None -> err (sprintf "テーブル %s に列 %s がありません" schema.Name column |> DbError.name)
