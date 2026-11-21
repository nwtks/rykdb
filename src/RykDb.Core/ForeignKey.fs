// ===========================================================================
// ForeignKey — 外部キー（REFERENCES）の定義の解決と参照整合性検査。
//
//  ・参照先は親テーブルの主キーだけ（列の型も一致させる）。
//  ・子の外部キー列に NULL を含む行は参照しない（MATCH SIMPLE。SQL の既定）。
//  ・参照アクションは未対応。RESTRICT（即時検査）だけを行う。
//      - 子の INSERT / UPDATE は親行の存在を要求する。
//        検査は「行を書いた後」の transaction 状態で行う（この行自身を親にできる）。
//      - 親の DELETE は参照している子行があると ConstraintViolation（削除前に行を検査）。
//    親の主キーは UPDATE できない（`Dml.updateRow` が拒否する）ので、親の UPDATE 検査は要らない。
// ===========================================================================

namespace RykDb.Core

module internal ForeignKey =
    /// 子行の外部キー検査の準備結果。
    /// 文単位で 1回だけ準備し、行ごとに使い回す
    /// （親スキーマ・列位置・mask を行ごとに作り直さない）。
    type internal ChildCheck =
        {
            /// 外部キー定義（列名・親テーブル）。
            Definition: ForeignKeyDefinition
            /// 子行から外部キーの値を取り出す位置。
            ChildPositions: int list
            /// 親テーブルの実行用スキーマ。
            Parent: TableSchema
            /// 存在確認だけに使う mask（全列 false）。
            ProbeMask: bool array
        }

    /// 親行の削除を止める子側の参照検査。
    /// 文単位で 1回だけ準備し、削除行ごとに使い回す
    /// （子スキーマ・列位置・mask を行ごとに作り直さない）。
    type internal ParentDeleteCheck =
        {
            /// 子テーブル（索引を載せた実行用スキーマ）。
            Child: TableSchema
            /// 子が参照している親の列名（エラーメッセージ用）。
            ParentColumns: string list
            /// 親行から参照値を取り出す位置。
            ParentPositions: int list
            /// 外部キーの列が前置になっている索引（あれば）。
            Index: IndexSchema option
            /// 外部キーの列が子の主キーと同順か。
            PrimaryKeyMatches: bool
            /// 存在確認だけに使う mask（全列 false）。
            ProbeMask: bool array
            /// 子の全走査で使う mask（外部キー列 + 主キー）。
            ScanMask: bool array
            /// 子行から外部キーの値を取り出す位置。
            ChildPositions: int list
        }

    /// 大文字小文字を無視した列名リストの一致。
    let private sameNames (a: string list) (b: string list) =
        a.Length = b.Length && List.forall2 SqlName.equal a b

    /// 列名のリストを列定義へ解決する（上から順）。
    let private columnsOf (columns: ColumnDefinition list) (names: string list) =
        names
        |> List.map (fun name ->
            match columns |> List.tryFind (fun col -> SqlName.equal col.Name name) with
            | Some col -> ret col
            | None -> err (DbError.name (sprintf "列 %s が定義されていません" name)))
        |> all

    /// 主キー列名（キーの符号化順 = 列の定義順）。
    let private primaryKeyNames (schema: TableSchema) =
        TableSchema.primaryKeyColumns schema |> List.ofArray

    /// `CREATE TABLE` の外部キーを検証し、カタログへ保存する形にする。
    /// 参照先の列は省略時には親の主キーへ解決し、指定時も主キーと同順であることを要求する。
    let resolveCreatedTable (catalog: Catalog) (schema: TableSchema) (c: CreateTable) =
        let references =
            (c.Columns
             |> List.choose (fun col -> col.Constraint.References |> Option.map (fun r -> [ col.Name ], r)))
            @ (c.Constraints
               |> List.choose (fun tc ->
                   match tc with
                   | TableConstraint.ForeignKey(columns, r) -> Some(columns, r)
                   | _ -> None))

        let resolveOne (columns: string list, r: ForeignKeyRef) =
            result {
                do!
                    if List.isEmpty columns then
                        err (DbError.constraintViolation "FOREIGN KEY の列が空です")
                    else
                        ret ()

                match SqlName.duplicate columns with
                | Some name -> return! err (DbError.name (sprintf "FOREIGN KEY の列 %s が重複しています" name))
                | None -> ()

                let! childColumns = columnsOf c.Columns columns

                let! parent =
                    if Catalog.eqName r.ParentTable schema.Name then
                        ret schema
                    else
                        match Catalog.findTable r.ParentTable catalog with
                        | Some defn -> Catalog.toSchema defn
                        | None -> err (DbError.name (sprintf "参照先テーブル %s が存在しません" r.ParentTable))

                let parentKey = primaryKeyNames parent

                let! resolvedKey =
                    match r.ParentColumns with
                    | None -> ret parentKey
                    | Some names when sameNames names parentKey -> ret parentKey
                    | Some names ->
                        err (
                            DbError.constraintViolation (
                                sprintf
                                    "参照先の列（%s）はテーブル %s の主キー（%s）と同順で指定してください"
                                    (System.String.Join(", ", names))
                                    parent.Name
                                    (System.String.Join(", ", parentKey))
                            )
                        )

                do!
                    if columns.Length = resolvedKey.Length then
                        ret ()
                    else
                        err (
                            DbError.constraintViolation (
                                sprintf "FOREIGN KEY の列数(%d)と参照先の主キー列数(%d)が一致しません" columns.Length resolvedKey.Length
                            )
                        )

                let! parentColumns = columnsOf parent.Columns resolvedKey

                do!
                    List.zip childColumns parentColumns
                    |> List.map (fun (childCol, parentCol) ->
                        if childCol.DataType = parentCol.DataType then
                            ret ()
                        else
                            err (
                                DbError.typeError (
                                    sprintf
                                        "外部キーの列 %s の型 %s が参照先 %s.%s の型 %s と一致しません"
                                        childCol.Name
                                        (DataType.toSqlName childCol.DataType)
                                        parent.Name
                                        parentCol.Name
                                        (DataType.toSqlName parentCol.DataType)
                                )
                            ))
                    |> all
                    |> Result.map ignore

                return
                    {
                        Columns = columns
                        ParentTable = r.ParentTable
                        ParentColumns = resolvedKey
                    }
            }

        references |> List.map resolveOne |> all

    /// 列位置のリストから行の値を取り出す。
    /// 列マスクで復号されていない列があれば InternalError（呼び出し側の mask 漏れ）。
    let private valuesAt (positions: int list) (row: SqlValue array) =
        let values = positions |> List.map (fun i -> row.[i]) |> List.toArray

        if
            values
            |> Array.exists (function
                | SqlValue.Unloaded -> true
                | _ -> false)
        then
            err (DbError.internalError "外部キーの列が復号されていません")
        else
            ret values

    let private parentSchemaOf (catalog: Catalog) (table: string) =
        match Catalog.findTable table catalog with
        | Some defn -> Catalog.toSchema defn
        | None -> err (DbError.name (sprintf "参照先テーブル %s が存在しません" table))

    /// 子行（INSERT / UPDATE）の外部キー検査を準備する。
    /// カタログに無い（単体テストの）スキーマは検査なし。
    /// 親テーブルや列が欠けたカタログは、行を読む前に `NameError` にする（fail fast）。
    let prepareChildChecks (catalog: Catalog) (schema: TableSchema) =
        match Catalog.findTable schema.Name catalog with
        | None -> ret []
        | Some defn ->
            defn.ForeignKeys
            |> List.map (fun fk ->
                result {
                    let! childPositions =
                        fk.Columns |> List.map (TableSchema.requireIndexOf schema) |> all

                    let! parent = parentSchemaOf catalog fk.ParentTable

                    return
                        {
                            Definition = fk
                            ChildPositions = childPositions
                            Parent = parent
                            ProbeMask = Array.create parent.Columns.Length false
                        }
                })
            |> all

    /// 準備済みの検査で 1行を検査する。
    /// 外部キー列に NULL を含む行は参照しない（MATCH SIMPLE）。
    let checkChildRow
        (store: RowStore)
        (visible: uint64 option)
        (txn: Transaction)
        (schema: TableSchema)
        (checks: ChildCheck list)
        (row: SqlValue array)
        =
        checks
        |> List.map (fun check ->
            result {
                let! values = valuesAt check.ChildPositions row

                if values |> Array.exists SqlValue.isNull then
                    return ()
                else
                    let! key = KeyBuilder.ofValues values

                    let! found =
                        Dml.getByPrimaryKeyBytesWithTxnMask store check.Parent key visible txn check.ProbeMask

                    if found.IsSome then
                        return ()
                    else
                        let fk = check.Definition

                        return!
                            sprintf
                                "外部キー制約に違反: %s(%s) が参照する %s(%s) の行がありません"
                                schema.Name
                                (System.String.Join(", ", fk.Columns))
                                fk.ParentTable
                                (System.String.Join(", ", fk.ParentColumns))
                            |> DbError.constraintViolation
                            |> err
            })
        |> all
        |> Result.map ignore

    /// 親を参照している子の検査を準備する。
    /// 削除行ごとにカタログを引き直さないよう、文の実行前に 1回だけ呼ぶ。
    let prepareParentDeleteChecks (catalog: Catalog) (schema: TableSchema) =
        Catalog.referencingForeignKeys schema.Name catalog
        |> List.map (fun (childDefn, fk) ->
            result {
                let! child =
                    Catalog.toSchema childDefn
                    |> Result.map (fun s ->
                        { s with
                            Indexes = Catalog.indexSchemasOf childDefn.Name catalog
                        })

                let! parentPositions =
                    fk.ParentColumns |> List.map (TableSchema.requireIndexOf schema) |> all

                let! childPositions =
                    fk.Columns |> List.map (TableSchema.requireIndexOf child) |> all

                let index =
                    child.Indexes
                    |> List.tryFind (fun idx -> sameNames (idx.Columns |> List.truncate fk.Columns.Length) fk.Columns)

                let scanMask =
                    let mask = Array.create child.Columns.Length false

                    for p in childPositions @ child.PrimaryKey do
                        if p >= 0 && p < mask.Length then
                            mask.[p] <- true

                    mask

                return
                    {
                        Child = child
                        ParentColumns = fk.ParentColumns
                        ParentPositions = parentPositions
                        Index = index
                        PrimaryKeyMatches = sameNames (primaryKeyNames child) fk.Columns
                        ProbeMask = Array.create child.Columns.Length false
                        ScanMask = scanMask
                        ChildPositions = childPositions
                    }
            })
        |> all

    /// Op の行を先頭から調べ、`matches` が真になった時点で打ち切る。
    /// Op は one-shot なので途中で捨てた走査は Scope の DisposeAll で閉じる。
    let private anyRow (matches: SqlValue array -> DbResult<bool>) (op: Op) =
        let rec loop current =
            match Op.next current with
            | OpStep.OpError e -> Error e
            | OpStep.OpDone -> Ok false
            | OpStep.OpRow(row, next) ->
                match matches row with
                | DbErr e -> Error e
                | DbOk true -> Ok true
                | DbOk false -> loop next

        loop op

    let private sameValue (a: SqlValue) (b: SqlValue) =
        match SqlValue.compareValuesV a b with
        | DbOk 0 -> true
        | _ -> false

    let private rowMatches (positions: int list) (values: SqlValue array) (row: SqlValue array) =
        positions
        |> List.mapi (fun i p -> sameValue row.[p] values.[i])
        |> List.forall id

    /// 子テーブルに、外部キー列が `values` と一致する行があるか。
    let private childRowExists
        (store: RowStore)
        (check: ParentDeleteCheck)
        (values: SqlValue array)
        (visible: uint64 option)
        (scope: ScanScope)
        (txn: Transaction)
        =
        result {
            match check.Index with
            | Some idx ->
                let! op =
                    Dml.scanByIndexPrefixWithTxnMask
                        store
                        check.Child
                        idx
                        values
                        1
                        visible
                        (Some scope)
                        txn
                        check.ProbeMask

                return! anyRow (fun _ -> DbOk true) op
            | None ->
                if check.PrimaryKeyMatches then
                    let! op =
                        Dml.scanByPrimaryKeyRangeWithTxnMask
                            store
                            check.Child
                            values
                            (Some values)
                            1
                            visible
                            (Some scope)
                            txn
                            check.ProbeMask

                    return! anyRow (fun _ -> DbOk true) op
                else
                    let! op =
                        Dml.scanByPrimaryKeyWithTxnMask store check.Child 0 visible (Some scope) txn check.ScanMask

                    return! anyRow (fun row -> DbOk(rowMatches check.ChildPositions values row)) op
        }

    /// 親行を削除してよいか（RESTRICT）。
    /// 参照している子行があれば `ConstraintViolation`。
    /// `checks` は `prepareParentDeleteChecks` の結果（文単位で 1回だけ用意する）。
    let checkParentDelete
        (store: RowStore)
        (visible: uint64 option)
        (scope: ScanScope)
        (txn: Transaction)
        (schema: TableSchema)
        (checks: ParentDeleteCheck list)
        (row: SqlValue array)
        =
        checks
        |> List.map (fun check ->
            result {
                let! values = valuesAt check.ParentPositions row

                if values |> Array.exists SqlValue.isNull then
                    return ()
                else
                    let! exists = childRowExists store check values visible scope txn

                    if exists then
                        return!
                            sprintf
                                "外部キー制約に違反: テーブル %s の行が %s(%s) を参照しています"
                                check.Child.Name
                                schema.Name
                                (System.String.Join(", ", check.ParentColumns))
                            |> DbError.constraintViolation
                            |> err
                    else
                        return ()
            })
        |> all
        |> Result.map ignore
