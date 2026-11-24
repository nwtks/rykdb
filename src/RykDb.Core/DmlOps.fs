// ===========================================================================
// DmlOps — INSERT / UPDATE / DELETE の実装。
// ===========================================================================

namespace RykDb.Core

module internal DmlOps =
    /// WHERE を評価するときの対象スキーマ（UPDATE / DELETE で共通）。
    let private targetSchemaOf (schema: TableSchema) = Schema.ofTable schema.Name None schema

    /// WHERE の述語。
    /// WHERE が無ければ「全部通過」（UPDATE / DELETE で共通）。
    let private matchesWhere (resolve: Prepared) (row: SqlValue array) =
        function
        | Some w -> Eval.matchesWithV resolve row w
        | None -> DbOk true

    /// 値式を列位置へ対応付ける再帰本体（アキュムレータを明示する）。
    [<TailCall>]
    let rec private gatherWithLoop
        (evalOne: Expr -> DbResult<SqlValue>)
        (schema: TableSchema)
        (acc: (int * SqlValue) list)
        (vs: Expr list)
        (ixs: int list)
        =
        match vs, ixs with
        | [], [] -> DbOk acc
        | v :: rest, i :: restIdx ->
            match evalOne v with
            | DbErr e -> DbErr e
            | DbOk value ->
                let colDef = schema.Columns.[i]

                match SqlValue.coerce colDef.DataType value with
                | Error e -> DbErr e
                | Ok coerced -> gatherWithLoop evalOne schema ((i, coerced) :: acc) rest restIdx
        | _ -> DbOk acc

    /// 値式を列位置に対応付け、型強制して (位置, 値) のリストにする。
    /// UPDATE の現在行を使った評価を受け取る
    /// （INSERT は `evalValues` → `assembleRow` の別経路）。
    let private gatherWith
        (evalOne: Expr -> DbResult<SqlValue>)
        (schema: TableSchema)
        (values: Expr list)
        (idxs: int list)
        =
        gatherWithLoop evalOne schema [] values idxs

    /// gather で埋まった `(列位置, 値)` の対を、列順の行へ戻す。
    /// 埋まっていない位置は fallback に委ねる
    /// （INSERT は DEFAULT / NULL、UPDATE は元の値）。
    let private scatter (width: int) (filled: (int * SqlValue) list) (fallback: int -> SqlValue) =
        let values = Map.ofList filled

        Array.init width (fun i ->
            match Map.tryFind i values with
            | Some v -> v
            | None -> fallback i)

    /// 列リストを列位置へ解決する。
    /// 列リストが無ければテーブル定義の順に並べる。
    let private insertTargets (schema: TableSchema) =
        function
        | None -> schema.Columns |> List.mapi (fun i _ -> i) |> ret
        | Some names ->
            match SqlName.duplicate names with
            | Some name -> err (DbError.name (sprintf "INSERT の列 %s が重複しています" name))
            | None -> names |> List.map (TableSchema.requireIndexOf schema) |> all

    /// 列の既定値（DEFAULT が無ければ NULL）。
    let private evalDefaultValue (schema: TableSchema) (i: int) =
        let col = schema.Columns.[i]

        match col.Constraint.Default with
        | Some d ->
            Eval.eval Schema.Empty [||] d
            >>= fun value -> SqlValue.coerce col.DataType value
        | None -> ret SqlValue.Null

    /// 省略された列を埋める既定値の並び。
    /// 値が指定された列の位置は `assembleRow` が上書きするので NULL を置く。
    /// 指定された列の DEFAULT を評価しないのは、壊れた DEFAULT を持つ列へ明示値を入れるときに
    /// エラーにしないため。
    let private defaultValuesOf (schema: TableSchema) (supplied: Set<int>) =
        schema.Columns
        |> List.mapi (fun i _ ->
            if Set.contains i supplied then
                ret SqlValue.Null
            else
                evalDefaultValue schema i)
        |> all
        |> Result.map List.toArray

    /// INSERT の列数と値の数の不一致を表すエラーを作る。
    let private insertWidthMismatchOf (expected: int) (provided: int) =
        sprintf "INSERT の列数(%d)と値の数(%d)が合いません" expected provided
        |> DbError.create ErrorKind.SyntaxError

    /// 列位置・値配列から INSERT の列数不一致エラーを作る。
    let private insertWidthMismatch (idxs: int list) (values: SqlValue array) =
        insertWidthMismatchOf (List.length idxs) values.Length

    /// 提供値を列の型へ強制して (位置, 値) の対を集める。
    [<TailCall>]
    let rec private assembleGather
        (schema: TableSchema)
        (idxs: int list)
        (values: SqlValue array)
        (acc: (int * SqlValue) list)
        (vs: SqlValue list)
        (ixs: int list)
        =
        match vs, ixs with
        | [], [] -> ret acc
        | v :: rest, i :: restIdx ->
            match SqlValue.coerce schema.Columns.[i].DataType v with
            | Ok coerced -> assembleGather schema idxs values ((i, coerced) :: acc) rest restIdx
            | Error e -> err e
        | _ -> err (insertWidthMismatch idxs values)

    /// 提供された値（`idxs` の順）を列の型へ強制し、省略列を `defaults` で埋めて列順の行にする。
    let private assembleRow (schema: TableSchema) (idxs: int list) (defaults: SqlValue array) (values: SqlValue array) =
        if values.Length <> List.length idxs then
            err (insertWidthMismatch idxs values)
        else
            assembleGather schema idxs values [] (List.ofArray values) idxs
            |> Result.map (fun filled -> scatter schema.Columns.Length filled (fun i -> defaults.[i]))

    /// UNIQUE 索引の重複を検査する本体。
    [<TailCall>]
    let rec private checkUniqueIndexes
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (row: SqlValue array)
        (pkMask: bool array)
        (pk: SqlValue array)
        (pkBytes: byte array)
        =
        function
        | [] -> ret pkBytes
        | idx :: rest ->
            match RowKey.indexValues schema idx row |> Result.map List.toArray with
            | Ok values when values |> Array.exists SqlValue.isNull ->
                checkUniqueIndexes ctx schema txn row pkMask pk pkBytes rest
            | Ok values ->
                match
                    Dml.scanByIndexPrefixWithTxnMask
                        ctx.Store
                        schema
                        idx
                        values
                        1
                        ctx.Visible
                        (Some ctx.Scope)
                        txn
                        pkMask
                with
                | Ok scan ->
                    match Op.toArray scan with
                    | Ok rows ->
                        if
                            rows
                            |> Array.exists (fun r ->
                                let existingPk = TableSchema.primaryKeyOf schema r
                                not (Array.forall2 (=) existingPk pk))
                        then
                            err (sprintf "UNIQUE 制約に違反: %s" idx.Name |> DbError.constraintViolation)
                        else
                            checkUniqueIndexes ctx schema txn row pkMask pk pkBytes rest
                    | Error e -> err e
                | Error e -> err e
            | Error e -> err e

    /// 主キーと UNIQUE 索引の重複を検査し、確定した主キーのバイト列を返す。
    let private checkUnique (ctx: ExecContext) (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
        let existsMask = Array.create schema.Columns.Length false

        let pkMask =
            Array.init schema.Columns.Length (fun i -> List.contains i schema.PrimaryKey)

        let pk = TableSchema.primaryKeyOf schema row

        KeyBuilder.ofValues pk
        >>= fun pkBytes ->
            match Dml.getByPrimaryKeyBytesWithTxnMask ctx.Store schema pkBytes ctx.Visible txn existsMask with
            | Ok(Some _) -> err (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name |> DbError.constraintViolation)
            | Ok None ->
                checkUniqueIndexes ctx schema txn row pkMask pk pkBytes (schema.Indexes |> List.filter _.Unique)
            | Error e -> err e

    /// 1行を制約検査して transaction へ積む。
    /// 外部キーの検査は `insert` / `insertSelect` が文単位で準備したものを使う。
    /// <remarks>
    /// 外部キーは**行を積んだ後**の状態で検査する。
    /// 先に検査すると、自分自身を参照する行（例 `(1, 1)`）を入れられない。
    /// 失敗したら文全体が Error になり、バッファごと捨てられるので残留しない。
    /// </remarks>
    let private insertRow
        (ctx: ExecContext)
        (schema: TableSchema)
        (checks: ChildCheck list)
        (txn: Transaction)
        (row: SqlValue array)
        =
        checkUnique ctx schema txn row
        >>= fun pkBytes ->
            Dml.insertWithKey pkBytes txn schema row
            >>= fun next ->
                ForeignKey.checkChildRow ctx.Store ctx.Visible next schema checks row
                |> Result.map (fun () -> next)

    /// INSERT。
    /// columns が None ならテーブル定義の順に並べる。
    /// DEFAULT / NULL の充足と型強制はここで行う。
    /// TableSchema.validateRow が NOT NULL と列数を最終確認する（型は`SqlValue.coerce`で強制済み）。
    /// INSERT の値式を 1つずつ評価して列順の配列へ集める再帰本体。
    [<TailCall>]
    let rec private insertGather
        (schema: TableSchema)
        (evalConstant: Expr -> Result<SqlValue, DbError>)
        (idxs: int list)
        (values: Expr list)
        (acc: SqlValue list)
        (vs: Expr list)
        (ixs: int list)
        =
        match vs, ixs with
        | [], [] -> ret (List.rev acc |> List.toArray)
        | v :: rest, i :: restIdx ->
            let evaluated =
                match v with
                | Expr.Default -> evalDefaultValue schema i
                | _ -> evalConstant v

            match evaluated with
            | Ok value -> insertGather schema evalConstant idxs values (value :: acc) rest restIdx
            | Error e -> err e
        | _ -> err (insertWidthMismatchOf (List.length idxs) (List.length values))

    /// INSERT の値式を評価して列順の配列にする。
    let private insertEvalValues
        (schema: TableSchema)
        (evalConstant: Expr -> Result<SqlValue, DbError>)
        (idxs: int list)
        (values: Expr list)
        =
        if List.length values <> List.length idxs then
            err (insertWidthMismatchOf (List.length idxs) (List.length values))
        else
            insertGather schema evalConstant idxs values [] values idxs

    /// INSERT（VALUES）。
    /// 列リスト省略時は表の列順。
    let insert (ctx: ExecContext) (schema: TableSchema) (columns: string list option) (rows: Expr list list) =
        let evalConstant = Eval.eval Schema.Empty [||]

        result {
            let! idxs = insertTargets schema columns
            let! checks = ForeignKey.prepareChildChecks ctx.Catalog schema

            match columns, rows with
            | None, [ [] ] ->
                let! row =
                    schema.Columns
                    |> List.mapi (fun i _ -> evalDefaultValue schema i)
                    |> all
                    |> Result.map List.toArray

                let! txn = insertRow ctx schema checks ctx.Txn row
                return txn, 1
            | _ ->
                let! defaults = defaultValuesOf schema (Set.ofList idxs)

                return!
                    rows
                    |> foldOk
                        (fun (txnAcc, count) values ->
                            result {
                                let! provided = insertEvalValues schema evalConstant idxs values
                                let! row = assembleRow schema idxs defaults provided
                                let! txnAcc2 = insertRow ctx schema checks txnAcc row
                                return txnAcc2, count + 1
                            })
                        (ctx.Txn, 0)
        }

    /// INSERT ... SELECT。
    /// 選択結果の行（値）を 1行ずつ挿入する。
    /// 述語・結合・集約は `Plan.runSelect` が済ませているので、
    /// ここでは列の対応付け・型強制・制約検査だけを行う。
    /// 選択結果の `Op` は計画時点の snapshot と transaction を握っているため、
    /// 挿入した行を読み返して増え続けることはない（`INSERT INTO t SELECT * FROM t` も止まる）。
    /// INSERT ... SELECT の選択結果を 1行ずつ挿入する再帰本体。
    [<TailCall>]
    let rec private insertSelectGo
        (ctx: ExecContext)
        (schema: TableSchema)
        (idxs: int list)
        (defaults: SqlValue array)
        (checks: ChildCheck list)
        (txn: Transaction)
        (count: int)
        (cur: Op)
        =
        match Op.next cur with
        | OpStep.OpError e -> err e
        | OpStep.OpDone -> ret (txn, count)
        | OpStep.OpRow(row, nxt) ->
            match assembleRow schema idxs defaults row with
            | Ok values ->
                match insertRow ctx schema checks txn values with
                | Ok next -> insertSelectGo ctx schema idxs defaults checks next (count + 1) nxt
                | Error e -> err e
            | Error e -> err e

    /// INSERT ... SELECT。
    /// 選択結果の Op を行ごとに変換して挿入する。
    let insertSelect (ctx: ExecContext) (schema: TableSchema) (columns: string list option) (source: Op) =
        result {
            let! idxs = insertTargets schema columns
            let! defaults = defaultValuesOf schema (Set.ofList idxs)
            let! checks = ForeignKey.prepareChildChecks ctx.Catalog schema
            return! insertSelectGo ctx schema idxs defaults checks ctx.Txn 0 source
        }

    /// UPDATE の代入式を現在行で評価して新しい行を作る。
    let private updateAssign
        (resolve: Prepared)
        (schema: TableSchema)
        (assignExprs: Expr list)
        (targets: int list)
        (row: SqlValue array)
        =
        gatherWith (Eval.evalWithV resolve row) schema assignExprs targets
        |> DbResult.map (fun filled -> scatter row.Length filled (fun i -> row.[i]))

    /// 行のバイト数を見積もる。
    let private rowByteCount (row: SqlValue array) = int64 (ValueCodec.managedRowBytes row)

    /// 更新後の UNIQUE 索引に他行が残っていないか検査する。
    let private checkExistingUpdated
        (ctx: ExecContext)
        (schema: TableSchema)
        (updatedPks: Set<SqlValue array>)
        (txn: Transaction)
        (pkMask: bool array)
        (idx: IndexSchema)
        (values: SqlValue array)
        =
        result {
            let! scan =
                Dml.scanByIndexPrefixWithTxnMask ctx.Store schema idx values 0 ctx.Visible (Some ctx.Scope) txn pkMask

            let! rows = Op.toArray scan

            if
                rows
                |> Array.exists (fun row -> not (Set.contains (TableSchema.primaryKeyOf schema row) updatedPks))
            then
                return! err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
            else
                return ()
        }

    /// 位置列から索引キーのバイト列を作る（NULL は対象外）。
    let private encodeKeyValues positions (row: SqlValue array) =
        let values = positions |> List.map (fun i -> row.[i]) |> List.toArray

        if values |> Array.exists SqlValue.isNull then
            Ok None
        else
            KeyBuilder.ofValues values |> Result.map Some

    /// 1つの UNIQUE 索引について更新前後のキーを検査する。
    let private checkIndexUpdates
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (updatedPks: Set<SqlValue array>)
        (targetPositions: Set<int>)
        (pkMask: bool array)
        (updates: (SqlValue array * SqlValue array) list)
        (idx: IndexSchema)
        =
        result {
            let! positions = idx.Columns |> List.map (TableSchema.requireIndexOf schema) |> all

            if not (positions |> List.exists (fun p -> Set.contains p targetPositions)) then
                return ()
            else
                let! _, changed =
                    updates
                    |> foldOk
                        (fun (seen, changed) (oldRow, newRow) ->
                            result {
                                let! newKey = encodeKeyValues positions newRow

                                match newKey with
                                | None -> return seen, changed
                                | Some key ->
                                    if Set.contains key seen then
                                        return! err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                                    else
                                        let! oldKey = encodeKeyValues positions oldRow
                                        let changed = if oldKey = newKey then changed else newRow :: changed
                                        return Set.add key seen, changed
                            })
                        (Set.empty, [])

                return!
                    changed
                    |> List.fold
                        (fun acc newRow ->
                            acc
                            >>= fun () ->
                                let values = positions |> List.map (fun i -> newRow.[i]) |> List.toArray
                                checkExistingUpdated ctx schema updatedPks txn pkMask idx values)
                        (ret ())
        }

    /// 更新される全 UNIQUE 索引をまとめて検査する。
    let private validateUniqueUpdates
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (targets: int list)
        (updates: (SqlValue array * SqlValue array) list)
        =
        let updatedPks =
            updates
            |> List.map (fun (_, newRow) -> TableSchema.primaryKeyOf schema newRow)
            |> Set.ofList

        let targetPositions = Set.ofList targets

        let pkMask =
            Array.init schema.Columns.Length (fun i -> List.contains i schema.PrimaryKey)

        schema.Indexes
        |> List.filter _.Unique
        |> foldOk (fun () idx -> checkIndexUpdates ctx schema txn updatedPks targetPositions pkMask updates idx) ()

    /// UPDATE の対象行を走査して収集する（末尾再帰）。
    [<TailCall>]
    let rec private updateRowsGo
        (resolve: Prepared)
        (where: Expr option)
        (schema: TableSchema)
        (assignExprs: Expr list)
        (limitBytes: int64)
        (targets: int list)
        (acc: (SqlValue array * SqlValue array) list)
        (bytes: int64)
        (cur: Op)
        =
        match Op.next cur with
        | OpStep.OpError e -> err e
        | OpStep.OpDone -> ret (List.rev acc)
        | OpStep.OpRow(row, next) ->
            match matchesWhere resolve row where with
            | DbErr e -> err e
            | DbOk false -> updateRowsGo resolve where schema assignExprs limitBytes targets acc bytes next
            | DbOk true ->
                match updateAssign resolve schema assignExprs targets row with
                | DbErr e -> err e
                | DbOk newRow ->
                    let bytes = bytes + rowByteCount row + rowByteCount newRow + 24L

                    if bytes > limitBytes then
                        sprintf "UPDATE の対象行が transaction の上限（%d バイト）を超えました" limitBytes
                        |> DbError.capacity
                        |> err
                    else
                        updateRowsGo
                            resolve
                            where
                            schema
                            assignExprs
                            limitBytes
                            targets
                            ((row, newRow) :: acc)
                            bytes
                            next

    /// UPDATE の対象行を開いて収集する。
    let private collectUpdates
        (scan: bool array -> Result<Op, DbError>)
        (ctx: ExecContext)
        (schema: TableSchema)
        (resolve: Prepared)
        (assignExprs: Expr list)
        (where: Expr option)
        (targets: int list)
        =
        result {
            let! scan = scan (Array.create schema.Columns.Length true)
            return! updateRowsGo resolve where schema assignExprs ctx.Txn.LimitBytes targets [] 0L scan
        }

    /// UPDATE。
    /// assignments の右辺は現在行で評価する。
    /// 検索キーは更新前の行の主キー。
    /// 主キー列の変更は `Dml.updateRow` が ConstraintViolation で拒否する
    /// （DELETE + INSERT を使う）。
    /// `scan` は候補行を開く関数（列マスクを受ける）。
    /// WHERE は行ごとに再評価するので、候補が広めでも正しさは変わらない。
    let updateWith
        (scan: bool array -> Result<Op, DbError>)
        (ctx: ExecContext)
        (schema: TableSchema)
        (assignments: (string * Expr) list)
        (where: Expr option)
        =
        let targetSchema = targetSchemaOf schema
        let assignExprs = List.map snd assignments
        let resolve = Eval.prepare targetSchema (assignExprs @ Option.toList where)

        match assignments |> List.map fst |> SqlName.duplicate with
        | Some name -> err (DbError.name (sprintf "UPDATE の列 %s が重複しています" name))
        | None ->
            assignments
            |> List.map (fun (name, _) -> TableSchema.requireIndexOf schema name)
            |> all
            >>= fun targets ->
                result {
                    do! Eval.checkPrepared resolve

                    do!
                        assignExprs @ Option.toList where
                        |> foldOk (fun () e -> Eval.checkComparableTypes targetSchema e) ()

                    let! checks = ForeignKey.prepareChildChecks ctx.Catalog schema
                    let! updates = collectUpdates scan ctx schema resolve assignExprs where targets
                    do! validateUniqueUpdates ctx schema ctx.Txn targets updates

                    let! txn2 =
                        updates
                        |> foldOk
                            (fun current (oldRow, newRow) ->
                                Dml.updateRow current schema oldRow newRow
                                >>= fun next ->
                                    ForeignKey.checkChildRow ctx.Store ctx.Visible next schema checks newRow
                                    |> Result.map (fun () -> next))
                            ctx.Txn

                    return txn2, List.length updates
                }

    /// DELETE で読み直す必要のある列だけを残す列マスクを作る。
    let private deleteMaskFor
        (targetSchema: Schema)
        (schema: TableSchema)
        (where: Expr option)
        (checks: ParentDeleteCheck list)
        =
        let width = schema.Columns.Length
        let allColumns = Array.create width true
        let mask = Array.create width false

        for i in schema.PrimaryKey do
            if i >= 0 && i < width then
                mask.[i] <- true

        for idx in schema.Indexes do
            for name in idx.Columns do
                match TableSchema.indexOf schema name with
                | Some i when i >= 0 && i < width -> mask.[i] <- true
                | _ -> ()

        for check in checks do
            for i in check.ParentPositions do
                if i >= 0 && i < width then
                    mask.[i] <- true

        match where with
        | None -> mask
        | Some w when Expr.hasAnySubquery w -> allColumns
        | Some w ->
            let refs =
                Expr.fold
                    (fun acc x ->
                        match x with
                        | Expr.Column c -> c :: acc
                        | Expr.Name n -> { Qualifier = None; Name = n } :: acc
                        | _ -> acc)
                    []
                    w

            let resolved =
                refs
                |> List.map (fun r ->
                    match Schema.resolve r targetSchema with
                    | Ok i -> Some i
                    | Error _ ->
                        match Schema.resolve { Qualifier = None; Name = r.Name } targetSchema with
                        | Ok i -> Some i
                        | Error _ -> None)

            if List.exists Option.isNone resolved then
                allColumns
            else
                for i in resolved |> List.choose id do
                    if i >= 0 && i < width then
                        mask.[i] <- true

                mask

    /// DELETE の候補行を畳み込む再帰本体。
    [<TailCall>]
    let rec private deleteGo
        (scan: 'a)
        (mask: 'b)
        (matched: SqlValue array -> DbResult<bool>)
        (onRow: 'c -> 'd -> SqlValue array -> Result<('c * 'd), DbError>)
        (t: 'c)
        (c: 'd)
        (cur: Op)
        =
        match Op.next cur with
        | OpStep.OpError e -> err e
        | OpStep.OpDone -> ret (t, c)
        | OpStep.OpRow(row, nxt) ->
            match matched row with
            | DbErr e -> err e
            | DbOk false -> deleteGo scan mask matched onRow t c nxt
            | DbOk true ->
                match onRow t c row with
                | Ok(tc, c') -> deleteGo scan mask matched onRow tc c' nxt
                | Error e -> err e

    /// DELETE の候補行を開いて畳み込む。
    let private foldDeleteRows
        (scan: 'a -> Result<Op, DbError>)
        (ctx: ExecContext)
        (mask: 'a)
        (matched: SqlValue array -> DbResult<bool>)
        (onRow: Transaction -> int -> SqlValue array -> Result<(Transaction * int), DbError>)
        =
        result {
            let! scan = scan mask
            return! deleteGo scan mask matched onRow ctx.Txn 0 scan
        }

    /// DELETE。
    /// WHERE に合う行を主キー + 全索引に tombstone で消す。
    /// `scan` は候補行を開く関数（列マスクを受ける）。
    let deleteWith
        (scan: bool array -> Result<Op, DbError>)
        (ctx: ExecContext)
        (schema: TableSchema)
        (where: Expr option)
        =
        let targetSchema = targetSchemaOf schema
        let resolve = Eval.prepare targetSchema (Option.toList where)

        result {
            do! Eval.checkPrepared resolve

            do!
                where
                |> Option.toList
                |> foldOk (fun () e -> Eval.checkComparableTypes targetSchema e) ()

            let! checks = ForeignKey.prepareParentDeleteChecks ctx.Catalog schema
            let mask = deleteMaskFor targetSchema schema where checks

            return!
                foldDeleteRows scan ctx mask (fun row -> matchesWhere resolve row where) (fun t count row ->
                    ForeignKey.checkParentDelete ctx.Store ctx.Visible ctx.Scope t schema checks row
                    >>= fun () -> Dml.deleteRow t schema row |> Result.map (fun t2 -> t2, count + 1))
        }
