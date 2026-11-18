namespace RykDb.Core

/// 実行時文脈。
/// 演算子はこれを「作る時点」に確定させる。
type internal ExecContext =
    {
        Store: RowStore
        /// 読み取りの可視性境界。
        /// Op 生成時に 1回だけ決まる。
        Visible: uint64 option
        /// テーブル名（別名含む） -> スキーマ
        Schemas: Map<string, TableSchema>
        /// カタログ。
        /// 索引の定義を引くのに使う。
        Catalog: Catalog
        /// 読み取りに重ねる transaction のバッファ。
        /// read-your-writes を実現する。
        Txn: Transaction
        /// このクエリで開いた走査の台帳。
        /// 早期終了・エラーでも Engine が DisposeAll する。
        Scope: ScanScope
        /// 各 `Logical.Scan` が必要とする列（true = 復号する）。
        /// `Logical` はこのファイルより後に定義されるため、参照等価の `obj` キーで持つ。
        /// `Optimizer.exec` がクエリ開始時に構築する（クエリ中は不変）。
        ScanNeeds: System.Collections.Immutable.ImmutableDictionary<obj, bool array>
    }

module internal ExecContext =
    let create (store: RowStore) (visible: uint64 option) =
        {
            Store = store
            Visible = visible
            Schemas = Map.empty
            Catalog = Catalog.Empty
            Txn = Transaction.create 0UL<txnId> { SeqNo = 0UL }
            Scope = ScanScope()
            ScanNeeds = System.Collections.Immutable.ImmutableDictionary.Create<obj, bool array> HashIdentity.Reference
        }

    /// スキーマを登録する（別名つきで）。
    let withSchema (alias: string) (schema: TableSchema) (ctx: ExecContext) =
        { ctx with
            Schemas = Map.add alias schema ctx.Schemas
        }

    /// カタログを差し替える（Plan の実行時に索引定義を引くため）。
    let withCatalog (catalog: Catalog) (ctx: ExecContext) = { ctx with Catalog = catalog }

    /// 読み取りへ重ねる transaction を設定する。
    let withTransaction (txn: Transaction) (ctx: ExecContext) = { ctx with Txn = txn }

    /// テーブルのスキーマを引く。
    /// 別名で登録されていればそれを、無ければ実テーブル名で探す。
    let schemaOf (name: string) (ctx: ExecContext) =
        match ctx.Schemas |> Map.tryFind name with
        | Some s -> ret s
        | None ->
            let aliasHit =
                ctx.Schemas
                |> Map.toList
                |> List.tryPick (fun (alias, _) -> if Catalog.eqName alias name then Some alias else None)

            match aliasHit with
            | Some key -> ret ctx.Schemas.[key]
            | None ->
                match Catalog.findTable ctx.Catalog name with
                | Some t -> Catalog.toSchema t
                | None -> err (sprintf "テーブル '%s' が存在しません" name |> DbError.name)

    /// テーブルを全走査する（列マスク付き）。
    let tableMasked (ctx: ExecContext) (table: string) (limit: int) (mask: bool array) =
        result {
            let! schema = schemaOf table ctx
            return! Dml.scanByPrimaryKeyWithTxnMask ctx.Store schema limit ctx.Visible (Some ctx.Scope) ctx.Txn mask
        }

    /// 主キーの範囲走査（列マスク付き）。
    let rangeScanMasked
        (ctx: ExecContext)
        (table: string)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        (mask: bool array)
        =
        result {
            let! schema = schemaOf table ctx

            return!
                Dml.scanByPrimaryKeyRangeWithTxnMask
                    ctx.Store
                    schema
                    startKey
                    endKey
                    limit
                    ctx.Visible
                    (Some ctx.Scope)
                    ctx.Txn
                    mask
        }

    /// 索引で一致する全行を引く（列マスク付き）。
    let byIndexMasked
        (ctx: ExecContext)
        (table: string)
        (indexName: string)
        (keyValues: SqlValue array)
        (limit: int)
        (mask: bool array)
        =
        result {
            let! schema = schemaOf table ctx

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx ->
                return!
                    Dml.scanByIndexPrefixWithTxnMask
                        ctx.Store
                        schema
                        idx
                        keyValues
                        limit
                        ctx.Visible
                        (Some ctx.Scope)
                        ctx.Txn
                        mask
        }

    /// 索引の値が範囲 [lower, upper] に入る行を引く（列マスク付き）。
    let byIndexRangeMasked
        (ctx: ExecContext)
        (table: string)
        (indexName: string)
        (lower: SqlValue array)
        (upper: SqlValue array option)
        (limit: int)
        (mask: bool array)
        =
        result {
            let! schema = schemaOf table ctx

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx ->
                return!
                    Dml.scanByIndexRangeWithTxnMask
                        ctx.Store
                        schema
                        idx
                        lower
                        upper
                        limit
                        ctx.Visible
                        (Some ctx.Scope)
                        ctx.Txn
                        mask
        }

    /// 索引スキャン用の mask を計算する。
    /// 返り値は (スキャン mask, 本体から fetch する列の mask)。
    let indexAccessMasks (ctx: ExecContext) (table: string) (indexName: string) (mask: bool array) =
        result {
            let! schema = schemaOf table ctx

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx -> return! Dml.indexAccessMasks schema idx mask
        }

    /// 索引スキャンで被覆できなかった列を、同じスナップショットの主キー lookup で埋める。
    /// 表行が見つからない索引エントリ（防御）は行ごと捨てる。
    let fetchMissing (ctx: ExecContext) (table: string) (fetchMask: bool array) (op: Op) =
        let fetchOne schema pkPositions (row: SqlValue array) =
            let pk = pkPositions |> Array.map (fun i -> row.[i])

            if
                pk
                |> Array.exists (function
                    | SqlValue.Unloaded -> true
                    | _ -> false)
            then
                err (DbError.internalError "索引 fetch: 主キーが復号されていません")
            else
                KeyBuilder.ofValues pk
                >>= fun keyBytes ->
                    Dml.getByPrimaryKeyBytesWithTxnMask ctx.Store schema keyBytes ctx.Visible ctx.Txn fetchMask
                    |> Result.map (
                        Option.map (fun fetched ->
                            Array.init schema.Columns.Length (fun i ->
                                if i < fetchMask.Length && fetchMask.[i] then
                                    fetched.[i]
                                else
                                    row.[i]))
                    )

        let rec go schema pkPositions op =
            Op.compute (fun () -> advance schema pkPositions op)

        and advance schema pkPositions current =
            match Op.next current with
            | OpStep.OpRow(row, next) ->
                match fetchOne schema pkPositions row with
                | Error e -> OpStep.OpError e
                | Ok None -> advance schema pkPositions next
                | Ok(Some merged) -> OpStep.OpRow(merged, go schema pkPositions next)
            | x -> x

        result {
            let! schema = schemaOf table ctx
            let pkPositions = Array.ofList schema.PrimaryKey
            return go schema pkPositions op
        }

    /// 主キーで 1行を列マスク付きで引く。
    let byPrimaryKeyMasked (ctx: ExecContext) (table: string) (key: SqlValue array) (mask: bool array) =
        result {
            let! schema = schemaOf table ctx

            let! row =
                Dml.getByPrimaryKeyWithTxnMask ctx.Store schema key ctx.Visible ctx.Txn mask

            match row with
            | None -> return Op.Empty
            | Some r -> return Op.ofRow r
        }

// 書き込みは Transaction にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
module internal DmlOps =
    /// WHERE を評価するときの対象スキーマ（UPDATE / DELETE で共通）。
    let private targetSchemaOf (schema: TableSchema) = Schema.ofTable schema.Name None schema

    /// WHERE の述語。
    /// WHERE が無ければ「全部通過」（UPDATE / DELETE で共通）。
    let private matchesWhere (resolve: Prepared) (row: SqlValue array) =
        function
        | Some w -> Eval.matchesWithV resolve row w
        | None -> DbOk true

    /// 値式を列位置に対応付け、型強制して (位置, 値) のリストにする。
    /// UPDATE の現在行を使った評価を受け取る
    /// （INSERT は `evalValues` → `assembleRow` の別経路）。
    let private gatherWith
        (evalOne: Expr -> DbResult<SqlValue>)
        (schema: TableSchema)
        (values: Expr list)
        (idxs: int list)
        =
        let rec gather acc vs ixs =
            match vs, ixs with
            | [], [] -> DbOk acc
            | v :: rest, i :: restIdx ->
                match evalOne v with
                | DbErr e -> DbErr e
                | DbOk value ->
                    let colDef = schema.Columns.[i]

                    match SqlValue.coerce colDef.DataType value with
                    | Error e -> DbErr e
                    | Ok coerced -> gather ((i, coerced) :: acc) rest restIdx
            | _ -> DbOk acc

        gather [] values idxs

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
            Eval.eval Schema.empty [||] d
            >>= fun value -> SqlValue.coerce col.DataType value
        | None -> ret SqlValue.Null

    /// 省略された列を埋める既定値の並び。
    /// 値が指定された列の位置は `assembleRow` が上書きするので NULL を置く。
    /// 指定された列の DEFAULT を評価しないのは、壊れた DEFAULT を持つ列へ明示値を入れる
    /// ときにエラーにしないため（gotchas §11.21）。
    let private defaultValuesOf (schema: TableSchema) (supplied: Set<int>) =
        schema.Columns
        |> List.mapi (fun i _ ->
            if Set.contains i supplied then
                ret SqlValue.Null
            else
                evalDefaultValue schema i)
        |> all
        |> Result.map List.toArray

    /// 提供された値（`idxs` の順）を列の型へ強制し、省略列を `defaults` で埋めて列順の行にする。
    let private assembleRow (schema: TableSchema) (idxs: int list) (defaults: SqlValue array) (values: SqlValue array) =
        let widthMismatch () =
            sprintf "INSERT の列数(%d)と値の数(%d)が合いません" (List.length idxs) values.Length
            |> DbError.create ErrorKind.SyntaxError

        let rec gather acc vs ixs =
            match vs, ixs with
            | [], [] -> ret acc
            | v :: rest, i :: restIdx ->
                match SqlValue.coerce schema.Columns.[i].DataType v with
                | Error e -> err e
                | Ok coerced -> gather ((i, coerced) :: acc) rest restIdx
            | _ -> err (widthMismatch ())

        if values.Length <> List.length idxs then
            err (widthMismatch ())
        else
            gather [] (List.ofArray values) idxs
            |> Result.map (fun filled -> scatter schema.Columns.Length filled (fun i -> defaults.[i]))

    /// 索引列の値を取り出す（索引列は CREATE INDEX で検証済み）。
    let private indexValuesOf (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.map (TableSchema.requireIndexOf schema)
        |> all
        |> Result.map (fun positions -> positions |> List.map (fun i -> row.[i]) |> List.toArray)

    /// 主キーと UNIQUE 索引の重複を検査し、確定した主キーのバイト列を返す。
    let private checkUnique (ctx: ExecContext) (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
        let existsMask = Array.create schema.Columns.Length false

        let pkMask =
            Array.init schema.Columns.Length (fun i -> List.contains i schema.PrimaryKey)

        let rec check pk pkBytes =
            function
            | [] -> ret pkBytes
            | idx :: rest ->
                match indexValuesOf schema idx row with
                | Error e -> err e
                | Ok values when values |> Array.exists SqlValue.isNull -> check pk pkBytes rest
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
                    | Error e -> err e
                    | Ok scan ->
                        match Op.toArray scan with
                        | Error e -> err e
                        | Ok rows ->
                            if
                                rows
                                |> Array.exists (fun r ->
                                    let existingPk = TableSchema.primaryKeyOf schema r
                                    not (Array.forall2 (=) existingPk pk))
                            then
                                err (sprintf "UNIQUE 制約に違反: %s" idx.Name |> DbError.constraintViolation)
                            else
                                check pk pkBytes rest

        let pk = TableSchema.primaryKeyOf schema row

        match KeyBuilder.ofValues pk with
        | Error e -> err e
        | Ok pkBytes ->
            match Dml.getByPrimaryKeyBytesWithTxnMask ctx.Store schema pkBytes ctx.Visible txn existsMask with
            | Error e -> err e
            | Ok(Some _) -> err (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name |> DbError.constraintViolation)
            | Ok None -> check pk pkBytes (schema.Indexes |> List.filter _.Unique)

    /// 1行を制約検査して transaction へ積む。
    let private insertRow (ctx: ExecContext) (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
        checkUnique ctx schema txn row
        >>= fun pkBytes -> Dml.insertWithKey pkBytes txn schema row

    /// INSERT。
    /// columns が None ならテーブル定義の順に並べる。
    /// DEFAULT / NULL の充足と型強制はここで行う。
    /// TableSchema.validateRow が NOT NULL と型を最終確認する。
    let insert (ctx: ExecContext) (schema: TableSchema) (columns: string list option) (rows: Expr list list) =
        let evalConstant = Eval.eval Schema.empty [||]

        let widthMismatch expected provided =
            sprintf "INSERT の列数(%d)と値の数(%d)が合いません" expected provided
            |> DbError.create ErrorKind.SyntaxError

        let evalValues idxs values =
            let rec gather acc vs ixs =
                match vs, ixs with
                | [], [] -> ret (List.rev acc |> List.toArray)
                | v :: rest, i :: restIdx ->
                    let evaluated =
                        match v with
                        | Expr.Default -> evalDefaultValue schema i
                        | _ -> evalConstant v

                    match evaluated with
                    | Error e -> err e
                    | Ok value -> gather (value :: acc) rest restIdx
                | _ -> err (widthMismatch (List.length idxs) (List.length values))

            if List.length values <> List.length idxs then
                err (widthMismatch (List.length idxs) (List.length values))
            else
                gather [] values idxs

        result {
            let! idxs = insertTargets schema columns

            match columns, rows with
            | None, [ [] ] ->
                let! row =
                    schema.Columns
                    |> List.mapi (fun i _ -> evalDefaultValue schema i)
                    |> all
                    |> Result.map List.toArray

                let! txn = insertRow ctx schema ctx.Txn row
                return txn, 1
            | _ ->
                let! defaults = defaultValuesOf schema (Set.ofList idxs)

                return!
                    rows
                    |> foldOk
                        (fun (txnAcc, count) values ->
                            result {
                                let! provided = evalValues idxs values
                                let! row = assembleRow schema idxs defaults provided
                                let! txnAcc2 = insertRow ctx schema txnAcc row
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
    let insertSelect (ctx: ExecContext) (schema: TableSchema) (columns: string list option) (source: Op) =
        let rec go idxs defaults txn count cur =
            match Op.next cur with
            | OpStep.OpError e -> err e
            | OpStep.OpDone -> ret (txn, count)
            | OpStep.OpRow(row, nxt) ->
                match assembleRow schema idxs defaults row with
                | Error e -> err e
                | Ok values ->
                    match insertRow ctx schema txn values with
                    | Error e -> err e
                    | Ok next -> go idxs defaults next (count + 1) nxt

        result {
            let! idxs = insertTargets schema columns
            let! defaults = defaultValuesOf schema (Set.ofList idxs)
            return! go idxs defaults ctx.Txn 0 source
        }

    /// UPDATE。
    /// assignments の右辺は現在行で評価する。
    /// 検索キーは更新前の行の主キー。
    /// 主キー列の変更は `Dml.updateRow` が ConstraintViolation で拒否する
    /// （DELETE + INSERT を使う）。
    let update (ctx: ExecContext) (schema: TableSchema) (assignments: (string * Expr) list) (where: Expr option) =
        let targetSchema = targetSchemaOf schema
        let assignExprs = List.map snd assignments
        let resolve = Eval.prepare targetSchema (assignExprs @ Option.toList where)

        let assign targets row =
            gatherWith (Eval.evalWithV resolve row) schema assignExprs targets
            |> DbResult.map (fun filled -> scatter row.Length filled (fun i -> row.[i]))

        let rowBytes row = int64 (ValueCodec.managedRowBytes row)

        let rec go targets acc bytes cur =
            match Op.next cur with
            | OpStep.OpError e -> err e
            | OpStep.OpDone -> ret (List.rev acc)
            | OpStep.OpRow(row, next) ->
                match matchesWhere resolve row where with
                | DbErr e -> err e
                | DbOk false -> go targets acc bytes next
                | DbOk true ->
                    match assign targets row with
                    | DbErr e -> err e
                    | DbOk newRow ->
                        let bytes = bytes + rowBytes row + rowBytes newRow + 24L

                        if bytes > ctx.Txn.LimitBytes then
                            sprintf "UPDATE の対象行が transaction の上限（%d バイト）を超えました" ctx.Txn.LimitBytes
                            |> DbError.capacity
                            |> err
                        else
                            go targets ((row, newRow) :: acc) bytes next

        let collectUpdates txn targets =
            result {
                let! scan =
                    Dml.scanByPrimaryKeyWithTxnValues ctx.Store schema 0 ctx.Visible (Some ctx.Scope) txn true

                return! go targets [] 0L scan
            }

        let validateUniqueUpdates txn updates =
            let updatedPks =
                updates
                |> List.map (fun (_, newRow) -> TableSchema.primaryKeyOf schema newRow)
                |> Set.ofList

            let pkMask =
                Array.init schema.Columns.Length (fun i -> List.contains i schema.PrimaryKey)

            let checkExisting idx seen key values =
                result {
                    let! scan =
                        Dml.scanByIndexPrefixWithTxnMask
                            ctx.Store
                            schema
                            idx
                            values
                            0
                            ctx.Visible
                            (Some ctx.Scope)
                            txn
                            pkMask

                    let! rows = Op.toArray scan

                    if
                        rows
                        |> Array.exists (fun row ->
                            let existingPk = TableSchema.primaryKeyOf schema row
                            not (Set.contains existingPk updatedPks))
                    then
                        return! err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                    else
                        return Set.add key seen
                }

            let checkIndex (idx: IndexSchema) =
                result {
                    let! positions = idx.Columns |> List.map (TableSchema.requireIndexOf schema) |> all

                    let! _ =
                        updates
                        |> foldOk
                            (fun seen (_, newRow) ->
                                let values = positions |> List.map (fun i -> newRow.[i]) |> List.toArray

                                if values |> Array.exists SqlValue.isNull then
                                    ret seen
                                else
                                    match KeyBuilder.ofValues values with
                                    | Error e -> err e
                                    | Ok key ->
                                        if Set.contains key seen then
                                            err (DbError.constraintViolation (sprintf "UNIQUE 制約に違反: %s" idx.Name))
                                        else
                                            checkExisting idx seen key values)
                            Set.empty

                    return ()
                }

            schema.Indexes
            |> List.filter _.Unique
            |> foldOk (fun () idx -> checkIndex idx) ()

        match assignments |> List.map fst |> SqlName.duplicate with
        | Some name -> err (DbError.name (sprintf "UPDATE の列 %s が重複しています" name))
        | None ->
            assignments
            |> List.map (fun (name, _) -> TableSchema.requireIndexOf schema name)
            |> all
            >>= fun targets ->
                result {
                    let! updates = collectUpdates ctx.Txn targets
                    do! validateUniqueUpdates ctx.Txn updates

                    let! txn2 =
                        updates
                        |> foldOk (fun current (oldRow, newRow) -> Dml.updateRow current schema oldRow newRow) ctx.Txn

                    return txn2, List.length updates
                }

    /// DELETE。
    /// WHERE に合う行を主キー + 全索引に tombstone で消す。
    let delete (ctx: ExecContext) (schema: TableSchema) (where: Expr option) =
        let targetSchema = targetSchemaOf schema
        let resolve = Eval.prepare targetSchema (Option.toList where)

        let deleteMask =
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
                        match Schema.resolve targetSchema r with
                        | Ok i -> Some i
                        | Error _ ->
                            match Schema.resolve targetSchema { Qualifier = None; Name = r.Name } with
                            | Ok i -> Some i
                            | Error _ -> None)

                if List.exists Option.isNone resolved then
                    allColumns
                else
                    for i in resolved |> List.choose id do
                        if i >= 0 && i < width then
                            mask.[i] <- true

                    mask

        let foldRows ctx schema matched onRow =
            let rec go t c cur =
                match Op.next cur with
                | OpStep.OpError e -> err e
                | OpStep.OpDone -> ret (t, c)
                | OpStep.OpRow(row, nxt) ->
                    match matched row with
                    | DbErr e -> err e
                    | DbOk false -> go t c nxt
                    | DbOk true ->
                        match onRow t c row with
                        | Error e -> err e
                        | Ok(tc, c') -> go tc c' nxt

            result {
                let! scan =
                    Dml.scanByPrimaryKeyWithTxnMask ctx.Store schema 0 ctx.Visible (Some ctx.Scope) ctx.Txn deleteMask

                return! go ctx.Txn 0 scan
            }

        foldRows ctx schema (fun row -> matchesWhere resolve row where) (fun t count row ->
            Dml.deleteRow t schema row |> Result.map (fun t2 -> t2, count + 1))
