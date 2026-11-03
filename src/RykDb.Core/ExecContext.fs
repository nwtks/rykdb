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
    }

module internal ExecContext =
    let create (store: RowStore) (visible: uint64 option) =
        {
            Store = store
            Visible = visible
            Schemas = Map.empty
            Catalog = Catalog.Empty
            Txn = Transaction.create 0UL { SeqNo = 0UL }
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

    /// テーブルを全走査する（主キー昇順 = 挿入順ではないが LSM のキー順）。
    /// limit > 0 なら先頭 limit 件だけをストレージから引く。
    /// 返り値は遅延の `Op`（1行ずつ引くまでストレージを読まない）。
    let table (ctx: ExecContext) (table: string) (limit: int) =
        result {
            let! schema = schemaOf table ctx
            return! Dml.scanByPrimaryKeyWithTxn ctx.Store schema limit ctx.Visible ctx.Txn
        }

    /// 主キーの範囲走査（遅延の `Op`）。
    let rangeScan
        (ctx: ExecContext)
        (table: string)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        =
        result {
            let! schema = schemaOf table ctx
            return! Dml.scanByPrimaryKeyRangeWithTxn ctx.Store schema startKey endKey limit ctx.Visible ctx.Txn
        }

    /// 索引で一致する全行を引く（遅延の `Op`）。
    /// 索引は一意とは限らないので、`WHERE indexed_col = v` が 1行になるとは限らない。
    /// `v` を持つ行をすべて返す。
    let byIndex (ctx: ExecContext) (table: string) (indexName: string) (keyValues: SqlValue array) (limit: int) =
        result {
            let! schema = schemaOf table ctx

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx -> return! Dml.scanByIndexPrefixWithTxn ctx.Store schema idx keyValues limit ctx.Visible ctx.Txn
        }

    /// 索引の値が範囲 [lower, upper] に入る行を引く（遅延の `Op`）。
    let byIndexRange
        (ctx: ExecContext)
        (table: string)
        (indexName: string)
        (lower: SqlValue array)
        (upper: SqlValue array option)
        (limit: int)
        =
        result {
            let! schema = schemaOf table ctx

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx -> return! Dml.scanByIndexRangeWithTxn ctx.Store schema idx lower upper limit ctx.Visible ctx.Txn
        }

    /// 主キーで 1行を引く。
    let byPrimaryKey (ctx: ExecContext) (table: string) (key: SqlValue array) =
        result {
            let! schema = schemaOf table ctx
            let! row = Dml.getByPrimaryKeyWithTxn ctx.Store schema key ctx.Visible ctx.Txn

            match row with
            | None -> return Op.empty
            | Some r -> return Op.ofRow r
        }

// 書き込みは Transaction にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
module internal DmlOps =
    /// WHERE を評価するときの対象スキーマ（UPDATE / DELETE で共通）。
    let private targetSchemaOf (schema: TableSchema) = Schema.ofTable schema.Name None schema

    /// WHERE の述語。
    /// WHERE が無ければ「全部通過」（UPDATE / DELETE で共通）。
    let private matchesWhere (targetSchema: Schema) (row: SqlValue array) =
        function
        | Some w -> Eval.matches targetSchema row w
        | None -> ret true

    /// 値式を列位置に対応付け、型強制して (位置, 値) のリストにする。
    /// INSERT は行の外（定数）で、UPDATE は現在行を参照して評価するので、
    /// 評価器 evalOne を受け取って両方を同じ経路で処理する。
    let private gatherWith
        (evalOne: Expr -> Result<SqlValue, DbError>)
        (schema: TableSchema)
        (values: Expr list)
        (idxs: int list)
        =
        let rec gather acc vs is_ =
            match vs, is_ with
            | [], [] -> ret acc
            | v :: rest, i :: restIdx ->
                match evalOne v with
                | Error e -> err e
                | Ok value ->
                    let colDef = schema.Columns.[i]

                    match SqlValue.coerce colDef.DataType value with
                    | Error e -> err e
                    | Ok coerced -> gather ((i, coerced) :: acc) rest restIdx
            | _ -> ret acc

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

    /// INSERT。
    /// columns が None ならテーブル定義の順に並べる。
    /// DEFAULT / NULL の充足と型強制はここで行う。
    /// TableSchema.validateRow が NOT NULL と型を最終確認する。
    let insert (ctx: ExecContext) (schema: TableSchema) (columns: string list option) (rows: Expr list list) =
        let evalConstant = Eval.eval Schema.empty [||]

        let indexValues (index: IndexSchema) (row: SqlValue array) =
            index.Columns
            |> List.map (TableSchema.requireIndexOf schema)
            |> all
            |> Result.map (fun positions -> positions |> List.map (fun i -> row.[i]) |> List.toArray)

        let rec check txn row pk indexes =
            match indexes with
            | [] -> ret ()
            | idx :: rest ->
                match indexValues idx row with
                | Error e -> err e
                | Ok values when values |> Array.exists SqlValue.isNull -> check txn row pk rest
                | Ok values ->
                    match Dml.scanByIndexPrefixWithTxn ctx.Store schema idx values 1 ctx.Visible txn with
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
                                check txn row pk rest

        let checkUnique txn row =
            let pk = TableSchema.primaryKeyOf schema row

            match Dml.getByPrimaryKeyWithTxn ctx.Store schema pk ctx.Visible txn with
            | Error e -> err e
            | Ok(Some _) -> err (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name |> DbError.constraintViolation)
            | Ok None -> check txn row pk (schema.Indexes |> List.filter _.Unique)

        let evalDefault i =
            let col = schema.Columns.[i]

            match col.Constraint.Default with
            | Some d -> evalConstant d >>= fun value -> SqlValue.coerce col.DataType value
            | None -> ret SqlValue.Null

        let buildRow width idxs (defaults: SqlValue array) values =
            if List.length values <> List.length idxs then
                DbError.create
                    ErrorKind.SyntaxError
                    (sprintf "INSERT の列数(%d)と値の数(%d)が合いません" (List.length idxs) (List.length values))
                |> err
            else
                let rec gather acc vs is_ =
                    match vs, is_ with
                    | [], [] -> ret acc
                    | v :: rest, i :: restIdx ->
                        let evaluated =
                            match v with
                            | Expr.Default -> evalDefault i
                            | _ ->
                                evalConstant v
                                >>= fun value -> SqlValue.coerce schema.Columns.[i].DataType value

                        match evaluated with
                        | Error e -> err e
                        | Ok coerced -> gather ((i, coerced) :: acc) rest restIdx
                    | _ ->
                        sprintf "INSERT の列数(%d)と値の数(%d)が合いません" (List.length idxs) (List.length values)
                        |> DbError.create ErrorKind.SyntaxError
                        |> err

                gather [] values idxs
                |> Result.map (fun filled -> scatter width filled (fun i -> defaults.[i]))

        result {
            let! idxs =
                match columns with
                | None -> schema.Columns |> List.mapi (fun i _ -> i) |> ret
                | Some names ->
                    match SqlName.duplicate names with
                    | Some name -> err (DbError.name (sprintf "INSERT の列 %s が重複しています" name))
                    | None -> names |> List.map (TableSchema.requireIndexOf schema) |> all

            let supplied = Set.ofList idxs

            match columns, rows with
            | None, [ [] ] ->
                let! row =
                    schema.Columns
                    |> List.mapi (fun i _ -> evalDefault i)
                    |> all
                    |> Result.map List.toArray

                do! checkUnique ctx.Txn row
                let! t = Dml.insert ctx.Txn schema row
                return t, 1
            | _ ->
                let! defaultValues =
                    schema.Columns
                    |> List.mapi (fun i _ ->
                        if Set.contains i supplied then
                            ret SqlValue.Null
                        else
                            evalDefault i)
                    |> all

                let defaults = List.toArray defaultValues

                return!
                    rows
                    |> foldOk
                        (fun (txnAcc, count) values ->
                            result {
                                let! row = buildRow schema.Columns.Length idxs defaults values
                                do! checkUnique txnAcc row
                                let! t = Dml.insert txnAcc schema row
                                return t, count + 1
                            })
                        (ctx.Txn, 0)
        }

    /// UPDATE。
    /// assignments の右辺は現在行で評価する。
    /// 検索キーは更新前の行の主キー。
    /// 主キー列を書き換えると古い位置の行が残って重複する。
    let update (ctx: ExecContext) (schema: TableSchema) (assignments: (string * Expr) list) (where: Expr option) =
        let assign targetSchema targets row =
            gatherWith (Eval.eval targetSchema row) schema (List.map snd assignments) targets
            |> Result.map (fun filled -> scatter row.Length filled (fun i -> row.[i]))

        let rowBytes row = int64 (ValueCodec.managedRowBytes row)

        let targetSchema = targetSchemaOf schema

        let rec go targets acc bytes cur =
            match Op.next cur with
            | Error e -> err e
            | Ok None -> ret (List.rev acc)
            | Ok(Some(row, next)) ->
                match matchesWhere targetSchema row where with
                | Error e -> err e
                | Ok false -> go targets acc bytes next
                | Ok true ->
                    match assign targetSchema targets row with
                    | Error e -> err e
                    | Ok newRow ->
                        let bytes = bytes + rowBytes row + rowBytes newRow + 24L

                        if bytes > ctx.Txn.LimitBytes then
                            sprintf "UPDATE の対象行が transaction の上限（%d バイト）を超えました" ctx.Txn.LimitBytes
                            |> DbError.capacity
                            |> err
                        else
                            go targets ((row, newRow) :: acc) bytes next

        let collectUpdates txn targets =
            result {
                let! scan = Dml.scanByPrimaryKeyWithTxn ctx.Store schema 0 ctx.Visible txn
                return! go targets [] 0L scan
            }

        let validateUniqueUpdates txn updates =
            let updatedPks =
                updates
                |> List.map (fun (_, newRow) -> TableSchema.primaryKeyOf schema newRow)
                |> Set.ofList

            let checkExisting idx seen key values =
                result {
                    let! scan =
                        Dml.scanByIndexPrefixWithTxn ctx.Store schema idx values 0 ctx.Visible txn

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
                                    let key = KeyBuilder.ofValues values

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
        let foldRows ctx schema matched onRow =
            let rec go t c cur =
                match Op.next cur with
                | Error e -> err e
                | Ok None -> ret (t, c)
                | Ok(Some(row, nxt)) ->
                    match matched row with
                    | Error e -> err e
                    | Ok false -> go t c nxt
                    | Ok true ->
                        match onRow t c row with
                        | Error e -> err e
                        | Ok(tc, c') -> go tc c' nxt

            result {
                let! scan = Dml.scanByPrimaryKeyWithTxn ctx.Store schema 0 ctx.Visible ctx.Txn
                return! go ctx.Txn 0 scan
            }

        let targetSchema = targetSchemaOf schema

        foldRows ctx schema (fun row -> matchesWhere targetSchema row where) (fun t count row ->
            Dml.deleteRow t schema row |> Result.map (fun t2 -> t2, count + 1))
