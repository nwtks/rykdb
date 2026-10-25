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
            Catalog = Catalog.empty
            Txn = Transaction.create 0UL { SeqNo = 0UL }
        }

    /// スキーマを登録する（別名つきで）。
    let withSchema (ctx: ExecContext) (alias: string) (schema: TableSchema) =
        { ctx with
            Schemas = Map.add alias schema ctx.Schemas
        }

    /// カタログを差し替える（Plan の実行時に索引定義を引くため）。
    let withCatalog (ctx: ExecContext) (catalog: Catalog) = { ctx with Catalog = catalog }

    /// 読み取りへ重ねる transaction を設定する。
    let withTransaction (ctx: ExecContext) (txn: Transaction) = { ctx with Txn = txn }

    /// テーブルのスキーマを引く。
    /// 別名で登録されていればそれを、無ければ実テーブル名で探す。
    let schemaOf (ctx: ExecContext) (name: string) =
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
    let table (ctx: ExecContext) (table: string) (limit: int) =
        result {
            let! schema = schemaOf ctx table
            let! rows = Dml.scanByPrimaryKeyWithTxn ctx.Store schema ctx.Visible ctx.Txn limit
            return Op.ofSeq rows
        }

    /// 主キーの範囲走査。
    let rangeScan
        (ctx: ExecContext)
        (table: string)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        =
        result {
            let! schema = schemaOf ctx table

            let! rows =
                Dml.scanByPrimaryKeyRangeWithTxn ctx.Store schema ctx.Visible ctx.Txn startKey endKey limit

            return Op.ofSeq rows
        }

    /// 索引で一致する全行を引く。
    /// 索引は一意とは限らないので、`WHERE indexed_col = v` が 1行になるとは限らない。
    /// `v` を持つ行をすべて返す。
    let byIndex (ctx: ExecContext) (table: string) (indexName: string) (keyValues: SqlValue array) (limit: int) =
        result {
            let! schema = schemaOf ctx table

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx ->
                let! rows =
                    Dml.scanByIndexPrefixWithTxn ctx.Store schema idx ctx.Visible ctx.Txn keyValues limit

                return Op.ofSeq rows
        }

    /// 索引の値が範囲 [lower, upper] に入る行を引く。
    let byIndexRange
        (ctx: ExecContext)
        (table: string)
        (indexName: string)
        (lower: SqlValue array)
        (upper: SqlValue array option)
        (limit: int)
        =
        result {
            let! schema = schemaOf ctx table

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx ->
                let! rows =
                    Dml.scanByIndexRangeWithTxn ctx.Store schema idx ctx.Visible ctx.Txn lower upper limit

                return Op.ofSeq rows
        }

    /// 主キーで 1行を引く。
    let byPrimaryKey (ctx: ExecContext) (table: string) (key: SqlValue array) =
        result {
            let! schema = schemaOf ctx table
            let! row = Dml.getByPrimaryKeyWithTxn ctx.Store schema ctx.Visible ctx.Txn key

            match row with
            | None -> return Op.empty
            | Some r -> return Op.ofRow r
        }
