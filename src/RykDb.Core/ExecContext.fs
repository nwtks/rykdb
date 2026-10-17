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
    }

module internal ExecContext =
    let create (store: RowStore) (visible: uint64 option) =
        {
            Store = store
            Visible = visible
            Schemas = Map.empty
            Catalog = Catalog.empty
        }

    /// スキーマを登録する（別名つきで）。
    let withSchema (ctx: ExecContext) (alias: string) (schema: TableSchema) =
        { ctx with
            Schemas = Map.add alias schema ctx.Schemas
        }

    /// カタログを差し替える（Plan の実行時に索引定義を引くため）。
    let withCatalog (ctx: ExecContext) (catalog: Catalog) = { ctx with Catalog = catalog }

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
    let table (ctx: ExecContext) (table: string) =
        result {
            let! schema = schemaOf ctx table
            let! rows = Dml.scanByPrimaryKey ctx.Store schema ctx.Visible 0
            return Op.ofArray rows
        }

    /// 索引で一致する全行を引く。
    /// 索引は一意とは限らないので、`WHERE indexed_col = v` が 1行になるとは限らない。
    /// `v` を持つ行をすべて返す。
    let byIndex (ctx: ExecContext) (table: string) (indexName: string) (keyValues: SqlValue array) =
        result {
            let! schema = schemaOf ctx table

            match schema.Indexes |> List.tryFind (fun i -> i.Name = indexName) with
            | None -> return! err (sprintf "テーブル %s に索引 %s がありません" table indexName |> DbError.name)
            | Some idx ->
                let! rows =
                    Dml.scanByIndexPrefix ctx.Store idx ctx.Visible keyValues System.Int32.MaxValue

                return Op.ofArray rows
        }

    /// 主キーで 1行を引く。
    let byPrimaryKey (ctx: ExecContext) (table: string) (key: SqlValue array) =
        result {
            let! schema = schemaOf ctx table
            let! row = Dml.getByPrimaryKey ctx.Store schema ctx.Visible key

            match row with
            | None -> return Op.empty
            | Some r -> return Op.ofRow r
        }
