namespace RykDb.Core

/// INNER 結合区間の並べ替え戦略。
/// `Greedy` は診断・A/B 用で、既定は `Dp`。
/// `Bushy` は1段の分割計画も試す。
[<RequireQualifiedAccess>]
type internal JoinOrderStrategy =
    | Dp
    | Greedy
    | Bushy

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
        /// 結合ノード（`Logical.Join`）の実行プラン（`Optimizer.JoinFrame`）。
        /// 型をこのファイルより前には置けないので `obj` で包む。
        /// `Optimizer.prepareContext` が計画時に埋め、`buildJoinFrame` が再利用する。
        JoinPlans: System.Collections.Immutable.ImmutableDictionary<obj, obj>
        /// 演算子が使ってよいメモリ作業領域とスピル先。
        /// クエリ開始時に 1回だけ決まる。
        Budget: MemoryBudget
        /// 結合順の戦略（A/B・診断用。既定は DP）。
        JoinOrder: JoinOrderStrategy
        /// EXPLAIN ANALYZE の実測行数（`Logical` のノード参照 -> 出力行数）。
        /// None のときは計測しない（通常の実行経路）。
        /// クエリごとに作る可変の台帳で、`Optimizer.execWithCounts` だけが入れる。
        RowCounts: System.Collections.Generic.Dictionary<obj, int64> option
    }

module internal ExecContext =
    /// ストアと可視 SeqNo から実行文脈を作る。
    let create (store: RowStore) (visible: uint64 option) =
        {
            Store = store
            Visible = visible
            Schemas = Map.empty
            Catalog = Catalog.Empty
            Txn = Transaction.create 0UL<txnId> { SeqNo = 0UL }
            Scope = ScanScope()
            ScanNeeds = System.Collections.Immutable.ImmutableDictionary.Create<obj, bool array> HashIdentity.Reference
            JoinPlans = System.Collections.Immutable.ImmutableDictionary.Create<obj, obj> HashIdentity.Reference
            Budget = MemoryBudget.Default
            JoinOrder = JoinOrderStrategy.Dp
            RowCounts = None
        }

    /// 結合順の戦略を差し替える（A/B・診断用）。
    let withJoinOrder (strategy: JoinOrderStrategy) (ctx: ExecContext) = { ctx with JoinOrder = strategy }

    /// メモリ作業領域の上限を差し替える。
    let withBudget (budget: MemoryBudget) (ctx: ExecContext) = { ctx with Budget = budget }

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
                match Catalog.findTable name ctx.Catalog with
                | Some t -> Catalog.toSchema t
                | None -> err (sprintf "テーブル '%s' が存在しません" name |> DbError.name)

    /// テーブルの統計を引く（別名はスキーマ経由で実テーブル名へ解決する）。
    let statsOf (name: string) (ctx: ExecContext) =
        match schemaOf name ctx with
        | Ok schema -> Catalog.findStats schema.Name ctx.Catalog
        | Error _ -> None

    /// テーブルを全走査する（列マスク付き）。
    let tableMasked (ctx: ExecContext) (table: string) (limit: int) (mask: bool array) =
        result {
            let! schema = schemaOf table ctx
            return! Dml.scanByPrimaryKeyWithTxnMask ctx.Store schema limit mask ctx.Visible (Some ctx.Scope) ctx.Txn
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
                    mask
                    ctx.Visible
                    (Some ctx.Scope)
                    ctx.Txn
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
                        mask
                        ctx.Visible
                        (Some ctx.Scope)
                        ctx.Txn
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
                        mask
                        ctx.Visible
                        (Some ctx.Scope)
                        ctx.Txn
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

    /// 索引 fetch で 1行の主キーを組み立てて本体を引く。
    let private fetchOneRow
        (ctx: ExecContext)
        (fetchMask: bool array)
        (schema: TableSchema)
        (pkPositions: int array)
        (row: SqlValue array)
        =
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
                Dml.getByPrimaryKeyBytesWithTxnMask ctx.Store schema keyBytes fetchMask ctx.Visible ctx.Txn
                |> Result.map (
                    Option.map (fun fetched ->
                        Array.init schema.Columns.Length (fun i ->
                            if i < fetchMask.Length && fetchMask.[i] then
                                fetched.[i]
                            else
                                row.[i]))
                )

    /// 索引 fetch の後続 Op を遅延して作る。
    [<TailCall>]
    let rec private fetchGo
        (ctx: ExecContext)
        (fetchMask: bool array)
        (schema: TableSchema)
        (pkPositions: int array)
        (op: Op)
        =
        Op.compute (fun () -> fetchAdvance ctx fetchMask schema pkPositions op)

    /// 索引 fetch の 1ステップを進める。
    and private fetchAdvance
        (ctx: ExecContext)
        (fetchMask: bool array)
        (schema: TableSchema)
        (pkPositions: int array)
        (current: Op)
        =
        match Op.next current with
        | OpStep.OpRow(row, next) ->
            match fetchOneRow ctx fetchMask schema pkPositions row with
            | Error e -> OpStep.OpError e
            | Ok None -> fetchAdvance ctx fetchMask schema pkPositions next
            | Ok(Some merged) -> OpStep.OpRow(merged, fetchGo ctx fetchMask schema pkPositions next)
        | x -> x

    /// 索引スキャンで被覆できなかった列を、同じスナップショットの主キー lookup で埋める。
    /// 表行が見つからない索引エントリ（防御）は行ごと捨てる。
    let fetchMissing (ctx: ExecContext) (table: string) (fetchMask: bool array) (op: Op) =
        result {
            let! schema = schemaOf table ctx
            let pkPositions = Array.ofList schema.PrimaryKey
            return fetchGo ctx fetchMask schema pkPositions op
        }

    /// 主キーで 1行を列マスク付きで引く。
    let byPrimaryKeyMasked (ctx: ExecContext) (table: string) (key: SqlValue array) (mask: bool array) =
        result {
            let! schema = schemaOf table ctx

            let! row =
                Dml.getByPrimaryKeyWithTxnMask ctx.Store schema key mask ctx.Visible ctx.Txn

            match row with
            | None -> return Op.Empty
            | Some r -> return Op.ofRow r
        }

// 書き込みは Transaction にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
