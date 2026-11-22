// ===========================================================================
// Catalog — スキーマ（テーブル・索引）の管理と永続化。
//
// 専用 keyspace __catalog__ に「1キー = 1定義」で構造化して保存する。
//   キー table:<name> -> テーブル定義
//   キー index:<name> -> 索引定義（属するテーブル名を含む）
// 既存の LSM（WAL / Compaction）をそのまま流用できる。
// カタログ内の名前比較は常に OrdinalIgnoreCase（SQL の識別子は大文字小文字を区別しない）。
// ===========================================================================

namespace RykDb.Core

/// 外部キーの定義（永続化の対象）。
/// 子テーブルが持つ（親の側は持たない）。
type internal ForeignKeyDefinition =
    {
        /// 子テーブル側の列名（宣言順）
        Columns: string list
        ParentTable: string
        /// 親テーブル側の列名（親の主キーと同順。作成時に解決済み）
        ParentColumns: string list
    }

/// テーブルの定義（永続化の対象）。
type internal TableDefinition =
    {
        Name: string
        Columns: ColumnDefinition list
        /// 主キーになる列名（位置ではなく名前を保持する。DDL 変更に強い）
        PrimaryKeyColumns: string list
        /// このテーブルが参照している外部キー
        ForeignKeys: ForeignKeyDefinition list
    }

/// 索引の定義（永続化の対象）。
type internal IndexDefinition =
    {
        IndexName: string
        TableName: string
        Columns: string list
        Unique: bool
    }

/// 列の統計。
type internal ColumnStats =
    {
        Name: string
        /// NULL を除いた異なり値数。
        NDV: int64
        Nulls: int64
        /// 数値型の最小値・最大値（数値以外は None）。
        Min: float option
        Max: float option
        /// 数値型の標本ヒストグラム（昇順の標本値。空なら未収集）。
        Histogram: float list
    }

/// 索引の統計。
type internal IndexStats =
    {
        Name: string
        Columns: string list
        /// 先頭 k 列の異なる組の数（k = 1..Columns.Length）。
        PrefixNDV: int64 list
    }

/// `ANALYZE` が収集するテーブル統計。
/// <remarks>
/// 統計は手動で更新する。INSERT / UPDATE / DELETE では自動更新しない。
/// </remarks>
type internal TableStats =
    {
        Table: string
        RowCount: int64
        Columns: ColumnStats list
        Indexes: IndexStats list
    }

/// カタログ全体のスナップショット。
type internal Catalog =
    {
        Tables: TableDefinition list
        Indexes: IndexDefinition list
        Stats: TableStats list
    }

module internal Catalog =
    let Empty =
        {
            Tables = []
            Indexes = []
            Stats = []
        }

    /// SQL の識別子は大文字小文字を区別しない（OrdinalIgnoreCase）。
    let eqName (a: string) (b: string) = SqlName.equal a b

    /// テーブルを探す。無ければ None。
    let findTable (name: string) (c: Catalog) =
        c.Tables |> List.tryFind (fun t -> eqName t.Name name)

    /// 索引を探す。無ければ None。
    let findIndex (name: string) (c: Catalog) =
        c.Indexes |> List.tryFind (fun i -> eqName i.IndexName name)

    /// あるテーブルが持つ索引を全部。
    let indexesOf (table: string) (c: Catalog) =
        c.Indexes |> List.filter (fun i -> eqName i.TableName table)

    /// 同名（大文字小文字を無視）の要素を除いてから末尾へ足す、共通の upsert。
    /// addTable / addIndex / addStats は名前の取り出し方だけが違う。
    let private upsert (nameOf: 'a -> string) (item: 'a) (items: 'a list) =
        items
        |> List.filter (fun x -> not (eqName (nameOf x) (nameOf item)))
        |> fun xs -> xs @ [ item ]

    /// テーブルの統計を探す。無ければ None。
    let findStats (table: string) (c: Catalog) =
        c.Stats |> List.tryFind (fun s -> eqName s.Table table)

    /// テーブルの統計を追加（同名があれば置き換える）。
    let addStats (s: TableStats) (c: Catalog) =
        { c with
            Stats = upsert _.Table s c.Stats
        }

    /// テーブルの統計を削除する。
    let removeStats (table: string) (c: Catalog) =
        { c with
            Stats = c.Stats |> List.filter (fun s -> not (eqName s.Table table))
        }

    /// 列の統計を探す。
    let findColumnStats (table: string) (column: string) (c: Catalog) =
        findStats table c
        |> Option.bind (fun s -> s.Columns |> List.tryFind (fun col -> eqName col.Name column))

    /// 索引の統計を探す。
    let findIndexStats (table: string) (index: string) (c: Catalog) =
        findStats table c
        |> Option.bind (fun s -> s.Indexes |> List.tryFind (fun idx -> eqName idx.Name index))

    /// テーブルを追加（同名があれば置き換える）。
    let addTable (t: TableDefinition) (c: Catalog) =
        { c with
            Tables = upsert _.Name t c.Tables
        }

    /// テーブルを削除する（そのテーブルの索引と統計も消える）。
    let removeTable (name: string) (c: Catalog) =
        { c with
            Tables = c.Tables |> List.filter (fun t -> not (eqName t.Name name))
            Indexes = c.Indexes |> List.filter (fun i -> not (eqName i.TableName name))
            Stats = c.Stats |> List.filter (fun s -> not (eqName s.Table name))
        }

    /// 索引を追加（同名があれば置き換える）。
    let addIndex (i: IndexDefinition) (c: Catalog) =
        { c with
            Indexes = upsert _.IndexName i c.Indexes
        }

    /// 索引を削除する。
    let removeIndex (name: string) (c: Catalog) =
        { c with
            Indexes = c.Indexes |> List.filter (fun i -> not (eqName i.IndexName name))
        }

    /// TableSchema を永続化形にする（外部キーなし）。
    let toDefinition (schema: TableSchema) =
        {
            Name = schema.Name
            Columns = schema.Columns
            PrimaryKeyColumns = TableSchema.primaryKeyColumns schema |> List.ofArray
            ForeignKeys = []
        }

    /// TableSchema と外部キー定義から永続化形を作る。
    let toDefinitionWithForeignKeys (schema: TableSchema) (foreignKeys: ForeignKeyDefinition list) =
        { toDefinition schema with
            ForeignKeys = foreignKeys
        }

    /// このテーブルを参照している外部キー（親側）を、子テーブルの定義と対にして返す。
    let referencingForeignKeys (table: string) (catalog: Catalog) =
        catalog.Tables
        |> List.collect (fun t ->
            t.ForeignKeys
            |> List.filter (fun fk -> eqName fk.ParentTable table)
            |> List.map (fun fk -> t, fk))

    /// 永続化形を TableSchema に戻す。
    let toSchema (t: TableDefinition) =
        let rec findIndices names acc =
            match names with
            | [] -> ret (List.rev acc)
            | name :: rest ->
                match t.Columns |> List.tryFindIndex (fun c -> eqName c.Name name) with
                | Some i -> findIndices rest (i :: acc)
                | None -> err (sprintf "テーブル %s に主キー列 %s がありません" t.Name name |> DbError.name)

        findIndices t.PrimaryKeyColumns []
        |> Result.map (fun idx ->
            {
                Name = t.Name
                Columns = t.Columns
                PrimaryKey = idx |> List.sort
                Indexes = []
            })

    /// 永続化形を IndexSchema に戻す。
    let toIndexSchema (i: IndexDefinition) =
        {
            Name = i.IndexName
            Columns = i.Columns
            Unique = i.Unique
        }

    /// UNIQUE 制約が自動作成する索引の名前。
    let implicitIndexName (table: string) (ordinal: int) = sprintf "__unique_%s_%d" table ordinal

    /// ユーザーが作成・削除できない予約索引名か。
    /// 暗黙の UNIQUE 索引を DROP して制約を消せないようにする。
    let isReservedIndexName (name: string) =
        name.StartsWith("__unique_", System.StringComparison.OrdinalIgnoreCase)

    /// カタログにある索引を実行用の IndexSchema に変換する。
    let indexSchemasOf (table: string) (catalog: Catalog) =
        indexesOf table catalog |> List.map toIndexSchema

    // -- 直列化 --------------------------------------------------------------
    //
    //   テーブル: [colCount:varint]
    //            col × [name:lenPrefixed][tag:1][flags:1][default:lenPrefixed]
    //            [pkCount:varint][name:lenPrefixed] × pkCount
    //            [fkCount:varint]
    //            fk × [colCount:varint][name:lenPrefixed] × colCount
    //                 [parentTable:lenPrefixed][parentColCount:varint]
    //                 [name:lenPrefixed] × parentColCount
    //
    //   索引    : [table:lenPrefixed][unique:1][colCount:varint]
    //            [name:lenPrefixed] × colCount
    //
    // flags: bit0 = NOT NULL / bit1 = PRIMARY KEY / bit2 = UNIQUE
    // default は長さ 0 が「DEFAULT なし」。
    // 外部キーを追加する前のテーブル定義は末尾に何も無いので、そこで読むのを止める。
    //

    let private encodeTable (t: TableDefinition) =
        let w0 = Writer.varint (uint64 t.Columns.Length) Writer.Empty

        let w2 =
            t.Columns
            |> List.fold
                (fun w c ->
                    let w = Writer.str c.Name w |> Writer.writeByte (DataType.toTag c.DataType)

                    let flags =
                        (if c.Constraint.NotNull then 1uy else 0uy)
                        ||| (if c.Constraint.PrimaryKey then 2uy else 0uy)
                        ||| if c.Constraint.Unique then 4uy else 0uy

                    let w = Writer.writeByte flags w

                    match c.Constraint.Default with
                    | None -> Writer.lengthPrefixed [||] w
                    | Some e -> Writer.str (Expr.toDisplayString e) w)
                w0
            |> Writer.varint (uint64 t.PrimaryKeyColumns.Length)

        let w4 =
            t.PrimaryKeyColumns
            |> List.fold (fun w name -> Writer.str name w) w2
            |> Writer.varint (uint64 t.ForeignKeys.Length)

        t.ForeignKeys
        |> List.fold
            (fun w fk ->
                let w = Writer.varint (uint64 fk.Columns.Length) w
                let w = fk.Columns |> List.fold (fun w name -> Writer.str name w) w
                let w = Writer.str fk.ParentTable w
                let w = Writer.varint (uint64 fk.ParentColumns.Length) w
                fk.ParentColumns |> List.fold (fun w name -> Writer.str name w) w)
            w4
        |> Writer.toArray

    let private readForeignKey (c: Cursor) =
        result {
            let! colCount, c1 = Reader.readVarint c
            let! cols, c2 = Reader.readItems Reader.readStr colCount c1
            let! parent, c3 = Reader.readStr c2
            let! parentCount, c4 = Reader.readVarint c3
            let! parentColumns, c5 = Reader.readItems Reader.readStr parentCount c4

            return
                {
                    Columns = cols
                    ParentTable = parent
                    ParentColumns = parentColumns
                },
                c5
        }

    let private decodeTable (name: string) (bytes: byte array) =
        let readColumn c =
            result {
                let! name, c1 = Reader.readStr c
                let! tag, c2 = Reader.readByte c1
                let! flags, c3 = Reader.readByte c2
                let! defaultText, c4 = Reader.readStr c3
                let! dt = DataType.fromTag tag

                let! defaultExpr =
                    if defaultText = "" then
                        ret None
                    else
                        match Parser.parseExpression defaultText with
                        | Ok e -> ret (Some e)
                        | Error e -> err (sprintf "DEFAULT 式を復元できません: %s" e.Message |> DbError.corruption)

                return
                    {
                        Name = name
                        DataType = dt
                        // 型引数は宣言のみで実行時の意味を持たないため保存しない。
                        TypeArguments = []
                        Constraint =
                            {
                                NotNull = flags &&& 1uy <> 0uy
                                PrimaryKey = flags &&& 2uy <> 0uy
                                Unique = flags &&& 4uy <> 0uy
                                Default = defaultExpr
                                References = None
                            }
                    },
                    c4
            }

        result {
            let! colCount, cur1 = Cursor.create bytes |> Reader.readVarint
            let! cols, cur2 = Reader.readItems readColumn colCount cur1
            let! pkCount, cur3 = Reader.readVarint cur2
            let! pks, cur4 = Reader.readItems Reader.readStr pkCount cur3

            let! foreignKeys =
                if Cursor.atEnd cur4 then
                    ret []
                else
                    Reader.readVarint cur4
                    >>= fun (fkCount, cur5) -> Reader.readItems readForeignKey fkCount cur5 |> Result.map fst

            return
                {
                    Name = name
                    Columns = cols
                    PrimaryKeyColumns = pks
                    ForeignKeys = foreignKeys
                }
        }

    let private encodeIndex (i: IndexDefinition) =
        let w2 =
            Writer.Empty
            |> Writer.str i.TableName
            |> Writer.bool i.Unique
            |> Writer.varint (uint64 i.Columns.Length)

        i.Columns |> List.fold (fun w c -> Writer.str c w) w2 |> Writer.toArray

    let private decodeIndex (name: string) (bytes: byte array) =
        result {
            let! table, cur1 = Cursor.create bytes |> Reader.readStr
            let! unique, cur2 = Reader.readBool cur1
            let! colCount, cur3 = Reader.readVarint cur2
            let! cols, _ = Reader.readItems Reader.readStr colCount cur3

            return
                {
                    IndexName = name
                    TableName = table
                    Columns = cols
                    Unique = unique
                }
        }

    let private writeFloat (v: float) =
        Writer.int64LE (System.BitConverter.DoubleToInt64Bits v)

    let private writeFloatOption (v: float option) (w: Writer) =
        match v with
        | None -> Writer.bool false w
        | Some x -> Writer.bool true w |> writeFloat x

    let private encodeStats (s: TableStats) =
        let w0 =
            Writer.Empty
            |> Writer.int64LE s.RowCount
            |> Writer.varint (uint64 s.Columns.Length)

        let w1 =
            s.Columns
            |> List.fold (fun w c -> w |> Writer.str c.Name |> Writer.int64LE c.NDV) w0
            |> Writer.varint (uint64 s.Indexes.Length)

        let w2 =
            s.Indexes
            |> List.fold
                (fun w i ->
                    let w = w |> Writer.str i.Name |> Writer.varint (uint64 i.Columns.Length)
                    i.Columns |> List.fold (fun w c -> Writer.str c w) w)
                w1

        let w3 =
            s.Columns
            |> List.fold
                (fun w c ->
                    let w =
                        w
                        |> Writer.int64LE c.Nulls
                        |> writeFloatOption c.Min
                        |> writeFloatOption c.Max
                        |> Writer.varint (uint64 c.Histogram.Length)

                    c.Histogram |> List.fold (fun w v -> writeFloat v w) w)
                (Writer.writeByte 1uy w2)

        s.Indexes
        |> List.fold
            (fun w i ->
                let w = Writer.varint (uint64 i.PrefixNDV.Length) w
                i.PrefixNDV |> List.fold (fun w n -> Writer.int64LE n w) w)
            w3
        |> Writer.toArray

    let private readColumnStat (c: Cursor) =
        result {
            let! name, c1 = Reader.readStr c
            let! ndv, c2 = Reader.readInt64LE c1
            return (name, ndv), c2
        }

    let private readIndexStat (c: Cursor) =
        result {
            let! name, c1 = Reader.readStr c
            let! count, c2 = Reader.readVarint c1
            let! columns, c3 = Reader.readItems Reader.readStr count c2
            return (name, columns), c3
        }

    let private readFloatOption (c: Cursor) =
        result {
            let! has, c1 = Reader.readBool c

            if not has then
                return None, c1
            else
                let! bits, c2 = Reader.readInt64LE c1
                return Some(System.BitConverter.Int64BitsToDouble bits), c2
        }

    let private readColumnExtras (c: Cursor) =
        result {
            let! nulls, c1 = Reader.readInt64LE c
            let! minValue, c2 = readFloatOption c1
            let! maxValue, c3 = readFloatOption c2
            let! histogramCount, c4 = Reader.readVarint c3
            let! bits, c5 = Reader.readItems Reader.readInt64LE histogramCount c4
            let histogram = bits |> List.map System.BitConverter.Int64BitsToDouble
            return (nulls, minValue, maxValue, histogram), c5
        }

    let private readIndexExtras (c: Cursor) =
        result {
            let! count, c1 = Reader.readVarint c
            let! ndvs, c2 = Reader.readItems Reader.readInt64LE count c1
            return ndvs, c2
        }

    let private decodeStats (table: string) (bytes: byte array) =
        result {
            let! rowCount, cur1 = Cursor.create bytes |> Reader.readInt64LE
            let! colCount, cur2 = Reader.readVarint cur1
            let! cols, cur3 = Reader.readItems readColumnStat colCount cur2

            let! indexDefs, afterIndexes =
                if Cursor.atEnd cur3 then
                    ret ([], cur3)
                else
                    Reader.readVarint cur3
                    >>= fun (count, cur4) ->
                        Reader.readItems readIndexStat count cur4
                        |> Result.map (fun (items, cur5) -> items, cur5)

            let columns =
                cols
                |> List.map (fun (name, ndv) ->
                    let column: ColumnStats =
                        {
                            Name = name
                            NDV = ndv
                            Nulls = 0L
                            Min = None
                            Max = None
                            Histogram = []
                        }

                    column)

            let indexes =
                indexDefs
                |> List.map (fun (name, columns) ->
                    let index: IndexStats =
                        {
                            Name = name
                            Columns = columns
                            PrefixNDV = []
                        }

                    index)

            if Cursor.atEnd afterIndexes then
                return
                    {
                        Table = table
                        RowCount = rowCount
                        Columns = columns
                        Indexes = indexes
                    }
            else
                let! tag, cur4 = Reader.readByte afterIndexes

                if tag <> 1uy then
                    return! err (sprintf "統計の形式が不明です: %d" tag |> DbError.corruption)
                else
                    let! columnExtras, cur5 =
                        Reader.readItems readColumnExtras (uint64 columns.Length) cur4

                    let! indexExtras, cur6 =
                        Reader.readItems readIndexExtras (uint64 indexes.Length) cur5

                    if not (Cursor.atEnd cur6) then
                        return! err (DbError.corruption "統計の末尾に余分なデータがあります")
                    else
                        let columns =
                            List.map2
                                (fun c (nulls, minValue, maxValue, histogram) ->
                                    { c with
                                        Nulls = nulls
                                        Min = minValue
                                        Max = maxValue
                                        Histogram = histogram
                                    })
                                columns
                                columnExtras

                        let indexes =
                            List.map2 (fun i prefix -> { i with PrefixNDV = prefix }) indexes indexExtras

                        return
                            {
                                Table = table
                                RowCount = rowCount
                                Columns = columns
                                Indexes = indexes
                            }
        }

    /// カタログのテーブル定義キー（`table:<name>`）。
    let private tableKey (name: string) =
        System.Text.Encoding.UTF8.GetBytes("table:" + name)

    /// カタログの索引定義キー（`index:<name>`）。
    let private indexKey (name: string) =
        System.Text.Encoding.UTF8.GetBytes("index:" + name)

    /// カタログの統計キー（`stats:<name>`）。
    let private statsKey (name: string) =
        System.Text.Encoding.UTF8.GetBytes("stats:" + name)

    let private toValue (bytes: byte array) = [| SqlValue.Blob bytes |]

    let private tableBatch (t: TableDefinition) (seq: uint64) =
        Keyspace.CatalogKeyspace, [| Entry.put (tableKey t.Name) seq (toValue (encodeTable t)) |]

    let private indexBatch (i: IndexDefinition) (seq: uint64) =
        Keyspace.CatalogKeyspace, [| Entry.put (indexKey i.IndexName) seq (toValue (encodeIndex i)) |]

    let private statsBatch (s: TableStats) (seq: uint64) =
        Keyspace.CatalogKeyspace, [| Entry.put (statsKey s.Table) seq (toValue (encodeStats s)) |]

    let private deleteBatch (key: byte array) (seq: uint64) =
        Keyspace.CatalogKeyspace, [| Entry.delete key seq |]

    let rec private numberList (mk: 'a -> uint64 -> 'b) (rest: 'a list) (seq: uint64) (acc: 'b list) =
        match rest with
        | [] -> List.rev acc, seq
        | x :: tail -> numberList mk tail (seq + 1UL) (mk x seq :: acc)

    /// テーブル定義を transaction に積む（`ALTER TABLE` が行の書き換えと原子的に commit する）。
    let putTableInTxn (t: TableDefinition) (txn: Transaction) =
        Transaction.put Keyspace.CatalogKeyspace (tableKey t.Name) (toValue (encodeTable t)) txn

    /// 索引定義を transaction に積む（列名の変更を 1 commit で反映する）。
    let putIndexInTxn (i: IndexDefinition) (txn: Transaction) =
        Transaction.put Keyspace.CatalogKeyspace (indexKey i.IndexName) (toValue (encodeIndex i)) txn

    /// テーブルの統計を transaction で削除する（`ALTER TABLE` で行の形が変わるとき）。
    let removeStatsInTxn (table: string) (txn: Transaction) =
        Transaction.remove Keyspace.CatalogKeyspace (statsKey table) txn

    /// カタログを保存する（消えた定義は tombstone で落とす）。
    /// previous は保存前のカタログ（store にあった内容）。
    /// 書き込みは「前回から変わった定義」と「消えた定義」の差分だけにする。
    /// 定義の比較は保存形（エンコード後）のバイト列で行う
    /// （`Expr` は構造比較できないため、カタログのレコードどうしは比べられない）。
    /// 書き込みは 1バッチにまとめる
    /// （tombstone も同じバッチに含めるので WAL の fsync は 1回で済む）。
    let save (store: RowStore) (previous: Catalog) (catalog: Catalog) =
        let bytesEqual (a: byte array) (b: byte array) =
            a.Length = b.Length && Array.forall2 (=) a b

        let changedTable (t: TableDefinition) =
            match previous.Tables |> List.tryFind (fun p -> eqName p.Name t.Name) with
            | Some p -> not (bytesEqual (encodeTable p) (encodeTable t))
            | None -> true

        let changedIndex (i: IndexDefinition) =
            match previous.Indexes |> List.tryFind (fun p -> eqName p.IndexName i.IndexName) with
            | Some p -> not (bytesEqual (encodeIndex p) (encodeIndex i))
            | None -> true

        let changedStats (s: TableStats) =
            match previous.Stats |> List.tryFind (fun p -> eqName p.Table s.Table) with
            | Some p -> not (bytesEqual (encodeStats p) (encodeStats s))
            | None -> true

        let staleKeys =
            let wanted =
                (catalog.Tables |> List.map (fun t -> tableKey t.Name))
                @ (catalog.Indexes |> List.map (fun i -> indexKey i.IndexName))
                @ (catalog.Stats |> List.map (fun s -> statsKey s.Table))

            let known =
                (previous.Tables |> List.map (fun t -> tableKey t.Name))
                @ (previous.Indexes |> List.map (fun i -> indexKey i.IndexName))
                @ (previous.Stats |> List.map (fun s -> statsKey s.Table))

            known
            |> List.filter (fun k -> not (wanted |> List.exists (fun w -> bytesEqual k w)))

        result {
            let! e = RowStore.ensure store
            let seq0 = e.NextSeqNo

            let tableBatches, seq1 =
                numberList tableBatch (catalog.Tables |> List.filter changedTable) seq0 []

            let indexBatches, seq2 =
                numberList indexBatch (catalog.Indexes |> List.filter changedIndex) seq1 []

            let statsBatches, seq3 =
                numberList statsBatch (catalog.Stats |> List.filter changedStats) seq2 []

            let deleteBatches, _ = numberList deleteBatch staleKeys seq3 []

            let! updated =
                RowStore.writeBatches (tableBatches @ indexBatches @ statsBatches @ deleteBatches) store

            return updated
        }

    /// カタログを読み込む。キーが無ければ空を返す。
    let load (store: RowStore) =
        let ofValue =
            function
            | [| SqlValue.Blob b |] -> Some b
            | [| SqlValue.Text t |] -> Some(System.Text.Encoding.UTF8.GetBytes t)
            | _ -> None

        let tryPrefix (key: byte array) (prefix: string) =
            let p = System.Text.Encoding.UTF8.GetBytes prefix

            if key.Length < p.Length then
                None
            elif Array.forall2 (=) key.[0 .. p.Length - 1] p then
                Some(System.Text.Encoding.UTF8.GetString(key, p.Length, key.Length - p.Length))
            else
                None

        let keyKindOf key =
            match tryPrefix key "table:" with
            | Some name -> Some(0, name)
            | None ->
                match tryPrefix key "index:" with
                | Some name -> Some(1, name)
                | None ->
                    match tryPrefix key "stats:" with
                    | Some name -> Some(2, name)
                    | None -> None

        let rec decodeRows (rows: (byte array * SqlValue array option) array) i c =
            if i >= rows.Length then
                ret c
            else
                let key, value = rows.[i]

                match value with
                | None -> decodeRows rows (i + 1) c
                | Some v ->
                    match ofValue v with
                    | None -> decodeRows rows (i + 1) c
                    | Some bytes ->
                        match keyKindOf key with
                        | Some(0, name) ->
                            match decodeTable name bytes with
                            | Ok t -> addTable t c |> decodeRows rows (i + 1)
                            | Error e -> err e
                        | Some(1, name) ->
                            match decodeIndex name bytes with
                            | Ok idx -> addIndex idx c |> decodeRows rows (i + 1)
                            | Error e -> err e
                        | Some(2, name) ->
                            match decodeStats name bytes with
                            | Ok s -> addStats s c |> decodeRows rows (i + 1)
                            | Error e -> err e
                        | _ -> decodeRows rows (i + 1) c

        RowStore.scan Keyspace.CatalogKeyspace [||] 0 store
        >>= fun rows -> SstScan.toArray rows
        >>= fun rows -> decodeRows (rows |> Array.map (fun e -> e.Key, e.Value)) 0 Empty

/// 出力の 1列。
type internal OutputColumn =
    {
        /// 表示名（別名があればそれ、無ければ列名）
        Name: string
        /// 修飾名（`t.id`）。修飾子付き参照の解決に使う。
        Qualifier: string option
        /// 型（式から導出できない場合は None）
        DataType: DataType option
    }

    member this.Display =
        match this.Qualifier with
        | Some q -> sprintf "%s.%s" q this.Name
        | None -> this.Name

module internal OutputColumn =
    /// テーブルの 1列から作る（修飾名はテーブル名か別名）。
    let ofTableColumn (table: string) (col: ColumnDefinition) =
        {
            Name = col.Name
            Qualifier = Some table
            DataType = Some col.DataType
        }

/// 演算子の出力スキーマ。
/// 列の解決と結果表示に使う。
type internal Schema =
    {
        Columns: OutputColumn array
        /// 参照可能なテーブル名（修飾子解決の候補）
        Tables: string list
    }

module internal Schema =
    let empty = { Columns = [||]; Tables = [] }

    /// 横連結（JOIN 用）。
    let concat (schema: Schema) (other: Schema) =
        if Array.isEmpty schema.Columns then
            if List.isEmpty schema.Tables then
                other
            else
                { other with
                    Tables = schema.Tables @ other.Tables
                }
        elif Array.isEmpty other.Columns then
            { schema with
                Tables = schema.Tables @ other.Tables
            }
        else
            {
                Columns = Array.append schema.Columns other.Columns
                Tables = schema.Tables @ other.Tables
            }

    /// スキーマ列を左から連結する（FROM / JOIN の出力スキーマ組み立て）。
    let concatAll = List.fold concat empty

    /// `*` / `t.*` が表す出力列を展開する。
    /// どちらもなければ None。
    /// <remarks>
    /// ヘッダ行（projectColumns）と射影の式（Ops.project）が同じ列を共有する。
    /// どちらかが先に変わると `SELECT *` の列数とヘッダがずれる（実際にずれた）。
    /// Expr は `[<NoComparison>]` なので「どちらの star だったか」は別のフラグで返す
    /// （qualified = `t.*` なら true）。
    /// </remarks>
    let expandStar (e: Expr) (schema: Schema) =
        match e with
        | Expr.Star -> Some(false, schema.Columns |> Array.toList)
        | Expr.QualifiedStar q ->
            Some(
                true,
                schema.Columns
                |> Array.filter (fun (c: OutputColumn) -> SqlName.equal (defaultArg c.Qualifier "") q)
                |> Array.toList
            )
        | _ -> None

    /// 指定した修飾子（テーブル名・別名）の列が 1つ以上あるか。
    /// 存在しない `t.*` を 0列の成功にしないための検証に使う。
    let hasQualifier (qualifier: string) (schema: Schema) =
        schema.Columns
        |> Array.exists (fun c ->
            match c.Qualifier with
            | Some q -> SqlName.equal q qualifier
            | None -> false)

    /// 式を「列の直接参照」に読み替える。
    /// Column はそのまま。修飾子なしの裸の名前（Name）も列として扱う
    /// （パーサが WHERE / ORDER BY の裸の名前を Name にするため）。
    /// 集約呼び出しや式は列ではないので None。
    let tryAsColumnRef =
        function
        | Expr.Column c -> Some c
        | Expr.Name n -> Some { Qualifier = None; Name = n }
        | _ -> None

    /// テーブルの全列を並べる。
    let ofTable (table: string) (alias: string option) (schema: TableSchema) =
        let name = defaultArg alias table

        let cols =
            schema.Columns
            |> List.map (fun c -> OutputColumn.ofTableColumn name c)
            |> List.toArray

        { Columns = cols; Tables = [ name ] }

    /// 列参照を解決する。見つからなければエラー、曖昧ならエラー。
    /// 大文字小文字を区別しない（`SqlName.equal` と同じ規則）。
    /// 行ごとに呼ばれるので、文字列の小文字化や一時配列を作らない。
    let resolve (r: ColumnRef) (schema: Schema) =
        let isMatch (c: OutputColumn) =
            match r.Qualifier with
            | Some q ->
                SqlName.equal c.Display r.Display
                || match c.Qualifier with
                   | Some cq -> SqlName.equal cq q && SqlName.equal c.Name r.Name
                   | None -> false
            | None -> SqlName.equal c.Name r.Name

        let rec collect i acc =
            if i >= schema.Columns.Length then
                List.rev acc
            elif isMatch schema.Columns.[i] then
                collect (i + 1) (i :: acc)
            else
                collect (i + 1) acc

        let hits = collect 0 []

        match hits with
        | [ i ] -> ret i
        | [] ->
            sprintf
                "列 '%s' が存在しません（使える列: %s）"
                r.Display
                (if schema.Columns.Length = 0 then
                     "(なし)"
                 else
                     schema.Columns |> Array.map (fun c -> c.Display) |> String.concat ", ")
            |> DbError.name
            |> err
        | many ->
            sprintf "列 '%s' が曖昧です（該当: %s）" r.Display (many |> List.map string |> String.concat ", ")
            |> DbError.name
            |> err

    /// 射影後の列（名前・修飾子・型）を作る。
    /// `SELECT *` は入力の列名をそのまま、`t.*` は修飾つきの表示名にする（元の挙動）。
    /// `SELECT id` は式を表示し、別名があればそれを使う。
    /// <remarks>
    /// 入力スキーマのヘッダをそのまま返すと `SELECT id` が `people.id` になるので、
    /// 射影後の名前を組み立て直す必要がある。
    /// 型は「直接の列参照」だけ引き継ぐ（式の型推論はしない）。
    /// 派生テーブル（FROM のサブクエリ）の列型が None だとハッシュ結合のキーにできないため、
    /// `SELECT id, v AS x FROM t` や `SELECT *` のような形では型を残す。
    /// </remarks>
    let internal projectedColumns (useDisplayName: bool) (schema: Schema) =
        let referenced e =
            match tryAsColumnRef e with
            | Some c ->
                match resolve c schema with
                | Ok i -> Some schema.Columns.[i]
                | Error _ -> None
            | None -> None

        let column name e =
            {
                Name = name
                Qualifier = None
                DataType = referenced e |> Option.bind (fun c -> c.DataType)
            }

        function
        | [] -> schema.Columns
        | items ->
            items
            |> List.collect (fun item ->
                match expandStar item.Expression schema with
                | Some(qualified, columns) ->
                    columns
                    |> List.map (fun c ->
                        {
                            Name = if qualified then c.Display else c.Name
                            Qualifier = None
                            DataType = c.DataType
                        })
                | None ->
                    match item.Alias with
                    | Some a -> [ column a item.Expression ]
                    | None ->
                        match if useDisplayName then None else referenced item.Expression with
                        | Some c ->
                            [
                                {
                                    Name = c.Name
                                    Qualifier = None
                                    DataType = c.DataType
                                }
                            ]
                        | None -> [ column (Expr.toDisplayString item.Expression) item.Expression ])
            |> List.toArray

    /// 射影後の列（結果表示用の名前）。
    let projectColumns = projectedColumns true

    /// 射影後の列（参照用の名前）。
    /// `SELECT t.id` の列名を `t.id` ではなく `id` にする。
    /// 派生テーブルの列名に使う（外側から `x.id` で引けるようにするため）。
    let referenceColumns = projectedColumns false

/// SELECT の射影（出力式とヘッダ）を 1回の展開でまとめたもの。
/// `Ops.project`（行の値）と `Plan` のヘッダが同じ展開用スキーマ（論理 = FROM 順）を共有し、
/// 「ヘッダの列数と行の幅がずれる」不変条件違反を防ぐ。
/// 行の評価は実レイアウトで行うため、`Ops.project` は展開用と評価用のスキーマを別に受け取る。
type internal SelectProjection =
    {
        SelectItems: SelectItem list
        /// `*` / `t.*` を展開した後の出力式（出力列と同じ順・同じ数）。
        Expressions: Expr list
        /// 結果表示用のヘッダ（`SELECT id` → `id`）。
        DisplayHeader: OutputColumn array
        /// 参照用のヘッダ（`SELECT t.id` → `id`）。派生テーブル用。
        ReferenceHeader: OutputColumn array
    }

module internal SelectProjection =
    let private expressionsOf (schema: Schema) =
        List.collect (fun item ->
            match Schema.expandStar item.Expression schema with
            | Some(_, columns) ->
                columns
                |> List.map (fun c ->
                    Expr.Column
                        {
                            Qualifier = c.Qualifier
                            Name = c.Name
                        })
            | None -> [ item.Expression ])

    /// 射影を組み立てる。
    /// 空の `items` は「入力スキーマをそのまま出力」を表す。
    let create (schema: Schema) (items: SelectItem list) =
        {
            SelectItems = items
            Expressions = expressionsOf schema items
            DisplayHeader = Schema.projectedColumns true schema items
            ReferenceHeader = Schema.projectedColumns false schema items
        }
