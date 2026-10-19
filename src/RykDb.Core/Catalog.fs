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

/// テーブルの定義（永続化の対象）。
type internal TableDefinition =
    {
        Name: string
        Columns: ColumnDefinition list
        /// 主キーになる列名（位置ではなく名前で持つ。DDL 変更に強い）
        PrimaryKeyColumns: string list
    }

/// 索引の定義（永続化の対象）。
type internal IndexDefinition =
    {
        IndexName: string
        TableName: string
        Columns: string list
        Unique: bool
    }

/// カタログ全体のスナップショット。
type internal Catalog =
    {
        Tables: TableDefinition list
        Indexes: IndexDefinition list
    }

module internal Catalog =
    let empty = { Tables = []; Indexes = [] }

    let private keyspace = "__catalog__"

    /// SQL の識別子は大文字小文字を区別しない（OrdinalIgnoreCase）。
    let eqName (a: string) (b: string) =
        System.String.Equals(a, b, System.StringComparison.OrdinalIgnoreCase)

    let private tableKey (name: string) =
        System.Text.Encoding.UTF8.GetBytes("table:" + name)

    let private indexKey (name: string) =
        System.Text.Encoding.UTF8.GetBytes("index:" + name)

    /// テーブルを探す。無ければ None。
    let findTable (c: Catalog) (name: string) =
        c.Tables |> List.tryFind (fun t -> eqName t.Name name)

    /// 索引を探す。無ければ None。
    let findIndex (c: Catalog) (name: string) =
        c.Indexes |> List.tryFind (fun i -> eqName i.IndexName name)

    /// あるテーブルが持つ索引を全部。
    let indexesOf (c: Catalog) (table: string) =
        c.Indexes |> List.filter (fun i -> eqName i.TableName table)

    /// 同名（大文字小文字を無視）の要素を除いてから末尾へ足す、共通の upsert。
    /// addTable / addIndex は名前の取り出し方だけが違う。
    let private upsert (nameOf: 'a -> string) (item: 'a) (items: 'a list) =
        items
        |> List.filter (fun x -> not (eqName (nameOf x) (nameOf item)))
        |> fun xs -> xs @ [ item ]

    /// テーブルを追加（同名があれば置き換える）。
    let addTable (c: Catalog) (t: TableDefinition) =
        { c with
            Tables = upsert (fun (x: TableDefinition) -> x.Name) t c.Tables
        }

    /// 索引を追加（同名があれば置き換える）。
    let addIndex (c: Catalog) (i: IndexDefinition) =
        { c with
            Indexes = upsert (fun (x: IndexDefinition) -> x.IndexName) i c.Indexes
        }

    /// テーブルを削除する（そのテーブルの索引も消える）。
    let removeTable (c: Catalog) (name: string) =
        { c with
            Tables = c.Tables |> List.filter (fun t -> not (eqName t.Name name))
            Indexes = c.Indexes |> List.filter (fun i -> not (eqName i.TableName name))
        }

    /// 索引を削除する。
    let removeIndex (c: Catalog) (name: string) =
        { c with
            Indexes = c.Indexes |> List.filter (fun i -> not (eqName i.IndexName name))
        }

    /// TableSchema を永続化形にする。
    let toDefinition (schema: TableSchema) =
        {
            Name = schema.Name
            Columns = schema.Columns
            PrimaryKeyColumns = TableSchema.primaryKeyColumns schema |> List.ofArray
        }

    /// 永続化形を TableSchema に戻す。
    let toSchema (t: TableDefinition) =
        let rec findIndices (names: string list) (acc: int list) =
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

    /// IndexSchema を永続化形にする。
    let toIndexDefinition (table: string) (index: IndexSchema) =
        {
            IndexName = index.Name
            TableName = table
            Columns = index.Columns
            Unique = index.Unique
        }

    /// 永続化形を IndexSchema に戻す。
    let toIndexSchema (i: IndexDefinition) =
        {
            Name = i.IndexName
            Columns = i.Columns
            Unique = i.Unique
        }

    /// カタログにある索引を実行用の IndexSchema へ落とす。
    let indexSchemasOf (catalog: Catalog) (table: string) =
        indexesOf catalog table |> List.map toIndexSchema

    // -- 直列化 --------------------------------------------------------------
    //
    //   テーブル: [colCount:varint]
    //            col × [name:lenPrefixed][tag:1][flags:1][default:lenPrefixed]
    //            [pkCount:varint][name:lenPrefixed] × pkCount
    //
    //   索引    : [table:lenPrefixed][unique:1][colCount:varint]
    //            [name:lenPrefixed] × colCount
    //
    // flags: bit0 = NOT NULL / bit1 = PRIMARY KEY / bit2 = UNIQUE
    // default は長さ 0 が「DEFAULT なし」。
    //

    let private encodeTable (t: TableDefinition) =
        let w0 = Writer.varint (uint64 t.Columns.Length) Writer.empty

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

        t.PrimaryKeyColumns
        |> List.fold (fun w name -> Writer.str name w) w2
        |> Writer.toArray

    let private decodeTable (name: string) (bytes: byte array) =
        let readColumn (c: Cursor) =
            result {
                let! name, c1 = Reader.readStr c
                let! tag, c2 = Reader.readByte c1
                let! flags, c3 = Reader.readByte c2
                let! _, c4 = Reader.readLengthPrefixed c3
                let! dt = DataType.fromTag tag

                return
                    {
                        Name = name
                        DataType = dt
                        TypeArguments = []
                        Constraint =
                            {
                                NotNull = flags &&& 1uy <> 0uy
                                PrimaryKey = flags &&& 2uy <> 0uy
                                Unique = flags &&& 4uy <> 0uy
                                Default = None
                            }
                    },
                    c4
            }

        result {
            let! colCount, cur1 = Cursor.create bytes |> Reader.readVarint
            let! cols, cur2 = Reader.readItems colCount readColumn cur1
            let! pkCount, cur3 = Reader.readVarint cur2
            let! pks, _ = Reader.readItems pkCount Reader.readStr cur3

            return
                {
                    Name = name
                    Columns = cols
                    PrimaryKeyColumns = pks
                }
        }

    let private encodeIndex (i: IndexDefinition) =
        let w2 =
            Writer.empty
            |> Writer.str i.TableName
            |> Writer.bool i.Unique
            |> Writer.varint (uint64 i.Columns.Length)

        i.Columns |> List.fold (fun w c -> Writer.str c w) w2 |> Writer.toArray

    let private decodeIndex (name: string) (bytes: byte array) =
        result {
            let! table, cur1 = Cursor.create bytes |> Reader.readStr
            let! unique, cur2 = Reader.readBool cur1
            let! colCount, cur3 = Reader.readVarint cur2
            let! cols, _ = Reader.readItems colCount Reader.readStr cur3

            return
                {
                    IndexName = name
                    TableName = table
                    Columns = cols
                    Unique = unique
                }
        }

    let private keyKindOf (key: byte array) =
        let tryPrefix (prefix: string) =
            let p = System.Text.Encoding.UTF8.GetBytes prefix

            if key.Length < p.Length then
                None
            elif Array.forall2 (=) key.[0 .. p.Length - 1] p then
                Some(System.Text.Encoding.UTF8.GetString(key, p.Length, key.Length - p.Length))
            else
                None

        match tryPrefix "table:" with
        | Some name -> Some(false, name)
        | None ->
            match tryPrefix "index:" with
            | Some name -> Some(true, name)
            | None -> None

    /// カタログを読み込む。キーが無ければ空を返す。
    let load (store: RowStore) =
        let ofValue (v: SqlValue array) =
            match v with
            | [| SqlValue.Blob b |] -> Some b
            | [| SqlValue.Text t |] -> Some(System.Text.Encoding.UTF8.GetBytes t)
            | _ -> None

        let rec decodeRows (rest: (byte array * SqlValue array option) array) (acc: Catalog) =
            if Array.isEmpty rest then
                ret acc
            else
                let key, value = rest.[0]
                let tail = rest.[1..]

                match value with
                | None -> decodeRows tail acc
                | Some v ->
                    match ofValue v with
                    | None -> decodeRows tail acc
                    | Some bytes ->
                        match keyKindOf key with
                        | Some(false, name) ->
                            match decodeTable name bytes with
                            | Error e -> Error e
                            | Ok t -> decodeRows tail (addTable acc t)
                        | Some(true, name) ->
                            match decodeIndex name bytes with
                            | Error e -> Error e
                            | Ok i -> decodeRows tail (addIndex acc i)
                        | None -> decodeRows tail acc

        RowStore.scan store keyspace [||] 0 >>= fun rows -> decodeRows rows empty

    /// カタログを保存する（消えた定義は tombstone で落とす）。
    let save (store: RowStore) (catalog: Catalog) =
        let rec numberList (mk: 'a -> uint64 -> 'b) (rest: 'a list) (seq: uint64) (acc: 'b list) =
            match rest with
            | [] -> List.rev acc, seq
            | x :: tail -> numberList mk tail (seq + 1UL) (mk x seq :: acc)

        let toValue (bytes: byte array) = [| SqlValue.Blob bytes |]

        let tableBatch (t: TableDefinition) (seq: uint64) =
            keyspace, [| Entry.put (tableKey t.Name) seq (toValue (encodeTable t)) |]

        let indexBatch (i: IndexDefinition) (seq: uint64) =
            keyspace, [| Entry.put (indexKey i.IndexName) seq (toValue (encodeIndex i)) |]

        let bytesEqual (a: byte array) (b: byte array) =
            a.Length = b.Length && Array.forall2 (=) a b

        let isWanted (key: byte array) =
            match keyKindOf key with
            | Some(false, _) -> catalog.Tables |> List.exists (fun t -> bytesEqual key (tableKey t.Name))
            | Some(true, _) -> catalog.Indexes |> List.exists (fun i -> bytesEqual key (indexKey i.IndexName))
            | None -> false

        result {
            let! e = RowStore.engine store keyspace
            let seq0 = e.NextSeqNo
            let tableBatches, seq1 = numberList tableBatch catalog.Tables seq0 []
            let indexBatches, seq2 = numberList indexBatch catalog.Indexes seq1 []
            let! updated = RowStore.writeBatches store (tableBatches @ indexBatches)
            let! rows = RowStore.scan updated keyspace [||] 0

            let stale =
                rows
                |> Array.choose (fun (key, v) ->
                    match v with
                    | Some _ when isWanted key -> None
                    | Some _ -> Some key
                    | None -> None)
                |> Array.toList

            if List.isEmpty stale then
                return updated
            else
                let deletes, _ = numberList Entry.delete stale seq2 []
                return! RowStore.writeBatches updated [ (keyspace, Array.ofList deletes) ]
        }
