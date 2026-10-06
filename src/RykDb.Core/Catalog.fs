// ===========================================================================
// Catalog — スキーマ（テーブル・索引）の管理と永続化。
//
// 専用 keyspace `__catalog__` に「1 キー = 1 定義」で構造化して保存する。
//   キー `table:<name>` -> テーブル定義
//   キー `index:<name>` -> 索引定義（属するテーブル名を含む）
// 既存の LSM（WAL / Compaction）をそのまま流用できる。
// SQL テキストではなく構造化で保存する理由は docs/architecture.md §7.3。
// カタログ内の名前比較は常に OrdinalIgnoreCase（SQL の識別子は大文字小文字を区別しない）。
// ===========================================================================

namespace RykDb.Core

open System
open System.Text

// ---------------------------------------------------------------------------
// 定義
// ---------------------------------------------------------------------------

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

/// カタログ全体のスナップショット。不変。
type internal Catalog =
    {
        Tables: TableDefinition list
        Indexes: IndexDefinition list
    }

    static member empty = { Tables = []; Indexes = [] }

module internal Catalog =
    let private keyspace = "__catalog__"

    /// SQL の識別子は大文字小文字を区別しない（OrdinalIgnoreCase）。
    let eqName (a: string) (b: string) =
        String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let private tableKey (name: string) = Encoding.UTF8.GetBytes("table:" + name)

    let private indexKey (name: string) = Encoding.UTF8.GetBytes("index:" + name)

    /// テーブルを探す。無ければ None。
    let findTable (c: Catalog) (name: string) =
        c.Tables |> List.tryFind (fun t -> eqName t.Name name)

    /// 索引を探す。無ければ None。
    let findIndex (c: Catalog) (name: string) =
        c.Indexes |> List.tryFind (fun i -> eqName i.IndexName name)

    /// あるテーブルが持つ索引を全部。
    let indexesOf (c: Catalog) (table: string) =
        c.Indexes |> List.filter (fun i -> eqName i.TableName table)

    /// テーブルを追加（同名があれば置き換える）。
    let addTable (c: Catalog) (t: TableDefinition) =
        { c with
            Tables =
                c.Tables
                |> List.filter (fun x -> not (eqName x.Name t.Name))
                |> fun ts -> ts @ [ t ]
        }

    /// 索引を追加（同名があれば置き換える）。
    let addIndex (c: Catalog) (i: IndexDefinition) =
        { c with
            Indexes =
                c.Indexes
                |> List.filter (fun x -> not (eqName x.IndexName i.IndexName))
                |> fun xs -> xs @ [ i ]
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

    /// `TableSchema` を永続化形にする。
    let toDefinition (schema: TableSchema) =
        {
            Name = schema.Name
            Columns = schema.Columns
            PrimaryKeyColumns = List.ofArray schema.PrimaryKeyColumns
        }

    /// 永続化形を `TableSchema` に戻す。
    let toSchema (t: TableDefinition) =
        let rec findIndices (names: string list) (acc: int list) =
            match names with
            | [] -> ret (List.rev acc)
            | name :: rest ->
                match t.Columns |> List.tryFindIndex (fun c -> eqName c.Name name) with
                | Some i -> findIndices rest (i :: acc)
                | None -> Error(DbError.name (sprintf "テーブル %s に主キー列 %s がありません" t.Name name))

        findIndices t.PrimaryKeyColumns []
        |> Result.map (fun idx ->
            {
                Name = t.Name
                Columns = t.Columns
                PrimaryKey = idx |> List.sort
                Indexes = []
            })

    /// `IndexSchema` を永続化形にする。
    let toIndexDefinition (table: string) (index: IndexSchema) =
        {
            IndexName = index.Name
            TableName = table
            Columns = index.Columns
            Unique = index.Unique
        }

    /// 永続化形を `IndexSchema` に戻す。
    /// <remarks>
    /// `Catalog.toSchema` は `Indexes = []` で埋まるので、
    /// この変換を通さないと索引が実行系から見えない。
    /// </remarks>
    let toIndexSchema (i: IndexDefinition) =
        {
            Name = i.IndexName
            Columns = i.Columns
            Unique = i.Unique
        }

    /// カタログにある索引を実行用の `IndexSchema` へ落とす。
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
    // `flags`: bit0 = NOT NULL / bit1 = PRIMARY KEY / bit2 = UNIQUE
    // `default` は長さ 0 が「DEFAULT なし」。
    //

    let private encodeTable (t: TableDefinition) =
        let w0 = Writer.varint (uint64 t.Columns.Length) Writer.empty

        let w2 =
            t.Columns
            |> List.fold
                (fun w c ->
                    let w = Writer.str c.Name w
                    let w = Writer.writeByte (DataType.toTag c.DataType) w

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

    let private readColumn (c: Cursor) =
        Read.readStr c
        >>= fun (name, c1) ->
            Read.readByte c1
            >>= fun (tag, c2) ->
                Read.readByte c2
                >>= fun (flags, c3) ->
                    Read.readLengthPrefixed c3
                    >>= fun (_, c4) ->
                        DataType.fromTag tag
                        |> Result.map (fun dt ->
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
                            c4)

    [<TailCall>]
    let rec private readColumns (n: uint64) (c: Cursor) (acc: ColumnDefinition list) =
        if n = 0UL then
            ret (List.rev acc, c)
        else
            match readColumn c with
            | Error e -> Error e
            | Ok(cd, c1) -> readColumns (n - 1UL) c1 (cd :: acc)

    [<TailCall>]
    let rec private readNames (n: uint64) (c: Cursor) (acc: string list) =
        if n = 0UL then
            ret (List.rev acc, c)
        else
            match Read.readStr c with
            | Error e -> Error e
            | Ok(name, c1) -> readNames (n - 1UL) c1 (name :: acc)

    let private decodeTable (name: string) (bytes: byte array) =
        let cur = Cursor.create bytes

        match Read.readVarint cur with
        | Error e -> Error e
        | Ok(colCount, cur1) ->
            match readColumns colCount cur1 [] with
            | Error e -> Error e
            | Ok(cols, cur2) ->
                match Read.readVarint cur2 with
                | Error e -> Error e
                | Ok(pkCount, cur3) ->
                    match readNames pkCount cur3 [] with
                    | Error e -> Error e
                    | Ok(pks, _) ->
                        ret
                            {
                                Name = name
                                Columns = cols
                                PrimaryKeyColumns = pks
                            }

    let private encodeIndex (i: IndexDefinition) =
        let w2 =
            Writer.str i.TableName Writer.empty
            |> Writer.bool i.Unique
            |> Writer.varint (uint64 i.Columns.Length)

        i.Columns |> List.fold (fun w c -> Writer.str c w) w2 |> Writer.toArray

    let private decodeIndex (name: string) (bytes: byte array) =
        let cur = Cursor.create bytes

        match Read.readStr cur with
        | Error e -> Error e
        | Ok(table, cur1) ->
            match Read.readBool cur1 with
            | Error e -> Error e
            | Ok(unique, cur2) ->
                match Read.readVarint cur2 with
                | Error e -> Error e
                | Ok(colCount, cur3) ->
                    match readNames colCount cur3 [] with
                    | Error e -> Error e
                    | Ok(cols, _) ->
                        ret
                            {
                                IndexName = name
                                TableName = table
                                Columns = cols
                                Unique = unique
                            }

    let private bytesEqual (a: byte array) (b: byte array) =
        a.Length = b.Length && Array.forall2 (=) a b

    let private asciiSuffix (prefix: string) (bytes: byte array) =
        let p = Encoding.UTF8.GetByteCount prefix

        if bytes.Length < p then
            ""
        else
            Encoding.UTF8.GetString(bytes, p, bytes.Length - p)

    let private startsWithAscii (prefix: string) (bytes: byte array) =
        let p = Encoding.UTF8.GetBytes prefix

        if bytes.Length < p.Length then
            false
        else
            Array.forall2 (=) bytes.[0 .. p.Length - 1] p

    let private toValue (bytes: byte array) = [| SqlValue.Blob bytes |]

    let private ofValue (v: SqlValue array) =
        match v with
        | [| SqlValue.Blob b |] -> Some b
        | [| SqlValue.Text t |] -> Some(Encoding.UTF8.GetBytes t)
        | _ -> None

    // -- 永続化 --------------------------------------------------------------

    /// カタログを読み込む。キーが無ければ空を返す。
    let load (store: RowStore) =
        let rec go (rest: (byte array * SqlValue array option) array) (acc: Catalog) =
            if Array.isEmpty rest then
                ret acc
            else
                let key, value = rest.[0]
                let tail = rest.[1..]

                match value with
                | None -> go tail acc
                | Some v ->
                    match ofValue v with
                    | None -> go tail acc
                    | Some bytes ->
                        if startsWithAscii "table:" key then
                            let name = asciiSuffix "table:" key
                            decodeTable name bytes >>= fun t -> go tail (addTable acc t)
                        elif startsWithAscii "index:" key then
                            let name = asciiSuffix "index:" key
                            decodeIndex name bytes >>= fun i -> go tail (addIndex acc i)
                        else
                            go tail acc

        RowStore.scan store keyspace [||] 0
        |> Result.bind (fun rows -> go rows Catalog.empty)

    /// カタログを保存する（消えた定義は tombstone で落とす）。
    let save (store: RowStore) (catalog: Catalog) =
        let rec numberTables (rest: TableDefinition list) (seq: uint64) (acc: (string * Entry array) list) =
            match rest with
            | [] -> List.rev acc, seq
            | t :: tail ->
                let e = Entry.put (tableKey t.Name) seq (toValue (encodeTable t))
                numberTables tail (seq + 1UL) ((keyspace, [| e |]) :: acc)

        let rec numberIndexes (rest: IndexDefinition list) (seq: uint64) (acc: (string * Entry array) list) =
            match rest with
            | [] -> List.rev acc, seq
            | i :: tail ->
                let e = Entry.put (indexKey i.IndexName) seq (toValue (encodeIndex i))
                numberIndexes tail (seq + 1UL) ((keyspace, [| e |]) :: acc)

        let isWanted (key: byte array) =
            if startsWithAscii "table:" key then
                catalog.Tables |> List.exists (fun t -> bytesEqual key (tableKey t.Name))
            else
                catalog.Indexes |> List.exists (fun i -> bytesEqual key (indexKey i.IndexName))

        let rec numberDeletes (rest: byte array list) (seq: uint64) (acc: Entry list) =
            match rest with
            | [] -> acc |> List.rev |> List.toArray
            | k :: tail -> numberDeletes tail (seq + 1UL) (Entry.delete k seq :: acc)

        RowStore.engine store keyspace
        |> Result.map (fun e -> e.NextSeqNo)
        >>= fun seq0 ->
            let tableBatches, seq1 = numberTables catalog.Tables seq0 []
            let indexBatches, seq2 = numberIndexes catalog.Indexes seq1 []

            RowStore.writeBatches store (tableBatches @ indexBatches)
            |> Result.bind (fun updated ->
                RowStore.scan updated keyspace [||] 0
                |> Result.bind (fun rows ->
                    let stale =
                        rows
                        |> Array.choose (fun (key, v) ->
                            match v with
                            | Some _ when isWanted key -> None
                            | Some _ -> Some key
                            | None -> None)
                        |> Array.toList

                    if List.isEmpty stale then
                        ret updated
                    else
                        let deletes = numberDeletes stale seq2 []
                        RowStore.writeBatches updated [ (keyspace, deletes) ]))
