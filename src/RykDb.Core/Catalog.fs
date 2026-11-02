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

/// 索引の定義。
type internal IndexSchema =
    {
        Name: string
        /// 索引を張る列
        Columns: string list
        /// 値が一意か（非 UNIQUE なら false）
        Unique: bool
    }

/// テーブルのスキーマ。
type internal TableSchema =
    {
        Name: string
        /// 列の定義（順序は行の並びと一致する）
        Columns: ColumnDefinition list
        /// 主キーになる列のインデックス（Columns 内の位置）
        PrimaryKey: int list
        /// この表の索引
        Indexes: IndexSchema list
    }

module internal TableSchema =
    /// 列名の重複を検証する（SQL の識別子は大文字小文字を区別しない）。
    let private validateColumnNames (columns: ColumnDefinition list) =
        match columns |> List.map _.Name |> SqlName.duplicate with
        | Some name -> err (DbError.name (sprintf "列 %s が重複しています" name))
        | None -> ret ()

    /// PRIMARY KEY の定義を1つだけ取り出す。
    /// 列レベルとテーブルレベルの両方、複数のテーブル制約はエラーにする。
    let private primaryKeyNames (c: CreateTable) =
        let tableConstraints =
            c.Constraints
            |> List.choose (fun tc ->
                match tc with
                | TableConstraint.PrimaryKeyColumns cols -> Some cols
                | _ -> None)

        let columnNames =
            c.Columns |> List.filter _.Constraint.PrimaryKey |> List.map _.Name

        match tableConstraints, columnNames with
        | [], [] -> err (sprintf "テーブル %s に主キーがありません" c.Name |> DbError.constraintViolation)
        | [ cols ], [] when cols.IsEmpty -> err (sprintf "テーブル %s の主キー列が空です" c.Name |> DbError.constraintViolation)
        | [ cols ], [] -> ret cols
        | [], [ name ] -> ret [ name ]
        | [], _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY が複数の列に指定されています" c.Name
                |> DbError.constraintViolation
            )
        | _ :: _ :: _, _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY のテーブル制約が複数あります" c.Name
                |> DbError.constraintViolation
            )
        | [ _ ], _ :: _ ->
            err (
                sprintf "テーブル %s の PRIMARY KEY が列とテーブルの両方に指定されています" c.Name
                |> DbError.constraintViolation
            )

    /// PRIMARY KEY の列が実在し、重複していないことを検証する。
    let private validatePrimaryKeyColumns (columns: ColumnDefinition list) (names: string list) =
        match SqlName.duplicate names with
        | Some name -> err (DbError.name (sprintf "PRIMARY KEY の列 %s が重複しています" name))
        | None ->
            names
            |> List.map (fun name ->
                match columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                | Some _ -> ret ()
                | None -> err (DbError.name (sprintf "主キー列 %s が定義されていません" name)))
            |> all
            |> Result.map ignore

    /// テーブルレベルの UNIQUE 制約の列が実在し、重複していないことを検証する。
    let private validateUniqueColumns (columns: ColumnDefinition list) (names: string list) =
        if List.isEmpty names then
            err (DbError.constraintViolation "UNIQUE 制約の列が空です")
        else
            match SqlName.duplicate names with
            | Some name -> err (DbError.name (sprintf "UNIQUE 制約の列 %s が重複しています" name))
            | None ->
                names
                |> List.map (fun name ->
                    match columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                    | Some _ -> ret ()
                    | None -> err (DbError.name (sprintf "UNIQUE 制約の列 %s が定義されていません" name)))
                |> all
                |> Result.map ignore

    /// 主キー列に NOT NULL と PRIMARY KEY を立てる。
    /// テーブルレベルの PRIMARY KEY でも列定義を自己完結させる。
    let private markPrimaryKeyColumns (pkNames: string list) (columns: ColumnDefinition list) =
        columns
        |> List.map (fun col ->
            if pkNames |> List.exists (fun name -> SqlName.equal name col.Name) then
                { col with
                    Constraint =
                        { col.Constraint with
                            NotNull = true
                            PrimaryKey = true
                        }
                }
            else
                col)

    /// CreateTable の AST からスキーマを作る。
    let ofCreateTable (c: CreateTable) =
        result {
            do!
                if c.Columns.IsEmpty then
                    err (sprintf "テーブル %s に列がありません" c.Name |> DbError.constraintViolation)
                else
                    ret ()

            do! validateColumnNames c.Columns
            let! pkNames = primaryKeyNames c
            do! validatePrimaryKeyColumns c.Columns pkNames

            let! () =
                c.Constraints
                |> List.choose (fun tc ->
                    match tc with
                    | TableConstraint.UniqueColumns cols -> Some cols
                    | _ -> None)
                |> List.map (validateUniqueColumns c.Columns)
                |> all
                |> Result.map ignore

            let! pk =
                pkNames
                |> List.map (fun name ->
                    match c.Columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
                    | Some i -> ret i
                    | None -> err (sprintf "主キー列 %s が定義されていません" name |> DbError.name))
                |> all
                |> Result.map List.sort

            return
                {
                    Name = c.Name
                    Columns = markPrimaryKeyColumns pkNames c.Columns
                    PrimaryKey = pk
                    Indexes = []
                }
        }

    /// NOT NULL 制約と型を検証する。
    let validateRow (schema: TableSchema) (row: SqlValue array) =
        if row.Length <> schema.Columns.Length then
            err (
                sprintf "テーブル %s は %d 列ですが %d 個の値が渡されました" schema.Name schema.Columns.Length row.Length
                |> DbError.constraintViolation
            )
        else
            schema.Columns
            |> List.mapi (fun i col -> i, col)
            |> foldOk
                (fun () (i, col) ->
                    let isPrimaryKey = schema.PrimaryKey |> List.contains i

                    if row.[i] = SqlValue.Null && (col.Constraint.NotNull || isPrimaryKey) then
                        err (
                            sprintf "NOT NULL 制約に違反: %s.%s" schema.Name col.Name
                            |> DbError.constraintViolation
                        )
                    else
                        ret ())
                ()

    /// 主キー部分だけ取り出す。
    let primaryKeyOf (schema: TableSchema) (row: SqlValue array) =
        schema.PrimaryKey |> List.map (fun i -> row.[i]) |> List.toArray

    /// 主キー列名。
    let primaryKeyColumns (schema: TableSchema) =
        schema.PrimaryKey |> List.map (fun i -> schema.Columns.[i].Name) |> List.toArray

    /// 列名 → 位置。
    /// SQL の識別子は大文字小文字を区別しないので、ここで正規化する。
    let indexOf (schema: TableSchema) (column: string) =
        schema.Columns |> List.tryFindIndex (fun c -> SqlName.equal c.Name column)

    /// 列名 → 位置（見つからなければエラー）。
    let requireIndexOf (schema: TableSchema) (column: string) =
        match indexOf schema column with
        | Some i -> ret i
        | None -> err (sprintf "テーブル %s に列 %s がありません" schema.Name column |> DbError.name)

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
    let Empty = { Tables = []; Indexes = [] }

    /// SQL の識別子は大文字小文字を区別しない（OrdinalIgnoreCase）。
    let eqName (a: string) (b: string) = SqlName.equal a b

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

    /// テーブルを削除する（そのテーブルの索引も消える）。
    let removeTable (c: Catalog) (name: string) =
        { c with
            Tables = c.Tables |> List.filter (fun t -> not (eqName t.Name name))
            Indexes = c.Indexes |> List.filter (fun i -> not (eqName i.TableName name))
        }

    /// 索引を追加（同名があれば置き換える）。
    let addIndex (c: Catalog) (i: IndexDefinition) =
        { c with
            Indexes = upsert (fun (x: IndexDefinition) -> x.IndexName) i c.Indexes
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

    /// UNIQUE 制約が自動作成する索引の名前。
    let implicitIndexName (table: string) (ordinal: int) = sprintf "__unique_%s_%d" table ordinal

    /// ユーザーが作成・削除できない予約索引名か。
    /// 暗黙の UNIQUE 索引を DROP して制約を消せないようにする。
    let isReservedIndexName (name: string) =
        name.StartsWith("__unique_", System.StringComparison.OrdinalIgnoreCase)

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

        t.PrimaryKeyColumns
        |> List.fold (fun w name -> Writer.str name w) w2
        |> Writer.toArray

    let private decodeTable (name: string) (bytes: byte array) =
        let readColumn (c: Cursor) =
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
                            }
                    },
                    c4
            }

        result {
            let! colCount, cur1 = Cursor.create bytes |> Reader.readVarint
            let! cols, cur2 = Reader.readItems readColumn colCount cur1
            let! pkCount, cur3 = Reader.readVarint cur2
            let! pks, _ = Reader.readItems Reader.readStr pkCount cur3

            return
                {
                    Name = name
                    Columns = cols
                    PrimaryKeyColumns = pks
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

    /// カタログを保存する（消えた定義は tombstone で落とす）。
    /// previous は保存前のカタログ（store にあった内容）。
    /// 消えた定義は「previous にあり catalog にないキー」として差分だけで求める
    /// （カタログ keyspace の全走査をしない）。
    /// 書き込みは 1バッチにまとめる
    /// （tombstone も同じバッチに含めるので WAL の fsync は 1回で済む）。
    let save (store: RowStore) (previous: Catalog) (catalog: Catalog) =
        let tableKey (name: string) =
            System.Text.Encoding.UTF8.GetBytes("table:" + name)

        let indexKey (name: string) =
            System.Text.Encoding.UTF8.GetBytes("index:" + name)

        let rec numberList (mk: 'a -> uint64 -> 'b) (rest: 'a list) (seq: uint64) (acc: 'b list) =
            match rest with
            | [] -> List.rev acc, seq
            | x :: tail -> numberList mk tail (seq + 1UL) (mk x seq :: acc)

        let toValue (bytes: byte array) = [| SqlValue.Blob bytes |]

        let tableBatch (t: TableDefinition) (seq: uint64) =
            Keyspace.CatalogKeyspace, [| Entry.put (tableKey t.Name) seq (toValue (encodeTable t)) |]

        let indexBatch (i: IndexDefinition) (seq: uint64) =
            Keyspace.CatalogKeyspace, [| Entry.put (indexKey i.IndexName) seq (toValue (encodeIndex i)) |]

        let deleteBatch (key: byte array) (seq: uint64) =
            Keyspace.CatalogKeyspace, [| Entry.delete key seq |]

        let bytesEqual (a: byte array) (b: byte array) =
            a.Length = b.Length && Array.forall2 (=) a b

        let staleKeys =
            let wanted =
                (catalog.Tables |> List.map (fun t -> tableKey t.Name))
                @ (catalog.Indexes |> List.map (fun i -> indexKey i.IndexName))

            let known =
                (previous.Tables |> List.map (fun t -> tableKey t.Name))
                @ (previous.Indexes |> List.map (fun i -> indexKey i.IndexName))

            known
            |> List.filter (fun k -> not (wanted |> List.exists (fun w -> bytesEqual k w)))

        result {
            let! e = RowStore.ensure Keyspace.CatalogKeyspace store
            let seq0 = e.NextSeqNo
            let tableBatches, seq1 = numberList tableBatch catalog.Tables seq0 []
            let indexBatches, seq2 = numberList indexBatch catalog.Indexes seq1 []
            let deleteBatches, _ = numberList deleteBatch staleKeys seq2 []

            let! updated =
                RowStore.writeBatches (tableBatches @ indexBatches @ deleteBatches) store

            return updated
        }

    /// カタログを読み込む。キーが無ければ空を返す。
    let load (store: RowStore) =
        let ofValue (v: SqlValue array) =
            match v with
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

        let keyKindOf (key: byte array) =
            match tryPrefix key "table:" with
            | Some name -> Some(false, name)
            | None ->
                match tryPrefix key "index:" with
                | Some name -> Some(true, name)
                | None -> None

        let rec decodeRows (rows: (byte array * SqlValue array option) array) (i: int) (acc: Catalog) =
            if i >= rows.Length then
                ret acc
            else
                let key, value = rows.[i]

                match value with
                | None -> decodeRows rows (i + 1) acc
                | Some v ->
                    match ofValue v with
                    | None -> decodeRows rows (i + 1) acc
                    | Some bytes ->
                        match keyKindOf key with
                        | Some(false, name) ->
                            match decodeTable name bytes with
                            | Error e -> err e
                            | Ok t -> addTable acc t |> decodeRows rows (i + 1)
                        | Some(true, name) ->
                            match decodeIndex name bytes with
                            | Error e -> err e
                            | Ok idx -> addIndex acc idx |> decodeRows rows (i + 1)
                        | None -> decodeRows rows (i + 1) acc

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
        {
            Columns = Array.append schema.Columns other.Columns
            Tables = schema.Tables @ other.Tables
        }

    /// スキーマ列を左から連結する（FROM / JOIN の出力スキーマ組み立て）。
    let concatAll (schemas: Schema list) =
        schemas |> List.fold (fun (acc: Schema) (s: Schema) -> concat acc s) empty

    /// `*` / `t.*` が表す出力列を展開する。
    /// どちらもなければ None。
    /// <remarks>
    /// ヘッダ行（projectHeader）と射影の式（Ops.project）が同じ列を共有する。
    /// どちらかが先に変わると `SELECT *` の列数とヘッダがずれる（実際にずれた）。
    /// Expr は `[<NoComparison>]` なので「どちらの star だったか」は別のフラグで返す
    /// （qualified = `t.*` なら true）。
    /// </remarks>
    let expandStar (schema: Schema) (e: Expr) =
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
    let hasQualifier (schema: Schema) (qualifier: string) =
        schema.Columns
        |> Array.exists (fun c ->
            match c.Qualifier with
            | Some q -> SqlName.equal q qualifier
            | None -> false)

    /// 射影後のヘッダ行を作る。
    /// `SELECT *` は入力の列名をそのまま（修飾つき）。
    /// `SELECT id` は式を表示する。別名があればそれを使う。
    /// 入力スキーマのヘッダを返すと `SELECT id` が `people.id` になるので、
    /// 射影後の名前を組み立て直す必要がある。
    /// <remarks>
    /// `*` は修飾なしの `c.Name`、`t.*` は修飾つきの `c.Display` で出す（元の挙動）。
    /// </remarks>
    let projectHeader (schema: Schema) (items: SelectItem list) =
        let header (schema: Schema) =
            schema.Columns |> Array.map (fun c -> c.Display)

        match items with
        | [] -> header schema
        | _ ->
            items
            |> List.collect (fun item ->
                match expandStar schema item.Expression with
                | Some(qualified, columns) ->
                    if qualified then
                        columns |> List.map (fun c -> c.Display)
                    else
                        columns |> List.map (fun c -> c.Name)
                | None ->
                    match item.Alias with
                    | Some a -> [ a ]
                    | None -> [ Expr.toDisplayString item.Expression ])
            |> List.toArray

    /// 式を「列の直接参照」に読み替える。
    /// Column はそのまま。修飾子なしの裸の名前（Name）も列として扱う
    /// （パーサが WHERE / ORDER BY の裸の名前を Name にするため）。
    /// 集約呼び出しや式は列ではないので None。
    let tryAsColumnRef (e: Expr) =
        match e with
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
    let resolve (schema: Schema) (r: ColumnRef) =
        let isMatch (c: OutputColumn) =
            match r.Qualifier with
            | Some q ->
                SqlName.equal c.Display r.Display
                || match c.Qualifier with
                   | Some cq -> SqlName.equal cq q && SqlName.equal c.Name r.Name
                   | None -> false
            | None -> SqlName.equal c.Name r.Name

        let rec collect (i: int) (acc: int list) =
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
            err (
                sprintf
                    "列 '%s' が存在しません（使える列: %s）"
                    r.Display
                    (if schema.Columns.Length = 0 then
                         "(なし)"
                     else
                         schema.Columns |> Array.map (fun c -> c.Display) |> String.concat ", ")
                |> DbError.name
            )
        | many ->
            err (
                sprintf "列 '%s' が曖昧です（該当: %s）" r.Display (many |> List.map string |> String.concat ", ")
                |> DbError.name
            )
