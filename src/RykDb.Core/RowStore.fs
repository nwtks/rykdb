// ===========================================================================
// RowStore — テーブル・索引を keyspace 単位の LSM ツリーへ載せる層。
//
//   主キー: keyspace "t:<table>"、キーは KeyBuilder.ofValues 主キー列
//   索引  : keyspace "i:<index>"、キーは KeyBuilder.ofIndexAndPrimary
//           （末尾に主キーを持たせるので索引値 → 行が一意に決まる）
//   ディレクトリ: <root>/<base64(keyspace)>/
//
// 行は列順の SqlValue 配列。保存時 ValueCodec でバイト列に落とす。
// 詳細は docs/architecture.md §7.1。
// ===========================================================================

namespace RykDb.Core

open System.IO

// ---------------------------------------------------------------------------
// スキーマ
// ---------------------------------------------------------------------------

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

    /// 列名 → 位置。
    member this.IndexOf(column: string) =
        this.Columns
        |> List.tryFindIndex (fun c -> c.Name = column)
        |> Option.map (fun i -> i)
        |> Option.defaultValue -1

    /// 列名 → 位置（見つからなければエラー）。
    member this.RequireIndexOf(column: string) =
        match this.IndexOf column with
        | i when i >= 0 -> ret i
        | _ -> Error(DbError.name (sprintf "テーブル %s に列 %s がありません" this.Name column))

    /// 主キー列名。
    member this.PrimaryKeyColumns =
        this.PrimaryKey |> List.map (fun i -> this.Columns.[i].Name) |> List.toArray

module internal TableSchema =
    /// CreateTable の AST からスキーマを作る。
    let ofCreateTable (c: CreateTable) =
        if c.Columns.IsEmpty then
            Error(DbError.constraintViolation (sprintf "テーブル %s に列がありません" c.Name))
        else
            let fromConstraint =
                c.Constraints
                |> List.tryPick (fun tc ->
                    match tc with
                    | TableConstraint.PrimaryKeyColumns cols -> Some cols
                    | _ -> None)

            let pkNames =
                match fromConstraint with
                | Some cols -> cols
                | None ->
                    c.Columns
                    |> List.filter (fun col -> col.Constraint.PrimaryKey)
                    |> List.map (fun col -> col.Name)

            if List.isEmpty pkNames then
                Error(DbError.constraintViolation (sprintf "テーブル %s に主キーがありません" c.Name))
            else
                pkNames
                |> List.map (fun name ->
                    match c.Columns |> List.tryFindIndex (fun col -> col.Name = name) with
                    | Some i -> ret i
                    | None -> Error(DbError.name (sprintf "主キー列 %s が定義されていません" name)))
                |> List.fold
                    (fun acc r ->
                        match acc, r with
                        | Error e, _ -> Error e
                        | _, Error e -> Error e
                        | Ok acc, Ok v -> Ok(v :: acc))
                    (ret [])
                |> Result.map List.rev
                |> Result.map List.sort
                |> Result.map (fun pk ->
                    {
                        Name = c.Name
                        Columns = c.Columns
                        PrimaryKey = pk
                        Indexes = []
                    })

    /// NOT NULL 制約と型を検証する。
    let validateRow (schema: TableSchema) (row: SqlValue array) =
        if row.Length <> schema.Columns.Length then
            Error(
                DbError.constraintViolation (
                    sprintf "テーブル %s は %d 列ですが %d 個の値が渡されました" schema.Name schema.Columns.Length row.Length
                )
            )
        else
            schema.Columns
            |> List.mapi (fun i col -> (i, col))
            |> List.fold
                (fun acc (i, col) ->
                    acc
                    >>= fun () ->
                        if row.[i] = SqlValue.Null && col.Constraint.NotNull then
                            Error(DbError.constraintViolation (sprintf "NOT NULL 制約に違反: %s.%s" schema.Name col.Name))
                        else
                            ret ())
                (ret ())

    /// 主キー部分だけ取り出す。
    let primaryKeyOf (schema: TableSchema) (row: SqlValue array) =
        schema.PrimaryKey |> List.map (fun i -> row.[i]) |> List.toArray

// ---------------------------------------------------------------------------
// キー生成
// ---------------------------------------------------------------------------

module internal RowKey =
    /// 主キーの LSM へ入れるキー。
    let primary (schema: TableSchema) (row: SqlValue array) =
        KeyBuilder.ofValues (TableSchema.primaryKeyOf schema row)

    /// 索引の LSM へ入れるキー（索引列 + 主キー列）。
    let indexKey (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        let indexCols =
            index.Columns
            |> List.filter (fun name -> schema.IndexOf name >= 0)
            |> List.map (fun name -> row.[schema.IndexOf name])

        KeyBuilder.ofIndexAndPrimary indexCols (TableSchema.primaryKeyOf schema row)

    /// 索引キー（検索側）。
    /// <paramref name="row"/> は列順の完全な行で受け取る。
    /// 列の値は「索引の列だけ」を並べたものではなく、スキーマのどの列に
    /// あるかが分かる形的（全部入り）である必要がある。
    let indexPrefix (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.filter (fun name -> schema.IndexOf name >= 0)
        |> List.map (fun name -> row.[schema.IndexOf name])
        |> KeyBuilder.ofValues

/// 書き込みの単位。(keyspace, key) -> 値（None は削除）。
module internal Keyspace =
    /// テーブルの主キー用 keyspace 名。
    let table (name: string) = "t:" + name

    /// インデックスのキー用 keyspace 名。
    let index (name: string) = "i:" + name

    /// keyspace 名をディレクトリ名に使える形にする。
    let toDirName (keyspace: string) =
        keyspace |> System.Text.Encoding.UTF8.GetBytes |> System.Convert.ToBase64String

    /// ディレクトリ名を keyspace 名に戻す（keyspace 名でなければ `None`）。
    let tryFromDirName (dirName: string) =
        try
            let bytes = System.Convert.FromBase64String dirName

            match System.Text.Encoding.UTF8.GetString bytes with
            // ディレクトリ名に使える形なので、keyspace の接頭辞で釣る。
            | ks when
                System.String.IsNullOrEmpty ks
                || not (ks.StartsWith "t:") && not (ks.StartsWith "i:")
                ->
                None
            | ks -> Some ks
        with _ ->
            None


// ---------------------------------------------------------------------------
// RowStore — keyspace ごとの StorageEngine を束ねる
// ---------------------------------------------------------------------------

/// keyspace ごとのエンジンとスキーマを束ねたもの。
type internal RowStore =
    {
        Root: string
        /// keyspace → (ディレクトリ, エンジン)
        Engines: Map<string, StorageEngine * string>
    }

module internal RowStore =
    let private dirOf (root: string) (keyspace: string) =
        Path.Combine(root, Keyspace.toDirName keyspace)

    let private ensure (store: RowStore) (keyspace: string) =
        match store.Engines |> Map.tryFind keyspace with
        | Some v -> ret v
        | None ->
            let dir = dirOf store.Root keyspace

            Manifest.ensureDirectory dir
            >>= fun () -> StorageEngine.openDatabase dir |> Result.map (fun engine -> engine, dir)

    /// DB を開く（keyspace は必要に応じて作られる）。
    /// <remarks>
    /// 既存の keyspace ディレクトリはすべて開いておく。
    /// 開かないまま後始末すると、`Database.openWith` が
    /// 「SeqNo をどこから採番するか」を知れず、`LastCommitSeqNo = 0` から始まり、
    /// 既にディスクにある行がすべて「未来」 として見えなくなる。
    /// </remarks>
    let openStore (root: string) =
        protect (fun () -> Directory.CreateDirectory root |> ignore)
        |> Result.bind (fun () ->
            let known =
                Directory.GetDirectories root
                |> Array.choose (fun dir ->
                    let name = Path.GetFileName dir

                    match Keyspace.tryFromDirName name with
                    | Some ks -> Some(ks, dir)
                    | None -> None)

            let rec go (rest: (string * string) list) (acc: Map<string, StorageEngine * string>) =
                match rest with
                | [] -> ret acc
                | (ks, dir) :: tail ->
                    Manifest.ensureDirectory dir
                    >>= fun () ->
                        StorageEngine.openDatabase dir
                        |> Result.bind (fun e -> go tail (Map.add ks (e, dir) acc))

            go (known |> Array.toList) Map.empty
            |> Result.map (fun engines -> { Root = root; Engines = engines }))

    /// 全 keyspace の NextSeqNo の最大値 − 1（＝保存済みの SeqNo）。
    let lastSeqNo (store: RowStore) =
        store.Engines
        |> Map.toList
        |> List.fold (fun acc (_, (e, _)) -> max acc (if e.NextSeqNo = 0UL then 0UL else e.NextSeqNo - 1UL)) 0UL

    /// keyspace のエンジン（無ければ作る）。
    let engine (store: RowStore) (keyspace: string) = ensure store keyspace |> Result.map fst

    /// keyspace ごとに Entry を書き込む。
    /// <paramref name="batches"/> は (keyspace, Entry 配列) の列。
    /// keyspace ごとに(engine へ渡す前に SeqNo を振り直す)。
    /// 各ツリーは独立なので、SeqNo はツリー内で単調増加（最後の採番 +1 で十分。
    /// keyspace ごとに Entry を書き込む。
    /// Entry の SeqNo は TransactionManager が採番した値をそのまま使う。
    /// keyspace ごとに 1 から採番し直すと MVCC が壊れる（同じ SeqNo が別のキーを占めてしまう）。
    /// keyspace ごとにエンジンが独立なので 並列に commit できるのは採番、
    /// ではなく「キー集合が重ならないこと」だから。
    let writeBatches (store: RowStore) (batches: (string * Entry array) list) =
        let rec go (rest: (string * Entry array) list) (current: RowStore) =
            match rest with
            | [] -> ret current
            | (ks, entries) :: tail ->
                if Array.isEmpty entries then
                    go tail current
                else
                    ensure current ks
                    >>= fun (e, dir) ->
                        StorageEngine.writeBatch e entries
                        >>= fun updated ->
                            let maxSeq = entries |> Array.fold (fun acc e -> max acc e.SeqNo) 0UL

                            let bumped =
                                { updated with
                                    StorageEngine.NextSeqNo = max updated.NextSeqNo (maxSeq + 1UL)
                                }

                            go
                                tail
                                { current with
                                    Engines = Map.add ks (bumped, dir) current.Engines
                                }

        go batches store

    /// keyspace から読む（SeqNo 制限つき）。
    let getAt (store: RowStore) (keyspace: string) (key: byte array) (visible: uint64 option) =
        engine store keyspace
        |> Result.bind (fun e -> StorageEngine.getAt e visible key)
        |> Result.map (function
            | None -> None
            | Some sst -> sst.Value)

    /// keyspace を prefix で走査する（主キー/索引の前方一致）。
    let scanAt (store: RowStore) (keyspace: string) (startKey: byte array) (visible: uint64 option) (limit: int) =
        engine store keyspace
        |> Result.bind (fun e -> StorageEngine.scanAt e visible startKey limit)
        |> Result.map (fun entries ->
            entries
            |> Array.map (fun sst -> sst.Key, sst.Value)
            |> Array.filter (fun (_, v) -> v.IsSome))

    /// keyspace を走査する（制限なし）。
    let scan (store: RowStore) (keyspace: string) (startKey: byte array) (limit: int) =
        scanAt store keyspace startKey None limit

    /// 全 keyspace の active snapshot を進める（GC の基準）。
    let setActiveSnapshot (seqNo: uint64) (store: RowStore) =
        { store with
            Engines =
                store.Engines
                |> Map.map (fun _ (e, dir) -> StorageEngine.setActiveSnapshot seqNo e, dir)
        }

    /// 全 keyspace の compaction を行う。
    let compactAll (store: RowStore) =
        store.Engines
        |> Map.toList
        |> List.fold
            (fun acc (ks, (e, dir)) ->
                acc
                >>= fun current ->
                    StorageEngine.maybeCompact e
                    >>= fun updated ->
                        ret
                            { current with
                                Engines = Map.add ks (updated, dir) current.Engines
                            })
            (ret store)

    /// 全 keyspace を close する。
    let close (store: RowStore) =
        store.Engines
        |> Map.toList
        |> List.fold
            (fun acc (_, (e, _)) -> acc >>= fun () -> StorageEngine.close e |> Result.map (fun () -> ()))
            (ret ())

/// DML — スキーマと transaction を使う上位操作。
/// 1 行の書き込みは「主キーの LSM への 1 件 + 各索引の LSM への 1 件」。
/// これらは異なる keyspace なので並列に commit できる。
/// 削除は主キー + 全索引に tombstone を書く（書き漏らすと索引経由で
/// 消えた行が復活する）。詳細は docs/architecture.md §7.2。
module internal Dml =
    /// テーブルの keyspace 名（"t:&lt;table&gt;"）。
    let private tableSpace (schema: TableSchema) = Keyspace.table schema.Name

    /// 索引 schema を登録する。
    let private indexSpace (index: IndexSchema) = Keyspace.index index.Name

    /// 1 行を transaction に積む（まだ DB には入らない）。
    /// 索引のエントリも同じ transaction に入れるので、commit 原子性が保たれる。
    let insert (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
        TableSchema.validateRow schema row
        >>= fun () ->
            let tks = tableSpace schema
            let pk = RowKey.primary schema row
            let next = Transaction.put txn tks pk row

            let withIndexes =
                schema.Indexes
                |> List.fold
                    (fun acc idx ->
                        match acc with
                        | Error e -> Error e
                        | Ok t ->
                            let ikey = RowKey.indexKey schema idx row
                            Ok(Transaction.put t (indexSpace idx) ikey row))
                    (ret next)

            withIndexes

    /// 主キーで既存行を取り出す。
    let getByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (key: SqlValue array) =
        RowStore.getAt store (tableSpace schema) (KeyBuilder.ofValues key) visible

    /// 主キーで prefix 走査する（キー昇順）。
    let scanByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (limit: int) =
        RowStore.scanAt store (tableSpace schema) [||] visible limit
        |> Result.map (Array.map (fun (_, v) -> v |> Option.defaultValue [||]))

    /// 索引の値で 1 行を引く。UNIQUE 索引なら前方一致で唯一の行が取れる。
    /// <paramref name="row"/> は列順の完全な行。
    let getByIndex
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (visible: uint64 option)
        (cols: SqlValue array)
        =
        let prefix = RowKey.indexPrefix schema index cols
        let ks = indexSpace index

        RowStore.scanAt store ks prefix visible 0
        |> Result.map (fun rows ->
            rows
            |> Array.tryFind (fun (k, _) -> KeyBuilder.startsWithPrefix k prefix)
            |> Option.bind snd)


    /// 索引の値に一致する全行を引く。
    /// 索引は一意とは限らないので `WHERE dept_id = 2` は
    /// 1 行ではなく「その値を持つ全部の行」を返す。
    /// 1 件だけ返す実装にすると `SELECT ... WHERE indexed_col = v` が
    /// 対象の一部だけを返すようになる。
    /// 索引キーは「索引列の値 ++ 主キー列」なので、キー昇順に走査すると
    /// 一致するエントリは `prefix` から連続して並ぶ。
    /// したがって `prefix` から始めて `startsWithPrefix` が外れるまで取る。
    let scanByIndexPrefix
        (store: RowStore)
        (index: IndexSchema)
        (visible: uint64 option)
        (values: SqlValue array)
        (limit: int)
        =
        let prefix = KeyBuilder.ofValues values
        let ks = indexSpace index

        RowStore.scanAt store ks prefix visible 0
        |> Result.map (fun rows ->
            let matched =
                rows
                |> Array.takeWhile (fun (k, _) -> KeyBuilder.startsWithPrefix k prefix)
                |> Array.choose snd

            if limit > 0 && limit < matched.Length then
                Array.take limit matched
            else
                matched)

    /// 主キーで削除する。主キーと全索引に tombstone を書く。
    /// 元の行を取得してから索引キーを組み立てるので、NOT NULL でも壊れない。
    let deleteByPrimaryKey
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        =
        getByPrimaryKey store schema visible key
        >>= function
            | None -> Error(DbError.notFound (sprintf "削除する行が見つかりません: %s" schema.Name))
            | Some row ->
                let pk = RowKey.primary schema row
                let next = Transaction.remove txn (tableSpace schema) pk

                schema.Indexes
                |> List.fold
                    (fun acc idx ->
                        match acc with
                        | Error e -> Error e
                        | Ok t ->
                            let ikey = RowKey.indexKey schema idx row
                            Ok(Transaction.remove t (indexSpace idx) ikey))
                    (ret next)

    /// 主キーで更新する（主キーと全索引に新しい値を書き込む）。
    /// 読み手には 1 transaction 分の原子的な書き換えとして見える。
    /// 削除してから挿入してはいけない。
    /// `Transaction.Buffers` は `Map&lt;key, value option&gt;` なので、
    /// 同一キーに「tombstone → put」と書くと最後の put だけが残る
    /// （Map の同一キーは上書きされるため tombstone が消える）。
    /// 逆に「put → tombstone」なら put が消える。
    /// LSM は「新しい SeqNo の put が古い put を隠す」ので、
    /// put だけを書けば更新になる。索引側は
    /// 「古い索引キー → tombstone, 新しい索引キー → put」の 2 件が必要。
    let updateByPrimaryKey
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        (newRow: SqlValue array)
        =
        TableSchema.validateRow schema newRow
        >>= fun () ->
            getByPrimaryKey store schema visible key
            >>= function
                | None -> Error(DbError.notFound "更新する行が見つかりません")
                | Some oldRow ->
                    let step (t: Transaction) (idx: IndexSchema) =
                        let oldKey = RowKey.indexKey schema idx oldRow
                        let newKey = RowKey.indexKey schema idx newRow

                        if oldKey = newKey then
                            Transaction.put t (indexSpace idx) newKey newRow
                        else
                            let removed = Transaction.remove t (indexSpace idx) oldKey
                            Transaction.put removed (indexSpace idx) newKey newRow

                    let tks = tableSpace schema
                    let newPk = RowKey.primary schema newRow
                    let afterMain = Transaction.put txn tks newPk newRow
                    schema.Indexes |> List.fold step afterMain |> ret
