// ===========================================================================
// RowStore — テーブル・索引を keyspace 単位の LSM ツリーへ載せる層。
//
//   主キー: keyspace "t:<table>"、キーは KeyBuilder.ofValues 主キー列
//   索引  : keyspace "i:<index>"、キーは KeyBuilder.ofIndexAndPrimary
//           （末尾に主キーを持たせるので索引値 → 行が一意に決まる）
//   ディレクトリ: <root>/<base64(keyspace)>/
//
// 行は列順の SqlValue 配列。保存時 ValueCodec でバイト列に落とす。
// ===========================================================================

namespace RykDb.Core

open System.IO

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
    /// CreateTable の AST からスキーマを作る。
    let ofCreateTable (c: CreateTable) =
        if c.Columns.IsEmpty then
            err (sprintf "テーブル %s に列がありません" c.Name |> DbError.constraintViolation)
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
                | None -> c.Columns |> List.filter _.Constraint.PrimaryKey |> List.map _.Name

            if List.isEmpty pkNames then
                err (sprintf "テーブル %s に主キーがありません" c.Name |> DbError.constraintViolation)
            else
                pkNames
                |> List.map (fun name ->
                    match c.Columns |> List.tryFindIndex (fun col -> col.Name = name) with
                    | Some i -> ret i
                    | None -> err (sprintf "主キー列 %s が定義されていません" name |> DbError.name))
                |> all
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
            err (
                sprintf "テーブル %s は %d 列ですが %d 個の値が渡されました" schema.Name schema.Columns.Length row.Length
                |> DbError.constraintViolation
            )
        else
            schema.Columns
            |> List.mapi (fun i col -> i, col)
            |> foldOk
                (fun () (i, col) ->
                    if row.[i] = SqlValue.Null && col.Constraint.NotNull then
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
    let indexOf (schema: TableSchema) (column: string) =
        schema.Columns
        |> List.tryFindIndex (fun c -> c.Name = column)
        |> Option.map (fun i -> i)
        |> Option.defaultValue -1

    /// 列名 → 位置（見つからなければエラー）。
    let requireIndexOf (schema: TableSchema) (column: string) =
        match indexOf schema column with
        | i when i >= 0 -> ret i
        | _ -> err (sprintf "テーブル %s に列 %s がありません" schema.Name column |> DbError.name)

module internal RowKey =
    let private indexValues (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.filter (fun name -> TableSchema.indexOf schema name >= 0)
        |> List.map (fun name -> row.[TableSchema.indexOf schema name])

    /// 主キーの LSM へ入れるキー。
    let primary (schema: TableSchema) (row: SqlValue array) =
        KeyBuilder.ofValues (TableSchema.primaryKeyOf schema row)

    /// 索引の LSM へ入れるキー（索引列 + 主キー列）。
    let indexKey (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        KeyBuilder.ofIndexAndPrimary (indexValues schema index row) (TableSchema.primaryKeyOf schema row)

    /// 索引キー（検索側）。
    /// row は列順の完全な行で受け取る。
    /// 列の値は「索引の列だけ」を並べたものではなく、
    /// スキーマのどの列にあるかが分かる形的（全部入り）である必要がある。
    let indexPrefix (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        indexValues schema index row |> KeyBuilder.ofValues

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
            let dir =
                System.Convert.FromBase64String dirName |> System.Text.Encoding.UTF8.GetString

            match dir with
            | ks when
                System.String.IsNullOrEmpty ks
                || not (ks.StartsWith "t:") && not (ks.StartsWith "i:")
                ->
                None
            | ks -> Some ks
        with _ ->
            None

/// keyspace ごとのエンジンとスキーマを束ねたもの。
type internal RowStore =
    {
        Root: string
        /// keyspace → (ディレクトリ, エンジン)
        Engines: Map<string, StorageEngine * string>
    }

module internal RowStore =
    let private ensure (store: RowStore) (keyspace: string) =
        let dirOf (root: string) (keyspace: string) =
            Path.Combine(root, Keyspace.toDirName keyspace)

        match store.Engines |> Map.tryFind keyspace with
        | Some v -> ret v
        | None ->
            let dir = dirOf store.Root keyspace

            Manifest.ensureDirectory dir
            >>= fun () -> StorageEngine.openDatabase dir |> Result.map (fun engine -> engine, dir)

    /// DB を開く（keyspace は必要に応じて作られる）。
    /// <remarks>
    /// 既存の keyspace ディレクトリはすべて開いておく。
    /// 開かないまま後始末すると、Database.openWith が「SeqNo をどこから採番するか」を知れず、
    /// LastCommitSeqNo = 0 から始まり、既にディスクにある行がすべて「未来」として見えなくなる。
    /// </remarks>
    let openStore (root: string) =
        let rec openEngines (rest: (string * string) list) (acc: Map<string, StorageEngine * string>) =
            match rest with
            | [] -> ret acc
            | (ks, dir) :: tail ->
                match Manifest.ensureDirectory dir with
                | Error err -> Error err
                | Ok() ->
                    match StorageEngine.openDatabase dir with
                    | Error err -> Error err
                    | Ok e -> openEngines tail (Map.add ks (e, dir) acc)

        let known =
            Directory.GetDirectories root
            |> Array.choose (fun dir ->
                Path.GetFileName dir
                |> Keyspace.tryFromDirName
                |> Option.map (fun ks -> ks, dir))

        result {
            do! protect (fun () -> Directory.CreateDirectory root |> ignore)
            let! engines = openEngines (known |> Array.toList) Map.empty
            return { Root = root; Engines = engines }
        }

    /// keyspace のエンジン（無ければ作る）。
    let engine (store: RowStore) (keyspace: string) = ensure store keyspace |> Result.map fst

    /// keyspace ごとに Entry を書き込む。
    /// batches は (keyspace, Entry 配列) の列。
    /// Entry の SeqNo は TransactionManager が採番した値をそのまま使う。
    /// keyspace ごとに 1 から採番し直すと MVCC が壊れる（同じ SeqNo が別のキーを占めてしまう）。
    /// keyspace ごとにエンジンが独立なので 並列に commit できるのは採番、
    /// ではなく「キー集合が重ならないこと」だから。
    let writeBatches (store: RowStore) (batches: (string * Entry array) list) =
        let rec writeAll (rest: (string * Entry array) list) (current: RowStore) =
            match rest with
            | [] -> ret current
            | (ks, entries) :: tail ->
                if Array.isEmpty entries then
                    writeAll tail current
                else
                    match ensure current ks with
                    | Error err -> Error err
                    | Ok(e, dir) ->
                        match StorageEngine.writeBatch e entries with
                        | Error err -> Error err
                        | Ok updated ->
                            let maxSeq = Entry.maxSeqNo entries

                            let bumped =
                                { updated with
                                    StorageEngine.NextSeqNo = max updated.NextSeqNo (maxSeq + 1UL)
                                }

                            writeAll
                                tail
                                { current with
                                    Engines = Map.add ks (bumped, dir) current.Engines
                                }

        writeAll batches store

    /// keyspace から読む（SeqNo 制限つき）。
    let getAt (store: RowStore) (keyspace: string) (key: byte array) (visible: uint64 option) =
        result {
            let! e = engine store keyspace
            let! sst = StorageEngine.getAt e visible key
            return Option.bind _.Value sst
        }

    /// keyspace を prefix で走査する（主キー/索引の前方一致）。
    let scanAt (store: RowStore) (keyspace: string) (startKey: byte array) (visible: uint64 option) (limit: int) =
        result {
            let! e = engine store keyspace
            let! entries = StorageEngine.scanAt e visible startKey limit

            return
                entries
                |> Array.map (fun sst -> sst.Key, sst.Value)
                |> Array.filter (fun (_, v) -> v.IsSome)
        }

    /// keyspace を走査する（制限なし）。
    let scan (store: RowStore) (keyspace: string) (startKey: byte array) (limit: int) =
        scanAt store keyspace startKey None limit

    /// 全 keyspace の NextSeqNo の最大値 − 1（＝保存済みの SeqNo）。
    let lastSeqNo (store: RowStore) =
        store.Engines
        |> Map.toList
        |> List.fold (fun acc (_, (e, _)) -> max acc (if e.NextSeqNo = 0UL then 0UL else e.NextSeqNo - 1UL)) 0UL

    /// 全 keyspace を close する。
    let close (store: RowStore) =
        store.Engines
        |> Map.toList
        |> foldOk (fun () (_, (e, _)) -> StorageEngine.close e |> Result.map (fun _ -> ())) ()

/// DML — スキーマと transaction を使う上位操作。
/// 1行の書き込みは「主キーの LSM への 1件 + 各索引の LSM への 1件」。
/// これらは異なる keyspace なので並列に commit できる。
/// 削除は主キー + 全索引に tombstone を書く
/// （書き漏らすと索引経由で消えた行が復活する）。
module internal Dml =
    /// テーブルの keyspace 名（"t:&lt;table&gt;"）。
    let private tableSpace (schema: TableSchema) = Keyspace.table schema.Name

    /// 索引 schema を登録する。
    let private indexSpace (index: IndexSchema) = Keyspace.index index.Name

    /// 旧行と新行の差分を索引へ反映する。
    /// 同じ索引キーなら上書きだけ行い、キーが変わった場合だけ旧キーを削除する。
    let private applyIndexChange
        (schema: TableSchema)
        (txn: Transaction)
        (oldRow: SqlValue array option)
        (newRow: SqlValue array option)
        =
        schema.Indexes
        |> foldOk
            (fun current index ->
                let keyFor row = RowKey.indexKey schema index row
                let space = indexSpace index

                match oldRow, newRow with
                | None, None -> ret current
                | None, Some row -> ret (Transaction.put current space (keyFor row) row)
                | Some row, None -> ret (Transaction.remove current space (keyFor row))
                | Some oldValue, Some newValue ->
                    let oldKey = keyFor oldValue
                    let newKey = keyFor newValue

                    if oldKey = newKey then
                        ret (Transaction.put current space newKey newValue)
                    else
                        let removed = Transaction.remove current space oldKey
                        ret (Transaction.put removed space newKey newValue))
            txn

    /// 主キーで既存行を取り出す。
    let getByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (key: SqlValue array) =
        RowStore.getAt store (tableSpace schema) (KeyBuilder.ofValues key) visible

    /// 主キーで prefix 走査する（キー昇順）。
    let scanByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (limit: int) =
        RowStore.scanAt store (tableSpace schema) [||] visible limit
        |> Result.map (Array.map (fun (_, v) -> v |> Option.defaultValue [||]))

    /// 索引の値に一致する全行を引く。
    /// 索引は一意とは限らないので `WHERE dept_id = 2` は 1行ではなく「その値を持つ全部の行」を返す。
    /// 1件だけ返す実装にすると `SELECT ... WHERE indexed_col = v` が対象の一部だけを返すようになる。
    /// 索引キーは「索引列の値 + 主キー列」なので、キー昇順に走査すると一致するエントリは `prefix` から連続して並ぶ。
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

    /// 1行を transaction に積む（まだ DB には入らない）。
    /// 索引のエントリも同じ transaction に入れるので、commit 原子性が保たれる。
    let insert (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
        result {
            do! TableSchema.validateRow schema row
            let tks = tableSpace schema
            let pk = RowKey.primary schema row
            let next = Transaction.put txn tks pk row
            return! applyIndexChange schema next None (Some row)
        }

    /// 主キーで更新する（主キーと全索引に新しい値を書き込む）。
    /// 読み手には 1transaction 分の原子的な書き換えとして見える。
    /// 削除してから挿入してはいけない。
    /// Transaction.Buffers は `Map<key, value option>` なので、
    /// 同一キーに「tombstone → put」と書くと最後の put だけが残る
    /// （Map の同一キーは上書きされるため tombstone が消える）。
    /// 逆に「put → tombstone」なら put が消える。
    /// LSM は「新しい SeqNo の put が古い put を隠す」ので、 put だけを書けば更新になる。
    /// 索引側は「古い索引キー → tombstone, 新しい索引キー → put」の 2件が必要。
    let updateByPrimaryKey
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        (newRow: SqlValue array)
        =
        result {
            do! TableSchema.validateRow schema newRow
            let! row = getByPrimaryKey store schema visible key

            match row with
            | None -> return! err (DbError.notFound "更新する行が見つかりません")
            | Some oldRow ->
                let tks = tableSpace schema
                let newPk = RowKey.primary schema newRow
                let afterMain = Transaction.put txn tks newPk newRow
                return! applyIndexChange schema afterMain (Some oldRow) (Some newRow)
        }

    /// 主キーで削除する。
    /// 主キーと全索引に tombstone を書く。
    /// 元の行を取得してから索引キーを組み立てるので、NOT NULL でも壊れない。
    let deleteByPrimaryKey
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        =
        result {
            let! row = getByPrimaryKey store schema visible key

            match row with
            | None -> return! err (sprintf "削除する行が見つかりません: %s" schema.Name |> DbError.notFound)
            | Some oldRow ->
                let pk = RowKey.primary schema oldRow
                let next = Transaction.remove txn (tableSpace schema) pk
                return! applyIndexChange schema next (Some oldRow) None
        }
