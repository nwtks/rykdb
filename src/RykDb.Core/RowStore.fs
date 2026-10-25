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
                    match c.Columns |> List.tryFindIndex (fun col -> SqlName.equal col.Name name) with
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
    /// SQL の識別子は大文字小文字を区別しないので、ここで正規化する。
    let indexOf (schema: TableSchema) (column: string) =
        schema.Columns |> List.tryFindIndex (fun c -> SqlName.equal c.Name column)

    /// 列名 → 位置（見つからなければエラー）。
    let requireIndexOf (schema: TableSchema) (column: string) =
        match indexOf schema column with
        | Some i -> ret i
        | None -> err (sprintf "テーブル %s に列 %s がありません" schema.Name column |> DbError.name)

module internal RowKey =
    /// 索引の列値を取り出す。
    /// 索引列は CREATE INDEX で検証済み。
    /// 万一欠けている場合は、その列を落とす（防御）。
    let private indexValues (schema: TableSchema) (index: IndexSchema) (row: SqlValue array) =
        index.Columns
        |> List.choose (fun name ->
            match TableSchema.indexOf schema name with
            | Some i -> Some row.[i]
            | None -> None)

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

/// keyspace を跨ぐ commit の意図ログ。
/// 各 keyspace の WAL は独立しているため、途中でクラッシュすると
/// 「テーブルは書けたが索引が書けていない」状態になりうる。
/// 全バッチを 1つのファイルへ原子的に公開してから書き込み、
/// 再オープン時に残っていれば未適用分をやり直す。
module internal CommitIntent =
    let private dir (root: string) = System.IO.Path.Combine(root, ".commit")

    let private encode (batches: (string * Entry array) list) =
        let steps =
            [
                yield Writer.varint (uint64 batches.Length)

                for ks, entries in batches do
                    yield Writer.str ks
                    yield Writer.lengthPrefixed (Entry.encodeBatch 0UL entries)
            ]

        Writer.build steps

    let private decode (data: byte array) =
        let readOne (c: Cursor) =
            result {
                let! ks, c1 = Reader.readStr c
                let! batchBytes, c2 = Reader.readLengthPrefixed c1
                let! _, entries = Entry.decodeBatch batchBytes
                return (ks, entries), c2
            }

        result {
            let c = Cursor.create data
            let! count, c1 = Reader.readVarint c

            if count > uint64 System.Int32.MaxValue then
                return! err (DbError.corruption "CommitIntent: バッチ数が大きすぎます")
            else
                let! batches, _ = Reader.readItems readOne count c1
                return batches
        }

    /// 未適用の intent ファイル（SeqNo の昇順）。
    let pending (root: string) =
        protect (fun () ->
            let d = dir root

            if System.IO.Directory.Exists d then
                System.IO.Directory.GetFiles(d, "*.commit") |> Array.sort
            else
                [||])

    /// バッチを intent として公開する。
    /// 一時ファイルへ書いて flush してから rename するので、
    /// 途中でクラッシュしても半端な intent は見えない。
    let publish (root: string) (batches: (string * Entry array) list) =
        protect (fun () ->
            let d = dir root
            System.IO.Directory.CreateDirectory d |> ignore

            let maxSeq =
                batches
                |> List.collect (fun (_, entries) -> Array.toList entries)
                |> List.map _.SeqNo
                |> List.max

            let name =
                sprintf "%s-%s.commit" (maxSeq.ToString("D20")) (System.Guid.NewGuid().ToString("N"))

            let final = System.IO.Path.Combine(d, name)
            let temp = final + ".tmp"
            let bytes = encode batches

            do
                use fs =
                    new System.IO.FileStream(
                        temp,
                        System.IO.FileMode.Create,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.None
                    )

                fs.Write(bytes, 0, bytes.Length)
                fs.Flush true

            System.IO.File.Move(temp, final, true)
            final)

    /// intent を読む。
    let read (path: string) =
        protect (fun () -> System.IO.File.ReadAllBytes path) >>= decode

    /// intent を消す。
    /// 失敗を握り潰すと、その後に同じキーが更新・GC されたあとの再起動で
    /// 古い intent が再適用され得るので、エラーとして返す。
    let remove (path: string) =
        protect (fun () ->
            if System.IO.File.Exists path then
                System.IO.File.Delete path)

/// keyspace ごとのエンジンとスキーマを束ねたもの。
/// エンジンの開き方を差し替えられるので、テストではディスクに触れない実装を使える。
[<NoEquality; NoComparison>]
type internal RowStore =
    {
        Root: string
        /// keyspace → エンジン
        Engines: Map<string, IStorageEngine>
        /// まだ開いていない keyspace のエンジンを作る
        OpenEngine: string -> Result<IStorageEngine, DbError>
        /// プロセス間ロックのトークン。
        /// メモリ上のテスト用 store は None（ファイルを作らない）。
        Lock: obj option
    }

/// 同じ DB を 2つのプロセスが同時に開かないようにする（`.rykdb.lock`）。
/// <remarks>
/// 全 keyspace の WAL / MANIFEST はプロセスを跨いだ排他を持たないため、
/// 2つのプロセスが書くとデータを壊す。
/// 開くときに root の `.rykdb.lock` を `FileShare.None` で押さえ、
/// 取れなければ `IoError` で失敗する（fail-fast）。
/// **同じプロセス内の複数ハンドルはロックを共有する**（台帳のトークンで数える）ので、
/// 従来どおり `Database.openWith` を何度も呼べる（クラッシュ再現テストもそのまま動く）。
/// Close で最後のトークンが外れたらファイルロックも解放する。
/// 全ハンドルを Close しないままプロセスが終了しても、OS がロックを解放する。
/// </remarks>
module internal ProcessLock =
    /// root に置くロックファイル名。
    let private fileName = ".rykdb.lock"

    /// 台帳の更新だけを直列化する（ロック自体は root ごとに 1本）。
    let private gate = new System.Threading.SemaphoreSlim(1, 1)

    /// 正規化した root -> (トークン -> プロセス内で共有する FileStream)。
    let private held =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            System.Collections.Concurrent.ConcurrentDictionary<obj, System.IO.FileStream>
         >
            System.StringComparer.Ordinal

    /// ロックを取り、解除に使うトークンを返す。
    /// 別プロセスが既に開いていれば `IoError`。
    let acquire (root: string) =
        gate.Wait()

        try
            let key = RootKey.normalize root

            let bucket =
                held.GetOrAdd(
                    key,
                    fun _ -> System.Collections.Concurrent.ConcurrentDictionary<obj, System.IO.FileStream>()
                )

            let token = obj ()

            match bucket |> Seq.tryHead with
            | Some existing ->
                bucket.[token] <- existing.Value
                ret token
            | None ->
                let path = System.IO.Path.Combine(key, fileName)

                match
                    protect (fun () ->
                        new System.IO.FileStream(
                            path,
                            System.IO.FileMode.OpenOrCreate,
                            System.IO.FileAccess.ReadWrite,
                            System.IO.FileShare.None
                        ))
                with
                | Ok stream ->
                    bucket.[token] <- stream
                    ret token
                | Error _ ->
                    err (DbError.create ErrorKind.IoError (sprintf "データベース %s は別のプロセスが開いています（%s）" key fileName))
        finally
            gate.Release() |> ignore

    /// ロックを解除する（二重解除は無害）。
    /// 同じ root を開いている他トークンが残っていればロックは保持したまま。
    let release (root: string) (token: obj) =
        gate.Wait()

        try
            let key = RootKey.normalize root

            match held.TryGetValue key with
            | true, bucket ->
                match bucket.TryRemove token with
                | true, stream ->
                    if bucket.IsEmpty then
                        held.TryRemove key |> ignore
                        protect (fun () -> stream.Dispose()) |> ignore
                | _ -> ()
            | _ -> ()
        finally
            gate.Release() |> ignore

module internal RowStore =
    let private ensure (store: RowStore) (keyspace: string) =
        match store.Engines |> Map.tryFind keyspace with
        | Some engine -> ret engine
        | None -> store.OpenEngine keyspace

    let private applyBatchesCore
        (idempotent: bool)
        (activeSnapshot: uint64)
        (store: RowStore)
        (batches: (string * Entry array) list)
        =
        let rec loop (engine: IStorageEngine) (rest: Entry list) (acc: Entry list) =
            match rest with
            | [] -> ret (List.rev acc |> List.toArray)
            | e :: tail ->
                match engine.GetAt (Some e.SeqNo) e.Key with
                | Error err -> Error err
                | Ok(Some existing) when existing.SeqNo = e.SeqNo && existing.Value = e.Value -> loop engine tail acc
                | Ok _ -> loop engine tail (e :: acc)

        let filterApplied (engine: IStorageEngine) (entries: Entry array) = loop engine (Array.toList entries) []

        let rec writeAll (rest: (string * Entry array) list) (current: RowStore) =
            match rest with
            | [] -> ret current
            | (ks, entries) :: tail ->
                if Array.isEmpty entries then
                    writeAll tail current
                else
                    match ensure current ks with
                    | Error err -> Error err
                    | Ok engine ->
                        let engine = engine.WithActiveSnapshot activeSnapshot

                        let filtered =
                            if idempotent then
                                filterApplied engine entries
                            else
                                ret entries

                        match filtered with
                        | Error err -> Error err
                        | Ok entries when Array.isEmpty entries -> writeAll tail current
                        | Ok entries ->
                            match engine.WriteBatch entries with
                            | Error err -> Error err
                            | Ok updated ->
                                let maxSeq = Entry.maxSeqNo entries
                                let bumped = updated.WithNextSeqNo(max updated.NextSeqNo (maxSeq + 1UL))

                                match bumped.MaybeFlush() with
                                | Error err -> Error err
                                | Ok flushed ->
                                    writeAll
                                        tail
                                        { current with
                                            Engines = Map.add ks flushed current.Engines
                                        }

        writeAll batches store

    /// keyspace ごとにバッチを適用する（intent の公開・削除はしない）。
    /// activeSnapshot は compaction の GC 境界。0 は「全版を残す」。
    let applyBatches (activeSnapshot: uint64) (store: RowStore) (batches: (string * Entry array) list) =
        applyBatchesCore false activeSnapshot store batches

    /// DB を開く（keyspace は必要に応じて作られる）。
    /// <remarks>
    /// 既存の keyspace ディレクトリはすべて開いておく。
    /// 開かないまま後始末すると、Database.openWith が「SeqNo をどこから採番するか」を知れず、
    /// LastCommitSeqNo = 0 から始まり、既にディスクにある行がすべて「未来」として見えなくなる。
    /// </remarks>
    let openStore (root: string) =
        let openDiskEngine (root: string) (keyspace: string) =
            let dir = System.IO.Path.Combine(root, Keyspace.toDirName keyspace)

            result {
                do! Manifest.ensureDirectory dir
                let! engine = StorageEngine.openDatabase dir
                return DiskStorageEngine engine :> IStorageEngine
            }

        let openEngine = openDiskEngine root

        let rec openEngines (rest: string list) (acc: Map<string, IStorageEngine>) =
            match rest with
            | [] -> ret acc
            | ks :: tail ->
                match openEngine ks with
                | Error err -> Error err
                | Ok e -> openEngines tail (Map.add ks e acc)

        let replayPending (store: RowStore) =
            if System.String.IsNullOrEmpty store.Root then
                ret store
            else
                result {
                    let! files = CommitIntent.pending store.Root

                    return!
                        files
                        |> foldOk
                            (fun current path ->
                                result {
                                    let! batches = CommitIntent.read path
                                    let! next = applyBatchesCore true 0UL current batches
                                    let! () = CommitIntent.remove path
                                    return next
                                })
                            store
                }

        result {
            do! protect (fun () -> System.IO.Directory.CreateDirectory root |> ignore)
            let! token = ProcessLock.acquire root

            let! known =
                protect (fun () ->
                    System.IO.Directory.GetDirectories root
                    |> Array.choose (fun dir -> System.IO.Path.GetFileName dir |> Keyspace.tryFromDirName)
                    |> Array.toList)

            let! engines = openEngines known Map.empty

            let store =
                {
                    Root = root
                    Engines = engines
                    OpenEngine = openEngine
                    Lock = Some token
                }

            match replayPending store with
            | Ok ready -> return ready
            | Error e ->
                ProcessLock.release root token
                return! Error e
        }

    /// エンジンの開き方を差し替えて、空の RowStore を作る（テスト用）。
    /// ディスクに触れない実装を渡せば、keyspace はすべてメモリ上に作られる。
    let openStoreWith (root: string) (openEngine: string -> Result<IStorageEngine, DbError>) =
        {
            Root = root
            Engines = Map.empty
            OpenEngine = openEngine
            Lock = None
        }

    /// keyspace のエンジン（無ければ作る）。
    let engine (store: RowStore) (keyspace: string) = ensure store keyspace

    /// keyspace ごとに Entry を書き込む。
    /// batches は (keyspace, Entry 配列) の列。
    /// Entry の SeqNo は TransactionManager が採番した値をそのまま使う。
    /// keyspace ごとに 1 から採番し直すと MVCC が壊れる（同じ SeqNo が別のキーを占めてしまう）。
    /// ディスクでは先に commit intent を公開するので、途中でクラッシュしても
    /// 再オープン時に全 keyspace へ適用し直せる。
    let writeBatchesWithSnapshot (activeSnapshot: uint64) (store: RowStore) (batches: (string * Entry array) list) =
        let nonEmpty =
            batches |> List.filter (fun (_, entries) -> not (Array.isEmpty entries))

        if List.isEmpty nonEmpty then
            ret store
        elif nonEmpty.Length = 1 || System.String.IsNullOrEmpty store.Root then
            applyBatches activeSnapshot store nonEmpty
        else
            result {
                let! intent = CommitIntent.publish store.Root nonEmpty
                let! next = applyBatches activeSnapshot store nonEmpty
                let! () = CommitIntent.remove intent
                return next
            }

    /// GC 境界を指定しない書き込み（カタログ・索引 backfill・テスト用）。
    let writeBatches (store: RowStore) (batches: (string * Entry array) list) =
        writeBatchesWithSnapshot 0UL store batches

    /// keyspace から読む（SeqNo 制限つき）。
    let getAt (store: RowStore) (keyspace: string) (key: byte array) (visible: uint64 option) =
        result {
            let! e = engine store keyspace
            let! sst = e.GetAt visible key
            return Option.bind _.Value sst
        }

    /// keyspace を prefix で走査する（主キー/索引の前方一致）。
    let scanAt (store: RowStore) (keyspace: string) (startKey: byte array) (visible: uint64 option) (limit: int) =
        result {
            let! e = engine store keyspace
            let! entries = e.ScanAt visible startKey limit

            return
                entries
                |> Seq.map (fun sst -> sst.Key, sst.Value)
                |> Seq.filter (fun (_, v) -> v.IsSome)
        }

    /// keyspace を走査する（制限なし）。
    let scan (store: RowStore) (keyspace: string) (startKey: byte array) (limit: int) =
        scanAt store keyspace startKey None limit

    /// 全 keyspace の NextSeqNo の最大値 − 1（＝保存済みの SeqNo）。
    let lastSeqNo (store: RowStore) =
        store.Engines
        |> Map.toList
        |> List.fold (fun acc (_, e) -> max acc (if e.NextSeqNo = 0UL then 0UL else e.NextSeqNo - 1UL)) 0UL

    /// 全 keyspace を close し、プロセス間ロックを外す。
    /// ロックの解除は二重 Close でも無害（トークンは一度しか外れない）。
    let close (store: RowStore) =
        let outcome = store.Engines |> Map.toList |> foldOk (fun () (_, e) -> e.Close()) ()

        match store.Lock with
        | Some token -> ProcessLock.release store.Root token
        | None -> ()

        outcome

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

    /// store の行に txn の書き込みを重ねる。
    /// 追加・削除の後もキー昇順を保つ（走査結果の順序を安定させるため）。
    let private overlayRows
        (keyOf: SqlValue array -> byte array)
        (slots: (byte array * SqlValue array option) list)
        (rows: seq<SqlValue array>)
        =
        if List.isEmpty slots then
            rows
        else
            let slotMap = Map.ofList slots
            let keyed = rows |> Seq.map (fun row -> keyOf row, row) |> Seq.toArray

            let kept =
                keyed
                |> Array.choose (fun (key, row) ->
                    match Map.tryFind key slotMap with
                    | Some None -> None
                    | Some(Some value) -> Some(key, value)
                    | None -> Some(key, row))

            let existing = keyed |> Array.map fst |> Set.ofArray

            let added =
                slots
                |> List.choose (fun (key, value) ->
                    match value with
                    | Some row when not (Set.contains key existing) -> Some(key, row)
                    | _ -> None)

            Array.append kept (Array.ofList added)
            |> Array.sortWith (fun (a, _) (b, _) -> Bytes.compare a b)
            |> Array.map snd
            |> Seq.ofArray

    let private applyLimit (limit: int) (rows: seq<SqlValue array>) =
        if limit > 0 then Seq.truncate limit rows else rows

    let private storeLimitFor (limit: int) (slots: (byte array * SqlValue array option) list) =
        if limit <= 0 then
            0
        else
            let deletions = slots |> List.filter (fun (_, value) -> value.IsNone) |> List.length
            int (min (int64 System.Int32.MaxValue) (int64 limit + int64 deletions))

    let private inKeyRange (lowerBytes: byte array) (upperBytes: byte array option) (key: byte array) =
        Bytes.compare key lowerBytes >= 0
        && match upperBytes with
           | None -> true
           | Some hb -> Bytes.compare key hb <= 0

    let private inIndexRange (lowerBytes: byte array) (upperBytes: byte array option) (key: byte array) =
        inKeyRange lowerBytes upperBytes key
        || match upperBytes with
           | Some hb -> KeyBuilder.startsWithPrefix key hb
           | None -> false

    /// 主キーで既存行を取り出す（txn の書き込みを含む）。
    let getByPrimaryKeyWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (key: SqlValue array)
        =
        let getByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (key: SqlValue array) =
            RowStore.getAt store (tableSpace schema) (KeyBuilder.ofValues key) visible

        match Transaction.tryFind txn (tableSpace schema) (KeyBuilder.ofValues key) with
        | Some slot -> ret slot
        | None -> getByPrimaryKey store schema visible key

    /// 主キーで prefix 走査する（キー昇順）。
    let scanByPrimaryKey (store: RowStore) (schema: TableSchema) (visible: uint64 option) (limit: int) =
        RowStore.scanAt store (tableSpace schema) [||] visible limit
        |> Result.map (Seq.map (fun (_, v) -> v |> Option.defaultValue [||]))

    /// 主キーで prefix 走査する（txn の書き込みを含む）。
    let scanByPrimaryKeyWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (limit: int)
        =
        let slots = Transaction.slotsOf txn (tableSpace schema)

        scanByPrimaryKey store schema visible (storeLimitFor limit slots)
        |> Result.map (fun rows -> overlayRows (RowKey.primary schema) slots rows |> applyLimit limit)

    /// 主キーの範囲走査（txn の書き込みを含む）。
    let scanByPrimaryKeyRangeWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (startKey: SqlValue array)
        (endKey: SqlValue array option)
        (limit: int)
        =
        let scanPrimaryKeyRange
            (store: RowStore)
            (schema: TableSchema)
            (visible: uint64 option)
            (lowerBytes: byte array)
            (upperBytes: byte array option)
            (limit: int)
            =
            RowStore.scanAt store (tableSpace schema) lowerBytes visible limit
            |> Result.map (fun rows ->
                rows
                |> Seq.choose (fun (key, value) ->
                    match value with
                    | Some row when inKeyRange lowerBytes upperBytes key -> Some row
                    | _ -> None)
                |> applyLimit limit)

        let startBytes = KeyBuilder.ofValues startKey
        let endBytes = endKey |> Option.map KeyBuilder.ofValues

        let slots =
            Transaction.slotsOf txn (tableSpace schema)
            |> List.filter (fun (key, _) -> inKeyRange startBytes endBytes key)

        scanPrimaryKeyRange store schema visible startBytes endBytes (storeLimitFor limit slots)
        |> Result.map (fun rows ->
            let merged = overlayRows (RowKey.primary schema) slots rows

            let filtered =
                match endBytes with
                | None -> merged
                | Some eb ->
                    merged
                    |> Seq.filter (fun row -> Bytes.compare (RowKey.primary schema row) eb <= 0)

            applyLimit limit filtered)

    /// 索引の値に一致する全行を引く（txn の書き込みを含む）。
    let scanByIndexPrefixWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (values: SqlValue array)
        (limit: int)
        =
        let scanIndexPrefix
            (store: RowStore)
            (index: IndexSchema)
            (visible: uint64 option)
            (prefix: byte array)
            (limit: int)
            =
            RowStore.scanAt store (indexSpace index) prefix visible limit
            |> Result.map (fun rows ->
                rows
                |> Seq.takeWhile (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)
                |> Seq.choose snd
                |> applyLimit limit)

        let prefix = KeyBuilder.ofValues values

        let slots =
            Transaction.slotsOf txn (indexSpace index)
            |> List.filter (fun (key, _) -> KeyBuilder.startsWithPrefix key prefix)

        scanIndexPrefix store index visible prefix (storeLimitFor limit slots)
        |> Result.map (fun rows -> overlayRows (RowKey.indexKey schema index) slots rows |> applyLimit limit)

    /// 索引の値が範囲 [lower, upper] に入る行を引く（txn の書き込みを含む）。
    let scanByIndexRangeWithTxn
        (store: RowStore)
        (schema: TableSchema)
        (index: IndexSchema)
        (visible: uint64 option)
        (txn: Transaction)
        (lower: SqlValue array)
        (upper: SqlValue array option)
        (limit: int)
        =
        let scanIndexRange
            (store: RowStore)
            (index: IndexSchema)
            (visible: uint64 option)
            (lowerBytes: byte array)
            (upperBytes: byte array option)
            (limit: int)
            =
            RowStore.scanAt store (indexSpace index) lowerBytes visible limit
            |> Result.map (fun rows ->
                rows
                |> Seq.filter (fun (key, _) -> inIndexRange lowerBytes upperBytes key)
                |> Seq.choose snd
                |> applyLimit limit)

        let lowerBytes = KeyBuilder.ofValues lower
        let upperBytes = upper |> Option.map KeyBuilder.ofValues

        let slots =
            Transaction.slotsOf txn (indexSpace index)
            |> List.filter (fun (key, _) -> inIndexRange lowerBytes upperBytes key)

        scanIndexRange store index visible lowerBytes upperBytes (storeLimitFor limit slots)
        |> Result.map (fun rows -> overlayRows (RowKey.indexKey schema index) slots rows |> applyLimit limit)

    /// 1行を transaction に積む（まだ DB には入らない）。
    /// 索引のエントリも同じ transaction に入れるので、commit 原子性が保たれる。
    let insert (schema: TableSchema) (txn: Transaction) (row: SqlValue array) =
        result {
            do! TableSchema.validateRow schema row
            let tks = tableSpace schema
            let pk = RowKey.primary schema row

            match Transaction.tryFind txn tks pk with
            | Some(Some _) -> return! err (DbError.constraintViolation (sprintf "PRIMARY KEY 制約に違反: %s" schema.Name))
            | _ -> ()

            let next = Transaction.put txn tks pk row
            return! applyIndexChange schema next None (Some row)
        }

    let private replaceRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) (newRow: SqlValue array) =
        let ensureSamePrimaryKey (schema: TableSchema) (oldRow: SqlValue array) (newRow: SqlValue array) =
            if Array.forall2 (=) (TableSchema.primaryKeyOf schema oldRow) (TableSchema.primaryKeyOf schema newRow) then
                ret ()
            else
                err (
                    sprintf "テーブル %s の主キー列は更新できません（DELETE + INSERT を使ってください）" schema.Name
                    |> DbError.constraintViolation
                )

        result {
            do! ensureSamePrimaryKey schema oldRow newRow

            let afterMain =
                Transaction.put txn (tableSpace schema) (RowKey.primary schema newRow) newRow

            return! applyIndexChange schema afterMain (Some oldRow) (Some newRow)
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
            let! row = getByPrimaryKeyWithTxn store schema visible txn key

            match row with
            | None -> return! err (DbError.notFound "更新する行が見つかりません")
            | Some oldRow -> return! replaceRow schema txn oldRow newRow
        }

    /// 走査済みの行をそのまま使って更新する（主キー検索をやり直さない）。
    /// 主キーは変更できない（DELETE + INSERT を使う）。
    let updateRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) (newRow: SqlValue array) =
        result {
            do! TableSchema.validateRow schema newRow
            return! replaceRow schema txn oldRow newRow
        }

    let private removeRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) =
        let next = Transaction.remove txn (tableSpace schema) (RowKey.primary schema oldRow)
        applyIndexChange schema next (Some oldRow) None

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
            let! row = getByPrimaryKeyWithTxn store schema visible txn key

            match row with
            | None -> return! err (sprintf "削除する行が見つかりません: %s" schema.Name |> DbError.notFound)
            | Some oldRow -> return! removeRow schema txn oldRow
        }

    /// 走査済みの行をそのまま使って削除する（主キー検索をやり直さない）。
    let deleteRow (schema: TableSchema) (txn: Transaction) (oldRow: SqlValue array) = removeRow schema txn oldRow
