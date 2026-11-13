// ---------------------------------------------------------------------------
// テスト共通の補助。各テストファイルから `open RykDb.TestHelpers` で使う。
// ---------------------------------------------------------------------------

module internal RykDb.TestHelpers

open Xunit
open RykDb.Core

/// Entry option を None ならテストを落とす。
let entry (e: Entry option) =
    match e with
    | Some v -> v
    | None ->
        Assert.Fail "エントリが見つかりません"
        Unchecked.defaultof<Entry>

/// Result が Ok でなければテストを落とす。Err なら DbError を表示する。
let ok (r: Result<'T, DbError>) : 'T =
    match r with
    | Ok v -> v
    | Error e ->
        Assert.Fail(sprintf "Result が Error でした: %s" (DbError.toDisplayString e))
        Unchecked.defaultof<'T>

/// Result の Ok 値を取り出す。Err なら 0 を返す（数値比較の補助など）。
let okOr defaultValue (r: Result<'T, DbError>) : 'T =
    match r with
    | Ok v -> v
    | Error _ -> defaultValue

/// テスト専用: 同じ Op 値を 2 回 `Op.next` すると失敗させる包み。
/// `Op` の one-shot 不変条件（元の値を再利用せず、返された継続だけを使う）を機械的に検査する。
let oneShot (label: string) (op: Op) =
    let used = ref false

    Op.compute (fun () ->
        if used.Value then
            failwithf "Op を 2 回消費しました: %s" label

        used.Value <- true
        Op.next op)

/// 一意な一時ディレクトリを作る。
let newDir (tag: string) =
    let dir =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "rykdb-%s-%d" tag System.DateTime.Now.Ticks)

    System.IO.Directory.CreateDirectory dir |> ignore
    dir

/// 一時ディレクトリ内でテストを実行し、最後に削除する。
let withDir (tag: string) (f: string -> unit) =
    let dir = newDir tag

    try
        f dir
    finally
        if System.IO.Directory.Exists dir then
            try
                System.IO.Directory.Delete(dir, true)
            with _ ->
                ()

/// 純粋な seq を SstScan に変換する。
let scanOfEntries (items: seq<SstEntry>) =
    let e = items.GetEnumerator()

    let rec loop () =
        SstScan.ofStep (fun () ->
            if e.MoveNext() then
                ret (Some(e.Current, loop ()))
            else
                (e :> System.IDisposable).Dispose() |> ignore
                ret None)

    loop ()

/// テスト用メモリエンジンの keyspace ごとの永続状態。
/// ディスクの WAL replay 相当で、store を作り直しても内容が見える。
type private MemoryEngineState() =
    member val Mem = Memtable.Empty with get, set
    member val NextSeqNo = 1UL with get, set

/// テスト用のメモリ StorageEngine。
/// ディスク I/O をせず、memtable だけで getAt / scanAt / writeBatch を実装する。
type private MemoryStorageEngine(state: MemoryEngineState) =
    interface IStorageEngine with
        member _.NextSeqNo = state.NextSeqNo
        member _.ActiveSnapshot = 0UL

        member _.HostState =
            {
                Version = Version.Empty
                ManifestNumber = 0L<manifestNo>
                ActiveSnapshot = 0UL
                LastCompactionError = None
                Generation = 0L
            }

        member _.WalState = None

        member _.GetAt visible key =
            Memtable.getVisible key visible state.Mem
            |> Option.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
            |> ret

        member _.ScanAt visible startKey limit =
            let entries =
                Memtable.rangeScan startKey 0 state.Mem
                |> Seq.filter (fun e ->
                    match visible with
                    | Some v -> e.SeqNo <= v
                    | None -> true)
                |> Seq.distinctBy (fun e -> e.Key)
                |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
                |> Seq.filter (fun e -> e.Value.IsSome)

            (if limit > 0 then Seq.truncate limit entries else entries)
            |> scanOfEntries
            |> ret

        member _.WriteBatch entries =
            let nextMem = Memtable.apply entries state.Mem
            state.Mem <- nextMem
            MemoryStorageEngine state :> IStorageEngine |> ret

        member _.WriteBatchDeferred entries =
            let nextMem = Memtable.apply entries state.Mem
            state.Mem <- nextMem
            MemoryStorageEngine state :> IStorageEngine |> ret

        member _.WithNextSeqNo next =
            state.NextSeqNo <- max state.NextSeqNo next
            MemoryStorageEngine state :> IStorageEngine

        member _.WithActiveSnapshot _ =
            MemoryStorageEngine state :> IStorageEngine

        member this.WithHostState _ = this :> IStorageEngine

        member this.FlushIfNeeded _ = ret (this :> IStorageEngine, false)

        member _.MaybeFlush() =
            MemoryStorageEngine state :> IStorageEngine |> ret

        member _.Close() = ret ()

/// メモリ上の StorageEngine を作る（`Session` を直接組み立てるテストにも使う）。
let memoryEngine () =
    let state = MemoryEngineState()
    MemoryStorageEngine state :> IStorageEngine


/// エンジンを差し替えて、空の RowStore を作る（テスト用）。
/// ディスクに触れないメモリ実装を渡せば、全 keyspace が 1本の LSM に載る。
let openStoreWith (root: string) (engine: IStorageEngine) =
    {
        Root = root
        Engine = engine
        Lock = None
        Host = None
    }

/// メモリ上の RowStore を渡す（ディスク I/O なし）。
let withStore (_tag: string) (f: RowStore -> unit) = f (openStoreWith "" (memoryEngine ()))

/// メモリ上の DB を開いてテストし、最後に閉じる（ディスク I/O なし）。
/// `Execute` の戻り値（新しいハンドル）は呼び出し側が `exec` で差し替える。
/// <remarks>
/// root はテストごとに一意にする。root はプロセス内の `RootGate` /
/// `ActiveSnapshotRegistry` のキーなので、空文字を共有すると
/// 並行する他のテストの transaction が DDL の排他判定に引っかかる。
/// store 側の root は空のまま（commit intent をディスクに書かない）。
/// </remarks>
let withDb (_tag: string) (f: Database -> unit) =
    let root = "mem:" + System.Guid.NewGuid().ToString "N"
    let store = openStoreWith "" (memoryEngine ())
    let db = ok (Database.OpenWithStore root store)

    try
        f db
    finally
        ok (db.Close()) |> ignore

/// Error でなければテストを落とす（エラー内容まで見るテストは match で個別に書く）。
let expectError (message: string) (r: Result<'a, DbError>) =
    match r with
    | Error _ -> ()
    | Ok _ -> Assert.Fail message

/// 特定の ErrorKind を返さなければテストを落とす。
/// 「| Error e -> Assert.Equal<ErrorKind>(kind, e.Kind) | Ok _ -> Assert.Fail msg」の定型をまとめる。
let expectErrorKind (kind: ErrorKind) (message: string) (r: Result<'a, DbError>) =
    match r with
    | Error e -> Assert.Equal<ErrorKind>(kind, e.Kind)
    | Ok _ -> Assert.Fail message

// ---------------------------------------------------------------------------
// スキーマを作るときの列定義ビルダー（各ファイルに書くとコピペになるため）
// ---------------------------------------------------------------------------

/// 制約のない列定義。
let col (name: string) (dt: DataType) =
    {
        Name = name
        DataType = dt
        TypeArguments = []
        Constraint = ColumnConstraint.Empty
    }

/// NOT NULL + PRIMARY KEY の列定義。
let pkCol (name: string) (dt: DataType) =
    { col name dt with
        Constraint =
            { ColumnConstraint.Empty with
                NotNull = true
                PrimaryKey = true
            }
    }

/// 出力 1 列（`SELECT` の出力スキーマ。修飾子と型は省略可）。
let outCol (name: string) (qualifier: string option) (dataType: DataType option) =
    {
        Name = name
        Qualifier = qualifier
        DataType = dataType
    }

/// 出力スキーマ（列のリストだけ指定して `Schema.empty` を敷く）。
let outSchema (cols: OutputColumn list) =
    { Schema.empty with
        Columns = cols |> List.toArray
    }

/// 共有のテスト用テーブルスキーマ（CatalogTests / PlannerTests が同一定義を使っていた）。
let peopleSchema: TableSchema =
    {
        Name = "people"
        Columns =
            [
                pkCol "id" DataType.Integer
                col "name" DataType.Text
                col "age" DataType.Integer
            ]
        PrimaryKey = [ 0 ]
        Indexes =
            [
                {
                    Name = "ix_people_age"
                    Columns = [ "age" ]
                    Unique = false
                }
            ]
    }

let ordersSchema: TableSchema =
    {
        Name = "orders"
        Columns =
            [
                pkCol "oid" DataType.Integer
                col "person_id" DataType.Integer
                col "amount" DataType.Integer
            ]
        PrimaryKey = [ 0 ]
        Indexes =
            [
                {
                    Name = "ix_orders_person"
                    Columns = [ "person_id" ]
                    Unique = false
                }
            ]
    }

/// INTEGER でなければテストを落とす。
let asInt (v: SqlValue) =
    match v with
    | SqlValue.Int n -> n
    | _ ->
        Assert.Fail(sprintf "INTEGER ではありません: %A" v)
        0L

/// TEXT でなければテストを落とす。
let asText (v: SqlValue) =
    match v with
    | SqlValue.Text s -> s
    | _ ->
        Assert.Fail(sprintf "TEXT ではありません: %A" v)
        ""

/// テスト用に SQL を 1文へ解析して `Database.Execute` へ渡す。
/// 複数文は公開 API の `Session.Execute` で扱う。
let execSql (db: Database) (sql: string) : Result<Database * StatementResult, DbError> =
    match Parser.parseStatements sql with
    | Error e -> Error e
    | Ok [] -> Ok(db, StatementResult.Empty)
    | Ok [ stmt ] -> db.Execute stmt
    | Ok _ -> failwithf "テストヘルパー execSql は 1文だけ受け付けます: %s" sql

/// `execSql` の戻り値を「新しいハンドル」に差し替える。
let exec (db: Database) (sql: string) =
    let next, _ = ok (execSql db sql)
    next

/// 実行して結果だけ返す。
let query (db: Database) (sql: string) =
    let _, r = ok (execSql db sql)
    r

/// 1 文を実行して「新しい Database」と結果の両方を返す。
let execRes (db: Database) (sql: string) = ok (execSql db sql)

/// `toSqlLiteral` で 1 行を文字列化する（クライアント比較用）。
let rowText (r: StatementResult) =
    r.Rows
    |> Array.toList
    |> List.map (fun row -> row |> Array.map SqlLiteral.toSqlLiteral |> String.concat ",")

// ---------------------------------------------------------------------------
// 決定的テストのため固定シードを使う。
// ---------------------------------------------------------------------------

/// Random はモジュールスコープに 1 つだけ置く（決定的・mutable 不使用）。
let private rng = System.Random 20261004
let private rnd () = rng

let nextInt (lo: int) (hi: int) = rnd().Next(lo, hi)

/// NUL・制御文字・多バイト文字を混ぜた文字列を作る（escape 経路と
/// UTF-8 バイト順 = コードポイント順の検証）。
let randomText () =
    let r = rnd ()
    let len = r.Next(0, 14)
    let sb = System.Text.StringBuilder()

    for _ in 1..len do
        match r.Next(0, 9) with
        | 0 -> sb.Append(char 0) |> ignore
        | 1 -> sb.Append(char 1) |> ignore
        | 2 -> sb.Append(char 0xFF) |> ignore
        | 3 -> sb.Append 'a' |> ignore
        | 4 -> sb.Append 'z' |> ignore
        | 5 -> sb.Append(char (r.Next(32, 127))) |> ignore
        | 6 -> sb.Append(char (r.Next(0x80, 0x800))) |> ignore
        | 7 -> sb.Append(char (r.Next(0x0800, 0xD800))) |> ignore
        | _ -> sb.Append(char (r.Next(0xE000, 0xFFFD))) |> ignore

    sb.ToString()

let randomValue () =
    let r = rnd ()

    match r.Next(0, 10) with
    | 0 -> SqlValue.Null
    | 1 -> SqlValue.Bool(r.Next 2 = 0)
    | 2 -> SqlValue.Int(int64 (r.Next(-5000, 5000)))
    | 3 -> SqlValue.Real(float (r.Next(-5000, 5000)) / float (r.Next(1, 8)))
    | 4 -> SqlValue.Decimal(decimal (r.Next(-5000, 5000)) / decimal (r.Next(1, 8)))
    | 5 -> SqlValue.Text(randomText ())
    | 6 -> SqlValue.Blob(Array.init (r.Next(0, 8)) (fun _ -> byte (r.Next(0, 256))))
    // DateOnly.FromDayNumber は 0..DateOnly.MaxValue.DayNumber のみ受け付ける
    | 7 -> SqlValue.Date(System.DateOnly.FromDayNumber(r.Next(0, 3_650_000)))
    | 8 -> SqlValue.Timestamp(System.DateTimeOffset(System.DateTime(r.Next(0, 2_000_000_000), System.DateTimeKind.Utc)))
    | _ -> SqlValue.Uuid(System.Guid(Array.init 16 (fun _ -> byte (r.Next(0, 256)))))

/// テスト用の全順序。型順位 → 同一型内は SQL 比較。
let compareForSort (a: SqlValue) (b: SqlValue) =
    let ra = a |> SqlValue.dataType |> DataType.rank
    let rb = b |> SqlValue.dataType |> DataType.rank

    if ra <> rb then
        compare ra rb
    else
        match SqlValue.compareValues a b with
        | Ok c -> c
        | Error _ -> 0

/// バイト列の辞書順比較（前方が短いほうが小さい）。
let compareKeyBytes (a: byte[]) (b: byte[]) =
    let n = min a.Length b.Length

    let rec firstDiff i =
        if i >= n then
            compare a.Length b.Length
        else
            let c = compare (int a.[i]) (int b.[i])
            if c = 0 then firstDiff (i + 1) else c

    firstDiff 0

/// 比較結果を見える形にする。型の情報も含めることで描画の衝突を避ける。
let render (vs: SqlValue[]) =
    vs
    |> Array.map (fun v -> (v |> SqlValue.dataType |> DataType.toSqlName) + ":" + SqlLiteral.toSqlLiteral v)

// KeyBuilder の戻り値はそのままバイト列キー
let keyOf (v: SqlValue) = KeyBuilder.ofValues [ v ]
let keyOfTuple (vs: SqlValue[]) = KeyBuilder.ofValues vs

/// 連番の整数から作るテスト用キー。
/// SSTable / MANIFEST / StorageEngine / WAL の `sstKey`、Memtable / BloomFilter の
/// `bloomKey` として 7ファイルに同一実装がコピーされていたので一本化した。
let intKey (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]

/// INTEGER 1 列の行。
/// Memtable / StorageEngine / WAL の `valOf` にあたる（Executor の `intArr` も同義）。
let intRow (i: int) = [| SqlValue.Int(int64 i) |]

/// FileMetaData のテスト用ビルダー（キー lo..hi の連番区間）。
/// ManifestTests と VersionSetTests に同一実装が 2 ファイル分あった。
let fmd (num: int64) (level: int) (lo: int) (hi: int) : FileMetaData =
    {
        FileNumber = num
        Level = level
        Smallest = intKey lo
        Largest = intKey hi
        EntryCount = int64 (hi - lo + 1)
        Size = int64 (hi - lo + 1) * 100L
        MaxSeqNo = uint64 (hi + 1)
    }

/// レベルごとのファイル数。
let levelCounts (v: Version) = v.Files |> Array.map Array.length

/// IndexSchema を永続化形にする。
let indexDefinition (table: string) (index: IndexSchema) : IndexDefinition =
    {
        IndexName = index.Name
        TableName = table
        Columns = index.Columns
        Unique = index.Unique
    }

/// エントリ列から SSTable のバイナリを作る。
let buildSstable (bloomFalsePositiveRate: float) (entries: SstEntry array) =
    let build () =
        use stream = new System.IO.MemoryStream()

        Sstable.writeTo stream bloomFalsePositiveRate entries
        |> Result.map (fun _ -> stream.ToArray())

    match protect build with
    | Ok result -> result
    | Error e -> Error e
