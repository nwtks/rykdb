// ---------------------------------------------------------------------------
// テスト共通の補助。各テストファイルから `open RykDb.TestHelpers` で使う。
// ---------------------------------------------------------------------------

module internal RykDb.TestHelpers

open System
open System.IO
open System.Text
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

/// 一意な一時ディレクトリを作る。
let newDir (tag: string) =
    let dir =
        Path.Combine(Path.GetTempPath(), sprintf "rykdb-%s-%d" tag DateTime.Now.Ticks)

    Directory.CreateDirectory dir |> ignore
    dir

/// 一時ディレクトリ内でテストを実行し、最後に削除する。
let withDir (tag: string) (f: string -> unit) =
    let dir = newDir tag

    try
        f dir
    finally
        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

/// テスト用メモリエンジンの keyspace ごとの永続状態。
/// ディスクの WAL replay 相当で、store を作り直しても内容が見える。
type private MemoryEngineState() =
    member val Mem = Memtable.empty with get, set
    member val NextSeqNo = 1UL with get, set

/// テスト用のメモリ StorageEngine。
/// ディスク I/O をせず、memtable だけで getAt / scanAt / writeBatch を実装する。
type private MemoryStorageEngine(state: MemoryEngineState, mem: Memtable, nextSeqNo: uint64) =
    interface IStorageEngine with
        member _.NextSeqNo = nextSeqNo
        member _.ActiveSnapshot = 0UL

        member _.GetAt visible key =
            Memtable.getVisible mem key visible
            |> Option.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
            |> ret

        member _.ScanAt visible startKey limit =
            let entries =
                Memtable.rangeScan mem startKey 0
                |> Seq.filter (fun e ->
                    match visible with
                    | Some v -> e.SeqNo <= v
                    | None -> true)
                |> Seq.distinctBy (fun e -> e.Key)
                |> Seq.map (fun e -> SstEntry.make e.Key e.SeqNo e.Value)
                |> Seq.filter (fun e -> e.Value.IsSome)

            (if limit > 0 then Seq.truncate limit entries else entries) |> ret

        member _.WriteBatch entries =
            let nextMem = Memtable.apply entries state.Mem
            state.Mem <- nextMem
            MemoryStorageEngine(state, nextMem, nextSeqNo) :> IStorageEngine |> ret

        member _.WithNextSeqNo next =
            state.NextSeqNo <- max state.NextSeqNo next
            MemoryStorageEngine(state, mem, next) :> IStorageEngine

        member _.WithActiveSnapshot _ =
            MemoryStorageEngine(state, mem, nextSeqNo) :> IStorageEngine

        member _.MaybeFlush() =
            MemoryStorageEngine(state, mem, nextSeqNo) :> IStorageEngine |> ret

        member _.Close() = ret ()

/// メモリ上の StorageEngine を作る。
/// 同じ store の中では keyspace ごとに状態を共有する（再オープンで内容が見える）。
let private newMemoryEngineFactory () =
    let states = System.Collections.Generic.Dictionary<string, MemoryEngineState>()

    fun keyspace ->
        match states.TryGetValue keyspace with
        | true, state -> ret (MemoryStorageEngine(state, state.Mem, state.NextSeqNo) :> IStorageEngine)
        | _ ->
            let state = MemoryEngineState()
            states.[keyspace] <- state
            ret (MemoryStorageEngine(state, state.Mem, state.NextSeqNo) :> IStorageEngine)

/// メモリ上の RowStore を渡す（ディスク I/O なし）。
let withStore (_tag: string) (f: RowStore -> unit) =
    f (RowStore.openStoreWith "" (newMemoryEngineFactory ()))

/// メモリ上の DB を開いてテストし、最後に閉じる（ディスク I/O なし）。
/// `Execute` の戻り値（新しいハンドル）は呼び出し側が `exec` で持ち替える。
let withDb (_tag: string) (f: Database -> unit) =
    let db = ok (Database.openWithEngine "" (newMemoryEngineFactory ()))

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

/// `Database.Execute` の戻り値を「新しいハンドル」に持ち替える。
let exec (db: Database) (sql: string) =
    let next, _ = ok (db.Execute sql)
    next

/// 実行して結果だけ返す。
let query (db: Database) (sql: string) =
    let _, r = ok (db.Query sql)
    r

/// `toSqlLiteral` で 1 行を文字列化する（クライアント比較用）。
let rowText (r: StatementResult) =
    r.Rows
    |> Array.toList
    |> List.map (fun row -> row |> Array.map SqlLiteral.toSqlLiteral |> String.concat ",")

// ---------------------------------------------------------------------------
// 決定的テストのため固定シードを使う。
// ---------------------------------------------------------------------------

/// Random はモジュールスコープに 1 つだけ置く（決定的・mutable 不使用）。
let private rng = Random 20261004
let private rnd () = rng

let nextInt (lo: int) (hi: int) = rnd().Next(lo, hi)

/// NUL・制御文字・多バイト文字を混ぜた文字列を作る（escape 経路と
/// UTF-8 バイト順 = コードポイント順の検証）。
let randomText () =
    let r = rnd ()
    let len = r.Next(0, 14)
    let sb = StringBuilder()

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
    | 7 -> SqlValue.Date(DateOnly.FromDayNumber(r.Next(0, 3_650_000)))
    | 8 -> SqlValue.Timestamp(DateTimeOffset(DateTime(r.Next(0, 2_000_000_000), DateTimeKind.Utc)))
    | _ -> SqlValue.Uuid(Guid(Array.init 16 (fun _ -> byte (r.Next(0, 256)))))

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
