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
    | None -> failwith "エントリが見つかりません"

/// Option が Some でなければテストを落とす。
/// `None` のまま `.Value` を呼ぶと NullReferenceException になり、
/// どのテストがどの理由で落ちたか分からなくなるのを避ける。
let expectSome (label: string) (o: 'T option) =
    match o with
    | Some v -> v
    | None -> failwithf "%s: None です" label

/// Version の不変条件（レベル構成・番号の一意性・範囲の並び）を検査する。
/// VersionSetTests と VersionCoordinatorTests（並行ストレス）で共有する。
let validateVersion (v: Version) =
    let fail msg =
        err (DbError.internalError ("Version: " + msg))

    let checkLevel () level =
        let files = v.Files.[level]

        let checkTag =
            if files |> Array.forall (fun f -> f.Level = level) then
                ret ()
            else
                fail "ファイルの Level と格納位置が不一致"

        let checkOrder =
            seq { 1 .. files.Length - 1 }
            |> foldOk
                (fun () i ->
                    (if Bytes.compare files.[i - 1].Smallest files.[i].Smallest <= 0 then
                         ret ()
                     else
                         fail "ファイルが Smallest 昇順に並んでいない")
                    >>= fun () ->
                        if level > 0 then
                            if Bytes.compare files.[i - 1].Largest files.[i].Smallest < 0 then
                                ret ()
                            else
                                fail "L1 以上のファイル範囲が重複"
                        else
                            ret ())
                ()

        checkTag >>= fun () -> checkOrder

    result {
        do!
            if v.Files.Length = Levels.Count then
                ret ()
            else
                fail "レベルの数が不正"

        let all = v.Files |> Array.collect id
        let numbers = all |> Array.map _.FileNumber

        do!
            if (numbers |> Array.distinct).Length = numbers.Length then
                ret ()
            else
                fail "ファイル番号が重複"

        do!
            if all |> Array.forall (fun f -> f.FileNumber < v.NextFileNumber) then
                ret ()
            else
                fail "NextFileNumber が巻き戻っている"

        do!
            if all |> Array.forall (fun f -> f.Level >= 0 && f.Level <= Levels.Max) then
                ret ()
            else
                fail "レベルが範囲外"

        do!
            if all |> Array.forall (fun f -> Bytes.compare f.Smallest f.Largest <= 0) then
                ret ()
            else
                fail "Smallest が Largest より大きい"

        return! seq { 0 .. Levels.Max } |> foldOk checkLevel ()
    }

/// Result が Ok でなければテストを落とす。Err なら DbError を表示する。
let ok (r: Result<'T, DbError>) : 'T =
    match r with
    | Ok v -> v
    | Error e -> failwithf "Result が Error でした: %s" (DbError.toDisplayString e)

/// Result の Ok 値を取り出す。Err なら 0 を返す（数値比較の補助など）。
let okOr defaultValue (r: Result<'T, DbError>) : 'T =
    match r with
    | Ok v -> v
    | Error _ -> defaultValue

/// DbResult の成功値を取り出す。Err ならテストを失敗させる。
let okV (r: DbResult<'T>) : 'T =
    match r with
    | DbOk v -> v
    | DbErr e -> failwithf "DbResult が Error でした: %s" (DbError.toDisplayString e)

/// DbResult が Error であることを確認する。
let expectErrorV message (r: DbResult<'T>) =
    match r with
    | DbErr _ -> ()
    | DbOk _ -> Assert.Fail message

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
        System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            sprintf "rykdb-%s-%d-%s" tag System.DateTime.Now.Ticks (System.Guid.NewGuid().ToString "N")
        )

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

/// スピルを強制する小さな作業領域（一時ファイルもこのディレクトリへ）。
/// OpsTests / OpsJoinTests / SpillTests で共有する。
let tightBudget (dir: string) (bytes: int64) =
    { MemoryBudget.Default with
        Work = bytes
        Sort = bytes
        Directory = Some dir
    }

/// 退避に使った一時ファイルが残っていないことを確かめる。
let assertNoLeftovers (dir: string) =
    let files = System.IO.Directory.GetFiles dir

    if files.Length > 0 then
        Assert.Fail(sprintf "一時ファイルが残っています: %A" (files |> Array.map System.IO.Path.GetFileName))

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

        member _.PublishedState =
            {
                Version = Version.Empty
                ManifestNumber = 0L<manifestNo>
                ActiveSnapshot = 0UL
                LastCompactionError = None
                Generation = 0L
            }

        member _.WalState = None
        member _.LastFlushError = None

        member _.GetAtMasked visible key mask =
            Memtable.getVisible key visible state.Mem
            |> Option.map (fun e ->
                let value = e.Value |> Option.map (SqlValue.maskRow mask)
                SstEntry.make e.Key e.SeqNo value)
            |> ret

        member _.ScanAtValues visible startKey limit needsValues =
            let entries =
                Memtable.rangeScan startKey 0 state.Mem
                |> Seq.filter (fun e ->
                    match visible with
                    | Some v -> e.SeqNo <= v
                    | None -> true)
                |> Seq.distinctBy (fun e -> e.Key)
                |> Seq.map (fun e ->
                    let value =
                        if needsValues || e.Value.IsNone then
                            e.Value
                        else
                            Some(Array.empty<SqlValue>)

                    SstEntry.make e.Key e.SeqNo value)
                |> Seq.filter (fun e -> e.Value.IsSome)

            (if limit > 0 then Seq.truncate limit entries else entries)
            |> scanOfEntries
            |> ret

        member _.ScanAtMasked visible startKey limit mask =
            let entries =
                Memtable.rangeScan startKey 0 state.Mem
                |> Seq.filter (fun e ->
                    match visible with
                    | Some v -> e.SeqNo <= v
                    | None -> true)
                |> Seq.distinctBy (fun e -> e.Key)
                |> Seq.map (fun e ->
                    let value =
                        match e.Value with
                        | None -> None
                        | Some _ when mask.Length = 0 -> Some(Array.empty<SqlValue>)
                        | Some row ->
                            Some(
                                Array.mapi
                                    (fun i v ->
                                        if i < mask.Length && not mask.[i] then
                                            SqlValue.Unloaded
                                        else
                                            v)
                                    row
                            )

                    SstEntry.make e.Key e.SeqNo value)
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
            ret (MemoryStorageEngine state :> IStorageEngine, None)

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
        Runtime = RootRuntime()
    }

/// メモリ上の RowStore を渡す（ディスク I/O なし）。
let withStore (_tag: string) (f: RowStore -> unit) = f (openStoreWith "" (memoryEngine ()))

/// メモリ上の DB を開いてテストし、最後に閉じる（ディスク I/O なし）。
/// `Session` は同じインスタンスを使い続ける（`exec` は同じ Session を返す）。
/// <remarks>
/// root はテストごとに一意にする。
/// root は DB ごとの RootRuntime / active snapshot 登録のキーなので、
/// 空文字を共有すると並行する他のテストの transaction が DDL の排他判定に引っかかる。
/// store 側の root は空のまま（commit intent をディスクに書かない）。
/// </remarks>
let withSession (_tag: string) (f: Session -> unit) =
    let root = "mem:" + System.Guid.NewGuid().ToString "N"
    let store = openStoreWith "" (memoryEngine ())
    let session = ok (Session.OpenWithStore(root, SessionOptions.Default, store))

    try
        f session
    finally
        ok (session.Close()) |> ignore

/// `withSession` と同じだが、`DatabaseHost` も渡す（registry / root を直接見るテスト用）。
let withDbHost (_tag: string) (f: DatabaseHost -> Session -> unit) =
    let root = "mem:" + System.Guid.NewGuid().ToString "N"
    let store = openStoreWith "" (memoryEngine ())
    let host = ok (DatabaseHost.OpenWithStore(root, store))
    let session = Session.Create(host, SessionOptions.Default, true)

    try
        f host session
    finally
        ok (session.Close()) |> ignore

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

/// 「<ラベル>: <実際の値（%A）>」でテストを落とす。
/// <remarks>
/// variant 不一致・想定外の形の定型（`| other -> failActual "X ではない" other`）。
/// 実際の値を必ず表示するので、原因の切り分けに失敗メッセージだけが使える。
/// </remarks>
let failActual (label: string) (actual: 'a) =
    Assert.Fail(sprintf "%s: %A" label actual)

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
    { Schema.Empty with
        Columns = cols |> List.toArray
    }

/// 2 列（a, b）の出力スキーマ（Eval / Op / Ops / ExtSort / Catalog のテストで共有）。
let schema2 =
    outSchema
        [
            outCol "a" None (Some DataType.Integer)
            outCol "b" None (Some DataType.Integer)
        ]

/// ANALYZE の列統計。
let colStats
    (name: string)
    (ndv: int64)
    (nulls: int64)
    (minValue: float option)
    (maxValue: float option)
    : ColumnStats =
    {
        Name = name
        NDV = ndv
        Nulls = nulls
        Min = minValue
        Max = maxValue
        Histogram = []
        Mcv = []
    }

/// 頻出値付きの列統計。
let colStatsMcv (name: string) (ndv: int64) (nulls: int64) (mcv: (string * int64) list) : ColumnStats =
    { colStats name ndv nulls None None with
        Mcv = mcv
    }

/// ヒストグラム付きの列統計。
let colStatsHist
    (name: string)
    (ndv: int64)
    (nulls: int64)
    (minValue: float option)
    (maxValue: float option)
    (histogram: float list)
    : ColumnStats =
    { colStats name ndv nulls minValue maxValue with
        Histogram = histogram
    }

/// ANALYZE の索引統計。
let idxStats (name: string) (columns: string list) (prefixNdv: int64 list) : IndexStats =
    {
        Name = name
        Columns = columns
        PrefixNDV = prefixNdv
    }

/// ANALYZE のテーブル統計。
let tableStats (table: string) (rows: int64) (columns: ColumnStats list) (indexes: IndexStats list) : TableStats =
    {
        Table = table
        RowCount = rows
        Columns = columns
        Indexes = indexes
        AnalyzedAt = None
        Changes = 0L
        RowBytes = 0.0
    }

/// 共有のテスト用テーブルスキーマ。
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

/// 索引なしの people スキーマ（DML 経路のテスト用）。
/// `peopleSchema` は索引付きで NOT NULL 検証が付くため、索引を触らない
/// ExecContext / DmlOps のテストではこちらを使う。
let plainPeopleSchema: TableSchema =
    {
        Name = "people"
        Columns =
            [
                col "id" DataType.Integer
                col "name" DataType.Text
                col "age" DataType.Integer
            ]
        PrimaryKey = [ 0 ]
        Indexes = []
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

/// people / orders を登録済みのテスト用 ExecContext
/// （Analyzer / Optimizer / PlanBuilder / Plan のテストで共有）。
let ctxOf (store: RowStore) =
    ExecContext.create store None
    |> ExecContext.withSchema "people" peopleSchema
    |> ExecContext.withSchema "orders" ordersSchema

/// 修飾子つきの列参照（people / orders）。
let pCol (n: string) =
    Expr.Column { Qualifier = Some "people"; Name = n }

let oCol (n: string) =
    Expr.Column { Qualifier = Some "orders"; Name = n }

/// テスト用の SELECT 文（句は Where まで指定し、残りは既定値）。
let selectOf (items: SelectItem list) (from: TableRef option) (joins: JoinClause list) (w: Expr option) =
    {
        Distinct = false
        Items = items
        From = Option.map TableSource.Named from
        Joins = joins
        Where = w
        GroupBy = []
        Having = None
        OrderBy = []
        Limit = None
        Offset = None
        SetOperations = []
    }

/// INTEGER でなければテストを落とす。
let asInt (v: SqlValue) =
    match v with
    | SqlValue.Int n -> n
    | _ -> failwithf "INTEGER ではありません: %A" v

/// TEXT でなければテストを落とす。
let asText (v: SqlValue) =
    match v with
    | SqlValue.Text s -> s
    | _ -> failwithf "TEXT ではありません: %A" v

// ---------------------------------------------------------------------------
// Parser テストの共通ヘルパー（ParserTests / ParserExprTests で同一実装が
// 2 ファイル分あったものを集約。失敗は failwith 系で総和化する）
// ---------------------------------------------------------------------------

/// SQL テキストを 1 文として解析する。複数文はエラー。
let parseStatement (sql: string) =
    Parser.parseStatements sql
    >>= fun stmts ->
        match stmts with
        | [ s ] -> ret s
        | [] -> err (DbError.create ErrorKind.SyntaxError "文がありません")
        | _ -> err (DbError.create ErrorKind.SyntaxError "複数の文が渡されました（1 文ずつ実行してください）")

/// SQL を 1 文解析し、失敗したらテストを落とす。
let parseOneOk (sql: string) : Statement =
    match parseStatement sql with
    | Ok s -> s
    | Error e -> failwithf "解析に失敗: %s\nSQL: %s" (DbError.toDisplayString e) sql

/// SQL を 1 文解析し、variant から値を抜き出せなければテストを落とす。
let expectVariant (tag: string) (pick: Statement -> 'T option) (sql: string) : 'T =
    match pick (parseOneOk sql) with
    | Some v -> v
    | None -> failwithf "%s ではない: %s" tag sql

let parseSelectOk (sql: string) : SelectStatement =
    expectVariant
        "SELECT"
        (function
        | Statement.Select s -> Some s
        | _ -> None)
        sql

/// SELECT 文の WHERE 式を取り出す（無い・SELECT でないならテストを落とす）。
let whereOf (sql: string) : Expr =
    match (parseSelectOk sql).Where with
    | Some e -> e
    | None -> failwithf "WHERE 句がありません: %s" sql

/// テスト用に SQL を 1文へ解析して `Session.ExecuteStatement` へ渡す。
/// 複数文は公開 API の `Session.Execute` で扱う。
let execSql (session: Session) (sql: string) : Result<Session * StatementResult, DbError> =
    match Parser.parseStatements sql with
    | Error e -> Error e
    | Ok [] -> Ok(session, StatementResult.Empty)
    | Ok [ stmt ] -> session.ExecuteStatement stmt |> Result.map (fun result -> session, result)
    | Ok _ -> failwithf "テストヘルパー execSql は 1文だけ受け付けます: %s" sql

/// `execSql` の戻り値から Session を取り出す（同じインスタンスを返す）。
let exec (session: Session) (sql: string) =
    let next, _ = ok (execSql session sql)
    next

/// 実行して結果だけ返す。
let query (session: Session) (sql: string) =
    let _, r = ok (execSql session sql)
    r

/// 1 文を実行して「Session」と結果の両方を返す。
let execRes (session: Session) (sql: string) = ok (execSql session sql)

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


/// 値どうしの比較。
let compareValues (a: SqlValue) (b: SqlValue) =
    SqlValue.compareValuesV a b |> DbResult.toResult

/// テスト用の全順序。型順位 → 同一型内は SQL 比較。
let compareForSort (a: SqlValue) (b: SqlValue) =
    let ra = a |> SqlValue.dataType |> ok |> DataType.rank
    let rb = b |> SqlValue.dataType |> ok |> DataType.rank

    if ra <> rb then
        compare ra rb
    else
        match compareValues a b with
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
    |> Array.map (fun v ->
        (v |> SqlValue.dataType |> ok |> DataType.toSqlName)
        + ":"
        + SqlLiteral.toSqlLiteral v)

// KeyBuilder の戻り値はそのままバイト列キー（テストでは Ok 前提）
let keyOf (v: SqlValue) = KeyBuilder.ofValues [ v ] |> ok
let keyOfTuple (vs: SqlValue[]) = KeyBuilder.ofValues vs |> ok

/// 連番の整数から作るテスト用キー。
let intKey (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ] |> ok

/// INTEGER 1 列の行。
let intRow (i: int) = [| SqlValue.Int(int64 i) |]

/// SSTable 用の連番エントリ（キー昇順・同一キーなし。SeqNo は 1..n）。
/// `SstScanTests` と `SstableTests` の両方で使う。
let sstEntries (n: int) =
    Array.init n (fun i -> SstEntry.make (intKey i) (uint64 (i + 1)) (Some(intRow i)))
    |> Array.sortWith SstEntry.compare

/// FileMetaData のテスト用ビルダー（キー lo..hi の連番区間）。
let fmd (num: int64) (level: int) (lo: int) (hi: int) : FileMetaData =
    {
        FileNumber = num
        Level = level
        Smallest = intKey lo
        Largest = intKey hi
        EntryCount = int64 (hi - lo + 1)
        Size = int64 (hi - lo + 1) * 100L
        MaxSeqNo = uint64 (hi + 1)
        ReadInfo = None
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

        SstableWriter.writeTo stream bloomFalsePositiveRate entries.Length entries
        |> Result.map (fun _ -> stream.ToArray())

    match protect build with
    | Ok result -> result
    | Error e -> Error e

/// OpsJoin のテスト用フィクスチャ（OpsJoinTests / OpsJoinHashTests / OpsJoinMergeTests で共有）。
module OpsJoinFixtures =
    /// このモジュールのテストは上限なし（スピルしない）経路を検証する。
    /// スピル経路は専用のバジェットで別途テストする。
    module UnlimitedOpsJoin =
        let innerJoin = OpsJoin.innerJoin MemoryBudget.Unlimited
        let leftJoin = OpsJoin.leftJoin MemoryBudget.Unlimited
        let innerJoinHash = OpsJoinHash.innerJoinHash MemoryBudget.Unlimited

        let innerJoinHashBuildLeft =
            OpsJoinHash.innerJoinHashBuildLeft MemoryBudget.Unlimited

        let leftJoinHash = OpsJoinHash.leftJoinHash MemoryBudget.Unlimited
        let innerJoinMerge = OpsJoinMerge.innerJoinMerge MemoryBudget.Unlimited
        let leftJoinMerge = OpsJoinMerge.leftJoinMerge MemoryBudget.Unlimited
        let innerJoinIndexed = OpsJoin.innerJoinIndexed
        let leftJoinIndexed = OpsJoin.leftJoinIndexed

    let leftSchema = outSchema [ outCol "lid" (Some "l") (Some DataType.Integer) ]

    let rightSchema = outSchema [ outCol "rid" (Some "r") (Some DataType.Integer) ]

    let lcol = Expr.Column { Qualifier = Some "l"; Name = "lid" }
    let rcol = Expr.Column { Qualifier = Some "r"; Name = "rid" }

    /// 行の並びを順序非依存の文字列にする（結合アルゴリズムが変わっても比較できる）。
    let rowsText (rows: SqlValue array array) =
        rows
        |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
        |> Array.sort
        |> String.concat "|"

    /// 重複キーを含む左右の入力（左 40行 / 右 24行）。
    let dupLeft () =
        Op.ofArray [| for i in 0..39 -> [| SqlValue.Int(int64 (i % 5)) |] |]

    let dupRight () =
        Op.ofArray [| for i in 0..23 -> [| SqlValue.Int(int64 (i % 8)) |] |]

    let onEq = Expr.Binary(BinaryOp.Eq, lcol, rcol)

    /// 複合キー (a, b) と、結果の同一性を確かめるための値 v を持つスキーマ。
    let diffLeftSchema =
        outSchema
            [
                outCol "a" (Some "l") (Some DataType.Integer)
                outCol "b" (Some "l") (Some DataType.Integer)
                outCol "v" (Some "l") (Some DataType.Integer)
            ]

    let diffRightSchema =
        outSchema
            [
                outCol "a" (Some "r") (Some DataType.Integer)
                outCol "b" (Some "r") (Some DataType.Integer)
                outCol "v" (Some "r") (Some DataType.Integer)
            ]

    let diffColumn side name =
        Expr.Column { Qualifier = Some side; Name = name }

    let diffKeyEq =
        Expr.Binary(
            BinaryOp.And,
            Expr.Binary(BinaryOp.Eq, diffColumn "l" "a", diffColumn "r" "a"),
            Expr.Binary(BinaryOp.Eq, diffColumn "l" "b", diffColumn "r" "b")
        )

    let diffResidual = Expr.Binary(BinaryOp.Lt, diffColumn "l" "v", diffColumn "r" "v")

    /// NULL を最小としてキー列を比較する（マージ結合用の整列）。
    let compareDiffKeys (a: SqlValue array) (b: SqlValue array) =
        let rec go i =
            if i >= 2 then
                0
            else
                match a.[i], b.[i] with
                | SqlValue.Null, SqlValue.Null -> go (i + 1)
                | SqlValue.Null, _ -> -1
                | _, SqlValue.Null -> 1
                | x, y ->
                    match SqlValue.compareValuesV x y with
                    | DbOk 0 -> go (i + 1)
                    | DbOk c -> c
                    | DbErr _ -> 0

        go 0

    /// 重複キー・NULL キー・複合キーを含むランダムな行。
    let diffRows (seed: int) (count: int) =
        let rnd = System.Random(seed)

        Array.init count (fun i ->
            let key modulus =
                if rnd.Next(0, 8) = 0 then
                    SqlValue.Null
                else
                    SqlValue.Int(int64 (rnd.Next(0, modulus)))

            [| key 5; key 4; SqlValue.Int(int64 i) |])

    /// 右テーブルの索引探索を模した lookup（キーは `compareValuesV` で比較する）。
    let diffLookup (rows: SqlValue array array) (keys: int array) (key: SqlValue array) : Result<Op, DbError> =
        let sameKey (row: SqlValue array) =
            let rec go i =
                if i >= keys.Length then
                    true
                else
                    match SqlValue.compareValuesV row.[keys.[i]] key.[i] with
                    | DbOk 0 -> go (i + 1)
                    | _ -> false

            go 0

        rows |> Array.filter sameKey |> Op.ofArray |> ret

    let assertSameRows (label: string) (expected: SqlValue array array) (actual: SqlValue array array) =
        if rowsText expected <> rowsText actual then
            Assert.Fail(sprintf "%s: 結果が一致しません\n期待: %s\n実際: %s" label (rowsText expected) (rowsText actual))

/// Ops のテスト用フィクスチャ（OpsTests / OpsSortTests / OpsDistinctTests / OpsAggregateTests で共有）。
module OpsFixtures =
    /// このモジュールのテストは上限なし（スピルしない）経路を検証する。
    /// スピル経路は専用のバジェットで別途テストする。
    module UnlimitedOps =
        let filter = Ops.filter

        let sortBy schema keys src =
            OpsSort.sortBy MemoryBudget.Unlimited schema [] [] keys src

        let distinct = OpsDistinct.distinct MemoryBudget.Unlimited
        let aggregate = OpsAggregate.aggregate MemoryBudget.Unlimited
        let havingFilter = OpsAggregate.havingFilter

    /// 行の配列を Op にする（入れ子の配列推論を避けるため明示的に作る）。
    let opOfRows (rows: SqlValue array list) = Op.ofArray (List.toArray rows)

    /// 1 行を作る。
    let row (values: SqlValue list) = List.toArray values

    /// SqlValue から値を取り出す（テスト用）。
    let asDec (v: SqlValue) =
        match v with
        | SqlValue.Decimal d -> d
        | SqlValue.Int i -> decimal i
        | other -> failwithf "DECIMAL ではありません: %A" other

    let asReal (v: SqlValue) =
        match v with
        | SqlValue.Real f -> f
        | other -> failwithf "REAL ではありません: %A" other

    let col (name: string) =
        Expr.Column { Qualifier = None; Name = name }

    /// 行の並びを表示用の文字列にする（スピル有無の比較用）。
    let rowsText (rows: SqlValue array array) =
        rows
        |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
        |> String.concat "|"

    /// 退避に使った一時ファイルが残っていないことを確かめる（サイズ付きの詳細表示）。
    let assertNoLeftoversDetailed (dir: string) =
        let files = System.IO.Directory.GetFiles dir

        if files.Length > 0 then
            let detail =
                files
                |> Array.map (fun p -> sprintf "%s(%d)" (System.IO.Path.GetFileName p) (System.IO.FileInfo(p).Length))
                |> String.concat " "

            Assert.Fail(sprintf "一時ファイルが残っています: %s" detail)
