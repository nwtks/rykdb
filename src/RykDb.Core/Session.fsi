namespace RykDb.Core

/// Session の結果上限と作業メモリ。
[<NoEquality; NoComparison>]
type SessionOptions =
    {
        /// 1回の Execute で保持する SELECT 行数の上限。
        /// `-1` で無制限（`-2` 以下はエラー）。
        MaxResultRows: int64
        /// 1回の Execute で保持する SELECT 行のメモリ概算上限。
        /// `-1` で無制限（`-2` 以下はエラー）。
        MaxResultBytes: int64
        /// JOIN / DISTINCT / GROUP BY / IN が作業用に保持してよいメモリの概算上限（バイト）。
        /// 超えた分は一時ファイルへ退避する。`-1` で無制限（`-2` 以下はエラー）。
        WorkMemBytes: int64
        /// ORDER BY の受け皿（メモリ内で整列する上限）。
        /// 超えた分は一時ファイルの run へ退避する。`-1` で無制限（`-2` 以下はエラー）。
        SortBufferBytes: int64
        /// 一時ファイルの置き場所（`None` は OS の一時ディレクトリ）。
        SpillDirectory: string option
        /// ANALYZE のキャンセル。
        CancelAnalysis: System.Threading.CancellationToken
        /// ANALYZE の進捗（処理名, 処理済み行数）。
        OnAnalyzeProgress: (string * int64 -> unit) option
    }

    /// 既定値（結果 100万行 / 256MiB、作業 256MiB、ソート受け皿 64MiB）。
    static member Default: SessionOptions

/// SELECT 結果を 1行ずつ読むリーダー。
[<Class>]
type RowReader =
    /// 結果の列名。
    member Columns: string array
    /// 次の 1行。終端は `Ok None`。
    member TryRead: unit -> Result<LoadedRow option, DbError>
    interface System.IDisposable

/// 1接続のセッション。
[<Class>]
type Session =
    /// DB を開く（background compaction 有効、既定の上限）。
    static member OpenWith: root: string -> Result<Session, DbError>
    /// DB を開く（background compaction 有効、指定した上限）。
    static member OpenWith: root: string * options: SessionOptions -> Result<Session, DbError>
    /// テスト・ベンチ用: 同期 compaction で開く（ホストを所有する）。
    static member internal OpenWithSync: root: string * options: SessionOptions -> Result<Session, DbError>

    /// テスト・内部用: 既に開いた `RowStore` から Session を作る（ホストを所有する）。
    static member internal OpenWithStore:
        root: string * options: SessionOptions * store: RowStore -> Result<Session, DbError>

    /// テスト・内部用: 既に開いた `DatabaseHost` から接続を作る。
    static member internal Create: host: DatabaseHost * options: SessionOptions * ownsHost: bool -> Session

    /// 実行中の transaction があるか。
    member InTransaction: bool
    /// 直近の flush 失敗（成功したら None）。
    member LastFlushError: DbError option
    /// 直近の background compaction の失敗（成功したら None）。
    member LastCompactionError: DbError option
    /// テーブル一覧を返す。
    member TableNames: unit -> Result<string list, DbError>
    /// 単一の SELECT を materialize せずに開く。
    member TryOpenReader: sql: string -> Result<RowReader option, DbError>
    /// パラメータ付きで単一の SELECT を materialize せずに開く。
    /// `?` は出現順に `parameters` の値へ置き換えられる。
    /// 値は SQL 文字列へ連結せず式として埋め込むため、SQL インジェクションは起こらない。
    member TryOpenReader: sql: string * parameters: SqlValue array -> Result<RowReader option, DbError>
    /// 単一文または複数文を実行する。
    member Execute: sql: string -> Result<StatementResult, DbError>
    /// パラメータ付きで単一文または複数文を実行する。
    /// SQL 中の `?` は出現順に `parameters` の値へ置き換えられる。
    /// 個数が一致しない場合は `TypeError` になる。
    /// 値は SQL 文字列へ連結せず式として埋め込むため、SQL インジェクションは起こらない。
    member Execute: sql: string * parameters: SqlValue array -> Result<StatementResult, DbError>
    /// 解析済みの 1文を実行する（テスト・ベンチ用）。
    member internal ExecuteStatement: stmt: Statement -> Result<StatementResult, DbError>
    /// SELECT の計画だけを組み立てる（テスト・ベンチ用。書き込みを挟まない用途限定）。
    member internal PrepareSelect: stmt: SelectStatement -> Result<PlannedQuery, DbError>
    /// トランザクションを始める。
    member Begin: unit -> Result<unit, DbError>
    /// COMMIT。
    member Commit: unit -> Result<unit, DbError>
    /// ROLLBACK。
    member Rollback: unit -> Result<unit, DbError>
    /// このセッションを閉じる。
    member Close: unit -> Result<unit, DbError>
    interface System.IDisposable

/// 同一 DB を共有する複数の `Session`（接続）を発行する。
[<Class>]
type SessionPool =
    /// DB を開く（既定の上限）。
    static member OpenWith: root: string -> Result<SessionPool, DbError>
    /// DB を開く（指定した上限）。
    static member OpenWith: root: string * options: SessionOptions -> Result<SessionPool, DbError>
    /// 接続用の Session を作る。
    member CreateSession: unit -> Result<Session, DbError>
    /// DB 全体を閉じる。
    member Close: unit -> Result<unit, DbError>
    interface System.IDisposable
