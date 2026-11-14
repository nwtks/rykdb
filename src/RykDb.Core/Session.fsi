namespace RykDb.Core

/// `Execute` の前後で transaction の状態がどう変わったか。
[<RequireQualifiedAccess>]
type SessionTransactionChange =
    /// transaction の状態は変わっていない。
    | NoChange
    /// この実行で transaction が始まった。
    | Started
    /// この実行で transaction が終わった（COMMIT / ROLLBACK）。
    | Ended

/// Session の結果上限。
type SessionOptions =
    {
        /// 1回の Execute で保持する SELECT 行数の上限。
        MaxResultRows: int64
        /// 1回の Execute で保持する SELECT 行のメモリ概算上限。
        MaxResultBytes: int64
    }

    /// 既定値（100万行 / 256MiB）。
    static member Default: SessionOptions

/// SELECT 結果を 1行ずつ読むリーダー。
[<Class>]
type RowReader =
    /// 結果の列名。
    member Columns: string array

    /// 次の 1行。終端は `Ok None`。
    member TryRead: unit -> Result<SqlValue array option, DbError>

    interface System.IDisposable

/// fsync のグループコミット（テスト用に internal 公開）。
[<Class>]
type internal CommitDurability =
    new: sync: (WalState -> Result<unit, DbError>) -> CommitDurability
    /// 記録済みの失敗。
    member Failure: DbError option
    /// 同期対象の WAL 位置を登録する。
    member Register: pos: WalState -> unit
    /// その位置まで同期されるのを待つ。
    member Await: pos: WalState -> Result<unit, DbError>

/// 1接続のセッション。
[<Class>]
type Session =
    /// DB を開く（background compaction 有効、既定の上限）。
    static member OpenWith: root: string -> Result<Session, DbError>

    /// DB を開く（background compaction 有効、指定した上限）。
    static member OpenWith: root: string * options: SessionOptions -> Result<Session, DbError>

    /// テスト・内部用: 既に開いた `Database` から Session を作る。
    static member internal OpenWith: root: string * options: SessionOptions * db: Database -> Session

    /// 実行中の transaction があるか。
    member InTransaction: bool

    /// テーブル一覧を返す。
    member TableNames: unit -> Result<string list, DbError>

    /// 単一の SELECT を materialize せずに開く。
    member TryOpenReader: sql: string -> Result<RowReader option, DbError>

    /// 単一文または複数文を実行する。
    member Execute: sql: string -> Result<StatementResult, DbError>

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
