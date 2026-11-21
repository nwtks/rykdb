namespace RykDb.Core

/// 1行取り出す pull 演算子（one-shot）。
/// <remarks>
/// ケース（`Done` / `Step` / `Compute`）はこのシグネチャで隠す。
/// 構築は `Empty` / `ofRow` / `emit` / `compute` / `ofArray` に限る。
/// F# の `private` はアセンブリ内の他ファイルには効かないため、
/// 構築規則はこの `.fsi` でコンパイラに守らせる。
/// </remarks>
type internal Op

[<Struct; NoEquality; NoComparison; RequireQualifiedAccess>]
type internal OpStep =
    | OpDone
    | OpError of error: DbError
    | OpRow of row: SqlValue array * next: Op

module internal Op =
    /// 空の演算子。
    val Empty: Op
    /// 1行だけの演算子。
    val ofRow: SqlValue array -> Op
    /// 1行と「その次」を返す。
    val emit: SqlValue array -> Op -> Op
    /// 「次に 1行を返す関数」から演算子を作る。
    val compute: (unit -> OpStep) -> Op
    /// 配列を順に返す演算子。
    val ofArray: SqlValue array array -> Op
    /// 1行取り出す。
    val next: Op -> OpStep
    /// ソースの次の 1行を引き出して継続に渡す。
    val pull: (SqlValue array -> Op -> OpStep) -> Op -> Op
    /// 行を 1つ変換する。
    val map: (SqlValue array -> DbResult<SqlValue array>) -> (Op -> Op)
    /// 述語で弾く。
    val filter: (SqlValue array -> DbResult<bool>) -> (Op -> Op)
    /// 先頭 n 行だけ。
    val take: int -> Op -> Op
    /// 先頭 n 行を読み飛ばす。
    val skip: (int -> Op -> Op)
    /// 2つの演算子を順に連結する（左をすべて出してから右）。
    val append: Op -> Op -> Op
    /// 全行を上限つきで配列に materialize する。
    val toArrayLimited: int64 -> int64 -> Op -> Result<SqlValue array array, DbError>
    /// 全行を配列に materialize する（無制限）。
    val toArray: (Op -> Result<SqlValue array array, DbError>)
