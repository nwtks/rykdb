namespace RykDb.Core

/// 1行取り出す pull 演算子（one-shot）。
/// <remarks>
/// ケース（`Done` / `Step` / `Compute`）はこのシグネチャで隠す。
/// 構築は `empty` / `ofRow` / `emit` / `compute` / `ofArray` に限る。
/// F# の `private` はアセンブリ内の他ファイルには効かないため、
/// 構築規則はこの `.fsi` でコンパイラに守らせる。
/// </remarks>
type internal Op

module internal Op =
    /// 空の演算子。
    val Empty: Op

    /// 1行だけの演算子。
    val ofRow: SqlValue array -> Op

    /// 1行と「その次」を返す。
    val emit: SqlValue array -> Op -> Op

    /// 「次に 1行を返す関数」から演算子を作る。
    val compute: (unit -> Result<(SqlValue array * Op) option, DbError>) -> Op

    /// 配列を順に返す演算子。
    val ofArray: SqlValue array array -> Op

    /// 1行取り出す。
    val next: Op -> Result<(SqlValue array * Op) option, DbError>

    /// ソースの次の 1行を引き出して継続に渡す。
    val pull: (SqlValue array -> Op -> Result<(SqlValue array * Op) option, DbError>) -> Op -> Op

    /// 行を 1つ変換する。
    val map: (SqlValue array -> Result<SqlValue array, DbError>) -> (Op -> Op)

    /// 述語で弾く。
    val filter: (SqlValue array -> Result<bool, DbError>) -> (Op -> Op)

    /// 先頭 n 行だけ。
    val take: int -> Op -> Op

    /// 先頭 n 行を読み飛ばす。
    val skip: (int -> Op -> Op)

    /// 全行を上限つきで配列に materialize する。
    val toArrayLimited: int64 -> int64 -> Op -> Result<SqlValue array array, DbError>

    /// 全行を配列に materialize する（無制限）。
    val toArray: (Op -> Result<SqlValue array array, DbError>)
