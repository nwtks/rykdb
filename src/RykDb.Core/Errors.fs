namespace RykDb.Core

/// 失敗の分類。
/// エラー報告と回復可能性の判断に使う。
[<RequireQualifiedAccess>]
type ErrorKind =
    /// トークナイザ/パーサが文法を満たさない入力を検出した
    | SyntaxError
    /// テーブル・列・インデックスなどの名前解決に失敗した
    | NameError
    /// 型の不一致や不正な型変換
    | TypeError
    /// NOT NULL / UNIQUE / PRIMARY KEY などの制約違反
    | ConstraintViolation
    /// 存在しないテーブル・インデックス・行を参照した
    | NotFound
    /// 既に存在する名前で作成しようとした
    | AlreadyExists
    /// memtable やバッファのメモリ上限に達した
    | CapacityError
    /// チェックサム不一致など、ファイル内容が壊れている
    | CorruptionError
    /// MVCC の write-write 競合など、同時実行上の衝突
    | ConflictError
    /// ファイル I/O の失敗
    | IoError
    /// 内部不整合。バグの兆候
    | InternalError

/// すべての失敗はこのレコードで表す。
type DbError =
    {
        Kind: ErrorKind
        Message: string
        Position: int option
        Detail: string option
    }

[<RequireQualifiedAccess>]
module DbError =
    let create kind message =
        {
            Kind = kind
            Message = message
            Position = None
            Detail = None
        }

    /// 構文エラー。
    /// 行番号を伴う。
    let syntax message line =
        {
            Kind = ErrorKind.SyntaxError
            Message = message
            Position = Some line
            Detail = None
        }

    let inline name message = create ErrorKind.NameError message
    let inline typeError message = create ErrorKind.TypeError message

    let inline constraintViolation message =
        create ErrorKind.ConstraintViolation message

    let inline notFound message = create ErrorKind.NotFound message
    let inline alreadyExists message = create ErrorKind.AlreadyExists message
    let inline capacity message = create ErrorKind.CapacityError message

    let inline corruption message =
        create ErrorKind.CorruptionError message

    let inline conflict message = create ErrorKind.ConflictError message
    let inline internalError message = create ErrorKind.InternalError message

    /// .NET の I/O 例外を DbError に包む。
    let io (e: exn) =
        {
            Kind = ErrorKind.IoError
            Message = e.Message
            Position = None
            Detail = Some(e.GetType().Name)
        }

    /// ユーザー向けの表示用メッセージ。
    let toDisplayString (e: DbError) =
        match e.Position, e.Detail with
        | Some p, _ -> sprintf "%s (位置 %d行)" e.Message p
        | None, Some d -> sprintf "%s [%s]" e.Message d
        | None, None -> e.Message

/// 成功値の割当を避けるための内部専用 Result。
/// <remarks>
/// FSharp.Core の `Result` は参照 DU なので、成功パスでも `Ok` を 1個確保する。
/// 行ごと・値ごとに評価するホットパスではこの struct 版を使い、
/// 公開 API の境界（`Session` / `Op.toArrayLimited`）で `Result` に戻す。
/// ケースは struct DU のためフィールド名を付ける（FS3585）。
/// `Unchecked.defaultof` で作らないこと（tag 0 = `DbOk` の既定値になる）。
/// コレクションの要素にしないこと（セルが大きくなり、コピーも増える）。
/// </remarks>
[<Struct>]
type internal DbResult<'T> =
    | DbOk of value: 'T
    | DbErr of error: DbError

[<RequireQualifiedAccess>]
module internal DbResult =
    /// 公開境界の `Result` へ戻す。
    let inline toResult (r: DbResult<'T>) =
        match r with
        | DbOk v -> Ok v
        | DbErr e -> Error e

    /// 公開境界の `Result` から内部表現へ変換する（成功値の確保は増えない）。
    let inline ofResult (r: Result<'T, DbError>) =
        match r with
        | Ok v -> DbOk v
        | Error e -> DbErr e

    let inline map (f: 'T -> 'U) (r: DbResult<'T>) =
        match r with
        | DbOk v -> DbOk(f v)
        | DbErr e -> DbErr e

/// Result<'T, DbError> の計算式ビルダ。
/// let! で失敗の伝播、return! で早期脱出ができる。
type internal DbResultBuilder() =
    member _.Return v = Ok v
    member _.ReturnFrom(v: Result<'a, DbError>) = v
    member _.Zero() = Ok()
    member _.Delay(f: unit -> Result<'a, DbError>) = f ()

    member _.Bind(m: Result<'a, DbError>, f: 'a -> Result<'b, DbError>) =
        match m with
        | Ok v -> f v
        | Error e -> Error e

    member _.BindReturn(m: Result<'a, DbError>, f: 'a -> 'b) =
        match m with
        | Ok v -> Ok(f v)
        | Error e -> Error e

    member _.Combine(a: Result<unit, DbError>, b: Result<'b, DbError>) =
        match a with
        | Ok _ -> b
        | Error e -> Error e

    member _.ReturnFromFinal(v: Result<'a, DbError>) = v

/// Result の組み合わせ演算子。
/// AutoOpen にして RykDb.Core 配下で常に使えるようにする。
/// これらは Result<'_, DbError> 専用で、制御フローに例外を使わない。
[<AutoOpen>]
module internal ResultOps =
    /// Result 計算式で使うビルダ。
    let result = DbResultBuilder()

    /// フラットな bind。
    let inline (>>=) (x: Result<'a, DbError>) (f: 'a -> Result<'b, DbError>) =
        match x with
        | Ok v -> f v
        | Error e -> Error e

    let inline ret (x: 'a) = Ok x

    let inline err (e: DbError) = Error e

    /// 共有の成功値。
    /// `Result` は参照 DU なので `Ok()` / `Ok true` / `Ok false` を書くとそのたびに確保する。
    /// 行ごと・値ごとに返す箇所ではこの定数を使う。
    let OkUnit: Result<unit, DbError> = Ok()
    let OkTrue: Result<bool, DbError> = Ok true
    let OkFalse: Result<bool, DbError> = Ok false

    /// Result の列を左から畳み込み、全て Ok なら値のリストを返す。
    /// 最初の Error で打ち切り、残りの項目は列挙しない（`foldOk` と同じ規則）。
    /// 複数の Error があっても返るのは最初の 1つ。
    /// `seq` は 1回だけ列挙するので、1回限りの列挙子でも渡せる。
    let all (rs: Result<'a, DbError> seq) =
        let rec gather (e: System.Collections.Generic.IEnumerator<Result<'a, DbError>>) (acc: 'a list) =
            if not (e.MoveNext()) then
                ret (List.rev acc)
            else
                match e.Current with
                | Error e -> Error e
                | Ok v -> gather e (v :: acc)

        use e = rs.GetEnumerator()
        gather e []

    /// 状態を受け取り Result を返す step で列を左から畳み込む Result 版 fold。
    /// 最初の Error で打ち切り、残りの項目は評価しない。
    /// 空の列は Ok state を返す。
    /// 副作用は step に任せる。
    let foldOk (step: 's -> 'a -> Result<'s, DbError>) (state: 's) =
        Seq.fold
            (fun acc item ->
                match acc with
                | Error e -> Error e
                | Ok s -> step s item)
            (ret state)

    /// .NET の I/O API は例外を送出するため、そこだけ Result へ包む境界。
    let protect (f: unit -> 'a) =
        try
            Ok(f ())
        with
        | :? System.IO.IOException as e -> Error(DbError.io e)
        | :? System.UnauthorizedAccessException as e -> Error(DbError.io e)
        | :? System.IO.InvalidDataException as e -> Error(DbError.corruption e.Message)
        | :? System.FormatException as e -> Error(DbError.corruption e.Message)
        | :? System.OverflowException as e -> Error(DbError.corruption e.Message)
        | e -> Error(DbError.internalError e.Message)
