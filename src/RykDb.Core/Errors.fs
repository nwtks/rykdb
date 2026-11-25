namespace RykDb.Core

/// SQL テキスト中の位置。
/// `Offset` は 0 始まり、`Line` / `Column` は 1 始まり。
/// エラー報告に使う。
[<Struct>]
type Position = { Offset: int; Line: int; Column: int }

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
    /// 存在しないテーブル・列・インデックスを参照した
    | NotFound
    /// 既に存在する名前で作成しようとした
    | AlreadyExists
    /// memtable・バッファ・結果件数の上限に達した
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
        Position: Position option
        Detail: string option
    }

[<RequireQualifiedAccess>]
module DbError =
    /// 位置も詳細も持たないエラーを作る。
    let create (kind: ErrorKind) (message: string) =
        {
            Kind = kind
            Message = message
            Position = None
            Detail = None
        }

    /// 構文エラー。
    /// 字句・構文解析器が持つ位置（行・列）を伴う。
    let syntax (message: string) (position: Position) =
        {
            Kind = ErrorKind.SyntaxError
            Message = message
            Position = Some position
            Detail = None
        }

    /// 名前解決の失敗。
    let inline name (message: string) = create ErrorKind.NameError message

    /// 型の不一致・不正な変換。
    let inline typeError (message: string) = create ErrorKind.TypeError message

    /// 制約違反。
    let inline constraintViolation (message: string) =
        create ErrorKind.ConstraintViolation message

    /// 対象が存在しない。
    let inline notFound (message: string) = create ErrorKind.NotFound message

    /// 同名の対象が既に存在する。
    let inline alreadyExists (message: string) = create ErrorKind.AlreadyExists message

    /// メモリ・件数などの上限超過。
    let inline capacity (message: string) = create ErrorKind.CapacityError message

    /// ファイル内容の破損。
    let inline corruption (message: string) =
        create ErrorKind.CorruptionError message

    /// write-write 競合などの同時実行衝突。
    let inline conflict (message: string) = create ErrorKind.ConflictError message

    /// 内部不整合（バグの兆候）。
    let inline internalError (message: string) = create ErrorKind.InternalError message

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
        | Some p, _ -> sprintf "%s (位置 %d行 %d列)" e.Message p.Line p.Column
        | None, Some d -> sprintf "%s [%s]" e.Message d
        | None, None -> e.Message

/// 成功値の確保を避ける内部専用 Result。
/// <remarks>
/// 参照 DU の `Result` は成功パスでも `Ok` を確保するため、行ごとのホットパスではこちらを使い、
/// 公開境界（`Session` / `Op.toArrayLimited`）で `Result` に戻す。
/// struct DU なのでケースにフィールド名が必要（FS3585）。
/// `Unchecked.defaultof` で作らず、コレクションの要素にもしない（既定値が `DbOk`、セルも大きくなる）。
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

    /// 成功値だけを変換する。
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

    /// 成功値を `Result` に包む。
    let inline ret (x: 'a) = Ok x

    /// エラーを `Result` に包む。
    let inline err (e: DbError) = Error e

    /// 共有の `Ok()`。
    /// 参照 DU の `Result` は成功値でも確保するため、行ごとに返す箇所ではこの定数を使う。
    let OkUnit: Result<unit, DbError> = Ok()

    /// 共有の `Ok true`。
    let OkTrue: Result<bool, DbError> = Ok true

    /// 共有の `Ok false`。
    let OkFalse: Result<bool, DbError> = Ok false

    /// Result の列を先頭から畳み、最初の Error で打ち切って値のリストを返す。
    /// `seq` は 1回だけ列挙するので、1回限りの列挙子でも渡せる。
    [<TailCall>]
    let rec private gather (acc: 'a list) (e: System.Collections.Generic.IEnumerator<Result<'a, DbError>>) =
        if not (e.MoveNext()) then
            ret (List.rev acc)
        else
            match e.Current with
            | Error e -> Error e
            | Ok v -> gather (v :: acc) e

    /// Result の列を値のリストにする。
    /// 最初の Error で打ち切る。
    let all (rs: Result<'a, DbError> seq) =
        use e = rs.GetEnumerator()
        gather [] e

    /// 状態付きの畳み込み。最初の Error で打ち切る。
    /// 空の列は `Ok state` を返す。
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
