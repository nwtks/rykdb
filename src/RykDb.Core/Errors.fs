namespace RykDb.Core

open System
open System.IO

/// 失敗の分類。エラー報告と回復可能性の判断に使う。
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

    /// 構文エラー。行番号を伴う。
    let syntax message line =
        {
            Kind = ErrorKind.SyntaxError
            Message = message
            Position = Some line
            Detail = None
        }

    let name message = create ErrorKind.NameError message
    let typeError message = create ErrorKind.TypeError message

    let constraintViolation message =
        create ErrorKind.ConstraintViolation message

    let notFound message = create ErrorKind.NotFound message
    let alreadyExists message = create ErrorKind.AlreadyExists message
    let capacity message = create ErrorKind.CapacityError message

    let corruption message =
        create ErrorKind.CorruptionError message

    let conflict message = create ErrorKind.ConflictError message
    let internalError message = create ErrorKind.InternalError message

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

    member _.BindFinal(m: Result<'a, DbError>, f: 'a -> Result<'b, DbError>) =
        match m with
        | Ok v -> f v
        | Error e -> Error e

    member _.ReturnFromFinal(v: Result<'a, DbError>) = v

    member _.CombineFinal(a: Result<unit, DbError>, b: Result<'b, DbError>) =
        match a with
        | Ok _ -> b
        | Error e -> Error e

    member _.DelayFinal(f: unit -> Result<'a, DbError>) = f ()

/// Result の組み合わせ演算子。
/// x |> f 形式だと読み取りにくい箇所で x >>= fun v -> ... と書けるOperatorsを用意する。
/// AutoOpen にして RykDb.Core 配下で常に使えるようにする。
/// これらは Result<'_, DbError> 専用で、制御フローに例外を使わない。
[<AutoOpen>]
module internal ResultOps =
    /// Result 計算式で使うビルダ。result { ... } と書ける。
    let result = DbResultBuilder()

    /// フラットな bind。x >>= fun v -> ... と書ける。
    let (>>=) (x: Result<'a, DbError>) (f: 'a -> Result<'b, DbError>) =
        match x with
        | Ok v -> f v
        | Error e -> Error e

    /// 最初の Ok の値を捨て、2 つ目を返す。x *> y。
    let ( *> ) (x: Result<'a, DbError>) (y: Result<'b, DbError>) =
        match x with
        | Ok _ -> y
        | Error e -> Error e

    let ret (x: 'a) = Ok x

    let err (e: DbError) = Error e

    let ofOption onNone (o: 'a option) =
        match o with
        | Some v -> Ok v
        | None -> Error onNone

    let tryGet (r: Result<'a, DbError>) =
        match r with
        | Ok v -> Some v
        | Error _ -> None

    let valueOr (fallback: 'a) (r: Result<'a, DbError>) =
        match r with
        | Ok v -> v
        | Error _ -> fallback

    /// Result の列を全て Ok になるまで畳み込む。1 つでも Error があればそれを返す。
    let all (rs: Result<'a, DbError> seq) =
        rs
        |> Seq.fold
            (fun acc r ->
                match acc, r with
                | Error e, _ -> Error e
                | _, Error e -> Error e
                | Ok acc, Ok v -> Ok(v :: acc))
            (ret [])
        |> Result.map List.rev

    /// .NET の I/O API は例外を送出するため、そこだけ Result へ包む境界。
    let protect (f: unit -> 'a) =
        try
            Ok(f ())
        with
        | :? IOException as e -> Error(DbError.io e)
        | :? UnauthorizedAccessException as e -> Error(DbError.io e)
        | :? InvalidDataException as e -> Error(DbError.corruption e.Message)
        | :? FormatException as e -> Error(DbError.corruption e.Message)
        | :? OverflowException as e -> Error(DbError.corruption e.Message)
        | e -> Error(DbError.internalError e.Message)
