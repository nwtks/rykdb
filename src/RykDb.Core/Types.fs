namespace RykDb.Core

/// バイト列のユーティリティ。
module internal Bytes =
    /// 昇順比較（前方が短いほうが小さい）。
    let compare (a: byte array) (b: byte array) =
        let n = min a.Length b.Length

        let rec compareAt i =
            if i >= n then compare a.Length b.Length
            elif a.[i] = b.[i] then compareAt (i + 1)
            else compare (int a.[i]) (int b.[i])

        compareAt 0

    /// 生のバイト列の固定オフセットから、リトルエンディアン 32bit を読む。
    let readLE32 (data: byte array) (offset: int) =
        let at (i: int) = uint32 data.[offset + i]
        at 0 ||| (at 1 <<< 8) ||| (at 2 <<< 16) ||| (at 3 <<< 24)

    /// readLE32 の 64bit 版。
    let readLE64 (data: byte array) (offset: int) =
        int64 (readLE32 data offset) ||| (int64 (readLE32 data (offset + 4)) <<< 32)

/// 実行時の値。
/// NULL は他のすべての型と区別できる特別なケースとして持つ。
[<RequireQualifiedAccess>]
type SqlValue =
    | Null
    | Bool of bool
    | Int of int64
    | Real of float
    | Text of string
    | Blob of byte[]
    | Date of System.DateOnly
    | Timestamp of System.DateTimeOffset
    | Decimal of decimal
    | Uuid of System.Guid

[<RequireQualifiedAccess>]
module SqlLiteral =
    /// SQL リテラルとしての表示。
    let toSqlLiteral (v: SqlValue) =
        let formatReal (f: double) =
            if System.Double.IsNaN f then
                "NaN"
            elif System.Double.IsPositiveInfinity f then
                "Infinity"
            elif System.Double.IsNegativeInfinity f then
                "-Infinity"
            else
                f.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

        let formatBlob (b: byte[]) =
            let sb = System.Text.StringBuilder "X'"

            for x in b do
                sb.Append(x.ToString("X2", System.Globalization.CultureInfo.InvariantCulture))
                |> ignore

            sb.Append('\'').ToString()

        match v with
        | SqlValue.Null -> "NULL"
        | SqlValue.Bool b -> if b then "TRUE" else "FALSE"
        | SqlValue.Int i -> i.ToString System.Globalization.CultureInfo.InvariantCulture
        | SqlValue.Real f -> formatReal f
        | SqlValue.Text s -> "'" + s.Replace("'", "''") + "'"
        | SqlValue.Blob b -> formatBlob b
        | SqlValue.Date d -> d.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture)
        | SqlValue.Timestamp t ->
            t.UtcDateTime.ToString("yyyy-MM-ddTHH:mm:ss.fffffffZ", System.Globalization.CultureInfo.InvariantCulture)
        | SqlValue.Decimal d -> d.ToString System.Globalization.CultureInfo.InvariantCulture
        | SqlValue.Uuid g -> g.ToString("D", System.Globalization.CultureInfo.InvariantCulture)

/// サポートする SQL の型。
[<RequireQualifiedAccess>]
type internal DataType =
    | Null
    | Boolean
    | Integer
    | Real
    | Text
    | Blob
    | Date
    | Timestamp
    | Decimal
    | Uuid

/// SQL の 3値論理。
/// 比較結果は TRUE / FALSE だけでなく UNKNOWN を持つ。
[<RequireQualifiedAccess>]
type internal TruthValue =
    | True
    | False
    | Unknown

module internal TruthValue =
    /// SQL の 3値論理の AND。
    /// FALSE が一つでもあれば FALSE、それ以外は UNKNOWN を返す。
    let and2 a b =
        if a = TruthValue.False || b = TruthValue.False then
            TruthValue.False
        elif a = TruthValue.Unknown || b = TruthValue.Unknown then
            TruthValue.Unknown
        else
            TruthValue.True

    /// SQL の 3値論理の OR。
    /// TRUE が一つでもあれば TRUE。
    let or2 a b =
        if a = TruthValue.True || b = TruthValue.True then
            TruthValue.True
        elif a = TruthValue.Unknown || b = TruthValue.Unknown then
            TruthValue.Unknown
        else
            TruthValue.False

module internal DataType =
    /// SQL の照合順序における型の順位。
    /// 異なる型を並べ替えるときの決定的なタイブレークに使う。
    /// 数値 3種は互いに数値比較されるので、ここでは型タグの符号で決めてよい。
    let rank t =
        match t with
        | DataType.Null -> 0
        | DataType.Boolean -> 1
        | DataType.Integer -> 2
        | DataType.Real -> 3
        | DataType.Decimal -> 4
        | DataType.Date -> 5
        | DataType.Timestamp -> 6
        | DataType.Text -> 7
        | DataType.Blob -> 8
        | DataType.Uuid -> 9

    /// 型を 1バイトのタグにする（カタログの永続化に使う）。
    /// rank の順に単調増加させてあるので、復元不能でも比較はできる。
    let inline toTag (t: DataType) = byte (rank t)

    /// タグから型を復元する。
    /// 範囲外なら InternalError。
    let fromTag (b: byte) =
        match b with
        | 0uy -> ret DataType.Null
        | 1uy -> ret DataType.Boolean
        | 2uy -> ret DataType.Integer
        | 3uy -> ret DataType.Real
        | 4uy -> ret DataType.Decimal
        | 5uy -> ret DataType.Date
        | 6uy -> ret DataType.Timestamp
        | 7uy -> ret DataType.Text
        | 8uy -> ret DataType.Blob
        | 9uy -> ret DataType.Uuid
        | other -> err (sprintf "カタログの型タグ %d は未知です" (int other) |> DbError.corruption)

    /// SQL の型名（DDL の読み書きに使う正準表記）。
    let toSqlName t =
        match t with
        | DataType.Null -> "NULL"
        | DataType.Boolean -> "BOOLEAN"
        | DataType.Integer -> "INTEGER"
        | DataType.Real -> "REAL"
        | DataType.Text -> "TEXT"
        | DataType.Blob -> "BLOB"
        | DataType.Date -> "DATE"
        | DataType.Timestamp -> "TIMESTAMP"
        | DataType.Decimal -> "DECIMAL"
        | DataType.Uuid -> "UUID"

module internal SqlValue =
    let dataType v =
        match v with
        | SqlValue.Null -> DataType.Null
        | SqlValue.Bool _ -> DataType.Boolean
        | SqlValue.Int _ -> DataType.Integer
        | SqlValue.Real _ -> DataType.Real
        | SqlValue.Text _ -> DataType.Text
        | SqlValue.Blob _ -> DataType.Blob
        | SqlValue.Date _ -> DataType.Date
        | SqlValue.Timestamp _ -> DataType.Timestamp
        | SqlValue.Decimal _ -> DataType.Decimal
        | SqlValue.Uuid _ -> DataType.Uuid

    let inline isNull (v: SqlValue) =
        match v with
        | SqlValue.Null -> true
        | _ -> false

    /// 数値として取り出す。
    /// 非数値なら None。
    /// NaOf と ±Infinity は decimal に表現できないので None を返す
    /// （System.Decimal の変換は例外を送出するため）。
    let asNumeric v =
        match v with
        | SqlValue.Int i -> Some(decimal i)
        | SqlValue.Real f when System.Double.IsNaN f || System.Double.IsInfinity f -> None
        | SqlValue.Real f -> Some(decimal f)
        | SqlValue.Decimal d -> Some d
        | _ -> None

    /// decimal に変換できる値か。
    /// NaN と ±Infinity は false。
    let private isDecimalRepresentable v =
        match v with
        | SqlValue.Real f -> not (System.Double.IsNaN f || System.Double.IsInfinity f)
        | _ -> true

    let inline asInt64 v =
        match v with
        | SqlValue.Int i -> Some i
        | _ -> None

    let inline asText v =
        match v with
        | SqlValue.Text s -> Some s
        | _ -> None

    /// 正の quiet NaN のビットパターン。
    /// .NET の Double.NaN は符号ビットが立った負の NaN なので、
    /// そのままキーにすると「負の NaN」とみなされ、順序保持が壊れる。
    [<Literal>]
    let CanonicalNaNBits = 0x7FF8000000000000L

    let canonicalNaN = System.BitConverter.Int64BitsToDouble CanonicalNaNBits

    /// NaN を単一のビットパターンに正規化する。
    /// キー符号化の順序を一意にし、「NaN は他のすべての数値より大きい」という
    /// SQL 比較の規則と矛盾しないようにする。
    let inline normalizeFloat f =
        if System.Double.IsNaN f then canonicalNaN else f

    let inline vReal f = normalizeFloat f |> SqlValue.Real

    /// 値どうしの比較。
    /// NULL は比較不能として例外を投げる。
    /// NaN は NaN と等しく、かつ他のすべての数値より大きいものとして扱う。
    let compareValues (a: SqlValue) (b: SqlValue) =
        let compareNumeric (a: SqlValue) (b: SqlValue) =
            match a, b with
            | SqlValue.Real fa, SqlValue.Real fb ->
                if System.Double.IsNaN fa && System.Double.IsNaN fb then
                    Ok 0
                elif System.Double.IsNaN fa then
                    Ok 1
                elif System.Double.IsNaN fb then
                    Ok -1
                else
                    Ok(compare fa fb)
            | _ ->
                match asNumeric a, asNumeric b with
                | Some da, Some db -> Ok(compare da db)
                | Some _, None -> err (DbError.typeError "NaN や ±Infinity は数値比較できません")
                | None, Some _ -> err (DbError.typeError "NaN や ±Infinity は数値比較できません")
                | None, None -> err (DbError.internalError "数値比較の分岐に到達しませんでした（内部不整合）")

        match a, b with
        | SqlValue.Null, _
        | _, SqlValue.Null -> err (DbError.typeError "NULL は比較できません")
        | (SqlValue.Int _ | SqlValue.Real _ | SqlValue.Decimal _),
          (SqlValue.Int _ | SqlValue.Real _ | SqlValue.Decimal _) -> compareNumeric a b
        | SqlValue.Bool x, SqlValue.Bool y -> Ok(compare (if x then 1 else 0) (if y then 1 else 0))
        | SqlValue.Text x, SqlValue.Text y -> Ok(System.StringComparer.Ordinal.Compare(x, y))
        | SqlValue.Blob x, SqlValue.Blob y -> Ok(Bytes.compare x y)
        | SqlValue.Uuid x, SqlValue.Uuid y -> Ok(compare x y)
        | SqlValue.Date x, SqlValue.Date y -> Ok(compare x y)
        | SqlValue.Timestamp x, SqlValue.Timestamp y -> Ok(System.DateTimeOffset.Compare(x, y))
        | SqlValue.Date x, SqlValue.Timestamp y ->
            let xs =
                System.DateTimeOffset(x.ToDateTime System.TimeOnly.MinValue, System.TimeSpan.Zero)

            Ok(System.DateTimeOffset.Compare(xs, y))
        | SqlValue.Timestamp x, SqlValue.Date y ->
            let ys =
                System.DateTimeOffset(y.ToDateTime System.TimeOnly.MinValue, System.TimeSpan.Zero)

            Ok(System.DateTimeOffset.Compare(x, ys))
        | _ ->
            err (
                sprintf "型 '%s' と '%s' は比較できません" (dataType a |> DataType.toSqlName) (dataType b |> DataType.toSqlName)
                |> DbError.typeError
            )

    /// 関数・制約メッセージ用の簡潔な表示（SQL リテラルではなく）。
    let toDisplayString (v: SqlValue) =
        match v with
        | SqlValue.Null -> "NULL"
        | SqlValue.Bool b -> if b then "true" else "false"
        | SqlValue.Int i -> i.ToString System.Globalization.CultureInfo.InvariantCulture
        | SqlValue.Real f -> f.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
        | SqlValue.Text s -> s
        | SqlValue.Blob b -> sprintf "<blob %d bytes>" b.Length
        | SqlValue.Date d -> d.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture)
        | SqlValue.Timestamp t ->
            t.UtcDateTime.ToString("yyyy-MM-ddTHH:mm:ss.fffZ", System.Globalization.CultureInfo.InvariantCulture)
        | SqlValue.Decimal d -> d.ToString System.Globalization.CultureInfo.InvariantCulture
        | SqlValue.Uuid g -> g.ToString("D", System.Globalization.CultureInfo.InvariantCulture)

    /// 暗黙変換。
    /// SQL の通常の型強制の規則を表す。
    /// 変換できない場合は型エラーを返す。
    let coerce (target: DataType) (v: SqlValue) =
        let fail () =
            err (
                sprintf "'%s' は '%s' に変換できません" (SqlLiteral.toSqlLiteral v) (DataType.toSqlName target)
                |> DbError.typeError
            )

        let asBoolean (v: SqlValue) =
            match v with
            | SqlValue.Bool _ -> Ok v
            | SqlValue.Int i when i = 0L -> Ok(SqlValue.Bool false)
            | SqlValue.Int i when i = 1L -> Ok(SqlValue.Bool true)
            | SqlValue.Real f when f = 0.0 -> Ok(SqlValue.Bool false)
            | SqlValue.Real f when f = 1.0 -> Ok(SqlValue.Bool true)
            | _ -> fail ()

        let asInteger (v: SqlValue) =
            match v with
            | SqlValue.Int _ -> Ok v
            | SqlValue.Bool b -> Ok(SqlValue.Int(if b then 1L else 0L))
            | SqlValue.Real f when
                not (System.Double.IsNaN f || System.Double.IsInfinity f)
                && System.Math.Truncate f = f
                ->
                Ok(SqlValue.Int(int64 f))
            | SqlValue.Decimal d when System.Decimal.Truncate d = d -> Ok(SqlValue.Int(int64 d))
            | _ -> fail ()

        let asReal (v: SqlValue) =
            match v with
            | SqlValue.Real _ -> Ok v
            | SqlValue.Int i -> Ok(SqlValue.Real(float i))
            | SqlValue.Bool b -> Ok(SqlValue.Real(if b then 1.0 else 0.0))
            | SqlValue.Decimal d -> Ok(SqlValue.Real(float d))
            | _ -> fail ()

        let asDecimal (v: SqlValue) =
            if not (isDecimalRepresentable v) then
                fail ()
            else
                match v with
                | SqlValue.Decimal _ -> Ok v
                | SqlValue.Int i -> Ok(SqlValue.Decimal(decimal i))
                | SqlValue.Real f -> Ok(SqlValue.Decimal(decimal f))
                | SqlValue.Bool b -> Ok(SqlValue.Decimal(if b then 1.0M else 0.0M))
                | _ -> fail ()

        let asDate (v: SqlValue) =
            match v with
            | SqlValue.Date _ -> Ok v
            | SqlValue.Timestamp t -> Ok(SqlValue.Date(System.DateOnly.FromDateTime t.UtcDateTime))
            | SqlValue.Text s ->
                match
                    System.DateOnly.TryParseExact(
                        s,
                        "yyyy-MM-dd",
                        System.Globalization.CultureInfo.InvariantCulture,
                        System.Globalization.DateTimeStyles.None
                    )
                with
                | true, d -> Ok(SqlValue.Date d)
                | _ -> fail ()
            | _ -> fail ()

        let asTimestamp (v: SqlValue) =
            match v with
            | SqlValue.Timestamp _ -> Ok v
            | SqlValue.Date d ->
                Ok(
                    SqlValue.Timestamp(
                        System.DateTimeOffset(d.ToDateTime System.TimeOnly.MinValue, System.TimeSpan.Zero)
                    )
                )
            | SqlValue.Text s ->
                match
                    System.DateTimeOffset.TryParse(
                        s,
                        System.Globalization.CultureInfo.InvariantCulture,
                        System.Globalization.DateTimeStyles.AssumeUniversal
                        ||| System.Globalization.DateTimeStyles.AdjustToUniversal
                    )
                with
                | true, t -> Ok(SqlValue.Timestamp t)
                | _ -> fail ()
            | _ -> fail ()

        if isNull v then
            Ok SqlValue.Null
        else
            match target with
            | DataType.Null -> Ok SqlValue.Null
            | DataType.Boolean -> asBoolean v
            | DataType.Integer -> asInteger v
            | DataType.Real -> asReal v
            | DataType.Decimal -> asDecimal v
            | DataType.Text ->
                match v with
                | SqlValue.Text _ -> Ok v
                | _ -> Ok(SqlValue.Text(SqlLiteral.toSqlLiteral v))
            | DataType.Blob ->
                match v with
                | SqlValue.Blob _ -> Ok v
                | SqlValue.Text s -> Ok(SqlValue.Blob(System.Text.Encoding.UTF8.GetBytes s))
                | _ -> fail ()
            | DataType.Date -> asDate v
            | DataType.Timestamp -> asTimestamp v
            | DataType.Uuid ->
                match v with
                | SqlValue.Uuid _ -> Ok v
                | SqlValue.Text s ->
                    match System.Guid.TryParse s with
                    | true, g -> Ok(SqlValue.Uuid g)
                    | _ -> fail ()
                | _ -> fail ()
