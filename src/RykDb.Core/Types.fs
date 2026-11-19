namespace RykDb.Core

/// 意味の違う数値を取り違えないための単位。
/// 素の `int64` / `uint64` 同士は取り違えてもコンパイルが通る。
/// そこで**同型で紛れやすい組の片側だけ**に単位を付ける:
/// - `manifestNo` — MANIFEST 番号（`FileNumber` / `NextFileNumber` は素の `int64` のまま）
/// - `txnId` — transaction 番号（`NextSeqNo` は素の `uint64` のまま）
/// 片側だけでも対が区別できる（`int64<manifestNo>` と `int64` は代入できない）。
[<Measure>]
type manifestNo

[<Measure>]
type txnId

/// transaction の識別子。
/// 単位 `txnId` を持つ。
type internal TxnId = uint64<txnId>

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
    /// 列が必要ないスキャンで復号を省略した値。
    /// エンジン内部の列マスク復号だけで使うプレースホルダであり、
    /// 結果の境界では `LoadedRow` が混入を拒否する（公開 API へは出さない）。
    | Unloaded

[<RequireQualifiedAccess>]
module SqlLiteral =
    /// SQL リテラルとしての表示。
    let toSqlLiteral =
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

        function
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
        | SqlValue.Unloaded -> "<unloaded>"

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
        let at i = uint32 data.[offset + i]
        at 0 ||| (at 1 <<< 8) ||| (at 2 <<< 16) ||| (at 3 <<< 24)

/// エントリの並び順（キー昇順・同一キーは SeqNo 降順）の**唯一の定義**。
/// memtable の `Entry` と SSTable の `SstEntry` は必ずこれを経由する
/// （2実装になると片方だけ壊れる。`SerializationTests` の `順序保持:` が検査する）。
module internal KeyOrder =
    let compare (keyA: byte array) (seqA: uint64) (keyB: byte array) (seqB: uint64) =
        match Bytes.compare keyA keyB with
        | 0 -> compare seqB seqA
        | c -> c

/// SQL の識別子比較。
/// テーブル名・列名・索引名・別名は大文字小文字を区別しない。
/// スキーマ・カタログ・実行層で同じ規則を使うため、ここに 1つだけ置く。
module internal SqlName =
    let equal (a: string) (b: string) =
        System.String.Equals(a, b, System.StringComparison.OrdinalIgnoreCase)

    /// 大文字小文字を無視して重複する名前を探す。
    /// 重複があれば最初に見つかった名前を返す。
    let duplicate (names: string list) =
        let rec loop (seen: string list) (rest: string list) =
            match rest with
            | [] -> None
            | name :: tail ->
                if seen |> List.exists (fun s -> equal s name) then
                    Some name
                else
                    loop (name :: seen) tail

        loop [] names

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
    let and2 (a: TruthValue) (b: TruthValue) =
        if a = TruthValue.False || b = TruthValue.False then
            TruthValue.False
        elif a = TruthValue.Unknown || b = TruthValue.Unknown then
            TruthValue.Unknown
        else
            TruthValue.True

    /// SQL の 3値論理の OR。
    /// TRUE が一つでもあれば TRUE。
    let or2 (a: TruthValue) (b: TruthValue) =
        if a = TruthValue.True || b = TruthValue.True then
            TruthValue.True
        elif a = TruthValue.Unknown || b = TruthValue.Unknown then
            TruthValue.Unknown
        else
            TruthValue.False

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

module internal DataType =
    /// SQL の照合順序における型の順位。
    /// 異なる型を並べ替えるときの決定的なタイブレークに使う。
    /// 数値 3種は互いに数値比較されるので、ここでは型タグの符号で決めてよい。
    let internal rank =
        function
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
    let inline toTag (t: DataType) = rank t |> byte

    /// タグから型を復元する。
    /// 範囲外なら InternalError。
    let fromTag =
        function
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
    let toSqlName =
        function
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
    /// 共有済みの Bool 値。
    /// 比較・IS NULL・3値論理の結果は true / false の 2種類しかないので、
    /// 行ごとの評価で新しい DU を作らない。
    let TrueValue = SqlValue.Bool true
    let FalseValue = SqlValue.Bool false

    /// bool を共有済みの SqlValue.Bool へ変換する。
    let inline ofBool b = if b then TrueValue else FalseValue

    /// 値の型。`Unloaded`（列マスクのプレースホルダ）は型を持たないためエラー。
    let dataType =
        function
        | SqlValue.Null -> ret DataType.Null
        | SqlValue.Bool _ -> ret DataType.Boolean
        | SqlValue.Int _ -> ret DataType.Integer
        | SqlValue.Real _ -> ret DataType.Real
        | SqlValue.Text _ -> ret DataType.Text
        | SqlValue.Blob _ -> ret DataType.Blob
        | SqlValue.Date _ -> ret DataType.Date
        | SqlValue.Timestamp _ -> ret DataType.Timestamp
        | SqlValue.Decimal _ -> ret DataType.Decimal
        | SqlValue.Uuid _ -> ret DataType.Uuid
        | SqlValue.Unloaded -> err (DbError.internalError "復号していない列の型を参照しました")

    /// 比較エラーの表示用の型名。
    /// `Unloaded` は比較前に弾かれるが、表示側でも例外にしない。
    let private typeNameOf (v: SqlValue) =
        match dataType v with
        | Ok t -> DataType.toSqlName t
        | Error _ -> "<unloaded>"

    let isNull =
        function
        | SqlValue.Null -> true
        | _ -> false

    /// 行を結果として返すときに DB 内部の配列と切り離す。
    /// `StatementResult.Rows` や `RowReader` はこれを経由する。
    /// 配列をそのまま返すと、呼び出し側の書き換えが memtable / SSTable の内容を壊す。
    /// Blob の byte[] も複製する。
    let copyRow =
        Array.map (function
            | SqlValue.Blob b -> SqlValue.Blob(Array.copy b)
            | other -> other)

    /// 行の不要列を `Unloaded` に置き換える（復号済みの行にマスクを適用する）。
    let maskRow (mask: bool array) (row: SqlValue array) =
        if mask.Length = 0 then
            [||]
        elif row.Length = 0 then
            Array.create mask.Length SqlValue.Unloaded
        else
            Array.mapi
                (fun i v ->
                    if i < mask.Length && not mask.[i] then
                        SqlValue.Unloaded
                    else
                        v)
                row

    /// 列マスクのプレースホルダが結果に漏れていないか検査する。
    /// 結果の境界では `LoadedRow.create` がこれを呼ぶ（直接呼ぶのはそこだけ）。
    let ensureLoaded (row: SqlValue array) =
        let rec firstUnloaded i =
            if i >= row.Length then
                None
            else
                match row.[i] with
                | SqlValue.Unloaded -> Some i
                | _ -> firstUnloaded (i + 1)

        match firstUnloaded 0 with
        | None -> ret row
        | Some i -> err (DbError.internalError (sprintf "復号していない列がクエリ結果に含まれています（列位置: %d）" i))

    /// REAL を decimal へ変換できる絶対値の上限。
    /// System.Decimal の最大値（約 7.9228e28）より少し小さい安全側の値。
    [<Literal>]
    let private DecimalMaxAsFloat = 7.9e28

    /// REAL を decimal で比較できる絶対値の下限（decimal の最小正値）。
    /// これ未満の極小値は decimal では 0 に潰れるため float で比較する。
    [<Literal>]
    let private DecimalMinMagnitudeAsFloat = 1e-28

    /// REAL が decimal へ安全に変換できる範囲かを返す。
    /// 変換自体が例外を送出し得るため、境界判定をここへ集約する。
    let private fitsDecimal (f: float) =
        not (System.Double.IsNaN f || System.Double.IsInfinity f)
        && System.Math.Abs f <= DecimalMaxAsFloat

    /// 数値として取り出す。
    /// 非数値なら None。
    /// NaN と ±Infinity、および decimal の範囲外の REAL は None を返す
    /// （System.Decimal の変換は例外を送出するため）。
    let asNumeric =
        function
        | SqlValue.Int i -> Some(decimal i)
        | SqlValue.Real f when fitsDecimal f -> Some(decimal f)
        | SqlValue.Real _ -> None
        | SqlValue.Decimal d -> Some d
        | _ -> None

    let asText =
        function
        | SqlValue.Text s -> Some s
        | _ -> None

    /// 正の quiet NaN のビットパターン。
    /// .NET の Double.NaN は符号ビットが立った負の NaN なので、
    /// そのままキーにすると「負の NaN」とみなされ、順序保持が壊れる。
    [<Literal>]
    let private CanonicalNaNBits = 0x7FF8000000000000L

    let canonicalNaN = System.BitConverter.Int64BitsToDouble CanonicalNaNBits

    /// 浮動小数点数を単一のビットパターンに正規化する。
    /// - NaN は単一のビットパターンに揃える（キー符号化の順序を一意にする）。
    /// - -0.0 は +0.0 に揃える（compareValues が両者を等しいと扱うため、
    ///   キーや DISTINCT の符号化でも同じ値にする）。
    let normalizeFloat (f: float) =
        if System.Double.IsNaN f then canonicalNaN
        elif f = 0.0 then 0.0
        else f

    let vReal = normalizeFloat >> SqlValue.Real

    /// TEXT を UTF-8 の符号化順（Unicode コードポイント順）で比較する。
    /// .NET の StringComparer.Ordinal は UTF-16 コード単位順であり、
    /// 補助文字（サロゲートペア）と U+E000..U+FFFF の間で UTF-8 の順序と逆転する。
    /// キーの符号化は UTF-8 バイト列なので、SQL の比較も同じ順序に合わせる。
    /// 不正なサロゲートは Encoding.UTF8 と同じく U+FFFD として扱う。
    let private compareText (x: string) (y: string) =
        let codePointAt (s: string) i =
            let c = s.[i]

            if
                System.Char.IsHighSurrogate c
                && i + 1 < s.Length
                && System.Char.IsLowSurrogate s.[i + 1]
            then
                System.Char.ConvertToUtf32(c, s.[i + 1]), i + 2
            elif System.Char.IsSurrogate c then
                0xFFFD, i + 1
            else
                int c, i + 1

        let rec loop i j =
            if i >= x.Length then
                if j >= y.Length then 0 else -1
            elif j >= y.Length then
                1
            else
                let cx = x.[i]
                let cy = y.[j]

                if cx = cy && not (System.Char.IsSurrogate cx) then
                    loop (i + 1) (j + 1)
                else
                    let px, nextX = codePointAt x i
                    let py, nextY = codePointAt y j
                    let c = compare px py
                    if c <> 0 then c else loop nextX nextY

        loop 0 0

    let private decimalOfValue =
        function
        | SqlValue.Int i -> decimal i
        | SqlValue.Decimal d -> d
        | _ -> 0M

    let private floatOfValue =
        function
        | SqlValue.Int i -> float i
        | SqlValue.Decimal d -> float d
        | SqlValue.Real f -> f
        | _ -> 0.0

    let private compareFloat (x: float) (y: float) =
        if x < y then -1
        elif x > y then 1
        else 0

    let private compareRealValue (f: float) (other: SqlValue) =
        if System.Double.IsNaN f || System.Double.IsInfinity f then
            DbErr(DbError.typeError "NaN や ±Infinity は数値比較できません")
        elif fitsDecimal f && (f = 0.0 || System.Math.Abs f >= DecimalMinMagnitudeAsFloat) then
            DbOk(System.Decimal.Compare(decimal f, decimalOfValue other))
        else
            DbOk(compareFloat f (floatOfValue other))

    let private negateInt (c: int) = -c

    let private compareNumericValue (a: SqlValue) (b: SqlValue) =
        match a, b with
        | SqlValue.Real fa, SqlValue.Real fb ->
            if System.Double.IsNaN fa && System.Double.IsNaN fb then
                DbOk 0
            elif System.Double.IsNaN fa then
                DbOk 1
            elif System.Double.IsNaN fb then
                DbOk -1
            else
                DbOk(compareFloat fa fb)
        | SqlValue.Real fa, _ -> compareRealValue fa b
        | _, SqlValue.Real fb -> compareRealValue fb a |> DbResult.map negateInt
        | _ -> DbOk(System.Decimal.Compare(decimalOfValue a, decimalOfValue b))

    /// 値どうしの比較（内部ホットパス用）。
    /// 成功値を struct で返すので、行ごとの比較で `Ok` を確保しない。
    /// ローカル関数は毎回クロージャを確保するため、ヘルパーはモジュール直下に置く。
    /// NULL は比較不能として `DbError.typeError` を返す。
    /// NaN は NaN と等しく、Real 同士では他のすべての値より大きいものとして扱う
    /// （Int / Decimal との比較は型エラー）。
    let compareValuesV (a: SqlValue) (b: SqlValue) =
        match a, b with
        | SqlValue.Unloaded, _
        | _, SqlValue.Unloaded -> DbErr(DbError.internalError "復号していない列を比較しようとしました")
        | SqlValue.Null, _
        | _, SqlValue.Null -> DbErr(DbError.typeError "NULL は比較できません")
        | (SqlValue.Int _ | SqlValue.Real _ | SqlValue.Decimal _),
          (SqlValue.Int _ | SqlValue.Real _ | SqlValue.Decimal _) -> compareNumericValue a b
        | SqlValue.Bool x, SqlValue.Bool y ->
            DbOk(
                if x = y then 0
                elif x then 1
                else -1
            )
        | SqlValue.Text x, SqlValue.Text y -> DbOk(compareText x y)
        | SqlValue.Blob x, SqlValue.Blob y -> DbOk(Bytes.compare x y)
        | SqlValue.Uuid x, SqlValue.Uuid y -> DbOk(x.CompareTo y)
        | SqlValue.Date x, SqlValue.Date y -> DbOk(x.CompareTo y)
        | SqlValue.Timestamp x, SqlValue.Timestamp y -> DbOk(System.DateTimeOffset.Compare(x, y))
        | SqlValue.Date x, SqlValue.Timestamp y ->
            let xs =
                System.DateTimeOffset(x.ToDateTime System.TimeOnly.MinValue, System.TimeSpan.Zero)

            DbOk(System.DateTimeOffset.Compare(xs, y))
        | SqlValue.Timestamp x, SqlValue.Date y ->
            let ys =
                System.DateTimeOffset(y.ToDateTime System.TimeOnly.MinValue, System.TimeSpan.Zero)

            DbOk(System.DateTimeOffset.Compare(x, ys))
        | _ ->
            sprintf "型 '%s' と '%s' は比較できません" (typeNameOf a) (typeNameOf b)
            |> DbError.typeError
            |> DbErr

    /// ORDER BY 用の全順序比較（内部ホットパス用）。
    /// NULL は他のすべての値より小さいものとして扱う（ASC の先頭、DESC の末尾）。
    /// SQL の 3値論理では NULL は比較不能だが、ソートには全順序が必要なのでここだけ規則を分ける。
    /// NULL 以外の型が比較不能な場合は compareValues と同じエラーを返す。
    let compareValuesForSortV (a: SqlValue) (b: SqlValue) =
        match a, b with
        | SqlValue.Null, SqlValue.Null -> DbOk 0
        | SqlValue.Null, _ -> DbOk -1
        | _, SqlValue.Null -> DbOk 1
        | _ -> compareValuesV a b

    /// 関数・制約メッセージ用の簡潔な表示（SQL リテラルではなく）。
    let toDisplayString =
        function
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
        | SqlValue.Unloaded -> "<unloaded>"

    [<Literal>]
    let private Int64MinAsFloat = -9223372036854775808.0

    [<Literal>]
    let private Int64MaxAsFloat = 9223372036854775808.0

    /// 暗黙変換。
    /// SQL の通常の型強制の規則を表す。
    /// 変換できない場合は型エラーを返す。
    let coerce (target: DataType) (v: SqlValue) =
        let fail () =
            sprintf "'%s' は '%s' に変換できません" (SqlLiteral.toSqlLiteral v) (DataType.toSqlName target)
            |> DbError.typeError
            |> err

        let isDecimalRepresentable =
            function
            | SqlValue.Real f -> fitsDecimal f
            | _ -> true

        let asBoolean =
            function
            | SqlValue.Bool _ as v -> ret v
            | SqlValue.Int i when i = 0L -> ret FalseValue
            | SqlValue.Int i when i = 1L -> ret TrueValue
            | SqlValue.Real f when f = 0.0 -> ret FalseValue
            | SqlValue.Real f when f = 1.0 -> ret TrueValue
            | _ -> fail ()

        let asInteger =
            function
            | SqlValue.Int _ as v -> ret v
            | SqlValue.Bool b -> (if b then 1L else 0L) |> SqlValue.Int |> ret
            | SqlValue.Real f when
                not (System.Double.IsNaN f || System.Double.IsInfinity f)
                && System.Math.Truncate f = f
                && f >= Int64MinAsFloat
                && f < Int64MaxAsFloat
                ->
                int64 f |> SqlValue.Int |> ret
            | SqlValue.Decimal d when
                System.Decimal.Truncate d = d
                && d >= decimal System.Int64.MinValue
                && d <= decimal System.Int64.MaxValue
                ->
                int64 d |> SqlValue.Int |> ret
            | _ -> fail ()

        let asReal =
            function
            | SqlValue.Real f -> ret (vReal f)
            | SqlValue.Int i -> float i |> vReal |> ret
            | SqlValue.Bool b -> (if b then 1.0 else 0.0) |> vReal |> ret
            | SqlValue.Decimal d -> float d |> vReal |> ret
            | _ -> fail ()

        let asDecimal v =
            if not (isDecimalRepresentable v) then
                fail ()
            else
                match v with
                | SqlValue.Decimal _ -> ret v
                | SqlValue.Int i -> decimal i |> SqlValue.Decimal |> ret
                | SqlValue.Real f -> decimal f |> SqlValue.Decimal |> ret
                | SqlValue.Bool b -> (if b then 1.0M else 0.0M) |> SqlValue.Decimal |> ret
                | _ -> fail ()

        let asDate =
            function
            | SqlValue.Date _ as v -> ret v
            | SqlValue.Timestamp t -> System.DateOnly.FromDateTime t.UtcDateTime |> SqlValue.Date |> ret
            | SqlValue.Text s ->
                match
                    System.DateOnly.TryParseExact(
                        s,
                        "yyyy-MM-dd",
                        System.Globalization.CultureInfo.InvariantCulture,
                        System.Globalization.DateTimeStyles.None
                    )
                with
                | true, d -> d |> SqlValue.Date |> ret
                | _ -> fail ()
            | _ -> fail ()

        let asTimestamp =
            function
            | SqlValue.Timestamp _ as v -> ret v
            | SqlValue.Date d ->
                System.DateTimeOffset(d.ToDateTime System.TimeOnly.MinValue, System.TimeSpan.Zero)
                |> SqlValue.Timestamp
                |> ret
            | SqlValue.Text s ->
                match
                    System.DateTimeOffset.TryParse(
                        s,
                        System.Globalization.CultureInfo.InvariantCulture,
                        System.Globalization.DateTimeStyles.AssumeUniversal
                        ||| System.Globalization.DateTimeStyles.AdjustToUniversal
                    )
                with
                | true, t -> t |> SqlValue.Timestamp |> ret
                | _ -> fail ()
            | _ -> fail ()

        if isNull v then
            ret SqlValue.Null
        else
            match target with
            | DataType.Null -> ret SqlValue.Null
            | DataType.Boolean -> asBoolean v
            | DataType.Integer -> asInteger v
            | DataType.Real -> asReal v
            | DataType.Decimal -> asDecimal v
            | DataType.Text ->
                match v with
                | SqlValue.Text _ -> ret v
                | _ -> SqlLiteral.toSqlLiteral v |> SqlValue.Text |> ret
            | DataType.Blob ->
                match v with
                | SqlValue.Blob _ -> ret v
                | SqlValue.Text s -> System.Text.Encoding.UTF8.GetBytes s |> SqlValue.Blob |> ret
                | _ -> fail ()
            | DataType.Date -> asDate v
            | DataType.Timestamp -> asTimestamp v
            | DataType.Uuid ->
                match v with
                | SqlValue.Uuid _ -> ret v
                | SqlValue.Text s ->
                    match System.Guid.TryParse s with
                    | true, g -> g |> SqlValue.Uuid |> ret
                    | _ -> fail ()
                | _ -> fail ()

    /// CAST 用の明示変換。
    /// 暗黙変換 (`coerce`) より広く、TEXT から数値・BOOLEAN への変換も行う。
    let cast (target: DataType) (v: SqlValue) =
        let fail () =
            sprintf "'%s' は '%s' に変換できません" (SqlLiteral.toSqlLiteral v) (DataType.toSqlName target)
            |> DbError.typeError
            |> err

        if isNull v then
            ret SqlValue.Null
        else
            match target, v with
            | DataType.Null, _ -> ret SqlValue.Null
            | DataType.Integer, SqlValue.Text s ->
                match
                    System.Int64.TryParse(
                        s,
                        System.Globalization.NumberStyles.Integer,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, i -> ret (SqlValue.Int i)
                | _ -> fail ()
            | DataType.Real, SqlValue.Text s ->
                match
                    System.Double.TryParse(
                        s,
                        System.Globalization.NumberStyles.Float,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, f -> ret (vReal f)
                | _ -> fail ()
            | DataType.Decimal, SqlValue.Text s ->
                match
                    System.Decimal.TryParse(
                        s,
                        System.Globalization.NumberStyles.Number,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, d -> ret (SqlValue.Decimal d)
                | _ -> fail ()
            | DataType.Boolean, SqlValue.Text s ->
                match s.Trim().ToUpperInvariant() with
                | "TRUE"
                | "T"
                | "1" -> ret TrueValue
                | "FALSE"
                | "F"
                | "0" -> ret FalseValue
                | _ -> fail ()
            | _ -> coerce target v

/// 復号済みの行。
/// 列マスクのプレースホルダ（`SqlValue.Unloaded`）を含まないことが構築時に保証される。
/// <remarks>
/// ケースは非公開で、構築は `LoadedRow.create` だけ。
/// 結果の境界（`StatementResult` / `RowReader`）はこの型を経由するので、
/// 復号していない行が公開 API へ漏れる経路は型で塞がれている。
/// `toArray` の配列は内部で複製済みのものを共有する（書き換えないこと）。
/// </remarks>
[<Struct>]
type LoadedRow =
    private
    | LoadedRow of SqlValue array

    /// 列数。
    member this.Length =
        match this with
        | LoadedRow row -> row.Length

    /// 列の値。
    member this.Item
        with get index =
            match this with
            | LoadedRow row -> row.[index]

[<RequireQualifiedAccess>]
module LoadedRow =
    /// 復号済みであることを検査して行を作る。
    /// `SqlValue.Unloaded` が混ざっていれば `InternalError`。
    let internal create (row: SqlValue array) =
        match SqlValue.ensureLoaded row with
        | Ok loaded -> ret (LoadedRow loaded)
        | Error e -> err e

    /// 内部配列を取り出す（コピーしない）。
    let toArray (LoadedRow row) = row
