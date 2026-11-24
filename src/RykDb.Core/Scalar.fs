namespace RykDb.Core

module internal Scalar =
    /// LIKE のトークン。
    /// リテラルは UTF-16 の code unit ではなく Unicode コードポイントで持つ
    /// （`_` の 1文字と同じ単位に揃える）。
    type internal LikeToken =
        | LiteralCodePoint of int
        | AnyOne
        | AnyMany

    /// コンパイル済みの LIKE パターン。
    /// リテラルは計画時に `Eval.prepare` が 1回だけコンパイルし、
    /// 行ごとの再コンパイルをしない（列パターンだけは評価のたびにコンパイルする）。
    type internal LikePattern = { Tokens: LikeToken array }

    /// 位置 i の Unicode コードポイントと、その UTF-16 幅（1 または 2）。
    /// 対になっていないサロゲートは 1 コード単位として返す（範囲外アクセスを避ける）。
    let private codePointAt (text: System.ReadOnlySpan<char>) (i: int) =
        let c = text.[i]

        if
            System.Char.IsHighSurrogate c
            && i + 1 < text.Length
            && System.Char.IsLowSurrogate text.[i + 1]
        then
            struct (System.Char.ConvertToUtf32(c, text.[i + 1]), 2)
        else
            struct (int c, 1)

    /// ESCAPE の解釈ループ（末尾再帰）。
    [<TailCall>]
    let rec private compileTokensLoop (escape: int option) (pattern: string) (i: int) (acc: LikeToken list) =
        if i >= pattern.Length then
            ret
                {
                    Tokens = List.rev acc |> List.toArray
                }
        else
            let struct (cp, w) = codePointAt (System.MemoryExtensions.AsSpan pattern) i

            match escape with
            | Some e when cp = e ->
                if i + w >= pattern.Length then
                    err (DbError.typeError "LIKE の ESCAPE の後に文字がありません")
                else
                    let struct (nextCp, nextW) =
                        codePointAt (System.MemoryExtensions.AsSpan pattern) (i + w)

                    compileTokensLoop escape pattern (i + w + nextW) (LiteralCodePoint nextCp :: acc)
            | _ ->
                match pattern.[i] with
                | '%' -> compileTokensLoop escape pattern (i + 1) (AnyMany :: acc)
                | '_' -> compileTokensLoop escape pattern (i + 1) (AnyOne :: acc)
                | _ -> compileTokensLoop escape pattern (i + w) (LiteralCodePoint cp :: acc)

    /// ESCAPE を解釈してパターンをトークン列にする。
    /// ESCAPE の後に文字が無ければ TypeError。
    /// ESCAPE 文字とエスケープした文字は 1 コードポイントとして扱う。
    let compileTokens (escape: int option) (pattern: string) = compileTokensLoop escape pattern 0 []

    /// ESCAPE なしを表すコードポイント。
    [<Literal>]
    let NoEscape = -1

    /// 末尾の連続する `%` を読み飛ばす。
    [<TailCall>]
    let rec private skipTrailingStars (tokens: LikeToken array) (i: int) =
        if i < tokens.Length && tokens.[i] = AnyMany then
            skipTrailingStars tokens (i + 1)
        else
            i

    /// テキストを 1 コードポイント進める（`%` のバックトラック用）。
    let private advanceCodePoint (text: System.ReadOnlySpan<char>) (i: int) =
        let struct (_, w) = codePointAt text i
        i + w

    /// トークン列によるワイルドカード照合。
    /// 最後の `%` だけを覚えて戻る貪欲法（指数的なバックトラックをしない）。
    /// `%` の位置は int（-1 = なし）で持つ。option は行ごとに割り当てるため使わない。
    /// span はローカル関数に捕捉できないため、ループはモジュールスコープに置く。
    /// テキストの位置は UTF-16 の code unit 添字だが、進める単位はコードポイント。
    [<TailCall>]
    let rec private goLike
        (tokens: LikeToken array)
        (text: System.ReadOnlySpan<char>)
        (ti: int)
        (pi: int)
        (star: int)
        (starTi: int)
        =
        let n = text.Length
        let m = tokens.Length

        if ti >= n then
            skipTrailingStars tokens pi >= m
        elif pi >= m then
            if star >= 0 && starTi < n then
                let next = advanceCodePoint text starTi
                goLike tokens text next (star + 1) star next
            else
                false
        else
            match tokens.[pi] with
            | AnyMany -> goLike tokens text ti (pi + 1) pi ti
            | AnyOne -> goLike tokens text (advanceCodePoint text ti) (pi + 1) star starTi
            | LiteralCodePoint cp ->
                let struct (c, w) = codePointAt text ti

                if c = cp then
                    goLike tokens text (ti + w) (pi + 1) star starTi
                elif star >= 0 && starTi < n then
                    let next = advanceCodePoint text starTi
                    goLike tokens text next (star + 1) star next
                else
                    false

    /// コンパイル済みトークン列で照合する（`Eval.prepare` がコンパイルした結果を使う）。
    let matchLikeTokens (tokens: LikeToken array) (text: System.ReadOnlySpan<char>) = goLike tokens text 0 0 -1 0

    /// ESCAPE 句の文字列を 1 コードポイントとして取り出す。
    /// コードポイントが 1個でなければ TypeError。
    let escapeCodePoint (s: string) =
        if s.Length = 1 then
            ret (int s.[0])
        elif
            s.Length = 2
            && System.Char.IsHighSurrogate s.[0]
            && System.Char.IsLowSurrogate s.[1]
        then
            ret (System.Char.ConvertToUtf32(s, 0))
        else
            err (DbError.typeError "LIKE の ESCAPE は 1 文字で指定してください")

    /// 文字列のコードポイント数を先頭から数える再帰ループ。
    [<TailCall>]
    let rec private charLengthLoop (s: string) (i: int) (n: int) =
        if i >= s.Length then
            n
        elif
            System.Char.IsHighSurrogate s.[i]
            && i + 1 < s.Length
            && System.Char.IsLowSurrogate s.[i + 1]
        then
            charLengthLoop s (i + 2) (n + 1)
        else
            charLengthLoop s (i + 1) (n + 1)

    /// Unicode のコードポイント数を数える。
    /// `string.Length` は UTF-16 の code unit 数なので、
    /// サロゲートペア（BMP 外の文字）を 1文字として数える。
    let private charLength (s: string) = charLengthLoop s 0 0

    /// 引数個数を検証する。
    let private arity (upper: string) (args: SqlValue array) (n: int) =
        if args.Length = n then
            OkUnit
        else
            err (sprintf "%s 関数は %d個の引数が必要ですが、%d個渡されました" upper n args.Length |> DbError.name)

    /// 引数を文字列として扱う（NULL 以外は表示形式に変換する）。
    let private asStr (v: SqlValue) =
        match SqlValue.asText v with
        | Some t -> t
        | None -> SqlValue.toDisplayString v

    /// 最初の NULL でない引数を探す。
    [<TailCall>]
    let rec private find (args: SqlValue array) (i: int) =
        if i >= args.Length then ret SqlValue.Null
        elif SqlValue.isNull args.[i] then find args (i + 1)
        else ret args.[i]

    /// スカラー関数。
    /// 未知の関数はエラー。
    let applyFunc (name: string) (args: SqlValue array) =
        let upper = name.ToUpperInvariant()

        match upper with
        | "UPPER" ->
            arity upper args 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = asStr args.[0]
                    ret (SqlValue.Text(t.ToUpperInvariant()))
        | "LOWER" ->
            arity upper args 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = asStr args.[0]
                    ret (SqlValue.Text(t.ToLowerInvariant()))
        | "LENGTH" ->
            arity upper args 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = asStr args.[0]
                    ret (SqlValue.Int(int64 (charLength t)))
        | "ABS" ->
            arity upper args 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    match args.[0] with
                    | SqlValue.Int i when i = System.Int64.MinValue ->
                        err (DbError.constraintViolation "ABS の結果が範囲を超えました")
                    | SqlValue.Int i -> ret (SqlValue.Int(abs i))
                    | SqlValue.Real f -> ret (SqlValue.vReal (abs f))
                    | SqlValue.Decimal d -> ret (SqlValue.Decimal(abs d))
                    | other ->
                        err (
                            sprintf "ABS は数値を受け付けますが %s です" (SqlLiteral.toSqlLiteral other)
                            |> DbError.typeError
                        )
        | "COALESCE" -> find args 0
        | "TRIM" ->
            arity upper args 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = (asStr args.[0]).Trim()
                    ret (SqlValue.Text t)
        | "ROUND" ->
            arity upper args 1
            >>= fun () ->
                match args.[0] with
                | SqlValue.Int i -> ret (SqlValue.Int i)
                | SqlValue.Real f -> ret (System.Math.Round(f, System.MidpointRounding.AwayFromZero) |> SqlValue.vReal)
                | SqlValue.Decimal d ->
                    ret (SqlValue.Decimal(System.Math.Round(d, System.MidpointRounding.AwayFromZero)))
                | SqlValue.Null -> ret SqlValue.Null
                | other ->
                    err (
                        sprintf "ROUND は数値を受け付けますが %s です" (SqlLiteral.toSqlLiteral other)
                        |> DbError.typeError
                    )
        | _ -> err (sprintf "未知のスカラー関数 '%s' です" name |> DbError.name)
