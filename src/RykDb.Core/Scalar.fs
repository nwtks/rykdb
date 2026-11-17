namespace RykDb.Core

module internal Scalar =
    /// LIKE のトークン。
    /// リテラルは UTF-16 の code unit ではなく Unicode コードポイントで持つ
    /// （`_` の 1文字と同じ単位に揃える）。
    type private LikeToken =
        | LiteralCodePoint of int
        | AnyOne
        | AnyMany

    /// コンパイル済みの LIKE パターン。
    /// パターンは行ごとに変わらないので、1回だけトークン列にして再利用する。
    type private LikePattern = { Tokens: LikeToken array }

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

    /// ESCAPE を解釈してパターンをトークン列にする。
    /// ESCAPE の後に文字が無ければ TypeError。
    /// ESCAPE 文字とエスケープした文字は 1 コードポイントとして扱う。
    let private compileTokens (escape: int option) (pattern: string) =
        let rec loop i acc =
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

                        loop (i + w + nextW) (LiteralCodePoint nextCp :: acc)
                | _ ->
                    match pattern.[i] with
                    | '%' -> loop (i + 1) (AnyMany :: acc)
                    | '_' -> loop (i + 1) (AnyOne :: acc)
                    | _ -> loop (i + w) (LiteralCodePoint cp :: acc)

        loop 0 []

    /// ESCAPE なしを表すキャッシュキー。
    [<Literal>]
    let private NoEscape = -1

    /// キャッシュするパターン数の上限。
    /// 列パターン（行ごとに違う文字列）で辞書が単調に増えないようにする。
    [<Literal>]
    let private MaxCachedLikePatterns = 1024

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

    let private matchLikeTokens (tokens: LikeToken array) (text: System.ReadOnlySpan<char>) =
        goLike tokens text 0 0 -1 0

    /// 1クエリ分のコンパイル済み LIKE パターン。
    /// resolver が所有するので、クエリをまたいで状態を共有しない。
    type internal LikeCache() =
        let cache =
            System.Collections.Generic.Dictionary<struct (int * string), Result<LikePattern, DbError>>()

        member private _.Compile (escapeCode: int) (pattern: string) =
            let key = struct (escapeCode, pattern)

            if cache.ContainsKey key then
                cache.[key]
            else
                if cache.Count >= MaxCachedLikePatterns then
                    cache.Clear()

                let escape = if escapeCode < 0 then None else Some escapeCode
                let compiled = compileTokens escape pattern
                cache.[key] <- compiled
                compiled

        member this.Match (pattern: string) (text: string) =
            match this.Compile NoEscape pattern with
            | Ok compiled -> matchLikeTokens compiled.Tokens (System.MemoryExtensions.AsSpan text)
            | Error _ -> false

        member this.MatchWithEscape (escape: int) (pattern: string) (text: string) =
            this.Compile escape pattern
            |> Result.map (fun compiled -> matchLikeTokens compiled.Tokens (System.MemoryExtensions.AsSpan text))

    /// LIKE のパターン照合。
    /// `%` は任意の文字列、`_` は Unicode コードポイント 1個。
    /// 正規表現に落とさずに手で照合する（LIKE の方言差を吸収するため）。
    /// パターンはキャッシュしたトークン列を使う（行ごとに解析しない）。
    /// バックトラックは 1段だけ（無限ループしないことが最優先）。
    let likeMatch (pattern: string) (text: string) = LikeCache().Match pattern text

    /// ESCAPE 付き LIKE。
    /// `escape` は Unicode コードポイント。
    let likeMatchWithEscape (escape: int) (pattern: string) (text: string) =
        LikeCache().MatchWithEscape escape pattern text

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

    /// Unicode のコードポイント数を数える。
    /// `string.Length` は UTF-16 の code unit 数なので、
    /// サロゲートペア（BMP 外の文字）を 1文字として数える。
    let private charLength (s: string) =
        let rec loop i n =
            if i >= s.Length then
                n
            elif
                System.Char.IsHighSurrogate s.[i]
                && i + 1 < s.Length
                && System.Char.IsLowSurrogate s.[i + 1]
            then
                loop (i + 2) (n + 1)
            else
                loop (i + 1) (n + 1)

        loop 0 0

    /// スカラー関数。
    /// 未知の関数はエラー。
    let applyFunc (name: string) (args: SqlValue array) =
        let upper = name.ToUpperInvariant()

        let arity (n: int) =
            if args.Length = n then
                OkUnit
            else
                err (sprintf "%s 関数は %d個の引数が必要ですが、%d個渡されました" upper n args.Length |> DbError.name)

        let asStr (v: SqlValue) =
            match SqlValue.asText v with
            | Some t -> t
            | None -> SqlValue.toDisplayString v

        match upper with
        | "UPPER" ->
            arity 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = asStr args.[0]
                    ret (SqlValue.Text(t.ToUpperInvariant()))
        | "LOWER" ->
            arity 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = asStr args.[0]
                    ret (SqlValue.Text(t.ToLowerInvariant()))
        | "LENGTH" ->
            arity 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = asStr args.[0]
                    ret (SqlValue.Int(int64 (charLength t)))
        | "ABS" ->
            arity 1
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
        | "COALESCE" ->
            let rec find (i: int) =
                if i >= args.Length then ret SqlValue.Null
                elif SqlValue.isNull args.[i] then find (i + 1)
                else ret args.[i]

            find 0
        | "TRIM" ->
            arity 1
            >>= fun () ->
                if SqlValue.isNull args.[0] then
                    ret SqlValue.Null
                else
                    let t = (asStr args.[0]).Trim()
                    ret (SqlValue.Text t)
        | "ROUND" ->
            arity 1
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
