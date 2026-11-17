// ===========================================================================
// Scalar.fs のテスト: LIKE の照合（トークン化・ESCAPE・コードポイント）
// ===========================================================================

module RykDb.ScalarTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// テスト用の独立オラクル: LIKE パターンを正規表現へ変換する。
/// 本体は正規表現を使わない（方言差を吸収するため）ので、テスト側だけで照合する。
let private likeToRegex (escape: char option) (pattern: string) =
    let rec loop i acc =
        if i >= pattern.Length then
            List.rev acc
        else
            match escape with
            | Some e when pattern.[i] = e && i + 1 < pattern.Length ->
                loop (i + 2) (System.Text.RegularExpressions.Regex.Escape(string pattern.[i + 1]) :: acc)
            | _ ->
                match pattern.[i] with
                | '%' -> loop (i + 1) (".*" :: acc)
                | '_' -> loop (i + 1) ("." :: acc)
                | c -> loop (i + 1) (System.Text.RegularExpressions.Regex.Escape(string c) :: acc)

    let body = loop 0 [] |> String.concat ""
    System.Text.RegularExpressions.Regex("\\A" + body + "\\z", System.Text.RegularExpressions.RegexOptions.Singleline)

/// ESCAPE 付き LIKE を char で書けるようにする（本体はコードポイントを受け取る）。
let private likeEsc (escape: char) (pattern: string) (text: string) =
    Scalar.likeMatchWithEscape (int escape) pattern text

[<Fact>]
let ``Scalar.likeMatch: LIKE の空パターンと % 走査の境界`` () =
    // 空パターンは空テキストだけに一致する
    Assert.True(Scalar.likeMatch "" "")
    Assert.False(Scalar.likeMatch "" "x")
    // 末尾が固定のパターンで、テキストが短すぎて走査の上限に当たる
    Assert.False(Scalar.likeMatch "%abcd" "x")
    // 途中の % の次のセグメントが残りより長い
    Assert.False(Scalar.likeMatch "%ab%c" "a")
    // 途中の % は 1 文字ずつずらして再試行する（末尾固定の走査）
    Assert.True(Scalar.likeMatch "%ab" "xab")
    // 途中の % の後のセグメントも、最初の位置で失敗したら次の位置を試す
    Assert.True(Scalar.likeMatch "%a%" "xa")
    // 先頭の固定セグメントが一致しない
    Assert.False(Scalar.likeMatch "a%b" "xa")

[<Fact>]
let ``Scalar.likeMatch: 正規表現オラクルと一致する（ランダム）`` () =
    let rng = System.Random 20261118

    let randomFrom (alphabet: char array) (maxLen: int) =
        let len = rng.Next(maxLen + 1)
        System.String(Array.init len (fun _ -> alphabet.[rng.Next alphabet.Length]))

    let patternAlphabet = [| 'a'; 'b'; '%'; '_' |]
    let textAlphabet = [| 'a'; 'b'; 'c' |]

    for _ in 1..2000 do
        let pattern = randomFrom patternAlphabet 6
        let text = randomFrom textAlphabet 8
        let expected = (likeToRegex None pattern).IsMatch text
        Assert.Equal(expected, Scalar.likeMatch pattern text)

    // ESCAPE 付き。末尾が ESCAPE のパターンは TypeError なので生成対象から外す。
    let escAlphabet = [| 'a'; 'b'; '%'; '_'; '!' |]
    let escTextAlphabet = [| 'a'; 'b'; 'c'; '!'; '%'; '_' |]

    let trailingEscapes (pattern: string) =
        let rec count i n =
            if i >= 0 && pattern.[i] = '!' then
                count (i - 1) (n + 1)
            else
                n

        count (pattern.Length - 1) 0

    for _ in 1..2000 do
        let pattern = randomFrom escAlphabet 6
        let text = randomFrom escTextAlphabet 8

        if trailingEscapes pattern % 2 = 0 then
            let expected = (likeToRegex (Some '!') pattern).IsMatch text
            Assert.Equal(expected, ok (likeEsc '!' pattern text))

[<Fact>]
let ``Scalar.likeMatch: 計画時にコンパイルしたパターンは行ごとに配列を確保しない`` () =
    // リテラルパターンは `Eval.prepare` が 1回だけコンパイルし、
    // 以降はコンパイル済みトークン列で照合する。
    let pattern = "a%b%c"
    let text = "aXXbYYc"
    let schema = outSchema [ outCol "s" None (Some DataType.Text) ]

    let expr =
        Expr.Like(Expr.Column { Qualifier = None; Name = "s" }, Expr.Literal(Literal.String pattern), false, None)

    let prepared = Eval.prepare schema [ expr ]
    let compiled = ok (prepared.Like Scalar.NoEscape pattern)
    Assert.True(Scalar.matchLikeTokens compiled.Tokens (System.MemoryExtensions.AsSpan text))

    let before = System.GC.GetAllocatedBytesForCurrentThread()

    let rec loop i matched =
        if i >= 1000 then
            matched
        else
            let m =
                match prepared.Like Scalar.NoEscape pattern with
                | Ok cached -> Scalar.matchLikeTokens cached.Tokens (System.MemoryExtensions.AsSpan text)
                | Error _ -> false

            loop (i + 1) (if m then matched + 1 else matched)

    let matched = loop 0 0
    let allocated = System.GC.GetAllocatedBytesForCurrentThread() - before
    Assert.Equal(1000, matched)
    Assert.True(allocated < 10_000L, sprintf "1000 回で %d バイト確保しました" allocated)

[<Fact>]
let ``Scalar.likeMatchWithEscape: エスケープした % _ ! をリテラルにする`` () =
    Assert.True(ok (likeEsc '!' "a!%b" "a%b"))
    Assert.False(ok (likeEsc '!' "a!%b" "axb"))
    Assert.True(ok (likeEsc '!' "a!_b" "a_b"))
    Assert.False(ok (likeEsc '!' "a!_b" "axb"))
    Assert.True(ok (likeEsc '!' "a!!b" "a!b"))
    Assert.False(ok (likeEsc '!' "a!!b" "ab"))
    expectErrorKind ErrorKind.TypeError "末尾 ESCAPE" (likeEsc '!' "a!" "a")

[<Fact>]
let ``Scalar.likeMatch: _ とリテラルは Unicode コードポイント単位で照合する`` () =
    // サロゲートペア（BMP 外）は `_` 1個に一致する（UTF-16 の code unit 2個分ではない）
    Assert.True(Scalar.likeMatch "a_b" "a\U0001F600b")
    Assert.True(Scalar.likeMatch "a_b" "a\U00020BB7b")
    Assert.False(Scalar.likeMatch "a__b" "a\U0001F600b")
    // パターン中のサロゲートペアは 1 リテラルとして比較する
    Assert.True(Scalar.likeMatch "a\U0001F600b" "a\U0001F600b")
    Assert.False(Scalar.likeMatch "a\U0001F600b" "a\U0001F601b")
    // `%` のバックトラックもコードポイント単位で進む
    Assert.True(Scalar.likeMatch "%b" "\U0001F600b")
    Assert.True(Scalar.likeMatch "%\U0001F600" "x\U0001F600")
    Assert.False(Scalar.likeMatch "%\U0001F600" "x\U0001F601")
    // ESCAPE 直後のサロゲートペアも 1 リテラル
    Assert.True(ok (likeEsc '!' "a!\U0001F600b" "a\U0001F600b"))
    Assert.False(ok (likeEsc '!' "a!\U0001F600b" "a\U0001F601b"))
    // ESCAPE 文字自身も 1 コードポイント（サロゲートペアも可）
    let smile = System.Char.ConvertToUtf32("\U0001F600", 0)
    Assert.True(ok (Scalar.likeMatchWithEscape smile "a\U0001F600%b" "a%b"))
    Assert.False(ok (Scalar.likeMatchWithEscape smile "a\U0001F600%b" "axb"))
    // ESCAPE が 1 コードポイントでなければ TypeError
    expectErrorKind ErrorKind.TypeError "2 コードポイントの ESCAPE" (Scalar.escapeCodePoint "ab")
    expectErrorKind ErrorKind.TypeError "空の ESCAPE" (Scalar.escapeCodePoint "")
