// ===========================================================================
// Eval.fs のテスト: 式評価（算術・NULL・3 値論理・LIKE・スカラー関数）
// ===========================================================================

module RykDb.EvalTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 2 列（a, b）のスキーマ。
let private schema2 =
    outSchema
        [
            outCol "a" None (Some DataType.Integer)
            outCol "b" None (Some DataType.Integer)
        ]

let private row2 (a: int64) (b: int64) = [| SqlValue.Int a; SqlValue.Int b |]

/// 1 行を作る。
let private row (values: SqlValue list) = List.toArray values

let private col (name: string) =
    Expr.Column { Qualifier = None; Name = name }

let private ev (row: SqlValue array) (e: Expr) = Eval.eval schema2 row e

let private evI (row: SqlValue array) (e: Expr) =
    match ok (ev row e) with
    | SqlValue.Int i -> i
    | v -> failwithf "INTEGER 以外が返りました: %A" v

[<Fact>]
let ``Eval: LIKE の % と _`` () =
    let schema = outSchema [ outCol "s" None (Some DataType.Text) ]

    let test (pattern: string) (text: string) =
        let e = Expr.Like(col "s", Expr.Literal(Literal.String pattern), false)

        match ok (Eval.eval schema [| SqlValue.Text text |] e) with
        | SqlValue.Bool b -> b
        | v -> failwithf "BOOLEAN 以外: %A" v

    Assert.True(test "ab%" "abcdef")
    // "%cd" は「cd で終わる」必要があり、"abcdef" は "def" で終わるので不一致。
    Assert.False(test "%cd" "abcdef")
    Assert.True(test "%cd" "abcd")
    Assert.True(test "a_c" "abc")
    Assert.False(test "a_c" "abbc")
    Assert.True(test "%" "anything")
    // `%` の後に何もない（0 文字一致）も許す
    Assert.True(test "abc%" "abc")

[<Fact>]
let ``Eval: スカラー関数`` () =
    let schema = outSchema [ outCol "s" None (Some DataType.Text) ]

    let call (name: string) (args: Expr list) =
        ok (Eval.eval schema [| SqlValue.Text "  AbC  " |] (Expr.FuncCall(name, args)))

    match call "UPPER" [ col "s" ] with
    | SqlValue.Text t -> Assert.Equal("  ABC  ", t)
    | v -> Assert.Fail(sprintf "UPPER が文字列ではありません: %A" v)

    match call "TRIM" [ col "s" ] with
    | SqlValue.Text t -> Assert.Equal("AbC", t)
    | v -> Assert.Fail(sprintf "TRIM が文字列ではありません: %A" v)

    // 未知の関数はエラー
    expectError "未知の関数が OK になっています" (Eval.eval schema [||] (Expr.FuncCall("NOPE", [])))

[<Fact>]
let ``Eval: スカラー関数の LOWER / LENGTH / COALESCE`` () =
    let schema = outSchema [ outCol "s" None (Some DataType.Text) ]

    let call (name: string) (args: Expr list) =
        ok (Eval.eval schema [| SqlValue.Text "  AbC  " |] (Expr.FuncCall(name, args)))

    match call "LOWER" [ col "s" ] with
    | SqlValue.Text t -> Assert.Equal("  abc  ", t)
    | v -> Assert.Fail(sprintf "LOWER が文字列ではありません: %A" v)

    match call "LENGTH" [ col "s" ] with
    | SqlValue.Int n -> Assert.Equal(7L, n)
    | v -> Assert.Fail(sprintf "LENGTH が整数ではありません: %A" v)

    // NULL は NULL のまま伝播する
    Assert.Equal(SqlValue.Null, call "LENGTH" [ Expr.Literal Literal.Null ])

    // COALESCE は最初の非 NULL を返し、全部 NULL なら NULL
    Assert.Equal(SqlValue.Int 5L, call "COALESCE" [ Expr.Literal Literal.Null; Expr.Literal(Literal.Int 5L) ])
    Assert.Equal(SqlValue.Int 7L, call "COALESCE" [ Expr.Literal(Literal.Int 7L); Expr.Literal(Literal.Int 5L) ])
    Assert.Equal(SqlValue.Null, call "COALESCE" [ Expr.Literal Literal.Null; Expr.Literal Literal.Null ])
    Assert.Equal(SqlValue.Null, call "COALESCE" [])

[<Fact>]
let ``Eval: スカラー関数の ABS / ROUND`` () =
    let schema = outSchema [ outCol "s" None (Some DataType.Text) ]

    let call (name: string) (args: Expr list) =
        Eval.eval schema [||] (Expr.FuncCall(name, args))

    Assert.Equal(SqlValue.Int 3L, ok (call "ABS" [ Expr.Literal(Literal.Int -3L) ]))
    Assert.Equal(SqlValue.Real 2.5, ok (call "ABS" [ Expr.Literal(Literal.Real -2.5) ]))
    Assert.Equal(SqlValue.Decimal 1.5M, ok (call "ABS" [ Expr.Literal(Literal.Decimal -1.5M) ]))
    Assert.Equal(SqlValue.Null, ok (call "ABS" [ Expr.Literal Literal.Null ]))
    expectErrorKind ErrorKind.TypeError "ABS は数値以外を受け付けない" (call "ABS" [ Expr.Literal(Literal.String "x") ])

    // ROUND は 0.5 を遠い方へ丸める（AwayFromZero）
    Assert.Equal(SqlValue.Real 3.0, ok (call "ROUND" [ Expr.Literal(Literal.Real 2.5) ]))
    Assert.Equal(SqlValue.Real 2.0, ok (call "ROUND" [ Expr.Literal(Literal.Real 2.4) ]))
    Assert.Equal(SqlValue.Int 7L, ok (call "ROUND" [ Expr.Literal(Literal.Int 7L) ]))
    Assert.Equal(SqlValue.Null, ok (call "ROUND" [ Expr.Literal Literal.Null ]))
    expectErrorKind ErrorKind.TypeError "ROUND は数値以外を受け付けない" (call "ROUND" [ Expr.Literal(Literal.String "x") ])

    // 引数個数の不一致は名前エラー
    expectErrorKind ErrorKind.NameError "引数が足りない" (call "UPPER" [])

[<Fact>]
let ``Eval: 算術は SQL の昇格規則に従う`` () =
    // int + int = int
    Assert.Equal<int64>(7L, evI (row2 3L 4L) (Expr.Binary(BinaryOp.Add, col "a", col "b")))

    // int / int は整数除算（SQL の INTEGER と同じ）
    Assert.Equal<int64>(3L, evI (row2 7L 2L) (Expr.Binary(BinaryOp.Div, col "a", col "b")))

    // 0 での除算はエラー
    match ev (row2 1L 0L) (Expr.Binary(BinaryOp.Div, col "a", col "b")) with
    | Error _ -> ()
    | Ok v -> Assert.Fail(sprintf "0 除算が OK になっています: %A" v)

[<Fact>]
let ``Eval: NULL が算術に絡むと NULL`` () =
    let r = [| SqlValue.Int 1L; SqlValue.Null |]
    Assert.True(SqlValue.isNull (ok (ev r (Expr.Binary(BinaryOp.Add, col "a", col "b")))))

[<Fact>]
let ``Eval: NULL の比較は NULL（UNKNOWN）であって FALSE ではない`` () =
    let r = [| SqlValue.Null; SqlValue.Int 1L |]

    // `NULL = 1` は NULL
    Assert.True(SqlValue.isNull (ok (ev r (Expr.Binary(BinaryOp.Eq, col "a", col "b")))))

    // `NULL <> 1` 도 NULL（FALSE ではない）
    Assert.True(SqlValue.isNull (ok (ev r (Expr.Binary(BinaryOp.Ne, col "a", col "b")))))

    // `NULL IS NULL` は TRUE
    match ok (ev r (Expr.IsNull(col "a", false))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "IS NULL が TRUE になりません: %A" v)

[<Fact>]
let ``Eval: 3 値論理の AND / OR / NOT`` () =
    // FALSE AND UNKNOWN = FALSE
    let r1 = [| SqlValue.Bool false; SqlValue.Null |]

    match ok (ev r1 (Expr.Binary(BinaryOp.And, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.False b
    | v -> Assert.Fail(sprintf "FALSE AND NULL が FALSE になりません: %A" v)

    // TRUE OR UNKNOWN = TRUE
    let r2 = [| SqlValue.Bool true; SqlValue.Null |]

    match ok (ev r2 (Expr.Binary(BinaryOp.Or, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "TRUE OR NULL が TRUE になりません: %A" v)

    // NOT UNKNOWN = UNKNOWN
    let r3 = [| SqlValue.Null; SqlValue.Int 0L |]

    Assert.True(SqlValue.isNull (ok (ev r3 (Expr.Unary(UnaryOp.Not, col "a")))))

[<Fact>]
let ``Eval: AND / OR は右辺の FALSE / TRUE も 3 値論理どおり`` () =
    // TRUE AND FALSE = FALSE（boolOf 由来の表だと右辺の FALSE が UNKNOWN に化けて NULL になっていた）
    let r1 = [| SqlValue.Bool true; SqlValue.Bool false |]

    match ok (ev r1 (Expr.Binary(BinaryOp.And, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.False b
    | v -> Assert.Fail(sprintf "TRUE AND FALSE が FALSE になりません: %A" v)

    // FALSE OR TRUE = TRUE（右辺の TRUE が UNKNOWN に化けて NULL になっていた）
    let r2 = [| SqlValue.Bool false; SqlValue.Bool true |]

    match ok (ev r2 (Expr.Binary(BinaryOp.Or, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "FALSE OR TRUE が TRUE になりません: %A" v)

    // NULL OR TRUE = TRUE（片方が NULL でも TRUE は TRUE のまま）
    let r3 = [| SqlValue.Null; SqlValue.Bool true |]

    match ok (ev r3 (Expr.Binary(BinaryOp.Or, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "NULL OR TRUE が TRUE になりません: %A" v)

    // NULL AND FALSE = FALSE
    let r4 = [| SqlValue.Null; SqlValue.Bool false |]

    match ok (ev r4 (Expr.Binary(BinaryOp.And, col "a", col "b"))) with
    | SqlValue.Bool b -> Assert.False b
    | v -> Assert.Fail(sprintf "NULL AND FALSE が FALSE になりません: %A" v)

[<Fact>]
let ``Eval: BETWEEN と IN は NULL を正しく扱う`` () =
    let r = [| SqlValue.Int 5L; SqlValue.Int 1L |]

    // 5 BETWEEN 1 AND 10 = TRUE
    match ok (ev r (Expr.Between(col "a", col "b", Expr.Literal(Literal.Int 10L), false))) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "BETWEEN が TRUE になりません: %A" v)

    // 5 NOT IN (1, 2) = TRUE
    let notIn =
        ok (ev r (Expr.InList(col "a", [ Expr.Literal(Literal.Int 1L); Expr.Literal(Literal.Int 2L) ], true)))

    match notIn with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "NOT IN が TRUE になりません: %A" v)

[<Fact>]
let ``末尾再帰: 巨大な IN 候補と WHEN 節を評価できる`` () =
    let r = row2 0L 1L
    let n = 100_000

    // `eval ... >>= fun` のままだと候補の数だけスタックが積まれる
    // （→ StackOverflow / exit 134）。`match` で受けて末尾再帰にしてある。
    let candidates = [ for i in 1..n -> Expr.Literal(Literal.Int(int64 i)) ]

    match ok (ev r (Expr.InList(col "a", candidates, false))) with
    | SqlValue.Bool b -> Assert.False b
    | v -> Assert.Fail(sprintf "10 万件の IN が FALSE になりません: %A" v)

    // WHEN 節も同じ（全部偽なので ELSE が返る）
    let branches =
        [
            for _ in 1..n -> Expr.Literal(Literal.Bool false), Expr.Literal(Literal.Int 1L)
        ]

    Assert.Equal<int64>(9L, evI r (Expr.Case(branches, Some(Expr.Literal(Literal.Int 9L)))))

[<Fact>]
let ``末尾再帰: 10 万段の式の木を評価できる`` () =
    let depth = 100_000

    // 「eval ... >>= fun」の実装では 1 段ごとにフレームが積もり 10 万段で
    // StackOverflow（exit 134）になる。継続を値として持つ評価機（Eval.step）は
    // ヒープの継続スタックだけが伸びるので、木の深さに依存せず評価できる。
    let nots =
        [ 1..depth ]
        |> List.fold (fun (acc: Expr) _ -> Expr.Unary(UnaryOp.Not, acc)) (Expr.Literal(Literal.Bool true))

    match ok (ev [||] nots) with
    // 10 万は偶数なので NOT は打ち消し合って TRUE になる。
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "NOT を %d 段重ねた式が評価できません: %A" depth v)

    // 二項演算子の左深い木（AND の鎖）も同じ。全段 TRUE を通すと全体で TRUE。
    let ands =
        [ 1..depth ]
        |> List.fold
            (fun (acc: Expr) _ -> Expr.Binary(BinaryOp.And, acc, Expr.Literal(Literal.Bool true)))
            (Expr.Literal(Literal.Bool true))

    match ok (ev [||] ands) with
    | SqlValue.Bool b -> Assert.True b
    | v -> Assert.Fail(sprintf "AND を %d 段重ねた式が評価できません: %A" depth v)

[<Fact>]
let ``Eval: WHERE では UNKNOWN を偽として扱う`` () =
    let r = [| SqlValue.Null; SqlValue.Int 1L |]
    let pred = Expr.Binary(BinaryOp.Gt, col "a", col "b")

    // UNKNOWN なので偽
    Assert.False(ok (Eval.matches schema2 r pred))
