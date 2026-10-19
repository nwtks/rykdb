// ===========================================================================
// Ast.fs（式・文の走査ユーティリティ）
// ===========================================================================

module RykDb.AstTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private literal l = Expr.Literal l
let private column q n = Expr.Column { Qualifier = q; Name = n }
let private name n = Expr.Name n
let private unary op e = Expr.Unary(op, e)
let private binary op a b = Expr.Binary(op, a, b)

[<Fact>]
let ``Ast: children は直接の子ノードを返す`` () =
    let e = Expr.Binary(BinaryOp.Add, Expr.Name "a", Expr.Name "b")
    let cs = Expr.children e
    Assert.Equal(2, cs.Length)

    // リーフは空
    Assert.Empty(Expr.children (Expr.Name "x"))

    // 3 引数・リスト・LIKE も子を返す
    Assert.Equal(3, (Expr.children (Expr.Between(name "a", name "b", name "c", false))).Length)
    Assert.Equal(3, (Expr.children (Expr.InList(name "a", [ name "b"; name "c" ], false))).Length)
    Assert.Equal(2, (Expr.children (Expr.Like(name "a", name "b", false))).Length)

[<Fact>]
let ``Ast: mapChildren は子を変換する`` () =
    let e = Expr.Binary(BinaryOp.Add, Expr.Name "a", Expr.Name "b")
    let e2 = Expr.mapChildren (fun _ -> Expr.Name "z") e

    match e2 with
    | Expr.Binary(_, Expr.Name "z", Expr.Name "z") -> ()
    | other -> Assert.Fail(sprintf "mapChildren が想定外: %A" other)

[<Fact>]
let ``Ast: fold は全ノードを訪問する`` () =
    let e = Expr.Binary(BinaryOp.Add, Expr.Name "a", Expr.Name "b")
    let count = Expr.fold (fun acc _ -> acc + 1) 0 e
    Assert.Equal(3, count)

[<Fact>]
let ``末尾再帰: AST の深い走査でスタックを食わない`` () =
    let depth = 200_000

    // テスト側も末尾再帰（fold）で深い式を作る
    let deep =
        List.init depth (fun _ -> ())
        |> List.fold (fun acc _ -> unary UnaryOp.Neg acc) (name "x")

    // ColumnRefs（明示スタック版）。最も深い式は EName なので列参照は 0 個。
    Assert.Empty(Expr.columnRefs deep)

    // isAggregate（明示スタック版）
    Assert.False(Expr.isAggregate deep)

    // toDisplayString（StringBuilder 蓄積版）
    let rendered = Expr.toDisplayString deep
    Assert.Equal(depth * 4 + 1, rendered.Length) // 1 段ごとに "(-" と ")" の 4 文字

[<Fact>]
let ``Ast: ColumnRefs が各ノードから列参照を集める`` () =
    let c1 = column (Some "t") "a"
    let c2 = column None "b"

    // Binary / AggCall / FuncCall / IsNull を 1 つの式に混ぜる
    let sample =
        Expr.Binary(
            BinaryOp.Add,
            Expr.AggCall(AggFunc.Max, false, c1),
            Expr.FuncCall("ABS", [ c2; Expr.IsNull(c1, false) ])
        )

    let got = Expr.columnRefs sample |> List.map (fun c -> c.Display) |> List.sort
    Assert.Equal<string list>([ "b"; "t.a"; "t.a" ] |> List.sort, got)

    // Case の中（WHEN 条件・THEN 本体・ELSE）も集める
    let caseE =
        Expr.Case([ (Expr.Binary(BinaryOp.Gt, column None "x", c1), column None "y") ], Some(column None "z"))

    let caseRefs = Expr.columnRefs caseE |> List.map (fun c -> c.Display) |> List.sort
    Assert.Equal<string list>([ "x"; "t.a"; "y"; "z" ] |> List.sort, caseRefs)

    // 列参照のない式は空
    Assert.Empty(Expr.columnRefs (Expr.Binary(BinaryOp.Add, name "a", literal (Literal.Int 1L))))

[<Fact>]
let ``末尾再帰: 集計呼び出しを含む深い AST も走査できる`` () =
    let depth = 100_000

    let deep =
        List.init depth (fun _ -> ())
        |> List.fold (fun acc _ -> binary BinaryOp.Add acc (Expr.AggCall(AggFunc.Count, false, (name "x")))) (name "y")

    Assert.True(Expr.isAggregate deep)
    Assert.Empty(Expr.columnRefs deep)

// ---------------------------------------------------------------------------
// toDisplayString の回帰（CPS 化の際に実際に壊れた箇所）
// ---------------------------------------------------------------------------

[<Fact>]
let ``Ast: InList と Case の表示が壊れない（回帰）`` () =
    // 以前は要素の区切りが壊れて "(a IN (, 1, 2))" のように出力されていた
    let inList =
        Expr.InList(name "a", [ literal (Literal.Int 1L); literal (Literal.Int 2L) ], false)

    Assert.Equal<string>("(a IN (1, 2))", Expr.toDisplayString inList)

    // 要素 1 個でも括弧が閉じる
    let one = Expr.InList(name "a", [ literal (Literal.Int 1L) ], true)
    Assert.Equal<string>("(a NOT IN (1))", Expr.toDisplayString one)

    // 以前は WHEN / THEN / ELSE / END が出力されていなかった
    let caseE =
        Expr.Case(
            [ name "a", literal (Literal.Int 1L); name "b", literal (Literal.Int 2L) ],
            Some(literal (Literal.Int 3L))
        )

    Assert.Equal<string>("(CASE WHEN a THEN 1 WHEN b THEN 2 ELSE 3 END)", Expr.toDisplayString caseE)

    // ELSE なし
    let caseNoElse = Expr.Case([ (name "a", literal (Literal.Int 1L)) ], None)
    Assert.Equal<string>("(CASE WHEN a THEN 1 END)", Expr.toDisplayString caseNoElse)

[<Fact>]
let ``Ast: その他の演算子の表示`` () =
    let a = name "a"

    Assert.Equal<string>("(a IS NULL)", Expr.toDisplayString (Expr.IsNull(a, false)))
    Assert.Equal<string>("(a IS NOT NULL)", Expr.toDisplayString (Expr.IsNull(a, true)))

    Assert.Equal<string>(
        "(a BETWEEN 1 AND 2)",
        Expr.toDisplayString (Expr.Between(a, literal (Literal.Int 1L), literal (Literal.Int 2L), false))
    )

    Assert.Equal<string>(
        "(a NOT BETWEEN 1 AND 2)",
        Expr.toDisplayString (Expr.Between(a, literal (Literal.Int 1L), literal (Literal.Int 2L), true))
    )

    Assert.Equal<string>("(NOT a)", Expr.toDisplayString (Expr.Unary(UnaryOp.Not, a)))
    Assert.Equal<string>("(- a)", Expr.toDisplayString (Expr.Unary(UnaryOp.Neg, a)))

    Assert.Equal<string>("(a LIKE 'x%')", Expr.toDisplayString (Expr.Like(a, literal (Literal.String "x%"), false)))

    Assert.Equal<string>("t.*", Expr.toDisplayString (Expr.QualifiedStar "t"))

[<Fact>]
let ``Ast: mapChildren は全ケースで子を置き換える`` () =
    let bump (e: Expr) =
        match e with
        | Expr.Name n -> Expr.Name(n + "!")
        | other -> other

    let namesOf (e: Expr) =
        Expr.fold
            (fun acc e ->
                match e with
                | Expr.Name n -> n :: acc
                | _ -> acc)
            []
            e
        |> List.sort

    // 子を持つ全ケースを列挙し、直接の子（リーフ名）がすべて置き換わることを見る
    let samples =
        [
            Expr.Unary(UnaryOp.Not, name "a"), [ "a" ]
            Expr.Binary(BinaryOp.Add, name "a", name "b"), [ "a"; "b" ]
            Expr.IsNull(name "a", false), [ "a" ]
            Expr.Between(name "a", name "b", name "c", false), [ "a"; "b"; "c" ]
            Expr.InList(name "a", [ name "b"; name "c" ], false), [ "a"; "b"; "c" ]
            Expr.Like(name "a", name "b", false), [ "a"; "b" ]
            Expr.Case([ (name "a", name "b") ], Some(name "c")), [ "a"; "b"; "c" ]
            Expr.Case([ (name "a", name "b") ], None), [ "a"; "b" ]
            Expr.FuncCall("ABS", [ name "a" ]), [ "a" ]
            Expr.AggCall(AggFunc.Sum, false, name "a"), [ "a" ]
        ]

    for e, expected in samples do
        Assert.Equal<string list>(
            expected |> List.map (fun n -> n + "!") |> List.sort,
            namesOf (Expr.mapChildren bump e)
        )

    // リーフは変換されない
    Assert.Equal<string>("t.*", Expr.toDisplayString (Expr.mapChildren bump (Expr.QualifiedStar "t")))
    Assert.Equal<string>("a", Expr.toDisplayString (Expr.mapChildren bump (name "a")))

[<Fact>]
let ``Ast: mapChildrenM は全ケースで子を置き換える`` () =
    let bump (e: Expr) =
        match e with
        | Expr.Name n -> ret (Expr.Name(n + "!"))
        | other -> ret other

    let namesOf (e: Expr) =
        Expr.fold
            (fun acc e ->
                match e with
                | Expr.Name n -> n :: acc
                | _ -> acc)
            []
            e
        |> List.sort

    let samples =
        [
            Expr.Unary(UnaryOp.Not, name "a"), [ "a" ]
            Expr.Binary(BinaryOp.Add, name "a", name "b"), [ "a"; "b" ]
            Expr.IsNull(name "a", false), [ "a" ]
            Expr.Between(name "a", name "b", name "c", false), [ "a"; "b"; "c" ]
            Expr.InList(name "a", [ name "b"; name "c" ], false), [ "a"; "b"; "c" ]
            Expr.Like(name "a", name "b", false), [ "a"; "b" ]
            Expr.Case([ (name "a", name "b") ], Some(name "c")), [ "a"; "b"; "c" ]
            Expr.Case([ (name "a", name "b") ], None), [ "a"; "b" ]
            Expr.FuncCall("ABS", [ name "a" ]), [ "a" ]
            Expr.AggCall(AggFunc.Sum, false, name "a"), [ "a" ]
        ]

    for e, expected in samples do
        let got = namesOf (ok (Expr.mapChildrenM bump e))
        Assert.Equal<string list>(expected |> List.map (fun n -> n + "!") |> List.sort, got)

    // リーフは変換されない
    Assert.Equal<string>("t.*", Expr.toDisplayString (ok (Expr.mapChildrenM bump (Expr.QualifiedStar "t"))))

    // 子の変換が失敗したら全体も失敗する
    expectError
        "子の変換失敗が伝播していません"
        (Expr.mapChildrenM (fun _ -> err (DbError.name "x")) (binary BinaryOp.Add (name "a") (name "b")))

[<Fact>]
let ``Ast: Literal と SqlValue の相互変換`` () =
    // BLOB リテラル → SqlValue
    match (Literal.Blob [| 1uy; 2uy |]).ToSqlValue with
    | SqlValue.Blob b -> Assert.Equal<byte array>([| 1uy; 2uy |], b)
    | v -> Assert.Fail(sprintf "BLOB が Blob になりません: %A" v)

    // SqlValue → リテラル表示（DDL 再構築用）。全ケースがリテラルへ落ちる。
    let cases: (SqlValue * Literal) list =
        [
            SqlValue.Null, Literal.Null
            SqlValue.Bool true, Literal.Bool true
            SqlValue.Int 3L, Literal.Int 3L
            SqlValue.Real 1.5, Literal.Real 1.5
            SqlValue.Decimal 2.5M, Literal.Decimal 2.5M
            SqlValue.Text "x", Literal.String "x"
            SqlValue.Blob [| 7uy |], Literal.Blob [| 7uy |]
            SqlValue.Date(System.DateOnly(2026, 10, 18)), Literal.String "2026-10-18"
            SqlValue.Timestamp(System.DateTimeOffset(2026, 10, 18, 1, 2, 3, System.TimeSpan.Zero)),
            Literal.String "2026-10-18T01:02:03.0000000Z"
            SqlValue.Uuid(System.Guid.Parse "00000000-0000-0000-0000-000000000001"),
            Literal.String "00000000-0000-0000-0000-000000000001"
        ]

    for v, expected in cases do
        Assert.Equal(expected, Literal.ofValue v)

[<Fact>]
let ``Ast: AggFunc と BinaryOp の表示名`` () =
    Assert.Equal("AVG", AggFunc.Avg.Name)
    Assert.Equal("LIKE", BinaryOp.Like.Display)
    Assert.Equal<string>("(a LIKE b)", Expr.toDisplayString (binary BinaryOp.Like (name "a") (name "b")))
