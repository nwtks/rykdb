// ===========================================================================
// Ast.fs（式・文の走査ユーティリティ）
// ===========================================================================

module RykDb.AstTests

open Xunit
open RykDb.Core

let private literal l = Expr.Literal l
let private column q n = Expr.Column { Qualifier = q; Name = n }
let private name n = Expr.Name n
let private unary op e = Expr.Unary(op, e)
let private binary op a b = Expr.Binary(op, a, b)


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
            [ (name "a", literal (Literal.Int 1L)); (name "b", literal (Literal.Int 2L)) ],
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
