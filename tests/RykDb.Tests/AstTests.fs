// ===========================================================================
// Ast.fs（式・文の走査ユーティリティ）
// ===========================================================================

module RykDb.AstTests

open Xunit
open RykDb.Core

[<Fact>]
let ``末尾再帰: AST の深い走査でスタックを食わない`` () =
    let depth = 200_000

    // テスト側も末尾再帰（fold）で深い式を作る
    let deep =
        List.init depth (fun _ -> ())
        |> List.fold (fun acc _ -> Expr.unary UnaryOp.Neg acc) (Expr.name "x")

    // ColumnRefs（明示スタック版）。最も深い式は EName なので列参照は 0 個。
    Assert.Empty(deep.ColumnRefs)

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
        |> List.fold
            (fun acc _ -> Expr.binary BinaryOp.Add acc (Expr.AggCall(AggFunc.Count, false, (Expr.name "x"))))
            (Expr.name "y")

    Assert.True(Expr.isAggregate deep)
    Assert.Empty(deep.ColumnRefs)

// ---------------------------------------------------------------------------
// toDisplayString の回帰（CPS 化の際に実際に壊れた箇所）
// ---------------------------------------------------------------------------

[<Fact>]
let ``Ast: InList と Case の表示が壊れない（回帰）`` () =
    // 以前は要素の区切りが壊れて "(a IN (, 1, 2))" のように出力されていた
    let inList =
        Expr.InList(Expr.name "a", [ Expr.literal (Literal.Int 1L); Expr.literal (Literal.Int 2L) ], false)

    Assert.Equal<string>("(a IN (1, 2))", Expr.toDisplayString inList)

    // 要素 1 個でも括弧が閉じる
    let one = Expr.InList(Expr.name "a", [ Expr.literal (Literal.Int 1L) ], true)
    Assert.Equal<string>("(a NOT IN (1))", Expr.toDisplayString one)

    // 以前は WHEN / THEN / ELSE / END が出力されていなかった
    let caseE =
        Expr.Case(
            [
                (Expr.name "a", Expr.literal (Literal.Int 1L))
                (Expr.name "b", Expr.literal (Literal.Int 2L))
            ],
            Some(Expr.literal (Literal.Int 3L))
        )

    Assert.Equal<string>("(CASE WHEN a THEN 1 WHEN b THEN 2 ELSE 3 END)", Expr.toDisplayString caseE)

    // ELSE なし
    let caseNoElse = Expr.Case([ (Expr.name "a", Expr.literal (Literal.Int 1L)) ], None)
    Assert.Equal<string>("(CASE WHEN a THEN 1 END)", Expr.toDisplayString caseNoElse)

[<Fact>]
let ``Ast: その他の演算子の表示`` () =
    let a = Expr.name "a"

    Assert.Equal<string>("(a IS NULL)", Expr.toDisplayString (Expr.IsNull(a, false)))
    Assert.Equal<string>("(a IS NOT NULL)", Expr.toDisplayString (Expr.IsNull(a, true)))

    Assert.Equal<string>(
        "(a BETWEEN 1 AND 2)",
        Expr.toDisplayString (Expr.Between(a, Expr.literal (Literal.Int 1L), Expr.literal (Literal.Int 2L), false))
    )

    Assert.Equal<string>(
        "(a NOT BETWEEN 1 AND 2)",
        Expr.toDisplayString (Expr.Between(a, Expr.literal (Literal.Int 1L), Expr.literal (Literal.Int 2L), true))
    )

    Assert.Equal<string>("(NOT a)", Expr.toDisplayString (Expr.Unary(UnaryOp.Not, a)))
    Assert.Equal<string>("(- a)", Expr.toDisplayString (Expr.Unary(UnaryOp.Neg, a)))

    Assert.Equal<string>(
        "(a LIKE 'x%')",
        Expr.toDisplayString (Expr.Like(a, Expr.literal (Literal.String "x%"), false))
    )

    Assert.Equal<string>("t.*", Expr.toDisplayString (Expr.QualifiedStar "t"))

[<Fact>]
let ``Ast: ColumnRefs が各ノードから列参照を集める`` () =
    let c1 = Expr.column (Some "t") "a"
    let c2 = Expr.column None "b"

    // Binary / AggCall / FuncCall / IsNull を 1 つの式に混ぜる
    let sample =
        Expr.Binary(
            BinaryOp.Add,
            Expr.AggCall(AggFunc.Max, false, c1),
            Expr.FuncCall("ABS", [ c2; Expr.IsNull(c1, false) ])
        )

    let got = sample.ColumnRefs |> List.map (fun c -> c.Display) |> List.sort
    Assert.Equal<string list>([ "b"; "t.a"; "t.a" ] |> List.sort, got)

    // Case の中（WHEN 条件・THEN 本体・ELSE）も集める
    let caseE =
        Expr.Case(
            [ (Expr.Binary(BinaryOp.Gt, Expr.column None "x", c1), Expr.column None "y") ],
            Some(Expr.column None "z")
        )

    let caseRefs = caseE.ColumnRefs |> List.map (fun c -> c.Display) |> List.sort
    Assert.Equal<string list>([ "x"; "t.a"; "y"; "z" ] |> List.sort, caseRefs)

    // 列参照のない式は空
    Assert.Empty(Expr.Binary(BinaryOp.Add, Expr.name "a", Expr.literal (Literal.Int 1L)).ColumnRefs)
