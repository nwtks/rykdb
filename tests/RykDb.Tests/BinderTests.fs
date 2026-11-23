// ===========================================================================
// Binder.fs のテスト（名前の束縛・別名・述語の分解）
// ===========================================================================

module RykDb.BinderTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private pCol (n: string) =
    Expr.Column { Qualifier = Some "people"; Name = n }

[<Fact>]
let ``Binder.qualify: 修飾子無しの列が一意なら修飾名を付ける`` () =
    // people だけのスキーマでは `id` は people.id に一意に決まる。
    let only = Schema.ofTable "people" None peopleSchema
    let e = Expr.Column { Qualifier = None; Name = "id" }
    let bound = ok (Binder.qualify only e)

    match bound with
    | Expr.Column c -> Assert.Equal<string>("people", defaultArg c.Qualifier "")
    | other -> failActual "EColumn ではない" other

[<Fact>]
let ``Binder.qualify: 修飾子無しの列が曖昧なら付けない`` () =
    // t1.a と t2.a の両方が「a」
    let s1 =
        {
            Name = "t1"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes = []
        }

    let s2 =
        {
            Name = "t2"
            Columns = [ col "a" DataType.Integer ]
            PrimaryKey = [ 0 ]
            Indexes = []
        }

    let combined =
        Schema.ofTable "t1" None s1
        |> fun s -> Schema.concat s (Schema.ofTable "t2" None s2)

    let bound =
        ok (Binder.qualify combined (Expr.Column { Qualifier = None; Name = "a" }))

    match bound with
    | Expr.Column c -> Assert.True(System.String.IsNullOrEmpty(defaultArg c.Qualifier ""))
    | other -> failActual "Expr.EColumn ではない" other

[<Fact>]
let ``Binder.qualify: スターは修飾の対象外`` () =
    let only = Schema.ofTable "people" None peopleSchema
    let bound = ok (Binder.qualify only (Expr.QualifiedStar "people"))

    match bound with
    | Expr.QualifiedStar q -> Assert.Equal<string>("people", q)
    | other -> failActual "QualifiedStar ではない" other

[<Fact>]
let ``Binder.qualify: 全ノード種を qualify しても構造が保たれる`` () =
    let schema = Schema.ofTable "people" None peopleSchema
    // `id` は people だけの一意な列なので、裸の名前はすべて people.id に修飾される
    let n = Expr.Name "id"
    let q = pCol "id"
    let i1 = Expr.Literal(Literal.Int 1L)
    let i2 = Expr.Literal(Literal.Int 2L)
    let text = Expr.Literal(Literal.String "x%")

    // 入力（葉 = 裸の名前）と、葉を修飾済みにした期待値を並べて比較する
    let samples =
        [
            Expr.Unary(UnaryOp.Not, n), Expr.Unary(UnaryOp.Not, q)
            Expr.Binary(BinaryOp.Add, n, i1), Expr.Binary(BinaryOp.Add, q, i1)
            Expr.IsNull(n, false), Expr.IsNull(q, false)
            Expr.Between(n, i1, i2, false), Expr.Between(q, i1, i2, false)
            Expr.InList(n, [ i1; i2 ], true), Expr.InList(q, [ i1; i2 ], true)
            Expr.Like(n, text, false, None), Expr.Like(q, text, false, None)
            Expr.Case([ n, i1; n, i2 ], Some i1), Expr.Case([ q, i1; q, i2 ], Some i1)
            Expr.Case([ (n, i1) ], None), Expr.Case([ (q, i1) ], None)
            Expr.FuncCall("ABS", [ n ]), Expr.FuncCall("ABS", [ q ])
            Expr.AggCall(AggFunc.Sum, false, n), Expr.AggCall(AggFunc.Sum, false, q)
            Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.Or, n, i1), Expr.IsNull(n, true)),
            Expr.Binary(BinaryOp.And, Expr.Binary(BinaryOp.Or, q, i1), Expr.IsNull(q, true))
        ]

    for input, expected in samples do
        Assert.Equal<string>(Expr.toDisplayString expected, Expr.toDisplayString (ok (Binder.qualify schema input)))

[<Fact>]
let ``Predicate.joinConjuncts: split した conjunct を AND で戻せる`` () =
    let e = Expr.Binary(BinaryOp.And, pCol "id", pCol "age")
    let parts = Predicate.splitConjuncts e
    let rejoined = Predicate.joinConjuncts parts

    Assert.True(rejoined |> Option.isSome)
    // 分割 → 再結合 で conjunct が 2 個のまま保たれる
    Assert.Equal<int>(2, Predicate.splitConjuncts (expectSome "rejoined" rejoined) |> List.length)

[<Fact>]
let ``Alias.substitute: 循環する別名でも終了する`` () =
    // `substitute` は置換後の式を再訪問するため、循環を残すと終わらない。
    // 呼び出し側の `dropCyclic` に頼らず、内部で冪等に落とす。
    let schema = Schema.ofTable "people" None peopleSchema
    let aliases = Map.ofList [ "a", Expr.Name "b"; "b", Expr.Name "a" ]

    match ok (Alias.substitute schema aliases (Expr.Name "a")) with
    | Expr.Name "a" -> ()
    | other -> failActual "循環する別名は置換されないはず" other
