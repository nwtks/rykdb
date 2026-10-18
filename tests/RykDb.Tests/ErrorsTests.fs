// ===========================================================================
// Errors.fs（DbError・ErrorKind・ResultOps・DbResultBuilder）
// ===========================================================================

module RykDb.ErrorsTests

open Xunit
open RykDb.Core

// ---------------------------------------------------------------------------
// DbError
// ---------------------------------------------------------------------------

[<Fact>]
let ``DbError.create: Kind と Message を持つ`` () =
    let e = DbError.create ErrorKind.NotFound "missing"
    Assert.Equal(ErrorKind.NotFound, e.Kind)
    Assert.Equal("missing", e.Message)
    Assert.True e.Position.IsNone
    Assert.True e.Detail.IsNone

[<Fact>]
let ``DbError.syntax: 位置を持つ`` () =
    let e = DbError.syntax "bad" 7
    Assert.Equal(ErrorKind.SyntaxError, e.Kind)
    Assert.Equal(Some 7, e.Position)

[<Fact>]
let ``DbError の各ファクトリが Kind を設定する`` () =
    Assert.Equal(ErrorKind.NameError, (DbError.name "x").Kind)
    Assert.Equal(ErrorKind.TypeError, (DbError.typeError "x").Kind)
    Assert.Equal(ErrorKind.ConstraintViolation, (DbError.constraintViolation "x").Kind)
    Assert.Equal(ErrorKind.NotFound, (DbError.notFound "x").Kind)
    Assert.Equal(ErrorKind.AlreadyExists, (DbError.alreadyExists "x").Kind)
    Assert.Equal(ErrorKind.CapacityError, (DbError.capacity "x").Kind)
    Assert.Equal(ErrorKind.CorruptionError, (DbError.corruption "x").Kind)
    Assert.Equal(ErrorKind.ConflictError, (DbError.conflict "x").Kind)
    Assert.Equal(ErrorKind.InternalError, (DbError.internalError "x").Kind)

[<Fact>]
let ``DbError.io: 例外を包む`` () =
    let ex = System.IO.IOException "boom"
    let e = DbError.io ex
    Assert.Equal(ErrorKind.IoError, e.Kind)
    Assert.Equal("boom", e.Message)
    Assert.Equal(Some "IOException", e.Detail)

[<Fact>]
let ``DbError.toDisplayString: 位置あり`` () =
    let e = DbError.syntax "bad" 7
    Assert.Equal("bad (位置 7行)", DbError.toDisplayString e)

[<Fact>]
let ``DbError.toDisplayString: Detail あり`` () =
    let e = DbError.io (System.IO.IOException "boom")
    Assert.Equal("boom [IOException]", DbError.toDisplayString e)

[<Fact>]
let ``DbError.toDisplayString: どちらもなし`` () =
    let e = DbError.create ErrorKind.NotFound "missing"
    Assert.Equal("missing", DbError.toDisplayString e)

// ---------------------------------------------------------------------------
// DbResultBuilder
// ---------------------------------------------------------------------------

[<Fact>]
let ``result 計算式: let! で伝播`` () =
    let r =
        result {
            let! a = ret 1
            let! b = ret 2
            return a + b
        }

    Assert.Equal(Ok 3, r)

[<Fact>]
let ``result 計算式: Error で早期脱出`` () =
    let r =
        result {
            let! a = err (DbError.notFound "x")
            return a + 1
        }

    match r with
    | Error e -> Assert.Equal(ErrorKind.NotFound, e.Kind)
    | Ok _ -> Assert.Fail "Error で脱出すべき"

// ---------------------------------------------------------------------------
// ResultOps
// ---------------------------------------------------------------------------

[<Fact>]
let ``ResultOps.>>= : Ok なら継続を呼ぶ`` () =
    let r = ret 1 >>= fun v -> ret (v + 1)
    Assert.Equal(Ok 2, r)

[<Fact>]
let ``ResultOps.>>= : Error なら伝播`` () =
    let r = err (DbError.notFound "x") >>= fun v -> ret (v + 1)

    match r with
    | Error e -> Assert.Equal(ErrorKind.NotFound, e.Kind)
    | Ok _ -> Assert.Fail "Error が伝播すべき"

[<Fact>]
let ``ResultOps.ret / err`` () =
    Assert.Equal(Ok 42, (ret 42: Result<int, DbError>))

    match err (DbError.notFound "x") with
    | Error e -> Assert.Equal(ErrorKind.NotFound, e.Kind)
    | Ok _ -> Assert.Fail "err は Error を返すべき"

[<Fact>]
let ``ResultOps.all: 全 Ok なら Ok`` () =
    let r = all [ ret 1; ret 2 ]

    match r with
    | Ok vs -> Assert.Equal<int list>([ 1; 2 ], vs)
    | Error _ -> Assert.Fail "all は Ok を返すべき"

[<Fact>]
let ``ResultOps.all: 1 つでも Error なら Error`` () =
    let r = all [ ret 1; err (DbError.notFound "x") ]

    match r with
    | Error e -> Assert.Equal(ErrorKind.NotFound, e.Kind)
    | Ok _ -> Assert.Fail "Error が伝播すべき"

[<Fact>]
let ``ResultOps.foldOk`` () =
    let r = foldOk (fun s (x: int) -> ret (s + x)) 0 [ 1; 2; 3 ]
    Assert.Equal(Ok 6, r)

[<Fact>]
let ``ResultOps.protect: IOException を IoError に`` () =
    let r = protect (fun () -> raise (System.IO.IOException "boom"))

    match r with
    | Error e -> Assert.Equal(ErrorKind.IoError, e.Kind)
    | Ok _ -> Assert.Fail "例外は Error に包まれるべき"

[<Fact>]
let ``ResultOps.protect: 正常値を返す`` () =
    Assert.Equal(Ok 42, protect (fun () -> 42))
