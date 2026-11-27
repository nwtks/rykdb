// ===========================================================================
// OpsJoinMerge.fs のテスト（マージ結合）
// ===========================================================================

module RykDb.OpsJoinMergeTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers
open RykDb.TestHelpers.OpsJoinFixtures

[<Fact>]
let ``結合: マージ INNER JOIN は重複キーを掛け合わせる`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    let right =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 1L |]; [| SqlValue.Int 3L |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoinMerge leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    // キー 1 の 2×2。キー 2 / 3 は一致しない。
    Assert.Equal<int>(4, rows.Length)
    Assert.True(rows |> Array.forall (fun r -> asInt r.[0] = 1L))

[<Fact>]
let ``結合: マージ LEFT JOIN は一致が無ければ NULL`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 2L |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.leftJoinMerge leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    Assert.Equal<int>(2, rows.Length)
    Assert.True(SqlValue.isNull rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

[<Fact>]
let ``結合: マージ結合は NULL キーを一致させない`` () =
    let left = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoinMerge leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])

[<Fact>]
let ``結合: マージ結合は複合キーと残りの ON 句を評価する`` () =
    let leftSchema2 =
        outSchema
            [
                outCol "a" (Some "l") (Some DataType.Integer)
                outCol "b" (Some "l") (Some DataType.Integer)
            ]

    let rightSchema2 =
        outSchema
            [
                outCol "a" (Some "r") (Some DataType.Integer)
                outCol "b" (Some "r") (Some DataType.Integer)
            ]

    // 複合キー (a, b)。両入力は a, b の昇順。
    let left =
        Op.ofArray
            [|
                [| SqlValue.Int 1L; SqlValue.Int 10L |]
                [| SqlValue.Int 1L; SqlValue.Int 20L |]
                [| SqlValue.Int 2L; SqlValue.Int 30L |]
            |]

    let right =
        Op.ofArray
            [|
                [| SqlValue.Int 1L; SqlValue.Int 20L |]
                [| SqlValue.Int 2L; SqlValue.Int 30L |]
                [| SqlValue.Int 2L; SqlValue.Int 31L |]
            |]

    let rows =
        ok (
            Op.toArray (
                ok (UnlimitedOpsJoin.innerJoinMerge leftSchema2 left [| 0; 1 |] rightSchema2 right [| 0; 1 |] None)
            )
        )

    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(20L, asInt rows.[0].[1])
    Assert.Equal<int64>(20L, asInt rows.[0].[3])
    Assert.Equal<int64>(30L, asInt rows.[1].[1])
    Assert.Equal<int64>(30L, asInt rows.[1].[3])

    // キー a だけ + residual（r.b > 15）は 2 左行 × 右グループ
    let residual =
        Expr.Binary(BinaryOp.Gt, Expr.Column { Qualifier = Some "r"; Name = "b" }, Expr.Literal(Literal.Int 15L))

    let rows2 =
        ok (
            Op.toArray (
                ok (UnlimitedOpsJoin.innerJoinMerge leftSchema2 left [| 0 |] rightSchema2 right [| 0 |] (Some residual))
            )
        )

    Assert.Equal<int>(4, rows2.Length)
    Assert.Equal<int64>(20L, asInt rows2.[0].[3])
    Assert.Equal<int64>(20L, asInt rows2.[1].[3])
    Assert.Equal<int64>(30L, asInt rows2.[2].[3])
    Assert.Equal<int64>(31L, asInt rows2.[3].[3])

[<Fact>]
let ``結合: マージ結合はハッシュ結合と同じ行を返す`` () =
    let rnd = System.Random(20261103)

    // 重複の多いキーを昇順に並べる（両実装とも左行順・右バケット順が昇順なので完全一致する）。
    let sortedKeys count =
        Array.init count (fun _ -> [| SqlValue.Int(int64 (rnd.Next(0, 10))) |])
        |> Array.sortBy (fun r ->
            match r.[0] with
            | SqlValue.Int i -> i
            | _ -> 0L)

    let assertSame isLeft =
        let left = Op.ofArray (sortedKeys 40)
        let right = Op.ofArray (sortedKeys 40)

        let merged =
            if isLeft then
                ok (
                    Op.toArray (
                        ok (UnlimitedOpsJoin.leftJoinMerge leftSchema left [| 0 |] rightSchema right [| 0 |] None)
                    )
                )
            else
                ok (
                    Op.toArray (
                        ok (UnlimitedOpsJoin.innerJoinMerge leftSchema left [| 0 |] rightSchema right [| 0 |] None)
                    )
                )

        let hashed =
            if isLeft then
                ok (
                    Op.toArray (
                        ok (UnlimitedOpsJoin.leftJoinHash leftSchema left [| 0 |] rightSchema right [| 0 |] None)
                    )
                )
            else
                ok (
                    Op.toArray (
                        ok (UnlimitedOpsJoin.innerJoinHash leftSchema left [| 0 |] rightSchema right [| 0 |] None)
                    )
                )

        Assert.Equal<int>(hashed.Length, merged.Length)

        for i in 0 .. merged.Length - 1 do
            Assert.Equal<SqlValue>(hashed.[i].[0], merged.[i].[0])
            Assert.Equal<SqlValue>(hashed.[i].[1], merged.[i].[1])

    assertSame false
    assertSame true

[<Fact>]
let ``結合: マージ LEFT JOIN は右が先に尽きても左を NULL 拡張する`` () =
    let left = Op.ofArray [| [| SqlValue.Int 0L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 1L |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.leftJoinMerge leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(0L, asInt rows.[0].[0])
    Assert.True(SqlValue.isNull rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[0])
    Assert.True(SqlValue.isNull rows.[1].[1])

[<Fact>]
let ``結合: マージ結合は同一キーのグループを退避しても結果が同じ`` () =
    withDir "join-merge-spill" (fun dir ->
        let baseline =
            ok (
                Op.toArray (
                    ok (
                        UnlimitedOpsJoin.innerJoinMerge
                            leftSchema
                            (dupLeft ())
                            [| 0 |]
                            rightSchema
                            (dupRight ())
                            [| 0 |]
                            None
                    )
                )
            )

        let spilled =
            ok (
                Op.toArray (
                    ok (
                        OpsJoinMerge.innerJoinMerge
                            (tightBudget dir 1L)
                            leftSchema
                            (dupLeft ())
                            [| 0 |]
                            rightSchema
                            (dupRight ())
                            [| 0 |]
                            None
                    )
                )
            )

        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: マージの LEFT JOIN もグループを退避すると NULL 拡張が同じ`` () =
    withDir "join-merge-left-spill" (fun dir ->
        let baseline =
            ok (
                Op.toArray (
                    ok (
                        UnlimitedOpsJoin.leftJoinMerge
                            leftSchema
                            (dupLeft ())
                            [| 0 |]
                            rightSchema
                            (dupRight ())
                            [| 0 |]
                            None
                    )
                )
            )

        let spilled =
            ok (
                Op.toArray (
                    ok (
                        OpsJoinMerge.leftJoinMerge
                            (tightBudget dir 1L)
                            leftSchema
                            (dupLeft ())
                            [| 0 |]
                            rightSchema
                            (dupRight ())
                            [| 0 |]
                            None
                    )
                )
            )

        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: マージ結合は左がメモリ・右が退避でも結果が同じ`` () =
    // 左がメモリに収まり右だけが退避するとき、右を行ごとに読み直さず 1回だけ流す。
    withDir "join-merge-right-spill" (fun dir ->
        let left =
            Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 1L |]; [| SqlValue.Int 9L |] |]

        let rightRows = [| for _ in 1..200 -> [| SqlValue.Int 1L |] |]

        let budget =
            { MemoryBudget.Unlimited with
                Work = 1024L
                Sort = 1024L
                Directory = Some dir
            }

        let inner =
            ok (
                Op.toArray (
                    ok (
                        OpsJoinMerge.innerJoinMerge
                            budget
                            leftSchema
                            left
                            [| 0 |]
                            rightSchema
                            (Op.ofArray rightRows)
                            [| 0 |]
                            None
                    )
                )
            )

        // キー 1 の左 2行 × 右 200行。キー 9 は一致しない。
        Assert.Equal<int>(400, inner.Length)
        Assert.True(inner |> Array.forall (fun r -> asInt r.[0] = 1L))

        // LEFT は一致しなかったキー 9 の左行を NULL 拡張する。
        let leftOnly =
            ok (
                Op.toArray (
                    ok (
                        OpsJoinMerge.leftJoinMerge
                            budget
                            leftSchema
                            left
                            [| 0 |]
                            rightSchema
                            (Op.ofArray rightRows)
                            [| 0 |]
                            None
                    )
                )
            )

        Assert.Equal<int>(401, leftOnly.Length)

        let padded =
            leftOnly |> Array.filter (fun r -> asInt r.[0] = 9L && SqlValue.isNull r.[1])

        Assert.Equal<int>(1, padded.Length)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: マージ結合のグループ退避は作成直後に ScanScope へ登録する`` () =
    withDir "join-merge-group-scope" (fun dir ->
        let scope = ScanScope()

        let budget =
            { tightBudget dir 1L with
                Scope = Some scope
            }

        let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 1L |] |]

        // 1行目だけ返してから Error になる右入力。
        // 右グループの取り出しで失敗するため、左グループの run はまだ開かれない。
        let right =
            let rec rowsFrom i =
                Op.compute (fun () ->
                    if i = 0 then
                        OpStep.OpRow([| SqlValue.Int 1L |], rowsFrom 1)
                    else
                        OpStep.OpError(DbError.internalError "right error"))

            rowsFrom 0

        let joined =
            ok (OpsJoinMerge.innerJoinMerge budget leftSchema left [| 0 |] rightSchema right [| 0 |] None)

        expectError "右入力のエラーが握り潰されています" (Op.toArray joined)

        // 消費側がエラーで止まったときの Engine の DisposeAll 相当。
        scope.DisposeAll()
        assertNoLeftovers dir)

[<Fact>]
let ``結合: マージ結合は左が先に尽きても右を読み切る（退避ファイルの後始末）`` () =
    let drained = ref false
    let rightRows = [| for i in 1..20 -> [| SqlValue.Int(int64 i) |] |]

    let right =
        let rec rowsFrom i =
            Op.compute (fun () ->
                if i >= rightRows.Length then
                    drained.Value <- true
                    OpStep.OpDone
                else
                    OpStep.OpRow(rightRows.[i], rowsFrom (i + 1)))

        rowsFrom 0

    // 左のキー 0 は右の全キーより小さいので、左が先に尽きる。
    let left = Op.ofArray [| [| SqlValue.Int 0L |] |]

    let rows =
        ok (
            Op.toArray (
                ok (
                    OpsJoinMerge.innerJoinMerge
                        MemoryBudget.Unlimited
                        leftSchema
                        left
                        [| 0 |]
                        rightSchema
                        right
                        [| 0 |]
                        None
                )
            )
        )

    Assert.Empty rows
    Assert.True(drained.Value, "左が先に尽きても右を読み切る必要があります")
