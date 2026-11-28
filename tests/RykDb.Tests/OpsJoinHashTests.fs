// ===========================================================================
// OpsJoinHash.fs のテスト（ハッシュ結合・退避・フォールバック）
// ===========================================================================

module RykDb.OpsJoinHashTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers
open RykDb.TestHelpers.OpsJoinFixtures

[<Fact>]
let ``結合: ハッシュ INNER JOIN`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let right = Op.ofArray [| [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoinHash leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(3L, asInt rows.[1].[0])

[<Fact>]
let ``結合: ハッシュ LEFT JOIN は一致が無ければ NULL`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 2L |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.leftJoinHash leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    Assert.Equal<int>(2, rows.Length)
    Assert.True(SqlValue.isNull rows.[0].[1])
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

[<Fact>]
let ``結合: ハッシュ結合は NULL キーを一致させない`` () =
    let left = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoinHash leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])

[<Fact>]
let ``結合: ハッシュ結合は NaN と -0.0 を SQL の比較どおりに一致させる`` () =
    // ハッシュのキーは順序保持符号化のバイト列。
    // `SqlValue` の構造的等価（NaN <> NaN）ではなく `compareValuesV`（NaN = NaN）に合わせる。
    let nan = System.Double.NaN
    let left = Op.ofArray [| [| SqlValue.Real nan |]; [| SqlValue.Real -0.0 |] |]
    let right = Op.ofArray [| [| SqlValue.Real nan |]; [| SqlValue.Real 0.0 |] |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoinHash leftSchema left [| 0 |] rightSchema right [| 0 |] None)))

    Assert.Equal<int>(2, rows.Length)

[<Fact>]
let ``結合: ハッシュ結合は残りの ON 句を評価する`` () =
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

    let left = Op.ofArray [| [| SqlValue.Int 1L; SqlValue.Int 10L |] |]

    let right =
        Op.ofArray
            [|
                [| SqlValue.Int 1L; SqlValue.Int 10L |]
                [| SqlValue.Int 1L; SqlValue.Int 20L |]
            |]

    let residual =
        Expr.Binary(BinaryOp.Gt, Expr.Column { Qualifier = Some "r"; Name = "b" }, Expr.Literal(Literal.Int 15L))

    // キー a だけでは 2 行候補。b > 15 で 1 行に絞る。
    let rows =
        ok (
            Op.toArray (
                ok (UnlimitedOpsJoin.innerJoinHash leftSchema2 left [| 0 |] rightSchema2 right [| 0 |] (Some residual))
            )
        )

    Assert.Equal<int>(1, rows.Length)
    Assert.Equal<int64>(20L, asInt rows.[0].[3])

[<Fact>]
let ``結合: 左 build のハッシュ INNER JOIN は右 build と同じ行を返す`` () =
    let baseline =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinHash
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

    let buildLeft =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinHashBuildLeft
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

    Assert.Equal<string>(rowsText baseline, rowsText buildLeft)

    // NULL キーはどちらの build 側でも一致しない。
    let leftWithNull = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]
    let rightWithNull = Op.ofArray [| [| SqlValue.Null |]; [| SqlValue.Int 2L |] |]

    let nullRows =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinHashBuildLeft
                        leftSchema
                        leftWithNull
                        [| 0 |]
                        rightSchema
                        rightWithNull
                        [| 0 |]
                        None
                )
            )
        )

    Assert.Equal<int>(1, nullRows.Length)
    Assert.Equal<int64>(2L, asInt nullRows.[0].[0])

[<Fact>]
let ``結合: 左 build のハッシュ INNER JOIN は左の退避時も結果が同じ`` () =
    withDir "join-hash-build-left-spill" (fun dir ->
        let baseline =
            ok (
                Op.toArray (
                    ok (
                        UnlimitedOpsJoin.innerJoinHash
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
                        OpsJoinHash.innerJoinHashBuildLeft
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
let ``結合: ハッシュ結合は右が上限を超えるとマージ結合へ切り替わり結果が同じ`` () =
    withDir "join-hash-spill" (fun dir ->
        let baseline =
            ok (
                Op.toArray (
                    ok (
                        UnlimitedOpsJoin.innerJoinHash
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
                        OpsJoinHash.innerJoinHash
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
let ``結合: 深度上限のフォールバックでも未整列のキーを取りこぼさない`` () =
    // パーティションはキーのハッシュで分けるのでキー順ではない。
    // 深さ上限でマージ結合へ落ちるときに整列していないと一致を取りこぼすため、
    // 「深さ上限までずっと同じパーティションに残る 2 キー」を総当たりで用意する。
    withDir "join-depth-fallback" (fun dir ->
        // 実装と同じ符号化（順序保持）でパーティション番号を求める。
        let keyBytes (i: int) =
            KeyBuilder.ofValues [| SqlValue.Int(int64 i) |] |> ok

        let count = 400
        let bytes = Array.init count keyBytes

        // 深さ 0..3 の再分割で使う salt は 1..4。
        let partitions =
            bytes
            |> Array.map (fun b -> [| for s in 1..4 -> Spill.partitionOfKey (uint64 s) b |])

        let mutates a b =
            Array.forall2 (=) partitions.[a] partitions.[b]

        let a, b =
            Seq.init count id
            |> Seq.collect (fun x -> Seq.init (count - 1 - x) (fun k -> x, x + 1 + k))
            |> Seq.find (fun (x, y) -> mutates x y)

        let left = Op.ofArray [| [| SqlValue.Int(int64 a) |]; [| SqlValue.Int(int64 b) |] |]

        // 右はキーの降順（未整列）。
        let right =
            Op.ofArray
                [|
                    [| SqlValue.Int(int64 b) |]
                    [| SqlValue.Int(int64 b) |]
                    [| SqlValue.Int(int64 a) |]
                    [| SqlValue.Int(int64 a) |]
                |]

        let inner =
            ok (
                Op.toArray (
                    ok (
                        OpsJoinHash.innerJoinHash
                            (tightBudget dir 1L)
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

        Assert.Equal<int>(4, inner.Length)

        // LEFT も同じ経路。一致する 2 行 + NULL 拡張 1 行。
        let left2 =
            Op.ofArray [| [| SqlValue.Int(int64 a) |]; [| SqlValue.Int(int64 b) |] |]

        let right2 =
            Op.ofArray [| [| SqlValue.Int(int64 b) |]; [| SqlValue.Int(int64 b) |] |]

        let leftRows =
            ok (
                Op.toArray (
                    ok (
                        OpsJoinHash.leftJoinHash
                            (tightBudget dir 1L)
                            leftSchema
                            left2
                            [| 0 |]
                            rightSchema
                            right2
                            [| 0 |]
                            None
                    )
                )
            )

        Assert.Equal<int>(3, leftRows.Length)
        Assert.True((leftRows |> Array.filter (fun r -> SqlValue.isNull r.[1])).Length = 1)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: ハッシュの LEFT JOIN も退避すると結果が同じ`` () =
    withDir "join-hash-left-spill" (fun dir ->
        let baseline =
            ok (
                Op.toArray (
                    ok (
                        UnlimitedOpsJoin.leftJoinHash
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
                        OpsJoinHash.leftJoinHash
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
let ``結合: 左が小さく右が退避しても左は run に書かない`` () =
    // 左が作業メモリに収まるなら、16本のパーティション run を作らずメモリで分割する。
    withDir "join-hash-left-memory" (fun dir ->
        let left =
            Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 1L |]; [| SqlValue.Int 9L |] |]

        let rightRows = [| for _ in 1..200 -> [| SqlValue.Int 1L |] |]

        let budget =
            { MemoryBudget.Unlimited with
                Work = 1024L
                Sort = 1024L
                Directory = Some dir
            }

        let rows =
            ok (
                Op.toArray (
                    ok (
                        OpsJoinHash.leftJoinHash
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

        // キー 1 の左 2行 × 右 200行 + 一致しない左行 1行。
        Assert.Equal<int>(401, rows.Length)

        let padded =
            rows |> Array.filter (fun r -> asInt r.[0] = 9L && SqlValue.isNull r.[1])

        Assert.Equal<int>(1, padded.Length)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: ハッシュ結合のパーティションは早期終了でも残らない`` () =
    withDir "join-hash-partition-scope" (fun dir ->
        let scope = ScanScope()

        let budget =
            { tightBudget dir 1L with
                Scope = Some scope
            }

        let op =
            ok (
                OpsJoinHash.innerJoinHash budget leftSchema (dupLeft ()) [| 0 |] rightSchema (dupRight ()) [| 0 |] None
            )

        match Op.next op with
        | OpStep.OpRow _ -> ()
        | other -> failActual "最初の行を取り出せません" other

        // Engine の早期終了（LIMIT / エラー）相当。
        scope.DisposeAll()
        assertNoLeftovers dir)

[<Fact>]
let ``結合: ハッシュ表のバケットは右入力の順序を保つ`` () =
    // 1段構築でもバケット内の順序が変わらないことを固定する。
    let rightSchema2 =
        outSchema
            [
                outCol "a" (Some "r") (Some DataType.Integer)
                outCol "b" (Some "r") (Some DataType.Integer)
            ]

    let left = Op.ofArray [| [| SqlValue.Int 1L |] |]

    let right =
        Op.ofArray
            [|
                [| SqlValue.Int 1L; SqlValue.Int 10L |]
                [| SqlValue.Int 1L; SqlValue.Int 20L |]
                [| SqlValue.Int 1L; SqlValue.Int 30L |]
            |]

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoinHash leftSchema left [| 0 |] rightSchema2 right [| 0 |] None)))

    Assert.Equal<int>(3, rows.Length)
    Assert.Equal<int64>(10L, asInt rows.[0].[2])
    Assert.Equal<int64>(20L, asInt rows.[1].[2])
    Assert.Equal<int64>(30L, asInt rows.[2].[2])
