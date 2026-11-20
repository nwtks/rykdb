// ===========================================================================
// OpsJoin.fs のテスト: nested loop / ハッシュ / マージ / インデックスの結合
// ===========================================================================

module RykDb.OpsJoinTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// このモジュールのテストは上限なし（スピルしない）経路を検証する。
/// スピル経路は専用のバジェットで別途テストする。
module private UnlimitedOpsJoin =
    let innerJoin = OpsJoin.innerJoin MemoryBudget.Unlimited
    let leftJoin = OpsJoin.leftJoin MemoryBudget.Unlimited
    let innerJoinHash = OpsJoin.innerJoinHash MemoryBudget.Unlimited
    let leftJoinHash = OpsJoin.leftJoinHash MemoryBudget.Unlimited
    let innerJoinMerge = OpsJoin.innerJoinMerge MemoryBudget.Unlimited
    let leftJoinMerge = OpsJoin.leftJoinMerge MemoryBudget.Unlimited
    let innerJoinIndexed = OpsJoin.innerJoinIndexed
    let leftJoinIndexed = OpsJoin.leftJoinIndexed

let private leftSchema =
    outSchema [ outCol "lid" (Some "l") (Some DataType.Integer) ]

let private rightSchema =
    outSchema [ outCol "rid" (Some "r") (Some DataType.Integer) ]

let private lcol = Expr.Column { Qualifier = Some "l"; Name = "lid" }
let private rcol = Expr.Column { Qualifier = Some "r"; Name = "rid" }

/// 行の並びを順序非依存の文字列にする（結合アルゴリズムが変わっても比較できる）。
let private rowsText (rows: SqlValue array array) =
    rows
    |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
    |> Array.sort
    |> String.concat "|"

/// 重複キーを含む左右の入力（左 40行 / 右 24行）。
let private dupLeft () =
    Op.ofArray [| for i in 0..39 -> [| SqlValue.Int(int64 (i % 5)) |] |]

let private dupRight () =
    Op.ofArray [| for i in 0..23 -> [| SqlValue.Int(int64 (i % 8)) |] |]

let private onEq = Expr.Binary(BinaryOp.Eq, lcol, rcol)

[<Fact>]
let ``結合: INNER JOIN`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let right = Op.ofArray [| [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]
    let on = Expr.Binary(BinaryOp.Eq, lcol, rcol)

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoin leftSchema left rightSchema right on)))

    Assert.Equal<int>(2, rows.Length)
    Assert.Equal<int64>(2L, asInt rows.[0].[0])
    Assert.Equal<int64>(3L, asInt rows.[1].[0])

[<Fact>]
let ``結合: LEFT JOIN は一致が無ければ NULL`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 2L |] |]
    let on = Expr.Binary(BinaryOp.Eq, lcol, rcol)

    let rows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.leftJoin leftSchema left rightSchema right on)))

    Assert.Equal<int>(2, rows.Length)
    // 1 は一致が無いので右が NULL
    Assert.True(SqlValue.isNull rows.[0].[1])
    // 2 は一致
    Assert.Equal<int64>(2L, asInt rows.[1].[1])

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
let ``結合: インデックス入れ子ループ（INNER / LEFT）`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let lookup key =
        match key with
        | [| SqlValue.Int 2L |] -> Ok(Op.ofArray [| [| SqlValue.Int 2L |] |])
        | [| SqlValue.Int 3L |] -> Ok(Op.ofArray [| [| SqlValue.Int 3L |] |])
        | _ -> Ok Op.Empty

    let inner =
        ok (Op.toArray (UnlimitedOpsJoin.innerJoinIndexed leftSchema left [| 0 |] rightSchema None lookup))

    Assert.Equal<int>(2, inner.Length)
    Assert.Equal<int64>(2L, asInt inner.[0].[0])
    Assert.Equal<int64>(3L, asInt inner.[1].[0])

    let left2 = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    let leftRows =
        ok (Op.toArray (UnlimitedOpsJoin.leftJoinIndexed leftSchema left2 [| 0 |] rightSchema None lookup))

    Assert.Equal<int>(2, leftRows.Length)
    Assert.True(SqlValue.isNull leftRows.[0].[1])
    Assert.Equal<int64>(2L, asInt leftRows.[1].[1])

[<Fact>]
let ``結合: INNER JOIN の ON 評価失敗は Error になる`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 1L |] |]

    let div0 =
        Expr.Binary(BinaryOp.Div, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L))

    let on = Expr.Binary(BinaryOp.Eq, div0, Expr.Literal(Literal.Int 0L))

    // 黙って空結果にせず、エラーとして呼び出し元へ返す。
    expectError "ON の評価失敗が握り潰されています" (Op.toArray (ok (UnlimitedOpsJoin.innerJoin leftSchema left rightSchema right on)))

[<Fact>]
let ``結合: LEFT JOIN の ON 評価失敗も Error になる`` () =
    let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
    let right = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    // 一致した後にエラーへ当たっても、中途半端な行を返さずエラーにする。
    let div0 =
        Expr.Binary(BinaryOp.Div, Expr.Literal(Literal.Int 1L), Expr.Literal(Literal.Int 0L))

    let on =
        Expr.Binary(
            BinaryOp.Or,
            Expr.Binary(BinaryOp.Eq, lcol, rcol),
            Expr.Binary(BinaryOp.Eq, div0, Expr.Literal(Literal.Int 0L))
        )

    expectError "ON の評価失敗が握り潰されています" (Op.toArray (ok (UnlimitedOpsJoin.leftJoin leftSchema left rightSchema right on)))

[<Fact>]
let ``結合: 入れ子ループは右を退避しても結果が変わらない`` () =
    withDir "join-nested-spill" (fun dir ->
        let baseline =
            ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoin leftSchema (dupLeft ()) rightSchema (dupRight ()) onEq)))

        let spilled =
            ok (
                Op.toArray (
                    ok (OpsJoin.innerJoin (tightBudget dir 1L) leftSchema (dupLeft ()) rightSchema (dupRight ()) onEq)
                )
            )

        Assert.Equal<int>(120, baseline.Length)
        Assert.Equal<string>(rowsText baseline, rowsText spilled)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: 入れ子ループの LEFT JOIN も右を退避すると NULL 拡張が同じ`` () =
    withDir "join-nested-left-spill" (fun dir ->
        let baseline =
            ok (Op.toArray (ok (UnlimitedOpsJoin.leftJoin leftSchema (dupLeft ()) rightSchema (dupRight ()) onEq)))

        let spilled =
            ok (
                Op.toArray (
                    ok (OpsJoin.leftJoin (tightBudget dir 1L) leftSchema (dupLeft ()) rightSchema (dupRight ()) onEq)
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
                        OpsJoin.innerJoinHash
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
                        OpsJoin.innerJoinHash
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
                        OpsJoin.leftJoinHash
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
                        OpsJoin.leftJoinHash
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
                        OpsJoin.innerJoinMerge
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
                        OpsJoin.leftJoinMerge
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
            ok (OpsJoin.innerJoinMerge budget leftSchema left [| 0 |] rightSchema right [| 0 |] None)

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
                    OpsJoin.innerJoinMerge MemoryBudget.Unlimited leftSchema left [| 0 |] rightSchema right [| 0 |] None
                )
            )
        )

    Assert.Empty rows
    Assert.True(drained.Value, "左が先に尽きても右を読み切る必要があります")
