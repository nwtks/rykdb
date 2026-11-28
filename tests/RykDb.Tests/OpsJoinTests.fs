// ===========================================================================
// OpsJoin.fs のテスト（入れ子ループ・インデックス・アルゴリズム間の同一性）
// ===========================================================================

module RykDb.OpsJoinTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers
open RykDb.TestHelpers.OpsJoinFixtures

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
let ``結合: インデックス入れ子ループ（INNER / LEFT）`` () =
    let left =
        Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |]; [| SqlValue.Int 3L |] |]

    let lookup key =
        match key with
        | [| SqlValue.Int 2L |] -> Ok(Op.ofArray [| [| SqlValue.Int 2L |] |])
        | [| SqlValue.Int 3L |] -> Ok(Op.ofArray [| [| SqlValue.Int 3L |] |])
        | _ -> Ok Op.Empty

    let inner =
        ok (Op.toArray (ok (UnlimitedOpsJoin.innerJoinIndexed leftSchema left [| 0 |] rightSchema None lookup)))

    Assert.Equal<int>(2, inner.Length)
    Assert.Equal<int64>(2L, asInt inner.[0].[0])
    Assert.Equal<int64>(3L, asInt inner.[1].[0])

    let left2 = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]

    let leftRows =
        ok (Op.toArray (ok (UnlimitedOpsJoin.leftJoinIndexed leftSchema left2 [| 0 |] rightSchema None lookup)))

    Assert.Equal<int>(2, leftRows.Length)
    Assert.True(SqlValue.isNull leftRows.[0].[1])
    Assert.Equal<int64>(2L, asInt leftRows.[1].[1])

[<Fact>]
let ``結合: 4つのアルゴリズムは INNER で同じ行を返す`` () =
    let leftRows = diffRows 20261121 80
    let rightRows = diffRows 20261122 60
    let keys = [| 0; 1 |]
    let on = Expr.Binary(BinaryOp.And, diffKeyEq, diffResidual)

    let baseline =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoin
                        diffLeftSchema
                        (Op.ofArray leftRows)
                        diffRightSchema
                        (Op.ofArray rightRows)
                        on
                )
            )
        )

    let hashed =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinHash
                        diffLeftSchema
                        (Op.ofArray leftRows)
                        keys
                        diffRightSchema
                        (Op.ofArray rightRows)
                        keys
                        (Some diffResidual)
                )
            )
        )

    let sortedRows (rows: SqlValue array array) = rows |> Array.sortWith compareDiffKeys

    let merged =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinMerge
                        diffLeftSchema
                        (Op.ofArray (sortedRows leftRows))
                        keys
                        diffRightSchema
                        (Op.ofArray (sortedRows rightRows))
                        keys
                        (Some diffResidual)
                )
            )
        )

    let indexed =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinIndexed
                        diffLeftSchema
                        (Op.ofArray leftRows)
                        keys
                        diffRightSchema
                        (Some diffResidual)
                        (diffLookup rightRows keys)
                )
            )
        )

    assertSameRows "HashJoin" baseline hashed
    assertSameRows "MergeJoin" baseline merged
    assertSameRows "IndexNestedLoop" baseline indexed

[<Fact>]
let ``結合: 4つのアルゴリズムは LEFT で同じ行を返す`` () =
    let leftRows = diffRows 20261123 70
    let rightRows = diffRows 20261124 50
    let keys = [| 0; 1 |]

    let baseline =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.leftJoin
                        diffLeftSchema
                        (Op.ofArray leftRows)
                        diffRightSchema
                        (Op.ofArray rightRows)
                        diffKeyEq
                )
            )
        )

    let hashed =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.leftJoinHash
                        diffLeftSchema
                        (Op.ofArray leftRows)
                        keys
                        diffRightSchema
                        (Op.ofArray rightRows)
                        keys
                        None
                )
            )
        )

    let sortedRows (rows: SqlValue array array) = rows |> Array.sortWith compareDiffKeys

    let merged =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.leftJoinMerge
                        diffLeftSchema
                        (Op.ofArray (sortedRows leftRows))
                        keys
                        diffRightSchema
                        (Op.ofArray (sortedRows rightRows))
                        keys
                        None
                )
            )
        )

    let indexed =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.leftJoinIndexed
                        diffLeftSchema
                        (Op.ofArray leftRows)
                        keys
                        diffRightSchema
                        None
                        (diffLookup rightRows keys)
                )
            )
        )

    assertSameRows "HashJoin" baseline hashed
    assertSameRows "MergeJoin" baseline merged
    assertSameRows "IndexNestedLoop" baseline indexed

[<Fact>]
let ``結合: 4つのアルゴリズムは NaN と -0.0 を同じ扱いにする`` () =
    let nan = System.Double.NaN

    let leftRows =
        [| [| SqlValue.Real nan |]; [| SqlValue.Real -0.0 |]; [| SqlValue.Real 0.0 |] |]

    let rightRows = [| [| SqlValue.Real nan |]; [| SqlValue.Real 0.0 |] |]
    let keys = [| 0 |]

    let baseline =
        ok (
            Op.toArray (
                ok (UnlimitedOpsJoin.innerJoin leftSchema (Op.ofArray leftRows) rightSchema (Op.ofArray rightRows) onEq)
            )
        )

    let hashed =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinHash
                        leftSchema
                        (Op.ofArray leftRows)
                        keys
                        rightSchema
                        (Op.ofArray rightRows)
                        keys
                        None
                )
            )
        )

    let compareReal (a: SqlValue array) (b: SqlValue array) =
        match SqlValue.compareValuesV a.[0] b.[0] with
        | DbOk c -> c
        | DbErr _ -> 0

    let merged =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinMerge
                        leftSchema
                        (Op.ofArray (Array.sortWith compareReal leftRows))
                        keys
                        rightSchema
                        (Op.ofArray (Array.sortWith compareReal rightRows))
                        keys
                        None
                )
            )
        )

    let indexed =
        ok (
            Op.toArray (
                ok (
                    UnlimitedOpsJoin.innerJoinIndexed
                        leftSchema
                        (Op.ofArray leftRows)
                        keys
                        rightSchema
                        None
                        (diffLookup rightRows keys)
                )
            )
        )

    Assert.Equal<int>(3, baseline.Length)
    assertSameRows "HashJoin" baseline hashed
    assertSameRows "MergeJoin" baseline merged
    assertSameRows "IndexNestedLoop" baseline indexed

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
let ``結合: 退避入れ子ループは右行優先でブロックを流す`` () =
    // ブロック化は 1右行に対してブロック内の左を回すため、出力順は右行優先になる。
    // 旧実装（左行ごとに右を読み直す）なら (1,1),(2,2) なので、順序でブロック化を固定する。
    // SQL の ORDER BY が無いときの行順は仕様ではないが、演算子のストリーム順は回帰で見る。
    withDir "join-nested-block-order" (fun dir ->
        let left = Op.ofArray [| [| SqlValue.Int 1L |]; [| SqlValue.Int 2L |] |]
        let right = Op.ofArray [| [| SqlValue.Int 2L |]; [| SqlValue.Int 1L |] |]

        let rows =
            ok (Op.toArray (ok (OpsJoin.innerJoin (tightBudget dir 1L) leftSchema left rightSchema right onEq)))

        let text =
            rows
            |> Array.map (fun r -> r |> Array.map SqlLiteral.toSqlLiteral |> String.concat ",")
            |> String.concat "|"

        Assert.Equal("2,2|1,1", text)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: 退避入れ子ループは複数ブロックでも全行を返す`` () =
    // 左 130行はブロック境界（64行）をまたぐ。右は 1回だけ流すので一致は 130行。
    withDir "join-nested-blocks" (fun dir ->
        let left = Op.ofArray [| for i in 0..129 -> [| SqlValue.Int(int64 (i % 10)) |] |]
        let right = Op.ofArray [| for i in 0..9 -> [| SqlValue.Int(int64 i) |] |]

        let rows =
            ok (Op.toArray (ok (OpsJoin.innerJoin (tightBudget dir 1L) leftSchema left rightSchema right onEq)))

        Assert.Equal<int>(130, rows.Length)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: 退避入れ子ループの LEFT JOIN はブロックごとに未一致行をNULL拡張する`` () =
    withDir "join-nested-blocks-left" (fun dir ->
        let left = Op.ofArray [| for i in 0..129 -> [| SqlValue.Int(int64 (i % 10)) |] |]
        let right = Op.ofArray [| [| SqlValue.Int 0L |] |]

        let rows =
            ok (Op.toArray (ok (OpsJoin.leftJoin (tightBudget dir 1L) leftSchema left rightSchema right onEq)))

        Assert.Equal<int>(130, rows.Length)
        // 値 0 の 13行だけが一致し、残り 117行は右が NULL。
        Assert.Equal<int>(13, rows |> Array.filter (fun r -> not (SqlValue.isNull r.[1])) |> Array.length)
        Assert.Equal<int>(117, rows |> Array.filter (fun r -> SqlValue.isNull r.[1]) |> Array.length)
        assertNoLeftovers dir)

[<Fact>]
let ``結合: ON と residual の未知の列は空の入力でも構築時に Error`` () =
    // 入力が 0 行でも ON 句は一度も評価されない。
    // 列解決は演算子の構築時に検査し、行数に依存せず NameError にする。
    let unknown = Expr.Column { Qualifier = None; Name = "nope" }

    expectErrorKind
        ErrorKind.NameError
        "nested loop の ON が OK になっています"
        (UnlimitedOpsJoin.innerJoin leftSchema Op.Empty rightSchema Op.Empty unknown)

    expectErrorKind
        ErrorKind.NameError
        "ハッシュ結合の residual が OK になっています"
        (UnlimitedOpsJoin.innerJoinHash leftSchema Op.Empty [| 0 |] rightSchema Op.Empty [| 0 |] (Some unknown))

    expectErrorKind
        ErrorKind.NameError
        "インデックス入れ子ループの residual が OK になっています"
        (UnlimitedOpsJoin.innerJoinIndexed leftSchema Op.Empty [| 0 |] rightSchema (Some unknown) (fun _ -> Ok Op.Empty))
