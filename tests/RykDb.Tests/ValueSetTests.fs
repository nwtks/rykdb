// ===========================================================================
// ValueSet.fs のテスト: IN サブクエリの値集合（メモリ / 退避）
// ===========================================================================

module RykDb.ValueSetTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// 作業メモリと退避先を指定したバジェット。
let private budgetIn (dir: string) (bytes: int64) =
    { MemoryBudget.Default with
        Work = bytes
        Sort = bytes
        Directory = Some dir
    }

/// 1列の Op を作る。
let private opOf (values: SqlValue list) =
    values |> List.map (fun v -> [| v |]) |> List.toArray |> Op.ofArray

let private build (budget: MemoryBudget) (values: SqlValue list) =
    ok (ValueSet.build budget (opOf values))

let private contains (set: ValueSet) (v: SqlValue) = okV (set.Contains v)

let private dispose (set: ValueSet) = (set :> System.IDisposable).Dispose()

[<Fact>]
let ``ValueSet: 空集合は FALSE、NULL だけの集合は UNKNOWN`` () =
    withDir "valueset-empty" (fun dir ->
        let budget = budgetIn dir (1024L * 1024L)

        let empty = build budget []
        Assert.Equal(TruthValue.False, contains empty (SqlValue.Int 1L))
        // 空集合では NULL も FALSE（既存の IN と同じ）。
        Assert.Equal(TruthValue.False, contains empty SqlValue.Null)

        let nulls = build budget [ SqlValue.Null ]
        Assert.Equal(TruthValue.Unknown, contains nulls SqlValue.Null)
        Assert.Equal(TruthValue.Unknown, contains nulls (SqlValue.Int 1L))

        dispose empty
        dispose nulls)

[<Fact>]
let ``ValueSet: 型をまたぐ数値の等値を SQL と同じに扱う`` () =
    withDir "valueset-numeric" (fun dir ->
        let budget = budgetIn dir (1024L * 1024L)

        let set =
            build budget [ SqlValue.Int 1L; SqlValue.Decimal 2.5M; SqlValue.vReal 3.0 ]

        // Int 1 と Decimal 1.0 は SQL では等しい。
        Assert.Equal(TruthValue.True, contains set (SqlValue.Int 1L))
        Assert.Equal(TruthValue.True, contains set (SqlValue.Decimal 1.0M))
        Assert.Equal(TruthValue.True, contains set (SqlValue.vReal 1.0))
        Assert.Equal(TruthValue.True, contains set (SqlValue.Decimal 2.5M))
        Assert.Equal(TruthValue.True, contains set (SqlValue.Int 3L))
        Assert.Equal(TruthValue.False, contains set (SqlValue.Int 4L))

        dispose set)

[<Fact>]
let ``ValueSet: 上限を超えて退避してもメモリと同じ判定になる`` () =
    withDir "valueset-spill" (fun dir ->
        let values = [ for i in 0..499 -> SqlValue.Int(int64 i) ] @ [ SqlValue.Null ]
        let memory = build (budgetIn dir (1024L * 1024L)) values
        let spilled = build (budgetIn dir 1L) values
        let probes = [ for i in -5 .. 505 -> SqlValue.Int(int64 i) ] @ [ SqlValue.Null ]

        for p in probes do
            Assert.Equal(contains memory p, contains spilled p)

        // 退避したファイルは Dispose で消える。
        Assert.NotEmpty(System.IO.Directory.GetFiles dir)
        dispose spilled
        Assert.Empty(System.IO.Directory.GetFiles dir)

        dispose memory)

[<Fact>]
let ``ValueSet: 退避しても型・スケール違いの等値を取りこぼさない`` () =
    // Bloom のキーを符号化バイト 1 つに固定すると、`Int 1` と `Decimal 1.0` のような
    // SQL では等しい値で偽陰性になり得る（退避時だけ 0 行になる静かな誤答）。
    withDir "valueset-spill-numeric" (fun dir ->
        let values =
            [
                SqlValue.Decimal 1.0M
                SqlValue.Decimal 2.50M
                SqlValue.Int 3L
                SqlValue.vReal 4.0
            ]

        let spilled = build (budgetIn dir 1L) values

        Assert.Equal(TruthValue.True, contains spilled (SqlValue.Int 1L))
        Assert.Equal(TruthValue.True, contains spilled (SqlValue.vReal 1.0))
        Assert.Equal(TruthValue.True, contains spilled (SqlValue.Decimal 1.00M))
        Assert.Equal(TruthValue.True, contains spilled (SqlValue.Decimal 2.5M))
        Assert.Equal(TruthValue.True, contains spilled (SqlValue.Decimal 3.0M))
        Assert.Equal(TruthValue.True, contains spilled (SqlValue.Int 4L))
        Assert.Equal(TruthValue.False, contains spilled (SqlValue.Int 5L))
        dispose spilled)

[<Fact>]
let ``ValueSet: 退避しても DATE と同日 00:00 UTC の TIMESTAMP を等値に扱う`` () =
    withDir "valueset-spill-temporal" (fun dir ->
        let spilled = build (budgetIn dir 1L) [ SqlValue.Date(System.DateOnly(2024, 1, 2)) ]

        let midnight =
            System.DateTimeOffset(System.DateTime(2024, 1, 2, 0, 0, 0), System.TimeSpan.Zero)

        Assert.Equal(TruthValue.True, contains spilled (SqlValue.Timestamp midnight))

        // 0 時以外は別の値。
        let noon =
            System.DateTimeOffset(System.DateTime(2024, 1, 2, 12, 0, 0), System.TimeSpan.Zero)

        Assert.Equal(TruthValue.False, contains spilled (SqlValue.Timestamp noon))
        dispose spilled)

[<Fact>]
let ``ValueSet: 退避した集合は ScanScope の早期終了でもファイルが残らない`` () =
    withDir "valueset-spill-scope" (fun dir ->
        let scope = ScanScope()

        let budget =
            { budgetIn dir 1L with
                Scope = Some scope
            }

        let set = build budget [ for i in 0..199 -> SqlValue.Text(sprintf "v%03d" i) ]
        Assert.NotEmpty(System.IO.Directory.GetFiles dir)
        Assert.Equal(TruthValue.True, contains set (SqlValue.Text "v123"))
        Assert.Equal(TruthValue.False, contains set (SqlValue.Text "nope"))

        // 明示的に Dispose しなくても台帳の破棄で消える。
        scope.DisposeAll()
        Assert.Empty(System.IO.Directory.GetFiles dir)
        dispose set)

[<Fact>]
let ``ValueSet: Dispose は ScanScope の登録を外す`` () =
    // 相関 IN のように外側行ごとに集合を作る用途で、索引と Bloom の参照が
    // クエリ終了まで残らないことを確認する。
    withDir "valueset-scope-forget" (fun dir ->
        let scope = ScanScope()

        let budget =
            { budgetIn dir 1L with
                Scope = Some scope
            }

        let set = build budget [ for i in 0..199 -> SqlValue.Text(sprintf "v%03d" i) ]
        Assert.Equal<int>(1, scope.ActiveCount)

        dispose set
        Assert.Equal<int>(0, scope.ActiveCount))

[<Fact>]
let ``ValueSet: 比較できない型が混ざると TypeError`` () =
    let mixed = [ SqlValue.Int 1L; SqlValue.Text "x" ]
    expectErrorKind ErrorKind.TypeError "比較できない型の集合が OK になっています" (ValueSet.build MemoryBudget.Unlimited (opOf mixed))

[<Fact>]
let ``ValueSet: 退避した集合でも比較できない型は TypeError`` () =
    withDir "valueset-spill-type" (fun dir ->
        let mixed = [ for i in 0..99 -> SqlValue.Int(int64 i) ] @ [ SqlValue.Text "x" ]
        expectErrorKind ErrorKind.TypeError "比較できない型の集合が OK になっています" (ValueSet.build (budgetIn dir 1L) (opOf mixed))
        Assert.Empty(System.IO.Directory.GetFiles dir))
