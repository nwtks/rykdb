// ===========================================================================
// テスト自体の健全性（乱数生成器の退行・FS0842）
// ===========================================================================

module RykDb.TestHealthTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

[<Fact>]
let ``テストデータ: 乱数生成器が実際に多様な値を返す`` () =
    let samples = Array.init 200 (fun _ -> randomValue ())

    let distinct =
        samples
        |> Array.map (fun v -> (v |> SqlValue.dataType |> DataType.toSqlName) + ":" + SqlLiteral.toSqlLiteral v)
        |> Set.ofArray

    Assert.True(distinct.Count > 20, sprintf "乱数データが退行しています（相異なる値が %d 種類しかない）" distinct.Count)

    // 10 種すべての型が生成されていること（偏った生成器を検出する）
    let typesCovered =
        samples
        |> Array.map (fun v -> v |> SqlValue.dataType |> DataType.toSqlName)
        |> Set.ofArray

    for t in
        [
            "NULL"
            "BOOLEAN"
            "INTEGER"
            "REAL"
            "DECIMAL"
            "TEXT"
            "BLOB"
            "DATE"
            "TIMESTAMP"
            "UUID"
        ] do
        Assert.True(Set.contains t typesCovered, sprintf "型 %s が生成されていません" t)

[<Fact>]
let ``テストデータ: 文字列に NUL や多バイト文字が含まれる`` () =
    // escape 経路を通すため、NUL や制御文字、マルチバイト文字が必ず混ざっていること
    let texts = Array.init 300 (fun _ -> randomText ())
    let hasNul = texts |> Array.exists (fun s -> s.Contains(char 0))

    let hasMultiByte =
        texts |> Array.exists (fun s -> s |> Seq.exists (fun c -> int c > 0x7F))

    Assert.True(hasNul, "NUL を含む文字列が生成されていません（escape 経路が未検証）")
    Assert.True(hasMultiByte, "多バイト文字を含む文字列が生成されていません")

// ---------------------------------------------------------------------------
// 「テストが 1 つも走っていない」検出（FS0842 対策）
// ---------------------------------------------------------------------------

/// このテストファイルが読み込まれていることの目印。
[<System.Runtime.CompilerServices.CompilerGenerated>]
type TestsMarker = class end

/// Assembly に残るテスト名を確認するためのマーカー。
type ProgramMarker = class end

[<Fact>]
let ``テスト: 各テストモジュールが 1 つ以上の Fact を持つ`` () =
    // FS0842（[<Fact>] が関数でない let 束縛に付いて黙って無視される）を検知する。
    // モジュール単位で Fact が 0 件なら「全部緑だが 1 件も走っていない」。
    let asm = typeof<TestsMarker>.Assembly

    let facts =
        asm.GetTypes()
        |> Array.collect (fun t -> t.GetMethods())
        |> Array.filter (fun m -> m.GetCustomAttributes(typeof<FactAttribute>, false).Length > 0)

    let namesOf (moduleName: string) =
        facts
        |> Array.filter (fun m -> m.DeclaringType.FullName = moduleName)
        |> Array.map (fun m -> m.Name)

    let modules =
        asm.GetTypes()
        |> Array.choose (fun t ->
            match t.FullName with
            | n when n.StartsWith "RykDb." && n.EndsWith "Tests" -> Some n
            | _ -> None)
        |> Array.sort

    Assert.True(modules.Length >= 21, sprintf "テストモジュールが %d 個しか見つかりません" modules.Length)

    for moduleName in modules do
        let count = (namesOf moduleName).Length
        Assert.True(count >= 1, sprintf "%s の Fact が 0 件（FS0842 の疑い）" moduleName)

    // テスト名は Assembly に残るので、カテゴリ名が消えていないか確認する。
    Assert.True(
        (namesOf "RykDb.DatabaseTests") |> Array.exists (fun n -> n.Contains "E2E"),
        "DatabaseTests に E2E のテストが見つかりません"
    )

    Assert.True(
        (namesOf "RykDb.DatabaseTests") |> Array.exists (fun n -> n.Contains "性能"),
        "DatabaseTests に性能のテストが見つかりません"
    )

    Assert.True(
        (namesOf "RykDb.ServerTests") |> Array.exists (fun n -> n.Contains "TCP"),
        "ServerTests に TCP のテストが見つかりません"
    )
