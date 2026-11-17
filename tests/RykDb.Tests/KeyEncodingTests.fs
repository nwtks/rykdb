// ===========================================================================
// KeyEncodingTests — 保存キー符号化の golden vector（フォーマットの契約）。
//
// このファイルの期待バイト列は **SSTable に永続化されるキー**の形式そのもの。
// 変更すると既存 DB のキー順・接頭辞一致が壊れる
// （`Sstable` の Version はヘッダ用で、キー符号化の変更を検出しない）。
// 変更するときは architecture.md の符号化記述と、この表を同時に更新すること。
// ===========================================================================

module RykDb.KeyEncodingTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// "20 80 00" のような 16進表記をバイト列へ。
let private hex (text: string) =
    text.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun h -> System.Convert.ToByte(h, 16))

/// バイト列を 16進表記へ（失敗メッセージ用）。
let private toHex (bytes: byte array) =
    bytes |> Array.map (fun b -> b.ToString("X2")) |> String.concat " "

/// 期待バイト列と比べる。失敗時はラベルと両方の 16進を出す。
let private check (label: string) (expectedHex: string) (actual: byte array) =
    let expected = hex expectedHex
    let same = expected = actual
    Assert.True(same, sprintf "%s: 期待 %s / 実際 %s" label expectedHex (toHex actual))

let private keyOf (v: SqlValue) = KeyBuilder.ofValues [ v ] |> ok
let private keysOf (vs: SqlValue list) = KeyBuilder.ofValues vs |> ok

/// 同じ 16進トークンを count 個並べる。
let private repeated (token: string) count =
    List.replicate count token |> String.concat " "

let private digitBytes (digits: int list) =
    digits |> List.map (sprintf "%02X") |> String.concat " "

/// DECIMAL の 29 桁（System.Decimal.MaxValue の非スケール値）。
let private maxDigits =
    [
        7
        9
        2
        2
        8
        1
        6
        2
        5
        1
        4
        2
        6
        4
        3
        3
        7
        5
        9
        3
        5
        4
        3
        9
        5
        0
        3
        3
        5
    ]

let private negMaxDigits = maxDigits |> List.map (fun d -> 9 - d)

[<Fact>]
let ``キー符号化 golden: NULL / BOOLEAN / INTEGER`` () =
    check "NULL" "00 00" (keyOf SqlValue.Null)
    check "BOOLEAN false" "10 00 00" (keyOf (SqlValue.Bool false))
    check "BOOLEAN true" "10 01 00" (keyOf (SqlValue.Bool true))
    check "INTEGER MinValue" "20 00 00 00 00 00 00 00 00 00" (keyOf (SqlValue.Int System.Int64.MinValue))
    check "INTEGER -1" "20 7F FF FF FF FF FF FF FF 00" (keyOf (SqlValue.Int -1L))
    check "INTEGER 0" "20 80 00 00 00 00 00 00 00 00" (keyOf (SqlValue.Int 0L))
    check "INTEGER 1" "20 80 00 00 00 00 00 00 01 00" (keyOf (SqlValue.Int 1L))
    check "INTEGER MaxValue" "20 FF FF FF FF FF FF FF FF 00" (keyOf (SqlValue.Int System.Int64.MaxValue))

[<Fact>]
let ``キー符号化 golden: REAL（-0.0 と NaN の正規化を含む）`` () =
    let negativeZero = System.BitConverter.Int64BitsToDouble 0x8000000000000000L
    check "REAL -0.0" "21 80 00 00 00 00 00 00 00 00" (keyOf (SqlValue.vReal negativeZero))
    check "REAL 0.0" "21 80 00 00 00 00 00 00 00 00" (keyOf (SqlValue.vReal 0.0))
    check "REAL -1.0" "21 40 0F FF FF FF FF FF FF 00" (keyOf (SqlValue.Real -1.0))
    check "REAL 1.0" "21 BF F0 00 00 00 00 00 00 00" (keyOf (SqlValue.Real 1.0))
    check "REAL NaN" "21 FF F8 00 00 00 00 00 00 00" (keyOf (SqlValue.vReal System.Double.NaN))

[<Fact>]
let ``キー符号化 golden: DECIMAL（正規化と ±MaxValue）`` () =
    check "DECIMAL 0" (sprintf "22 01 00 00 00 00 00 00 00 00 %s 00" (repeated "00" 29)) (keyOf (SqlValue.Decimal 0M))

    check
        "DECIMAL -0.0"
        (sprintf "22 01 00 00 00 00 00 00 00 00 %s 00" (repeated "00" 29))
        (keyOf (SqlValue.Decimal -0.0M))

    // 1M / 1.0M / 1.00M は同じキー（末尾 0 の正規化）
    let oneKey = sprintf "22 01 80 00 00 00 00 00 00 01 01 %s 00" (repeated "00" 28)
    check "DECIMAL 1" oneKey (keyOf (SqlValue.Decimal 1M))
    check "DECIMAL 1.0" oneKey (keyOf (SqlValue.Decimal 1.0M))
    check "DECIMAL 1.00" oneKey (keyOf (SqlValue.Decimal 1.00M))

    // 100 は桁が落ちても指数が 3 になる（値の大きさは指数が表す）
    check
        "DECIMAL 100"
        (sprintf "22 01 80 00 00 00 00 00 00 03 01 %s 00" (repeated "00" 28))
        (keyOf (SqlValue.Decimal 100M))

    check
        "DECIMAL -1"
        (sprintf "22 00 7F FF FF FF FF FF FF FF 08 %s 00" (repeated "09" 28))
        (keyOf (SqlValue.Decimal -1M))

    check
        "DECIMAL 1e-28"
        (sprintf "22 01 7F FF FF FF FF FF FF E5 01 %s 00" (repeated "00" 28))
        (keyOf (SqlValue.Decimal 0.0000000000000000000000000001M))

    check
        "DECIMAL MaxValue"
        (sprintf "22 01 80 00 00 00 00 00 00 1D %s 00" (digitBytes maxDigits))
        (keyOf (SqlValue.Decimal System.Decimal.MaxValue))

    check
        "DECIMAL MinValue"
        (sprintf "22 00 7F FF FF FF FF FF FF E3 %s 00" (digitBytes negMaxDigits))
        (keyOf (SqlValue.Decimal System.Decimal.MinValue))

[<Fact>]
let ``キー符号化 golden: DATE / TIMESTAMP`` () =
    check "DATE day 0" "30 80 00 00 00 00 00 00 00 00" (keyOf (SqlValue.Date(System.DateOnly.FromDayNumber 0)))

    check
        "DATE 1970-01-01 (day 719162)"
        "30 80 00 00 00 00 0A F9 3A 00"
        (keyOf (SqlValue.Date(System.DateOnly.FromDateTime(System.DateTime(1970, 1, 1)))))

    check
        "TIMESTAMP ticks 0"
        "40 80 00 00 00 00 00 00 00 00"
        (keyOf (SqlValue.Timestamp(System.DateTimeOffset(0L, System.TimeSpan.Zero))))

    check
        "TIMESTAMP 2020-01-01"
        "40 88 D7 8E 4D 8B 7C 00 00 00"
        (keyOf (SqlValue.Timestamp(System.DateTimeOffset(2020, 1, 1, 0, 0, 0, System.TimeSpan.Zero))))

[<Fact>]
let ``キー符号化 golden: TEXT / BLOB の escape`` () =
    check "TEXT empty" "50 00" (keyOf (SqlValue.Text ""))
    check "TEXT a" "50 61 00" (keyOf (SqlValue.Text "a"))
    check "TEXT a NUL b" "50 61 00 FF 62 00" (keyOf (SqlValue.Text("a" + string (char 0) + "b")))
    check "TEXT é (UTF-8)" "50 C3 A9 00" (keyOf (SqlValue.Text "é"))
    check "TEXT emoji (UTF-8)" "50 F0 9F 98 80 00" (keyOf (SqlValue.Text "😀"))

    check "BLOB empty" "60 00" (keyOf (SqlValue.Blob [||]))
    check "BLOB 00" "60 00 FF 00" (keyOf (SqlValue.Blob [| 0uy |]))
    check "BLOB 01" "60 01 00" (keyOf (SqlValue.Blob [| 1uy |]))
    check "BLOB FF" "60 FF 00" (keyOf (SqlValue.Blob [| 0xFFuy |]))
    check "BLOB 00 00" "60 00 FF 00 FF 00" (keyOf (SqlValue.Blob [| 0uy; 0uy |]))

[<Fact>]
let ``キー符号化 golden: UUID / 複合キー`` () =
    check
        "UUID"
        "70 0F 8F AD 5B D9 CB 46 9F A1 65 70 86 77 28 95 0E 00"
        (keyOf (SqlValue.Uuid(System.Guid "0f8fad5b-d9cb-469f-a165-70867728950e")))

    check "複合キー INTEGER + TEXT" "20 80 00 00 00 00 00 00 01 00 50 61 00" (keysOf [ SqlValue.Int 1L; SqlValue.Text "a" ])

    let indexAndPrimary =
        KeyBuilder.ofIndexAndPrimary [ SqlValue.Int 20L ] [ SqlValue.Int 7L ] |> ok

    check "索引キー（索引列 + 主キー列）" "20 80 00 00 00 00 00 00 14 00 20 80 00 00 00 00 00 00 07 00" indexAndPrimary
