// ===========================================================================
// VersionSet.fs（レベル構成・VersionEdit）
// ===========================================================================

module RykDb.VersionSetTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private sstKey (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]

let private fmd (num: int64) (level: int) (lo: int) (hi: int) =
    {
        FileNumber = num
        Level = level
        Smallest = sstKey lo
        Largest = sstKey hi
        EntryCount = int64 (hi - lo + 1)
        Size = int64 (hi - lo + 1) * 100L
        MaxSeqNo = uint64 (hi + 1)
    }

let private compose (a: VersionEdit) (b: VersionEdit) =
    {
        AddedFiles = a.AddedFiles @ b.AddedFiles
        DeletedFiles = a.DeletedFiles @ b.DeletedFiles
    }

[<Fact>]
let ``VersionSet: ファイル追加でレベル構成が変わる`` () =
    let v =
        Version.empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 1 100 199)
        |> Version.addFile (fmd 3L 2 0 99)

    Assert.Equal<int array>([| 1; 1; 1; 0; 0; 0; 0 |], Version.levelCounts v)

[<Fact>]
let ``VersionSet: 追加は破壊的でない`` () =
    let v0 = Version.empty
    let v1 = Version.addFile (fmd 1L 0 100 199) v0

    Assert.Equal(0, (Version.levelCounts v0).[0])
    Assert.Equal(1, (Version.levelCounts v1).[0])
    // v0 に後から v1 のファイルは見えない
    Assert.Empty(Version.filesAt v0 0)
    Assert.Equal(1, (Version.filesAt v1 0).Length)

[<Fact>]
let ``VersionSet: 削除でレベル構成が変わる`` () =
    let v =
        Version.empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 1 100 199)
        |> Version.deleteFiles [ fmd 1L 0 100 199 ]

    Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], Version.levelCounts v)

[<Fact>]
let ``VersionSet: 点ルックアップの候補は新しい順`` () =
    let v =
        Version.empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 0 150 249)
        |> Version.addFile (fmd 3L 1 100 199)

    // キー 180 は L0 の 2 ファイルと L1 の 1 ファイルの範囲内。
    // 全レベルを「レベル昇順・番号降順」で読む（L1 までしか読まないと取りこぼす）。
    let candidates =
        Version.filesForPointLookup v (sstKey 180) |> List.map (fun f -> f.FileNumber)

    Assert.Equal<int64 list>([ 2L; 1L; 3L ], candidates)

    // キー 210 は L0 のファイル 1（100..199）と L1（100..199）の範囲外
    let only2 =
        Version.filesForPointLookup v (sstKey 210) |> List.map (fun f -> f.FileNumber)

    Assert.Equal<int64 list>([ 2L ], only2)

    // L1 だけにあるキーでも L0 を読み飛ばして見つかる
    let l1Only =
        Version.empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 3L 1 300 399)
        |> fun v2 -> Version.filesForPointLookup v2 (sstKey 350) |> List.map (fun f -> f.FileNumber)

    Assert.Equal<int64 list>([ 3L ], l1Only)

    // 範囲外のキーは候補なし
    Assert.Empty(Version.filesForPointLookup v (sstKey 9999))

[<Fact>]
let ``VersionSet: コンパクション候補を選ぶ`` () =
    // L1 が閾値を超えるので L1 → L2 の候補が出る
    let thresholds = [| 10L; 1000L; 1000000L; 1000000L; 1000000L; 1000000L; 1000000L |]

    let v = Version.empty |> Version.addFile (fmd 1L 1 0 999) // size = 100000 > 1000

    match Version.pickCompaction v thresholds with
    | Some plan ->
        Assert.Equal(1, plan.Level)
        Assert.Equal(2, plan.OutputLevel)
        Assert.Equal(1, plan.InputFiles.Length)
    | None -> Assert.Fail("L1 が閾値を超えるので候補があるはず")

    // すべて閾値未満なら候補なし
    let big =
        [|
            10000000L
            10000000L
            10000000L
            10000000L
            10000000L
            10000000L
            10000000L
        |]

    Assert.True((Version.pickCompaction v big).IsNone)

[<Fact>]
let ``VersionEdit: エンコードの往復`` () =
    let added = fmd 3L 1 300 350
    let deleted = fmd 1L 0 100 199

    let edit = compose (VersionEdit.addFile added) (VersionEdit.deleteFile deleted)

    match ok (VersionEdit.decode (VersionEdit.encode edit)) with
    | [ e ] ->
        Assert.Equal(1, e.AddedFiles.Length)
        Assert.Equal(1, e.DeletedFiles.Length)

        let a = e.AddedFiles.[0]
        Assert.Equal(added.FileNumber, a.FileNumber)
        Assert.Equal(added.Level, a.Level)
        Assert.Equal(added.EntryCount, a.EntryCount)
        Assert.Equal(added.Size, a.Size)

        Assert.Equal<string>(
            System.Text.Encoding.UTF8.GetString added.Smallest,
            System.Text.Encoding.UTF8.GetString a.Smallest
        )

        Assert.Equal<string>(
            System.Text.Encoding.UTF8.GetString added.Largest,
            System.Text.Encoding.UTF8.GetString a.Largest
        )

        Assert.Equal(deleted.FileNumber, e.DeletedFiles.[0].FileNumber)
        Assert.Equal(deleted.Level, e.DeletedFiles.[0].Level)
    | _ -> Assert.Fail("1 レコードに復元されるはず")

// ---------------------------------------------------------------------------
// 末尾再帰の検証（深い入力でスタックを食わないこと）
// ---------------------------------------------------------------------------

[<Fact>]
let ``末尾再帰: VersionEdit の巨大レコードを読める`` () =
    // 1 レコードに 20 万個のファイル追加を入れる
    let n = 200_000

    let added = List.init n (fun i -> fmd (int64 i) 0 (i * 2) (i * 2 + 1))

    let edit =
        compose
            (VersionEdit.addFile added.[0])
            {
                AddedFiles = added.[1..]
                DeletedFiles = []
            }

    match ok (VersionEdit.decode (VersionEdit.encode edit)) with
    | [ e ] -> Assert.Equal(n, e.AddedFiles.Length)
    | _ -> Assert.Fail("1 レコードに復元されるはず")
