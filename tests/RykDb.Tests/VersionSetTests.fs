// ===========================================================================
// VersionSet.fs（レベル構成・VersionEdit）
// ===========================================================================

module RykDb.VersionSetTests

open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private compose (a: VersionEdit) (b: VersionEdit) =
    {
        AddedFiles = a.AddedFiles @ b.AddedFiles
        DeletedFiles = a.DeletedFiles @ b.DeletedFiles
    }

[<Fact>]
let ``Version: ファイル追加でレベル構成が変わる`` () =
    let v =
        Version.Empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 1 100 199)
        |> Version.addFile (fmd 3L 2 0 99)

    Assert.Equal<int array>([| 1; 1; 1; 0; 0; 0; 0 |], Version.levelCounts v)

[<Fact>]
let ``Version: 追加は破壊的でない`` () =
    let v0 = Version.Empty
    let v1 = Version.addFile (fmd 1L 0 100 199) v0
    Assert.Equal(0, (Version.levelCounts v0).[0])
    Assert.Equal(1, (Version.levelCounts v1).[0])
    // v0 に後から v1 のファイルは見えない
    Assert.Empty(Version.filesAt 0 v0)
    Assert.Equal(1, (Version.filesAt 0 v1).Length)

[<Fact>]
let ``Version: 削除でレベル構成が変わる`` () =
    let v =
        Version.Empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 1 100 199)
        |> Version.deleteFiles [ fmd 1L 0 100 199 ]

    Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], Version.levelCounts v)

[<Fact>]
let ``Version: ポイントルックアップの候補は新しい順`` () =
    let v =
        Version.Empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 0 150 249)
        |> Version.addFile (fmd 3L 1 100 199)

    // キー 180 は L0 の 2 ファイルと L1 の 1 ファイルの範囲内。
    // 全レベルを「レベル昇順・番号降順」で読む（L1 までしか読まないと取りこぼす）。
    let candidates =
        Version.filesForPointLookup (intKey 180) v |> List.map (fun f -> f.FileNumber)

    Assert.Equal<int64 list>([ 2L; 1L; 3L ], candidates)

    // キー 210 は L0 のファイル 1（100..199）と L1（100..199）の範囲外
    let only2 =
        Version.filesForPointLookup (intKey 210) v |> List.map (fun f -> f.FileNumber)

    Assert.Equal<int64 list>([ 2L ], only2)

    // L1 だけにあるキーでも L0 を読み飛ばして見つかる
    let l1Only =
        Version.Empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 3L 1 300 399)
        |> fun v2 -> Version.filesForPointLookup (intKey 350) v2 |> List.map (fun f -> f.FileNumber)

    Assert.Equal<int64 list>([ 3L ], l1Only)
    // 範囲外のキーは候補なし
    Assert.Empty(Version.filesForPointLookup (intKey 9999) v)

[<Fact>]
let ``VersionEdit: 変更適用は削除後に追加する`` () =
    let oldFile = fmd 1L 0 100 199
    let replacement = fmd 1L 1 200 299
    let initial = Version.Empty |> Version.addFile oldFile

    let updated =
        VersionEdit.apply
            initial
            {
                AddedFiles = [ replacement ]
                DeletedFiles = [ oldFile ]
            }

    Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], Version.levelCounts updated)
    Assert.Equal(1L, (Version.filesAt 1 updated).[0].FileNumber)

[<Fact>]
let ``VersionEdit: レベルが範囲外なら CorruptionError`` () =
    let bad =
        { fmd 1L 0 100 199 with
            Level = Levels.Max + 1
        }

    expectErrorKind
        ErrorKind.CorruptionError
        "レベルは 0..Max の範囲内"
        (VersionEdit.decode (VersionEdit.encode (VersionEdit.addFile bad)))

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
    | _ -> Assert.Fail "1レコードに復元されるはず"
