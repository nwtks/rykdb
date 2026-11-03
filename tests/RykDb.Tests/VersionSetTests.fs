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

let private sstEntries n =
    Array.init n (fun i -> SstEntry.make (intKey i) (uint64 (i + 1)) (Some [| SqlValue.Int(int64 i) |]))
    |> Array.sortWith SstEntry.compare

[<Fact>]
let ``FileMetaData.zero: 空メタデータ`` () =
    Assert.Equal(0L, FileMetaData.Zero.FileNumber)
    Assert.Equal(0, FileMetaData.Zero.Level)
    Assert.Equal(0L, FileMetaData.Zero.EntryCount)

[<Fact>]
let ``FileMetaData.writeSstable: 書き出してメタデータを返す`` () =
    withDir "fmd-write" (fun dir ->
        let entries = sstEntries 5
        let fmd = ok (FileMetaData.writeSstable dir 1L 0 0.01 entries)
        Assert.Equal(1L, fmd.FileNumber)
        Assert.Equal(0, fmd.Level)
        Assert.Equal(5L, fmd.EntryCount)
        Assert.True(fmd.Size > 0L))

[<Fact>]
let ``FileMetaData.writeSstable: キー順でない入力はエラー`` () =
    withDir "fmd-write-bad" (fun dir ->
        let bad =
            [|
                SstEntry.make (intKey 5) 1UL (Some [| SqlValue.Int 5L |])
                SstEntry.make (intKey 1) 2UL (Some [| SqlValue.Int 1L |])
            |]

        expectErrorKind ErrorKind.InternalError "キー順でない入力はエラーになるはず" (FileMetaData.writeSstable dir 1L 0 0.01 bad)
        // 失敗時に部分ファイルを残さない（次の open で孤児扱いされるのを防ぐ）
        Assert.False(System.IO.File.Exists(FileMetaData.path dir 1L)))

[<Fact>]
let ``FileMetaData.writeSstable: 書き込み失敗は IoError`` () =
    withDir "fmd-write-io" (fun dir ->
        let missing = System.IO.Path.Combine(dir, "no-such-dir")

        expectErrorKind
            ErrorKind.IoError
            "存在しないディレクトリへの書き込みはエラーになるはず"
            (FileMetaData.writeSstable missing 1L 0 0.01 (sstEntries 3)))

[<Fact>]
let ``FileMetaData.mightContain: 範囲内なら true`` () =
    let f = FileMetaData.Zero

    let f2 =
        { f with
            Smallest = [| 1uy |]
            Largest = [| 5uy |]
        }

    Assert.True(FileMetaData.mightContain [| 3uy |] f2)
    Assert.False(FileMetaData.mightContain [| 0uy |] f2)
    Assert.False(FileMetaData.mightContain [| 9uy |] f2)

[<Fact>]
let ``Version.addFile: ファイル追加でレベル構成が変わる`` () =
    let v =
        Version.Empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 1 100 199)
        |> Version.addFile (fmd 3L 2 0 99)

    Assert.Equal<int array>([| 1; 1; 1; 0; 0; 0; 0 |], Version.levelCounts v)

[<Fact>]
let ``Version.addFile: 追加は破壊的でない`` () =
    let v0 = Version.Empty
    let v1 = Version.addFile (fmd 1L 0 100 199) v0
    Assert.Equal(0, (Version.levelCounts v0).[0])
    Assert.Equal(1, (Version.levelCounts v1).[0])
    // v0 に後から v1 のファイルは見えない
    Assert.Empty(Version.filesAt 0 v0)
    Assert.Equal(1, (Version.filesAt 0 v1).Length)

[<Fact>]
let ``Version.deleteFiles: 削除でレベル構成が変わる`` () =
    let v =
        Version.Empty
        |> Version.addFile (fmd 1L 0 100 199)
        |> Version.addFile (fmd 2L 1 100 199)
        |> Version.deleteFiles [ fmd 1L 0 100 199 ]

    Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], Version.levelCounts v)

[<Fact>]
let ``Version.filesForPointLookup: ポイントルックアップの候補は新しい順`` () =
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
let ``VersionEdit.apply: 変更適用は削除後に追加する`` () =
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
let ``VersionEdit.encode / decode: エンコードの往復`` () =
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

[<Fact>]
let ``VersionEdit.encode / decode: レベルが範囲外なら CorruptionError`` () =
    let bad =
        { fmd 1L 0 100 199 with
            Level = Levels.Max + 1
        }

    expectErrorKind
        ErrorKind.CorruptionError
        "レベルは 0..Max の範囲内"
        (VersionEdit.decode (VersionEdit.encode (VersionEdit.addFile bad)))
