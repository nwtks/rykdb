// ===========================================================================
// Manifest.fs（MANIFEST / CURRENT）
// ===========================================================================

module RykDb.ManifestTests

open System
open System.IO
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

/// MANIFEST 用の一時ディレクトリ（テスト名とは無関係に固有にする）。
let private manifestDir () =
    Path.Combine(Path.GetTempPath(), sprintf "rykdb-manifest-%d" DateTime.Now.Ticks)

[<Fact>]
let ``Manifest: 新規作成と CURRENT の内容`` () =
    let dir = manifestDir ()

    try
        let version, manifestNumber = ok (Manifest.openOrCreate dir)
        Assert.Equal(1L, manifestNumber)
        Assert.Equal<int array>([| 0; 0; 0; 0; 0; 0; 0 |], Version.levelCounts version)
    finally
        if Directory.Exists dir then
            Directory.Delete(dir, true)

[<Fact>]
let ``Manifest: 変更を反映して再オープンしても復元できる`` () =
    let dir = manifestDir ()

    try
        let v0, manifestNumber = ok (Manifest.openOrCreate dir)

        let apply version edit =
            match Manifest.logAndApply dir manifestNumber version edit with
            | Ok(next, _) -> next
            | Error e ->
                Assert.Fail(DbError.toDisplayString e)
                RykDb.Core.Version.empty

        let v1 = apply v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        let v2 = apply v1 (VersionEdit.addFile (fmd 2L 1 100 199))
        let v3 = apply v2 (VersionEdit.deleteFile (fmd 1L 0 100 199))

        // v2 はまだ L0 に file1 がある。v3 で削除されている
        Assert.Equal<int array>([| 1; 1; 0; 0; 0; 0; 0 |], Version.levelCounts v2)
        Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], Version.levelCounts v3)

        // 再オープンで同じ Version に戻る
        let reopened, manifestNumber2 = ok (Manifest.openOrCreate dir)
        Assert.Equal(manifestNumber, manifestNumber2)
        Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], Version.levelCounts reopened)

        // 復元されたファイルの番号とキー範囲も一致する
        let restored = (Version.filesAt reopened 1).[0]
        Assert.Equal(2L, restored.FileNumber)

        Assert.Equal<string>(
            System.Text.Encoding.UTF8.GetString (fmd 2L 1 100 199).Smallest,
            System.Text.Encoding.UTF8.GetString restored.Smallest
        )
    finally
        if Directory.Exists dir then
            Directory.Delete(dir, true)

// ---------------------------------------------------------------------------
// 追記と NextFileNumber
// ---------------------------------------------------------------------------

/// logAndApply して新しい Version を返す（失敗はテストを落とす）。
let private applyEdit (dir: string) (manifestNumber: int64) (version: RykDb.Core.Version) (edit: VersionEdit) =
    match Manifest.logAndApply dir manifestNumber version edit with
    | Ok(next, _) -> next
    | Error e ->
        Assert.Fail(DbError.toDisplayString e)
        version

[<Fact>]
let ``Manifest: NextFileNumber は追加したファイル番号より必ず大きくなる`` () =
    withDir "manifest-next" (fun dir ->
        let v0, mn = ok (Manifest.openOrCreate dir)
        Assert.Equal(1L, v0.NextFileNumber)

        let v1 = applyEdit dir mn v0 (VersionEdit.addFile (fmd 5L 0 0 9))
        Assert.Equal(6L, v1.NextFileNumber)

        // 小さい番号の追加で巻き戻らない（次の flush が既存 SSTable を上書きしないため）
        let v2 = applyEdit dir mn v1 (VersionEdit.addFile (fmd 2L 2 0 9))
        Assert.True(v2.NextFileNumber >= v1.NextFileNumber, sprintf "NextFileNumber が巻き戻った: %d" v2.NextFileNumber)

        // 大きい番号を追加したらその次まで進む
        let v3 = applyEdit dir mn v2 (VersionEdit.addFile (fmd 9L 3 0 9))
        Assert.Equal(10L, v3.NextFileNumber))

[<Fact>]
let ``Manifest: 同じ CURRENT をもう一度開いても状態は変わらない`` () =
    withDir "manifest-idempotent" (fun dir ->
        let v0, mn0 = ok (Manifest.openOrCreate dir)
        Assert.Empty(Version.allFiles v0)

        // 2 回目の openOrCreate は新しい manifest を作らず同じ番号を指す
        let v1, mn1 = ok (Manifest.openOrCreate dir)
        Assert.Equal(mn0, mn1)
        Assert.Empty(Version.allFiles v1)
        Assert.Equal<int array>(Version.levelCounts v0, Version.levelCounts v1))

[<Fact>]
let ``Manifest: CURRENT が不正なら CorruptionError`` () =
    withDir "manifest-corrupt" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        File.WriteAllText(Path.Combine(dir, "CURRENT"), "not-a-manifest")

        match Manifest.openOrCreate dir with
        | Error e -> Assert.Equal<ErrorKind>(ErrorKind.CorruptionError, e.Kind)
        | Ok _ -> Assert.Fail("壊れた CURRENT はエラーになるはず"))
