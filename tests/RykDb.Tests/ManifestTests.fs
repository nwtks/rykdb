// ===========================================================================
// Manifest.fs（MANIFEST / CURRENT）
// ===========================================================================

module RykDb.ManifestTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// logAndApply して新しい Version を返す（失敗はテストを落とす）。
let private applyEdit (dir: string) (manifestNumber: int64) (version: Version) (edit: VersionEdit) =
    match Manifest.logAndApply dir manifestNumber version edit with
    | Ok(next, _) -> next
    | Error e ->
        Assert.Fail(DbError.toDisplayString e)
        version

[<Fact>]
let ``Manifest: 新規作成と CURRENT の内容`` () =
    withDir "manifest-new" (fun dir ->
        let version, manifestNumber = ok (Manifest.openOrCreate dir)
        Assert.Equal(1L, manifestNumber)
        Assert.Equal<int array>([| 0; 0; 0; 0; 0; 0; 0 |], Version.levelCounts version))

[<Fact>]
let ``Manifest: 変更を反映して再オープンしても復元できる`` () =
    withDir "manifest-reopen" (fun dir ->
        let v0, manifestNumber = ok (Manifest.openOrCreate dir)
        let v1 = applyEdit dir manifestNumber v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        let v2 = applyEdit dir manifestNumber v1 (VersionEdit.addFile (fmd 2L 1 100 199))
        let v3 = applyEdit dir manifestNumber v2 (VersionEdit.deleteFile (fmd 1L 0 100 199))
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
        ))

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
let ``Manifest: 途中の壊れたレコードで読み込みを打ち切る`` () =
    withDir "manifest-bad-record" (fun dir ->
        let v0, mn = ok (Manifest.openOrCreate dir)
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        // 2 行目に不正な base64 を追記する
        File.AppendAllText(Path.Combine(dir, "MANIFEST-000001"), "!!!not-base64!!!\n")

        // 打ち切られるので 1 件目までの状態が残る
        let reopened, _ = ok (Manifest.openOrCreate dir)
        Assert.Equal<int array>([| 1; 0; 0; 0; 0; 0; 0 |], Version.levelCounts reopened))

[<Fact>]
let ``Manifest: 空行は読み飛ばす`` () =
    withDir "manifest-blank-line" (fun dir ->
        let v0, mn = ok (Manifest.openOrCreate dir)
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        // 末尾に空行を挟んでも再オープンできる
        File.AppendAllText(Path.Combine(dir, "MANIFEST-000001"), "\n")

        let reopened, _ = ok (Manifest.openOrCreate dir)
        Assert.Equal<int array>([| 1; 0; 0; 0; 0; 0; 0 |], Version.levelCounts reopened))

[<Fact>]
let ``Manifest: 空の CURRENT は新規作成として扱う`` () =
    withDir "manifest-empty-current" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        File.WriteAllText(Path.Combine(dir, "CURRENT"), "\n")

        let version, manifestNumber = ok (Manifest.openOrCreate dir)
        Assert.Equal(1L, manifestNumber)
        Assert.Empty(Version.allFiles version))

[<Fact>]
let ``Manifest: CURRENT の .manifest 付き表記は番号として読む`` () =
    withDir "manifest-suffix" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        File.WriteAllText(Path.Combine(dir, "CURRENT"), "MANIFEST-000007.manifest\n")

        // 指している MANIFEST が無くても空の Version で開ける
        let version, manifestNumber = ok (Manifest.openOrCreate dir)
        Assert.Equal(7L, manifestNumber)
        Assert.Empty(Version.allFiles version))

[<Fact>]
let ``Manifest: CURRENT が不正なら CorruptionError`` () =
    withDir "manifest-corrupt" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        File.WriteAllText(Path.Combine(dir, "CURRENT"), "not-a-manifest")
        expectErrorKind ErrorKind.CorruptionError "壊れた CURRENT はエラーになるはず" (Manifest.openOrCreate dir))

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
let ``Manifest: NextFileNumber が Int64.MaxValue のとき負に回り込まない`` () =
    withDir "manifest-next-max" (fun dir ->
        let _, mn = ok (Manifest.openOrCreate dir)

        let v =
            { Version.empty with
                NextFileNumber = System.Int64.MaxValue
            }
        // used = MaxValue + 1 は負になるため、据え置き（else 側）になる
        let next = applyEdit dir mn v (VersionEdit.addFile (fmd 1L 0 0 9))
        Assert.Equal<int64>(System.Int64.MaxValue, next.NextFileNumber))
