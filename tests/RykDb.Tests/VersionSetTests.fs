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

/// logAndApply して新しい Version を返す（失敗はテストを落とす）。
let private applyEdit (dir: string) (manifestNumber: ManifestNumber) (version: Version) (edit: VersionEdit) =
    match Manifest.logAndApply dir manifestNumber version edit with
    | Ok(next, _) -> next
    | Error e ->
        Assert.Fail(DbError.toDisplayString e)
        version

[<Fact>]
let ``FileMetaData.Zero: 空メタデータ`` () =
    Assert.Equal(0L, FileMetaData.Zero.FileNumber)
    Assert.Equal(0, FileMetaData.Zero.Level)
    Assert.Equal(0L, FileMetaData.Zero.EntryCount)

[<Fact>]
let ``VersionEdit.decode: int64 を超えるエントリ数は CorruptionError`` () =
    let body =
        Writer.build
            [
                Writer.varint 1UL // タグ（追加）
                Writer.varint 7UL // ファイル番号
                Writer.varint 0UL // レベル
                Writer.varint 0x8000000000000000UL // エントリ数（2^63）
                Writer.varint 1UL // サイズ
                Writer.varint 1UL // MaxSeqNo
                Writer.lengthPrefixed [| 1uy |]
                Writer.lengthPrefixed [| 2uy |]
            ]

    expectErrorKind
        ErrorKind.CorruptionError
        "int64 を超えるエントリ数は CorruptionError"
        (VersionEdit.decode (Writer.build [ Writer.lengthPrefixed body ]))

[<Fact>]
let ``FileMetaData.writeSstable: 書き出してメタデータを返す`` () =
    withDir "fmd-write" (fun dir ->
        let entries = sstEntries 5
        let fmd = ok (FileMetaData.writeSstable dir 1L 0 0.01 entries.Length entries)
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

        expectErrorKind
            ErrorKind.InternalError
            "キー順でない入力はエラーになるはず"
            (FileMetaData.writeSstable dir 1L 0 0.01 bad.Length bad)
        // 失敗時に部分ファイルを残さない（次の open で孤児扱いされるのを防ぐ）
        Assert.False(System.IO.File.Exists(FileMetaData.path dir 1L)))

[<Fact>]
let ``FileMetaData.writeSstable: 書き込み失敗は IoError`` () =
    withDir "fmd-write-io" (fun dir ->
        let missing = System.IO.Path.Combine(dir, "no-such-dir")

        expectErrorKind
            ErrorKind.IoError
            "存在しないディレクトリへの書き込みはエラーになるはず"
            (let entries = sstEntries 3 in FileMetaData.writeSstable missing 1L 0 0.01 entries.Length entries))

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

    Assert.Equal<int array>([| 1; 1; 1; 0; 0; 0; 0 |], levelCounts v)

[<Fact>]
let ``Version.addFile: 追加は破壊的でない`` () =
    let v0 = Version.Empty
    let v1 = Version.addFile (fmd 1L 0 100 199) v0
    Assert.Equal(0, (levelCounts v0).[0])
    Assert.Equal(1, (levelCounts v1).[0])
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

    Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], levelCounts v)

[<Fact>]
let ``Version.validate: 空の Version は妥当`` () =
    ok (validateVersion Version.Empty) |> ignore

[<Fact>]
let ``Version.validate: ファイル番号の重複を検出`` () =
    let v =
        { Version.Empty with
            NextFileNumber = 2L
        }
        |> Version.addFile (fmd 1L 0 0 9)
        |> Version.addFile (fmd 1L 1 100 109)

    expectErrorKind ErrorKind.InternalError "番号の重複を検出するはず" (validateVersion v)

[<Fact>]
let ``Version.validate: NextFileNumber の巻き戻しを検出`` () =
    let v = Version.addFile (fmd 5L 0 0 9) Version.Empty

    expectErrorKind
        ErrorKind.InternalError
        "NextFileNumber の巻き戻しを検出するはず"
        (validateVersion { v with NextFileNumber = 5L })

[<Fact>]
let ``Version.validate: L1 以上の範囲重複を検出`` () =
    let v =
        { Version.Empty with
            NextFileNumber = 3L
        }
        |> Version.addFile (fmd 1L 1 0 99)
        |> Version.addFile (fmd 2L 1 50 149)

    expectErrorKind ErrorKind.InternalError "L1 の範囲重複を検出するはず" (validateVersion v)

[<Fact>]
let ``Version.validate: L0 の範囲重複は許す`` () =
    let v =
        { Version.Empty with
            NextFileNumber = 3L
        }
        |> Version.addFile (fmd 1L 0 0 99)
        |> Version.addFile (fmd 2L 0 50 149)

    ok (validateVersion v) |> ignore

[<Fact>]
let ``Version.validate: レベルと格納位置の不一致を検出`` () =
    let broken =
        { Version.Empty with
            NextFileNumber = 2L
            Files =
                Version.Empty.Files
                |> Array.mapi (fun i files -> if i = 0 then [| fmd 1L 1 0 9 |] else files)
        }

    expectErrorKind ErrorKind.InternalError "Level と格納位置の不一致を検出するはず" (validateVersion broken)

[<Fact>]
let ``Version.validate: L0 の追加・削除を繰り返しても不変条件を保つ`` () =
    let random = System.Random 20261111
    let mutable v = Version.Empty
    let mutable next = 1L
    let mutable files = []

    for _ in 1..300 do
        if not (List.isEmpty files) && random.Next 3 = 0 then
            let target = files.[random.Next files.Length]
            v <- Version.deleteFiles [ target ] v
            files <- files |> List.filter (fun f -> f.FileNumber <> target.FileNumber)
        else
            let lo = random.Next 10000
            let f = fmd next 0 lo (lo + random.Next 100)

            v <-
                { Version.addFile f v with
                    NextFileNumber = next + 1L
                }

            files <- f :: files
            next <- next + 1L

        ok (validateVersion v) |> ignore

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

    // L1 以降は二分探索で 1ファイルに絞る（境界の前後も確認する）。
    let l1Many =
        Version.Empty
        |> Version.addFile (fmd 10L 1 100 199)
        |> Version.addFile (fmd 11L 1 200 299)
        |> Version.addFile (fmd 12L 1 300 399)

    Assert.Equal<int64 list>(
        [ 11L ],
        Version.filesForPointLookup (intKey 250) l1Many
        |> List.map (fun f -> f.FileNumber)
    )

    Assert.Equal<int64 list>(
        [ 12L ],
        Version.filesForPointLookup (intKey 399) l1Many
        |> List.map (fun f -> f.FileNumber)
    )

    Assert.Empty(Version.filesForPointLookup (intKey 50) l1Many)

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

    Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], levelCounts updated)
    Assert.Equal(1L, (Version.filesAt 1 updated).[0].FileNumber)

[<Fact>]
let ``VersionEdit.encode / decode: エンコードの往復`` () =
    let added = fmd 3L 1 300 350
    let deleted = fmd 1L 0 100 199

    let edit =
        compose
            (VersionEdit.addFile added)
            {
                AddedFiles = []
                DeletedFiles = [ deleted ]
            }

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

[<Fact>]
let ``VersionEdit: ランダムな編集を encode/decode しても同じ`` () =
    let random = System.Random 20261114

    for _ in 1..200 do
        let added =
            [
                for i in 1 .. random.Next(0, 6) do
                    let lo = random.Next 1000
                    let hi = lo + random.Next 100
                    fmd (int64 i) (random.Next Levels.Count) lo hi
            ]

        let deleted =
            [
                for _ in 1 .. random.Next(0, 4) ->
                    { FileMetaData.Zero with
                        FileNumber = int64 (random.Next(1, 20))
                        Level = random.Next Levels.Count
                    }
            ]

        let edit =
            {
                AddedFiles = added
                DeletedFiles = deleted
            }

        let decoded = ok (VersionEdit.decode (VersionEdit.encode edit))
        Assert.Equal<int>(1, decoded.Length)
        Assert.Equal<byte array>(VersionEdit.encode edit, VersionEdit.encode decoded.Head)

[<Fact>]
let ``Manifest: 新規作成と CURRENT の内容`` () =
    withDir "manifest-new" (fun dir ->
        let version, manifestNumber = ok (Manifest.openOrCreate dir)
        Assert.Equal<ManifestNumber>(1L<manifestNo>, manifestNumber)
        Assert.Equal<int array>([| 0; 0; 0; 0; 0; 0; 0 |], levelCounts version))

[<Fact>]
let ``Manifest: 変更を反映して再オープンしても復元できる`` () =
    withDir "manifest-reopen" (fun dir ->
        let v0, manifestNumber = ok (Manifest.openOrCreate dir)
        let v1 = applyEdit dir manifestNumber v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        let v2 = applyEdit dir manifestNumber v1 (VersionEdit.addFile (fmd 2L 1 100 199))

        let v3 =
            applyEdit
                dir
                manifestNumber
                v2
                {
                    AddedFiles = []
                    DeletedFiles = [ fmd 1L 0 100 199 ]
                }
        // v2 はまだ L0 に file1 がある。v3 で削除されている
        Assert.Equal<int array>([| 1; 1; 0; 0; 0; 0; 0 |], levelCounts v2)
        Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], levelCounts v3)

        // 再オープンで同じ Version に戻る
        let reopened, manifestNumber2 = ok (Manifest.openOrCreate dir)
        Assert.Equal<ManifestNumber>(manifestNumber, manifestNumber2)
        Assert.Equal<int array>([| 0; 1; 0; 0; 0; 0; 0 |], levelCounts reopened)
        // 復元されたファイルの番号とキー範囲も一致する
        let restored = (Version.filesAt 1 reopened).[0]
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
        Assert.Equal<int array>(levelCounts v0, levelCounts v1))

[<Fact>]
let ``Manifest: 末尾の書きかけレコードは読み飛ばす`` () =
    withDir "manifest-bad-record" (fun dir ->
        let v0, mn = ok (Manifest.openOrCreate dir)
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        // 末尾に不正な base64 を追記する（クラッシュで書きかけになった追記を模擬）
        System.IO.File.AppendAllText(System.IO.Path.Combine(dir, "MANIFEST-000001"), "!!!not-base64!!!\n")

        // 末尾だけなので読み飛ばし、1 件目までの状態が残る
        let reopened, _ = ok (Manifest.openOrCreate dir)
        Assert.Equal<int array>([| 1; 0; 0; 0; 0; 0; 0 |], levelCounts reopened))

[<Fact>]
let ``Manifest: 末尾の書きかけを修復した後に追記できる`` () =
    withDir "manifest-repair-tail" (fun dir ->
        let v0, mn0 = ok (Manifest.openOrCreate dir)
        let _ = applyEdit dir mn0 v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        System.IO.File.AppendAllText(System.IO.Path.Combine(dir, "MANIFEST-000001"), "!!!not-base64!!!\n")

        // 末尾の書きかけを検出したら、復元できた Version を新しい MANIFEST に移す。
        let v1, mn1 = ok (Manifest.openOrCreate dir)
        Assert.NotEqual<ManifestNumber>(mn0, mn1)
        Assert.Equal<int>(1, Version.allFiles v1 |> List.length)

        // 修復後の MANIFEST に追記しても、次の再オープンで両方の編集が残る。
        let _ = applyEdit dir mn1 v1 (VersionEdit.addFile (fmd 2L 0 200 299))
        let v2, _ = ok (Manifest.openOrCreate dir)
        Assert.Equal<int array>([| 2; 0; 0; 0; 0; 0; 0 |], levelCounts v2))

[<Fact>]
let ``Manifest: ローテーション失敗時は旧 MANIFEST に編集を残さない`` () =
    withDir "manifest-rotation-failure" (fun dir ->
        let v0, mn0 = ok (Manifest.openOrCreate dir)
        let big = Array.create (5 * 1024 * 1024) 1uy

        let file =
            { FileMetaData.Zero with
                FileNumber = 1L
                Level = 0
                Smallest = big
                Largest = big
                EntryCount = 1L
                Size = 1L
                MaxSeqNo = 1UL
                ReadInfo = None
            }

        // 次の manifest 番号をディレクトリで塞ぎ、スナップショット書き込みを失敗させる。
        System.IO.Directory.CreateDirectory(System.IO.Path.Combine(dir, "MANIFEST-000002"))
        |> ignore

        match Manifest.logAndApplyWithOutcome dir mn0 v0 (VersionEdit.addFile file) with
        | ManifestApplyResult.NotApplied _ -> ()
        | other -> failActual "ローテーション失敗は NotApplied であるべき" other

        // NotApplied は「編集が反映されていない」ことを意味する。旧 MANIFEST はそのまま開ける。
        let v1, mn1 = ok (Manifest.openOrCreate dir)
        Assert.Equal<ManifestNumber>(mn0, mn1)
        Assert.Empty(Version.allFiles v1)

        System.IO.Directory.Delete(System.IO.Path.Combine(dir, "MANIFEST-000002"))
        let _, mn2 = ok (Manifest.logAndApply dir mn1 v1 (VersionEdit.addFile file))
        Assert.NotEqual<ManifestNumber>(mn1, mn2)
        let v3, _ = ok (Manifest.openOrCreate dir)
        Assert.Equal<int>(1, Version.allFiles v3 |> List.length))

[<Fact>]
let ``Manifest.logAndApplyWithOutcome: 通常の追記は Applied を返す`` () =
    withDir "manifest-outcome-applied" (fun dir ->
        let v0, mn0 = ok (Manifest.openOrCreate dir)

        match Manifest.logAndApplyWithOutcome dir mn0 v0 (VersionEdit.addFile (fmd 1L 0 100 199)) with
        | ManifestApplyResult.Applied(v1, mn1) ->
            Assert.Equal<ManifestNumber>(mn0, mn1)
            Assert.Equal<int>(1, Version.allFiles v1 |> List.length)
        | other -> failActual "通常の追記は Applied であるべき" other)

[<Fact>]
let ``Manifest.logAndApplyWithOutcome: 追記を戻せない場合は Unknown`` () =
    if not (System.OperatingSystem.IsLinux()) then
        ()
    else
        withDir "manifest-unknown" (fun dir ->
            let v0, mn0 = ok (Manifest.openOrCreate dir)
            let path = System.IO.Path.Combine(dir, "MANIFEST-000001")
            System.IO.File.Delete path
            System.IO.File.CreateSymbolicLink(path, "/dev/full") |> ignore

            match Manifest.logAndApplyWithOutcome dir mn0 v0 (VersionEdit.addFile (fmd 1L 0 100 199)) with
            | ManifestApplyResult.Unknown _ -> ()
            | other -> failActual "追記もロールバックも失敗したら Unknown であるべき" other)

[<Fact>]
let ``Manifest: 途中の壊れたレコードは CorruptionError`` () =
    withDir "manifest-mid-corrupt" (fun dir ->
        let v0, mn = ok (Manifest.openOrCreate dir)
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 2L 0 100 199))

        // 有効な 2 行の間に壊れた行を挿入する（bit rot を模擬）
        let path = System.IO.Path.Combine(dir, "MANIFEST-000001")
        let lines = System.IO.File.ReadAllLines path
        System.IO.File.WriteAllLines(path, Array.append [| lines.[0]; "!!!not-base64!!!" |] lines.[1..])

        expectErrorKind ErrorKind.CorruptionError "途中の破損は黙って捨てない" (Manifest.openOrCreate dir))

[<Fact>]
let ``Manifest: 途中のレコードのチェックサム不一致は CorruptionError`` () =
    withDir "manifest-mid-checksum" (fun dir ->
        let v0, mn = ok (Manifest.openOrCreate dir)
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 2L 0 100 199))

        // base64 としては正しいまま中身を 1バイト壊す（bit rot を模擬）。
        // チェックサムが無いと VersionEdit として偶然デコードされ得る破損。
        let path = System.IO.Path.Combine(dir, "MANIFEST-000001")
        let lines = System.IO.File.ReadAllLines path
        let bytes = System.Convert.FromBase64String lines.[0]
        bytes.[bytes.Length - 1] <- bytes.[bytes.Length - 1] ^^^ 0xFFuy
        lines.[0] <- System.Convert.ToBase64String bytes
        System.IO.File.WriteAllLines(path, lines)

        expectErrorKind ErrorKind.CorruptionError "チェックサムで途中の破損を検出する" (Manifest.openOrCreate dir))

[<Fact>]
let ``Manifest: 空行は読み飛ばす`` () =
    withDir "manifest-blank-line" (fun dir ->
        let v0, mn = ok (Manifest.openOrCreate dir)
        let _ = applyEdit dir mn v0 (VersionEdit.addFile (fmd 1L 0 100 199))
        // 末尾に空行を挟んでも再オープンできる
        System.IO.File.AppendAllText(System.IO.Path.Combine(dir, "MANIFEST-000001"), "\n")

        let reopened, _ = ok (Manifest.openOrCreate dir)
        Assert.Equal<int array>([| 1; 0; 0; 0; 0; 0; 0 |], levelCounts reopened))

[<Fact>]
let ``Manifest: 空の CURRENT は新規作成として扱う`` () =
    withDir "manifest-empty-current" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "CURRENT"), "\n")

        let version, manifestNumber = ok (Manifest.openOrCreate dir)
        Assert.Equal<ManifestNumber>(1L<manifestNo>, manifestNumber)
        Assert.Empty(Version.allFiles version))

[<Fact>]
let ``Manifest: CURRENT の .manifest 付き表記は番号として読む`` () =
    withDir "manifest-suffix" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "CURRENT"), "MANIFEST-000007.manifest\n")
        // 指している MANIFEST は空でよい
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "MANIFEST-000007"), "")

        let version, manifestNumber = ok (Manifest.openOrCreate dir)
        Assert.Equal<ManifestNumber>(7L<manifestNo>, manifestNumber)
        Assert.Empty(Version.allFiles version))

[<Fact>]
let ``Manifest: CURRENT が指す MANIFEST が無ければ CorruptionError`` () =
    withDir "manifest-missing" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "CURRENT"), "MANIFEST-000007\n")

        // 空の Version で開くと SSTable が孤児になるので、開かずにエラーにする
        expectErrorKind ErrorKind.CorruptionError "manifest の欠落はエラーになる" (Manifest.openOrCreate dir))

[<Fact>]
let ``Manifest: CURRENT が無いのに SSTable があれば CorruptionError`` () =
    withDir "manifest-no-current" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        System.IO.File.WriteAllBytes(System.IO.Path.Combine(dir, "000001.sst"), [| 1uy |])

        expectErrorKind ErrorKind.CorruptionError "SSTable を孤児にしないため開かない" (Manifest.openOrCreate dir))

[<Fact>]
let ``Manifest: 中身があるのに Version が空なら CorruptionError`` () =
    withDir "manifest-empty-version" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "CURRENT"), "MANIFEST-000001\n")
        // "AAAA" は 4バイトで、チェックサムフレームとしても VersionEdit としても不正
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "MANIFEST-000001"), "AAAA\n")
        System.IO.File.WriteAllBytes(System.IO.Path.Combine(dir, "000002.sst"), [| 1uy |])

        expectErrorKind ErrorKind.CorruptionError "SSTable があるのに空の Version はエラー" (Manifest.openOrCreate dir))

[<Fact>]
let ``Manifest: CURRENT が不正なら CorruptionError`` () =
    withDir "manifest-corrupt" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "CURRENT"), "not-a-manifest")
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
let ``Manifest: 大きくなった manifest はスナップショットへローテーションする`` () =
    withDir "manifest-rotate" (fun dir ->
        let v0, mn0 = ok (Manifest.openOrCreate dir)

        // 1レコードで 4MiB を超える編集を作る。
        let big = Array.create (5 * 1024 * 1024) 1uy

        let file =
            { FileMetaData.Zero with
                FileNumber = 1L
                Level = 0
                Smallest = big
                Largest = big
                EntryCount = 1L
                Size = 1L
                MaxSeqNo = 1UL
                ReadInfo = None
            }

        let _, mn1 = ok (Manifest.logAndApply dir mn0 v0 (VersionEdit.addFile file))
        Assert.NotEqual<ManifestNumber>(mn0, mn1)

        // 新しい manifest を CURRENT が指し、Version 全体が復元できる。
        let v2, mn2 = ok (Manifest.openOrCreate dir)
        Assert.Equal<ManifestNumber>(mn1, mn2)
        Assert.Equal<int>(1, Version.allFiles v2 |> List.length))

[<Fact>]
let ``Manifest: NextFileNumber が Int64.MaxValue のとき負に回り込まない`` () =
    withDir "manifest-next-max" (fun dir ->
        let _, mn = ok (Manifest.openOrCreate dir)

        let v =
            { Version.Empty with
                NextFileNumber = System.Int64.MaxValue
            }
        // used = MaxValue + 1 は負になるため、据え置き（else 側）になる
        let next = applyEdit dir mn v (VersionEdit.addFile (fmd 1L 0 0 9))
        Assert.Equal<int64>(System.Int64.MaxValue, next.NextFileNumber))

[<Fact>]
let ``Manifest: ランダムな VersionEdit を追記して再オープンしても同じファイル集合`` () =
    withDir "manifest-roundtrip-random" (fun dir ->
        let v0, mn0 = ok (Manifest.openOrCreate dir)
        let random = System.Random 20261115
        let mutable v = v0
        let mutable mn = mn0
        let mutable expected = []

        for _ in 1..50 do
            let lo = random.Next 1000
            let f = fmd v.NextFileNumber (random.Next Levels.Count) lo (lo + random.Next 100)
            v <- applyEdit dir mn v (VersionEdit.addFile f)
            expected <- f :: expected

        let v2, _ = ok (Manifest.openOrCreate dir)

        let numbersOf (files: FileMetaData list) =
            files |> List.map _.FileNumber |> List.sort

        Assert.Equal<int64 list>(numbersOf expected, numbersOf (Version.allFiles v2)))
