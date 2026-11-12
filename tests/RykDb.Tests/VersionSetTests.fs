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

let private validate (v: Version) =
    let fail msg =
        err (DbError.internalError ("Version: " + msg))

    let checkLevel () level =
        let files = v.Files.[level]

        let checkTag =
            if files |> Array.forall (fun f -> f.Level = level) then
                ret ()
            else
                fail "ファイルの Level と格納位置が不一致"

        let checkOrder =
            seq { 1 .. files.Length - 1 }
            |> foldOk
                (fun () i ->
                    (if Bytes.compare files.[i - 1].Smallest files.[i].Smallest <= 0 then
                         ret ()
                     else
                         fail "ファイルが Smallest 昇順に並んでいない")
                    >>= fun () ->
                        if level > 0 then
                            if Bytes.compare files.[i - 1].Largest files.[i].Smallest < 0 then
                                ret ()
                            else
                                fail "L1 以上のファイル範囲が重複"
                        else
                            ret ())
                ()

        checkTag >>= fun () -> checkOrder

    result {
        do!
            if v.Files.Length = Levels.Count then
                ret ()
            else
                fail "レベルの数が不正"

        let all = v.Files |> Array.collect id
        let numbers = all |> Array.map _.FileNumber

        do!
            if (numbers |> Array.distinct).Length = numbers.Length then
                ret ()
            else
                fail "ファイル番号が重複"

        do!
            if all |> Array.forall (fun f -> f.FileNumber < v.NextFileNumber) then
                ret ()
            else
                fail "NextFileNumber が巻き戻っている"

        do!
            if all |> Array.forall (fun f -> f.Level >= 0 && f.Level <= Levels.Max) then
                ret ()
            else
                fail "レベルが範囲外"

        do!
            if all |> Array.forall (fun f -> Bytes.compare f.Smallest f.Largest <= 0) then
                ret ()
            else
                fail "Smallest が Largest より大きい"

        return! seq { 0 .. Levels.Max } |> foldOk checkLevel ()
    }


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
let ``Version.validate: 空の Version は妥当`` () = ok (validate Version.Empty) |> ignore

[<Fact>]
let ``Version.validate: ファイル番号の重複を検出`` () =
    let v =
        { Version.Empty with
            NextFileNumber = 2L
        }
        |> Version.addFile (fmd 1L 0 0 9)
        |> Version.addFile (fmd 1L 1 100 109)

    expectErrorKind ErrorKind.InternalError "番号の重複を検出するはず" (validate v)

[<Fact>]
let ``Version.validate: NextFileNumber の巻き戻しを検出`` () =
    let v = Version.addFile (fmd 5L 0 0 9) Version.Empty

    expectErrorKind ErrorKind.InternalError "NextFileNumber の巻き戻しを検出するはず" (validate { v with NextFileNumber = 5L })

[<Fact>]
let ``Version.validate: L1 以上の範囲重複を検出`` () =
    let v =
        { Version.Empty with
            NextFileNumber = 3L
        }
        |> Version.addFile (fmd 1L 1 0 99)
        |> Version.addFile (fmd 2L 1 50 149)

    expectErrorKind ErrorKind.InternalError "L1 の範囲重複を検出するはず" (validate v)

[<Fact>]
let ``Version.validate: L0 の範囲重複は許す`` () =
    let v =
        { Version.Empty with
            NextFileNumber = 3L
        }
        |> Version.addFile (fmd 1L 0 0 99)
        |> Version.addFile (fmd 2L 0 50 149)

    ok (validate v) |> ignore

[<Fact>]
let ``Version.validate: レベルと格納位置の不一致を検出`` () =
    let broken =
        { Version.Empty with
            NextFileNumber = 2L
            Files =
                Version.Empty.Files
                |> Array.mapi (fun i files -> if i = 0 then [| fmd 1L 1 0 9 |] else files)
        }

    expectErrorKind ErrorKind.InternalError "Level と格納位置の不一致を検出するはず" (validate broken)

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

        ok (validate v) |> ignore

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
