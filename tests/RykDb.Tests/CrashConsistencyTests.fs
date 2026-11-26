// ===========================================================================
// CrashConsistencyTests — クラッシュ整合性（C1/C2）の復旧経路。
//
// C1: プロセスの異常終了（flush せずに開き直す）
// C2: ファイル名の消失（新しい WAL / SSTable / CURRENT スロットを消す）
// ===========================================================================

module RykDb.CrashConsistencyTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// memtable 上限 1 で開く（書き込みごとに flush させる）。
let private openEngine (dir: string) =
    ok (StorageEngine.openDatabaseWith (Some 1) dir)

/// B+tree ではなく LSM の SSTable を 1 行書く。
let private writeRow (engine: StorageEngine) (i: int) =
    ok (StorageEngine.writeBatch [| Entry.put (intKey i) (uint64 i) (intRow i) |] engine)

/// 行が見えるか。
let private hasRow (engine: StorageEngine) (i: int) =
    (ok (StorageEngine.getAtMasked (intKey i) [||] None engine)).IsSome

/// 最新の SSTable のパス。
let private newestSstable (dir: string) =
    Directory.GetFiles(dir, "*" + FileMetaData.SstExtension)
    |> Array.maxBy (fun path -> System.Int64.Parse(Path.GetFileNameWithoutExtension path))

/// ファイル番号の SSTable を消す。
let private deleteSstable (dir: string) (fileNumber: FileNumber) =
    let path = FileMetaData.path dir fileNumber

    if File.Exists path then
        File.Delete path

[<Fact>]
let ``CrashConsistency: CURRENT スロットの片方が壊れても他方で開ける`` () =
    withDir "cc-current-slot" (fun dir ->
        let engine = ok (StorageEngine.openDatabaseWith (Some 1) dir)
        let engine = writeRow engine 1
        ok (StorageEngine.close engine)

        // 片方のスロットを壊す。もう片方のスロットが同じ manifest を指している。
        File.WriteAllText(Path.Combine(dir, "CURRENT.A"), "broken")
        let reopened = ok (StorageEngine.openDatabaseWith None dir)
        Assert.True(hasRow reopened 1, "残った CURRENT スロットから復旧できません"))

[<Fact>]
let ``CrashConsistency: CURRENT の両スロットが失われても MANIFEST から復旧する`` () =
    withDir "cc-current-both" (fun dir ->
        let engine = ok (StorageEngine.openDatabaseWith (Some 1) dir)
        let engine = writeRow engine 1
        ok (StorageEngine.close engine)

        // スロットを両方消す。MANIFEST ファイルは残っている。
        File.Delete(Path.Combine(dir, "CURRENT.A"))
        File.Delete(Path.Combine(dir, "CURRENT.B"))
        let reopened = ok (StorageEngine.openDatabaseWith None dir)
        Assert.True(hasRow reopened 1, "MANIFEST からの最終手段の復旧が効きません"))

[<Fact>]
let ``CrashConsistency: CURRENT が指さない新しい MANIFEST は採用しない`` () =
    withDir "cc-uncommitted-manifest" (fun dir ->
        let engine = ok (StorageEngine.openDatabaseWith (Some 1) dir)
        let engine = writeRow engine 1
        ok (StorageEngine.close engine)

        // スロットを更新せずに、より新しい番号の MANIFEST スナップショットを置く。
        let edit: VersionEdit = { AddedFiles = []; DeletedFiles = [] }
        let payload = VersionEdit.encode edit

        let frame =
            Writer.build [ Writer.uint32LE (Hashing.checksum payload); Writer.writeBytes payload ]

        File.WriteAllText(Path.Combine(dir, "MANIFEST-000009"), System.Convert.ToBase64String frame + "\n")

        // スロットが指す MANIFEST を優先し、未反映の新しい MANIFEST を採用しない。
        let reopened = ok (StorageEngine.openDatabaseWith None dir)
        Assert.Equal<ManifestNumber>(1L<manifestNo>, reopened.ManifestNumber)
        Assert.True(hasRow reopened 1, "未反映の MANIFEST を採用して行が消えました"))

[<Fact>]
let ``CrashConsistency: 最新 WAL の名前が失われても flush 済みデータは残る`` () =
    withDir "cc-wal-name-loss" (fun dir ->
        let engine = openEngine dir
        let engine = writeRow engine 1
        let engine, error = ok (StorageEngine.maybeFlush engine)
        Assert.True(error.IsNone, "flush が失敗しました")

        // 2 行目は flush 前の WAL にだけ存在する。
        let engine = writeRow engine 2
        let newestWal = Wal.path dir engine.Wal.FileNumber
        Assert.True(File.Exists newestWal, "最新 WAL がありません")

        // 新しい WAL の名前が失われた状況を模す。
        File.Delete newestWal
        let reopened = ok (StorageEngine.openDatabaseWith None dir)
        Assert.True(hasRow reopened 1, "flush 済みの行が WAL の名前消失で消えました")
        Assert.False(hasRow reopened 2, "失われた WAL の未 flush 行が見えています"))

[<Fact>]
let ``CrashConsistency: 新しい SSTable の名前が失われても WAL から復旧する`` () =
    withDir "cc-sst-name-loss" (fun dir ->
        let engine = openEngine dir
        let engine = writeRow engine 1
        let engine, error = ok (StorageEngine.maybeFlush engine)
        Assert.True(error.IsNone, "1 回目の flush が失敗しました")

        let engine = writeRow engine 2
        let _, error = ok (StorageEngine.maybeFlush engine)
        Assert.True(error.IsNone, "2 回目の flush が失敗しました")

        // 2 行目を含む SSTable の名前が失われた状況を模す。
        let lost = newestSstable dir
        File.Delete lost
        let reopened = ok (StorageEngine.openDatabaseWith None dir)
        Assert.True(hasRow reopened 1, "1 行目が復旧していません")
        Assert.True(hasRow reopened 2, "失われた SSTable の行が WAL から復旧していません")

        // 復旧を 2 回繰り返しても同じ manifest を指し、結果が変わらない。
        let second = ok (StorageEngine.openDatabaseWith None dir)
        Assert.Equal(reopened.ManifestNumber, second.ManifestNumber)
        Assert.True(hasRow second 1)
        Assert.True(hasRow second 2))

[<Fact>]
let ``CrashConsistency: compaction の出力名が失われても前の Version と入力から復旧する`` () =
    withDir "cc-compaction-output-loss" (fun dir ->
        let mutable engine = ok (StorageEngine.openDatabaseWith (Some 1) dir)

        for i in 1..3 do
            engine <- writeRow engine i
            let next, error = ok (StorageEngine.maybeFlush engine)
            engine <- next
            Assert.True(error.IsNone, sprintf "%d 回目の flush が失敗しました" i)

        // 4 回目の flush で L0→L1 compaction が走る。
        let inputs =
            Version.filesAt 0 engine.Version |> Array.map _.FileNumber |> Array.toList

        Assert.Equal(3, inputs.Length)

        engine <- writeRow engine 4
        let next, error = ok (StorageEngine.maybeFlush engine)
        engine <- next
        Assert.True(error.IsNone, "compaction が失敗しました")

        let outputs =
            Version.filesAt 1 engine.Version |> Array.map _.FileNumber |> Array.toList

        Assert.NotEmpty outputs

        // 入力は遅延削除で残っている（復旧のロールバック元）。
        inputs
        |> List.iter (fun n -> Assert.True(File.Exists(FileMetaData.path dir n), sprintf "入力 %d が残っていません" n))

        // compaction の出力名が失われた状況を模す。
        outputs |> List.iter (deleteSstable dir)

        let reopened = ok (StorageEngine.openDatabaseWith None dir)

        for i in 1..4 do
            Assert.True(hasRow reopened i, sprintf "%d 行目が入力から復旧していません" i))

[<Fact>]
let ``CrashConsistency: WAL v2 のフレームヘッダー破損は CorruptionError`` () =
    withDir "cc-wal-frame-crc" (fun dir ->
        let engine = openEngine dir
        let engine = writeRow engine 1

        // ファイルヘッダー（16 バイト）の直後のフレームヘッダーの CRC を壊す。
        let path = Wal.path dir engine.Wal.FileNumber
        let bytes = File.ReadAllBytes path
        let headerCrcOffset = 16 + 8
        bytes.[headerCrcOffset] <- bytes.[headerCrcOffset] ^^^ 0xFFuy
        File.WriteAllBytes(path, bytes)

        expectErrorKind
            ErrorKind.CorruptionError
            "壊れたフレームヘッダーを torn write として黙って捨てない"
            (StorageEngine.openDatabaseWith None dir))
