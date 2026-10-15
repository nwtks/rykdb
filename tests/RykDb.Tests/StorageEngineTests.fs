// ===========================================================================
// StorageEngine.fs（put / flush / scan と復旧）
// ===========================================================================

module RykDb.StorageEngineTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private withEngine (dir: string) (f: StorageEngine -> unit) =
    let engine = ok (StorageEngine.openDatabase dir)
    f engine

let private get (engine: StorageEngine) (key: byte array) = StorageEngine.getAt engine None key

let private scan (engine: StorageEngine) (startKey: byte array) (limit: int) =
    StorageEngine.scanAt engine None startKey limit

// ----------------------------------------------------------- StorageEngine

[<Fact>]
let ``StorageEngine: put して memtable から引ける`` () =
    withDir "se1" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let found = ok (get engine (intKey 1))

            match found with
            | Some e -> Assert.Equal<uint64>(1UL, e.SeqNo)
            | None -> Assert.Fail("見つからないはず")))

[<Fact>]
let ``StorageEngine: flush すると SSTable から引ける`` () =
    withDir "se2" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            let engine = ok (StorageEngine.flush engine)

            // memtable は空になり、L0 にファイルが 1 個できる
            Assert.Equal(0, engine.Mem.Count)
            Assert.True((ok (get engine (intKey 1))).IsSome)
            Assert.True((ok (get engine (intKey 2))).IsSome)))

[<Fact>]
let ``StorageEngine: 削除は tombstone になる`` () =
    withDir "se3" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.delete engine (intKey 1))

            match ok (get engine (intKey 1)) with
            | Some e -> Assert.True(e.IsDelete)
            | None -> Assert.Fail("tombstone が残るはず")))

[<Fact>]
let ``StorageEngine: scan はキー昇順で重複を畳む`` () =
    withDir "se4" (fun dir ->
        withEngine dir (fun engine ->
            let engine =
                List.fold (fun e i -> ok (StorageEngine.put e (intKey i) (intRow i))) engine [ 1; 2; 3 ]

            let got = ok (scan engine [||] 0)
            Assert.Equal(3, got.Length)
            Assert.Equal<byte array>(intKey 1, got.[0].Key)
            Assert.Equal<byte array>(intKey 2, got.[1].Key)
            Assert.Equal<byte array>(intKey 3, got.[2].Key)))

[<Fact>]
let ``StorageEngine: scan の limit が効く`` () =
    withDir "se5" (fun dir ->
        withEngine dir (fun engine ->
            let engine =
                List.fold (fun e i -> ok (StorageEngine.put e (intKey i) (intRow i))) engine [ 1; 2; 3; 4; 5 ]

            let scanned = ok (scan engine [||] 2)
            Assert.Equal(2, scanned.Length)))

[<Fact>]
let ``StorageEngine: flush 済みと memtable をまたいで scan できる`` () =
    withDir "se6" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            let engine = ok (StorageEngine.flush engine)
            let engine = ok (StorageEngine.put engine (intKey 3) (intRow 3))

            let got = ok (scan engine [||] 0)
            Assert.Equal(3, got.Length)
            Assert.Equal<byte array>(intKey 3, got.[2].Key)))

// ------------------------------------------------------------------ 復旧

[<Fact>]
let ``復旧: close して開き直すとデータが残る`` () =
    withDir "re1" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            ok (StorageEngine.close engine))

        withEngine dir (fun engine ->
            // close は flush しているので L0 から引ける
            Assert.True((ok (get engine (intKey 1))).IsSome)
            Assert.True((ok (get engine (intKey 2))).IsSome)))

[<Fact>]
let ``復旧: close せずにプロセスを終了しても WAL から復旧できる`` () =
    withDir "re2" (fun dir ->
        // flush しないまま「プロセス終了」を模擬する（WAL にだけ残っている）
        withEngine dir (fun engine ->
            let _ = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let _ = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            ())

        // 開き直すと WAL 再生で値が戻る
        withEngine dir (fun engine ->
            Assert.True((ok (get engine (intKey 1))).IsSome)
            Assert.True((ok (get engine (intKey 2))).IsSome)))

[<Fact>]
let ``復旧: WAL 再生後は SeqNo が既存版より小さくならない`` () =
    withDir "re3" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.flush engine)
            let _ = ok (StorageEngine.put engine (intKey 2) (intRow 2))
            ())

        withEngine dir (fun engine ->
            // 既存の L0 に入る前の seqNo より大きい値で書き再開できる
            let before = engine.NextSeqNo
            let engine = ok (StorageEngine.put engine (intKey 3) (intRow 3))
            Assert.True(engine.NextSeqNo > before)
            let engine = ok (StorageEngine.flush engine)
            let got = ok (scan engine [||] 0)
            Assert.Equal(3, got.Length)))

[<Fact>]
let ``復旧: 存在しない SSTable は Version から落とされる`` () =
    withDir "re4" (fun dir ->
        withEngine dir (fun engine ->
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let _ = ok (StorageEngine.flush engine)
            // flush でできるファイルを消す（クラッシュの模擬）
            let ssts = Directory.GetFiles(dir, "*.sst")
            Assert.NotEmpty(ssts)
            ssts |> Array.iter (fun p -> File.Delete(p))
            ())

        withEngine dir (fun engine ->
            // Version から落ちているのでエラーにならない
            Assert.False((ok (get engine (intKey 1))).IsSome)))

[<Fact>]
let ``復旧: 孤児の SSTable は削除される`` () =
    withDir "re5" (fun dir ->
        withEngine dir (fun _ -> ())

        // MANIFEST に無いファイルを 1 個置いてから開く
        File.WriteAllBytes(Path.Combine(dir, "000999.sst"), [| 1uy; 2uy; 3uy |])

        withEngine dir (fun _ -> Assert.False(File.Exists(Path.Combine(dir, "000999.sst")))))

[<Fact>]
let ``復旧: WAL は flush 後に切り替わる`` () =
    withDir "re6" (fun dir ->
        withEngine dir (fun engine ->
            let before = engine.Wal.FileNumber
            let engine = ok (StorageEngine.put engine (intKey 1) (intRow 1))
            let engine = ok (StorageEngine.flush engine)

            Assert.True(engine.Wal.FileNumber > before)
            // 古い WAL は消えて、新しいものは空
            let wals = Wal.walFiles dir
            Assert.Single<string>(wals) |> ignore
            Assert.Equal(0, (ok (Wal.read dir engine.Wal.FileNumber)).Length)))
