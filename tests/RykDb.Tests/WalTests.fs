// ===========================================================================
// Wal.fs（書き込みログ）
// ===========================================================================

module RykDb.WalTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

let private sstKey (i: int) =
    KeyBuilder.ofValues [ SqlValue.Int(int64 i) ]

let private valOf (i: int) = [| SqlValue.Int(int64 i) |]

[<Fact>]
let ``WAL: 往復する`` () =
    withDir "wal" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)

        let entries =
            [|
                Entry.put (sstKey 1) 1UL (valOf 1)
                Entry.put (sstKey 2) 2UL (valOf 2)
                Entry.delete (sstKey 3) 3UL
            |]

        let records =
            [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, entries); WalRecord.Commit 1UL ]

        let w1 = ok (Wal.append w0 dir records)
        let w2 = ok (Wal.append w1 dir [ WalRecord.Begin 2UL; WalRecord.Abort 2UL ])

        // 中断した transaction は捨てられ、コミットした分だけ残る
        let recovered = ok (Wal.read dir 1L)
        Assert.Equal(3, recovered.Length)
        Assert.Equal<uint64>(1UL, recovered.[0].SeqNo)
        Assert.Equal<uint64>(3UL, recovered.[2].SeqNo)
        Assert.True(recovered.[2].IsDelete))

[<Fact>]
let ``WAL: 未コミットは再生しない`` () =
    withDir "wal2" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)
        // Commit を書かずに終わる = クラッシュした状態
        let entries = [| Entry.put (sstKey 9) 1UL (valOf 9) |]

        let _w =
            ok (Wal.append w0 dir [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, entries) ])

        Assert.Empty(ok (Wal.read dir 1L)))

[<Fact>]
let ``WAL: 書きかけレコードは読み飛ばす`` () =
    withDir "wal3" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)
        let entries = [| Entry.put (sstKey 1) 1UL (valOf 1) |]

        let w1 =
            ok (Wal.append w0 dir [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, entries); WalRecord.Commit 1UL ])

        // 末尾に壊れたレコードを書き足す（torn write の再現）
        let path = Wal.path dir w1.FileNumber
        File.AppendAllBytes(path, [| 0x10uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |])

        // 壊れたレコードは打ち切られるが、それまでのものはちゃんと読めた
        Assert.Equal(1, (ok (Wal.read dir 1L)).Length))

[<Fact>]
let ``WAL: チェックサム不一致は打ち切る`` () =
    withDir "wal4" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)
        let first = [| Entry.put (sstKey 1) 1UL (valOf 1) |]
        let second = [| Entry.put (sstKey 2) 2UL (valOf 2) |]

        let wA =
            ok (Wal.append w0 dir [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, first); WalRecord.Commit 1UL ])

        let w1 =
            ok (Wal.append w0 dir [ WalRecord.Begin 2UL; WalRecord.Batch(2UL, second); WalRecord.Commit 2UL ])

        // 2 つ目のレコードのバイトを 1 つだけ壊す
        let path = Wal.path dir w1.FileNumber
        let bytes = File.ReadAllBytes(path)
        bytes.[bytes.Length - 1] <- bytes.[bytes.Length - 1] ^^^ 0xFFuy
        File.WriteAllBytes(path, bytes)

        // 壊れた 2 つ目で打ち切られ、1 つ目だけ残る
        Assert.Equal(1, (ok (Wal.read dir 1L)).Length))

[<Fact>]
let ``WAL: rotate で番号が進み、新しいファイルへ追記できる`` () =
    withDir "wal-rotate" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)
        let first = [| Entry.put (sstKey 1) 1UL (valOf 1) |]

        let w1 =
            ok (Wal.append w0 dir [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, first); WalRecord.Commit 1UL ])

        // append は書き込んだバイト数を Size に積む
        Assert.True(w1.Size > w0.Size, "append しても Size が増えていない")

        let w2 = Wal.rotate w1
        Assert.Equal(2L, w2.FileNumber)
        Assert.Equal(0, w2.Size)

        let second = [| Entry.put (sstKey 2) 2UL (valOf 2) |]

        let _w3 =
            ok (Wal.append w2 dir [ WalRecord.Begin 2UL; WalRecord.Batch(2UL, second); WalRecord.Commit 2UL ])

        // 古い WAL はそのまま残り、新しい WAL にだけ新しい内容が入る
        Assert.Equal(1, (ok (Wal.read dir 1L)).Length)
        Assert.Equal(1, (ok (Wal.read dir 2L)).Length)
        Assert.Equal(2, (Wal.walFiles dir).Length)

        // 存在しない番号は空として読める
        Assert.Empty(ok (Wal.read dir 3L)))

[<Fact>]
let ``WAL: 複数トランザクションはコミットした分だけ、コミット順に再生される`` () =
    withDir "wal-txn" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)

        let t1 = [| Entry.put (sstKey 1) 1UL (valOf 1) |]
        let t2 = [| Entry.put (sstKey 2) 2UL (valOf 2) |]
        let t3 = [| Entry.put (sstKey 3) 3UL (valOf 3) |]

        // t1 は中断、t2 と t3 はコミット。記録は Begin/Batch が混在する
        let _w =
            ok (
                Wal.append
                    w0
                    dir
                    [
                        WalRecord.Begin 1UL
                        WalRecord.Batch(1UL, t1)
                        WalRecord.Begin 2UL
                        WalRecord.Batch(2UL, t2)
                        WalRecord.Begin 3UL
                        WalRecord.Batch(3UL, t3)
                        WalRecord.Abort 1UL
                        WalRecord.Commit 2UL
                        WalRecord.Commit 3UL
                    ]
            )

        // コミット済みの t2, t3 だけがコミット順に並ぶ
        let recovered = ok (Wal.read dir 1L)
        Assert.Equal<uint64 array>([| 2UL; 3UL |], recovered |> Array.map (fun e -> e.SeqNo)))

[<Fact>]
let ``WAL: readRaw は未コミットも含めた生の記録を返す`` () =
    withDir "wal-raw" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)
        let entries = [| Entry.put (sstKey 1) 1UL (valOf 1) |]

        let w1 =
            ok (Wal.append w0 dir [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, entries); WalRecord.Commit 1UL ])

        // コミットしないトランザクションを追記する
        let _w2 =
            ok (Wal.append w1 dir [ WalRecord.Begin 2UL; WalRecord.Batch(2UL, entries) ])

        // 再生対象はコミット済みの 1 件だけ
        Assert.Equal(1, (ok (Wal.read dir 1L)).Length))

[<Fact>]
let ``WAL: 同じ番号で作り直すと内容を捨ててやり直す`` () =
    withDir "wal-recreate" (fun dir ->
        ok (Manifest.ensureDirectory dir)
        let w0 = ok (Wal.create dir 1L)
        let entries = [| Entry.put (sstKey 1) 1UL (valOf 1) |]

        let _w =
            ok (Wal.append w0 dir [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, entries); WalRecord.Commit 1UL ])

        Assert.Equal(1, (ok (Wal.read dir 1L)).Length)

        // 同じ番号で create し直すと必ず空になる（使い回しの防御）
        let _w2 = ok (Wal.create dir 1L)
        Assert.Empty(ok (Wal.read dir 1L)))
