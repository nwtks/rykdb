// ===========================================================================
// Wal.fs（書き込みログ）
// ===========================================================================

module RykDb.WalTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.TestHelpers

/// ディレクトリを用意して WAL を 1つ作る（ディスク経路のテスト用）。
let private freshWal (dir: string) =
    ok (Manifest.ensureDirectory dir)
    ok (Wal.create dir 1L)

/// `Begin / Batch / Commit` の 3レコード（1トランザクション分）。
let private commitRecords (txnId: uint64) (entries: Entry array) =
    [
        WalRecord.Begin txnId
        WalRecord.Batch(txnId, entries)
        WalRecord.Commit txnId
    ]

/// ディスクを介さずにレコード列を往復させる。
let private roundTrip (records: WalRecord list) =
    records |> Wal.encodeFrames |> Wal.decodeFrames |> Wal.committedEntries

[<Fact>]
let ``Wal: 往復する`` () =
    let entries =
        [|
            Entry.put (intKey 1) 1UL (intRow 1)
            Entry.put (intKey 2) 2UL (intRow 2)
            Entry.delete (intKey 3) 3UL
        |]

    let records =
        commitRecords 1UL entries @ [ WalRecord.Begin 2UL; WalRecord.Abort 2UL ]

    // 中断した transaction は捨てられ、コミットした分だけ残る
    let recovered = roundTrip records
    Assert.Equal(3, recovered.Length)
    Assert.Equal<uint64>(1UL, recovered.[0].SeqNo)
    Assert.Equal<uint64>(3UL, recovered.[2].SeqNo)
    Assert.True(recovered.[2].IsDelete)

[<Fact>]
let ``Wal.decodeFrames: 書きかけレコードは読み飛ばす`` () =
    let entries = [| Entry.put (intKey 1) 1UL (intRow 1) |]

    // 末尾に壊れたレコードを書き足す（torn write の再現）
    let data =
        Array.append
            (Wal.encodeFrames (commitRecords 1UL entries))
            [| 0x10uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    // 壊れたレコードは打ち切られるが、それまでのものは読める
    let recovered = data |> Wal.decodeFrames |> Wal.committedEntries
    Assert.Equal(1, recovered.Length)

[<Fact>]
let ``Wal.decodeFrames: チェックサム不一致は打ち切る`` () =
    let first = [| Entry.put (intKey 1) 1UL (intRow 1) |]
    let second = [| Entry.put (intKey 2) 2UL (intRow 2) |]

    // 2つ目のレコードのバイトを 1つだけ壊す
    let secondFrame = Wal.encodeFrames (commitRecords 2UL second)
    secondFrame.[secondFrame.Length - 1] <- secondFrame.[secondFrame.Length - 1] ^^^ 0xFFuy

    let data = Array.append (Wal.encodeFrames (commitRecords 1UL first)) secondFrame

    // 壊れた 2つ目で打ち切られ、1つ目だけ残る
    let recovered = data |> Wal.decodeFrames |> Wal.committedEntries
    Assert.Equal(1, recovered.Length)

[<Fact>]
let ``Wal.decodeFrames: 不明なレコード種別は打ち切る`` () =
    let entries = [| Entry.put (intKey 1) 1UL (intRow 1) |]

    // フレームとしては正しい（長さも checksum も合っている）が、種別が未知
    let payload = [| 0x7Fuy |]

    let frame =
        Writer.build
            [
                Writer.uint32LE (uint32 payload.Length)
                Writer.uint32LE (Hashing.checksum payload)
                Writer.writeBytes payload
            ]

    let data = Array.append (Wal.encodeFrames (commitRecords 1UL entries)) frame

    // 不明レコードで打ち切られ、それまでの 1 件は読める
    let recovered = data |> Wal.decodeFrames |> Wal.committedEntries
    Assert.Equal(1, recovered.Length)

[<Fact>]
let ``Wal.decodeFrames: 長さが Int32 を超えるフレームは打ち切る`` () =
    let entries = [| Entry.put (intKey 1) 1UL (intRow 1) |]

    // 長さ 0xFFFFFFFF は int で負になる（不正なフレーム）
    let data =
        Array.append
            (Wal.encodeFrames (commitRecords 1UL entries))
            [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    let recovered = data |> Wal.decodeFrames |> Wal.committedEntries
    Assert.Equal(1, recovered.Length)

[<Fact>]
let ``Wal.committedEntries: 未コミットは再生しない`` () =
    let entries = [| Entry.put (intKey 9) 1UL (intRow 9) |]

    let recovered =
        Wal.committedEntries [ WalRecord.Begin 1UL; WalRecord.Batch(1UL, entries) ]

    Assert.Empty recovered

[<Fact>]
let ``Wal.committedEntries: 複数トランザクションはコミットした分だけ、コミット順に再生される`` () =
    let t1 = [| Entry.put (intKey 1) 1UL (intRow 1) |]
    let t2 = [| Entry.put (intKey 2) 2UL (intRow 2) |]
    let t3 = [| Entry.put (intKey 3) 3UL (intRow 3) |]

    // t1 は中断、t2 と t3 はコミット。記録は Begin/Batch が混在する
    let recovered =
        Wal.committedEntries
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

    // コミット済みの t2, t3 だけがコミット順に並ぶ
    Assert.Equal<uint64 array>([| 2UL; 3UL |], recovered |> Array.map (fun e -> e.SeqNo))

[<Fact>]
let ``Wal.read: 同じ番号で作り直すと内容を捨ててやり直す`` () =
    withDir "wal-recreate" (fun dir ->
        let w0 = freshWal dir
        let entries = [| Entry.put (intKey 1) 1UL (intRow 1) |]
        let _w = ok (Wal.append w0 dir (commitRecords 1UL entries))
        Assert.Equal(1, (ok (Wal.read dir 1L)).Length)

        // 同じ番号で create し直すと必ず空になる（使い回しの防御）
        let _w2 = ok (Wal.create dir 1L)
        Assert.Empty(ok (Wal.read dir 1L)))

[<Fact>]
let ``Wal.walFiles: 存在しないディレクトリの walFiles は空`` () =
    let missing =
        Path.Combine(Path.GetTempPath(), "rykdb-wal-missing-" + string System.DateTime.UtcNow.Ticks)

    Assert.Empty(Wal.walFiles missing)

[<Fact>]
let ``Wal.rotate: rotate で番号が進み、新しいファイルへ追記できる`` () =
    withDir "wal-rotate" (fun dir ->
        let w0 = freshWal dir
        let first = [| Entry.put (intKey 1) 1UL (intRow 1) |]
        let w1 = ok (Wal.append w0 dir (commitRecords 1UL first))
        // append は書き込んだバイト数を Size に積む
        Assert.True(w1.Size > w0.Size, "append しても Size が増えていない")

        let w2 = Wal.rotate w1
        Assert.Equal(2L, w2.FileNumber)
        Assert.Equal(0, w2.Size)

        let second = [| Entry.put (intKey 2) 2UL (intRow 2) |]
        let _w3 = ok (Wal.append w2 dir (commitRecords 2UL second))
        // 古い WAL はそのまま残り、新しい WAL にだけ新しい内容が入る
        Assert.Equal(1, (ok (Wal.read dir 1L)).Length)
        Assert.Equal(1, (ok (Wal.read dir 2L)).Length)
        Assert.Equal(2, (Wal.walFiles dir).Length)
        // 存在しない番号は空として読める
        Assert.Empty(ok (Wal.read dir 3L)))
