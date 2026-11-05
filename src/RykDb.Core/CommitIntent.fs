namespace RykDb.Core

/// keyspace を跨ぐ commit の意思決定ログ（追記型ジャーナル）。
/// <remarks>
/// keyspace ごとに WAL が分かれているため、複数の keyspace へ書き込む途中でクラッシュすると、
/// テーブルだけ更新されて索引が更新されない状態になりうる。
/// この不整合を回復するため、`.commit/journal` に適用開始（publish）と適用完了（resolve）を記録する。
/// - publish は既存ファイルへの追記と `Flush(true)` で永続化する。ディレクトリエントリは更新しない。
///   .NET 標準機能ではディレクトリを fsync できず、クロスプラットフォーム方針により P/Invoke も使わないため、
///   rename や delete に依存しない。ジャーナルはオープン時に作成しておくので、publish ごとの新規作成もない。
/// - レコードは `[長さ][ペイロード][チェックサム]` のフレームである。
///   追記が途中で終わった末尾はチェックサムで検出し、未公開として破棄する。
///   publish の書き込みが完了するまで WAL には書き込まないため、この末尾は安全に捨てられる。
/// - WAL への適用が終わったら resolve を追記する。resolve 済みの publish は再生しない。
/// - WAL への再適用は冪等である（`applyBatchesCore idempotent = true`）。
///   resolve の追記に失敗して publish が残っても、再起動後の適用では既に書かれた項目を読み飛ばす。
/// </remarks>
module internal CommitIntent =
    /// ジャーナルをこれ以上伸ばさない閾値（バイト）。
    /// 超えていて「未解決ゼロ」のときだけ間引く（公開記録は消さない）。
    [<Literal>]
    let private CompactThresholdBytes = 262144L

    /// 1フレームの最大長（異常な長さを弾く）。
    [<Literal>]
    let private MaxFrameBytes = 268435456L

    /// レコード種別。
    [<Literal>]
    let private TagPublished = 1uy

    [<Literal>]
    let private TagResolved = 2uy

    /// ジャーナルの 1レコード。
    type private Frame =
        /// 公開（適用開始の意思表示）。id は maxSeq + GUID。
        | Published of string * (string * Entry array) list
        /// 解決（適用完了）。
        | Resolved of string

    let private dir (root: string) = System.IO.Path.Combine(root, ".commit")

    let private journalPath (root: string) =
        System.IO.Path.Combine(dir root, "journal")

    /// 1バッチ分の読み取り。
    let private readBatchItem (c: Cursor) =
        result {
            let! ks, c1 = Reader.readStr c
            let! batchBytes, c2 = Reader.readLengthPrefixed c1
            let! _, entries = Entry.decodeBatch batchBytes
            return (ks, entries), c2
        }

    let private encodePublish (ident: string) (batches: (string * Entry array) list) =
        let steps =
            [
                yield Writer.writeByte TagPublished
                yield Writer.str ident
                yield Writer.varint (uint64 batches.Length)

                for ks, entries in batches do
                    yield Writer.str ks
                    yield Writer.lengthPrefixed (Entry.encodeBatch 0UL entries)
            ]

        Writer.build steps

    let private encodeResolve (ident: string) =
        Writer.build [ Writer.writeByte TagResolved; Writer.str ident ]

    let private decodeFrame (data: byte array) =
        result {
            let c = Cursor.create data
            let! tag, c1 = Reader.readByte c

            match tag with
            | TagPublished ->
                let! ident, c2 = Reader.readStr c1
                let! count, c3 = Reader.readVarint c2

                if count > uint64 System.Int32.MaxValue then
                    return! err (DbError.corruption "CommitIntent: バッチ数が大きすぎます")
                else
                    let! batches, _ = Reader.readItems readBatchItem count c3
                    return Published(ident, batches)
            | TagResolved ->
                let! ident, _ = Reader.readStr c1
                return Resolved ident
            | other -> return! err (sprintf "CommitIntent: 未知のレコード種別 %d" other |> DbError.corruption)
        }

    /// フレーム 1個分のバイト列（[長さ][ペイロード][チェックサム]）。
    let private frameBytes (payload: byte array) =
        let head =
            Writer.build [ Writer.uint32LE (uint32 payload.Length); Writer.writeBytes payload ]

        let crc = Hashing.checksum head
        Array.append head (Writer.build [ Writer.uint32LE crc ])

    /// フレーム 1個を読む。
    /// `None` は「クリーンな終端」または「直前の追記が途切れた尾部」（両方とも捨てて安全）。
    let private readFrame (s: System.IO.Stream) =
        let rec fill buf have want =
            if have >= want then
                ret true
            else
                let got = s.Read(buf, have, want - have)
                if got <= 0 then ret false else fill buf (have + got) want

        let header = Array.zeroCreate<byte> 4
        let first = s.Read(header, 0, 4)

        if first < 4 then
            ret None
        else
            let len = uint64 (Bytes.readLE32 header 0)

            if len > uint64 MaxFrameBytes then
                ret None
            else
                let payload = Array.zeroCreate<byte>(int len)

                match fill payload 0 (int len) with
                | Error e -> err e
                | Ok false -> ret None
                | Ok true ->
                    let crcBytes = Array.zeroCreate<byte> 4

                    match fill crcBytes 0 4 with
                    | Error e -> err e
                    | Ok false -> ret None
                    | Ok true ->
                        let stored = Bytes.readLE32 crcBytes 0
                        let body = Array.append header payload

                        if Hashing.checksum body <> stored then
                            ret None
                        else
                            decodeFrame payload |> Result.map Some

    /// ジャーナルを読んで全フレームを返す。
    /// 返り値は (フレーム, 有効な末尾バイト位置)。尾部の途切れは読み飛ばす。
    let private readFrames (path: string) =
        protect (fun () ->
            use s = System.IO.File.OpenRead path

            let rec loop (acc: Frame list) =
                match readFrame s with
                | Error e -> err e
                | Ok None -> ret (List.rev acc, s.Position)
                | Ok(Some frame) -> loop (frame :: acc)

            loop [])
        >>= id

    /// ジャーナルの追記・間引きを直列化する（root ごとではなくプロセス全体で 1本）。
    /// 書き込みは `RootGate` の共有ロック下で走るので、複数スレッドが同時に commit し得る。
    let private Gate = new System.Threading.SemaphoreSlim(1, 1)

    /// フレームを 1つ追記してディスクへ書き込む。
    let private appendFrame (root: string) (payload: byte array) =
        let frame = frameBytes payload
        Gate.Wait()

        try
            protect (fun () ->
                System.IO.Directory.CreateDirectory(dir root) |> ignore

                use fs =
                    new System.IO.FileStream(
                        journalPath root,
                        System.IO.FileMode.Append,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.Read
                    )

                fs.Write(frame, 0, frame.Length)
                fs.Flush true)
        finally
            Gate.Release() |> ignore

    /// ジャーナルを先に作っておく。
    /// 作成はディレクトリを 1回だけ汚す。
    /// 以後の公開は「すでにあるファイルへの追記」になり、ディレクトリエントリを更新しなくなる
    /// （標準機能でディレクトリを fsync できないため）。
    /// 呼び出し側は **ProcessLock を取得してから** 呼ぶこと。
    /// 別プロセスが publish / resolve を追記している最中に尾部を切り詰めないため。
    /// <remarks>
    /// 途中で切れた尾部があれば**切り詰める**。
    /// 切れたまま後ろへ追記すると、あの尾部で読みが止まり、以後の公開が全部読めなくなるため。
    /// 切り詰めは既存ファイルの長さ変更（inode は同じ）なので、ディレクトリエントリは更新しない。
    /// </remarks>
    let touch (root: string) =
        if System.String.IsNullOrEmpty root then
            ret ()
        else
            result {
                let d = dir root
                do! protect (fun () -> System.IO.Directory.CreateDirectory d |> ignore)
                let p = journalPath root

                do!
                    protect (fun () ->
                        if not (System.IO.File.Exists p) then
                            use fs =
                                new System.IO.FileStream(
                                    p,
                                    System.IO.FileMode.CreateNew,
                                    System.IO.FileAccess.Write,
                                    System.IO.FileShare.Read
                                )

                            fs.Flush true)

                do!
                    match readFrames p with
                    | Error _ -> ret ()
                    | Ok(_, validLen) ->
                        let len =
                            match protect (fun () -> System.IO.FileInfo(p).Length) with
                            | Ok n -> n
                            | Error _ -> 0L

                        if validLen < len then
                            protect (fun () ->
                                use fs =
                                    new System.IO.FileStream(
                                        p,
                                        System.IO.FileMode.Open,
                                        System.IO.FileAccess.Write,
                                        System.IO.FileShare.Read
                                    )

                                fs.SetLength validLen
                                fs.Flush true)
                        else
                            ret ()

                return ()
            }

    /// 未解決が無いときだけジャーナルを間引く（閾値超えのとき）。
    /// <remarks>
    /// すでにあるファイルを上書き（truncate）するだけなので、ディレクトリエントリは更新しない。
    /// 間引くのは「未解決ゼロ」のときだけ。
    /// 公開記録を消すと、適用前の commit が再生できなくなる。
    /// 失敗しても致命的ではない（次回のオープンで挑戦する）ので握り潰す。
    /// </remarks>
    let trimIfIdle (root: string) =
        if not (System.String.IsNullOrEmpty root) then
            let p = journalPath root

            let oversize =
                match protect (fun () -> System.IO.FileInfo(p).Length) with
                | Ok len -> len >= CompactThresholdBytes
                | Error _ -> false

            if oversize then
                Gate.Wait()

                try
                    match readFrames p with
                    | Error _ -> ()
                    | Ok(frames, _) ->
                        let solved =
                            frames
                            |> List.choose (function
                                | Resolved id -> Some id
                                | Published _ -> None)
                            |> Set.ofList

                        let unresolved =
                            frames
                            |> List.exists (function
                                | Published(id, _) -> not (Set.contains id solved)
                                | Resolved _ -> false)

                        if not unresolved then
                            protect (fun () ->
                                use fs =
                                    new System.IO.FileStream(
                                        p,
                                        System.IO.FileMode.Create,
                                        System.IO.FileAccess.Write,
                                        System.IO.FileShare.Read
                                    )

                                fs.Flush true)
                            |> ignore
                finally
                    Gate.Release() |> ignore

    /// 公開レコードを追記する。
    /// 返り値は解決に使う id。
    /// <remarks>
    /// ここではまだ WAL に何も書かない。
    /// フレームが完成して `Flush(true)` が返った時点で初めて
    /// 「この commit は開始した」と外部から見える。
    /// 途中で切れた尾部はフレーム不完全として捨てられる。
    /// </remarks>
    let publish (root: string) (batches: (string * Entry array) list) =
        let maxSeq =
            batches
            |> List.collect (fun (_, entries) -> Array.toList entries)
            |> List.map _.SeqNo
            |> List.max

        let ident =
            sprintf "%s-%s" (maxSeq.ToString "D20") (System.Guid.NewGuid().ToString "N")

        appendFrame root (encodePublish ident batches) |> Result.map (fun () -> ident)

    /// 解決レコードを追記する（適用完了の記録）。
    /// 失敗を握り潰すと、その後に同じキーが更新・GC されたあとの再起動で
    /// 古い intent が再適用され得るので、エラーとして返す
    /// （再生は冪等なので、それでも安全側には倒れる）。
    let resolve (root: string) (ident: string) = appendFrame root (encodeResolve ident)

    /// 未解決の intent を返す（ジャーナルの公開 − 解決）。
    /// 尾部の途切れは「公開未完了」として読み飛ばされる。
    /// (id, batches) を返し、解決には同じ id を使う。
    let pending (root: string) =
        let p = journalPath root

        if System.IO.File.Exists p then
            result {
                let! frames, _ = readFrames p

                let solved =
                    frames
                    |> List.choose (function
                        | Resolved id -> Some id
                        | Published _ -> None)
                    |> Set.ofList

                return
                    frames
                    |> List.choose (function
                        | Published(id, batches) when not (Set.contains id solved) -> Some(id, batches)
                        | _ -> None)
            }
        else
            ret []
