// ===========================================================================
// Spill / ExtSort — SQL レベルの行を一時ファイルへ溢らせる外部マージソート。
//
// ソートは入力全体をメモリに載せられない。
// そこで次のように処理する。
//
//   1. 受け皿（バジェット）に収まるうちはメモリに溜める
//   2. バジェットを超えたら「今の受け皿を整列して 1本の run ファイルへ吐く」
//   3. 全部読み終えたら run を k-way merge して 1本の有序列を得る
//
// メモリ使用量は「受け皿 1枚 + run ごとの先頭 1行」に比例し、
// 入力全体のサイズには比例しない。
// run は [varint 長][ValueCodec 符号化] の連なりで、行は 1件ずつ遅延読みする。
//
// 一時ファイルは run が尽きた時点で削除する。
// 下流が LIMIT などで途中打ち切りした場合の残骸は、
// 次のソート開始時に 24時間を超えたものだけ回収する
// （実行中の run を誤って消さないため、年齢で判定する）。
// ===========================================================================

namespace RykDb.Core

/// 書き込み口。
type private Sink = Sink of System.IO.Stream

/// 読み込み口。
type private Source = Source of System.IO.Stream

/// run の読み込み状態（先頭 1行を先読みしている）。
type private RunState =
    {
        Path: string
        Src: Source
        Head: SqlValue array option
    }

/// 行を一時ファイルへ書き出し、そこから遅延読みする。
module private Spill =
    /// 一時ファイルのプレフィックス（掃除の対象をこれで見分ける）。
    [<Literal>]
    let private Prefix = "rykdb-sort-"

    /// 一時ファイルの拡張子。
    [<Literal>]
    let private RunExt = ".run"

    /// 24時間を超えて残った run を回収する（途中打ち切りで放置された分）。
    /// 実行中の run は 24時間を超えない前提で、年齢で見分ける。
    let sweepStale () =
        protect (fun () ->
            let dir = System.IO.Path.GetTempPath()

            if System.IO.Directory.Exists dir then
                let cutoff = System.DateTime.UtcNow.AddHours -24.0

                System.IO.Directory.GetFiles(dir, Prefix + "*" + RunExt)
                |> Array.iter (fun p ->
                    if System.IO.FileInfo(p).CreationTimeUtc < cutoff then
                        protect (fun () -> System.IO.File.Delete p) |> ignore))
        |> ignore

    /// run ファイルを消す（無ければ何もしない）。
    let private unlink (path: string) =
        protect (fun () ->
            if System.IO.File.Exists path then
                System.IO.File.Delete path)
        |> ignore

    /// 残っている run を全部消す（エラー経路の後始末）。
    let forget (paths: string list) = paths |> List.iter unlink

    /// run ファイルのパス。
    let runPath (stamp: string) (serial: int) =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "%s%s-%d%s" Prefix stamp serial RunExt)

    /// run の書き込み口を開く。
    let openSink (path: string) =
        protect (fun () ->
            let fs =
                new System.IO.FileStream(
                    path,
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.None
                )

            Sink(new System.IO.BufferedStream(fs, 65536) :> System.IO.Stream))

    /// 書き込み口を閉じる。
    let closeSink (sink: Sink) =
        protect (fun () ->
            let (Sink s) = sink
            s.Dispose())
        |> ignore

    let private frameRow (row: SqlValue seq) =
        let payload = ValueCodec.writeValues row Writer.Empty |> Writer.toArray
        Writer.build [ Writer.varint (uint64 payload.Length); Writer.writeBytes payload ]

    /// 1行を run へ書き出す。
    let writeRow (row: SqlValue seq) (sink: Sink) =
        let (Sink s) = sink
        let bytes = frameRow row
        protect (fun () -> s.Write(bytes, 0, bytes.Length))

    /// run ファイル 1本を作る（受入中の行を整列して書き出す）。
    let writeRun
        (stamp: string)
        (serial: int)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (rows: SqlValue array array)
        =
        let rec writeAll (rows: SqlValue array array) i sink =
            if i >= rows.Length then
                ret ()
            else
                match writeRow rows.[i] sink with
                | Error e -> err e
                | Ok() -> writeAll rows (i + 1) sink

        let path = runPath stamp serial

        openSink path
        >>= fun sink ->
            let sorted = Array.sortWith compareRows rows

            match writeAll sorted 0 sink with
            | Error e ->
                closeSink sink
                unlink path
                err e
            | Ok() ->
                closeSink sink
                ret path

    /// 1行読む。
    /// None は終端。
    let readRow (src: Source) =
        let rec loop (s: System.IO.Stream) shift acc lead =
            if shift > 63 then
                err (DbError.corruption "Spill: varint が長すぎます")
            else if lead &&& 0x80uy <> 0uy then
                if shift >= 63 then
                    err (DbError.corruption "Spill: varint が長すぎます")
                else
                    let next = s.ReadByte()

                    if next < 0 then
                        err (DbError.corruption "Spill: run ファイルが途中で終わりました")
                    else
                        loop s (shift + 7) (acc ||| (uint64 (lead &&& 0x7Fuy) <<< shift)) (byte next)
            else
                ret (acc ||| (uint64 (lead &&& 0x7Fuy) <<< shift))

        let readVarintFrom s = loop s 0 0UL

        let rec fill (s: System.IO.Stream) buffer n pos =
            if pos >= n then
                ret buffer
            else
                let got = s.Read(buffer, pos, n - pos)

                if got <= 0 then
                    err (DbError.corruption "Spill: run ファイルが途中で終わりました")
                else
                    fill s buffer n (pos + got)

        let readExactly s n =
            let buffer = Array.zeroCreate<byte> n
            fill s buffer n 0

        let (Source s) = src

        protect (fun () ->
            let first = s.ReadByte()

            if first < 0 then
                ret None
            else
                match readVarintFrom s (byte first) with
                | Error e -> err e
                | Ok len when len > uint64 System.Int32.MaxValue -> err (DbError.corruption "Spill: 行の長さが異常です")
                | Ok len ->
                    readExactly s (int len)
                    >>= (Cursor.create >> ValueCodec.readValues)
                    >>= fun (values, _) -> ret (Some values))
        >>= id

    let private closeSource (src: Source) =
        protect (fun () ->
            let (Source s) = src
            s.Dispose())
        |> ignore

    /// run ファイルを開いて先頭 1行を読む。
    let openRun (path: string) =
        let openSource path =
            protect (fun () ->
                let fs = System.IO.File.OpenRead path
                Source(new System.IO.BufferedStream(fs, 65536) :> System.IO.Stream))

        openSource path
        >>= fun src ->
            match readRow src with
            | Error e ->
                closeSource src
                unlink path
                err e
            | Ok head -> ret { Path = path; Src = src; Head = head }

    /// run を閉じてファイルを消す（尽きた・破棄するとき）。
    /// ストリームを閉じずにファイルだけ消すとハンドルが洩れるので、必ず一緒に閉じる。
    let closeRun (state: RunState) =
        closeSource state.Src
        unlink state.Path

    /// 複数の run をまとめて閉じて消す。
    let closeAll = List.iter closeRun

/// 外部マージソート。
/// <remarks>
/// 入力を 1行ずつ受け、バジェットを超えると run として溢らし、最後に k-way merge する。
/// 比較関数は呼び出し側（`Ops.sortBy`）が渡す。
/// `check` は「これまでの代表値 → 行 → 次の代表値」を返す不変の fold で、
/// 比較不能な型などを早い段階で Error にする（状態は値として持ち回る）。
/// </remarks>
module internal ExtSort =
    /// 受け皿の既定バジェット（概算バイト）。
    /// これ以内に収まるソートは従来どおりメモリだけで完結する。
    let DefaultBudgetBytes = 64L * 1024L * 1024L

    /// 同時に開く run 数の上限。
    /// run が多い場合は最大 fan-in 本ずつ中間 run にマージし、
    /// 最後に最大 fan-in 本を k-way merge する（file descriptor の枯渇防止）。
    [<Literal>]
    let private MaxFanIn = 32

    /// 入力を整列して返す。
    /// バジェット内ならメモリ、超過分は run として溢らして外部マージする。
    let sortRows
        (budgetBytes: int64)
        (init: 'S)
        (check: 'S -> SqlValue array -> Result<'S, DbError>)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (src: Op)
        =
        let inMemory (rows: ResizeArray<SqlValue array>) =
            rows.ToArray() |> Array.sortWith compareRows |> Op.ofArray

        let rec openAll rest acc =
            match rest with
            | [] -> ret (List.rev acc)
            | p :: tail ->
                match Spill.openRun p with
                | Error e ->
                    Spill.closeAll acc
                    Spill.forget rest
                    err e
                | Ok state -> openAll tail (state :: acc)

        let rec scan i bestIdx best =
            function
            | [] -> bestIdx
            | s :: tail ->
                match s.Head with
                | None -> scan (i + 1) bestIdx best tail
                | Some h ->
                    match best with
                    | Some b when compareRows h b >= 0 -> scan (i + 1) bestIdx best tail
                    | _ -> scan (i + 1) i (Some h) tail

        let rec splitAt n acc states =
            if n <= 0 then
                List.rev acc, states
            else
                match states with
                | [] -> List.rev acc, []
                | h :: tail -> splitAt (n - 1) (h :: acc) tail

        let rec writeMerged sink states =
            let idx = scan 0 -1 None states

            if idx < 0 then
                Spill.closeAll states
                ret ()
            else
                let before, chosenAndAfter = splitAt idx [] states

                match chosenAndAfter with
                | [] ->
                    Spill.closeAll states
                    ret ()
                | chosen :: after ->
                    match chosen.Head with
                    | None ->
                        Spill.closeRun chosen
                        writeMerged sink (before @ after)
                    | Some row ->
                        match Spill.writeRow row sink with
                        | Error e ->
                            Spill.closeAll (before @ chosenAndAfter)
                            err e
                        | Ok() ->
                            match Spill.readRow chosen.Src with
                            | Error e ->
                                Spill.closeAll (before @ chosenAndAfter)
                                err e
                            | Ok None ->
                                Spill.closeRun chosen
                                writeMerged sink (before @ after)
                            | Ok(Some nxt) -> writeMerged sink (before @ { chosen with Head = Some nxt } :: after)

        let mergeToRun stamp serial paths =
            match openAll paths [] with
            | Error e ->
                Spill.forget paths
                err e
            | Ok states ->
                let path = Spill.runPath stamp serial

                match Spill.openSink path with
                | Error e ->
                    Spill.closeAll states
                    err e
                | Ok sink ->
                    match writeMerged sink states with
                    | Error e ->
                        Spill.closeSink sink
                        Spill.forget [ path ]
                        err e
                    | Ok() ->
                        Spill.closeSink sink
                        ret path

        let rec reduceRuns stamp nextSerial known paths =
            if List.length paths <= MaxFanIn then
                ret paths
            else
                paths |> List.chunkBySize MaxFanIn |> combine stamp nextSerial known 0 []

        and combine stamp nextSerial known count outputs =
            function
            | [] ->
                let outputs = List.rev outputs
                reduceRuns stamp (nextSerial + count) (known @ outputs) outputs
            | group :: rest ->
                match mergeToRun stamp (nextSerial + count) group with
                | Error e ->
                    Spill.forget (List.concat rest)
                    Spill.forget known
                    Spill.forget outputs
                    err e
                | Ok path -> combine stamp nextSerial known (count + 1) (path :: outputs) rest

        let pull states =
            let idx = scan 0 -1 None states

            if idx < 0 then
                Spill.closeAll states
                ret None
            else
                let before, chosenAndAfter = splitAt idx [] states

                match chosenAndAfter with
                | [] -> ret None
                | chosen :: after ->
                    match chosen.Head with
                    | None -> ret None
                    | Some row ->
                        match Spill.readRow chosen.Src with
                        | Error e ->
                            Spill.closeAll (before @ chosenAndAfter)
                            err e
                        | Ok None ->
                            Spill.closeRun chosen
                            ret (Some(row, before @ after))
                        | Ok(Some nxt) ->
                            let advanced = { chosen with Head = Some nxt }
                            ret (Some(row, before @ advanced :: after))

        let rec mergeRuns states =
            Op.Compute(fun () ->
                match pull states with
                | Error e -> err e
                | Ok None -> ret None
                | Ok(Some(row, rest)) -> ret (Some(row, mergeRuns rest)))

        let finish stamp buf written paths =
            if List.isEmpty paths then
                ret (inMemory buf)
            else
                let all =
                    if buf.Count = 0 then
                        ret paths
                    else
                        Spill.writeRun stamp written compareRows (buf.ToArray())
                        |> Result.map (fun p -> paths @ [ p ])

                match all with
                | Error e ->
                    Spill.forget paths
                    err e
                | Ok runPaths ->
                    match reduceRuns stamp (written + 1) runPaths runPaths with
                    | Error e ->
                        Spill.forget runPaths
                        err e
                    | Ok reduced ->
                        match openAll reduced [] with
                        | Error e ->
                            Spill.forget reduced
                            err e
                        | Ok states -> ret (mergeRuns states)

        let approxRow = Array.sumBy ValueCodec.approximateBytes

        let rec ingest stamp buf bufBytes written paths state next =
            match Op.next next with
            | Error e ->
                Spill.forget paths
                err e
            | Ok None -> finish stamp buf written paths
            | Ok(Some(row, tail)) ->
                match check state row with
                | Error e ->
                    Spill.forget paths
                    err e
                | Ok state' ->
                    let size = int64 (approxRow row)

                    if buf.Count > 0 && bufBytes + size > budgetBytes then
                        match Spill.writeRun stamp written compareRows (buf.ToArray()) with
                        | Error e ->
                            Spill.forget paths
                            err e
                        | Ok p ->
                            buf.Clear()
                            ingest stamp buf 0L (written + 1) (p :: paths) state' (Op.Step(row, tail))
                    else
                        buf.Add row
                        ingest stamp buf (bufBytes + size) written paths state' tail

        Spill.sweepStale ()
        let stamp = System.Guid.NewGuid().ToString "N"
        ingest stamp (ResizeArray<SqlValue array>()) 0L 0 [] init src
