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

    /// 書き込み口。
    type private Sink = Sink of System.IO.Stream

    /// run ファイル 1本を作る（受入中の行を整列して書き出す）。
    let writeRun
        (stamp: string)
        (serial: int)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (rows: SqlValue array array)
        =
        let runPath stamp serial =
            System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "%s%s-%d%s" Prefix stamp serial RunExt)

        let openSink path =
            protect (fun () ->
                let fs =
                    new System.IO.FileStream(
                        path,
                        System.IO.FileMode.Create,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.None
                    )

                Sink(new System.IO.BufferedStream(fs, 65536) :> System.IO.Stream))

        let frame row =
            let payload = ValueCodec.writeValues row Writer.Empty |> Writer.toArray
            Writer.build [ Writer.varint (uint64 payload.Length); Writer.writeBytes payload ]

        let writeRow sink row =
            let (Sink s) = sink
            let bytes = frame row
            protect (fun () -> s.Write(bytes, 0, bytes.Length))

        let rec writeAll sink (rows: SqlValue array array) i =
            if i >= rows.Length then
                ret ()
            else
                match writeRow sink rows.[i] with
                | Error e -> Error e
                | Ok() -> writeAll sink rows (i + 1)

        let closeSink sink =
            protect (fun () ->
                let (Sink s) = sink
                s.Dispose())
            |> ignore

        let path = runPath stamp serial

        match openSink path with
        | Error e -> Error e
        | Ok sink ->
            let sorted = Array.sortWith compareRows rows

            match writeAll sink sorted 0 with
            | Error e ->
                closeSink sink
                unlink path
                Error e
            | Ok() ->
                closeSink sink
                ret path

    /// 読み込み口。
    type internal Source = Source of System.IO.Stream

    /// 1行読む。
    /// None は終端。
    let readRow (src: Source) =
        let rec loop (s: System.IO.Stream) shift acc lead =
            if shift > 63 then
                err (DbError.corruption "Spill: varint が長すぎます")
            else
                let b = lead

                if b &&& 0x80uy <> 0uy then
                    if shift >= 63 then
                        err (DbError.corruption "Spill: varint が長すぎます")
                    else
                        let next = s.ReadByte()

                        if next < 0 then
                            err (DbError.corruption "Spill: run ファイルが途中で終わりました")
                        else
                            loop s (shift + 7) (acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)) (byte next)
                else
                    ret (acc ||| (uint64 (b &&& 0x7Fuy) <<< shift))

        let readVarintFrom (s: System.IO.Stream) first = loop s 0 0UL first

        let rec fill (s: System.IO.Stream) buffer n pos =
            if pos >= n then
                ret buffer
            else
                let got = s.Read(buffer, pos, n - pos)

                if got <= 0 then
                    err (DbError.corruption "Spill: run ファイルが途中で終わりました")
                else
                    fill s buffer n (pos + got)

        let readExactly (s: System.IO.Stream) n =
            let buffer = Array.zeroCreate<byte> n
            fill s buffer n 0

        let (Source s) = src

        protect (fun () ->
            let first = s.ReadByte()

            if first < 0 then
                Ok None
            else
                match readVarintFrom s (byte first) with
                | Error e -> Error e
                | Ok len when len > uint64 System.Int32.MaxValue -> err (DbError.corruption "Spill: 行の長さが異常です")
                | Ok len ->
                    match readExactly s (int len) with
                    | Error e -> Error e
                    | Ok payload ->
                        match ValueCodec.readValues (Cursor.create payload) with
                        | Error e -> Error e
                        | Ok(values, _) -> Ok(Some values))
        |> Result.bind id

    /// run の読み込み状態（先頭 1行を先読みしている）。
    type internal RunState =
        {
            Path: string
            Src: Source
            Head: SqlValue array option
        }

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

        match openSource path with
        | Error e -> Error e
        | Ok src ->
            match readRow src with
            | Error e ->
                closeSource src
                unlink path
                Error e
            | Ok head -> Ok { Path = path; Src = src; Head = head }

    /// run を閉じてファイルを消す（尽きた・破棄するとき）。
    /// ストリームを閉じずにファイルだけ消すとハンドルが洩れるので、必ず一緒に閉じる。
    let closeRun (state: RunState) =
        closeSource state.Src
        unlink state.Path

    /// 複数の run をまとめて閉じて消す。
    let closeAll (states: RunState list) = states |> List.iter closeRun

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
            | [] -> Ok(List.rev acc)
            | p :: tail ->
                match Spill.openRun p with
                | Error e ->
                    Spill.closeAll acc
                    Spill.forget rest
                    Error e
                | Ok state -> openAll tail (state :: acc)

        let rec scan i bestIdx best (states: Spill.RunState list) =
            match states with
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

        let rec pull states =
            let idx = scan 0 -1 None states

            if idx < 0 then
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
                            Error e
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

        let approxRow row =
            row |> Array.sumBy ValueCodec.approximateBytes

        let rec ingest stamp buf bufBytes written paths state next =
            match Op.next next with
            | Error e ->
                Spill.forget paths
                Error e
            | Ok None ->
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
                        Error e
                    | Ok runPaths ->
                        match openAll runPaths [] with
                        | Error e ->
                            Spill.forget runPaths
                            Error e
                        | Ok states -> ret (mergeRuns states)
            | Ok(Some(row, tail)) ->
                match check state row with
                | Error e ->
                    Spill.forget paths
                    Error e
                | Ok state' ->
                    let size = int64 (approxRow row)

                    if buf.Count > 0 && bufBytes + size > budgetBytes then
                        match Spill.writeRun stamp written compareRows (buf.ToArray()) with
                        | Error e ->
                            Spill.forget paths
                            Error e
                        | Ok p ->
                            buf.Clear()
                            ingest stamp buf 0L (written + 1) (p :: paths) state' (Op.Step(row, tail))
                    else
                        buf.Add row
                        ingest stamp buf (bufBytes + size) written paths state' tail

        Spill.sweepStale ()
        let stamp = System.Guid.NewGuid().ToString "N"
        ingest stamp (ResizeArray<SqlValue array>()) 0L 0 [] init src
