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
module internal Spill =
    /// 1行の枠: [varint 長][ValueCodec で符号化した値の並び]。
    let private frame (row: SqlValue array) =
        let payload = ValueCodec.writeValues row Writer.empty |> Writer.toArray
        Writer.build [ Writer.varint (uint64 payload.Length); Writer.writeBytes payload ]

    /// 一時ファイルのプレフィックス（掃除の対象をこれで見分ける）。
    let private prefix = "rykdb-sort-"

    /// 一時ファイルのパスを作る。
    let private runPath (stamp: string) (serial: int) =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "%s%s-%d.run" prefix stamp serial)

    /// 24時間を超えて残った run を回収する（途中打ち切りで放置された分）。
    /// 実行中の run は 24時間を超えない前提で、年齢で見分ける。
    let sweepStale () =
        protect (fun () ->
            let dir = System.IO.Path.GetTempPath()

            if System.IO.Directory.Exists dir then
                let cutoff = System.DateTime.UtcNow.AddHours -24.0

                System.IO.Directory.GetFiles(dir, prefix + "*.run")
                |> Array.iter (fun p ->
                    if System.IO.FileInfo(p).CreationTimeUtc < cutoff then
                        protect (fun () -> System.IO.File.Delete p) |> ignore))
        |> ignore

    /// 書き込み口。
    type Sink = private Sink of System.IO.Stream

    let private openSink (path: string) =
        protect (fun () ->
            let fs =
                new System.IO.FileStream(
                    path,
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.None
                )

            Sink(new System.IO.BufferedStream(fs, 65536) :> System.IO.Stream))

    let private closeSink (sink: Sink) =
        protect (fun () ->
            let (Sink s) = sink
            s.Dispose())
        |> ignore

    /// 1行を追記する。
    let writeRow (sink: Sink) (row: SqlValue array) =
        let (Sink s) = sink
        let bytes = frame row
        protect (fun () -> s.Write(bytes, 0, bytes.Length))

    /// 読み込み口。
    type Source = private Source of System.IO.Stream

    let private openSource (path: string) =
        protect (fun () ->
            let fs = System.IO.File.OpenRead path
            Source(new System.IO.BufferedStream(fs, 65536) :> System.IO.Stream))

    let private closeSource (src: Source) =
        protect (fun () ->
            let (Source s) = src
            s.Dispose())
        |> ignore

    /// run ファイルを消す（無ければ何もしない）。
    let private unlink (path: string) =
        protect (fun () ->
            if System.IO.File.Exists path then
                System.IO.File.Delete path)
        |> ignore

    /// 残っている run を全部消す（エラー経路の後始末）。
    let forget (paths: string list) = paths |> List.iter unlink

    /// run の読み込み状態（先頭 1行を先読みしている）。
    type RunState =
        {
            Path: string
            Src: Source
            Head: SqlValue array option
        }

    /// run を閉じてファイルを消す（尽きた・破棄するとき）。
    /// ストリームを閉じずにファイルだけ消すとハンドルが洩れるので、必ず一緒に閉じる。
    let closeRun (state: RunState) =
        closeSource state.Src
        unlink state.Path

    /// 複数の run をまとめて閉じて消す。
    let closeAll (states: RunState list) = states |> List.iter closeRun

    /// varint を読む（先頭バイトはすでに読んでいる）。
    let private readVarintFrom (s: System.IO.Stream) (first: byte) =
        let rec loop (shift: int) (acc: uint64) (lead: byte) =
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
                            loop (shift + 7) (acc ||| (uint64 (b &&& 0x7Fuy) <<< shift)) (byte next)
                else
                    ret (acc ||| (uint64 (b &&& 0x7Fuy) <<< shift))

        loop 0 0UL first

    /// 指定バイト数をちょうど読む。
    let private readExactly (s: System.IO.Stream) (n: int) =
        let buffer = Array.zeroCreate<byte> n

        let rec fill (pos: int) =
            if pos >= n then
                ret buffer
            else
                let got = s.Read(buffer, pos, n - pos)

                if got <= 0 then
                    err (DbError.corruption "Spill: run ファイルが途中で終わりました")
                else
                    fill (pos + got)

        fill 0

    /// 1行読む。
    /// None は終端。
    let readRow (src: Source) =
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

    /// run ファイル 1本を作る（受入中の行を整列して書き出す）。
    let writeRun
        (stamp: string)
        (serial: int)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (rows: SqlValue array array)
        =
        let path = runPath stamp serial

        match openSink path with
        | Error e -> Error e
        | Ok sink ->
            let sorted = Array.sortWith compareRows rows

            let rec writeAll (i: int) =
                if i >= sorted.Length then
                    ret ()
                else
                    match writeRow sink sorted.[i] with
                    | Error e -> Error e
                    | Ok() -> writeAll (i + 1)

            match writeAll 0 with
            | Error e ->
                closeSink sink
                unlink path
                Error e
            | Ok() ->
                closeSink sink
                ret path

    /// run ファイルを開いて先頭 1行を読む。
    let openRun (path: string) =
        match openSource path with
        | Error e -> Error e
        | Ok src ->
            match readRow src with
            | Error e ->
                closeSource src
                unlink path
                Error e
            | Ok head -> Ok { Path = path; Src = src; Head = head }

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

    /// 1行の概算バイト数。
    let private approxRow (row: SqlValue array) =
        row |> Array.sumBy ValueCodec.approximateBytes

    /// 整列済みの受け皿を Op として返す。
    let private inMemory (rows: ResizeArray<SqlValue array>) (compareRows: SqlValue array -> SqlValue array -> int) =
        rows.ToArray() |> Array.sortWith compareRows |> Op.ofArray

    /// run を k-way merge して遅延 Op にする。
    let private mergeRuns (compareRows: SqlValue array -> SqlValue array -> int) (states: Spill.RunState list) =
        let rec scan (i: int) (bestIdx: int) (best: SqlValue array option) (xs: Spill.RunState list) =
            match xs with
            | [] -> bestIdx
            | s :: tail ->
                match s.Head with
                | None -> scan (i + 1) bestIdx best tail
                | Some h ->
                    match best with
                    | Some b when compareRows h b >= 0 -> scan (i + 1) bestIdx best tail
                    | _ -> scan (i + 1) i (Some h) tail

        let rec splitAt (n: int) (xs: Spill.RunState list) (acc: Spill.RunState list) =
            if n <= 0 then
                List.rev acc, xs
            else
                match xs with
                | [] -> List.rev acc, []
                | h :: tail -> splitAt (n - 1) tail (h :: acc)

        let rec pull (xs: Spill.RunState list) : Result<(SqlValue array * Spill.RunState list) option, DbError> =
            let idx = scan 0 -1 None xs

            if idx < 0 then
                ret None
            else
                let before, chosenAndAfter = splitAt idx xs []

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

        let rec emit (xs: Spill.RunState list) =
            Op.Compute(fun () ->
                match pull xs with
                | Error e -> err e
                | Ok None -> ret None
                | Ok(Some(row, rest)) -> ret (Some(row, emit rest)))

        emit states

    /// 入力を整列して返す。
    /// バジェット内ならメモリ、超過分は run として溢らして外部マージする。
    let sortRows
        (budgetBytes: int64)
        (init: 'S)
        (check: 'S -> SqlValue array -> Result<'S, DbError>)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (src: Op)
        =
        Spill.sweepStale ()
        let stamp = System.Guid.NewGuid().ToString "N"

        let rec ingest
            (buf: ResizeArray<SqlValue array>)
            (bufBytes: int64)
            (written: int)
            (paths: string list)
            (state: 'S)
            (next: Op)
            =
            match Op.next next with
            | Error e ->
                Spill.forget paths
                Error e
            | Ok None ->
                if List.isEmpty paths then
                    ret (inMemory buf compareRows)
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
                        let rec openAll (rest: string list) (acc: Spill.RunState list) =
                            match rest with
                            | [] -> Ok(List.rev acc)
                            | p :: tail ->
                                match Spill.openRun p with
                                | Error e ->
                                    Spill.closeAll acc
                                    Spill.forget rest
                                    Error e
                                | Ok state -> openAll tail (state :: acc)

                        match openAll runPaths [] with
                        | Error e ->
                            Spill.forget runPaths
                            Error e
                        | Ok states -> ret (mergeRuns compareRows states)
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
                            ingest buf 0L (written + 1) (p :: paths) state' (Op.Step(row, tail))
                    else
                        buf.Add row
                        ingest buf (bufBytes + size) written paths state' tail

        ingest (ResizeArray<SqlValue array>()) 0L 0 [] init src
