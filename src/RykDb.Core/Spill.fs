// ===========================================================================
// Spill — 行を一時ファイルへ退避し、遅延読みする共通基盤。
//
// ORDER BY（ExtSort）・JOIN・GROUP BY / DISTINCT の外部アルゴリズムが共有する。
// run は [varint 長][行の符号化] の連なりで、行は 1件ずつ遅延読みする。
// 行の符号化は `ValueCodec.writeValuesSparse` を使うので、
// 列マスクで `SqlValue.Unloaded` のままの値も往復できる（一時ファイル専用）。
//
// 一時ファイルは読み終えた時点で削除する。
// 下流が LIMIT などで途中打ち切りすると一部が残るので、
// 次の退避の開始時に作成から24時間を超えたファイルを回収する。
// 実行中の run は24時間を超えない前提で、作成時刻を使って残骸と区別する。
//
// 開いたファイルは `ScanScope` に登録する。
// 早期終了・エラーでも `Engine` の `Scope.DisposeAll()` が閉じる。
// ===========================================================================

namespace RykDb.Core

/// 一時ファイルの書き込み口。
[<Sealed>]
type internal Sink(stream: System.IO.Stream) =
    /// 書き込み先。
    member _.Stream = stream

    interface System.IDisposable with
        member _.Dispose() =
            protect (fun () -> stream.Dispose()) |> ignore

/// 一時ファイルの読み込み口。
[<Sealed>]
type internal Source(stream: System.IO.Stream) =
    /// 読み込み元。
    member _.Stream = stream

    interface System.IDisposable with
        member _.Dispose() =
            protect (fun () -> stream.Dispose()) |> ignore

/// run の読み込み状態（先頭 1行を先読みしている）。
type internal RunState =
    {
        Path: string
        Src: Source
        Head: SqlValue array option
    }

/// クエリが使ってよいメモリ作業領域とスピル先。
/// <remarks>
/// バイト数は管理メモリの概算で、負値は「無制限（スピルしない）」を意味する。
/// </remarks>
type internal MemoryBudget =
    {
        /// 演算子が作業用に保持してよい量（`ValueCodec.managedRowBytes` の合計）。
        Work: int64
        /// 1回のソートで受け皿に溜めてよい量。
        Sort: int64
        /// 一時ファイルの置き場所（`None` は OS の一時ディレクトリ）。
        Directory: string option
        /// このクエリの走査台帳。早期終了で開いた run を閉じて消すために使う。
        Scope: ScanScope option
    }

module internal MemoryBudget =
    /// 既定（作業 256MiB / ソート受け皿 64MiB / OS の一時ディレクトリ）。
    let Default =
        {
            Work = 256L * 1024L * 1024L
            Sort = 64L * 1024L * 1024L
            Directory = None
            Scope = None
        }

    /// 上限なし（一切スピルしない）。
    let Unlimited =
        {
            Work = -1L
            Sort = -1L
            Directory = None
            Scope = None
        }

    /// 上限を超えたか（負の上限は無制限）。
    let exceeded (limit: int64, used: int64) = limit >= 0L && used > limit

/// 行を一時ファイルへ書き出し、そこから遅延読みする。
module internal Spill =
    /// 一時ファイルのプレフィックス（掃除の対象をこれで見分ける）。
    [<Literal>]
    let private Prefix = "rykdb-sort-"

    /// 一時ファイルの拡張子。
    [<Literal>]
    let private RunExt = ".run"

    /// パーティション分割の本数。
    /// 同時に開くファイル数を固定するため、分割が足りないときは再分割する。
    [<Literal>]
    let Partitions = 16

    /// 一時ファイルの置き場所。
    let directory (dir: string option) =
        match dir with
        | Some d -> d
        | None -> System.IO.Path.GetTempPath()

    /// 24時間を超えて残った run を回収する（途中打ち切りで放置された分）。
    /// 実行中の run は24時間を超えない前提で、年齢で見分ける。
    let sweepStale (dir: string option) =
        protect (fun () ->
            let dir = directory dir

            if System.IO.Directory.Exists dir then
                let cutoff = System.DateTime.UtcNow.AddHours -24.0

                System.IO.Directory.GetFiles(dir, Prefix + "*" + RunExt)
                |> Array.iter (fun p ->
                    if System.IO.FileInfo(p).CreationTimeUtc < cutoff then
                        protect (fun () -> System.IO.File.Delete p) |> ignore))
        |> ignore

    /// 一時ファイルを消す（無ければ何もしない）。
    let unlink (path: string) =
        protect (fun () ->
            if System.IO.File.Exists path then
                System.IO.File.Delete path)
        |> ignore

    /// 残っている一時ファイルを全部消す（エラー経路の後始末）。
    let forget (paths: string list) = paths |> List.iter unlink

    /// run ファイルのパス。
    let runPath (dir: string option) (stamp: string) (serial: int) =
        System.IO.Path.Combine(directory dir, sprintf "%s%s-%d%s" Prefix stamp serial RunExt)

    /// run の書き込み口を開く（既存ファイルは作り直す）。
    let openSink (path: string) =
        protect (fun () ->
            let fs =
                new System.IO.FileStream(
                    path,
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.None
                )

            new Sink(new System.IO.BufferedStream(fs, 65536) :> System.IO.Stream))

    /// 書き込み口を閉じる（失敗は握る）。
    let closeSink (sink: Sink) = (sink :> System.IDisposable).Dispose()

    /// 複数の書き込み口を閉じる。
    let closeSinks (sinks: Sink list) = sinks |> List.iter closeSink

    /// 1行をフレーム（varint 長 + 符号化）にする。
    let private frameRow (row: SqlValue array) =
        ValueCodec.writeValuesSparse row Writer.Empty
        |> Result.map (fun writer ->
            let payload = Writer.toArray writer
            Writer.build [ Writer.varint (uint64 payload.Length); Writer.writeBytes payload ])

    /// 1行を run へ書き出す。
    let writeRow (row: SqlValue array) (sink: Sink) =
        frameRow row
        >>= fun bytes -> protect (fun () -> sink.Stream.Write(bytes, 0, bytes.Length))

    /// 行を整列せずに 1本の run へ書き出す（順序は入力のまま）。
    let writeChunk (dir: string option) (stamp: string) (serial: int) (rows: SqlValue array array) =
        let path = runPath dir stamp serial

        let rec writeAll i sink =
            if i >= rows.Length then
                ret ()
            else
                match writeRow rows.[i] sink with
                | Ok() -> writeAll (i + 1) sink
                | Error e -> err e

        openSink path
        >>= fun sink ->
            match writeAll 0 sink with
            | Error e ->
                closeSink sink
                unlink path
                err e
            | Ok() ->
                closeSink sink
                ret path

    /// run ファイル 1本を作る（受け皿の行を整列して書き出す）。
    let writeRun
        (dir: string option)
        (stamp: string)
        (serial: int)
        (compareRows: SqlValue array -> SqlValue array -> int)
        (rows: SqlValue array array)
        =
        writeChunk dir stamp serial (Array.sortWith compareRows rows)

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

        let s = src.Stream

        protect (fun () ->
            let first = s.ReadByte()

            if first < 0 then
                ret None
            else
                match readVarintFrom s (byte first) with
                | Ok len when len > uint64 System.Int32.MaxValue -> err (DbError.corruption "Spill: 行の長さが異常です")
                | Ok len ->
                    readExactly s (int len)
                    >>= (Cursor.create >> ValueCodec.readValues)
                    >>= fun (values, _) -> ret (Some values)
                | Error e -> err e)
        >>= id

    /// 読み込み口を閉じる。
    let closeSource (src: Source) = (src :> System.IDisposable).Dispose()

    /// run ファイルを開いて先頭 1行を読む。
    let openRun (path: string) =
        let openSource path =
            protect (fun () ->
                let fs = System.IO.File.OpenRead path
                new Source(new System.IO.BufferedStream(fs, 65536) :> System.IO.Stream))

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

    /// ScanScope に登録して run を開く。
    /// 早期終了（`DisposeAll`）で閉じて消す後始末を一緒に登録する。
    let openTracked (scope: ScanScope option) (path: string) =
        openRun path
        >>= fun st ->
            match scope with
            | Some s ->
                let cleanup =
                    { new System.IDisposable with
                        member _.Dispose() =
                            closeSource st.Src
                            unlink path
                    }

                s.Register(box st.Src, cleanup)
            | None -> ()

            ret st

    /// 通常終了: 登録を外してから閉じる（`deleteRun` なら消す）。
    let closeTracked (scope: ScanScope option) (deleteRun: bool) (st: RunState) =
        match scope with
        | Some s -> s.Forget(box st.Src)
        | None -> ()

        if deleteRun then closeRun st else closeSource st.Src

    /// 複数の run をまとめて（通常終了として）閉じる。
    let closeAllTracked (scope: ScanScope option) (states: RunState list) =
        states |> List.iter (closeTracked scope true)

    /// キーからパーティション番号を決める。
    let private partitionOf (count: int) (key: byte array) =
        if count <= 1 then
            0
        else
            int (Hashing.hashBytes key % uint64 count)

    /// 退避キーからパーティション番号を決める。
    /// 再分割では `salt` を混ぜて分け方を変える（同じキー分布で同じ分割に戻らないようにする）。
    /// 呼び出し側と再分割で同じ式を使うため、ここに一本化する。
    let partitionOfKey (salt: uint64) (key: byte array) =
        partitionOf Partitions (System.BitConverter.GetBytes(Hashing.hashBytes key ^^^ salt))

    /// N 本のパーティションへ行を振り分けて書き出す。
    /// 書き込み口は構築時にすべて開く（本数は固定なので fd は有界）。
    [<Sealed>]
    type internal PartitionWriter internal (sinks: Sink array, paths: string array) =
        /// パーティションのパスの並び。
        member _.Paths = paths

        /// 指定したパーティションへ 1行追記する。
        member _.Write(index: int, row: SqlValue array) =
            if index < 0 || index >= sinks.Length then
                DbError.internalError "Spill: パーティション番号が範囲外です" |> err
            else
                writeRow row sinks.[index]

        /// すべて閉じる。
        member _.Close() = sinks |> Array.iter closeSink

        /// すべて閉じてファイルを消す。
        member _.Abandon() =
            sinks |> Array.iter closeSink
            paths |> Array.iter unlink

    /// パーティション書き込みの口を開く。
    let openPartitions (dir: string option) (stamp: string) (serial: int) (count: int) =
        let rec go i sinks paths =
            if i >= count then
                ret (PartitionWriter(Array.ofList (List.rev sinks), Array.ofList (List.rev paths)))
            else
                let path = runPath dir stamp (serial + i)

                match openSink path with
                | Error e ->
                    closeSinks sinks
                    forget paths
                    err e
                | Ok sink -> go (i + 1) (sink :: sinks) (path :: paths)

        if count <= 0 then
            err (DbError.internalError "Spill: パーティション数が 0 以下です")
        else
            go 0 [] []

    /// 1本の run を先頭から読み、行ごとに `step` を適用する。
    /// 終端でもエラーでもファイルを閉じて消す。
    let foldRun (path: string) (init: 's) (step: 's -> SqlValue array -> Result<'s, DbError>) =
        let rec go acc st =
            match st.Head with
            | None ->
                closeRun st
                ret acc
            | Some row ->
                match step acc row with
                | Error e ->
                    closeRun st
                    err e
                | Ok next ->
                    match readRow st.Src with
                    | Error e ->
                        closeRun st
                        err e
                    | Ok None ->
                        closeRun st
                        ret next
                    | Ok(Some head) -> go next { st with Head = Some head }

        openRun path >>= go init

    /// 1本の run を先頭から読み、行ごとに `step` を適用する。
    /// 終端・エラーで閉じるが、**ファイルは消さない**（分割し直すときに使う）。
    let foldRunKeep (path: string) (init: 's) (step: 's -> SqlValue array -> Result<'s, DbError>) =
        let rec go acc st =
            match st.Head with
            | None ->
                closeSource st.Src
                ret acc
            | Some row ->
                match step acc row with
                | Error e ->
                    closeSource st.Src
                    err e
                | Ok next ->
                    match readRow st.Src with
                    | Error e ->
                        closeSource st.Src
                        err e
                    | Ok None ->
                        closeSource st.Src
                        ret next
                    | Ok(Some head) -> go next { st with Head = Some head }

        openRun path >>= go init

    /// 1本の run を N 本のパーティションへ分割し直す。
    /// `salt` をキーに混ぜるので、同じキー分布でも分割のされ方が変わる
    /// （1本が上限に収まらないときの再分割で無限に同じ分け方へ戻らないようにする）。
    /// 元の run は読み終えた時点で消す。
    let repartitionRun
        (dir: string option)
        (stamp: string)
        (serial: int)
        (count: int)
        (salt: uint64)
        (keyOf: SqlValue array -> Result<byte array, DbError>)
        (path: string)
        =
        match openPartitions dir stamp serial count with
        | Error e ->
            unlink path
            err e
        | Ok writer ->
            let writeToPartition row =
                keyOf row >>= fun key -> writer.Write(partitionOfKey salt key, row)

            match foldRunKeep path () (fun () row -> writeToPartition row) with
            | Error e ->
                writer.Abandon()
                unlink path
                err e
            | Ok() ->
                writer.Close()
                unlink path
                ret (writer.Paths |> Array.toList)

    /// 何度でも先頭から読める行の並び。
    /// 上限内ならメモリ、超過したら run ファイルへ退避する。
    [<RequireQualifiedAccess>]
    type RowSpool =
        /// メモリに収まっている行。
        | Memory of SqlValue array array
        /// run ファイルへ退避した行（パスの並びは入力順）。
        | Disk of paths: string list * count: int

    /// 行数。
    let spoolCount =
        function
        | RowSpool.Memory rows -> rows.Length
        | RowSpool.Disk(_, count) -> count

    /// ディスクへ退避したか。
    let isSpilled =
        function
        | RowSpool.Memory _ -> false
        | RowSpool.Disk _ -> true

    /// 退避した run を消す（メモリなら何もしない）。
    let deleteSpool =
        function
        | RowSpool.Memory _ -> ()
        | RowSpool.Disk(paths, _) -> forget paths

    /// 退避した spool を ScanScope に登録する。
    /// 早期終了でファイルを消すための後始末を一緒に登録する。
    let registerSpool (scope: ScanScope option) (spool: RowSpool) =
        match scope, spool with
        | Some s, RowSpool.Disk(paths, _) ->
            let cleanup =
                { new System.IDisposable with
                    member _.Dispose() = forget paths
                }

            s.Register(box spool, cleanup)
        | _ -> ()

    /// 通常終了: spool の登録を外す（ファイルは呼び出し側が消す）。
    let forgetSpool (scope: ScanScope option) (spool: RowSpool) =
        match scope with
        | Some s -> s.Forget(box spool)
        | None -> ()

    /// 退避した行を先頭から読む Op を作る（`deleteRun` なら尽きた run を消す）。
    /// <remarks>
    /// `openNext` / `stream` は `Op.compute` の継続の中で呼ぶので、
    /// 1行ごとに 1段だけ進み、スタックは積み上がらない。
    /// </remarks>
    let private reopenWith (deleteRun: bool) (scope: ScanScope option) =
        let closeOne st = closeTracked scope deleteRun st

        let rec openNext rest =
            match rest with
            | [] -> Op.Empty
            | path :: tail ->
                match openTracked scope path with
                | Error e -> Op.compute (fun () -> OpStep.OpError e)
                | Ok st -> stream tail st

        and stream rest st =
            match st.Head with
            | None ->
                closeOne st
                openNext rest
            | Some row ->
                Op.compute (fun () ->
                    match readRow st.Src with
                    | Error e ->
                        closeOne st
                        OpStep.OpError e
                    | Ok None ->
                        closeOne st
                        OpStep.OpRow(row, openNext rest)
                    | Ok(Some head) -> OpStep.OpRow(row, stream rest { st with Head = Some head }))

        function
        | RowSpool.Memory rows -> ret (Op.ofArray rows)
        | RowSpool.Disk(paths, _) -> ret (openNext paths)

    /// 退避した行を先頭から読む Op を作る。
    /// `scope` を渡すと、途中で捨てられても開いたファイルを閉じられる。
    /// 尽きた run は閉じて消す（何度でも呼べる）。
    let reopen (scope: ScanScope option) (spool: RowSpool) = reopenWith true scope spool

    /// `reopen` と同じだが、run を消さない。
    /// 同じ行を何度も読み直すとき（JOIN の右側を左行ごとに走査するなど）に使い、
    /// 使い終わったら `deleteSpool` で消す。
    let reopenKeep (scope: ScanScope option) (spool: RowSpool) = reopenWith false scope spool

    /// `src` の全行を上限までメモリに溜め、超えたら run へ退避する。
    /// 入力を 1回だけ消費する（JOIN の右側・同一キーのグループなどに使う）。
    let spool (budget: MemoryBudget) (src: Op) =
        let stamp = System.Guid.NewGuid().ToString "N"

        let rec go buf bufCount bufBytes written serial paths cur =
            match Op.next cur with
            | OpStep.OpError e ->
                forget paths
                err e
            | OpStep.OpDone ->
                if List.isEmpty paths then
                    ret (RowSpool.Memory(List.rev buf |> List.toArray))
                elif List.isEmpty buf then
                    ret (RowSpool.Disk(List.rev paths, written))
                else
                    match writeChunk budget.Directory stamp serial (List.rev buf |> List.toArray) with
                    | Error e ->
                        forget paths
                        err e
                    | Ok last -> ret (RowSpool.Disk(List.rev (last :: paths), written + bufCount))
                |> Result.map (fun spool ->
                    registerSpool budget.Scope spool
                    spool)
            | OpStep.OpRow(row, nxt) ->
                let size = int64 (ValueCodec.managedRowBytes row) + 32L

                if not (List.isEmpty buf) && MemoryBudget.exceeded (budget.Work, bufBytes + size) then
                    match writeChunk budget.Directory stamp serial (List.rev buf |> List.toArray) with
                    | Error e ->
                        forget paths
                        err e
                    | Ok p -> go [] 0 0L (written + bufCount) (serial + 1) (p :: paths) (Op.emit row nxt)
                else
                    go (row :: buf) (bufCount + 1) (bufBytes + size) written serial paths nxt

        go [] 0 0L 0 0 [] src

    /// パーティションを分割し直す深さの上限。
    /// ここに達したら上限を無視して畳む（メモリは best effort。失敗はさせない）。
    [<Literal>]
    let private MaxGroupDepth = 8

    /// 再分割で混ぜる salt（深さごとに変える）。
    let private saltAt (depth: int) =
        uint64 (depth + 1) * 0x9E3779B97F4A7C15UL

    /// パーティション 1本をキーごとに畳む。
    /// 状態の合計が上限を超えたら `None` を返す（呼び出し側が分割し直す）。
    /// `seeds` は先に作られていた状態（引き継ぐ）。
    let private foldEntries
        (budget: MemoryBudget)
        (keyOf: SqlValue array -> Result<byte array, DbError>)
        (seed: unit -> 'A)
        (add: 'A -> SqlValue array -> Result<'A, DbError>)
        (stateBytes: 'A -> int64)
        (seeds: (byte array * 'A) list)
        (path: string)
        =
        let dict =
            System.Collections.Generic.Dictionary<byte array, 'A> HashIdentity.Structural

        seeds |> List.iter (fun (k, v) -> dict.[k] <- v)

        let snapshot () =
            dict |> Seq.map (fun kv -> kv.Key, kv.Value) |> List.ofSeq

        let rec go used st =
            match st.Head with
            | None ->
                closeSource st.Src
                ret (Some(snapshot ()))
            | Some row ->
                match keyOf row with
                | Error e ->
                    closeSource st.Src
                    err e
                | Ok key ->
                    let isNew = not (dict.ContainsKey key)
                    let current = if isNew then seed () else dict.[key]

                    if isNew && MemoryBudget.exceeded (budget.Work, used + stateBytes current) then
                        closeSource st.Src
                        ret None
                    else
                        match add current row with
                        | Error e ->
                            closeSource st.Src
                            err e
                        | Ok next ->
                            dict.[key] <- next
                            let used = if isNew then used + stateBytes next else used

                            match readRow st.Src with
                            | Error e ->
                                closeSource st.Src
                                err e
                            | Ok None -> go used { st with Head = None }
                            | Ok(Some head) -> go used { st with Head = Some head }

        openRun path >>= go (seeds |> List.sumBy (snd >> stateBytes))

    /// 退避した行をキーごとに畳み、結果行の Op を作る。
    /// <remarks>
    /// パーティションを 1本ずつ読み、上限に収まる範囲で辞書に畳み込む。
    /// 収まらないパーティションは salt を変えて分割し直し、子を作業リストへ積む。
    /// `seeds` は上限内で先に作られていた状態で、該当するパーティションへ引き継ぐ
    /// （`PART` を超えない限り、上限内の状態と退避した行がここで合流する）。
    /// 出力順はパーティション順（呼び出し側が必要なら seq で並べ直す）。
    /// 再分割の深さは `MaxGroupDepth` で有界で、そこから先は上限を無視して畳む（best effort）。
    /// </remarks>
    let groupPartitions
        (budget: MemoryBudget)
        (stamp: string)
        (keyOf: SqlValue array -> Result<byte array, DbError>)
        (seed: unit -> 'A)
        (add: 'A -> SqlValue array -> Result<'A, DbError>)
        (stateBytes: 'A -> int64)
        (emit: 'A -> Result<SqlValue array option, DbError>)
        (seeds: (byte array * 'A) list)
        (paths: string list)
        =
        let abandon = List.iter (fun (_, _, path) -> unlink path)

        let rec foldPartition acc =
            function
            | [] -> ret (List.rev acc |> List.concat)
            | (depth, entries, path) :: rest ->
                let effective =
                    if depth >= MaxGroupDepth then
                        { budget with Work = -1L }
                    else
                        budget

                match foldEntries effective keyOf seed add stateBytes entries path with
                | Error e ->
                    unlink path
                    abandon rest
                    err e
                | Ok(Some folded) ->
                    unlink path

                    match folded |> List.map (fun (_, state) -> emit state) |> all with
                    | Error e ->
                        abandon rest
                        err e
                    | Ok emitted -> foldPartition (List.choose id emitted :: acc) rest
                | Ok None ->
                    let salt = saltAt depth

                    match
                        repartitionRun
                            budget.Directory
                            (sprintf "%s-d%d" stamp (depth + 1))
                            0
                            Partitions
                            salt
                            keyOf
                            path
                    with
                    | Error e ->
                        abandon rest
                        err e
                    | Ok subs ->
                        let byPartition =
                            entries |> List.groupBy (fun (k, _) -> partitionOfKey salt k) |> Map.ofList

                        let children =
                            subs
                            |> List.mapi (fun i sub ->
                                let carried = byPartition |> Map.tryFind i |> Option.defaultValue []
                                depth + 1, carried, sub)

                        foldPartition acc (children @ rest)

        paths
        |> List.mapi (fun i path ->
            let carried = seeds |> List.filter (fun (k, _) -> partitionOfKey 0UL k = i)
            0, carried, path)
        |> foldPartition []
        |> Result.map (List.toArray >> Op.ofArray)

    /// `src` の各行に連番を振ってパーティションへ書き出す。
    /// 枠は `frame seq row` で作り、振り分けは `keyOf row` で決める。
    /// 返り値は次に使える連番。
    let fillPartitions
        (writer: PartitionWriter)
        (keyOf: SqlValue array -> Result<byte array, DbError>)
        (frame: int64 -> SqlValue array -> SqlValue array)
        =
        let rec go seq cur =
            match Op.next cur with
            | OpStep.OpError e -> err e
            | OpStep.OpDone -> ret seq
            | OpStep.OpRow(row, nxt) ->
                match keyOf row with
                | Ok key ->
                    match writer.Write(partitionOfKey 0UL key, frame seq row) with
                    | Ok() -> go (seq + 1L) nxt
                    | Error e -> err e
                | Error e -> err e

        go
