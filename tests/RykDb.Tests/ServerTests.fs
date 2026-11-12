// ===========================================================================
// Server.fs / Client.fs のテスト（loopback TCP）
// ===========================================================================

module RykDb.ServerTests

open System.IO
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks
open Xunit
open RykDb.Core
open RykDb.Server
open RykDb.Client
open RykDb.TestHelpers

/// テスト用にサーバを起動して 1 接続を処理し、確実に止める。
/// 停止は「クライアントを閉じる → 停止要請 → タスク完了を待つ → flush/削除」の順。
/// 完了を待たずに Close すると、サーバが DB を触っている間に削除して落ちる。
let private withServer (f: RykDbClient -> unit) =
    let dir = newDir "tcp"
    // 接続ごとに独立した Session を発行するプール。1つのサーバがこれを共有する。
    let session: SessionPool = ok (SessionPool.OpenWith dir)
    // port 0 = OS が空いているポートを割り当てる。
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve session listener cts.Token (fun _ -> ())
        finally
            finished.Set())
    |> ignore

    let client = new RykDbClient("127.0.0.1", port)

    try
        f client
    finally
        // 1) 接続を閉じる（サーバの `ReadLine` を解放する）
        client.Close()
        // 2) 停止を要請する
        cts.Cancel()

        // 3) サーバが完全に停止するまで待つ
        if not (finished.Wait 10000) then
            Assert.Fail "サーバが 10 秒以内に停止しませんでした"

        cts.Dispose()
        // 4) flush してからディレクトリを消す
        session.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

        // タスクは Dispose しない（実行中の task の Dispose は危険）。

let private okRows (r: Reply) =
    match r with
    | Rows(cols, rows) -> cols, rows
    | Affected _ -> [], []
    | Streamed(cols, _) -> cols, []
    | Failure m ->
        Assert.Fail(sprintf "サーバがエラーを返しました: %s" m)
        [], []
    | Halt ->
        Assert.Fail "サーバが HALT を返しました"
        [], []

[<Fact>]
let ``TCP: loopback で SQL が通る`` () =
    withServer (fun client ->
        let cols, _ =
            okRows (client.Send "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")

        Assert.Equal<string list>([], cols)

        match client.Send "INSERT INTO t VALUES (1, 'a'), (2, 'b')" with
        | Affected 2 -> ()
        | other -> Assert.Fail(sprintf "INSERT の影響行数が一致しません: %A" other)

        let cols, rows = okRows (client.Send "SELECT id, name FROM t ORDER BY id")
        Assert.Equal<string list>([ "id"; "name" ], cols)
        Assert.Equal<string list>([ "1,'a'"; "2,'b'" ], rows)

        // 集約も通る
        let _, rows = okRows (client.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "2" ], rows)

        // エラーは ERR で返る（HALT しない）
        match client.Send "SELECT * FROM nope" with
        | Failure m -> Assert.Contains("nope", m)
        | other -> Assert.Fail(sprintf "ERR を期待しましたが: %A" other)

        // ping
        let cols, _ = okRows (client.Send ".ping")
        Assert.Equal<string list>([ "pong" ], cols)

        // 終了
        Assert.True(
            match client.Send ".exit" with
            | Halt -> true
            | _ -> false
        ))

[<Fact>]
let ``TCP: 0行の SELECT でも列名を返す`` () =
    withServer (fun client ->
        okRows (client.Send "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")
        |> ignore

        let cols, rows = okRows (client.Send "SELECT id, name FROM t WHERE id = 999")
        Assert.Equal<string list>([ "id"; "name" ], cols)
        Assert.Empty rows)

[<Fact>]
let ``TCP: カンマを含む列名も正しく返す`` () =
    withServer (fun client ->
        okRows (client.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (client.Send "INSERT INTO t VALUES (1)") |> ignore
        let cols, rows = okRows (client.Send "SELECT id AS \"a,b\" FROM t")
        Assert.Equal<string list>([ "a,b" ], cols)
        Assert.Equal<string list>([ "1" ], rows))

[<Fact>]
let ``TCP: ライブラリ API と同じ結果が返る`` () =
    // 同じスクリプトを 2 経路で流して結果を突き合わせる。
    let script =
        [
            "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)"
            "INSERT INTO t VALUES (1, 10), (2, 20), (3, 10), (4, 30)"
            "SELECT id, g FROM t ORDER BY id"
            "SELECT g, COUNT(*) FROM t GROUP BY g ORDER BY g"
            "SELECT COUNT(*), MIN(g), MAX(g), SUM(g) FROM t"
            "SELECT DISTINCT g FROM t ORDER BY g"
        ]

    // --- ライブラリ API ---
    withDir "cmp-lib" (fun dir ->
        let rec run
            (session: Session)
            (rest: string list)
            (acc: (string list * string list) list)
            : (string list * string list) list =
            match rest with
            | [] -> List.rev acc
            | sql :: tail ->
                let r = ok (session.Execute sql)
                run session tail ((r.Columns |> Array.toList, rowText r) :: acc)

        let session = ok (Session.OpenWith dir)
        let libResults = run session script []
        ok (session.Close())

        // --- TCP ---
        withServer (fun client ->
            let tcpResults =
                script
                |> List.map (fun sql ->
                    let cols, rows = okRows (client.Send sql)
                    cols, rows)

            Assert.Equal<int>(script.Length, libResults.Length)

            // 差異があればここで落ちる。
            List.zip libResults tcpResults
            |> List.iteri (fun i ((lc, lr), (tc, tr)) ->
                if lc <> tc || lr <> tr then
                    Assert.Fail(
                        sprintf
                            "i=%d で結果が一致しませんでした\n  SQL: %s\n  ライブラリ: %A / %A\n  TCP: %A / %A"
                            i
                            script.[i]
                            lc
                            lr
                            tc
                            tr
                    ))

            Assert.Equal<int>(script.Length, tcpResults.Length)))

[<Fact>]
let ``TCP: .tables はテーブル名を返す`` () =
    withServer (fun client ->
        // テーブルが無いときは空
        Assert.Equal<string list>([], fst (okRows (client.Send ".tables")))

        okRows (client.Send "CREATE TABLE zzz (id INTEGER PRIMARY KEY)") |> ignore
        let cols, rows = okRows (client.Send ".tables")
        Assert.Equal<string list>([ "tables" ], cols)
        // 行は SqlLiteral 表記（文字列は引用符つき）で返る
        Assert.Contains("'zzz'", rows))

[<Fact>]
let ``TCP: 空行は空応答になり、切断後も接続を受け付ける`` () =
    let dir = newDir "tcp-raw"
    let session: SessionPool = ok (SessionPool.OpenWith dir)
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve session listener cts.Token (fun _ -> ())
        finally
            finished.Set())
    |> ignore

    try
        // 空行 → 有効な空結果応答
        use raw = new TcpClient("127.0.0.1", port)
        let stream = raw.GetStream()
        use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
        use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
        writer.AutoFlush <- true

        writer.WriteLine ""
        Assert.Equal<string>("OK 0", reader.ReadLine())

        writer.WriteLine ".ping"
        Assert.Equal<string>("OK 0\tpong", reader.ReadLine())

        // .exit を送らずに切断する（サーバの EOF 経路）
        raw.Dispose()

        // サーバは止まらず、次の接続を受け付ける
        use raw2 = new TcpClient("127.0.0.1", port)
        let stream2 = raw2.GetStream()
        use writer2 = new StreamWriter(stream2, System.Text.Encoding.UTF8, 1024, true)

        use reader2 =
            new StreamReader(stream2, System.Text.Encoding.UTF8, false, 1024, true)

        writer2.AutoFlush <- true

        writer2.WriteLine ".exit"
        Assert.Equal<string>("HALT", reader2.ReadLine())
    finally
        cts.Cancel()
        finished.Wait 10000 |> ignore
        cts.Dispose()
        session.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

[<Fact>]
let ``TCP: 突然の切断（RST）でもサーバは生き続ける`` () =
    let dir = newDir "tcp-rst"
    let session: SessionPool = ok (SessionPool.OpenWith dir)
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve session listener cts.Token (fun _ -> ())
        finally
            finished.Set())
    |> ignore

    try
        // まとめて送ってから読まずに RST で切る → サーバの応答書き込みが失敗する
        let raw = new TcpClient("127.0.0.1", port)
        let stream = raw.GetStream()
        use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
        writer.AutoFlush <- true

        for _ in 1..200 do
            writer.WriteLine ".ping"

        raw.Client.LingerState <- new LingerOption(true, 0)
        raw.Close()
        Thread.Sleep 300

        // サーバは止まらず、次の接続を受け付ける
        use raw2 = new TcpClient("127.0.0.1", port)
        let stream2 = raw2.GetStream()
        use writer2 = new StreamWriter(stream2, System.Text.Encoding.UTF8, 1024, true)

        use reader2 =
            new StreamReader(stream2, System.Text.Encoding.UTF8, false, 1024, true)

        writer2.AutoFlush <- true

        writer2.WriteLine ".ping"
        Assert.Equal<string>("OK 0\tpong", reader2.ReadLine())
    finally
        cts.Cancel()
        finished.Wait 10000 |> ignore
        cts.Dispose()
        session.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

[<Fact>]
let ``TCP: Client を二度閉じても落ちない`` () =
    withServer (fun client ->
        client.Close()
        // 2 回目は送信に失敗するが握りつぶす
        client.Close()
        // IDisposable 経由も同じ経路
        (client :> System.IDisposable).Dispose())

/// 1 接続だけ受けて、行ごとに respond の応答を返す偽サーバでテストする。
let private withFakeServer (respond: string -> string option) (f: int -> unit) =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port

    let task =
        Task.Run(fun () ->
            use c = listener.AcceptTcpClient()
            use stream = c.GetStream()
            use r = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
            use w = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
            w.AutoFlush <- true

            let mutable line = r.ReadLine()

            while not (isNull line) do
                match respond line with
                | Some resp -> w.WriteLine resp
                | None -> ()

                line <- r.ReadLine())

    try
        f port
    finally
        listener.Stop()
        task.Wait 5000 |> ignore

[<Fact>]
let ``Client: 不正な短い OK は Failure`` () =
    withFakeServer (fun _ -> Some "OK") (fun port ->
        use c = new RykDbClient("127.0.0.1", port)

        match c.Send "x" with
        | Failure _ -> ()
        | other -> Assert.Fail(sprintf "Failure ではない: %A" other))

[<Fact>]
let ``Client: 不正な件数は Failure`` () =
    withFakeServer (fun _ -> Some "OK abc\tcol") (fun port ->
        use c = new RykDbClient("127.0.0.1", port)

        match c.Send "x" with
        | Failure _ -> ()
        | other -> Assert.Fail(sprintf "Failure ではない: %A" other))

[<Fact>]
let ``Client: 応答の列と行を読む`` () =
    withFakeServer (fun _ -> Some "OK 2\tc1,c2\n1,2\n3,4") (fun port ->
        // IDisposable 経由の Close もここで通る
        use c = new RykDbClient("127.0.0.1", port)

        match c.Send "x" with
        | Rows([ "c1"; "c2" ], [ "1,2"; "3,4" ]) -> ()
        | other -> Assert.Fail(sprintf "行列が一致しない: %A" other))

[<Fact>]
let ``Client: Send は STREAM を Rows に materialize する`` () =
    withFakeServer (fun _ -> Some "STREAM\tc1,c2\n1,2\n3,4\nEND") (fun port ->
        use c = new RykDbClient("127.0.0.1", port)

        match c.Send "x" with
        | Rows([ "c1"; "c2" ], [ "1,2"; "3,4" ]) -> ()
        | other -> Assert.Fail(sprintf "STREAM の materialize が一致しません: %A" other))

[<Fact>]
let ``Client: 接続が RST で切れたら Halt`` () =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port

    let task =
        Task.Run(fun () ->
            use c = listener.AcceptTcpClient()
            // 要求を読まずに放置して受信バッファを残し、LINGER 0 で RST にする
            Thread.Sleep 200
            c.Client.LingerState <- new LingerOption(true, 0)
            c.Close())

    try
        use c = new RykDbClient("127.0.0.1", port)

        match c.Send "x" with
        | Halt -> ()
        | other -> Assert.Fail(sprintf "Halt ではない: %A" other)
    finally
        listener.Stop()
        task.Wait 5000 |> ignore

[<Fact>]
let ``TCP: TEXT の改行・CR・バックスラッシュが 1行プロトコルを壊さない`` () =
    let dir = newDir "tcp-newline"
    let session: SessionPool = ok (SessionPool.OpenWith dir)

    // 事前データは 1 接続で入れて閉じる（DB はプールが保持したまま）。
    do
        let setup = ok (session.CreateSession())
        ok (setup.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore

        ok (setup.Execute "INSERT INTO t VALUES (1, 'a\nb'), (2, 'c\rd'), (3, 'C:\\temp')")
        |> ignore

        ok (setup.Close())

    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve session listener cts.Token (fun _ -> ())
        finally
            finished.Set())
    |> ignore

    try
        // 生の応答では改行が \n などのエスケープとして 1行に収まる。
        let raw = new TcpClient("127.0.0.1", port)

        try
            let stream = raw.GetStream()
            use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
            use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
            writer.AutoFlush <- true

            writer.WriteLine "SELECT id, v FROM t ORDER BY id"
            Assert.Equal<string>("OK 3\tid,v", reader.ReadLine())
            Assert.Equal<string>("1,'a\\nb'", reader.ReadLine())
            Assert.Equal<string>("2,'c\\rd'", reader.ReadLine())
            Assert.Equal<string>("3,'C:\\\\temp'", reader.ReadLine())

            // 続けて別の応答がずれずに読める = 余分な行が混ざっていない。
            writer.WriteLine ".ping"
            Assert.Equal<string>("OK 0\tpong", reader.ReadLine())

            writer.WriteLine ".exit"
            Assert.Equal<string>("HALT", reader.ReadLine())
        finally
            raw.Dispose()

        // クライアント API はエスケープを戻すので、ライブラリと同じ値が見える。
        use client = new RykDbClient("127.0.0.1", port)
        let _, rows = okRows (client.Send "SELECT id, v FROM t ORDER BY id")
        Assert.Equal<string list>([ "1,'a\nb'"; "2,'c\rd'"; "3,'C:\\temp'" ], rows)
    finally
        cts.Cancel()
        finished.Wait 10000 |> ignore
        cts.Dispose()
        session.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

[<Fact>]
let ``TCP: 切断時に open transaction は rollback される`` () =
    let dir = newDir "tcp-rollback"
    let session: SessionPool = ok (SessionPool.OpenWith dir)
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve session listener cts.Token (fun _ -> ())
        finally
            finished.Set())
    |> ignore

    try
        // BEGIN して INSERT したまま .exit も ROLLBACK も送らずに切る。
        let raw = new TcpClient("127.0.0.1", port)

        try
            let stream = raw.GetStream()
            use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
            use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
            writer.AutoFlush <- true

            writer.WriteLine "CREATE TABLE t (id INTEGER PRIMARY KEY)"
            Assert.Equal<string>("OK 0", reader.ReadLine())

            writer.WriteLine "BEGIN"
            Assert.Equal<string>("OK 0", reader.ReadLine())

            writer.WriteLine "INSERT INTO t VALUES (1)"
            Assert.Equal<string>("OK 1", reader.ReadLine())
        finally
            raw.Dispose()

        // 切断の検知と rollback はサーバ側で非同期に進む。
        // その接続の rollback が終わるまで、新規接続からは行が見えないままになる。
        use client = new RykDbClient("127.0.0.1", port)

        let rolledBack =
            SpinWait.SpinUntil(
                (fun () ->
                    let _, rows = okRows (client.Send "SELECT COUNT(*) FROM t")
                    rows = [ "0" ]),
                10000
            )

        Assert.True(rolledBack, "切断時の rollback が 10 秒以内に完了しませんでした")
    finally
        cts.Cancel()
        finished.Wait 10000 |> ignore
        cts.Dispose()
        session.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

/// 複数接続のテスト用: サーバを起動し、割り当てられたポートを渡す。
let private withTestServerOptions (options: SessionOptions) (tag: string) (f: int -> unit) =
    let dir = newDir tag
    let session: SessionPool = ok (SessionPool.OpenWith(dir, options))
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    // accept でブロックするので専用スレッドで動かす（他のテストのプール飽和に影響されない）。
    Task.Factory.StartNew(
        (fun () ->
            try
                serve session listener cts.Token (fun _ -> ())
            finally
                finished.Set()),
        TaskCreationOptions.LongRunning
    )
    |> ignore

    try
        f port
    finally
        cts.Cancel()

        if not (finished.Wait 10000) then
            Assert.Fail "サーバが 10 秒以内に停止しませんでした"

        cts.Dispose()
        session.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

let private withTestServer (tag: string) (f: int -> unit) =
    withTestServerOptions SessionOptions.Default tag f

[<Fact>]
let ``TCP: 接続中の接続があっても新しい接続を受け付ける`` () =
    withTestServer "tcp-concurrent-accept" (fun port ->
        // A は接続したまま何も送らない。逐次 accept の実装では B が処理されずここで止まる。
        use rawA = new TcpClient("127.0.0.1", port)
        rawA.ReceiveTimeout <- 10000
        let streamA = rawA.GetStream()

        use readerA =
            new StreamReader(streamA, System.Text.Encoding.UTF8, false, 1024, true)

        use writerA = new StreamWriter(streamA, System.Text.Encoding.UTF8, 1024, true)
        writerA.AutoFlush <- true

        // A が接続中のまま B が接続して応答を得る。
        use clientB = new RykDbClient("127.0.0.1", port)
        let cols, _ = okRows (clientB.Send ".ping")
        Assert.Equal<string list>([ "pong" ], cols)

        // A もそのまま使える。
        writerA.WriteLine ".ping"
        Assert.Equal<string>("OK 0\tpong", readerA.ReadLine()))

[<Fact>]
let ``TCP: 2 接続から並行に SELECT できる`` () =
    withTestServer "tcp-parallel-select" (fun port ->
        use clientA = new RykDbClient("127.0.0.1", port)
        okRows (clientA.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (clientA.Send "INSERT INTO t VALUES (1), (2), (3)") |> ignore
        use clientB = new RykDbClient("127.0.0.1", port)

        let reads =
            [ clientA; clientB ]
            |> List.map (fun client ->
                Task.Run(fun () ->
                    for _ in 1..20 do
                        let _, rows = okRows (client.Send "SELECT COUNT(*) FROM t")
                        Assert.Equal<string list>([ "3" ], rows)))

        Task.WaitAll(List.toArray reads))

[<Fact>]
let ``TCP: 他接続の切断は transaction を rollback しない`` () =
    withTestServer "tcp-txn-owner" (fun port ->
        use clientA = new RykDbClient("127.0.0.1", port)
        okRows (clientA.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (clientA.Send "BEGIN") |> ignore
        okRows (clientA.Send "INSERT INTO t VALUES (1)") |> ignore

        // B は接続してすぐ切る（切断時に他接続の transaction を巻き戻さない）。
        do
            use clientB = new RykDbClient("127.0.0.1", port)

            Assert.True(
                match clientB.Send ".exit" with
                | Halt -> true
                | _ -> false
            )

        // A の transaction は生きている。
        let _, rows = okRows (clientA.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "1" ], rows)

        okRows (clientA.Send "ROLLBACK") |> ignore
        let _, after = okRows (clientA.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "0" ], after))

[<Fact>]
let ``TCP: 各接続の transaction は独立している`` () =
    withTestServer "tcp-txn-isolated" (fun port ->
        use clientA = new RykDbClient("127.0.0.1", port)
        okRows (clientA.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (clientA.Send "BEGIN") |> ignore
        okRows (clientA.Send "INSERT INTO t VALUES (1)") |> ignore

        // A の未 commit の行は B からは見えない（接続ごとに transaction が独立）。
        use clientB = new RykDbClient("127.0.0.1", port)
        let _, hidden = okRows (clientB.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "0" ], hidden)

        // A 自身には見える（read-your-writes）。
        let _, own = okRows (clientA.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "1" ], own)

        // A が COMMIT すると B からも見える。
        okRows (clientA.Send "COMMIT") |> ignore
        let _, visible = okRows (clientB.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "1" ], visible))

[<Fact>]
let ``TCP: 明示 transaction は開始時の snapshot を見る（repeatable read）`` () =
    withTestServer "tcp-txn-repeatable" (fun port ->
        use setup = new RykDbClient("127.0.0.1", port)
        okRows (setup.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (setup.Send "INSERT INTO t VALUES (1)") |> ignore

        use clientA = new RykDbClient("127.0.0.1", port)
        okRows (clientA.Send "BEGIN") |> ignore
        let _, before = okRows (clientA.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "1" ], before)

        // A の transaction 中に B が commit しても、A は開始時の snapshot を見る。
        use clientB = new RykDbClient("127.0.0.1", port)
        okRows (clientB.Send "INSERT INTO t VALUES (2)") |> ignore

        let _, after = okRows (clientA.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "1" ], after)

        okRows (clientA.Send "ROLLBACK") |> ignore

        // transaction の外では新しい snapshot が見える。
        let _, fresh = okRows (clientA.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "2" ], fresh))

[<Fact>]
let ``TCP: 他接続の transaction 中でも書き込みできる`` () =
    withTestServer "tcp-txn-concurrent-write" (fun port ->
        use clientA = new RykDbClient("127.0.0.1", port)
        okRows (clientA.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (clientA.Send "BEGIN") |> ignore
        okRows (clientA.Send "INSERT INTO t VALUES (1)") |> ignore

        // A が transaction を開いたままでも、B の暗黙 transaction は commit できる。
        use clientB = new RykDbClient("127.0.0.1", port)
        okRows (clientB.Send "INSERT INTO t VALUES (2)") |> ignore

        // A は自分の行だけを見る。
        let _, mine = okRows (clientA.Send "SELECT id FROM t ORDER BY id")
        Assert.Equal<string list>([ "1" ], mine)

        okRows (clientA.Send "COMMIT") |> ignore

        let _, all = okRows (clientA.Send "SELECT id FROM t ORDER BY id")
        Assert.Equal<string list>([ "1"; "2" ], all))

[<Fact>]
let ``TCP: 接続間で write-write 競合を検出する`` () =
    withTestServer "tcp-txn-conflict" (fun port ->
        use setup = new RykDbClient("127.0.0.1", port)

        okRows (setup.Send "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
        |> ignore

        okRows (setup.Send "INSERT INTO t VALUES (1, 10)") |> ignore

        use clientA = new RykDbClient("127.0.0.1", port)
        use clientB = new RykDbClient("127.0.0.1", port)
        okRows (clientA.Send "BEGIN") |> ignore
        okRows (clientB.Send "BEGIN") |> ignore
        okRows (clientA.Send "UPDATE t SET v = 11 WHERE id = 1") |> ignore
        okRows (clientB.Send "UPDATE t SET v = 12 WHERE id = 1") |> ignore

        // 先に commit した方が勝ち、後から commit した方は ConflictError。
        okRows (clientA.Send "COMMIT") |> ignore

        match clientB.Send "COMMIT" with
        | Failure m -> Assert.Contains("競合", m)
        | other -> Assert.Fail(sprintf "競合エラーを期待しましたが: %A" other)

        okRows (clientB.Send "ROLLBACK") |> ignore

        let _, rows = okRows (clientA.Send "SELECT v FROM t WHERE id = 1")
        Assert.Equal<string list>([ "11" ], rows))

[<Fact>]
let ``TCP: transaction 中の DDL は他接続で ConflictError になる`` () =
    withTestServer "tcp-txn-ddl" (fun port ->
        use clientA = new RykDbClient("127.0.0.1", port)
        okRows (clientA.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (clientA.Send "BEGIN") |> ignore
        okRows (clientA.Send "INSERT INTO t VALUES (1)") |> ignore

        use clientB = new RykDbClient("127.0.0.1", port)

        match clientB.Send "CREATE INDEX ix ON t (id)" with
        | Failure m -> Assert.Contains("transaction", m)
        | other -> Assert.Fail(sprintf "ConflictError を期待しましたが: %A" other)

        okRows (clientA.Send "COMMIT") |> ignore

        // transaction が終われば DDL は通る。
        okRows (clientB.Send "CREATE INDEX ix ON t (id)") |> ignore)

[<Fact>]
let ``TCP: 接続中でも serve は停止できる`` () =
    let dir = newDir "tcp-stop-active"
    let session: SessionPool = ok (SessionPool.OpenWith dir)
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve session listener cts.Token (fun _ -> ())
        finally
            finished.Set())
    |> ignore

    try
        use raw = new TcpClient("127.0.0.1", port)
        raw.ReceiveTimeout <- 10000
        let stream = raw.GetStream()
        use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
        writer.AutoFlush <- true
        writer.WriteLine ".ping"
        use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
        Assert.Equal<string>("OK 0\tpong", reader.ReadLine())

        // 接続を開いたまま停止を要求する。
        cts.Cancel()
        Assert.True(finished.Wait 10000, "接続中に serve が停止できませんでした")
    finally
        cts.Cancel()
        finished.Wait 5000 |> ignore
        cts.Dispose()
        session.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

[<Fact>]
let ``TCP: .stream は STREAM ヘッダ・行・END を返す`` () =
    withTestServer "tcp-stream-raw" (fun port ->
        use setup = new RykDbClient("127.0.0.1", port)
        okRows (setup.Send "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore
        okRows (setup.Send "INSERT INTO t VALUES (1, 'a'), (2, 'b')") |> ignore

        use raw = new TcpClient("127.0.0.1", port)
        raw.ReceiveTimeout <- 10000
        let stream = raw.GetStream()
        use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
        use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
        writer.AutoFlush <- true

        writer.WriteLine ".stream SELECT id, v FROM t ORDER BY id"
        Assert.Equal<string>("STREAM\tid,v", reader.ReadLine())
        Assert.Equal<string>("1,'a'", reader.ReadLine())
        Assert.Equal<string>("2,'b'", reader.ReadLine())
        Assert.Equal<string>("END", reader.ReadLine())

        // 0行でもヘッダと END を返す。
        writer.WriteLine ".stream SELECT id FROM t WHERE id = 999"
        Assert.Equal<string>("STREAM\tid", reader.ReadLine())
        Assert.Equal<string>("END", reader.ReadLine())

        // SELECT 以外は通常の OK にフォールバックする。
        writer.WriteLine ".stream INSERT INTO t VALUES (3, 'c')"
        Assert.Equal<string>("OK 1", reader.ReadLine())

        // 列名もヘッダー用にエスケープする（クライアントはデコードする）。
        writer.WriteLine ".stream SELECT id AS \"a,b\" FROM t WHERE id = 1"
        Assert.Equal<string>("STREAM\ta%2Cb", reader.ReadLine())
        Assert.Equal<string>("1", reader.ReadLine())
        Assert.Equal<string>("END", reader.ReadLine())

        // SQL の無い .stream は ERR を返し、接続は維持する。
        writer.WriteLine ".stream"
        let missingSqlError = reader.ReadLine()
        Assert.True(missingSqlError.StartsWith("ERR ", System.StringComparison.Ordinal), missingSqlError)

        // ストリーム後も同じ接続で次の要求を送れる。
        writer.WriteLine ".ping"
        Assert.Equal<string>("OK 0\tpong", reader.ReadLine()))

[<Fact>]
let ``TCP: Client.SendStream は行を蓄積せずに受け取る`` () =
    withServer (fun client ->
        okRows (client.Send "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore
        okRows (client.Send "INSERT INTO t VALUES (1, 'a'), (2, 'b')") |> ignore

        let rows = ResizeArray<string>()

        match client.SendStream("SELECT id, v FROM t ORDER BY id", rows.Add) with
        | Streamed([ "id"; "v" ], 2) -> ()
        | other -> Assert.Fail(sprintf "STREAM の結果が一致しません: %A" other)

        Assert.Equal<string list>([ "1,'a'"; "2,'b'" ], List.ofSeq rows)

        // ストリーム後も同じ接続で次の要求を送れる。
        let _, counts = okRows (client.Send "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "2" ], counts)

        // 対象外は OK フォールバックとして読む。DML は RowsAffected を返す。
        let fallbackRows = ResizeArray<string>()

        match client.SendStream("INSERT INTO t VALUES (3, 'c')", fallbackRows.Add) with
        | Affected 1 -> ()
        | other -> Assert.Fail(sprintf "OK フォールバックの結果が一致しません: %A" other)

        Assert.Empty fallbackRows)

[<Fact>]
let ``Client: SendStream は STREAM・OK・途中 ERR を読む`` () =
    withFakeServer
        (fun line ->
            if line.StartsWith(".stream STREAM", System.StringComparison.Ordinal) then
                Some "STREAM\tc1,c2\n1,2\n3,4\nEND"
            elif line.StartsWith(".stream FALLBACK", System.StringComparison.Ordinal) then
                Some "OK 1\tc\n1"
            else
                Some "STREAM\tc\n1\nERR boom")
        (fun port ->
            use c = new RykDbClient("127.0.0.1", port)
            let rows = ResizeArray<string>()

            match c.SendStream("STREAM", rows.Add) with
            | Streamed([ "c1"; "c2" ], 2) -> ()
            | other -> Assert.Fail(sprintf "STREAM の結果が一致しません: %A" other)

            Assert.Equal<string list>([ "1,2"; "3,4" ], List.ofSeq rows)

            let fallbackRows = ResizeArray<string>()

            match c.SendStream("FALLBACK", fallbackRows.Add) with
            | Streamed([ "c" ], 1) -> ()
            | other -> Assert.Fail(sprintf "OK フォールバックの結果が一致しません: %A" other)

            Assert.Equal<string list>([ "1" ], List.ofSeq fallbackRows)

            let partialRows = ResizeArray<string>()

            match c.SendStream("ERROR", partialRows.Add) with
            | Failure m -> Assert.Equal("boom", m)
            | other -> Assert.Fail(sprintf "途中の ERR が Failure ではありません: %A" other)

            Assert.Equal<string list>([ "1" ], List.ofSeq partialRows))

[<Fact>]
let ``TCP: .stream は SessionOptions の結果上限を適用しない`` () =
    let options =
        { SessionOptions.Default with
            MaxResultRows = 1L
            MaxResultBytes = 1L
        }

    withTestServerOptions options "tcp-stream-limit" (fun port ->
        use client = new RykDbClient("127.0.0.1", port)
        okRows (client.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore
        okRows (client.Send "INSERT INTO t VALUES (1), (2), (3)") |> ignore

        // 通常の SELECT は上限で失敗する。
        match client.Send "SELECT id FROM t ORDER BY id" with
        | Failure m -> Assert.Contains("上限", m)
        | other -> Assert.Fail(sprintf "通常 SELECT が上限エラーになりません: %A" other)

        // .stream は materialize しないので 3 行すべて読める。
        let rows = ResizeArray<string>()

        match client.SendStream("SELECT id FROM t ORDER BY id", rows.Add) with
        | Streamed([ "id" ], 3) -> ()
        | other -> Assert.Fail(sprintf "STREAM の結果が一致しません: %A" other)

        Assert.Equal<string list>([ "1"; "2"; "3" ], List.ofSeq rows))

[<Fact>]
let ``TCP: STREAM の途中エラーは ERR を返し接続を維持する`` () =
    withTestServer "tcp-stream-mid-error" (fun port ->
        use setup = new RykDbClient("127.0.0.1", port)
        okRows (setup.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        match setup.Send "INSERT INTO t VALUES (1), (2), (3)" with
        | Affected 3 -> ()
        | other -> Assert.Fail(sprintf "INSERT の影響行数が一致しません: %A" other)

        use raw = new TcpClient("127.0.0.1", port)
        raw.ReceiveTimeout <- 10000
        let stream = raw.GetStream()
        use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
        use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
        writer.AutoFlush <- true

        // 1行目は返り、2行目の評価で 0 除算になる。
        writer.WriteLine ".stream SELECT 1 / (id - 2) AS x FROM t ORDER BY id"
        Assert.Equal<string>("STREAM\tx", reader.ReadLine())
        Assert.Equal<string>("-1", reader.ReadLine())

        let errorLine = reader.ReadLine()
        Assert.True(errorLine.StartsWith("ERR ", System.StringComparison.Ordinal), errorLine)

        // エラー後も同じ接続で次の要求を送れる。
        writer.WriteLine ".ping"
        Assert.Equal<string>("OK 0\tpong", reader.ReadLine()))

[<Fact>]
let ``TCP: STREAM 中に切断してもサーバは次の接続を受け付ける`` () =
    withTestServer "tcp-stream-disconnect" (fun port ->
        use setup = new RykDbClient("127.0.0.1", port)
        okRows (setup.Send "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)") |> ignore

        let values =
            [ 1..2000 ]
            |> List.map (fun i -> sprintf "(%d, 'value-%d')" i i)
            |> String.concat ", "

        okRows (setup.Send("INSERT INTO t VALUES " + values)) |> ignore

        let raw = new TcpClient("127.0.0.1", port)
        let stream = raw.GetStream()
        use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
        use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
        writer.AutoFlush <- true

        writer.WriteLine ".stream SELECT id FROM t ORDER BY id"
        Assert.Equal<string>("STREAM\tid", reader.ReadLine())
        Assert.Equal<string>("1", reader.ReadLine())

        // 読み切らずに RST で切断する（サーバ側の STREAM 送信を失敗させる）。
        raw.Client.LingerState <- new LingerOption(true, 0)
        raw.Dispose()

        // サーバが前の接続を後始末しても、新しい接続は処理できる。
        use client = new RykDbClient("127.0.0.1", port)

        match client.Send ".ping" with
        | Rows([ "pong" ], []) -> ()
        | other -> Assert.Fail(sprintf "ping が一致しません: %A" other)

        let rows = ResizeArray<string>()

        match client.SendStream("SELECT id FROM t WHERE id <= 3 ORDER BY id", rows.Add) with
        | Streamed([ "id" ], 3) -> ()
        | other -> Assert.Fail(sprintf "再 STREAM の結果が一致しません: %A" other)

        Assert.Equal<string list>([ "1"; "2"; "3" ], List.ofSeq rows))

[<Fact>]
let ``TCP: transaction 中の .stream SELECT は OK にフォールバックする`` () =
    withTestServer "tcp-stream-transaction" (fun port ->
        use setup = new RykDbClient("127.0.0.1", port)
        okRows (setup.Send "CREATE TABLE t (id INTEGER PRIMARY KEY)") |> ignore

        match setup.Send "INSERT INTO t VALUES (1), (2)" with
        | Affected 2 -> ()
        | other -> Assert.Fail(sprintf "INSERT の影響行数が一致しません: %A" other)

        use raw = new TcpClient("127.0.0.1", port)
        raw.ReceiveTimeout <- 10000
        let stream = raw.GetStream()
        use writer = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true)
        use reader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 1024, true)
        writer.AutoFlush <- true

        writer.WriteLine "BEGIN"
        Assert.Equal<string>("OK 0", reader.ReadLine())

        writer.WriteLine ".stream SELECT id FROM t ORDER BY id"
        Assert.Equal<string>("OK 2\tid", reader.ReadLine())
        Assert.Equal<string>("1", reader.ReadLine())
        Assert.Equal<string>("2", reader.ReadLine())

        writer.WriteLine "COMMIT"
        Assert.Equal<string>("OK 0", reader.ReadLine())

        writer.WriteLine ".ping"
        Assert.Equal<string>("OK 0\tpong", reader.ReadLine()))
