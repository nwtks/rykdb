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
    let db = ok (Database.openWith dir)

    // port 0 = OS が空いているポートを割り当てる。
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()

    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve db listener cts.Token (fun _ -> ())
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
        db.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

        // タスクは Dispose しない（実行中の task の Dispose は危険）。

let private okRows (r: Reply) =
    match r with
    | Rows(cols, rows) -> cols, rows
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
            okRows (client.Query "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")

        Assert.Equal<string list>([], cols)
        okRows (client.Query "INSERT INTO t VALUES (1, 'a'), (2, 'b')") |> ignore

        let cols, rows = okRows (client.Query "SELECT id, name FROM t ORDER BY id")

        Assert.Equal<string list>([ "id"; "name" ], cols)
        Assert.Equal<string list>([ "1,'a'"; "2,'b'" ], rows)

        // 集約も通る
        let _, rows = okRows (client.Query "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "2" ], rows)

        // エラーは ERR で返る（HALT しない）
        match client.Query "SELECT * FROM nope" with
        | Failure m -> Assert.Contains("nope", m)
        | other -> Assert.Fail(sprintf "ERR を期待しましたが: %A" other)

        // ping
        let cols, _ = okRows (client.Query ".ping")
        Assert.Equal<string list>([ "pong" ], cols)

        // 終了
        Assert.True(
            match client.Query ".exit" with
            | Halt -> true
            | _ -> false
        ))

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
            (db0: Database)
            (rest: string list)
            (acc: (string list * string list) list)
            : (string list * string list) list =
            match rest with
            | [] -> List.rev acc
            | sql :: tail ->
                // `Execute` は新しい `Database` を返すので持ち替える。
                let next, r = ok (db0.Execute sql)
                run next tail ((r.Columns |> Array.toList, rowText r) :: acc)

        let db = ok (Database.openWith dir)
        let libResults = run db script []
        ok (db.Close())

        // --- TCP ---
        withServer (fun client ->
            let tcpResults =
                script
                |> List.map (fun sql ->
                    let cols, rows = okRows (client.Query sql)
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
        Assert.Equal<string list>([], fst (okRows (client.Query ".tables")))

        okRows (client.Query "CREATE TABLE zzz (id INTEGER PRIMARY KEY)") |> ignore

        let cols, rows = okRows (client.Query ".tables")
        Assert.Equal<string list>([ "tables" ], cols)
        // 行は SqlLiteral 表記（文字列は引用符つき）で返る
        Assert.Contains("'zzz'", rows))

[<Fact>]
let ``TCP: 空行は空応答になり、切断後も接続を受け付ける`` () =
    let dir = newDir "tcp-raw"
    let db = ok (Database.openWith dir)
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve db listener cts.Token (fun _ -> ())
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
        db.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

[<Fact>]
let ``TCP: 突然の切断（RST）でもサーバは生き続ける`` () =
    let dir = newDir "tcp-rst"
    let db = ok (Database.openWith dir)
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve db listener cts.Token (fun _ -> ())
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
        db.Close() |> ignore

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

        match c.Query "x" with
        | Failure _ -> ()
        | other -> Assert.Fail(sprintf "Failure ではない: %A" other))

[<Fact>]
let ``Client: 不正な件数は Failure`` () =
    withFakeServer (fun _ -> Some "OK abc\tcol") (fun port ->
        use c = new RykDbClient("127.0.0.1", port)

        match c.Query "x" with
        | Failure _ -> ()
        | other -> Assert.Fail(sprintf "Failure ではない: %A" other))

[<Fact>]
let ``Client: 応答の列と行を読む`` () =
    withFakeServer (fun _ -> Some "OK 2\tc1,c2\n1,2\n3,4") (fun port ->
        // IDisposable 経由の Close もここで通る
        use c = new RykDbClient("127.0.0.1", port)

        match c.Query "x" with
        | Rows([ "c1"; "c2" ], [ "1,2"; "3,4" ]) -> ()
        | other -> Assert.Fail(sprintf "行列が一致しない: %A" other))

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

        match c.Query "x" with
        | Halt -> ()
        | other -> Assert.Fail(sprintf "Halt ではない: %A" other)
    finally
        listener.Stop()
        task.Wait 5000 |> ignore

[<Fact>]
let ``TCP: TEXT の改行・CR・バックスラッシュが 1行プロトコルを壊さない`` () =
    let dir = newDir "tcp-newline"
    let db = ok (Database.openWith dir)
    let db, _ = ok (db.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)")

    let db, _ =
        ok (db.Execute "INSERT INTO t VALUES (1, 'a\nb'), (2, 'c\rd'), (3, 'C:\\temp')")

    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve db listener cts.Token (fun _ -> ())
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
        let _, rows = okRows (client.Query "SELECT id, v FROM t ORDER BY id")

        Assert.Equal<string list>([ "1,'a\nb'"; "2,'c\rd'"; "3,'C:\\temp'" ], rows)
    finally
        cts.Cancel()
        finished.Wait 10000 |> ignore
        cts.Dispose()
        db.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()

[<Fact>]
let ``TCP: 切断時に open transaction は rollback される`` () =
    let dir = newDir "tcp-rollback"
    let db = ok (Database.openWith dir)
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let cts = new CancellationTokenSource()
    let finished = new ManualResetEventSlim false

    Task.Run(fun () ->
        try
            serve db listener cts.Token (fun _ -> ())
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
            Assert.Equal<string>("OK 0", reader.ReadLine())
        finally
            raw.Dispose()

        // 次の接続では rollback 済みで、行は見えない。
        use client = new RykDbClient("127.0.0.1", port)
        let _, rows = okRows (client.Query "SELECT COUNT(*) FROM t")
        Assert.Equal<string list>([ "0" ], rows)
    finally
        cts.Cancel()
        finished.Wait 10000 |> ignore
        cts.Dispose()
        db.Close() |> ignore

        if Directory.Exists dir then
            try
                Directory.Delete(dir, true)
            with _ ->
                ()
