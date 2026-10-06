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
