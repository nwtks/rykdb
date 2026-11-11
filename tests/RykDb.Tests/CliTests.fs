// ===========================================================================
// CLI の SQL 実行・表示（ファイル実行も同じ経路を使う）
// ===========================================================================

module RykDb.CliTests

open System.IO
open Xunit
open RykDb.Core
open RykDb.Cli
open RykDb.TestHelpers

let private lines (text: string) =
    text.Replace("\r\n", "\n").TrimEnd('\n').Split '\n' |> Array.toList

[<Fact>]
let ``Cli: ファイル実行の SELECT を逐次表示する`` () =
    withDir "cli-file-stream" (fun dir ->
        use session = ok (Session.OpenWith dir)
        use out = new StringWriter()

        let script =
            """
CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT);
INSERT INTO t VALUES (1, 'a'), (2, 'b');
SELECT id, v FROM t ORDER BY id;
"""

        match runScript out session script with
        | Ok() -> ()
        | Error e -> Assert.Fail(sprintf "スクリプトが失敗しました: %s" (DbError.toDisplayString e))

        Assert.Equal<string list>(
            [ "(0 rows)"; "(2 rows)"; "id | v"; "------"; "  1, 'a'"; "  2, 'b'" ],
            lines (out.ToString())
        ))

[<Fact>]
let ``Cli: ファイル実行の SELECT は SessionOptions の結果上限を適用しない`` () =
    let options =
        { SessionOptions.Default with
            MaxResultRows = 1L
            MaxResultBytes = 1L
        }

    withDir "cli-file-stream-limit" (fun dir ->
        use session = ok (Session.OpenWith(dir, options))
        use out = new StringWriter()

        let script =
            """
CREATE TABLE t (id INTEGER PRIMARY KEY);
INSERT INTO t VALUES (1), (2), (3);
SELECT id FROM t ORDER BY id;
"""

        match runScript out session script with
        | Ok() -> ()
        | Error e -> Assert.Fail(sprintf "スクリプトが失敗しました: %s" (DbError.toDisplayString e))

        Assert.Equal<string list>([ "(0 rows)"; "(3 rows)"; "id"; "--"; "  1"; "  2"; "  3" ], lines (out.ToString())))

[<Fact>]
let ``Cli: SELECT の途中エラーで停止し、後続の文を実行しない`` () =
    withDir "cli-file-stream-error" (fun dir ->
        use session = ok (Session.OpenWith dir)
        use out = new StringWriter()

        let script =
            """
CREATE TABLE t (id INTEGER PRIMARY KEY);
INSERT INTO t VALUES (1), (2), (3);
SELECT 1 / (id - 2) AS x FROM t ORDER BY id;
INSERT INTO t VALUES (4);
"""

        match runScript out session script with
        | Error _ -> ()
        | Ok() -> Assert.Fail "SELECT の途中エラーが Ok になっています"

        let expected = [ "(0 rows)"; "(3 rows)"; "x"; "-"; "  -1" ]
        Assert.Equal<string list>(expected, lines (out.ToString()))

        let result = ok (session.Execute "SELECT COUNT(*) FROM t")

        Assert.Equal<string list>(
            [ "3" ],
            result.Rows
            |> Array.map (Array.map SqlLiteral.toSqlLiteral >> String.concat ",")
            |> Array.toList
        ))
