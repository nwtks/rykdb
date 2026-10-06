# RykDb

**net10.0 / F# 10** で書かれた、**LSM-Tree ストレージ + MVCC + WAL** を備えた組込み RDBMS。

テーブル本体も索引も **B-Tree ではなく LSM ツリー（keyspace 単位）** で保持する。
ライブラリ API・SQL REPL・TCP サーバの 3 つのインターフェースを持つ。

---

## 目次

- [クイックスタート](#クイックスタート)
- [性能の目安](#性能の目安)
- [付属ドキュメント](#付属ドキュメント)

---

## クイックスタート

### 必要なもの

- **.NET 10 SDK**

### ビルドとテスト

```bash
dotnet build -t:Rebuild      # ビルド
dotnet test                  # テスト
```

### SQL REPL を起動

```bash
dotnet run --project src/RykDb.Cli -- ./mydata
```

```
RykDb REPL
DB: ./mydata
`.help` でコマンド一覧、`.exit` で終了

db> CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER);
(0 rows)
db> INSERT INTO people VALUES (1, 'alice', 30), (2, 'bob', 25);
(2 rows)
db> SELECT id, name FROM people ORDER BY id;
id | name
---------
  1, 'alice'
  2, 'bob'
db> SELECT age, COUNT(*) FROM people GROUP BY age ORDER BY age;
age | COUNT(*)
--------------
  25, 1
  30, 1
db> .tables
  people
db> .exit
```

-dot コマンド:

| コマンド                | 説明                                        |
| ----------------------- | ------------------------------------------- |
| `.tables`               | テーブル一覧                                |
| `.commit` / `.rollback` | 開いている transaction を確定／破棄         |
| `.help`                 | コマンド一覧                                |
| `.exit` / `.quit`       | 終了（`Close` して memtable を flush する） |

-スクリプト実行:

```bash
dotnet run --project src/RykDb.Cli -- -f schema.sql ./mydata
```

### TCP サーバを起動

```bash
dotnet run --project src/RykDb.Server -- -p 7090 ./mydata
```

```
RykDb Server listening on port 7090
DB: ./mydata
Ctrl+C で終了します。
```

### ライブラリ API

```fsharp
open RykDb.Core

// Result から中身を取り出す例示用のヘルパー。
let ok =
    function
    | Ok v -> v
    | Error e -> failwith (DbError.toDisplayString e)

let db = ok (Database.openWith "./mydata")

// `Execute` は**新しい Database と結果のペアを返す**。戻り値を持ち替えること。
let db, _ = ok (db.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)")
let db, _ = ok (db.Execute "INSERT INTO t VALUES (1, 10), (2, 20)")

let db, result = ok (db.Query "SELECT id, v FROM t ORDER BY id")
printfn "%A" result.Rows          // [|[|Int 1L; Int 10L|]; [|Int 2L; Int 20L|]|]
printfn "%A" result.Columns       // [|"id"; "v"|]
printfn "%A" result.RowsAffected  // 2

result.ToDisplayLines()          // [ "id | v"; "------"; "  1, 10"; "  2, 20" ]
db.TableNames()                  // [ "t" ]
db.Close()
```

---

## 性能の目安

Debug ビルド・単一スレッド・10 万行（`INTEGER` 主キー 1 列 + `INTEGER` 1 列）:

| 操作                                | 所要時間                |
| ----------------------------------- | ----------------------- |
| INSERT 10 万行（2,000 行ずつ bulk） | 約 3.0 秒               |
| 点検索 200 回                       | 約 0.06 秒（0.3 ms/回） |
| 全走査（`COUNT(*)`）                | 約 0.2 秒               |

Release ビルドではこれより速くなる。Bloom filter の bits/key とレベルファウト比は未調整（既定のまま）。

---

## 付属ドキュメント

| ファイル                                         | 内容                                                      |
| ------------------------------------------------ | --------------------------------------------------------- |
| [`docs/architecture.md`](docs/architecture.md)   | 内部設計（レイヤ構成・実行モデル・LSM・MVCC・既知の制約） |
| [`docs/sql-reference.md`](docs/sql-reference.md) | 対応 SQL の文法一覧・型・関数・エラー・既知の制限         |
| [`docs/gotchas.md`](docs/gotchas.md)             | 実際に踏んだ落とし穴と対策（30 件以上）                   |
| [`AGENTS.md`](AGENTS.md)                         | 開発・変更時の作業指針（ビルド・テスト・規約）            |
