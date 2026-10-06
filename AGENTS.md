# AGENTS.md

RykDb（**net10.0 / F# 10** の組込み RDBMS、LSM-Tree + MVCC + WAL）を作業するときの指針。

## 参照先（詳細はこの 4 ファイルに集約済み。複製しない）

| ファイル                                         | 内容                                                      |
| ------------------------------------------------ | --------------------------------------------------------- |
| [`README.md`](README.md)                         | クイックスタート、起動手順、性能の目安                    |
| [`docs/architecture.md`](docs/architecture.md)   | 内部設計（レイヤ構成・実行モデル・LSM・MVCC・既知の制約） |
| [`docs/sql-reference.md`](docs/sql-reference.md) | 対応 SQL の文法一覧（実装から機械的に抽出）               |
| [`docs/gotchas.md`](docs/gotchas.md)             | **実際に踏んだ落とし穴 30 件以上**（症状 → 原因 → ❌/✅） |

## ビルドと検証

```bash
dotnet build -t:Rebuild
dotnet test
```

- **`TreatWarningsAsErrors=true`**。警告が 1 件出たらビルドは落ちる。
- `dotnet test` は**逐次**で走らせる（並行はビルドロック競合で謎の Error が出る）。
- fantomas は `dotnet fantomas <path>`。整形後は必ず `dotnet build -t:Rebuild`
  （FS0020「戻り値の無視」などが整形後に増える）。
- **tail call 監査は 3 層**: ① build = `[<TailCall>]` 付き関数の非末尾（FS3569）
  ② `dotnet run --project tools/TailAudit` = 属性の無い関数（ローカル `let rec` 含む）の構文木検査
  （既知分は `tools/TailAudit/TailAudit.baseline` と差分、新規があれば exit 1）
  ③ `dotnet test` = スタック深度の実測（`末尾再帰:` 系）。手順は `tools/TailAudit/Program.fs` のヘッダ参照。

## コンパイル順（F# は順序依存）

依存の要点:

- `Types` → `Serialization` → `Ast` → `Lexer` → `Parser`
- `RowStore.fs` は **`Catalog.fs` より前**（Catalog が RowStore に依存する）
- `Catalog` → `Executor` → `Planner` → `Engine` → `Database` の順

## Core の規約（`src/RykDb.Core/` のみ）

1. **`mutable` / `while` / `ref` を使わない。** 現状 0 件。
2. **再帰は末尾位置の `match` + `let rec` で書く。**
   `>>=` や `|>` を挟むと F# の末尾呼び出し最適化が効かず、
   大きいデータで **Stack overflow（SIGABRT / exit 134）** になる。
   ```fsharp
   // ✅
   let rec go (cur: Op) : Result<T, DbError> =
       match next cur with
       | Error e -> Error e
       | Ok None -> Ok done
       | Ok(Some(row, nxt)) -> rows.Add row; go nxt
   ```
3. **点検索・prefix 検索に `Memtable.rangeScan` を使わない**（O(n) になる）。
   `Memtable.getVisible` / `Dml.scanByIndexPrefix` を使う。
4. **pull 演算子（`Op`）の「継続」を作り直さない。**
   `Some(row, go nxt)` ではなく `Some(row, nxt)` を返す（`docs/gotchas.md` §2.2）。
5. **エラーは例外で投げない。** すべて `Result<'T, DbError>` で返す。

## 設計上の不可侵ルール

- **keyspace 分離**。テーブルは `t:<name>`、索引は `i:<name>`、カタログは `__catalog__`。
  ディレクトリ名は keyspace の base64。keyspace をまたぐ write batch は 1 本の WAL レコードにまとめる。
- **順序保持符号化**。キーのバイト順が SQL 比較順と一致していなければならない
  （`Serialization.fs` の型タグ順／符号反転／NUL escape）。
- **SeqNo は単調増加**。put と tombstone に同じ SeqNo を書くと **tombstone が消える**。
- **MANIFEST 反映 → 入力ファイル削除**の順。逆順はデータ損失。
- **GC は版単位**。`SeqNo < activeSnapshot` の版を捨てる（ファイル単位では SeqNo 判定をしない）。
- **`Database` は不変ハンドル**。`Execute` / `Query` は新しい `Database` を返すので
  **戻り値を持ち替える**（持ち忘れると書き込みが失われる）。

## テスト

- 1 テスト = 1 関数。`let ``説明`` () = ...` の**関数形**（FS0842: 関数でないと黙って無視される）。
- ファイルは**テスト対象のソースに対応**させる（`src/RykDb.Core/Executor.fs` → `tests/RykDb.Tests/ExecutorTests.fs`）。
  モジュール名も `module RykDb.ExecutorTests` と対象名に合わせる。
- 共通ヘルパー（`ok` / `withDir` / `asInt` / 乱数生成）は `TestHelpers.fs` に置く（`.fsproj` では**先頭**）。
  ファイル固有の fixture（スキーマなど）は各ファイルに private で置く。
- 新しいファイルは `.fsproj` の Compile 順（= src のコンパイル順）に追加する。
- 一時ディレクトリは `Path.GetTempPath()` + `Ticks`。`try/finally` で必ず削除する。
- `TestHealthTests.fs` に**「Fact が 0 件のテストモジュールが無い」ことを検出するテスト**がある（リフレクションで数える）。ファイルを増やしたら下限数のアサートも更新する。

## 変更後のチェックリスト

- [ ] `dotnet build -t:Rebuild` が 0 error / 0 warning
- [ ] `dotnet test` が全緑（逐次実行）
- [ ] `dotnet run --project tools/TailAudit` が exit 0（新規の非末尾再帰なし。追認は内容確認のうえ `-- --update`）
- [ ] `docs/gotchas.md` に**同じ原因の再発**があれば追記
