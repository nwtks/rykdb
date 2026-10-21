# 開発ガイド

RykDb（**.NET 10 / F# 10**）のビルド・テスト・品質ゲート。
設計は [`architecture.md`](architecture.md)、
コーディング規約は [`coding-conventions.md`](coding-conventions.md)、
実際に踏んだ落とし穴は [`gotchas.md`](gotchas.md) を参照。

---

## 目次

1. [ビルドと品質ゲート](#1-ビルドと品質ゲート)
2. [tail call 監査](#2-tail-call-監査)
3. [テスト構成](#3-テスト構成)
4. [変更後のチェックリスト](#4-変更後のチェックリスト)

---

## 1. ビルドと品質ゲート

```bash
dotnet build -t:Rebuild       # ビルド
dotnet test                   # テスト
dotnet tool restore           # fantomas インストール
dotnet fantomas src/ tests/   # 整形
```

| ゲート                  | 設定                              |
| ----------------------- | --------------------------------- |
| `TreatWarningsAsErrors` | **有効**                          |
| `LangVersion`           | `10.0`                            |
| `WarningLevel`          | `3`                               |
| Core の命令形要素       | `mutable` / `while` / `ref` = 0件 |

> **fantomas 整形後は必ず `dotnet build -t:Rebuild` で FS0020 を確認する**
> （[`gotchas.md`](gotchas.md) §10.4）。

> **dotnet build / dotnet test を並行して走らせない**
> （幻影エラーの詳細は [`gotchas.md`](gotchas.md) §9.4）。

## 2. tail call 監査

再帰が末尾位置から外れるとスタックオーバーフローに直結するため、3層で検出する。

1. **ビルド** — `[<TailCall>]` 属性付き関数の非末尾再帰を FS3569 として検出（`TreatWarningsAsErrors` によりエラー）。
2. **TailAudit** — `dotnet run --project tools/TailAudit` が構文木を走査し、属性の無い 関数（ローカル `let rec` を含む）の非末尾再帰を検出。
   既知分は `tools/TailAudit/TailAudit.baseline` と差分し、**新規があれば exit 1**。
   （ベースラインは `dotnet run --project tools/TailAudit -- --update` で生成する。
   **未生成なら全件を新規とみなして exit 1 になる**ので、内容を確認のうえ生成しておく。）
   - 非末尾は**「ラムダ内 / 直接」の内訳**を表示する。
     ラムダ内 = `Compute` / `pull` 継続・CPS 継続（深度は 1行ごと / `MaxExpressionDepth` で頭打ち）。
     直接 = 監査部・引数など「結果を消費してから」型（**本来の末尾化候補**）。
     ベースラインのキーには含めない（内訳が変わっても差分化しないため）。
   - pipe の**関数側**は末尾として数える（`x |> f a` = `f a x`）。
     値側は非末尾のまま。
3. **テスト** — スタック深度の実測（`末尾再帰:` 系テスト）。

`[<TailCall>]` は構文上の検査にすぎない。
`Directory.Build.props` の `<Tailcalls>true</Tailcalls>` により Debug でも実際の `tail.` を出力する。
この設定を外すと CPS の相互再帰がテストでスタックオーバーフローする（[`gotchas.md`](gotchas.md) §8.16）。
CPS でのスタック深度とその上限は [`coding-conventions.md`](coding-conventions.md) §3 を参照。

## 3. テスト構成

xUnit。
**テスト対象のソースファイルごとに 1ファイル**に分ける
（`src/RykDb.Core/Executor.fs` → `tests/RykDb.Tests/ExecutorTests.fs`）。
モジュール名も `module RykDb.ExecutorTests` と対象名に合わせる。

```bash
dotnet test                                 # 全件
dotnet test --filter "RykDb.ServerTests"    # Server のみ
```

**テスト方針**: `DatabaseTests` / `ServerTests` は「SQL 文字列 → 結果」の通し経路しか検証しない。
Parser / Planner / Executor / Catalog の連携バグはそこだけで分かる。

**書き方**:

- 1テスト = 1関数。
  `let ``説明`` () = ...` の**関数形**（FS0842: 関数でないと黙って無視される）。
- 共通ヘルパー（`ok` / `withDir` / `asInt` / 乱数生成）は `TestHelpers.fs` に置く（`.fsproj` では**先頭**）。
  ファイル固有の fixture（スキーマなど）は各ファイルに private で置く。
- `withStore` / `withDb` は `IStorageEngine` のメモリ実装で動く（ディスク I/O なし）。
  ディスク固有の挙動（WAL / MANIFEST / 永続化）は `withDir` + `RowStore.openStore` / `Database.openWith` を直接使って検証する。
- 新しいファイルは `.fsproj` の Compile 順（= src のコンパイル順）に追加する。
- 一時ディレクトリは `Path.GetTempPath()` + `Ticks`。
  `try/finally` で必ず削除する。
- `TestHealthTests.fs` に**「Fact が 0 件のテストモジュールが無い」ことを検出するテスト**がある
  （リフレクションで数える）。
  F# 10 の FS0842 への対策（[`gotchas.md`](gotchas.md) §9.1）。
  ファイルを増やしたら下限数のアサートも更新する。
  `DatabaseTests` に `E2E` / `性能`、`ServerTests` に `TCP` のテスト名が残っていることも検査する。

**乱数生成のテストは退行に注意**（[`gotchas.md`](gotchas.md) §9.6）。

## 4. 変更後のチェックリスト

- [ ] `dotnet build -t:Rebuild` が 0 error / 0 warning
- [ ] `dotnet test` が全緑（逐次実行）
- [ ] `dotnet run --project tools/TailAudit` が exit 0（新規の非末尾再帰なし。追認は内容確認のうえ `-- --update`）
- [ ] `docs/gotchas.md` に**同じ原因の再発**があれば追記
