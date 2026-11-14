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
4. [性能ベンチ](#4-性能ベンチ)
5. [変更後のチェックリスト](#5-変更後のチェックリスト)

---

## 1. ビルドと品質ゲート

```bash
dotnet build -t:Rebuild       # ビルド
dotnet test                   # テスト
dotnet tool restore           # fantomas インストール
dotnet fantomas src/ tests/   # 整形

# CI と同じゲート（coverage 90% 必須）
dotnet restore --locked-mode
dotnet test -p:CollectCoverage=true -p:Threshold=90 -p:ThresholdType=line -p:ThresholdStat=total
```

依存は `global.json` で SDK を、`packages.lock.json` でパッケージ版を固定する。
パッケージを追加・更新したときは `dotnet restore` で lock ファイルを更新してコミットする。

`FSharp.Core` は SDK 同梱の `library-packs` ではなく nuget.org から復元する
（`Directory.Build.props` の `DisableImplicitLibraryPacksFolder`）。
同じバージョン番号でも SDK のビルドごとに中身が異なり、
`--locked-mode` が NU1403（contentHash 不一致）で落ちるため
（[`gotchas.md`](gotchas.md) §9.10）。
lock ファイルを作り直すときは、`~/.nuget/packages/fsharp.core/<ver>/` を消してから
`dotnet restore --force-evaluate` を実行する。

| ゲート                  | 設定                                             |
| ----------------------- | ------------------------------------------------ |
| `TreatWarningsAsErrors` | **有効**                                         |
| `LangVersion`           | `10.0`                                           |
| `WarningLevel`          | `3`                                              |
| `OtherFlags`            | `--warnon:1182`（未使用ローカル変数もエラー）    |
| 依存の再現性            | `global.json`（SDK） / `packages.lock.json`      |
| Core の命令型構文・機能 | `mutable` / `while` / `ref` は `Session.fs` のみ |

> **fantomas 整形後は必ず `dotnet build -t:Rebuild` で FS0020 を確認する**
> （[`gotchas.md`](gotchas.md) §10.4）。

> **dotnet build / dotnet test を並行して実行しない**
> （並行実行時に発生する見かけ上のエラーについては [`gotchas.md`](gotchas.md) §9.4 を参照）。

## 2. tail call 監査

再帰呼び出しの後に処理が残る形（非末尾再帰）は、入力が大きいとスタックオーバーフローを起こす。
これを次の 3 層で検出する。

1. **ビルド** — `[<TailCall>]` 属性付き関数の非末尾再帰を FS3569 として検出
   （`TreatWarningsAsErrors` によりエラー）。
2. **TailAudit** — `dotnet run --project tools/TailAudit` が構文木を調べ、
   属性のない関数（ローカル `let rec` を含む）の非末尾再帰を検出する。
   検出結果を `tools/TailAudit/TailAudit.baseline` と比較し、**新しい項目があれば exit 1**。
   （ベースラインは `dotnet run --project tools/TailAudit -- --update` で生成する。
   **未生成なら全件を新規とみなして exit 1 になる**ので、内容を確認のうえ生成しておく。）
   - 非末尾は**「ラムダ内 / 直接」の内訳**を表示する。
     ラムダ内 = `Compute` / `pull` 継続・CPS 継続（深度は 1行ごと、`MaxExpressionDepth` を上限とする）。
     直接 = 監査部・引数など「結果を消費してから」型（**本来の末尾化候補**）。
     ベースラインのキーには含めない（内訳が変わっても差分化しないため）。
   - pipe の**関数側**は末尾として数える（`x |> f a` = `f a x`）。
     値側は非末尾のまま。
3. **テスト** — `末尾再帰:` 系テストで、実際のスタック使用量を確認する。

`[<TailCall>]` は構文上の検査にすぎない。
`Directory.Build.props` の `<Tailcalls>true</Tailcalls>` により Debug でも実際の `tail.` を出力する。
この設定を外すと CPS の相互再帰がテストでスタックオーバーフローする（[`gotchas.md`](gotchas.md) §8.16）。
CPS でのスタック深度とその上限は [`coding-conventions.md`](coding-conventions.md) §3 を参照。

## 3. テスト構成

xUnit。
**テスト対象のソースファイルごとに、対応するテストファイルを 1つ設ける**
（`src/RykDb.Core/Dml.fs` → `tests/RykDb.Tests/DmlTests.fs`）。
モジュール名も `module RykDb.DmlTests` と対象名に合わせる。

**例外**: `Database.fs` の通し経路テストは 2ファイルに分かれる。
`Session.fs` の公開 API は `SessionTests.fs` で検証する。
`ProcessLock.fs` / `EngineHost.fs` は専用ファイルを持たず、
`RowStoreTests` / `BackgroundCompactionTests` / `DatabaseTests` で間接的に検証する。
`DurabilityTests` / `OrderingPropertyTests` は横断的な性質テスト、`CliTests` は CLI 全体のテスト。
[`sql-reference.md`](sql-reference.md) が扱う構文・意味論は `SqlReferenceTests.fs`、
ハンドル API と内部機構は `DatabaseTests.fs`（役割の分担は下の「テスト方針」）。

```bash
dotnet test                                                    # 全件
dotnet test --filter "FullyQualifiedName~RykDb.ServerTests"    # Server のみ
```

**テスト方針**: `DatabaseTests` / `SqlReferenceTests` / `ServerTests` は「SQL 文字列 → 結果」の通し経路しか検証しない。
Parser / Analyzer / Planner / ExecContext / Catalog の連携バグはそこだけで分かる。

**役割の分担**:

- `SqlReferenceTests` — [`sql-reference.md`](sql-reference.md) の構文・意味論
  （文・句・式・リテラル・識別子が受理 / 拒否され、評価結果も仕様どおりであること）。
  テスト名の接頭辞は `SqlReference:`。
- `DatabaseTests` — ハンドル API と内部機構
  （MVCC / WAL / プロセスロック / 索引の backfill・後始末 / snapshot 登録 / 永続性 / 性能 / 末尾再帰）。

**書き方**:

- 1テスト = 1関数。
  `let ``説明`` () = ...` の**関数形**
  （FS0842: 関数でない場合、テストとして認識されず無視される）。
- 共通ヘルパー（`ok` / `withDir` / `asInt` / 乱数生成 / `buildSstable` / `scanOfEntries` / `levelCounts` / `indexDefinition`）は `TestHelpers.fs` に置く（`.fsproj` では**先頭**）。
  ファイル固有のヘルパー・fixture（スキーマ、`DmlTests` の `insertRow` など）は各ファイルに private で置く。
- `withStore` / `withDb` は `IStorageEngine` のメモリ実装で動く（ディスク I/O なし）。
  ディスク固有の挙動（WAL / MANIFEST / 永続化）は `withDir` + `RowStore.openStore` / `Database.OpenWith` を直接使って検証する。
- 新しいファイルは `.fsproj` の Compile 順（= src のコンパイル順）に追加する。
- 一時ディレクトリは `Path.GetTempPath()` + `Ticks`。
  `try/finally` で必ず削除する。
- `TestHealthTests.fs` に**「Fact が 0件のテストモジュールが無い」ことを検出するテスト**がある
  （リフレクションで数える）。
  F# 10 の FS0842 への対策（[`gotchas.md`](gotchas.md) §9.1）。
  ファイルを増やしたら下限数のアサートも更新する。
  `DatabaseTests` に `E2E` / `性能`、`ServerTests` に `TCP` のテスト名が残っていることも検査する。

**並び順**:

- テストは**テスト対象（`src/RykDb.Core/*.fs`）の定義順**に合わせて並べる。
  対象ファイルで関数 A → B → C の順に定義されていれば、
  テストファイルも A のテスト → B のテスト → C のテストの順にする。
- 同一関数のテストは **正常系 → 特殊状況（境界値・空入力・NULL・並行実行など）→ 異常系（エラー）** の順に並べる。
- ヘルパー定義はテストブロックの間に置かず、**先頭にまとめる**。
  テストを並べ替えたときに参照順が壊れるのを防ぐ。
- 例外: `SqlReferenceTests` は `sql-reference.md` の節順に従う（ソースの定義順ではない）。
  `DatabaseTests` / `ServerTests` / `CliTests` は利用者から見た操作・プロトコルの流れを優先する。

**乱数生成のテストは退行に注意**（[`gotchas.md`](gotchas.md) §9.6）。

## 4. 性能ベンチ

性能に影響する変更は `tools/RykDb.Bench` で前後を測る。
数値はマシン依存なので、**同じマシンでの前後比較**にだけ使う。

```bash
dotnet run -c Release --project tools/RykDb.Bench                    # 全シナリオ
dotnet run -c Release --project tools/RykDb.Bench -- --class query  # クエリ系のみ
dotnet run -c Release --project tools/RykDb.Bench -- --quick        # 短いジョブ
dotnet run -c Release --project tools/RykDb.Bench -- --dry          # 1 回だけ実行して検証
```

- **CI ではビルドのみ**（`dotnet build -t:Rebuild` に含まれる）。実行は手元で行う。
- 結果は `BenchmarkDotNet.Artifacts/`（実行時のカレントディレクトリ）に残る。git 対象外。
- シナリオ:
  - `PointLookup` / `CountStar` / `RangeScan` / `FilterSeqScan` / `Insert100Rows`
  - `Join3Tables` / `MergeJoin2Tables` / `JoinWithResidual` / `NestedLoopNoMatch`
  - `CompactL0` — L0 の 4ファイル（各 25,000件）を 1回 compaction する
- クエリ系は `Database.OpenWith`（同期 compaction）で開き、
  シード後に一度 Close して memtable を SSTable へ flush してから測る
  （背景 compaction のタイミングで揺れないようにする）。
- compaction の出力削除は測定の外側（`IterationCleanup`）で行う。
- 変更前後で `--quick` の結果が大きく変わったら、`--dry` で正しさを確認してから通常ジョブで再測する。

## 5. 変更後のチェックリスト

- [ ] `dotnet build -t:Rebuild` が 0 error / 0 warning
- [ ] `dotnet test` が全緑（逐次実行）
- [ ] `dotnet run --project tools/TailAudit` が exit 0（新規の非末尾再帰なし。追認は内容確認のうえ `-- --update`）
- [ ] 不変条件を追加・変更したら [`invariants.md`](invariants.md) と対応するテストを更新
- [ ] 構築に不変条件がある型は `.fsi` で表現を隠したか（`[<Struct>]` レコードは不可。[`gotchas.md`](gotchas.md) §8.21）
- [ ] 性能に影響する変更は `tools/RykDb.Bench` で前後を測ったか（測れない場合は理由を記録）
- [ ] `docs/gotchas.md` に**同じ原因の再発**があれば追記
