# 開発ガイド

RykDb のビルド、テスト、品質ゲートを説明する。
設計は [`architecture.md`](architecture.md)、
コーディング規約は [`coding-conventions.md`](coding-conventions.md)、
既知の落とし穴は [`gotchas.md`](gotchas.md) を参照する。

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
dotnet build -t:Rebuild
dotnet test
dotnet tool restore
dotnet fantomas src/ tests/ tools/
dotnet run --project tools/UnusedAudit

# CI と同じゲート。行カバレッジ 90%以上が必要。
dotnet restore --locked-mode
dotnet test -p:CollectCoverage=true -p:Threshold=90 -p:ThresholdType=line -p:ThresholdStat=total
```

SDK とパッケージを固定する。

- `global.json` が SDK を固定する。
- `packages.lock.json` がパッケージのバージョンを固定する。
- パッケージを追加または更新したら `dotnet restore` で lock ファイルを更新してコミットする。

`FSharp.Core` は SDK 同梱の `library-packs` ではなく nuget.org から復元する。
設定は `Directory.Build.props` の `DisableImplicitLibraryPacksFolder` にある。
SDK のビルドごとにパッケージ内容が変わる場合があり、
同じバージョンでも `--locked-mode` が NU1403（contentHash 不一致）で失敗する
（[`gotchas.md`](gotchas.md) §9.15）。

lock ファイルを作り直す場合は、`~/.nuget/packages/fsharp.core/<ver>/` を削除してから
`dotnet restore --force-evaluate` を実行する。

主要なビルド設定は次のとおり。

| 項目                    | 設定                                                |
| ----------------------- | --------------------------------------------------- |
| `TreatWarningsAsErrors` | 有効                                                |
| `LangVersion`           | `10.0`                                              |
| `WarningLevel`          | `3`                                                 |
| `OtherFlags`            | `--warnon:1182`（未使用ローカル変数をエラーにする） |
| 依存の再現性            | `global.json` / `packages.lock.json`                |

ロジック層では原則として `mutable`、`while`、`ref` を使わない。
例外的に可変状態を使う箇所は次のとおり。

- `Session.fs` / `DatabaseHost.fs` の状態 DU（lifecycle・`SyncState`・`RowReaderState`）
- `ValueSet.IndexedValues` の reader と 1ブロックキャッシュ
- `BloomFilter.BloomBuilder` の件数カウンタ

fantomas の整形後は、必ず次のコマンドで FS0020 を確認する。

```bash
dotnet build -t:Rebuild
```

この確認が必要な理由は [`gotchas.md`](gotchas.md) §10.9 にある。

`dotnet build` と `dotnet test` は並行して実行しない。
並行実行時の見かけ上のエラーは [`gotchas.md`](gotchas.md) §9.13 にある。

### 未使用 private 宣言の監査

F# コンパイラは未使用のローカル束縛（FS1182）しか検出しない。
`let private`・private 型・private メンバーは検出しないため、
`tools/UnusedAudit` が `src/` を監査する。

```bash
dotnet run --project tools/UnusedAudit              # 新規 0 件なら exit 0
dotnet run --project tools/UnusedAudit -- --all     # 既知分も含めて全件表示
dotnet run --project tools/UnusedAudit -- --update  # 内容確認後にベースラインを更新
```

検出対象は次のとおり。

- module 直下の `let private`（値・関数）
- `module private`
- `type private`（DU はケース名、レコードはフィールド名の使用も型の使用とみなす）
- `member private`、`member val private`、`val private`

class 内の `let` は FS1182 が担当するため対象外（`--warnon:1182` で検出済み）。
判定は同じファイル内の識別子トークンとの名前一致で行う。
同名の別宣言が使われていれば「使用済み」とみなす（偽陽性を避ける）。

リフレクション・シリアライズ等で意図的に残す宣言は、
内容を確認してからベースライン（`tools/UnusedAudit/UnusedAudit.baseline`）へ追認する。
演算子・アクティブパターン・private な union case / レコードフィールド・`.fsi`・`tests/` は対象外。

## 2. tail call 監査

再帰呼び出しの後に処理が残る非末尾再帰は、入力が大きいとスタックオーバーフローを起こす。
次の 3層で検出する。

1. **ビルド**

   `[<TailCall>]` を付けた関数の非末尾再帰を FS3569 として検出する。
   `TreatWarningsAsErrors` により警告はエラーになる。

2. **TailAudit**

   次のコマンドで構文木を調べる。

   ```bash
   dotnet run --project tools/TailAudit
   ```

   属性を付けていない関数を含め、ローカルの `let rec` も検査する。
   検出結果は `tools/TailAudit/TailAudit.baseline` と比較する。
   新しい項目がある場合は exit 1 になる。

   ベースラインは次のコマンドで更新する。

   ```bash
   dotnet run --project tools/TailAudit -- --update
   ```

   ベースライン未生成の場合も全項目を新規とみなして exit 1 になる。
   更新前に検出内容を確認する。

   監査では非末尾再帰を次の 2種類に分類する。
   - ラムダ内
     `Compute`、`pull` の継続、CPS 継続、Pratt の右辺降下などにある。
     Parser の CPS 継続呼び出しは構文上は `bare` と数えるが、実行時は `tail.` になる。
     この項目はベースラインに含める。
   - 直接
     監査処理や引数の評価など、結果を使ってから再帰する型である。
     本来は末尾再帰へ直せる候補になる。
     分類が変わっても差分にならないよう、ベースラインのキーには含めない。

   pipe の関数側（`x |> f a`）は末尾として数える。
   値側は非末尾のまま扱う。

3. **テスト**

   テスト名が「末尾再帰:」で始まるテストで実際のスタック使用量を確認する。

`[<TailCall>]` は構文検査だけで、コード生成は `Directory.Build.props` の
`<Tailcalls>true</Tailcalls>` が担う。
この設定を外すと、Plan のサブクエリ解決や式の再帰でスタックオーバーフローが発生する
（[`gotchas.md`](gotchas.md) §9.9）。
再帰の書き方は [`coding-conventions.md`](coding-conventions.md) §3 を参照する。

## 3. テスト構成

テストフレームワークは xUnit を使う。

原則として、テスト対象のソースファイルごとに対応するテストファイルを1つ用意する。
モジュール名は対象名に合わせる。
たとえば `Dml.fs` に対応するテストモジュールは `module RykDb.DmlTests` とする。

| 対象                    | テスト                          |
| ----------------------- | ------------------------------- |
| `src/RykDb.Core/Dml.fs` | `tests/RykDb.Tests/DmlTests.fs` |
| `Session.fs` 公開 API   | `SessionTests.fs`               |
| `DatabaseHost.fs`       | `DatabaseHostTests.fs`          |

例外的なテスト構成は次のとおり。

- `DatabaseTests.fs` はソースファイルに対応せず、`Session` / `SessionPool` 経由の DB 全体の通し経路を検証する。
- `CrashConsistencyTests.fs` はソースファイルに対応せず、CURRENT スロット・WAL 名・SSTable 名の
  消失からの復旧（[`crash-consistency.md`](crash-consistency.md) の C1/C2）を検証する。
- `ProcessLock.fs` は専用ファイルを持たず、`RowStoreTests` / `DatabaseTests` で間接的に検証する。
- `EngineAnalyze.fs` / `EngineDdl.fs` は専用ファイルを持たず、`DatabaseTests` / `SqlReferenceTests` の E2E で検証する。
- `CommitDurability:` と `SerializationTests` の「順序保持:」は横断的な性質を検証する。
- `CliTests` は CLI 全体を検証する。
- SQL の構文と意味論は [`sql-reference.md`](sql-reference.md) に対応する `SqlReferenceTests.fs` に書く。

全テストと個別テストは次のコマンドで実行する。

```bash
dotnet test
dotnet test --filter "FullyQualifiedName~RykDb.ServerTests"
```

### 通し経路

- `SqlReferenceTests`
  - SQL の文、句、式、リテラル、識別子の受理と拒否を検証する。
  - 評価結果が仕様どおりか確認する。
  - テスト名には `SqlReference:` を付ける。
- `DatabaseTests`
  - MVCC、WAL、プロセスロック、索引の backfill と後始末、永続性、末尾再帰を検証する。
  - Parser、Analyzer、Plan、PlanBuilder、ExecContext、Catalog の連携も検証する。
- `CrashConsistencyTests`
  - CURRENT の A/B スロット、WAL / SSTable のファイル名消失、compaction 出力の消失、
    WAL v2 のフレームヘッダー破損を検証する。
  - 復旧を 2 回繰り返しても同じ manifest を指すこと（冪等性）を検証する。

### テストの書き方

- 1テストは 1関数とする。
- 関数名は `let ``説明`` () = ...` の形にする。
- FS0842 により、値として定義したテストは認識されない。

共通ヘルパーは `TestHelpers.fs` に置く。
このファイルは `.fsproj` の先頭に登録する。
ファイル固有のヘルパーや fixture は各テストファイル内で private として置く。
同じ実装を 2ファイル以上で使うヘルパーは重複させず、`TestHelpers.fs` に集約する。

失敗時の検証方法は用途に合わせて選ぶ。

- variant の不一致や想定外の unit 腕は `failActual "ラベル" 実値` を使う。
  メッセージではなく実値を `%A` で必ず表示する。
- エラーになるべき処理は `expectError` または `expectErrorKind` を使う。
- 値を返すヘルパーは `failwithf` を使う。
- 値を返すヘルパーで `Assert.Fail` と `Unchecked.defaultof<'T>` の組み合わせを使わない。
  xUnit が例外を失敗として扱う。

`withStore` と `withSession` はメモリ上の `IStorageEngine` を使う。
WAL、MANIFEST、永続化などディスク固有の挙動は `withDir` と `RowStore.openStore`
または `Session.OpenWithSync` を直接使って検証する。

新しいテストファイルは `.fsproj` の Compile 順に登録する。
原則として、ソースファイルと同じコンパイル順にする。
一時ディレクトリは `Path.GetTempPath()`、`Ticks`、GUID で衝突しない名前にする。
`try/finally` で必ず削除する。

`TestHealthTests.fs` は、Fact を持たないテストモジュールがないことをリフレクションで検査する。
F# 10 の FS0842 に対策するためである（[`gotchas.md`](gotchas.md) §9.1）。
テストファイルを追加したら、下限件数のアサートも更新する。

### 並び順

テストは、テスト対象のソースファイルにある関数の定義順に並べる。
対象ファイルで A、B、C の順に定義している場合、テストファイルでも A、B、C のテストの順にする。

同じ関数のテストは次の順に並べる。

1. 正常系
2. 境界値、空入力、NULL、並行実行などの特殊状況
3. エラー系

ヘルパー定義はテストブロックの間に置かず、ファイル先頭にまとめる。
テストの並び替えで参照順が壊れるのを防ぐ。

例外として、次のファイルはソースの定義順ではなく利用者に見える流れを優先する。

- `SqlReferenceTests` は [`sql-reference.md`](sql-reference.md) の節順に並べる。
- `DatabaseTests`、`ServerTests`、`CliTests` は操作とプロトコルの流れに並べる。
- `SessionTests` は主要な公開 API の順に並べる。
  `Session`、`SessionOptions`、`RowReader`、`SessionPool` の順を基本とする。

乱数を使うテストはシードを固定し、失敗を再現できるようにする。

## 4. 性能ベンチ

性能に影響する変更は `tools/RykDb.Bench` で前後を測定する。
ベンチマークの数値はマシン依存なので、同じマシンでの比較だけに使う。

```bash
# 全シナリオ
dotnet run -c Release --project tools/RykDb.Bench

# compaction
dotnet run -c Release --project tools/RykDb.Bench -- --class compaction

# 索引の読み書きとサイズ
dotnet run -c Release --project tools/RykDb.Bench -- --class index

# 式評価器
dotnet run -c Release --project tools/RykDb.Bench -- --class eval

# Op（pull 層）
dotnet run -c Release --project tools/RykDb.Bench -- --class op

# ストレージ層のポイント検索（Version lease の有無）
dotnet run -c Release --project tools/RykDb.Bench -- --class storage

# 短いジョブ
dotnet run -c Release --project tools/RykDb.Bench -- --quick

# 1回だけ実行して正しさを確認
dotnet run -c Release --project tools/RykDb.Bench -- --dry

# BenchmarkDotNet のフィルターでシナリオを絞る
dotnet run -c Release --project tools/RykDb.Bench -- --class query --quick --filter "*QueryBenchmarks.PointLookup" --filter "*PointLookupParsed*"
```

- CI はビルドだけを実行する。ベンチマークは手元で実行する。
- 結果は実行時のカレントディレクトリにある `BenchmarkDotNet.Artifacts/` に保存される（git 対象外）。

主なシナリオは次のとおり。

- `PointLookup`、`PointLookup500`、`CountStar`、`RangeScan`、`FilterSeqScan`、`SelectConstant`、`CountConstantWhere`
- `ParsePoint`: 字句解析と構文解析だけを測定する。
- `PointLookupParsed`、`PointLookupPlanned`、`PointLookupStorage`:
  同じポイント検索を、構文解析済み、計画済み、ストレージ直呼び出しで測定する。
  `PkLookup` が実行する `byPrimaryKeyMasked` のオーバーヘッドを分離できる。
- `StorageBenchmarks` の `RowStorePointLookup` と `EnginePointLookup`:
  `RowStore.getAtMasked`（Version lease あり）と `IStorageEngine.GetAtMasked`（なし）を比べる。
  計画や実行の変更はこの3シナリオで確認する。
  `PointLookup` は計画が支配的なため、ストレージの変化が埋もれる。
- `Insert100Rows`、`Insert10kRows`: INSERT のスループットを測定する。
- `Join3Tables`、`MergeJoin2Tables`、`IndexNestedLoop2Tables`、`JoinWithResidual`、`NestedLoopNoMatch`:
  結合を測定する。
  4つのアルゴリズム（Hash / Merge / INL / NL）を同じデータで比べられる。
- `Join4TablesSparse`、`Join4TablesDense`:
  4テーブル結合の結合順を測定する。
  前者は直積を含む疎なグラフ、後者は全テーブルが結合述語で繋がる密なグラフ。
- `Join4TablesSparseDp`、`Join4TablesSparseGreedy`、`Join4TablesDenseDp`、`Join4TablesDenseGreedy`:
  parse / plan を除き、DP と貪欲の計画を実行して比べる。
  `Session.PrepareSelectWith(JoinOrderStrategy.Greedy, stmt)` で貪欲計画を作る。
- `Join4TablesSparseNoStats`、`Join4TablesDenseNoStats`:
  同じ形・同じ行数で `ANALYZE` していないテーブル（`k1..k4`）を使い、統計ありとの差を測る。
- `DistinctSpill`、`GroupBySpill`、`Join3TablesSpill`、`JoinByValueSpill`、`SortSpill`:
  作業メモリを1バイトにして退避経路を強制する（`--class spill`）。
  通常経路の同名シナリオと比較する。
  正常経路と退避経路のバジェットを照合し、誤った退避判定を検出する。
- `MatchesLiteralV`、`MatchesColumnV`、`MatchesBinaryV`、`MatchesComplexV`:
  式評価器を DB なしで測定する。
- `MatchesComplexJoinedV`: JOIN の ON 句の式評価を測定する。
- `CompareValuesV`: `SqlValue.compareValuesV` の割当を測定する。
- `OpMaterialize`、`OpFilterMapTake`、`NextStep`: `Op` の pull 層を DB なしで測定する。
- `IndexCoveredLookup`、`IndexFetchLookup`、`IndexCoveredCount`、`IndexFetchRange`:
  被覆読み、非被覆読み、件数読み取りを測定する。
- `InsertWithIndex100Rows`、`InsertNoIndex100Rows`: 索引あり・なしで100行 INSERT を比較する。
- `CompactL0`: L0 の4ファイルを1回 compaction する。

計画と実行の測定では、`Session.PrepareSelect`（internal）で計画だけを組み、
`Optimizer.exec` と `Op.toArrayLimited` を繰り返す。
実行ごとに `Scope.DisposeAll()` を呼ぶ。

割当の見方には注意する。

- `EvalBenchmarks` の `Allocated` は `Result` の `Ok` を反映しない。
- Release の JIT が式評価の `Ok` をスタック割当または除去する。
- `DbResult` 化の前後で592Bのままになる場合がある。
- `Result` の割当は `OpBenchmarks` とクエリ系で確認する。
- 実測値は [`gotchas.md`](gotchas.md) §7.10 にある。

性能スモークは次の流れで確認する。

1. 10,000行を INSERTする。
2. point lookup を500回実行する。
3. `COUNT(*)` を実行する。

実時間の上限でテストを落とさない。
マシン依存になるため、ベンチマークの前後比較で確認する。

クエリ系ベンチマークは次の手順で初期化する。

1. `Session.OpenWithSync`（同期 compaction）で開く。
2. シード後に一度 Closeする。
3. memtable を SSTableへflushしてから測定する。

この手順で背景 compaction のタイミングによる揺らぎを避ける。
compaction の出力削除は `IterationCleanup` の外側で行う。

変更前後で `--quick` の結果が大きく変わった場合は、最初に `--dry` で正しさを確認する。
確認後に通常ジョブで再測定する。

## 5. 変更後のチェックリスト

- [ ] `dotnet build -t:Rebuild` が 0 error / 0 warning
- [ ] `dotnet test` が全件成功（逐次実行）
- [ ] `dotnet run --project tools/TailAudit` が exit 0
  - 新規の非末尾再帰がないこと。
  - ベースラインに追認する項目は、更新前に内容を確認する。
- [ ] `dotnet run --project tools/UnusedAudit` が exit 0
  - 新規の未使用 private 宣言がないこと。
  - ベースラインに追認する項目は、更新前に内容を確認する。
- [ ] 不変条件を追加または変更した場合は、[`invariants.md`](invariants.md) と対応テストを更新した
- [ ] 構築に不変条件がある型は `.fsi` で表現を隠した
  - `[<Struct>]` レコードは不可（[`gotchas.md`](gotchas.md) §9.11）。
- [ ] 性能に影響する変更は `tools/RykDb.Bench` で前後を測定した
  - 測定できない場合は理由を記録した。
- [ ] 再発する落とし穴は [`gotchas.md`](gotchas.md) に追記した
