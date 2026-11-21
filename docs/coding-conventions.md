# コーディング規約

RykDb の実装で守る規約。
設計は [`architecture.md`](architecture.md)、
落とし穴は [`gotchas.md`](gotchas.md) を参照。

## 目次

1. [例外ではなく `Result` を使う](#1-例外ではなく-result-を使う)
2. [可変状態を局所化する](#2-可変状態を局所化する)
3. [末尾再帰とスタック安全性を保つ](#3-末尾再帰とスタック安全性を保つ)
4. [予約語に注意する](#4-予約語に注意する)
5. [クロスプラットフォームにする](#5-クロスプラットフォームにする)
6. [型と所有権の境界を守る](#6-型と所有権の境界を守る)

---

## 1. 例外ではなく `Result` を使う

このプロジェクトでは、例外を制御フローに使わない。
失敗は原則として `Result<'T, DbError>` を返す。例外定義はない。

合成関数は `Errors.fs` の `[<AutoOpen>] module ResultOps` に集約する。

| 構文・関数       | 用途                           |
| ---------------- | ------------------------------ |
| `( >>= )`        | `Result` の連結                |
| `ret` / `err`    | 成功値・エラーの作成           |
| `all` / `foldOk` | 複数項目の短絡評価             |
| `result { ... }` | `DbResultBuilder` による計算式 |
| `protect`        | .NET I/O API 境界での例外捕捉  |

`all` と `foldOk` は最初の `Error` で処理を終了し、残りの項目を評価しない。
複数の失敗があっても、先に発生したエラーだけを返す。

`protect` は主な例外変換経路。

- `IOException` / `UnauthorizedAccessException` → `IoError`
- `InvalidDataException` / `FormatException` / `OverflowException` → `CorruptionError`
- ファイル内容の不整合は `InternalError` にせず、データ破損として扱う

例外境界は次の用途だけに限定する。

- `try` / `with`:
  `protect`、`Manifest.decodeRecord` の base64 解析、`Manifest.append` の追記失敗時の切り詰め、
  `VersionCoordinator` の worker ループと `Shutdown`、`Session.fs` の `RowReader` 解放
- `try` / `finally`:
  `Engine`、`Compaction`、`VersionCoordinator`、`SstScan`、`Session`、
  `ProcessLock`、`StorageEngine`、`Transactions` のリソース解放

例外は外へ伝播させず、`DbError` に変換するか、解放処理では例外を限定的に処理する。

### エラー分類

`DbError.Kind`（`ErrorKind`）で失敗を分類する。

| `ErrorKind`                                          | 意味                                 |
| ---------------------------------------------------- | ------------------------------------ |
| `SyntaxError` / `NameError` / `TypeError`            | 字句・構文、名前解決、型の失敗       |
| `ConstraintViolation` / `NotFound` / `AlreadyExists` | DML・DDL の失敗                      |
| `CapacityError`                                      | memtable・バッファのメモリ上限       |
| `CorruptionError`                                    | チェックサム不一致などのファイル破損 |
| `ConflictError`                                      | MVCC の write-write 競合             |
| `IoError` / `InternalError`                          | I/O 失敗 / 内部不整合                |

---

## 2. 可変状態を局所化する

ロジック層（型、字句解析、構文解析、計画、実行、ストレージ）では、
原則として `mutable`、`while`、`ref` を使わない。

可変状態は完全に廃止するものではなく、使用箇所を明示して閉じ込める。

### 許可する mutable 状態

- `ValueSet.IndexedValues`
  - 複数ブロックをまたぐ reader、直近 1ブロックのキャッシュ、`disposed`
  - 1つの評価スレッドから順に使用する
- `BloomFilter.BloomBuilder`
  - 構築中の追加済みハッシュ数
- `DatabaseHost.lifecycle`
  - `HostLifecycle = Running | Closed`
- `Session.local` / `Session.readers`
  - 接続ごとの `LocalSession = Open | Closed` と実行中の読み取り数
- `SessionPool.lifecycle`
  - `PoolLifecycle = Open | Closed`
- `RowReader.state`
  - `RowReaderState = Reading | Exhausted | Closed`
- `CommitDurability.state`
  - `SyncState = Idle | Leading | Failed`

公開セッション層以外の可変状態は、原則として次の抽象で表現する。

| 用途               | 使用する型                                     |
| ------------------ | ---------------------------------------------- |
| 書き込み           | 不変チャンク列の `Writer`                      |
| 読み取り位置       | 値として保持する `Cursor` / `PState`           |
| 走査・畳み込み     | `Result` を返す末尾再帰                        |
| ホットパスの成功値 | `[<Struct>] DbResult<'T>`（`DbOk` / `DbErr`）  |
| 平衡木の再構築     | `list` を明示スタックとして使用                |
| 行ごとの列解決     | 計画時に準備する不変値と `ImmutableDictionary` |
| 遅延列挙           | `Op` と `OpStep`                               |

`DbResult` はホットパス専用。公開境界では `Result` を返すラップ関数を使う。
関数名末尾の `V` は、`Result` ではなく `DbResult` を扱う値ベース版を示す。

### 同期とコレクション

共有状態は短い `lock` または非同期プリミティブで保護する。

- `stateGate`: `Session` の状態・読み取り数、`DatabaseHost` のライフサイクル
- `writeGate`: バッチ、`Begin`、`Commit`、`Rollback`、`Close` の直列化
- `gate`: `ValueSet.IndexedValues` の reader と 1ブロックキャッシュ
- `Monitor.Wait` / `PulseAll`: 読み取りの終了待ち、グループコミット

Core の可変コレクションは用途を限定する。
`ResizeArray` は使わず、行・キー・アキュムレータには `list` / `array` を使う。

- `Ops.aggregate`: グループ表
- `Ops.distinct`: `HashSet<byte array>`
- `OpsJoin.innerJoinHash` / `leftJoinHash`: 右側ハッシュ表
- `VersionCoordinator`: 公開状態、ファイル refcount、worker 状態
- `StorageEngine`、`ProcessLock`、`Transactions`: 同期・登録台帳

演算子の作業用コレクションは、演算子の構築時に作り、返した `Op` の中だけで保持する。
モジュールスコープに置くと実行ごとの状態が共有され、2回目以降の結果が壊れる。

回帰テスト:

- `SqlReference: DISTINCT を続けて実行しても毎回全行が返る`
- `SqlReference: GROUP BY とハッシュ結合を続けて実行しても毎回同じ結果になる`

---

## 3. 末尾再帰とスタック安全性を保つ

末尾再帰は、要素数に比例する処理でスタックフレームの蓄積を防ぐ。

`Directory.Build.props` の `<Tailcalls>true</Tailcalls>` により、
Debug ビルドでも F# コンパイラが `tail.` を生成する。
`[<TailCall>]` は構文上の末尾性を検査し、コード生成は切り替えない。
両方を組み合わせて、末尾性の検査と実行時のスタック安全性を確保する。

### 基本方針

- 要素数に比例する反復は `match` と末尾再帰で実装する
- ローカルの頻繁な再帰は避け、モジュールレベルの `let rec` にする
- `result { ... }` をループ内で使わない
- 式やサブクエリの深さに上限を設けない
- 深さカウンタでスタック増加を止める設計にしない
- 継続と走査フレームはヒープ上の値・リストに保持する

`result { ... }` は `Delay` によりクロージャを生成し、末尾最適化が効かない。
500000トークンのループで `StackOverflow` が発生したため、ループ関数では使用しない。

### 実装パターン

**パーサーと式評価**

- 式と SELECT の構文解析は CPS（継続渡し）で実装する
- `parseExprC`、`parseExprBpC`、`parseExprLoopC` などが例
- Pratt parser では、中置演算子をループで処理し、右辺だけ結合強度で再帰する
- 左結合の右辺は `lbp + 1`、比較・述語の右辺は `PredicateRightBp` を使う
- 比較・述語は非結合とし、2つ目が来たら `SyntaxError` にする
- 前置演算子は、それ以上に結合する式の右辺だけで受理する
- CPS で自己再帰がすべて末尾位置の関数には `[<TailCall>]` を付ける
- 継続ラムダ内の相互再帰など、属性検査が失敗する箇所には付けない

深い括弧とサブクエリは、`ParserExprTests` の次のテストで確認する。

- `末尾再帰: 1000000段の深い括弧の入れ子もスタックを食わない`
- `末尾再帰: 5000段の深いサブクエリの入れ子も解析できる`

**式の書き換えと評価**

- 葉の書き換えは `Expr.rewriteWithC` または `Expr.rewriteWith` を使う
- `Expr.rewrite` は `RewriteFrame` に未処理ノードを積み、ローカル再帰で組み直す
- `Eval.step` はタグ付きの継続 `Kont` を使い、自己再帰だけを末尾再帰にする
- ホットパスは `[<Struct>] DbResult<'T>` を返し、公開境界で `Result` に変換する
- `Op.next` は `[<Struct>] OpStep`（`OpDone` / `OpError` / `OpRow`）を返す
- 行ループの継続は `Op.emit` に直接渡さず、
  `Op.compute (fun () -> OpStep.OpRow(row, next))` として遅延する
- `Optimizer.exec` は `ExecFrame` と `ExecStep` に分解し、論理木を反復処理する

`Result` の参照型 DU を行ごとに生成しないこと、深い式、深い IN、長い WHEN 節、
大量の行読み出しではスタックを一定に保つことが重要。

**繰り返し処理**

- IN の候補リストと CASE の WHEN 節は `match` による末尾再帰で処理する
- カーソルからの項目読み出しは `Reader.readItems` に集約する
- WHERE の AND は `Predicate.flattenLoop` で平坦化する
- 左偏ヒープのマージは右スパインを `picked` リストへ移し、`List.fold` で組み直す

`Plan.runSubquery` と `StorageEngine.scanAtWith.pull` など、
継続を関数値として渡す処理は TailAudit では保守的に非末尾として数えられる。
実行時は実呼び出しが末尾位置になるため、これらは TailAudit のベースラインで承認する。

### `[<TailCall>]` の基準

関数本体の自己再帰がすべて末尾位置の場合だけ属性を付ける。
1箇所でも末尾でなければ FS3569になり、`TreatWarningsAsErrors` によりビルドエラーになる。

構造上末尾再帰にできない箇所は、属性を付けず、呼び出し直前に理由をコメントする。

- `Op.map` / `Op.take`: 再帰が `Compute` 内にある
- `Sstable.rangeScanPartial` の `scanEntries` / `loadBlock`: ブロック読みが後続処理へ遅延する
- `SstScan.emit`: 続きの組み立てがラムダ内にある
- `Plan.runSubquery`: 自分自身を runner として渡す

逆に、要素数に比例する反復は末尾再帰へ変更する。

### 回帰テスト

- `SstMerge: 多数のソースをまとめてマージできる`
- `StorageEngine.scanAtValues: 多数のファイルをキー昇順にマージする`
- `Compaction.compact: 多数の入力ファイルをストリーミングでマージする`
- `Analyzer.chooseScanMethod: 20000段の AND を chooseScanMethod してもスタックを食わない`
- `Binder.qualify: 全ノード種を qualify しても構造が保たれる`
- `末尾再帰: 50000段の式を havingFilter してもスタックを食わない`

さらに詳しい検査内容は [`development.md`](development.md) の tail call 監査を参照。

---

## 4. 予約語に注意する

`key`、`base`、`begin`、`new`、`old`、`type`、`value` は F# の予約語。
匿名レコードのフィールド名にも使えない。

衝突例と回避策は [`gotchas.md`](gotchas.md) §9.5 を参照。

---

## 5. クロスプラットフォームにする

特定の OS に依存する実装を避ける。

パス操作、ファイル I/O、プロセス間排他には、`System.IO.Path`、`Environment`、`FileShare`
などのクロスプラットフォーム API を使用する。
OS 固有の区切り文字、コマンド、API を直接呼ばない。

CI では Linux、Windows、macOS の3環境で検証する。

---

## 6. 型と所有権の境界を守る

一覧と根拠は [`invariants.md`](invariants.md)、
具体的な書き方は [`gotchas.md`](gotchas.md) を参照。

### 異なる意味の番号を型で区別する

同じ素の型でも意味が異なる番号は、単位付き型で分ける（INV-04）。

- `FileNumber` と `ManifestNumber`
- `SeqNo` と `TxnId`

型略記と単位は `Types.fs` に定義する。
`Wal.fs` より前にコンパイルされる必要がある。
直列化の境界だけで `uint64 x` または `LanguagePrimitives.Int64WithMeasure` に変換する。

### 配列の所有権を明確にする

保持する側は、渡された配列を複製してから保持する（INV-19）。

- `Memtable.insert`: `Entry.takeOwnership`
- `Transaction.put` / `remove`: `setSlot`

読み取り専用のビューである `SstEntry` は複製しない。
公開する行は `SqlValue.copyRow` により呼び出し側と切り離す（INV-02）。

### 状態は bool ではなく DU で表現する

状態遷移を2つの `bool` で表現しない（INV-15）。
状態が増えると、両方 `true` の組み合わせや判定順序への依存が生じる。

代わりに判別共用体を使用し、解放など副作用が1回だけ起こる遷移は状態関数内に閉じ込める。
