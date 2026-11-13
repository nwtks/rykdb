# 不変条件レジストリ

RykDb の「型では守れない不変条件」を 1箇所に集約する。
F# の型システムでは表現できないため、
**スマートコンストラクタ / `validate` / プロパティテスト / ソース検査 / 実行時検証** を組み合わせて守る。

設計の全体像は [`architecture.md`](architecture.md)、
実際に踏んだ落とし穴は [`gotchas.md`](gotchas.md) を参照。

---

## 使い方

- 新しい不変条件を導入したら、この表に 1行追加する。
- 「担保」列は必ず具体的な関数・テスト名・ツールを書く（コメントだけにしない）。
- 不変条件を変更したら、対応するテストも同時に更新する。
- 本番経路で検出できるものは `Result` で返す。
  テスト専用の検査は `validate` として `internal` で公開する。

## 担保の種類

| 種類           | 意味                                                  | 例                                                 |
| -------------- | ----------------------------------------------------- | -------------------------------------------------- |
| 型             | DU / record / newtype / units of measure で表現できる | `SqlValue`、`DataType`、`TruthValue`               |
| コンストラクタ | 生ケースを隠し、スマートコンストラクタだけを使う      | `Op.empty` / `Op.ofRow` / `Op.emit` / `Op.compute` |
| `validate`     | テストから呼ぶ `Result` ベースの検査関数              | `Version.validate`、`TransactionManager.validate`  |
| プロパティ     | ランダム入力で性質を検査する                          | `OrderingPropertyTests`                            |
| ソース検査     | 禁止された構文・構築を grep するテスト                | `OpTests` の「構築はスマートコンストラクタに限る」 |
| 実行時         | 本番経路で `Result` として検出する                    | チェックサム、SSTable のキー順検証                 |

## 一覧

| ID     | 不変条件                                               | 担保                                                                                                                      | 状態     |
| ------ | ------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------- | -------- |
| INV-01 | `Op` / `SstScan` は one-shot                           | `Op.emit` / `Op.compute`、`TestHelpers.oneShot`、`SstScan.ofStep` / `ofArray` / `ofState`、`SstScan.withScan`、ソース検査 | ✅ 完了  |
| INV-02 | 返した行は DB 内部と独立                               | `SqlValue.copyRow`、`Engine.runSelect`、`RowReader.TryRead`                                                               | ✅ 完了  |
| INV-03 | キー順の定義は 1つ                                     | `KeyOrder.compare`、`Entry.compareKeys`、`SstEntry.compare`                                                               | ✅ 完了  |
| INV-04 | `FileNumber` と `SeqNo` を混ぜない                     | （未対策）units of measure / newtype                                                                                      | 計画     |
| INV-05 | `TransactionManager` の SeqNo / Active / LastCommitted | `TransactionManager.validate`                                                                                             | ✅ 完了  |
| INV-06 | `Version` のレベル・番号・範囲                         | `Version.validate`                                                                                                        | ✅ 完了  |
| INV-07 | compaction GC は可視集合を保存                         | `Compaction.gcGroup` と可視性プロパティテスト                                                                             | ✅ 完了  |
| INV-08 | MANIFEST は追記専用・番号は単調                        | `VersionEdit` encode/decode + 再オープンのプロパティテスト                                                                | ✅ 完了  |
| INV-09 | WAL は commit 済み prefix だけ再生                     | `Wal.foldCommitted` の全切断位置テスト                                                                                    | ✅ 完了  |
| INV-10 | keyspace 接頭辞が混ざらない                            | `Keyspace.prefixedKey` と個別テスト                                                                                       | 拡張予定 |
| INV-11 | ヘッダ長 = 行幅 = 射影長                               | `SelectProjection.create`（式・表示ヘッダ・参照ヘッダを一度に構築）                                                       | ✅ 完了  |
| INV-12 | `Cursor` は `0 <= Pos <= End <= Data.Length`           | `Cursor.createSlice` の検証                                                                                               | 拡張予定 |
| INV-13 | `BloomFilter.bitCount` は 8 の倍数                     | 個別テスト                                                                                                                | 拡張予定 |
| INV-14 | `Memtable.ApproxBytes` は `Σ Entry.managedBytes`       | 個別テスト                                                                                                                | 拡張予定 |
| INV-15 | `RowReader` は Open / Exhausted / Closed のいずれか    | `closed` / `exhausted` の bool 2つ                                                                                        | 拡張予定 |
| INV-16 | `Session` のロック規約                                 | `stateGate` / `writeGate` とコメント                                                                                      | 拡張予定 |
| INV-17 | lease の所有権                                         | `ProcessLockHandle` / `ProcessLockLease` / `RegistrationLease`                                                            | ✅ 完了  |
| INV-18 | `Expr.children` / `plugChildren` の arity              | `Ast` の個別テスト                                                                                                        | 拡張予定 |

### INV-01 SstScan

- 構築は `SstScan.ofStep` / `ofArray` / `ofState` / `ofFilter` / `ofScoped` に限定する。
- `SstScan.withScan` は開いた走査を使い、成功・失敗のどちらでも `Dispose` する。
- テスト専用の `oneShotScan` が、同じ `SstScan` 値の 2回目の `next` を検出する。
- 回帰テスト:
  - `SstScan: 構築はスマートコンストラクタに限る`（ソース検査）
  - `SstScan.withScan: 成功・失敗のどちらでも Dispose する`
  - `SstScan: one-shot ガードは同じ値の 2回目の next を検出する`
  - `SstScan: filter / toArray はソースを 1回だけ消費する`

### INV-02 返した行は DB 内部と独立

- 宣言: `StatementResult.Rows` と `RowReader` が返す行は、呼び出し側が書き換えても DB の内容に影響しない。
- 理由: `byte[]` / `SqlValue array` は可変。内部配列をそのまま返すと memtable / SSTable を壊す。
- 担保:
  - `SqlValue.copyRow` が行配列と Blob の `byte[]` を複製する。
  - `Engine.runSelect` が materialize した行に適用する。
  - `RowReader.TryRead` がストリーミングの行に適用する。
- 回帰テスト: `Database: 返した行を書き換えても DB の内容は変わらない`。
- 注意: 内部 API（`Memtable` / `Transaction` / `SstEntry`）は所有権を移す契約。
  呼び出し後にキー・行の配列を変更しない。

### INV-03 キー順の単一権威

- 宣言: エントリの順序は「キー昇順・同一キーは SeqNo 降順」。
- 理由: SSTable のマージ・範囲走査・GC・memtable の Treap が同じ順序を前提にする。
- 担保: `KeyOrder.compare` が唯一の実装。
  `Entry.compareKeys`（memtable）と `SstEntry.compare`（SSTable）はこれを呼ぶ。
- 回帰テスト:
  - `順序保持プロパティ: Entry と SstEntry の並びが一致する`
  - `順序保持プロパティ: KeyOrder は反対称で推移的`

### INV-05 TransactionManager

- 宣言:
  - `NextSeqNo > LastCommitSeqNo`
  - `Active` の TxnId はマップのキーと一致し、`NextTxnId` より小さい
  - `Active` の snapshot は `LastCommitSeqNo` 以下
  - `LastCommitted` の SeqNo は `0 < seq <= LastCommitSeqNo` かつ `seq < NextSeqNo`
- 担保: `TransactionManager.validate`。
- 回帰テスト:
  - `TransactionManager.validate: 初期状態は妥当`
  - `TransactionManager.validate: NextSeqNo の逆転を検出`
  - `TransactionManager.validate: Active の snapshot が未来なら検出`
  - `TransactionManager.validate: LastCommitted の範囲外を検出`
  - `TransactionManager.validate: begin/put/commit/abort を繰り返しても不変条件を保つ`
- 補足: 以前の `TxnRecord.Keys` は更新されない死んだフィールドだったため削除した。
  `Active` は `Map<TxnId, Transaction>` で直接持つ。

### INV-06 Version

- 宣言:
  - `Files.Length = Levels.Count`
  - ファイル番号は一意で、`NextFileNumber` より小さい
  - `Level` は `0..6` で、格納位置と一致する
  - 各レベルのファイルは `Smallest` 昇順
  - L1 以上は範囲が重ならない（L0 は重なってよい）
  - `Smallest <= Largest`
- 担保: `Version.validate`。
- 回帰テスト:
  - `Version.validate: 空の Version は妥当`
  - `Version.validate: ファイル番号の重複を検出`
  - `Version.validate: NextFileNumber の巻き戻しを検出`
  - `Version.validate: L1 以上の範囲重複を検出`
  - `Version.validate: L0 の範囲重複は許す`
  - `Version.validate: レベルと格納位置の不一致を検出`
  - `Version.validate: L0 の追加・削除を繰り返しても不変条件を保つ`

### INV-07 Compaction GC

- `Compaction.gcGroup`（純粋関数）が唯一の GC 判定。
- 回帰テスト: `Compaction.gcGroup: 任意の snapshot で可視行が変わらない`。
  ランダムな版・snapshot・tombstone 設定で、件数の会計、SeqNo の一意性、
  `activeSnapshot` 以降の全 snapshot の可視行が不変であることを検査する。

### INV-08 MANIFEST

- `VersionEdit.encode` / `decode` の round-trip。
- `Manifest.logAndApply` で追記した編集を `Manifest.openOrCreate` で再読込しても同じファイル集合になる。
- 回帰テスト:
  - `VersionEdit: ランダムな編集を encode/decode しても同じ`
  - `Manifest: ランダムな VersionEdit を追記して再オープンしても同じファイル集合`
- 補足: `VersionEdit.apply` は `NextFileNumber` を進めない。再オープン時の採番は
  `StorageEngine` がディスク上の最大番号から再構築する。

### INV-09 WAL

- `Wal.foldCommitted` は任意のバイト位置で切っても、commit 済みトランザクションの prefix だけを再生する。
- 回帰テスト: `Wal.foldCommitted: 任意の位置で切っても commit 済み prefix だけ再生する`。

### INV-11 ヘッダと行幅

- `SelectProjection.create` が `*` / `t.*` の展開を 1回だけ行い、
  `Expressions`（行の値）と `DisplayHeader` / `ReferenceHeader`（ヘッダ）を同時に作る。
- `Ops.project` と `Plan.runSelect` は同じ `SelectProjection` を使う。
- 回帰テスト:
  - `SelectProjection: ヘッダと式の列数が一致する`
  - `SelectProjection: 項目が空なら入力スキーマをそのまま使う`

### INV-17 lease

- `ProcessLockHandle` が所有権ハンドルを表し、`IsOwner` はこの型だけを受け取る。
- `RowStore` のエラー経路も `ProcessLockLease.ForceRelease` を使う。
  `ProcessLock.release` の呼び出しは lease 内だけ。
- 回帰テスト: 既存のプロセスロック / 古いハンドルの Close テスト。

---

## 既知の穴（次の候補）

- `FileNumber` / `SeqNo` は素の数値。units of measure / newtype 化は未実施。
- `Cursor` / `Writer` / `Entry` / `SstEntry` の record リテラルは直接構築できる（ソース検査なし）。
- `RowReader` の状態は `closed` / `exhausted` の bool 2つで、状態 DU にはなっていない。
- `Session` / `EngineHost` の並行操作をランダムに組み合わせるストレステストは未実施。
- `BloomFilter.bitCount` / `Memtable.ApproxBytes` の会計は個別テストのみ。
- 内部 API（`Memtable` / `Transaction` / `SstEntry`）のキー・行の所有権はコメント契約のまま。
