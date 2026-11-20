# 不変条件レジストリ

RykDb の「型では守れない不変条件」を 1箇所に集約する。
F# の型システムでは表現できないため、
**スマートコンストラクタ / `validate` / プロパティテスト / `.fsi` による構築制限 / 実行時検証** を組み合わせて守る。

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

| 種類           | 意味                                                  | 例                                                                             |
| -------------- | ----------------------------------------------------- | ------------------------------------------------------------------------------ |
| 型             | DU / record / newtype / units of measure で表現できる | `SqlValue`、`DataType`、`TruthValue`                                           |
| コンストラクタ | 生ケースを隠し、スマートコンストラクタだけを使う      | `Op.fsi`（ケース非公開）と `Op.Empty` / `ofRow` / `emit` / `compute`           |
| `validate`     | テストから呼ぶ `Result` ベースの検査関数              | `VersionSetTests.fs` / `TransactionsTests.fs` 内の private `validate`          |
| プロパティ     | ランダム入力で性質を検査する                          | `SerializationTests`（順序保持） / `VersionCoordinatorTests`（ランダム操作列） |
| 構築制限       | `.fsi` でレコード・DU のケースを隠す                  | `Op.fsi` / `Writer.fsi` / `SstScan.fsi` / `Cursor.fsi` / `Memtable.fsi`        |
| 実行時         | 本番経路で `Result` として検出する                    | チェックサム、SSTable のキー順検証                                             |

## 一覧

| ID     | 不変条件                                                                     | 担保                                                                                                                                                                                                                                   | 状態    |
| ------ | ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------- |
| INV-01 | `Op` / `SstScan` は one-shot                                                 | `Op.emit` / `Op.compute`、`Op.fsi`（Op のケース非公開）、`SstScan.fsi`（SstScan のケース非公開）、`TestHelpers.oneShot`、`SstScan.ofStep` / `ofArray` / `ofState`、`SstScan.withScan`                                                  | ✅ 完了 |
| INV-02 | 返した行は DB 内部と独立                                                     | `SqlValue.copyRow`、`Engine.runSelect`、`RowReader.TryRead`                                                                                                                                                                            | ✅ 完了 |
| INV-03 | キー順の定義は 1つ                                                           | `KeyOrder.compare`、`Entry.compareKeys`、`SstEntry.compare`                                                                                                                                                                            | ✅ 完了 |
| INV-04 | 意味の違う番号（manifest / file / seq / txn）を混ぜない                      | 単位 `manifestNo` / `txnId`（同型ペアの片側だけに付与）                                                                                                                                                                                | ✅ 完了 |
| INV-05 | `TransactionManager` の SeqNo / Active / LastCommitted                       | `TransactionsTests.fs` の private `validate`                                                                                                                                                                                           | ✅ 完了 |
| INV-06 | `Version` のレベル・番号・範囲                                               | `VersionSetTests.fs` の private `validate`                                                                                                                                                                                             | ✅ 完了 |
| INV-07 | compaction GC は可視集合を保存                                               | `Compaction.gcGroup` と可視性プロパティテスト                                                                                                                                                                                          | ✅ 完了 |
| INV-08 | MANIFEST は追記専用・番号は単調                                              | `VersionEdit` encode/decode + 再オープンのプロパティテスト                                                                                                                                                                             | ✅ 完了 |
| INV-09 | WAL は commit 済み prefix だけ再生                                           | `Wal.foldCommitted` の全切断位置テスト                                                                                                                                                                                                 | ✅ 完了 |
| INV-10 | keyspace 接頭辞が混ざらない                                                  | `Keyspace.prefixedKey` の往復・非相互接頭辞テスト、`RowStore` のキー分離テスト                                                                                                                                                         | ✅ 完了 |
| INV-11 | ヘッダ長 = 行幅 = 射影長                                                     | `SelectProjection.create`（式・表示ヘッダ・参照ヘッダを一度に構築）                                                                                                                                                                    | ✅ 完了 |
| INV-12 | `Cursor` は `0 <= Pos <= End <= Data.Length`                                 | `Cursor.fsi`（レコード非公開、構築は `create` / `createSlice`、前進は `advance`）+ `createSlice` の構築時検証 + プロパティテスト                                                                                                       | ✅ 完了 |
| INV-13 | `BloomFilter.BitCount` は 8 の倍数                                           | 築く側 `bitsFor` が 8 単位に丸める + 読む側 `deserialize` の拒否 + パラメータ網羅テスト                                                                                                                                                | ✅ 完了 |
| INV-14 | `Memtable.ApproxBytes` は `Σ Entry.managedBytes`                             | `Memtable.fsi`（構築は `Empty` / `insert` / `apply` に限定）+ `Memtable.insert` の会計とランダム挿入・順序独立性のプロパティテスト                                                                                                     | ✅ 完了 |
| INV-15 | `RowReader` は Reading / Exhausted / Closed のいずれか                       | `RowReaderState`（状態 DU に置き換え、`releaseTo` が遷移と 1回解放を担当）                                                                                                                                                             | ✅ 完了 |
| INV-16 | `Session` のロック規約                                                       | `stateGate` / `writeGate` の設計 + `SessionTests` の並行テスト 6 本                                                                                                                                                                    | ✅ 完了 |
| INV-17 | lease の所有権                                                               | `ProcessLockLease.ForceRelease`（二重解放は無害） + `TxnState.Lease` の `RegistrationLease`（GC で解除） + `DatabaseHostTests` の GC 回帰テスト                                                                                        | ✅ 完了 |
| INV-18 | `Expr.children` / `plugChildren` の arity                                    | `Expr.plugChildren`（モジュールレベルへ引き上げ）+ `AstTests` の全ノード種テスト                                                                                                                                                       | ✅ 完了 |
| INV-19 | 保持する側はキー・行の所有権を複製で取り込む                                 | `Entry.takeOwnership` / `Transaction.setSlot` の複製と回帰テスト                                                                                                                                                                       | ✅ 完了 |
| INV-20 | `Writer.Chunks` は逆順（先頭が最新の書き込み）                               | `Writer.fsi`（ケース非公開、構築は `Empty` / 各 write 関数に限定）+ 順序プロパティテスト                                                                                                                                               | ✅ 完了 |
| INV-21 | UNIQUE の予約は commit まで競合検出に残る                                    | `Transaction.reserveUnique` / `writeKeys` / `TransactionManager.commit` + `TransactionsTests` / `SessionTests` の同時実行テスト                                                                                                        | ✅ 完了 |
| INV-22 | `Session.fs` / `DatabaseHost.fs` の `mutable` は状態 DU のスロットとカウンタ | `SyncState` / `HostLifecycle` / `LocalSession` / `PoolLifecycle` / `RowReaderState`。クエリ局所の `ValueSet.IndexedValues` / `BloomFilter.BloomBuilder` は別枠                                                                         | ✅ 完了 |
| INV-23 | 結果行は `LoadedRow`（`Unloaded` を含まない）                                | `LoadedRow.create` + `StatementResult` の非公開コンストラクタ（`Create` は `LoadedRow` だけを受ける） + `RowReader.TryRead` の戻り値型                                                                                                 | ✅ 完了 |
| INV-24 | `VersionCoordinator` の Version 公開とファイル refcount                      | `VersionCoordinator.publish` / `install` / `AcquireCall` + `VersionCoordinatorTests` の並行・ランダム操作列テスト                                                                                                                      | ✅ 完了 |
| INV-25 | commit の WAL 位置は fsync 前に登録する                                      | `DatabaseHost.serialized` の `writeGate` 内 `Register` + `DatabaseHostTests` の統合テスト（3 commit / 2 fsync）                                                                                                                        | ✅ 完了 |
| INV-26 | 確定状態は transaction を持たない                                            | `Committed`（transaction フィールド無し）+ `HostLifecycle` が `Committed` のみを保持 + 実行時だけ `ExecState` に合成                                                                                                                   | ✅ 完了 |
| INV-27 | 並列制御以外に DB・プロセス寿命の可変キャッシュを持たない                    | `Eval.Prepared`（計画時の `ImmutableDictionary`）+ `FileMetaData.ReadInfo`（不変の `Version` に付随）+ mutable/`ConcurrentDictionary` の許可リスト（Session 層・`VersionCoordinator`・`ProcessLock`・`Transactions`・`SstScan.Scope`） | ✅ 完了 |
| INV-28 | 索引エントリの値は空。索引列 + 主キーはキーから復号し、非被覆列は PK lookup  | `RowKey.indexKey` / `opOfIndexScan` / `indexAccessMasks` / `ExecContext.fetchMissing` + キー復号 golden / 索引 fetch テスト                                                                                                            | ✅ 完了 |
| INV-29 | 退避した一時ファイルは必ず閉じ、読み切ったら消す                             | `Spill.openSink` / `closeSink` / `foldRun`（閉じて消す）/ `foldRunKeep`（閉じるだけ）/ `reopen`（消す）/ `reopenKeep`（消さない）+ `SpillTests` / `OpsTests` / `OpsJoinTests` の「一時ファイルが残らない」検査                         | ✅ 完了 |

---

## 不変条件の詳細

ID 順。宣言・根拠・担保・回帰テストを 1件ずつ示す。

### INV-01 SstScan

- 構築は `SstScan.ofStep` / `ofStepWithClose` / `ofArray` / `ofState` / `ofFilter` / `ofScoped` に限定する。
- `SstScan.withScan` は開いた走査を使い、成功・失敗のどちらでも `Dispose` する。
- ファイルを開いたままの step 走査は `ofStepWithClose` で作る（`Dispose` で閉じる）。
  途中で捨てられた走査がハンドルを残すと、Windows では rename / 削除が共有違反で失敗する。
- テスト専用の `oneShotScan` が、同じ `SstScan` 値の 2回目の `next` を検出する。
- `Op` のケース（`Done` / `Step` / `Compute`）は **`Op.fsi` で隠す**。
  構築は `Op.Empty` / `ofRow` / `emit` / `compute` / `ofArray` に限られ、
  他ファイルからの生ケース構築は**コンパイルエラー**になる。
- `SstScan` のケースも **`SstScan.fsi` で隠す**。
  構築は `ofStep` / `ofStepWithClose` / `ofArray` / `ofState` / `ofFilter` / `ofScoped` に限る。
- 回帰テスト:
  - `SstScan.withScan: 成功・失敗のどちらでも Dispose する`
  - `SstScan: one-shot ガードは同じ値の 2回目の next を検出する`
  - `SstScan: filter / toArray はソースを 1回だけ消費する`
  - `RowStore: compaction 失敗で中断した走査は入力 SSTable のハンドルを残さない`
  - `Compaction.compactWith: 停止したら出力を残さず入力の走査を閉じる`

### INV-02 返した行は DB 内部と独立

- 宣言: `StatementResult.Rows` と `RowReader` が返す行は、呼び出し側が書き換えても DB の内容に影響しない。
- 理由: `byte[]` / `SqlValue array` は可変。内部配列をそのまま返すと memtable / SSTable を壊す。
- 担保:
  - `SqlValue.copyRow` が行配列と Blob の `byte[]` を複製する。
  - `Engine.runSelect` が materialize した行に適用する。
  - `RowReader.TryRead` がストリーミングの行に適用する。
- 回帰テスト: `Session: 返した行を書き換えても DB の内容は変わらない`。
- 注意: 内部 API（`Memtable` / `Transaction` / `SstEntry`）は所有権を移す契約。
  呼び出し後にキー・行の配列を変更しない。

### INV-03 キー順の単一権威

- 宣言: エントリの順序は「キー昇順・同一キーは SeqNo 降順」。
- 理由: SSTable のマージ・範囲走査・GC・memtable の Treap が同じ順序を前提にする。
- 担保: `KeyOrder.compare` が唯一の実装。
  `Entry.compareKeys`（memtable）と `SstEntry.compare`（SSTable）はこれを呼ぶ。
- 回帰テスト:
  - `順序保持: Entry と SstEntry の並びが一致する`
  - `順序保持: KeyOrder は反対称で推移的`

### INV-04 意味の違う番号

- **単位は `Types.fs` に集約**（`Memtable` / `Wal` より前のコンパイル順で使うため）。
  `TxnId` の型略記もここに置く（`Transactions.fs` に置くと `Wal.fs` から見えない）。

  ```fsharp
  [<Measure>] type manifestNo
  [<Measure>] type txnId
  type internal TxnId = uint64<txnId>
  ```

- **同型で紛れやすい組の片側だけに単位を付ける**。
  片側だけでも対は区別できる（`int64<manifestNo>` と `int64` は互いに代入できない）。

  | 素の型   | 意味 A（素のまま）              | 意味 B（単位付き）              |
  | -------- | ------------------------------- | ------------------------------- |
  | `int64`  | `FileNumber` / `NextFileNumber` | `ManifestNumber` (`manifestNo`) |
  | `uint64` | `SeqNo` / `ActiveSnapshot`      | `TxnId` (`txnId`)               |

  `FileNumber`（int64）と `SeqNo`（uint64）は元々別型なので混ざらない。
  実際に危なかったのは**同じ素の型どうし**の 2組。

- **単位は直列化の境界でだけ外す**。
  WAL / ファイル名は素のバイト列なので、書き出しで `uint64 txnId`、
  読み込みで `LanguagePrimitives.Int64WithMeasure` / `UInt64WithMeasure` を使う
  （parse, don't validate）。
- `sprintf "%d"` / `max` / `compare` / `Map` / `Set` は単位付きのまま使える。
  使えないのは**単位が付いたリテラル**と**素との加減算**だけ
  （`1UL` → `1UL<txnId>`、`LanguagePrimitives.UInt64WithMeasure`）。
- `Manifest.logAndApply` の第 2 引数は `int64<manifestNo>`（例: `1L<manifestNo>`）。
  素の `1L` を渡すと file number 扱いで FS0001 になる。
- 単位は実行時に消えるのでコストは 0。
  `Manifest` / `StorageEngine` / `VersionSet` / `Transactions` / `Wal` / `ExecContext`
  と対応テストが対象。

### INV-05 TransactionManager

- 宣言:
  - `NextSeqNo > LastCommitSeqNo`
  - `Active` の TxnId はマップのキーと一致し、`NextTxnId` より小さい
  - `Active` の snapshot は `LastCommitSeqNo` 以下
  - `LastCommitted` の SeqNo は `0 < seq <= LastCommitSeqNo` かつ `seq < NextSeqNo`
- 担保: `TransactionsTests.fs` 内の private `validate`（src には公開していない）。
- 回帰テスト:
  - `TransactionManager: 初期状態は妥当`
  - `TransactionManager: NextSeqNo の逆転を検出`
  - `TransactionManager: Active の snapshot が未来なら検出`
  - `TransactionManager: LastCommitted の範囲外を検出`
  - `TransactionManager: begin/put/commit/abort を繰り返しても不変条件を保つ`
- `Active` は `Map<TxnId, Transaction>` で直接持つ。

### INV-06 Version

- 宣言:
  - `Files.Length = Levels.Count`
  - ファイル番号は一意で、`NextFileNumber` より小さい
  - `Level` は `0..6` で、格納位置と一致する
  - 各レベルのファイルは `Smallest` 昇順
  - L1 以上は範囲が重ならない（L0 は重なってよい）
  - `Smallest <= Largest`
- 担保: `VersionSetTests.fs` 内の private `validate`（src には公開していない）。
- 本番では検査しないが、`Compaction.boundsOf` が `Smallest > Largest` の入力を
  `CorruptionError` に変換する
  （部分関数の例外を protect 外へ脱出させない。§11.76）。
- 回帰テスト:
  - `Version.validate: 空の Version は妥当`
  - `Compaction.compact: Smallest > Largest の入力は CorruptionError になる`
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

### INV-10 keyspace 接頭辞

- 宣言: `Keyspace.prefixedKey ks key` を `Keyspace.prefix ks` で外すと元のキーに戻り、
  別 keyspace の物理キーは**自分の接頭辞だけ**が一致する。
- 理由: `RowStore.scanWithinPrefix` は `Array.sub` で接頭辞を外す。
  エスケープを持たないので、**長さフィールド（varint）だけが混入を防ぐ**。
- 回帰テスト: `Keyspace: prefixedKey は接頭辞を外すと元のキーに戻る`
  （空キー / NUL / 0xFF を含むキー × 6種の keyspace、varint が 2バイトになる長い名前、
  全ペアでの非相互接頭辞一致）。
- 通し経路は `RowStore: 前方一致する keyspace 名でもキーが混ざらない` が担保。

### INV-11 ヘッダと行幅

- `SelectProjection.create` が `*` / `t.*` の展開を 1回だけ行い、
  `Expressions`（行の値）と `DisplayHeader` / `ReferenceHeader`（ヘッダ）を同時に作る。
- `Ops.project` と `Plan.runSelect` は同じ `SelectProjection` を使う。
- 回帰テスト:
  - `SelectProjection: ヘッダと式の列数が一致する`
  - `SelectProjection: 項目が空なら入力スキーマをそのまま使う`

### INV-12 Cursor の範囲不変

- 宣言: `0 <= Pos <= End <= Data.Length`。
- 理由: 検証せずに作ると `Pos` / `End` が `Data.Length` を超え、
  `Reader.readByte` が `IndexOutOfRangeException` を投げる
  （壊れた SSTable を `CorruptionError` ではなく未処理例外で拒否してしまう）。
- 担保:
  - **`Cursor.fsi` がレコードを隠す**。構築は `create` / `createSlice`、前進は `advance` に限る。
  - **`Cursor.createSlice` が構築時に範囲を検証**し、範囲外は `CorruptionError` を返す
    （判定は `data.Length - length` なので巨大な `length` でもオーバーフローしない）。
  - `Cursor.create` は `Pos = 0` / `End = data.Length` で常に妥当。
  - 各読み取りは `Pos < End` を確認してから進むので、帰納的に保たれる。
  - `Cursor.advance` がレコード更新を 1箇所に集約する。
- 回帰テスト:
  - `Cursor.createSlice: 範囲外を CorruptionError`
    （負の offset / length、境界超過、オーバーフローを狙った指定）
  - `Cursor: create / createSlice / 読み取りは不変条件 0 <= Pos <= End <= Data.Length を保つ`

### INV-13 BloomFilter のビット数

- 宣言: `BitCount % 8 = 0` かつ `Bits.Length = BitCount / 8`。
- 理由: `positionAt` は `% BitCount` で 0 以下を作るが、`Bits.Length = BitCount / 8` なので
  `BitCount` が 8 の倍数でないと `Bits.[pos / 8]` が範囲外を指す
  （`firstAndStep` のコメントが根拠）。
- 担保: 築く側 `bitsFor` が 8 単位に丸める / 読む側 `deserialize` が `% 8 <> 0` を
  `CorruptionError` で拒否する。
- 回帰テスト:
  - `BloomFilter.ofKeys: ビット数は必ず 8 の倍数でビット配列と一致する`
    （`expectedCount = -3..10_000` × 誤検出率 4 種を網羅し、築く側・往復側・false negative の 3 点）
  - `BloomFilter.deserialize: ビット数が 8 の倍数でないデータは CorruptionError`

### INV-14 ApproxBytes の会計

- 宣言: `Memtable.ApproxBytes = Σ Entry.managedBytes`（挿入順に依存しない）。
- 担保: `Memtable.fsi` がレコードを隠し、構築を `Memtable.Empty` / `insert` / `apply` に限定する。
  `Memtable.insert` が `Entry.managedBytes` を足す。
  `Entry.takeOwnership` の複製でもサイズが変わらないことまで含めて検査する。
- 回帰テスト: `Memtable: ランダム挿入でも件数と ApproxBytes は合計と一致する`
  （重複キー・tombstone・BLOB・可変長 TEXT を 400回、順序反転での一致、
  挿入後の配列書き換えでも格納内容と会計が一致）。

### INV-15 RowReader の状態

- 宣言: `Reading` / `Exhausted` / `Closed` のいずれか 1 つ。
- 理由: `closed` / `exhausted` の bool 2 つでは
  「終端後」と「Dispose 後」の区別が**判定の順序に依存**し、
  両方が true になる状態も作れた。
- 担保:
  - `RowReaderState`（`Reading of Op` を含む private DU）が単一の状態。
  - `releaseTo next` が「`Reading` のときだけ `release ()` を呼んで遷移する」ので、
    `release` は 1 回しか呼ばれないことも型ではなく遷移で保たれる。
  - `Exhausted` と `Closed` はどちらも解放済みで、`TryRead` の結果だけが違う。
- 回帰テスト: `RowReader: 単一 SELECT を1行ずつ読める`（終端後の `TryRead` を含む） /
  `RowReader: Dispose すると Close が待たずに完了できる`（Dispose 後の `TryRead` を含む）。

### INV-16 Session のロック規約

- 宣言:
  - `Session.stateGate` は `local` / `readers`、`DatabaseHost.stateGate` は `lifecycle` を守る（短いロック）。
  - `Session.writeGate` は接続内、`DatabaseHost.writeGate` は DB 全体の
    書き込み・BEGIN / COMMIT / ROLLBACK・DDL・Close を直列化する。
  - SELECT のみのバッチは `writeGate` を取らず、`AcquireReadSnapshot` で並行する。
- 担保（`SessionTests`）:
  - `Session: 読み取りは実行中の書き込みを待たない`
  - `Session: 書き込みは実行中の読み取りを待たない`
  - `Session: Close は実行中の読み取りの完了を待つ`
  - `Session: 並行読み取りは commit 済みの一貫した snapshot を見る`
  - `Session: 同一 Session を複数スレッドで使っても書き込みが消えない`
  - **`Session: BEGIN は writeGate を取るので実行中の書き込みを待ち、SELECT は待たない`**
    （transaction 制御が `writeGate` に入る側を初めて直接検査）
- Session のロック規約は決定的テストのみ。
  **ランダム操作列のストレステストは未実施**
  （`VersionCoordinator` の並行性は INV-24 で実施済み。Session のロックは別の問題）。

### INV-17 lease

- `ProcessLockLease` は root とトークンだけを持ち、ロックの解放は `ForceRelease` だけ
  （`RowStore.close` とエラー経路が呼ぶ。二重解放は `ProcessLock.release` の台帳で無害）。
  DB を開閉する所有者は `DatabaseHost` 1つなので、接続ごとの所有権トークンは持たない。
- 進行中 transaction の登録は `TxnState.Lease` が `RegistrationLease` として持つ。
  接続が Close されずに捨てられても、最後のリースが GC された時点で登録を外す
  （`ActiveRegistration` の生存トークン）。
- 回帰テスト:
  - `DatabaseHost: commit せずに捨てた transaction も GC されれば登録が外れる`
  - `DatabaseHost: 生きている transaction は GC されても登録が残る`
  - `DatabaseHost: Dispose でも transaction の登録が外れる`
  - `Session: 別プロセスが開いている DB は開けない` /
    `Session: 同じ root を二度開くと同じプロセスのエラーになり、Close 後は開ける`

### INV-18 children / plugChildren の arity

- 宣言: `Expr.plugChildren (Expr.children e) e` が同じ式を復元し、
  子の個数が合わなければ `InternalError` で返る。
- 理由: `children`（列挙）と `plugChildren`（組み立て）が別々の match だと、
  ノードを追加したときに片方だけ更新して arity がずれる
  （`Case` の ELSE 取りこぼしは実際に発生したバグ）。
- 担保:
  - **子の構造を `Expr.mapChildrenState` の 1 つの match に統合**。
    `children` は「子を状態へ積む `step`」、`plugChildren` は「未使用の子を渡す `step`」
    として同じ match から導出するので、ノードを追加したときに直す場所は 1箇所。
    さらにこの match は網羅検査される（欠けたケースは FS0025 = エラー）。
  - `children` / `plugChildren` は `internal` で直接テスト可能。
  - `AstTests.allNodeKinds`（29 サンプル: 葉 / 1..3 子 / 可変子 / CASE の ELSE 有無 /
    LIKE の ESCAPE 有無 / サブクエリを含む型）で 3点を検査:
    - `Expr.plugChildren: children と同じ順・同じ内容で復元する（全ノード種）`
    - `Expr.plugChildren: 子を渡した順にそのまま入れる`
    - `Expr.plugChildren: arity が children と合わないと InternalError`
- **新しい `Expr` ケースを足したら `AstTests.allNodeKinds` にも足す**
  （ここに無いノードは arity のずれを検出できない）。

### INV-19 保持する側の所有権

- 宣言: **保持する側**（memtable の Treap / transaction のバッファ）は、
  渡されたキー・行を複製してから保持する。
  呼び出し側が後から配列を書き換えても格納内容と順序は変わらない。
- 理由: Treap の探索と `Map<byte array, _>` の順序は**配列の中身**に依存する。
  所有権を渡したのに複製しないと、後からの書き換えで木 / バッファが壊れる
  （F# に線形型が無いので、コメント契約ではなく複製で保つ）。
- 担保:
  - `Entry.takeOwnership`（キー + 行。Blob の `byte[]` も複製）— `Memtable.insert` が呼ぶ。
  - `Transaction.setSlot` — `put` / `remove` の入口で複製。
  - `Session.Execute(sql, parameters)` / `TryOpenReader(sql, parameters)` —
    パラメータの配列と Blob の `byte[]` を `SqlValue.copyRow` で複製してから AST に埋め込む
    （`RowReader` が読んでいる間の書き換えに影響されない）。
  - `SqlValue.copyRow` は INV-02（出口）と INV-19（入口）で共有。
- 回帰テスト（**修正前に実際に失敗することを確認済み**）:
  - `Memtable: 挿入したキー配列を後から書き換えても木は保たれる`
  - `Memtable: 挿入した行配列を後から書き換えても値は変わらない`
  - `Transaction: put したキー・行配列を後から書き換えてもバッファは保たれる`
  - `RowReader: パラメータの BLOB を後から書き換えても結果は変わらない`
- コスト: キー + 行の複製で 1行あたり約 0.4µs（20万行の実測）。
  50k行 INSERT の全体（5.7秒）に対し約 0.7%。正しさを優先して採用。
- **`SstEntry` は複製しない**（読み取り専用のビュー）。
  走査で得たエントリは保持せず、出力は `SqlValue.copyRow` を通る。
  所有するのはデコード元と memtable。

### INV-20 Writer のチャンク順

- 宣言: `Writer.Chunks` は**逆順**（先頭が最後に書いたチャンク）で保持する。
- 理由: `add` が先頭に積み、`toArray` が出力配列の**末尾から**埋めるので `List.rev` が不要になる。
  外でレコードを組むと（自然な順序で `Chunks = [ a; b ]` と書くと）
  **バイト列が反転したまま静かに書き出される**（例外にもならない）。
- 担保:
  - `Writer.fsi` が型を不透明にし、構築を `Writer.Empty` と各 write 関数だけに限定する
    （`Chunks` を直接組むレコード式は**コンパイルエラー**）。
  - 型の doc コメントに「`Chunks` は逆順」と明記。
- 回帰テスト:
  - `Writer: 多数のチャンクを積んでも書き込み順に並ぶ`
    （`writeByte` / `varint` / `uint32LE` / `writeBytes` を混ぜた 64 チャンクを
    期待バイト列と完全一致で比較。末尾からの埋め戻しが崩れると落ちる）

### INV-21 UNIQUE の論理予約

- 宣言: UNIQUE 索引の値は、transaction が commit するまで
  「その索引値に対応する論理予約キー」を保持し、競合検出の対象にする。
- 理由: 索引の物理キーは「索引値 + 主キー」なので、主キーが違えば同一 UNIQUE 値でも物理キーが衝突しない。
  snapshot だけの `checkUnique` では、同時に開始した 2つの transaction が同じ UNIQUE 値を書く競合を防げない。
- 担保:
  - `Transaction.reserveUnique` が `@unique:<索引名>:<base64(索引値)>` を
    `Reservations : Set<string>` に記録する（同じ値は 1件にまとめる）。
  - `Transaction.writeKeys` が物理書き込みキーと `Reservations` の和集合を返す。
  - `TransactionManager.commit` が `writeKeys` で first-committer-wins を判定し、
    commit した予約を commit SeqNo で `LastCommitted` に登録する。
  - 物理エントリの無い予約だけの commit でも SeqNo を進める。
  - `NULL` を含む UNIQUE 値は予約しない（既存の UNIQUE 検査と同じ）。
  - 予約は物理キーではないため、`CREATE UNIQUE INDEX` の backfill は不要。
    DDL は排他（`RootRuntime`）で active transaction を拒否する。
- 回帰テスト:
  - `Transaction.reserveUnique: 同じ索引値の予約は 1 件にまとまる`
  - `TransactionManager.commit: UNIQUE の論理予約が競合する`
  - `TransactionManager.commit: active が無ければ commit 直後に LastCommitted を捨てる`
    （予約キーも同じ GC 経路で消える）
  - `SessionPool: 接続間で UNIQUE の同時 INSERT を検出する`
  - `SessionPool: 接続間で UNIQUE の同時 UPDATE を検出する`
  - `SessionPool: UNIQUE の NULL は接続間でも重複できる`
  - `SessionPool: rollback した UNIQUE の予約は残らない`

### INV-22 Session 層とクエリ局所リソースに残る mutable

- 宣言: `Session.fs` / `DatabaseHost.fs` に残る `mutable` は、
  状態 DU を保持するスロットと読み取り数 `readers`（カウンタ）だけ。
  クエリ局所の例外は `ValueSet.IndexedValues` と `BloomFilter.BloomBuilder` だけ。
- 理由: bool・option・カウンタを並べると「同時に成立してはいけない組み合わせ」を作れてしまう
  （INV-15 と同じ理由）。
  Session 層は意図的に `mutable` を使う層なので、残る `mutable` を状態 DU に寄せる。
- 担保:
  - `CommitDurability.SyncState`（`Idle` / `Leading` / `Failed`）。
    「同期中かつ失敗」は表現できない。
  - `DatabaseHost.HostLifecycle`（`Running of Committed` / `Closed of Committed`）。
    Close 後の publish は `Closed` 側で捨てる（INV-26）。
  - `Session.LocalSession`（`Open of TxnState` / `Closed`）。
    閉じた後に `TxnState` を読む経路が無い。
  - `SessionPool.PoolLifecycle`（`Open` / `Closed`）。
  - `RowReader.RowReaderState`（INV-15）。
  - `TxnState` / `ExecState` / `Committed` は不変レコード（`mutable` を持たない）。
- クエリ局所の例外: `ValueSet.IndexedValues`（reader / 直近 1ブロックのキャッシュ / `disposed`）と
  `BloomFilter.BloomBuilder`（件数カウンタ）は、クエリ・構築の寿命に閉じ、DB を跨いで共有しない。
- 回帰テスト: `DatabaseHostTests` の `CommitDurability:` 4本、
  `SessionTests` の並行・Close 系（INV-16 と同じ一覧）、`SessionPool` の接続テスト。

### INV-23 結果行は `LoadedRow`

- 宣言: 公開 API へ返す行は `LoadedRow`（`SqlValue.Unloaded` を含まない）。
- 理由: `Unloaded` は列マスク復号の内部プレースホルダで、SQL の NULL ではない。
  比較・型の問い合わせ・符号化は `InternalError` にするが、
  「結果に漏れない」を規約とテストだけに頼ると、新しい出力経路を足したときに検査を忘れる。
  F# は union case 単位のアクセシビリティを持たない（FS0572）ため、
  `SqlValue.Unloaded` 自体は公開のままにし、**出力境界の型**で閉じる。
- 担保:
  - `LoadedRow` はケース非公開の struct DU。
    構築は `LoadedRow.create` だけ（内部で `SqlValue.ensureLoaded` を使う）。
  - 非公開コンストラクタの `StatementResult`。
    行付きの構築は `StatementResult.Create`（`LoadedRow array` のみ受理）だけ。
    生の行を受ける `OfRows` も内部で `LoadedRow.create` を通す。
  - `RowReader.TryRead` の戻り値は `LoadedRow option`。
- 回帰テスト: `LoadedRow.create: Unloaded を含む行は InternalError`、
  `StatementResult.OfRows: Unloaded を含む行は拒否する`、
  `SqlValue.dataType: Unloaded は InternalError`。

### INV-24 VersionCoordinator の Version 公開とファイル refcount

- 宣言:
  - `publish` による `Generation` と `Version.NextFileNumber` は単調（戻らない）。
  - `install` した Version が参照するファイルは存在し、pin されている間は削除されない。
  - retired のファイルは pin が 0 になってから削除される。
- 理由: compaction は不変の `StorageEngine` の外側で Version を差し替える。
  pin を忘れると読み取り中のファイルが消え、採番を戻すと古い Version が公開される。
- 担保:
  - 状態は `ConcurrentDictionary` の単一セル + `TryUpdate` の CAS。
  - ファイル番号は `Version.NextFileNumber` を同じセルの CAS で予約する。
    `publish` は現在値との `max` を取るので、古い Version を公開しても番号は戻らない。
    MANIFEST 追記と install は `installGate` で直列化する。
  - `AcquireCall` が現在の Version の全ファイルを pin し、`Dispose` で release + sweep する。
- 回帰テスト（`VersionCoordinatorTests`）:
  - `background compaction: 書き込み・読み取り・snapshot pin を並行に回しても壊れない`
    （複数スレッドの同時実行）
  - `background compaction: ランダムな操作列でも Version と refcount の不変条件を保つ`
    （固定シードの操作列: Generation / NextFileNumber の単調性、Version の妥当性、
    pin 中の全ファイル存在、`LastCompactionError = None` を検査）
- 残る既知の未対応: プロセス断を挿入するランダム操作列は未実施
  （クラッシュ再開自体は他の `VersionCoordinatorTests` が個別に検証する）。

### INV-25 commit の WAL 位置は fsync 前に登録する

- 宣言: `DatabaseHost.serialized` は、実行後の `WalState` を `writeGate` の内側で
  `CommitDurability.Register` し、`writeGate` の外側で `Await` する。
- 理由: `Await` は「登録済みの最大位置」までしか同期しない。
  `Register` を忘れると、待機中の commit の WAL 位置が leader の fsync target に入らず、
  commit ごとに fsync が走る（グループコミットが実質無効になる）。
  `CommitDurability` 単体のテストは `Register` を直接呼ぶため、
  この統合欠落を検出できない。
- 担保:
  - `DatabaseHost.serialized` が `writeGate` の内側で `position |> Option.iter durability.Register` を呼ぶ。
  - `DatabaseHost` はテスト用に `CommitDurability` を注入できる（`?commitDurability`）。
- 回帰テスト: `DatabaseHost: 同時 commit の WAL 位置を writeGate の内側で登録し fsync をまとめる`
  （3 commit が 2回の fsync にまとまり、最後の fsync が 3番目の位置まで届く）。

### INV-26 確定状態は transaction を持たない

- 宣言: 全接続で共有する確定状態 `Committed`（`Root` / `Store` / `Catalog` / `Manager` / `DeferDurability`）は
  transaction のフィールドを持たない。接続ローカルの transaction は `TxnState`
  （`CatalogAtBegin` / `Active` / `Pending` / `Lease`）だけが持つ。
- 理由: 確定状態と transaction を 1つのレコードに同居させると、
  committed を publish するたびに transaction 系フィールドを空に書き足す必要があり、
  埋め忘れると「確定状態に transaction が載る」状態を作れた。
- 担保:
  - `Committed` に transaction 系のフィールドが無い（型として作れない）。
  - `HostLifecycle` は `Running of Committed` / `Closed of Committed` だけを持つ。
  - 文の実行時だけ `ExecState = { Committed; Txn; Registration }` に合成する。
    `ExecState.Catalog` は transaction 中なら `Txn.CatalogAtBegin`、外なら `Committed.Catalog`。
  - DDL / DML / SELECT は `ExecState` を受けるが、確定状態の更新は `Committed` の組み直しだけ。
- 回帰テスト: `SessionPool` / `DatabaseHostTests` / `DatabaseTests` の transaction・再オープン・
  DDL 排他のテストが経路を覆う（型変更なので専用テストは置かない）。

### INV-27 並列制御以外に DB・プロセス寿命の可変キャッシュを持たない

- 宣言: 可変状態は並列制御（ロック・CAS セル・登録台帳・ワーカー状態）だけに置く。
  式評価と SSTable 読み取りの前計算は、**計画時または `Version` の構築時に作る不変値**とし、
  評価・読み取り中に変化しない。
- 理由: キャッシュはクエリ間で状態を共有するため、キーの取り違え（曲がったファイル）や
  並行実行時の汚染を起こしやすい。
  実際に `Ops.distinct` のモジュールスコープ `HashSet` で 2回目以降の実行が 0行になるバグを踏んだ
  （[`gotchas.md`](gotchas.md) §2.6）。
- 担保:
  - `Eval.Prepared` は `Eval.prepare` が計画時に作る不変の `ImmutableDictionary` を持つ。
    参照列とリテラル LIKE を 1回で解決・コンパイルし、行ごとに再解決しない。
  - `FileMetaData.ReadInfo`（`SstReadInfo`）は書き出し時または open 時の hydrate で 1回だけ作り、
    不変の `Version` に載せる。DB・プロセス寿命のキャッシュは持たない。
  - 残る可変コレクションは `coding-conventions.md` §2 の許可リスト
    （Session 層の状態 DU・`VersionCoordinator` の CAS セル・`ProcessLock` / `Transactions` の台帳・
    演算子の作業用コレクション・`ValueSet.IndexedValues` のクエリ局所キャッシュ）に限る。
- 回帰テスト: `Eval.matchesWithV: リテラル述語は 1000 回でも結果を確保しない`、
  `Scalar.likeMatch: 計画時にコンパイルしたパターンは行ごとに配列を確保しない`、
  `StorageEngine.openDatabase: 書き込み後に壊れたファイルは再オープンで検出する`、
  `SqlReference: DISTINCT を続けて実行しても毎回全行が返る` /
  `SqlReference: GROUP BY とハッシュ結合を続けて実行しても毎回同じ結果になる`。

### INV-28 索引エントリの値は空で、索引列 + 主キーはキーから復号する

- 宣言: `i:<index>` keyspace の値は空（`Array.empty`）。
  論理キーは「索引列（`IndexSchema.Columns` 順）+ 主キー列（`TableSchema.PrimaryKey` 順）」。
  索引スキャンはキーから索引列 + 主キーを `OrderPreserving.readValuesMasked` で復号し、
  被覆外の列は `ExecContext.fetchMissing` が同じ `Visible` / `Txn` で主キー lookup して埋める。
  結果に `Unloaded` を漏らさない。
- 理由: 値に行全体を複製すると、索引 1本ごとにストレージと書き込みが行幅ぶん増え、
  `ALTER TABLE` が全索引の書き換えを必要とする。
  索引キーだけで行を一意に特定できる。
- 担保:
  - 書き込みは `Dml.applyIndexChange` と `Engine.backfillIndex` の 2箇所だけで、どちらも空値を書く。
  - 読み取りは `RowKey.indexKey` / `opOfIndexScan`。transaction のスロットも**生キーで**マージする。
  - `Dml.indexAccessMasks` が (スキャン mask, fetch mask) を返し、
    `Optimizer.execScan` / `execIndexJoin` が非被覆のときだけ fetch を挟む。
  - `CREATE UNIQUE INDEX` は `Engine.createIndex` の `checkUniqueIndex` が
    `RowStore.scanKeys` でキーだけを走査し、索引列を復号して重複を検査する。
  - `ALTER TABLE` の行書き換えは `Engine.rewriteTable` でテーブル本体だけを対象にする。
- 回帰テスト:
  - `Dml.insertWithKey: 索引エントリの値は空になる`
  - `Dml.scanByIndexPrefix: 索引キーから索引列と主キーを復元する`
  - `Dml.scanByIndexPrefix: マスクした列は Unloaded になる`
  - `Dml.scanByIndexPrefix: transaction の索引書き込みもキーで overlay する`
  - `Session: 非被覆索引は本体から列を fetch する`
  - `Session: 索引 fetch は transaction の書き込みを見る`
  - `Session: UNIQUE 索引の backfill は複数の NULL を許す`
  - `SqlReference: ALTER TABLE DROP COLUMN は行と索引から列を除く`
  - `キー復号: 全型が往復する` / `キー復号: 壊れたキーは CorruptionError になる`

---

### INV-29 退避した一時ファイルは必ず閉じ、読み切ったら消す

- 宣言: `Spill` が作る run ファイルは、
  **書き込み後は必ず閉じ**（`closeSink`）、
  **読み切ったら閉じて消す**（`foldRun` / `reopen`）。
  同じ行を何度も読む用途では「閉じるだけで消さない」(`foldRunKeep` / `reopenKeep`) を使い、
  使い終わったら `deleteSpool` で消す。
- 理由: 途中打ち切り（`LIMIT`・`RowReader.Dispose`）で開いたままにすると
  file descriptor が漏れ、消し忘れると一時ファイルが溜まる。
- 担保:
  - 書き込みは `Spill.openSink` → `writeRow`/`writeChunk` → `closeSink` の 1経路。
  - 読み取りは 4種類だけ: `foldRun`（閉じて消す）/ `foldRunKeep`（閉じるだけ）/
    `reopen`（消す）/ `reopenKeep`（消さない）。
  - 分割のやり直しは `repartitionRun` が元の run を消す。
  - 開いた run と退避した spool は `MemoryBudget.Scope`（`ExecContext.Scope`）に登録し、
    `LIMIT` や `RowReader.Dispose` の早期終了で `DisposeAll()` が閉じて消す。
    通常終了では `ScanScope.Forget` で登録を外してから明示的に消す。
  - 途中打ち切りで残ったものは、次の外部ソート時に `Spill.sweepStale` が
    24時間を超えた分だけ回収する。
- 回帰テスト:
  - `Spill.spool: 上限を超えると退避し、reopen で同じ順序で読める`
  - `Spill.spool: reopenKeep は何度でも読める`
  - `Spill.foldRun: 終端でファイルを消す`
  - `Spill.openRun: 破損した run は CorruptionError`
  - `Spill.spool: 存在しないディレクトリは IoError`
  - `Spill.reopen: ScanScope の早期終了でファイルを消す`
  - `ValueSet: 退避した集合は ScanScope の早期終了でもファイルが残らない`
  - `RowReader: LIMIT で早期終了しても一時ファイルが残らない`
  - `distinct: 上限を超えて退避しても結果と順序が変わらない`
  - `集約: 上限を超えて退避しても結果と順序が変わらない`
  - `結合: 入れ子ループは右を退避しても結果が変わらない`
  - `結合: ハッシュ結合は右が上限を超えるとマージ結合へ切り替わり結果が同じ`
  - `結合: マージ結合は同一キーのグループを退避しても結果が同じ`
  - `結合: マージ結合は左が先に尽きても右を読み切る（退避ファイルの後始末）`

---

## 既知の未対応（次の候補）

- `FileMetaData.FileNumber` と `Version.NextFileNumber` は素の `int64` のまま
  （同じ意味なので統一不要。単位は意味が違う数値にだけ付ける）。
- `Entry` / `SstEntry` の record リテラルは直接構築できるが、**ソース検査は追加しない**（検討済み）。
  `Entry.put` / `delete` と `SstEntry.make` は素のコンストラクタで**構築時の不変条件を持たない**。
  レコード更新も `RowStore` の接頭辞の付け外し（`{ e with Key = ... }`）だけ。
  所有権は INV-19（`Entry.takeOwnership` / `Transaction.setSlot`）が担保するので、
  チェックを足しても**守るものが無く空振りになる**。
  （接頭辞の付与・剥離は `Keyspace` と同じファイルに置くのが凝集的で、他のファイルへ移すとむしろ悪化する。）
- INV-19 は 2回所持するため、commit 経路（`Transaction.put` → `Memtable.insert`）は行を 2回複製する。
  単純な重複排除には「複製済みを渡す」経路（線形型）が必要で、現状はコストを許容している。
