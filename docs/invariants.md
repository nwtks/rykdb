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

| ID     | 不変条件                                                | 担保                                                                                                                      | 状態     |
| ------ | ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- | -------- |
| INV-01 | `Op` / `SstScan` は one-shot                            | `Op.emit` / `Op.compute`、`TestHelpers.oneShot`、`SstScan.ofStep` / `ofArray` / `ofState`、`SstScan.withScan`、ソース検査 | ✅ 完了  |
| INV-02 | 返した行は DB 内部と独立                                | `SqlValue.copyRow`、`Engine.runSelect`、`RowReader.TryRead`                                                               | ✅ 完了  |
| INV-03 | キー順の定義は 1つ                                      | `KeyOrder.compare`、`Entry.compareKeys`、`SstEntry.compare`                                                               | ✅ 完了  |
| INV-04 | 意味の違う番号（manifest / file / seq / txn）を混ぜない | 単位 `manifestNo` / `txnId`（同型ペアの片側だけに付与）                                                                   | ✅ 完了  |
| INV-05 | `TransactionManager` の SeqNo / Active / LastCommitted  | `TransactionManager.validate`                                                                                             | ✅ 完了  |
| INV-06 | `Version` のレベル・番号・範囲                          | `Version.validate`                                                                                                        | ✅ 完了  |
| INV-07 | compaction GC は可視集合を保存                          | `Compaction.gcGroup` と可視性プロパティテスト                                                                             | ✅ 完了  |
| INV-08 | MANIFEST は追記専用・番号は単調                         | `VersionEdit` encode/decode + 再オープンのプロパティテスト                                                                | ✅ 完了  |
| INV-09 | WAL は commit 済み prefix だけ再生                      | `Wal.foldCommitted` の全切断位置テスト                                                                                    | ✅ 完了  |
| INV-10 | keyspace 接頭辞が混ざらない                             | `Keyspace.prefixedKey` の往復・非相互接頭辞テスト、`RowStore` のキー分離テスト                                            | ✅ 完了  |
| INV-11 | ヘッダ長 = 行幅 = 射影長                                | `SelectProjection.create`（式・表示ヘッダ・参照ヘッダを一度に構築）                                                       | ✅ 完了  |
| INV-12 | `Cursor` は `0 <= Pos <= End <= Data.Length`            | `Cursor.createSlice` の検証                                                                                               | 拡張予定 |
| INV-13 | `BloomFilter.bitCount` は 8 の倍数                      | 築く側 `bitsFor` が 8 単位に丸める + 読む側 `deserialize` の拒否 + パラメータ網羅テスト                                   | ✅ 完了  |
| INV-14 | `Memtable.ApproxBytes` は `Σ Entry.managedBytes`        | `Memtable.insert` の会計とランダム挿入・順序独立性のプロパティテスト                                                      | ✅ 完了  |
| INV-15 | `RowReader` は Reading / Exhausted / Closed のいずれか  | `RowReaderState`（状態 DU に置き換え、`releaseTo` が遷移と 1回解放を担当）                                                | ✅ 完了  |
| INV-16 | `Session` のロック規約                                  | `stateGate` / `writeGate` の設計 + `SessionTests` の並行テスト 6 本                                                       | ✅ 完了  |
| INV-17 | lease の所有権                                          | `ProcessLockHandle` / `ProcessLockLease` / `RegistrationLease`                                                            | ✅ 完了  |
| INV-18 | `Expr.children` / `plugChildren` の arity               | `Expr.plugChildren`（モジュールレベルへ引き上げ）+ `AstTests` の全ノード種テスト                                          | ✅ 完了  |
| INV-19 | 保持する側はキー・行の所有権を複製で取り込む            | `Entry.takeOwnership` / `Transaction.setSlot` の複製と回帰テスト                                                          | ✅ 完了  |

---

## 不変条件の詳細

ID 順。宣言・根拠・担保・回帰テストを 1件ずつ示す。

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
- 実際に効いた箇所（ともにビルド時に FS0001）:
  - `Manifest.logAndApply dir 1L` ← `1L` は file number 扱いで拒否された
  - `StorageEngine.NextTxnId = 1UL` ← `NextSeqNo`（素の uint64）との取り違えを拒否
- 単位は実行時に消えるのでコストは 0。
  `Manifest` / `StorageEngine` / `VersionSet` / `Transactions` / `Wal` / `ExecContext`
  と対応テストが対象。

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
- 担保: `Memtable.insert` が `Entry.managedBytes` を足す。
  `Entry.takeOwnership` の複製でもサイズが変わらないことまで含めて検査する。
- 回帰テスト: `Memtable: ランダム挿入でも Count と ApproxBytes は合計と一致する`
  （重複キー・tombstone・BLOB・可変長 TEXT を 400回、順序反転での一致、
  挿入後の配列書き換えでも格納内容と会計が一致）。

### INV-15 RowReader の状態

- 宣言: `Reading` / `Exhausted` / `Closed` のいずれか 1 つ。
- 理由: 以前は `closed` / `exhausted` の bool 2 つで、
  「終端後」と「Dispose 後」の区別が**判定の順序に依存**し、
  両方が true になる状態も作れた。
- 担保:
  - `RowReaderState`（`Reading of Op` を含む private DU）が単一の状態。
  - `releaseTo next` が「`Reading` のときだけ `release ()` を呼んで遷移する」ので、
    `release` は 1 回しか呼ばれないことも型ではなく遷移で保たれる。
  - `Exhausted` と `Closed` はどちらも解放済みで、`TryRead` の結果だけが違う。
- 回帰テスト: 既存の `RowReader: 終端後の TryRead` / `Dispose 後の TryRead` /
  `Dispose すると Close が待たずに完了できる`。

### INV-16 Session のロック規約

- 宣言:
  - `stateGate` は committed / closed / readers のみを守る（短いロック）。
  - `writeGate` は書き込み・BEGIN / COMMIT / ROLLBACK・DDL・Close を直列化する。
  - SELECT のみのバッチは `writeGate` を取らず、`AcquireReadSnapshot` で並行する。
- 担保（`SessionTests`）:
  - `Session: 読み取りは実行中の書き込みを待たない`
  - `Session: 書き込みは実行中の読み取りを待たない`
  - `Session: Close は実行中の読み取りの完了を待つ`
  - `Session: 並行読み取りは commit 済みの一貫した snapshot を見る`
  - `Session: 同一 Session を複数スレッドで使っても書き込みが消えない`
  - **`Session: BEGIN は writeGate を取るので実行中の書き込みを待ち、SELECT は待たない`**
    （transaction 制御が `writeGate` に入る側を初めて直接検査）
- ランダムに操作を組み合わせるストレステストは未実施（既知の穴）。

### INV-17 lease

- `ProcessLockHandle` が所有権ハンドルを表し、`IsOwner` はこの型だけを受け取る。
- `RowStore` のエラー経路も `ProcessLockLease.ForceRelease` を使う。
  `ProcessLock.release` の呼び出しは lease 内だけ。
- 回帰テスト: 既存のプロセスロック / 古いハンドルの Close テスト。

### INV-18 children / plugChildren の arity

- 宣言: `Expr.plugChildren (Expr.children e) e` が同じ式を復元し、
  子の個数が合わなければ `InternalError` で返る。
- 理由: `children`（列挙）と `plugChildren`（組み立て）は別々の match なので、
  ノードを追加したときに片方だけ更新すると arity がずれる
  （`Case` の ELSE 取りこぼしは実際に発生したバグ）。
- 担保:
  - `children` を `internal` に、`plugChildren` を `rewriteWithC` の中から
    `Expr` モジュールレベルへ引き上げて**隣接させ、直接テスト可能**にした
    （以前は private / ローカル `let` で外部から検査できなかった）。
  - `AstTests.allNodeKinds`（29 サンプル: 葉 / 1..3 子 / 可変子 / CASE の ELSE 有無 /
    LIKE の ESCAPE 有無 / サブクエリを含む型）で 3点を検査:
    - `Ast: plugChildren は children と同じ順・同じ内容で復元する（全ノード種）`
    - `Ast: plugChildren は子を渡した順にそのまま入れる`
    - `Ast: plugChildren は arity が children と合わないと InternalError`
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
  - `SqlValue.copyRow` は INV-02（出口）と INV-19（入口）で共有。
- 回帰テスト（**修正前に実際に失敗することを確認済み**）:
  - `Memtable: 挿入したキー配列を後から書き換えても木は保たれる`
  - `Memtable: 挿入した行配列を後から書き換えても値は変わらない`
  - `Transaction: put したキー・行配列を後から書き換えてもバッファは保たれる`
- コスト: キー + 行の複製で 1行あたり約 0.4µs（20万行の実測）。
  50k行 INSERT の全体（5.7秒）に対し約 0.7%。正しさを優先して採用。
- **`SstEntry` は複製しない**（読み取り専用のビュー）。
  走査で得たエントリは保持せず、出力は `SqlValue.copyRow` を通る。
  所有するのはデコード元と memtable。

---

## 既知の未対応（次の候補）

- `FileMetaData.FileNumber` と `Version.NextFileNumber` は素の `int64` のまま
  （同じ意味なので統一不要。単位は意味が違う数値にだけ付ける）。
- `Cursor` / `Writer` / `Entry` / `SstEntry` の record リテラルは直接構築できる（ソース検査なし）。
- `EngineHost` の並行操作（CAS / refcount / retire）をランダムに組み合わせるストレステストは未実施
  （`Session` 側は INV-16 の 6本で担保済み）。
- INV-19 は 2回所持するため、commit 経路（`Transaction.put` → `Memtable.insert`）は行を 2回複製する。
  単純な重複排除には「複製済みを渡す」経路（線形型）が必要で、現状はコストを許容している。
