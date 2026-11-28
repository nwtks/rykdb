# 不変条件レジストリ

この文書では、RykDb の型だけでは表現できない不変条件を管理する。
スマートコンストラクタ、検証関数、型による構築制限、プロパティテスト、実行時検証を組み合わせて、
各条件が壊れた状態を作れない、または検出できる設計にする。

設計全体は [`architecture.md`](architecture.md)、
実際に発生した落とし穴は [`gotchas.md`](gotchas.md) を参照する。

## 運用

不変条件を追加または変更した場合は、次の手順で更新する。

1. 一覧へ項目を追加または更新する。
2. 原因となる不具合、守るべき状態、実装上の担保を記述する。
3. 関数名、テスト名、ファイル名など検証可能な担保を明記する。
4. 対応する回帰テストを更新する。

本番経路で検出できる違反は `Result` と `DbError` で返す。
テスト専用の検査には、テストから呼べる `internal` な `validate` を使う。

## 担保の種類

| 種類           | 意味                                                              | 例                                                 |
| -------------- | ----------------------------------------------------------------- | -------------------------------------------------- |
| 型             | discriminated union、record、newtype、units of measure で表現する | `SqlValue`、`DataType`、`TruthValue`               |
| コンストラクタ | ケースやレコードを隠し、許可された関数だけで構築する              | `Op.fsi`、`Writer.fsi`                             |
| `validate`     | テストから呼ぶ検証関数                                            | `TestHelpers.validateVersion`、`TransactionsTests` |
| プロパティ     | ランダム入力で性質を検査する                                      | 順序、ランダム操作列                               |
| 構築制限       | `.fsi` でレコードや union case を隠す                             | `Op`、`SstScan`、`Cursor`、`Memtable`              |
| 実行時検証     | 本番経路で不正な状態を検出する                                    | チェックサム、キー順、範囲                         |

---

## データ構造と所有権

### INV-01: スキャンと演算子は one-shot

`Op` と `SstScan` は、同じ値を 2回消費してはならない。
ステップ走査、フィルタ、配列走査、ファイル走査をこの制約で統一する。

- `Op.emit`、`Op.compute`、`Op.ofArray` が構築経路となる。
- `Op.fsi` で union case を隠し、外部ファイルから `Done`、`Step`、`Compute` を直接構築できないようにする。
- `SstScan.ofStep`、`ofStepWithClose`、`ofArray`、`ofState`、`ofFilter`、`ofScoped` が構築経路となる。
- `SstScan.fsi` で union case を隠す。
- `SstScan.withScan` は走査の終了時と失敗時に必ず `Dispose` を呼ぶ。
- `ofStepWithClose` はファイルハンドルを保持する walk に使う。
- `StorageEngine`のlimit終端と、`Dml`の索引キー復号エラーは、
  `ScanScope`が無くてもその場で下流の走査を閉じる。

回帰テストでは、成功、失敗、フィルタ、配列化、
compaction 中断の各経路で一度だけ消費されることとハンドルが残らないことを検査する。

### INV-02: 出力行は内部データと独立

`StatementResult.Rows` と `RowReader.TryRead` が返す配列や Blob は、
呼び出し側が変更しても memtable、SSTable、transaction に影響してはならない。
内部配列をそのまま返すと、外部変更がデータ破壊に直結する。

- `SqlValue.copyRow` が行配列と Blob の `byte[]` を複製する。
- `Engine.runSelect` が materialize 前に複製を適用する。
- `RowReader.TryRead` がストリーミング行にも複製を適用する。
- 内部 API は所有権を受け取る契約を持つため、呼び出し後にキーや行を変更しない。

回帰テストは、返した行を書き換えても DB の内容が変わらないことを検査する。

### INV-03: エントリ順序は単一の定義を使う

エントリの論理順序は「キー昇順、同一キーでは SeqNo 降順」とする。
SSTable のマージ、範囲走査、GC、memtable の Treap が同じ順序を前提にする。

- `KeyOrder.compare` が唯一の比較処理である。
- memtable の `Entry.compareKeys` と SSTable の `SstEntry.compare` は `KeyOrder.compare` を呼ぶ。
- `Transaction.slotsOf` は `Bytes.compare` の昇順で返す。
  Map の構造比較（byte[] の長さ優先）をそのまま露出すると、
  索引スキャンのマージが store 側と食い違い `LIMIT` の結果が誤る。

プロパティテストで反対称性、推移性、memtable と SSTable の並び一致を検査する。
回帰: `Transaction.slotsOf: キーは Bytes.compare の昇順で返る` /
`SqlReference: txn 内の索引スキャンはキー順にマージされ LIMIT でも正しい行を返す`。

### INV-04: 異なる意味の番号は型で区別する

型や数値の値域が同じでも意味が違う番号を混ぜない。
特に `ManifestNumber` と `FileNumber`、`TxnId` と `SeqNo` を区別する。

`Types.fs` に次の units of measure を集約する。

```fsharp
[<Measure>] type manifestNo
[<Measure>] type txnId
type internal TxnId = uint64<txnId>
```

同じ素の型で混同しやすい組だけ片側に単位を付ける。
`int64<manifestNo>` と `int64`、`uint64<txnId>` と `uint64` は代入できない。
単位は直列化の境界で外す。

| 素の型   | 意味なし                       | 単位付き         |
| -------- | ------------------------------ | ---------------- |
| `int64`  | `FileNumber`、`NextFileNumber` | `ManifestNumber` |
| `uint64` | `SeqNo`、`ActiveSnapshot`      | `TxnId`          |

`Manifest.logAndApply` の manifest 番号引数は `int64<manifestNo>` を要求する。
単位付きリテラルが必要な箇所では `1L<manifestNo>` を使う。
単位は実行時に消えるため実行コストは発生しない。

### INV-05: TransactionManager の状態関係

transaction manager は次の関係を常に満たす。

- `NextSeqNo > LastCommitSeqNo`
- `Active` の `TxnId` は map のキーと一致し、`NextTxnId` より小さい
- `Active` の snapshot は `LastCommitSeqNo` 以下
- `LastCommitted.SeqNo` は `0 < seq <= LastCommitSeqNo` かつ `seq < NextSeqNo`
- 書き込みの採番は engine の高水位（`RowStore.lastSeqNo`）より後ろから行う
  （カタログ保存や失敗した DDL の後始末は Manager を経由せず engine へ書くため）

検証は `TransactionsTests.fs` 内の private `validate` が担当する。
本番では公開しない。

回帰テストでは初期状態、SeqNo の逆転、未来の snapshot、範囲外の `LastCommitted`、
begin/put/commit/abort の繰り返しを検査する。

### INV-06: Version のレベルとキー範囲

`Version` は次の関係を満たす。

- `Files.Length = Levels.Count`
- ファイル番号は一意で、`NextFileNumber` より小さい
- `Level` は `0..6` で格納位置と一致する
- 各レベルのファイルは `Smallest` 昇順
- L1 以上はキー範囲が重ならない。L0 は重複を許す
- `Smallest <= Largest`

検証は `TestHelpers.validateVersion` が担当する。
本番経路では、範囲が不正な compaction 入力を `Compaction.boundsOf` が `CorruptionError` に変換する。

回帰テストでは空の Version、ファイル番号の重複、採番の巻き戻し、
レベル間の範囲重複、レベルと格納位置の不一致、L0 の追加と削除を検査する。

### INV-07: compaction GC は可視行を保存

`Compaction.gcGroup` が compaction 後に残す行を判定する唯一の実装とする。
可視行、件数、SeqNo の基準をランダムな状態から検査する。

プロパティテストではランダムな版、snapshot、tombstone を生成し、
件数、SeqNo の一意性、`activeSnapshot` 以降の全 snapshot で可視行が変わらないことを検査する。

### INV-08: MANIFEST は追記専用で番号が単調

MANIFEST の編集は追記専用とする。
末尾の不完全なレコードは open 時に修復し、再オープン後も同じファイル集合を復元できるようにする。

- `VersionEdit.encode` と `decode` は round-trip できる。
- `Manifest.logAndApplyWithOutcome` は `Applied` / `NotApplied` / `Unknown` を返す。
  `NotApplied` は追記前の内容が保たれており、`Unknown` は追記の有無を保証できない。
- `VersionCoordinator.install` は `Unknown` のとき MANIFEST を読み直し、
  期待するファイル集合と一致すれば反映済みとして扱い、そうでなければ出力を削除する。
  読み直しに失敗した場合だけ孤児として残す（次回 open の `quarantineOrphans`）。
- ローテーションは追記前に判定し、追記失敗時は追記前の長さへ戻す。
- 末尾が書きかけの場合、復元した Version を新しい MANIFEST に移して `CURRENT` を切り替える。
- `VersionEdit.apply` は `NextFileNumber` を進めない。
  採番は再オープン時に `StorageEngine` がディスク上の最大ファイル番号から再構築する。

回帰テストはランダムな編集の encode/decode、追記と再オープン、末尾修復、ローテーション失敗時の保持、
`Applied` / `NotApplied` の区別を検査する。

### INV-09: WAL は commit済み prefix を再生

任意のバイト位置で WAL を切っても、commit済み transaction の prefix だけを再生する。

- `Wal.foldCommitted` は最後の完全なフレームの終端位置を返す。
- `StorageEngine.openDatabaseWith` はその位置まで WAL を切り詰める。
- 追記は `WalState.Size` から行い、不完全な末尾を残したまま次の commit を書かない。
- flush 後の rotate は新 WAL を作成してから旧 WAL を削除する。
- `WalState.Size` は `int64`（2GiB を超える WAL でも位置が溢れない）。
- flush の失敗は `StorageRuntime.LastFlushError` に残す。

回帰テストは任意の切断位置、torn tail 後の追記、切り詰め後の commit 継続
（`Wal.append: 書きかけの末尾を切り捨ててから追記する`）を検査する。

### INV-10: keyspace 接頭辞は分離される

物理キーへ付加した keyspace 接頭辞は、元のキーへ確実に戻せる。
接頭辞の似た別 keyspace にもキーを混在させない。
エスケープは使わず、接頭辞に varint の長さを含める。

- `Keyspace.prefixedKey ks key` と `Keyspace.prefix ks` は往復できる。
- `RowStore.scanWithinPrefix` は `Array.sub` で接頭辞を外す。
- 異なる keyspace の物理キーは互いの接頭辞が一致しない。

回帰テストは空キー、NUL、0xFF、2バイトになる長い接頭辞、6種の keyspace の全ペアを含む。

### INV-11: ヘッダ、式、行幅は一致

`SELECT *` と `t.*` を展開した列数、式の列数、表示ヘッダ、参照ヘッダを同じ場所で構築する。

`SelectProjection.create` は展開結果とヘッダを一度だけ作る。
`Plan.runSelect` の射影は `Ops.project` を通り、同じ `SelectProjection` を使う。

回帰テストはヘッダと式の列数、および項目が空の場合に入力スキーマを使うことを検査する。

### INV-12: Cursor の位置範囲は常に有効

`Cursor` は次の範囲を常に満たす。

```text
0 <= Pos <= End <= Data.Length
```

範囲外の状態を作ると `Reader.readByte` が例外を投げ、
壊れた SSTable を `CorruptionError` ではなく処理漏れの例外として拒否してしまう。

- `Cursor.fsi` はレコードを隠す。
- `Cursor.create` は `Pos = 0`、`End = data.Length`で生成する。
- `Cursor.createSlice` は構築時に範囲を検証し、不正なら `CorruptionError` を返す。
- `Cursor.advance` が位置更新を一箇所に集約する。
- 読み取りは `Pos < End` を確認してから進む。
- 範囲計算は `data.Length - length` を使い、巨大な `length` でもオーバーフローしない。

回帰テストは負の offset/length、境界超過、オーバーフロー相当の入力を検査する。

### INV-13: BloomFilter のビット数は 8 の倍数

Bloom filter は次の関係を満たす。

```text
BitCount % 8 = 0
Bits.Length = BitCount / 8
```

`BitCount` が 8 の倍数でないと、`positionAt` の添字が `Bits` の範囲外を指す可能性がある。

- 作成側の `bitsFor` は 8 単位へ丸める。
- 読み込み側の `deserialize` は `% 8 <> 0` を `CorruptionError` にする。

回帰テストは件数の負値、複数の誤検出率、往復変換、false negative、不正な直列化データを検査する。

### INV-14: memtable の ApproxBytes は正確

`Memtable.ApproxBytes` は保持中の全エントリの `Entry.managedBytes` の合計と一致する。
挿入順や重複キーに依存してはならない。

- `Memtable.fsi` はレコードを隠し、`Empty`、`insert`、`apply`だけで構築する。
- `Memtable.insert` が挿入時にサイズを加算する。
- `Entry.takeOwnership` が配列を複製してもサイズは変わらない。

プロパティテストは重複キー、tombstone、BLOB、可変長 TEXT、挿入順の反転、挿入後の配列変更を検査する。

### INV-15: RowReader の状態は一意

`RowReader` の状態は `Reading`、`Exhausted`、`Closed` のいずれか 1つにする。
`closed` と `exhausted` を別々の bool にすると、
終端後と Dispose 後の組み合わせ、二重解放を表現できてしまう。

- `RowReaderState` は private discriminated unionで、`Reading` に `Op` を保持する。
- `releaseTo next` は `Reading` のときだけ解放し、状態を遷移させる。
- 解放は一度だけ行われる。
- `Exhausted` と `Closed` はどちらも解放済みで、`TryRead` の結果だけが異なる。

回帰テストは終端、Dispose、Dispose 後の `TryRead` を検査する。

---

## 並行実行とライフサイクル

### INV-16: Session のロック規約

状態ロックと DB 全体の書き込みロックを役割ごとに分離する。

- `Session.stateGate` は `local` と `readers` を保護する。
- `DatabaseHost.stateGate` は `lifecycle` を保護する。
- `Session.writeGate` は接続内の操作を直列化する。
- `DatabaseHost.writeGate` は書き込み、BEGIN、COMMIT、ROLLBACK、DDL、Close を直列化する。
- SELECT のみのバッチは `writeGate` を取らず、`AcquireReadSnapshot` で並行する。
- snapshot 登録は種別（`Transaction` / `Statement`）を持ち、
  DDL の可否は `TransactionExists`（明示 transaction だけ）で判定する。
  読み取り・暗黙 transaction は共有ロックで待つ。

回帰テストは読み取り中の書き込み、書き込み中の読み取り、Close 待ち、並行 snapshot、同一 Session の並行使用、
BEGIN が書き込みを待ち SELECT を待たないことを検査する。
ロック規約は決定的テストで検査し、ランダムなストレステストは実施しない。

### INV-17: lease の所有権

process lock と transaction の lease は所有者を明示する。

- `ProcessLockLease` は root とトークンだけを保持し、解放は `ForceRelease`が担当する。
- 二重解放は `ProcessLock.release` の台帳で無害になる。
- DB を開く所有者は `DatabaseHost` 1つで、接続ごとの所有権トークンは持たない。
- transaction の登録は `TxnState.Lease` が `RegistrationLease`として保持する。
- 接続が Closeされずに捨てられた場合、最後の lease が GCされた時点で登録を外す。

回帰テストは未 commit の transaction の GC、存続中の transaction、
Dispose、別プロセスからの open、Close 後の再 open を検査する。

### INV-22: mutable 状態は許可されたスロットだけに残す

`Session.fs` と `DatabaseHost.fs` に残す mutable は、
状態 discriminated unionを保持するスロットと `readers` カウンタだけに制限する。

- `CommitDurability.SyncState`: `Idle`、`Leading`、`Failed`
- `DatabaseHost.HostLifecycle`: `Running of Committed`、`Closed of Committed`
- `Session.LocalSession`: `Open of TxnState`、`Closed`
- `SessionPool.PoolLifecycle`: `Open`、`Closed`
- `RowReader.RowReaderState`: `Reading`、`Exhausted`、`Closed`

クエリ局所の例外は `ValueSet.IndexedValues` と `BloomFilter.BloomBuilder`だけにする。
bool、option、カウンタを別々に保持すると成立してはいけない組み合わせが作れる。
詳細な使用制限は [`coding-conventions.md`](coding-conventions.md) を参照する。

回帰テストは commit durability、並行 Session、Close、SessionPool を検査する。

### INV-24: 公開 Version とファイル refcount

compaction の公開状態は、Versionの世代、採番、参照中ファイルの一貫性を保つ。

- `publish` による `Generation` と `Version.NextFileNumber` は単調である。
- `install` した Version が参照するファイルは存在し、pin 中は削除できない。
- retired ファイルは pin が 0になってから削除する。

実装では、`ConcurrentDictionary` の1セルと CAS を使う。
ファイル番号は同じセルで予約し、`publish` は現在値との `max` を取るため番号は戻らない。
MANIFEST追記と install は `installGate`で直列化する。
`AcquireCall` / `AcquireVersion` はVersion単位の参照数を増やし、最初のleaseだけ全ファイルをpinする。
最後のleaseがreleaseとsweepを行う。
`RowStore`の走査は`SstScan.withResource`でVersion pinのleaseを結び付け、
Sessionのread leaseより長く走査を保持してもファイルが消えない。
`withResource`は続き（continuation）すべてに後始末を付け、
自然終端・途中破棄・エラーのどこで止まってもleaseを1回だけ解放する
（`ScanScope.Release`の冪等性を使う）。
`ScanScope`は最終的な走査を1つだけ登録し、正常終了時の`Dispose`で登録を外す。
`RowStore.getAtMasked`も短いVersion leaseを取る。
leaseの取得と解放はCASだけで、`installGate`を取らない。
`AcquireCall`も`installGate`を取らず、状態を読んでpinした後に参照等価で状態が変わっていないことを確かめ、
変わっていれば解放してやり直す
（installはpublish→retireの順なのでpin後は削除されない）。

プロパティテストは並行読み取り、書き込み、snapshot pin、固定シードの操作列、
Generation、採番、Version の妥当性、pin中のファイル存在を検査する。
プロセス断をランダム操作列へ挿入するテストは未実施である。

### INV-25: commit の WAL 位置は fsync 前に登録

`DatabaseHost.serialized` は、実行後の `WalState` を `writeGate` の内側で
`CommitDurability.Register` に登録し、`writeGate` の外側で `Await`する。

`Register`を忘れると leader の fsync targetに待機中のcommitが入らず、
グループコミットが機能しない。
単体テストだけでは統合経路の登録漏れを検出できないため、
`DatabaseHost.serialized` の経路を統合テストする。

回帰テストでは、同時に3 commitが2回の fsyncにまとまり、最後の fsyncが3番目の WAL位置まで届くことを検査する。

### INV-26: 確定状態と transaction を分離

全接続で共有する確定状態 `Committed` は transactionを保持しない。

`Committed` は `Root`、`Store`、`Catalog`、`Manager`、`DeferDurability`だけを持つ。
接続ローカルの transactionは `TxnState`が保持する。

- `Committed` に transactionフィールドは存在しない。
- `HostLifecycle`は `Running of Committed`または `Closed of Committed`だけを持つ。
- 文の実行時だけ `ExecState = { Committed; Txn; Registration }`を組み立てる。
- `ExecState.Catalog` はtransaction中は `Txn.CatalogAtBegin`、 transaction外は `Committed.Catalog`を使う。

回帰テストは SessionPool、DatabaseHost、Sessionのtransaction、再オープン、DDL排他で経路を検査する。

## 式、射影、検索

### INV-18: 式ノードの children と arity

`Expr.plugChildren (Expr.children e) e` は元の式を復元し、
子の個数が合わない場合は `InternalError`を返さなければならない。
列挙と組み立てを別々のmatchで実装すると、片方だけ更新漏れが起きる。

`Expr.children`と `Expr.plugChildren`は、
`Expr.mapChildrenState`の1つの網羅matchから導出する。

- `step`はchildren列挙時に子を状態へ積む。
- 同じmatchで `plugChildren`用の組み立ても処理する。
- 新しい union caseを追加すると網羅性検査により更新漏れを検出する。

`AstTests.allNodeKinds`は葉、1～3個の子、可変長の子、
CASEのELSE有無、LIKEのESCAPE有無、サブクエリを含む型を検査する。

回帰テストは復元順、渡した順序通りの組み立て、arity不一致の `InternalError`を検査する。
新しい `Expr` caseを追加した場合は `allNodeKinds`にも追加する。

### INV-27: DB・プロセス寿命の可変キャッシュを持たない

可変状態は並列制御だけに置く。
式評価と SSTable読み取りの前計算は計画時または `Version`構築時に不変値として作る。

- `Eval.Prepared` は計画時の不変 `ImmutableDictionary`を保持する。
- 参照列とリテラルLIKEを一度解決し、行ごとの再解決をしない。
- `FileMetaData.ReadInfo`（`SstReadInfo`）は書き出し時またはopen時に一度だけ作り、不変の `Version`へ載せる。
- mutableコレクションはコーディング規約の許可リストに限定する。

回帰テストはリテラル述語、LIKEパターン、DISTINCT、GROUP BY、ハッシュ結合、再オープン時の破損検出を検査する。

### INV-28: 索引エントリの値は空

索引 keyspace `i:<index>` の値は常に `Array.empty` とする。
論理キーは、索引列と主キー列をこの順で連結したものとする。

- 書き込みは `Dml.applyIndexChange` と `Engine.backfillIndex`だけが行い、どちらも空値を書く。
- 読み取りは `RowKey.indexKey`と `opOfIndexScan`が行う。
- transactionの索引書き込みも生キーでmergeする。
- `Dml.indexAccessMasks`がscan maskとfetch maskを返す。
- 被覆外の列は `ExecContext.fetchMissing`が主キー lookupで補う。
- 被覆列（索引列 + 主キー）だけを参照する conjunct は fetch の前に評価してよい
  （`Optimizer.execIndexScan` / `Engine.indexCandidateScan` の `preFilter`）。
  WHERE 全体は呼び出し側で再評価し、preFilter に移した conjunct も元の式に残す。
- UNIQUE backfillは `RowStore.scanKeys`でキーだけを走査し、重複を検査する。
- `ALTER TABLE`はテーブル本体の行だけを書き換える。

回帰テストは索引値が空であること、キー復号、maskされた列の `Unloaded`、transaction書き込みのoverlay、
非被覆索引のfetch、複数のNULL、ALTER TABLEを検査する。

### INV-31: INL は右部分木の制約を落とさない

インデックス入れ子ループでは、右部分木を実行しない。
そのため右の `Scan`に含まれる制約は residualとして再評価できる必要がある。

`Analyzer.impliedByScan`は、`PkLookup`と `IndexLookup`が保証する等値述語を `Filter`から除く。
吸収済み述語は `ScanMethod`に存在しても `keys.Residual`には残らない。
このまま INLにすると右テーブルだけの述語が消える。

したがって `Analyzer.absorbedPredicates`が `ScanMethod`とテーブル/索引定義から等値を `Expr`へ再構成し、
`chooseJoinAlgorithm`が residualへ加える。
再構成できないときは INLを選ばない。
再構成した列は右側の列マスクへ追加する（`Optimizer.execIndexJoin`）。

回帰テストは、右のpk lookup/index lookupがresidualを再構成すること、
索引が見つからない場合はINLを選ばないこと、吸収済み述語を落とさないことを検査する。

### INV-32: 射影の `*` は論理スキーマ順

`SELECT *`と `t.*`の列順は FROM順とする。
JOINの物理レイアウトや結合順の並べ替えに依存しない。

結合順を並べ替えると `outputOf`は変わるが、ヘッダは `plan.Input`（FROM順）で作る。
展開と評価に別々のスキーマを使わないとヘッダと値がずれる。

`ProjectFrame`は展開用 `inSchema`と評価用 `outputOf`を分け、
`Ops.project`は `SelectProjection.create expandSchema`と `Eval.prepare evalSchema`を使う。

回帰テストはJOINの並べ替え、派生テーブル、`INSERT ... SELECT *`でFROM順の列を返すことを検査する。

### INV-33: ハッシュ結合の build 側とパーティションの寿命

INNERハッシュ結合は推定行数の小さい側をbuildし、大きい側を1回だけ流す。
左をbuildした場合に左が退避したら、右をbuildする通常経路へフォールバックする。
LEFTは左行を保存する必要があるため常に右をbuildする。

ハッシュ結合のパーティションは読み終えた時点で削除する。
早期終了で読み切れなかったrunは `ScanScope`が閉じて削除する。

回帰テストは、左buildと右buildの結果一致、左退避時のフォールバック、
早期終了後の一時ファイル残存、`HashJoin(left)`のEXPLAIN表示を検査する。

### INV-34: 走査とポイント検索の tombstone

範囲走査（`StorageEngine.scanAtValues` / `scanAtMasked`）は可視・最新・非 tombstone のエントリだけを返す。
ポイント検索（`StorageEngine.getAtMasked`）は最新版をそのまま返し、削除を示す `Value = None` を返す。
`RowStore.getAtMasked` が None にする。

`Dml` の overlay は「走査に tombstone は現れない」ことを前提にする。
現れても読み飛ばす防御分岐はあるが、通常経路では通らない。
逆に、engine が tombstone を返すようになるとMVCC の可視性判定と overlay の両方が壊れるため、
この契約は回帰テストで固定する。

回帰テストは `StorageEngine: 走査は tombstone を返さず、ポイント検索は削除を Value = None で返す`
（memtable と SSTable の両方）を検査する。

### INV-35: 統計キーはテーブル定義の寿命に従う

`ANALYZE` が保存する `stats:<table>` は、そのテーブルの定義が存在する間だけ有効である。

- `DROP TABLE` はテーブル・索引・統計を同じカタログ差分で消す。
- `ALTER TABLE` は行の形が変わるため、テーブルの統計を transaction 内で tombstone にする。
- 統計が無い・古い場合は見積りが粗くなるだけで、クエリ結果は変わらない。
- DML では統計を自動更新しない（手動 `ANALYZE` のみ）。

回帰テストは `SqlReference: ANALYZE の統計は再オープン後も残り、ALTER TABLE で消える` と
`Catalog.save / load: 統計を往復し、removeTable で消える` を検査する。

### INV-36: 真偽の解釈は 1か所で行う

`WHERE` / `HAVING` / `ON`、`NOT`、`AND` / `OR`、`CASE WHEN`、`IS [NOT] TRUE/FALSE/UNKNOWN` は
同じ真偽の規則を使う。
規則を分けると `WHERE 5` と `WHERE NOT 5` が同時に真になる。

- `Eval.truthOfValue` が唯一の変換である。
- `BOOLEAN` はそのまま、`INTEGER` は `0 = FALSE`・非 `0 = TRUE`、それ以外の型は `UNKNOWN`。
- `NOT` は `UNKNOWN` を `NULL` へ戻し、`WHERE` は `UNKNOWN` を偽として扱う。
- 実行演算子の構築時にも同じ解釈を使う（`Eval.matchesWithV`）。

回帰テストは `Eval.eval: 真偽の解釈は WHERE / NOT / CASE / IS UNKNOWN で一致する` と
`SqlReference: WHERE の真偽と NOT / CASE WHEN / IS UNKNOWN が一致する` を検査する。

### INV-37: マージ結合のキーは連言の位置の対応を保つ

等値キーのペアは ON の連言の位置で決まる。
左右を列名で独立に並べ替えると、`ON a.id = b.k AND a.k = b.id` のように名前が一致してもペアが反転し、
誤った列同士を比較して静かに 0行になる。

- `Analyzer.mergeJoinKeys` は並べ直した左の順に右の列が連言の位置どおり対応するか検証する。
- 対応が崩れるとき・片側の走査順が結合キーと一致しないときは選ばない（`HashJoin` へ）。
- 片側が単一行の走査なら、もう片側の走査順へ連言の位置から合わせる。

回帰テストは `Analyzer.chooseJoinAlgorithm: クロス名の等値はマージ結合にしない` と
`SqlReference: クロス名の等値結合は行を取りこぼさない` を検査する。

### INV-38: 退避した値集合の Bloom は SQL 等値の全表現を網羅する

`ValueSet` の OnDisk Bloom は偽陰性を出してはならない。
SQL の等値は型・DECIMAL のスケール・DATE/TIMESTAMP を跨ぐため、
値の符号化 1つだけをキーにすると等しい値で偽陰性になる。

- `ValueKey` が等値になり得る表現をすべてキーにする。
  （数値の Int/Decimal/Real、DECIMAL の末尾 0 除去、DATE と同日 00:00 UTC の TIMESTAMP）
- 格納側（`addBloom`）と探査側（`Contains`）の両方で同じ候補集合を使う。
- 偽陽性はブロック読みを増やすだけで正しさに影響しない。

回帰テストは `ValueSet: 退避しても型・スケール違いの等値を取りこぼさない` と
`ValueSet: 退避しても DATE と同日 00:00 UTC の TIMESTAMP を等値に扱う` を検査する。

### INV-39: 等値キーの符号化は SQL の等値と一致する

`DISTINCT`、`GROUP BY`、ハッシュ結合は同じ等値で行をまとめる必要がある。
符号化を別々にすると、`DECIMAL` の `1.0` と `1.00` や `REAL` の `-0.0` と `0.0` で
演算子ごとに答えが変わる。

- キーは `KeyBuilder.ofValues`（順序保持符号化）を使う。
- `Ops.distinct` の重複キー、`Ops.aggregate` の退避グループキー、`OpsJoin.encodeKey` が対象。
- `ValueCodec` はストレージの値符号化であり、DECIMAL のスケールを保存するためキーに使わない。
- DISTINCT 集約の退避 run は「正規化キー」と「元の値」の 2列を書き、再集計はキーで重複排除する
  （元の値だけで比較すると、退避後にだけ Int 1 と DECIMAL 1.0 を別の値として数える）。

回帰テストは `SqlReference: DISTINCT は DECIMAL のスケール違いを同じ値とみなす` と
`集約: 退避しても DECIMAL のスケール違いを同じグループにする` と
`SqlReference: DISTINCT 集約は退避しても型の違う同じ値を 1つに数える` を検査する。

### INV-40: 有限の REAL から非有限値を作らない

有限の入力から `NaN` / `±Infinity` を作ると、値は出力できても比較段階で初めて型エラーになる。
暗黙の非有限を作らず、演算の時点で `ConstraintViolation` にする。

- `Eval.realArith` は結果が非有限で両入力が有限なら `overflowError` を返す。
- `SUM` / `AVG` の `SumReal` が有限の入力から非有限になった時点でエラーにする。
- 入力自体が非有限の場合は汚染として伝播する（ストレージには保存できる）。

回帰テストは `SqlReference: REAL の演算が無限大になる場合はエラー` を検査する。

### INV-41: 集約の作業メモリは `Seen` を含めて全体で計上する

`COUNT(DISTINCT) ... GROUP BY` の `Seen` はグループごとに持つが、上限は全体で見る必要がある。
グループ局所に判定するとグループ数分だけメモリが伸びる。

- `seenBytes` がバケットの `Seen` のバイト概算を返し、`bucketBytes` と行のたびの増分計上で `used` に合算する。
- パーティションへ退避した後の畳み込み（`Spill.foldEntries`）でも、既存グループの `stateBytes` の増分を毎行計上する。
  新規グループだけを見ると、グループ数分だけ上限を超え得る。
- 上限を超えたら、その行以降をパーティションへ退避する。
- 極小の `WorkMem` では 1 run あたり最低 64値（最大 2KiB の超過）までまとめる。
- `Seen` の件数は `Acc.SeenCount` が持ち、行ごとの上限判定とバイト見積りで
  `Map.count`（木全体の走査）を呼ばない。`Map.count` は 20k ユニーク値で O(n²) になっていた。

回帰テストは `集約: DISTINCT の値が上限を超えても全集約関数が同じ結果` と
`集約: 最後のスピル以降に溜まった DISTINCT 値も結果に含める` を検査する。

### INV-42: 比較の型は計画時に判定し、行の有無に依存させない

`compareValuesV` が型エラーにする組み合わせ（INTEGER と TEXT など）を実行時の行評価に任せると、
対象が 0行のときだけ黙って成功する（同じクエリが行数で成否の変わる「静かな誤答」）。

- `SqlValue.typesComparable` が `compareValuesV` の型判定を写し、`Eval.checkComparableTypes` が
  列の宣言型・リテラル・`Expr.Value`・`CAST` から型が決まる比較（Eq/Ne/Lt/Le/Gt/Ge、BETWEEN、IN リスト）を検査する。
- `Binder.qualify` と `DmlOps.update` / `delete` から呼ぶので、SELECT / JOIN ON / HAVING / ORDER BY /
  UPDATE / DELETE のすべてが対象になる。
- 演算・関数・集約の結果は型を決めないので実行時の判定に委ねる（遅い検出は残るが、誤検出は作らない）。

回帰テストは `SqlReference: 比較できない型は行が 0 件でも計画時に TypeError` を検査する。

### INV-43: 解析用の修飾失敗で ON 述語を落とさない

結合順の解析は ON の `Binder.qualify` をベストエフォートで呼ぶ。
ここで失敗を `None` として捨てると、`on=TRUE`（直積）になり述語が黙って消える。

- 解析用の修飾が失敗したら、**元の ON 式**を解析結果として使う
  （実行時に `buildJoin` がもう一度修飾して同じエラーを返す）。
- どの結合にも昇格できなかった ON 述語は、結合の上に `Logical.Filter` として残す
  （INNER では ON = WHERE）。
  `usedOns` は結合句の番号で管理し、位置と取り違えない。

回帰テストは `SqlReference: 型の違う列の等値結合は TypeError`・
`PlanBuilder.buildLogicalWith: WHERE の結合述語は INNER の ON へ昇格する` を検査する。

---

## 永続化とリソース管理

### INV-19: 保持側は所有権を複製

memtableやtransactionなど保持する側は、渡されたキーと行を複製してから保持する。
呼び出し側が配列を書き換えても格納内容と順序が変わらない。

- `Entry.takeOwnership`がキーと行、Blobを複製する。
- `Memtable.insert`が複製済みのエントリを保持する。
- `Transaction.setSlot`が `put`と `remove`の入口で複製する。
- `Session.Execute(sql, parameters)`と `TryOpenReader(sql, parameters)`は、
  `SqlValue.copyRow`でパラメータ配列とBLOBを複製してからASTへ埋め込む。
- `SqlValue.copyRow`はINV-02の出力とINV-19の入力で共有する。

複製コストは 1行あたり約 0.4µsである。
正しさを優先し、commit経路では行を複製しない最適化は実施しない。
`SstEntry`は読み取り専用のビューで複製せず、走査で得たエントリは保持せず、出力は `SqlValue.copyRow`を通す。

回帰テストはキー配列、行配列、BLOBを挿入後に変更しても Tree、transaction、RowReaderの結果が変わらないことを検査する。

### INV-20: Writer のチャンク順

`Writer.Chunks`は逆順、つまり先頭が最後に書いたチャンクで保持する。
`add`は先頭へ積み、`toArray`は配列の末尾から埋めるため、出力時に`List.rev`を使わない。

- `Writer.fsi`は型を不透明にし、構築を `Writer.Empty`と各write関数に限定する。
- レコード式から `Chunks`を直接組み立てるとコンパイルエラーになる。
- 型のdocコメントに逆順であることを明記する。

回帰テストは64チャンクを混在させ、期待バイト列と完全一致することを検査する。

### INV-29: spill ファイルのライフサイクル

`Spill`が作る runファイルは、書き込み後に必ず閉じ、読み終わったら閉じて削除する。
同じ行を繰り返し読む用途では閉じるだけにし、使い終わった時点で `deleteSpool`から削除する。

- 書き込みは `Spill.openSink`から `writeRow`/`writeChunk`を経て `closeSink`を呼ぶ。
- 読み取りは次の4経路だけに限定する。
  - `foldRun`: 閉じて削除
  - `foldRunKeep`: 閉じるだけ
  - `reopen`: 閉じて削除
  - `reopenKeep`: 閉じるだけ
- `repartitionRun`は元のrunを削除する。
- 開いたrunとspoolは `MemoryBudget.Scope`（`ExecContext.Scope`）へ登録する。
- `ValueSet` の OnDisk 索引（`IndexedValues`）も Dispose で `ScanScope.Forget` する
  （相関 IN で外側行ごとに作ってもメモリが残らない）。
- `LIMIT`や `RowReader.Dispose`の早期終了では `DisposeAll()`が閉じて削除する。
- 通常終了では `ScanScope.Forget`で登録を外してから明示的に削除する。
- 途中で残ったrunは、DB を開くときに `Spill.sweepStale` が 24時間超だけ回収する
  （クエリごとにディレクトリを列挙しない）。

回帰テストは上限超過、再open、破損run、不在ディレクトリ、早期終了、DISTINCT、集約、結合アルゴリズムでの残留を検査する。

---

## トランザクションと整合性

### INV-21: UNIQUE の論理予約

UNIQUE索引の物理キーは「索引値＋主キー」のため、
主キーが違えば同じUNIQUE値でも物理キーは衝突しない。
そこで、同じUNIQUE値を論理予約キーとして transaction中に保持し、commit時に競合を検出する。

- `Transaction.reserveUnique`が `@unique:<索引名>:<base64(索引値)>`を
  `Reservations : Map<string,int>`の参照カウントへ追加する。
- `Transaction.releaseUnique`が参照カウントを減らし、0になったキーを削除する。
- `Dml.applyIndexChange`が旧値を解放してから新値を予約する（文の順序に依存しない）。
- 同じ値の予約はキー1件にまとめ、カウントだけが増減する。
- `Transaction.writeKeys`が物理書き込みキーと**カウントが残る予約**の和集合を返す。
- `TransactionManager.commit`が `writeKeys`で競合を判定する。
- commitした予約をcommit SeqNoで `LastCommitted`へ登録する。
- 物理エントリがなくても予約だけのcommitでSeqNoを進める。
- NULLを含むUNIQUE値は予約しない。
- INSERT文の検査は`DmlOps.UniqueGuard`へ文開始時のスロットを 1回だけ畳み、
  文内で確定したキーは不変`Map`の`Added`へ積む。
  行ごとに`Transaction.slotsOf`を呼ばない。
  `Added`はcommit用の`Reservations`とは別で、文内の可視性だけを担う。
- UPDATE文のUNIQUE検査も、代入対象の列を含む索引の文開始スロットを1回だけ変換して使い回す
  （変更行ごとに`Transaction.slotsOf`を呼ばない）。
- UNIQUE reservationは物理キーではないため、CREATE UNIQUE INDEXのbackfillは不要である。
- DDLは排他の `RootRuntime` で実行し、文を跨ぐ明示 transaction の登録があれば拒否する
  （読み取り・暗黙 transaction の登録は種別が違い、排他ロックで完了を待つ）。

回帰テストは予約の重複排除と参照カウント、解放した予約の非競合、論理予約の競合、
activeなしでの予約破棄、Session間のINSERT/UPDATE、NULL、rollbackを検査する。

### INV-23: 結果行は LoadedRow

公開APIへ返す行は `LoadedRow`だけにする。
`SqlValue.Unloaded`は列マスク復号の内部プレースホルダーであり、SQLのNULLではない。
構成規則だけに頼ると新しい出力経路で漏れるため、出力境界の型で閉じる。

- `LoadedRow`はcaseを隠したstruct discriminated unionとする。
- 構築は `LoadedRow.create`だけで、内部で `SqlValue.ensureLoaded`を呼ぶ。
- `StatementResult`のコンストラクタは非公開とする。
- 行付き構築は `LoadedRow array`を受け取る `StatementResult.Create`だけにする。
- `StatementResult.OfRows`も内部で `LoadedRow.create`を通す。
- `RowReader.TryRead`の戻り値は `LoadedRow option`とする。

回帰テストはUnloaded行の拒否、StatementResultへの混入防止、`SqlValue.dataType`での拒否を検査する。

### INV-30: 外部キーの参照整合性

外部キーは親テーブルの主キーだけを参照する。
CREATE TABLEの時点で列名を解決し、`TableDefinition.ForeignKeys`へ保存する。
DMLは即時RESTRICTを適用する。

- 子のINSERT/UPDATEで親行がない場合は `ConstraintViolation`を返す。
- 外部キー列にNULLが含まれる場合は参照を要求しない。これはMATCH SIMPLEに相当する。
- 親のDELETEで参照中の子行がある場合は `ConstraintViolation`を返す。
- 親の主キーは更新できないため、親側の主キー更新検査は不要である。

実装は次の関数に分ける。

- `ForeignKey.resolveCreatedTable`で型、主キーとの列数・順序、列の存在を検証する。
- `ForeignKey.checkChildRow`をINSERT、INSERT ... SELECT、UPDATEが呼ぶ。
  子スキーマと列位置、maskの準備は`ForeignKey.prepareChildChecks`が文単位で1回だけ行う。
  検査は**行を書いた後**の transaction 状態で行うので、自分自身を参照する行は通る。
  同一文の後続行はまだ書かれていないので参照できない（行単位の即時検査）。
- `ForeignKey.checkParentDelete`をDELETEが呼ぶ。
  子スキーマ、列位置、maskの準備は`ForeignKey.prepareParentDeleteChecks`が文単位で1回だけ行う。
- 子テーブルの探索は、外部キー列から始まる索引、子主キー、全走査の順で行う。
- 複数行の親 DELETE は、索引（または主キー）で引ける子なら 16行以下は行ごとの lookup、
  それ以外は親キーの集合 + 子の全走査 1回で検査する（結果は同じ）。
- DROP TABLEは参照先を持つテーブルを拒否する。
- DROP COLUMNは外部キーに使われる子・親の列を拒否する。
- RENAME COLUMNは外部キー定義の列名を追随させる。

カタログは末尾に外部キー定義を追加する形式とする。
導入前の定義には末尾がないため、外部キーなしとして読み込む。

回帰テストは参照先省略、NameError、主キー以外、型不一致、
複合主キー、カタログ再オープン、INSERT/UPDATE/DELETE、
自己参照（INSERT / UPDATE / 同一文の先行行）、DROP、RENAME、ADD COLUMNを検査する。

### INV-44: 放棄された transaction の回収

接続がCloseされずにGCされた場合でも、二度とcommitできないtransactionを
`TransactionManager.Active`に残さない。
残すと`activeSnapshot`が下がり、`LastCommitted`が以後のcommitのキーを保持し続ける（メモリリーク）。

- `RegistrationLease`のfinalizerは、明示解放されていなければ`onAbandon`で`TxnId`を`DatabaseHost`へ通知する。
- finalizerは`ConcurrentQueue`に積むだけで、`writeGate`/`stateGate`を取らない
  （finalizerスレッドをgate待ちで止めない）。
- `DatabaseHost.serialized`が`writeGate`の内側でキューを回収し、
  `TransactionManager.abortId`で`Active`から外して公開する。
- 回収は冪等で、既にcommit/abort済みの`TxnId`は無視される。
- 明示的なcommit / rollback / Closeは先にリースを解放するので通知しない。

回帰テストは `DatabaseHost: 放棄された transaction は次の書き込みで manager から回収される`、
`DatabaseHost: commit せずに捨てた transaction も GC されれば登録が外れる` を検査する。

### INV-45: commit できない COMMIT は接続を transaction 外へ戻す

write-write競合や書き込みバッファの上限超過でCOMMITが失敗したtransactionは、再試行してもcommitできない。
開いたまま残すと、`Begin`が「既にtransaction中」を返し、以後の文は存在しないtransactionで走る。

- `DatabaseHost.Commit`は`ConflictError`/`CapacityError`のとき`Engine.executeRollback`でabortし、
  managerを公開して`CommitOutcome.RolledBack`を返す。
- `Session`（`Commit()`とSQL `COMMIT`の両方）は、成功・自動rollbackのどちらでも
  接続ローカル状態を返された`TxnState`へ進めてからエラーを返す。
- 競合以外のCOMMIT失敗（WALのI/Oなど）はtransactionを残す。
  WALは書きかけの末尾を切り捨てて再追記するため、同じtransactionを再COMMITできる。
- transactionが無い状態の`ROLLBACK`は何もしない（成功）。`COMMIT`のみエラー。

回帰テストは `SessionPool: 競合した COMMIT は transaction を自動的に rollback する`、
`SessionPool: SQL の COMMIT 競合でも transaction が閉じる`、
`DatabaseHost: 上限を超えた transaction の COMMIT も自動で rollback する`、
`Wal.append: 書きかけの末尾を切り捨ててから追記する` を検査する。

---

### INV-46: fsync 失敗で適用済みの COMMIT の結果を捨てない

`DatabaseHost.serialized`は`run`（commit の実行・publish・lease 解放）を完了させた後、
gate の外で fsync を待つ。
fsync が失敗しても commit は**適用済み**である
（WAL に記録済みで、再オープン時の WAL 再生で復元される）。

- `serialized`は`(run の値, 永続化エラー)`を返し、run が成功した結果を捨てない。
- `DatabaseHost.Commit`は永続化エラーを`CommitOutcome.CommittedUnsynced`
  （適用済み＋永続化失敗）へ変換する。
  `Session`は接続ローカルを transaction 外へ進めてからエラーを返す
  （接続と host の分断を防ぐ）。
- BEGIN / ROLLBACK・失敗した文は WAL を進めないので fsync を待たない。
  待つと、fsync 失敗後の`Begin`が manager に ghost transaction を残す。
- 暗黙 commit（autocommit の DML / DDL）も「適用済みだが Error」になり得る。
  接続ローカルは transaction を持たないので分断は無い。

回帰テストは `DatabaseHost: fsync 失敗の COMMIT は適用済みを CommittedUnsynced で返す`、
`DatabaseHost: fsync 失敗後も BEGIN と ROLLBACK は成功する`、
`Session: fsync 失敗の COMMIT は transaction を閉じてからエラーを返す` を検査する。

---

### INV-47: 集約後の式は計画時にスロット参照へ置き換える

集約後の行は「GROUP BY値 + 集約値」の位置だけに意味がある。
`SELECT`・`HAVING`・`ORDER BY`の式を、GROUP BY式と集約呼び出しのスロット参照へ
計画時に 1回だけ置き換える。

- 行ごとに式を書き換えない（`Expr.rewrite` のコストを行数に比例させない）。
- 置換後に残った列参照はスロット専用のスキーマで解決できず、行を 1つも評価する前に
  `NameError` になる（0行の入力で静かに通らない）。
- 対応付けは表示文字列ではなく、識別子を大小無視に畳み、リテラルに型タグを付けた正規化キーで行う
  （`1` と `1e0` の表示衝突を避ける）。
- `ORDER BY COUNT(*)`・集約の別名・序数・`GROUP BY`式（`ORDER BY age + 1`）も同じ置換を通る。
- 回帰: `sortBy: 集約後の行を集約呼び出し・GROUP BY 式で並べ替えられる` /
  `SqlReference: ORDER BY は集約式・集約の別名・序数・GROUP BY 式で並べ替えられる`。

### INV-48: DML の候補走査は WHERE の十分条件だけを使う

`UPDATE` / `DELETE`の候補行の走査は`SELECT`と同じ規則で選ぶが、走査は候補の**十分条件**にだけ使う。
行の可否は常に`WHERE`の再評価で決める。

- 範囲の閉区間・`LIKE`前置の上限など、走査が広めに返しても結果は変わらない。
- 修飾・型検査に失敗する`WHERE`（未知列など）は全走査で進め、エラーは既存の
  `checkPrepared` / `checkComparableTypes` が返す。
- 候補走査の列マスクは、`UPDATE`は全列、`DELETE`は主キー + 索引列 + `WHERE`参照列。
- 回帰: `SqlReference: UPDATE / DELETE は索引・主キーの走査で候補を絞っても WHERE で再評価する`。

### INV-49: LEFT を跨ぐ INNER は ON の参照が閉じるときだけ繰り上げる

`LEFT JOIN` より後ろの INNER を前へ動かしてよいのは、
ON の参照がその `LEFT JOIN` より前の表だけで閉じているときだけ。
`LEFT` の右（NULL 拡張される表）を見る ON を動かすと、一致判定が NULL 拡張の前後で変わる。

- `PlanBuilder.hoistAcrossLeft` が、先頭 INNER 区間を種にして参照の閉じた INNER だけを繰り上げる。
- 先頭区間の表を 1 つも参照しない INNER（直積）は繰り上げない。
- 繰り上げても中間結果の行数は変わらない（左行ごとの一致行数が同じ）。
- 参照が閉じない INNER は元の位置に残り、その場所から次の INNER 区間として並べ直す。
- 回帰: `PlanBuilder.buildLogicalWith: LEFT JOIN を跨ぐ INNER は前へ繰り上げる` /
  `SqlReference: LEFT JOIN を跨ぐ INNER は LEFT より前に繰り上げる`。

### INV-50: 結合順の見積りと実行時のアルゴリズム選択は同じ条件を使う

結合順（`JoinOrder.transitionVariants`）が前提にするアルゴリズムと、
実行時に `Analyzer.chooseJoinAlgorithmWith` が選ぶアルゴリズムがずれると、
DP は実行されないアルゴリズムのコストで順序を決めてしまう。

- 結合キーは両側が同じ型の等値だけ（`Analyzer.isJoinKeyEquality`）。
  型が違う等値は residual として評価し、入れ子ループとして見積もる。
- INL の候補は右部分木が走査のときだけ（`JoinOrderItem.Lookupable`）。
- マージ結合の候補は左が走査のときだけ（`PlanState.ScanInput`）。
  実行時は左部分木の `scanOrder` を使うため、結合を重ねた左では選ばれない。
- 等値キーが 1本も無い辺だけなら入れ子ループとして見積もる。
- INL の索引選択は `Analyzer.joinLookupFor`、マージのキー対応は
  `Analyzer.mergeKeyOrderFor` を計画側・実行側の両方が呼ぶ（判定の二重実装を置かない）。
- スピル量は `Analyzer.spilledBuildRows` を結合順のコストと `EXPLAIN` の
  `HashJoin(spill=…KB)` で共有する（表示とコストを食い違わせない）。
- 回帰: `JoinOrder.selinger: 等値キーの無い辺は入れ子ループのコストで見積もる` /
  `JoinOrder.selinger: 右が走査でなければ INL のコストにしない` /
  `JoinOrder.selinger: マージ結合は左が走査のときだけ候補にする` /
  `JoinOrder.selinger: 主キーと索引の両方があるとき主キーの INL コストを使う` /
  `Analyzer.joinLookupFor: 主キーが被覆すれば prefix NDV の小さい索引より優先する` /
  `Analyzer.mergeKeyOrderFor: 走査順の先頭が結合キーでなければマージしない` /
  `SqlReference: 型が違う等値は結合キーにせず入れ子ループになる`。

### INV-51: 実測行数の計測は `EXPLAIN ANALYZE` だけが有効にする

行数を数えるラッパを常に挟むと、通常の実行経路に `Op.map` の変換が 1段増える。
計測は診断用のときだけ有効にする。

- `ExecContext.RowCounts` は既定で `None`。
  `Optimizer.execWithCounts` だけが `Some` にする。
- `Optimizer.countOutput` は `None` なら `Op` をそのまま返す（包まない）。
- 数えるのは `Scan` と `Join` だけ（結合順の見積りを確かめる対象）。
- 台帳はクエリごとに作り、結果と一緒に捨てる。
- 回帰: `SqlReference: EXPLAIN ANALYZE は走査と結合の実測行数を返す`。

### INV-52: 統計の鮮度は COMMIT した DML だけで進み、ROLLBACK で戻る

`TableStats.Changes` は「この統計を取った後に変わった行数」で、
`EXPLAIN` が `Stats stale:` を出す根拠になる。

- 実行中の行数は `TxnState.StatsDeltas`（接続ローカル）に置き、確定状態には載せない。
  明示 transaction では COMMIT まで文を跨いで残り、ROLLBACK で捨てる。
- `ExecState.commitTransaction` が同じ transaction に `Catalog.putStatsInTxn` で畳み込むので、
  COMMIT と統計の更新は原子的に決まる。
  ROLLBACK すれば統計は変わらない。
- 統計の書き込みは `Transaction.putAdvisory` で積み、write-write 競合と `LastCommitted` から外す
  （助言情報のため。commit は直列なので最新カタログから畳み込めば正しい）。
- 統計の無いテーブルは数えない（鮮度を示す相手が無い）。
- `ANALYZE` は `Changes = 0` で作り直し、未反映の行数も捨てる。
- 保存形は tag 5（tag 3 に平均行幅、tag 4 に頻出値を追加）。
  tag 1〜4 は `Changes` / `RowBytes` / MCV を 0 / 空として読む。

### INV-53: クラッシュ復旧は COMMIT 済み prefix を保持し、壊れた CURRENT / 出力をロールバックする

保証レベルは[`crash-consistency.md`](crash-consistency.md)に定義する。
C1（プロセスの異常終了）は保証し、C2（ファイル名の消失）は保持している
WAL / MANIFEST / compaction 入力から最善努力で復旧する。

- `CURRENT.A` / `CURRENT.B` は MANIFEST 名と CRC を持ち、有効な方の新しい世代を選ぶ。
  片方が壊れても、もう一方から復旧する。
- `Manifest.openOrCreateWith` は編集履歴を先頭から適用し、
  参照する SSTable がすべて存在する最後の Version を選ぶ。
  壊れた Version は新しい MANIFEST にスナップショットして復旧する。
- 古い MANIFEST と compaction の入力はすぐに削除しない。
  起動時の検証が終わるまで残し、`Manifest.collectGarbage` が保持世代を超えた分だけ回収する。
- WAL v2 のフレームヘッダーは CRC で保護し、長さ破損を torn write と誤認しない。
  ファイルヘッダーが不完全な WAL はフレームを読まない。
- 起動時は残っている WAL をすべて再生し、復旧済み Version の SSTable に含まれる SeqNo 以下は適用しない。
- 回帰: `CrashConsistency: CURRENT スロットの片方が壊れても他方で開ける` /
  `CrashConsistency: CURRENT の両スロットが失われても MANIFEST から復旧する` /
  `CrashConsistency: 最新 WAL の名前が失われても flush 済みデータは残る` /
  `CrashConsistency: 新しい SSTable の名前が失われても WAL から復旧する` /
  `CrashConsistency: compaction の出力名が失われても前の Version と入力から復旧する` /
  `CrashConsistency: WAL v2 のフレームヘッダー破損は CorruptionError` /
- 回帰: `Catalog: 統計の旧形式は Changes 0 として読む` /
  `SqlReference: EXPLAIN は ANALYZE 以降の更新行数を示す`。

### INV-54: 索引の等値検索は接頭辞の上限で切る

`TEXT`/`BLOB`は埋め込みの`0x00`を`0x00 0xFF`へエスケープするため、
短い値の符号化`prefix`は「その値 + NUL + 続き」の符号化のバイト接頭辞になる。
索引キーは`prefix`の後ろに主キーの型タグ（すべて`0xFF`未満）が続くので、
`[prefix, prefix ++ 0xFF)`の範囲が等値のキー集合になる。

- `KeyBuilder.equalityPrefixUpper`が上限を作り、`KeyBuilder.inEqualityPrefix`が範囲を判定する。
- `Dml.scanByIndexPrefixWithSlots`は tx スロットとストア走査の両方にこの範囲を適用する。
- 生の`startsWithPrefix`は範囲走査の下限・keyspace境界用であり、索引の等値には使わない。
- 回帰: `KeyBuilder.inEqualityPrefix: NUL 継続値を等値から除外する` /
  `SqlReference: BLOB 索引の等値検索は NUL 継続値を返さない` /
  `SqlReference: BLOB の UNIQUE は NUL 継続値を重複と見なさない` /
  `SqlReference: 外部キーの子行検索は NUL 継続値を一致と見なさない`。

### INV-55: 相関サブクエリへ渡す外側行はクエリ入力（FROM 順）へ詰め替える

相関サブクエリの外側参照は、計画時にクエリの入力スキーマ（FROM 順）の列位置へ解決される。
一方、実行時の行は結合順を並べ替えた物理順になる。
そのまま渡すと外側参照が別の列を読む。

- `Optimizer.rowRemap`が評価スキーマの列を FROM 順の位置へ写す表を作り、
  `adaptCorrelated`が相関サブクエリを含む式の`Run`へ詰め替えを挟む。
- 対象は Filter・Sort・Project・JOIN ON（と residual）。
  相関サブクエリを含まない式には何もしない（行ごとの割当を増やさない）。
- 評価行に存在しない列は`SqlValue.Unloaded`で埋め、`Plan.substituteOuterGo`が
  外側参照として解決しない（後続 JOIN のテーブルを参照した場合は`NameError`）。
- 回帰: `SqlReference: 相関サブクエリは結合順を並べ替えても外側行を取り違えない` /
  `SqlReference: 相関サブクエリ`（既存の WHERE・ON・SELECT・ORDER BY 経路） /
  `SqlReference: 相関サブクエリの未対応形`（後続 JOIN の参照は NameError）。

### INV-56: IN サブクエリ式のキーは構文で決まる

`InSet`の`Membership`は `Contains` / `Release` の関数を持つので、
表示や等値では同じ構文のサブクエリを一致させられない。
出現ごとに別インスタンスでもある。

- `Ast.SubqueryKey.ofStatement`がパース結果から構文キーを作り、`Membership.Id`に入れる。
- `OpsShared.exprKey`は`InSet`を「左辺 + 否定 + 構文キー」で表す。
  これで`SELECT`と`GROUP BY`に同じ構文で書いたサブクエリが同じ集約スロットに一致し、
  内容の違うサブクエリは別のキーになる。
- 入れ子のサブクエリを含む文は式の表示で区別できないため、構文キーを作らず一意な印を付ける
  （一致せず`NameError`になり、静かな誤答にしない）。
- 相関`IN`の集合は外側行ごとに作るので、識別子は使わない。
- 回帰: `SqlReference: GROUP BY の IN サブクエリは集合ごとに別のキーになる` /
  `SqlReference: GROUP BY の相関サブクエリ`（SyntaxError）。

### INV-57: 集約呼び出しは集約を置ける位置でのみ評価する

集約を評価できるのは集約ノードの上（SELECT 項目・HAVING・ORDER BY）だけ。
`WHERE` / `JOIN ... ON` / `GROUP BY` / `UPDATE` の `SET` に書かれた `AggCall` は計画時に拒否する。
行が 1行も無くてもエラーにする（評価時まで遅らせると、空のテーブルで黙って 0行を返す）。

- `Eval.prepare` が `FirstError` に `AggCall` のエラーを載せ、
  `checkPrepared` が行を 1つも評価する前に返す。
- 集約後の式は計画時にスロット参照へ置き換わるので、残った `AggCall` だけが対象。
- メッセージと種別は実行時と同じ（`COUNT は集約なのでここでは評価できません` / `NameError`）。
- 回帰: `SqlReference: WHERE の集約は 0行でも計画時にエラー`。

### INV-58: 既定式は保存後に再パースできる形にする

`ColumnDefinition.Default` はカタログへ `Expr.toDisplayString` の文字列で保存し、
再オープン時に `Parser.parseExpression` で復元する。
表示が式として読み戻せない値は、永続化した時点で壊れる。

- 非有限 `REAL` のリテラルは `Infinity` / `NaN` と表示され、式として復元できない。
  `CREATE TABLE` / `ALTER TABLE ... ADD COLUMN` の `DEFAULT` では拒否する
  （`EngineDdl.validateDefaultPersistable`）。
- `CAST('NaN' AS REAL)` のような式は表示も式なので保存できる。
- 既定式の評価は従来どおり INSERT / ALTER の行埋めのとき（CREATE では評価しない）。
- 回帰: `SqlReference: DEFAULT に非有限の REAL は指定できない` /
  `SqlReference: 非有限 REAL でも式で書いた DEFAULT は再オープン後も使える`。

### INV-59: 派生テーブルは参照名で引き当てる

派生テーブルの別名は参照名であり、実表名とは独立する。
実表名で派生の計画を引くと、`FROM (…) AS foo JOIN foo AS f` で
`foo AS f` まで派生の計画に置き換わり、存在しない列を読んだり
同じ走査ノードが 2 回現れたりする。

- `Plan.registerSources` と `PlanBuilder.bindSource` は
  `defaultArg r.Alias r.Name` で引く。
- 別名なしの実表と派生の別名が同じ場合は、参照名の重複として先に `NameError` になる。
- 回帰: `SqlReference: 派生テーブルの別名は実表名と衝突しない`。

### INV-60: SELECT のみのバッチは開始時の snapshot を登録する

読み取り専用バッチは開始時の committed 状態を固定して実行するが、
文ごとの snapshot 登録はその文の実行中しか無い。
文と文の間に commit + compaction が入ると GC 境界が前進し、
後の文が開始 snapshot の版を見落とす。

- `DatabaseHost.AcquireReadSnapshot` が `ActiveRegistrationKind.Statement` で登録し、
  Version pin と同じ lease で `Session.exitRead` まで維持する。
- 種別は`Statement`なのでDDLは拒否しない（共有ロックで文の完了を待つだけ）。
- 例外で抜ける経路はSessionの`finally`がleaseを返す。
- 回帰: `DatabaseHost: AcquireReadSnapshot はバッチの snapshot を GC 境界に固定する`。

### INV-61: 同じ列どうしの等値の選択性は NULL 率だけを掛ける

`WHERE t.a = t.a` は NULL でない行すべてで真になる。
独立な 2列として `1/max(NDV)` を掛けると、行数を NDV 分の 1 に過小評価する。

- `Analyzer.equalityPredicateSelectivity` が同じ列（修飾子と名前が大小無視で一致）を検出し、
  `1 - NULL率` を返す。
- 別の列どうし（`t.a = t.b`）は従来どおり独立な列として `1/max(NDV)`。
- 統計が無いときは 1.0（行を減らさない）。
- 回帰: `SqlReference: 同じ列どうしの等値は行を減らさない見積りになる`。

### INV-62: Bushy の区間木には区間内の述語だけを載せる

bushy は先頭 INNER 区間（3〜6表）の木を組み、使わなかった述語を区間木の上に `Filter` として残す。
区間外のテーブルを参照する述語をそこに載せると、区間木のスキーマで列を解決できず `NameError` になる。

- `PlanBuilder.bushyResultOf` は leftover の ON / WHERE 述語を
  「参照表がすべて区間内」のものだけ区間木へ載せる。
- 区間外の述語は未使用のまま返し、`leftoverPredicates` が結合木全体の上で評価する。
- 回帰: `SqlReference: bushy 計画でも区間外を参照する WHERE が使える`
  （bushy が選ばれたことを木の形で確認してから、LEFT の後ろの表を参照する WHERE を実行）。

### INV-63: 列パターン LIKE のキャッシュは演算子スクラッチで上限付き

列パターン（`a LIKE b`）は計画時にコンパイルできない。
評価中に見たパターンを覚えないと、同じパターンを毎行コンパイルする。

- `Eval.Prepared.LikeCache` が演算子 1つ分の `Dictionary` でコンパイル結果を保持する。
- 上限 1024件を超えたら追加しない（高カーディナリティのパターンでメモリを食わない）。
- `Prepared` は演算子の寿命だけであり、DB / プロセス寿命のキャッシュにはしない。
- 回帰: `Eval.prepare: 列パターンの LIKE は同じパターンをコンパイルし直さない`。

### INV-64: 細工された SSTable は構造検査で CorruptionError にする

層状 CRC が正しい（作成側の規約違反）ファイルでも、構造の不整合は検出する。

- `decodeBlockEntry` / `scanLookupBlock` はブロック内のキーが直前キー以上であることを検査する
  （同一キーの複数版は許す）。
- `parseRawBlock` はブロックの先頭キーが前ブロックの `LastKey` 以上であることを検査する。
- Footer は `MaxSeqNo` が非負、`EntryCount = 0` と索引が空が同値であることを検査する。
- Meta は空（空テーブル）以外の解析失敗を CorruptionError にする。
- 回帰: `SstableReader.rangeScanPartial: ブロック内のキーが昇順でなければ CorruptionError` /
  `SstableReader.openPartial: Footer の MaxSeqNo が負なら CorruptionError` /
  `SstableReader.openPartial: エントリ数と索引の有無が食い違えば CorruptionError`。

### INV-65: 相関サブクエリの外側参照は枝（UNION ALL）ごとの入力スキーマで判定する

相関サブクエリが `UNION ALL` の枝を持つとき、枝ごとに入力スキーマが違う。
全枝の入力を連結したスキーマで「内側で解決できるか」を判定すると、
枝 2 に外側と同じ名前の表（別名）があるだけで枝 1 の外側参照を内側と誤判定し、
実行時に `NameError` になる。

- `Plan.branchSchemasOf` が複合計画の枝（コア → `UNION ALL` の順）の `Input` を取り出す。
  `Sort` / `Limit` のラッパは剥がして `Logical.UnionAll` を探す。
- `Plan.substituteStatement` は枝ごとのスキーマで置換し、
  枝の数が文と合わないときは全体の入力（保守的）で埋める。
- 回帰: `SqlReference: UNION ALL の枝を持つ相関サブクエリは枝ごとに外側参照を判定する`。

### INV-66: DROP の後始末は失わず、失敗で open を止めない

DROP の後始末対象はカタログ削除前にジャーナルへ書く。
ジャーナルは後始末が完了したときだけ消す。

- カタログ保存が失敗してもジャーナルを消さない
  （WAL の commit だけ成立した場合、再起動後のカタログは削除済みで後始末が必要）。
- 復旧時の後始末に失敗しても open は止めない。途中までの tombstone を含む状態を返し
  （SeqNo を巻き戻さない）、ジャーナルを残して次の open で再試行する。
- ジャーナルを読めない場合も open は続ける（対象を特定できないだけ）。
- `.orphans/` の孤児は退避時に mtime を更新し、7日を過ぎたものを起動時に回収する。
- 回帰: `Session: カタログ保存に失敗した DROP は後始末ジャーナルを残す` /
  `Session: 壊れた DROP ジャーナルでも open は続く` /
  `DropCleanupRecovery: 後始末に失敗してもジャーナルを残して続行する` /
  `StorageEngine.openDatabaseWith: 検疫済みの孤児は保持期間を過ぎたら削除する`。

---

## 既知の未対応

### 番号の統一

`FileMetaData.FileNumber`と `Version.NextFileNumber`は
同じ意味なのでどちらも素の `int64`のままにする。
units of measureは意味が違う番号にだけ付ける。

### recordリテラル

`Entry`と `SstEntry`はrecordリテラルから直接構築できる。
ソース検査は追加しない。

- `Entry.put`と `delete`、`SstEntry.make`は構築時の不変条件を持たない。
- 所有権はINV-19で担保する。
- keyspaceの付加と剥離は `Keyspace`の管理範囲に残す。

この検査を強制しても守れる不変条件が増えず、凝集性を損なう。

### 複製の重複

commit経路では `Transaction.put`から `Memtable.insert`へ行を渡すまでに最大2回複製する。
線形型を導入して複製済みの所有権を移せるまでは、コストを許容して正しさを優先する。
