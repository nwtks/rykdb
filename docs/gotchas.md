# 開発で踏んだ落とし穴

この文書は、RykDb の実装で実際に発生したバグと、その再発防止策を簡潔にまとめたもの。
設計は [`architecture.md`](architecture.md)、不変条件は [`invariants.md`](invariants.md)、コーディング規約は [`coding-conventions.md`](coding-conventions.md) を参照する。

## 目次

1. [基本原則](#1-基本原則)
2. [Pull 演算子と再帰](#2-pull-演算子と再帰)
3. [計画・実行・スキーマ](#3-計画実行スキーマ)
4. [LSM・WAL・MANIFEST](#4-lsmwalmanifest)
5. [MVCC・transaction](#5-mvcctransaction)
6. [索引・結合・クエリ意味論](#6-索引結合クエリ意味論)
7. [メモリ・退避・列読み込み](#7-メモリ退避列読み込み)
8. [並行実行・接続・通信](#8-並行実行接続通信)
9. [F# とビルド](#9-f-とビルド)
10. [テストとデバッグ](#10-テストとデバッグ)
11. [実装時チェックリスト](#11-実装時チェックリスト)

---

## 1. 基本原則

以下を守ると、多くの落とし穴を事前に防げる。

- 履歴がある処理は、実際のトラブルの再現テストから作る。
- 失敗は例外ではなく `Result<'T, DbError>` またはホットパス用の `DbResult<'T>` で返す。
- 同じループ・再帰・スキーマ解決を複数箇所に実装しない。
- 可変な操作用コレクションは、演算子やクエリごとに作り、モジュールスコープやクエリ間で共有しない。
- 配列・キー・行を内部に保持する側は所有権を取り込むため複製する。
- クエリの状態は値として進める。ロジック層に無制限の可変キャッシュを持たない。
- 複数の長大な走査は末尾再帰か CPSで、フレームが要素数や深さに比例しない形にする。
- commit point は WAL の commit 到達時。後続の flush / compaction の失敗で成功済み commit を巻き戻さない。
- 破損・不足・不整合は黙って空結果や既定値にせず、`DbError` を返す。
- 変更後はビルド、テスト、TailAudit を単独実行し、必要なら性能ベンチを測る。

---

## 2. Pull 演算子と再帰

### 2.1 `Op.Step` は遅延ではない

`Op.Step(row, nextOp)` の第2引数は「次の行を生成する関数」ではなく、すでに計算された継続。
厳格評価なので、再帰で次の継続を第2引数に直接渡すと、全行ぶんの連鎖を構築するか無限再帰になる。

```fsharp
// 非推奨（第2引数を構築時に評価してしまう）
let rec loop () = Op.emit row (loop ())

// 正しい
let rec loop () =
    Op.compute (fun () -> OpStep.OpRow(row, loop ()))
```

有限の既知の連鎖には `Op.emit`、それ以外は `Op.compute` を使う。

### 2.2 継続を別ループへ変更しない

filter・map・join は、継続を「今のループの残り」に進める。述語を通した後に元の継続を捨て、別のループを作り直すと、行が重複・欠落する。
JOIN では、結合済みの左行に対する「次の右行」を、元の右走査ではなく現在の結合ループへ渡す。

### 2.3 `>>=` に再帰を載せない

`let rec go ... = next cur >>= fun ... -> go ...` は、フレームを要素数ぶん積む。末尾再帰の形に直す。

```fsharp
let rec go cur =
    match Op.next cur with
    | OpStep.OpError e -> ...
    | OpStep.OpDone -> ...
    | OpStep.OpRow(row, next) ->
        add row
        go next
```

再帰の末尾呼び出しに処理を残さない。`Result.map`・`>>=` の引数やレシーバ内で自己再帰しない。

### 2.4 `DISTINCT`・集約・ハッシュ結合はクエリごとに作る

`seen`・グループ表・右側ハッシュ表をモジュールスコープの値にすると、複数の SELECT や並行セッションで状態が共有される。
演算子を構築する関数の中で作り、返した `Op` の中だけで使う。

### 2.5 エラーを空行へ変換しない

式評価や JOIN の ON 評価が失敗したとき、`Op.Empty` を返すと「一致なし」に見えてエラーが消える。
`OpError` として呼び出し元まで伝播する。

### 2.6 `Op` は one-shot

`Op.next cur` を呼んだ後は、元の `cur`ではなく返された `nxt` を使う。
同じ値を再利用すると、行が重複・欠落・無限ループになる。
構築は `Op.Empty` / `ofRow` / `ofArray` / `emit` / `compute`だけを使う
（ケースは `.fsi` で非公開）。

### 2.7 配列へ変換するときのメモリ

`Op.toArray` と外部走査では、複数の行配列を途中で実体化しない。
制限やメモリ上限がある場合、上限を超えたらスプールし、ストリーミングで完走する。

### 2.8 `DISTINCT` と集約のキー

重複判定やグループキーを文字列化しない。
`ValueCodec.writeValues` で符号化したバイト列を使う。
メモリ上限を超えたら、最初に現れた順を保ちながらパーティションへ退避する。

---

## 3. 計画・実行・スキーマ

### 3.1 下へ渡すスキーマは実行時の入力スキーマ

`Sort` / `Aggregate` / `Join` のフレームに入れるスキーマは二種類ある。

- 子を読む側のスキーマ（子の出力）
- 子を実行する側のスキーマ（現在の入力）

下へ渡すのは入力スキーマ。集約後のヘッダを子へ渡すと、集約引数や GROUP BY 列が解決できない。

### 3.2 入れ子 JOIN は左部分木の実際の出力スキーマを渡す

結合順を並べ替える場合、元のスキーマから右テーブル名を取り除く方法では列位置がずれる。
`outputOf ctx inSchema lTree`で左部分木の実レイアウトを求める。

### 3.3 JOIN の右部分木を捨てない

WHERE pushdownで右部分木に付いた `Filter`を実行せずスキャンを直接呼ぶと、右テーブルだけの述語と横断述語が消える。
INNER JOIN のみに安全な pushdown を行い、LEFT JOIN の右側述語は結合後に置く。
`Analyzer.impliedByScan` が走査方法で保証済みの等値述語を `Filter` から除くため、
右が `PkLookup` / `IndexLookup` のスキャンでは述語が `Filter` に残らない。
INL は右部分木を実行しないので、吸収済みの等値を `Expr` に再構成して residual へ戻す
（`Analyzer.absorbedPredicates`）。
再構成した列は右の列マスクにも追加する。
含めないと `Unloaded` と比較してしまう（`Optimizer.execIndexJoin`）。
再構成できないときは INL を選ばない。

### 3.4 JOIN の対応付けはテーブル名ではなく位置と別名で行う

自己結合では同じ実テーブル名が複数ある。対応する JOINはbindingsの位置で決める。
同じテーブル名をエイリアスなしで複数指定する場合は `NameError` にする。

### 3.5 集約ノードを包みから直接探さない

集約ノードは `Limit → Distinct → Project → Sort → Filter`などのラップに隠れる。
ラップを降りながら `Aggregate`・`Having`を探す。

### 3.6 ORDER BY は修飾名を優先して解決する

集約後の出力では列名が修飾名のまま残る場合がある。
修飾付きから解決し、失敗したときだけ修飾なしへフォールバックする。

### 3.7 ORDER BY の序数は計画時に式へ置き換える

`ORDER BY 1`を式として評価すると定数ソートになる。
実装は1始まりで項目へ置換し、範囲外は構文エラーにする。

### 3.8 SQL の処理順序を揃える

論理計画は概ね次の順序。

`FROM/JOIN → WHERE → GROUP BY/HAVING → 集約 → ORDER BY → 射影 → DISTINCT → LIMIT/OFFSET`

特にDISTINCTは射影の後、LIMITは射影とDISTINCTの後に適用する。

### 3.9 サブクエリの計画と実行を分ける

派生テーブルや相関サブクエリを計画時に実行しない。

- 非相関サブクエリは計画時またはクエリ開始時に1回畳み込む。
- 相関サブクエリは外側行ごとに再計画・実行する。
- 相関述語は Filter pushdownやJOINへ昇格しない。
- 集約後の式、LIMIT、OFFSETは外側スコープを持たない。

派生テーブルは内側のエラーを外側のスキーマへ漏らさない形で別計画・別実行にする。

### 3.10 FROMなしのSELECTは1行0列

`SELECT 1`は`Logical.Values [||]`（1行0列）を入力にする。
`WHERE`や集約をこの1行へ適用する。
`FROM`なしの`*`や未知の`t.*`は成功させずエラーにする。

### 3.11 LIMIT/OFFSETは定数式に限定する

負数、列参照、定数に評価できない式、`Int32`上限を超える値はエラー。
`LIMIT 0`は0行、`LIMITなし`は別の状態として扱う。

### 3.12 未知の列・別名・識別子を黙らせない

未知の修飾列、未知の`t.*`、重複する識別名は`NameError`または`SyntaxError`。
識別子比較は`SqlName.equal`を共通利用する。

### 3.13 未対応のSQL語を別名へ吸収しない

`UNION`・`INTERSECT`・`EXCEPT`・`RIGHT`・`FULL`・`CROSS`・`NATURAL`・`OUTER`を裸の別名として読まない。
未対応の構文は明示エラーにする。集合演算は文の区切り規則へ通す。

### 3.14 結合アルゴリズムは走査順を検証する

マージ結合は左右の走査が結合キーで昇順に並ぶ場合だけ選ぶ。
結合キーが走査の先頭列に一致し、左右の列順も一致しない場合は選ばない。
ハッシュ・索引結合は等値列の型が一致する場合だけ使う。

### 3.15 JOIN を並べ替えても `*` は論理スキーマで展開する

結合順の並べ替えで行の物理レイアウトは変わるが、`SELECT *` の列順は `FROM` 順でなければならない。
`Ops.project` は展開用スキーマ（`inSchema` = FROM 順）と評価用スキーマ（`outputOf` = 実際の行レイアウト）を分けて受け取る。
両者を取り違えるとヘッダと値がずれ、`INSERT ... SELECT *` では誤った列へ書き込む。

### 3.16 ハッシュ結合の build 側とパーティション

`INNER JOIN` のハッシュ結合は推定行数の小さい側を build する。
左を build した場合に左が退避したら、右を build する通常経路へフォールバックする。
`LEFT JOIN` は左行を保存するため常に右を build する。
読み終えたパーティションの run はその場で削除する。
早期終了で読み切れなかった分は `ScanScope` が閉じて消す。

### 3.17 数値リテラルの直後の識別子は字句エラー

`0x1F` が `0 AS x1F`、`1_000` が `1 AS _000` として SELECT 項目で静かに通っていた。
数値の直後に識別子が続く入力は字句の段階で `SyntaxError` にする。
回帰: `Lexer: 数値の直後に識別子が続く入力はエラー`。

### 3.18 IF EXISTS は文種で綴りを固定する

`IF [NOT] EXISTS` の `NOT` を省略可能にすると、`DROP TABLE IF NOT EXISTS` が
`IF EXISTS` と同じ意味になり、テーブルを削除してしまう。
`CREATE` は `IF NOT EXISTS`、`DROP` は `IF EXISTS` だけを受理し、逆の綴りは `SyntaxError` にする。
回帰: `Parser: IF EXISTS と IF NOT EXISTS を取り違えない` /
`SqlReference: CREATE TABLE IF EXISTS は エラー（IF NOT EXISTS を使う）`。

---

## 4. LSM・WAL・MANIFEST

### 4.1 ポイント検索にrangeScanを使わない

ポイント検索は`Memtable.getVisible`かストレージの lookupを使う。
`rangeScan`は範囲走査であり、10万行規模ではポイント検索より桁違いに遅くなる。

### 4.2 下限とファイル枝刈りは別処理

SSTableのファイル選択でLargestを確認するだけでなく、読み出し側でもstartKeyより前をスキップする。
再オープン後はmemtableが空になるため、startKeyの欠落が一度に顕在化する。

### 4.3 prefix検索は先頭一致だけで決めない

走査の先頭がprefix外のことがある。先頭エントリを取得したら`startsWithPrefix`を検証する。

### 4.4 ファイル番号は巻き戻さない

新規/再オープン時はディスク上の最大番号より大きい`NextFileNumber`を設定する。
MANIFESTに反映した後に出力ファイルを削除し、削除の前にMANIFESTへ反映する。
番号の巻き戻しは既存SSTableの上書きとデータ損失につながる。

### 4.5 全レベルをポイント検索対象にする

`filesForPointLookup`はL0だけでなく全レベルを見る。
レベルごとに順位を揃え、同レベルではファイル番号の降順で並べる。
L0は範囲内の全ファイルを番号降順に返し、L1以降は`Smallest`の二分探索で1ファイルに絞る。

### 4.6 Footerサイズとレイアウトは同期する

Footerフィールドを追加・変更したら`FooterSize`、解析処理、チェックサム型を同時に更新する。
開始位置やサイズがずれると再オープン時に壊れる。

### 4.7 走査の進行状態は必ず返す

SSTableのk-way mergeは、開いたファイルをヒープへ追加した後、残りのpendingファイルも更新して返す。
heapだけを返すと同じファイルを毎行再オープンし無限ループになる。

### 4.8 WALはフレーム単位で追記する

Begin・Batch・Commitのフレームを結合して一度に書かず、フレームごとにWriteして最後に一度fsyncする。
commit済みprefixだけを再生し、不完全な末尾は捨てる。
open時は`Wal.foldCommitted`が返す最後の完全なフレーム位置までファイルを切り詰める。
追記は`FileMode.Append`ではなく`WalState.Size`へ`SetLength`/`Seek`してから行い、
不完全な末尾を残したまま次のcommitを書かない。
flush後のrotateは新しいWALを作成してから旧WALを削除する。
作成失敗時に旧WALを残す。
flush失敗は`StorageRuntime.LastFlushError`に残し、次のwriteで再試行する。

### 4.9 複数keyspaceのcommitは1本のWALバッチ

テーブルと全索引の更新は1つの`Entry array`へまとめ、`StorageEngine.writeBatch`を1回だけ呼ぶ。
テーブルだけ先にcommitされたり、索引だけ失敗したりしない状態を作る。

### 4.10 MANIFEST破損は空のVersionとして扱わない

途中のレコード破損は`CorruptionError`。末尾の書きかけだけを許容する。
末尾の書きかけを検出したら、復元できたVersionを新しいMANIFESTへ移して`CURRENT`を切り替える。
ローテーションは追記の前に判定し、追記失敗時は追記前の長さへ切り詰める。
`CURRENT`やMANIFESTがないのにSSTableが残っている場合もエラー。
孤児は削除せず`.orphans/`へ退避する。

### 4.11 SSTable検証は構造比較ではなくファイルの順序規則で行う

並びはキー昇順・同キーでSeqNo降順。レコードのF#構造比較を使うと正しいファイルも壊れたと判定する。

### 4.12 壊れたブロック・キー・メタデータは例外ではなくCorruptionError

境界で長さ、範囲、チェックサム、エスケープ、DECIMAL、レベル番号を検証する。
未捕捉例外や空の成功を作らない。

### 4.13 compaction出力はバイト上限とキー境界で分割する

件数やファイル数をエントリ数とみなして分割しない。
目標バイト数に達したときだけ、キーグループの境界で切る。

### 4.14 compactionのinstallは現在の状態を再検証する

MANIFESTへ反映する前に、削除対象ファイルが現在のVersionに存在するか、古いsnapshotが必要な版を守っているかを再確認する。
ファイル番号は同じCAS採番経路だけで進める。
`Manifest.logAndApplyWithOutcome`は`Applied` / `NotApplied` / `Unknown`を返す。
`NotApplied`は出力を削除してよく、`Unknown`はすぐには削除も退避もしない。
`Unknown`のときは`Manifest.openOrCreate`でMANIFESTを読み直し、
期待するファイル集合と一致すればAppliedとして公開し、
一致しなければNotAppliedとして出力を削除する。
読み直し自体が失敗した場合だけ、次回openの`quarantineOrphans`に任せる。
同期のflush/compactionは失敗時に次のファイル番号を進めたengineを返し、参照中SSTableを上書きしない。

### 4.15 maybeFlushの結果をcompaction失敗で巻き戻さない

flush成功後にcompactionが失敗したら、flush後のengineを返す。
flush失敗時だけpre-flush engineを返す。
WAL commit成功後に`Error`を返さない。

### 4.16 compactionの停止でも入力の走査を閉じる

`pump`は停止時に途中のmergeを持ち帰らない。
`runPump`の`finally`で`SstMerge.dispose`し、出力の削除と入力ハンドルの解放を両方行う。
片方だけだとWindowsで削除・renameが共有違反になる。
停止確認は出力ファイル境界だけでなく、同一キーグループを畳む`gather`の各ステップでも行う。
1回のブロック読み取り中だけは停止できない。

### 4.17 MANIFESTが参照するSSTableの欠落はCorruptionError

`ensureFilesPresent`は、MANIFESTが参照するSSTableが実在しなければ`CorruptionError`にする。
欠落をVersionから黙って除くと、バックアップの誤りや削除事故を正常な空DBとして扱ってしまうため。
`.orphans/`へ退避するのはMANIFESTが参照しないファイルだけにする。

### 4.18 ローテーション後の失敗でもMANIFEST番号を進める

`logAndApplyWithOutcome`のローテーションは`CURRENT`を切り替え、旧MANIFESTを削除してから`Applied`を返す。
その後のWAL切り替えが失敗したときに旧MANIFEST番号を返すと、次のflushが`CURRENT`の指さない番号へ追記し、
再オープンでそのSSTableがVersionから消える（データ損失）。
`Applied`の後で続きが失敗したら、**新しい番号と反映済みVersionを持ったengine**を返す。
さらに`Manifest.append`は対象ファイルが無いとき、`OpenOrCreate`で空ファイルを作り直さず`NotApplied`にする。
回帰テスト: `StorageEngine.maybeFlush: MANIFEST ローテーション後の WAL 失敗でも番号を進めて次の flush を失わない`。

### 4.19 走査に結び付けた資源は継続にも後始末を付ける

`SstScan`の遅延走査は、`next`が返す**続き（continuation）を捨てる**のが実際の破棄経路になる。
最初の値だけを`Dispose`するラッパー（`ofStepWithClose`で包むだけの形）では、
2件目以降の続きを捨てたときにVersion pinのleaseが残る。
`withResource`は各続きを包み直し、自然終端・破棄・エラーのどこで止まっても1回だけ解放する（`ScanScope.Release`が冪等）。
回帰テスト: `background compaction: 途中で捨てた走査の Version pin も解放される`。

---

## 5. MVCC・transaction

### 5.1 putとdeleteには異なるSeqNoを使う

同一キー・同一SeqNoのputとtombstoneでは、SeqNo降順の規則によりtombstoneが負ける。
書き込みごとに現在の`NextSeqNo`から単調増加させる。

### 5.2 再オープン時は最大SeqNoを復元する

`NextSeqNo`とLastCommitSeqNoをディスク上の最大SeqNoから復元する。
復元しないと既存行がすべて「未来」になり0行になる。

### 5.3 snapshotに必要な版をGCしない

読み手のsnapshotは`SeqNo <= snapshot`を見る。
activeな最古snapshotより古い版だけをGC対象にする。
tombstoneは古い版を残している間は消さない。
ファイルの回収は参照されなくなったファイルだけを対象にする。
`ActiveSnapshot = 0`は履歴を残す状態。

### 5.4 putとdeleteは最終状態へ正規化する

transactionの同じキーはputとdeleteを別々のslotとして残さず、最後の書き込みだけを保持する。
主キー変更はdelete/insertとして扱い、索引も主キーと全索引へ更新する。

### 5.5 同じTransactionManagerを複数作らない

`create`を複数回呼ぶとSeqNoを別々に1から始める。
`beginTxn`で返されたmanagerを次のbeginへ連鎖させる。
公開APIでは利用者が状態を持たず、`Session`が管理する。

### 5.6 commit後のLastCommittedと競合検査を掃除する

生きている最古snapshotより古いLastCommittedを捨てる。
active transactionがない場合は`LastCommitSeqNo + 1`を境界にする。
このcommit自身のSeqNoを反映した後の境界で掃除する。

### 5.7 UNIQUEの同時重複を論理予約で防ぐ

物理索引キーは主キーまで含むため、同じUNIQUE値の別主キー同士を競合検出できない。
transactionに`@unique:<index>:<encoded value>`の論理予約キーを持たせ、commit時の競合検査へ含める。
予約キーはWAL/SSTableへ書かない。

### 5.8 明示transactionは接続ごとに保持する

接続ごとの状態はBEGIN時点のCatalog・Active・Pending・leaseだけを保持し、
StoreとManagerは共有hostの最新committed stateを使う。
文ごとにtransactionを最新stateへrebaseする。
切断時はその接続のtransactionだけをrollbackする。

### 5.9 DDLは明示transactionに含めない

DDLと索引backfillは同一transactionへ載せられないため、`BEGIN`中のDDLは拒否する。
CREATE INDEXは排他ロックを取り、backfill成功後にカタログへ登録する。
backfill中の他の書き込みを防ぐ。

### 5.10 採番はengineの高水位より後ろで行う

カタログの保存はTransactionを介さずengineの`NextSeqNo`から採番する。
失敗したDDLの後始末（`CREATE UNIQUE INDEX`の巻き戻しなど）もManagerを経由せずengineへ書く。
書き込みの前に`TransactionManager.synchronize`でManagerをengineの高水位へ合わせる。
合わせないと次の採番が過去のSeqNoを再利用し、同じキーに同じSeqNoの版ができて可視性が壊れる。
回帰: `DatabaseHost: DDL のカタログ保存後も Manager の採番は保存済み SeqNo の後ろになる` /
`Session: UNIQUE 索引の backfill は重複値を拒否する`。

---

## 6. 索引・結合・クエリ意味論

### 6.1 索引は先頭から連続する等値列で選ぶ

索引選択は索引の先頭から連続する等値列で行う。
次の列に範囲条件があれば `IndexRangeScan` を優先する。

### 6.2 Catalogと実行スキーマを同期する

`TableSchema.Indexes`へ索引schemaを載せる。
載せないと実行層が索引を見つけられず、実行時に失敗するか空になる。

### 6.3 CREATE INDEXにはbackfillと再試行を含める

既存行をバッチでindex keyspaceへ書き込む。
失敗時もカタログへ載せず、再実行時に残骸を消してやり直せるようにする。

### 6.4 索引走査にはTableSchemaとIndexSchemaが必要

索引キーには索引列と主キー列を含むため、片方のスキーマだけでは復号・構築できない。

### 6.5 DELETEは主キーと全索引へtombstoneを書く

索引側の削除を忘れると削除済み行が索引から復活する。

### 6.6 DROPはカタログとkeyspaceを安全な順序で消す

DROP後始末はカタログ削除前にジャーナルへ記録し、一定件数ずつtombstoneを消す。
再オープン時にジャーナルを読み、未完了の削除を再開する。

### 6.7 集約・射影の式を正しく配置する

- 集約引数の式は集約前に評価する。
- `AggCall`の第2引数はDISTINCTフラグ。
- `COUNT(*)`はNULLを数え、`COUNT(列)`はNULLを数えない。
- HAVINGにしか現れない集約も計画時に集める。
- GROUP BY式は木全体を集約スロットへ置換する。

### 6.8 3値論理を維持する

`IN`候補が一致しなければFALSE。NULL比較が一つでもあり一致がなければUNKNOWN。
失敗や不足を0/falseに変換しない。

### 6.9 LIKEは進捗を保証する

バックトラックでstar位置やtext位置が進まなくなると無限ループになる。
状態遷移ごとに必ず位置を進めるか、失敗を返す。

### 6.10 CASTと暗黙変換を分ける

INSERT/UPDATEの暗黙変換は`SqlValue.coerce`。
`CAST`は明示変換で`SqlValue.cast`を使う。
暗黙変換の受理範囲を広げない。

### 6.11 TEXT・REAL・NULLの比較を全順序で扱う

- TEXTはUTF-8/Unicodeコードポイント順。
- REALの-0.0は+0.0へ正規化。
- ORDER BYの比較ではNULLを昇順先頭・降順末尾に置く全順序を使う。
- WHEREの比較は3値論理を維持する。

### 6.12 マージ結合は連言の位置でキーを対応させる

左右を列名で独立に並べ替えると、列名が同じでも `ON a.id = b.k AND a.k = b.id` でペアが反転し、
正しい 3 行が 0 行になる。
並べ替えた左の順に右の列が連言の位置どおり対応するかを検証し、崩れるときは `HashJoin` へフォールバックする。
回帰: `Analyzer.chooseJoinAlgorithm: クロス名の等値はマージ結合にしない` /
`SqlReference: クロス名の等値結合は行を取りこぼさない`。

### 6.13 真偽の解釈は1か所で行う

`WHERE`、`NOT`、`CASE WHEN`、`IS UNKNOWN` で非 `BOOLEAN` の扱いが違うと、
`WHERE 5` と `WHERE NOT 5` が同時に真になる。
`Eval.truthOfValue`（`BOOLEAN` はそのまま、`INTEGER` は `0 = FALSE`・非 `0 = TRUE`、他は `UNKNOWN`）に一本化する。
回帰: `Eval.eval: 真偽の解釈は WHERE / NOT / CASE / IS UNKNOWN で一致する` /
`SqlReference: WHERE の真偽と NOT / CASE WHEN / IS UNKNOWN が一致する`。

---

## 7. メモリ・退避・列読み込み

### 7.1 メモリ上限は管理メモリで測る

memtable・Join・DISTINCT・GROUP BYの上限は、Treapノード、配列ヘッダ、UTF-16、参照型を含む管理メモリで計算する。
SSTableのファイルサイズ見積りをメモリ上限へ流用しない。

### 7.2 作業上限を超えたら退避する

右側を保持する結合、DISTINCT、GROUP BY、外部ソートは、上限までメモリに保持し、超過分をrunへ退避する。
退避後の走査も終了・エラー・閉じ忘れがないようにする。

### 7.3 退避runはone-shot

`reopen`した`Op`は、読み直し用に前の状態へ戻せない。
読みかけの行をpendingとして次の状態へ渡す。

### 7.4 外部ソートのrunは一度に全部openしない

最終k-way mergeは同時に開くrun数を上限以下にする。
上限を超えたら複数パスで中間runへまとめる。

### 7.5 格納する配列を複製する

memtable・transaction・内部バッファへ渡すキー・行・BLOBは複製して所有する。
呼び出し側が配列を書き換えても内部状態が壊れないようにする。

### 7.6 結果へ返す配列も複製する

`StatementResult.Rows`と`RowReader`は内部配列とは別物を返す。
`SqlValue.copyRow`で行とBLOBを複製する。

### 7.7 列マスクの`Unloaded`をNULLと混同しない

列マスクで復号しなかった列は`SqlValue.Unloaded`。
SQLのNULLではない。比較・ソート・DISTINCT・符号化・型問い合わせへ渡さない。
最終結果境界でロード済み行へ変換する。

### 7.8 必要な列だけを復号する

SELECT定数、`COUNT(*)`、DELETE、INDEX lookupは参照列だけ復号する。
全列を復号しない。
非被覆索引は主キーで本体をfetchし、`fetch`を全行へ適用する。

範囲走査の上限は`opOfScan`の打ち切りで担保する。
返した行を読み直して主キーを再符号化するfilterを足さない（主キーの復号も強制される）。
回帰: `Dml.scanByPrimaryKeyRange: 上限を超える行と範囲外の書き込みは結果に混ざらない`。

### 7.9 compactionはraw値スライスを使う

値を復号せずブロックバッファのスライスを保持し、出力へコピーする。
スライスの間にバッファを再利用しない。
削除判定は`SstEntry.IsDelete`で行う。

### 7.10 結果型の割当をホットパスへ持ち込まない

ホットパスでは`DbResult`と`OpStep`を使い、公開APIの境界で`Result`へ変換する。
`Op.emit`の第2引数は遅延させる。即時再帰するとスタックオーバーフローになる。

### 7.11 DISTINCT 集約の退避は最後の Seen も run に含める

`finalizeAcc` が `SeenRuns` だけを畳むと、最後のスピル以降に `Seen` へ溜まった値が消える。
残っていれば先に run へ書き出してから全 run を畳み直す（実測: 100個が 65個になっていた）。
回帰: `集約: 最後のスピル以降に溜まった DISTINCT 値も結果に含める`。

### 7.12 退避した値集合の Bloom キーは SQL 等値の全表現を網羅する

`ValueSet` の Bloom は符号化バイトで照合するが、SQL の等値は型・DECIMAL のスケール・DATE/TIMESTAMP を跨ぐ。
1表現に固定すると `1 IN (SELECT DECIMAL列)` が退避時だけ 0行になる。
`ValueKey` が等値になり得る表現（数値の Int/Decimal/Real、DATE と同日 00:00 UTC の TIMESTAMP）を
すべてキーにする。
回帰: `ValueSet: 退避しても型・スケール違いの等値を取りこぼさない` /
`ValueSet: 退避しても DATE と同日 00:00 UTC の TIMESTAMP を等値に扱う`。

---

## 8. 並行実行・接続・通信

### 8.1 DBの所有者はDatabaseHostに集約する

ストアを開くのはhostだけ。接続ごとにRowStoreを開くと二重オープンやcommit非可視になる。
通常のSessionとTCPのセッションはhostを共有し、ownerだけが閉じる。

### 8.2 二重オープンはロックと台帳で防ぐ

プロセス間は`.rykdb.format`を`FileShare.None`でロックする。
同一プロセスの二重オープンは正規化rootの台帳で拒否する。
オープン途中の失敗でもロックを解放する。

### 8.3 Session.Closeは実行中の読み取りを待つ

closed状態を先に確定し、`readers`が0になるまで待ってからrollback/closeする。
読み取りはローカルtransactionを書き戻さない。

### 8.4 read snapshotとファイルpinは一度に取得する

committed stateを取得してから別にpinすると、pinとGCの間に必要な版が消える。
`AcquireReadSnapshot`でstateと全ファイルのleaseを一度に取得する。

### 8.5 TCP切断は接続ごとのtransactionだけrollbackする

切断検知とrollbackは非同期になるため、テストでは完了を待つ。
サーバのSessionを接続間で共有しない。

### 8.6 TCPプロトコルのレスポンス形を区別する

列がある`OK n`はn行のSELECT。列がない`OK n`はDML/DDLのaffected件数。
STREAMは`END`または`ERR`まで読み切る。

### 8.7 background compactionは専用スレッドで実行する

`TaskCreationOptions.LongRunning`で起こす。
スレッドプールの空きに依存させない。

### 8.8 compactionの状態は1つの値で管理する

workerの起動状態と Idle / Busy / Pending の activity を `WorkerState` の1セルで管理する。
busyとpendingを別に読むとidle判定が完了前にtrueになる。
shutdownではpermitに関係なくsignalを発行する。

### 8.9 走査のハンドルは発生源でScopeへ登録する

ラップではなく、走査を作った直後に`ScanScope`へ登録する。
continuationを更新し、終端・エラー・Disposeで登録を外す。
文の終端で`ScanScope.DisposeAll()`を呼ぶ。
`clearSpaceChunks` / `clearKeyspaceChunks`のようにチャンク単位で走査し直す処理も、
取得したチャンクのcontinuationを必ず`SstScan.dispose`する。

### 8.10 group commitの失敗はSession全体をfatalにする

fsync失敗を待機者だけに返さず、失敗位置とエラーを共有する。
以後のcommitも同じfsyncエラーを返す。

### 8.11 fsync位置はwriteGate内で登録する

`WalState`を`CommitDurability.Register`した後にgate外で`Await`する。
登録漏れはcommitごとのfsyncを再発させる。

### 8.12 commit中のファイルをworkerが削除しない

Session単位のread leaseでSSTableをpinする。
workerはretiredとしてrefcountが0になった後に削除する。
closeはworker join後にWALとロックを解放する。

### 8.13 compactionの失敗は次のwriteで再試行する

`VersionCoordinator.LastCompactionError`で直近の失敗を観測できる。
失敗後は次のwriteでもworkerへsignalを送る（flushが起きなくても再試行する）。
明示的な再試行には`Signal`を使う。
失敗が続いてもMANIFESTの状態とファイル番号は巻き戻さない。

### 8.14 Sessionの境界で例外をInternalErrorにする

想定外の例外が `readers` や lease の解放を跨ぐと、`Session.Close` が永久に待つ。
`executeStatementsCore` が例外を `InternalError` へ変換し、
`TryOpenReader` は lease を `finally` で返す。
通常経路では例外は起きないため、既存の Session 並行・Close テストが退行の検出を担う。

---

## 9. F# とビルド

### 9.1 主コンストラクタで副作用を単独実行しない

副作用は引数で初期化するか、型の中で確定してから公開する。
`use`や接続処理を主コンストラクタ本体へ単独で置かない。

### 9.2 共用体ケース名とレコードフィールド名を衝突させない

`Result.Ok`を独自型のケース名として使わない。
一般名のフィールドや引数により型推論が変わるため、用途の明確な名前へする。
レコード型を追加したら、既存の同名フィールドをgrepする。

### 9.3 namespace直下やmoduleスコープに不正な束縛を置かない

namespace直下に値を置かずmoduleへ入れる。
モジュールスコープにDUを`let`束縛で置かない。`type`宣言を使う。
`let private rec`ではなく`let rec private`を書く。

### 9.4 型注釈を省かない箇所を把握する

型推論はレコード追加や関数名の衝突で静かに変わることがある。
特に配列を受けるラムダ、レコードを受ける関数、CPSグループ、オーバーロードを持つ文字列処理は型注釈を付ける。

### 9.5 数値と予約語を取り違えない

`default`・`value`・`old`・`type`・`base`・`key`・`begin`・`new`は識別子に使えない場合がある。
意味の異なる番号はunits of measureで区別する。

### 9.6 `f(x = y, msg)`は比較式ではなく名前付き引数になる

比較式を二重括弧で囲む。

### 9.7 privateはアセンブリ内では隠蔽しない

同じアセンブリの別ファイルからは見える場合がある。
生ケース・レコードフィールドを隠すには`.fsi`を使う。

### 9.8 `[<TailCall>]`は末尾位置でだけ付ける

自己再帰が`>>=`・`map`・引数位置・ラムダ内の分岐に残る場合は付けないか、末尾再帰へ分解する。
ローカル`let rec`には付けられないため、モジュールレベルへhoistする。

### 9.9 Tailcallsを有効に保つ

`Directory.Build.props`の`<Tailcalls>true</Tailcalls>`を外さない。
式・サブクエリ・Parserは深さ上限なしのCPSで、テストでスタックを検証する。

### 9.10 CPSグループの型を固定しない

相互再帰グループの各関数は具体型ではなく汎用の継続型を保つ。
リスト走査や相関実行が型変数を固定すると、グループ全体が型推論に失敗する。

### 9.11 struct DUのフィールド名と配置に注意する

struct DUのケースにはフィールド名を付ける。
`Unchecked.defaultof`で構築しない。コレクション要素や参照型化の対象にしない。
`Kont`など参照DUのサイズと割当はベンチで確認する。

### 9.12 部分関数を公開境界へ出さない

`Array.reduce`・`Array.take`・`.Value`・`List.last`・`List.head`・`Array.forall2`などの例外経路を避ける。
空入力と範囲外を先に検査し、型で表せない不変条件は`InternalError`/`CorruptionError`へ変換する。

### 9.13 build/testは並行実行しない

MSBuildのincremental stateとファイルロックが競合し、ソースが正しいのに幻影エラーになる。

### 9.14 SDK更新時はTailAuditを個別にビルドする

TailAuditはソリューション外。FCS APIの型変更はソリューションのビルドでは検出されない。
SDK更新後は`dotnet build tools/TailAudit/TailAudit.fsproj -t:Rebuild`を実行する。

### 9.15 FSharp.Coreの復元元を固定する

`DisableImplicitLibraryPacksFolder=true`でnuget.orgへ固定する。
ロック更新時は古いFSharp.Coreパッケージキャッシュを消して`dotnet restore --force-evaluate`を実行する。

### 9.16 レコードのフィールド名はメンバー解決を横取りする

`Ast.fs`の`Membership`は`Contains: SqlValue -> DbResult<TruthValue>`というフィールドを持つ。
この状態で`Set<int>`に`xs.Contains`と書くと、F#はレコードフィールド`Membership.Contains`を優先し、
`xs`を`Membership`と推論して型エラーになる
（`List.exists`に`SqlValue -> DbResult<TruthValue>`を渡した、という分かりにくいメッセージになる）。
集合・辞書では`Set.contains p xs`のようにモジュール関数で書く。

---

## 10. テストとデバッグ

### 10.1 `[<Fact>]`には関数形を使う

値束縛に`[<Fact>]`を付けても無視される。
`let ``説明`` () = ...`とする。

### 10.2 テストディレクトリは明示的に作る

`withDir`ではディレクトリが作られないので、`Directory.CreateDirectory`を呼ぶ。
`try/finally`で削除する。

### 10.3 Assertの型を揃える

`list.Length`は`int`。`int64`として比較しない。

### 10.4 SIGABRTは件数ではなくスタックを疑う

末尾再帰・`>>=`・CPS・deep ASTの経路を確認する。
`dotnet test --logger "console;verbosity=detailed"`で切り分ける。

### 10.5 ガードは一度わざと失敗させて検証する

禁止事項や`.fsi`の検査は違反を組み込んで落ちることを確認する。
常に緑になるガードは機能していない可能性がある。

### 10.6 actions/setup-dotnetのcacheはglobを明示する

lockファイルが複数プロジェクトに分散する場合は`cache-dependency-path: "**/packages.lock.json"`を指定する。

### 10.7 WindowsのファイルハンドルをLinuxでも再現する

SSTableの走査は終端・エラー・Disposeで閉じる。
`ofStepWithClose`を使わないか、close漏れを発生させるテストを用意する。

### 10.8 DLLを直接叩くときは参照先を揃える

FSIでの相対パス参照を確認し、コピーしたDLLを参照しない。

### 10.9 ファイルを整形したらリビルドする

Fantomas後は`dotnet build -t:Rebuild`を必ず実行する。
FS0020など整形後の警告を検出する。

### 10.10 スピルの残留ファイルは全体テストを落とす

`ExtSortTests`の一部は一時ディレクトリ全体を見て「一時ファイルが残っていない」ことを検査する。
テストホストが途中でクラッシュして`/tmp/rykdb-sort-*`が残ると、関係のない回帰が落ちる。
先に`rm -f /tmp/rykdb-sort-*`で掃除してから再実行する。

---

## 11. 実装時チェックリスト

変更の種類ごとに、最低限次を確認する。

### 11.1 Pull演算子・再帰

- [ ] `Step`の第2引数を遅延せず構築していないか
- [ ] 継続を別ループへ飛ばしていないか
- [ ] 再帰に`>>=`や`Result.map`を載せていないか
- [ ] 可変状態をモジュールスコープで共有していないか
- [ ] 式評価エラーを`Op.Empty`へ変換していないか
- [ ] `Op.next cur`の次は`nxt`を使っているか
- [ ] 退避runはone-shotとして扱っているか

### 11.2 計画・実行

- [ ] 下へ渡すスキーマは入力スキーマか
- [ ] JOINの右部分木とそのpushdown Filterを実行しているか
- [ ] INLの右スキャンで吸収済み述語を落としていないか
- [ ] JOINを並べ替えても`SELECT *`の列順がFROM順か
- [ ] 入れ子JOINへ実際の左出力スキーマを渡しているか
- [ ] 集約ノードをラップを降りて探しているか
- [ ] HAVINGにだけある集約を集めているか
- [ ] GROUP BY式を木全体で置換しているか
- [ ] `ORDER BY`序数を計画時に解決しているか
- [ ] DISTINCTは射影の後に適用しているか
- [ ] 相関サブクエリを外側行ごとに再計画しているか
- [ ] 未対応語を別名へ吸収していないか
- [ ] 列解決の失敗を演算子の構築時に検査しているか（0行でも`NameError`）
- [ ] テーブル名・別名の重複を大小文字を区別せず検出しているか
- [ ] 見積りの作業リストは「子を訪問してから後置処理」の順に積んでいるか（`Visit child :: Apply ...`）

見積りの作業リストは**子を先に処理する**。
`ApplyFilter` / `ApplyJoin` を先頭に積むと、子の値がまだ無いまま後置処理が走り、
Filterの1/10やJoinの選択性が無視される（統計テストでJoinが左の値のままになった）。
正しくは `Visit child :: ApplyFilter :: rest` / `Visit left :: Visit right :: ApplyJoin ... :: rest`。

列解決の失敗は演算子の構築時に検査する。
`Eval.prepare`の解決結果を評価時まで遅らせると、入力が0行のときに述語・射影が一度も評価されず、
未知の列が黙って0行になる（同じクエリが行数で成否の変わる状態になる）。
集約後の射影・HAVINGは、GROUP BY列と集約呼び出しを置き換えた後の式を検査する。

テーブル名・別名の重複は大小文字を区別せずに検出する。
`t`と`T`を別名として扱うと計画は通り、実行時の列解決だけが曖昧になる。

### 11.3 Storage・MVCC

- [ ] ポイント検索にrangeScanを使っていないか
- [ ] SSTable走査のstartKeyとファイル枝刈りを両方適用しているか
- [ ] ファイル番号を巻き戻していないか
- [ ] MANIFEST反映をファイル削除より先にしているか
- [ ] MANIFESTローテーション後の失敗経路でも新しい番号を返しているか
- [ ] 走査に結び付けた資源を、続きの破棄でも解放しているか
- [ ] SSTableのキー順とFooterを正しく検証しているか
- [ ] WALはフレーム単位で書き、commit済みprefixだけ再生するか
- [ ] 複数keyspaceを1回のWAL/batchへまとめているか
- [ ] 古いsnapshotとtombstoneをGCで消していないか
- [ ] 再オープン時にSeqNoを復元しているか
- [ ] UNIQUEの論理予約を競合検査へ含めているか

### 11.4 索引・JOIN・SQL意味論

- [ ] 索引の列名が一致するものだけ選んでいるか
- [ ] CatalogへIndexSchemaを載せているか
- [ ] CREATE INDEXはbackfill→カタログ登録の順か
- [ ] DELETEは主キーと全索引へtombstoneを書いているか
- [ ] DROPと再作成時に残骸を掃除しているか
- [ ] `IN`と`LIKE`が3値論理・進捗を守るか
- [ ] TEXT・REAL・NULLの比較規則を揃えているか
- [ ] ハッシュ結合のキーを`compareValuesV`と同じ等値（順序保持符号化）にしているか
- [ ] INNERハッシュ結合で小さい側をbuildし、LEFTは右をbuildしているか
- [ ] パーティションしたrunをキー順と仮定していないか
- [ ] 読み終えたパーティションのrunを逐次削除しているか
- [ ] INLで右部分木を実行しないとき、右の`Filter`と吸収済み述語をresidualに回しているか
- [ ] residualの列を右の列マスクに含めているか
- [ ] CASTと暗黙変換を分けているか
- [ ] 外部キーは参照先を親の主キーへ解決し、型と列順まで検証しているか
- [ ] 子の `INSERT` / `UPDATE` と親の `DELETE` を即時 RESTRICT で検査しているか
- [ ] `DELETE` の列マスクに、親として参照される列（主キー）が入っているか
- [ ] 参照されている親の `DROP TABLE` と、外部キー列の `DROP COLUMN` を拒否しているか
- [ ] `RENAME COLUMN` で子の列名と親側の参照列名を追随させているか
- [ ] カタログの外部キーは末尾追記で、古い定義（末尾なし）も読めるか
- [ ] 結合順のDPはINNER区間・先頭固定・テーブル数上限・依存充足・有限推定の条件を守っているか
- [ ] DPが使えないとき貪欲法へフォールバックするか
- [ ] `ANALYZE` の統計（行数・列NDV）を保存・再読込でき、`ALTER TABLE` で無効化しているか
- [ ] 0行のテーブルに Filter / Join を付けても 0行のまま見積もるか
- [ ] NDV が一部欠けても、既知の等値は使い残りを既定の 0.1 で穴埋めしているか
- [ ] 等値結合から NULL 率を除いているか
- [ ] `SAMPLE n` は行数を全体で数え、NDV・NULL率・範囲を標本から推定しているか
- [ ] `SAMPLE` の標本で NULL 数を全体行数へスケールしているか
- [ ] 数値ヒストグラムは標本値を昇順に保存し、範囲選択性の CDF に使っているか
- [ ] `ANALYZE` の進捗コールバックとキャンセルを長時間処理中に確認しているか
- [ ] DPのINL/マージ判定は索引先頭列・走査順・作業メモリを実際の実行条件と揃えているか
- [ ] 先頭テーブルを入れ替えても、ON述語を参照先が揃った結合へ昇格しているか
- [ ] ON述語の昇格で述語を落としていないか（FROM全順列の結果同値テスト）
- [ ] マージ結合のキーは連言の位置の対応まで検証しているか（クロス名等値でフォールバックするか）
- [ ] 真偽の解釈（WHERE / NOT / AND / OR / CASE / IS TRUE）を同じ関数に揃えているか
- [ ] 数値リテラルの直後の識別子を裸の別名として静かに解釈していないか
- [ ] `IF EXISTS` / `IF NOT EXISTS` の綴りを文種ごとに検査しているか

### 11.5 メモリ・並行・通信

- [ ] 管理メモリ上限とファイルサイズ見積りを混同していないか
- [ ] 上限超過時に退避runへ移行できるか
- [ ] 走査をcloseせずに捨てていないか
- [ ] 内部へ格納する配列を複製しているか
- [ ] 結果へ返す配列を内部配列と分離しているか
- [ ] `Unloaded`をNULLとして扱っていないか
- [ ] Snapshotとファイルpinを一度に取得しているか
- [ ] 接続ごとのtransactionだけrollbackしているか
- [ ] STREAMはENDまで読み切っているか
- [ ] fsync失敗を全Sessionへ伝播しているか
- [ ] 退避した値集合のBloomキーがSQL等値の全表現（数値の型・スケール、DATE/TIMESTAMP）を網羅しているか
- [ ] DISTINCT集約のfinalizeが最後の`Seen`もrunに含めているか
- [ ] Sessionの境界で想定外の例外を`InternalError`にし、leaseと`readers`を解放しているか

### 11.6 F#・テスト

- [ ] 同名レコード・ケースで型推論が変わっていないか
- [ ] `.fsi`で公開する表現を制限しているか
- [ ] `[<TailCall>]`と`<Tailcalls>true</Tailcalls>`を正しく使っているか
- [ ] build/testを並行実行していないか
- [ ] `[<Fact>]`の関数形を守っているか
- [ ] ガードは意図的な違反で一度失敗を確認したか
- [ ] Fantomas後にリビルドしているか
- [ ] TailAuditと回帰テストを単独実行しているか
