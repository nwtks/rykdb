# コーディング規約

RykDb（**.NET 10 / F# 10**）を書くときの規約。
設計の全体像は [`architecture.md`](architecture.md)、
実際に踏んだ落とし穴は [`gotchas.md`](gotchas.md) を参照。

---

## 目次

1. [例外を使わない](#1-例外を使わない)
2. [命令型の構文・機能を使わない](#2-命令型の構文機能を使わない)
3. [末尾再帰を前提にする](#3-末尾再帰を前提にする)
4. [予約語に注意](#4-予約語に注意)
5. [クロスプラットフォームにする](#5-クロスプラットフォームにする)
6. [意味の違う番号と所有権を守る](#6-意味の違う番号と所有権を守る)

---

## 1. 例外を使わない

**このプロジェクトは例外を制御フローに使わない**。
失敗は必ず `Result<'T, DbError>` として返す。
`exception` 定義は **0件**。

合成に必要な関数は `Errors.fs` の `[<AutoOpen>] module ResultOps` にまとめている。

| 提供                       | 用途                                                 |
| -------------------------- | ---------------------------------------------------- |
| `( >>= )`                  | monadic な合成（フラットな bind）                    |
| `ret` `err` `all` `foldOk` | 単発・畳み込み                                       |
| `result { ... }`           | `DbResultBuilder` による計算式                       |
| `protect`                  | .NET I/O API 境界の例外捕捉（`try`/`with` の主経路） |

`all` と `foldOk` は**最初の `Error` で打ち切り**、残りの項目は評価しない。
複数の `Error` があっても返るのは最初の 1つ。

`protect` は `IOException` / `UnauthorizedAccessException` を `IoError`、
`InvalidDataException` / `FormatException` / `OverflowException` を `CorruptionError` に変換する。
ファイル内容の不整合を `InternalError`（バグの兆候）として扱わないため。
`try` / `with` は `protect` のほか、`Manifest.decodeRecord`（base64 の解析）と
`RowReader` の解放処理（`Session.fs`）で使い、
いずれも `DbError` に変換するか例外を飲み込んで、外へ出さない。
`try` / `finally` は `Engine` / `EngineHost` / `Sstable` / `Session` /
`ProcessLock` / `StorageEngine` / `Transactions` の解放処理に使う。

失敗は `ErrorKind`（`DbError.Kind`）で分類する。

| `ErrorKind`                                          | 意味                                     |
| ---------------------------------------------------- | ---------------------------------------- |
| `SyntaxError` / `NameError` / `TypeError`            | 字句・構文 / 名前解決 / 型の失敗         |
| `ConstraintViolation` / `NotFound` / `AlreadyExists` | DML / DDL の失敗                         |
| `CapacityError`                                      | memtable・バッファのメモリ上限           |
| `CorruptionError`                                    | チェックサム不一致などファイル内容の問題 |
| `ConflictError`                                      | MVCC の write-write 競合                 |
| `IoError` / `InternalError`                          | I/O 失敗 / 内部不整合（バグの兆候）      |

## 2. 命令型の構文・機能を使わない

`src/RykDb.Core/` では **`mutable` / `while` / `ref` を 0件**に保つ。
例外は公開セッション層の `Session.fs` だけで、次を意図的に使う。

- `DatabaseHost` の `let mutable committed` / `let mutable closed`:
  全接続で共有する committed な `DbState` と Close 状態。
- `Session` の `let mutable local` / `let mutable closed` / `let mutable readers`:
  接続ローカルの transaction 状態、Close 状態、実行中の読み取り数を保持する。
- `SessionPool` の `let mutable closed`: 新しい接続を受け付けるかを保持する。
- `RowReader` の `current` / `closed` / `exhausted`:
  ストリーミング SELECT の読み取り位置と解放状態を保持する。
  `Session.fs` 内に閉じる。
- `CommitDurability` の `syncedFile` / `syncedSize` / `pending` / `syncing` / `failure`:
  WAL の fsync 位置と進行状態（グループコミットの待機・失敗の共有）を保持する。
- `lock stateGate`: `Session` の `local` / `closed` / `readers` と
  `DatabaseHost` の `committed` / `closed` を保護する短いロック。
  読み取りは入口・出口でしか取らない。
- `lock writeGate`: `Execute`（書き込みを含むバッチ）/ `Begin` / `Commit` / `Rollback` / `Close` を直列化する。
  `DatabaseHost.writeGate` は DB 全体、`Session.writeGate` は接続内を守る。
- `Monitor.Wait` / `PulseAll`: `Session.Close` が実行中の読み取りの完了を待つために使う。

これらは `Session` / `SessionPool` / `DatabaseHost` の内部に閉じ込め、
`Database` を internal にして外部へ漏らさない。

`ResizeArray` / `Dictionary` / `HashSet` / `ConcurrentDictionary` を使うのは次の場所に限る
（CLI / Server は対象外）。
いずれもホットパスか同時実行の台帳であり、使う理由はコメントに残す。

- `Op.toArray` の行の蓄積
- `Ops.aggregate` のグループ表（入力行は抱えず、アキュムレータだけ）
- `Ops.distinct` の符号化済みキー集合（`HashSet<byte array>`）
- `Ops.innerJoinHash` / `leftJoinHash` の右側ハッシュ表
- `ExtSort` の受け皿（バジェット内に収まる分。超過分は一時ファイルへ退避する）
- `Sstable` のブロック構築（`ResizeArray`）と partial reader / scan scope のキャッシュ（`ConcurrentDictionary`）
- `EngineHost` の host state / ファイル refcount / 採番（`ConcurrentDictionary`）
- `Compaction` の作業ディレクトリ別カウンタ（`ConcurrentDictionary`）
- `StorageEngine` / `ProcessLock` / `Transactions` のキャッシュ・ロック・登録台帳（`ConcurrentDictionary`）
- `Eval` の LIKE パターンキャッシュ（コンパイル済みトークン列を `(escape, pattern)` ごとに再利用）

| やりたいこと   | 命令形の代わりに使うもの                                                       |
| -------------- | ------------------------------------------------------------------------------ |
| 書き込み       | `Writer` = 不変のチャンク列                                                    |
| 読み取り       | `Cursor` = 読み取り位置を値として保持する                                      |
| パーサの状態   | `PState`（読み取り位置を値として保持する）                                     |
| 走査・畳み込み | `Result` を返す末尾再帰                                                        |
| 平衡木の再構築 | 明示スタック（`list` を積む）                                                  |
| 遅延列挙       | `Op`（one-shot。構築は `Op.compute` / `Op.emit`、消費は `Op.next` の継続だけ） |

## 3. 末尾再帰を前提にする

末尾呼び出しとは、関数が最後の処理として別の関数を呼ぶ形である。
呼び出し元のスタックを残さずに済むため、要素数の多い処理では再帰呼び出しを末尾に置く。

`Directory.Build.props` で `<Tailcalls>true</Tailcalls>` を設定し、
Debug ビルドでも F# に `tail.` プレフィックスを出させる
（Debug の既定は `Tailcalls=false`、Release の既定は `true`）。
JIT は `tail.` を尊重する。
ただし、JIT の自己再帰ループ化だけでは、相互再帰やクロージャ経由の呼び出しでスタックが残る。
この設定により、CPS の継続呼び出しも実際の末尾呼び出しにする。
`[<TailCall>]` は構文上の末尾性を検査する属性であり、コード生成方法は切り替えない。

採用した手法は次のとおり。

- **構文規則は CPS（継続渡し）** — 引数を「読むもの」ではなく「読んだ後どうするか」に変える。
- **継続は名前付きの `and` グループメンバーへ hoist** する
  （属性付き関数でラムダを書くと FS3569 が出る）。
- **`Parser` の式グループでは `IN` 候補リスト・`CASE` の `WHEN` 節・関数引数リストを CPS にし、`parseExprC` の呼び出しを継続位置（末尾）へ移す**。
  回帰テストは `ParserTests` の `末尾再帰: 大量のトークンを解析できる`（50000個の `IN` リスト）。
- **CPS グループの全メンバーに `: Result<'r, DbError>` の戻り型を明示する**
  （`parseExprListToParen` / `parseInListBodyLoop` / SELECT 句の `parseSelectItemsC` / `Plan` の `resolveExprC` など）。
  型引数 `<'r>` だけだと、群内の相互参照経由で戻り値の型が別の推論変数 'r0 になり
  FS0043「'r と 'r0 が一致しない」で**群全体が型検査に失敗する**
  （失敗すると後続の `parseExprC` 参照が FS0039「定義されていません」になり、原因が分かりにくい）。
- **「葉の書き換えが別の再帰を経る」式走査は `Expr.rewriteWithC`（継続を取る版）を使う**。
  `Plan` のサブクエリ解決（`resolveExprC` → `resolveLeafC` → `planSelectCWith` → …）がこれ。
  `Expr.rewriteWith` はその糖で、失敗を返す葉の書き換え（束縛・集約の読み替え）に使う。
- **継続を値として渡す呼び出しは、TailAudit の引数位置の判定では非末尾と判定される**
  （`parseExprC (parseExprListElemC k acc) st`・`parseCaseBranches (parseCaseTailC k) [] st`）。
  部分適用は関数値を作るだけで、実呼び出しは呼ばれた側（`parseExprC` など）の末尾位置なのでフレームは積まれない。
  ベースラインで追認する（TailAudit の `[lambda]` 判定と同じ保守側の扱い）。
- **実行時のスタック増加は `PState.Depth` で数えて `MaxExpressionDepth = 200` で止める**。
  `tail.` が効かない環境（設定漏れ・JIT 非対応）でも暴走しないための保険。
- **`result { }` 計算式はループする関数では使わない**。
  `Delay` でクロージャに包まれるため末尾最適化が効かず、500000トークンで Lexer が `StackOverflow` した実測がある。
- **左偏ヒープのマージは明示スタックで畳む**（`SstMerge`、`Sstable.fs`）。
  再帰の結果を `node` の引数にする形は非末尾になる。
  右スパインを `picked` リストへ積み、`List.fold` で組み直せば自己再帰が消える
  （左偏性により右スパインは log オーダーだが、深度保証に頼らず監査を通す）。
  ヒープは `SstMerge` に集約し、`StorageEngine.scanAt` と `Compaction` のストリーミング merge が共有する。
  回帰テストは `SstScanTests` の `SstMerge: 多数のソースをまとめてマージできる`、
  `StorageEngineTests` の `StorageEngine.scanAt: 多数のファイルをキー昇順にマージする`、
  `CompactionTests` の `Compaction: 多数の入力ファイルをストリーミングでマージする`。
- **`[<TailCall>]` は「その関数本体の自己再帰が全部末尾位置」のときだけ付ける**。
  1つでも外れると FS3569 になり、`TreatWarningsAsErrors` のせいでビルドエラーになる。
  構造上末尾にできないものは**付けない理由を呼び出しの直前にコメントで残す**:
  - `Op.map` / `Op.take` — 再帰が `Compute` の中にある。
    次の 1行を取り出すときに 1段ずつ評価されるので、フレームは積み上がらない。
    同じパターンが `Sstable.rangeScanPartial` の `scanEntries` / `loadBlock`
    （ブロック読みを `SstScan.next` まで遅らせるため、続きの組み立てがラムダ内に入る）と `SstScan.emit` にもある。
  - `Plan.runSubquery` — 相関サブクエリの再計画用に自分自身を runner として渡す。
    実際の呼び出しは `Run` の末尾位置（`Result.bind` の継続）で起きるが、
    TailAudit は関数値を引数として渡す形を `bare` として非末尾に数えるため、
    `StorageEngine.scanAt` の `pull` と同様にベースラインで追認する。
  - 逆に**要素数に比例する反復は `match` に直して末尾再帰にする**
    （`IN` の候補リストと `CASE` の WHEN 節。`eval ... >>= fun` だと候補の数だけスタックが積まれる）。
    カーソルから件数分を読み出す処理（`Catalog` の列定義・名前、`Memtable` のエントリ）は
    `Reader.readItems`（Serialization.fs）に集約し、`match` の末尾再帰で回す。
  - WHERE の AND を平坦化する `Predicate.flattenLoop` は明示スタック（`Expr list`）で末尾再帰にする
    （`[<TailCall>]` 付き。ローカルの `let rec` には属性を付けられないので、モジュールスコープに置いている）。
    深い AND を `Analyzer.chooseScanMethod` に渡す経路も
    `AnalyzerTests` の `Analyzer.chooseScanMethod: 20000段の AND を chooseScanMethod してもスタックを食わない` で検証する。
    さらに進めて **`Eval.eval` は継続を値（`Kont`）として持つ評価機 `Eval.step`** にする
    （`[<TailCall>]` 付き。CPS だが継続はラムダではなくタグ付きの値）。
    再帰は `step` の自己再帰だけなので全末尾位置になり、式の深さ・`IN` の候補数に依存せずスタックは一定
    （継続はヒープのリストに積む）。
  - `Optimizer.exec` も同じ方式。
    論理木の評価を `ExecFrame`（継続）と `ExecStep`（`EvalNode` / `ApplyOp`）に分け、
    `run` の自己再帰だけにまとめる（`[<TailCall>]` 付き）。
    ラッパ（`Filter` / `Aggregate` / `Having` / `Distinct` / `Sort` / `Limit` / `Project`）と
    `Join` の左右評価をすべて `ExecFrame list` に積むので、継続はヒープ、スタックは論理木の深さに依存しない。
    `Join` は左 → 右 → 結合の順を `JoinRightFrame` / `JoinApplyFrame` で保つ。
  - 式の走査・組み立ては**評価機 `Expr.rewrite`（`Ast.fs`）を共用**する。
    葉の書き換え関数（`rewriteLeaf`）だけを受け取り、
    非葉ノードは `RewriteFrame`（`Parent` / `Done` / `Rest`）に積んで、
    上がってきた子を内部のローカル関数 `plugChildren`（`Expr.children` の逆変換）で組み直す。
    `rewrite` 内のローカル関数 `loop` の自己再帰だけなので、式の深さに依存せずスタックは一定。
    使うのは束縛の `Binder.qualify` と、集約後の読み替え（`Ops` の内部 `substituteAggs`）
    （`AggCall` のように子を持つノードを丸ごと定数へ置き換えるのも `rewriteLeaf` で表現できる）。
    検証は `AnalyzerTests` の `Binder.qualify: 全ノード種を qualify しても構造が保たれる` と
    `OpsTests` の `末尾再帰: 50000段の式を havingFilter してもスタックを食わない`。

## 4. 予約語に注意

`key` / `base` / `begin` / `new` / `old` / `type` / `value` は F# の予約語で、
匿名レコードのフィールド名にも使えない。
衝突例と回避策は [`gotchas.md`](gotchas.md) §8.10 を参照。

## 5. クロスプラットフォームにする

特定の OS に依存しない。
パス操作・ファイル I/O・プロセス間排他などは .NET のクロスプラットフォーム API
（`System.IO.Path` / `Environment` / `FileShare` 等）を使い、
OS 固有のパス区切り・コマンド・API を直接呼ばない。
CI は Linux で走るが、Windows / macOS でも同じ挙動を保つこと。

## 6. 意味の違う番号と所有権を守る

一覧と根拠は [`invariants.md`](invariants.md)、実際の書き方は [`gotchas.md`](gotchas.md) を参照。

- **同じ素の型で意味が違う番号は単位で分ける**（INV-04）。
  `FileNumber` ↔ `ManifestNumber` は `manifestNo`、`SeqNo` ↔ `TxnId` は `txnId`。
  両側ではなく**片側だけ**付ければ対は区別できる。
  単位と型略記は `Types.fs` に置く（`Wal.fs` より前のコンパイル順が必要）。
  直列化の境界でだけ `uint64 x` / `LanguagePrimitives.Int64WithMeasure` で外す。
- **保持する側は渡された配列を複製してから保持する**（INV-19）。
  `Memtable.insert` は `Entry.takeOwnership`、`Transaction.put` / `remove` は `setSlot` が複製。
  逆に `SstEntry` は読み取り専用のビューなので複製しない。
  出口の行も `SqlValue.copyRow` で切り離す（INV-02）。
- **状態は bool で表現せず状態 DU にする**（INV-15）。
  2つの bool は「両方 true」と「判定の順序への依存」を許す。
  1回だけの副作用（リソース解放）は遷移関数に閉じ込める。
