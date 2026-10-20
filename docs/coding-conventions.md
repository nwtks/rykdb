# コーディング規約

RykDb（**.NET 10 / F# 10**）を書くときの規約。
設計の全体像は [`architecture.md`](architecture.md)、
実際に踏んだ落とし穴は [`gotchas.md`](gotchas.md) を参照。

---

## 目次

1. [例外を使わない](#1-例外を使わない)
2. [命令形要素を使わない](#2-命令形要素を使わない)
3. [末尾再帰を前提にする](#3-末尾再帰を前提にする)
4. [予約語に注意](#4-予約語に注意)

---

## 1. 例外を使わない

**このプロジェクトは例外を制御フローに使わない**。
失敗は必ず `Result<'T, DbError>` として返す。
`exception` 定義は **0件**。

`Errors.fs` の `[<AutoOpen>] module ResultOps` が合成に必要なものを全部揃える。

| 提供                                            | 用途                                                 |
| ----------------------------------------------- | ---------------------------------------------------- |
| `( >>= )` `( *> )`                              | monadic な合成                                       |
| `ret` `err` `tryGet` `valueOr` `ofOption` `all` | 単発・畳み込み                                       |
| `result { ... }`                                | `DbResultBuilder` による計算式                       |
| `protect`                                       | .NET I/O API 境界の例外捕捉（`try`/`with` の主経路） |

`protect` は `IOException` / `UnauthorizedAccessException` を `IoError`、
`InvalidDataException` / `FormatException` / `OverflowException` を `CorruptionError` に写す。
ファイル内容の問題を `InternalError`（バグの兆候）にしないため。
`try` / `with` は `protect` のほかに `Manifest.readRecord` と `RowStore.tryFromDirName` の 2箇所だけで、
どちらも `DbError` へ写して例外は外へは出ない。

失敗は `ErrorKind`（`DbError.Kind`）で分類する。

| `ErrorKind`                                          | 意味                                     |
| ---------------------------------------------------- | ---------------------------------------- |
| `SyntaxError` / `NameError` / `TypeError`            | 字句・構文 / 名前解決 / 型の失敗         |
| `ConstraintViolation` / `NotFound` / `AlreadyExists` | DML / DDL の失敗                         |
| `CapacityError`                                      | memtable・バッファのメモリ上限           |
| `CorruptionError`                                    | チェックサム不一致などファイル内容の問題 |
| `ConflictError`                                      | MVCC の write-write 競合                 |
| `IoError` / `InternalError`                          | I/O 失敗 / 内部不整合（バグの兆候）      |

## 2. 命令形要素を使わない

`src/RykDb.Core/` では **`mutable` / `while` / `ref` を 0件**に保つ。
`ResizeArray` / `Dictionary` を使うのは次の 3箇所だけで、いずれもホットパスなので理由をコメントに残す
（CLI / Server は対象外）。

- `Executor.Op.toArray` の行の蓄積
- `Executor.Ops.aggregate` のグループ表
- `Database.fs` のクライアント層（応答の組み立て）

| やりたいこと   | 命令形の代わりに使うもの       |
| -------------- | ------------------------------ |
| 書き込み       | `Writer` = 不変のチャンク列    |
| 読み取り       | `Cursor` = 読み位置を値に持つ  |
| パーサの状態   | `PState`（読み位置は値が持つ） |
| 走査・畳み込み | `Result` を返す末尾再帰        |
| 平衡木の再構築 | 明示スタック（`list` を積む）  |

## 3. 末尾再帰を前提にする

**.NET には tail call 命令がない**。
`[<TailCall>]` がループ化できるのは「自分自身を呼ぶ」場合だけで、相互再帰する構文規則は 1フレーム残る。

採用した手法は次のとおり。

- **構文規則は CPS（継続渡し）** — 引数を「読むもの」ではなく「読んだ後どうするか」に変える。
- **継続は名前付きの `and` グループメンバーへ hoist** する（ラムダだと FS3569 が出る）。
- **実行時のスタック増加は `PState.Depth` で数えて `MaxExpressionDepth = 200` で止める**。
  `.NET` に tail call 命令が無い分、これは「構文上の末尾性」以上の意味を持つ。
- **`result { }` 計算式はループする関数では使わない**。
  `Delay` でクロージャに閉じられるため末尾最適化が効かず、50万トークンで Lexer が `StackOverflow` した実測がある。
- **`[<TailCall>]` は「その関数本体の自己再帰が全部末尾位置」のときだけ付ける**。
  1つでも外れると FS3569 になり、`TreatWarningsAsErrors` のせいでビルドエラーになる。
  構造上末尾にできないものは**付けない理由を呼び出しの直前にコメントで残す**:
  - `Planner.search`（`Optimizer.buildLogical` の点探索候補探し）—
    `match` で左の結果を「消費してから」AND の右へ分岐するため末尾にならない。
    深さは WHERE 式の深さ（`MaxExpressionDepth = 200`）で頭打ちになる。
  - `Op.map` / `Op.take` / `Op.append` — 再帰が `Compute` の中身の中にある。
    次の 1行を取りに来たときに 1段だけ評価されるのでフレームは積み上がらない。
    同じ型が `Sstable.rangeScan` の `fromPos` / `fromBlock`
    （ブロック読みを `next` まで遅らせるため、続きの組み立てがラムダ内に入る）と `SstScan.emit` にもある。
  - 逆に**要素数に比例する反復は `match` に直して末尾再帰にする**
    （`IN` の候補リストと `CASE` の WHEN 節。`eval ... >>= fun` だと候補の数だけスタックが積まれる）。
    カーソルからの数え上げ読み出し（`Catalog` の列定義・名前、`Wal` のバッチ）は
    `Reader.readItems`（Serialization.fs）に集約し、`match` の末尾再帰で回す。
  - これをさらに進めて **`Eval.eval` は継続を値（`Kont`）として持つ評価機 `Eval.step`** にする
    （`[<TailCall>]` 付き。CPS だが継続はラムダではなくタグ付きの値）。
    再帰は `step` の自己再帰だけなので全末尾位置になり、
    式の深さ・`IN` の候補数に依存せずスタックは一定（継続はヒープのリストに積む）。
  - `Planner.exec` も同じ方式。論理木の評価を `ExecFrame`（継続）と
    `ExecStep`（`EvalNode` / `ApplyOp`）に分け、`run` の自己再帰だけにまとめる
    （`[<TailCall>]` 付き）。ラッパ（`Filter` / `Aggregate` / `Having` / `Distinct` /
    `Sort` / `Limit` / `Project`）と `Join` の左右評価をすべて `ExecFrame list` に積むので、
    継続はヒープ、スタックは論理木の深さに依存しない。
    `Join` は左 → 右 → 結合の順を `JoinRightFrame` / `JoinApplyFrame` で保つ。
  - 式の走査・組み立ては**評価機 `Expr.rewrite`（`Ast.fs`）を共用**する。
    葉の書き換え関数（`rewriteLeaf`）だけを受け取り、非葉ノードは `RewriteFrame`（`Parent` / `Done` / `Rest`）に積んで、
    上がてきた子を `Expr.plugChildren`（`Expr.children` の逆変換）で組み直す。
    `rewriteLoop` の自己再帰だけ（`[<TailCall>]` 付き）なので、式の深さに依存せずスタックは一定。
    使うのは束縛の `Binder.qualify` と、集約後の読み替え `Ops.substituteAggs`
    （`AggCall` のように子を持つノードを丸ごと定数へ置き換えるのも `rewriteLeaf` で表現できる）。
    検証は `PlannerTests` の `末尾再帰: 20 万段の式を qualify してもスタックを食わない`、
    `OpsTests` の `末尾再帰: 20 万段の式を havingFilter してもスタックを食わない`、
    `AstTests` の `plugChildren は children の順で子を差し替える`。

## 4. 予約語に注意

`key` / `base` / `begin` / `new` / `old` / `type` / `value` は F# の予約語で、
匿名レコードのフィールド名にも使えない。
衝突例と回避策は [`gotchas.md`](gotchas.md) §8.10 を参照。
