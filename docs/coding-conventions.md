# コーディング規約

RykDb（**net10.0 / F# 10**）を書くときの規約。
設計の全体像は [`architecture.md`](architecture.md)、実際に踏んだ落とし穴は [`gotchas.md`](gotchas.md) を参照。

---

## 目次

1. [例外を使わない](#1-例外を使わない)
2. [命令形要素を使わない](#2-命令形要素を使わない)
3. [末尾再帰を前提にする](#3-末尾再帰を前提にする)
4. [予約語に注意](#4-予約語に注意)

---

## 1. 例外を使わない

**このプロジェクトは例外を制御フローに使わない。**
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
`try` / `with` は `protect` のほかに `Manifest.readRecord` と `RowStore.tryFromDirName` の 2箇所だけで、どちらも `DbError` へ写して例外は外へは出ない。
`unwrap` / `raiseDb` 相当は持たない。

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
`ResizeArray` / `Dictionary` を使うのは次の 3箇所だけで、いずれもホットパスなので理由をコメントに残す（CLI / Server は対象外）。

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

**.NET には tail call 命令がない。**
`[<TailCall>]` がループ化できるのは「自分自身を呼ぶ」場合だけで、相互再帰する構文規則は 1フレーム残る。

採用した手法は次のとおり。

- **構文規則は CPS（継続渡し）** — 引数を「読むもの」ではなく「読んだ後どうするか」に変える。
- **継続は名前付きの `and` グループメンバーへ hoist** する（ラムダだと FS3569 が出る）。
- **実行時のスタック増加は `PState.Depth` で数えて `MaxExpressionDepth = 200` で止める。**
  `.NET` に tail call 命令が無い分、これは「構文上の末尾性」以上の意味を持つ。
- **`result { }` 計算式はループする関数では使わない。**
  `Delay` でクロージャに閉じられるため末尾最適化が効かず、50 万トークンで Lexer が `StackOverflow` した実測がある。
- **`[<TailCall>]` は「その関数本体の自己再帰が全部末尾位置」のときだけ付ける。**
  1つでも外れると FS3569 になり、`TreatWarningsAsErrors` のせいでビルドエラーになる
  （違反箇所の一覧は [`gotchas.md`](gotchas.md) §8.16）。
  構造上末尾にできないものは**付けない理由を呼び出しの直前にコメントで残す**:
  - `Eval.inferType` / `collectAggs` / `rewriteAggs` / `hasAggregate` —
    結果を `>>=` / `match` で「消費してから」分岐するため末尾にならない。
    深さは式の深さ（`MaxExpressionDepth = 200`）で頭打ちになる。
  - `Planner.exec` — 包み（`Filter` / `Aggregate` / `Having` / `Distinct` / `Sort` / `Limit` / `Project`）
    は子の `Result` を `Result.map` / `>>=` で消費する。
    深さは 1文が作るラッパの数（一定）で頭打ち。
    **`Join` だけは左深い木になるので `descend`（積む側・左スパインを降りる）/
    `unwind`（回す側・引き返して結合する）に分けて末尾再帰にする**
    （JOIN 数でスタックが積もらない。左へは「右テーブルを除いたスキーマ」を渡す。`gotchas.md` §3.5）。
  - `Op.map` / `Op.take` / `Op.append` — 再帰が `Compute` の中身の中にある。
    次の 1行を取りに来たときに 1段だけ評価されるのでフレームは積み上がらない。
  - 逆に**要素数に比例する反復は `match` に直して末尾再帰にする**
    （`IN` の候補リストと `CASE` の WHEN 節。`eval ... >>= fun` だと候補の数だけスタックが積まれる）。
    カーソルからの数え上げ読み出し（`Catalog` の列定義・名前、`Wal` のバッチ）は
    `Read.readItems`（Serialization.fs）に集約し、`match` の末尾再帰で回す。
  - これをさらに進めて **`Eval.eval` は継続を値（`Kont`）として持つ評価機 `Eval.step`** にする
    （`[<TailCall>]` 付き。CPS だが継続はラムダではなくタグ付きの値）。
    再帰は `step` の自己再帰だけなので全末尾位置になり、
    式の深さ・`IN` の候補数に依存せずスタックは一定（継続はヒープのリストに積む）。

## 4. 予約語に注意

`key` / `base` / `begin` / `new` / `old` / `type` / `value` は F# の予約語で、匿名レコードのフィールド名にも使えない。
衝突例と回避策は [`gotchas.md`](gotchas.md) §8.10 を参照。
