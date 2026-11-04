# SQL リファレンス

RykDb が受け付ける SQL 文法の一覧。
**実装ソースから機械的に抜き出した「実際に通る範囲」** を記載する。
標準 SQL と食い違う部分は［制限］印を付けて明示する。

---

## 目次

1. [文の一覧](#1-文の一覧)
2. [字句](#2-字句)
3. [データ型](#3-データ型)
4. [値の書き方と表示](#4-値の書き方と表示)
5. [識別子と名前解決](#5-識別子と名前解決)
6. [式](#6-式)
7. [SELECT](#7-select)
8. [INSERT](#8-insert)
9. [UPDATE](#9-update)
10. [DELETE](#10-delete)
11. [DDL](#11-ddl)
12. [トランザクション](#12-トランザクション)
13. [エラー](#13-エラー)
14. [既知の制限](#14-既知の制限)
15. [付録: 完全な例](#15-付録-完全な例)

---

## 1. 文の一覧

```ebnf
statement      ::= select | insert | update | delete | ddl | txn

select         ::= SELECT [DISTINCT] select_item {"," select_item}
                   [ FROM table_ref { join_clause } ]
                   [ WHERE expr ]
                   [ GROUP BY expr {"," expr} ]
                   [ HAVING expr ]
                   [ ORDER BY sort_key {"," sort_key} ]
                   [ LIMIT expr ] [ OFFSET expr ]

select_item    ::= "*" | ident "." "*" | expr [ [AS] ident ]
table_ref      ::= ident [ [AS] ident ]
join_clause    ::= [INNER | LEFT [OUTER]] JOIN table_ref ON expr
sort_key       ::= expr [ASC | DESC]

insert         ::= INSERT INTO table_ref [ "(" column_list ")" ]
                   (VALUES row {"," row} | DEFAULT VALUES)
row            ::= "(" expr {"," expr} ")"
update         ::= UPDATE table_ref SET assignment {"," assignment} [ WHERE expr ]
delete         ::= DELETE FROM table_ref [ WHERE expr ]

ddl            ::= CREATE TABLE [IF NOT EXISTS] ident "(" table_element {"," table_element} ")"
                | CREATE [UNIQUE] INDEX [IF NOT EXISTS] ident ON ident "(" column_list ")"
                | DROP TABLE  [IF EXISTS] ident
                | DROP INDEX  [IF EXISTS] ident
table_element  ::= column_def | PRIMARY KEY "(" column_list ")" | UNIQUE "(" column_list ")"
column_def     ::= ident [data_type [type_args]] [column_constraint {column_constraint}]
column_constraint ::= NOT NULL | PRIMARY KEY | UNIQUE | DEFAULT expr

txn            ::= BEGIN [TRANSACTION] | START TRANSACTION
                | COMMIT [WORK | TRANSACTION] | END
                | ROLLBACK [WORK | TRANSACTION]
```

> **注意** `BEGIN` / `COMMIT` / `ROLLBACK` は SQL 文として transaction を制御する（§12）。
> 詳しくは [12. トランザクション](#12-トランザクション)。

文を複数書く場合は `;` で区切る（**区切りなしは `SyntaxError`**。末尾の `;` は省略可）。
`Database.Execute` は複数文を順に実行する。
`SELECT` が 1 つだけならその結果を返し、複数（または 0 個）なら `RowsAffected` の合計だけを返す
（`Columns` / `Rows` は空）。

---

## 2. 字句

### コメント

| 記法        | 範囲                                                      |
| ----------- | --------------------------------------------------------- |
| `--`        | 行末まで                                                  |
| `/* ... */` | **入れ子可**（`/* a /* b */ c */` として `c` もコメント） |

### 識別子

- 通常: 開始は**文字または `_`**、以降は文字・数字・ `_`
  （`Char.IsLetter` / `LetterOrDigit`。英字以外の Unicode 文字も識別子として使える）
- 引用: `"..."`（`""` で `"` をエスケープ）

**大文字小文字は区別しない**（テーブル名・列名・索引名の比較は `OrdinalIgnoreCase`）。
数字から始めることはできない（`1a` は構文エラー。`_u` のように `_` 始まりは使える）。
テーブル名・索引名は **UTF-8 で 128 バイト以下**（超えると `NameError`）。

### 予約語

```
SELECT  FROM    WHERE   INSERT  INTO    VALUES  UPDATE  SET     DELETE
CREATE  TABLE   INDEX    DROP    ON      AND     OR      NOT     NULL
IS      IN      BETWEEN  LIKE    GROUP   BY      HAVING  ORDER   ASC
DESC    LIMIT   OFFSET   JOIN    INNER   LEFT    AS      DISTINCT
BEGIN   COMMIT  ROLLBACK PRIMARY KEY     UNIQUE  DEFAULT CASE    WHEN
THEN    ELSE    END      EXISTS  IF      TRUE    FALSE
```

`TRUE` / `FALSE` は予約語一覧（`Lexer.Keywords`）には無いが、`Keyword` として特別扱いされる。

### 記号（演算子・区切り）

長いものから照合する。

```
<>   !=   <=   >=   ||   =   <   >   +   -   *   /   %   ;   ,   .   (   )
```

### 数値リテラル

| 形                         | 例                        | 型        |
| -------------------------- | ------------------------- | --------- |
| 整数                       | `0` `42` `-7`             | `INTEGER` |
| 実数（小数部か指数部あり） | `1.5` `.5` `1e3` `1.5E-3` | `REAL`    |

**`DECIMAL` のリテラルはない**。 `1.5` は `REAL`。
厳密な小数を入れたい場合は文字列リテラルで渡し、列の型で変換させる（§3）。

### 文字列リテラル

```sql
'abc'        -- 通常の文字列
'it''s'      -- ' を 1つ含む（'' がエスケープ）
```

### バイナリリテラル

```sql
X'0A1B'       -- 大文字の 16進のみ。バイト数は偶数
```

奇数長・16進以外の文字が含まれる場合は構文エラーになる。

### 式の深さ上限

入れ子深度は **200** を超えない（`Parser.MaxExpressionDepth`）。
`a + b + c + ...` を 1000個並べると構文エラーになる。
構文規則は相互再帰なので実行時にフレームが 1 段ぶん積み上がるため、深さで検出して打ち切る。

---

## 3. データ型

| 型          | 受け付ける綴り           | 内部表現               | 表示                           |
| ----------- | ------------------------ | ---------------------- | ------------------------------ |
| `NULL`      | `NULL`                   | ―                      | `NULL`                         |
| `BOOLEAN`   | `BOOLEAN` `BOOL`         | 真偽値                 | `TRUE` / `FALSE`               |
| `INTEGER`   | `INTEGER` `INT` `BIGINT` | 64 bit 符号付き整数    | `42`                           |
| `REAL`      | `REAL` `DOUBLE`          | IEEE 754 double        | `1.5`                          |
| `DECIMAL`   | `DECIMAL` `NUMERIC`      | 固定小数点数           | `150.25`                       |
| `TEXT`      | `TEXT` `VARCHAR` `CHAR`  | UTF-16 文字列          | `'abc'`                        |
| `BLOB`      | `BLOB`                   | バイト列               | `X'0A1B'`                      |
| `DATE`      | `DATE`                   | 日付（UTC）            | `2024-01-15`                   |
| `TIMESTAMP` | `TIMESTAMP` `DATETIME`   | UTC オフセット付き時刻 | `2024-01-15T10:30:00.0000000Z` |
| `UUID`      | `UUID`                   | GUID                   | `0f8fad5b-…`                   |

### 型引数

`VARCHAR(32)` の `(32)` は**読み取られるだけ**で、切り詰めも検証も行わない。
`CHAR(1)` に 100 文字を入れても保存される。

### 暗黙の型変換（`SqlValue.coerce`）

`INSERT` / `UPDATE` で値を書き込むとき、列の型へ次の変換をかける。
変換できない場合は `TypeError` になる。

| 目標型      | 受け付ける値                                                            |
| ----------- | ----------------------------------------------------------------------- |
| `INTEGER`   | `INTEGER` / `BOOLEAN`(0,1) / 小数部が 0 の `REAL`・`DECIMAL`            |
| `REAL`      | `REAL` / `INTEGER` / `BOOLEAN` / `DECIMAL`                              |
| `DECIMAL`   | `DECIMAL` / `INTEGER` / `REAL` / `BOOLEAN`（`NaN`・`±Infinity` は不可） |
| `BOOLEAN`   | `BOOLEAN` / 0・1 の `INTEGER`・`REAL`                                   |
| `TEXT`      | **すべて**（`SqlLiteral.toSqlLiteral` の文字列表現になる）              |
| `BLOB`      | `BLOB` / `TEXT`（UTF-8 エンコード）                                     |
| `DATE`      | `DATE` / `TIMESTAMP` / `yyyy-MM-dd` 形式の `TEXT`                       |
| `TIMESTAMP` | `TIMESTAMP` / `DATE` / `DateTimeOffset` が解析できる `TEXT`             |
| `UUID`      | `UUID` / `Guid.TryParse` が通る `TEXT`                                  |

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, hired DATE, uid UUID);
INSERT INTO t VALUES (1, '2024-01-15', '0f8fad5b-d9cb-469f-a165-70867728950e');
-- hired は DATE、uid は UUID になる
```

### 値の比較

| 種別                       | 規則                                                                                                                        |
| -------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `INTEGER` `REAL` `DECIMAL` | 3種とも `decimal` に寄せて比較                                                                                              |
| `REAL` の `NaN`            | **`REAL` 同士の比較でのみ**最大。`NaN = NaN` は真。`NaN` と `INTEGER` / `DECIMAL` の比較は `TypeError`（§14「演算の注意」） |
| `TEXT`                     | `StringComparer.Ordinal`（= UTF-8 のバイト順 = コードポイント順）                                                           |
| `BLOB`                     | バイト列の辞書順（短い方が先）                                                                                              |
| `DATE` `TIMESTAMP`         | 相互比較可（`DATE` は UTC 午前 0時の `TIMESTAMP` として扱う）                                                               |
| `UUID`                     | `Guid` の比較                                                                                                               |
| `BOOLEAN`                  | `FALSE < TRUE`                                                                                                              |

**異なる型の組み合わせは比較できない**（`TypeError`）。
`active = 1`（`BOOLEAN` と `INTEGER`）はエラーになる。

- 数値 3種の比較順位: `INTEGER` = `REAL` = `DECIMAL` < `DATE` < `TIMESTAMP` < `TEXT` < `BLOB` < `UUID`
- `NULL` は比較できない。SQL の 3値論理（[§6.2](#62-3値論理)）として扱われる。

---

## 4. 値の書き方と表示

### 入力できる値

| 値       | 書き方                                          |
| -------- | ----------------------------------------------- |
| NULL     | `NULL`                                          |
| 真偽     | `TRUE` / `FALSE`                                |
| 整数     | `42`                                            |
| 実数     | `1.5` `1e3`                                     |
| 文字列   | `'abc'`                                         |
| バイナリ | `X'0A1B'`                                       |
| 日付     | `'2024-01-15'`（`DATE` 列に入れると変換される） |
| 時刻     | `'2024-01-15T10:30:00Z'`                        |
| UUID     | `'0f8fad5b-…'`                                  |

**リテラルには `DECIMAL`・`DATE`・`TIMESTAMP`・`UUID` の書式がない**。
これらは文字列リテラルで書き、列の型で変換させる。

### `INSERT` の値式は定数のみ

`INSERT ... VALUES` の値は **列参照を含む式にできない**。

```sql
INSERT INTO t VALUES (1, 2 + 3);      -- OK（定数式）
INSERT INTO t VALUES (1, other_col);  -- NameError（'other_col' は列でも関数でもありません）
```

### 出力の表示（`SqlLiteral.toSqlLiteral`）

| 型        | 表示                                                  |
| --------- | ----------------------------------------------------- |
| NULL      | `NULL`                                                |
| BOOLEAN   | `TRUE` / `FALSE`                                      |
| INTEGER   | `42`                                                  |
| REAL      | `1.567`（`R` 書式。`NaN` / `Infinity` / `-Infinity`） |
| DECIMAL   | `100.5`                                               |
| TEXT      | `'alice'`（`'` は `''` にエスケープ）                 |
| BLOB      | `X'0A1B'`                                             |
| DATE      | `2024-01-15`                                          |
| TIMESTAMP | `2024-01-15T10:30:00.0000000Z`                        |
| UUID      | `0f8fad5b-d9cb-469f-a165-70867728950e`                |

CLI は 1行目に列名、2行目に区切り（列名と同じ文字数の `-`）、3行目以降に `  値, 値, …` を出す。
0 行の `SELECT` でもヘッダの 2行だけは出る。
`SELECT` 以外は `(N rows)` の 1行だけ。

---

## 5. 識別子と名前解決

```sql
SELECT p.id, p.name FROM people p;      -- テーブル別名
SELECT id FROM people p;                -- 修飾は省略できる
SELECT people.id FROM people;           -- 修飾はテーブル実名でもよい
SELECT "Order Id" FROM "My Table";      -- 引用識別子
```

- 修飾なしの列名は `FROM` にあるテーブルの順に探す。
- 結合句で同じ列名が複数の表にあれば、修飾が必須。
- `UNION` / `RIGHT` / `FULL` / `CROSS` / `NATURAL` / `OUTER` / `USING` などの未対応の SQL 語は、
  列名・`SELECT` 項目の別名には使えるが、**裸のテーブル別名**には使えない
  （`FROM t AS right` のように `AS` を付ける）。
  `JOIN ... USING (...)` は明示的な `SyntaxError` になる（§14）。

### `SELECT` 項目の後置演算子

`SELECT` の項目は式として解析されるので、裸の識別子・関数呼び出しの後に演算子を書ける。

```sql
SELECT name || 'x' FROM t;          -- OK
SELECT SUM(d) + 0.5 FROM t;        -- OK
```

---

## 6. 式

### 6.1 演算子の優先順位

優先順位の高い方から。

| 優先順位 | 演算子                                                                                            | 意味                                         |
| -------: | ------------------------------------------------------------------------------------------------- | -------------------------------------------- |
|        1 | `.`                                                                                               | 修飾                                         |
|        2 | `()`                                                                                              | 関数呼び出し・グループ化                     |
|        3 | `-` `+`                                                                                           | 単項符号（`+x` はそのまま、`-x` は符号反転） |
|        4 | `*` `/` `%`                                                                                       | 乗除算                                       |
|        5 | `+` `-` `\|\|`                                                                                    | 加減算・文字列連結                           |
|        6 | `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `IS [NOT] NULL`, `[NOT] IN`, `[NOT] BETWEEN`, `[NOT] LIKE` | 述語                                         |
|        7 | `NOT`                                                                                             | 論理否定                                     |
|        8 | `AND`                                                                                             | 論理積                                       |
|        9 | `OR`                                                                                              | 論理和                                       |

- `AND` は `OR` より強い（`a OR b AND c` は `a OR (b AND c)`）。
- 算術演算子と `||` は同じ優先順位で**左結合**（`1 + 2 || 'x'` は `(1 + 2) || 'x'`）。

### 6.2 3 値論理

比較・`IS NULL` 以外の述語は `TRUE` / `FALSE` / `UNKNOWN` の 3 値。

| 式                            | 結果                                  |
| ----------------------------- | ------------------------------------- |
| `NULL = NULL`                 | `UNKNOWN`（＝`WHERE` では偽）         |
| `NULL <> NULL`                | `UNKNOWN`                             |
| `1 = NULL`                    | `UNKNOWN`                             |
| `NULL AND FALSE`              | `FALSE`                               |
| `NULL AND TRUE`               | `UNKNOWN`                             |
| `NULL OR TRUE`                | `TRUE`                                |
| `NOT NULL`                    | `UNKNOWN`                             |
| `x IS NULL` / `x IS NOT NULL` | `TRUE` / `FALSE`（NULL でも常に真偽） |

`WHERE` / `HAVING` では `UNKNOWN` は `FALSE` として扱う。
算術演算・`ABS`・`CASE` の条件など、述語以外は NULL が**そのまま伝播**する。

### 6.3 算術

| 式              | 挙動                                                              |
| --------------- | ----------------------------------------------------------------- |
| `1 + 2`         | `3`                                                               |
| `10 / 4`        | `2`（**整数演算は切り捨て**。`INTEGER` ÷ `INTEGER` は `INTEGER`） |
| `10 % 4`        | `2`                                                               |
| `1 + NULL`      | `NULL`                                                            |
| `1 / 0`         | エラー（`0 での除算は定義されていません`）                        |
| `1.5 + 1`       | `REAL`（片方が REAL/DECIMAL なら結果もそうなる）                  |
| `'a' \|\| 'b'`  | `'ab'`                                                            |
| `'a' \|\| NULL` | `NULL`                                                            |

数値の型は次のとおり決まる。

```
INTEGER op INTEGER        -> INTEGER
DECIMAL op DECIMAL        -> DECIMAL
REAL が 1 つでも含まれる  -> REAL
片方が NULL               -> NULL
```

実数除算にしたいときは `10 / 4.0` のように `REAL` を混ぜる。

### 6.4 述語

```sql
x = 1        x <> 1       x != 1
x <  1       x <= 1       x >  1       x >= 1
x IS NULL    x IS NOT NULL
x IS TRUE    x IS NOT TRUE    x IS FALSE    x IS NOT FALSE
x IS UNKNOWN x IS NOT UNKNOWN
x IN (1, 2, 3)      x NOT IN (1, 2, 3)
x BETWEEN 1 AND 10   x NOT BETWEEN 1 AND 10
name LIKE 'a%'       name NOT LIKE 'a%'
```

- `BETWEEN` は両端を含む（`1 <= x <= 10`）。
- `IN` の要素は**式のリスト**。`x IN (SELECT ...)` も使える（§7.9）。
- `EXISTS (SELECT ...)` / `NOT EXISTS (SELECT ...)` も使える（§7.9）。
- `LIKE` のワイルドカード:

| 記号 | 意味                     |
| ---- | ------------------------ |
| `%`  | 0 文字以上の任意の文字列 |
| `_`  | ちょうど 1 文字          |

- `LIKE ... ESCAPE '文字'` に対応する。エスケープ文字は 1 文字。
  エスケープ文字の直後の `%` / `_` / エスケープ文字自身はリテラルになる。
  エスケープ文字がパターンの末尾に来た場合は `TypeError`。
  `\` を既定のエスケープにする仕組みや、大文字小文字を区別しないモード（`ILIKE`）は**ない**。
- パターンも被比較も文字列表現（`toDisplayString`）で照合し、引数が `NULL` なら結果は `NULL`。
- `IS TRUE` / `IS FALSE` / `IS UNKNOWN` は 3値論理の判定。
  `NULL`（UNKNOWN）は `IS TRUE` / `IS FALSE` とも `FALSE`、`IS UNKNOWN` は `TRUE`。
  否定形（`IS NOT …`）はその否定。`BOOLEAN` 以外の値は UNKNOWN として扱う
  （例: `1 IS TRUE` は `FALSE`、`1 IS UNKNOWN` は `TRUE`）。

```sql
SELECT name FROM people WHERE name LIKE 'a%';     -- a で始まる
SELECT name FROM people WHERE name LIKE '_ob';     -- 2文字目が o、3文字目が b
SELECT name FROM people WHERE name LIKE 'a_c';     -- 3文字、2文字目が任意
SELECT name FROM people WHERE name LIKE 'a!%' ESCAPE '!';  -- 文字列 'a%'
```

### 6.5 集約関数

| 関数       | 戻り値             | 空のグループ | `NULL` の扱い |
| ---------- | ------------------ | ------------ | ------------- |
| `COUNT(*)` | `INTEGER`          | `0`          | 数える        |
| `COUNT(x)` | `INTEGER`          | `0`          | **数えない**  |
| `SUM(x)`   | `DECIMAL` / `REAL` | `NULL`       | 無視          |
| `AVG(x)`   | 常に `REAL`        | `NULL`       | 無視          |
| `MIN(x)`   | 入力と同じ型       | `NULL`       | 無視          |
| `MAX(x)`   | 入力と同じ型       | `NULL`       | 無視          |

```sql
SELECT dept_id,
       COUNT(*)    AS c,
       SUM(salary) AS total,
       AVG(salary) AS mean,
       MIN(name)   AS lo,
       MAX(name)   AS hi
  FROM people
 GROUP BY dept_id
HAVING COUNT(*) > 1
 ORDER BY dept_id;
```

補足:

- `AVG` は `SUM / COUNT` を実行時に計算する。
- `MIN` / `MAX` は文字列・日付・時刻にも使える。
- `GROUP BY` を書かない `SELECT SUM(x) FROM t` は表全体の 1グループとして集計される。
- `GROUP BY` の式は `SELECT` 項目 / `HAVING` でも同じ式を書ける
  （式全体が集約後のスロットへ解決される）。
- `GROUP BY` / 集約関数を使うときに `SELECT *` は書けない（列を明示する）。
- 集約も `GROUP BY` も無い `HAVING` は `SyntaxError`。
- `SUM` / `AVG` は `INTEGER` / `DECIMAL` / `REAL` が混在しても合計を落とさない
  （`REAL` が混在すると `SUM` も `REAL`、それ以外の `SUM` は `DECIMAL`。`AVG` は常に `REAL`）。
  入力が 0行でも 1行を返す（`COUNT(*)` は `0`、`SUM` / `AVG` / `MIN` / `MAX` は `NULL`）。
- 集約関数のエラーは**伝播する**。文字列列に `SUM` を掛けると `TypeError` になる。
- `COUNT` / `SUM` / `AVG` / `MIN` / `MAX` の `DISTINCT` は**重複排除する**
  （`SUM(DISTINCT x)` など）。
  `DISTINCT` は 1 引数の集約にだけ書ける。

### 6.6 スカラー関数

関数名は大文字小文字を区別しない。

| 関数                |   引数 | 戻り値               | `NULL` の扱い    | 備考                                                                    |
| ------------------- | -----: | -------------------- | ---------------- | ----------------------------------------------------------------------- |
| `UPPER(x)`          |      1 | `TEXT`               | 大文字化         |
| `LOWER(x)`          |      1 | `TEXT`               | 小文字化         |
| `TRIM(x)`           |      1 | `TEXT`               | 前後の空白を削除 |
| `LENGTH(x)`         |      1 | `INTEGER`            | `NULL`           | UTF-16 文字数                                                           |
| `ABS(x)`            |      1 | 入力と同じ数値型     | `NULL`           | 非数値はエラー                                                          |
| `ROUND(x)`          |      1 | 入力と同じ数値型     | `NULL`           | 0.5 は 1 へ（四捨五入・`AwayFromZero`）。`INTEGER` / `REAL` / `DECIMAL` |
| `COALESCE(a, b, …)` | 1 以上 | 最初の非 `NULL` の型 | 全滅なら `NULL`  | 引数数は自由                                                            |
| `CAST(x AS 型)`     |      1 | 変換後の型           | `NULL`           | 明示変換。`TEXT` → 数値 / `BOOLEAN` も可                                |

- **未知の関数**は `未知のスカラー関数 '…' です`。
- **引数数が違う**場合は `… 関数は 1個の引数が必要ですが、2個渡されました`。
  ただし空の引数リスト（`LENGTH()`）は関数の前に構文エラーになる。
- `ROUND` は `INTEGER` / `REAL` / `DECIMAL` を受け付け、同じ型を返す。
  文字列など数値以外を渡すと `ROUND は数値を受け付けますが 'abc' です` になる。
- `CAST` は `expr AS 型` の形で、型引数（`VARCHAR(10)` の `(10)`）は読み飛ばす。
  `TEXT` から `INTEGER` / `REAL` / `DECIMAL` / `BOOLEAN` への変換は CAST 専用の規則で行う
  （`CAST('42' AS INTEGER)` は `42`）。
  変換できない値は `TypeError`。

### 6.7 条件式

```sql
CASE WHEN 条件 THEN 値 [WHEN 条件 THEN 値 …] [ELSE 既定値] END
```

- 条件は上から順に評価され、**最初に真になった** `THEN` を返す。
- 条件が `NULL`（`UNKNOWN`）のときは次の `WHEN` に進む。
- `ELSE` を書かないと、真の分岐が 1 つも無いときは `NULL` を返す。

```sql
SELECT name,
       CASE WHEN age > 30 THEN 'senior'
            WHEN age > 20 THEN 'mid'
            ELSE 'junior'
       END AS band
  FROM people
 ORDER BY id;
```

### 6.8 関数呼び出し

```
関数名 "(" [expr {"," expr}] ")"
```

引数は 0個以上。集約関数以外は無制限に呼び出せる。

---

## 7. SELECT

### 7.1 文法

```sql
SELECT [DISTINCT] 項目 [, 項目 …]
  [FROM テーブル [[AS] 別名]
       [[INNER | LEFT [OUTER]] JOIN テーブル [[AS] 別名] ON 条件] …]
  [WHERE 条件]
  [GROUP BY 式 [, 式 …]]
  [HAVING 条件]
  [ORDER BY 式 [ASC|DESC] [, …]]
  [LIMIT 定数式] [OFFSET 定数式];
```

- **`FROM` は省略できる**。入力は 1行 0列になる。
  `SELECT 1` / `SELECT 1 + 2 AS x` は 1行 1列（列名は式の表示か別名）を返し、
  `WHERE` / `ORDER BY` / `LIMIT` はその 1行に適用される。
  集約は 1行を入力として集計する（`SELECT COUNT(*)` は 1、
  `SELECT COUNT(*) WHERE 1 = 2` は 0）。
  `FROM` が無いときは `*` / `t.*` を書けない（展開する列が無い）。
  `JOIN` には `FROM` が必要。
- 条件・式の中にはサブクエリを書ける（§7.9）。
  `FROM` の派生テーブル（`FROM (SELECT ...) AS x`）は未対応。
- `JOIN` は `INNER JOIN` / `JOIN` / `LEFT JOIN` / `LEFT OUTER JOIN`。
  `RIGHT` / `FULL` / `CROSS` は**ない**。
- `ON` は省略できない
  （`CROSS JOIN` の代わりに `ON 1 = 1`）。
- `DISTINCT` は射影の**後**に適用される
  （`SELECT DISTINCT a, b` は `(a, b)` の組で重複除去）。

### 7.2 評価順

```
FROM → JOIN → WHERE → GROUP BY → HAVING → 集約 → ORDER BY → 射影 → DISTINCT → LIMIT
```

`LIMIT` は最後に適用される（pushdown されない）。

### 7.3 結果の列名

| 記述             | 列名                                            |
| ---------------- | ----------------------------------------------- |
| `SELECT id`      | `id`                                            |
| `SELECT id AS x` | `x`                                             |
| `SELECT (1 + 2)` | `(1 + 2)`（式の表示文字列。**常に丸括弧つき**） |
| `SELECT *`       | 各列名（`id`, `name`, …）                       |
| `SELECT t.*`     | `t.id`, `t.name`, …（修飾つき）                 |

クライアントが列名でアクセスする場合は `AS` を付けるのが安全。

#### 別名の参照

`ORDER BY` / `GROUP BY` / `HAVING` では `SELECT` 項目の別名（`AS`）を参照できる。

```sql
SELECT name, age AS a FROM people ORDER BY a;      -- age の順
SELECT age AS a, COUNT(*) FROM people GROUP BY a;  -- age でまとめる
SELECT COUNT(*) AS c FROM people HAVING c > 2;     -- 別名を使った条件
```

- 入力の表に**同名の列があるときは列が優先**される
  （`SELECT name AS age ... ORDER BY age` は `age` 列の順になる）。
- 別名の連鎖（別名の別名）も解決する。
- 別名が自分自身や互いを参照する場合は解決しない
  （`ORDER BY` では「列 'x' が存在しません」になる）。
- `ORDER BY` には列参照・式・序数（1 始まり）を書ける。
  序数は `SELECT` 項目の位置（`ORDER BY 2` は 2 番目の項目）。
  式は行ごとに評価され、ソート後に元の列だけが残る。
  集約式（`ORDER BY COUNT(*)` など）は未対応。
- `t.*` の `t` が `FROM` / `JOIN` に存在しない場合は `NameError`
  （0列の成功にはしない）。

### 7.4 索引が使われる条件

`WHERE` に**列 = 定数**（＝リテラル、または負の数値リテラル）の等価述語が `AND` で連結されているとき、それらをまとめて評価する。
`LIKE 'prefix%'`（`%` が末尾のみ・`_` なし）は**範囲条件として索引に降ろす**。

| 条件                                                                   | 走査方法         |
| ---------------------------------------------------------------------- | ---------------- |
| 主キーの全列 `= 定数`                                                  | `PkLookup`       |
| 主キーの一部の列 `= 定数`                                              | `SeqScan`        |
| 索引の**先頭から連続する** `= 定数`（1列以上）                         | `IndexLookup`    |
| 単一列の主キーに下限のある範囲条件 (`>`, `>=`, `BETWEEN`, `LIKE 'x%'`) | `RangeScan`      |
| 索引（**複合可**）の範囲条件。ただし範囲列より前の全列に `= 定数`      | `IndexRangeScan` |
| それ以外                                                               | `SeqScan`        |

主キー・索引は値のバイト列表現で引くため、列の宣言型とリテラルの型が一致するときだけ使う。
型が違うリテラルは全走査に落ち、比較時に `TypeError` になる。

```sql
CREATE INDEX idx_dept ON people (dept_id);
SELECT id FROM people WHERE dept_id = 2;      -- 単一列索引の等値 → IndexLookup
SELECT id FROM people WHERE dept_id > 2;      -- 範囲 → IndexRangeScan
SELECT id FROM people WHERE name LIKE 'car%'; -- 接頭辞 → 範囲（索引があれば）

CREATE INDEX ix_pair ON t (a, b);
SELECT id FROM t WHERE a = 1 AND b > 2;       -- 前置 a が等値 + b が範囲 → IndexRangeScan
SELECT id FROM t WHERE b > 2;                 -- 前置 a が無い → SeqScan
SELECT id FROM t WHERE a = 1 AND b = 2;       -- 全列等値 → IndexLookup
SELECT id FROM t WHERE a = 1;                 -- 前置 1列の等値 → IndexLookup
```

制限:

- **範囲走査は下限のある条件のみ**。`<` / `<=` だけの条件は全走査になる。
- **索引の等値一致（`IndexLookup`）は、索引の先頭から連続する `= 定数` が 1列以上あるとき**
  （全列でなくてもよい）。
  範囲条件がその次の列にある場合は、より多くの列を絞れる `IndexRangeScan` を優先する。
  複合索引の範囲は「前置列すべてが `=`、その次の 1列が範囲」の形で使う。
- `LIKE` は **`%` が末尾のみ・`_` なし**のときだけ範囲に変換する。
  `LIKE '%x'` や `LIKE 'a_b%'` は全走査 + 後段の `Filter`。
- 型が一致しないリテラルは全走査に落ちる（比較時に `TypeError`）。

### 7.5 結合アルゴリズム

`ON` 句に左×右の等値条件がある場合、結合アルゴリズムを自動選択する。

| 条件                                                                                    | アルゴリズム             |
| --------------------------------------------------------------------------------------- | ------------------------ |
| 左が小さい（`LIMIT` / 主キー等値 / `VALUES` で 32行以下）かつ右が `Scan` で結合列に索引 | インデックス入れ子ループ |
| 両入力が結合キー順で走査できる（主キー / 索引の先頭列と結合キーが一致）                 | マージ結合               |
| 左×右の等値条件がある（上記以外）                                                       | ハッシュ結合             |
| 等値条件がない（非等値など）                                                            | 入れ子ループ             |

- 等値キーは**列の型が一致する**ときだけ使う。
  型が違うと入れ子ループに落ちる。
- **マージ結合**は両入力が結合キーで昇順のときだけ選ぶ。
  主キー走査・索引走査はキー順に返るので、結合キーがその先頭列と一致すれば使える
  （`WHERE` の絞り込みや `LIMIT` は順序を変えない）。
  `ON` の等値条件の記述順は自由で、走査順に合わせて並べ替える
  （左 PK が `(a,b)` なら `ON b = b AND a = a` でもよい）。
  右側を丸ごと保持せず、両側を 1行ずつ流す。
  `LEFT JOIN` では一致が無い（または残りの `ON` 句が偽の）左行を `NULL` で補う。
- `NULL` は `=` で一致しない（ハッシュ/索引/マージとも NULL キーは結合しない）。
- 等値キー以外の `ON` 句は残りの条件として評価する。
- 結合順序は `FROM` に書かれた順（選択性に基づく並べ替えは未実装）。
- `LEFT JOIN` は右側の一致がない場合に `NULL` を補う。
- `ON` 句の評価が失敗した場合（型エラーなど）は、黙って行を落とさず**クエリ全体がエラー**になる。

```sql
SELECT p.id, p.name, d.dname
  FROM people p
  JOIN depts d ON p.dept_id = d.id
 ORDER BY p.id;

SELECT p.id, d.dname
  FROM people p
  LEFT JOIN depts d ON p.dept_id = d.id
 ORDER BY p.id;
```

### 7.6 `ORDER BY` の安定性

ソートは外部マージ（`ExtSort`）で並べる。
バジェット（管理メモリの概算。既定 64MiB）に収まる入力はメモリだけで、
超過分は一時ファイル（`%TEMP%/rykdb-sort-*`）へ溢らしてから k-way merge する。
同時に開く run は最大 32本で、それを超える場合は中間 run へ multi-pass merge する。
同値な行の順序は保証されない（`ORDER BY` のキーが一意でないとき）。

- ソートは全入力を消費してからでないと 1行も出せないため、比較不能な型があれば**その時点で** `TypeError` になる。
- 一時ファイルは run が尽いた時点で消える。途中打ち切りで残ったものは次回のソート時に回収する。
- **主キー順の `ORDER BY` はソートを省略する**。
  単一テーブル・集約なし・`DISTINCT` なしで、
  `ORDER BY` の先頭が**主キー全列を定義順・昇順**で並べている場合、
  走査がそのまま主キー順に返るので `Sort` を作らない
  （主キーは一意なので後続のキーは無関係）。
  主キーの一部だけ・`DESC`・索引走査になる条件・集約・`DISTINCT`・結合では通常どおりソートする。

### 7.7 `LIMIT` / `OFFSET`

- 値は **0 以上の定数式**（行ごとの列参照・負数・範囲外はエラー）。
  `LIMIT 1+0` のような定数式は評価される。
- 評価は最後に 1回だけ。
  `OFFSET` → `LIMIT` の順で適用される。
- `LIMIT` / `OFFSET` はそれぞれ単独でも書ける（`OFFSET` 単独は `LIMIT` なし扱い）。
- `LIMIT 0` は「0 行」を返す（スキャン自体を実行しない）。
- **単純な全走査**（`WHERE` / `GROUP BY` / `DISTINCT` / 結合なし）では、
  `OFFSET + LIMIT` の件数をストレージへ降ろす。
  ストレージは先頭 `OFFSET + LIMIT` 行だけをデコードし、必要になるまで後続の SSTable を開かない。
  `ORDER BY` があっても、主キー全列の昇順（単一テーブル）なら Sort を作らないためヒントが降りる（§7.6）。
  `WHERE` など行数を変える演算子がある場合は全件走査のまま（ヒントを降ろさない）。

```sql
SELECT id FROM people ORDER BY id LIMIT 2;            -- 先頭 2 行
SELECT id FROM people ORDER BY id LIMIT 2 OFFSET 1;  -- 2 行目から 2 行
```

### 7.8 例

```sql
CREATE TABLE people (
  id      INTEGER PRIMARY KEY,
  name    TEXT NOT NULL,
  age     INTEGER,
  dept_id INTEGER,
  salary  DECIMAL
);

INSERT INTO people VALUES
  (1, 'alice', 30, 1, 100.5),
  (2, 'bob',   25, 1, 200.25),
  (3, 'carol', 41, 2, 150.0),
  (4, 'dan',   25, 2, 120.0);
```

```sql
SELECT id, name FROM people WHERE age >= 25 ORDER BY name;

-- name
-- ----
-- 'alice'
-- 'bob'
-- 'dan'

SELECT dept_id, COUNT(*) AS c, AVG(salary) AS mean
  FROM people
 GROUP BY dept_id
 ORDER BY dept_id;

-- dept_id | c | mean
--        1 | 2 | 150.375
--        2 | 2 | 135
```

### 7.9 サブクエリ

式の中に `SELECT` を書ける。**相関サブクエリ（外側の列の参照）は未対応**。

| 形                                     | 結果                                                                  |
| -------------------------------------- | --------------------------------------------------------------------- |
| `(SELECT ...)`                         | スカラー値。1列だけ・0 行なら `NULL`・2行以上は `ConstraintViolation` |
| `x [NOT] IN (SELECT ...)`              | 1列の値を `IN` の要素として評価（`NULL` の 3値論理もそのまま）        |
| `EXISTS (SELECT ...)` / `NOT EXISTS …` | 1行以上なら `TRUE`（列数は問わない）                                  |

```sql
SELECT id, (SELECT MAX(v) FROM t) AS mx FROM t;

SELECT id FROM t WHERE v > (SELECT AVG(v) FROM t);

SELECT id FROM t WHERE id IN (SELECT id FROM t WHERE v >= 20);

SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t WHERE v > 25);
```

- サブクエリは**外側のクエリを実行する前に 1 回だけ**実行され、結果は定数へ置き換わる
  （`IN` は要素のリストへ展開する）。同じ文の中のサブクエリは同じスナップショットを見る。
- 列数が 1 でないサブクエリは `TypeError`。
- `INSERT ... VALUES` / `UPDATE ... SET` / `UPDATE`・`DELETE` の `WHERE` でも使える。
- `LIMIT` / `OFFSET` にも書ける（定数へ解決される）。
- 外側の列を参照する（相関）サブクエリは `NameError` になる。
- サブクエリでも `FROM` を省略できる（`EXISTS (SELECT 1)` は常に真）。
- `FROM (SELECT ...) AS x`（派生テーブル）は未対応（`SyntaxError`）。
- `CREATE TABLE ... DEFAULT (SELECT ...)` は未対応（INSERT 時に `SyntaxError`）。
- スカラーサブクエリは最大 2行、`EXISTS` は 1行だけを読む。
  `IN (SELECT ...)` は結果をすべて保持する
  （管理メモリ換算 256MiB の上限。超過は `CapacityError`）。
- サブクエリの入れ子も**式の深さ上限（200）に含まれる**。
  1 段で複数の深さ単位を消費するため、実測では 90段は実行でき、
  150段は `式の入れ子が深すぎます` になる（StackOverflow にはならない）。

---

## 8. INSERT

```sql
INSERT INTO テーブル [ (列 [, 列 …]) ] VALUES (式 [, 式 …]) [, (式 [, …]) …];
INSERT INTO テーブル DEFAULT VALUES;
```

### 挙動

- **列リストを省略**すると、テーブル定義の順に値を並べる。
- **列リストに無い列**は `DEFAULT` を使い、無ければ `NULL` になる。
- `DEFAULT VALUES` は列リスト無しで全列を既定値で埋める
  （主キーなどに既定値が無い場合は `NOT NULL` 違反）。
- `VALUES` の要素に `DEFAULT` を書くと、その列の既定値を使う。
- **値式は定数のみ**（列参照不可）。サブクエリは使える（§7.9）。
- `INSERT ... SELECT` と `ON CONFLICT` / `ON DUPLICATE KEY UPDATE`（upsert）は未対応（§14）。
- 値は列の型へ**暗黙変換**される（[§3](#暗黙の型変換sqlvaluecoerce)）。
- `NOT NULL` 制約は行ごとに検証される。
- **1つの `INSERT` は 1トランザクション**（途中で失敗したら何も残らない）。

### 主キー重複はエラー

**エラーになる**（upsert しない）。
複数列の主キーも同じで、値の組が一致すればエラーになる。

### `UNIQUE` は検査される

`UNIQUE` 制約・`UNIQUE` 索引は重複を検査する。
ただし `NULL` は「値なし」なので重複とみなさない。
複合 `UNIQUE` では、対象列のどれか1つでも `NULL` なら検査をスキップする。
複数行 `UPDATE` は文全体の最終状態で検査する。
そのため、同一文内で一意値を交換する更新も成功する
（更新対象外の行との衝突は `ConstraintViolation`）。

```sql
INSERT INTO people VALUES (1, 'alice2', 30, 1, 1.0);  -- id=1 が既に存在するのでエラー
INSERT INTO people (id, email) VALUES (2, NULL), (3, NULL);  -- 複数の NULL は OK
```

### 例

```sql
INSERT INTO people VALUES (1, 'alice', 30, 1, 100.5), (2, 'bob', 25, 1, 200.25);

INSERT INTO people (name, id) VALUES ('carol', 3);
-- age, dept_id, salary は NULL
```

---

## 9. UPDATE

```sql
UPDATE テーブル SET 列 = 式 [, 列 = 式 …] [ WHERE 条件 ];
```

### 挙動

- 複数列を同時に更新できる。式は**更新前の行**に対して評価される。
  サブクエリは使えるが、外側の行の列は参照できない（§7.9）。
- `WHERE` を省略すると全行が対象。
- 条件に合う行すべてが更新される（`LIMIT` 相当の句がない）。
- `UPDATE ... FROM` と `UPDATE ... LIMIT` は未対応（§14）。
- テーブル別名は `SET` / `WHERE` の列解決に使われない。
  `UPDATE t AS x ... WHERE x.id = 1` は `NameError` になる（対象行が 1行も無ければ 0行のまま）。
  `t.id` のように実名で修飾する。
- 影響行数は `StatementResult.RowsAffected` に入る。
- 索引があるテーブルでは、更新のたびにすべての索引のエントリが書き直される。

### 主キー列は更新できない

主キー列を `SET` すると `ConstraintViolation` になる（古い主キーの行が残って重複するため）。
主キーを変えたい場合は `DELETE` + `INSERT` を使う。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, n TEXT);
INSERT INTO t VALUES (1,'a'),(2,'b');
UPDATE t SET id = 100 WHERE id = 1;   -- エラー（何も更新されない）

SELECT id, n FROM t ORDER BY id;
-- 1, 'a'
-- 2, 'b'
```

主キーを変えたい用途では `DELETE` + `INSERT` を使う。

```sql
UPDATE people SET age = age + 1 WHERE id = 1;
UPDATE people SET salary = salary * 1.1;    -- 全行
```

---

## 10. DELETE

```sql
DELETE FROM テーブル [ WHERE 条件 ];
```

- `WHERE` を省略すると全行を削除する。
- 削除は主キーと**すべての索引**に墓碑（tombstone）を書く。
- `DELETE ... USING` と `DELETE ... LIMIT` は未対応（§14）。
- テーブル別名は `WHERE` の列解決に使われない（`t.id` のように実名で修飾する）。
  対象行が 1 行も無ければ 0 行のままエラーにならない。
- 影響行数は `StatementResult.RowsAffected` に入る。
- 条件に合う行が 0件でもエラーにはならない（0行になる）。

```sql
DELETE FROM people WHERE age < 18;
DELETE FROM people;             -- 全削除
```

---

## 11. DDL

### 11.1 CREATE TABLE

```sql
CREATE TABLE [IF NOT EXISTS] テーブル名 (
  列定義 [, 列定義 …]
  [, PRIMARY KEY (列 [, …])]
  [, UNIQUE (列 [, …])]
);
```

```sql
CREATE TABLE people (
  id      INTEGER PRIMARY KEY,          -- 列レベル PRIMARY KEY（NOT NULL も付く）
  name    TEXT NOT NULL,
  email   VARCHAR(255) UNIQUE,          -- 長さ 255 は宣言されるだけ
  age     INTEGER DEFAULT 0,
  created TIMESTAMP DEFAULT '2024-01-01T00:00:00Z',
  UNIQUE (name, email)                  -- テーブルレベル制約
);
```

#### 制約

| 制約          | 場所         | 効果                                                      |
| ------------- | ------------ | --------------------------------------------------------- |
| `NOT NULL`    | 列           | **検査される**（`NULL` を入れると `ConstraintViolation`） |
| `PRIMARY KEY` | 列・テーブル | **必須**。1テーブルに1定義だけ。`NOT NULL` も付く         |
| `UNIQUE`      | 列・テーブル | 重複を検査する（`NULL` は除く）                           |
| `DEFAULT`     | 列           | `INSERT` で値を省略したときに使われる（定数式のみ）       |

`DEFAULT` はカタログに保存され、DB を再オープンした後も使われる。
`DEFAULT` の値も列の型へ暗黙変換される。
変換できない既定値は `INSERT` 時に `TypeError` になる。

- **主キーは省略できない**。
  1つも無いと`テーブル t に主キーがありません` になる。
- 主キーは1テーブルに1定義だけ。
  列レベルとテーブルレベルの併用、複数列への列レベル `PRIMARY KEY`、
  複数のテーブルレベル `PRIMARY KEY` は `ConstraintViolation` になる。
- 主キーは複数列にできる。
  値は列の宣言順に連結される。
- テーブルレベルの `PRIMARY KEY` も、対象列に `NOT NULL` を付ける。
- 列名の重複（大文字小文字を無視）は `NameError`。
- テーブルレベルの `UNIQUE` は、存在しない列・重複列を `NameError` で拒否する。
- `IF NOT EXISTS` を付けると、既存テーブルなら**何もせずに成功**する。
- `IF NOT EXISTS` と `IF EXISTS` の両方の綴りを受け付ける。

#### 列定義で型を省略する

型を書かずに制約だけを書く構文も受け付ける（型は `TEXT` になる）。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, name NOT NULL);
```

### 11.2 CREATE INDEX

```sql
CREATE [UNIQUE] INDEX [IF NOT EXISTS] 索引名 ON テーブル (列 [, 列 …]);
```

- **既存の行はすべて索引に投入される**（backfill）。
- backfill は **プロセス内で排他**。
  同じ DB で実行中の transaction があれば `ConflictError`（`実行中の transaction があります。…`）を返すので、
  COMMIT / ROLLBACK してから実行する。
  他の文は backfill が終わるまで待つ。
- 索引は**独立した LSM ツリー**（keyspace `i:<索引名>`）に格納される。
- `UNIQUE` は重複を検査する（`NULL` は除く）。
- 同名の索引が既にあれば `索引 <名前> は既に存在します`。
- 索引する列が存在しない場合は `テーブル <t> に列 <c> がありません`。
- 索引列の重複は `NameError`。
- `__unique_` で始まる索引名は `UNIQUE` 制約用に予約されており、作成できない。
- `IF NOT EXISTS` を付けると、同名の索引が既にあっても**何もせずに成功**する。

```sql
CREATE INDEX idx_dept ON people (dept_id);
CREATE UNIQUE INDEX idx_email ON people (email);
```

### 11.3 DROP

```sql
DROP TABLE [IF EXISTS] テーブル名;
DROP INDEX [IF EXISTS] 索引名;
```

- `IF EXISTS` を付けると、存在しなくても**エラーにならない**。
- `DROP TABLE` は先に定義を外してテーブルを見えなくし、
  その後でテーブルと全索引 keyspace の全エントリへ tombstone を 8192件ずつ書く。
  後始末は `.drop-cleanup` ジャーナルに記録され、失敗時は次回 open 時に再開する。
- `DROP INDEX` も同様に、定義を外した後の索引 keyspace 消去を
  `.drop-cleanup` ジャーナル経由で再開できる。
- `__unique_` で始まる暗黙の `UNIQUE` 索引は `DROP` できない（`ConstraintViolation`）。
  制約を消すにはテーブルを再作成する。

```sql
DROP TABLE IF EXISTS tmp_data;
DROP INDEX idx_dept;
```

---

## 12. トランザクション

### SQL 文としての BEGIN / COMMIT / ROLLBACK

`BEGIN` / `COMMIT` / `ROLLBACK` は SQL 文として受理され、**暗黙の transaction を明示的に制御する**。

```sql
BEGIN;
INSERT INTO t VALUES (1, 'a');
INSERT INTO t VALUES (2, 'b');
COMMIT;          -- 両方を確定

BEGIN;
DELETE FROM t WHERE id = 1;
ROLLBACK;        -- 削除を取り消し
```

- `BEGIN` は新しい transaction を始める（既に transaction 中なら何もしない）。
  `BEGIN TRANSACTION` / `START TRANSACTION` も同じ。
  `COMMIT WORK` / `COMMIT TRANSACTION` / `END` は `COMMIT`、
  `ROLLBACK WORK` / `ROLLBACK TRANSACTION` は `ROLLBACK` の別綴り。
  ライブラリ API の `Database.Begin()` も `Commit()` / `Rollback()` と同じく `Result` を返す。
- 識別子（テーブル・列・索引・別名）は**大文字小文字を区別しない**。
- `COMMIT` はバッファをエンジンへ流す。
- `ROLLBACK` はバッファを捨てる。
- 1 transaction の書き込みバッファは管理メモリの概算 256MiB が上限。
  超えると `CapacityError` になる（DDL の一括処理は除く）。
- **DDL（`CREATE TABLE` / `DROP TABLE` / `CREATE INDEX` / `DROP INDEX`）は明示 transaction 中には実行できない**
  （即時反映のため）。
  `COMMIT` または `ROLLBACK` してから実行する。
- 1 つの `Execute` / スクリプト内で複数文を `;` 区切りで渡した場合、`BEGIN` 以降は同じ transaction で実行される。
- ライブラリ API の `Database.Begin()` / `Commit()` / `Rollback()` と等価。

### 暗黙のトランザクション

`Begin()` していない状態の `Execute` は、**1文ごとに**暗黙のトランザクションを開始して自動 commit する。
任意の SQL を実行するには `Execute` を使う。

```fsharp
// ok は Result の中身を取り出す例示用ヘルパー。
let ok =
    function
    | Ok v -> v
    | Error e -> failwith (DbError.toDisplayString e)

let db, _ = ok (db.Execute "INSERT INTO t VALUES (1, 10)")   // 1 文 = 1 commit
```

`Execute` に `;` 区切りで複数文を渡した場合も、`BEGIN` を張っていない限り 1文ごとに commit される。

### 明示的なトランザクション（ライブラリ API）

```fsharp
open RykDb.Core

// ok は Result の中身を取り出す例示用ヘルパー。
let ok =
    function
    | Ok v -> v
    | Error e -> failwith (DbError.toDisplayString e)

let db  = ok (Database.OpenWith "./mydata")
let db1 = ok (db.Begin())                          // 明示トランザクション開始（Result）
let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 10)")
let db3 = ok (db2.Commit())                        // 確定
// または
let db4 = db2.Rollback()                           // 破棄
```

- ハンドルは**不変**。
  戻り値の `Database` を受け取り直さないと書き込みが見えない。
- 入れ子の `Begin()` は何もしない
  （既にトランザクション中なら `this` を返す）。
- `Close()` 後のハンドルは破棄する。
  閉じた状態はハンドルに記録されないため同じハンドルを `Execute` すると WAL が再作成される
  （二重 `Close` は安全）。
  flush / WAL 削除 / `.rykdb.lock` の解放は**最新のハンドル**の `Close()` で行う。
  古いハンドルの `Close()` は no-op（生きている新しいハンドルのロックを外さない）。
- `Database` は `IDisposable`。`use db = ...` / `Dispose()` は `Close()` と同じ経路
  （進行中 transaction の登録も外れる）。
- **同じ DB を別プロセスから開くことはできない**。
  `Database.OpenWith` は `<root>/.rykdb.lock` を排他で押さえ、
  取れなければ `IoError`（`データベース <root> は別のプロセスが開いています（.rykdb.lock）`）を返す。
- **同じ root を同じプロセスで二度開くこともできない**。
  2回目の `Database.OpenWith` は `IoError`
  （`データベース <root> は同じプロセスで既に開かれています。Close してから開き直してください`）を返す。
  別の `RowStore` ができて memtable が分かれ、一方の commit が他方から見えないため。
  `Close()` してから開き直すこと。
- snapshot はトランザクション開始時に固定される
  （repeatable read）。
- 分離レベルは Snapshot Isolation（repeatable read）で固定。
  `BEGIN ISOLATION LEVEL ...` / `SET TRANSACTION ...` による指定は未対応。
- `SAVEPOINT` / `ROLLBACK TO SAVEPOINT` / `RELEASE SAVEPOINT` と読み取り専用トランザクションは未対応（§14）。
- 自分の snapshot 取得後に別トランザクションが同じキーを commit していた場合、後続側は `ConflictError` になる
  （first-committer-wins）。

### CLI のドットコマンド

| コマンド    | 効果                             |
| ----------- | -------------------------------- |
| `.begin`    | transaction を開始               |
| `.commit`   | 開いているトランザクションを確定 |
| `.rollback` | 開いているトランザクションを破棄 |

```
db> .begin
(begin しました)
db> INSERT INTO people VALUES (9, 'eve', 50, 1, 1.0);
db> .rollback
(rollback しました)
```

---

## 13. エラー

すべての失敗は `DbError` として返る（例外を制御フローに使わない）。

| 分類                  | 意味                           | 例                                      |
| --------------------- | ------------------------------ | --------------------------------------- |
| `SyntaxError`         | 文法を満たさない               | `JOIN には FROM が必要です`             |
| `NameError`           | 名前解決・関数の失敗           | `テーブル 'x' が存在しません`           |
| `TypeError`           | 型の不一致・不可能な変換       | `''abc'' は 'INTEGER' に変換できません` |
| `ConstraintViolation` | `NOT NULL`・主キー欠落・0 除算 | `NOT NULL 制約に違反: t.n`              |
| `NotFound`            | 存在しないテーブル・索引・行   | `テーブル nosuch が存在しません`        |
| `AlreadyExists`       | 同名の作成                     | `テーブル t は既に存在します`           |
| `CapacityError`       | memtable / バッファの上限      | ―                                       |
| `CorruptionError`     | チェックサム不一致             | ―                                       |
| `ConflictError`       | MVCC の write-write 競合       | ―                                       |
| `IoError`             | ファイル I/O の失敗            | ―                                       |
| `InternalError`       | 内部不整合（バグの兆候）       | ―                                       |

### 表示形式

構文エラーには行番号が付く。

```
エラー: JOIN には FROM が必要です (位置 1行)
```

### よく見るメッセージ（実測）

| SQL                                        | メッセージ                                                                |
| ------------------------------------------ | ------------------------------------------------------------------------- |
| `SELECT 1 JOIN t ON 1 = 1;`                | `JOIN には FROM が必要です (位置 1行)`                                    |
| `SELECT *;`                                | `FROM のない SELECT では * を使えません（列を明示してください）`          |
| `SELECT * FROM t WHERE id > 1 AND;`        | `式が必要ですが見つかりませんでした (位置 1行)`                           |
| `SELECT 'abc;`                             | `unclosed string (位置 1行)`                                              |
| `SELECT id FROM nosuchtable;`              | `テーブル 'nosuchtable' が存在しません`                                   |
| `SELECT nosuchcol FROM t;`                 | `'nosuchcol' は列でも関数でもありません`                                  |
| `CREATE TABLE t (a INTEGER);`              | `テーブル t に主キーがありません`                                         |
| `CREATE TABLE t (...);`（重複）            | `テーブル t は既に存在します`                                             |
| `CREATE INDEX ix ON t (nosuchcol);`        | `テーブル t に列 nosuchcol がありません`                                  |
| `SELECT id FROM t JOIN t ON t.id = t.id;`  | `テーブル t が重複しています（エイリアスを付けてください）`               |
| `DROP TABLE nosuch;`                       | `テーブル nosuch が存在しません`                                          |
| `INSERT INTO t (id) VALUES (1, 2);`        | `INSERT の列数(1)と値の数(2)が合いません`                                 |
| `INSERT INTO t VALUES (1, NULL, …);`       | `NOT NULL 制約に違反: t.n`                                                |
| `INSERT INTO t VALUES (5, 'z', 'abc');`    | `''abc'' は 'INTEGER' に変換できません`                                   |
| `SELECT (id) / 0 FROM t;`                  | `0 での除算は定義されていません`                                          |
| `SELECT LENGTH(id, n) FROM t;`             | `LENGTH 関数は 1 個の引数が必要ですが、2 個渡されました`                  |
| `SELECT NOSUCHFN(id) FROM t;`              | `未知のスカラー関数 'NOSUCHFN' です`                                      |
| `SELECT active = 1 FROM u;`                | `型 'BOOLEAN' と 'INTEGER' は比較できません`                              |
| `SELECT ROUND(name) FROM t;`               | `ROUND は数値を受け付けますが 'abc' です`                                 |
| `SELECT id, (SELECT id, v FROM t) FROM t;` | `サブクエリは 1 列だけを返す必要があります（2 列）`                       |
| `SELECT id, (SELECT v FROM t) FROM t;`     | `スカラーサブクエリが 2 行以上を返しました`                               |
| `SELECT id FROM (SELECT id FROM t) x;`     | `FROM のサブクエリ（派生テーブル）は未対応です (位置 1行)`                |
| `SAVEPOINT s;`                             | `文の開始として解釈できません (位置 1行)`                                 |
| `UPDATE t SET v = 1 FROM u;`               | `文の区切りには ; が必要です（複数文は ; で区切ってください） (位置 1行)` |

未対応の後置句（`DELETE ... LIMIT`、`ON CONFLICT`、ウィンドウ関数の `OVER (...)` など）は
専用のメッセージにならず、`;` 区切りのエラーとして報告される（§14）。

---

## 14. 既知の制限

### 構文が未対応のもの

- **相関サブクエリ**（外側の列を参照するサブクエリ）
- **FROM の派生テーブル**（`FROM (SELECT ...) AS x`）
- `UNION` / `UNION ALL` / `INTERSECT` / `EXCEPT`
- CTE（`WITH`）、再帰クエリ
- ウィンドウ関数（`OVER` / `PARTITION BY` / `ROW_NUMBER` …）
- `RIGHT JOIN` / `FULL OUTER JOIN` / `CROSS JOIN` / `USING`
- 自然結合（`NATURAL JOIN`）、`INSERT ... SELECT`
- カンマ結合（`FROM a, b`）
- `ON CONFLICT` / `ON DUPLICATE KEY UPDATE`（upsert）
- `UPDATE ... FROM` / `UPDATE ... LIMIT` / `DELETE ... LIMIT`
- `DELETE ... USING`（明示 `SyntaxError`）
- `SAVEPOINT` / `ROLLBACK TO SAVEPOINT` / `RELEASE SAVEPOINT`
- 分離レベルの指定（`BEGIN ISOLATION LEVEL ...` / `SET TRANSACTION ...`）と
  読み取り専用トランザクション（分離レベルは Snapshot Isolation 固定）
- `ALTER TABLE`（列の追加・削除・型変更）
- `TRUNCATE`
- `EXPLAIN`、`RETURNING`
- `CREATE TABLE ... DEFAULT (SELECT ...)`（構文は受理されるが INSERT 時に `SyntaxError`。§7.9）
- ユーザ定義関数・トリガー・ビュー・ストアドプロシージャ
- パーティショニング、圧縮、暗号化
- 複数プロセスからの同時アクセス
  （2つ目のプロセスは `IoError` で開けない。§12.2「プロセス間ロック」）

未対応の集合演算・JOIN の綴りは、テーブル別名として解釈されず **`SyntaxError`** になる。
以前は `FROM t UNION SELECT ...` が「別名 + 別文」に分解され、
エラーにならずに複数文の集計だけが返っていた。
これらの語を裸のテーブル別名に使うことはできない
（`AS` を付ければ使える）。

### 実装上の制約

| 項目                | 内容                                                                                                                                |
| ------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| 索引                | 単一列・複合索引の前置等値（1列以上）、複合索引の前置等値 + 次列の範囲、`LIKE 'prefix%'` の範囲                                     |
| `LIMIT` / `OFFSET`  | 0 以上の定数式のみ（行ごとの列参照・負数はエラー）。`LIMIT 0` は「0行」                                                             |
| 型引数              | `VARCHAR(32)` は宣言のみ。切り詰めなし                                                                                              |
| ソート              | 外部マージ（一時ファイルに溢らす）。バジェット内はメモリのみ                                                                        |
| `SELECT DISTINCT`   | 符号化キーの集合。既定 256MiB で上限があり、超過は `CapacityError`                                                                  |
| `JOIN`              | 入れ子ループ / ハッシュ結合は右側を、マージ結合は同じキーの行だけを保持。管理メモリ換算 256MiB で上限があり、超過は `CapacityError` |
| `GROUP BY`          | 行を抱えず集約。グループ数の上限は 400万                                                                                            |
| `AVG`               | `SUM / COUNT` を実行時に計算                                                                                                        |
| `UPDATE` / `DELETE` | `FROM` / `USING` / `LIMIT` 相当の句がない                                                                                           |
| DML の別名          | `UPDATE t AS x` / `DELETE FROM t x` はパースできるが、`WHERE x.id` は `NameError`（対象行が無ければ 0 行）。列は実名で修飾する      |
| トランザクション    | 分離レベルは Snapshot Isolation 固定。`SAVEPOINT` と読み取り専用トランザクションは未対応（§12）                                     |
| DDL と transaction  | 明示 transaction 中は DDL を実行できない（即時反映のため）                                                                          |
| `ORDER BY`          | 列参照・式・序数（1 始まり）が使える。同値行の順序は保証しない                                                                      |

### 演算の注意

- `INTEGER / INTEGER` は切り捨て（`10 / 4` → `2`）。実数にしたいなら `10 / 4.0`。
- `UPPER(NULL)` / `LOWER(NULL)` / `TRIM(NULL)` は `NULL` を返す。
- `DATE` と `TIMESTAMP` は相互比較できるが、`DATE` は UTC 午前 0時として扱われる。
- `REAL` の `NaN` は `REAL` 同士の比較でのみ特別扱いされる（`NaN` が最大、`NaN = NaN` は真）。
  `NaN` と `INTEGER` / `DECIMAL` を比較すると `TypeError`（`NaN や ±Infinity は数値比較できません`）になる。
  `NaN` / `±Infinity` は `DECIMAL` に変換できない。

---

## 15. 付録: 完全な例

### 15.1 スキーマ作成から集計まで

```sql
CREATE TABLE depts (
  id   INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE people (
  id       INTEGER PRIMARY KEY,
  name     TEXT NOT NULL,
  dept_id  INTEGER,
  age      INTEGER,
  salary   DECIMAL,
  hired    DATE,
  active   BOOLEAN DEFAULT TRUE
);

INSERT INTO depts VALUES (1, 'eng'), (2, 'sales');
INSERT INTO people VALUES
  (1, 'alice', 1, 30, 100.5,  '2024-01-15', TRUE),
  (2, 'bob',   1, 25, 200.25, '2023-06-01', FALSE),
  (3, 'carol', 2, 41, 150.0,  '2022-03-10', TRUE);

CREATE INDEX idx_people_dept ON people (dept_id);
CREATE INDEX idx_people_name ON people (name);
```

```sql
-- 索引付きポイント検索
SELECT id, name FROM people WHERE dept_id = 2;
-- id | name
--  3 | 'carol'

-- 結合
SELECT p.name, d.name AS dept
  FROM people p
  JOIN depts d ON p.dept_id = d.id
 ORDER BY p.name;

-- LEFT JOIN（所属先が無くても残す）
SELECT p.name, d.name AS dept
  FROM people p
  LEFT JOIN depts d ON p.dept_id = d.id + 100
 ORDER BY p.name;

-- 集計
SELECT d.name AS dept, COUNT(*) AS headcount, AVG(p.salary) AS mean
  FROM depts d
  JOIN people p ON p.dept_id = d.id
 GROUP BY d.name
HAVING COUNT(*) >= 1
 ORDER BY d.name;

-- 条件分岐
SELECT name, CASE WHEN age > 30 THEN 'senior' ELSE 'junior' END AS band
  FROM people
 ORDER BY id;

-- ページング
SELECT id, name FROM people ORDER BY id LIMIT 2 OFFSET 1;

-- 更新と削除
UPDATE people SET age = age + 1 WHERE id = 1;
DELETE FROM people WHERE id = 3;
```

### 15.2 エラーを起こす例

```sql
CREATE TABLE t (a INTEGER);            -- 主キーがない
-- エラー: テーブル t に主キーがありません

SELECT * FROM nosuch;                  -- 存在しないテーブル
-- エラー: テーブル 'nosuch' が存在しません

CREATE TABLE u (id INTEGER PRIMARY KEY, n TEXT NOT NULL);
INSERT INTO u VALUES (1, NULL);        -- NOT NULL 違反
-- エラー: NOT NULL 制約に違反: u.n

SELECT (id) / 0 FROM u;                -- 0 除算
-- エラー: 0 での除算は定義されていません

SELECT active = 1 FROM u;              -- 型の比較
-- エラー: 型 'BOOLEAN' と 'INTEGER' は比較できません
```
