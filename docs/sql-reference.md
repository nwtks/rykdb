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
                   FROM table_ref
                   { join_clause }
                   [ WHERE expr ]
                   [ GROUP BY expr {"," expr} ]
                   [ HAVING expr ]
                   [ ORDER BY sort_key {"," sort_key} ]
                   [ LIMIT expr [ OFFSET expr] ]

select_item    ::= "*" | ident "." "*" | expr [AS ident]
join_clause    ::= [INNER | LEFT] JOIN table_ref ON expr
sort_key       ::= expr [ASC | DESC]

insert         ::= INSERT INTO table_ref [ "(" column_list ")" ] VALUES row {"," row}
row            ::= "(" expr {"," expr} ")"
update         ::= UPDATE table_ref SET assignment {"," assignment} [ WHERE expr ]
delete         ::= DELETE FROM table_ref [ WHERE expr ]

ddl            ::= CREATE TABLE [IF NOT EXISTS] ident "(" table_element {"," table_element} ")"
                | CREATE [UNIQUE] INDEX ident ON ident "(" column_list ")"
                | DROP TABLE  [IF EXISTS] ident
                | DROP INDEX  [IF EXISTS] ident
table_element  ::= column_def | PRIMARY KEY "(" column_list ")" | UNIQUE "(" column_list ")"
column_def     ::= ident data_type [type_args] [column_constraint {column_constraint}]
column_constraint ::= NOT NULL | PRIMARY KEY | UNIQUE | DEFAULT expr

txn            ::= BEGIN | COMMIT | ROLLBACK
```

> **注意** `BEGIN` / `COMMIT` / `ROLLBACK` は SQL 文として transaction を制御する（§12）。
> 詳しくは [12. トランザクション](#12-トランザクション)。

文は `;` で区切れる（末尾の `;` は省略可）。
`Database.Execute` は複数文を順に実行し、**最後の `SELECT` の結果**を返す。

---

## 2. 字句

### コメント

| 記法        | 範囲                                                      |
| ----------- | --------------------------------------------------------- |
| `--`        | 行末まで                                                  |
| `/* ... */` | **入れ子可**（`/* a /* b */ c */` として `c` もコメント） |

### 識別子

- 通常: `[A-Za-z_][A-Za-z0-9_]*`
- 引用: `"..."`（`""` で `"` をエスケープ）

**大文字小文字は区別しない**（テーブル名・列名・索引名の比較は `OrdinalIgnoreCase`）。
英字のみが識別子の開始文字（数字や `_` 始まりの識別子は不可）。

### 予約語

```
SELECT  FROM    WHERE   INSERT  INTO    VALUES  UPDATE  SET     DELETE
CREATE  TABLE   INDEX    DROP    ON      AND     OR      NOT     NULL
IS      IN      BETWEEN  LIKE    GROUP   BY      HAVING  ORDER   ASC
DESC    LIMIT   OFFSET   JOIN    INNER   LEFT    AS      DISTINCT
BEGIN   COMMIT  ROLLBACK PRIMARY KEY     UNIQUE  DEFAULT CASE    WHEN
THEN    ELSE    END      EXISTS  IF      TRUE    FALSE
```

`TRUE` / `FALSE` は予約語一覧（`Lexer.keywords`）には無いが、`Keyword` として特別扱いされる。

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
| `DATE` `TIMESTAMP`         | 相互比較可（`DATE` は UTC 午中の `TIMESTAMP` として扱う）                                                                   |
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
INSERT INTO t VALUES (1, other_col);  -- SyntaxError
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
x IN (1, 2, 3)      x NOT IN (1, 2, 3)
x BETWEEN 1 AND 10   x NOT BETWEEN 1 AND 10
name LIKE 'a%'       name NOT LIKE 'a%'
```

- `BETWEEN` は両端を含む（`1 <= x <= 10`）。
- `IN` の要素は**リテラル式のリスト**（サブクエリ不可）。
- `LIKE` のワイルドカード:

| 記号 | 意味                     |
| ---- | ------------------------ |
| `%`  | 0 文字以上の任意の文字列 |
| `_`  | ちょうど 1 文字          |

`ESCAPE` 句、`\` によるエスケープ、大文字小文字を区別しないモード（`ILIKE`）は**ない**。
パターンも被比較も文字列表現（`toDisplayString`）で照合し、引数が `NULL` なら結果は `NULL`。

```sql
SELECT name FROM people WHERE name LIKE 'a%';     -- a で始まる
SELECT name FROM people WHERE name LIKE '_ob';     -- 2文字目が o、3文字目が b
SELECT name FROM people WHERE name LIKE 'a_c';     -- 3文字、2文字目が任意
```

### 6.5 集約関数

| 関数       | 戻り値                         | 空のグループ | `NULL` の扱い |
| ---------- | ------------------------------ | ------------ | ------------- |
| `COUNT(*)` | `INTEGER`                      | `0`          | 数える        |
| `COUNT(x)` | `INTEGER`                      | `0`          | **数えない**  |
| `SUM(x)`   | `INTEGER` / `DECIMAL` / `REAL` | `NULL`       | 無視          |
| `AVG(x)`   | 常に `REAL`                    | `NULL`       | 無視          |
| `MIN(x)`   | 入力と同じ型                   | `NULL`       | 無視          |
| `MAX(x)`   | 入力と同じ型                   | `NULL`       | 無視          |

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

- `SUM` / `AVG` は `SUM / COUNT` を実行時に計算する。
- `MIN` / `MAX` は文字列・日付・時刻にも使える。
- `GROUP BY` を書かない `SELECT SUM(x) FROM t` は表全体の 1グループとして集計される。
- `GROUP BY` / 集約関数を使うときに `SELECT *` は書けない（列を明示する）。
- 集約も `GROUP BY` も無い `HAVING` は `SyntaxError`。
- `SUM` / `AVG` は `INTEGER` / `DECIMAL` / `REAL` が混在しても合計を落とさない（REAL で返る）。
  入力が 0行でも 1行を返す（`COUNT(*)` は `0`、`SUM` / `AVG` / `MIN` / `MAX` は `NULL`）。
- 集約関数のエラーは**伝播する**。
  文字列列に `SUM` を掛けると `NULL` が返り、エラーにはならない。
- `COUNT(DISTINCT x)` は**重複排除する**。

### 6.6 スカラー関数

関数名は大文字小文字を区別しない。

| 関数                |   引数 | 戻り値               | `NULL` の扱い    | 備考                                                                 |
| ------------------- | -----: | -------------------- | ---------------- | -------------------------------------------------------------------- |
| `UPPER(x)`          |      1 | `TEXT`               | 大文字化         |
| `LOWER(x)`          |      1 | `TEXT`               | 小文字化         |
| `TRIM(x)`           |      1 | `TEXT`               | 前後の空白を削除 |
| `LENGTH(x)`         |      1 | `INTEGER`            | `NULL`           | UTF-16 文字数                                                        |
| `ABS(x)`            |      1 | 入力と同じ数値型     | `NULL`           | 非数値はエラー                                                       |
| `ROUND(x)`          |      1 | `REAL`               | `NULL`           | 0.5 は 1 へ（四捨五入・`AwayFromZero`）。**`INTEGER` / `REAL` のみ** |
| `COALESCE(a, b, …)` | 1 以上 | 最初の非 `NULL` の型 | 全滅なら `NULL`  | 引数数は自由                                                         |

- **未知の関数**は `未知のスカラー関数 '…' です`。
- **引数数が違う**場合は `… 関数は 1個の引数が必要ですが、2個渡されました`。
  ただし空の引数リスト（`LENGTH()`）は関数の前に構文エラーになる。
- `DECIMAL` 列に `ROUND` を掛けると `ROUND は数値を受け付けますが 100.5 です` になる。
  `ROUND(x * 1.0)` のように `REAL` に変換してから渡す。

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
  FROM テーブル [AS 別名]
       [[INNER | LEFT] JOIN テーブル [AS 別名] ON 条件] …
  [WHERE 条件]
  [GROUP BY 式 [, 式 …]]
  [HAVING 条件]
  [ORDER BY 式 [ASC|DESC] [, …]]
  [LIMIT 整数 [OFFSET 整数]];
```

- **`FROM` は省略できない**。
  `SELECT 1` は `FROM が必要ですが見つかりませんでした` になる。
- `JOIN` は `INNER JOIN` / `JOIN` / `LEFT JOIN` の 3 種。
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
- `ORDER BY` のキーは**列の直接参照**に限る
  （`ORDER BY` の式に演算子を書くと「ORDER BY には列の直接参照だけを書けます」になる）。

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

| 条件                                                                          | アルゴリズム             |
| ----------------------------------------------------------------------------- | ------------------------ |
| 左が小さい（`LIMIT` / 主キー等値 / `VALUES` で 32行以下）かつ右の結合列に索引 | インデックス入れ子ループ |
| 左×右の等値条件がある（上記以外）                                             | ハッシュ結合             |
| 等値条件がない（非等値など）                                                  | 入れ子ループ             |

- 等値キーは**列の型が一致する**ときだけ使う。
  型が違うと入れ子ループに落ちる。
- `NULL` は `=` で一致しない（ハッシュ/索引とも NULL キーは結合しない）。
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

ソートはメモリ上に実体化してから `Array.sortWith`（不安定ソート）で並べる。
同値な行の順序は保証されない（`ORDER BY` のキーが一意でないとき）。

### 7.7 `LIMIT` / `OFFSET`

- 値は **0 以上の整数リテラル**（式・負数・範囲外は `SyntaxError`）。
- 評価は最後に 1回だけ。
  `OFFSET` → `LIMIT` の順で適用される。
- `LIMIT` だけの形か `LIMIT n OFFSET m` の形だけ書ける
  （`OFFSET` 単独は不可）。
- `LIMIT 0` は「0 行」を返す（スキャン自体を実行しない）。
- **単純な全走査**（`WHERE` / `ORDER BY` / `GROUP BY` / `DISTINCT` / 結合なし）では、
  `OFFSET + LIMIT` の件数をストレージへ降ろす。
  ストレージは先頭 `OFFSET + LIMIT` 行だけをデコードし、必要になるまで後続の SSTable を開かない。
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

---

## 8. INSERT

```sql
INSERT INTO テーブル [ (列 [, 列 …]) ] VALUES (式 [, 式 …]) [, (式 [, …]) …];
```

### 挙動

- **列リストを省略**すると、テーブル定義の順に値を並べる。
- **列リストに無い列**は `DEFAULT` を使い、無ければ `NULL` になる。
- **値式は定数のみ**（列参照不可）。
- 値は列の型へ**暗黙変換**される（[§3](#暗黙の型変換sqlvaluecoerce)）。
- `NOT NULL` 制約は行ごとに検証される。
- **1つの `INSERT` は 1トランザクション**（途中で失敗したら何も残らない）。

### 主キー重複はエラー

**エラーになる**（upsert しない）。
複数列の主キーも同じで、値の組が一致すればエラーになる。

### `UNIQUE` は検査される

`UNIQUE` 制約・`UNIQUE` 索引は重複を検査する。

```sql
INSERT INTO people VALUES (1, 'alice2', 30, 1, 1.0);  -- id=1 が既に存在するのでエラー
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
- `WHERE` を省略すると全行が対象。
- 条件に合う行すべてが更新される（`LIMIT` 相当の句がない）。
- 影響行数は `StatementResult.RowsAffected` に入る。
- 索引があるテーブルでは、更新のたびにすべての索引のエントリが書き直される。

### 主キー列は更新できない

主キー列を `SET` するとエラーになる（幽霊行を防ぐため）。
主キーを変えたい場合は `DELETE` + `INSERT` を使う。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, n TEXT);
INSERT INTO t VALUES (1,'a'),(2,'b');
UPDATE t SET id = 100 WHERE id = 1;   -- エラー

SELECT id, n FROM t ORDER BY id;
-- 1, 'a'      ← 消えない
-- 2, 'b'
-- 100, 'a'    ← 複製された
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
| `PRIMARY KEY` | 列・テーブル | **必須**。`NOT NULL` も付く                               |
| `UNIQUE`      | 列・テーブル | 重複を検査する                                            |
| `DEFAULT`     | 列           | `INSERT` で値を省略したときに使われる（定数式のみ）       |

`DEFAULT` はカタログに保存され、DB を再オープンした後も使われる。

- **主キーは省略できない**。
  1つも無いと`テーブル t に主キーがありません` になる。
- 主キーは複数列にできる。
  値は列の宣言順に連結される。
- `PRIMARY KEY` はテーブル制約が列レベルより優先される。
- `IF NOT EXISTS` を付けると、既存テーブルなら**何もしずに成功**する。
- `IF NOT EXISTS` と `IF EXISTS` の両方の綴りを受け付ける。

#### 列定義で型を省略する

型を書かずに制約だけを書く構文も受け付ける（型は `TEXT` になる）。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, name NOT NULL);
```

### 11.2 CREATE INDEX

```sql
CREATE [UNIQUE] INDEX 索引名 ON テーブル (列 [, 列 …]);
```

- **既存の行はすべて索引に投入される**（backfill）。
- backfill は **プロセス内で排他**。
  同じ DB で実行中の transaction があれば `ConflictError`（`実行中の transaction があります。…`）を返すので、
  COMMIT / ROLLBACK してから実行する。
  他の文は backfill が終わるまで待つ。
- 索引は**独立した LSM ツリー**（keyspace `i:<索引名>`）に格納される。
- `UNIQUE` は重複を検査する。
- 同名の索引が既にあれば `索引 <名前> は既に存在します`。
- 索引する列が存在しない場合は `テーブル <t> に列 <c> がありません`。
- `IF NOT EXISTS` は**ない**。

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
- `DROP TABLE` はテーブル本体と、その索引 keyspace のエントリに tombstone を書いてからカタログ定義を外す
  （行データは LSM の compaction で再利用されるため即座に解放される）。
  カタログだけを外すと、同名で再作成したときに旧データが復活するため、tombstone を先に確定させる。
- `DROP INDEX` も索引 keyspace の全エントリに tombstone を書いてから定義を外す。

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
  ライブラリ API の `Database.Begin()` も `Commit()` / `Rollback()` と同じく `Result` を返す。
- 識別子（テーブル・列・索引・別名）は**大文字小文字を区別しない**。
- `COMMIT` はバッファをエンジンへ流す。
- `ROLLBACK` はバッファを捨てる。
- **DDL（`CREATE TABLE` / `DROP TABLE` / `CREATE INDEX` / `DROP INDEX`）は明示 transaction 中には実行できない**
  （即時反映のため）。
  `COMMIT` または `ROLLBACK` してから実行する。
- 1 つの `Execute` / スクリプト内で複数文を `;` 区切りで渡した場合、`BEGIN` 以降は同じ transaction で実行される。
- ライブラリ API の `Database.Begin()` / `Commit()` / `Rollback()` と等価。

### 暗黙のトランザクション

`Begin()` していない状態の `Execute` / `Query` は、**1文ごとに**暗黙のトランザクションを開始して自動 commit する。
`Query` は **SELECT 専用**（DDL / DML / `BEGIN` / `COMMIT` / `ROLLBACK` は `SyntaxError`）。
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

let db  = ok (Database.openWith "./mydata")
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
- `Query` は SELECT 専用。DDL / DML / transaction 制御は `Execute` を使う。
- `Close()` 後のハンドルは破棄する。
  閉じた状態はハンドルに記録されないため同じハンドルを `Execute` すると WAL が再作成される
  （二重 `Close` は安全）。
- snapshot はトランザクション開始時に固定される
  （repeatable read）。
- 同一キーへ別トランザクションが未 commit のエントリを持つ場合、後続側は `ConflictError` になる
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
| `SyntaxError`         | 文法を満たさない               | `FROM が必要ですが見つかりませんでした` |
| `NameError`           | 名前解決・関数の失敗           | `テーブル 'x' が存在しません`           |
| `TypeError`           | 型の不一致・不可能な変換       | `'abc' は 'INTEGER' に変換できません`   |
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
エラー: FROM が必要ですが見つかりませんでした (位置 3 行)
```

### よく見るメッセージ（実測）

| SQL                                       | メッセージ                                                  |
| ----------------------------------------- | ----------------------------------------------------------- |
| `SELECT 1;`                               | `FROM が必要ですが見つかりませんでした (位置 1 行)`         |
| `SELECT * FROM t WHERE id > 1 AND;`       | `式が必要ですが見つかりませんでした (位置 1 行)`            |
| `SELECT 'abc;`                            | `文字列が閉じられていません (位置 1 行)`                    |
| `SELECT id FROM nosuchtable;`             | `テーブル 'nosuchtable' が存在しません`                     |
| `SELECT nosuchcol FROM t;`                | `'nosuchcol' は列でも関数でもありません`                    |
| `CREATE TABLE t (a INTEGER);`             | `テーブル t に主キーがありません`                           |
| `CREATE TABLE t (...);`（重複）           | `テーブル t は既に存在します`                               |
| `CREATE INDEX ix ON t (nosuchcol);`       | `テーブル t に列 nosuchcol がありません`                    |
| `SELECT id FROM t JOIN t ON t.id = t.id;` | `テーブル t が重複しています（エイリアスを付けてください）` |
| `DROP TABLE nosuch;`                      | `テーブル nosuch が存在しません`                            |
| `INSERT INTO t (id) VALUES (1, 2);`       | `INSERT の列数(1)と値の数(2)が合いません`                   |
| `INSERT INTO t VALUES (1, NULL, …);`      | `NOT NULL 制約に違反: t.n`                                  |
| `INSERT INTO t VALUES (5, 'z', 'abc');`   | `'abc' は 'INTEGER' に変換できません`                       |
| `SELECT (id) / 0 FROM t;`                 | `0 での除算は定義されていません`                            |
| `SELECT LENGTH(id, n) FROM t;`            | `LENGTH 関数は 1 個の引数が必要ですが、2 個渡されました`    |
| `SELECT NOSUCHFN(id) FROM t;`             | `未知のスカラー関数 'NOSUCHFN' です`                        |
| `SELECT active = 1 FROM u;`               | `型 'BOOLEAN' と 'INTEGER' は比較できません`                |
| `SELECT ROUND(salary) FROM t;`            | `ROUND は数値を受け付けますが 100.5 です`                   |

---

## 14. 既知の制限

### 構文が未対応のもの

- サブクエリ、`UNION` / `UNION ALL` / `INTERSECT` / `EXCEPT`
- CTE（`WITH`）、再帰クエリ
- ウィンドウ関数（`OVER` / `PARTITION BY` / `ROW_NUMBER` …）
- `RIGHT JOIN` / `FULL OUTER JOIN` / `CROSS JOIN` / `USING`
- 自然結合、`INSERT ... SELECT`
- `ALTER TABLE`（列の追加・削除・型変更）
- `TRUNCATE`
- `EXPLAIN`、`RETURNING`
- ユーザ定義関数・トリガー・ビュー・ストアドプロシージャ
- パーティショニング、圧縮、暗号化
- 複数プロセスからの同時アクセス（1 プロセス前提）

### 実装上の制約

| 項目                | 内容                                                                                            |
| ------------------- | ----------------------------------------------------------------------------------------------- |
| `SELECT` の `FROM`  | **省略不可**。定数だけの SELECT は書けない                                                      |
| 索引                | 単一列・複合索引の前置等値（1列以上）、複合索引の前置等値 + 次列の範囲、`LIKE 'prefix%'` の範囲 |
| `LIMIT` / `OFFSET`  | 0 以上の整数リテラルのみ（式・負数はエラー）。`LIMIT 0` は「0 行」                              |
| 型引数              | `VARCHAR(32)` は宣言のみ。切り詰めなし                                                          |
| ソート              | メモリ上に実体化。spilling する外部マージは未実装                                               |
| `AVG`               | `SUM / COUNT` を実行時に計算                                                                    |
| `UPDATE` / `DELETE` | `LIMIT` 相当の句がない                                                                          |
| DDL と transaction  | 明示 transaction 中は DDL を実行できない（即時反映のため）                                      |
| `ORDER BY`          | 同値行の順序は保証しない                                                                        |

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
