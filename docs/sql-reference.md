# SQL リファレンス

RykDb が受け付ける SQL 文法の一覧です。
**実装ソースから機械的に抜き出した「実際に通る範囲」**を記載しています。
標準 SQL と食い違う部分は［制限］印を付けて明示しています。

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

> **注意** `BEGIN` / `COMMIT` / `ROLLBACK` は構文としては受理されますが、**実行しても何も起きません**。
> 詳しくは [12. トランザクション](#12-トランザクション)。

文は `;` で区切れます（末尾の `;` は省略可）。
`Database.Execute` は複数文を順に実行し、**最後の `SELECT` の結果**を返します。

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
英字のみが識別子の開始文字です（数字や `_` 始まりの識別子は不可）。

### 予約語

```
SELECT  FROM    WHERE   INSERT  INTO    VALUES  UPDATE  SET     DELETE
CREATE  TABLE   INDEX    DROP    ON      AND     OR      NOT     NULL
IS      IN      BETWEEN  LIKE    GROUP   BY      HAVING  ORDER   ASC
DESC    LIMIT   OFFSET   JOIN    INNER   LEFT    AS      DISTINCT
BEGIN   COMMIT  ROLLBACK PRIMARY KEY     UNIQUE  DEFAULT CASE    WHEN
THEN    ELSE    END      EXISTS  IF      TRUE    FALSE
```

`TRUE` / `FALSE` は予約語一覧（`Lexer.keywords`）には無いが、`Keyword` として特別扱いされます。

### 記号（演算子・区切り）

長いものから照合されます。

```
<>   !=   <=   >=   ||   =   <   >   +   -   *   /   %   ;   ,   .   (   )
```

### 数値リテラル

| 形                         | 例                        | 型        |
| -------------------------- | ------------------------- | --------- |
| 整数                       | `0` `42` `-7`             | `INTEGER` |
| 実数（小数部か指数部あり） | `1.5` `.5` `1e3` `1.5E-3` | `REAL`    |

**`DECIMAL` のリテラルはありません。** `1.5` は `REAL` です。
厳密な小数を入れたい場合は文字列リテラルで渡し、列の型で変換させてください（§3）。

### 文字列リテラル

```sql
'abc'        -- 通常の文字列
'it''s'      -- ' を 1 つ含む（'' がエスケープ）
```

### バイナリリテラル

```sql
X'0A1B'       -- 大文字の 16 進のみ。バイト数は偶数
```

奇数長・16 進以外の文字が含まれる場合は構文エラーになります。

### 式の深さ上限

入れ子深度は **200** を超えません（`Parser.MaxExpressionDepth`）。
`a + b + c + ...` を 1000 個並べると構文エラーになります。
構文規則は相互再帰なので実行時にフレームが 1 段ぶん積み上がるため、深さで検出して打ち切ります。

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

`VARCHAR(32)` の `(32)` は**読み取られるだけ**で、切り詰めも検証も行いません。
`CHAR(1)` に 100 文字を入れても保存されます。

### 暗黙の型変換（`SqlValue.coerce`）

`INSERT` / `UPDATE` で値を書き込むとき、列の型へ次の変換をかけます。
変換できない場合は `TypeError` になります。

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
| `INTEGER` `REAL` `DECIMAL` | 3 種とも `decimal` に寄せて比較                                                                                             |
| `REAL` の `NaN`            | **`REAL` 同士の比較でのみ**最大。`NaN = NaN` は真。`NaN` と `INTEGER` / `DECIMAL` の比較は `TypeError`（§14「演算の注意」） |
| `TEXT`                     | `StringComparer.Ordinal`（= UTF-8 のバイト順 = コードポイント順）                                                           |
| `BLOB`                     | バイト列の辞書順（短い方が先）                                                                                              |
| `DATE` `TIMESTAMP`         | 相互比較可（`DATE` は UTC 午中の `TIMESTAMP` として扱う）                                                                   |
| `UUID`                     | `Guid` の比較                                                                                                               |
| `BOOLEAN`                  | `FALSE < TRUE`                                                                                                              |

**異なる型の組み合わせは比較できません**（`TypeError`）。
`active = 1`（`BOOLEAN` と `INTEGER`）はエラーになります。

- 数値 3 種の比較順位: `INTEGER` = `REAL` = `DECIMAL` < `DATE` < `TIMESTAMP` < `TEXT` < `BLOB` < `UUID`
- `NULL` は比較できません。SQL の 3 値論理（[§6.2](#62-3-値論理)）として扱われます。

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

**リテラルには `DECIMAL`・`DATE`・`TIMESTAMP`・`UUID` の書式がありません。**
これらは文字列リテラルで書き、列の型で変換させます。

### `INSERT` の値式は定数のみ

`INSERT ... VALUES` の値は **列参照を含む式にできません**。

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

CLI は 1 行目に列名、2 行目に区切り（列名と同じ文字数の `-`）、3 行目以降に `  値, 値, …` を出します。
0 行の `SELECT` でもヘッダの 2 行だけは出ます。
`SELECT` 以外は `(N rows)` の 1 行だけです。

---

## 5. 識別子と名前解決

```sql
SELECT p.id, p.name FROM people p;      -- テーブル別名
SELECT id FROM people p;                -- 修飾は省略できる
SELECT people.id FROM people;           -- 修飾はテーブル実名でもよい
SELECT "Order Id" FROM "My Table";      -- 引用識別子
```

- 修飾なしの列名は `FROM` にあるテーブルの順に探します。
- 結合句で同じ列名が複数の表にあれば、修飾が必須です。

### ［制限］`SELECT` 項目の先頭が裸の識別子・関数呼び出しのとき

`SELECT` の項目は「識別子」「`関数名(...)`」「`*`」をそのまま 1 項目として扱い、**後ろに演算子が続く場合は式として解析されません**。
演算子を使うときは全体を括弧で囲んでください。

```sql
SELECT name || ' (' || dept_id || ')' FROM p;   -- SyntaxError（FROM が必要）
SELECT (name || ' (' || dept_id || ')') FROM p; -- OK

SELECT SUM(d) + 0.5 FROM t;     -- SyntaxError（FROM が必要）
SELECT (SUM(d)) + 0.5 FROM t;   -- OK
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
- 算術演算子と `||` は同じ優先順位で**左結合**です（`1 + 2 || 'x'` は `(1 + 2) || 'x'`）。

### 6.2 3 値論理

比較・`IS NULL` 以外の述語は `TRUE` / `FALSE` / `UNKNOWN` の 3 値です。

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

`WHERE` / `HAVING` では `UNKNOWN` は `FALSE` として扱います。
算術演算・`ABS`・`CASE` の条件など、述語以外は NULL が**そのまま伝播**します。

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

数値の型は次のとおり決まります。

```
INTEGER op INTEGER        -> INTEGER
DECIMAL op DECIMAL        -> DECIMAL
REAL が 1 つでも含まれる  -> REAL
片方が NULL               -> NULL
```

実数除算にしたいときは `10 / 4.0` のように `REAL` を混ぜてください。

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
- `IN` の要素は**リテラル式のリスト**です（サブクエリ不可）。
- `LIKE` のワイルドカード:

| 記号 | 意味                     |
| ---- | ------------------------ |
| `%`  | 0 文字以上の任意の文字列 |
| `_`  | ちょうど 1 文字          |

`ESCAPE` 句、`\` によるエスケープ、大文字小文字を区別しないモード（`ILIKE`）は**ありません**。
パターンも被比較も文字列表現（`toDisplayString`）で照合し、引数が `NULL` なら結果は `NULL` です。

```sql
SELECT name FROM people WHERE name LIKE 'a%';     -- a で始まる
SELECT name FROM people WHERE name LIKE '_ob';     -- 2 文字目が o、3 文字目が b
SELECT name FROM people WHERE name LIKE 'a_c';     -- 3 文字、2 文字目が任意
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

- `SUM` / `AVG` は `SUM / COUNT` を実行時に計算します。
- `MIN` / `MAX` は文字列・日付・時刻にも使えます。
- `GROUP BY` を書かない `SELECT SUM(x) FROM t` は表全体の 1 グループとして集計されます。
- ［制限］**集約関数のエラーは握り潰され `NULL` になります。**
  文字列列に `SUM` を掛けると `NULL` が返り、エラーにはなりません。
- ［制限］**`COUNT(DISTINCT x)` は構文は通りますが重複排除しません**（`COUNT(x)` と同じ結果）。

### 6.6 スカラー関数

関数名は大文字小文字を区別しません。

| 関数                |   引数 | 戻り値               | `NULL` の扱い           | 備考                                                                 |
| ------------------- | -----: | -------------------- | ----------------------- | -------------------------------------------------------------------- |
| `UPPER(x)`          |      1 | `TEXT`               | ［制限］文字列 `"NULL"` | 大文字化                                                             |
| `LOWER(x)`          |      1 | `TEXT`               | ［制限］文字列 `"NULL"` | 小文字化                                                             |
| `TRIM(x)`           |      1 | `TEXT`               | ［制限］文字列 `"NULL"` | 前後の空白を削除                                                     |
| `LENGTH(x)`         |      1 | `INTEGER`            | `NULL`                  | UTF-16 文字数                                                        |
| `ABS(x)`            |      1 | 入力と同じ数値型     | `NULL`                  | 非数値はエラー                                                       |
| `ROUND(x)`          |      1 | `REAL`               | `NULL`                  | 0.5 は 1 へ（四捨五入・`AwayFromZero`）。**`INTEGER` / `REAL` のみ** |
| `COALESCE(a, b, …)` | 1 以上 | 最初の非 `NULL` の型 | 全滅なら `NULL`         | 引数数は自由                                                         |

- **未知の関数**は `未知のスカラー関数 '…' です`。
- **引数数が違う**場合は `… 関数は 1 個の引数が必要ですが、2 個渡されました`。
  ただし空の引数リスト（`LENGTH()`）は関数の前に構文エラーになります。
- `DECIMAL` 列に `ROUND` を掛けると `ROUND は数値を受け付けますが 100.5 です` になります。
  `ROUND(x * 1.0)` のように `REAL` に変換してから渡してください。

### 6.7 条件式

```sql
CASE WHEN 条件 THEN 値 [WHEN 条件 THEN 値 …] [ELSE 既定値] END
```

- 条件は上から順に評価され、**最初に真になった** `THEN` を返します。
- 条件が `NULL`（`UNKNOWN`）のときは次の `WHEN` に進みます。
- `ELSE` を書かないと、真の分岐が 1 つも無いときは `NULL` を返します。

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

引数は 0 個以上。集約関数以外は無制限に呼び出せます。

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

- **`FROM` は省略できません。** `SELECT 1` は `FROM が必要ですが見つかりませんでした` になります。
- `JOIN` は `INNER JOIN` / `JOIN` / `LEFT JOIN` の 3 種。
  `RIGHT` / `FULL` / `CROSS` は**ありません**。
- `ON` は省略できません（`CROSS JOIN` の代わりに `ON 1 = 1`）。
- `DISTINCT` は射影の**後**に適用されます（`SELECT DISTINCT a, b` は `(a, b)` の組で重複除去）。

### 7.2 評価順

```
FROM → JOIN → WHERE → GROUP BY → HAVING → 集約 → ORDER BY → 射影 → DISTINCT → LIMIT
```

`LIMIT` は最後に適用されます（pushdown されません）。

### 7.3 結果の列名

| 記述             | 列名                                            |
| ---------------- | ----------------------------------------------- |
| `SELECT id`      | `id`                                            |
| `SELECT id AS x` | `x`                                             |
| `SELECT (1 + 2)` | `(1 + 2)`（式の表示文字列。**常に丸括弧つき**） |
| `SELECT *`       | 各列名（`id`, `name`, …）                       |
| `SELECT t.*`     | `t.id`, `t.name`, …（修飾つき）                 |

クライアントが列名でアクセスする場合は `AS` を付けるのが安全です。

### 7.4 索引が使われる条件

`WHERE` に**列 = 定数**（＝リテラル、または負の数値リテラル）の等価述語が `AND` で連結されているとき、左から順に評価されます。

| 条件                           | 走査方法                          |
| ------------------------------ | --------------------------------- |
| 主キー列 `= 定数`              | 点ルックアップ（`PkLookup`）      |
| `CREATE INDEX` した列 `= 定数` | 索引ルックアップ（`IndexLookup`） |
| それ以外                       | 全走査（`SeqScan`）               |

```sql
CREATE INDEX idx_dept ON people (dept_id);
SELECT id FROM people WHERE dept_id = 2;   -- 索引が使われる（一致する行をすべて返す）
SELECT id FROM people WHERE dept_id > 2;   -- 全走査（範囲検索はない）
```

制限:

- **範囲走査は使いません。** `>`, `<`, `BETWEEN`, `LIKE` は索引を要求しません。
- **複合索引の Leftmost prefix は考慮しません。
  ** 索引の**いずれかの列**に等価条件があればその索引で 1 値だけ引きます
  （`WHERE b = 2` が複合索引 `(a, b)` を使っても、 `b = 2` だけでは意味がないのに使ってしまいます）。

### 7.5 結合アルゴリズム

| 条件                                   | アルゴリズム                      |
| -------------------------------------- | --------------------------------- |
| `ON` に等価結合がある                  | ハッシュ結合                      |
| `ON` に等価結合がない & 右に索引がある | インデックス入れ子ループ          |
| それ以外                               | 入れ子ループ（cross join + 述語） |

- 結合順序は `FROM` に書かれた順です（選択性に基づく並べ替えは未実装）。
- `LEFT JOIN` は右側の一致がない場合に `NULL` を補います。

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

ソートはメモリ上に実体化してから安定ソートします。
同値な行の順序は保証されません（`ORDER BY` のキーが一意でないとき）。

### 7.7 `LIMIT` / `OFFSET`

- 値は**整数リテラル**です（それ以外は 0 とみなされ「制限なし」になります）。
- 評価は最後に 1 回だけ。`OFFSET` → `LIMIT` の順で適用されます。
- `LIMIT` だけの形か `LIMIT n OFFSET m` の形だけ書けます（`OFFSET` 単独は不可）。
- ［制限］`LIMIT 0` は「0 行」ではなく「**制限なし**」として扱われます。

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

- **列リストを省略**すると、テーブル定義の順に値を並べます。
- **列リストに無い列**は `DEFAULT` を使い、無ければ `NULL` になります。
- **値式は定数のみ**です（列参照不可）。
- 値は列の型へ**暗黙変換**されます（[§3](#暗黙の型変換sqlvaluecoerce)）。
- `NOT NULL` 制約は行ごとに検証されます。
- **1 つの `INSERT` は 1 トランザクション**です（途中で失敗したら何も残りません）。

### 主キー重複は upsert

**エラーにならず上書きされます。**［制限］
複数列の主キーも同じで、値の組が一致すれば上書きされます。

### `UNIQUE` は検査されない

`UNIQUE` 制約・`UNIQUE` 索引も宣言できますが、**重複はエラーになりません**。［制限］

```sql
INSERT INTO people VALUES (1, 'alice2', 30, 1, 1.0);  -- id=1 が置き換わる
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

- 複数列を同時に更新できます。式は**更新前の行**に対して評価されます。
- `WHERE` を省略すると全行が対象です。
- 条件に合う行すべてが更新されます（`LIMIT` 相当の句がありません）。
- 影響行数は `StatementResult.RowsAffected` に入ります。
- 索引があるテーブルでは、更新のたびにすべての索引のエントリが書き直されます。

### ［制限］主キー列は更新できない

主キー列を `SET` すると、行が新しい主キーの位置にも**コピーされるだけで、古い主キー位置の行は消えません**（幽霊行）。
`RowsAffected` は 1 になりますが、行数は増えてしまいます。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, n TEXT);
INSERT INTO t VALUES (1,'a'),(2,'b');
UPDATE t SET id = 100 WHERE id = 1;   -- RowsAffected = 1

SELECT id, n FROM t ORDER BY id;
-- 1, 'a'      ← 消えない
-- 2, 'b'
-- 100, 'a'    ← 複製された
```

主キーを変えたい用途では `DELETE` + `INSERT` を使ってください。

```sql
UPDATE people SET age = age + 1 WHERE id = 1;
UPDATE people SET salary = salary * 1.1;    -- 全行
```

---

## 10. DELETE

```sql
DELETE FROM テーブル [ WHERE 条件 ];
```

- `WHERE` を省略すると全行を削除します。
- 削除は主キーと**すべての索引**に墓碑（tombstone）を書きます。
- 影響行数は `StatementResult.RowsAffected` に入ります。
- 条件に合う行が 0 件でもエラーにはなりません（0 行になります）。

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
| `UNIQUE`      | 列・テーブル | **宣言だけ。重複は検査されない**［制限］                  |
| `DEFAULT`     | 列           | `INSERT` で値を省略したときに使われる（定数式のみ）       |

- **主キーは省略できません。** 1 つも無いと
  `テーブル t に主キーがありません` になります。
- 主キーは複数列にできます。値は列の宣言順に連結されます。
- `PRIMARY KEY` はテーブル制約が列レベルより優先されます。
- `IF NOT EXISTS` を付けると、既存テーブルなら**何もしずに成功**します。
- `IF NOT EXISTS` と `IF EXISTS` の両方の綴りを受け付けます。

#### 列定義で型を省略する

型を書かずに制約だけを書く構文も受け付けます（型は `TEXT` になります）。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, name NOT NULL);
```

### 11.2 CREATE INDEX

```sql
CREATE [UNIQUE] INDEX 索引名 ON テーブル (列 [, 列 …]);
```

- **既存の行はすべて索引に投入されます**（backfill）。
  backfill の間は排他を取らないので、並行 insert との整合は保証されません。
- 索引は**独立した LSM ツリー**（keyspace `i:<索引名>`）に格納されます。
- `UNIQUE` は**宣言だけ**で、重複は検査されません［制限］。
- 同名の索引が既にあれば `索引 <名前> は既に存在します`。
- 索引する列が存在しない場合は `テーブル <t> に列 <c> がありません`。
- `IF NOT EXISTS` は**ありません**。

```sql
CREATE INDEX idx_dept ON people (dept_id);
CREATE UNIQUE INDEX idx_email ON people (email);
```

### 11.3 DROP

```sql
DROP TABLE [IF EXISTS] テーブル名;
DROP INDEX [IF EXISTS] 索引名;
```

- `IF EXISTS` を付けると、存在しなくても**エラーになりません**。
- `DROP TABLE` はテーブル本体と、その索引 keyspace だけを消します
  （行データは LSM の compaction で再利用されるため即座に解放されます）。

```sql
DROP TABLE IF EXISTS tmp_data;
DROP INDEX idx_dept;
```

---

## 12. トランザクション

### SQL 文としての BEGIN / COMMIT / ROLLBACK

［制限］**構文としては受理されますが、実行しても何も起きません。**
特に `ROLLBACK` を書いても行は取り消されません。

```sql
BEGIN;
INSERT INTO people VALUES (9, 'eve', 50, 1, 1.0);
ROLLBACK;            -- ← 取り消されない
```

### 暗黙のトランザクション

`Begin()` していない状態の `Execute` / `Query` は、**1 文ごとに**暗黙のトランザクションを開始して自動 commit します。

```fsharp
// ok は Result の中身を取り出す例示用ヘルパー。
let ok =
    function
    | Ok v -> v
    | Error e -> failwith (DbError.toDisplayString e)

let db, _ = ok (db.Execute "INSERT INTO t VALUES (1, 10)")   // 1 文 = 1 commit
```

`Execute` に `;` 区切りで複数文を渡した場合も、`BEGIN` を張っていない限り 1 文ごとに commit されます。

### 明示的なトランザクション（ライブラリ API）

```fsharp
open RykDb.Core

// ok は Result の中身を取り出す例示用ヘルパー。
let ok =
    function
    | Ok v -> v
    | Error e -> failwith (DbError.toDisplayString e)

let db  = ok (Database.openWith "./mydata")
let db1 = db.Begin()                               // 明示トランザクション開始
let db2, _ = ok (db1.Execute "INSERT INTO t VALUES (1, 10)")
let db3 = ok (db2.Commit())                        // 確定
// または
let db4 = db2.Rollback()                           // 破棄
```

- ハンドルは**不変**です。戻り値の `Database` を受け取り直さないと書き込みが見えません。
- 入れ子の `Begin()` は何もしません（既にトランザクション中なら `this` を返す）。
- snapshot はトランザクション開始時に固定されます（repeatable read）。
- 同一キーへ別トランザクションが未 commit のエントリを持つ場合、後続側は `ConflictError` になります（first-committer-wins）。

### CLI のドットコマンド

| コマンド    | 効果                             |
| ----------- | -------------------------------- |
| `.commit`   | 開いているトランザクションを確定 |
| `.rollback` | 開いているトランザクションを破棄 |

```
db> BEGIN;
db> INSERT INTO people VALUES (9, 'eve', 50, 1, 1.0);
db> .rollback
(rollback しました)
```

---

## 13. エラー

すべての失敗は `DbError` として返ります（例外を制御フローに使いません）。

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

構文エラーには行番号が付きます。

```
エラー: FROM が必要ですが見つかりませんでした (位置 3 行)
```

### よく見るメッセージ（実測）

| SQL                                     | メッセージ                                               |
| --------------------------------------- | -------------------------------------------------------- |
| `SELECT 1;`                             | `FROM が必要ですが見つかりませんでした (位置 1 行)`      |
| `SELECT * FROM t WHERE id > 1 AND;`     | `式が必要ですが見つかりませんでした (位置 1 行)`         |
| `SELECT 'abc;`                          | `文字列が閉じられていません (位置 1 行)`                 |
| `SELECT id FROM nosuchtable;`           | `テーブル 'nosuchtable' が存在しません`                  |
| `SELECT nosuchcol FROM t;`              | `'nosuchcol' は列でも関数でもありません`                 |
| `CREATE TABLE t (a INTEGER);`           | `テーブル t に主キーがありません`                        |
| `CREATE TABLE t (...);`（重複）         | `テーブル t は既に存在します`                            |
| `CREATE INDEX ix ON t (nosuchcol);`     | `テーブル t に列 nosuchcol がありません`                 |
| `DROP TABLE nosuch;`                    | `テーブル nosuch が存在しません`                         |
| `INSERT INTO t (id) VALUES (1, 2);`     | `INSERT の列数(1)と値の数(2)が合いません`                |
| `INSERT INTO t VALUES (1, NULL, …);`    | `NOT NULL 制約に違反: t.n`                               |
| `INSERT INTO t VALUES (5, 'z', 'abc');` | `'abc' は 'INTEGER' に変換できません`                    |
| `SELECT (id) / 0 FROM t;`               | `0 での除算は定義されていません`                         |
| `SELECT LENGTH(id, n) FROM t;`          | `LENGTH 関数は 1 個の引数が必要ですが、2 個渡されました` |
| `SELECT NOSUCHFN(id) FROM t;`           | `未知のスカラー関数 'NOSUCHFN' です`                     |
| `SELECT active = 1 FROM u;`             | `型 'BOOLEAN' と 'INTEGER' は比較できません`             |
| `SELECT ROUND(salary) FROM t;`          | `ROUND は数値を受け付けますが 100.5 です`                |

---

## 14. 既知の制限

### 構文が未対応のもの

- サブクエリ、`UNION` / `UNION ALL` / `INTERSECT` / `EXCEPT`
- CTE（`WITH`）、再帰クエリ
- ウィンドウ関数（`OVER` / `PARTITION BY` / `ROW_NUMBER` …）
- `RIGHT JOIN` / `FULL OUTER JOIN` / `CROSS JOIN` / `USING`
- 自然結合、`INSERT ... SELECT`
- `ALTER TABLE`（列の追加・削除・型変更）
- `TRUNCATE`、`VACUUM`（不要です。compaction が自動回収します）
- `EXPLAIN`、`RETURNING`
- ユーザ定義関数・トリガー・ビュー・ストアドプロシージャ
- パーティショニング、圧縮、暗号化
- 複数プロセスからの同時アクセス（1 プロセス前提）

### 実装上の制約

| 項目                            | 内容                                                                          |
| ------------------------------- | ----------------------------------------------------------------------------- |
| `COUNT(DISTINCT x)`             | 構文は通るが**重複排除しない**（`COUNT(x)` と同じ結果）                       |
| 集約関数のエラー                | 握り潰されて `NULL` になる（文字列列への `SUM` など）                         |
| `ROUND(x)`                      | `INTEGER` / `REAL` のみ。`DECIMAL` は実行時エラー                             |
| `UPPER(NULL)` など              | `NULL` ではなく文字列 `'NULL'` を返す                                         |
| `UNIQUE` 制約・索引             | 宣言されるが**重複を検査しない**                                              |
| 主キー重複 `INSERT`             | エラーにならず**上書き**（upsert）                                            |
| `UPDATE` での主キー変更         | 行が複製され、幽霊行が残る                                                    |
| `BEGIN` / `COMMIT` / `ROLLBACK` | **文としては無効**。ライブラリ API / ドットコマンドを使う                     |
| `SELECT` の `FROM`              | **省略不可**。定数だけの SELECT は書けない                                    |
| 演算子の後置                    | `SELECT` 項目の先頭が裸の識別子・関数のとき演算子を追記できない（括弧で囲む） |
| 索引                            | 等価一致のみ。範囲走査・複合索引の Leftmost prefix は使わない                 |
| `LIMIT` / `OFFSET`              | 整数リテラルのみ。`LIMIT 0` は「制限なし」                                    |
| 型引数                          | `VARCHAR(32)` は宣言のみ。切り詰めなし                                        |
| ソート                          | メモリ上に実体化。spilling する外部マージは未実装                             |
| `AVG`                           | `SUM / COUNT` を実行時に計算                                                  |
| `UPDATE` / `DELETE`             | `LIMIT` 相当の句がない                                                        |
| `ORDER BY`                      | 同値行の順序は保証しない                                                      |

### 演算の注意

- `INTEGER / INTEGER` は切り捨て（`10 / 4` → `2`）。実数にしたいなら `10 / 4.0`。
- `UPPER(NULL)` / `LOWER(NULL)` / `TRIM(NULL)` は文字列 `'NULL'` を返します。
- `DATE` と `TIMESTAMP` は相互比較できますが、`DATE` は UTC 午前 0 時として扱われます。
- `REAL` の `NaN` は `REAL` 同士の比較でのみ特別扱いされます（`NaN` が最大、`NaN = NaN` は真）。
  `NaN` と `INTEGER` / `DECIMAL` を比較すると `TypeError`（`NaN や ±Infinity は数値比較できません`）になります。
  `NaN` / `±Infinity` は `DECIMAL` に変換できません。

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
-- 索引付き点検索
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
