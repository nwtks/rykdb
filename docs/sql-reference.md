# SQL リファレンス

RykDbが受け付けるSQLの文法と意味を示す。
標準SQLとの主な相違点も併記する。

実装は [`invariants.md`](invariants.md)、
設計は [`architecture.md`](architecture.md)、
既知の落とし穴は [`gotchas.md`](gotchas.md) を参照する。

## 目次

1. [文](#1-文)
2. [字句](#2-字句)
3. [データ型](#3-データ型)
4. [リテラルとパラメータ](#4-リテラルとパラメータ)
5. [識別子と名前解決](#5-識別子と名前解決)
6. [式](#6-式)
7. [SELECT](#7-select)
8. [INSERT](#8-insert)
9. [UPDATEとDELETE](#9-updateとdelete)
10. [DDL](#10-ddl)
11. [トランザクション](#11-トランザクション)
12. [エラー](#12-エラー)
13. [既知の制限](#13-既知の制限)
14. [例](#14-例)

## 1. 文

文は次の形式を受け付ける。

```ebnf
statement ::= select | explain | insert | update | delete | ddl | transaction

select_core ::= SELECT [DISTINCT] select_item { "," select_item }
                [ FROM table_ref { join_clause } ]
                [ WHERE expr ]
                [ GROUP BY expr { "," expr } ]
                [ HAVING expr ]

select ::= select_core { UNION ALL select_core }
           [ ORDER BY sort_key { "," sort_key } ]
           [ LIMIT expr ] [ OFFSET expr ]

table_ref  ::= ident [ [AS] ident ]
             | "(" select ")" [AS] ident
join_clause ::= [ INNER | LEFT [OUTER] ] JOIN table_ref ON expr
sort_key   ::= expr [ ASC | DESC ]

explain    ::= EXPLAIN SELECT ...

insert     ::= INSERT INTO ident [ "(" column_list ")" ]
               ( VALUES row { "," row } | SELECT ... | DEFAULT VALUES )

update     ::= UPDATE ident SET assignment { "," assignment } [ WHERE expr ]
delete     ::= DELETE FROM ident [ WHERE expr ]

ddl        ::= CREATE TABLE [IF NOT EXISTS] ident "(" table_element { "," table_element } ")"
             | CREATE [UNIQUE] INDEX [IF NOT EXISTS] ident ON ident "(" column_list ")"
             | DROP TABLE [IF EXISTS] ident
             | DROP INDEX [IF EXISTS] ident
             | ALTER TABLE ident ( ADD [COLUMN] column_def
                                  | DROP [COLUMN] ident
                                  | RENAME [COLUMN] ident TO ident )
             | ANALYZE [TABLE] [ident]

transaction ::= BEGIN [TRANSACTION] | START TRANSACTION
              | COMMIT [WORK | TRANSACTION] | END
              | ROLLBACK [WORK | TRANSACTION]
```

複数の文は `;`で区切る。
末尾の `;`は省略できる。
区切りなしは `SyntaxError`になる。

`Session.Execute` は複数文を順に実行する。

- 結果を返す文が1つだけの場合は、その文の結果を返す。
- 結果を返す文が複数、または結果を返さない場合は、`RowsAffected`の合計を返す。
- `SELECT`以外で `Columns`と `Rows`は空になる。

`EXPLAIN`は `SELECT`だけを受け付ける。

## 2. 字句

### コメント

- `--` は行末までコメントになる。
- `/* ... */` はブロックコメントになる。
- ブロックコメントは入れ子にできる。

```sql
/* 外側 /* 内側 */ 外側もコメント */
```

### 識別子

通常の識別子は、先頭が文字または `_`、以降が文字・数字・ `_`で構成される。

- Unicodeの文字は識別子に使える。
- 数字から始めることはできない。
- テーブル名と索引名はUTF-8で128バイト以内である必要がある。
- 識別子名の大小文字は区別しない。
- 引用識別子は `"..."`で囲む。`""`で `"`を表現する。

```sql
SELECT "Order Id" FROM "My Table";
SELECT _u FROM t;
```

予約語の例は次のとおり。

```text
SELECT  FROM    WHERE   INSERT  INTO    VALUES  UPDATE  SET     DELETE
CREATE  TABLE   INDEX    DROP    ON      AND     OR      NOT     NULL
IS      IN      BETWEEN  LIKE    GROUP   BY      HAVING  ORDER   ASC
DESC    LIMIT   OFFSET   JOIN    INNER   LEFT    AS      DISTINCT
BEGIN   COMMIT  ROLLBACK PRIMARY KEY     UNIQUE  DEFAULT CASE    WHEN
THEN    ELSE    END      EXISTS  IF      TRUE    FALSE
```

予約語を別名として使う場合は原則として `AS`を付ける。

```sql
SELECT 1 AS right FROM t AS union;
```

### 記号と数値

演算子と区切り記号は次のとおり。

```text
<>   !=   <=   >=   ||   =   <   >   +   -   *   /   %   ;   ,   .   (   )
```

整数は `INTEGER`、小数点または指数部を持つ数値は `REAL`になる。

```sql
0 42 -7       -- INTEGER
1.5 .5 1e3    -- REAL
```

`DECIMAL`のリテラルはない。
厳密な小数を渡す場合は、文字列リテラルを列の型へ変換する。

### 文字列とバイナリ

文字列リテラルはシングルクォートで囲み、`'`は `''`で表す。

```sql
'abc'
'it''s'
```

バイナリリテラルは偶数桁の16進文字列を使う。

```sql
X'0A1B'
x'0a1b'
```

奇数桁や16進以外の文字を含むバイナリリテラルは構文エラーになる。

## 3. データ型

| 型          | 宣言に使える名前           | 内部表現              | 表示                                   |
| ----------- | -------------------------- | --------------------- | -------------------------------------- |
| `BOOLEAN`   | `BOOLEAN`、`BOOL`          | 真偽値                | `TRUE` / `FALSE`                       |
| `INTEGER`   | `INTEGER`、`INT`、`BIGINT` | 64ビット符号付き整数  | `42`                                   |
| `REAL`      | `REAL`、`DOUBLE`           | IEEE 754倍精度        | `1.5`                                  |
| `DECIMAL`   | `DECIMAL`、`NUMERIC`       | 固定小数点数          | `150.25`                               |
| `TEXT`      | `TEXT`                     | UTF-16文字列          | `'abc'`                                |
| `BLOB`      | `BLOB`                     | バイト列              | `X'0A1B'`                              |
| `DATE`      | `DATE`                     | 日付                  | `2024-01-15`                           |
| `TIMESTAMP` | `TIMESTAMP`、`DATETIME`    | UTCオフセット付き時刻 | `2024-01-15T10:30:00.0000000Z`         |
| `UUID`      | `UUID`                     | GUID                  | `0f8fad5b-d9cb-469f-a165-70867728950e` |

`NULL`はリテラル専用で、列の型として宣言できない。

### 型引数

`DECIMAL(10, 2)`の型引数は読まれるだけで、丸めや桁数検証は行わない。

```sql
CREATE TABLE t (v DECIMAL(3, 1));
INSERT INTO t VALUES (12345.6789);
```

保存時に丸めることなく、値を保持する。

### 型への自動変換

`INSERT`と `UPDATE`は値を列の型へ変換する。
変換できない場合は `TypeError`になる。

| 目標型      | 受け付ける値                                                      |
| ----------- | ----------------------------------------------------------------- |
| `INTEGER`   | `INTEGER`、`BOOLEAN`、小数部が0でint64範囲内の `REAL` / `DECIMAL` |
| `REAL`      | `REAL`、`INTEGER`、`BOOLEAN`、`DECIMAL`                           |
| `DECIMAL`   | `DECIMAL`、`INTEGER`、`REAL`、`BOOLEAN`                           |
| `BOOLEAN`   | `BOOLEAN`、0または1の `INTEGER` / `REAL`                          |
| `TEXT`      | すべての値                                                        |
| `BLOB`      | `BLOB`、UTF-8へ変換できる `TEXT`                                  |
| `DATE`      | `DATE`、`TIMESTAMP`、`yyyy-MM-dd`形式の `TEXT`                    |
| `TIMESTAMP` | `TIMESTAMP`、`DATE`、解析できる `TEXT`                            |
| `UUID`      | `UUID`、`Guid.TryParse`が通る `TEXT`                              |

`DECIMAL`への変換では、`NaN`、無限大、decimal範囲外を拒否する。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, hired DATE, uid UUID);
INSERT INTO t VALUES (1, '2024-01-15', '0f8fad5b-d9cb-469f-a165-70867728950e');
```

### 比較規則

| 型                   | 比較規則                                                                   |
| -------------------- | -------------------------------------------------------------------------- |
| 数値                 | `INTEGER`と `DECIMAL`はdecimalで比較する。`REAL`同士は浮動小数点で比較する |
| `REAL`の `NaN`       | `REAL`同士では最大値として扱う。`NaN = NaN`は真                            |
| `TEXT`               | UTF-8バイト列の辞書順（Unicodeコードポイント順）                           |
| `BLOB`               | バイト列の辞書順。短い方が先                                               |
| `DATE` / `TIMESTAMP` | 相互比較可。`DATE`はUTC午前0時として扱う                                   |
| `UUID`               | GUIDの比較規則                                                             |
| `BOOLEAN`            | `FALSE < TRUE`                                                             |

`INTEGER`、`REAL`、`DECIMAL`をまたぐ比較は、値がdecimalで表せる範囲にある場合はdecimalで比較する。
範囲外または極小値はfloatで比較する。
異なる型の組み合わせや、`NaN`と整数・decimalの比較は `TypeError`になる。
`BOOLEAN`と `INTEGER`は比較できない。

## 4. リテラルとパラメータ

### リテラル

リテラルには次の表記がある。

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

`DECIMAL`、`DATE`、`TIMESTAMP`、`UUID`の専用リテラルはない。
文字列リテラルを列の型へ変換するか、値をパラメータで渡す。

### 位置パラメータ

`?`は位置パラメータである。出現順が引数配列の添字になる。

```fsharp
let sql = "SELECT id FROM people WHERE age >= ? AND name LIKE ? LIMIT ?"
let parameters =
    [| SqlValue.Int 20L
       SqlValue.Text "a%"
       SqlValue.Int 10L |]

session.Execute(sql, parameters)
```

- `VALUES`、述語、`HAVING`、`LIMIT`、`OFFSET`、`LIKE`パターンなどに使える。
- SQL文字列へ値を連結しないため、SQLインジェクションを防ぐ。
- 個数が一致しない場合と `SqlValue.Unloaded`は `TypeError`になる。
- 名前付きパラメータは未対応。
- `EXPLAIN`でも定数パラメータを計画に使える。

### 文字数

文字数はUnicodeコードポイント単位で数える。

- `LENGTH`はコードポイント数を返す。
- `LIKE`の `_`はコードポイント1個に一致する。
- `ESCAPE`文字はコードポイント1個で指定する。

サロゲートペアは1文字として数える。結合文字は別々のコードポイントとして数える。

## 5. 識別子と名前解決

```sql
SELECT p.id, p.name FROM people p;
SELECT id FROM people p;
SELECT people.id FROM people;
SELECT "Order Id" FROM "My Table";
```

- 修飾なしの列名は `FROM`に現れるテーブルの順に解決する。
- 結合で同名の列が複数ある場合は修飾が必要になる。
- 別名を付けた場合は別名で修飾する（実名では解決しない）。
- 名前の比較は大文字小文字を区別しない。

## 6. 式

### 6.1 演算子の優先順位

優先順位が高いものから示す。

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

算術演算と文字列連結は左結合する。

### 6.2 3値論理

述語の結果は `TRUE`、`FALSE`、`UNKNOWN`のいずれかになる。

| 式                                        | 結果      |
| ----------------------------------------- | --------- |
| `NULL = NULL`、`NULL <> NULL`、`1 = NULL` | `UNKNOWN` |
| `NULL AND FALSE`                          | `FALSE`   |
| `NULL AND TRUE`                           | `UNKNOWN` |
| `NULL OR TRUE`                            | `TRUE`    |
| `NOT NULL`                                | `UNKNOWN` |
| `x IS NULL`、`x IS NOT NULL`              | 常に真偽  |

`WHERE`と `HAVING`では `UNKNOWN`を偽として扱う。
算術、`ABS`、`CASE`では `NULL`が伝播する。

### 6.3 算術

| 式              | 結果                            |
| --------------- | ------------------------------- |
| `1 + 2`         | `3`                             |
| `10 / 4`        | `2`。整数同士の除算は切り捨てる |
| `10 % 4`        | `2`                             |
| `1 + NULL`      | `NULL`                          |
| `1 / 0`         | `0 での除算`エラー              |
| `1.5 + 1`       | `REAL`                          |
| `'a' \|\| 'b'`  | `'ab'`                          |
| `'a' \|\| NULL` | `NULL`                          |

計算に `REAL`が含まれれば `REAL`になる。
`INTEGER`同士は `INTEGER`、`DECIMAL`同士は `DECIMAL`、`INTEGER`と `DECIMAL`の混合は `DECIMAL`になる。
片方が `NULL`なら結果も `NULL`になる。
実数除算には `10 / 4.0`のように小数を使う。

### 6.4 述語

```sql
x = 1  x <> 1  x != 1
x < 1  x <= 1  x > 1  x >= 1
x IS NULL  x IS NOT NULL
x IS TRUE  x IS NOT TRUE
x IS FALSE  x IS NOT FALSE
x IS UNKNOWN  x IS NOT UNKNOWN
x IN (1, 2, 3)
x NOT IN (1, 2, 3)
x BETWEEN 1 AND 10
x NOT BETWEEN 1 AND 10
name LIKE 'a%'
name NOT LIKE 'a%'
```

`BETWEEN`は両端を含む。
`IN`には式のリストまたはサブクエリを指定できる。

`LIKE`は次のワイルドカードを使う。

| 記号 | 意味            |
| ---- | --------------- |
| `%`  | 0文字以上       |
| `_`  | 1コードポイント |

エスケープ文字を指定できる。

```sql
name LIKE 'a!%' ESCAPE '!'
```

`!%`の `!`をエスケープとして解釈し、末尾の `%`をリテラルとして扱う。

- パターン末尾にエスケープ文字だけが残る場合は `TypeError`になる。
- `ILIKE`や既定の `\`エスケープはない。
- `NULL`に対する `LIKE`の結果は `NULL`になる。

`IS TRUE`、`IS FALSE`、`IS UNKNOWN`は、`BOOLEAN`以外の値をUNKNOWNとして扱う。

### 6.5 集約関数

| 関数       | 戻り値                 | 空のグループ | `NULL`   |
| ---------- | ---------------------- | ------------ | -------- |
| `COUNT(*)` | `INTEGER`              | `0`          | 数える   |
| `COUNT(x)` | `INTEGER`              | `0`          | 数えない |
| `SUM(x)`   | `DECIMAL`または `REAL` | `NULL`       | 無視     |
| `AVG(x)`   | 常に `REAL`            | `NULL`       | 無視     |
| `MIN(x)`   | 入力と同じ型           | `NULL`       | 無視     |
| `MAX(x)`   | 入力と同じ型           | `NULL`       | 無視     |

```sql
SELECT dept_id,
       COUNT(*) AS c,
       SUM(salary) AS total,
       AVG(salary) AS mean,
       MIN(name) AS lo,
       MAX(name) AS hi
FROM people
GROUP BY dept_id
HAVING COUNT(*) > 1
ORDER BY dept_id;
```

- `GROUP BY`を省略した集約は、表全体を1グループとして計算する。
- `FROM`がない集約は1行を入力として計算する。
- `SUM`と `AVG`に数値以外を渡すと `TypeError`になる。
- 集約引数には `DISTINCT`を指定できる。
- 集約と `GROUP BY`を使うクエリでは `SELECT *`を使えない。
- 集約も `GROUP BY`もない `HAVING`は構文エラーになる。

### 6.6 スカラー関数

関数名の大小文字は区別しない。

| 関数                  |  引数 | 戻り値              | `NULL`                 |
| --------------------- | ----: | ------------------- | ---------------------- |
| `UPPER(x)`            |     1 | `TEXT`              | `NULL`                 |
| `LOWER(x)`            |     1 | `TEXT`              | `NULL`                 |
| `TRIM(x)`             |     1 | `TEXT`              | `NULL`                 |
| `LENGTH(x)`           |     1 | `INTEGER`           | `NULL`                 |
| `ABS(x)`              |     1 | 入力と同じ数値型    | `NULL`                 |
| `ROUND(x)`            |     1 | 入力と同じ数値型    | `NULL`                 |
| `COALESCE(a, b, ...)` | 1以上 | 最初の非 `NULL`の型 | 全て `NULL`なら `NULL` |
| `CAST(x AS type)`     |     1 | 変換後の型          | `NULL`                 |

`CAST`は型引数を無視して変換する。
`TEXT`から数値・真偽値への変換もできる。

```sql
CAST('42' AS INTEGER)
CAST('TRUE' AS BOOLEAN)
CAST(3.14 AS TEXT)
```

未知の関数、引数不足、引数過多はエラーになる。
空の引数リストは構文エラーになる。

### 6.7 CASE式

```sql
CASE
  WHEN 条件 THEN 値
  [WHEN 条件 THEN 値 ...]
  [ELSE 値]
END
```

条件は上から順に評価し、最初に真となった `THEN`を返す。
`ELSE`を省略し、真となる条件がない場合は `NULL`になる。

```sql
CASE WHEN age > 30 THEN 'senior'
     WHEN age > 20 THEN 'mid'
     ELSE 'junior'
END
```

式とサブクエリの入れ子には深さ制限を設けない。

## 7. SELECT

### 7.1 基本文法

```sql
SELECT [DISTINCT] select_item [, ...]
FROM table_ref [ [AS] alias ]
     [join_clause ...]
[WHERE expr]
[GROUP BY expr [, ...]]
[HAVING expr]
[UNION ALL select_core ...]
[ORDER BY expr [ASC | DESC] [, ...]]
[LIMIT expr]
[OFFSET expr]
```

`FROM`は省略できる。
`SELECT 1`は1行1列を返し、列名は式の表示文字列になる。
`FROM`がないクエリでは `*`と `t.*`は使えない。

`JOIN`は次の形式を受け付ける。

```sql
INNER JOIN table ON expr
JOIN table ON expr
LEFT JOIN table ON expr
LEFT OUTER JOIN table ON expr
```

`ON`は省略できない。
`RIGHT`、`FULL`、`CROSS`、`NATURAL`、`USING`、カンマ結合は未対応。

### 7.2 評価順

実行順は概ね次のとおり。

```text
FROM → JOIN → WHERE → GROUP BY → 集約 → HAVING → ORDER BY
→ 射影 → DISTINCT → LIMIT / OFFSET
```

`OFFSET`の後に `LIMIT`を適用する。

### 7.3 結果の列名

| 記述             | 列名         |
| ---------------- | ------------ |
| `SELECT id`      | `id`         |
| `SELECT id AS x` | `x`          |
| `SELECT (1 + 2)` | `(1 + 2)`    |
| `SELECT *`       | 各列名       |
| `SELECT t.*`     | 修飾付き列名 |

クライアントで列名を参照する場合は `AS`を付ける。

`ORDER BY`、`GROUP BY`、`HAVING`では `SELECT`項目の別名を参照できる。
入力に同名の列がある場合は列が優先される。

### 7.4 索引と走査

`WHERE`の列比較が索引に使える場合は、実行計画で索引走査を選ぶ。

| 条件                                             | 走査                                   |
| ------------------------------------------------ | -------------------------------------- |
| 主キー全列が定数と等しい                         | `PkLookup`                             |
| 主キーの一部の列だけが定数と等しい               | `PkLookup`にせず、索引の先頭等値を検討 |
| 索引の先頭から連続する列が定数と等しい           | `IndexLookup`                          |
| 索引の先頭列の範囲、または前置等値の次の列の範囲 | `IndexRangeScan`                       |
| 単一主キー列の下限を含む範囲                     | `RangeScan`                            |
| 使用可能な条件がない                             | `SeqScan`                              |

複合索引では、先頭列から順に条件が必要になる。

```sql
CREATE INDEX ix_pair ON t (a, b);

SELECT * FROM t WHERE a = 1 AND b > 2; -- IndexRangeScan
SELECT * FROM t WHERE a > 2;           -- IndexRangeScan
SELECT * FROM t WHERE b > 2;           -- SeqScan
SELECT * FROM t WHERE a = 1;           -- IndexLookup
SELECT * FROM t WHERE a = 1 AND b = 2; -- IndexLookup
```

走査方法に使う等値・範囲は、列の宣言型と定数の型が一致する場合だけ選ぶ。
型が異なる述語は `SeqScan` で評価する（評価自体は数値同士や `DATE` / `TIMESTAMP` 間でも可能）。

範囲走査には下限が必要で、`<`、`<=`だけでは索引を使わない。
`LIKE 'prefix%'`は末尾の `%`だけで `_`を含まない場合に範囲へ変換する。

索引エントリの値は空で、キーは索引列と主キーから復元する。
被覆していない列は主キーlookupで取得する。

### 7.5 結合

結合アルゴリズムは次の規則で選ぶ。

| 条件                                                 | アルゴリズム      |
| ---------------------------------------------------- | ----------------- |
| 左の見積りが32行以下で、右を主キーまたは索引で引ける | Index Nested Loop |
| 両方を結合キー順に走査できる                         | Merge Join        |
| 等値結合キーがあるが上記でない                       | Hash Join         |
| 等値結合キーがない                                   | Nested Loop       |

右部分木が`PkLookup` / `IndexLookup`の場合は、走査が吸収した等値をresidualへ戻してINLを使う。
再構成できない場合はINLにしない。

等値キーは列の型が一致する場合だけ使う。
型が異なる場合はNested Loopになる。

`NULL`は `=`で一致しない。
`LEFT JOIN`では一致しない左行へ `NULL`を補う。

ハッシュ結合は、`INNER JOIN`では推定行数の小さい側をbuildする（`EXPLAIN`では`HashJoin(left)`と表示する）。
`LEFT JOIN`は常に右をbuildする。

結合順は次の順で決める。

1. 中間結果の推定行数が小さいか
2. `FROM`で先に書かれているか

推定行数は統計ではなく、走査方法・`Filter` の数・結合述語の本数から決める。
結合述語1本ごとに選択性を1/10と見積もり、述語で繋がらない候補は直積として見積もる。

`SELECT *`の列順は結合順を並べ替えても、`FROM`に書いた論理順を維持する。

```sql
SELECT p.id, p.name, d.dname
FROM people p
JOIN depts d ON p.dept_id = d.id
ORDER BY p.id;

SELECT p.id, d.dname
FROM people p
LEFT JOIN depts d ON p.dept_id = d.id;
```

### 7.6 ORDER BY

ソートは外部マージソートを使う。
既定の管理メモリ64MiBを超える入力は一時ファイルへ退避する。

- `NULL`は `ASC`で先頭、`DESC`で末尾になる。
- `NULLS FIRST`、`NULLS LAST`は未対応。
- 同値行の順序は保証しない。
- 列参照、式、序数（1始まり）を指定できる。

主キー順に走査する単一テーブル（集約・`DISTINCT`・`JOIN`なし）で、
`ORDER BY`の先頭が主キー全列を定義順・昇順に並べる場合はソートを省略する。

### 7.7 LIMITとOFFSET

- 値は0以上の定数式である必要がある。
- リテラル、パラメータ、定数式を受け付ける。
- 負数、列参照、範囲外の値はエラーになる。
- `OFFSET`単独、`LIMIT`単独を指定できる。
- `OFFSET`を先に、次に `LIMIT`を適用する。
- `LIMIT 0`はスキャンを実行せず0行を返す。

```sql
SELECT id FROM people ORDER BY id LIMIT 2;
SELECT id FROM people ORDER BY id LIMIT 2 OFFSET 1;
```

単純な全走査では `OFFSET + LIMIT`をストレージへ渡せる。
`WHERE`、`GROUP BY`、`DISTINCT`、結合がある場合はヒントを使わない。

### 7.8 サブクエリ

式には次のサブクエリを使える。

| 形                        | 結果                                      |
| ------------------------- | ----------------------------------------- |
| `(SELECT ...)`            | スカラー値。0行は `NULL`、2行以上はエラー |
| `x IN (SELECT ...)`       | 1列の値を含むか判定                       |
| `EXISTS (SELECT ...)`     | 1行以上なら `TRUE`                        |
| `NOT EXISTS (SELECT ...)` | 1行もなければ `TRUE`                      |

```sql
SELECT id, (SELECT MAX(v) FROM t) AS mx FROM t;

SELECT id FROM t WHERE v > (SELECT AVG(v) FROM t);

SELECT id FROM t WHERE EXISTS (
  SELECT 1 FROM t AS x WHERE x.v > t.v
);
```

サブクエリは相関、または非相関にできる。

- 非相関サブクエリは外側のクエリの前に1回だけ計算して使い回す。
- 相関サブクエリは外側の行ごとに計算する。
- 内側のスコープで解決できない列だけ、外側へフォールバックする。
- サブクエリの列数が1ではない場合は `TypeError`になる。

相関サブクエリは次の場所で使える。

- `WHERE`
- `JOIN`の `ON`
- 集約を使わない `SELECT`項目
- `ORDER BY`
- `UPDATE`の `SET`
- `UPDATE`と `DELETE`の `WHERE`

`GROUP BY`または集約関数を使うクエリの `SELECT`・`HAVING`・`ORDER BY`、`LIMIT`・`OFFSET`、派生テーブルでは相関サブクエリを使えない。

サブクエリの入れ子には深さ制限を設けない。

### 7.9 派生テーブル

`FROM`と `JOIN`にはサブクエリを使える。
別名は必須である。

```sql
SELECT x.id, x.v
FROM (SELECT id, v FROM t WHERE v >= 20) AS x
WHERE x.v < 100;

SELECT t.id, d.w
FROM t JOIN (SELECT id, w FROM u) AS d ON d.id = t.id;
```

派生テーブルの列名は内側の射影から決まる。
別名を付けない式の列名は式の表示名になる。
列別名リスト `AS x(a, b)`、相関派生テーブル、カンマ結合は未対応。

### 7.10 EXPLAIN

```sql
EXPLAIN SELECT id FROM t WHERE id = 2;
```

実行計画を表示する。
クエリ本体は実行しない。

```text
plan
----
  Project(t.id)
    Scan(t, PkLookup(2))
```

`PkLookup`が吸収した等値述語は`Filter`に残らない。

主なノードは次のとおり。

| ノード      | 内容                                                                |
| ----------- | ------------------------------------------------------------------- |
| `Scan`      | `SeqScan`、`PkLookup`、`IndexLookup`、`RangeScan`、`IndexRangeScan` |
| `Filter`    | 検査条件                                                            |
| `Project`   | 射影                                                                |
| `Join`      | 結合種類とアルゴリズム                                              |
| `Aggregate` | グループと集約                                                      |
| `Having`    | 集約後の述語                                                        |
| `Distinct`  | 重複除去                                                            |
| `Sort`      | 並び替え                                                            |
| `Limit`     | 件数制限                                                            |
| `Values`    | `FROM`なしの入力                                                    |
| `Derived`   | 派生テーブル                                                        |
| `UnionAll`  | UNION ALL                                                           |

### 7.11 UNION ALL

```sql
SELECT ... UNION ALL SELECT ... [UNION ALL SELECT ...]
[ORDER BY ...] [LIMIT ...] [OFFSET ...];
```

全枝の列数は一致させる必要がある。
列名は先頭の `SELECT`のものになる。
値は型変換せず連結する。

```sql
SELECT id, name FROM people WHERE age >= 30
UNION ALL
SELECT id, name FROM people WHERE age < 30
ORDER BY id;
```

枝ごとの `ORDER BY`と `LIMIT`は使えない。
`UNION`、`INTERSECT`、`EXCEPT`は未対応。

### 7.12 DISTINCT

`DISTINCT`は射影後の行に適用する。

```sql
SELECT DISTINCT dept_id, name FROM people;
```

結果は `(dept_id, name)`の組で重複を除去する。

## 8. INSERT

```sql
INSERT INTO table [(column, ...)]
VALUES (expr, ...)[, (expr, ...) ...];

INSERT INTO table [(column, ...)] SELECT ...;
INSERT INTO table DEFAULT VALUES;
```

- 列リストを省略するとテーブル定義順に値を並べる。
- 列リストにない列には `DEFAULT`、または `NULL`を使用する。
- `VALUES`の要素には定数式とサブクエリを使える。
- 列参照は使えない。
- 値は列の型へ変換する。
- `NOT NULL`違反や型変換失敗は文全体を取り消す。

```sql
INSERT INTO people VALUES
  (1, 'alice', 30),
  (2, 'bob', 25);

INSERT INTO people (name, id) VALUES ('carol', 3);
```

### 主キーとUNIQUE

主キーの重複はエラーになる。
複合主キーも値の組で重複を判定する。

`UNIQUE`制約と索引は重複を検査する。
`NULL`を含む場合は重複とみなさない。

複数行の更新は文全体の最終状態で一意性を検査する。
そのため、一意値の交換は成功する。

同時実行では、別々のtransactionが同じUNIQUE値を挿入すると、先にcommitした側が成功し、
後の側は `ConflictError`になる。

### INSERT ... SELECT

選択結果の列数は挿入先の列数または列リストの列数と一致させる。
選択は文の開始時点のsnapshotを読む。

```sql
INSERT INTO people (id, name)
SELECT id + 100, UPPER(name)
FROM people
WHERE age >= 30;

INSERT INTO stats (name, total)
SELECT name, SUM(salary)
FROM people
GROUP BY name;
```

`INSERT INTO t SELECT * FROM t`は開始時点の行だけを複製し、無限に増殖しない。

`ON CONFLICT`と `ON DUPLICATE KEY UPDATE`は未対応。

## 9. UPDATEとDELETE

### UPDATE

```sql
UPDATE table SET column = expr [, ...] [WHERE expr];
```

- 式は更新前の行に対して評価する。
- サブクエリと外側の列参照を使える。
- `WHERE`を省略すると全行を更新する。
- テーブル別名は使えない。

主キーの変更はできない。

```sql
UPDATE people SET age = age + 1 WHERE id = 1;
UPDATE people SET salary = salary * 1.1;
```

主キーへ同じ値を代入する場合は成功する。

索引のあるテーブルでは、更新した行の索引エントリだけを更新する
（変わらない索引はそのまま）。

### DELETE

```sql
DELETE FROM table [WHERE expr];
```

- `WHERE`を省略すると全行を削除する。
- テーブル別名は使えない。
- 対象が0行でもエラーにはならない。
- 主キーとすべての索引へ墓標を追加する。

```sql
DELETE FROM people WHERE age < 18;
DELETE FROM people;
```

`UPDATE ... FROM`、`UPDATE ... LIMIT`、`DELETE ... USING`、`DELETE ... LIMIT`は未対応。

## 10. DDL

### 10.1 CREATE TABLE

```sql
CREATE TABLE [IF NOT EXISTS] table (
  column_def [, ...]
  [, PRIMARY KEY (column [, ...])]
  [, UNIQUE (column [, ...])]
  [, FOREIGN KEY (column [, ...])
     REFERENCES table [(column [, ...])]]
);
```

```sql
CREATE TABLE people (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT UNIQUE,
  age INTEGER DEFAULT 0,
  UNIQUE (name, email)
);
```

#### 制約

| 制約          | 効果                                                      |
| ------------- | --------------------------------------------------------- |
| `NOT NULL`    | `NULL`の挿入を拒否する                                    |
| `PRIMARY KEY` | 主キーを定義する。テーブルに1つだけ、各列をNOT NULLにする |
| `UNIQUE`      | 重複を拒否する。`NULL`は重複とみなさない                  |
| `DEFAULT`     | 値省略時に定数式を使用する                                |
| `REFERENCES`  | 外部キーを定義する                                        |
| `FOREIGN KEY` | 複合外部キーを定義する                                    |

主キーは省略できない。
列レベルとテーブルレベルの併用、複数の主キー定義は拒否する。

列名の重複、テーブルレベル制約の存在しない列、重複列はエラーになる。
型を省略した列は `TEXT`になる。

```sql
CREATE TABLE t (id INTEGER PRIMARY KEY, name NOT NULL);
```

### 10.2 CREATE INDEX

```sql
CREATE [UNIQUE] INDEX [IF NOT EXISTS] index_name
ON table_name (column [, ...]);
```

- 既存行を索引へ投入する。
- 索引エントリのキーは索引列と主キー、値は空である。
- 被覆しない列は主キーlookupで取得する。
- `UNIQUE`は重複を検査する。`NULL`は除外する。
- バックフィル中はプロセス内で排他する。
- 明示transaction中の索引作成は拒否する。

```sql
CREATE INDEX idx_dept ON people (dept_id);
CREATE UNIQUE INDEX idx_email ON people (email);
```

`__unique_`で始まる索引名はUNIQUE制約用に予約される。

### 10.3 DROP

```sql
DROP TABLE [IF EXISTS] table_name;
DROP INDEX [IF EXISTS] index_name;
```

テーブルと索引の削除は論理定義を先に外し、エントリへ順次tombstoneを追加する。
失敗時の後始末は再オープン時に再開する。

暗黙の `UNIQUE`索引は個別に `DROP`できない。

### 10.4 ALTER TABLE

```sql
ALTER TABLE table ADD [COLUMN] column_def;
ALTER TABLE table DROP [COLUMN] column_name;
ALTER TABLE table RENAME [COLUMN] old_name TO new_name;
```

#### ADD COLUMN

- 列を末尾へ追加する。
- 既存行には `DEFAULT`、または `NULL`を設定する。
- `NOT NULL`には `DEFAULT`が必要になる。
- 主キー、UNIQUE、外部キー制約の追加はできない。

#### DROP COLUMN

- 既存行から列を削除する。
- 主キー列、索引列、外部キー列は削除できない。
- 削除列が索引列でない場合は索引keyspaceを書き換えない。

#### RENAME COLUMN

- 列名を変更する。
- 主キー、索引、外部キーの定義へ変更を反映する。
- 新しい名前が既存列と重複する場合はエラーになる。

ALTER TABLEの行変更は 1 transactionで行い、失敗時はカタログと行を巻き戻す。
明示transaction中はDDLを実行できない。

テーブル名の変更と列型の変更は未対応。

### 10.5 ANALYZE

```sql
ANALYZE [TABLE] [table_name] [SAMPLE n];
```

テーブルの行数、列ごとの異なり値数（NDV）、NULL 数、数値列の最小/最大、
索引の prefix NDV を収集し、カタログへ保存する。
テーブル名を省略すると全テーブルを対象にする。

- 結合順、結合アルゴリズム、走査基数、Filter 選択性の見積りに使う。
- 結果は `table` / `rows` / `columns` / `ndv` の 1行（全テーブルならテーブルごとに 1行）。
- `SAMPLE n` は reservoir sampling で最大 n 行を標本にする。
  行数は全体を数え、NDV・範囲・NULL 率は標本から推定する。
- 統計はDML（`INSERT` / `UPDATE` / `DELETE`）では自動更新しない。
  更新後は必要に応じて再度 `ANALYZE` を実行する。
- `ALTER TABLE` は対象テーブルの統計を削除する。
- 明示transaction中は実行できない（DDLと同じ扱い）。

統計が無いテーブルは、走査方法から得る粗い定数で見積もる。

### 10.6 外部キー

```sql
CREATE TABLE orders (
  id INTEGER PRIMARY KEY,
  person_id INTEGER REFERENCES people (id)
);

CREATE TABLE order_lines (
  id INTEGER PRIMARY KEY,
  oid INTEGER,
  line_no INTEGER,
  FOREIGN KEY (oid, line_no)
    REFERENCES order_items (oid, line_no)
);
```

参照先は親テーブルの主キーだけに制限する。
親の列を省略すると主キー全列を参照する。
列を指定する場合は、主キーと同じ列数・列順で指定する。

検査は即時RESTRICTで行う。

- 子の `INSERT`または `UPDATE`で親行がない場合は `ConstraintViolation`になる。
- 外部キーに `NULL`を含む行は参照検査をスキップする。
- 親の `DELETE`で参照中の子がある場合は拒否する。
- 親の主キーは更新できない。

参照アクション、MATCH、DEFERRABLE、名前付き制約は未対応。

参照されている親テーブルは `DROP TABLE`できない。
外部キー列は `DROP COLUMN`できない。

自己参照も可能。
ただし参照する行が残る間は親行を削除できない。

参照検査は**行を書いた後**の状態で行う。

- 自分自身の主キーを参照する行は `INSERT` / `UPDATE` できる（`(1, 1)` / `SET mgr = id`）。
- 同じ文の先行行は参照できるが、後続行はまだ書かれていないので参照できない
  （`VALUES (2, 1), (1, NULL)` は `(2, 1)` の時点で違反）。
- 複数行の `UPDATE` も行の順に検査するので、別の行の新しい値を親にする更新は順序によっては失敗する。

## 11. トランザクション

### 11.1 SQLによる制御

```sql
BEGIN;
INSERT INTO t VALUES (1, 'a');
INSERT INTO t VALUES (2, 'b');
COMMIT;

BEGIN;
DELETE FROM t WHERE id = 1;
ROLLBACK;
```

次の別綴りを受け付ける。

- `BEGIN TRANSACTION`、`START TRANSACTION`
- `COMMIT WORK`、`COMMIT TRANSACTION`、`END`
- `ROLLBACK WORK`、`ROLLBACK TRANSACTION`

`BEGIN`を重ねても新しいtransactionは開始しない。
明示transaction中にはDDLを実行できない。

1 transactionの書き込みバッファ上限は256MiBである。
超過時は `CapacityError`になる。

### 11.2 暗黙のtransaction

`Begin`を呼ばない `Execute`は、文ごとにtransactionを開始し、自動commitする。
複数文を渡した場合も、明示transaction中でなければ文ごとにcommitする。

### 11.3 ライブラリAPI

```fsharp
let session = ok (Session.OpenWith "./mydata")
ok (session.Begin())
ok (session.Execute "INSERT INTO t VALUES (1, 10)") |> ignore
ok (session.Commit())
ok (session.Close())
```

破棄する場合は `Rollback()`を呼ぶ。

- `Close()`後の操作は `InternalError`になる。
- `Close()`は進行中のtransactionをrollbackする。
- `Session`は `IDisposable`で、`Dispose()`と `Close()`は同じ経路になる。
- 複数スレッドで同じtransactionを共有しない。

### 11.4 SessionとSessionPool

同じDBを別プロセスから開くことはできない。
同じプロセスで同じrootを二度開くこともできない。
複数接続では `SessionPool.OpenWith`と `pool.CreateSession()`を使う。

```fsharp
let pool = ok (SessionPool.OpenWith "./mydata")
let session = ok (pool.CreateSession())
```

明示transactionは接続ごとに独立する。
未commitの変更を別の接続からは見られない。

読み取りだけのSELECTは複数接続・スレッドから並行実行できる。
書き込み、BEGIN、COMMIT、ROLLBACK、CloseはDB全体で直列化する。

読み取りは呼び出し開始時点のsnapshotを見る。
実行中の書き込みを待たない。
書き込みも実行中の読み取りを待たない。
Closeだけは読み取り完了を待つ。

### 11.5 隔離レベル

分離レベルはSnapshot Isolationで固定する。

- 他のtransactionの未commit変更は見えない。
- transaction内で同じ行を読み直すと、後続のcommitは見えない。
- snapshot取得後に別transactionが先に同じキーをcommitした場合、
  自分のcommitは `ConflictError`になる。

`BEGIN ISOLATION LEVEL`、`SET TRANSACTION`、`SAVEPOINT`、読み取り専用transactionは未対応。

### 11.6 複数文の実行

複数文を `Session.Execute`へ渡した場合、途中の文でエラーになっても、成功した文は確定済みのまま残る。
明示transaction中のエラーはtransactionを開いたままにするので、必要に応じて `Rollback()`する。

## 12. エラー

失敗は例外ではなく `DbError`で返す。

| 分類                  | 意味                                  |
| --------------------- | ------------------------------------- |
| `SyntaxError`         | 文法エラー                            |
| `NameError`           | 名前解決・未知の関数                  |
| `TypeError`           | 型不一致・変換失敗                    |
| `ConstraintViolation` | NOT NULL、主キー、外部キー、0除算など |
| `NotFound`            | テーブル・索引・列の不在              |
| `AlreadyExists`       | 重複作成                              |
| `CapacityError`       | memtable、バッファ、結果件数の上限    |
| `CorruptionError`     | チェックサム不一致やデータ破損        |
| `ConflictError`       | MVCCのwrite-write競合、DDLとの競合    |
| `IoError`             | ファイルI/O失敗                       |
| `InternalError`       | 内部不整合                            |

字句・構文解析エラーには行・列の位置を付ける。

```text
エラー: JOIN には FROM が必要です (位置 1行 10列)
```

主なエラー例を示す。

| SQLまたは操作              | メッセージ                                                                          |
| -------------------------- | ----------------------------------------------------------------------------------- |
| 存在しないテーブルの参照   | `テーブル 'nosuch' が存在しません`                                                  |
| 主キーなしのCREATE TABLE   | `テーブル t に主キーがありません`                                                   |
| 重複したテーブル名         | `テーブル t は既に存在します`                                                       |
| NOT NULL違反               | `NOT NULL 制約に違反: t.n`                                                          |
| 型変換失敗                 | `''abc'' は 'INTEGER' に変換できません`                                             |
| 0除算                      | `0 での除算は定義されていません`                                                    |
| BOOLEANとINTEGERの比較     | `型 'BOOLEAN' と 'INTEGER' は比較できません`                                        |
| スカラーサブクエリが複数行 | `スカラーサブクエリが 2行以上を返しました`                                          |
| 列数の違うUNION ALL        | `UNION ALL の列数が一致しません`                                                    |
| テーブル別名付きDML        | `INSERT / UPDATE / DELETE の対象テーブルに別名は使えません（別名を外してください）` |

未対応の後置句は専用エラーにならず、文の区切りエラーとして報告される場合がある。
`TEXT`値の変換エラーは、値を引用符付きで表示するため引用符が二重になる。

## 13. 既知の制限

### 未対応構文

- 相関派生テーブル（LATERAL）
- `UNION`、`INTERSECT`、`EXCEPT`
- CTE、再帰クエリ
- ウィンドウ関数
- `RIGHT JOIN`、`FULL JOIN`、`CROSS JOIN`
- 自然結合、`JOIN ... USING`、カンマ結合
- `ON CONFLICT`、upsert
- 外部キーの参照アクション、MATCH、DEFERRABLE、名前付き制約
- `UPDATE ... FROM`、`UPDATE / DELETE`のLIMIT、`DELETE ... USING`
- `SAVEPOINT`
- 分離レベル指定と読み取り専用transaction
- テーブル名の変更と列型の変更
- `TRUNCATE`、`RETURNING`
- ユーザ定義関数、トリガー、ビュー、ストアドプロシージャ
- 名前付きパラメータ
- パーティショニング、圧縮、暗号化

### 実装上の制約

| 項目               | 制約                                                   |
| ------------------ | ------------------------------------------------------ |
| 索引               | 単一列・複合索引、複合索引の前置等値、`LIKE 'prefix%'` |
| `LIMIT` / `OFFSET` | 0以上の定数式のみ                                      |
| 型引数             | `DECIMAL(p, s)`の桁数は検証しない                      |
| ソート             | 外部マージソート。既定メモリ64MiB                      |
| DISTINCT           | 作業メモリ256MiBを超えると退避                         |
| 結合               | 作業メモリ256MiBを超えると退避                         |
| GROUP BY           | グループを一時ファイルへ退避                           |
| INサブクエリ       | メモリ上限を超えたら値集合を退避し、Bloom filterで絞る |
| 最終結果           | 既定で100万行・256MiB。超過時は `CapacityError`        |
| TryOpenReader      | 単一SELECTのみ。行数上限を適用しない                   |
| UPDATE / DELETE    | FROM、USING、LIMIT句なし                               |
| 外部キー           | 親の主キーだけ。即時RESTRICT                           |
| DML別名            | 対象テーブル別名なし                                   |
| DDL                | 明示transaction中に実行不可                            |

`SessionOptions.MaxResultRows` / `MaxResultBytes`で最終結果の上限を、
`WorkMemBytes` / `SortBufferBytes` / `SpillDirectory`で作業領域を変更できる。
数値上限は `-1`で無制限になる。

旧形式のデータベースは開けない。
形式マーカーがないデータや、行全体を索引値に保持する旧形式は `CorruptionError`になる。

### 演算の注意

- 整数除算は切り捨てる。
- `DATE`と `TIMESTAMP`は相互比較できる。
- `REAL`の `NaN`は `REAL`同士でのみ最大値として扱う。
- `NaN`と整数・decimalの比較、無限大とdecimalへの変換はエラーになる。
- `-0.0`と `+0.0`は同じ値として扱う。主キーとUNIQUE索引も同じキーになる。

## 14. 例

### スキーマ作成

```sql
CREATE TABLE depts (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE people (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  dept_id INTEGER,
  age INTEGER,
  salary DECIMAL,
  hired DATE,
  active BOOLEAN DEFAULT TRUE
);

INSERT INTO depts VALUES (1, 'eng'), (2, 'sales');

INSERT INTO people VALUES
  (1, 'alice', 1, 30, 100.5,  '2024-01-15', TRUE),
  (2, 'bob',   1, 25, 200.25, '2023-06-01', FALSE),
  (3, 'carol', 2, 41, 150.0,  '2022-03-10', TRUE);

CREATE INDEX idx_people_dept ON people (dept_id);
CREATE INDEX idx_people_name ON people (name);
```

### SELECTと集約

```sql
SELECT id, name
FROM people
WHERE dept_id = 2;

SELECT p.name, d.name AS dept
FROM people p
JOIN depts d ON p.dept_id = d.id
ORDER BY p.name;

SELECT d.name AS dept,
       COUNT(*) AS headcount,
       AVG(p.salary) AS mean
FROM depts d
JOIN people p ON p.dept_id = d.id
GROUP BY d.name
HAVING COUNT(*) >= 1
ORDER BY d.name;
```

### 更新と削除

```sql
UPDATE people SET age = age + 1 WHERE id = 1;
DELETE FROM people WHERE id = 3;
```

### エラー

```sql
CREATE TABLE t (a INTEGER);
-- エラー: テーブル t に主キーがありません

SELECT * FROM nosuch;
-- エラー: テーブル 'nosuch' が存在しません

CREATE TABLE u (id INTEGER PRIMARY KEY, n TEXT NOT NULL);
INSERT INTO u VALUES (1, NULL);
-- エラー: NOT NULL 制約に違反: u.n

SELECT (id) / 0 FROM u;
-- エラー: 0 での除算は定義されていません
```
