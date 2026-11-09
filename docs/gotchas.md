# 開発で踏んだ落とし穴

このプロジェクトで**実際に発生し、修正した**バグと、その原因・対策をまとめたもの。
「設計の意図」ではなく「間違えると何が壊れるか」を軸にした記録。

---

## 目次

1. [特に注意が必要な 5つのカテゴリ](#1-特に注意が必要な-5つのカテゴリ)
2. [pull 演算子（`Op`）の罠](#2-pull-演算子op-の罠)
3. [`Optimizer.exec` の罠](#3-optimizer-exec-の罠)
4. [LSM / ストレージの罠](#4-lsm--ストレージの罠)
5. [MVCC / トランザクションの罠](#5-mvcc--トランザクションの罠)
6. [索引（index）の罠](#6-索引index-の罠)
7. [SQL 意味論の罠](#7-sql-意味論の罠)
8. [F# 10 の言語の罠](#8-f-10-の言語の罠)
9. [テスト・ビルドの罠](#9-テスト・ビルドの罠)
10. [デバッグ手法](#10-デバッグ手法)
11. [設計上の注意点](#11-設計上の注意点)

---

## 1. 特に注意が必要な 5つのカテゴリ

| #   | 分類                        | 症状                                                 | 原因                                                            |
| --- | --------------------------- | ---------------------------------------------------- | --------------------------------------------------------------- |
| 1   | **スタックオーバーフロー**  | テストプロセスが `SIGABRT`（exit 134）で異常終了する | `>>=` チェーンで組んだ末尾再帰                                  |
| 2   | **pull 演算子の継続飛ばし** | 射影が最初の 1行だけ / JOIN の 2行目以降が消える     | 継続に「次の行」ではなく「次のループ」を渡した                  |
| 3   | **SeqNo の取り違え**        | tombstone が消える / reopen 後 0行                   | put と delete で同じ SeqNo、または reopen で SeqNo を再開しない |
| 4   | **O(n) な操作の混入**       | ポイント検索 43ms                                    | `rangeScan` をポイント検索に使った                              |
| 5   | **F# の型推論の破綻**       | 謎の FS0001 / FS0072                                 | 引数の型注釈・スコープ                                          |

---

## 2. pull 演算子（`Op`）の罠

`Op.fs` の `Op` は、「残りの計算」を値として持ち回る不変な pull 演算子
（Volcano 方式 / イテレータモデル）。

```fsharp
type Op =
    | Done
    | Step    of SqlValue array * Op                                    // 遅延しない
    | Compute of (unit -> Result<(SqlValue array * Op) option, DbError>) // 遅延する
```

### 2.1 `Step` は遅延ではない（最重要）

`Step(row, nextOp)` の **`nextOp` は「次の行の値を返す」のではなく「継続を表す値」**。
F# は評価が厳しいので `Step(f x, g y)` と書くと **`g y` が先に計算される**。

```fsharp
// ❌ 無限再帰になる（Step は遅延ではない）
let rec loop () = Op.Step(row, loop ())

// ✅ 遅延が必要なら Compute
let rec loop () = Op.Compute(fun () -> ...)
```

**`Step` は「有限の既知の連鎖」専用**。
無限系列は `Compute` で組む。

有限の連鎖でも、**再帰を第 2引数（または `let rest = ...`）に渡すとコンストラクト時に「深さ = 要素数」のスタック**を積む。

```fsharp
// ❌ コンストラクト時に全行ぶんのスタックを積む（行数比例で Stack overflow）
let rec go (i: int) : Op =
    if i >= rows.Length then Done
    else
        let rest = go (i + 1)      // ← これが先に評価される
        Step(rows.[i], rest)

// ✅ Compute で「次を要求されてから 1行だけ」作る
let rec go (i: int) : Op =
    if i >= rows.Length then Done
    else Compute(fun () -> ret (Some(rows.[i], go (i + 1))))
```

### 2.2 継続を「別ループ」に飛ばすと結果が落ちる

```fsharp
// ❌ filter が効かない（偽の行がそのまま流れ落ちる）
let rec go (src: Op) : Op =
    Compute(fun () ->
        next src
        >>= function
        | Some(row, nxt) -> ret (Some(row, go nxt))   // ← ここが `nxt`
        | None -> ret None)
```

`go nxt` は「**まだ述語を通していない**残り」を表すので、正しくは `nxt` をそのまま渡す。
`go nxt` にすると「述語を通した列」と「通していない列」が交互に出る。

**同じ罠は `Op.map` / `innerJoin` / `leftJoin` にもあった**。

```fsharp
// ❌ 内側ループの 2行目以降が消える（JOIN で 3行あるはずが 2行になる）
| Ok true -> Op.Step(Array.append lrow rrow, go lnext)
// ✅ 同じ左行の「次の右行」へ続ける
| Ok true -> Op.Step(Array.append lrow rrow, inner rest lrow lnext)
```

教訓: **「継続」は必ず「今の位置の残り」を表す。ループを作り直さない**。

### 2.3 `Op.toArray` に `>>=` チェーンを使うとスタックオーバーフロー

```fsharp
// ❌ 10万行で SIGABRT
let rec go (cur: Op) (acc: SqlValue array list) =
    next cur
    >>= function
    | None -> ret (List.rev acc |> List.toArray)
    | Some(row, nxt) -> go nxt (row :: acc)
```

`>>=` が F# の末尾呼び出し最適化を**妨げる**ため、フレームが 1行ずつ増える。
`Array.take` で最後に `List.rev` する形は **O(n²)** でもある。

```fsharp
// ✅ 末尾位置の match + let rec。フレームが増えない。
let rec go (cur: Op) : Result<SqlValue array array, DbError> =
    match next cur with
    | Error e -> Error e
    | Ok None -> Ok(rows.ToArray())
    | Ok(Some(row, nxt)) ->
        rows.Add row
        go nxt
```

教訓: **「再帰 + `>>=`」組み合わせは使わない**。**`match` を書いて末尾位置を保つ**。

### 2.4 `Ops.distinct` は「直前の値」比較では書けない

`2, 1, 2` の 3つ目の `2` は直前の `1` と違うため、直前との比較では重複を除去できない。

```fsharp
// ❌ 2 が 2回目に出るので除去されない
// ✅ 既出の値をすべて覚えておく必要がある（符号化キーの HashSet<byte array>）
```

### 2.5 評価エラーを `Op.empty` で「0行」にしない

JOIN の ON 句のように行ごとに評価する式が失敗したとき、`Op.empty` に置き換えると
**エラーが消えて「一致なし」の結果**になる。
`INNER JOIN` では空の結果に、`LEFT JOIN` では NULL 埋めの行になり、どちらも原因を隠してしまう。

```fsharp
// ❌ 評価失敗を「行が無い」と区別できない
| Error _ -> Op.empty
// ✅ エラーを Op の計算として流し、呼び出し元の Result まで届ける
| Error e -> Op.Compute(fun () -> err e)
```

### 2.6 `Op` を返す関数を「値」にすると状態が全クエリで共有される

F# のモジュールスコープ `let` は起動時に 1回だけ評価される。
`Ops.distinct` を引数なしの値として書くと、内側の `HashSet`（既出行）も 1個だけ作られ、
**同じプロセスで 2回目以降の `SELECT DISTINCT` が全行を重複と見なして 0行**になる。
サーバでは他セッションの行まで混ざり、`HashSet` は非スレッドセーフなので並行時に壊れる。

```fsharp
// ❌ seen がモジュール初期化時の 1個で固定され、全クエリで共有される
let distinct =
    let seen = HashSet<byte array> HashIdentity.Structural
    let rec skipSeen used cur = ...
    skipSeen 0L
// ✅ src を引数に取り、seen を呼び出しごとに作る
let distinct (src: Op) =
    let seen = HashSet<byte array> HashIdentity.Structural
    let rec skipSeen used cur = ...
    skipSeen 0L src
```

- 部分適用で関数を作るときも、可変な状態を呼び出しをまたいで共有しないこと（純粋な関数だけにする）。
- 気付き方: 単体のテストは成功するが、他のテストと並行実行すると 0行になる（xUnit の並列実行で表面化）。
- 回帰テスト: `SqlReference: DISTINCT を続けて実行しても毎回全行が返る`。

---

## 3. `Optimizer.exec` の罠

### 3.1 下へ渡すのは「入力スキーマ」

```fsharp
// run の EvalNode 分岐（継続は ExecFrame list に積む）
| Logical.Sort(keys, child) ->
    let childSchema = outputOf child inSchema               // ← child の出力スキーマ
    run ctx (SortFrame(keys, childSchema) :: frames) (EvalNode(child, childSchema)) // ← ❌ 破綻
```

`outputOf child inSchema` は「**child の出力**スキーマ」であって、「child を**実行**するときの**入力**スキーマ」ではない。

これを間違えると、集約ノードが「集約後のヘッダ（`SUM(t.age)` など）」を**入力**として受け取る:

- `SELECT SUM(age) FROM t` → `SUM` が `NULL` になる（引数の `t.age` を評価できない）
- `GROUP BY age` → `id` でグループ化してしまう

```fsharp
// ✅ childSchema は「結果を読む側」だけが使う。下へは inSchema を渡す。
| Logical.Sort(keys, child) ->
    let childSchema = outputOf child inSchema
    run ctx (SortFrame(keys, childSchema) :: frames) (EvalNode(child, inSchema))
```

### 3.2 `Aggregate` は「包みを剥がして」探す

`Project` の `child` は `Limit → Sort → Distinct → Aggregate` と包まれている。
直接 `Aggregate` を match すると**見つからない**。

```fsharp
// ✅ 包みを全部剥がして探す
let rec private groupByOf (tree: Logical) : Expr list =
    match tree with
    | Logical.Aggregate(groupBy, _, _) -> groupBy
    | Logical.Having(_, _, _, child) -> groupByOf child
    | Logical.Distinct child -> groupByOf child
    | Logical.Sort(_, child) -> groupByOf child
    | Logical.Limit(_, _, child) -> groupByOf child
    | Logical.Filter(_, child) -> groupByOf child
    | _ -> []
```

### 3.3 `Ops.sortBy` は「修飾つきを先に」試す

集約後の出力では列名が `t.age` のまま残るので、修飾子を先に剥がすと「列 age が存在しません」になる。

```fsharp
// ✅ 修飾つき → 修飾なしの順で試す
match Schema.resolve schema c with
| Ok i -> Ok(i, dir)
| Error _ ->
    Schema.resolve schema bare |> Result.map (fun i -> (i, dir))
```

### 3.4 `Join` の右部分木を捨てると WHERE が消える

`buildLogical` は WHERE を各スキャンの直下へ pushdown して右部分木に持たせる。
なのに `exec` は `execScan ctx name method` を呼んで **`rTree` を捨てていた**:

- 右テーブルだけを参照する `WHERE b.v = 100` が効かなくなり、**全行**が返る
- 2テーブルをまたぐ WHERE は pushdown 先がなく、どこにも置かれないまま**エラーにならず無視される**

```fsharp
// ❌ rTree（pushdown した Filter を含む）が未使用
| Join(kind, name, method, on, lTree, rTree) ->
    exec ctx lTree schema >>= fun lOp ->
        execScan ctx name method >>= fun rOp -> ...
// ✅ 右部分木をそのまま実行する
| Join(kind, name, on, lTree, rTree) ->
    exec ctx rTree rightSchema >>= fun rOp -> ...
```

あわせて **LEFT JOIN の右（null 供給側）には WHERE を pushdown してはいけない**。
入力側へ適用すると NULL 拡張行を残すかどうかの判断が変わるため、JOIN の上で評価する必要がある。
`WHERE b.v IS NULL`（未一致行だけ残す）は、入力側へ先に適用すると**全行**が残ってしまう。

- 修正: `buildLogical` は INNER だけ pushdown し、LEFT の WHERE は top（JOIN の上）に置く。
  どのテーブルにも属さない述語も top に置く。
- 回帰テスト: `SqlReferenceTests` の `SqlReference: JOIN の WHERE は右テーブルと横断述語に効く` /
  `SqlReference: LEFT JOIN の WHERE は null 供給側に効く`

### 3.5 入れ子 `Join` に「全スキーマ」を渡すと 3テーブル結合が 0行になる

`buildLogical` は `List.fold` で `Join(Join(a, b), c)` と**左深い木**を組む。
`exec` が左部分木へ `schema`（全テーブル分）をそのまま渡していたので、
内側の JOIN は「自分と左のテーブル」ではなく「全テーブル」のスキーマで `ON` 句を解決してしまう。

- 症状: `FROM a JOIN b ON … JOIN c ON …` が**エラーにならず 0行**を返す
  （2テーブル結合は正しく動く）。
  `joinPred` の `Schema.resolve` が列を見つけられず `Error` →
  `joinLoop` の `onError` が `Op.empty` を返すため、**エラーにならず空の結果になる**。
- 原因: `schema − 右テーブル` は段ごとに計算していたが、
  左へ渡すのが**全スキーマ**のままだったこと。

```fsharp
// ❌ 左部分木に全スキーマを渡す（内側で外側のテーブルまで解決してしまう）
run ctx (JoinRightFrame frame :: frames) (EvalNode(lTree, inSchema))

// ✅ 積む側で「自分の右テーブルを除いたスキーマ」を左へ下ろす
run ctx (JoinRightFrame frame :: frames) (EvalNode(lTree, leftSchema))
```

あわせて `exec l >>= fun lOp -> exec r >>= …` は JOIN の数だけフレームが積もるので、
`JoinRightFrame`（左を評価し終えたら右を評価）と
`JoinApplyFrame`（右も評価し終えたら結合）を `ExecFrame list` に積む。
ラッパも含めて単一の `run` の自己再帰で回すので、
評価順（left → right → join）を保ったまま末尾再帰になる。

- 回帰テスト: `SqlReferenceTests` の `SqlReference: 3 テーブルの入れ子 JOIN が行を返す` /
  `DatabaseTests` の `末尾再帰: 60 段の入れ子 JOIN が正しい行を返す`（深さ方向の累積を検証）

### 3.6 JOIN の対応付けをテーブル名で探すと自己結合が消える

`buildLogical` は `bindings |> List.findIndex (fun b -> b.RealName = j.Table.Name)` で
「その JOIN 句がどのバインディングか」を探していた。
`FROM t AS x JOIN t AS y`（自己結合）だと**両方 `RealName = "t"`** なので先頭にヒットし、
`| idx when idx = 0 -> …` で **JOIN が丸ごと省略**される。

- 症状: `SELECT p.id, q.v FROM t AS p JOIN t AS q ON p.id = q.id` が
  `列インデックス 3 が範囲外です（2列中）: q.v` で落ちる
  （JOIN が無いので p の 2列しか無く、q を参照する射影が外れる）。
  q を参照しない場合は、**エラーにならず p の全行**を返す。
- ついでに、F# の `List.findIndex` は見つからないと**例外を投げる**。
  コードは `| -1 -> …` で判定していた（.NET の `List.FindIndex` と取り違えていた）。
  そのため未マッチの経路は実質存在しなかった。

```fsharp
// ❌ RealName で探す（重複時に先頭へヒット、未マッチは例外）
let idx = bindings |> List.findIndex (fun b -> b.RealName = j.Table.Name)

// ✅ sources と同順なので「i 番目の JOIN 句 = bindings.[i + 1]」で位置対応
let right = bindings.[i + 1]
```

あわせて**識別名が重複する** `FROM t JOIN t`（エイリアス無し）は
`Plan.runSelect` で `ErrorKind.NameError` にする。
exec が「右テーブルを除いたスキーマ」を**名前で**引く（§3.5）ため、
重複名を区別できず、誤った結果がエラーにならず返る。
自己結合は必ずエイリアスで表す。

- 回帰テスト: `SqlReferenceTests` の `SqlReference: エイリアスでの自己結合が行を返す` /
  `SqlReference: テーブル名の重複はエラー（エイリアスが必要）`

---

## 4. LSM / ストレージの罠

### 4.1 ポイント検索に `rangeScan` を使うと O(n) になる

```fsharp
// ❌ ポイント検索なのに「そのキー以降の全エントリ」を列挙している
let fromMem =
    Memtable.rangeScan key 0 engine.Mem
    |> Seq.map (...)
    |> firstVisible visible key
```

10万行で **1回あたり 43ms**。
`SELECT COUNT(*)` 全体（10万行走査）が 0.2秒なので、ポイント検索のほうが**全走査より 200倍遅い**という異常な状態だった。

```fsharp
// ✅ ポイントルックアップ（O(log n)）を使う
let fromMem =
    match Memtable.getVisible key visible engine.Mem with
    | None -> ret None
    | Some e -> ret (Some(SstEntry.make e.Key e.SeqNo e.Value))
```

教訓: **ポイント検索・prefix 検索に `rangeScan` を使わない**。**`getVisible` を使う**。

> `Memtable.rangeScan` は遅延 seq なので、❌ の形でも必要な分しか実体化しない
> （`startKey` 以降を 1件ずつ返す）。
> それでも `getVisible` のほうが軽い（列挙子も `Result` も作らず、O(log n) で直接引く）ので方針は不変。

### 4.2 `scanAt` の `startKey` が SSTable に効いていない（**再オープン後にだけ表面化する**）

```fsharp
// ❌ 枝刈りだけして、中身は全件 merge している
let files =
    Version.allFiles engine.Version
    |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
// ... そのままだと startKey より前のエントリも混ざる
```

`memtable` 側は `Memtable.rangeScan` が下限を適用するので、**同一プロセス内では動く**。
**reopen すると memtable が空になり SSTable だけが残るため、そこで初めて壊れる**。

```fsharp
// ✅ 下限は読み出し側で適用する
//   ファイルを選ぶ枝刈り（Largest >= startKey）と、中身の下限は別物。
//   → Sstable.rangeScanPartial が index を二分探索して startKey より前のブロックを読み飛ばし、
//     Memtable.rangeScan は木を途中から降りる。
let merged = SstMerge.ofScans (memScan :: fileScans)
// 可視性の判定と同一キーの最新版への畳み込みは、SstMerge から 1件ずつ取り出す
// 側（StorageEngine.scanAt の isVisible）が行う。
// memtable も各ファイルも startKey 以降しか返さないので、merge 後の絞り込みは不要
```

教訓: **「ファイルを選ぶ枝刈り」と「中身の下限」は別物。両方が必要**。
中身の下限はいま `Memtable.rangeScan` / `Sstable.rangeScanPartial` が持っているので、
`scanAt` 側で絞り直さない
（絞り直すと、下限が効かなくなった退行に気付けなくなる）。

### 4.3 prefix 検索では先頭の 1件が答えとは限らない

```fsharp
// ❌ scanAt の先頭が接頭辞と一致しないことがある
RowStore.scanAt ks prefix 0 visible store
|> Result.map (fun scan -> SstScan.next scan)

// ✅ 接頭辞が一致するか確かめてから取り出す
RowStore.scanAt ks prefix 0 visible store
|> Result.bind (fun scan ->
    SstScan.next scan
    |> Result.map (fun head ->
        match head with
        | Some(e, _) when KeyBuilder.startsWithPrefix e.Key prefix -> e.Value
        | _ -> None))
```

### 4.4 `Manifest.logAndApply` が `NextFileNumber` を進めていないとデータを消す

compaction の出力が入力と同じファイル番号になり、`deleteObsolete` が**自分で書いたファイルを消す**。
`logAndApply` 内で `max(追加ファイルの番号) + 1` まで進めること。

併せて `Compaction.compact` には `nextFileNumber` を**明示引数**で渡す
（`version.NextFileNumber` は MANIFEST 反映後にしか更新されない）。

### 4.5 `Version.filesForPointLookup` が `v.Files.[0]` だけ見ていた

compaction で L1+ に降ろしたデータが一切見つからなくなった。

```fsharp
// ❌ L0 しか見ていない
// ✅ 全レベルを走査し (level, fileNumber) の降順で並べる
Array.mapi (fun level files -> ...)
```

### 4.6 `Sstable.FooterSize` の変更は `readFooterFields` と合わせて行う

`MaxSeqNo` を追加したときは `FooterSize` を `72 → 80` に、
Footer 自身のチェックサムを追加したときは `80 → 84` に変更した。
v5 で全体 checksum を削除して `metadataChecksum` を追加したときは `88 → 84` に戻した。
**放置すると Footer の開始位置がずれて、ファイル全体の読み取りが壊れる**。

### 4.7 「状態を進めるヘルパー」は進んだ状態も返す（`scanAt` の無限ループ）

`scanAt` の k-way merge は、ヒープと「まだ開いていないファイル一覧（pending）」を持ち回る。
`prepare` がファイルを開いてヒープへ積むとき、**pending の残りも返さないと、
呼び出し側は毎行同じファイルを開き直し、同じ先頭エントリを積み続けて終わらない**。

```fsharp
// ❌ heap だけ返す → 次の行で同じファイルを再オープン（無限ループ）
let rec prepare heap pending = ...
// ✅ 使い切った pending も返す
let rec prepare heap pending =
    | [] -> Ok(heap, [])
    | (i, bound, f) :: rest -> ... Ok(heap', rest) / Ok(heap, pending)
```

症状は「SSTable だけの keyspace（Close 直後・再オープン直後）を読むと固まる」。
memtable に 1件でもあると先へ進むため、テストが無いと気づきにくい。
回帰テストは `StorageEngineTests` の
`StorageEngine.scanAt: memtable が空でも SSTable だけを走査できる` /
`StorageEngine.scanAt: 多数のファイルをキー昇順にマージする`。

---

## 5. MVCC / トランザクションの罠

### 5.1 put と tombstone に同じ SeqNo を書くと tombstone が消える

LSM の規則「同じキーでは SeqNo が大きい put が勝つ」により、
**同じ SeqNo の put と delete が同居すると、tombstone が負ける**。

```fsharp
// ❌ 全部 0UL にしていた
// ✅ 現在値から採番して単調増加させる
let nextSeq = engine.NextSeqNo
```

### 5.2 reopen 時に SeqNo を再開しないと「全部未来」で見えない

`StorageEngine.openDatabase` はディスク上の最大 SeqNo から `NextSeqNo` を復元する。
`RowStore.lastSeqNo` を通して `TransactionManager` の `LastCommitSeqNo` を初期化しないと、
**ディスク上の行がすべて「未来」** と判定されて **0行**になる。

```fsharp
// ✅ 開いた engine から SeqNo を復元する
let lastSeqNo (store: RowStore) =
    let next = store.Engine.NextSeqNo
    if next = 0UL then 0UL else next - 1UL
```

### 5.3 GC の SeqNo 判定は「snapshot が小さいほどまだ読まれている」

snapshot `S` の読み手は `SeqNo <= S` の版を見る。
つまり **SeqNo が小さい版こそ、まだ読まれている**。

```fsharp
// ❌ データ損失になる誤った判定
let obsolete = Version.allFiles version
               |> List.filter (fun f -> f.MaxSeqNo < activeSnapshot)
```

正しい方針:

- **版（エントリ）単位の GC**: 同キーのうち SeqNo 最大の版は**必ず残す**。
- 最新版以外は `SeqNo > activeSnapshot` の版を全部残す。
  `SeqNo <= activeSnapshot` の版は **最新の 1件だけ**残す
  （最古の active snapshot が読むのはこの版）。
  それより古い版だけ捨てる。
- 最新版が `activeSnapshot` 以下なら、最新版自身が全 snapshot から見えるのでそれより古い版は全て捨てる
  （compaction の GC にある `keepOlder`）。
- 最下位レベルでも、**同じキーの古い版が残るなら tombstone は消さない**。
  消すと下位の古い値が復活する（`Compaction.gc` は `older.IsEmpty` のときだけ削除する）。
- **ファイル単位の回収は SeqNo で判定しない**。
  LevelDB と同じ「Version が参照していないファイル」だけ
  （`quarantineOrphans` が `.orphans/` へ退避する。削除はしない）。

### 5.4 `Transaction.Buffers` は「同一キーに put と delete を書くと打ち消し合う」

```fsharp
type Transaction =
    {
        Buffers: Map<string, Map<byte array, SqlValue array option>>
        //                                       ^^^^ これが option
    }
```

`Dml.updateRow` は「delete → put」ではなく「put だけ」で更新する
（LSM は新しい SeqNo の put が旧 put を隠す）。
主キーの変更は禁止されているため、変更する場合は DELETE + INSERT を使う。

索引側は `Dml.applyIndexChange` が「値が変わるときだけ古い索引キーへ tombstone → 新しい索引キーへ put」。

### 5.5 `TransactionManager.create ()` を 2回呼ぶと SeqNo が衝突する

テストで `TransactionManager.create ()` を 2回呼ぶと、
両方とも SeqNo 1 から採番し、2つ目の transaction の書き込みが**先に見えた上位の SeqNo に隠される**。

```fsharp
// ✅ beginTxn の戻り値を次の transaction に引き継ぐ
let (mgr, txn) = TransactionManager.beginTxn mgr
// ... 任意の処理 ...
let (mgr, txn2) = TransactionManager.beginTxn mgr
```

### 5.6 公開 API では `Session` を使う（`Database.Execute` は internal）

`Database` は internal の不変ハンドルで、`Execute` は新しいハンドルを返す。
公開 API の `Session` は現在のハンドルを保持し、各文の成功後に差し替えるため、
利用者が戻り値を引き継ぐ必要はない。

```fsharp
// ✅ Session が状態を進める
ok (session.Execute "INSERT ...")

// Database を直接使うのは internal のテストだけ。
// Execute は Statement を 1つ受け取り、次のハンドルを返す。
let db2, _ = db.Execute stmt
```

---

## 6. 索引（index）の罠

### 6.1 索引選択が緩いと「別の列の索引」を引く

`chooseScanMethod` の判定が:

```fsharp
// ❌ 1列の索引なら列名に関係なく当ててしまう
let found =
    idxes
    |> List.tryFind (fun i ->
        i.Columns |> List.forall (fun c -> Catalog.eqName c colName)
        || i.Columns.Length = 1)
```

`CREATE INDEX ON t (name)` と `ON t (age)` の両方があれば、
`WHERE name = 'bob'` が **age の索引**を引いて 0行になっていた。

```fsharp
// ✅ 索引の列が検索列と完全に一致するものだけを使う
let found =
    idxes
    |> List.tryFind (fun i ->
        match i.Columns with
        | [ c ] -> Catalog.eqName c colName
        | _ -> false)
```

- 等値一致は索引の**先頭から連続する** `= 定数` が 1列以上あれば `IndexLookup`
  （`Analyzer.chooseScanMethod` の `tryIndexEquality`）。
- 範囲条件がその次の列にある場合は、より多くの列を絞れる `IndexRangeScan` を優先（`tryIndexMethod`）。
- 複合索引の範囲は「前置列すべてが等値 + 次列が範囲」（`tryIndexRange`）。

### 6.2 `Catalog.toSchema` は `Indexes = []` で埋まる

索引を定義しても実行系の `TableSchema.Indexes` に載らないと、
`ExecContext.byIndex` が「索引が無い」と言って検索に失敗する。

```fsharp
// ✅ contextOf で明示的に載せる
let withIndexes =
    { schema with
        Indexes = Catalog.indexSchemasOf state.Catalog t.Name }
```

### 6.3 `CREATE INDEX` に backfill が無いと過去行が見つからない

索引は**別の論理 keyspace**（`i:<name>`）に持つので、`CREATE INDEX` 時に中身を写さないと、
**その前に `INSERT` した行が検索に出てこない**。

```fsharp
// ✅ 既存行をチャンクごとの transaction で index keyspace に書き込む
let! rows = Dml.scanByPrimaryKey cleared.Store withIndex 0 None

let! filled =
    applyOpChunks
        (fun txn row ->
            ret (Transaction.put (Keyspace.index idx.Name) (RowKey.indexKey withIndex idx row) row txn))
        rows
        cleared
```

`Dml.scanByPrimaryKey` は遅延 `Op` を返すので、
全行をメモリに載せず `BackfillChunkRows` 単位で transaction を分ける。

### 6.4 索引走査は `TableSchema` と `IndexSchema` の両方が要る

`RowKey.indexKey` は索引列の位置を `TableSchema.indexOf`（`Option`）で引き、見つからない列は除外する。
索引キーには索引列だけでなく主キー列も埋め込むため、`IndexSchema` だけでは完全なキーを作れない。

```fsharp
// ✅ スキーマ・索引 schema・値・limit・visible・txn を渡す
Dml.scanByIndexPrefixWithTxn store schema index values limit visible txn
```

---

## 7. SQL 意味論の罠

### 7.1 SQL の評価順は「集約 → 並び替え → 射影 → DISTINCT → LIMIT」

`DISTINCT` を射影の前にすると `(id, g)` の組を重複排除してしまう。

```fsharp
// ✅ DISTINCT は射影の後
let projected = Project(items, withSorted)
let deduped = if stmt.Distinct then Distinct projected else projected
applyLimit deduped stmt
```

### 7.2 `AggCall` の第 2引数は「DISTINCT フラグ」

`COUNT(*)` は**引数が `Star` かどうか**で `isStar` を判定する。

```fsharp
// ✅ COUNT(*) は Star で表す
AggCall(AggFunc.Count, false, Star)
// ✅ COUNT(DISTINCT a) は第 2引数が true
AggCall(AggFunc.Count, true, Name "a")
```

第 2引数を `isStar` と誤解すると `COUNT(DISTINCT a)` が解析できない。

### 7.3 `COUNT(*)` は NULL を数える。`COUNT(列)` は数えない

`addAcc` の `isStar` 分岐を**先に**置かないと、`Some Null` が「NULL を無視」側に落ちて **0** になる。

### 7.4 `InList` の未一致は FALSE であって UNKNOWN ではない

SQL 標準の 3値論理:

```fsharp
// ❌ 5 IN (1,2) が NULL になる
| [] -> ret Null
// ✅ 一致すれば TRUE / どれも一致しなければ FALSE /
//    NULL 比較が 1つでもあれば UNKNOWN
| [] -> if sawUnknown then ret Null else ret (Bool negated)
```

### 7.5 `LIKE` のバックトラックは「進捗保証」が無いと無限ループ

テストプロセスが **SIGABRT / exit 134** で落ちた。

`%` で分割 → 区間を左から順に前方一致で探す、という**単純な走査**に書き換えた。
`%` で `go (pi+1) ti pi ti` のように `ti` が進まず、`star+1` のリトライが永久に終わらない。

教訓: **バックトラックする再帰には必ず「進捗が必ず進む」保証を書く**。

---

## 8. F# 10 の言語の罠

### 8.1 主コンストラクタの中に副作用のある呼び出しを単独で書けない

```fsharp
// ❌ FS0010 "Unexpected identifier in member definition"
type T(host: string, port: int) =
    let client = new TcpClient()
    client.Connect(host, port)   // ← ここ
    member _.Port = port

// ✅ コンストラクタ引数で接続を成立させる
type T(host: string, port: int) =
    let client = new TcpClient(host, port)
    member _.Port = port
```

### 8.2 `Result` の共用体ケース名が衝突する

```fsharp
// ❌ Reply.Ok が Result.Ok を shadow して全部壊れる
type Reply = | Ok of ... | Error of string
// ✅ 名前を変える
type Reply = | Rows of ... | Failure of string | Halt
```

### 8.3 `let private rec` の順序は逆

```fsharp
// ❌ FS0110
let private rec f x = ...
// ✅
let rec private f x = ...
```

### 8.4 namespace 直下には値を置けない（FS0201）

```fsharp
// ❌ FS0201
namespace RykDb.Core
let myHelper x = ...     // ← 不可
// ✅ module に入れる
namespace RykDb.Core
module MyModule =
    let myHelper x = ...
```

### 8.5 型の member から同じファイルの後にある `let` を見られない

```fsharp
type Catalog =
    { Tables: TableDefinition list }
    // ← この member から下の updateTables が見えない
    member this.Count = this.Tables.Length

let updateTables t ... = ...   // ← member から参照不可
```

→ **検索・更新は module 関数**にする。

### 8.6 `all` は `seq` を要求する

リスト・配列はそのまま渡せる。
空リストリテラルだけは要素型が決まらないので注釈を付ける（FS0030）。

```fsharp
// ✅ リストも配列も渡せる
all [ r1; r2 ]

// ✅ 空のときは注釈で要素型を決める
let empty: Result<int list, DbError> = all []
```

### 8.7 `SqlValue` は `:?>` できない（FS0016）

proper subtype がないので型テストできない。

```fsharp
// ❌ FS0016
match v with :? SqlValue.Int n -> ...
// ✅ ヘルパー
let asInt (v: SqlValue) = match v with | SqlValue.Int n -> n | _ -> 0L
```

### 8.8 `Binary` などの「引数 3 つ」のケースはタプル形

```fsharp
// ❌ FS0727
| Binary(op, _, _) -> ...
// ✅
| Binary(op, a, b) -> ...
```

### 8.9 `take` 述語は「先頭から連続」で止まる

`Dml.scanByIndexPrefixWithTxn` は `RowStore.scanAt` が返す pull を
`opOfScan` の `take` 述語（`KeyBuilder.startsWithPrefix`）で読み進め、外れた時点で終端にする。
**ファイル側の下限が効いていないと 1件目で止まり、0件になる**（§4.2 を参照）。

### 8.10 予約語

`default` / `value` / `old` / `type` / `base` は変数名・引数名に使えない。

```fsharp
// ❌ base は予約語
let f (base: Schema) = ...
// ✅
let f (tree: Logical) = ...
```

モジュール名に使えないのもある（`CompactionOptions` の既定値は現在 `CompactionOptions.Defaults`）。

`key` / `base` / `begin` / `new` / `old` / `type` / `value` は**匿名レコードのフィールド名にも使えない**。

```fsharp
// ❌ 予約語と衝突
let private entryKey ... = { Key = ... }

// ✅ 動詞で始まる名前にする
let private hashKey (e: Entry) = ...
let private writeByte (w: Writer) = ...
```

モジュール名と型名が同じ（`Wal` / `Memtable`）場合、`Module.field` は型と誤認される。
型注釈付きのヘルパー経由で構築する。

### 8.11 ローカル関数名が外側の関数と被ると後者が優先される

```fsharp
// ❌ pickMax (2引数) に解決されて壊れる
let pickMaxFrom gs = pickMax (gs + 1) gs ge
// ✅ 名前を変える
let pickMaxFrom gs = pickMaxIdx (gs + 1) gs ge
```

### 8.12 `op_GreaterGreaterEquals` の連鎖で末尾再帰が壊れる

`Result` のbind を `>>=` で重ねると F# の末尾呼び出し最適化が効かない。
**「再帰 + `>>=`」の組み合わせは使わない**（§2.3 を参照）。

### 8.13 `use` は主コンストラクタに書けない（FS0523）

```fsharp
// ❌ FS0523
type T() =
    use stream = ...
// ✅ let で持って Dispose で破棄
type T() =
    let stream = ...
    interface IDisposable with
        member _.Dispose() = stream.Dispose()
```

### 8.14 `member _.Foo = _.Bar` の短縮ラムダは atomic 式のみ

```fsharp
// ❌ FS3584
member _.Query sql = _.Send sql
// ✅
member this.Query sql = this.Send sql
```

### 8.15 F# 10 では型注釈付きの戻り値宣言は 1行

```fsharp
// ❌ FS0058
let rec go (rest: T list) (acc: Map<string, int>) :
    Result<string list * Map<string, int>, DbError> =
    ...
// ✅ 引数リストを複数行にして、戻り値型も 1行で
let rec go
    (rest: T list)
    (acc: Map<string, int>)
    : Result<string list * Map<string, int>, DbError> =
    ...
```

---

### 8.16 `[<TailCall>]` は末尾位置に置けたときだけ付ける（FS3569）

`TreatWarningsAsErrors=true` なので、属性を付けた関数の自己再帰が 1つでも末尾位置に来ないと **FS3569 がビルドエラーになる**。
最小プローブで位置ごとに実測した結果は次のとおり。

| 書き方                                             | 判定      |
| -------------------------------------------------- | --------- |
| `match f x with` の**枝**で `f (n-1)`              | ✅ 末尾   |
| `match 別関数 x with` の枝で `f (n-1)`             | ✅ 末尾   |
| `if ... then f (n-1) else ...`（本体レベル）       | ✅ 末尾   |
| ネストした `let rec go` の本体                     | ✅ 末尾   |
| ラムダの本体が**そのまま** `f (n-1)`               | ✅ 末尾   |
| `match f x with`（自己再帰の結果がそのまま戻り値） | ❌ FS3569 |
| `f (n-1) \|> Result.map g` / `g (f (n-1))`（引数） | ❌ FS3569 |
| `f (n-1) >>= fun ... ->`（レシーバ = 引数）        | ❌ FS3569 |
| ラムダの本体が `if` で、枝に `f (n-1)`             | ❌ FS3569 |

「評価結果を**消費してから**次をする」再帰（`>>=` / `Result.map` / 戻り値をそのまま返す形）は
構造上どうしても末尾にならない。選択肢は 2 つ。

```fsharp
// ❌ FS3569（結果を消費してから整形する）
exec ctx child schema |> Result.map (fun op -> Ops.sortBy schema op keys)

// ✅ 属性を外して理由をコメントで残す
// [<TailCall>] を付けない理由: 子の Result を消費するため末尾にならず FS3569 になる。
// 深さは論理木の深さ（式は MaxExpressionDepth = 200）で頭打ちになる。
exec ctx child schema |> Result.map (fun op -> Ops.sortBy schema op keys)

// ✅ あるいは「積む側」と「回す側」に分けて本当に末尾再帰にする
let rec readAll n c acc =
    match readOne c with
    | Error e -> Error e
    | Ok(v, c1) -> readAll (n - 1UL) c1 (v :: acc)   // 末尾
```

**ローカルの `let rec` には `[<TailCall>]` を書けない**（FS0010 → `TreatWarningsAsErrors` でエラー）。
末尾化したい関数が別の関数の中にあるなら、その関数を**モジュールスコープへ引き上げ**、必要なものは引数で渡す
（`Expr.rewrite`（`Ast.fs`）は `rewriteLeaf` を受け取る形で、
内部の `rewriteLoop` が末尾再帰。`Binder.qualify` と `Ops` の集約読み替えが共用している）。

**要素数に比例する反復は必ず後者にする**（`match` に直せば末尾になる）。
`>>=` で受けると候補の数・WHEN 節の数だけスタックが積まれる。
該当は `IN` の候補リストと `CASE` の WHEN 節（`Eval.eval`）。
深い式の回帰テストは `AstTests` の `末尾再帰: AST の深い走査でスタックを食わない` と
`OpsTests` の `末尾再帰: 50000段の式を havingFilter してもスタックを食わない`。
カーソルからの数え上げ読み出しは `Reader.readItems`（Serialization.fs）に共通化し、
`Catalog` / `Memtable` が使う。

**Debug の `Tailcalls` は既定で false**。
`dotnet test` / `dotnet run` は Debug なので、CPS の相互末尾再帰が `tail.` にならず、
50000個の `IN` リストで `Stack overflow.`（exit 134）になる。
`Directory.Build.props` の `<Tailcalls>true</Tailcalls>` を消すと再発するので注意。
`MaxExpressionDepth` はこの前提のための保険（→ §11.48）。

属性の無い関数（ローカル `let rec` 含む）まで含めた**反復検証**は `dotnet run --project tools/TailAudit`
（既知分は `tools/TailAudit/TailAudit.baseline` と差分を取り、新規があれば exit 1。
追認は内容確認のうえ `-- --update`）。

### 8.17 モジュールスコープには DU を `let` 束縛で書けない（FS0010）

ヘルパー化の過程で「テーブルの定義か索引の定義か」を表したいとき、
`let private Kind = | A | B` と書くと FS0010 になる（`|` が binding に来た）。
DU は `type` 宣言（モジュールスコープのものは `private` 可）か、
**フラグと名前だけ返す**（`(bool * string) option`）のどちらか。

- ✅ 1種類の値だけ返すなら `bool` フラグで足りる（`Catalog.keyKindOf` はこれ）。
- ❌ 相互再帰でない `let` を `and` で並べると FS0576（`keepExtreme` を `and` にしてしまった）。
- ❌ ローカル `let tryPrefix prefix = ...` の中で `Encoding.UTF8.GetBytes prefix` を
  引数注釈なしのままだと FS0041（オーバーロード解決が決められない）。`let p: byte array = ...` と注釈する。

---

## 9. テスト・ビルドの罠

### 9.1 `[<Fact>]` は関数でない let 束縛に付くと黙って無視される（FS0842）

```fsharp
// ❌ 実行されない（FS0842 が警告、しかしエラーに昇格しない）
let ``説明`` = assertEqual 1 1
// ✅ 関数形
let ``説明`` () = assertEqual 1 1
```

「全部緑なのに 0件しか走っていない」状態になる。
そこで、`[<Fact>]` の件数をリフレクションで数えるテストを置いた。

### 9.2 `withDir` はディレクトリを作らない

```fsharp
// ❌ ディレクトリが無いので失敗する
let withDir tag f = f ("/tmp/" + tag)
// ✅ 明示的に作る
let withDir tag f =
    let dir = Path.Combine(Path.GetTempPath(), sprintf "rykdb-%s-%d" tag DateTime.Now.Ticks)
    Directory.CreateDirectory dir |> ignore
    try f dir finally Directory.Delete(dir, true)
```

### 9.3 `Assert.Equal<int64>(3L, list.Length)` は通らない

`list.Length` は `int`。`Assert.Equal<int>(3, list.Length)` にする。

### 9.4 `dotnet build` / `dotnet test` を並行で走らせない

ビルドロック競合で謎の `Error` が出る。**逐次実行**すること。

並行にすると incremental state が壊れ、
`StorageEngine.fs(338): error FS0039 'isEmpty' is not defined`
のような**幻影エラー**が出る（ソース自体は正しい）。

### 9.5 テストが SIGABRT すると件数が不定になる

実行済み分だけ報告される。`--logger "console;verbosity=detailed"` で個別に切り分ける。

### 9.6 乱数生成のテストは seed 固定だと退行する

`let rnd () = Random(seed)` は全ケースが同じ値になり、バグを検出できないまま緑になる。

モジュールスコープに `Random` を 1つ置き、「テストデータ: 乱数生成器が実際に多様な値を返す」で明示的にアサートする。

---

## 10. デバッグ手法

### 10.1 `/tmp/probe.fsx` で DLL を直接叩く

```fsharp
#r "src/RykDb.Core/bin/Debug/net10.0/RykDb.Core.dll"   // リポジトリルートからの相対パス
open RykDb.Core

let ok r = match r with | Ok v -> v | Error e -> failwithf "%s" (DbError.toDisplayString e)

let dir = "/tmp/probe-db"
if System.IO.Directory.Exists dir then System.IO.Directory.Delete(dir, true)

let session = ok (Session.OpenWith dir)
ok (session.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, age INTEGER)")
// ... 1 演算子ずつ確認 ...
```

重要: **DLL をコピーしないこと**。
FSI は `/tmp/probe.dll` のようなパスをキャッシュする。
**呼び出し側が同じ DLL を参照しているか**確認すること。

### 10.2 データが本当にディスクに書かれたか確認

全 keyspace がルート直下の 1本の LSM に入る
（`t:people` の行は `[長さ]["t:people"][主キー]` という物理キーになる）。

```fsharp
System.IO.Directory.GetFiles(dir, "*", System.IO.SearchOption.AllDirectories)
|> Array.iter (fun f -> printfn "%s (%d B)" f (int (System.IO.FileInfo f).Length))
```

### 10.3 treap の検索が O(n) になっていないか

`Memtable.getVisible` を使うようにすると **43ms → 0.3ms** になった。
「ポイント検索が遅い」ときは、まず `rangeScan` を使っていないか確認する。

### 10.4 fantomas で整形したら必ずリビルド

```bash
dotnet fantomas tests/RykDb.Tests/
dotnet build -t:Rebuild   # FS0020（戻り値の無視）などは整形後に増える
```

---

## 11. 設計上の注意点

| #   | 注意点                                    | 詳細         |
| --- | ----------------------------------------- | ------------ |
| 1   | **`Op` の継続を飛ばさない**               | §2.2         |
| 2   | **`exec` には「入力スキーマ」を渡す**     | §3.1         |
| 3   | **再帰を `>>=` で組まない**               | §2.3 / §8.12 |
| 4   | **ポイント検索に `rangeScan` を使わない** | §4.1         |

### 11.1 `Array.take` に「全部」を表す巨大数を渡さない

FSharp.Core の `Array.take` は `Seq.take` 相当で、要素数が足りないと
`The input sequence has an insufficient number of elements.`
で落ちる（`Int32.MaxValue` を「全部」の意で渡さない）。

### 11.2 ファイル番号を巻き戻さない

`NextFileNumber` は MANIFEST に残らない（§4.4）。
復旧時にディスク上の最大番号より小さい値に戻すと、
**次の flush が既存の SSTable を上書きしてデータを失う**。
復旧時は必ず `maxFileNumberOnDisk + 1` で上へ進める。

### 11.3 削除は主キー + 全索引に墓碑を書く

論理 keyspace を分けている代償として、削除は主キー側に加えて**全索引にもtombstone を書き込む**。
索引側を忘れると、索引経由の検索で**削除済みの行が復活する**。

### 11.4 MANIFEST を先に、ファイル削除を後にする

compaction / version 適用では **MANIFEST への反映が先、入力ファイルの削除が後**。
逆順にすると「MANIFEST は新しいがファイルは古い」状態になり、
**データ損失**になる（§4.4 参照）。

### 11.5 SSTable の検証はレコードの構造比較を使ってはいけない

並びは「キー昇順・同一キーは SeqNo 降順」（`SstEntry.compare`）。
レコードの構造比較（F# のレコード等価）はこの順序と一致しないため、
検証に使うと正しく並んでいるファイルを「壊れている」と誤判定する。

### 11.6 順序保持符号化の実装ミス（キー順が壊れる）

- `int64` の順序写像に `~~~` は**使えない**。
  `-1` のビットパターンが全 1 なので反転すると最小になり `-2` より小さく並ぶ。
  正しくは `mapInt64 v = (uint64) v + 0x8000000000000000UL`。
- `DECIMAL` の raw ビット列（96bit + スケール）も順序保持にならない。
  内部表現は `(符号, 10進指数, 桁列 29桁右詰め)` で、**負数は指数も桁列も両方反転**する。
- `Double.NaN` は .NET では符号ビットが立っている。
  正の quiet NaN に正規化しないとキー順が壊れる（`SqlValue.normalizeFloat`）。

### 11.7 エスケープ付きフィールドの終端処理は「ピーク分」を戻す

`TEXT` / `BLOB` のペイロードは `0x00` を `0x00 0xFF` に escape し、フィールド終端に素の `0x00` を置く。
デコードは終端 `0x00` の**次を 1 バイトピーク**して、`0xFF` なら escape の続き、
それ以外なら「ここがフィールド終端で、ピークしたバイトは次フィールドの先頭」と判定する。
このときカーソルを**終端バイトを指す位置**（ピーク分を戻す）にしないと、
`readFirstValue` の終端検証が次フィールドのタグを読んで `終端が不正 (0xXX)` で失敗する。

- 症状: 複合キー（後続フィールドあり）の先頭が `TEXT` / `BLOB` のとき、先頭フィールドの読み取りが失敗する。
  単一フィールドのキーでは終端が末尾に来て `Cursor.atEnd` の分岐だけを通るため露見しない
  （src からの `readFirstValue` 呼び出しが無く、テストも単一フィールドのみだったため潜在化していた）。
- 回帰テスト: `SerializationTests` の `OrderPreserving: 複合キーの先頭フィールドを読める`

### 11.8 keyspace 横断 commit は 1回の WAL バッチにする

テーブルと索引は論理 keyspace が別だが、物理ツリーと WAL は DB 全体で 1本。
`RowStore.commitToStore`（内部の `writeBatchesWithSnapshot`）は
全 keyspace のエントリに接頭辞を付けて **1つの `Entry array`** にまとめ、
`StorageEngine.writeBatch` を 1回だけ呼ぶ。
WAL には `Begin / Batch / Commit` が 1回の `Wal.append` で書かれるため、
「テーブルだけ書けて索引が書けない」状態は発生しない。

- **WAL 追記の成功を commit point にする**。
  その後の memtable flush 失敗で `Error` を返してはいけない。
  呼び出し側が `TransactionManager` を進めず、同じ SeqNo を再利用してコミット済み WAL と食い違う。
- flush に失敗しても `WriteBatch` が返した memtable は保持し、成功を返す。
  次の commit / `Close` で flush を再試行する。
- 旧 `.commit/journal` の publish / resolve は削除した。
  再適用用の `filterApplied` も不要（WAL の Commit レコードが原子性を保証する）。
- 回帰テスト: `RowStore: 複数 keyspace の書き込みは 1 回の WAL バッチで回復する`。

### 11.9 `ActiveSnapshot = 0` は「全版を残す」

`Compaction.gc` は `ActiveSnapshot = 0` を「全版を残す」として扱う。
0 を「最新版だけ残す」と解釈すると replay 中や境界未設定時に履歴を失う。
replay 中や境界未設定時は 0 のままにし、通常の commit では
`TransactionManager.activeSnapshot`（active の最小 snapshot、無ければ `LastCommitSeqNo + 1`）を設定する。

### 11.10 DROP は種類ごとに安全な順序で行う

`DROP TABLE` / `DROP INDEX` はカタログから定義を外すだけでは不十分。
論理 keyspace の接頭辞は名前から決まる（`t:<name>` / `i:<name>`）ので、
残骸があるまま同名で再作成すると**旧データや旧索引エントリが見える**。
特に「索引を DROP → 行を DELETE → 同名 CREATE INDEX」の順では、
DELETE 時に索引 tombstone が書かれないため、backfill 後の索引検索で削除済みの行が復活する。

- `DROP TABLE` / `DROP INDEX` は、カタログ削除の前に後始末対象 keyspace を
  root の `.drop-cleanup` ジャーナルへ書く。
- その後でカタログ定義を外し、keyspace を 8192エントリずつ tombstone 消去する。
- 後始末が成功したらジャーナルを消す。
  途中で失敗・クラッシュしても、次回 `Session.OpenWith` がジャーナルを読み、
  カタログに存在しない keyspace だけを再消去する。
- カタログに定義が残っている場合（カタログ削除前にクラッシュ）は、
  DROP 未完了としてジャーナルを捨て、データは消さない。
- 残骸は同名の `CREATE TABLE` / `CREATE INDEX` も掃除する。

### 11.11 compaction の出力分割は「件数」ではなく「バイト」、かつキーの切れ目で切る

`SstEntry.splitInto (MaxInputFiles * 4)` のように **ファイル数の上限をエントリ数として使う**と、
値の大きさに関係なく 128エントリで分割され、大きい値では 1ファイルが肥大化する。
出力は `CompactionOptions.OutputFileTargetBytes`（既定 4MiB）と `SstEntry.approximateBytes` の
概算バイトで分ける（厳密なファイルサイズではない）。

- **1キー分の版が複数ファイルに跨らない**ように、キーグループの切れ目で切り替える（`Compaction.pump`）。
  1キー分が目標を超える場合は、そのキーだけで 1ファイルにする（空のファイルは作らない）。
- 入力は `SstScan`（部分読みリーダー）+ `SstMerge` で 1件ずつ読み、
  出力中の 1ファイル分だけをメモリに持つ。入力全体は実体化しない。
- 固定長の型の定数は `ValueCodec.fixedEncodedBytes` に集約し、
  memtable の `Entry.approxBytes` と共有する
  （TEXT / BLOB だけは、memtable が安価な UTF-16 長、SSTable が UTF-8 実バイト数と方針が違う）。
- 途中で読み取りエラーになると、書きかけの出力ファイルが残ることがある。
  MANIFEST には載らないので、次回オープン時に孤児として `.orphans/` へ退避される（§11.13）。
- 回帰テスト: `Compaction: 出力はエントリ数ではなく OutputFileTargetBytes ごとに分ける` /
  `Compaction: 同じキーの版は出力ファイルを跨がない` /
  `Compaction: 複数ブロックの入力をストリーミングで読み書きできる` /
  `Compaction: 多数の入力ファイルをストリーミングでマージする`。

### 11.12 `LastCommitted` は commit のたびに active snapshot 以前を捨てる

`LastCommitted` は書き込みキーの衝突検出用だが、捨てないと**プロセス寿命の間ずっと全キーを保持**する。
`TransactionManager.commit` は自分の transaction を `Active` から外した後で `activeSnapshot` を取り直し、
`SeqNo < その値` の版を `Map.filter` で捨てる。

- 生きている transaction の snapshot 以降の版だけが衝突判定に使われる
  （衝突条件は「commit の SeqNo > txn の snapshot」なので、それより古い版は不要）。
- active が無ければ境界は `LastCommitSeqNo + 1`。次の commit で直前の commit の版まで掃除される。
- 掃除を「commit した版そのものを除く」にすると、まだ古い snapshot の transaction が
  同じキーを書いたときに衝突を見逃すので、必ず `activeSnapshot` を基準にする。

### 11.13 MANIFEST が壊れていたら「空の Version」で開かずエラーにする

`Manifest.openOrCreate` 内の `decodeLines` は**途中**のレコード破損を `CorruptionError` にし、
**末尾の書きかけ**だけを読み飛ばす。
`CURRENT` の指す manifest が無い場合や、`CURRENT` が無い/空なのに `*.sst` が残っている場合もエラーにする。

- 空の Version を返すと `StorageEngine.openDatabase` が全 SSTable を孤児として扱い、**データが見えなくなる**。
- 孤児は削除せず `<root>/.orphans/` へ退避する（`quarantineOrphans`）。
  manifest を読み落としただけのケースでも実体は復元できる。
- 末尾の書きかけを許容するのは、`logAndApply` の `fs.Write` + `Flush(true)` が途中で切れた場合のため。
- 各レコードは `[checksum:u32LE][VersionEdit 本体]` のフレーム（FNV-1a）を base64 にした 1行。
  base64 としては正しいが中身が壊れた行もチェックサムで検出する。
- 回帰テスト: `Manifest: 途中の壊れたレコードは CorruptionError` /
  `Manifest: 途中のレコードのチェックサム不一致は CorruptionError` /
  `Manifest: CURRENT が指す MANIFEST が無ければ CorruptionError`。

### 11.14 古い `Database` ハンドルの `Close` は WAL を消さない（internal の話）

公開 API の `Session` は常に最新のハンドルを保持するため、この問題は起きない。
以下は internal の `Database.Execute` を直接使うテストの注意。

ハンドルは不変で、`Execute` は新しいハンドルを返す。
古いハンドルも `Close` を呼べるが、そのまま flush / WAL 削除をすると
**新しいハンドルの commit 済みデータを失う**。

`StorageEngine.close` は WAL の実サイズとハンドルの `Wal.Size` を比較し、
一致する（= 自分が最新の書き手）ときだけ flush と WAL 削除を行う。
一致しなければ何もしない（データは WAL に残っており、新しいハンドルが flush する）。

同じ理由で **古いハンドルの `Close()` は `ProcessLock` も解放しない**。
`Database` は `ProcessLockLease` の所有者トークンを持ち、
新しいハンドルが作られるたびに所有者が移る。
所有者（最新）の `Close()` だけが LSM を閉じて `.rykdb.format` を解放する。

- 回帰テスト:
  `Database: 古いハンドルを Close しても新しいハンドルの commit は失われない` /
  `Database: 古いハンドルの Close は生きている新しいハンドルのロックを解放しない`。

### 11.15 DDL は明示トランザクションに参加しない（ので拒否する）

カタログと SSTable（索引 backfill）の更新を同じ transaction に載せる設計になっていない。
黙って DDL を auto-commit すると:

- `BEGIN; CREATE TABLE ...; ROLLBACK` でテーブルが残る。
- `BEGIN; INSERT ...; CREATE INDEX ...; COMMIT` の backfill が未 commit 行を見ないため、
  索引検索が行を取りこぼす。

`Engine.runStatement` は `Active` な transaction があるとき DDL を `SyntaxError` にする。
DDL を transaction に載せる設計に変えるまでは、この制約を維持する。

### 11.16 `CREATE INDEX` は backfill → カタログの順で行う

逆順（カタログ → backfill）だと backfill の失敗で「索引はあるが中身が無い」状態が永続化され、
再オープン後も索引検索が行を取りこぼす。
`Ddl.createIndex` は既存の索引エントリをチャンク単位で tombstone 消去してから、
現在行を 8192行ずつ backfill し、成功後にカタログへ登録する。
途中で失敗しても索引はカタログに載っておらず、次回の `CREATE INDEX` が残骸を消してからやり直す。

### 11.16a DDL（CREATE INDEX など）は `RootGate` で排他にする

backfill の途中に別の書き込みが入ると、**その行が索引に載らない**（逆に、消えた行の
エントリが残る）。
DDL だけはプロセス内で排他にする必要がある。

- 文の実行と commit は `RootGate.withShared`、DDL は `RootGate.withExclusive`（§7.8）。
- DDL は排他を取ったあと `ActiveSnapshotRegistry` を見て、
  **同じ root で実行中の transaction があれば `ConflictError`** を返す。
  古いカタログで計算した索引エントリを後から commit されるのを防ぐため。
  **待たない**（commit されないまま残った transaction で永久に止まらないように）。
- 同じスレッドで「共有 → 排他」の昇格はできない。
  そのため `Database.Execute` は **DDL 文だけ共有ロックを取らない**（`Ddl` 側が排他を取る）。
- 結果: トランザクションを `Rollback` / `Commit` せずに捨てると、その登録が残り、
  **その root の DDL は `ConflictError` になる**。
  `Close()` / `Dispose()` で解除され、ハンドルへの参照が切れて GC されても外れる
  （`RegistrationLease`。§7.5）。GC の時期は不定なので、明示的に終わらせるのが基本。
- 回帰テスト: `Database: 実行中の transaction があると DDL は ConflictError` /
  `Database: commit せずに捨てた transaction も GC されれば登録が外れる` /
  `Database: 生きている transaction は GC されても登録が残る` /
  `Database: Dispose でも transaction の登録が外れる`。

### 11.16b 二重オープンは「ロック + 台帳」で防ぐ

`FileShare.None` で他プロセスは防げるが、**同じプロセスの二重オープンは別問題**。
`Session.OpenWith` のたびに別の `RowStore`（別 memtable / WAL ビュー）ができ、
一方の commit が他方から見えない（**古い行を黙って返す**）。
そこで `ProcessLock` は台帳（正規化 root → トークン → `FileStream`）を持ち、
**既にこのプロセスが開いている root の 2回目は `IoError`** にする。

- 失敗を返す方が、黙って古い行を返すより安全。
- **最新ハンドルの `Close()`** でロックを解放（二重 `Close` は無害）。
  古いハンドルの `Close()` は no-op（`ProcessLockLease` の所有者だけが解放する）。
- **クラッシュ再現テストは `RowStore.openStoreTakingOver` と internal の `Database.OpenWithStore` を使う**。
  ここだけは意図的に二重オープンし、WAL 再生を検証する。
- 開く途中で失敗したら（壊れた MANIFEST など）**必ず解放する**。
  押さえたままだと、同じプロセスからも再オープンできない。
- 注意: `FileShare` の強制力は OS / ファイルシステム依存（同一マシンのローカル FS 前提）。
- 回帰テスト:
  `Database: 別プロセスが開いている DB は開けない` /
  `Database: 同じ root を二度開くと同じプロセスのエラーになり、Close 後は開ける` /
  `Database: 明示 transaction があるハンドルは Close でロックを外す` /
  `Database: 古いハンドルの Close は生きている新しいハンドルのロックを解放しない`。

### 11.17 識別子の比較は `SqlName.equal` に一本化する

`TableSchema.indexOf` が完全一致で `Catalog.eqName` が `OrdinalIgnoreCase` だったため、
`CREATE INDEX ix ON c (V)` が成功しても索引キーに列が入らず、索引検索が **0 行** を返していた
（UNIQUE 索引では `row.[-1]` で未捕捉の `IndexOutOfRangeException`）。
`Types.fs` の `SqlName.equal` をカタログ・スキーマ・実行層で共用する。
`PRIMARY KEY (ID)` と列 `id` の対応も同じ規則で解決する。

### 11.18 `LIMIT` / `OFFSET` は 0 以上の定数式

非リテラルを `0` に丸めると `LIMIT 1+0` が **0 行** になる（かつてドキュメントは「制限なし」と書いていた）。
現在の `Optimizer.buildLogical` 内の `applyLimit` は定数式を `Eval.eval Schema.empty [||]` で評価し、
行ごとの列参照・負数・`Int32.MaxValue` 超をエラーにする。
`Limit` ノードの `limit = -1` が「LIMIT なし」、`0` は 0行。

### 11.19 走査の開封エラーを握り潰さない

以前は開けなかったファイルを空と見なしていたため、
壊れた SSTable があると `Ok` のまま行が欠けた（静かな誤答）。
現在の `StorageEngine.scanAt` は pull（`SstScan`）を返し、
候補ファイルをキー下限順に必要になった時点で `prepare` で開き、
開封・ブロック解析のエラーをその `next` が返す（§11.24）。
全件走査でも必要になったファイルだけを開く（最初に一括では開かない）。
`limit > 0` は limit 件で止まるため、到達しない後続ファイルの破損は検出しない。
全件を消費する SQL 演算子（集約・ソート・DISTINCT など）は、
途中で Error になれば最終結果を返さない。

### 11.20 壊れたデータは未捕捉例外にしない

ファイル内容から値を復元するコンストラクタは例外を送出し得る。境界で検証して `CorruptionError` にする:

- `Sstable.parseBlock`: `shared > prev.Length` を拒否。
- `Sstable.parseFooterBytes` / `validateFooter`: 未知バージョン、セクションの重なり・隙間・範囲外を拒否。
- `ValueCodec`: DECIMAL の flags/scale（scale ≤ 28・予約ビット 0）、DATE / TIMESTAMP の範囲。
- `VersionEdit`: レベルは `0..Levels.Max`。
- `BloomFilter.deserialize`: ビット数・ハッシュ数 0、8 の倍数でないビット数、過大な件数、余分な末尾バイトを拒否
  （`% 0` と過大確保の防止）。

### 11.21 `Result` を握り潰さない（DEFAULT / `Begin()` / schema）

- `DmlOps.insert`: 未指定列の DEFAULT の評価エラーを `NULL` にしない（値を指定した列の DEFAULT は評価しない）。
- `Database.Begin()` は `Result<Database, DbError>` を返す（カタログ再読み込みの失敗を隠さない）。
- `Engine.contextOf`: `Catalog.toSchema` の失敗を無視せず、文のエラーとして返す。
- `Ops.sortBy`: 非 NULL の代表値と比較できない値があれば、ソート前にエラーにする。

### 11.22 SSTable の partial reader キャッシュは「パス + サイズ + mtime」で無効化する

`SstPartialCache` はパース済み `SstPartial` を弱参照で保持する。
キーにサイズと mtime を含めないと、同じファイル番号のファイルを差し替えたときに古い内容を返す。

- テストで SSTable を書き換えたら `File.SetLastWriteTimeUtc` で mtime を確実に進める。
- キャッシュは弱参照なので、メモリが逼迫すれば GC が回収する（LRU ではない）。
- キーはファイル世代ごとに増えるため、`put` がキー数 512 ごとに死んだ弱参照（GC 済み）を辞書から外す。
  生きた参照は実行中の走査が使うので残す。
  回帰テストは `SstPartialCache: 掃除は死んだ弱参照だけを外し、生きた参照は残す`。
- 欠落ファイルはキー計算（`FileInfo.Length`）の時点で IoError になる。

### 11.23 DISTINCT / GROUP BY のキーは文字列化しない

- **`SELECT DISTINCT`**: `ValueCodec.writeValues` で行を扁平なバイト列にしてから
  `HashSet<byte array>` に入れ、重複を判定する（同じ値なら同じ列になる）。
  `SqlValue array` のまま抱えるとボックス付きオブジェクトが山盛りになるため。
  メモリは既定 256MiB のバジェットで抑え、超過は `CapacityError`（OOM でプロセスごと落とさない）。
- **`GROUP BY`**: `HashIdentity.Structural` で `SqlValue array` を構造比較する（NULL は NULL 同士で等しい）。
  行は抱えず、グループごとのアキュムレータだけを更新する。
- `COUNT(DISTINCT)` は `Set<SqlValue>`（集約の中で扱う値は少数なので構造比較のまま）。
- `SqlLiteral` による文字列化は表示専用で、行ごとのホットパスでは使わない。

### 11.24 limit 付き scan は後続の SSTable を開かない

`StorageEngine.scanAt` は `limit > 0` のとき、**必要になったファイル・ブロックだけ**を開き、
`limit` 件に達したら止まる。
そのため、limit で到達しない後続 SSTable / ブロックの破損・欠落はそのクエリでは検出されない
（全件走査でも必要になった時点で検出し、クエリは結果を返さない）。
これは「先頭 limit 件に寄与しないデータは読まない」という最適化の副作用。
`openDatabase` は `FileMetaData.MaxSeqNo` を使うため起動時にデータブロックを読まない。
したがって起動時にも検出されない。
全件を読むクエリ（`limit = 0`）で、そのファイルに到達した時点で検出する。

### 11.25 LIMIT pushdown は Filter / Sort / Aggregate を越えない

`Optimizer` は `Limit` の件数ヒント（`OFFSET + LIMIT`）を `Project` だけ通して `Scan` に降ろす。
`Filter` / `Sort` / `Aggregate` / `Distinct` / `Join` は行数を変える（または順序を作る）ため、
ヒントを落として全件走査に戻す。`WHERE` 付きの `LIMIT` は pushdown されない。

**例外は主キー順の `ORDER BY`**。単一テーブル・集約なし・`DISTINCT` なしで、
`ORDER BY` の先頭が主キー全列（定義順・昇順）なら `Sort` ノードを作らないので、
`ORDER BY 主キー LIMIT n` のヒントが走査まで降りる（`Planner.sortIsRedundant`）。
主キーの一部だけ・`DESC`・索引走査になる条件（`WHERE 索引列 = ...` など）・集約・`DISTINCT`・結合では
従来どおりソートする。
回帰テストは `Database: 主キー順の ORDER BY は Sort を作らず LIMIT がストレージへ降りる`
（2本目の SSTable を壊し、`ORDER BY id LIMIT 1` は成功・`ORDER BY id DESC LIMIT 1` は `CorruptionError`）。

### 11.26 部分読みはブロック / メタデータ / Footer のチェックサムで守る

v5 の SSTable は各ブロック末尾に 4バイトのチェックサムを持つ。
Footer は Meta / Bloom / Index の連続領域を守る `metadataChecksum` と、
自身（先頭 80バイト）を守る `footerChecksum` を持ち、`openPartial` でも検証する。
全体チェックサムは部分読みでは検証できないため持たない
（ファイル全体を読む経路も、各ブロックとメタデータの検証で破損を検出する）。
`SstPartial` は Footer / meta / Bloom / index だけを開き、
データブロックは必要になったとき `FileStream` で読む。
読んだブロックは `parseBlock` がチェックサムを検証する。

- 部分読みで読まなかったブロックの破損は、そのクエリでは検出されない。
- テストでファイルを書き換えたら mtime を進める（§11.22）。
- `SstPartial` の `rangeScanPartial` は最大 8ブロックを 1回の open で読み、解析は必要になったときだけ行う。
  `lookup` は 1ブロックだけ読む。

### 11.27 結合アルゴリズムの前提

- ハッシュ/インデックス結合は、`ON` の左×右の等値条件が**同じ DataType** の列同士のときだけ使う。
  型が違うと入れ子ループに落ちる（暗黙変換した等値は自動選択しない）。
- `NULL` キーは一致しない。
  ハッシュ表にも登録しない。
- インデックス入れ子ループは、右部分木が `Filter` のときは使わない
  （右への WHERE pushdown を落とさないため）。
  `LEFT JOIN` の右側述語は結合後なので `Scan` のまま使える。

### 11.28 `Op` は one-shot — `Op.next cur` の後の `cur` を再利用しない

`Op.ofSeq` は共有の enumerator を進めながら行を返す。
`Op.next cur` を呼んだ後、返り値の `nxt` ではなく `cur` をもう一度 `Op.next` すると、
**先頭行ではなく次の行**が返る。
transaction の overlay（store の行 + txn のスロット）で `cur` を再利用してしまい、
既存行がある状態の `BEGIN; INSERT/UPDATE; SELECT` が行の欠落・重複・古い値を返していた
（`Dml.overlayRows`）。

- 正しい形: `match Op.next cur with | Ok(Some(row, nxt)) -> ... merge nxt ...`
- 回帰テスト: `E2E: 既存行がある transaction 内の INSERT は SELECT に全行見える` /
  `...複数 UPDATE が正しい` / `...索引検索が書き込みを見る`。
- store が空の transaction だけを試すと、このバグは出ない。

### 11.29 HAVING の集約は HAVING からも集める

`allAggs` を SELECT / ORDER BY の集約だけから作ると、
`SELECT age ... GROUP BY age HAVING COUNT(*) > 1` の `COUNT(*)` が集約ノードで計算されず、
`substituteAggs` が `NULL` に置き換えるため条件が UNKNOWN になり、**静かに 0行**になる。

- `Planner.applyAggregate` は HAVING の式から `Ops.aggCalls` で集約を集め、
  `allAggs` と `hasAgg` に含める。
- 回帰テスト: `SqlReference: HAVING にしか現れない集約でも絞り込める`。

### 11.30 `BEGIN` 後のエラーでは transaction が開いたままになる

`Session.Execute` は 1文ずつ実行し、成功するたびに現在のハンドルを更新する。
`BEGIN` が成功した後に後続の文が失敗した場合、Session は transaction を開いたまま保持する
（それ以前に成功した文の状態も保持する）。
呼び出し側が `Rollback()` するか `Close()` するまで transaction は続く。
`Close()` は進行中 transaction を rollback してから閉じ、ActiveSnapshot 登録も解除する。

- 回帰テスト: `Session: 複数文の BEGIN 後エラーは transaction を開いたままにする` /
  `Session: Close は開いた transaction を rollback する`。

### 11.31 GROUP BY の式は木全体でスロットへ置き換える

`GROUP BY age + 1` の集約結果は「式の値」1列になるが、式の木は `age + 1` のまま残る。
葉の `t.age` だけをスロットへ置き換えると、
`SELECT age + 1` が集約後のスキーマに無い `t.age` を参照して `列 't.age' が存在しません` になる。

- `Ops.substituteAggs` は **非葉の式の表示文字列** でもスロットを探し、
  式全体をリテラルへ置き換える
  （`project` / `havingFilter` 共通）。
- `GROUP BY` が葉だけなら表示文字列を作らない。
  深い HAVING 式で `Expr.toDisplayString` をノードごとに呼ぶと O(深さ²) になる
  （`末尾再帰: 50000段の式を havingFilter してもスタックを食わない` がハングした）。
- `project` は GROUP BY があるとき、集約が 0個でも置換経路を通す。
- 回帰テスト: `SqlReference: GROUP BY の式を SELECT / HAVING でも使える`。

### 11.32 未知の `t.*` は 0列の成功にしない

`Schema.expandStar` は修飾子に一致する列が 0件でも `Some(true, [])` を返す。
そのままだと `SELECT nope.* FROM t` が「0列・1行」で成功し、列の無い結果を静かに返す。
`Planner.buildLogical` で `Schema.hasQualifier` を検査し、`NameError` にする。

- 回帰テスト: `SqlReference: FROM に無い t.* は NameError`。

### 11.33 DEFAULT も列型へ強制する

`DmlOps.insert` は指定された値を `SqlValue.coerce` していたが、
省略列の `DEFAULT` は生の評価結果をそのまま保存していた。
`INTEGER` 列に `DEFAULT 'oops'` を書くと TEXT のまま格納され、後続の比較が型エラーになる。

- `defaultValues` の評価後に `SqlValue.coerce col.DataType` を通す。
- 回帰テスト: `SqlReference: DEFAULT は列型へ暗黙変換される`。

### 11.34 format マーカーの検証は排他を取ってから行う

`RowStore.openStore` は `.rykdb.format` を `ProcessLock.acquire` で `FileShare.None` のまま開き、
そのストリームを検証 / 新規作成に使う（ロックファイルを別に作らない）。
ロック前に書くと、同じ root を開こうとした第2のプロセスが
使用中の DB のマーカーを変更してしまう。

- `Directory.CreateDirectory root` → `ProcessLock.acquire` → `ensureFormat` の順にする。
- `ensureFormat` は `ProcessLock.acquire` が返したストリームを使う（開き直すと自分のロックに弾かれる）。
- `ensureFormat` が失敗したら取得済みのロックを解放する。
- 回帰テスト:
  `RowStore.openStore: ロック失敗時は format マーカーを書き換えない` /
  `RowStore: format マーカーがプロセス間ロックを兼ねる`。

### 11.35 `ActiveSnapshotRegistry` の bucket 削除と register は競合する

`ConcurrentDictionary.GetOrAdd` で bucket を取ってから登録を追加する間、
別スレッドの `unregister` が「空になった」と判断して bucket を削除すると、
追加した登録が削除済みの bucket に入って消える。
registry の登録が消えると compaction の GC 境界が上がり、まだ必要な版を捨て得る。

- `register` / `unregister` / `oldestExcept` を 1本の `SemaphoreSlim`（Gate）で直列化する。
- 回帰テスト: `ActiveSnapshotRegistry: 並行 register/unregister で登録が消えない`
  （登録直後の `oldestExcept` が必ず `Some` になることを並列で確認）。

### 11.36 外部ソートの run は全部を同時に open しない

`ExtSort` の最終 k-way merge で全 run を `openRun` すると、
run の数だけ file descriptor を消費する（大入力で枯渇する）。
同時に開くのは **32本まで** にし、超える場合は 32本ずつ中間 run にマージしてから最終 merge する
（multi-pass merge）。

- 中間 run も同じ `[varint 長][ValueCodec]` 形式で、読み終えた入力 run は削除する。
- 回帰テスト: `ExtSort: 5000行を小さなバジェットで溢らしても整列できる`
  （数百 run を作り、一時ファイルが残らないことも確認）。

### 11.37 `StorageEngine` の書き込み Gate はディレクトリ単位にする

以前は `WriteCloseGate` というプロセス全体の 1本の `SemaphoreSlim` で
`writeBatch` / `close` を直列化していた。
このため、無関係な DB の書き込みまで互いに待たされていた。

- `StorageEngine` は `Dir`（正規化した DB ディレクトリ）ごとの Gate を持つ。
- `writeBatch` / `maybeFlush`（flush + compaction）/ `close` を同じ Gate で直列化する。
- 異なる DB は並行。Gate 表はプロセス内で共有し、キーは正規化したパス。
- Gate 表は開いた DB の数だけ残る（プロセス寿命。通常は小さい）。

### 11.38 未対応の SQL 語をテーブル別名として飲み込まない

`UNION` / `INTERSECT` / `EXCEPT` / `RIGHT` / `FULL` / `CROSS` / `NATURAL` / `OUTER` は
`Lexer.Keywords` に無いため識別子として読まれる。
以前は `parseOptAlias` がこれをテーブル別名として消費し、次のような静かな誤動作になっていた。

- `SELECT id FROM t UNION SELECT id FROM t` → `FROM t AS UNION` + 別文の SELECT に分解され、
  `Execute` が `RowsAffected` の合計だけを返す（エラーにならない）。
- `SELECT u.id FROM t RIGHT JOIN u ON u.id = 1` → `RIGHT` が `t` の別名になり、
  通常の INNER JOIN として実行される（本来の RIGHT JOIN と結果が違う）。

対策:

- `parseTableAlias` が未対応語を裸のテーブル別名として消費しない（`AS` を付ければ使える）。
- `parseJoins` が `USING` を明示エラーにする。
  FROM の直後に残った未対応語（`FROM t AS x RIGHT JOIN ...` など）も明示エラーにする。
- `parseStatements` は文の区切りに `;` を必須にする。
- `parseSelect` は句の後ろの `UNION` / `INTERSECT` / `EXCEPT` も明示エラーにする。

回帰テスト:

- `パーサ: 未対応の集合演算は明示エラー`
- `パーサ: 未対応の JOIN は明示エラー`
- `パーサ: 複数文は ; で区切る`
- `SqlReference: UNION はエラーになり 2 文として実行されない`
- `SqlReference: RIGHT JOIN はエラーになり INNER JOIN として実行されない`

### 11.39 CAST は暗黙変換 (`SqlValue.coerce`) と分ける

`coerce` は INSERT / UPDATE の暗黙変換で、TEXT → INTEGER を受け付けない
（`INSERT INTO t (int_col) VALUES ('42')` は `TypeError`）。
`CAST('42' AS INTEGER)` は明示変換なので、`SqlValue.cast` を別に用意し、
TEXT → 数値 / BOOLEAN を InvariantCulture で解析する。
数値 → TEXT などは `coerce` に委譲する。
`coerce` を広げると暗黙変換の仕様が変わってしまうので、両者を混ぜないこと。

回帰テスト: `Eval.eval: CAST は TEXT から数値・BOOLEAN へ変換できる`。

### 11.40 ORDER BY の序数は Planner で解決する

`ORDER BY 1` を式として評価すると「定数 1」のソートになり、静かに no-op になる。
`Planner.applySort` が 1 始まりの序数を `SELECT` 項目の式へ置き換える。
範囲外の序数は `SyntaxError`。
`SELECT *` の序数は `Schema.expandStar` で列へ展開する。

回帰テスト: `SqlReference: ORDER BY の式と序数`。

### 11.41 `LastCommitted` の掃除は「この commit を反映した後」の境界で行う

`TransactionManager.commit` の GC 境界を更新前の `LastCommitSeqNo` で計算すると、
`activeSnapshot` が「前回 commit の SeqNo + 1」を返すため、
**今 commit した全キーが次の commit まで `LastCommitted` に残る**
（100k 行の commit で約 10.7 MiB。次の commit で解放されていた）。

```fsharp
// ❌ 更新前の LastCommitSeqNo（= 前回の commit の SeqNo）が境界になる
let gcThreshold = activeSnapshot staged
// ✅ この commit の SeqNo を反映した状態で境界を求める
let gcThreshold = activeSnapshot { staged with LastCommitSeqNo = commitSeq }
```

active な transaction が無ければ境界は `commitSeq + 1` になり、commit 直後に空へ戻る。
生きている transaction がある間は「最古の active snapshot」が境界なので、必要な版は残る。

`LastCommitted` が空のときは `findConflicts` が即 `[]` を返す
（全書き込みキーの `Set<string>`（base64 文字列）を作らない）。
回帰テスト: `TransactionManager.commit: active が無ければ commit 直後に LastCommitted を捨てる` /
`TransactionManager.commit: 古い snapshot の transaction が居る間は掃除しない`。

### 11.42 メモリ上限は「符号化サイズ」ではなく「管理メモリ」で見積もる

`ValueCodec.approximateBytes` / `SstEntry.approximateBytes` は SSTable の**ファイルサイズ**見積りで、
型タグとペイロードしか数えない
（参照型の `SqlValue` オブジェクト・string の UTF-16・配列ヘッダ・Treap / Map ノードを含まない）。
これをメモリ上限の判定に使うと、`MemLimitBytes = 64MiB` でも memtable が実測 450MiB 程度まで膨らんだ
（20万行・2列で符号化 13.4MiB に対して実測 96.8MiB、約 7倍）。

- メモリ判定は `ValueCodec.managedBytes`（値 1個）・`managedRowBytes`（行配列）・
  `Entry.managedBytes`（Treap ノード + Entry + option + キー配列 + 行）・
  `Transaction.slotBytes`（Map スロット）を使う。
- ファイルサイズの見積り（compaction の出力分割）は `ValueCodec.approximateBytes` /
  `SstEntry.approximateBytes` のまま変えない。
- 実測（2列・TEXT 48文字）: 20,000行で実測 7.8MiB / 見積り 6.7MiB（約 1.2倍）。
  200,000行では 64MiB を超えて commit 中に flush され、memtable の保持はほぼ 0 になった。
- 回帰テスト: `Memtable: Count と ApproxBytes は挿入したエントリの合計になる` /
  `ValueCodec.managedBytes: 参照型と UTF-16 を含む管理メモリの概算` /
  `Entry.managedBytes: memtable の保持量は符号化サイズより大きい`。

### 11.43 `Wal.append` はフレーム単位で書き出す（全フレームを連結しない）

Begin / Batch / Commit の全フレームを `Array.concat` して 1回の `Write` にすると、
大きな transaction で「バッチ本体 + 連結コピー + 中間配列」が同時に生じ、ピークが約 2倍になる。
`Wal.append` は **1フレームずつ `Write` し、最後に `Flush(true)`** する。

- 原子性は「フレームごとの CRC」+「Commit に到達した Batch だけを再生する `foldCommitted`」が守る。
  途中で落ちても未 commit の Batch は捨てられるので、コミット済みトランザクションが中途半端にならない。
- 空のレコード列ではファイルを作らない（従来どおり）。
- 回帰テスト: `WalTests`（フレームの往復）と `DatabaseTests` の WAL 再生・クラッシュ耐性。

### 11.44 JOIN が保持する行は上限付きにする

入れ子ループ / ハッシュ結合は右側の全行を保持する（設計どおり）。
無制限だと巨大な右テーブルで OOM し、プロセスごと落ちる。
`Ops.toArrayWithBudget` が **管理メモリ換算 256MiB** で打ち切り、
超過は `CapacityError`（`Ops.distinct` / transaction と同じ考え方）。

- インデックス入れ子ループの検索結果（非一意索引の 1キーに大量の行）も同じ上限。
- マージ結合は右側を丸ごと持たないが、**同じキーの行のグループ**（左右それぞれ）を保持する。
  1キーに大量の行が付く場合は `takeMergeGroup` が 1グループ 256MiB で打ち切る。
- ハッシュ結合の表（バケット）は行配列の後に作られ、行配列は表作りの後に捨てられる
  （ピークは概ね上限の 2倍まで）。
- `lookupHash` はバケットをそのまま返す（probe ごとの `ToArray` を廃止）。
- 回帰テスト: `Ops.toArrayWithBudget: 上限を超えると CapacityError`。

### 11.45 `Sstable.writeTo` は入力を 1回だけ列挙する

`writeTo` は `seq` を受け、**1回だけ列挙**してブロック単位で書き出す
（memtable の flush が全件を `SstEntry` 配列へ実体化しない）。
キー順の検証・先頭/末尾キー・`maxSeqNo`・Bloom 用のキーは書き出しと同時に集める。

- `FileMetaData.writeSstable` は失敗時（キー順違反・書き込み失敗・空の入力）に
  **部分ファイルを消してから**エラーを返す（孤児 `.sst` を残さない）。
- 検証は `writeTo` 内に統合したため、
  `Sstable.validate` / `SstEntry.splitInto` / `SstEntry.maxSeqNo` は削除
  （それぞれ `loop` の逐次チェック・1ブロック分のため・書き出し中の集計に置き換え）。
- 末尾再帰の `loop` は `use`（列挙子の破棄）の後ろにあるが、
  F# は `use` / `try/finally` の中でも自己再帰を末尾呼び出しにする
  （300万回の反復で実測済み）。
- 回帰テスト: `Sstable.writeTo: 入力の seq は 1 回だけ列挙する` /
  `FileMetaData.writeSstable: キー順でない入力はエラー`
  （部分ファイルが残らないことを含む）。

### 11.46 マージ結合は「走査が結合キー順」のときだけ選ぶ

`Analyzer.chooseJoinAlgorithm` は `Logical` の走査から昇順の列を求める（`scanOrder`）。
`SeqScan` / `RangeScan` は主キー列、`IndexLookup` / `IndexRangeScan` は索引列、
`Filter` / `Limit` は透過、`PkLookup` / `Values` は単一行（任意の順序）。

- 結合キーが走査順の**先頭 |keys| 列**と一致するときだけマージ結合にする。
  キーでない列がキーの間に挟まる走査（PK `(a,b)` に対するキー `{a,c}` など）は
  全体としてキー順にならないので使わない。
- 左右のキー順が違う場合（左 PK `(a,b)` / 右 PK `(b,a)` で `{a,b}` を結合）も使わない。
- `ON` の記述順は自由。等値は対称なので、走査順にキーを並べ替えてよい。
- 回帰テスト: `Analyzer.chooseJoinAlgorithm: 結合キーが走査順の先頭と一致するときだけマージ結合` /
  `SqlReference: マージ結合（主キー同士・LEFT・複合キー）` /
  `OpsTests` のマージ結合 5 本（ハッシュ結合との差分比較を含む）。

### 11.47 サブクエリは計画時に定数へ畳み込む（相関は実行時に外側行ごと）

`Plan.planSelectCWith` は `SELECT` の全式（項目 / `ON` / `WHERE` / `GROUP BY` / `HAVING` /
`ORDER BY` / `LIMIT` / `OFFSET`）からサブクエリを再帰的に探し、
**非相関**なら論理代数を組む前に実行して `Expr.Value` / `Expr.InList` / 真偽値へ置き換える。

- **相関サブクエリは実行時評価**。
  `resolveSelectC` は外側クエリの入力スキーマをスコープ列として受け取り、
  内側で解決できない列参照が外側スコープに一致すれば相関と判定して
  `Expr.CorrelatedSubquery`（`Run` と `Depths`）へ置き換える。
  実行時は外側行の値で参照を置換し、`runSubquery` が内側を再計画・実行する。
  外側と同じ実テーブル名が内側の `FROM` にある場合は標準どおり内側が優先される。
- `Depths` は外側スコープの深さ（1 = 直近）。
  入れ子の相関は深さ 2以上だけを外側クエリの相関として引き継ぐ。
- **相関参照を含む述語は押し下げない**。
  子スコープ（1テーブルの走査）の行で評価すると外側列の位置が変わる。
  `Expr.hasCorrelated` で `buildScan` / `rightPreds` / `localToAny` から除外する。
- 集約後の行を読む位置（集約クエリの `SELECT` / `HAVING` / `ORDER BY`）と
  `LIMIT` / `OFFSET` は外側行を持たないので `SyntaxError`。
- 非相関サブクエリは**クエリごとに 1回**実行される。
  `IN (SELECT ...)` は結果をすべて保持する
  （スカラーは最大 2行、`EXISTS` は 1行だけ読む）。
  相関サブクエリは外側行ごとに実行される（外側行数 × 内側走査）。
- ヘッダは解決前の式から作る。
  解決後から作ると `SELECT (SELECT MAX(v) FROM t) FROM t` の列名が `30` のようになる。
- 集約後の行（`Ops.substituteAggs`）は `Expr.Value` で値を埋め込む。
  `Literal.ofValue` は DATE / TIMESTAMP / UUID を TEXT にするため、
  集約結果（`MAX(dt)` など）と DATE 列の比較が `TypeError` になる。
- DML は `Engine.runInsert` / `runUpdate` / `runDelete` が `Plan.resolveDmlExprWith` で解決する
  （UPDATE / DELETE は対象行のスキーマを渡すので外側行を参照できる）。
  `CREATE TABLE ... DEFAULT (SELECT ...)` は解決しない（実行時に `SyntaxError`）。
- Parser は式と SELECT のパーサを同じ `let rec` グループにするため、句の側も CPS にする
  （`parseSelectStatementC` / `parseSelectItemsC` / `parseJoinsC` / `parseWhereC` / …）。
  継続をラムダで渡すので FS3569 は通るが、**群の全メンバーに `: Result<'r, DbError>` の戻り型注釈を付ける**
  （推論に任せると FS0043「'r と 'r0 が一致しない」で群全体が落ち、
  後続が FS0039「parseExprC は定義されていません」という無関係なエラーになる）。
  深さは `MaxExpressionDepth` で頭打ち。
- **Planner のサブクエリ解決（`Plan.planSelectCWith` / `resolveSelectC` / `resolveExprC` / `resolveLeafC`）も CPS**。
  式の走査は `Expr.rewriteWithC`（葉の書き換えが継続を取る版）、
  リスト（項目 / JOIN / GROUP BY / ORDER BY）は `traverseC` が 1件ずつ継続で回す。
  葉からの `planSelectC` 呼び出しを `>>=` のレシーバに置くとそこでフレームが残るので、
  `match` / 継続の末尾呼び出しで書く。
- **相関の再実行は計画関数の型を具体化しない**。
  `planCoreCWith` / `planSelectCWith` / `resolveSelectC` / `resolveExprC` / `resolveLeafC` は相互再帰グループなので、
  `resolveLeafC` の中で `planSelectCWith ... ret` のように具体型の継続を渡すと、
  グループの `'r` が `Schema * Op` に固定されて他の呼び出し（DML の式解決など）が型エラーになる。
  外側行ごとの実行は `runSubquery`（グループの外の `let rec`）に切り出し、
  グループ内へは関数引数 `runner` として渡す。
- 回帰テスト: `SqlReference: サブクエリ（スカラー・IN・EXISTS）` /
  `SqlReference: 相関サブクエリ` / `SqlReference: 相関サブクエリの未対応形` /
  `SqlReference: サブクエリの未対応形とエラー` / `パーサ: サブクエリ（スカラー / IN / EXISTS）` /
  `末尾再帰: 大量の SELECT 項目・GROUP BY・ORDER BY がパースできる`。

### 11.48 `MaxExpressionDepth` は `tail.` 前提の保険（外すとプロセスごと落ちる）

`Parser.MaxExpressionDepth = 200` は **SQL の意味上の制限ではない**。
CPS の相互再帰が末尾呼び出し（`tail.`）にならない環境でも StackOverflow を起こさないための保険。
`enterExpr` は式の入口・単項演算子・二項演算子の右辺ごとに深さを 1 消費するので、
演算子の連鎖・`NOT` の連鎖・括弧・サブクエリの入れ子がすべてここで頭打ちになる。

実測（2026-11-04、`SELECT * FROM t WHERE a = (v + 1 + …)`）:

| ビルド                            | 10,000 連鎖                                        | 備考                 |
| --------------------------------- | -------------------------------------------------- | -------------------- |
| `Tailcalls=true`（既定）          | 上限を 2,000,000 にすると**成功**（0.64s / 52MiB） | スタックは積まれない |
| `dotnet build -p:Tailcalls=false` | **StackOverflow（exit 134）**                      | プロセス即死         |
| `Tailcalls=false` + 上限 200      | `式の入れ子が深すぎます（上限 200）`（exit 1）     | 安全に失敗する       |

- 上限を 2,000,000 にして実測すると、1,000,000 演算子でも成功するが **11.7s / 635MiB** かかる。
  深さに比例して AST と継続クロージャをヒープに持つため、**入力長 O(n) のメモリに上限がなくなる**
  （1,000 段の入れ子サブクエリも 0.83s / 50MiB で通る）。
- .NET の `StackOverflowException` は**捕捉できない**。
  RykDb はホストプロセス内で動く組込み DB なので、ここが破れると**クエリ 1 本でアプリが落ちる**。
- したがって**削除しない**。緩めるなら 10,000 程度
  （実測 100k で 112MiB なので 10k は数十 MiB 級）。
  削除するなら `Directory.Build.props` の `<Tailcalls>true</Tailcalls>` を**テストで固定**し、
  `tail.` が消えたら CI が落ちるようにすること。
- `Planner` のサブクエリ解決は CPS なのでこの上限に依存しない
  （残る依存は Parser の再帰だけ）。
- 回帰テスト: `ParserTests` の `末尾再帰: CPS にした式構文規則は深さ境界で正確に動く` /
  `末尾再帰: 深い入れ子はエラーになる（StackOverflow ではない）` /
  `末尾再帰: 深いサブクエリの入れ子もエラーになる（StackOverflow ではない）`。

### 11.49 FROM を省略した SELECT は「1 行 0 列」を入力にする

`SELECT 1` / `SELECT 1 + 2 AS x` のような FROM なしの SELECT は、
Parser が `From = None` にし、Planner が `Logical.Values [||]`（1行 0列）を入力にする。
`WHERE` / 集約 / `ORDER BY` / 射影 / `DISTINCT` / `LIMIT` はその 1行にそのまま被せる。

- `SELECT COUNT(*)` は **1**（`WHERE` で 0行にすれば 0）、
  `SELECT 1 WHERE 1 = 2` は **0行**。
- `FROM` の無い `JOIN` は Parser が `JOIN には FROM が必要です` にする。
- **`*` / `t.*` はエラーにする**（`FROM のない SELECT では * を使えません` / `NameError`）。
  許すと 0列 1行の「成功」になり、ヘッダと行の列数がずれる
  （§11.32 の `SELECT nope.*` と同じ型の静かな誤答）。
- ⚠️ 句を読む処理をローカル関数 `let parseAfterFrom …` に切り出すと **FS3569** で落ちる。
  CPS 群のメンバーへの呼び出しがローカル関数の中では末尾と見なされないため。
  `let rec … and` 群に `parseSelectTailC` として足すこと（ラムダ内から呼ぶのは従来どおり可）。
- 回帰テスト: `SqlReference: FROM を省略した SELECT は定数 1 行を返す` /
  `パーサ: FROM を省略した SELECT`。

### 11.50 派生テーブルは「計画」と「実行」を分ける（`planCoreCWith` / `Logical.Derived`）

`FROM (SELECT ...) AS x` は **計画時に実行しない**。
`Plan.planSelectCWith` を `planCoreCWith`（論理木まで）と実行に分け、
派生は `PlannedQuery { Output; Header; Input; Context; Tree }` として持ち回る。

- `Plan.resolveSourcesC` が AST の `TableSource.Derived` を `TableSource.Named`（別名）へ書き換え、
  別名の小文字をキーにした Map を `Optimizer.buildLogicalWith` に渡す。
  書き換え後に `From` / `Joins[].Table` に `Derived` が残っていたら解決漏れ。
- `Logical.Derived` の実行は `Optimizer.run` の `DerivedFrame`。
  内側の木は **内側の `ExecContext`**（内側のテーブル別名を登録したもの）で走らせる。
  外側の ctx で走らせると `FROM people p` の `p` を引けない。
- 派生の列名は参照名（`SELECT t.id` → `id`）で持つ（`Schema.referenceColumns`）。
  表示名のままだと入れ子の派生で `y.v` ではなく `y."x.v"` になる。
- 述語は内側へ押し下げない（集約 / DISTINCT / LIMIT を跨ぐと意味が変わる）。
  派生ノードの上に `Filter` を付ける。
- `Logical.Derived` は `scanOrder` で `None` に落ちるのでマージ結合にはならない。
  型が `None` の派生列は等値キーにもならない（入れ子ループになるが結果は正しい）。
- 再帰：`planCoreCWith` / `resolveSourcesC` は CPS だが、
  `planCoreCWith` を呼ぶ継続はラムダ内なので TailAudit のベースラインに載る
  （深さは Parser の `MaxExpressionDepth` で頭打ち）。
- 回帰テスト: `SqlReference: 派生テーブル（FROM / JOIN）` /
  `パーサ: サブクエリ（スカラー / IN / EXISTS）` /
  `Optimizer.buildLogical: FROM のサブクエリは Logical.Derived になる` /
  `Analyzer.chooseJoinAlgorithm: テーブルスキーマが無い右は IndexNestedLoop にしない`。

### 11.51 `maybeFlush` は WAL commit 後に Error を返さない（compaction 失敗で flush を巻き戻さない）

`maybeFlush` を `flush engine >>= compactOnce` にすると、
flush 成功・compaction 失敗のときに `Error` が返り、
`RowStore` は **pre-flush の engine** を保持してしまう。
一方、ディスクでは flush の SSTable が MANIFEST に載り、WAL は rotate 済み。
この状態で次の write は古い WAL 番号へ追記し、再起動時の復旧は最大番号の WAL しか読まないため、
**commit 済みの行が失われ得る**。

- ❌ `match flush engine with Error e -> e | Ok flushed -> compactOnce flushed`
  （compaction のエラーが flush の結果を巻き戻す）
- ✅ flush 失敗: pre-flush の engine を `Ok` で返す（memtable を残して次回再試行）。
  flush 成功・compaction 失敗: **flush 後の engine** を `Ok` で返し、
  `LastCompactionError = Some e` を記録する。
- WAL への書き込みが成功した時点で commit は成立している。
  ここで `Error` を返すと、呼び出し側が SeqNo を再利用し得る（§11.8）。
  `RowStore.writeBatchesWithSnapshot` の `MaybeFlush` エラー握り潰しはこのための保険として残す。
- 回帰テスト: `StorageEngine.maybeFlush: compaction が失敗しても flush 後の engine を返す` /
  `RowStore: compaction 失敗後も flush 結果を保持し、再起動で commit を失わない`。

### 11.52 background compaction は install 時に Version / GC 境界 / 採番を再検証する

background compaction の worker は、開始時の `Version` と `ActiveSnapshot` をそのまま
MANIFEST に書いてはいけない。
merge 中に foreground の flush や新しい transaction が入り得る。

- ❌ 開始時の Version に `AddedFiles` / `DeletedFiles` を適用して MANIFEST に書く。
  flush で追加されたファイルを巻き戻したり、
  開始後に登録された古い snapshot が見る版を削除したりする。
- ✅ install は **installGate** の下で現在の `HostState` を再読する。
  1. `Removed` の全ファイルが現在の Version に存在するか。
  2. 現在の最古 snapshot（`ActiveSnapshotRegistry.oldestExcept` と
     `HostState.ActiveSnapshot` の小さい方）が開始時より古くなっていないか。
     どちらかが不成立なら **出力を削除して再試行**する。
- ファイル番号は `EngineHost` の CAS 採番だけを使う。
  flush も worker も `Manifest.logAndApply` を同じ installGate の内側で呼ぶ。
- 読み取りは Session 呼び出し単位で Version 全ファイルを pin する。
  worker は入力の実ファイルを即削除せず、retired にして refcount が 0 になってから消す。
  `SstScan` 自体には pin を配線していないため、早期終了で FileStream が残る既存問題は別。
- close は cancel → **無条件の signal** → worker join → flush → WAL 削除 → ProcessLock 解放。
  worker が install 中でも join が完了してから WAL を消す。
  Pending ビットが立っていても、worker が既に signal を消費して Pending を消す前かもしれない。
  この状態で release を省略すると cancel 後の `signal.Wait()` から永久に戻らず、close がハングする
  （`SessionTests` の短命な open/close で実際に発生した）。
- **破棄したあとは GC 境界を下げて publish する**。
  境界を戻さないと、次のサイクルも同じ判定で破棄し続けて前進しない
  （古い transaction が開いている間、compaction が終わらない）。
- **pin は「実際に読むハンドルの Version」に対して取る**。
  書き込みを含む `Session.Execute` バッチで複数文を 1回の lease で囲むと、
  途中の文の flush で Version が進んだあとの文が pin 外のファイルを読む
  （書き込みバッチは文ごとに pin する）。
  読み取りバッチは SELECT が state を変えないのでハンドルが変わらない。
  入口で 1回 pin してバッチ全体をそのハンドルで実行してよい（§11.53 と同じ条件）。
- **Idle 判定は単一の状態で行う**。
  busy と pending を別々に持つと、 worker が pending を消してから busy を立てる間や、
  2つを別々に読む間に `IsIdle` が true を返し、
  `waitIdle` が compaction 完了前に進んでテストが flaky になる。
  `EngineHost` は `Idle` / `Busy` / `Pending` を 1つの CAS セルで持つ。
  `Signal` の wake 要否は pending を立てる**前**の値で判断する
  （更新後の値を見ると release が漏れて worker が起動せず、テストがハングする）。
  **Shutdown だけは例外**で、Pending の有無に関係なく `signal.Release()` する。
  Pending は「signal 未消費」を意味しない（worker が permit を消費した直後、
  Pending を消す前に close が割り込むと、Pending あり・permit なしになる）ため。
- **worker の開始はリカバリ後**。
  `Database.OpenWithStore` のカタログ読込と DROP 後始末が終わってから `RowStore.startBackground` を呼ぶ。
  先に動かすと、リカバリの走査中に入力ファイルが retired / 削除され得る。
- 回帰テスト: `background compaction: flush 後に L0 がbackgroundで compaction される` /
  `background compaction: compaction 中でも次の write が返る` /
  `background compaction: 読み取り中のファイルは削除されず、lease 解放後に削除される` /
  `background compaction: close は進行中の compaction を停止して再オープンできる` /
  `background compaction: install 後に再オープンしても行が残る` /
  `background compaction: 開始後に古い snapshot が現れたら境界を下げてやり直す` /
  `background compaction: Close しなくても install 済みの行は再オープンで残る` /
  `background compaction: flush と install が競合してもファイル番号が重複しない` /
  `background compaction: DB を開いた時点で未 compaction の L0 を解消する`。

### 11.53 Session の並行読み取りは「捕捉と pin を同じ短いロックで」行う

並行読み取りを入れるときの落とし穴。

❌ `let db = current` をロックで取ってから、ロックを外して `db.AcquireSnapshot()`。
捕捉したハンドルの snapshot seq は古いのに、pin する Version は新しい。
間に commit + flush + compaction が入ると、古い snapshot に必要な版が GC され、
「その時点では見えていた行」が読めなくなる
（`RowStore.acquireSnapshot` は host の最新 Version でハンドルを作り直すため）。

✅ `stateGate` の下で `current.AcquireSnapshot()` まで行い、`(db, lease)` を取ってからロックを外す。
pin した Version とハンドルの snapshot seq が一致する。

❌ 読み取りで `current <- next` を書き戻す。
読み取りは state を変えないので不要なうえ、実行中の書き込みのハンドルを古いもので上書きする競合になる。

✅ 読み取りはハンドルをローカルにスレッドして終わり。
`current` には触らない。

❌ `Close` が `readers` を待たずに store を閉じる。
実行中の読み取りが close 後の engine / WAL を触る。

✅ `closed <- true` の後に `readers = 0` まで `Monitor.Wait` してから rollback + close。

回帰テスト: `Session: 読み取りは実行中の書き込みを待たない` /
`Session: 書き込みは実行中の読み取りを待たない` /
`Session: Close は実行中の読み取りの完了を待つ` /
`Session: 並行読み取りは commit 済みの一貫した snapshot を見る`。

### 11.54 TCP サーバの切断 rollback は「transaction を開始した接続」だけ

❌ 切断時に `if session.InTransaction then Rollback()`。
複数接続が Session を共有すると、関係ない接続の切断で他の transaction が消える。

✅ `Session.ExecuteTracked` の `SessionTransactionChange`（`Started` / `Ended`）で所有権を追い、
所有接続の切断時だけ rollback する。

回帰テスト: `TCP: 他接続の切断は transaction を rollback しない` /
`TCP: 接続中の接続があっても新しい接続を受け付ける`。

### 11.55 background compaction の worker は専用スレッドで起動する

❌ `EngineHost.Start()` を `Task.Factory.StartNew` の既定スケジューラ（=`Task.Run`）で起動する。
worker は `signal.Wait()` でブロックし続ける長寿命タスクなので、
スレッドプールが混雑していると `WaitingToRun` のままキューに積まり、
書き込みは L0 に揃っても compaction がいつまでも始まらない。

✅ `TaskCreationOptions.LongRunning` で専用スレッドを起こす。
スレッドプールの可用性に依存しない。

- 起動が遅れているときの診断は `EngineHost.Activity` と `EngineHost.WorkerStatus`。
  `WorkerStatus = WaitingToRun` かつ `Activity = Pending` なら未スケジュールが原因で、
  `ThreadPool.PendingWorkItemCount` を見るとプールの枯渇が分かる。
  `L0=4`（トリガ条件は満たされている）なのに compaction が動かない、という切り分けになる。
- `LongRunning` のスレッドは background thread なので、
  store を閉じ忘れた worker が残っていてもプロセス終了は妨げない。

回帰テスト: `background compaction: 開始後に古い snapshot が現れたら境界を下げてやり直す`
（`started.Wait 5000` が false になると「compaction が開始されません」で落ちる）。

### 11.56 TCP の切断 rollback 完了を待たずに次の接続から照会すると flaky

❌ クライアントを切った直後に別の接続から `SELECT` する。
切断（EOF）の検知と rollback はサーバの接続タスクで非同期に進むため、
先に届いた `SELECT` が rollback より先に `writeGate` を取ると、未 commit の行が見える
（テストでは期待 `0` に対して `1` が返り、`Collections differ` になる）。

✅ テストでは `SpinWait.SpinUntil((fun () -> not session.InTransaction), 10000)` で rollback 完了を待つ。

回帰テスト: `TCP: 切断時に open transaction は rollback される`。
