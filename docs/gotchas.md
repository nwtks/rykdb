# 開発で踏んだ落とし穴

このプロジェクトで**実際に踏んで直した**バグと、その原因・対策をまとめたもの。
「設計の意図」ではなく「間違えると何が壊れるか」を軸にした記録。

---

## 目次

1. [最も危険な 5大カテゴリ](#1-最も危険な-5大カテゴリ)
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

## 1. 最も危険な 5大カテゴリ

| #   | 分類                        | 症状                                             | 原因                                                            |
| --- | --------------------------- | ------------------------------------------------ | --------------------------------------------------------------- |
| 1   | **スタックオーバーフロー**  | テストプロセスが `SIGABRT`（exit 134）で死ぬ     | `>>=` チェーンで組んだ末尾再帰                                  |
| 2   | **pull 演算子の継続飛ばし** | 射影が最初の 1行だけ / JOIN の 2行目以降が消える | 継続に「次の行」ではなく「次のループ」を渡した                  |
| 3   | **SeqNo の取り違え**        | tombstone が消える / reopen 後 0行               | put と delete で同じ SeqNo、または reopen で SeqNo を再開しない |
| 4   | **O(n) な操作の混入**       | ポイント検索 43ms                                | `rangeScan` をポイント検索に使った                              |
| 5   | **F# の型推論の壊れ**       | 謎の FS0001 / FS0072                             | 引数の型注釈・スコープ                                          |

---

## 2. pull 演算子（`Op`）の罠

`Executor.fs` の `Op` は、「残りの計算」を値として持ち回る不変な pull 演算子（Volcano 方式 / イテレータモデル）。

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

### 2.4 `Op.distinct` は「直前の値」比較では書けない

`2, 1, 2` という入力では「直前の `1` と今の `2`」は違うので、重複除去できない。

```fsharp
// ❌ 2 が 2回目に出るので除去されない
// ✅ 既出の値を全部覚える（Set<string> / HashSet）必要がある
```

### 2.5 評価エラーを `Op.empty` で「0 行」にしない

JOIN の ON 句のように行ごとに評価する式が失敗したとき、`Op.empty` に落とすと
**エラーが消えて「一致なし」の結果**になる。
`INNER JOIN` では空結果、`LEFT JOIN` では NULL 拡張行が返り、どちらも原因を隠す。

```fsharp
// ❌ 評価失敗を「行が無い」と区別できない
| Error _ -> Op.empty
// ✅ Op の計算としてエラーを流す（戻り値の Result で呼び出し元へ届く）
| Error e -> Op.Compute(fun () -> err e)
```

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

- 右テーブルだけを参照する `WHERE b.v = 100` が無効化され**全行**返る
- 2テーブルにまたがる WHERE は pushdown 先が無いのに誰も置かず、**黙って無視される**

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
落とすと「NULL 拡張行を残すか」の判断が変わるので、JOIN の上での評価が必要。
`WHERE b.v IS NULL`（未一致行だけ残す）だと、入力へ落とした瞬間に**全行**になる。

- 修復: `buildLogical` は INNER のみ pushdown、LEFT は top へ回す。
  どのテーブルにも属さない述語も top に置いた。
- 回帰テスト: `DatabaseTests` の `Database: JOIN の WHERE は右テーブルと横断述語に効く` /
  `Database: LEFT JOIN の WHERE は null 供給側に効く`

### 3.5 入れ子 `Join` に「全スキーマ」を渡すと 3テーブル結合が 0行になる

`buildLogical` は `List.fold` で `Join(Join(a, b), c)` と**左深い木**を組む。
`exec` が左部分木へ `schema`（全テーブル分）をそのまま渡していたので、
内側の JOIN は「自分と左のテーブル」ではなく「全テーブル」のスキーマで `ON` 句を解決してしまう。

- 症状: `FROM a JOIN b ON … JOIN c ON …` が**エラーにならず 0行**を返す
  （2テーブル結合は正しく動く）。
  `joinPred` の `Schema.resolve` が列を見つけられず `Error` →
  `joinLoop` の `onError` が `Op.empty` を返すため、**黙って空**になる。
- 原因: `schema − 右テーブル` を「その level でのみ」計算し、
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

- 回帰テスト: `DatabaseTests` の `Database: 3テーブルの入れ子 JOIN が行を返す` /
  `末尾再帰: 60段の入れ子 JOIN が正しい行を返す`（深さ方向の累積を検証）

### 3.6 JOIN の対応付けをテーブル名で探すと自己結合が消える

`buildLogical` は `bindings |> List.findIndex (fun b -> b.RealName = j.Table.Name)` で
「その JOIN 句がどのバインディングか」を探していた。
`FROM t AS x JOIN t AS y`（自己結合）だと**両方 `RealName = "t"`** なので先頭にヒットし、
`| idx when idx = 0 -> …` で **JOIN が丸ごと省略**される。

- 症状: `SELECT p.id, q.v FROM t AS p JOIN t AS q ON p.id = q.id` が
  `列インデックス 3 が範囲外です（2列中）: q.v` で落ちる
  （JOIN が無いので p の 2列しか無く、q を参照する射影が外れる）。
  q を参照しなければ**黙って p の全行**を返す。
- ついでに F# の `List.findIndex` は未マッチだと**例外を投げる**のに、
  コードは `| -1 -> …` を見ていた（`List.FindIndex` との取り違え）。
  未マッチ経路は存在しなかった。

```fsharp
// ❌ RealName で探す（重複時に先頭へヒット、未マッチは例外）
let idx = bindings |> List.findIndex (fun b -> b.RealName = j.Table.Name)

// ✅ sources と同順なので「i 番目の JOIN 句 = bindings.[i + 1]」で位置対応
let right = bindings.[i + 1]
```

あわせて**識別名が重複する** `FROM t JOIN t`（エイリアス無し）は
`Plan.runSelect` で `ErrorKind.NameError` にする。
exec が「右テーブルを除いたスキーマ」を**名前で**引く（§3.5）ため、
重複名は区別できず静かな誤答になる。
自己結合は必ずエイリアスで表す。

- 回帰テスト: `DatabaseTests` の `Database: エイリアスでの自己結合が行を返す` /
  `Database: テーブル名の重複はエラー（エイリアスが必要）`

---

## 4. LSM / ストレージの罠

### 4.1 ポイント検索に `rangeScan` を使うと O(n)（10万倍遅くなる）

```fsharp
// ❌ ポイント検索なのに「そのキー以降の全エントリ」を挽いている
let fromMem =
    Memtable.rangeScan engine.Mem key 0
    |> Seq.map (...)
    |> firstVisible visible key
```

10万行で **1回 43ms**。
`SELECT COUNT(*)` 全体（10万行走査）が 0.2秒なので、ポイント検索のほうが**全走査より 200倍遅い**という異常な状態だった。

```fsharp
// ✅ ポイントルックアップ（O(log n)）を使う
let fromMem =
    match Memtable.getVisible engine.Mem key visible with
    | None -> ret None
    | Some e -> ret (Some(SstEntry.make e.Key e.SeqNo e.Value))
```

教訓: **ポイント検索・prefix 検索に `rangeScan` を使わない**。**`getVisible` を使う**。

> `Memtable.rangeScan` は遅延 seq になったため、❌ の形でも「キー以降の全件」は読まない
> （先頭の一致まで降りて、キーが変わったら止まる）。
> それでも `getVisible` のほうが軽い（列挙子も `Result` も作らず、O(log n) で直接引く）ので方針は不変。

### 4.2 `scanAt` の `startKey` は SSTable に効かない（**再オープン後だけ露見する**）

```fsharp
// ❌ 枝刈りだけして中身は全部 merge する
let files =
    Version.allFiles engine.Version
    |> List.filter (fun f -> Bytes.compare f.Largest startKey >= 0)
// ... そのままだと startKey より前のエントリも混ざる
```

`memtable` 側は `Memtable.rangeScan` が下限を適用するので、**同一プロセス内では動く**。
**reopen すると memtable が空になり、SSTable だけになることで初めて壊れる**。

```fsharp
// ✅ 下限は「読む側」が付ける
//   ファイルを選ぶ枝刈り（Largest >= startKey）と、中身の下限は別物。
//   → Sstable.rangeScan が index を二分探索してstartKey より前のブロックを読み飛ばし、
//     Memtable.rangeScan は木を途中から降りる。
let merged = SstEntry.mergeSorted (memSeq :: ordered) |> collapseDuplicates visible
// memtable も各ファイルも startKey 以降しか返さないので、merge 後の絞り込みは不要
```

教訓: **「ファイルを選ぶ枝刈り」と「中身の下限」は別物。両方やる**。
中身の下限はいま `Memtable.rangeScan` / `Sstable.rangeScan` が持っているので、`scanAt` 側で絞り直さない
（絞り直すと、下限が効かなくなった退行に気付けなくなる）。

### 4.3 prefix 検索は「1件目が答え」ではない

```fsharp
// ❌ scanAt の 1件目が別の値であることがある
RowStore.scanAt store ks prefix visible 0
|> Result.map (fun rows -> if rows.Length = 0 then None else Some rows.[0])

// ✅ 接頭辞一致を検査してから取る
RowStore.scanAt store ks prefix visible 0
|> Result.map (fun rows ->
    rows
    |> Array.tryFind (fun (k, _) -> KeyBuilder.startsWithPrefix k prefix)
    |> Option.bind snd)
```

`WHERE name = 'carol'` が `alice` の行を返すrowseのがこれ。

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

### 4.6 `Sstable.FooterSize` の変更は `decodeFooter` と成套で

`MaxSeqNo` を追加したときは `FooterSize` を `72 → 80` に。
**放置するとファイル名の長さがずれて、.footer の読み取りが壊れる**。

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

`RowStore.openStore` が keyspace を**開かず** `Engines = Map.empty` にしていたため、
`Database.openWith` の `TransactionManager.create ()` が `LastCommitSeqNo = 0` になり、
**ディスク上の行がすべて「未来」** と判定されて **0行**になっていた。

```fsharp
// ✅ 既存 keyspace を全部開いて、SeqNo を再開する
let openStore (root: string) : Result<RowStore, DbError> =
    protect (fun () -> Directory.CreateDirectory(root) |> ignore)
    |> Result.bind (fun () ->
        let known =
            Directory.GetDirectories(root)
            |> Array.choose (fun dir ->
                let name = Path.GetFileName dir
                match Keyspace.tryFromDirName name with
                | Some ks -> Some(ks, dir)
                | None -> None)
        // ... 全ての StorageEngine.openDatabase を engines に積む ...)
    // ...

// ✅ 開いた engine から SeqNo を復元する
let lastSeqNo (store: RowStore) : uint64 =
    store.Engines
    |> Map.toList
    |> List.fold
        (fun acc (_, (e, _)) -> max acc (if e.NextSeqNo = 0UL then 0UL else e.NextSeqNo - 1UL))
        0UL
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
  （`Compaction.gc` の `keepOlder`）。
- 最下位レベルでも、**同じキーの古い版が残るなら tombstone は消さない**。
  消すと下位の古い値が復活する（`Compaction.gc` は `older.IsEmpty` のときだけ落とす）。
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

`Dml.updateByPrimaryKey` の「delete → put」は**この構造で壊れる**ので、
「put だけ」（LSM は新しい SeqNo の put が旧 put を隠す）に変更。

索引側は「値が変わるときだけ 古い索引キーへ tombstone → 新しい索引キーへ put」。

### 5.5 `TransactionManager.create ()` を 2回呼ぶと SeqNo が衝突する

テストで `TransactionManager.create ()` を 2回呼ぶと、
両方とも SeqNo 1 から採番し、2つ目の transaction の書き込みが**先に見えた上位の SeqNo に隠される**。

```fsharp
// ✅ beginTxn の戻り値を次の transaction に引き継ぐ
let (mgr, txn) = TransactionManager.beginTxn mgr
// ... uzzyな処理 ...
let (mgr, txn2) = TransactionManager.beginTxn mgr
```

### 5.6 `Database.Execute` は新しい `Database` を返す

ハンドルは**不変**。

```fsharp
// ❌ 書き込みが失われる
db.Execute "INSERT ..." |> ignore

// ✅ 戻り値を持ち替える
let db2, _ = db.Execute "INSERT ..."
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

### 6.2 `Catalog.toSchema` は `Indexes = []` で埋まる

索引を定義しても実行系の `TableSchema.Indexes` に載らないと、
`Scan.byIndex` が「索引が無い」と言って検索に失敗する。

```fsharp
// ✅ contextOf で明示的に載せる
let withIndexes =
    { schema with
        Indexes = Catalog.indexSchemasOf state.Catalog t.Name }
```

### 6.3 `CREATE INDEX` に backfill が無いと過去行が見つからない

索引は**別の keyspace**（`i:<name>`）に持つので、
`CREATE INDEX` 時に中身を写さないと、**その前に `INSERT` した行が検索に出てこない**。

```fsharp
// ✅ 既存行を index keyspace に書き込む
Dml.scanByPrimaryKey state.Store withIndex Snapshot.noLimit 0
|> Result.bind (fun rows ->
    rows |> Array.fold (fun acc row ->
        let key = RowKey.indexKey withIndex idx row
        Ok(Transaction.put t (Keyspace.index idx.Name) key row)) (ret txn))
```

### 6.4 `Dml.getByIndex` は「完全な行」が必要

`RowKey.indexPrefix` は `row.[schema.IndexOf name]` を使うので、**完全な行**を要求する。
索引の値だけを持つ実行計画からは `IndexOutOfRangeException` になる。

```fsharp
// ✅ 値だけで引くなら専用の関数を使う
Dml.scanByIndexPrefix store index visible values 0
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

### 8.6 `Result.all` は `seq` を要求する

リスト・配列はそのまま渡せる。
空リストリテラルだけは要素型が決まらないので注釈を付ける（FS0030）。

```fsharp
// ✅ リストも配列も渡せる
all [ r1; r2 ]

// ✅ 空のときは注釈で要素型を決める
let empty: Result<int list, DbError> = all []
```

`Lattice.combine`（Executor.fs）と `Results.combine`（Planner.fs）は同じ処理の三重複だったので、
`Errors.fs` の `ResultOps.all` へ一本化して削除した。

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

### 8.9 `Array.takeWhile` は「配列を全部見る」

`Scan.byIndexPrefix` では「先頭から連続する接頭辞一致」を取りたいので `takeWhile` を使うが、
**ファイル側の下限が効いていないと 1件も一致しない**（§4.2 を参照）。

### 8.10 予約語

`default` / `value` / `old` / `type` / `base` は変数名・引数名に使えない。

```fsharp
// ❌ base は予約語
let f (base: Schema) = ...
// ✅
let f (tree: Logical) = ...
```

モジュール名に使えないのもある（`CompactionOptions.default` → `CompactionOptions.defaults`）。

`key` / `base` / `begin` / `new` / `old` / `type` / `value` は**匿名レコードのフィールド名にも使えない**。

```fsharp
// ❌ 予約語と衝突
let private entryKey ... = { Key = ... }

// ✅ 動詞で始る名前にする
let private hashKey (e: Entry) = ...
let private writeByte (w: Writer) = ...
```

モジュール名と型名が同じ（`Wal` / `Memtable`）と
`Module.field` が型と取り違えられます。型注釈付きのヘルパー経由で構築する。

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
末尾化したい関数が関数の中にいるなら、機を**モジュールスコープへ引き上げて**必要なものは引数で渡す
（`Expr.rewriteLoop`（`Ast.fs`）は `rewriteLeaf` を受け取る形で、
`Binder.qualify` と `Ops.substituteAggs` が共用している）。

**要素数に比例する反復は必ず後者にする**（`match` に直せば末尾になる）。
`>>=` で受けると候補の数・WHEN 節の数だけスタックが積まれる。
該当は `IN` の候補リストと `CASE` の WHEN 節（`Eval.eval`）で、
回帰テストは `ExecutorTests` の `末尾再帰: 巨大な IN 候補と WHEN 節を評価できる`。
カーソルからの数え上げ読み出しは `Reader.readItems`（Serialization.fs）に共通化し、
`Catalog` / `Wal` が使う。

**Debug の `Tailcalls` は既定で false**。`dotnet test` / `dotnet run` は Debug なので、
CPS の相互末尾再帰が `tail.` にならず、50 万個の `IN` リストで `Stack overflow.`（exit 134）になる。
`Directory.Build.props` の `<Tailcalls>true</Tailcalls>` を消すと再発するので注意。

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

「全部緑だが 0件走っていない」壊れた状態になる。
**リフレクションで `[<Fact>]` を数えるテスト自身**を置いた。

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

let db0 = ok (Database.openWith dir)
let db1, _ = ok (db0.Execute "CREATE TABLE t (id INTEGER PRIMARY KEY, age INTEGER)")
// ... 1 演算子ずつ確認 ...
```

重要: **DLL をコピーしないこと**。
FSI は `/tmp/probe.dll` のようなパスをキャッシュする。
**呼び出し側が同じ DLL を参照しているか**確認すること。

### 10.2 データが本当にディスクに書かれたか確認

keyspace ごとに base64 のディレクトリができる（`t:people` → `dDpwZW9wbGU=`）。

```fsharp
System.IO.Directory.GetFiles(dir, "*", System.IO.SearchOption.AllDirectories)
|> Array.iter (fun f -> printfn "%s (%d B)" f (int (System.IO.FileInfo f).Length))
```

### 10.3 treap の検索が O(n) になっていないか

`Memtable.getVisible` を使うようにすると **43ms → 0.3ms** になった。
「ポイント検索が遅い」BANはまず `rangeScan` を使っていないか確認する。

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

keyspace を分けている代償として、削除は主キー側に加えて**全索引にもtombstone を書き込む**。
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

### 11.8 keyspace 横断 commit は「先に intent、最後に削除」

テーブルと索引は別 keyspace・別 WAL。テーブルだけ書けて索引が書けないクラッシュを防ぐため、
`RowStore.writeBatchesWithSnapshot` は全バッチを `.commit/` の intent に原子的公開してから書き、
全 keyspace の適用後に intent を削除する。

- intent のペイロードは `Entry.encodeBatch`（txnId + 件数 + 各エントリ）を
  `Writer.lengthPrefixed` で包む。`Writer.writeBytes` で包むと
  `Reader.readLengthPrefixed` が先頭 8 バイトを長さと誤認し、`Reader: 8 バイトを読み取れません` になる。
- 再適用は `filterApplied` が `(key, SeqNo, value)` の完全一致を `GetAt` で確認して読み飛ばす。
  これを Memtable 全体の重複排除にしてはいけない（通常の同一 SeqNo 重複の意味が変わる）。
- intent 削除の失敗はエラーにする。握り潰して書き込みを続けると、後で同じキーが GC されたあとの再起動で古い intent が再適用され得る。

### 11.9 `ActiveSnapshot = 0` は「全版を残す」

`Compaction.gc` は `ActiveSnapshot = 0` を「全版を残す」として扱う。
0 を「最新版だけ残す」と解釈すると replay 中や境界未設定時に履歴を失う。
replay 中や境界未設定時は 0 のままにし、通常の commit では
`TransactionManager.activeSnapshot`（active の最小 snapshot、無ければ `LastCommitSeqNo + 1`）を設定する。

### 11.10 DROP は「keyspace の tombstone → カタログ削除」の順で行う

`DROP TABLE` / `DROP INDEX` はカタログから定義を外すだけでは不十分。
keyspace のディレクトリ名は名前から決まる（`t:<name>` / `i:<name>`）ので、
残骸があるまま同名で再作成すると**旧データや旧索引エントリが見える**。
特に「索引を DROP → 行を DELETE → 同名 CREATE INDEX」の順では、
DELETE 時に索引 tombstone が書かれないため、backfill 後の索引検索で削除済みの行が復活する。

- `DROP TABLE`: 残りの全行を `Dml.deleteByPrimaryKey`（主キー + 全索引へ tombstone）で消して commit し、その後 `Catalog.removeTable`。
- `DROP INDEX`: 索引 keyspace の全キーへ `Transaction.remove` を書いて commit し、その後 `Catalog.removeIndex`。
- 順序を逆にすると「カタログに無いのに keyspace は残る」状態で窓が空く。
  tombstone を先に確定させること。

### 11.11 compaction の出力分割は「件数」ではなく「バイト」

`SstEntry.splitInto (MaxInputFiles * 4)` のように **ファイル数の上限をエントリ数として使う**と、
値の大きさに関係なく 128エントリで分割され、大きい値では 1ファイルが肥大化する。
出力は `CompactionOptions.OutputFileTargetBytes`（既定 4MiB）と `SstEntry.splitIntoBytes` で分ける。
`splitIntoBytes` は概算バイト（`SstEntry.approximateBytes`）で切るので、厳密なファイルサイズではない。
固定長の型の定数は `ValueCodec.fixedEncodedBytes` に集約し、memtable の `Entry.approxBytes` と共有する
（TEXT / BLOB だけは、memtable が安価な UTF-16 長、SSTable が UTF-8 実バイト数と方針が違う）。
1エントリが目標を超える場合は、空の塊を作らず 1エントリだけのファイルにする。

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

`Manifest.decodeLines` は**途中**のレコード破損を `CorruptionError` にし、**末尾の書きかけ**だけを読み飛ばす。
`CURRENT` の指す manifest が無い場合や、`CURRENT` が無い/空なのに `*.sst` が残っている場合もエラーにする。

- 空の Version を返すと `StorageEngine.openDatabase` が全 SSTable を孤児として扱い、**データが見えなくなる**。
- 孤児は削除せず `<keyspace>/.orphans/` へ退避する（`quarantineOrphans`）。
  manifest を読み落としただけのケースでも実体は復元できる。
- 末尾の書きかけを許容するのは、`logAndApply` の `fs.Write` + `Flush(true)` が途中で切れた場合のため。
- 回帰テスト: `Manifest: 途中の壊れたレコードは CorruptionError` /
  `StorageEngine: manifest の途中破損では開かず SSTable を残す`。

### 11.14 古い `Database` ハンドルの `Close` は WAL を消さない

ハンドルは不変で、`Execute` は新しいハンドルを返す。
古いハンドルも `Close` を呼べるが、そのまま flush / WAL 削除をすると
**新しいハンドルの commit 済みデータを失う**。

`StorageEngine.close` は WAL の実サイズとハンドルの `Wal.Size` を比較し、
一致する（= 自分が最新の書き手）ときだけ flush と WAL 削除を行う。
一致しなければ何もしない（データは WAL に残っており、新しいハンドルが flush する）。

- 回帰テスト: `Database: 古いハンドルを Close しても新しいハンドルの commit は失われない`。

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
`Ddl.createIndex` は索引 keyspace を先に空にしてから backfill し、成功後にカタログへ登録する。
途中で失敗しても、次回の `CREATE INDEX` が残骸を消してからやり直す。

### 11.17 識別子の比較は `SqlName.equal` に一本化する

`TableSchema.indexOf` が完全一致で `Catalog.eqName` が `OrdinalIgnoreCase` だったため、
`CREATE INDEX ix ON c (V)` が成功しても索引キーに列が入らず、索引検索が **0 行** を返していた
（UNIQUE 索引では `row.[-1]` で未捕捉の `IndexOutOfRangeException`）。
`Types.fs` の `SqlName.equal` をカタログ・スキーマ・実行層で共用する。
`PRIMARY KEY (ID)` と列 `id` の対応も同じ規則で解決する。

### 11.18 `LIMIT` / `OFFSET` は 0 以上の整数リテラル

非リテラルを `0` に丸めると `LIMIT 1+0` が **0 行** になる（かつてドキュメントは「制限なし」と書いていた）。
`Planner.applyLimit` は式・負数・`Int32.MaxValue` 超を `SyntaxError` にする。
`Limit` ノードの `-1` が「LIMIT なし」、`0` は 0 行。

### 11.19 走査は候補 SSTable を先に開く

`lazyMergeScan` が開けなかったファイルを `Drained`（空）と見なしていたため、
壊れた SSTable があると `Ok` のまま行が欠けた（静かな誤答）。
`StorageEngine.scanAt` は `limit` の有無にかかわらず候補ファイルを先に `openSstable` し、
エラーをそのまま返す。`limit` による遅延オープンは諦めた（正しさを優先）。

### 11.20 壊れたデータは未捕捉例外にしない

ファイル内容から値を復元するコンストラクタは例外を送出し得る。境界で検証して `CorruptionError` にする:

- `Sstable.parseBlock`: `shared > prev.Length` を拒否。restart 件数は `(n / 4) - 1` 以下に制限（加算オーバーフロー防止）。
- `ValueCodec`: DECIMAL の flags/scale（scale ≤ 28・予約ビット 0）、DATE / TIMESTAMP の範囲。
- `VersionEdit`: レベルは `0..Levels.Max`。
- `BloomFilter.deserialize`: ビット数・ハッシュ数 0 を拒否（`% 0` の防止）。

### 11.21 `Result` を握り潰さない（DEFAULT / `Begin()` / schema）

- `DmlOps.insert`: 未指定列の DEFAULT の評価エラーを `NULL` にしない（値を指定した列の DEFAULT は評価しない）。
- `Database.Begin()` は `Result<Database, DbError>` を返す（カタログ再読み込みの失敗を隠さない）。
- `Engine.contextOf`: `Catalog.toSchema` の失敗を無視せず、文のエラーとして返す。
- `Ops.sortBy`: 非 NULL の代表値と比較できない値があれば、ソート前にエラーにする。
