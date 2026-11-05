// ===========================================================================
// RowStore — テーブル・索引・カタログを 1 本の LSM ツリーへ載せる層。
//
//   論理 keyspace: "t:<table>" / "i:<index>" / "__catalog__"
//   物理キー     : [varint(keyspace の UTF-8 長)][keyspace][論理キー]
//
// keyspace はキーの前方一致する接頭辞で分離する。
// 別 keyspace のキーが混ざらないよう、走査は接頭辞の境界で停止し、返すキーから接頭辞を外す。
// 行は列順の SqlValue 配列。保存時 ValueCodec でバイト列に変換する。
// ===========================================================================

namespace RykDb.Core

/// 書き込みの単位。(keyspace, key) -> 値（None は削除）。
module internal Keyspace =
    [<Literal>]
    let CatalogKeyspace = "__catalog__"

    let private MaxNameBytes = 128

    /// テーブルの主キー用 keyspace 名。
    let table (name: string) = "t:" + name

    /// インデックスのキー用 keyspace 名。
    let index (name: string) = "i:" + name

    /// 物理キーの keyspace 接頭辞。
    /// 長さを先頭に置くため、`t:a` と `t:ab` のような前方一致する名前でも別 keyspace のキーが混ざらない。
    let prefix (keyspace: string) = Writer.build [ Writer.str keyspace ]

    /// 論理キーを 1本の LSM 用の物理キーにする。
    let prefixedKey (keyspace: string) (key: byte array) = Array.append (prefix keyspace) key

    /// テーブル名・索引名の長さを検証する。
    let validateName (kind: string) (name: string) =
        let bytes = System.Text.Encoding.UTF8.GetByteCount name

        if bytes > MaxNameBytes then
            sprintf "%s名 %s は長すぎます（UTF-8 で %d バイト以下にしてください）" kind name MaxNameBytes
            |> DbError.name
            |> err
        else
            ret ()

/// 1本の LSM ツリーと、その論理 keyspace を束ねたもの。
/// エンジンを差し替えられるので、テストではディスクに触れない実装を使える。
[<NoEquality; NoComparison>]
type internal RowStore =
    {
        Root: string
        Engine: IStorageEngine
        /// プロセス間ロックの寿命管理。
        /// メモリ上のテスト用 store は None（ファイルを作らない）。
        Lock: ProcessLockLease option
    }

module internal RowStore =
    [<Literal>]
    let private ClearChunkEntries = 8192

    /// レイアウトの format マーカー。
    [<Literal>]
    let private FormatFileName = ".rykdb.format"

    [<Literal>]
    let private FormatVersion = "rykdb-prefixed-v1"

    /// 論理 keyspace 名は物理キーの接頭辞で表すため、エンジンは常に 1本。
    let ensure (store: RowStore) = ret store.Engine

    /// Entry を論理 keyspace の接頭辞付きで書き込む。
    /// 複数 keyspace の書き込みも同じ WAL バッチに入るため、途中で分断されない。
    let writeBatchesWithSnapshot (activeSnapshot: uint64) (batches: (string * Entry array) list) (store: RowStore) =
        let physicalEntries =
            batches
            |> List.collect (fun (keyspace, entries) ->
                entries
                |> Array.toList
                |> List.map (fun e ->
                    { e with
                        Key = Keyspace.prefixedKey keyspace e.Key
                    }))
            |> Array.ofList

        if Array.isEmpty physicalEntries then
            ret store
        else
            let engine = store.Engine.WithActiveSnapshot activeSnapshot

            match engine.WriteBatch physicalEntries with
            | Error e -> err e
            | Ok updated ->
                let maxSeq = Entry.maxSeqNo physicalEntries
                let bumped = updated.WithNextSeqNo(max updated.NextSeqNo (maxSeq + 1UL))

                match bumped.MaybeFlush() with
                | Ok flushed -> ret { store with Engine = flushed }
                | Error _ -> ret { store with Engine = bumped }

    /// GC 境界を指定しない書き込み（カタログ・索引 backfill・テスト用）。
    let writeBatches = writeBatchesWithSnapshot 0UL

    /// commit してバッチを Store に書き込む。
    /// 戻り値は (書き込み後の Store, commit 後の Manager)。
    /// Active / Pending のような state フィールドの更新は呼び出し側で行う
    /// （backfillIndex はトランザクションを外さないため触らない）。
    /// GC 境界は「自ハンドルの active snapshot」と「同じ DB を開いている他ハンドルの active snapshot」の最小値。
    /// 後者は `ActiveSnapshotRegistry` から集約する（`registration` は自分自身の登録で、除外する）。
    /// 書き込みは `RootGate` の共有ロックを取り、DDL の排他と交差しないようにする。
    let commitToStore
        (root: string)
        (registration: ActiveRegistration option)
        (mgr: TransactionManager)
        (txn: Transaction)
        (store: RowStore)
        =
        RootGate.withShared root (fun () ->
            result {
                let! manager, _, batches = TransactionManager.commit txn mgr

                let snapshot =
                    let own = TransactionManager.activeSnapshot manager

                    match ActiveSnapshotRegistry.oldestExcept root registration with
                    | Some other -> min own other
                    | None -> own

                let! next = writeBatchesWithSnapshot snapshot batches store
                return next, manager
            })

    /// keyspace から読む（SeqNo 制限つき）。
    let getAt (keyspace: string) (key: byte array) (visible: uint64 option) (store: RowStore) =
        result {
            let! e = ensure store
            let! sst = e.GetAt visible (Keyspace.prefixedKey keyspace key)
            return Option.bind _.Value sst
        }

    /// 接頭辞の境界で走査を止め、返すキーから接頭辞を外す。
    let private scanWithinPrefix (prefix: byte array) (scan: SstScan) =
        let rec loop current =
            match SstScan.next current with
            | Error e -> err e
            | Ok None -> ret None
            | Ok(Some(e, rest)) ->
                if not (KeyBuilder.startsWithPrefix e.Key prefix) then
                    ret None
                else
                    let key = Array.sub e.Key prefix.Length (e.Key.Length - prefix.Length)
                    ret (Some({ e with Key = key }, SstScan(fun () -> loop rest)))

        SstScan(fun () -> loop scan)

    /// keyspace を prefix で走査する（最新エントリ。値の filter はしない）。
    /// 返すキーは接頭辞を外した論理キー。1行要求されるまで SSTable のブロックを読まない。
    let scanAtRaw (keyspace: string) (startKey: byte array) (limit: int) (visible: uint64 option) (store: RowStore) =
        let prefix = Keyspace.prefix keyspace

        result {
            let! e = ensure store
            let! scan = e.ScanAt visible (Keyspace.prefixedKey keyspace startKey) limit
            return scanWithinPrefix prefix scan
        }

    /// keyspace を prefix で走査する（主キー/索引の前方一致）。
    /// 返り値は pull（`SstScan`）。tombstone は含まない。
    let scanAt (keyspace: string) (startKey: byte array) (limit: int) (visible: uint64 option) (store: RowStore) =
        scanAtRaw keyspace startKey limit visible store
        |> Result.map (SstScan.filter (fun e -> e.Value.IsSome))

    /// keyspace を走査する（制限なし）。
    let scan (keyspace: string) (startKey: byte array) (limit: int) = scanAt keyspace startKey limit None

    /// keyspace の全エントリをチャンク単位で tombstone 消去する。
    /// 起動時の DROP 後始末にも使う。
    /// 同じ keyspace を走査しながら書くため、1チャンク書くたびに新しい store で
    /// `lastKey` から走査し直す（走査中に compaction がファイルを消しても安全）。
    let clearKeyspaceChunks (mgr: TransactionManager) (space: string) (store: RowStore) =
        let rec takeKeys n acc scan =
            if n <= 0 then
                ret (List.rev acc, scan)
            else
                match SstScan.next scan with
                | Error e -> err e
                | Ok None -> ret (List.rev acc, SstScan.Empty)
                | Ok(Some(e, rest)) -> takeKeys (n - 1) (e.Key :: acc) rest

        let rec loop (currentStore, currentMgr) startKey =
            match scan space startKey 0 currentStore with
            | Error e -> err e
            | Ok stale ->
                match takeKeys ClearChunkEntries [] stale with
                | Error e -> err e
                | Ok([], _) -> ret (currentStore, currentMgr)
                | Ok(chunk, _) ->
                    let lastKey = List.last chunk

                    let mgr0, txn =
                        TransactionManager.beginTxnWithLimit (System.Int64.MaxValue |> Some) currentMgr

                    match foldOk (fun t key -> ret (Transaction.remove space key t)) txn chunk with
                    | Error e -> err e
                    | Ok txn2 ->
                        match TransactionManager.commit txn2 mgr0 with
                        | Error e -> err e
                        | Ok(mgr1, _, batches) ->
                            match writeBatches batches currentStore with
                            | Error e -> err e
                            | Ok store1 -> loop (store1, mgr1) lastKey

        loop (store, mgr) [||]

    /// 保存済みの SeqNo（= 次に採番する値 - 1）。
    let lastSeqNo (store: RowStore) =
        let next = store.Engine.NextSeqNo
        if next = 0UL then 0UL else next - 1UL

    let private formatPath (root: string) =
        System.IO.Path.Combine(root, FormatFileName)

    /// format マーカーを検証し、新規なら作る。
    let private ensureFormat (root: string) =
        let path = formatPath root

        result {
            let! exists = protect (fun () -> System.IO.File.Exists path)

            if exists then
                let! text = protect (fun () -> System.IO.File.ReadAllText path)

                if text.Trim() = FormatVersion then
                    return ()
                else
                    return! err (sprintf "データベース %s の format が不正です（%s）" root FormatFileName |> DbError.corruption)
            else
                do! protect (fun () -> System.IO.File.WriteAllText(path, FormatVersion + "\n"))
                return ()
        }

    /// DB を開く（新規ならルート直下に 1本の LSM を作る）。
    /// 同じ root が既にこのプロセスで開かれていれば `IoError`。
    let private openStoreCore (takeOver: bool) (root: string) =
        result {
            do! protect (fun () -> System.IO.Directory.CreateDirectory root |> ignore)
            let! token = ProcessLock.acquire root takeOver

            let! () =
                match ensureFormat root with
                | Ok() -> ret ()
                | Error e ->
                    ProcessLock.release root token
                    err e

            let! engine =
                match StorageEngine.openDatabase root with
                | Ok engine -> ret (DiskStorageEngine engine :> IStorageEngine)
                | Error e ->
                    ProcessLock.release root token
                    err e

            return
                {
                    Root = root
                    Engine = engine
                    Lock = Some(ProcessLockLease(root, token))
                }
        }

    /// DB を開く。同じ root が既にこのプロセスで開かれていれば `IoError`。
    let openStore = openStoreCore false

    /// 既に開いているハンドルを「クラッシュで失った」ことにして開き直す（テスト用）。
    /// 本番経路では使わない（二重に開くと memtable が分かれて古い行を読む）。
    let internal openStoreTakingOver = openStoreCore true

    /// エンジンを差し替えて、空の RowStore を作る（テスト用）。
    /// ディスクに触れないメモリ実装を渡せば、全 keyspace が 1本の LSM に載る。
    let internal openStoreWith (root: string) (engine: IStorageEngine) =
        {
            Root = root
            Engine = engine
            Lock = None
        }

    /// LSM を close し、プロセス間ロックを外す。
    /// ロックの解除は二重 Close でも無害（トークンは一度しか外れない）。
    let close (store: RowStore) =
        let outcome = store.Engine.Close()

        match store.Lock with
        | Some lease -> lease.ForceRelease()
        | None -> ()

        outcome
