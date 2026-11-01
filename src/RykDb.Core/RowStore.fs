// ===========================================================================
// RowStore — テーブル・索引を keyspace 単位の LSM ツリーへ載せる層。
//
//   主キー: keyspace "t:<table>"、キーは KeyBuilder.ofValues 主キー列
//   索引  : keyspace "i:<index>"、キーは KeyBuilder.ofIndexAndPrimary
//           （末尾に主キーを持たせるので索引値 → 行が一意に決まる）
//   ディレクトリ: <root>/<base32(keyspace)>/
//
// 行は列順の SqlValue 配列。保存時 ValueCodec でバイト列に落とす。
// ===========================================================================

namespace RykDb.Core

/// 書き込みの単位。(keyspace, key) -> 値（None は削除）。
module internal Keyspace =
    [<Literal>]
    let CatalogKeyspace = "__catalog__"

    let private base32Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

    let private MaxNameBytes = 128

    /// UTF-8 バイト列を base32（RFC 4648、パディングなし）にする。
    let private encodeBase32 (bytes: byte array) =
        let rec loop i buffer bits acc =
            if bits >= 5 then
                let shift = bits - 5
                let value = buffer >>> shift &&& 0x1F
                loop i (buffer &&& (1 <<< shift) - 1) shift (base32Alphabet.[value] :: acc)
            elif i < bytes.Length then
                loop (i + 1) (buffer <<< 8 ||| int bytes.[i]) (bits + 8) acc
            elif bits > 0 then
                let value = buffer <<< 5 - bits &&& 0x1F
                base32Alphabet.[value] :: acc
            else
                acc

        loop 0 0 0 [] |> List.rev |> List.toArray |> System.String

    /// base32 をデコードする。不正な文字・長さ・パディングなら None。
    let private decodeBase32 (text: string) =
        let valueOf c =
            if c >= 'A' && c <= 'Z' then Some(int c - int 'A')
            elif c >= 'a' && c <= 'z' then Some(int c - int 'a')
            elif c >= '2' && c <= '7' then Some(26 + int c - int '2')
            else None

        let rec loop i buffer bits acc =
            if i >= text.Length then
                if bits >= 5 || buffer <> 0 then
                    None
                else
                    Some(List.rev acc |> List.toArray)
            else
                match valueOf text.[i] with
                | None -> None
                | Some value ->
                    let buffer = buffer <<< 5 ||| value
                    let bits = bits + 5

                    if bits >= 8 then
                        let shift = bits - 8
                        let b = buffer >>> shift &&& 0xFF
                        loop (i + 1) (buffer &&& (1 <<< shift) - 1) shift (byte b :: acc)
                    else
                        loop (i + 1) buffer bits acc

        loop 0 0 0 []

    let private Utf8 = System.Text.UTF8Encoding(false, true)

    let private keyspaceOfBytes (bytes: byte array) =
        try
            let keyspace = Utf8.GetString bytes

            match keyspace with
            | ks when System.String.IsNullOrEmpty ks -> None
            | CatalogKeyspace -> Some keyspace
            | ks when
                not (ks.StartsWith("t:", System.StringComparison.Ordinal))
                && not (ks.StartsWith("i:", System.StringComparison.Ordinal))
                ->
                None
            | ks -> Some ks
        with _ ->
            None

    /// テーブルの主キー用 keyspace 名。
    let table (name: string) = "t:" + name

    /// インデックスのキー用 keyspace 名。
    let index (name: string) = "i:" + name

    /// keyspace 名をディレクトリ名に使える形にする。
    /// base32 は英大文字と数字だけで、`/` や大文字小文字だけの衝突を作らない。
    let toDirName (keyspace: string) =
        keyspace |> System.Text.Encoding.UTF8.GetBytes |> encodeBase32

    /// ディレクトリ名を keyspace 名に戻す（keyspace 名でなければ `None`）。
    let tryFromDirName = decodeBase32 >> Option.bind keyspaceOfBytes

    /// テーブル名・索引名の長さを検証する。
    /// ディレクトリ名（base32）が主要 OS の 255 バイト制限に収まる範囲に制限する。
    let validateName (kind: string) (name: string) =
        let bytes = System.Text.Encoding.UTF8.GetByteCount name

        if bytes > MaxNameBytes then
            sprintf "%s名 %s は長すぎます（UTF-8 で %d バイト以下にしてください）" kind name MaxNameBytes
            |> DbError.name
            |> err
        else
            ret ()

/// keyspace ごとのエンジンとスキーマを束ねたもの。
/// エンジンの開き方を差し替えられるので、テストではディスクに触れない実装を使える。
[<NoEquality; NoComparison>]
type internal RowStore =
    {
        Root: string
        /// keyspace → エンジン
        Engines: Map<string, IStorageEngine>
        /// まだ開いていない keyspace のエンジンを作る
        OpenEngine: string -> Result<IStorageEngine, DbError>
        /// プロセス間ロックの寿命管理。
        /// メモリ上のテスト用 store は None（ファイルを作らない）。
        Lock: ProcessLockLease option
    }

module internal RowStore =
    [<Literal>]
    let private ClearChunkEntries = 8192

    /// keyspace のエンジン（無ければ作る）。
    let ensure (keyspace: string) (store: RowStore) =
        match store.Engines |> Map.tryFind keyspace with
        | Some engine -> ret engine
        | None -> store.OpenEngine keyspace

    let private applyBatchesCore
        (idempotent: bool)
        (activeSnapshot: uint64)
        (store: RowStore)
        (batches: (string * Entry array) list)
        =
        let rec filter (engine: IStorageEngine) acc entries =
            match entries with
            | [] -> ret (List.rev acc |> List.toArray)
            | e: Entry :: tail ->
                match engine.GetAt (Some e.SeqNo) e.Key with
                | Error e -> err e
                | Ok(Some existing) when existing.SeqNo = e.SeqNo && existing.Value = e.Value -> filter engine acc tail
                | Ok _ -> filter engine (e :: acc) tail

        let filterApplied engine entries =
            if idempotent then
                entries |> Array.toList |> filter engine []
            else
                ret entries

        let rec writeAll rest current =
            match rest with
            | [] -> ret current
            | (ks, entries) :: tail ->
                if Array.isEmpty entries then
                    writeAll tail current
                else
                    match ensure ks current with
                    | Error e -> err e
                    | Ok engine ->
                        let engine = engine.WithActiveSnapshot activeSnapshot

                        match filterApplied engine entries with
                        | Error e -> err e
                        | Ok entries when Array.isEmpty entries -> writeAll tail current
                        | Ok entries ->
                            match engine.WriteBatch entries with
                            | Error e -> err e
                            | Ok updated ->
                                let maxSeq = Entry.maxSeqNo entries
                                let bumped = updated.WithNextSeqNo(max updated.NextSeqNo (maxSeq + 1UL))

                                match bumped.MaybeFlush() with
                                | Error e -> err e
                                | Ok flushed ->
                                    writeAll
                                        tail
                                        { current with
                                            Engines = Map.add ks flushed current.Engines
                                        }

        writeAll batches store

    /// keyspace ごとにバッチを適用する（intent の公開・削除はしない）。
    /// activeSnapshot は compaction の GC 境界。0 は「全版を残す」。
    let applyBatches = applyBatchesCore false

    /// keyspace ごとに Entry を書き込む。
    /// batches は (keyspace, Entry 配列) の列。
    /// Entry の SeqNo は TransactionManager が採番した値をそのまま使う。
    /// keyspace ごとに 1 から採番し直すと MVCC が壊れる（同じ SeqNo が別のキーを占めてしまう）。
    /// <remarks>
    /// 複数 keyspace にまたがる書き込みは、**先に intent を追記して公開**してから適用する。
    /// 適用が途中で失敗したら公開のままエラーを返す（握り潰さない）。
    /// 次回のオープンで再生し、冪等に適用される（`replayPending`）。
    /// 適用が終われば resolve を追記して解決する。
    /// 1 keyspace の書き込みは WAL 追記自体が原子的なので intent を作らない。
    /// </remarks>
    let private writeBatchesWithSnapshot
        (activeSnapshot: uint64)
        (batches: (string * Entry array) list)
        (store: RowStore)
        =
        let nonEmpty =
            batches |> List.filter (fun (_, entries) -> not (Array.isEmpty entries))

        if List.isEmpty nonEmpty then
            ret store
        elif nonEmpty.Length = 1 || System.String.IsNullOrEmpty store.Root then
            applyBatches activeSnapshot store nonEmpty
        else
            result {
                let! ident = CommitIntent.publish store.Root nonEmpty
                let! next = applyBatches activeSnapshot store nonEmpty
                let! () = CommitIntent.resolve store.Root ident
                CommitIntent.trimIfIdle store.Root
                return next
            }

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
            let! e = ensure keyspace store
            let! sst = e.GetAt visible key
            return Option.bind _.Value sst
        }

    /// keyspace を prefix で走査する（主キー/索引の前方一致）。
    let scanAt (keyspace: string) (startKey: byte array) (limit: int) (visible: uint64 option) (store: RowStore) =
        result {
            let! e = ensure keyspace store
            let! entries = e.ScanAt visible startKey limit

            return
                entries
                |> Seq.map (fun sst -> sst.Key, sst.Value)
                |> Seq.filter (fun (_, v) -> v.IsSome)
        }

    /// keyspace を走査する（制限なし）。
    let scan (keyspace: string) (startKey: byte array) (limit: int) = scanAt keyspace startKey limit None

    /// その keyspace に既にエンジンまたはディレクトリがあるか。
    /// 空の keyspace を読むためにエンジンを新規作成しないための判定。
    let keyspaceExists (keyspace: string) (store: RowStore) =
        Map.containsKey keyspace store.Engines
        || not (System.String.IsNullOrEmpty store.Root)
           && System.IO.Directory.Exists(System.IO.Path.Combine(store.Root, Keyspace.toDirName keyspace))

    /// keyspace の全エントリをチャンク単位で tombstone 消去する。
    /// 起動時の DROP 後始末にも使う。
    let clearKeyspaceChunks (mgr: TransactionManager) (space: string) (store: RowStore) =
        if not (keyspaceExists space store) then
            ret (store, mgr)
        else
            scan space [||] 0 store
            >>= fun stale ->
                stale
                |> Seq.chunkBySize ClearChunkEntries
                |> foldOk
                    (fun (currentStore, currentMgr) chunk ->
                        let mgr0, txn =
                            TransactionManager.beginTxnWithLimit (System.Int64.MaxValue |> Some) currentMgr

                        result {
                            let! txn2 = foldOk (fun t (key, _) -> ret (Transaction.remove space key t)) txn chunk
                            let! mgr1, _, batches = TransactionManager.commit txn2 mgr0
                            let! store1 = writeBatches batches currentStore
                            return store1, mgr1
                        })
                    (store, mgr)

    /// 全 keyspace の NextSeqNo の最大値 − 1（＝保存済みの SeqNo）。
    let lastSeqNo (store: RowStore) =
        store.Engines
        |> Map.values
        |> Seq.fold (fun acc e -> max acc (if e.NextSeqNo = 0UL then 0UL else e.NextSeqNo - 1UL)) 0UL

    /// DB を開く（keyspace は必要に応じて作られる）。
    /// <remarks>
    /// 既存の keyspace ディレクトリはすべて開いておく。
    /// 開かないまま後始末すると、Database.OpenWith が「SeqNo をどこから採番するか」を知れず、
    /// LastCommitSeqNo = 0 から始まり、既にディスクにある行がすべて「未来」として見えなくなる。
    /// 同じ root が既にこのプロセスで開かれていれば `IoError`。
    /// </remarks>
    let private openStoreCore (takeOver: bool) (root: string) =
        let isStoreDir dir =
            protect (fun () ->
                System.IO.File.Exists(System.IO.Path.Combine(dir, Manifest.CurrentFileName))
                || System.IO.Directory.GetFiles(dir, Manifest.ManifestPrefix + "*").Length > 0
                || System.IO.Directory.GetFiles(dir, "*" + Wal.WalExtension).Length > 0)
            |> Result.defaultValue false

        let discoverKeyspaces =
            System.IO.Directory.GetDirectories
            >> Array.filter isStoreDir
            >> Array.choose (System.IO.Path.GetFileName >> Keyspace.tryFromDirName)
            >> Array.toList
            >> List.distinct

        let openEngine keyspace =
            let dir = System.IO.Path.Combine(root, Keyspace.toDirName keyspace)

            result {
                do! Manifest.ensureDirectory dir
                let! engine = StorageEngine.openDatabase dir
                return DiskStorageEngine engine :> IStorageEngine
            }

        let rec openEngines acc keyspaces =
            match keyspaces with
            | [] -> ret acc
            | ks :: tail ->
                match openEngine ks with
                | Error e -> Error(e, acc)
                | Ok e -> openEngines (Map.add ks e acc) tail

        let closeEngines =
            Map.values
            >> Seq.iter (fun (engine: IStorageEngine) -> engine.Close() |> ignore)

        let replayPending store =
            if System.String.IsNullOrEmpty store.Root then
                ret store
            else
                result {
                    let! unresolved = CommitIntent.pending store.Root

                    return!
                        unresolved
                        |> foldOk
                            (fun current (ident, batches) ->
                                result {
                                    let! next = applyBatchesCore true 0UL current batches
                                    let! () = CommitIntent.resolve store.Root ident
                                    return next
                                })
                            store
                }

        result {
            do! protect (fun () -> System.IO.Directory.CreateDirectory root |> ignore)
            do! CommitIntent.touch root
            let! token = ProcessLock.acquire root takeOver

            let! known =
                match protect (fun () -> discoverKeyspaces root) with
                | Ok keyspaces -> ret keyspaces
                | Error e ->
                    ProcessLock.release root token
                    err e

            let! engines =
                match openEngines Map.empty known with
                | Ok opened when Map.containsKey Keyspace.CatalogKeyspace opened -> ret opened
                | Ok opened ->
                    match openEngine Keyspace.CatalogKeyspace with
                    | Ok catalogEngine -> ret (Map.add Keyspace.CatalogKeyspace catalogEngine opened)
                    | Error e ->
                        closeEngines opened
                        ProcessLock.release root token
                        err e
                | Error(e, opened) ->
                    closeEngines opened
                    ProcessLock.release root token
                    err e

            let store =
                {
                    Root = root
                    Engines = engines
                    OpenEngine = openEngine
                    Lock = Some(ProcessLockLease(root, token))
                }

            match replayPending store with
            | Ok ready -> return ready
            | Error e ->
                closeEngines store.Engines
                ProcessLock.release root token
                return! err e
        }

    /// DB を開く。同じ root が既にこのプロセスで開かれていれば `IoError`。
    let openStore = openStoreCore false

    /// 既に開いているハンドルを「クラッシュで失った」ことにして開き直す（テスト用）。
    /// 本番経路では使わない（二重に開くと memtable が分かれて古い行を読む）。
    let internal openStoreTakingOver = openStoreCore true

    /// エンジンの開き方を差し替えて、空の RowStore を作る（テスト用）。
    /// ディスクに触れない実装を渡せば、keyspace はすべてメモリ上に作られる。
    let internal openStoreWith (root: string) (openEngine: string -> Result<IStorageEngine, DbError>) =
        {
            Root = root
            Engines = Map.empty
            OpenEngine = openEngine
            Lock = None
        }

    /// 全 keyspace を close し、プロセス間ロックを外す。
    /// ロックの解除は二重 Close でも無害（トークンは一度しか外れない）。
    /// <remarks>
    /// 1つの engine の Close が失敗しても残りを閉じる。
    /// `foldOk` で途中打ち切りにすると、失敗した keyspace より後ろの memtable が flush されず
    /// WAL も残ったままになる。
    /// 戻り値は最初のエラー。
    /// </remarks>
    let close (store: RowStore) =
        let outcome =
            store.Engines
            |> Map.values
            |> Seq.fold
                (fun acc engine ->
                    match acc, engine.Close() with
                    | Error e, _ -> Error e
                    | Ok(), Error e -> Error e
                    | Ok(), Ok() -> Ok())
                (Ok())

        match store.Lock with
        | Some lease -> lease.ForceRelease()
        | None -> ()

        outcome
