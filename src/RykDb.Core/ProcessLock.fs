namespace RykDb.Core

/// 同じ DB を二重に開かないようにする（`.rykdb.lock`）。
/// <remarks>
/// **別プロセス**の同時オープンは WAL / MANIFEST を壊すので `FileShare.None` で拒否する。
/// **同じプロセス**でも既に開いている root は二度開けない。
/// `Database.OpenWith` のたびに別の `RowStore`（別 memtable / WAL ビュー）ができ、
/// 一方の commit が他方から見えない（古い行を読んでしまう）ため。
/// クラッシュ再現テストだけは `takeOver = true` で意図的に二重オープンする。
/// 最後のハンドルを Close するとファイルロックも解放される。
/// </remarks>
module internal ProcessLock =
    /// root に置くロックファイル名。
    let private FileName = ".rykdb.lock"

    /// 台帳の更新だけを直列化する（ロック自体は root ごとに 1本）。
    let private Gate = new System.Threading.SemaphoreSlim(1, 1)

    /// 正規化した root -> (トークン -> プロセス内で共有する FileStream)。
    let private Held =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            System.Collections.Concurrent.ConcurrentDictionary<obj, System.IO.FileStream>
         >
            System.StringComparer.Ordinal

    /// ロックを取り、解除に使うトークンを返す。
    /// - 別プロセスが開いている → `IoError`
    /// - 同じプロセスが既に開いている → `takeOver` でなければ `IoError`
    let acquire (root: string) (takeOver: bool) =
        Gate.Wait()

        try
            let key = RootKey.normalize root

            let bucket =
                Held.GetOrAdd(
                    key,
                    fun _ -> System.Collections.Concurrent.ConcurrentDictionary<obj, System.IO.FileStream>()
                )

            let token = obj ()

            match bucket |> Seq.tryHead with
            | Some existing when takeOver ->
                bucket.[token] <- existing.Value
                ret token
            | Some _ ->
                err (DbError.create ErrorKind.IoError (sprintf "データベース %s は同じプロセスで既に開かれています。Close してから開き直してください" key))
            | None ->
                let path = System.IO.Path.Combine(key, FileName)

                match
                    protect (fun () ->
                        new System.IO.FileStream(
                            path,
                            System.IO.FileMode.OpenOrCreate,
                            System.IO.FileAccess.ReadWrite,
                            System.IO.FileShare.None
                        ))
                with
                | Ok stream ->
                    bucket.[token] <- stream
                    ret token
                | Error _ ->
                    err (DbError.create ErrorKind.IoError (sprintf "データベース %s は別のプロセスが開いています（%s）" key FileName))
        finally
            Gate.Release() |> ignore

    /// ロックを解除する（二重解除は無害）。
    /// 同じ root を開いている他トークンが残っていればロックは保持したまま。
    let release (root: string) (token: obj) =
        Gate.Wait()

        try
            let key = RootKey.normalize root

            match Held.TryGetValue key with
            | true, bucket ->
                match bucket.TryRemove token with
                | true, stream ->
                    if bucket.IsEmpty then
                        Held.TryRemove key |> ignore
                        protect (fun () -> stream.Dispose()) |> ignore
                | _ -> ()
            | _ -> ()
        finally
            Gate.Release() |> ignore
