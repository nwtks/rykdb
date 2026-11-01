namespace RykDb.Core

/// DROP 後の keyspace 後始末を再開するためのジャーナル。
/// カタログ削除前にこれを書き、後始末が完了したら消す。
/// 起動時にカタログに存在しない keyspace だけを掃除する。
module internal DropCleanup =
    [<Literal>]
    let private FileName = ".drop-cleanup"

    let private path (root: string) = System.IO.Path.Combine(root, FileName)

    /// 未完了の後始末対象 keyspace を読む。
    let pending (root: string) =
        if System.String.IsNullOrEmpty root then
            ret []
        else
            protect (fun () ->
                let p = path root

                if not (System.IO.File.Exists p) then
                    []
                else
                    System.IO.File.ReadAllLines p
                    |> Array.filter (fun line -> line.Trim() <> "")
                    |> Array.map (fun line ->
                        System.Convert.FromBase64String line |> System.Text.Encoding.UTF8.GetString)
                    |> Array.toList)

    /// 後始末対象を書く（カタログ削除より前）。
    let write (root: string) (spaces: string list) =
        if System.String.IsNullOrEmpty root then
            ret ()
        elif List.isEmpty spaces then
            ret ()
        else
            protect (fun () ->
                let p = path root
                let tmp = p + ".tmp"

                let text =
                    spaces
                    |> List.map (fun space -> System.Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes space))
                    |> String.concat "\n"

                let data = System.Text.Encoding.UTF8.GetBytes(text + "\n")

                use fs =
                    new System.IO.FileStream(
                        tmp,
                        System.IO.FileMode.Create,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.None
                    )

                fs.Write(data, 0, data.Length)
                fs.Flush true
                fs.Close()
                System.IO.File.Move(tmp, p, true))

    /// 既存の未完了対象を残したまま、新しい後始末対象を追加する。
    let add (root: string) (spaces: string list) =
        pending root >>= fun existing -> write root (List.distinct (existing @ spaces))

    /// 後始末が完了したらジャーナルを消す。
    let clear (root: string) =
        if System.String.IsNullOrEmpty root then
            ret ()
        else
            protect (fun () ->
                let p = path root
                let tmp = p + ".tmp"

                if System.IO.File.Exists p then
                    System.IO.File.Delete p

                if System.IO.File.Exists tmp then
                    System.IO.File.Delete tmp)

/// 中断した DROP の keyspace 後始末を open 時に再開する。
module internal DropCleanupRecovery =
    let private shouldClean (catalog: Catalog) (space: string) =
        if space.StartsWith("t:", System.StringComparison.Ordinal) then
            Catalog.findTable catalog (space.Substring 2) |> Option.isNone
        elif space.StartsWith("i:", System.StringComparison.Ordinal) then
            Catalog.findIndex catalog (space.Substring 2) |> Option.isNone
        else
            false

    let recover (store: RowStore) (catalog: Catalog) (mgr: TransactionManager) =
        let rec clear (currentStore, currentMgr) spaces =
            match spaces with
            | [] -> ret (currentStore, currentMgr)
            | space :: tail ->
                match RowStore.clearKeyspaceChunks currentMgr space currentStore with
                | Error e -> err e
                | Ok next -> clear next tail

        DropCleanup.pending store.Root
        >>= (List.filter (shouldClean catalog) >> clear (store, mgr))
        >>= fun ready ->
            DropCleanup.clear store.Root |> ignore
            ret ready
