// ===========================================================================
// Manifest — Version の変更履歴を MANIFEST に追記し、
// 「いま有効な manifest はどれか」を CURRENT（1行ファイル）で指す。
// MANIFEST が 4MiB を超えたら Version 全体を新しい manifest へ書き直す。
//
// CURRENT は一時ファイルに書いてから原子的にリネームで切り替えるので、
// クラッシュしても常に有効な manifest を指す。
// 各 VersionEdit は長さ付きで包むので、途中で切れた追記も検出できる。
// 途中のレコードが壊れていれば CorruptionError にして開かない（SSTable を消さない）。
// 末尾の書きかけ（後ろに有効な行が無い）だけはクラッシュ時の追記とみなして読み飛ばす。
// ===========================================================================

namespace RykDb.Core

module internal Manifest =
    /// manifest ファイル名の接頭辞。
    [<Literal>]
    let internal manifestPrefix = "MANIFEST-"

    let private fileName (fileNumber: int64) =
        sprintf "%s%06d" manifestPrefix fileNumber

    let private manifestPath (dir: string) (fileNumber: int64) =
        System.IO.Path.Combine(dir, fileName fileNumber)

    [<Literal>]
    let private currentFileName = "CURRENT"

    let private currentPath (dir: string) =
        System.IO.Path.Combine(dir, currentFileName)

    let private writeCurrent (dir: string) (fileNumber: int64) =
        protect (fun () ->
            let tmp = currentPath dir + ".tmp"
            System.IO.File.WriteAllText(tmp, fileName fileNumber + "\n")

            use fs =
                new System.IO.FileStream(
                    tmp,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.None
                )

            fs.Flush true
            fs.Close()
            System.IO.File.Move(tmp, currentPath dir, true))

    /// ディレクトリを作る（既にあれば何もしない）。
    let ensureDirectory (dir: string) =
        protect (fun () ->
            if not (System.IO.Directory.Exists dir) then
                System.IO.Directory.CreateDirectory dir |> ignore)

    /// データベースを開く（新規なら作成する）。
    /// 返り値は (Version, 有効な manifest 番号)。
    let openOrCreate (dir: string) =
        let readCurrent (dir: string) =
            protect (fun () ->
                if not (System.IO.File.Exists(currentPath dir)) then
                    None
                else
                    let text = System.IO.File.ReadAllText(currentPath dir).Trim()

                    if text = "" then
                        None
                    elif text.StartsWith manifestPrefix && text.EndsWith ".manifest" then
                        Some(System.Int64.Parse(text.Substring(9, text.Length - 9 - ".manifest".Length)))
                    elif text.StartsWith manifestPrefix then
                        Some(System.Int64.Parse(text.Substring 9))
                    else
                        raise (System.IO.InvalidDataException(sprintf "CURRENT の内容が不正: %s" text)))

        let readRecord (line: string) =
            try
                System.Convert.FromBase64String line |> VersionEdit.decode
            with
            | :? System.FormatException as e ->
                err (sprintf "manifest の base64 が不正: %s" e.Message |> DbError.corruption)
            | e -> err (sprintf "manifest のレコードが不正: %s" e.Message |> DbError.corruption)

        let rec decodeLines (lines: string list) (acc: VersionEdit list) =
            match lines with
            | [] -> Ok(List.rev acc, false)
            | line :: rest ->
                if line.Trim() = "" then
                    decodeLines rest acc
                else
                    match readRecord line with
                    | Ok edits -> decodeLines rest (List.rev edits @ acc)
                    | Error _ when rest |> List.forall (fun l -> l.Trim() = "") -> Ok(List.rev acc, true)
                    | Error e -> Error e

        let read dir fileNumber =
            let path = manifestPath dir fileNumber

            if System.IO.File.Exists path then
                Ok(System.IO.File.ReadAllLines path |> Array.toList)
            else
                Error(DbError.corruption (sprintf "有効な manifest が見つかりません: %s" (fileName fileNumber)))

        let readAll (dir: string) (fileNumber: int64) =
            match protect (fun () -> read dir fileNumber) with
            | Error e -> Error e
            | Ok(Error e) -> Error e
            | Ok(Ok lines) ->
                let hasContent = lines |> List.exists (fun line -> line.Trim() <> "")

                decodeLines lines []
                |> Result.map (fun (edits, truncated) -> edits, truncated, hasContent)

        let applyAll (edits: VersionEdit list) (initial: Version) =
            List.fold VersionEdit.apply initial edits

        let hasSstFiles (dir: string) =
            protect (fun () -> System.IO.Directory.GetFiles(dir, "*" + FileMetaData.sstExtension).Length > 0)

        result {
            let! _ = ensureDirectory dir
            let! hasSsts = hasSstFiles dir
            let! current = readCurrent dir

            match current with
            | Some manifestNumber ->
                let! decoded = readAll dir manifestNumber
                let edits, _, hasContent = decoded
                let version = applyAll edits Version.empty

                if List.isEmpty (Version.allFiles version) && hasContent && hasSsts then
                    return! err (DbError.corruption "manifest が壊れており、SSTable の管理情報を復元できません（SSTable は変更していません）")
                else
                    return version, manifestNumber
            | None ->
                if hasSsts then
                    return! err (DbError.corruption "CURRENT が無いのに SSTable が残っています（データ損失の恐れ）")
                else
                    let manifestNumber = 1L

                    let! _ =
                        protect (fun () -> System.IO.File.WriteAllText(manifestPath dir manifestNumber, ""))

                    let! _ = writeCurrent dir manifestNumber
                    return Version.empty, manifestNumber
        }

    /// manifest がこれを超えたら、現在の Version 全体を新しい manifest へ書き直す。
    [<Literal>]
    let private MaxManifestBytes = 4L * 1024L * 1024L

    let private parseManifestNumber (path: string) =
        let name = System.IO.Path.GetFileName path

        if name.StartsWith(manifestPrefix, System.StringComparison.Ordinal) then
            match System.Int64.TryParse(name.Substring 9) with
            | true, value -> Some value
            | _ -> None
        else
            None

    let private nextManifestNumber (dir: string) (current: int64) =
        protect (fun () ->
            System.IO.Directory.GetFiles(dir, manifestPrefix + "*")
            |> Array.choose parseManifestNumber
            |> Array.fold (fun acc number -> max acc number) current
            |> fun highest -> highest + 1L)

    /// 新しい manifest に Version 全体のスナップショットを書く。
    let private writeSnapshot (dir: string) (fileNumber: int64) (version: Version) =
        protect (fun () ->
            let edit =
                {
                    AddedFiles = Version.allFiles version
                    DeletedFiles = []
                }

            let bytes = VersionEdit.encode edit
            let text = System.Convert.ToBase64String bytes + "\n"
            let data = System.Text.Encoding.UTF8.GetBytes text

            use fs =
                new System.IO.FileStream(
                    manifestPath dir fileNumber,
                    System.IO.FileMode.Create,
                    System.IO.FileAccess.Write,
                    System.IO.FileShare.Read
                )

            fs.Write(data, 0, data.Length)
            fs.Flush true)

    /// .VersionEdit を反映し、Version と次の manifest 番号を返す。
    let logAndApply (dir: string) (manifestNumber: int64) (version: Version) (edit: VersionEdit) =
        let append (dir: string) (fileNumber: int64) (edit: VersionEdit) =
            protect (fun () ->
                let path = manifestPath dir fileNumber
                let bytes = VersionEdit.encode edit

                use fs =
                    new System.IO.FileStream(
                        path,
                        System.IO.FileMode.Append,
                        System.IO.FileAccess.Write,
                        System.IO.FileShare.Read
                    )

                let text = System.Convert.ToBase64String bytes + "\n"
                let data = System.Text.Encoding.UTF8.GetBytes text
                fs.Write(data, 0, data.Length)
                fs.Flush true)

        result {
            let! _ = append dir manifestNumber edit
            let next = VersionEdit.apply version edit
            let seed = version.NextFileNumber

            let highestAdded =
                edit.AddedFiles |> List.fold (fun acc f -> max acc f.FileNumber) seed

            let used = highestAdded + 1L
            let current = next.NextFileNumber

            let bumped =
                if used > current then
                    { next with NextFileNumber = used }
                else
                    next

            let! size =
                protect (fun () -> System.IO.FileInfo(manifestPath dir manifestNumber).Length)

            if size < MaxManifestBytes then
                return bumped, manifestNumber
            else
                let! nextNumber = nextManifestNumber dir manifestNumber
                let! _ = writeSnapshot dir nextNumber bumped
                let! _ = writeCurrent dir nextNumber

                protect (fun () ->
                    let old = manifestPath dir manifestNumber

                    if System.IO.File.Exists old then
                        System.IO.File.Delete old)
                |> ignore

                return bumped, nextNumber
        }
