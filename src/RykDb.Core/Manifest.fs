// ===========================================================================
// Manifest — Version の変更履歴を追記専用ファイルに記録し、
// 「いま有効な manifest はどれか」を CURRENT（1行ファイル）で指す。
//
// CURRENT は一時ファイルに書いてから原子的にリネームで切り替えるので、
// クラッシュしても常に有効な manifest を指す。
// 各 VersionEdit は長さ付きで包むので、途中で切れた追記も検出できる。
// ===========================================================================

namespace RykDb.Core

module internal Manifest =
    let private fileName (fileNumber: int64) = sprintf "MANIFEST-%06d" fileNumber

    let private manifestPath (dir: string) (fileNumber: int64) =
        System.IO.Path.Combine(dir, fileName fileNumber)

    let private currentFileName = "CURRENT"

    let private currentPath (dir: string) =
        System.IO.Path.Combine(dir, currentFileName)

    /// ディレクトリを作る（既にあれば何もしない）。
    let ensureDirectory (dir: string) =
        protect (fun () ->
            if not (System.IO.Directory.Exists dir) then
                System.IO.Directory.CreateDirectory dir |> ignore)

    let private writeCurrent (dir: string) (fileNumber: int64) =
        protect (fun () ->
            let tmp = currentPath dir + ".tmp"
            System.IO.File.WriteAllText(tmp, fileName fileNumber + "\n")
            System.IO.File.Move(tmp, currentPath dir, true))

    let private readCurrent (dir: string) =
        protect (fun () ->
            if not (System.IO.File.Exists(currentPath dir)) then
                None
            else
                let text = System.IO.File.ReadAllText(currentPath dir).Trim()

                if text = "" then
                    None
                elif text.StartsWith "MANIFEST-" && text.EndsWith ".manifest" then
                    Some(System.Int64.Parse(text.Substring(9, text.Length - 9 - ".manifest".Length)))
                elif text.StartsWith "MANIFEST-" then
                    Some(System.Int64.Parse(text.Substring 9))
                else
                    raise (System.IO.InvalidDataException(sprintf "CURRENT の内容が不正: %s" text)))

    let private readAll (dir: string) (fileNumber: int64) =
        let readRecord (line: string) =
            try
                System.Convert.FromBase64String line |> VersionEdit.decode
            with
            | :? System.FormatException as e ->
                err (sprintf "manifest の base64 が不正: %s" e.Message |> DbError.corruption)
            | e -> err (sprintf "manifest のレコードが不正: %s" e.Message |> DbError.corruption)

        let rec decodeLines (lines: string list) (acc: VersionEdit list) =
            match lines with
            | [] -> List.rev acc
            | line :: rest ->
                if line.Trim() = "" then
                    decodeLines rest acc
                else
                    match readRecord line with
                    | Ok edits -> decodeLines rest (edits @ acc)
                    | Error _ -> List.rev acc

        let scan () =
            let path = manifestPath dir fileNumber

            if System.IO.File.Exists path then
                decodeLines (System.IO.File.ReadAllLines path |> Array.toList) []
            else
                []

        protect scan

    /// データベースを開く（新規なら作成する）。
    /// 返り値は (Version, 有効な manifest 番号)。
    let openOrCreate (dir: string) =
        let applyAll (edits: VersionEdit list) (initial: Version) =
            List.fold VersionEdit.apply initial edits

        result {
            let! _ = ensureDirectory dir
            let! current = readCurrent dir

            match current with
            | Some manifestNumber ->
                let! edits = readAll dir manifestNumber
                let version = applyAll edits Version.empty
                return version, manifestNumber
            | None ->
                let manifestNumber = 1L

                let! _ =
                    protect (fun () -> System.IO.File.WriteAllText(manifestPath dir manifestNumber, ""))

                let! _ = writeCurrent dir manifestNumber
                return Version.empty, manifestNumber
        }

    /// .VersionEdit を反映し、Version と次の manifest 番号を返す。
    let logAndApply (dir: string) (manifestNumber: int64) (version: Version) (edit: VersionEdit) =
        let append (dir: string) (fileNumber: int64) (edit: VersionEdit) =
            protect (fun () ->
                let path = manifestPath dir fileNumber
                let bytes = VersionEdit.encode edit
                System.IO.File.AppendAllText(path, System.Convert.ToBase64String bytes + "\n"))

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

            return bumped, manifestNumber
        }
