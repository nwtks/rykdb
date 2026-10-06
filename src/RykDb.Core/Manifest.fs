// ===========================================================================
// Manifest — Version の変更履歴を追記専用ファイルに記録し、
// 「いま有効な manifest はどれか」を CURRENT（1 行ファイル）で指す。
//
// CURRENT は一時ファイルに書いてから原子的にリネームで切り替えるので、
// クラッシュしても常に有効な manifest を指す。
// 各 VersionEdit は長さ付きで包むので、途中で切れた追記も検出できる。
// 詳細は docs/architecture.md §9.5。
// ===========================================================================

namespace RykDb.Core

open System
open System.IO

module internal Manifest =
    let private fileName (fileNumber: int64) = sprintf "MANIFEST-%06d" fileNumber

    let private currentFileName = "CURRENT"

    let private currentPath (dir: string) = Path.Combine(dir, currentFileName)

    let private manifestPath (dir: string) (fileNumber: int64) = Path.Combine(dir, fileName fileNumber)

    /// ディレクトリを作る（既にあれば何もしない）。
    let ensureDirectory (dir: string) =
        protect (fun () ->
            if not (Directory.Exists dir) then
                Directory.CreateDirectory dir |> ignore)

    let private writeCurrent (dir: string) (fileNumber: int64) =
        protect (fun () ->
            let tmp = currentPath dir + ".tmp"
            File.WriteAllText(tmp, fileName fileNumber + "\n")
            File.Move(tmp, currentPath dir, true))

    let private readCurrent (dir: string) =
        protect (fun () ->
            if not (File.Exists(currentPath dir)) then
                None
            else
                let text = File.ReadAllText(currentPath dir).Trim()

                if text = "" then
                    None
                elif text.StartsWith "MANIFEST-" && text.EndsWith ".manifest" then
                    Some(Int64.Parse(text.Substring(9, text.Length - 9 - ".manifest".Length)))
                elif text.StartsWith "MANIFEST-" then
                    Some(Int64.Parse(text.Substring 9))
                else
                    raise (InvalidDataException(sprintf "CURRENT の内容が不正: %s" text)))

    let private append (dir: string) (fileNumber: int64) (edit: VersionEdit) =
        protect (fun () ->
            let path = manifestPath dir fileNumber
            let bytes = VersionEdit.encode edit
            File.AppendAllText(path, Convert.ToBase64String bytes + "\n"))

    let private readRecord (line: string) =
        try
            VersionEdit.decode (Convert.FromBase64String line)
        with
        | :? FormatException as e -> Error(DbError.corruption (sprintf "manifest の base64 が不正: %s" e.Message))
        | e -> Error(DbError.corruption (sprintf "manifest のレコードが不正: %s" e.Message))

    let private readAll (dir: string) (fileNumber: int64) =
        let rec go (lines: string list) (acc: VersionEdit list) =
            match lines with
            | [] -> List.rev acc
            | line :: rest ->
                if line.Trim() = "" then
                    go rest acc
                else
                    match readRecord line with
                    | Ok edits -> go rest (edits @ acc)
                    | Error _ -> List.rev acc

        let scan () =
            let path = manifestPath dir fileNumber

            if File.Exists path then
                go (File.ReadAllLines path |> Array.toList) []
            else
                []

        protect scan

    let private applyAll (edits: VersionEdit list) (initial: RykDb.Core.Version) =
        let step (v: RykDb.Core.Version) (e: VersionEdit) =
            e.DeletedFiles
            |> List.fold (fun v f -> Version.deleteFiles [ f ] v) v
            |> fun v -> e.AddedFiles |> List.fold (fun v f -> Version.addFile f v) v

        List.fold step initial edits

    /// データベースを開く（新規なら作成する）。
    /// 返り値は (Version, 有効な manifest 番号)。
    let openOrCreate (dir: string) =
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
                let! _ = protect (fun () -> File.WriteAllText(manifestPath dir manifestNumber, ""))
                let! _ = writeCurrent dir manifestNumber
                return Version.empty, manifestNumber
        }

    /// .VersionEdit を反映し、Version と次の manifest 番号を返す。
    let logAndApply (dir: string) (manifestNumber: int64) (version: RykDb.Core.Version) (edit: VersionEdit) =
        result {
            let! _ = append dir manifestNumber edit
            let afterDelete = Version.deleteFiles edit.DeletedFiles version
            let next = edit.AddedFiles |> List.fold (fun v f -> Version.addFile f v) afterDelete
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
