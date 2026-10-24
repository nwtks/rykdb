// ===========================================================================
// Executor — Volcano（イテレータ）方式の実行層。
// pull 演算子 Op（Done / Step / Compute）で組む。
// LIMIT / WHERE の早期打ち切りが効き、無限長でもメモリは 1行分。
// ===========================================================================

namespace RykDb.Core

/// 出力の 1列。
type internal OutputColumn =
    {
        /// 表示名（別名があればそれ、無ければ列名）
        Name: string
        /// 修飾名（`t.id`）。修飾子付き参照の解決に使う。
        Qualifier: string option
        /// 型（式から導出できない場合は None）
        DataType: DataType option
    }

    member this.Display =
        match this.Qualifier with
        | Some q -> sprintf "%s.%s" q this.Name
        | None -> this.Name

module internal OutputColumn =
    /// テーブルの 1列から作る（修飾名はテーブル名か別名）。
    let ofTableColumn (table: string) (col: ColumnDefinition) =
        {
            Name = col.Name
            Qualifier = Some table
            DataType = Some col.DataType
        }

/// 演算子の出力スキーマ。
/// 列の解決と結果表示に使う。
type internal Schema =
    {
        Columns: OutputColumn array
        /// 参照可能なテーブル名（修飾子解決の候補）
        Tables: string list
    }

module internal Schema =
    let empty = { Columns = [||]; Tables = [] }

    /// 横連結（JOIN 用）。
    let concat (schema: Schema) (other: Schema) =
        {
            Columns = Array.append schema.Columns other.Columns
            Tables = schema.Tables @ other.Tables
        }

    /// スキーマ列を左から連結する（FROM / JOIN の出力スキーマ組み立て）。
    let concatAll (schemas: Schema list) =
        schemas |> List.fold (fun (acc: Schema) (s: Schema) -> concat acc s) empty

    /// `*` / `t.*` が表す出力列を展開する。
    /// どちらもなければ None。
    /// <remarks>
    /// ヘッダ行（projectHeader）と射影の式（Ops.project）が同じ列を共有する。
    /// どちらかが先に変わると `SELECT *` の列数とヘッダがずれる（実際にずれた）。
    /// Expr は `[<NoComparison>]` なので「どちらの star だったか」は別のフラグで返す
    /// （qualified = `t.*` なら true）。
    /// </remarks>
    let expandStar (schema: Schema) (e: Expr) =
        match e with
        | Expr.Star -> Some(false, schema.Columns |> Array.toList)
        | Expr.QualifiedStar q ->
            Some(
                true,
                schema.Columns
                |> Array.filter (fun (c: OutputColumn) -> SqlName.equal (defaultArg c.Qualifier "") q)
                |> Array.toList
            )
        | _ -> None

    /// 射影後のヘッダ行を作る。
    /// `SELECT *` は入力の列名をそのまま（修飾つき）。
    /// `SELECT id` は式を表示する。別名があればそれを使う。
    /// 入力スキーマのヘッダを返すと `SELECT id` が `people.id` になるので、
    /// 射影後の名前を組み立て直す必要がある。
    /// <remarks>
    /// `*` は修飾なしの `c.Name`、`t.*` は修飾つきの `c.Display` で出す（元の挙動）。
    /// </remarks>
    let projectHeader (schema: Schema) (items: SelectItem list) =
        let header (schema: Schema) =
            schema.Columns |> Array.map (fun c -> c.Display)

        match items with
        | [] -> header schema
        | _ ->
            items
            |> List.collect (fun item ->
                match expandStar schema item.Expression with
                | Some(qualified, columns) ->
                    if qualified then
                        columns |> List.map (fun c -> c.Display)
                    else
                        columns |> List.map (fun c -> c.Name)
                | None ->
                    match item.Alias with
                    | Some a -> [ a ]
                    | None -> [ Expr.toDisplayString item.Expression ])
            |> List.toArray

    /// 式を「列の直接参照」に読み替える。
    /// Column はそのまま。修飾子なしの裸の名前（Name）も列として扱う
    /// （パーサが WHERE / ORDER BY の裸の名前を Name にするため）。
    /// 集約呼び出しや式は列ではないので None。
    let tryAsColumnRef (e: Expr) =
        match e with
        | Expr.Column c -> Some c
        | Expr.Name n -> Some { Qualifier = None; Name = n }
        | _ -> None

    /// テーブルの全列を並べる。
    let ofTable (table: string) (alias: string option) (schema: TableSchema) =
        let name = defaultArg alias table

        let cols =
            schema.Columns
            |> List.map (fun c -> OutputColumn.ofTableColumn name c)
            |> List.toArray

        { Columns = cols; Tables = [ name ] }

    /// 列参照を解決する。見つからなければエラー、曖昧ならエラー。
    /// 大文字小文字を区別しない（`SqlName.equal` と同じ規則）。
    /// 行ごとに呼ばれるので、文字列の小文字化や一時配列を作らない。
    let resolve (schema: Schema) (r: ColumnRef) =
        let isMatch (c: OutputColumn) =
            match r.Qualifier with
            | Some q ->
                SqlName.equal c.Display r.Display
                || match c.Qualifier with
                   | Some cq -> SqlName.equal cq q && SqlName.equal c.Name r.Name
                   | None -> false
            | None -> SqlName.equal c.Name r.Name

        let rec collect (i: int) (acc: int list) =
            if i >= schema.Columns.Length then
                List.rev acc
            elif isMatch schema.Columns.[i] then
                collect (i + 1) (i :: acc)
            else
                collect (i + 1) acc

        let hits = collect 0 []

        match hits with
        | [ i ] -> ret i
        | [] ->
            err (
                sprintf
                    "列 '%s' が存在しません（使える列: %s）"
                    r.Display
                    (if schema.Columns.Length = 0 then
                         "(なし)"
                     else
                         schema.Columns |> Array.map (fun c -> c.Display) |> String.concat ", ")
                |> DbError.name
            )
        | many ->
            err (
                sprintf "列 '%s' が曖昧です（該当: %s）" r.Display (many |> List.map string |> String.concat ", ")
                |> DbError.name
            )
