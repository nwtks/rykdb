// ===========================================================================
// Schema — 出力スキーマと射影。
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
    let Empty = { Columns = [||]; Tables = [] }

    /// 横連結（JOIN 用）。
    let concat (schema: Schema) (other: Schema) =
        if Array.isEmpty schema.Columns then
            if List.isEmpty schema.Tables then
                other
            else
                { other with
                    Tables = schema.Tables @ other.Tables
                }
        elif Array.isEmpty other.Columns then
            { schema with
                Tables = schema.Tables @ other.Tables
            }
        else
            {
                Columns = Array.append schema.Columns other.Columns
                Tables = schema.Tables @ other.Tables
            }

    /// スキーマ列を左から連結する（FROM / JOIN の出力スキーマ組み立て）。
    let concatAll = List.fold concat Empty

    /// `*` / `t.*` が表す出力列を展開する。
    /// どちらもなければ None。
    /// <remarks>
    /// ヘッダ行（projectColumns）と射影の式（Ops.project）が同じ列を共有する。
    /// どちらかが先に変わると `SELECT *` の列数とヘッダがずれる（実際にずれた）。
    /// Expr は `[<NoComparison>]` なので「どちらの star だったか」は別のフラグで返す
    /// （qualified = `t.*` なら true）。
    /// </remarks>
    let expandStar (e: Expr) (schema: Schema) =
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

    /// 指定した修飾子（テーブル名・別名）の列が 1つ以上あるか。
    /// 存在しない `t.*` を 0列の成功にしないための検証に使う。
    let hasQualifier (qualifier: string) (schema: Schema) =
        schema.Columns
        |> Array.exists (fun c ->
            match c.Qualifier with
            | Some q -> SqlName.equal q qualifier
            | None -> false)

    /// 式を「列の直接参照」に読み替える。
    /// Column はそのまま。修飾子なしの裸の名前（Name）も列として扱う
    /// （パーサが WHERE / ORDER BY の裸の名前を Name にするため）。
    /// 集約呼び出しや式は列ではないので None。
    let tryAsColumnRef =
        function
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

    /// 出力列が列参照に一致するか。
    let private columnMatchesRef (r: ColumnRef) (c: OutputColumn) =
        match r.Qualifier with
        | Some q ->
            SqlName.equal c.Display r.Display
            || match c.Qualifier with
               | Some cq -> SqlName.equal cq q && SqlName.equal c.Name r.Name
               | None -> false
        | None -> SqlName.equal c.Name r.Name

    /// 一致する列位置を昇順に集める。
    [<TailCall>]
    let rec private resolveCollect (r: ColumnRef) (schema: Schema) (i: int) (acc: int list) =
        if i >= schema.Columns.Length then
            List.rev acc
        elif columnMatchesRef r schema.Columns.[i] then
            resolveCollect r schema (i + 1) (i :: acc)
        else
            resolveCollect r schema (i + 1) acc

    /// 列参照を解決する。見つからなければエラー、曖昧ならエラー。
    /// 大文字小文字を区別しない（`SqlName.equal` と同じ規則）。
    /// 行ごとに呼ばれるので、文字列の小文字化や一時配列を作らない。
    let resolve (r: ColumnRef) (schema: Schema) =
        let hits = resolveCollect r schema 0 []

        match hits with
        | [ i ] -> ret i
        | [] ->
            sprintf
                "列 '%s' が存在しません（使える列: %s）"
                r.Display
                (if schema.Columns.Length = 0 then
                     "(なし)"
                 else
                     schema.Columns |> Array.map (fun c -> c.Display) |> String.concat ", ")
            |> DbError.name
            |> err
        | many ->
            sprintf "列 '%s' が曖昧です（該当: %s）" r.Display (many |> List.map string |> String.concat ", ")
            |> DbError.name
            |> err

/// SELECT の射影（出力式とヘッダ）を 1回の展開でまとめたもの。
/// `Ops.project`（行の値）と `Plan` のヘッダが同じ展開用スキーマ（論理 = FROM 順）を共有し、
/// 「ヘッダの列数と行の幅がずれる」不変条件違反を防ぐ。
/// 行の評価は実レイアウトで行うため、`Ops.project` は展開用と評価用のスキーマを別に受け取る。
type internal SelectProjection =
    {
        SelectItems: SelectItem list
        /// `*` / `t.*` を展開した後の出力式（出力列と同じ順・同じ数）。
        Expressions: Expr list
        /// 結果表示用のヘッダ（`SELECT id` → `id`）。
        DisplayHeader: OutputColumn array
        /// 参照用のヘッダ（`SELECT t.id` → `id`）。派生テーブル用。
        ReferenceHeader: OutputColumn array
    }

module internal SelectProjection =
    let private expressionsOf (schema: Schema) =
        List.collect (fun item ->
            match Schema.expandStar item.Expression schema with
            | Some(_, columns) ->
                columns
                |> List.map (fun c ->
                    Expr.Column
                        {
                            Qualifier = c.Qualifier
                            Name = c.Name
                        })
            | None -> [ item.Expression ])

    /// 式が直接参照する出力列を返す。
    let private referencedColumn (schema: Schema) (e: Expr) =
        match Schema.tryAsColumnRef e with
        | Some c ->
            match Schema.resolve c schema with
            | Ok i -> Some schema.Columns.[i]
            | Error _ -> None
        | None -> None

    /// 射影後の列 1つを作る（型は直接参照からだけ引き継ぐ）。
    let private projectedColumn (schema: Schema) (name: string) (e: Expr) =
        {
            Name = name
            Qualifier = None
            DataType = referencedColumn schema e |> Option.bind (fun c -> c.DataType)
        }

    /// 射影後の列（名前・修飾子・型）を作る。
    /// `SELECT *` は入力の列名をそのまま、`t.*` は修飾つきの表示名にする（元の挙動）。
    /// `SELECT id` は式を表示し、別名があればそれを使う。
    /// <remarks>
    /// 入力スキーマのヘッダをそのまま返すと `SELECT id` が `people.id` になるので、
    /// 射影後の名前を組み立て直す必要がある。
    /// 型は「直接の列参照」だけ引き継ぐ（式の型推論はしない）。
    /// 派生テーブル（FROM のサブクエリ）の列型が None だとハッシュ結合のキーにできないため、
    /// `SELECT id, v AS x FROM t` や `SELECT *` のような形では型を残す。
    /// </remarks>
    let private projectedColumns (useDisplayName: bool) (schema: Schema) =
        function
        | [] -> schema.Columns
        | items ->
            items
            |> List.collect (fun item ->
                match Schema.expandStar item.Expression schema with
                | Some(qualified, columns) ->
                    columns
                    |> List.map (fun c ->
                        {
                            Name = if qualified then c.Display else c.Name
                            Qualifier = None
                            DataType = c.DataType
                        })
                | None ->
                    match item.Alias with
                    | Some a -> [ projectedColumn schema a item.Expression ]
                    | None ->
                        match
                            if useDisplayName then
                                None
                            else
                                referencedColumn schema item.Expression
                        with
                        | Some c ->
                            [
                                {
                                    Name = c.Name
                                    Qualifier = None
                                    DataType = c.DataType
                                }
                            ]
                        | None ->
                            [
                                projectedColumn schema (Expr.toDisplayString item.Expression) item.Expression
                            ])
            |> List.toArray

    /// 射影を組み立てる。
    /// 空の `items` は「入力スキーマをそのまま出力」を表す。
    let create (schema: Schema) (items: SelectItem list) =
        {
            SelectItems = items
            Expressions = expressionsOf schema items
            DisplayHeader = projectedColumns true schema items
            ReferenceHeader = projectedColumns false schema items
        }
