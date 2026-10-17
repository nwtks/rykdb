namespace RykDb.Core

// 書き込みは Transaction にバッファされ、commit 時にだけ RowStore へ届く。
// よって「同じ文の途中で読んだ値」と「書く値」が食い違わない。
module internal DmlOps =
    /// WHERE を評価するときの対象スキーマ（UPDATE / DELETE で共通）。
    let private targetSchemaOf (schema: TableSchema) = Schema.ofTable schema.Name None schema

    /// WHERE の述語。
    /// WHERE が無ければ「全部通過」（UPDATE / DELETE で共通）。
    let private matchesWhere (targetSchema: Schema) (where: Expr option) (row: SqlValue array) =
        match where with
        | Some w -> Eval.matches targetSchema row w
        | None -> ret true

    /// 値式を列位置に対応付け、型強制して (位置, 値) のリストにする。
    /// INSERT は行の外（定数）で、UPDATE は現在行を参照して評価するので、
    /// 評価器 evalOne を受け取って両方を同じ経路で処理する。
    let private gatherWith
        (evalOne: Expr -> Result<SqlValue, DbError>)
        (schema: TableSchema)
        (values: Expr list)
        (idxs: int list)
        =
        let rec gather (vs: Expr list) (is_: int list) (acc: (int * SqlValue) list) =
            match vs, is_ with
            | [], [] -> ret acc
            | v :: rest, i :: restIdx ->
                match evalOne v with
                | Error e -> Error e
                | Ok value ->
                    let colDef = schema.Columns.[i]

                    match SqlValue.coerce colDef.DataType value with
                    | Error e -> Error e
                    | Ok coerced -> gather rest restIdx ((i, coerced) :: acc)
            | _ -> ret acc

        gather values idxs []

    /// 表を全走査して 1行ずつ処理する UPDATE / DELETE 共通の土台。
    /// matched が偽の行は件数を変えず先へ進み、 onRow が「(トランザクション, 件数)」を進めた結果を返す。
    let private foldRows
        (txn: Transaction)
        (ctx: ExecContext)
        (schema: TableSchema)
        (matched: SqlValue array -> Result<bool, DbError>)
        (onRow: Transaction -> int -> SqlValue array -> Result<Transaction * int, DbError>)
        =
        Dml.scanByPrimaryKey ctx.Store schema ctx.Visible 0
        >>= foldOk (fun (t, c) row -> matched row >>= fun keep -> if keep then onRow t c row else ret (t, c)) (txn, 0)

    /// gather で埋まった `(列位置, 値)` の対を、列順の行へ戻す。
    /// 埋まっていない位置は fallback に委ねる
    /// （INSERT は DEFAULT / NULL、UPDATE は元の値）。
    let private scatter (width: int) (filled: (int * SqlValue) list) (fallback: int -> SqlValue) =
        Array.init width (fun i ->
            match filled |> List.tryFind (fun (j, _) -> j = i) with
            | Some(_, v) -> v
            | None -> fallback i)

    /// INSERT。
    /// columns が None ならテーブル定義の順に並べる。
    /// DEFAULT / NULL の充足と型強制はここで行う。
    /// TableSchema.validateRow が NOT NULL と型を最終確認する。
    let insert (schema: TableSchema) (txn: Transaction) (columns: string list option) (rows: Expr list list) =
        let evalConstant (e: Expr) = Eval.eval Schema.empty [||] e

        let buildRow idxs width (values: Expr list) =
            if List.length values <> List.length idxs then
                Error(
                    DbError.create
                        ErrorKind.SyntaxError
                        (sprintf "INSERT の列数(%d)と値の数(%d)が合いません" (List.length idxs) (List.length values))
                )
            else
                gatherWith evalConstant schema values idxs
                |> Result.map (fun filled ->
                    scatter width filled (fun i ->
                        match schema.Columns.[i].Constraint.Default with
                        | Some d ->
                            match evalConstant d with
                            | Ok dv -> dv
                            | Error _ -> SqlValue.Null
                        | None -> SqlValue.Null))

        match columns with
        | None -> schema.Columns |> List.mapi (fun i _ -> i) |> ret
        | Some names -> names |> List.map (fun n -> schema.RequireIndexOf n) |> all
        >>= fun idxs ->
            rows
            |> foldOk
                (fun (txnAcc, count) values ->
                    result {
                        let! row = buildRow idxs schema.Columns.Length values
                        let! t = Dml.insert schema txnAcc row
                        return t, count + 1
                    })
                (txn, 0)

    /// UPDATE。
    /// assignments の右辺は現在行で評価する。
    /// 検索キーは更新前の行の主キー。
    /// 主キー列を書き換えると古い位置の行が残って重複する。
    let update
        (ctx: ExecContext)
        (schema: TableSchema)
        (txn: Transaction)
        (assignments: (string * Expr) list)
        (where: Expr option)
        =
        let assign (targetSchema: Schema) (targets: int list) (row: SqlValue array) =
            gatherWith (Eval.eval targetSchema row) schema (List.map snd assignments) targets
            |> Result.map (fun filled -> scatter row.Length filled (fun i -> row.[i]))

        let targetSchema = targetSchemaOf schema

        assignments
        |> List.map (fun (name, _) -> schema.RequireIndexOf name)
        |> all
        >>= fun targets ->
            foldRows txn ctx schema (matchesWhere targetSchema where) (fun txnAcc count row ->
                result {
                    let! newRow = assign targetSchema targets row
                    let pk = TableSchema.primaryKeyOf schema row

                    let! t = Dml.updateByPrimaryKey ctx.Store schema ctx.Visible txnAcc pk newRow
                    return t, count + 1
                })

    /// DELETE。
    /// WHERE に合う行を主キー + 全索引に tombstone で消す。
    let delete (ctx: ExecContext) (schema: TableSchema) (txn: Transaction) (where: Expr option) =
        let targetSchema = targetSchemaOf schema

        foldRows txn ctx schema (matchesWhere targetSchema where) (fun t count row ->
            let pk = TableSchema.primaryKeyOf schema row

            Dml.deleteByPrimaryKey ctx.Store schema ctx.Visible t pk
            |> Result.map (fun t2 -> t2, count + 1))
