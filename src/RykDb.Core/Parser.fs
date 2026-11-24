// ===========================================================================
// Parser — 文（DML / DDL / トランザクション）の構文規則と公開 API。
// ===========================================================================

namespace RykDb.Core

open ParserExpr

/// SQL 文を AST に変換するパーサ（文は再帰下降、式は `ParserExpr` の Pratt / CPS）。
/// 失敗はすべて Result<'T, DbError> として返し、例外は送らない。
module internal Parser =
    /// INSERT の値が単純リテラルで、直後に区切りがあるかを調べる。
    let private simpleInsertLiteral (st: PState) =
        let nextEnds =
            st.Pos + 1 < st.Tokens.Length
            && match st.Tokens.[st.Pos + 1].Token with
               | Token.Symbol ","
               | Token.Symbol ")" -> true
               | _ -> false

        if not nextEnds then
            None
        else
            match peek st with
            | LiteralExpr e -> Some(e, advance st)
            | _ -> None

    /// INSERT の 1行分の値を読む。
    [<TailCall>]
    let rec private insertOne (acc: Expr list) (st: PState) =
        let parsed =
            match simpleInsertLiteral st with
            | Some e -> ret e
            | None -> parseExprValue st

        match parsed with
        | Ok(e, st) ->
            if isSymbol "," st then
                advance st |> insertOne (e :: acc)
            else
                expectSymbol ")" st |> Result.map (fun (_, st) -> List.rev (e :: acc), st)
        | Error e -> err e

    /// INSERT の VALUES 行を読む。
    [<TailCall>]
    let rec private insertRows (acc: Expr list list) (st: PState) =
        match expectSymbol "(" st with
        | Ok(_, st) ->
            match insertOne [] st with
            | Ok(r, st) ->
                if isSymbol "," st then
                    advance st |> insertRows (r :: acc)
                else
                    ret (List.rev (r :: acc), st)
            | Error e -> err e
        | Error e -> err e

    let private parseInsert (st: PState) =
        result {
            let! _, st = expectKeyword "INSERT" st
            let! _, st = expectKeyword "INTO" st
            let! tbl, st = parseTableRef st
            let hasColumns = isSymbol "(" st

            let! columns, st =
                if hasColumns then
                    parseParenIdentList st |> Result.map (fun (cols, st) -> Some cols, st)
                else
                    ret (None, st)

            if isKeyword "DEFAULT" st then
                match columns with
                | Some _ -> return! fail "INSERT ... DEFAULT VALUES では列リストを指定できません" st
                | None ->
                    let st = advance st
                    let! _, st = expectKeyword "VALUES" st
                    let! _, st = eatOptionalSemicolon st

                    return
                        {
                            Table = tbl
                            Columns = None
                            Source = InsertSource.Values [ [] ]
                        },
                        st
            elif isKeyword "SELECT" st then
                let! sel, st = parseSelectStatementValue st
                let! _, st = eatOptionalSemicolon st

                return
                    {
                        Table = tbl
                        Columns = columns
                        Source = InsertSource.Select sel
                    },
                    st
            else
                let! _, st = expectKeyword "VALUES" st
                let! rs, st = insertRows [] st
                let! _, st = eatOptionalSemicolon st

                return
                    {
                        Table = tbl
                        Columns = columns
                        Source = InsertSource.Values rs
                    },
                    st
        }

    /// UPDATE の SET 代入リストを読む。
    [<TailCall>]
    let rec private updateAssigns (acc: (string * Expr) list) (st: PState) =
        match parseIdent st with
        | Ok(col, st) ->
            match expectSymbol "=" st with
            | Ok(_, st) ->
                match parseExprValue st with
                | Ok(e, st) ->
                    if isSymbol "," st then
                        advance st |> updateAssigns ((col, e) :: acc)
                    else
                        ret (List.rev ((col, e) :: acc), st)
                | Error e -> err e
            | Error e -> err e
        | Error e -> err e

    let private parseUpdate (st: PState) =
        result {
            let! _, st = expectKeyword "UPDATE" st
            let! tbl, st = parseTableRef st
            let! _, st = expectKeyword "SET" st
            let! asg, st = updateAssigns [] st
            let! whereE, st = parseWhereValue st
            let! _, st = eatOptionalSemicolon st

            return
                {
                    Table = tbl
                    Assignments = asg
                    Where = whereE
                },
                st
        }

    let private parseDelete (st: PState) =
        result {
            let! _, st = expectKeyword "DELETE" st
            let! _, st = expectKeyword "FROM" st
            let! tbl, st = parseTableRef st

            do!
                if isIdentName "USING" st then
                    fail "DELETE ... USING は未対応です" st
                else
                    ret ()

            let! whereE, st = parseWhereValue st
            let! _, st = eatOptionalSemicolon st
            return { Table = tbl; Where = whereE }, st
        }

    /// 型引数のカンマ区切り要素を読む。
    [<TailCall>]
    let rec private typeArgumentsLoop (acc: (int * bool) list) (st: PState) =
        match parseExprValue st with
        | Ok(e, st) ->
            let v =
                match e with
                | Expr.Literal(Literal.Int n) -> int n, true
                | _ -> 0, false

            if isSymbol "," st then
                advance st |> typeArgumentsLoop (v :: acc)
            else
                expectSymbol ")" st
                |> Result.map (fun (_, st) ->
                    v :: acc
                    |> List.rev
                    |> List.choose (fun (n, ok) -> if ok then Some("n", n) else None),
                    st)
        | Error e -> err e

    /// DECIMAL(10, 2) のような型引数を読む。
    let private parseTypeArguments (st: PState) =
        if not (isSymbol "(" st) then
            ret ([], st)
        else
            advance st |> typeArgumentsLoop []

    /// `REFERENCES 親 [(列 [, …])]` を読む。
    /// 参照アクション（ON DELETE / ON UPDATE）・MATCH・DEFERRABLE は未対応として明示エラーにする
    /// （黙って読み飛ばすと制約が無いものとして動いてしまう）。
    let private parseReferences (st: PState) =
        result {
            let! parent, st = parseIdent st

            let! columns, st =
                if isSymbol "(" st then
                    parseParenIdentList st |> Result.map (fun (cols, st) -> Some cols, st)
                else
                    ret (None, st)

            do!
                if isKeyword "ON" st then
                    fail "参照アクション（ON DELETE / ON UPDATE）は未対応です" st
                elif isIdentName "MATCH" st then
                    fail "MATCH の指定は未対応です" st
                elif isIdentName "DEFERRABLE" st then
                    fail "DEFERRABLE は未対応です" st
                else
                    ret ()

            return
                {
                    ParentTable = parent
                    ParentColumns = columns
                },
                st
        }

    /// `FOREIGN KEY`（テーブルレベルの外部キー）の開始位置か。
    /// `FOREIGN` は予約語ではないので、続く `KEY` まで見て判定する
    /// （`foreign` という名前の列を壊さない）。
    let private isForeignKeyStart (st: PState) =
        isIdentName "FOREIGN" st
        && st.Pos + 1 < st.Tokens.Length
        && match st.Tokens.[st.Pos + 1].Token with
           | Token.Keyword "KEY" -> true
           | _ -> false

    /// 列制約を 1つずつ畳み込む。
    [<TailCall>]
    let rec private columnConstraintLoop (c: ColumnConstraint) (st: PState) =
        if isKeyword "NOT" st then
            match advance st |> expectKeyword "NULL" with
            | Ok(_, st) -> columnConstraintLoop { c with NotNull = true } st
            | Error e -> err e
        elif isKeyword "PRIMARY" st then
            match advance st |> expectKeyword "KEY" with
            | Ok(_, st) ->
                columnConstraintLoop
                    { c with
                        PrimaryKey = true
                        NotNull = true
                    }
                    st
            | Error e -> err e
        elif isKeyword "UNIQUE" st then
            advance st |> columnConstraintLoop { c with Unique = true }
        elif isKeyword "DEFAULT" st then
            match advance st |> parseExprValue with
            | Ok(e, st) -> columnConstraintLoop { c with Default = Some e } st
            | Error e -> err e
        elif isIdentName "REFERENCES" st then
            match advance st |> parseReferences with
            | Ok(r, st) -> columnConstraintLoop { c with References = Some r } st
            | Error e -> err e
        else
            ret (c, st)

    let private parseColumnDefinition (st: PState) =
        result {
            let! name, st = parseIdent st

            let isConstraintOnly =
                match peek st with
                | Token.Keyword "NOT"
                | Token.Keyword "PRIMARY"
                | Token.Keyword "UNIQUE"
                | Token.Keyword "DEFAULT" -> true
                | Token.Ident s when s.ToUpperInvariant() = "REFERENCES" -> true
                | _ -> false

            let! dt, st =
                if isConstraintOnly then
                    ret (DataType.Text, st)
                else
                    parseDataType st

            let! args, st = parseTypeArguments st
            let! columnConstraint, st = columnConstraintLoop ColumnConstraint.Empty st

            return
                {
                    Name = name
                    DataType = dt
                    TypeArguments = args
                    Constraint = columnConstraint
                },
                st
        }

    /// `IF [NOT] EXISTS` を読む。
    /// 戻り値は `(IF 句があるか, NOT があるか)`。
    /// `CREATE ... IF NOT EXISTS` と `DROP ... IF EXISTS` を取り違えないよう、
    /// 綴りの検査は `ifFlag` で文種ごとに行う。
    let private parseIfExists (st: PState) =
        if isKeyword "IF" st then
            let st1 = advance st

            if isKeyword "NOT" st1 then
                expectKeyword "EXISTS" (advance st1)
                |> Result.map (fun (_, st) -> (true, true), st)
            else
                expectKeyword "EXISTS" st1 |> Result.map (fun (_, st) -> (true, false), st)
        else
            ret ((false, false), st)

    /// `IF` 句の綴りを文種ごとに検査し、AST 用のフラグを返す。
    /// `wantNot = true` は CREATE 用（`IF NOT EXISTS`）、false は DROP 用（`IF EXISTS`）。
    let private ifFlag (kind: string) (wantNot: bool) (hasIf: bool) (hasNot: bool) (st: PState) =
        if hasIf && hasNot <> wantNot then
            let expected = if wantNot then "IF NOT EXISTS" else "IF EXISTS"
            fail (sprintf "%s では %s を使ってください" kind expected) st
        else
            ret (hasIf, st)

    /// CREATE TABLE の列・テーブル制約を 1つずつ読む。
    [<TailCall>]
    let rec private createTableBody (cols: ColumnDefinition list) (constrs: TableConstraint list) (st: PState) =
        let next =
            if isKeyword "PRIMARY" st then
                match expectKeyword "PRIMARY" st with
                | Ok(_, st) ->
                    match expectKeyword "KEY" st with
                    | Ok(_, st) ->
                        parseParenIdentList st
                        |> Result.map (fun (pkCols, st) ->
                            cols, TableConstraint.PrimaryKeyColumns pkCols :: constrs, st)
                    | Error e -> err e
                | Error e -> err e
            elif isKeyword "UNIQUE" st then
                advance st
                |> parseParenIdentList
                |> Result.map (fun (uqCols, st) -> cols, TableConstraint.UniqueColumns uqCols :: constrs, st)
            elif isIdentName "CONSTRAINT" st then
                fail "名前付き制約（CONSTRAINT）は未対応です" st
            elif isForeignKeyStart st then
                let st1 = advance st

                match advance st1 |> parseParenIdentList with
                | Error e -> err e
                | Ok(fkCols, st2) ->
                    if not (isIdentName "REFERENCES" st2) then
                        fail "FOREIGN KEY の後ろには REFERENCES が必要です" st2
                    else
                        advance st2
                        |> parseReferences
                        |> Result.map (fun (r, st3) -> cols, TableConstraint.ForeignKey(fkCols, r) :: constrs, st3)
            else
                parseColumnDefinition st |> Result.map (fun (c, st) -> c :: cols, constrs, st)

        match next with
        | Ok(cols, constrs, st) ->
            if isSymbol "," st then
                advance st |> createTableBody cols constrs
            else
                expectSymbol ")" st
                |> Result.map (fun (_, st) -> List.rev cols, List.rev constrs, st)
        | Error e -> err e

    let private parseCreateTable (st: PState) =
        result {
            let! _, st = expectKeyword "CREATE" st
            let! _, st = expectKeyword "TABLE" st
            let! (hasIf, hasNot), st = parseIfExists st
            let! ifNotExists, st = ifFlag "CREATE TABLE" true hasIf hasNot st
            let! name, st = parseIdent st
            let! _, st = expectSymbol "(" st
            let! cols, constrs, st = createTableBody [] [] st
            let! _, st = eatOptionalSemicolon st

            return
                {
                    Name = name
                    IfNotExists = ifNotExists
                    Columns = cols
                    Constraints = constrs
                },
                st
        }

    let private parseCreateIndex (st: PState) =
        result {
            let! _, st = expectKeyword "CREATE" st
            let unique = isKeyword "UNIQUE" st
            let st = if unique then advance st else st
            let! _, st = expectKeyword "INDEX" st
            let! (hasIf, hasNot), st = parseIfExists st
            let! ifNotExists, st = ifFlag "CREATE INDEX" true hasIf hasNot st
            let! name, st = parseIdent st
            let! _, st = expectKeyword "ON" st
            let! tbl, st = parseIdent st
            let! cols, st = parseParenIdentList st
            let! _, st = eatOptionalSemicolon st

            return
                {
                    Name = name
                    Table = tbl
                    Columns = cols
                    Unique = unique
                    IfNotExists = ifNotExists
                },
                st
        }

    let private parseDropTable (st: PState) =
        result {
            let! _, st = expectKeyword "DROP" st
            let! _, st = expectKeyword "TABLE" st
            let! (hasIf, hasNot), st = parseIfExists st
            let! ifEx, st = ifFlag "DROP TABLE" false hasIf hasNot st
            let! name, st = parseIdent st
            let! _, st = eatOptionalSemicolon st
            return Statement.DropTable(name, ifEx), st
        }

    let private parseDropIndex (st: PState) =
        result {
            let! _, st = expectKeyword "DROP" st
            let! _, st = expectKeyword "INDEX" st
            let! (hasIf, hasNot), st = parseIfExists st
            let! ifEx, st = ifFlag "DROP INDEX" false hasIf hasNot st
            let! name, st = parseIdent st
            let! _, st = eatOptionalSemicolon st
            return Statement.DropIndex(name, ifEx), st
        }

    /// ALTER の COLUMN キーワードを読み飛ばす。
    let private optionalColumnKeyword (st: PState) =
        if isIdentName "COLUMN" st then advance st else st

    /// ALTER TABLE の操作を読む。
    let private alterTableAction (name: string) (st: PState) =
        if isIdentName "ADD" st then
            let st = advance st |> optionalColumnKeyword

            result {
                let! col, st = parseColumnDefinition st
                let! _, st = eatOptionalSemicolon st

                return
                    Statement.AlterTable
                        {
                            Table = name
                            Action = AlterTableAction.AddColumn col
                        },
                    st
            }
        elif isKeyword "DROP" st then
            let st = advance st |> optionalColumnKeyword

            result {
                let! column, st = parseIdent st
                let! _, st = eatOptionalSemicolon st

                return
                    Statement.AlterTable
                        {
                            Table = name
                            Action = AlterTableAction.DropColumn column
                        },
                    st
            }
        elif isIdentName "RENAME" st then
            let st = advance st

            if isIdentName "TO" st then
                fail "ALTER TABLE ... RENAME TO（テーブル名の変更）は未対応です" st
            else
                let st = if isIdentName "COLUMN" st then advance st else st

                result {
                    let! oldName, st = parseIdent st

                    do!
                        if isIdentName "TO" st then
                            ret ()
                        else
                            fail "RENAME COLUMN の後ろには TO が必要です" st

                    let! newName, st = parseIdent (advance st)
                    let! _, st = eatOptionalSemicolon st

                    return
                        Statement.AlterTable
                            {
                                Table = name
                                Action = AlterTableAction.RenameColumn(oldName, newName)
                            },
                        st
                }
        else
            fail "ALTER TABLE では ADD COLUMN / DROP COLUMN / RENAME COLUMN を指定してください" st

    /// `ALTER TABLE`。
    /// <remarks>
    /// `ALTER` / `ADD` / `COLUMN` / `RENAME` / `TO` は予約語にしない
    /// （`EXPLAIN` と同じく、識別子として使える語を増やさないため）。
    /// 未対応の操作（型変更・テーブル名の変更など）は黙って無視せず SyntaxError にする。
    /// </remarks>
    let private parseAlterTable (st: PState) =
        result {
            let! _, st = expectKeyword "TABLE" st
            let! name, st = parseIdent st
            return! alterTableAction name st
        }

    /// 現在位置が `SAMPLE` で、直後が数値（`SAMPLE n` の n）か。
    /// `SAMPLE` は予約語にしないので、表名と区別するために 1 トークン先を見る。
    let private sampleClauseAhead (st: PState) =
        isIdentName "SAMPLE" st
        && st.Pos + 1 < st.Tokens.Length
        && match st.Tokens.[st.Pos + 1].Token with
           | Token.IntLit _
           | Token.RealLit _
           | Token.Symbol "-" -> true
           | _ -> false

    /// `ANALYZE [TABLE] テーブル名` / `ANALYZE [TABLE] SAMPLE n`。
    /// `ANALYZE` / `TABLE` は予約語にしない（`EXPLAIN` / `ALTER` と同じ）。
    let private parseSample (st: PState) =
        if isIdentName "SAMPLE" st then
            let st = advance st

            result {
                match peek st with
                | Token.IntLit n when n > 0L && n <= int64 System.Int32.MaxValue ->
                    let st = advance st
                    return Some(int n), st
                | _ -> return! fail "SAMPLE には正の整数が必要です" st
            }
        else
            ret (None, st)

    let private parseAnalyze (st: PState) =
        let st =
            match peek st with
            | Token.Keyword "TABLE" -> advance st
            | _ -> st

        if sampleClauseAhead st then
            result {
                let! sample, st = parseSample st
                let! _, st = eatOptionalSemicolon st
                return Statement.Analyze(None, sample), st
            }
        else
            match peek st with
            | Token.Ident name
            | Token.QuotedIdent name ->
                let st = advance st

                result {
                    let! sample, st = parseSample st
                    let! _, st = eatOptionalSemicolon st
                    return Statement.Analyze(Some name, sample), st
                }
            | Token.Eos
            | Token.Symbol ";" ->
                eatOptionalSemicolon st
                |> Result.map (fun (_, st) -> Statement.Analyze(None, None), st)
            | _ -> fail "ANALYZE にはテーブル名が必要です" st

    let private parseOne (st: PState) =
        let nextToken =
            if st.Pos + 1 < st.Tokens.Length then
                st.Tokens.[st.Pos + 1].Token
            else
                Token.Eos

        match peek st with
        | Token.Keyword "SELECT" -> parseSelect st
        | Token.Keyword "INSERT" -> parseInsert st |> Result.map (fun (x, st) -> Statement.Insert x, st)
        | Token.Keyword "UPDATE" -> parseUpdate st |> Result.map (fun (x, st) -> Statement.Update x, st)
        | Token.Keyword "DELETE" -> parseDelete st |> Result.map (fun (x, st) -> Statement.Delete x, st)
        | Token.Keyword "CREATE" ->
            if nextToken = Token.Keyword "TABLE" then
                parseCreateTable st |> Result.map (fun (x, st) -> Statement.CreateTable x, st)
            elif nextToken = Token.Keyword "UNIQUE" || nextToken = Token.Keyword "INDEX" then
                parseCreateIndex st |> Result.map (fun (x, st) -> Statement.CreateIndex x, st)
            else
                fail "CREATE TABLE または CREATE INDEX を指定してください" st
        | Token.Keyword "DROP" ->
            if nextToken = Token.Keyword "TABLE" then
                parseDropTable st
            elif nextToken = Token.Keyword "INDEX" then
                parseDropIndex st
            else
                fail "DROP TABLE または DROP INDEX を指定してください" st
        | Token.Keyword "BEGIN" ->
            advance st
            |> eatIdentName "TRANSACTION"
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Begin, st)
        | Token.Ident name when name.ToUpperInvariant() = "EXPLAIN" ->
            let st = advance st

            let st, analyze =
                if isIdentName "ANALYZE" st then
                    advance st, true
                else
                    st, false

            match peek st with
            | Token.Keyword "SELECT" ->
                result {
                    let! sel, st = parseSelectStatementValue st
                    let! _, st = eatOptionalSemicolon st

                    return
                        (if analyze then
                             Statement.ExplainAnalyze sel
                         else
                             Statement.Explain sel),
                        st
                }
            | _ -> fail "EXPLAIN は SELECT 文だけに対応しています" st
        | Token.Ident name when name.ToUpperInvariant() = "ALTER" -> advance st |> parseAlterTable
        | Token.Ident name when name.ToUpperInvariant() = "ANALYZE" -> advance st |> parseAnalyze
        | Token.Ident name when name.ToUpperInvariant() = "START" ->
            let st = advance st

            if isIdentName "TRANSACTION" st then
                st
                |> advance
                |> eatOptionalSemicolon
                |> Result.map (fun (_, st) -> Statement.Begin, st)
            else
                fail "START TRANSACTION が必要です" st
        | Token.Keyword "COMMIT" ->
            advance st
            |> eatIdentName "WORK"
            |> eatIdentName "TRANSACTION"
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Commit, st)
        | Token.Keyword "END" ->
            advance st
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Commit, st)
        | Token.Keyword "ROLLBACK" ->
            advance st
            |> eatIdentName "WORK"
            |> eatIdentName "TRANSACTION"
            |> eatOptionalSemicolon
            |> Result.map (fun (_, st) -> Statement.Rollback, st)
        | Token.Ident name when Set.contains (name.ToUpperInvariant()) SetOperationWords ->
            fail "集合演算は UNION ALL のみ対応しています" st
        | _ -> fail "文の開始として解釈できません" st

    /// `Token.Param` の個数。
    let private parameterCount =
        Array.sumBy (fun t ->
            match t.Token with
            | Token.Param _ -> 1
            | _ -> 0)

    /// 最初の `Token.Param` の位置。個数不一致のエラー位置に使う。
    let private firstParameterPosition =
        Array.tryPick (fun t ->
            match t.Token with
            | Token.Param _ -> Some t.Pos
            | _ -> None)
        >> Option.defaultValue { Offset = 0; Line = 1; Column = 1 }

    /// トークン列を 1文以上へ分解し、`?` を `parameters` の順に値へ置き換える。
    /// トークン列を文へ分解し、区切りを検査する。
    [<TailCall>]
    let rec private parseAllStatements (acc: Statement list) (st: PState) =
        if st.Pos >= st.Tokens.Length - 1 then
            ret (List.rev acc)
        else
            match parseOne st with
            | Ok(statement, next) ->
                let separated =
                    next.Pos > st.Pos && st.Tokens.[next.Pos - 1].Token = Token.Symbol ";"

                if next.Pos < st.Tokens.Length - 1 && not separated then
                    fail "文の区切りには ; が必要です（複数文は ; で区切ってください）" next
                else
                    parseAllStatements (statement :: acc) next
            | Error e -> err e

    let private parseTokens (parameters: SqlValue array) (tokens: TokenItem array) =
        let expected = parameterCount tokens

        if expected <> parameters.Length then
            let message = sprintf "パラメータの個数が一致しません（SQL: %d個、指定: %d個）" expected parameters.Length

            { DbError.typeError message with
                Position = Some(firstParameterPosition tokens)
            }
            |> err
        else
            parseAllStatements
                []
                {
                    Tokens = tokens
                    Pos = 0
                    Parameters = parameters
                }

    /// SQL テキストを 1文以上へ分解して AST にする。
    /// `?` は出現順に `parameters` の値へ置き換える。
    let parseStatementsWith (parameters: SqlValue array) (sql: string) =
        Lexer.tokenize sql
        >>= fun tokens -> parseTokens parameters (Array.ofList tokens)

    /// SQL テキストを 1文以上へ分解して AST にする（パラメータなし）。
    let parseStatements (sql: string) = parseStatementsWith [||] sql

    /// SQL テキストを 1つの式として解析する。
    /// DEFAULT の永続化文字列を復元するために使う。
    let parseExpression (sql: string) =
        result {
            let! tokens = Lexer.tokenize sql

            let st =
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                    Parameters = [||]
                }

            let! e, st = parseExprValue st

            if st.Pos >= st.Tokens.Length - 1 then
                return e
            else
                return! err (DbError.create ErrorKind.SyntaxError "式の後に余分なトークンがあります")
        }
