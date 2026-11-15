// ===========================================================================
// Parser — 文（DML / DDL / トランザクション）の構文規則と公開 API。
// 字句の読み進め・式・SELECT の構文規則は `ParserExpr.fs`。
// ===========================================================================

namespace RykDb.Core

open ParserExpr

/// SQL 文を AST に変換するパーサ（文は再帰下降、式は `ParserExpr` の Pratt / CPS）。
/// 失敗はすべて Result<'T, DbError> として返し、例外は送らない。
module internal Parser =
    let private parseInsert (st: PState) =
        let simpleLiteral st =
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
                | Token.IntLit v -> Some(Expr.Literal(Literal.Int v), advance st)
                | Token.RealLit v -> Some(Expr.Literal(Literal.Real v), advance st)
                | Token.StringLit v -> Some(Expr.Literal(Literal.String v), advance st)
                | Token.BlobLit v -> Some(Expr.Literal(Literal.Blob v), advance st)
                | Token.Keyword "NULL" -> Some(Expr.Literal Literal.Null, advance st)
                | Token.Keyword "TRUE" -> Some(Expr.Literal(Literal.Bool true), advance st)
                | Token.Keyword "FALSE" -> Some(Expr.Literal(Literal.Bool false), advance st)
                | Token.Keyword "DEFAULT" -> Some(Expr.Default, advance st)
                | _ -> None

        let rec one acc st =
            let parsed =
                match simpleLiteral st with
                | Some e -> Ok e
                | None -> parseExprValue st

            match parsed with
            | Error e -> err e
            | Ok(e, st) ->
                if isSymbol "," st then
                    advance st |> one (e :: acc)
                else
                    expectSymbol ")" st |> Result.map (fun (_, st) -> List.rev (e :: acc), st)

        let rec rows acc st =
            match expectSymbol "(" st with
            | Error e -> err e
            | Ok(_, st) ->
                match one [] st with
                | Error e -> err e
                | Ok(r, st) ->
                    if isSymbol "," st then
                        advance st |> rows (r :: acc)
                    else
                        ret (List.rev (r :: acc), st)

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
                let! rs, st = rows [] st
                let! _, st = eatOptionalSemicolon st

                return
                    {
                        Table = tbl
                        Columns = columns
                        Source = InsertSource.Values rs
                    },
                    st
        }

    let private parseUpdate (st: PState) =
        let rec assigns acc st =
            match parseIdent st with
            | Error e -> err e
            | Ok(col, st) ->
                match expectSymbol "=" st with
                | Error e -> err e
                | Ok(_, st) ->
                    match parseExprValue st with
                    | Error e -> err e
                    | Ok(e, st) ->
                        if isSymbol "," st then
                            advance st |> assigns ((col, e) :: acc)
                        else
                            ret (List.rev ((col, e) :: acc), st)

        result {
            let! _, st = expectKeyword "UPDATE" st
            let! tbl, st = parseTableRef st
            let! _, st = expectKeyword "SET" st
            let! asg, st = assigns [] st
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

    /// VARCHAR(32) のような型引数を読む。
    let private parseTypeArguments (st: PState) =
        let rec loop acc st =
            match parseExprValue st with
            | Error e -> err e
            | Ok(e, st) ->
                let v =
                    match e with
                    | Expr.Literal(Literal.Int n) -> int n, true
                    | _ -> 0, false

                if isSymbol "," st then
                    advance st |> loop (v :: acc)
                else
                    expectSymbol ")" st
                    |> Result.map (fun (_, st) ->
                        v :: acc
                        |> List.rev
                        |> List.choose (fun (n, ok) -> if ok then Some("n", n) else None),
                        st)

        if not (isSymbol "(" st) then
            ret ([], st)
        else
            advance st |> loop []

    let private parseColumnConstraint (acc: ColumnConstraint) (st: PState) =
        let rec loop c st =
            if isKeyword "NOT" st then
                match advance st |> expectKeyword "NULL" with
                | Error e -> err e
                | Ok(_, st) -> loop { c with NotNull = true } st
            elif isKeyword "PRIMARY" st then
                match advance st |> expectKeyword "KEY" with
                | Error e -> err e
                | Ok(_, st) ->
                    loop
                        { c with
                            PrimaryKey = true
                            NotNull = true
                        }
                        st
            elif isKeyword "UNIQUE" st then
                advance st |> loop { c with Unique = true }
            elif isKeyword "DEFAULT" st then
                match advance st |> parseExprValue with
                | Error e -> err e
                | Ok(e, st) -> loop { c with Default = Some e } st
            else
                ret (c, st)

        loop acc st

    let private parseColumnDefinition (st: PState) =
        result {
            let! name, st = parseIdent st

            let isConstraintOnly =
                match peek st with
                | Token.Keyword "NOT"
                | Token.Keyword "PRIMARY"
                | Token.Keyword "UNIQUE"
                | Token.Keyword "DEFAULT" -> true
                | _ -> false

            let! dt, st =
                if isConstraintOnly then
                    ret (DataType.Text, st)
                else
                    parseDataType st

            let! args, st = parseTypeArguments st
            let! columnConstraint, st = parseColumnConstraint ColumnConstraint.Empty st

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
    /// `NOT` は省略形（`IF EXISTS`）にも許可される SQL 実装があるため、あれば消費する。
    /// 消費しないと `CREATE TABLE IF NOT EXISTS` が `EXISTS が必要` で落ちる。
    let private parseIfExists (st: PState) =
        if isKeyword "IF" st then
            let st1 = advance st
            let st2 = if isKeyword "NOT" st1 then advance st1 else st1
            expectKeyword "EXISTS" st2 |> Result.map (fun (_, st) -> true, st)
        else
            ret (false, st)

    let private parseCreateTable (st: PState) =
        let rec body cols constrs st =
            let next =
                if isKeyword "PRIMARY" st then
                    match expectKeyword "PRIMARY" st with
                    | Error e -> err e
                    | Ok(_, st) ->
                        match expectKeyword "KEY" st with
                        | Error e -> err e
                        | Ok(_, st) ->
                            parseParenIdentList st
                            |> Result.map (fun (pkCols, st) ->
                                cols, TableConstraint.PrimaryKeyColumns pkCols :: constrs, st)
                elif isKeyword "UNIQUE" st then
                    advance st
                    |> parseParenIdentList
                    |> Result.map (fun (uqCols, st) -> cols, TableConstraint.UniqueColumns uqCols :: constrs, st)
                else
                    parseColumnDefinition st |> Result.map (fun (c, st) -> c :: cols, constrs, st)

            match next with
            | Error e -> err e
            | Ok(cols, constrs, st) ->
                if isSymbol "," st then
                    advance st |> body cols constrs
                else
                    expectSymbol ")" st
                    |> Result.map (fun (_, st) -> List.rev cols, List.rev constrs, st)

        result {
            let! _, st = expectKeyword "CREATE" st
            let! _, st = expectKeyword "TABLE" st
            let! ifNotExists, st = parseIfExists st
            let! name, st = parseIdent st
            let! _, st = expectSymbol "(" st
            let! cols, constrs, st = body [] [] st
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
            let! ifNotExists, st = parseIfExists st
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
            let! ifEx, st = parseIfExists st
            let! name, st = parseIdent st
            let! _, st = eatOptionalSemicolon st
            return Statement.DropTable(name, ifEx), st
        }

    let private parseDropIndex (st: PState) =
        result {
            let! _, st = expectKeyword "DROP" st
            let! _, st = expectKeyword "INDEX" st
            let! ifEx, st = parseIfExists st
            let! name, st = parseIdent st
            let! _, st = eatOptionalSemicolon st
            return Statement.DropIndex(name, ifEx), st
        }

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

            match peek st with
            | Token.Keyword "SELECT" ->
                result {
                    let! sel, st = parseSelectStatementValue st
                    let! _, st = eatOptionalSemicolon st
                    return Statement.Explain sel, st
                }
            | _ -> fail "EXPLAIN は SELECT 文だけに対応しています" st
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
            fail "集合演算（UNION / INTERSECT / EXCEPT）は未対応です" st
        | _ -> fail "文の開始として解釈できません" st

    /// SQL テキストを 1文以上へ分解して AST にする。
    let parseStatements (sql: string) =
        let rec parseAll acc st =
            if st.Pos >= st.Tokens.Length - 1 then
                ret (List.rev acc)
            else
                match parseOne st with
                | Error e -> err e
                | Ok(statement, next) ->
                    let separated =
                        next.Pos > st.Pos && st.Tokens.[next.Pos - 1].Token = Token.Symbol ";"

                    if next.Pos < st.Tokens.Length - 1 && not separated then
                        fail "文の区切りには ; が必要です（複数文は ; で区切ってください）" next
                    else
                        parseAll (statement :: acc) next

        Lexer.tokenize sql
        >>= fun tokens ->
            parseAll
                []
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                }

    /// SQL テキストを 1つの式として解析する。
    /// DEFAULT の永続化文字列を復元するために使う。
    let parseExpression (sql: string) =
        result {
            let! tokens = Lexer.tokenize sql

            let st =
                {
                    Tokens = Array.ofList tokens
                    Pos = 0
                }

            let! e, st = parseExprValue st

            if st.Pos >= st.Tokens.Length - 1 then
                return e
            else
                return! err (DbError.create ErrorKind.SyntaxError "式の後に余分なトークンがあります")
        }
