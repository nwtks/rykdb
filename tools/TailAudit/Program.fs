// ===========================================================================
// TailAudit — 「末尾位置でない再帰」の反復監査
//
//   dotnet run --project tools/TailAudit               検証（新規の非末尾が 0 件なら exit 0）
//   dotnet run --project tools/TailAudit -- --all      検出全件（既知分も表示）
//   dotnet run --project tools/TailAudit -- --update   現状をベースラインへ書き出し（内容確認後に）
//   dotnet run --project tools/TailAudit -- <path>     ベースラインを差し替えて実行
//
// tail call 検証は 3 層で行う（本プログラムは 2 層目）:
//   1) `dotnet build -t:Rebuild` — [<TailCall>] 付き関数の非末尾を FS3569 で検出（警告 = エラー）
//   2) 本プログラム — 属性の無い関数（ローカル let rec 含む）を FCS の実 AST で検出。
//      ローカル let rec には属性を付けられない（FS0010）ためコンパイラでは見られない。
//      「再帰呼び出しが末尾位置か」を判定し、ラムダ内・入れ子関数の末尾は
//      保守的に非末尾と数えて、ベースライン（tools/TailAudit/TailAudit.baseline）で既知分を吸収する。
//      pipe の関数側は末尾を継承する（`x |> f a` = `f a x` → 右が関数側、
//      `f a <| x` = `(f a) x` → 左が関数側）。値側（先に評価される側）は非末尾のまま。
//      非末尾は「ラムダ内 / 直接」で内訳表示する（直接 = 監査部・引数 = 末尾化候補）。
//   3) `dotnet test` — 深さが積もる事象を実測（`末尾再帰:` 系テスト・スタック深度）
//
// FCS は NuGet ではなく SDK 同梱品を参照する
// （HintPath = `$(MSBuildBinPath)/FSharp/FSharp.Compiler.Service.dll`）。
// インストール場所・SDK バージョンに依存せず、ビルド中の SDK と必ず一致する。
// ===========================================================================

open System
open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics
open Microsoft.FSharp.Reflection

let checker = FSharpChecker.Create()

let mkOpts (path: string) =
    {
        SourceFiles = [| path |]
        ApplyLineDirectives = false
        ConditionalDefines = []
        DiagnosticOptions =
            {
                WarnLevel = 3
                GlobalWarnAsError = false
                WarnOff = []
                WarnOn = []
                WarnAsError = []
                WarnAsWarn = []
                WarnScopeData = None
            }
        LangVersionText = "default"
        IsInteractive = false
        StrictIndentation = None
        CompilingFSharpCore = false
        IsExe = true
    }

// ---------------------------------------------------------------------------
// 状態（fsx トップレベルの mutable はクロージャから見える）
// ---------------------------------------------------------------------------
type Hit =
    {
        File: string
        GroupLine: int
        GroupNames: string
        GroupAttr: bool
        Fn: string
        Line: int
        Col: int
        Tail: bool
        Bare: bool
        /// ラムダ本体の中（Compute/pull 継続・CPS 継続）にある呼び出しか
        InLambda: bool
    }

type Group =
    {
        File: string
        Names: string list
        Bindings: SynBinding list
        Line: int
        Attr: bool
    }

let hits = ResizeArray<Hit>()
let groups = ResizeArray<Group>()
let seenGroups = Collections.Generic.HashSet<string * int * string>()
let seenHits = Collections.Generic.HashSet<string>()
let unhandled = ResizeArray<string>()

let mutable curFile = ""
let mutable collectPhase = true
let mutable curGroupLine = 0
let mutable curGroupNames = ""
let mutable curGroupAttr = false
/// 入れ子ラムダの深さ（0 = ラムダの外）。
/// walk のシグネチャを変えずに「ラムダ内か」を hit へ運ぶ。
let mutable inLambdaDepth = 0

let unionAll (sets: Set<string> list) = List.fold Set.union Set.empty sets

// ---------------------------------------------------------------------------
// パターン / 識別子ヘルパ
// ---------------------------------------------------------------------------
let sidText (si: SynIdent) =
    match si with
    | SynIdent(idr, _) -> idr.idText

let longIds (sli: SynLongIdent) =
    match sli with
    | SynLongIdent(ids, _, _) -> ids

let rec patName (p: SynPat) : string option =
    match p with
    | SynPat.Named(si, _, _, _) -> Some(sidText si)
    | SynPat.LongIdent(sli, _, _, _, _, _) -> List.tryLast (longIds sli) |> Option.map (fun i -> i.idText)
    | SynPat.Paren(inner, _) -> patName inner
    | SynPat.Typed(inner, _, _) -> patName inner
    | SynPat.Attrib(inner, _, _) -> patName inner
    | SynPat.As(_, rhs, _) -> patName rhs
    | _ -> None

let rec patIdents (p: SynPat) : Set<string> =
    match p with
    | SynPat.Named(si, _, _, _) -> Set.singleton (sidText si)
    | SynPat.LongIdent(_, _, _, argPats, _, _) -> argPatIdents argPats
    | SynPat.Paren(inner, _) -> patIdents inner
    | SynPat.Typed(inner, _, _) -> patIdents inner
    | SynPat.Attrib(inner, _, _) -> patIdents inner
    | SynPat.Ands(pats, _) -> unionAll (List.map patIdents pats)
    | SynPat.Or(l, r, _, _) -> Set.union (patIdents l) (patIdents r)
    | SynPat.ListCons(l, r, _, _) -> Set.union (patIdents l) (patIdents r)
    | SynPat.As(l, r, _) -> Set.union (patIdents l) (patIdents r)
    | SynPat.Tuple(_, pats, _, _) -> unionAll (List.map patIdents pats)
    | SynPat.ArrayOrList(_, pats, _) -> unionAll (List.map patIdents pats)
    | SynPat.OptionalVal(idn, _) -> Set.singleton idn.idText
    | _ -> Set.empty

and argPatIdents (ap: SynArgPats) =
    match ap with
    | SynArgPats.Pats pats -> unionAll (List.map patIdents pats)
    | SynArgPats.NamePatPairs(pairs, _, _) ->
        unionAll
            [
                for npf in pairs do
                    match npf with
                    | NamePatPairField(_, _, _, p, _) -> patIdents p
            ]

let rec simplePatIds (sp: SynSimplePat) =
    match sp with
    | SynSimplePat.Id(idn, _, _, _, _, _) -> Set.singleton idn.idText
    | SynSimplePat.Typed(inner, _, _) -> simplePatIds inner
    | SynSimplePat.Attrib(inner, _, _) -> simplePatIds inner

let simplePatsIds (p: SynSimplePats) =
    match p with
    | SynSimplePats.SimplePats(ps, _, _) -> unionAll (List.map simplePatIds ps)

// ---------------------------------------------------------------------------
// App チェーンの分解: (head 識別子, head 位置, head 以降の引数リスト)
// ---------------------------------------------------------------------------
let rec splitApp (e: SynExpr) : (string * range) option * SynExpr list =
    match e with
    | SynExpr.App(_, _, f, a, _) ->
        let h, args = splitApp f
        h, args @ [ a ]
    | SynExpr.Paren(inner, _, _, _) -> splitApp inner
    | SynExpr.DebugPoint(_, _, inner) -> splitApp inner
    | SynExpr.TypeApp(inner, _, _, _, _, _, _) -> splitApp inner
    | SynExpr.Typed(inner, _, _) -> splitApp inner
    | SynExpr.Ident id -> Some(id.idText, id.idRange), []
    | SynExpr.LongIdent(_, sli, _, r) ->
        (match List.tryLast (longIds sli) with
         | Some id -> Some(id.idText, r)
         | None -> None),
        []
    | other -> None, [ other ]

let bindingExpr (b: SynBinding) =
    match b with
    | SynBinding(expr = e) -> e

let bindingRange (b: SynBinding) =
    match b with
    | SynBinding(range = r) -> r

let bindingHeadPat (b: SynBinding) =
    match b with
    | SynBinding(headPat = p) -> p

let bindingAttr (b: SynBinding) : bool =
    match b with
    | SynBinding(attributes = attrLists) ->
        attrLists
        |> List.collect (fun al -> al.Attributes)
        |> List.exists (fun a ->
            let tn = a.TypeName |> longIds |> List.map (fun i -> i.idText) |> String.concat "."
            tn.EndsWith "TailCall")

let registerGroup (bs: SynBinding list) =
    if not (List.isEmpty bs) then
        let names = bs |> List.choose (bindingHeadPat >> patName)
        let line = (bindingRange (List.head bs)).StartLine

        if seenGroups.Add(curFile, line, String.concat "/" names) then
            groups.Add
                {
                    File = curFile
                    Names = names
                    Bindings = bs
                    Line = line
                    Attr = List.exists bindingAttr bs
                }

let hit (fn: string) (r: range) (tail: bool) (bare: bool) (targets: Set<string>) (shadow: Set<string>) =
    if not collectPhase then
        let active = targets - shadow

        if Set.contains fn active then
            let key = sprintf "%s:%d:%d:%s:%d" curFile r.StartLine r.StartColumn fn curGroupLine

            if seenHits.Add key then
                hits.Add
                    {
                        File = curFile
                        GroupLine = curGroupLine
                        GroupNames = curGroupNames
                        GroupAttr = curGroupAttr
                        Fn = fn
                        Line = r.StartLine
                        Col = r.StartColumn
                        Tail = tail
                        Bare = bare
                        InLambda = inLambdaDepth > 0
                    }

// ---------------------------------------------------------------------------
// 本体走査。collectPhase=true では rec グループ収集のみ、
// false では targets（現在解析中のグループ名集合）への呼び出しを記録する。
// ---------------------------------------------------------------------------
let rec walk (targets: Set<string>) (shadow: Set<string>) (tail: bool) (e: SynExpr) : unit =
    let w = walk targets

    match e with
    | SynExpr.Paren(inner, _, _, _) -> w shadow tail inner
    | SynExpr.DebugPoint(_, _, inner) -> w shadow tail inner
    | SynExpr.Typed(inner, _, _) -> w shadow tail inner
    | SynExpr.FromParseError(inner, _) -> w shadow tail inner
    | SynExpr.DiscardAfterMissingQualificationAfterDot(inner, _, _) -> w shadow tail inner
    | SynExpr.Const _
    | SynExpr.Null _
    | SynExpr.ImplicitZero _
    | SynExpr.Typar _
    | SynExpr.ArbitraryAfterError _ -> ()
    | SynExpr.Ident id -> hit id.idText id.idRange tail true targets shadow
    | SynExpr.LongIdent(_, sli, _, r) ->
        match List.tryLast (longIds sli) with
        | Some id when List.length (longIds sli) = 1 -> hit id.idText r tail true targets shadow
        | _ -> ()
    | SynExpr.App(_, _, funcE, argE, _) ->
        let head, funcArgs = splitApp funcE

        // `x |> f a` = `f a x` なら関数側は右（argE）、
        // `f a <| x` = `(f a) x` なら関数側は左（funcArgs）。
        // 演算子の関数側だけは末尾を継承し、値側（先に評価される側）は非末尾のまま。
        let headName = head |> Option.map (fun (n, _) -> n)
        let pipeRight = headName |> Option.exists (fun n -> n.StartsWith "op_PipeRight")
        let pipeLeft = headName |> Option.exists (fun n -> n.StartsWith "op_PipeLeft")

        match head with
        | Some(fn, r) -> hit fn r tail false targets shadow
        | None -> ()

        // head の後ろに付く引数と head 以外の関数位置はすべて非末尾
        // （`f a |> g` の f、`a >>= k` の a など。値は先に評価されるため）。
        for a in funcArgs do
            w shadow (if pipeLeft then tail else false) a

        w shadow (if pipeRight then tail else false) argE
    | SynExpr.IfThenElse(c, th, el, _, _, _, _) ->
        w shadow false c
        w shadow tail th

        match el with
        | Some x -> w shadow tail x
        | None -> ()
    | SynExpr.Match(_, scrut, clauses, _, _) ->
        w shadow false scrut
        walkClauses targets shadow tail clauses
    | SynExpr.MatchBang(_, scrut, clauses, _, _) ->
        w shadow false scrut
        walkClauses targets shadow tail clauses
    | SynExpr.MatchLambda(_, _, clauses, _, _) -> walkClauses targets shadow tail clauses
    | SynExpr.Sequential(_, _, e1, e2, _, _) ->
        w shadow false e1
        w shadow tail e2
    | SynExpr.SequentialOrImplicitYield(_, e1, e2, ifNot, _) ->
        w shadow false e1
        w shadow tail e2
        w shadow false ifNot
    | SynExpr.LetOrUse lo ->
        let names = lo.Bindings |> List.choose (bindingHeadPat >> patName) |> Set.ofList
        let shadowBody = Set.union shadow names
        let shadowB = if lo.IsRecursive then shadowBody else shadow

        if lo.IsRecursive && collectPhase then
            registerGroup lo.Bindings

        for b in lo.Bindings do
            walk targets shadowB false (bindingExpr b)

        walk targets shadowBody tail lo.Body
    | SynExpr.Lambda(_, _, pats, body, _, _, _) ->
        // ラムダ本体は「ラムダ内」を立てたうえで非末尾として走査する
        // （FS3569 はラムダ末尾を許容するが、実行時にはフレームが積みうるため）。
        inLambdaDepth <- inLambdaDepth + 1
        w (Set.union shadow (simplePatsIds pats)) false body
        inLambdaDepth <- inLambdaDepth - 1
    | SynExpr.TryWith(tryE, cases, _, _, _, _) ->
        // .NET の tail. 命令は保護区域内では出せない
        w shadow false tryE
        walkClauses targets shadow false cases
    | SynExpr.TryFinally(tryE, finE, _, _, _, _) ->
        w shadow false tryE
        w shadow false finE
    | SynExpr.While(_, cond, body, _)
    | SynExpr.WhileBang(_, cond, body, _) ->
        w shadow false cond
        w shadow false body
    | SynExpr.For(_, _, _, _, identBody, _, toBody, doBody, _) ->
        w shadow false identBody
        w shadow false toBody
        w shadow false doBody
    | SynExpr.ForEach(_, _, _, _, pat, enumE, bodyE, _) ->
        w shadow false enumE
        w (Set.union shadow (patIdents pat)) false bodyE
    | SynExpr.Do(inner, _) -> w shadow tail inner
    | SynExpr.DoBang(inner, _, _) -> w shadow false inner
    | SynExpr.Assert(inner, _) -> w shadow false inner
    | SynExpr.YieldOrReturn(_, inner, _, _) -> w shadow false inner
    | SynExpr.YieldOrReturnFrom(_, inner, _, _) -> w shadow false inner
    | SynExpr.ComputationExpr(_, inner, _) -> w shadow false inner
    | SynExpr.New(_, _, inner, _) -> w shadow false inner
    | SynExpr.Tuple(_, exprs, _, _) ->
        for x in exprs do
            w shadow false x
    | SynExpr.ArrayOrList(_, exprs, _) ->
        for x in exprs do
            w shadow false x
    | SynExpr.ArrayOrListComputed(_, inner, _) -> w shadow false inner
    | SynExpr.Record(_, _, fields, _) ->
        for f in fields do
            match f with
            | SynExprRecordField(_, _, eOpt, _, _) ->
                match eOpt with
                | Some x -> w shadow false x
                | None -> ()
    | SynExpr.DotGet(inner, _, _, _) -> w shadow false inner
    | SynExpr.DotSet(_, _, rhs, _) -> w shadow false rhs
    | SynExpr.Set(_, rhs, _) -> w shadow false rhs
    | SynExpr.LongIdentSet(_, inner, _) -> w shadow false inner
    | SynExpr.DotIndexedGet(objE, idxE, _, _) ->
        w shadow false objE
        w shadow false idxE
    | SynExpr.DotIndexedSet(objE, idxE, valE, _, _, _) ->
        w shadow false objE
        w shadow false idxE
        w shadow false valE
    | SynExpr.NamedIndexedPropertySet(_, e1, e2, _) ->
        w shadow false e1
        w shadow false e2
    | SynExpr.DotNamedIndexedPropertySet(_, _, e1, e2, _) ->
        w shadow false e1
        w shadow false e2
    | SynExpr.TypeTest(inner, _, _)
    | SynExpr.Upcast(inner, _, _)
    | SynExpr.Downcast(inner, _, _)
    | SynExpr.InferredUpcast(inner, _)
    | SynExpr.InferredDowncast(inner, _)
    | SynExpr.AddressOf(_, inner, _, _)
    | SynExpr.Lazy(inner, _)
    | SynExpr.Fixed(inner, _)
    | SynExpr.IndexFromEnd(inner, _) -> w shadow false inner
    | SynExpr.Quote(op, _, quoted, _, _) ->
        w shadow false op
        w shadow false quoted
    | SynExpr.TraitCall(_, _, argE, _) -> w shadow false argE
    | SynExpr.InterpolatedString(parts, _, _) ->
        for p in parts do
            match p with
            | SynInterpolatedStringPart.String _ -> ()
            | SynInterpolatedStringPart.FillExpr(x, _) -> w shadow false x
    | SynExpr.Dynamic(f, _, a, _) ->
        w shadow false f
        w shadow false a
    | SynExpr.ObjExpr(_, _, _, bindings, members, _, _, _) ->
        for b in bindings do
            walk targets shadow false (bindingExpr b)

        for m in members do
            walkMember targets shadow false m
    | SynExpr.LibraryOnlyILAssembly(_, _, args, _, _) ->
        for x in args do
            w shadow false x
    | SynExpr.LibraryOnlyStaticOptimization(_, _, optimized, _) -> w shadow false optimized
    | SynExpr.LibraryOnlyUnionCaseFieldGet(inner, _, _, _) -> w shadow false inner
    | SynExpr.LibraryOnlyUnionCaseFieldSet(_, _, _, rhs, _) -> w shadow false rhs
    | _ ->
        let caseName =
            if FSharpType.IsUnion(e.GetType()) then
                (FSharpValue.GetUnionFields(e, e.GetType()) |> fst).Name
            else
                e.GetType().Name

        unhandled.Add(sprintf "%s %s" curFile caseName)

and walkClauses (targets: Set<string>) (shadow: Set<string>) (tail: bool) (clauses: SynMatchClause list) =
    for c in clauses do
        let (SynMatchClause(pat = pat; whenExpr = whenE; resultExpr = resE)) = c
        let sh = Set.union shadow (patIdents pat)

        match whenE with
        | Some wE -> walk targets sh false wE
        | None -> ()

        walk targets sh tail resE

and walkMember (targets: Set<string>) (shadow: Set<string>) (tail: bool) (m: SynMemberDefn) =
    match m with
    | SynMemberDefn.Member(b, _) -> walk targets shadow tail (bindingExpr b)
    | SynMemberDefn.GetSetMember(b1, b2, _, _) ->
        match b1 with
        | Some b -> walk targets shadow tail (bindingExpr b)
        | None -> ()

        match b2 with
        | Some b -> walk targets shadow tail (bindingExpr b)
        | None -> ()
    | SynMemberDefn.LetBindings(bs, _, isRec, _, _) ->
        if isRec && collectPhase then
            registerGroup bs

        for b in bs do
            walk targets shadow false (bindingExpr b)
    | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, _, _, e, _, _) -> walk targets shadow false e
    | SynMemberDefn.Interface(_, _, Some ms, _) ->
        for m2 in ms do
            walkMember targets shadow tail m2
    | _ -> ()

// ---------------------------------------------------------------------------
// モジュール宣言の走査
// ---------------------------------------------------------------------------
let rec walkDecl (d: SynModuleDecl) =
    match d with
    | SynModuleDecl.Let(isRec, bindings, _, _) ->
        if isRec && collectPhase then
            registerGroup bindings

        for b in bindings do
            walk Set.empty Set.empty false (bindingExpr b)
    | SynModuleDecl.Expr(expr, _) -> walk Set.empty Set.empty false expr
    | SynModuleDecl.NestedModule(_, _, decls, _, _, _) ->
        for d2 in decls do
            walkDecl d2
    | SynModuleDecl.Types(typeDefns, _) ->
        for td in typeDefns do
            walkTypeDefn td
    | SynModuleDecl.NamespaceFragment(frag) -> walkModuleOrNs frag
    | _ -> ()

and walkModuleOrNs (m: SynModuleOrNamespace) =
    let (SynModuleOrNamespace(decls = decls)) = m

    for d in decls do
        walkDecl d

and walkTypeDefn (td: SynTypeDefn) =
    let (SynTypeDefn(_, repr, mems, _, _, _)) = td

    match repr with
    | SynTypeDefnRepr.ObjectModel(_, ms, _) ->
        for m in ms do
            walkMember Set.empty Set.empty false m
    | _ -> ()

    for m in mems do
        walkMember Set.empty Set.empty false m

// ---------------------------------------------------------------------------
// ファイル単位の実行
// ---------------------------------------------------------------------------
let runFile (path: string) =
    curFile <- path
    collectPhase <- true
    let gsBefore = groups.Count
    let src = File.ReadAllText path

    let res =
        checker.ParseFile(path, SourceText.ofString src, mkOpts path)
        |> Async.RunSynchronously

    for d in res.Diagnostics do
        if d.Severity = FSharpDiagnosticSeverity.Error then
            printfn "PARSE ERROR %s:%d %s" path d.StartLine (string d.Message)

    match res.ParseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, topLevels, _, _, _)) ->
        for m in topLevels do
            walkModuleOrNs m
    | ParsedInput.SigFile _ -> ()

    for gi in gsBefore .. groups.Count - 1 do
        let g = groups[gi]

        if g.File = path then
            collectPhase <- false
            curGroupLine <- g.Line
            curGroupNames <- String.concat "/" g.Names
            curGroupAttr <- g.Attr

            for b in g.Bindings do
                walk (Set.ofList g.Names) Set.empty true (bindingExpr b)

    collectPhase <- true

// ---------------------------------------------------------------------------
// メイン: 実行 → サマリ → ベースライン差分（新規があれば exit 1）
// ---------------------------------------------------------------------------
/// リポジトリルート（RykDb.slnx があるディレクトリ）を
/// カレントディレクトリとアセンブリ配置地の両方から上へ遡って探す。
let findRoot () =
    let isRoot (d: string) =
        File.Exists(Path.Combine(d, "RykDb.slnx"))

    let rec up (d: string) =
        let full = Path.GetFullPath d

        if isRoot full then
            Some full
        else
            let p = Path.GetDirectoryName full

            if isNull p || p = full then None else up p

    [ Directory.GetCurrentDirectory(); AppContext.BaseDirectory ]
    |> List.tryPick (fun s ->
        try
            up s
        with _ ->
            None)

let root =
    match findRoot () with
    | Some r -> r
    | None ->
        eprintfn "リポジトリルートが見つかりません（上位階層に RykDb.slnx が無い）"
        Environment.Exit 2
        ""

let rootN = root.Replace('\\', '/')

let rel (f: string) =
    let fn = f.Replace('\\', '/')

    if fn.StartsWith(rootN + "/") then
        fn.Substring(rootN.Length + 1)
    else
        fn

let srcRoot = Path.Combine(root, "src")

let files =
    Directory.GetFiles(srcRoot, "*.fs", SearchOption.AllDirectories)
    |> Array.filter (fun p -> not (p.Contains "/obj/" || p.Contains "/bin/"))
    |> Array.sort

if files.Length = 0 then
    eprintfn "対象ファイルが見つかりません: %s" srcRoot
    exit 2

for f in files do
    runFile f

let lineCacheAll = System.Collections.Generic.Dictionary<string, string[]>()

let getLines (f: string) =
    match lineCacheAll.TryGetValue f with
    | true, v -> v
    | _ ->
        let v = File.ReadAllLines f
        lineCacheAll[f] <- v
        v

let srcText (f: string) (line: int) =
    let ls = getLines f

    if line >= 1 && line <= ls.Length then
        ls[line - 1].Trim()
    else
        ""

let nonTail =
    hits
    |> Seq.filter (fun h -> not h.Tail)
    |> Seq.sortBy (fun h -> h.File, h.Line, h.Col)
    |> Seq.toList

let tailOk = hits |> Seq.filter (fun h -> h.Tail) |> Seq.length

// 行番号を含めないキー（行が動いても一致する）
let keyOf (h: Hit) =
    sprintf "%s|%s|%s|%s" (rel h.File) h.GroupNames h.Fn (if h.Bare then "bare" else "call")

let currentKeys = nonTail |> List.map keyOf |> List.distinct |> List.sort

let printSite (h: Hit) =
    let kind = if h.Bare then "bare" else "call"
    let attr = if h.GroupAttr then " [TailCall attr!]" else ""
    let lam = if h.InLambda then " [lambda]" else ""

    printfn
        "  %s:%d:%d  grp@%d(%s) -> %s [%s]%s%s"
        (rel h.File)
        h.Line
        h.Col
        h.GroupLine
        h.GroupNames
        h.Fn
        kind
        attr
        lam

    printfn "      %s" (srcText h.File h.Line)

// --- CLI 引数（先頭はこのアセンブリのパスなので捨てる） ---
let cliArgs = Environment.GetCommandLineArgs() |> Array.skip 1 |> Array.toList

let update = cliArgs |> List.exists ((=) "--update")
let showAll = cliArgs |> List.exists ((=) "--all")

let baselinePath =
    cliArgs
    |> List.filter (fun a -> not (a.StartsWith "--"))
    |> List.tryHead
    |> Option.defaultValue (Path.Combine(root, "tools", "TailAudit", "TailAudit.baseline"))

let readBaseline (p: string) =
    if File.Exists p then
        File.ReadAllLines p
        |> Array.map (fun l -> l.Trim())
        |> Array.filter (fun l -> l <> "" && not (l.StartsWith "#"))
        |> Array.toList
    else
        []

// --- サマリ ---
let mutual = groups |> Seq.filter (fun g -> List.length g.Names > 1) |> Seq.toList
let attrGroups = groups |> Seq.filter (fun g -> g.Attr) |> Seq.toList

let unhandledCases =
    unhandled
    |> Seq.distinct
    |> Seq.map (fun u ->
        let i = u.LastIndexOf ' '

        if i >= 0 then u.Substring(i + 1) else u)
    |> Seq.countBy id
    |> Seq.toList

printfn "===== tail 監査: %s =====" rootN
printfn "ファイル %d / rec グループ %d（相互再帰 %d・[<TailCall>] 付き %d）" files.Length groups.Count mutual.Length attrGroups.Length

let inLambdaNonTail = nonTail |> List.filter (fun h -> h.InLambda) |> List.length

printfn
    "再帰呼び出し %d（末尾 %d / 非末尾 %d：うちラムダ内 %d・直接 %d）"
    hits.Count
    tailOk
    nonTail.Length
    inLambdaNonTail
    (nonTail.Length - inLambdaNonTail)

printfn "  区分: ラムダ内 = Compute/pull 継続・CPS 継続（深度は 1 行ごと / MaxExpressionDepth で頭打ち）"

printfn "        直接 = 監査部・引数など「結果を消費してから」型（本来の末尾化候補）"

for (caseName, n) in unhandledCases do
    printfn "walk 未対応ケース: %s（%d ファイル）※スライス境界など・再帰を含まない想定" caseName n

let contra = nonTail |> List.filter (fun h -> h.GroupAttr)

if not (List.isEmpty contra) then
    printfn "属性付きグループ内の非末尾検出: %d（FS3569 はラムダ末尾を許容する既知の差）" contra.Length

if showAll then
    printfn ""
    printfn "===== 非末尾の再帰呼び出し（全 %d 件）=====" nonTail.Length

    for h in nonTail do
        printSite h

// --- ベースライン ---
if update then
    let header =
        [|
            "# RykDb tail 監査ベースライン（dotnet run --project tools/TailAudit -- --update で再生成）"
            "# 形式: ファイル相対|グループ|呼出先|種別 — 行番号は含めない（行が動いても一致する）"
        |]

    File.WriteAllLines(baselinePath, Array.append header (List.toArray currentKeys))
    printfn ""
    printfn "ベースラインを更新: %s（%d キー）" baselinePath currentKeys.Length
    exit 0

let baselineExists = File.Exists baselinePath
let baseSet = Set.ofList (readBaseline baselinePath)
let curSet = Set.ofList currentKeys
let newKeys = Set.difference curSet baseSet |> Set.toList
let staleKeys = Set.difference baseSet curSet |> Set.toList

printfn ""

if not baselineExists then
    printfn "ベースラインがありません: %s（--update で生成）" baselinePath

printfn "ベースライン: %d キー / 新規 %d / 消滅 %d" baseSet.Count newKeys.Length staleKeys.Length

if List.isEmpty newKeys then
    if not (List.isEmpty staleKeys) then
        printfn "消失キー（--update で整理可）:"

        for k in staleKeys do
            printfn "  - %s" k

    printfn "✅ 新規の非末尾再帰はありません（既知 %d キー。全件表示は --all）" currentKeys.Length
    exit 0
else
    printfn ""
    printfn "===== 新規の非末尾再帰（%d キー）=====" newKeys.Length

    for h in nonTail do
        if List.contains (keyOf h) newKeys then
            printSite h

    printfn ""
    printfn "新しいキー:"

    for k in newKeys do
        printfn "  + %s" k

    printfn ""
    printfn "対応方針: 末尾位置に直す。"
    printfn "直せない場合は理由をコメントで残し、内容を確認のうえ --update でベースラインへ追加する。"
    exit 1

[<EntryPoint>]
let main _ =
    // 上のモジュール初期化がすべての経路で exit 済み（ここには到達しない）。
    0
