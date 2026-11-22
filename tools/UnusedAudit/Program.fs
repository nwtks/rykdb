// ===========================================================================
// UnusedAudit — 「未使用の private 宣言」の反復監査
//
//   dotnet run --project tools/UnusedAudit               検証（新規 0 件なら exit 0）
//   dotnet run --project tools/UnusedAudit -- --all      検出全件（既知分も表示）
//   dotnet run --project tools/UnusedAudit -- --update   現状をベースラインへ書き出し（内容確認後に）
//   dotnet run --project tools/UnusedAudit -- <path>     ベースラインを差し替えて実行
//
// 対象:
//   - module 直下の let private（値・関数）
//   - module private / type private / member private / val private / member val private
//   - private 宣言のみ（public / internal は対象外）
//   - class 内の let は FS1182 が担当するため対象外（--warnon:1182 で検出済み）
//
// 判定:
//   - FCS の AST で private 宣言と宣言範囲を集める
//   - 同じファイルのトークン列から宣言名と一致する識別子を探す
//   - 宣言範囲の内側（自己再帰を含む）は使用と数えない
//   - 名前一致のみで判定するため、同名の別宣言は使用とみなす（偽陽性を避ける）
//   - リフレクション・シリアライズ等で使う宣言はベースラインで追認する
//
// 制約:
//   - private な union case / record フィールド / 演算子 / アクティブパターンは対象外
//   - .fsi と tests/ は対象外
//   - 大文字で始まる private 関数（LongIdent 宣言）は取りこぼす（現状の src/ には無い）
//
// FCS は NuGet ではなく SDK 同梱品を参照する
// （HintPath = `$(MSBuildBinPath)/FSharp/FSharp.Compiler.Service.dll`）。
// インストール場所・SDK バージョンに依存せず、ビルド中の SDK と必ず一致する。
// ===========================================================================

open System
open System.Collections.Generic
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization

// ---------------------------------------------------------------------------
// リポジトリルート（RykDb.slnx があるディレクトリ）を
// カレントディレクトリとアセンブリ配置地の両方から上へ遡って探す。
// ---------------------------------------------------------------------------
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
    |> Array.filter (fun p ->
        let n = p.Replace('\\', '/')
        not (n.Contains "/obj/" || n.Contains "/bin/"))
    |> Array.sort

if files.Length = 0 then
    eprintfn "対象ファイルが見つかりません: %s" srcRoot
    exit 2

// ---------------------------------------------------------------------------
// 宣言
// ---------------------------------------------------------------------------
type DeclKind =
    | Value
    | Field
    | Member
    | Type
    | Module

let kindKey =
    function
    | Value -> "value"
    | Field -> "field"
    | Member -> "member"
    | Type -> "type"
    | Module -> "module"

type Decl =
    {
        File: string
        Kind: DeclKind
        Name: string
        Line: int
        Col: int
        /// 宣言全体の範囲。この内側の同名トークンは使用と数えない。
        Exclude: range
        /// 型の判別子・レコードフィールド名。これらが使われていれば型は使用済みとみなす。
        Aliases: string list
    }

let isPrivateAccess =
    function
    | Some(SynAccess.Private _) -> true
    | _ -> false

/// 識別子として監査できる名前か。
/// 演算子・アクティブパターン（`|Foo|_|`）・意図的な `_` 始まりは除外する。
let isAuditableName (name: string) =
    name.Length > 0
    && not (name.StartsWith("_", StringComparison.Ordinal))
    && not (name.StartsWith("op_", StringComparison.Ordinal))
    && not (name.Contains "|")
    && (Char.IsLetter name.[0] || name.[0] = '_')

/// `let private f x = ...` の LongIdent 宣言を取り込むか。
/// `let private (Some x) = ...` のようなコンストラクタパターンと区別するため、
/// 小文字始まり・`op_` 始まりだけを宣言名とみなす。
let isLikelyFunctionName (name: string) =
    name.Length > 0
    && (Char.IsLower name.[0]
        || name.[0] = '_'
        || name.StartsWith("op_", StringComparison.Ordinal))

let attributeNames (attrs: SynAttributes) =
    [
        for attrList in attrs do
            for attr in attrList.Attributes do
                let parts =
                    match attr.TypeName with
                    | SynLongIdent(ids, _, _) -> ids |> List.map (fun i -> i.idText)

                yield String.concat "." parts
    ]

let hasEntryPointAttribute (attrs: SynAttributes) =
    attributeNames attrs
    |> List.exists (fun n -> n.EndsWith("EntryPoint", StringComparison.Ordinal))

// ---------------------------------------------------------------------------
// パターンから宣言名を取り出す
// ---------------------------------------------------------------------------
/// バインディング自体が private のときの名前。
/// タプル・As・型注釈をたどり、LongIdent は「private 関数」のときだけ名前として扱う。
let rec topLevelNames (allowLongIdent: bool) (pat: SynPat) : (string * range) list =
    match pat with
    | SynPat.Named(ident = SynIdent(id, _)) -> [ id.idText, id.idRange ]
    | SynPat.Typed(pat = inner)
    | SynPat.Paren(pat = inner)
    | SynPat.Attrib(pat = inner)
    | SynPat.FromParseError(pat = inner) -> topLevelNames false inner
    | SynPat.As(lhsPat = l; rhsPat = r) -> topLevelNames false l @ topLevelNames false r
    | SynPat.Tuple(elementPats = pats)
    | SynPat.Ands(pats = pats)
    | SynPat.ArrayOrList(elementPats = pats) -> List.collect (topLevelNames false) pats
    | SynPat.LongIdent(longDotId = SynLongIdent(ids, _, _)) when allowLongIdent ->
        match List.tryLast ids with
        | Some id when isLikelyFunctionName id.idText -> [ id.idText, id.idRange ]
        | _ -> []
    | _ -> []

/// パターン要素ごとに `private` が付いている名前（`let (private a, private b) = ...`）。
let rec privatePatternNames (pat: SynPat) : (string * range) list =
    match pat with
    | SynPat.Named(ident = SynIdent(id, _); accessibility = acc) ->
        if isPrivateAccess acc then
            [ id.idText, id.idRange ]
        else
            []
    | SynPat.Typed(pat = inner)
    | SynPat.Paren(pat = inner)
    | SynPat.Attrib(pat = inner)
    | SynPat.FromParseError(pat = inner) -> privatePatternNames inner
    | SynPat.As(lhsPat = l; rhsPat = r) -> privatePatternNames l @ privatePatternNames r
    | SynPat.Tuple(elementPats = pats)
    | SynPat.Ands(pats = pats)
    | SynPat.ArrayOrList(elementPats = pats) -> List.collect privatePatternNames pats
    | SynPat.LongIdent(longDotId = SynLongIdent(ids, _, _); accessibility = acc) ->
        if isPrivateAccess acc then
            match List.tryLast ids with
            | Some id when isLikelyFunctionName id.idText -> [ id.idText, id.idRange ]
            | _ -> []
        else
            []
    | _ -> []

/// メンバー宣言の名前（`member private this.Foo` など）。
let memberName (pat: SynPat) : (string * range) option =
    match pat with
    | SynPat.LongIdent(longDotId = SynLongIdent(ids, _, _)) ->
        match List.tryLast ids with
        | Some id -> Some(id.idText, id.idRange)
        | None -> None
    | SynPat.InstanceMember(memberId = id) -> Some(id.idText, id.idRange)
    | SynPat.Named(ident = SynIdent(id, _)) -> Some(id.idText, id.idRange)
    | _ -> None

let headPatIsPrivate (pat: SynPat) =
    match pat with
    | SynPat.LongIdent(accessibility = acc)
    | SynPat.Named(accessibility = acc)
    | SynPat.InstanceMember(accessibility = acc) -> isPrivateAccess acc
    | _ -> false

/// 自動プロパティの accessibility。
let autoPropIsPrivate (access: SynValSigAccess) =
    match access with
    | SynValSigAccess.Single(accessibility = a) -> isPrivateAccess a
    | SynValSigAccess.GetSet(accessibility = a; getterAccessibility = g; setterAccessibility = s) ->
        isPrivateAccess a || isPrivateAccess g || isPrivateAccess s

// ---------------------------------------------------------------------------
// 宣言の収集
// ---------------------------------------------------------------------------
let decls = ResizeArray<Decl>()
let seenDecls = HashSet<string * DeclKind * string>()

let addDeclWithAliases (kind: DeclKind) (name: string) (nameRange: range) (exclude: range) (aliases: string list) =
    if isAuditableName name then
        let key = nameRange.FileName, kind, name

        if seenDecls.Add key then
            decls.Add
                {
                    File = nameRange.FileName
                    Kind = kind
                    Name = name
                    Line = nameRange.StartLine
                    Col = nameRange.StartColumn
                    Exclude = exclude
                    Aliases = aliases |> List.filter isAuditableName
                }

let addDecl (kind: DeclKind) (name: string) (nameRange: range) (exclude: range) =
    addDeclWithAliases kind name nameRange exclude []

let walkTopLevelBinding (b: SynBinding) (privateCtx: bool) =
    match b with
    | SynBinding(accessibility = access; attributes = attrs; headPat = headPat) ->
        if not (hasEntryPointAttribute attrs) && (isPrivateAccess access || privateCtx) then
            for name, r in topLevelNames true headPat do
                addDecl Value name r b.RangeOfBindingWithRhs
        else
            for name, r in privatePatternNames headPat do
                addDecl Value name r b.RangeOfBindingWithRhs

let walkMemberBinding (b: SynBinding) =
    match b with
    | SynBinding(accessibility = access; headPat = headPat) ->
        if isPrivateAccess access || headPatIsPrivate headPat then
            match memberName headPat with
            | Some(name, r) -> addDecl Member name r b.RangeOfBindingWithRhs
            | None -> ()

let walkField (f: SynField) =
    match f with
    | SynField(idOpt = Some id; accessibility = access; range = r) ->
        if isPrivateAccess access then
            addDecl Field id.idText id.idRange r
    | _ -> ()

let rec walkModuleOrNamespace (m: SynModuleOrNamespace) (inheritedPrivate: bool) =
    match m with
    | SynModuleOrNamespace(accessibility = access; decls = decls) ->
        let privateCtx = inheritedPrivate || isPrivateAccess access

        for d in decls do
            walkDecl d privateCtx

and walkDecl (d: SynModuleDecl) (privateCtx: bool) =
    match d with
    | SynModuleDecl.Let(bindings = bindings) ->
        for b in bindings do
            walkTopLevelBinding b privateCtx
    | SynModuleDecl.NestedModule(
        moduleInfo = SynComponentInfo(accessibility = access; longId = longId); decls = nested; range = r) ->
        let explicitlyPrivate = isPrivateAccess access

        if explicitlyPrivate then
            match List.tryLast longId with
            | Some id -> addDecl Module id.idText id.idRange r
            | None -> ()

        for d in nested do
            walkDecl d (privateCtx || explicitlyPrivate)
    | SynModuleDecl.Types(typeDefns = typeDefns) ->
        for td in typeDefns do
            walkTypeDefn td privateCtx
    | SynModuleDecl.NamespaceFragment(fragment = frag) -> walkModuleOrNamespace frag privateCtx
    | _ -> ()

and walkTypeDefn (td: SynTypeDefn) (privateCtx: bool) =
    match td with
    | SynTypeDefn(
        typeInfo = SynComponentInfo(accessibility = access; longId = longId)
        typeRepr = repr
        members = members
        range = r) ->
        // DU のケース名・レコードのフィールド名は型名を書かずに使えるため、
        // 型が未使用かどうかはこれらも含めて判定する。
        let aliases =
            match repr with
            | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Union(unionCases = cases)) ->
                [
                    for c in cases do
                        let (SynUnionCase(ident = SynIdent(id, _))) = c
                        yield id.idText
                ]
            | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Enum(cases = cases)) ->
                [
                    for c in cases do
                        let (SynEnumCase(ident = SynIdent(id, _))) = c
                        yield id.idText
                ]
            | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Record(recordFieldsAndSpreads = fields)) ->
                [
                    for field in fields do
                        match field with
                        | SynFieldOrSpread.Field(field = SynField(idOpt = Some id)) -> yield id.idText
                        | _ -> ()
                ]
            | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.General(fields = fields)) ->
                [
                    for field in fields do
                        let (SynField(idOpt = idOpt)) = field

                        match idOpt with
                        | Some id -> yield id.idText
                        | None -> ()
                ]
            | _ -> []

        if isPrivateAccess access || privateCtx then
            match List.tryLast longId with
            | Some id -> addDeclWithAliases Type id.idText id.idRange r aliases
            | None -> ()

        for m in members do
            walkMember m

        match repr with
        | SynTypeDefnRepr.ObjectModel(members = reprMembers) ->
            for m in reprMembers do
                walkMember m
        | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.General(fields = fields)) ->
            for f in fields do
                walkField f
        | _ -> ()

and walkMember (m: SynMemberDefn) =
    match m with
    | SynMemberDefn.Member(memberDefn = b) -> walkMemberBinding b
    | SynMemberDefn.GetSetMember(memberDefnForGet = Some g; memberDefnForSet = Some s) ->
        walkMemberBinding g
        walkMemberBinding s
    | SynMemberDefn.GetSetMember(memberDefnForGet = Some g) -> walkMemberBinding g
    | SynMemberDefn.GetSetMember(memberDefnForSet = Some s) -> walkMemberBinding s
    | SynMemberDefn.AutoProperty(ident = id; accessibility = acc; range = r) ->
        if autoPropIsPrivate acc then
            addDecl Member id.idText id.idRange r
    | SynMemberDefn.ValField(fieldInfo = f) -> walkField f
    | SynMemberDefn.NestedType(typeDefn = td; accessibility = acc) -> walkTypeDefn td (isPrivateAccess acc)
    | SynMemberDefn.Interface(members = Some members) ->
        for m in members do
            walkMember m
    | _ -> ()

// ---------------------------------------------------------------------------
// トークン列（同名トークンを「使用」として数える）
// ---------------------------------------------------------------------------
let normalizeTokenText (text: string) =
    if
        text.Length >= 4
        && text.StartsWith("``", StringComparison.Ordinal)
        && text.EndsWith("``", StringComparison.Ordinal)
    then
        text.Substring(2, text.Length - 4)
    else
        text

let tokenize (path: string) : (string * range) array =
    let lines = File.ReadAllLines path
    let tokenizer = FSharpSourceTokenizer([], Some path, None)
    let tokens = ResizeArray<string * range>()
    let mutable state = FSharpTokenizerLexState.Initial

    for i in 0 .. lines.Length - 1 do
        let line = lines[i]
        let lineNo = i + 1
        let lineTokenizer = tokenizer.CreateLineTokenizer line
        let mutable running = true

        while running do
            let tokenOpt, nextState = lineTokenizer.ScanToken state
            state <- nextState

            match tokenOpt with
            | None -> running <- false
            | Some tok ->
                // RightColumn は包括（最終文字の位置）。
                let len = tok.RightColumn - tok.LeftColumn + 1

                if len > 0 && tok.LeftColumn >= 0 && tok.LeftColumn + len <= line.Length then
                    let text = normalizeTokenText (line.Substring(tok.LeftColumn, len))

                    let r =
                        Range.mkRange
                            path
                            (Position.mkPos lineNo tok.LeftColumn)
                            (Position.mkPos lineNo (tok.LeftColumn + len))

                    tokens.Add(text, r)

    tokens.ToArray()

let isInside (outer: range) (inner: range) =
    outer.FileName = inner.FileName && Range.rangeContainsRange outer inner

let isUsed (tokens: (string * range) array) (d: Decl) =
    tokens
    |> Array.exists (fun (text, r) -> (text = d.Name || List.contains text d.Aliases) && not (isInside d.Exclude r))

// ---------------------------------------------------------------------------
// メイン: 解析 → サマリ → ベースライン差分（新規があれば exit 1）
// ---------------------------------------------------------------------------
let checker = FSharpChecker.Create()

let mkOpts (path: string) =
    let opts, _diagnostics =
        checker.GetParsingOptionsFromCommandLineArgs [ "--target:exe"; "--warn:3"; path ]

    opts

let parseModules (path: string) : SynModuleOrNamespace list =
    let src = File.ReadAllText path

    let parsed =
        checker.ParseFile(path, SourceText.ofString src, mkOpts path)
        |> Async.RunSynchronously

    match parsed.ParseTree with
    | ParsedInput.ImplFile impl -> impl.Contents
    | ParsedInput.SigFile _ -> []

let findings = ResizeArray<Decl>()

for file in files do
    decls.Clear()
    seenDecls.Clear()

    for m in parseModules file do
        walkModuleOrNamespace m false

    if decls.Count > 0 then
        let tokens = tokenize file

        for d in decls do
            if not (isUsed tokens d) then
                findings.Add d

let sorted =
    findings
    |> Seq.sortBy (fun d -> rel d.File, d.Line, d.Col, kindKey d.Kind, d.Name)
    |> Seq.toList

let keyOf (d: Decl) =
    sprintf "%s|%s|%s" (rel d.File) (kindKey d.Kind) d.Name

let lineCache = Dictionary<string, string[]>()

let getLines (file: string) =
    match lineCache.TryGetValue file with
    | true, v -> v
    | _ ->
        let v = File.ReadAllLines file
        lineCache[file] <- v
        v

let printFinding (d: Decl) =
    printfn "  %s:%d:%d  [%s] %s" (rel d.File) d.Line (d.Col + 1) (kindKey d.Kind) d.Name

    let lines = getLines d.File

    if d.Line >= 1 && d.Line <= lines.Length then
        printfn "      %s" (lines[d.Line - 1].Trim())

// --- CLI 引数（先頭はこのアセンブリのパスなので捨てる） ---
let cliArgs = Environment.GetCommandLineArgs() |> Array.skip 1 |> Array.toList

let update = cliArgs |> List.exists ((=) "--update")
let showAll = cliArgs |> List.exists ((=) "--all")

let baselinePath =
    cliArgs
    |> List.filter (fun a -> not (a.StartsWith("--", StringComparison.Ordinal)))
    |> List.tryHead
    |> Option.defaultValue (Path.Combine(root, "tools", "UnusedAudit", "UnusedAudit.baseline"))

let readBaseline (p: string) =
    if File.Exists p then
        File.ReadAllLines p
        |> Array.map (fun l -> l.Trim())
        |> Array.filter (fun l -> l <> "" && not (l.StartsWith("#", StringComparison.Ordinal)))
        |> Array.toList
    else
        []

printfn "===== 未使用 private 監査: %s =====" rootN
printfn "ファイル %d / 未使用 private 宣言 %d 件" files.Length sorted.Length

if showAll then
    printfn ""
    printfn "===== 未使用 private 宣言（全 %d 件）=====" sorted.Length

    for d in sorted do
        printFinding d

// --- ベースライン ---
if update then
    let header =
        [|
            "# RykDb 未使用 private 監査ベースライン（dotnet run --project tools/UnusedAudit -- --update で再生成）"
            "# 形式: ファイル相対|種別|名前 — 行番号は含めない（行が動いても一致する）"
        |]

    let currentKeys = sorted |> List.map keyOf |> List.distinct |> List.sort

    File.WriteAllLines(baselinePath, Array.append header (List.toArray currentKeys))
    printfn ""
    printfn "ベースラインを更新: %s（%d キー）" baselinePath currentKeys.Length
    exit 0

let baselineExists = File.Exists baselinePath
let baseSet = Set.ofList (readBaseline baselinePath)
let currentKeys = sorted |> List.map keyOf |> List.distinct
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

    printfn "✅ 新規の未使用 private 宣言はありません（既知 %d キー。全件表示は --all）" currentKeys.Length
    exit 0
else
    let byKey = sorted |> List.groupBy keyOf |> Map.ofList

    printfn ""
    printfn "===== 新規の未使用 private 宣言（%d キー）=====" newKeys.Length

    for k in newKeys |> List.sort do
        match Map.tryFind k byKey with
        | Some ds ->
            for d in ds do
                printFinding d
        | None -> ()

    printfn ""
    printfn "新しいキー:"

    for k in newKeys |> List.sort do
        printfn "  + %s" k

    printfn ""
    printfn "未使用が意図的である場合は、内容を確認してから"
    printfn "  dotnet run --project tools/UnusedAudit -- --update"
    printfn "でベースラインへ追認してください。"
    exit 1
