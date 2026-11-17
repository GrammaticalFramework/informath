/-
Export the declarations of a Lean file to Informath's Dedukti dialect.

    lean --run Export.lean File.lean outDir [mapping.tsv]
    # -> outDir/File.dk, File.json, File-hide.dkgf, File-deps.dk

(lean-export.py runs this, with `lake env` when the file is in a Lake project.)

The file is elaborated by Lean, and each declaration it makes (theorems,
definitions, axioms, inductive types with their constructors, structures with
their fields) is translated, as mathlib-informath's exporter does for Mathlib
but with Lean only:

* a binder `(x : α)` with `α : Type` becomes `(x : Elem α)`, one with `α : Prop`
  becomes `(h : Proof α)`, and `(α : Type u)` becomes `(α : Set)`; an instance
  binder `[C α]` becomes a hypothesis `(_ : Proof (C α))`;
* in a proposition, ∀ is `forall`, an implication `if`, ∃ `exists`, and the
  constants of mapping.tsv become Informath's (`And` is `and`, `HAdd.hAdd`
  is `plus`, `LE.le` is `Leq`, ...) applied to their explicit arguments;
  `=` is `Eq` on numbers and `Eqt` elsewhere; numerals are Informath's;
* any other constant keeps its Lean name, with `.` written `__`, and its
  arguments but the instances; the implicit ones are hidden in the readings by
  the #HIDE lines of File-hide.dkgf;
* a definition by pattern matching or recursion becomes rewrite rules, one
  for each of its equation lemmas (`f (n + 1) = ...`); another definition gets
  its definiens; a theorem is stated without its proof.

Each declaration is preceded by `(; //LEAN:<name> ;)`.  File.json has, for each
declaration, its Lean name, its Dedukti name, its kind, its docstring, Lean's
own statement and its lines.  File-deps.dk declares the constants that the
file uses but neither defines nor maps.

The output is for reading with Informath, not for checking with Dedukti:
universe levels and instances are left out.
-/
import Lean

open Lean Meta Elab

/-- Lean name -> Dedukti identifier: `.` as `__`, other non-identifier
characters as `_u<hex>_`; the words of Informath's framework are not redefined. -/
def mangle (n : Name) : String :=
  let s := n.toString (escape := false)
  let s := if ["Set", "Elem", "Prop", "Proof", "Type"].contains s then "Lean_" ++ s else s
  s.foldl (fun acc c =>
    if c == '.' then acc ++ "__"
    else if c.isAlphanum || c == '_' || c == '\'' || c == '!' || c == '?' then acc.push c
    else acc ++ s!"_u{String.ofList (Nat.toDigits 16 c.toNat)}_") ""

structure St where
  mapping : Std.HashMap Name String := {}
  used : Std.HashSet Name := {}      -- constants referred to, not mapped
  names : Std.HashSet String := {}   -- variable names in use

abbrev M := StateRefT St MetaM

/-- A numeral in Informath's base: a digit, or nn d (...) for more digits. -/
def numeral (n : Nat) : String :=
  let ds := (toString n).toList.map (·.toString)
  match ds with
  | [d] => d
  | _ =>
    let rec go : List String → String
      | [d] => s!"nd {d}"
      | d :: rest => s!"nn {d} ({go rest})"
      | [] => "0"
    go ds

def paren (s : String) : String := if s.contains ' ' then s!"({s})" else s

/-- A variable name in ASCII as Informath reads it: a Greek letter by its name,
a subscript digit as a digit. -/
def asciiName (s : String) : String :=
  s.foldl (fun acc c => acc ++ match c with
    | 'α' => "alpha" | 'β' => "beta" | 'γ' => "gamma" | 'δ' => "delta" | 'ε' => "epsilon"
    | 'ζ' => "zeta" | 'η' => "eta" | 'θ' => "theta" | 'ι' => "iota" | 'κ' => "kappa"
    | 'λ' => "lambda" | 'μ' => "mu" | 'ν' => "nu" | 'ξ' => "xi" | 'π' => "pi" | 'ρ' => "rho"
    | 'σ' => "sigma" | 'τ' => "tau" | 'φ' => "phi" | 'χ' => "chi" | 'ψ' => "psi" | 'ω' => "omega"
    | '₀' => "0" | '₁' => "1" | '₂' => "2" | '₃' => "3" | '₄' => "4" | '₅' => "5"
    | '₆' => "6" | '₇' => "7" | '₈' => "8" | '₉' => "9"
    | c => c.toString) ""

/-- A sort, or a sort as an outParam. -/
def sortish (t : Expr) : Bool :=
  t.isSort || (t.isAppOfArity ``outParam 1 && t.appArg!.isSort)

/-- A variable name for a binder: the user's name if it is a plain identifier
and free, else a fresh one (h for a proof, X for a type, x otherwise). -/
def freshName (n : Name) (prop : Bool) (sort : Bool := false) : M String := do
  let raw := n.eraseMacroScopes.toString (escape := false)
  let latin := match raw with
    | "α" => "X" | "β" => "Y" | "γ" => "Z" | "δ" => "W" | "ι" => "I" | "κ" => "J"
    | "σ" => "S" | "τ" => "T" | _ => "X"
  let base0 := if sort then raw else asciiName raw
  let ok := !base0.isEmpty && base0.all (fun c => c.isAlphanum || c == '_' || c == '\'') &&
    !(base0.toList.head?.map Char.isDigit |>.getD false) && !n.hasMacroScopes &&
    !["Set", "Elem", "Prop", "Proof", "Type", "forall", "exists", "if", "and", "or", "not"].contains base0
  let base := if ok then base0 else if prop then "h" else if sort then latin else "x"
  if !(← get).names.contains base then
    modify fun s => { s with names := s.names.insert base }
    return base
  let mut i := 1
  while (← get).names.contains s!"{base}{i}" do i := i + 1
  let nm := s!"{base}{i}"
  modify fun s => { s with names := s.names.insert nm }
  return nm

/-- The binder infos of the arguments of a constant. -/
def argInfos (c : Name) : MetaM (Array BinderInfo) := do
  let info ← getConstInfo c
  forallTelescope info.type fun xs _ => xs.mapM fun x => x.fvarId!.getBinderInfo

/-- The number types, whose equality is Informath's Eq. -/
def numberType (t : Expr) : Bool :=
  [``Nat, ``Int, `Rat, `Real].any t.isConstOf

mutual

/-- An expression in Dedukti: a proposition, an object or a type. -/
partial def tr (e : Expr) : M String := do
  match e with
  | .mdata _ b => tr b
  | .fvar id => return (← id.getUserName).toString (escape := false)
  | .sort .zero => return "Prop"
  | .sort _ => return "Set"
  | .lit (.natVal n) => return numeral n
  | .lit (.strVal s) => return s!"\"{s}\""
  | .const c _ => app c #[]
  | .app .. =>
    match e.getAppFn with
    | .const c _ => app c e.getAppArgs
    | f => do
      let as ← e.getAppArgs.mapM tr
      return " ".intercalate (paren (← tr f) :: (as.map paren).toList)
  | .lam n t b bi => binder n t b bi true
  | .forallE n t b bi => binder n t b bi false
  | .letE _ _ v b _ => tr (b.instantiate1 v)
  | .proj s i b => return s!"{mangle s}__proj{i} {paren (← tr b)}"
  | _ => return "UNSUPPORTED"

/-- A λ, or a Π / ∀: a quantifier or an implication in a proposition, a
function type elsewhere. -/
partial def binder (n : Name) (t b : Expr) (bi : BinderInfo) (isLam : Bool) : M String := do
  let prop ← isProp t
  let nm ← freshName n prop (sortish t)
  withLocalDecl (Name.mkSimple nm) bi t fun y => do
    let body := b.instantiate1 y
    let bs ← tr body
    if isLam then return s!"{nm} => {bs}"
    if ← isProp body then
      if prop && !b.hasLooseBVars then return s!"if {paren (← tr t)} {paren bs}"
      if prop then return s!"forall (Proof {paren (← tr t)}) ({nm} => {bs})"
      return s!"forall {paren (← tr t)} ({nm} => {bs})"
    if !b.hasLooseBVars then return s!"{← hypoType t} -> {bs}"
    return s!"({nm} : {← hypoType t}) -> {bs}"

/-- The type of a hypothesis or a value: Proof P, Set, Prop, or Elem α. -/
partial def hypoType (t : Expr) : M String := do
  if (← isProp t) then return s!"Proof {paren (← tr t)}"
  if t.isProp then return "Prop"
  if sortish t then return "Set"
  if (← isType t) && (← whnf t).isForall then return s!"({← tr t})"
  return s!"Elem {paren (← tr t)}"

/-- An application of a constant. -/
partial def app (c : Name) (args : Array Expr) : M String := do
  -- numerals: OfNat.ofNat α n inst
  if c == ``OfNat.ofNat && args.size ≥ 2 then
    if let .lit (.natVal n) := args[1]!.consumeMData then return numeral n
  if c == ``Nat.zero && args.isEmpty then return "0"
  -- ∃ x, p x
  if c == ``Exists && args.size == 2 then
    let p := args[1]!
    let body ← match p with
      | .lam n t b bi => binder n t b bi true
      | _ => tr p
    return s!"exists {paren (← tr args[0]!)} ({body})"
  -- equality: Eq on numbers, Eqt elsewhere
  if c == ``Eq && args.size == 3 then
    let ty ← whnfR args[0]!
    let (a, b) := (← tr args[1]!, ← tr args[2]!)
    if numberType ty then return s!"Eq {paren a} {paren b}"
    return s!"Eqt {paren (← tr args[0]!)} {paren a} {paren b}"
  let infos ← try argInfos c catch _ => pure #[]
  let info (i : Nat) : BinderInfo := infos.getD i .default
  let (name, keep) ← match (← get).mapping.get? c with
    -- a mapped constant: its explicit arguments
    | some m => pure (m, fun i => info i == .default)
    -- any other: all but the instances
    | none => do
      modify fun s => { s with used := s.used.insert c }
      pure (mangle c, fun i => info i != .instImplicit)
  let mut as := #[]
  for i in [0:args.size] do
    if keep i then as := as.push (paren (← tr args[i]!))
  return " ".intercalate (name :: as.toList)

end

/-- The statement of a declaration as a Dedukti type: its binders as
hypotheses, its conclusion as Proof P or as the type of what it defines. -/
partial def statement (t : Expr) : M String := do
  match t with
  | .forallE n ty b bi =>
    let prop ← isProp ty
    let nm ← freshName n prop (sortish ty)
    withLocalDecl (Name.mkSimple nm) bi ty fun y => do
      let rest ← statement (b.instantiate1 y)
      let h ← if bi == .instImplicit then pure s!"(_ : Proof {paren (← tr ty)})"
              else pure s!"({nm} : {← hypoType ty})"
      return s!"{h} -> {rest}"
  | _ => hypoType t

/-- The definiens of a definition, its λs matching the binders of its type. -/
partial def value (t v : Expr) : M String := do
  match t, v with
  | .forallE n ty b bi, .lam _ _ vb _ =>
    let nm ← freshName n (← isProp ty) (sortish ty)
    withLocalDecl (Name.mkSimple nm) bi ty fun y => do
      let rest ← value (b.instantiate1 y) (vb.instantiate1 y)
      return s!"{nm} => {rest}"
  | _, _ => tr v

/-- An equation lemma ∀ xs, lhs = rhs as a rewrite rule [xs] lhs --> rhs. -/
partial def rule (t : Expr) (vars : Array String := #[]) : M String := do
  match t.consumeMData with
  | .forallE n ty b bi =>
    let nm ← freshName n (← isProp ty) (sortish ty)
    withLocalDecl (Name.mkSimple nm) bi ty fun y => rule (b.instantiate1 y) (vars.push nm)
  | body =>
    let some (_, lhs, rhs) := body.eq? | throwError "not an equation"
    return s!"[{", ".intercalate vars.toList}] {← tr lhs} --> {← tr rhs}"

def jsonStr (s : String) : String := (Json.str s).compress

def kindOf : ConstantInfo → String
  | .thmInfo _ => "theorem" | .defnInfo _ => "def" | .axiomInfo _ => "axiom"
  | .inductInfo _ => "inductive" | .ctorInfo _ => "constructor" | .opaqueInfo _ => "opaque"
  | .recInfo _ => "recursor" | .quotInfo _ => "quot"

/-- Declarations worth a reading: not internal, not generated, not instances
nor their parts (the functions of a derived instance), not recursors;
constructors and fields come with their types. -/
def wanted (env : Environment) (n : Name) (ci : ConstantInfo) : Bool :=
  !n.isInternal && !isAuxRecursor env n && !isNoConfusion env n && !(isMatcherCore env n) &&
  !(Meta.isInstanceCore env n) && !(Meta.isInstanceCore env n.getPrefix) && !(ci matches .recInfo _) && !(ci matches .ctorInfo _) &&
  !(ci matches .quotInfo _) && (env.getProjectionFnInfo? n).isNone &&
  !n.isImplementationDetail

/-- Whether an expression uses compiled pattern matching or recursion. -/
def compiled (env : Environment) (v : Expr) : Bool :=
  (v.find? fun e => match e with
    | .const c _ => isMatcherCore env c || isAuxRecursor env c || isRecCore env c ||
        c.toString.endsWith ".brecOn" || c.toString.endsWith ".rec"
    | _ => false).isSome

def strip (s : String) : String :=
  String.ofList (s.toList.dropWhile Char.isWhitespace |>.reverse.dropWhile Char.isWhitespace |>.reverse)

/-- mapping.tsv: Lean constant, tab, Informath constant; # starts a comment line. -/
def readMapping (path : System.FilePath) : IO (Std.HashMap Name String) := do
  let mut m := {}
  if !(← path.pathExists) then return m
  for l in ← IO.FS.lines path do
    if l.startsWith "#" || l.all Char.isWhitespace then continue
    match l.splitOn "\t" with
    | [a, b] => m := m.insert (strip a).toName (strip b)
    | _ => pure ()
  return m

/-- Elaborate a Lean file: the environment it makes and its messages. -/
def elaborate (path : System.FilePath) : IO (Environment × MessageLog) := do
  let input ← IO.FS.readFile path
  let inputCtx := Parser.mkInputContext input path.toString
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header {} messages inputCtx
  let cmdState := Command.mkState env messages {}
  let s ← IO.processCommands inputCtx parserState cmdState
  return (s.commandState.env, s.commandState.messages)

/-- The Dedukti of a declaration (its judgements) and its kind. -/
def declDk (env : Environment) (n : Name) (ci : ConstantInfo) : M (String × String) := do
  let reset : M Unit := modify fun s => { s with names := {} }
  let decl (c : Name) (t : Expr) : M String := do
    reset
    return s!"{mangle c} : {← statement t}."
  match ci with
  | .inductInfo ii =>
    let mut out := #[s!"(; //LEAN:{n} ;)\n{← decl n ci.type}"]
    for c in ii.ctors do
      out := out.push s!"(; //LEAN:{c} ;)\n{← decl c (← getConstInfo c).type}"
    -- the fields of a structure
    if isStructure env n then
      for f in getStructureFields env n do
        let p := n ++ f
        if let some pci := env.find? p then
          out := out.push s!"(; //LEAN:{p} ;)\n{← decl p pci.type}"
    return ("\n\n".intercalate out.toList, if isStructure env n then "structure" else "inductive")
  | .defnInfo di =>
    let header := s!"(; //LEAN:{n} ;)\n"
    -- a definition by pattern matching or recursion: its equation lemmas as rules
    if compiled env di.value then
      if let some eqns ← getEqnsFor? n then
        let mut rules := #[]
        for e in eqns do
          reset
          rules := rules.push (← rule (← getConstInfo e).type)
        reset
        let d := s!"def {mangle n} : {← statement ci.type}."
        return (header ++ d ++ "\n" ++ "\n".intercalate rules.toList ++ ".", "def")
      return (header ++ (← decl n ci.type), "def")
    reset
    let t ← statement ci.type
    reset
    let v ← value ci.type di.value
    return (header ++ s!"def {mangle n} : {t} :=\n  {v}.", "def")
  | _ => return (s!"(; //LEAN:{n} ;)\n{← decl n ci.type}", kindOf ci)

def exportFile (path : System.FilePath) (outDir : String) (mapping : Std.HashMap Name String) :
    IO UInt32 := do
  let (env, messages) ← elaborate path
  let mut errors := 0
  for m in messages.toList do
    if m.severity == .error then
      errors := errors + 1
      IO.eprintln (← m.toString)
  let mod := (path.fileStem.getD "Input")
  let ctx : Core.Context := { fileName := path.toString, fileMap := default, maxHeartbeats := 0,
                              options := {} }
  let st : Core.State := { env }
  -- the declarations of the file, in the order of the source
  let mut found := #[]
  for (n, ci) in env.constants.map₂.toList do
    if !wanted env n ci then continue
    match ← ((Lean.findDeclarationRanges? n : CoreM _).toIO ctx st |>.toBaseIO) with
    | .ok (some r, _) => found := found.push (n, ci, r)
    | _ => pure ()
  let decls := found.qsort fun (_, _, a) (_, _, b) => a.range.pos.line < b.range.pos.line ||
    (a.range.pos.line == b.range.pos.line && a.range.pos.column < b.range.pos.column)
  let mut dk := s!"(; {mod}: declarations exported from {path.fileName.getD ""} by scripts/lean/Export.lean ;)\n\n"
  let mut json := #[]
  let mut used : Std.HashSet Name := {}
  let mut exported : Std.HashSet Name := {}
  for (n, ci, r) in decls do
    let res ← (((declDk env n ci).run {mapping}).run' {} {}).toIO ctx st |>.toBaseIO
    let doc ← findDocString? env n
    let pp ← ((Meta.ppExpr ci.type).run' {} {}).toIO ctx st |>.toBaseIO
    let ppStr := match pp with | .ok (f, _) => toString f | .error _ => ""
    match res with
    | .ok (((s, kind), stt), _) =>
      dk := dk ++ s ++ "\n\n"
      used := stt.used.fold (fun acc c => acc.insert c) used
      exported := exported.insert n
      if let .inductInfo ii := ci then
        for c in ii.ctors do exported := exported.insert c
        if isStructure env n then
          for f in getStructureFields env n do exported := exported.insert (n ++ f)
      json := json.push s!"\{\"lean\": {jsonStr n.toString}, \"dk\": {jsonStr (mangle n)}, \"kind\": {jsonStr kind}, \"doc\": {jsonStr (doc.getD "")}, \"statement\": {jsonStr ppStr}, \"line\": {r.range.pos.line}, \"endLine\": {r.range.endPos.line}}"
    | .error e =>
      dk := dk ++ s!"(; //LEAN:{n} ;)\n(; NOT EXPORTED: {toString e} ;)\n\n"
  -- the constants used but neither exported nor mapped, with their types
  let mut deps := s!"(; constants used by {mod}, declared with their translated types ;)\n\n"
  for c in used.toList.toArray.qsort (·.toString < ·.toString) do
    if exported.contains c then continue
    let some ci := env.find? c | continue
    let res ← (((do modify fun s => { s with names := {} }; statement ci.type : M String).run {mapping}).run' {} {}).toIO ctx st |>.toBaseIO
    match res with
    | .ok ((s, _), _) => deps := deps ++ s!"(; //LEAN:{c} ;)\n{mangle c} : {s}.\n\n"
    | .error _ => deps := deps ++ s!"(; NOT EXPORTED: {c} ;)\n\n"
  -- the implicit arguments, hidden in the readings: #HIDE c i j counts the
  -- arguments that Dedukti has, which are all but the instances
  let mut hide := s!"# {mod}: the implicit arguments of its constants (scripts/lean/Export.lean)\n"
  for c in (exported.toList ++ used.toList).toArray.qsort (·.toString < ·.toString) do
    if mapping.contains c || [``Eq, ``Exists, ``OfNat.ofNat].contains c then continue
    let r ← ((argInfos c).run' {} {}).toIO ctx st |>.toBaseIO
    if let .ok (infos, _) := r then
      let mut hidden := #[]
      let mut i := 0
      for bi in infos do
        if bi == .instImplicit then continue
        i := i + 1
        if bi != .default then hidden := hidden.push i
      if !hidden.isEmpty then
        hide := hide ++ s!"#HIDE {mangle c} {" ".intercalate (hidden.toList.map toString)}\n"
  IO.FS.createDirAll outDir
  IO.FS.writeFile s!"{outDir}/{mod}.dk" dk
  IO.FS.writeFile s!"{outDir}/{mod}-deps.dk" deps
  IO.FS.writeFile s!"{outDir}/{mod}-hide.dkgf" hide
  IO.FS.writeFile s!"{outDir}/{mod}.json" ("[\n" ++ ",\n".intercalate json.toList ++ "\n]\n")
  IO.println s!"{mod}: {decls.size} declarations -> {outDir}/{mod}.dk, .json, -hide.dkgf, -deps.dk"
  return if errors > 0 then 1 else 0

unsafe def main (args : List String) : IO UInt32 := do
  let (path, outDir, mappingFile) ← match args with
    | [p, o] => pure (p, o, none)
    | [p, o, m] => pure (p, o, some m)
    | _ => do
      IO.eprintln "usage: lean --run Export.lean <File.lean> <outDir> [mapping.tsv]"
      return 1
  initSearchPath (← findSysroot)
  enableInitializersExecution
  let mapping ← readMapping (mappingFile.getD "mapping.tsv")
  exportFile path outDir mapping
