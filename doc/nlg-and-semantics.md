# NLG and semantics as rewrite rules, and how they fit together

The informalization goes Dedukti → MathCore → Informath (`Dedukti2MathCore`,
then NLG in `MathCore2Informath` and `GodementNLG`), and the formalization
back Informath → MathCore → Dedukti (semantics in `Informath2MathCore`,
`GodementSemantics`, `Semantics`, then `MathCore2Dedukti`). This document
looks at the middle part only, MathCore ⇄ Informath, against the
specification

> if NLG produces a tree t' from a MathCore tree t, then the semantics of t'
> includes t

("includes", because the semantics of an ambiguous t' is a list of trees).
Part 1 writes NLG, and Part 2 semantics, as rewrite rules; Part 3 pairs each
NLG rule with the semantics that should undo it and lists where the
specification fails.

Written by Claude from the code of 10 November 2026 (informath `1c17d79`),
by reading it; the findings marked *to be checked* need the empirical test of
Part 4.

## Notation

Trees are written with the abstract syntax functions of `grammars/*.gf`,
without the `G` that the Haskell code adds (`AndProp`, not `GAndProp`).
Lexical leaves are written by their names (`set_Noun`). Metavariables:

| | category |
|---|---|
| `p q a b c` | Prop |
| `x y z v` | Ident |
| `xs ys` | lists of Ident |
| `k` | Kind |
| `e` | Exp |
| `f` | Formula |
| `s t` | Term |
| `ps` | list of Prop |
| `h hs` | Hypo, list of Hypo |

A rule

    lhs  ==>  rhs                         | condition

rewrites a subtree matching `lhs`. Unless said otherwise, a pass is applied
top-down to every subtree (`composOp`): the first rule that matches a node
rewrites it, and the pass goes on into the result's subtrees (or into the
node's subtrees if no rule matches). `[a, b]` is a GF list (`ListProp` etc.),
`f*` the pass applied recursively to `f`. A rule with `==>>` gives several
alternatives (the original tree among them where written `t ;`).

---

## Part 1. NLG: MathCore → Informath

`MathCore2Informath.nlg` applies the passes in this order (`ft`, `afts`, ...
in the code), and by default keeps only the final trees of pass 11, each
after `letDeclared` (pass 12); `-more-variants` also keeps the trees of the
earlier stages, and the variations of pass 13 and the user's `#NLG` rules of
pass 14. `InformathAPI` applies `bareQuantifiers` (pass 15) to the chosen
tree. With `-godement`, the Godement variants (pass 16) are made from the
trees of pass 11.

    t ─unparenth→ ─uncoerce→ ─funKinds→ ─extendStructure→ ─flatten→ ─aggregate→
      ─insitu→ ─varless→ ─collectivize→ ─negated→ ─implied→ ─scoped→   (= ncviafts)
      ─letDeclared→ (default)  |  ─variations→ ─#NLG→  |  ─godementVariants→

### 1. unparenth: binary connectives as lists

    CoreAndProp a b   ==>  AndProp [a, b]
    CoreOrProp a b    ==>  OrProp [a, b]
    CoreIfProp a b    ==>  IfProp a b
    CoreIffProp a b   ==>  IffProp a b

### 2. uncoerce: coercions removed

    ProofProp p       ==>  p
    ElemKind k        ==>  k
    CoercionExp _ e   ==>  e

### 3. funKinds: function kinds of sets

    FunKind [KindArgKind (ExpKind e)] (ExpKind v)
                      ==>  FunExpKind e v                         -- "function from A to B"
    FunKind [KindArgKind (ExpKind (TermExp s1)), ..., KindArgKind (ExpKind (TermExp sn))]
            (ExpKind (TermExp s))
                      ==>  ExpKind (TermExp (FunTypeTerm s1 (... (FunTypeTerm sn s))))   | n ≥ 2
    FunKind args (ExpKind v)
                      ==>  FunKind args* (ExpKind v*)             | every arg is KindArgKind (ExpKind _)

### 4. extendStructure: structures as definitions by extension (top level only)

    StructureJmt l [VarClause x k0, PropClause (Noun1Prop (NounNoun1 n) x), rest] k
        ==>  ExtensionJmt l k (NounKind n) x fields (conj props)
                                   | fields, props ≠ []; no other VarClause of kind k0 in rest
        ==>  ExtensionPropJmt l k (NounKind n) x (conj props)   | fields = []
        ==>  ExtensionDataJmt l k (NounKind n) x fields         | props = []
      where fields = the VarClause y k' of rest, consecutive equal kinds grouped
                     as IdentsArgKind k' [y1, y2, ...]
            props  = the PropClause q of rest;  conj [q] = q,  conj qs = AndProp qs
    StructureJmt l (params ++ [VarClause x k0, PropClause (Noun1Prop (NounNoun1 n) x), rest]) k
        ==>  ExtensionHyposJmt l (paramHypos params) k (NounKind n) x fields (conj props)
                                   | params ≠ [], x the last variable of kind k0, fields, props ≠ []
      where paramHypos: VarClause y _ , PropClause (Noun1Prop (NounNoun1 m) y)  ↦  VarsHypo [y] (NounKind m)
                        VarClause y k'  ↦  VarsHypo [y] k';   PropClause q  ↦  PropHypo q
    StructureJmt l [VarClause x (NounKind n), rest] k      | n ≠ set_Noun
        ==>  Extension(Prop|Data)Jmt l k (NounKind n) x ...  -- as above, the parent being x's kind

### 5. flatten: nested lists of the same connective flattened

    AndProp [p1, ..., AndProp [q1, ..., qm], ..., pn]  ==>  AndProp [p1, ..., q1, ..., qm, ..., pn]
    OrProp  (the same)

### 6. aggregate

Propositions:

    CoreNotProp p     ==>  NotAdjProp a e          | p* = AdjProp a e
                      ==>  CoreNotProp p*          | otherwise
    AndProp ps        ==>  q                       | groupProps "and" (map aggregate ps) = [q]
                      ==>  AndProp (groupProps "and" (map aggregate ps))
    OrProp ps         ==>  OrProp (groupProps "or" ps)     -- inner props not aggregated
    CoreAllProp k x (IfProp f b)            -- also with CoreIfProp
                      ==>  BoundedInAllProp f e b*      | bounding x f, k = ExpKind e, e = TermExp _
                      ==>  AllProp [FormulaArgKind k f] b*   | bounding x f
    CoreExistProp k x (AndProp [f, b])      -- also with CoreAndProp f b
                      ==>  BoundedInExistProp f e b*  /  ExistProp [FormulaArgKind k f] b*  (the same)
    CoreAllProp k x (CoreAllProp k y1 (... (CoreAllProp k yn p)))
                      ==>  AllProp [IdentsArgKind k [x, y1, ..., yn]] p*
    CoreExistProp     (the same, ExistProp)
    ListHypo hs       ==>  ListHypo (aggregateHypos hs)

  where `bounding x f` holds for `f = FormulaProp (EquationFormula (BinaryEquation c (IdentTerm x) s))`,
  `c` one of `< > ≤ ≥ ≠ ∈ ⊂ ⊆` and `x` not in `s`.

groupProps, on a flat list of conjuncts or disjuncts, left to right:

    AllProp ks p, AllProp ks q, ...         ==>  AllProp ks (p ∧ q), ...      | "and"; the same idents and kinds
    AdjProp a e, AdjProp b1 e, ..., AdjProp bn e, ...
                                            ==>  AdjProp (AndAdj [a, b1, ..., bn]) e, ...   (OrAdj for "or")
    AdjProp a e, AdjProp a e1, ..., AdjProp a en, ...
                                            ==>  AdjProp a (AndExp [e, e1, ..., en]), ...   (OrExp)
    FormulaProp (s0 c1 s1), FormulaProp (s1 c2 s2), ...
                                            ==>  FormulaProp (ChainEquation c1 s0 (... (BinaryEquation cn sn-1 sn))), ...  | "and"

aggregateHypos, on the list of hypotheses, in this order:

    splitVars:   VarsHypo [x1, ..., xn] k  ==>  VarsHypo [x1] k, ..., VarsHypo [xn] k
    nounKinds:   VarsHypo [x] set_Noun, ..., PropHypo (Noun1Prop (NounNoun1 n) x), ...
                                           ==>  VarsHypo [x] (NounKind n), ...    -- the PropHypo goes
    reorderVar:  a VarsHypo [x] k moves right after the last earlier VarsHypo [_] k,
                 unless k mentions a variable declared in between
    groupVars:   VarsHypo [x] k, VarsHypo [y1] k, ..., VarsHypo [yn] k  ==>  VarsHypo [x, y1, ..., yn] k
    attachProps: VarsHypo xs k, h1, ..., hm, ...   where h1 ... hm are the PropHypo/PropVarHypo
                 right after it whose props q1 ... qm mention some of xs:
       ==>  attached xs k [q1, ..., qm], ...
         attached xs set_Noun qs  =  attached xs (NounKind n) (qs minus the Noun1Prop n x)
                                                        | each x of xs has a q = Noun1Prop (NounNoun1 n) x
         attached xs k []         =  VarsHypo xs k
         attached xs k qs         =  AdjKindHypo xs a k  | k not ExpKind (TermExp _), and the conjuncts
                                                          of qs are exactly AdjProp a x for x in xs
         attached xs k [q]        =  SuchThatHypo xs q* k
         attached xs k qs         =  SuchThatHypo xs (AndProp qs)* k
    pairProps:   PropHypo a, PropHypo b, ...  ==>  PropHypo (AndProp [a, b])*, ...

### 7. insitu: a quantifier into the place of its variable

    AllProp [IdentsArgKind k [x]] (AdjProp a x)               ==>  AdjProp a (QuantExp (EveryIdentKindQuant x k))
    AllProp [IdentsArgKind k [x]] (CoreNotProp (AdjProp a x)) ==>  AdjProp a (QuantExp (NoIdentsKindQuant [x] k))
    CoreNotProp (ExistProp [IdentsArgKind k [x]] (AdjProp a x))
                                                              ==>  AdjProp a (QuantExp (NoIdentsKindQuant [x] k))
    ExistProp [IdentsArgKind k [x]] (AdjProp a x)             ==>  AdjProp a (QuantExp (SomeIdentsKindQuant [x] k))

### 8. varless: names that are not needed dropped

    EveryIdentKindQuant _ k       ==>  EveryKindQuant k
    AllIdentsKindQuant [_] k      ==>  AllKindQuant k
    NoIdentsKindQuant [_] k       ==>  NoKindQuant k
    SomeIdentsKindQuant [_] k     ==>  SomeKindQuant k
    IndefIdentKindQuant _ k       ==>  IndefKindQuant k
    PropVarHypo _ p               ==>  PropHypo p

### 9. collectivize (==>> alternatives)

    AndProp [AdjEProp a x1 y1, ..., AdjEProp a xn yn]
                ==>  AdjECollProp a (nub [x1, y1, ..., xn, yn])   | each pair shares a term with the later ones
    FunCExp g x y   ==>  FunCCollExp g [x1, ..., xn]     | x, y nested FunCExp g give n > 2 arguments
                    ==>> FunCExp g x' y'                 | otherwise, x' and y' the alternatives of x, y

### 10. negated

    CoreNotProp (AdjProp a e)       ==>  NotAdjProp a e
    CoreNotProp (Adj2Prop a e1 e2)  ==>  NotAdj2Prop a e1 e2
    CoreNotProp (AdjCProp a e1 e2)  ==>  NotAdjCProp a [e1, e2]
    CoreNotProp (AdjEProp a e1 e2)  ==>  NotAdjEProp a [e1, e2]
    CoreNotProp (Noun1Prop n e)     ==>  NotNoun1Prop n e
      ... and so for Noun2Prop, Noun3Prop, VerbProp, Verb2Prop, AdvProp, Adv2Prop, AdvCProp

### 11. implied: "A implies B"

    IfProp a b        ==>  IfProp (ant a*) b*
    IffProp a b       ==>  IffProp (ant a*) (ant b*)
    PropHypo p        ==>  PropHypo (assumed p*)      -- also PropVarHypo x p, SuchThatHypo xs p k
      where ant p = imp p if defined, else p
            assumed (AndProp ps) = AndProp (map ant ps);  assumed p = ant p
            imp (IfProp a b)  = FormulaImpliesProp (form a) (form b)
            imp (AndProp ps)  = AndProp (map imp ps);   imp (OrProp ps) = OrProp (map imp ps)
            form (FormulaProp f)    = f
            form (IdentProp s)      = MacroFormula s
            form FalseProp          = MacroFormula "\bot"
            form (CoreNotProp a)    = NotFormula (form a)
            form (IfProp a b)       = IfFormula (form a) (form b)
            form (FormulaImpliesProp f g) = IfFormula f g
            form (IffProp a b)      = IffFormula (form a) (form b)
            form (AndProp ps)       = foldl1 AndFormula (map form ps)
            form (OrProp ps)        = foldl1 OrFormula (map form ps)
            -- undefined otherwise, and then the rule does not apply

### 12. scoped: the scope of connectives

    AndProp ps   ==>  both a b               | ps' = [CoreNotProp _ = a, b]
                 ==>  AndProp ps'
      where ps' = the conjuncts of ps*, an AndProp among them spliced in, and each
                  inner AndProp [a, b] as BothAndProp a b, OrProp [a, b] as EitherOrProp a b
            both = BothAndProp
    OrProp ps    (the same, EitherOrProp)
    CoreNotProp p  ==>  CoreNotProp (inner p*)

### 12'. letDeclared (the default output)

    VarsHypo xs (ExpKind (TermExp s))            ==>  LetDeclarationHypo (ElemDeclaration xs s)
    SuchThatHypo xs p (ExpKind (TermExp s))      ==>  LetFormulaSuchThatHypo (ElemFormula xs s) p

### 13. variations (==>>, with -more-variants or -variations)

    AxiomJmt l hs p   ==>> t ;  AxiomJmt l hs1' (hypoProp hs2 p')    | hs = hs1 ++ hs2, each part varied
      where hypoProp (PropHypo q : hs) p = IfProp q (hypoProp hs p)
            hypoProp (VarsHypo xs k : hs) p = AllProp [IdentsArgKind k xs] (hypoProp hs p)
    VarsHypo xs (ExpKind (TermExp s))          ==>> LetDeclarationHypo (ElemDeclaration xs s) ; t
    VarsHypo xs (FunExpKind (TermExp s1) (TermExp s2))
                                               ==>> t ; LetDeclarationHypo (ElemDeclaration xs (FunTypeTerm s1 s2))
    FunExpKind (TermExp s1) (TermExp s2)       ==>> t ; ExpKind (TermExp (FunTypeTerm s1 s2))
    AllProp [ak] p                             ==>> t ; PostQuantProp p q   | q in allQuantVariations ak
    ExistProp [ak] p                           ==>> t ; PostQuantProp p q   | q in existQuantVariations ak
      where allQuantVariations (IdentsArgKind k [x]) = [EveryIdentKindQuant x k]
            allQuantVariations (IdentsArgKind k xs)  = [AllIdentsKindQuant xs k]
            existQuantVariations (IdentsArgKind k xs) = [SomeIdentsKindQuant xs k]
    CoreNotProp (ExistProp aks p)              ==>> t ; ExistNoProp aks p
    IfProp (FormulaProp f) (FormulaProp g)     ==>> t ; OnlyIfProp (FormulaProp f) (FormulaProp g) ; FormulaImpliesProp f g
    App4MacroTerm "\Summa" m n (IdentTerm i) s ==>> t ; sum3dots_Term s[i:=m] s[i:=m+1] s[i:=n]
    FormulaProp f                              ==>> t ; DisplayFormulaProp f'   | f' a variant of f of display size
    Oper2Term times_Oper2 s1 s2                ==>> t ; times_Term s1' s2'
    AndProp [a, b]   ==>> t ; BothAndProp a' b'     -- also AndAdj, AndExp (BothAnd...)
    OrProp [a, b]    ==>> t ; EitherOrProp a' b'    -- also OrAdj, OrExp (EitherOr...)

### 14. #NLG rules of symbol tables

`f x1 ... xn = rhs` (`Semantics.applyNLGDefs`): bottom-up, each function `f`
with NLG definitions gives the alternatives `rhs[x1 := a1, ..., xn := an]`.

### 15. bareQuantifiers (in InformathAPI, after NLG)

    AllProp [IdentsArgKind k xs] p    ==>  BareAllProp xs p*      | k marked #IMPLICIT
    ExistProp [IdentsArgKind k xs] p  ==>  BareExistProp xs p*    | k marked #IMPLICIT
    CoreAllProp k x p                 ==>  BareAllProp (x : ys) q | k #IMPLICIT; p* = BareAllProp ys q, else ys = [], q = p*
    CoreExistProp (the same)

### 16. GodementNLG (with -godement; all combinations of the transformations)

    typedHypos:  VarsHypo xs obj, ..., PropHypo (typing of x)
                   ==>  VarHypo x k (consecutive ones as VarsHypo), ...
                   | obj = mathematical object; typing: Noun1Prop (NounNoun1 n) x ↦ (x, NounKind n),
                     Noun2Prop (NounPrepNoun2 n p) x e ↦ (x, DepKind (NounPrepDep n p) e), ...
    typedDefinition:
      DefPropJmt l hs (AdjProp a x) df   ==>  DefAdjJmt l (hs − x) (QuantExp (IndefIdentKindQuant x k)) said a df'
                                           | df = typing of x as k ∧ df'
      DefPropJmt l hs (Noun2Prop (AdjNoun2Noun2 a n2) x e) df
                                         ==>  DefAdjJmt l (hs − x) (indef x k) said a df'   | df = Noun2Prop n2 x e ∧ df'
      DefPropJmt l hs p df               ==>  DefNounJmt l hs x said k df   | p predicates a noun k of x
      DefPropJmt l hs (AdjProp a x) df   ==>  DefAdjJmt l hs x said a df
    calledDefinition:  Def(Adj|Noun)Jmt ... said ...  ==>  ... called ...
    definedExp:        DefExpJmt l hs e _ df  ==>  NamingJmt l hs e defined df
    typedQuantifiers:  CoreAllProp obj x (IfProp (typing of x as k) b)   ==>  AllProp [IdentsArgKind k [x]] b
                       CoreExistProp obj x (typing of x as k ∧ b)        ==>  ExistProp [IdentsArgKind k [x]] b
    adjKinds:          AndProp [..., p, AdjProp a x, ...]  ==>  AndProp [..., KindProp x (AdjKind a k), ...]
                       | p predicates the noun k of x (also with p after the AdjProp)
    necSuff:           IffProp (AdjProp a x) q  ==>  NecSuffProp necsuff x (AdjPred a) q

---

## Part 2. Semantics: Informath → MathCore

`Informath2MathCore.semantics` applies, in this order:

    t' ─#SEMANTICS→ ─removeFonts→ ─godementExpand→ (several Jmts)
       ─godementSem→ ─sem→ ─addParenth→ ─addCoercions→ ─inSituResults→ (several trees)

### S1. #SEMANTICS rules of symbol tables

`f x1 ... xn = rhs` (`Semantics.applySemDefs`): bottom-up, one result.

### S2. removeFonts

    TextbfTerm s  ==>  s

### S3. godementExpand (one Jmt to several)

    a Jmt with RespProp a b / RespAdj a b  ==>  [the Jmt with a, the Jmt with b]
    Thm/AxiomJmt with CalledExp x n, DenotedExp x n inside
        ==>  the Jmt with x for them, and DefExpJmt noLabel hs n object_Noun x for each
    (a Jmt with an ill-formed declaration (DeclArgKind etc.) gives none)

### S4. godementSem (selected; `GodementSemantics.hs` has them all)

    DefAdjJmt l hs x _ a p      ==>  DefPropJmt l (hs ++ hs') (AdjProp a x') p     | (hs', x') = liftIndef x
    DefNounJmt l hs x _ k p     ==>  DefPropJmt l (hs ++ hs') (kindPred k x') p
    NamingJmt l hs x v y        ==>  DefExpJmt l hs y object_Noun x   | v = called
                                ==>  DefExpJmt l hs x object_Noun y   | otherwise
    KindProp x k                ==>  kindPred k x
    NecSuffProp c x pr q        ==>  IfProp a q / IfProp q a / IffProp a q   (necessary / sufficient / both), a = predProp pr x
    AdjKind a k                 ==>  SuchThatKind k z (AdjProp a z)      | z fresh (_g0, ...)
    ...
      where liftIndef (QuantExp (IndefIdentKindQuant x k)) = ([VarHypo x k], x)
            kindPred (NounKind n) x = Noun1Prop (NounNoun1 n) x
            kindPred (AdjKind a k) x = AndProp [kindPred k x, AdjProp a x]
            kindPred (ExpKind s) x = Noun2Prop element_Noun2 x s ...

### S5. sem

Judgements and hypotheses:

    ExtensionJmt l k par x [aks] p       ==>  StructureJmt l (ext x par aks p) k
    ExtensionPropJmt / ExtensionDataJmt  (the same, without data / without props)
    ExtensionHyposJmt l hs k par x aks p ==>  StructureJmt l (params hs ++ ext x par aks p) k
      where ext x par aks p = [VarClause x set_Noun, PropClause (ProofProp (par of x))]
                              ++ [VarClause y (ElemKind k') | y : k' in aks]
                              ++ [PropClause (ProofProp q) | q a conjunct of p]
            params: VarsHypo ys (NounKind n) ↦ VarClause y set_Noun, PropClause (ProofProp (Noun1Prop n y))
                    VarsHypo ys k ↦ VarClause y (ElemKind k);  PropHypo q ↦ PropClause (ProofProp q)
    DefinedAdjJmt l hs e a p             ==>  DefPropJmt l hs (AdjProp a e) p
    ListHypo hs                          ==>  ListHypo (concatMap semHypo (map sem hs))
      where semHypo (AdjKindHypo xs a k)   = VarsHypo xs k, PropHypo (AdjProp a x1), ..., PropHypo (AdjProp a xn)
            semHypo (SuchThatHypo xs p k)  = VarsHypo xs k, PropHypo p
    LetFormulaHypo f                     ==>  VarsHypo xs (ExpKind (TermExp s))     | f* = ElemFormula xs s
                                         ==>  VarsHypo [x] (ExpKind (TermExp s))    | f* = x ∈ s as an equation
                                         ==>  PropHypo (FormulaProp f*)
    LetDeclarationHypo d                 ==>  VarsHypo xs (ExpKind (TermExp s))     | d* = ElemDeclaration xs s
    LetFormulaSuchThatHypo f p           ==>  SuchThatHypo xs p (ExpKind (TermExp s))*   | f* = ElemFormula xs s

Propositions:

    BoundedInAllProp f s p      ==>  CoreAllProp (ExpKind s) x (IfProp (FormulaProp f) p)*     | x the variable of f
    BoundedInExistProp f s p    ==>  CoreExistProp (ExpKind s) x (AndProp [FormulaProp f, p])*
    IfProp (AndProp [p1, ..., pn]) q  ==>  (IfProp p1 (... (IfProp pn q)))*       -- curried
    IfProp a b                  ==>  IfProp a* b*
    OnlyIfProp a b              ==>  (IfProp a b)*
    FormulaImpliesProp f g      ==>  (IfProp (FormulaProp f) (FormulaProp g))*
    ExistNoProp aks p           ==>  CoreNotProp (ExistProp aks p)*
    PostQuantProp p q           ==>  CoreAllProp k x p*                  | q* = EveryIdentKindQuant x k
                                ==>  CoreExistProp k x p*                | q* = IndefIdentKindQuant x k
                                ==>  ExistProp [IdentsArgKind k xs] p*   | q* = SomeIdentsKindQuant xs k
                                ==>  AllProp [IdentsArgKind k xs] p*     | q* = AllIdentsKindQuant xs k
                                ==>  AllProp [IdentsArgKind k xs] (CoreNotProp p)*  | q* = NoIdentsKindQuant xs k
    AdjProp a (QuantExp (AllIdentsKindQuant [x] k))   ==>  AllProp [IdentsArgKind k [x]] (AdjProp a x)*
    AdjProp a (QuantExp (EveryIdentKindQuant x k))    ==>  (the same)
    AdjProp a (QuantExp (SomeIdentsKindQuant [x] k))  ==>  ExistProp [IdentsArgKind k [x]] (AdjProp a x)*
    AdjProp a (QuantExp (IndefIdentKindQuant x k))    ==>  (the same)
    AdjProp a (QuantExp (NoIdentsKindQuant [x] k))    ==>  AllProp [IdentsArgKind k [x]] (NotAdjProp a x)*
    AdjProp a (QuantExp (EveryKindQuant k))  ==>  AllProp [IdentsArgKind k [h]] (AdjProp a h)*    | h fresh (_h0, ...)
    AdjProp a (QuantExp (AllKindQuant k))    ==>  (the same)
    AdjProp a (QuantExp (SomeKindQuant k))   ==>  ExistProp [IdentsArgKind k [h]] (AdjProp a h)*
    AdjProp a (QuantExp (IndefKindQuant k))  ==>  (the same)
    AdjProp a (QuantExp (NoKindQuant k))     ==>  AllProp [IdentsArgKind k [h]] (NotAdjProp a h)*
    AdjProp (AndAdj [a1, ..., an]) e  ==>  AndProp [AdjProp a1 e*, ..., AdjProp an e*]     (OrAdj: OrProp)
    AdjProp (Adj2Adj a e2) e          ==>  Adj2Prop a e* e2*       (Adj3Adj: Adj3Prop)
    AdjProp a (AndExp [e1, ..., en])  ==>  AndProp [AdjProp a e1, ..., AdjProp a en]       (OrExp: OrProp)
    AdjCCollProp a [e1, e2]           ==>  AdjCProp a e1* e2*
    AdjECollProp a [e1, ..., en]      ==>  AndProp [AdjEProp a e1 e2, AdjEProp a e2 e3, ..., AdjEProp a en-1 en]
    BothAndProp a b                   ==>  AndProp [a*, b*]   (BothAndAdj, BothAndExp: AndAdj, AndExp)
    EitherOrProp a b                  ==>  OrProp [a*, b*]    (EitherOr...: Or...)
    NotAdjProp a e                    ==>  CoreNotProp (AdjProp a e)*
      ... and so for every Not...Prop of pass 10
    DisplayFormulaProp f              ==>  (FormulaProp f)*
    FormulaProp (ChainEquation c s0 eq) ==>  AndProp [FormulaProp (s0 c s1), ..., FormulaProp (sn-1 cn sn)]*
    FormulaProp (ElemFormula [s] s')  ==>  Noun2Prop element_Noun2 s s'
    FormulaProp (ElemFormula ss s')   ==>  AndProp [FormulaProp (ElemFormula [s] s') | s in ss]*

Kinds, arguments and terms:

    FunExpKind e v              ==>  FunKind [KindArgKind (ExpKind e)] (ExpKind v)*
    KindArgKind k               ==>  IdentsArgKind k* [h]           | h fresh
    PluralKindExp k             ==>  KindExp k
    times_Term s1 s2            ==>  Oper2Term times_Oper2 s1* s2*
    ParenthTerm s               ==>  s*
    (and the Naproche constructions: SupposePropHypo, IffIffProp, WeHaveProp,
     NoCommaAllProp, BareIdentsArgKind, DeclarationArgKind, NoCommaExistProp,
     NoArticleExistProp, WeDefineAdjJmt)

### S6. addParenth: lists back to binary connectives, quantifier lists unfolded

    AndProp [p1, ..., pn]   ==>  CoreAndProp (... (CoreAndProp p1* p2*) ...) pn*     -- foldl1: left-nested
    OrProp  (the same, CoreOrProp)
    IfProp a b              ==>  CoreIfProp a* b*
    IffProp a b             ==>  CoreIffProp a* b*
    AllProp aks p           ==>  CoreAllProp k1 x1 (... (CoreAllProp kn xn p*))
                                 with CoreIfProp c (...) for a FormulaArgKind k f (c = FormulaProp f)
    ExistProp aks p         ==>  CoreExistProp ... with CoreAndProp c (...) for a FormulaArgKind

### S7. addCoercions

    AxiomJmt l hs p        ==>  AxiomJmt l hs* (ProofProp p)        -- unless p is a ProofProp
    ThmJmt l hs p pr       ==>  ThmJmt l hs* (ProofProp p) pr
    AxiomExpJmt l hs e k   ==>  AxiomExpJmt l hs* e (ElemKind k)
    PropHypo p             ==>  PropHypo (ProofProp p)
    VarsHypo xs k          ==>  VarsHypo xs (ElemKind k)            -- unless k is an ElemKind

### S8. inSituResults (Cooper storage)

    Adj2Prop, Adj3Prop, Verb2Prop, Noun2Prop, Noun3Prop, NegNoun2Prop, AdjC3Prop,
    Adv2Prop, VerbProp, Noun1Prop with QuantExp q1, ..., QuantExp qn as arguments
        ==>>  for each permutation of the quantifiers (and a negation on top):
              the prefixes CoreAllProp / CoreExistProp k _x_i ... over the prop
              with _x_i in the places of the QuantExps
    AxiomJmt l hs p   ==>>  AxiomJmt l hs p'   | p' in inSituResults p

---

## Part 3. Pairing: does semantics undo NLG?

For each NLG rule, the semantics that undoes it, and whether
`t ∈ sem(nlg(t))`. Four verdicts:

- **exact**: the semantics gives back `t`;
- **equivalent**: it gives a tree that differs from `t` only in a way that
  is logically equivalent (associativity, currying, alpha-renaming, order of
  independent hypotheses, ∀ over ∧, ¬∃ as ∀¬): the specification fails
  literally, but a normalization on both sides would make it hold;
- **lost**: the semantics does not give back `t`, nor a tree equivalent to
  it: it gives a different meaning, or keeps an Informath construction that
  `MathCore2Dedukti` does not translate;
- *to be checked*: reading the code does not settle it.

### Exact

| NLG | undone by |
|---|---|
| 1 unparenth `CoreIfProp`, `CoreIffProp` | S6 `IfProp`, `IffProp` |
| 1 unparenth `CoreAndProp a b`, `CoreOrProp a b` (when no flatten follows) | S6 |
| 3 funKinds, one argument: `FunExpKind e v` | S5 `FunExpKind` (but see F3) |
| 6 aggregate: `NotAdjProp` | S5 `NotAdjProp` |
| 6 groupProps: `AndAdj`, `OrAdj`, `AndExp`, `OrExp`, chains of equations | S5 |
| 6 bounded quantifiers | S5 `BoundedIn...`, S6 `FormulaArgKind` |
| 6 `CoreAllProp`/`CoreExistProp` nested → one `AllProp`/`ExistProp` | S6 |
| 7 insitu `EveryIdentKindQuant`, `SomeIdentsKindQuant`, `NoIdentsKindQuant` (from ∀¬) | S5 |
| 10 negated | S5 `Not...Prop` |
| 12 scoped `BothAndProp`, `EitherOrProp` | S5 |
| 12' letDeclared | S5 `LetDeclarationHypo`, `LetFormulaSuchThatHypo` |
| 13 `LetDeclarationHypo`, `PostQuantProp`, `ExistNoProp`, `OnlyIfProp`, `FormulaImpliesProp` of two formulas, `DisplayFormulaProp`, `times_Term`, `BothAnd...`, `EitherOr...` | S5 |
| 16 `calledDefinition`, `necSuff`, `adjKinds` (one order) | S4 |

### Findings

**F1. Conjunctive antecedents are curried** (equivalent). NLG keeps "if A
and B, then C" as `IfProp (AndProp [a, b]) c`; S5 rewrites it to
`IfProp a (IfProp b c)`. From `t = CoreIfProp (CoreAndProp a b) c` the
semantics gives only `CoreIfProp a (CoreIfProp b c)`.

**F2. Associativity of ∧ and ∨ is reversed** (equivalent). Dedukti's
`and a (and b c)` becomes `CoreAndProp a (CoreAndProp b c)`, flattened to
`AndProp [a, b, c]` (passes 1, 5, and `scoped`'s splicing); S6 folds a list
to the left, `CoreAndProp (CoreAndProp a b) c`. Every conjunction or
disjunction of three or more comes back re-bracketed.

**F3. Coercions come back only in some places** (lost or equivalent, *to be
checked*). Pass 2 removes every `ProofProp`, `ElemKind` and `CoercionExp`;
S7 puts `ProofProp`/`ElemKind` back only on the top of `AxiomJmt`, `ThmJmt`,
`AxiomExpJmt`, and on `PropHypo` and `VarsHypo` (its comment: "TODO: check
where exactly coercions are needed"). Not restored: the kinds of other
judgements (`DefExpJmt`, `AxiomKindJmt`, ...), the arguments of `FunKind`,
the hypotheses inside `AllProp`/`ExistProp`, and `CoercionExp` anywhere.
And S7 adds `ElemKind` also where `t` had none: a set variable,
`(X : Set)`, is `VarsHypo [X] (NounKind set_Noun)` in `t` but
`VarsHypo [X] (ElemKind (NounKind set_Noun))` after S7.

**F4. Unnamed arguments get names** (equivalent). `Dedukti2MathCore` makes
an argument of a function type without a variable `KindArgKind k`; S5
rewrites every `KindArgKind k` to `IdentsArgKind k [_h0]`. So `FunKind
[KindArgKind k] v` in `t` comes back as `FunKind [IdentsArgKind k [_h0]] v`,
whether NLG touched it or not.

**F5. Symbolic function types** (*to be checked*). Pass 3 with two or more
arguments, and pass 13, give `ExpKind (TermExp (FunTypeTerm s1 s2))`, "$A
\rightarrow B$"; no semantic rule makes a `FunKind` of it again.
`MathCore2Dedukti` translates `FunTypeTerm` to a Dedukti arrow between the
terms, `A -> B`, where `t` had `Elem A -> Elem B`, so the round trip may
differ in Dedukti too.

**F6. Hypotheses are regrouped and reordered** (equivalent, if the order
does not matter). `aggregateHypos` moves a variable next to the last one of
its kind (`reorderVar`), and merges the propositions attached to variables
(`attachProps`) and two remaining ones (`pairProps`) into one `AndProp`.
S5 splits a `SuchThatHypo` into one `VarsHypo` and *one* `PropHypo`, and an
`AdjKindHypo` into one `PropHypo` per variable. So `p, q : ℕ, prime p ∧ prime
q` (one hypothesis) comes back as two hypotheses `prime p`, `prime q`
(through `AdjKindHypo`), and `x > 0`, `y > 0` (two hypotheses) come back as
one, `x > 0 ∧ y > 0` (through `SuchThatHypo`). The order of the variables is
not restored either.

**F7. "Let G be a monoid" changes the type of G** (lost). `nounKinds` and
`attached` turn `VarsHypo [G] set_Noun` with a hypothesis `Noun1Prop monoid G`
into `VarsHypo [G] (NounKind monoid_Noun)`, and drop the hypothesis. S5 keeps
`VarsHypo [G] (NounKind monoid_Noun)`, and S7 makes it `ElemKind (NounKind
monoid_Noun)`: G is an *element* of the kind "monoid" rather than a set of
which "is a monoid" is assumed. In Dedukti, `G : Elem monoid` for `G : Set,
_ : Proof (Monoid G)`. The same for `typedHypos` in `GodementNLG`
(`VarHypo x (NounKind n)`). This is the most important finding: the reading
is right for a reader, but its semantics is a different formalization.

**F8. Proof labels are dropped** (equivalent for statements). Pass 8 turns
`PropVarHypo h p` into `PropHypo p`; the semantics cannot know `h`.

**F9. Quantifier variables are renamed** (equivalent). Pass 8 drops the
variable of a quantifier in place ("every natural number is ..."); S5 gives
it a fresh name `_h0`.

**F10. ∀ distributed over ∧ is not undone** (equivalent). `groupProps` makes
`∀x P ∧ ∀x Q` into `∀x (P ∧ Q)`; the semantics keeps `∀x (P ∧ Q)`.

**F11. ¬∃ comes back as ∀¬** (equivalent). `insitu` makes `¬∃x A(x)` into "no
x is A" (`NoIdentsKindQuant`), which S5 reads as `∀x ¬A(x)`.
(And the `insitu` rule for `∀x ¬A(x)` never applies: `aggregate`, before it,
has already made the `CoreNotProp (AdjProp ...)` a `NotAdjProp`.)

**F12. "A implies B" between propositions that are not formulas** (lost).
Pass 11 turns propositions into formulas: `IdentProp A` into `MacroFormula
A`, `CoreNotProp a` into `NotFormula`, `AndProp` into `AndFormula`, ...
S5 turns `FormulaImpliesProp f g` into `IfProp (FormulaProp f) (FormulaProp
g)`, but nothing turns `NotFormula`, `AndFormula`, `IfFormula`, `IffFormula`,
`OrFormula` or a `MacroFormula` of a propositional variable back into
propositions, and `MathCore2Dedukti` has no case for them.

**F13. Several constructions have no semantics** (lost). NLG produces them,
but neither `Informath2MathCore` nor `MathCore2Dedukti` mentions them:
`BareAllProp` and `BareExistProp` (pass 15, "for all x"), `FunCCollExp`
(pass 9, "the sum of a, b and c"), `sum3dots_Term` (pass 13, "$f(1) + f(2) +
\dots + f(n)$"; the code that would guess the term back, `iqTest`, is
commented out), and `VarHypo` (pass 16).

**F14. Collected equivalences are re-paired** (equivalent only by
transitivity and symmetry). `collectivize` makes `a ~ b ∧ a ~ c` into "a, b
and c are equivalent" (`AdjECollProp [a, b, c]`), which S5 reads as `a ~ b ∧
b ~ c`: a different conjunction, equivalent only because the relation is an
equivalence.

**F15. Structures** (*to be checked*). `extendStructure` with the parent as
the carrier's kind (`VarClause x (NounKind n)`) comes back, through
`extensionStructure`, as `VarClause x set_Noun, PropClause (Noun1Prop n x)`
(the F7 situation the other way round). A single property that is itself a
conjunction is split into several clauses.

**F16. GodementNLG moves content between definiens and hypotheses** (lost,
or equivalent only with logic). `typedDefinition` takes the typing conjunct
out of the definiens, "x is commutative if x is a group and P" becoming "a
group x is said to be commutative if P", which S4 reads as a definition with
the hypothesis "x is a group": the definition of commutativity for groups
rather than for all objects. `definedExp` replaces the definiendum's kind by
`object_Noun` (S4 `NamingJmt`). `typedQuantifiers` moves the typing from an
implication into the kind (as F7).

**F17. Moving hypotheses into the conclusion is not undone** (equivalent).
The `AxiomJmt` rule of pass 13 makes "Let n be a natural number. Assume P.
Then Q" into "for all natural numbers n, if P then Q" with fewer hypotheses;
the semantics keeps the quantified conclusion. In Dedukti this is
`Proof (forall Nat (n => if P Q))` for `(n : Elem Nat) -> Proof P -> Proof Q`.

**F18. #NLG and #SEMANTICS rules are not checked against each other.** A
symbol table may give a function an NLG variant (pass 14) without a
semantics rule (S1) that undoes it.

---

## Status

The empirical check of Part 4 is `RunInformath -check-nlg-semantics`, and its
results are in [nlg-semantics-check.md](nlg-semantics-check.md). Fixed after
it: F5, F7 (`ClassKind`), F12, and `FunCCollExp` of F13; the variants of the
test corpora are all exact or equivalent, and of 87,537 variants of eight
Mathlib modules 45 are lost (a table wording with "an").

Decided on 12 November: the specification is taken **modulo the
normalization** of `NLGCheck.normalize` (t is in the semantics of t' up to
it), and `ClassKind` stays. A proof of the specification is then a proof,
pass by pass, that the semantics undoes each NLG rule up to the
normalization, together with a proof that each step of the normalization
preserves meaning.

## Part 4. Next steps

1. **An empirical check.** A mode of `RunInformath` that, for each judgement
   of a corpus (`test/exx.dk`, `test/top100.dk`, `test/sets.dk`, the Mathlib
   statements), computes `t = dedukti2core j`, every NLG variant `t'` of `t`
   (with `-more-variants`, so that each pass is covered), and checks whether
   `t ∈ semantics t'`, reporting each failure with the passes that made `t'`.
   Comparing modulo a normalization (flattened ∧/∨, uncurried antecedents,
   alpha-renaming, sorted independent hypotheses, coercions removed) would
   separate the *equivalent* failures from the *lost* ones, and confirm or
   refute the findings marked *to be checked*.
2. **The lost cases first:** F7 (kinds from nouns), F12 (formulas from
   propositions), F13 (constructions without semantics), F16.
3. **Then a decision on the equivalent ones:** either make the semantics
   undo them (`addParenth` folding to the right, no currying in S5, restoring
   coercions where `Dedukti2MathCore` puts them), or weaken the
   specification to equality modulo the normalization.

---

## Part 5. Information loss: what is ambiguous, what can be restored, what cannot

The specification (t is among the semantics of t', modulo the normalization)
says nothing about *how much* NLG loses. A step can lose information in four
ways, by how it could be restored:

- **E, equivalent**: t' means the same as t, but a canonical choice (the
  normalization) is needed to get t back literally. Harmless for meaning;
  but the Dedukti type of a constant is not the same, so its uses elsewhere
  (proofs applying it) do not type check against the round-trip.
- **A, ambiguous**: the text has several readings, t among them; something
  outside the text must choose.
- **T, restorable by type checking**: the missing information is determined
  by the types (dependent types, the expected type of an argument): a type
  checker with metavariables and unification, or with coercion insertion,
  would restore it. Informath's semantics does not do this yet; Dedukti
  needs it as explicit terms.
- **S, restorable by the symbol tables**: a table says how (`#IMPLICIT`,
  `#SEMANTICS`, the profiles of entries).
- **X, impossible**: the information is gone; neither the text, nor the
  types, nor the tables determine it.

| step | what is lost | class | `-roundtrip-safe` |
|---|---|---|---|
| 1 unparenth, 5 flatten, `scoped` splicing (F2) | the bracketing of ∧, ∨ | E (associativity) | kept |
| S5 currying of "if A and B" (F1) | one antecedent or two | E | kept |
| 2 uncoerce: `ProofProp`, `ElemKind` (F3) | where `Proof`/`Elem` stand | T (the judgement form and the argument types tell) | kept |
| 2 uncoerce: `CoercionExp` (F3; Mathlib `#COERCION`) | the cast ℕ → ℤ, `Subtype.val`, ... | T (only with coercion insertion by expected type) | kept |
| 6 `nounKinds`/`attached` with `ClassKind` (F7) | NLG keeps it (`ClassKind`), but a *parsed* text reads "let G be a monoid" with `NounKind`: `G : Elem monoid` | T (`Elem Monoid` is ill-typed where `Monoid` is a predicate) | only when the class hypothesis is right after its variable |
| 6 `reorderVar` (F6) | the order of the hypotheses | X (the arguments of the constant change places) | off |
| 6 `pairProps`, `attachProps` with two or more propositions (F6) | how many hypotheses: "such that A and B" is one hypothesis A ∧ B or two | A, not decidable by types (both are well typed): X | off: one proposition attached at most |
| 6 `AdjKindHypo` (F6) | "let x, y be even": one hypothesis per variable, or one conjunction | A → X | only from one hypothesis per variable, in their order |
| 6 groupProps ∀ over ∧ (F10) | `∀x P ∧ ∀x Q` or `∀x (P ∧ Q)` | E | kept |
| 7 insitu, 13 `PostQuantProp` | none in NLG, which places one quantifier; but the scope of "P for all x" after a connective, and of quantifiers in table wordings (Cooper storage, S8), is read in several ways | A (S8 gives all orders) | kept; see item 1 of nlg-semantics-check.md for table wordings |
| 8 varless (F8, F9) | the names of bound variables and of proof labels | E for statements (alpha); X for the *names* of labels a proof cites, which are restored as positions only | kept |
| 9 collectivize `AdjECollProp` (F14) | which pairs: "a, b and c are equivalent" for a ~ b ∧ a ~ c reads as a ~ b ∧ b ~ c | X (equivalent only by the laws of an equivalence relation) | only when the semantics gives back the same pairs (a chain) |
| 9 collectivize `FunCCollExp` | the bracketing of a + (b + c) | X (equivalent only by associativity, which is a theorem, not a conversion) | only when nested to the left, as the semantics folds |
| 11 implied (F12) | none since `formulaProp` | — | kept |
| 13 `AxiomJmt` hypotheses into the conclusion (F17) | how many hypotheses | E logically, X for the type of the constant | off |
| 13 `sum3dots_Term` (F13) | the general term of f(1) + … + f(n), guessed from three instances | X | off |
| 14 `#NLG` rules (F18) | whatever the rule drops | S if a `#SEMANTICS` rule inverts it, else X | off (not checkable) |
| 15 bareQuantifiers (F13) | the kind of "for all x" | S (`#IMPLICIT`), when one kind is implicit; not implemented in the semantics | off |
| 16 GodementNLG `typedHypos`, `typedQuantifiers` (F7) | set + typing as a kind | T, as `ClassKind` | off |
| 16 `typedDefinition` (F16) | a conjunct of the definiens moved into the hypotheses | X (a definition for groups instead of for all objects) | off |
| 16 `definedExp` (F16) | the kind of the definiendum (`object_Noun`) | X | off |
| 16 `calledDefinition`, `adjKinds`, `necSuff` | none | — | kept |
| annotation: one wording for several constants (`backConstantTable`) | which constant: "is nonempty" for `Nonempty`, `Set.Nonempty`, `Finset.Nonempty`; ⊆ for `Set.Subset`, `Finset.Subset` | A, T (the types of the arguments choose) | kept (not NLG) |
| annotation: `#HIDE`, `#DROP` and profiles that leave out arguments | the arguments | T if they are determined by the types of the others (implicit type arguments, instances); X for data or proofs that are not; `unappProfile` puts metavariables in their places | kept (not NLG) |
| annotation: a `PermProfile` that leaves out the *last* arguments (`[1]` of a two-argument constant) | the arguments after the highest shown position | X as implemented: `unappProfile` makes a partial application, without metavariables for them; T if it added them | kept (not NLG); to be fixed |
| `dropImplicitHypos` (`#IMPLICIT`, ProofText) | the leading hypotheses of an implicit type | S: the free variables of the statement get the implicit type | kept |

### The flag `-roundtrip-safe`

`RunInformath -roundtrip-safe` leaves out the steps marked "off" above and
restricts those marked "only": the NLG output is then one whose semantics
gives back the same hypotheses, in the same order and number, the same
collective structures, and no construction without a semantics. What
remains is class E (a normalization), A of quantifier scope and of
wordings, and T and S (types and tables). It is implemented in
`MathCore2Informath` (`aggregateSafe`, `aggregateHyposSafe`,
`collectivizeSafe`, `restorableVariation`), `GodementNLG`
(`godementVariantsSafe`) and `InformathAPI` (no `implicitKinds`); the check
of nlg-semantics-check.md follows it with the flag.

The effect on the test corpora (the default output, one line per judgement):
`test/exx.dk` 1 of 18 lines differs, `test/top100.dk` 4 of 27,
`test/sets.dk` 1 of 32; for example

    Let A and B be sets such that A ∪ B = 𝕌 and A ∩ B = ∅. Then B = Aᶜ.     (default)
    Let A and B be sets. Assume that A ∪ B = 𝕌. Assume that A ∩ B = ∅. Then B = Aᶜ.   (-roundtrip-safe)

as the Dedukti statement has the two hypotheses as two arguments. The
variants of the check (`-check-nlg-semantics`, all stages): exx 359 → 247,
top100 653 → 479, sets 615 → 245, none lost with or without the flag; the
exact ones are the same (150, 182, 10), as the others are still class E.

To do: the semantics of bare quantifiers from `#IMPLICIT`, metavariables
for trailing arguments in `unappProfile`, and a class-E-free round trip
(fold ∧ to the right, no currying, coercions where `Dedukti2MathCore` puts
them), which would make `-roundtrip-safe` give exact round trips.
