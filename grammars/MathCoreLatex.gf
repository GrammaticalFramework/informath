-- MathCore in standard logical notation, as LaTeX formulas.
-- Propositions use the precedences of TermsLatex:
--   0: quantifiers, <->   1: ->   2: \/   3: /\   4: negation and atoms

concrete MathCoreLatex of MathCore =
  CategoriesLatex **
  open
    Formal,
    Prelude
  in {

lin
  -- the statement only: the proof is better shown by -proof-text
  ThmJmt label hypos prop proof =
    judgement "Theorem" label hypos (top prop) ;
  AxiomJmt label hypos prop =
    judgement "Axiom" label hypos (top prop) ;

  DefPropJmt label hypos prop df =
    judgement "Definition" label hypos (top prop ++ ":\\Leftrightarrow" ++ top df) ;
  DefKindJmt label hypos kind df =
    judgement "Definition" label hypos (top kind ++ ":=" ++ top df) ;
  DefExpJmt label hypos exp kind df =
    judgement "Definition" label hypos (top exp ++ ":=" ++ top df ++ "\\in" ++ top kind) ;

  AxiomPropJmt label hypos prop =
    judgement "Axiom" label hypos (top prop ++ "\\ \\mathsf{prop}") ;
  AxiomKindJmt label hypos kind =
    judgement "Axiom" label hypos (top kind ++ "\\ \\mathsf{set}") ;
  AxiomExpJmt label hypos exp kind =
    judgement "Axiom" label hypos (top exp ++ "\\in" ++ top kind) ;

  DefUntypedExpJmt label exp df =
    judgement "Definition" label {s = [] ; isEmpty = True} (top exp ++ ":=" ++ top df) ;

  RewriteJmt rules = bold "Rules" ++ "." ++ math rules ;
  RewriteRule idents patt exp = top patt ++ "\\longrightarrow" ++ top exp ;
  NoVarRewriteRule patt exp = top patt ++ "\\longrightarrow" ++ top exp ;

  PropHypo prop = top prop ;
  PropVarHypo ident prop = top prop ;
  VarHypo ident kind = ident ++ "\\in" ++ top kind ;
  BareVarHypo ident = ident ;
  LocalHypo local = local ;

  LetLocal ident kind exp = ident ++ ":=" ++ top exp ++ "\\in" ++ top kind ;
  BareLetLocal ident exp = ident ++ ":=" ++ top exp ;

  AppExp exp exps = constant (usePrec highest exp ++ parenth exps) ;
  TermExp term = {s = term.s ; p = term.p} ;
  KindExp kind = kind ;
  PropExp prop = prop ;
  TypedExp exp kind = exp ;
  EnumSetExp exps = constant ("\\{" ++ exps ++ "\\}") ;

  CoreAndProp a b = infixl 3 "\\wedge" a b ;
  CoreOrProp a b = infixl 2 "\\vee" a b ;
  CoreIfProp a b = infixr 1 "\\rightarrow" a b ;
  CoreIffProp a b = infixn 0 "\\leftrightarrow" a b ;
  CoreNotProp a = prefix highest "\\neg" a ;
  FalseProp = constant "\\bot" ;
  CoreAllProp kind ident prop = quantifier "\\forall" ident kind prop ;
  CoreExistProp kind ident prop = quantifier "\\exists" ident kind prop ;
  IdentProp ident = constant ident ;
  AppProp ident exps = constant (ident ++ parenth exps) ;
  ExistKindProp kind = prefix highest "\\exists" kind ;

  SuchThatKind kind ident prop =
    constant ("\\{" ++ ident ++ "\\in" ++ top kind ++ "\\mid" ++ top prop ++ "\\}") ;
  FunKind argkinds kind = mkPrec 0 (argkinds.d ++ "\\rightarrow" ++ top kind) ;
  ExpKind exp = exp ;

  KindArgKind kind = {q = usePrec 2 kind ++ "\\rightarrow" ; d = kind} ;
  IdentArgKind kind ident = {q = "\\forall" ++ ident ++ "\\in" ++ top kind ++ "." ; d = kind} ;

  IdentLabel ident = {s = "\\texttt{\\detokenize{" ++ BIND ++ ident ++ BIND ++ "}}" ; isIdent = True} ;
  noLabel = {s = [] ; isIdent = True} ;
  axiomLabel = {s = "Axiom" ; isIdent = False} ;
  theoremLabel = {s = "Theorem" ; isIdent = False} ;
  definitionLabel = {s = "Definition" ; isIdent = False} ;

  AppProof exp proofs = case proofs.isEmpty of {
    True => exp ;
    False => constant (usePrec highest exp ++ parenth proofs.s)
    } ;
  AbsProof hypos proof = mkPrec 0 ("\\lambda" ++ hypos.s ++ "." ++ top proof) ;

  AppProofExp proofexp exps = constant (usePrec highest proofexp ++ parenth exps) ;
  AbsProofExp hypos proofexp = mkPrec 0 ("\\lambda" ++ hypos.s ++ "." ++ top proofexp) ;

  OneExps exp = top exp ;
  ManyExps exps = exps ;

  LabelProofExp label = constant label.s ;

-- coercions are invisible in symbolic notation

  ProofProp prop = prop ;
  ElemKind kind = kind ;
  CoercionExp coercion exp = exp ;

oper
  bold : Str -> Str = \s -> "\\textbf{" ++ BIND ++ s ++ BIND ++ "}" ;
  math : Str -> Str = \s -> "$" ++ s ++ "$" ;

  -- the keyword of the judgement, unless the label gives one, then the
  -- hypotheses as the context of a sequent
  judgement : Str -> {s : Str ; isIdent : Bool} -> {s : Str ; isEmpty : Bool} -> Str -> Str =
    \kw, label, hypos, body ->
      case label.isIdent of {
        True => bold kw ++ label.s ;
        False => bold label.s
        } ++ "." ++
      math (case hypos.isEmpty of {
        True => body ;
        False => hypos.s ++ "\\vdash" ++ body
        }) ;

  quantifier : Str -> Str -> TermPrec -> TermPrec -> TermPrec = \q, x, kind, prop ->
    mkPrec 0 (q ++ x ++ "\\in" ++ top kind ++ "." ++ top prop) ;

}
