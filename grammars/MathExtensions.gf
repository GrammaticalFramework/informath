abstract MathExtensions =
  Categories,
  Terms
  ** {

cat
  [Adj] {2} ;

fun
  TermKind : Term -> Kind ;
  FormulaProp : Formula -> Prop ;
  DisplayFormulaProp : Formula -> Prop ;
  FormulaImpliesProp : Formula -> Formula -> Prop ;

-- to introduce lists of idents
  IdentsArgKind : Kind -> [Ident] -> ArgKind ;
  VarsHypo : [Ident] -> Kind -> Hypo ;
  BareVarsHypo : [Ident] -> Hypo ;  -- needed in proofs: let x be arbitrary
  AbsExp : [Ident] -> Exp -> Exp ;

-- to remove parentheses around complex propositions and introduce lists
  AndProp : [Prop] -> Prop ;
  OrProp : [Prop] -> Prop ;
  IfProp : Prop -> Prop -> Prop ;
  IffProp : Prop -> Prop -> Prop ;
  AllProp : [ArgKind] -> Prop -> Prop ;
  ExistProp : [ArgKind] -> Prop -> Prop ; 

  NotAdjProp : Adj -> Exp -> Prop ;
  NotAdj2Prop : Adj2 -> Exp -> Exp -> Prop ;
  NotAdjCProp : AdjC -> Exps -> Prop ;
  NotAdjEProp : AdjE -> Exps -> Prop ;
  NotNoun1Prop : Noun1 -> Exp -> Prop ;
  NotNoun2Prop : Noun2 -> Exp -> Exp -> Prop ;
  NotVerb2Prop : Verb2 -> Exp -> Exp -> Prop ;
  NotVerbProp : Verb -> Exp -> Prop ;

  AndAdj : [Adj] -> Adj ;
  OrAdj : [Adj] -> Adj ;

  AndExp : [Exp] -> Exp ;
  OrExp : [Exp] -> Exp ;

  BothAndProp : Prop -> Prop -> Prop ;
  EitherOrProp : Prop -> Prop -> Prop ;

  BothAndAdj : Adj -> Adj -> Adj ;
  EitherOrAdj : Adj -> Adj -> Adj ;

  BothAndExp : Exp -> Exp -> Exp ;
  EitherOrExp : Exp -> Exp -> Exp ;

  OnlyIfProp : Prop -> Prop -> Prop ;

  ExistNoProp : [ArgKind] -> Prop -> Prop ;

  Adj2Adj : Adj2 -> Exp -> Adj ;
  Adj3Adj : Adj3 -> Exp -> Exp -> Adj ;

-- for indexed parsing (terms in $...$ stored in a dictionary)

  IndexedTermExp : Int -> Exp ;
  IndexedFormulaProp : Int -> Prop ;
  IndexedLetFormulaHypo : Int -> Hypo ;

-- for Pathak's examples (and more)

  LetFormulaHypo : Formula -> Hypo ;
  PostQuantProp : Prop -> Exp -> Prop ;

  LetDeclarationHypo : Declaration -> Hypo ;

  DefinedAdjJmt : Label -> [Hypo] -> Exp -> Adj -> Prop -> Jmt ;
  WeDefineAdjJmt : Label -> [Hypo] -> Exp -> Adj -> Prop -> Jmt ;

  AdjKind : Adj -> Kind -> Kind ;
  KindProp : Exp -> Kind -> Prop ;

  AllKindExp : Kind -> Exp ;
  AllIdentsKindExp : [Ident] -> Kind -> Exp ;
  EveryKindExp : Kind -> Exp ;
  EveryIdentKindExp : Ident -> Kind -> Exp ;
----  AllIdentsSetExp : [Ident] -> Set -> Exp ; -- TODO: all $x, y : N$

  IdentsArgKind : Kind -> [Ident] -> ArgKind ;

  SomeKindExp : Kind -> Exp ;
  SomeIdentsKindExp : [Ident] -> Kind -> Exp ;
  IndefKindExp : Kind -> Exp ;
  IndefIdentKindExp : Ident -> Kind -> Exp ;
----  SomeIdentsSetExp : [Ident] -> Set -> Exp ;

  NoIdentsKindExp : [Ident] -> Kind -> Exp ;
  NoKindExp : Kind -> Exp ;
----  NoIdentsSetExp : [Ident] -> Set -> Exp ;

  AdjCCollProp : AdjC -> Exps -> Prop ;
  AdjECollProp : AdjE -> Exps -> Prop ;
  FunCCollExp : FunC -> Exps -> Exp ;

}