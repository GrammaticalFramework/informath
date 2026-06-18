abstract MathExtensions =
  Categories,
  Terms
  ** {

cat
  [Adj] {2} ;
  Pred ;
----  [Pred] {2} ;
  Quant ;

fun
  FormulaProp : Formula -> Prop ;
  DisplayFormulaProp : Formula -> Prop ;
  FormulaImpliesProp : Formula -> Formula -> Prop ;

-- to introduce lists of idents
  IdentsArgKind : Kind -> [Ident] -> ArgKind ;
    
  VarsHypo : [Ident] -> Kind -> Hypo ;
  BareVarsHypo : [Ident] -> Hypo ;  -- needed in proofs: let x be arbitrary
  AdjKindHypo : [Ident] -> Adj -> Kind -> Hypo ;
  
  AbsExp : [Ident] -> Exp -> Exp ;

-- comprehension with verbal condition

  ComprehensionTextTerm : Term -> Ident -> Prop -> Term ;

-- to remove parentheses around complex propositions and introduce lists
  AndProp : [Prop] -> Prop ;
  OrProp : [Prop] -> Prop ;
  IfProp : Prop -> Prop -> Prop ;
  IffProp : Prop -> Prop -> Prop ;
  AllProp : [ArgKind] -> Prop -> Prop ;
  ExistProp : [ArgKind] -> Prop -> Prop ; 
  ExistNoProp : [ArgKind] -> Prop -> Prop ;

  NotAdjProp : Adj -> Exp -> Prop ;
  NotAdj2Prop : Adj2 -> Exp -> Exp -> Prop ;
  NotAdjCProp : AdjC -> [Exp] -> Prop ;
  NotAdjEProp : AdjE -> [Exp] -> Prop ;
  NotNoun1Prop : Noun1 -> Exp -> Prop ;
  NotNoun2Prop : Noun2 -> Exp -> Exp -> Prop ;
  NotNounCProp : NounC -> [Exp] -> Prop ;
  NotVerb2Prop : Verb2 -> Exp -> Exp -> Prop ;
  NotVerbCProp : VerbC -> [Exp] -> Prop ;
  NotVerbProp : Verb -> Exp -> Prop ;
  NotAdvProp : Adv -> Exp -> Prop ;
  NotAdv2Prop : Adv2 -> Exp -> Exp -> Prop ;
  NotAdvCProp : AdvC -> [Exp] -> Prop ;

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

  OnlyIfProp : Prop -> Prop -> Prop ;    -- A only if B <-> if A then B
  InverseIfProp : Prop -> Prop -> Prop ; -- B if A <-> if A then B

  Adj2Adj : Adj2 -> Exp -> Adj ;
  Adj3Adj : Adj3 -> Exp -> Exp -> Adj ;

-- for indexed parsing (terms in $...$ stored in a dictionary)

  IndexedTermExp : Int -> Exp ;
  IndexedFormulaProp : Int -> Prop ;
  IndexedLetFormulaHypo : Int -> Hypo ;

-- for Pathak's examples (and more)

  LetFormulaHypo : Formula -> Hypo ;
  PostQuantProp : Prop -> Quant -> Prop ;

  LetDeclarationHypo : Declaration -> Hypo ;

  DefinedAdjJmt : Label -> [Hypo] -> Exp -> Adj -> Prop -> Jmt ;
  WeDefineAdjJmt : Label -> [Hypo] -> Exp -> Adj -> Prop -> Jmt ;

  PluralKindExp : Kind -> Exp ; -- the rationals


  QuantExp : Quant -> Exp ;
  
  AllKindQuant : Kind -> Quant ;
  AllIdentsKindQuant : [Ident] -> Kind -> Quant ;
  EveryKindQuant : Kind -> Quant ;
  EveryIdentKindQuant : Ident -> Kind -> Quant ;

  IdentsArgKind : Kind -> [Ident] -> ArgKind ;

  SomeKindQuant : Kind -> Quant ;
  SomeIdentsKindQuant : [Ident] -> Kind -> Quant ;
  IndefKindQuant : Kind -> Quant ;
  IndefIdentKindQuant : Ident -> Kind -> Quant ;

  NoIdentsKindQuant : [Ident] -> Kind -> Quant ;
  NoKindQuant : Kind -> Quant ;

  AdjCCollProp : AdjC -> [Exp] -> Prop ;
  AdjECollProp : AdjE -> [Exp] -> Prop ;
  FunCCollExp : FunC -> [Exp] -> Exp ;
  AdvCCollProp : AdvC -> [Exp] -> Prop ;

-- predicates as a general category
{- ----
  PredProp : Pred -> Exp -> Prop ;
  AdjPred : Adj -> Pred ;
  VerbPred : Verb -> Pred ;
  Noun1Pred : Noun1 -> Pred ;
  KindPredKind : Kind -> Pred -> Kind ;
-}

}