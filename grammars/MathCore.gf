abstract MathCore =
  Categories
  ** {

flags startcat = Jmt ;

fun
  ThmJmt : Label -> [Hypo] -> Prop -> Proof -> Jmt ;
  AxiomJmt : Label -> [Hypo] -> Prop -> Jmt ;
  
  DefPropJmt : Label -> [Hypo] -> Prop -> Prop -> Jmt ;
  DefKindJmt : Label -> [Hypo] -> Kind -> Kind -> Jmt ;
  DefExpJmt  : Label -> [Hypo] -> Exp -> Kind -> Exp -> Jmt ;
  
  AxiomPropJmt : Label -> [Hypo] -> Prop -> Jmt ;
  AxiomKindJmt : Label -> [Hypo] -> Kind -> Jmt ;
  AxiomExpJmt  : Label -> [Hypo] -> Exp -> Kind -> Jmt ;

  DefUntypedExpJmt  : Label -> Exp -> Exp -> Jmt ;

  RewriteJmt : [Rule] -> Jmt ;
  RewriteRule : [Ident] -> Exp -> Exp -> Rule ; ---- generalize to [] and x:A
  NoVarRewriteRule : Exp -> Exp -> Rule ;

  PropHypo : Prop -> Hypo ;
  VarHypo : Ident -> Kind -> Hypo ;
  BareVarHypo : Ident -> Hypo ;  -- needed in proofs: let x be arbitrary
  LocalHypo : Local -> Hypo ;
  
  LetLocal : Ident -> Kind -> Exp -> Local ;
  BareLetLocal : Ident -> Exp -> Local ;

  AppExp : Exp -> Exps -> Exp ;
  CoreAbsExp : Ident -> Exp -> Exp ;
  TermExp : Term -> Exp ; 
  KindExp : Kind -> Exp ;
  TypedExp : Exp -> Kind -> Exp ;
  EnumSetExp : Exps -> Exp ;

  CoreAndProp : Prop -> Prop -> Prop ;
  CoreOrProp : Prop -> Prop -> Prop ;
  CoreIfProp : Prop -> Prop -> Prop ;
  CoreIffProp : Prop -> Prop -> Prop ;
  CoreNotProp : Prop -> Prop ;
  FalseProp : Prop ;
  CoreAllProp : Kind -> Ident -> Prop -> Prop ;
  CoreExistProp : Kind -> Ident -> Prop -> Prop ; 
  IdentProp : Ident -> Prop ;
  AppProp : Ident -> Exps -> Prop ;

  IdentKind : Ident -> Kind ; 
  SuchThatKind : Kind -> Ident -> Prop -> Kind ;
  AppKind : Ident -> Exps -> Kind ;
  FunKind : [ArgKind] -> Kind -> Kind ;

  KindArgKind : Kind -> ArgKind ;
  IdentArgKind : Kind -> Ident -> ArgKind ;

  IdentLabel : Ident -> Label ; -- to deal with Dedukti labels not in grammar
  noLabel : Label ; --- to deal with unlabelled statements
  axiomLabel : Label ;
  theoremLabel : Label ;
  definitionLabel : Label ;

  AppProof : ProofExp -> [Proof] ->  Proof ;
  AbsProof : [Hypo] -> Proof -> Proof ;

  AppProofExp : ProofExp -> Exps -> ProofExp ;
  AbsProofExp : [Hypo] -> ProofExp -> ProofExp ;

  OneExps : Exp -> Exps ;
  ManyExps : [Exp] -> Exps ;

-- using Constants

  AdjProp : Adj -> Exp -> Prop ;
  Adj2Prop : Adj2 -> Exp -> Exp -> Prop ;
  AdjCProp : AdjC -> Exp -> Exp -> Prop ;
  AdjEProp : AdjE -> Exp -> Exp -> Prop ;
  NounKind : Noun -> Kind ;
  NameExp : Name -> Exp ;
  FunExp : Fun -> Exp -> Exp ;
  Fun2Exp : Fun2 -> Exp -> Exp -> Exp ;
  FunCExp : FunC -> Exp -> Exp -> Exp ;
  LabelProofExp : Label -> ProofExp ;
  FamKind : Fam -> Kind -> Kind ;
  Fam2Kind : Fam2 -> Kind -> Kind -> Kind ;
  Noun1Prop : Noun1 -> Exp -> Prop ;
  VerbProp : Verb -> Exp -> Prop ;
  Verb2Prop : Verb2 -> Exp -> Exp -> Prop ;
  Noun2Prop : Noun2 -> Exp -> Exp -> Prop ;
  Adj3Prop : Adj3 -> Exp -> Exp -> Exp -> Prop ;

-- coercions, to disappear in Core2Informath
-- their purpose is to maintain lossless rendering of Dedukti
-- only few are needed if Number types are identified following Ganesalingam

  ProofProp : Prop -> Prop ;
  ElemKind : Kind -> Kind ;

  CoercionExp : Coercion -> Exp -> Exp ;

-- extensions of lexicon definable in symbol tables

  NounName : Noun -> Name ;
  DefNounName : Noun -> Name ;

  NounPrepFam : Noun -> Prep -> Fam ;
  NounPrepFam2 : Noun -> Prep -> Prep -> Fam2 ;
  NounPrepFun : Noun -> Prep -> Fun ;
  NounPrepFun2 : Noun -> Prep -> Prep -> Fun2 ;
  NounPrepFunC : Noun -> Prep -> FunC ;
  
  AdjPrepAdj2 : Adj -> Prep -> Adj2 ;
  AdjAdjC : Adj -> AdjC ;
  AdjAdjE : Adj -> AdjE ;
  AdjPrepAdj3 : Adj -> Prep -> Prep -> Adj3 ;

  VerbVerb2 : Verb -> Prep -> Verb2 ;
  
  NounAdjNoun : Noun -> Adj -> Noun ;

  NoPrep : Prep ;
  betweenPrep : Prep ;
  byPrep : Prep ;
  forPrep : Prep ;
  fromPrep : Prep ;
  inPrep : Prep ;
  ofPrep : Prep ;
  onPrep : Prep ;
  toPrep : Prep ;
  withPrep : Prep ;
  
}