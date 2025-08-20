abstract MathCore =
  Terms,
  VerbalConstants
  ** {

flags startcat = Jmt ;

cat
  Jmt ;
  Exp ;
  Exps ;
  Prop ;
  [Prop] {2} ;
  Kind ;
  ArgKind ;
  [ArgKind] {1} ;
  Hypo ;
  [Hypo] ;
  [Ident] {1} ;
  Proof ;
  [Proof] {0} ;
  ProofExp ;
  Rule ;
  [Rule] {1} ;
  Coercion ;

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
  VarsHypo : [Ident] -> Kind -> Hypo ;
  BareVarsHypo : [Ident] -> Hypo ;  -- needed in proofs: let x be arbitrary
  LetHypo : Ident -> Kind -> Exp -> Hypo ;
  BareLetHypo : Ident -> Exp -> Hypo ;

  AppExp : Exp -> Exps -> Exp ;
  AbsExp : [Ident] -> Exp -> Exp ;
  TermExp : Term -> Exp ;
  KindExp : Kind -> Exp ;
  TypedExp : Exp -> Kind -> Exp ;
  EnumSetExp : Exps -> Exp ;

  AndProp : [Prop] -> Prop ;
  OrProp : [Prop] -> Prop ;
  IfProp : Prop -> Prop -> Prop ;
  IffProp : Prop -> Prop -> Prop ;
  NotProp : Prop -> Prop ;
  FalseProp : Prop ;
  AllProp : [ArgKind] -> Prop -> Prop ;
  ExistProp : [ArgKind] -> Prop -> Prop ; 
  IdentProp : Ident -> Prop ;
  AppProp : Ident -> Exps -> Prop ;

  TermKind : Term -> Kind ;
  SuchThatKind : Ident -> Kind -> Prop -> Kind ;
  AppKind : Ident -> Exps -> Kind ;
  FunKind : [ArgKind] -> Kind -> Kind ;

  KindArgKind : Kind -> ArgKind ;
  IdentsArgKind : Kind -> [Ident] -> ArgKind ;

  StrLabel : String -> Label ; -- to deal with Dedukti labels not in grammar
  noLabel : Label ; -- to deal with unlabelled statements
  axiomLabel : Label ;
  theoremLabel : Label ;
  definitionLabel : Label ;

  AppProof : ProofExp -> [Proof] ->  Proof ;
  AbsProof : [Hypo] -> Proof -> Proof ;

  AppProofExp : ProofExp -> Exps -> ProofExp ;
  AbsProofExp : [Hypo] -> ProofExp -> ProofExp ;

  OneExps : Exp -> Exps ;
  AddExps : Exp -> Exps -> Exps ;

-- using Constants

  AdjProp : Adj -> Exp -> Prop ;
  NotAdjProp : Adj -> Exp -> Prop ;
  ReladjProp : Reladj -> Exp -> Exp -> Prop ;
  NounKind : Noun -> Kind ;
  NameExp : Name -> Exp ;
  FunListExp : Fun -> Exps -> Exp ;
  LabelProofExp : Label -> ProofExp ;
  FamKind : Fam -> Kind -> Kind ;
  Fam2Kind : Fam -> Kind -> Kind -> Kind ;
  VerbProp : Verb -> Exp -> Prop ;
  RelverbProp : Relverb -> Exp -> Exp -> Prop ;
  RelnounProp : Relnoun -> Exp -> Exp -> Prop ;
  NotVerbProp : Verb -> Exp -> Prop ;
  NotRelverbProp : Relverb -> Exp -> Exp -> Prop ;
  NotRelnounProp : Relnoun -> Exp -> Exp -> Prop ;
  Pred3Prop : Pred3 -> Exp -> Exp -> Exp -> Prop ;

-- coercions, to disappear in Core2Informath
-- their purpose is to maintain lossless rendering of Dedukti
-- only few are needed if Number types are identified following Ganesalingam

  ProofProp : Prop -> Prop ;
  ElemKind : Kind -> Kind ;

  CoercionExp : Coercion -> Exp -> Exp ;

}
