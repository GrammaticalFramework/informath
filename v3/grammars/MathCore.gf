abstract MathCore =
  Terms,
  UserConstants   -- notations, BaseConstants + user-defined with MkConstants.hs
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

  RewriteJmt : [Rule] -> Jmt ;
  RewriteRule : [Ident] -> Exp -> Exp -> Rule ; ---- generalize to [] and x:A
  NoVarRewriteRule : Exp -> Exp -> Rule ;

  PropHypo : Prop -> Hypo ;
  VarsHypo : [Ident] -> Kind -> Hypo ;
  BareVarsHypo : [Ident] -> Hypo ;  -- needed in proofs: let x be arbitrary

  AppExp : Exp -> Exps -> Exp ;
  AbsExp : [Ident] -> Exp -> Exp ;
  TermExp : Term -> Exp ;
  TypedExp : Exp -> Kind -> Exp ;

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

  AppProof : [Proof] -> Exp -> Proof ;
  AbsProof : [Hypo] -> Proof -> Proof ;

  OneExps : Exp -> Exps ;
  AddExps : Exp -> Exps -> Exps ;

-- using Constants

  AdjProp : Adj -> Exp -> Prop ;
  NotAdjProp : Adj -> Exp -> Prop ;
  RelAdj : Rel -> Exp -> Adj ;
  NounKind : Noun -> Kind ;
  SetKind : Set -> Kind ;
  NameExp : Name -> Exp ;
  FunListExp : Fun -> Exps -> Exp ;
  LabelExp : Label -> Exp ;
  ConstExp : Const -> Exp ;
  OperListExp : Oper -> Exps -> Exp ; -- binary operation applied collectively
  ComparAdj : Compar -> Exp -> Adj ;

-- coercions, to disappear in Core2Informath
-- their purpose is to maintain lossless rendering of Dedukti
-- only few are needed if Number types are identified following Ganesalingam

  ProofProp : Prop -> Prop ;
  ElemKind : Kind -> Kind ;

  CoercionExp : Coercion -> Exp -> Exp ;

}