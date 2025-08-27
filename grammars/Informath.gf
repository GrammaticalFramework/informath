abstract Informath =
  MathCore
  ** {

flags startcat=Jmt ;

cat
  [Adj] {2} ;
  [Exp] {2} ;

fun
  FormulaProp : Formula -> Prop ;
  DisplayFormulaProp : Formula -> Prop ;
  FormulaImpliesProp : Formula -> Formula -> Prop ;

  SetTerm : Set -> Term ;
  ConstTerm : Const -> Term ;
  ComparEqsign : Compar -> Eqsign ;
  ComparnounEqsign : Comparnoun -> Eqsign ;
  AppOperTerm : Oper -> Term -> Term -> Term ;
  AppOperOneTerm : Oper -> Term -> Term ;

-- to remove parentheses around complex propositions
  SimpleAndProp : [Prop] -> Prop ;
  SimpleOrProp : [Prop] -> Prop ;
  SimpleIfProp : Prop -> Prop -> Prop ;
  SimpleIffProp : Prop -> Prop -> Prop ;

  AndAdj : [Adj] -> Adj ;
  OrAdj : [Adj] -> Adj ;

  AndExp : [Exp] -> Exp ;
  OrExp : [Exp] -> Exp ;

  OnlyIfProp : Prop -> Prop -> Prop ;

  ExistNoProp : [ArgKind] -> Prop -> Prop ;

  ReladjAdj : Reladj -> Exp -> Adj ;
  Pred3Adj : Pred3 -> Exp -> Exp -> Adj ;

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

  SomeKindExp : Kind -> Exp ;
  SomeIdentsKindExp : [Ident] -> Kind -> Exp ;
  IndefKindExp : Kind -> Exp ;
  IndefIdentKindExp : Ident -> Kind -> Exp ;
----  SomeIdentsSetExp : [Ident] -> Set -> Exp ;

  NoIdentsKindExp : [Ident] -> Kind -> Exp ;
  NoKindExp : Kind -> Exp ;
----  NoIdentsSetExp : [Ident] -> Set -> Exp ;

}