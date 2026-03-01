incomplete concrete ExamplesFunctor of Examples = Categories **

open
  Syntax,
  MathCore,
  Utilities,
  Symbolic,
  Prelude

in {

lincat
  Example = Utt ;
  Argument = Exp ;
  KindArgument = Kind ;
  BoundVariable = Str ;

lin
  AdjExample adj x = mkExample (AdjProp adj x) ;
  Adj2Example adj x y = mkExample (Adj2Prop adj x y) ;
  AdjCExample adj x y = mkExample (AdjCProp adj x y) ;
  AdjEExample adj x y = mkExample (mkExample (AdjEProp adj x y)) "EQUIVALENCE" ;
  Adj3Example adj x y z = mkExample (Adj3Prop adj x y z) ;

  NounExample noun = mkExample (NounKind noun) ;

  FamExample fam a = mkExample (FamKind fam a) ;
  Fam2Example fam a b = mkExample (Fam2Kind fam a b) ;

  NameExample name = mkExample (NameExp name) ;
  FunExample f x = mkExample (FunExp f x) ;
  Fun2Example f x y = mkExample (Fun2Exp f x y) ;
  FunCExample f x y = mkExample (FunCExp f x y) ;

  Noun1Example noun x = mkExample (Noun1Prop noun x) ;
  Noun2Example noun x y = mkExample (Noun2Prop noun x y) ;
  NounCExample noun x y = mkExample (NounCProp noun x y) ;

  VerbExample verb x = mkExample (VerbProp verb x) ;
  Verb2Example verb x y = mkExample (Verb2Prop verb x y) ;
  VerbCExample verb x y = mkExample (VerbCProp verb x y) ;

  DepExample f x = mkExample (DepKind f x) ;
  Dep2Example f x y = mkExample (Dep2Kind f x y) ;
  DepCExample f x y = mkExample (DepCKind f x y) ;

  BinderExample b i f = mkExample (BinderExp b i f) ;
  Binder1Example b k i f = mkExample (Binder1Exp b k i f) ;
  Binder2Example b x y i f = mkExample (Binder2Exp b x y i f) ;

  LabelExample label = mkExample (mkUtt label.np) "." ;

  X_Argument = NameExp (mkName "X") ;
  Y_Argument = NameExp (mkName "Y") ;
  Z_Argument = NameExp (mkName "Z") ;

  x_BoundVariable = "x" ;
  i_BoundVariable = "i" ;

  A_KindArgument = NounKind (mkNoun "A") ;
  B_KindArgument = NounKind (mkNoun "B") ;

---  NounName noun = mkNP noun ;
  DefNounName noun = mkNP the_Det noun ;
  ProperNameNounName name noun = npGenNounNP (mkNP name) noun ;

  NounPrepFam noun prep = {cn = noun ; prep = prep ; isCollective = False} ;
  --- isC only relevant for Fam2
  NounPrepFam2 noun prep1 prep2 = {cn = noun ; prep1 = prep1 ; prep2 = prep2 ; isCollective = False} ;
  NounPrepFun noun prep = {cn = noun ; prep = prep} ;
  NounPrepFun2 noun prep1 prep2 = {cn = noun ; prep1 = prep1 ; prep2 = prep2 ; isCollective = False} ;
  NounPrepFunC noun prep = {cn = noun ; prep = prep} ;

  AdverbAdjAdj adv adj = mkAP (lin AdA adv) adj ;
  AdjPrepAdj2 adj prep = {ap = adj ; prep = prep} ;
  AdjAdjC adj = adj ;
  AdjAdjE adj = adj ;
  AdjPrepAdj3 adj prep1 prep2 = {ap = adj ; prep1 = prep1 ; prep2 = prep2} ;
  
  NounNoun1 noun = noun ;
  NounPrepNoun2 noun prep = {cn = noun ; prep = prep} ;
  NounNounC noun = noun ;

  VerbPrepNounVerb verb prep noun = mkVP verb (mkAdv prep (mkNP a_Det noun)) ;
  VerbPrepVerb2 verb prep = mkVerb2 verb prep ;
  VerbVerbC verb = verb ;

  NounPrepDep noun prep = {cn = noun ; prep = prep} ;
  NounPrepDep2 noun prep1 prep2 = {cn = noun ; prep1 = prep1 ; prep2 = prep2} ;
  NounPrepDepC noun prep = {cn = noun ; prep = prep} ;

  AdjNounNoun adj noun = mkCN adj noun ;
  NounNounNoun noun1 noun2 = compoundCN noun1 noun2 ;
  ProperNameNounNoun name noun = nameCompoundCN name noun ;

  NounLabel noun = mkLabel (mkNP noun) ;
  DefNounLabel noun = mkLabel (mkNP the_Det noun) ;
  NounIntLabel noun int = mkLabel (mkNP (mkCN noun <symb int : NP>)) ;
  NounIdentLabel noun ident = mkLabel (mkNP (mkCN noun <symb (mkSymb ident) : NP>)) ;
  NounOfNounLabel noun1 noun2 = mkLabel (mkNP the_Det (mkCN noun1 (mkAdv of_Prep (mkNP noun2)))) ;
  ProperNameNounLabel name noun = mkLabel (npGenNounNP (mkNP name) noun) ;

  NounBinder noun = noun ;
  NounBinder1 noun = noun ;
  NounBinder2 noun = noun ;


  noPrep = strPrep "" ;

  at_Prep = Utilities.at_Prep ;
  between_Prep = Syntax.between_Prep ;
  by_Prep = Syntax.by8means_Prep ;
  for_Prep = Syntax.for_Prep ;
  from_Prep = Syntax.from_Prep ;
  in_Prep = Syntax.in_Prep ;
  modulo_Prep = strPrep "modulo" ;
  of_Prep = Syntax.possess_Prep ;
  on_Prep = Syntax.on_Prep ;
  over_Prep = Utilities.over_Prep ;
  to_Prep = Syntax.to_Prep ;
  under_Prep = Syntax.under_Prep ;
  with_Prep = Syntax.with_Prep ;

oper
  mkExample = overload {
    mkExample : Prop -> Utt = \p -> mkUtt (topProp p) ;
    mkExample : Kind -> Utt = \p -> mkUtt (useKind p) ;
    mkExample : Exp -> Utt = \p -> mkUtt p ;
    mkExample : Utt -> Str -> Utt = \u, s -> lin Utt {s = u.s ++ s} ;
    } ;


}