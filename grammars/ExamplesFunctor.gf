incomplete concrete ExamplesFunctor of Examples = Categories **

open
  Syntax,
  MathCore,
  Utilities

in {

lincat
  Example = Utt ;
  Argument = Exp ;
  KindArgument = Kind ;

lin
  AdjExample adj x = mkExample (AdjProp adj x) ;
  Adj2Example adj x y = mkExample (Adj2Prop adj x y) ;
  AdjCExample adj x y = mkExample (AdjCProp adj x y) ;
  AdjEExample adj x y = mkExample (mkExample (AdjEProp adj x y)) "EQ" ;
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

  VerbExample verb x = mkExample (VerbProp verb x) ;
  Verb2Example verb x y = mkExample (Verb2Prop verb x y) ;

  X_Argument = NameExp (mkName "X") ;
  Y_Argument = NameExp (mkName "Y") ;
  Z_Argument = NameExp (mkName "Z") ;

  A_KindArgument = NounKind (mkNoun "A") ;
  B_KindArgument = NounKind (mkNoun "B") ;

oper
  mkExample = overload {
    mkExample : Prop -> Utt = \p -> mkUtt (topProp p) ;
    mkExample : Kind -> Utt = \p -> mkUtt (useKind p) ;
    mkExample : Exp -> Utt = \p -> mkUtt p ;
    mkExample : Utt -> Str -> Utt = \u, s -> lin Utt {s = u.s ++ s} ;
    } ;


}