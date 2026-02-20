abstract Examples = Categories, MathCore ** {

-- extensions of lexicon definable in symbol tables and parsable from examples

cat
  Example ;
  Argument ;
  KindArgument ;

fun
  AdjExample : Adj -> Argument -> Example ;
  Adj2Example : Adj2 -> Argument -> Argument -> Example ;
  AdjCExample : AdjC -> Argument -> Argument -> Example ;
  AdjEExample : AdjE -> Argument -> Argument -> Example ;
  NounExample : Noun -> Example ;
  NameExample : Name -> Example ;
  FunExample : Fun -> Argument -> Example ;
  Fun2Example : Fun2 -> Argument -> Argument -> Example ;
  FunCExample : FunC -> Argument -> Argument -> Example ;
  FamExample : Fam -> KindArgument -> Example ;
  Fam2Example : Fam2 -> KindArgument -> KindArgument -> Example ;
  Noun1Example : Noun1 -> Argument -> Example ;
  VerbExample : Verb -> Argument -> Example ;
  Verb2Example : Verb2 -> Argument -> Argument -> Example ;
  Noun2Example : Noun2 -> Argument -> Argument -> Example ;
  Adj3Example : Adj3 -> Argument -> Argument -> Argument -> Example ;

  X_Argument, Y_Argument, Z_Argument : Argument ;
  A_KindArgument, B_KindArgument : KindArgument ;

  NounName : Noun -> Name ;
  DefNounName : Noun -> Name ;

  NounPrepFam : Noun -> Prep -> Fam ;
  NounPrepFam2 : Noun -> Prep -> Prep -> Fam2 ;
  NounPrepFun : Noun -> Prep -> Fun ;
  NounPrepFun2 : Noun -> Prep -> Prep -> Fun2 ;
  NounPrepFunC : Noun -> Prep -> FunC ;

  AdverbAdjAdj : Adverb -> Adj -> Adj ;
  AdjPrepAdj2 : Adj -> Prep -> Adj2 ;
  AdjAdjC : Adj -> AdjC ;
  AdjAdjE : Adj -> AdjE ;
  AdjPrepAdj3 : Adj -> Prep -> Prep -> Adj3 ;

  NounNoun1 : Noun -> Noun1 ;
  NounPrepNoun2 : Noun -> Prep -> Noun2 ;

  VerbPrepVerb2 : Verb -> Prep -> Verb2 ;
  
  AdjNounNoun : Adj -> Noun -> Noun ;

  NoPrep : Prep ;

  at_Prep : Prep ;
  betweenPrep : Prep ;
  byPrep : Prep ;
  forPrep : Prep ;
  fromPrep : Prep ;
  inPrep : Prep ;
  moduloPrep : Prep ;
  ofPrep : Prep ;
  onPrep : Prep ;
  toPrep : Prep ;
  withPrep : Prep ;

}