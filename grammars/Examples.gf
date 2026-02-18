abstract Examples = Categories, MathCore ** {

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


}