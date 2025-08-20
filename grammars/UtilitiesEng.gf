resource UtilitiesEng =

open
  SyntaxEng,
  ParadigmsEng,
  (P=ParadigmsEng),
  SymbolicEng,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {
oper
  RelationT : Type = {ap : AP ; prep : Prep} ;
  FunctionT : Type = {cn : CN ; prep : Prep} ;
  RelnounT : Type = {cn : CN ; prep : Prep} ;
  ConstantT : Type = {np : NP ; c : Str} ;
  FamilyT : Type = {cn : CN ; prep1, prep2 : Prep ; isCollective : Bool} ;
  LabelT = {np : NP ; isEmpty : Bool} ;
  Pred3T = {ap : AP ; prep1, prep2 : Prep} ;

  mkNoun = overload {
    mkNoun : Str -> CN
      = \s -> mkCN (mkN s) ;
    mkNoun : Str -> Str -> CN
      = \a, n -> mkCN (mkA a) (mkN n) ;
    } ;
    
  mkFun = overload {
    mkFun : Str -> FunctionT
      = \s -> {cn = mkCN (mkN s) ; prep = possess_Prep} ;
    mkFun : N -> FunctionT
      = \n -> {cn = mkCN n ; prep = possess_Prep} ;
    mkFun : CN -> FunctionT
      = \n -> {cn = n ; prep = possess_Prep} ;
    mkFun : N -> Prep -> FunctionT
      = \n, p -> {cn = mkCN n ; prep = p} ;
    mkFun : N -> Str -> FunctionT
      = \n, s -> {cn = mkCN (mkCN n) (P.mkAdv s) ; prep = possess_Prep} ;
    mkFun : (a, n : Str) -> FunctionT
      = \a, n -> {cn = mkCN (mkA a) (mkN n) ; prep = possess_Prep} ;
    mkFun : (a, b, n : Str) -> FunctionT
      = \a, b, n -> {cn = mkCN (mkA a) (mkCN (mkA b) (mkN n)) ; prep = possess_Prep} ;
    } ;
    
  mkRelnoun = overload {
    mkRelnoun : Str -> RelnounT
      = \s -> {cn = mkCN (mkN s) ; prep = possess_Prep} ;
    mkRelnoun : N -> RelnounT
      = \n -> {cn = mkCN n ; prep = possess_Prep} ;
    mkRelnoun : CN -> RelnounT
      = \n -> {cn = n ; prep = possess_Prep} ;
    mkRelnoun : N -> Prep -> RelnounT
      = \n, p -> {cn = mkCN n ; prep = p} ;
    mkRelnoun : N -> Str -> RelnounT
      = \n, s -> {cn = mkCN (mkCN n) (P.mkAdv s) ; prep = possess_Prep} ;
    mkRelnoun : (a, n : Str) -> RelnounT
      = \a, n -> {cn = mkCN (mkA a) (mkN n) ; prep = possess_Prep} ;
    mkRelnoun : (a, b, n : Str) -> RelnounT
      = \a, b, n -> {cn = mkCN (mkA a) (mkCN (mkA b) (mkN n)) ; prep = possess_Prep} ;
    } ;

  mkFam = overload {
    mkFam : Str -> FamilyT = \s ->
      {cn = mkCN (mkN s) ; prep1, prep2 = possess_Prep ; isCollective = True} ;
    mkFam : Str -> Prep -> Prep -> FamilyT = \s, p1, p2 ->
      {cn = mkCN (mkN s) ; prep1 = p1 ; prep2 = p2 ; isCollective = False} ;
    mkFam : CN -> FamilyT = \cn ->
      {cn = cn ; prep1, prep2 = possess_Prep ; isCollective = True} ;
    mkFam : CN -> Prep -> Prep -> FamilyT = \cn, p1, p2 ->
      {cn = cn ; prep1 = p1 ; prep2 = p2 ; isCollective = False} ;
    } ;

  mkAdj = overload {
    mkAdj : Str -> AP
      = \s -> mkAP (mkA s) ;
    } ;
    
  mkReladj = overload {
    mkRel : Str -> Str -> {ap : AP ; prep : Prep}
      = \s, p -> {ap = mkAP (mkA s) ; prep = mkPrep p}
    } ;
    
  mkName = overload {
    mkName : Str -> NP
      = \s -> mkNP (mkPN s)
    } ;

  mkLabel = overload {
    mkLabel : Str -> LabelT
      = \s -> {np = mkNP (mkPN s) ; isEmpty = False}
    } ;

  mkConst = overload {
    mkConst : Str -> Str -> ConstantT
      = \c, w -> {np = mkName w ; c = c} ;
    mkConst : Str -> NP -> ConstantT
      = \c, np -> {np = np ; c = c} ;
    } ;
    
  mkPred3 = overload {
    mkPred3 : AP -> Prep -> Prep -> Pred3T
      = \ap, p1, p2 -> {ap = ap ; prep1 = p1 ; prep2 = p2} ;
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;
}