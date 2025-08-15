resource UtilitiesSwe =

open
  SyntaxSwe,
  ParadigmsSwe,
  (P=ParadigmsSwe),
  SymbolicSwe,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {
oper
  RelationT : Type = {ap : AP ; prep : Prep} ;
  FunctionT : Type = {cn : CN ; prep : Prep} ;
  ConstantT : Type = {np : NP ; c : Str} ;
  OperatorT : Type = {op : L.OperT ; f : FunctionT} ; 
  ComparisonT : Type = {rel : RelationT ; op :  Str} ;
  SetT : Type = {cn : CN ; c : Str} ;
  FamilyT : Type = {cn : CN ; prep1, prep2 : Prep ; isCollective : Bool} ;
  LabelT = {np : NP ; isEmpty : Bool} ;
  ComparnounT = {cn : CN ; prep : Prep ; op : Str} ;
  Pred3T = {ap : AP ; prep1, prep2 : Prep} ;
 
  mkNoun = overload {
    mkNoun : Str -> CN
      = \s -> mkCN (mkN s) ;
    mkNoun : N -> CN
      = \n -> mkCN n ;
    mkNoun : Str -> Str -> CN
      = \a, n -> mkCN (mkA a) (mkN n) ;
    } ;
    
  mkSet = overload {
    mkSet : Str -> Str -> SetT
      = \c, s -> {cn = mkCN (mkN s) ; c = c} ;
    mkSet : Str -> N -> SetT
      = \c, n -> {cn = mkCN n ; c = c} ;
    mkSet : Str -> Str -> Str -> SetT
      = \c, a, n -> {cn = mkCN (mkA a) (mkN n) ; c = c} ;
    mkSet : Str -> Str -> N -> SetT
      = \c, a, n -> {cn = mkCN (mkA a) n ; c = c} ;
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
    
  mkRel = overload {
    mkRel : Str -> Str -> {ap : AP ; prep : Prep}
      = \s, p -> {ap = mkAP (mkA s) ; prep = mkPrep p} ;
    mkRel : AP -> Prep -> {ap : AP ; prep : Prep}
      = \ap, prep -> {ap = ap ; prep = prep}
    } ;
    
  mkName = overload {
    mkName : Str -> NP
      = \s -> mkNP (mkPN s) ;
    mkName : N -> NP
      = \n -> mkNP n
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
    

  mkOper = overload {
    mkOper : L.OperT -> Str -> OperatorT
      = \op, w -> {op = op ; f = mkFun w} ;
    mkOper : L.OperT -> N -> OperatorT
      = \op, w -> {op = op ; f = mkFun w} ;
    mkOper : L.OperT -> CN -> OperatorT
      = \op, w -> {op = op ; f = mkFun w} ;
    mkOper : L.OperT -> N -> Prep -> OperatorT
      = \op, w, prep -> {op = op ; f = mkFun w prep} ; 
    } ;

  mkCompar = overload {
    mkCompar : Str -> Str -> Str -> ComparisonT
      = \op, s, p -> {rel = mkRel s p ; op = op} ;
    mkCompar : Str -> AP -> Prep -> ComparisonT
      = \op, ap, prep -> {rel = mkRel ap prep ; op = op} ;
    } ;
    
  mkComparnoun = overload {
    mkComparnoun : Str -> Str -> ComparnounT
      = \op, s -> {cn = mkCN (mkN s) ; prep = possess_Prep ; op = op} ;
    mkComparnoun : Str -> N -> ComparnounT
      = \op, n -> {cn = mkCN n ; prep = possess_Prep ; op = op} ;
    mkComparnoun : Str -> CN -> ComparnounT
      = \op, cn -> {cn = cn ; prep = possess_Prep ; op = op} ;
    mkComparnoun : Str -> CN -> Prep -> ComparnounT
      = \op, cn, prep -> {cn = cn ; prep = prep ; op = op} ;
    } ;

  mkPred3 = overload {
    mkPred3 : AP -> Prep -> Prep -> Pred3T
      = \ap, p1, p2 -> {ap = ap ; prep1 = p1 ; prep2 = p2} ;
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;

  tal_N : N = mkN "tal" "tal" ;

  mängd_N = mkN "mängd" "mängder" ;
  element_N = mkN "element" "element" ;
}