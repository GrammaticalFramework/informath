interface Utilities =

open
  Syntax,
  Symbolic,
  Markup,
  Formal,
  Prelude

in {

oper

-- generic utilities

  labelText : LabelT -> Text -> Text = \label, text ->
    let period = if_then_Str label.isEmpty "" "." in
    lin Text {s = (mkUtt label.np).s ++ period ++ text.s} ;

  thenText : {text : Text ; isEmpty : Bool} -> S -> Text = \hypos, s ->
    case hypos.isEmpty of {
      True => mkText hypos.text (mkText (mkUtt s)) ;
      False => mkText hypos.text (mkText (mkUtt (mkS thenText_Adv s)))
	     | mkText hypos.text (mkText (mkUtt s))    ---- variants !!??
      } ;

  latexNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  commaConj : Conj ;

  useKind : {cn : CN ; adv : Adv} -> CN = \kind -> mkCN kind.cn kind.adv ;

  latexS : Symb -> S = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  simpleProp : S -> Proposition = \s -> {s = s ; isComplex = False} ;
  complexProp : S -> Proposition = \s -> {s = s ; isComplex = True} ;

  topProp : Proposition -> S = \prop -> prop.s ;
  partProp : Proposition -> S = \prop -> case prop.isComplex of {
    True => parenthS prop.s ;
    False => prop.s
    } ;
    
  parenthS : S -> S = \s -> MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

  prefixText : Str -> Text -> Text = \s, t -> lin Text {s = s ++ t.s} ;

  item_Label : Str = "\\item" ;


-- type synonyms
  Proposition : Type = {s : S ; isComplex : Bool} ;

  RelationT : Type = {ap : AP ; prep : Prep} ;
  RelnounT : Type = {cn : CN ; prep : Prep} ;
  FunctionT : Type = {cn : CN ; prep : Prep} ;
  ConstantT : Type = {np : NP ; c : Str} ;
  OperatorT : Type = {f : FunctionT} ; 
  ComparisonT : Type = {rel : RelationT ; op :  Str} ;
  SetT : Type = {cn : CN ; c : Str} ;
  FamilyT : Type = {cn : CN ; prep1, prep2 : Prep ; isCollective : Bool} ;
  LabelT = {np : NP ; isEmpty : Bool} ;
  ComparnounT = {cn : CN ; prep : Prep ; op : Str} ;
  Pred3T = {ap : AP ; prep1, prep2 : Prep} ;


-- lintype constructors

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

  mkPred3 = overload {
    mkPred3 : AP -> Prep -> Prep -> Pred3T
      = \ap, p1, p2 -> {ap = ap ; prep1 = p1 ; prep2 = p2} ;
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;

-- often used words

  type_CN : CN ;
  case_N : N ;
  contradiction_N : N ;
  then_Adv : Adv ;
  thenText_Adv : Adv ;
  such_that_Subj : Subj ;
  applied_to_Prep : Prep ;
  defined_as_Prep : Prep ;
  function_N : N ;
  basic_type_CN : CN ;
  map_V3 : V3 ;
  say_VS : VS ;
  hold_V2 : V2 ;
  arbitrary_A : A ;
  set_N : N ;

  iff_Subj : Subj ;
  commaConj : Conj ;

  basic_concept_Str : Str ;
  by_cases_Str : Str ;
  proof_Str : Str ;
  axiom_Str : Str ;
  theorem_Str : Str ;
  definition_Str : Str ;

  instance_N : N ;
  prove_VS : VS ;

  as_Prep : Prep ;

  let_Str : Bool => Str ;

  assuming_Str : Str ;

  imply_V2 : V2 ;
  only_if_Subj : Subj ;


}