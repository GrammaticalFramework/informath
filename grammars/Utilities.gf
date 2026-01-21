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

  strText : Str -> Text = \s -> lin Text {s = s} ;

  ccText = overload {    
    ccText : (a, b : Text) -> Text = \a, b -> mkText a b ;
    ccText : (a, b, c : Text) -> Text = \a, b, c -> mkText a (mkText b c) ;
    ccText : (a, b, c, d : Text) -> Text = \a, b, c, d -> mkText a (mkText b (mkText c d)) ;
    ccText : (a, b, c, d, e : Text) -> Text = \a, b, c, d, e ->
      mkText a (mkText b (mkText c (mkText d e))) ;
    } ;


  latexNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  commaConj : Conj ;

  useKind : {cn : CN ; adv : Adv} -> CN = \kind -> mkCN kind.cn kind.adv ;

  latexS : Symb -> S = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;
    
  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;

  simpleProp : S -> Proposition = \s -> {s = s ; isComplex = False} ;
  complexProp : S -> Proposition = \s -> {s = s ; isComplex = True} ;

  topProp : Proposition -> S = \prop -> prop.s ;
  partProp : Proposition -> S = \prop -> case prop.isComplex of {
    True => parenthS prop.s ;
    False => prop.s
    } ;


  propText : Proposition -> Text = \prop -> mkText (topProp prop) ;
  propInText : Proposition -> Text = \prop -> mkText (mkUtt (topProp prop)) ;
    
  parenthS : S -> S = \s -> MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

  prefixText : Str -> Text -> Text = \s, t -> lin Text {s = s ++ t.s} ;
  prefixTextFullStop : Str -> Text -> Text = \s, t -> lin Text {s = s ++ "." ++ t.s} ;
  embedText : Str -> Str -> Text -> Text = \b, e, t -> lin Text {s = b ++ t.s ++ e} ;

  item_str : Str = "\\item" ;
  begin_itemize_str : Str = "\\begin{itemize}" ;
  end_itemize_str : Str = "\\end{itemize}" ;

  postAdvS : S -> Adv -> S ;
  displayLatexS : Symb -> S ;


-- type synonyms
  Proposition : Type = {s : S ; isComplex : Bool} ;

  Adj2T : Type = {ap : AP ; prep : Prep} ;
  Noun2T : Type = {cn : CN ; prep : Prep} ;
  FunT : Type = {cn : CN ; prep : Prep} ;
  Fun2T : Type = {cn : CN ; prep1, prep2 : Prep ; isColl : Bool} ; -- isColl means f prep x and y
  FamT : Type = {cn : CN ; prep : Prep ; isCollective : Bool} ;
  Fam2T : Type = {cn : CN ; prep1, prep2 : Prep ; isCollective : Bool} ;
  LabelT = {np : NP ; isEmpty : Bool} ;
  Adj3T = {ap : AP ; prep1, prep2 : Prep} ;
  AdjT = AP ;
  NounT = CN ;
  VerbT = VP ;
  Verb2T = V2 ;

-- lintype constructors, language-independent

  mkNoun = overload {
    mkNoun : Str -> NounT
      = \s -> (mkCN (strN s)) ;
    mkNoun : Str -> Str -> NounT
      = \a, n -> (mkCN (strA a) (strN n)) ;
    mkNoun : N -> NounT
      = \n -> mkCN n ;
    mkNoun : A -> N -> NounT
      = \a, n -> mkCN a n ;
    mkNoun : CN -> NounT
      = \cn -> cn ;
    } ;

  mkNoun2 = overload {
    mkNoun2 : Str -> Noun2T
      = \s -> {cn = mkCN (strN s) ; prep = possess_Prep} ;
    mkNoun2 : N -> Noun2T
      = \n -> {cn = mkCN n ; prep = possess_Prep} ;
    mkNoun2 : CN -> Noun2T
      = \n -> {cn = n ; prep = possess_Prep} ;
    mkNoun2 : N -> Prep -> Noun2T
      = \n, p -> {cn = mkCN n ; prep = p} ;
    mkNoun2 : CN -> Prep -> Noun2T
      = \cn, p -> {cn = cn ; prep = p} ;
    mkNoun2 : N -> Str -> Noun2T
      = \n, s -> {cn = mkCN (mkCN n) (P.mkAdv s) ; prep = possess_Prep} ;
    mkNoun2 : (a, n : Str) -> Noun2T
      = \a, n -> {cn = mkCN (strA a) (strN n) ; prep = possess_Prep} ;
    mkNoun2 : (a, b, n : Str) -> Noun2T
      = \a, b, n -> {cn = mkCN (strA a) (mkCN (strA b) (strN n)) ; prep = possess_Prep} ;
    } ;

  mkFun = overload {
    mkFun : Str -> FunT
      = \s -> {cn = mkCN (strN s) ; prep = possess_Prep} ;
    mkFun : N -> FunT
      = \n -> {cn = mkCN n ; prep = possess_Prep} ;
    mkFun : CN -> FunT
      = \n -> {cn = n ; prep = possess_Prep} ;
    mkFun : N -> Prep -> FunT
      = \n, p -> {cn = mkCN n ; prep = p} ;
    mkFun : CN -> Prep -> FunT
      = \cn, p -> {cn = cn ; prep = p} ;
    mkFun : N -> Str -> FunT
      = \n, s -> {cn = mkCN (mkCN n) (P.mkAdv s) ; prep = possess_Prep} ;
    mkFun : (a, n : Str) -> FunT
      = \a, n -> {cn = mkCN (strA a) (strN n) ; prep = possess_Prep} ;
    mkFun : (a, b, n : Str) -> FunT
      = \a, b, n -> {cn = mkCN (strA a) (mkCN (strA b) (strN n)) ; prep = possess_Prep} ;
    } ;

  mkFun2 = overload {
    mkFun2 : Str -> Fun2T
      = \s -> {cn = mkCN (strN s) ; prep1, prep2 = possess_Prep ; isColl = True} ;
    mkFun2 : N -> Fun2T
      = \n -> {cn = mkCN n ; prep1, prep2 = possess_Prep ; isColl = True} ;
    mkFun2 : CN -> Fun2T
      = \cn -> {cn = cn ; prep1, prep2 = possess_Prep ; isColl = True} ;
    mkFun2 : Str -> Str -> Fun2T
      = \s, sprep2 -> {cn = mkCN (strN s) ; prep1 = possess_Prep ; prep2 = strPrep sprep2 ; isColl = False} ;
    mkFun2 : N -> Prep -> Prep -> Fun2T
      = \n, prep1, prep2 -> {cn = mkCN n ; prep1 = prep1 ; prep2 = prep2 ; isColl = False} ;
    mkFun2 : CN -> Prep -> Prep -> Fun2T
      = \cn, prep1, prep2 -> {cn = cn ; prep1 = prep1 ; prep2 = prep2 ; isColl = False} ;
    mkFun2 : N -> Prep -> Fun2T
      = \n, prep1 -> {cn = mkCN n ; prep1, prep2 = prep1 ; isColl = True} ; --- prep2 is junk
    mkFun2 : CN -> Prep -> Fun2T
      = \cn, prep1 -> {cn = cn ; prep1, prep2 = prep1 ; isColl = True} ;
    } ;

  mkFunC = overload {
    mkFunC : Str -> FunT
      = \s -> {cn = mkCN (strN s) ; prep = possess_Prep} ;
    mkFunC : N -> FunT
      = \n -> {cn = mkCN n ; prep = possess_Prep} ;
    mkFunC : CN -> FunT
      = \n -> {cn = n ; prep = possess_Prep} ;
    mkFunC : N -> Prep -> FunT
      = \n, p -> {cn = mkCN n ; prep = p} ;
    mkFunC : CN -> Prep -> FunT
      = \cn, p -> {cn = cn ; prep = p} ;
    mkFunC : N -> Str -> FunT
      = \n, s -> {cn = mkCN (mkCN n) (P.mkAdv s) ; prep = possess_Prep} ;
    mkFunC : (a, n : Str) -> FunT
      = \a, n -> {cn = mkCN (strA a) (strN n) ; prep = possess_Prep} ;
    mkFunC : (a, b, n : Str) -> FunT
      = \a, b, n -> {cn = mkCN (strA a) (mkCN (strA b) (strN n)) ; prep = possess_Prep} ;
    } ;

  mkFam = overload {
    mkFam : Str -> FamT = \s ->
      {cn = mkCN (strN s) ; prep = possess_Prep ; isCollective = True} ;
    mkFam : Str -> Prep -> FamT = \s, p ->
      {cn = mkCN (strN s) ; prep = p ; isCollective = False} ;
    mkFam : N -> FamT = \n ->
      {cn = mkCN n ; prep = possess_Prep ; isCollective = True} ;
    mkFam : N -> Prep -> FamT = \n, p ->
      {cn = mkCN n ; prep = p ; isCollective = False} ;
    mkFam : CN -> FamT = \cn ->
      {cn = cn ; prep = possess_Prep ; isCollective = True} ;
    mkFam : CN -> Prep -> FamT = \cn, p ->
      {cn = cn ; prep = p ; isCollective = False} ;
    } ;

  mkFam2 = overload {
    mkFam2 : Str -> Fam2T = \s ->
      {cn = mkCN (strN s) ; prep1, prep2 = possess_Prep ; isCollective = True} ;
    mkFam2 : Str -> Prep -> Prep -> Fam2T = \s, p1, p2 ->
      {cn = mkCN (strN s) ; prep1 = p1 ; prep2 = p2 ; isCollective = False} ;
    mkFam2 : N -> Prep -> Prep -> Fam2T = \n, p1, p2 ->
      {cn = mkCN n ; prep1 = p1 ; prep2 = p2 ; isCollective = False} ;
    mkFam2 : CN -> Prep -> Prep -> Fam2T = \cn, p1, p2 ->
      {cn = cn ; prep1 = p1 ; prep2 = p2 ; isCollective = False} ;
    } ;

  mkAdj = overload {
    mkAdj : Str -> AdjT
      = \s -> mkAP (strA s) ;
    mkAdj : A -> AdjT
      = \a -> mkAP a ;
    mkAdj : AP -> AdjT
      = \ap -> ap ;
    } ;
    
  mkAdjC = overload {
    mkAdjC : Str -> AdjT
      = \s -> mkAP (strA s) ;
    mkAdjC : A -> AdjT
      = \a -> mkAP a ;
    mkAdjC : AP -> AdjT
      = \ap -> ap ;
    } ;
    
  mkAdjE = overload {
    mkAdjE : Str -> AdjT
      = \s -> mkAP (strA s) ;
    mkAdjE : A -> AdjT
      = \a -> mkAP a  ;
    mkAdjE : AP -> AdjT
      = \ap -> ap ;
    } ;
    
  mkAdj2 = overload {
    mkAdj2 : Str -> Str -> Adj2T
      = \s, p -> {ap = mkAP (strA s) ; prep = strPrep p} ;
    mkAdj2 : A -> Prep -> Adj2T
      = \a, p -> {ap = mkAP a ; prep = p} ;
    mkAdj2 : AP -> Prep -> Adj2T
      = \ap, p -> {ap = ap ; prep = p}
    } ;

  mkAdj3 = overload {
    mkAdj3 : A -> Prep -> Prep -> Adj3T
      = \a, p1, p2 -> {ap = mkAP a ; prep1 = p1 ; prep2 = p2} ;
    mkAdj3 : AP -> Prep -> Prep -> Adj3T
      = \ap, p1, p2 -> {ap = ap ; prep1 = p1 ; prep2 = p2} ;
    } ;

  mkName = overload {
    mkName : Str -> NP
      = \s -> mkNP (strPN s) ;
    mkName : PN -> NP
      = \pn -> mkNP pn ;
    mkName : NP -> NP
      = \np -> np ;
    } ;

  mkLabel = overload {
    mkLabel : Str -> LabelT
      = \s -> {np = mkNP (mkPN s) ; isEmpty = False} ;
    mkLabel : NP -> LabelT
      = \np -> {np = np ; isEmpty = False}
    } ;

  mkVerb = overload {
    mkVerb : Str -> VerbT
      = \s -> mkVP (strV s) ;
    mkVerb : V -> VerbT
      = \v -> mkVP v ;
    mkVerb : VP -> VerbT
      = \vp -> vp ;
    } ;

  mkVerb2 = overload {
    mkVerb2 : Str -> Verb2T
      = \s -> mkV2 (strV s) ;
    mkVerb2 : V -> Verb2T
      = \v -> mkV2 v ;
    mkVerb2 : V2 -> Verb2T
      = \v -> v ;
    } ;

-- language-specific functions

  strN : Str -> N ;
  strA : Str -> A ;
  strV : Str -> V ;
  strPN : Str -> PN ;
  strPrep : Str -> Prep ;

-- negation, needs to be overwritten in Eng
  negPol : Pol ; -- = negativePol ;

-- often used words, language-dependent

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

-- for syntax

oper

  notionNP : {np : NP ; isPl : Bool} -> {cn : CN ; adv : Adv} -> NP = \idents, kind ->
    let det = case idents.isPl of {
      True => aPl_Det ; 
      False => a_Det
      }
    in mkNP det (mkCN (mkCN kind.cn idents.np) kind.adv) ;

  definedCN : CN -> NP -> CN = \cn, np ->
    mkCN cn (S.mkAdv defined_as_Prep np) ;
    
  definedAdv : NP -> Adv = \df ->
    S.mkAdv defined_as_Prep df ;

  by_Prep : Prep = by8means_Prep ;

  ccAdv : Adv -> Adv -> Adv = \x, y -> lin Adv {s = x.s ++ y.s} ;

  identKindCN : Str -> {cn : CN ; adv : Adv} -> CN = \ident, kind ->
    mkCN (mkCN kind.cn (latexNP (mkSymb ident))) kind.adv ;

  allNP : CN -> NP = \cn ->
    mkNP all_Predet (mkNP aPl_Det cn) ;

}