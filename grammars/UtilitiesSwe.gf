instance UtilitiesSwe of Utilities =

open
  SyntaxSwe,
  (S=SyntaxSwe),
  ParadigmsSwe,
  (P=ParadigmsSwe),
  SymbolicSwe,
  MarkupSwe,
  (I=IrregSwe),
  (E=ExtendSwe),
  (M=MakeStructuralSwe),
  Formal,
  Prelude

in {

oper

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  displayLatexS : Symb -> S = \x -> symb (mkSymb ("$$" ++ x.s ++ "$$")) ;
  
  negPol = negativePol ;

  strN : Str -> N = mkN ;
  strA : Str -> A = mkA ;
  strV : Str -> V = mkV ;
  strPN : Str -> PN = mkPN ;
  strPrep : Str -> Prep = mkPrep ;
  
  define_V2 : V2 = mkV2 (mkV "definiera") ;
  assume_VS : VS = mkVS (mkV "anta" "antar" "anta" "antog" "antagit" "antagen") ;
  type_CN : CN = mkCN (mkN "typ" "typer") ;
  case_N : N = mkN "fall" "fall" ;
  contradiction_N : N = mkN "kontradiction" "kontradictioner" ;
  then_Adv : Adv = ParadigmsSwe.mkAdv "så" ;
  thenText_Adv : Adv = ParadigmsSwe.mkAdv "då" ;
  such_that_Subj : Subj = mkSubj "så att" ;
  applied_to_Prep : Prep = mkPrep "applicerat på" ;
  defined_as_Prep : Prep = mkPrep "definierat som" ;
  function_N : N = mkN "funktion" "funktioner" ;
  basic_type_CN : CN = mkCN (mkN "grundtyp" "grundtyper") ;
  map_V3 = mkV3 (mkV "avbilda") noPrep as_Prep ;
  say_VS = mkVS I.säga_V ;
  hold_V2 = mkV2 I.hålla_V for_Prep ;
  arbitrary_A = mkA "godtycklig" ;
  set_N = mkN "mängd" "mängder" ;

  iff_Subj : Subj = mkSubj "om och endast om" ;
  commaConj : Conj = M.mkConj "," ;

  basic_concept_Str = "grundbegrepp" ;
  by_cases_Str = "med fallanalys:" ;
  proof_Str = "bevis" ;
  axiom_Str = "axiom" ;
  theorem_Str = "teorem" ;
  definition_Str = "definition" ;

  instance_N = mkN "instans" "instanser" ;
  prove_VS = mkVS (mkV "bevisa") ;
  
  as_Prep : Prep = mkPrep "som" ;

  let_Str : Bool => Str = \\_ => "låt" ;
  assuming_Str = "under följande antaganden:" ;

  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; ---- should be in RGL

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  imply_V2 : V2 = mkV2 (mkV "implicera") ;
  only_if_Subj : Subj = mkSubj "endast om" ;

  number_Noun = mkCN (mkN "tal" "tal") ;
  range_V3 = mkV3 (mkV "löper") from_Prep to_Prep ;
  sum_N = mkN "summa" ;
  where_Subj = mkSubj "där" ;

  given_A2 = mkA2 (mkA "given") as_Prep ;
  infinity_NP = mkNP (mkN "oändlighet") ;
  series_N = mkN "serie" "serier" ;
  integral_N = mkN "integral" ;

  latexSymbNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  tal_N : N = mkN "tal" "tal" ;
  tom_A : A = mkA "tom" "tomt" "tomma" "tommare" "tommast" ;

  mängd_N = mkN "mängd" "mängder" ;
  element_N = mkN "element" "element" ;
  
}
