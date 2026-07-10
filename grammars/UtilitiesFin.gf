instance UtilitiesFin of Utilities =

open
  SyntaxFin,
  (S=SyntaxFin),
  ParadigmsFin,
  (P=ParadigmsFin),
  SymbolicFin,
  MarkupFin,
  (Extend=ExtendFin),
  (X=ExtraFin),
  Formal,
  Prelude

in {

oper

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = s.s ++ adv.s} ;
  displayLatexS : Symb -> S = \x -> symb (mkSymb ("$$" ++ x.s ++ "$$")) ;

  compoundCN : CN -> CN -> CN = \cn1, cn2 ->
    cn2 ** {s = \\nc => (mkUtt cn1).s ++ Predef.BIND ++ cn2.s ! nc} ;
  
  nameCompoundCN : PN -> CN -> CN = \pn, cn ->
    cn ** {s = \\nc => (mkUtt (mkNP pn)).s ++ Predef.BIND ++ "+" ++ Predef.BIND ++ cn.s ! nc} ;

  npGenNounNP : NP -> CN -> NP = \np, cn -> mkNP (Extend.GenNP np) cn ;

  
  negPol = negativePol ;

  strN : Str -> N = mkN ;
  strA : Str -> A = mkA ;
  strV : Str -> V = mkV ;
  strPN : Str -> PN = mkPN ;
  strPrep : Str -> Prep = mkPrep ;
  
  define_V2 : V2 = mkV2 (mkV "määritellä") ;
  assume_VS : VS = mkVS (mkV "olettaa") ;
  type_CN : CN = mkCN (mkN "tyyppi") ;
  case_N : N = mkN "tapaus" ;
  contradiction_N : N = mkN "ristiriita" ;
  then_Adv : Adv = ParadigmsFin.mkAdv "så" ;
  thenText_Adv : Adv = ParadigmsFin.mkAdv "då" ;
  such_that_Subj : Subj = mkSubj "så att" ;
  applied_to_Prep : Prep = mkPrep "applicerat på" ;
  defined_as_Prep : Prep = mkPrep "definierat som" ;
  function_N : N = mkN "funktio" ;
  predicate_N : N = mkN "predikaatti" ;
  basic_type_CN : CN = mkCN (mkN "perus" (mkN "tyyppi")) ;
  map_V3 = mkV3 (mkV "kuvata") accusative for_Prep ;
  say_VS = mkVS (mkV "sanoa") ;
  hold_V2 = mkV2 (mkV "päteä") for_Prep ;
  arbitrary_A = mkA "godtycklig" ;
  set_N = mkN "joukko" ;
  proposition_N = mkN "påstående" ;
  
  iff_Subj : Subj = mkSubj "om och endast om" ;
  commaConj : Conj = mkConj "," ;

  basic_concept_Str = "grundbegrepp" ;
  by_cases_Str = "med fallanalys:" ;
  proof_Str = "bevis" ;
  axiom_Str = "axiom" ;
  theorem_Str = "teorem" ;
  definition_Str = "definition" ;

  instance_N = mkN "instanssi" ;
  prove_VS = mkVS (mkV "todistaa") ;
  
  as_Prep : Prep = mkPrep "som" ;
  at_Prep : Prep = mkPrep "vid" ;
  over_Prep : Prep = mkPrep "over" ;
  

  let_Str : Bool => Str = \\_ => "låt" ;
  assuming_Str = "under följande antaganden:" ;

  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; ---- should be in RGL

  imply_V2 : V2 = mkV2 (mkV "implikoida") ;
  only_if_Subj : Subj = mkSubj "endast om" ;

  number_Noun = mkCN (mkN "tal" "tal") ;
  range_V3 = mkV3 (mkV "kulkea") from_Prep to_Prep ;
  sum_N = mkN "summa" ;
  where_Subj = mkSubj "där" ;

  given_A2 = mkA2 (mkA "annettu") as_Prep ;
  infinity_NP = mkNP (mkN "äärettön") ;
  series_N = mkN "sarja" ;
  integral_N = mkN "integraali" ;

  latexSymbNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  tal_N : N = mkN "luku" ;
  tom_A : A = mkA "tyhjä" ;

  mängd_N = mkN "joukko" ;
  element_N = mkN "alkio" ;
  
}
