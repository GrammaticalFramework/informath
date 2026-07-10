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
  then_Adv : Adv = ParadigmsFin.mkAdv "niin" ;
  thenText_Adv : Adv = ParadigmsFin.mkAdv "silloin" ;
  such_that_Subj : Subj = mkSubj "siten että" ;
  applied_to_Prep : Prep = mkPrep "sovellettuna" ;
  defined_as_Prep : Prep = mkPrep "määriteltynä" ;
  function_N : N = mkN "funktio" ;
  predicate_N : N = mkN "predikaatti" ;
  basic_type_CN : CN = mkCN (mkN "perus" (mkN "tyyppi")) ;
  map_V3 = mkV3 (mkV "kuvata") accusative for_Prep ;
  say_VS = mkVS (mkV "sanoa") ;
  hold_V2 = mkV2 (mkV "päteä") for_Prep ;
  arbitrary_A = mkA "mielivaltainen" ;
  set_N = mkN "joukko" ;
  proposition_N = mkN "väittämä" ;

  iff_Subj : Subj = mkSubj "jos ja vain jos" ;
  commaConj : Conj = mkConj "," ;

  basic_concept_Str = "peruskäsite" ;
  by_cases_Str = "tapauksittain:" ;
  proof_Str = "todistus" ;
  axiom_Str = "aksiooma" ;
  theorem_Str = "lause" ;
  definition_Str = "määritelmä" ;

  instance_N = mkN "instanssi" ;
  prove_VS = mkVS (mkV "todistaa") ;
  
  as_Prep : Prep = mkPrep "muodossa" ;
  at_Prep : Prep = mkPrep "kohdassa" ;
  over_Prep : Prep = mkPrep "yli" ;
  

  let_Str : Bool => Str = \\_ => "olkoon" ;
  assuming_Str = "seuraavin oletuksin:" ;

  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; ---- should be in RGL

  imply_V2 : V2 = mkV2 (mkV "implikoida") ;
  only_if_Subj : Subj = mkSubj "vain jos" ;

  number_Noun = mkCN (mkN "luku") ;
  range_V3 = mkV3 (mkV "kulkea") from_Prep to_Prep ;
  sum_N = mkN "summa" ;
  where_Subj = mkSubj "missä" ;

  given_A2 = mkA2 (mkA "annettu") as_Prep ;
  infinity_NP = mkNP (mkN "ääretön") ;
  series_N = mkN "sarja" ;
  integral_N = mkN "integraali" ;

  latexSymbNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  tal_N : N = mkN "luku" ;
  tom_A : A = mkA "tyhjä" ;

  mängd_N = mkN "joukko" ;
  element_N = mkN "alkio" ;
  
}
