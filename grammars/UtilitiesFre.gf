instance UtilitiesFre of Utilities =

open
  SyntaxFre,
  (S=SyntaxFre),
  ParadigmsFre,
  (P=ParadigmsFre),
  SymbolicFre,
  MarkupFre,
  (I=IrregFre),
  (E=ExtendFre),
  (Mk=MakeStructuralFre),
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

  define_V2 : V2 = mkV2 (mkV "définir") ;
  assume_VS : VS = mkVS (mkV "supposer") ;
  type_CN : CN = mkCN (mkN "type" masculine) ;
  element_N : N = mkN "élément" ;
  case_N : N = mkN "cas" ;
  contradiction_N : N = mkN "contradiction" ;
  then_Adv : Adv = ParadigmsFre.mkAdv "alors" ;
  thenText_Adv : Adv = ParadigmsFre.mkAdv "alors" ;
  such_that_Subj : Subj = Mk.mkSubjSubj "tel que" ; -----
  applied_to_Prep : Prep = mkPrep "appliqué à" ; ----
  defined_as_Prep : Prep = mkPrep "défini comme" ; ----
  function_N : N = mkN "fonction" ;
  basic_type_CN : CN = mkCN type_CN (ParadigmsFre.mkAdv "de base") ;
  map_V3 = mkV3 I.envoyer_V accusative dative ; ----
  say_VS = mkVS I.dire_V ;
  hold_V2 = mkV2 I.tenir_V for_Prep ; ----
  arbitrary_A = mkA "arbitraire" ;
  set_N = mkN "ensemble" masculine ;
  combinaison_N = mkN "combinaison" feminine ;

  iff_Subj : Subj = Mk.mkSubj "si et seulement si" ;
  commaConj : Conj = Mk.mkConj "," ;

  by_cases_Str = "par des cas" ;
  proof_Str = "démonstration" ;
  axiom_Str = "axiome" ;
  theorem_Str = "théorème" ;
  definition_Str = "définition" ;
  basic_concept_Str = "concept de base" ;

  instance_N = mkN "instance" ;
  prove_VS = mkVS (mkV "démontrer") ;
  
  as_Prep : Prep = mkPrep "comme" ;

  let_Str : Bool => Str = table {False => "soit" ; True => "soient"} ;
  assuming_Str = "en supposant :" ;

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  imply_V2 : V2 = mkV2 (mkV "impliquer") ;
  only_if_Subj : Subj = Mk.mkSubj "seulement si" ;

{-
  number_Noun = mkCN (mkN "tal" "tal") ;
  range_V3 = mkV3 (mkV "löper") from_Prep to_Prep ;
  sum_N = mkN "summa" ;
  where_Subj = mkSubj "där" ;

  given_A2 = mkA2 (mkA "given") as_Prep ;
  infinity_NP = mkNP (mkN "oändlighet") ;
  series_N = mkN "serie" "serier" ;
  integral_N = mkN "integral" ;
-}

  latexSymbNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  nombre_N : N = mkN "nombre" masculine ;
  tom_A : A = mkA "vide" ;

  ensemble_N = mkN "ensemble" masculine ;
  element_N = mkN "élément" ;
  
}
