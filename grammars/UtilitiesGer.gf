instance UtilitiesGer of Utilities =

open
  SyntaxGer,
  (S=SyntaxGer),
  ParadigmsGer,
  (P=ParadigmsGer),
  SymbolicGer,
  MarkupGer,
  (I=IrregGer),
  (E=ExtendGer),
  (M=MakeStructuralGer),
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
  strPrep : Str -> Prep = \s -> mkPrep s dative ; ---


  define_V2 : V2 = mkV2 (mkV "definieren") (mkPrep "als" nominative) ;
  assume_VS : VS = mkVS (mkV "an" I.nehmen_V) ;
  type_CN : CN = mkCN (mkN "Typ") ;
  case_N : N = mkN "Fall" ;
  contradiction_N : N = mkN "Widerspruch" ;
  then_Adv : Adv = ParadigmsGer.mkAdv "dann" ;
  thenText_Adv : Adv = ParadigmsGer.mkAdv "dann" ;
  such_that_Subj : Subj = mkSubj "sodass" ;
  applied_to_Prep : Prep = mkPrep "angewandt auf" accusative ;
  defined_as_Prep : Prep = mkPrep "definiert als" nominative ;
  function_N : N = mkN "Funktion" ;
  basic_type_CN : CN = mkCN (mkN "Grundtyp") ;
  map_V3 = mkV3 (mkV "ab" (mkV "bilden")) (mkPrep accusative) as_Prep ;
  say_VS = mkVS (mkV "sagen") ;
  hold_V2 = mkV2 I.halten_V for_Prep ;
  arbitrary_A = mkA "beliebig" ;
  set_N = mkN "Menge" ;

  iff_Subj : Subj = mkSubj "genau dann, wenn" ;
  commaConj : Conj = M.mkConj "" "," plural ;

  basic_concept_Str = "Grundbegriff" ;
  by_cases_Str = "per Fallanalyse:" ;
  proof_Str = "Beweis" ;
  axiom_Str = "Axiom" ;
  theorem_Str = "Theorem" ;
  definition_Str = "Definition" ;

  instance_N = mkN "Instanz" "Instanzen" feminine ;
  prove_VS = mkVS (mkV "beweisen") ;
  
  as_Prep : Prep = mkPrep "als" nominative ;
  at_Prep : Prep = anDat_Prep ;

  alle_Det = M.mkWeakDet "all" plural ;

  let_Str : Bool => Str = \\_ => "sei" ;
  assuming_Str = "Unter folgender Annahme:" ;

  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; ---- should be in RGL
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  imply_V2 : V2 = mkV2 (mkV "implizieren") ;
  only_if_Subj : Subj = mkSubj "nur dann , wenn" ;

{-
  number_Noun = mkNoun (mkN "tal" "tal") ;
  range_V3 = mkV3 (mkV "löper") from_Prep to_Prep ;
  sum_N = mkN "summa" ;
  where_Subj = mkSubj "där" ;
  given_A2 = mkA2 (mkA "given") as_Prep ;
  infinity_NP = mkNP (mkN "oändlighet") ;
  series_N = mkN "serie" "serier" ;
  integral_N = mkN "integral" ;
-}

  leer_A : A = mkA "leer" ;

  zahl_N : N = mkN "Zahl" feminine ;

  menge_N = mkN "Menge" ;
  element_N = mkN "Element" "Elemente" neuter ;
  
}
