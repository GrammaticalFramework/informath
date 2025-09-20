instance UtilitiesEng of Utilities =

open
  SyntaxEng,
  ParadigmsEng,
  (P=ParadigmsEng),
  SymbolicEng,
  MarkupEng,
  (I=IrregEng),
  (E=ExtendEng),
  Formal,
  Prelude

in {

oper
  commaConj : Conj = mkConj "," ;

  define_V2 : V2 = mkV2 (mkV "define") ;
  assume_VS : VS = mkVS (mkV "assume") ;
  element_N : N = mkN "element" ;
  type_CN : CN = mkCN (mkN "type") ;
  case_N : N = mkN "case" ;
  contradiction_N : N = mkN "contradiction" ;
  then_Adv : Adv = ParadigmsEng.mkAdv "then" ;
  thenText_Adv : Adv = ParadigmsEng.mkAdv "then" ;
  such_that_Subj : Subj = mkSubj "such that" ;
  applied_to_Prep : Prep = mkPrep "applied to" ;
  defined_as_Prep : Prep = mkPrep "defined as" ;
  function_N : N = mkN "function" ;
  basic_type_CN : CN = mkCN (mkA "basic") (mkN "type") ;
  map_V3 = mkV3 (mkV "map") noPrep to_Prep ;
  say_VS = mkVS I.say_V ;
  hold_V2 = mkV2 I.hold_V for_Prep ;
  arbitrary_A = mkA "arbitrary" ;
  set_N = mkN "set" ;

  iff_Subj : Subj = mkSubj "if and only if" ;
  commaConj : Conj = mkConj "," ;

  basic_concept_Str = "basic concept" ;
  by_cases_Str = "by cases:" ;
  proof_Str = "proof" ;
  axiom_Str = "axiom" ;
  theorem_Str = "theorem" ;
  definition_Str = "definition" ;

  instance_N = mkN "instance" ;
  prove_VS = mkVS (mkV "prove") ;

  as_Prep : Prep = mkPrep "as" ;

  let_Str : Bool => Str = \\_ => "let" ;

  assuming_Str = "assuming:" ;
  
  imply_V2 : V2 = mkV2 (mkV "imply") ;
  only_if_Subj : Subj = mkSubj "only if" ;

}