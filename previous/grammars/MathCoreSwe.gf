concrete MathCoreSwe of MathCore =
  TermsLatex, 
  UserConstantsSwe **
  MathCoreFunctor with
    (Syntax=SyntaxSwe),
    (Grammar=GrammarSwe),
    (Markup=MarkupSwe),
    (Extend=ExtendSwe),
    (Symbolic=SymbolicSwe)
  ** open
    UtilitiesSwe,
    Prelude,
    ParadigmsSwe,
    (M=MakeStructuralSwe),
    (I=IrregSwe)

in {

oper

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

}