concrete MathCoreGer of MathCore =
  TermsLatex, 
  UserConstantsGer **
  MathCoreFunctor - [Pred3Prop] --- takes a long time
    with
    (Syntax=SyntaxGer),
    (Grammar=GrammarGer),
    (Markup=MarkupGer),
    (Extend=ExtendGer),
    (Symbolic=SymbolicGer)
  ** open
    UtilitiesGer,
    Prelude,
    ParadigmsGer,
    (M=MakeStructuralGer),
    (I=IrregGer)

in {

-- exceptions from functor
--lin
  --Pred3Prop pred x y z =
    --simpleProp (mkS (mkCl x (AdvAP pred.ap (Syntax.mkAdv pred.prep1 (mkNP y (Syntax.mkAdv pred.prep2 z)))))) ; --- fake structure


oper
  alle_Det = M.mkWeakDet "all" plural ;

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

  let_Str : Bool => Str = \\_ => "sei" ;
  assuming_Str = "Unter folgender Annahme:" ;

  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; ---- should be in RGL

}
