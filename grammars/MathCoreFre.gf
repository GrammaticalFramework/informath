concrete MathCoreFre of MathCore =
  TermsLatex, 
  UserConstantsFre **
  MathCoreFunctor - [
    PropHypo, BaseArgKind, ConsArgKind
  ] with
    (Syntax=SyntaxFre),
    (Grammar=GrammarFre),
    (Markup=MarkupFre),
    (Extend=ExtendFre),
    (Symbolic=SymbolicFre)
  ** open
    UtilitiesFre,
    Prelude,
    ParadigmsFre,
    (I=IrregFre),
    (Mk=MakeStructuralFre)

in {

-- functor exceptions
lin
  PropHypo prop = lets_Utt (mkVP assume_VS (topProp prop)) ;

  BaseArgKind kind = {
    sg = case kind.isPl of {
      True => mkNP aPl_Det (useKind kind) ;
      False => mkNP aSg_Det (useKind kind)
      } ;
    neg = case kind.isPl of {
      True => mkNP no_Quant (useKind kind) ;
      False => mkNP no_Quant (useKind kind)
      } ;
    pl = mkNP thePl_Det (useKind kind)
    } ;
  ConsArgKind kind kinds = {
    sg = case kind.isPl of {
      True => mkNP and_Conj (mkNP aPl_Det (useKind kind)) kinds.sg ;
      False => mkNP and_Conj (mkNP aSg_Det (useKind kind)) kinds.sg
      } ;
    neg = case kind.isPl of {
      True => mkNP and_Conj (mkNP no_Quant (useKind kind)) kinds.sg ;
      False => mkNP and_Conj (mkNP no_Quant (useKind kind)) kinds.sg
      } ;
    pl = mkNP and_Conj (mkNP thePl_Det (useKind kind)) kinds.pl 
    } ;


oper

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
  basic_type_CN : CN = mkCN type_N (ParadigmsFre.mkAdv "de base") ;
  map_V3 = mkV3 I.envoyer_V accusative dative ; ----
  say_VS = mkVS I.dire_V ;
  hold_V2 = mkV2 I.tenir_V for_Prep ; ----
  arbitrary_A = mkA "arbitraire" ;
  set_N = mkN "ensemble" masculine ;

  iff_Subj : Subj = Mk.mkSubj "si et seulement si" ;
  commaConj : Conj = Mk.mkConj "," ;

  by_cases_Str = "par des cas" ;
  proof_Str = "démonstration" ;
  axiom_Str = "axiome" ;
  theorem_Str = "théorème" ;
  definition_Str = "définition" ;

  instance_N = mkN "instance" ;
  prove_VS = mkVS (mkV "démontrer") ;
  
  as_Prep : Prep = mkPrep "comme" ;

  let_Str : Bool => Str = table {False => "soit" ; True => "soient"} ;
  assuming_Str = "en supposant :" ;

}