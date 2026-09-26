instance UtilitiesGodementEng of UtilitiesGodement =
  open SyntaxEng, ParadigmsEng, (I=IrregEng), (X=ExtraEng), SymbolEng, Prelude in {

oper
  say_V2 = mkV2 I.say_V ;
  call_V2 = mkV2 (mkV "call") ;
  defineG_V2 = mkV2 (mkV "define") ;
  denote_V2 = mkV2 (mkV "denote") ;
  endow_V2 = mkV2 (mkV "endow") with_Prep ;
  by_definition_AdV = mkAdV "by definition" ;
  resp_Str = "resp" ++ "." ;
  or_Str = "or" ;
  namely_Str = "namely" ;
  following_A = mkA "following" ;
  unique_A = mkA "unique" ;
  necessary_A = mkA "necessary" ;
  sufficient_A = mkA "sufficient" ;
  equivalent_A = mkA "equivalent" ;
  condition_N = mkN "condition" ;
  property_N = mkN "property" "properties" ;
  form_N = mkN "form" ;
  numberG_N = mkN "number" ;
  whereG_Subj = mkSubj "where" ;
  whenever_Subj = mkSubj "whenever" ;
  given_Prep = mkPrep "given" ;
  so_is_Str = "so is" ;
  exactly_AdN = mkAdN "exactly" ;
  oneG_Card = mkCard "1" ;
  twoG_Card = mkCard "2" ;
  threeG_Card = mkCard "3" ;
  any_Det = mkDet X.any_Quant ;
  called_Str = "called" ;
  denoted_by_Str = "denoted by" ;
  endowed_with_Str = "endowed with" ;
  termCard s = SymbolEng.SymbNum (SymbolEng.MkSymb (ss s)) ;
  symbS s = SymbolEng.SymbS (SymbolEng.MkSymb (ss s)) ;

}
