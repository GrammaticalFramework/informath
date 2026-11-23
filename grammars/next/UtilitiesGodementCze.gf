instance UtilitiesGodementCze of UtilitiesGodement =
  open SyntaxCze, ParadigmsCze, (Mk=ParadigmsCze), SymbolCze, Prelude in {

-- written by Claude, to be reviewed: the Czech of UtilitiesGodementEng

oper
  say_V2 = mkV2 (mkV "říkat") ;
  -- the Czech passive is reflexive: "E se nazývá X"
  call_V2 = mkV2 (mkV "nazývat") ;
  defineG_V2 = mkV2 (mkV "definovat") ;
  denote_V2 = mkV2 (mkV "označovat") ;
  endow_V2 = mkV2 (mkV "opatřovat") instrumental ;
  by_definition_AdV = mkAdV "z definice" ;
  resp_Str = "resp" ++ "." ;
  or_Str = "neboli" ;
  namely_Str = "totiž" ;
  following_A = mkA "následující" ;
  unique_A = mkA "jediný" ;
  necessary_A = mkA "nutný" ;
  sufficient_A = mkA "postačující" ;
  equivalent_A = mkA "ekvivalentní" ;
  condition_N = zenaN "podmínka" ;
  property_N = mkN "vlastnost" "vlastnosti" feminine ;
  form_N = hradN "tvar" ;
  numberG_N = mkN "počet" "počtu" mascInanimate ;
  whereG_Subj = mkSubj "kde" ;
  whenever_Subj = mkSubj ", kdykoli" ;
  given_Prep = mkPrep "máme-li" accusative ;
  so_is_Str = "totéž platí pro" ;
  exactly_AdN = mkAdN "právě" ;
  -- the numerals as words, with their agreement: "alespoň dvě podgrupy"
  oneG_Card = mkCard (mkNumeral n1_Unit) ;
  twoG_Card = mkCard (mkNumeral n2_Unit) ;
  threeG_Card = mkCard (mkNumeral n3_Unit) ;
  any_Det = mkDet (Mk.mkQuant "libovolný") ;
  called_Str = "zvaný" ;
  denoted_by_Str = "označovaný" ;
  endowed_with_Str = "spolu s" ;
  termCard s = SymbolCze.SymbNum (SymbolCze.MkSymb (ss s)) ;
  symbS s = SymbolCze.SymbS (SymbolCze.MkSymb (ss s)) ;

}
