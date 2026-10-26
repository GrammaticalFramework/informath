instance UtilitiesGodementFre of UtilitiesGodement =
  open SyntaxFre, ParadigmsFre, (I=IrregFre), (X=ExtraFre), (Mk=MakeStructuralFre), SymbolFre, Prelude in {

-- written by Claude, to be reviewed: the French of UtilitiesGodementEng,
-- in the words of Godement's Cours d'algèbre

oper
  say_V2 = I.dire_V2 ;
  call_V2 = mkV2 (mkV "appeler") ;
  defineG_V2 = mkV2 (mkV "définir") ;
  denote_V2 = mkV2 (mkV "désigner") ;
  endow_V2 = mkV2 (mkV "munir") (mkPrep "de") ;
  by_definition_AdV = mkAdV "par définition" ;
  resp_Str = "resp" ++ "." ;
  or_Str = "ou" ;
  namely_Str = "à savoir" ;
  following_A = mkA "suivant" ;
  unique_A = mkA "unique" ;
  necessary_A = mkA "nécessaire" ;
  sufficient_A = mkA "suffisant" ;
  equivalent_A = mkA "équivalent" ;
  condition_N = mkN "condition" feminine ;
  property_N = mkN "propriété" feminine ;
  form_N = mkN "forme" feminine ;
  numberG_N = mkN "nombre" masculine ;
  whereG_Subj = Mk.mkSubj "où" ;
  whenever_Subj = Mk.mkSubj "chaque fois que" ;
  given_Prep = mkPrep "étant donné" ;
  so_is_Str = "il en est de même de" ;
  exactly_AdN = lin AdN {s = "exactement"} ;
  oneG_Card = mkCard "1" ;
  twoG_Card = mkCard "2" ;
  threeG_Card = mkCard "3" ;
  any_Det = mkDet (Mk.mkQuant "tout" "toute" "tous" "toutes") ;
  called_Str = "appelé" ;
  denoted_by_Str = "désigné par" ;
  endowed_with_Str = "muni de" ;
  termCard s = SymbolFre.SymbNum (SymbolFre.MkSymb (ss s)) ;
  symbS s = SymbolFre.SymbS (SymbolFre.MkSymb (ss s)) ;

}
