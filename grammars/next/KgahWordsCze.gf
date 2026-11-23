concrete KgahWordsCze of KgahWords = CategoriesCze **

-- written by Claude, to be reviewed: the Czech of KgahWordsEng

open UtilitiesCze, SyntaxCze, ParadigmsCze, (G = GrammarCze) in {
oper
  -- "reálné číslo"
  reals : CN = mkCN (mkA "reálný") cislo_N ;
  ofReals : Adv = SyntaxCze.mkAdv possess_Prep (mkNP aPl_Det reals) ;
lin real_object_Noun = reals ;
lin set_of_reals_Noun = mkCN (mkCN mnozina_N) ofReals ;
lin relation_between_sets_of_reals_and_reals_Noun =
  mkCN (mkCN (mkN "relace" "relace" feminine))
       (SyntaxCze.mkAdv (mkPrep "mezi" instrumental)
          (mkNP and_Conj (mkNP aPl_Det (mkCN (mkCN mnozina_N) ofReals)) (mkNP aPl_Det reals))) ;
lin force_Verb = UtilitiesCze.mkVerb (mkV "vynucovat") ;
lin closed_downwards_Adj = G.AdvAP (mkAP (mkA "uzavřený")) (ParadigmsCze.mkAdv "směrem dolů") ;
}
