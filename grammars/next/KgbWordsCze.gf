concrete KgbWordsCze of KgbWords = CategoriesCze **

-- written by Claude, to be reviewed: the Czech of KgbWordsEng

open UtilitiesCze, SyntaxCze, ParadigmsCze, (G = GrammarCze) in {
lin proof_of_well_ordering_Noun =
  mkCN (mkCN (hradN "důkaz")) (SyntaxCze.mkAdv possess_Prep (mkNP (mkN "dobré uspořádání" "dobrého uspořádání" neuter))) ;
lin accompanying_functional_Noun = mkCN (mkA "průvodní") (hradN "funkcionál") ;
lin proof_functional_Noun = mkCN (mkA "důkazový") (hradN "funkcionál") ;
lin inflationary_Adj = mkAP (mkA "inflační") ;
}
