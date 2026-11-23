concrete MytextWordsCze of MytextWords = CategoriesCze **

-- written by Claude, to be reviewed: the Czech of MytextWordsEng

open UtilitiesCze, SyntaxCze, ParadigmsCze, (G = GrammarCze) in {

oper
  -- the genitive plural: "okruh podmnožin"
  ofPl : CN -> Adv = \cn -> SyntaxCze.mkAdv possess_Prep (mkNP aPl_Det cn) ;
  subsets : Adv = ofPl (mkCN (mkN "podmnožina" "podmnožiny" feminine)) ;
  algebra_N : N = zenaN "algebra" ;
  ring_N : N = mkN "okruh" "okruhu" mascInanimate ;
  sequence_N : N = zenaN "posloupnost" ;
  class_N : N = zenaN "třída" ;
lin sigma_algebra_Noun = mkCN (mkN "σ-algebra" "σ-algebry" feminine) ;
lin sigma_ring_Noun = mkCN (mkN "σ-okruh" "σ-okruhu" mascInanimate) ;
lin ring_of_subsets_Noun = mkCN (mkCN ring_N) subsets ;
lin algebra_of_subsets_Noun = mkCN (mkCN algebra_N) subsets ;
lin sigma_ring_of_subsets_Noun = mkCN (mkCN (mkN "σ-okruh" "σ-okruhu" mascInanimate)) subsets ;
lin sigma_algebra_of_subsets_Noun = mkCN (mkCN (mkN "σ-algebra" "σ-algebry" feminine)) subsets ;
lin class_of_subsets_Noun = mkCN (mkCN class_N) subsets ;
lin sequence_of_subsets_Noun = mkCN (mkCN sequence_N) subsets ;
lin sequence_of_elements_Noun = mkCN (mkCN sequence_N) (ofPl (mkCN element_N)) ;
lin family_of_classes_of_subsets_Noun = mkCN (mkCN (zenaN "rodina")) (ofPl (mkCN (mkCN class_N) subsets)) ;
-- "uzavřený na rozdíly"
oper closedUnder : CN -> AP = \cn -> G.AdvAP (mkAP (mkA "uzavřený")) (SyntaxCze.mkAdv (mkPrep "na" accusative) (mkNP aPl_Det cn)) ;
lin closed_under_differences_Adj = closedUnder (mkCN (mkN "rozdíl" "rozdílu" mascInanimate)) ;
lin closed_under_unions_Adj = closedUnder (mkCN (mkN "sjednocení" "sjednocení" neuter)) ;
lin closed_under_intersections_Adj = closedUnder (mkCN (mkN "průnik" "průniku" mascInanimate)) ;
lin closed_under_symmetric_differences_Adj = closedUnder (mkCN (mkA "symetrický") (mkN "rozdíl" "rozdílu" mascInanimate)) ;
lin closed_under_complements_Adj = closedUnder (mkCN (mkN "doplněk" "doplňku" mascInanimate)) ;
lin closed_under_countable_unions_Adj = closedUnder (mkCN (mkA "spočetný") (mkN "sjednocení" "sjednocení" neuter)) ;
}
