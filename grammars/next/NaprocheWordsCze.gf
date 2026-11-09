concrete NaprocheWordsCze of NaprocheWords = CategoriesCze **

-- partial: the words that the Mathlib informalization uses

open UtilitiesCze, ParadigmsCze, (S=SyntaxCze) in {

lin equal_Adj = mkAdj (mkA "rovný") ;
lin irreflexive_Adj = mkAdj (mkA "ireflexivní") ;
lin subsingleton_Noun = mkNoun (mkN "subsingleton" "subsingletonu" mascInanimate) ;
lin powerset_Noun = mkNoun (S.mkCN (mkA "potenční") mnozina_N) ;
lin negation_Noun = mkNoun (mkN "negace" "negace" feminine) ;
lin meet_Noun = mkNoun (mkN "průsek" "průseku" mascInanimate) ;

}
