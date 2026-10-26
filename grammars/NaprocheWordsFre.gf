concrete NaprocheWordsFre of NaprocheWords = CategoriesFre **

-- partial: the words that the Mathlib informalization uses

open UtilitiesFre, ParadigmsFre in {

lin equal_Adj = mkAdj (mkA "égal" "égale" "égaux" "égales") ;
lin irreflexive_Adj = mkAdj (mkA "irréflexif" "irréflexive") ;
lin subsingleton_Noun = mkNoun (mkN "sous-singleton" masculine) ;

-- BEGIN words of the Mathlib definitions, written by Claude, to be reviewed
lin Euclidean_Adj = mkAdj (mkA "euclidien" "euclidienne") ;
lin application_Noun = mkNoun (mkN "application" feminine) ;
lin arbitrary_Adj = mkAdj (mkA "arbitraire") ;
lin axiom_Noun = mkNoun (mkN "axiome" masculine) ;
lin carrier_Noun = mkNoun (mkN "ensemble sous-jacent" "ensembles sous-jacents" masculine) ;
lin composable_Adj = mkAdj (mkA "composable") ;
lin construct_Noun = mkNoun (mkN "construction" feminine) ;
lin coreflexive_Adj = mkAdj (mkA "coréflexif" "coréflexive") ;
lin definition_Noun = mkNoun (mkN "définition" feminine) ;
lin disconnection_Noun = mkNoun (mkN "déconnexion" feminine) ;
lin expression_Noun = mkNoun (mkN "expression" feminine) ;
lin fixpoint_Noun = mkNoun (mkN "point fixe" "points fixes" masculine) ;
lin negation_Noun = mkNoun (mkN "négation" feminine) ;
lin powerset_Noun = mkNoun (mkN "ensemble des parties" "ensembles des parties" masculine) ;
lin same_Adj = mkAdj (mkA "même" "même" "mêmes") ;
lin set_Adj = mkAdj (mkA "ensembliste") ;
lin surjection_Noun = mkNoun (mkN "surjection" feminine) ;
lin tuple_Noun = mkNoun (mkN "n-uplet" masculine) ;
-- END words of the Mathlib definitions

-- a word of the Godement symbol tables, written by Claude, to be reviewed
lin usual_Adj = mkAdj (mkA "usuel") ;

}
