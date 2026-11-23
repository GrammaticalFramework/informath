concrete NaprocheWordsCze of NaprocheWords = CategoriesCze **

-- partial: the words that the Mathlib informalization uses

open UtilitiesCze, ParadigmsCze, (S=SyntaxCze) in {

lin equal_Adj = mkAdj (mkA "rovný") ;
lin irreflexive_Adj = mkAdj (mkA "ireflexivní") ;
lin subsingleton_Noun = mkNoun (mkN "subsingleton" "subsingletonu" mascInanimate) ;
lin powerset_Noun = mkNoun (S.mkCN (mkA "potenční") mnozina_N) ;
lin negation_Noun = mkNoun (mkN "negace" "negace" feminine) ;
lin meet_Noun = mkNoun (mkN "průsek" "průseku" mascInanimate) ;

-- BEGIN the rest of the Naproche words, written by Claude, to be reviewed
lin Euclidean_Adj = mkAdj (mkA "eukleidovský") ;
lin arbitrary_Adj = mkAdj (mkA "libovolný") ;
lin comparable_Adj = mkAdj (mkA "srovnatelný") ;
lin composable_Adj = mkAdj (mkA "složitelný") ;
lin connex_Adj = mkAdj (mkA "konexní") ;
lin coreflexive_Adj = mkAdj (mkA "koreflexivní") ;
lin desugar_Adj = mkAdj (mkA "rozvinutý") ;
lin equinumerous_Adj = mkAdj (mkA "stejně mohutný") ;
lin 'in-transitive_Adj' = mkAdj (mkA "$\\in$-tranzitivní") ;
lin 'non-relational_Adj' = mkAdj (mkA "nerelační") ;
lin particular_Adj = mkAdj (mkA "konkrétní") ;
lin quasiconnex_Adj = mkAdj (mkA "kvazikonexní") ;
lin quasiorder_Adj = mkAdj (mkA "kvaziuspořádaný") ;
lin quasiordered_Adj = mkAdj (mkA "kvaziuspořádaný") ;
lin quasireflexive_Adj = mkAdj (mkA "kvazireflexivní") ;
lin same_Adj = mkAdj (mkA "stejný") ;
lin set_Adj = mkAdj (mkA "množinový") ;
lin setminus_Adj = mkAdj (mkA "rozdílový") ;
lin straightforward_Adj = mkAdj (mkA "přímočarý") ;
lin subfinite_Adj = mkAdj (mkA "subkonečný") ;
lin usual_Adj = mkAdj (mkA "obvyklý") ;
lin abbreviaton_Noun = mkNoun (mkN "zkratka" "zkratky" feminine) ;
lin application_Noun = mkNoun (mkN "aplikace" "aplikace" feminine) ;
lin assumption_Noun = mkNoun (mkN "předpoklad" "předpokladu" mascInanimate) ;
lin asymmetry_Noun = mkNoun (mkN "asymetrie" "asymetrie" feminine) ;
lin automation_Noun = mkNoun (mkN "automatizace" "automatizace" feminine) ;
lin axiom_Noun = mkNoun (mkN "axiom" "axiomu" mascInanimate) ;
lin bipartition_Noun = mkNoun (mkN "bipartice" "bipartice" feminine) ;
lin carrier_Noun = mkNoun (mkN "nosič" "nosiče" mascInanimate) ;
lin connex_Noun = mkNoun (mkN "konexe" "konexe" feminine) ;
lin construct_Noun = mkNoun (mkN "konstrukt" "konstruktu" mascInanimate) ;
lin contradiction_Noun = mkNoun (mkN "spor" "sporu" mascInanimate) ;
lin definition_Noun = mkNoun (mkN "definice" "definice" feminine) ;
lin detachment_Noun = mkNoun (mkN "odloučení" "odloučení" neuter) ;
lin disconnection_Noun = mkNoun (mkN "rozpojení" "rozpojení" neuter) ;
lin example_Noun = mkNoun (mkN "příklad" "příkladu" mascInanimate) ;
lin expression_Noun = mkNoun (mkN "výraz" "výrazu" mascInanimate) ;
lin extensionality_Noun = mkNoun (mkN "extenzionalita" "extenzionality" feminine) ;
lin fixpoint_Noun = mkNoun (S.mkCN (mkA "pevný") (mkN "bod" "bodu" mascInanimate)) ;
lin goal_Noun = mkNoun (mkN "cíl" "cíle" mascInanimate) ;
lin implosion_Noun = mkNoun (mkN "imploze" "imploze" feminine) ;
lin irreflexivity_Noun = mkNoun (mkN "ireflexivita" "ireflexivity" feminine) ;
lin ordertopology_Noun = mkNoun (S.mkCN (mkN "topologie" "topologie" feminine) (ParadigmsCze.mkAdv "uspořádání")) ;
lin prebasis_Noun = mkNoun (mkN "subbáze" "subbáze" feminine) ;
lin preclosure_Noun = mkNoun (mkN "předuzávěr" "předuzávěru" mascInanimate) ;
lin quasiorder_Noun = mkNoun (mkN "kvaziuspořádání" "kvaziuspořádání" neuter) ;
lin submagma_Noun = mkNoun (mkN "podmagma" "podmagmatu" neuter) ;
lin subseteq_Noun = mkNoun (mkN "podmnožina" "podmnožiny" feminine) ;
lin suppose_Noun = mkNoun (mkN "předpoklad" "předpokladu" mascInanimate) ;
lin surjection_Noun = mkNoun (mkN "surjekce" "surjekce" feminine) ;
lin tuple_Noun = mkNoun (mkN "n-tice" "n-tice" feminine) ;
-- END the rest of the Naproche words

}
