concrete MizarWordsCze of MizarWords = CategoriesCze **

-- written by Claude, to be reviewed: the Czech of MizarWordsEng

open UtilitiesCze, SyntaxCze, ParadigmsCze, (G = GrammarCze) in {

oper
  acc_Prep : Prep = mkPrep "" accusative ;
  vLoc_Prep : Prep = mkPrep "v" locative ;
  relation_N : N = mkN "relace" "relace" feminine ;

lin
  subseteq_comparable_AdjC = mkAdjC (mkA "$\\subseteq$-srovnatelný") ;
  miss_Verb2 = mkVerb2 (mkV "míjet") acc_Prep ;
  meet_Verb2 = mkVerb2 (mkV "protínat") acc_Prep ;
  strongly_connected_Adj = mkAdj (mkA "silně souvislý") ;
  reflexive_in_Adj2 = mkAdj2 (mkA "reflexivní") vLoc_Prep ;
  irreflexive_in_Adj2 = mkAdj2 (mkA "ireflexivní") vLoc_Prep ;
  symmetric_in_Adj2 = mkAdj2 (mkA "symetrický") vLoc_Prep ;
  antisymmetric_in_Adj2 = mkAdj2 (mkA "antisymetrický") vLoc_Prep ;
  asymmetric_in_Adj2 = mkAdj2 (mkA "asymetrický") vLoc_Prep ;
  connected_in_Adj2 = mkAdj2 (mkA "souvislý") vLoc_Prep ;
  strongly_connected_in_Adj2 = mkAdj2 (mkA "silně souvislý") vLoc_Prep ;
  transitive_in_Adj2 = mkAdj2 (mkA "tranzitivní") vLoc_Prep ;
  binary_relation_Noun = mkCN (mkA "binární") relation_N ;
  -- the field of a relation: "pole relace R"
  field_Fun = mkFun (mestoN "pole") possess_Prep ;
  family_of_subsets_Dep = {cn = mkCN (mkCN (zenaN "rodina")) (SyntaxCze.mkAdv possess_Prep (mkNP aPl_Det (mkN "podmnožina" "podmnožiny" feminine))) ; prep = possess_Prep} ;
  topological_structure_Noun = mkCN (mkA "topologický") (zenaN "struktura") ;
  topology_Fun = mkFun (mkN "topologie" "topologie" feminine) possess_Prep ;
  carrier_Fun = mkFun (mkN "nosič" "nosiče" mascInanimate) possess_Prep ;
  one_sorted_Noun = mkCN (mkA "jednosortový") (zenaN "struktura") ;
  isomorphic_AdjC = mkAdjC (mkA "izomorfní") ;
  isomorphism_DepC = {cn = mkCN (mkN "izomorfismus" "izomorfismu" mascInanimate) ; prep = mkPrep "mezi" instrumental} ;
  function_like_Adj = mkAdj (mkA "funkční") ;
  finer_than_Adj2 = mkAdj2 (mkA "jemnější") (mkPrep "než" nominative) ;
  coarser_than_Adj2 = mkAdj2 (mkA "hrubší") (mkPrep "než" nominative) ;
  well_order_Verb2 = mkVerb2 (mkV "dobře uspořádávat") acc_Prep ;
  well_ordering_Adj = mkAdj (mkA "dobře uspořádávající") ;
  compatible_Adj2 = mkAdj2 (mkA "kompatibilní") (mkPrep "s" instrumental) ;

}
