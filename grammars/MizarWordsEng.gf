--# -path=.:morphodict:extraction
concrete MizarWordsEng of MizarWords = CategoriesEng **

open UtilitiesEng, MathWordsEng, SyntaxEng, NaprocheWordsEng, ExamplesEng, WikidataWordsEng in {
  lin
    subseteq_comparable_AdjC = mkAdj "$\\subseteq$-comparable";
    miss_Verb2 = mkVerb2 "miss";
    meet_Verb2 = mkVerb2 meet_V;
    strongly_connected_Adj = AdverbAdjAdj strongly_Adv connected_Adj ;
    reflexive_in_Adj2 = mkAdj2 <lin AP reflexive_Adj : AP> in_Prep ;
    irreflexive_in_Adj2 = mkAdj2 irreflexive_Adj in_Prep ;
    symmetric_in_Adj2 = mkAdj2 symmetric_Adj in_Prep ;
    antisymmetric_in_Adj2 = mkAdj2 antisymmetric_Adj in_Prep ;
  	asymmetric_in_Adj2 = mkAdj2 asymmetric_Adj in_Prep ;
  	connected_in_Adj2 = mkAdj2 connected_Adj in_Prep ;
  	strongly_connected_in_Adj2 = mkAdj2 strongly_connected_Adj in_Prep ;
  	transitive_in_Adj2 = mkAdj2 transitive_Adj in_Prep ;
    binary_relation_Noun = AdjNounNoun binary_Adj relation_Noun ;
    field_Fun = NounPrepFun field_Noun possess_Prep ;

}