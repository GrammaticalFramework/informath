--# -path=.:morphodict:extraction

concrete MizarWordsPol of MizarWords = CategoriesPol **

open
  UtilitiesPol,
  SyntaxPol,
  ParadigmsPol,
  (R=ResPol),
  ExamplesPol,
  WikidataWordsPol

in {

-- Terminology reused from WikidataWordsPol wherever the sense agrees, as
-- MizarWordsEng reuses WikidataWordsEng. in_Prep ("w" + locative) and
-- between_Prep ("między" + instrumental) come from StructuralPol.

lin
    subseteq_comparable_AdjC = mkAdj (mkA "$\\subseteq$-porównywalny") ;

-- Mizar's "X misses Y" is X ∩ Y = ∅ and "X meets Y" is X ∩ Y ≠ ∅. Polish
-- says "X przecina Y" for the second; the first has no single idiomatic verb
-- (it is normally "X jest rozłączny z Y"), so "omijać" stands in for it.
    miss_Verb2 = mkVerb2 (mkV "omijać") (mkPrep "" R.Acc) ;
    meet_Verb2 = mkVerb2 (mkV "przecinać") (mkPrep "" R.Acc) ;

    strongly_connected_Adj = AdverbAdjAdj strongly_Adverb connected_Adj ;

    reflexive_in_Adj2 = mkAdj2 <lin AP reflexive_Adj : AP> in_Prep ;
    irreflexive_in_Adj2 = mkAdj2 (mkA "przeciwzwrotny") in_Prep ;
    symmetric_in_Adj2 = mkAdj2 symmetric_Adj in_Prep ;
    antisymmetric_in_Adj2 = mkAdj2 antisymmetric_Adj in_Prep ;
    asymmetric_in_Adj2 = mkAdj2 asymmetric_Adj in_Prep ;
    connected_in_Adj2 = mkAdj2 connected_Adj in_Prep ;
    strongly_connected_in_Adj2 = mkAdj2 strongly_connected_Adj in_Prep ;
    transitive_in_Adj2 = mkAdj2 transitive_Adj in_Prep ;

-- A classifying adjective follows its noun in Polish: "relacja binarna",
-- "struktura topologiczna". AdjNounNoun would preposition it.
    binary_relation_Noun = postAdjCN "binarny" relation_Noun ;

-- Mizar's "field of a relation" (dom R ∪ rng R) is "pole relacji"; it is not
-- the algebraic field, which WikidataWordsPol renders as "ciało".
    field_Fun = NounPrepFun (mkNoun (neutEN "pole")) possess_Prep ;

    family_of_subsets_Dep =
      NounPrepDep (mkCN family_Noun (PrepNounAdv possess_Prep subset_Noun)) possess_Prep ;

    topological_structure_Noun = postAdjCN "topologiczny" structure_Noun ;
    topology_Fun = NounPrepFun topology_Noun possess_Prep ;
    carrier_Fun = NounPrepFun (mkNoun (mascN "nośnik" "nośnika")) possess_Prep ;
    one_sorted_Noun = postAdjCN "jednosortowy" structure_Noun ;
    isomorphic_AdjC = AdjAdjC (mkAdj (mkA "izomorficzny")) ;
    isomorphism_DepC = NounPrepDepC isomorphism_Noun between_Prep ;

oper
  strongly_Adverb : Adverb = ParadigmsPol.mkAdv "silnie" ;

}
