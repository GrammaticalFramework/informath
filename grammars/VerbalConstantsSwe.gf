concrete VerbalConstantsSwe of VerbalConstants = CategoriesSwe **

open
UtilitiesSwe,
SyntaxSwe,
ParadigmsSwe,
SymbolicSwe,
Prelude

in {



lin proposition_Noun = mkNoun "påstående" ;

lin digit_Noun = mkNoun "siffra" ;

lin boolean_Noun = mkNoun "sanningsvärde" ;

lin natural_Noun = mkNoun (mkA "naturlig") tal_N ;

lin rational_Noun = mkNoun (mkA "rationell") tal_N ;
lin real_Noun = mkNoun (mkA "reell") tal_N ;


lin list_Fam = mkFam "lista" ;
lin set_Fam = mkFam mängd_N ;

lin Eq_Adj2 = mkAdj2 "lika" "med" ;
lin Eq_AdjE = mkAP (mkA "lika") ;
lin Lt_Adj2 = mkAdj2 "mindre" "än" ;
lin Gt_Adj2 = mkAdj2 "större" "än" ;
lin Neq_Adj2 = mkAdj2 "skild" "från" ;
lin Neq_AdjC = mkAP (mkA "olik") ;
lin Leq_Adj2 = mkAdj2 "mindre än eller lika" "med" ;
lin Geq_Adj2 = mkAdj2 "större än eller lika" "med" ;




lin converge_Verb = mkVerb (mkV "konvergera") ;
lin divide_Verb2 = mkVerb2 "dela" ;

lin member_Noun2 = mkNoun2 element_N ;
lin divisor_Noun2 = mkNoun2 (mkN "delare") ;

lin plus_FunC = mkFunC "summa" ;
lin minus_Fun2 = mkFun2 (mkN "skillnad" "skillnader") (mkPrep "mellan") ;
lin times_FunC = mkFunC (mkN "produkt" "produkter") ;
lin div_Fun2 = mkFun2 (mkN "kvot" "kvoter") (mkPrep "mellan") ; ----
lin pow_Fun2 = mkFun2 (mkN "potens" "potenser") possess_Prep (mkPrep "upphöjt till") ; ----
lin neg_Fun = mkFun (mkN "negation" "negationer") ;
lin logarithm_Fun2 = mkFun2 (mkN "logaritm" "logaritmer") (mkPrep "i bas") possess_Prep ;
lin square_root_Fun = mkFun (mkN "kvadratrot" "kvadratrötter") ;

lin successor_Fun = mkFun (mkN "efterföljare" neutrum) ;
lin absolute_value_Fun = mkFun (mkN "absolutbelopp" neutrum) ;
lin factorial_Fun = mkFun (mkN "fakultet" "fakulteter") ;
lin gcd_FunC = mkFunC "störst" "gemensam" "delare" ;



lin divisible_Adj2 = mkAdj2 "delbar" "med" ;


lin function_Fam2 = mkFam2 (mkN "funktion" "funktioner") from_Prep to_Prep ;
lin union_FunC = mkFun "union" ;
lin intersection_FunC = mkFun (mkN "snitt" "snitt") ;
lin difference_Fun2 = mkFun2 (mkN "skillnad" "skillnader") (mkPrep "mellan") ;
lin complement_Fun = mkFun (mkN "komplement" "komplement") ;
lin cartesian_FunC = mkFun (mkCN (mkA "kartesisk") (mkN "produkt")) ;
lin powerset_Fun = mkFun (mkN "potens" mängd_N) ;

lin subset_Noun2 = mkNoun2 (mkCN (mkA "äkta") (mkN "del" mängd_N)) ;
lin subseteq_Noun2 = mkNoun2 (mkN "del" mängd_N) ;
lin superset_Noun2 = mkNoun2 (mkCN (mkA "äkta") (mkN "över" mängd_N)) ;
lin superseteq_Noun2 = mkNoun2 (mkN "över" mängd_N) ;
lin equalset_Adj2 = mkAdj2 (mkA "lika") with_Prep ;
lin notequalset_Adj2 = mkAdj2 (mkA "skild") from_Prep ;
lin element_Noun2 = mkNoun2 element_N ;
lin notelement_Noun2 = mkNoun2 (mkN "icke-" element_N) ; ----

lin emptyset_Name = mkName (mkNP the_Det (mkCN tom_A mängd_N)) ;
lin universeset_Name = mkName (mkNP the_Det (mkN "universal" mängd_N)) ;

lin congruent_Adj3 = mkAdj3 (mkA "kongruent") with_Prep (mkPrep "modulo") ;




lin combinationsFromSet_Fun2 = mkFun2 (mkCN (mkN "an" tal_N) (SyntaxSwe.mkAdv (mkPrep "") (mkNP aPl_Det (mkN "kombination")))) from_Prep (mkPrep "av storlek") ;
lin combinations_Fun2 = mkFun2 (mkCN mängd_N (SyntaxSwe.mkAdv possess_Prep (mkNP aPl_Det (mkN "Kombination")))) from_Prep (mkPrep "av storlek") ;
lin binomial_Fun2 = mkFun2 (mkCN (mkN "binomialkoefficient" "binomialkoefficienter")) possess_Prep (mkPrep "över") ;

lin area_Fun = mkFun "area" ;
lin radius_Fun = mkFun "radie" ;

lin pi_Name = mkName (mkNP the_Det (mkCN tal_N (symb "\\(\\pi\\)"))) ;
lin legendre_symbol_Fun2 = mkFun2 (mkN "Legendresymbol" "Legendresymboler") possess_Prep (mkPrep "över") ;
lin square_Fun = mkFun (mkN "kvadrat" "kvadrater") ;
lin resultant_FunC = mkFunC (mkN "resultant" "resultanter") ;
lin perpendicular_Adj2 = mkAdj2 "vinkelrät" "mot" ;
lin perpendicular_AdjC = mkAdjC "vinkelrät" ;
lin length_Fun = mkFun (mkN "längd" "längder") ;
lin norm_Fun = mkFun (mkN "norm" "normer") ;

lin denumerable_Adj = mkAdj "uppräknelig" ;
lin cardinality_Fun = mkFun (mkN "kardinalitet" "kardinaliteter") ;
lin root_Noun2 = mkNoun2 (mkN "rot" "rötter") ;
lin degree_Fun = mkFun (mkN "grad" "grader") ;

lin irrational_Adj = mkAdj "irrationell" ;


lin sin_Fun = mkFun "sinus" ;
lin cos_Fun = mkFun "cosinus" ;
lin tan_Fun = mkFun "tangens" ;
lin arcsin_Fun = mkFun (mkN "arcsinus") ;
lin arccos_Fun = mkFun (mkN "arccosinus") ;
lin arctan_Fun = mkFun (mkN "arctangens") ;
lin orthogonal_Adj2 = mkAdj2 "ortogonal" "till" ;
lin orthogonal_AdjC = mkAdjC "ortogonal" ;
lin angle_between_Fun2 = mkFun2 (mkN "vinkel" "vinklar") (mkPrep "mellan") ;
lin dot_product_FunC = mkFunC "punktprodukt" ;
lin vector_plus_FunC = mkFunC "summa" ;


}
