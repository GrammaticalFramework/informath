concrete VerbalConstantsFin of VerbalConstants = CategoriesFin **

open
UtilitiesFin,
SyntaxFin,
ParadigmsFin,
SymbolicFin,
Prelude

in {



lin proposition_Noun = mkNoun "propositio" ;

lin digit_Noun = mkNoun "numero" ;

lin boolean_Noun = mkNoun "totuusarvo" ;

lin natural_Noun = mkNoun "luonnollinen" "luku" ;

lin rational_Noun = mkNoun "rationaaliluku" ;
lin real_Noun = mkNoun "reaaliluku" ;


lin list_Fam = mkFam "lista" ;
lin set_Fam = mkFam "joukko" ;

lin Eq_Adj2 = mkAdj2 "yhtä suuri" "kuin" ;
lin Eq_AdjE = mkAdjE (mkA "yhtä suuri") ;
lin Lt_Adj2 = mkAdj2 "pienempi" "kuin" ;
lin Gt_Adj2 = mkAdj2 "suurempi" "kuin" ;
lin Neq_Adj2 = mkAdj2 "eri suuri" "kuin" ;
lin Neq_AdjC = mkAdjC (mkA "erisuuri") ;
lin Leq_Adj2 = mkAdj2 "pienempi tai yhtä suuri" "kuin" ;
lin Geq_Adj2 = mkAdj2 "suurempi tai yhtä suuri" "kuin" ;




lin converge_Verb = mkVerb "supeta" ;
lin divide_Verb2 = mkVerb2 "jakaa" ;

lin member_Noun2 = mkNoun2 "jäsen" ;
lin divisor_Noun2 = mkNoun2 "jakaja" ;

lin plus_FunC = mkFun "summa" ;
lin minus_Fun2 = mkFun2 (mkN "erotus") (mkPrep "välillä") ;
lin times_FunC = mkFun "tulo" ;
lin div_Fun2 = mkFun2 (mkN "osamäärä") possess_Prep ;
lin pow_Fun2 = mkFun2 "korotus" "potenssiin";
lin neg_Fun = mkFun "vastaluku" ;
lin logarithm_Fun2 = mkFun2 "logaritmi" "kannassa" ;
lin square_root_Fun = mkFun "neliöjuuri" ;

lin successor_Fun = mkFun "seuraaja" ;
lin absolute_value_Fun = mkFun "itseisarvo" ;
lin factorial_Fun = mkFun "kertoma" ;
lin gcd_FunC = mkFun "suurin" "yhteinen" "tekijä" ;



lin divisible_Adj2 = mkAdj2 "jaollinen" "luvulla" ;


lin function_Fam2 = mkFam2 "funktio" from_Prep to_Prep ;
lin union_FunC = mkFun "yhdiste" ;
lin intersection_FunC = mkFun "leikkaus" ;
lin difference_Fun2 = mkFun2 (mkN "erotus") possess_Prep ;
lin complement_Fun = mkFun "komplementti" ;
lin cartesian_FunC = mkFun "karteesinen" "tulo" ;
lin powerset_Fun = mkFun "potenssijoukko" ;

lin subset_Noun2 = mkNoun2 "aito" "osajoukko" ;
lin subseteq_Noun2 = mkNoun2 "osajoukko" ;
lin superset_Noun2 = mkNoun2 "aito" "ylijoukko" ;
lin superseteq_Noun2 = mkNoun2 "ylijoukko" ;
lin equalset_Adj2 = mkAdj2 "sama" "kuin" ;
lin notequalset_Adj2 = mkAdj2 "eri" "kuin" ; ----
lin element_Noun2 = mkNoun2 "alkio" ;
lin notelement_Noun2 = mkNoun2 "ei-alkio" ; ----

lin emptyset_Name = mkNP the_Det (mkCN (mkA "tyhjä") (mkN "joukko")) ;
lin universeset_Name = mkNP the_Det (mkCN (mkA "universaali") (mkN "joukko")) ;

lin congruent_Adj3 = mkAdj3 (mkAP (mkA "kongruentti")) to_Prep (mkPrep "modulo") ;




lin combinationsFromSet_Fun2 = mkFun2 "kombinaatioiden lukumäärä" "kokoa" ;
lin combinations_Fun2 = mkFun2 "kombinaatioiden joukko" "kokoa" ;
lin binomial_Fun2 = mkFun2 "binomikerroin" "yli" ;
lin area_Fun = mkFun "pinta-ala" ;
lin radius_Fun = mkFun "säde" ;

lin pi_Name = mkNP the_Det (mkCN (mkN "luku") (symb "\\(\\pi\\)")) ;
lin legendre_symbol_Fun2 = mkFun2 "Legendren symboli" "yli" ;
lin square_Fun = mkFun "neliö" ;
lin resultant_FunC = mkFun "resultantti" ;
lin perpendicular_Adj2 = mkAdj2 "kohtisuora" "vastaan" ;
lin length_Fun = mkFun "pituus" ;
lin norm_Fun = mkFun "normi" ;

lin denumerable_Adj = mkAdj "numeroituva" ;
lin cardinality_Fun = mkFun "mahtavuus" ;
lin root_Noun2 = mkNoun2 "juuri" ;
lin degree_Fun = mkFun "aste" ;

lin irrational_Adj = mkAdj "irrationaalinen" ;

lin sin_Fun = mkFun "sini" ;
lin cos_Fun = mkFun "kosini" ;
lin tan_Fun = mkFun "tangentti" ;
lin arcsin_Fun = mkFun "arkussini" ;
lin arccos_Fun = mkFun "arkuskosini" ;
lin arctan_Fun = mkFun "arkustangentti" ;
lin orthogonal_Adj2 = mkAdj2 "ortogonaalinen" "vastaan" ;
lin orthogonal_AdjC = mkAP (mkA "ortogonaalinen") ;
lin perpendicular_Adj2 = mkAdj2 "kohtisuora" "vastaan" ;
lin perpendicular_AdjC = mkAdjC (mkA "kohtisuora") ;
lin angle_between_Fun2 = mkFun2 (mkN "kulma") (mkPrep "välillä") ;
lin dot_product_FunC = mkFunC "pistetulo" ;
lin vector_plus_FunC = mkFunC "summa" ;

lin everywhere_Adverb = mkAdv "kaikkialla" ;
lin almost_everywhere_Adverb = mkAdv "melkein kaikkialla" ;
lin uniformly_Adverb = mkAdv "tasaisesti" ;
lin non_Adverb = mkAdv "ei" ;

}
