concrete VerbalConstantsGer of VerbalConstants = CategoriesGer **

open
UtilitiesGer,
SyntaxGer,
ParadigmsGer,
SymbolicGer,
Prelude

in {



lin proposition_Noun = mkNoun "Proposition" ;

lin digit_Noun = mkNoun (mkN "Ziffer" "Ziffer" masculine) ;

lin boolean_Noun = mkNoun "Wahrheitswert" ;


lin list_Fam = mkFam "Liste" ;
lin set_Fam = mkFam menge_N ;

lin natural_Noun = mkNoun (mkA "natürlich") zahl_N ;

lin rational_Noun = mkNoun (mkA "rational") zahl_N ;
lin real_Noun = mkNoun (mkA "reell") zahl_N ;


lin Eq_Adj2 = mkAdj2 "" "gleich" ;
lin Eq_AdjE = mkAdjE "gleich" ;
lin Lt_Adj2 = mkAdj2 "" "kleiner als" ; ---- ...
lin Gt_Adj2 = mkAdj2 "" "größer als" ;
lin Neq_Adj2 = mkAdj2 "" "ungleich" ; ----
lin Neq_AdjC = mkAdjC "ungleich" ; ----
lin Leq_Adj2 = mkAdj2 "" "kleiner oder gleich" ;
lin Geq_Adj2 = mkAdj2 "" "größer oder gleich" ;




lin converge_Verb = mkVerb (mkV "konvergieren") ;
lin divide_Verb2 = mkVerb2 (mkV "teilen") ;
lin member_Noun2 = mkNoun2 (mkN "Element" "Elemente" neuter) ;
lin divisor_Noun2 = mkNoun2 (mkN "Teiler") ;

lin plus_FunC = mkFunC "Summe" ;
lin minus_Fun2 = mkFun2 (mkN "Differenz" feminine) (mkPrep "zwischen" dative) ;
lin times_FunC = mkFunC (mkN "Produkt" neuter) ;
lin div_Fun2 = mkFun2 (mkN "Quotient" "Quotienten" masculine) between_Prep ;
lin pow_Fun2 = mkFun2 (mkN "Potenz" feminine) ; ----
lin neg_Fun = mkFun "Negation" ;
lin logarithm_Fun2 = mkFun2 "Logarithmus" ;
lin square_root_Fun = mkFun (mkN "Quadratwurzel" feminine) ;

lin successor_Fun = mkFun (mkN "Nachfolger" masculine) ;
lin absolute_value_Fun = mkFun (mkCN (mkA "absolut") (mkN "Wert" masculine)) ;
lin factorial_Fun = mkFun (mkN "Fakultät" feminine);
lin gcd_FunC = mkFunC "groß" "gemeinsam" "Teiler" ;



lin divisible_Adj2 = mkAdj2 (mkA "teilbar") (mkPrep "durch" accusative) ;


lin function_Fam2 = mkFam2 (mkCN (mkN "Funktion")) from_Prep to_Prep ;
lin union_FunC = mkFunC "Vereinigung" ;
lin intersection_FunC = mkFunC (mkN "Schnittmenge") ;
lin complement_Fun = mkFun (mkN "Komplement") ;
lin cartesian_FunC = mkFunC (mkCN (mkA "kartesische") (mkN "Produkt")) ;
lin difference_Fun2 = mkFun2 (mkN "Differenz" feminine);
lin powerset_Fun = mkFun "Potenzmenge" ;

lin subset_Noun2 = mkNoun2 (mkCN (mkA "echt") (mkN "Teilmenge"));
lin subseteq_Noun2 = mkNoun2 (mkN "Teilmenge") ;
lin superset_Noun2 = mkNoun2 (mkCN (mkA "echt") (mkN "Obermenge")) ;
lin superseteq_Noun2 = mkNoun2 (mkN "Obermenge") ;
lin equalset_Adj2 = mkAdj2 "" "gleich" ; ----
lin notequalset_Adj2 = mkAdj2 "" "ungleich" ; ----
lin element_Noun2 = mkNoun2 element_N ;
lin notelement_Noun2 = mkNoun2 (mkN "nicht-" element_N) ; ----

lin emptyset_Name = mkName (mkNP the_Det (mkCN (mkA "leer") menge_N)) ;
lin universeset_Name = mkName (mkNP the_Det (mkCN (mkA "universell") menge_N)) ;

lin congruent_Adj3 = mkAdj3 (mkAP (mkA "kongruent")) with_Prep (mkPrep "modulo" dative) ;




lin combinationsFromSet_Fun2 = mkFun2 (mkCN (mkN "An" zahl_N) (SyntaxGer.mkAdv (mkPrep genitive) (mkNP thePl_Det (mkN "Kombination")))) ;
lin combinations_Fun2 = mkFun2 (mkCN menge_N (SyntaxGer.mkAdv (mkPrep genitive) (mkNP aPl_Det (mkN "Kombination")))) ;
lin binomial_Fun2 = mkFun2 (mkCN (mkN "Binomialkoeffizient")) ;

lin area_Fun = mkFun "Fläche" ;
lin radius_Fun = mkFun "Radius" ;

lin pi_Name = mkName (mkNP the_Det (mkCN zahl_N (symb "\\(\\pi\\)"))) ;
lin legendre_symbol_Fun2 = mkFun2 (mkN "Legendresymbol") ;
lin square_Fun = mkFun (mkN "Quadrat" neuter) ;
lin resultant_FunC = mkFunC (mkN "Ergebnis") ;
lin perpendicular_Adj2 = mkAdj2 "senkrecht" "zu" ;
lin perpendicular_AdjC = mkAdjC "senkrecht" ;
lin length_Fun = mkFun (mkN "Länge") ;
lin norm_Fun = mkFun (mkN "Betrag" "Beträge" masculine) ;

lin denumerable_Adj = mkAdj "abzählbar" ;
lin cardinality_Fun = mkFun (mkN "Kardinalität") ;
lin root_Noun2 = mkNoun2 (mkN "Wurzel" feminine) ;
lin degree_Fun = mkFun (mkN "Grad") ;

lin irrational_Adj = mkAdj "irrational" ;


lin sin_Fun = mkFun "Sinus" ;
lin cos_Fun = mkFun "Cosinus" ;
lin tan_Fun = mkFun "Tangens" ;
lin arcsin_Fun = mkFun (mkN "Arcsinus") for_Prep ;
lin arccos_Fun = mkFun (mkN "Arccosinus") for_Prep ;
lin arctan_Fun = mkFun (mkN "Arctangens") for_Prep ;
lin orthogonal_Adj2 = mkAdj2 (mkA "orthogonal") to_Prep ;
lin orthogonal_AdjC = mkAdjC (mkA "orthogonal") ;
lin angle_between_Fun2 = mkFun2 (mkN "Winkel") (mkPrep "zwischen" dative) ;
lin dot_product_FunC = mkFunC "Skalarprodukt" ;
lin vector_plus_FunC = mkFunC "Summe" ;

lin everywhere_Adverb = mkAdv "überall" ;
lin almost_everywhere_Adverb = mkAdv "fast überall" ;
lin uniformly_Adverb = mkAdv "gleichäßig" ;


}
