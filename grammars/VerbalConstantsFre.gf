concrete VerbalConstantsFre of VerbalConstants = CategoriesFre **

open
UtilitiesFre,
SyntaxFre,
ParadigmsFre,
SymbolicFre,
Prelude

in {



lin proposition_Noun = mkNoun "proposition" ;

lin digit_Noun = mkNoun (mkN "chiffra" masculine) ;

lin boolean_Noun = mkNoun (mkCN (mkA "booléen") (mkN "valeur" feminine)) ;

lin natural_Noun = mkNoun (mkA "naturel") nombre_N ;

lin rational_Noun = mkNoun (mkA "rationnel") nombre_N ;
lin real_Noun = mkNoun (mkA "réel") nombre_N ;


lin list_Fam = mkFam "liste" ;
lin set_Fam = mkFam ensemble_N ;

lin Eq_Adj2 = mkAdj2 (mkA "égal") dative ;
lin Eq_AdjE = mkAdj (mkAP (mkA "égal")) ;
lin Lt_Adj2 = mkAdj2 (mkA "inférieur") dative ;
lin Gt_Adj2 = mkAdj2 (mkA "supérieur") dative ;
lin Neq_Adj2 = mkAdj2 (mkA "distinct") genitive ;
lin Neq_AdjC = mkAP (mkA "distinct") ;
lin Leq_Adj2 = mkAdj2 (mkAP or_Conj (mkAP (mkA "inférieur")) (mkAP (mkA "égal"))) dative ;
lin Geq_Adj2 = mkAdj2 (mkAP or_Conj (mkAP (mkA "supérieur")) (mkAP (mkA "égal"))) dative ;




lin converge_Verb = mkVerb "converger" ;
lin divide_Verb2 = mkVerb2 "diviser" ;

lin member_Noun2 = mkNoun2 element_N ;
lin divisor_Noun2 = mkNoun2 (mkN "diviseur") ;

lin plus_FunC = mkFunC "somme" ;
lin minus_Fun2 = mkFun2 (mkN "différence") (mkPrep "entre") ;
lin times_FunC = mkFunC (mkN "produit") ;
lin div_Fun2 = mkFun2 (mkN "quotient") (mkPrep "entre") ; ----
lin pow_Fun2 = mkFun2 (mkN "puissance") possess_Prep (mkPrep "en") ; ----
lin neg_Fun = mkFun (mkN "négation") ;
lin logarithm_Fun2 = mkFun2 (mkN "logarithme" masculine) (mkPrep "en base") possess_Prep ; ----
lin square_root_Fun = mkFun (mkCN (mkA "carré") (mkN "racine")) ;

lin successor_Fun = mkFun (mkN "successeur") ;
lin absolute_value_Fun = mkFun (mkCN (mkA "absolu") (mkN "valeur" feminine)) ;
lin factorial_Fun = mkFun (mkN "factorielle") ;
lin gcd_FunC = mkFunC "plus grand" "commun" "diviseur" ; ---- should be in this order



lin divisible_Adj2 = mkAdj2 "divisible" "par" ;


lin function_Fam2 = mkFam2 "fonction" genitive dative ;
lin union_FunC = mkFun "union" ;
lin intersection_FunC = mkFun "intersection" ;
lin complement_Fun = mkFun (mkN "complément") ;
lin cartesian_FunC = mkFun (mkCN (mkA "cartésien") (mkN "produit")) ;
lin difference_Fun2 = mkFun2 (mkN "différence") (mkPrep "entre") ;
lin powerset_Fun = mkFun "puissance" ; ----

lin subset_Noun2 = mkNoun2 (mkCN (mkA "propre") (mkN "sous-ensemble" masculine)) ;
lin subseteq_Noun2 = mkNoun2 (mkN "sous-ensemble" masculine) ;
lin superset_Noun2 = mkNoun2 (mkCN (mkA "propre") (mkN "sur-ensemble" masculine)) ;
lin superseteq_Noun2 = mkNoun2 (mkN "sur-ensemble" masculine) ;
lin equalset_Adj2 = mkAdj2 (mkA "égal") dative ;
lin notequalset_Adj2 = mkAdj2 (mkA "égal") dative ; ----
lin element_Noun2 = mkNoun2 (mkN "élément") ;
lin notelement_Noun2 = mkNoun2 (mkN "non-élément") ; ----

lin emptyset_Name = mkName (mkNP the_Det (mkCN (mkA "vide") ensemble_N)) ;
lin universeset_Name = mkName (mkNP the_Det (mkCN (mkA "universel") ensemble_N)) ;

lin congruent_Adj3 = mkAdj3 (mkAP (mkA "congruent")) dative (mkPrep "modulo") ;




lin combinationsFromSet_Fun2 = mkFun2 (mkCN nombre_N (SyntaxFre.mkAdv genitive (mkNP thePl_Det combinaison_N))) genitive on_Prep ; ----
lin combinations_Fun2 = mkFun2 (mkCN ensemble_N (SyntaxFre.mkAdv genitive (mkNP aPl_Det combinaison_N))) genitive on_Prep ; ----
lin binomial_Fun2 = mkFun2 (mkCN (mkA "binomial") (mkN "coefficient")) genitive on_Prep ; ----

lin area_Fun = mkFun "aire" ;
lin radius_Fun = mkFun "rayon" ;

lin pi_Name = mkName (mkNP the_Det (mkCN nombre_N (symb "\\(\\pi\\)"))) ;
lin legendre_symbol_Fun2 = mkFun2 (mkCN (mkN "symbole" masculine) (SyntaxFre.mkAdv genitive (mkNP (mkPN "Legendre")))) genitive on_Prep ; ----
lin square_Fun = mkFun (mkN "carré" masculine) ;
lin resultant_FunC = mkFunC (mkN "addition") ;
lin perpendicular_Adj2 = mkAdj2 (mkA "perpendiculaire") dative ;
lin perpendicular_AdjC = mkAdjC (mkA "perpendiculaire") ;
lin length_Fun = mkFun "norme" ;
lin norm_Fun = mkFun "norme" ;

lin denumerable_Adj = mkAdj "dénombrable" ;
lin cardinality_Fun = mkFun "cardinalité" ;
lin root_Noun2 = mkNoun2 (mkN "racine") genitive ;
lin degree_Fun = mkFun (mkN "degré" masculine) ;

lin irrational_Adj = mkAdj "irrationnel" ;


lin sin_Fun = mkFun "sinus" ;
lin cos_Fun = mkFun "cosinus" ;
lin tan_Fun = mkFun "tangente" ;
lin arcsin_Fun = mkFun "arcsinus" ;
lin arccos_Fun = mkFun "arccosinus" ;
lin arctan_Fun = mkFun "arctangente" ;
lin orthogonal_Adj2 = mkAdj2 (mkA "orthogonal") dative ;
lin orthogonal_AdjC = mkAdjC (mkA "orthogonal") ;
lin angle_between_Fun2 = mkFun2 (mkN "angle" masculine) (mkPrep "entre") ;
lin dot_product_FunC = mkFunC (mkCN (mkA "scalaire") (mkN "produit")) ;
lin vector_plus_FunC = mkFunC "somme" ;



}
