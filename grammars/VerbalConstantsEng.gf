concrete VerbalConstantsEng of VerbalConstants = CategoriesEng **

open
UtilitiesEng,
SyntaxEng,
ParadigmsEng,
SymbolicEng,
Prelude

in {



lin proposition_Noun = mkNoun "proposition" ;

lin digit_Noun = mkNoun "digit" ;

lin boolean_Noun = mkNoun "boolean" ;

lin natural_Noun = mkNoun "natural" "number" ;

lin rational_Noun = mkNoun "rational" "number" ;
lin real_Noun = mkNoun "real" "number" ;


lin list_Fam = mkFam "list" ;
lin set_Fam = mkFam "set" ;

lin Eq_Adj2 = mkAdj2 "equal" "to" ;
lin Eq_AdjE = mkAdjE (mkA "equal") ;
lin Lt_Adj2 = mkAdj2 "less" "than" ;
lin Gt_Adj2 = mkAdj2 "greater" "than" ;
lin Neq_Adj2 = mkAdj2 "distinct" "from" ;
lin Neq_AdjC = mkAdjC (mkA "distinct") ;
lin Leq_Adj2 = mkAdj2 "less than or equal" "to" ;
lin Geq_Adj2 = mkAdj2 "greater than or equal" "to" ;




lin converge_Verb = mkVerb "converge" ;
lin divide_Verb2 = mkVerb2 "divide" ;

lin member_Noun2 = mkNoun2 "member" ;
lin divisor_Noun2 = mkNoun2 "divisor" ;

lin plus_FunC = mkFun "sum" ;
lin minus_Fun2 = mkFun2 (mkN "difference") (mkPrep "between") ;
lin times_FunC = mkFun "product" ;
lin div_Fun2 = mkFun2 (mkN "quotient") possess_Prep ;
lin pow_Fun2 = mkFun2 "exponentiation" "to";
lin neg_Fun = mkFun "negation" ;
lin logarithm_Fun2 = mkFun2 "logarithm" "in base" ;
lin square_root_Fun = mkFun "square root" ;

lin successor_Fun = mkFun "successor" ;
lin absolute_value_Fun = mkFun "absolute value" ;
lin factorial_Fun = mkFun "factorial" ;
lin gcd_FunC = mkFun "greatest" "common" "divisor" ;



lin divisible_Adj2 = mkAdj2 "divisible" "by" ;


lin function_Fam2 = mkFam2 "function" from_Prep to_Prep ;
lin union_FunC = mkFun "union" ;
lin intersection_FunC = mkFun "intersection" ;
lin difference_Fun2 = mkFun2 (mkN "difference") possess_Prep ;
lin complement_Fun = mkFun "complement" ;
lin cartesian_FunC = mkFun "cartesian product" ;
lin powerset_Fun = mkFun "power set" ;

lin subset_Noun2 = mkNoun2 "proper subset" ;
lin subseteq_Noun2 = mkNoun2 "subset" ;
lin superset_Noun2 = mkNoun2 "proper superset" ;
lin superseteq_Noun2 = mkNoun2 "superset" ;
lin equalset_Adj2 = mkAdj2 "equal" "to" ;
lin notequalset_Adj2 = mkAdj2 "distinct" "from" ; ----
lin element_Noun2 = mkNoun2 "element" ;
lin notelement_Noun2 = mkNoun2 "non-element" ; ----

lin emptyset_Name = mkNP the_Det (mkCN (mkA "empty") (mkN "set")) ;
lin universeset_Name = mkNP the_Det (mkCN (mkA "universal") (mkN "set")) ;

lin congruent_Adj3 = mkAdj3 (mkAP (mkA "congruent")) to_Prep (mkPrep "modulo") ;




lin combinationsFromSet_Fun2 = mkFun2 "number of combinations" "of size" ;
lin combinations_Fun2 = mkFun2 "set of combinations" "of size" ;
lin binomial_Fun2 = mkFun2 "binomial coefficient" "over" ;
lin area_Fun = mkFun "area" ;
lin radius_Fun = mkFun "radius" ;

lin pi_Name = mkNP the_Det (mkCN (mkN "number") (symb "\\(\\pi\\)")) ;
lin legendre_symbol_Fun2 = mkFun2 "Legendre symbol" "over" ;
lin square_Fun = mkFun "square" ;
lin resultant_FunC = mkFun "resultant" ;
lin perpendicular_Adj2 = mkAdj2 "perpendicular" "to" ;
lin length_Fun = mkFun "length" ;
lin norm_Fun = mkFun "norm" ;

lin denumerable_Adj = mkAdj "denumerable" ;
lin cardinality_Fun = mkFun "cardinality" ;
lin root_Noun2 = mkNoun2 "root" ;
lin degree_Fun = mkFun "degree" ;

lin irrational_Adj = mkAdj "irrational" ;


lin sin_Fun = mkFun "sine" ;
lin cos_Fun = mkFun "cosine" ;
lin tan_Fun = mkFun "tangent" ;
lin arcsin_Fun = mkFun "arcsine" ;
lin arccos_Fun = mkFun "arccosine" ;
lin arctan_Fun = mkFun "arctangent" ;
lin orthogonal_Adj2 = mkAdj2 "orthogonal" "to" ;
lin orthogonal_AdjC = mkAP (mkA "orthogonal") ;
lin perpendicular_Adj2 = mkAdj2 "perpendicular" "to" ;
lin perpendicular_AdjC = mkAdjC (mkA "perpendicular") ;
lin angle_between_Fun2 = mkFun2 (mkN "angle") (mkPrep "between") ;
lin dot_product_FunC = mkFunC "dot product" ;
lin vector_plus_FunC = mkFunC "sum" ;



-- special constants

lin SigmaExp m n i exp =
mkNP the_Det (mkCN (mkCN sum_N)
(SyntaxEng.mkAdv possess_Prep
(mkNP all_Predet
(mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
(SyntaxEng.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))))) ;

lin SeriesExp m i exp =
mkNP the_Det (mkCN (mkCN series_N)
(SyntaxEng.mkAdv possess_Prep
(mkNP all_Predet
(mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
(SyntaxEng.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m infinity_NP)))))))) ;

lin IntegralExp m n i exp =
mkNP the_Det (mkCN (mkCN integral_N)
(SyntaxEng.mkAdv possess_Prep
(mkNP exp (SyntaxEng.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))) ;

lin sameParityProp x y =
simpleProp (mkS (mkCl (mkNP and_Conj x y)
(SyntaxEng.mkAdv possess_Prep (mkNP the_Det (mkCN (mkA "same") (mkN "parity")))))) ;

}
