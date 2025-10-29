concrete VerbalConstantsEng of VerbalConstants = CategoriesEng **

open
  UtilitiesEng,
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  Prelude

in {

lin
  type_Noun = mkNoun "type" ;
  set_Noun = mkNoun "set" ;
  proposition_Noun = mkNoun "proposition" ;

  digit_Noun = mkNoun "digit" ;
  number_Noun = mkNoun "number" ;
  boolean_Noun = mkNoun "boolean" ;
  cardinal_Noun = mkNoun "cardinal" ;
  natural_Noun = mkNoun "natural" "number" ;
  integer_Noun = mkNoun "integer" ;
  rational_Noun = mkNoun "rational" "number" ;
  real_Noun = mkNoun "real" "number" ;
  complex_Noun = mkNoun "complex" "number" ;

  list_Fam = mkFam "list" ;
  set_Fam = mkFam "set" ;


  Eq_Adj2 = mkReladj "equal" "to" ;
  Eq_AdjE = mkAP (mkA "equal") ;
  Lt_Adj2 = mkReladj "less" "than" ;
  Gt_Adj2 = mkReladj "greater" "than" ;
  Neq_Adj2 = mkReladj "distinct" "from" ;
  Neq_AdjC = mkAP (mkA "distinct") ;
  Leq_Adj2 = mkReladj "less than or equal" "to" ;
  Geq_Adj2 = mkReladj "greater than or equal" "to" ;

  positive_Adj = mkAdj "positive" ;
  negative_Adj = mkAdj "negative" ;

  converge_Verb = mkVP (mkV "converge") ;
  divide_Verb2 = mkV2 "divide" ;
  
  member_Noun2 = mkRelnoun "member" ;
  divisor_Noun2 = mkRelnoun "divisor" ;

  plus_FunC = mkFun "sum" ;
  minus_Fun2 = mkFun2 "subtraction" "from" ;
  times_FunC = mkFun "product" ;
  div_Fun2 = mkFun2 "division" "by" ;
  pow_Fun2 = mkFun2 "exponentiation" "to";
  neg_Fun = mkFun "negation" ;
  logarithm_Fun2 = mkFun2 "logarithm" "in base" ;
  square_root_Fun = mkFun "square root" ;

  successor_Fun = mkFun "successor" ;
  absolute_value_Fun = mkFun "absolute value" ;
  factorial_Fun = mkFun "factorial" ;
  gcd_FunC = mkFun "greatest" "common" "divisor" ;

  even_Adj = mkAdj "even" ;
  odd_Adj = mkAdj "odd" ;
  divisible_Adj2 = mkReladj "divisible" "by" ;
  prime_Adj = mkAdj "prime" ;

  function_Fam2 = mkFam "function" from_Prep to_Prep ;
  union_FunC = mkFun "union" ;
  intersection_FunC = mkFun "intersection" ;
  difference_Fun2 = mkFun2 "difference" "from" ;
  complement_Fun = mkFun "complement" ;
  cartesian_FunC = mkFun "cartesian product" ;
  powerset_Fun = mkFun "power set" ;

  subset_Noun2 = mkRelnoun "proper subset" ;  
  subseteq_Noun2 = mkRelnoun "subset" ;  
  superset_Noun2 = mkRelnoun "proper superset" ;  
  superseteq_Noun2 = mkRelnoun "superset" ;  
  equalset_Adj2 = mkReladj "equal" "to" ;
  notequalset_Adj2 = mkReladj "distinct" "from" ; ----
  element_Noun2 = mkRelnoun "element" ;
  notelement_Noun2 = mkRelnoun "non-element" ; ----

  emptyset_Name = mkNP the_Det (mkCN (mkA "empty") (mkN "set")) ;
  universeset_Name = mkNP the_Det (mkCN (mkA "universal") (mkN "set")) ;

  congruent_Pred3 = mkPred3 (mkAP (mkA "congruent")) to_Prep (mkPrep "modulo") ;

  finite_Adj = mkAdj "finite" ;
  infinite_Adj = mkAdj "infinite" ;

  combinationsFromSet_Fun2 = mkFun2 "number of combinations" "of size" ;
  combinations_Fun2 = mkFun2 "set of combinations" "of size" ;
  binomial_Fun2 = mkFun2 "binomial coefficient" "over" ;
  area_Fun = mkFun "area" ;
  radius_Fun = mkFun "radius" ;
  circle_Noun = mkNoun "circle" ;
  pi_Name = mkNP the_Det (mkCN (mkN "number") (symb "\\(\\pi\\)")) ;
  legendre_symbol_Fun2 = mkFun2 "Legendre symbol" "over" ;
  square_Fun = mkFun "square" ;
  resultant_FunC = mkFun "resultant" ;
  perpendicular_Adj2 = mkReladj "perpendicular" "to" ;
  length_Fun = mkFun "length" ;
  norm_Fun = mkFun "norm" ;
  vector_Noun = mkNoun "vector" ;
  denumerable_Adj = mkAdj "denumerable" ;
  cardinality_Fun = mkFun "cardinality" ;
  is_root_Noun2 = mkRelnoun "root" ;
  degree_Fun = mkFun "degree" ;
  polynomial_Noun = mkNoun "polynomial" ;
  irrational_Adj = mkAdj "irrational" ;
  rational_Adj = mkAdj "rational" ;

  sin_Fun = mkFun "sine" ;
  cos_Fun = mkFun "cosine" ;
  tan_Fun = mkFun "tangent" ;
  arcsin_Fun = mkFun "arcsine" ;
  arccos_Fun = mkFun "arccosine" ;
  arctan_Fun = mkFun "arctangent" ;
  orthogonal_Adj2 = mkReladj "orthogonal" "to" ;
  orthogonal_AdjC = mkAP (mkA "orthogonal") ;
  perpendicular_Adj2 = mkReladj "perpendicular" "to" ;
  perpendicular_AdjC = mkAP (mkA "perpendicular") ;
  angle_between_Fun2 = mkFun2 "angle" "with" ;
  dot_product_FunC = mkFun "dot product" ;
  vector_plus_FunC = mkFun "sum" ;

  sphenic_Adj = mkAdj "sphenic" ;

-- special constants

  SigmaExp i m n exp =
    mkNP the_Det (mkCN (mkCN sum_N)
      (SyntaxEng.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxEng.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))))) ;
	    
  SeriesExp i m exp =
    mkNP the_Det (mkCN (mkCN series_N)
      (SyntaxEng.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxEng.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m infinity_NP)))))))) ;
	    
  IntegralExp i m n exp =
    mkNP the_Det (mkCN (mkCN integral_N)
      (SyntaxEng.mkAdv possess_Prep
        (mkNP exp (SyntaxEng.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))) ;
}