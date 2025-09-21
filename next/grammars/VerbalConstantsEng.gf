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

  elements_Fun = mkFun "type of elements" ;
  proofs_Fun = mkFun  "type of proofs" ;

  absurdity_Name = mkName "absurdity" ;
  conjunction_Fun2 = mkFun "conjunction" ;
  disjunction_Fun2 = mkFun "disjunction" ;
  implication_Fun2 = mkFun "implication" ;
  universal_Fun2 = mkFun "universal" "quanfication" ;
  existential_Fun2 = mkFun "existential" "quanfication" ;
  negation_Fun = mkFun "negation" ;
  equivalence_Fun2 = mkFun "equivalence" ;

  digit_Noun = mkNoun "digit" ;
  number_Noun = mkNoun "number" ;
  boolean_Noun = mkNoun "boolean" ;
  cardinal_Noun = mkNoun "cardinal" ;
  list_Fam = mkFam "list" ;

  natural_Noun = mkNoun "natural" "number" ;
  integer_Noun = mkNoun "integer" ;
  rational_Noun = mkNoun "rational" "number" ;
  real_Noun = mkNoun "real" "number" ;
  complex_Noun = mkNoun "complex" "number" ;

  Eq_Adj2 = mkReladj "equal" "to" ;
  Lt_Adj2 = mkReladj "less" "than" ;
  Gt_Adj2 = mkReladj "greater" "than" ;
  Neq_Adj2 = mkReladj "not equal" "to" ;
  Leq_Adj2 = mkReladj "less than or equal" "to" ;
  Geq_Adj2 = mkReladj "greater than or equal" "to" ;

  positive_Adj = mkAdj "positive" ;
  negative_Adj = mkAdj "negative" ;

  converge_Verb = mkVP (mkV "converge") ;
  divide_Verb2 = mkV2 "divide" ;
  
  member_Noun2 = mkRelnoun "member" ;
  divisor_Noun2 = mkRelnoun "divisor" ;

  plus_Fun2 = mkFun "sum" ;
  minus_Fun2 = mkFun "difference" ;
  times_Fun2 = mkFun "product" ;
  div_Fun2 = mkFun "quotient" ;
  pow_Fun2 = mkFun "exponentiation" ;
  neg_Fun = mkFun "negation" ;
  logarithm_Fun2 = mkFun "logarithm" ; ----
  square_root_Fun = mkFun "square root" ;

  successor_Fun = mkFun "successor" ;
  absolute_value_Fun = mkFun "absolute value" ;
  factorial_Fun = mkFun "factorial" ;
  gcd_Fun2 = mkFun "greatest" "common" "divisor" ;

  even_Adj = mkAdj "even" ;
  odd_Adj = mkAdj "odd" ;
  divisible_Adj2 = mkReladj "divisible" "by" ;
  prime_Adj = mkAdj "prime" ;

  function_Fam2 = mkFam "function" from_Prep to_Prep ;
  union_Fun2 = mkFun "union" ;
  intersection_Fun2 = mkFun "intersection" ;
  difference_Fun2 = mkFun "difference" ;
  complement_Fun = mkFun "complement" ;
  cartesian_Fun2 = mkFun "cartesian product" ;
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

  combinationsFromSet_Fun2 = mkFun "number of combinations" ;
  combinations_Fun2 = mkFun "set of combinations" ;
  binomial_Fun2 = mkFun "binomial coefficient" ;
  area_Fun = mkFun "area" ;
  radius_Fun = mkFun "radius" ;
  circle_Noun = mkNoun "circle" ;
  pi_Name = mkNP the_Det (mkCN (mkN "number") (symb "\\(\\pi\\)")) ;
  legendre_symbol_Fun2 = mkFun "Legendre symbol" ;
  square_Fun = mkFun "square" ;
  resultant_Fun2 = mkFun "resultant" ;
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
  angle_between_Fun2 = mkFun "angle" "between" ;
  dot_product_Fun2 = mkFun "dot product" ;
  vector_plus_Fun2 = mkFun "sum" ;

  sphenic_Adj = mkAdj "sphenic" ;

}