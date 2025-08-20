concrete VerbalConstantsEng of VerbalConstants =

open
  UtilitiesEng,
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  Prelude

in {

lincat
  Noun = CN ;
  Fam = FamilyT ; 
  Adj = AP ;
  Verb = VP ;
  Reladj = RelationT ;
  Relverb = V2 ;
  Relnoun = RelnounT ;
  Name = NP ;
  Fun = FunctionT ;
  Label = LabelT ;
  Pred3 = Pred3T ;

lin
  type_Noun = mkNoun "type" ;
  set_Noun = mkNoun "set" ;
  proposition_Noun = mkNoun "proposition" ;

  elements_Fun = mkFun "type of elements" ;
  proofs_Fun = mkFun  "type of proofs" ;

  absurdity_Name = mkName "absurdity" ;
  conjunction_Fun = mkFun "conjunction" ;
  disjunction_Fun = mkFun "disjunction" ;
  implication_Fun = mkFun "implication" ;
  universal_Fun = mkFun "universal" "quanfication" ;
  existential_Fun = mkFun "existential" "quanfication" ;
  negation_Fun = mkFun "negation" ;
  equivalence_Fun = mkFun "equivalence" ;

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

  Eq_Reladj = mkReladj "equal" "to" ;
  Lt_Reladj = mkReladj "less" "than" ;
  Gt_Reladj = mkReladj "greater" "than" ;
  Neq_Reladj = mkReladj "not equal" "to" ;
  Leq_Reladj = mkReladj "less than or equal" "to" ;
  Geq_Reladj = mkReladj "greater than or equal" "to" ;

  positive_Adj = mkAdj "positive" ;
  negative_Adj = mkAdj "negative" ;

  converge_Verb = mkVP (mkV "converge") ;
  divide_Relverb = mkV2 "divide" ;
  
  member_Relnoun = mkRelnoun "member" ;
  divisor_Relnoun = mkRelnoun "divisor" ;

  plus_Fun = mkFun "sum" ;
  minus_Fun = mkFun "difference" ;
  times_Fun = mkFun "product" ;
  div_Fun = mkFun "quotient" ;
  pow_Fun = mkFun "exponentiation" ;
  neg_Fun = mkFun "negation" ;
  logarithm_Fun = mkFun "logarithm" ; ----
  square_root_Fun = mkFun "square root" ;

  successor_Fun = mkFun "successor" ;
  absolute_value_Fun = mkFun "absolute value" ;
  factorial_Fun = mkFun "factorial" ;
  gcd_Fun = mkFun "greatest" "common" "divisor" ;

  even_Adj = mkAdj "even" ;
  odd_Adj = mkAdj "odd" ;
  divisible_Reladj = mkReladj "divisible" "by" ;
  prime_Adj = mkAdj "prime" ;

  function_Fam = mkFam "function" from_Prep to_Prep ;
  union_Fun = mkFun "union" ;
  intersection_Fun = mkFun "intersection" ;
  difference_Fun = mkFun "difference" ;
  complement_Fun = mkFun "complement" ;
  cartesian_Fun = mkFun "cartesian product" ;
  powerset_Fun = mkFun "power set" ;

  subset_Relnoun = mkRelnoun "proper subset" ;  
  subseteq_Relnoun = mkRelnoun "subset" ;  
  superset_Relnoun = mkRelnoun "proper superset" ;  
  superseteq_Relnoun = mkRelnoun "superset" ;  
  equalset_Reladj = mkReladj "equal" "to" ;
  notequalset_Reladj = mkReladj "distinct" "from" ; ----
  element_Relnoun = mkRelnoun "element" ;
  notelement_Relnoun = mkRelnoun "non-element" ; ----

  emptyset_Name = mkNP the_Det (mkCN (mkA "empty") (mkN "set")) ;
  universeset_Name = mkNP the_Det (mkCN (mkA "universal") (mkN "set")) ;

  congruent_Pred3 = mkPred3 (mkAP (mkA "congruent")) to_Prep (mkPrep "modulo") ;

  finite_Adj = mkAdj "finite" ;
  infinite_Adj = mkAdj "infinite" ;

  combinationsFromSet_Fun = mkFun "number of combinations" ;
  combinations_Fun = mkFun "set of combinations" ;
  binomial_Fun = mkFun "binomial coefficient" ;
  area_Fun = mkFun "area" ;
  radius_Fun = mkFun "radius" ;
  circle_Noun = mkNoun "circle" ;
  pi_Name = mkNP the_Det (mkCN (mkN "number") (symb "\\(\\pi\\)")) ;
  legendre_symbol_Fun = mkFun "Legendre symbol" ;
  square_Fun = mkFun "square" ;
  resultant_Fun = mkFun "resultant" ;
  perpendicular_Reladj = mkReladj "perpendicular" "to" ;
  length_Fun = mkFun "length" ;
  norm_Fun = mkFun "norm" ;
  vector_Noun = mkNoun "vector" ;
  denumerable_Adj = mkAdj "denumerable" ;
  cardinality_Fun = mkFun "cardinality" ;
  is_root_Relnoun = mkRelnoun "root" ;
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
  orthogonal_Reladj = mkReladj "orthogonal" "to" ;
  angle_between_Fun = mkFun "angle" "between" ;
  dot_product_Fun = mkFun "dot product" ;
  vector_plus_Fun = mkFun "sum" ;

}