concrete BaseConstantsEng of BaseConstants =

open
  UtilitiesEng,
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {

lincat
  Noun = CN ;
  Fam = FamilyT ; 
  Set = SetT ;
  Adj = AP ;
  Reladj = RelationT ;
  Verb = VP ;
  Relverb = V2 ;
  Relnoun = N2 ;
  Name = NP ;
  Fun = FunctionT ;
  Label = LabelT ;
  Const = ConstantT ;
  Oper = OperatorT ;
  Compar = ComparisonT ;
  Comparnoun = ComparnounT ;
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

  natural_Set = mkSet L.natural_Set "natural" "number" ;
  integer_Set = mkSet L.integer_Set "integer" ;
  rational_Set = mkSet L.rational_Set "rational" "number" ;
  real_Set = mkSet L.real_Set "real" "number" ;
  complex_Set = mkSet  L.complex_Set "complex" "number" ;

  Eq_Compar = mkCompar L.Eq_Compar "equal" "to" ;
  Lt_Compar = mkCompar L.Lt_Compar "less" "than" ;
  Gt_Compar = mkCompar L.Gt_Compar "greater" "than" ;
  Neq_Compar = mkCompar L.Neq_Compar "not equal" "to" ;
  Leq_Compar = mkCompar L.Leq_Compar "less than or equal" "to" ;
  Geq_Compar = mkCompar L.Geq_Compar "greater than or equal" "to" ;

  positive_Adj = mkAdj "positive" ;
  negative_Adj = mkAdj "negative" ;

  converge_Verb = mkVP (mkV "converge") ;
  divide_Relverb = mkV2 "divide" ;
  member_Relnoun = mkN2 (mkN "member") ;
  divisor_Relnoun = mkN2 (mkN "divisor") ;

  plus_Oper = mkOper L.plus_Oper "sum" ;
  minus_Oper = mkOper L.minus_Oper "difference" ;
  times_Oper = mkOper L.times_Oper "product" ;
  div_Oper = mkOper L.div_Oper "quotient" ;
  pow_Oper = mkOper L.pow_Oper "exponentiation" ;
  neg_Oper = mkOper L.neg_Oper "negation" ;
  logarithm_Oper = mkOper L.logarithm_Oper "logarithm" ; ----
  square_root_Oper = mkOper L.square_root_Oper "square root" ;

  successor_Fun = mkFun "successor" ;
  absolute_value_Oper = mkOper L.absolute_value_Oper "absolute value" ;
  factorial_Oper = mkOper L.factorial_Oper "factorial" ;
  gcd_Fun = mkFun "greatest" "common" "divisor" ;

  even_Adj = mkAdj "even" ;
  odd_Adj = mkAdj "odd" ;
  divisible_Reladj = mkRel "divisible" "by" ;
  prime_Adj = mkAdj "prime" ;

  function_Fam = mkFam "function" from_Prep to_Prep ;
  union_Oper = mkOper L.union_Oper "union" ;
  intersection_Oper = mkOper L.intersection_Oper "intersection" ;
  difference_Oper = mkOper L.difference_Oper "difference" ;
  complement_Oper = mkOper L.complement_Oper "complement" ;
  cartesian_Oper = mkOper L.cartesian_Oper "cartesian product" ;
  powerset_Oper = mkOper L.powerset_Oper "power set" ;

  subset_Comparnoun = mkComparnoun L.subset_Comparnoun "proper subset" ;  
  subseteq_Comparnoun = mkComparnoun L.subseteq_Comparnoun "subset" ;  
  superset_Comparnoun = mkComparnoun L.superset_Comparnoun "proper superset" ;  
  superseteq_Comparnoun = mkComparnoun L.superseteq_Comparnoun "superset" ;  
  equalset_Compar = mkCompar L.equalset_Compar "equal" "to" ;
  notequalset_Compar = mkCompar L.notequalset_Compar "not equal" "to" ; ----
  element_Comparnoun = mkComparnoun L.element_Comparnoun "element" ;
  notelement_Comparnoun = mkComparnoun L.notelement_Comparnoun "non-element" ; ----

  emptyset_Const = mkConst L.emptyset_Const (mkNP the_Det (mkCN (mkA "empty") (mkN "set"))) ;
  universeset_Const = mkConst L.universeset_Const (mkNP the_Det (mkCN (mkA "universal") (mkN "set"))) ;

  congruent_Pred3 = mkPred3 (mkAP (mkA "congruent")) to_Prep (mkPrep "modulo") ;

  finite_Adj = mkAdj "finite" ;
  infinite_Adj = mkAdj "infinite" ;

  combinationsFromSet_Oper = mkOper L.binomial_Oper "number of combinations" ;
  combinations_Oper = mkOper L.combinations_Oper "set of combinations" ;
  binomial_Oper = mkOper L.binomial_Oper "binomial coefficient" ;
  area_Fun = mkFun "area" ;
  radius_Fun = mkFun "radius" ;
  circle_Noun = mkNoun "circle" ;
  pi_Const = mkConst "\\pi" (mkNP the_Det (mkCN (mkN "number") (symb "\\(\\pi\\)"))) ;
  legendre_symbol_Oper = mkOper L.legendre_symbol_Oper "Legendre symbol" ;
  square_Oper = mkOper L.square_Oper "square" ;
  resultant_Oper = mkOper L.plus_Oper "resultant" ;
  perpendicular_Compar = mkCompar L.perpendicular_Compar "perpendicular" "to" ;
  length_Oper = mkOper L.length_Oper "length" ;
  norm_Oper = mkOper L.length_Oper "norm" ;
  vector_Noun = mkNoun "vector" ;
  denumerable_Adj = mkAdj "denumerable" ;
  cardinality_Oper = mkOper L.absolute_value_Oper "cardinality" ;
  is_root_Relnoun = mkN2 (mkN "root") ;
  degree_Fun = mkFun "degree" ;
  polynomial_Noun = mkNoun "polynomial" ;
  irrational_Adj = mkAdj "irrational" ;
  rational_Adj = mkAdj "rational" ;

  sin_Oper = mkOper L.sin_Oper "sine" ;
  cos_Oper = mkOper L.cos_Oper "cosine" ;
  tan_Oper = mkOper L.tan_Oper "tangent" ;
  arcsin_Oper = mkOper L.arcsin_Oper "arcsine" ;
  arccos_Oper = mkOper L.arccos_Oper "arccosine" ;
  arctan_Oper = mkOper L.arctan_Oper "arctangent" ;
  orthogonal_Compar = mkCompar L.perpendicular_Compar "orthogonal" "to" ;
  angle_between_Fun = mkFun "angle" "between" ;
  dot_product_Oper = mkOper L.dot_product_Oper "dot product" ;
  vector_plus_Oper = mkOper L.plus_Oper "sum" ;

  sphenic_Adj = mkAdj "sphenic" ;

}