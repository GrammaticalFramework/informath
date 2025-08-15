abstract BaseConstants = {

cat
  Noun ; -- Kind -- set
  Fam ; -- Kind -> Kind -- list of integers
  Set ;  -- Kind + symbol -- integer, Z
  Adj ;  -- Exp -> Prop -- even
  Verb ; -- Exp -> Exp -- converge
  Reladj ;  -- Exp -> Exp -> Prop -- divisible by
  Relverb ; -- Exp -> Exp -> Prop -- divide
  Relnoun ; -- Exp -> Exp -> Prop  -- divisor of
  Name ; -- Exp -- absurdity
  Fun ;  -- [Exp] -> Exp -- equivalence of
  Label ; -- Exp -- theorem 1
  Const ; -- Exp + symbol -- the empty set, Ø
  Oper ;  -- Exp -> Exp -> Exp + symbol -- the sum, +
  Compar ; -- Exp -> Exp -> Prop + symbol -- greater than, >
  Comparnoun ; -- Exp -> Exp -> Prop + symbol -- subset of, >
  Pred3 ; -- Exp -> Exp -> Exp -> Prop -- congruent to y modulo z

fun
  type_Noun : Noun ;
  set_Noun : Noun ;
  proposition_Noun : Noun ;

  elements_Fun : Fun ;
  proofs_Fun : Fun ;

  absurdity_Name : Name ;
  conjunction_Fun : Fun ;
  disjunction_Fun : Fun ;
  implication_Fun : Fun ;
  universal_Fun : Fun ;
  existential_Fun : Fun ;
  negation_Fun : Fun ;
  equivalence_Fun : Fun ;

  number_Noun : Noun ;
  boolean_Noun : Noun ;
  list_Fam : Fam ;
  
  cardinal_Noun : Noun ;
  digit_Noun : Noun ;
  natural_Set : Set ;
  integer_Set : Set ;
  rational_Set : Set ;
  real_Set : Set ;
  complex_Set : Set ;

  Eq_Compar : Compar ;
  Lt_Compar : Compar ;
  Gt_Compar : Compar ;
  Neq_Compar : Compar ;
  Leq_Compar : Compar ;
  Geq_Compar : Compar ;

  positive_Adj : Adj ;
  negative_Adj : Adj ;

  converge_Verb : Verb ;
  divide_Relverb : Relverb ;
  member_Relnoun : Relnoun ;
  divisor_Relnoun : Relnoun ;

  plus_Oper : Oper ;
  minus_Oper : Oper ;
  times_Oper : Oper ;
  div_Oper : Oper ;
  pow_Oper : Oper ;
  neg_Oper : Oper ;
  logarithm_Oper : Oper ;
  square_root_Oper : Oper ;
  
  successor_Fun : Fun ;
  absolute_value_Oper : Oper ;
  factorial_Oper : Oper ;
  gcd_Fun : Fun ;

  even_Adj : Adj ;
  odd_Adj : Adj ;
  divisible_Reladj : Reladj ;
  prime_Adj : Adj ;

  function_Fam : Fam ;
  union_Oper : Oper ;
  intersection_Oper : Oper ;
  cartesian_Oper : Oper ;
  difference_Oper : Oper ;
  complement_Oper : Oper ;
  powerset_Oper : Oper ;

  subset_Comparnoun : Comparnoun ;  
  subseteq_Comparnoun : Comparnoun ;  
  superset_Comparnoun : Comparnoun ;
  superseteq_Comparnoun : Comparnoun ;
  equalset_Compar : Compar ;
  notequalset_Compar : Compar ;
  element_Comparnoun : Comparnoun ;
  notelement_Comparnoun : Comparnoun ;

  emptyset_Const : Const ;
  universeset_Const : Const ;

  congruent_Pred3 : Pred3 ;

  finite_Adj : Adj ;
  infinite_Adj : Adj ;

  rational_Adj : Adj ; -- top100
  irrational_Adj : Adj ; -- top100
  polynomial_Noun : Noun ; -- top100
  degree_Fun : Fun ; -- top100
  is_root_Relnoun : Relnoun ; -- top100
  cardinality_Oper : Oper ; -- top100
  denumerable_Adj : Adj ; -- top100
  vector_Noun : Noun ; -- top100
  length_Oper : Oper ; -- top100
  norm_Oper : Oper ; -- top100
  perpendicular_Compar : Compar ; -- top100
  resultant_Oper : Oper ; -- top100
  square_Oper : Oper ; -- top100
  legendre_symbol_Oper : Oper ; -- top100
  pi_Const : Const ; -- top100
  circle_Noun : Noun ; -- top100
  radius_Fun  : Fun ; -- top100
  area_Fun : Fun ; -- top100
  binomial_Oper : Oper ; -- top100
  combinations_Oper : Oper ; -- top100
  combinationsFromSet_Oper : Oper ; -- top100
  sin_Oper : Oper ;
  cos_Oper : Oper ;
  tan_Oper : Oper ;
  arcsin_Oper : Oper ;
  arccos_Oper : Oper ;
  arctan_Oper : Oper ;
  orthogonal_Compar : Compar ;
  angle_between_Fun : Fun ;
  dot_product_Oper : Oper ;
  vector_plus_Oper : Oper ;

}