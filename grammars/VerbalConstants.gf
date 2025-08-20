abstract VerbalConstants = {

cat
-- verbal 
  Noun ;    -- Kind -- set
  Fam ;     -- Kind -> Kind        -- list of integers
  Adj ;     -- Exp -> Prop         -- even
  Verb ;    -- Exp -> Exp          -- converge
  Reladj ;  -- Exp -> Exp -> Prop  -- divisible by
  Relverb ; -- Exp -> Exp -> Prop  -- divide
  Relnoun ; -- Exp -> Exp -> Prop  -- divisor of
  Name ;    -- Exp                 -- the empty set
  Fun ;     -- [Exp] -> Exp        -- the sum of
  Label ;   -- Exp                 -- theorem 1
  Pred3 ;   -- Exp -> Exp -> Exp -> Prop -- congruent to y modulo z

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
  natural_Noun : Noun ;
  integer_Noun : Noun ;
  rational_Noun : Noun ;
  real_Noun : Noun ;
  complex_Noun : Noun ;

  Eq_Reladj : Reladj ;
  Lt_Reladj : Reladj ;
  Gt_Reladj : Reladj ;
  Neq_Reladj : Reladj ;
  Leq_Reladj : Reladj ;
  Geq_Reladj : Reladj ;
  
  positive_Adj : Adj ;
  negative_Adj : Adj ;

  converge_Verb : Verb ;
  divide_Relverb : Relverb ;
  member_Relnoun : Relnoun ;
  divisor_Relnoun : Relnoun ;

  plus_Fun : Fun ;
  minus_Fun : Fun ;
  times_Fun : Fun ;
  div_Fun : Fun ;
  pow_Fun : Fun ;
  neg_Fun : Fun ;
  logarithm_Fun : Fun ;
  square_root_Fun : Fun ;
  
  successor_Fun : Fun ;
  absolute_value_Fun : Fun ;
  factorial_Fun : Fun ;
  gcd_Fun : Fun ;

  even_Adj : Adj ;
  odd_Adj : Adj ;
  divisible_Reladj : Reladj ;
  prime_Adj : Adj ;

  function_Fam : Fam ;
  
  union_Fun : Fun ;
  intersection_Fun : Fun ;
  cartesian_Fun : Fun ;
  difference_Fun : Fun ;
  complement_Fun : Fun ;
  powerset_Fun : Fun ;

  subset_Relnoun : Relnoun ;  
  subseteq_Relnoun : Relnoun ;  
  superset_Relnoun : Relnoun ;
  superseteq_Relnoun : Relnoun ;
  equalset_Reladj : Reladj ;
  notequalset_Reladj : Reladj ;
  element_Relnoun : Relnoun ;
  notelement_Relnoun : Relnoun ;

  emptyset_Name : Name ;
  universeset_Name : Name ;

  congruent_Pred3 : Pred3 ;

  finite_Adj : Adj ;
  infinite_Adj : Adj ;

  rational_Adj : Adj ; -- top100
  irrational_Adj : Adj ; -- top100
  polynomial_Noun : Noun ; -- top100
  degree_Fun : Fun ; -- top100
  is_root_Relnoun : Relnoun ; -- top100
  cardinality_Fun : Fun ; -- top100
  denumerable_Adj : Adj ; -- top100
  vector_Noun : Noun ; -- top100
  length_Fun : Fun ; -- top100
  norm_Fun : Fun ; -- top100
  perpendicular_Reladj : Reladj ; -- top100
  resultant_Fun : Fun ; -- top100
  square_Fun : Fun ; -- top100
  legendre_symbol_Fun : Fun ; -- top100
  pi_Name : Name ; -- top100
  circle_Noun : Noun ; -- top100
  radius_Fun  : Fun ; -- top100
  area_Fun : Fun ; -- top100
  binomial_Fun : Fun ; -- top100
  combinations_Fun : Fun ; -- top100
  combinationsFromSet_Fun : Fun ; -- top100
  sin_Fun : Fun ;
  cos_Fun : Fun ;
  tan_Fun : Fun ;
  arcsin_Fun : Fun ;
  arccos_Fun : Fun ;
  arctan_Fun : Fun ;
  orthogonal_Reladj : Reladj ;
  angle_between_Fun : Fun ;
  dot_product_Fun : Fun ;
  vector_plus_Fun : Fun ;

}