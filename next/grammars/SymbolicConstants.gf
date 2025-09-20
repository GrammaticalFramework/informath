abstract SymbolicConstants = Terms ** {

fun
  natural_Term : Term ;
  integer_Term : Term ;
  rational_Term : Term ;
  real_Term : Term ;
  complex_Term : Term ;

  Eq_Eqsign : Eqsign ;
  Lt_Eqsign : Eqsign ;
  Gt_Eqsign : Eqsign ;
  Neq_Eqsign : Eqsign ;
  Leq_Eqsign : Eqsign ;
  Geq_Eqsign : Eqsign ;

  plus_Term : Term -> Term -> Term ;
  minus_Term : Term -> Term -> Term ;
  times_Term : Term -> Term -> Term ;
  div_Term : Term -> Term -> Term ;
  pow_Term : Term -> Term -> Term ;
  neg_Term : Term -> Term -> Term ;
  logarithm_Term : Term -> Term -> Term ;
  square_root_Term : Term -> Term ;
  
  absolute_value_Term : Term -> Term ;
  factorial_Term : Term -> Term -> Term ;

  function_Term : Term -> Term -> Term ;
  union_Term : Term -> Term -> Term ;
  intersection_Term : Term -> Term -> Term ;
  cartesian_Term : Term -> Term -> Term ;
  difference_Term : Term -> Term -> Term ;
  complement_Term : Term -> Term -> Term ;
  powerset_Term : Term -> Term -> Term ;

  subset_Eqsign : Eqsign ;  
  subseteq_Eqsign : Eqsign ;  
  superset_Eqsign : Eqsign ;
  superseteq_Eqsign : Eqsign ;
  equalset_Eqsign : Eqsign ;
  notequalset_Eqsign : Eqsign ;
  element_Eqsign : Eqsign ;
  notelement_Eqsign : Eqsign ;

  emptyset_Term : Term ;
  universeset_Term : Term ;

  cardinality_Term : Term -> Term ; -- top100
  length_Term : Term -> Term ; -- top100
  perpendicular_Eqsign : Eqsign ; -- top100
  square_Term : Term -> Term -> Term ; -- top100
  legendre_symbol_Term : Term -> Term -> Term ; -- top100
  pi_Term : Term ; -- top100
  binomial_Term : Term -> Term -> Term ; -- top100
  combinations_Term : Term -> Term -> Term ; -- top100
  sin_Term : Term -> Term ;
  cos_Term : Term -> Term ;
  tan_Term : Term -> Term ;
  arcsin_Term : Term -> Term ;
  arccos_Term : Term -> Term ;
  arctan_Term : Term -> Term ;
  orthogonal_Eqsign : Eqsign ;
  dot_product_Term : Term -> Term -> Term ;
  vector_plus_Term : Term -> Term -> Term ;

}