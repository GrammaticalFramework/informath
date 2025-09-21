abstract SymbolicConstants = Terms ** {

fun
  natural_Term : Term ;
  integer_Term : Term ;
  rational_Term : Term ;
  real_Term : Term ;
  complex_Term : Term ;

  Eq_Compar : Compar ;
  Lt_Compar : Compar ;
  Gt_Compar : Compar ;
  Neq_Compar : Compar ;
  Leq_Compar : Compar ;
  Geq_Compar : Compar ;

  plus_Term : Term -> Term -> Term ;
  minus_Term : Term -> Term -> Term ;
  times_Term : Term -> Term -> Term ;
  div_Term : Term -> Term -> Term ;
  pow_Term : Term -> Term -> Term ;
  neg_Term : Term -> Term ;
  logarithm_Term : Term -> Term -> Term ;
  square_root_Term : Term -> Term ;
  
  absolute_value_Term : Term -> Term ;
  factorial_Term : Term -> Term ;

  function_Term : Term -> Term -> Term ;
  union_Term : Term -> Term -> Term ;
  intersection_Term : Term -> Term -> Term ;
  cartesian_Term : Term -> Term -> Term ;
  difference_Term : Term -> Term -> Term ;
  complement_Term : Term -> Term ;
  powerset_Term : Term -> Term ;

  subset_Compar : Compar ;  
  subseteq_Compar : Compar ;  
  superset_Compar : Compar ;
  superseteq_Compar : Compar ;
  equalset_Compar : Compar ;
  notequalset_Compar : Compar ;
  element_Compar : Compar ;
  notelement_Compar : Compar ;

  emptyset_Term : Term ;
  universeset_Term : Term ;

  cardinality_Term : Term -> Term ; -- top100
  length_Term : Term -> Term ; -- top100
  perpendicular_Compar : Compar ; -- top100
  square_Term : Term -> Term ; -- top100
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
  orthogonal_Compar : Compar ;
  dot_product_Term : Term -> Term -> Term ;
  vector_plus_Term : Term -> Term -> Term ;

}