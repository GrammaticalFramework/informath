abstract SymbolicConstants = {

cat
  Set ;     -- Kind                      -- Z
  Const ;   -- Exp                       -- Ø
  Oper ;    -- [Exp] -> Exp symbol       -- +
  Compar ;  -- Exp -> Exp -> Prop symbol -- >

fun
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

  plus_Oper : Oper ;
  minus_Oper : Oper ;
  times_Oper : Oper ;
  div_Oper : Oper ;
  pow_Oper : Oper ;
  neg_Oper : Oper ;
  logarithm_Oper : Oper ;
  square_root_Oper : Oper ;
  
  absolute_value_Oper : Oper ;
  factorial_Oper : Oper ;

  function_Oper : Oper ;
  union_Oper : Oper ;
  intersection_Oper : Oper ;
  cartesian_Oper : Oper ;
  difference_Oper : Oper ;
  complement_Oper : Oper ;
  powerset_Oper : Oper ;

  subset_Compar : Compar ;  
  subseteq_Compar : Compar ;  
  superset_Compar : Compar ;
  superseteq_Compar : Compar ;
  equalset_Compar : Compar ;
  notequalset_Compar : Compar ;
  element_Compar : Compar ;
  notelement_Compar : Compar ;

  emptyset_Const : Const ;
  universeset_Const : Const ;

  cardinality_Oper : Oper ; -- top100
  length_Oper : Oper ; -- top100
  perpendicular_Compar : Compar ; -- top100
  square_Oper : Oper ; -- top100
  legendre_symbol_Oper : Oper ; -- top100
  pi_Const : Const ; -- top100
  binomial_Oper : Oper ; -- top100
  combinations_Oper : Oper ; -- top100
  sin_Oper : Oper ;
  cos_Oper : Oper ;
  tan_Oper : Oper ;
  arcsin_Oper : Oper ;
  arccos_Oper : Oper ;
  arctan_Oper : Oper ;
  orthogonal_Compar : Compar ;
  dot_product_Oper : Oper ;
  vector_plus_Oper : Oper ;

}