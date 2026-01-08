abstract SymbolicConstants = Terms ** {

fun
  natural_Const : Const ;
  integer_Const : Const ;
  rational_Const : Const ;
  real_Const : Const ;
  complex_Const : Const ;

  Eq_Compar : Compar ;
  Lt_Compar : Compar ;
  Gt_Compar : Compar ;
  Neq_Compar : Compar ;
  Leq_Compar : Compar ;
  Geq_Compar : Compar ;

  plus_Oper2 : Oper2 ;
  minus_Oper2 : Oper2 ;
  times_Oper2 : Oper2 ;
  div_Oper2 : Oper2 ;
  pow_Oper2 : Oper2 ;
  neg_Oper : Oper ;
  logarithm_Oper2 : Oper2 ;
  square_root_Oper : Oper ;
  
  absolute_value_Oper : Oper ;
  factorial_Oper : Oper ;

  function_Oper2 : Oper2 ;
  union_Oper2 : Oper2 ;
  intersection_Oper2 : Oper2 ;
  cartesian_Oper2 : Oper2 ;
  difference_Oper2 : Oper2 ;
  complement_Oper : Oper ;
  powerset_Oper : Oper ;
  
  positive_part_Oper : Oper ; -- R^+
  negative_part_Oper : Oper ;

  subset_Compar : Compar ;  
  subseteq_Compar : Compar ;  
  superset_Compar : Compar ;
  superseteq_Compar : Compar ;
  element_Compar : Compar ;
  notelement_Compar : Compar ;

  emptyset_Const : Const ;
  universeset_Const : Const ;

  cardinality_Oper : Oper ; -- top100
  length_Oper : Oper ; -- top100
  perpendicular_Compar : Compar ; -- top100
  square_Oper : Oper ; -- top100
  legendre_symbol_Oper2 : Oper2 ; -- top100
  pi_Const : Const ; -- top100
  binomial_Oper2 : Oper2 ; -- top100
  combinations_Oper2 : Oper2 ; -- top100
  
  sin_Oper : Oper ;
  cos_Oper : Oper ;
  tan_Oper : Oper ;
  arcsin_Oper : Oper ;
  arccos_Oper : Oper ;
  arctan_Oper : Oper ;
  orthogonal_Compar : Compar ;
  
  dot_product_Oper2 : Oper2 ;


-- special syntax

  times_Term : Term -> Term -> Term ; -- special syntax with no operator

  modulo_Formula : Term -> Term -> Term -> Formula ;

  sigma_Term : Term -> Term -> Ident -> Term -> Term ;
  sum3dots_Term : Term -> Term -> Term -> Term ;
  series_Term : Term -> Ident -> Term -> Term ;
  integral_Term : Term -> Term -> Ident -> Term -> Term ;


}