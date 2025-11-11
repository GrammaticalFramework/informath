abstract VerbalConstants = Categories ** {

fun
  type_Noun : Noun ;
  set_Noun : Noun ;
  proposition_Noun : Noun ;

  number_Noun : Noun ;
  boolean_Noun : Noun ;  
  cardinal_Noun : Noun ;
  digit_Noun : Noun ;
  natural_Noun : Noun ;
  integer_Noun : Noun ;
  rational_Noun : Noun ;
  real_Noun : Noun ;
  complex_Noun : Noun ;

  list_Fam : Fam ;
  set_Fam : Fam ;
  
  Eq_Adj2 : Adj2 ;
  Lt_Adj2 : Adj2 ;
  Gt_Adj2 : Adj2 ;
  Neq_Adj2 : Adj2 ;
  Leq_Adj2 : Adj2 ;
  Geq_Adj2 : Adj2 ;

  Eq_AdjE : AdjE ;
  Neq_AdjC : AdjC ;

  positive_Adj : Adj ;
  negative_Adj : Adj ;

  converge_Verb : Verb ;
  divide_Verb2 : Verb2 ;
  member_Noun2 : Noun2 ;
  divisor_Noun2 : Noun2 ;

  plus_FunC : FunC ;
  minus_Fun2 : Fun2 ;
  times_FunC : FunC ;
  div_Fun2 : Fun2 ;
  pow_Fun2 : Fun2 ;
  neg_Fun : Fun ;
  logarithm_Fun2 : Fun2 ;
  square_root_Fun : Fun ;
  
  successor_Fun : Fun ;
  absolute_value_Fun : Fun ;
  factorial_Fun : Fun ;
  gcd_FunC : FunC ;

  even_Adj : Adj ;
  odd_Adj : Adj ;
  divisible_Adj2 : Adj2 ;
  prime_Adj : Adj ;

  function_Fam2 : Fam2 ;
  
  union_FunC : FunC ;
  intersection_FunC : FunC ;
  cartesian_FunC : FunC ;
  difference_Fun2 : Fun2 ;
  complement_Fun : Fun ;
  powerset_Fun : Fun ;

  subset_Noun2 : Noun2 ;  
  subseteq_Noun2 : Noun2 ;  
  superset_Noun2 : Noun2 ;
  superseteq_Noun2 : Noun2 ;
  equalset_Adj2 : Adj2 ;
  notequalset_Adj2 : Adj2 ;
  element_Noun2 : Noun2 ;
  notelement_Noun2 : Noun2 ;

  emptyset_Name : Name ;
  universeset_Name : Name ;

  congruent_Adj3 : Adj3 ;

  finite_Adj : Adj ;
  infinite_Adj : Adj ;

  rational_Adj : Adj ; -- top100
  irrational_Adj : Adj ; -- top100
  polynomial_Noun : Noun ; -- top100
  degree_Fun : Fun ; -- top100
  root_Noun2 : Noun2 ; -- top100
  cardinality_Fun : Fun ; -- top100
  denumerable_Adj : Adj ; -- top100
  vector_Noun : Noun ; -- top100
  length_Fun : Fun ; -- top100
  norm_Fun : Fun ; -- top100
  perpendicular_Adj2 : Adj2 ; -- top100
  resultant_FunC : FunC ; -- top100
  square_Fun : Fun ; -- top100
  legendre_symbol_Fun2 : Fun2 ; -- top100
  pi_Name : Name ; -- top100
  circle_Noun : Noun ; -- top100
  radius_Fun  : Fun ; -- top100
  area_Fun : Fun ; -- top100
  binomial_Fun2 : Fun2 ; -- top100
  combinations_Fun2 : Fun2 ; -- top100
  combinationsFromSet_Fun2 : Fun2 ; -- top100
  sin_Fun : Fun ;
  cos_Fun : Fun ;
  tan_Fun : Fun ;
  arcsin_Fun : Fun ;
  arccos_Fun : Fun ;
  arctan_Fun : Fun ;
  orthogonal_Adj2 : Adj2 ;
  orthogonal_AdjC : AdjC ;
  perpendicular_Adj2 : Adj2 ;
  perpendicular_AdjC : AdjC ;
  angle_between_Fun2 : Fun2 ;
  dot_product_FunC : FunC ;
  vector_plus_FunC : FunC ;

  sphenic_Adj : Adj ;

-- special constants
  SigmaExp : Ident -> Exp -> Exp -> Exp -> Exp ;
  SeriesExp : Ident -> Exp -> Exp ->  Exp ;
  IntegralExp : Ident -> Exp -> Exp -> Exp -> Exp ;

}