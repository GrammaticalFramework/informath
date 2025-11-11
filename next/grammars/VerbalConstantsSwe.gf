concrete VerbalConstantsSwe of VerbalConstants = CategoriesSwe **

open
  UtilitiesSwe,
  SyntaxSwe,
  ParadigmsSwe,
  SymbolicSwe,
  Prelude

in {

lin
  type_Noun = type_CN ;
  set_Noun = mkNoun mängd_N ;
  proposition_Noun = mkNoun "påstående" ;

  digit_Noun = mkNoun "siffra" ;
  number_Noun = mkNoun tal_N ;
  boolean_Noun = mkNoun "sanningsvärde" ;
  cardinal_Noun = mkNoun (mkN "kardinal" tal_N) ;
  natural_Noun = mkNoun (mkA "naturlig") tal_N ;
  integer_Noun = mkNoun (mkN "hel" tal_N) ;
  rational_Noun = mkNoun (mkA "rationell") tal_N ;
  real_Noun = mkNoun (mkA "reell") tal_N ;
  complex_Noun = mkNoun (mkA "komplex") tal_N ;

  list_Fam = mkFam "lista" ;  

  Eq_Adj2 = mkAdj2  "lika" "med" ;
  Eq_AdjE = mkAP (mkA "lika") ;
  Lt_Adj2 = mkAdj2 "mindre" "än" ; 
  Gt_Adj2 = mkAdj2 "större" "än" ; 
  Neq_Adj2 = mkAdj2 "skild" "från" ;
  Neq_AdjC = mkAP (mkA "olik") ;
  Leq_Adj2 = mkAdj2 "mindre än eller lika" "med" ; 
  Geq_Adj2 = mkAdj2 "större än eller lika" "med" ; 

  positive_Adj = mkAdj "positiv" ;
  negative_Adj = mkAdj "negativ" ;

  converge_Verb = mkVP (mkV "konvergera") ;
  divide_Verb2 = mkV2 "dela" ;
  
  member_Noun2 = mkNoun2 element_N ;
  divisor_Noun2 = mkNoun2 (mkN "delare") ;

  plus_FunC = mkFunC "summa" ;
  minus_Fun2 = mkFun2 (mkN "skillnad" "skillnader") (mkPrep "mellan") (mkPrep "och") ;
  times_FunC = mkFunC (mkN "produkt" "produkter") ;
  div_Fun2 = mkFun2 (mkN "kvot" "kvoter") (mkPrep "mellan") (mkPrep "och") ; ----
  pow_Fun2 = mkFun2 (mkN "potens" "potenser") possess_Prep (mkPrep "upphöjt till") ; ----
  neg_Fun = mkFun (mkN "negation" "negationer") ;
  logarithm_Fun2 = mkFun2 (mkN "logaritm" "logaritmer") (mkPrep "i bas") possess_Prep ;
  square_root_Fun = mkFun (mkN "kvadratrot" "kvadratrötter") ;

  successor_Fun = mkFun (mkN "efterföljare" neutrum) ;
  absolute_value_Fun = mkFun (mkN "absolutbelopp" neutrum) ;
  factorial_Fun = mkFun (mkN "fakultet" "fakulteter") ;
  gcd_Fun = mkFun "störst" "gemensam" "delare" ;

  even_Adj = mkAdj "jämn" ;
  odd_Adj = mkAdj "udda" ;
  divisible_Adj2 = mkAdj2 "delbar" "med" ;
  prime_Adj = mkAdj "prim" ;
{-
  function_Fam2 = mkFam "function" from_Prep to_Prep ;
  union_FunC = mkFun "union" ;
  intersection_FunC = mkFun "intersection" ;
  difference_Fun2 = mkFun2 "difference" "from" ;
  complement_Fun = mkFun "complement" ;
  cartesian_FunC = mkFun "cartesian product" ;
  powerset_Fun = mkFun "power set" ;

  subset_Noun2 = mkNoun2 "proper subset" ;  
  subseteq_Noun2 = mkNoun2 "subset" ;  
  superset_Noun2 = mkNoun2 "proper superset" ;  
  superseteq_Noun2 = mkNoun2 "superset" ;  
  equalset_Adj2 = mkAdj2 "equal" "to" ;
  notequalset_Adj2 = mkAdj2 "distinct" "from" ; ----
  element_Noun2 = mkNoun2 "element" ;
  notelement_Noun2 = mkNoun2 "non-element" ; ----

  emptyset_Name = mkNP the_Det (mkCN (mkA "empty") (mkN "set")) ;
  universeset_Name = mkNP the_Det (mkCN (mkA "universal") (mkN "set")) ;

  congruent_Pred3 = mkPred3 (mkAP (mkA "congruent")) to_Prep (mkPrep "modulo") ;

  finite_Adj = mkAdj "finite" ;
  infinite_Adj = mkAdj "infinite" ;

  combinationsFromSet_Fun2 = mkFun2 "number of combinations" "of size" ;
  combinations_Fun2 = mkFun2 "set of combinations" "of size" ;
  binomial_Fun2 = mkFun2 "binomial coefficient" "over" ;
  area_Fun = mkFun "area" ;
  radius_Fun = mkFun "radius" ;
  circle_Noun = mkNoun "circle" ;
  pi_Name = mkNP the_Det (mkCN (mkN "number") (symb "\\(\\pi\\)")) ;
  legendre_symbol_Fun2 = mkFun2 "Legendre symbol" "over" ;
  square_Fun = mkFun "square" ;
  resultant_FunC = mkFun "resultant" ;
  perpendicular_Adj2 = mkAdj2 "perpendicular" "to" ;
  length_Fun = mkFun "length" ;
  norm_Fun = mkFun "norm" ;
  vector_Noun = mkNoun "vector" ;
  denumerable_Adj = mkAdj "denumerable" ;
  cardinality_Fun = mkFun "cardinality" ;
  is_root_Noun2 = mkNoun2 "root" ;
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
  orthogonal_Adj2 = mkAdj2 "orthogonal" "to" ;
  orthogonal_AdjC = mkAP (mkA "orthogonal") ;
  perpendicular_Adj2 = mkAdj2 "perpendicular" "to" ;
  perpendicular_AdjC = mkAP (mkA "perpendicular") ;
  angle_between_Fun2 = mkFun2 "angle" "with" ;
  dot_product_FunC = mkFun "dot product" ;
  vector_plus_FunC = mkFun "sum" ;

  sphenic_Adj = mkAdj "sphenic" ;

-- special constants

  SigmaExp i m n exp =
    mkNP the_Det (mkCN (mkCN sum_N)
      (SyntaxSwe.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxSwe.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))))) ;
	    
  SeriesExp i m exp =
    mkNP the_Det (mkCN (mkCN series_N)
      (SyntaxSwe.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxSwe.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m infinity_NP)))))))) ;
	    
  IntegralExp i m n exp =
    mkNP the_Det (mkCN (mkCN integral_N)
      (SyntaxSwe.mkAdv possess_Prep
        (mkNP exp (SyntaxSwe.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))) ;

-}
}
