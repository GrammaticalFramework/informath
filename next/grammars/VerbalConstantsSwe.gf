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
  set_Fam = mkFam mängd_N ;  

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
  gcd_FunC = mkFunC "störst" "gemensam" "delare" ;

  even_Adj = mkAdj "jämn" ;
  odd_Adj = mkAdj "udda" ;
  divisible_Adj2 = mkAdj2 "delbar" "med" ;
  prime_Adj = mkAdj "prim" ;

  function_Fam2 = mkFam2 (mkN "funktion" "funktioner") from_Prep to_Prep ;
  union_FunC = mkFun "union" ;
  intersection_FunC = mkFun (mkN "snitt" "snitt") ;
  difference_Fun2 = mkFun2 "difference" "from" ;
  complement_Fun = mkFun (mkN "komplement" "komplement") ;
  cartesian_FunC = mkFun (mkCN (mkA "kartesisk") (mkN "produkt")) ;
  powerset_Fun = mkFun (mkN "potens" mängd_N) ;

  subset_Noun2 = mkNoun2 (mkCN (mkA "äkta") (mkN "del" mängd_N)) ;
  subseteq_Noun2 = mkNoun2 (mkN "del" mängd_N) ;  
  superset_Noun2 = mkNoun2 (mkCN (mkA "äkta") (mkN "över" mängd_N)) ;
  superseteq_Noun2 = mkNoun2 (mkN "över" mängd_N) ;
  equalset_Adj2 = mkAdj2 (mkA "lika") with_Prep ;
  notequalset_Adj2 = mkAdj2 (mkA "skild") from_Prep ;
  element_Noun2 = mkNoun2 element_N ;
  notelement_Noun2 = mkNoun2 (mkN "icke-" element_N) ; ----

  emptyset_Name = mkName (mkNP the_Det (mkCN tom_A mängd_N)) ;
  universeset_Name = mkName (mkNP the_Det (mkN "universal" mängd_N)) ;

  congruent_Adj3 = mkAdj3 (mkA "kongruent") with_Prep (mkPrep "modulo") ;

  finite_Adj = mkAdj "ändlig" ;
  infinite_Adj = mkAdj "oändlig" ;

  combinationsFromSet_Fun2 = mkFun2 (mkCN (mkN "an" tal_N) (SyntaxSwe.mkAdv (mkPrep "") (mkNP aPl_Det (mkN "kombination")))) from_Prep (mkPrep "av storlek") ;
  combinations_Fun2 = mkFun2 (mkCN mängd_N (SyntaxSwe.mkAdv possess_Prep (mkNP aPl_Det  (mkN "Kombination")))) from_Prep (mkPrep "av storlek") ;
  binomial_Fun2 = mkFun2 (mkCN (mkN "binomialkoefficient" "binomialkoefficienter")) possess_Prep (mkPrep "över") ;

  area_Fun = mkFun "area" ;
  radius_Fun = mkFun "radie" ;
  circle_Noun = mkNoun (mkN "cirkel" "cirklar") ;
  pi_Name = mkName (mkNP the_Det (mkCN tal_N (symb "\\(\\pi\\)"))) ;
  legendre_symbol_Fun2 = mkFun2 (mkN "Legendresymbol" "Legendresymboler") possess_Prep (mkPrep "över") ;
  square_Fun = mkFun (mkN "kvadrat" "kvadrater") ;
  resultant_FunC = mkFunC (mkN "resultant" "resultanter") ;
  perpendicular_Adj2 = mkAdj2 "vinkelrät" "mot" ;
  perpendicular_AdjC = mkAdjC "vinkelrät" ;
  length_Fun = mkFun (mkN "längd" "längder") ;
  norm_Fun = mkFun (mkN "norm" "normer") ;
  vector_Noun = mkNoun (mkN "vektor" "vektorer") ;
  denumerable_Adj = mkAdj "uppräknelig" ;
  cardinality_Fun = mkFun (mkN "kardinalitet" "kardinaliteter") ;
  root_Noun2 = mkNoun2 (mkN "rot" "rötter") ;
  degree_Fun = mkFun (mkN "grad" "grader") ;
  polynomial_Noun = mkNoun (mkN "polynom" "polynom") ;
  irrational_Adj = mkAdj "irrationell" ;
  rational_Adj = mkAdj "rationell" ;
  
  sin_Fun = mkFun "sinus" ;
  cos_Fun = mkFun "cosinus" ;
  tan_Fun = mkFun "tangens" ;
  arcsin_Fun = mkFun (mkN "arcsinus") ;
  arccos_Fun = mkFun (mkN "arccosinus") ;
  arctan_Fun = mkFun (mkN "arctangens") ;
  orthogonal_Adj2 = mkAdj2 "ortogonal" "till" ;
  orthogonal_AdjC = mkAdjC "ortogonal" ;
  angle_between_Fun2 = mkFun2 (mkN "vinkel") possess_Prep (mkPrep "med") ;
  dot_product_FunC = mkFunC "punktprodukt" ;
  vector_plus_FunC = mkFunC "summa" ;
  
  sphenic_Adj = mkAdj "sfenisk" ;

{-

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
