concrete VerbalConstantsGer of VerbalConstants = CategoriesGer **

open
  UtilitiesGer,
  SyntaxGer,
  ParadigmsGer,
  SymbolicGer,
  Prelude

in {

lin
  type_Noun = mkNoun (mkN "Typ") ;
  set_Noun = mkNoun menge_N ;
  proposition_Noun = mkNoun "Proposition" ;

  digit_Noun = mkNoun (mkN "Ziffer" "Ziffer" masculine) ;
  number_Noun = mkNoun zahl_N ;
  boolean_Noun = mkNoun "Wahrheitswert" ;
  cardinal_Noun = mkNoun (mkN "Kardinal" zahl_N) ;
  
  list_Fam = mkFam "Liste" ;
  set_Fam = mkFam menge_N ;  

  natural_Noun = mkNoun (mkA "natürlich") zahl_N ;
  integer_Noun = mkNoun (mkA "ganz") zahl_N ;
  rational_Noun = mkNoun (mkA "rational") zahl_N ;
  real_Noun = mkNoun (mkA "reell") zahl_N ;
  complex_Noun = mkNoun (mkA "komplex") zahl_N ;

  Eq_Adj2 = mkAdj2 "" "gleich" ;
  Eq_AdjE = mkAdjE "gleich" ;
  Lt_Adj2 = mkAdj2 "" "kleiner als" ; ---- ...
  Gt_Adj2 = mkAdj2 "" "größer als" ; 
  Neq_Adj2 = mkAdj2 "" "ungleich" ; ---- 
  Neq_AdjC = mkAdjC "ungleich" ; ---- 
  Leq_Adj2 = mkAdj2 "" "kleiner oder gleich" ; 
  Geq_Adj2 =  mkAdj2 "" "größer oder gleich" ;

  positive_Adj = mkAdj "positiv" ;
  negative_Adj = mkAdj "negativ" ;

  converge_Verb = mkVerb (mkV "konvergieren") ;
  divide_Verb2 = mkVerb2 (mkV2 "teilen") ;
  member_Noun2 = mkNoun2 (mkN "Element" "Elemente" neuter) ;
  divisor_Noun2 = mkNoun2 (mkN "Teiler") ;

  plus_FunC = mkFunC "Summe" ;
  minus_Fun2 = mkFun2 (mkN "Differenz" feminine) (mkPrep "zwischen" dative) ;
  times_FunC = mkFunC (mkN "Produkt" neuter) ;
  div_Fun2 = mkFun2 (mkN "Quotient" "Quotienten" masculine) between_Prep ;
  pow_Fun2 = mkFun2 (mkN "Potenz" feminine) ; ----
  neg_Fun = mkFun "Negation" ;
  logarithm_Fun2 = mkFun2 "Logarithmus" ;
  square_root_Fun = mkFun (mkN "Quadratwurzel" feminine) ;

  successor_Fun = mkFun (mkN "Nachfolger" masculine) ;
  absolute_value_Fun = mkFun (mkCN (mkA "absolut") (mkN "Wert" masculine)) ;
  factorial_Fun = mkFun (mkN "Fakultät" feminine);
  gcd_FunC = mkFunC "groß" "gemeinsam" "Teiler" ;

  even_Adj = mkAdj "gerade" ;
  odd_Adj = mkAdj "ungerade" ;
  divisible_Adj2 = mkAdj2 (mkA "teilbar") (mkPrep "durch" accusative) ;
  prime_Adj = mkAdj "prim" ;

  function_Fam2 = mkFam2 (mkCN (mkN "Funktion")) from_Prep to_Prep ;
  union_FunC = mkFunC "Vereinigung" ;
  intersection_FunC = mkFunC (mkN "Schnittmenge") ;
  complement_Fun = mkFun (mkN "Komplement") ;
  cartesian_FunC = mkFunC (mkCN (mkA "kartesische") (mkN "Produkt")) ;
  difference_Fun2 = mkFun2 (mkN "Differenz" feminine);
  powerset_Fun = mkFun "Potenzmenge" ;

  subset_Noun2 = mkNoun2 (mkCN (mkA "echt") (mkN "Teilmenge"));
  subseteq_Noun2 = mkNoun2 (mkN "Teilmenge") ;  
  superset_Noun2 = mkNoun2 (mkCN (mkA "echt") (mkN "Obermenge")) ;
  superseteq_Noun2 = mkNoun2 (mkN "Obermenge") ;
  equalset_Adj2 = mkAdj2 "" "gleich" ; ----
  notequalset_Adj2 = mkAdj2 "" "ungleich" ; ----
  element_Noun2 = mkNoun2 element_N ;
  notelement_Noun2 = mkNoun2 (mkN "nicht-" element_N) ; ----

  emptyset_Name = mkName (mkNP the_Det (mkCN (mkA "leer") menge_N)) ;
  universeset_Name = mkName (mkNP the_Det (mkCN (mkA "universell") menge_N)) ;

  congruent_Adj3 = mkAdj3 (mkAP (mkA "kongruent")) with_Prep (mkPrep "modulo" dative) ;
  
  finite_Adj = mkAdj "endlich" ;
  infinite_Adj = mkAdj "unendlich" ;

  combinationsFromSet_Fun2 = mkFun2 (mkCN (mkN "An" zahl_N) (SyntaxGer.mkAdv (mkPrep genitive) (mkNP thePl_Det (mkN "Kombination")))) ;
  combinations_Fun2 = mkFun2 (mkCN menge_N (SyntaxGer.mkAdv (mkPrep genitive) (mkNP aPl_Det  (mkN "Kombination")))) ;
  binomial_Fun2 = mkFun2 (mkCN (mkN "Binomialkoeffizient")) ;

  area_Fun = mkFun "Fläche" ;
  radius_Fun = mkFun "Radius" ;
  circle_Noun = mkNoun (mkN "Kreis") ;
  pi_Name = mkName (mkNP the_Det (mkCN zahl_N (symb "\\(\\pi\\)"))) ;
  legendre_symbol_Fun2 = mkFun2 (mkN "Legendresymbol") ;
  square_Fun = mkFun (mkN "Quadrat" neuter) ;
  resultant_FunC = mkFunC (mkN "Ergebnis") ;
  perpendicular_Adj2 = mkAdj2 "senkrecht" "zu" ;
  perpendicular_AdjC = mkAdjC "senkrecht" ;
  length_Fun = mkFun (mkN "Länge") ;
  norm_Fun = mkFun (mkN "Betrag" "Beträge" masculine) ;
  vector_Noun = mkNoun (mkN "Vektor") ;
  denumerable_Adj = mkAdj "abzählbar" ;
  cardinality_Fun = mkFun (mkN "Kardinalität") ;
  root_Noun2 = mkNoun2 (mkN "Wurzel" feminine) ;
  degree_Fun = mkFun (mkN "Grad") ;
  polynomial_Noun = mkNoun (mkN "Polynom") ;
  irrational_Adj = mkAdj "irrational" ;
  rational_Adj = mkAdj "rational" ;
  
  sin_Fun = mkFun "Sinus" ;
  cos_Fun = mkFun "Cosinus" ;
  tan_Fun = mkFun "Tangens" ;
  arcsin_Fun = mkFun (mkN "Arcsinus") for_Prep ;
  arccos_Fun = mkFun (mkN "Arccosinus") for_Prep ;
  arctan_Fun = mkFun (mkN "Arctangens") for_Prep ;
  orthogonal_Adj2 = mkAdj2 (mkA "orthogonal") to_Prep ;
  orthogonal_AdjC = mkAdjC (mkA "orthogonal") ;
  angle_between_Fun2 = mkFun2 (mkN "Winkel") (mkPrep "zwischen" dative) ;
  dot_product_FunC = mkFunC "Skalarprodukt" ;
  vector_plus_FunC = mkFunC "Summe" ;

  sphenic_Adj = mkAdj "sphenisch" ;


{-

-- special constants

  SigmaExp i m n exp =
    mkNP the_Det (mkCN (mkCN sum_N)
      (SyntaxGer.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxGer.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))))) ;
	    
  SeriesExp i m exp =
    mkNP the_Det (mkCN (mkCN series_N)
      (SyntaxGer.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxGer.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m infinity_NP)))))))) ;
	    
  IntegralExp i m n exp =
    mkNP the_Det (mkCN (mkCN integral_N)
      (SyntaxGer.mkAdv possess_Prep
        (mkNP exp (SyntaxGer.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))) ;

-}
}
