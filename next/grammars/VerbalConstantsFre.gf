concrete VerbalConstantsFre of VerbalConstants = CategoriesFre **

open
  UtilitiesFre,
  SyntaxFre,
  ParadigmsFre,
  SymbolicFre,
  Prelude

in {

lin
  type_Noun = type_CN ;
  set_Noun = mkNoun ensemble_N ;
  proposition_Noun = mkNoun "proposition" ;

  digit_Noun = mkNoun (mkN "chiffra" masculine) ;
  number_Noun = mkNoun nombre_N ;
  boolean_Noun = mkNoun (mkCN (mkA "booléen") (mkN "valeur" feminine)) ;
  cardinal_Noun = mkNoun "cardinal" ;
  natural_Noun = mkNoun (mkA "naturel") nombre_N ;
  integer_Noun = mkNoun "entier" ;
  rational_Noun = mkNoun (mkA "rationnel") nombre_N ;
  real_Noun = mkNoun (mkA "réel") nombre_N ;
  complex_Noun = mkNoun (mkA "complexe") nombre_N ;

  list_Fam = mkFam "liste" ;  
  set_Fam = mkFam ensemble_N ;  

  Eq_Adj2 = mkAdj2  (mkA "égal") dative ;
  Eq_AdjE = mkAdj (mkAP (mkA "égal")) ;
  Lt_Adj2 = mkAdj2 (mkA "inférieur") dative ;
  Gt_Adj2 = mkAdj2 (mkA "supérieur") dative ;
  Neq_Adj2 = mkAdj2 (mkA "distinct") genitive ;
  Neq_AdjC = mkAP (mkA "distinct") ;
  Leq_Adj2 = mkAdj2 (mkAP or_Conj (mkAP (mkA "inférieur")) (mkAP (mkA "égal"))) dative ; 
  Geq_Adj2 = mkAdj2 (mkAP or_Conj (mkAP (mkA "supérieur")) (mkAP (mkA "égal"))) dative ;

  positive_Adj = mkAdj "positif" ;
  negative_Adj = mkAdj "negatif" ;

  converge_Verb = mkVP (mkV "converger") ;
  divide_Verb2 = mkV2 "diviser" ;
  
  member_Noun2 = mkNoun2 element_N ;
  divisor_Noun2 = mkNoun2 (mkN "diviseur") ;

  plus_FunC = mkFunC "somme" ;
  minus_Fun2 = mkFun2 (mkN "différence") (mkPrep "entre") (mkPrep "et") ;
  times_FunC = mkFunC (mkN "produit") ;
  div_Fun2 = mkFun2 (mkN "quotient") (mkPrep "entre") (mkPrep "et") ; ----
  pow_Fun2 = mkFun2 (mkN "puissance") possess_Prep (mkPrep "en") ; ----
  neg_Fun = mkFun (mkN "négation") ;
  logarithm_Fun2 = mkFun2 (mkN "logarithme" masculine) (mkPrep "en base") possess_Prep ; ----
  square_root_Fun = mkFun (mkCN (mkA "carré") (mkN "racine")) ;

  successor_Fun = mkFun (mkN "successeur") ;
  absolute_value_Fun = mkFun (mkCN (mkA "absolu") (mkN "valeur" feminine)) ;
  factorial_Fun = mkFun (mkN "factorielle") ;
  gcd_FunC = mkFunC "plus grand" "commun" "diviseur" ; ---- should be in this order

  even_Adj = mkAdj "pair" ;
  odd_Adj = mkAdj "impair" ;
  divisible_Adj2 = mkAdj2 "divisible" "par" ;
  prime_Adj = mkAdj "premier" ;

  function_Fam2 = mkFam2 "fonction" genitive dative ;
  union_FunC = mkFun "union" ;
  intersection_FunC = mkFun "intersection" ;
  complement_Fun = mkFun (mkN "complément") ;
  cartesian_FunC = mkFun (mkCN (mkA "cartésien") (mkN "produit")) ;
  difference_Fun2 = mkFun2 (mkN "différence") (mkPrep "entre") (mkPrep "et") ;
  powerset_Fun = mkFun "puissance" ; ----

  subset_Noun2 = mkNoun2 (mkCN (mkA "propre") (mkN "sous-ensemble" masculine)) ;  
  subseteq_Noun2 = mkNoun2 (mkN "sous-ensemble" masculine) ;  
  superset_Noun2 = mkNoun2 (mkCN (mkA "propre") (mkN "sur-ensemble" masculine)) ;
  superseteq_Noun2 = mkNoun2 (mkN "sur-ensemble" masculine) ;
  equalset_Adj2 = mkAdj2 (mkA "égal") dative ;
  notequalset_Adj2 = mkAdj2 (mkA "égal") dative ; ----
  element_Noun2 = mkNoun2 (mkN "élément") ;
  notelement_Noun2 = mkNoun2 (mkN "non-élément") ; ----

  emptyset_Name = mkName (mkNP the_Det (mkCN (mkA "vide") ensemble_N)) ;
  universeset_Name = mkName (mkNP the_Det (mkCN (mkA "universel") ensemble_N)) ;

  congruent_Adj3 = mkAdj3 (mkAP (mkA "congruent")) dative (mkPrep "modulo") ;

  finite_Adj = mkAdj "fini" ;
  infinite_Adj = mkAdj "infini" ;

  combinationsFromSet_Fun2 = mkFun2 (mkCN nombre_N (SyntaxFre.mkAdv genitive (mkNP thePl_Det combinaison_N))) genitive on_Prep ; ----
  combinations_Fun2 = mkFun2 (mkCN ensemble_N (SyntaxFre.mkAdv genitive (mkNP aPl_Det combinaison_N))) genitive on_Prep ; ----
  binomial_Fun2 = mkFun2 (mkCN (mkA "binomial") (mkN "coefficient")) genitive on_Prep ; ----

  area_Fun = mkFun "aire" ;
  radius_Fun = mkFun "rayon" ;
  circle_Noun = mkNoun (mkN "cercle" masculine) ;
  pi_Name = mkName (mkNP the_Det (mkCN nombre_N (symb "\\(\\pi\\)"))) ;
  legendre_symbol_Fun2 = mkFun2 (mkCN (mkN "symbole" masculine) (SyntaxFre.mkAdv genitive (mkNP (mkPN "Legendre")))) genitive on_Prep ; ----
  square_Fun = mkFun (mkN "carré" masculine) ;
  resultant_FunC = mkFunC (mkN "addition") ;
  perpendicular_Adj2 = mkAdj2 (mkA "perpendiculaire") dative ;
  perpendicular_AdjC = mkAdjC (mkA "perpendiculaire") ;
  length_Fun = mkFun "norme" ;
  norm_Fun = mkFun "norme" ;
  vector_Noun = mkNoun "vecteur" ;
  denumerable_Adj = mkAdj "dénombrable" ;
  cardinality_Fun = mkFun "cardinalité" ;
  root_Noun2 = mkNoun2 (mkN "racine") genitive ;
  degree_Fun = mkFun (mkN "degré" masculine) ;
  polynomial_Noun = mkNoun (mkN "polynôme" masculine) ;
  irrational_Adj = mkAdj "irrationnel" ;
  rational_Adj = mkAdj "rationnel" ;

  sin_Fun = mkFun "sinus" ;
  cos_Fun = mkFun "cosinus" ;
  tan_Fun = mkFun "tangente" ;
  arcsin_Fun = mkFun "arcsinus" ;
  arccos_Fun = mkFun "arccosinus" ;
  arctan_Fun = mkFun "arctangente" ;
  orthogonal_Adj2 = mkAdj2 (mkA "orthogonal") dative ;
  orthogonal_AdjC = mkAdjC (mkA "orthogonal") ;
  angle_between_Fun2 = mkFun2 (mkN "angle" masculine) (mkPrep "entre") (mkPrep "et") ;
  dot_product_FunC = mkFunC (mkCN (mkA "scalaire") (mkN "produit")) ;
  vector_plus_FunC = mkFunC "somme" ;

  sphenic_Adj = mkAdj "sphénique" ;

{-
  function_Fam2 = mkFam2 (mkN "funktion" "funktioner") from_Prep to_Prep ;
  union_FunC = mkFun "union" ;
  intersection_FunC = mkFun (mkN "snitt" "snitt") ;
  difference_Fun2 = mkFun2 "difference" "from" ;
  complement_Fun = mkFun (mkN "komplement" "komplement") ;
  cartesian_FunC = mkFun (mkCN (mkA "kartesisk") (mkN "produkt")) ;
  powerset_Fun = mkFun (mkN "potens" ensemble_N) ;

  subset_Noun2 = mkNoun2 (mkCN (mkA "äkta") (mkN "del" ensemble_N)) ;
  subseteq_Noun2 = mkNoun2 (mkN "del" ensemble_N) ;  
  superset_Noun2 = mkNoun2 (mkCN (mkA "äkta") (mkN "över" ensemble_N)) ;
  superseteq_Noun2 = mkNoun2 (mkN "över" ensemble_N) ;
  equalset_Adj2 = mkAdj2 (mkA "lika") with_Prep ;
  notequalset_Adj2 = mkAdj2 (mkA "skild") from_Prep ;
  element_Noun2 = mkNoun2 element_N ;
  notelement_Noun2 = mkNoun2 (mkN "icke-" element_N) ; ----

  emptyset_Name = mkName (mkNP the_Det (mkCN tom_A ensemble_N)) ;
  universeset_Name = mkName (mkNP the_Det (mkN "universal" ensemble_N)) ;

  congruent_Adj3 = mkAdj3 (mkA "kongruent") with_Prep (mkPrep "modulo") ;

  finite_Adj = mkAdj "ändlig" ;
  infinite_Adj = mkAdj "oändlig" ;

  combinationsFromSet_Fun2 = mkFun2 (mkCN (mkN "an" nombre_N) (SyntaxFre.mkAdv (mkPrep "") (mkNP aPl_Det (mkN "kombination")))) from_Prep (mkPrep "av storlek") ;
  combinations_Fun2 = mkFun2 (mkCN ensemble_N (SyntaxFre.mkAdv possess_Prep (mkNP aPl_Det  (mkN "Kombination")))) from_Prep (mkPrep "av storlek") ;
  binomial_Fun2 = mkFun2 (mkCN (mkN "binomialkoefficient" "binomialkoefficienter")) possess_Prep (mkPrep "över") ;

  area_Fun = mkFun "area" ;
  radius_Fun = mkFun "radie" ;
  circle_Noun = mkNoun (mkN "cirkel" "cirklar") ;
  pi_Name = mkName (mkNP the_Det (mkCN nombre_N (symb "\\(\\pi\\)"))) ;
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
-}

{-

-- special constants

  SigmaExp i m n exp =
    mkNP the_Det (mkCN (mkCN sum_N)
      (SyntaxFre.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxFre.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))))) ;
	    
  SeriesExp i m exp =
    mkNP the_Det (mkCN (mkCN series_N)
      (SyntaxFre.mkAdv possess_Prep
        (mkNP all_Predet
	  (mkNP thePl_Det (mkCN (mkCN (mkAP given_A2 exp) number_N)
	    (SyntaxFre.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m infinity_NP)))))))) ;
	    
  IntegralExp i m n exp =
    mkNP the_Det (mkCN (mkCN integral_N)
      (SyntaxFre.mkAdv possess_Prep
        (mkNP exp (SyntaxFre.mkAdv where_Subj (mkS (mkCl (latexSymbNP (mkSymb i)) range_V3 m n)))))) ;

-}
}
