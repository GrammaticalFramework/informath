concrete WikidataWordsFre of WikidataWords = CategoriesFre **

open
  UtilitiesFre,
  ParadigmsFre

in {

lin empty_Adj = mkAdj (mkA "vide") ;
lin type_Noun = type_CN ;
lin set_Noun = mkNoun ensemble_N ;
lin number_Noun = mkNoun nombre_N ;
lin cardinal_Noun = mkNoun "cardinal" ;
lin integer_Noun = mkNoun "entier" ;
lin complex_Noun = mkNoun (mkA "complexe") nombre_N ;
lin positive_Adj = mkAdj "positif" ;
lin negative_Adj = mkAdj "negatif" ;
lin even_Adj = mkAdj "pair" ;
lin odd_Adj = mkAdj "impair" ;
lin prime_Adj = mkAdj "premier" ;
lin finite_Adj = mkAdj "fini" ;
lin infinite_Adj = mkAdj "infini" ;
lin circle_Noun = mkNoun (mkN "cercle" masculine) ;
lin vector_Noun = mkNoun "vecteur" ;
lin polynomial_Noun = mkNoun (mkN "polynôme" masculine) ;
lin rational_Adj = mkAdj "rationnel" ;
lin sphenic_Adj = mkAdj "sphénique" ;

}