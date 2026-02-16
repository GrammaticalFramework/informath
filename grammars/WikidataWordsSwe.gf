concrete WikidataWordsSwe of WikidataWords = CategoriesSwe **

open
  UtilitiesSwe,
  ParadigmsSwe

in {

lin type_Noun = type_CN ;
lin set_Noun = mkNoun mängd_N ;
lin number_Noun = mkNoun tal_N ;
lin cardinal_Noun = mkNoun (mkN "kardinal" tal_N) ;
lin integer_Noun = mkNoun (mkN "hel" tal_N) ;
lin complex_Noun = mkNoun (mkA "komplex") tal_N ;
lin positive_Adj = mkAdj "positiv" ;
lin negative_Adj = mkAdj "negativ" ;
lin even_Adj = mkAdj "jämn" ;
lin odd_Adj = mkAdj "udda" ;
lin prime_Adj = mkAdj "prim" ;
lin finite_Adj = mkAdj "ändlig" ;
lin infinite_Adj = mkAdj "oändlig" ;
lin circle_Noun = mkNoun (mkN "cirkel" "cirklar") ;
lin vector_Noun = mkNoun (mkN "vektor" "vektorer") ;
lin polynomial_Noun = mkNoun (mkN "polynom" "polynom") ;
lin rational_Adj = mkAdj "rationell" ;
lin sphenic_Adj = mkAdj "sfenisk" ;

}
