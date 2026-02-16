concrete WikidataWordsGer of WikidataWords = CategoriesGer **

open
  UtilitiesGer,
  ParadigmsGer

in {

lin empty_Adj = mkAdj (mkA "leer") ;
lin type_Noun = mkNoun (mkN "Typ") ;
lin set_Noun = mkNoun menge_N ;
lin number_Noun = mkNoun zahl_N ;
lin cardinal_Noun = mkNoun (mkN "Kardinal" zahl_N) ;
lin integer_Noun = mkNoun (mkA "ganz") zahl_N ;
lin complex_Noun = mkNoun (mkA "komplex") zahl_N ;
lin positive_Adj = mkAdj "positiv" ;
lin negative_Adj = mkAdj "negativ" ;
lin even_Adj = mkAdj "gerade" ;
lin odd_Adj = mkAdj "ungerade" ;
lin prime_Adj = mkAdj "prim" ;
lin finite_Adj = mkAdj "endlich" ;
lin infinite_Adj = mkAdj "unendlich" ;
lin circle_Noun = mkNoun (mkN "Kreis") ;
lin vector_Noun = mkNoun (mkN "Vektor") ;
lin polynomial_Noun = mkNoun (mkN "Polynom") ;
lin rational_Adj = mkAdj "rational" ;
lin sphenic_Adj = mkAdj "sphenisch" ;

}