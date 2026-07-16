concrete VerbalConstantsPol of VerbalConstants = CategoriesPol **

open
UtilitiesPol,
SyntaxPol,
ParadigmsPol,
(R=ResPol),
(V=VerbMorphoPol),
SymbolicPol,
Prelude

in {

lin proposition_Noun = mkNoun (neutEN "zdanie") ;

lin digit_Noun = mkNoun (femN "cyfra" "cyfry") ;

lin boolean_Noun = mkNoun (postAdjN "logiczny" (oscN "wartość")) ;

lin natural_Noun = mkNoun (postAdjN "naturalny" liczba_N) ;

lin rational_Noun = mkNoun (postAdjN "wymierny" liczba_N) ;
lin real_Noun = mkNoun (postAdjN "rzeczywisty" liczba_N) ;


lin list_Fam = mkFam (femN "lista" "listy") ;
lin set_Fam = mkFam zbior_N ;

lin Eq_Adj2 = mkAdj2 (mkA "równy") (mkPrep "" R.Dat) ;
lin Eq_AdjE = mkAP (mkA "równy") ;
lin Lt_Adj2 = mkAdj2 (mkA "mniejszy") (mkPrep "niż" R.Nom) ;
lin Gt_Adj2 = mkAdj2 (mkA "większy") (mkPrep "niż" R.Nom) ;
lin Neq_Adj2 = mkAdj2 (mkA "różny") (mkPrep "od" R.Gen) ;
lin Neq_AdjC = mkAP (mkA "różny") ;
lin Leq_Adj2 = mkAdj2 (mkA "mniejszy") (mkPrep "lub równy" R.Dat) ;
lin Geq_Adj2 = mkAdj2 (mkA "większy") (mkPrep "lub równy" R.Dat) ;




lin converge_Verb = mkVerb (mkV "zbiegać") ;
lin divide_Verb2 = mkVerb2 (mkV "dzielić") (mkPrep "" R.Acc) ;

lin member_Noun2 = mkNoun2 element_N ;
lin divisor_Noun2 = mkNoun2 (mascN "dzielnik" "dzielnika") ;

lin plus_FunC = mkFunC suma_N ;
lin minus_Fun2 = mkFun2 roznica_N (mkPrep "między" R.Instr) ;
lin times_FunC = mkFunC iloczyn_N ;
lin div_Fun2 = mkFun2 (mascN "iloraz" "ilorazu") (mkPrep "między" R.Instr) ;
lin pow_Fun2 = mkFun2 (femN "potęga" "potęgi") possess_Prep (mkPrep "do" R.Gen) ;
lin neg_Fun = mkFun (femN "negacja" "negacji") ;
lin logarithm_Fun2 = mkFun2 (mascN "logarytm" "logarytmu") (mkPrep "o podstawie" R.Loc) possess_Prep ;
lin square_root_Fun = mkFun (postAdjN "kwadratowy" pierwiastek_N) ;

lin successor_Fun = mkFun (mascN "następnik" "następnika") ;
lin absolute_value_Fun = mkFun (postAdjN "bezwzględny" (oscN "wartość")) ;
lin factorial_Fun = mkFun (femN "silnia" "silni") ;
lin gcd_FunC = mkFunC (mkCN (mkA "największy") (mkCN (mkA "wspólny") (mascN "dzielnik" "dzielnika"))) ;



lin divisible_Adj2 = mkAdj2 (mkA "podzielny") (mkPrep "przez" R.Acc) ;


lin function_Fam2 = mkFam2 funkcja_N (mkPrep "z" R.Gen) (mkPrep "do" R.Gen) ;
lin union_FunC = mkFun suma_N ;
lin intersection_FunC = mkFun (neutEN "przecięcie") ;
lin difference_Fun2 = mkFun2 roznica_N (mkPrep "między" R.Instr) ;
lin complement_Fun = mkFun (neutEN "dopełnienie") ;
lin cartesian_FunC = mkFun (postAdjCN "kartezjański" (mkCN iloczyn_N)) ;
lin powerset_Fun = mkFun (postAdjN "potęgowy" zbior_N) ;

lin subset_Noun2 = mkNoun2 (postAdjN "właściwy" podzbior_N) ;
lin subseteq_Noun2 = mkNoun2 podzbior_N ;
lin superset_Noun2 = mkNoun2 (postAdjN "właściwy" nadzbior_N) ;
lin superseteq_Noun2 = mkNoun2 nadzbior_N ;
lin equalset_Adj2 = mkAdj2 (mkA "równy") (mkPrep "" R.Dat) ;
lin notequalset_Adj2 = mkAdj2 (mkA "różny") (mkPrep "od" R.Gen) ;
lin element_Noun2 = mkNoun2 element_N ;
lin notelement_Noun2 = mkNoun2 (mascN "nieelement" "nieelementu") ;

lin emptyset_Name = mkName (mkNP (postAdjN "pusty" zbior_N)) ;
lin universeset_Name = mkName (mkNP (postAdjN "uniwersalny" zbior_N)) ;

lin congruent_Adj3 = mkAdj3 (mkA "przystający") (mkPrep "do" R.Gen) (mkPrep "modulo" R.Nom) ;




lin combinationsFromSet_Fun2 = mkFun2 (mkCN (mascN "wybór" "wyboru") (SyntaxPol.mkAdv (mkPrep "spośród" R.Gen) (mkNP aPl_Det element_N))) (mkPrep "ze" R.Gen) (mkPrep "o rozmiarze" R.Loc) ;
lin combinations_Fun2 = mkFun2 (mkCN zbior_N (SyntaxPol.mkAdv possess_Prep (mkNP aPl_Det (femN "kombinacja" "kombinacji")))) (mkPrep "ze" R.Gen) (mkPrep "o rozmiarze" R.Loc) ;
lin binomial_Fun2 = mkFun2 (mkCN (mascN "symbol" "symbolu") (SyntaxPol.mkAdv possess_Prep (mkNP (UtilitiesPol.mkPN "Newtona")))) possess_Prep (mkPrep "po" R.Loc) ;

lin area_Fun = mkFun (neutEN "pole") ;
lin radius_Fun = mkFun (mascN "promień" "promienia") ;

lin pi_Name = mkName (mkNP (mkCN liczba_N (symb "\\(\\pi\\)"))) ;
lin legendre_symbol_Fun2 = mkFun2 (mkCN (mascN "symbol" "symbolu") (SyntaxPol.mkAdv possess_Prep (mkNP (UtilitiesPol.mkPN "Legendre'a")))) possess_Prep (mkPrep "nad" R.Instr) ;
lin square_Fun = mkFun (mascN "kwadrat" "kwadratu") ;
lin resultant_FunC = mkFunC (femN "rezultanta" "rezultanty") ;
lin perpendicular_Adj2 = mkAdj2 (mkA "prostopadły") (mkPrep "do" R.Gen) ;
lin perpendicular_AdjC = mkAdjC (mkA "prostopadły") ;
lin length_Fun = mkFun (oscN "długość") ;
lin norm_Fun = mkFun (femN "norma" "normy") ;

lin denumerable_Adj = mkAdj (mkA "przeliczalny") ;
lin cardinality_Fun = mkFun (femConsN "moc" "mocy") ;
lin root_Noun2 = mkNoun2 pierwiastek_N ;
lin degree_Fun = mkFun (mascN "stopień" "stopnia") ;

lin irrational_Adj = mkAdj (mkA "niewymierny") ;


lin sin_Fun = mkFun (mascN "sinus" "sinusa") ;
lin cos_Fun = mkFun (mascN "cosinus" "cosinusa") ;
lin tan_Fun = mkFun (mascN "tangens" "tangensa") ;
lin arcsin_Fun = mkFun (mascN "arkus sinus" "arkus sinusa") ;
lin arccos_Fun = mkFun (mascN "arkus cosinus" "arkus cosinusa") ;
lin arctan_Fun = mkFun (mascN "arkus tangens" "arkus tangensa") ;
lin orthogonal_Adj2 = mkAdj2 (mkA "ortogonalny") (mkPrep "do" R.Gen) ;
lin orthogonal_AdjC = mkAdjC (mkA "ortogonalny") ;
lin angle_between_Fun2 = mkFun2 (mascN "kąt" "kąta") (mkPrep "między" R.Instr) ;
lin dot_product_FunC = mkFunC (postAdjCN "skalarny" (mkCN iloczyn_N)) ;
lin vector_plus_FunC = mkFunC suma_N ;

lin everywhere_Adverb = ParadigmsPol.mkAdv "wszędzie" ;
lin almost_everywhere_Adverb = ParadigmsPol.mkAdv "prawie wszędzie" ;
lin uniformly_Adverb = ParadigmsPol.mkAdv "jednostajnie" ;
lin non_Adverb = ParadigmsPol.mkAdv "nie" ;

oper
  suma_N : N = femN "suma" "sumy" ;
  iloczyn_N : N = mascN "iloczyn" "iloczynu" ;
  roznica_N : N = femN "różnica" "różnicy" ;
  podzbior_N : N = mascN "podzbiór" "podzbioru" ;
  nadzbior_N : N = mascN "nadzbiór" "nadzbioru" ;
  pierwiastek_N : N = mascN "pierwiastek" "pierwiastka" ;

}
