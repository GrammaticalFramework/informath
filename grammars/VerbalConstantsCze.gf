concrete VerbalConstantsCze of VerbalConstants = CategoriesCze **

open
UtilitiesCze,
SyntaxCze,
ParadigmsCze,
SymbolicCze,
Prelude

in {

lin proposition_Noun = mkNoun (hradN "výrok") ;

lin digit_Noun = mkNoun (ruzeN "číslice") ;

lin boolean_Noun = mkNoun (jarniA "pravdivostní") (zenaN "hodnota") ;

lin natural_Noun = mkNoun (mladyA "přirozený") cislo_N ;

lin rational_Noun = mkNoun (jarniA "racionální") cislo_N ;
lin real_Noun = mkNoun (mladyA "reálný") cislo_N ;


lin list_Fam = mkFam (hradN "seznam") ;
lin set_Fam = mkFam mnozina_N ;

lin Eq_Adj2 = mkAdj2 (mladyA "rovný") (mkPrep "" dative) ;
lin Eq_AdjE = mkAP (mladyA "rovný") ;
lin Lt_Adj2 = mkAdj2 (jarniA "menší") (mkPrep "než" nominative) ;
lin Gt_Adj2 = mkAdj2 (jarniA "větší") (mkPrep "než" nominative) ;
lin Neq_Adj2 = mkAdj2 (mladyA "různý") (mkPrep "od" genitive) ;
lin Neq_AdjC = mkAP (mladyA "různý") ;
lin Leq_Adj2 = mkAdj2 (mladyA "menší nebo rovný") (mkPrep "než" nominative) ;
lin Geq_Adj2 = mkAdj2 (mladyA "větší nebo rovný") (mkPrep "než" nominative) ;




lin converge_Verb = mkVerb (mkV "konvergovat") ;
lin divide_Verb2 = mkVerb2 (mkV "dělit") (mkPrep "" accusative) ;

lin member_Noun2 = mkNoun2 element_N ;
lin divisor_Noun2 = mkNoun2 (muzN "dělitel") ;

lin plus_FunC = mkFunC soucet_N ;
lin minus_Fun2 = mkFun2 rozdil_N (mkPrep "mezi" instrumental) ;
lin times_FunC = mkFunC soucin_N ;
lin div_Fun2 = mkFun2 (hradN "podíl") (mkPrep "mezi" instrumental) ;
lin pow_Fun2 = mkFun2 (zenaN "mocnina") possess_Prep (mkPrep "na" accusative) ;
lin neg_Fun = mkFun (ruzeN "negace") ;
lin logarithm_Fun2 = mkFun2 (mkN "logaritmus" "logaritmu" mascInanimate) (mkPrep "o základu" locative) possess_Prep ;
lin square_root_Fun = mkFun (zenaN "odmocnina") ;

lin successor_Fun = mkFun (muzN "následník") ;
lin absolute_value_Fun = mkFun (mkCN (jarniA "absolutní") (zenaN "hodnota")) ;
lin factorial_Fun = mkFun (hradN "faktoriál") ;
lin gcd_FunC = mkFunC (mkCN (jarniA "největší") (mkCN (mladyA "společný") (muzN "dělitel"))) ;



lin divisible_Adj2 = mkAdj2 (mladyA "dělitelný") (mkPrep "" instrumental) ;


lin function_Fam2 = mkFam2 (ruzeN "funkce") from_Prep to_Prep ;
lin union_FunC = mkFun (staveniN "sjednocení") ;
lin intersection_FunC = mkFun (hradN "průnik") ;
lin difference_Fun2 = mkFun2 rozdil_N (mkPrep "mezi" instrumental) ;
lin complement_Fun = mkFun (mkN "doplněk" "doplňku" mascInanimate) ;
lin cartesian_FunC = mkFun (mkCN (mladyA "kartézský") soucin_N) ;
lin powerset_Fun = mkFun (mkCN (jarniA "potenční") mnozina_N) ;

lin subset_Noun2 = mkNoun2 (mkCN (jarniA "vlastní") (zenaN "podmnožina")) ;
lin subseteq_Noun2 = mkNoun2 (zenaN "podmnožina") ;
lin superset_Noun2 = mkNoun2 (mkCN (jarniA "vlastní") (zenaN "nadmnožina")) ;
lin superseteq_Noun2 = mkNoun2 (zenaN "nadmnožina") ;
lin equalset_Adj2 = mkAdj2 (mladyA "rovný") (mkPrep "" dative) ;
lin notequalset_Adj2 = mkAdj2 (mladyA "různý") from_Prep ;
lin element_Noun2 = mkNoun2 element_N ;
lin notelement_Noun2 = mkNoun2 (mkN "neprvek" "neprvku" mascInanimate) ;

lin emptyset_Name = mkName (mkNP (mkCN prazdny_A mnozina_N)) ;
lin universeset_Name = mkName (mkNP (mkCN (jarniA "univerzální") mnozina_N)) ;

lin congruent_Adj3 = mkAdj3 (jarniA "kongruentní") with_Prep (mkPrep "modulo" nominative) ;




lin combinationsFromSet_Fun2 = mkFun2 (mkCN (hradN "počet") (SyntaxCze.mkAdv possess_Prep (mkNP aPl_Det (ruzeN "kombinace")))) from_Prep (mkPrep "velikosti" genitive) ;
lin combinations_Fun2 = mkFun2 (mkCN mnozina_N (SyntaxCze.mkAdv possess_Prep (mkNP aPl_Det (ruzeN "kombinace")))) from_Prep (mkPrep "velikosti" genitive) ;
lin binomial_Fun2 = mkFun2 (mkCN (mladyA "binomický") (mkN "koeficient" "koeficientu" mascInanimate)) possess_Prep (mkPrep "nad" instrumental) ;

lin area_Fun = mkFun (hradN "obsah") ;
lin radius_Fun = mkFun (mkN "poloměr" "poloměru" mascInanimate) ;

lin pi_Name = mkName (mkNP (mkCN cislo_N (symb "\\(\\pi\\)"))) ;
lin legendre_symbol_Fun2 = mkFun2 (mkCN (invarA "Legendreův") (hradN "symbol")) possess_Prep (mkPrep "nad" instrumental) ;
lin square_Fun = mkFun (mkN "čtverec" "čtverce" mascInanimate) ;
lin resultant_FunC = mkFunC (hradN "rezultant") ;
lin perpendicular_Adj2 = mkAdj2 (mladyA "kolmý") (mkPrep "k" dative) ;
lin perpendicular_AdjC = mkAdjC (mladyA "kolmý") ;
lin length_Fun = mkFun (zenaN "délka") ;
lin norm_Fun = mkFun (zenaN "norma") ;

lin denumerable_Adj = mkAdj (mladyA "spočetný") ;
lin cardinality_Fun = mkFun (kostN "mohutnost") ;
lin root_Noun2 = mkNoun2 (mkN "kořen" "kořene" mascInanimate) ;
lin degree_Fun = mkFun (strojN "stupeň") ;

lin irrational_Adj = mkAdj (jarniA "iracionální") ;


lin sin_Fun = mkFun (hradN "sinus") ;
lin cos_Fun = mkFun (hradN "kosinus") ;
lin tan_Fun = mkFun (hradN "tangens") ;
lin arcsin_Fun = mkFun (hradN "arkussinus") ;
lin arccos_Fun = mkFun (hradN "arkuskosinus") ;
lin arctan_Fun = mkFun (hradN "arkustangens") ;
lin orthogonal_Adj2 = mkAdj2 (jarniA "ortogonální") (mkPrep "k" dative) ;
lin orthogonal_AdjC = mkAdjC (jarniA "ortogonální") ;
lin angle_between_Fun2 = mkFun2 (mkN "úhel" "úhlu" mascInanimate) (mkPrep "mezi" instrumental) ;
lin dot_product_FunC = mkFunC (mkCN (jarniA "skalární") soucin_N) ;
lin vector_plus_FunC = mkFunC soucet_N ;

lin everywhere_Adverb = mkAdv "všude" ;
lin almost_everywhere_Adverb = mkAdv "skoro všude" ;
lin uniformly_Adverb = mkAdv "stejnoměrně" ;
lin non_Adverb = mkAdv "ne" ;

oper
  soucet_N : N = mkN "součet" "součtu" mascInanimate ;
  soucin_N : N = hradN "součin" ;
  rozdil_N : N = mkN "rozdíl" "rozdílu" mascInanimate ;

}
