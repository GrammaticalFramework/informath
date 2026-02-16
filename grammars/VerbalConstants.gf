abstract VerbalConstants = Categories ** {



fun proposition_Noun : Noun ;


fun boolean_Noun : Noun ;

fun digit_Noun : Noun ;
fun natural_Noun : Noun ;

fun rational_Noun : Noun ;
fun real_Noun : Noun ;


fun list_Fam : Fam ;
fun set_Fam : Fam ;

fun Eq_Adj2 : Adj2 ;
fun Lt_Adj2 : Adj2 ;
fun Gt_Adj2 : Adj2 ;
fun Neq_Adj2 : Adj2 ;
fun Leq_Adj2 : Adj2 ;
fun Geq_Adj2 : Adj2 ;

fun Eq_AdjE : AdjE ;
fun Neq_AdjC : AdjC ;




fun converge_Verb : Verb ;
fun divide_Verb2 : Verb2 ;
fun member_Noun2 : Noun2 ;
fun divisor_Noun2 : Noun2 ;

fun plus_FunC : FunC ;
fun minus_Fun2 : Fun2 ;
fun times_FunC : FunC ;
fun div_Fun2 : Fun2 ;
fun pow_Fun2 : Fun2 ;
fun neg_Fun : Fun ;
fun logarithm_Fun2 : Fun2 ;
fun square_root_Fun : Fun ;

fun successor_Fun : Fun ;
fun absolute_value_Fun : Fun ;
fun factorial_Fun : Fun ;
fun gcd_FunC : FunC ;



fun divisible_Adj2 : Adj2 ;


fun function_Fam2 : Fam2 ;

fun union_FunC : FunC ;
fun intersection_FunC : FunC ;
fun cartesian_FunC : FunC ;
fun difference_Fun2 : Fun2 ;
fun complement_Fun : Fun ;
fun powerset_Fun : Fun ;

fun subset_Noun2 : Noun2 ;
fun subseteq_Noun2 : Noun2 ;
fun superset_Noun2 : Noun2 ;
fun superseteq_Noun2 : Noun2 ;
fun equalset_Adj2 : Adj2 ;
fun notequalset_Adj2 : Adj2 ;
fun element_Noun2 : Noun2 ;
fun notelement_Noun2 : Noun2 ;

fun emptyset_Name : Name ;
fun universeset_Name : Name ;

fun congruent_Adj3 : Adj3 ;





fun irrational_Adj : Adj ; -- top100

fun degree_Fun : Fun ; -- top100
fun root_Noun2 : Noun2 ; -- top100
fun cardinality_Fun : Fun ; -- top100
fun denumerable_Adj : Adj ; -- top100

fun length_Fun : Fun ; -- top100
fun norm_Fun : Fun ; -- top100
fun perpendicular_Adj2 : Adj2 ; -- top100
fun resultant_FunC : FunC ; -- top100
fun square_Fun : Fun ; -- top100
fun legendre_symbol_Fun2 : Fun2 ; -- top100
fun pi_Name : Name ; -- top100

fun radius_Fun : Fun ; -- top100
fun area_Fun : Fun ; -- top100
fun binomial_Fun2 : Fun2 ; -- top100
fun combinations_Fun2 : Fun2 ; -- top100
fun combinationsFromSet_Fun2 : Fun2 ; -- top100
fun sin_Fun : Fun ;
fun cos_Fun : Fun ;
fun tan_Fun : Fun ;
fun arcsin_Fun : Fun ;
fun arccos_Fun : Fun ;
fun arctan_Fun : Fun ;
fun orthogonal_Adj2 : Adj2 ;
fun orthogonal_AdjC : AdjC ;
fun perpendicular_Adj2 : Adj2 ;
fun perpendicular_AdjC : AdjC ;
fun angle_between_Fun2 : Fun2 ;
fun dot_product_FunC : FunC ;
fun vector_plus_FunC : FunC ;



-- special constants
fun SigmaExp : Exp -> Exp -> Ident -> Exp -> Exp ;
fun SeriesExp : Exp -> Ident -> Exp -> Exp ;
fun IntegralExp : Exp -> Exp -> Ident -> Exp -> Exp ;

fun sameParityProp : Exp -> Exp -> Prop ;

}
