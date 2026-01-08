concrete SymbolicConstantsLatex of SymbolicConstants = TermsLatex **

open
  Formal,
  Prelude

in {

lin
  natural_Const = "N" ;
  integer_Const = "Z";
  rational_Const = "Q";
  real_Const = "R";
  complex_Const = "C";

  Eq_Compar = "=" ;
  Lt_Compar = "<" ;
  Gt_Compar = ">" ;
  Neq_Compar = "\\neq" ;
  Leq_Compar = "\\leq" ;
  Geq_Compar = "\\geq" ;
  
  plus_Oper2 = mkOper2 "+" <1 : Prec> <1 : Prec> <2 : Prec> ;
  minus_Oper2 = mkOper2 "-" <1 : Prec> <1 : Prec> <2 : Prec> ;
  times_Oper2 = mkOper2 "\\times"  <2 : Prec> <2 : Prec> <3 : Prec> ;
  div_Oper2 = mkOper2 "\\frac{" "} {" "}" <3 : Prec> <0 : Prec> <0 : Prec> ;
  pow_Oper2 = mkOper2 "" "^ {" "}" <3 : Prec> <4 : Prec> <2 : Prec> ;
  neg_Oper = prefixOper "-" <1 : Prec> <2 : Prec> ;
  logarithm_Oper2 = mkOper2 "\\log_{" "}" "" <3 : Prec> <1 : Prec> <3 : Prec> ;
  square_root_Oper = mkOper "\\sqrt{" "}" ;

  absolute_value_Oper = mkOper "|" "|" ;
  factorial_Oper = postfixOper "!" <3 : Prec> <4 : Prec> ;
  
  function_Oper2 = mkOper2 "\\rightarrow" ; ---
  union_Oper2 = mkOper2 "\\cup" <2 : Prec> ;
  intersection_Oper2 = mkOper2 "\\cap" <2 : Prec> ;
  cartesian_Oper2 = mkOper2 "\\times" <3 : Prec> ;
  difference_Oper2 = mkOper2 "\\setminus" <2 : Prec> ;
  complement_Oper = mkOper "{" "}^{\\complement}" <3 : Prec> ;
  powerset_Oper = prefixOper "\\wp" <3 : Prec> <4 : Prec> ;

  positive_part_Oper = mkOper "" "^{+}" <3 : Prec> ;
  negative_part_Oper = mkOper "" "^{-}" <3 : Prec> ;

  subset_Compar = "\\subset" ;  
  subseteq_Compar = "\\subseteq" ;
  superset_Compar = "\\supset" ;  
  superseteq_Compar = "\\supseteq" ;  
  element_Compar = "\\in" ;  
  notelement_Compar = "\\notin" ;

  emptyset_Const = "\\emptyset" ;
  universeset_Const = "\\mathbb{ U }" ;

  cardinality_Oper = mkOper "|" "|" ;
  length_Oper = mkOper "\\|" "\\|" ;
  perpendicular_Compar = "\\perp" ;
  square_Oper = mkOper "" "^{ 2 }" <2 : Prec> ;
  legendre_symbol_Oper2 = mkOper2 "\\left(\\frac{" "}{" "}\\right)" <4 : Prec> ;
  pi_Const = "\\pi" ;
  binomial_Oper2 = mkOper2 "\\binom{" "}{" "}" <4 : Prec> ;
  combinations_Oper2 = mkOper2 "C^{" "}_{" "}" <4 : Prec> ;

  sin_Oper = mkOper  "\\sin" ;
  cos_Oper = mkOper  "\\cos" ;
  tan_Oper = mkOper  "\\tan" ;
  arcsin_Oper = mkOper  "\\arcsin" ;
  arccos_Oper = mkOper  "\\arccos" ;
  arctan_Oper = mkOper  "\\arctan" ;
  orthogonal_Compar = "\\perp" ;

  dot_product_Oper2 = mkOper2 "\\cdot" <1 : Prec> <1 : Prec> <2 : Prec> ;



-- special syntax

  times_Term x y = case <x.isNumber, y.isNumber> of {
     <True, True> => infixl 2 "\\times" x y ** {isNumber = True} ;
     _ => tinfixl 2 "" x y
     } ;

  modulo_Formula a b m =
    constant (top a ++ "\\equiv" ++ top b ++ "\\pmod{" ++ top m ++ "}") ;

  sigma_Term m n i t = {
    s = "\\sum_{" ++ i ++ "=" ++ top m ++ "}^{" ++ top n ++ "}{" ++ usePrec 2 t ++ "}" ;
    p = 1 ;
    isNumber = False
    } ;
    
  sum3dots_Term a b c = {
    s = usePrec 1 a ++ "+" ++ usePrec 1 b ++ "+ \\cdots +" ++ usePrec 1 c ;
    p = 0 ;
    isNumber = False
    } ;
    
  series_Term m i t = {
    s = "\\sum_{" ++ i ++ "=" ++ top m ++ "}^{" ++ "\\infty" ++ "}{" ++ usePrec 2 t ++ "}" ;
    p = 1 ;
    isNumber = False
    } ;

  integral_Term a b x t = {
    s = "\\int_{" ++ top a ++ "}^{" ++ top b ++ "}" ++ usePrec 2 t ++ "\\, d" ++ x ;
    p = 1 ;
    isNumber = False
    } ;
    


}