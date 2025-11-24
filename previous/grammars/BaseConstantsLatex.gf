resource BaseConstantsLatex = 

open
  Formal,
  Prelude,
  TermsLatex

in {

oper
  natural_Set = "N" ;
  integer_Set = "Z";
  rational_Set = "Q";
  real_Set = "R";
  complex_Set = "C";

  Eq_Compar = "=" ;
  Lt_Compar = "<" ;
  Gt_Compar = ">" ;
  Neq_Compar = "\\neq" ;
  Leq_Compar = "\\leq" ;
  Geq_Compar = "\\geq" ;
  perpendicular_Compar = "\\perp" ;
  
  plus_Oper : OperT = mkOper "+" <1 : Prec> <1 : Prec> <2 : Prec> ;
  minus_Oper : OperT = mkOper "-" <1 : Prec> <1 : Prec> <2 : Prec> ;
  times_Oper : OperT = mkOper "\\times"  <2 : Prec> <2 : Prec> <3 : Prec> ;
  div_Oper : OperT = mkOper "\\frac{" "} {" "}" <3 : Prec> <0 : Prec> <0 : Prec> ;
  pow_Oper : OperT = mkOper "" "^ {" "}" <3 : Prec> <4 : Prec> <2 : Prec> ;
  neg_Oper : OperT = mkOper "\\negated" ;
  logarithm_Oper : OperT = mkOper "\\log_" "{" "}" <3 : Prec> ;
  square_root_Oper : OperT = mkOper "\\sqrt{" "}" ;
  factorial_Oper : OperT = mkOper "" "" "!" <3 : Prec> ;
  absolute_value_Oper : OperT = mkOper "|" "|" ;
  length_Oper : OperT = mkOper "\\|" "\\|" ;

  function_Oper : OperT = mkOper "\\rightarrow" ; ---
  union_Oper : OperT = mkOper "\\cup" <2 : Prec> ;
  intersection_Oper : OperT = mkOper "\\cap" <2 : Prec> ;
  cartesian_Oper : OperT = mkOper "\\times" <3 : Prec> ;
  difference_Oper : OperT = mkOper "\\setminus" <2 : Prec> ;
  complement_Oper : OperT = mkOper "{" "" "}^{\\complement}" <3 : Prec> ;
  powerset_Oper : OperT = mkOper "\\wp" "" "" <3 : Prec> <4 : Prec> <4 : Prec> ;
  square_Oper : OperT = mkOper "" "" "^{ 2 }" <2 : Prec> ;
  legendre_symbol_Oper : OperT = mkOper "\\left(\\frac{" "}{" "}\\right)" <4 : Prec> ;

  subset_Comparnoun = "\\subset" ;  
  subseteq_Comparnoun = "\\subseteq" ;
  superset_Comparnoun = "\\supset" ;  
  superseteq_Comparnoun = "\\supseteq" ;  
  equalset_Compar = "=" ;
  notequalset_Compar = "\\neq" ;
  element_Comparnoun = "\\in" ;  
  notelement_Comparnoun = "\\notin" ;

  emptyset_Const = "\\emptyset" ;
  universeset_Const = "\\mathbb{ U }" ;

  positivePart : OperT = mkOper "" "" "^{+}" <3 : Prec> ;
  negativePart : OperT = mkOper "" "" "^{-}" <3 : Prec> ;

  binomial_Oper : OperT = mkOper "\\binom{" "}{" "}" <4 : Prec> ;
  combinations_Oper : OperT = mkOper "C^{" "}_{" "}" <4 : Prec> ;

  sin_Oper : OperT = prefixOper "\\sin" ;
  cos_Oper : OperT = prefixOper "\\cos" ;
  tan_Oper : OperT = prefixOper "\\tan" ;
  arcsin_Oper : OperT = prefixOper "\\arcsin" ;
  arccos_Oper : OperT = prefixOper "\\arccos" ;
  arctan_Oper : OperT = prefixOper "\\arctan" ;
  orthogonal_Compar : Str = "\\perp" ;

  dot_product_Oper : OperT = mkOper "\\cdot" <1 : Prec> <1 : Prec> <2 : Prec> ;
  vector_plus_Oper : OperT = mkOper "+" <1 : Prec> <1 : Prec> <2 : Prec> ;


oper
  OperT : Type = {
    begin, op, end : Str ; -- op = between args
    p : Prec ;  -- p = resulting
    ep1 : Prec ; -- ep1 = expected, first arg
    ep2 : Prec ; -- ep2 = expected, second arg
    } ;
  
  mkOper = overload {
    mkOper : Str -> OperT
      = \c -> {begin, end = "" ; op = c ; p = 0 ; ep1, ep2 = 1} ; -- lowest Prec
    mkOper : Str -> Str -> OperT
      = \b, e -> {begin = b ; end = e ; op = "" ; p = highest ; ep1, ep2 = 0} ; -- bracket, highest prec
    mkOper : Str -> Prec -> OperT
      = \c, p -> {begin, end = "" ; op = c ; p = p ; ep1, ep2 = nextPrec p} ; -- non-associative
    mkOper : Str -> Prec -> Prec -> Prec -> OperT
      = \c, p, p1, p2 -> {begin, end = "" ; op = c ; p = p ; ep1 = p1 ; ep2 = p2} ; -- associative
    mkOper : (beg, op, end : Str) -> Prec -> OperT
      = \beg, op, end , p -> {begin = beg ;  end = end ; op = op ; p = p ; ep1, ep2 = nextPrec p} ;
    mkOper : (beg, op, end : Str) -> Prec -> Prec -> Prec -> OperT -- worst case
      = \beg, op, end , p, p1, p2 ->
        {begin = beg ;  end = end ; op = op ; p = p ; ep1 = p1 ; ep2 = p2} ;
    } ;

  prefixOper : Str -> OperT = \op -> mkOper op "" "" <3 : Prec> <3 : Prec> <3 : Prec> ;

  appOper = overload {
    appOper : OperT -> TermPrecNum -> TermPrecNum = \op, trm -> {
      s = op.begin ++ op.op ++ usePrec op.ep1 trm ++ op.end ;
      p = op.p ;
      isNumber = False
      } ;
    appOper : OperT -> TermPrecNum -> TermPrecNum -> TermPrecNum = \op, x, y -> {
      s = op.begin ++ usePrec op.ep1 x ++ op.op ++ usePrec op.ep2 y ++ op.end ;
      p = op.p ;
      isNumber = False
      } ; 
    } ;

}