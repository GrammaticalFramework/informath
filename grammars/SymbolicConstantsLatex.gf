concrete SymbolicConstantsLatex of SymbolicConstants = 

open
  Formal,
  Prelude,
  TermsLatex

in {

lincat
  Set = Str ;
  Const = Str ;
  Oper = OperT ;
  Compar = Str ;

lin
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
  
  plus_Oper = mkOper "+" <1 : Prec> <1 : Prec> <2 : Prec> ;
  minus_Oper = mkOper "-" <1 : Prec> <1 : Prec> <2 : Prec> ;
  times_Oper = mkOper "\\times"  <2 : Prec> <2 : Prec> <3 : Prec> ;
  div_Oper = mkOper "\\frac{" "} {" "}" <3 : Prec> <0 : Prec> <0 : Prec> ;
  pow_Oper = mkOper "" "^ {" "}" <3 : Prec> <4 : Prec> <2 : Prec> ;
  neg_Oper = mkOper "\\negated" ;
  logarithm_Oper = mkOper "\\log_" "{" "}" <3 : Prec> ;
  square_root_Oper = mkOper "\\sqrt{" "}" ;
  factorial_Oper = mkOper "" "" "!" <3 : Prec> ;
  absolute_value_Oper = mkOper "|" "|" ;
  length_Oper = mkOper "\\|" "\\|" ;
  cardinality_Oper = mkOper "|" "|" ;

  function_Oper = mkOper "\\rightarrow" ; ---
  union_Oper = mkOper "\\cup" <2 : Prec> ;
  intersection_Oper = mkOper "\\cap" <2 : Prec> ;
  cartesian_Oper = mkOper "\\times" <3 : Prec> ;
  difference_Oper = mkOper "\\setminus" <2 : Prec> ;
  complement_Oper = mkOper "{" "" "}^{\\complement}" <3 : Prec> ;
  powerset_Oper = mkOper "\\wp" "" "" <3 : Prec> <4 : Prec> <4 : Prec> ;
  square_Oper = mkOper "" "" "^{ 2 }" <2 : Prec> ;
  legendre_symbol_Oper = mkOper "\\left(\\frac{" "}{" "}\\right)" <4 : Prec> ;

  subset_Compar = "\\subset" ;  
  subseteq_Compar = "\\subseteq" ;
  superset_Compar = "\\supset" ;  
  superseteq_Compar = "\\supseteq" ;  
  equalset_Compar = "=" ;
  notequalset_Compar = "\\neq" ;
  element_Compar = "\\in" ;  
  notelement_Compar = "\\notin" ;

  emptyset_Const = "\\emptyset" ;
  universeset_Const = "\\mathbb{ U }" ;
  pi_Const = "\\pi" ;

---  positivePart = mkOper "" "" "^{+}" <3 : Prec> ;
---  negativePart = mkOper "" "" "^{-}" <3 : Prec> ;

  binomial_Oper = mkOper "\\binom{" "}{" "}" <4 : Prec> ;
  combinations_Oper = mkOper "C^{" "}_{" "}" <4 : Prec> ;

  sin_Oper = prefixOper "\\sin" ;
  cos_Oper = prefixOper "\\cos" ;
  tan_Oper = prefixOper "\\tan" ;
  arcsin_Oper = prefixOper "\\arcsin" ;
  arccos_Oper = prefixOper "\\arccos" ;
  arctan_Oper = prefixOper "\\arctan" ;
  orthogonal_Compar = "\\perp" ;

  dot_product_Oper = mkOper "\\cdot" <1 : Prec> <1 : Prec> <2 : Prec> ;
  vector_plus_Oper = mkOper "+" <1 : Prec> <1 : Prec> <2 : Prec> ;


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