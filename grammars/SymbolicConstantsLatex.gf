concrete SymbolicConstantsLatex of SymbolicConstants = TermsLatex **

open
  Formal,
  Prelude

in {

lin
  natural_Term = tconstant "N" ;
  integer_Term = tconstant "Z";
  rational_Term = tconstant "Q";
  real_Term = tconstant "R";
  complex_Term = tconstant "C";

  Eq_Eqsign = "=" ;
  Lt_Eqsign = "<" ;
  Gt_Eqsign = ">" ;
  Neq_Eqsign = "\\neq" ;
  Leq_Eqsign = "\\leq" ;
  Geq_Eqsign = "\\geq" ;
  perpendicular_Eqsign = "\\perp" ;
  
  plus_Term = mkTerm "+" <1 : Prec> <1 : Prec> <2 : Prec> ;
  minus_Term = mkTerm "-" <1 : Prec> <1 : Prec> <2 : Prec> ;
  times_Term = mkTerm "\\times"  <2 : Prec> <2 : Prec> <3 : Prec> ;
  div_Term = mkTerm "\\frac{" "} {" "}" <3 : Prec> <0 : Prec> <0 : Prec> ;
  pow_Term = mkTerm "" "^ {" "}" <3 : Prec> <4 : Prec> <2 : Prec> ;
  neg_Term = mkTerm "\\negated" ;
  logarithm_Term = mkTerm "\\log_" "{" "}" <3 : Prec> ;
  square_root_Term = mkTerm "\\sqrt{" "}" ;
  factorial_Term = mkTerm "" "" "!" <3 : Prec> ;
  absolute_value_Term = mkTerm "|" "|" ;
  length_Term = mkTerm "\\|" "\\|" ;
  cardinality_Term = mkTerm "|" "|" ;

  function_Term = mkTerm "\\rightarrow" ; ---
  union_Term = mkTerm "\\cup" <2 : Prec> ;
  intersection_Term = mkTerm "\\cap" <2 : Prec> ;
  cartesian_Term = mkTerm "\\times" <3 : Prec> ;
  difference_Term = mkTerm "\\setminus" <2 : Prec> ;
  complement_Term = mkTerm "{" "" "}^{\\complement}" <3 : Prec> ;
  powerset_Term = mkTerm "\\wp" "" "" <3 : Prec> <4 : Prec> <4 : Prec> ;
  square_Term = mkTerm "" "" "^{ 2 }" <2 : Prec> ;
  legendre_symbol_Term = mkTerm "\\left(\\frac{" "}{" "}\\right)" <4 : Prec> ;

  subset_Eqsign = "\\subset" ;  
  subseteq_Eqsign = "\\subseteq" ;
  superset_Eqsign = "\\supset" ;  
  superseteq_Eqsign = "\\supseteq" ;  
  equalset_Eqsign = "=" ;
  notequalset_Eqsign = "\\neq" ;
  element_Eqsign = "\\in" ;  
  notelement_Eqsign = "\\notin" ;

  emptyset_Term = tconstant "\\emptyset" ;
  universeset_Term = tconstant "\\mathbb{ U }" ;
  pi_Term = tconstant "\\pi" ;

---  positivePart = mkOper "" "" "^{+}" <3 : Prec> ;
---  negativePart = mkOper "" "" "^{-}" <3 : Prec> ;

  binomial_Term = mkTerm "\\binom{" "}{" "}" <4 : Prec> ;
  combinations_Term = mkTerm "C^{" "}_{" "}" <4 : Prec> ;

  sin_Term = mkTerm  "\\sin" ;
  cos_Term = mkTerm  "\\cos" ;
  tan_Term = mkTerm  "\\tan" ;
  arcsin_Term = mkTerm  "\\arcsin" ;
  arccos_Term = mkTerm  "\\arccos" ;
  arctan_Term = mkTerm  "\\arctan" ;
  orthogonal_Eqsign = "\\perp" ;

  dot_product_Term = mkTerm "\\cdot" <1 : Prec> <1 : Prec> <2 : Prec> ;
  vector_plus_Term = mkTerm "+" <1 : Prec> <1 : Prec> <2 : Prec> ;


oper

  OperT : Type = TermPrecNum -> TermPrecNum -> TermPrecNum ;

  mkTerm = overload {
    mkTerm : (op : Str) -> TermPrecNum -> TermPrecNum
      = tprefix 2 ;
    mkTerm : (op : Str) -> OperT
      = \op, x, y -> tinfixl 0 op x y ;
    mkTerm : (beg, end : Str) -> TermPrecNum -> TermPrecNum
      = \b, e, x -> {
        s = b ++ top x ++ e ;
	p = highest ;
	isNumber = False
	} ;
    mkTerm : (op : Str) -> Prec -> OperT 
      = \op, p -> tinfixl p op ;
    mkTerm : (op : Str) -> Prec -> Prec -> Prec -> OperT
      = \op, p, p1, p2, x, y  -> {
        s = usePrec p1 x ++ op ++ usePrec p2 y ;
	p = p ;
	isNumber = False
	} ;
    mkTerm : (beg, op, end : Str) -> Prec -> OperT
      = \beg, op, end, p, x, y  -> {
        s = beg ++ usePrec (nextPrec p) x ++ op ++ usePrec (nextPrec p) y ++ end ;
	p = p ;
	isNumber = False
	} ;
    mkOper : (beg, op, end : Str) -> Prec -> Prec -> Prec -> OperT -- worst case
      = \beg, op, end , p, p1, p2, x, y -> {
        s = beg ++ usePrec p1 x ++ op ++ usePrec p2 y ++ end ;
	p = p ;
	isNumber = False
	} ; 
    } ;

}