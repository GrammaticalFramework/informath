concrete TermsLatex of Terms = IdentifiersLatex **
  open Formal, Prelude in {

lincat
  Formula = TermPrec ;
  Declaration = {s : Str ; isPl : Bool} ;
  Equation = {s : Str} ;
  Compar = Str ;
  [Term] = {s : Str ; isPl : Bool} ;
  Function = Str ;
  Const = Str ;
  Oper = OperT ;
  Oper2 = Oper2T ;
  Terms = Str ;

lin
  EquationFormula eq = constant eq.s ;
  ElemFormula es e = constant (es.s ++ "\\in" ++ top e) ;
  
  ElemDeclaration es e = {s = es.s ++ "\\in" ++ top e ; isPl = es.isPl} ;
  FunctionDeclaration f a b = {s = f ++ ":" ++ top a ++ "\\rightarrow" ++ top b ; isPl = False} ;

  ChainEquation op x eq = {s = top x ++ op ++ eq.s} ;
  BinaryEquation op x y = {s = top x ++ op ++ top y} ;

  ParenthTerm t = constant (parenth (top t)) ** {isNumber = False} ;

  AppFunctionTerm f xs = constant (f ++ parenth xs.s) ** {isNumber = False} ;

  EnumSetTerm ts = constant ("\\{" ++ ts.s ++ "\\}") ** {isNumber = False} ;
  
  ComprehensionTerm a b f =
    tconstant ("\\{" ++ top a ++ "\\in" ++ top b ++
                ":" ++ top f ++ "\\}") ;

  BaseTerm x = {s = top x ; isPl = False} ;
  ConsTerm x xs = {s = top x ++ "," ++ xs.s ; isPl = True} ;

  IdentFunction v = v ;
  DerivativeFunction f = f ++ "'" ;

  TextbfTerm e = e ** {s = macroApp "\\textbf" (top e)} ;

  ConstTerm const = constant const ** {isNumber = False} ;

  OperTerm op trm = {
    s = op.begin ++ usePrec op.ep1 trm ++ op.end ;
    p = op.p ;
    isNumber = False
    } ;
      
  Oper2Term op x y = {
    s = op.begin ++ usePrec op.ep1 x ++ op.op ++ usePrec op.ep2 y ++ op.end ;
    p = op.p ;
    isNumber = False
    } ; 

  MacroTerm ident terms =
    constant (ident ++ terms) ** {isNumber = False} ; --- False

  OneTerms term = curly (top term) ;
  AddTerms term terms = curly (top term) ++ terms ;

oper

  tinfixl : Prec -> Str -> (_,_ : TermPrecNum) -> TermPrecNum = \p, op, x, y ->
    infixl p op x y ** {isNumber = False} ;
  tprefix : Prec -> Str -> TermPrecNum -> TermPrecNum = \p, op, x ->
    prefix p op x ** {isNumber = False} ;
---  tinfix : Prec -> Str -> (_,_ : TermPrecNum) -> TermPrecNum = \p, op, x, y ->
   --- infix p op x y ** {isNumber = False} ;
  tconstant : Str -> TermPrecNum = \s ->
    constant s ** {isNumber = False} ;

  tbracket : Str -> Str -> TermPrec -> TermPrec = \begin, end, t ->
    tconstant (begin ++ top t ++ end) ;

  -- to be usable at runtime, therefore ++
  mathEnvStr : Str -> Str = \s -> "$" ++ s ++ "$" ;
  curlyStr : Str -> Str = \s -> "{" ++ s ++ "}" ;

  macroApp = overload {
    macroApp : (f : Str) -> Str = \f -> f ;
    macroApp : (f, x : Str) -> Str = \f, x -> f ++ "{" ++ x ++ "}" ;
    macroApp : (f, x, y : Str) -> Str = \f, x, y ->
      f ++ "{" ++ x ++ "} {" ++ y ++ "}" ;
    macroApp : (f, x, y, z : Str) -> Str = \f, x, y, z ->
      f ++ "{" ++ x ++ "} {" ++ y ++ "} {" ++ z ++ "}" ;
    macroApp : (f, x, y, z, u : Str) -> Str = \f, x, y, z, u ->
      f ++ "{" ++ x ++ "} {" ++ y ++ "} {" ++ z ++ "} {" ++ u ++ "}" ;
   } ;

  OperT : Type = {
    begin, end : Str ;
    p : Prec ;  -- p = resulting
    ep1 : Prec ; -- ep1 = expected of arg
    } ;
    
  Oper2T : Type = {
    begin, op, end : Str ; -- op = between args
    p : Prec ;  -- p = resulting
    ep1 : Prec ; -- ep1 = expected, first arg
    ep2 : Prec ; -- ep2 = expected, second arg
    } ;
  
  mkOper = overload {
    mkOper : Str -> OperT  -- prefix, lowest Prec
      = \c -> {begin = c ; end = "" ; p = 0 ; ep1 = 1} ;
    mkOper : Str -> Str -> OperT   -- bracket, highest prec
      = \b, e -> {begin = b ; end = e ; p = highest ; ep1 = 0} ;
    mkOper : Str -> Prec -> OperT -- prefix, argument one higher than result
      = \c, p -> {begin = c ; end = "" ; p = p ; ep1 = nextPrec p} ;
    mkOper : (beg, end : Str) -> Prec -> OperT
      = \beg, end , p -> {begin = beg ;  end = end ; p = p ; ep1 = nextPrec p} ;
    mkOper : (beg, end : Str) -> Prec -> Prec -> OperT -- worst case
      = \beg, end, p, p1 ->
        {begin = beg ;  end = end ; p = p ; ep1 = p1} ;
    } ;
    
  prefixOper : Str -> Prec -> Prec -> OperT
    = \op, p, ep -> mkOper op "" p ep ;
  postfixOper : Str -> Prec -> Prec -> OperT
    = \op, p, ep -> mkOper "" op p ep ;
  macroOper : Str -> Prec -> Prec -> OperT
    = \op, p, ep -> mkOper (op + "{") "}" p ep ; 

  mkOper2 = overload {
    mkOper2 : Str -> Oper2T
      = \c -> {begin, end = "" ; op = c ; p = 0 ; ep1, ep2 = 1} ; -- lowest Prec
    mkOper2 : Str -> Str -> Oper2T
      = \b, e -> {begin = b ; end = e ; op = "" ; p = highest ; ep1, ep2 = 0} ; -- bracket, highest prec
    mkOper2 : Str -> Prec -> Oper2T
      = \c, p -> {begin, end = "" ; op = c ; p = p ; ep1, ep2 = nextPrec p} ; -- non-associative
    mkOper2 : Str -> Prec -> Prec -> Prec -> Oper2T
      = \c, p, p1, p2 -> {begin, end = "" ; op = c ; p = p ; ep1 = p1 ; ep2 = p2} ; -- associative
    mkOper2 : (beg, op, end : Str) -> Prec -> Oper2T
      = \beg, op, end , p -> {begin = beg ;  end = end ; op = op ; p = p ; ep1, ep2 = nextPrec p} ;
    mkOper2 : (beg, op, end : Str) -> Prec -> Prec -> Prec -> Oper2T -- worst case
      = \beg, op, end , p, p1, p2 ->
        {begin = beg ;  end = end ; op = op ; p = p ; ep1 = p1 ; ep2 = p2} ;
    } ;

  macroOper2 : Str -> Prec -> Prec -> Prec -> Oper2T
    = \op, p, ep1, ep2 -> mkOper2 (op ++ "{") "} {" "}" p ep1 ep2  ; 

  curly : Str -> Str = \s -> "{" ++ s ++ "}" ;
  
}