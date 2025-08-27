concrete TermsLatex of Terms =
  open Formal, Prelude in {

lincat
  Formula = TermPrec ;
  Declaration = {s : Str ; isPl : Bool} ;
  Equation = {s : Str} ;
  Eqsign = Str ;
  Term = TermPrecNum ;
  [Term] = {s : Str ; isPl : Bool} ;
  Ident = Str ;
  Function = Str ;

lin
  FEquation eq = constant eq.s ;
  FElem es e = constant (es.s ++ "\\in" ++ top e) ;
  FModulo a b m = constant (top a ++ "\\equiv" ++ top b ++ "\\pmod{" ++ top m ++ "}") ;
  
  DElem es e = {s = es.s ++ "\\in" ++ top e ; isPl = es.isPl} ;
  DFunction f a b = {s = f ++ ":" ++ top a ++ "\\rightarrow" ++ top b ; isPl = False} ;

  EChain op x eq = {s = top x ++ op ++ eq.s} ;
  EBinary op x y = {s = top x ++ op ++ top y} ;

  TParenth t = constant (parenth (top t)) ** {isNumber = False} ;

  TTimes x y = case <x.isNumber, y.isNumber> of {
     <True, True> => infixl 2 "\\times" x y ** {isNumber = True} ;
     _ => tinfixl 2 "" x y
     } ;
  TNeg x = prefix 2 "-" x ** {isNumber = x.isNumber} ;
  TApp f xs = constant (f ++ parenth xs.s) ** {isNumber = False} ;

  TSigma i m n t = {
    s = "\\sum_{" ++ i ++ "=" ++ top m ++ "}^{" ++ top n ++ "}{" ++ usePrec 2 t ++ "}" ;
    p = 1 ;
    isNumber = False
    } ;
    
  TSum3dots a b c = {
    s = usePrec 1 a ++ "+" ++ usePrec 1 b ++ "+ \\cdots +" ++ usePrec 1 c ;
    p = 0 ;
    isNumber = False
    } ;
    
  TSeries i m t = {
    s = "\\sum_{" ++ i ++ "=" ++ top m ++ "}^{" ++ "\\infty" ++ "}{" ++ usePrec 2 t ++ "}" ;
    p = 1 ;
    isNumber = False
    } ;

  TIntegral x a b t = {
    s = "\\int_{" ++ top a ++ "}^{" ++ top b ++ "}" ++ usePrec 2 t ++ "\\, d" ++ x ;
    p = 1 ;
    isNumber = False
    } ;
    


  TEnumSet ts = constant ("\\{" ++ ts.s ++ "\\}") ** {isNumber = False} ;

  TIdent x =  constant x ** {isNumber = False} ;
  TNumber n = constant n.s ** {isNumber = True} ;

  BaseTerm x = {s = top x ; isPl = False} ;
  ConsTerm x xs = {s = top x ++ "," ++ xs.s ; isPl = True} ;

  StrIdent s = s.s ;

  FIdent v = v ;
  FDerivative f = f ++ "'" ;

  
  TPositive c = tinfixl 3 "^" c (tconstant (curlyStr "+")) ;
  TNegative c = tinfixl 3 "^" c (tconstant (curlyStr "-")) ;

  TFrac a b = tconstant (macroApp "frac" (top a) (top b)) ;
    
  TComprehension a b f =
    tconstant ("\\{" ++ top a ++ "\\in" ++ top b ++
                ":" ++ top f ++ "\\}") ;

  TLog base arg =
    mkPrec 3 ("\\log_" ++ top base ++ usePrec 3 arg) ** {isNumber = arg.isNumber} ;
    ---- isNumber only on right: 2 log_e 5 vs. log_e 5 x 2

  TextbfTerm e = e ** {s = macroApp "textbf" (top e)} ;

oper
  TermPrecNum = TermPrec ** {isNumber : Bool} ;

  tinfixl : Prec -> Str -> (_,_ : TermPrecNum) -> TermPrecNum = \p, op, x, y ->
    infixl p op x y ** {isNumber = False} ;
  tconstant : Str -> TermPrecNum = \s ->
    constant s ** {isNumber = False} ;

  tbracket : Str -> Str -> TermPrec -> TermPrec = \begin, end, t ->
    tconstant (begin ++ top t ++ end) ;

  -- to be usable at runtime, therefore ++
  mathEnvStr : Str -> Str = \s -> "$" ++ s ++ "$" ;
  curlyStr : Str -> Str = \s -> "{" ++ s ++ "}" ;

  macroApp = overload {
    macroApp : (f : Str) -> Str = \f -> "\\" + f ;
    macroApp : (f, x : Str) -> Str = \f, x -> "\\" + f ++ "{" ++ x ++ "}" ;
    macroApp : (f, x, y : Str) -> Str = \f, x, y ->
      "\\" + f ++ "{" ++ x ++ "} {" ++ y ++ "}" ;
   } ;

}