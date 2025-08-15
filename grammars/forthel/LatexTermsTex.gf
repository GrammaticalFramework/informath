concrete LatexTermsTex of LatexTerms =
  open Formal, Prelude in {

lincat
  Formula = TermPrec ;
  Equation = {s : Str} ;
  Eqsign = Str ;
  Exp = TermPrecNum ;
  [Exp] = {s : Str} ;
  Var = Str ;
  Const = Str ;
  Function = Str ;

lin
  FEquation eq = constant eq.s ;
  FElem es e = constant (es.s ++ "\\in" ++ top e) ;

  EChain op x eq = {s = top x ++ op ++ eq.s} ;
  EBinary op x y = {s = top x ++ op ++ top y} ;

  EEq = "=" ; 
  ELt = "<" ; 
  EGt = ">" ; 
  ENeq = "\\neq" ; 
  ELe = "\\leq" ; 
  EGe = "\\geq" ; 
  ESim = "\\sim" ;

  TParenth t = constant (parenth (top t)) ** {isNumber = False} ;

  TPlus = tinfixl 1 "+" ;
  TMinus = tinfixl 1 "-" ;
  TTimes x y = case <x.isNumber, y.isNumber> of {
     <True, True> => infixl 2 "\\times" x y ** {isNumber = True} ;
     _ => tinfixl 2 "" x y
     } ;
  TDiv = tinfixl 2 "\\div" ;
  TExp a b = tinfixl 3 "^" a (b ** {s = curlyStr b.s}) ;

  TNeg x = prefix 2 "-" x ** {isNumber = x.isNumber} ;
  TApp f xs = constant (f ++ parenth xs.s) ** {isNumber = False} ;

  TVar x =  constant x ** {isNumber = False} ;
  TConst c =  constant c ** {isNumber = False} ;
---  TFloat n = constant n.s ** {isNumber = True} ;
  TNumber n = constant n.s ** {isNumber = True} ;

  BaseExp x = {s = top x} ;
  ConsExp x xs = {s = top x ++ "," ++ xs.s} ;

  N_Const = "N" ;
  Z_Const = "Z" ;
  Q_Const = "Q" ;
  R_Const = "R" ;
  C_Const = "C" ;

  stringVar s = s.s ;

  FVar v = v ;
  FDerivative f = f ++ "'" ;

  
  TPositive c = tinfixl 3 "^" c (tconstant (curlyStr "+")) ;
  TNegative c = tinfixl 3 "^" c (tconstant (curlyStr "-")) ;

  TFrac a b = tconstant (macroApp "frac" (top a) (top b)) ;
  
  TAbsolute a = tconstant ("|" ++ (top a) ++ "|") ;
  
  TComprehension a b f =
    tconstant ("\\{" ++ top a ++ "\\in" ++ top b ++
                ":" ++ top f ++ "\\}") ;

  TextbfExp e = e ** {s = macroApp "textbf" (top e)} ;

oper
  TermPrecNum = TermPrec ** {isNumber : Bool} ;

  tinfixl : Prec -> Str -> (_,_ : TermPrecNum) -> TermPrecNum = \p, op, x, y ->
    infixl p op x y ** {isNumber = False} ;
  tconstant : Str -> TermPrecNum = \s ->
    constant s ** {isNumber = False} ;

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