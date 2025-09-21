-- mathematical terms as they appear in "normal" mathematical text

abstract Terms = Identifiers ** {

cat
  Formula ;
  Declaration ;
  Equation ;
  Compar ;
  Term ;
  [Term] {1} ;
  Function ;

fun
  FEquation : Equation -> Formula ;
  FElem : [Term] -> Term -> Formula ;
  FModulo : Term -> Term -> Term -> Formula ;
  
  DElem : [Term] -> Term -> Declaration ;
  DFunction : Ident -> Term -> Term -> Declaration ;

  EChain : Compar -> Term -> Equation -> Equation ;
  EBinary : Compar -> Term -> Term -> Equation ;

  TParenth : Term -> Term ; -- extra parentheses

  TTimes : Term -> Term -> Term ;
  TNeg : Term -> Term ;
  TApp : Function -> [Term] -> Term ;

  TSigma : Ident -> Term -> Term -> Term -> Term ;
  TSum3dots : Term -> Term -> Term -> Term ;

  TSeries : Ident -> Term -> Term -> Term ;
  TIntegral : Ident -> Term -> Term -> Term -> Term ;

  TEnumSet : [Term] -> Term ;

  TIdent : Ident -> Term ;
  TNumber : Int -> Term ; --- was float

  FIdent : Ident -> Function ;
  FDerivative : Function -> Function ;
  
  TFrac : Term -> Term -> Term ;
  TComprehension : Term -> Term -> Formula -> Term ;
  TPositive : Term -> Term ; -- R^+
  TNegative : Term -> Term ;

  TLog : Term -> Term -> Term ;
  
  TextbfTerm : Term -> Term ;
}
