-- mathematical terms as they appear in "normal" mathematical text

abstract LatexTerms = {

cat
  Formula ;
  Equation ;
  Eqsign ;
  Exp ;
  [Exp] {1} ;
  Var ;
  Const ;
  Function ;

fun
  FEquation : Equation -> Formula ;
  FElem : [Exp] -> Exp -> Formula ;

  EChain : Eqsign -> Exp -> Equation -> Equation ;
  EBinary : Eqsign -> Exp -> Exp -> Equation ;

  EEq, ENeq, ELt, EGt, ELe, EGe, ESim : Eqsign ;

  TParenth : Exp -> Exp ; -- extra parentheses

  TPlus, TMinus, TTimes, TDiv, TExp : Exp -> Exp -> Exp ;
  TNeg : Exp -> Exp ;
  TApp : Function -> [Exp] -> Exp ;

  TVar : Var -> Exp ;
  TConst : Const -> Exp ;
---  TFloat : Float -> Exp ;
  TNumber : Float -> Exp ;

  FVar : Var -> Function ;
  FDerivative : Function -> Function ;

  N_Const, Z_Const, Q_Const, R_Const, C_Const : Const ;

  stringVar : String -> Var ;
  
  TFrac : Exp -> Exp -> Exp ;
  TAbsolute : Exp -> Exp ;
  TComprehension : Exp -> Exp -> Formula -> Exp ;
  TPositive : Exp -> Exp ; -- R^+
  TNegative : Exp -> Exp ;

  TextbfExp : Exp -> Exp ;
}
