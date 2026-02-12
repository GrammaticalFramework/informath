-- mathematical terms as they appear in "normal" mathematical text

abstract Terms = Identifiers ** {

cat
  Formula ;
  Declaration ;
  Equation ;
  Compar ;
  [Term] {1} ;
  Function ;
  Const ;
  Oper ;
  Oper2 ;
  Terms ; -- arguments of macros

fun
  EquationFormula : Equation -> Formula ;
  ElemFormula : [Term] -> Term -> Formula ;
  
  ElemDeclaration : [Term] -> Term -> Declaration ;
  FunctionDeclaration : Ident -> Term -> Term -> Declaration ;

  ChainEquation : Compar -> Term -> Equation -> Equation ;
  BinaryEquation : Compar -> Term -> Term -> Equation ;

  ParenthTerm : Term -> Term ; -- extra parentheses

  AppFunctionTerm : Function -> [Term] -> Term ;

  EnumSetTerm : [Term] -> Term ;
  ComprehensionTerm : Term -> Term -> Formula -> Term ;
  
  IdentFunction : Ident -> Function ;
  DerivativeFunction : Function -> Function ;
  
  TextbfTerm : Term -> Term ;

  ConstTerm : Const -> Term ;
  OperTerm : Oper -> Term -> Term ;
  Oper2Term : Oper2 -> Term -> Term -> Term ;

  MacroFormula : Ident -> Terms -> Formula ;
  MacroTerm : Ident -> Terms -> Term ;
  OneTerms : Term -> Terms ;
  AddTerms : Term -> Terms -> Terms ;
  
}
