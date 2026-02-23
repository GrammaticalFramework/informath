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
  Terms ; -- argument list for macros
  Macro ; -- the macro itself

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

  MacroFormula : Macro -> Formula ;
  MacroTerm : Macro -> Term ;
  
  App1MacroFormula : Macro -> Term -> Formula ;
  App1MacroTerm : Macro -> Term -> Term ;
  
  App2MacroFormula : Macro -> Term -> Term -> Formula ;
  App2MacroTerm : Macro -> Term -> Term -> Term ;
  
  App3MacroFormula : Macro -> Term -> Term -> Term -> Formula ;
  App3MacroTerm : Macro -> Term -> Term -> Term -> Term ;
  
  StringMacro : String -> Macro ;
  
}
