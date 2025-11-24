concrete IdentifiersLatex of Identifiers =
  open Formal, Prelude in {

lincat
  Term = TermPrecNum ;
  Ident = Str ;

lin
  IdentTerm x =  constant x ** {isNumber = False} ;
  NumberTerm n = constant n.s ** {isNumber = True} ;

  StrIdent s = s.s ;

oper
  TermPrecNum = TermPrec ** {isNumber : Bool} ;
}