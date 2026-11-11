-- Informath in standard logical notation, with the symbolic constants of
-- SymbolicConstantsLatex and the macros of .dkgf files.  Only MathCore is
-- linearized, together with the few MathExtensions functions that the
-- conversion from Dedukti produces; verbal constants have no linearization.

concrete InformathLatex of Informath =
  MathCoreLatex,
  SymbolicConstantsLatex
  ** open Formal, Prelude in {

lincat
  Predicate = Str ;

lin
  FormulaProp formula = formula ;

  -- a constant without an entry applied to formulas, as a formula: c(x, y)
  -- (Dedukti2MathCore makes it of Mizar's predicate formula)
  IdentPredicate id = id ;
  AppPredicateFormula P xs = tconstant (P ++ "(" ++ xs.s ++ ")") ;

  IdentsArgKind kind idents =
    {q = "\\forall" ++ idents ++ "\\in" ++ top kind ++ "." ; d = kind} ;

  VarsHypo idents kind = idents ++ "\\in" ++ top kind ;
  BareVarsHypo idents = idents ;

  AbsExp idents exp = mkPrec 0 ("\\lambda" ++ idents ++ "." ++ top exp) ;

  AllProp argkinds prop = mkPrec 0 (argkinds.q ++ top prop) ;
  BareAllProp idents prop = mkPrec 0 ("\\forall" ++ idents ++ "." ++ top prop) ;
  BareExistProp idents prop = mkPrec 0 ("\\exists" ++ idents ++ "." ++ top prop) ;
  FormulaArgKind kind formula = {q = "\\forall" ++ top formula ++ "." ; d = kind} ;
  BoundedInAllProp formula set prop = mkPrec 0 ("\\forall" ++ top formula ++ "." ++ top prop) ;
  BoundedInExistProp formula set prop = mkPrec 0 ("\\exists" ++ top formula ++ "." ++ top prop) ;
  FunExpKind a b = mkPrec 0 (usePrec 1 a ++ "\\rightarrow" ++ top b) ;

}
