-- Informath in standard logical notation, with the symbolic constants of
-- SymbolicConstantsLatex and the macros of .dkgf files.  Only MathCore is
-- linearized, together with the few MathExtensions functions that the
-- conversion from Dedukti produces; verbal constants have no linearization.

concrete InformathLatex of Informath =
  MathCoreLatex,
  SymbolicConstantsLatex
  ** open Formal, Prelude in {

lin
  FormulaProp formula = formula ;

  IdentsArgKind kind idents =
    {q = "\\forall" ++ idents ++ "\\in" ++ top kind ++ "." ; d = kind} ;

  VarsHypo idents kind = idents ++ "\\in" ++ top kind ;
  BareVarsHypo idents = idents ;

  AbsExp idents exp = mkPrec 0 ("\\lambda" ++ idents ++ "." ++ top exp) ;

  AllProp argkinds prop = mkPrec 0 (argkinds.q ++ top prop) ;

}
