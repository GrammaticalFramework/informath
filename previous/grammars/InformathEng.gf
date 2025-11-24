concrete InformathEng of Informath =
  MathCoreEng,
  NaprocheEng
  **
  InformathFunctor with
    (Syntax = SyntaxEng),
    (Symbolic = SymbolicEng),
    (Grammar = GrammarEng),
    (Extend = ExtendEng)
  ** open
    ParadigmsEng,
    Formal,
    Prelude,
    BaseConstantsLatex
in {

-- functor parameters
oper
  imply_V2 : V2 = mkV2 (mkV "imply") ;
  only_if_Subj : Subj = mkSubj "only if" ;
  
}