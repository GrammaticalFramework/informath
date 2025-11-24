concrete InformathSwe of Informath =
  MathCoreSwe **
  InformathFunctor - [postAdvS] with
    (Syntax = SyntaxSwe),
    (Symbolic = SymbolicSwe),
    (Grammar = GrammarSwe),
    (Extend = ExtendSwe)
  ** open
    ParadigmsSwe,
    Formal,
    Prelude,
    BaseConstantsLatex
in {

oper
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  imply_V2 : V2 = mkV2 (mkV "implicera") ;
  only_if_Subj : Subj = mkSubj "endast om" ;

}