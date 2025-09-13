--# -path=.:present

concrete InformathGer of Informath =
  MathCoreGer,
  NaprocheGer
  **
  InformathFunctor - [
    Pred3Adj,
    postAdvS,
    AllKindExp, AllIdentsKindExp
  ] with
    (Syntax = SyntaxGer),
    (Symbolic = SymbolicGer),
    (Grammar = GrammarGer),
    (Extend = ExtendGer)
  ** open
    ParadigmsGer,
    Formal,
    Prelude,
    BaseConstantsLatex
in {

lin
  AllKindExp kind = mkNP alle_Det (useKind kind) ;
  AllIdentsKindExp idents kind = mkNP alle_Det (mkCN (mkCN kind.cn idents.np) kind.adv) ;

  Pred3Adj pred y z =
    AdvAP pred.ap (Syntax.mkAdv pred.prep1 (mkNP y (Syntax.mkAdv pred.prep2 z))) ; --- fake structure


oper
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  imply_V2 : V2 = mkV2 (mkV "implizieren") ;
  only_if_Subj : Subj = mkSubj "nur dann , wenn" ;

}
