concrete InformathFre of Informath =
  MathCoreFre,
  NaprocheFre
  **
  InformathFunctor - [
    AllKindExp, AllIdentsKindExp,
    postAdvS
  ] with
    (Syntax = SyntaxFre),
    (Symbolic = SymbolicFre),
    (Grammar = GrammarFre),
    (Extend = ExtendFre)
  ** open
    ParadigmsFre,
    (Mk = MakeStructuralFre),
    Formal,
    Prelude,
    BaseConstantsLatex
in {

lin
  AllKindExp kind = mkNP all_Predet (mkNP thePl_Det (useKind kind)) ;
  AllIdentsKindExp idents kind = mkNP all_Predet (mkNP thePl_Det (mkCN (mkCN kind.cn idents.np) kind.adv)) ;

oper
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  imply_V2 : V2 = mkV2 (mkV "impliquer") ;
  only_if_Subj : Subj = Mk.mkSubj "seulement si" ;

}