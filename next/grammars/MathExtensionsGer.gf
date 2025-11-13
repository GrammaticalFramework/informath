concrete MathExtensionsGer of MathExtensions =
  CategoriesGer,
  TermsLatex **
  MathExtensionsFunctor - [Adj3Adj, AllKindExp, AllIdentsKindExp]
  with
    (Syntax = SyntaxGer),
    (Symbolic = SymbolicGer),
    (Grammar = GrammarGer),
    (Extend = ExtendGer),
    (Utilities = UtilitiesGer)
  ** open
    ParadigmsGer,
    Formal,
    Prelude
in {

lin
  AllKindExp kind = mkNP alle_Det (useKind kind) ;
  AllIdentsKindExp idents kind = mkNP alle_Det (mkCN (mkCN kind.cn idents.np) kind.adv) ;

  Adj3Adj pred y z =
    AdvAP pred.ap (Syntax.mkAdv pred.prep1 (mkNP y (Syntax.mkAdv pred.prep2 z))) ; --- fake structure
}
