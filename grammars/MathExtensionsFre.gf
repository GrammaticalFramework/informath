concrete MathExtensionsFre of MathExtensions =
  CategoriesFre,
  TermsLatex **
  MathExtensionsFunctor  - [AllKindExp, AllIdentsKindExp, AllKindQuant]

  with
    (Syntax = SyntaxFre),
    (Symbolic = SymbolicFre),
    (Grammar = GrammarFre),
    (Extend = ExtendFre),
    (Utilities = UtilitiesFre)
  ** open
    ParadigmsFre,
    Formal,
    Prelude
in {

lin
  AllKindExp kind = mkNP all_Predet (mkNP thePl_Det (useKind kind)) ;
  AllIdentsKindExp idents kind = mkNP all_Predet (mkNP thePl_Det (mkCN (mkCN kind.cn idents.np) kind.adv)) ;
  AllKindQuant kind = mkNP all_Predet (mkNP thePl_Det (useKind kind)) ;  -- "tous les", not "tous des"

}
