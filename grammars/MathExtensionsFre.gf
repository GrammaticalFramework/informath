concrete MathExtensionsFre of MathExtensions =
  CategoriesFre,
  TermsLatex **
  MathExtensionsFunctor  - [AllKindExp, AllIdentsKindExp, AllKindQuant, BothAndProp, EitherOrProp]

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

  -- "à la fois A et B", "soit A , soit B": the RGL's "et A et B", "ou A ou B"
  -- are rare in mathematical French
  BothAndProp x y = simpleProp (mkS (lin Conj {s1 = "à la fois" ; s2 = "et" ; n = plural}) x.s y.s) ;
  EitherOrProp x y = simpleProp (mkS (lin Conj {s1 = "soit" ; s2 = "," ++ "soit" ; n = singular}) x.s y.s) ;

}
