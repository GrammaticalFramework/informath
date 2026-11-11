concrete MathExtensionsSwe of MathExtensions =
  CategoriesSwe,
  TermsLatex **
  MathExtensionsFunctor - [BoundedInAllProp] with
    (Syntax = SyntaxSwe),
    (Symbolic = SymbolicSwe),
    (Grammar = GrammarSwe),
    (Extend = ExtendSwe),
    (Utilities = UtilitiesSwe)
  ** open
    ParadigmsSwe,
    (S=SyntaxSwe),
    (G=GrammarSwe),
    UtilitiesSwe,
    Formal,
    Prelude
in {

lin
  -- "för alla $\epsilon > 0$ in $\mathbb{R}$", not the singular neuter of all_Predet
  BoundedInAllProp formula set prop =
    simpleProp (G.ExtAdvS (ParadigmsSwe.mkAdv ("för alla" ++
      (S.mkUtt (S.mkNP (latexNP (mkSymb formula.s)) (S.mkAdv S.in_Prep set))).s)) (partProp prop)) ;

}
