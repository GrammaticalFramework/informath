concrete MathExtensionsCze of MathExtensions =
  CategoriesCze,
  TermsLatex **
  MathExtensionsFunctor - [BoundedInAllProp] with
    (Syntax = SyntaxCze),
    (Symbolic = SymbolicCze),
    (Grammar = GrammarCze),
    (Extend = ExtendCze),
    (Utilities = UtilitiesCze)
  ** open
    ParadigmsCze,
    (S=SyntaxCze),
    (G=GrammarCze),
    UtilitiesCze,
    Formal,
    Prelude
in {

lin
  -- "pro každé $\epsilon > 0$ in $\mathbb{R}$", not the singular neuter of all_Predet
  BoundedInAllProp formula set prop =
    simpleProp (G.ExtAdvS (ParadigmsCze.mkAdv ("pro každé" ++
      (S.mkUtt (S.mkNP (latexNP (mkSymb formula.s)) (S.mkAdv S.in_Prep set))).s)) (partProp prop)) ;

}
