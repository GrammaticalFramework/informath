concrete MizarCze of Mizar = CategoriesCze, TermsLatex
  **
  MizarFunctor with
    (Utilities=UtilitiesCze),
    (Syntax=SyntaxCze),
    (Grammar=GrammarCze),
    (Markup=MarkupCze),
    (Extend=ExtendCze),
    (Symbolic=SymbolicCze)
  ** open
    UtilitiesCze,
    Prelude,
    ParadigmsCze

in {
  lin
  placeholderAdj_Adj2 = mkAdj2 (mkA "přídavný") (mkPrep "" nominative) ;
  otherwise_Adv = ParadigmsCze.mkAdv "jinak" ;
}
