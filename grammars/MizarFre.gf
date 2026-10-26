concrete MizarFre of Mizar = CategoriesFre, TermsLatex
  **
  MizarFunctor with
    (Utilities=UtilitiesFre),
    (Syntax=SyntaxFre),
    (Grammar=GrammarFre),
    (Markup=MarkupFre),
    (Extend=ExtendFre),
    (Symbolic=SymbolicFre)
  ** open
    UtilitiesFre,
    Prelude,
    ParadigmsFre,
    (I=IrregFre)

in {
  -- lin non_Adverb = mkAdv "non" ;
  lin
  placeholderAdj_Adj2 = mkAdj2 "adjectif" "";
  otherwise_Adv = ParadigmsFre.mkAdv "sinon" ;
}