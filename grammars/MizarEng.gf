concrete MizarEng of Mizar = CategoriesEng, TermsLatex
  **
  MizarFunctor with
    (Utilities=UtilitiesEng),
    (Syntax=SyntaxEng),
    (Grammar=GrammarEng),
    (Markup=MarkupEng),
    (Extend=ExtendEng),
    (Symbolic=SymbolicEng)
  ** open
    UtilitiesEng,
    Prelude,
    ParadigmsEng,
    (I=IrregEng)

in {
  -- lin non_Adverb = mkAdv "non" ;
  lin
  placeholderAdj_Adj2 = mkAdj2 "adjective" "";
  otherwise_Adv = ParadigmsEng.mkAdv "otherwise" ;
}