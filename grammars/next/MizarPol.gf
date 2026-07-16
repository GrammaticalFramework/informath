concrete MizarPol of Mizar = CategoriesPol, TermsLatex
  **
  MizarFunctor with
    (Utilities=UtilitiesPol),
    (Syntax=SyntaxPol),
    (Grammar=GrammarPol),
    (Markup=MarkupPol),
    (Extend=ExtendPol),
    (Symbolic=SymbolicPol)
  ** open
    UtilitiesPol,
    (R=ResPol),
    Prelude,
    ParadigmsPol

in {
  -- the Str overload of mkAdj2 would send "adjective" through
  -- AdjectiveMorphoPol.guess_model, which errors on anything not in -y/-i
  lin placeholderAdj_Adj2 = mkAdj2 (mkA "przymiotnikowy") (mkPrep "" R.Nom) ;
}
