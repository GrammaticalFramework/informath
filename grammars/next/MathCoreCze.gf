concrete MathCoreCze of MathCore =
  CategoriesCze
  **
  MathCoreFunctor - [ExpKind] with
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
  -- "prvky z X", not the bare genitive, which a symbol does not show:
  -- "pro všechny prvky x z X"
  ExpKind exp = {cn = mkCN element_N ; adv = SyntaxCze.mkAdv (mkPrep "z" genitive) exp} ;

}
