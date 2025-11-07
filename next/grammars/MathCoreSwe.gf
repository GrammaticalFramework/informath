concrete MathCoreSwe of MathCore =
  CategoriesSwe
  **
  MathCoreFunctor - [negPol] with
    (Utilities=UtilitiesSwe),
    (Syntax=SyntaxSwe),
    (Grammar=GrammarSwe),
    (Markup=MarkupSwe),
    (Extend=ExtendSwe),
    (Symbolic=SymbolicSwe)
  ** open
    UtilitiesSwe,
    Prelude,
    ParadigmsSwe,
    (I=IrregSwe)

in {

oper
-- override
  negPol : Pol = UncontractedNeg ;

}
