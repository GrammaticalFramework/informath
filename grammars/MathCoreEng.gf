concrete MathCoreEng of MathCore =
  CategoriesEng
  **
  MathCoreFunctor - [negPol] with
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

oper
-- override
  negPol : Pol = UncontractedNeg ;

}