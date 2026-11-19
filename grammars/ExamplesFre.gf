concrete ExamplesFre of Examples = CategoriesFre ** ExamplesFunctor - [to_Prep] with
  (Syntax = SyntaxFre),
  (Grammar = GrammarFre),
  (Symbolic = SymbolicFre),
  (MathCore = MathCoreFre),
  (Utilities = UtilitiesFre)
  ** open ParadigmsFre in {

-- "de A à B": the French of to is à (with its contractions au, aux), which
-- Syntax.to_Prep does not show before a symbol
lin to_Prep = mkPrep "à" ;

}
