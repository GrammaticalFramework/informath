concrete MathCoreFre of MathCore =
  CategoriesFre
  **
  MathCoreFunctor - [PropHypo, BaseArgKind, ConsArgKind] with
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

-- functor exceptions
lin
  PropHypo prop = lets_Utt (mkVP assume_VS (topProp prop)) ;

}
