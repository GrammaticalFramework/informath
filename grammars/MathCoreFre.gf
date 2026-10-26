concrete MathCoreFre of MathCore =
  CategoriesFre
  **
  MathCoreFunctor - [PropHypo, BaseArgKind, ConsArgKind, CoreAllProp] with
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
  PropVarHypo ident prop = lin Utt {s = (lets_Utt (mkVP assume_VS (topProp prop))).s ++  "(" ++ ident ++ ")"} ; 
  -- "pour tous les objets x", not "pour tous des objets x" (allNP)
  CoreAllProp kind ident prop =
    simpleProp (Grammar.ExtAdvS (Syntax.mkAdv for_Prep (mkNP all_Predet (mkNP thePl_Det (identKindCN ident kind)))) (partProp prop)) ;

}
