concrete MathCoreGer of MathCore =
  CategoriesGer
  **
  MathCoreFunctor - [Adj3Prop] with
    (Utilities=UtilitiesGer),
    (Syntax=SyntaxGer),
    (Grammar=GrammarGer),
    (Markup=MarkupGer),
    (Extend=ExtendGer),
    (Symbolic=SymbolicGer)
  ** open
    UtilitiesGer,
    Prelude,
    ParadigmsGer,
    (I=IrregGer)

in {

{- --- too slow to compile even like this
lin
  Adj3Prop pred x y z =
    simpleProp (mkS (mkCl x (AdvAP pred.ap (lin Adv
      {s = (Syntax.mkAdv pred.prep1 y).s ++ (Syntax.mkAdv pred.prep2 z).s})))) ;
-}

}
