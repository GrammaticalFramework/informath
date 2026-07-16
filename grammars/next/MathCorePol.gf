concrete MathCorePol of MathCore =
  CategoriesPol
  **
  MathCoreFunctor - [FalseProp, AxiomPropJmt, ProofProp] with
    (Utilities=UtilitiesPol),
    (Syntax=SyntaxPol),
    (Grammar=GrammarPol),
    (Markup=MarkupPol),
    (Extend=ExtendPol),
    (Symbolic=SymbolicPol)
  ** open
    UtilitiesPol,
    SyntaxPol,
    (E=ExtendPol),
    Prelude,
    ParadigmsPol

in {

-- Polish is pro-drop: mathematical prose says "mamy sprzeczność", not
-- "my mamy sprzeczność", where the overt pronoun reads as emphatic. we_NP is
-- fixed to "mkNP we_Pron" by the Syntax interface itself, so it cannot be
-- overridden there; instead the three functor rules that use it as a subject
-- are excluded above and redefined here.

oper
  weNP : NP = mkNP (E.ProDrop we_Pron) ;

lin
  FalseProp = simpleProp (mkS (mkCl weNP have_V2 (mkNP a_Det contradiction_N))) ;

  AxiomPropJmt label hypos prop =
    labelText label
      (thenText hypos (mkS (mkCl weNP can_VV (mkVP say_VS (topProp prop))))) ;

  ProofProp prop = prop ** {
    s = mkS (mkCl weNP can_VV (mkVP prove_VS prop.s)) ;
    } ;

}
