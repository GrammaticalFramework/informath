concrete GodementSyntaxFre of GodementSyntax =
  MathExtensionsFre
  **
  GodementSyntaxFunctor - [said_DefVerb, defined_DefVerb] with
    (Syntax = SyntaxFre),
    (Symbolic = SymbolicFre),
    (Grammar = GrammarFre),
    (Extend = ExtendFre),
    (Utilities = UtilitiesFre),
    (UtilitiesGodement = UtilitiesGodementFre)
  ** open
    ParadigmsFre,
    Formal,
    Prelude
in {

-- French says "E est dit symétrisable" and "E est par définition X", not
-- "est dit être" and "est défini être" (the English modes, PassToBe)
lin
  said_DefVerb = {v2 = say_V2 ; adv = by_definition_AdV ; mode = PassDirect} ;
  defined_DefVerb = {v2 = defineG_V2 ; adv = by_definition_AdV ; mode = ByDef} ;

}
