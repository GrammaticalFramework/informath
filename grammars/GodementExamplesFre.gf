concrete GodementExamplesFre of GodementExamples =
  CategoriesFre, ExamplesFre, MizarFre
  **
  GodementExamplesFunctor - [
    NotAdjAdj,
    into_Prep, onto_Prep, and_Prep, as_Prep, among_Prep,
    generated_by_Prep, spanned_by_Prep, defined_by_Prep, induced_by_Prep, indexed_by_Prep,
    with_respect_to_Prep, relative_to_Prep, associated_with_Prep, joining_Prep,
    with_coefficients_in_Prep, with_coefficients_Prep, with_exponents_Prep, with_masses_Prep,
    of_degree_Prep, of_order_Prep, en_Prep, two_Name, three_Name
    ]
  with
    (Syntax = SyntaxFre),
    (Grammar = GrammarFre),
    (Utilities = UtilitiesFre),
    (Symbolic = SymbolicFre)
  ** open UtilitiesFre, ParadigmsFre, Prelude in {

-- The functor's English strings in French, in the words of Godement's Cours
-- d'algèbre.  and_Prep, of_order_Prep and with_respect_to_Prep were here
-- before, for the Mathlib informalization; the rest written by Claude, to be
-- reviewed.

lin
  NotAdjAdj adj = mkAP (lin AdA {s = "non"}) adj ;

  into_Prep = mkPrep "dans" ;
  onto_Prep = mkPrep "sur" ;
  and_Prep = mkPrep "et" ;
  as_Prep = mkPrep "comme" ;
  among_Prep = mkPrep "parmi" ;
  generated_by_Prep = mkPrep "engendré par" ;
  spanned_by_Prep = mkPrep "engendré par" ;
  defined_by_Prep = mkPrep "défini par" ;
  induced_by_Prep = mkPrep "induit par" ;
  indexed_by_Prep = mkPrep "indexé par" ;
  with_respect_to_Prep = mkPrep "par rapport" dative ;
  relative_to_Prep = mkPrep "relativement" dative ;
  associated_with_Prep = mkPrep "associé" dative ;
  joining_Prep = mkPrep "joignant" ;
  with_coefficients_in_Prep = mkPrep "à coefficients dans" ;
  with_coefficients_Prep = mkPrep "de coefficients" ;
  with_exponents_Prep = mkPrep ("d'" ++ BIND ++ "exposants") ;  -- bound, as the lexer splits d'
  with_masses_Prep = mkPrep "affectés des masses" ;
  of_degree_Prep = mkPrep "de degré" ;
  of_order_Prep = mkPrep ("d'" ++ BIND ++ "ordre") ;
  en_Prep = mkPrep "en" ;

  two_Name = mkName "deux" ;
  three_Name = mkName "trois" ;

}
