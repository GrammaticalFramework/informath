concrete GodementExamplesCze of GodementExamples =
  CategoriesCze, ExamplesCze, MizarCze
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
    (Syntax = SyntaxCze),
    (Grammar = GrammarCze),
    (Utilities = UtilitiesCze),
    (Symbolic = SymbolicCze)
  ** open UtilitiesCze, ParadigmsCze, (S=SyntaxCze), Prelude in {

-- and_Prep, of_order_Prep and with_respect_to_Prep were here before, for the
-- Mathlib informalization; the rest written by Claude, to be reviewed.
-- The participial prepositions cannot agree with their noun: they are in the
-- masculine, except "indexovaná", which goes with "rodina" (family).

lin
  NotAdjAdj adj = mkAP (lin AdA {s = "ne" ++ BIND}) adj ;

-- genitive, as the second argument of "the intersection of A and B" (průnik A a B)
  and_Prep = mkPrep "a" genitive ;
  of_order_Prep = mkPrep "řádu" nominative ;
  with_respect_to_Prep = mkPrep "vzhledem k" dative ;

  into_Prep = mkPrep "do" genitive ;
  onto_Prep = mkPrep "na" accusative ;
  as_Prep = mkPrep "jako" nominative ;
  among_Prep = mkPrep "mezi" instrumental ;
  generated_by_Prep = mkPrep "generovaný" instrumental ;
  spanned_by_Prep = mkPrep "generovaný" instrumental ;
  defined_by_Prep = mkPrep "definovaný" instrumental ;
  induced_by_Prep = mkPrep "indukovaný" instrumental ;
  indexed_by_Prep = mkPrep "indexovaná" instrumental ;
  relative_to_Prep = mkPrep "vzhledem k" dative ;
  associated_with_Prep = mkPrep "přidružený k" dative ;
  joining_Prep = mkPrep "spojující" accusative ;
  with_coefficients_in_Prep = mkPrep "s koeficienty v" locative ;
  with_coefficients_Prep = mkPrep "s koeficienty" nominative ;
  with_exponents_Prep = mkPrep "s exponenty" nominative ;
  with_masses_Prep = mkPrep "s hmotnostmi" nominative ;
  of_degree_Prep = mkPrep "stupně" nominative ;
  en_Prep = mkPrep "v" locative ;

  two_Name = mkName "dva" ;
  three_Name = mkName "tři" ;

}
