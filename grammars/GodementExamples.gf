-- Forms for symbol table examples (.dkgf entries) needed for Godement's Algebra,
-- in addition to those in Examples.gf. New lexical categories come with their
-- application functions, which are also usable in text.

abstract GodementExamples = Categories, Examples, Mizar ** {

cat
  Fun3 ;      -- Exp -> Exp -> Exp -> Exp    -- the class of x modulo R in X
  AdjC3 ;     -- Exp -> Exp -> Exp -> Prop   -- x and y are equivalent under R
  NegNoun2 ;  -- Exp -> Exp -> Prop          -- x is not an element of X

fun
-- application functions of the new categories
  Fun3Exp : Fun3 -> Exp -> Exp -> Exp -> Exp ;
  AdjC3Prop : AdjC3 -> Exp -> Exp -> Exp -> Prop ;
  NegNoun2Prop : NegNoun2 -> Exp -> Exp -> Prop ;

-- examples
  Noun3Example : Noun3 -> Argument -> Argument -> Argument -> Example ;  -- X is a homomorphism of Y into Z
  Fun3Example : Fun3 -> Argument -> Argument -> Argument -> Example ;    -- the class of X modulo Y in Z
  AdjC3Example : AdjC3 -> Argument -> Argument -> Argument -> Example ;  -- X and Y are equivalent under Z
  NegNoun2Example : NegNoun2 -> Argument -> Argument -> Example ;        -- X is not an element of Y

-- building lexical items (NounPrepsNoun3 : Noun -> Prep -> Prep -> Noun3 is in Mizar)
  NounPrepsFun3 : Noun -> Prep -> Prep -> Prep -> Fun3 ;
  AdjPrepAdjC3 : Adj -> Prep -> AdjC3 ;
  NotNoun2 : Noun2 -> NegNoun2 ;
  NotAdjAdj : Adj -> Adj ;    -- not empty

-- prepositions missing from Examples, including participial ones
  into_Prep, onto_Prep, and_Prep, as_Prep, among_Prep : Prep ;
  generated_by_Prep, spanned_by_Prep, defined_by_Prep, induced_by_Prep, indexed_by_Prep : Prep ;
  with_respect_to_Prep, relative_to_Prep : Prep ;
  associated_with_Prep, joining_Prep : Prep ;
  with_coefficients_in_Prep, with_coefficients_Prep, with_exponents_Prep, with_masses_Prep : Prep ;
  of_degree_Prep, of_order_Prep : Prep ;

-- numbers used as names
  two_Name, three_Name : Name ;

}
