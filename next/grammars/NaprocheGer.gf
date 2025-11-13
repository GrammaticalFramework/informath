concrete NaprocheGer of Naproche = CategoriesGer, TermsLatex **

  open
    SyntaxGer,
    ParadigmsGer,
    UtilitiesGer,
    SymbolicGer,
    IrregGer,
    Prelude,
    (E = ExtendGer)
in {

lin
  -- we only need the lexicon, because we don't translate Naproche directly
  
  inhabited_Adj = mkAP (mkA "nichtleer") ;
  empty_Adj = mkAP (mkA "leer") ;
  disjoint_AdjC = mkAdjC (mkAP (mkA "disjunkt")) ;
  contain_Verb2 = mkVerb2 (fixprefixV "ent" halten_V) ;
  ni_Compar = "\\ni" ;

}
