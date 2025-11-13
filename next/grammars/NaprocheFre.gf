concrete NaprocheFre of Naproche = CategoriesFre, TermsLatex **

  open
    SyntaxFre,
    ParadigmsFre,
    UtilitiesFre,
    SymbolicFre,
    Prelude,
    (E = ExtendFre)
in {

lin
  -- we only need the lexicon, because we don't translate Naproche directly
  
  inhabited_Adj = mkAdj (mkA "habité") ;
  empty_Adj = mkAdj (mkA "vide") ;
  disjoint_AdjC = mkAdjC (mkA "disjoint") ;
  
}
