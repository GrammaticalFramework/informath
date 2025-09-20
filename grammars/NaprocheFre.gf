concrete NaprocheFre of Naproche = MathCoreFre **

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
  
  inhabited_Adj = mkAP (mkA "habité") ;
  empty_Adj = mkAP (mkA "vide") ;
  disjoint_Compar = mkCompar "\\notmeets" (mkA "disjoint") genitive ;
  ni_Compar = mkCompar "\\ni" "contenant" "" ; ---- should be Relverb "contenir"
  
  
}