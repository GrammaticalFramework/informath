concrete NaprocheGer of Naproche = MathCoreGer **

  open
    SyntaxGer,
    ParadigmsGer,
    UtilitiesGer,
    SymbolicGer,
    Prelude,
    (E = ExtendGer)
in {

lin
  -- we only need the lexicon, because we don't translate Naproche directly
  
  inhabited_Adj = mkAP (mkA "nichtleer") ;
  empty_Adj = mkAP (mkA "leer") ;
  disjoint_Compar = mkCompar "\\notmeets" (mkAP (mkA "disjunkt")) von_Prep ;
  ni_Compar = mkCompar "\\ni" "enthaltend" "" ; ---- should be Relverb "enthalten"
  
  
}