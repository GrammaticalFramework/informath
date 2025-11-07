concrete NaprocheSwe of Naproche = CategoriesSwe, TermsLatex **

  open
    SyntaxSwe,
    ParadigmsSwe,
    UtilitiesSwe,
    SymbolicSwe,
    Prelude,
    (E = ExtendSwe)
in {

lin
  SupposePropHypo prop =
    mkUtt (mkImp (E.ComplBareVS suppose_VS (topProp prop))) ;
  IffIffProp a b = simpleProp (lin S {s = a.s.s ++ "iff" ++ b.s.s}) ;
  WeHaveProp prop =
    simpleProp (mkS (mkCl we_NP (E.ComplBareVS have_VS prop.s))) ;
  NoCommaAllProp argkinds prop =
    simpleProp (lin S {s = (SyntaxSwe.mkAdv for_Prep (mkNP all_Predet argkinds.pl)).s ++ (partProp prop).s}) ;
  BareIdentsArgKind idents =
    {cn = mkCN emptyN idents.np ; adv = emptyAdv ; isPl = idents.isPl} ;
  DeclarationArgKind declaration =
    {cn = mkCN emptyN (latexNP (mkSymb declaration.s)) ; adv = emptyAdv ; isPl = declaration.isPl} ; 
  IndexedDeclarationArgKind i =
    {cn = mkCN emptyN <symb (mkSymb ("\\INDEXEDTERM{" ++ i.s ++ "}")) : NP> ; adv = emptyAdv ; isPl = False} ; --- isPl

  NoCommaExistProp argkinds prop =
    simpleProp (lin S
      {s = (mkS (E.ExistsNP argkinds.sg)).s ++ "such that" ++ prop.s.s}) ; 

  NoArticleExistProp argkind prop =
    simpleProp (lin S
      {s = (mkS (E.ExistsNP (mkNP (mkCN argkind.cn argkind.adv)))).s ++ "such that" ++ prop.s.s}) ;  

  inhabited_Adj = mkAP (mkA "inhabited") ;
  empty_Adj = mkAP (mkA "empty") ;
  disjoint_AdjC = mkAP (mkA "empty") ;
  disjoint_Compar = "\\notmeets" ;
  contain_Verb2 = mkV2 "contain" ;
  ni_Compar = "\\ni" ;
  

oper
  suppose_VS = mkVS (mkV "suppose") ;
  have_VS = mkVS (lin V have_V2) ;

  emptyN = mkN "" "" ;
  emptyAdv = ParadigmsSwe.mkAdv "" ;
  
}
