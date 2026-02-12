concrete NaprocheEng of Naproche = CategoriesEng, TermsLatex **

  open
    SyntaxEng,
    ParadigmsEng,
    UtilitiesEng,
    SymbolicEng,
    Prelude,
    (E = ExtendEng)
in {

lincat
  Method = Text ;

lin
  SupposePropHypo prop =
    mkUtt (mkImp (E.ComplBareVS suppose_VS (topProp prop))) ;
  IffIffProp a b = simpleProp (lin S {s = a.s.s ++ "iff" ++ b.s.s}) ;
  WeHaveProp prop =
    simpleProp (mkS (mkCl we_NP (E.ComplBareVS have_VS prop.s))) ;
  NoCommaAllProp argkinds prop =
    simpleProp (lin S {s = (SyntaxEng.mkAdv for_Prep (mkNP all_Predet argkinds.pl)).s ++ (partProp prop).s}) ;
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


  BeginPropositionUnit label = beginUnit "proposition" label ;
  EndPropositionUnit = endUnit "proposition" ;
  BeginProofUnit = beginUnit "proof" ; 
  BeginProofMethodUnit method = ccText (beginUnit "proof") (strText ("(" ++ method.s ++ ")")) ;
  EndProofUnit = endUnit "proof" ;
  BeginAbbreviationUnit label = beginUnit "abbreviation" label ;
  EndAbbreviationUnit = endUnit "abbreviation" ;
  BeginLemmaUnit label = beginUnit "lemma" label ;
  EndLemmaUnit = endUnit "lemma" ;
  BeginDefinitionUnit label = beginUnit "definition" label ;
  EndDefinitionUnit = endUnit "definition" ;
  BeginStructUnit label = beginUnit "struct" label ;
  EndStructUnit = endUnit "struct" ;
  BeginEnumerateUnit = beginUnit "enumerate" ;
  EndEnumerateUnit = endUnit "enumerate" ;

  crefLabel ident = mkLabel ("\\cref {" ++ ident ++ "}") ;

  inhabited_Adj = mkAP (mkA "inhabited") ;
  empty_Adj = mkAP (mkA "empty") ;
  disjoint_AdjC = mkAP (mkA "disjoint") ;
  disjoint_Compar = "\\notmeets" ;
  contain_Verb2 = mkV2 "contain" ;
  ni_Compar = "\\ni" ;


oper
  suppose_VS = mkVS (mkV "suppose") ;
  have_VS = mkVS (lin V have_V2) ;

  emptyN = mkN "" "" ;
  emptyAdv = ParadigmsEng.mkAdv "" ;

  beginUnit = overload {
    beginUnit : Str -> Text = \s -> strText ("\\begin { " ++ s ++ "}") ;
    beginUnit : Str -> Label -> Text = \s, label ->
      ccText (strText ("\\begin {" ++ s ++ "}")) (strText ("\\label {" ++ (mkUtt label.np).s ++ "}")) ;
    } ;

  endUnit : Str -> Text = \s -> strText ("\\end {" ++ s ++ "}") ;

}