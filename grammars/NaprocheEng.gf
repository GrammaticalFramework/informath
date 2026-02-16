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
  Title = Text ;
  Filename = Str ;
  Environment = Str ;

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

  BeginEnvironmentUnit env label = beginUnit env label ;
  BeginProofMethodUnit label method = ccText (beginUnit "proof" label) (strText ("(" ++ method.s ++ ")")) ;
  
  EndEnvironmentUnit env = endUnit env ;


  abbreviation_Environment = "abbreviation" ;
  alignstar_Environment = "align*" ;
  axiom_Environment = "axiom" ;
  byCase_Environment = "byCase" ;
  corollary_Environment = "corollary" ;
  datatype_Environment = "datatype" ;
  definition_Environment = "definition" ;
  enumerate_Environment = "enumerate" ;
  inductive_Environment = "inductive" ;
  lemma_Environment = "lemma" ;
  primrec_Environment = "primrec" ;
  proof_Environment = "proof" ;
  proposition_Environment = "proposition" ;
  signature_Environment = "signature" ;
  struct_Environment = "struct" ;
  subproof_Environment = "subproof" ;
  theorem_Environment = "theorem" ;
  
  ImportUnit filename = strText ("\\import {" ++ filename.s ++ "}") ;
  LabelUnit label = strText ("\\label {" ++ (mkUtt label.np).s ++ "}") ;
  SectionUnit title label = strText ("\\section {" ++ title.s ++ "}" ++ labelLatex label) ;
  SubsectionUnit title label = strText ("\\subsection {" ++ title.s ++ "}" ++ labelLatex label) ;

  StringTitle s = strText s.s ;
  StringMethod s = strText s.s ;
  StringFilename s = s.s ;

  crefLabel ident = mkLabel ("\\cref {" ++ ident ++ "}") ;

  inhabited_Adj = mkAP (mkA "inhabited") ;
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
      ccText (strText ("\\begin {" ++ s ++ "}")) (strText (labelLatex label))
    } ;

  endUnit : Str -> Text = \s -> strText ("\\end {" ++ s ++ "}") ;

  labelLatex : Label -> Str = \label -> case label.isEmpty of {
    True => (mkUtt label.np).s ;
    False => "\\label {" ++ (mkUtt label.np).s ++ "}"
    } ;

}