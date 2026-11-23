concrete NaprocheCze of Naproche = CategoriesCze, TermsLatex **

  open
    SyntaxCze,
    ParadigmsCze,
    UtilitiesCze,
    SymbolicCze,
    Prelude,
    (E = ExtendCze),
    (R = ResCze),
    (Sy = SymbolCze)
in {

-- written by Claude, to be reviewed: the Czech of NaprocheEng

lincat
  Method = Text ;
  Title = Text ;
  Filename = Str ;
  Environment = Str ;

lin
  SupposePropHypo prop = assumeUtt (mkVP assume_VS (topProp prop)) ;
  IffIffProp a b = simpleProp (strS (a.s.s ++ "právě tehdy, když" ++ b.s.s)) ;
  WeHaveProp prop = simpleProp (strS ("platí, že" ++ prop.s.s)) ;
  NoCommaAllProp argkinds prop =
    simpleProp (strS ((SyntaxCze.mkAdv for_Prep (mkNP all_Predet (fromArgKindNP argkinds.pl))).s ++ (partProp prop).s)) ;
  -- the variables in the neuter, as in "pro všechna x"
  BareIdentsArgKind idents =
    {cn = mkCN emptyCN idents.np ; adv = emptyAdv ; isPl = idents.isPl} ;
  DeclarationArgKind declaration =
    {cn = mkCN emptyCN (latexNP (mkSymb declaration.s)) ; adv = emptyAdv ; isPl = declaration.isPl} ;
  IndexedDeclarationArgKind i =
    {cn = mkCN emptyCN <symb (mkSymb ("\\INDEXEDTERM{" ++ i.s ++ "}")) : NP> ; adv = emptyAdv ; isPl = False} ; --- isPl

  NoCommaExistProp argkinds prop =
    simpleProp (strS
      ((mkS (E.ExistsNP (fromArgKindNP argkinds.sg))).s ++ "takový, že" ++ prop.s.s)) ;

  NoArticleExistProp argkind prop =
    simpleProp (strS
      ((mkS (E.ExistsNP (mkNP (mkCN argkind.cn argkind.adv)))).s ++ "takový, že" ++ prop.s.s)) ;

  BeginEnvironmentUnit env label = beginUnit env label ;
  BeginProofMethodUnit label method = ccText (beginUnit "proof" label) (strText ("(" ++ method.s ++ ")")) ;

  EndEnvironmentUnit env = endUnit env ;

  -- the LaTeX environment names are not translated
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

  inhabited_Adj = mkAdj (mkA "obydlený") ;
  disjoint_AdjC = mkAdjC (mkA "disjunktní") ;
  disjoint_Compar = "\\notmeets" ;
  contain_Verb2 = mkVerb2 (mkV "obsahovat") (mkPrep "" accusative) ;
  ni_Compar = "\\ni" ;

oper
  strS : Str -> S = \s -> Sy.SymbS (Sy.MkSymb (ss s)) ;
  emptyCN : CN = lin CN {s = \\_, _ => [] ; g = R.Neutr ; gPl = R.Neutr} ;
  emptyAdv : Adv = ParadigmsCze.mkAdv "" ;

  beginUnit = overload {
    beginUnit : Str -> Text = \s -> strText ("\\begin { " ++ s ++ "}") ;
    beginUnit : Str -> LabelT -> Text = \s, label ->
      ccText (strText ("\\begin {" ++ s ++ "}")) (strText (labelLatex label))
    } ;

  endUnit : Str -> Text = \s -> strText ("\\end {" ++ s ++ "}") ;

  labelLatex : LabelT -> Str = \label -> case label.isEmpty of {
    True => (mkUtt label.np).s ;
    False => "\\label {" ++ (mkUtt label.np).s ++ "}"
    } ;

}
