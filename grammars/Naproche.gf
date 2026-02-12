abstract Naproche = Categories, Terms ** {

-- extensions from Naproche-ZF

cat
  Method ; -- sometimes used in \begin{proof}[ <Method> ]
  Title ;  -- used in \section, \subsection
  Filename ; -- used in \import

fun
-- combinations
  SupposePropHypo : Prop -> Hypo ;
  IffIffProp : Prop -> Prop -> Prop ;
  WeHaveProp : Prop -> Prop ;
  NoCommaAllProp : [ArgKind] -> Prop -> Prop ;
  BareIdentsArgKind : [Ident] -> ArgKind ;
  DeclarationArgKind : Declaration -> ArgKind ;
  IndexedDeclarationArgKind : Int -> ArgKind ;
  NoCommaExistProp : [ArgKind] -> Prop -> Prop ;
  NoArticleExistProp : ArgKind -> Prop -> Prop ;  -- there exists x such that P

-- proof units

  BeginAbbreviationUnit : Label -> Unit ;
  BeginAxiomUnit : Label -> Unit ;
  BeginCorollaryUnit : Label -> Unit ;
  BeginDatatypeUnit : Label -> Unit ;
  BeginDefinitionUnit : Label -> Unit ;
  BeginEnumerateUnit : Label -> Unit ;
  BeginInductiveUnit : Label -> Unit ;
  BeginLemmaUnit : Label -> Unit ;
  BeginPrimrecUnit : Label -> Unit ;
  BeginProofMethodUnit : Label -> Method -> Unit ;
  BeginProofUnit : Label -> Unit ;
  BeginPropositionUnit : Label -> Unit ;
  BeginSignatureUnit : Label -> Unit ;
  BeginStructUnit : Label -> Unit ;
  BeginTheoremUnit : Label -> Unit ;
  
  EndAbbreviationUnit : Unit ;
  EndAxiomUnit : Unit ;
  EndCorollaryUnit : Unit ;
  EndDatatypeUnit : Unit ;
  EndDefinitionUnit : Unit ;
  EndEnumerateUnit : Unit ;
  EndInductiveUnit : Unit ;
  EndLemmaUnit : Unit ;
  EndPrimrecUnit : Unit ;
  EndProofUnit : Unit ;
  EndPropositionUnit : Unit ;
  EndSignatureUnit : Unit ;
  EndStructUnit : Unit ;
  EndTheoremUnit : Unit ;
  
  ImportUnit : String -> Unit ;
  LabelUnit : Label -> Unit ;
  SectionUnit : Title -> Label -> Unit ;
  SubsectionUnit : Title -> Label -> Unit ;

  StringTitle : String -> Title ;
  StringMethod : String -> Method ;
  StringFilename : String -> Filename ;

-- reference
  crefLabel : Ident -> Label ;


-- lexicon
  inhabited_Adj : Adj ;
  empty_Adj : Adj ;
  disjoint_AdjC : AdjC ;
  disjoint_Compar : Compar ;
  contain_Verb2 : Verb2 ;
  ni_Compar : Compar ;
  
}