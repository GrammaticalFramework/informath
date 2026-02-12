abstract Naproche = Categories, Terms ** {

-- extensions from Naproche-ZF

cat
  Method ; -- sometimes used in \begin{proof}[ <Method> ]

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

  BeginPropositionUnit : Label -> Unit ;
  EndPropositionUnit : Unit ;
  BeginProofUnit : Unit ;
  BeginProofMethodUnit : Method -> Unit ;
  EndProofUnit : Unit ;
  BeginAbbreviationUnit : Label -> Unit ;
  EndAbbreviationUnit : Unit ;
  BeginLemmaUnit : Label -> Unit ;
  EndLemmaUnit : Unit ;
  BeginDefinitionUnit : Label -> Unit ;
  EndDefinitionUnit : Unit ;
  BeginStructUnit : Label -> Unit ;
  EndStructUnit : Unit ;
  BeginEnumerateUnit : Unit ;
  EndEnumerateUnit : Unit ;
----  ImportUnit : String -> Unit ;

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