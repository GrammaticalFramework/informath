abstract Naproche = Categories, Terms ** {

-- extensions from Naproche-ZF

cat
  Method ; -- sometimes used in \begin{proof}[ <Method> ]
  Title ;  -- used in \section, \subsection
  Filename ; -- used in \import
  Environment ; -- used in \begin and \end

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

  BeginEnvironmentUnit : Environment -> Label -> Unit ;
  BeginProofMethodUnit : Label -> Method -> Unit ;
  
  EndEnvironmentUnit : Environment -> Unit ;

  abbreviation_Environment : Environment ;
  alignstar_Environment : Environment ;
  axiom_Environment : Environment ;
  byCase_Environment : Environment ;
  corollary_Environment : Environment ;
  datatype_Environment : Environment ;
  definition_Environment : Environment ;
  enumerate_Environment : Environment ;
  inductive_Environment : Environment ;
  lemma_Environment : Environment ;
  primrec_Environment : Environment ;
  proof_Environment : Environment ;
  proposition_Environment : Environment ;
  signature_Environment : Environment ;
  struct_Environment : Environment ;
  subproof_Environment : Environment ;
  theorem_Environment : Environment ;
  
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