abstract Naproche = Categories, Terms ** {

-- extensions from Naproche-ZF

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


-- lexicon
  inhabited_Adj : Adj ;
  empty_Adj : Adj ;
  disjoint_Eqsign : Eqsign ;
  ni_Eqsign : Eqsign ;
  
}