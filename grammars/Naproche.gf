abstract Naproche = MathCore ** {

-- extensions from Naproche-ZF

fun
-- combinations
  SupposePropHypo : Prop -> Hypo ;
  IffIffProp : Prop -> Prop -> Prop ;
  WeHaveFormulaProp : Formula -> Prop ;
  NoCommaAllProp : [ArgKind] -> Prop -> Prop ;
  BareIdentsArgKind : [Ident] -> ArgKind ;

-- lexicon
  inhabited_Adj : Adj ;
  empty_Adj : Adj ;
  distinct_Reladj : Reladj ;

}