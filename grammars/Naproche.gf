abstract Naproche = MathCore ** {

-- extensions from Naproche-ZF

fun
  SupposePropHypo : Prop -> Hypo ;
  IffIffProp : Prop -> Prop -> Prop ;
  WeHaveFormulaProp : Formula -> Prop ;
  NoCommaAllProp : [ArgKind] -> Prop -> Prop ;
  BareIdentsArgKind : [Ident] -> ArgKind ;

}