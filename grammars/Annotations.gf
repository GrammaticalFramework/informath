abstract Annotations = Categories ** {

-- intermediate representation to store Dedukti items; not meant to be parsed or linearized

fun
  AnnotateProp : Ident -> Prop -> Prop ;
  AnnotateKind : Ident -> Kind -> Kind ;
  AnnotateExp  : Ident -> Exp  -> Exp ;
  AnnotateProof : Ident -> Proof -> Proof ;
  AnnotateProofExp : Ident -> ProofExp -> ProofExp ;


}
