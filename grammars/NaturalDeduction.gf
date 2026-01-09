abstract NaturalDeduction = Categories ** {

fun
  falseEProof : Prop -> Proof -> Proof ;
  
  andIProof : Prop -> Prop -> Proof -> Proof -> Proof ;
  andElProof : Prop -> Prop -> Proof -> Proof ;
  andErProof : Prop -> Prop -> Proof -> Proof ;

  orIlProof : (A : Prop) -> (B : Prop) -> Proof -> Proof ;
  orIrProof : (A : Prop) -> (B : Prop) -> Proof -> Proof ;

  orEProof : (A : Prop) -> (B : Prop) -> (C : Prop) -> Proof ->
    (x : Ident) -> (d : Proof) -> (y : Ident) -> Proof -> Proof ;
  
  iIProof : Prop -> Prop -> Ident -> Proof -> Proof ;
  ifEProof : Prop -> Prop -> Proof -> Proof -> Proof ;

  forallIProof : (A : Kind) -> (x : Ident) -> (B : Prop) -> (z : Ident) -> (b : Proof) -> Proof ;

  forallEProof : (A : Kind) -> (x : Ident) -> (B : Prop) -> Proof -> Exp -> Proof ;

  existsIProof : (A : Kind) -> (x : Ident) -> (B : Prop) -> (a : Exp) -> (b : Proof) -> Proof ;

  existsEProof : (A : Kind) -> (x : Ident) -> (B : Prop) -> (C : Prop) ->
    Proof -> (x : Ident) -> (y : Ident) -> Proof -> Proof ;


  hypoProof : Prop -> Ident -> Proof ;

  reflProof : (A : Kind) -> (a : Exp) -> Proof ;

  NatEProof : (n : Exp) -> (x : Ident) -> (C : Prop) -> 
    (d : Proof) -> (n : Ident) -> (h : Ident) -> Proof -> Proof ;

  evenZeroProof : Proof ;
  evenSuccProof : Exp -> Proof -> Proof ;
  oddSuccProof : Exp -> Proof -> Proof ;

}