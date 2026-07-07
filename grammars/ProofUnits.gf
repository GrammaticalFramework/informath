abstract ProofUnits = Categories ** {

-- core functions for proof text linearization

fun
  HyposUnit : [Hypo] -> Unit ;
  ConclusionUnit : Prop -> Unit ;
  
  UnitJmt : Unit -> Jmt ; --- for top level?



-- old stuff

cat
  [Unit] ;
  Hence ;


fun
  --- these are not (yet) interpretable in Dedukti
  UnitsProof : [Unit] -> Proof ;

  HyposAssumption : [Hypo] -> Unit ;
  IdentKindAssumption : Kind -> Ident -> Unit ;
  IdentExpAssumption : Exp -> Ident -> Unit ;
  PropAssumption : Prop -> Label -> Unit ;
  
  PropConclusion : Hence -> Prop -> Unit ;
  SinceConclusion : Prop -> Prop -> Unit ;
  LabelConclusion : Label -> Unit ;
  PropLabelConclusion : Hence -> Prop -> Label -> Unit ;
  FollowsPropConclusion : Prop -> Unit ;
  ObviousConclusion : Unit ;
  
  FirstVerifyGoal : Prop -> Unit ;
  EnoughGoal : Prop -> Unit ;
  SinceGoal : Prop -> Prop -> Unit ;
  CasesGoal : Unit ;
  InductionGoal : Unit ;
  CaseGoal : Prop -> Ident -> Unit ;

  noHence : Hence ;
  henceHence : Hence ;
  thusHence : Hence ;
  thenHence : Hence ;
  altogetherHence : Hence ;
  afortioriHence : Hence ;
  inParticularHence : Hence ;
  weConcludeHence : Hence ;

}