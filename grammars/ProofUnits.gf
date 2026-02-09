abstract ProofUnits = Categories ** {

cat
  [Unit] ;
  Hence ;

fun
  UnitsProof : [Unit] -> Proof ;

  HyposAssumption : [Hypo] -> Unit ;
  IdentKindAssumption : Kind -> Ident -> Unit ;
  IdentExpAssumption : Exp -> Ident -> Unit ;
  PropAssumption : Prop -> Unit ;
  
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
  CaseGoal : Prop -> Unit ;

  noHence : Hence ;
  henceHence : Hence ;
  thusHence : Hence ;
  thenHence : Hence ;
  altogetherHence : Hence ;
  afortioriHence : Hence ;
  inParticularHence : Hence ;
  weConcludeHence : Hence ;

}