abstract ProofUnits = Categories ** {

cat
  Unit ;
  [Unit] ;
  Goal ;
  Assumption ;
  Conclusion ;

fun
--  UnitsProof : [Unit] -> Proof ;

  HyposAssumption : [Hypo] -> Proof ; -- Unit ;
  ElemAssumption : Kind -> Exp -> Proof ;
  PropAssumption : Prop -> Proof ;
  
  PropConclusion : Prop -> Proof ; -- Unit ;
  SinceConclusion : Prop -> Prop -> Proof ; -- Unit ;
  LabelConclusion : Label -> Proof ; -- Unit ;
  PropLabelConclusion : Prop -> Label -> Proof ; -- Unit ;
  FollowsPropConclusion : Prop -> Proof ; -- Unit ;
  ObviousConclusion : Proof ;
  
  FirstVerifyGoal : Prop -> Proof ; -- Goal ;
  EnoughGoal : Prop -> Proof ; -- Goal ;
  SinceGoal : Prop -> Prop -> Proof ; -- Unit ;
  CaseGoal : Prop -> Prop -> Proof ;

}