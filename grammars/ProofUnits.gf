abstract ProofUnits = Categories ** {

cat
  Unit ;
  [Unit] ;
  Goal ;

fun
  UnitsProof : [Unit] -> Proof ;

  HyposUnit : [Hypo] -> Unit ;
  PropUnit : Prop -> Unit ;
  GoalUnit : Goal -> Unit ;
  FollowsLabelUnit : Label -> Unit ;
  FollowsPropUnit : Prop -> Unit ;
  
  FirstVerifyGoal : Prop -> Goal ;
  EnoughGoal : Prop -> Goal ;
  SinceGoal : Prop -> Goal -> Unit ;

}