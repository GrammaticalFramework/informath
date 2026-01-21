concrete ProofUnitsEng of ProofUnits = CategoriesEng **

open SyntaxEng, ParadigmsEng, UtilitiesEng

in {

lincat
  Unit = Text ;
  [Unit] = Text ;
  Goal = Text ;

lin
  UnitsProof units = units ;

  HyposUnit hypos = hypos.text ;
  PropUnit prop = mkText (topProp prop) ;
  GoalUnit goal = goal ;
  FollowsLabelUnit label =
    strText ("follows by" ++ (mkUtt label.np).s) ;
  FollowsPropUnit prop = 
    mkText (propInText prop) (strText "follows .") ;
    
  FirstVerifyGoal prop =
    prefixText "first verify that" (mkText (topProp prop)) ;
  EnoughGoal prop = 
    prefixText "it is enough to prove that" (propText prop) ;
  SinceGoal prop goal =
    prefixText "since" (ccText (propInText prop) goal) ;


  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


}