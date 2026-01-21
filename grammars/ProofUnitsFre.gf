concrete ProofUnitsFre of ProofUnits = CategoriesFre **

open SyntaxFre, ParadigmsFre, UtilitiesFre

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
  SinceUnit A B =
    ccText (strText "comme") (commaText (propInText A)) (propText B) ;
  FollowsLabelUnit label =
    strText ("par" ++ (mkUtt label.np).s) ;
  FollowsPropUnit prop = 
    mkText (propInText prop) (strText "s'ensuit .") ;
    
  FirstVerifyGoal prop =
    prefixText "d'abord vérifier que" (mkText (topProp prop)) ;
  EnoughGoal prop = 
    prefixText "il suffit de vérifier que" (propText prop) ;
  SinceGoal prop goal =
    ccText (strText "comme") (commaText (propInText prop)) goal ;


  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


}