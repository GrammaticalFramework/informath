concrete ProofUnitsEng of ProofUnits = CategoriesEng **

open
  SyntaxEng,
  ParadigmsEng,
  UtilitiesEng,
  (Grammar=GrammarEng)

in {

lincat
  Unit = Text ;
  [Unit] = Text ;
  Goal = Text ;
  Assumption = Text ;
  Conclusion = Text ;

lin
--  UnitsProof units = units ;

  HyposAssumption hypos = hypos.text ;
  ElemAssumption kind elem =
    mkText (Grammar.ImpP3 elem (mkVP (useKind kind))) ; 
  PropAssumption prop =
    prefixText "assume that" (propText prop) ;

  PropConclusion prop = propText prop ;
  SinceConclusion A B =
    ccText (strText "since") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("follows by" ++ (mkUtt label.np).s) ;
  PropLabelConclusion prop label =
    prefixText ("by" ++ (mkUtt label.np).s) (propText prop) ;
  FollowsPropConclusion prop =
    prefixText "it follows that" (propText prop) ;
  ObviousConclusion = strText "it is obvious ." ;
  
  FirstVerifyGoal prop =
    prefixText "first verify that" (mkText (topProp prop)) ;
  EnoughGoal prop = 
    prefixText "it is enough to show that" (propText prop) ;
  SinceGoal prop goal =
    ccText (strText "since") (commaText (propInText prop))
      (prefixText "it is enough to show that" (propText goal)) ;
  CaseGoal A B =
    ccText (strText "we have two cases :") (propText A) (propText B) ;


  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


}