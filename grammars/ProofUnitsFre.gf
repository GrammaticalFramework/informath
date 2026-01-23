concrete ProofUnitsFre of ProofUnits = CategoriesFre **

open
  SyntaxFre,
  ParadigmsFre,
  UtilitiesFre,
  (Grammar=GrammarFre)

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
    prefixText "supposons que" (propText prop) ;

  PropConclusion prop = propText prop ;
  SinceConclusion A B =
    ccText (strText "comme") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("s'ensuit par" ++ (mkUtt label.np).s) ;
  PropLabelConclusion prop label =
    prefixText ("par" ++ (mkUtt label.np).s) (propText prop) ;
  FollowsPropConclusion prop =
    prefixText "il s'ensuit que" (propText prop) ;
  ObviousConclusion = strText "il est évident ." ;
    
  FirstVerifyGoal prop =
    prefixText "d'abord vérifier que" (mkText (topProp prop)) ;
  EnoughGoal prop = 
    prefixText "il suffit de montrer que" (propText prop) ;
  SinceGoal prop goal =
    ccText (strText "comme") (commaText (propInText prop))
      (prefixText "il suffit de montrer que" (propText goal)) ;
  CaseGoal A B =
    ccText (strText "nous avons deux cas :") (propText A) (propText B) ;


  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


}
