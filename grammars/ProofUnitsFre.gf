concrete ProofUnitsFre of ProofUnits = CategoriesFre **

open
  SyntaxFre,
  ParadigmsFre,
  (P=ParadigmsFre),
  SymbolicFre,
  UtilitiesFre,
  (Grammar=GrammarFre)

in {

lincat
  [Unit] = Text ;

lin
----  UnitsProof units = units ;

  HyposAssumption hypos =
    hypos.text ;
  IdentKindAssumption kind ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP (useKind kind))) ; 
  IdentExpAssumption exp ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP exp)) ; 
  PropAssumption prop =
    prefixText "supposons que" (propText prop) ;

  PropConclusion hence prop =
    prefixText hence.s (propText prop) ;
  SinceConclusion A B =
    ccText (strText "comme") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("s'ensuit par" ++ (mkUtt label.np).s) ;
  PropLabelConclusion hence prop label =
    ccText (strText hence.s) (propText prop) (strText ("par" ++ (mkUtt label.np).s)) ;
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
  InductionGoal =
    strText "par recurrence :" ;
  CasesGoal =
    strText "par cas :" ;
  CaseGoal A =
    prefixText "\\item supposons que" (propText A) ;

  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "donc" ;
  altogetherHence = P.mkAdv "em somme" ;
  afortioriHence = P.mkAdv "a fortiori" ;
  inParticularHence = P.mkAdv "en particulier" ;
  weConcludeHence = P.mkAdv "nous concluons que" ;
 
  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


}
