concrete ProofUnitsSwe of ProofUnits = CategoriesSwe **

open
  SyntaxSwe,
  ParadigmsSwe,
  (P=ParadigmsSwe),
  SymbolicSwe,
  UtilitiesSwe,
  (Grammar=GrammarSwe)

in {

lin
  HyposUnit hypos = hypos.text ;
  GoalHyposUnit hypos prop = prefixText ("för att visa" ++ (SyntaxSwe.mkAdv that_Subj (partProp prop)).s ++ ",") hypos.text ;
  ConclusionUnit prop = mkText (topProp prop) ;

  UnitJmt unit = lin Text {s = ">" ++ unit.s} ; --- not to be used, just to be reached

lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
----  UnitsProof units = units ;

  HyposAssumption hypos =
    hypos.text ;
  IdentKindAssumption kind ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP (useKind kind))) ;
  IdentExpAssumption exp ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP exp)) ;
  PropAssumption prop label =
    ccText (prefixText "anta att" (propText prop)) (strText ("(" ++ (mkUtt label.np).s ++ ")")) ;

  PropConclusion hence prop =
    prefixText hence.s (propText prop) ;
  SinceConclusion A B =
    ccText (strText "eftersom") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("följer av" ++ (mkUtt label.np).s) ;
  PropLabelConclusion hence prop label =
    ccText (strText hence.s) (propInText prop) (strText ("enligt" ++ (mkUtt label.np).s ++ ".")) ;
  -- the fronted adverbial inverts the clause: "enligt modus ponens är n jämnt"
  LabelPropConclusion label prop =
    mkText (mkS (SyntaxSwe.mkAdv (P.mkPrep "enligt") label.np) (topProp prop)) ;
  HencePropConclusion hence prop = mkText (mkS hence (topProp prop)) ;
  FollowsPropConclusion prop =
    prefixText "det följer att" (propText prop) ;
  ObviousConclusion = strText "det är uppenbart ." ;

  FirstVerifyGoal prop =
    prefixText "verifiera först att" (mkText (topProp prop)) ;
  EnoughGoal prop =
    prefixText "det räcker att visa att" (propText prop) ;
  SinceGoal prop goal =
    ccText (strText "eftersom") (commaText (propInText prop))
      (prefixText "det räcker att visa att" (propText goal)) ;
  InductionGoal =
    strText "vi resonerar med induktion :" ;
  CasesGoal =
    strText "vi resonerar med fallanalys :" ;
  CaseGoal A h =
    ccText (prefixText "\\item anta att" (propText A)) (strText ("(" ++ h ++ ")")) ;

  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "alltså" ;
  thusHence = P.mkAdv "således" ;
  thenHence = P.mkAdv "då" ;
  altogetherHence = P.mkAdv "sammantaget" ;
  afortioriHence = P.mkAdv "a fortiori" ;
  inParticularHence = P.mkAdv "i synnerhet" ;
  inBothCasesHence = P.mkAdv "i båda fallen" ;
  weConcludeHence = P.mkAdv "vi drar slutsatsen att" ;

  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


  -- case analyses: "we have two cases :", "in the first case , assume that A ."
  lincat CaseNumber = Numeral ;
  lin
  NumberCasesGoal n =
    strText ("vi har" ++ (SyntaxSwe.mkUtt (SyntaxSwe.mkNP (SyntaxSwe.mkDet n) case_N)).s ++ ":") ;
  NumberCaseGoal n A =
    prefixText ((SyntaxSwe.mkAdv in_Prep (SyntaxSwe.mkNP (SyntaxSwe.mkDet the_Quant (SyntaxSwe.mkOrd n)) case_N)).s ++ "," ++ "anta att") (propText A) ;
  one_CaseNumber = mkNumeral "1" ;
  two_CaseNumber = mkNumeral "2" ;
  three_CaseNumber = mkNumeral "3" ;
  four_CaseNumber = mkNumeral "4" ;
  five_CaseNumber = mkNumeral "5" ;
  six_CaseNumber = mkNumeral "6" ;
  seven_CaseNumber = mkNumeral "7" ;
  eight_CaseNumber = mkNumeral "8" ;
  nine_CaseNumber = mkNumeral "9" ;
}
