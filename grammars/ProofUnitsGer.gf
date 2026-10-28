concrete ProofUnitsGer of ProofUnits = CategoriesGer **

-- the functions that proof texts (-proof-text) are made of

open
  SyntaxGer,
  ParadigmsGer,
  (P=ParadigmsGer),
  UtilitiesGer

in {

lin
  HyposUnit hypos = hypos.text ;
  GoalHyposUnit hypos prop = prefixText ("um zu zeigen ," ++ (SyntaxGer.mkAdv that_Subj (partProp prop)).s ++ ",") hypos.text ;
  ConclusionUnit prop = mkText (topProp prop) ;
  -- the fronted adverbial inverts the clause: "nach dem Modus ponens ist n gerade"
  LabelPropConclusion label prop =
    mkText (mkS (SyntaxGer.mkAdv (P.mkPrep "nach" P.dative) label.np) (topProp prop)) ;

  UnitJmt unit = lin Text {s = ">" ++ unit.s} ; --- not to be used, just to be reached

  -- the units of tactic proofs (-tactic-proof)
  ObviousConclusion = strText "das ist offensichtlich ." ;
  -- dass with the verb last: "es genügt zu zeigen, dass p prim ist"
  FirstVerifyGoal prop =
    strText ("zuerst prüfen wir ," ++ (SyntaxGer.mkAdv that_Subj (partProp prop)).s ++ ".") ;
  EnoughGoal prop =
    strText ("es genügt zu zeigen ," ++ (SyntaxGer.mkAdv that_Subj (partProp prop)).s ++ ".") ;
  InductionGoal =
    strText "wir argumentieren durch Induktion :" ;
  CasesGoal =
    strText "wir unterscheiden Fälle :" ;

lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "also" ;
  thusHence = P.mkAdv "somit" ;
  thenHence = P.mkAdv "dann" ;
  altogetherHence = P.mkAdv "insgesamt" ;
  afortioriHence = P.mkAdv "erst recht" ;
  inParticularHence = P.mkAdv "insbesondere" ;
  weConcludeHence = P.mkAdv "wir schließen, dass" ;

  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


  -- case analyses: "we have two cases :", "in the first case , assume that A ."
  lincat CaseNumber = Numeral ;
  lin
  NumberCasesGoal n =
    strText ("wir haben" ++ (SyntaxGer.mkUtt (SyntaxGer.mkNP (SyntaxGer.mkDet n) case_N)).s ++ ":") ;
  NumberCaseGoal n A =
    strText ((SyntaxGer.mkAdv in_Prep (SyntaxGer.mkNP (SyntaxGer.mkDet the_Quant (SyntaxGer.mkOrd n)) case_N)).s ++
             "nehmen wir an ," ++ (SyntaxGer.mkAdv that_Subj (partProp A)).s ++ ".") ;
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
